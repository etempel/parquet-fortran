!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for reading a `STRUCT` column into a `parquet_struct_column` -- the
!> whole-column and row-group-scoped specifics of
!> `parquet_read_column`/`parquet_read_column_chunk`.
!!
!! **Every oracle here is a file this library did not write.**
!! `test/fixtures/struct_payloads.parquet` is authored by Arrow
!! (`tools/generate_fixtures.cpp`) and carries one flat struct column per payload family plus one
!! holding all nine at once, over two row groups, with four deliberately chosen rows:
!!
!!   row 1  nothing null
!!   row 2  the STRUCT INSTANCE is null
!!   row 3  the struct is present and field `v` is null
!!   row 4  the struct is present and EVERY field is null
!!
!! **Rows 2 and 4 are the pair this suite exists for.** Every FIELD of both reads back null, so
!! the two are indistinguishable from any field's mask alone -- only the struct's own row validity
!! separates them, and that is the one thing `unwrap_struct_path` cannot supply. A reader that
!! took a field's combined mask as the row's answer would pass every other test in this file.
module test_struct_read
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    ! The struct comparison oracle lives with the write tests, which need it most; importing it is
    ! what keeps this file from carrying a second copy of the same per-field-kind dispatch.
    use test_struct_write, only : structs_equal
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_struct_read

    !> The Arrow-authored fixture every test here reads. Never written to.
    character(len=*), parameter :: PAYLOADS = "test/fixtures/struct_payloads.parquet"

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_struct_read(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("struct read reports the declared field set", test_field_set), &
            new_unittest("struct read separates row nullness from field nullness", test_two_null_levels), &
            new_unittest("a null struct row's fields are null in their OWN bitmaps", test_field_mask_is_face_value), &
            new_unittest("struct read returns every payload family's values", test_all_families), &
            new_unittest("struct read of the nine-field column keeps field order", test_mixed_order), &
            new_unittest("struct read per row group agrees with the whole column", test_chunked_agrees), &
            new_unittest("struct column shape and type queries answer", test_shape_queries), &
            new_unittest("struct read under a filter keeps both null levels", test_filtered), &
            new_unittest("struct leaves stay readable by their dotted paths", test_dotted_still_works) &
            ]
    end subroutine collect_tests_parquet_struct_read

    !> The field set comes from the file, not from the caller: names, order, count and kinds.
    subroutine test_field_set(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        character(len=:), allocatable :: nm
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_int32", sc)
        call check(error, sc%field_count() == 2, "the file's field count is reported")
        if (allocated(error)) return
        call sc%field_name(1, nm)
        call check(error, nm == "v", "the first field's name comes from the file")
        if (allocated(error)) return
        call sc%field_name(2, nm)
        call check(error, nm == "tag", "the second field's name comes from the file")
        if (allocated(error)) return
        call check(error, sc%field_kind(1) == PK_INT32, "the first field's kind comes from the file")
        if (allocated(error)) return
        call check(error, sc%field_kind(2) == PK_STRING, "the second field's kind comes from the file")
        if (allocated(error)) return
        call check(error, sc%size() == 4_int64, "every row of the fixture is read")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column read back satisfies its own invariants")
        call parquet_close_reader(r)
    end subroutine test_field_set

    !> **The test this suite exists for.** Row 2 is a null struct instance; row 4 is a present
    !> struct whose every field is null. Both give a null answer for every field, and only
    !> `%is_null` tells them apart.
    subroutine test_two_null_levels(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        character(len=:), allocatable :: tag
        logical :: okv, okt
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_int32", sc)
        call check(error, .not. sc%is_null(1), "row 1 is a present struct")
        if (allocated(error)) return
        call check(error, sc%is_null(2), "row 2 is a NULL struct instance")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(3), "row 3 is a present struct with one null field")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(4), &
            "row 4 is a PRESENT struct even though every field of it is null")
        if (allocated(error)) return
        call check(error, sc%null_count() == 1_int64, "exactly one row of the fixture is a null struct")
        if (allocated(error)) return
        h = sc%view(2)
        call h%get_field("v", v, is_valid=okv)
        call h%get_field("tag", tag, is_valid=okt)
        call check(error, (.not. okv) .and. (.not. okt), "every field of the null row reads back null")
        if (allocated(error)) return
        h = sc%view(4)
        call h%get_field("v", v, is_valid=okv)
        call h%get_field("tag", tag, is_valid=okt)
        call check(error, (.not. okv) .and. (.not. okt), &
            "every field of row 4 reads back null too -- which is why %is_null must separate 2 from 4")
        if (allocated(error)) return
        h = sc%view(3)
        call h%get_field("v", v, is_valid=okv)
        call h%get_field("tag", tag, is_valid=okt)
        call check(error, (.not. okv) .and. okt .and. tag == "r2", &
            "row 3's null field and present field are reported independently")
        call parquet_close_reader(r)
    end subroutine test_two_null_levels

    !> A null struct row's fields must be null in their OWN bitmaps, not merely masked by the row.
    !!
    !! **This is the field-level half of the problem, and it needs a trick to observe at all.**
    !! `unwrap_struct_path` returns each leaf's COMBINED mask (`struct_valid AND field_valid`) and
    !! `read_struct_field` stores it unchanged. That is correct -- Parquet's definition levels
    !! cannot encode "the struct is absent but its field is present", so for a file the combined
    !! mask IS the field's own stored validity -- and it looks wrong, so the tempting "fix" is to
    !! divide the struct's contribution back out. Doing that makes every field of every null struct
    !! row report `is_valid = .true.` over a value the file does not contain.
    !!
    !! **`%get_field` alone cannot see the difference**, which is why `test_two_null_levels` above
    !! does not catch it: `begin_get` tests the ROW level first and short-circuits, so a null row's
    !! fields answer null whatever their own bitmaps hold. Confirmed by mutation -- the division
    !! was applied to `read_struct_field` and all eight tests in this suite still passed.
    !!
    !! **`%clear_null_row` is the exposure.** Clearing the row level makes `begin_get` fall through
    !! to the field bitmap, which is then the only thing answering. Row 3 is the negative control:
    !! it is untouched, and its one null and one present field must still be reported apart --
    !! without it this test would also pass against a reader that nulled every field of every row.
    subroutine test_field_mask_is_face_value(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        character(len=:), allocatable :: tag
        logical :: okv, okt
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_int32", sc)
        call check(error, sc%is_null(2), "precondition: row 2 is a null struct instance")
        if (allocated(error)) return
        call sc%clear_null_row(2_int64)
        call check(error, .not. sc%is_null(2), "precondition: the ROW level is now clear")
        if (allocated(error)) return
        ! With the row level cleared, only the fields' own bitmaps can answer.
        h = sc%view(2)
        call h%get_field("v", v, is_valid=okv)
        call h%get_field("tag", tag, is_valid=okt)
        call check(error, .not. okv, &
            "field 'v' of a null struct row must be null in its OWN bitmap, not only via the row")
        if (allocated(error)) return
        call check(error, .not. okt, &
            "field 'tag' of a null struct row must be null in its OWN bitmap too")
        if (allocated(error)) return
        call check(error, v == 0_int32, &
            "and the value is the type's default rather than whatever the buffer held")
        if (allocated(error)) return
        ! NEGATIVE CONTROL: row 3 was never cleared, and its two fields differ.
        h = sc%view(3)
        call h%get_field("v", v, is_valid=okv)
        call h%get_field("tag", tag, is_valid=okt)
        call check(error, (.not. okv) .and. okt .and. tag == "r2", &
            "control: row 3's null and present fields must still be reported apart")
        call parquet_close_reader(r)
    end subroutine test_field_mask_is_face_value

    !> One column per payload family, each read back with row 1's value intact.
    subroutine test_all_families(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a
        integer(int64) :: b
        real(real32) :: c
        real(real64) :: d
        logical :: e
        character(len=:), allocatable :: f
        type(parquet_date) :: g
        type(parquet_time) :: t
        type(parquet_timestamp) :: u
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_int32", sc);     h = sc%view(1); call h%get_field("v", a)
        call check(error, a == 0, "an int32 field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_int64", sc);     h = sc%view(1); call h%get_field("v", b)
        call check(error, b == 0_int64, "an int64 field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_float32", sc);   h = sc%view(1); call h%get_field("v", c)
        call check(error, abs(c - 0.5_real32) < 1.0e-6_real32, "a float32 field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_float64", sc);   h = sc%view(1); call h%get_field("v", d)
        call check(error, abs(d - 0.25_real64) < 1.0e-12_real64, "a float64 field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_bool", sc);      h = sc%view(1); call h%get_field("v", e)
        call check(error, e, "a logical field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_string", sc);    h = sc%view(1); call h%get_field("v", f)
        call check(error, f == "x", "a string field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_date", sc);      h = sc%view(1); call h%get_field("v", g)
        call check(error, .not. g%is_null(), "a date field reads a non-null value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_time", sc);      h = sc%view(1); call h%get_field("v", t)
        call check(error, t%hour() == 1, "a time field reads its value back")
        if (allocated(error)) return
        call parquet_read_column(r, "s_timestamp", sc); h = sc%view(1); call h%get_field("v", u)
        call check(error, .not. u%is_null(), "a timestamp field reads a non-null value back")
        call parquet_close_reader(r)
    end subroutine test_all_families

    !> The nine-field column: every field present in declaration order, with its own kind.
    !>
    !> A per-family column cannot catch a field-ordering defect, because both its fields differ in
    !> kind; nine fields in one struct can, and this is the column that would expose it.
    subroutine test_mixed_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        character(len=:), allocatable :: nm
        character(len=6), parameter :: want(9) = [character(len=6) :: "a_i32", "b_i64", "c_f32", &
            "d_f64", "e_bool", "f_str", "g_date", "h_time", "i_ts"]
        integer, parameter :: want_kind(9) = [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, &
            PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP]
        integer :: j
        integer(int32) :: a
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_mixed", sc)
        call check(error, sc%field_count() == 9, "the nine-field struct reports nine fields")
        if (allocated(error)) return
        do j = 1, 9
            call sc%field_name(j, nm)
            call check(error, nm == trim(want(j)), "field "//trim(want(j))//" is at its declared position")
            if (allocated(error)) return
            call check(error, sc%field_kind(j) == want_kind(j), &
                "field "//trim(want(j))//" has its declared kind")
            if (allocated(error)) return
        end do
        h = sc%view(1)
        call h%get_field("a_i32", a)
        call check(error, a == 0, "the nine-field struct's first field reads its value back")
        call parquet_close_reader(r)
    end subroutine test_mixed_order

    !> Reading row group by row group gives the same rows, in the same order, with both null
    !> levels intact -- and the fixture's row groups straddle the row-2/row-4 pair deliberately.
    subroutine test_chunked_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: whole, chunk, joined
        type(parquet_struct_row) :: hw, hc
        integer :: ng, g, ci
        integer(int64) :: pos, i
        integer(int32) :: vw, vc
        logical :: okw, okc, same
        character(len=:), allocatable :: why
        !> One column per payload kind the struct reader supports, each a single field `v`.
        character(len=11), parameter :: PAYLOAD_COLS(9) = [character(len=11) :: &
            "s_int32", "s_int64", "s_float32", "s_float64", "s_bool", "s_string", &
            "s_date", "s_time", "s_timestamp"]
        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "s_int32", whole)
        call parquet_get_num_row_groups(r, ng)
        call check(error, ng == 2, "the fixture really has two row groups")
        if (allocated(error)) return
        pos = 0_int64
        do g = 1, ng
            call parquet_read_column_chunk(r, "s_int32", g, chunk)
            do i = 1_int64, chunk%size()
                pos = pos + 1_int64
                call check(error, chunk%is_null(i) .eqv. whole%is_null(pos), &
                    "a chunked read reports the same ROW nullness as the whole-column read")
                if (allocated(error)) return
                hw = whole%view(pos); hc = chunk%view(i)
                call hw%get_field("v", vw, is_valid=okw)
                call hc%get_field("v", vc, is_valid=okc)
                call check(error, (okw .eqv. okc) .and. (.not. okw .or. vw == vc), &
                    "a chunked read reports the same FIELD value and nullness as the whole-column read")
                if (allocated(error)) return
            end do
        end do
        call check(error, pos == whole%size(), "the row groups account for every row of the column")
        if (allocated(error)) then
            call parquet_close_reader(r)
            return
        end if
        !
        ! EVERY payload kind, not just int32. `read_struct_field` reads a field's payload with a
        ! separate call per kind and a separate arm for the chunked form, so the eight other kinds
        ! reach code the int32 sweep above never touches -- and a chunked arm reading the wrong
        ! row group, or slicing wrongly, produces a column that still has the right shape.
        !
        ! Reassembling the chunks and comparing against the whole-column read is the oracle: it
        ! checks every field value and both null levels through `structs_equal`, rather than only
        ! the counts a size comparison would.
        do ci = 1, size(PAYLOAD_COLS)
            call parquet_read_column_chunk(r, trim(PAYLOAD_COLS(ci)), 1, joined)
            do g = 2, ng
                call parquet_read_column_chunk(r, trim(PAYLOAD_COLS(ci)), g, chunk)
                call joined%append_from(chunk)
            end do
            call parquet_read_column(r, trim(PAYLOAD_COLS(ci)), whole)
            call structs_equal(whole, joined, same, why)
            call check(error, same, trim(PAYLOAD_COLS(ci)) // &
                ": the row groups must reassemble into the whole-column read -- " // why)
            if (allocated(error)) exit
        end do
        call parquet_close_reader(r)
    end subroutine test_chunked_agrees

    !> The schema-only queries answer for a bare struct column name. `parquet_get_column_shape`
    !> answering `"struct"` was already implemented and simply had no test reaching it.
    subroutine test_shape_queries(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        character(len=:), allocatable :: shp, typ
        call parquet_open_reader(r, PAYLOADS)
        call check(error, parquet_column_exists(r, "s_int32"), "a bare struct column name exists")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "s_int32", shp)
        call check(error, shp == "struct", "%get_column_shape answers 'struct' for a struct column")
        if (allocated(error)) return
        call parquet_get_column_type(r, "s_int32", typ)
        call check(error, typ == "unknown", &
            "%get_column_type answers 'unknown' for a struct: it has no single element type")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "rowid", shp)
        call check(error, shp == "scalar", "an ordinary column still answers 'scalar'")
        call parquet_close_reader(r)
    end subroutine test_shape_queries

    !> A row filter removes rows and nothing else: the surviving rows keep both null levels.
    subroutine test_filtered(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_filter) :: f
        type(parquet_struct_row) :: h
        character(len=:), allocatable :: tag
        logical :: ok
        call f%add("rowid >= 1")            ! drops the first row, keeping rows 2..4
        call parquet_open_reader(r, PAYLOADS, filter=f)
        call parquet_read_column(r, "s_int32", sc)
        call check(error, sc%size() == 3_int64, "a filter removes rows from a struct read")
        if (allocated(error)) return
        call check(error, sc%is_null(1), "the filtered result keeps the null struct row")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(3), "the filtered result keeps a present all-fields-null row")
        if (allocated(error)) return
        h = sc%view(2)
        call h%get_field("tag", tag, is_valid=ok)
        call check(error, ok .and. tag == "r2", &
            "the filtered result keeps its surviving rows' field values")
        call parquet_close_reader(r)
    end subroutine test_filtered

    !> The dotted-path reader is unchanged and permanent: a struct's leaves stay addressable as
    !> ordinary columns, and that is the mechanism the struct read is BUILT on.
    subroutine test_dotted_still_works(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        integer(int32) :: v(4)
        logical :: ok(4)
        call parquet_open_reader(r, PAYLOADS)
        call check(error, parquet_column_exists(r, "s_int32.v"), "a struct leaf's dotted path still resolves")
        if (allocated(error)) return
        call parquet_read_column(r, "s_int32.v", v, is_valid=ok)
        call check(error, ok(1) .and. .not. ok(2) .and. .not. ok(3) .and. .not. ok(4), &
            "the dotted path returns the COMBINED mask, which is what the struct read is built on")
        if (allocated(error)) return
        call check(error, v(1) == 0, "the dotted path returns the leaf's values")
        call parquet_close_reader(r)
    end subroutine test_dotted_still_works

end module test_struct_read
