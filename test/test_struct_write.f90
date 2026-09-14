!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for writing a `STRUCT` column to a Parquet file -- the whole-column and
!> row-group-scoped specifics of
!> `parquet_write_column`/`parquet_write_column_chunk`, plus the MAML `struct`
!> declaration a schema-enforced writer needs.
!!
!! **Three oracles, deliberately, because a writer cannot be its own.** Every test here
!! establishes what the file really contains in one of three ways, and the third is what stops
!! the first two passing against a writer that quietly emitted something else:
!!
!! * **The round trip.** Build a `parquet_struct_column`, write it, read it back with
!!   `parquet_read_column`, and compare the field set, every field's value and BOTH null levels.
!!   Not circular: the read path is tested in its own suite against a fixture this library did
!!   not write.
!! * **The re-write.** Read `test/fixtures/struct_payloads.parquet` -- authored by Arrow, not by
!!   this library -- write every one of its struct columns straight back out, re-read, and compare
!!   against the first read. One test covering all nine field kinds, a nine-field struct, null
!!   rows, null fields and two row groups, against an oracle nothing here produced.
!! * **`parquet_get_column_shape`.** On the written file it must answer `"struct"`. That is what
!!   distinguishes "a genuine STRUCT column was written" from "something else was written and the
!!   reader was tolerant" -- and without it a round trip would pass against the latter.
!!
!! One round-trip asymmetry is expected and asserted rather than worked around: a row marked null
!! by `%set_null` keeps its field values in memory, and Parquet cannot store them -- its
!! definition levels have no way to say "the struct is absent but its field is present" -- so such
!! a row reads back with every field null. See `test_null_row_fields_do_not_survive`.
!!
!! Every test writes to its own fixture path: test-drive runs a suite's tests concurrently, so a
!! shared output filename is a truncation race (`.claude/rules/testing.md`, "Tests run concurrently").
module test_struct_write
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_struct_write
    !> The struct oracle, shared with `test_struct_read`'s chunked-read test rather than
    !! duplicated there: comparing two struct columns needs a dispatch over every field kind,
    !! and two copies of that dispatch would drift apart one kind at a time.
    public :: structs_equal

    !> The Arrow-authored fixture the re-write test uses as its oracle. Never written to.
    character(len=*), parameter :: PAYLOADS = "test/fixtures/struct_payloads.parquet"

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_struct_write(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("struct write round-trips every field kind", test_round_trip_all_kinds), &
            new_unittest("struct write round-trips both null levels", test_round_trip_nulls), &
            new_unittest("struct write re-writes the Arrow fixture identically", test_rewrite_fixture), &
            new_unittest("struct write emits a genuine STRUCT column", test_shape_is_struct), &
            new_unittest("struct write accepts a MAML struct declaration", test_maml_declaration), &
            new_unittest("struct write per row group matches a whole write", test_chunked_matches), &
            new_unittest("struct null row's field values do not survive a round trip", &
                test_null_row_fields_do_not_survive), &
            new_unittest("struct write of a row-masked column keeps the survivors", test_row_mask) &
            ]
    end subroutine collect_tests_parquet_struct_write

    !> Fills `sc` with the three-row, nine-field column every round-trip test below uses.
    !>
    !> Row 1 holds a value in every field, row 2 is a null struct instance, row 3 is present with
    !> its `a` field null and the rest set -- so one fixture exercises both null levels at once.
    subroutine build_all_kinds(sc)
        type(parquet_struct_column), intent(inout) :: sc !! cleared, then filled.
        type(parquet_date) :: g
        type(parquet_time) :: t
        type(parquet_timestamp) :: u
        call sc%init(["a", "b", "c", "d", "e", "f", "g", "t", "u"], &
            [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP])
        call g%set(2026, 8, 26)
        call t%set(13, 45, 0)
        call u%set(g, t)
        call sc%append_row()
        call sc%set_field(1, "a", 5_int32)
        call sc%set_field(1, "b", 6_int64)
        call sc%set_field(1, "c", 1.5_real32)
        call sc%set_field(1, "d", 2.5_real64)
        call sc%set_field(1, "e", .true.)
        call sc%set_field(1, "f", "hello")
        call sc%set_field(1, "g", g)
        call sc%set_field(1, "t", t)
        call sc%set_field(1, "u", u)
        call sc%append_null_row()
        call sc%append_row()
        call sc%set_field(3, "b", 7_int64)
        call sc%set_field(3, "f", "third")
    end subroutine build_all_kinds

    !> Compares two struct columns cell by cell, at both null levels, across every field kind.
    !>
    !> The shared oracle of every test in this file. `why` names which comparison failed, because
    !> a bare line number in a nine-field, three-row sweep says nothing.
    subroutine structs_equal(a, b, same, why)
        type(parquet_struct_column), intent(in), target :: a !! the expected column.
        type(parquet_struct_column), intent(in), target :: b !! the column under test.
        logical, intent(out) :: same                         !! .true. when every cell agrees.
        character(len=:), allocatable, intent(out) :: why    !! what differed, or "".
        type(parquet_struct_row) :: ha, hb
        character(len=:), allocatable :: na, nb, sa, sb
        integer(int64) :: i
        integer :: j
        integer(int32) :: ia, ib
        integer(int64) :: la, lb
        real(real32) :: fa, fb
        real(real64) :: da, db
        logical :: ba, bb, oka, okb
        type(parquet_date) :: ga, gb
        type(parquet_time) :: ta, tb
        type(parquet_timestamp) :: ua, ub

        same = .false.
        why = ""
        if (a%size() /= b%size()) then
            why = "row counts differ"
            return
        end if
        if (a%field_count() /= b%field_count()) then
            why = "field counts differ"
            return
        end if
        do j = 1, a%field_count()
            call a%field_name(j, na)
            call b%field_name(j, nb)
            if (na /= nb) then
                why = "field "//na//" is not at the same position"
                return
            end if
            if (a%field_kind(j) /= b%field_kind(j)) then
                why = "field "//na//" has a different kind"
                return
            end if
        end do
        do i = 1_int64, a%size()
            if (a%is_null(i) .neqv. b%is_null(i)) then
                why = "row nullness differs"
                return
            end if
            if (a%is_null(i)) cycle
            ha = a%view(i)
            hb = b%view(i)
            do j = 1, a%field_count()
                call a%field_name(j, na)
                select case (a%field_kind(j))
                case (PK_INT32)
                    call ha%get_field(na, ia, is_valid=oka)
                    call hb%get_field(na, ib, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. ia /= ib)) then
                        why = "int32 field "//na//" differs"
                        return
                    end if
                case (PK_INT64)
                    call ha%get_field(na, la, is_valid=oka)
                    call hb%get_field(na, lb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. la /= lb)) then
                        why = "int64 field "//na//" differs"
                        return
                    end if
                case (PK_FLOAT32)
                    call ha%get_field(na, fa, is_valid=oka)
                    call hb%get_field(na, fb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. abs(fa - fb) > 1.0e-6_real32)) then
                        why = "float32 field "//na//" differs"
                        return
                    end if
                case (PK_FLOAT64)
                    call ha%get_field(na, da, is_valid=oka)
                    call hb%get_field(na, db, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. abs(da - db) > 1.0e-12_real64)) then
                        why = "float64 field "//na//" differs"
                        return
                    end if
                case (PK_LOGICAL)
                    call ha%get_field(na, ba, is_valid=oka)
                    call hb%get_field(na, bb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. (ba .neqv. bb))) then
                        why = "logical field "//na//" differs"
                        return
                    end if
                case (PK_STRING)
                    call ha%get_field(na, sa, is_valid=oka)
                    call hb%get_field(na, sb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. sa /= sb)) then
                        why = "string field "//na//" differs"
                        return
                    end if
                case (PK_DATE)
                    call ha%get_field(na, ga, is_valid=oka)
                    call hb%get_field(na, gb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. ga%raw() /= gb%raw())) then
                        why = "date field "//na//" differs"
                        return
                    end if
                case (PK_TIME)
                    call ha%get_field(na, ta, is_valid=oka)
                    call hb%get_field(na, tb, is_valid=okb)
                    if ((oka .neqv. okb) .or. (oka .and. ta%raw() /= tb%raw())) then
                        why = "time field "//na//" differs"
                        return
                    end if
                case (PK_TIMESTAMP)
                    call ha%get_field(na, ua, is_valid=oka)
                    call hb%get_field(na, ub, is_valid=okb)
                    if (oka .neqv. okb) then
                        why = "timestamp field "//na//" differs in nullness"
                        return
                    end if
                    if (oka) then
                        if (ua%to_unix(parquet_unit_micros) /= ub%to_unix(parquet_unit_micros)) then
                            why = "timestamp field "//na//" differs"
                            return
                        end if
                    end if
                end select
            end do
        end do
        same = .true.
    end subroutine structs_equal

    !> Every field kind survives a write and a read.
    subroutine test_round_trip_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc, back
        character(len=*), parameter :: path = "test_run/struct_write_all_kinds.parquet"
        logical :: same
        character(len=:), allocatable :: why
        call build_all_kinds(sc)
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call structs_equal(sc, back, same, why)
        call check(error, same, "a nine-field struct column round-trips unchanged: "//why)
    end subroutine test_round_trip_all_kinds

    !> Both null levels survive independently: a null row stays a null row, and a null field of a
    !> present row stays a null field of a present row.
    subroutine test_round_trip_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc, back
        type(parquet_struct_row) :: h
        character(len=*), parameter :: path = "test_run/struct_write_nulls.parquet"
        integer(int32) :: v
        logical :: ok
        call sc%init(["v  ", "w  "], [PK_INT32, PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 1_int32); call sc%set_field(1, "w", 2_int32)
        call sc%append_null_row()                                   ! the instance is absent
        call sc%append_row(); call sc%set_field(3, "w", 3_int32)    ! present, `v` null
        call sc%append_row()                                        ! present, EVERY field null
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call check(error, back%size() == 4_int64, "every row survives the round trip")
        if (allocated(error)) return
        call check(error, .not. back%is_null(1), "a fully-populated row reads back present")
        if (allocated(error)) return
        call check(error, back%is_null(2), "a null struct row reads back null")
        if (allocated(error)) return
        call check(error, .not. back%is_null(3), "a row with one null field reads back PRESENT")
        if (allocated(error)) return
        call check(error, .not. back%is_null(4), &
            "a row with EVERY field null reads back present -- not as a null row")
        if (allocated(error)) return
        h = back%view(3)
        call h%get_field("v", v, is_valid=ok)
        call check(error, .not. ok, "the null field of row 3 reads back null")
        if (allocated(error)) return
        call h%get_field("w", v, is_valid=ok)
        call check(error, ok .and. v == 3, "the present field of row 3 reads back with its value")
    end subroutine test_round_trip_nulls

    !> Reads every struct column of the Arrow-authored fixture, writes them all back out, re-reads
    !> and compares -- an oracle nothing in this library produced.
    subroutine test_rewrite_fixture(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_writer) :: w
        type(parquet_struct_column), target :: orig(10), back
        character(len=*), parameter :: path = "test_run/struct_write_rewrite.parquet"
        character(len=11), parameter :: cols(10) = [character(len=11) :: "s_int32", "s_int64", &
            "s_float32", "s_float64", "s_bool", "s_string", "s_date", "s_time", "s_timestamp", "s_mixed"]
        integer :: j
        logical :: same
        character(len=:), allocatable :: why
        call parquet_open_reader(r, PAYLOADS)
        do j = 1, size(cols)
            call parquet_read_column(r, trim(cols(j)), orig(j))
        end do
        call parquet_close_reader(r)
        call parquet_open_writer(w, path)
        do j = 1, size(cols)
            call parquet_write_column(w, trim(cols(j)), orig(j))
        end do
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        do j = 1, size(cols)
            call parquet_read_column(r, trim(cols(j)), back)
            call structs_equal(orig(j), back, same, why)
            call check(error, same, "column "//trim(cols(j))//" re-writes identically: "//why)
            if (allocated(error)) then
                call parquet_close_reader(r)
                return
            end if
        end do
        call parquet_close_reader(r)
    end subroutine test_rewrite_fixture

    !> The written file really carries a `STRUCT` column, not something the reader tolerated.
    subroutine test_shape_is_struct(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        character(len=*), parameter :: path = "test_run/struct_write_shape.parquet"
        character(len=:), allocatable :: shp
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 1_int32)
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_get_column_shape(r, "s", shp)
        call check(error, shp == "struct", "the written column's shape is 'struct'")
        if (allocated(error)) then
            call parquet_close_reader(r)
            return
        end if
        call check(error, parquet_column_exists(r, "s.v"), &
            "the written struct's leaf is addressable by its dotted path")
        call parquet_close_reader(r)
    end subroutine test_shape_is_struct

    !> A schema-enforced writer accepts the bare MAML `struct` token, which declares only that the
    !> column IS a struct -- the field layout comes from the column object, never from MAML.
    subroutine test_maml_declaration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_schema) :: sch
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc, back
        character(len=*), parameter :: path = "test_run/struct_write_maml.parquet"
        logical :: same
        character(len=:), allocatable :: why
        call sch%init("structs")
        call sch%add_field("s", "struct")
        call parquet_parse_maml(sch)
        call sc%init(["v  ", "nm "], [PK_INT32, PK_STRING])
        call sc%append_row(); call sc%set_field(1, "v", 3_int32); call sc%set_field(1, "nm", "abc")
        call parquet_open_writer(w, path, sch)
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call structs_equal(sc, back, same, why)
        call check(error, same, "a schema-declared struct column round-trips: "//why)
    end subroutine test_maml_declaration

    !> Writing row group by row group produces the same column as one whole-column write.
    subroutine test_chunked_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: whole, part, back
        character(len=*), parameter :: path = "test_run/struct_write_chunked.parquet"
        integer(int64) :: i
        logical :: same
        character(len=:), allocatable :: why
        ! The same six rows, once as a whole column and once split 3 + 3.
        call whole%init(["v  "], [PK_INT32])
        do i = 1_int64, 6_int64
            if (i == 2_int64) then
                call whole%append_null_row()
            else
                call whole%append_row()
                call whole%set_field(i, "v", int(i * 10, int32))
            end if
        end do
        call parquet_open_writer(w, path)
        call parquet_new_row_group(w, 3_int64)
        call part%init(["v  "], [PK_INT32])
        call part%append_row(); call part%set_field(1, "v", 10_int32)
        call part%append_null_row()
        call part%append_row(); call part%set_field(3, "v", 30_int32)
        call parquet_write_column_chunk(w, "s", part)
        call parquet_finish_row_group(w)
        call parquet_new_row_group(w, 3_int64)
        call part%init(["v  "], [PK_INT32])
        call part%append_row(); call part%set_field(1, "v", 40_int32)
        call part%append_row(); call part%set_field(2, "v", 50_int32)
        call part%append_row(); call part%set_field(3, "v", 60_int32)
        call parquet_write_column_chunk(w, "s", part)
        call parquet_finish_row_group(w)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call structs_equal(whole, back, same, why)
        call check(error, same, "a streamed struct write matches the same rows written whole: "//why)
    end subroutine test_chunked_matches

    !> **A deliberate, documented round-trip asymmetry, asserted rather than worked around.**
    !>
    !> `%set_null` leaves a nulled row's field values in memory (which is what makes it O(1) and
    !> lets `%clear_null` restore them), but Parquet cannot store them: its definition levels have
    !> no way to say "the struct is absent but its field is present". So a written and re-read
    !> column has every field of every null row null, and a caller comparing an in-memory column
    !> against a read-back one must expect that.
    subroutine test_null_row_fields_do_not_survive(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc, back
        type(parquet_struct_row) :: h
        character(len=*), parameter :: path = "test_run/struct_write_nullrow_fields.parquet"
        integer(int32) :: v
        logical :: ok
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 99_int32)   ! a real value ...
        call sc%set_null(1)                   ! ... in a row then marked absent
        h = sc%view(1)
        call h%get_field("v", v, is_valid=ok)
        call check(error, .not. ok, "%get on a null row reports the field null even in memory")
        if (allocated(error)) return
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call check(error, back%is_null(1), "the null row reads back null")
        if (allocated(error)) return
        h = back%view(1)
        call h%get_field("v", v, is_valid=ok)
        call check(error, .not. ok, &
            "the unreachable field value did not survive -- Parquet cannot encode it, by design")
    end subroutine test_null_row_fields_do_not_survive

    !> A row mask removes rows and nothing else: the survivors keep their values and both null
    !> levels, and the rebuild goes through `%gather_rows` on every field.
    subroutine test_row_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc, back
        type(parquet_struct_row) :: h
        character(len=*), parameter :: path = "test_run/struct_write_row_mask.parquet"
        integer(int32) :: v
        logical :: ok
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 10_int32)
        call sc%append_null_row()
        call sc%append_row(); call sc%set_field(3, "v", 30_int32)
        call sc%append_row(); call sc%set_field(4, "v", 40_int32)
        call parquet_open_writer(w, path)
        call parquet_write_row_mask(w, [.true., .true., .false., .true.])
        call parquet_write_column(w, "s", sc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "s", back)
        call parquet_close_reader(r)
        call check(error, back%size() == 3_int64, "the row mask dropped exactly one row")
        if (allocated(error)) return
        call check(error, back%is_null(2), "the surviving null row is still null")
        if (allocated(error)) return
        h = back%view(3)
        call h%get_field("v", v, is_valid=ok)
        call check(error, ok .and. v == 40, "the last surviving row kept its value")
    end subroutine test_row_mask

end module test_struct_write
