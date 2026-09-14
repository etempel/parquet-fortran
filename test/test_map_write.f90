!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for writing a `MAP` column to a Parquet file -- the whole-column and
!> row-group-scoped specifics of `parquet_write_column`/`parquet_write_column_chunk`,
!> plus the MAML `map[<valuetype>]` declaration a schema-enforced writer needs.
!!
!! **Three oracles, deliberately, because a writer cannot be its own.** Every test here
!! establishes what the file really contains in one of three ways, and the third is what stops
!! the first two passing against a writer that quietly emitted something else:
!!
!! * **The round trip.** Build a `parquet_map_column`, write it, read it back with
!!   `parquet_read_column`, and compare every row's keys, values, order and BOTH null levels.
!!   Not circular: the read path is tested in its own suite against a fixture this library did
!!   not write.
!! * **The re-write.** Read `test/fixtures/map_payloads.parquet` -- authored by Arrow, not by this
!!   library -- write its map columns straight back out, re-read, and compare against the first
!!   read. One test covering all nine value kinds, duplicate keys, null rows, empty rows, null
!!   values and two row groups, against an oracle nothing here produced.
!! * **`parquet_get_column_shape`.** On the written file it must answer `"map"`. That is what
!!   distinguishes "a genuine MAP column was written" from "something else was written and the
!!   reader was tolerant" -- and without it a round trip would pass against the latter.
!!
!! Every test writes to its own fixture path: test-drive runs a suite's tests concurrently, so a
!! shared output filename is a truncation race (`.claude/rules/testing.md`, "Tests run concurrently").
module test_map_write
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_map_write

    !> The Arrow-authored fixture the re-write test uses as its oracle. Never written to.
    character(len=*), parameter :: PAYLOADS = "test/fixtures/map_payloads.parquet"

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_map_write(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("map write round-trips every value kind", test_round_trip_all_kinds), &
            new_unittest("map write round-trips all three null shapes", test_round_trip_nulls), &
            new_unittest("map write re-writes the Arrow fixture identically", test_rewrite_fixture), &
            new_unittest("map write emits a genuine MAP column", test_shape_is_map), &
            new_unittest("map write accepts a MAML map declaration", test_maml_declaration), &
            new_unittest("map write per row group matches a whole write", test_chunked_matches), &
            new_unittest("map write preserves duplicate keys and their order", test_duplicates_survive), &
            new_unittest("map write of a row-masked column keeps the survivors", test_row_mask), &
            new_unittest("map declared but never written closes with zero rows", test_empty_at_close) &
            ]
    end subroutine collect_tests_parquet_map_write

    !> Compares two map columns entry by entry: row count, value kind, row nullness, entry counts,
    !> key order and every value's own nullness. `why` names the first difference.
    !>
    !> Written out rather than leaning on any single query, because the properties that can go
    !> wrong independently -- a row's nullness, its entry count, the ORDER of its keys, and each
    !> value's validity -- are exactly the ones a summary comparison would hide.
    subroutine maps_equal(a, b, same, why)
        type(parquet_map_column), intent(in), target :: a !! the expected column.
        type(parquet_map_column), intent(in), target :: b !! the column read back.
        logical, intent(out) :: same                      !! .true. when every property matches.
        character(len=:), allocatable, intent(out) :: why !! the first difference, or "".
        type(parquet_map_row) :: ra, rb
        integer(int64) :: i
        integer :: p
        character(len=:), allocatable :: ka, kb, sa, sb
        integer(int32) :: ia, ib
        integer(int64) :: la, lb
        real(real32) :: fa, fb
        real(real64) :: da, db
        logical :: ba, bb, va, vb
        type(parquet_date) :: gda, gdb
        type(parquet_time) :: gta, gtb
        type(parquet_timestamp) :: gua, gub
        character(len=32) :: buf

        same = .false.
        why = ""
        if (a%size() /= b%size()) then
            why = "row counts differ"
            return
        end if
        if (a%element_kind() /= b%element_kind()) then
            why = "value kinds differ"
            return
        end if
        do i = 1_int64, a%size()
            write(buf, '(i0)') i
            if (a%is_null(i) .neqv. b%is_null(i)) then
                why = "row "//trim(buf)//": row nullness differs"
                return
            end if
            if (a%length(i) /= b%length(i)) then
                why = "row "//trim(buf)//": entry counts differ"
                return
            end if
            if (a%is_null(i)) cycle
            ra = a%view(i)
            rb = b%view(i)
            do p = 1, ra%size()
                call ra%key_at(p, ka)
                call rb%key_at(p, kb)
                if (ka /= kb) then
                    why = "row "//trim(buf)//": keys differ ('"//ka//"' vs '"//kb//"')"
                    return
                end if
                select case (a%element_kind())
                case (PK_INT32)
                    call ra%get_at(p, ia, is_valid=va); call rb%get_at(p, ib, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. ia /= ib) then; why = "row "//trim(buf)//": int32 values differ"; return; end if
                case (PK_INT64)
                    call ra%get_at(p, la, is_valid=va); call rb%get_at(p, lb, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. la /= lb) then; why = "row "//trim(buf)//": int64 values differ"; return; end if
                case (PK_FLOAT32)
                    call ra%get_at(p, fa, is_valid=va); call rb%get_at(p, fb, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. fa /= fb) then; why = "row "//trim(buf)//": float32 values differ"; return; end if
                case (PK_FLOAT64)
                    call ra%get_at(p, da, is_valid=va); call rb%get_at(p, db, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. da /= db) then; why = "row "//trim(buf)//": float64 values differ"; return; end if
                case (PK_LOGICAL)
                    call ra%get_at(p, ba, is_valid=va); call rb%get_at(p, bb, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. (ba .neqv. bb)) then; why = "row "//trim(buf)//": logical values differ"; return; end if
                case (PK_STRING)
                    call ra%get_at(p, sa, is_valid=va); call rb%get_at(p, sb, is_valid=vb)
                    if (va .neqv. vb) then; why = "row "//trim(buf)//": value validity differs"; return; end if
                    if (va .and. sa /= sb) then; why = "row "//trim(buf)//": string values differ"; return; end if
                case (PK_DATE)
                    call ra%get_at(p, gda); call rb%get_at(p, gdb)
                    if (gda%is_null() .neqv. gdb%is_null()) then
                        why = "row "//trim(buf)//": date nullness differs"; return
                    end if
                    if (.not. gda%is_null() .and. gda%raw() /= gdb%raw()) then
                        why = "row "//trim(buf)//": date values differ"; return
                    end if
                case (PK_TIME)
                    call ra%get_at(p, gta); call rb%get_at(p, gtb)
                    if (gta%is_null() .neqv. gtb%is_null()) then
                        why = "row "//trim(buf)//": time nullness differs"; return
                    end if
                    if (.not. gta%is_null() .and. gta%raw() /= gtb%raw()) then
                        why = "row "//trim(buf)//": time values differ"; return
                    end if
                case (PK_TIMESTAMP)
                    call ra%get_at(p, gua); call rb%get_at(p, gub)
                    if (gua%is_null() .neqv. gub%is_null()) then
                        why = "row "//trim(buf)//": timestamp nullness differs"; return
                    end if
                    if (.not. gua%is_null()) then
                        if (gua%to_unix(parquet_unit_micros) /= gub%to_unix(parquet_unit_micros)) then
                            why = "row "//trim(buf)//": timestamp values differ"; return
                        end if
                    end if
                end select
            end do
        end do
        same = .true.
    end subroutine maps_equal

    !> One column per value kind, written and read back. The nine-way generic fan-out on the write
    !> side is the surface here, exactly as it is on the read side.
    subroutine test_round_trip_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc, back
        character(len=*), parameter :: path = "test_run/map_write_kinds.parquet"
        type(parquet_date) :: g
        type(parquet_time) :: t
        type(parquet_timestamp) :: u
        logical :: same
        character(len=:), allocatable :: why
        integer :: k
        integer, parameter :: kinds(9) = [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
            PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP]
        character(len=16) :: cname

        call g%set(2026, 8, 27)
        call t%set(13, 45, 30)
        call u%set(g, t)
        do k = 1, size(kinds)
            write(cname, '(a,i0)') "m", k
            call mc%init(kinds(k))
            select case (kinds(k))
            case (PK_INT32);     call mc%append_row(["a", "b"], [1_int32, 2_int32])
            case (PK_INT64);     call mc%append_row(["a", "b"], [1_int64, 2_int64])
            case (PK_FLOAT32);   call mc%append_row(["a", "b"], [1.5_real32, 2.5_real32])
            case (PK_FLOAT64);   call mc%append_row(["a", "b"], [1.25_real64, 2.25_real64])
            case (PK_LOGICAL);   call mc%append_row(["a", "b"], [.true., .false.])
            case (PK_STRING);    call mc%append_row(["a", "b"], ["one", "two"])
            case (PK_DATE);      call mc%append_row(["a", "b"], [g, g])
            case (PK_TIME);      call mc%append_row(["a", "b"], [t, t])
            case (PK_TIMESTAMP); call mc%append_row(["a", "b"], [u, u])
            end select
            call parquet_open_writer(w, path)
            call parquet_write_column(w, trim(cname), mc)
            call parquet_close_writer(w)
            call parquet_open_reader(r, path)
            call parquet_read_column(r, trim(cname), back)
            call parquet_close_reader(r)
            call maps_equal(mc, back, same, why)
            call check(error, same, trim(cname)//" round-trips: "//why)
            if (allocated(error)) return
        end do
    end subroutine test_round_trip_all_kinds

    !> The three shapes that must survive distinctly: a null row, a present-but-empty row, and a
    !> present row whose value is null.
    subroutine test_round_trip_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc, back
        character(len=*), parameter :: path = "test_run/map_write_nulls.parquet"
        logical :: same
        character(len=:), allocatable :: why

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32, 2_int32])
        call mc%append_null_row()
        call mc%append_empty_row()
        call mc%append_row(["c"], [0_int32], is_valid=[.false.])

        call parquet_open_writer(w, path)
        call parquet_write_column(w, "m", mc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "m", back)
        call parquet_close_reader(r)

        call maps_equal(mc, back, same, why)
        call check(error, same, "all three null shapes survive: "//why)
        if (allocated(error)) return
        ! Stated separately as well, because maps_equal comparing two identical mistakes would
        ! pass: the null/empty pair is the one that a writer collapsing them still round-trips.
        call check(error, back%is_null(2_int64), "the null row read back null")
        if (allocated(error)) return
        call check(error, .not. back%is_null(3_int64), "the empty row read back present")
        if (allocated(error)) return
        call check(error, back%is_empty(3_int64), "and still empty")
    end subroutine test_round_trip_nulls

    !> Reads every map column of the Arrow-authored fixture, writes them straight back out,
    !> re-reads and compares. The oracle is a file this library did not produce.
    subroutine test_rewrite_fixture(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_writer) :: w
        type(parquet_map_column), target :: orig, back
        character(len=*), parameter :: path = "test_run/map_write_rewrite.parquet"
        character(len=12), parameter :: cols(10) = [character(len=12) :: "m_int32", "m_int64", &
            "m_float32", "m_float64", "m_bool", "m_string", "m_date", "m_time", "m_timestamp", "m_dup"]
        integer :: c
        logical :: same
        character(len=:), allocatable :: why

        do c = 1, size(cols)
            call parquet_open_reader(r, PAYLOADS)
            call parquet_read_column(r, trim(cols(c)), orig)
            call parquet_close_reader(r)
            call parquet_open_writer(w, path)
            call parquet_write_column(w, trim(cols(c)), orig)
            call parquet_close_writer(w)
            call parquet_open_reader(r, path)
            call parquet_read_column(r, trim(cols(c)), back)
            call parquet_close_reader(r)
            call maps_equal(orig, back, same, why)
            call check(error, same, trim(cols(c))//" survives a re-write: "//why)
            if (allocated(error)) return
        end do
    end subroutine test_rewrite_fixture

    !> The third oracle: the written file really holds a `MAP`, not something the reader tolerated.
    subroutine test_shape_is_map(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc
        character(len=*), parameter :: path = "test_run/map_write_shape.parquet"
        character(len=:), allocatable :: txt

        call mc%init(PK_FLOAT64)
        call mc%append_row(["a"], [1.5_real64])
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "m", mc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, path)
        call parquet_get_column_shape(r, "m", txt)
        call check(error, txt == "map", "the written column's shape is 'map', got '"//txt//"'")
        if (allocated(error)) return
        call parquet_get_column_type(r, "m", txt)
        call check(error, trim(txt) == "unknown", &
            "and it has no single element type, got '"//trim(txt)//"'")
        call parquet_close_reader(r)
    end subroutine test_shape_is_map

    !> A schema-enforced writer accepts a `map[<valuetype>]` declaration, and the column written
    !> under it round-trips.
    subroutine test_maml_declaration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_schema) :: sch
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc, back
        character(len=*), parameter :: path = "test_run/map_write_maml.parquet"
        logical :: same
        character(len=:), allocatable :: why

        call sch%init("maps")
        call sch%add_field("m", "map[int32]")
        call parquet_parse_maml(sch)
        call mc%init(PK_INT32)
        call mc%append_row(["k"], [3_int32])
        call parquet_open_writer(w, path, sch)
        call parquet_write_column(w, "m", mc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "m", back)
        call parquet_close_reader(r)
        call maps_equal(mc, back, same, why)
        call check(error, same, "a schema-declared map column round-trips: "//why)
    end subroutine test_maml_declaration

    !> Writing row group by row group produces the same column as one whole-column write.
    subroutine test_chunked_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: whole, part, back
        character(len=*), parameter :: path = "test_run/map_write_chunked.parquet"
        logical :: same
        character(len=:), allocatable :: why

        ! The same four rows, once as a whole column and once split 2 + 2 -- with the null row and
        ! the empty row landing in different groups.
        call whole%init(PK_INT32)
        call whole%append_row(["a", "b"], [10_int32, 20_int32])
        call whole%append_null_row()
        call whole%append_empty_row()
        call whole%append_row(["c"], [30_int32])

        call parquet_open_writer(w, path)
        call parquet_new_row_group(w, 2_int64)
        call part%init(PK_INT32)
        call part%append_row(["a", "b"], [10_int32, 20_int32])
        call part%append_null_row()
        call parquet_write_column_chunk(w, "m", part)
        call parquet_finish_row_group(w)
        call parquet_new_row_group(w, 2_int64)
        call part%init(PK_INT32)
        call part%append_empty_row()
        call part%append_row(["c"], [30_int32])
        call parquet_write_column_chunk(w, "m", part)
        call parquet_finish_row_group(w)
        call parquet_close_writer(w)

        call parquet_open_reader(r, path)
        call parquet_read_column(r, "m", back)
        call parquet_close_reader(r)
        call maps_equal(whole, back, same, why)
        call check(error, same, "a chunked write reproduces the whole column: "//why)
    end subroutine test_chunked_matches

    !> Duplicate keys and their order survive a write, which is the half of the promise the read
    !> tests cannot make: they assert what Arrow's writer stored, this asserts what ours does.
    subroutine test_duplicates_survive(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc, back
        type(parquet_map_row) :: row
        character(len=*), parameter :: path = "test_run/map_write_dup.parquet"
        integer(int32) :: v
        character(len=:), allocatable :: k

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b", "a"], [1_int32, 2_int32, 3_int32])
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "m", mc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, path)
        call parquet_read_column(r, "m", back)
        call parquet_close_reader(r)

        row = back%view(1_int64)
        call check(error, row%size() == 3, "all three entries survived the write")
        if (allocated(error)) return
        call check(error, row%key_count("a") == 2, "both 'a' entries survived")
        if (allocated(error)) return
        call row%key_at(2, k)
        call check(error, k == "b", "the middle key kept its position, got '"//k//"'")
        if (allocated(error)) return
        call row%get("a", v)
        call check(error, v == 1_int32, "the first 'a' is still first")
        if (allocated(error)) return
        call row%get("a", v, occurrence=2)
        call check(error, v == 3_int32, "and the second 'a' is still second")
    end subroutine test_duplicates_survive

    !> A writer row mask drops rows before the column is written, and the map column has to be
    !> rebuilt -- keys, values, offsets and row bitmap together -- rather than written whole.
    subroutine test_row_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: mc, back
        type(parquet_map_row) :: row
        character(len=*), parameter :: path = "test_run/map_write_mask.parquet"
        integer(int32) :: v

        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_row(["b"], [2_int32])
        call mc%append_row(["c"], [3_int32])
        call parquet_open_writer(w, path)
        call parquet_write_row_mask(w, [.true., .false., .true.])
        call parquet_write_column(w, "m", mc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, path)
        call parquet_read_column(r, "m", back)
        call parquet_close_reader(r)
        call check(error, back%size() == 2_int64, "two rows survived the mask")
        if (allocated(error)) return
        row = back%view(1_int64)
        call row%get("a", v)
        call check(error, v == 1_int32, "the first survivor kept its entry")
        if (allocated(error)) return
        row = back%view(2_int64)
        call row%get("c", v)
        call check(error, v == 3_int32, "and the second survivor is the THIRD original row")
    end subroutine test_row_mask

    !> A `map[<valuetype>]` column declared in a schema but never written is written with zero
    !> rows at close, exactly as a `list[<elemtype>]` column is -- and reads back as an empty map
    !> column of the declared value kind rather than aborting.
    subroutine test_empty_at_close(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_schema) :: sch
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_map_column), target :: back
        character(len=*), parameter :: path = "test_run/map_write_empty.parquet"
        character(len=:), allocatable :: saved
        !> The nine value tokens `parquet_parse_map_type` reports, and the kind each must resolve
        !! to. The close-time path maps token back to kind through a `select case` with one arm
        !! per token, and a wrong arm still produces a file that opens and reads back as an empty
        !! map -- of the wrong value type. Kept in step by position.
        character(len=9), parameter :: TOKENS(9) = [character(len=9) :: "int32", "int64", &
            "float32", "float64", "boolean", "string", "date", "time", "timestamp"]
        integer, parameter :: KINDS(9) = [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, &
            PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP]
        integer :: k

        ! "errors_only", not "silent": close warns that no column was written, and only
        ! errors_only silences a WARNING (silent covers informational and solicited output).
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("errors_only")
        call sch%init("maps")
        do k = 1, size(TOKENS)
            call sch%add_field("m_"//trim(TOKENS(k)), "map["//trim(TOKENS(k))//"]")
        end do
        call parquet_parse_maml(sch)
        call parquet_open_writer(w, path, sch)
        call parquet_close_writer(w)
        call parquet_set_verbosity(saved)

        call parquet_open_reader(r, path)
        do k = 1, size(TOKENS)
            call parquet_read_column(r, "m_"//trim(TOKENS(k)), back)
            call check(error, back%size() == 0_int64, &
                "the unwritten map["//trim(TOKENS(k))//"] column has no rows")
            if (allocated(error)) exit
            call check(error, back%element_kind() == KINDS(k), &
                "and kept the declared value kind, which is what the token had to carry")
            if (allocated(error)) exit
        end do
        call parquet_close_reader(r)
    end subroutine test_empty_at_close

end module test_map_write
