!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for `parquet_struct_column` and `parquet_struct_row` in memory -- build,
!> fill, null, query, copy and rebuild -- with no file and no Arrow anywhere.
!!
!! The struct column's counterpart to `test_columns`/`test_list`. What it exists to pin, beyond
!! the ordinary accessor behaviour, is the **two independent null levels**: a null struct row (the
!! instance is absent) and a null field value inside a present row are different states, and a row
!! whose every field is null is emphatically NOT a null row. Nearly every test here checks the
!! pair rather than one of them, because an implementation that collapses the two passes any test
!! that only ever looks at one.
module test_struct
    use testdrive, only : new_unittest, unittest_type, error_type, check
    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_struct
    use parquet_columns
    use parquet_temporal
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_struct

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_struct(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("struct init fixes the field set", test_init_fixes_fields), &
            new_unittest("struct append_row is present with null fields", test_append_row), &
            new_unittest("struct append_from concatenates field by field", test_append_from), &
            new_unittest("struct null row differs from all-fields-null", test_null_row_vs_all_null), &
            new_unittest("struct set_field by name and by index agree", test_name_and_index_agree), &
            new_unittest("struct every field kind round-trips in memory", test_all_kinds), &
            new_unittest("struct handle narrowing and get_field agree", test_handle_forms_agree), &
            new_unittest("struct gather_rows rebuilds every field", test_gather_rows), &
            new_unittest("struct deep_copy is independent", test_deep_copy), &
            new_unittest("struct move_from empties the source", test_move_from), &
            new_unittest("struct clear_null restores the field values", test_clear_null_restores), &
            new_unittest("struct kind_text and summary describe the column", test_kind_text), &
            new_unittest("struct field_index answers 0 for an unknown name", test_field_index_zero), &
            new_unittest("struct soft-fail field returns an invalid handle", test_soft_fail_field), &
            new_unittest("struct adopt_fields builds from moved-in columns", test_adopt_fields), &
            new_unittest("struct ensure_validity covers both levels", test_ensure_validity), &
            new_unittest("struct grow_rows appends null rows", test_grow_rows), &
            new_unittest("struct set_field agrees across all four addressing forms", &
                test_set_field_every_addressing_form), &
            new_unittest("struct set_field_null nulls a field, not the row", &
                test_set_field_null_every_form), &
            new_unittest("struct reserve and shrink_to_fit keep the rows", &
                test_reserve_and_shrink), &
            new_unittest("struct container row bindings agree with the public ones", &
                test_container_row_bindings), &
            new_unittest("struct row handle reports its row, not its narrowing", &
                test_row_handle_introspection), &
            new_unittest("struct kind_text spells every field kind", &
                test_kind_text_every_kind), &
            new_unittest("struct validity bitmap keeps its bits when it grows", &
                test_validity_bitmap_grows), &
            new_unittest("struct uninitialized column answers its non-fatal queries", &
                test_uninitialized_column_queries) &
            ]
    end subroutine collect_tests_parquet_struct

    !> `%init` fixes names, kinds and count, and answers for an EMPTY column -- which is what the
    !> writer needs before any row exists.
    subroutine test_init_fixes_fields(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        character(len=:), allocatable :: nm
        call sc%init(["id ", "nm "], [PK_INT32, PK_STRING])
        call check(error, sc%is_init(), "a column %init has run on reports is_init")
        if (allocated(error)) return
        call check(error, sc%field_count() == 2, "%field_count answers the declared count")
        if (allocated(error)) return
        call check(error, sc%size() == 0_int64, "%init with no nrows leaves the column empty")
        if (allocated(error)) return
        call sc%field_name(1, nm)
        call check(error, nm == "id", "%field_name(1) is the first declared name")
        if (allocated(error)) return
        call sc%field_name(2, nm)
        call check(error, nm == "nm", "%field_name(2) is the second declared name")
        if (allocated(error)) return
        call check(error, sc%field_kind(1) == PK_INT32, "%field_kind(1) is the declared kind")
        if (allocated(error)) return
        call check(error, sc%field_kind(2) == PK_STRING, "%field_kind(2) is the declared kind")
        if (allocated(error)) return
        call check(error, sc%field_index("nm") == 2, "%field_index finds a declared name")
    end subroutine test_init_fixes_fields

    !> `%append_row` adds a PRESENT row whose every field is null -- the only sane default, since
    !> the fields are filled one at a time afterwards.
    subroutine test_append_row(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        logical :: ok
        call sc%init(["id "], [PK_INT32])
        call sc%append_row()
        call check(error, sc%size() == 1_int64, "%append_row adds one row")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(1), "a row from %append_row is PRESENT")
        if (allocated(error)) return
        h = sc%view(1)
        call h%get_field("id", v, is_valid=ok)
        call check(error, .not. ok, "every field of a fresh %append_row row is null")
        if (allocated(error)) return
        call sc%set_field(1, "id", 7_int32)
        call h%get_field("id", v, is_valid=ok)
        call check(error, ok .and. v == 7, "%set_field writes the value and clears its null")
    end subroutine test_append_row

    !> **The test this suite exists for.** A null struct row and a present row whose every field
    !> is null give the same answer to every FIELD query and different answers to `%is_null`.
    !> Collapsing the two is a silent wrong answer that any single-level test would miss.
    subroutine test_null_row_vs_all_null(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        logical :: ok1, ok2
        call sc%init(["id "], [PK_INT32])
        call sc%append_null_row()   ! row 1: the struct instance is absent
        call sc%append_row()        ! row 2: present, its one field never set (so null)
        call check(error, sc%is_null(1), "an %append_null_row row reports is_null")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(2), "a present row with every field null is NOT a null row")
        if (allocated(error)) return
        h = sc%view(1)
        call h%get_field("id", v, is_valid=ok1)
        h = sc%view(2)
        call h%get_field("id", v, is_valid=ok2)
        call check(error, (.not. ok1) .and. (.not. ok2), &
            "the field is null in both rows -- which is why %is_null must separate them")
        if (allocated(error)) return
        call check(error, sc%null_count() == 1_int64, "%null_count counts null ROWS only")
    end subroutine test_null_row_vs_all_null

    !> The name form and the index form of `%set_field` write the same cell, for both row-index
    !> kinds -- 36 specifics whose whole point is that they agree.
    subroutine test_name_and_index_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a, b, c, d
        integer(int64) :: r64
        integer :: k
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%append_row(); call sc%append_row(); call sc%append_row()
        k = sc%field_index("v")
        r64 = 2_int64
        call sc%set_field(1, "v", 11_int32)          ! int32 row, name
        call sc%set_field(r64, "v", 22_int32)        ! int64 row, name
        call sc%set_field(3, k, 33_int32)            ! int32 row, index
        call sc%set_field(4_int64, k, 44_int32)      ! int64 row, index
        h = sc%view(1); call h%get_field("v", a)
        h = sc%view(2); call h%get_field("v", b)
        h = sc%view(3); call h%get_field("v", c)
        h = sc%view(4); call h%get_field("v", d)
        call check(error, a == 11 .and. b == 22 .and. c == 33 .and. d == 44, &
            "all four set_field families write the cell they name")
    end subroutine test_name_and_index_agree

    !> All nine field kinds in ONE struct, written and read back, including a null in each.
    subroutine test_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a
        integer(int64) :: b
        real(real32) :: c
        real(real64) :: d
        logical :: e, ok
        character(len=:), allocatable :: f
        type(parquet_date) :: g
        type(parquet_time) :: t
        type(parquet_timestamp) :: u
        call sc%init(["a", "b", "c", "d", "e", "f", "g", "t", "u"], &
            [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP])
        call sc%append_row()
        call sc%set_field(1, "a", 5_int32)
        call sc%set_field(1, "b", 6_int64)
        call sc%set_field(1, "c", 1.5_real32)
        call sc%set_field(1, "d", 2.5_real64)
        call sc%set_field(1, "e", .true.)
        call sc%set_field(1, "f", "hello")
        call g%set(2026, 8, 26)
        call sc%set_field(1, "g", g)
        call t%set(13, 45, 0)
        call sc%set_field(1, "t", t)
        call u%set(g, t)
        call sc%set_field(1, "u", u)
        h = sc%view(1)
        call h%get_field("a", a); call check(error, a == 5, "int32 field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("b", b); call check(error, b == 6_int64, "int64 field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("c", c); call check(error, abs(c - 1.5_real32) < 1.0e-6_real32, "float32 field round-trips")
        if (allocated(error)) return
        call h%get_field("d", d); call check(error, abs(d - 2.5_real64) < 1.0e-12_real64, "float64 field round-trips")
        if (allocated(error)) return
        call h%get_field("e", e); call check(error, e, "logical field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("f", f); call check(error, f == "hello", "string field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("g", g, is_valid=ok)
        call check(error, ok .and. g%year() == 2026 .and. g%month() == 8 .and. g%day() == 26, &
            "date field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("t", t, is_valid=ok)
        call check(error, ok .and. t%hour() == 13 .and. t%minute() == 45, "time field round-trips in memory")
        if (allocated(error)) return
        call h%get_field("u", u, is_valid=ok)
        g = u%get_date()
        call check(error, ok .and. g%year() == 2026 .and. g%month() == 8 .and. g%day() == 26, &
            "timestamp field round-trips in memory")
    end subroutine test_all_kinds

    !> `%get_field(name, v)` and narrowing with `%field(name)` then `%get(v)` are the same read.
    !>
    !> Both forms exist because `call h%field("x")%get(v)` does NOT compile -- F2018 R1522 makes a
    !> procedure-designator a `data-ref % binding-name` and a function reference is not one, which
    !> gfortran and nagfor both reject. So the two forms are what a caller actually has.
    subroutine test_handle_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, slot
        integer(int32) :: a, b
        logical :: ok1, ok2
        call sc%init(["v  ", "w  "], [PK_INT32, PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 9_int32)
        h = sc%view(1)
        call h%get_field("v", a, is_valid=ok1)
        slot = h%field("v")
        call check(error, slot%is_valid(), "a narrowed handle is valid")
        if (allocated(error)) return
        call check(error, slot%is_narrowed(), "%is_narrowed reports the narrowing")
        if (allocated(error)) return
        call check(error, slot%field_kind() == PK_INT32, "%field_kind answers the narrowed field's kind")
        if (allocated(error)) return
        call slot%get(b, is_valid=ok2)
        call check(error, a == b .and. (ok1 .eqv. ok2), "the named and narrowed forms are the same read")
    end subroutine test_handle_forms_agree

    !> `%gather_rows` rebuilds every field and the row bitmap together -- the operation behind
    !> `%filter_rows`, `%sort_by` and a masked write.
    subroutine test_gather_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 10_int32)
        call sc%append_null_row()
        call sc%append_row(); call sc%set_field(3, "v", 30_int32)
        call sc%gather_rows([3_int64, 1_int64, 2_int64])
        call check(error, sc%size() == 3_int64, "%gather_rows keeps the requested row count")
        if (allocated(error)) return
        h = sc%view(1); call h%get_field("v", v)
        call check(error, v == 30, "%gather_rows moved the field value with its row")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(1) .and. .not. sc%is_null(2) .and. sc%is_null(3), &
            "%gather_rows moved ROW nullness with its row")
    end subroutine test_gather_rows

    !> `%deep_copy` shares nothing: mutating the copy leaves the source alone, at both levels.
    subroutine test_deep_copy(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc, cp
        type(parquet_struct_row) :: h
        integer(int32) :: v
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 1_int32)
        call sc%deep_copy(cp)
        call cp%set_field(1, "v", 99_int32)
        call cp%set_null(1)
        h = sc%view(1); call h%get_field("v", v)
        call check(error, v == 1, "mutating a deep copy's field leaves the source's alone")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(1), "nulling a deep copy's row leaves the source's alone")
        if (allocated(error)) return
        call check(error, cp%field_count() == 1, "the copy carries the field set")
    end subroutine test_deep_copy

    !> `%move_from` transfers the storage and leaves the source uninitialized.
    subroutine test_move_from(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: src, dst
        type(parquet_struct_row) :: h
        integer(int32) :: v
        call src%init(["v  "], [PK_INT32])
        call src%append_row(); call src%set_field(1, "v", 42_int32)
        call dst%move_from(src)
        call check(error, dst%size() == 1_int64, "%move_from carries the rows across")
        if (allocated(error)) return
        h = dst%view(1); call h%get_field("v", v)
        call check(error, v == 42, "%move_from carries the field values across")
        if (allocated(error)) return
        call check(error, .not. src%is_init(), "%move_from leaves the source uninitialized")
        if (allocated(error)) return
        call check(error, src%size() == 0_int64, "%move_from leaves the source empty")
    end subroutine test_move_from

    !> `%set_null` leaves the row's field values in place, so `%clear_null` restores them intact.
    !> That is what makes `%set_null` O(1) rather than O(fields), and it is deliberate.
    subroutine test_clear_null_restores(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        logical :: ok
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row(); call sc%set_field(1, "v", 77_int32)
        call sc%set_null(1)
        call check(error, sc%is_null(1), "%set_null marks the row absent")
        if (allocated(error)) return
        call sc%clear_null(1)
        call check(error, .not. sc%is_null(1), "%clear_null marks the row present again")
        if (allocated(error)) return
        h = sc%view(1); call h%get_field("v", v, is_valid=ok)
        call check(error, ok .and. v == 77, "%set_null left the field value in place for %clear_null to restore")
    end subroutine test_clear_null_restores

    !> `%kind_text` and `%summary` describe the column in the MAML type spelling.
    subroutine test_kind_text(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        character(len=:), allocatable :: txt
        call sc%init(["id ", "nm "], [PK_INT32, PK_STRING])
        call sc%append_row(); call sc%append_null_row()
        call sc%kind_text(txt)
        call check(error, txt == "struct<id:int32,nm:string>", "%kind_text spells the field set")
        if (allocated(error)) return
        call sc%summary(txt)
        call check(error, index(txt, "2 rows") > 0 .and. index(txt, "2 fields") > 0 .and. &
            index(txt, "1 null") > 0, "%summary reports rows, fields and null rows")
    end subroutine test_kind_text

    !> `%field_index` ANSWERS 0 rather than aborting -- it is the hoistable lookup and the
    !> presence test, so it must have a non-fatal miss.
    subroutine test_field_index_zero(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        call sc%init(["v  "], [PK_INT32])
        call check(error, sc%field_index("nope") == 0, "%field_index answers 0 for an unknown name")
        if (allocated(error)) return
        call check(error, sc%field_index("v") == 1, "%field_index answers the position for a known name")
    end subroutine test_field_index_zero

    !> `%field(name, warn=.true.)` returns an INVALID handle instead of aborting -- the campaign's
    !> soft-fail signal for a function that returns a handle and so has no `found=` to write into.
    subroutine test_soft_fail_field(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, slot
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        slot = h%field("nope", warn=.true.)
        call check(error, .not. slot%is_valid(), "a soft-failed %field returns an invalid handle")
        if (allocated(error)) return
        slot = h%field("v", warn=.true.)
        call check(error, slot%is_valid(), "warn= does not affect a name that IS found")
    end subroutine test_soft_fail_field

    !> `%adopt_fields` builds a whole column from moved-in field columns -- the bulk path the
    !> reader uses, and the one that derives the row count and every kind from what it was given.
    subroutine test_adopt_fields(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        type(parquet_column), allocatable :: flds(:)
        integer(int32) :: v
        logical :: ok
        allocate(flds(2))
        call flds(1)%init(PK_INT32, 3_int64)
        call flds(2)%init(PK_STRING, 3_int64)
        call flds(1)%set_at(1_int64, 10_int32)
        call flds(1)%set_at(2_int64, 20_int32)
        call flds(1)%set_at(3_int64, 30_int32)
        call sc%adopt_fields(["a  ", "b  "], flds, row_valid=[.true., .false., .true.])
        call check(error, .not. allocated(flds), "%adopt_fields moves the field array in")
        if (allocated(error)) return
        call check(error, sc%size() == 3_int64, "%adopt_fields takes the row count from the fields")
        if (allocated(error)) return
        call check(error, sc%field_kind(1) == PK_INT32 .and. sc%field_kind(2) == PK_STRING, &
            "%adopt_fields takes each field's kind from the column it was given")
        if (allocated(error)) return
        call check(error, sc%is_null(2) .and. .not. sc%is_null(1), "%adopt_fields applies row_valid")
        if (allocated(error)) return
        h = sc%view(3); call h%get_field("a", v, is_valid=ok)
        call check(error, ok .and. v == 30, "%adopt_fields keeps the values it was handed")
    end subroutine test_adopt_fields

    !> `%ensure_validity` materializes BOTH levels' storage, which is what a caller runs before a
    !> parallel region so the first null cannot race with a lazy allocation.
    subroutine test_ensure_validity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        call check(error, .not. sc%has_validity_storage(), "the row bitmap starts LAZY")
        if (allocated(error)) return
        call sc%ensure_validity()
        call check(error, sc%has_validity_storage(), "%ensure_validity materializes the row bitmap")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(1), "%ensure_validity does not make any row null")
    end subroutine test_ensure_validity

    !> `%grow_rows` appends null rows, which is how `parquet_column` grows a container column.
    subroutine test_grow_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        call sc%init(["v  "], [PK_INT32])
        call sc%grow_rows(3_int64)
        call check(error, sc%size() == 3_int64, "%grow_rows appends the requested rows")
        if (allocated(error)) return
        call check(error, sc%null_count() == 3_int64, "%grow_rows appends NULL rows")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column is still valid after %grow_rows")
    end subroutine test_grow_rows

    !> `append_from` concatenates one struct column onto another, field column by field column.
    !>
    !> A struct has no offsets to rebase, so the arithmetic a list and a map can get wrong does
    !> not exist here. What it has instead is a LAYOUT that two columns can appear to share while
    !> differing: same field count, different names or different kinds. `struct<a,b>` and
    !> `struct<b,a>` are the sharp case -- identical field sets, and appending one onto the other
    !> would transpose two columns silently, so the check is on field name IN ORDER, not on the
    !> set of names.
    subroutine test_append_from(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: dst, src
        type(parquet_struct_row) :: h
        integer(int32) :: v
        logical :: ok

        call dst%init(["id  ", "size"], [PK_INT32, PK_INT32])
        call dst%append_row()
        call dst%set_field(1, "id", 10_int32)
        call dst%set_field(1, "size", 100_int32)
        call dst%append_null_row()

        call src%init(["id  ", "size"], [PK_INT32, PK_INT32])
        call src%append_row()
        call src%set_field(1, "id", 20_int32)
        call src%set_field(1, "size", 200_int32)
        call src%append_null_row()

        call dst%append_from(src)
        call check(error, dst%size() == 4_int64, "every row of both columns survives")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(1), "the destination's own present row is still present")
        if (allocated(error)) return
        call check(error, dst%is_null(2), "and its null row is still null")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(3), "the appended present row arrived present")
        if (allocated(error)) return
        call check(error, dst%is_null(4), "and the appended NULL row arrived null, not present")
        if (allocated(error)) return
        ! Both fields of an appended row, because appending one field column and not the other
        ! gives the right row count with a field silently left behind.
        h = dst%view(3)
        call h%get_field("id", v, is_valid=ok)
        call check(error, ok .and. v == 20, "the appended row's 'id' came across")
        if (allocated(error)) return
        call h%get_field("size", v, is_valid=ok)
        call check(error, ok .and. v == 200, "and so did its 'size' -- both field columns grew")
        if (allocated(error)) return
        h = dst%view(1)
        call h%get_field("size", v, is_valid=ok)
        call check(error, ok .and. v == 100, "the destination's own values are untouched")
        if (allocated(error)) return
        call check(error, dst%validate(), "the concatenated column satisfies its own invariants")
        if (allocated(error)) return
        call check(error, src%size() == 2_int64, "the source column is unchanged")
        if (allocated(error)) return
        ! An EMPTY source is a no-op, and returns before the layout is even compared -- so a
        ! mismatched but empty column must not abort. The destination has to be left exactly as
        ! it was, which is what the row count and the surviving value check.
        block
            type(parquet_struct_column) :: empty
            integer(int64) :: before
            before = dst%size()
            call empty%init(["wholly_different"], [PK_INT64])
            call dst%append_from(empty)
            call check(error, dst%size() == before, &
                "appending an empty column must add no row")
            if (allocated(error)) return
            h = dst%view(1)
            call h%get_field("size", v, is_valid=ok)
            call check(error, ok .and. v == 100, &
                "and must leave the destination's values alone")
            if (allocated(error)) return
            call check(error, dst%validate(), "and leave it valid")
        end block
    end subroutine test_append_from

    !> `%set_field` answers the same whichever of its four addressing forms is used.
    !!
    !! The generic has thirty-six specifics: nine value types across `{field index, field name}`
    !! times `{int32 row, int64 row}`. Only the (index, int64) family does any work; the other
    !! twenty-seven forward onto it, which is exactly why they need driving -- a forwarder that
    !! transposed its row and field arguments, or widened the wrong one, still compiles and writes
    !! a plausible cell. `test_all_kinds` drives the (name, int32) family; this drives the other
    !! two and asserts all four agree, value for value, on one row each.
    subroutine test_set_field_every_addressing_form(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a
        integer(int64) :: b
        real(real32) :: c
        real(real64) :: d
        logical :: e, ok
        character(len=:), allocatable :: f
        type(parquet_date) :: g
        type(parquet_time) :: t
        type(parquet_timestamp) :: u
        integer(int64) :: row
        integer :: r

        call sc%init(["a", "b", "c", "d", "e", "f", "g", "t", "u"], &
            [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP])
        call sc%append_row()
        call sc%append_row()
        call g%set(2026, 8, 26)
        call t%set(13, 45, 0)
        call u%set(g, t)
        ! Row 1 through the (field INDEX, int32 row) family; row 2 through (field NAME, int64 row).
        ! The same nine values both times, so the two rows must come back identical.
        call sc%set_field(1_int32, 1, 5_int32)
        call sc%set_field(1_int32, 2, 6_int64)
        call sc%set_field(1_int32, 3, 1.5_real32)
        call sc%set_field(1_int32, 4, 2.5_real64)
        call sc%set_field(1_int32, 5, .true.)
        call sc%set_field(1_int32, 6, "hello")
        call sc%set_field(1_int32, 7, g)
        call sc%set_field(1_int32, 8, t)
        call sc%set_field(1_int32, 9, u)
        row = 2_int64
        call sc%set_field(row, "a", 5_int32)
        call sc%set_field(row, "b", 6_int64)
        call sc%set_field(row, "c", 1.5_real32)
        call sc%set_field(row, "d", 2.5_real64)
        call sc%set_field(row, "e", .true.)
        call sc%set_field(row, "f", "hello")
        call sc%set_field(row, "g", g)
        call sc%set_field(row, "t", t)
        call sc%set_field(row, "u", u)
        do r = 1, 2
            h = sc%view(r)
            call h%get_field("a", a)
            call check(error, a == 5_int32, "int32 field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("b", b)
            call check(error, b == 6_int64, "int64 field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("c", c)
            call check(error, abs(c - 1.5_real32) < 1.0e-6_real32, "float32 field agrees")
            if (allocated(error)) return
            call h%get_field("d", d)
            call check(error, abs(d - 2.5_real64) < 1.0e-12_real64, "float64 field agrees")
            if (allocated(error)) return
            call h%get_field("e", e)
            call check(error, e, "logical field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("f", f)
            call check(error, f == "hello", "string field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("g", g, is_valid=ok)
            call check(error, ok .and. g%year() == 2026 .and. g%month() == 8 .and. g%day() == 26, &
                "date field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("t", t, is_valid=ok)
            call check(error, ok .and. t%hour() == 13 .and. t%minute() == 45, &
                "time field agrees across the addressing forms")
            if (allocated(error)) return
            call h%get_field("u", u, is_valid=ok)
            g = u%get_date()
            call check(error, ok .and. g%year() == 2026 .and. g%day() == 26, &
                "timestamp field agrees across the addressing forms")
            if (allocated(error)) return
        end do
        call check(error, sc%validate(), "the column is still valid after both addressing forms")
    end subroutine test_set_field_every_addressing_form

    !> `%set_field_null` nulls ONE field and leaves the struct row itself present.
    !!
    !! The distinction this asserts is the module's central one: a null FIELD inside a present row
    !! is not the same thing as a null ROW. A specific that reached for the row bitmap instead of
    !! the field's would make the whole row absent -- every other field would read back null too,
    !! and `%null_count` would move -- so both are checked, not just the field that was nulled.
    !!
    !! All four addressing forms are driven for the same reason `%set_field`'s are: three of them
    !! are one-line forwarders whose arguments are easy to transpose.
    subroutine test_set_field_null_every_form(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a
        integer(int64) :: b, row
        logical :: ok
        integer :: r

        call sc%init(["a  ", "b  "], [PK_INT32, PK_INT64])
        do r = 1, 4
            call sc%append_row()
            call sc%set_field(int(r, int64), 1, 10_int32 * int(r, int32))
            call sc%set_field(int(r, int64), 2, 20_int64 * int(r, int64))
        end do
        ! One row per addressing form, nulling field "a" and leaving "b" alone.
        call sc%set_field_null(1_int64, 1)        ! int64 row, field index
        call sc%set_field_null(2_int32, 1)        ! int32 row, field index
        row = 3_int64
        call sc%set_field_null(row, "a")          ! int64 row, field name
        call sc%set_field_null(4_int32, "a")      ! int32 row, field name
        do r = 1, 4
            h = sc%view(r)
            call check(error, .not. h%is_null(), &
                "nulling a field must leave the struct ROW present")
            if (allocated(error)) return
            call h%get_field("a", a, is_valid=ok)
            call check(error, .not. ok, "the nulled field must read back as null")
            if (allocated(error)) return
            call h%get_field("b", b, is_valid=ok)
            call check(error, ok .and. b == 20_int64 * int(r, int64), &
                "the sibling field must keep its value")
            if (allocated(error)) return
        end do
        call check(error, sc%null_count() == 0_int64, &
            "no ROW became null, so the row null count must not move")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column is still valid after nulling fields")
    end subroutine test_set_field_null_every_form

    !> `%reserve` takes capacity in either index kind, and `%shrink_to_fit` gives it back.
    !!
    !! Capacity is not row count, and conflating the two is the failure worth catching: a reserve
    !! that appended rows, or a shrink that dropped them, would change the answers `%size` and the
    !! stored values give. Both are asserted around each call, so a capacity change that moved the
    !! data fails rather than merely reporting a different number.
    subroutine test_reserve_and_shrink(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: a

        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        call sc%set_field(1_int64, 1, 42_int32)
        call sc%reserve(64_int32)
        call check(error, sc%capacity() >= 64_int64, "reserve(int32) must raise the capacity")
        if (allocated(error)) return
        call check(error, sc%size() == 1_int64, "reserve must not append any row")
        if (allocated(error)) return
        call sc%reserve(256_int64)
        call check(error, sc%capacity() >= 256_int64, "reserve(int64) must raise the capacity")
        if (allocated(error)) return
        call check(error, sc%size() == 1_int64, "nor must the int64 form")
        if (allocated(error)) return
        h = sc%view(1)
        call h%get_field("v", a)
        call check(error, a == 42_int32, "the stored value must survive both reserves")
        if (allocated(error)) return
        call sc%shrink_to_fit()
        call check(error, sc%size() == 1_int64, "shrink_to_fit must not drop a row")
        if (allocated(error)) return
        h = sc%view(1)
        call h%get_field("v", a)
        call check(error, a == 42_int32, "nor the value in it")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column is still valid after shrink_to_fit")
        if (allocated(error)) return
        ! A column that was never appended to has no slack: the call is a request, not an assertion.
        block
            type(parquet_struct_column) :: fresh
            call fresh%init(["v  "], [PK_INT32])
            call fresh%shrink_to_fit()
            call check(error, fresh%size() == 0_int64, "shrink_to_fit on an empty column is a no-op")
        end block
    end subroutine test_reserve_and_shrink

    !> The three row-nullness bindings named for the container base agree with the public forms.
    !!
    !! `%is_null_row`, `%set_null_row` and `%reserve_rows` exist because `parquet_column`'s own
    !! bitmap is never allocated for a container kind, so the generic column-level spellings would
    !! answer about storage that is not there. Each has to reach this type's own row bitmap, and
    !! the way to see that it does is to ask the same question through both names.
    subroutine test_container_row_bindings(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        integer :: r

        call sc%init(["v  "], [PK_INT32])
        do r = 1, 3
            call sc%append_row()
            call sc%set_field(int(r, int64), 1, int(r, int32))
        end do
        call sc%reserve_rows(128_int64)
        call check(error, sc%capacity() >= 128_int64, "reserve_rows must raise the capacity")
        if (allocated(error)) return
        call check(error, sc%size() == 3_int64, "reserve_rows must not append a row")
        if (allocated(error)) return
        call check(error, .not. sc%is_null_row(2_int64), &
            "a present row must not report as a null row")
        if (allocated(error)) return
        call sc%set_null_row(2_int64)
        call check(error, sc%is_null_row(2_int64), "set_null_row must make that row null")
        if (allocated(error)) return
        call check(error, sc%is_null(2_int64), "and the public is_null must agree")
        if (allocated(error)) return
        call check(error, .not. sc%is_null_row(1_int64) .and. .not. sc%is_null_row(3_int64), &
            "and must leave its neighbours present")
        if (allocated(error)) return
        call check(error, sc%null_count() == 1_int64, "the row null count must be exactly one")
        if (allocated(error)) return
        ! Out of range answers .false. rather than stopping: this form is a query, not a guard.
        call check(error, .not. sc%is_null_row(0_int64) .and. .not. sc%is_null_row(99_int64), &
            "a row index outside the column answers .false.")
        if (allocated(error)) return
        call sc%clear_null_row(2_int64)
        call check(error, .not. sc%is_null_row(2_int64), "clear_null_row must restore it")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column is still valid afterwards")
    end subroutine test_container_row_bindings

    !> The row handle answers about the row it refers to, not about the column or a narrowed field.
    !!
    !! `%is_null` on the handle is deliberately always about the ROW -- a narrowed handle reports
    !! its field's nullness through `%get`'s `is_valid` instead -- so the assertion that matters is
    !! that narrowing does NOT change what `%is_null` answers. `%row_index`, `%field_count` and
    !! `%field_name` are the handle's other introspection, checked against the column they came
    !! from so that a handle pointing at the wrong row or column cannot pass.
    subroutine test_row_handle_introspection(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, n
        character(len=:), allocatable :: nm, colnm
        integer :: r

        call sc%init(["alpha", "beta "], [PK_INT32, PK_INT64])
        do r = 1, 3
            call sc%append_row()
            call sc%set_field(int(r, int64), 1, int(r, int32))
        end do
        call sc%set_null_row(2_int64)
        do r = 1, 3
            h = sc%view(r)
            call check(error, h%row_index() == int(r, int64), &
                "the handle must report the row it was taken from")
            if (allocated(error)) return
            call check(error, h%is_null() .eqv. (r == 2), &
                "the handle's is_null must agree with the column's row nullness")
            if (allocated(error)) return
            call check(error, h%field_count() == sc%field_count(), &
                "the handle must report the column's field count")
            if (allocated(error)) return
            call h%field_name(1, nm)
            call sc%field_name(1, colnm)
            call check(error, nm == colnm .and. nm == "alpha", &
                "the handle must report the column's field names")
            if (allocated(error)) return
            call h%field_name(2, nm)
            call check(error, nm == "beta", "including the second field's")
            if (allocated(error)) return
        end do
        ! Narrowing changes what %get reads, and must not change what %is_null answers.
        h = sc%view(2)
        n = h%field("alpha")
        call check(error, n%is_narrowed(), "the narrowed handle must report itself narrowed")
        if (allocated(error)) return
        call check(error, n%is_null(), &
            "a narrowed handle's is_null must still be about the ROW, not the field")
        if (allocated(error)) return
        call check(error, n%row_index() == 2_int64, "and must still name the same row")
    end subroutine test_row_handle_introspection

    !> `%kind_text` spells EVERY field kind, not just the two a small fixture uses.
    !!
    !! The kind-name table is one `select case` with an arm per `PK_*` value, and a wrong or
    !! missing arm shows up nowhere else: the column still stores and reads its values correctly,
    !! and only the text a schema or a diagnostic prints is wrong. Naming all nine in one string is
    !! also what makes an arm wired to the neighbouring kind visible, since each name appears once.
    subroutine test_kind_text_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        character(len=:), allocatable :: txt

        call sc%init(["a", "b", "c", "d", "e", "f", "g", "t", "u"], &
            [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP])
        call sc%kind_text(txt)
        call check(error, txt == "struct<a:int32,b:int64,c:float32,d:float64,e:logical," // &
            "f:string,g:date,t:time,u:timestamp>", &
            "%kind_text must spell every field kind, in declaration order")
    end subroutine test_kind_text_every_kind

    !> The row validity bitmap GROWS when rows are appended after it already exists.
    !!
    !! The bitmap is allocated lazily, at the first `%set_null`, sized for the rows there are then.
    !! Appending past that size has to reallocate it and carry the existing bits across -- a grow
    !! that dropped them would silently make an already-null row present again, which no later call
    !! reports. So the earlier null is re-checked AFTER the growth, which is the only place that
    !! loss would show.
    subroutine test_validity_bitmap_grows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        integer :: r

        call sc%init(["v  "], [PK_INT32])
        do r = 1, 4
            call sc%append_row()
        end do
        call sc%set_null_row(2_int64)
        call check(error, sc%is_null_row(2_int64), "the first null allocates the bitmap")
        if (allocated(error)) return
        ! Well past the first allocation's size, so the bitmap has to be reallocated.
        do r = 5, 200
            call sc%append_row()
        end do
        call sc%set_null_row(175_int64)
        call check(error, sc%is_null_row(2_int64), &
            "growing the bitmap must carry the existing null bits across")
        if (allocated(error)) return
        call check(error, sc%is_null_row(175_int64), "and must record the new one")
        if (allocated(error)) return
        call check(error, .not. sc%is_null_row(174_int64) .and. .not. sc%is_null_row(176_int64), &
            "without touching its neighbours")
        if (allocated(error)) return
        call check(error, sc%null_count() == 2_int64, "exactly two rows are null")
        if (allocated(error)) return
        call check(error, sc%validate(), "the column is still valid after the bitmap grew")
    end subroutine test_validity_bitmap_grows

    !> An uninitialized column answers every query it can, rather than stopping.
    !!
    !! `%init` fixes the field set, and the queries that do not need one must still work before it:
    !! `%kind_text` spells the empty struct, `%field_index` answers 0 for any name, and
    !! `%gather_rows` over an empty index is a no-op. Each is the non-fatal counterpart of a guard
    !! that DOES stop, so the property worth pinning is that they stay non-fatal -- a query that
    !! started aborting here would break every "is this column built yet?" test a caller writes.
    subroutine test_uninitialized_column_queries(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        character(len=:), allocatable :: txt
        integer(int64) :: none(0)

        call check(error, .not. sc%is_init(), "a fresh column reports itself uninitialized")
        if (allocated(error)) return
        ! An uninitialized column is a VALID column -- no fields and no rows is a consistent
        ! state, not a broken one, so %validate must say so rather than reserving .true. for
        ! columns that have been through %init.
        call check(error, sc%validate(), "an uninitialized column satisfies its invariants")
        if (allocated(error)) return
        call sc%kind_text(txt)
        call check(error, txt == "struct<>", "%kind_text spells an uninitialized column")
        if (allocated(error)) return
        call check(error, sc%field_index("anything") == 0, &
            "%field_index answers 0 on an uninitialized column")
        if (allocated(error)) return
        call sc%gather_rows(none)
        call check(error, sc%size() == 0_int64, &
            "%gather_rows over an empty index leaves an uninitialized column empty")
        if (allocated(error)) return
        ! And the same call on an INITIALIZED column drops every row rather than keeping them.
        block
            type(parquet_struct_column) :: built
            call built%init(["v  "], [PK_INT32])
            call built%append_row()
            call built%append_row()
            call built%gather_rows(none)
            call check(error, built%size() == 0_int64, &
                "%gather_rows over an empty index empties a built column")
            if (allocated(error)) return
            call check(error, built%validate(), "and leaves it valid")
        end block
    end subroutine test_uninitialized_column_queries

end module test_struct
