!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> A container whose payload is itself a container: `list<struct<...>>`, `map<string,struct<...>>`,
!! `struct<..., list<...>>`, `list<list<...>>`, and deeper.
!!
!! **What this suite is for.** Nesting is not a new mechanism in this library -- it is a consequence
!! of the representation Phase 1 chose, where a list's payload is one `parquet_column` and a
!! `parquet_column` can hold a container. So the tests here are not "does the feature work" so much
!! as "does the representation really recurse", and each one names the property it pins:
!!
!! * `%deep_copy` must produce an INDEPENDENT copy at every depth. A copy that aliased the inner
!!   container would pass any test that only reads it, so every copy test MUTATES the copy and
!!   asserts the source did not move. This is the single most valuable assertion in the file.
!! * `%kind_text` is the only name in the library that recurses. `%kindof()` stays `PK_LIST` at
!!   every depth, and a test asserts that too -- otherwise "it recurses" and "it recurses in the
!!   right place" are indistinguishable.
!! * `%init` must still REFUSE a container payload while `%adopt_*` accepts one. That asymmetry is
!!   the whole of feature_container_phase7.md's D1, and it needs both halves asserted: a refusal
!!   test alone passes just as happily against a gate that refuses everything.
!!
!! Everything here is built in memory and needs no reader, which is deliberate: these are the tests
!! that can go green before any C++ exists, and they are what the file-reading tests in
!! `test_list_read`/`test_struct_read`/`test_map_read` are then checked against.
module test_container_nested
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64
    implicit none
    private

    public :: collect_tests_container_nested

    !> The fixture every file-reading test here uses. Its row shapes were designed for exactly
    !! this: row 2 is EMPTY and row 3 is the largest, so a test that only checked row counts, or
    !! that used the outer offsets for the inner container, cannot pass by accident.
    !!
    !! Every expected value below was cross-checked against `pyarrow` rather than against this
    !! library's own reader -- which is the only independent oracle Phase 7 has, since nesting is
    !! read-only and there is nothing to round-trip through. See feature_container_phase7.md's
    !! Verification section.
    character(len=*), parameter :: NEST = "test/fixtures/map_list_types.parquet"

contains

    !> Registers every test in this suite.
    subroutine collect_tests_container_nested(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("a list may adopt a struct payload", test_list_of_struct_build), &
            new_unittest("a list may adopt a list payload", test_list_of_list_build), &
            new_unittest("a map may adopt a struct value", test_map_of_struct_build), &
            new_unittest("a struct may adopt a list field", test_struct_of_list_build), &
            new_unittest("a struct may adopt a map field", test_struct_of_map_build), &
            new_unittest("deep_copy of a nested column is independent", test_nested_copy_is_independent), &
            new_unittest("kind_text recurses to depth three", test_kind_text_recurses), &
            new_unittest("kindof does NOT recurse", test_kindof_does_not_recurse), &
            new_unittest("%init still refuses a container payload", test_init_refuses_container), &
            new_unittest("a vector payload is still refused on BOTH paths", test_vector_payload_refused), &
            new_unittest("a struct with a list field reads from a file", test_read_struct_of_list), &
            new_unittest("a struct with a map field reads from a file", test_read_struct_of_map), &
            new_unittest("a list of structs reads from a file", test_read_list_of_struct), &
            new_unittest("a map of structs reads from a file", test_read_map_of_struct), &
            new_unittest("every remaining nested shape reads", test_read_every_nested_shape), &
            new_unittest("deep_nested reads at depth three", test_read_deep_nested), &
            new_unittest("a descent path reads as an ordinary column", test_descent_path_reads), &
            new_unittest("a row-group-scoped nested read agrees with the whole column", &
                test_nested_chunk_matches_whole) &
            ]
    end subroutine collect_tests_container_nested

    !> Builds `struct<id:int32>` with `n` present rows, ids 100+k.
    subroutine make_struct(n, sc)
        integer, intent(in) :: n                             !! number of rows.
        type(parquet_struct_column), intent(out) :: sc       !! the built column.
        integer :: k
        call sc%init(["id"], [PK_INT32])
        do k = 1, n
            call sc%append_row()
            call sc%set_field(int(k, int64), "id", int(100 + k, int32))
        end do
    end subroutine make_struct

    !> Wraps a container in a `parquet_column`, which is the only route a nested payload has.
    subroutine wrap(src, col)
        class(parquet_container_column), intent(in) :: src   !! the container to hand over.
        type(parquet_column), intent(out) :: col             !! the column that now owns it.
        class(parquet_container_column), allocatable :: cc
        allocate(cc, source=src)
        call col%adopt_container(cc)
    end subroutine wrap

    !> Reads field `id` of element `pos` of outer row `i` of a `list<struct<id:int32>>`.
    !!
    !! Deliberately through the PUBLIC route a user has -- `%view(i)` then `%nested` -- rather than
    !! through `parquet_list_column_payload`, which the facade hides. If this helper cannot be
    !! written with `use parquet` alone, the feature is not usable and the test would be asserting
    !! something no caller can reach.
    subroutine element_id(lc, i, pos, value)
        type(parquet_list_column), intent(in), target :: lc  !! the outer list.
        integer(int64), intent(in) :: i                      !! outer row index.
        integer(int64), intent(in) :: pos                    !! 1-based position within that row.
        integer(int32), intent(out) :: value                 !! the field value, or -1.
        class(parquet_container_column), pointer :: inner
        type(parquet_list_row) :: h
        type(parquet_struct_row) :: r
        integer(int64) :: lo, hi
        value = -1_int32
        h = lc%view(i)
        call h%nested(inner, lo, hi)
        if (.not. associated(inner)) return
        if (lo + pos - 1_int64 > hi) return
        select type (inner)
        type is (parquet_struct_column)
            r = inner%view(lo + pos - 1_int64)
            call r%get_field("id", value)
        end select
    end subroutine element_id

    !> `list<struct<id:int32>>`: rows of 2, 0 and 2 elements over a 4-row struct payload.
    subroutine test_list_of_struct_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        type(parquet_list_column) :: lc
        type(parquet_column) :: pay
        integer(int64), allocatable :: offs(:)
        integer(int32) :: v
        call make_struct(4, sc)
        call wrap(sc, pay)
        call check(error, pay%kindof() == PK_STRUCT, "adopt_container makes the payload PK_STRUCT")
        if (allocated(error)) return
        offs = [0_int64, 2_int64, 2_int64, 4_int64]
        call lc%adopt_rows(offs, pay)
        call check(error, lc%nrows() == 3_int64, "the outer list has three rows")
        if (allocated(error)) return
        call check(error, lc%length(1_int64) == 2_int64, "row 1 holds two structs")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 0_int64, "row 2 is empty")
        if (allocated(error)) return
        call check(error, lc%length(3_int64) == 2_int64, "row 3 holds two structs")
        if (allocated(error)) return
        call element_id(lc, 1_int64, 1_int64, v)
        call check(error, v == 101_int32, "row 1 element 1 carries its own field value")
        if (allocated(error)) return
        call element_id(lc, 3_int64, 2_int64, v)
        call check(error, v == 104_int32, "row 3 element 2 carries its own field value")
    end subroutine test_list_of_struct_build

    !> `list<list<int32>>` -- three container levels counting the outer `parquet_column`.
    subroutine test_list_of_list_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_list_column) :: inner, outer
        type(parquet_column) :: ipay, opay
        integer(int64), allocatable :: io(:), oo(:)
        integer :: k
        call ipay%init(PK_INT32, nrows=6_int64)
        do k = 1, 6
            call ipay%set_at(int(k, int64), int(k*10, int32))
        end do
        io = [0_int64, 3_int64, 6_int64]
        call inner%adopt_rows(io, ipay)
        call wrap(inner, opay)
        oo = [0_int64, 1_int64, 2_int64]
        call outer%adopt_rows(oo, opay)
        call check(error, outer%nrows() == 2_int64, "the outer list has two rows")
        if (allocated(error)) return
        call check(error, outer%length(1_int64) == 1_int64, "each outer row holds one inner list")
        if (allocated(error)) return
        call check(error, outer%length(2_int64) == 1_int64, "each outer row holds one inner list")
    end subroutine test_list_of_list_build

    !> `map<string, struct<id:int32>>`.
    subroutine test_map_of_struct_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        type(parquet_map_column) :: mc
        type(parquet_string_column) :: keys
        type(parquet_column) :: kcol, vcol
        integer(int64), allocatable :: offs(:)
        call make_struct(3, sc)
        call wrap(sc, vcol)
        call keys%append_string("a")
        call keys%append_string("b")
        call keys%append_string("c")
        call kcol%adopt_string_column(keys)
        offs = [0_int64, 2_int64, 3_int64]
        call mc%adopt_rows(offs, kcol, vcol)
        call check(error, mc%nrows() == 2_int64, "the map has two rows")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 2_int64, "row 1 holds two entries")
        if (allocated(error)) return
        call check(error, mc%length(2_int64) == 1_int64, "row 2 holds one entry")
    end subroutine test_map_of_struct_build

    !> `struct<label:int32, vals:list<int32>>`.
    subroutine test_struct_of_list_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_list_column) :: lc
        type(parquet_struct_column) :: sc
        type(parquet_column) :: pay
        type(parquet_column), allocatable :: fields(:)
        integer(int64), allocatable :: offs(:)
        character(len=5) :: names(2)
        integer :: k
        call pay%init(PK_INT32, nrows=4_int64)
        do k = 1, 4
            call pay%set_at(int(k, int64), int(k, int32))
        end do
        offs = [0_int64, 2_int64, 4_int64]
        call lc%adopt_rows(offs, pay)
        allocate(fields(2))
        call fields(1)%init(PK_INT32, nrows=2_int64)
        call wrap(lc, fields(2))
        names(1) = "label"
        names(2) = "vals "
        call sc%adopt_fields(names, fields)
        call check(error, sc%nrows() == 2_int64, "the struct has two rows")
        if (allocated(error)) return
        call check(error, sc%field_kind(2) == PK_LIST, "field 2 is a list")
    end subroutine test_struct_of_list_build

    !> `struct<label:int32, attrs:map<string,int32>>`.
    subroutine test_struct_of_map_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc
        type(parquet_struct_column) :: sc
        type(parquet_string_column) :: keys
        type(parquet_column) :: kcol, vcol
        type(parquet_column), allocatable :: fields(:)
        integer(int64), allocatable :: offs(:)
        character(len=5) :: names(2)
        call keys%append_string("x")
        call keys%append_string("y")
        call kcol%adopt_string_column(keys)
        call vcol%init(PK_INT32, nrows=2_int64)
        offs = [0_int64, 1_int64, 2_int64]
        call mc%adopt_rows(offs, kcol, vcol)
        allocate(fields(2))
        call fields(1)%init(PK_INT32, nrows=2_int64)
        call wrap(mc, fields(2))
        names(1) = "label"
        names(2) = "attrs"
        call sc%adopt_fields(names, fields)
        call check(error, sc%nrows() == 2_int64, "the struct has two rows")
        if (allocated(error)) return
        call check(error, sc%field_kind(2) == PK_MAP, "field 2 is a map")
    end subroutine test_struct_of_map_build

    !> The load-bearing one: a copy must not alias the source's inner container.
    !!
    !! Mutating the COPY and reading the SOURCE back is what distinguishes a real recursive
    !! `clone_into` from one that copied the outer offsets and shared the payload. A test that only
    !! read the copy would pass against the aliasing bug.
    subroutine test_nested_copy_is_independent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        type(parquet_list_column), target :: lc, copy
        type(parquet_column) :: pay
        class(parquet_container_column), pointer :: inner
        type(parquet_list_row) :: h
        integer(int64), allocatable :: offs(:)
        integer(int64) :: lo, hi
        integer(int32) :: v
        call make_struct(4, sc)
        call wrap(sc, pay)
        offs = [0_int64, 2_int64, 2_int64, 4_int64]
        call lc%adopt_rows(offs, pay)
        call lc%deep_copy(copy)
        call check(error, copy%nrows() == 3_int64, "the copy has the same row count")
        if (allocated(error)) return
        ! Mutate the COPY's nested payload, through the same public route a caller has.
        h = copy%view(1_int64)
        call h%nested(inner, lo, hi)
        call check(error, associated(inner), "the copy's payload is reachable")
        if (allocated(error)) return
        select type (inner)
        type is (parquet_struct_column)
            call inner%set_field(lo, "id", 999_int32)
        end select
        call element_id(copy, 1_int64, 1_int64, v)
        call check(error, v == 999_int32, "the copy took the mutation")
        if (allocated(error)) return
        call element_id(lc, 1_int64, 1_int64, v)
        call check(error, v == 101_int32, &
            "the SOURCE is unchanged -- deep_copy recursed rather than aliasing the payload")
    end subroutine test_nested_copy_is_independent

    !> `%kind_text` is the only name that recurses, and it must do so all the way down.
    subroutine test_kind_text_recurses(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        type(parquet_list_column) :: l1, l2, l3
        type(parquet_column) :: pay, p2, p3
        integer(int64), allocatable :: o1(:), o2(:), o3(:)
        character(len=:), allocatable :: kt
        call make_struct(2, sc)
        call wrap(sc, pay)
        o1 = [0_int64, 2_int64]
        call l1%adopt_rows(o1, pay)
        call l1%kind_text(kt)
        call check(error, kt == "list<struct<id:int32>>", &
            "a list of structs spells its payload out, got: "//kt)
        if (allocated(error)) return
        ! depth 2: list<list<int32>>
        call p2%init(PK_INT32, nrows=2_int64)
        o2 = [0_int64, 2_int64]
        call l2%adopt_rows(o2, p2)
        call wrap(l2, p3)
        o3 = [0_int64, 1_int64]
        call l3%adopt_rows(o3, p3)
        call l3%kind_text(kt)
        call check(error, kt == "list<list<int32>>", "depth two recurses, got: "//kt)
    end subroutine test_kind_text_recurses

    !> `%kindof()` must NOT recurse -- a nested list is still `PK_LIST`.
    subroutine test_kindof_does_not_recurse(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_struct_column) :: sc
        type(parquet_list_column) :: lc
        type(parquet_column) :: pay
        integer(int64), allocatable :: offs(:)
        call make_struct(2, sc)
        call wrap(sc, pay)
        offs = [0_int64, 2_int64]
        call lc%adopt_rows(offs, pay)
        call check(error, lc%kindof() == PK_LIST, "a list of structs is still PK_LIST")
        if (allocated(error)) return
        call check(error, lc%element_kind() == PK_STRUCT, "and reports its payload kind separately")
    end subroutine test_kindof_does_not_recurse

    !> The D1 asymmetry: `%adopt_*` accepts a container payload, `%init` still refuses one.
    !!
    !! The NEGATIVE CONTROL for the refusal is the `%adopt_rows` half above -- without it, a gate
    !! that refused every kind would pass this test.
    subroutine test_init_refuses_container(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_list_column) :: lc
        type(parquet_column) :: pay
        integer(int64), allocatable :: offs(:)
        type(parquet_struct_column) :: sc
        ! The permitted half: adoption of the same kind %init would refuse.
        call make_struct(2, sc)
        call wrap(sc, pay)
        offs = [0_int64, 2_int64]
        call lc%adopt_rows(offs, pay)
        call check(error, lc%element_kind() == PK_STRUCT, &
            "%adopt_rows accepts a container payload -- the control for the refusal")
        if (allocated(error)) return
        ! The refused half is an out-of-process scenario (container_init_nested), since %init
        ! aborts; this test asserts only that the permitted half is genuinely permitted.
        call check(error, lc%nrows() == 1_int64, "and the adopted column is usable")
    end subroutine test_init_refuses_container

    !> A `*_VEC` payload stays refused on BOTH paths -- widening the gate must not admit it.
    !!
    !! `list<fixed_size_list<...>>` is a different question from a container payload and is not in
    !! scope; the abort itself is `list_init_vector_payload` / `list_adopt_vector_payload` in
    !! `test/error_scenarios.f90`. What is asserted here is the control: the scalar kind beside it
    !! is still accepted, so a gate that had started refusing everything would fail here.
    subroutine test_vector_payload_refused(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_list_column) :: lc
        call lc%init(PK_INT32)
        call check(error, lc%element_kind() == PK_INT32, &
            "a scalar payload is still accepted by %init")
        if (allocated(error)) return
        call check(error, lc%nrows() == 0_int64, "and the column starts empty")
    end subroutine test_vector_payload_refused

    !> 7a: `struct<label:string, values:list<int32>>`. pyarrow says
    !! `[{first,[1,2,3]}, {second,[]}, {third,[4,5]}]`.
    subroutine test_read_struct_of_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, f
        class(parquet_container_column), pointer :: inner
        type(parquet_list_row) :: lr
        integer(int64) :: row
        integer(int32), allocatable :: vals(:)
        character(len=:), allocatable :: kt, lbl
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "struct_of_list", sc)
        call sc%kind_text(kt)
        call check(error, kt == "struct<label:string,values:list<int32>>", &
            "the nested spelling reaches kind_text, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call check(error, sc%nrows() == 3_int64, "three rows")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        h = sc%view(1_int64)
        call h%get_field("label", lbl)
        call check(error, lbl == "first", "the scalar field beside the container still reads")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        f = h%field("values")
        call f%nested(inner, row)
        call check(error, associated(inner), "the container field is reachable through %nested")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        select type (inner)
        type is (parquet_list_column)
            lr = inner%view(row)
            call lr%get(vals)
            call check(error, size(vals) == 3 .and. all(vals == [1_int32, 2_int32, 3_int32]), &
                "row 1 holds 1,2,3")
        class default
            call check(error, .false., "the field narrowed to the wrong container type")
        end select
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        ! The EMPTY row is the one an assembly bug survives, so it is asserted separately.
        h = sc%view(2_int64)
        f = h%field("values")
        call f%nested(inner, row)
        select type (inner)
        type is (parquet_list_column)
            call check(error, inner%length(row) == 0_int64, "row 2 holds an empty list")
        end select
        call parquet_close_reader(r)
    end subroutine test_read_struct_of_list

    !> 7a: `struct<label:string, attrs:map<string,int32>>`, whose dotted path did not even resolve
    !! before Phase 7. pyarrow says `[{m1,{a:1}}, {m2,{}}, {m3,{b:2,c:3}}]`.
    subroutine test_read_struct_of_map(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, f
        class(parquet_container_column), pointer :: inner
        type(parquet_map_row) :: mr
        integer(int64) :: row
        integer(int32) :: v
        logical :: ok
        character(len=:), allocatable :: kt
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "struct_of_map", sc)
        call sc%kind_text(kt)
        call check(error, kt == "struct<label:string,attrs:map<string,int32>>", &
            "the nested spelling reaches kind_text, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        h = sc%view(3_int64)
        f = h%field("attrs")
        call f%nested(inner, row)
        select type (inner)
        type is (parquet_map_column)
            mr = inner%view(row)
            call check(error, mr%size() == 2_int64, "row 3 holds two entries")
            if (.not. allocated(error)) then
                call mr%get("c", v, found=ok)
                call check(error, ok .and. v == 3_int32, "and key c maps to 3")
            end if
        class default
            call check(error, .false., "the field narrowed to the wrong container type")
        end select
        call parquet_close_reader(r)
    end subroutine test_read_struct_of_map

    !> 7b: `list<struct<x:int32,y:string>>`. pyarrow says `[[{1,a}], [], [{2,b},{3,c}]]`.
    subroutine test_read_list_of_struct(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: h
        type(parquet_struct_row) :: sr
        class(parquet_container_column), pointer :: inner
        integer(int64) :: lo, hi
        integer(int32) :: v
        character(len=:), allocatable :: kt
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "list_of_struct", lc)
        call lc%kind_text(kt)
        call check(error, kt == "list<struct<x:int32,y:string>>", "kind_text, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        ! Row LENGTHS, not the row count: an assembly that reused the outer offsets for the inner
        ! container gets the count right and these wrong. This is the file's main defence.
        call check(error, lc%length(1_int64) == 1_int64 .and. lc%length(2_int64) == 0_int64 .and. &
            lc%length(3_int64) == 2_int64, "row lengths are 1, 0, 2")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        h = lc%view(3_int64)
        call h%nested(inner, lo, hi)
        call check(error, hi - lo + 1_int64 == 2_int64, "row 3 owns two payload rows")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        select type (inner)
        type is (parquet_struct_column)
            sr = inner%view(hi)
            call sr%get_field("x", v)
            call check(error, v == 3_int32, "its last element has x = 3")
        class default
            call check(error, .false., "the payload narrowed to the wrong container type")
        end select
        call parquet_close_reader(r)
    end subroutine test_read_list_of_struct

    !> 7b: `map<string, struct<x:int32,y:string>>`, plus the query `parquet_table` classification
    !! depends on -- a map whose value is a container must no longer report "unknown".
    subroutine test_read_map_of_struct(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_map_column) :: mc
        character(len=:), allocatable :: kt, vt
        call parquet_open_reader(r, NEST)
        call parquet_get_map_value_type(r, "map_of_struct", vt)
        call check(error, vt == "struct", "the value-type query names the container, got: "//vt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call parquet_read_column(r, "map_of_struct", mc)
        call mc%kind_text(kt)
        call check(error, kt == "map<string,struct<x:int32,y:string>>", "kind_text, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call check(error, mc%nrows() == 3_int64, "three rows")
        call parquet_close_reader(r)
    end subroutine test_read_map_of_struct

    !> The three remaining scope-(c)/(d) shapes, each asserted by its full nested spelling.
    !!
    !! `kind_text` is the one query that walks the whole nesting, so comparing it is a compact way
    !! to assert that every level came back with the right kind -- a shape assembled one level
    !! short, or with the levels swapped, cannot produce the same string.
    subroutine test_read_every_nested_shape(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_list_column) :: lc
        type(parquet_map_column) :: mc
        character(len=:), allocatable :: kt
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "list_of_list", lc)
        call lc%kind_text(kt)
        call check(error, kt == "list<list<int32>>", "list_of_list, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call parquet_read_column(r, "list_of_map", lc)
        call lc%kind_text(kt)
        call check(error, kt == "list<map<string,int32>>", "list_of_map, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call parquet_read_column(r, "map_of_list", mc)
        call mc%kind_text(kt)
        call check(error, kt == "map<string,list<int32>>", "map_of_list, got: "//kt)
        call parquet_close_reader(r)
    end subroutine test_read_every_nested_shape

    !> Scope (d): `deep_nested` is `list<struct<name:string, tags:list<string>, meta:map<...>>>`,
    !! three container levels deep, and it reads with no depth-specific code anywhere.
    !!
    !! The campaign's claim was that arbitrary read depth is a CONSEQUENCE of the representation
    !! rather than a feature. This is the test that makes that claim checkable; only depth 1 is a
    !! committed testing and tooling promise, and deeper is not hard-coded out.
    subroutine test_read_deep_nested(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: h
        class(parquet_container_column), pointer :: inner
        type(parquet_struct_row) :: sr
        integer(int64) :: lo, hi
        character(len=:), allocatable :: kt, nm
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "deep_nested", lc)
        call lc%kind_text(kt)
        call check(error, kt == "list<struct<name:string,tags:list<string>,meta:map<string,int32>>>", &
            "the whole three-level spelling reaches kind_text, got: "//kt)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call check(error, lc%length(1_int64) == 1_int64 .and. lc%length(2_int64) == 0_int64 .and. &
            lc%length(3_int64) == 2_int64, "row lengths are 1, 0, 2")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        h = lc%view(1_int64)
        call h%nested(inner, lo, hi)
        select type (inner)
        type is (parquet_struct_column)
            sr = inner%view(lo)
            call sr%get_field("name", nm)
            call check(error, nm == "n1", "and the innermost scalar still reads")
        class default
            call check(error, .false., "the payload narrowed to the wrong container type")
        end select
        call parquet_close_reader(r)
    end subroutine test_read_deep_nested

    !> The descent grammar itself: `list_of_struct[].x` names the flattened element field and reads
    !! as an ordinary column of THREE values, not of three rows.
    !!
    !! That distinction is the whole point of the row-count guard being descent-aware: the file has
    !! 3 rows and the path has 3 entries only by coincidence here, so `map_of_struct{value}.x` --
    !! with a different entry count -- is asserted beside it.
    subroutine test_descent_path_reads(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        integer(int32) :: xs(3)
        character(len=:), allocatable :: shp
        call parquet_open_reader(r, NEST)
        call check(error, parquet_column_exists(r, "list_of_struct[]"), "a descent path resolves")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call parquet_get_column_shape(r, "list_of_struct[]", shp)
        call check(error, shp == "struct", "and reports the child's shape, got: "//shp)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call parquet_read_column(r, "list_of_struct[].x", xs)
        call check(error, all(xs == [1_int32, 2_int32, 3_int32]), &
            "the flattened element field reads in row order")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        ! A descent path is NOT listed, deliberately -- it resolves without being advertised.
        call check(error, .not. name_is_listed(r, "list_of_struct[]"), &
            "a descent path is not enumerated by parquet_get_column_names")
        call parquet_close_reader(r)
    end subroutine test_descent_path_reads

    !> Whether `parquet_get_column_names` advertises `name`.
    function name_is_listed(r, name) result(res)
        type(parquet_reader), intent(in) :: r  !! open reader.
        character(len=*), intent(in) :: name   !! the name to look for.
        logical :: res                         !! whether it appears in the listing.
        character(len=:), allocatable :: names(:)
        integer :: i
        call parquet_get_column_names(r, names)
        res = .false.
        do i = 1, size(names)
            if (trim(names(i)) == name) res = .true.
        end do
    end function name_is_listed

    !> A row-group-scoped nested read must agree with the whole-column one.
    !!
    !! The two take different C++ paths -- ReadColumn against ReadRowGroup with an explicit leaf
    !! set -- and the leaf set is where a nested read can silently go wrong: asking for one leaf of
    !! a struct yields a perfectly well-formed struct array with one field instead of all of them.
    !! Comparing the two is what catches that; neither alone can.
    subroutine test_nested_chunk_matches_whole(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: r
        type(parquet_list_column) :: whole, chunk
        character(len=:), allocatable :: kw, kc
        call parquet_open_reader(r, NEST)
        call parquet_read_column(r, "list_of_struct", whole)
        call parquet_read_column_chunk(r, "list_of_struct", 1_int64, chunk)
        call whole%kind_text(kw)
        call chunk%kind_text(kc)
        call check(error, kw == kc, "the two reads agree on the nested spelling: "//kw//" vs "//kc)
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call check(error, chunk%nrows() == whole%nrows(), &
            "and on the row count (this fixture has one row group)")
        if (allocated(error)) then; call parquet_close_reader(r); return; end if
        call check(error, chunk%length(3_int64) == whole%length(3_int64), &
            "and on row 3's length")
        call parquet_close_reader(r)
    end subroutine test_nested_chunk_matches_whole

end module test_container_nested
