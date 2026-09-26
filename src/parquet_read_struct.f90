!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> `STRUCT` read specifics (whole-column and row-group-scoped): bodies of the
!> module procedures declared in parquet_core.f90's interface block, plus the
!> private per-field-kind worker this file exists to hold.
!>
!> **This file adds no per-kind C++ entry point, and that is its whole design.**
!> Every field of a struct is already an ordinary column at an ordinary DOTTED
!> PATH -- `person.age` -- which resolve_struct_path resolves and every existing
!> per-kind reader has read since long before container columns existed, whole
!> column and per row group alike. So a struct read is: ask the C++ side for the
!> field set, read each field through the reader that already exists, ask for the
!> struct's own row validity, and hand the whole set over in one move. Three new
!> entry points, where the list read needed ten.
!>
!> **A field's own nullness IS the mask the ordinary read returns, with no
!> arithmetic** -- which is the opposite of what the obvious derivation suggests,
!> so it is stated here and in src/parquet_wrapper.cpp's own section banner to
!> stop someone adding the derivation back. unwrap_struct_path combines the
!> struct's validity with the field's, and for a PRESENT row that combination is
!> the identity; for a NULL struct row Parquet has already forced every child
!> null on write, because its definition levels cannot encode "the struct is
!> absent but its field is present". Measured against Arrow 25.0.0.
!>
!> The struct's OWN row validity is the one thing that is NOT derivable: a row
!> where every field is null and a row where the struct instance is absent give
!> the same combined mask for every field, and they are different rows. Hence
!> parquet_read_struct_row_validity.
submodule (parquet_core:parquet_read) parquet_read_struct
    implicit none
contains

    !> Maps a C++-side element family (PF_ELEM_*) onto the parquet_columns kind a field of that
    !> family is stored in, naming the FIELD when it cannot.
    !!
    !! The ONE place the two vocabularies meet on the struct path, and deliberately on this side
    !! of the boundary, for the same reason list_payload_kind gives: PF_ELEM_* is what crosses
    !! bind(C) and PK_* is a parquet_columns discriminator the C++ half must never be given a
    !! reason to know about.
    !!
    !! An unreadable field type -- including a nested `struct`, `list` or `map`, which is Phase
    !! 7's business -- is refused here rather than further down, and the message names the field
    !! and not just the column. That precision is the whole obligation this phase has towards the
    !! nesting it does not implement.
    subroutine struct_field_kind(family, colname, fieldname, kind)
        integer(c_int32_t), intent(in) :: family !! the family parquet_read_struct_column_fields reported.
        character(len=*), intent(in) :: colname  !! struct column name, for the abort message.
        character(len=*), intent(in) :: fieldname !! field name, for the abort message.
        integer, intent(out) :: kind             !! the PK_* kind to store the field in.
        select case (family)
        case (PF_ELEM_INT32);     kind = PK_INT32
        case (PF_ELEM_INT64);     kind = PK_INT64
        case (PF_ELEM_FLOAT32);   kind = PK_FLOAT32
        case (PF_ELEM_FLOAT64);   kind = PK_FLOAT64
        case (PF_ELEM_BOOL);      kind = PK_LOGICAL
        case (PF_ELEM_STRING);    kind = PK_STRING
        case (PF_ELEM_DATE);      kind = PK_DATE
        case (PF_ELEM_TIME);      kind = PK_TIME
        case (PF_ELEM_TIMESTAMP); kind = PK_TIMESTAMP
        ! A LIST or MAP field, read recursively through its own dotted path -- see
        ! read_struct_container_field. A nested STRUCT field is deliberately NOT here: it is not one
        ! of the four shapes scope (c) covers, and reading it would need an intermediate struct to
        ! be addressable by a dotted path, which this library refuses on purpose (a path resolves to
        ! a LEAF -- see doc/pages/types/supported-data-types.md, and the intermediate-struct error
        ! scenario that pins it). Its own leaves already read at ANY depth through their dotted
        ! paths, which is what the message below points at.
        case (PF_ELEM_LIST);      kind = PK_LIST
        case (PF_ELEM_MAP);       kind = PK_MAP
        case (PF_ELEM_STRUCT)
            error stop "parquet_read_column: field '"//trim(fieldname)//"' of struct column '"// &
                trim(colname)//"' is itself a struct; read its leaves by their own dotted paths "// &
                "(e.g. '"//trim(colname)//"."//trim(fieldname)//".<field>'), which works at any depth"
        case default
            error stop "parquet_read_column: field '"//trim(fieldname)//"' of struct column '"// &
                trim(colname)//"' has a type this library cannot read into a struct column"
        end select
    end subroutine struct_field_kind

    !> The shared body of both struct read specifics: `row_group` <= 0 reads the whole column,
    !> and any positive value reads exactly that row group.
    !!
    !! One worker rather than two because the only difference between a whole-column and a
    !! row-group-scoped struct read lives in which read entry point each field goes through, and
    !! a second copy of the field-set handling would be a second place to get the two null levels
    !! wrong.
    subroutine read_struct_impl(reader, name, row_group, values, context)
        type(parquet_reader), intent(in) :: reader           !! open reader.
        character(len=*), intent(in) :: name                 !! struct column name.
        integer(int64), intent(in) :: row_group              !! 1-based row group, or <= 0 for the whole column.
        type(parquet_struct_column), intent(inout) :: values !! cleared, then filled.
        character(len=*), intent(in) :: context              !! calling entry point, for error messages.
        integer(c_long_long) :: nrows, rg
        integer(c_int32_t) :: nfields, name_width
        character(len=:), allocatable :: fnames(:)
        integer(c_int32_t), allocatable :: families(:), units(:)
        integer(c_int8_t), allocatable :: utc(:), row_valid(:)
        logical, allocatable :: row_present(:)
        type(parquet_column), allocatable :: fields(:)
        integer :: j, kind

        rg = int(row_group, kind=c_long_long)
        call parquet_read_struct_column_shape(reader%handle, trim(name)//char(0), rg, nrows, &
            nfields, name_width)
        ! Allocate at least one of everything: a zero-row column is ordinary (an empty row group,
        ! a filter that matched nothing), and a zero-sized actual associated with an assumed-size
        ! bind(C) dummy is exactly the shape nagfor's -C=pointer check exists to catch. The
        ! counts, not the allocations, bound every loop below.
        allocate(character(len=name_width) :: fnames(max(int(nfields), 1)))
        allocate(families(max(int(nfields), 1)))
        allocate(units(max(int(nfields), 1)))
        allocate(utc(max(int(nfields), 1)))
        call parquet_read_struct_column_fields(reader%handle, trim(name)//char(0), nfields, &
            name_width, fnames, families, units, utc)
        allocate(fields(max(int(nfields), 1)))
        do j = 1, int(nfields)
            call struct_field_kind(families(j), name, fnames(j), kind)
            call read_struct_field(reader, trim(name)//"."//trim(fnames(j)), rg, nrows, kind, &
                fields(j), context)
        end do
        allocate(row_valid(max(nrows, 1_c_long_long)))
        call parquet_read_struct_row_validity(reader%handle, trim(name)//char(0), rg, nrows, row_valid)
        allocate(row_present(nrows))
        row_present = row_valid(1:nrows) /= 0_c_int8_t
        ! A struct column always has at least one field -- Arrow cannot build a zero-field struct
        ! -- so the trimmed slices below are never empty in practice; the guard is what keeps a
        ! malformed file from reaching %adopt_fields' own refusal with a confusing shape.
        if (nfields < 1_c_int32_t) then
            ! The `if` stays OUTSIDE the exclusion: its condition is evaluated on every read, so
            ! excluding it would report as a stale exclusion rather than as dead code.
            error stop trim(context)//": struct column '"//trim(name)//"' declares no fields" ! GCOVR_EXCL_LINE
        end if
        call parquet_struct_column_build(values, fnames(1:int(nfields)), fields, row_valid=row_present)
    end subroutine read_struct_impl

    !> Reads ONE field of a struct column -- an ordinary column at a dotted path -- into a
    !> `parquet_column` of the right kind.
    !!
    !! Split out from read_struct_impl purely so that the nine-way dispatch is one procedure with
    !! nothing else in it: everything before and after it is identical for every field kind, and
    !! interleaving them would make the shared part nine times as easy to get subtly wrong in one
    !! arm.
    !!
    !! **The mask each read returns is stored as the field's own validity, unchanged** -- see this
    !! file's header for why no combination has to be undone. The three temporal kinds take no
    !! mask at all, because their null state lives inside the element.
    subroutine read_struct_field(reader, path, rg, nrows, kind, field, context)
        type(parquet_reader), intent(in) :: reader     !! open reader.
        character(len=*), intent(in) :: path           !! the field's dotted column path.
        integer(c_long_long), intent(in) :: rg         !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows      !! rows to read.
        integer, intent(in) :: kind                    !! the PK_* kind the field's family maps to.
        type(parquet_column), intent(out) :: field     !! receives the field's values and nullness.
        character(len=*), intent(in) :: context        !! calling entry point, for error messages.
        integer(int32), allocatable :: v_i32(:)
        integer(int64), allocatable :: v_i64(:)
        real(real32), allocatable :: v_f32(:)
        real(real64), allocatable :: v_f64(:)
        logical, allocatable :: v_bool(:)
        type(parquet_date), allocatable :: v_date(:)
        type(parquet_time), allocatable :: v_time(:)
        type(parquet_timestamp), allocatable :: v_ts(:)
        type(parquet_string_column) :: store
        logical, allocatable :: ok(:)
        integer(int64) :: n, i
        logical :: whole

        n = int(nrows, kind=int64)
        whole = (rg <= 0_c_long_long)
        ! A NESTED field is handled first and returns, BEFORE the %init below -- which refuses a
        ! container kind by design, because %init fixes a kind and a container is a kind plus a
        ! whole inner schema. The field is read through its own dotted path by the reader that
        ! already handles that container type, including a recursive call for PK_STRUCT. That is
        ! the whole of Phase 7's 7a half, and it is this small because the struct read path was
        ! already built on "every field of a struct is an ordinary column at a dotted path": a
        ! container field is one too.
        !
        ! No `ok` mask, for the same reason PK_STRING needs none: a container carries its own
        ! per-ROW validity inside itself, so there is no caller-side mask to apply and writing one
        ! into the field column's bitmap as well would be a second, redundant copy of the same fact
        ! -- and the two could then disagree.
        if (kind == PK_LIST .or. kind == PK_MAP) then
            call read_struct_container_field(reader, path, rg, kind, field, context)
            return
        end if
        ! Every field starts as an empty column of the right kind, and a non-empty one then
        ! REPLACES that by adopting its values array. The up-front %init is what makes the
        ! zero-row case come out as a correctly typed empty field instead of adopting a scratch
        ! buffer.
        call field%init(kind, 0_int64)
        allocate(ok(max(n, 1_int64)))
        ok = .true.
        select case (kind)
        case (PK_INT32)
            allocate(v_i32(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_i32(1:n), is_valid=ok(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_i32(1:n), is_valid=ok(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_i32)
        case (PK_INT64)
            allocate(v_i64(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_i64(1:n), is_valid=ok(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_i64(1:n), is_valid=ok(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_i64)
        case (PK_FLOAT32)
            allocate(v_f32(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_f32(1:n), is_valid=ok(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_f32(1:n), is_valid=ok(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_f32)
        case (PK_FLOAT64)
            allocate(v_f64(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_f64(1:n), is_valid=ok(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_f64(1:n), is_valid=ok(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_f64)
        case (PK_LOGICAL)
            allocate(v_bool(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_bool(1:n), is_valid=ok(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_bool(1:n), is_valid=ok(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_bool)
        case (PK_DATE)
            allocate(v_date(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_date(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_date(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_date)
        case (PK_TIME)
            allocate(v_time(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_time(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_time(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_time)
        case (PK_TIMESTAMP)
            allocate(v_ts(max(n, 1_int64)))
            if (whole) then
                call parquet_read_column(reader, path, v_ts(1:n))
            else
                call parquet_read_column_chunk(reader, path, rg, v_ts(1:n))
            end if
            if (n > 0_int64) call field%adopt(v_ts)
        case (PK_STRING)
            ! A parquet_string_column carries its own per-element nullness, so it needs no mask
            ! and %adopt_string_column settles the field's row count from the store.
            if (whole) then
                call parquet_read_column(reader, path, store)
            else
                call parquet_read_column_chunk(reader, path, rg, store)
            end if
            call field%adopt_string_column(store)
            return
        case default
            ! Not reachable: struct_field_kind has already refused every family this does not
            ! cover, so `kind` is always one of the twelve above. Kept as a second line of defence.
            error stop trim(context)//": unsupported struct field kind for column: "//trim(path) ! GCOVR_EXCL_LINE
        end select
        ! Per-FIELD nullness, applied once for the six kinds that reported a mask. The three
        ! temporal kinds are exempt: their null state lives INSIDE the element (a
        ! default-initialized parquet_date IS null), so it is already in the values above and
        ! writing it into the column's bitmap as well would be a second, redundant copy.
        select case (kind)
        case (PK_DATE, PK_TIME, PK_TIMESTAMP)
            continue
        case default
            do i = 1_int64, n
                if (.not. ok(i)) call parquet_column_set_null(field, i)
            end do
        end select
    end subroutine read_struct_field

    !> Reads one CONTAINER field of a struct column and hands it to `field` as a container column.
    !!
    !! Split out of `read_struct_field` rather than written inline because each arm needs its own
    !! local of a different derived type, and three more locals in a procedure that already declares
    !! ten would make the scalar arms harder to read than the nesting is worth.
    !!
    !! Every arm goes through the ORDINARY public read for that container type at the field's dotted
    !! path, so a nested field costs no new C++ crossing and inherits every guard, every null rule
    !! and every row-group scoping decision those paths already make. `%adopt_container` is then the
    !! single writer of a container kind, exactly as it is everywhere else.
    subroutine read_struct_container_field(reader, path, rg, kind, field, context)
        type(parquet_reader), intent(in) :: reader     !! open reader.
        character(len=*), intent(in) :: path           !! the field's dotted column path.
        integer(c_long_long), intent(in) :: rg         !! 1-based row group, or <= 0 for the whole column.
        integer, intent(in) :: kind                    !! PK_LIST or PK_MAP.
        type(parquet_column), intent(inout) :: field   !! receives the container.
        character(len=*), intent(in) :: context        !! calling entry point, for error messages.
        type(parquet_list_column) :: lc
        type(parquet_map_column) :: mc
        class(parquet_container_column), allocatable :: cc
        logical :: whole
        whole = (rg <= 0_c_long_long)
        select case (kind)
        case (PK_LIST)
            if (whole) then
                call parquet_read_column(reader, path, lc)
            else
                call parquet_read_column_chunk(reader, path, rg, lc)
            end if
            allocate(cc, source=lc)
        case (PK_MAP)
            if (whole) then
                call parquet_read_column(reader, path, mc)
            else
                call parquet_read_column_chunk(reader, path, rg, mc)
            end if
            allocate(cc, source=mc)
        case default
            error stop trim(context)//": unsupported container field kind for column: "//trim(path) ! GCOVR_EXCL_LINE
        end select
        call field%adopt_container(cc)
    end subroutine read_struct_container_field

    module procedure parquet_read_struct_column
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call read_struct_impl(reader, name, 0_int64, values, "parquet_read_column")
    end procedure parquet_read_struct_column

    !> Shared body of parquet_read_struct_column_chunk_rg32/_rg64 -- see the generic interface's
    !> own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine read_struct_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader           !! open reader.
        character(len=*), intent(in) :: name                 !! struct column name.
        integer(int64), intent(in) :: row_group              !! 1-based row group.
        type(parquet_struct_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call read_struct_impl(reader, name, row_group, values, "parquet_read_column_chunk")
    end subroutine read_struct_chunk_impl

    module procedure parquet_read_struct_column_chunk_rg32
        call read_struct_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_struct_column_chunk_rg32

    module procedure parquet_read_struct_column_chunk_rg64
        call read_struct_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_struct_column_chunk_rg64

end submodule parquet_read_struct
