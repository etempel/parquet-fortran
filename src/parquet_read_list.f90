!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Variable-length `LIST` read specifics (whole-column and row-group-scoped):
!> bodies of the module procedures declared in parquet_core.f90's interface
!> block, plus the private per-payload-kind worker this file exists to hold.
!>
!> This is the one read path whose result is not a flat rectangle, so it is the
!> one place a column's own shape -- how many elements each row holds -- has to
!> cross the boundary rather than being known from the caller's array. It does
!> that in TWO calls: parquet_read_list_column_shape reports the counts and the
!> payload family, and one parquet_read_list_<family>_fill then copies into the
!> buffers this file has allocated from them. Nothing Arrow owns is aliased in
!> either direction -- see the "Variable-length LIST column reads" banner in
!> src/parquet_wrapper.cpp for why that differs from the string path, which does.
submodule (parquet_core:parquet_read) parquet_read_list
    implicit none
contains

    !> Maps a C++-side element family (PF_ELEM_*) onto the parquet_columns kind a payload of that
    !> family is stored in.
    !!
    !! The ONE place the two vocabularies meet, and deliberately on this side of the boundary:
    !! PF_ELEM_* is what crosses bind(C) and PK_* is a parquet_columns discriminator the C++ half
    !! must never be given a reason to know about. An unrecognised family aborts rather than
    !! defaulting, because every wrong answer it could produce instead is a silently mistyped
    !! column.
    subroutine list_payload_kind(family, name, kind)
        integer(c_int32_t), intent(in) :: family !! the family parquet_read_list_column_shape reported.
        character(len=*), intent(in) :: name     !! column name, for the abort message.
        integer, intent(out) :: kind             !! the PK_* kind to %init the payload to.
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
        case default
            error stop "parquet_read_column: unsupported list element type for column: "//trim(name)
        end select
    end subroutine list_payload_kind

    !> The shared body of both list read specifics: `row_group` <= 0 reads the whole column, and
    !> any positive value reads exactly that row group.
    !!
    !! One worker rather than two because the only difference between a whole-column and a
    !! row-group-scoped list read lives on the C++ side, in which array it fetches. Everything
    !! this file does -- ask for the shape, allocate, fill, assemble -- is identical, and a second
    !! copy of it would be a second place for the two null levels to be got wrong.
    subroutine read_list_impl(reader, name, row_group, values, context)
        type(parquet_reader), intent(in) :: reader          !! open reader.
        character(len=*), intent(in) :: name                !! list column name.
        integer(int64), intent(in) :: row_group             !! 1-based row group, or <= 0 for the whole column.
        type(parquet_list_column), intent(inout) :: values  !! cleared, then filled.
        character(len=*), intent(in) :: context             !! calling entry point, for error messages.
        integer(c_long_long) :: nrows, nelems, nchars, rg
        integer(c_int32_t) :: family, unit_sel
        integer(int64), allocatable :: offsets(:)
        integer(c_int8_t), allocatable :: row_valid(:), elem_valid(:)
        logical, allocatable :: row_present(:)
        type(parquet_column) :: payload
        integer :: kind

        rg = int(row_group, kind=c_long_long)
        call parquet_read_list_column_shape(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
            nchars, family, unit_sel)
        call list_payload_kind(family, name, kind)
        ! Allocate at least one element of everything: a zero-row or zero-element column is
        ! ordinary (an empty row group, a filter that matched nothing), and a zero-sized array
        ! passed on to a bind(C) assumed-size dummy is exactly the shape nagfor's -C=pointer check
        ! exists to catch. The counts, not the allocations, are what bound every loop below.
        allocate(offsets(nrows + 1_c_long_long))
        allocate(row_valid(max(nrows, 1_c_long_long)))
        allocate(elem_valid(max(nelems, 1_c_long_long)))
        call fill_list_payload(reader, name, rg, nrows, nelems, nchars, unit_sel, kind, &
            offsets, row_valid, elem_valid, payload, context)
        allocate(row_present(nrows))
        row_present = row_valid(1:nrows) /= 0_c_int8_t
        call values%adopt_rows(offsets, payload, row_valid=row_present)
    end subroutine read_list_impl

    !> Allocates the typed value buffer for `kind`, calls the matching fill entry point, and
    !> builds the flattened payload column from what came back.
    !!
    !! Split out from read_list_impl purely so that the nine-way dispatch is one procedure with
    !! nothing else in it: everything before and after it is identical for every payload kind, and
    !! interleaving them would make the shared part nine times as easy to get subtly wrong in one
    !! arm. Per-ELEMENT nullness is applied here, onto the payload, where a parquet_column already
    !! carries it -- the ROW level is the caller's business and stays separate.
    subroutine fill_list_payload(reader, name, rg, nrows, nelems, nchars, unit_sel, kind, &
            offsets, row_valid, elem_valid, payload, context)
        type(parquet_reader), intent(in) :: reader       !! open reader.
        character(len=*), intent(in) :: name             !! list column name.
        integer(c_long_long), intent(in) :: rg           !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows        !! rows to read.
        integer(c_long_long), intent(in) :: nelems       !! elements those rows hold.
        integer(c_long_long), intent(in) :: nchars       !! payload bytes (string family only).
        integer(c_int32_t), intent(in) :: unit_sel       !! temporal unit selector (timestamp only).
        integer, intent(in) :: kind                      !! the PK_* kind the file's element family maps to.
        integer(int64), intent(inout) :: offsets(:)      !! receives nrows+1 offsets.
        integer(c_int8_t), intent(inout) :: row_valid(:) !! receives per-ROW validity.
        integer(c_int8_t), intent(inout) :: elem_valid(:) !! receives per-ELEMENT validity.
        type(parquet_column), intent(out) :: payload     !! receives the flattened elements.
        character(len=*), intent(in) :: context          !! calling entry point, for error messages.
        integer(int32), allocatable :: v_i32(:)
        integer(int64), allocatable :: v_i64(:)
        real(real32), allocatable :: v_f32(:)
        real(real64), allocatable :: v_f64(:)
        integer(c_int8_t), allocatable :: v_bool(:)
        logical, allocatable :: l_bool(:)
        type(parquet_date), allocatable :: v_date(:)
        type(parquet_time), allocatable :: v_time(:)
        type(parquet_timestamp), allocatable :: v_ts(:)
        integer(c_int64_t), allocatable, target :: str_offsets(:)
        character(kind=c_char), allocatable, target :: str_data(:)
        type(parquet_string_column) :: store
        integer(int64) :: k, n
        logical :: any_elem_null

        n = int(nelems, kind=int64)
        ! Every payload starts as an empty column of the right kind, and a non-empty one then
        ! REPLACES that by adopting its values array (%adopt takes kind, width and row count from
        ! the array itself, so it supersedes %init rather than following it). The up-front %init
        ! is what makes the zero-element case -- an empty row group, a filter that matched
        ! nothing, a column of nothing but null and empty rows -- come out as a correctly typed
        ! empty payload instead of adopting the one-element scratch buffer below.
        !
        ! That scratch element is why the buffers are max(n, 1) and not n: a zero-sized actual
        ! argument associated with an assumed-size bind(C) dummy is not something to rely on, and
        ! costs one element to avoid.
        call payload%init(kind, 0_int64)
        select case (kind)
        case (PK_INT32)
            allocate(v_i32(max(n, 1_int64)))
            call parquet_read_list_int32_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
                offsets, row_valid, v_i32, elem_valid)
            if (n > 0_int64) call payload%adopt(v_i32)
        case (PK_INT64)
            allocate(v_i64(max(n, 1_int64)))
            call parquet_read_list_int64_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
                offsets, row_valid, v_i64, elem_valid)
            if (n > 0_int64) call payload%adopt(v_i64)
        case (PK_FLOAT32)
            allocate(v_f32(max(n, 1_int64)))
            call parquet_read_list_float32_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
                offsets, row_valid, v_f32, elem_valid)
            if (n > 0_int64) call payload%adopt(v_f32)
        case (PK_FLOAT64)
            allocate(v_f64(max(n, 1_int64)))
            call parquet_read_list_float64_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
                offsets, row_valid, v_f64, elem_valid)
            if (n > 0_int64) call payload%adopt(v_f64)
        case (PK_LOGICAL)
            allocate(v_bool(max(n, 1_int64)))
            call parquet_read_list_bool8_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
                offsets, row_valid, v_bool, elem_valid)
            allocate(l_bool(max(n, 1_int64)))
            l_bool = v_bool /= 0_c_int8_t
            if (n > 0_int64) call payload%adopt(l_bool)
        case (PK_DATE)
            allocate(v_date(max(n, 1_int64)))
            call read_list_date_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, v_date)
            if (n > 0_int64) call payload%adopt(v_date)
        case (PK_TIME)
            allocate(v_time(max(n, 1_int64)))
            call read_list_time_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, v_time)
            if (n > 0_int64) call payload%adopt(v_time)
        case (PK_TIMESTAMP)
            allocate(v_ts(max(n, 1_int64)))
            call read_list_timestamp_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, &
                unit_sel, v_ts)
            if (n > 0_int64) call payload%adopt(v_ts)
        case (PK_STRING)
            allocate(str_offsets(n + 1_int64))
            allocate(str_data(max(int(nchars, kind=int64), 1_int64)))
            call parquet_read_list_string_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, nchars, &
                offsets, row_valid, str_offsets, str_data, elem_valid)
            ! A parquet_string_column is a packed variable-length store with no fixed row slots,
            ! so it is built up and then handed over whole, rather than written into a payload
            ! column that already exists. %adopt_string_column is what settles the payload's row
            ! count from the store: appending into the store reached through
            ! parquet_column_string_column would leave the column's own nrows at 0, and
            ! %adopt_rows' final-offset check is what would report that -- eventually.
            if (n > 0_int64) then
                call store%append_buffers(n, int(nchars, kind=int64), c_loc(str_offsets), &
                    c_loc(str_data), c_null_ptr, .false.)
                call payload%adopt_string_column(store)
            end if
        case default
            ! Not reachable: list_payload_kind has already refused every family this does not
            ! cover, so `kind` is always one of the nine above. Kept as a second line of defence.
            error stop trim(context)//": unsupported list payload kind for column: "//trim(name) ! GCOVR_EXCL_LINE
        end select
        ! Per-ELEMENT nullness, applied once for every kind. The temporal kinds are exempt: their
        ! null state lives INSIDE the element (a default-initialized parquet_date IS null), so the
        ! three read_list_*_values helpers above have already applied it, and writing it into the
        ! payload's bitmap as well would be a second, independent copy of the same fact.
        if (kind == PK_DATE .or. kind == PK_TIME .or. kind == PK_TIMESTAMP) return
        any_elem_null = .false.
        do k = 1_int64, n
            if (elem_valid(k) == 0_c_int8_t) then
                any_elem_null = .true.
                exit
            end if
        end do
        if (.not. any_elem_null) return
        call payload%ensure_validity()
        do k = 1_int64, n
            if (elem_valid(k) == 0_c_int8_t) call parquet_column_set_null(payload, k)
        end do
    end subroutine fill_list_payload

    !> Reads a date-payload list column's elements as `parquet_date` values, a Null element
    !> becoming a null (default-initialized) element rather than a bitmap bit -- see
    !> doc/pages/types/date-time.md's "Null values are part of the element".
    subroutine read_list_date_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, values)
        type(parquet_reader), intent(in) :: reader        !! open reader.
        character(len=*), intent(in) :: name              !! list column name.
        integer(c_long_long), intent(in) :: rg            !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows         !! rows to read.
        integer(c_long_long), intent(in) :: nelems        !! elements those rows hold.
        integer(int64), intent(inout) :: offsets(:)       !! receives nrows+1 offsets.
        integer(c_int8_t), intent(inout) :: row_valid(:)  !! receives per-ROW validity.
        integer(c_int8_t), intent(inout) :: elem_valid(:) !! receives per-ELEMENT validity.
        type(parquet_date), intent(inout) :: values(:)    !! receives the elements.
        integer(c_int32_t), allocatable :: days(:)
        integer(int64) :: k, n
        n = int(nelems, kind=int64)
        allocate(days(max(n, 1_int64)))
        call parquet_read_list_date_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
            offsets, row_valid, days, elem_valid)
        do k = 1_int64, n
            if (elem_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_raw(days(k))
        end do
    end subroutine read_list_date_values

    !> Reads a time-payload list column's elements as `parquet_time` values; see
    !> read_list_date_values for the null convention.
    subroutine read_list_time_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, values)
        type(parquet_reader), intent(in) :: reader        !! open reader.
        character(len=*), intent(in) :: name              !! list column name.
        integer(c_long_long), intent(in) :: rg            !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows         !! rows to read.
        integer(c_long_long), intent(in) :: nelems        !! elements those rows hold.
        integer(int64), intent(inout) :: offsets(:)       !! receives nrows+1 offsets.
        integer(c_int8_t), intent(inout) :: row_valid(:)  !! receives per-ROW validity.
        integer(c_int8_t), intent(inout) :: elem_valid(:) !! receives per-ELEMENT validity.
        type(parquet_time), intent(inout) :: values(:)    !! receives the elements.
        integer(c_int64_t), allocatable :: ns(:)
        integer(int64) :: k, n
        n = int(nelems, kind=int64)
        allocate(ns(max(n, 1_int64)))
        call parquet_read_list_time_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
            offsets, row_valid, ns, elem_valid)
        do k = 1_int64, n
            if (elem_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_raw(ns(k))
        end do
    end subroutine read_list_time_values

    !> Reads a timestamp-payload list column's elements as `parquet_timestamp` values in the
    !> column's own stored unit; see read_list_date_values for the null convention.
    subroutine read_list_timestamp_values(reader, name, rg, nrows, nelems, offsets, row_valid, elem_valid, &
            unit_sel, values)
        type(parquet_reader), intent(in) :: reader          !! open reader.
        character(len=*), intent(in) :: name                !! list column name.
        integer(c_long_long), intent(in) :: rg              !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows           !! rows to read.
        integer(c_long_long), intent(in) :: nelems          !! elements those rows hold.
        integer(int64), intent(inout) :: offsets(:)         !! receives nrows+1 offsets.
        integer(c_int8_t), intent(inout) :: row_valid(:)    !! receives per-ROW validity.
        integer(c_int8_t), intent(inout) :: elem_valid(:)   !! receives per-ELEMENT validity.
        integer(c_int32_t), intent(in) :: unit_sel          !! the column's own stored unit selector.
        type(parquet_timestamp), intent(inout) :: values(:) !! receives the elements.
        integer(c_int64_t), allocatable :: raw(:)
        integer(int64) :: k, n
        n = int(nelems, kind=int64)
        allocate(raw(max(n, 1_int64)))
        call parquet_read_list_timestamp_fill(reader%handle, trim(name)//char(0), rg, nrows, nelems, &
            offsets, row_valid, raw, elem_valid)
        do k = 1_int64, n
            if (elem_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_unix(raw(k), int(unit_sel))
        end do
    end subroutine read_list_timestamp_values

    module procedure parquet_read_list_column
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call read_list_impl(reader, name, 0_int64, values, "parquet_read_column")
    end procedure parquet_read_list_column

    !> Shared body of parquet_read_list_column_chunk_rg32/_rg64 -- see the generic interface's own
    !> doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine read_list_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader         !! open reader.
        character(len=*), intent(in) :: name               !! list column name.
        integer(int64), intent(in) :: row_group            !! 1-based row group.
        type(parquet_list_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call read_list_impl(reader, name, row_group, values, "parquet_read_column_chunk")
    end subroutine read_list_chunk_impl

    module procedure parquet_read_list_column_chunk_rg32
        call read_list_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_list_column_chunk_rg32

    module procedure parquet_read_list_column_chunk_rg64
        call read_list_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_list_column_chunk_rg64

end submodule parquet_read_list
