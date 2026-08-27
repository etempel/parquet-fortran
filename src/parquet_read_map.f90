!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> `MAP` read specifics (whole-column and row-group-scoped): bodies of the module
!> procedures declared in parquet_core.f90's interface block, plus the private
!> per-value-kind worker this file exists to hold.
!>
!> Structurally this is the LIST read with one extra payload. A map is physically
!> a `LIST` of `struct<key, value>`, so the same two-crossing shape applies --
!> parquet_read_map_column_shape reports the counts and the value family, then
!> parquet_read_map_keys_fill brings back the offsets, the per-ROW validity and
!> the keys, and one parquet_read_map_<family>_fill brings back the values and
!> their own validity. Nothing Arrow owns is aliased in either direction.
!>
!> **A key is never null**, so nothing here carries a key validity buffer. Arrow's
!> MapType declares its key field non-nullable and offers no way to change it, so a
!> map has exactly two null levels: the row, and each value. Do not add a third --
!> Arrow would not store it.
submodule (parquet_core:parquet_read) parquet_read_map
    implicit none
contains

    !> Maps a C++-side value family (PF_ELEM_*) onto the parquet_columns kind a value of that
    !> family is stored in.
    !!
    !! The ONE place the two vocabularies meet, and deliberately on this side of the boundary:
    !! PF_ELEM_* is what crosses bind(C) and PK_* is a parquet_columns discriminator the C++ half
    !! must never be given a reason to know about. An unrecognised family aborts rather than
    !! defaulting, because every wrong answer it could produce instead is a silently mistyped
    !! column.
    subroutine map_value_kind(family, name, kind)
        integer(c_int32_t), intent(in) :: family !! the family parquet_read_map_column_shape reported.
        character(len=*), intent(in) :: name     !! column name, for the abort message.
        integer, intent(out) :: kind             !! the PK_* kind to %init the values to.
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
        ! A CONTAINER value, read by descending to `<name>{value}` -- see read_map_impl.
        case (PF_ELEM_LIST);      kind = PK_LIST
        case (PF_ELEM_MAP);       kind = PK_MAP
        case (PF_ELEM_STRUCT);    kind = PK_STRUCT
        case default
            error stop "parquet_read_column: unsupported map value type for column: "//trim(name)
        end select
    end subroutine map_value_kind

    !> The shared body of both map read specifics: `row_group` <= 0 reads the whole column, and
    !> any positive value reads exactly that row group.
    !!
    !! One worker rather than two because the only difference between a whole-column and a
    !! row-group-scoped map read lives on the C++ side, in which array it fetches.
    subroutine read_map_impl(reader, name, row_group, values, context)
        type(parquet_reader), intent(in) :: reader         !! open reader.
        character(len=*), intent(in) :: name               !! map column name.
        integer(int64), intent(in) :: row_group            !! 1-based row group, or <= 0 for the whole column.
        type(parquet_map_column), intent(inout) :: values  !! cleared, then filled.
        character(len=*), intent(in) :: context            !! calling entry point, for error messages.
        integer(c_long_long) :: nrows, nentries, nkeychars, nvalchars, rg
        integer(c_int32_t) :: family, unit_sel
        integer(int64), allocatable :: offsets(:)
        integer(c_int8_t), allocatable :: row_valid(:), value_valid(:)
        logical, allocatable :: row_present(:)
        type(parquet_column) :: keys, vals
        integer :: kind

        rg = int(row_group, kind=c_long_long)
        call parquet_read_map_column_shape(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
            nkeychars, family, unit_sel, nvalchars)
        call map_value_kind(family, name, kind)
        ! Allocate at least one element of everything: a zero-row or zero-entry column is ordinary
        ! (an empty row group, a filter that matched nothing, a column of nothing but null and
        ! empty maps), and a zero-sized array passed on to a bind(C) assumed-size dummy is exactly
        ! the shape nagfor's -C=pointer check exists to catch. The counts, not the allocations,
        ! are what bound every loop below.
        allocate(offsets(nrows + 1_c_long_long))
        allocate(row_valid(max(nrows, 1_c_long_long)))
        allocate(value_valid(max(nentries, 1_c_long_long)))
        call fill_map_keys(reader, name, rg, nrows, nentries, nkeychars, offsets, row_valid, keys)
        if (kind == PK_LIST .or. kind == PK_MAP .or. kind == PK_STRUCT) then
            ! A NESTED value, read as a column in its own right at the DESCENT path
            ! `<name>{value}`. The keys, the offsets and the row validity all came from
            ! fill_map_keys above and are unaffected -- only the VALUES have no typed buffer a
            ! container could be filled into. See feature_container_phase7.md's D4 (7b) and D6.
            call read_nested_payload(reader, trim(name)//"{value}", rg, kind, vals, context)
        else
            call fill_map_values(reader, name, rg, nrows, nentries, nvalchars, unit_sel, kind, &
                value_valid, vals, context)
        end if
        allocate(row_present(nrows))
        row_present = row_valid(1:nrows) /= 0_c_int8_t
        call values%adopt_rows(offsets, keys, vals, row_valid=row_present)
    end subroutine read_map_impl

    !> Brings back the offsets, the per-ROW validity and the KEYS, and builds the flattened keys
    !> column from them.
    !!
    !! Always a `PK_STRING` column, and never carrying any validity: v1 keys are strings and a key
    !! is never null (see the file header), so there is nothing here that a value kind or a null
    !! level could vary.
    subroutine fill_map_keys(reader, name, rg, nrows, nentries, nkeychars, offsets, row_valid, keys)
        type(parquet_reader), intent(in) :: reader       !! open reader.
        character(len=*), intent(in) :: name             !! map column name.
        integer(c_long_long), intent(in) :: rg           !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows        !! rows to read.
        integer(c_long_long), intent(in) :: nentries     !! entries those rows hold.
        integer(c_long_long), intent(in) :: nkeychars    !! total key bytes.
        integer(int64), intent(inout) :: offsets(:)      !! receives nrows+1 offsets.
        integer(c_int8_t), intent(inout) :: row_valid(:) !! receives per-ROW validity.
        type(parquet_column), intent(out) :: keys        !! receives the flattened keys.
        integer(c_int64_t), allocatable, target :: key_offsets(:)
        character(kind=c_char), allocatable, target :: key_data(:)
        type(parquet_string_column) :: store
        integer(int64) :: n

        n = int(nentries, kind=int64)
        allocate(key_offsets(n + 1_int64))
        allocate(key_data(max(int(nkeychars, kind=int64), 1_int64)))
        call parquet_read_map_keys_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
            nkeychars, offsets, row_valid, key_offsets, key_data)
        ! The empty column is %init'd first and only then replaced, exactly as the list payload is:
        ! that is what makes a zero-entry map come out as a correctly typed empty keys column
        ! rather than adopting the one-element scratch buffer above.
        call keys%init(PK_STRING, 0_int64)
        if (n > 0_int64) then
            call store%append_buffers(n, int(nkeychars, kind=int64), c_loc(key_offsets), &
                c_loc(key_data), c_null_ptr, .false.)
            call keys%adopt_string_column(store)
        end if
    end subroutine fill_map_keys

    !> Allocates the typed value buffer for `kind`, calls the matching fill entry point, and builds
    !> the flattened values column from what came back.
    !!
    !! Split out purely so that the nine-way dispatch is one procedure with nothing else in it:
    !! everything before and after it is identical for every value kind, and interleaving them
    !! would make the shared part nine times as easy to get subtly wrong in one arm.
    subroutine fill_map_values(reader, name, rg, nrows, nentries, nvalchars, unit_sel, kind, &
            value_valid, vals, context)
        type(parquet_reader), intent(in) :: reader         !! open reader.
        character(len=*), intent(in) :: name               !! map column name.
        integer(c_long_long), intent(in) :: rg             !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows          !! rows to read.
        integer(c_long_long), intent(in) :: nentries       !! entries those rows hold.
        integer(c_long_long), intent(in) :: nvalchars      !! value bytes (string family only).
        integer(c_int32_t), intent(in) :: unit_sel         !! temporal unit selector (timestamp only).
        integer, intent(in) :: kind                        !! the PK_* kind the file's value family maps to.
        integer(c_int8_t), intent(inout) :: value_valid(:) !! receives per-VALUE validity.
        type(parquet_column), intent(out) :: vals          !! receives the flattened values.
        character(len=*), intent(in) :: context            !! calling entry point, for error messages.
        integer(int32), allocatable :: v_i32(:)
        integer(int64), allocatable :: v_i64(:)
        real(real32), allocatable :: v_f32(:)
        real(real64), allocatable :: v_f64(:)
        integer(c_int8_t), allocatable :: v_bool(:)
        logical, allocatable :: l_bool(:)
        type(parquet_date), allocatable :: v_date(:)
        type(parquet_time), allocatable :: v_time(:)
        type(parquet_timestamp), allocatable :: v_ts(:)
        integer(c_int64_t), allocatable, target :: val_offsets(:)
        character(kind=c_char), allocatable, target :: val_data(:)
        type(parquet_string_column) :: store
        integer(int64) :: k, n
        logical :: any_value_null

        n = int(nentries, kind=int64)
        call vals%init(kind, 0_int64)
        select case (kind)
        case (PK_INT32)
            allocate(v_i32(max(n, 1_int64)))
            call parquet_read_map_int32_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                v_i32, value_valid)
            if (n > 0_int64) call vals%adopt(v_i32)
        case (PK_INT64)
            allocate(v_i64(max(n, 1_int64)))
            call parquet_read_map_int64_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                v_i64, value_valid)
            if (n > 0_int64) call vals%adopt(v_i64)
        case (PK_FLOAT32)
            allocate(v_f32(max(n, 1_int64)))
            call parquet_read_map_float32_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                v_f32, value_valid)
            if (n > 0_int64) call vals%adopt(v_f32)
        case (PK_FLOAT64)
            allocate(v_f64(max(n, 1_int64)))
            call parquet_read_map_float64_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                v_f64, value_valid)
            if (n > 0_int64) call vals%adopt(v_f64)
        case (PK_LOGICAL)
            allocate(v_bool(max(n, 1_int64)))
            call parquet_read_map_bool8_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                v_bool, value_valid)
            allocate(l_bool(max(n, 1_int64)))
            l_bool = v_bool /= 0_c_int8_t
            if (n > 0_int64) call vals%adopt(l_bool)
        case (PK_DATE)
            allocate(v_date(max(n, 1_int64)))
            call read_map_date_values(reader, name, rg, nrows, nentries, value_valid, v_date)
            if (n > 0_int64) call vals%adopt(v_date)
        case (PK_TIME)
            allocate(v_time(max(n, 1_int64)))
            call read_map_time_values(reader, name, rg, nrows, nentries, value_valid, v_time)
            if (n > 0_int64) call vals%adopt(v_time)
        case (PK_TIMESTAMP)
            allocate(v_ts(max(n, 1_int64)))
            call read_map_timestamp_values(reader, name, rg, nrows, nentries, value_valid, unit_sel, v_ts)
            if (n > 0_int64) call vals%adopt(v_ts)
        case (PK_STRING)
            allocate(val_offsets(n + 1_int64))
            allocate(val_data(max(int(nvalchars, kind=int64), 1_int64)))
            call parquet_read_map_string_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
                nvalchars, val_offsets, val_data, value_valid)
            if (n > 0_int64) then
                call store%append_buffers(n, int(nvalchars, kind=int64), c_loc(val_offsets), &
                    c_loc(val_data), c_null_ptr, .false.)
                call vals%adopt_string_column(store)
            end if
        case default
            ! Not reachable: map_value_kind has already refused every family this does not cover,
            ! so `kind` is always one of the nine above. Kept as a second line of defence.
            error stop trim(context)//": unsupported map value kind for column: "//trim(name) ! GCOVR_EXCL_LINE
        end select
        ! Per-VALUE nullness, applied once for every kind. The temporal kinds are exempt: their null
        ! state lives INSIDE the element (a default-initialized parquet_date IS null), so the three
        ! read_map_*_values helpers above have already applied it, and writing it into the values
        ! column's bitmap as well would be a second, independent copy of the same fact.
        if (kind == PK_DATE .or. kind == PK_TIME .or. kind == PK_TIMESTAMP) return
        any_value_null = .false.
        do k = 1_int64, n
            if (value_valid(k) == 0_c_int8_t) then
                any_value_null = .true.
                exit
            end if
        end do
        if (.not. any_value_null) return
        call vals%ensure_validity()
        do k = 1_int64, n
            if (value_valid(k) == 0_c_int8_t) call parquet_column_set_null(vals, k)
        end do
    end subroutine fill_map_values

    !> Reads a date-valued map column's values as `parquet_date`, a Null value becoming a null
    !> (default-initialized) element rather than a bitmap bit -- see
    !> doc/pages/types/date-time.md's "Null values are part of the element".
    subroutine read_map_date_values(reader, name, rg, nrows, nentries, value_valid, values)
        type(parquet_reader), intent(in) :: reader         !! open reader.
        character(len=*), intent(in) :: name               !! map column name.
        integer(c_long_long), intent(in) :: rg             !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows          !! rows to read.
        integer(c_long_long), intent(in) :: nentries       !! entries those rows hold.
        integer(c_int8_t), intent(inout) :: value_valid(:) !! receives per-VALUE validity.
        type(parquet_date), intent(inout) :: values(:)     !! receives the values.
        integer(c_int32_t), allocatable :: days(:)
        integer(int64) :: k, n
        n = int(nentries, kind=int64)
        allocate(days(max(n, 1_int64)))
        call parquet_read_map_date_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
            days, value_valid)
        do k = 1_int64, n
            if (value_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_raw(days(k))
        end do
    end subroutine read_map_date_values

    !> Reads a time-valued map column's values as `parquet_time`; see read_map_date_values for the
    !> null convention.
    subroutine read_map_time_values(reader, name, rg, nrows, nentries, value_valid, values)
        type(parquet_reader), intent(in) :: reader         !! open reader.
        character(len=*), intent(in) :: name               !! map column name.
        integer(c_long_long), intent(in) :: rg             !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows          !! rows to read.
        integer(c_long_long), intent(in) :: nentries       !! entries those rows hold.
        integer(c_int8_t), intent(inout) :: value_valid(:) !! receives per-VALUE validity.
        type(parquet_time), intent(inout) :: values(:)     !! receives the values.
        integer(c_int64_t), allocatable :: ns(:)
        integer(int64) :: k, n
        n = int(nentries, kind=int64)
        allocate(ns(max(n, 1_int64)))
        call parquet_read_map_time_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
            ns, value_valid)
        do k = 1_int64, n
            if (value_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_raw(ns(k))
        end do
    end subroutine read_map_time_values

    !> Reads a timestamp-valued map column's values as `parquet_timestamp` in the column's own
    !> stored unit; see read_map_date_values for the null convention.
    subroutine read_map_timestamp_values(reader, name, rg, nrows, nentries, value_valid, unit_sel, values)
        type(parquet_reader), intent(in) :: reader          !! open reader.
        character(len=*), intent(in) :: name                !! map column name.
        integer(c_long_long), intent(in) :: rg              !! 1-based row group, or <= 0 for the whole column.
        integer(c_long_long), intent(in) :: nrows           !! rows to read.
        integer(c_long_long), intent(in) :: nentries        !! entries those rows hold.
        integer(c_int8_t), intent(inout) :: value_valid(:)  !! receives per-VALUE validity.
        integer(c_int32_t), intent(in) :: unit_sel          !! the column's own stored unit selector.
        type(parquet_timestamp), intent(inout) :: values(:) !! receives the values.
        integer(c_int64_t), allocatable :: raw(:)
        integer(int64) :: k, n
        n = int(nentries, kind=int64)
        allocate(raw(max(n, 1_int64)))
        call parquet_read_map_timestamp_fill(reader%handle, trim(name)//char(0), rg, nrows, nentries, &
            raw, value_valid)
        do k = 1_int64, n
            if (value_valid(k) == 0_c_int8_t) cycle
            call values(k)%set_unix(raw(k), int(unit_sel))
        end do
    end subroutine read_map_timestamp_values

    module procedure parquet_read_map_column
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call read_map_impl(reader, name, 0_int64, values, "parquet_read_column")
    end procedure parquet_read_map_column

    !> Shared body of parquet_read_map_column_chunk_rg32/_rg64 -- see the generic interface's own
    !> doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine read_map_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader        !! open reader.
        character(len=*), intent(in) :: name              !! map column name.
        integer(int64), intent(in) :: row_group           !! 1-based row group.
        type(parquet_map_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call read_map_impl(reader, name, row_group, values, "parquet_read_column_chunk")
    end subroutine read_map_chunk_impl

    module procedure parquet_read_map_column_chunk_rg32
        call read_map_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_map_column_chunk_rg32

    module procedure parquet_read_map_column_chunk_rg64
        call read_map_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_map_column_chunk_rg64

end submodule parquet_read_map
