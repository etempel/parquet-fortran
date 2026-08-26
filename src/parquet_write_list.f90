!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Variable-length `LIST` write specifics (whole-column and row-group-scoped):
!> bodies of the two module procedures declared in parquet_core.f90's interface
!> block, plus the private workers this file exists to hold.
!>
!> The mirror of parquet_read_list.f90, and simpler than it in one respect: a
!> write needs ONE crossing per column where a read needs two. A read cannot
!> know the shape until Arrow has been asked; a write already knows the row
!> count, the element count, the payload kind and the temporal unit, so it hands
!> all of them over at once, in buffers this side owns and which stay alive for
!> the duration of the call.
!>
!> Two things are specific to a list column and worth reading before changing
!> anything here. Its nullness has TWO levels -- a null ROW (an absent list) and
!> a null ELEMENT inside a present row -- which are independent and both cross;
!> and the payload's elements are reached through the typed accessors
!> `parquet_list_column_offsets`/`_payload`/`_row_validity` rather than through a
!> row handle per row, because a handle per row is one allocation per row on a
!> path whose whole purpose is to hand over a contiguous buffer.
submodule (parquet_core:parquet_write) parquet_write_list
    implicit none
contains

    !> The MAML `data_type` token a list column of payload kind `kind` is declared with -- the
    !> canonical BASE form, so a temporal payload's unit is not part of it (a `list[timestamp]`
    !> column's unit lives in the same `time_unit`/`is_utc` fields a scalar timestamp column's
    !> does). This is what a schema-enforced write compares against, and the comparison is EXACT:
    !> there is no widening between list element kinds the way parquet_is_type_compatible allows
    !> between scalar numeric kinds.
    subroutine list_type_token(kind, token)
        integer, intent(in) :: kind                        !! the payload's PK_* kind.
        character(len=:), allocatable, intent(out) :: token !! e.g. "list[int32]".
        character(len=:), allocatable :: kname
        select case (kind)
        case (PK_INT32);     token = "list[int32]"
        case (PK_INT64);     token = "list[int64]"
        case (PK_FLOAT32);   token = "list[float32]"
        case (PK_FLOAT64);   token = "list[float64]"
        case (PK_LOGICAL);   token = "list[boolean]"
        case (PK_STRING);    token = "list[string]"
        case (PK_DATE);      token = "list[date]"
        case (PK_TIME);      token = "list[time]"
        case (PK_TIMESTAMP); token = "list[timestamp]"
        case default
            ! Not reachable through the public API: %init refuses any other payload kind, so a
            ! list column cannot exist with one. Kept so the message names what it saw.
            call parquet_kind_name(kind, kname) ! GCOVR_EXCL_LINE
            token = "list["//kname//"]" ! GCOVR_EXCL_LINE
        end select
    end subroutine list_type_token

    !> Shared bookkeeping for both list write specifics: schema checks (defined, enabled, col_size,
    !> exact data_type match), mark-written, and output-name resolution -- the list column's
    !> counterpart to temporal_write_preamble. Returns do_write=.false. (caller returns without
    !> writing) for a defined-but-disabled column; `idx` is the schema column index (0 for a
    !> schema-less writer), for the temporal payload's unit resolution.
    subroutine list_write_preamble(writer, name, kind, context, do_write, outname, idx)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer, intent(in) :: kind !! the payload's PK_* kind.
        character(len=*), intent(in) :: context !! "parquet_write_column" or "..._chunk".
        logical, intent(out) :: do_write !! .true. if the column should actually be written.
        character(len=:), allocatable, intent(out) :: outname !! resolved output (file) name.
        integer, intent(out) :: idx !! schema column index, or 0 for a schema-less writer.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: expected

        do_write = .false.
        idx = 0
        call list_type_token(kind, expected)
        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                context // ": column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, 1_int64, context, ctx)
            if (trim(writer%all_columns(idx)%data_type) /= expected) then
                error stop context // ": type mismatch for column " // trim(name) // &
                    " (expected " // expected // ", got " // trim(writer%all_columns(idx)%data_type) // &
                    "); the column passed holds " // expected // " while the schema declares " // &
                    trim(writer%all_columns(idx)%data_type) // ", and a list column's element type must " // &
                    "match exactly -- there is no widening between list element kinds" // ctx
            end if
        end if
        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_resolve_output_name(writer, name, outname)
        do_write = .true.
    end subroutine list_write_preamble

    !> Builds this column's per-ELEMENT validity mask from the payload.
    !>
    !> Two sources, and the split is the same one parquet_read_list.f90's fill makes in the other
    !> direction: the three temporal kinds carry their null state INSIDE the element (a
    !> default-initialized parquet_date IS null), so their mask comes from the elements; every
    !> other kind keeps it in the payload column's own bitmap.
    subroutine list_element_validity(pay, nelems, kind, is_valid, any_null)
        type(parquet_column), intent(in) :: pay !! the flattened payload column.
        integer(int64), intent(in) :: nelems !! elements it holds.
        integer, intent(in) :: kind !! the payload's PK_* kind.
        logical, allocatable, intent(out) :: is_valid(:) !! .true. where the element is present.
        logical, intent(out) :: any_null !! .true. if at least one element is null.
        type(parquet_date), pointer :: p_date(:)
        type(parquet_time), pointer :: p_time(:)
        type(parquet_timestamp), pointer :: p_ts(:)
        integer(int64) :: k

        allocate(is_valid(max(nelems, 1_int64)))
        is_valid = .true.
        any_null = .false.
        if (nelems <= 0_int64) return
        select case (kind)
        case (PK_DATE)
            call parquet_column_data_ptr(pay, p_date)
            is_valid(1_int64:nelems) = .not. p_date(1_int64:nelems)%is_null()
        case (PK_TIME)
            call parquet_column_data_ptr(pay, p_time)
            is_valid(1_int64:nelems) = .not. p_time(1_int64:nelems)%is_null()
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(pay, p_ts)
            is_valid(1_int64:nelems) = .not. p_ts(1_int64:nelems)%is_null()
        case default
            do k = 1_int64, nelems
                is_valid(k) = .not. parquet_column_is_null(pay, k)
            end do
        end select
        any_null = .not. all(is_valid(1_int64:nelems))
    end subroutine list_element_validity

    !> The shared body of both list write specifics. `chunked` selects the row-group-scoped
    !> counterpart of every step that differs: which mask helper supplies the row mask, which row
    !> count is checked, which mark-written call is made, and which `bind(C)` family is called.
    !>
    !> One worker rather than two because everything between those steps -- the preamble, the
    !> masked rebuild, both validity levels, the protected/qc checks and the payload dispatch --
    !> is identical, and a second copy of it would be a second place for the two null levels to be
    !> got wrong.
    subroutine write_list_common(writer, name, values, chunked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_list_column), intent(in), target :: values !! the column to write.
        logical, intent(in) :: chunked !! .true. for the row-group-scoped form.
        character(len=:), allocatable :: outname, ctx, context
        integer :: idx, kind, unit
        integer(c_int32_t) :: is_utc
        integer(int64) :: nrows, nelems, i
        logical :: do_write, masked, any_row_null, any_elem_null, protected
        logical, allocatable :: row_mask(:), row_valid(:), elem_valid(:)
        integer(c_int8_t), allocatable, target :: row_buf(:), elem_buf(:)
        type(c_ptr) :: row_ptr, elem_ptr
        integer(int64), pointer :: offs(:)
        type(parquet_column), pointer :: pay
        type(parquet_list_column), target :: values_c !! row-masked rebuild; unused on the fast path.
        type(parquet_list_column), pointer :: v !! the column actually written.

        context = "parquet_write_column"
        if (chunked) context = "parquet_write_column_chunk"

        if (.not. values%is_init()) then
            call writer_context_suffix(writer, ctx)
            error stop context // ": the list column passed for '" // trim(name) // &
                "' has not been initialized; call %init(<payload kind>) first" // ctx
        end if
        kind = values%element_kind()
        call list_write_preamble(writer, name, kind, context, do_write, outname, idx)
        if (.not. do_write) return
        call resolve_temporal_write_unit(writer, idx, unit, is_utc)

        nrows = values%size()
        if (chunked) then
            call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        else
            call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        end if

        if (masked) then
            ! %gather_rows rebuilds offsets, row validity and the payload (element validity and a
            ! string payload included) in one kind-dispatched pass, and drops the elements of rows
            ! the index list does not name -- so a masked write also compacts away exactly the
            ! unreachable elements the unmasked path deliberately leaves alone.
            call values%deep_copy(values_c)
            call values_c%gather_rows(pack([(i, i = 1_int64, nrows)], row_mask))
            v => values_c
            nrows = count(row_mask, kind=int64)
        else
            ! Nothing to remove, so the caller's own column is written as it stands.
            v => values
        end if

        call parquet_list_column_offsets(v, offs)
        call parquet_list_column_payload(v, pay)
        nelems = offs(nrows + 1_int64)

        allocate(row_valid(max(nrows, 1_int64)))
        call parquet_list_column_row_validity(v, row_valid, any_row_null)
        call list_element_validity(pay, nelems, kind, elem_valid, any_elem_null)

        ! A protected list column may hold neither a null ROW nor a null ELEMENT: at the Parquet
        ! level a null element IS a Null in the same leaf column the row nullness lives in, so the
        ! weaker reading would declare a non-nullable element field for a column that can contain
        ! null elements -- which is exactly the invariant build_field's safety comment forbids.
        ! The row half is parquet_check_protected's ordinary job; the element half is checked here
        ! so that its message names which level failed.
        call parquet_check_protected(writer, name, row_valid(1_int64:nrows), protected)
        if (protected .and. any_elem_null) then
            error stop context // ": column '" // trim(name) // &
                "' is protected (extra: protected_cols:) and cannot contain Null values" // &
                " -- one of its list ELEMENTS is Null"
        end if
        call parquet_check_qc_miss(writer, name, row_valid(1_int64:nrows))

        if (any_row_null) then
            call parquet_make_valid_buf_write(row_valid(1_int64:nrows), row_buf, row_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=row_buf, valid_ptr=row_ptr)
        end if
        if (any_elem_null) then
            call parquet_make_valid_buf_write(elem_valid(1_int64:nelems), elem_buf, elem_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=elem_buf, valid_ptr=elem_ptr)
        end if

        if (chunked) then
            call parquet_chunk_mark_written_if_first(writer, name)
        else
            call parquet_check_row_count(writer, name, nrows)
            call parquet_mark_column_written(writer, name)
        end if

        call send_list_payload(writer, outname, pay, kind, nrows, nelems, offs, row_ptr, elem_ptr, &
            unit, is_utc, chunked)
    end subroutine write_list_common

    !> Hands one list column over to C++, dispatched on the payload kind: nine families, each with
    !> a whole-column and a row-group-scoped entry point.
    !>
    !> Every arm passes the SAME shape -- nrows, nelems, offsets, row validity, values, element
    !> validity -- so the only per-kind differences are the value buffer's type and, for the two
    !> unit-carrying temporal kinds, the unit itself.
    subroutine send_list_payload(writer, outname, pay, kind, nrows, nelems, offs, row_ptr, elem_ptr, &
        unit, is_utc, chunked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: outname !! resolved output (file) column name.
        type(parquet_column), intent(in) :: pay !! the flattened payload column being written.
        integer, intent(in) :: kind !! the payload's PK_* kind.
        integer(int64), intent(in) :: nrows !! rows in this write.
        integer(int64), intent(in) :: nelems !! elements those rows hold.
        integer(int64), intent(in) :: offs(:) !! nrows+1 offsets.
        type(c_ptr), intent(in) :: row_ptr !! per-ROW validity, or c_null_ptr.
        type(c_ptr), intent(in) :: elem_ptr !! per-ELEMENT validity, or c_null_ptr.
        integer, intent(in) :: unit !! resolved temporal unit selector.
        integer(c_int32_t), intent(in) :: is_utc !! 1 if the timestamp payload is UTC-adjusted.
        logical, intent(in) :: chunked !! .true. for the row-group-scoped entry points.
        integer(int32), pointer :: p_i32(:)
        integer(int64), pointer :: p_i64(:)
        real(real32), pointer :: p_f32(:)
        real(real64), pointer :: p_f64(:)
        logical, pointer :: p_bool(:)
        type(parquet_date), pointer :: p_date(:)
        type(parquet_time), pointer :: p_time(:)
        type(parquet_timestamp), pointer :: p_ts(:)
        type(parquet_string_column), pointer :: sc
        integer(c_int8_t), allocatable, target :: b_bool(:)
        integer(c_int32_t), allocatable, target :: b_i32(:)
        integer(c_int64_t), allocatable, target :: b_i64(:)
        real(c_float), allocatable, target :: b_f32(:)
        real(c_double), allocatable, target :: b_f64(:)
        integer(int64), pointer :: str_offs(:)
        character(kind=c_char), pointer :: str_data(:)
        character(kind=c_char), allocatable, target :: str_scratch(:)
        integer(int64), allocatable, target :: str_offs_scratch(:)
        type(c_ptr) :: off_ptr, dat_ptr, val_ptr
        integer(int64) :: k, nstr, nchars
        logical :: has_validity
        character(len=:), allocatable :: kname, ctx

        ! Every buffer below is sized max(nelems, 1): a zero-size actual against an assumed-size
        ! bind(C) dummy is not something to rely on, and a list column with no elements at all is
        ! an ordinary, documented case (every row null, every row empty, or a declared column
        ! written with zero rows at close).
        !
        ! EVERY parquet_column_data_ptr CALL IS GUARDED BY `nelems > 0` FOR THE SAME REASON, and
        ! the guard is not decoration: a zero-element payload never allocated its storage at all
        ! (grow_storage returns early at n == 0), so `p => col%i32(1:col%nrows)` references an
        ! unallocated allocatable. Only nagfor's -C=array sees it -- gfortran, ifx and flang hand
        ! back a null-based pointer nothing then dereferences, so the whole suite stays green.
        ! CLAUDE.md, "a zero-length case reaches storage that was never allocated".
        select case (kind)
        case (PK_INT32)
            allocate(b_i32(max(nelems, 1_int64)))
            b_i32 = 0_c_int32_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_i32)
                b_i32(1_int64:nelems) = int(p_i32(1_int64:nelems), kind=c_int32_t)
            end if
            if (chunked) then
                call parquet_append_list_int32_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i32, elem_ptr)
            else
                call parquet_append_list_int32_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i32, elem_ptr)
            end if
        case (PK_INT64)
            allocate(b_i64(max(nelems, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_i64)
                b_i64(1_int64:nelems) = int(p_i64(1_int64:nelems), kind=c_int64_t)
            end if
            if (chunked) then
                call parquet_append_list_int64_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr)
            else
                call parquet_append_list_int64_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr)
            end if
        case (PK_FLOAT32)
            allocate(b_f32(max(nelems, 1_int64)))
            b_f32 = 0.0_c_float
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_f32)
                b_f32(1_int64:nelems) = real(p_f32(1_int64:nelems), kind=c_float)
            end if
            if (chunked) then
                call parquet_append_list_float32_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_f32, elem_ptr)
            else
                call parquet_append_list_float32_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_f32, elem_ptr)
            end if
        case (PK_FLOAT64)
            allocate(b_f64(max(nelems, 1_int64)))
            b_f64 = 0.0_c_double
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_f64)
                b_f64(1_int64:nelems) = real(p_f64(1_int64:nelems), kind=c_double)
            end if
            if (chunked) then
                call parquet_append_list_float64_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_f64, elem_ptr)
            else
                call parquet_append_list_float64_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_f64, elem_ptr)
            end if
        case (PK_LOGICAL)
            allocate(b_bool(max(nelems, 1_int64)))
            b_bool = 0_c_int8_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_bool)
                do k = 1_int64, nelems
                    if (p_bool(k)) b_bool(k) = 1_c_int8_t
                end do
            end if
            if (chunked) then
                call parquet_append_list_bool8_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_bool, elem_ptr)
            else
                call parquet_append_list_bool8_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_bool, elem_ptr)
            end if
        case (PK_DATE)
            allocate(b_i32(max(nelems, 1_int64)))
            b_i32 = 0_c_int32_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_date)
                b_i32(1_int64:nelems) = p_date(1_int64:nelems)%raw()
            end if
            if (chunked) then
                call parquet_append_list_date_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i32, elem_ptr)
            else
                call parquet_append_list_date_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i32, elem_ptr)
            end if
        case (PK_TIME)
            allocate(b_i64(max(nelems, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_time)
                b_i64(1_int64:nelems) = p_time(1_int64:nelems)%raw()
            end if
            if (chunked) then
                call parquet_append_list_time_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr, int(unit, c_int32_t))
            else
                call parquet_append_list_time_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr, int(unit, c_int32_t))
            end if
        case (PK_TIMESTAMP)
            allocate(b_i64(max(nelems, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nelems > 0_int64) then
                call parquet_column_data_ptr(pay, p_ts)
                ! A null instant has no value to convert, and %to_unix would abort on one; the
                ! element validity buffer carries its nullness across, so 0 is never read.
                do k = 1_int64, nelems
                    if (.not. p_ts(k)%is_null()) b_i64(k) = p_ts(k)%to_unix(unit)
                end do
            end if
            if (chunked) then
                call parquet_append_list_timestamp_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr, int(unit, c_int32_t), is_utc)
            else
                call parquet_append_list_timestamp_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, offs, row_ptr, b_i64, elem_ptr, int(unit, c_int32_t), is_utc)
            end if
        case (PK_STRING)
            ! The payload's own packed offsets+bytes go over unchanged -- the same layout Arrow
            ! wants, so this is a copy rather than a translation. The bitmap raw_buffers also
            ! offers is deliberately NOT used: element nullness crosses as the same one-byte-per-
            ! element buffer every other family uses, built once by list_element_validity.
            call parquet_column_string_column(pay, sc)
            call parquet_string_column_raw_buffers(sc, off_ptr, dat_ptr, val_ptr, nstr, nchars, has_validity)
            if (c_associated(off_ptr)) then
                call c_f_pointer(off_ptr, str_offs, [nstr + 1_int64])
            else
                allocate(str_offs_scratch(1))
                str_offs_scratch(1) = 0_int64
                str_offs => str_offs_scratch
            end if
            if (nchars > 0_int64 .and. c_associated(dat_ptr)) then
                call c_f_pointer(dat_ptr, str_data, [nchars])
            else
                allocate(str_scratch(1))
                str_scratch(1) = c_null_char
                str_data => str_scratch
            end if
            if (chunked) then
                call parquet_append_list_string_column_chunk(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, nchars, offs, row_ptr, str_offs, str_data, elem_ptr)
            else
                call parquet_append_list_string_column(writer%handle, trim(outname)//char(0), nrows, &
                    nelems, nchars, offs, row_ptr, str_offs, str_data, elem_ptr)
            end if
        case default
            ! Not reachable: list_write_preamble's type token has already refused every kind this
            ! does not cover, and %init refuses to create a column of one. Kept as a second line
            ! of defence, and it names what it saw.
            call parquet_kind_name(kind, kname) ! GCOVR_EXCL_LINE
            call writer_context_suffix(writer, ctx) ! GCOVR_EXCL_LINE
            error stop "parquet_write_column: unsupported list payload kind '" // kname // &
                "' for column " // trim(outname) // ctx ! GCOVR_EXCL_LINE
        end select
    end subroutine send_list_payload

    module procedure parquet_write_list_column
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_list_common(writer, name, values, .false.)
    end procedure parquet_write_list_column

    !> Writes a declared-but-never-written `list[<elemtype>]` column with zero rows; see the
    !> interface in parquet_core.f90 for why it takes the element base TOKEN rather than a kind.
    !>
    !> Placed BELOW parquet_write_list_column, which it calls, and that ordering is load-bearing:
    !> nagfor binds a separate module procedure's name to an implicit EXTERNAL at the first call
    !> site, so implementing it later in the same submodule is rejected outright ("... is not the
    !> interface of a separate module procedure"). gfortran, ifx and flang accept either order, so
    !> nothing else in the fleet enforces it -- see CLAUDE.md, "A separate module procedure must be
    !> IMPLEMENTED before it is CALLED in the same submodule".
    module procedure parquet_write_empty_list_column
        type(parquet_list_column) :: empty_list
        integer :: kind
        select case (trim(elem_base))
        case ("int32");     kind = PK_INT32
        case ("int64");     kind = PK_INT64
        case ("float32");   kind = PK_FLOAT32
        case ("float64");   kind = PK_FLOAT64
        case ("boolean");   kind = PK_LOGICAL
        case ("string");    kind = PK_STRING
        case ("date");      kind = PK_DATE
        case ("time");      kind = PK_TIME
        case ("timestamp"); kind = PK_TIMESTAMP
        case default
            ! Unreachable: parquet_parse_list_type reports a well-formed token only for these nine,
            ! and the caller has already checked that. An error stop rather than a sentinel kind, so
            ! a future tenth element token fails where it is missed rather than inside %init.
            error stop "parquet_close_writer: unsupported list element type '"//trim(elem_base)//"'" ! GCOVR_EXCL_LINE
        end select
        call empty_list%init(kind)
        call parquet_write_list_column(writer, name, empty_list)
    end procedure parquet_write_empty_list_column

    module procedure parquet_write_list_column_chunk
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_list_common(writer, name, values, .true.)
    end procedure parquet_write_list_column_chunk

end submodule parquet_write_list
