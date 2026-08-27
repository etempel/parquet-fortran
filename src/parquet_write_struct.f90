!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> `STRUCT` write specifics (whole-column and row-group-scoped): bodies of the
!> module procedures declared in parquet_core.f90's interface block, plus the
!> private workers this file exists to hold.
!>
!> The mirror of parquet_read_struct.f90, and unlike it this file really does
!> need one crossing per FIELD -- but staged rather than parallel: a struct with
!> M fields of arbitrary kinds cannot cross a fixed bind(C) signature in one
!> call, so a write opens staging on the writer (parquet_struct_begin), pushes
!> one field at a time (parquet_struct_field_<kind>) and then assembles
!> (parquet_append_struct_column). See src/parquet_wrapper.cpp's "STRUCT column
!> writes" banner for the C++ half.
!>
!> **The writer's concurrency guard is held across the WHOLE sequence**, by the
!> `writer_lock` the two module procedures at the bottom claim. That is not new
!> machinery -- every write takes it -- but this is the first time it protects
!> state spanning several C++ calls, and without it two threads writing two
!> struct columns to one writer would interleave their pushes.
!>
!> Two things are specific to a struct column and worth reading before changing
!> anything here. Its nullness has 1 + M levels -- a null ROW (an absent struct
!> instance) and a null value in each FIELD of a present row -- which are
!> independent and all of which cross; and each field's values are reached
!> through `parquet_struct_column_field`/`_names`/`_row_validity` rather than
!> through a row handle per row, because a handle per row is one allocation per
!> row on a path whose whole purpose is to hand over contiguous buffers.
submodule (parquet_core:parquet_write) parquet_write_struct
    implicit none
contains

    !> Shared bookkeeping for both struct write specifics: schema checks (defined, enabled,
    !> col_size, exact data_type match), mark-written, and output-name resolution -- the struct
    !> column's counterpart to list_write_preamble. Returns do_write=.false. (caller returns
    !> without writing) for a defined-but-disabled column.
    !>
    !> The declared token is the bare `struct`, with no bracket and no field layout: a struct's
    !> fields come entirely from the `parquet_struct_column` the caller passes, so the schema says
    !> only that the column IS one. That is a campaign decision, and the reason is that declaring
    !> the layout twice would create a consistency check with no purpose.
    subroutine struct_write_preamble(writer, name, context, do_write, outname)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: context !! "parquet_write_column" or "..._chunk".
        logical, intent(out) :: do_write !! .true. if the column should actually be written.
        character(len=:), allocatable, intent(out) :: outname !! resolved output (file) name.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        integer :: idx

        do_write = .false.
        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                context // ": column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, 1_int64, context, ctx)
            if (trim(writer%all_columns(idx)%data_type) /= "struct") then
                error stop context // ": type mismatch for column " // trim(name) // &
                    " (expected struct, got " // trim(writer%all_columns(idx)%data_type) // &
                    "); the column passed is a struct column while the schema declares " // &
                    trim(writer%all_columns(idx)%data_type) // ctx
            end if
        end if
        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_resolve_output_name(writer, name, outname)
        do_write = .true.
    end subroutine struct_write_preamble

    ! Its interface, and its documentation, live in parquet_core.f90: parquet_write_map stages a
    ! map's VALUES through this same rule, and a sibling submodule can only reach it that way.
    module procedure struct_field_validity
        type(parquet_date), pointer :: p_date(:)
        type(parquet_time), pointer :: p_time(:)
        type(parquet_timestamp), pointer :: p_ts(:)
        integer(int64) :: k

        allocate(is_valid(max(nrows, 1_int64)))
        is_valid = .true.
        any_null = .false.
        if (nrows <= 0_int64) return
        select case (kind)
        case (PK_DATE)
            call parquet_column_data_ptr(fcol, p_date)
            is_valid(1_int64:nrows) = .not. p_date(1_int64:nrows)%is_null()
        case (PK_TIME)
            call parquet_column_data_ptr(fcol, p_time)
            is_valid(1_int64:nrows) = .not. p_time(1_int64:nrows)%is_null()
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(fcol, p_ts)
            is_valid(1_int64:nrows) = .not. p_ts(1_int64:nrows)%is_null()
        case default
            do k = 1_int64, nrows
                is_valid(k) = .not. parquet_column_is_null(fcol, k)
            end do
        end select
        any_null = .not. all(is_valid(1_int64:nrows))
    end procedure struct_field_validity

    ! Its interface, and its documentation, live in parquet_core.f90, for the same reason as
    ! struct_field_validity above -- parquet_write_map stages a map's keys and values through it.
    !
    ! Every buffer below is sized max(nrows, 1), and every `parquet_column_data_ptr` call is
    ! guarded by `nrows > 0`, for the reason parquet_write_list.f90's own dispatch records: a
    ! zero-row column never allocated its storage at all (grow_storage returns early at n == 0),
    ! so reaching for a pointer into it references an unallocated allocatable. Only nagfor's
    ! `-C=array` sees it -- gfortran, ifx and flang hand back a null-based pointer nothing then
    ! dereferences, so the whole suite stays green.
    module procedure push_struct_field
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
        type(c_ptr) :: off_ptr, dat_ptr, sval_ptr
        integer(int64) :: k, nstr, nchars
        logical :: has_validity
        character(len=:), allocatable :: kname, ctx
        integer :: wunit
        integer(c_int32_t) :: wutc

        ! Microseconds, timezone-naive, unless the caller declared otherwise -- which only a map
        ! write does, since a struct field has no MAML declaration to carry a unit.
        wunit = parquet_unit_micros
        if (present(unit)) wunit = unit
        wutc = 0_c_int32_t
        if (present(is_utc)) wutc = is_utc
        select case (kind)
        case (PK_INT32)
            allocate(b_i32(max(nrows, 1_int64)))
            b_i32 = 0_c_int32_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_i32)
                b_i32(1_int64:nrows) = int(p_i32(1_int64:nrows), kind=c_int32_t)
            end if
            call parquet_struct_field_int32(writer%handle, trim(fname)//char(0), b_i32, nrows, val_ptr)
        case (PK_INT64)
            allocate(b_i64(max(nrows, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_i64)
                b_i64(1_int64:nrows) = int(p_i64(1_int64:nrows), kind=c_int64_t)
            end if
            call parquet_struct_field_int64(writer%handle, trim(fname)//char(0), b_i64, nrows, val_ptr)
        case (PK_FLOAT32)
            allocate(b_f32(max(nrows, 1_int64)))
            b_f32 = 0.0_c_float
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_f32)
                b_f32(1_int64:nrows) = real(p_f32(1_int64:nrows), kind=c_float)
            end if
            call parquet_struct_field_float32(writer%handle, trim(fname)//char(0), b_f32, nrows, val_ptr)
        case (PK_FLOAT64)
            allocate(b_f64(max(nrows, 1_int64)))
            b_f64 = 0.0_c_double
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_f64)
                b_f64(1_int64:nrows) = real(p_f64(1_int64:nrows), kind=c_double)
            end if
            call parquet_struct_field_float64(writer%handle, trim(fname)//char(0), b_f64, nrows, val_ptr)
        case (PK_LOGICAL)
            allocate(b_bool(max(nrows, 1_int64)))
            b_bool = 0_c_int8_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_bool)
                do k = 1_int64, nrows
                    if (p_bool(k)) b_bool(k) = 1_c_int8_t
                end do
            end if
            call parquet_struct_field_bool8(writer%handle, trim(fname)//char(0), b_bool, nrows, val_ptr)
        case (PK_DATE)
            allocate(b_i32(max(nrows, 1_int64)))
            b_i32 = 0_c_int32_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_date)
                b_i32(1_int64:nrows) = p_date(1_int64:nrows)%raw()
            end if
            call parquet_struct_field_date(writer%handle, trim(fname)//char(0), b_i32, nrows, val_ptr)
        case (PK_TIME)
            allocate(b_i64(max(nrows, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_time)
                b_i64(1_int64:nrows) = p_time(1_int64:nrows)%raw()
            end if
            call parquet_struct_field_time(writer%handle, trim(fname)//char(0), b_i64, nrows, val_ptr, &
                int(wunit, c_int32_t))
        case (PK_TIMESTAMP)
            allocate(b_i64(max(nrows, 1_int64)))
            b_i64 = 0_c_int64_t
            if (nrows > 0_int64) then
                call parquet_column_data_ptr(fcol, p_ts)
                ! A null instant has no value to convert, and %to_unix would abort on one; the
                ! validity buffer carries its nullness across, so 0 is never read.
                do k = 1_int64, nrows
                    if (.not. p_ts(k)%is_null()) b_i64(k) = p_ts(k)%to_unix(wunit)
                end do
            end if
            call parquet_struct_field_timestamp(writer%handle, trim(fname)//char(0), b_i64, nrows, val_ptr, &
                int(wunit, c_int32_t), wutc)
        case (PK_STRING)
            ! The field's own packed offsets+bytes go over unchanged -- the same layout Arrow
            ! wants, so this is a copy rather than a translation. The bitmap raw_buffers also
            ! offers is deliberately NOT used: field nullness crosses as the same one-byte-per-row
            ! buffer every other family uses, built once by struct_field_validity.
            call parquet_column_string_column(fcol, sc)
            call parquet_string_column_raw_buffers(sc, off_ptr, dat_ptr, sval_ptr, nstr, nchars, has_validity)
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
            call parquet_struct_field_string(writer%handle, trim(fname)//char(0), str_offs, str_data, &
                nrows, nchars, val_ptr)
        case default
            ! Not reachable: %init refuses any other field kind, so a struct column cannot exist
            ! with one. Kept as a second line of defence, and it names what it saw.
            call parquet_kind_name(kind, kname) ! GCOVR_EXCL_LINE
            call writer_context_suffix(writer, ctx) ! GCOVR_EXCL_LINE
            error stop "parquet_write_column: unsupported struct field kind '" // kname // &
                "' for field " // trim(fname) // ctx ! GCOVR_EXCL_LINE
        end select
    end procedure push_struct_field

    !> The shared body of both struct write specifics. `chunked` selects the row-group-scoped
    !> counterpart of every step that differs: which mask helper supplies the row mask, which row
    !> count is checked, which mark-written call is made, and which finisher is called.
    !>
    !> One worker rather than two because everything between those steps -- the preamble, the
    !> masked rebuild, all 1 + M validity levels, the protected/qc checks and the field staging --
    !> is identical, and a second copy of it would be a second place for the null levels to be got
    !> wrong.
    subroutine write_struct_common(writer, name, values, chunked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_struct_column), intent(in), target :: values !! the column to write.
        logical, intent(in) :: chunked !! .true. for the row-group-scoped form.
        character(len=:), allocatable :: outname, ctx, context
        character(len=:), allocatable :: fnames(:)
        integer :: j, nf, kind
        integer(int64) :: nrows, i
        logical :: do_write, masked, any_row_null, any_field_null, protected
        logical, allocatable :: row_mask(:), row_valid(:), fld_valid(:)
        integer(c_int8_t), allocatable, target :: row_buf(:), fld_buf(:)
        type(c_ptr) :: row_ptr, fld_ptr
        type(parquet_column), pointer :: fcol
        type(parquet_struct_column), target :: values_c !! row-masked rebuild; unused on the fast path.
        type(parquet_struct_column), pointer :: v !! the column actually written.

        context = "parquet_write_column"
        if (chunked) context = "parquet_write_column_chunk"

        if (.not. values%is_init()) then
            call writer_context_suffix(writer, ctx)
            error stop context // ": the struct column passed for '" // trim(name) // &
                "' has not been initialized; call %init(<names>, <kinds>) first" // ctx
        end if
        call struct_write_preamble(writer, name, context, do_write, outname)
        if (.not. do_write) return

        nrows = values%size()
        if (chunked) then
            call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        else
            call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        end if

        if (masked) then
            ! %gather_rows rebuilds every field (its own validity and a string field included) and
            ! the row bitmap in one kind-dispatched pass per field.
            call values%deep_copy(values_c)
            call values_c%gather_rows(pack([(i, i = 1_int64, nrows)], row_mask))
            v => values_c
            nrows = count(row_mask, kind=int64)
        else
            ! Nothing to remove, so the caller's own column is written as it stands.
            v => values
        end if

        nf = v%field_count()
        call parquet_struct_column_names(v, fnames)
        allocate(row_valid(max(nrows, 1_int64)))
        call parquet_struct_column_row_validity(v, row_valid, any_row_null)

        ! A protected struct column may hold neither a null ROW nor a null FIELD value: at the
        ! Parquet level a null field IS a Null in a leaf column of this struct, so the weaker
        ! reading would declare a non-nullable field for a column that can contain nulls -- which
        ! is exactly the invariant build_field's safety comment forbids. The row half is
        ! parquet_check_protected's ordinary job; the field half is checked below so that its
        ! message names which level, and which field, failed.
        call parquet_check_protected(writer, name, row_valid(1_int64:nrows), protected)
        call parquet_check_qc_miss(writer, name, row_valid(1_int64:nrows))

        if (any_row_null) then
            call parquet_make_valid_buf_write(row_valid(1_int64:nrows), row_buf, row_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=row_buf, valid_ptr=row_ptr)
        end if

        if (chunked) then
            call parquet_chunk_mark_written_if_first(writer, name)
        else
            call parquet_check_row_count(writer, name, nrows)
            call parquet_mark_column_written(writer, name)
        end if

        ! Staging: open, push every field in declaration order, then finish. The writer's
        ! concurrency guard is held by the caller across all of it -- see the file header.
        call parquet_struct_begin(writer%handle, trim(outname)//char(0), nrows, int(nf, c_int32_t))
        do j = 1, nf
            call parquet_struct_column_field(v, j, fcol)
            kind = fcol%kindof()
            call struct_field_validity(fcol, nrows, kind, fld_valid, any_field_null)
            if (protected .and. any_field_null) then
                error stop context // ": column '" // trim(name) // &
                    "' is protected (extra: protected_cols:) and cannot contain Null values" // &
                    " -- field '" // trim(fnames(j)) // "' holds a Null"
            end if
            if (any_field_null) then
                call parquet_make_valid_buf_write(fld_valid(1_int64:nrows), fld_buf, fld_ptr)
            else
                call parquet_make_valid_buf_write(valid_buf=fld_buf, valid_ptr=fld_ptr)
            end if
            call push_struct_field(writer, fnames(j), fcol, kind, nrows, fld_ptr)
            ! parquet_make_valid_buf_write leaves `fld_buf` UNALLOCATED on its no-mask arm (a
            ! null-free field passes c_null_ptr and needs no buffer), so both deallocations are
            ! guarded. Reallocating on the next iteration would otherwise be an error too.
            if (allocated(fld_valid)) deallocate(fld_valid)
            if (allocated(fld_buf)) deallocate(fld_buf)
        end do
        if (chunked) then
            call parquet_append_struct_column_chunk(writer%handle, trim(outname)//char(0), nrows, row_ptr)
        else
            call parquet_append_struct_column(writer%handle, trim(outname)//char(0), nrows, row_ptr)
        end if
    end subroutine write_struct_common

    module procedure parquet_write_struct_column
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_struct_common(writer, name, values, .false.)
    end procedure parquet_write_struct_column

    module procedure parquet_write_struct_column_chunk
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_struct_common(writer, name, values, .true.)
    end procedure parquet_write_struct_column_chunk

end submodule parquet_write_struct
