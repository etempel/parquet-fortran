!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Temporal (date/time/timestamp) write specifics, scalar and matrix, whole-
!> column and row-group-chunked: bodies of the module procedures declared in
!> parquet.f90's interface block, plus the temporal-only private helpers
!> (preamble/unit-resolution/validity/flat-write workers) they depend on.
submodule (parquet:parquet_write) parquet_write_temporal
    implicit none
contains

    !> Shared bookkeeping for every temporal write specific: schema checks (defined, enabled,
    !> col_size, exact data_type match), mark-written, and output-name resolution. Returns
    !> do_write=.false. (caller returns without writing) for a defined-but-disabled column;
    !> `idx` is the schema column index (0 for a schema-less writer), for unit resolution.
    subroutine temporal_write_preamble(writer, name, expected_type, asize, do_write, outname, idx)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: expected_type !! "date"/"time"/"timestamp".
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        logical, intent(out) :: do_write !! .true. if the column should actually be written.
        character(len=:), allocatable, intent(out) :: outname !! resolved output (file) name.
        integer, intent(out) :: idx !! schema column index, or 0 for a schema-less writer.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        do_write = .false.
        idx = 0
        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column", ctx)
            if (trim(writer%all_columns(idx)%data_type) /= expected_type) then
                error stop "parquet_write_column: type mismatch for column " // trim(name) // &
                    " (expected " // expected_type // ", got " // trim(writer%all_columns(idx)%data_type) // ")" // ctx
            end if
        end if
        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)
        call parquet_resolve_output_name(writer, name, outname)
        do_write = .true.
    end subroutine temporal_write_preamble
    !> Resolves the file unit (a parquet_unit_* selector) and UTC flag for a temporal write:
    !> the schema-declared unit/utc when the column comes from a MAML/schema (idx > 0 and its
    !> time_unit is set), else the fixed default (microseconds, timezone-naive) for a
    !> schema-less writer or a schema column with no explicit unit.
    subroutine resolve_temporal_write_unit(writer, idx, unit, is_utc)
        type(parquet_writer), intent(in) :: writer !! open writer.
        integer, intent(in) :: idx !! schema column index, or 0 for schema-less.
        integer, intent(out) :: unit !! resolved unit selector.
        integer(c_int32_t), intent(out) :: is_utc !! 1 if UTC-adjusted, else 0.

        unit = parquet_unit_micros
        is_utc = 0_c_int32_t
        if (idx > 0) then
            if (writer%all_columns(idx)%time_unit /= 0) unit = writer%all_columns(idx)%time_unit
            if (writer%all_columns(idx)%is_utc) is_utc = 1_c_int32_t
        end if
    end subroutine resolve_temporal_write_unit
    !> Builds the write-side validity pointer for a temporal column: a null-free column is
    !> written non-nullable (c_null_ptr); otherwise a 1=valid/0=null buffer is built from the
    !> per-element null mask, and (for a schema-enforced writer) parquet_check_protected enforces
    !> that no protected column receives a null.
    subroutine temporal_valid_ptr(writer, name, is_null_mask, valid_buf, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name (for the protected-column check).
        logical, intent(in) :: is_null_mask(:) !! .true. where the element is null.
        integer(c_int8_t), allocatable, target, intent(out) :: valid_buf(:) !! backing buffer for valid_ptr.
        type(c_ptr), intent(out) :: valid_ptr !! c_loc(valid_buf), or c_null_ptr if no nulls.

        if (any(is_null_mask)) then
            if (writer%is_schema_enforced) call parquet_check_protected(writer, name, .not. is_null_mask)
            call parquet_make_valid_buf_write(.not. is_null_mask, valid_buf, valid_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
    end subroutine temporal_valid_ptr
    !> Flat worker for the scalar and matrix date writes: `flat` holds asize*nrows dates in
    !> (element, row)-major order (a scalar column is asize == 1).
    subroutine write_date_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_date), intent(in) :: flat(:) !! flattened dates.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! row count.
        logical :: do_write
        integer :: idx
        character(len=:), allocatable :: outname
        integer(c_int32_t), allocatable :: days(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_date), allocatable :: flat_c(:)
        integer(int64) :: nrows_c

        call check_writer_open(writer)
        call temporal_write_preamble(writer, name, "date", asize, do_write, outname, idx)
        if (.not. do_write) return
        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        nrows_c = count(row_mask, kind=int64)
        allocate(days(size(flat_c, kind=int64)))
        days = flat_c%raw()
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows_c)
        call parquet_append_date_column(writer%handle, trim(outname)//char(0), days, nrows_c, asize, valid_ptr)
    end subroutine write_date_flat
    !> Flat worker for the scalar and matrix time writes; see write_date_flat.
    subroutine write_time_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_time), intent(in) :: flat(:) !! flattened times.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! row count.
        logical :: do_write
        integer :: idx, unit
        integer(c_int32_t) :: is_utc
        character(len=:), allocatable :: outname
        integer(c_int64_t), allocatable :: ns(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_time), allocatable :: flat_c(:)
        integer(int64) :: nrows_c

        call check_writer_open(writer)
        call temporal_write_preamble(writer, name, "time", asize, do_write, outname, idx)
        if (.not. do_write) return
        call resolve_temporal_write_unit(writer, idx, unit, is_utc)
        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        nrows_c = count(row_mask, kind=int64)
        allocate(ns(size(flat_c, kind=int64)))
        ns = flat_c%raw()
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows_c)
        call parquet_append_time_column(writer%handle, trim(outname)//char(0), ns, nrows_c, asize, &
            int(unit, c_int32_t), valid_ptr)
    end subroutine write_time_flat
    !> Flat worker for the scalar and matrix timestamp writes; see write_date_flat. Each
    !> non-null instant is converted to the file unit exactly (to_unix aborts on finer precision).
    subroutine write_timestamp_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_timestamp), intent(in) :: flat(:) !! flattened instants.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! row count.
        logical :: do_write
        integer :: idx, unit
        integer(c_int32_t) :: is_utc
        character(len=:), allocatable :: outname
        integer(c_int64_t), allocatable :: vals(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_timestamp), allocatable :: flat_c(:)
        integer(int64) :: nrows_c

        call check_writer_open(writer)
        call temporal_write_preamble(writer, name, "timestamp", asize, do_write, outname, idx)
        if (.not. do_write) return
        call resolve_temporal_write_unit(writer, idx, unit, is_utc)
        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        nrows_c = count(row_mask, kind=int64)
        n = size(flat_c, kind=int64)
        allocate(vals(n))
        do i = 1_int64, n
            if (flat_c(i)%is_null()) then
                vals(i) = 0_c_int64_t
            else
                vals(i) = flat_c(i)%to_unix(unit)
            end if
        end do
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows_c)
        call parquet_append_timestamp_column(writer%handle, trim(outname)//char(0), vals, nrows_c, asize, &
            int(unit, c_int32_t), is_utc, valid_ptr)
    end subroutine write_timestamp_flat
    module procedure parquet_write_date_column
        call write_date_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_date_column
    module procedure parquet_write_date_matrix_column
        call write_date_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_date_matrix_column
    module procedure parquet_write_time_column
        call write_time_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_time_column
    module procedure parquet_write_time_matrix_column
        call write_time_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_time_matrix_column
    module procedure parquet_write_timestamp_column
        call write_timestamp_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_timestamp_column
    module procedure parquet_write_timestamp_matrix_column
        call write_timestamp_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_timestamp_matrix_column
    ! ---- Temporal streaming (row-group-chunked) writes -------------------------------
    !> Chunk counterpart of temporal_write_preamble: schema checks + exact type match, the
    !> streaming row-group row-count check, and first-chunk mark-written. Returns do_write.
    subroutine temporal_chunk_preamble(writer, name, expected_type, asize, nrows, do_write, outname, idx, row_mask)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: expected_type !! "date"/"time"/"timestamp".
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! this chunk's row count.
        logical, intent(out) :: do_write !! whether to write this chunk.
        character(len=:), allocatable, intent(out) :: outname !! resolved output name.
        integer, intent(out) :: idx !! schema column index, or 0 schema-less.
        logical, allocatable, intent(out) :: row_mask(:) !! this chunk's applicable row-keep mask, `nrows` long.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        do_write = .false.
        idx = 0
        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column_chunk", ctx)
            if (trim(writer%all_columns(idx)%data_type) /= expected_type) then
                error stop "parquet_write_column_chunk: type mismatch for column " // trim(name) // &
                    " (expected " // expected_type // ", got " // trim(writer%all_columns(idx)%data_type) // ")" // ctx
            end if
        end if
        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        call parquet_chunk_mark_written_if_first(writer, name)
        call parquet_resolve_output_name(writer, name, outname)
        do_write = .true.
    end subroutine temporal_chunk_preamble
    !> Flat worker for the scalar and matrix date chunk writes.
    subroutine write_date_chunk_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_date), intent(in) :: flat(:) !! this chunk's flattened dates.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! this chunk's row count.
        logical :: do_write
        integer :: idx
        character(len=:), allocatable :: outname
        integer(c_int32_t), allocatable :: days(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_date), allocatable :: flat_c(:)

        call check_writer_open(writer)
        call temporal_chunk_preamble(writer, name, "date", asize, nrows, do_write, outname, idx, row_mask)
        if (.not. do_write) return
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        allocate(days(size(flat_c, kind=int64)))
        days = flat_c%raw()
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        if (size(flat_c, kind=int64) > 0_int64) &
            call parquet_append_date_column_chunk(writer%handle, trim(outname)//char(0), days, asize, valid_ptr)
    end subroutine write_date_chunk_flat
    !> Flat worker for the scalar and matrix time chunk writes.
    subroutine write_time_chunk_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_time), intent(in) :: flat(:) !! this chunk's flattened times.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! this chunk's row count.
        logical :: do_write
        integer :: idx, unit
        integer(c_int32_t) :: is_utc
        character(len=:), allocatable :: outname
        integer(c_int64_t), allocatable :: ns(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_time), allocatable :: flat_c(:)

        call check_writer_open(writer)
        call temporal_chunk_preamble(writer, name, "time", asize, nrows, do_write, outname, idx, row_mask)
        if (.not. do_write) return
        call resolve_temporal_write_unit(writer, idx, unit, is_utc)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        allocate(ns(size(flat_c, kind=int64)))
        ns = flat_c%raw()
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        if (size(flat_c, kind=int64) > 0_int64) &
            call parquet_append_time_column_chunk(writer%handle, trim(outname)//char(0), ns, asize, &
                int(unit, c_int32_t), valid_ptr)
    end subroutine write_time_chunk_flat
    !> Flat worker for the scalar and matrix timestamp chunk writes.
    subroutine write_timestamp_chunk_flat(writer, name, flat, asize, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_timestamp), intent(in) :: flat(:) !! this chunk's flattened instants.
        integer(int64), intent(in) :: asize !! per-row element count.
        integer(int64), intent(in) :: nrows !! this chunk's row count.
        logical :: do_write
        integer :: idx, unit
        integer(c_int32_t) :: is_utc
        character(len=:), allocatable :: outname
        integer(c_int64_t), allocatable :: vals(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, m
        logical, allocatable :: row_mask(:), elem_mask(:)
        type(parquet_timestamp), allocatable :: flat_c(:)

        call check_writer_open(writer)
        call temporal_chunk_preamble(writer, name, "timestamp", asize, nrows, do_write, outname, idx, row_mask)
        if (.not. do_write) return
        call resolve_temporal_write_unit(writer, idx, unit, is_utc)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        flat_c = pack(flat, elem_mask)
        m = size(flat_c, kind=int64)
        allocate(vals(m))
        do i = 1_int64, m
            if (flat_c(i)%is_null()) then
                vals(i) = 0_c_int64_t
            else
                vals(i) = flat_c(i)%to_unix(unit)
            end if
        end do
        call temporal_valid_ptr(writer, name, flat_c%is_null(), valid_buf, valid_ptr)
        if (m > 0_int64) call parquet_append_timestamp_column_chunk(writer%handle, trim(outname)//char(0), vals, asize, &
            int(unit, c_int32_t), is_utc, valid_ptr)
    end subroutine write_timestamp_chunk_flat
    module procedure parquet_write_date_column_chunk
        call write_date_chunk_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_date_column_chunk
    module procedure parquet_write_date_matrix_column_chunk
        call write_date_chunk_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_date_matrix_column_chunk
    module procedure parquet_write_time_column_chunk
        call write_time_chunk_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_time_column_chunk
    module procedure parquet_write_time_matrix_column_chunk
        call write_time_chunk_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_time_matrix_column_chunk
    module procedure parquet_write_timestamp_column_chunk
        call write_timestamp_chunk_flat(writer, name, values, 1_int64, size(values, kind=int64))
    end procedure parquet_write_timestamp_column_chunk
    module procedure parquet_write_timestamp_matrix_column_chunk
        call write_timestamp_chunk_flat(writer, name, reshape(values, [size(values, kind=int64)]), &
            size(values, 1, kind=int64), size(values, 2, kind=int64))
    end procedure parquet_write_timestamp_matrix_column_chunk

end submodule parquet_write_temporal
