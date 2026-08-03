!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Writing a `parquet_table` back out to a parquet file.
!!
!! Deliberately thin. The schema decides which columns are written, in what order, and under
!! what output names; the existing writer decides everything else -- type agreement, QC, row
!! groups, compression -- so this file adds no validation of its own beyond "the schema names a
!! column the table does not have". Reusing `parquet_open_writer`/`parquet_write_column` rather
!! than reimplementing them is what keeps a table write and a hand-written write path identical
!! in behaviour.
submodule (parquet_tables) parquet_tables_write
    implicit none
    !
contains
    !
    module procedure parquet_write_table
        type(parquet_writer) :: writer
        character(len=:), allocatable :: fname, sfx
        integer :: i, nfields, idx
        !
        call table_check_open(table, "parquet_write_table")
        ! A schema built in code with %init/%add_field only has MAML *text* until
        ! parquet_parse_maml populates %cinfo -- and %get_num_fields on an unpopulated %cinfo
        ! reads uninitialized state, which turns the loop below into a runaway allocation and an
        ! OOM kill rather than any kind of diagnosable failure. There is no reason to make the
        ! caller say so themselves, though: a schema that has been built but not parsed is parsed
        ! here. It is a visible side effect (the caller's schema stays parsed afterwards, which is
        ! what they wanted anyway), which is why `schema` is intent(inout).
        !
        ! A schema that was never built at all is a different mistake and still an error: parsing
        ! empty MAML text would report something about the text rather than about the call.
        if (.not. schema%is_parsed()) then
            if (.not. schema%is_init()) then
                error stop EP // "parquet_write_table: this schema has not been built; call " // &
                    "schema%init/%add_field (or load a MAML file) before writing with it"
            end if
            call parquet_parse_maml(schema)
        end if
        nfields = schema%get_num_fields()
        !
        call parquet_open_writer(writer, trim(filename), schema)
        if (present(row_mask)) call parquet_write_row_mask(writer, row_mask)
        do i = 1, nfields
            call schema%get_field_name(i, fname)
            ! A schema may deliberately disable a field (set_column_unavailable); skip those
            ! rather than demanding the table carry a column nobody is going to write.
            if (.not. schema%is_column_set(fname)) cycle
            ! The lookup key is the INTERNAL name. A col_map: rename lives in the schema and is
            ! applied by the writer on the way out, so nothing here ever sees the output name.
            idx = table_find(table, fname)
            if (idx == 0) then
                call table_context_suffix(table%cache, fname, sfx)
                error stop EP // "parquet_write_table: the schema declares a column the table " // &
                    "does not have" // sfx
            end if
            if (.not. table%cache%cols(idx)%supported) then
                call table_context_suffix(table%cache, fname, sfx)
                error stop EP // "parquet_write_table: the schema declares a column that holds " // &
                    "no values" // sfx
            end if
            ! Writing a column the caller never read is a first touch like any other: the schema
            ! naming it IS the request to read it. Nothing has to be pre-materialized to write.
            call table_touch(table%cache, table_scope_of(table), idx, "parquet_write_table")
            call write_one_column(writer, table, idx, fname)
        end do
        call parquet_close_writer(writer)
    end procedure parquet_write_table
    !
    !> Writes slot `idx` through the `parquet_write_column` specific matching its stored kind.
    !!
    !! Validity is passed as `is_valid=` for every kind that accepts one; the temporal kinds take
    !! no mask because their null state lives inside each element, and the string kind carries
    !! its own validity inside the `parquet_string_column`.
    subroutine write_one_column(writer, table, idx, name)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        type(parquet_table), intent(in) :: table      !! the table being written.
        integer, intent(in) :: idx                    !! slot to write.
        character(len=*), intent(in) :: name          !! the column's internal name.
        !
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_bool(:), p_boolv(:,:)
        type(parquet_date), pointer :: p_dt(:), p_dtv(:,:)
        type(parquet_time), pointer :: p_tm(:), p_tmv(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        type(parquet_string_column), pointer :: p_str
        logical, allocatable :: valid(:), validv(:,:)
        character(len=:), allocatable :: sfx, kname, chr(:,:)
        !
        associate (col => table%cache%cols(idx)%values)
            select case (col%kindof())
            case (PK_INT32)
                call col%data_ptr(p_i32)
                call scalar_validity(table, idx, valid)
                call parquet_write_column(writer, name, p_i32, is_valid=valid)
            case (PK_INT64)
                call col%data_ptr(p_i64)
                call scalar_validity(table, idx, valid)
                call parquet_write_column(writer, name, p_i64, is_valid=valid)
            case (PK_FLOAT32)
                call col%data_ptr(p_f32)
                call scalar_validity(table, idx, valid)
                call parquet_write_column(writer, name, p_f32, is_valid=valid)
            case (PK_FLOAT64)
                call col%data_ptr(p_f64)
                call scalar_validity(table, idx, valid)
                call parquet_write_column(writer, name, p_f64, is_valid=valid)
            case (PK_LOGICAL)
                call col%data_ptr(p_bool)
                call scalar_validity(table, idx, valid)
                call parquet_write_column(writer, name, p_bool, is_valid=valid)
            case (PK_STRING)
                call col%string_column(p_str)
                call parquet_write_column(writer, name, p_str)
            case (PK_DATE)
                call col%data_ptr(p_dt)
                call parquet_write_column(writer, name, p_dt)
            case (PK_TIME)
                call col%data_ptr(p_tm)
                call parquet_write_column(writer, name, p_tm)
            case (PK_TIMESTAMP)
                call col%data_ptr(p_ts)
                call parquet_write_column(writer, name, p_ts)
            case (PK_INT32_VEC)
                call col%data_ptr(p_i32v)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, p_i32v, is_valid=validv)
            case (PK_INT64_VEC)
                call col%data_ptr(p_i64v)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, p_i64v, is_valid=validv)
            case (PK_FLOAT32_VEC)
                call col%data_ptr(p_f32v)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, p_f32v, is_valid=validv)
            case (PK_FLOAT64_VEC)
                call col%data_ptr(p_f64v)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, p_f64v, is_valid=validv)
            case (PK_LOGICAL_VEC)
                call col%data_ptr(p_boolv)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, p_boolv, is_valid=validv)
            case (PK_STRING_VEC)
                ! No compact rank-2 string write path exists, so this goes through the
                ! fixed-width form -- the same asymmetry the read side has.
                call table%get(name, chr)
                call vector_validity(table, idx, validv)
                call parquet_write_column(writer, name, chr, is_valid=validv)
            case (PK_DATE_VEC)
                call col%data_ptr(p_dtv)
                call parquet_write_column(writer, name, p_dtv)
            case (PK_TIME_VEC)
                call col%data_ptr(p_tmv)
                call parquet_write_column(writer, name, p_tmv)
            case (PK_TIMESTAMP_VEC)
                call col%data_ptr(p_tsv)
                call parquet_write_column(writer, name, p_tsv)
            case default
                ! Not reachable through the public API: by the time write_one_column runs, the
                ! caller (parquet_write_table) has already rejected an unsupported slot and
                ! table_touch has resolved/materialized this one, so col%kindof() is always one
                ! of the 18 kinds handled above.
                call table_context_suffix(table%cache, name, sfx) ! GCOVR_EXCL_LINE
                call parquet_kind_name(col%kindof(), kname) ! GCOVR_EXCL_LINE
                error stop EP // "parquet_write_table: column kind " // kname // & ! GCOVR_EXCL_LINE
                    " cannot be written" // sfx ! GCOVR_EXCL_LINE
            end select
        end associate
    end subroutine write_one_column
    !
    !> Builds a per-row validity mask for a scalar column, or leaves `valid` UNALLOCATED when the
    !! column holds no nulls.
    !!
    !! Every call site passes the result straight on as `is_valid=`, and an unallocated allocatable
    !! actual makes an `optional` dummy absent (F2018 15.5.2.12) -- so a null-free column reaches
    !! `parquet_write_column` with no mask argument at all, which is exactly what a hand-written
    !! write of the same data would do. That matters more than it looks: passing a uniformly-`.true.`
    !! mask instead costs an nrows-long allocation here AND makes the writer build an Arrow null
    !! bitmap it did not need, which together were measured as the whole of `parquet_write_table`'s
    !! ~2.5x gap against a hand-written per-column loop.
    subroutine scalar_validity(table, idx, valid)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:)       !! .true. where the row is not null; see above.
        !
        call table%cache%cols(idx)%values%row_validity(valid)
    end subroutine scalar_validity
    !
    !> Builds a per-element validity mask for a vector column, shaped (width, nrows) to match
    !! the stored orientation -- or leaves `valid` unallocated when there are no nulls, exactly as
    !! `scalar_validity` does and for the same reason.
    !!
    !! parquet_column's validity is ROW-granular even for a vector kind -- is_null(i) takes a row
    !! index bounded by nrows, not a flat element index -- so a null row comes back as a whole
    !! row of .false. here. That mirrors the read side, which widens any per-element null to the
    !! whole row for the same reason.
    subroutine vector_validity(table, idx, valid)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:,:)     !! .true. where the element is not null.
        logical, allocatable :: rows(:)
        integer(int64) :: i, n
        integer :: wdt
        !
        call table%cache%cols(idx)%values%row_validity(rows)
        if (.not. allocated(rows)) return
        n = table%cache%cols(idx)%values%length()
        wdt = table%cache%cols(idx)%values%colwidth()
        allocate(valid(wdt, n))
        do i = 1, n
            valid(:, i) = rows(i)
        end do
    end subroutine vector_validity
    !
end submodule parquet_tables_write
