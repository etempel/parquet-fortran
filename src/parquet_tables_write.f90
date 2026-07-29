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
        ! reads uninitialized state, which turns the loop below into a runaway allocation and
        ! an OOM kill rather than any kind of diagnosable failure. Catch it here instead.
        if (.not. schema%is_parsed()) then
            error stop EP // "parquet_write_table: this schema has not been parsed; call " // &
                "parquet_parse_maml(schema) after building it with %init/%add_field"
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
                call table_context_suffix(table, fname, sfx)
                error stop EP // "parquet_write_table: the schema declares a column the table " // &
                    "does not have" // sfx
            end if
            if (.not. table%cache%cols(idx)%supported .or. &
                    table%cache%cols(idx)%residency == RES_EMPTY) then
                call table_context_suffix(table, fname, sfx)
                error stop EP // "parquet_write_table: the schema declares a column that holds " // &
                    "no values" // sfx
            end if
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
                call table_context_suffix(table, name, sfx)
                call parquet_kind_name(col%kindof(), kname)
                error stop EP // "parquet_write_table: column kind " // kname // &
                    " cannot be written" // sfx
            end select
        end associate
    end subroutine write_one_column
    !
    !> Builds a per-row validity mask for a scalar column from its own null state.
    subroutine scalar_validity(table, idx, valid)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:)       !! .true. where the row is not null.
        integer(int64) :: i, n
        !
        n = table%cache%cols(idx)%values%length()
        allocate(valid(n))
        do i = 1, n
            valid(i) = .not. table%cache%cols(idx)%values%is_null(i)
        end do
    end subroutine scalar_validity
    !
    !> Builds a per-element validity mask for a vector column, shaped (width, nrows) to match
    !! the stored orientation.
    !!
    !! parquet_column's validity is ROW-granular even for a vector kind -- is_null(i) takes a row
    !! index bounded by nrows, not a flat element index -- so a null row comes back as a whole
    !! row of .false. here. That mirrors the read side, which widens any per-element null to the
    !! whole row for the same reason.
    subroutine vector_validity(table, idx, valid)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:,:)     !! .true. where the element is not null.
        integer(int64) :: i, n
        integer :: wdt
        !
        n = table%cache%cols(idx)%values%length()
        wdt = table%cache%cols(idx)%values%colwidth()
        allocate(valid(wdt, n))
        do i = 1, n
            valid(:, i) = .not. table%cache%cols(idx)%values%is_null(i)
        end do
    end subroutine vector_validity
    !
end submodule parquet_tables_write
