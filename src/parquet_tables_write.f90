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
        type(parquet_schema) :: carried
        character(len=:), allocatable :: fname, sfx
        integer :: i, nfields, idx
        logical :: want_metadata
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
        want_metadata = present(metadata_keys)
        if (present(copy_metadata)) then
            if (present(metadata_keys) .and. copy_metadata) then
                error stop EP // "parquet_write_table: copy_metadata= and metadata_keys= cannot " // &
                    "both be given; copy_metadata=.true. carries every key, metadata_keys= only " // &
                    "the listed ones"
            end if
            want_metadata = want_metadata .or. copy_metadata
        end if
        ! The carried metadata goes onto a COPY of the schema, never the caller's own: writing a
        ! second table with the same schema afterwards would otherwise inherit the first table's
        ! source-file metadata, silently and permanently.
        if (want_metadata) then
            carried = schema
            call carry_source_metadata(table, carried, metadata_keys)
            call parquet_open_writer(writer, trim(filename), carried)
        else
            call parquet_open_writer(writer, trim(filename), schema)
        end if
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
    !> Copies the table's source-file metadata onto `sch`, which is already a private copy.
    !!
    !! Three rules, all deliberate:
    !!
    !! * **The schema wins a collision.** A key the schema declares itself is the caller's explicit
    !!   statement about the output; the carried one is inherited from wherever the input came
    !!   from, so it is skipped rather than overwriting.
    !! * **A requested key that does not exist is an error**, not a silent omission -- naming a key
    !!   is a claim that it is there, and quietly writing a file without it is the failure mode
    !!   this is supposed to prevent.
    !! * **It works after a detach**, because the metadata was snapshotted at open. That is the
    !!   whole point: the natural shape is read, mutate rows, write, and the reader is gone by then.
    subroutine carry_source_metadata(table, sch, keys)
        type(parquet_table), intent(in) :: table                   !! the table being written.
        type(parquet_schema), intent(inout) :: sch                 !! private schema copy to add to.
        character(len=*), intent(in), optional :: keys(:)          !! only these keys, if given.
        character(len=:), allocatable :: sfx
        integer :: i, k
        logical :: wanted
        !
        if (.not. allocated(table%cache%meta_keys)) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // "parquet_write_table: this table was not opened from a file, so it " // &
                "has no source metadata to copy" // sfx
        end if
        ! Every requested key is checked BEFORE anything is added, so a typo fails with the output
        ! file not yet opened rather than half-written.
        if (present(keys)) then
            do k = 1, size(keys)
                if (.not. source_has_key(table, trim(keys(k)))) then
                    call table_context_suffix(table%cache, "", sfx)
                    error stop EP // "parquet_write_table: metadata_keys names '" // trim(keys(k)) // &
                        "', which this table's source file does not have" // sfx
                end if
            end do
        end if
        do i = 1, size(table%cache%meta_keys)
            wanted = .true.
            if (present(keys)) then
                wanted = .false.
                do k = 1, size(keys)
                    if (trim(keys(k)) == trim(table%cache%meta_keys(i))) wanted = .true.
                end do
            end if
            if (.not. wanted) cycle
            if (schema_declares_key(sch, trim(table%cache%meta_keys(i)))) cycle
            call sch%add_metadata(trim(table%cache%meta_keys(i)), trim(table%cache%meta_values(i)))
        end do
    end subroutine carry_source_metadata
    !
    !> .true. when the table's source file carried `key`.
    logical function source_has_key(table, key) result(has)
        type(parquet_table), intent(in) :: table !! the table being written.
        character(len=*), intent(in) :: key      !! the key to look for.
        integer :: i
        !
        has = .false.
        do i = 1, size(table%cache%meta_keys)
            if (trim(table%cache%meta_keys(i)) == key) has = .true.
        end do
    end function source_has_key
    !
    !> .true. when the schema already declares `key` itself, so a carried entry must not replace it.
    logical function schema_declares_key(sch, key) result(has)
        type(parquet_schema), intent(in) :: sch !! the output schema.
        character(len=*), intent(in) :: key     !! the key to look for.
        integer :: i
        !
        has = .false.
        if (.not. allocated(sch%metadata%items)) return
        do i = 1, size(sch%metadata%items)
            if (trim(sch%metadata%items(i)%key) == key) has = .true.
        end do
    end function schema_declares_key
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
    !! A pass-through, and that is the point: `parquet_column` stores validity per element and
    !! `parquet_write_column` accepts it per element, so the writer records exactly the nulls the
    !! table holds. It used to read the row bit and broadcast it back across the row, which turned
    !! one null element into a null row in the output file.
    subroutine vector_validity(table, idx, valid)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:,:)     !! .true. where the element is not null.
        !
        call table%cache%cols(idx)%values%element_validity(valid)
    end subroutine vector_validity
    !
end submodule parquet_tables_write
