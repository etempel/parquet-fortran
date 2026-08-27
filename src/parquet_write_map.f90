!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> `MAP` write specifics (whole-column and row-group-scoped): bodies of the module
!> procedures declared in parquet_core.f90's interface block, plus the private
!> preamble this file exists to hold.
!>
!> The mirror of parquet_read_map.f90, and the smallest of the three container
!> write paths -- because a map's entries ARE a two-field struct, so this reuses
!> Phase 4's STAGING REGISTRY wholesale instead of building a second one. The
!> sequence is: parquet_struct_begin(name, NENTRIES, 2), push the keys through
!> push_struct_field with the field name "key", push the values with "value", then
!> parquet_append_map_column (or its _chunk twin), which wraps the staged struct in
!> a MAP array using this column's offsets and per-row validity.
!>
!> Note what is staged over: the struct's "rows" are this map's ENTRIES, not its
!> rows, so `nentries` is what parquet_struct_begin and both pushes are told. That
!> is exactly what check_struct_staging wants to be given, so no new staging state
!> and no new validation exist on the C++ side at all.
!>
!> **The writer's concurrency guard is held across the WHOLE sequence**, by the
!> `writer_lock` the two module procedures at the bottom claim -- the same reason
!> the struct write holds it: without it two threads writing two container columns
!> to one writer would interleave their pushes.
!>
!> **A key is never null.** Arrow's MapType declares its key field non-nullable, so
!> a map has two null levels (the row, and each value) where a struct has 1 + M.
!> The keys are pushed with `c_null_ptr` for their validity buffer, and that is a
!> statement about the format rather than an optimisation.
submodule (parquet_core:parquet_write) parquet_write_map
    implicit none
contains

    !> The MAML `data_type` token a map column of this value kind must be declared as.
    !>
    !> The base form only: a temporal value's unit and UTC flag live in the same schema fields a
    !> scalar temporal column uses, exactly as they do for `list[timestamp]`, so the token compared
    !> against the schema never carries a bracket suffix of its own.
    subroutine map_type_token(kind, token)
        integer, intent(in) :: kind !! the values' PK_* kind.
        character(len=:), allocatable, intent(out) :: token !! the expected data_type token.
        character(len=:), allocatable :: kname
        select case (kind)
        case (PK_INT32);     token = "map[int32]"
        case (PK_INT64);     token = "map[int64]"
        case (PK_FLOAT32);   token = "map[float32]"
        case (PK_FLOAT64);   token = "map[float64]"
        case (PK_LOGICAL);   token = "map[boolean]"
        case (PK_STRING);    token = "map[string]"
        case (PK_DATE);      token = "map[date]"
        case (PK_TIME);      token = "map[time]"
        case (PK_TIMESTAMP); token = "map[timestamp]"
        case default
            ! Not reachable through the public API: %init refuses any other value kind, so a map
            ! column cannot exist with one. Kept so the message names what it saw.
            call parquet_kind_name(kind, kname) ! GCOVR_EXCL_LINE
            token = "map["//kname//"]" ! GCOVR_EXCL_LINE
        end select
    end subroutine map_type_token

    !> Shared bookkeeping for both map write specifics: schema checks (defined, enabled, col_size,
    !> exact data_type match), and output-name resolution -- the map column's counterpart to
    !> list_write_preamble. Returns do_write=.false. (caller returns without writing) for a
    !> defined-but-disabled column; `idx` is the schema column index (0 for a schema-less writer),
    !> for the temporal value's unit resolution.
    subroutine map_write_preamble(writer, name, kind, context, do_write, outname, idx)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer, intent(in) :: kind !! the values' PK_* kind.
        character(len=*), intent(in) :: context !! "parquet_write_column" or "..._chunk".
        logical, intent(out) :: do_write !! .true. if the column should actually be written.
        character(len=:), allocatable, intent(out) :: outname !! resolved output (file) name.
        integer, intent(out) :: idx !! schema column index, or 0 for a schema-less writer.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: expected

        do_write = .false.
        idx = 0
        call map_type_token(kind, expected)
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
                    trim(writer%all_columns(idx)%data_type) // ", and a map column's value type must " // &
                    "match exactly -- there is no widening between map value kinds" // ctx
            end if
        end if
        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_resolve_output_name(writer, name, outname)
        do_write = .true.
    end subroutine map_write_preamble

    !> The shared body of both map write specifics. `chunked` selects the row-group-scoped
    !> counterpart of every step that differs: which mask helper supplies the row mask, which row
    !> count is checked, which mark-written call is made, and which finisher is called.
    !>
    !> One worker rather than two because everything between those steps -- the preamble, the
    !> masked rebuild, both null levels, the protected/qc checks and the two staged pushes -- is
    !> identical, and a second copy of it would be a second place for the null levels to be got
    !> wrong.
    subroutine write_map_common(writer, name, values, chunked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_map_column), intent(in), target :: values !! the column to write.
        logical, intent(in) :: chunked !! .true. for the row-group-scoped form.
        character(len=:), allocatable :: outname, ctx, context
        integer :: idx, kind, unit
        integer(c_int32_t) :: is_utc
        integer(int64) :: nrows, nentries, i
        logical :: do_write, masked, any_row_null, any_value_null, protected
        logical, allocatable :: row_mask(:), row_valid(:), val_valid(:)
        integer(c_int8_t), allocatable, target :: row_buf(:), val_buf(:)
        type(c_ptr) :: row_ptr, val_ptr
        integer(int64), pointer :: offs(:)
        type(parquet_column), pointer :: kcol, vcol
        type(parquet_map_column), target :: values_c !! row-masked rebuild; unused on the fast path.
        type(parquet_map_column), pointer :: v !! the column actually written.

        context = "parquet_write_column"
        if (chunked) context = "parquet_write_column_chunk"

        if (.not. values%is_init()) then
            call writer_context_suffix(writer, ctx)
            error stop context // ": the map column passed for '" // trim(name) // &
                "' has not been initialized; call %init(<value_kind>) first" // ctx
        end if
        kind = values%element_kind()
        call map_write_preamble(writer, name, kind, context, do_write, outname, idx)
        if (.not. do_write) return

        nrows = values%size()
        if (chunked) then
            call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        else
            call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        end if

        if (masked) then
            ! %gather_rows rebuilds the offsets, the row bitmap and BOTH entry columns (each by one
            ! parquet_column%gather over the same flattened entry indices, which is what keeps the
            ! keys and the values index-aligned by construction).
            call values%deep_copy(values_c)
            call values_c%gather_rows(pack([(i, i = 1_int64, nrows)], row_mask))
            v => values_c
            nrows = count(row_mask, kind=int64)
        else
            ! Nothing to remove, so the caller's own column is written as it stands.
            v => values
        end if

        allocate(row_valid(max(nrows, 1_int64)))
        call parquet_map_column_row_validity(v, row_valid, any_row_null)
        call parquet_map_column_offsets(v, offs)
        nentries = offs(nrows + 1_int64)
        call parquet_map_column_keys(v, kcol)
        call parquet_map_column_values(v, vcol)

        ! A protected map column may hold neither a null ROW nor a null VALUE: at the Parquet level
        ! a null value IS a Null in this map's value leaf column, so the weaker reading would
        ! declare a non-nullable field for a column that can contain nulls -- exactly the invariant
        ! build_field's safety comment forbids. The row half is parquet_check_protected's ordinary
        ! job; the value half is checked below so that its message names which level failed.
        call parquet_check_protected(writer, name, row_valid(1_int64:nrows), protected)
        call parquet_check_qc_miss(writer, name, row_valid(1_int64:nrows))

        call struct_field_validity(vcol, nentries, kind, val_valid, any_value_null)
        if (protected .and. any_value_null) then
            error stop context // ": column '" // trim(name) // &
                "' is protected (extra: protected_cols:) and cannot contain Null values" // &
                " -- one of its map values is a Null"
        end if

        if (any_row_null) then
            call parquet_make_valid_buf_write(row_valid(1_int64:nrows), row_buf, row_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=row_buf, valid_ptr=row_ptr)
        end if
        if (any_value_null) then
            call parquet_make_valid_buf_write(val_valid(1_int64:nentries), val_buf, val_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=val_buf, valid_ptr=val_ptr)
        end if

        if (chunked) then
            call parquet_chunk_mark_written_if_first(writer, name)
        else
            call parquet_check_row_count(writer, name, nrows)
            call parquet_mark_column_written(writer, name)
        end if

        call resolve_temporal_write_unit(writer, idx, unit, is_utc)
        ! Staging: open over the ENTRIES, push the keys and then the values, then finish. The
        ! writer's concurrency guard is held by the caller across all of it -- see the file header.
        call parquet_struct_begin(writer%handle, trim(outname)//char(0), nentries, 2_c_int32_t)
        call push_struct_field(writer, "key", kcol, PK_STRING, nentries, c_null_ptr)
        call push_struct_field(writer, "value", vcol, kind, nentries, val_ptr, unit, is_utc)
        if (chunked) then
            call parquet_append_map_column_chunk(writer%handle, trim(outname)//char(0), nrows, nentries, &
                offs, row_ptr)
        else
            call parquet_append_map_column(writer%handle, trim(outname)//char(0), nrows, nentries, &
                offs, row_ptr)
        end if
    end subroutine write_map_common

    module procedure parquet_write_map_column
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_map_common(writer, name, values, .false.)
    end procedure parquet_write_map_column

    ! ORDER MATTERS: this CALLS parquet_write_map_column above, and NAG 7.2 binds a separate module
    ! procedure's name to an implicit external at the first call site -- so implementing it below
    ! its own caller is rejected ("PARQUET_WRITE_MAP_COLUMN is not the interface of a separate
    ! module procedure"), naming the implementing statement rather than the call that caused it.
    ! gfortran, ifx and flang accept either order, so nothing else in the fleet enforces this.
    ! See CLAUDE.md, "A separate module procedure must be IMPLEMENTED before it is CALLED".
    module procedure parquet_write_empty_map_column
        type(parquet_map_column) :: empty_map
        integer :: kind
        select case (trim(value_base))
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
            ! Unreachable: parquet_parse_map_type reports a well-formed token only for these nine,
            ! and the caller has already checked that. An error stop rather than a sentinel kind, so
            ! a future tenth value token fails where it is missed rather than inside %init.
            error stop "parquet_close_writer: unsupported map value type '"//trim(value_base)//"'" ! GCOVR_EXCL_LINE
        end select
        call empty_map%init(kind)
        call parquet_write_map_column(writer, name, empty_map)
    end procedure parquet_write_empty_map_column

    module procedure parquet_write_map_column_chunk
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        call write_map_common(writer, name, values, .true.)
    end procedure parquet_write_map_column_chunk

end submodule parquet_write_map
