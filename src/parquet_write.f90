!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Writer lifecycle (open/close/new_row_group/finish_row_group) and the
!> type-generic shared helpers every write-specifics child (parquet_write_numeric/
!> _string/_temporal) reaches by host association: schema/column-info lookups,
!> row-count and row-mask bookkeeping, output-name resolution, qc: integer-count
!> text formatting (shared by numeric and string qc: reports), and the
!> write_maml=.true. sidecar .maml.
submodule (parquet) parquet_write
    implicit none
contains

    !> Every parquet_write_column variant calls this first: writer%handle is
    !> c_null_ptr until parquet_open_writer sets it, and every C++ entry point
    !> dereferences the handle immediately (see ConcurrencyGuard in
    !> parquet_wrapper.cpp) with no null check of its own -- calling in with an
    !> unopened writer previously crashed with an unhelpful SIGSEGV instead of
    !> a clean, diagnosable error.
    subroutine check_writer_open(writer)
        type(parquet_writer), intent(in) :: writer !! writer to check.
        if (.not. c_associated(writer%handle)) then
            error stop "parquet_write_column: writer has not been opened (call parquet_open_writer first)"
        end if
    end subroutine check_writer_open
    !> The name actually written to the parquet file/VOTable header for
    !> `col`: its output_name if set, else its (internal) name. Falling back
    !> to name defensively handles a parquet_column_type built without going
    !> through parquet_read_maml (output_name left unallocated).
    subroutine parquet_column_output_name(col, output_name)
        type(parquet_column_type), intent(in) :: col !! column whose output name is wanted.
        character(len=:), allocatable, intent(out) :: output_name !! col%output_name if set, else col%name.

        output_name = col%name
        if (allocated(col%output_name)) then
            if (len_trim(col%output_name) > 0) output_name = col%output_name
        end if
    end subroutine parquet_column_output_name
    !> Resolves the caller-facing (internal) column `name` used in a
    !> parquet_write_column call to the name that should actually be passed
    !> to the C++ append_* calls -- its col_map:-renamed output_name, or
    !> `name` itself if there's no schema (schema-less writer) or no match.
    subroutine parquet_resolve_output_name(writer, name, output_name)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! internal (caller-facing) column name.
        character(len=:), allocatable, intent(out) :: output_name !! name to actually pass to the C++ append_* calls.
        integer :: idx

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx > 0) then
            call parquet_column_output_name(writer%enabled_columns(idx), output_name)
        else
            output_name = name
        end if
    end subroutine parquet_resolve_output_name
    module procedure parquet_get_enabled_column_index
        integer :: i

        parquet_get_enabled_column_index = 0
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_enabled_column_index = i
                return
            end if
        end do
    end procedure parquet_get_enabled_column_index
    module procedure parquet_get_defined_column_index
        integer :: i

        parquet_get_defined_column_index = 0

        do i = 1, size(writer%all_columns)
            if (trim(writer%all_columns(i)%name) == trim(name)) then
                parquet_get_defined_column_index = i
                return
            end if
        end do
    end procedure parquet_get_defined_column_index
    module procedure parquet_is_type_compatible
        character(len=:), allocatable :: schema_type, write_type

        schema_type = trim(actual_type)
        write_type = trim(expected_type)

        select case (schema_type)
        case ("boolean")
            parquet_is_type_compatible = write_type == "boolean" .or. write_type == "bool8"
        case ("float32", "float64")
            parquet_is_type_compatible = write_type == "float32" .or. write_type == "float64" .or. &
                                          write_type == "int32" .or. write_type == "int64"
        case ("int32", "int64")
            parquet_is_type_compatible = write_type == "int32" .or. write_type == "int64" .or. &
                                          write_type == "float32" .or. write_type == "float64"
        case default
            parquet_is_type_compatible = write_type == schema_type
        end select
    end procedure parquet_is_type_compatible
    !> The schema-declared data_type for `name` on a schema-enforced writer,
    !> or "" for a schema-less writer or an undefined column. Used by the
    !> parquet_append_as_schema_* family to decide whether a write call's own
    !> values need widening to match the schema's declared type.
    subroutine parquet_get_schema_type(writer, name, schema_type)
        type(parquet_writer), intent(in) :: writer !! writer to check.
        character(len=*), intent(in) :: name !! column name.
        character(len=:), allocatable, intent(out) :: schema_type !! schema-declared data_type, or "".
        integer :: idx

        schema_type = ""
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        schema_type = trim(writer%all_columns(idx)%data_type)
    end subroutine parquet_get_schema_type
    !> "" for a schema-less writer with no output filename set either;
    !> otherwise " (file: X)", " (maml: Y)", or " (file: X, maml: Y)"
    !> depending on which of writer%filename/writer%maml_name are set
    !> (writer%filename is set by parquet_open_writer as soon as the writer
    !> is created, so it is virtually always present here; writer%maml_name
    !> is unset only for a schema assembled by hand without going through
    !> schema%init/parquet_schema(...), which always set it to
    !> "internal:<table>"). Appended to parquet_write_column's
    !> schema-mismatch and completeness errors so they're diagnosable
    !> without needing to know which parquet_open_writer call produced them.
    subroutine writer_context_suffix(writer, suffix)
        type(parquet_writer), intent(in) :: writer !! writer whose filename/maml_name is reported.
        character(len=:), allocatable, intent(out) :: suffix !! " (file: X, maml: Y)"-style suffix, or "".
        character(len=:), allocatable :: parts

        parts = ""
        if (allocated(writer%filename)) then
            if (len_trim(writer%filename) > 0) parts = "file: " // trim(writer%filename)
        end if
        if (allocated(writer%maml_name)) then
            if (len_trim(writer%maml_name) > 0) then
                if (len(parts) > 0) then
                    parts = parts // ", maml: " // trim(writer%maml_name)
                else
                    parts = "maml: " // trim(writer%maml_name) ! GCOVR_EXCL_LINE
                end if
            end if
        end if

        suffix = ""
        if (len(parts) > 0) suffix = " (" // parts // ")"
    end subroutine writer_context_suffix
    module procedure parquet_assert_column_type
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (.not. writer%is_schema_enforced) return

        idx = parquet_get_defined_column_index(writer, name)

        if (.not. parquet_is_type_compatible(writer%all_columns(idx)%data_type, expected_type)) then
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column: type mismatch for column " // trim(name) // &
                                  " (expected " // trim(expected_type) // ", got " // &
                                  trim(writer%all_columns(idx)%data_type) // ")" // &
                                  ctx
        end if
    end procedure parquet_assert_column_type
    !> Records that `name` has now been written at least once, growing
    !> writer%written_names (schema-less writer only) so a later repeat
    !> write to the same name can be detected.
    !> Plain contained subroutine (not a module procedure) for the same
    !> reason as parquet_check_read_row_count above -- its body already
    !> lived in this file, the parquet_write parent, not a descendant
    !> submodule.
    subroutine parquet_mark_column_written(writer, name)
        type(parquet_writer), intent(inout) :: writer !! open (schema-less) writer being written to.
        character(len=*), intent(in) :: name !! column name just written.
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (.not. allocated(writer%enabled_columns)) then
            call parquet_check_and_mark_written_name(writer, name)
            return
        end if

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx == 0) return

        if (writer%write_counts(idx) > 0) then
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column: column written more than once: " // trim(name) // ctx
        end if
        writer%write_counts(idx) = writer%write_counts(idx) + 1
    end subroutine parquet_mark_column_written
    !> Schema-less writer (no cinfo, so enabled_columns/write_counts above
    !> don't exist and can't track this): checks `name` against every name
    !> already written by this writer, error stopping on a repeat, then
    !> records `name` as written. Grows written_names by one each call --
    !> fine here since it only holds as many entries as columns actually
    !> written (typically a handful to a few dozen), unlike a per-row buffer.
    subroutine parquet_check_and_mark_written_name(writer, name)
        type(parquet_writer), intent(inout) :: writer !! schema-less writer whose written_names gains one entry.
        character(len=*), intent(in) :: name !! column name just written.
        character(len=256), allocatable :: tmp(:)
        integer :: n, i
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (allocated(writer%written_names)) then
            do i = 1, size(writer%written_names)
                if (trim(writer%written_names(i)) == trim(name)) then
                    call writer_context_suffix(writer, ctx)
                    error stop "parquet_write_column: column written more than once: " // trim(name) // &
                        ctx
                end if
            end do
            n = size(writer%written_names)
            allocate(tmp(n + 1))
            tmp(1:n) = writer%written_names
            tmp(n + 1) = trim(name)
            call move_alloc(tmp, writer%written_names)
        else
            allocate(writer%written_names(1))
            writer%written_names(1) = trim(name)
        end if
    end subroutine parquet_check_and_mark_written_name
    module procedure parquet_is_column_enabled
        parquet_is_column_enabled = .true.
        if (.not. allocated(writer%enabled_columns)) return

        parquet_is_column_enabled = parquet_get_enabled_column_index(writer, name) > 0
    end procedure parquet_is_column_enabled
    module procedure parquet_get_column_col_size
        integer :: i

        parquet_get_column_col_size = 1
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                if (writer%enabled_columns(i)%col_size == parquet_size_auto) then
                    error stop "parquet_write_column: col_size for column '" // trim(name) // "' is still " // &
                        "'auto' -- call schema%set_col_size before parquet_open_writer, or use the matrix " // &
                        "write form to resolve it automatically from the data"
                end if
                parquet_get_column_col_size = max(1, writer%enabled_columns(i)%col_size)
                return
            end if
        end do
    end procedure parquet_get_column_col_size
    !> Resolves writer%all_columns(idx)/enabled_columns(idx)'s col_size against a matrix-form
    !> write call's own structural asize (size(values,1)): if col_size is still "auto" (a MAML
    !> col_size: auto not yet resolved by schema%set_col_size), this is the write that resolves
    !> it -- asize becomes the column's permanent col_size for the rest of this writer's lifetime,
    !> kept in sync across all_columns/enabled_columns and the C++-side column_metadata (so
    !> parquet_writer_get_chunk_size and a write_maml=.true. sidecar both see the resolved value).
    !> Otherwise (already resolved, from the MAML/set_col_size, or a previous write to the same
    !> column), asize must match exactly -- error stops (using `context`, e.g. "parquet_write_column"
    !> or "parquet_write_column_chunk", and optional pre-formatted `ctx` suffix, to match each
    !> call site's own existing message) on any mismatch.
    subroutine parquet_resolve_or_check_col_size(writer, name, idx, asize, context, ctx)
        type(parquet_writer), intent(inout) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! column being written.
        integer, intent(in) :: idx !! writer%all_columns index for this column.
        integer(int64), intent(in) :: asize !! this write call's own structural col_size.
        character(len=*), intent(in) :: context !! error message prefix ("parquet_write_column"/"parquet_write_column_chunk").
        character(len=*), intent(in), optional :: ctx !! optional writer_context_suffix text appended to the error.
        integer :: k
        character(len=:), allocatable :: outname

        if (writer%all_columns(idx)%col_size == parquet_size_auto) then
            writer%all_columns(idx)%col_size = int(asize)
            k = parquet_get_enabled_column_index(writer, name)
            if (k > 0) writer%enabled_columns(k)%col_size = int(asize)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_update_column_metadata_size(writer%handle, trim(outname)//char(0), &
                int(asize, kind=c_long_long), int(writer%all_columns(idx)%array_size, kind=c_long_long))
        else if (writer%all_columns(idx)%col_size /= asize) then
            if (present(ctx)) then
                error stop trim(context) // ": array size mismatch for column " // trim(name) // ctx
            else
                error stop trim(context) // ": array size mismatch for column " // trim(name)
            end if
        end if
    end subroutine parquet_resolve_or_check_col_size
    !> String-only counterpart of parquet_resolve_or_check_col_size, for array_size (maximum
    !> string length): if array_size is still "auto", this write resolves it to `item_len` (the
    !> caller's own Fortran-declared character length for this call, i.e. len(values(1)) or
    !> len(values(1,1)) -- a structural property of the call, not derived from actual string
    !> content, mirroring col_size's own shape-derived resolution). Otherwise a no-op: the
    !> existing max_item_len/array_size ceiling check at each call site runs unchanged afterward.
    subroutine parquet_resolve_or_check_array_size(writer, name, idx, item_len)
        type(parquet_writer), intent(inout) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! string column being written.
        integer, intent(in) :: idx !! writer%all_columns index for this column.
        integer, intent(in) :: item_len !! this write call's own declared character length.
        integer :: k
        character(len=:), allocatable :: outname

        if (writer%all_columns(idx)%array_size == parquet_size_auto) then
            writer%all_columns(idx)%array_size = item_len
            k = parquet_get_enabled_column_index(writer, name)
            if (k > 0) writer%enabled_columns(k)%array_size = item_len
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_update_column_metadata_size(writer%handle, trim(outname)//char(0), &
                int(writer%all_columns(idx)%col_size, kind=c_long_long), int(item_len, kind=c_long_long))
        end if
    end subroutine parquet_resolve_or_check_array_size
    !> Every column in a file must have the same number of rows (Arrow/Parquet
    !> requirement). Called by every parquet_write_column variant with that
    !> call's own row count: the first call for a given writer fixes the
    !> expected row count, every later call must match it or error stop.
    !> Plain contained subroutine (not a module procedure) for the same
    !> reason as parquet_check_read_row_count above -- its body already
    !> lived in this file, the parquet_write parent, not a descendant
    !> submodule.
    subroutine parquet_check_row_count(writer, name, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer whose expected_nrows this call checks/sets.
        character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
        integer(c_long_long), intent(in) :: nrows !! row count of this write call's own values.
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (writer%expected_nrows < 0) then
            writer%expected_nrows = nrows
            return
        end if

        if (writer%expected_nrows /= nrows) then
            write(expected_str, '(i0)') writer%expected_nrows
            write(got_str, '(i0)') nrows
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column: row count mismatch for column " // trim(name) // &
                ": expected " // trim(expected_str) // " rows (from an earlier column) but got " // trim(got_str) // &
                ctx
        end if
    end subroutine parquet_check_row_count
    !> Expands a `nrows`-length row-keep mask into an element-level mask for a flat array laid
    !> out `block_width` contiguous elements per row (the col_size for a numeric/logical/matrix
    !> column, or item_len*col_size for a packed fixed-width string buffer) -- i.e. mask(i)
    !> applies to elements ((i-1)*block_width+1) : (i*block_width). Used with `pack` to compact
    !> a flat values/is_valid buffer down to its kept rows in one step.
    function parquet_mask_expand_block(mask, block_width) result(elem_mask)
        logical, intent(in) :: mask(:) !! per-row keep mask.
        integer(int64), intent(in) :: block_width !! array elements per row.
        logical, allocatable :: elem_mask(:) !! element-level mask, size(mask)*block_width long.

        if (block_width == 1_int64) then
            elem_mask = mask
        else
            elem_mask = reshape(spread(mask, 1, int(block_width)), [size(mask, kind=int64)*block_width])
        end if
    end function parquet_mask_expand_block
    !> Returns the row-keep mask applicable to a whole-column write of `nrows` (pre-mask) rows:
    !> the writer's file_mask (set by parquet_write_row_mask), or an all-.true. identity mask if
    !> masking is not in use. `error stop`s if a file_mask is set but its size doesn't match
    !> `nrows` exactly. Also marks the writer as started (blocking a later parquet_write_row_mask
    !> call) and as having had a whole-column write (blocking parquet_write_chunk_row_mask).
    subroutine parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
        integer(int64), intent(in) :: nrows !! this call's own (pre-mask) row count.
        logical, allocatable, intent(out) :: row_mask(:) !! this call's applicable row-keep mask, `nrows` long.
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        writer%write_started = .true.
        writer%any_whole_column_write = .true.

        if (allocated(writer%file_mask)) then
            if (size(writer%file_mask, kind=int64) /= nrows) then
                write(expected_str, '(i0)') size(writer%file_mask, kind=int64)
                write(got_str, '(i0)') nrows
                call writer_context_suffix(writer, ctx)
                error stop "parquet_write_column: values row count (" // trim(got_str) // &
                    ") does not match the mask set via parquet_write_row_mask (" // trim(expected_str) // &
                    " rows) for column " // trim(name) // ctx
            end if
            row_mask = writer%file_mask
        else
            allocate(row_mask(nrows))
            row_mask = .true.
        end if
    end subroutine parquet_writer_whole_column_mask
    module procedure parquet_write_row_mask_impl
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        call check_writer_open(writer)
        call writer_context_suffix(writer, ctx)
        if (writer%write_started) error stop &
            "parquet_write_row_mask: must be called before the writer's first parquet_write_column " // &
            "or parquet_new_row_group call" // ctx
        if (allocated(writer%file_mask)) error stop &
            "parquet_write_row_mask: already called for this writer -- it can only be set once" // ctx
        if (size(mask, kind=int64) <= 0_int64) error stop &
            "parquet_write_row_mask: mask must not be zero-length" // ctx
        writer%file_mask = mask
    end procedure parquet_write_row_mask_impl
    module procedure parquet_write_chunk_row_mask_impl
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        call check_writer_open(writer)
        call writer_context_suffix(writer, ctx)
        if (.not. writer%in_row_group) error stop &
            "parquet_write_chunk_row_mask: no row group is open (call parquet_new_row_group first)" // ctx
        if (writer%chunk_mask_set_this_group) error stop &
            "parquet_write_chunk_row_mask: already called for this row group" // ctx
        if (allocated(writer%file_mask)) error stop &
            "parquet_write_chunk_row_mask: cannot be used together with parquet_write_row_mask on the same writer" // ctx
        if (writer%any_whole_column_write) error stop &
            "parquet_write_chunk_row_mask: unavailable once a column has been written whole via " // &
            "parquet_write_column -- use parquet_write_row_mask instead" // ctx
        if (writer%row_group_first_write_done) error stop &
            "parquet_write_chunk_row_mask: must be called before this row group's first " // &
            "parquet_write_column_chunk call" // ctx
        if (writer%chunk_mask_scheme == 2) error stop &
            "parquet_write_chunk_row_mask: was not used for this writer's first row group -- it cannot be " // &
            "introduced for a later one" // ctx
        if (size(mask, kind=int64) /= writer%current_row_group_nrows) then
            write(expected_str, '(i0)') writer%current_row_group_nrows
            write(got_str, '(i0)') size(mask, kind=int64)
            error stop "parquet_write_chunk_row_mask: mask size (" // trim(got_str) // &
                ") does not match the open row group's own nrows (" // trim(expected_str) // ")" // ctx
        end if
        writer%chunk_mask = mask
        writer%chunk_mask_set_this_group = .true.
        ! The kept count for this row group is now known -- open (or re-open, with the correct
        ! kept count) the underlying C++ row group before any column's parquet_write_column_chunk
        ! call for it. See parquet_new_row_group_impl's own comment for why this can't happen any
        ! earlier for the per-row-group mask scheme, and row_group_is_empty for the kept-count-0 case.
        if (count(mask) == 0) then
            writer%row_group_is_empty = .true.
        else
            call parquet_writer_new_row_group(writer%handle, count(mask, kind=c_long_long))
            writer%cpp_row_group_open = .true.
        end if
    end procedure parquet_write_chunk_row_mask_impl
    module procedure parquet_open_writer
        integer :: i, k, n_enabled, jc
        character(len=:), allocatable :: compression_name, col_out_name
        integer :: level_value, chunk_size_value
        logical :: comp_ok
        logical :: compression_defaulted
        logical :: use_threads_value
        logical :: overwrite_value, file_exists
        character(len=12), parameter :: valid_compressions(6) = [character(len=12) :: &
            "uncompressed", "snappy", "gzip", "zstd", "brotli", "lz4"]

        overwrite_value = .true.
        if (present(overwrite)) overwrite_value = overwrite
        if (.not. overwrite_value) then
            inquire(file=trim(filename), exist=file_exists)
            if (file_exists) then
                error stop "parquet_open_writer: file already exists and overwrite=.false. (file: " // &
                    trim(filename) // ")"
            end if
        end if

        writer%handle = create_parquet_writer(trim(filename)//char(0))
        writer%filename = trim(filename)
        writer%is_schema_enforced = present(schema)
        writer%qc = present(schema)
        if (present(qc)) writer%qc = qc

        ! Defaults to "zstd" (level 3) -- Parquet-the-library's own built-in default is
        ! actually "uncompressed" (confirmed in parquet/properties.h), and the wider
        ! ecosystem convention on top of it (pyarrow, Spark, ...) is snappy, but this
        ! library intentionally goes one step further: zstd at a moderate level gives a
        ! meaningfully better compression ratio than snappy for a modest write-time cost,
        ! with no read-time penalty (see the compression benchmark backing this decision).
        ! The default level (3) only applies when compression itself is left absent --
        ! an explicit compression="zstd" with no compression_level still falls through to
        ! Arrow's own codec default (level 1) a few lines down, unchanged from before.
        compression_defaulted = .not. present(compression)
        compression_name = "zstd"
        if (present(compression)) call parquet_to_lower(trim(compression), compression_name)

        comp_ok = .false.
        do jc = 1, size(valid_compressions)
            if (trim(compression_name) == trim(valid_compressions(jc))) then
                comp_ok = .true.
                exit
            end if
        end do
        if (.not. comp_ok) then
            error stop "parquet_open_writer: unknown compression codec '" // trim(compression_name) // &
                "' (expected one of: uncompressed, snappy, gzip, zstd, brotli, lz4) (file: " // trim(filename) // ")"
        end if

        level_value = -huge(level_value) - 1 ! Arrow's kUseDefaultCompressionLevel sentinel (INT_MIN):
                                              ! "use the codec's own default".
        if (compression_defaulted) level_value = 3 ! This library's own default level for the default "zstd" codec.
        if (present(compression_level)) level_value = compression_level

        chunk_size_value = -1 ! <= 0 tells the C++ side "not set": auto-size from the final row count at close time.
        if (present(chunk_size)) chunk_size_value = chunk_size

        use_threads_value = .true.
        if (present(use_threads)) use_threads_value = use_threads

        call parquet_set_writer_options(writer%handle, trim(compression_name)//char(0), &
            int(level_value, kind=c_int), int(chunk_size_value, kind=c_long_long), &
            merge(1_c_int, 0_c_int, use_threads_value))

        if (present(schema)) then
            if (allocated(schema%maml%name)) writer%maml_name = schema%maml%name

            allocate(writer%all_columns(size(schema%cinfo%col)))
            writer%all_columns = schema%cinfo%col

            n_enabled = 0
            do i = 1, size(schema%cinfo%col)
                if (schema%cinfo%col(i)%is_set) n_enabled = n_enabled + 1
            end do

            if (n_enabled > 0) then
                allocate(writer%enabled_columns(n_enabled))
                allocate(writer%write_counts(n_enabled))
                writer%write_counts = 0
                k = 0
            end if

            do i = 1, size(schema%cinfo%col)
                if (schema%cinfo%col(i)%is_set) then
                    k = k + 1
                    writer%enabled_columns(k) = schema%cinfo%col(i)
                    ! Registers the schema under output_name (the column's
                    ! display/file name -- equal to name unless a col_map:
                    ! entry renamed it), not the internal name, since that's
                    ! what append_column's own arguments must also use for
                    ! Arrow to line up the field with its data.
                    call parquet_column_output_name(schema%cinfo%col(i), col_out_name)
                    call parquet_add_column_info(&
                        writer, &
                        col_out_name, &
                        schema%cinfo%col(i)%unit, &
                        schema%cinfo%col(i)%info, &
                        schema%cinfo%col(i)%ucd, &
                        schema%cinfo%col(i)%data_type, &
                        schema%cinfo%col(i)%array_size, &
                        schema%cinfo%col(i)%col_size )
                end if
            end do

            if (allocated(schema%metadata%items)) then
                do i = 1, size(schema%metadata%items)
                    call parquet_add_table_metadata(writer%handle, &
                        trim(schema%metadata%items(i)%key)//char(0), &
                        trim(schema%metadata%items(i)%value)//char(0), &
                        trim(schema%metadata%items(i)%description)//char(0))
                end do
            end if
        end if

        if (present(write_maml)) then
            if (write_maml) then
                if (.not. present(schema)) then
                    error stop "parquet_open_writer: write_maml=.true. requires a schema " // &
                        "(prepared by parquet_parse_maml) to be present (file: " // trim(filename) // ")"
                end if
                ! No separate "schema present but not obtained from parquet_parse_maml" check here:
                ! schema%cinfo%col (read just above, unconditionally, to populate writer%all_columns)
                ! is only ever populated by parquet_parse_maml (object or file form), and both of
                ! those forms unconditionally also set schema%metadata%source_maml_lines -- so by the
                ! time a schema safely reaches this point, source_maml_lines is already allocated.
                !
                ! The sidecar file itself is not written here: a col_size:/array_size: auto field
                ! may still be unresolved at this point (resolved later, at first write, or via
                ! schema%set_col_size/%set_array_size) -- writing now could bake in a stale "auto"
                ! that no longer matches the .parquet file's actual columns. parquet_close_writer
                ! writes it instead, once every column is guaranteed resolved.
                writer%sidecar_lines = schema%metadata%source_maml_lines
                call parquet_prune_disabled_fields(writer%sidecar_lines, schema%cinfo)
                writer%write_maml_requested = .true.
            end if
        end if
    end procedure parquet_open_writer
    !> Removes, from `lines` (a working copy of metadata%source_maml_lines),
    !> the `fields:` entries whose column is disabled (is_set = .false.) in
    !> `cinfo`, so that a sidecar .maml written via write_maml=.true. only
    !> lists the columns actually present in the .parquet file. Matches source
    !> MAML field blocks to cinfo%col by output_name (the name the source MAML
    !> text itself declares under fields:/name: -- equal to cinfo%col%name
    !> unless a col_map: rename applies); a block runs from its top-level
    !> "- ..." line up to (but not including) the next top-level line. If every
    !> column in cinfo is disabled, lines is left untouched instead of emptying
    !> out fields: entirely, since a MAML file with no fields fails
    !> parquet_validate_maml on the next read.
    subroutine parquet_prune_disabled_fields(lines, cinfo)
        character(len=:), allocatable, intent(inout) :: lines(:) !! working copy of source MAML lines, pruned in place.
        type(parquet_column_info), intent(in) :: cinfo !! schema whose disabled columns' fields: entries are removed.
        logical, allocatable :: keep(:)
        character(len=:), allocatable :: tline, key, cvalue, field_name, scratch_str
        character(len=:), allocatable :: new_lines(:)
        integer :: i, j, k, n, idx_fields, block_start, block_end, col_idx, n_keep

        if (.not. allocated(cinfo%col)) return
        if (size(cinfo%col) == 0) return
        if (.not. any(cinfo%col(:)%is_set)) return

        n = size(lines)
        idx_fields = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "fields:") then
                idx_fields = i
                exit
            end if
        end do
        if (idx_fields == 0) return

        allocate(keep(n))
        keep = .true.

        i = idx_fields + 1
        do while (i <= n)
            if (len_trim(lines(i)) == 0) then
                i = i + 1
                cycle
            end if

            if (lines(i)(1:1) /= " " .and. index(trim(adjustl(lines(i))), "-") /= 1) exit

            if (lines(i)(1:1) /= " ") then
                ! Top-level "- ..." line: start of a new field block. It runs
                ! until the next top-level (non-indented) line.
                block_start = i
                block_end = i
                j = i + 1
                do while (j <= n)
                    if (len_trim(lines(j)) == 0) exit
                    if (lines(j)(1:1) /= " ") exit
                    block_end = j
                    j = j + 1
                end do

                field_name = ""
                do k = block_start, block_end
                    if (k == block_start) then
                        tline = trim(adjustl(lines(k)))
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) == 0) cycle
                    else
                        tline = trim(adjustl(lines(k)))
                    end if
                    call parquet_split_key_value(tline, key, cvalue)
                    if (len_trim(key) == 0) cycle
                    call parquet_to_lower(trim(key), scratch_str)
                    if (scratch_str == "name") then
                        call parquet_unquote(cvalue, field_name)
                        exit
                    end if
                end do

                col_idx = 0
                if (len_trim(field_name) > 0) then
                    do k = 1, size(cinfo%col)
                        call parquet_column_output_name(cinfo%col(k), scratch_str)
                        if (trim(scratch_str) == trim(field_name)) then
                            col_idx = k
                            exit
                        end if
                    end do
                end if

                if (col_idx > 0) then
                    if (.not. cinfo%col(col_idx)%is_set) keep(block_start:block_end) = .false.
                end if

                i = block_end + 1
                cycle
            end if

            i = i + 1
        end do

        n_keep = count(keep)
        if (n_keep == n) return

        allocate(character(len=len(lines)) :: new_lines(n_keep))
        j = 0
        do i = 1, n
            if (keep(i)) then
                j = j + 1
                new_lines(j) = lines(i)
            end if
        end do

        call move_alloc(new_lines, lines)
    end subroutine parquet_prune_disabled_fields
    !> Rewrites every surviving field block's col_size:/array_size: value (if present) in `lines`
    !> (a working copy of a write_maml=.true. sidecar's source lines, already pruned of disabled
    !> fields by parquet_prune_disabled_fields) to `all_columns`' own final, fully-resolved
    !> value -- so a col_size: auto/array_size: auto placeholder in the original MAML source
    !> becomes the concrete number actually written to the .parquet file, matched by output_name
    !> the same way parquet_prune_disabled_fields matches field blocks to columns. Called by
    !> parquet_close_writer, once every enabled column's col_size/array_size is guaranteed
    !> resolved (parquet_close_writer's own missing-write check already ensures every enabled
    !> column was written at least once). A no-op for a column whose source never declared
    !> col_size:/array_size: at all (nothing to rewrite -- the implicit default of 1 already
    !> matches). Reallocates `lines` to a longer fixed element length up front if needed, since a
    !> resolved value's text (e.g. "col_size: 2000000000") can be longer than the original
    !> "col_size: auto"/blank line.
    subroutine parquet_rewrite_resolved_sizes(lines, all_columns)
        character(len=:), allocatable, intent(inout) :: lines(:) !! working copy of sidecar MAML lines, rewritten in place.
        type(parquet_column_type), intent(in) :: all_columns(:) !! writer's final, fully-resolved column state.
        character(len=:), allocatable :: tline, key, cvalue, field_name, scratch_str, new_text
        character(len=:), allocatable :: new_lines(:)
        integer :: i, j, k, n, idx_fields, block_start, block_end, col_idx, newlen
        character(len=32) :: numbuf

        if (size(all_columns) == 0) return

        n = size(lines)
        idx_fields = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "fields:") then
                idx_fields = i
                exit
            end if
        end do
        if (idx_fields == 0) return

        ! Room for the longest possible "  array_size: <10-digit int>" replacement text, so no
        ! rewritten line below is ever truncated by a too-short fixed element length.
        newlen = max(len(lines), 30)
        if (newlen > len(lines)) then
            allocate(character(len=newlen) :: new_lines(n))
            do i = 1, n
                new_lines(i) = lines(i)
            end do
            call move_alloc(new_lines, lines)
        end if

        i = idx_fields + 1
        do while (i <= n)
            if (len_trim(lines(i)) == 0) then
                i = i + 1
                cycle
            end if

            if (lines(i)(1:1) /= " " .and. index(trim(adjustl(lines(i))), "-") /= 1) exit

            if (lines(i)(1:1) /= " ") then
                block_start = i
                block_end = i
                j = i + 1
                do while (j <= n)
                    if (len_trim(lines(j)) == 0) exit
                    if (lines(j)(1:1) /= " ") exit
                    block_end = j
                    j = j + 1
                end do

                field_name = ""
                do k = block_start, block_end
                    if (k == block_start) then
                        tline = trim(adjustl(lines(k)))
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) == 0) cycle
                    else
                        tline = trim(adjustl(lines(k)))
                    end if
                    call parquet_split_key_value(tline, key, cvalue)
                    if (len_trim(key) == 0) cycle
                    call parquet_to_lower(trim(key), scratch_str)
                    if (scratch_str == "name") then
                        call parquet_unquote(cvalue, field_name)
                        exit
                    end if
                end do

                col_idx = 0
                if (len_trim(field_name) > 0) then
                    do k = 1, size(all_columns)
                        call parquet_column_output_name(all_columns(k), scratch_str)
                        if (trim(scratch_str) == trim(field_name)) then
                            col_idx = k
                            exit
                        end if
                    end do
                end if

                if (col_idx > 0) then
                    do k = block_start, block_end
                        tline = trim(adjustl(lines(k)))
                        if (k == block_start) then
                            tline = trim(adjustl(tline(2:)))
                            if (len_trim(tline) == 0) cycle
                        end if
                        call parquet_split_key_value(tline, key, cvalue)
                        if (len_trim(key) == 0) cycle
                        call parquet_to_lower(trim(key), scratch_str)
                        if (scratch_str == "col_size") then
                            write(numbuf, '(I0)') all_columns(col_idx)%col_size
                            new_text = "  col_size: " // trim(numbuf)
                            lines(k) = new_text
                        else if (scratch_str == "array_size") then
                            write(numbuf, '(I0)') all_columns(col_idx)%array_size
                            new_text = "  array_size: " // trim(numbuf)
                            lines(k) = new_text
                        end if
                    end do
                end if

                i = block_end + 1
                cycle
            end if

            i = i + 1
        end do
    end subroutine parquet_rewrite_resolved_sizes
    !> Writes `lines` to a sidecar .maml file next to `parquet_filename`: the same
    !> path with a trailing ".parquet" replaced by ".maml", or ".maml" appended if
    !> there is no ".parquet" suffix.
    subroutine parquet_write_maml_sidecar(parquet_filename, lines)
        character(len=*), intent(in) :: parquet_filename !! output .parquet path; sidecar name is derived from this.
        character(len=*), intent(in) :: lines(:) !! MAML source lines to write verbatim.
        character(len=:), allocatable :: maml_filename
        integer :: unit, i, n

        n = len(parquet_filename)
        if (n >= 8) then
            if (parquet_filename(n-7:n) == ".parquet") then
                maml_filename = parquet_filename(1:n-8) // ".maml"
            else
                maml_filename = parquet_filename // ".maml"
            end if
        else
            maml_filename = parquet_filename // ".maml"
        end if

        open(newunit=unit, file=maml_filename, status="replace", action="write", form="formatted")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine parquet_write_maml_sidecar
    !> Declares one column on a schema-less writer (%is_schema_enforced
    !> .false.), mirroring the field metadata a MAML fields: entry would
    !> otherwise supply. Called internally the first time each column
    !> name is written; not part of the public API.
    !> Plain contained subroutine (not a module procedure) for the same
    !> reason as parquet_check_read_row_count above -- its body already
    !> lived in this file, the parquet_write parent, not a descendant
    !> submodule.
    subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size, col_size)
        type(parquet_writer), intent(inout) :: writer !! schema-less writer gaining this column.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: unit !! unit of measurement.
        character(len=*), intent(in) :: description !! short free-text description.
        character(len=*), intent(in) :: ucd !! IVOA Unified Content Descriptor.
        character(len=*), intent(in) :: data_type !! column's data type.
        integer, intent(in) :: array_size !! maximum string length (string columns only).
        integer, intent(in) :: col_size !! vector-column element count (1 for a scalar column).

        call parquet_add_column_metadata(&
            writer%handle, &
            trim(name)//char(0), &
            trim(unit)//char(0), &
            trim(description)//char(0), &
            trim(ucd)//char(0), &
            trim(data_type)//char(0), &
            int(array_size, kind=c_long_long), &
            int(col_size, kind=c_long_long) )
    end subroutine parquet_add_column_info
    !> Formats `value` as a trimmed plain integer for a qc-violation WARNING message.
    subroutine parquet_qc_format_int(value, text)
        integer(int64), intent(in) :: value !! value to format.
        character(len=:), allocatable, intent(out) :: text !! formatted, trimmed text.
        character(len=32) :: buf

        write(buf, '(i0)') value
        text = trim(adjustl(buf))
    end subroutine parquet_qc_format_int
    !> Errors out if `name`'s column is listed under extra: protected_cols:
    !> (see parquet_parse_protected_cols) and `is_valid_flat` contains any
    !> .false. entry. A no-op for a schema-less writer or an unlisted column.
    subroutine parquet_check_protected(writer, name, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! column name.
        logical, intent(in) :: is_valid_flat(:) !! flattened validity mask for this write call.
        integer :: idx

        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (writer%all_columns(idx)%is_protected .and. .not. all(is_valid_flat)) then
            error stop "parquet_write_column: column '" // trim(name) // &
                "' is protected (extra: protected_cols:) and cannot contain Null values"
        end if
    end subroutine parquet_check_protected
    !> If writer%qc is set and the column's declared qc: miss: does NOT allow Null (the default,
    !! including when no qc: block at all was declared -- see parquet_column_type%qc_allow_null),
    !! prints a WARNING naming the column and how many of its elements are Null in this write
    !! call. Never errors -- writing proceeds regardless, and qc_min/qc_max are unaffected (they
    !! already only ever look at valid elements). No-op for a schema-less writer, an undeclared
    !! column, or a column whose qc: miss: allows Null.
    subroutine parquet_check_qc_miss(writer, name, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! column name.
        logical, intent(in) :: is_valid_flat(:) !! flattened validity mask for this write call (.true. => valid).
        integer :: idx
        integer(int64) :: n_null, n_total
        character(len=:), allocatable :: fmt_int, fmt_int2

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (writer%all_columns(idx)%qc_allow_null) return

        n_total = size(is_valid_flat, kind=int64)
        n_null = count(.not. is_valid_flat, kind=int64)
        if (n_null == 0) return

        call parquet_qc_format_int(n_null, fmt_int)
        call parquet_qc_format_int(n_total, fmt_int2)
        print '(a)', "WARNING: qc violation for column '" // trim(name) // "': " // fmt_int // " of " // &
            fmt_int2 // " element(s) are Null (qc: miss: not declared)"
    end subroutine parquet_check_qc_miss
    !> Builds the int8 validity buffer and c_ptr passed down to the C++
    !> append_* functions from a caller's flattened `is_valid` mask (1 =
    !> valid, 0 = Null); `valid_ptr` stays c_null_ptr (no validity buffer
    !> passed to C++ at all) when `is_valid` was not supplied by the caller,
    !> which keeps every column non-nullable by default -- exactly today's
    !> behavior.
    subroutine parquet_make_valid_buf_write(is_valid_flat, valid_buf, valid_ptr)
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask, or absent for a non-nullable write.
        integer(c_int8_t), allocatable, target, intent(out) :: valid_buf(:) !! int8 validity buffer backing valid_ptr.
        type(c_ptr), intent(out) :: valid_ptr !! c_loc(valid_buf), or c_null_ptr if is_valid_flat is absent.
        integer(int64) :: i

        if (present(is_valid_flat)) then
            allocate(valid_buf(size(is_valid_flat, kind=int64)))
            do i = 1_int64, size(is_valid_flat, kind=int64)
                valid_buf(i) = merge(1_c_int8_t, 0_c_int8_t, is_valid_flat(i))
            end do
            valid_ptr = c_loc(valid_buf)
        else
            valid_ptr = c_null_ptr
        end if
    end subroutine parquet_make_valid_buf_write
    !> Validates that a parquet_write_column_chunk call's own row count (`nrows`, from the shape
    !> of its `values`) matches the currently-open row group's own size
    !> (writer%current_row_group_nrows, set by parquet_new_row_group) -- the streaming
    !> counterpart to parquet_check_row_count, which instead fixes/checks a row count against
    !> the whole file.
    !> Also returns this chunk's applicable row-keep mask (row_mask, always allocated and
    !> `nrows` long: an identity mask if no masking scheme is active) and enforces
    !> parquet_write_chunk_row_mask's all-or-nothing-per-writer rule: writer%chunk_mask_scheme is
    !> fixed (0 -> 1 or 2) at the first parquet_write_column_chunk call of the writer's first row
    !> group, and any later row group must then agree with that decision.
    subroutine parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        type(parquet_writer), intent(inout) :: writer !! open writer, expected to have a row group open.
        character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
        integer(c_long_long), intent(in) :: nrows !! row count of this chunk's own values.
        logical, allocatable, intent(out) :: row_mask(:) !! this chunk's applicable row-keep mask, `nrows` long.
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        call writer_context_suffix(writer, ctx)
        if (.not. writer%in_row_group) error stop &
            "parquet_write_column_chunk: no row group is open (call parquet_new_row_group first) for column " // &
            trim(name) // ctx
        if (nrows /= writer%current_row_group_nrows) then
            write(expected_str, '(i0)') writer%current_row_group_nrows
            write(got_str, '(i0)') nrows
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column_chunk: row count mismatch for column " // trim(name) // &
                ": the open row group has " // trim(expected_str) // " rows but this chunk has " // trim(got_str) // &
                ctx
        end if

        if (writer%chunk_mask_scheme == 0) then
            writer%chunk_mask_scheme = merge(1, 2, writer%chunk_mask_set_this_group)
        else if (writer%chunk_mask_scheme == 1 .and. .not. writer%chunk_mask_set_this_group) then
            error stop "parquet_write_column_chunk: parquet_write_chunk_row_mask must be called for every " // &
                "row group once used for any one of them (column " // trim(name) // ")" // ctx
        end if
        if (.not. writer%cpp_row_group_open .and. .not. writer%row_group_is_empty) then
            ! No mask was ever set for this row group (parquet_write_chunk_row_mask was not
            ! called) -- the kept count is simply nrows (identity), exactly today's non-masked
            ! behavior. Open the underlying C++ row group now, before this first chunk is appended.
            ! (row_group_is_empty can only already be true here via an explicit
            ! parquet_write_chunk_row_mask call with an all-.false. mask -- identity is never empty
            ! since nrows itself is always positive.)
            call parquet_writer_new_row_group(writer%handle, nrows)
            writer%cpp_row_group_open = .true.
        end if
        writer%row_group_first_write_done = .true.
        row_mask = writer%chunk_mask
    end subroutine parquet_check_row_group_row_count
    !> Like parquet_assert_column_type, but requires an EXACT data_type match rather than
    !> parquet_is_type_compatible's lenient cross-numeric-type compatibility: unlike
    !> parquet_write_column, parquet_write_column_chunk never converts `values`' own kind to the
    !> schema's declared type before writing (there is no parquet_append_as_schema_chunk_*
    !> dispatcher the way there is for the batch path) -- see parquet_write_column_chunk's own
    !> doc-comment in parquet.f90.
    subroutine parquet_assert_column_type_exact(writer, name, expected_type)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: expected_type !! this chunk-write call's own value kind, e.g. "int32".
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        ! Unreachable in practice: every one of this function's 12 callers (one per type/shape)
        ! already performs this identical is_schema_enforced-guarded idx==0 check and aborts
        ! itself first, before ever calling in here -- kept as a defensive belt-and-suspenders
        ! check in case a future caller is added without repeating it.
        call writer_context_suffix(writer, ctx)
        if (idx == 0) error stop &
            "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
            trim(name) // ctx ! GCOVR_EXCL_LINE
        if (trim(writer%all_columns(idx)%data_type) /= trim(expected_type)) then
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column_chunk: type mismatch for column " // trim(name) // &
                " (expected " // trim(expected_type) // ", got " // trim(writer%all_columns(idx)%data_type) // &
                ") -- parquet_write_column_chunk requires an exact type match, unlike parquet_write_column" // &
                ctx
        end if
    end subroutine parquet_assert_column_type_exact
    !> Marks `name` as written for parquet_mark_column_written's own bookkeeping (write_counts/
    !> written_names), but only on its very first chunk ever -- unlike parquet_write_column, a
    !> chunk column legitimately receives many write calls (one per row group), and
    !> parquet_mark_column_written itself would error stop ("written more than once") on any
    !> call after the first.
    subroutine parquet_chunk_mark_written_if_first(writer, name)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer :: idx, i
        logical :: is_first

        is_first = .true.
        if (writer%is_schema_enforced) then
            if (allocated(writer%enabled_columns)) then
                idx = parquet_get_enabled_column_index(writer, name)
                if (idx > 0) is_first = (writer%write_counts(idx) == 0)
            end if
        else
            if (allocated(writer%written_names)) then
                do i = 1, size(writer%written_names)
                    if (trim(writer%written_names(i)) == trim(name)) then
                        is_first = .false.
                        exit
                    end if
                end do
            end if
        end if
        if (is_first) call parquet_mark_column_written(writer, name)
    end subroutine parquet_chunk_mark_written_if_first
    !> Shared worker for parquet_new_row_group_int32/_int64 -- see the parquet_new_row_group
    !> generic interface in parquet.f90. Also resets the current row group's masking state and,
    !> for a writer using the shared whole-file mask (parquet_write_row_mask) together with row
    !> groups, claims this row group's window (the next `nrows` positions of the stored mask) --
    !> see "Chunked masking mechanics -- shared whole-file mask" in feature_write_mask.md.
    subroutine parquet_new_row_group_impl(writer, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        integer(c_long_long), intent(in) :: nrows !! row count for the new row group; must be positive.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        call check_writer_open(writer)
        call writer_context_suffix(writer, ctx)
        if (writer%in_row_group) error stop &
            "parquet_new_row_group: a row group is already open -- call parquet_finish_row_group first" // ctx
        if (nrows <= 0) error stop "parquet_new_row_group: nrows must be positive" // ctx
        writer%write_started = .true.
        writer%in_row_group = .true.
        writer%current_row_group_nrows = nrows
        writer%chunk_mask_set_this_group = .false.
        writer%row_group_first_write_done = .false.
        writer%cpp_row_group_open = .false.
        writer%row_group_is_empty = .false.

        if (allocated(writer%file_mask)) then
            ! The shared whole-file mask's window (and therefore this row group's post-mask kept
            ! count) is fully known right now -- unlike the per-row-group scheme below, there is no
            ! later call that could still change it -- so the underlying C++ row group can (and
            ! must, to avoid a col_size/row-count mismatch against the compacted data any
            ! parquet_write_column_chunk call below will supply) be opened immediately with the
            ! *kept* count, not `nrows` itself. A kept count of exactly 0 has no underlying C++ row
            ! group at all (Arrow's NewRowGroup requires a positive count) -- see row_group_is_empty.
            writer%mask_used_with_row_groups = .true.
            if (writer%mask_cursor + nrows > size(writer%file_mask, kind=int64)) then
                error stop "parquet_new_row_group: this row group's nrows would claim more positions of the " // &
                    "mask (set via parquet_write_row_mask) than it has left" // ctx
            end if
            writer%chunk_mask = writer%file_mask(writer%mask_cursor+1 : writer%mask_cursor+nrows)
            writer%mask_cursor = writer%mask_cursor + nrows
            if (count(writer%chunk_mask) == 0) then
                writer%row_group_is_empty = .true.
            else
                call parquet_writer_new_row_group(writer%handle, count(writer%chunk_mask, kind=c_long_long))
                writer%cpp_row_group_open = .true.
            end if
        else
            ! Per-row-group masking (parquet_write_chunk_row_mask) may or may not be used for this
            ! row group -- not knowable yet, since that call (if it comes at all) happens after
            ! this one. Default to an identity mask (kept count = nrows, exactly today's non-masked
            ! behavior) and defer the actual C++ open to whichever comes first of
            ! parquet_write_chunk_row_mask_impl or parquet_check_row_group_row_count (this row
            ! group's first parquet_write_column_chunk call) -- both commit using the by-then-known
            ! kept count.
            writer%chunk_mask = spread(.true., 1, nrows)
        end if
    end subroutine parquet_new_row_group_impl
    module procedure parquet_new_row_group_int32
        call parquet_new_row_group_impl(writer, int(nrows, kind=c_long_long))
    end procedure parquet_new_row_group_int32
    module procedure parquet_new_row_group_int64
        call parquet_new_row_group_impl(writer, nrows)
    end procedure parquet_new_row_group_int64
    module procedure parquet_finish_row_group
        call check_writer_open(writer)
        if (writer%row_group_is_empty) then
            ! A zero-kept-row row group has no underlying C++ row group at all (never opened) --
            ! nothing to finish either.
            continue
        else
            if (.not. writer%cpp_row_group_open) then
                ! Fallback: this row group never got a mask (parquet_write_chunk_row_mask) nor any
                ! parquet_write_column_chunk call at all -- open the C++ row group now (identity
                ! kept count) so parquet_writer_finish_row_group's own "no columns have been
                ! written" diagnostic still fires exactly as it did before masking existed, rather
                ! than the less specific "no row group is open". Both of the next two lines always
                ! execute immediately before the very same call's C++-side abort (there is no
                ! legitimate way to reach this fallback and then NOT abort -- see
                ! scenario_mask_row_group_no_writes_at_all), and a std::abort() discards this whole
                ! process's coverage counters (Fortran gcov included, not just C++'s own -- see
                ! CLAUDE.md's "Fortran gcov attribution artifacts"/report_fatal_error notes), so
                ! these two lines can never show as covered despite genuinely executing every time.
                call parquet_writer_new_row_group(writer%handle, writer%current_row_group_nrows) ! GCOVR_EXCL_LINE
                writer%cpp_row_group_open = .true. ! GCOVR_EXCL_LINE
            end if
            call parquet_writer_finish_row_group(writer%handle)
        end if
        writer%in_row_group = .false.
        writer%current_row_group_nrows = 0
    end procedure parquet_finish_row_group
    module procedure parquet_get_chunk_size_writer_int32
        call check_writer_open(writer)
        chunk_size = int(parquet_writer_get_chunk_size(writer%handle), kind=int32)
    end procedure parquet_get_chunk_size_writer_int32
    module procedure parquet_get_chunk_size_writer_int64
        call check_writer_open(writer)
        chunk_size = parquet_writer_get_chunk_size(writer%handle)
    end procedure parquet_get_chunk_size_writer_int64
    module procedure parquet_close_writer
        integer :: i
        character(len=32) :: cursor_str, mask_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (.not. c_associated(writer%handle)) then
            error stop "parquet_close_writer: writer has not been opened, or was already closed"
        end if

        ! Nested rather than one `.and.`: Fortran does not guarantee short-circuit evaluation, so a
        ! single combined condition references size(writer%file_mask) even when no mask was ever
        ! set and file_mask is unallocated (a runtime error under -fcheck=all, undefined otherwise).
        if (writer%mask_used_with_row_groups) then
            if (allocated(writer%file_mask)) then
                if (writer%mask_cursor /= size(writer%file_mask, kind=int64)) then
                    write(cursor_str, '(i0)') writer%mask_cursor
                    write(mask_str, '(i0)') size(writer%file_mask, kind=int64)
                    call writer_context_suffix(writer, ctx)
                    error stop "parquet_close_writer: the mask set via parquet_write_row_mask (" // trim(mask_str) // &
                        " rows) was not fully consumed by this writer's row groups (only " // trim(cursor_str) // &
                        " rows claimed)" // ctx
                end if
            end if
        end if

        if (writer%is_schema_enforced .and. allocated(writer%enabled_columns)) then
            do i = 1, size(writer%enabled_columns)
                if (writer%write_counts(i) == 0) then
                    ! Filename/schema name go on their own print lines rather
                    ! than into the error stop text, to keep that text short.
                    if (allocated(writer%filename)) print '(a)', "parquet_close_writer: output file: " // trim(writer%filename)
                    if (allocated(writer%maml_name)) then
                        print '(a)', "parquet_close_writer: schema: " // trim(writer%maml_name)
                    else
                        print '(a)', "parquet_close_writer: schema: (unnamed, built in-memory)"
                    end if
                    error stop "parquet_close_writer: missing write for enabled column: " // trim(writer%enabled_columns(i)%name)
                end if
            end do
        end if

        call close_parquet_writer(writer%handle)

        if (writer%write_maml_requested) then
            call parquet_rewrite_resolved_sizes(writer%sidecar_lines, writer%all_columns)
            call parquet_write_maml_sidecar(writer%filename, writer%sidecar_lines)
            deallocate(writer%sidecar_lines)
            writer%write_maml_requested = .false.
        end if

        writer%handle = c_null_ptr
        if (allocated(writer%all_columns)) deallocate(writer%all_columns)
        if (allocated(writer%write_counts)) deallocate(writer%write_counts)
        if (allocated(writer%enabled_columns)) deallocate(writer%enabled_columns)
        if (allocated(writer%written_names)) deallocate(writer%written_names)
        writer%is_schema_enforced = .false.
        writer%expected_nrows = -1_c_long_long
    end procedure parquet_close_writer
    !> Safety net for a writer whose handle is still open when it goes out of
    !> scope or is overwritten (e.g. reassigned, or an early RETURN between
    !> parquet_open_writer and parquet_close_writer): frees the underlying
    !> C++ object so the process doesn't leak it. Uses abandon_parquet_writer, not
    !> close_parquet_writer: the latter builds/writes the final table and checks that every
    !> declared column was written and every row group finished, and throws (an uncaught C++
    !> exception that crosses the extern "C" boundary and aborts the process) if not -- erroring,
    !> let alone crashing, from an implicit finalizer on an incompletely-written file would be
    !> surprising. The resulting output file is therefore not guaranteed complete/valid when
    !> reached this way -- always prefer calling parquet_close_writer explicitly.
    module procedure writer_finalize
        if (c_associated(this%handle)) then
            call abandon_parquet_writer(this%handle)
            this%handle = c_null_ptr
        end if
    end procedure writer_finalize

end submodule parquet_write
