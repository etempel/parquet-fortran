!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Writer lifecycle (open/close/new_row_group/finish_row_group) and the
!> type-generic shared helpers every write-specifics child (parquet_write_numeric/
!> _string/_temporal) reaches by host association: schema/column-info lookups,
!> row-count and row-mask bookkeeping, output-name resolution, qc: integer-count
!> text formatting (shared by numeric and string qc: reports), and the
!> write_maml=.true. sidecar .maml.
submodule (parquet_core) parquet_write
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use parquet_bindings
    use parquet_settings, only: parquet_valid_compressions, parquet_resolve_writer_compression, parquet_get_default_use_threads, &
        parquet_push_settings_to_cpp, &
        parquet_emit_warning, parquet_emit_error_context
    use parquet_temporal, only: parquet_date, parquet_time, parquet_timestamp
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
    module procedure writer_lock_claim
        ! An unopened writer has no handle to guard. Leaving self%handle null here is what makes
        ! the finalizer below safe to run unconditionally.
        if (.not. c_associated(writer%handle)) return
        self%handle = writer%handle
        call parquet_writer_enter(self%handle)
    end procedure writer_lock_claim
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
    !> The type token `items(idx)` should be written to the file with, which is its own
    !> %datatype unless the caller has ALSO added a literal "<key>.datatype" entry of their own.
    !>
    !> A typed entry makes the writer synthesize a companion "<key>.datatype" key (see
    !> build_file_metadata, parquet_wrapper.cpp); a caller's explicit entry of that name would
    !> then be the second writer of it, and the file would carry two same-named entries free to
    !> disagree -- exactly the wrong-answer-with-no-abort failure a reader trusting the companion
    !> would inherit. So the explicit entry wins and the synthesized one is dropped, with a
    !> warning: refusing the whole write over a metadata naming clash would be disproportionate
    !> when the file is otherwise valid.
    !>
    !> Two consequences of resolving this here rather than in %add_metadata, both deliberate.
    !> The warning names the file/schema (per the error-context convention), which %add_metadata
    !> cannot -- but it therefore fires once per parquet_open_writer call, so a schema reused for
    !> several files warns once per file, and it is not silenced by that call's warn=.false.
    !> (which suppresses the duplicate-key warning at add time, a different check). And the key
    !> loses its VOTable type too, falling back to char/arraysize="*": the caller has taken over
    !> that key's type declaration, so the library declines to guess a second one.
    subroutine resolve_metadata_datatype(writer, items, idx, datatype)
        type(parquet_writer), intent(in) :: writer !! writer whose file/maml names the warning reports.
        type(parquet_metadata_entry), intent(in) :: items(:) !! every table metadata entry, scanned for a collision.
        integer, intent(in) :: idx !! 1-based index of the entry being written.
        character(len=:), allocatable, intent(out) :: datatype !! token to write, or "" for none.
        character(len=:), allocatable :: ctx
        integer :: j

        datatype = ""
        if (.not. allocated(items(idx)%datatype)) return
        if (len_trim(items(idx)%datatype) == 0) return
        datatype = trim(items(idx)%datatype)

        ! O(n^2) over table metadata entries, which is tens of entries at the sizes this is
        ! written for -- not worth a hash, and this runs once per file open.
        do j = 1, size(items)
            if (trim(items(j)%key) /= trim(items(idx)%key) // ".datatype") cycle
            call writer_context_suffix(writer, ctx)
            call parquet_emit_warning("parquet_open_writer: metadata key '" // trim(items(idx)%key) // &
                ".datatype' was added explicitly, so the type recorded for '" // trim(items(idx)%key) // &
                "' (" // datatype // ") is not written -- the explicit entry wins" // ctx)
            datatype = ""
            return
        end do
    end subroutine resolve_metadata_datatype
    module procedure parquet_assert_column_type
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: proc !! calling procedure named in the message.

        if (.not. writer%is_schema_enforced) return

        proc = "parquet_write_column"
        if (present(context)) proc = context

        idx = parquet_get_defined_column_index(writer, name)

        if (.not. parquet_is_type_compatible(writer%all_columns(idx)%data_type, expected_type)) then
            call writer_context_suffix(writer, ctx)
            error stop proc // ": type mismatch for column " // trim(name) // &
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
                    ! Deliberately does not tell the caller to "use the matrix write form": this is
                    ! also reached by a parquet_string_column write, which has no matrix form at all
                    ! (it is scalar-only, and rejects col_size /= 1 a few lines later).
                    error stop "parquet_write_column: col_size for column '" // trim(name) // "' is still " // &
                        "'auto' -- call schema%set_col_size before parquet_open_writer. Only a matrix (2-D) " // &
                        "write can resolve col_size from the data's own shape"
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
    !>
    !> A parquet_string_column write has no declared length to pass here at all; it records what it
    !> actually wrote in writer%observed_string_len instead, and parquet_reconcile_string_sizes
    !> below settles the column's reported array_size once, at close.
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
    !> Makes every string column's reported array_size describe what was actually written, run once
    !> by parquet_close_writer BEFORE close_parquet_writer serializes the footer (the sidecar is
    !> rewritten from writer%all_columns afterwards, so both outputs follow from this one step).
    !>
    !> Only a parquet_string_column write can make this necessary, and it does so in two ways. A
    !> column declared `array_size: auto` and written only that way has no declared length anywhere
    !> to resolve from -- left alone it would reach the sidecar as the raw parquet_size_auto
    !> sentinel, i.e. `array_size: -1`, which is not a valid MAML at all (feature_risks.md Risk-83).
    !> And a column whose declaration is simply too small is written anyway: the compact path stores
    !> each element's own bytes and does not enforce array_size, which is deliberate -- a reader
    !> takes each length from the data and never needs the declaration. **Accepting an inaccurate
    !> declaration on the way in is a choice; repeating it on the way out is not**, so the value
    !> reported here is the longest element actually written whenever that exceeds the declaration.
    !>
    !> Deliberately at CLOSE rather than per write: raising the value mid-write would also raise the
    !> ceiling `any_item_too_long` enforces on the padded (character-array) paths, so a column
    !> written through both forms would have its declared limit quietly relaxed for the padded half.
    !> At close there are no writes left to affect.
    subroutine parquet_reconcile_string_sizes(writer)
        type(parquet_writer), intent(inout) :: writer !! writer being closed.
        integer :: i, k, resolved
        character(len=:), allocatable :: colname !! internal column name; must NOT be outname, see below.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        if (.not. writer%is_schema_enforced) return
        if (.not. allocated(writer%all_columns) .or. .not. allocated(writer%observed_string_len)) return

        do i = 1, size(writer%all_columns)
            if (writer%all_columns(i)%array_size == parquet_size_auto) then
                ! An all-null or all-empty column observes 0, which is not a legal array_size; 1 is
                ! the same floor parquet_write_empty_columns applies for the same reason.
                resolved = max(1, writer%observed_string_len(i))
            else if (writer%observed_string_len(i) > writer%all_columns(i)%array_size) then
                resolved = writer%observed_string_len(i)
            else
                cycle
            end if

            writer%all_columns(i)%array_size = resolved
            colname = trim(writer%all_columns(i)%name)
            k = parquet_get_enabled_column_index(writer, colname)
            if (k > 0) writer%enabled_columns(k)%array_size = resolved
            ! colname and outname must stay SEPARATE variables. parquet_resolve_output_name's
            ! output_name dummy is allocatable intent(out), so it is deallocated on entry --
            ! passing one variable as both arguments frees the very storage its own intent(in)
            ! `name` dummy points at, and the trim(name) inside it then reads freed memory.
            call parquet_resolve_output_name(writer, colname, outname)
            call parquet_update_column_metadata_size(writer%handle, trim(outname)//char(0), &
                int(writer%all_columns(i)%col_size, kind=c_long_long), int(resolved, kind=c_long_long))
        end do
    end subroutine parquet_reconcile_string_sizes
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
    !> Reports whether a whole-file row mask (parquet_write_row_mask) is in force for a
    !> whole-column write of `nrows` (pre-mask) rows and, when one is, returns it in `row_mask`.
    !> `error stop`s if a file_mask is set but its size doesn't match `nrows` exactly. Also marks
    !> the writer as started (blocking a later parquet_write_row_mask call) and as having had a
    !> whole-column write (blocking parquet_write_chunk_row_mask).
    !>
    !> `masked` is .false. for the overwhelmingly common unmasked write, and `row_mask` is then
    !> left UNALLOCATED rather than filled with an all-.true. identity mask. Callers take their
    !> own fast path on it -- see the "unmasked fast path" note in parquet_write_numeric.f90.
    !> Fabricating the identity mask here is what used to make every ordinary write allocate and
    !> then `pack` through a full-length logical array that could not remove a single row.
    subroutine parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
        integer(int64), intent(in) :: nrows !! this call's own (pre-mask) row count.
        logical, allocatable, intent(out) :: row_mask(:) !! the `nrows`-long row-keep mask; unallocated if .not. masked.
        logical, intent(out) :: masked !! .true. if a whole-file row mask applies to this write.
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        writer%write_started = .true.
        writer%any_whole_column_write = .true.

        masked = allocated(writer%file_mask)
        if (.not. masked) return

        if (size(writer%file_mask, kind=int64) /= nrows) then
            write(expected_str, '(i0)') size(writer%file_mask, kind=int64)
            write(got_str, '(i0)') nrows
            call writer_context_suffix(writer, ctx)
            error stop "parquet_write_column: values row count (" // trim(got_str) // &
                ") does not match the mask set via parquet_write_row_mask (" // trim(expected_str) // &
                " rows) for column " // trim(name) // ctx
        end if
        row_mask = writer%file_mask
    end subroutine parquet_writer_whole_column_mask
    module procedure parquet_write_row_mask_impl
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).

        call check_writer_open(writer)
        call lk%claim(writer)
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
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).

        call check_writer_open(writer)
        call lk%claim(writer)
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
        character(len=:), allocatable :: dt !! resolve_metadata_datatype scratch.
        integer :: level_value, chunk_size_value
        logical :: comp_ok
        logical :: use_threads_value
        logical :: overwrite_value, file_exists
        !> The schema this writer actually reads, which is a local COPY of the caller's. It has to
        !! be a copy because `schema` is intent(in) and a schema whose %maml was populated
        !! directly (an embedded MAML, say) still needs parsing before %cinfo exists -- parsing a
        !! copy is legal under intent(in) and cannot race, since no other thread can see it.
        !! Unconditional rather than only-when-unparsed so that everything below reads one
        !! variable; the cost is one schema copy per file opened, on a path that already copies
        !! every column into writer%all_columns.
        type(parquet_schema) :: eff_schema
        !> An alias derived from parquet_settings' own list, never a second copy of it: the setter
        !! and this argument check must accept exactly the same set of codecs.
        character(len=12), parameter :: valid_compressions(6) = parquet_valid_compressions

        ! Refresh the C++ side's copy of every mirrored setting before any C++ state exists --
        ! see parquet_push_settings_to_cpp. `target_row_group_bytes` is read by the row-group
        ! sizing at close, so it has to be current from the moment this writer exists.
        call parquet_push_settings_to_cpp()

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
        ! Both defaults are overridable process-wide (parquet_set_default_compression /
        ! ..._level), and the whole resolution -- including the rule that level 3 applies only
        ! when the codec was defaulted all the way -- lives in one place rather than here; see
        ! parquet_resolve_writer_compression (src/parquet_settings.f90) for why that condition
        ! is load-bearing.
        call parquet_resolve_writer_compression(compression, compression_level, compression_name, level_value)

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

        chunk_size_value = -1 ! <= 0 tells the C++ side "not set": auto-size from the final row count at close time.
        if (present(chunk_size)) chunk_size_value = chunk_size

        use_threads_value = parquet_get_default_use_threads()
        if (present(use_threads)) use_threads_value = use_threads

        call parquet_set_writer_options(writer%handle, trim(compression_name)//char(0), &
            int(level_value, kind=c_int), int(chunk_size_value, kind=c_long_long), &
            merge(1_c_int, 0_c_int, use_threads_value))

        if (present(schema)) then
            eff_schema = schema
            if (.not. eff_schema%is_parsed()) then
                ! A schema built with %init/%add_field is already in step with its own text, and
                ! one loaded by parquet_parse_maml obviously is -- so this only fires for a schema
                ! whose %maml was populated directly (the embedded-MAML route) and never parsed.
                if (.not. allocated(eff_schema%maml%lines)) then
                    error stop "parquet_open_writer: schema is empty -- build it with " // &
                        "schema%init/%add_field, or load one with parquet_parse_maml (file: " // &
                        trim(filename) // ")"
                end if
                call parquet_parse_maml(eff_schema)
            end if
            if (allocated(eff_schema%maml%name)) writer%maml_name = eff_schema%maml%name

            allocate(writer%all_columns(size(eff_schema%cinfo%col, kind=int64)))
            writer%all_columns = eff_schema%cinfo%col
            allocate(writer%observed_string_len(size(eff_schema%cinfo%col, kind=int64)))
            writer%observed_string_len = 0

            n_enabled = 0
            do i = 1, size(eff_schema%cinfo%col)
                if (eff_schema%cinfo%col(i)%is_set) n_enabled = n_enabled + 1
            end do

            if (n_enabled > 0) then
                allocate(writer%enabled_columns(n_enabled))
                allocate(writer%write_counts(n_enabled))
                writer%write_counts = 0
                k = 0
            end if

            do i = 1, size(eff_schema%cinfo%col)
                if (eff_schema%cinfo%col(i)%is_set) then
                    k = k + 1
                    writer%enabled_columns(k) = eff_schema%cinfo%col(i)
                    ! Registers the schema under output_name (the column's
                    ! display/file name -- equal to name unless a col_map:
                    ! entry renamed it), not the internal name, since that's
                    ! what append_column's own arguments must also use for
                    ! Arrow to line up the field with its data.
                    call parquet_column_output_name(eff_schema%cinfo%col(i), col_out_name)
                    call parquet_add_column_info(&
                        writer, &
                        col_out_name, &
                        eff_schema%cinfo%col(i)%unit, &
                        eff_schema%cinfo%col(i)%info, &
                        eff_schema%cinfo%col(i)%ucd, &
                        eff_schema%cinfo%col(i)%data_type, &
                        eff_schema%cinfo%col(i)%array_size, &
                        eff_schema%cinfo%col(i)%col_size )
                    ! A protected column may hold no Null at all (parquet_check_protected), so
                    ! its Arrow field is written NON-nullable. The C++ side is told here, once,
                    ! rather than at each write: for the mask-carrying kinds an all-.true. mask is
                    ! erased before the append anyway, but a temporal column and a
                    ! parquet_string_column carry their null state inside the element with no mask
                    ! to erase, and protection is the only signal that can make their fields
                    ! non-nullable. Registered under the OUTPUT name, like every other C++-side
                    ! column registration above.
                    if (eff_schema%cinfo%col(i)%is_protected) then
                        call parquet_writer_set_protected_column(writer%handle, trim(col_out_name)//char(0))
                    end if
                end if
            end do

            if (allocated(eff_schema%metadata%items)) then
                do i = 1, size(eff_schema%metadata%items)
                    call resolve_metadata_datatype(writer, eff_schema%metadata%items, i, dt)
                    call parquet_add_table_metadata(writer%handle, &
                        trim(eff_schema%metadata%items(i)%key)//char(0), &
                        trim(eff_schema%metadata%items(i)%value)//char(0), &
                        trim(eff_schema%metadata%items(i)%description)//char(0), &
                        trim(dt)//char(0))
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
                ! every route that populates eff_schema%cinfo%col (read just above, unconditionally,
                ! to populate writer%all_columns) also establishes
                ! eff_schema%metadata%source_maml_lines -- both parquet_parse_maml forms set it
                ! outright, and schema%init sets it before %add_field starts appending to it -- so
                ! by the time a schema safely reaches this point it is already allocated.
                !
                ! The sidecar file itself is not written here: a col_size:/array_size: auto field
                ! may still be unresolved at this point (resolved later, at first write, or via
                ! schema%set_col_size/%set_array_size) -- writing now could bake in a stale "auto"
                ! that no longer matches the .parquet file's actual columns. parquet_close_writer
                ! writes it instead, once every column is guaranteed resolved.
                writer%sidecar_lines = eff_schema%metadata%source_maml_lines
                call parquet_prune_disabled_fields(writer%sidecar_lines, eff_schema%cinfo)
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
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(lines(i), "fields:")) then
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
        character(len=:), allocatable :: kind_word, size_reason
        logical, allocatable :: drop(:)
        logical :: is_container
        integer :: i, j, k, n, idx_fields, block_start, block_end, col_idx, newlen, keep
        character(len=32) :: numbuf

        if (size(all_columns) == 0) return

        n = size(lines)
        idx_fields = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(lines(i), "fields:")) then
                idx_fields = i
                exit
            end if
        end do
        if (idx_fields == 0) return

        allocate(drop(n), source=.false.)

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
                    ! A CONTAINER column's two size keys are DROPPED rather than rewritten. Both
                    ! declare a width that is the same in every row, which is exactly the property
                    ! a container does not have -- its rows carry their own lengths -- so neither
                    ! key means anything for one, and a `col_size: 1` a hand-written MAML happens
                    ! to carry would otherwise be copied into the sidecar and read as a real
                    ! declaration. Dropping them makes every sidecar this library writes agree
                    ! with the schema-less parquet_write_table path, which has never emitted
                    ! either key for a container (parquet_tables_write.f90), so "no col_size: key"
                    ! is a sound signal to a sidecar reader on both routes rather than only one.
                    ! See doc/pages/schema/maml-format.md's col_size/array_size callout.
                    call parquet_container_col_size_rule(all_columns(col_idx)%data_type, &
                        is_container, kind_word, size_reason)
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
                            if (is_container) then
                                drop(k) = .true.
                            else
                                write(numbuf, '(I0)') all_columns(col_idx)%col_size
                                new_text = "  col_size: " // trim(numbuf)
                                lines(k) = new_text
                            end if
                        else if (scratch_str == "array_size") then
                            if (is_container) then
                                drop(k) = .true.
                            else
                                write(numbuf, '(I0)') all_columns(col_idx)%array_size
                                new_text = "  array_size: " // trim(numbuf)
                                lines(k) = new_text
                            end if
                        end if
                    end do
                end if

                i = block_end + 1
                cycle
            end if

            i = i + 1
        end do

        ! Compact out whatever the container rule marked. Done in one pass at the end rather than
        ! by deleting as we go, because the walk above indexes `lines` by absolute position and
        ! removing an element underneath it would renumber every block boundary still to come.
        if (any(drop)) then
            keep = count(.not. drop)
            allocate(character(len=len(lines)) :: new_lines(keep))
            keep = 0
            do i = 1, n
                if (drop(i)) cycle
                keep = keep + 1
                new_lines(keep) = lines(i)
            end do
            call move_alloc(new_lines, lines)
        end if
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
    !> (see parquet_parse_protected_cols, or schema%set_protected) and `is_valid_flat` contains
    !> any .false. entry. A no-op for a schema-less writer or an unlisted column.
    !>
    !> Also reports, in the optional `protected`, whether this column is protected at all --
    !> which callers use to ERASE an all-.true. mask before writing. A protected column may hold
    !> no Null, so a mask that survived the check above says nothing; dropping it means no
    !> validity buffer is built and the column's Arrow field comes out non-nullable through the
    !> ordinary mask-presence rule, instead of needing a precedence rule between "protected" and
    !> "a mask was passed". Passing an all-.true. mask for a protected column is explicitly
    !> allowed -- declaring a column protected must not make the is_valid keyword unusable.
    subroutine parquet_check_protected(writer, name, is_valid_flat, protected)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! column name.
        logical, intent(in) :: is_valid_flat(:) !! flattened validity mask for this write call.
        logical, intent(out), optional :: protected !! .true. if this column is protected (mask may be erased).
        integer :: idx

        if (present(protected)) protected = .false.
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. writer%all_columns(idx)%is_protected) return
        if (present(protected)) protected = .true.
        if (.not. all(is_valid_flat)) then
            error stop "parquet_write_column: column '" // trim(name) // &
                "' is protected (extra: protected_cols:) and cannot contain Null values"
        end if
    end subroutine parquet_check_protected
    !> If writer%qc is set and the column declares an EXPLICIT, EMPTY qc: miss: -- the one form that
    !! asks for Null validation, see parquet_column_type%qc_allow_null -- prints a WARNING naming the
    !! column and how many of its elements are Null in this write call. Never errors: writing proceeds
    !! regardless, which is a deliberate write-side design choice (the read side escalates per
    !! qc_soft), and qc_min/qc_max are unaffected (they already only ever look at valid elements).
    !! No-op for a schema-less writer, an undeclared column, a column whose qc: miss: says Null/NA, and
    !! -- the common case -- a column declaring no qc: miss: at all.
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
        call parquet_emit_warning("qc violation for column '" // trim(name) // "': " // fmt_int // " of " // &
            fmt_int2 // " element(s) are Null (qc: miss: is declared empty, so Nulls are not expected here)")
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
    !> Also reports whether a row mask applies to this chunk (`masked`) and, when one does,
    !> returns it in `row_mask`; `row_mask` is left UNALLOCATED otherwise, so an unmasked chunked
    !> write allocates and packs nothing. Both masking routes count: a whole-file mask
    !> (parquet_write_row_mask, whose per-row-group window parquet_new_row_group_impl has already
    !> sliced into writer%chunk_mask) and a per-row-group one (parquet_write_chunk_row_mask).
    !> They are mutually exclusive by parquet_write_chunk_row_mask's own guard.
    !>
    !> Enforces parquet_write_chunk_row_mask's all-or-nothing-per-writer rule too:
    !> writer%chunk_mask_scheme is fixed (0 -> 1 or 2) at the first parquet_write_column_chunk
    !> call of the writer's first row group, and any later row group must then agree with that
    !> decision. This subroutine therefore must still be CALLED on every chunk write, unmasked or
    !> not -- it also validates the chunk's row count and opens the underlying C++ row group. Only
    !> the mask itself is skippable.
    subroutine parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        type(parquet_writer), intent(inout) :: writer !! open writer, expected to have a row group open.
        character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
        integer(c_long_long), intent(in) :: nrows !! row count of this chunk's own values.
        logical, allocatable, intent(out) :: row_mask(:) !! the `nrows`-long row-keep mask; unallocated if .not. masked.
        logical, intent(out) :: masked !! .true. if a row mask applies to this chunk (either scheme).
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
        masked = allocated(writer%file_mask) .or. writer%chunk_mask_set_this_group
        if (masked) row_mask = writer%chunk_mask
    end subroutine parquet_check_row_group_row_count
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
    !> generic interface in parquet_core.f90. Also resets the current row group's masking state and,
    !> for a writer using the shared whole-file mask (parquet_write_row_mask) together with row
    !> groups, claims this row group's window (the next `nrows` positions of the stored mask) --
    !> see "Chunked masking mechanics -- shared whole-file mask" in feature_write_mask.md.
    subroutine parquet_new_row_group_impl(writer, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        integer(c_long_long), intent(in) :: nrows !! row count for the new row group; must be positive.
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).

        call check_writer_open(writer)
        call lk%claim(writer)
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
            ! this one. Leave chunk_mask UNALLOCATED, meaning "no mask for this row group unless
            ! parquet_write_chunk_row_mask says otherwise", and defer the actual C++ open to
            ! whichever comes first of parquet_write_chunk_row_mask_impl or
            ! parquet_check_row_group_row_count (this row group's first parquet_write_column_chunk
            ! call) -- both commit using the by-then-known kept count.
            !
            ! This used to store an all-.true. identity mask of `nrows` instead, which every
            ! chunk write of the row group then packed through to remove nothing.
            !
            ! The deallocate is DEFENSIVE, not load-bearing: parquet_check_row_group_row_count
            ! decides whether a mask applies from file_mask/chunk_mask_set_this_group, never from
            ! allocated(chunk_mask), so a previous row group's mask left lying here could not be
            ! read anyway (confirmed by mutation: removing this line changes no test result).
            ! It is kept so that the state matches what it claims -- a predicate keyed on
            ! allocated(chunk_mask) is the obvious thing for a later change to reach for, and it
            ! would be silently wrong if a stale mask survived here.
            if (allocated(writer%chunk_mask)) deallocate(writer%chunk_mask)
        end if
    end subroutine parquet_new_row_group_impl
    module procedure parquet_new_row_group_int32
        call parquet_new_row_group_impl(writer, int(nrows, kind=c_long_long))
    end procedure parquet_new_row_group_int32
    module procedure parquet_new_row_group_int64
        call parquet_new_row_group_impl(writer, nrows)
    end procedure parquet_new_row_group_int64
    module procedure parquet_finish_row_group
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
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
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        chunk_size = int(parquet_writer_get_chunk_size(writer%handle), kind=int32)
    end procedure parquet_get_chunk_size_writer_int32
    module procedure parquet_get_chunk_size_writer_int64
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)
        chunk_size = parquet_writer_get_chunk_size(writer%handle)
    end procedure parquet_get_chunk_size_writer_int64
    !> Writes every enabled column as a ZERO-ROW column when parquet_close_writer finds that a
    !> schema-enforced writer had nothing written to it at all -- an analysis stage that legitimately
    !> produced no rows, which used to abort with "missing write for enabled column".
    !>
    !> The point of doing it HERE, through the ordinary write specifics, is that no guard has to
    !> learn an exception: parquet_mark_column_written increments write_counts and the C++ side
    !> receives a real (empty) array, so parquet_close_writer's own missing-write check and
    !> close_parquet_writer's "Missing column data before close" both pass on their existing terms.
    !> Synthesizing empty arrays anywhere else would mean a second implementation of "append a
    !> column of type T", which could drift from the real one.
    !>
    !> The resulting file carries the schema in full -- every declared column, its unit/info/ucd and
    !> the table metadata -- with 0 rows, so a reader finds the columns it expects rather than the
    !> 0-column file a schema-less writer produces. That is exactly what a caller writing zero-length
    !> arrays by hand already got, which is why this is expressed as "as if they had".
    !>
    !> Deliberately does NOTHING in four cases, each meaning rows were expected or none were
    !> promised: any write already happened (`write_started`, which parquet_new_row_group sets too,
    !> so an opened row group counts); a row mask was set; the writer is schema-less; or no column
    !> is enabled. The last two already produce a valid 0-column file.
    subroutine parquet_write_empty_columns_if_none_written(writer)
        type(parquet_writer), intent(inout) :: writer !! writer being closed with nothing written.
        integer :: i, ncols
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: cname !! this column's name, copied out of `writer`.
        character(len=:), allocatable :: dtype !! this column's declared data_type.
        !> Zero-sized actuals, one per declared type. A rank-1 zero-length array serves a vector
        !! column too: the specifics check `mod(size(values), col_size) == 0`, which 0 satisfies for
        !! any width, and derive nrows = 0 from it.
        integer(int32) :: empty_i32(0)
        integer(int64) :: empty_i64(0)
        real(real32) :: empty_f32(0)
        real(real64) :: empty_f64(0)
        logical :: empty_bool(0)
        character(len=1) :: empty_str(0)
        type(parquet_date) :: empty_date(0)
        type(parquet_time) :: empty_time(0)
        type(parquet_timestamp) :: empty_ts(0)
        character(len=:), allocatable :: elem_base !! parquet_parse_list_type/_map_type scratch.
        integer :: elem_unit
        logical :: elem_utc, is_list, list_ok, is_map, map_ok

        if (.not. writer%is_schema_enforced) return
        if (writer%write_started) return
        if (.not. allocated(writer%enabled_columns)) return
        if (allocated(writer%file_mask)) return

        ! An unresolved col_size:/array_size: auto has no data to be resolved from, and every value
        ! is vacuously consistent with a width when there are no values -- so resolve to 1, which is
        ! also the only choice that keeps a write_maml=.true. sidecar valid (parquet_validate_maml
        ! requires a positive integer, and schema%set_col_size refuses anything below 1).
        do i = 1, size(writer%enabled_columns)
            if (writer%enabled_columns(i)%col_size == parquet_size_auto) writer%enabled_columns(i)%col_size = 1
            if (writer%enabled_columns(i)%array_size == parquet_size_auto) writer%enabled_columns(i)%array_size = 1
        end do
        do i = 1, size(writer%all_columns)
            if (writer%all_columns(i)%col_size == parquet_size_auto) writer%all_columns(i)%col_size = 1
            if (writer%all_columns(i)%array_size == parquet_size_auto) writer%all_columns(i)%array_size = 1
        end do

        call writer_context_suffix(writer, ctx)
        call parquet_emit_warning("parquet_close_writer: no column was written; writing every declared " // &
            "column with 0 rows" // ctx)

        ncols = size(writer%enabled_columns)
        do i = 1, ncols
            ! Copied out first: `writer` and a component of `writer` may not both be actual
            ! arguments of one call, since the callee defines `writer` (F2018 15.5.2.13).
            cname = trim(writer%enabled_columns(i)%name)
            dtype = trim(writer%enabled_columns(i)%data_type)
            ! A list column is handled before the select case: its token carries an element type
            ! (list[int32], list[timestamp[ms]]) so it cannot be matched by a fixed case list, and
            ! the zero-row column has to be %init'd to that element kind before it can be written.
            call parquet_parse_list_type(dtype, elem_base, elem_unit, elem_utc, is_list, list_ok)
            if (is_list) then
                if (.not. list_ok) then
                    ! Unreachable: parquet_validate_maml rejects a malformed list token before a
                    ! schema can reach a writer. GCOVR_EXCL_LINE
                    error stop "parquet_close_writer: cannot write an empty column of data_type '" // &
                        dtype // "' for column " // cname // ctx ! GCOVR_EXCL_LINE
                end if
                ! Delegated to parquet_write_list rather than built here, and that is NOT a filing
                ! preference: this file is an intermediate submodule, and nagfor 7.2 cannot compile
                ! ANY of its sibling descendants once its own code references a PK_* constant
                ! use-associated into parquet_core from parquet_columns ("Bad module file format
                ! for PARQUET_WRITE, could not ref PK_INT32"). Keeping every PK_* reference in the
                ! leaf that needs it is what the read side already does.
                call parquet_write_empty_list_column(writer, cname, elem_base)
                cycle
            end if
            ! A map column is handled the same way and for the same two reasons: its token carries
            ! a value type so no fixed case list can match it, and the zero-row column has to be
            ! %init'd to that value kind before it can be written.
            call parquet_parse_map_type(dtype, elem_base, elem_unit, elem_utc, is_map, map_ok)
            if (is_map) then
                if (.not. map_ok) then
                    ! Unreachable: parquet_validate_maml rejects a malformed map token before a
                    ! schema can reach a writer. GCOVR_EXCL_LINE
                    error stop "parquet_close_writer: cannot write an empty column of data_type '" // &
                        dtype // "' for column " // cname // ctx ! GCOVR_EXCL_LINE
                end if
                call parquet_write_empty_map_column(writer, cname, elem_base)
                cycle
            end if
            select case (dtype)
            case ("int32")
                call parquet_write_int32_column(writer, cname, empty_i32)
            case ("int64")
                call parquet_write_int64_column(writer, cname, empty_i64)
            case ("float32")
                call parquet_write_float32_column(writer, cname, empty_f32)
            case ("float64")
                call parquet_write_float64_column(writer, cname, empty_f64)
            case ("boolean", "bool8")
                call parquet_write_logical_column(writer, cname, empty_bool)
            case ("string")
                call parquet_write_string_column(writer, cname, empty_str)
            case ("date")
                call parquet_write_date_column(writer, cname, empty_date)
            case ("time")
                call parquet_write_time_column(writer, cname, empty_time)
            case ("timestamp")
                call parquet_write_timestamp_column(writer, cname, empty_ts)
            case default
                ! Unreachable through the public API: parquet_validate_maml rejects any other
                ! data_type before a schema can reach a writer. GCOVR_EXCL_LINE
                !
                ! gcov attribution artifact: gfortran gives this statement no entry for its first
                ! line and a positive count for its CONTINUATION, so the excluded line below is
                ! expected to show hits even though the arm never runs -- if it ever did, the
                ! error stop would take the whole suite down with it.
                error stop "parquet_close_writer: cannot write an empty column of data_type '" // &
                    dtype // "' for column " // cname // ctx ! GCOVR_EXCL_LINE
            end select
        end do
    end subroutine parquet_write_empty_columns_if_none_written
    module procedure parquet_close_writer
        integer :: i
        character(len=32) :: cursor_str, mask_str
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.

        if (.not. c_associated(writer%handle)) then
            error stop "parquet_close_writer: writer has not been opened, or was already closed"
        end if

        call parquet_write_empty_columns_if_none_written(writer)

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
                    ! Filename/schema name go on their own lines rather than into the error stop
                    ! text, to keep that text short -- so they are ERROR CONTEXT, not warnings, and
                    ! go through the channel that is never suppressed and never redirected. Routing
                    ! them through parquet_emit_warning instead would let verbosity="errors_only"
                    ! produce an abort that names no file at all.
                    if (allocated(writer%filename)) call parquet_emit_error_context( &
                        "parquet_close_writer: output file: " // trim(writer%filename))
                    if (allocated(writer%maml_name)) then
                        call parquet_emit_error_context("parquet_close_writer: schema: " // trim(writer%maml_name))
                    else
                        call parquet_emit_error_context("parquet_close_writer: schema: (unnamed, built in-memory)")
                    end if
                    error stop "parquet_close_writer: missing write for enabled column: " // trim(writer%enabled_columns(i)%name)
                end if
            end do
        end if

        call parquet_reconcile_string_sizes(writer)

        call close_parquet_writer(writer%handle)

        if (writer%write_maml_requested) then
            call parquet_rewrite_resolved_sizes(writer%sidecar_lines, writer%all_columns)
            call parquet_write_maml_sidecar(writer%filename, writer%sidecar_lines)
            deallocate(writer%sidecar_lines)
            writer%write_maml_requested = .false.
        end if

        writer%handle = c_null_ptr
        if (allocated(writer%all_columns)) deallocate(writer%all_columns)
        if (allocated(writer%observed_string_len)) deallocate(writer%observed_string_len)
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

end submodule parquet_write
