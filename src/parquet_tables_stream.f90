!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The output file that stays open: `parquet_table_writer`.
!!
!! A thin object over two things this library already has: a `parquet_table` used as a row
!! buffer, grown by `%append`'s own rules, and `parquet_write_table_chunk`, which writes that
!! buffer as one row group whenever it holds at least `chunk_size` rows. The sink adds the
!! threshold, the reset-and-reserve after each flush, the final row group at close, and the
!! guards -- and nothing else: what a buffer can hold, what an append refuses and how a row group
!! is written are decided where they already were.
!!
!! **The buffer is reached through the table's public bindings, never by wrapping them in its
!! lock**: `table_lock` is an OpenMP simple lock, not a recursive one, so a sink that took the
!! buffer's lock and then called `%append` -- which takes it itself -- would deadlock rather than
!! fail to build. The sink needs no lock of its own because it refuses shared use outright
!! (`sink_check_not_shared`), on the same ownership test a shared table refuses a structural
!! change with.
submodule (parquet_tables) parquet_tables_stream
    implicit none
    !
    !> Message prefix for every abort raised here, so a caller can tell the sink from the table.
    character(len=*), parameter :: WP = "parquet_table_writer: "
    !
contains
    !
    !> Opens the sink -- see the interface's doc-comment for the contract.
    !!
    !! The order of the steps is the point: the column set is decided and checked against the
    !! template, the vector widths are resolved into the schema and the buffer is built BEFORE
    !! the writer is opened, so a template the schema does not fit is refused with no output
    !! file created, and the writer's own chunk-size estimate sees every width (contract 4:
    !! without it a vector column counts as width 1 and the estimate skips its own clamp). The
    !! open itself is `parquet_open_writer_like` with the resolved schema, which forwards
    !! every writer option still absent when the caller omitted it (`feature_risks.md`
    !! Risk-8).
    module procedure parquet_open_table_writer
        type(parquet_schema) :: own
        character(len=:), allocatable :: stem, fname, sfx, names(:)
        integer :: i, k, idx, nfields, nenabled, maxlen, cs, chunk
        !
        call table_check_open(template, "parquet_open_table_writer")
        if (present(chunk_size)) then
            if (chunk_size <= 0) then
                error stop WP // "parquet_open_table_writer: chunk_size must be positive (file: " // &
                    trim(filename) // ")"
            end if
        end if
        out%file = trim(filename)
        ! The column set is the schema's enabled fields: the caller's schema, or one derived
        ! from the template's resident columns and named after the output file's stem, as a
        ! schema-less parquet_write_table names it.
        if (present(schema)) then
            call ensure_schema_parsed(schema, "parquet_open_table_writer")
            own = schema
            out%from_schema = .true.
        else
            if (count_writable_resident(template) == 0) then
                call table_context_suffix(template%cache, "", sfx)
                error stop WP // "parquet_open_table_writer: the template table has no resident column, " // &
                    "so there is nothing to write; materialize the columns the output should have, " // &
                    "or pass schema=" // sfx
            end if
            call output_stem(trim(filename), stem)
            call parquet_derive_schema(template, own, name=stem)
            out%from_schema = .false.
        end if
        ! Every enabled field names a template column that holds values, checked field by field
        ! before the file exists (question 15); the buffer holds exactly those, in schema order.
        nfields = own%get_num_fields()
        nenabled = 0
        maxlen = 1
        do i = 1, nfields
            call own%get_field_name(i, fname)
            if (.not. own%is_column_set(fname)) cycle
            nenabled = nenabled + 1
            maxlen = max(maxlen, len_trim(fname))
        end do
        allocate(character(len=maxlen) :: names(nenabled))
        k = 0
        do i = 1, nfields
            call own%get_field_name(i, fname)
            if (.not. own%is_column_set(fname)) cycle
            k = k + 1
            names(k) = fname
            idx = table_find(template, fname)
            if (idx == 0) then
                call table_context_suffix(template%cache, fname, sfx)
                error stop WP // "parquet_open_table_writer: the schema declares a column the template " // &
                    "table does not have" // sfx
            end if
            if (.not. template%cache%cols(idx)%supported) then
                call table_context_suffix(template%cache, fname, sfx)
                error stop WP // "parquet_open_table_writer: the schema declares a column that holds no " // &
                    "values" // sfx
            end if
            ! A vector column's `auto` col_size is resolved from the template's width, so the
            ! chunk-size estimate below sees the real bytes per row; the writer would resolve the
            ! same value from the first chunk, so no output changes. A container has no col_size
            ! (its width lives in its own offsets), and a string column's array_size stays auto.
            call own%get_field(i, fname, col_size=cs)
            if (cs == parquet_size_auto) then
                if (template%cache%cols(idx)%width > 1) then
                    if (.not. parquet_kind_is_container(template%cache%cols(idx)%declared_kind)) then
                        call own%set_col_size(fname, template%cache%cols(idx)%width)
                    end if
                end if
            end if
        end do
        call table_clone_columns(template, out%buf, names, "parquet_open_table_writer")
        call parquet_open_writer_like(out%w, trim(filename), template, schema=own,                   &
            copy_metadata=copy_metadata, metadata_keys=metadata_keys, write_maml=write_maml, qc=qc, &
            compression=compression, compression_level=compression_level, chunk_size=chunk_size,   &
            use_threads=use_threads, overwrite=overwrite)
        ! The flush threshold is the writer's answer: the caller's chunk_size=, validated against
        ! every vector column, or the schema-based estimate.
        call parquet_get_chunk_size(out%w, chunk)
        out%chunk_rows = chunk
        ! Reserved now and after every flush (contract 7), so no append between two flushes
        ! reallocates a column's rows.
        call out%buf%reserve(int(chunk, int64))
        out%opened = .true.
    end procedure parquet_open_table_writer
    !
    !> How many of `t`'s columns a schema-less write would take: resident, supported, and not
    !! the row index -- `build_table_schema`'s rule, asked once before deriving so that the
    !! refusal names this call rather than `parquet_derive_schema`.
    integer function count_writable_resident(t) result(n)
        class(parquet_table), intent(in) :: t !! the template.
        integer :: i
        !
        n = 0
        do i = 1, t%cache%ncols
            associate (slot => t%cache%cols(i))
                if (slot%residency /= RES_FULL) cycle
                if (.not. slot%supported) cycle
                if (slot%name == PARQUET_ROW_INDEX) cycle
                n = n + 1
            end associate
        end do
    end function count_writable_resident
    !
    !> `%append(table)` -- see the interface and the type's own note.
    !!
    !! Three steps around the buffer's own append. First the refusal of a resident column the
    !! output does not declare, with the sink's message rather than the table's, when the columns
    !! came from the template (with `schema=` the schema is the selection and the buffer's append
    !! is told to skip such a column). Then the read of every declared column the table has but
    !! has not read -- the output naming it is the request, as for `parquet_write_table` -- and,
    !! after the copy, the release of exactly what this append read, so the caller's table is
    !! left in the residency state it arrived in. Last the flush, if the buffer now holds a row
    !! group's worth.
    module procedure sink_append_table
        logical, allocatable :: read_here(:)
        integer :: j
        !
        call sink_check_usable(self, "append")
        call sink_check_not_shared(self, "append")
        call table_check_open(table, "append")
        if (.not. self%from_schema) call sink_refuse_extras(self, table%cache)
        allocate(read_here(max(table%cache%ncols, 1)))
        read_here = .false.
        if (table%nrows() > 0_int64) then
            call sink_read_declared(self, table%cache, table_scope_of(table), read_here)
        end if
        call table_append_table_ext(self%buf, table, self%from_schema)
        do j = 1, table%cache%ncols
            if (read_here(j)) call release_written_column(table%cache, j)
        end do
        if (self%buf%nrows() >= int(self%chunk_rows, int64)) call sink_flush_worker(self)
    end procedure sink_append_table
    !
    !> `%append(row)` -- see the interface. The same steps as the table form, except that a
    !! declared column the row's table has not read is read and KEPT resident: a loop appending
    !! one table's rows one at a time would otherwise read it from the file once per row.
    module procedure sink_append_row
        logical, allocatable :: read_here(:)
        !
        call sink_check_usable(self, "append")
        call sink_check_not_shared(self, "append")
        ! A stale or detached handle gets the row append's own diagnostics before anything
        ! here reaches through it.
        if (.not. row%is_valid()) call row_check_current(row, "append")
        if (.not. self%from_schema) call sink_refuse_extras(self, row%cache)
        allocate(read_here(max(row%cache%ncols, 1)))
        read_here = .false.
        call sink_read_declared(self, row%cache, row%scope, read_here)
        call table_append_row_ext(self%buf, row, self%from_schema)
        if (self%buf%nrows() >= int(self%chunk_rows, int64)) call sink_flush_worker(self)
    end procedure sink_append_row
    !
    !> `%flush()` -- see the interface.
    module procedure sink_flush
        call sink_check_usable(self, "flush")
        call sink_check_not_shared(self, "flush")
        call sink_flush_worker(self)
    end procedure sink_flush
    !
    !> Closes the sink -- see the interface.
    module procedure parquet_close_table_writer
        call sink_check_usable(out, "parquet_close_table_writer")
        call sink_check_not_shared(out, "parquet_close_table_writer")
        call sink_flush_worker(out)
        call parquet_close_writer(out%w)
        out%closed = .true.
    end procedure parquet_close_table_writer
    !
    module procedure sink_nrows
        call sink_check_usable(self, "nrows")
        n = self%rows_written + self%buf%nrows()
    end procedure sink_nrows
    !
    module procedure sink_rows_pending
        call sink_check_usable(self, "rows_pending")
        n = self%buf%nrows()
    end procedure sink_rows_pending
    !
    module procedure sink_row_groups
        call sink_check_usable(self, "row_groups")
        n = self%groups_written
    end procedure sink_row_groups
    !
    module procedure sink_chunk_size
        call sink_check_usable(self, "chunk_size")
        n = self%chunk_rows
    end procedure sink_chunk_size
    !
    module procedure sink_filename
        call sink_check_usable(self, "filename")
        name = self%file
    end procedure sink_filename
    !
    module procedure sink_is_open
        ok = self%opened .and. .not. self%closed
    end procedure sink_is_open
    !
    module procedure sink_assign_guard
        ! Deliberately unconditional, as parquet_table's own guard is: a shallow copy would leave
        ! two sinks sharing (and double-closing) one file, and there is no deep copy to offer.
        error stop WP // "assignment is not supported (it would leave two writers sharing one " // &
            "output file); open a second output with parquet_open_table_writer"
    end procedure sink_assign_guard
    !
    module procedure parquet_debug_table_writer_capacity
        integer :: i
        !
        cap = 0_int64
        if (.not. out%opened) return
        if (out%buf%cache%ncols == 0) return
        cap = huge(0_int64)
        do i = 1, out%buf%cache%ncols
            cap = min(cap, out%buf%cache%cols(i)%values%capacity())
        end do
    end procedure parquet_debug_table_writer_capacity
    !
    !> Writes the pending rows as one row group and resets the buffer; nothing when none are
    !! pending. The reset is `%truncate(0)` -- which rebuilds every column exact-fit, at zero
    !! capacity -- followed by `%reserve(chunk_size)`, so the next row group's appends allocate
    !! nothing (contract 7; `feature_risks.md` Risk-227 is what dropping the reserve costs).
    subroutine sink_flush_worker(self)
        class(parquet_table_writer), intent(inout) :: self !! the sink.
        integer(int64) :: n
        !
        n = self%buf%nrows()
        if (n == 0_int64) return
        call parquet_write_table_chunk(self%w, self%buf)
        self%rows_written = self%rows_written + n
        self%groups_written = self%groups_written + 1_int64
        call self%buf%truncate(0_int64)
        call self%buf%reserve(int(self%chunk_rows, int64))
    end subroutine sink_flush_worker
    !
    !> Reads every column the output declares that `cache` holds but has not read, marking the
    !! slots it read in `read_here` so the table form can give them back afterwards. An
    !! unsupported column is left alone: the buffer's append null-fills it, as it does for any
    !! column it cannot read values out of.
    subroutine sink_read_declared(self, cache, sc, read_here)
        class(parquet_table_writer), intent(in) :: self  !! the sink.
        type(parquet_table_cache), intent(inout) :: cache !! the appended table's column store.
        type(table_scope), intent(in) :: sc              !! its row scope.
        logical, intent(inout) :: read_here(:)           !! set .true. for every slot read here.
        integer :: i, j
        !
        do i = 1, self%buf%cache%ncols
            j = cache_find(cache, self%buf%cache%cols(i)%name)
            if (j == 0) cycle
            if (cache%cols(j)%residency == RES_FULL) cycle
            if (.not. cache%cols(j)%supported) cycle
            call table_touch(cache, sc, j, "append")
            read_here(j) = .true.
        end do
    end subroutine sink_read_declared
    !
    !> Refuses a resident column of `cache` the output does not declare, naming the file: with
    !! the columns fixed from the template, silently dropping data is the worse answer. The row
    !! index is exempt (the buffer's append drops it), and so is a column that was never read,
    !! which `%append` already ignores.
    subroutine sink_refuse_extras(self, cache)
        class(parquet_table_writer), intent(in) :: self !! the sink.
        type(parquet_table_cache), intent(in) :: cache  !! the appended table's column store.
        character(len=:), allocatable :: sfx
        integer :: j
        !
        do j = 1, cache%ncols
            if (.not. cache%cols(j)%supported) cycle
            if (cache%cols(j)%residency /= RES_FULL) cycle
            if (cache%cols(j)%name == PARQUET_ROW_INDEX) cycle
            if (cache_find(self%buf%cache, cache%cols(j)%name) > 0) cycle
            call sink_suffix(self, sfx)
            error stop WP // "append: this output file has no column '" // trim(cache%cols(j)%name) // &
                "' -- the output's columns were fixed when it was opened, from the template table" // sfx
        end do
    end subroutine sink_refuse_extras
    !
    !> Aborts unless the sink is open and not yet closed; `proc` is the binding's name.
    subroutine sink_check_usable(self, proc)
        class(parquet_table_writer), intent(in) :: self !! the sink.
        character(len=*), intent(in) :: proc            !! calling binding, for the message.
        character(len=:), allocatable :: sfx
        !
        if (.not. self%opened) then
            error stop WP // trim(proc) // ": this output file has not been opened; call " // &
                "parquet_open_table_writer first"
        end if
        if (self%closed) then
            call sink_suffix(self, sfx)
            error stop WP // trim(proc) // ": this output file has already been closed" // sfx
        end if
    end subroutine sink_check_usable
    !
    !> Refuses a sink another thread may share, on the ownership test every structural change
    !! to a table keys on (`unsafe_shared_mutation`): a sink this thread opened inside the
    !! current parallel region is its own, anything else may be shared. Called directly rather
    !! than through `table_check_not_shared`, whose message is about a table.
    subroutine sink_check_not_shared(self, proc)
        class(parquet_table_writer), intent(in) :: self !! the sink.
        character(len=*), intent(in) :: proc            !! calling binding, for the message.
        character(len=:), allocatable :: sfx
        !
        if (.not. unsafe_shared_mutation(self%buf%cache)) return
        call sink_suffix(self, sfx)
        error stop WP // trim(proc) // ": this output file is being used from more than one thread; " // &
            "a parquet file is written by one thread in row-group order -- open one output per " // &
            "thread" // sfx
    end subroutine sink_check_not_shared
    !
    !> The " (file: ...)" suffix every message about an open sink carries.
    subroutine sink_suffix(self, sfx)
        class(parquet_table_writer), intent(in) :: self  !! the sink.
        character(len=:), allocatable, intent(out) :: sfx !! receives the suffix.
        !
        sfx = " (file: " // self%file // ")"
    end subroutine sink_suffix
    !
end submodule parquet_tables_stream
