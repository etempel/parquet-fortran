!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Writing a `parquet_table` back out to a parquet file.
!!
!! Deliberately thin. The schema decides which columns are written, in what order, and under
!! what output names; the existing writer decides everything else -- type agreement, QC, row
!! groups, compression -- so this file adds no validation of its own beyond "the schema names a
!! column the table does not have", the chunk write's zero-row refusal, and the one rule the
!! chunk write adds (its always-present validity mask, explained at `write_one_column`). Reusing
!! `parquet_open_writer`/`parquet_write_column` rather than reimplementing them is what keeps a
!! table write and a hand-written write path identical in behaviour.
!!
!! **A schema-less write BUILDS a schema rather than taking a second path.** `build_table_schema`
!! turns the resident columns' descriptors into an ordinary `parquet_schema` and everything below
!! proceeds as it always did. Two things fall out of that choice and are the reason for it: the
!! sidecar MAML costs nothing (the writer already emits one from whatever schema it was given),
!! and there is exactly one write loop to keep correct rather than two that can drift. The
!! generated schema declares `col_size:`/`array_size:` as `auto`, so the writer resolves them from
!! the data exactly as it would with no schema at all -- and because the sidecar is emitted at
!! CLOSE, it records the resolved values rather than `auto`.
!!
!! **A row-group-at-a-time write (`parquet_write_table_chunk`) drives the SAME dispatch.**
!! `write_one_column` takes a `chunked` flag and each kind's arm chooses between
!! `parquet_write_column` and `parquet_write_column_chunk`; a second copy of the dispatch would be
!! free to drift from the first. The one deliberate difference between the two paths -- the chunked
!! one passes a validity mask whether or not the column holds a Null -- is explained there.
submodule (parquet_tables) parquet_tables_write
    implicit none
    !
contains
    !
    !> Parses a caller-supplied schema that was built with `%init`/`%add_field` and never parsed,
    !! and refuses one that was never built at all; `context` is the public procedure's name.
    !!
    !! `%get_num_fields` on an unpopulated `%cinfo` reads uninitialized state, which turns the
    !! write loop into a runaway allocation and an OOM kill rather than any kind of diagnosable
    !! failure. So a schema that has not been parsed is stopped here, one way or the other.
    !!
    !! **Which schema lands in which arm follows from what the two queries actually read** --
    !! `%is_parsed()` is `allocated(%cinfo%col)` and `%is_init()` is that OR the `%init` flag:
    !!
    !! * built with `%init`/`%add_field` -- `%add_field` parses as it goes, so `%is_parsed()` is
    !!   already `.true.` after the first field and neither arm runs;
    !! * from `parquet_parse_maml`, `parquet_load_maml_file` or an embedded `get_parquet_maml` --
    !!   parsed on arrival, likewise neither arm;
    !! * `%maml%lines` assigned DIRECTLY and never parsed -- `%init` was never called, so
    !!   `%is_init()` is `.false.` and this is the error stop below, not the parse;
    !! * `%init` called and no field added yet -- `%is_init()` `.true.`, `%is_parsed()` `.false.`,
    !!   which is the ONE state reaching the parse call.
    !!
    !! That last one is header-only MAML, so the parse always fails validation with "no fields
    !! defined" naming the schema. Keeping the call rather than adding a second bespoke guard is
    !! deliberate: the parser's own message is the accurate one, and the call is also what makes
    !! the callers' `schema` `intent(inout)` rather than `intent(in)`.
    !!
    !! A schema that was never built at all is a different mistake and still an error: parsing
    !! empty MAML text would report something about the text rather than the call.
    module procedure ensure_schema_parsed
        !
        if (.not. schema%is_parsed()) then
            if (.not. schema%is_init()) then
                error stop EP // context // ": this schema has not been built; call " // &
                    "schema%init/%add_field (or load a MAML file) before using it here"
            end if
            call parquet_parse_maml(schema)
        end if
    end procedure ensure_schema_parsed
    !
    !> The output file's stem -- its basename with any directory part and a trailing ".parquet"
    !! removed -- used as the generated schema's `table:` name, which MAML requires.
    module procedure output_stem
        integer :: i, first
        !
        first = 1
        do i = len_trim(filename), 1, -1
            if (filename(i:i) == "/" .or. filename(i:i) == "\") then
                first = i + 1
                exit
            end if
        end do
        stem = trim(filename(first:))
        if (len(stem) > 8) then
            if (stem(len(stem)-7:) == ".parquet") stem = stem(1:len(stem)-8)
        end if
        ! A path that is nothing but a directory separator or an extension would leave nothing to
        ! name the schema with, and MAML requires a table: value.
        if (len_trim(stem) == 0) stem = "table"
    end procedure output_stem
    !
    !> Gives back a column this write had to materialize, leaving the descriptor alone so the
    !! slot stays listed, queryable and re-readable -- `%evict_column`'s body, without its checks.
    !!
    !! The checks are not needed and are deliberately not repeated: every caller runs this
    !! solely for a slot that was `RES_EMPTY` before `table_touch` and is `RES_FULL` after, and
    !! `table_touch` itself has already rejected a slot with no file column behind it and a table
    !! detached from its file. So there is no unreleasable case left to skip silently here -- a
    !! column that could not be released could not have been read either, and the write would
    !! have aborted before reaching this point.
    !!
    !! The generation counter advances because storage a `%col` pointer could alias really has
    !! been freed. No pointer a caller can hold is ever affected (taking one materializes the
    !! column, which puts it outside the released set), so the signal is conservative rather than
    !! precise -- and it does not move at all when nothing was released.
    module procedure release_written_column
        !
        call cache%cols(idx)%values%clear()
        cache%cols(idx)%residency = RES_EMPTY
        cache%cols(idx)%user_populated = .false.
        cache%generation = cache%generation + 1_int64
    end procedure release_written_column
    !
    module procedure parquet_write_table
        type(parquet_schema) :: own
        integer :: nfields
        logical :: want_metadata, use_own
        character(len=:), allocatable :: stem
        !
        call table_check_open(table, "parquet_write_table")
        ! release= evicts columns as it writes, which replaces their storage -- so a write is a
        ! structural change to the table as far as another thread is concerned, not a read.
        call table_check_not_shared(table, "parquet_write_table")
        if (present(schema)) call ensure_schema_parsed(schema, "parquet_write_table")
        call resolve_metadata_request(copy_metadata, metadata_keys, "parquet_write_table", want_metadata)
        call check_row_index_request(table, present(schema), "parquet_write_table", row_index_name)
        ! `own` is used in two unrelated situations, and only one of them existed before: a
        ! schema-less write has to BUILD a schema, and a metadata carry-over has to write onto a
        ! COPY of the caller's rather than the caller's own -- otherwise writing a second table
        ! with the same schema would inherit the first table's source-file metadata, silently and
        ! permanently. Both end up wanting a local schema, so they share one.
        use_own = want_metadata .or. .not. present(schema)
        if (.not. present(schema)) then
            call output_stem(trim(filename), stem)
            call build_table_schema(table, stem, own, nfields, row_index_name)
            ! Nothing resident is not an error -- it writes a genuinely empty file, which Arrow
            ! accepts and this library reopens as a 0-column, 0-row table. It cannot go through
            ! the generated schema, though: MAML requires at least one field, so there is no
            ! schema to build and the bare writer does the whole job. `row_index_name=` puts a
            ! field in, so a table with nothing resident then takes the ordinary path and writes
            ! a one-column file of row numbers rather than an empty one.
            if (nfields == 0) then
                call write_empty_file(trim(filename), write_maml, compression, compression_level, &
                    chunk_size, use_threads, overwrite)
                return
            end if
        else if (want_metadata) then
            own = schema
        end if
        if (want_metadata) call carry_source_metadata(table, own, metadata_keys, "parquet_write_table")
        !
        ! row_index_name is forwarded on both arms although only the first can carry it (it and
        ! schema= are mutually exclusive, refused above), so the two calls stay one call written
        ! twice rather than two that could drift.
        if (use_own) then
            call write_through_schema(table, own, filename, row_mask, write_maml, qc,         &
                compression, compression_level, chunk_size, use_threads, overwrite, release,  &
                row_index_name)
        else
            call write_through_schema(table, schema, filename, row_mask, write_maml, qc,      &
                compression, compression_level, chunk_size, use_threads, overwrite, release,  &
                row_index_name)
        end if
    end procedure parquet_write_table
    !
    !> Hands back the schema a schema-less `parquet_write_table` would build for `table` -- see
    !! the interface's doc-comment for what goes in and what never does.
    !!
    !! A thin wrapper over `build_table_schema`, deliberately: that procedure is the one place
    !! the descriptor-to-field rules live (`feature_risks.md` Risk-2 pins its `auto` sizes with a
    !! lint check), and a second copy here would be free to drift from the write path. What this
    !! adds is the `table:` name rule and the refusal: the schema-less write's answer to "nothing
    !! resident" is an empty file, which is a valid output, while a schema with no field is not
    !! a valid schema, so there is nothing to hand back and the caller is told so.
    module procedure parquet_derive_schema
        integer :: nfields
        character(len=:), allocatable :: tname, sfx
        !
        call table_check_open(table, "parquet_derive_schema")
        call check_row_index_request(table, .false., "parquet_derive_schema", row_index_name)
        if (present(name)) then
            ! Refused here rather than left to %init, whose message would name a procedure the
            ! caller never called. MAML requires a table: value.
            if (len_trim(name) == 0) then
                call table_context_suffix(table%cache, "", sfx)
                error stop EP // "parquet_derive_schema: name= must not be blank (MAML requires a " // &
                    "table: value)" // sfx
            end if
            tname = trim(name)
        else
            call source_stem(table, tname)
        end if
        call build_table_schema(table, tname, schema, nfields, row_index_name)
        if (nfields == 0) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // "parquet_derive_schema: this table has no resident column to build a " // &
                "schema from; materialize a column first" // sfx
        end if
    end procedure parquet_derive_schema
    !
    !> `parquet_derive_schema` plus `parquet_open_writer` -- see the interface's doc-comment.
    !!
    !! The body is `parquet_write_table`'s own opening half, with the write loop and the close
    !! left to the caller: the same parse guard, the same metadata rules on the same private copy,
    !! the same forwarding of every writer option still absent when the caller omitted it. Kept
    !! in step with it by sharing the helpers (`ensure_schema_parsed`, `resolve_metadata_request`,
    !! `build_table_schema`, `carry_source_metadata`) rather than by care.
    !!
    !! The derived schema takes the OUTPUT file's stem as its `table:` name, as the schema-less
    !! write does: this call names an output file, so the name a sidecar records should be that
    !! file's, not the template's source. `parquet_derive_schema` alone, which names no file,
    !! takes the source stem instead.
    module procedure parquet_open_writer_like
        type(parquet_schema) :: own
        integer :: nfields
        logical :: want_metadata, use_own
        character(len=:), allocatable :: stem, sfx
        !
        call table_check_open(table, "parquet_open_writer_like")
        if (present(schema)) call ensure_schema_parsed(schema, "parquet_open_writer_like")
        call resolve_metadata_request(copy_metadata, metadata_keys, "parquet_open_writer_like", want_metadata)
        use_own = want_metadata .or. .not. present(schema)
        if (.not. present(schema)) then
            call output_stem(trim(filename), stem)
            call build_table_schema(table, stem, own, nfields)
            ! Unlike the schema-less write, which has a valid empty file to fall back on, an open
            ! writer with no column is nothing a caller can use -- every write into it would be
            ! refused as undeclared -- so the refusal happens here, before any file exists.
            if (nfields == 0) then
                call table_context_suffix(table%cache, "", sfx)
                error stop EP // "parquet_open_writer_like: this table has no resident column to " // &
                    "derive a schema from; materialize the columns the output should have, or " // &
                    "pass schema=" // sfx
            end if
        else if (want_metadata) then
            own = schema
        end if
        if (want_metadata) call carry_source_metadata(table, own, metadata_keys, "parquet_open_writer_like")
        ! Every writer option is forwarded untouched, absent ones included (see
        ! write_through_schema for why that is the whole point).
        if (use_own) then
            call parquet_open_writer(writer, trim(filename), own, write_maml=write_maml, qc=qc,   &
                compression=compression, compression_level=compression_level,                     &
                chunk_size=chunk_size, use_threads=use_threads, overwrite=overwrite)
        else
            call parquet_open_writer(writer, trim(filename), schema, write_maml=write_maml, qc=qc, &
                compression=compression, compression_level=compression_level,                     &
                chunk_size=chunk_size, use_threads=use_threads, overwrite=overwrite)
        end if
    end procedure parquet_open_writer_like
    !
    !> Writes `table`'s rows as one complete row group of an open writer -- see the interface's
    !! doc-comment for the contract, and `write_one_column` for the one place this path
    !! deliberately differs from `parquet_write_table` (the always-present validity mask).
    !!
    !! The column set is the WRITER's, asked for through its public query rather than walked from
    !! a schema object this call never sees: every declared field in schema order, the disabled
    !! and deactivated ones skipped, each looked up in the table under its internal name exactly
    !! as `write_through_schema` does. A schema-less writer answers no names and gets the
    !! schema-less table write's own rule -- every resident column in slot order, the row index
    !! never -- through the same predicate `build_table_schema` uses, so the two cannot disagree
    !! about what a schema-less write takes.
    !!
    !! Only the zero-row refusal is this procedure's own. Every other failure is the writer's and
    !! keeps the writer's message, deliberately: a writer that is not open, a row group already
    !! open (the message names the call that has to come first), a mask whose size is not the row
    !! count, a mask introduced or dropped after the first row group, a column absent from the
    !! first row group.
    module procedure parquet_write_table_chunk
        character(len=:), allocatable :: names(:), fname, sfx
        integer :: i, idx
        integer(int64) :: nrows
        !
        call table_check_open(table, "parquet_write_table_chunk")
        nrows = table%nrows()
        if (nrows == 0_int64) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // "parquet_write_table_chunk: this table has no rows; a Parquet row group " // &
                "must hold at least one row (skip the call, or write nothing at all for an empty " // &
                "result)" // sfx
        end if
        ! The query doubles as the writer-open check, with the writer's own message.
        call parquet_get_column_names(writer, names)
        call parquet_new_row_group(writer, nrows)
        ! The per-row-group mask, under the writer's own rules (used on the first row group means
        ! used on every one; unavailable after a whole-column write; sized to the row count).
        if (present(row_mask)) call parquet_write_chunk_row_mask(writer, row_mask)
        if (size(names) > 0) then
            do i = 1, size(names)
                fname = trim(names(i))
                if (.not. parquet_is_column_enabled(writer, fname)) cycle
                call locate_column_to_write(table, fname, "parquet_write_table_chunk", idx)
                ! A column the caller never read is read here, as the whole-table write reads it:
                ! the schema naming it is the request. Nothing is released afterwards -- the next
                ! row group is another table, and this one's residency is the caller's to manage.
                call table_touch(table%cache, table_scope_of(table), idx, "parquet_write_table_chunk")
                call write_one_column(writer, table, idx, fname, chunked=.true.)
            end do
        else
            do idx = 1, table%cache%ncols
                if (.not. schemaless_writes_slot(table%cache%cols(idx))) cycle
                ! Copied out first: the name is reached through the table, and a component and
                ! its parent may not both be actual arguments of one call.
                fname = table%cache%cols(idx)%name
                call write_one_column(writer, table, idx, fname, chunked=.true.)
            end do
        end if
        call parquet_finish_row_group(writer)
    end procedure parquet_write_table_chunk
    !> Turns the `copy_metadata=`/`metadata_keys=` pair into one flag, refusing the combination
    !! that says two things at once; `context` is the public procedure's name.
    subroutine resolve_metadata_request(copy_metadata, metadata_keys, context, want_metadata)
        logical, intent(in), optional :: copy_metadata             !! .true.: carry every key.
        character(len=*), intent(in), optional :: metadata_keys(:) !! carry only these keys.
        character(len=*), intent(in) :: context                    !! calling procedure, for the message.
        logical, intent(out) :: want_metadata                      !! whether anything is to be carried.
        !
        want_metadata = present(metadata_keys)
        if (present(copy_metadata)) then
            if (present(metadata_keys) .and. copy_metadata) then
                error stop EP // context // ": copy_metadata=.true. and metadata_keys= " // &
                    "cannot both be given; copy_metadata=.true. carries every key, metadata_keys= " // &
                    "only the listed ones (copy_metadata=.false. alongside metadata_keys= is " // &
                    "accepted, and carries the listed keys)"
            end if
            want_metadata = want_metadata .or. copy_metadata
        end if
    end subroutine resolve_metadata_request
    !
    !> The `table:` name `parquet_derive_schema` uses when the caller gives none: the stem of the
    !! file the table was opened from, or "table" for one built in memory.
    subroutine source_stem(table, stem)
        class(parquet_table), intent(in) :: table          !! the table.
        character(len=:), allocatable, intent(out) :: stem !! the name, never empty.
        !
        stem = "table"
        if (.not. table%cache%file_backed) return
        if (.not. allocated(table%cache%source_file)) return
        call output_stem(trim(table%cache%source_file), stem)
    end subroutine source_stem
    !
    !> Opens the writer, writes every enabled column of `sch`, and closes -- the whole write, for
    !! whichever schema the caller's arguments resolved to.
    !!
    !! It exists as its own procedure only so that the caller's schema and a locally built one can
    !! share one body: Fortran has no way to bind a name to "whichever of these two objects", and
    !! duplicating an eleven-argument `parquet_open_writer` call plus the write loop is exactly the
    !! kind of pair that drifts.
    !!
    !! Every writer option is forwarded untouched, absent ones included: passing an absent optional
    !! dummy on as an actual argument leaves the callee's own dummy absent, so
    !! `parquet_open_writer` applies exactly the defaults it would for a hand-written open, and
    !! there is no second set of defaults here to drift from it.
    subroutine write_through_schema(table, sch, filename, row_mask, write_maml, qc,              &
            compression, compression_level, chunk_size, use_threads, overwrite, release,         &
            row_index_name)
        type(parquet_table), intent(in) :: table                   !! the table being written.
        type(parquet_schema), intent(in) :: sch                    !! schema that decides the output.
        character(len=*), intent(in) :: filename                   !! output parquet file.
        logical, intent(in), optional :: row_mask(:)               !! per-row write mask.
        logical, intent(in), optional :: write_maml                !! emit a sidecar .maml.
        logical, intent(in), optional :: qc                        !! run the schema's qc: checks.
        character(len=*), intent(in), optional :: compression      !! codec name.
        integer, intent(in), optional :: compression_level         !! codec level.
        integer, intent(in), optional :: chunk_size                !! row-group size, in rows.
        logical, intent(in), optional :: use_threads               !! Arrow's multi-threaded writer.
        logical, intent(in), optional :: overwrite                 !! allow truncating an existing file.
        logical, intent(in), optional :: release                   !! give back what this write read.
        character(len=*), intent(in), optional :: row_index_name   !! the field `sch` declares for the row numbers.
        type(parquet_writer) :: writer
        character(len=:), allocatable :: fname
        integer(int64), allocatable :: ridx(:)
        integer :: i, nfields, idx
        logical :: do_release, was_empty, have_ridx
        !
        nfields = sch%get_num_fields()
        ! Derived BEFORE the writer is opened, so a table that cannot produce row numbers -- built
        ! in memory, or detached -- aborts with no output file created rather than a half-written
        ! one. Once, not once per row group: this path writes the whole table in one go.
        have_ridx = present(row_index_name)
        if (have_ridx) call row_index_to_write(table, ridx)
        call parquet_open_writer(writer, trim(filename), sch, write_maml=write_maml, qc=qc,       &
            compression=compression, compression_level=compression_level, chunk_size=chunk_size,  &
            use_threads=use_threads, overwrite=overwrite)
        do_release = .true.
        if (present(release)) do_release = release
        if (present(row_mask)) call parquet_write_row_mask(writer, row_mask)
        do i = 1, nfields
            call sch%get_field_name(i, fname)
            ! A schema may deliberately disable a field (set_column_unavailable); skip those
            ! rather than demanding the table carry a column nobody is going to write.
            if (.not. sch%is_column_set(fname)) cycle
            ! The one field that is not a column: its values were derived above, so it is written
            ! straight out and never looked up. Nested rather than one .and., since the second
            ! operand references a dummy that may be absent (fortran-gotchas.md: .and. does not
            ! short-circuit). No is_valid= mask, because a row number is never Null -- the same
            ! rule scalar_validity applies to every other column on this path.
            if (have_ridx) then
                if (fname == trim(row_index_name)) then
                    call parquet_write_column(writer, fname, ridx)
                    cycle
                end if
            end if
            call locate_column_to_write(table, fname, "parquet_write_table", idx)
            ! Writing a column the caller never read is a first touch like any other: the schema
            ! naming it IS the request to read it. Nothing has to be pre-materialized to write.
            ! Whether it WAS is the whole of release=: what the caller had already read is theirs
            ! and stays, what this write had to read is this write's to give back. (A schema-less
            ! write names only resident columns, so nothing is ever released on that path.)
            was_empty = table%cache%cols(idx)%residency == RES_EMPTY
            call table_touch(table%cache, table_scope_of(table), idx, "parquet_write_table")
            call write_one_column(writer, table, idx, fname, chunked=.false.)
            ! Released here rather than after the loop, so peak residency is one column rather
            ! than every column the schema names.
            if (do_release .and. was_empty) call release_written_column(table%cache, idx)
        end do
        call parquet_close_writer(writer)
    end subroutine write_through_schema
    !
    !> Finds the slot that field `fname` names, refusing a field the table has no column for and
    !! one whose column holds no values; `context` is the public procedure's name, so that the
    !! same two refusals name whichever table write raised them.
    subroutine locate_column_to_write(table, fname, context, idx)
        type(parquet_table), intent(in) :: table   !! the table being written.
        character(len=*), intent(in) :: fname      !! the field's internal name.
        character(len=*), intent(in) :: context    !! calling procedure, for the message.
        integer, intent(out) :: idx                !! the slot; never 0.
        character(len=:), allocatable :: sfx
        !
        ! The lookup key is the INTERNAL name. A col_map: rename lives in the schema and is
        ! applied by the writer on the way out, so nothing here ever sees the output name.
        idx = table_find(table, fname)
        if (idx == 0) then
            call table_context_suffix(table%cache, fname, sfx)
            error stop EP // context // ": the schema declares a column the table does not have" // sfx
        end if
        if (.not. table%cache%cols(idx)%supported) then
            call table_context_suffix(table%cache, fname, sfx)
            error stop EP // context // ": the schema declares a column that holds no values" // sfx
        end if
    end subroutine locate_column_to_write
    !
    !> Builds the schema a SCHEMA-LESS write uses: one field per resident column, in slot order,
    !! under the column's own internal name.
    !!
    !! Three rules decide what goes in, and each is a decision rather than an implementation
    !! detail:
    !!
    !! * **Resident columns only.** That is what makes this the quick path -- it writes what is
    !!   already in memory and reads nothing. A column the caller never touched is not written, so
    !!   `release=` never has anything to give back on this path.
    !! * **`parquet_row_index` is never written unasked**, even when it is resident. It is this
    !!   library's own provenance column rather than the table's data, and having it appear
    !!   unasked-for in an output file is the more surprising of the two possible answers.
    !!   `row_index_name=` is how to ask: it appends one `int64` field of that name AFTER every
    !!   resident column, and the values are then written by `write_through_schema` from the
    !!   table's row numbers rather than from any slot -- which is why the resident-column rule
    !!   above still skips the reserved slot, and why the two cannot produce a duplicate. A
    !!   `row_index_name=` that collides with a column being written is refused HERE, where both
    !!   names are in hand, rather than left to `%add_field`'s duplicate refusal, whose message
    !!   names a procedure the caller never invoked.
    !! * **`col_size:`/`array_size:` are declared `auto`**, never measured here. The writer resolves
    !!   both from the data at the first write exactly as it would with no schema at all, so this
    !!   procedure cannot get them wrong -- and the sidecar MAML, emitted at close, records the
    !!   resolved values.
    !!
    !! The `table:` name is the caller's, since a schema-less table has no schema name to take one
    !! from and MAML requires the key: the output file's stem for a write, the source file's stem
    !! or the caller's own choice for `parquet_derive_schema`.
    subroutine build_table_schema(table, tname, sch, nfields, row_index_name)
        type(parquet_table), intent(in) :: table    !! the table being written.
        character(len=*), intent(in) :: tname       !! the MAML table: name; never blank.
        type(parquet_schema), intent(out) :: sch    !! the schema built from the descriptors.
        integer, intent(out) :: nfields             !! fields added; 0 means "no field to declare".
        character(len=*), intent(in), optional :: row_index_name !! declare the row numbers, last, under this name.
        character(len=:), allocatable :: dtype, u
        logical :: is_vec, is_str
        integer :: i
        !
        nfields = 0
        call sch%init(tname)
        do i = 1, table%cache%ncols
            associate (slot => table%cache%cols(i))
                if (.not. schemaless_writes_slot(slot)) cycle
                call schema_type_token(slot, dtype)
                is_vec = slot%width > 1
                is_str = slot%declared_kind == PK_STRING .or. slot%declared_kind == PK_STRING_VEC
                ! A CONTAINER column gets neither key, and that is the point rather than an
                ! omission: `col_size:` declares a FIXED per-row width and `array_size:` a fixed
                ! per-element string width, and a container has neither. Its width lives in its own
                ! offsets, one length per row. See doc/pages/schema/maml-format.md's callout.
                if (parquet_kind_is_container(slot%declared_kind)) then
                    is_vec = .false.
                    is_str = .false.
                end if
                ! An UNALLOCATED allocatable actual makes an optional dummy absent (F2018
                ! 15.5.2.12), which is how a column with no unit gets no `unit:` key at all rather
                ! than an empty one. The unit is resolved the way every %unit query resolves it
                ! (table_slot_unit: the descriptor, then the values), never from the descriptor
                ! alone -- a column built with %add_column(unit=) keeps its unit on its values, and
                ! reading the descriptor only wrote such a column's sidecar without its unit.
                call table_slot_unit(table%cache, i, u)
                if (len_trim(u) == 0) deallocate(u)
                if (is_vec .and. is_str) then
                    call sch%add_field(slot%name, dtype, unit=u, col_size=parquet_size_auto, &
                        array_size=parquet_size_auto)
                else if (is_vec) then
                    call sch%add_field(slot%name, dtype, unit=u, col_size=parquet_size_auto)
                else if (is_str) then
                    call sch%add_field(slot%name, dtype, unit=u, array_size=parquet_size_auto)
                else
                    call sch%add_field(slot%name, dtype, unit=u)
                end if
                nfields = nfields + 1
            end associate
        end do
        ! LAST, after every resident column, so the row numbers sit at the end of the file rather
        ! than in the middle of the caller's own columns. No unit: a row number has none, and no
        ! col_size:/array_size: either, since it is a scalar int64 column.
        if (present(row_index_name)) then
            call sch%add_field(trim(row_index_name), "int64")
            nfields = nfields + 1
        end if
        ! No parquet_parse_maml here: %init/%add_field keep %cinfo in step as the schema is
        ! built. MAML still requires at least one field, so the caller checks `nfields` and takes
        ! the empty-file path instead of this one.
    end subroutine build_table_schema
    !
    !> Validates a `row_index_name=` request, before the schema is built or any file is opened;
    !! `context` is the public procedure's name and `has_schema` whether the caller gave `schema=`.
    !! Absent, it does nothing at all -- which is why every existing write is untouched.
    !!
    !! **`schema=` is refused rather than honoured** because a schema IS the statement of which
    !! columns the output has and what they are called, so a second argument adding one to it says
    !! two things at once. The equivalent with a schema already exists and the message points at
    !! it: materialize `parquet_row_index` and give the MAML a `col_map:` entry renaming it. (It
    !! is not merely a policy choice -- `%add_field` requires `%init`, which a MAML-parsed schema
    !! never called, so appending a field to the caller's schema is not available here anyway.)
    !!
    !! The collision test asks the same predicate the write itself will ask
    !! (`schemaless_writes_slot`), so it answers about the columns actually going into the file
    !! rather than about every column the table happens to hold; a slot the write skips cannot
    !! collide with anything.
    !!
    !! **The reserved name is a WARNING, not a refusal**, and it is emitted last, after every
    !! refusal above: the file is perfectly valid and other readers see the column, so refusing
    !! would deny a legitimate request -- but `parquet_open_table` drops a file column of that
    !! name on reopen, so a caller who has not read `table-open.md` would find the column missing
    !! with nothing to explain it. Warning about a request that is then refused would be noise,
    !! hence the order.
    subroutine check_row_index_request(table, has_schema, context, row_index_name)
        class(parquet_table), intent(in) :: table                !! the table being written.
        logical, intent(in) :: has_schema                        !! .true. when the caller gave schema=.
        character(len=*), intent(in) :: context                  !! calling procedure, for the messages.
        character(len=*), intent(in), optional :: row_index_name !! the caller's request, if any.
        character(len=:), allocatable :: sfx
        integer :: i
        !
        if (.not. present(row_index_name)) return
        if (has_schema) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // context // ": row_index_name= and schema= cannot both be given; " // &
                "the schema is what chooses and names the output columns, so to write the row " // &
                "numbers under a schema, materialize '" // PARQUET_ROW_INDEX // "' and rename " // &
                "it with a col_map: entry" // sfx
        end if
        if (len_trim(row_index_name) == 0) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // context // ": row_index_name= must not be blank; it is the name " // &
                "the row numbers are written under" // sfx
        end if
        do i = 1, table%cache%ncols
            if (.not. schemaless_writes_slot(table%cache%cols(i))) cycle
            if (table%cache%cols(i)%name /= trim(row_index_name)) cycle
            call table_context_suffix(table%cache, trim(row_index_name), sfx)
            error stop EP // context // ": row_index_name='" // trim(row_index_name) // "' is " // &
                "already the name of a column this write is writing; give the row numbers a " // &
                "name of their own" // sfx
        end do
        if (trim(row_index_name) == PARQUET_ROW_INDEX) then
            call table_context_suffix(table%cache, "", sfx)
            call parquet_emit_warning(context // ": row_index_name='" // PARQUET_ROW_INDEX // &
                "' is the reserved name of the automatic row-index column, so parquet_open_table " // &
                "drops this column when it reopens the output file, with a warning; only a " // &
                "read-in MAML's extra: remap: can reach it there" // sfx)
        end if
    end subroutine check_row_index_request
    !
    !> The row numbers `row_index_name=` writes: the resident `parquet_row_index` column when the
    !! table has one, otherwise derived.
    !!
    !! **The resident column wins, and that is not an optimisation.** `%sort_by` reorders it with
    !! every other column, so on a table sorted after it was materialized the resident values are
    !! the correct answer and a fresh derivation from the reader is not -- the reader knows the
    !! read-time transform, not what the table did to its rows afterwards.
    !!
    !! An evicted one (residency back to RES_EMPTY, which is reachable: the row index is never
    !! `user_populated`, so `%evict_column` accepts it) falls through to the derivation, which is
    !! what would have rebuilt it anyway.
    subroutine row_index_to_write(table, rows)
        type(parquet_table), intent(in) :: table            !! the table being written.
        integer(int64), allocatable, intent(out) :: rows(:) !! one 1-based file row number per table row.
        integer(int64), pointer :: p(:)
        integer :: idx
        !
        if (table%cache%row_index_live) then
            idx = table_find(table, PARQUET_ROW_INDEX)
            if (idx > 0) then
                if (table%cache%cols(idx)%residency == RES_FULL) then
                    call parquet_column_data_ptr(table%cache%cols(idx)%values, p)
                    rows = p
                    return
                end if
            end if
        end if
        call table_row_index_values(table, "parquet_write_table", rows)
    end subroutine row_index_to_write
    !
    !> Whether a schema-less write takes `slot`: resident, supported, and not the row index --
    !! the three rules `build_table_schema`'s doc-comment gives. One predicate for the two callers
    !! that must agree on it: `build_table_schema`, which decides what a schema-less
    !! `parquet_write_table` declares, and `parquet_write_table_chunk`'s schema-less arm, which
    !! decides what such a chunk writes.
    pure logical function schemaless_writes_slot(slot) result(yes)
        type(parquet_table_column), intent(in) :: slot !! the descriptor.
        !
        yes = .false.
        if (slot%residency /= RES_FULL) return
        if (.not. slot%supported) return
        if (slot%name == PARQUET_ROW_INDEX) return
        yes = .true.
    end function schemaless_writes_slot
    !
    !> Writes a valid parquet file with no columns and no rows, for a schema-less write of a table
    !! that holds nothing resident.
    !!
    !! A bare (schema-less) writer does the whole job: opened and closed with nothing written, it
    !! produces a file this library reopens as a 0-column, 0-row table -- verified against Arrow
    !! rather than assumed. `qc` and `release` have nothing to act on and are deliberately not
    !! forwarded; `row_mask` likewise, since there are no rows to mask.
    !!
    !! `write_maml=.true.` ABORTS here rather than silently producing no sidecar. A MAML file has
    !! no way to describe zero columns (`fields:` may not be empty), so the request cannot be
    !! satisfied, and dropping a requested output file without a word is the worse failure.
    subroutine write_empty_file(filename, write_maml, compression, compression_level, chunk_size, &
            use_threads, overwrite)
        character(len=*), intent(in) :: filename                   !! output parquet file.
        logical, intent(in), optional :: write_maml                !! sidecar request; see above.
        character(len=*), intent(in), optional :: compression      !! codec name.
        integer, intent(in), optional :: compression_level         !! codec level.
        integer, intent(in), optional :: chunk_size                !! row-group size, in rows.
        logical, intent(in), optional :: use_threads               !! Arrow's multi-threaded writer.
        logical, intent(in), optional :: overwrite                 !! allow truncating an existing file.
        type(parquet_writer) :: writer
        !
        if (present(write_maml)) then
            if (write_maml) then
                error stop EP // "parquet_write_table: write_maml=.true. was requested for a " // &
                    "schema-less write of a table with no resident column, but a MAML file " // &
                    "cannot describe zero columns; materialize a column first, or pass a schema " // &
                    "(file: " // filename // ")"
            end if
        end if
        call parquet_open_writer(writer, filename, compression=compression, &
            compression_level=compression_level, chunk_size=chunk_size, &
            use_threads=use_threads, overwrite=overwrite)
        call parquet_close_writer(writer)
    end subroutine write_empty_file
    !
    !> The MAML `data_type` token for a descriptor -- the inverse of `table_kind_from_type`, plus
    !! the `[unit]`/`[unit,utc]` suffix a TIME/TIMESTAMP column needs.
    !!
    !! **The suffix is what makes a temporal column round-trip.** Without it the writer defaults to
    !! microseconds, and a column that was stored at nanoseconds does not truncate quietly -- it
    !! fails the write, because `to_unix` refuses to lose precision. The unit comes from
    !! `time_unit`, recorded on the descriptor at classification time precisely because a
    !! `parquet_timestamp` carries no unit of its own and nothing else remembers it.
    !!
    !! A column with no recorded unit -- one built in memory with `%add_column` rather than read
    !! from a file -- gets no suffix and therefore the writer's own microsecond default, which is
    !! exactly what a hand-written schema-less write of the same data would produce.
    subroutine schema_type_token(slot, tok)
        type(parquet_table_column), intent(in) :: slot        !! the descriptor to describe.
        character(len=:), allocatable, intent(out) :: tok     !! the MAML data_type token.
        character(len=:), allocatable :: sfx
        !
        select case (slot%declared_kind)
        case (PK_INT32, PK_INT32_VEC)
            tok = "int32"
        case (PK_INT64, PK_INT64_VEC)
            tok = "int64"
        case (PK_FLOAT32, PK_FLOAT32_VEC)
            tok = "float32"
        case (PK_FLOAT64, PK_FLOAT64_VEC)
            tok = "float64"
        case (PK_LOGICAL, PK_LOGICAL_VEC)
            tok = "boolean"
        case (PK_STRING, PK_STRING_VEC)
            tok = "string"
        case (PK_DATE, PK_DATE_VEC)
            ! DATE is a day count: no unit and no timezone, so no suffix exists for it.
            tok = "date"
        case (PK_TIME, PK_TIME_VEC)
            call temporal_suffix(slot, .false., sfx)
            tok = "time" // sfx
        case (PK_TIMESTAMP, PK_TIMESTAMP_VEC)
            call temporal_suffix(slot, .true., sfx)
            tok = "timestamp" // sfx
        case (PK_LIST, PK_MAP)
            ! `list[<element>]` / `map[<value>]`, with the payload's token taken from the container
            ! itself rather than from a second table -- one recursive step through the same nine
            ! tokens the scalar arms use. A struct is NOT here: it has no single payload token, and
            ! its layout lives in the parquet_struct_column the descriptor already holds, so MAML
            ! never declares it (the campaign's "struct fields never appear in MAML" decision).
            !
            ! Neither emits a `col_size:` key, and that is what the reader needs: `col_size:`
            ! declares a FIXED per-row width, which is exactly the property a container column does
            ! not have. See doc/pages/schema/maml-format.md's col_size/array_size callout.
            call container_type_token(slot, tok)
        case (PK_STRUCT)
            tok = "struct"
        case default
            ! Not reachable: build_table_schema skips every unsupported slot, and a resident
            ! column always has one of the 18 kinds above. gcov nonetheless credits the line with
            ! the procedure's whole call count (37 in one full run) while the real arms account
            ! for all 37 between them (14+2+2+6+3+3+2+2+3), so the count is the select's dispatch
            ! rather than this arm running -- hence the artifact tag.
            tok = "" ! GCOVR_EXCL_LINE -- gcov attribution artifact
        end select
    end subroutine schema_type_token
    !
    !> The `list[<element>]`/`map[<value>]` MAML token for a container column.
    !!
    !! The payload token comes from the container's own payload `parquet_column`, put back through
    !! `schema_type_token` -- so a `list<timestamp[us]>` inherits the temporal suffix rule with no
    !! second copy of it. The recursion is exactly one level deep: a container payload is always a
    !! scalar kind, because the reader refuses a nested container outright.
    subroutine container_type_token(slot, tok)
        type(parquet_table_column), intent(in) :: slot        !! the descriptor to describe.
        character(len=:), allocatable, intent(out) :: tok     !! the MAML data_type token.
        type(parquet_table_column) :: payload
        class(parquet_container_column), pointer :: c
        character(len=:), allocatable :: inner
        !
        call parquet_column_container(slot%values, c)
        ! A synthetic descriptor rather than a second token table: schema_type_token reads only
        ! `declared_kind` and `time_unit`/`time_utc`, so handing it the payload's own values is
        ! what makes the suffix rules apply unchanged.
        call container_payload_descriptor(c, slot, payload)
        call schema_type_token(payload, inner)
        select case (slot%declared_kind)
        case (PK_LIST)
            tok = "list[" // inner // "]"
        case default
            tok = "map[" // inner // "]"
        end select
    end subroutine container_type_token
    !
    !> Builds a synthetic descriptor describing a container's PAYLOAD, so that `schema_type_token`
    !! can be reused on it verbatim.
    !!
    !! `schema_type_token` reads exactly three fields -- `declared_kind`, `time_unit` and
    !! `time_utc` -- so copying the temporal pair off the container's own descriptor and taking the
    !! kind from the container is enough to make every suffix rule apply with no second copy of it.
    !! The temporal pair is recorded at classification by `record_temporal_unit`, which asks about
    !! the payload for a container column.
    subroutine container_payload_descriptor(c, slot, payload)
        class(parquet_container_column), intent(in) :: c      !! the container.
        type(parquet_table_column), intent(in) :: slot        !! the container's own descriptor.
        type(parquet_table_column), intent(out) :: payload    !! descriptor of its payload.
        !
        payload%time_unit = slot%time_unit
        payload%time_utc = slot%time_utc
        select type (c)
        type is (parquet_list_column)
            payload%declared_kind = c%element_kind()
        type is (parquet_map_column)
            payload%declared_kind = c%element_kind()
        class default ! GCOVR_EXCL_START -- only a list or a map reaches container_type_token,
            ! which is the only caller; a struct emits a bare `struct` token and never asks for a
            ! payload kind, because it has several.
            payload%declared_kind = PK_NONE
        end select ! GCOVR_EXCL_STOP
    end subroutine container_payload_descriptor
    !
    !> The `[unit]`/`[unit,utc]` suffix for a TIME/TIMESTAMP token, or "" when the column carries
    !! no recorded unit (an in-memory column) and the writer's default should stand.
    subroutine temporal_suffix(slot, allow_utc, sfx)
        type(parquet_table_column), intent(in) :: slot     !! the descriptor to describe.
        logical, intent(in) :: allow_utc                   !! .true. for timestamp; time has no tz.
        character(len=:), allocatable, intent(out) :: sfx  !! "[us]", "[ns,utc]", or "".
        !
        select case (slot%time_unit)
        case (parquet_unit_millis)
            sfx = "[ms"
        case (parquet_unit_micros)
            sfx = "[us"
        case (parquet_unit_nanos)
            sfx = "[ns"
        case default
            ! Includes parquet_unit_seconds, which no parquet file can actually store (a MAML
            ! `time[s]`/`timestamp[s]` token is rejected at parse time), so it can only mean
            ! "nothing was recorded".
            sfx = ""
            return
        end select
        if (allow_utc .and. slot%time_utc) sfx = sfx // ",utc"
        sfx = sfx // "]"
    end subroutine temporal_suffix
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
    !! * **A key the writer regenerates from the output schema is never carried**, and naming one
    !!   through `metadata_keys=` is an error. See `writer_regenerates_key`.
    !! * **It works after a detach**, because the metadata was snapshotted at open. That is the
    !!   whole point: the natural shape is read, mutate rows, write, and the reader is gone by then.
    subroutine carry_source_metadata(table, sch, keys, context)
        type(parquet_table), intent(in) :: table                   !! the table being written.
        type(parquet_schema), intent(inout) :: sch                 !! private schema copy to add to.
        character(len=*), intent(in), optional :: keys(:)          !! only these keys, if given.
        character(len=*), intent(in) :: context                    !! calling procedure, for the messages.
        character(len=:), allocatable :: sfx
        integer :: i, k
        !> How many entries `sch` declared BEFORE this loop started adding to it. The loop mutates
        !! `sch`, so "does the schema declare this key itself?" has to be asked of the original
        !! entries only -- by the time a carried "<key>.datatype" is tested, the carried `<key>`
        !! one line earlier is already in there, and an unbounded scan would answer .true. for a
        !! key the schema never declared and drop a companion that should have been carried.
        integer :: n_declared
        logical :: wanted
        !
        if (.not. allocated(table%cache%meta_keys)) then
            call table_context_suffix(table%cache, "", sfx)
            error stop EP // context // ": this table was not opened from a file, so it " // &
                "has no source metadata to copy" // sfx
        end if
        ! Every requested key is checked BEFORE anything is added, so a typo fails with the output
        ! file not yet opened rather than half-written.
        if (present(keys)) then
            do k = 1, size(keys)
                if (.not. source_has_key(table, trim(keys(k)))) then
                    call table_context_suffix(table%cache, "", sfx)
                    error stop EP // context // ": metadata_keys names '" // trim(keys(k)) // &
                        "', which this table's source file does not have" // sfx
                end if
                ! Refused rather than skipped: the source file HAS this key, so the check above
                ! passes and a silent skip would make naming it a no-op -- against this
                ! procedure's own "naming a key is a claim" rule. Carrying it is worse still; see
                ! writer_regenerates_key.
                if (writer_regenerates_key(trim(keys(k)))) then
                    call table_context_suffix(table%cache, "", sfx)
                    error stop EP // context // ": metadata_keys names '" // trim(keys(k)) // &
                        "', which the writer generates itself from the output schema -- the output " // &
                        "carries its own value for it, so it cannot also be copied from the source" // sfx
                end if
            end do
        end if
        n_declared = 0
        if (allocated(sch%metadata%items)) n_declared = size(sch%metadata%items)
        do i = 1, size(table%cache%meta_keys)
            wanted = .true.
            if (present(keys)) then
                wanted = .false.
                do k = 1, size(keys)
                    if (trim(keys(k)) == trim(table%cache%meta_keys(i))) wanted = .true.
                end do
            end if
            if (.not. wanted) cycle
            if (writer_regenerates_key(trim(table%cache%meta_keys(i)))) cycle
            if (schema_declares_key(sch, trim(table%cache%meta_keys(i)))) cycle
            ! A carried "<key>.datatype" describes a key the schema declares itself, so the key it
            ! describes was just skipped by the rule above and this one now describes nothing that
            ! got written. Worse, the schema's own typed entry synthesizes a "<key>.datatype" of
            ! its own at write time, so keeping this one would put TWO same-named companions in
            ! the output, free to disagree -- and the stale carried one would be the survivor
            ! (parquet_open_writer's collision guard suppresses the synthesized one on seeing it),
            ! inverting this procedure's own "the schema wins a collision" rule.
            if (carried_companion_is_superseded(sch, trim(table%cache%meta_keys(i)), n_declared)) cycle
            call sch%add_metadata(trim(table%cache%meta_keys(i)), trim(table%cache%meta_values(i)))
        end do
    end subroutine carry_source_metadata
    !
    !> .true. when `build_file_metadata` (`src/parquet_wrapper.cpp`) emits `key` itself from the
    !> output schema, so carrying the source file's copy would put two entries of that name in one
    !> file. Both halves of the damage are silent, and they differ in kind because of the order
    !> that function pushes its keys in:
    !>
    !> * `column.<name>.<attr>` is pushed AFTER the carried table metadata, so the carried copy
    !>   comes first and `parquet_get_metadata` -- which resolves the first match -- answers with
    !>   the SOURCE file's value for a column whose own Arrow field and VOTable FIELD say what the
    !>   output schema declared. Two answers in one file, and the read path returns the wrong one.
    !> * `DATE`, `name` and the two `IVOA.VOTable-Parquet.*` keys are pushed BEFORE it, so the
    !>   writer's own wins and the carried copy is dead weight -- a duplicated full XML sidecar
    !>   among it -- plus a `WARNING: ... reserved for the parquet writer's own internal file
    !>   metadata` line per key, the library warning about its own copy operation.
    !>
    !> Every `column.` key is refused, not just those naming a column the output has: a
    !> `column.x.*` entry for a column the output does not write is metadata about a column that
    !> is not there. A new key that function starts generating must be added here, or it silently
    !> joins the first case above -- see `feature_risks.md` Risk-137.
    logical function writer_regenerates_key(key) result(regenerated)
        character(len=*), intent(in) :: key !! the source key to test.
        character(len=*), parameter :: COL_PREFIX = "column."
        character(len=28), parameter :: FIXED(4) = [character(len=28) :: &
            "DATE", "name", "IVOA.VOTable-Parquet.version", "IVOA.VOTable-Parquet.content"]
        integer :: i
        !
        regenerated = .true.
        if (len(key) > len(COL_PREFIX)) then
            if (key(1:len(COL_PREFIX)) == COL_PREFIX) return
        end if
        do i = 1, size(FIXED)
            if (trim(FIXED(i)) == key) return
        end do
        regenerated = .false.
    end function writer_regenerates_key
    !
    !> .true. when `key` is a "<name>.datatype" companion whose own `name` the schema declared
    !> itself, so carrying it would leave the output with two companions for one key. See the
    !> call site. `n_declared` bounds the scan to the schema's own entries: the carry loop is
    !> adding to `sch` as it goes, and a key it carried a moment ago must not count as declared.
    logical function carried_companion_is_superseded(sch, key, n_declared) result(superseded)
        type(parquet_schema), intent(in) :: sch !! the output schema.
        character(len=*), intent(in) :: key     !! the carried key to test.
        integer, intent(in) :: n_declared       !! entries `sch` had before the carry loop began.
        character(len=*), parameter :: SFX = ".datatype"
        integer :: cut, i
        !
        superseded = .false.
        cut = len(key) - len(SFX)
        if (cut < 1) return
        if (key(cut+1:) /= SFX) return
        if (.not. allocated(sch%metadata%items)) return
        do i = 1, n_declared
            if (trim(sch%metadata%items(i)%key) == key(1:cut)) superseded = .true.
        end do
    end function carried_companion_is_superseded
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
        ! Two separate things are true about the line below, and only the second is about coverage.
        ! The GUARD never fires: `sch` here is either a copy of a parsed schema or one
        ! `build_table_schema` just produced, and a parse cannot succeed without a non-empty
        ! `table:` metadata entry -- so `items` is allocated by the time this is reachable. The
        ! LINE, on the other hand, plainly executes: it shares a basic block with `has = .false.`
        ! above it, which gcov credits with the whole block's count (53 against 0 here) while
        ! reporting this one uncovered. The sibling guard in carried_companion_is_superseded is
        ! the same test and IS reported covered, because the `return`s above it start a new block.
        if (.not. allocated(sch%metadata%items)) return ! GCOVR_EXCL_LINE -- gcov attribution artifact
        do i = 1, size(sch%metadata%items)
            if (trim(sch%metadata%items(i)%key) == key) has = .true.
        end do
    end function schema_declares_key
    !
    !> Writes slot `idx` through the `parquet_write_column` specific matching its stored kind --
    !! or, with `chunked`, through the `parquet_write_column_chunk` specific, as one row group's
    !! worth of the column. ONE dispatch for both paths, deliberately: a second copy of the
    !! twenty-one arms would be free to drift from this one, and drift between the table write
    !! and a hand-written one is exactly what `feature_risks.md` Risk-8 is about.
    !!
    !! Validity is passed as `is_valid=` for every kind that accepts one; the temporal kinds take
    !! no mask because their null state lives inside each element, the string kind carries its
    !! own validity inside the `parquet_string_column`, and so does a container.
    !!
    !! **The chunked path passes a mask whether or not the column holds a Null** (`force` on the
    !! validity helpers), and this is the one place a table write deliberately does what a
    !! hand-written loop need not. A streamed column's Arrow field is fixed nullable or not by
    !! its FIRST row group, from whether a mask was passed, and every later row group must use
    !! the same form -- a mismatch is a hard C++ abort in both directions. Carry the whole-column
    !! path's null-free-means-no-mask rule into a loop and that abort becomes data-dependent: row
    !! group 1 happens to be Null-free, row group 7 holds one Null, and the write dies rows away
    !! from anything the caller did wrong. So the mask is always there, the field is always
    !! nullable, and a Null may arrive in any row group. The cost is one mask per column per row
    !! group plus the null bitmap the writer builds from it (Risk-8's measured ~2.5x, bounded to
    !! one row group's worth); a column declared `protected_cols:` pays neither, because
    !! `parquet_check_protected` erases a protected column's all-`.true.` mask before the write
    !! and the field is stored non-nullable -- a Null in it is then an abort naming the column
    !! rather than a corrupt file. `feature_risks.md` Risk-226 pins the rule.
    subroutine write_one_column(writer, table, idx, name, chunked)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        type(parquet_table), intent(in) :: table      !! the table being written.
        integer, intent(in) :: idx                    !! slot to write.
        character(len=*), intent(in) :: name          !! the column's internal name.
        logical, intent(in) :: chunked                !! .true.: one row group of an open writer.
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
        class(parquet_container_column), pointer :: p_cont
        logical, allocatable :: valid(:), validv(:,:)
        character(len=:), allocatable :: sfx, kname, who, chr(:,:)
        !
        associate (col => table%cache%cols(idx)%values)
            select case (col%kindof())
            case (PK_INT32)
                call col%data_ptr(p_i32)
                call scalar_validity(table, idx, valid, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_i32, is_valid=valid)
                else
                    call parquet_write_column(writer, name, p_i32, is_valid=valid)
                end if
            case (PK_INT64)
                call col%data_ptr(p_i64)
                call scalar_validity(table, idx, valid, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_i64, is_valid=valid)
                else
                    call parquet_write_column(writer, name, p_i64, is_valid=valid)
                end if
            case (PK_FLOAT32)
                call col%data_ptr(p_f32)
                call scalar_validity(table, idx, valid, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_f32, is_valid=valid)
                else
                    call parquet_write_column(writer, name, p_f32, is_valid=valid)
                end if
            case (PK_FLOAT64)
                call col%data_ptr(p_f64)
                call scalar_validity(table, idx, valid, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_f64, is_valid=valid)
                else
                    call parquet_write_column(writer, name, p_f64, is_valid=valid)
                end if
            case (PK_LOGICAL)
                call col%data_ptr(p_bool)
                call scalar_validity(table, idx, valid, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_bool, is_valid=valid)
                else
                    call parquet_write_column(writer, name, p_bool, is_valid=valid)
                end if
            case (PK_STRING)
                call col%string_column(p_str)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_str)
                else
                    call parquet_write_column(writer, name, p_str)
                end if
            case (PK_DATE)
                call col%data_ptr(p_dt)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_dt)
                else
                    call parquet_write_column(writer, name, p_dt)
                end if
            case (PK_TIME)
                call col%data_ptr(p_tm)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_tm)
                else
                    call parquet_write_column(writer, name, p_tm)
                end if
            case (PK_TIMESTAMP)
                call col%data_ptr(p_ts)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_ts)
                else
                    call parquet_write_column(writer, name, p_ts)
                end if
            case (PK_INT32_VEC)
                call col%data_ptr(p_i32v)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_i32v, is_valid=validv)
                else
                    call parquet_write_column(writer, name, p_i32v, is_valid=validv)
                end if
            case (PK_INT64_VEC)
                call col%data_ptr(p_i64v)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_i64v, is_valid=validv)
                else
                    call parquet_write_column(writer, name, p_i64v, is_valid=validv)
                end if
            case (PK_FLOAT32_VEC)
                call col%data_ptr(p_f32v)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_f32v, is_valid=validv)
                else
                    call parquet_write_column(writer, name, p_f32v, is_valid=validv)
                end if
            case (PK_FLOAT64_VEC)
                call col%data_ptr(p_f64v)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_f64v, is_valid=validv)
                else
                    call parquet_write_column(writer, name, p_f64v, is_valid=validv)
                end if
            case (PK_LOGICAL_VEC)
                call col%data_ptr(p_boolv)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_boolv, is_valid=validv)
                else
                    call parquet_write_column(writer, name, p_boolv, is_valid=validv)
                end if
            case (PK_STRING_VEC)
                ! No compact rank-2 string write path exists, so this goes through the
                ! fixed-width form -- the same asymmetry the read side has.
                call table%get(name, chr)
                call vector_validity(table, idx, validv, chunked)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, chr, is_valid=validv)
                else
                    call parquet_write_column(writer, name, chr, is_valid=validv)
                end if
            case (PK_DATE_VEC)
                call col%data_ptr(p_dtv)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_dtv)
                else
                    call parquet_write_column(writer, name, p_dtv)
                end if
            case (PK_TIME_VEC)
                call col%data_ptr(p_tmv)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_tmv)
                else
                    call parquet_write_column(writer, name, p_tmv)
                end if
            case (PK_TIMESTAMP_VEC)
                call col%data_ptr(p_tsv)
                if (chunked) then
                    call parquet_write_column_chunk(writer, name, p_tsv)
                else
                    call parquet_write_column(writer, name, p_tsv)
                end if
            case (PK_LIST, PK_MAP, PK_STRUCT)
                ! No `is_valid=`, exactly as the temporal kinds take none: a container carries its
                ! own per-row nullness inside itself, and adopt_container deliberately leaves the
                ! surrounding parquet_column's bitmap unallocated so there is only one answer.
                call parquet_column_container(col, p_cont)
                call write_container_column(writer, name, p_cont, chunked)
            case default
                ! Not reachable through the public API: by the time write_one_column runs, the
                ! caller has already rejected an unsupported slot and table_touch has
                ! resolved/materialized this one, so col%kindof() is always one of the 18 kinds
                ! handled above.
                who = "parquet_write_table" ! GCOVR_EXCL_LINE
                if (chunked) who = "parquet_write_table_chunk" ! GCOVR_EXCL_LINE
                call table_context_suffix(table%cache, name, sfx) ! GCOVR_EXCL_LINE
                call parquet_kind_name(col%kindof(), kname) ! GCOVR_EXCL_LINE
                error stop EP // who // ": column kind " // kname // & ! GCOVR_EXCL_LINE
                    " cannot be written" // sfx ! GCOVR_EXCL_LINE
            end select
        end associate
    end subroutine write_one_column
    !
    !> Writes a container column through the `parquet_write_column` (or, with `chunked`, the
    !! `parquet_write_column_chunk`) specific matching its concrete type.
    !!
    !! The downcast is unavoidable and belongs here rather than at the call site: the three
    !! specifics of either generic take a `type(parquet_list_column)` / `type(parquet_map_column)`
    !! / `type(parquet_struct_column)`, so a generic reference needs the concrete type to resolve.
    subroutine write_container_column(writer, name, c, chunked)
        type(parquet_writer), intent(inout) :: writer            !! open writer.
        character(len=*), intent(in) :: name                     !! the column's internal name.
        class(parquet_container_column), pointer, intent(in) :: c !! the container to write.
        logical, intent(in) :: chunked                           !! .true.: one row group of an open writer.
        !
        select type (c)
        type is (parquet_list_column)
            if (chunked) then
                call parquet_write_column_chunk(writer, name, c)
            else
                call parquet_write_column(writer, name, c)
            end if
        type is (parquet_map_column)
            if (chunked) then
                call parquet_write_column_chunk(writer, name, c)
            else
                call parquet_write_column(writer, name, c)
            end if
        type is (parquet_struct_column)
            if (chunked) then
                call parquet_write_column_chunk(writer, name, c)
            else
                call parquet_write_column(writer, name, c)
            end if
        class default ! GCOVR_EXCL_START -- unreachable: adopt_container is the only writer of a
            ! container kind and takes the kind FROM the container, so a PK_LIST/PK_MAP/PK_STRUCT
            ! column always holds the matching concrete type.
            error stop EP // "parquet_write_table: internal: unknown container type for column '" // &
                trim(name) // "'"
        end select ! GCOVR_EXCL_STOP
    end subroutine write_container_column
    !
    !> Builds a per-row validity mask for a scalar column, or leaves `valid` UNALLOCATED when the
    !! column holds no nulls -- unless `force`, which hands back an all-`.true.` mask instead.
    !!
    !! Every call site passes the result straight on as `is_valid=`, and an unallocated allocatable
    !! actual makes an `optional` dummy absent (F2018 15.5.2.12) -- so a null-free column reaches
    !! `parquet_write_column` with no mask argument at all, which is exactly what a hand-written
    !! write of the same data would do. That matters more than it looks: passing a uniformly-`.true.`
    !! mask instead costs an nrows-long allocation here AND makes the writer build an Arrow null
    !! bitmap it did not need, which together were measured as the whole of `parquet_write_table`'s
    !! ~2.5x gap against a hand-written per-column loop.
    !!
    !! `force` is the chunked path's, and buys exactly that cost on purpose -- see
    !! `write_one_column` for why a row group's mask has to be present whether or not it carries
    !! a Null. Never pass it from the whole-column path.
    subroutine scalar_validity(table, idx, valid, force)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:)       !! .true. where the row is not null; see above.
        logical, intent(in) :: force                        !! .true.: an all-.true. mask, never unallocated.
        !
        call table%cache%cols(idx)%values%row_validity(valid)
        if (.not. force) return
        if (allocated(valid)) return
        allocate(valid(table%cache%cols(idx)%values%length()))
        valid = .true.
    end subroutine scalar_validity
    !
    !> Builds a per-element validity mask for a vector column, shaped (width, nrows) to match
    !! the stored orientation -- or leaves `valid` unallocated when there are no nulls, exactly as
    !! `scalar_validity` does and for the same reason, with the same `force` for the chunked path.
    !!
    !! A pass-through, and that is the point: `parquet_column` stores validity per element and
    !! `parquet_write_column` accepts it per element, so the writer records exactly the nulls the
    !! table holds. It used to read the row bit and broadcast it back across the row, which turned
    !! one null element into a null row in the output file.
    subroutine vector_validity(table, idx, valid, force)
        type(parquet_table), intent(in) :: table            !! the table.
        integer, intent(in) :: idx                          !! slot index.
        logical, allocatable, intent(out) :: valid(:,:)     !! .true. where the element is not null.
        logical, intent(in) :: force                        !! .true.: an all-.true. mask, never unallocated.
        !
        call table%cache%cols(idx)%values%element_validity(valid)
        if (.not. force) return
        if (allocated(valid)) return
        allocate(valid(table%cache%cols(idx)%values%colwidth(), table%cache%cols(idx)%values%length()))
        valid = .true.
    end subroutine vector_validity
    !
end submodule parquet_tables_write
