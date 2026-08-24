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
!!
!! **A schema-less write BUILDS a schema rather than taking a second path.** `build_table_schema`
!! turns the resident columns' descriptors into an ordinary `parquet_schema` and everything below
!! proceeds as it always did. Two things fall out of that choice and are the reason for it: the
!! sidecar MAML costs nothing (the writer already emits one from whatever schema it was given),
!! and there is exactly one write loop to keep correct rather than two that can drift. The
!! generated schema declares `col_size:`/`array_size:` as `auto`, so the writer resolves them from
!! the data exactly as it would with no schema at all -- and because the sidecar is emitted at
!! CLOSE, it records the resolved values rather than `auto`.
submodule (parquet_tables) parquet_tables_write
    implicit none
    !
contains
    !
    module procedure parquet_write_table
        type(parquet_schema) :: own
        integer :: nfields
        logical :: want_metadata, use_own
        !
        call table_check_open(table, "parquet_write_table")
        ! release= evicts columns as it writes, which replaces their storage -- so a write is a
        ! structural change to the table as far as another thread is concerned, not a read.
        call table_check_not_shared(table, "parquet_write_table")
        if (present(schema)) then
            ! %get_num_fields on an unpopulated %cinfo reads uninitialized state, which turns the
            ! write loop into a runaway allocation and an OOM kill rather than any kind of
            ! diagnosable failure. So a schema that has not been parsed is stopped here, one way
            ! or the other.
            !
            ! **Which schema lands in which arm follows from what the two queries actually read**
            ! -- %is_parsed() is `allocated(%cinfo%col)` and %is_init() is that OR the %init flag:
            !
            !   * built with %init/%add_field -- %add_field parses as it goes, so %is_parsed() is
            !     already .true. after the first field and neither arm runs;
            !   * from parquet_parse_maml, parquet_load_maml_file or an embedded get_parquet_maml
            !     -- parsed on arrival, likewise neither arm;
            !   * %maml%lines assigned DIRECTLY and never parsed -- %init was never called, so
            !     %is_init() is .false. and this is the error stop below, not the parse;
            !   * %init called and no field added yet -- %is_init() .true., %is_parsed() .false.,
            !     which is the ONE state reaching the parse call.
            !
            ! That last one is header-only MAML, so the parse always fails validation with
            ! "no fields defined" naming the schema. Keeping the call rather than adding a second
            ! bespoke guard is deliberate: the parser's own message is the accurate one, and the
            ! call is also what makes `schema` intent(inout) rather than intent(in).
            !
            ! A schema that was never built at all is a different mistake and still an error:
            ! parsing empty MAML text would report something about the text rather than the call.
            if (.not. schema%is_parsed()) then
                if (.not. schema%is_init()) then
                    error stop EP // "parquet_write_table: this schema has not been built; call " // &
                        "schema%init/%add_field (or load a MAML file) before writing with it"
                end if
                call parquet_parse_maml(schema)
            end if
        end if
        !
        want_metadata = present(metadata_keys)
        if (present(copy_metadata)) then
            if (present(metadata_keys) .and. copy_metadata) then
                error stop EP // "parquet_write_table: copy_metadata=.true. and metadata_keys= " // &
                    "cannot both be given; copy_metadata=.true. carries every key, metadata_keys= " // &
                    "only the listed ones (copy_metadata=.false. alongside metadata_keys= is " // &
                    "accepted, and carries the listed keys)"
            end if
            want_metadata = want_metadata .or. copy_metadata
        end if
        ! `own` is used in two unrelated situations, and only one of them existed before: a
        ! schema-less write has to BUILD a schema, and a metadata carry-over has to write onto a
        ! COPY of the caller's rather than the caller's own -- otherwise writing a second table
        ! with the same schema would inherit the first table's source-file metadata, silently and
        ! permanently. Both end up wanting a local schema, so they share one.
        use_own = want_metadata .or. .not. present(schema)
        if (.not. present(schema)) then
            call build_table_schema(table, trim(filename), own, nfields)
            ! Nothing resident is not an error -- it writes a genuinely empty file, which Arrow
            ! accepts and this library reopens as a 0-column, 0-row table. It cannot go through
            ! the generated schema, though: MAML requires at least one field, so there is no
            ! schema to build and the bare writer does the whole job.
            if (nfields == 0) then
                call write_empty_file(trim(filename), write_maml, compression, compression_level, &
                    chunk_size, use_threads, overwrite)
                return
            end if
        else if (want_metadata) then
            own = schema
        end if
        if (want_metadata) call carry_source_metadata(table, own, metadata_keys)
        !
        if (use_own) then
            call write_through_schema(table, own, filename, row_mask, write_maml, qc,         &
                compression, compression_level, chunk_size, use_threads, overwrite, release)
        else
            call write_through_schema(table, schema, filename, row_mask, write_maml, qc,      &
                compression, compression_level, chunk_size, use_threads, overwrite, release)
        end if
    end procedure parquet_write_table
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
            compression, compression_level, chunk_size, use_threads, overwrite, release)
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
        type(parquet_writer) :: writer
        character(len=:), allocatable :: fname, sfx
        integer :: i, nfields, idx
        logical :: do_release, was_empty
        !
        nfields = sch%get_num_fields()
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
            ! Whether it WAS is the whole of release=: what the caller had already read is theirs
            ! and stays, what this write had to read is this write's to give back. (A schema-less
            ! write names only resident columns, so nothing is ever released on that path.)
            was_empty = table%cache%cols(idx)%residency == RES_EMPTY
            call table_touch(table%cache, table_scope_of(table), idx, "parquet_write_table")
            call write_one_column(writer, table, idx, fname)
            ! Released here rather than after the loop, so peak residency is one column rather
            ! than every column the schema names.
            if (do_release .and. was_empty) call release_written_column(table%cache, idx)
        end do
        call parquet_close_writer(writer)
    end subroutine write_through_schema
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
    !! * **`parquet_row_index` is never written**, even when it is resident. It is this library's
    !!   own provenance column rather than the table's data, and having it appear unasked-for in an
    !!   output file is the more surprising of the two possible answers. Name it in a schema to
    !!   write it.
    !! * **`col_size:`/`array_size:` are declared `auto`**, never measured here. The writer resolves
    !!   both from the data at the first write exactly as it would with no schema at all, so this
    !!   procedure cannot get them wrong -- and the sidecar MAML, emitted at close, records the
    !!   resolved values.
    !!
    !! The `table:` name is the output file's stem, since a schema-less table has no schema name to
    !! take one from and MAML requires the key.
    subroutine build_table_schema(table, filename, sch, nfields)
        type(parquet_table), intent(in) :: table    !! the table being written.
        character(len=*), intent(in) :: filename    !! output parquet file, for the table: name.
        type(parquet_schema), intent(out) :: sch    !! the schema built from the descriptors.
        integer, intent(out) :: nfields             !! fields added; 0 means "write an empty file".
        character(len=:), allocatable :: stem, dtype, u
        logical :: is_vec, is_str
        integer :: i
        !
        nfields = 0
        call output_stem(filename, stem)
        call sch%init(stem)
        do i = 1, table%cache%ncols
            associate (slot => table%cache%cols(i))
                if (slot%residency /= RES_FULL) cycle
                if (.not. slot%supported) cycle
                if (slot%name == PARQUET_ROW_INDEX) cycle
                call schema_type_token(slot, dtype)
                is_vec = slot%width > 1
                is_str = slot%declared_kind == PK_STRING .or. slot%declared_kind == PK_STRING_VEC
                ! An UNALLOCATED allocatable actual makes an optional dummy absent (F2018
                ! 15.5.2.12), which is how a column with no unit gets no `unit:` key at all rather
                ! than an empty one -- deallocated first, since the previous column may have left
                ! one behind.
                if (allocated(u)) deallocate(u)
                if (allocated(slot%unit)) u = slot%unit
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
        ! No parquet_parse_maml here: %init/%add_field keep %cinfo in step as the schema is
        ! built. MAML still requires at least one field, so the caller checks `nfields` and takes
        ! the empty-file path instead of this one.
    end subroutine build_table_schema
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
    !
    !> The output file's stem -- its basename with any directory part and a trailing ".parquet"
    !! removed -- used as the generated schema's `table:` name, which MAML requires.
    subroutine output_stem(filename, stem)
        character(len=*), intent(in) :: filename           !! output parquet path.
        character(len=:), allocatable, intent(out) :: stem !! the stem, never empty.
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
    end subroutine output_stem
    !
    !> Gives back a column this write had to materialize, leaving the descriptor alone so the
    !! slot stays listed, queryable and re-readable -- `%evict_column`'s body, without its checks.
    !!
    !! The checks are not needed and are deliberately not repeated: the only caller runs this
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
    subroutine release_written_column(cache, idx)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        integer, intent(in) :: idx                        !! slot just written.
        !
        call cache%cols(idx)%values%clear()
        cache%cols(idx)%residency = RES_EMPTY
        cache%cols(idx)%user_populated = .false.
        cache%generation = cache%generation + 1_int64
    end subroutine release_written_column
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
