!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Bodies of the schema-building module procedures declared in parquet_core.f90's
!> interface block: parsing a MAML into a parquet_schema (parquet_parse_maml),
!> building a schema from scratch (schema%init/%add_field), and the central
!> parquet_parse_maml_lines parser, plus the private MAML-section helpers
!> (keyarray:/DOI/depends/keywords/col_map/protected_cols) it depends on.
submodule (parquet_core) parquet_metadata
    use iso_fortran_env, only: output_unit
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use parquet_bindings
    use parquet_settings, only: parquet_max_filter_rule_len, parquet_max_filter_depth, &
        parquet_max_filter_nodes, parquet_max_sort_keys, parquet_max_sort_key_len, &
        parquet_max_maml_line_len, parquet_valid_compressions, &
        parquet_resolve_writer_compression, parquet_get_default_use_threads, &
        parquet_emit_warning, parquet_emit_error_context
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    use parquet_strings, only: parquet_string_column, parquet_string
    use parquet_temporal, only: parquet_date, parquet_time, parquet_timestamp, &
        parquet_unit_seconds, parquet_unit_millis, parquet_unit_micros, parquet_unit_nanos, &
        parquet_ns_per_sec, parquet_ns_per_day, parquet_ns_to_sec, parquet_ns_to_day
    implicit none

    !> Maximum length, in characters, of one MAML source line -- shared by parquet_append_line's
    !! growth buffer and every fixed-length line-scratch variable in this submodule tree
    !! (parquet_metadata_maml.f90's file loaders and parquet_parse_maml_lines/parquet_parse_qc_maml
    !! below). parquet_load_maml_file/parquet_load_qc_maml_file enforce this at read time
    !! (error stop on a longer line) rather than silently truncating -- see CLAUDE.md's MAML
    !! parser robustness notes. An alias *derived from* parquet_settings' public
    !! parquet_max_maml_line_len (reached by host association through parquet_core's own import),
    !! never a second copy of the number, so the two cannot drift.
    integer, parameter :: maml_max_line_len = parquet_max_maml_line_len

    ! ---- MAML load/parse/validate helpers (metadata-subtree-only private interfaces,
    ! relocated here from parquet_core.f90 -- see CLAUDE.md's private-helper relocation rule) ----
    interface
        !> Parses one qc: min:/max: value (already unquoted or not) into an
        !> operator + bound-text pair: a leading ">=", "<=", ">", or "<" (checked
        !> in that order, so the two-char operators are never mistaken for the
        !> one-char ones) is stripped and used as the operator; otherwise
        !> `default_op` applies (">=" for min:, "<=" for max:, matching the MAML
        !> format's documented inclusive-by-default convention). The remaining
        !> text is kept verbatim (not yet converted to a number) -- numeric
        !> parsing/validity is deferred to parquet_validate_maml_internal and to
        !> the write-time qc check, since it depends on the field's data_type,
        !> which may not be known yet at this point in parsing.
        module subroutine parquet_set_qc_bound(has_flag, op, raw, cvalue, default_op)
            logical, intent(out) :: has_flag !! .true. once set (a qc: min:/max: value was present).
            character(len=2), intent(out) :: op !! parsed operator (">=", "<=", "> ", or "< ").
            character(len=:), allocatable, intent(out) :: raw !! bound text, verbatim, operator prefix stripped.
            character(len=*), intent(in) :: cvalue !! raw qc: min:/max: value text (possibly quoted).
            character(len=*), intent(in) :: default_op !! operator to use when cvalue has no explicit prefix.
        end subroutine parquet_set_qc_bound

        !> Checks every top-level section name (and, for map-list sections
        !> like fields:/keyarray:, their items' sub-keys) in `lines` against
        !> the allowed_maml_sections/allowed_maml_nested_sections schema
        !> (src/parquet_metadata_maml.f90); appends one message per
        !> violation to `errors` (key presence only, not semantic content).
        !> Called by both parquet_validate_maml_internal (full schema mamls)
        !> and parquet_parse_qc_maml (qc-mamls, a strict subset of the schema).
        module subroutine parquet_validate_maml_sections(lines, errors)
            character(len=*), intent(in) :: lines(:) !! raw MAML source lines to check.
            character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
        end subroutine parquet_validate_maml_sections

        !> Appends `line` to `lines` at 1-based position `n`, growing the
        !> array if needed; internal MAML-source-lines plumbing.
        module subroutine parquet_append_line(lines, n, line)
            character(len=maml_max_line_len), allocatable, intent(inout) :: lines(:) !! line buffer being appended to.
            integer, intent(in) :: n !! number of lines already in use before this call.
            character(len=*), intent(in) :: line !! line text to store.
        end subroutine parquet_append_line

        !> Shared worker behind every add_metadata specific: warns (see
        !> parquet_metadata_warn_duplicate, unless warn=.false.) if `key` collides with a
        !> writer-reserved key or an already-present key, then appends one
        !> already-stringified key/value/description to `metadata%items` regardless.
        !>
        !> `datatype` records what the caller's value actually was, since a parquet key-value
        !> pair can only ever store text. An absent or blank `datatype` means "no companion
        !> entry" -- the value is then indistinguishable from a string on read, which is exactly
        !> right for the string specific and for a MAML-declared key. It never becomes an entry
        !> of `metadata%items` in its own right, and it is deliberately NOT forwarded to
        !> parquet_append_keyarray_line: a MAML-declared value is a string by design, so the
        !> write_maml=.true. sidecar stays untyped.
        module subroutine parquet_metadata_append_entry(metadata, key, value, description, warn, datatype)
            class(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value, already converted to text.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            logical, intent(in), optional :: warn !! .false. suppresses the duplicate-key warning (default .true.).
            character(len=*), intent(in), optional :: datatype !! type token for `value` ("int32", "boolean[]",
            !! ...); absent/blank means the value is a string and gets no companion entry.
        end subroutine parquet_metadata_append_entry

        !> Grows `columns` by one empty (default-initialized) entry and
        !> increments `n` to match; internal fields: parsing plumbing.
        module subroutine parquet_append_empty_cinfo(columns, n)
            type(parquet_column_type), allocatable, intent(inout) :: columns(:) !! column array being grown.
            integer, intent(inout) :: n !! number of entries in use before this call; incremented by 1.
        end subroutine parquet_append_empty_cinfo
    end interface

contains

    module procedure parquet_parse_maml_from_file
        if (schema%is_init()) then
            error stop "parquet_parse_maml: schema is already initialized -- call schema%clear() first " // &
                "to load a different .maml file into it"
        end if
        schema%maml = parquet_load_maml_file(filename)
        call parquet_parse_maml_lines(schema%maml%lines, schema%cinfo, schema%metadata)
        call parquet_merge_missing_columns(schema%maml, schema%cinfo)
        schema%metadata%source_maml_lines = schema%maml%lines
    end procedure parquet_parse_maml_from_file

    module procedure parquet_parse_maml_from_object
        type(parquet_metadata_entry), allocatable :: user_items(:)

        if (.not. allocated(schema%maml%lines)) &
            error stop "parquet_parse_maml: schema%maml has no loaded content " // &
                "(use the filename form, or populate schema%maml first)" ! GCOVR_EXCL_LINE
        call parquet_validate_maml(schema%maml)
        ! The parse is a destructive rebuild -- %cinfo and %metadata are both intent(out) on the
        ! worker -- so anything %add_metadata contributed is lifted out first and put back
        ! afterwards. Without this, a parse run after %add_metadata would silently drop the
        ! user's entries, which is what used to force the "%add_metadata only after the parse"
        ! rule. %n_base_items is exactly the boundary: everything past it is user-added.
        call take_user_metadata(schema%metadata, user_items)
        call parquet_parse_maml_lines(schema%maml%lines, schema%cinfo, schema%metadata)
        call parquet_merge_missing_columns(schema%maml, schema%cinfo)
        schema%metadata%source_maml_lines = schema%maml%lines
        call restore_user_metadata(schema%metadata, user_items)
    end procedure parquet_parse_maml_from_object

    ! ---- schema%init / schema%add_field: building a MAML from scratch -----
    ! These emit raw MAML text lines into schema%maml%lines, the same
    ! representation parquet_parse_maml_lines below parses -- and they keep
    ! %cinfo/%metadata in step with that text as they go (schema%init parses
    ! its own header lines; schema_sync_appended_lines parses each field's).
    ! So a schema built this way needs no parquet_parse_maml call of its own,
    ! unlike a MAML loaded from disk, and %add_field/%add_metadata may be
    ! interleaved in any order.

    !> Appends one line to maml%lines, growing the (deferred-length) array and
    !> renormalizing its element length to fit. A smaller, independent copy of
    !> the identically-named helper in parquet_maml_base_add_col_qc.f90 -- that
    !> one is private to a different module (parquet_maml_base sits below this
    !> one in the module stack) and cannot be reused here without a new public
    !> API neither add_col_qc nor add_field need for anything else.
    subroutine maml_push_line(maml, s)
        type(parquet_maml_file), intent(inout) :: maml !! schema being built; gains one more source line.
        character(len=*), intent(in) :: s !! line to append.
        character(len=:), allocatable :: tmp(:)
        integer :: n, newlen, i

        if (allocated(maml%lines)) then
            n = size(maml%lines)
        else
            n = 0
        end if
        newlen = len_trim(s)
        if (n > 0) newlen = max(newlen, len(maml%lines))
        if (newlen < 1) newlen = 1

        allocate(character(len=newlen) :: tmp(n + 1))
        do i = 1, n
            tmp(i) = maml%lines(i)
        end do
        tmp(n + 1) = s
        call move_alloc(tmp, maml%lines)
    end subroutine maml_push_line

    !> Keeps a schema's parsed state in step with the raw MAML lines %add_field/%add_col_qc has
    !> just appended, so that building a schema in code needs no explicit parquet_parse_maml call
    !> at all. `n_before` is size(%maml%lines) as it stood before that call, so
    !> %maml%lines(n_before+1:) is exactly what the one call added.
    !>
    !> Those lines are parsed as a minimal one-field document through parquet_parse_maml_lines --
    !> the same worker a whole-file parse uses -- rather than through a second, hand-rolled field
    !> parser, so the two cannot drift. Parsing one field's own lines is flat in the field count;
    !> re-parsing the whole document once per %add_field would be quadratic.
    !>
    !> The WHOLE-DOCUMENT half of parquet_validate_maml is deliberately not run here, and the
    !> per-field half is (through parquet_validate_field_rules, the same code the document
    !> validator calls). What is skipped is exactly the set of rules that cannot fire for a
    !> schema built this way -- a missing table:, an empty fields:, an unknown top-level section
    !> or field sub-key, an extra: protected_cols: or col_map: naming a column that does not
    !> exist -- because %init requires a non-empty table: and emits only known top-level keys,
    !> %add_field rejects an empty and a duplicate name itself, and neither builder can emit an
    !> extra: section at all. A .maml file loaded from disk still validates in full. If a future
    !> change teaches either builder to emit extra:, col_map: or protected_cols:, that reasoning
    !> stops holding and the whole-document call has to come back.
    subroutine schema_sync_appended_lines(schema, n_before)
        class(parquet_schema), intent(inout) :: schema !! schema whose %maml%lines just grew.
        integer, intent(in) :: n_before !! size(%maml%lines) as it stood before the appending call.
        type(parquet_column_info) :: sub_cinfo !! parsed one-field sub-document.
        type(parquet_table_metadata) :: sub_metadata !! unused (a fields:-only document has no metadata).
        type(parquet_column_type), allocatable :: merged(:)
        character(len=:), allocatable :: doc(:)
        character(len=:), allocatable :: errors, name_suffix
        integer :: n_now, first, n_new, n_old, i, w

        n_now = 0
        if (allocated(schema%maml%lines)) n_now = size(schema%maml%lines)
        if (n_now <= n_before) return ! nothing appended (an all-blank %add_col_qc input adds nothing)

        ! A "fields:" header, if this call was the one that had to add it, belongs to the
        ! document rather than to the field entry -- the sub-document supplies its own.
        first = n_before + 1
        if (parquet_maml_key_matches(trim(adjustl(schema%maml%lines(first))), "fields:")) first = first + 1

        if (first <= n_now) then
            n_new = n_now - first + 1
            w = max(len(schema%maml%lines), 7)
            allocate(character(len=w) :: doc(n_new + 1))
            doc(1) = "fields:"
            do i = 1, n_new
                doc(i + 1) = schema%maml%lines(first + i - 1)
            end do
            call parquet_parse_maml_lines(doc, sub_cinfo, sub_metadata)

            if (allocated(sub_cinfo%col)) then
                if (size(sub_cinfo%col) > 0) then
                    errors = ""
                    do i = 1, size(sub_cinfo%col)
                        call parquet_validate_field_rules(sub_cinfo%col(i), errors)
                    end do
                    if (len_trim(errors) > 0) then
                        call maml_name_suffix(schema%maml, name_suffix)
                        error stop "parquet_schema%add_field: " // trim(errors) // name_suffix
                    end if

                    ! See g_maml_mutex in parquet_wrapper.cpp -- same grow-and-move_alloc pattern
                    ! as parquet_merge_missing_columns below.
                    call parquet_maml_lock()
                    n_old = 0
                    if (allocated(schema%cinfo%col)) n_old = size(schema%cinfo%col)
                    allocate(merged(n_old + size(sub_cinfo%col, kind=int64)))
                    if (n_old > 0) merged(1:n_old) = schema%cinfo%col
                    merged(n_old+1:) = sub_cinfo%col
                    call move_alloc(merged, schema%cinfo%col)
                    call parquet_maml_unlock()
                end if
            end if
        end if

        call append_source_lines(schema%metadata, schema%maml%lines, n_before + 1)

    end subroutine schema_sync_appended_lines

    !> Mirrors `lines(first:)` into %metadata%source_maml_lines, the verbatim MAML text a
    !> write_maml=.true. sidecar is written from, so an in-code schema's sidecar stays complete
    !> without an explicit parse. Appending is correct however the two arrays have diverged:
    !> %add_metadata inserts its keyarray: block *before* fields:, so a new field entry still
    !> belongs at the end. A no-op until schema%init has established the array.
    !>
    !> Takes the whole array plus a start index rather than a section, and is a module-level
    !> helper rather than a procedure contained in its caller: passing a section of a
    !> deferred-length allocatable character array to an assumed-length dummy declared inside a
    !> submodule's module procedure is the shape that ICEs gfortran 15.2 (see also the
    !> parquet_parse_protected_cols relay note in CLAUDE.md's "Nested submodule tree").
    subroutine append_source_lines(metadata, lines, first)
        type(parquet_table_metadata), intent(inout) :: metadata !! gains the same lines.
        character(len=*), intent(in) :: lines(:) !! the schema's full %maml%lines.
        integer, intent(in) :: first !! 1-based index of the first newly appended line.
        character(len=:), allocatable :: grown(:)
        integer :: n_have, n_new, k, newlen

        if (.not. allocated(metadata%source_maml_lines)) return
        n_new = size(lines) - first + 1
        if (n_new <= 0) return ! GCOVR_EXCL_LINE (the caller returns early when nothing was appended)
        n_have = size(metadata%source_maml_lines)
        newlen = max(len(metadata%source_maml_lines), len(lines))
        allocate(character(len=newlen) :: grown(n_have + n_new))
        do k = 1, n_have
            grown(k) = metadata%source_maml_lines(k)
        end do
        do k = 1, n_new
            grown(n_have + k) = lines(first + k - 1)
        end do
        call move_alloc(grown, metadata%source_maml_lines)
    end subroutine append_source_lines

    !> Appends to `errors` every rule violation that one parsed field carries ON ITS OWN --
    !> everything parquet_validate_maml checks about a field without looking at any other field
    !> or at the document around it. Shared, deliberately, by the two places that need it:
    !> parquet_validate_maml_internal (a whole .maml document, once per field) and
    !> schema_sync_appended_lines (one field at a time, as %add_field builds a schema in code).
    !> A second copy of these rules would let the two routes disagree about what a valid field is,
    !> which is exactly what an in-code schema that is never explicitly parsed would then hide.
    !>
    !> Cross-field and whole-document rules stay with the caller: an empty or duplicate name, a
    !> missing table:, an empty fields:, an unknown section or sub-key, extra: protected_cols:
    !> and col_map:. %add_field enforces the name rules itself and can emit none of the rest.
    subroutine parquet_validate_field_rules(col, errors)
        type(parquet_column_type), intent(in) :: col !! one parsed field.
        character(len=:), allocatable, intent(inout) :: errors !! accumulating "...; "-joined message.
        character(len=:), allocatable :: cur_name
        real(real64) :: qc_bound_value

        cur_name = trim(col%name)

        if (.not. parquet_data_type_token_valid(col%data_type)) then
            errors = errors // "field '" // cur_name // "' has invalid data_type '" // &
                trim(col%data_type) // "'; "
        end if

        if (col%col_size == size_invalid_sentinel) then
            errors = errors // "field '" // cur_name // &
                "' has an invalid col_size (must be a positive integer or 'auto'); "
        end if
        if (col%array_size == size_invalid_sentinel) then
            errors = errors // "field '" // cur_name // &
                "' has an invalid array_size (must be a positive integer or 'auto'); "
        end if
        if (col%array_size == parquet_size_auto .and. trim(col%data_type) /= "string") then
            errors = errors // "field '" // cur_name // &
                "' declares array_size: auto, which only applies to string columns; "
        end if

        ! qc: is not supported for temporal (date/time/timestamp) columns yet -- reject
        ! it with a clear message rather than silently ignoring a declared bound.
        select case (trim(col%data_type))
        case ("date", "time", "timestamp")
            if (col%has_qc_min .or. col%has_qc_max) then
                errors = errors // "field '" // cur_name // "' declares qc:, which is not " // &
                    "supported for a " // trim(col%data_type) // " column; "
            end if
        end select

        ! qc: min: must use a lower-bound operator (>= or >) and qc: max: an upper-bound
        ! operator (<= or <); the opposite direction (e.g. min: '< 5') is a nonsensical bound.
        ! This is a purely syntactic check, applied to every enforced type (numeric and string
        ! alike); boolean's qc: is silently ignored entirely (see the numeric block below), so
        ! it's exempt here too.
        if (trim(col%data_type) /= "boolean") then
            if (col%has_qc_min .and. col%qc_min_op(1:1) == "<") then
                errors = errors // "field '" // cur_name // "' has a qc: min value with a '" // &
                    trim(col%qc_min_op) // "' operator; min: accepts only >= or > " // &
                    "(use max: for an upper bound); "
            end if
            if (col%has_qc_max .and. col%qc_max_op(1:1) == ">") then
                errors = errors // "field '" // cur_name // "' has a qc: max value with a '" // &
                    trim(col%qc_max_op) // "' operator; max: accepts only <= or < " // &
                    "(use min: for a lower bound); "
            end if
        end if

        ! qc: min:/max: numeric convertibility only applies to the numeric types; string uses
        ! its bound as a literal (nothing to convert, so it can't fail), and boolean's qc: is
        ! always silently ignored (never enforced), so it isn't checked here.
        select case (trim(col%data_type))
        case ("int32", "int64", "float32", "float64")
            if (col%has_qc_min) then
                if (.not. parquet_qc_numeric_bound(col%qc_min_raw, col%data_type, qc_bound_value)) then
                    errors = errors // "field '" // cur_name // "' has an invalid qc: min value '" // &
                        trim(col%qc_min_raw) // "' for data_type " // trim(col%data_type) // "; "
                end if
            end if
            if (col%has_qc_max) then
                if (.not. parquet_qc_numeric_bound(col%qc_max_raw, col%data_type, qc_bound_value)) then
                    errors = errors // "field '" // cur_name // "' has an invalid qc: max value '" // &
                        trim(col%qc_max_raw) // "' for data_type " // trim(col%data_type) // "; "
                end if
            end if
        end select
    end subroutine parquet_validate_field_rules

    !> Lifts a schema's user-added metadata entries -- everything past %n_base_items, i.e.
    !> everything %add_metadata contributed -- out of `metadata`, ahead of a parse that is about
    !> to rebuild it from scratch. `items` comes back unallocated when there is nothing to keep.
    subroutine take_user_metadata(metadata, items)
        type(parquet_table_metadata), intent(in) :: metadata !! schema metadata about to be rebuilt.
        type(parquet_metadata_entry), allocatable, intent(out) :: items(:) !! preserved user entries.
        integer :: n

        if (.not. allocated(metadata%items)) return
        n = size(metadata%items)
        if (n <= metadata%n_base_items) return
        items = metadata%items(metadata%n_base_items+1:n)
    end subroutine take_user_metadata

    !> Puts the entries take_user_metadata preserved back onto the freshly rebuilt `metadata`, in
    !> their original order and past the new %n_base_items boundary, so %clear_metadata still
    !> discards exactly them. Appends directly rather than re-entering %add_metadata on purpose:
    !> a re-entry would re-run the duplicate-key check and re-print a WARNING the caller already
    !> saw when the entry was first added (%init establishes the base entries immediately, so a
    !> collision is reported there, not here). Each entry's keyarray: line in the write_maml
    !> sidecar is regenerated, since %source_maml_lines was reset by the parse.
    subroutine restore_user_metadata(metadata, items)
        type(parquet_table_metadata), intent(inout) :: metadata !! freshly rebuilt schema metadata.
        type(parquet_metadata_entry), allocatable, intent(in) :: items(:) !! entries to re-append.
        type(parquet_metadata_entry), allocatable :: merged(:)
        integer :: n_old, i

        if (.not. allocated(items)) return
        if (size(items) == 0) return ! GCOVR_EXCL_LINE (take_user_metadata never returns an empty array)

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        n_old = 0
        if (allocated(metadata%items)) n_old = size(metadata%items)
        allocate(merged(n_old + size(items, kind=int64)))
        if (n_old > 0) merged(1:n_old) = metadata%items
        merged(n_old+1:) = items
        call move_alloc(merged, metadata%items)
        call parquet_maml_unlock()

        if (allocated(metadata%source_maml_lines)) then
            do i = 1, size(items)
                call parquet_append_keyarray_line(metadata%source_maml_lines, &
                    trim(items(i)%key), trim(items(i)%value), trim(items(i)%description))
            end do
        end if
    end subroutine restore_user_metadata

    ! ---- add_metadata duplicate-key warning (parquet_metadata_append_entry's shared
    ! pre-append check) -- plain contained procedures here so both parquet_metadata_base
    ! and parquet_metadata_get (siblings under this submodule) could reach them by host
    ! association if ever needed; only parquet_metadata_base actually calls them today. ----

    !> .true. if `key` already has an entry in metadata%items (exact,
    !! case-sensitive match, mirroring parquet_metadata_find_index's own read-side lookup).
    logical function parquet_metadata_key_exists(metadata, key) result(exists)
        class(parquet_table_metadata), intent(in) :: metadata !! table metadata to search.
        character(len=*), intent(in) :: key !! key to look for.
        integer :: i

        exists = .false.
        if (.not. allocated(metadata%items)) return
        do i = 1, size(metadata%items)
            if (.not. allocated(metadata%items(i)%key)) cycle
            if (trim(metadata%items(i)%key) == trim(key)) then
                exists = .true.
                return
            end if
        end do
    end function parquet_metadata_key_exists

    !> Looks up this table metadata's own "table" entry to name the table in a
    !> duplicate-key warning message -- always present by the time %add_metadata is
    !> reachable (schema%init requires table=, and a loaded MAML requires a top-level
    !> table: key), so the "(unknown)" fallback is defensive only. A subroutine rather
    !> than a character(len=:), allocatable function (see CLAUDE.md's "Compiler & language
    !> gotchas" -- GCC PR113797), since add_metadata is a public, per-thread-callable binding.
    subroutine parquet_metadata_table_name(metadata, name)
        class(parquet_table_metadata), intent(in) :: metadata !! table metadata to search.
        character(len=:), allocatable, intent(out) :: name !! this table's declared name, or "(unknown)" if absent.
        integer :: i

        name = "(unknown)"
        if (.not. allocated(metadata%items)) return
        do i = 1, size(metadata%items)
            if (.not. allocated(metadata%items(i)%key)) cycle
            if (trim(metadata%items(i)%key) == "table") then
                name = trim(metadata%items(i)%value)
                return
            end if
        end do
    end subroutine parquet_metadata_table_name

    !> Warns (unless warn=.false.) when an %add_metadata call is about to write a key
    !> that collides with one of three reserved categories, checked in this order so at
    !> most one warning prints per call: (C) a key the parquet writer always injects
    !> itself into the output file's key-value metadata (build_file_metadata in
    !> parquet_wrapper.cpp) -- checked unconditionally, not as a duplicate; (A) a MAML
    !> top-level scalar key from schema%init's own keyword list, but only if that key
    !> already has an entry (i.e. the MAML source actually declared it -- calling
    !> add_metadata("author", ...) on a schema whose MAML never declared author: does not
    !> warn); (B) any other key that already has an entry, covering everything not
    !> covered by A/C -- indexed keys (comment_1, DOI_1, ...), keyarray:-supplied keys,
    !> and plain user duplicates alike. Called from parquet_metadata_append_entry before
    !> the new entry is appended, so the existence checks see pre-append state. Applies
    !> uniformly to every %add_metadata call, including the MAML parser's own internal
    !> ones (see parquet_parse_maml_lines) -- the library itself should not silently
    !> write a duplicate key any more than a caller should.
    subroutine parquet_metadata_warn_duplicate(metadata, key, warn)
        class(parquet_table_metadata), intent(in) :: metadata !! table metadata about to gain a new entry.
        character(len=*), intent(in) :: key !! key about to be added.
        logical, intent(in), optional :: warn !! .false. suppresses every warning below (default .true.).
        !> Keys the writer always injects itself (build_file_metadata, parquet_wrapper.cpp) --
        !! IVOA.VOTable-Parquet.content is deliberately excluded (only emitted when the
        !! schema has columns, so it is not a fixed collision risk the way these three are).
        character(len=29), parameter :: writer_keys(3) = &
            [character(len=29) :: "IVOA.VOTable-Parquet.version", "DATE", "name"]
        !> MAML top-level scalar keys settable via schema%init's own keyword arguments
        !! (schema_init) -- deliberately excludes indexed/derived keys (comment_1, DOI_1,
        !! ...) and keywords: (not an %init argument), which fall through to category B.
        character(len=12), parameter :: maml_init_keys(9) = [character(len=12) :: &
            "table", "survey", "dataset", "version", "date", "author", "description", "license", "maml_version"]
        character(len=:), allocatable :: table_name
        logical :: warn_value
        integer :: i

        warn_value = .true.
        if (present(warn)) warn_value = warn
        if (.not. warn_value) return

        call parquet_metadata_table_name(metadata, table_name)

        do i = 1, size(writer_keys)
            if (trim(writer_keys(i)) /= trim(key)) cycle
            call parquet_emit_warning("add_metadata: key '" // trim(key) // "' in table '" // table_name // &
                "' is reserved for the parquet writer's own internal file metadata -- this entry " // &
                "will be duplicated in the output file")
            return
        end do

        if (.not. parquet_metadata_key_exists(metadata, key)) return

        do i = 1, size(maml_init_keys)
            if (trim(maml_init_keys(i)) /= trim(key)) cycle
            call parquet_emit_warning("add_metadata: key '" // trim(key) // "' in table '" // table_name // &
                "' already exists from the MAML source -- this entry will be duplicated")
            return
        end do

        call parquet_emit_warning("add_metadata: key '" // trim(key) // "' in table '" // table_name // &
            "' already exists -- this entry will be duplicated")
    end subroutine parquet_metadata_warn_duplicate

    !> True if any line of maml%lines equals `target` after trimming leading
    !> and trailing blanks (used for the fields: header check). Only ever
    !> called from schema_add_field, which requires schema%init (always
    !> pushes a "table:" line first) to have run -- so maml%lines is always
    !> allocated here.
    logical function maml_line_exists(maml, target) result(found)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=*), intent(in) :: target !! line text to look for (matched after trim(adjustl(...))).
        integer :: i

        found = .false.
        do i = 1, size(maml%lines)
            if (trim(adjustl(maml%lines(i))) == trim(target)) then
                found = .true.
                return
            end if
        end do
    end function maml_line_exists

    !> True if maml%lines already declares a fields: entry named `name`, i.e.
    !> a "- name: <name>" line. Only ever called from schema_add_field, which
    !> requires schema%init (always pushes a "table:" line first) to have
    !> run -- so maml%lines is always allocated here.
    logical function maml_field_name_exists(maml, name) result(found)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=*), intent(in) :: name !! field name to look for (case-sensitive).
        integer :: i, colon
        character(len=:), allocatable :: t, key, val, key_lower

        found = .false.
        do i = 1, size(maml%lines)
            t = trim(adjustl(maml%lines(i)))
            if (len(t) == 0) cycle
            if (t(1:1) /= "-") cycle
            t = trim(adjustl(t(2:)))
            colon = index(t, ":")
            if (colon <= 1) cycle
            key = trim(adjustl(t(1:colon-1)))
            call parquet_to_lower(key, key_lower)
            if (key_lower /= "name") cycle
            val = trim(adjustl(t(colon+1:)))
            if (len(val) >= 2) then
                if ((val(1:1) == '"' .and. val(len(val):len(val)) == '"') .or. &
                    (val(1:1) == "'" .and. val(len(val):len(val)) == "'")) then
                    val = val(2:len(val)-1) ! GCOVR_EXCL_LINE
                end if
            end if
            if (trim(val) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function maml_field_name_exists

    module procedure schema_init
        logical :: do_force
        type(parquet_column_info) :: header_only_cinfo !! unused (discarded -- see the parse below).

        do_force = .false.
        if (present(force)) do_force = force

        if (this%is_init() .and. .not. do_force) then
            error stop "parquet_schema%init: schema is already initialized"
        end if
        if (len_trim(table) == 0) then
            error stop "parquet_schema%init: table must not be empty"
        end if

        if (do_force) then
            ! Full reset: this%init(..., force=.true.) behaves exactly like a
            ! first-time %init, discarding any fields/qc/metadata accumulated
            ! via %add_field/%add_col_qc/%add_metadata (and %cinfo/%metadata,
            ! if parquet_parse_maml had already been run) since whichever
            ! earlier %init call this one is overriding. %clear does exactly
            ! this same reset (it's the same "back to pristine" operation),
            ! just without immediately rebuilding afterward.
            call this%clear()
        end if

        ! Gives an in-memory schema (no source .maml file) a name anyway, so
        ! writer_maml_suffix and parquet_close_writer's missing-write error
        ! can still identify which schema they're complaining about.
        this%maml%name = "internal:" // trim(table)

        call maml_push_line(this%maml, "table: " // trim(table))
        if (present(survey))       call maml_push_line(this%maml, "survey: " // trim(survey))
        if (present(dataset))      call maml_push_line(this%maml, "dataset: " // trim(dataset))
        if (present(version))     call maml_push_line(this%maml, "version: " // trim(version))
        if (present(date))         call maml_push_line(this%maml, "date: " // trim(date))
        if (present(author))      call maml_push_line(this%maml, "author: " // trim(author))
        if (present(description)) call maml_push_line(this%maml, "description: " // trim(description))
        if (present(license))     call maml_push_line(this%maml, "license: " // trim(license))
        if (present(maml_version)) call maml_push_line(this%maml, "MAML_version: " // trim(maml_version))

        ! Parses the header lines just pushed straight into %metadata, so an in-code schema
        ! carries its table:/author:/... entries from the moment it is initialized rather than
        ! only after an explicit parquet_parse_maml. Two things depend on that: %add_metadata
        ! can run immediately (its duplicate-key check needs the base entries to check against,
        ! and %n_base_items must be right for %clear_metadata), and parquet_open_writer copies
        ! %metadata%items into the file's key-value metadata whether or not a parse ever ran.
        ! The scratch cinfo is discarded on purpose -- a header-only document has no fields:,
        ! and letting an empty %cinfo%col through would make %is_parsed() answer .true. for a
        ! schema that has not declared a single field yet.
        call parquet_parse_maml_lines(this%maml%lines, header_only_cinfo, this%metadata)
        this%metadata%source_maml_lines = this%maml%lines

        this%is_initialized = .true.
    end procedure schema_init

    module procedure parquet_schema_new
        call this%init(table=table, survey=survey, dataset=dataset, version=version, date=date, &
                author=author, description=description, license=license, maml_version=maml_version)
    end procedure parquet_schema_new

    module procedure schema_is_init
        schema_is_init = this%is_initialized .or. allocated(this%cinfo%col)
    end procedure schema_is_init

    module procedure schema_is_parsed
        schema_is_parsed = allocated(this%cinfo%col)
    end procedure schema_is_parsed

    module procedure schema_clear
        if (allocated(this%maml%name)) deallocate(this%maml%name)
        if (allocated(this%maml%lines)) deallocate(this%maml%lines)
        if (allocated(this%maml%missing_columns)) deallocate(this%maml%missing_columns)
        if (allocated(this%maml%col_map)) deallocate(this%maml%col_map)
        this%maml%user_maml = .false.
        if (allocated(this%cinfo%col)) deallocate(this%cinfo%col)
        if (allocated(this%metadata%items)) deallocate(this%metadata%items)
        if (allocated(this%metadata%source_maml_lines)) deallocate(this%metadata%source_maml_lines)
        this%metadata%n_base_items = 0
        this%is_initialized = .false.
    end procedure schema_clear

    !> "" if maml%name was never set; otherwise " (maml: X)". schema%add_field
    !> can only be reached after schema%init (checked below), and %init/
    !> parquet_schema(...) always give an in-memory schema a name
    !> ("internal:<table>"), so this is effectively always populated for
    !> every add_field error below it.
    subroutine maml_name_suffix(maml, suffix)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=:), allocatable, intent(out) :: suffix !! " (maml: X)"-style suffix, or "".

        suffix = ""
        if (allocated(maml%name)) then
            if (len_trim(maml%name) > 0) suffix = " (maml: " // trim(maml%name) // ")"
        end if
    end subroutine maml_name_suffix

    !> Extracts this MAML's required top-level table: value, by scanning its raw source lines
    !> for the (unindented, scalar) "table:" key -- schema%init/schema_add_field's maml_push_line
    !> always writes it as the first line, but a MAML loaded from disk (schemas/maml_example*.maml)
    !> may declare it after other top-level keys, so every line is checked rather than assuming
    !> position. Returns "" (not error stop) if genuinely absent, so the caller decides how to
    !> report that -- in practice this never happens for a schema that reached schema_print_schema_info,
    !> since table: presence is enforced at schema%init/parquet_validate_maml time already.
    subroutine maml_table_name(maml, name)
        type(parquet_maml_file), intent(in) :: maml !! schema whose %maml%lines are scanned.
        character(len=:), allocatable, intent(out) :: name !! this MAML's table: value, or "" if not found.
        character(len=:), allocatable :: tline, key, cvalue, klo
        integer :: i

        name = ""
        if (.not. allocated(maml%lines)) return
        do i = 1, size(maml%lines)
            if (len_trim(maml%lines(i)) == 0) cycle
            if (maml%lines(i)(1:1) == " ") cycle ! indented (nested) line, not a top-level key.
            tline = trim(maml%lines(i))
            if (tline(1:1) == "-") cycle ! list item, not a scalar key line.
            if (index(tline, ":") == 0) cycle
            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle
            call parquet_to_lower(key, klo)
            if (klo == "table") then
                call parquet_unquote(cvalue, name)
                return
            end if
        end do
    end subroutine maml_table_name

    !> Guards every schema%add_metadata specific: the schema must have a metadata table to add
    !> to, which means %init has run (an in-code schema) or parquet_parse_maml has (a schema
    !> loaded from a file, or one whose %maml was populated directly). What it protects against
    !> is a genuinely too-early call: %init and the file-form parse both establish %metadata
    !> from scratch, so an entry added before either would be silently discarded rather than
    !> merely arriving early.
    !>
    !> It deliberately does NOT require the schema to be parsed. An in-code schema accumulates
    !> its entries from %init onwards and a later parquet_parse_maml preserves them (see
    !> parquet_parse_maml_from_object), so %add_field and %add_metadata may be interleaved in
    !> any order.
    subroutine check_schema_metadata_ready(schema)
        class(parquet_schema), intent(in) :: schema !! schema about to gain a %add_metadata entry.
        character(len=:), allocatable :: name_suffix

        if (.not. schema%is_init()) then
            call maml_name_suffix(schema%maml, name_suffix)
            error stop "parquet_schema%add_metadata: schema has no metadata table yet -- call " // &
                "schema%init(...) (or parquet_parse_maml) before add_metadata" // name_suffix
        end if
    end subroutine check_schema_metadata_ready

    module procedure schema_add_field
        character(len=:), allocatable :: miss_low
        character(len=32) :: buf
        logical :: have_qc_min, have_qc_max, have_qc_miss
        integer :: n_before !! size(%maml%lines) before this call appends anything.
        character(len=:), allocatable :: tlo1 !! scratch (unquote/to_lower).
        character(len=:), allocatable :: name_suffix !! scratch (maml_name_suffix).

        if (.not. this%is_initialized) then
            error stop "parquet_schema%add_field: call schema%init(...) before adding fields"
        end if

        if (len_trim(name) == 0) then
            call maml_name_suffix(this%maml, name_suffix)
            error stop "parquet_schema%add_field: field name must not be empty" // name_suffix
        end if

        if (maml_field_name_exists(this%maml, trim(name))) then
            call maml_name_suffix(this%maml, name_suffix)
            error stop "parquet_schema%add_field: duplicate field name '" // trim(name) // "'" // name_suffix
        end if

        if (.not. parquet_data_type_token_valid(data_type)) then
            call maml_name_suffix(this%maml, name_suffix)
            error stop "parquet_schema%add_field: field '" // trim(name) // "' has invalid data_type '" // &
                trim(data_type) // "'" // name_suffix
        end if

        call validate_qc_bound(qc_min, .true.)
        call validate_qc_bound(qc_max, .false.)

        if (present(qc_miss)) then
            if (len_trim(qc_miss) > 0) then
                call parquet_to_lower(trim(adjustl(qc_miss)), tlo1)
                miss_low = tlo1
                if (.not. (miss_low == "null" .or. miss_low == "na")) then
                    call maml_name_suffix(this%maml, name_suffix)
                    error stop "parquet_schema%add_field: invalid qc_miss value '" // trim(adjustl(qc_miss)) // &
                        "' for field '" // trim(name) // "' (expected Null/NA or empty)" // name_suffix
                end if
            end if
        end if

        ! Everything above this line only validates; from here on %maml%lines grows, and
        ! n_before is what lets schema_sync_appended_lines see exactly this field's own lines.
        n_before = size(this%maml%lines)

        if (.not. maml_line_exists(this%maml, "fields:")) call maml_push_line(this%maml, "fields:")

        call maml_push_line(this%maml, "- name: " // trim(name))
        if (present(unit)) call maml_push_line(this%maml, "  unit: " // trim(unit))
        if (present(info)) call maml_push_line(this%maml, "  info: " // trim(info))
        if (present(ucd))  call maml_push_line(this%maml, "  ucd: " // trim(ucd))
        call maml_push_line(this%maml, "  data_type: " // trim(data_type))

        if (present(array_size)) then
            if (array_size == parquet_size_auto) then
                call maml_push_line(this%maml, "  array_size: auto")
            else
                write(buf, '(I0)') array_size
                call maml_push_line(this%maml, "  array_size: " // trim(buf))
            end if
        end if
        if (present(col_size)) then
            if (col_size == parquet_size_auto) then
                call maml_push_line(this%maml, "  col_size: auto")
            else
                write(buf, '(I0)') col_size
                call maml_push_line(this%maml, "  col_size: " // trim(buf))
            end if
        end if

        have_qc_min = .false.
        if (present(qc_min)) have_qc_min = len_trim(qc_min) > 0
        have_qc_max = .false.
        if (present(qc_max)) have_qc_max = len_trim(qc_max) > 0
        ! PRESENCE, not non-emptiness: an explicit qc_miss="" is how a caller asks for Null
        ! validation (an empty qc: miss: is the only thing that turns it on -- see
        ! parquet_column_type%qc_allow_null), so it must reach the MAML text as a bare "miss:"
        ! line. Testing len_trim here instead would discard it, leaving the caller with a schema
        ! that silently validates nothing; qc_min/qc_max keep the len_trim test, since an empty
        ! bound is genuinely nothing to declare.
        have_qc_miss = present(qc_miss)

        if (have_qc_min .or. have_qc_max .or. have_qc_miss) then
            call maml_push_line(this%maml, "  qc:")
            if (present(qc_min)) then
                if (len_trim(qc_min) > 0) &
                    call maml_push_line(this%maml, "    min: '" // trim(adjustl(qc_min)) // "'")
            end if
            if (present(qc_max)) then
                if (len_trim(qc_max) > 0) &
                    call maml_push_line(this%maml, "    max: '" // trim(adjustl(qc_max)) // "'")
            end if
            if (present(qc_miss)) then
                if (len_trim(qc_miss) > 0) then
                    call maml_push_line(this%maml, "    miss: " // trim(adjustl(qc_miss)))
                else
                    call maml_push_line(this%maml, "    miss:")
                end if
            end if
        end if

        call schema_sync_appended_lines(this, n_before)

    contains

        !> Validates one qc_min/qc_max bound (absent or empty -- nothing to
        !> check). If an operator prefix is present it must point the right
        !> way (min: >=/>, max: <=/<) and be followed by a non-empty value; a
        !> bare value with no operator is accepted as-is. Mirrors the rule
        !> %add_col_qc enforces for its own min:/max: fields, checked
        !> independently here -- see schema_add_field's doc comment (parquet_core.f90)
        !> for why these two aren't unified into one implementation.
        subroutine validate_qc_bound(raw, is_min)
            character(len=*), intent(in), optional :: raw !! qc_min/qc_max text, absent or empty means nothing to check.
            logical, intent(in) :: is_min !! .true. when validating qc_min (accepts >=/>); .false. for qc_max (<=/<).
            character(len=:), allocatable :: t, rem
            character(len=2) :: op
            logical :: has_op

            if (.not. present(raw)) return
            t = trim(adjustl(raw))
            if (len_trim(t) == 0) return

            has_op = .true.
            if (index(t, ">=") == 1) then
                op = ">="; rem = trim(adjustl(t(3:)))
            else if (index(t, "<=") == 1) then
                op = "<="; rem = trim(adjustl(t(3:)))
            else if (index(t, ">") == 1) then
                op = "> "; rem = trim(adjustl(t(2:)))
            else if (index(t, "<") == 1) then
                op = "< "; rem = trim(adjustl(t(2:)))
            else
                has_op = .false.; rem = t
            end if

            if (has_op) then
                if (is_min .and. op(1:1) == "<") then
                    call maml_name_suffix(this%maml, name_suffix)
                    error stop "parquet_schema%add_field: qc_min for field '" // trim(name) // "' uses a '" // &
                        trim(op) // "' operator; qc_min accepts only >= or > (use qc_max for an upper bound)" // &
                        name_suffix
                end if
                if (.not. is_min .and. op(1:1) == ">") then
                    call maml_name_suffix(this%maml, name_suffix)
                    error stop "parquet_schema%add_field: qc_max for field '" // trim(name) // "' uses a '" // &
                        trim(op) // "' operator; qc_max accepts only <= or < (use qc_min for a lower bound)" // &
                        name_suffix
                end if
                if (len_trim(rem) == 0) then
                    if (is_min) then
                        call maml_name_suffix(this%maml, name_suffix)
                        error stop "parquet_schema%add_field: bad qc_min value provided for field '" // trim(name) // &
                            "'" // name_suffix
                    else
                        call maml_name_suffix(this%maml, name_suffix)
                        error stop "parquet_schema%add_field: bad qc_max value provided for field '" // trim(name) // &
                            "'" // name_suffix
                    end if
                end if
            end if
        end subroutine validate_qc_bound

    end procedure schema_add_field

    !> Applies one comma-separated suffix token (a unit s/ms/us/ns, or "utc") to a temporal
    !> type's accumulating unit/utc/validity; an unrecognized token, or "utc" on a non-timestamp
    !> (allow_utc=.false.), sets valid=.false.
    subroutine apply_temporal_unit_token(tok, allow_utc, unit_sel, is_utc, valid)
        character(len=*), intent(in) :: tok !! one lowercased, trimmed suffix token.
        logical, intent(in) :: allow_utc !! .true. only for timestamp (time has no timezone).
        integer, intent(inout) :: unit_sel !! accumulating unit selector.
        logical, intent(inout) :: is_utc !! accumulating UTC flag.
        logical, intent(inout) :: valid !! cleared on an unrecognized/misplaced token.
        select case (tok)
        ! "s"/"sec"/"seconds" is deliberately NOT accepted here: Parquet's physical format has
        ! no seconds-resolution TIME/TIMESTAMP encoding at all (the format's own TimeUnit is
        ! MILLIS/MICROS/NANOS only) -- Arrow's writer silently downgrades a SECOND-unit column to
        ! MILLIS on write with no error, which would make a declared "timestamp[s]"/"time[s]"
        ! column silently report a different unit than declared once written. Rejecting it here
        ! (an "invalid data_type" error, same as any other malformed unit) fails fast instead of
        ! producing that silent mismatch. parquet_unit_seconds itself is still valid/useful for
        ! set_unix/to_unix (Unix-time interop, independent of what a file actually stores).
        case ("ms", "milli", "millis")
            unit_sel = parquet_unit_millis
        case ("us", "micro", "micros")
            unit_sel = parquet_unit_micros
        case ("ns", "nano", "nanos")
            unit_sel = parquet_unit_nanos
        case ("utc")
            if (allow_utc) then
                is_utc = .true.
            else
                valid = .false.
            end if
        case default
            valid = .false.
        end select
    end subroutine apply_temporal_unit_token

    !> Parses a time/timestamp `[unit(,utc)]` suffix, defaulting to microseconds when empty.
    subroutine parse_temporal_suffix(suffix, has_bracket, closed, allow_utc, unit_sel, is_utc, valid)
        character(len=*), intent(in) :: suffix !! the text between [ and ] (empty if no bracket).
        logical, intent(in) :: has_bracket !! whether a '[' was present.
        logical, intent(in) :: closed !! whether the bracket closed with ']' at the end.
        logical, intent(in) :: allow_utc !! .true. for timestamp; .false. for time.
        integer, intent(out) :: unit_sel !! resolved unit selector.
        logical, intent(inout) :: is_utc !! resolved UTC flag.
        logical, intent(inout) :: valid !! cleared on any malformed part.
        integer :: a, c
        character(len=:), allocatable :: s

        unit_sel = parquet_unit_micros
        if (has_bracket .and. .not. closed) then
            valid = .false.
            return
        end if
        s = trim(adjustl(suffix))
        if (len_trim(s) == 0) return
        a = 1
        do
            c = index(s(a:), ",")
            if (c == 0) then
                call apply_temporal_unit_token(trim(adjustl(s(a:))), allow_utc, unit_sel, is_utc, valid)
                exit
            else
                call apply_temporal_unit_token(trim(adjustl(s(a:a+c-2))), allow_utc, unit_sel, is_utc, valid)
                a = a + c
            end if
        end do
    end subroutine parse_temporal_suffix

    module procedure parquet_parse_temporal_type
        character(len=:), allocatable :: lo, base_tok, suffix
        integer :: lb, rb
        logical :: has_bracket, closed

        is_temporal = .false.
        valid = .true.
        is_utc = .false.
        unit_sel = 0
        call parquet_to_lower(trim(adjustl(token)), lo)

        lb = index(lo, "[")
        has_bracket = lb > 0
        if (has_bracket) then
            rb = index(lo, "]", back=.true.)
            closed = rb == len(lo) .and. rb > lb
            base_tok = lo(1:lb-1)
            if (closed) then
                suffix = lo(lb+1:rb-1)
            else
                suffix = ""
            end if
        else
            base_tok = lo
            suffix = ""
            closed = .true.
        end if

        select case (base_tok)
        case ("date")
            is_temporal = .true.
            base = "date"
            valid = .not. has_bracket ! date takes no unit/utc suffix
        case ("timestamp")
            is_temporal = .true.
            base = "timestamp"
            call parse_temporal_suffix(suffix, has_bracket, closed, .true., unit_sel, is_utc, valid)
        case ("time")
            is_temporal = .true.
            base = "time"
            call parse_temporal_suffix(suffix, has_bracket, closed, .false., unit_sel, is_utc, valid)
        case default
            base = lo
        end select
    end procedure parquet_parse_temporal_type

    module procedure parquet_data_type_token_valid
        character(len=:), allocatable :: base, lo
        integer :: unit_sel, j
        logical :: is_utc, is_temporal, valid

        call parquet_parse_temporal_type(token, base, unit_sel, is_utc, is_temporal, valid)
        if (is_temporal) then
            parquet_data_type_token_valid = valid
            return
        end if
        ! Non-temporal: an exact match against the numeric/string/boolean base tokens (same
        ! rule the original add_field used, so behavior for those is unchanged).
        call parquet_to_lower(trim(adjustl(token)), lo)
        parquet_data_type_token_valid = .false.
        do j = 1, size(valid_maml_data_types)
            if (trim(lo) == trim(valid_maml_data_types(j))) then
                parquet_data_type_token_valid = .true.
                return
            end if
        end do
    end procedure parquet_data_type_token_valid

    ! ---- parquet_schema flat convenience passthroughs ---------------------
    ! Each simply forwards to the matching procedure on %cinfo, %metadata or
    ! %maml. Absent optional arguments propagate unchanged.

    module procedure set_column_available
        call this%cinfo%set_column_available(name)
    end procedure set_column_available

    module procedure set_column_unavailable
        call this%cinfo%set_column_unavailable(name)
    end procedure set_column_unavailable

    module procedure schema_set_protected
        call this%cinfo%set_protected(name, protected)
    end procedure schema_set_protected

    module procedure schema_set_col_size
        call this%cinfo%set_col_size(name, col_size, force)
    end procedure schema_set_col_size

    module procedure schema_set_array_size
        call this%cinfo%set_array_size(name, array_size, force)
    end procedure schema_set_array_size

    module procedure schema_get_column_index
        schema_get_column_index = this%cinfo%get_column_index(name)
    end procedure schema_get_column_index

    module procedure schema_is_column_set
        schema_is_column_set = this%cinfo%is_column_set(name)
    end procedure schema_is_column_set

    module procedure schema_get_num_fields
        schema_get_num_fields = this%cinfo%get_num_fields()
    end procedure schema_get_num_fields

    module procedure schema_get_field_name
        call this%cinfo%get_field_name(index, name)
    end procedure schema_get_field_name

    module procedure schema_get_field_by_name
        call this%cinfo%get_field(name, data_type, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss)
    end procedure schema_get_field_by_name

    module procedure schema_get_field_by_index
        call this%cinfo%get_field(index, name, data_type, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss)
    end procedure schema_get_field_by_index

    module procedure schema_add_field_from
        character(len=:), allocatable :: data_type, unit, info, ucd, qc_min, qc_max, qc_miss
        integer :: array_size, col_size

        call source_schema%get_field(name, data_type, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss)
        call this%add_field(name, data_type, unit=unit, info=info, ucd=ucd, array_size=array_size, &
            col_size=col_size, qc_min=qc_min, qc_max=qc_max, qc_miss=qc_miss)
    end procedure schema_add_field_from

    module procedure schema_print_schema_info
        integer :: u, i, k, n_enabled, ios
        integer :: w_name, w_unit, w_type, w_len, w_ucd, w_info, w_total
        logical :: do_header, do_table_name, do_dash_before, do_dash_after_header, do_dash_after_fields
        logical :: opened_here, is_open, do_allow_uninitialized
        character(len=1) :: dchar
        character(len=:), allocatable :: pfx, name_suffix, dashline, table_name_val
        character(len=:), allocatable :: pad_name, pad_unit, pad_type, pad_len, pad_ucd !! scratch (pad).
        character(len=16) :: iq_action
        character(len=1024) :: iq_name
        character(len=32) :: lenbuf
        integer, allocatable :: enabled_idx(:)
        character(len=*), parameter :: hdr_name = "name", hdr_unit = "unit", hdr_type = "type", &
            hdr_len = "len", hdr_ucd = "ucd", hdr_info = "info"

        do_allow_uninitialized = .false.
        if (present(allow_uninitialized)) do_allow_uninitialized = allow_uninitialized

        call maml_name_suffix(this%maml, name_suffix)

        if (.not. allocated(this%cinfo%col)) then
            if (do_allow_uninitialized) return
            error stop "parquet_schema%print_schema_info: schema is not initialized (not parsed) -- call " // &
                "parquet_parse_maml on it first, or pass allow_uninitialized=.true. to skip silently" // name_suffix
        end if

        if (.not. present(unit) .and. .not. present(filename)) then
            error stop "parquet_schema%print_schema_info: either unit or filename must be given" // name_suffix
        end if

        ! Solicited output: verbosity="silent" and below turn this into a no-op. Placed after the
        ! argument validation above (so a bad call is still reported) but before the file is opened
        ! below (so a suppressed call does not leave an empty file behind as a side effect).
        if (parquet_output_is_suppressed()) return

        if (present(unit)) then
            inquire(unit=unit, opened=is_open)
            if (.not. is_open) then
                error stop "parquet_schema%print_schema_info: unit is not open" // name_suffix
            end if
            inquire(unit=unit, action=iq_action)
            if (trim(iq_action) == "READ") then
                error stop "parquet_schema%print_schema_info: unit is open for reading only, not writable" &
                    // name_suffix
            end if
            if (present(filename)) then
                inquire(unit=unit, name=iq_name)
                if (trim(iq_name) /= trim(filename)) then
                    error stop "parquet_schema%print_schema_info: filename '" // trim(filename) // &
                        "' does not match the file connected to unit (" // trim(iq_name) // ")" // name_suffix
                end if
            end if
            u = unit
            opened_here = .false.
        else
            open(newunit=u, file=filename, status="unknown", position="append", action="write", &
                form="formatted", iostat=ios)
            if (ios /= 0) then
                error stop "parquet_schema%print_schema_info: failed to open '" // trim(filename) // &
                    "' for writing" // name_suffix
            end if
            opened_here = .true.
        end if

        ! Enabled (is_set) column indices -- same filtering pattern used by
        ! parquet_open_writer to build writer%enabled_columns.
        n_enabled = 0
        if (allocated(this%cinfo%col)) then
            do i = 1, size(this%cinfo%col)
                if (this%cinfo%col(i)%is_set) n_enabled = n_enabled + 1
            end do
        end if
        allocate(enabled_idx(n_enabled))
        k = 0
        if (allocated(this%cinfo%col)) then
            do i = 1, size(this%cinfo%col)
                if (this%cinfo%col(i)%is_set) then
                    k = k + 1
                    enabled_idx(k) = i
                end if
            end do
        end if

        do_header = .true.
        if (present(header)) do_header = header
        do_table_name = .true.
        if (present(table_name)) do_table_name = table_name
        do_dash_before = .false.
        if (present(dash_before_header)) do_dash_before = dash_before_header
        do_dash_after_header = .true.
        if (present(dash_after_header)) do_dash_after_header = dash_after_header
        do_dash_after_fields = .false.
        if (present(dash_after_fields)) do_dash_after_fields = dash_after_fields
        dchar = "-"
        if (present(dash_char)) dchar = dash_char
        pfx = ""
        if (present(prefix)) pfx = prefix

        ! Column widths, derived from the longest value actually present (plus the header
        ! label itself, when printed) -- so header and data rows always line up.
        w_name = 0; w_unit = 0; w_type = 0; w_len = 0; w_ucd = 0; w_info = 0
        do k = 1, n_enabled
            i = enabled_idx(k)
            w_name = max(w_name, len_trim(this%cinfo%col(i)%name))
            w_unit = max(w_unit, len_trim(this%cinfo%col(i)%unit))
            w_type = max(w_type, len_trim(this%cinfo%col(i)%data_type))
            write(lenbuf, '(I0)') this%cinfo%col(i)%col_size
            w_len = max(w_len, len_trim(lenbuf))
            w_ucd = max(w_ucd, len_trim(this%cinfo%col(i)%ucd))
            w_info = max(w_info, len_trim(this%cinfo%col(i)%info))
        end do
        if (do_header) then
            w_name = max(w_name, len(hdr_name))
            w_unit = max(w_unit, len(hdr_unit))
            w_type = max(w_type, len(hdr_type))
            w_len  = max(w_len,  len(hdr_len))
            w_ucd  = max(w_ucd,  len(hdr_ucd))
            w_info = max(w_info, len(hdr_info))
        end if
        w_name = max(w_name, 1); w_unit = max(w_unit, 1); w_type = max(w_type, 1)
        w_len  = max(w_len, 1);  w_ucd  = max(w_ucd, 1);  w_info = max(w_info, 1)

        ! 5 single-space separators between the 6 columns.
        w_total = w_name + w_unit + w_type + w_len + w_ucd + w_info + 5
        dashline = pfx // repeat(dchar, w_total)

        if (do_dash_before) write(u, '(a)') dashline

        if (do_table_name) then
            call maml_table_name(this%maml, table_name_val)
            write(u, '(a)') pfx // "Table name: " // trim(table_name_val)
        end if

        if (do_header) then
            call pad(hdr_name, w_name, pad_name)
            call pad(hdr_unit, w_unit, pad_unit)
            call pad(hdr_type, w_type, pad_type)
            call pad(hdr_len, w_len, pad_len)
            call pad(hdr_ucd, w_ucd, pad_ucd)
            write(u, '(a)') pfx // pad_name // " " // pad_unit // " " // &
                pad_type // " " // pad_len // " " // pad_ucd // " " // hdr_info
        end if

        if (do_dash_after_header) write(u, '(a)') dashline

        do k = 1, n_enabled
            i = enabled_idx(k)
            write(lenbuf, '(I0)') this%cinfo%col(i)%col_size
            call pad(this%cinfo%col(i)%name, w_name, pad_name)
            call pad(this%cinfo%col(i)%unit, w_unit, pad_unit)
            call pad(this%cinfo%col(i)%data_type, w_type, pad_type)
            call pad(trim(lenbuf), w_len, pad_len)
            call pad(this%cinfo%col(i)%ucd, w_ucd, pad_ucd)
            write(u, '(a)') pfx // pad_name // " " // pad_unit // " " // pad_type // " " // &
                pad_len // " " // pad_ucd // " " // trim(this%cinfo%col(i)%info)
        end do

        if (do_dash_after_fields) write(u, '(a)') dashline

        if (opened_here) close(u)

    contains

        !> Left-justified, blank-padded to width w (at least len_trim(s) wide, so content is
        !> never truncated even if a caller-miscounted width somehow undershoots). A subroutine
        !> rather than a character(len=:), allocatable function (see CLAUDE.md's "Compiler &
        !> language gotchas" -- GCC PR113797), since print_schema_info is a public,
        !> per-thread-callable binding.
        subroutine pad(s, w, r)
            character(len=*), intent(in) :: s !! text to pad.
            integer, intent(in) :: w !! target width.
            character(len=:), allocatable, intent(out) :: r !! s, left-justified and blank-padded to width w.
            if (len_trim(s) >= w) then
                r = trim(s)
            else
                r = trim(s) // repeat(" ", w - len_trim(s))
            end if
        end subroutine pad

    end procedure schema_print_schema_info

    ! %add_col_qc/%set_col_qc deliberately do NOT get schema_add_field's incremental %cinfo sync.
    ! A qc-maml is a different dialect, not a smaller schema: its fields: entries declare a name
    ! and a qc: block and no data_type:, which parquet_parse_maml_lines rejects outright
    ! ("missing data_type in fields block") -- so a qc-maml has never been parsable by the
    ! schema parser at all, whether one field at a time or whole. It is read through
    ! parquet_parse_qc_maml instead, straight from %maml%lines, which is why
    ! parquet_load_qc_maml_file populates %maml and nothing else. Syncing here would need either
    ! a second field parser (which could then drift from this one) or a fabricated data_type in
    ! %cinfo; nothing reads %cinfo for a qc schema, so neither is worth having.
    module procedure schema_add_col_qc
        call this%maml%add_col_qc(qc_input, col_name)
    end procedure schema_add_col_qc

    module procedure schema_set_col_qc
        call this%maml%set_col_qc(col_name)
    end procedure schema_set_col_qc

    module procedure schema_add_metadata_int32
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_int32

    module procedure schema_add_metadata_int64
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_int64

    module procedure schema_add_metadata_float32
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, fmt, warn)
    end procedure schema_add_metadata_float32

    module procedure schema_add_metadata_float64
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, fmt, warn)
    end procedure schema_add_metadata_float64

    module procedure schema_add_metadata_logical
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_logical

    module procedure schema_add_metadata_string
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_string

    module procedure schema_add_metadata_int32_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_int32_array

    module procedure schema_add_metadata_int64_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_int64_array

    module procedure schema_add_metadata_float32_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, fmt, warn)
    end procedure schema_add_metadata_float32_array

    module procedure schema_add_metadata_float64_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, fmt, warn)
    end procedure schema_add_metadata_float64_array

    module procedure schema_add_metadata_logical_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_logical_array

    module procedure schema_add_metadata_string_array
        call check_schema_metadata_ready(this)
        call this%metadata%add_metadata(key, value, description, warn=warn)
    end procedure schema_add_metadata_string_array

    module procedure schema_clear_metadata
        call this%metadata%clear_metadata()
    end procedure schema_clear_metadata

    !> Restores every column in maml%missing_columns (populated by
    !> parquet_validate_user_maml for a user MAML that omits base-schema
    !> columns) back into cinfo%col, as disabled/deactivated entries -- so
    !> they exist for lookups but are never written and can't be re-enabled
    !> via set_column_available. No-op unless maml%user_maml is set and it
    !> actually has missing columns recorded.
    subroutine parquet_merge_missing_columns(maml, cinfo)
        type(parquet_maml_file), intent(in) :: maml !! validated user MAML, possibly with missing_columns recorded.
        type(parquet_column_info), intent(inout) :: cinfo !! schema gaining one disabled entry per missing column.
        type(parquet_column_type), allocatable :: merged(:)
        integer :: n_old, n_new, i

        if (.not. maml%user_maml) return
        if (.not. allocated(maml%missing_columns)) return
        if (size(maml%missing_columns) == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        n_old = 0
        if (allocated(cinfo%col)) n_old = size(cinfo%col)
        n_new = size(maml%missing_columns)

        allocate(merged(n_old + n_new))
        if (n_old > 0) merged(1:n_old) = cinfo%col

        do i = 1, n_new
            merged(n_old + i)%name = maml%missing_columns(i)%name
            merged(n_old + i)%unit = maml%missing_columns(i)%unit
            merged(n_old + i)%info = maml%missing_columns(i)%info
            merged(n_old + i)%ucd = maml%missing_columns(i)%ucd
            merged(n_old + i)%data_type = maml%missing_columns(i)%data_type
            merged(n_old + i)%time_unit = maml%missing_columns(i)%time_unit
            merged(n_old + i)%is_utc = maml%missing_columns(i)%is_utc
            merged(n_old + i)%array_size = maml%missing_columns(i)%array_size
            merged(n_old + i)%col_size = maml%missing_columns(i)%col_size
            merged(n_old + i)%is_set = .false.
            merged(n_old + i)%is_deactivated = .true.
            merged(n_old + i)%output_name = maml%missing_columns(i)%name
        end do

        call move_alloc(merged, cinfo%col)
        call parquet_maml_unlock()
    end subroutine parquet_merge_missing_columns

    !> Parses raw MAML source `lines` into `cinfo` (per-field schema/QC)
    !> and `metadata` (flat key-value table metadata); the shared worker
    !> behind parquet_parse_maml's file/object specifics.
    !> Plain contained subroutine (not a module procedure) for the same reason
    !> as parquet_parse_col_map above -- its body already lived in this file,
    !> the parquet_metadata parent, not a descendant submodule.
    subroutine parquet_parse_maml_lines(lines, cinfo, metadata)
        character(len=*), intent(in) :: lines(:) !! raw MAML source, one array element per line.
        type(parquet_column_info), intent(out) :: cinfo !! parsed per-field schema/QC state.
        type(parquet_table_metadata), intent(out) :: metadata !! parsed flat key-value table metadata.
        type(parquet_column_type), allocatable :: tmp(:)
        character(len=maml_max_line_len) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list, in_field_list, in_keyarray, in_doiarray, in_dependsarray, in_extra
        logical :: in_qc
        character(len=:), allocatable :: list_key, field_list_key, list_item
        character(len=:), allocatable :: ka_key, ka_value, ka_comment
        character(len=:), allocatable :: doi_value, doi_type
        character(len=:), allocatable :: depends_survey, depends_dataset, depends_table, depends_version
        character(len=:), allocatable :: keywords_value
        type(parquet_maml_col_map_entry), allocatable :: col_map(:)
        integer :: ios, n, i, j, list_item_idx, doi_idx, depends_idx
        character(len=32) :: idx_buf
        character(len=:), allocatable :: tlo2, tlo3, tlo8, tlo9, tlo12, tlo17, tlo30, &
            tlo33, tlo34, tlo37, tlo41, tlo42, tlo43, tlo44 !! scratch (to_lower).
        character(len=:), allocatable :: tuq1, tuq4, tuq5, tuq6, tuq7, tuq10, tuq11, tuq13, tuq14, tuq15, &
            tuq16, tuq18, tuq19, tuq20, tuq21, tuq22, tuq23, tuq24, tuq25, tuq26, tuq31, tuq32, tuq35, tuq36, &
            tuq38, tuq39, tuq40, tuq44 !! scratch (unquote).
        character(len=:), allocatable :: dt_base !! temporal base type scratch (parquet_parse_temporal_type).
        integer :: dt_unit !! temporal unit selector scratch.
        logical :: dt_utc, dt_is_temporal, dt_valid !! temporal utc/is-temporal/well-formed scratch.

        ! See g_maml_mutex in parquet_wrapper.cpp: this function's repeated
        ! "grow tmp(:), whole-array-assign the old contents in, move_alloc"
        ! pattern is not safely reentrant under genuine concurrent threads,
        ! even with -frecursive -- this lock keeps concurrent MAML parsing
        ! correct (serialized) rather than racing (recursive: this function
        ! calls other locked helpers, e.g. parquet_metadata_append_entry).
        call parquet_maml_lock()

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        in_field_list = .false.
        in_keyarray = .false.
        in_doiarray = .false.
        in_dependsarray = .false.
        in_extra = .false.
        in_qc = .false.
        list_item_idx = 0
        doi_idx = 0
        depends_idx = 0
        n = 0
        list_key = ""
        field_list_key = ""
        ka_key = ""
        ka_value = ""
        ka_comment = ""
        doi_value = ""
        doi_type = ""
        depends_survey = ""
        depends_dataset = ""
        depends_table = ""
        depends_version = ""
        keywords_value = ""
        if (allocated(metadata%items)) deallocate(metadata%items)

        do i = 1, size(lines)
            line = lines(i)

            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (in_keyarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            call parquet_unquote(cvalue, tuq1)
                            call parquet_to_lower(key, tlo2)
                            if (tlo2 == "key") ka_key = tuq1
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        call parquet_to_lower(key, tlo3)
                        select case (tlo3)
                        case ("key")
                            call parquet_unquote(cvalue, tuq4)
                            ka_key = tuq4
                        case ("value")
                            call parquet_unquote(cvalue, tuq5)
                            ka_value = tuq5
                        case ("comment")
                            call parquet_unquote(cvalue, tuq6)
                            ka_comment = tuq6
                        end select
                        cycle
                    else
                        call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
                        in_keyarray = .false.
                    end if
                end if

                if (in_doiarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
                        doi_value = ""
                        doi_type = ""
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            call parquet_unquote(cvalue, tuq7)
                            call parquet_to_lower(key, tlo8)
                            if (tlo8 == "doi") doi_value = tuq7
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        call parquet_to_lower(key, tlo9)
                        select case (tlo9)
                        case ("doi")
                            call parquet_unquote(cvalue, tuq10)
                            doi_value = tuq10
                        case ("type")
                            call parquet_unquote(cvalue, tuq11)
                            doi_type = tuq11
                        end select
                        cycle
                    else
                        call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
                        in_doiarray = .false.
                    end if
                end if

                if (in_dependsarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_depends_item(metadata, depends_idx, &
                            depends_survey, depends_dataset, depends_table, depends_version)
                        depends_survey = ""
                        depends_dataset = ""
                        depends_table = ""
                        depends_version = ""
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            call parquet_to_lower(key, tlo12)
                            select case (tlo12)
                            case ("survey")
                                call parquet_unquote(cvalue, tuq13)
                                depends_survey = tuq13
                            case ("dataset")
                                call parquet_unquote(cvalue, tuq14)
                                depends_dataset = tuq14
                            case ("table")
                                call parquet_unquote(cvalue, tuq15)
                                depends_table = tuq15
                            case ("version")
                                call parquet_unquote(cvalue, tuq16)
                                depends_version = tuq16
                            end select
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        call parquet_to_lower(key, tlo17)
                        select case (tlo17)
                        case ("survey")
                            call parquet_unquote(cvalue, tuq18)
                            depends_survey = tuq18
                        case ("dataset")
                            call parquet_unquote(cvalue, tuq19)
                            depends_dataset = tuq19
                        case ("table")
                            call parquet_unquote(cvalue, tuq20)
                            depends_table = tuq20
                        case ("version")
                            call parquet_unquote(cvalue, tuq21)
                            depends_version = tuq21
                        end select
                        cycle
                    else
                        call parquet_flush_depends_item(metadata, depends_idx, &
                            depends_survey, depends_dataset, depends_table, depends_version)
                        in_dependsarray = .false.
                    end if
                end if

                if (in_extra) then
                    ! extra:'s content is fully opaque/discarded: unlike the
                    ! generic in_list handler below, no metadata entry is
                    ! ever produced for it, whether its children are nested
                    ! maps or a top-level dash list (col_map: parsing, which
                    ! specifically looks inside extra:, works directly off
                    ! `lines`, independent of this skip).
                    if ((line(1:1) /= " " .and. index(tline, "-") /= 1)) then
                        in_extra = .false.
                    else
                        cycle
                    end if
                end if

                if (in_list) then
                    if (index(tline, "- ") == 1) then
                        list_item_idx = list_item_idx + 1
                        if (list_key == "comments" .or. list_key == "comment") then
                            write(idx_buf, '(I0)') list_item_idx
                            call parquet_unquote(tline(3:), tuq22)
                            call metadata%add_metadata("comment_" // trim(idx_buf), tuq22)
                        else if (list_key == "coauthors" .or. list_key == "coauthor") then
                            write(idx_buf, '(I0)') list_item_idx
                            call parquet_unquote(tline(3:), tuq23)
                            call metadata%add_metadata("coauthor_" // trim(idx_buf), tuq23)
                        else if (list_key == "keywords" .or. list_key == "keyword") then
                            if (len_trim(keywords_value) > 0) then
                                call parquet_unquote(tline(3:), tuq24)
                                keywords_value = trim(keywords_value) // ";" // trim(tuq24)
                            else
                                call parquet_unquote(tline(3:), tuq25)
                                keywords_value = trim(tuq25)
                            end if
                        else
                            ! A plain-string list under an ordinary section (survey:, license:,
                            ! ...) is DEFINED to produce one entry per item, all sharing that
                            ! key's name -- so warn=.false.: the duplicate-key warning exists to
                            ! catch a caller's own accidental %add_metadata collision, and firing
                            ! it here told a user their perfectly well-formed MAML was suspect.
                            ! Only the parser's own deliberate multi-entry emission is exempt.
                            call parquet_unquote(tline(3:), tuq26)
                            call metadata%add_metadata(list_key, tuq26, warn=.false.)
                        end if
                        cycle
                    else if (index(tline, ":") > 0 .and. line(1:1) /= " ") then
                        if (list_key == "keywords" .or. list_key == "keyword") then
                            call parquet_flush_keywords(metadata, keywords_value)
                            keywords_value = ""
                        end if
                        in_list = .false.
                    else
                        cycle
                    end if
                end if

                if (parquet_maml_key_matches(tline, "fields:")) in_fields = .true.
                if (in_fields) cycle

                if (parquet_maml_key_matches(tline, "keyarray:")) then
                    in_keyarray = .true.
                    ka_key = ""
                    ka_value = ""
                    ka_comment = ""
                    cycle
                end if

                if (parquet_maml_key_matches(tline, "dois:")) then
                    in_doiarray = .true.
                    doi_idx = 0
                    doi_value = ""
                    doi_type = ""
                    cycle
                end if

                if (parquet_maml_key_matches(tline, "depends:")) then
                    in_dependsarray = .true.
                    depends_idx = 0
                    depends_survey = ""
                    depends_dataset = ""
                    depends_table = ""
                    depends_version = ""
                    cycle
                end if

                if (parquet_maml_key_matches(tline, "extra:")) then
                    in_extra = .true.
                    cycle
                end if

                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle

                call parquet_to_lower(key, tlo30)
                key = tlo30

                if (key == "fields") then
                    in_fields = .true.
                else if (len_trim(cvalue) > 0) then
                    call parquet_unquote(cvalue, tuq31)
                    call metadata%add_metadata(key, tuq31)
                else
                    in_list = .true.
                    list_key = key
                    list_item_idx = 0
                end if
                cycle
            end if

            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. line(1:1) /= " ") exit

            if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                call parquet_append_empty_cinfo(tmp, n)
                have_current = .true.
                tline = trim(adjustl(tline(2:)))
                if (len_trim(tline) == 0) cycle
            end if

            if (.not. have_current) cycle

            if (in_field_list) then
                if (index(tline, "- ") == 1 .and. line(1:1) == " ") then
                    call parquet_unquote(tline(3:), tuq32)
                    list_item = tuq32
                    select case (field_list_key)
                    case ("ucd")
                        if (.not. allocated(tmp(n)%ucd) .or. len_trim(tmp(n)%ucd) == 0) then
                            tmp(n)%ucd = trim(list_item)
                        else
                            tmp(n)%ucd = trim(tmp(n)%ucd) // ";" // trim(list_item)
                        end if
                    end select
                    cycle
                else
                    in_field_list = .false.
                    field_list_key = ""
                end if
            end if

            if (in_qc) then
                call parquet_split_key_value(tline, key, cvalue)
                call parquet_to_lower(key, tlo33)
                select case (tlo33)
                case ("min")
                    call parquet_set_qc_bound(tmp(n)%has_qc_min, tmp(n)%qc_min_op, tmp(n)%qc_min_raw, cvalue, ">=")
                    cycle
                case ("max")
                    call parquet_set_qc_bound(tmp(n)%has_qc_max, tmp(n)%qc_max_op, tmp(n)%qc_max_raw, cvalue, "<=")
                    cycle
                case ("miss")
                    call parquet_unquote(cvalue, tuq44)
                    call parquet_to_lower(tuq44, tlo44)
                    tmp(n)%qc_allow_null = (trim(tlo44) == "null" .or. trim(tlo44) == "na")
                    cycle
                case default
                    in_qc = .false.
                end select
            end if

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            call parquet_to_lower(key, tlo34)
            select case (tlo34)
            case ("name")
                call parquet_unquote(cvalue, tuq35)
                tmp(n)%name = tuq35
            case ("unit")
                call parquet_unquote(cvalue, tuq36)
                tmp(n)%unit = tuq36
                call parquet_to_lower(tmp(n)%unit, tlo37)
                if (tlo37 == "unitless") tmp(n)%unit = ""
            case ("info")
                call parquet_unquote(cvalue, tuq38)
                tmp(n)%info = tuq38
            case ("ucd")
                if (len_trim(cvalue) > 0) then
                    call parquet_unquote(cvalue, tuq39)
                    tmp(n)%ucd = tuq39
                else
                    tmp(n)%ucd = ""
                    in_field_list = .true.
                    field_list_key = "ucd"
                end if
            case ("data_type")
                call parquet_unquote(cvalue, tuq40)
                call parquet_to_lower(tuq40, tlo41)
                cvalue = tlo41
                if (index(cvalue, "string") == 1) then
                    tmp(n)%data_type = "string"
                else
                    ! Split a temporal token (timestamp[us,utc], time[ms], date) into its base
                    ! type plus unit/utc; a malformed temporal token is stored verbatim so
                    ! parquet_validate_maml rejects it (parquet_data_type_token_valid).
                    call parquet_parse_temporal_type(cvalue, dt_base, dt_unit, dt_utc, dt_is_temporal, dt_valid)
                    if (dt_is_temporal .and. dt_valid) then
                        tmp(n)%data_type = dt_base
                        tmp(n)%time_unit = dt_unit
                        tmp(n)%is_utc = dt_utc
                    else
                        tmp(n)%data_type = cvalue
                    end if
                end if
            case ("array_size")
                if (len_trim(cvalue) == 0) then
                    tmp(n)%array_size = 1
                else
                    call parquet_to_lower(trim(adjustl(cvalue)), tlo42)
                    if (trim(tlo42) == "auto") then
                        tmp(n)%array_size = parquet_size_auto
                    else
                        read(cvalue, *, iostat=ios) tmp(n)%array_size
                        if (ios /= 0 .or. tmp(n)%array_size <= 0) tmp(n)%array_size = size_invalid_sentinel
                    end if
                end if
            case ("col_size")
                if (len_trim(cvalue) == 0) then
                    tmp(n)%col_size = 1
                else
                    call parquet_to_lower(trim(adjustl(cvalue)), tlo43)
                    if (trim(tlo43) == "auto") then
                        tmp(n)%col_size = parquet_size_auto
                    else
                        read(cvalue, *, iostat=ios) tmp(n)%col_size
                        if (ios /= 0 .or. tmp(n)%col_size <= 0) tmp(n)%col_size = size_invalid_sentinel
                    end if
                end if
            case ("qc")
                in_qc = .true.
            end select
        end do

        if (in_keyarray) call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        if (in_doiarray) call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        if (in_dependsarray) call parquet_flush_depends_item(metadata, depends_idx, &
            depends_survey, depends_dataset, depends_table, depends_version)
        if (in_list .and. (list_key == "keywords" .or. list_key == "keyword")) then
            call parquet_flush_keywords(metadata, keywords_value)
        end if

        ! Marks how many %metadata%items exist as of this parse -- schema%clear_metadata
        ! truncates back to this count, discarding only entries a later %add_metadata call adds.
        ! Set here rather than at the end of the subroutine because the fields-less early return
        ! below would otherwise skip it, leaving n_base_items at 0 and making %clear_metadata
        ! discard the document's own entries too. Everything after this point builds cinfo%col
        ! only; no further %items are appended.
        metadata%n_base_items = 0
        if (allocated(metadata%items)) metadata%n_base_items = size(metadata%items)

        if (n <= 0) then
            allocate(cinfo%col(0))
            call parquet_maml_unlock()
            return
        end if

        do i = 1, n
            if (.not. allocated(tmp(i)%name)) error stop "parquet_read_maml: missing field name in fields block"
            if (.not. allocated(tmp(i)%data_type)) error stop "parquet_read_maml: missing data_type in fields block"
            if (.not. allocated(tmp(i)%unit)) tmp(i)%unit = ""
            if (.not. allocated(tmp(i)%info)) tmp(i)%info = ""
            if (.not. allocated(tmp(i)%ucd)) tmp(i)%ucd = ""
            tmp(i)%is_set = .true.
        end do

        ! Apply this MAML's own col_map: (if any): a field declared under
        ! `output_name` in fields: is renamed in place to `internal_name`,
        ! keeping its originally-declared name as output_name. Unmapped
        ! fields keep output_name == name (identity). See
        ! parquet_column_type%output_name and parquet_parse_col_map.
        col_map = parquet_parse_col_map(lines)
        do i = 1, n
            tmp(i)%output_name = tmp(i)%name
            do j = 1, size(col_map)
                if (trim(col_map(j)%output_name) == trim(tmp(i)%name)) then
                    tmp(i)%name = col_map(j)%internal_name
                    exit
                end if
            end do
        end do

        ! extra: protected_cols: names this MAML's own fields: (matched
        ! by output_name, the name as literally declared under fields: in
        ! this file, before any col_map: rename) -- marked here so
        ! parquet_write_column can error stop if an is_valid mask with any
        ! .false. entry is ever passed for one of these columns.
        block
            character(len=:), allocatable :: protected_names(:)
            call parquet_parse_protected_cols(lines, protected_names)
            do i = 1, n
                do j = 1, size(protected_names)
                    if (trim(protected_names(j)) == trim(tmp(i)%output_name)) then
                        tmp(i)%is_protected = .true.
                        exit
                    end if
                end do
            end do
        end block

        call move_alloc(tmp, cinfo%col)

        call parquet_maml_unlock()
    end subroutine parquet_parse_maml_lines

    !> Appends a `- key: / value: / comment:` entry to the `keyarray:` block
    !> inside `lines` (the verbatim source MAML content kept in
    !> metadata%source_maml_lines), so that metadata added at runtime via
    !> add_metadata after parquet_read_maml is reflected in a later
    !> write_maml sidecar. Always appends; does not update an existing entry
    !> that has the same key. Inserted before `extra:` if present, else
    !> before `fields:`; synthesizes the `keyarray:` header itself if the
    !> source MAML did not already have one.
    !> Plain contained subroutine (not a module procedure) for the same reason
    !> as parquet_parse_col_map above -- its body already lived in this file,
    !> the parquet_metadata parent, not a descendant submodule.
    subroutine parquet_append_keyarray_line(lines, key, value, desc)
        character(len=:), allocatable, intent(inout) :: lines(:) !! verbatim source MAML lines being amended.
        character(len=*), intent(in) :: key !! metadata key for the new entry.
        character(len=*), intent(in) :: value !! metadata value for the new entry.
        character(len=*), intent(in) :: desc !! metadata description for the new entry (may be "").
        character(len=:), allocatable :: entries(:), new_lines(:)
        integer :: insert_pos, n_old, n_new, new_len
        logical :: need_header

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        call parquet_locate_keyarray_insert(lines, insert_pos, need_header)

        new_len = max(len(lines), 7+len(key), 9+len(value), 11+len(desc))

        if (need_header) then
            allocate(character(len=new_len) :: entries(4))
            entries(1) = "keyarray:"
            entries(2) = "- key: " // key
            entries(3) = "  value: " // value
            entries(4) = "  comment: " // desc
        else
            allocate(character(len=new_len) :: entries(3))
            entries(1) = "- key: " // key
            entries(2) = "  value: " // value
            entries(3) = "  comment: " // desc
        end if

        n_old = size(lines)
        n_new = size(entries)

        allocate(character(len=new_len) :: new_lines(n_old + n_new))
        if (insert_pos > 1) new_lines(1:insert_pos-1) = lines(1:insert_pos-1)
        new_lines(insert_pos:insert_pos+n_new-1) = entries
        if (insert_pos <= n_old) new_lines(insert_pos+n_new:) = lines(insert_pos:n_old)

        call move_alloc(new_lines, lines)
        call parquet_maml_unlock()
    end subroutine parquet_append_keyarray_line

    !> Locates where a new keyarray entry should be inserted in `lines`.
    !> If a top-level `keyarray:` header already exists, `insert_pos` points
    !> just past its last item (need_header = .false.). Otherwise, `insert_pos`
    !> points at the top-level `extra:` line if present, else at the top-level
    !> `fields:` line (always present), and need_header = .true.
    subroutine parquet_locate_keyarray_insert(lines, insert_pos, need_header)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        integer, intent(out) :: insert_pos !! 1-based line index to insert the new entry (or header) at.
        logical, intent(out) :: need_header !! .true. if a keyarray: header line must be synthesized too.
        integer :: i, n
        character(len=:), allocatable :: tline

        n = size(lines)
        need_header = .true.
        insert_pos = n + 1

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(tline, "keyarray:")) then
                need_header = .false.
                insert_pos = i + 1
                do while (insert_pos <= n)
                    if (len_trim(lines(insert_pos)) == 0) exit
                    if (lines(insert_pos)(1:1) /= " " .and. lines(insert_pos)(1:1) /= "-") exit
                    insert_pos = insert_pos + 1
                end do
                return
            end if
        end do

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(tline, "extra:")) then
                insert_pos = i
                return
            end if
        end do

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(tline, "fields:")) then
                insert_pos = i
                return
            end if
        end do
    end subroutine parquet_locate_keyarray_insert

    !> Appends one parsed `keyarray:` item (key/value/comment) as a table
    !> metadata entry; a no-op if `ka_key` is empty (an incomplete/malformed item).
    subroutine parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        character(len=*), intent(in) :: ka_key !! parsed key: value.
        character(len=*), intent(in) :: ka_value !! parsed value: value.
        character(len=*), intent(in) :: ka_comment !! parsed comment: value (used as the entry's description).

        if (len_trim(ka_key) == 0) return
        call metadata%add_metadata(trim(ka_key), trim(ka_value), trim(ka_comment))
    end subroutine parquet_flush_keyarray_item

    !> Appends one parsed `DOIs:` list item as a "DOI_N" table metadata entry
    !> (N = doi_idx, incremented here); a no-op if `doi_value` is empty.
    subroutine parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        integer, intent(inout) :: doi_idx !! running DOI count so far; incremented by 1 on a real flush.
        character(len=*), intent(in) :: doi_value !! parsed DOI: value.
        character(len=*), intent(in) :: doi_type !! parsed type: value (used as the entry's description).
        character(len=32) :: idx_buf

        if (len_trim(doi_value) == 0) return
        doi_idx = doi_idx + 1
        write(idx_buf, '(I0)') doi_idx
        call metadata%add_metadata("DOI_" // trim(idx_buf), trim(doi_value), trim(doi_type))
    end subroutine parquet_flush_doi_item

    !> Combines one `depends:` list entry's survey/dataset/table/version
    !> sub-keys into a single "survey;dataset;table;version" string, stored
    !> as table-level metadata "depends_N" (matching the coauthor_N/comment_N
    !> naming already used for other simple list sections). Skipped entirely
    !> if the entry had none of the four sub-keys set.
    subroutine parquet_flush_depends_item(metadata, depends_idx, survey, dataset, table, version)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        integer, intent(inout) :: depends_idx !! running depends: count so far; incremented by 1 on a real flush.
        character(len=*), intent(in) :: survey !! parsed survey: sub-key.
        character(len=*), intent(in) :: dataset !! parsed dataset: sub-key.
        character(len=*), intent(in) :: table !! parsed table: sub-key.
        character(len=*), intent(in) :: version !! parsed version: sub-key.
        character(len=32) :: idx_buf

        if (len_trim(survey) == 0 .and. len_trim(dataset) == 0 .and. &
            len_trim(table) == 0 .and. len_trim(version) == 0) return

        depends_idx = depends_idx + 1
        write(idx_buf, '(I0)') depends_idx
        call metadata%add_metadata("depends_" // trim(idx_buf), &
            trim(survey) // ";" // trim(dataset) // ";" // trim(table) // ";" // trim(version))
    end subroutine parquet_flush_depends_item

    !> Stores a `keywords:` (or `keyword:`) plain-string list as a single
    !> semicolon-separated "keywords" metadata entry, rather than one entry
    !> per item (which is what a generic plain-string list gets otherwise).
    subroutine parquet_flush_keywords(metadata, keywords_value)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining the keywords entry.
        character(len=*), intent(in) :: keywords_value !! semicolon-joined keywords: list text.

        if (len_trim(keywords_value) == 0) return ! GCOVR_EXCL_LINE
        call metadata%add_metadata("keywords", trim(keywords_value))
    end subroutine parquet_flush_keywords

    !> Parses a `col_map:` block nested inside `extra:` (col_map: is NOT a
    !> valid top-level MAML section) into (internal_name -> output_name)
    !> entries. Each list item is a single "<internal_name>: <output_name>"
    !> line (with its leading "- "), e.g.:
    !>   extra:
    !>     col_map:
    !>     - col_internal: col_user
    !> Unlike every other map-list section (fields:, keyarray:, ...), the key
    !> here IS the data (an arbitrary internal column name) rather than a
    !> fixed sub-key label, so this is a dedicated parser rather than a
    !> generic one. extra:'s own content is otherwise entirely unvalidated
    !> (see the "extra" entry in allowed_maml_sections), so this is a
    !> narrow, specific lookup rather than a generically-validated section.
    !> Returns a zero-size array if there is no extra:/col_map: section.
    !> Plain contained function (not a module procedure): its interface used
    !> to live in parquet_core.f90, but since its body already lived here in the
    !> parquet_metadata parent (not a descendant submodule), keeping it a
    !> module procedure after relocating the interface into this same file's
    !> own spec would mean parquet_metadata implementing its own spec-declared
    !> interface -- not the ancestor/descendant relationship module
    !> procedures require. Descendants reach it by host association (fact 3.6).
    function parquet_parse_col_map(lines) result(col_map)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        type(parquet_maml_col_map_entry), allocatable :: col_map(:) !! parsed (internal_name, output_name) entries.
        type(parquet_maml_col_map_entry), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue
        integer :: i, n, idx_extra, extra_end, idx_col_map, n_entries
        character(len=:), allocatable :: tuq1, tuq2 !! scratch (unquote).

        allocate(col_map(0))

        ! col_map: is only valid nested inside extra: (extra:'s own internal
        ! structure is otherwise entirely unvalidated/opaque -- see
        ! allowed_maml_sections -- so this is a dedicated, narrow lookup
        ! rather than a generically-validated section of its own).
        n = size(lines)
        idx_extra = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(lines(i), "extra:")) then
                idx_extra = i
                exit
            end if
        end do
        if (idx_extra == 0) return

        ! extra:'s block runs until the next top-level (non-indented) line.
        extra_end = n
        do i = idx_extra + 1, n
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " ") then
                extra_end = i - 1
                exit
            end if
        end do

        idx_col_map = 0
        do i = idx_extra + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            if (parquet_maml_key_matches(lines(i), "col_map:")) then
                idx_col_map = i
                exit
            end if
        end do
        if (idx_col_map == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        do i = idx_col_map + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle

            tline = trim(adjustl(lines(i)))
            if (tline(1:1) /= "-") exit
            tline = trim(adjustl(tline(2:)))
            if (len_trim(tline) == 0) cycle

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0 .or. len_trim(cvalue) == 0) cycle

            n_entries = size(col_map)
            allocate(tmp(n_entries + 1))
            if (n_entries > 0) tmp(1:n_entries) = col_map
            call parquet_unquote(key, tuq1)
            tmp(n_entries + 1)%internal_name = tuq1
            call parquet_unquote(cvalue, tuq2)
            tmp(n_entries + 1)%output_name = tuq2
            call move_alloc(tmp, col_map)
        end do
        call parquet_maml_unlock()
    end function parquet_parse_col_map

    module subroutine parquet_parse_protected_cols(lines, names)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        character(len=:), allocatable, intent(out) :: names(:) !! trimmed, unquoted protected column names.
        character(len=:), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue, token
        character(len=:), allocatable :: key_lower, unquoted
        integer :: i, n, idx_extra, extra_end, idx_key, n_names, p, sep
        integer :: maxlen

        maxlen = 0
        do i = 1, size(lines)
            maxlen = max(maxlen, len_trim(lines(i)))
        end do
        allocate(character(len=max(maxlen,1)) :: names(0))

        n = size(lines)
        idx_extra = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. parquet_maml_key_matches(lines(i), "extra:")) then
                idx_extra = i
                exit
            end if
        end do
        if (idx_extra == 0) return

        extra_end = n
        do i = idx_extra + 1, n
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " ") then
                extra_end = i - 1
                exit
            end if
        end do

        idx_key = 0
        do i = idx_extra + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            tline = trim(adjustl(lines(i)))
            call parquet_split_key_value(tline, key, cvalue)
            call parquet_to_lower(trim(key), key_lower)
            if (key_lower == "protected_cols") then
                idx_key = i
                exit
            end if
        end do
        if (idx_key == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (len_trim(cvalue) > 0) then
            ! Scalar semicolon-separated form: protected_cols: col1;col2; col3
            cvalue = trim(adjustl(cvalue))
            p = 1
            do while (p <= len(cvalue))
                sep = index(cvalue(p:), ";")
                if (sep == 0) then
                    token = cvalue(p:)
                    p = len(cvalue) + 1
                else
                    token = cvalue(p:p+sep-2)
                    p = p + sep
                end if
                call parquet_unquote(token, unquoted)
                token = trim(adjustl(unquoted))
                if (len_trim(token) > 0) then
                    n_names = size(names)
                    allocate(character(len=len(names)) :: tmp(n_names + 1))
                    if (n_names > 0) tmp(1:n_names) = names
                    tmp(n_names + 1) = token
                    call move_alloc(tmp, names)
                end if
            end do
            call parquet_maml_unlock()
            return
        end if

        ! Dash-list form: protected_cols: (empty) followed by "- col1" lines.
        do i = idx_key + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            tline = trim(adjustl(lines(i)))
            if (tline(1:1) /= "-") exit
            tline = trim(adjustl(tline(2:)))
            call parquet_unquote(tline, unquoted)
            token = trim(adjustl(unquoted))
            if (len_trim(token) == 0) cycle

            n_names = size(names)
            allocate(character(len=len(names)) :: tmp(n_names + 1))
            if (n_names > 0) tmp(1:n_names) = names
            tmp(n_names + 1) = token
            call move_alloc(tmp, names)
        end do
        call parquet_maml_unlock()
    end subroutine parquet_parse_protected_cols

    !> gfortran 15.2.0 ICE workaround: a direct call to parquet_parse_protected_cols from a
    !> submodule nested two levels under parquet (e.g. parquet:parquet_metadata:parquet_metadata_maml)
    !> reproducibly crashes the compiler (confirmed: removing the call, or flattening the caller
    !> back to one level of nesting, both avoid it -- isolated to this exact argument shape,
    !> character(len=*), intent(in) :: lines(:) paired with character(len=:), allocatable,
    !> intent(out) :: names(:), called from 2+ levels deep). Relaying through this ordinary
    !> contained subroutine in the parent submodule (reached by any descendant via host
    !> association, fact 3.6) sidesteps it: the grandchild calls this one-level-up wrapper
    !> instead of reaching two levels up to parquet_core.f90's interface directly.
    subroutine parquet_parse_protected_cols_relay(lines, names)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        character(len=:), allocatable, intent(out) :: names(:) !! trimmed, unquoted protected column names.

        call parquet_parse_protected_cols(lines, names)
    end subroutine parquet_parse_protected_cols_relay

end submodule parquet_metadata
