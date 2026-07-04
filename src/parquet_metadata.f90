!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_metadata
    implicit none

    ! Valid values for a field's data_type: in a MAML file, checked by parquet_validate_maml.
    ! Add new supported types here as needed.
    character(len=7), parameter :: valid_maml_data_types(6) = [character(len=7) :: &
        "int32", "int64", "string", "boolean", "float32", "float64"]

    ! Schema of allowed top-level MAML sections and, for sections whose list
    ! items are maps (e.g. "fields:", "keyarray:"), the allowed sub-keys within
    ! each item -- checked (presence only, not content) by parquet_validate_maml.
    !
    ! To allow a new top-level section, add an entry here. To allow a new
    ! sub-key within an existing map-list section's items, add a name to that
    ! entry's subkeys(:) (unused slots must stay ""). Section/sub-key names are
    ! matched case-insensitively. Sections with an empty subkeys(:) are either
    ! plain scalars (survey:, table:, ...) or plain string lists (comments:,
    ! keywords:, ...): their list items (if any) are opaque strings, not maps,
    ! so no sub-keys are validated for them.
    !
    ! "extra:" is the sole exception: opaque = .true. means its entire
    ! internal structure (arbitrarily nested) is accepted unvalidated.
    !
    ! Sub-keys are only checked one level into a list item (e.g. fields:'s
    ! "qc:"); anything nested deeper than that (e.g. qc:'s own min/max/miss)
    ! is not descended into or validated, the same as extra:.
    integer, parameter :: maml_max_subkeys = 8

    type :: maml_section_schema
        character(len=32) :: name = ""
        logical :: opaque = .false.
        character(len=32) :: subkeys(maml_max_subkeys) = ""
    end type maml_section_schema

    ! Schema for sub-keys allowed one level deeper than allowed_maml_sections,
    ! i.e. inside a specific sub-key of a map-list item -- currently just
    ! fields:'s "qc:" sub-block (min/max/miss). Add an entry here for any
    ! other sub-key that itself has structured children needing validation;
    ! anything not listed here is left unvalidated at that depth (see
    ! parquet_validate_maml_sections).
    integer, parameter :: maml_max_nested_subkeys = 4

    type :: maml_nested_schema
        character(len=32) :: parent_section = ""
        character(len=32) :: parent_subkey = ""
        character(len=32) :: subkeys(maml_max_nested_subkeys) = ""
    end type maml_nested_schema

    type(maml_nested_schema), parameter :: allowed_maml_nested_sections(1) = [ &
        maml_nested_schema("fields", "qc", [character(len=32) :: "min", "max", "miss", ""]) &
        ]

    type(maml_section_schema), parameter :: allowed_maml_sections(17) = [ &
        maml_section_schema("survey",      .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("dataset",     .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("table",       .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("version",     .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("date",        .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("author",      .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("coauthors",   .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("dois",        .false., [character(len=32) :: "doi", "type", "", "", "", "", "", ""]), &
        maml_section_schema("depends",     .false., &
            [character(len=32) :: "survey", "dataset", "table", "version", "", "", "", ""]), &
        maml_section_schema("description", .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("comments",    .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("license",     .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("keywords",    .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("maml_version", .false., [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        maml_section_schema("keyarray",    .false., &
            [character(len=32) :: "key", "value", "comment", "", "", "", "", ""]), &
        maml_section_schema("extra",       .true.,  [character(len=32) :: "", "", "", "", "", "", "", ""]), &
        ! col_map: is deliberately NOT a top-level section: it is only valid
        ! nested inside extra: (see parquet_parse_col_map), which is already
        ! opaque/unvalidated here. A stray top-level "col_map:" is therefore
        ! correctly flagged as an unknown top-level section.
        maml_section_schema("fields",      .false., &
            [character(len=32) :: "name", "unit", "info", "ucd", "data_type", "array_size", "col_size", "qc"]) &
        ]

contains

    module procedure parquet_read_maml_file
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file(maml_filename)
        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)
        call parquet_merge_missing_columns(maml, cinfo)
        metadata%source_maml_lines = maml%lines
    end procedure parquet_read_maml_file

    module procedure parquet_read_maml_internal
        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)
        call parquet_merge_missing_columns(maml, cinfo)
        metadata%source_maml_lines = maml%lines
    end procedure parquet_read_maml_internal

    module procedure parquet_validate_user_maml
        type(parquet_column_info) :: base_cinfo, user_cinfo
        type(parquet_table_metadata) :: base_metadata, user_metadata
        character(len=:), allocatable :: bad_names, map_errors
        integer :: i, j
        logical :: found

        call parquet_validate_maml(base_maml)
        call parquet_validate_maml(user_maml)

        call parquet_parse_maml_lines(base_maml%lines, base_cinfo, base_metadata)
        call parquet_parse_maml_lines(user_maml%lines, user_cinfo, user_metadata)

        ! col_map: renames (col_internal -> col_user) are already applied to
        ! user_cinfo%col(:)%name by parquet_parse_maml_lines above -- every
        ! check below that compares names against base_cinfo therefore
        ! already operates on resolved internal names, with no changes
        ! needed. What's checked here, specific to col_map itself: every
        ! mapped internal name actually exists in the base schema, and the
        ! map has no internal-name duplicates or output-name collisions.
        user_maml%col_map = parquet_parse_col_map(user_maml%lines)
        map_errors = ""
        do i = 1, size(user_maml%col_map)
            found = .false.
            if (allocated(base_cinfo%col)) then
                do j = 1, size(base_cinfo%col)
                    if (trim(base_cinfo%col(j)%name) == trim(user_maml%col_map(i)%internal_name)) then
                        found = .true.
                        exit
                    end if
                end do
            end if
            if (.not. found) then
                map_errors = map_errors // "col_map: internal column '" // &
                    trim(user_maml%col_map(i)%internal_name) // "' not present in base MAML; "
            end if

            do j = 1, i - 1
                if (trim(user_maml%col_map(j)%internal_name) == trim(user_maml%col_map(i)%internal_name)) then
                    map_errors = map_errors // "col_map: duplicate internal column '" // &
                        trim(user_maml%col_map(i)%internal_name) // "'; "
                    exit
                end if
            end do

            do j = 1, i - 1
                if (trim(user_maml%col_map(j)%output_name) == trim(user_maml%col_map(i)%output_name)) then
                    map_errors = map_errors // "col_map: output name '" // &
                        trim(user_maml%col_map(i)%output_name) // "' used for more than one internal column; "
                    exit
                end if
            end do

            ! The renamed column must actually be declared in fields: under
            ! its output_name -- parquet_parse_maml_lines only renames a
            ! field it finds already declared as `output_name`; if none
            ! exists, the rename silently has nothing to apply to.
            found = .false.
            if (allocated(user_cinfo%col)) then
                do j = 1, size(user_cinfo%col)
                    if (trim(user_cinfo%col(j)%output_name) == trim(user_maml%col_map(i)%output_name)) then
                        found = .true.
                        exit
                    end if
                end do
            end if
            if (.not. found) then
                map_errors = map_errors // "col_map: renamed column '" // &
                    trim(user_maml%col_map(i)%output_name) // "' is not declared in fields:; "
            end if

            ! A remapped internal column must not also appear directly
            ! (un-renamed) in fields: -- that's ambiguous: was it meant to be
            ! renamed, or used as-is? (A field whose declared name matches
            ! internal_name but never got renamed keeps name == output_name
            ! == internal_name, since only a field declared under
            ! output_name is renamed.)
            if (allocated(user_cinfo%col)) then
                do j = 1, size(user_cinfo%col)
                    if (trim(user_cinfo%col(j)%name) == trim(user_maml%col_map(i)%internal_name) .and. &
                        trim(user_cinfo%col(j)%output_name) == trim(user_maml%col_map(i)%internal_name)) then
                        map_errors = map_errors // "col_map: internal column '" // &
                            trim(user_maml%col_map(i)%internal_name) // &
                            "' is remapped but also appears directly (un-renamed) in fields:; "
                        exit
                    end if
                end do
            end if

            ! The chosen output_name must not coincide with a *different*
            ! existing (base) column's own name: if it did, that other base
            ! column -- whether or not this user MAML mentions it -- would
            ! collide with the renamed one the moment it's ever activated
            ! (e.g. via set_available), since both would then share the same
            ! output_name in the written schema.
            if (allocated(base_cinfo%col)) then
                do j = 1, size(base_cinfo%col)
                    if (trim(base_cinfo%col(j)%name) == trim(user_maml%col_map(i)%output_name) .and. &
                        trim(base_cinfo%col(j)%name) /= trim(user_maml%col_map(i)%internal_name)) then
                        map_errors = map_errors // "col_map: output name '" // &
                            trim(user_maml%col_map(i)%output_name) // &
                            "' coincides with the existing base column of that name; "
                        exit
                    end if
                end do
            end if
        end do

        ! Guards against a renamed column's output_name silently colliding
        ! with another, unrelated field's own declared name (or with another
        ! renamed column's output_name): every field ending up in the
        ! schema must have a distinct output_name, since that's what
        ! actually gets registered/written to the parquet file.
        if (allocated(user_cinfo%col)) then
            do i = 1, size(user_cinfo%col)
                do j = 1, i - 1
                    if (trim(user_cinfo%col(j)%output_name) == trim(user_cinfo%col(i)%output_name)) then
                        map_errors = map_errors // "duplicate output name '" // &
                            trim(user_cinfo%col(i)%output_name) // "' used by more than one field in fields:; "
                        exit
                    end if
                end do
            end do
        end if

        if (len_trim(map_errors) > 0) then
            error stop "parquet_validate_user_maml: " // trim(map_errors)
        end if

        bad_names = ""
        if (allocated(user_cinfo%col)) then
            do i = 1, size(user_cinfo%col)
                found = .false.
                if (allocated(base_cinfo%col)) then
                    do j = 1, size(base_cinfo%col)
                        if (trim(base_cinfo%col(j)%name) == trim(user_cinfo%col(i)%name)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. found) then
                    if (len_trim(bad_names) > 0) bad_names = bad_names // ", "
                    bad_names = bad_names // trim(user_cinfo%col(i)%name)
                end if
            end do
        end if

        if (len_trim(bad_names) > 0) then
            error stop "parquet_validate_user_maml: columns not present in base MAML: " // trim(bad_names)
        end if

        if (allocated(user_maml%missing_columns)) deallocate(user_maml%missing_columns)
        user_maml%user_maml = .false.

        if (allocated(base_cinfo%col)) then
            do i = 1, size(base_cinfo%col)
                found = .false.
                if (allocated(user_cinfo%col)) then
                    do j = 1, size(user_cinfo%col)
                        if (trim(user_cinfo%col(j)%name) == trim(base_cinfo%col(i)%name)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. found) then
                    user_maml%user_maml = .true.
                    call parquet_append_missing_column(user_maml, base_cinfo%col(i))
                end if
            end do
        end if
    end procedure parquet_validate_user_maml

    subroutine parquet_append_missing_column(maml, col)
        type(parquet_maml_file), intent(inout) :: maml
        type(parquet_column_type), intent(in) :: col
        type(parquet_maml_missing_column), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(maml%missing_columns)) then
            allocate(maml%missing_columns(1))
            n = 1
        else
            allocate(tmp(size(maml%missing_columns) + 1))
            tmp(1:size(maml%missing_columns)) = maml%missing_columns
            call move_alloc(tmp, maml%missing_columns)
            n = size(maml%missing_columns)
        end if

        maml%missing_columns(n)%name = col%name
        maml%missing_columns(n)%unit = col%unit
        maml%missing_columns(n)%info = col%info
        maml%missing_columns(n)%ucd = col%ucd
        maml%missing_columns(n)%data_type = col%data_type
        maml%missing_columns(n)%array_size = col%array_size
        maml%missing_columns(n)%col_size = col%col_size
    end subroutine parquet_append_missing_column

    subroutine parquet_merge_missing_columns(maml, cinfo)
        type(parquet_maml_file), intent(in) :: maml
        type(parquet_column_info), intent(inout) :: cinfo
        type(parquet_column_type), allocatable :: merged(:)
        integer :: n_old, n_new, i

        if (.not. maml%user_maml) return
        if (.not. allocated(maml%missing_columns)) return
        if (size(maml%missing_columns) == 0) return

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
            merged(n_old + i)%array_size = maml%missing_columns(i)%array_size
            merged(n_old + i)%col_size = maml%missing_columns(i)%col_size
            merged(n_old + i)%is_set = .false.
            merged(n_old + i)%deactivated = .true.
            merged(n_old + i)%output_name = maml%missing_columns(i)%name
        end do

        call move_alloc(merged, cinfo%col)
    end subroutine parquet_merge_missing_columns

    module procedure parquet_validate_maml_internal
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: cur_name
        character(len=:), allocatable :: protected_names(:)
        character(len=32) :: idx_buf
        integer :: i, j
        logical :: type_ok, has_table, found

        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)

        errors = ""

        if (.not. allocated(cinfo%col)) then
            errors = errors // "no fields defined; "
        else if (size(cinfo%col) == 0) then
            errors = errors // "no fields defined; "
        else
            do i = 1, size(cinfo%col)
                cur_name = trim(cinfo%col(i)%name)

                if (len_trim(cur_name) == 0) then
                    write(idx_buf, '(I0)') i
                    errors = errors // "field #" // trim(idx_buf) // " has an empty name; "
                    cycle
                end if

                type_ok = .false.
                do j = 1, size(valid_maml_data_types)
                    if (trim(cinfo%col(i)%data_type) == trim(valid_maml_data_types(j))) then
                        type_ok = .true.
                        exit
                    end if
                end do
                if (.not. type_ok) then
                    errors = errors // "field '" // cur_name // "' has invalid data_type '" // &
                        trim(cinfo%col(i)%data_type) // "'; "
                end if

                do j = 1, i - 1
                    if (trim(cinfo%col(j)%name) == cur_name) then
                        errors = errors // "duplicate field name '" // cur_name // "'; "
                        exit
                    end if
                end do
            end do
        end if

        has_table = .false.
        if (allocated(metadata%items)) then
            do i = 1, size(metadata%items)
                if (trim(metadata%items(i)%key) == "table") then
                    has_table = len_trim(metadata%items(i)%value) > 0
                    exit
                end if
            end do
        end if
        if (.not. has_table) errors = errors // "missing required non-empty metadata: table; "

        ! extra: protected_cols: may only name columns declared under this
        ! same MAML's own fields: (matched by output_name -- see
        ! parquet_parse_maml_lines); anything else is a typo/dangling reference.
        protected_names = parquet_parse_protected_cols(maml%lines)
        if (allocated(cinfo%col)) then
            do i = 1, size(protected_names)
                found = .false.
                do j = 1, size(cinfo%col)
                    if (trim(protected_names(i)) == trim(cinfo%col(j)%output_name)) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    errors = errors // "protected_cols: unknown column '" // trim(protected_names(i)) // "'; "
                end if
            end do
        end if

        call parquet_validate_maml_sections(maml%lines, errors)

        if (len_trim(errors) > 0) then
            error stop "parquet_validate_maml: " // trim(errors)
        end if
    end procedure parquet_validate_maml_internal

    !> Loads maml_filename from disk and validates it (parquet_load_maml_file
    !> already validates internally, but this keeps that requirement explicit
    !> and self-contained here rather than depending on that side effect).
    module procedure parquet_validate_maml_file
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file(maml_filename)
        call parquet_validate_maml_internal(maml)
    end procedure parquet_validate_maml_file

    module procedure parquet_load_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(maml_filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_maml_file: cannot open file: " // trim(maml_filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        maml%name = trim(maml_filename)

        max_len = 1
        do i = 1, nlines
            max_len = max(max_len, len_trim(lines(i)))
        end do

        allocate(character(len=max_len) :: maml%lines(nlines))
        do i = 1, nlines
            maml%lines(i) = lines(i)(1:max_len)
        end do

        call parquet_validate_maml(maml)
    end procedure parquet_load_maml_file

    module procedure parquet_parse_maml_lines
        type(parquet_column_type), allocatable :: tmp(:)
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list, in_field_list, in_keyarray, in_doiarray, in_dependsarray, in_extra
        character(len=:), allocatable :: list_key, field_list_key, list_item
        character(len=:), allocatable :: ka_key, ka_value, ka_comment
        character(len=:), allocatable :: doi_value, doi_type
        character(len=:), allocatable :: depends_survey, depends_dataset, depends_table, depends_version
        character(len=:), allocatable :: keywords_value
        type(parquet_maml_col_map_entry), allocatable :: col_map(:)
        integer :: ios, n, i, j, list_item_idx, doi_idx, depends_idx
        character(len=32) :: idx_buf

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        in_field_list = .false.
        in_keyarray = .false.
        in_doiarray = .false.
        in_dependsarray = .false.
        in_extra = .false.
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
                            if (parquet_to_lower(key) == "key") ka_key = parquet_unquote(cvalue)
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("key")
                            ka_key = parquet_unquote(cvalue)
                        case ("value")
                            ka_value = parquet_unquote(cvalue)
                        case ("comment")
                            ka_comment = parquet_unquote(cvalue)
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
                            if (parquet_to_lower(key) == "doi") doi_value = parquet_unquote(cvalue)
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("doi")
                            doi_value = parquet_unquote(cvalue)
                        case ("type")
                            doi_type = parquet_unquote(cvalue)
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
                            select case (parquet_to_lower(key))
                            case ("survey")
                                depends_survey = parquet_unquote(cvalue)
                            case ("dataset")
                                depends_dataset = parquet_unquote(cvalue)
                            case ("table")
                                depends_table = parquet_unquote(cvalue)
                            case ("version")
                                depends_version = parquet_unquote(cvalue)
                            end select
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("survey")
                            depends_survey = parquet_unquote(cvalue)
                        case ("dataset")
                            depends_dataset = parquet_unquote(cvalue)
                        case ("table")
                            depends_table = parquet_unquote(cvalue)
                        case ("version")
                            depends_version = parquet_unquote(cvalue)
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
                            call metadata%add_metadata("comment_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else if (list_key == "coauthors" .or. list_key == "coauthor") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("coauthor_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else if (list_key == "keywords" .or. list_key == "keyword") then
                            if (len_trim(keywords_value) > 0) then
                                keywords_value = trim(keywords_value) // ";" // trim(parquet_unquote(tline(3:)))
                            else
                                keywords_value = trim(parquet_unquote(tline(3:)))
                            end if
                        else
                            call metadata%add_metadata(list_key, parquet_unquote(tline(3:)))
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

                if (tline == "fields:") in_fields = .true.
                if (in_fields) cycle

                if (tline == "keyarray:") then
                    in_keyarray = .true.
                    ka_key = ""
                    ka_value = ""
                    ka_comment = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "dois:") then
                    in_doiarray = .true.
                    doi_idx = 0
                    doi_value = ""
                    doi_type = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "depends:") then
                    in_dependsarray = .true.
                    depends_idx = 0
                    depends_survey = ""
                    depends_dataset = ""
                    depends_table = ""
                    depends_version = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "extra:") then
                    in_extra = .true.
                    cycle
                end if

                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle

                key = parquet_to_lower(key)

                if (key == "fields") then
                    in_fields = .true.
                else if (len_trim(cvalue) > 0) then
                    call metadata%add_metadata(key, parquet_unquote(cvalue))
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
                    list_item = parquet_unquote(tline(3:))
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

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            select case (parquet_to_lower(key))
            case ("name")
                tmp(n)%name = parquet_unquote(cvalue)
            case ("unit")
                tmp(n)%unit = parquet_unquote(cvalue)
                if (parquet_to_lower(tmp(n)%unit) == "unitless") tmp(n)%unit = ""
            case ("info")
                tmp(n)%info = parquet_unquote(cvalue)
            case ("ucd")
                if (len_trim(cvalue) > 0) then
                    tmp(n)%ucd = parquet_unquote(cvalue)
                else
                    tmp(n)%ucd = ""
                    in_field_list = .true.
                    field_list_key = "ucd"
                end if
            case ("data_type")
                cvalue = parquet_to_lower(parquet_unquote(cvalue))
                if (index(cvalue, "string") == 1) then
                    cvalue = "string"
                end if
                tmp(n)%data_type = cvalue
            case ("array_size")
                read(cvalue, *, iostat=ios) tmp(n)%array_size
                if (ios /= 0) tmp(n)%array_size = 1
            case ("col_size")
                read(cvalue, *, iostat=ios) tmp(n)%col_size
                if (ios /= 0) tmp(n)%col_size = 1
            end select
        end do

        if (in_keyarray) call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        if (in_doiarray) call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        if (in_dependsarray) call parquet_flush_depends_item(metadata, depends_idx, &
            depends_survey, depends_dataset, depends_table, depends_version)
        if (in_list .and. (list_key == "keywords" .or. list_key == "keyword")) then
            call parquet_flush_keywords(metadata, keywords_value)
        end if

        if (n <= 0) then
            allocate(cinfo%col(0))
            return
        end if

        do i = 1, n
            if (.not. allocated(tmp(i)%name)) error stop "parquet_read_maml: missing field name in fields block"
            if (.not. allocated(tmp(i)%data_type)) error stop "parquet_read_maml: missing data_type in fields block"
            if (.not. allocated(tmp(i)%unit)) tmp(i)%unit = ""
            if (.not. allocated(tmp(i)%info)) tmp(i)%info = ""
            if (.not. allocated(tmp(i)%ucd)) tmp(i)%ucd = ""
            if (tmp(i)%array_size <= 0) tmp(i)%array_size = 1
            if (tmp(i)%col_size <= 0) tmp(i)%col_size = 1
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
            protected_names = parquet_parse_protected_cols(lines)
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
    end procedure parquet_parse_maml_lines

    module procedure parquet_append_line
        character(len=1024), allocatable :: tmp(:)

        if (.not. allocated(lines)) then
            allocate(lines(1))
            lines(1) = line
            return
        end if

        allocate(tmp(n))
        tmp(1:n-1) = lines
        tmp(n) = line
        call move_alloc(tmp, lines)
    end procedure parquet_append_line

    module procedure parquet_metadata_append_entry
        type(parquet_metadata_entry), allocatable :: tmp(:)
        integer :: n
        character(len=:), allocatable :: desc_val

        if (len_trim(key) == 0) return

        desc_val = ""
        if (present(description)) desc_val = trim(description)

        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            metadata%items(1)%description = desc_val
        else
            n = size(metadata%items)
            allocate(tmp(n+1))
            tmp(1:n) = metadata%items
            tmp(n+1)%key = trim(key)
            tmp(n+1)%value = trim(value)
            tmp(n+1)%description = desc_val
            call move_alloc(tmp, metadata%items)
        end if

        ! metadata%source_maml_lines is only allocated once parquet_read_maml has
        ! finished parsing (see parquet_read_maml_file/_internal below), so this
        ! never fires for the add_metadata calls the parser itself makes while
        ! building up metadata%items above -- only for calls made by external
        ! code after parquet_read_maml has returned.
        if (allocated(metadata%source_maml_lines)) then
            call parquet_append_keyarray_line(metadata%source_maml_lines, trim(key), trim(value), desc_val)
        end if
    end procedure parquet_metadata_append_entry

    module procedure add_metadata_int32
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description)
    end procedure add_metadata_int32

    module procedure add_metadata_int64
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description)
    end procedure add_metadata_int64

    module procedure add_metadata_float32
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES15.7E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description)
    end procedure add_metadata_float32

    module procedure add_metadata_float64
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES24.16E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description)
    end procedure add_metadata_float64

    module procedure add_metadata_logical
        if (value) then
            call parquet_metadata_append_entry(this, key, "true", description)
        else
            call parquet_metadata_append_entry(this, key, "false", description)
        end if
    end procedure add_metadata_logical

    module procedure add_metadata_string
        call parquet_metadata_append_entry(this, key, trim(value), description)
    end procedure add_metadata_string

    module procedure add_metadata_int32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_int32_array

    module procedure add_metadata_int64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_int64_array

    module procedure add_metadata_float32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES15.7E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_float32_array

    module procedure add_metadata_float64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES24.16E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_float64_array

    module procedure add_metadata_logical_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            if (value(i)) then
                joined = joined // "true"
            else
                joined = joined // "false"
            end if
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_logical_array

    module procedure add_metadata_string_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(value(i))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_string_array

    module procedure get_column_index
        integer :: i

        get_column_index = 0
        if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (allocated(this%col(i)%name)) then
                    if (trim(this%col(i)%name) == trim(name)) then
                        get_column_index = i
                        exit
                    end if
                end if
            end do
        end if

        if (get_column_index == 0) then
            error stop "parquet_column_info%get_column_index: column not found: " // trim(name)
        end if
    end procedure get_column_index

    module procedure set_unavailable
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%deactivated) then
                error stop "parquet_column_info%set_unavailable: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .false.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%deactivated) this%col(i)%is_set = .false.
            end do
        end if
    end procedure set_unavailable

    module procedure set_available
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%deactivated) then
                error stop "parquet_column_info%set_available: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .true.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%deactivated) this%col(i)%is_set = .true.
            end do
        end if
    end procedure set_available

    module procedure parquet_append_empty_cinfo
        type(parquet_column_type), allocatable :: tmp(:)

        if (.not. allocated(columns)) then
            allocate(columns(1))
            n = 1
        else
            allocate(tmp(size(columns) + 1))
            if (size(columns) > 0) tmp(1:size(columns)) = columns
            call move_alloc(tmp, columns)
            n = size(columns)
        end if

        columns(n)%is_set = .true.
        columns(n)%array_size = 1
        columns(n)%col_size = 1
    end procedure parquet_append_empty_cinfo

    module procedure parquet_split_key_value
        integer :: p

        p = index(line, ":")
        if (p <= 0) then
            key = ""
            value = ""
            return
        end if

        key = trim(adjustl(line(1:p-1)))
        if (p < len_trim(line)) then
            value = trim(adjustl(line(p+1:len_trim(line))))
        else
            value = ""
        end if
    end procedure parquet_split_key_value

    module procedure parquet_unquote
        integer :: n

        out = trim(adjustl(s))
        n = len_trim(out)
        if (n >= 2) then
            if ((out(1:1) == '"' .and. out(n:n) == '"') .or. (out(1:1) == "'" .and. out(n:n) == "'")) then
                out = out(2:n-1)
            end if
        end if
    end procedure parquet_unquote

    module procedure parquet_to_lower
        integer :: i, c

        out = s
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end procedure parquet_to_lower

    !> Appends a `- key: / value: / comment:` entry to the `keyarray:` block
    !> inside `lines` (the verbatim source MAML content kept in
    !> metadata%source_maml_lines), so that metadata added at runtime via
    !> add_metadata after parquet_read_maml is reflected in a later
    !> write_maml sidecar. Always appends; does not update an existing entry
    !> that has the same key. Inserted before `extra:` if present, else
    !> before `fields:`; synthesizes the `keyarray:` header itself if the
    !> source MAML did not already have one.
    subroutine parquet_append_keyarray_line(lines, key, value, desc)
        character(len=:), allocatable, intent(inout) :: lines(:)
        character(len=*), intent(in) :: key, value, desc
        character(len=:), allocatable :: entries(:), new_lines(:)
        integer :: insert_pos, n_old, n_new, new_len
        logical :: need_header

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
    end subroutine parquet_append_keyarray_line

    !> Locates where a new keyarray entry should be inserted in `lines`.
    !> If a top-level `keyarray:` header already exists, `insert_pos` points
    !> just past its last item (need_header = .false.). Otherwise, `insert_pos`
    !> points at the top-level `extra:` line if present, else at the top-level
    !> `fields:` line (always present), and need_header = .true.
    subroutine parquet_locate_keyarray_insert(lines, insert_pos, need_header)
        character(len=*), intent(in) :: lines(:)
        integer, intent(out) :: insert_pos
        logical, intent(out) :: need_header
        integer :: i, n
        character(len=:), allocatable :: tline

        n = size(lines)
        need_header = .true.
        insert_pos = n + 1

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. tline == "keyarray:") then
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
            if (lines(i)(1:1) /= " " .and. tline == "extra:") then
                insert_pos = i
                return
            end if
        end do

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. tline == "fields:") then
                insert_pos = i
                return
            end if
        end do
    end subroutine parquet_locate_keyarray_insert

    subroutine parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        type(parquet_table_metadata), intent(inout) :: metadata
        character(len=*), intent(in) :: ka_key
        character(len=*), intent(in) :: ka_value
        character(len=*), intent(in) :: ka_comment

        if (len_trim(ka_key) == 0) return
        call metadata%add_metadata(trim(ka_key), trim(ka_value), trim(ka_comment))
    end subroutine parquet_flush_keyarray_item

    subroutine parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        type(parquet_table_metadata), intent(inout) :: metadata
        integer, intent(inout) :: doi_idx
        character(len=*), intent(in) :: doi_value
        character(len=*), intent(in) :: doi_type
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
        type(parquet_table_metadata), intent(inout) :: metadata
        integer, intent(inout) :: depends_idx
        character(len=*), intent(in) :: survey, dataset, table, version
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
        type(parquet_table_metadata), intent(inout) :: metadata
        character(len=*), intent(in) :: keywords_value

        if (len_trim(keywords_value) == 0) return
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
    function parquet_parse_col_map(lines) result(col_map)
        character(len=*), intent(in) :: lines(:)
        type(parquet_maml_col_map_entry), allocatable :: col_map(:)
        type(parquet_maml_col_map_entry), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue
        integer :: i, n, idx_extra, extra_end, idx_col_map, n_entries

        allocate(col_map(0))

        ! col_map: is only valid nested inside extra: (extra:'s own internal
        ! structure is otherwise entirely unvalidated/opaque -- see
        ! allowed_maml_sections -- so this is a dedicated, narrow lookup
        ! rather than a generically-validated section of its own).
        n = size(lines)
        idx_extra = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "extra:") then
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
            if (trim(adjustl(lines(i))) == "col_map:") then
                idx_col_map = i
                exit
            end if
        end do
        if (idx_col_map == 0) return

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
            tmp(n_entries + 1)%internal_name = parquet_unquote(key)
            tmp(n_entries + 1)%output_name = parquet_unquote(cvalue)
            call move_alloc(tmp, col_map)
        end do
    end function parquet_parse_col_map

    !> Parses a `protected_cols:` entry nested inside `extra:`, e.g.:
    !>   extra:
    !>     protected_cols: col1;col2; col3
    !> or, equivalently:
    !>   extra:
    !>     protected_cols:
    !>     - col1
    !>     - col2
    !>     - col3
    !> Returns a zero-size array if there is no extra:/protected_cols:
    !> section. Names are trimmed and unquoted; empty tokens (e.g. a stray
    !> ";;" or trailing ";") are skipped. Matching against declared field
    !> names (by output_name) is done by the caller -- this function only
    !> extracts the raw name list.
    function parquet_parse_protected_cols(lines) result(names)
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue, token
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
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "extra:") then
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
            if (parquet_to_lower(trim(key)) == "protected_cols") then
                idx_key = i
                exit
            end if
        end do
        if (idx_key == 0) return

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
                token = trim(adjustl(parquet_unquote(token)))
                if (len_trim(token) > 0) then
                    n_names = size(names)
                    allocate(character(len=len(names)) :: tmp(n_names + 1))
                    if (n_names > 0) tmp(1:n_names) = names
                    tmp(n_names + 1) = token
                    call move_alloc(tmp, names)
                end if
            end do
            return
        end if

        ! Dash-list form: protected_cols: (empty) followed by "- col1" lines.
        do i = idx_key + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            tline = trim(adjustl(lines(i)))
            if (tline(1:1) /= "-") exit
            tline = trim(adjustl(tline(2:)))
            token = trim(adjustl(parquet_unquote(tline)))
            if (len_trim(token) == 0) cycle

            n_names = size(names)
            allocate(character(len=len(names)) :: tmp(n_names + 1))
            if (n_names > 0) tmp(1:n_names) = names
            tmp(n_names + 1) = token
            call move_alloc(tmp, names)
        end do
    end function parquet_parse_protected_cols

    !> Checks that every top-level section in `lines`, every sub-key found
    !> one level inside a map-list section's items (e.g. "name:"/"data_type:"/
    !> ... inside a fields: entry), and every sub-key one level deeper still
    !> where allowed_maml_nested_sections declares one (currently just
    !> fields:'s "qc:" sub-block), is declared in the schema. Presence only:
    !> values are not inspected. Anything nested deeper than that, or
    !> anywhere inside "extra:", is left unvalidated.
    !> Appends one "; "-terminated message per unrecognized name to `errors`.
    subroutine parquet_validate_maml_sections(lines, errors)
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable, intent(inout) :: errors
        character(len=:), allocatable :: tline, item_tline, key, cvalue
        character(len=32) :: current_subkey
        integer :: i, n, indent, item_indent, section_idx, nested_idx
        logical :: have_item_indent, section_opaque

        n = size(lines)
        section_idx = 0
        section_opaque = .false.
        have_item_indent = .false.
        item_indent = 0
        current_subkey = ""

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            indent = parquet_line_indent(lines(i))

            if (indent == 0 .and. tline(1:1) /= "-") then
                ! A new top-level section.
                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle
                section_idx = parquet_find_maml_section(key)
                if (section_idx == 0) then
                    errors = errors // "unknown top-level section '" // trim(key) // "'; "
                    section_opaque = .true.
                else
                    section_opaque = allowed_maml_sections(section_idx)%opaque
                end if
                have_item_indent = .false.
                current_subkey = ""
                cycle
            end if

            if (section_opaque .or. section_idx == 0) cycle

            if (indent == 0 .and. tline(1:1) == "-") then
                ! Start of a new list item; the dash line may itself carry
                ! the item's first sub-key, e.g. "- name: id0".
                have_item_indent = .false.
                current_subkey = ""
                if (parquet_section_has_subkeys(section_idx)) then
                    item_tline = trim(adjustl(tline(2:)))
                    if (len_trim(item_tline) > 0) then
                        call parquet_split_key_value(item_tline, key, cvalue)
                        if (len_trim(key) > 0) then
                            call parquet_check_maml_subkey(section_idx, key, errors)
                            current_subkey = key
                        end if
                    end if
                end if
                cycle
            end if

            ! Indented continuation line.
            if (.not. have_item_indent) then
                item_indent = indent
                have_item_indent = .true.
            end if

            if (indent > item_indent) then
                ! Nested one level deeper than the item's direct sub-keys
                ! (e.g. fields:'s "qc:" children): only validated where
                ! allowed_maml_nested_sections declares a schema for the
                ! enclosing sub-key; anything else is left unvalidated here.
                nested_idx = parquet_find_maml_nested_section(allowed_maml_sections(section_idx)%name, current_subkey)
                if (nested_idx == 0) cycle
                if (index(tline, ":") == 0) cycle
                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) > 0) call parquet_check_maml_nested_subkey(nested_idx, key, errors)
                cycle
            end if

            if (.not. parquet_section_has_subkeys(section_idx)) cycle
            if (index(tline, ":") == 0) cycle

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) > 0) then
                call parquet_check_maml_subkey(section_idx, key, errors)
                current_subkey = key
            end if
        end do
    end subroutine parquet_validate_maml_sections

    function parquet_line_indent(line) result(indent)
        character(len=*), intent(in) :: line
        integer :: indent, k

        do k = 1, len(line)
            if (line(k:k) /= " ") then
                indent = k - 1
                return
            end if
        end do
        indent = len(line)
    end function parquet_line_indent

    function parquet_find_maml_section(key) result(idx)
        character(len=*), intent(in) :: key
        integer :: idx, k

        idx = 0
        do k = 1, size(allowed_maml_sections)
            if (trim(parquet_to_lower(allowed_maml_sections(k)%name)) == trim(parquet_to_lower(key))) then
                idx = k
                return
            end if
        end do
    end function parquet_find_maml_section

    function parquet_section_has_subkeys(section_idx) result(has_subkeys)
        integer, intent(in) :: section_idx
        logical :: has_subkeys

        has_subkeys = any(len_trim(allowed_maml_sections(section_idx)%subkeys) > 0)
    end function parquet_section_has_subkeys

    function parquet_find_maml_nested_section(parent_section, parent_subkey) result(idx)
        character(len=*), intent(in) :: parent_section, parent_subkey
        integer :: idx, k

        idx = 0
        if (len_trim(parent_subkey) == 0) return
        do k = 1, size(allowed_maml_nested_sections)
            if (trim(parquet_to_lower(allowed_maml_nested_sections(k)%parent_section)) == &
                trim(parquet_to_lower(parent_section)) .and. &
                trim(parquet_to_lower(allowed_maml_nested_sections(k)%parent_subkey)) == &
                trim(parquet_to_lower(parent_subkey))) then
                idx = k
                return
            end if
        end do
    end function parquet_find_maml_nested_section

    subroutine parquet_check_maml_nested_subkey(nested_idx, key, errors)
        integer, intent(in) :: nested_idx
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: errors
        logical :: ok
        integer :: k

        ok = .false.
        do k = 1, maml_max_nested_subkeys
            if (len_trim(allowed_maml_nested_sections(nested_idx)%subkeys(k)) == 0) cycle
            if (trim(parquet_to_lower(allowed_maml_nested_sections(nested_idx)%subkeys(k))) == &
                trim(parquet_to_lower(key))) then
                ok = .true.
                exit
            end if
        end do
        if (.not. ok) then
            errors = errors // "unknown sub-key '" // trim(key) // "' in '" // &
                trim(allowed_maml_nested_sections(nested_idx)%parent_subkey) // ":' block (inside section '" // &
                trim(allowed_maml_nested_sections(nested_idx)%parent_section) // "'); "
        end if
    end subroutine parquet_check_maml_nested_subkey

    subroutine parquet_check_maml_subkey(section_idx, key, errors)
        integer, intent(in) :: section_idx
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: errors
        logical :: ok
        integer :: k

        ok = .false.
        do k = 1, maml_max_subkeys
            if (len_trim(allowed_maml_sections(section_idx)%subkeys(k)) == 0) cycle
            if (trim(parquet_to_lower(allowed_maml_sections(section_idx)%subkeys(k))) == &
                trim(parquet_to_lower(key))) then
                ok = .true.
                exit
            end if
        end do
        if (.not. ok) then
            errors = errors // "unknown sub-key '" // trim(key) // "' in section '" // &
                trim(allowed_maml_sections(section_idx)%name) // "'; "
        end if
    end subroutine parquet_check_maml_subkey

end submodule parquet_metadata
