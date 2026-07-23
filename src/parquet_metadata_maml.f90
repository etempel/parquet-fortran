!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> MAML-format-specific validation: the declarative schema of every top-level
!> MAML section and its allowed sub-keys plus the validator and its private
!> helpers that check a raw MAML's section/sub-key names against it (presence
!> only, not values); full-schema structural validation
!> (parquet_validate_maml); cross-checking a user MAML against a base schema
!> (parquet_validate_user_maml); loading MAML/qc-maml files from disk; and
!> qc-maml field parsing/validation for read-time quality control. Merges
!> what used to be parquet_metadata_sections.f90 and parquet_metadata_validate.f90.
submodule (parquet:parquet_metadata) parquet_metadata_maml
    use ieee_arithmetic, only: ieee_is_nan
    implicit none

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
    integer, parameter :: maml_max_subkeys = 8 !! Declared subkeys(:) capacity per maml_section_schema entry.

    !> One allowed top-level MAML section: its name, whether its contents are
    !> opaque/unvalidated (extra:), and (for map-list sections) its allowed
    !> item sub-keys.
    type :: maml_section_schema
        character(len=32) :: name = "" !! Section name (matched case-insensitively).
        logical :: opaque = .false. !! .true. if this section's contents are accepted unvalidated (extra: only).
        character(len=32) :: subkeys(maml_max_subkeys) = "" !! Allowed item sub-keys; "" for unused slots.
    end type maml_section_schema

    ! Schema for sub-keys allowed one level deeper than allowed_maml_sections,
    ! i.e. inside a specific sub-key of a map-list item -- currently just
    ! fields:'s "qc:" sub-block (min/max/miss). Add an entry here for any
    ! other sub-key that itself has structured children needing validation;
    ! anything not listed here is left unvalidated at that depth (see
    ! parquet_validate_maml_sections).
    integer, parameter :: maml_max_nested_subkeys = 4 !! Declared subkeys(:) capacity per maml_nested_schema entry.

    !> One allowed nested sub-key block, one level deeper than a
    !> maml_section_schema item's own direct sub-keys (e.g. fields:'s "qc:").
    type :: maml_nested_schema
        character(len=32) :: parent_section = "" !! Enclosing top-level section name.
        character(len=32) :: parent_subkey = "" !! The item sub-key whose own children this schema describes.
        character(len=32) :: subkeys(maml_max_nested_subkeys) = "" !! Allowed nested sub-keys; "" for unused slots.
    end type maml_nested_schema

    !> Currently just fields:'s "qc:" sub-block (min:/max:/miss:).
    type(maml_nested_schema), parameter :: allowed_maml_nested_sections(1) = [ &
        maml_nested_schema("fields", "qc", [character(len=32) :: "min", "max", "miss", ""]) &
        ]

    !> The full set of allowed top-level MAML sections; see each entry's
    !> inline comments above for the rationale behind opaque/subkeys choices.
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

    !> Checks that every top-level section in `lines`, every sub-key found
    !> one level inside a map-list section's items (e.g. "name:"/"data_type:"/
    !> ... inside a fields: entry), and every sub-key one level deeper still
    !> where allowed_maml_nested_sections declares one (currently just
    !> fields:'s "qc:" sub-block), is declared in the schema. Presence only:
    !> values are not inspected. Anything nested deeper than that, or
    !> anywhere inside "extra:", is left unvalidated.
    !> Appends one "; "-terminated message per unrecognized name to `errors`.
    module subroutine parquet_validate_maml_sections(lines, errors)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to check.
        character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
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

    !> Number of leading blank characters in `line` (0 for a top-level line);
    !> `len(line)` for an all-blank line.
    function parquet_line_indent(line) result(indent)
        character(len=*), intent(in) :: line !! raw MAML source line.
        integer :: indent !! number of leading blanks.
        integer :: k

        do k = 1, len(line)
            if (line(k:k) /= " ") then
                indent = k - 1
                return
            end if
        end do
        indent = len(line) ! GCOVR_EXCL_LINE
    end function parquet_line_indent

    !> 1-based index of `key` in allowed_maml_sections (case-insensitive), or 0 if unknown.
    function parquet_find_maml_section(key) result(idx)
        character(len=*), intent(in) :: key !! top-level section name to look up.
        integer :: idx !! index into allowed_maml_sections, or 0.
        integer :: k
        character(len=:), allocatable :: tlo1, tlo2 !! scratch (to_lower).

        idx = 0
        do k = 1, size(allowed_maml_sections)
            call parquet_to_lower(allowed_maml_sections(k)%name, tlo1)
            call parquet_to_lower(key, tlo2)
            if (trim(tlo1) == trim(tlo2)) then
                idx = k
                return
            end if
        end do
    end function parquet_find_maml_section

    !> True if the section at `section_idx` is a map-list section (has at
    !> least one declared sub-key), i.e. its items are validated as maps
    !> rather than treated as opaque scalars/strings.
    function parquet_section_has_subkeys(section_idx) result(has_subkeys)
        integer, intent(in) :: section_idx !! index into allowed_maml_sections.
        logical :: has_subkeys !! .true. if this section has at least one declared sub-key.

        has_subkeys = any(len_trim(allowed_maml_sections(section_idx)%subkeys) > 0)
    end function parquet_section_has_subkeys

    !> 1-based index of the (parent_section, parent_subkey) pair in
    !> allowed_maml_nested_sections (case-insensitive), or 0 if this
    !> sub-key has no declared nested schema.
    function parquet_find_maml_nested_section(parent_section, parent_subkey) result(idx)
        character(len=*), intent(in) :: parent_section !! enclosing top-level section name.
        character(len=*), intent(in) :: parent_subkey !! item sub-key whose nested schema is being looked up.
        integer :: idx !! index into allowed_maml_nested_sections, or 0.
        integer :: k
        character(len=:), allocatable :: tlo1, tlo2, tlo3, tlo4 !! scratch (to_lower).

        idx = 0
        if (len_trim(parent_subkey) == 0) return ! GCOVR_EXCL_LINE
        do k = 1, size(allowed_maml_nested_sections)
            call parquet_to_lower(allowed_maml_nested_sections(k)%parent_section, tlo1)
            call parquet_to_lower(parent_section, tlo2)
            call parquet_to_lower(allowed_maml_nested_sections(k)%parent_subkey, tlo3)
            call parquet_to_lower(parent_subkey, tlo4)
            if (trim(tlo1) == &
                trim(tlo2) .and. &
                trim(tlo3) == &
                trim(tlo4)) then
                idx = k
                return
            end if
        end do
    end function parquet_find_maml_nested_section

    !> Appends an "unknown sub-key" message to `errors` if `key` is not among
    !> allowed_maml_nested_sections(nested_idx)'s declared subkeys(:).
    subroutine parquet_check_maml_nested_subkey(nested_idx, key, errors)
        character(len=:), allocatable :: tlo1, tlo2 !! scratch (to_lower).
        integer, intent(in) :: nested_idx !! index into allowed_maml_nested_sections.
        character(len=*), intent(in) :: key !! nested sub-key name found in the MAML.
        character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
        logical :: ok
        integer :: k

        ok = .false.
        do k = 1, maml_max_nested_subkeys
            if (len_trim(allowed_maml_nested_sections(nested_idx)%subkeys(k)) == 0) cycle
            call parquet_to_lower(allowed_maml_nested_sections(nested_idx)%subkeys(k), tlo1)
            call parquet_to_lower(key, tlo2)
            if (trim(tlo1) == &
                trim(tlo2)) then
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

    !> Appends an "unknown sub-key" message to `errors` if `key` is not among
    !> allowed_maml_sections(section_idx)'s declared subkeys(:).
    subroutine parquet_check_maml_subkey(section_idx, key, errors)
        character(len=:), allocatable :: tlo1, tlo2 !! scratch (to_lower).
        integer, intent(in) :: section_idx !! index into allowed_maml_sections.
        character(len=*), intent(in) :: key !! sub-key name found in the MAML.
        character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
        logical :: ok
        integer :: k

        ok = .false.
        do k = 1, maml_max_subkeys
            if (len_trim(allowed_maml_sections(section_idx)%subkeys(k)) == 0) cycle
            call parquet_to_lower(allowed_maml_sections(section_idx)%subkeys(k), tlo1)
            call parquet_to_lower(key, tlo2)
            if (trim(tlo1) == &
                trim(tlo2)) then
                ok = .true.
                exit
            end if
        end do
        if (.not. ok) then
            errors = errors // "unknown sub-key '" // trim(key) // "' in section '" // &
                trim(allowed_maml_sections(section_idx)%name) // "'; "
        end if
    end subroutine parquet_check_maml_subkey

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
                    ! gcov attribution artifact: this condition is evaluated for every col_map(i)/col(j) pair
                    ! regardless of whether it's ever true, so gcov always marks it "hit".
                    if (trim(user_cinfo%col(j)%name) == trim(user_maml%col_map(i)%internal_name) .and. &
                        trim(user_cinfo%col(j)%output_name) == trim(user_maml%col_map(i)%internal_name)) then ! GCOVR_EXCL_START
                        map_errors = map_errors // "col_map: internal column '" // &
                            trim(user_maml%col_map(i)%internal_name) // &
                            "' is remapped but also appears directly (un-renamed) in fields:; "
                        exit
                    end if ! GCOVR_EXCL_STOP
                end do
            end if

            ! The chosen output_name must not coincide with a *different*
            ! existing (base) column's own name: if it did, that other base
            ! column -- whether or not this user MAML mentions it -- would
            ! collide with the renamed one the moment it's ever activated
            ! (e.g. via set_column_available), since both would then share the same
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
                    ! gcov attribution artifact: this condition is evaluated for every (i, j) pair regardless
                    ! of whether it's ever true, so gcov always marks it "hit".
                    if (trim(user_cinfo%col(j)%output_name) == trim(user_cinfo%col(i)%output_name)) then ! GCOVR_EXCL_START
                        map_errors = map_errors // "duplicate output name '" // &
                            trim(user_cinfo%col(i)%output_name) // "' used by more than one field in fields:; "
                        exit
                    end if ! GCOVR_EXCL_STOP
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

    !> Appends `col` (a base-schema column absent from `maml`) to
    !> maml%missing_columns, so parquet_merge_missing_columns can later
    !> restore it as a disabled/deactivated column.
    subroutine parquet_append_missing_column(maml, col)
        type(parquet_maml_file), intent(inout) :: maml !! user MAML gaining one missing-column entry.
        type(parquet_column_type), intent(in) :: col !! base-schema column that maml doesn't declare.
        type(parquet_maml_missing_column), allocatable :: tmp(:)
        integer :: n

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
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
        maml%missing_columns(n)%time_unit = col%time_unit
        maml%missing_columns(n)%is_utc = col%is_utc
        maml%missing_columns(n)%array_size = col%array_size
        maml%missing_columns(n)%col_size = col%col_size
        call parquet_maml_unlock()
    end subroutine parquet_append_missing_column

    module procedure parquet_validate_maml_internal
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: cur_name
        character(len=:), allocatable :: protected_names(:)
        character(len=32) :: idx_buf
        integer :: i, j
        logical :: has_table, found
        real(real64) :: qc_bound_value

        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)

        errors = ""

        ! cinfo%col is always allocated here (parquet_parse_maml_lines never leaves it
        ! unallocated -- it allocates an explicit zero-size array when there are no fields).
        if (size(cinfo%col) == 0) then
            errors = errors // "no fields defined; "
        else
            do i = 1, size(cinfo%col)
                cur_name = trim(cinfo%col(i)%name)

                if (len_trim(cur_name) == 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
                    write(idx_buf, '(I0)') i
                    errors = errors // "field #" // trim(idx_buf) // " has an empty name; "
                    cycle
                end if ! GCOVR_EXCL_STOP

                if (.not. parquet_data_type_token_valid(cinfo%col(i)%data_type)) then
                    errors = errors // "field '" // cur_name // "' has invalid data_type '" // &
                        trim(cinfo%col(i)%data_type) // "'; "
                end if

                if (cinfo%col(i)%col_size == size_invalid_sentinel) then
                    errors = errors // "field '" // cur_name // &
                        "' has an invalid col_size (must be a positive integer or 'auto'); "
                end if
                if (cinfo%col(i)%array_size == size_invalid_sentinel) then
                    errors = errors // "field '" // cur_name // &
                        "' has an invalid array_size (must be a positive integer or 'auto'); "
                end if
                if (cinfo%col(i)%array_size == parquet_size_auto .and. trim(cinfo%col(i)%data_type) /= "string") then
                    errors = errors // "field '" // cur_name // &
                        "' declares array_size: auto, which only applies to string columns; "
                end if

                ! qc: is not supported for temporal (date/time/timestamp) columns yet -- reject
                ! it with a clear message rather than silently ignoring a declared bound.
                select case (trim(cinfo%col(i)%data_type))
                case ("date", "time", "timestamp")
                    if (cinfo%col(i)%has_qc_min .or. cinfo%col(i)%has_qc_max) then
                        errors = errors // "field '" // cur_name // "' declares qc:, which is not " // &
                            "supported for a " // trim(cinfo%col(i)%data_type) // " column; "
                    end if
                end select

                do j = 1, i - 1
                    if (trim(cinfo%col(j)%name) == cur_name) then
                        errors = errors // "duplicate field name '" // cur_name // "'; "
                        exit
                    end if
                end do

                ! qc: min: must use a lower-bound operator (>= or >) and qc:
                ! max: an upper-bound operator (<= or <); the opposite
                ! direction (e.g. min: '< 5') is a nonsensical bound. This is
                ! a purely syntactic check, applied to every enforced type
                ! (numeric and string alike); boolean's qc: is silently
                ! ignored entirely (see the numeric block below), so it's
                ! exempt here too.
                if (trim(cinfo%col(i)%data_type) /= "boolean") then
                    if (cinfo%col(i)%has_qc_min .and. cinfo%col(i)%qc_min_op(1:1) == "<") then
                        errors = errors // "field '" // cur_name // "' has a qc: min value with a '" // &
                            trim(cinfo%col(i)%qc_min_op) // "' operator; min: accepts only >= or > " // &
                            "(use max: for an upper bound); "
                    end if
                    if (cinfo%col(i)%has_qc_max .and. cinfo%col(i)%qc_max_op(1:1) == ">") then
                        errors = errors // "field '" // cur_name // "' has a qc: max value with a '" // &
                            trim(cinfo%col(i)%qc_max_op) // "' operator; max: accepts only <= or < " // &
                            "(use min: for a lower bound); "
                    end if
                end if

                ! qc: min:/max: numeric convertibility only applies to the
                ! numeric types; string uses its bound as a literal (nothing
                ! to convert, so it can't fail), and boolean's qc: is always
                ! silently ignored (never enforced), so it isn't checked here.
                select case (trim(cinfo%col(i)%data_type))
                case ("int32", "int64", "float32", "float64")
                    if (cinfo%col(i)%has_qc_min) then
                        if (.not. parquet_qc_numeric_bound( &
                                cinfo%col(i)%qc_min_raw, cinfo%col(i)%data_type, qc_bound_value)) then
                            errors = errors // "field '" // cur_name // "' has an invalid qc: min value '" // &
                                trim(cinfo%col(i)%qc_min_raw) // "' for data_type " // trim(cinfo%col(i)%data_type) // "; "
                        end if
                    end if
                    if (cinfo%col(i)%has_qc_max) then
                        if (.not. parquet_qc_numeric_bound( &
                                cinfo%col(i)%qc_max_raw, cinfo%col(i)%data_type, qc_bound_value)) then
                            errors = errors // "field '" // cur_name // "' has an invalid qc: max value '" // &
                                trim(cinfo%col(i)%qc_max_raw) // "' for data_type " // trim(cinfo%col(i)%data_type) // "; "
                        end if
                    end if
                end select
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
        ! Relayed through parquet_parse_protected_cols_relay (parquet_metadata.f90) rather than
        ! called directly -- see that wrapper's own comment for the gfortran 15.2.0 ICE it avoids.
        call parquet_parse_protected_cols_relay(maml%lines, protected_names)
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

    !> Loads maml (a filename) from disk and validates it (parquet_load_maml_file
    !> already validates internally, but this keeps that requirement explicit
    !> and self-contained here rather than depending on that side effect).
    module procedure parquet_validate_maml_file
        type(parquet_maml_file) :: loaded_maml

        loaded_maml = parquet_load_maml_file(maml)
        call parquet_validate_maml_internal(loaded_maml)
    end procedure parquet_validate_maml_file

    module procedure parquet_load_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_maml_file: cannot open file: " // trim(filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        maml%name = trim(filename)

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

    module procedure parquet_load_qc_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_qc_maml_file: cannot open file: " // trim(filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        schema%maml%name = trim(filename)

        max_len = 1
        do i = 1, nlines
            max_len = max(max_len, len_trim(lines(i)))
        end do

        allocate(character(len=max_len) :: schema%maml%lines(nlines))
        do i = 1, nlines
            schema%maml%lines(i) = lines(i)(1:max_len)
        end do
        ! Deliberately no parquet_validate_maml call here -- a qc-maml has its
        ! own, lighter validation (parquet_parse_qc_maml), run later once
        ! parquet_open_reader actually uses it.
    end procedure parquet_load_qc_maml_file

    !> Grows `rules(:)` by one empty entry and increments `n` -- same
    !> grow-by-one-element pattern as parquet_append_line/parquet_filter_add
    !> elsewhere in this codebase; qc-maml field counts are always small, so
    !> no capacity-doubling scheme is warranted.
    subroutine parquet_qc_append_empty_rule(rules, n)
        type(parquet_qc_rule), allocatable, intent(inout) :: rules(:) !! rule array being grown.
        integer, intent(inout) :: n !! number of rules in use; incremented by 1.
        type(parquet_qc_rule), allocatable :: tmp(:)

        n = n + 1
        if (.not. allocated(rules)) then
            allocate(rules(1))
            return
        end if
        if (size(rules) < n) then
            allocate(tmp(n))
            tmp(1:n-1) = rules
            call move_alloc(tmp, rules)
        end if
    end subroutine parquet_qc_append_empty_rule

    module procedure parquet_parse_qc_maml
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue, raw, errors, miss_lower
        character(len=:), allocatable :: qc_maml_suffix
        logical :: in_fields, have_current, in_qc
        integer :: i, j, n
        character(len=32) :: idx_buf
        type(parquet_qc_rule), allocatable :: tmp(:)
        character(len=:), allocatable :: tlo1, tlo3, tlo4 !! scratch (to_lower).
        character(len=:), allocatable :: tuq2, tuq5 !! scratch (unquote).

        ! "" if maml%name was never set (e.g. a qc-maml built in memory via
        ! add_col_qc); otherwise " (maml: X)", appended to every error stop
        ! below so it names which qc-maml file failed validation.
        qc_maml_suffix = ""
        if (allocated(maml%name)) then
            if (len_trim(maml%name) > 0) qc_maml_suffix = " (maml: " // trim(maml%name) // ")"
        end if

        ! Reuses the same top-level-section/sub-key name schema every other
        ! MAML validation path checks against (allowed_maml_sections/
        ! allowed_maml_nested_sections) -- so a typo'd section name or an
        ! unrecognized fields:/qc: sub-key is still caught here, exactly as
        ! it would be for a schema-authoring maml. Everything else
        ! parquet_validate_maml_internal additionally requires (table:, at
        ! least one field, valid data_type, ...) is deliberately NOT applied
        ! to a qc-maml -- see parquet_qc_rule's own doc comment.
        errors = ""
        call parquet_validate_maml_sections(maml%lines, errors)
        if (len_trim(errors) > 0) then
            error stop "parquet_open_reader: invalid qc maml: " // trim(errors) // qc_maml_suffix
        end if

        in_fields = .false.
        have_current = .false.
        in_qc = .false.
        n = 0

        do i = 1, size(maml%lines)
            line = maml%lines(i)
            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (tline == "fields:") in_fields = .true.
                cycle
            end if

            ! A new top-level section (unindented, has a ":", not a dash
            ! item) ends the fields: block, same as parquet_parse_maml_lines.
            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. line(1:1) /= " ") exit

            if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                call parquet_qc_append_empty_rule(tmp, n)
                have_current = .true.
                in_qc = .false.
                tline = trim(adjustl(tline(2:)))
                if (len_trim(tline) == 0) cycle
            end if

            if (.not. have_current) cycle

            if (in_qc) then
                call parquet_split_key_value(tline, key, cvalue)
                call parquet_to_lower(key, tlo1)
                select case (tlo1)
                case ("min")
                    call parquet_set_qc_bound(tmp(n)%has_min, tmp(n)%min_op, raw, cvalue, ">=")
                    tmp(n)%min_text = raw
                    cycle
                case ("max")
                    call parquet_set_qc_bound(tmp(n)%has_max, tmp(n)%max_op, raw, cvalue, "<=")
                    tmp(n)%max_text = raw
                    cycle
                case ("miss")
                    call parquet_unquote(cvalue, tuq2)
                    call parquet_to_lower(tuq2, tlo3)
                    miss_lower = trim(tlo3)
                    if (len_trim(miss_lower) == 0) then
                        tmp(n)%null_values_allowed = .false.
                    else if (trim(miss_lower) == "null" .or. trim(miss_lower) == "na") then
                        tmp(n)%null_values_allowed = .true.
                    else
                        error stop "parquet_open_reader: invalid qc maml: qc: miss: value '" // trim(miss_lower) // &
                            "' for field '" // trim(tmp(n)%name) // &
                            "' is not recognized (expected Null/NA or empty)" // qc_maml_suffix
                    end if
                    cycle
                case default
                    in_qc = .false.
                end select
            end if

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            call parquet_to_lower(key, tlo4)
            select case (tlo4)
            case ("name")
                call parquet_unquote(cvalue, tuq5)
                tmp(n)%name = tuq5
            case ("qc")
                in_qc = .true.
                tmp(n)%has_qc_block = .true.
            end select
        end do

        do i = 1, n
            if (len_trim(tmp(i)%name) == 0) then
                write(idx_buf, '(I0)') i
                error stop "parquet_open_reader: invalid qc maml: field #" // trim(idx_buf) // &
                    " is missing required 'name'" // qc_maml_suffix
            end if
            do j = 1, i - 1
                if (trim(tmp(j)%name) == trim(tmp(i)%name)) then
                    error stop "parquet_open_reader: invalid qc maml: duplicate field name '" // trim(tmp(i)%name) // &
                        "'" // qc_maml_suffix
                end if
            end do
            ! qc: min: must be a lower bound (>= or >), qc: max: an upper
            ! bound (<= or <); the reversed direction is a nonsensical bound.
            ! Unlike the write side this can't (and needn't) consult a
            ! data_type -- a qc-maml has none -- so it applies to every field.
            if (tmp(i)%has_min .and. tmp(i)%min_op(1:1) == "<") then
                error stop "parquet_open_reader: invalid qc maml: qc: min: for field '" // trim(tmp(i)%name) // &
                    "' uses a '" // trim(tmp(i)%min_op) // "' operator; min: accepts only >= or > (use max: for an upper bound)" &
                    // qc_maml_suffix
            end if
            if (tmp(i)%has_max .and. tmp(i)%max_op(1:1) == ">") then
                error stop "parquet_open_reader: invalid qc maml: qc: max: for field '" // trim(tmp(i)%name) // &
                    "' uses a '" // trim(tmp(i)%max_op) // "' operator; max: accepts only <= or < (use min: for a lower bound)" &
                    // qc_maml_suffix
            end if
        end do

        ! Fields with just a name: and no qc: block at all get no rule --
        ! same as a field never mentioned in this maml (see parquet_qc_rule's
        ! has_qc_block doc comment).
        allocate(rules(0))
        do i = 1, n
            if (tmp(i)%has_qc_block) rules = [rules, tmp(i)]
        end do
    end procedure parquet_parse_qc_maml

    module procedure parquet_set_qc_bound
        character(len=:), allocatable :: text
        character(len=:), allocatable :: tuq1 !! scratch (unquote).

        call parquet_unquote(cvalue, tuq1)
        text = trim(adjustl(tuq1))
        if (index(text, ">=") == 1) then
            op = ">="
            text = trim(adjustl(text(3:)))
        else if (index(text, "<=") == 1) then
            op = "<="
            text = trim(adjustl(text(3:)))
        else if (index(text, ">") == 1) then
            op = "> "
            text = trim(adjustl(text(2:)))
        else if (index(text, "<") == 1) then
            op = "< "
            text = trim(adjustl(text(2:)))
        else
            op = default_op
        end if
        raw = text
        has_flag = .true.
    end procedure parquet_set_qc_bound

    module procedure parquet_qc_numeric_bound
        integer :: ios
        real(real64) :: rounded

        value = 0.0_real64
        parquet_qc_numeric_bound = .false.

        read(raw, *, iostat=ios) value
        if (ios /= 0) return
        if (ieee_is_nan(value)) return
        if (.not. (abs(value) <= huge(1.0_real64))) return ! Inf (or a magnitude beyond real64's finite range)

        select case (trim(data_type))
        case ("int32")
            rounded = anint(value)
            if (value /= rounded) return
            if (rounded < -real(huge(0_int32), real64) - 1.0_real64 .or. rounded > real(huge(0_int32), real64)) return
        case ("int64")
            rounded = anint(value)
            if (value /= rounded) return
            if (rounded < -real(huge(0_int64), real64) .or. rounded >= real(huge(0_int64), real64)) return
        end select

        parquet_qc_numeric_bound = .true.
    end procedure parquet_qc_numeric_bound

end submodule parquet_metadata_maml
