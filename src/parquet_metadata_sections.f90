!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Backs parquet_validate_maml_sections (declared in parquet.f90): the
!> declarative schema of every top-level MAML section and its allowed
!> sub-keys, plus the validator and its private helpers that check a raw
!> MAML's section/sub-key names against it (presence only, not values).
submodule (parquet) parquet_metadata_sections
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

        idx = 0
        do k = 1, size(allowed_maml_sections)
            if (trim(parquet_to_lower(allowed_maml_sections(k)%name)) == trim(parquet_to_lower(key))) then
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

        idx = 0
        if (len_trim(parent_subkey) == 0) return ! GCOVR_EXCL_LINE
        do k = 1, size(allowed_maml_nested_sections)
            if (trim(parquet_to_lower(allowed_maml_nested_sections(k)%parent_section)) == &
                trim(parquet_to_lower(parent_section)) .and. &
                trim(parquet_to_lower(allowed_maml_nested_sections(k)%parent_subkey)) == &
                trim(parquet_to_lower(parent_subkey))) then ! GCOVR_EXCL_LINE
                idx = k
                return
            end if
        end do
    end function parquet_find_maml_nested_section

    !> Appends an "unknown sub-key" message to `errors` if `key` is not among
    !> allowed_maml_nested_sections(nested_idx)'s declared subkeys(:).
    subroutine parquet_check_maml_nested_subkey(nested_idx, key, errors)
        integer, intent(in) :: nested_idx !! index into allowed_maml_nested_sections.
        character(len=*), intent(in) :: key !! nested sub-key name found in the MAML.
        character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
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

    !> Appends an "unknown sub-key" message to `errors` if `key` is not among
    !> allowed_maml_sections(section_idx)'s declared subkeys(:).
    subroutine parquet_check_maml_subkey(section_idx, key, errors)
        integer, intent(in) :: section_idx !! index into allowed_maml_sections.
        character(len=*), intent(in) :: key !! sub-key name found in the MAML.
        character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
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

end submodule parquet_metadata_sections
