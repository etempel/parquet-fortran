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
submodule (parquet_core:parquet_metadata) parquet_metadata_maml
    use ieee_arithmetic, only: ieee_is_nan
    use iso_fortran_env, only: iostat_eor, iostat_end
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
    integer, parameter :: maml_max_subkeys = 9 !! Declared subkeys(:) capacity per maml_section_schema entry.

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
        maml_section_schema("survey",      .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("dataset",     .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("table",       .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("version",     .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("date",        .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("author",      .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("coauthors",   .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("dois",        .false., [character(len=32) :: "doi", "type", "", "", "", "", "", "", ""]), &
        maml_section_schema("depends",     .false., &
            [character(len=32) :: "survey", "dataset", "table", "version", "", "", "", "", ""]), &
        maml_section_schema("description", .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("comments",    .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("license",     .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("keywords",    .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("maml_version", .false., [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        maml_section_schema("keyarray",    .false., &
            [character(len=32) :: "key", "value", "comment", "", "", "", "", "", ""]), &
        maml_section_schema("extra",       .true.,  [character(len=32) :: "", "", "", "", "", "", "", "", ""]), &
        ! col_map: is deliberately NOT a top-level section: it is only valid
        ! nested inside extra: (see parquet_parse_col_map), which is already
        ! opaque/unvalidated here. A stray top-level "col_map:" is therefore
        ! correctly flagged as an unknown top-level section.
        ! "source" is read only by tools/generate_user_table_code.py, which turns a MAML into a
        ! generated parquet_table extension: it says whether a predefined column comes from the
        ! file ("file", the default) or is filled in by the program ("computed"). It is declared
        ! here so that such a MAML still passes parquet_validate_maml and stays usable as an
        ! ordinary write schema; nothing on the Fortran side reads it.
        maml_section_schema("fields",      .false., &
            [character(len=32) :: "name", "unit", "info", "ucd", "data_type", "array_size", "col_size", "qc", &
            "source"]) &
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

    module procedure parquet_maml_key_matches
        character(len=len(line)) :: t
        character(len=1) :: c
        integer :: i, n

        t = adjustl(line)
        n = len_trim(t)
        parquet_maml_key_matches = .false.
        if (n /= len_trim(key)) return
        do i = 1, n
            c = t(i:i)
            if (c >= "A" .and. c <= "Z") c = achar(iachar(c) + 32)
            if (c /= key(i:i)) return
        end do
        parquet_maml_key_matches = .true.
    end procedure parquet_maml_key_matches

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
            allocate(tmp(size(maml%missing_columns, kind=int64) + 1))
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
        character(len=:), allocatable :: nullable_names(:)
        character(len=32) :: idx_buf
        integer :: i, j, k
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

                call parquet_validate_field_rules(cinfo%col(i), errors)

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
        ! Relayed through parquet_parse_protected_cols_relay (parquet_metadata.f90) rather than
        ! called directly -- see that wrapper's own comment for the gfortran 15.2.0 ICE it avoids.
        call parquet_parse_protected_cols_relay(maml%lines, protected_names)
        ! extra: nullable_cols: is the opposite declaration and is checked against the same
        ! fields:, by the same rule (parquet_parse_nullable_cols, reached by host association from
        ! parquet_metadata -- the shape the relay comment above explains).
        call parquet_parse_nullable_cols(maml%lines, nullable_names)
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
            do i = 1, size(nullable_names)
                found = .false.
                do j = 1, size(cinfo%col)
                    if (trim(nullable_names(i)) == trim(cinfo%col(j)%output_name)) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    errors = errors // "nullable_cols: unknown column '" // trim(nullable_names(i)) // "'; "
                end if
            end do
        end if
        ! One column in both lists is a contradiction with no defensible reading -- one says the
        ! field must reject every Null, the other that it must be written able to hold one -- so it
        ! is refused rather than resolved by a precedence nobody wrote down. Checked whether or not
        ! the names are declared fields, so a MAML with both mistakes reports both.
        do i = 1, size(protected_names)
            do k = 1, size(nullable_names)
                if (trim(protected_names(i)) == trim(nullable_names(k))) then
                    errors = errors // "column '" // trim(protected_names(i)) // &
                        "' is listed under both protected_cols: and nullable_cols:; "
                    exit
                end if
            end do
        end do

        call parquet_validate_maml_sections(maml%lines, errors)

        if (len_trim(errors) > 0) then
            error stop "parquet_validate_maml: " // trim(errors)
        end if
    end procedure parquet_validate_maml_internal

    module procedure parquet_load_maml_file
        character(len=maml_max_line_len), allocatable :: lines(:)
        integer :: nlines, i, max_len

        ! gcov attribution artifact: this call is the first executable statement of this
        ! abbreviated module procedure body, right after its declarations. Confirmed exercised
        ! (test_load_maml_file in test/test_maml.f90; tools/coverage.sh shows this file at 100%
        ! locally) -- only GitLab CI's toolchain misattributes this one entry line, consistently
        ! across separate runs, same as the module-procedure-header shape documented in
        ! `.claude/rules/coverage.md`'s "Fortran gcov attribution artifacts".
        call parquet_read_maml_source_lines(filename, "parquet_load_maml_file", lines, nlines) ! GCOVR_EXCL_LINE

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

    !> Loads maml (a filename) from disk and validates it (parquet_load_maml_file
    !> already validates internally, but this keeps that requirement explicit
    !> and self-contained here rather than depending on that side effect).
    module procedure parquet_validate_maml_file
        type(parquet_maml_file) :: loaded_maml

        loaded_maml = parquet_load_maml_file(maml)
        call parquet_validate_maml_internal(loaded_maml)
    end procedure parquet_validate_maml_file

    !> Reads `filename` line-by-line into a freshly allocated `lines(:)`/`nlines`, shared by
    !! parquet_load_maml_file/parquet_load_qc_maml_file below. Strips a trailing CRLF `char(13)`
    !! from every line (a `.maml` file authored/edited on Windows retains one after a formatted
    !! read on a Unix build, which would otherwise survive into every parsed key/value and produce
    !! a misleading "invalid data_type"-style error naming what looks like a perfectly correct
    !! value); `error stop`s naming the offending line number if any single line exceeds
    !! maml_max_line_len characters, rather than silently truncating it with no diagnostic (a
    !! `read(unit,'(A)')` into a fixed-length variable is defined to discard the remainder of a
    !! longer record and still report `iostat == 0`) -- see CLAUDE.md's MAML parser robustness
    !! notes. `context` is the caller's own name, used as the error-message prefix, matching the
    !! "cannot open file" error below.
    subroutine parquet_read_maml_source_lines(filename, context, lines, nlines)
        character(len=*), intent(in) :: filename !! path to the .maml file to read.
        character(len=*), intent(in) :: context !! caller's own name, used as the error-message prefix.
        character(len=maml_max_line_len), allocatable, intent(out) :: lines(:) !! one element per source line.
        integer, intent(out) :: nlines !! number of lines read.
        character(len=maml_max_line_len) :: line
        integer :: unit, ios, reclen
        character(len=32) :: idx_buf, len_buf

        nlines = 0

        ! Concurrent non-advancing (advance='no', size=) reads on separate units have been
        ! observed to spuriously report ios == 0 (i.e. a false "line exceeds maml_max_line_len"
        ! abort on a line nowhere near that long) under heavy multi-threaded contention with
        ! ifx's I/O runtime -- e.g. 100+ OpenMP threads from test-drive's own concurrent
        ! "examples" suite each parsing the same static fixture in the schemas dir at once. Each
        ! thread opens its own unit, so this is not a shared-fixture race in this library's own
        ! logic; serializing the whole open/read-loop/close sequence works around the runtime
        ! race regardless of its exact cause. (Written "schemas dir" below, not "schemas/*.maml",
        ! since a literal "/*" anywhere in this file opens a C block comment under cpp -- see
        ! `.claude/rules/fortran-gotchas.md`'s "General Fortran & language gotchas".)
        !$omp critical (parquet_read_maml_source_lines_critical)
        open(newunit=unit, file=trim(filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop trim(context) // ": cannot open file: " // trim(filename)

        do
            ! Non-advancing read + size= is the standard idiom to detect a record longer than the
            ! buffer: ios == 0 (rather than iostat_eor) after the read means the buffer filled
            ! before the record ended, i.e. more of this line remains unread.
            read(unit, '(A)', advance='no', size=reclen, iostat=ios) line
            if (ios == iostat_end) exit
            if (ios /= 0 .and. ios /= iostat_eor) exit
            nlines = nlines + 1
            if (ios == 0) then
                write(idx_buf, '(I0)') nlines
                write(len_buf, '(I0)') maml_max_line_len
                error stop trim(context) // ": line " // trim(idx_buf) // " exceeds " // &
                    trim(len_buf) // " characters: " // trim(filename)
            end if
            if (reclen > 0) then
                if (line(reclen:reclen) == char(13)) reclen = reclen - 1
            end if
            call parquet_append_line(lines, nlines, line(1:reclen))
        end do

        close(unit)
        !$omp end critical (parquet_read_maml_source_lines_critical)
    end subroutine parquet_read_maml_source_lines

    module procedure parquet_load_qc_maml_file
        character(len=maml_max_line_len), allocatable :: lines(:)
        integer :: nlines, i, max_len

        ! gcov attribution artifact: same shape/cause as parquet_load_maml_file's call to this
        ! same helper above -- confirmed exercised (test_load_qc_maml_file in test/test_maml.f90;
        ! 100% locally) but consistently misattributed by GitLab CI's toolchain across separate
        ! runs. See `.claude/rules/coverage.md`'s "Fortran gcov attribution artifacts".
        call parquet_read_maml_source_lines(filename, "parquet_load_qc_maml_file", lines, nlines) ! GCOVR_EXCL_LINE

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
        character(len=maml_max_line_len) :: line
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
                if (parquet_maml_key_matches(tline, "fields:")) in_fields = .true.
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

    module procedure parquet_qc_bound_as_int64_text
        integer :: k, first, ios
        logical :: ok
        character(len=:), allocatable :: text

        value = 0_int64
        parquet_qc_bound_as_int64_text = .false.
        text = trim(adjustl(raw))
        if (len(text) == 0) return

        ! Shape first, by hand: an optional sign then nothing but digits, to the end. A bare
        ! list-directed read cannot do this -- it accepts "5 6" as 5, and "1.5" as 1.5 -- so the
        ! read below only ever runs on text already known to be a plain integer, and its iostat is
        ! then reporting one thing only: that the value does not fit in int64.
        first = 1
        if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        ok = len(text) >= first
        do k = first, len(text)
            if (text(k:k) < "0" .or. text(k:k) > "9") then
                ok = .false.
                exit
            end if
        end do
        if (.not. ok) return

        read(text, *, iostat=ios) value
        if (ios /= 0) then
            value = 0_int64
            return
        end if
        parquet_qc_bound_as_int64_text = .true.
    end procedure parquet_qc_bound_as_int64_text

    module procedure parquet_qc_numeric_bound
        integer :: ios
        real(real64) :: rounded
        integer(int64) :: exact

        value = 0.0_real64
        parquet_qc_numeric_bound = .false.

        ! An integer-typed bound is judged as an int64 whenever the text IS a plain integer, so a
        ! bound past 2**53 -- huge(int64) included -- validates exactly rather than being tested
        ! against a rounded copy of itself. `value` is still handed back as real64 for the callers
        ! that compare in real64; the int64 write path re-parses the text for its own comparison
        ! (see qc_numeric_i64), so no precision is lost where it matters.
        select case (trim(data_type))
        case ("int32", "int64")
            if (parquet_qc_bound_as_int64_text(raw, exact)) then
                if (trim(data_type) == "int32") then
                    if (exact < -int(huge(0_int32), int64) - 1_int64 .or. exact > int(huge(0_int32), int64)) return
                end if
                value = real(exact, kind=real64)
                parquet_qc_numeric_bound = .true.
                return
            end if
        end select

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

    !> Renames the column each parquet_read_qc entry declares. Only the entry's FIRST field is
    !> touched -- everything from the first comma onwards is carried across byte for byte -- so
    !> this needs no knowledge of the bound grammar beyond where the column name ends, and a bound
    !> value that happens to spell a column name cannot be hit. See parquet_read_qc's own doc
    !> comment for what this is for; it is the qc sibling of parquet_filter%remap_column_names.
    module procedure parquet_read_qc_remap_column_names
        character(len=:), allocatable :: name, rest, text, tmp(:)
        integer :: i, k, comma

        if (size(from) /= size(to)) error stop "parquet_read_qc%remap_column_names: from and to " // &
            "must have the same size"
        if (size(from) == 0 .or. this%n == 0) return

        do i = 1, this%n
            comma = index(this%entries(i), ",")
            if (comma > 0) then
                name = trim(adjustl(this%entries(i)(1:comma - 1)))
                rest = trim(this%entries(i)(comma:))
            else
                name = trim(adjustl(this%entries(i)))
                rest = ""
            end if
            ! An entry with no column name at all is left alone, so the real error comes from
            ! parquet_compose_read_qc's own %add_col_qc call, which says exactly what is wrong.
            if (len(name) == 0) cycle
            do k = 1, size(from)
                if (name /= trim(from(k))) cycle
                text = trim(to(k)) // rest
                if (len(text) > read_qc_max_entry_len) then
                    ! Raised here rather than left to %add_col_qc, whose message would name a
                    ! limit the caller never exceeded: their own entry fitted.
                    error stop "parquet_read_qc%remap_column_names: entry exceeds the maximum " // &
                        "supported length after remapping its column name: " // text(1:100) // "..."
                end if
                if (len(text) <= len(this%entries)) then
                    this%entries(i) = text
                else
                    ! Every entry shares one length (the %add convention), so a longer rewritten
                    ! entry re-lengthens the whole array.
                    allocate(character(len=len(text)) :: tmp(this%n))
                    tmp(1:this%n) = this%entries(1:this%n)
                    tmp(i) = text
                    call move_alloc(tmp, this%entries)
                end if
                exit
            end do
        end do
    end procedure parquet_read_qc_remap_column_names

    !> Merges a MAML-declared and a code-declared read-time QC into one schema. See the interface
    !> in parquet_core.f90 for the override rule and why `composed` carries only qc-bearing entries.
    !>
    !> The MAML side is copied as raw SOURCE LINES rather than re-emitted from the parsed
    !> parquet_qc_rule array: a round trip through that representation would have to reconstruct
    !> the compact "col, min, max, miss" form from min_op/min_text/null_values_allowed, which is a
    !> third representation of the same thing and one more place for the three to drift.
    !>
    !> `parquet_parse_qc_maml` already drops any field with no `qc:` key (see its own tail), so the
    !> rules it returns ARE exactly the columns the MAML claims under Q9's `has_qc_block` rule --
    !> there is no separate test to keep in step with it.
    module procedure parquet_get_qc_columns
        type(parquet_qc_rule), allocatable :: rules(:)
        integer :: i, n, w

        allocate(character(len=1) :: names(0))
        if (.not. allocated(schema%maml%lines)) return
        call parquet_parse_qc_maml(schema%maml, rules)
        ! Two passes, as everywhere a deferred-length array is built here: one to size it, one to
        ! fill it. A rule with no qc: block declares nothing and is skipped -- that distinction is
        ! the whole reason this reports rules rather than fields.
        n = 0
        w = 1
        do i = 1, size(rules)
            if (.not. rules(i)%has_qc_block) cycle
            n = n + 1
            w = max(w, len_trim(rules(i)%name))
        end do
        if (n == 0) return
        deallocate(names)
        allocate(character(len=w) :: names(n))
        n = 0
        do i = 1, size(rules)
            if (.not. rules(i)%has_qc_block) cycle
            n = n + 1
            names(n) = trim(rules(i)%name)
        end do
    end procedure parquet_get_qc_columns

    module procedure parquet_compose_read_qc
        type(parquet_qc_rule), allocatable :: rules(:)
        character(len=:), allocatable :: lines(:), name
        integer :: i, j
        logical :: claimed

        ncolumns = 0
        allocate(rules(0))
        if (present(schema)) then
            if (allocated(schema%maml%lines)) then
                ! Validates the MAML as a side effect, which is wanted: a bad qc-maml should fail
                ! here rather than at parquet_open_reader, where the composed object is all that is
                ! left to name in the message.
                call parquet_parse_qc_maml(schema%maml, rules)
                call parquet_copy_qc_field_lines(schema%maml%lines, lines)
                if (allocated(lines)) composed%maml%lines = lines
            end if
            if (allocated(schema%maml%name)) composed%maml%name = schema%maml%name
        end if

        if (present(qc)) then
            do i = 1, qc%n
                call parquet_read_qc_entry_column(qc%entries(i), name)
                claimed = .false.
                do j = 1, size(rules)
                    if (trim(rules(j)%name) /= name) cycle
                    claimed = .true.
                    exit
                end do
                if (claimed) cycle
                ! %add_col_qc is what parses and validates the compact string -- this procedure
                ! deliberately never does, so there is exactly one implementation of that grammar.
                call composed%maml%add_col_qc(trim(qc%entries(i)))
            end do
        end if

        ! Counted by re-parsing rather than by tallying the loop above, so the answer is exactly
        ! "what a reader would enforce": an entry naming a column but declaring no bound at all
        ! ("mass" on its own) is a legal no-op that adds a field with no qc: block, and must not
        ! count towards it.
        if (allocated(composed%maml%lines)) then
            call parquet_parse_qc_maml(composed%maml, rules)
            ncolumns = size(rules)
        end if
    end procedure parquet_compose_read_qc

    !> The column name a compact "col, min, max, miss" entry declares: everything before its first
    !> comma, trimmed. "" for an entry with no name, which the caller leaves for %add_col_qc to
    !> reject with its own message.
    subroutine parquet_read_qc_entry_column(entry, name)
        character(len=*), intent(in) :: entry            !! one parquet_read_qc entry.
        character(len=:), allocatable, intent(out) :: name !! the column it declares, or "".
        integer :: comma

        comma = index(entry, ",")
        if (comma > 0) then
            name = trim(adjustl(entry(1:comma - 1)))
        else
            name = trim(adjustl(entry))
        end if
    end subroutine parquet_read_qc_entry_column

    !> Copies the `fields:` entries that carry a `qc:` key out of a MAML's source lines, header
    !> included, dropping everything else -- other top-level sections, and any field entry with no
    !> `qc:` key. `lines` is left unallocated when nothing qualifies.
    !>
    !> Entry boundaries are detected exactly as parquet_parse_qc_maml detects them (an UNINDENTED
    !> dash starts a field; an unindented non-dash key ends the block), deliberately: if the two
    !> ever disagree about where an entry begins, the composed schema would silently claim a
    !> different set of columns than the reader enforces.
    subroutine parquet_copy_qc_field_lines(src, lines)
        character(len=*), intent(in) :: src(:)                !! the MAML's raw source lines.
        character(len=:), allocatable, intent(out) :: lines(:) !! the qc-bearing fields: block, or unallocated.
        integer, allocatable :: lo(:), hi(:)
        integer :: i, j, k, nentry, nout, width

        call parquet_locate_qc_field_entries(src, lo, hi, nentry)
        if (nentry == 0) return

        width = len("fields:")
        nout = 1
        do k = 1, nentry
            do j = lo(k), hi(k)
                nout = nout + 1
                width = max(width, len_trim(src(j)))
            end do
        end do

        allocate(character(len=width) :: lines(nout))
        ! Blanked element by element, never as `lines = ""` -- see CLAUDE.md: a whole-array
        ! assignment to a deferred-length allocatable array reallocates it to length zero.
        do i = 1, nout
            lines(i) = ""
        end do
        lines(1) = "fields:"
        nout = 1
        do k = 1, nentry
            do j = lo(k), hi(k)
                nout = nout + 1
                lines(nout) = src(j)
            end do
        end do
    end subroutine parquet_copy_qc_field_lines

    !> Line ranges of the `fields:` entries that carry a `qc:` key, one (lo, hi) pair per entry.
    !> `nentry` is 0 when the MAML has no fields: block, or none of its entries declares qc.
    !>
    !> Entry boundaries are detected exactly as parquet_parse_qc_maml detects them (an UNINDENTED
    !> dash starts a field; an unindented non-dash key ends the block), deliberately: if the two
    !> ever disagreed about where an entry begins, the composed schema would silently claim a
    !> different set of columns than the reader enforces.
    subroutine parquet_locate_qc_field_entries(src, lo, hi, nentry)
        character(len=*), intent(in) :: src(:)          !! the MAML's raw source lines.
        integer, allocatable, intent(out) :: lo(:)      !! first line of each qualifying entry.
        integer, allocatable, intent(out) :: hi(:)      !! last line of each qualifying entry.
        integer, intent(out) :: nentry                  !! qualifying entries found.
        character(len=:), allocatable :: tline, key, cvalue, klow
        integer :: i, j, start, stop_at
        logical :: in_fields, has_qc

        allocate(lo(max(size(src, kind=int64), 1_int64)), hi(max(size(src, kind=int64), 1_int64)))
        nentry = 0
        in_fields = .false.
        i = 0
        do
            i = i + 1
            if (i > size(src)) exit
            tline = trim(adjustl(src(i)))
            if (len(tline) == 0) cycle
            if (tline(1:1) == "#") cycle
            if (.not. in_fields) then
                if (parquet_maml_key_matches(tline, "fields:")) in_fields = .true.
                cycle
            end if
            ! A new unindented top-level key ends the fields: block.
            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. src(i)(1:1) /= " ") exit
            if (index(tline, "-") /= 1 .or. src(i)(1:1) == " ") cycle
            ! src(i) starts a field entry; walk its indented sub-lines to find where it ends.
            start = i
            stop_at = size(src)
            do
                if (i + 1 > size(src)) exit
                tline = trim(adjustl(src(i + 1)))
                if (len(tline) > 0 .and. src(i + 1)(1:1) /= " ") then
                    stop_at = i
                    exit
                end if
                i = i + 1
            end do
            has_qc = .false.
            do j = start, stop_at
                tline = trim(adjustl(src(j)))
                if (len(tline) == 0) cycle
                if (j == start) tline = trim(adjustl(tline(2:)))
                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle
                call parquet_to_lower(key, klow)
                if (trim(klow) == "qc") then
                    has_qc = .true.
                    exit
                end if
            end do
            if (.not. has_qc) cycle
            nentry = nentry + 1
            lo(nentry) = start
            hi(nentry) = stop_at
        end do
    end subroutine parquet_locate_qc_field_entries

end submodule parquet_metadata_maml
