!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for MAML parsing/validation and parquet_column_info helpers.
!> Mostly "happy path": most negative cases in this module's functions
!> trigger `error stop`, which aborts the whole test process, so those run as
!> subprocess scenarios (test/error_scenarios.f90) instead -- some via
!> check_scenario_exit_status directly below (schema%init/add_field), others
!> in test_errors.f90.
!>
!> NB: always pass the failure message directly to `check(error, cond, message)`.
!> Do NOT follow a failed `check` with a separate `test_failed` call on the same
!> `error` variable: `check` already allocates `error` internally when `cond` is
!> false, and passing that already-allocated `error` into another `intent(out)`
!> argument (as `test_failed` also expects) triggers Fortran's automatic
!> finalization of the old value before reassignment, which for test-drive's
!> `error_type` calls its FINAL `escalate_error` and aborts the whole process.
module test_maml
    use parquet
    use parquet_maml_base, only : parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : int32, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_stderr
    !
    implicit none
    private
    public :: collect_tests_parquet_maml
    !
contains
    !
    subroutine collect_tests_parquet_maml(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("validate a well-formed MAML file", test_validate_maml_ok), &
            new_unittest("validate maml_example2.maml (extra top-level keys, depends:, extra:, " // &
                "list-form ucd:, keyarray:)", test_validate_maml_example2_ok), &
            new_unittest("validate maml_example2.maml by filename (parquet_validate_maml overload)", &
                test_validate_maml_by_filename_ok), &
            new_unittest("a fields: sub-key other than qc: with its own deeper-indented content is " // &
                "silently ignored (no declared nested schema)", test_validate_maml_unmatched_nested_ok), &
            new_unittest("keyarray:/DOIs:/depends: entries parse correctly with a bare dash and first-key " // &
                "variations", test_keyarray_dois_depends_key_variations), &
            new_unittest("a generic (non-comments/coauthors/keywords) top-level list section parses, " // &
                "skipping a malformed indented line", test_generic_list_section_and_malformed_line), &
            new_unittest("validate a user MAML that is a valid subset", test_validate_user_maml_ok), &
            new_unittest("col_map: renames a field to an internal name", test_validate_user_maml_col_map_ok), &
            new_unittest("load a MAML file from disk", test_load_maml_file), &
            new_unittest("get_column_index finds an existing column", test_get_column_index_found), &
            new_unittest("get_num_fields/get_field_name report all fields, unfiltered", &
                test_get_num_fields_and_field_name), &
            new_unittest("get_num_fields on a freshly declared (uninitialized) schema is 0", &
                test_get_num_fields_uninitialized), &
            new_unittest("set_unavailable/set_available toggle is_set", test_set_available_unavailable), &
            new_unittest("schema_new = schema_old deep-copies independently, even overwriting an " // &
                "already-initialized schema_new", test_schema_deep_copy_independence), &
            new_unittest("add_col_qc builds a qc-maml from compact strings", test_add_col_qc_builds_maml), &
            new_unittest("add_col_qc result reads back through parquet_open_reader", test_add_col_qc_roundtrip), &
            new_unittest("add_col_qc with an empty input is a no-op", test_add_col_qc_empty_input_is_noop), &
            new_unittest("set_col_qc (in-place form) returns the name in place", test_set_col_qc), &
            new_unittest("schema%init emits the requested top-level keys", test_schema_init_builds_top_level_lines), &
            new_unittest("parquet_schema(...) constructor matches schema%init", &
                test_schema_constructor_matches_init), &
            new_unittest("schema%add_field builds exact fields: lines (incl. a qc: block)", &
                test_schema_add_field_builds_maml_lines), &
            new_unittest("schema%init + add_field round-trips through parquet_parse_maml", &
                test_schema_init_add_field_parses_correctly), &
            new_unittest("schema%init + add_field writes/reads a real parquet file", &
                test_schema_init_add_field_write_read_roundtrip), &
            new_unittest("schema%add_field accepts a bare qc_min with no operator", &
                test_schema_add_field_bare_qc_bound), &
            new_unittest("schema%add_field before schema%init aborts", test_schema_add_field_before_init_aborts), &
            new_unittest("schema%init called twice aborts", test_schema_init_twice_aborts), &
            new_unittest("schema%init after a MAML parse (no force) aborts", &
                test_schema_init_after_maml_parse_aborts), &
            new_unittest("loading a .maml file into an already-initialized schema aborts", &
                test_parse_maml_file_after_init_aborts), &
            new_unittest("schema%init(force=.true.) after a MAML parse gives a clean slate", &
                test_schema_init_force_after_maml_parse_ok), &
            new_unittest("loading a .maml file after schema%clear() succeeds", &
                test_parse_maml_file_after_clear_ok), &
            new_unittest("schema%init with an empty table aborts", test_schema_init_empty_table_aborts), &
            new_unittest("schema%add_field with an empty name aborts", test_schema_add_field_empty_name_aborts), &
            new_unittest("schema%add_field with a duplicate name aborts", &
                test_schema_add_field_duplicate_name_aborts), &
            new_unittest("schema%add_field with an invalid data_type aborts", &
                test_schema_add_field_invalid_data_type_aborts), &
            new_unittest("schema%add_field: reversed qc_min operator aborts", &
                test_schema_add_field_qc_min_reversed_operator_aborts), &
            new_unittest("schema%add_field: reversed qc_max operator aborts", &
                test_schema_add_field_qc_max_reversed_operator_aborts), &
            new_unittest("schema%add_field: qc operator with no value aborts", &
                test_schema_add_field_qc_operator_without_value_aborts), &
            new_unittest("schema%add_field: invalid qc_miss value aborts", &
                test_schema_add_field_bad_qc_miss_value_aborts), &
            new_unittest("schema%add_field accepts date/time[unit]/timestamp[unit,utc] tokens", &
                test_schema_add_field_temporal_tokens_ok), &
            new_unittest("schema%add_field: a unit suffix on date aborts", &
                test_schema_add_field_date_with_unit_aborts), &
            new_unittest("parquet_validate_maml rejects qc: on a temporal field", &
                test_validate_qc_on_temporal_column_aborts), &
            new_unittest("schema%add_field: an explicit seconds unit aborts (Parquet has no " // &
                "seconds-resolution TIME/TIMESTAMP encoding)", test_schema_add_field_seconds_unit_aborts), &
            new_unittest("schema%add_field: a ,utc suffix on time (not timestamp) aborts", &
                test_schema_add_field_time_utc_aborts), &
            new_unittest("schema%add_field: an unclosed unit bracket aborts", &
                test_schema_add_field_unclosed_bracket_aborts), &
            new_unittest("schema%is_init reflects state before/after schema%init", test_schema_is_init_reflects_state), &
            new_unittest("schema%is_init is .true. after a MAML parse, even without %init", &
                test_schema_is_init_true_after_maml_parse), &
            new_unittest("schema%init(force=.true.) on a never-initialized schema behaves like a plain init", &
                test_schema_init_force_never_initialized_ok), &
            new_unittest("schema%init(force=.true.) fully resets fields/qc/metadata and can be reused", &
                test_schema_init_force_resets_and_reuses), &
            new_unittest("schema%clear resets the entire schema to its pristine state", &
                test_schema_clear_resets_to_pristine), &
            new_unittest("schema%clear on an already-blank schema is a no-op", &
                test_schema_clear_noop_on_blank_schema), &
            new_unittest("schema%clear then a plain %init makes the variable reusable", &
                test_schema_clear_then_reinit_reusable), &
            new_unittest("col_size: auto/array_size: auto parse to an unresolved, still-valid state", &
                test_parse_col_size_array_size_auto_ok), &
            new_unittest("a malformed col_size: value aborts", test_col_size_malformed_value_aborts), &
            new_unittest("a malformed array_size: value aborts", test_array_size_malformed_value_aborts), &
            new_unittest("array_size: auto on a non-string column aborts", &
                test_array_size_auto_on_non_string_aborts), &
            new_unittest("set_col_size resolves an 'auto' column", test_set_col_size_resolves_auto), &
            new_unittest("set_col_size with a non-positive value aborts", test_set_col_size_non_positive_aborts), &
            new_unittest("set_col_size on an already-resolved column aborts without force=.true.", &
                test_set_col_size_already_resolved_no_force_aborts), &
            new_unittest("set_col_size(force=.true.) overrides an already-resolved column", &
                test_set_col_size_force_overrides), &
            new_unittest("set_array_size resolves an 'auto' string column", test_set_array_size_resolves_auto), &
            new_unittest("set_array_size on a non-string column aborts", &
                test_set_array_size_non_string_column_aborts), &
            new_unittest("set_array_size with a non-positive value aborts", &
                test_set_array_size_non_positive_aborts), &
            new_unittest("set_array_size on an already-resolved column aborts without force=.true.", &
                test_set_array_size_already_resolved_no_force_aborts), &
            new_unittest("set_array_size(force=.true.) overrides an already-resolved column", &
                test_set_array_size_force_overrides) &
            ]
    end subroutine collect_tests_parquet_maml

    subroutine test_validate_maml_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = get_parquet_maml("maml_example.maml")

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml(maml)

        call check(error, .true.)
    end subroutine test_validate_maml_ok

    !> schemas/maml_example2.maml exercises several things maml_example.maml does
    !> not: extra top-level keys (survey, version, date, depends:, keywords:,
    !> MAML_version), an extra: block, list-form ucd: on some fields, and
    !> blank array_size:/col_size: values -- none of which parquet_validate_maml
    !> should object to, since it only checks field names/data_type/table:.
    subroutine test_validate_maml_example2_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file("schemas/maml_example2.maml")

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml(maml)

        call check(error, .true.)
    end subroutine test_validate_maml_example2_ok

    !> parquet_find_maml_nested_section (parquet_metadata_sections.f90) only
    !> declares a nested schema for fields:'s "qc:" sub-key; a deeper-indented
    !> block under any other fields: sub-key (here "info:") has no declared
    !> nested schema, so it must be silently skipped rather than flagged as
    !> an error -- covers the case where that lookup exhausts every entry in
    !> allowed_maml_nested_sections without a match (idx stays 0).
    subroutine test_validate_maml_unmatched_nested_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml%name = "unmatched_nested.maml"
        maml%lines = [character(len=40) :: &
            "table: unmatched_nested_table", &
            "Fields:", &
            "- name: id0", &
            "  data_type: int32", &
            "  info: some description", &
            "    extra_note: ignored" ]

        ! Should not error stop: the "extra_note:" line is nested one level
        ! deeper than fields:'s own sub-keys, under "info:" rather than
        ! "qc:", so it has no declared nested schema and is left unvalidated.
        call parquet_validate_maml(maml)

        call check(error, .true.)
    end subroutine test_validate_maml_unmatched_nested_ok

    !> parquet_validate_maml is generic: it also accepts a filename directly
    !> (loading the file from disk internally), instead of requiring the
    !> caller to first call parquet_load_maml_file themselves.
    subroutine test_validate_maml_by_filename_ok(error)
        type(error_type), allocatable, intent(out) :: error

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml("schemas/maml_example2.maml")

        call check(error, .true.)
    end subroutine test_validate_maml_by_filename_ok

    !> schemas/maml_example.maml/maml_example2.maml (and every other fixture)
    !> always put keyarray:'s "key:" inline with the leading dash ("- key:
    !> X"), and depends:'s dash-line entries always start with "survey:"
    !> first. Both are just conventions, not requirements: parquet_parse_maml_lines
    !> (src/parquet_metadata.f90) also accepts a bare "-" line followed by
    !> "key:"/"doi:" on their own indented line, and depends: entries whose
    !> first (dash-line) key is "dataset:"/"table:" instead of "survey:",
    !> with "survey:" itself then appearing as a later indented continuation
    !> line, and a depends: entry whose first (dash-line) key is "version:".
    !> This test is the only one exercising those variations. It also
    !> spells the fields: header "Fields:" -- the fast-path literal check
    !> for "fields:" is case-sensitive, but the fallback generic key/value
    !> path (reached because none of keyarray:/DOIs:/depends:/extra: match
    !> either) lowercases the key first, so "Fields:" still works; no other
    !> fixture in this repo uses anything but lowercase "fields:".
    subroutine test_keyarray_dois_depends_key_variations(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: i
        logical :: found_keyarray, found_doi, found_depends1, found_depends2, found_depends3

        schema%maml%name = "key_variations.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: key_variations_table", &
            "keyarray:", &
            "-", &
            "  key: test_bare", &
            "  value: 1.5", &
            "  comment: bare form", &
            "DOIs:", &
            "-", &
            "  doi: 10.9999/bare", &
            "  type: dataset", &
            "depends:", &
            "- dataset: SpecZ", &
            "  survey: The Medium Survey", &
            "- table: Phot_South", &
            "  version: 2", &
            "- version: 3", &
            "Fields:", &
            "- name: id0", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        found_keyarray = .false.
        found_doi = .false.
        found_depends1 = .false.
        found_depends2 = .false.
        found_depends3 = .false.

        do i = 1, size(schema%metadata%items)
            if (trim(schema%metadata%items(i)%key) == "test_bare") then
                found_keyarray = .true.
                call check(error, trim(schema%metadata%items(i)%value) == "1.5" .and. &
                    trim(schema%metadata%items(i)%description) == "bare form", &
                    "keyarray: entry with a bare dash and indented key: did not parse as expected")
                if (allocated(error)) return
            else if (trim(schema%metadata%items(i)%key) == "DOI_1") then
                found_doi = .true.
                call check(error, trim(schema%metadata%items(i)%value) == "10.9999/bare" .and. &
                    trim(schema%metadata%items(i)%description) == "dataset", &
                    "DOIs: entry with a bare dash and indented doi: did not parse as expected")
                if (allocated(error)) return
            else if (trim(schema%metadata%items(i)%key) == "depends_1") then
                found_depends1 = .true.
                call check(error, trim(schema%metadata%items(i)%value) == "The Medium Survey;SpecZ;;", &
                    "depends: entry starting with 'dataset:' (dash-line) did not parse as expected")
                if (allocated(error)) return
            else if (trim(schema%metadata%items(i)%key) == "depends_2") then
                found_depends2 = .true.
                call check(error, trim(schema%metadata%items(i)%value) == ";;Phot_South;2", &
                    "depends: entry starting with 'table:' (dash-line) did not parse as expected")
                if (allocated(error)) return
            else if (trim(schema%metadata%items(i)%key) == "depends_3") then
                found_depends3 = .true.
                call check(error, trim(schema%metadata%items(i)%value) == ";;;3", &
                    "depends: entry starting with 'version:' (dash-line) did not parse as expected")
                if (allocated(error)) return
            end if
        end do

        call check(error, found_keyarray, "expected keyarray: entry 'test_bare' not found")
        if (allocated(error)) return
        call check(error, found_doi, "expected DOIs: entry 'DOI_1' not found")
        if (allocated(error)) return
        call check(error, found_depends1, "expected depends: entry 'depends_1' not found")
        if (allocated(error)) return
        call check(error, found_depends2, "expected depends: entry 'depends_2' not found")
        if (allocated(error)) return
        call check(error, found_depends3, "expected depends: entry 'depends_3' not found")
    end subroutine test_keyarray_dois_depends_key_variations

    !> Any top-level plain-string list section other than comments:/coauthors:/
    !> keywords: falls through to the generic per-item
    !> metadata%add_metadata(list_key, ...) branch -- one metadata entry per
    !> item, all sharing the section's own key. license: is normally a plain
    !> scalar, but nothing in allowed_maml_sections restricts it to that form
    !> (it declares no subkeys, so parquet_validate_maml_sections leaves its
    !> content unvalidated), so it doubles as a convenient stand-in here for
    !> an otherwise-untested generic list section. Also checks that a stray
    !> indented "key: value" line inside such a list (neither a "- " item
    !> nor a new top-level section) is silently skipped rather than breaking
    !> the list or being misparsed as a new section.
    subroutine test_generic_list_section_and_malformed_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: i, license_count

        schema%maml%name = "generic_list.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: generic_list_table", &
            "license:", &
            "- first term", &
            "  stray: junk", &
            "- second term", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        license_count = 0
        do i = 1, size(schema%metadata%items)
            if (trim(schema%metadata%items(i)%key) == "license") then
                license_count = license_count + 1
                call check(error, trim(schema%metadata%items(i)%value) == "first term" .or. &
                    trim(schema%metadata%items(i)%value) == "second term", &
                    "unexpected value for a generic 'license' list item")
                if (allocated(error)) return
            end if
        end do

        call check(error, license_count == 2, &
            "expected exactly 2 'license' metadata entries (the stray indented line must not add one)")
    end subroutine test_generic_list_section_and_malformed_line

    subroutine test_validate_user_maml_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: base_maml
        type(parquet_schema) :: schema
        integer :: idx

        base_maml = get_parquet_maml("maml_example.maml")

        schema%maml%name = "user_subset.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32", &
            "- name: name", &
            "  data_type: string", &
            "  array_size: 18" ]

        ! Should not error stop: every field in the user MAML (id0, name) exists in base_maml.
        call parquet_validate_user_maml(base_maml, schema%maml)

        call parquet_parse_maml(schema)

        ! Columns declared in base_maml but omitted from the user MAML (11 of the 13) are
        ! merged back in as deactivated placeholders, so the schema still covers every
        ! base column; only "id0" and "name" should actually be active (is_set).
        call check(error, size(schema%cinfo%col) == 13, &
            "expected the merged schema to cover all 13 base columns")
        if (allocated(error)) return

        idx = schema%get_column_index("id0")
        call check(error, schema%cinfo%col(idx)%is_set .and. .not. schema%cinfo%col(idx)%is_deactivated, &
            "expected 'id0' to be active (declared by the user MAML)")
        if (allocated(error)) return

        idx = schema%get_column_index("idarr")
        call check(error, (.not. schema%cinfo%col(idx)%is_set) .and. schema%cinfo%col(idx)%is_deactivated, &
            "expected 'idarr' to be merged in as a deactivated placeholder")
    end subroutine test_validate_user_maml_ok

    !> col_map: renames a field's internal (base) name to whatever name the
    !> user MAML's own fields: section declares. parquet_write_column etc.
    !> still address the column by its internal name ("id0"); only the
    !> resulting schema/parquet output uses the renamed name ("my_id").
    subroutine test_validate_user_maml_col_map_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: base_maml
        type(parquet_schema) :: schema
        integer :: idx

        base_maml = get_parquet_maml("maml_example.maml")

        schema%maml%name = "user_col_map.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - id0: my_id", &
            "fields:", &
            "- name: my_id", &
            "  data_type: int32", &
            "  info: renamed via col_map" ]

        ! Should not error stop: id0 (referenced by col_map) exists in base_maml.
        call parquet_validate_user_maml(base_maml, schema%maml)

        call check(error, allocated(schema%maml%col_map), "expected col_map to be populated on the user MAML")
        if (allocated(error)) return
        call check(error, size(schema%maml%col_map) == 1 .and. &
            trim(schema%maml%col_map(1)%internal_name) == "id0" .and. &
            trim(schema%maml%col_map(1)%output_name) == "my_id", &
            "unexpected schema%maml%col_map contents")
        if (allocated(error)) return

        call parquet_parse_maml(schema)

        ! The renamed field is stored under its internal name ("id0"), same as
        ! every other lookup (parquet_write_column, set_available, ...);
        ! output_name carries the user-facing rename ("my_id").
        idx = schema%get_column_index("id0")
        call check(error, idx > 0, "expected 'id0' (internal name) to be found via get_column_index")
        if (allocated(error)) return
        call check(error, schema%cinfo%col(idx)%is_set .and. .not. schema%cinfo%col(idx)%is_deactivated, &
            "expected the renamed field to be active")
        if (allocated(error)) return
        call check(error, trim(schema%cinfo%col(idx)%output_name) == "my_id", &
            "expected output_name to carry the col_map-declared name 'my_id'")
        if (allocated(error)) return
        call check(error, trim(schema%cinfo%col(idx)%info) == "renamed via col_map", &
            "expected the renamed field's own fields: attributes (info) to be preserved")
    end subroutine test_validate_user_maml_col_map_ok

    subroutine test_load_maml_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file("schemas/maml_example.maml")

        call check(error, trim(maml%name) == "schemas/maml_example.maml" .and. size(maml%lines) == 91, &
            "parquet_load_maml_file returned unexpected content")
    end subroutine test_load_maml_file

    subroutine test_get_column_index_found(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: idx

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        idx = schema%get_column_index("id0")
        call check(error, idx == 1, "get_column_index('id0') did not return the expected index")
        if (allocated(error)) return

        idx = schema%get_column_index("idarr")
        call check(error, idx == 2, "get_column_index('idarr') did not return the expected index")
    end subroutine test_get_column_index_found

    !> get_num_fields/get_field_name report every field declared in maml
    !> source order, regardless of set_column_unavailable -- unlike
    !> get_column_index, they are not a "currently active" view.
    subroutine test_get_num_fields_and_field_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        character(len=:), allocatable :: fname

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call check(error, schema%get_num_fields() == 13, &
            "get_num_fields did not return the expected total field count")
        if (allocated(error)) return

        call schema%get_field_name(1, fname)
        call check(error, trim(fname) == "id0", &
            "get_field_name(1) did not return the first declared field")
        if (allocated(error)) return

        call schema%get_field_name(13, fname)
        call check(error, trim(fname) == "flag_array", &
            "get_field_name(13) did not return the last declared field")
        if (allocated(error)) return

        ! Deactivating a field must not change the count or shift indices --
        ! get_num_fields/get_field_name see every declared field, not just
        ! the currently-active ones.
        call schema%set_column_unavailable("id0")
        call check(error, schema%get_num_fields() == 13, &
            "get_num_fields changed after set_column_unavailable")
        if (allocated(error)) return

        call schema%get_field_name(1, fname)
        call check(error, trim(fname) == "id0", &
            "get_field_name(1) changed after set_column_unavailable")
    end subroutine test_get_num_fields_and_field_name

    !> get_num_fields on a schema that has never had %init/parquet_parse_maml
    !> called on it (cinfo%col still unallocated) must return 0, not abort.
    subroutine test_get_num_fields_uninitialized(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call check(error, schema%get_num_fields() == 0, &
            "get_num_fields on an uninitialized schema should be 0")
    end subroutine test_get_num_fields_uninitialized

    subroutine test_set_available_unavailable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: idx

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        idx = schema%get_column_index("id0")
        call check(error, schema%cinfo%col(idx)%is_set, "expected id0 to be set by default after parquet_parse_maml")
        if (allocated(error)) return

        call schema%set_column_unavailable("id0")
        call check(error, .not. schema%cinfo%col(idx)%is_set, "set_column_unavailable('id0') did not clear is_set")
        if (allocated(error)) return

        call schema%set_column_available("id0")
        call check(error, schema%cinfo%col(idx)%is_set, "set_column_available('id0') did not restore is_set")
        if (allocated(error)) return

        call schema%set_column_unavailable()
        call check(error, all(.not. schema%cinfo%col(:)%is_set), &
            "set_column_unavailable() (no name) did not clear is_set for every column")
        if (allocated(error)) return

        call schema%set_column_available()
        call check(error, all(schema%cinfo%col(:)%is_set), &
            "set_column_available() (no name) did not set is_set for every column")
    end subroutine test_set_available_unavailable

    !> `schema_new = schema_old` (plain intrinsic assignment) is the supported way to duplicate an
    !> already-initialized schema: parquet_schema has no custom assignment(=) and no FINAL, unlike
    !> parquet_writer/parquet_reader, so gfortran's default structure assignment deep-copies every
    !> allocatable component (including nested ones in %maml/%cinfo/%metadata) instead of aliasing
    !> them. This is a safeguard for future schema extensions: if a new component is ever added
    !> that breaks value semantics (e.g. a pointer/handle needing an explicit deep-copy routine
    !> instead of relying on default assignment), mutating one copy leaking into the other -- or
    !> stale state surviving a copy into an already-initialized variable -- should start failing
    !> here.
    subroutine test_schema_deep_copy_independence(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema_old, schema_new
        integer :: idx_id0, idx_idlong, n_items_before, i
        logical :: found_ra

        call parquet_parse_maml("schemas/maml_example.maml", schema_old)
        idx_id0 = schema_old%get_column_index("id0")
        idx_idlong = schema_old%get_column_index("idlong")
        n_items_before = size(schema_old%metadata%items)

        ! schema_new starts out already initialized with a *different* schema -- the assignment
        ! below must fully replace this state, not merge with or leak from it.
        call parquet_parse_maml("schemas/maml_example2.maml", schema_new)
        call check(error, schema_new%get_num_fields() /= schema_old%get_num_fields(), &
            "test setup: maml_example.maml and maml_example2.maml unexpectedly have the same field count")
        if (allocated(error)) return

        schema_new = schema_old

        call check(error, schema_new%get_num_fields() == schema_old%get_num_fields(), &
            "schema_new = schema_old did not fully replace an already-initialized schema_new (field count)")
        if (allocated(error)) return
        call check(error, schema_new%get_column_index("id0") == idx_id0, &
            "schema_new = schema_old did not copy schema_old's fields (id0 not found)")
        if (allocated(error)) return
        ! get_column_index itself error stops on a not-found name, so check absence with a plain
        ! scan instead of relying on it to report "not found".
        found_ra = .false.
        do i = 1, size(schema_new%cinfo%col)
            if (allocated(schema_new%cinfo%col(i)%name)) then
                if (trim(schema_new%cinfo%col(i)%name) == "RA") found_ra = .true.
            end if
        end do
        call check(error, .not. found_ra, &
            "schema_new = schema_old left a stale field (RA) behind from schema_new's previous schema")
        if (allocated(error)) return
        call check(error, size(schema_new%metadata%items) == n_items_before, &
            "schema_new = schema_old did not copy metadata items")
        if (allocated(error)) return
        call check(error, schema_new%cinfo%col(idx_id0)%is_set .eqv. schema_old%cinfo%col(idx_id0)%is_set, &
            "schema_new = schema_old did not copy is_set state")
        if (allocated(error)) return

        ! Mutating the new copy must not leak back into the old one.
        call schema_new%set_column_unavailable("id0")
        call schema_new%add_metadata("copy_only_key", 99_int32)
        call check(error, schema_old%cinfo%col(idx_id0)%is_set, &
            "mutating schema_new leaked into schema_old (is_set)")
        if (allocated(error)) return
        call check(error, size(schema_old%metadata%items) == n_items_before, &
            "mutating schema_new (add_metadata) leaked into schema_old (item count)")
        if (allocated(error)) return

        ! And the reverse: mutating the original after the copy must not leak into schema_new.
        call schema_old%set_column_unavailable("idlong")
        call check(error, schema_new%cinfo%col(idx_idlong)%is_set, &
            "mutating schema_old after the copy leaked into schema_new (is_set)")
    end subroutine test_schema_deep_copy_independence

    !> maml%add_col_qc builds a read-time qc-maml incrementally from compact
    !> "col, min, max, miss" strings: positional fields, empty tokens skipped,
    !> the returned col_name, and the exact generated lines (fields: header
    !> created once, quoted min/max, name-only entry with no qc: block).
    subroutine test_add_col_qc_builds_maml(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: cn

        call maml%add_col_qc("ra, >0, <=360, Null", cn)
        call check(error, cn == "ra", "add_col_qc did not return the parsed column name")
        if (allocated(error)) return

        call maml%add_col_qc("dec, , <=90,", cn)   ! max only
        call check(error, cn == "dec", "add_col_qc did not return 'dec'")
        if (allocated(error)) return

        call maml%add_col_qc("mag, 5", cn)         ! bare numeric min
        if (allocated(error)) return
        call maml%add_col_qc("flag", cn)           ! name only, no qc: block

        ! Exact expected lines (order of appends preserved). The name-only
        ! "flag" entry adds just a "- name:" line, with no qc: block.
        call check(error, size(maml%lines) == 13, "add_col_qc produced an unexpected number of lines")
        if (allocated(error)) return
        call check(error, &
            trim(maml%lines(1))  == "fields:"           .and. &
            trim(maml%lines(2))  == "- name: ra"        .and. &
            trim(maml%lines(3))  == "  qc:"             .and. &
            trim(maml%lines(4))  == "    min: '>0'"     .and. &
            trim(maml%lines(5))  == "    max: '<=360'"  .and. &
            trim(maml%lines(6))  == "    miss: Null"    .and. &
            trim(maml%lines(7))  == "- name: dec"       .and. &
            trim(maml%lines(8))  == "  qc:"             .and. &
            trim(maml%lines(9))  == "    max: '<=90'"   .and. &
            trim(maml%lines(10)) == "- name: mag"       .and. &
            trim(maml%lines(11)) == "  qc:"             .and. &
            trim(maml%lines(12)) == "    min: '5'"      .and. &
            trim(maml%lines(13)) == "- name: flag", &
            "add_col_qc generated unexpected qc-maml lines")
    end subroutine test_add_col_qc_builds_maml

    !> A qc-maml built purely with add_col_qc is accepted by parquet_open_reader
    !> and drives the read-side qc checks: ra in the list_vector fixture is
    !> [1.5,2.5,3.5,4.5], within [>=0, <=10], so no violation occurs and the
    !> column reads back correctly. Also exercises the schema-level
    !> set_col_qc wrapper (schema_set_col_qc in src/parquet_metadata.f90, a
    !> thin forward to maml%set_col_qc) -- declared here for a column absent
    !> from the fixture, which is fine (a qc-maml may declare columns the
    !> file doesn't have; see test_qc_column_not_in_file in test_writing.f90).
    subroutine test_add_col_qc_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_reader) :: reader
        character(len=:), allocatable :: cn, cn2
        integer :: nrows
        real(real64), allocatable :: ra(:)

        call schema%add_col_qc("ra, >=0, <=10", cn)

        cn2 = "extra_dummy, >=0"
        call schema%set_col_qc(cn2)
        call check(error, cn2 == "extra_dummy", "schema%set_col_qc did not return the parsed column name")
        if (allocated(error)) return

        call parquet_open_reader(reader, "test/fixtures/list_vector.parquet", schema=schema)
        call parquet_get_nrows(reader, nrows)
        allocate(ra(nrows))
        call parquet_read_column(reader, cn, ra)
        call parquet_close_reader(reader)

        call check(error, nrows == 4 .and. abs(ra(1) - 1.5_real64) < 1.0e-12_real64 .and. &
            abs(ra(4) - 4.5_real64) < 1.0e-12_real64, &
            "add_col_qc-built qc-maml did not read back the ra column correctly")
    end subroutine test_add_col_qc_roundtrip

    !> An empty (or all-blank) qc_input is an explicit no-op: col_name comes
    !> back empty and the maml's lines are left untouched. A subsequent real
    !> add_col_qc call on the same maml still works and creates the fields:
    !> header itself (i.e. the no-op left nothing half-initialized).
    subroutine test_add_col_qc_empty_input_is_noop(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: cn

        call maml%add_col_qc("", cn)
        call check(error, len(cn) == 0, "add_col_qc('') should return an empty col_name")
        if (allocated(error)) return
        call check(error, .not. allocated(maml%lines), "add_col_qc('') should not add any lines")
        if (allocated(error)) return

        ! blank (whitespace-only) input is treated the same way.
        call maml%add_col_qc("   ", cn)
        call check(error, len(cn) == 0 .and. .not. allocated(maml%lines), &
            "add_col_qc('   ') should be a no-op too")
        if (allocated(error)) return

        ! a real call afterwards behaves normally and builds from scratch.
        call maml%add_col_qc("ra, >0", cn)
        call check(error, cn == "ra" .and. allocated(maml%lines) .and. size(maml%lines) == 4 .and. &
            trim(maml%lines(1)) == "fields:" .and. trim(maml%lines(2)) == "- name: ra", &
            "a real add_col_qc after a no-op did not build correctly")
    end subroutine test_add_col_qc_empty_input_is_noop

    !> set_col_qc is the in-place form of add_col_qc: it appends the same entry
    !> and writes the column name back into its own (intent(inout)) argument,
    !> so a caller reuses one variable (call maml%set_col_qc(col)) -- which
    !> add_col_qc's two-argument form cannot do (that would alias an
    !> intent(out) argument with qc_input). Also confirms the subroutine's
    !> col_name argument is now optional.
    subroutine test_set_col_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml, m2
        character(len=:), allocatable :: s, r

        ! subroutine form with col_name omitted (now optional) still adds.
        call maml%add_col_qc("ra, >0, <=360")

        ! in-place form: same-variable assignment, exact length.
        s = 'id_galaxy , 0,'
        call maml%set_col_qc(s)
        call check(error, s == "id_galaxy" .and. len(s) == 9, &
            "set_col_qc did not return the exact in-place column name")
        if (allocated(error)) return

        ! both entries were appended, in order.
        call check(error, size(maml%lines) == 8 .and. &
            trim(maml%lines(1)) == "fields:"          .and. &
            trim(maml%lines(2)) == "- name: ra"       .and. &
            trim(maml%lines(6)) == "- name: id_galaxy" .and. &
            trim(maml%lines(7)) == "  qc:"            .and. &
            trim(maml%lines(8)) == "    min: '0'", &
            "set_col_qc / optional-col_name add_col_qc produced unexpected lines")
        if (allocated(error)) return

        ! an empty input is a no-op for the in-place form too (returns "").
        r = ""
        call m2%set_col_qc(r)
        call check(error, len(r) == 0 .and. .not. allocated(m2%lines), &
            "set_col_qc('') should be a no-op returning an empty string")
    end subroutine test_set_col_qc

    !> schema%init builds a from-scratch MAML's top-level scalar keys: table:
    !> is unconditional, every other optional argument only emits its line
    !> when actually supplied, in the fixed order schema_init writes them.
    subroutine test_schema_init_builds_top_level_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="input_table", survey="The Big Survey", dataset="ds", &
            version="1.0", date="2026-01-01", author="Dave Smith", description="An example", &
            license="Copyright [Private]", maml_version="1.2")

        call check(error, size(schema%maml%lines) == 9, "schema%init produced an unexpected number of lines")
        if (allocated(error)) return

        call check(error, &
            trim(schema%maml%lines(1)) == "table: input_table"           .and. &
            trim(schema%maml%lines(2)) == "survey: The Big Survey"       .and. &
            trim(schema%maml%lines(3)) == "dataset: ds"                  .and. &
            trim(schema%maml%lines(4)) == "version: 1.0"                 .and. &
            trim(schema%maml%lines(5)) == "date: 2026-01-01"             .and. &
            trim(schema%maml%lines(6)) == "author: Dave Smith"           .and. &
            trim(schema%maml%lines(7)) == "description: An example"      .and. &
            trim(schema%maml%lines(8)) == "license: Copyright [Private]" .and. &
            trim(schema%maml%lines(9)) == "MAML_version: 1.2", &
            "schema%init generated unexpected top-level lines")
    end subroutine test_schema_init_builds_top_level_lines

    !> parquet_schema(...) is the structure-constructor alternative to
    !> declaring a schema and calling %init separately -- it must produce
    !> the exact same MAML lines as schema%init for the same arguments.
    subroutine test_schema_constructor_matches_init(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema_via_init
        type(parquet_schema) :: schema_via_constructor

        call schema_via_init%init(table="input_table", survey="The Big Survey", dataset="ds", &
            version="1.0", date="2026-01-01", author="Dave Smith", description="An example", &
            license="Copyright [Private]", maml_version="1.2")

        schema_via_constructor = parquet_schema(table="input_table", survey="The Big Survey", dataset="ds", &
            version="1.0", date="2026-01-01", author="Dave Smith", description="An example", &
            license="Copyright [Private]", maml_version="1.2")

        call check(error, size(schema_via_constructor%maml%lines) == size(schema_via_init%maml%lines), &
            "parquet_schema(...) produced a different number of lines than schema%init")
        if (allocated(error)) return

        call check(error, all(schema_via_constructor%maml%lines(:) == schema_via_init%maml%lines(:)), &
            "parquet_schema(...) produced different MAML lines than schema%init")
    end subroutine test_schema_constructor_matches_init

    !> schema%add_field builds one "- name:" entry per call, with only the
    !> optional sub-keys actually supplied, and a qc: block only when at
    !> least one of qc_min/qc_max/qc_miss is given (min/max quoted, exactly
    !> like %add_col_qc quotes its own min:/max: values). The "fields:"
    !> header is created once, on the first call.
    subroutine test_schema_add_field_builds_maml_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("id0", "int32", unit="unitless", info="ID field.", ucd="meta.id;meta.main")
        call schema%add_field("ra", "float64", unit="deg", info="Right ascension", &
            qc_min=">=0", qc_max="<360", qc_miss="Null")
        call schema%add_field("name", "string", array_size=18)
        call schema%add_field("arr", "float32", col_size=5)

        ! schema%maml%lines(1) is schema%init's own "table: t"; fields:
        ! content starts at line 2.
        call check(error, size(schema%maml%lines) == 21, "schema%add_field produced an unexpected number of lines")
        if (allocated(error)) return

        call check(error, &
            trim(schema%maml%lines(1))  == "table: t"                 .and. &
            trim(schema%maml%lines(2))  == "fields:"                  .and. &
            trim(schema%maml%lines(3))  == "- name: id0"              .and. &
            trim(schema%maml%lines(4))  == "  unit: unitless"         .and. &
            trim(schema%maml%lines(5))  == "  info: ID field."        .and. &
            trim(schema%maml%lines(6))  == "  ucd: meta.id;meta.main" .and. &
            trim(schema%maml%lines(7))  == "  data_type: int32"       .and. &
            trim(schema%maml%lines(8))  == "- name: ra"               .and. &
            trim(schema%maml%lines(9))  == "  unit: deg"              .and. &
            trim(schema%maml%lines(10)) == "  info: Right ascension"  .and. &
            trim(schema%maml%lines(11)) == "  data_type: float64"     .and. &
            trim(schema%maml%lines(12)) == "  qc:"                    .and. &
            trim(schema%maml%lines(13)) == "    min: '>=0'"           .and. &
            trim(schema%maml%lines(14)) == "    max: '<360'"          .and. &
            trim(schema%maml%lines(15)) == "    miss: Null"           .and. &
            trim(schema%maml%lines(16)) == "- name: name"             .and. &
            trim(schema%maml%lines(17)) == "  data_type: string"      .and. &
            trim(schema%maml%lines(18)) == "  array_size: 18"         .and. &
            trim(schema%maml%lines(19)) == "- name: arr"              .and. &
            trim(schema%maml%lines(20)) == "  data_type: float32"     .and. &
            trim(schema%maml%lines(21)) == "  col_size: 5", &
            "schema%add_field generated unexpected fields: lines")
    end subroutine test_schema_add_field_builds_maml_lines

    !> The lines schema%init/add_field build are still just text: this checks
    !> parquet_parse_maml turns them into the same structured schema%cinfo/
    !> schema%metadata a MAML loaded from disk would produce.
    subroutine test_schema_init_add_field_parses_correctly(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: idx, i
        logical :: found_table

        call schema%init(table="input_table", author="Dave Smith")
        ! "count" (not "unitless"): parquet_parse_maml_lines normalizes a
        ! unit: of "unitless" down to an empty string (a documented sentinel
        ! for "no unit"), so a real unit is needed here to check that %unit
        ! itself round-trips correctly.
        call schema%add_field("id0", "int32", unit="count", info="ID field.")
        call schema%add_field("ra", "float64", qc_min=">=0", qc_max="<=360")
        call schema%add_field("flag", "boolean")

        call parquet_parse_maml(schema)

        call check(error, size(schema%cinfo%col) == 3, "expected 3 parsed fields")
        if (allocated(error)) return

        idx = schema%get_column_index("id0")
        call check(error, trim(schema%cinfo%col(idx)%data_type) == "int32" .and. &
            trim(schema%cinfo%col(idx)%unit) == "count" .and. &
            trim(schema%cinfo%col(idx)%info) == "ID field.", &
            "'id0' field attributes were not parsed as expected")
        if (allocated(error)) return

        idx = schema%get_column_index("ra")
        ! qc_max="<=360" (rather than the more common "<360") specifically
        ! exercises validate_qc_bound's "<=" operator-recognition branch in
        ! src/parquet_metadata.f90, which no other test happened to reach.
        call check(error, trim(schema%cinfo%col(idx)%data_type) == "float64" .and. &
            schema%cinfo%col(idx)%has_qc_min .and. trim(schema%cinfo%col(idx)%qc_min_raw) == "0" .and. &
            schema%cinfo%col(idx)%has_qc_max .and. trim(schema%cinfo%col(idx)%qc_max_raw) == "360" .and. &
            trim(schema%cinfo%col(idx)%qc_max_op) == "<=", &
            "'ra' field's qc: min:/max: were not parsed as expected")
        if (allocated(error)) return

        idx = schema%get_column_index("flag")
        call check(error, trim(schema%cinfo%col(idx)%data_type) == "boolean", &
            "'flag' field data_type was not parsed as expected")
        if (allocated(error)) return

        found_table = .false.
        do i = 1, size(schema%metadata%items)
            if (trim(schema%metadata%items(i)%key) == "table" .and. &
                trim(schema%metadata%items(i)%value) == "input_table") found_table = .true.
        end do
        call check(error, found_table, "expected schema%init's table: key to be present in schema%metadata")
    end subroutine test_schema_init_add_field_parses_correctly

    !> End-to-end: a schema built entirely in memory via schema%init/add_field
    !> (no MAML file on disk at all) must write and read back a real parquet
    !> file exactly like a schema loaded from a .maml file would.
    subroutine test_schema_init_add_field_write_read_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        real(real64) :: ra(3) = [10.0_real64, 20.0_real64, 30.0_real64]
        integer(int32) :: id0_back(3)
        real(real64) :: ra_back(3)
        integer :: nrows
        character(len=*), parameter :: out_file = "test_run/schema_init_add_field_roundtrip.parquet"

        call schema%init(table="input_table")
        call schema%add_field("id0", "int32")
        call schema%add_field("ra", "float64")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 3, "expected 3 rows after the round-trip")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        call parquet_read_column(reader, "id0", id0_back)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)

        call check(error, all(id0_back == id0), "'id0' column did not round-trip correctly")
        if (allocated(error)) return
        call check(error, all(abs(ra_back - ra) < 1.0e-9_real64), "'ra' column did not round-trip correctly")
    end subroutine test_schema_init_add_field_write_read_roundtrip

    !> A qc_min/qc_max with no operator prefix (a bare value) is accepted
    !> as-is, the same rule %add_col_qc applies to its own min:/max: fields.
    subroutine test_schema_add_field_bare_qc_bound(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("mag", "float32", qc_min="5")

        call check(error, size(schema%maml%lines) == 6 .and. &
            trim(schema%maml%lines(1)) == "table: t" .and. &
            trim(schema%maml%lines(2)) == "fields:" .and. &
            trim(schema%maml%lines(3)) == "- name: mag" .and. &
            trim(schema%maml%lines(4)) == "  data_type: float32" .and. &
            trim(schema%maml%lines(5)) == "  qc:" .and. &
            trim(schema%maml%lines(6)) == "    min: '5'", &
            "a bare (operator-less) qc_min value was not accepted/emitted as expected")
    end subroutine test_schema_add_field_bare_qc_bound

    subroutine test_schema_add_field_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_before_init", expect_abort=.true., &
            failure_message="schema%add_field before schema%init was expected to error stop")
    end subroutine test_schema_add_field_before_init_aborts

    subroutine test_schema_init_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_twice", expect_abort=.true., &
            failure_message="calling schema%init twice was expected to error stop")
    end subroutine test_schema_init_twice_aborts

    subroutine test_schema_init_after_maml_parse_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_after_maml_parse", expect_abort=.true., &
            failure_message="schema%init on a MAML-parsed schema (without force) was expected to error stop")
    end subroutine test_schema_init_after_maml_parse_aborts

    subroutine test_parse_maml_file_after_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "parse_maml_file_after_init", expect_abort=.true., &
            failure_message="loading a .maml file into an already-initialized schema was expected to error stop")
    end subroutine test_parse_maml_file_after_init_aborts

    !> schema%init(force=.true.) on a schema already populated via a MAML parse must still
    !! give a genuine clean slate -- the same guarantee already covered for a schema previously
    !! built via %init (test_schema_init_force_resets_and_reuses); this checks it also holds
    !! when the earlier content came from parquet_parse_maml instead.
    subroutine test_schema_init_force_after_maml_parse_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call check(error, size(schema%maml%lines) > 1, &
            "sanity check: parsing schemas/maml_example.maml should populate many maml lines")
        if (allocated(error)) return

        call schema%init(table="after_force", force=.true.)

        call check(error, size(schema%maml%lines) == 1 .and. trim(schema%maml%lines(1)) == "table: after_force", &
            "force=.true. after a MAML parse should discard all prior content, keeping only the new table: line")
        if (allocated(error)) return
        call check(error, .not. allocated(schema%cinfo%col), &
            "force=.true. after a MAML parse should deallocate %cinfo%col")
    end subroutine test_schema_init_force_after_maml_parse_ok

    !> schema%clear() is the documented workaround for reusing a schema variable across two
    !! different parquet_parse_maml(filename, ...) calls, now that doing so directly aborts.
    subroutine test_parse_maml_file_after_clear_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="t1")
        call schema%add_field("x", "int32")
        call schema%clear()

        call parquet_parse_maml("schemas/maml_example.maml", schema)

        call check(error, schema%is_init() .and. allocated(schema%cinfo%col), &
            "loading a .maml file after schema%clear() should succeed normally")
    end subroutine test_parse_maml_file_after_clear_ok

    subroutine test_schema_init_empty_table_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_empty_table", expect_abort=.true., &
            failure_message="schema%init with an empty table was expected to error stop")
    end subroutine test_schema_init_empty_table_aborts

    subroutine test_schema_add_field_empty_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_empty_name", expect_abort=.true., &
            failure_message="schema%add_field with an empty name was expected to error stop")
    end subroutine test_schema_add_field_empty_name_aborts

    !> Also checks the error names which schema/maml it came from: schema%init
    !> (table="t" in this scenario) always gives an in-memory schema the name
    !> "internal:t" -- see maml_name_suffix in src/parquet_metadata.f90.
    subroutine test_schema_add_field_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_duplicate_name", expect_abort=.true., &
            failure_message="schema%add_field with a duplicate field name was expected to error stop", &
            required_stderr="parquet_schema%add_field: duplicate field name 'ra' (maml: internal:t)")
    end subroutine test_schema_add_field_duplicate_name_aborts

    subroutine test_schema_add_field_invalid_data_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_invalid_data_type", expect_abort=.true., &
            failure_message="schema%add_field with an invalid data_type was expected to error stop")
    end subroutine test_schema_add_field_invalid_data_type_aborts

    subroutine test_schema_add_field_qc_min_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_min_reversed_operator", expect_abort=.true., &
            failure_message="schema%add_field with a reversed qc_min operator was expected to error stop")
    end subroutine test_schema_add_field_qc_min_reversed_operator_aborts

    subroutine test_schema_add_field_qc_max_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_max_reversed_operator", expect_abort=.true., &
            failure_message="schema%add_field with a reversed qc_max operator was expected to error stop")
    end subroutine test_schema_add_field_qc_max_reversed_operator_aborts

    subroutine test_schema_add_field_qc_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_operator_without_value", expect_abort=.true., &
            failure_message="schema%add_field with a qc operator but no value was expected to error stop")
    end subroutine test_schema_add_field_qc_operator_without_value_aborts

    subroutine test_schema_add_field_bad_qc_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_bad_qc_miss_value", expect_abort=.true., &
            failure_message="schema%add_field with an invalid qc_miss value was expected to error stop")
    end subroutine test_schema_add_field_bad_qc_miss_value_aborts
    !
    ! ------------------------------------------------------------------------------
    ! date/time/timestamp (parquet_temporal) MAML token validation
    ! ------------------------------------------------------------------------------
    !
    !> schema%add_field accepts every temporal token shape (bare date/time/timestamp, an
    !> explicit unit, and timestamp's ",utc" suffix), and parquet_validate_maml accepts the
    !> resulting schema -- see parquet_parse_temporal_type in parquet_metadata.f90.
    subroutine test_schema_add_field_temporal_tokens_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("day", "date")
        call schema%add_field("clock", "time")
        call schema%add_field("clock_ms", "time[ms]")
        call schema%add_field("ev", "timestamp")
        call schema%add_field("ev_ns", "timestamp[ns]")
        call schema%add_field("ev_utc", "timestamp[us,utc]")
        call parquet_parse_maml(schema)

        ! Should not error stop: every token above is well-formed.
        call parquet_validate_maml(schema%maml)

        call check(error, schema%cinfo%col(1)%data_type == "date", "field 1 data_type should be 'date'")
        if (allocated(error)) return
        call check(error, schema%cinfo%col(4)%data_type == "timestamp" .and. schema%cinfo%col(4)%time_unit == 3, &
            "bare 'timestamp' should default to time_unit 3 (microseconds)")
        if (allocated(error)) return
        call check(error, schema%cinfo%col(6)%time_unit == 3 .and. schema%cinfo%col(6)%is_utc, &
            "'timestamp[us,utc]' should resolve to time_unit 3 and is_utc .true.")
    end subroutine test_schema_add_field_temporal_tokens_ok

    !> "date" takes no unit -- a bracketed suffix on it is rejected the same way any other
    !> malformed data_type token is.
    subroutine test_schema_add_field_date_with_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_date_with_unit", expect_abort=.true., &
            failure_message="schema%add_field with 'date[us]' was expected to error stop", &
            required_stderr="invalid data_type 'date[us]'")
    end subroutine test_schema_add_field_date_with_unit_aborts

    !> qc: min:/max: on a date/time/timestamp field is deliberately unsupported (see
    !> feature_temporal.md); parquet_validate_maml rejects it with a clear message rather than
    !> silently ignoring the declared bound.
    subroutine test_validate_qc_on_temporal_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_qc_on_temporal_column", expect_abort=.true., &
            failure_message="qc: on a timestamp field was expected to error stop", &
            required_stderr="declares qc:, which is not supported for a timestamp column")
    end subroutine test_validate_qc_on_temporal_column_aborts

    !> Regression test: an explicit seconds unit ("timestamp[s]") must be rejected at add_field
    !> time, not silently accepted and then silently downgraded to milliseconds by Arrow's
    !> Parquet writer on write (Parquet's physical format has no seconds-resolution TIME/
    !> TIMESTAMP encoding at all) -- see apply_temporal_unit_token's own comment in
    !> parquet_metadata.f90 for how this was caught.
    subroutine test_schema_add_field_seconds_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_seconds_unit_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with 'timestamp[s]' was expected to error stop", &
            required_stderr="invalid data_type 'timestamp[s]'")
    end subroutine test_schema_add_field_seconds_unit_aborts

    !> "time" has no timezone concept (no date part to be UTC-adjusted relative to) -- only
    !> "timestamp" accepts a ",utc" suffix.
    subroutine test_schema_add_field_time_utc_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_time_utc_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with 'time[ms,utc]' was expected to error stop", &
            required_stderr="invalid data_type 'time[ms,utc]'")
    end subroutine test_schema_add_field_time_utc_aborts

    subroutine test_schema_add_field_unclosed_bracket_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_unclosed_bracket_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with an unclosed unit bracket ('time[ms') was expected to error stop", &
            required_stderr="invalid data_type 'time[ms'")
    end subroutine test_schema_add_field_unclosed_bracket_aborts

    subroutine test_schema_is_init_reflects_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call check(error, .not. schema%is_init(), "a freshly declared schema should report is_init() == .false.")
        if (allocated(error)) return

        call schema%init(table="is_init_test")
        call check(error, schema%is_init(), "schema%is_init() should be .true. immediately after %init")
    end subroutine test_schema_is_init_reflects_state

    !> A schema loaded via parquet_parse_maml (from a file or an already-populated object) never
    !! calls %init at all, but is fully valid and ready to use -- is_init() must report .true. for
    !! it too, not just for the in-code %init/%add_field builder path.
    subroutine test_schema_is_init_true_after_maml_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call check(error, schema%is_init(), &
            "schema%is_init() should be .true. for a schema loaded via parquet_parse_maml, " // &
            "even though %init was never called on it")
    end subroutine test_schema_is_init_true_after_maml_parse

    !> force=.true. on a schema that was never initialized must behave exactly like a plain
    !! %init -- there is nothing to reset, and it must not error stop just because force was given.
    subroutine test_schema_init_force_never_initialized_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="force_fresh", force=.true.)
        call check(error, schema%is_init(), &
            "schema%init(force=.true.) on a fresh schema should succeed and initialize it")
        if (allocated(error)) return

        call check(error, size(schema%maml%lines) == 1 .and. trim(schema%maml%lines(1)) == "table: force_fresh", &
            "schema%init(force=.true.) on a fresh schema should build the same single table: line as a plain init")
    end subroutine test_schema_init_force_never_initialized_ok

    !> force=.true. on an already-initialized schema with fields/metadata already added must
    !! discard all of that (maml lines, cinfo) and rebuild from scratch with the new table/field
    !! arguments -- verified both structurally (maml%lines/cinfo%col contents) and end-to-end
    !! (the old field is genuinely gone, not just hidden, by writing/reading a real file).
    subroutine test_schema_init_force_resets_and_reuses(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: new_col(2) = [10_int32, 20_int32]
        integer(int32) :: new_col_back(2)
        integer :: nrows
        character(len=*), parameter :: out_file = "test_run/schema_init_force_reset.parquet"

        call schema%init(table="before_reset")
        call schema%add_field("old_field", "int32")
        call parquet_parse_maml(schema)

        call check(error, size(schema%cinfo%col) == 1, "sanity check: 'old_field' should be parsed before the reset")
        if (allocated(error)) return

        call schema%init(table="after_reset", force=.true.)

        call check(error, schema%is_init(), "schema%init(force=.true.) should leave the schema initialized")
        if (allocated(error)) return
        call check(error, size(schema%maml%lines) == 1 .and. trim(schema%maml%lines(1)) == "table: after_reset", &
            "force=.true. should discard the old table: line and any fields, keeping only the new table: line")
        if (allocated(error)) return
        call check(error, .not. allocated(schema%cinfo%col), &
            "force=.true. should deallocate %cinfo%col from the previous parquet_parse_maml call")
        if (allocated(error)) return

        ! End-to-end: the old field must be genuinely gone -- a new field added after the
        ! reset should be the only declared column, and should round-trip normally.
        call schema%add_field("new_field", "int32")
        call parquet_parse_maml(schema)

        call check(error, size(schema%cinfo%col) == 1 .and. trim(schema%cinfo%col(1)%name) == "new_field", &
            "after the reset, only 'new_field' should be a declared column")
        if (allocated(error)) return

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "new_field", new_col)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, "expected 2 rows after writing through the force-reset schema")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "new_field", new_col_back)
        call parquet_close_reader(reader)

        call check(error, all(new_col_back == new_col), "'new_field' did not round-trip correctly after force reset")
    end subroutine test_schema_init_force_resets_and_reuses

    !> %clear resets the entire schema back to its pristine, just-declared state -- is_init()
    !! becomes .false. again, and %maml/%cinfo/%metadata are all back to their defaults.
    subroutine test_schema_clear_resets_to_pristine(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="clear_test")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        call schema%add_metadata("k", 1_int32)

        call check(error, schema%is_init(), "sanity check: schema should be initialized before %clear")
        if (allocated(error)) return

        call schema%clear()

        call check(error, .not. schema%is_init(), "%clear should leave the schema uninitialized (is_init() == .false.)")
        if (allocated(error)) return
        call check(error, .not. allocated(schema%cinfo%col), "%clear should deallocate %cinfo%col")
        if (allocated(error)) return
        call check(error, .not. allocated(schema%metadata%items), "%clear should deallocate %metadata%items")
        if (allocated(error)) return
        call check(error, .not. allocated(schema%maml%lines), "%clear should deallocate %maml%lines")
    end subroutine test_schema_clear_resets_to_pristine

    subroutine test_schema_clear_noop_on_blank_schema(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%clear()
        call check(error, .not. schema%is_init(), "%clear on a never-initialized schema should leave it uninitialized")
    end subroutine test_schema_clear_noop_on_blank_schema

    !> After %clear, the schema variable can be reused via a normal (non-force) %init call.
    subroutine test_schema_clear_then_reinit_reusable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="before_clear")
        call schema%add_field("old_field", "int32")
        call schema%clear()

        call schema%init(table="after_clear")
        call schema%add_field("new_field", "int32")
        call parquet_parse_maml(schema)

        call check(error, size(schema%cinfo%col) == 1 .and. trim(schema%cinfo%col(1)%name) == "new_field", &
            "after %clear + a plain %init, only 'new_field' should be a declared column")
    end subroutine test_schema_clear_then_reinit_reusable

    !> col_size: auto/array_size: auto (case-insensitive) parse cleanly (parquet_validate_maml
    !> does not reject them) to parquet_size_auto -- a genuinely unresolved, not-yet-usable-for-
    !> writing state, distinct from the "not specified" blank-value default of 1.
    subroutine test_parse_col_size_array_size_auto_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "col_size_array_size_auto.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: col_size_array_size_auto_table", &
            "fields:", &
            "- name: vec", &
            "  data_type: int32", &
            "  col_size: AUTO", &
            "- name: txt", &
            "  data_type: string", &
            "  array_size: Auto" ]

        call parquet_parse_maml(schema)

        call check(error, schema%cinfo%col(1)%col_size == parquet_size_auto, &
            "col_size: AUTO did not parse to parquet_size_auto")
        if (allocated(error)) return
        call check(error, schema%cinfo%col(2)%array_size == parquet_size_auto, &
            "array_size: Auto did not parse to parquet_size_auto")
    end subroutine test_parse_col_size_array_size_auto_ok

    subroutine test_col_size_malformed_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "col_size_malformed_value", expect_abort=.true., &
            failure_message="a malformed col_size value should abort", &
            required_stderr="has an invalid col_size")
    end subroutine test_col_size_malformed_value_aborts

    subroutine test_array_size_malformed_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "array_size_malformed_value", expect_abort=.true., &
            failure_message="a malformed array_size value should abort", &
            required_stderr="has an invalid array_size")
    end subroutine test_array_size_malformed_value_aborts

    subroutine test_array_size_auto_on_non_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "array_size_auto_on_non_string", expect_abort=.true., &
            failure_message="array_size: auto on a non-string column should abort", &
            required_stderr="only applies to string columns")
    end subroutine test_array_size_auto_on_non_string_aborts

    !> schema%set_col_size resolves an "auto" column's col_size, and that resolution is a plain
    !> in-memory schema mutation -- no write/parse involved, so this runs in-process.
    subroutine test_set_col_size_resolves_auto(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "set_col_size_resolves_auto.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: set_col_size_resolves_auto_table", &
            "fields:", &
            "- name: vec", &
            "  data_type: int32", &
            "  col_size: auto" ]
        call parquet_parse_maml(schema)

        call schema%set_col_size("vec", 4)
        call check(error, schema%cinfo%col(1)%col_size == 4, &
            "set_col_size did not resolve the 'auto' col_size to the requested value")
    end subroutine test_set_col_size_resolves_auto

    subroutine test_set_col_size_non_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_col_size_non_positive", expect_abort=.true., &
            failure_message="set_col_size with a non-positive value should abort", &
            required_stderr="must be a positive integer")
    end subroutine test_set_col_size_non_positive_aborts

    subroutine test_set_col_size_already_resolved_no_force_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_col_size_already_resolved_no_force", &
            expect_abort=.true., &
            failure_message="set_col_size on an already-resolved column should abort without force=.true.", &
            required_stderr="pass force=.true. to override")
    end subroutine test_set_col_size_already_resolved_no_force_aborts

    !> force=.true. is the one path that lets set_col_size override an already-resolved (not
    !> "auto") col_size -- in-process, since nothing here aborts.
    subroutine test_set_col_size_force_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="set_col_size_force_table")
        call schema%add_field("vec", "int32", col_size=3)
        call parquet_parse_maml(schema)

        call schema%set_col_size("vec", 7, force=.true.)
        call check(error, schema%cinfo%col(1)%col_size == 7, &
            "set_col_size(force=.true.) did not override the already-resolved col_size")
    end subroutine test_set_col_size_force_overrides

    !> schema%set_array_size resolves an "auto" string column's array_size.
    subroutine test_set_array_size_resolves_auto(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "set_array_size_resolves_auto.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: set_array_size_resolves_auto_table", &
            "fields:", &
            "- name: txt", &
            "  data_type: string", &
            "  array_size: auto" ]
        call parquet_parse_maml(schema)

        call schema%set_array_size("txt", 20)
        call check(error, schema%cinfo%col(1)%array_size == 20, &
            "set_array_size did not resolve the 'auto' array_size to the requested value")
    end subroutine test_set_array_size_resolves_auto

    subroutine test_set_array_size_non_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_non_string_column", expect_abort=.true., &
            failure_message="set_array_size on a non-string column should abort", &
            required_stderr="not a string column")
    end subroutine test_set_array_size_non_string_column_aborts

    subroutine test_set_array_size_non_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_non_positive", expect_abort=.true., &
            failure_message="set_array_size with a non-positive value should abort", &
            required_stderr="must be a positive integer")
    end subroutine test_set_array_size_non_positive_aborts

    subroutine test_set_array_size_already_resolved_no_force_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_already_resolved_no_force", &
            expect_abort=.true., &
            failure_message="set_array_size on an already-resolved column should abort without force=.true.", &
            required_stderr="pass force=.true. to override")
    end subroutine test_set_array_size_already_resolved_no_force_aborts

    !> force=.true. is the one path that lets set_array_size override an already-resolved (not
    !> "auto") array_size too -- in-process, since nothing here aborts.
    subroutine test_set_array_size_force_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_force_table")
        call schema%add_field("txt", "string", array_size=8)
        call parquet_parse_maml(schema)

        call schema%set_array_size("txt", 12, force=.true.)
        call check(error, schema%cinfo%col(1)%array_size == 12, &
            "set_array_size(force=.true.) did not override the already-resolved array_size")
    end subroutine test_set_array_size_force_overrides
    !
end module test_maml
