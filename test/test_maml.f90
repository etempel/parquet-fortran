!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for MAML parsing/validation and parquet_column_info helpers.
!> "Happy path" only: most negative cases in this module's functions trigger `error stop`, which
!> aborts the whole test process, so they run as subprocess scenarios
!> (test/error_scenarios.f90) instead -- the ones specific to MAML in `test_maml_errors.f90`,
!> the rest in `test_errors.f90`. This file therefore spawns nothing and imports no scenario
!> helper; see `feature_tests.md` section 8.3 for why the two halves live apart.
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
    !
    implicit none
    private
    public :: collect_tests_parquet_maml
    !
contains
    !
    subroutine collect_tests_parquet_maml(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        type(unittest_type), allocatable :: p1(:)
        !
        p1 = [ &
            new_unittest("validate a well-formed MAML file", test_validate_maml_ok), &
            new_unittest("validate maml_example2.maml (extra top-level keys, depends:, extra:, " // &
                "list-form ucd:, keyarray:)", test_validate_maml_example2_ok), &
            new_unittest("validate maml_example2.maml by filename (parquet_validate_maml overload)", &
                test_validate_maml_by_filename_ok), &
            new_unittest("a fields: sub-key other than qc: with its own deeper-indented content is " // &
                "silently ignored (no declared nested schema)", test_validate_maml_unmatched_nested_ok), &
            new_unittest("keyarray:/DOIs:/depends: entries parse correctly with a bare dash and first-key " // &
                "variations", test_keyarray_dois_depends_key_variations), &
            new_unittest("a fields: header carrying trailing text still opens the field list", &
                test_fields_header_with_trailing_text), &
            new_unittest("a keyarray: key gets no .datatype companion (a MAML value is a string by design)", &
                test_maml_keyarray_key_gets_no_datatype), &
            new_unittest("a generic (non-comments/coauthors/keywords) top-level list section parses, " // &
                "skipping a malformed indented line", test_generic_list_section_and_malformed_line), &
            new_unittest("validate a user MAML that is a valid subset", test_validate_user_maml_ok), &
            new_unittest("col_map: renames a field to an internal name", test_validate_user_maml_col_map_ok), &
            new_unittest("load a MAML file from disk", test_load_maml_file), &
            new_unittest("a CRLF-terminated MAML file parses correctly (trailing char(13) stripped)", &
                test_load_maml_file_crlf), &
            new_unittest("load a qc-maml file from disk and use it via parquet_open_reader", &
                test_load_qc_maml_file), &
            new_unittest("get_column_index finds an existing column", test_get_column_index_found), &
            new_unittest("get_num_fields/get_field_name report all fields, unfiltered", &
                test_get_num_fields_and_field_name), &
            new_unittest("get_num_fields on a freshly declared (uninitialized) schema is 0", &
                test_get_num_fields_uninitialized), &
            new_unittest("set_unavailable/set_available toggle is_set", test_set_available_unavailable), &
            new_unittest("is_column_set reports a column's current is_set state", test_is_column_set), &
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
            new_unittest("a struct column accepts col_size: 1 and a positive array_size", &
                test_struct_col_size_one_accepted), &
            new_unittest("schema%init + add_field round-trips through parquet_parse_maml", &
                test_schema_init_add_field_parses_correctly), &
            new_unittest("schema%init + add_field writes/reads a real parquet file", &
                test_schema_init_add_field_write_read_roundtrip), &
            new_unittest("schema%add_field accepts a bare qc_min with no operator", &
                test_schema_add_field_bare_qc_bound), &
            new_unittest("schema%init(force=.true.) after a MAML parse gives a clean slate", &
                test_schema_init_force_after_maml_parse_ok), &
            new_unittest("loading a .maml file after schema%clear() succeeds", &
                test_parse_maml_file_after_clear_ok), &
            new_unittest("schema%get_field(name=) reads back a full field definition, incl. qc", &
                test_get_field_by_name), &
            new_unittest("schema%get_field(index=) reads back a full field definition by position", &
                test_get_field_by_index), &
            new_unittest("schema%add_field_from copies a field's full definition, incl. qc", &
                test_add_field_from_copies_field), &
            new_unittest("schema%add_field accepts date/time[unit]/timestamp[unit,utc] tokens", &
                test_schema_add_field_temporal_tokens_ok), &
            new_unittest("schema%is_init reflects state before/after schema%init", test_schema_is_init_reflects_state), &
            new_unittest("schema%is_init is .true. after a MAML parse, even without %init", &
                test_schema_is_init_true_after_maml_parse), &
            new_unittest("schema%is_parsed is .false. after %init alone and .true. after %add_field", &
                test_schema_is_parsed_false_after_init_alone), &
            new_unittest("schema%is_parsed is .true. after a MAML parse", &
                test_schema_is_parsed_true_after_maml_parse), &
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
            new_unittest("unit: unitless parses to no unit at all, in any capitalization", &
                test_unit_unitless_parses_to_empty), &
            new_unittest("a list-form ucd: joins its items with ';'", test_ucd_list_form_joins), &
            new_unittest("a MAML key is case-insensitive, section headers included", &
                test_maml_block_headers_case_insensitive), &
            new_unittest("schema%add_field(col_size=parquet_size_auto) writes 'col_size: auto', not a raw -1", &
                test_add_field_col_size_auto_writes_auto_token), &
            new_unittest("schema%add_field(array_size=parquet_size_auto) writes 'array_size: auto', not a raw -1", &
                test_add_field_array_size_auto_writes_auto_token), &
            new_unittest("set_col_size resolves an 'auto' column", test_set_col_size_resolves_auto), &
            new_unittest("set_col_size(force=.true.) overrides an already-resolved column", &
                test_set_col_size_force_overrides), &
            new_unittest("set_array_size resolves an 'auto' string column", test_set_array_size_resolves_auto), &
            new_unittest("set_array_size(force=.true.) overrides an already-resolved column", &
                test_set_array_size_force_overrides), &
            new_unittest("parquet_read_qc: %add stores entries verbatim and %remap renames the column", &
                test_read_qc_add_and_remap), &
            new_unittest("compose_read_qc: the MAML wins in full for any column with a qc: block", &
                test_compose_read_qc_maml_wins), &
            new_unittest("compose_read_qc: an EMPTY qc: block still wins (Nulls stay banned)", &
                test_compose_read_qc_empty_qc_block_wins), &
            new_unittest("compose_read_qc: a column the MAML only names takes the code's rule", &
                test_compose_read_qc_named_without_qc), &
            new_unittest("compose_read_qc: either source alone, and neither", &
                test_compose_read_qc_single_and_empty_sources), &
            new_unittest("compose_read_qc: the composed schema enforces qc on a real read", &
                test_compose_read_qc_enforced_on_read) &
            ]
        !
        testsuite = p1
    end subroutine collect_tests_parquet_maml

    subroutine test_validate_maml_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = get_parquet_maml("maml_example.maml")

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml(maml)

        call check(error, .true., &
            ".true.")
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

        call check(error, .true., &
            ".true.")
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

        call check(error, .true., &
            ".true.")
    end subroutine test_validate_maml_unmatched_nested_ok

    !> parquet_validate_maml is generic: it also accepts a filename directly
    !> (loading the file from disk internally), instead of requiring the
    !> caller to first call parquet_load_maml_file themselves.
    subroutine test_validate_maml_by_filename_ok(error)
        type(error_type), allocatable, intent(out) :: error

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml("schemas/maml_example2.maml")

        call check(error, .true., &
            ".true.")
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
    !> spells the fields: header "Fields:", which parquet_maml_key_matches
    !> accepts directly -- that helper lowercases each character before
    !> comparing, so the "fields:" test at the top of parquet_parse_maml_lines'
    !> non-field branch is case-INSENSITIVE. (It was a case-sensitive literal
    !> comparison once, and this comment used to say that "Fields:" therefore
    !> reached the generic key/value fallback below it. It does not, and has
    !> not since that helper took over; the fallback's own "fields" arm is
    !> reached by a header whose TRIMMED LENGTH differs from "fields:" instead,
    !> which is what test_fields_header_with_trailing_text covers.) No other
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
    !
    !> A `fields:` header with anything after the colon must still open the field list.
    !!
    !! **Two tests for "fields:" exist because the parser answers it in two places, and only one of
    !! them was reachable.** `parquet_maml_key_matches` demands the trimmed line be exactly as long
    !! as `"fields:"`, so it recognises `fields:` and `Fields:` and nothing else; a header carrying
    !! a trailing comment falls past it to the generic key/value branch further down, which splits
    !! on the colon, lowercases the key and tests it against `"fields"` there. That second arm had
    !! no fixture at all -- `test_keyarray_dois_depends_key_variations`'s doc-comment claimed its
    !! `Fields:` reached it, which stopped being true when the literal comparison became the
    !! case-insensitive helper, and nothing failed when it did.
    !!
    !! **The assertion is an equality against the bare form**, not merely "some fields parsed": the
    !! branch's whole job is to be indistinguishable from the header it tolerates, and a parse that
    !! produced the fields with, say, the comment text attached somewhere would satisfy a weaker
    !! check. The bare arm is the negative control -- if it ever stopped parsing, the equality would
    !! hold for the wrong reason, so its field count is asserted first.
    subroutine test_fields_header_with_trailing_text(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: bare, trailing
        integer :: i

        bare%maml%name = "fields_header_bare.maml"
        bare%maml%lines = [character(len=40) :: &
            "table: fields_header_table", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32", &
            "- name: ra", &
            "  data_type: float64" ]
        call parquet_parse_maml(bare)

        trailing%maml%name = "fields_header_trailing.maml"
        trailing%maml%lines = [character(len=40) :: &
            "table: fields_header_table", &
            "fields: # the column list", &
            "- name: id0", &
            "  data_type: int32", &
            "- name: ra", &
            "  data_type: float64" ]
        call parquet_parse_maml(trailing)

        call check(error, bare%get_num_fields() == 2, &
            "the bare fields: control did not parse two fields, so the equality below would be vacuous")
        if (allocated(error)) return
        call check(error, trailing%get_num_fields() == bare%get_num_fields(), &
            "a fields: header with a trailing comment did not open the field list -- the generic " // &
            "key/value branch's own fields arm is what has to catch it")
        if (allocated(error)) return
        do i = 1, bare%get_num_fields()
            call check(error, trim(trailing%cinfo%col(i)%name) == trim(bare%cinfo%col(i)%name) .and. &
                trim(trailing%cinfo%col(i)%data_type) == trim(bare%cinfo%col(i)%data_type), &
                "a fields: header with a trailing comment parsed a different field list than the bare form")
            if (allocated(error)) return
        end do
        ! The trailing text itself must be discarded rather than becoming a table-metadata entry:
        ! the branch sets in_fields and cycles, so nothing after the colon is ever stored.
        do i = 1, size(trailing%metadata%items)
            call check(error, trim(trailing%metadata%items(i)%key) /= "fields", &
                "the fields: header was stored as a metadata entry instead of opening the field list")
            if (allocated(error)) return
        end do
    end subroutine test_fields_header_with_trailing_text
    !

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

    !> Writes `lines` to `path` with an explicit trailing CRLF (char(13)//char(10)) on every line,
    !! via unformatted stream I/O so the bytes on disk are exact -- a plain formatted `write`
    !! always emits the platform's own line terminator (LF on Unix), so this is the only way to
    !! construct a genuinely CRLF-terminated fixture at runtime without checking in a CRLF file
    !! (which risks being silently normalized by .gitattributes/editor settings).
    subroutine write_text_file_crlf(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        integer :: unit, i

        open(newunit=unit, file=path, status="replace", action="write", access="stream", form="unformatted")
        do i = 1, size(lines)
            write(unit) trim(lines(i)) // char(13) // char(10)
        end do
        close(unit)
    end subroutine write_text_file_crlf

    !> Regression test for feature_doc.md point 7's F2 finding: a `.maml` file authored/edited on
    !! Windows retains a trailing `\r` on every line after a Unix `read(unit,'(A)')`, and `trim()`
    !! does not strip it (`char(13)` is not a blank) -- so `data_type: int32\r` failed to match
    !! `valid_maml_data_types` and the reader printed a misleading "invalid data_type" error naming
    !! what looked like a perfectly correct value. Fixed in
    !! parquet_metadata_maml.f90's parquet_read_maml_source_lines, which now strips a trailing
    !! char(13) from every line read. This constructs a CRLF-terminated fixture at runtime (see
    !! write_text_file_crlf) and confirms it now parses successfully with the correct field type.
    subroutine test_load_maml_file_crlf(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        character(len=*), parameter :: maml_file = "test_run/maml_crlf.maml"

        call write_text_file_crlf(maml_file, [character(len=32) :: &
            "table: crlf_test", "fields:", "- name: id0", "  data_type: int32"])

        call parquet_parse_maml(maml_file, schema)

        call check(error, schema%is_parsed(), "a CRLF-terminated MAML file should parse successfully")
        if (allocated(error)) return
        call check(error, schema%get_num_fields() == 1, "expected exactly one parsed field")
    end subroutine test_load_maml_file_crlf

    !> Writes `lines` verbatim to `path`, one per record -- used below to produce a throwaway
    !! qc-maml file (parquet_load_qc_maml_file only reads from disk, no in-memory constructor
    !! exists for a qc-maml, same as every other maml in this codebase; mirrors
    !! error_scenarios.f90's own write_text_file).
    subroutine write_text_file(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        integer :: unit, i

        open(newunit=unit, file=path, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine write_text_file

    !> Happy-path coverage for parquet_load_qc_maml_file (every other exercise of it lives in
    !! test/error_scenarios.f90's abort/negative-path scenarios -- this is the one in-process
    !! test-drive test proving the disk-loaded qc-maml also works end to end on a clean read,
    !! not just that it can trigger an error stop). Declares qc: min:/max: for "ra" in
    !! test/fixtures/list_vector.parquet, whose ra values ([1.5,2.5,3.5,4.5], see
    !! test_add_col_qc_roundtrip above) all fall within [0, 10], so the default hard qc mode
    !! (qc_soft=.false.) does not abort.
    subroutine test_load_qc_maml_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_reader) :: reader
        integer :: nrows
        real(real64), allocatable :: ra(:)
        character(len=*), parameter :: qc_maml_file = "test_run/qc_maml_load_test.maml"

        call write_text_file(qc_maml_file, [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 10"])

        schema = parquet_load_qc_maml_file(qc_maml_file)

        call check(error, trim(schema%maml%name) == qc_maml_file .and. size(schema%maml%lines) == 5, &
            "parquet_load_qc_maml_file returned unexpected raw content")
        if (allocated(error)) return

        call parquet_open_reader(reader, "test/fixtures/list_vector.parquet", schema=schema)
        call parquet_get_nrows(reader, nrows)
        allocate(ra(nrows))
        call parquet_read_column(reader, "ra", ra)
        call parquet_close_reader(reader)

        call check(error, nrows == 4 .and. abs(ra(1) - 1.5_real64) < 1.0e-12_real64 .and. &
            abs(ra(4) - 4.5_real64) < 1.0e-12_real64, &
            "disk-loaded qc-maml did not read back the ra column correctly")
    end subroutine test_load_qc_maml_file

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

    subroutine test_is_column_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call check(error, schema%is_column_set("id0"), &
            "expected id0 to be reported as set by default after parquet_parse_maml")
        if (allocated(error)) return

        call schema%set_column_unavailable("id0")
        call check(error, .not. schema%is_column_set("id0"), &
            "is_column_set('id0') did not reflect set_column_unavailable")
        if (allocated(error)) return

        call schema%set_column_available("id0")
        call check(error, schema%is_column_set("id0"), &
            "is_column_set('id0') did not reflect set_column_available")
    end subroutine test_is_column_set

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
    !> The ACCEPTANCE side of the struct column's col_size:/array_size: rules.
    !!
    !! The refusals are covered by the struct_col_size_rejected scenario (col_size: 3) and by
    !! parquet_metadata.f90's own auto/`> 1` arms. Nothing covered the boundary those refusals sit
    !! on, so both would pass against a validator that rejected EVERY col_size: on a struct -- and
    !! doc/pages/types/struct-columns.md said exactly that until this review measured it.
    !!
    !! `col_size: 1` is the default, so declaring it must be a no-op rather than an error; a
    !! positive `array_size:` is accepted and simply never consulted for a struct column (only
    !! `array_size: auto` is refused, by the general string-only rule). Each is parsed in its own
    !! schema so that a failure names which one broke.
    subroutine test_struct_col_size_one_accepted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: s_col, s_arr
        call s_col%init(table="struct_colsize_one")
        call s_col%add_field("s", "struct", col_size=1)
        call parquet_parse_maml(s_col)
        call check(error, s_col%is_parsed(), "col_size: 1 on a struct column should parse")
        if (allocated(error)) return
        call s_arr%init(table="struct_arraysize")
        call s_arr%add_field("s", "struct", array_size=8)
        call parquet_parse_maml(s_arr)
        call check(error, s_arr%is_parsed(), "a positive array_size: on a struct column should parse")
    end subroutine test_struct_col_size_one_accepted

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


    !> schema%get_field(name=) reads back every attribute add_field accepted, including the
    !> qc_min/qc_max/qc_miss round trip (reconstructed as an operator-prefixed string / "Null").
    !> Also checks that requesting only a subset of the optional outputs (a bare data_type query)
    !> works.
    !>
    !> **The three qc_miss states are all asserted here, and the mapping is not symmetric.** A
    !> declared miss: Null and NO miss: at all both report "Null", because both mean the same
    !> thing -- Nulls are allowed -- while only an explicit EMPTY miss: reports "", which is the
    !> one form that asks for Null validation. That is what makes the result re-feedable into
    !> %add_field without changing behaviour, and it is the property %add_field_from depends on
    !> (see test_add_field_from_copies_field).
    subroutine test_get_field_by_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        character(len=:), allocatable :: data_type, unit, info, ucd, qc_min, qc_max, qc_miss
        integer :: array_size, col_size

        call schema%init(table="t")
        call schema%add_field("ra", "float64", unit="deg", info="Right ascension.", ucd="pos.eq.ra", &
            qc_min=">=0", qc_max="<360", qc_miss="Null")
        call schema%add_field("flag", "boolean")
        call schema%add_field("checked", "int32", qc_miss="")

        call schema%get_field("ra", data_type=data_type, unit=unit, info=info, ucd=ucd, array_size=array_size, &
            col_size=col_size, qc_min=qc_min, qc_max=qc_max, qc_miss=qc_miss)
        call check(error, data_type == "float64" .and. unit == "deg" .and. info == "Right ascension." .and. &
            ucd == "pos.eq.ra" .and. array_size == 1 .and. col_size == 1 .and. &
            trim(qc_min) == ">= 0" .and. trim(qc_max) == "< 360" .and. qc_miss == "Null", &
            "get_field(name='ra') did not return the expected field definition")
        if (allocated(error)) return

        call schema%get_field("flag", qc_min=qc_min, qc_max=qc_max, qc_miss=qc_miss)
        call check(error, qc_min == "" .and. qc_max == "" .and. qc_miss == "Null", &
            "get_field(name='flag') should report no bounds and, for an undeclared miss:, ""Null""")
        if (allocated(error)) return

        call schema%get_field("checked", qc_min=qc_min, qc_max=qc_max, qc_miss=qc_miss)
        call check(error, qc_min == "" .and. qc_max == "" .and. qc_miss == "", &
            "get_field(name='checked') should report """" for an explicit empty qc: miss:")
        if (allocated(error)) return

        data_type = "unset"
        call schema%get_field("flag", data_type=data_type)
        call check(error, data_type == "boolean", "get_field with only data_type requested did not populate it")
    end subroutine test_get_field_by_name

    !> schema%get_field(index=) is the same lookup as by-name, keyed by 1-based MAML source
    !> position instead, and additionally returns the field's own name.
    subroutine test_get_field_by_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        character(len=:), allocatable :: name, data_type

        call schema%init(table="t")
        call schema%add_field("id0", "int32")
        call schema%add_field("ra", "float64")

        call schema%get_field(2, name, data_type=data_type)
        call check(error, name == "ra" .and. data_type == "float64", &
            "get_field(index=2) did not return the expected field")
    end subroutine test_get_field_by_index


    !> schema%add_field_from copies a field's full definition (incl. qc) from one schema into
    !> another, without the caller re-typing type/unit/qc by hand -- the motivating "shared
    !> identity columns across several output tables" use case.
    subroutine test_add_field_from_copies_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: source, target
        character(len=:), allocatable :: data_type, unit, qc_min, qc_max, qc_miss

        call source%init(table="catalog")
        call source%add_field("obj_id", "int64", unit="count", qc_min=">=0")

        call target%init(table="derived")
        call target%add_field_from(source, "obj_id")

        call check(error, size(target%cinfo%col) == 1 .and. trim(target%cinfo%col(1)%name) == "obj_id", &
            "add_field_from did not append the copied field to the target schema")
        if (allocated(error)) return

        call target%get_field("obj_id", data_type=data_type, unit=unit, qc_min=qc_min, qc_max=qc_max, qc_miss=qc_miss)
        call check(error, data_type == "int64" .and. unit == "count" .and. trim(qc_min) == ">= 0" .and. &
            qc_max == "" .and. qc_miss == "Null", &
            "add_field_from's copied field did not match the source field's definition")
        if (allocated(error)) return

        ! The copy must inherit the source's Null POLICY, not just its text: the source declared no
        ! qc: miss:, so neither schema checks Nulls, and the copy reports "Null" for the same reason
        ! the source does. A copy that came back "" would have silently switched Null validation ON
        ! for the derived table -- the one way this procedure can change behaviour while still
        ! looking like it copied everything.
        call source%get_field("obj_id", qc_miss=qc_miss)
        call check(error, qc_miss == "Null", &
            "the source field's own qc_miss should read back as ""Null"" for an undeclared miss:")
    end subroutine test_add_field_from_copies_field


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

    !> %is_init() alone is not a sufficient readiness check for %print_schema_info -- a
    !! from-scratch schema that has only had %init/%add_field called (never parsed) reports
    !! is_init() == .true. but is_parsed() == .false., since %cinfo%col is only populated by
    !! parquet_parse_maml. Also confirms a freshly declared schema reports is_parsed() == .false.
    !! (matching is_init() == .false. for the same schema).
    subroutine test_schema_is_parsed_false_after_init_alone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call check(error, .not. schema%is_parsed(), &
            "a freshly declared schema should report is_parsed() == .false.")
        if (allocated(error)) return
        call check(error, schema%is_init() .eqv. schema%is_parsed(), &
            "a freshly declared schema should have is_init() == is_parsed() (both .false.)")
        if (allocated(error)) return

        call schema%init(table="is_parsed_test")
        call check(error, schema%is_init(), "sanity check: schema%is_init() should be .true. after %init")
        if (allocated(error)) return
        call check(error, .not. schema%is_parsed(), &
            "schema%is_parsed() should still be .false. after %init alone (no field declared yet)")
        if (allocated(error)) return

        ! The other half of the same distinction: %add_field parses its own field as it goes, so
        ! the schema becomes parsed with no parquet_parse_maml call. %init alone stays unparsed
        ! because a fieldless MAML is not a document -- which is what keeps %is_init() and
        ! %is_parsed() two different questions rather than synonyms.
        call schema%add_field("x", "int32")
        call check(error, schema%is_parsed(), &
            "schema%is_parsed() should be .true. after %add_field, with no parquet_parse_maml call")
    end subroutine test_schema_is_parsed_false_after_init_alone

    !> A schema loaded via parquet_parse_maml (from a file or an already-populated object) never
    !! calls %init at all, but %cinfo%col is populated by the parse -- is_parsed() must report
    !! .true. for it, which is exactly the readiness %print_schema_info itself requires.
    subroutine test_schema_is_parsed_true_after_maml_parse(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call check(error, schema%is_parsed(), &
            "schema%is_parsed() should be .true. for a schema loaded via parquet_parse_maml, " // &
            "even though %init was never called on it")
    end subroutine test_schema_is_parsed_true_after_maml_parse

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

    !> `unit: unitless` is a declaration that the column HAS no unit, so the parser stores an empty
    !! string for it rather than the literal word -- which is what keeps "unitless" out of the
    !! written file's VOTable header and out of column.<name>.unit. It is matched case-insensitively
    !! like every other MAML value that has a fixed vocabulary.
    !!
    !! The erasure is a single unremarkable line in parquet_parse_maml_lines and nothing else fails
    !! if it goes: every file written from then on simply carries a bogus "unitless" unit. The
    !! negative control is the third field -- a real unit must survive untouched, or this test would
    !! pass just as happily against a parser that erased every unit it saw.
    subroutine test_unit_unitless_parses_to_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "unit_unitless.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: unit_unitless_table", &
            "fields:", &
            "- name: plain", &
            "  unit: unitless", &
            "  data_type: int32", &
            "- name: mixedcase", &
            "  unit: UnItLeSs", &
            "  data_type: int32", &
            "- name: real_unit", &
            "  unit: kg", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call check(error, len_trim(schema%cinfo%col(1)%unit) == 0, &
            "unit: unitless should parse to an empty unit, not to the literal word")
        if (allocated(error)) return
        call check(error, len_trim(schema%cinfo%col(2)%unit) == 0, &
            "unit: UnItLeSs should parse to an empty unit too (the token is case-insensitive)")
        if (allocated(error)) return
        call check(error, trim(schema%cinfo%col(3)%unit) == "kg", &
            "a real unit must survive unchanged -- the control that rules out erasing every unit")
    end subroutine test_unit_unitless_parses_to_empty

    !> A field's `ucd:` may be written either as one string or as a YAML list, and the list form is
    !! joined into a single ';'-separated value. schemas/maml_example.maml uses the list form and
    !! test_examples.f90 asserts the result end to end; this is the direct unit test of the join
    !! itself, including the single-item case (which must NOT acquire a separator) and the scalar
    !! form beside it as the control.
    subroutine test_ucd_list_form_joins(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "ucd_list_form.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: ucd_list_form_table", &
            "fields:", &
            "- name: many", &
            "  ucd:", &
            "  - meta.id", &
            "  - meta.main", &
            "  data_type: int32", &
            "- name: one", &
            "  ucd:", &
            "  - pos.eq.ra", &
            "  data_type: int32", &
            "- name: scalar", &
            "  ucd: pos.eq.dec", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call check(error, trim(schema%cinfo%col(1)%ucd) == "meta.id;meta.main", &
            "a two-item list-form ucd: should join with ';'")
        if (allocated(error)) return
        call check(error, trim(schema%cinfo%col(2)%ucd) == "pos.eq.ra", &
            "a single-item list-form ucd: should carry no separator")
        if (allocated(error)) return
        call check(error, trim(schema%cinfo%col(3)%ucd) == "pos.eq.dec", &
            "a scalar ucd: must be unaffected -- the control for the list handling")
    end subroutine test_ucd_list_form_joins

    !> Every MAML KEY is case-insensitive, and that has to hold for the block headers as well as for
    !! the scalar keys. It did not: `parquet_find_maml_section` lowercased both sides while the block
    !! locators compared against lowercase literals, so a MAML spelling its section `Extra:` passed
    !! validation with its nested `protected_cols:`/`col_map:` never found at all -- silently, with
    !! the Null protection the author asked for simply gone. See feature_risks.md Risk-91.
    !!
    !! This is the in-process half: that a fully capitalized MAML parses to the same schema and the
    !! same metadata as its lowercase twin. The abort half -- that a capitalized `Extra:` block's
    !! contents are really reached, not merely tolerated -- is
    !! scenario_extra_section_capitalized in test/error_scenarios.f90.
    subroutine test_maml_block_headers_case_insensitive(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: upper, lower
        character(len=:), allocatable :: got_survey
        integer :: i

        upper%maml%name = "block_headers_upper.maml"
        upper%maml%lines = [character(len=40) :: &
            "TABLE: case_table", &
            "Survey: The Big Survey", &
            "KeyArray:", &
            "- Key: scale", &
            "  Value: 8.1", &
            "DOIs:", &
            "- DOI: 10.1234/x", &
            "  Type: article", &
            "FIELDS:", &
            "- Name: id", &
            "  Data_Type: int32", &
            "  Col_Size: 2" ]
        lower%maml%name = "block_headers_lower.maml"
        lower%maml%lines = [character(len=40) :: &
            "table: case_table", &
            "survey: The Big Survey", &
            "keyarray:", &
            "- key: scale", &
            "  value: 8.1", &
            "dois:", &
            "- doi: 10.1234/x", &
            "  type: article", &
            "fields:", &
            "- name: id", &
            "  data_type: int32", &
            "  col_size: 2" ]

        ! parquet_parse_maml validates first, so reaching the checks at all already proves the
        ! capitalized spelling was accepted as a known set of sections.
        call parquet_parse_maml(upper)
        call parquet_parse_maml(lower)

        call check(error, upper%cinfo%get_num_fields() == 1, &
            "a capitalized FIELDS: block should parse exactly like fields:")
        if (allocated(error)) return
        call check(error, trim(upper%cinfo%col(1)%name) == "id" .and. &
            trim(upper%cinfo%col(1)%data_type) == "int32" .and. upper%cinfo%col(1)%col_size == 2, &
            "a capitalized field's Name:/Data_Type:/Col_Size: should parse like their lowercase twins")
        if (allocated(error)) return

        ! Every metadata entry the lowercase MAML produced must be present, with the same value,
        ! in the capitalized one -- which covers KeyArray:'s and DOIs:' own derived entries.
        call check(error, size(upper%metadata%items) == size(lower%metadata%items), &
            "a capitalized MAML should produce the same number of metadata entries as its lowercase twin")
        if (allocated(error)) return
        do i = 1, size(lower%metadata%items)
            call check(error, trim(upper%metadata%items(i)%key) == trim(lower%metadata%items(i)%key) .and. &
                trim(upper%metadata%items(i)%value) == trim(lower%metadata%items(i)%value), &
                "metadata entry " // trim(lower%metadata%items(i)%key) // " differs between a " // &
                "capitalized MAML and its lowercase twin")
            if (allocated(error)) return
        end do

        ! The control: a VALUE stays case-sensitive. Only keys were made case-insensitive, and a
        ! survey name that came back lowercased would mean the lowercasing had reached the data.
        got_survey = ""
        do i = 1, size(upper%metadata%items)
            if (trim(upper%metadata%items(i)%key) == "survey") got_survey = trim(upper%metadata%items(i)%value)
        end do
        call check(error, got_survey == "The Big Survey", &
            "a metadata VALUE must keep its own capitalization -- only keys are case-insensitive")
    end subroutine test_maml_block_headers_case_insensitive

    !> Regression test: %add_field used to format col_size=parquet_size_auto as a raw integer
    !! ("col_size: -1") instead of the "auto" token the parser actually recognizes, so it would
    !! silently come back as an invalid col_size once (re)parsed/validated. %add_field must emit
    !! the literal "col_size: auto" line for this sentinel, just like hand-written MAML text would.
    !! parquet_parse_maml (object form) runs parquet_validate_maml internally before parsing, so
    !! simply reaching the check below without aborting already proves validation accepted it.
    subroutine test_add_field_col_size_auto_writes_auto_token(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="col_size_auto_add_field_table")
        call schema%add_field("vec", "float32", col_size=parquet_size_auto)

        call check(error, schema%cinfo%col(1)%col_size == parquet_size_auto, &
            "schema%add_field(col_size=parquet_size_auto) should parse back to parquet_size_auto, " // &
            "not size_invalid_sentinel")
    end subroutine test_add_field_col_size_auto_writes_auto_token

    !> Same regression as above, for array_size=parquet_size_auto (only valid on a string column).
    subroutine test_add_field_array_size_auto_writes_auto_token(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="array_size_auto_add_field_table")
        call schema%add_field("txt", "string", array_size=parquet_size_auto)

        call check(error, schema%cinfo%col(1)%array_size == parquet_size_auto, &
            "schema%add_field(array_size=parquet_size_auto) should parse back to parquet_size_auto, " // &
            "not size_invalid_sentinel")
    end subroutine test_add_field_array_size_auto_writes_auto_token


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


    !> force=.true. is the one path that lets set_col_size override an already-resolved (not
    !> "auto") col_size -- in-process, since nothing here aborts.
    subroutine test_set_col_size_force_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="set_col_size_force_table")
        call schema%add_field("vec", "int32", col_size=3)

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


    !> force=.true. is the one path that lets set_array_size override an already-resolved (not
    !> "auto") array_size too -- in-process, since nothing here aborts.
    subroutine test_set_array_size_force_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_force_table")
        call schema%add_field("txt", "string", array_size=8)

        call schema%set_array_size("txt", 12, force=.true.)
        call check(error, schema%cinfo%col(1)%array_size == 12, &
            "set_array_size(force=.true.) did not override the already-resolved array_size")
    end subroutine test_set_array_size_force_overrides
    !
    ! ------------------------------------------------------------------------------
    ! parquet_read_qc and parquet_compose_read_qc
    !
    ! The merge rule under test is per-COLUMN, not per-bound: a column whose MAML
    ! fields: entry carries a qc: key at all wins in full, and the code's entry for
    ! it is dropped entirely rather than filled in around. Tests assert on the
    ! composed schema's own qc-maml text, which is what parquet_open_reader parses.
    ! ------------------------------------------------------------------------------
    !
    !> Whether the composed schema's qc-maml declares `text` on some line -- the usable way to
    !> state "the merge produced this rule", since the composed object is raw MAML source.
    logical function composed_has_line(schema, text) result(res)
        type(parquet_schema), intent(in) :: schema !! composed schema to inspect.
        character(len=*), intent(in) :: text       !! trimmed line to look for.
        integer :: i
        res = .false.
        if (.not. allocated(schema%maml%lines)) return
        do i = 1, size(schema%maml%lines)
            if (trim(adjustl(schema%maml%lines(i))) == text) then
                res = .true.
                return
            end if
        end do
    end function composed_has_line
    !
    !> %add keeps the entry byte for byte (it validates nothing, like its filter/sort siblings),
    !> and %remap_column_names rewrites only the column field, leaving the bounds untouched --
    !> including a bound whose text happens to spell the name being renamed.
    subroutine test_read_qc_add_and_remap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_read_qc) :: qc

        call qc%add("mass, >0, <=1000, Null")
        call qc%add("flag, , , NA")
        call qc%add("other, >mass")
        ! A bound-less entry -- no comma anywhere, so the whole entry IS the column name. Legal
        ! (parquet_compose_read_qc calls it "a legal no-op"), and the one entry shape whose column
        ! name is not delimited by anything, so it is the shape a comma-driven parse gets wrong.
        call qc%add("  mass  ")
        call check(error, trim(qc%entries(1)) == "mass, >0, <=1000, Null" .and. qc%n == 4, &
            "parquet_read_qc%add did not store the entry verbatim")
        if (allocated(error)) return

        call qc%remap_column_names(["mass"], ["m_200c"])
        call check(error, trim(qc%entries(1)) == "m_200c, >0, <=1000, Null", &
            "remap_column_names did not rename the qc entry's column")
        if (allocated(error)) return
        call check(error, trim(qc%entries(2)) == "flag, , , NA", &
            "remap_column_names altered an entry whose column was not renamed")
        if (allocated(error)) return
        call check(error, trim(qc%entries(3)) == "other, >mass", &
            "remap_column_names rewrote a BOUND that happened to spell the renamed column")
        if (allocated(error)) return
        ! The bound-less entry is a column name and nothing else, so it must be renamed -- the
        ! exact opposite of entry 3, whose identical text is a bound and must not be. The two
        ! together are what says the rename keys on POSITION in the entry rather than on the text.
        call check(error, trim(qc%entries(4)) == "m_200c", &
            "remap_column_names did not rename a bound-less entry, whose whole text is the column")
    end subroutine test_read_qc_add_and_remap
    !
    !> The headline rule: a MAML that declares any qc for a column wins for that column IN FULL --
    !> including the bounds it deliberately left empty -- while a column it says nothing about
    !> takes the code's declaration.
    subroutine test_compose_read_qc_maml_wins(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: maml_schema, composed
        type(parquet_read_qc) :: qc
        integer :: ncolumns

        call maml_schema%add_col_qc("column_x, , 100")     ! max: only, min:/miss: deliberately empty
        call qc%add("column_x, 0, 10, Null")               ! must be dropped in full
        call qc%add("column_y, >=1")                       ! MAML says nothing: this one applies

        call parquet_compose_read_qc(maml_schema, qc, composed, ncolumns)
        call check(error, ncolumns == 2, "compose_read_qc reported the wrong qc column count")
        if (allocated(error)) return
        call check(error, composed_has_line(composed, "max: '100'"), &
            "the MAML's own max: bound is missing from the composed schema")
        if (allocated(error)) return
        call check(error, .not. composed_has_line(composed, "min: '0'"), &
            "the code's min: bound overrode a column the MAML had already declared")
        if (allocated(error)) return
        call check(error, .not. composed_has_line(composed, "miss: Null"), &
            "the code's miss: value overrode a column the MAML had already declared")
        if (allocated(error)) return
        call check(error, composed_has_line(composed, "- name: column_y") .and. &
            composed_has_line(composed, "min: '>=1'"), &
            "the code's rule for a column the MAML never mentions did not survive the merge")
    end subroutine test_compose_read_qc_maml_wins
    !
    !> Q9's point: an EMPTY qc: block is not a no-op -- null_values_allowed defaults to .false., so
    !> it already means "no Nulls in this column" -- and must therefore win like any other
    !> declaration. Built by hand, since %add_col_qc emits a qc: block only when a bound is given.
    subroutine test_compose_read_qc_empty_qc_block_wins(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: maml_schema, composed
        type(parquet_read_qc) :: qc
        integer :: ncolumns

        maml_schema%maml%lines = [character(len=20) :: "fields:", "- name: column_x", "  qc:"]
        call qc%add("column_x, 0, 10, Null")

        call parquet_compose_read_qc(maml_schema, qc, composed, ncolumns)
        call check(error, ncolumns == 1, "an empty qc: block did not count as a declared qc column")
        if (allocated(error)) return
        call check(error, .not. composed_has_line(composed, "max: '10'"), &
            "the code's rule overrode a column whose MAML qc: block was merely empty")
    end subroutine test_compose_read_qc_empty_qc_block_wins
    !
    !> The converse of the rule above, and the reason the composed schema copies only qc-BEARING
    !> field entries: a MAML that merely NAMES a column (`unit:` but no `qc:`) has declared no qc
    !> for it, so the code's rule applies -- and must not collide with the MAML's own entry.
    subroutine test_compose_read_qc_named_without_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: maml_schema, composed
        type(parquet_read_qc) :: qc
        integer :: ncolumns

        maml_schema%maml%lines = [character(len=20) :: "fields:", "- name: column_x", &
            "  unit: Msun", "- name: column_z", "  qc:", "    max: '5'"]
        call qc%add("column_x, 0, 10")

        call parquet_compose_read_qc(maml_schema, qc, composed, ncolumns)
        call check(error, ncolumns == 2, "a column the MAML only names should have taken the code's rule")
        if (allocated(error)) return
        call check(error, composed_has_line(composed, "max: '10'"), &
            "the code's rule for a merely-named column did not survive the merge")
        if (allocated(error)) return
        call check(error, .not. composed_has_line(composed, "unit: Msun"), &
            "a field entry with no qc: block was copied into the composed schema")
        if (allocated(error)) return
        call check(error, composed_has_line(composed, "max: '5'"), &
            "the MAML's own qc-bearing entry was lost")
    end subroutine test_compose_read_qc_named_without_qc
    !
    !> Each source on its own, and neither: with nothing to compose, ncolumns is 0 and the caller
    !> knows not to pass a schema to parquet_open_reader at all.
    subroutine test_compose_read_qc_single_and_empty_sources(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: maml_schema, composed
        type(parquet_read_qc) :: qc
        integer :: ncolumns

        call parquet_compose_read_qc(composed=composed, ncolumns=ncolumns)
        call check(error, ncolumns == 0, "composing nothing at all did not report zero qc columns")
        if (allocated(error)) return

        call qc%add("column_y, >=1")
        call parquet_compose_read_qc(qc=qc, composed=composed, ncolumns=ncolumns)
        call check(error, ncolumns == 1 .and. composed_has_line(composed, "min: '>=1'"), &
            "composing code-declared qc with no MAML did not produce the rule")
        if (allocated(error)) return

        call maml_schema%add_col_qc("column_x, , 100")
        call parquet_compose_read_qc(schema=maml_schema, composed=composed, ncolumns=ncolumns)
        call check(error, ncolumns == 1 .and. composed_has_line(composed, "max: '100'"), &
            "composing a MAML with no code-declared qc did not carry its rule through")
        if (allocated(error)) return

        ! An entry naming a column but declaring no bound at all is a legal no-op: it adds a
        ! field with no qc: block, which a reader enforces nothing from, so it must not count.
        call parquet_compose_read_qc(composed=composed, qc=bare_name_qc(), ncolumns=ncolumns)
        call check(error, ncolumns == 0, "an entry declaring no bound at all was counted as a qc column")
    end subroutine test_compose_read_qc_single_and_empty_sources
    !
    !> A parquet_read_qc holding one bound-less entry.
    function bare_name_qc() result(qc)
        type(parquet_read_qc) :: qc !! one entry, "column_x", with no bounds at all.
        call qc%add("column_x")
    end function bare_name_qc
    !
    !> End to end: the composed schema is an ordinary qc-maml, so handing it to
    !> parquet_open_reader enforces exactly the merged rules -- here the MAML's bound, which is
    !> satisfied, while the code's much tighter bound for the same column was correctly dropped.
    subroutine test_compose_read_qc_enforced_on_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: maml_schema, composed
        type(parquet_read_qc) :: qc
        type(parquet_reader) :: reader
        integer :: ncolumns, nrows
        real(real64), allocatable :: ra(:)

        call maml_schema%add_col_qc("ra, >=0, <=10")
        call qc%add("ra, >=100, <=200")      ! would fail the same data, and must be dropped
        call parquet_compose_read_qc(maml_schema, qc, composed, ncolumns)
        call check(error, ncolumns == 1, "compose_read_qc reported the wrong qc column count")
        if (allocated(error)) return

        call parquet_open_reader(reader, "test/fixtures/list_vector.parquet", schema=composed)
        call parquet_get_nrows(reader, nrows)
        allocate(ra(nrows))
        call parquet_read_column(reader, "ra", ra)
        call parquet_close_reader(reader)
        call check(error, nrows == 4 .and. abs(ra(1) - 1.5_real64) < 1.0e-12_real64, &
            "the composed qc schema did not read the column back correctly")
    end subroutine test_compose_read_qc_enforced_on_read
    !
    !> A MAML-declared table-level value is a STRING, by design -- so a keyarray: entry never
    !! gets a "<KEY>.datatype" companion however numeric its text looks, while the same keyword
    !! added in code through a typed %add_metadata call does. This is the rule, not a gap: the
    !! MAML schema carries no type for these keys and is not going to grow one. Without this
    !! test, someone reading the asymmetry as an oversight can "fix" it by sniffing the value
    !! text, and nothing else in the suite would fail.
    subroutine test_maml_keyarray_key_gets_no_datatype(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id0(1) = [1_int32]
        character(len=:), allocatable :: dt, val
        character(len=*), parameter :: out_file = "test_run/maml_keyarray_no_datatype.parquet"

        schema%maml%name = "keyarray_no_datatype.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: keyarray_no_datatype_table", &
            "keyarray:", &
            "- key: NSIDE", &
            "  value: 1024", &
            "Fields:", &
            "- name: id0", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_metadata(reader, "NSIDE", val)
        call check(error, val == "1024", "the keyarray: value itself should be written, got '" // val // "'")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_get_metadata(reader, "NSIDE.datatype", dt, default="<none>", warn=.false.)
        call check(error, dt == "<none>", &
            "a MAML-declared key must get no .datatype companion (it is a string by design), got '" // dt // "'")
        call parquet_close_reader(reader)
    end subroutine test_maml_keyarray_key_gets_no_datatype
    !
end module test_maml
