!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module test_writing
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_stderr, &
        check_scenario_exit_status_and_no_output, run_error_scenario
    !
    implicit none
    private
    public :: collect_tests_parquet_writing
    integer, parameter :: rk = kind(1.0d0)

    type test_output_type
        integer :: id
        integer(int64), dimension(1:2) :: idarr
        character(len=18) :: name
        character(len=4), dimension(1:3) :: name_arr
        integer(int64) :: idlong
        real(real32) :: value
        real(rk) :: value2
        real(real32), dimension(1:5) :: arr
        real(real64), dimension(1:5) :: arrlong
        real(real32) :: val
        integer(int32), dimension(1:3) :: iarr
        logical :: flag
        logical, dimension(1:6) :: flag_arr
    end type test_output_type
    !
contains
    !
    !> Collect all exported unit tests
    subroutine collect_tests_parquet_writing(testsuite)
        implicit none
        integer :: status
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        !
        testsuite = [ &
            new_unittest("write extensive parquet file", test_write_parquet_file), &
            new_unittest("write simple parquet file", test_write_simple_parquet), &
            new_unittest("write_maml=.true. saves a sidecar .maml file", test_write_maml_sidecar), &
            new_unittest("write_maml=.true. does not prune when every column is enabled", &
                test_write_maml_sidecar_no_pruning_when_all_enabled), &
            new_unittest("write_maml=.true. prunes correctly across blank/comment lines and reordered keys", &
                test_write_maml_sidecar_prune_scan_edge_cases), &
            new_unittest("write_maml=.true. appends .maml for a non-.parquet output filename", &
                test_write_maml_sidecar_non_parquet_filename), &
            new_unittest("add_metadata after parquet_parse_maml is reflected in the sidecar", &
                test_write_maml_sidecar_with_runtime_metadata), &
            new_unittest("add_metadata inserts keyarray: before an existing extra:", &
                test_add_metadata_inserts_before_extra), &
            new_unittest("col_map: renamed column is written/read under its output name", &
                test_col_map_write_renames_output_column), &
            new_unittest("write scalar column with is_valid produces genuine Nulls", &
                test_write_scalar_with_is_valid), &
            new_unittest("write matrix column with is_valid produces element-level Nulls", &
                test_write_matrix_with_is_valid), &
            new_unittest("write string column with is_valid produces genuine Nulls", &
                test_write_string_with_is_valid), &
            new_unittest("write with is_valid all .true. keeps the column non-nullable", &
                test_write_is_valid_all_true), &
            new_unittest("protected_cols: allows writing a protected column with no Nulls", &
                test_write_protected_column_without_null), &
            new_unittest("protected_cols: semicolon and dash-list forms are equivalent", &
                test_protected_cols_semicolon_and_dash_list_equivalent), &
            new_unittest("qc: min/max parsed with and without an explicit operator", &
                test_qc_parsing_plain_and_operator_forms), &
            new_unittest("qc=.true. prints a WARNING for an out-of-range numeric value", &
                test_qc_warning_printed_for_numeric_violation), &
            new_unittest("qc=.true. WARNING for a fractional bound uses fractional formatting", &
                test_qc_warning_printed_for_fractional_bound), &
            new_unittest("qc=.true. prints a WARNING for an out-of-range string value", &
                test_qc_warning_printed_for_string_violation), &
            new_unittest("qc: on a boolean field is accepted but never enforced", &
                test_qc_silently_ignored_for_boolean), &
            new_unittest("compression=gzip round-trips and shrinks a compressible file", &
                test_compression_gzip_round_trip), &
            new_unittest("compression=uncompressed writes a larger file than the snappy default", &
                test_compression_uncompressed_larger_than_default), &
            new_unittest("an unknown compression codec aborts", test_compression_unknown_aborts), &
            new_unittest("overwrite=.false. aborts when the file already exists", &
                test_overwrite_false_existing_file_aborts), &
            new_unittest("overwrite=.false. writes normally when the file does not yet exist", &
                test_overwrite_false_new_file), &
            new_unittest("overwrite defaults to .true. and truncates an existing file", &
                test_overwrite_default_truncates), &
            new_unittest("reusing the same writer variable across two files leaves no stale " // &
                "row-count/written-name state", test_reused_writer_variable_across_files_no_stale_state), &
            new_unittest("chunk_size forces multiple row groups and still round-trips", &
                test_chunk_size_round_trip), &
            new_unittest("chunk_size auto-sizes when not given and still round-trips", &
                test_chunk_size_auto_sizing_without_explicit_value), &
            new_unittest("chunk_size auto-sizing accounts for a wide vector column", &
                test_chunk_size_auto_sizing_wide_row), &
            new_unittest("parquet_prefetch_columns still allows reading a non-prefetched column", &
                test_prefetch_columns_then_read_non_prefetched), &
            new_unittest("repeated, overlapping parquet_prefetch_columns calls still read correctly", &
                test_prefetch_columns_repeated_overlapping_calls), &
            new_unittest("parquet_prefetch_columns accepts a comma/semicolon string of names", &
                test_prefetch_columns_string_form), &
            new_unittest("parquet_open_reader(prefetch=.true.) warms every column and still reads correctly", &
                test_open_reader_prefetch_true), &
            new_unittest("parquet_open_reader(prefetch=.true.) with an active filter still returns filtered rows", &
                test_open_reader_prefetch_true_with_filter), &
            new_unittest("parquet_close_reader(print_stat=.true.) does not disturb a normal close", &
                test_close_reader_print_stat_smoke), &
            new_unittest("a string/string-vector column too large for arrow::utf8() round-trips via large_utf8()", &
                test_large_string_column_roundtrip), &
            new_unittest("a STRING_VIEW column (from a file written by another Arrow-based tool) round-trips " // &
                "correctly", test_string_view_column_roundtrip), &
            new_unittest("a vector column whose auto-sized row-group size is clamped for the int32 " // &
                "list-element-count limit still round-trips, split across multiple row groups", &
                test_list_element_count_auto_multi_row_group_roundtrip), &
            new_unittest("qc: range violation prints a WARNING but does not abort", &
                test_qc_range_violation_warns), &
            new_unittest("qc: range violation on an extended (uint16) source type warns but does not abort", &
                test_extended_qc_range_violation_warns), &
            new_unittest("qc-maml: a stray no-colon line before qc: still warns correctly", &
                test_qc_maml_stray_no_colon_line_ok), &
            new_unittest("qc: unexpected Null prints a WARNING but does not abort", &
                test_qc_null_violation_warns), &
            new_unittest("qc: miss: Null suppresses the Null-presence WARNING", &
                test_qc_miss_null_no_warning), &
            new_unittest("qc=.false. suppresses a would-be range violation WARNING", &
                test_qc_disabled_explicit_no_warning), &
            new_unittest("qc-maml may declare a column absent from the parquet file", &
                test_qc_column_not_in_file), &
            new_unittest("parquet_open_reader(filter=) ANDs multiple rules and updates nrows", &
                test_open_reader_filter_ands_rules), &
            new_unittest("parquet_open_reader(filter=) supports is_null/is_not_null and quoted strings", &
                test_open_reader_filter_null_and_string_rules), &
            new_unittest("parquet_open_reader(filter=) leaves a non-filter vector column readable", &
                test_filter_leaves_vector_column_readable), &
            new_unittest("parquet_open_reader(filter=) supports boolean equality and string ordering", &
                test_filter_boolean_and_string_ordering), &
            new_unittest("parquet_prefetch_columns after a filtered open still returns filtered rows", &
                test_filter_prefetch_after_open), &
            new_unittest("parquet_open_reader(filter=) with zero matching rows still works", &
                test_filter_zero_matching_rows), &
            new_unittest("parquet_open_reader(nrows=) fills in the post-filter row count", &
                test_open_reader_nrows_arg), &
            new_unittest("re-opening an already-open reader without closing it finalizes the old handle", &
                test_reopen_reader_without_closing_finalizes_old_handle), &
            new_unittest("re-opening an already-open writer without closing it finalizes the old handle", &
                test_reopen_writer_without_closing_finalizes_old_handle), &
            new_unittest("use_threads=.false. on writer and reader still round-trips", &
                test_use_threads_false_still_round_trips), &
            new_unittest("parquet_set_max_threads with a valid value does not break a round-trip", &
                test_set_max_threads_valid_value_does_not_break_round_trip), &
            new_unittest("writing a numeric kind that differs from the schema's declared data_type " // &
                "converts to match it", test_write_cross_type_schema_coercion), &
            new_unittest("float32 scalar (with is_valid) and float32/boolean vector columns round-trip", &
                test_write_float32_boolean_vector_columns), &
            new_unittest("vector columns written through a qc-enabled schema round-trip (with/without is_valid)", &
                test_write_vector_columns_schema_qc), &
            new_unittest("streaming row-group write (schema-enforced): mixed whole/chunked columns round-trip", &
                test_streaming_write_schema_enforced_roundtrip), &
            new_unittest("streaming row-group write (schema-less): mixed whole/chunked columns round-trip", &
                test_streaming_write_schemaless_roundtrip), &
            new_unittest("streaming row-group write: a single row group covering the whole file round-trips", &
                test_streaming_write_single_row_group_roundtrip), &
            new_unittest("streaming row-group write: string and logical chunked columns round-trip", &
                test_streaming_write_string_logical_roundtrip), &
            new_unittest("parquet_get_chunk_size(writer) returns a positive value before and during streaming", &
                test_streaming_get_chunk_size), &
            new_unittest("streaming row-group write: every type/shape (incl. logical/string) round-trips " // &
                "with is_valid+qc branches exercised", test_streaming_write_all_types_roundtrip), &
            new_unittest("streaming row-group write: col_size>1 string column via a flat rank-1 array round-trips", &
                test_streaming_write_string_flat_vector_chunk), &
            new_unittest("compact (parquet_string_column) string round-trip, incl. nulls/empty strings", &
                test_compact_string_roundtrip), &
            new_unittest("compact string write is readable via the padded path, and vice versa", &
                test_compact_string_cross_path_compat), &
            new_unittest("compact string chunked write/read round-trips across multiple row groups", &
                test_compact_string_chunked_roundtrip), &
            new_unittest("compact string write under qc=.true. warns (does not abort) on a violation", &
                test_compact_string_qc_warning), &
            new_unittest("compact string read reflects an active row filter", &
                test_compact_string_filter_read) &
            ]
        !
    end subroutine collect_tests_parquet_writing
    !
    subroutine test_write_simple_parquet(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        real(rk), dimension(5) :: xdata = [1,2,3,4,5]
        real(rk), dimension(3,5) :: xdata2
        type(parquet_writer) :: writer
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_simple.parquet"
        integer:: i,j
        !
        do i = 1, 5
            do j = 1, 3
                xdata2(j,i) = real(i+j, kind=rk)
            end do
        end do
        !
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "colx", xdata)
        call parquet_write_column(writer, "arr", xdata2)
        call parquet_close_writer(writer)
        !
        inquire(file=out_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "expected simple output parquet file was not created")
            return
        end if
        !
    end subroutine test_write_simple_parquet
    !
    !> write_maml=.true. prunes the fields: entries of columns disabled via
    !> set_unavailable, so the sidecar's field list matches what was actually
    !> written to the .parquet file, while leaving every other MAML section
    !> (dataset/author/keyarray/etc.) untouched.
    subroutine test_write_maml_sidecar(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema, sidecar_schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml.maml"

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)

        call check(error, size(sidecar_schema%cinfo%col) == 1, &
            "expected the sidecar .maml to only list the one enabled column ('id0')")
        if (allocated(error)) return
        call check(error, trim(sidecar_schema%cinfo%col(1)%name) == "id0", &
            "expected the sidecar .maml's only field entry to be 'id0'")
        if (allocated(error)) return

        ! Non-field content (table-level metadata, keyarray) must be untouched.
        call check(error, size(sidecar_schema%metadata%items) == size(schema%metadata%items), &
            "pruning disabled fields should not affect table-level metadata items")
    end subroutine test_write_maml_sidecar

    !> When every column in cinfo stays enabled, nothing should be pruned:
    !> the sidecar should still match the source MAML line-for-line.
    subroutine test_write_maml_sidecar_no_pruning_when_all_enabled(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_maml_file) :: source_maml, sidecar_maml
        type(test_output_type), allocatable :: test_data(:)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml_full.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml_full.maml"
        integer :: i

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call init_test_data(test_data, 5)
        call write_test_data(out_file, test_data, schema, write_maml=.true.)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        source_maml = parquet_load_maml_file("schemas/maml_example.maml")
        sidecar_maml = parquet_load_maml_file(sidecar_file)

        call check(error, size(sidecar_maml%lines) == size(source_maml%lines), &
            "sidecar .maml file does not have the same number of lines as the source MAML " // &
            "when no columns are disabled")
        if (allocated(error)) return

        do i = 1, size(source_maml%lines)
            call check(error, trim(sidecar_maml%lines(i)) == trim(source_maml%lines(i)), &
                "sidecar .maml file content does not match the source MAML line-for-line " // &
                "when no columns are disabled")
            if (allocated(error)) return
        end do
    end subroutine test_write_maml_sidecar_no_pruning_when_all_enabled

    !> parquet_prune_disabled_fields's field-block scan must handle three
    !> real-world MAML authoring variations that schemas/maml_example.maml
    !> happens not to use anywhere, so this schema is built in-memory instead
    !> of loaded from a fixture:
    !>   1. an indented comment line right after "fields:", before the first
    !>      "- name:" entry (outer scan's blank/non-block-line fallthrough);
    !>   2. a blank line between two "- name:" blocks (ordinary readability
    !>      formatting);
    !>   3. a field block ("extra") whose "name:" key is not the first
    !>      attribute -- valid, order-independent MAML (the real field parser
    !>      in parquet_metadata.f90 doesn't require any key ordering), just an
    !>      unconventional style no existing fixture happens to use.
    subroutine test_write_maml_sidecar_prune_scan_edge_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema, sidecar_schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml_scan_edge_cases.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml_scan_edge_cases.maml"

        schema%maml%name = "prune_scan_edge_cases.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: edge_case_table", &
            "fields:", &
            "  # comment before the first field", &
            "- name: id0", &
            "  data_type: int32", &
            "", &
            "- data_type: int32", &
            "  name: extra" ]

        call parquet_parse_maml(schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)

        call check(error, size(sidecar_schema%cinfo%col) == 1, &
            "expected the sidecar .maml to only list the one enabled column ('id0') " // &
            "with a leading comment, a blank line, and a reordered-key field block present")
        if (allocated(error)) return
        call check(error, trim(sidecar_schema%cinfo%col(1)%name) == "id0", &
            "expected the sidecar .maml's only field entry to be 'id0' " // &
            "with a leading comment, a blank line, and a reordered-key field block present")
    end subroutine test_write_maml_sidecar_prune_scan_edge_cases

    !> parquet_write_maml_sidecar derives the sidecar path from the output
    !> filename: a trailing ".parquet" is replaced with ".maml", but for any
    !> other extension (or none) ".maml" is simply appended instead. Covers
    !> both branches of that function's length-safety guard (parquet_write.f90
    !> checks len >= 8 before indexing the last 8 characters to compare
    !> against ".parquet"): a filename >= 8 characters that doesn't end in
    !> ".parquet", and one under 8 characters (too short to even hold
    !> ".parquet", so the substring comparison is skipped entirely). The short
    !> case can't live under test_run/ (whose own prefix is already 9
    !> characters), so it's written to, and cleaned up from, the current
    !> working directory instead.
    subroutine test_write_maml_sidecar_non_parquet_filename(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: id0(2) = [1_int32, 2_int32]
        logical :: exists
        integer :: unit, ios

        schema%maml%name = "non_parquet_filename.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: non_parquet_filename_table", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)

        ! Long enough (>= 8 characters) but no ".parquet" suffix.
        call parquet_open_writer(writer, "test_run/plainfile.dat", schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file="test_run/plainfile.dat.maml", exist=exists)
        call check(error, exists, &
            "write_maml=.true. with a non-.parquet, >=8-character filename did not append .maml")
        if (allocated(error)) return

        ! Too short to hold ".parquet" (< 8 characters).
        call parquet_open_writer(writer, "s.dat", schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file="s.dat.maml", exist=exists)

        open(newunit=unit, file="s.dat", status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")
        open(newunit=unit, file="s.dat.maml", status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")

        call check(error, exists, &
            "write_maml=.true. with a short (<8-character) filename did not append .maml")
    end subroutine test_write_maml_sidecar_non_parquet_filename
    !
    subroutine test_write_maml_sidecar_with_runtime_metadata(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema, sidecar_schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml_runtime.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml_runtime.maml"
        integer :: i
        logical :: found

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        ! Added after parquet_parse_maml: should now be reflected in the sidecar.
        call schema%add_metadata("generated_by", "unit_test", "added at runtime")

        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)

        found = .false.
        do i = 1, size(sidecar_schema%metadata%items)
            if (trim(sidecar_schema%metadata%items(i)%key) == "generated_by") then
                call check(error, trim(sidecar_schema%metadata%items(i)%value) == "unit_test", &
                    "sidecar keyarray entry for 'generated_by' has an unexpected value")
                if (allocated(error)) return
                call check(error, trim(sidecar_schema%metadata%items(i)%description) == "added at runtime", &
                    "sidecar keyarray entry for 'generated_by' has an unexpected comment")
                if (allocated(error)) return
                found = .true.
                exit
            end if
        end do

        call check(error, found, &
            "add_metadata call made after parquet_parse_maml was not reflected in the written sidecar .maml")
        if (allocated(error)) return

        ! The keyarray entries already present in the source MAML must still be there too.
        found = .false.
        do i = 1, size(sidecar_schema%metadata%items)
            if (trim(sidecar_schema%metadata%items(i)%key) == "test_scalar") found = .true.
        end do
        call check(error, found, &
            "pre-existing keyarray entries from the source MAML were lost when appending runtime metadata")
    end subroutine test_write_maml_sidecar_with_runtime_metadata
    !
    !> Covers both of parquet_locate_keyarray_insert's header-less fallbacks
    !> (src/parquet_metadata.f90): inserting before an existing "extra:"
    !> section when there's no "keyarray:" yet, and -- when there's neither
    !> "keyarray:" nor "extra:" -- inserting before "fields:" instead.
    subroutine test_add_metadata_inserts_before_extra(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema, schema_no_extra
        integer :: idx_keyarray, idx_extra, idx_fields, i

        ! Built in-memory (rather than loaded from schemas/) so this test does not
        ! depend on whether the on-disk fixture happens to have a keyarray:
        ! block already: this specifically covers the "no existing keyarray:,
        ! but an extra: section is present" case.
        schema%maml%name = "no_keyarray_with_extra.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: no_keyarray_table", &
            "extra:", &
            "  anything:", &
            "    like: 1.3", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call schema%add_metadata("added_key", "42", "added comment")

        call check(error, allocated(schema%metadata%source_maml_lines), &
            "expected source_maml_lines to be populated after parquet_parse_maml")
        if (allocated(error)) return

        idx_keyarray = 0
        idx_extra = 0
        do i = 1, size(schema%metadata%source_maml_lines)
            if (schema%metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(schema%metadata%source_maml_lines(i))) == "keyarray:") idx_keyarray = i
            if (schema%metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(schema%metadata%source_maml_lines(i))) == "extra:") idx_extra = i
        end do

        call check(error, idx_keyarray > 0, "expected a synthesized 'keyarray:' header in source_maml_lines")
        if (allocated(error)) return
        call check(error, idx_extra > 0, "expected the pre-existing 'extra:' header to still be present")
        if (allocated(error)) return
        call check(error, idx_keyarray < idx_extra, &
            "expected the synthesized 'keyarray:' block to be inserted before the existing 'extra:' section")
        if (allocated(error)) return

        call check(error, trim(adjustl(schema%metadata%source_maml_lines(idx_keyarray+1))) == "- key: added_key", &
            "expected the new keyarray entry right after the synthesized header")
        if (allocated(error)) return
        call check(error, trim(adjustl(schema%metadata%source_maml_lines(idx_keyarray+2))) == "value: 42", &
            "unexpected value line for the new keyarray entry")
        if (allocated(error)) return
        call check(error, trim(adjustl(schema%metadata%source_maml_lines(idx_keyarray+3))) == "comment: added comment", &
            "unexpected comment line for the new keyarray entry")
        if (allocated(error)) return

        ! Second case: no "extra:" section either -- the synthesized
        ! "keyarray:" header must land right before "fields:" instead.
        schema_no_extra%maml%name = "no_keyarray_no_extra.maml"
        schema_no_extra%maml%lines = [character(len=40) :: &
            "table: no_keyarray_no_extra_table", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema_no_extra)
        call schema_no_extra%add_metadata("added_key", "42", "added comment")

        idx_keyarray = 0
        idx_fields = 0
        do i = 1, size(schema_no_extra%metadata%source_maml_lines)
            if (schema_no_extra%metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(schema_no_extra%metadata%source_maml_lines(i))) == "keyarray:") idx_keyarray = i
            if (schema_no_extra%metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(schema_no_extra%metadata%source_maml_lines(i))) == "fields:") idx_fields = i
        end do

        call check(error, idx_keyarray > 0, &
            "expected a synthesized 'keyarray:' header in source_maml_lines (no extra: case)")
        if (allocated(error)) return
        call check(error, idx_fields > 0, "expected the pre-existing 'fields:' header to still be present")
        if (allocated(error)) return
        call check(error, idx_keyarray < idx_fields, &
            "expected the synthesized 'keyarray:' block to be inserted before 'fields:' when no extra: exists")
    end subroutine test_add_metadata_inserts_before_extra
    !
    !> A col_map:-renamed column ("id0" -> "my_id") is written/read using the
    !> internal name ("id0", as parquet_write_column/parquet_read_column
    !> always expect), but the actual file column, sidecar .maml fields:
    !> entry, and read-back must all use the output/user-facing name
    !> ("my_id") -- confirming the rename actually reaches the parquet file
    !> itself, not just cinfo%col in memory.
    subroutine test_col_map_write_renames_output_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: base_maml
        type(parquet_schema) :: schema, sidecar_schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        integer(int32), allocatable :: id_read(:)
        integer(int64) :: nrows
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_col_map.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_col_map.maml"

        base_maml = get_parquet_maml("maml_example.maml")

        schema%maml%name = "user_col_map_write.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - id0: my_id", &
            "fields:", &
            "- name: my_id", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, schema%maml)
        call parquet_parse_maml(schema)

        ! parquet_write_column is called with the internal name ("id0"), not
        ! the renamed output name ("my_id").
        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        ! The actual file column must be named "my_id": reading it back by
        ! its internal name ("id0") must fail to find a match, while reading
        ! by "my_id" must return the written data.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id_read(nrows))
        call parquet_read_column(reader, "my_id", id_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id_read == id0), &
            "expected the renamed column to be readable (and match written data) under its output name 'my_id'")
        if (allocated(error)) return

        ! The sidecar .maml's fields: entry must also say "my_id" (the
        ! source MAML's own declared name), not "id0". The sidecar keeps
        ! col_map: verbatim too, so re-parsing it round-trips exactly like
        ! the original: name is resolved back to "id0" (internal), with
        ! output_name carrying "my_id" again.
        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)
        call check(error, size(sidecar_schema%cinfo%col) == 1 .and. trim(sidecar_schema%cinfo%col(1)%name) == "id0" .and. &
            trim(sidecar_schema%cinfo%col(1)%output_name) == "my_id", &
            "expected the sidecar .maml to round-trip the col_map: rename (name=id0, output_name=my_id)")
    end subroutine test_col_map_write_renames_output_column
    !
    !> Writing a scalar int32 column with an is_valid mask containing a
    !> .false. entry produces a genuine Parquet Null there (not a sentinel
    !> value): confirmed by reading it back with is_valid, which must report
    !> that slot invalid and default it to 0 (no null_value requested).
    subroutine test_write_scalar_with_is_valid(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid_out(3), is_valid_in(3) = [.true., .false., .true.]
        integer(int32) :: read_back(3)
        character(len=*), parameter :: out_file = "test_run/test_write_is_valid_scalar.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id_with_null", values, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "id_with_null", read_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)

        call check(error, is_valid_out(1) .and. (.not. is_valid_out(2)) .and. is_valid_out(3), &
            "is_valid mask not correctly round-tripped for a written scalar Null")
        if (allocated(error)) return

        call check(error, read_back(1) == 1_int32 .and. read_back(2) == 0_int32 .and. read_back(3) == 3_int32, &
            "written scalar Null did not default to 0 on read-back")
    end subroutine test_write_scalar_with_is_valid

    !> Same as above but for a matrix (vector-column) write: is_valid is
    !> element-level, shaped like values -- a single element within an
    !> otherwise-present row can be Null without affecting its row's other
    !> elements or any other row.
    subroutine test_write_matrix_with_is_valid(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(2,3), read_back(2,3)
        logical :: is_valid_in(2,3), is_valid_out(2,3)
        character(len=*), parameter :: out_file = "test_run/test_write_is_valid_matrix.parquet"

        values = reshape([1,2,3,4,5,6], [2,3])
        is_valid_in = .true.
        is_valid_in(2,3) = .false.

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "arr_with_null", values, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "arr_with_null", read_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)

        call check(error, all(is_valid_out(:,1)) .and. all(is_valid_out(:,2)) .and. &
            is_valid_out(1,3) .and. (.not. is_valid_out(2,3)), &
            "is_valid mask not correctly round-tripped for a written array-column Null")
        if (allocated(error)) return

        call check(error, read_back(1,1) == 1_int32 .and. read_back(2,1) == 2_int32 .and. &
            read_back(1,2) == 3_int32 .and. read_back(2,2) == 4_int32 .and. &
            read_back(1,3) == 5_int32 .and. read_back(2,3) == 0_int32, &
            "written array-column Null did not default to 0 on read-back")
    end subroutine test_write_matrix_with_is_valid

    subroutine test_write_string_with_is_valid(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=16) :: values(3), read_back(3)
        logical :: is_valid_in(3) = [.true., .false., .true.], is_valid_out(3)
        character(len=*), parameter :: out_file = "test_run/test_write_is_valid_string.parquet"

        values = ["first ", "second", "third "]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "name_with_null", values, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name_with_null", read_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)

        call check(error, is_valid_out(1) .and. (.not. is_valid_out(2)) .and. is_valid_out(3), &
            "is_valid mask not correctly round-tripped for a written string Null")
        if (allocated(error)) return

        call check(error, trim(read_back(1)) == "first" .and. len_trim(read_back(2)) == 0 .and. &
            trim(read_back(3)) == "third", &
            "written string Null did not default to blank on read-back")
    end subroutine test_write_string_with_is_valid

    !> Passing is_valid with every entry .true. must not make the column
    !> nullable or introduce a Null: reading it back with the default,
    !> strict (no null_value/is_valid) call must succeed exactly as if
    !> is_valid had never been passed at all.
    subroutine test_write_is_valid_all_true(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid_in(3) = [.true., .true., .true.]
        integer(int32) :: read_back(3)
        character(len=*), parameter :: out_file = "test_run/test_write_is_valid_all_true.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id_all_valid", values, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "id_all_valid", read_back)
        call parquet_close_reader(reader)

        call check(error, all(read_back == values), &
            "strict (non-null-tolerant) read failed for a column written with an all-.true. is_valid mask")
    end subroutine test_write_is_valid_all_true

    !> A protected column with no Nulls at all (is_valid omitted) must write
    !> and read back completely normally -- protection only blocks an actual
    !> Null, never a plain write.
    subroutine test_write_protected_column_without_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer(int32) :: read_back(3)
        character(len=*), parameter :: out_file = "test_run/test_protected_no_null.parquet"

        schema%maml%name = "protected_no_null.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call check(error, schema%cinfo%col(1)%is_protected, "expected column 'a' to be marked is_protected")
        if (allocated(error)) return

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "a", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "a", read_back)
        call parquet_close_reader(reader)

        call check(error, all(read_back == values), &
            "writing/reading a protected column with no Nulls should behave exactly as normal")
    end subroutine test_write_protected_column_without_null

    !> extra: protected_cols: col1;col2 (semicolon-scalar form) and the
    !> equivalent dash-list form must mark exactly the same columns
    !> is_protected.
    subroutine test_protected_cols_semicolon_and_dash_list_equivalent(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema_semicolon, schema_dashlist

        schema_semicolon%maml%name = "protected_semicolon.maml"
        schema_semicolon%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a;b", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32", &
            "- name: c", &
            "  data_type: int32" ]

        schema_dashlist%maml%name = "protected_dashlist.maml"
        schema_dashlist%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols:", &
            "  - a", &
            "  - b", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32", &
            "- name: c", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema_semicolon)
        call parquet_parse_maml(schema_dashlist)

        call check(error, schema_semicolon%cinfo%col(1)%is_protected .and. schema_semicolon%cinfo%col(2)%is_protected .and. &
            (.not. schema_semicolon%cinfo%col(3)%is_protected), &
            "semicolon-form protected_cols did not mark the expected columns")
        if (allocated(error)) return

        call check(error, schema_dashlist%cinfo%col(1)%is_protected .and. schema_dashlist%cinfo%col(2)%is_protected .and. &
            (.not. schema_dashlist%cinfo%col(3)%is_protected), &
            "dash-list-form protected_cols did not mark the expected columns")
    end subroutine test_protected_cols_semicolon_and_dash_list_equivalent

    !> qc: min:/max: with a plain number (no operator) default to inclusive
    !> (">="/"<=" respectively); a quoted value with an explicit operator
    !> prefix (">=", "<=", ">", "<") uses that operator instead.
    subroutine test_qc_parsing_plain_and_operator_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        schema%maml%name = "qc_parsing.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: id", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1", &
            "    max: 1000", &
            "- name: ra", &
            "  data_type: float64", &
            "  qc:", &
            "    min: '>= 0'", &
            "    max: '< 360'" ]

        call parquet_parse_maml(schema)

        call check(error, schema%cinfo%col(1)%has_qc_min .and. schema%cinfo%col(1)%has_qc_max .and. &
            trim(schema%cinfo%col(1)%qc_min_op) == ">=" .and. trim(schema%cinfo%col(1)%qc_max_op) == "<=" .and. &
            trim(schema%cinfo%col(1)%qc_min_raw) == "1" .and. trim(schema%cinfo%col(1)%qc_max_raw) == "1000", &
            "plain-number qc: min/max did not default to inclusive operators with the expected bound text")
        if (allocated(error)) return

        call check(error, schema%cinfo%col(2)%has_qc_min .and. schema%cinfo%col(2)%has_qc_max .and. &
            trim(schema%cinfo%col(2)%qc_min_op) == ">=" .and. trim(schema%cinfo%col(2)%qc_max_op) == "<" .and. &
            trim(schema%cinfo%col(2)%qc_min_raw) == "0" .and. trim(schema%cinfo%col(2)%qc_max_raw) == "360", &
            "operator-prefixed qc: min/max did not parse the expected operator and bound text")
    end subroutine test_qc_parsing_plain_and_operator_forms

    !> qc=.true. never errors -- it only ever prints a WARNING to stdout and
    !> lets the write proceed. Since test-drive can't observe an in-process
    !> print statement's stdout reliably, the actual write (error_scenarios'
    !> "qc_warning_numeric"/"qc_warning_string" scenarios) is run as a
    !> subprocess with its stdout captured to a file, which is then checked
    !> for the expected WARNING text.
    subroutine test_qc_warning_printed_for_numeric_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat

        call check_qc_scenario_warns(error, "qc_warning_numeric", "a", exitstat, cmdstat)
        if (allocated(error)) return

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range numeric value should not error stop (warning only)")
    end subroutine test_qc_warning_printed_for_numeric_violation

    !> Same as test_qc_warning_printed_for_numeric_violation, but the qc:
    !> bound is fractional ("0.5") rather than a whole number, so the WARNING
    !> text's bounds_desc must go through parquet_qc_format_real's
    !> fractional-value (g0.7) formatting branch instead of its whole-number
    !> (i0) one -- checked directly by looking for "0.5" in the captured
    !> output, not just the presence of a WARNING.
    subroutine test_qc_warning_printed_for_fractional_bound(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=*), parameter :: out_file = "test_run/qc_warning_fractional_bound_output.txt"
        logical :: found_bound_text

        call run_error_scenario("qc_warning_fractional_bound", "> " // out_file // " 2>&1", exitstat, cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range fractional-bound value should not error stop (warning only)")
        if (allocated(error)) return

        call file_contains(out_file, "0.5", found_bound_text)
        call check(error, found_bound_text, &
            "expected the qc violation WARNING to include the fractionally-formatted bound '0.5'")
    end subroutine test_qc_warning_printed_for_fractional_bound

    subroutine test_qc_warning_printed_for_string_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat

        call check_qc_scenario_warns(error, "qc_warning_string", "s", exitstat, cmdstat)
        if (allocated(error)) return

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range string value should not error stop (warning only)")
    end subroutine test_qc_warning_printed_for_string_violation

    subroutine test_qc_silently_ignored_for_boolean(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=*), parameter :: out_file = "test_run/qc_boolean_output.txt"
        logical :: found_warning

        call run_error_scenario("qc_silently_ignored_for_boolean", "> " // out_file // " 2>&1", exitstat, cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "qc: on a boolean field should never be enforced (no error expected)")
        if (allocated(error)) return

        call file_contains(out_file, "WARNING", found_warning)
        call check(error, .not. found_warning, "qc: on a boolean field must never print a WARNING")
    end subroutine test_qc_silently_ignored_for_boolean

    !> Runs error_scenarios' `scenario_name` as a subprocess (its stdout
    !> captured to test_run/<scenario_name>_output.txt) and asserts the
    !> captured output contains a WARNING mentioning `column_name`.
    subroutine check_qc_scenario_warns(error, scenario_name, column_name, exitstat, cmdstat)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario_name, column_name
        integer, intent(out) :: exitstat, cmdstat
        character(len=:), allocatable :: out_file
        logical :: found_warning

        out_file = "test_run/" // trim(scenario_name) // "_output.txt"

        call run_error_scenario(scenario_name, "> " // out_file // " 2>&1", exitstat, cmdstat)
        if (cmdstat /= 0) return

        call file_contains_warning_for_column(out_file, column_name, found_warning)
        call check(error, found_warning, &
            "expected a WARNING message mentioning column '" // trim(column_name) // "' in stdout")
    end subroutine check_qc_scenario_warns

    subroutine file_contains_warning_for_column(filename, column_name, found)
        character(len=*), intent(in) :: filename, column_name
        logical, intent(out) :: found
        integer :: unit, ios
        character(len=512) :: line

        found = .false.
        open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "WARNING") > 0 .and. index(line, "'" // trim(column_name) // "'") > 0) found = .true.
        end do
        close(unit)
    end subroutine file_contains_warning_for_column

    subroutine file_contains(filename, needle, found)
        character(len=*), intent(in) :: filename, needle
        logical, intent(out) :: found
        integer :: unit, ios
        character(len=512) :: line

        found = .false.
        open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, needle) > 0) found = .true.
        end do
        close(unit)
    end subroutine file_contains

    subroutine test_compression_gzip_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(2000), read_back(2000)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_compression_gzip.parquet"

        values = 42_int32

        call parquet_open_writer(writer, out_file, compression="gzip")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2000_int64 .and. all(read_back == values), &
            "compression=gzip did not round-trip the written data correctly")
    end subroutine test_compression_gzip_round_trip

    !> A single repeated value is fully collapsed by Parquet's own default
    !> dictionary encoding regardless of the compression codec on top, so
    !> that can't be used to demonstrate a codec-driven size difference. This
    !> test instead uses long, highly repetitive but mutually distinct
    !> strings (a run of "A"s plus a distinguishing per-row suffix): too many
    !> distinct values for dictionary encoding to collapse them away, but
    !> still very compressible at the byte level, so compression="gzip"
    !> (a strong, reliable ratio) must produce a smaller file than the same
    !> data written with compression="uncompressed".
    subroutine test_compression_uncompressed_larger_than_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        character(len=256) :: values(100)
        character(len=*), parameter :: gzip_file = "test_run/test_compression_gzip_size.parquet"
        character(len=*), parameter :: uncompressed_file = "test_run/test_compression_uncompressed.parquet"
        integer(int64) :: gzip_size, uncompressed_size
        logical :: exists
        integer :: i

        do i = 1, size(values)
            values(i) = repeat("A", 240)
            write(values(i)(241:256), '(i16.16)') i
        end do

        call parquet_open_writer(writer, gzip_file, compression="gzip")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_writer(writer, uncompressed_file, compression="uncompressed")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        inquire(file=gzip_file, exist=exists, size=gzip_size)
        call check(error, exists, "gzip-compression output file was not created")
        if (allocated(error)) return

        inquire(file=uncompressed_file, exist=exists, size=uncompressed_size)
        call check(error, exists, "uncompressed output file was not created")
        if (allocated(error)) return

        call check(error, uncompressed_size > gzip_size, &
            "expected compression=uncompressed to produce a larger file than compression=gzip")
    end subroutine test_compression_uncompressed_larger_than_default

    subroutine test_compression_unknown_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_unknown_compression", expect_abort=.true., &
            failure_message="an unknown compression codec name was expected to error stop")
    end subroutine test_compression_unknown_aborts

    !> parquet_open_writer(..., overwrite=.false.) must error stop rather than truncate an
    !> already-existing file (out-of-process: error stop kills the test process itself).
    subroutine test_overwrite_false_existing_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_overwrite_false_existing_file", expect_abort=.true., &
            failure_message="overwrite=.false. over an existing file was expected to error stop")
    end subroutine test_overwrite_false_existing_file_aborts

    !> overwrite=.false. is not a hazard when the file does not exist yet -- it must write
    !> and round-trip exactly like the default.
    subroutine test_overwrite_false_new_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(3) = [1, 2, 3]
        integer(int32) :: read_back(3)
        character(len=*), parameter :: out_file = "test_run/test_overwrite_false_new_file.parquet"
        integer :: unit, ios

        open(newunit=unit, file=out_file, status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")

        call parquet_open_writer(writer, out_file, overwrite=.false.)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, all(read_back == values), &
            "overwrite=.false. on a new file did not round-trip the written data correctly")
    end subroutine test_overwrite_false_new_file

    !> Not passing overwrite= (or passing overwrite=.true. explicitly) must keep the existing
    !> silent-truncate behavior -- reopening the same path must succeed and reflect the newest
    !> write, not the first one.
    subroutine test_overwrite_default_truncates(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: first_values(3) = [1, 2, 3]
        integer(int32) :: second_values(2) = [9, 8]
        integer(int32) :: read_back(2)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_overwrite_default_truncates.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", first_values)
        call parquet_close_writer(writer)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", second_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2_int64 .and. all(read_back == second_values), &
            "default overwrite behavior did not truncate the file down to the second write")
    end subroutine test_overwrite_default_truncates

    !> Reusing the same schema-less parquet_writer variable across two separate
    !! open/write/close cycles must not carry over per-file bookkeeping (row-count
    !! expectation, written-column names) from the first file to the second: writing
    !! the same column name with a different row count on the second file must not
    !! spuriously trip either the row-count-mismatch or already-written-name checks.
    subroutine test_reused_writer_variable_across_files_no_stale_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: first_values(3) = [1, 2, 3]
        integer(int32) :: second_values(2) = [9, 8]
        integer(int32) :: read_back(2)
        integer(int64) :: nrows
        character(len=*), parameter :: first_file = "test_run/test_reused_writer_first.parquet"
        character(len=*), parameter :: second_file = "test_run/test_reused_writer_second.parquet"

        call parquet_open_writer(writer, first_file)
        call parquet_write_column(writer, "v", first_values)
        call parquet_close_writer(writer)

        call parquet_open_writer(writer, second_file)
        call parquet_write_column(writer, "v", second_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, second_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2_int64 .and. all(read_back == second_values), &
            "reusing the writer variable across files leaked stale row-count/written-name state")
    end subroutine test_reused_writer_variable_across_files_no_stale_state

    !> A small chunk_size (row group length) relative to the row count forces
    !> multiple row groups; the file must still read back correctly.
    subroutine test_chunk_size_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(10)
        integer(int32) :: read_back(10)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_chunk_size.parquet"
        integer :: i

        values = [(i, i=1,10)]

        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 10_int64 .and. all(read_back == values), &
            "chunk_size=2 (multiple row groups) did not round-trip the written data correctly")
    end subroutine test_chunk_size_round_trip

    !> When chunk_size is not given, the writer auto-sizes it from the final
    !> table's actual in-memory byte size (targeting ~256 MiB per row group),
    !> not a flat row count -- a table smaller than that target must still
    !> round-trip in a single row group.
    subroutine test_chunk_size_auto_sizing_without_explicit_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(10)
        integer(int32) :: read_back(10)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_chunk_size_auto.parquet"
        integer :: i

        values = [(i, i=1,10)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 10_int64 .and. all(read_back == values), &
            "auto-sized chunk_size (no explicit value given) did not round-trip the written data correctly")
    end subroutine test_chunk_size_auto_sizing_without_explicit_value

    !> A "wide" row (a vector column with a large col_size) reaches the ~256 MiB row-group byte
    !> target at well under a million rows -- a flat row-count-based auto-sizing heuristic would
    !> pick the same chunk_size regardless of col_size, but a byte-size-aware one should scale it
    !> down for a wide column. Checked directly via parquet_get_chunk_size's schema-based estimate
    !> (computed purely from column metadata -- see estimate_chunk_size_from_schema in
    !> parquet_wrapper.cpp -- before any data is written), comparing a wide schema's estimate
    !> against a narrow one's, rather than by actually writing enough data to reach that target:
    !> doing that for a genuinely wide column would need writing/reading a file hundreds of MB in
    !> size on every `fpm test` run. tools/test_large_scale.sh is the maintainer-runnable check
    !> that exercises writing/reading data at real scale instead (see CONTRIBUTING.md).
    subroutine test_chunk_size_auto_sizing_wide_row(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: narrow_schema, wide_schema
        type(parquet_writer) :: narrow_writer, wide_writer
        integer(int64) :: narrow_chunk_size, wide_chunk_size
        integer(int64) :: narrow_data(1) = [1_int64]
        integer(int64), allocatable :: wide_data(:, :)
        character(len=*), parameter :: narrow_file = "test_run/test_chunk_size_auto_narrow.parquet"
        character(len=*), parameter :: wide_file = "test_run/test_chunk_size_auto_wide.parquet"

        call narrow_schema%init(table="chunk_size_narrow_table")
        call narrow_schema%add_field("v", "int64")
        call parquet_parse_maml(narrow_schema)

        call wide_schema%init(table="chunk_size_wide_table")
        call wide_schema%add_field("v", "int64", col_size=1000)
        call parquet_parse_maml(wide_schema)

        call parquet_open_writer(narrow_writer, narrow_file, narrow_schema)
        call parquet_get_chunk_size(narrow_writer, narrow_chunk_size)
        call parquet_write_column(narrow_writer, "v", narrow_data)
        call parquet_close_writer(narrow_writer)

        allocate(wide_data(1000, 1))
        wide_data = 1_int64
        call parquet_open_writer(wide_writer, wide_file, wide_schema)
        call parquet_get_chunk_size(wide_writer, wide_chunk_size)
        call parquet_write_column(wide_writer, "v", wide_data)
        call parquet_close_writer(wide_writer)

        call check(error, narrow_chunk_size > 0 .and. wide_chunk_size > 0 .and. &
            wide_chunk_size < narrow_chunk_size, &
            "a wide vector column's auto-sized chunk_size estimate was not scaled down for its col_size")
    end subroutine test_chunk_size_auto_sizing_wide_row

    !> parquet_prefetch_columns must warm the cache for the requested columns
    !> without breaking parquet_read_column for a column that was never
    !> prefetched -- the non-prefetched column still falls through to the
    !> existing lazy, read-on-first-request path.
    subroutine test_prefetch_columns_then_read_non_prefetched(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5), c_values(5)
        integer(int32) :: a_back(5), b_back(5), c_back(5)
        character(len=*), parameter :: out_file = "test_run/test_prefetch.parquet"
        integer :: i

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]
        c_values = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_write_column(writer, "c", c_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_prefetch_columns(reader, ["a", "b"])
        call parquet_read_column(reader, "a", a_back)
        call parquet_read_column(reader, "c", c_back)
        call parquet_read_column(reader, "b", b_back)
        call parquet_read_column(reader, "c", c_back)
        call parquet_close_reader(reader)

        call check(error, all(a_back == a_values) .and. all(b_back == b_values) .and. all(c_back == c_values), &
            "prefetching a subset of columns broke reading either the prefetched or the non-prefetched column")
    end subroutine test_prefetch_columns_then_read_non_prefetched

    !> Calling parquet_prefetch_columns more than once, with column sets that
    !> partially overlap an earlier call, must accumulate the union of every
    !> column named across all calls -- not just the columns from the most
    !> recent call. This exercises the column_cache skip-if-already-cached
    !> logic in parquet_reader_prefetch_columns (src/parquet_wrapper.cpp).
    subroutine test_prefetch_columns_repeated_overlapping_calls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5), c_values(5)
        integer(int32) :: a_back(5), b_back(5), c_back(5)
        character(len=*), parameter :: out_file = "test_run/test_prefetch_overlap.parquet"
        integer :: i

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]
        c_values = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_write_column(writer, "c", c_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_prefetch_columns(reader, ["a", "b"])
        call parquet_prefetch_columns(reader, ["b", "c"])
        call parquet_read_column(reader, "a", a_back)
        call parquet_read_column(reader, "b", b_back)
        call parquet_read_column(reader, "c", c_back)
        call parquet_close_reader(reader)

        call check(error, all(a_back == a_values) .and. all(b_back == b_values) .and. all(c_back == c_values), &
            "repeated, overlapping parquet_prefetch_columns calls did not yield the union of all requested columns")
    end subroutine test_prefetch_columns_repeated_overlapping_calls

    !> The scalar-string form of parquet_prefetch_columns accepts names of
    !> differing lengths separated by commas and/or semicolons -- notably a
    !> short name next to a longer one ("a", "xa"), the case that a
    !> fixed-length character array literal cannot express without truncation.
    !> Also checks that surrounding spaces and repeated/trailing delimiters are
    !> tolerated, and that the file genuinely has no column "x" (so a truncated
    !> "xa" -> "x" would have failed) yet the read still succeeds.
    subroutine test_prefetch_columns_string_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), xa_values(5), b_values(5)
        integer(int32) :: a_back(5), xa_back(5), b_back(5)
        character(len=*), parameter :: out_file = "test_run/test_prefetch_string.parquet"
        integer :: i

        a_values  = [(i, i=1,5)]
        xa_values = [(i*10, i=1,5)]
        b_values  = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "xa", xa_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        ! mixed comma/semicolon, stray spaces, and a trailing delimiter.
        call parquet_prefetch_columns(reader, "a; xa , b;")
        call parquet_read_column(reader, "a", a_back)
        call parquet_read_column(reader, "xa", xa_back)
        call parquet_read_column(reader, "b", b_back)
        call parquet_close_reader(reader)

        call check(error, all(a_back == a_values) .and. all(xa_back == xa_values) .and. all(b_back == b_values), &
            "the string form of parquet_prefetch_columns did not prefetch a/xa/b correctly")
    end subroutine test_prefetch_columns_string_form

    !> parquet_open_reader(..., prefetch=.true.) must warm every column in
    !> the file at open time -- reading any of them afterward (in any order)
    !> must still return the correct data, exactly as if prefetch had never
    !> been requested.
    subroutine test_open_reader_prefetch_true(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5), c_values(5)
        integer(int32) :: a_back(5), b_back(5), c_back(5)
        character(len=*), parameter :: out_file = "test_run/test_open_reader_prefetch.parquet"
        integer :: i

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]
        c_values = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_write_column(writer, "c", c_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, prefetch=.true.)
        call parquet_read_column(reader, "c", c_back)
        call parquet_read_column(reader, "a", a_back)
        call parquet_read_column(reader, "b", b_back)
        call parquet_close_reader(reader)

        call check(error, all(a_back == a_values) .and. all(b_back == b_values) .and. all(c_back == c_values), &
            "parquet_open_reader(..., prefetch=.true.) did not correctly prefetch every column")
    end subroutine test_open_reader_prefetch_true

    !> Regression test for the ordering loophole where prefetch-all runs
    !> before the filter mask is set: a column NOT referenced by the filter
    !> must still come back correctly filtered when prefetch=.true. and a
    !> filter are both used together -- see parquet_open_reader_base's own
    !> comment (parquet_read.f90) on why prefetch must run after
    !> parquet_apply_filter.
    subroutine test_open_reader_prefetch_true_with_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(6), payload(6)
        integer(int32) :: nrows
        integer(int32), allocatable :: id_back(:), payload_back(:)
        character(len=*), parameter :: out_file = "test_run/test_open_reader_prefetch_filter.parquet"
        integer :: i

        id = [(i, i=1,6)]
        payload = [(i*1000, i=1,6)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "payload", payload)
        call parquet_close_writer(writer)

        call filt%add("id > 4")
        call parquet_open_reader(reader, out_file, filter=filt, prefetch=.true.)
        call parquet_get_nrows(reader, nrows)

        allocate(id_back(nrows), payload_back(nrows))
        ! "payload" is never referenced by the filter -- it is only ever
        ! touched via the prefetch=.true. all-columns warm-up, so this is
        ! exactly the case that would silently return unfiltered data if
        ! prefetch-all ran before the filter mask existed.
        call parquet_read_column(reader, "payload", payload_back)
        call parquet_read_column(reader, "id", id_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2 .and. all(id_back == [5, 6]) .and. all(payload_back == [5000, 6000]), &
            "prefetch=.true. combined with an active filter returned unfiltered data for a non-filter column")
    end subroutine test_open_reader_prefetch_true_with_filter

    !> print_stat=.true. always prints to stdout -- run out-of-process (see
    !> scenario_print_stat_smoke in error_scenarios.f90) so that output is
    !> captured/discarded by check_scenario_exit_status instead of
    !> interleaving with test-drive's own progress lines in the visible
    !> `fpm test` console output.
    subroutine test_close_reader_print_stat_smoke(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_smoke", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) was expected to exit cleanly")
    end subroutine test_close_reader_print_stat_smoke

    !> A string or string-vector column whose byte payload would overflow Arrow's real int32
    !> STRING-offset limit (~2GiB) is written as arrow::large_utf8() instead of arrow::utf8()
    !> (see would_overflow_string_offset_limit in parquet_wrapper.cpp) and must still round-trip
    !> correctly. Exercising this for real would need a genuine multi-gigabyte column (tens of
    !> seconds to build), far too slow for this suite -- so the actual round-trip (write, read
    !> back, parquet_get_string_length, print_stat, and a row filter, all against a tiny fixture
    !> forced onto the large_utf8 path via a test-only threshold override) runs out-of-process
    !> as scenario_large_string_roundtrip in error_scenarios.f90; see that scenario's own
    !> comment for why the override is safe only when isolated like this.
    subroutine test_large_string_column_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "large_string_roundtrip", expect_abort=.false., &
            failure_message="a string/string-vector column forced onto the arrow::large_utf8() path " // &
            "did not round-trip correctly")
    end subroutine test_large_string_column_roundtrip

    !> Unlike LARGE_STRING (above), this library's own writer can never produce a STRING_VIEW
    !> column at all -- it only ever arrives from a Parquet file written by another Arrow-based
    !> tool whose stored Arrow schema declared the column as utf8_view() (see
    !> is_string_like_type's own comment in parquet_wrapper.cpp). So the fixture here is built
    !> directly with Arrow's own StringViewBuilder (parquet_debug_write_string_view_fixture, a
    !> test-only hook), bypassing this library's writer entirely -- run out-of-process as
    !> scenario_string_view_roundtrip in error_scenarios.f90; see that scenario's own comment.
    subroutine test_string_view_column_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "string_view_roundtrip", expect_abort=.false., &
            failure_message="a STRING_VIEW column did not round-trip correctly")
    end subroutine test_string_view_column_roundtrip

    !> A vector column's flattened element count (nrows * col_size) is capped at 2^31-1 *per row
    !> group*, not per file -- close_parquet_writer's auto-sizing path silently clamps its own
    !> computed row-group size down to whatever is safe for the widest vector column present, so
    !> a column whose total nrows * col_size would otherwise exceed that limit now writes
    !> successfully, split across multiple row groups. Exercising this for real would need a
    !> genuine multi-billion-element column, far too slow/large for this suite -- so the actual
    !> round-trip runs out-of-process as scenario_list_element_count_auto_multi_row_group in
    !> error_scenarios.f90 (same pattern as test_large_string_column_roundtrip, above), against a
    !> tiny fixture forced to split via a test-only threshold override; see that scenario's own
    !> comment for why the override is safe only when isolated like this.
    subroutine test_list_element_count_auto_multi_row_group_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "list_element_count_auto_multi_row_group", expect_abort=.false., &
            failure_message="a vector column split across multiple auto-clamped row groups " // &
            "did not round-trip correctly")
    end subroutine test_list_element_count_auto_multi_row_group_roundtrip

    !> Read-time qc, like print_stat, always prints straight to stdout --
    !> run out-of-process (see scenario_qc_range_violation_warns in
    !> error_scenarios.f90) for the same reason print_stat's smoke test
    !> does: keep that output out of the visible `fpm test` console log,
    !> while still asserting on its content via the captured file.
    subroutine test_qc_range_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_warns", expect_abort=.false., &
            failure_message="a qc: range violation must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra'")
    end subroutine test_qc_range_violation_warns

    !> Same as test_qc_range_violation_warns, but proves run_qc_range_check's
    !> extension to the new read-time source types (see
    !> scenario_extended_qc_range_violation_warns's own comment in
    !> error_scenarios.f90) actually fires rather than silently never
    !> checking a UINT16 column.
    subroutine test_extended_qc_range_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_qc_range_violation_warns", expect_abort=.false., &
            failure_message="a qc: range violation on a uint16 column must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'v_uint16'")
    end subroutine test_extended_qc_range_violation_warns

    !> A stray line with no colon inside a qc-maml field block (before its
    !> qc: sub-block) must be silently skipped rather than breaking parsing
    !> -- checked by confirming the qc: min: bound declared right after it
    !> is still correctly recognized and enforced (the WARNING fires).
    subroutine test_qc_maml_stray_no_colon_line_ok(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_stray_no_colon_line", expect_abort=.false., &
            failure_message="a qc-maml with a stray no-colon line before its qc: block must still warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra'")
    end subroutine test_qc_maml_stray_no_colon_line_ok

    subroutine test_qc_null_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_null_violation_warns", expect_abort=.false., &
            failure_message="a qc: unexpected-Null violation must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'id'")
    end subroutine test_qc_null_violation_warns

    subroutine test_qc_miss_null_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_miss_null_no_warning", expect_abort=.false., &
            failure_message="qc: miss: Null scenario was expected to exit cleanly", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_miss_null_no_warning

    subroutine test_qc_disabled_explicit_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_disabled_explicit_no_warning", expect_abort=.false., &
            failure_message="qc=.false. scenario was expected to exit cleanly", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_disabled_explicit_no_warning

    subroutine test_qc_column_not_in_file(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_column_not_in_file", expect_abort=.false., &
            failure_message="a qc-maml field naming a column absent from the file was expected to exit cleanly")
    end subroutine test_qc_column_not_in_file

    !> Every parquet_filter%add rule ANDs together: "ra > 200", "ra <= 360",
    !> and "id /= 7" together should keep only rows where ra is in (200,360]
    !> AND id isn't 7 -- and parquet_get_nrows/parquet_read_column should
    !> transparently reflect just that filtered row set.
    subroutine test_open_reader_filter_ands_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: ra(10), id(10)
        integer(int32) :: nrows
        integer(int32), allocatable :: ra_back(:), id_back(:)
        character(len=*), parameter :: out_file = "test_run/test_filter_and.parquet"
        integer :: i

        ra = [(i*40, i=1,10)]
        id = [(i, i=1,10)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "ra", ra)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call filt%add("ra > 200")
        call filt%add("ra <= 360")
        call filt%add("id /= 7")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)

        allocate(ra_back(nrows), id_back(nrows))
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_read_column(reader, "id", id_back)
        call parquet_close_reader(reader)

        ! ra in (200, 360] is {240, 280, 320, 360} (id 6,7,8,9); excluding
        ! id == 7 (ra == 280) leaves exactly {240, 320, 360} / {6, 8, 9}.
        call check(error, nrows == 3, "parquet_get_nrows did not reflect the AND-combined filter's row count")
        if (allocated(error)) return

        call check(error, all(ra_back == [240, 320, 360]) .and. all(id_back == [6, 8, 9]), &
            "parquet_read_column did not return the AND-combined filter's expected rows")
    end subroutine test_open_reader_filter_ands_rules

    !> is_null/is_not_null and a double-quoted string equality rule, each
    !> exercised on their own so a false match/no-match on one wouldn't be
    !> masked by another passing rule.
    subroutine test_open_reader_filter_null_and_string_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt_string, filt_not_null
        integer(int32) :: id(5)
        logical :: is_valid(5)
        character(len=8) :: name(5)
        integer(int32) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_filter_null_string.parquet"
        integer :: i

        id = [(i, i=1,5)]
        is_valid = [.true., .false., .true., .true., .false.]
        do i = 1, 5
            write(name(i), '(A,I0)') "row", i
        end do

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id, is_valid=is_valid)
        call parquet_write_column(writer, "name", name)
        call parquet_close_writer(writer)

        call filt_string%add('name == "row3"')
        call parquet_open_reader(reader, out_file, filter=filt_string)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        call check(error, nrows == 1, "quoted string equality filter did not match exactly one row")
        if (allocated(error)) return

        call filt_not_null%add("id is_not_null")
        call parquet_open_reader(reader, out_file, filter=filt_not_null)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        call check(error, nrows == 3, "id is_not_null did not exclude exactly the two genuine Nulls")
    end subroutine test_open_reader_filter_null_and_string_rules

    !> A filter narrows every column's row count via arrow::compute::Filter,
    !> including fixed-size-list (vector) columns -- even one never named in
    !> any filter rule. This specifically guards against the filtered array's
    !> internal offsets/slicing being wrong for get_uniform_list_values/
    !> get_row_list_values (parquet_wrapper.cpp), which was never exercised
    !> by the AND/is_null/string tests above (those only ever read scalar
    !> columns back).
    subroutine test_filter_leaves_vector_column_readable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(6)
        integer(int64) :: vec(3,6)
        integer(int32) :: nrows
        integer(int64), allocatable :: vec_back(:,:)
        integer(int64) :: row_back(3)
        character(len=*), parameter :: out_file = "test_run/test_filter_vector_readback.parquet"
        integer :: i

        id = [(i, i=1,6)]
        do i = 1, 6
            vec(:,i) = [i*10_int64, i*20_int64, i*30_int64]
        end do

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        ! Filter only references "id" -- "vec" is never named in a rule.
        call filt%add("id > 2")
        call filt%add("id <= 5")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 3, "filter on 'id' did not produce the expected 3 filtered rows")
        if (allocated(error)) return

        allocate(vec_back(3, nrows))
        call parquet_read_column(reader, "vec", vec_back)
        call check(error, &
            all(vec_back(:,1) == [30_int64, 60_int64, 90_int64]) .and. &
            all(vec_back(:,2) == [40_int64, 80_int64, 120_int64]) .and. &
            all(vec_back(:,3) == [50_int64, 100_int64, 150_int64]), &
            "reading a non-filter vector column after filtering did not return the correctly filtered rows")
        if (allocated(error)) return

        ! Row-mode read of the (already-filtered) 2nd row must line up with
        ! vec_back(:,2) above -- i.e. id == 4's original vector, not id == 2's.
        call parquet_read_array_row_mode(reader, "vec", row_back, 2)
        call parquet_close_reader(reader)

        call check(error, all(row_back == [40_int64, 80_int64, 120_int64]), &
            "parquet_read_array_row_mode on a filtered reader did not return the correctly filtered row")
    end subroutine test_filter_leaves_vector_column_readable

    !> Boolean equality (only ==//= are supported for boolean columns) and
    !> string ordering comparisons (<, <=, >, >=, lexicographic), neither of
    !> which the AND/is_null/quoted-equality tests above exercise.
    subroutine test_filter_boolean_and_string_ordering(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt_bool, filt_string
        integer(int32) :: id(6)
        logical :: flag(6)
        character(len=8) :: name(6)
        integer(int32) :: nrows
        character(len=8), allocatable :: name_back(:)
        character(len=*), parameter :: out_file = "test_run/test_filter_bool_string.parquet"
        integer :: i

        id = [(i, i=1,6)]
        flag = [.true., .false., .true., .false., .true., .false.]
        do i = 1, 6
            write(name(i), '(A,I0)') "n", i
        end do

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "flag", flag)
        call parquet_write_column(writer, "name", name)
        call parquet_close_writer(writer)

        call filt_bool%add("flag == true")
        call parquet_open_reader(reader, out_file, filter=filt_bool)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        call check(error, nrows == 3, "flag == true did not match exactly the 3 true rows")
        if (allocated(error)) return

        ! Lexicographic: "n2" < "n3" < "n4" < "n5" -- rule keeps "n3"/"n4" only.
        call filt_string%add('name > "n2"')
        call filt_string%add('name <= "n4"')
        call parquet_open_reader(reader, out_file, filter=filt_string)
        call parquet_get_nrows(reader, nrows)
        allocate(name_back(nrows))
        call parquet_read_column(reader, "name", name_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2 .and. all(name_back == ["n3      ", "n4      "]), &
            "string ordering filter rules did not select exactly ['n3','n4']")
    end subroutine test_filter_boolean_and_string_ordering

    !> parquet_prefetch_columns called on a reader that already has a filter
    !> set must still route through the same filtering path as an ordinary
    !> lazy read (see apply_filter_mask in parquet_wrapper.cpp) -- otherwise
    !> a prefetched column would silently return unfiltered data.
    subroutine test_filter_prefetch_after_open(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(6)
        integer(int32) :: nrows
        integer(int32), allocatable :: id_back(:)
        character(len=*), parameter :: out_file = "test_run/test_filter_prefetch_after.parquet"
        integer :: i

        id = [(i, i=1,6)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call filt%add("id > 4")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)

        ! "id" was already decoded/filtered as part of applying the filter
        ! itself -- prefetch it again anyway, to exercise the "already
        ! cached" skip path together with an active filter.
        call parquet_prefetch_columns(reader, ["id"])
        allocate(id_back(nrows))
        call parquet_read_column(reader, "id", id_back)
        call parquet_close_reader(reader)

        call check(error, nrows == 2 .and. all(id_back == [5, 6]), &
            "parquet_prefetch_columns after a filtered open did not return filtered rows")
    end subroutine test_filter_prefetch_after_open

    !> A filter that matches no rows at all is a valid, non-error outcome:
    !> parquet_get_nrows must report 0, and reading a zero-length column must
    !> not crash.
    subroutine test_filter_zero_matching_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(6)
        integer(int32) :: nrows
        integer(int32), allocatable :: id_back(:)
        character(len=*), parameter :: out_file = "test_run/test_filter_zero_rows.parquet"
        integer :: i

        id = [(i, i=1,6)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call filt%add("id > 100")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 0, "a filter matching no rows did not produce nrows == 0")
        if (allocated(error)) return

        allocate(id_back(nrows))
        call parquet_read_column(reader, "id", id_back)
        call parquet_close_reader(reader)

        call check(error, size(id_back) == 0, "reading a zero-length filtered column did not behave correctly")
    end subroutine test_filter_zero_matching_rows

    !> parquet_open_reader(nrows=) is sugar for opening then calling
    !> parquet_get_nrows(reader, nrows, check_positive=.true.) -- checks it
    !> returns the post-filter count (not the file's raw total) when a
    !> filter narrows the rows, and the unfiltered total otherwise. The
    !> zero-matching-rows abort path itself is covered separately by the
    !> open_reader_nrows_zero_rows error scenario, since it error stops.
    !>
    !> nrows= is generic over integer(int32)/integer(int64) (see
    !> parquet_open_reader's interface in src/parquet.f90), so this also
    !> checks the int32 form -- including a plain default INTEGER actual
    !> argument, the form most callers reach for and the one a caller who
    !> only declares `integer :: nrows` (no explicit kind) would use.
    subroutine test_open_reader_nrows_arg(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(6)
        integer(int64) :: nrows
        integer(int32) :: nrows32
        integer :: nrows_default
        character(len=*), parameter :: out_file = "test_run/test_open_reader_nrows_arg.parquet"
        integer :: i

        id = [(i, i=1,6)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, nrows=nrows)
        call check(error, nrows == 6_int64, "parquet_open_reader(nrows=) did not return the unfiltered row count")
        call parquet_close_reader(reader)
        if (allocated(error)) return

        call filt%add("id > 3")
        call parquet_open_reader(reader, out_file, filter=filt, nrows=nrows)
        call check(error, nrows == 3_int64, "parquet_open_reader(nrows=) did not return the post-filter row count")
        call parquet_close_reader(reader)
        if (allocated(error)) return

        call parquet_open_reader(reader, out_file, nrows=nrows32)
        call check(error, nrows32 == 6_int32, "parquet_open_reader(nrows=) with an integer(int32) actual " // &
            "did not return the row count")
        call parquet_close_reader(reader)
        if (allocated(error)) return

        call parquet_open_reader(reader, out_file, nrows=nrows_default)
        call check(error, nrows_default == 6, &
            "parquet_open_reader(nrows=) with a plain default-INTEGER actual did not return the row count")
        call parquet_close_reader(reader)
    end subroutine test_open_reader_nrows_arg

    !> A parquet_reader variable that's still open (never explicitly closed)
    !> when parquet_open_reader is called on it again must not leak or crash:
    !> since `reader` is an intent(out) argument, Fortran finalizes the old
    !> handle automatically first (see reader_finalize, and MANUAL.md's
    !> "Important behavior" note on re-opening). This exercises
    !> reader_finalize's actual cleanup branch (a still-open handle), not
    !> just its no-op path (which every already-closed reader hits).
    subroutine test_reopen_reader_without_closing_finalizes_old_handle(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_reopen_reader.parquet"
        integer :: i

        id = [(i, i=1,4)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, nrows=nrows)
        call check(error, nrows == 4_int64, "first parquet_open_reader(nrows=) did not return the expected row count")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        ! reader is intentionally NOT closed here -- re-opening it below must
        ! finalize (free) the still-open handle automatically.
        call parquet_open_reader(reader, out_file, nrows=nrows)
        call check(error, nrows == 4_int64, &
            "re-opening an already-open reader (without closing it first) did not return the expected row count")
        call parquet_close_reader(reader)
    end subroutine test_reopen_reader_without_closing_finalizes_old_handle

    !> Same as above, for the write side: re-opening an open parquet_writer
    !> without closing it first must finalize (free) the old handle
    !> automatically -- see writer_finalize, which deliberately skips
    !> parquet_close_writer's "missing required column" check for exactly
    !> this case (an incomplete write must not surprise-abort from an
    !> implicit finalizer).
    subroutine test_reopen_writer_without_closing_finalizes_old_handle(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(3)
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/test_reopen_writer.parquet"
        integer :: i

        id = [(i, i=1,3)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)

        ! writer is intentionally NOT closed here -- re-opening it below must
        ! finalize (free) the still-open handle automatically, without
        ! erroring over the incomplete previous write.
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, nrows=nrows)
        call check(error, nrows == 3_int64, &
            "file written after re-opening an already-open writer did not round-trip the expected row count")
        call parquet_close_reader(reader)
    end subroutine test_reopen_writer_without_closing_finalizes_old_handle

    !> use_threads=.false. must still be a fully functional writer/reader --
    !> it only turns off Arrow's internal thread pool for that instance, it
    !> never changes what gets written or read.
    subroutine test_use_threads_false_still_round_trips(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(5), read_back(5)
        character(len=*), parameter :: out_file = "test_run/test_use_threads_false.parquet"
        integer :: i

        values = [(i, i=1,5)]

        call parquet_open_writer(writer, out_file, use_threads=.false.)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, use_threads=.false.)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, all(read_back == values), &
            "use_threads=.false. on the writer and/or reader broke the round-trip")
    end subroutine test_use_threads_false_still_round_trips

    !> parquet_set_max_threads is a global Arrow thread-pool capacity knob,
    !> not something that changes any file's content -- a valid call must be
    !> a no-op as far as write/read correctness goes.
    subroutine test_set_max_threads_valid_value_does_not_break_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(5), read_back(5)
        character(len=*), parameter :: out_file = "test_run/test_set_max_threads.parquet"
        integer :: i

        values = [(i, i=1,5)]

        call parquet_set_max_threads(2)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", read_back)
        call parquet_close_reader(reader)

        call check(error, all(read_back == values), &
            "parquet_set_max_threads(2) broke a subsequent write/read round-trip")
    end subroutine test_set_max_threads_valid_value_does_not_break_round_trip

    !> parquet_write_column is generic over the *declared kind of the `values`
    !> array you pass in*, not the schema's own data_type for that column -- as
    !> long as the conversion is a supported one, the writer converts to match
    !> the schema (narrowing int64->int32 checked for overflow; float->int
    !> checked for both range and a non-integral value; everything else is an
    !> unchecked direct conversion). Exercises every schema/values kind pairing
    !> that has a dedicated conversion branch in parquet_write.f90's
    !> parquet_append_as_schema_* family; the reversed-direction failure modes
    !> (overflow, non-integral) are covered separately as error_scenarios.
    subroutine test_write_cross_type_schema_coercion(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        ! Column name suffix "_from_<kind>" names the kind of the `values`
        ! array passed to parquet_write_column; the schema declares each
        ! column's data_type as the prefix (i32/i64/f32/f64) instead.
        integer(int64) :: src_i32_from_i64(3)
        real(real64)   :: src_i32_from_f64(3)
        real(real32)   :: src_i32_from_f32(3)
        integer(int32) :: src_i64_from_i32(3)
        real(real64)   :: src_i64_from_f64(3)
        real(real32)   :: src_i64_from_f32(3)
        integer(int32) :: src_f32_from_i32(3)
        integer(int64) :: src_f32_from_i64(3)
        real(real64)   :: src_f32_from_f64(3)
        integer(int32) :: src_f64_from_i32(3)
        integer(int64) :: src_f64_from_i64(3)
        real(real32)   :: src_f64_from_f32(3)
        integer(int32) :: i32_from_i64_back(3), i32_from_f64_back(3), i32_from_f32_back(3)
        integer(int64) :: i64_from_i32_back(3), i64_from_f64_back(3), i64_from_f32_back(3)
        real(real32)   :: f32_from_i32_back(3), f32_from_i64_back(3), f32_from_f64_back(3)
        real(real64)   :: f64_from_i32_back(3), f64_from_i64_back(3), f64_from_f32_back(3)
        character(len=*), parameter :: out_file = "test_run/test_write_cross_type_schema_coercion.parquet"

        src_i32_from_i64 = [1_int64, 2_int64, 3_int64]
        src_i32_from_f64 = [1.0_real64, 2.0_real64, 3.0_real64]
        src_i32_from_f32 = [1.0_real32, 2.0_real32, 3.0_real32]
        src_i64_from_i32 = [1_int32, 2_int32, 3_int32]
        src_i64_from_f64 = [1.0_real64, 2.0_real64, 3.0_real64]
        src_i64_from_f32 = [1.0_real32, 2.0_real32, 3.0_real32]
        src_f32_from_i32 = [1_int32, 2_int32, 3_int32]
        src_f32_from_i64 = [1_int64, 2_int64, 3_int64]
        src_f32_from_f64 = [1.0_real64, 2.0_real64, 3.0_real64]
        src_f64_from_i32 = [1_int32, 2_int32, 3_int32]
        src_f64_from_i64 = [1_int64, 2_int64, 3_int64]
        src_f64_from_f32 = [1.0_real32, 2.0_real32, 3.0_real32]

        call schema%init(table="cross_type_table")
        call schema%add_field("i32_from_i64", "int32")
        call schema%add_field("i32_from_f64", "int32")
        call schema%add_field("i32_from_f32", "int32")
        call schema%add_field("i64_from_i32", "int64")
        call schema%add_field("i64_from_f64", "int64")
        call schema%add_field("i64_from_f32", "int64")
        call schema%add_field("f32_from_i32", "float32")
        call schema%add_field("f32_from_i64", "float32")
        call schema%add_field("f32_from_f64", "float32")
        call schema%add_field("f64_from_i32", "float64")
        call schema%add_field("f64_from_i64", "float64")
        call schema%add_field("f64_from_f32", "float64")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "i32_from_i64", src_i32_from_i64)
        call parquet_write_column(writer, "i32_from_f64", src_i32_from_f64)
        call parquet_write_column(writer, "i32_from_f32", src_i32_from_f32)
        call parquet_write_column(writer, "i64_from_i32", src_i64_from_i32)
        call parquet_write_column(writer, "i64_from_f64", src_i64_from_f64)
        call parquet_write_column(writer, "i64_from_f32", src_i64_from_f32)
        call parquet_write_column(writer, "f32_from_i32", src_f32_from_i32)
        call parquet_write_column(writer, "f32_from_i64", src_f32_from_i64)
        call parquet_write_column(writer, "f32_from_f64", src_f32_from_f64)
        call parquet_write_column(writer, "f64_from_i32", src_f64_from_i32)
        call parquet_write_column(writer, "f64_from_i64", src_f64_from_i64)
        call parquet_write_column(writer, "f64_from_f32", src_f64_from_f32)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "i32_from_i64", i32_from_i64_back)
        call parquet_read_column(reader, "i32_from_f64", i32_from_f64_back)
        call parquet_read_column(reader, "i32_from_f32", i32_from_f32_back)
        call parquet_read_column(reader, "i64_from_i32", i64_from_i32_back)
        call parquet_read_column(reader, "i64_from_f64", i64_from_f64_back)
        call parquet_read_column(reader, "i64_from_f32", i64_from_f32_back)
        call parquet_read_column(reader, "f32_from_i32", f32_from_i32_back)
        call parquet_read_column(reader, "f32_from_i64", f32_from_i64_back)
        call parquet_read_column(reader, "f32_from_f64", f32_from_f64_back)
        call parquet_read_column(reader, "f64_from_i32", f64_from_i32_back)
        call parquet_read_column(reader, "f64_from_i64", f64_from_i64_back)
        call parquet_read_column(reader, "f64_from_f32", f64_from_f32_back)
        call parquet_close_reader(reader)

        call check(error, all(i32_from_i64_back == [1_int32, 2_int32, 3_int32]) .and. &
            all(i32_from_f64_back == [1_int32, 2_int32, 3_int32]) .and. &
            all(i32_from_f32_back == [1_int32, 2_int32, 3_int32]) .and. &
            all(i64_from_i32_back == [1_int64, 2_int64, 3_int64]) .and. &
            all(i64_from_f64_back == [1_int64, 2_int64, 3_int64]) .and. &
            all(i64_from_f32_back == [1_int64, 2_int64, 3_int64]) .and. &
            all(f32_from_i32_back == [1.0_real32, 2.0_real32, 3.0_real32]) .and. &
            all(f32_from_i64_back == [1.0_real32, 2.0_real32, 3.0_real32]) .and. &
            all(f32_from_f64_back == [1.0_real32, 2.0_real32, 3.0_real32]) .and. &
            all(f64_from_i32_back == [1.0_real64, 2.0_real64, 3.0_real64]) .and. &
            all(f64_from_i64_back == [1.0_real64, 2.0_real64, 3.0_real64]) .and. &
            all(f64_from_f32_back == [1.0_real64, 2.0_real64, 3.0_real64]), &
            "cross-type schema coercion did not round-trip one or more columns correctly")
    end subroutine test_write_cross_type_schema_coercion

    !> Existing is_valid/matrix write tests only exercise int32; this covers the
    !> float32 scalar-with-is_valid path and the float32 and boolean vector
    !> (matrix) write procedures (parquet_write_float32_matrix_column /
    !> parquet_write_logical_matrix_column), which were otherwise never written.
    !> Schema-less writer, so it exercises the non-schema-enforced branch of each.
    subroutine test_write_float32_boolean_vector_columns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real32) :: f32_scalar(3) = [1.5_real32, 2.5_real32, 3.5_real32]
        real(real32) :: f32_scalar_back(3)
        logical :: f32_valid_in(3) = [.true., .false., .true.]
        logical :: f32_valid_out(3)
        real(real32) :: f32_vec(2, 3), f32_vec_back(2, 3)
        logical :: bool_vec(2, 3), bool_vec_back(2, 3)
        logical :: bool_valid_in(2, 3), bool_valid_out(2, 3)
        character(len=*), parameter :: out_file = "test_run/test_write_f32_bool_vectors.parquet"

        f32_vec = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [2, 3])
        bool_vec = reshape([.true., .false., .true., .true., .false., .false.], [2, 3])
        bool_valid_in = .true.
        bool_valid_in(2, 3) = .false.

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "f32_scalar", f32_scalar, is_valid=f32_valid_in)
        call parquet_write_column(writer, "f32_vec", f32_vec)
        call parquet_write_column(writer, "bool_vec", bool_vec, is_valid=bool_valid_in)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "f32_scalar", f32_scalar_back, is_valid=f32_valid_out)
        call parquet_read_column(reader, "f32_vec", f32_vec_back)
        call parquet_read_column(reader, "bool_vec", bool_vec_back, is_valid=bool_valid_out)
        call parquet_close_reader(reader)

        call check(error, f32_valid_out(1) .and. (.not. f32_valid_out(2)) .and. f32_valid_out(3), &
            "float32 scalar is_valid mask did not round-trip")
        if (allocated(error)) return

        call check(error, abs(f32_scalar_back(1) - 1.5_real32) < 1.0e-5_real32 .and. &
            abs(f32_scalar_back(3) - 3.5_real32) < 1.0e-5_real32, &
            "float32 scalar values did not round-trip")
        if (allocated(error)) return

        call check(error, all(abs(f32_vec_back - f32_vec) < 1.0e-5_real32), &
            "float32 vector column did not round-trip")
        if (allocated(error)) return

        call check(error, all(bool_vec_back(:, 1:2) .eqv. bool_vec(:, 1:2)) .and. &
            bool_vec_back(1, 3) .eqv. bool_vec(1, 3), &
            "boolean vector column did not round-trip")
        if (allocated(error)) return

        call check(error, all(bool_valid_out(:, 1:2)) .and. bool_valid_out(1, 3) .and. &
            (.not. bool_valid_out(2, 3)), &
            "boolean vector column is_valid mask did not round-trip")
    end subroutine test_write_float32_boolean_vector_columns

    !> Writes int32/int64/float32/float64 *scalar* and int32/int64/float32/
    !> float64/boolean/string *vector* columns through a MAML schema (built via
    !> schema%init/add_field) with qc=.true., which exercises the schema-enforced
    !> + qc branches inside both the scalar and matrix writers for every numeric
    !> type -- previously unhit for int64 (never written under qc) and for the
    !> scalar-column qc paths (only vector columns were written under qc). Two
    !> writers cover both the with-is_valid and without-is_valid qc branches. The
    !> f64 scalar and the string column use strict >/< qc operators (rather than
    !> the default >=/<=), exercising the ">" and "<" cases of the numeric and
    !> string qc comparison helpers. Data is deliberately in range, so qc prints
    !> no warning (the qc check path still runs, which is what's being covered).
    subroutine test_write_vector_columns_schema_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: i32s(3)
        integer(int64) :: i64s(3)
        real(real32) :: f32s(3)
        real(real64) :: f64s(3)
        integer(int32) :: i32v(2, 3), i32v_back(2, 3)
        integer(int64) :: i64v(2, 3), i64v_back(2, 3)
        real(real32) :: f32v(2, 3)
        real(real64) :: f64v(2, 3)
        logical :: boolv(2, 3)
        character(len=8) :: strv(2, 3)
        logical :: valid_in(2, 3), svalid(3)
        character(len=*), parameter :: out_valid = "test_run/test_vector_schema_qc_valid.parquet"
        character(len=*), parameter :: out_novalid = "test_run/test_vector_schema_qc_novalid.parquet"

        i32s = [1_int32, 2_int32, 3_int32]
        i64s = [1_int64, 2_int64, 3_int64]
        f32s = [1.0_real32, 2.0_real32, 3.0_real32]
        f64s = [1.0_real64, 2.0_real64, 3.0_real64]
        i32v = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        i64v = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])
        f32v = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [2, 3])
        f64v = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
        boolv = reshape([.true., .false., .true., .false., .true., .false.], [2, 3])
        ! All strictly greater than "aa" and less than "zz" (strict qc bounds).
        strv = reshape(["ab", "bb", "cc", "dd", "ee", "ff"], [2, 3])
        valid_in = .true.
        svalid = .true.

        call schema%init(table="vector_qc_table")
        call schema%add_field("i32s", "int32", qc_min="0", qc_max="100")
        call schema%add_field("i64s", "int64", qc_min="0", qc_max="100")
        call schema%add_field("f32s", "float32", qc_min="0", qc_max="100")
        call schema%add_field("f64s", "float64", qc_min=">0", qc_max="<100")
        call schema%add_field("i32v", "int32", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("i64v", "int64", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("f32v", "float32", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("f64v", "float64", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("boolv", "boolean", col_size=2)
        call schema%add_field("strv", "string", col_size=2, array_size=8, qc_min=">aa", qc_max="<zz")
        call parquet_parse_maml(schema)

        ! Writer 1: every column written WITH is_valid -> the is_valid + qc
        ! (present(is_valid)) branch of each scalar/matrix writer.
        call parquet_open_writer(writer, out_valid, schema, qc=.true.)
        call parquet_write_column(writer, "i32s", i32s, is_valid=svalid)
        call parquet_write_column(writer, "i64s", i64s, is_valid=svalid)
        call parquet_write_column(writer, "f32s", f32s, is_valid=svalid)
        call parquet_write_column(writer, "f64s", f64s, is_valid=svalid)
        call parquet_write_column(writer, "i32v", i32v, is_valid=valid_in)
        call parquet_write_column(writer, "i64v", i64v, is_valid=valid_in)
        call parquet_write_column(writer, "f32v", f32v, is_valid=valid_in)
        call parquet_write_column(writer, "f64v", f64v, is_valid=valid_in)
        call parquet_write_column(writer, "boolv", boolv, is_valid=valid_in)
        call parquet_write_column(writer, "strv", strv, is_valid=valid_in)
        call parquet_close_writer(writer)

        ! Writer 2: every column written WITHOUT is_valid -> the else (no
        ! is_valid) qc branch of each scalar/matrix writer.
        call parquet_open_writer(writer, out_novalid, schema, qc=.true.)
        call parquet_write_column(writer, "i32s", i32s)
        call parquet_write_column(writer, "i64s", i64s)
        call parquet_write_column(writer, "f32s", f32s)
        call parquet_write_column(writer, "f64s", f64s)
        call parquet_write_column(writer, "i32v", i32v)
        call parquet_write_column(writer, "i64v", i64v)
        call parquet_write_column(writer, "f32v", f32v)
        call parquet_write_column(writer, "f64v", f64v)
        call parquet_write_column(writer, "boolv", boolv)
        call parquet_write_column(writer, "strv", strv)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_novalid)
        call parquet_read_column(reader, "i32v", i32v_back)
        call parquet_read_column(reader, "i64v", i64v_back)
        call parquet_close_reader(reader)

        call check(error, all(i32v_back == i32v) .and. all(i64v_back == i64v), &
            "vector columns written through a qc-enabled schema did not round-trip")
    end subroutine test_write_vector_columns_schema_qc

    !> Streaming row-group write, schema-enforced: "id" is written whole (parquet_write_column,
    !> before any row group opens), "big_vec" is written in two row groups of uneven size (3
    !> rows, then 2) via parquet_new_row_group/parquet_write_column_chunk/
    !> parquet_finish_row_group. Verifies both columns round-trip correctly, exercising the
    !> whole-column-sliced-per-row-group path (for "id") alongside the freshly-built-per-chunk
    !> path (for "big_vec").
    subroutine test_streaming_write_schema_enforced_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_streaming_schema_enforced.parquet"
        integer(int32) :: id_values(5), id_back(5)
        integer(int32) :: vec_values(3, 5), vec_back(3, 5)
        integer :: i

        id_values = [(i, i=1,5)]
        vec_values = reshape([(i, i=1,15)], [3, 5])

        call schema%init(table="streaming_table")
        call schema%add_field("id", "int32")
        call schema%add_field("big_vec", "int32", col_size=3)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id", id_values)

        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "big_vec", vec_values(:, 1:3))
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "big_vec", vec_values(:, 4:5))
        call parquet_finish_row_group(writer)

        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "id", id_back)
        call parquet_read_column(reader, "big_vec", vec_back)
        call parquet_close_reader(reader)

        call check(error, all(id_back == id_values) .and. all(vec_back == vec_values), &
            "streaming-written schema-enforced columns did not round-trip correctly")
    end subroutine test_streaming_write_schema_enforced_roundtrip

    !> Same shape as test_streaming_write_schema_enforced_roundtrip, above, but for a
    !> schema-less writer -- exercises check_column_chunk_write_preconditions' other branch
    !> (finding/registering a column by name in `fields` directly, since column_metadata stays
    !> empty for a schema-less writer).
    subroutine test_streaming_write_schemaless_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_streaming_schemaless.parquet"
        integer(int32) :: id_values(5), id_back(5)
        integer(int32) :: vec_values(3, 5), vec_back(3, 5)
        integer :: i

        id_values = [(i, i=1,5)]
        vec_values = reshape([(i, i=1,15)], [3, 5])

        call parquet_open_writer(writer, out_file, chunk_size=3)
        call parquet_write_column(writer, "id", id_values)

        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "big_vec", vec_values(:, 1:3))
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "big_vec", vec_values(:, 4:5))
        call parquet_finish_row_group(writer)

        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "id", id_back)
        call parquet_read_column(reader, "big_vec", vec_back)
        call parquet_close_reader(reader)

        call check(error, all(id_back == id_values) .and. all(vec_back == vec_values), &
            "streaming-written schema-less columns did not round-trip correctly")
    end subroutine test_streaming_write_schemaless_roundtrip

    !> A file whose only row group covers every row (no whole columns at all) -- the simplest
    !> possible streaming shape, and the one closest to how a single-row-group auto-sized batch
    !> write behaves.
    subroutine test_streaming_write_single_row_group_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_streaming_single_row_group.parquet"
        integer(int32) :: vec_values(2, 4), vec_back(2, 4)
        integer :: i

        vec_values = reshape([(i, i=1,8)], [2, 4])

        call parquet_open_writer(writer, out_file, chunk_size=4)
        call parquet_new_row_group(writer, 4)
        call parquet_write_column_chunk(writer, "v", vec_values)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", vec_back)
        call parquet_close_reader(reader)

        call check(error, all(vec_back == vec_values), &
            "a single-row-group streamed column did not round-trip correctly")
    end subroutine test_streaming_write_single_row_group_roundtrip

    !> String and logical (boolean) chunked columns, both scalar and vector forms, round-trip
    !> across two row groups -- these two types take different code paths in
    !> parquet_write_*_column_chunk (bool8 conversion; string packing + always-large_utf8) from
    !> the numeric types already covered above.
    subroutine test_streaming_write_string_logical_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_streaming_string_logical.parquet"
        character(len=8) :: names(4), names_back(4)
        character(len=8) :: tags(2, 4), tags_back(2, 4)
        logical :: flags(4), flags_back(4)

        names = [character(len=8) :: "alpha", "bravo", "charlie", "delta"]
        tags = reshape([character(len=8) :: "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8"], [2, 4])
        flags = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, out_file, chunk_size=2)

        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "name", names(1:2))
        call parquet_write_column_chunk(writer, "tag", tags(:, 1:2))
        call parquet_write_column_chunk(writer, "flag", flags(1:2))
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "name", names(3:4))
        call parquet_write_column_chunk(writer, "tag", tags(:, 3:4))
        call parquet_write_column_chunk(writer, "flag", flags(3:4))
        call parquet_finish_row_group(writer)

        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", names_back)
        call parquet_read_column(reader, "tag", tags_back)
        call parquet_read_column(reader, "flag", flags_back)
        call parquet_close_reader(reader)

        call check(error, all(names_back == names) .and. all(tags_back == tags) .and. all(flags_back .eqv. flags), &
            "streamed string/logical columns did not round-trip correctly")
    end subroutine test_streaming_write_string_logical_roundtrip

    !> parquet_get_chunk_size(writer) must return a usable positive value both before any data
    !> is written (schema-based estimate) and once streaming is under way (locked-in value).
    !! Also exercises the int32-kind specific (parquet_get_chunk_size_writer_int32) alongside the
    !! int64 one, and the int64-kind specific of parquet_new_row_group.
    subroutine test_streaming_get_chunk_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=*), parameter :: out_file = "test_run/test_streaming_get_chunk_size.parquet"
        integer(int64) :: chunk_size_before, chunk_size_during
        integer(int32) :: chunk_size_before32
        integer(int32) :: v(2, 2)

        v = reshape([1, 2, 3, 4], [2, 2])

        call schema%init(table="chunk_size_table")
        call schema%add_field("v", "int32", col_size=2)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_get_chunk_size(writer, chunk_size_before)
        call parquet_get_chunk_size(writer, chunk_size_before32)

        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "v", v)
        call parquet_get_chunk_size(writer, chunk_size_during)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call check(error, chunk_size_before > 0 .and. chunk_size_during > 0 .and. chunk_size_before32 > 0, &
            "parquet_get_chunk_size(writer) did not return a positive value")
    end subroutine test_streaming_get_chunk_size

    !> Covers every parquet_write_column_chunk type/shape specific across a SCHEMA-ENFORCED,
    !! qc-enabled writer: int32/int64/float32/float64 scalar+matrix, logical scalar+matrix, and
    !! string scalar+matrix -- including the schema-enforced branch of logical-scalar/string
    !! chunk writes (only ever exercised schema-less elsewhere, by
    !! test_streaming_write_string_logical_roundtrip) and the is_valid-present/absent branches
    !! (row group 1 passes is_valid=, row group 2 omits it) under qc=.true.. Two uneven row
    !! groups (3 rows, then 1) so every chunked column also exercises a real multi-row-group
    !! split, not just a single chunk.
    subroutine test_streaming_write_all_types_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_streaming_all_types.parquet"
        integer(int32) :: i32s(4), i32s_back(4)
        integer(int32) :: i32v(2, 4), i32v_back(2, 4)
        integer(int64) :: i64s(4), i64s_back(4)
        integer(int64) :: i64v(2, 4), i64v_back(2, 4)
        real(real32) :: f32s(4), f32s_back(4)
        real(real32) :: f32v(2, 4), f32v_back(2, 4)
        real(real64) :: f64s(4), f64s_back(4)
        real(real64) :: f64v(2, 4), f64v_back(2, 4)
        logical :: logv(2, 4), logv_back(2, 4)
        logical :: logs(4), logs_back(4)
        character(len=8) :: strs(4), strs_back(4)
        character(len=8) :: strv(2, 4), strv_back(2, 4)
        logical :: valid_s(4), valid_m(2, 4)
        integer :: i

        i32s = [(i, i=1,4)]
        i32v = reshape([(i, i=1,8)], [2, 4])
        i64s = [(int(i, kind=int64), i=5,8)]
        i64v = reshape([(int(i, kind=int64), i=1,8)], [2, 4])
        f32s = [(real(i, kind=real32), i=1,4)]
        f32v = reshape([(real(i, kind=real32), i=1,8)], [2, 4])
        f64s = [(real(i, kind=real64), i=5,8)]
        f64v = reshape([(real(i, kind=real64), i=1,8)], [2, 4])
        logv = reshape([.true., .false., .false., .true., .true., .true., .false., .false.], [2, 4])
        logs = [.true., .false., .true., .false.]
        strs = [character(len=8) :: "alpha", "bravo", "charlie", "delta"]
        strv = reshape([character(len=8) :: "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8"], [2, 4])
        valid_s = .true.
        valid_m = .true.

        call schema%init(table="streaming_all_types_table")
        call schema%add_field("i32s", "int32", qc_min="0", qc_max="100")
        call schema%add_field("i32v", "int32", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("i64s", "int64", qc_min="0", qc_max="100")
        call schema%add_field("i64v", "int64", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("f32s", "float32", qc_min="0", qc_max="100")
        call schema%add_field("f32v", "float32", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("f64s", "float64", qc_min="0", qc_max="100")
        call schema%add_field("f64v", "float64", col_size=2, qc_min="0", qc_max="100")
        call schema%add_field("logv", "boolean", col_size=2)
        call schema%add_field("logs", "boolean")
        call schema%add_field("strs", "string", array_size=8, qc_min=">aa", qc_max="<zz")
        call schema%add_field("strv", "string", col_size=2, array_size=8, qc_min=">aa", qc_max="<zz")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema, qc=.true.)

        call parquet_new_row_group(writer, 3_int64)
        call parquet_write_column_chunk(writer, "i32s", i32s(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "i32v", i32v(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_write_column_chunk(writer, "i64s", i64s(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "i64v", i64v(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_write_column_chunk(writer, "f32s", f32s(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "f32v", f32v(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_write_column_chunk(writer, "f64s", f64s(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "f64v", f64v(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_write_column_chunk(writer, "logv", logv(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_write_column_chunk(writer, "logs", logs(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "strs", strs(1:3), is_valid=valid_s(1:3))
        call parquet_write_column_chunk(writer, "strv", strv(:, 1:3), is_valid=valid_m(:, 1:3))
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "i32s", i32s(4:4))
        call parquet_write_column_chunk(writer, "i32v", i32v(:, 4:4))
        call parquet_write_column_chunk(writer, "i64s", i64s(4:4))
        call parquet_write_column_chunk(writer, "i64v", i64v(:, 4:4))
        call parquet_write_column_chunk(writer, "f32s", f32s(4:4))
        call parquet_write_column_chunk(writer, "f32v", f32v(:, 4:4))
        call parquet_write_column_chunk(writer, "f64s", f64s(4:4))
        call parquet_write_column_chunk(writer, "f64v", f64v(:, 4:4))
        call parquet_write_column_chunk(writer, "logv", logv(:, 4:4))
        call parquet_write_column_chunk(writer, "logs", logs(4:4))
        call parquet_write_column_chunk(writer, "strs", strs(4:4))
        call parquet_write_column_chunk(writer, "strv", strv(:, 4:4))
        call parquet_finish_row_group(writer)

        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "i32s", i32s_back)
        call parquet_read_column(reader, "i32v", i32v_back)
        call parquet_read_column(reader, "i64s", i64s_back)
        call parquet_read_column(reader, "i64v", i64v_back)
        call parquet_read_column(reader, "f32s", f32s_back)
        call parquet_read_column(reader, "f32v", f32v_back)
        call parquet_read_column(reader, "f64s", f64s_back)
        call parquet_read_column(reader, "f64v", f64v_back)
        call parquet_read_column(reader, "logv", logv_back)
        call parquet_read_column(reader, "logs", logs_back)
        call parquet_read_column(reader, "strs", strs_back)
        call parquet_read_column(reader, "strv", strv_back)
        call parquet_close_reader(reader)

        call check(error, all(i32s_back == i32s) .and. all(i32v_back == i32v) .and. all(i64s_back == i64s) .and. &
            all(i64v_back == i64v) .and. &
            all(f32s_back == f32s) .and. all(f32v_back == f32v) .and. all(f64s_back == f64s) .and. &
            all(f64v_back == f64v) .and. all(logv_back .eqv. logv) .and. all(logs_back .eqv. logs) .and. &
            all(strs_back == strs) .and. all(strv_back == strv), &
            "streamed int32/int64/float32/float64/logical/string scalar+matrix columns did not round-trip")
    end subroutine test_streaming_write_all_types_roundtrip

    !> parquet_write_string_column_chunk (the rank-1 "values(:)" chunk-write entry point) supports
    !> a col_size>1 column too, packing a flat array in (element varies fastest, then row) order
    !> -- a separate branch from parquet_write_string_matrix_column_chunk's rank-2 "values(:,:)"
    !> form, which every other chunked-string test in this suite uses instead. Exercises that
    !> flat-array branch directly and checks the round-trip against a schema col_size=2 column.
    subroutine test_streaming_write_string_flat_vector_chunk(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/streaming_string_flat_vector_chunk.parquet"
        character(len=8) :: flat(6), back(2, 3)
        logical :: ok

        flat = [character(len=8) :: "a1", "a2", "b1", "b2", "c1", "c2"]

        call schema%init(table="string_flat_vector_chunk_table")
        call schema%add_field("strv", "string", col_size=2, array_size=8)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_new_row_group(writer, 3_int64)
        call parquet_write_column_chunk(writer, "strv", flat)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "strv", back)
        call parquet_close_reader(reader)

        ok = trim(back(1, 1)) == "a1" .and. trim(back(2, 1)) == "a2" .and. &
             trim(back(1, 2)) == "b1" .and. trim(back(2, 2)) == "b2" .and. &
             trim(back(1, 3)) == "c1" .and. trim(back(2, 3)) == "c2"

        call check(error, ok, &
            "chunk write of a col_size>1 string column via a flat rank-1 array did not round-trip")
    end subroutine test_streaming_write_string_flat_vector_chunk

    subroutine test_write_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(test_output_type), allocatable :: test_data(:)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_parquet.parquet"
        integer :: cmdstat

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call schema%add_metadata("creator", "Parquet Fortran Test", "Description 1")
        call schema%add_metadata("PI", 3.14, "Description 1", fmt='F6.1')
        call schema%add_metadata("PI2", 3.14_rk, fmt='F0.2', description="Description 2")
        call schema%add_metadata("row_count", 20_int32, "Description 3")
        call schema%add_metadata("row_count_long", 20_int64, "Description 4")
        call schema%add_metadata("is_test", .true.)

        call schema%add_metadata("arrint", [0,1,2], "Array int")
        call schema%add_metadata("arrint64", [1_int64,2_int64])
        call schema%add_metadata("arreal", [1.1,1.0,2.0], "Array real")
        call schema%add_metadata("arrreal64", [1.1_rk,1.0_rk,2.0_rk], description="Array real64", fmt='F0.1')
        call schema%add_metadata("arrlogical", [.true., .false., .true.], description="Array logical")
        call schema%add_metadata("arrstring", ["one  ","two  ","three"], "Array string")

        call init_test_data(test_data, 20)
        call write_test_data(out_file, test_data, schema)

        inquire(file=out_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "expected output parquet file was not created")
            return
        end if
        !
    end subroutine test_write_parquet_file

    subroutine write_test_data(filename, data, schema, write_maml)
        character(len=*), intent(in) :: filename
        type(test_output_type), dimension(:), intent(in) :: data
        type(parquet_schema), intent(in) :: schema
        logical, intent(in), optional :: write_maml
        type(parquet_writer) :: writer
        integer :: i, j, n, name_len_max
        integer(int64), allocatable :: idarr_col(:)
        character(len=:), allocatable :: name_col(:)
        character(len=4), allocatable :: name_arr_col(:)
        real(real32), allocatable :: arr_col(:)
        real(real64), allocatable :: arrlong_col(:)
        integer(int32), allocatable :: iarr_col(:)
        logical, allocatable :: flag_arr_col(:)

        if (size(schema%cinfo%col) < 13) error stop "write_test_data: expected at least 13 columns in schema"

        n = size(data)
        allocate(idarr_col(n*2), name_arr_col(n*3), arr_col(n*5), arrlong_col(n*5), iarr_col(n*3), flag_arr_col(n*6))

        name_len_max = max(1, maxval([(len_trim(adjustl(data(i)%name)) + 4, i=1,n)]))
        allocate(character(len=name_len_max) :: name_col(n))

        do i = 1, n
            do j = 1, 2
                idarr_col((i-1)*2 + j) = data(i)%idarr(j)
            end do

            do j = 1, 5
                arr_col((i-1)*5 + j) = data(i)%arr(j)
                arrlong_col((i-1)*5 + j) = data(i)%arrlong(j)
            end do

            do j = 1, 3
                name_arr_col((i-1)*3 + j) = data(i)%name_arr(j)
                iarr_col((i-1)*3 + j) = data(i)%iarr(j)
            end do

            do j = 1, 6
                flag_arr_col((i-1)*6 + j) = data(i)%flag_arr(j)
            end do
        end do

        do i = 1, n
            name_col(i) = "var_" // trim(adjustl(data(i)%name))
        end do

        call parquet_open_writer(writer, filename, schema, write_maml=write_maml)

        call parquet_write_column(writer, schema%cinfo%col(8)%name, arr_col)
        call parquet_write_column(writer, "id0", data(:)%id)
        call parquet_write_column(writer, schema%cinfo%col(12)%name, data(:)%flag)
        call parquet_write_column(writer, schema%cinfo%col(2)%name, idarr_col)
        call parquet_write_column(writer, "name", name_col)
        call parquet_write_column(writer, schema%cinfo%col(4)%name, name_arr_col)
        call parquet_write_column(writer, schema%cinfo%col(13)%name, flag_arr_col)
        call parquet_write_column(writer, schema%cinfo%col(7)%name, data(:)%value2)
        call parquet_write_column(writer, schema%cinfo%col(5)%name, data(:)%idlong)
        call parquet_write_column(writer, schema%cinfo%col(6)%name, data(:)%value)
        call parquet_write_column(writer, schema%cinfo%col(10)%name, data(:)%val)
        call parquet_write_column(writer, schema%cinfo%col(11)%name, iarr_col)
        call parquet_write_column(writer, schema%cinfo%col(9)%name, arrlong_col)

        call parquet_close_writer(writer)
    end subroutine write_test_data

    subroutine init_test_data(test_data, n)
        type(test_output_type), allocatable, intent(out) :: test_data(:)
        integer, intent(in) :: n
        integer :: i, j

        allocate(test_data(1:n))

        do i = 1, n
            test_data(i)%id = i
            test_data(i)%idarr = [int(i*10 + 1, kind=int64), int(i*10 + 2, kind=int64)]
            write(test_data(i)%name, '(A,I3)') "Obj", i
            do j = 1, 3
                write(test_data(i)%name_arr(j), '(A,I1)') "N", j
            end do
            test_data(i)%idlong = int(i*1000, kind=int64)
            test_data(i)%value = real(i*10.0, kind=real32)
            test_data(i)%value2 = real(i*20.0, kind=rk)
            test_data(i)%arr = [(real(j+i, kind=real32), j=1,5)]
            test_data(i)%arrlong = [(real(j+i*10, kind=real64), j=1,5)]
            test_data(i)%val = real(i*0.1, kind=real32)
            test_data(i)%iarr = [(i+j, j=1,3)]
            test_data(i)%flag = mod(i,2) == 0
            test_data(i)%flag_arr = [(mod(i+j,2) == 0, j=1,6)]
        end do
    end subroutine init_test_data
    !
    !> Compact (parquet_string_column) write/read round-trip, including a Null and an empty
    !> string. Deliberately shortest element first, then longer ones (see CLAUDE.md's
    !> "Regression tests for 'sized/typed from the first element' bugs" guidance) -- not that the
    !> compact path has any fixed-width sizing to get wrong, but this keeps the fixture honest.
    subroutine test_compact_string_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_string_column) :: col, back
        character(len=:), allocatable :: s
        character(len=*), parameter :: out_file = "test_run/test_compact_string_roundtrip.parquet"

        call col%append_string("")
        call col%append_string("a much longer second value")
        call col%append_null()
        call col%append_string("third")

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "name", col)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)

        call check(error, back%size() == 4_int64, "compact string round-trip: wrong row count")
        if (allocated(error)) return
        call check(error, back%null_count() == 1_int64, "compact string round-trip: wrong null count")
        if (allocated(error)) return
        call check(error, (.not. back%is_null(1)) .and. back%is_empty(1), "compact string round-trip: row 1 (empty)")
        if (allocated(error)) return
        call back%get(2, s)
        call check(error, s == "a much longer second value", "compact string round-trip: row 2 content")
        if (allocated(error)) return
        call check(error, back%is_null(3), "compact string round-trip: row 3 should be Null")
        if (allocated(error)) return
        call back%get(4, s)
        call check(error, s == "third", "compact string round-trip: row 4 content")
    end subroutine test_compact_string_roundtrip

    !> A file is fully interchangeable between the two Fortran-side string representations --
    !> both are just Parquet BYTE_ARRAY on disk (see doc/pages/string-columns.md's "Reading and
    !> writing compact string columns"): a compact write must read back correctly via the padded
    !> character(len=...) path, and a padded write must read back correctly via the compact path.
    subroutine test_compact_string_cross_path_compat(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_string_column) :: col, back
        character(len=5) :: padded_values(3)
        character(len=16) :: padded_back(3)
        character(len=:), allocatable :: s
        character(len=*), parameter :: file1 = "test_run/test_compact_write_padded_read.parquet"
        character(len=*), parameter :: file2 = "test_run/test_padded_write_compact_read.parquet"

        ! compact write -> padded read
        call col%append_string("alpha")
        call col%append_string("beta")
        call col%append_null()

        call parquet_open_writer(writer, file1)
        call parquet_write_column(writer, "name", col)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, file1)
        call parquet_read_column(reader, "name", padded_back, null_value="NA")
        call parquet_close_reader(reader)

        call check(error, trim(padded_back(1)) == "alpha" .and. trim(padded_back(2)) == "beta" .and. &
            trim(padded_back(3)) == "NA", "compact write not readable via the padded path")
        if (allocated(error)) return

        ! padded write -> compact read
        padded_values = ["gamma", "delta", "     "]

        call parquet_open_writer(writer, file2)
        call parquet_write_column(writer, "name", padded_values, is_valid=[.true., .true., .false.])
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, file2)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)

        call check(error, back%size() == 3_int64, "padded write not readable via the compact path (size)")
        if (allocated(error)) return
        call back%get(1, s)
        call check(error, s == "gamma", "padded write not readable via the compact path (row 1)")
        if (allocated(error)) return
        call check(error, back%is_null(3), "padded write not readable via the compact path (row 3 Null)")
    end subroutine test_compact_string_cross_path_compat

    !> Chunked write (parquet_new_row_group/parquet_write_column_chunk/parquet_finish_row_group)
    !> of two parquet_string_column row groups, read back both chunk-by-chunk (parquet_read_
    !> column_chunk) and as a whole column (parquet_read_column, which forces combine_column_
    !> chunks' multi-chunk arrow::Concatenate path in parquet_wrapper.cpp, since the file now has
    !> more than one row group).
    subroutine test_compact_string_chunked_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_string_column) :: chunk1, chunk2, back, whole
        character(len=:), allocatable :: s
        character(len=*), parameter :: out_file = "test_run/test_compact_string_chunked.parquet"
        integer(int64) :: row_group2

        call chunk1%append_string("row1")
        call chunk1%append_null()
        call chunk2%append_string("row3")
        call chunk2%append_string("row4 longer value")

        call parquet_open_writer(writer, out_file)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "name", chunk1)
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "name", chunk2)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        ! row_group=1 as a plain (int32) literal exercises the _rg32 specific.
        call parquet_read_column_chunk(reader, "name", 1, back)
        call check(error, back%size() == 2_int64 .and. (.not. back%is_null(1)) .and. back%is_null(2), &
            "compact chunked read: row group 1 mismatch")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        ! row_group=2 as an explicit integer(int64) exercises the _rg64 specific.
        row_group2 = 2_int64
        call parquet_read_column_chunk(reader, "name", row_group2, back)
        call back%get(2, s)
        call check(error, back%size() == 2_int64 .and. s == "row4 longer value", &
            "compact chunked read: row group 2 mismatch")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        call parquet_read_column(reader, "name", whole)
        call parquet_close_reader(reader)

        call check(error, whole%size() == 4_int64, "compact whole-column read after chunked write: wrong size")
        if (allocated(error)) return
        call whole%get(1, s)
        call check(error, s == "row1", "compact whole-column read after chunked write: row 1")
        if (allocated(error)) return
        call check(error, whole%null_count() == 1_int64, "compact whole-column read after chunked write: null count")
    end subroutine test_compact_string_chunked_roundtrip

    !> A qc: min:/max: violation on a compact string write warns (prints to stdout) but must not
    !> abort the write or corrupt the file -- exercises parquet_check_qc_string_compact's
    !> has_qc_min and has_qc_max branches (both the per-row check and the bounds_desc message
    !> building) together, since both are declared here. Doesn't assert on the printed WARNING
    !> text itself (that formatting is already covered by the padded path's own qc string tests);
    !> this only proves the compact write's qc call site is wired up and non-fatal.
    subroutine test_compact_string_qc_warning(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_string_column) :: col, back
        character(len=*), parameter :: out_file = "test_run/test_compact_string_qc_warning.parquet"

        call schema%init(table="compact_qc_table")
        call schema%add_field("name", "string", qc_min="a", qc_max="m")
        call parquet_parse_maml(schema)

        call col%append_string("apple")
        call col%append_string("zebra") ! lexicographically > "m" -> qc violation (WARNING only)

        call parquet_open_writer(writer, out_file, schema, qc=.true.)
        call parquet_write_column(writer, "name", col)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)

        call check(error, back%size() == 2_int64, &
            "compact string qc violation (WARNING only) should not abort the write or corrupt the data")
    end subroutine test_compact_string_qc_warning

    !> A reader opened with an active row filter transparently narrows a compact string read to
    !> just the matching rows, exercising apply_filter_mask's arrow::compute::Filter branch
    !> through the new buffer-extraction read path (see extract_string_buffers's own comment on
    !> why this matters: Filter always produces a fresh, offset()==0 array).
    subroutine test_compact_string_filter_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_string_column) :: col, back
        integer(int32) :: id(4)
        character(len=:), allocatable :: s
        character(len=*), parameter :: out_file = "test_run/test_compact_string_filter.parquet"
        integer :: i

        id = [(i, i=1,4)]
        call col%append_string("one")
        call col%append_string("two")
        call col%append_string("three")
        call col%append_string("four")

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "name", col)
        call parquet_close_writer(writer)

        call filt%add("id > 2")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)

        call check(error, back%size() == 2_int64, "compact string read under a row filter: wrong row count")
        if (allocated(error)) return
        call back%get(1, s)
        call check(error, s == "three", "compact string read under a row filter: row 1 content")
        if (allocated(error)) return
        call back%get(2, s)
        call check(error, s == "four", "compact string read under a row filter: row 2 content")
    end subroutine test_compact_string_filter_read
    !
end module test_writing