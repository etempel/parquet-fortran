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
        check_scenario_exit_status_and_no_output
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
            new_unittest("qc=.true. prints a WARNING for an out-of-range string value", &
                test_qc_warning_printed_for_string_violation), &
            new_unittest("qc: on a boolean field is accepted but never enforced", &
                test_qc_silently_ignored_for_boolean), &
            new_unittest("compression=gzip round-trips and shrinks a compressible file", &
                test_compression_gzip_round_trip), &
            new_unittest("compression=uncompressed writes a larger file than the snappy default", &
                test_compression_uncompressed_larger_than_default), &
            new_unittest("an unknown compression codec aborts", test_compression_unknown_aborts), &
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
            new_unittest("parquet_close_reader(print_stat=.true.) does not disturb a normal close", &
                test_close_reader_print_stat_smoke), &
            new_unittest("qc: range violation prints a WARNING but does not abort", &
                test_qc_range_violation_warns), &
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
            new_unittest("use_threads=.false. on writer and reader still round-trips", &
                test_use_threads_false_still_round_trips), &
            new_unittest("parquet_set_max_threads with a valid value does not break a round-trip", &
                test_set_max_threads_valid_value_does_not_break_round_trip), &
            new_unittest("writing a numeric kind that differs from the schema's declared data_type " // &
                "converts to match it", test_write_cross_type_schema_coercion), &
            new_unittest("float32 scalar (with is_valid) and float32/boolean vector columns round-trip", &
                test_write_float32_boolean_vector_columns), &
            new_unittest("vector columns written through a qc-enabled schema round-trip (with/without is_valid)", &
                test_write_vector_columns_schema_qc) &
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

        call parquet_parse_maml("docs/maml_example.maml", schema)
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

        call parquet_parse_maml("docs/maml_example.maml", schema)
        call init_test_data(test_data, 5)
        call write_test_data(out_file, test_data, schema, write_maml=.true.)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        source_maml = parquet_load_maml_file("docs/maml_example.maml")
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

        call parquet_parse_maml("docs/maml_example.maml", schema)
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
    subroutine test_add_metadata_inserts_before_extra(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: idx_keyarray, idx_extra, i

        ! Built in-memory (rather than loaded from docs/) so this test does not
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

    subroutine test_qc_warning_printed_for_string_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat

        call check_qc_scenario_warns(error, "qc_warning_string", "s", exitstat, cmdstat)
        if (allocated(error)) return

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range string value should not error stop (warning only)")
    end subroutine test_qc_warning_printed_for_string_violation

    subroutine test_qc_silently_ignored_for_boolean(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=*), parameter :: out_file = "test_run/qc_boolean_output.txt"
        logical :: found_warning

        call execute_command_line("mkdir -p test_run", wait=.true.)
        call execute_command_line( &
            "fpm test error_scenarios -- qc_silently_ignored_for_boolean > " // out_file // " 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
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

        call execute_command_line("mkdir -p test_run", wait=.true.)
        call execute_command_line( &
            "fpm test error_scenarios -- " // trim(scenario_name) // " > " // out_file // " 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)
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
        character(len=256) :: values(2000)
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

    !> A "wide" row (a vector column with a large col_size) can reach the
    !> ~256 MiB row-group byte target at well under a million rows -- a flat
    !> row-count-based auto-sizing heuristic would never split a table this
    !> small into multiple row groups, but a byte-size-aware one should.
    !> This only checks the round-trip still comes back correct; the actual
    !> row-group count/size was verified manually against a standalone
    !> parquet::ParquetFileReader inspector during development (2 row groups
    !> of ~256 MiB and ~132 MiB for 50,000 rows x col_size=1000 int64).
    subroutine test_chunk_size_auto_sizing_wide_row(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer, parameter :: nrows = 50000
        integer, parameter :: col_size = 1000
        integer(int64), allocatable :: wide(:, :), wide_read(:, :)
        integer(int64) :: n
        character(len=*), parameter :: out_file = "test_run/test_chunk_size_auto_wide.parquet"
        integer :: i, j

        allocate(wide(col_size, nrows), wide_read(col_size, nrows))
        do i = 1, nrows
            do j = 1, col_size
                wide(j, i) = int(i, kind=int64) * 10000_int64 + j
            end do
        end do

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "wide", wide)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, n)
        call parquet_read_column(reader, "wide", wide_read)
        call parquet_close_reader(reader)

        call check(error, n == int(nrows, kind=int64) .and. all(wide_read == wide), &
            "auto-sized chunk_size for a wide vector column did not round-trip the written data correctly")
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
        call check(error, nrows32 == 6_int32, "parquet_open_reader(nrows=) with an integer(int32) actual did not return the row count")
        call parquet_close_reader(reader)
        if (allocated(error)) return

        call parquet_open_reader(reader, out_file, nrows=nrows_default)
        call check(error, nrows_default == 6, &
            "parquet_open_reader(nrows=) with a plain default-INTEGER actual did not return the row count")
        call parquet_close_reader(reader)
    end subroutine test_open_reader_nrows_arg

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
end module test_writing