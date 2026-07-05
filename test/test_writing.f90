!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module test_writing
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
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
            new_unittest("add_metadata after parquet_read_maml is reflected in the sidecar", &
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
            new_unittest("parquet_prefetch_columns still allows reading a non-prefetched column", &
                test_prefetch_columns_then_read_non_prefetched), &
            new_unittest("use_threads=.false. on writer and reader still round-trips", &
                test_use_threads_false_still_round_trips), &
            new_unittest("parquet_set_max_threads with a valid value does not break a round-trip", &
                test_set_max_threads_valid_value_does_not_break_round_trip) &
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
        type(parquet_column_info) :: cinfo, sidecar_cinfo
        type(parquet_table_metadata) :: metadata, sidecar_metadata
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml.maml"

        call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)
        call cinfo%set_unavailable()
        call cinfo%set_available("id0")

        call parquet_open_writer(writer, out_file, cinfo, metadata, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_read_maml(sidecar_file, sidecar_cinfo, sidecar_metadata)

        call check(error, size(sidecar_cinfo%col) == 1, &
            "expected the sidecar .maml to only list the one enabled column ('id0')")
        if (allocated(error)) return
        call check(error, trim(sidecar_cinfo%col(1)%name) == "id0", &
            "expected the sidecar .maml's only field entry to be 'id0'")
        if (allocated(error)) return

        ! Non-field content (table-level metadata, keyarray) must be untouched.
        call check(error, size(sidecar_metadata%items) == size(metadata%items), &
            "pruning disabled fields should not affect table-level metadata items")
    end subroutine test_write_maml_sidecar

    !> When every column in cinfo stays enabled, nothing should be pruned:
    !> the sidecar should still match the source MAML line-for-line.
    subroutine test_write_maml_sidecar_no_pruning_when_all_enabled(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(parquet_maml_file) :: source_maml, sidecar_maml
        type(test_output_type), allocatable :: test_data(:)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml_full.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml_full.maml"
        integer :: i

        call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)
        call init_test_data(test_data, 5)
        call write_test_data(out_file, test_data, cinfo, metadata, write_maml=.true.)

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
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata, sidecar_metadata
        type(parquet_column_info) :: sidecar_cinfo
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_write_maml_runtime.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_write_maml_runtime.maml"
        integer :: i
        logical :: found

        call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)
        call cinfo%set_unavailable()
        call cinfo%set_available("id0")

        ! Added after parquet_read_maml: should now be reflected in the sidecar.
        call metadata%add_metadata("generated_by", "unit_test", "added at runtime")

        call parquet_open_writer(writer, out_file, cinfo, metadata, write_maml=.true.)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_read_maml(sidecar_file, sidecar_cinfo, sidecar_metadata)

        found = .false.
        do i = 1, size(sidecar_metadata%items)
            if (trim(sidecar_metadata%items(i)%key) == "generated_by") then
                call check(error, trim(sidecar_metadata%items(i)%value) == "unit_test", &
                    "sidecar keyarray entry for 'generated_by' has an unexpected value")
                if (allocated(error)) return
                call check(error, trim(sidecar_metadata%items(i)%description) == "added at runtime", &
                    "sidecar keyarray entry for 'generated_by' has an unexpected comment")
                if (allocated(error)) return
                found = .true.
                exit
            end if
        end do

        call check(error, found, &
            "add_metadata call made after parquet_read_maml was not reflected in the written sidecar .maml")
        if (allocated(error)) return

        ! The keyarray entries already present in the source MAML must still be there too.
        found = .false.
        do i = 1, size(sidecar_metadata%items)
            if (trim(sidecar_metadata%items(i)%key) == "test_scalar") found = .true.
        end do
        call check(error, found, &
            "pre-existing keyarray entries from the source MAML were lost when appending runtime metadata")
    end subroutine test_write_maml_sidecar_with_runtime_metadata
    !
    subroutine test_add_metadata_inserts_before_extra(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(parquet_maml_file) :: maml
        integer :: idx_keyarray, idx_extra, i

        ! Built in-memory (rather than loaded from docs/) so this test does not
        ! depend on whether the on-disk fixture happens to have a keyarray:
        ! block already: this specifically covers the "no existing keyarray:,
        ! but an extra: section is present" case.
        maml%name = "no_keyarray_with_extra.maml"
        maml%lines = [character(len=40) :: &
            "table: no_keyarray_table", &
            "extra:", &
            "  anything:", &
            "    like: 1.3", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32" ]

        call parquet_read_maml(maml, cinfo, metadata)

        call metadata%add_metadata("added_key", "42", "added comment")

        call check(error, allocated(metadata%source_maml_lines), &
            "expected source_maml_lines to be populated after parquet_read_maml")
        if (allocated(error)) return

        idx_keyarray = 0
        idx_extra = 0
        do i = 1, size(metadata%source_maml_lines)
            if (metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(metadata%source_maml_lines(i))) == "keyarray:") idx_keyarray = i
            if (metadata%source_maml_lines(i)(1:1) /= " " .and. &
                trim(adjustl(metadata%source_maml_lines(i))) == "extra:") idx_extra = i
        end do

        call check(error, idx_keyarray > 0, "expected a synthesized 'keyarray:' header in source_maml_lines")
        if (allocated(error)) return
        call check(error, idx_extra > 0, "expected the pre-existing 'extra:' header to still be present")
        if (allocated(error)) return
        call check(error, idx_keyarray < idx_extra, &
            "expected the synthesized 'keyarray:' block to be inserted before the existing 'extra:' section")
        if (allocated(error)) return

        call check(error, trim(adjustl(metadata%source_maml_lines(idx_keyarray+1))) == "- key: added_key", &
            "expected the new keyarray entry right after the synthesized header")
        if (allocated(error)) return
        call check(error, trim(adjustl(metadata%source_maml_lines(idx_keyarray+2))) == "value: 42", &
            "unexpected value line for the new keyarray entry")
        if (allocated(error)) return
        call check(error, trim(adjustl(metadata%source_maml_lines(idx_keyarray+3))) == "comment: added comment", &
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
        type(parquet_maml_file) :: base_maml, user_maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata, sidecar_metadata
        type(parquet_column_info) :: sidecar_cinfo
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        integer(int32), allocatable :: id_read(:)
        integer(int64) :: nrows
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_col_map.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/test_col_map.maml"

        base_maml = get_parquet_maml("maml_example.maml")

        user_maml%name = "user_col_map_write.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - id0: my_id", &
            "fields:", &
            "- name: my_id", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
        call parquet_read_maml(user_maml, cinfo, metadata)

        ! parquet_write_column is called with the internal name ("id0"), not
        ! the renamed output name ("my_id").
        call parquet_open_writer(writer, out_file, cinfo, metadata, write_maml=.true.)
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

        call parquet_read_maml(sidecar_file, sidecar_cinfo, sidecar_metadata)
        call check(error, size(sidecar_cinfo%col) == 1 .and. trim(sidecar_cinfo%col(1)%name) == "id0" .and. &
            trim(sidecar_cinfo%col(1)%output_name) == "my_id", &
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
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer(int32) :: read_back(3)
        character(len=*), parameter :: out_file = "test_run/test_protected_no_null.parquet"

        maml%name = "protected_no_null.maml"
        maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
        call parquet_read_maml(maml, cinfo, metadata)

        call check(error, cinfo%col(1)%is_protected, "expected column 'a' to be marked is_protected")
        if (allocated(error)) return

        call parquet_open_writer(writer, out_file, cinfo, metadata)
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
        type(parquet_maml_file) :: maml_semicolon, maml_dashlist
        type(parquet_column_info) :: cinfo_semicolon, cinfo_dashlist
        type(parquet_table_metadata) :: metadata

        maml_semicolon%name = "protected_semicolon.maml"
        maml_semicolon%lines = [character(len=40) :: &
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

        maml_dashlist%name = "protected_dashlist.maml"
        maml_dashlist%lines = [character(len=40) :: &
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

        call parquet_validate_maml(maml_semicolon)
        call parquet_validate_maml(maml_dashlist)
        call parquet_read_maml(maml_semicolon, cinfo_semicolon, metadata)
        call parquet_read_maml(maml_dashlist, cinfo_dashlist, metadata)

        call check(error, cinfo_semicolon%col(1)%is_protected .and. cinfo_semicolon%col(2)%is_protected .and. &
            (.not. cinfo_semicolon%col(3)%is_protected), &
            "semicolon-form protected_cols did not mark the expected columns")
        if (allocated(error)) return

        call check(error, cinfo_dashlist%col(1)%is_protected .and. cinfo_dashlist%col(2)%is_protected .and. &
            (.not. cinfo_dashlist%col(3)%is_protected), &
            "dash-list-form protected_cols did not mark the expected columns")
    end subroutine test_protected_cols_semicolon_and_dash_list_equivalent

    !> qc: min:/max: with a plain number (no operator) default to inclusive
    !> (">="/"<=" respectively); a quoted value with an explicit operator
    !> prefix (">=", "<=", ">", "<") uses that operator instead.
    subroutine test_qc_parsing_plain_and_operator_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata

        maml%name = "qc_parsing.maml"
        maml%lines = [character(len=40) :: &
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

        call parquet_validate_maml(maml)
        call parquet_read_maml(maml, cinfo, metadata)

        call check(error, cinfo%col(1)%has_qc_min .and. cinfo%col(1)%has_qc_max .and. &
            trim(cinfo%col(1)%qc_min_op) == ">=" .and. trim(cinfo%col(1)%qc_max_op) == "<=" .and. &
            trim(cinfo%col(1)%qc_min_raw) == "1" .and. trim(cinfo%col(1)%qc_max_raw) == "1000", &
            "plain-number qc: min/max did not default to inclusive operators with the expected bound text")
        if (allocated(error)) return

        call check(error, cinfo%col(2)%has_qc_min .and. cinfo%col(2)%has_qc_max .and. &
            trim(cinfo%col(2)%qc_min_op) == ">=" .and. trim(cinfo%col(2)%qc_max_op) == "<" .and. &
            trim(cinfo%col(2)%qc_min_raw) == "0" .and. trim(cinfo%col(2)%qc_max_raw) == "360", &
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

        call check_scenario_exit_status_local(error, "write_unknown_compression", expect_abort=.true., &
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
    !> row count (capped at 500000) instead of the old fixed default of 1024;
    !> a table smaller than the cap must still round-trip in a single row group.
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

    !> Mirrors test_errors.f90's check_scenario_exit_status, duplicated here
    !> (rather than exposed from test_errors) since it's test-module-private
    !> plumbing, not part of that module's public collect_tests_* interface.
    subroutine check_scenario_exit_status_local(error, scenario, expect_abort, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message
        logical, intent(in) :: expect_abort
        integer :: exitstat, cmdstat
        logical :: aborted

        ! --features thread_safe forced regardless of how the outer `fpm
        ! test` was invoked -- see the identical note on
        ! check_scenario_exit_status in test_errors.f90.
        call execute_command_line( &
            "fpm test error_scenarios --features thread_safe -- "//trim(scenario)//" > /dev/null 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
    end subroutine check_scenario_exit_status_local

    subroutine test_write_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(test_output_type), allocatable :: test_data(:)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_parquet.parquet"
        integer :: cmdstat

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        maml = get_parquet_maml("maml_example.maml")
        call parquet_read_maml(maml, cinfo, metadata)

        call metadata%add_metadata("creator", "Parquet Fortran Test", "Description 1")
        call metadata%add_metadata("PI", 3.14, "Description 1", fmt='F6.1')
        call metadata%add_metadata("PI2", 3.14_rk, fmt='F0.2', description="Description 2")
        call metadata%add_metadata("row_count", 20_int32, "Description 3")
        call metadata%add_metadata("row_count_long", 20_int64, "Description 4")
        call metadata%add_metadata("is_test", .true.)

        call metadata%add_metadata("arrint", [0,1,2], "Array int")
        call metadata%add_metadata("arrint64", [1_int64,2_int64])
        call metadata%add_metadata("arreal", [1.1,1.0,2.0], "Array real")
        call metadata%add_metadata("arrreal64", [1.1_rk,1.0_rk,2.0_rk], description="Array real64", fmt='F0.1')
        call metadata%add_metadata("arrlogical", [.true., .false., .true.], description="Array logical")
        call metadata%add_metadata("arrstring", ["one  ","two  ","three"], "Array string")

        call init_test_data(test_data, 20)
        call write_test_data(out_file, test_data, cinfo, metadata)

        inquire(file=out_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "expected output parquet file was not created")
            return
        end if
        !
    end subroutine test_write_parquet_file

    subroutine write_test_data(filename, data, col, tmeta, write_maml)
        character(len=*), intent(in) :: filename
        type(test_output_type), dimension(:), intent(in) :: data
        type(parquet_column_info), intent(in) :: col
        type(parquet_table_metadata), intent(in), optional :: tmeta
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

        if (size(col%col) < 13) error stop "write_test_data: expected at least 13 columns in col"

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

        call parquet_open_writer(writer, filename, col, metadata=tmeta, write_maml=write_maml)

        call parquet_write_column(writer, col%col(8)%name, arr_col)
        call parquet_write_column(writer, "id0", data(:)%id)
        call parquet_write_column(writer, col%col(12)%name, data(:)%flag)
        call parquet_write_column(writer, col%col(2)%name, idarr_col)
        call parquet_write_column(writer, "name", name_col)
        call parquet_write_column(writer, col%col(4)%name, name_arr_col)
        call parquet_write_column(writer, col%col(13)%name, flag_arr_col)
        call parquet_write_column(writer, col%col(7)%name, data(:)%value2)
        call parquet_write_column(writer, col%col(5)%name, data(:)%idlong)
        call parquet_write_column(writer, col%col(6)%name, data(:)%value)
        call parquet_write_column(writer, col%col(10)%name, data(:)%val)
        call parquet_write_column(writer, col%col(11)%name, iarr_col)
        call parquet_write_column(writer, col%col(9)%name, arrlong_col)

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