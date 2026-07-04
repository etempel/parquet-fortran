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
                test_col_map_write_renames_output_column) &
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