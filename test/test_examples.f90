!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Compiles and runs the code examples shown in README.md, so that a change to
!> the library's public API that silently breaks a documented example is
!> caught here rather than by a user copy-pasting the README.
!>
!> Each subroutine below mirrors one README example as closely as possible
!> (same `use` clauses, same variable declarations, same calls), except that
!> it writes to a path under test_run/ instead of the README's "data.parquet",
!> and is wrapped as a subroutine instead of a standalone `program` so it can
!> be driven by test-drive and checked with a round-trip read-back.
module test_examples
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_examples
    !
contains
    !
    subroutine collect_tests_parquet_examples(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("README minimal writer/reader example", test_readme_minimal_example), &
            new_unittest("README MAML-schema writer example", test_readme_maml_schema_writer_example), &
            new_unittest("README combined example", test_readme_combined_example), &
            new_unittest("maml_example2 writer produces a matching sidecar .maml", &
                test_maml_example2_sidecar_keyarray) &
            ]
    end subroutine collect_tests_parquet_examples

    !> "Minimal writer example" + "Minimal reader example"
    !> (README sections "Writing parquet files..." / "Reading parquet files...")
    subroutine test_readme_minimal_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_minimal_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id(:)

        call readme_write_parquet_example(out_file)

        ! README "Minimal reader example", reading the file written above.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)

        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)

        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. id(1) == 1_int32 .and. id(3) == 3_int32, &
            "README minimal writer/reader example did not round-trip the 'id' column correctly")
    end subroutine test_readme_minimal_example

    subroutine readme_write_parquet_example(out_file)
        character(len=*), intent(in) :: out_file
        type(parquet_writer) :: writer
        integer(int32) :: id(3)
        real(real64) :: value(3)

        id = [1_int32, 2_int32, 3_int32]
        value = [10.0_real64, 20.0_real64, 30.0_real64]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "value", value)
        call parquet_close_writer(writer)
    end subroutine readme_write_parquet_example

    !> The "explicit column definitions and table metadata" MAML-schema
    !> snippet from the README's "Writing parquet files..." section.
    subroutine test_readme_maml_schema_writer_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_maml_schema_example.parquet"
        type(parquet_writer) :: writer
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(parquet_reader) :: reader
        integer(int32) :: id0(3)
        integer(int32), allocatable :: id0_read(:)
        integer(int64) :: nrows

        ! README: call parquet_read_maml("maml_example.maml", cinfo, metadata)
        call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)

        ! Only write the one column this test provides data for.
        call cinfo%set_unavailable()
        call cinfo%set_available("id0")

        id0 = [1_int32, 2_int32, 3_int32]

        ! README: call parquet_open_writer(writer, "data.parquet", cinfo, metadata)
        call parquet_open_writer(writer, out_file, cinfo, metadata)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0), &
            "README MAML-schema writer example did not round-trip the 'id0' column correctly")
    end subroutine test_readme_maml_schema_writer_example

    !> README "Combined example: MAML schema, matrices and metadata".
    !> Mirrors the program's use clause and declarations verbatim (this is
    !> what caught a missing `int64` import in an earlier version of the
    !> README example).
    subroutine test_readme_combined_example(error)
        use iso_fortran_env, only: int32, int64, real64
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(parquet_writer) :: writer
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        integer(int32) :: id0(3)
        integer(int64) :: idarr(2, 3)   ! (col_size, nrows) for the "idarr" vector column

        character(len=*), parameter :: out_file = "test_run/readme_combined_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id0_read(:)
        integer(int64), allocatable :: idarr_read(:,:)

        ! Parse column definitions + table metadata from the MAML file.
        call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)

        ! This schema defines more columns than we have data for in this example;
        ! disable everything, then re-enable only the columns we are about to write.
        call cinfo%set_unavailable()
        call cinfo%set_available("id0")
        call cinfo%set_available("idarr")

        ! Add an extra, run-time-only piece of metadata not present in the MAML file.
        call metadata%add_metadata("generated_by", "write_parquet_combined_example")

        id0 = [1_int32, 2_int32, 3_int32]
        idarr = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])

        call parquet_open_writer(writer, out_file, cinfo, metadata)
        call parquet_write_column(writer, "id0", id0)
        call parquet_write_column(writer, "idarr", idarr)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows), idarr_read(2, nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_read_column(reader, "idarr", idarr_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0) .and. all(idarr_read == idarr), &
            "README combined example did not round-trip the 'id0'/'idarr' columns correctly")
    end subroutine test_readme_combined_example
    !
    !> Writes a two-column ("id", "name") table using docs/maml_example2.maml's
    !> schema, with write_maml=.true. so a sidecar .maml is produced alongside
    !> the parquet file. Checks that the two keyarray: entries already present
    !> in that source MAML ("test_url"/"test_url2") come through correctly in
    !> both the in-memory metadata and the written sidecar file.
    subroutine test_maml_example2_sidecar_keyarray(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata, sidecar_metadata
        type(parquet_column_info) :: sidecar_cinfo
        integer(int32) :: id(3)
        character(len=24) :: name(3)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/maml_example2.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/maml_example2.maml"

        call parquet_read_maml("docs/maml_example2.maml", cinfo, metadata)

        call check_keyarray_entries(error, metadata, "in-memory metadata parsed from docs/maml_example2.maml")
        if (allocated(error)) return

        id = [1_int32, 2_int32, 3_int32]
        name = ["Alice", "Bob  ", "Carol"]

        call parquet_open_writer(writer, out_file, cinfo, metadata, write_maml=.true.)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "name", name)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_read_maml(sidecar_file, sidecar_cinfo, sidecar_metadata)
        call check_keyarray_entries(error, sidecar_metadata, "metadata reparsed from the written sidecar .maml")
    end subroutine test_maml_example2_sidecar_keyarray

    subroutine check_keyarray_entries(error, metadata, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata), intent(in) :: metadata
        character(len=*), intent(in) :: context
        integer :: i
        logical :: found_url, found_url2

        found_url = .false.
        found_url2 = .false.

        do i = 1, size(metadata%items)
            if (trim(metadata%items(i)%key) == "test_url") then
                found_url = .true.
                call check(error, trim(metadata%items(i)%value) == "//example.com/data #new", &
                    "unexpected value for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
            else if (trim(metadata%items(i)%key) == "test_url2") then
                found_url2 = .true.
                call check(error, trim(metadata%items(i)%value) == "http://example.com/data :#new", &
                    "unexpected value for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
            end if
        end do

        call check(error, found_url, "keyarray entry 'test_url' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_url2, "keyarray entry 'test_url2' not found (" // context // ")")
    end subroutine check_keyarray_entries
    !
end module test_examples
