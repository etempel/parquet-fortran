!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!
module parquet
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use parquet_bindings
    use parquet_maml, only: parquet_maml_file
    implicit none
    !
    character(len=*),parameter:: cversion = "v0.2 (2026-06-30)" !< version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

    type parquet_column_info
        logical :: is_set = .false.
        character(len=:), allocatable :: name
        character(len=:), allocatable :: unit
        character(len=:), allocatable :: info
        character(len=:), allocatable :: ucd
        character(len=:), allocatable :: data_type
        integer :: array_size
    end type parquet_column_info

    type parquet_metadata_entry
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    end type parquet_metadata_entry

    type parquet_table_metadata
        type(parquet_metadata_entry), allocatable :: items(:)
    contains
        procedure :: add_metadata_int32
        procedure :: add_metadata_int64
        procedure :: add_metadata_float32
        procedure :: add_metadata_float64
        procedure :: add_metadata_logical
        procedure :: add_metadata_string
        generic :: add_metadata => add_metadata_int32, add_metadata_int64, add_metadata_float32, &
                                    add_metadata_float64, add_metadata_logical, add_metadata_string
    end type parquet_table_metadata

    type parquet_writer
        type(c_ptr) :: handle = c_null_ptr
        type(parquet_column_info), allocatable :: all_columns(:)
        type(parquet_column_info), allocatable :: enabled_columns(:)
        integer, allocatable :: write_counts(:)
        logical :: enforce_schema = .false.
    end type parquet_writer

    type parquet_reader
        type(c_ptr) :: handle = c_null_ptr
    end type parquet_reader

    interface parquet_write_column
        module procedure parquet_write_int32_column
        module procedure parquet_write_int64_column
        module procedure parquet_write_float32_column
        module procedure parquet_write_float64_column
        module procedure parquet_write_logical_column
        module procedure parquet_write_string_column
    end interface parquet_write_column

    interface parquet_read_maml
        module procedure parquet_read_maml_file
        module procedure parquet_read_maml_internal
    end interface parquet_read_maml

    interface parquet_read_column
        module procedure parquet_read_int32_column_1d
        module procedure parquet_read_int64_column_1d
        module procedure parquet_read_float32_column_1d
        module procedure parquet_read_float64_column_1d
        module procedure parquet_read_logical_column_1d
        module procedure parquet_read_string_column_1d
        module procedure parquet_read_int32_array_full
        module procedure parquet_read_int64_array_full
        module procedure parquet_read_float32_array_full
        module procedure parquet_read_float64_array_full
        module procedure parquet_read_logical_array_full
        module procedure parquet_read_string_array_full
    end interface parquet_read_column

    public :: parquet_writer
    public :: parquet_reader
    public :: parquet_column_info
    public :: parquet_table_metadata
    public :: parquet_open_writer
    public :: parquet_write_column
    public :: parquet_close_writer
    public :: get_parquet_fortran_version
    public :: parquet_read_maml
    public :: parquet_open_reader
    public :: parquet_close_reader
    public :: parquet_get_nrows
    public :: parquet_get_column_array_size_read
    public :: parquet_get_column_total_elements
    public :: parquet_get_string_length
    public :: parquet_read_column
    public :: parquet_read_int32_array_row_mode
    public :: parquet_read_int64_array_row_mode
    public :: parquet_read_float32_array_row_mode
    public :: parquet_read_float64_array_row_mode
    public :: parquet_read_logical_array_row_mode
    public :: parquet_read_string_array_row_mode
    public :: parquet_read_int32_array_element_mode
    public :: parquet_read_int64_array_element_mode
    public :: parquet_read_float32_array_element_mode
    public :: parquet_read_float64_array_element_mode
    public :: parquet_read_logical_array_element_mode
    public :: parquet_read_string_array_element_mode

    interface
        module integer function parquet_get_enabled_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_enabled_column_index

        module integer function parquet_get_defined_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_defined_column_index

        module logical function parquet_is_type_compatible(actual_type, expected_type)
            character(len=*), intent(in) :: actual_type
            character(len=*), intent(in) :: expected_type
        end function parquet_is_type_compatible

        module subroutine parquet_assert_column_type(writer, name, expected_type)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: expected_type
        end subroutine parquet_assert_column_type

        module subroutine parquet_mark_column_written(writer, name)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
        end subroutine parquet_mark_column_written

        module logical function parquet_is_column_enabled(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_is_column_enabled

        module integer function parquet_get_column_array_size(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_column_array_size

        module subroutine parquet_open_writer(writer, filename, cinfo, metadata)
            type(parquet_writer), intent(out) :: writer
            character(len=*), intent(in) :: filename
            type(parquet_column_info), intent(in), optional :: cinfo(:)
            type(parquet_table_metadata), intent(in), optional :: metadata
        end subroutine parquet_open_writer

        module subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: unit
            character(len=*), intent(in) :: description
            character(len=*), intent(in) :: ucd
            character(len=*), intent(in) :: data_type
            integer, intent(in) :: array_size
        end subroutine parquet_add_column_info

        module subroutine parquet_write_int32_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int32), intent(in) :: data(:)
        end subroutine parquet_write_int32_column

        module subroutine parquet_write_int64_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: data(:)
        end subroutine parquet_write_int64_column

        module subroutine parquet_write_float32_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real32), intent(in) :: data(:)
        end subroutine parquet_write_float32_column

        module subroutine parquet_write_float64_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real64), intent(in) :: data(:)
        end subroutine parquet_write_float64_column

        module subroutine parquet_write_logical_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            logical, intent(in) :: data(:)
        end subroutine parquet_write_logical_column

        module subroutine parquet_write_string_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: data(:)
        end subroutine parquet_write_string_column

        module subroutine parquet_close_writer(writer)
            type(parquet_writer), intent(inout) :: writer
        end subroutine parquet_close_writer

        module subroutine parquet_read_maml_file(maml_filename, cinfo, metadata)
            character(len=*), intent(in) :: maml_filename
            type(parquet_column_info), allocatable, intent(out) :: cinfo(:)
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_read_maml_file

        module subroutine parquet_read_maml_internal(maml, cinfo, metadata)
            type(parquet_maml_file), intent(in) :: maml
            type(parquet_column_info), allocatable, intent(out) :: cinfo(:)
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_read_maml_internal

        module subroutine parquet_parse_maml_lines(lines, cinfo, metadata)
            character(len=*), intent(in) :: lines(:)
            type(parquet_column_info), allocatable, intent(out) :: cinfo(:)
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_parse_maml_lines

        module subroutine parquet_append_line(lines, n, line)
            character(len=1024), allocatable, intent(inout) :: lines(:)
            integer, intent(in) :: n
            character(len=*), intent(in) :: line
        end subroutine parquet_append_line

        module subroutine parquet_metadata_append_entry(metadata, key, value)
            class(parquet_table_metadata), intent(inout) :: metadata
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value
        end subroutine parquet_metadata_append_entry

        module subroutine add_metadata_int32(this, key, val)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: val
        end subroutine add_metadata_int32

        module subroutine add_metadata_int64(this, key, val)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: val
        end subroutine add_metadata_int64

        module subroutine add_metadata_float32(this, key, val, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: val
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float32

        module subroutine add_metadata_float64(this, key, val, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: val
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float64

        module subroutine add_metadata_logical(this, key, val)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: val
        end subroutine add_metadata_logical

        module subroutine add_metadata_string(this, key, val)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: val
        end subroutine add_metadata_string

        module subroutine parquet_append_empty_cinfo(cinfo, n)
            type(parquet_column_info), allocatable, intent(inout) :: cinfo(:)
            integer, intent(inout) :: n
        end subroutine parquet_append_empty_cinfo

        module subroutine parquet_split_key_value(line, key, value)
            character(len=*), intent(in) :: line
            character(len=:), allocatable, intent(out) :: key
            character(len=:), allocatable, intent(out) :: value
        end subroutine parquet_split_key_value

        module function parquet_unquote(s) result(out)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: out
        end function parquet_unquote

        module function parquet_to_lower(s) result(out)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: out
        end function parquet_to_lower

        module subroutine parquet_open_reader(reader, filename)
            type(parquet_reader), intent(out) :: reader
            character(len=*), intent(in) :: filename
        end subroutine parquet_open_reader

        module subroutine parquet_close_reader(reader)
            type(parquet_reader), intent(inout) :: reader
        end subroutine parquet_close_reader

        module subroutine parquet_get_nrows(reader, nrows)
            type(parquet_reader), intent(in) :: reader
            integer(int64), intent(out) :: nrows
        end subroutine parquet_get_nrows

        module subroutine parquet_get_column_array_size_read(reader, name, array_size)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer, intent(out) :: array_size
        end subroutine parquet_get_column_array_size_read

        module subroutine parquet_get_column_total_elements(reader, name, nelem)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: nelem
        end subroutine parquet_get_column_total_elements

        module subroutine parquet_get_string_length(reader, name, strlen_max)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer, intent(out) :: strlen_max
        end subroutine parquet_get_string_length

        module subroutine parquet_read_int32_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
        end subroutine parquet_read_int32_column_1d

        module subroutine parquet_read_int64_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
        end subroutine parquet_read_int64_column_1d

        module subroutine parquet_read_float32_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
        end subroutine parquet_read_float32_column_1d

        module subroutine parquet_read_float64_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
        end subroutine parquet_read_float64_column_1d

        module subroutine parquet_read_logical_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
        end subroutine parquet_read_logical_column_1d

        module subroutine parquet_read_string_column_1d(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
        end subroutine parquet_read_string_column_1d

        module subroutine parquet_read_int32_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:, :)
        end subroutine parquet_read_int32_array_full

        module subroutine parquet_read_int64_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:, :)
        end subroutine parquet_read_int64_array_full

        module subroutine parquet_read_float32_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:, :)
        end subroutine parquet_read_float32_array_full

        module subroutine parquet_read_float64_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:, :)
        end subroutine parquet_read_float64_array_full

        module subroutine parquet_read_logical_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:, :)
        end subroutine parquet_read_logical_array_full

        module subroutine parquet_read_string_array_full(reader, name, values)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:, :)
        end subroutine parquet_read_string_array_full

        module subroutine parquet_read_int32_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_int32_array_row_mode

        module subroutine parquet_read_int64_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_int64_array_row_mode

        module subroutine parquet_read_float32_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_float32_array_row_mode

        module subroutine parquet_read_float64_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_float64_array_row_mode

        module subroutine parquet_read_logical_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_logical_array_row_mode

        module subroutine parquet_read_string_array_row_mode(reader, name, values, row_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
            integer, intent(in) :: row_index
        end subroutine parquet_read_string_array_row_mode

        module subroutine parquet_read_int32_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_int32_array_element_mode

        module subroutine parquet_read_int64_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_int64_array_element_mode

        module subroutine parquet_read_float32_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_float32_array_element_mode

        module subroutine parquet_read_float64_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_float64_array_element_mode

        module subroutine parquet_read_logical_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_logical_array_element_mode

        module subroutine parquet_read_string_array_element_mode(reader, name, values, col_index)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
            integer, intent(in) :: col_index
        end subroutine parquet_read_string_array_element_mode
    end interface

contains

    function get_parquet_fortran_version() result(ver_string)
        implicit none
        character (len=:), allocatable :: ver_string
        integer :: i
        !
! Accept solution from https://stackoverflow.com/questions/31649691/stringify-macro-with-gnu-gfortran
! which provides the easiest way to pass a macro to a string in Fortran complying with both
! gfortran traditional cpp and the standard cpp syntaxes
#ifdef __GFORTRAN__
#  define STRINGIFY_START(X) "&
#  define STRINGIFY_END(X) &X"
#else
#  define STRINGIFY_(X) #X
#  define STRINGIFY_START(X) &
#  define STRINGIFY_END(X) STRINGIFY_(X)
#endif

        ver_string = STRINGIFY_START(RELEASE_VERSION)
        STRINGIFY_END(RELEASE_VERSION)
        !
        i = index(cversion, " ")
        !
        if (cversion(2:i-1) /= ver_string) then
            write(*,*) "WARNING: using developmentparquet-fortran library!"
            write(*,*) "         library version: ", trim(cversion)
            write(*,*) "         RELEASE_VERSION: ", trim(ver_string)
        end if
        !
    end function get_parquet_fortran_version

end module
