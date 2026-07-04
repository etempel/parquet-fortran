!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module parquet
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use parquet_bindings
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    implicit none
    private
    !
    character(len=*),parameter:: cversion = "v0.3.3 (2026-07-04)" !< version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

    type parquet_column_type
        logical :: is_set = .false.
        logical :: deactivated = .false. ! true for columns merged in from a base MAML that the user's MAML excluded;
                                          ! protects is_set from being changed by set_available/set_unavailable (bulk or by name).
        character(len=:), allocatable :: name      ! The name of the field [required]; always the internal/canonical
                                                    ! name, i.e. what parquet_write_column/set_available/etc. use --
                                                    ! never affected by a col_map: rename (see output_name).
        character(len=:), allocatable :: unit      ! The unit of measurement for the field.
        character(len=:), allocatable :: info      ! A short description of the field.
        character(len=:), allocatable :: ucd       ! Unified Content Descriptor for IVOA (can have many).
        character(len=:), allocatable :: data_type ! The data type of the field [required].
        integer :: array_size = 1 ! Maximum length of character strings.
        integer :: col_size = 1   ! The number of elements in the vector column.
        character(len=:), allocatable :: output_name ! The name actually written to the parquet file/VOTable header.
                                                       ! Equal to `name` unless a col_map: entry in the MAML that
                                                       ! declared this field renamed it (col_map: maps
                                                       ! internal_name -> output_name; `name` is then set to the
                                                       ! internal_name and `output_name` keeps the field's own
                                                       ! declared name from that MAML's fields: section).
    end type parquet_column_type

    type parquet_column_info
        type(parquet_column_type), allocatable :: col(:)
    contains
        procedure :: get_column_index
        procedure :: set_unavailable
        procedure :: set_available
    end type parquet_column_info

    type parquet_metadata_entry
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        character(len=:), allocatable :: description
    end type parquet_metadata_entry

    type parquet_table_metadata
        type(parquet_metadata_entry), allocatable :: items(:)
        ! Verbatim source MAML lines, populated by parquet_read_maml; used by
        ! parquet_open_writer(..., write_maml=.true.) to save a sidecar .maml
        ! file next to the .parquet output. add_metadata calls made after
        ! parquet_read_maml append a new keyarray: entry to these lines (see
        ! parquet_append_keyarray_line), so the sidecar reflects them; it is
        ! NOT kept in sync with which columns end up enabled/written, though.
        character(len=:), allocatable :: source_maml_lines(:)
    contains
        procedure :: add_metadata_int32
        procedure :: add_metadata_int64
        procedure :: add_metadata_float32
        procedure :: add_metadata_float64
        procedure :: add_metadata_logical
        procedure :: add_metadata_string
        procedure :: add_metadata_int32_array
        procedure :: add_metadata_int64_array
        procedure :: add_metadata_float32_array
        procedure :: add_metadata_float64_array
        procedure :: add_metadata_logical_array
        procedure :: add_metadata_string_array
        generic :: add_metadata => add_metadata_int32, add_metadata_int64, add_metadata_float32, &
                                    add_metadata_float64, add_metadata_logical, add_metadata_string, &
                                    add_metadata_int32_array, add_metadata_int64_array, add_metadata_float32_array, &
                                    add_metadata_float64_array, add_metadata_logical_array, add_metadata_string_array
    end type parquet_table_metadata

    ! parquet_writer/parquet_reader own a handle to a C++-side Arrow/Parquet
    ! object with no automatic Fortran cleanup. Always prefer an explicit
    ! parquet_close_writer/parquet_close_reader call; the FINAL procedures
    ! below are only a safety net for a handle that's still open when its
    ! variable goes out of scope or is overwritten (e.g. an early RETURN
    ! between open and close), not a substitute for closing normally.
    !
    ! Do not copy a parquet_writer/parquet_reader (`w2 = w1`, passing one as
    ! a function result, etc.): the handle is a plain c_ptr, so a copy
    ! aliases the same underlying C++ object without any reference counting.
    ! Whichever copy is finalized/closed first frees it out from under the
    ! other, which would then double-free/use-after-free when it is itself
    ! later closed or finalized. Always use a single named writer/reader,
    ! passed by reference (as every procedure in this module already does).
    type parquet_writer
        type(c_ptr) :: handle = c_null_ptr
        type(parquet_column_type), allocatable :: all_columns(:)
        type(parquet_column_type), allocatable :: enabled_columns(:)
        integer, allocatable :: write_counts(:)
        logical :: enforce_schema = .false.
    contains
        final :: parquet_writer_finalize
    end type parquet_writer

    type parquet_reader
        type(c_ptr) :: handle = c_null_ptr
    contains
        final :: parquet_reader_finalize
    end type parquet_reader

    interface parquet_write_column
        module procedure parquet_write_int32_column
        module procedure parquet_write_int32_matrix_column
        module procedure parquet_write_int64_column
        module procedure parquet_write_int64_matrix_column
        module procedure parquet_write_float32_column
        module procedure parquet_write_float32_matrix_column
        module procedure parquet_write_float64_column
        module procedure parquet_write_float64_matrix_column
        module procedure parquet_write_logical_column
        module procedure parquet_write_logical_matrix_column
        module procedure parquet_write_string_column
        module procedure parquet_write_string_matrix_column
    end interface parquet_write_column

    interface parquet_read_maml
        module procedure parquet_read_maml_file
        module procedure parquet_read_maml_internal
    end interface parquet_read_maml

    !> Validates either a parquet_maml_file (already loaded, e.g. via
    !> parquet_load_maml_file or built in memory) or a MAML filename (loaded
    !> from disk first). See parquet_validate_maml_internal/_file for behavior.
    interface parquet_validate_maml
        module procedure parquet_validate_maml_internal
        module procedure parquet_validate_maml_file
    end interface parquet_validate_maml

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

    interface parquet_read_array_row_mode
        module procedure parquet_read_int32_array_row_mode
        module procedure parquet_read_int64_array_row_mode
        module procedure parquet_read_float32_array_row_mode
        module procedure parquet_read_float64_array_row_mode
        module procedure parquet_read_logical_array_row_mode
        module procedure parquet_read_string_array_row_mode
    end interface parquet_read_array_row_mode

    interface parquet_read_array_element_mode
        module procedure parquet_read_int32_array_element_mode
        module procedure parquet_read_int64_array_element_mode
        module procedure parquet_read_float32_array_element_mode
        module procedure parquet_read_float64_array_element_mode
        module procedure parquet_read_logical_array_element_mode
        module procedure parquet_read_string_array_element_mode
    end interface parquet_read_array_element_mode

    interface parquet_get_nrows
        module procedure parquet_get_nrows_int64
        module procedure parquet_get_nrows_int32
    end interface parquet_get_nrows

    interface parquet_get_column_total_elements
        module procedure parquet_get_column_total_elements_int64
        module procedure parquet_get_column_total_elements_int32
    end interface parquet_get_column_total_elements

    public :: parquet_writer
    public :: parquet_reader
    public :: parquet_column_info
    public :: parquet_column_type
    public :: parquet_table_metadata
    public :: parquet_maml_file
    public :: parquet_open_writer
    public :: parquet_write_column
    public :: parquet_close_writer
    public :: get_parquet_fortran_version
    public :: parquet_read_maml
    public :: parquet_load_maml_file
    public :: parquet_validate_maml
    public :: parquet_validate_user_maml
    public :: parquet_open_reader
    public :: parquet_close_reader
    public :: parquet_get_nrows
    public :: parquet_get_col_size
    public :: parquet_get_column_total_elements
    public :: parquet_get_string_length
    public :: parquet_read_column
    public :: parquet_read_array_row_mode
    public :: parquet_read_array_element_mode

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

        module integer function parquet_get_column_col_size(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_column_col_size

        module integer function parquet_get_column_array_size(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_column_array_size

        module subroutine parquet_open_writer(writer, filename, cinfo, metadata, write_maml)
            type(parquet_writer), intent(out) :: writer
            character(len=*), intent(in) :: filename
            type(parquet_column_info), intent(in), optional :: cinfo
            type(parquet_table_metadata), intent(in), optional :: metadata
            logical, intent(in), optional :: write_maml
        end subroutine parquet_open_writer

        module subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size, col_size)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: unit
            character(len=*), intent(in) :: description
            character(len=*), intent(in) :: ucd
            character(len=*), intent(in) :: data_type
            integer, intent(in) :: array_size
            integer, intent(in) :: col_size
        end subroutine parquet_add_column_info

        module subroutine parquet_write_int32_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int32), intent(in) :: data(:)
        end subroutine parquet_write_int32_column

        module subroutine parquet_write_int32_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int32), intent(in) :: data(:,:)
        end subroutine parquet_write_int32_matrix_column

        module subroutine parquet_write_int64_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: data(:)
        end subroutine parquet_write_int64_column

        module subroutine parquet_write_int64_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: data(:,:)
        end subroutine parquet_write_int64_matrix_column

        module subroutine parquet_write_float32_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real32), intent(in) :: data(:)
        end subroutine parquet_write_float32_column

        module subroutine parquet_write_float32_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real32), intent(in) :: data(:,:)
        end subroutine parquet_write_float32_matrix_column

        module subroutine parquet_write_float64_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real64), intent(in) :: data(:)
        end subroutine parquet_write_float64_column

        module subroutine parquet_write_float64_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real64), intent(in) :: data(:,:)
        end subroutine parquet_write_float64_matrix_column

        module subroutine parquet_write_logical_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            logical, intent(in) :: data(:)
        end subroutine parquet_write_logical_column

        module subroutine parquet_write_logical_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            logical, intent(in) :: data(:,:)
        end subroutine parquet_write_logical_matrix_column

        module subroutine parquet_write_string_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: data(:)
        end subroutine parquet_write_string_column

        module subroutine parquet_write_string_matrix_column(writer, name, data)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: data(:,:)
        end subroutine parquet_write_string_matrix_column

        module subroutine parquet_close_writer(writer)
            type(parquet_writer), intent(inout) :: writer
        end subroutine parquet_close_writer

        module subroutine parquet_writer_finalize(this)
            type(parquet_writer), intent(inout) :: this
        end subroutine parquet_writer_finalize

        module subroutine parquet_read_maml_file(maml_filename, cinfo, metadata)
            character(len=*), intent(in) :: maml_filename
            type(parquet_column_info), intent(out) :: cinfo
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_read_maml_file

        module function parquet_load_maml_file(maml_filename) result(maml)
            character(len=*), intent(in) :: maml_filename
            type(parquet_maml_file) :: maml
        end function parquet_load_maml_file

        module subroutine parquet_validate_user_maml(base_maml, user_maml)
            type(parquet_maml_file), intent(in) :: base_maml
            type(parquet_maml_file), intent(inout) :: user_maml
        end subroutine parquet_validate_user_maml

        module subroutine parquet_validate_maml_internal(maml)
            type(parquet_maml_file), intent(in) :: maml
        end subroutine parquet_validate_maml_internal

        module subroutine parquet_validate_maml_file(maml_filename)
            character(len=*), intent(in) :: maml_filename
        end subroutine parquet_validate_maml_file

        module subroutine parquet_read_maml_internal(maml, cinfo, metadata)
            type(parquet_maml_file), intent(in) :: maml
            type(parquet_column_info), intent(out) :: cinfo
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_read_maml_internal

        module subroutine parquet_parse_maml_lines(lines, cinfo, metadata)
            character(len=*), intent(in) :: lines(:)
            type(parquet_column_info), intent(out) :: cinfo
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_parse_maml_lines

        module integer function get_column_index(this, name)
            class(parquet_column_info), intent(in) :: this
            character(len=*), intent(in) :: name
        end function get_column_index

        module subroutine set_unavailable(this, name)
            class(parquet_column_info), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_unavailable

        module subroutine set_available(this, name)
            class(parquet_column_info), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_available

        module subroutine parquet_append_line(lines, n, line)
            character(len=1024), allocatable, intent(inout) :: lines(:)
            integer, intent(in) :: n
            character(len=*), intent(in) :: line
        end subroutine parquet_append_line

        module subroutine parquet_metadata_append_entry(metadata, key, value, description)
            class(parquet_table_metadata), intent(inout) :: metadata
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine parquet_metadata_append_entry

        module subroutine add_metadata_int32(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: val
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_int32

        module subroutine add_metadata_int64(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: val
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_int64

        module subroutine add_metadata_float32(this, key, val, desc, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: val
            character(len=*), intent(in), optional :: desc
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float32

        module subroutine add_metadata_float64(this, key, val, desc, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: val
            character(len=*), intent(in), optional :: desc
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float64

        module subroutine add_metadata_logical(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: val
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_logical

        module subroutine add_metadata_string(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: val
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_string

        module subroutine add_metadata_int32_array(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_int32_array

        module subroutine add_metadata_int64_array(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_int64_array

        module subroutine add_metadata_float32_array(this, key, val, desc, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float32_array

        module subroutine add_metadata_float64_array(this, key, val, desc, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float64_array

        module subroutine add_metadata_logical_array(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_logical_array

        module subroutine add_metadata_string_array(this, key, val, desc)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: val(:)
            character(len=*), intent(in), optional :: desc
        end subroutine add_metadata_string_array

        module subroutine parquet_append_empty_cinfo(columns, n)
            type(parquet_column_type), allocatable, intent(inout) :: columns(:)
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

        module subroutine parquet_reader_finalize(this)
            type(parquet_reader), intent(inout) :: this
        end subroutine parquet_reader_finalize

        module subroutine parquet_get_nrows_int64(reader, nrows)
            type(parquet_reader), intent(in) :: reader
            integer(int64), intent(out) :: nrows
        end subroutine parquet_get_nrows_int64

        module subroutine parquet_get_nrows_int32(reader, nrows)
            type(parquet_reader), intent(in) :: reader
            integer(int32), intent(out) :: nrows
        end subroutine parquet_get_nrows_int32

        module subroutine parquet_get_col_size(reader, name, col_size)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer, intent(out) :: col_size
        end subroutine parquet_get_col_size

        module subroutine parquet_get_column_total_elements_int64(reader, name, nelem)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: nelem
        end subroutine parquet_get_column_total_elements_int64

        module subroutine parquet_get_column_total_elements_int32(reader, name, nelem)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: nelem
        end subroutine parquet_get_column_total_elements_int32

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

    function get_parquet_fortran_version(internal) result(ver_string)
        implicit none
        character (len=:), allocatable :: ver_string
        integer :: i
        logical, intent(in), optional :: internal
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
        if (present(internal)) then
            if (internal) then
                ver_string = trim(cversion)
            else
                ver_string = trim(ver_string)
            end if
        end if
        !
    end function get_parquet_fortran_version

end module
