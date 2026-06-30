!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!
module parquet
        use iso_c_binding
        use iso_fortran_env, only: int8, int32, int64, real32, real64
        use parquet_bindings
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

        interface parquet_write_column
            module procedure parquet_write_int32_column
            module procedure parquet_write_int64_column
            module procedure parquet_write_float32_column
            module procedure parquet_write_float64_column
            module procedure parquet_write_logical_column
            module procedure parquet_write_string_column
        end interface parquet_write_column

        public :: parquet_writer
        public :: parquet_column_info
        public :: parquet_table_metadata
        public :: parquet_open_writer
        public :: parquet_write_column
        public :: parquet_close_writer
        public :: get_parquet_fortran_version
        public :: parquet_read_maml

contains

    integer function parquet_get_enabled_column_index(writer, name)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer :: i

        parquet_get_enabled_column_index = 0
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_enabled_column_index = i
                return
            end if
        end do
    end function parquet_get_enabled_column_index

    integer function parquet_get_defined_column_index(writer, name)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer :: i

        parquet_get_defined_column_index = 0
        if (.not. allocated(writer%all_columns)) return

        do i = 1, size(writer%all_columns)
            if (trim(writer%all_columns(i)%name) == trim(name)) then
                parquet_get_defined_column_index = i
                return
            end if
        end do
    end function parquet_get_defined_column_index

    logical function parquet_is_type_compatible(actual_type, expected_type)
        character(len=*), intent(in) :: actual_type
        character(len=*), intent(in) :: expected_type

        select case (trim(expected_type))
        case ("boolean")
            parquet_is_type_compatible = trim(actual_type) == "boolean" .or. trim(actual_type) == "bool8"
        case default
            parquet_is_type_compatible = trim(actual_type) == trim(expected_type)
        end select
    end function parquet_is_type_compatible

    subroutine parquet_assert_column_type(writer, name, expected_type)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: expected_type
        integer :: idx

        if (.not. writer%enforce_schema) return

        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) then
            error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
        end if

        if (.not. parquet_is_type_compatible(writer%all_columns(idx)%data_type, expected_type)) then
            error stop "parquet_write_column: type mismatch for column " // trim(name) // &
                                  " (expected " // trim(expected_type) // ", got " // trim(writer%all_columns(idx)%data_type) // ")"
        end if
    end subroutine parquet_assert_column_type

    subroutine parquet_mark_column_written(writer, name)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        integer :: idx

        if (.not. allocated(writer%enabled_columns)) return

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx == 0) return

        if (writer%write_counts(idx) > 0) then
            error stop "parquet_write_column: column written more than once: " // trim(name)
        end if
        writer%write_counts(idx) = writer%write_counts(idx) + 1
    end subroutine parquet_mark_column_written

    logical function parquet_is_column_enabled(writer, name)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name

        parquet_is_column_enabled = .true.
        if (.not. allocated(writer%enabled_columns)) return

        parquet_is_column_enabled = parquet_get_enabled_column_index(writer, name) > 0
    end function parquet_is_column_enabled

    integer function parquet_get_column_array_size(writer, name)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer :: i

        parquet_get_column_array_size = 1
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_column_array_size = max(1, writer%enabled_columns(i)%array_size)
                return
            end if
        end do
    end function parquet_get_column_array_size

    subroutine parquet_open_writer(writer, filename, cinfo, metadata)
        type(parquet_writer), intent(out) :: writer
        character(len=*), intent(in) :: filename
        type(parquet_column_info), intent(in), optional :: cinfo(:)
        type(parquet_table_metadata), intent(in), optional :: metadata
        integer :: i, k, n_enabled

        writer%handle = create_parquet_writer(trim(filename)//char(0))
        writer%enforce_schema = present(cinfo)

        if (present(cinfo)) then
            allocate(writer%all_columns(size(cinfo)))
            writer%all_columns = cinfo

            n_enabled = 0
            do i = 1, size(cinfo)
                if (cinfo(i)%is_set) n_enabled = n_enabled + 1
            end do

            if (n_enabled > 0) then
                allocate(writer%enabled_columns(n_enabled))
                allocate(writer%write_counts(n_enabled))
                writer%write_counts = 0
                k = 0
            end if

            do i = 1, size(cinfo)
                if (cinfo(i)%is_set) then
                    k = k + 1
                    writer%enabled_columns(k) = cinfo(i)
                    call parquet_add_column_info(&
                        writer, &
                        cinfo(i)%name, &
                        cinfo(i)%unit, &
                        cinfo(i)%info, &
                        cinfo(i)%ucd, &
                        cinfo(i)%data_type, &
                        cinfo(i)%array_size )
                end if
            end do
        end if

        if (present(metadata)) then
            if (allocated(metadata%items)) then
                do i = 1, size(metadata%items)
                    call parquet_add_table_metadata(writer%handle, &
                        trim(metadata%items(i)%key)//char(0), &
                        trim(metadata%items(i)%value)//char(0))
                end do
            end if
        end if
    end subroutine parquet_open_writer

    subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: unit
        character(len=*), intent(in) :: description
        character(len=*), intent(in) :: ucd
        character(len=*), intent(in) :: data_type
        integer, intent(in) :: array_size

        call parquet_add_column_metadata(&
            writer%handle, &
            trim(name)//char(0), &
            trim(unit)//char(0), &
            trim(description)//char(0), &
            trim(ucd)//char(0), &
            trim(data_type)//char(0), &
            int(array_size, kind=c_long_long) )
    end subroutine parquet_add_column_info

    subroutine parquet_write_int32_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: data(:)
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_int32_column: data size is not divisible by array_size"
        nrows = size(data) / asize

        call parquet_append_int32_column(&
            writer%handle, &
            trim(name)//char(0), &
            data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end subroutine parquet_write_int32_column

    subroutine parquet_write_int64_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: data(:)
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_int64_column: data size is not divisible by array_size"
        nrows = size(data) / asize

        call parquet_append_int64_column(&
            writer%handle, &
            trim(name)//char(0), &
            data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end subroutine parquet_write_int64_column

    subroutine parquet_write_float32_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        real(real32), intent(in) :: data(:)
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_float32_column: data size is not divisible by array_size"
        nrows = size(data) / asize

        call parquet_append_float32_column(&
            writer%handle, &
            trim(name)//char(0), &
            data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end subroutine parquet_write_float32_column

    subroutine parquet_write_float64_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: data(:)
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_float64_column: data size is not divisible by array_size"
        nrows = size(data) / asize

        call parquet_append_float64_column(&
            writer%handle, &
            trim(name)//char(0), &
            data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end subroutine parquet_write_float64_column

    subroutine parquet_write_logical_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        logical, intent(in) :: data(:)
        integer :: asize, nrows, i, idx
        integer(c_int8_t), allocatable :: bool_data(:)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_logical_column: data size is not divisible by array_size"
        nrows = size(data) / asize

        allocate(bool_data(size(data)))
        do i = 1, size(data)
            if (data(i)) then
                bool_data(i) = 1_c_int8_t
            else
                bool_data(i) = 0_c_int8_t
            end if
        end do

        call parquet_append_bool8_column(&
            writer%handle, &
            trim(name)//char(0), &
            bool_data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end subroutine parquet_write_logical_column

    subroutine parquet_write_string_column(writer, name, data)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: data(:)
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, nrows, item_len, idx, asize, nitems

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_array_size(writer, name)
        nitems = size(data)
        if (nitems <= 0) return
        if (mod(nitems, asize) /= 0) stop "parquet_write_string_column: data size is not divisible by array_size"

        nrows = nitems / asize

        item_len = len(data(1))
        allocate(packed(item_len*nitems))

        k = 0
        do i = 1, nitems
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(data(i)(j:j)), kind=c_char)
            end do
        end do

        if (asize == 1) then
            call parquet_append_string_column(&
                writer%handle, &
                trim(name)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long) )
        else
            call parquet_append_string_array_column(&
                writer%handle, &
                trim(name)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long), &
                int(asize, kind=c_long_long) )
        end if
    end subroutine parquet_write_string_column

    subroutine parquet_close_writer(writer)
        type(parquet_writer), intent(inout) :: writer
        integer :: i

        if (writer%enforce_schema .and. allocated(writer%enabled_columns)) then
            do i = 1, size(writer%enabled_columns)
                if (writer%write_counts(i) == 0) then
                    error stop "parquet_close_writer: missing write for enabled column: " // trim(writer%enabled_columns(i)%name)
                end if
            end do
        end if

        call close_parquet_writer(writer%handle)
        writer%handle = c_null_ptr
        if (allocated(writer%all_columns)) deallocate(writer%all_columns)
        if (allocated(writer%write_counts)) deallocate(writer%write_counts)
        if (allocated(writer%enabled_columns)) deallocate(writer%enabled_columns)
        writer%enforce_schema = .false.
    end subroutine parquet_close_writer
    !
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

    subroutine parquet_read_maml(maml_filename, cinfo, metadata)
        character(len=*), intent(in) :: maml_filename
        type(parquet_column_info), allocatable, intent(out) :: cinfo(:)
        type(parquet_table_metadata), intent(out) :: metadata

        type(parquet_column_info), allocatable :: tmp(:)
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list
        character(len=:), allocatable :: list_key
        integer :: unit, ios, n, i, list_item_idx
        character(len=32) :: idx_buf

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        list_item_idx = 0
        n = 0
        list_key = ""
        if (allocated(metadata%items)) deallocate(metadata%items)

        open(newunit=unit, file=trim(maml_filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_read_maml: cannot open file: " // trim(maml_filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (in_list) then
                    if (index(tline, "- ") == 1) then
                        list_item_idx = list_item_idx + 1
                        if (list_key == "comments" .or. list_key == "comment") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("comment_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else
                            call metadata%add_metadata(list_key, parquet_unquote(tline(3:)))
                        end if
                        cycle
                    else if (index(tline, ":") > 0 .and. line(1:1) /= " ") then
                        in_list = .false.
                    else
                        cycle
                    end if
                end if

                if (tline == "fields:") in_fields = .true.
                if (in_fields) cycle

                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle

                key = parquet_to_lower(key)

                if (key == "fields") then
                    in_fields = .true.
                else if (len_trim(cvalue) > 0) then
                    call metadata%add_metadata(key, parquet_unquote(cvalue))
                else
                    in_list = .true.
                    list_key = key
                    list_item_idx = 0
                end if
                cycle
            end if

            ! End fields block on next top-level key (e.g. "license:")
            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. line(1:1) /= " ") exit

            if (index(tline, "-") == 1) then
                call parquet_append_empty_cinfo(tmp, n)
                have_current = .true.
                tline = trim(adjustl(tline(2:)))
                if (len_trim(tline) == 0) cycle
            end if

            if (.not. have_current) cycle

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            select case (parquet_to_lower(key))
            case ("name")
                tmp(n)%name = parquet_unquote(cvalue)
            case ("unit")
                tmp(n)%unit = parquet_unquote(cvalue)
                if (parquet_to_lower(tmp(n)%unit) == "unitless") tmp(n)%unit = ""
            case ("info")
                tmp(n)%info = parquet_unquote(cvalue)
            case ("ucd")
                tmp(n)%ucd = parquet_unquote(cvalue)
            case ("data_type")
                cvalue = parquet_to_lower(parquet_unquote(cvalue))
                if (index(cvalue, "string") == 1) then
                    cvalue = "string"
                end if
                !
                tmp(n)%data_type = cvalue
            case ("array_size")
                read(cvalue, *, iostat=ios) tmp(n)%array_size
                if (ios /= 0) tmp(n)%array_size = 1
            end select
        end do

        close(unit)

        if (n <= 0) then
            allocate(cinfo(0))
            return
        end if

        do i = 1, n
            if (.not. allocated(tmp(i)%name)) error stop "parquet_read_maml: missing field name in fields block"
            if (.not. allocated(tmp(i)%data_type)) error stop "parquet_read_maml: missing data_type in fields block"
            if (.not. allocated(tmp(i)%unit)) tmp(i)%unit = ""
            if (.not. allocated(tmp(i)%info)) tmp(i)%info = ""
            if (.not. allocated(tmp(i)%ucd)) tmp(i)%ucd = ""
            if (tmp(i)%array_size <= 0) tmp(i)%array_size = 1
            tmp(i)%is_set = .true.
        end do

        call move_alloc(tmp, cinfo)
    end subroutine parquet_read_maml

    subroutine parquet_metadata_append_entry(metadata, key, value)
        class(parquet_table_metadata), intent(inout) :: metadata
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        type(parquet_metadata_entry), allocatable :: tmp(:)
        integer :: n

        if (len_trim(key) == 0) return

        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            return
        end if

        n = size(metadata%items)
        allocate(tmp(n+1))
        tmp(1:n) = metadata%items
        tmp(n+1)%key = trim(key)
        tmp(n+1)%value = trim(value)
        call move_alloc(tmp, metadata%items)
    end subroutine parquet_metadata_append_entry

    subroutine add_metadata_int32(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: val
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval))
    end subroutine add_metadata_int32

    subroutine add_metadata_int64(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: val
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval))
    end subroutine add_metadata_int64

    subroutine add_metadata_float32(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        real(real32), intent(in) :: val
        character(len=64) :: cval

        write(cval, '(ES15.7E3)') val
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)))
    end subroutine add_metadata_float32

    subroutine add_metadata_float64(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: val
        character(len=64) :: cval

        write(cval, '(ES24.16E3)') val
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)))
    end subroutine add_metadata_float64

    subroutine add_metadata_logical(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        logical, intent(in) :: val

        if (val) then
            call parquet_metadata_append_entry(this, key, "true")
        else
            call parquet_metadata_append_entry(this, key, "false")
        end if
    end subroutine add_metadata_logical

    subroutine add_metadata_string(this, key, val)
        class(parquet_table_metadata), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val

        call parquet_metadata_append_entry(this, key, trim(val))
    end subroutine add_metadata_string

    subroutine parquet_append_empty_cinfo(cinfo, n)
        type(parquet_column_info), allocatable, intent(inout) :: cinfo(:)
        integer, intent(inout) :: n
        type(parquet_column_info), allocatable :: tmp(:)

        if (.not. allocated(cinfo)) then
            allocate(cinfo(1))
            n = 1
        else
            allocate(tmp(size(cinfo) + 1))
            if (size(cinfo) > 0) tmp(1:size(cinfo)) = cinfo
            call move_alloc(tmp, cinfo)
            n = size(cinfo)
        end if

        cinfo(n)%is_set = .true.
        cinfo(n)%array_size = 1
    end subroutine parquet_append_empty_cinfo

    subroutine parquet_split_key_value(line, key, value)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        character(len=:), allocatable, intent(out) :: value
        integer :: p

        p = index(line, ":")
        if (p <= 0) then
            key = ""
            value = ""
            return
        end if

        key = trim(adjustl(line(1:p-1)))
        if (p < len_trim(line)) then
            value = trim(adjustl(line(p+1:len_trim(line))))
        else
            value = ""
        end if
    end subroutine parquet_split_key_value

    function parquet_unquote(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: n

        out = trim(adjustl(s))
        n = len_trim(out)
        if (n >= 2) then
            if ((out(1:1) == '"' .and. out(n:n) == '"') .or. (out(1:1) == "'" .and. out(n:n) == "'")) then
                out = out(2:n-1)
            end if
        end if
    end function parquet_unquote

    function parquet_to_lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i, c

        out = s
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end function parquet_to_lower

end module
