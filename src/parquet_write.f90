!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!
submodule (parquet) parquet_write
contains

    module procedure parquet_get_enabled_column_index
        integer :: i

        parquet_get_enabled_column_index = 0
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_enabled_column_index = i
                return
            end if
        end do
    end procedure parquet_get_enabled_column_index

    module procedure parquet_get_defined_column_index
        integer :: i

        parquet_get_defined_column_index = 0
        if (.not. allocated(writer%all_columns)) return

        do i = 1, size(writer%all_columns)
            if (trim(writer%all_columns(i)%name) == trim(name)) then
                parquet_get_defined_column_index = i
                return
            end if
        end do
    end procedure parquet_get_defined_column_index

    module procedure parquet_is_type_compatible
        select case (trim(expected_type))
        case ("boolean")
            parquet_is_type_compatible = trim(actual_type) == "boolean" .or. trim(actual_type) == "bool8"
        case default
            parquet_is_type_compatible = trim(actual_type) == trim(expected_type)
        end select
    end procedure parquet_is_type_compatible

    module procedure parquet_assert_column_type
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
    end procedure parquet_assert_column_type

    module procedure parquet_mark_column_written
        integer :: idx

        if (.not. allocated(writer%enabled_columns)) return

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx == 0) return

        if (writer%write_counts(idx) > 0) then
            error stop "parquet_write_column: column written more than once: " // trim(name)
        end if
        writer%write_counts(idx) = writer%write_counts(idx) + 1
    end procedure parquet_mark_column_written

    module procedure parquet_is_column_enabled
        parquet_is_column_enabled = .true.
        if (.not. allocated(writer%enabled_columns)) return

        parquet_is_column_enabled = parquet_get_enabled_column_index(writer, name) > 0
    end procedure parquet_is_column_enabled

    module procedure parquet_get_column_array_size
        integer :: i

        parquet_get_column_array_size = 1
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_column_array_size = max(1, writer%enabled_columns(i)%array_size)
                return
            end if
        end do
    end procedure parquet_get_column_array_size

    module procedure parquet_open_writer
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
    end procedure parquet_open_writer

    module procedure parquet_add_column_info
        call parquet_add_column_metadata(&
            writer%handle, &
            trim(name)//char(0), &
            trim(unit)//char(0), &
            trim(description)//char(0), &
            trim(ucd)//char(0), &
            trim(data_type)//char(0), &
            int(array_size, kind=c_long_long) )
    end procedure parquet_add_column_info

    module procedure parquet_write_int32_column
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
    end procedure parquet_write_int32_column

    module procedure parquet_write_int64_column
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
    end procedure parquet_write_int64_column

    module procedure parquet_write_float32_column
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
    end procedure parquet_write_float32_column

    module procedure parquet_write_float64_column
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
    end procedure parquet_write_float64_column

    module procedure parquet_write_logical_column
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
    end procedure parquet_write_logical_column

    module procedure parquet_write_string_column
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
    end procedure parquet_write_string_column

    module procedure parquet_close_writer
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
    end procedure parquet_close_writer

end submodule parquet_write
