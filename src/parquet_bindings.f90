!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!
module parquet_bindings
    use iso_c_binding
    implicit none

    interface
        function create_parquet_writer(filename) &
                bind(C, name="create_parquet_writer") result(writer)
            import
            character(kind=c_char) :: filename(*)
            type(c_ptr) :: writer
        end function

        function create_parquet_reader(filename) &
                bind(C, name="create_parquet_reader") result(reader)
            import
            character(kind=c_char) :: filename(*)
            type(c_ptr) :: reader
        end function

        subroutine parquet_add_column_metadata(writer, name, unit, description, ucd, data_type, array_size, col_size) &
                bind(C, name="parquet_add_column_metadata")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: unit(*)
            character(kind=c_char) :: description(*)
            character(kind=c_char) :: ucd(*)
            character(kind=c_char) :: data_type(*)
            integer(c_long_long), value :: array_size
            integer(c_long_long), value :: col_size
        end subroutine

        subroutine parquet_add_table_metadata(writer, key, value) &
                bind(C, name="parquet_add_table_metadata")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: key(*)
            character(kind=c_char) :: value(*)
        end subroutine

        subroutine parquet_append_int32_column(writer, name, data, nrows, array_size) &
                bind(C, name="parquet_append_int32_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_append_int64_column(writer, name, data, nrows, array_size) &
                bind(C, name="parquet_append_int64_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_append_float32_column(writer, name, data, nrows, array_size) &
                bind(C, name="parquet_append_float32_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_append_float64_column(writer, name, data, nrows, array_size) &
                bind(C, name="parquet_append_float64_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_append_bool8_column(writer, name, data, nrows, array_size) &
                bind(C, name="parquet_append_bool8_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_append_string_column(writer, name, data, item_len, nrows) &
                bind(C, name="parquet_append_string_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_append_string_array_column(writer, name, data, item_len, nrows, array_size) &
                bind(C, name="parquet_append_string_array_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine close_parquet_writer(writer) &
                bind(C, name="close_parquet_writer")
            import
            type(c_ptr), value :: writer
        end subroutine

        subroutine close_parquet_reader(reader) &
                bind(C, name="close_parquet_reader")
            import
            type(c_ptr), value :: reader
        end subroutine

        function parquet_reader_get_nrows(reader) &
                bind(C, name="parquet_reader_get_nrows") result(nrows)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: nrows
        end function

        function parquet_reader_get_column_array_size(reader, name) &
                bind(C, name="parquet_reader_get_column_array_size") result(array_size)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: array_size
        end function

        function parquet_reader_get_column_total_elements(reader, name) &
                bind(C, name="parquet_reader_get_column_total_elements") result(nelem)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: nelem
        end function

        function parquet_reader_get_string_length(reader, name) &
                bind(C, name="parquet_reader_get_string_length") result(strlen_max)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: strlen_max
        end function

        subroutine parquet_read_int32_column(reader, name, data, nrows) &
                bind(C, name="parquet_read_int32_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_int64_column(reader, name, data, nrows) &
                bind(C, name="parquet_read_int64_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_float32_column(reader, name, data, nrows) &
                bind(C, name="parquet_read_float32_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_float64_column(reader, name, data, nrows) &
                bind(C, name="parquet_read_float64_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_bool8_column(reader, name, data, nrows) &
                bind(C, name="parquet_read_bool8_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_string_column(reader, name, data, item_len, nrows) &
                bind(C, name="parquet_read_string_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
        end subroutine

        subroutine parquet_read_int32_array_column(reader, name, data, nrows, array_size) &
                bind(C, name="parquet_read_int32_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_int64_array_column(reader, name, data, nrows, array_size) &
                bind(C, name="parquet_read_int64_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float32_array_column(reader, name, data, nrows, array_size) &
                bind(C, name="parquet_read_float32_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float64_array_column(reader, name, data, nrows, array_size) &
                bind(C, name="parquet_read_float64_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_bool8_array_column(reader, name, data, nrows, array_size) &
                bind(C, name="parquet_read_bool8_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_string_array_column(reader, name, data, item_len, nrows, array_size) &
                bind(C, name="parquet_read_string_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_int32_array_row(reader, name, row_index, data, array_size) &
                bind(C, name="parquet_read_int32_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_int64_array_row(reader, name, row_index, data, array_size) &
                bind(C, name="parquet_read_int64_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float32_array_row(reader, name, row_index, data, array_size) &
                bind(C, name="parquet_read_float32_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            real(c_float) :: data(*)
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float64_array_row(reader, name, row_index, data, array_size) &
                bind(C, name="parquet_read_float64_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            real(c_double) :: data(*)
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_bool8_array_row(reader, name, row_index, data, array_size) &
                bind(C, name="parquet_read_bool8_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_string_array_row(reader, name, row_index, data, item_len, array_size) &
                bind(C, name="parquet_read_string_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_int32_array_element(reader, name, col_index, data, nrows, array_size) &
                bind(C, name="parquet_read_int32_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_int64_array_element(reader, name, col_index, data, nrows, array_size) &
                bind(C, name="parquet_read_int64_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float32_array_element(reader, name, col_index, data, nrows, array_size) &
                bind(C, name="parquet_read_float32_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_float64_array_element(reader, name, col_index, data, nrows, array_size) &
                bind(C, name="parquet_read_float64_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_bool8_array_element(reader, name, col_index, data, nrows, array_size) &
                bind(C, name="parquet_read_bool8_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine

        subroutine parquet_read_string_array_element(reader, name, col_index, data, item_len, nrows, array_size) &
                bind(C, name="parquet_read_string_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: array_size
        end subroutine
    end interface

end module
