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

    subroutine parquet_add_column_metadata(writer, name, unit, description, ucd, data_type, array_size) &
        bind(C, name="parquet_add_column_metadata")
      import
      type(c_ptr), value :: writer
      character(kind=c_char) :: name(*)
      character(kind=c_char) :: unit(*)
      character(kind=c_char) :: description(*)
      character(kind=c_char) :: ucd(*)
      character(kind=c_char) :: data_type(*)
      integer(c_long_long), value :: array_size
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

    function create_parquet_double_writer(filename) &
        bind(C, name="create_parquet_double_writer") result(writer)
      import
      character(kind=c_char) :: filename(*)
      type(c_ptr) :: writer
    end function

    subroutine write_parquet_double_data(writer, data, n) &
        bind(C, name="write_parquet_double_data")
      import
      type(c_ptr), value :: writer
      real(c_double) :: data(*)
      integer(c_long_long), value :: n
    end subroutine

    subroutine write_parquet_votable_metadata(writer, unit, description) &
        bind(C, name="write_parquet_votable_metadata")
      import
      type(c_ptr), value :: writer
      character(kind=c_char) :: unit(*)
      character(kind=c_char) :: description(*)
    end subroutine

    subroutine close_parquet_writer(writer) &
        bind(C, name="close_parquet_writer")
      import
      type(c_ptr), value :: writer
    end subroutine

    subroutine read_parquet_double(filename, data, n) &
        bind(C, name="read_parquet_double")
      import
      character(kind=c_char) :: filename(*)
      real(c_double) :: data(*)
      integer(c_long_long) :: n
    end subroutine
  end interface

end module
