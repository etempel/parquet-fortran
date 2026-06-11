!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!

module parquet_bindings
  use iso_c_binding
  implicit none

  interface
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
