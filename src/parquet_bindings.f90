!========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!========================
!

module parquet_bindings
  use iso_c_binding
  implicit none

  interface
    subroutine write_parquet_double_meta(filename, data, n, unit, description) &
        bind(C, name="write_parquet_double_meta")
      import
      character(kind=c_char) :: filename(*)
      real(c_double) :: data(*)
      integer(c_long_long), value :: n
      character(kind=c_char) :: unit(*)
      character(kind=c_char) :: description(*)
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
