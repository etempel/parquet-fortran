
module parquet
  use iso_c_binding
  use parquet_bindings
  implicit none

contains

  subroutine write_column(filename, data, unit, description)
    character(len=*), intent(in) :: filename
    real(c_double), intent(in) :: data(:)
    character(len=*), intent(in) :: unit, description

    call write_parquet_double_meta( &
        trim(filename)//char(0), &
        data, size(data, kind=c_long_long), &
        trim(unit)//char(0), &
        trim(description)//char(0) )
  end subroutine


  subroutine read_column(filename, data, n)
    character(len=*), intent(in) :: filename
    real(c_double), intent(out) :: data(:)
    integer(c_long_long), intent(out) :: n

    call read_parquet_double( &
        trim(filename)//char(0), &
        data, n )
  end subroutine

end module
