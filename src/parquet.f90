
module parquet
  use iso_c_binding
  use parquet_bindings
  implicit none

contains

  subroutine write_column(filename, data, unit, description)
    character(len=*), intent(in) :: filename
    real(c_double), intent(in) :: data(:)
    character(len=*), intent(in) :: unit, description
    type(c_ptr) :: writer

    writer = create_parquet_double_writer(trim(filename)//char(0))
    call write_parquet_double_data( &
      writer, &
      data, size(data, kind=c_long_long) )
    call write_parquet_votable_metadata( &
      writer, &
      trim(unit)//char(0), &
      trim(description)//char(0) )
    call close_parquet_writer(writer)
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
