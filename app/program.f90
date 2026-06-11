!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
program parquet_fortran
    use parquet
  use iso_c_binding
  implicit none

  real(c_double), dimension(5) :: data = [1,2,3,4,5]
  real(c_double), dimension(100) :: read_data
  integer(c_long_long) :: n
  integer :: i

    print*, "Writing data to test.parquet"
  call write_column("test.parquet", data, "km/s", "velocity")
    print*, "Done writing."

  call read_column("test.parquet", read_data, n)

  print *, "Read:", n
  do i = 1, n
     print *, read_data(i)
  end do

    !
end program parquet_fortran