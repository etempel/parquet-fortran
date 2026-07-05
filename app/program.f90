!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
program parquet_fortran
    use parquet, only: parquet_get_version
    implicit none
    character(len=:), allocatable :: ver_string
    !
    call parquet_get_version(ver_string)
    print*, "Parquet Fortran version: "//ver_string
    !
end program parquet_fortran