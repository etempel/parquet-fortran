!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
program parquet_fortran
    use parquet, only: get_parquet_fortran_version
    implicit none
    character(len=:), allocatable :: ver_string
    !
    ver_string = get_parquet_fortran_version()
    print*, "Parquet Fortran version: "//ver_string
    !
end program parquet_fortran