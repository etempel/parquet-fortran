!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
program parquet_fortran
    use parquet, only: parquet_get_version, parquet_get_arrow_version
    implicit none
    character(len=:), allocatable :: ver_string
    !
    call parquet_get_version(ver_string)
    print*, "Parquet Fortran version: "//ver_string
    !
    call parquet_get_version(ver_string,mode='internal')
    print*, "Parquet Fortran internal version: "//ver_string
    !
    call parquet_get_arrow_version(ver_string)
    print*, "Apache Arrow version: "//ver_string
    !
    call parquet_get_arrow_version(ver_string,mode='parquet')
    print*, "Apache Parquet version: "//ver_string
    !
end program parquet_fortran