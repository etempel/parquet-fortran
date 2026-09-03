!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
!> Prints the parquet-fortran library version number -- a quick sanity check that a build or install
!! actually picked up the version you expect.
!!
!! This is the ONLY `app/` executable meant for a consumer of this library. Every other program
!! under `app/` is a maintainer benchmark or probe; `bench/` is where those belong.
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