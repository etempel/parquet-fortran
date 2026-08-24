!=========================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!=========================
!
!> Prints the parquet-fortran library version number -- a quick sanity check that a build or install
!! actually picked up the version you expect.
!!
!! This is the ONLY `app/` executable that ships in the fpm-published package
!! (`tools/prep_fpm_publish.sh`'s `APP_KEEP` allow-list). Every other program under `app/` is a
!! maintainer benchmark or probe and is stripped from what a consumer installs.
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