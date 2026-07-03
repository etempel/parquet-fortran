!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module test_writing
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    !
    implicit none
    private
    public :: collect_tests_parquet_writing
    integer, parameter :: rk = kind(1.0d0)

    type test_output_type
        integer :: id
        integer(int64), dimension(1:2) :: idarr
        character(len=18) :: name
        character(len=4), dimension(1:3) :: name_arr
        integer(int64) :: idlong
        real(real32) :: value
        real(rk) :: value2
        real(real32), dimension(1:5) :: arr
        real(real64), dimension(1:5) :: arrlong
        real(real32) :: val
        integer(int32), dimension(1:3) :: iarr
        logical :: flag
        logical, dimension(1:6) :: flag_arr
    end type test_output_type
    !
contains
    !
    !> Collect all exported unit tests
    subroutine collect_tests_parquet_writing(testsuite)
        implicit none
        integer :: status
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        !
        testsuite = [ &
            new_unittest("write extensive parquet file", test_write_parquet_file), &
            new_unittest("write simple parquet file", test_write_simple_parquet) &
            ]
        !
    end subroutine collect_tests_parquet_writing
    !
    subroutine test_write_simple_parquet(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        real(rk), dimension(5) :: xdata = [1,2,3,4,5]
        real(rk), dimension(3,5) :: xdata2
        type(parquet_writer) :: writer
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_simple.parquet"
        integer:: i,j
        !
        do i = 1, 5
            do j = 1, 3
                xdata2(j,i) = real(i+j, kind=rk)
            end do
        end do
        !
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "colx", xdata)
        call parquet_write_column(writer, "arr", xdata2)
        call parquet_close_writer(writer)
        !
        inquire(file=out_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "expected simple output parquet file was not created")
            return
        end if
        !
    end subroutine test_write_simple_parquet
    !
    subroutine test_write_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        type(test_output_type), allocatable :: test_data(:)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/test_parquet.parquet"
        integer :: cmdstat

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        maml = get_parquet_maml("maml_example.maml")
        call parquet_read_maml(maml, cinfo, metadata)

        call metadata%add_metadata("creator", "Parquet Fortran Test", "Description 1")
        call metadata%add_metadata("PI", 3.14, "Description 1", fmt='F6.1')
        call metadata%add_metadata("PI2", 3.14_rk, fmt='F0.2', desc="Description 2")
        call metadata%add_metadata("row_count", 20_int32, "Description 3")
        call metadata%add_metadata("row_count_long", 20_int64, "Description 4")
        call metadata%add_metadata("is_test", .true.)

        call metadata%add_metadata("arrint", [0,1,2], "Array int")
        call metadata%add_metadata("arrint64", [1_int64,2_int64])
        call metadata%add_metadata("arreal", [1.1,1.0,2.0], "Array real")
        call metadata%add_metadata("arrreal64", [1.1_rk,1.0_rk,2.0_rk], desc="Array real64", fmt='F0.1')
        call metadata%add_metadata("arrlogical", [.true., .false., .true.], desc="Array logical")
        call metadata%add_metadata("arrstring", ["one  ","two  ","three"], "Array string")

        call init_test_data(test_data, 20)
        call write_test_data(out_file, test_data, cinfo, metadata)

        inquire(file=out_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "expected output parquet file was not created")
            return
        end if
        !
    end subroutine test_write_parquet_file

    subroutine write_test_data(filename, data, col, tmeta)
        character(len=*), intent(in) :: filename
        type(test_output_type), dimension(:), intent(in) :: data
        type(parquet_column_info), intent(in) :: col
        type(parquet_table_metadata), intent(in), optional :: tmeta
        type(parquet_writer) :: writer
        integer :: i, j, n, name_len_max
        integer(int64), allocatable :: idarr_col(:)
        character(len=:), allocatable :: name_col(:)
        character(len=4), allocatable :: name_arr_col(:)
        real(real32), allocatable :: arr_col(:)
        real(real64), allocatable :: arrlong_col(:)
        integer(int32), allocatable :: iarr_col(:)
        logical, allocatable :: flag_arr_col(:)

        if (size(col%col) < 13) error stop "write_test_data: expected at least 13 columns in col"

        n = size(data)
        allocate(idarr_col(n*2), name_arr_col(n*3), arr_col(n*5), arrlong_col(n*5), iarr_col(n*3), flag_arr_col(n*6))

        name_len_max = max(1, maxval([(len_trim(adjustl(data(i)%name)) + 4, i=1,n)]))
        allocate(character(len=name_len_max) :: name_col(n))

        do i = 1, n
            do j = 1, 2
                idarr_col((i-1)*2 + j) = data(i)%idarr(j)
            end do

            do j = 1, 5
                arr_col((i-1)*5 + j) = data(i)%arr(j)
                arrlong_col((i-1)*5 + j) = data(i)%arrlong(j)
            end do

            do j = 1, 3
                name_arr_col((i-1)*3 + j) = data(i)%name_arr(j)
                iarr_col((i-1)*3 + j) = data(i)%iarr(j)
            end do

            do j = 1, 6
                flag_arr_col((i-1)*6 + j) = data(i)%flag_arr(j)
            end do
        end do

        do i = 1, n
            name_col(i) = "var_" // trim(adjustl(data(i)%name))
        end do

        call parquet_open_writer(writer, filename, col, metadata=tmeta)

        call parquet_write_column(writer, col%col(8)%name, arr_col)
        call parquet_write_column(writer, "id0", data(:)%id)
        call parquet_write_column(writer, col%col(12)%name, data(:)%flag)
        call parquet_write_column(writer, col%col(2)%name, idarr_col)
        call parquet_write_column(writer, "name", name_col)
        call parquet_write_column(writer, col%col(4)%name, name_arr_col)
        call parquet_write_column(writer, col%col(13)%name, flag_arr_col)
        call parquet_write_column(writer, col%col(7)%name, data(:)%value2)
        call parquet_write_column(writer, col%col(5)%name, data(:)%idlong)
        call parquet_write_column(writer, col%col(6)%name, data(:)%value)
        call parquet_write_column(writer, col%col(10)%name, data(:)%val)
        call parquet_write_column(writer, col%col(11)%name, iarr_col)
        call parquet_write_column(writer, col%col(9)%name, arrlong_col)

        call parquet_close_writer(writer)
    end subroutine write_test_data

    subroutine init_test_data(test_data, n)
        type(test_output_type), allocatable, intent(out) :: test_data(:)
        integer, intent(in) :: n
        integer :: i, j

        allocate(test_data(1:n))

        do i = 1, n
            test_data(i)%id = i
            test_data(i)%idarr = [int(i*10 + 1, kind=int64), int(i*10 + 2, kind=int64)]
            write(test_data(i)%name, '(A,I3)') "Obj", i
            do j = 1, 3
                write(test_data(i)%name_arr(j), '(A,I1)') "N", j
            end do
            test_data(i)%idlong = int(i*1000, kind=int64)
            test_data(i)%value = real(i*10.0, kind=real32)
            test_data(i)%value2 = real(i*20.0, kind=rk)
            test_data(i)%arr = [(real(j+i, kind=real32), j=1,5)]
            test_data(i)%arrlong = [(real(j+i*10, kind=real64), j=1,5)]
            test_data(i)%val = real(i*0.1, kind=real32)
            test_data(i)%iarr = [(i+j, j=1,3)]
            test_data(i)%flag = mod(i,2) == 0
            test_data(i)%flag_arr = [(mod(i+j,2) == 0, j=1,6)]
        end do
    end subroutine init_test_data
    !
end module test_writing