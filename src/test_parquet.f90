module test_parquet
    use parquet
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    !
    ! col_name, var_name, data_type, array_size
    !#:set cols = [
    !#:  ("id0",       "id",       "int32"),
    !#:  ("idarr",     "idarr",    "int64",   2),
    !#:  ("name",      "name",     "string_8"),
    !#:  ("name_arr",  "name_arr", "string_4",  3),
    !#:  ("idlong",    "idlong",   "int64"),
    !#:  ("value",     "value",    "float32"),
    !#:  ("value_64",  "value2",   "float64"),
    !#:  ("arr",       "arr",      "float32", 5),
    !#:  ("arrlong",   "arrlong",  "float64", 5),
    !#:  ("val",       "val",      "float32"),
    !#:  ("iarr",      "iarr",     "int32",   3),
    !#:  ("myflag",    "flag",     "boolean"),
    !#:  ("flag_array", "flag_arr", "boolean", 6),
    !#:]
    !
    ! Reusable fypp approach for any codebase:
    !#:include "./parquet_cols.fypp"
    !
    ! type test_output_type
    !#:call emit_type_fields(cols)
    !#:endcall
    ! end type test_output_type
    !
    ! subroutine init_column_info()
    !   if (allocated(cinfo)) return
    !#:call emit_cinfo_init("cinfo", cols)
    !#:endcall
    ! end subroutine init_column_info
    !
    ! Fallback concrete definitions are kept here because current local fpm
    ! build does not run fypp preprocessing in this environment.
    type test_output_type
        integer(int32)         :: id
        integer(int64),dimension(1:2) :: idarr
        character(len=8)       :: name
        character(len=4),dimension(1:3):: name_arr
        integer(int64)         :: idlong
        real(real32)           :: value
        real(real64)           :: value2
        real(real32),dimension(1:5) :: arr
        real(real64),dimension(1:5):: arrlong
        real(real32)           :: val
        integer(int32),dimension(1:3) :: iarr
        logical                :: flag
        logical,dimension(1:6) :: flag_arr
    end type test_output_type
    !
    type(column_info), dimension(:), allocatable :: cinfo
    !
    type(test_output_type),dimension(:),allocatable :: test_data
    !
contains
    !
    subroutine start_parquet_test()
        implicit none
        !
        print*, "Starting Parquet Fortran test..."
        !
        call init_column_info()
        !
        call init_test_data(20)
        !
        print*, "Writing test data to Parquet file..."
        !
        call write_test_data("test_parquet.parquet", test_data, cinfo)
        !
        print*, "Done. Test data written to 'test_parquet.parquet'."
        !
    end subroutine start_parquet_test
    !
    subroutine write_test_data(filename, data, col)
        character(len=*), intent(in) :: filename
        type(test_output_type), dimension(:), intent(in) :: data
        type(column_info), dimension(:), intent(in) :: col
        type(parquet_writer) :: writer
        integer :: i, j, n
        integer(int64), allocatable :: idarr_col(:)
        character(len=4), allocatable :: name_arr_col(:)
        real(real32), allocatable :: arr_col(:)
        real(real64), allocatable :: arrlong_col(:)
        integer(int32), allocatable :: iarr_col(:)
        logical, allocatable :: flag_arr_col(:)

        if (size(col) < 13) stop "write_test_data: expected at least 13 columns in col"

        n = size(data)
        allocate(idarr_col(n*2), name_arr_col(n*3), arr_col(n*5), arrlong_col(n*5), iarr_col(n*3), flag_arr_col(n*6))

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

        call parquet_open_writer(writer, filename, col)

        call parquet_write_column(writer, col(8)%name, arr_col)
        call parquet_write_column(writer, 'id0', data(:)%id)
        call parquet_write_column(writer, col(12)%name, data(:)%flag)
        call parquet_write_column(writer, col(2)%name, idarr_col)
        call parquet_write_column(writer, col(3)%name, data(:)%name)
        call parquet_write_column(writer, col(4)%name, name_arr_col)
        call parquet_write_column(writer, col(13)%name, flag_arr_col)
        call parquet_write_column(writer, col(7)%name, data(:)%value2)
        call parquet_write_column(writer, col(5)%name, data(:)%idlong)
        call parquet_write_column(writer, col(6)%name, data(:)%value)
        call parquet_write_column(writer, col(10)%name, data(:)%val)
        call parquet_write_column(writer, col(11)%name, iarr_col)
        call parquet_write_column(writer, col(9)%name, arrlong_col)

        call parquet_close_writer(writer)
    end subroutine write_test_data
    !
    subroutine init_column_info()
        implicit none

        if (allocated(cinfo)) return

        allocate(cinfo(13))
        cinfo(1)  = column_info(.true., "id0", "", "ID of the object", "meta.id;meta.main", "int32", "id", 1, 0)
        cinfo(2)  = column_info(.true., "idarr", "", "Array of IDs", "meta.id;meta.main", "int64", "idarr", 2, 0)
        cinfo(3)  = column_info(.true., "name", "", "Name of the object", "meta.id;meta.main", "string", "name", 1, 8)
        cinfo(4)  = column_info(.true., "name_arr", "", "Array of short names", "meta.id;meta.main", "string", "name_arr", 3, 4)
        cinfo(5)  = column_info(.true., "idlong", "count", "Long ID of the object", "meta.id;meta.main", "int64", "idlong", 1, 0)
        cinfo(6)  = column_info(.true., "value", "m/s", "Value of the object", "phys.veloc;phys.speed", "float32", "value", 1, 0)
        cinfo(7)  = column_info(.true., "value_64", "m/s", "Second value of the object", "phys.veloc;phys.speed", "float64", "value2", 1, 0)
        cinfo(8)  = column_info(.true., "arr", "none", "Array of values (float32)", "", "float32", "arr", 5, 0)
        cinfo(9)  = column_info(.true., "arrlong", "none", "Array of values (float64)", "", "float64", "arrlong", 5, 0)
        cinfo(10) = column_info(.true., "val", "none", "Single value (float32)", "", "float32", "val", 1, 0)
        cinfo(11) = column_info(.true., "iarr", "none", "Array of integers (int32)", "", "int32", "iarr", 3, 0)
        cinfo(12) = column_info(.true., "myflag", "", "Logical flag (boolean)", "", "boolean", "flag", 1, 0)
        cinfo(13) = column_info(.true., "flag_array", "none", "Array of logical values (boolean)", "", "boolean", "flag_arr", 6, 0)
    end subroutine init_column_info
    !
    subroutine init_test_data(n)
        implicit none
        integer, intent(in) :: n
        integer :: i, j
        !
        allocate(test_data(1:n))
        !
        do i = 1, n
            test_data(i)%id = i
            test_data(i)%idarr = [int(i*10 + 1, kind=int64), int(i*10 + 2, kind=int64)]
            write(test_data(i)%name, '(A,I3)') "Obj", i
            do j = 1, 3
                write(test_data(i)%name_arr(j), '(A,I1)') "N", j
            end do
            test_data(i)%idlong = int(i*1000, kind=int64)
            test_data(i)%value = real(i*10.0, kind=real32)
            test_data(i)%value2 = real(i*20.0, kind=real64)
            test_data(i)%arr = [(real(j+i, kind=real32), j=1,5)]
            test_data(i)%arrlong = [(real(j+i*10, kind=real64), j=1,5)]
            test_data(i)%val = real(i*0.1, kind=real32)
            test_data(i)%iarr = [(i+j, j=1,3)]
            test_data(i)%flag = mod(i,2) == 0
            test_data(i)%flag_arr = [(mod(i+j,2) == 0, j=1,6)]
        end do
        !
    end subroutine init_test_data
    !
end module test_parquet