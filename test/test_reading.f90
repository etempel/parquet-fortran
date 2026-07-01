!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module test_reading
    use parquet
    use parquet_maml
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    !
    implicit none
    private
    public :: collect_tests_parquet_reading
    !
contains
    !
    subroutine collect_tests_parquet_reading(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("read parquet file", test_read_parquet_file), &
            new_unittest("read column info", test_read_column_info), &
            new_unittest("read array modes", test_read_array_modes), &
            new_unittest("get library version", test_get_library_version), &
            new_unittest("get parquet maml examples", test_get_parquet_maml_examples) &
            ]
    end subroutine collect_tests_parquet_reading

    subroutine test_read_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer :: n
        integer(int32), allocatable :: ids(:)
        integer(int64), allocatable :: idarr(:,:)
        integer(int64), allocatable :: idlong(:)
        real(real32), allocatable :: value(:), val(:), arr(:,:)
        real(real64), allocatable :: value_64(:), arrlong(:,:)
        integer(int32), allocatable :: iarr(:,:)
        logical, allocatable :: flag(:), flag_array(:,:)
        character(len=16), allocatable :: name(:)
        character(len=8), allocatable :: name_arr(:,:)
        logical :: exists
        character(len=*), parameter :: in_file = "test_run/test_parquet.parquet"

        inquire(file=in_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        n = int(nrows)

        allocate(ids(n))
        call parquet_read_column(reader, "id0", ids)

        call check(error, ids(1) == 1_int32 .and. ids(size(ids)) == int(nrows, kind=int32))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "id0 column contents do not match expected values")
            return
        end if

        allocate(idarr(2, n), idlong(n), value(n), value_64(n), val(n), flag(n))
        allocate(arr(5, n), arrlong(5, n), iarr(3, n), flag_array(6, n))
        allocate(name(n), name_arr(3, n))

        call parquet_read_column(reader, "idarr", idarr)
        call parquet_read_column(reader, "name", name)
        call parquet_read_column(reader, "name_arr", name_arr)
        call parquet_read_column(reader, "idlong", idlong)
        call parquet_read_column(reader, "value", value)
        call parquet_read_column(reader, "value_64", value_64)
        call parquet_read_column(reader, "arr", arr)
        call parquet_read_column(reader, "arrlong", arrlong)
        call parquet_read_column(reader, "val", val)
        call parquet_read_column(reader, "iarr", iarr)
        call parquet_read_column(reader, "myflag", flag)
        call parquet_read_column(reader, "flag_array", flag_array)

        call check(error, idarr(1,1) == 11_int64 .and. idarr(2,n) == 202_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr column values do not match expected pattern")
            return
        end if

        call check(error, index(trim(name(1)), "var_Obj") == 1 .and. index(trim(name(n)), "var_Obj") == 1)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "name column values do not have expected prefix")
            return
        end if

        call check(error, trim(name_arr(1,1)) == "N1" .and. trim(name_arr(3,n)) == "N3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "name_arr column values do not match expected values")
            return
        end if

        call check(error, idlong(1) == 1000_int64 .and. idlong(n) == 20000_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idlong column values do not match expected values")
            return
        end if

        call check(error, abs(value(1) - 10.0_real32) < 1.0e-6_real32 .and. abs(value(n) - 200.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "value column values do not match expected values")
            return
        end if

        call check(error, abs(value_64(1) - 20.0_real64) < 1.0d-12 .and. abs(value_64(n) - 400.0_real64) < 1.0d-12)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "value_64 column values do not match expected values")
            return
        end if

        call check(error, abs(arr(1,1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr(5,n) - 25.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr column values do not match expected values")
            return
        end if

        call check(error, abs(arrlong(1,1) - 11.0_real64) < 1.0d-12 .and. abs(arrlong(5,n) - 205.0_real64) < 1.0d-12)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong column values do not match expected values")
            return
        end if

        call check(error, abs(val(1) - 0.1_real32) < 1.0e-6_real32 .and. abs(val(10) - 1.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "val column values do not match expected values")
            return
        end if

        call check(error, iarr(1,1) == 2_int32 .and. iarr(3,n) == 23_int32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr column values do not match expected values")
            return
        end if

        call check(error, (.not. flag(1)) .and. flag(n))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "myflag column values do not match expected pattern")
            return
        end if

        call check(error, flag_array(1,1) .and. flag_array(6,n))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array column values do not match expected pattern")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_parquet_file

    subroutine test_read_column_info(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows, nelem
        integer :: col_size, strlen_max
        logical :: exists
        character(len=*), parameter :: in_file = "test_run/test_parquet.parquet"

        inquire(file=in_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        call parquet_get_col_size(reader, "arr", col_size)
        call check(error, col_size == 5)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for arr")
            return
        end if

        call parquet_get_col_size(reader, "idarr", col_size)
        call check(error, col_size == 2)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for idarr")
            return
        end if

        call parquet_get_col_size(reader, "iarr", col_size)
        call check(error, col_size == 3)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for iarr")
            return
        end if

        call parquet_get_col_size(reader, "flag_array", col_size)
        call check(error, col_size == 6)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for flag_array")
            return
        end if

        call parquet_get_col_size(reader, "id0", col_size)
        call check(error, col_size == 1)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for id0")
            return
        end if

        call parquet_get_column_total_elements(reader, "arr", nelem)
        call check(error, nelem == 100_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected total element count for arr")
            return
        end if

        call parquet_get_column_total_elements(reader, "id0", nelem)
        call check(error, nelem == 20_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected total element count for id0")
            return
        end if

        call parquet_get_string_length(reader, "name", strlen_max)
        call check(error, strlen_max == 10)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected max string length for name")
            return
        end if

        call parquet_get_string_length(reader, "name_arr", strlen_max)
        call check(error, strlen_max == 2)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected max string length for name_arr")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_column_info

    subroutine test_read_array_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer :: n
        real(real32) :: arr_row(5)
        integer(int64) :: idarr_row(2)
        real(real32) :: arr_elem(20)
        integer(int64) :: idarr_elem(20)
        logical :: exists
        character(len=*), parameter :: in_file = "test_run/test_parquet.parquet"

        inquire(file=in_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        n = int(nrows)

        call parquet_read_array_row_mode(reader, "arr", arr_row, 1)
        call check(error, abs(arr_row(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 6.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "arr", arr_row, n)
        call check(error, abs(arr_row(1) - 21.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 25.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_row_mode(reader, "idarr", idarr_row, 1)
        call check(error, idarr_row(1) == 11_int64 .and. idarr_row(2) == 12_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "idarr", idarr_row, n)
        call check(error, idarr_row(1) == 201_int64 .and. idarr_row(2) == 202_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "arr", arr_elem, 1)
        call check(error, abs(arr_elem(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 21.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "arr", arr_elem, 5)
        call check(error, abs(arr_elem(1) - 6.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 25.0_real32) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr element_mode values do not match expected values for element 5")
            return
        end if

        call parquet_read_array_element_mode(reader, "idarr", idarr_elem, 1)
        call check(error, idarr_elem(1) == 11_int64 .and. idarr_elem(n) == 201_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "idarr", idarr_elem, 2)
        call check(error, idarr_elem(1) == 12_int64 .and. idarr_elem(n) == 202_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr element_mode values do not match expected values for element 2")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_array_modes

    subroutine test_get_library_version(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: ver_string

        ver_string = get_parquet_fortran_version()

        call check(error, len_trim(ver_string) > 0)
        if (allocated(error)) then
            call test_failed(error, "library version string is empty")
            return
        end if
    end subroutine test_get_library_version

    subroutine test_get_parquet_maml_examples(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_maml_maml_example()
        call check(error, trim(maml%name) == "maml_example.maml" .and. size(maml%lines) == 76)
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example returned unexpected content")
            return
        end if

        maml = parquet_maml_maml_example2()
        call check(error, trim(maml%name) == "maml_example2.maml" .and. size(maml%lines) == 13)
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example2 returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example.maml")
        call check(error, trim(maml%name) == "maml_example.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example")
        call check(error, trim(maml%name) == "maml_example.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example2.maml")
        call check(error, trim(maml%name) == "maml_example2.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example2.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example2")
        call check(error, trim(maml%name) == "maml_example2.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example2') returned unexpected content")
            return
        end if
    end subroutine test_get_parquet_maml_examples
    !
end module test_reading
