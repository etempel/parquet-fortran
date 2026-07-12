!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> NB: several tests below read "test_run/test_simple.parquet" and
!> "test_run/test_parquet.parquet" without writing them first -- those files
!> are only ever produced by test_writing.f90 (collect_tests_parquet_writing).
!> This is an undocumented-to-the-runtime, order-dependent assumption: it
!> works only because run_tester.f90 happens to list the "writing" suite
!> before "reading". If that order ever changes, these tests would fail
!> (file not found) with no explanatory trail pointing back here -- if you
!> reorder run_tester.f90's testsuites array, check this comment first.
module test_reading
    use parquet
    use parquet_maml_base
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
            new_unittest("read simple parquet file", test_read_simple_parquet_file), &
            new_unittest("read parquet file", test_read_parquet_file), &
            new_unittest("read column info", test_read_column_info), &
            new_unittest("read array modes", test_read_array_modes), &
            new_unittest("read array modes for int32/float64/boolean vectors", &
                test_read_array_modes_more_types), &
            new_unittest("read string vector with a short first element (row/element/matrix modes, get_string_length)", &
                test_read_string_vector_short_first), &
            new_unittest("get library version", test_get_library_version), &
            new_unittest("get parquet maml examples", test_get_parquet_maml_examples), &
            new_unittest("read scalar column with null_value", test_read_scalar_null_value), &
            new_unittest("read scalar column with is_valid", test_read_scalar_is_valid), &
            new_unittest("read scalar column with both null_value and is_valid", test_read_scalar_both), &
            new_unittest("read string column with null_value and is_valid", test_read_string_null), &
            new_unittest("read array column with null_value and is_valid", test_read_array_null), &
            new_unittest("read list-encoded vector column", test_read_list_vector_column) &
            ]
    end subroutine collect_tests_parquet_reading

    subroutine test_read_simple_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        real(real64), dimension(:),allocatable :: xdata
        real(real32), dimension(:),allocatable :: xdata32
        real(real64), dimension(:,:),allocatable :: arr
        logical :: exists
        character(len=*), parameter :: in_file = "test_run/test_simple.parquet"
        integer:: nrows, nelem, ntot
        !
        inquire(file=in_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_simple.parquet")
            return
        end if
        !
        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        !
        call check(error, nrows == 5)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in simple parquet file")
            return
        end if
        !
        allocate(xdata(nrows))
        allocate(xdata32(nrows))
        !
        call parquet_read_column(reader, "colx", xdata)
        call check(error, xdata(1) == 1.0_real64 .and. xdata(size(xdata)) == real(nrows, kind=real64))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "colx column contents do not match expected values")
            return
        end if

        call parquet_read_column(reader, "colx", xdata32)
        call check(error, xdata32(1) == 1.0_real32 .and. xdata32(size(xdata32)) == real(nrows, kind=real32))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "colx column contents do not match expected values (xdata32)")
            return
        end if

        call parquet_get_col_size(reader, "arr", nelem)
        call parquet_get_column_total_elements(reader, "arr", ntot)

        call check(error, nelem == 3 .and. ntot == 15)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column size or total elements for arr")
            return
        end if

        allocate(arr(3, nrows))
        call parquet_read_column(reader, "arr", arr)
        call check(error, arr(1,1) == 2.0_real64 .and. arr(3,nrows) == real(nrows + 3, kind=real64))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr column contents do not match expected values")
            return
        end if
        !
        call parquet_close_reader(reader)
        !
    end subroutine test_read_simple_parquet_file

    subroutine test_read_parquet_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer :: n
        integer(int32), allocatable :: ids(:)
        real(real32), allocatable :: ids_r32(:)
        real(real64), allocatable :: ids_r64(:)
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

        allocate(ids_r32(n), ids_r64(n))
        call parquet_read_column(reader, "id0", ids_r32)
        call parquet_read_column(reader, "id0", ids_r64)

        call check(error, abs(ids_r32(1) - 1.0_real32) < 1.0e-6_real32 .and. &
                          abs(ids_r32(size(ids_r32)) - real(nrows, kind=real32)) < 1.0e-6_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "id0 int32->real32 conversion failed")
            return
        end if

        call check(error, abs(ids_r64(1) - 1.0_real64) < 1.0d-12 .and. &
                          abs(ids_r64(size(ids_r64)) - real(nrows, kind=real64)) < 1.0d-12)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "id0 int32->real64 conversion failed")
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

    !> Companion to test_read_array_modes, which only exercises the float32
    !> ("arr") and int64 ("idarr") specializations of parquet_read_array_row_mode
    !> / parquet_read_array_element_mode. This covers the remaining numeric/logical
    !> kinds -- int32 ("iarr"), float64 ("arrlong") and boolean ("flag_array") --
    !> reading the same test_run/test_parquet.parquet fixture written by the
    !> "writing" suite. Expected values mirror init_test_data in test_writing.f90:
    !> row i holds iarr=[i+1,i+2,i+3], arrlong=[i*10+1 .. i*10+5] and
    !> flag_array=[mod(i+j,2)==0, j=1..6].
    subroutine test_read_array_modes_more_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer :: n
        integer(int32) :: iarr_row(3), iarr_elem(20)
        real(real64) :: arrlong_row(5), arrlong_elem(20)
        logical :: flag_row(6), flag_elem(20)
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

        ! --- int32 vector "iarr" (col_size 3): row i = [i+1, i+2, i+3] ---
        call parquet_read_array_row_mode(reader, "iarr", iarr_row, 1)
        call check(error, all(iarr_row == [2_int32, 3_int32, 4_int32]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "iarr", iarr_row, n)
        call check(error, all(iarr_row == [21_int32, 22_int32, 23_int32]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "iarr", iarr_elem, 1)
        call check(error, iarr_elem(1) == 2_int32 .and. iarr_elem(n) == 21_int32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "iarr", iarr_elem, 3)
        call check(error, iarr_elem(1) == 4_int32 .and. iarr_elem(n) == 23_int32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr element_mode values do not match expected values for element 3")
            return
        end if

        ! --- float64 vector "arrlong" (col_size 5): row i = [i*10+1 .. i*10+5] ---
        call parquet_read_array_row_mode(reader, "arrlong", arrlong_row, 1)
        call check(error, all(abs(arrlong_row - [11.0_real64, 12.0_real64, 13.0_real64, &
            14.0_real64, 15.0_real64]) < 1.0e-9_real64))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "arrlong", arrlong_row, n)
        call check(error, all(abs(arrlong_row - [201.0_real64, 202.0_real64, 203.0_real64, &
            204.0_real64, 205.0_real64]) < 1.0e-9_real64))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "arrlong", arrlong_elem, 1)
        call check(error, abs(arrlong_elem(1) - 11.0_real64) < 1.0e-9_real64 .and. &
                          abs(arrlong_elem(n) - 201.0_real64) < 1.0e-9_real64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "arrlong", arrlong_elem, 5)
        call check(error, abs(arrlong_elem(1) - 15.0_real64) < 1.0e-9_real64 .and. &
                          abs(arrlong_elem(n) - 205.0_real64) < 1.0e-9_real64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong element_mode values do not match expected values for element 5")
            return
        end if

        ! --- boolean vector "flag_array" (col_size 6): row i = [mod(i+j,2)==0, j=1..6] ---
        call parquet_read_array_row_mode(reader, "flag_array", flag_row, 1)
        call check(error, all(flag_row .eqv. [.true., .false., .true., .false., .true., .false.]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "flag_array", flag_row, n)
        call check(error, all(flag_row .eqv. [.false., .true., .false., .true., .false., .true.]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "flag_array", flag_elem, 1)
        call check(error, flag_elem(1) .eqv. .true. .and. flag_elem(n) .eqv. .false.)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "flag_array", flag_elem, 6)
        call check(error, flag_elem(1) .eqv. .false. .and. flag_elem(n) .eqv. .true.)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array element_mode values do not match expected values for element 6")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_array_modes_more_types

    !> Regression guard for a past bug where a string vector column was sized
    !> from the length of the FIRST string it read, truncating every longer
    !> string that followed. The fixture written here is a 3-row x 3-element
    !> string vector whose element at flattened position k has length k, so:
    !>   * within a row (row mode) the first element is the shortest --
    !>     row 1 -> lengths 1,2,3;
    !>   * within one element index across rows (element mode) the first row is
    !>     the shortest -- element 1 -> row lengths 1,4,7.
    !> Either truncation bug therefore corrupts a strictly-longer trailing
    !> string and fails an assertion below. Also exercises the string
    !> specialization of both array-read modes, which the fixture-based tests
    !> above do not cover.
    !>
    !> Also covers two related paths the row/element-mode checks above don't
    !> reach, both against the same shortest-first fixture (plus a scalar
    !> "note" column, also shortest-first, for the scalar case):
    !>   * a full 2D matrix read (parquet_read_column into values(:,:)), not
    !>     just single row/element reads;
    !>   * parquet_get_string_length, which must report the true maximum
    !>     across the whole column rather than the first element's length.
    subroutine test_read_string_vector_short_first(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=1), parameter :: letters(9) = &
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
        character(len=12) :: tags(9)
        character(len=12) :: note(3)
        character(len=12) :: row_buf(3), elem_buf(3)
        character(len=12) :: tags_mat(3, 3)
        integer :: nrows, k, strlen_max
        character(len=*), parameter :: out_file = "test_run/string_vector_short_first.parquet"

        ! tags(k) is a run of letters(k) of length k, laid out row-major as
        ! three rows of three elements (flattened index = (row-1)*3 + col).
        do k = 1, 9
            tags(k) = repeat(letters(k), k)
        end do

        ! note is a plain (col_size=1) string column, one value per row,
        ! also deliberately shortest-first: "z", "zzzz", "zzzzzzzzzz".
        note = [character(len=12) :: "z", repeat("z", 4), repeat("z", 10)]

        call schema%init(table="string_vector_table")
        call schema%add_field("tags", "string", array_size=12, col_size=3)
        call schema%add_field("note", "string", array_size=12)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "tags", tags)
        call parquet_write_column(writer, "note", note)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 3)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "expected 3 rows in the string-vector fixture")
            return
        end if

        ! --- row mode: each row's three strings, first element the shortest ---
        call parquet_read_array_row_mode(reader, "tags", row_buf, 1)
        call check(error, trim(row_buf(1)) == repeat("a", 1) .and. &
                          trim(row_buf(2)) == repeat("b", 2) .and. &
                          trim(row_buf(3)) == repeat("c", 3))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "row-mode row 1 strings truncated or wrong (short first element)")
            return
        end if

        call parquet_read_array_row_mode(reader, "tags", row_buf, 3)
        call check(error, trim(row_buf(1)) == repeat("g", 7) .and. &
                          trim(row_buf(2)) == repeat("h", 8) .and. &
                          trim(row_buf(3)) == repeat("i", 9))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "row-mode row 3 strings truncated or wrong")
            return
        end if

        ! --- element mode: one element index across all rows, first row shortest ---
        call parquet_read_array_element_mode(reader, "tags", elem_buf, 1)
        call check(error, trim(elem_buf(1)) == repeat("a", 1) .and. &
                          trim(elem_buf(2)) == repeat("d", 4) .and. &
                          trim(elem_buf(3)) == repeat("g", 7))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "element-mode element 1 strings truncated or wrong (short first row)")
            return
        end if

        call parquet_read_array_element_mode(reader, "tags", elem_buf, 3)
        call check(error, trim(elem_buf(1)) == repeat("c", 3) .and. &
                          trim(elem_buf(2)) == repeat("f", 6) .and. &
                          trim(elem_buf(3)) == repeat("i", 9))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "element-mode element 3 strings truncated or wrong")
            return
        end if

        ! --- full 2D matrix read (parquet_read_column into values(:,:)),
        ! not just row/element mode -- same short-first fixture, checked at
        ! both the shortest (row 1) and longest (row 3) ends. ---
        call parquet_read_column(reader, "tags", tags_mat)
        call check(error, trim(tags_mat(1,1)) == repeat("a", 1) .and. &
                          trim(tags_mat(2,1)) == repeat("b", 2) .and. &
                          trim(tags_mat(3,1)) == repeat("c", 3) .and. &
                          trim(tags_mat(1,3)) == repeat("g", 7) .and. &
                          trim(tags_mat(2,3)) == repeat("h", 8) .and. &
                          trim(tags_mat(3,3)) == repeat("i", 9))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "full matrix read of a string-vector column truncated or wrong (short first element)")
            return
        end if

        ! --- parquet_get_string_length must reflect the true maximum across
        ! the whole column, not just the (deliberately shortest) first value. ---
        call parquet_get_string_length(reader, "tags", strlen_max)
        call check(error, strlen_max == 9)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "parquet_get_string_length for a string-vector column ignored a longer, later element")
            return
        end if

        call parquet_get_string_length(reader, "note", strlen_max)
        call check(error, strlen_max == 10)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "parquet_get_string_length for a scalar string column ignored a longer, later element")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_string_vector_short_first

    subroutine test_get_library_version(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: ver_string, internal_ver_string

        call parquet_get_version(ver_string)

        call check(error, len_trim(ver_string) > 0)
        if (allocated(error)) then
            call test_failed(error, "library version string is empty")
            return
        end if

        ! internal=.true. returns the full internal version string (e.g.
        ! "v0.9.0 (2026-07-11)": v-prefixed, with a trailing date), as opposed
        ! to the default release-number-only form (e.g. "0.9.0") -- see the
        ! parquet_get_version entry in MANUAL.md's "parquet module API".
        call parquet_get_version(internal_ver_string, internal=.true.)

        call check(error, len_trim(internal_ver_string) > 0 .and. internal_ver_string(1:1) == "v" .and. &
            index(internal_ver_string, "(") > 0)
        if (allocated(error)) then
            call test_failed(error, "parquet_get_version(internal=.true.) did not return a v-prefixed, dated internal version string")
            return
        end if
    end subroutine test_get_library_version

    subroutine test_get_parquet_maml_examples(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_maml_maml_example()
        call check(error, trim(maml%name) == "maml_example.maml" .and. size(maml%lines) == 91)
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example returned unexpected content")
            return
        end if

        maml = parquet_maml_maml_example2()
        call check(error, trim(maml%name) == "maml_example2.maml" .and. size(maml%lines) == 83)
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

        maml = parquet_maml_maml_example3()
        call check(error, trim(maml%name) == "maml_example3.maml" .and. size(maml%lines) == 48)
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example3 returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example3.maml")
        call check(error, trim(maml%name) == "maml_example3.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example3.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example3")
        call check(error, trim(maml%name) == "maml_example3.maml")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example3') returned unexpected content")
            return
        end if
    end subroutine test_get_parquet_maml_examples

    subroutine test_read_scalar_null_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: values(3)
        character(len=*), parameter :: in_file = "test/fixtures/has_null.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "id_with_null", values, null_value=-1_int32)
        call parquet_close_reader(reader)

        call check(error, values(1) == 1_int32 .and. values(2) == -1_int32 .and. values(3) == 3_int32)
        if (allocated(error)) then
            call test_failed(error, "null_value substitution did not replace the Null in row 2")
            return
        end if
    end subroutine test_read_scalar_null_value

    subroutine test_read_scalar_is_valid(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: values(3)
        logical :: valid(3)
        character(len=*), parameter :: in_file = "test/fixtures/has_null.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "id_with_null", values, is_valid=valid)
        call parquet_close_reader(reader)

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3))
        if (allocated(error)) then
            call test_failed(error, "is_valid mask did not correctly flag the Null in row 2")
            return
        end if

        ! Without null_value, the Null slot must still get a safe type-default
        ! (0), never Arrow's undefined buffer content.
        call check(error, values(2) == 0_int32)
        if (allocated(error)) then
            call test_failed(error, "is_valid-only read did not default the Null slot to 0")
            return
        end if
    end subroutine test_read_scalar_is_valid

    subroutine test_read_scalar_both(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: values(3)
        logical :: valid(3)
        character(len=*), parameter :: in_file = "test/fixtures/has_null.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "id_with_null", values, null_value=-99_int32, is_valid=valid)
        call parquet_close_reader(reader)

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3))
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect when combined with null_value")
            return
        end if

        call check(error, values(1) == 1_int32 .and. values(2) == -99_int32 .and. values(3) == 3_int32)
        if (allocated(error)) then
            call test_failed(error, "null_value substitution incorrect when combined with is_valid")
            return
        end if
    end subroutine test_read_scalar_both

    subroutine test_read_string_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=16) :: values(3)
        logical :: valid(3)
        character(len=*), parameter :: in_file = "test/fixtures/has_null.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "name_with_null", values, null_value="MISSING", is_valid=valid)
        call parquet_close_reader(reader)

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3))
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect for string column with a Null")
            return
        end if

        call check(error, trim(values(1)) == "first" .and. trim(values(2)) == "MISSING" .and. trim(values(3)) == "third")
        if (allocated(error)) then
            call test_failed(error, "null_value substitution incorrect for string column")
            return
        end if
    end subroutine test_read_string_null

    subroutine test_read_array_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: values(2, 3)
        logical :: valid(2, 3)
        character(len=*), parameter :: in_file = "test/fixtures/has_null.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "arr_with_null", values, null_value=-1_int32, is_valid=valid)
        call parquet_close_reader(reader)

        ! Row 1: [1, 2], row 2: [3, 4], row 3: [5, Null].
        call check(error, all(valid(:, 1)) .and. all(valid(:, 2)) .and. valid(1, 3) .and. (.not. valid(2, 3)))
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect for array column with an element-level Null")
            return
        end if

        call check(error, values(1,1) == 1_int32 .and. values(2,1) == 2_int32 .and. &
                          values(1,2) == 3_int32 .and. values(2,2) == 4_int32 .and. &
                          values(1,3) == 5_int32 .and. values(2,3) == -1_int32)
        if (allocated(error)) then
            call test_failed(error, "null_value substitution incorrect for array column")
            return
        end if
    end subroutine test_read_array_null

    !> Reads test/fixtures/list_vector.parquet, whose "spec" vector column is
    !> stored with Arrow's variable-length `list<element: double>` encoding
    !> (the standard 3-level Parquet LIST layout) rather than the
    !> `fixed_size_list` this library's own writer emits. Every row is length
    !> 3, so it is a well-formed uniform vector column; this test proves the
    !> read side treats that alternate on-disk schema identically to a
    !> fixed_size_list -- same col_size, total elements, and values. See
    !> tools/generate_fixtures.cpp (generate_list_vector_fixture).
    subroutine test_read_list_vector_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=*), parameter :: in_file = "test/fixtures/list_vector.parquet"
        logical :: exists
        integer :: nrows, nelem, ntot, row
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: ra(:)
        real(real64), allocatable :: spec(:,:)
        logical :: ok
        !
        inquire(file=in_file, exist=exists)
        call check(error, exists)
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test/fixtures/list_vector.parquet")
            return
        end if
        !
        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 4)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in list_vector fixture")
            return
        end if
        !
        ! The list-encoded vector column reports its shape exactly like a
        ! fixed_size_list column would: uniform per-row length 3, 12 total.
        call parquet_get_col_size(reader, "spec", nelem)
        call parquet_get_column_total_elements(reader, "spec", ntot)
        call check(error, nelem == 3 .and. ntot == 12)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected col_size/total elements for list-encoded spec column")
            return
        end if
        !
        allocate(id(nrows), ra(nrows), spec(nelem, nrows))
        call parquet_read_column(reader, "ID", id)
        call parquet_read_column(reader, "ra", ra)
        call parquet_read_column(reader, "spec", spec)
        call parquet_close_reader(reader)
        !
        call check(error, all(id == [10_int32, 20_int32, 30_int32, 40_int32]))
        if (allocated(error)) then
            call test_failed(error, "scalar ID column alongside list-encoded vector read incorrectly")
            return
        end if
        call check(error, all(abs(ra - [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64]) < 1.0e-12_real64))
        if (allocated(error)) then
            call test_failed(error, "scalar ra column alongside list-encoded vector read incorrectly")
            return
        end if
        !
        ! Row r (0-based) holds [r+0.1, r+0.2, r+0.3]; spec is (nelem, nrows).
        ok = .true.
        do row = 1, nrows
            ok = ok .and. abs(spec(1, row) - (real(row - 1, real64) + 0.1_real64)) < 1.0e-12_real64
            ok = ok .and. abs(spec(2, row) - (real(row - 1, real64) + 0.2_real64)) < 1.0e-12_real64
            ok = ok .and. abs(spec(3, row) - (real(row - 1, real64) + 0.3_real64)) < 1.0e-12_real64
        end do
        call check(error, ok)
        if (allocated(error)) then
            call test_failed(error, "list-encoded spec vector column values do not match expected")
            return
        end if
    end subroutine test_read_list_vector_column
    !
end module test_reading
