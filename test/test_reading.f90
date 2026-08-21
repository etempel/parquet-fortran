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
    use parquet_strings, only : parquet_string_column
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_random_vectors, only : samp_label, n_samp, n_samp_frac, samp_seed, samp_row, &
        samp_u_bits, samp_frac_bits, samp_keep
    use test_errors, only : check_scenario_exit_status
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
            new_unittest("read array row_mode with an integer(int64) row_index", &
                test_read_array_row_mode_int64_row_index), &
            new_unittest("read string vector with a short first element (row/element/matrix modes, get_string_length)", &
                test_read_string_vector_short_first), &
            new_unittest("get library version", test_get_library_version), &
            new_unittest("get parquet maml examples", test_get_parquet_maml_examples), &
            new_unittest("read scalar column with null_value", test_read_scalar_null_value), &
            new_unittest("read scalar column with is_valid", test_read_scalar_is_valid), &
            new_unittest("read scalar column with both null_value and is_valid", test_read_scalar_both), &
            new_unittest("read string column with null_value and is_valid", test_read_string_null), &
            new_unittest("read array column with null_value and is_valid", test_read_array_null), &
            new_unittest("read list-encoded vector column", test_read_list_vector_column), &
            new_unittest("the LIST width footer screen and its proof disagree where expected", &
                test_list_width_screen_and_proof), &
            new_unittest("is_valid on a null-free column comes back uniformly true", &
                test_is_valid_on_clean_column), &
            new_unittest("array row-mode read at a row_index beyond the first row group", &
                test_read_array_row_mode_beyond_first_row_group), &
            new_unittest("null_value on a Null-containing vector column (full/row/element modes, every type)", &
                test_read_vector_null_value_all_types), &
            new_unittest("is_valid on a vector column with zero Nulls anywhere (full/row/element modes)", &
                test_read_vector_is_valid_no_nulls_present), &
            new_unittest("chunked read (parquet_read_column_chunk): every type/shape round-trips row group by " // &
                "row group", test_read_column_chunk_all_types_roundtrip), &
            new_unittest("chunked read: int64 row_group kind, parquet_get_num_row_groups, " // &
                "parquet_get_chunk_size(reader,...)", test_read_column_chunk_int64_row_group), &
            new_unittest("chunked read: numeric kinds convert exactly as on the whole-column path", &
                test_read_column_chunk_numeric_conversion), &
            new_unittest("chunked read: parquet_close_reader(check_complete=.true.) passes when every row " // &
                "group was read", test_read_column_chunk_check_complete_pass), &
            new_unittest("chunked read: parquet_close_reader(check_complete=.true., check_hard=.false.) " // &
                "warns instead of aborting on an incomplete read", test_read_column_chunk_check_complete_soft_warns), &
            new_unittest("parquet_get_col_size/parquet_get_column_total_elements/parquet_read_array_row_mode/" // &
                "parquet_read_array_element_mode avoid a whole-column read", &
                test_col_size_and_row_mode_avoid_whole_column_read), &
            new_unittest("parquet_get_col_size/parquet_get_column_total_elements avoid a whole-column " // &
                "read on a plain LIST column", test_plain_list_size_queries_avoid_whole_column_read), &
            new_unittest("read extended source types (int8/16, uint8/16/32/64, half_float, decimal32/64/128/256)", &
                test_read_extended_types), &
            new_unittest("read a real32 column as real64", test_read_float32_column_as_float64), &
            new_unittest("integer-to-real reads are NOT checked for precision loss", &
                test_int_to_real_precision_is_unchecked), &
            new_unittest("row filter on an extended (int8) source type column", &
                test_filter_extended_type), &
            new_unittest("row filter passing-comparison on a half_float column", &
                test_filter_extended_type_half_float), &
            new_unittest("row filter passing-comparison on a uint64 column", &
                test_filter_extended_type_uint64), &
            new_unittest("row filter passing-comparison on a decimal32 column", &
                test_filter_extended_type_decimal32), &
            new_unittest("read nested struct-field scalar/vector leaves (arbitrary depth, combined nulls)", &
                test_read_nested_struct_leaves), &
            new_unittest("read a struct-nested string leaf into a compact parquet_string_column", &
                test_read_nested_struct_string_compact), &
            new_unittest("row filter on a nested struct-field leaf", &
                test_filter_nested_struct_leaf), &
            new_unittest("date/time/timestamp: row-mode and element-mode reads on a vector column", &
                test_datetime_row_element_mode), &
            new_unittest("array element-mode read on a genuinely zero-row vector column does not crash", &
                test_read_array_element_mode_zero_rows), &
            new_unittest("date/time/timestamp: foreign INT96 and non-UTC-timezone fixtures round-trip", &
                test_datetime_foreign_fixtures), &
            new_unittest("date/time/timestamp: row-mode and element-mode reads under an active row filter", &
                test_datetime_array_filtered), &
            new_unittest("int32/boolean/string: row-mode and element-mode reads under an active row filter", &
                test_array_row_element_mode_filtered), &
            new_unittest("sample mapping: (seed, row) -> uniform and keep match the frozen oracle", &
                test_sample_mapping_golden), &
            new_unittest("sample_fraction absent or >= 1.0 reads every row (current/default behavior)", &
                test_sample_fraction_no_op), &
            new_unittest("sample_fraction == 0.0 deterministically yields zero rows", &
                test_sample_fraction_zero), &
            new_unittest("sample_fraction with sample_seed > 0 is reproducible across two opens", &
                test_sample_fraction_seed_reproducible), &
            new_unittest("sample_fraction with sample_seed absent draws a fresh (differing) sample each open", &
                test_sample_fraction_entropy_seed_differs), &
            new_unittest("sample_fraction combined with filter= applies the filter on top of the downsample", &
                test_sample_fraction_with_filter), &
            new_unittest("sample_seed uses the full int64 width: two seeds differing only above bit 31 differ", &
                test_sample_seed_is_full_width), &
            new_unittest("the deferred draw (filter= follows) selects the same rows as the immediate one", &
                test_sample_deferred_equals_immediate), &
            new_unittest("plain LIST/LARGE_LIST columns from a foreign-written file (col_size, string length, print_stat)", &
                test_list_type_foreign_fixture), &
            new_unittest("parquet_column_exists/parquet_get_column_type: all 9 canonical types, group aliases, " // &
                "case-insensitivity, missing columns, struct-leaf paths, and a foreign-typed column", &
                test_column_exists_and_get_column_type), &
            new_unittest("parquet_get_column_type reports the narrowest lossless Fortran kind", &
                test_column_type_narrowest_lossless_mapping), &
            new_unittest("parquet_column_exists(types=) asks 'can I read it as this?'", &
                test_column_exists_types_alias_asymmetry), &
            new_unittest("parquet_get_column_names lists every column, expanding nested structs " // &
                "into dotted leaf paths", &
                test_get_column_names), &
            new_unittest("parquet_release_column frees a column's buffers without changing any result", &
                test_release_column) &
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
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_simple.parquet")
            return
        end if
        !
        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        !
        call check(error, nrows == 5, &
            "nrows == 5")
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
        call check(error, xdata(1) == 1.0_real64 .and. xdata(size(xdata)) == real(nrows, kind=real64), &
            "xdata(1) == 1.0_real64 .and. xdata(size(xdata)) == real(nrows, kind=real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "colx column contents do not match expected values")
            return
        end if

        call parquet_read_column(reader, "colx", xdata32)
        call check(error, xdata32(1) == 1.0_real32 .and. xdata32(size(xdata32)) == real(nrows, kind=real32), &
            "xdata32(1) == 1.0_real32 .and. xdata32(size(xdata32)) == real(nrows, kind=real32)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "colx column contents do not match expected values (xdata32)")
            return
        end if

        call parquet_get_col_size(reader, "arr", nelem)
        call parquet_get_column_total_elements(reader, "arr", ntot)

        call check(error, nelem == 3 .and. ntot == 15, &
            "nelem == 3 .and. ntot == 15")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column size or total elements for arr")
            return
        end if

        allocate(arr(3, nrows))
        call parquet_read_column(reader, "arr", arr)
        call check(error, arr(1,1) == 2.0_real64 .and. arr(3,nrows) == real(nrows + 3, kind=real64), &
            "arr(1,1) == 2.0_real64 .and. arr(3,nrows) == real(nrows + 3, kind=real64)")
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
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64, &
            "nrows == 20_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        n = int(nrows)

        allocate(ids(n))
        call parquet_read_column(reader, "id0", ids)

        call check(error, ids(1) == 1_int32 .and. ids(size(ids)) == int(nrows, kind=int32), &
            "ids(1) == 1_int32 .and. ids(size(ids)) == int(nrows, kind=int32)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "id0 column contents do not match expected values")
            return
        end if

        allocate(ids_r32(n), ids_r64(n))
        call parquet_read_column(reader, "id0", ids_r32)
        call parquet_read_column(reader, "id0", ids_r64)

        call check(error, abs(ids_r32(1) - 1.0_real32) < 1.0e-6_real32 .and. abs(ids_r32(size(ids_r32)) - real(nrows,kind=real32)) &
            < 1.0e-6_real32, &
            "abs(ids_r32(1) - 1.0_real32) < 1.0e-6_real32 .and. abs(ids_r32(size(ids_r32)) - real(nrows, kind=real32)) < 1.0e-6_re")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "id0 int32->real32 conversion failed")
            return
        end if

        call check(error, abs(ids_r64(1) - 1.0_real64) < 1.0d-12 .and. abs(ids_r64(size(ids_r64)) - real(nrows,kind=real64)) < &
            1.0d-12, &
            "abs(ids_r64(1) - 1.0_real64) < 1.0d-12 .and. abs(ids_r64(size(ids_r64)) - real(nrows, kind=real64)) < 1.0d-12")
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

        call check(error, idarr(1,1) == 11_int64 .and. idarr(2,n) == 202_int64, &
            "idarr(1,1) == 11_int64 .and. idarr(2,n) == 202_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr column values do not match expected pattern")
            return
        end if

        call check(error, index(trim(name(1)), "var_Obj") == 1 .and. index(trim(name(n)), "var_Obj") == 1, &
            "index(trim(name(1)), ""var_Obj"") == 1 .and. index(trim(name(n)), ""var_Obj"") == 1")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "name column values do not have expected prefix")
            return
        end if

        call check(error, trim(name_arr(1,1)) == "N1" .and. trim(name_arr(3,n)) == "N3", &
            "trim(name_arr(1,1)) == ""N1"" .and. trim(name_arr(3,n)) == ""N3""")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "name_arr column values do not match expected values")
            return
        end if

        call check(error, idlong(1) == 1000_int64 .and. idlong(n) == 20000_int64, &
            "idlong(1) == 1000_int64 .and. idlong(n) == 20000_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idlong column values do not match expected values")
            return
        end if

        call check(error, abs(value(1) - 10.0_real32) < 1.0e-6_real32 .and. abs(value(n) - 200.0_real32) < 1.0e-6_real32, &
            "abs(value(1) - 10.0_real32) < 1.0e-6_real32 .and. abs(value(n) - 200.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "value column values do not match expected values")
            return
        end if

        call check(error, abs(value_64(1) - 20.0_real64) < 1.0d-12 .and. abs(value_64(n) - 400.0_real64) < 1.0d-12, &
            "abs(value_64(1) - 20.0_real64) < 1.0d-12 .and. abs(value_64(n) - 400.0_real64) < 1.0d-12")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "value_64 column values do not match expected values")
            return
        end if

        call check(error, abs(arr(1,1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr(5,n) - 25.0_real32) < 1.0e-6_real32, &
            "abs(arr(1,1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr(5,n) - 25.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr column values do not match expected values")
            return
        end if

        call check(error, abs(arrlong(1,1) - 11.0_real64) < 1.0d-12 .and. abs(arrlong(5,n) - 205.0_real64) < 1.0d-12, &
            "abs(arrlong(1,1) - 11.0_real64) < 1.0d-12 .and. abs(arrlong(5,n) - 205.0_real64) < 1.0d-12")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong column values do not match expected values")
            return
        end if

        call check(error, abs(val(1) - 0.1_real32) < 1.0e-6_real32 .and. abs(val(10) - 1.0_real32) < 1.0e-6_real32, &
            "abs(val(1) - 0.1_real32) < 1.0e-6_real32 .and. abs(val(10) - 1.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "val column values do not match expected values")
            return
        end if

        call check(error, iarr(1,1) == 2_int32 .and. iarr(3,n) == 23_int32, &
            "iarr(1,1) == 2_int32 .and. iarr(3,n) == 23_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr column values do not match expected values")
            return
        end if

        call check(error, (.not. flag(1)) .and. flag(n), &
            "(.not. flag(1)) .and. flag(n)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "myflag column values do not match expected pattern")
            return
        end if

        call check(error, flag_array(1,1) .and. flag_array(6,n), &
            "flag_array(1,1) .and. flag_array(6,n)")
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
        integer :: col_size, strlen_max, list_w
        logical :: exists
        character(len=*), parameter :: in_file = "test_run/test_parquet.parquet"

        inquire(file=in_file, exist=exists)
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64, &
            "nrows == 20_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        call parquet_get_col_size(reader, "arr", col_size)
        call check(error, col_size == 5, &
            "col_size == 5")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for arr")
            return
        end if

        call parquet_get_col_size(reader, "idarr", col_size)
        call check(error, col_size == 2, &
            "col_size == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for idarr")
            return
        end if

        call parquet_get_col_size(reader, "iarr", col_size)
        call check(error, col_size == 3, &
            "col_size == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for iarr")
            return
        end if

        call parquet_get_col_size(reader, "flag_array", col_size)
        call check(error, col_size == 6, &
            "col_size == 6")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for flag_array")
            return
        end if

        call parquet_get_col_size(reader, "id0", col_size)
        call check(error, col_size == 1, &
            "col_size == 1")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected column array size for id0")
            return
        end if

        call parquet_get_column_total_elements(reader, "arr", nelem)
        call check(error, nelem == 100_int64, &
            "nelem == 100_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected total element count for arr")
            return
        end if

        ! parquet_measure_list_width's own non-data-needing fallback (candidate AND proven), for a
        ! genuine FIXED_SIZE_LIST vector column -- distinct from every other column above, which
        ! only ever went through parquet_get_col_size's own separate, direct FIXED_SIZE_LIST branch.
        call parquet_measure_list_width(reader, "arr", 0, 0, .false., list_w)
        call check(error, list_w == 5, "the unproven candidate for a FIXED_SIZE_LIST column is its own list_size")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected candidate list width for FIXED_SIZE_LIST column arr")
            return
        end if
        call parquet_measure_list_width(reader, "arr", 0, 0, .true., list_w)
        call check(error, list_w == 5, "the proven width for a FIXED_SIZE_LIST column is its own list_size")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected proven list width for FIXED_SIZE_LIST column arr")
            return
        end if

        call parquet_get_column_total_elements(reader, "id0", nelem)
        call check(error, nelem == 20_int64, &
            "nelem == 20_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected total element count for id0")
            return
        end if

        call parquet_get_string_length(reader, "name", strlen_max)
        call check(error, strlen_max == 10, &
            "strlen_max == 10")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected max string length for name")
            return
        end if

        call parquet_get_string_length(reader, "name_arr", strlen_max)
        call check(error, strlen_max == 2, &
            "strlen_max == 2")
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
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)

        call check(error, nrows == 20_int64, &
            "nrows == 20_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if

        n = int(nrows)

        call parquet_read_array_row_mode(reader, "arr", arr_row, 1)
        call check(error, abs(arr_row(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 6.0_real32) < 1.0e-6_real32, &
            "abs(arr_row(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 6.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "arr", arr_row, n)
        call check(error, abs(arr_row(1) - 21.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 25.0_real32) < 1.0e-6_real32, &
            "abs(arr_row(1) - 21.0_real32) < 1.0e-6_real32 .and. abs(arr_row(5) - 25.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_row_mode(reader, "idarr", idarr_row, 1)
        call check(error, idarr_row(1) == 11_int64 .and. idarr_row(2) == 12_int64, &
            "idarr_row(1) == 11_int64 .and. idarr_row(2) == 12_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "idarr", idarr_row, n)
        call check(error, idarr_row(1) == 201_int64 .and. idarr_row(2) == 202_int64, &
            "idarr_row(1) == 201_int64 .and. idarr_row(2) == 202_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "arr", arr_elem, 1)
        call check(error, abs(arr_elem(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 21.0_real32) < 1.0e-6_real32, &
            "abs(arr_elem(1) - 2.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 21.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "arr", arr_elem, 5)
        call check(error, abs(arr_elem(1) - 6.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 25.0_real32) < 1.0e-6_real32, &
            "abs(arr_elem(1) - 6.0_real32) < 1.0e-6_real32 .and. abs(arr_elem(n) - 25.0_real32) < 1.0e-6_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arr element_mode values do not match expected values for element 5")
            return
        end if

        call parquet_read_array_element_mode(reader, "idarr", idarr_elem, 1)
        call check(error, idarr_elem(1) == 11_int64 .and. idarr_elem(n) == 201_int64, &
            "idarr_elem(1) == 11_int64 .and. idarr_elem(n) == 201_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "idarr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "idarr", idarr_elem, 2)
        call check(error, idarr_elem(1) == 12_int64 .and. idarr_elem(n) == 202_int64, &
            "idarr_elem(1) == 12_int64 .and. idarr_elem(n) == 202_int64")
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
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test_run/test_parquet.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 20_int64, &
            "nrows == 20_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in parquet file")
            return
        end if
        n = int(nrows)

        ! --- int32 vector "iarr" (col_size 3): row i = [i+1, i+2, i+3] ---
        call parquet_read_array_row_mode(reader, "iarr", iarr_row, 1)
        call check(error, all(iarr_row == [2_int32, 3_int32, 4_int32]), &
            "all(iarr_row == [2_int32, 3_int32, 4_int32])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "iarr", iarr_row, n)
        call check(error, all(iarr_row == [21_int32, 22_int32, 23_int32]), &
            "all(iarr_row == [21_int32, 22_int32, 23_int32])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "iarr", iarr_elem, 1)
        call check(error, iarr_elem(1) == 2_int32 .and. iarr_elem(n) == 21_int32, &
            "iarr_elem(1) == 2_int32 .and. iarr_elem(n) == 21_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "iarr", iarr_elem, 3)
        call check(error, iarr_elem(1) == 4_int32 .and. iarr_elem(n) == 23_int32, &
            "iarr_elem(1) == 4_int32 .and. iarr_elem(n) == 23_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "iarr element_mode values do not match expected values for element 3")
            return
        end if

        ! --- float64 vector "arrlong" (col_size 5): row i = [i*10+1 .. i*10+5] ---
        call parquet_read_array_row_mode(reader, "arrlong", arrlong_row, 1)
        call check(error, all(abs(arrlong_row - [11.0_real64,12.0_real64,13.0_real64,14.0_real64,15.0_real64]) < 1.0e-9_real64), &
            "all(abs(arrlong_row - [11.0_real64, 12.0_real64, 13.0_real64, 14.0_real64, 15.0_real64]) < 1.0e-9_real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "arrlong", arrlong_row, n)
        call check(error, all(abs(arrlong_row - [201.0_real64,202.0_real64,203.0_real64,204.0_real64,205.0_real64]) < &
            1.0e-9_real64), &
            "all(abs(arrlong_row - [201.0_real64, 202.0_real64, 203.0_real64, 204.0_real64, 205.0_real64]) < 1.0e-9_real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "arrlong", arrlong_elem, 1)
        call check(error, abs(arrlong_elem(1) - 11.0_real64) < 1.0e-9_real64 .and. abs(arrlong_elem(n) - 201.0_real64) < &
            1.0e-9_real64, &
            "abs(arrlong_elem(1) - 11.0_real64) < 1.0e-9_real64 .and. abs(arrlong_elem(n) - 201.0_real64) < 1.0e-9_real64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "arrlong", arrlong_elem, 5)
        call check(error, abs(arrlong_elem(1) - 15.0_real64) < 1.0e-9_real64 .and. abs(arrlong_elem(n) - 205.0_real64) < &
            1.0e-9_real64, &
            "abs(arrlong_elem(1) - 15.0_real64) < 1.0e-9_real64 .and. abs(arrlong_elem(n) - 205.0_real64) < 1.0e-9_real64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "arrlong element_mode values do not match expected values for element 5")
            return
        end if

        ! --- boolean vector "flag_array" (col_size 6): row i = [mod(i+j,2)==0, j=1..6] ---
        call parquet_read_array_row_mode(reader, "flag_array", flag_row, 1)
        call check(error, all(flag_row .eqv. [.true., .false., .true., .false., .true., .false.]), &
            "all(flag_row .eqv. [.true., .false., .true., .false., .true., .false.])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array row_mode values do not match expected values for row 1")
            return
        end if

        call parquet_read_array_row_mode(reader, "flag_array", flag_row, n)
        call check(error, all(flag_row .eqv. [.false., .true., .false., .true., .false., .true.]), &
            "all(flag_row .eqv. [.false., .true., .false., .true., .false., .true.])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array row_mode values do not match expected values for last row")
            return
        end if

        call parquet_read_array_element_mode(reader, "flag_array", flag_elem, 1)
        call check(error, flag_elem(1) .eqv. .true. .and. flag_elem(n) .eqv. .false., &
            "flag_elem(1) .eqv. .true. .and. flag_elem(n) .eqv. .false.")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array element_mode values do not match expected values for element 1")
            return
        end if

        call parquet_read_array_element_mode(reader, "flag_array", flag_elem, 6)
        call check(error, flag_elem(1) .eqv. .false. .and. flag_elem(n) .eqv. .true., &
            "flag_elem(1) .eqv. .false. .and. flag_elem(n) .eqv. .true.")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "flag_array element_mode values do not match expected values for element 6")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_array_modes_more_types

    !> Exercises the integer(int64) row_index specifics of parquet_read_array_row_mode
    !> (parquet_read_<type>_array_row_mode_row_index_int64 in src/parquet_read.f90, added
    !> alongside the existing integer(int32) row_index ones so a caller can address a row
    !> beyond huge(1_int32) on a suitably large file) -- for all six types, checking an
    !> explicit integer(int64) row_index literal returns exactly the same values as the
    !> plain integer(int32) row_index call on the same row.
    subroutine test_read_array_row_mode_int64_row_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: i32v(2, 2), i32_row32(2), i32_row64(2)
        integer(int64) :: i64v(2, 2), i64_row32(2), i64_row64(2)
        real(real32) :: f32v(2, 2), f32_row32(2), f32_row64(2)
        real(real64) :: f64v(2, 2), f64_row32(2), f64_row64(2)
        logical :: boolv(2, 2), bool_row32(2), bool_row64(2)
        character(len=8) :: strv(2, 2), str_row32(2), str_row64(2)
        logical :: ok
        character(len=*), parameter :: out_file = "test_run/array_row_mode_int64_index.parquet"

        i32v = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
        i64v = reshape([10_int64, 20_int64, 30_int64, 40_int64], [2, 2])
        f32v = reshape([1.5_real32, 2.5_real32, 3.5_real32, 4.5_real32], [2, 2])
        f64v = reshape([1.25_real64, 2.25_real64, 3.25_real64, 4.25_real64], [2, 2])
        boolv = reshape([.true., .false., .false., .true.], [2, 2])
        strv = reshape([character(len=8) :: "aa", "bb", "cc", "dd"], [2, 2])

        call schema%init(table="row_mode_int64_index_table")
        call schema%add_field("i32v", "int32", col_size=2)
        call schema%add_field("i64v", "int64", col_size=2)
        call schema%add_field("f32v", "float32", col_size=2)
        call schema%add_field("f64v", "float64", col_size=2)
        call schema%add_field("boolv", "boolean", col_size=2)
        call schema%add_field("strv", "string", array_size=8, col_size=2)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "i32v", i32v)
        call parquet_write_column(writer, "i64v", i64v)
        call parquet_write_column(writer, "f32v", f32v)
        call parquet_write_column(writer, "f64v", f64v)
        call parquet_write_column(writer, "boolv", boolv)
        call parquet_write_column(writer, "strv", strv)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)

        call parquet_read_array_row_mode(reader, "i32v", i32_row32, 2)
        call parquet_read_array_row_mode(reader, "i32v", i32_row64, 2_int64)
        call parquet_read_array_row_mode(reader, "i64v", i64_row32, 2)
        call parquet_read_array_row_mode(reader, "i64v", i64_row64, 2_int64)
        call parquet_read_array_row_mode(reader, "f32v", f32_row32, 2)
        call parquet_read_array_row_mode(reader, "f32v", f32_row64, 2_int64)
        call parquet_read_array_row_mode(reader, "f64v", f64_row32, 2)
        call parquet_read_array_row_mode(reader, "f64v", f64_row64, 2_int64)
        call parquet_read_array_row_mode(reader, "boolv", bool_row32, 2)
        call parquet_read_array_row_mode(reader, "boolv", bool_row64, 2_int64)
        call parquet_read_array_row_mode(reader, "strv", str_row32, 2)
        call parquet_read_array_row_mode(reader, "strv", str_row64, 2_int64)

        call parquet_close_reader(reader)

        ok = all(i32_row64 == i32_row32) .and. all(i32_row64 == [3_int32, 4_int32])
        ok = ok .and. all(i64_row64 == i64_row32) .and. all(i64_row64 == [30_int64, 40_int64])
        ok = ok .and. all(abs(f32_row64 - f32_row32) < 1.0e-6_real32) .and. &
             all(abs(f32_row64 - [3.5_real32, 4.5_real32]) < 1.0e-6_real32)
        ok = ok .and. all(abs(f64_row64 - f64_row32) < 1.0e-10_real64) .and. &
             all(abs(f64_row64 - [3.25_real64, 4.25_real64]) < 1.0e-10_real64)
        ok = ok .and. all(bool_row64 .eqv. bool_row32) .and. all(bool_row64 .eqv. [.false., .true.])
        ok = ok .and. all(str_row64 == str_row32) .and. trim(str_row64(1)) == "cc" .and. trim(str_row64(2)) == "dd"

        call check(error, ok, "integer(int64) row_index result did not match integer(int32) row_index for one or more types")
    end subroutine test_read_array_row_mode_int64_row_index

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

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "tags", tags)
        call parquet_write_column(writer, "note", note)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 3, &
            "nrows == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "expected 3 rows in the string-vector fixture")
            return
        end if

        ! --- row mode: each row's three strings, first element the shortest ---
        call parquet_read_array_row_mode(reader, "tags", row_buf, 1)
        call check(error, trim(row_buf(1)) == repeat("a",1) .and. trim(row_buf(2)) == repeat("b",2) .and. trim(row_buf(3)) == &
            repeat("c",3), &
            "trim(row_buf(1)) == repeat(""a"", 1) .and. trim(row_buf(2)) == repeat(""b"", 2) .and. trim(row_buf(3)) == repeat(""c")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "row-mode row 1 strings truncated or wrong (short first element)")
            return
        end if

        call parquet_read_array_row_mode(reader, "tags", row_buf, 3)
        call check(error, trim(row_buf(1)) == repeat("g",7) .and. trim(row_buf(2)) == repeat("h",8) .and. trim(row_buf(3)) == &
            repeat("i",9), &
            "trim(row_buf(1)) == repeat(""g"", 7) .and. trim(row_buf(2)) == repeat(""h"", 8) .and. trim(row_buf(3)) == repeat(""i")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "row-mode row 3 strings truncated or wrong")
            return
        end if

        ! --- element mode: one element index across all rows, first row shortest ---
        call parquet_read_array_element_mode(reader, "tags", elem_buf, 1)
        call check(error, trim(elem_buf(1)) == repeat("a",1) .and. trim(elem_buf(2)) == repeat("d",4) .and. trim(elem_buf(3)) == &
            repeat("g",7), &
            "trim(elem_buf(1)) == repeat(""a"", 1) .and. trim(elem_buf(2)) == repeat(""d"", 4) .and. trim(elem_buf(3)) == repeat(")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "element-mode element 1 strings truncated or wrong (short first row)")
            return
        end if

        call parquet_read_array_element_mode(reader, "tags", elem_buf, 3)
        call check(error, trim(elem_buf(1)) == repeat("c",3) .and. trim(elem_buf(2)) == repeat("f",6) .and. trim(elem_buf(3)) == &
            repeat("i",9), &
            "trim(elem_buf(1)) == repeat(""c"", 3) .and. trim(elem_buf(2)) == repeat(""f"", 6) .and. trim(elem_buf(3)) == repeat(")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "element-mode element 3 strings truncated or wrong")
            return
        end if

        ! --- full 2D matrix read (parquet_read_column into values(:,:)),
        ! not just row/element mode -- same short-first fixture, checked at
        ! both the shortest (row 1) and longest (row 3) ends. ---
        call parquet_read_column(reader, "tags", tags_mat)
        call check(error, trim(tags_mat(1,1)) == repeat("a",1) .and. trim(tags_mat(2,1)) == repeat("b",2) .and. &
            trim(tags_mat(3,1)) == repeat("c",3) .and. trim(tags_mat(1,3)) == repeat("g",7) .and. trim(tags_mat(2,3)) == &
            repeat("h",8) .and. trim(tags_mat(3,3)) == repeat("i",9), &
            "trim(tags_mat(1,1)) == repeat(""a"", 1) .and. trim(tags_mat(2,1)) == repeat(""b"", 2) .and. trim(tags_mat(3,1)) == re")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "full matrix read of a string-vector column truncated or wrong (short first element)")
            return
        end if

        ! --- parquet_get_string_length must reflect the true maximum across
        ! the whole column, not just the (deliberately shortest) first value. ---
        call parquet_get_string_length(reader, "tags", strlen_max)
        call check(error, strlen_max == 9, &
            "strlen_max == 9")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "parquet_get_string_length for a string-vector column ignored a longer, later element")
            return
        end if

        call parquet_get_string_length(reader, "note", strlen_max)
        call check(error, strlen_max == 10, &
            "strlen_max == 10")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "parquet_get_string_length for a scalar string column ignored a longer, later element")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_read_string_vector_short_first

    subroutine test_get_library_version(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: ver_string, internal_ver_string, arrow_ver_string, parquet_ver_string

        call parquet_get_version(ver_string)

        call check(error, len_trim(ver_string) > 0, &
            "len_trim(ver_string) > 0")
        if (allocated(error)) then
            call test_failed(error, "library version string is empty")
            return
        end if

        ! mode="internal" returns the full internal version string (e.g.
        ! "v0.9.0 (2026-07-11)": v-prefixed, with a trailing date), as opposed
        ! to the default release-number-only form (e.g. "0.9.0").
        call parquet_get_version(internal_ver_string, mode="internal")

        call check(error, len_trim(internal_ver_string) > 0 .and. internal_ver_string(1:1) == "v" .and. &
            index(internal_ver_string, "(") > 0, &
            "len_trim(internal_ver_string) > 0 .and. internal_ver_string(1:1) == ""v"" .and. " // &
            "index(internal_ver_string, ""("") > 0")
        if (allocated(error)) then
            call test_failed(error, "parquet_get_version(mode='internal') did not return a v-prefixed, " // &
                "dated internal version string")
            return
        end if

        ! mode="arrow"/mode="parquet" each format major.minor.patch from the linked Arrow/Parquet
        ! C++ libraries -- the actual numbers vary by build, so just check the format.
        call parquet_get_version(arrow_ver_string, mode="arrow")

        call check(error, is_dotted_version_triplet(arrow_ver_string), &
            "is_dotted_version_triplet(arrow_ver_string)")
        if (allocated(error)) then
            call test_failed(error, "parquet_get_version(mode='arrow') did not return a major.minor.patch string, got '" // &
                arrow_ver_string // "'")
            return
        end if

        call parquet_get_version(parquet_ver_string, mode="parquet")

        call check(error, is_dotted_version_triplet(parquet_ver_string), &
            "is_dotted_version_triplet(parquet_ver_string)")
        if (allocated(error)) then
            call test_failed(error, "parquet_get_version(mode='parquet') did not return a major.minor.patch string, got '" // &
                parquet_ver_string // "'")
            return
        end if
    end subroutine test_get_library_version

    !> True if `text` matches \d+\.\d+\.\d+ (a plain major.minor.patch version string).
    function is_dotted_version_triplet(text) result(is_match)
        character(len=*), intent(in) :: text !! candidate version string.
        logical :: is_match !! .true. if text is three dot-separated non-empty digit runs.
        integer :: dot1, dot2, k

        is_match = .false.
        dot1 = index(text, ".")
        if (dot1 < 2) return
        dot2 = index(text(dot1+1:), ".")
        if (dot2 < 2) return
        dot2 = dot1 + dot2
        if (dot2 >= len_trim(text)) return

        do k = 1, len_trim(text)
            if (k == dot1 .or. k == dot2) cycle
            if (text(k:k) < "0" .or. text(k:k) > "9") return
        end do
        is_match = .true.
    end function is_dotted_version_triplet

    subroutine test_get_parquet_maml_examples(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_maml_maml_example()
        call check(error, trim(maml%name) == "maml_example.maml" .and. size(maml%lines) == 91, &
            "trim(maml%name) == ""maml_example.maml"" .and. size(maml%lines) == 91")
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example returned unexpected content")
            return
        end if

        maml = parquet_maml_maml_example2()
        call check(error, trim(maml%name) == "maml_example2.maml" .and. size(maml%lines) == 83, &
            "trim(maml%name) == ""maml_example2.maml"" .and. size(maml%lines) == 83")
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example2 returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example.maml")
        call check(error, trim(maml%name) == "maml_example.maml", &
            "trim(maml%name) == ""maml_example.maml""")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example")
        call check(error, trim(maml%name) == "maml_example.maml", &
            "trim(maml%name) == ""maml_example.maml""")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example2.maml")
        call check(error, trim(maml%name) == "maml_example2.maml", &
            "trim(maml%name) == ""maml_example2.maml""")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example2.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example2")
        call check(error, trim(maml%name) == "maml_example2.maml", &
            "trim(maml%name) == ""maml_example2.maml""")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example2') returned unexpected content")
            return
        end if

        maml = parquet_maml_maml_example3()
        call check(error, trim(maml%name) == "maml_example3.maml" .and. size(maml%lines) == 48, &
            "trim(maml%name) == ""maml_example3.maml"" .and. size(maml%lines) == 48")
        if (allocated(error)) then
            call test_failed(error, "parquet_maml_maml_example3 returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example3.maml")
        call check(error, trim(maml%name) == "maml_example3.maml", &
            "trim(maml%name) == ""maml_example3.maml""")
        if (allocated(error)) then
            call test_failed(error, "get_parquet_maml('maml_example3.maml') returned unexpected content")
            return
        end if

        maml = get_parquet_maml("maml_example3")
        call check(error, trim(maml%name) == "maml_example3.maml", &
            "trim(maml%name) == ""maml_example3.maml""")
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

        call check(error, values(1) == 1_int32 .and. values(2) == -1_int32 .and. values(3) == 3_int32, &
            "values(1) == 1_int32 .and. values(2) == -1_int32 .and. values(3) == 3_int32")
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

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3), &
            "valid(1) .and. (.not. valid(2)) .and. valid(3)")
        if (allocated(error)) then
            call test_failed(error, "is_valid mask did not correctly flag the Null in row 2")
            return
        end if

        ! Without null_value, the Null slot must still get a safe type-default
        ! (0), never Arrow's undefined buffer content.
        call check(error, values(2) == 0_int32, &
            "values(2) == 0_int32")
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

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3), &
            "valid(1) .and. (.not. valid(2)) .and. valid(3)")
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect when combined with null_value")
            return
        end if

        call check(error, values(1) == 1_int32 .and. values(2) == -99_int32 .and. values(3) == 3_int32, &
            "values(1) == 1_int32 .and. values(2) == -99_int32 .and. values(3) == 3_int32")
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

        call check(error, valid(1) .and. (.not. valid(2)) .and. valid(3), &
            "valid(1) .and. (.not. valid(2)) .and. valid(3)")
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect for string column with a Null")
            return
        end if

        call check(error, trim(values(1)) == "first" .and. trim(values(2)) == "MISSING" .and. trim(values(3)) == "third", &
            "trim(values(1)) == ""first"" .and. trim(values(2)) == ""MISSING"" .and. trim(values(3)) == ""third""")
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
        call check(error, all(valid(:, 1)) .and. all(valid(:, 2)) .and. valid(1, 3) .and. (.not. valid(2, 3)), &
            "all(valid(:, 1)) .and. all(valid(:, 2)) .and. valid(1, 3) .and. (.not. valid(2, 3))")
        if (allocated(error)) then
            call test_failed(error, "is_valid mask incorrect for array column with an element-level Null")
            return
        end if

        call check(error, values(1,1) == 1_int32 .and. values(2,1) == 2_int32 .and. values(1,2) == 3_int32 .and. values(2,2) == &
            4_int32 .and. values(1,3) == 5_int32 .and. values(2,3) == -1_int32, &
            "values(1,1) == 1_int32 .and. values(2,1) == 2_int32 .and. values(1,2) == 3_int32 .and. values(2,2) == 4_int32 .and. v")
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
        real(real64), allocatable :: row_out(:), elem_out(:)
        logical :: ok
        !
        inquire(file=in_file, exist=exists)
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test/fixtures/list_vector.parquet")
            return
        end if
        !
        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 4, &
            "nrows == 4")
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
        call check(error, nelem == 3 .and. ntot == 12, &
            "nelem == 3 .and. ntot == 12")
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
        !
        ! row-mode/element-mode reads had never been exercised against
        ! a LIST-encoded (as opposed to fixed_size_list) vector column -- get_row_list_values'
        ! own LIST branch, and resolve_element_mode_col_size's LIST/LARGE_LIST fallback, only
        ! ever ran against this library's own fixed_size_list writer output before.
        allocate(row_out(nelem), elem_out(nrows))
        call parquet_read_array_row_mode(reader, "spec", row_out, 2)
        call check(error, all(abs(row_out - [1.1_real64, 1.2_real64, 1.3_real64]) < 1.0e-12_real64), &
            "all(abs(row_out - [1.1_real64, 1.2_real64, 1.3_real64]) < 1.0e-12_real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "row-mode read of a LIST-encoded vector column did not match row 2")
            return
        end if
        call parquet_read_array_element_mode(reader, "spec", elem_out, 2)
        call check(error, all(abs(elem_out - [0.2_real64, 1.2_real64, 2.2_real64, 3.2_real64]) < 1.0e-12_real64), &
            "all(abs(elem_out - [0.2_real64, 1.2_real64, 2.2_real64, 3.2_real64]) < 1.0e-12_real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "element-mode read of a LIST-encoded vector column did not match element 2")
            return
        end if
        call parquet_close_reader(reader)
        !
        call check(error, all(id == [10_int32, 20_int32, 30_int32, 40_int32]), &
            "all(id == [10_int32, 20_int32, 30_int32, 40_int32])")
        if (allocated(error)) then
            call test_failed(error, "scalar ID column alongside list-encoded vector read incorrectly")
            return
        end if
        call check(error, all(abs(ra - [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64]) < 1.0e-12_real64), &
            "all(abs(ra - [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64]) < 1.0e-12_real64)")
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
        call check(error, ok, &
            "ok")
        if (allocated(error)) then
            call test_failed(error, "list-encoded spec vector column values do not match expected")
            return
        end if
    end subroutine test_read_list_vector_column

    !> Every prior parquet_read_array_row_mode test reads from a
    !> single-row-group file (or, when multi-row-group, only ever from row 1), so
    !> resolve_row_group_for_row's "row isn't in this group, keep looking" loop-decrement branch
    !> had never fired. Writes a 5-row, col_size=2 vector column with chunk_size=2 (3 row groups:
    !> 2, 2, 1 rows) and reads row 4, which physically falls in the second row group (local row 2).
    subroutine test_read_array_row_mode_beyond_first_row_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(2, 5), row_out(2)
        character(len=*), parameter :: out_file = "test_run/test_row_mode_beyond_first_group.parquet"
        integer :: k

        v = reshape([(k, k=1,10)], [2, 5])

        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_row_mode(reader, "v", row_out, 4)
        call parquet_close_reader(reader)

        call check(error, all(row_out == v(:, 4)), &
            "array row-mode read did not match row 4 (second row group) values")
    end subroutine test_read_array_row_mode_beyond_first_row_group

    !> The existing null_value tests read only int32/string scalar columns;
    !> this covers the null-replacement branch inside every *vector*-column read
    !> variant -- parquet_read_column (full 2D), parquet_read_array_row_mode,
    !> and parquet_read_array_element_mode -- for all six types, plus the scalar
    !> (col_size=1) read variants for the remaining numeric/logical types
    !> (int64/float32/float64/logical). A 3-row fixture is written with the last
    !> vector element (col 2, row 3) and the last scalar element (row 3)
    !> deliberately Null (is_valid=.false. there); each read passes null_value=
    !> and must substitute it exactly at the Null position, leaving the rest intact.
    subroutine test_read_vector_null_value_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        logical :: vmask(2, 3), smask(3)
        integer(int32) :: i32v(2, 3), i32_full(2, 3), i32_row(2), i32_elem(3)
        integer(int64) :: i64v(2, 3), i64_full(2, 3), i64_row(2), i64_elem(3)
        real(real32) :: f32v(2, 3), f32_full(2, 3), f32_row(2), f32_elem(3)
        real(real64) :: f64v(2, 3), f64_full(2, 3), f64_row(2), f64_elem(3)
        logical :: boolv(2, 3), bool_full(2, 3), bool_row(2), bool_elem(3)
        character(len=8) :: strv(2, 3), str_full(2, 3), str_row(2), str_elem(3)
        integer(int64) :: i64s(3), i64s_back(3)
        real(real32) :: f32s(3), f32s_back(3)
        real(real64) :: f64s(3), f64s_back(3)
        logical :: bools(3), bools_back(3)
        logical :: ok
        character(len=*), parameter :: out_file = "test_run/vector_null_value_all_types.parquet"

        ! Element (col 2, row 3) is Null; all other elements carry known values.
        vmask = .true.
        vmask(2, 3) = .false.
        ! Scalar columns: the last row (row 3) is Null.
        smask = [.true., .true., .false.]

        i32v = reshape([10_int32, 20_int32, 30_int32, 40_int32, 50_int32, 60_int32], [2, 3])
        i64v = reshape([11_int64, 21_int64, 31_int64, 41_int64, 51_int64, 61_int64], [2, 3])
        f32v = reshape([1.5_real32, 2.5_real32, 3.5_real32, 4.5_real32, 5.5_real32, 6.5_real32], [2, 3])
        f64v = reshape([1.25_real64, 2.25_real64, 3.25_real64, 4.25_real64, 5.25_real64, 6.25_real64], [2, 3])
        boolv = reshape([.true., .false., .true., .false., .true., .false.], [2, 3])
        strv = reshape(["aaa", "bbb", "ccc", "ddd", "eee", "fff"], [2, 3])
        i64s = [101_int64, 102_int64, 103_int64]
        f32s = [10.5_real32, 20.5_real32, 30.5_real32]
        f64s = [10.25_real64, 20.25_real64, 30.25_real64]
        bools = [.true., .false., .true.]

        call schema%init(table="vector_null_table")
        call schema%add_field("i32v", "int32", col_size=2)
        call schema%add_field("i64v", "int64", col_size=2)
        call schema%add_field("f32v", "float32", col_size=2)
        call schema%add_field("f64v", "float64", col_size=2)
        call schema%add_field("boolv", "boolean", col_size=2)
        call schema%add_field("strv", "string", array_size=8, col_size=2)
        call schema%add_field("i64s", "int64")
        call schema%add_field("f32s", "float32")
        call schema%add_field("f64s", "float64")
        call schema%add_field("bools", "boolean")

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "i32v", i32v, is_valid=vmask)
        call parquet_write_column(writer, "i64v", i64v, is_valid=vmask)
        call parquet_write_column(writer, "f32v", f32v, is_valid=vmask)
        call parquet_write_column(writer, "f64v", f64v, is_valid=vmask)
        call parquet_write_column(writer, "boolv", boolv, is_valid=vmask)
        call parquet_write_column(writer, "strv", strv, is_valid=vmask)
        call parquet_write_column(writer, "i64s", i64s, is_valid=smask)
        call parquet_write_column(writer, "f32s", f32s, is_valid=smask)
        call parquet_write_column(writer, "f64s", f64s, is_valid=smask)
        call parquet_write_column(writer, "bools", bools, is_valid=smask)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)

        ! --- scalar (col_size=1) reads: row 3 is Null ---
        call parquet_read_column(reader, "i64s", i64s_back, null_value=-222_int64)
        call parquet_read_column(reader, "f32s", f32s_back, null_value=-3.5_real32)
        call parquet_read_column(reader, "f64s", f64s_back, null_value=-4.5_real64)
        call parquet_read_column(reader, "bools", bools_back, null_value=.true.)

        ! --- full 2D reads: the Null lands at (2,3) ---
        call parquet_read_column(reader, "i32v", i32_full, null_value=-111_int32)
        call parquet_read_column(reader, "i64v", i64_full, null_value=-222_int64)
        call parquet_read_column(reader, "f32v", f32_full, null_value=-3.5_real32)
        call parquet_read_column(reader, "f64v", f64_full, null_value=-4.5_real64)
        call parquet_read_column(reader, "boolv", bool_full, null_value=.true.)
        call parquet_read_column(reader, "strv", str_full, null_value="NULL")

        ! --- row mode: row 3 is [<col1>, Null] ---
        call parquet_read_array_row_mode(reader, "i32v", i32_row, 3, null_value=-111_int32)
        call parquet_read_array_row_mode(reader, "i64v", i64_row, 3, null_value=-222_int64)
        call parquet_read_array_row_mode(reader, "f32v", f32_row, 3, null_value=-3.5_real32)
        call parquet_read_array_row_mode(reader, "f64v", f64_row, 3, null_value=-4.5_real64)
        call parquet_read_array_row_mode(reader, "boolv", bool_row, 3, null_value=.true.)
        call parquet_read_array_row_mode(reader, "strv", str_row, 3, null_value="NULL")

        ! --- element mode: element 2 across rows is [<row1>, <row2>, Null] ---
        call parquet_read_array_element_mode(reader, "i32v", i32_elem, 2, null_value=-111_int32)
        call parquet_read_array_element_mode(reader, "i64v", i64_elem, 2, null_value=-222_int64)
        call parquet_read_array_element_mode(reader, "f32v", f32_elem, 2, null_value=-3.5_real32)
        call parquet_read_array_element_mode(reader, "f64v", f64_elem, 2, null_value=-4.5_real64)
        call parquet_read_array_element_mode(reader, "boolv", bool_elem, 2, null_value=.true.)
        call parquet_read_array_element_mode(reader, "strv", str_elem, 2, null_value="NULL")

        call parquet_close_reader(reader)

        ! Null position substituted with null_value in every mode, and a
        ! representative non-null element left untouched.
        ok = i32_full(2, 3) == -111_int32 .and. i32_full(1, 1) == 10_int32 .and. &
             i32_row(2) == -111_int32 .and. i32_row(1) == 50_int32 .and. &
             i32_elem(3) == -111_int32 .and. i32_elem(1) == 20_int32
        ok = ok .and. i64_full(2, 3) == -222_int64 .and. i64_row(2) == -222_int64 .and. &
             i64_elem(3) == -222_int64 .and. i64_elem(1) == 21_int64
        ok = ok .and. abs(f32_full(2, 3) + 3.5_real32) < 1.0e-5_real32 .and. &
             abs(f32_row(2) + 3.5_real32) < 1.0e-5_real32 .and. abs(f32_elem(3) + 3.5_real32) < 1.0e-5_real32
        ok = ok .and. abs(f64_full(2, 3) + 4.5_real64) < 1.0e-10_real64 .and. &
             abs(f64_row(2) + 4.5_real64) < 1.0e-10_real64 .and. abs(f64_elem(3) + 4.5_real64) < 1.0e-10_real64
        ok = ok .and. bool_full(2, 3) .and. bool_row(2) .and. bool_elem(3)
        ok = ok .and. trim(str_full(2, 3)) == "NULL" .and. trim(str_row(2)) == "NULL" .and. &
             trim(str_elem(3)) == "NULL" .and. trim(str_full(1, 1)) == "aaa"
        ! Scalar reads: row 3 substituted, rows 1-2 untouched.
        ok = ok .and. i64s_back(3) == -222_int64 .and. i64s_back(1) == 101_int64 .and. &
             abs(f32s_back(3) + 3.5_real32) < 1.0e-5_real32 .and. abs(f32s_back(1) - 10.5_real32) < 1.0e-5_real32 .and. &
             abs(f64s_back(3) + 4.5_real64) < 1.0e-10_real64 .and. abs(f64s_back(1) - 10.25_real64) < 1.0e-10_real64 .and. &
             bools_back(3) .and. bools_back(1) .and. (.not. bools_back(2))

        call check(error, ok, &
            "null_value substitution failed for one or more scalar/vector-column read variants/types")
    end subroutine test_read_vector_null_value_all_types

    !> Regression test: parquet_read_column/parquet_read_array_row_mode/parquet_read_array_element_mode's
    !> shared null-reporting helpers (report_nulls_list_full/report_nulls_list_element in
    !> parquet_wrapper.cpp) used to skip populating is_valid entirely whenever the array being
    !> read happened to contain zero Nulls, leaving it as uninitialized memory instead of all
    !> .true. -- every other test in this suite reads a column/row-group that has at least one
    !> Null somewhere, so this never-nulls-at-all case was untested and the bug went unnoticed.
    !> Writes a 3-row, col_size=2 int32 vector column with no Nulls at all and asserts is_valid
    !> comes back entirely .true. via every read mode that shares those helpers.
    subroutine test_read_vector_is_valid_no_nulls_present(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/vector_is_valid_no_nulls.parquet"
        integer(int32) :: i32v(2, 3), full(2, 3), row_mode(2), elem_mode(3)
        logical :: valid_full(2, 3), valid_row(2), valid_elem(3)
        logical :: ok

        i32v = reshape([10_int32, 20_int32, 30_int32, 40_int32, 50_int32, 60_int32], [2, 3])

        call schema%init(table="vector_no_nulls_table")
        call schema%add_field("i32v", "int32", col_size=2)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "i32v", i32v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "i32v", full, is_valid=valid_full)
        call parquet_read_array_row_mode(reader, "i32v", row_mode, 2, is_valid=valid_row)
        call parquet_read_array_element_mode(reader, "i32v", elem_mode, 1, is_valid=valid_elem)
        call parquet_close_reader(reader)

        ok = all(valid_full) .and. all(full == i32v) .and. &
             all(valid_row) .and. all(row_mode == i32v(:, 2)) .and. &
             all(valid_elem) .and. all(elem_mode == i32v(1, :))

        call check(error, ok, &
            "is_valid was not all .true. (or data was corrupted) for a vector column with zero Nulls present")
    end subroutine test_read_vector_is_valid_no_nulls_present
    !
    !> Writes a 5-row, 12-column (every type x scalar/matrix) file with chunk_size=2 (forcing 3
    !> row groups: 2, 2, 1 rows), then reads it back row group by row group -- once via each
    !> row_group kind-specific of parquet_read_column_chunk/parquet_get_chunk_size (int64, then
    !> int32), plus parquet_get_num_row_groups's own int32 specific -- comparing each row group's
    !> slice against the known full data. This is the primary functional/round-trip check for the
    !> whole chunked-read feature. The last row (col 2 for vector columns) is written Null and
    !> every read passes null_value=, also covering the null-substitution branch shared by both
    !> row_group kind-specifics.
    subroutine test_read_column_chunk_all_types_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_read_chunk_all_types.parquet"
        integer, parameter :: nrows = 5
        integer(int32), parameter :: i32_null = -111_int32
        integer(int64), parameter :: i64_null = -222_int64
        real(real32), parameter :: f32_null = -3.5_real32
        real(real64), parameter :: f64_null = -4.5_real64
        logical, parameter :: log_null = .true.
        character(len=8), parameter :: str_null = "NULLSTR"
        integer(int32) :: i32s(nrows), i32v(2, nrows), exp_i32s(nrows), exp_i32v(2, nrows)
        integer(int64) :: i64s(nrows), i64v(2, nrows), exp_i64s(nrows), exp_i64v(2, nrows)
        real(real32) :: f32s(nrows), f32v(2, nrows), exp_f32s(nrows), exp_f32v(2, nrows)
        real(real64) :: f64s(nrows), f64v(2, nrows), exp_f64s(nrows), exp_f64v(2, nrows)
        logical :: logs(nrows), logv(2, nrows), exp_logs(nrows), exp_logv(2, nrows)
        character(len=8) :: strs(nrows), strv(2, nrows), exp_strs(nrows), exp_strv(2, nrows)
        logical :: vmask(2, nrows), smask(nrows)
        integer(int64) :: num_row_groups, rg, rg_size, row0, i
        integer(int32) :: num_row_groups32, rg32, rg_size32
        logical :: ok
        integer :: k

        i32s = [(k, k=1,nrows)]
        i32v = reshape([(k, k=1,2*nrows)], [2, nrows])
        i64s = [(int(k, kind=int64), k=101,100+nrows)]
        i64v = reshape([(int(k, kind=int64), k=1,2*nrows)], [2, nrows])
        f32s = [(real(k, kind=real32), k=1,nrows)]
        f32v = reshape([(real(k, kind=real32), k=1,2*nrows)], [2, nrows])
        f64s = [(real(k, kind=real64), k=1,nrows)]
        f64v = reshape([(real(k, kind=real64), k=1,2*nrows)], [2, nrows])
        logs = [.true., .false., .true., .false., .true.]
        logv = reshape([.true., .false., .false., .true., .true., .true., .false., .false., .true., .false.], [2, nrows])
        strs = [character(len=8) :: "alpha", "bravo", "charlie", "delta", "echo"]
        strv = reshape([character(len=8) :: "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10"], [2, nrows])

        ! Last scalar row, and (col 2, last row) of each vector column, are Null.
        smask = .true.
        smask(nrows) = .false.
        vmask = .true.
        vmask(2, nrows) = .false.

        exp_i32s = i32s; exp_i32s(nrows) = i32_null
        exp_i32v = i32v; exp_i32v(2, nrows) = i32_null
        exp_i64s = i64s; exp_i64s(nrows) = i64_null
        exp_i64v = i64v; exp_i64v(2, nrows) = i64_null
        exp_f32s = f32s; exp_f32s(nrows) = f32_null
        exp_f32v = f32v; exp_f32v(2, nrows) = f32_null
        exp_f64s = f64s; exp_f64s(nrows) = f64_null
        exp_f64v = f64v; exp_f64v(2, nrows) = f64_null
        exp_logs = logs; exp_logs(nrows) = log_null
        exp_logv = logv; exp_logv(2, nrows) = log_null
        exp_strs = strs; exp_strs(nrows) = str_null
        exp_strv = strv; exp_strv(2, nrows) = str_null

        call schema%init(table="read_chunk_all_types_table")
        call schema%add_field("i32s", "int32")
        call schema%add_field("i32v", "int32", col_size=2)
        call schema%add_field("i64s", "int64")
        call schema%add_field("i64v", "int64", col_size=2)
        call schema%add_field("f32s", "float32")
        call schema%add_field("f32v", "float32", col_size=2)
        call schema%add_field("f64s", "float64")
        call schema%add_field("f64v", "float64", col_size=2)
        call schema%add_field("logs", "boolean")
        call schema%add_field("logv", "boolean", col_size=2)
        call schema%add_field("strs", "string", array_size=8)
        call schema%add_field("strv", "string", col_size=2, array_size=8)

        call parquet_open_writer(writer, out_file, schema, chunk_size=2)
        call parquet_write_column(writer, "i32s", i32s, is_valid=smask)
        call parquet_write_column(writer, "i32v", i32v, is_valid=vmask)
        call parquet_write_column(writer, "i64s", i64s, is_valid=smask)
        call parquet_write_column(writer, "i64v", i64v, is_valid=vmask)
        call parquet_write_column(writer, "f32s", f32s, is_valid=smask)
        call parquet_write_column(writer, "f32v", f32v, is_valid=vmask)
        call parquet_write_column(writer, "f64s", f64s, is_valid=smask)
        call parquet_write_column(writer, "f64v", f64v, is_valid=vmask)
        call parquet_write_column(writer, "logs", logs, is_valid=smask)
        call parquet_write_column(writer, "logv", logv, is_valid=vmask)
        call parquet_write_column(writer, "strs", strs, is_valid=smask)
        call parquet_write_column(writer, "strv", strv, is_valid=vmask)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_num_row_groups(reader, num_row_groups)
        call parquet_get_num_row_groups(reader, num_row_groups32)
        ok = (num_row_groups == 3_int64) .and. (num_row_groups32 == 3_int32)

        ! Pass 1: row_group as integer(int64) throughout (parquet_get_chunk_size/
        ! parquet_read_column_chunk's int64 row_group kind-specifics).
        row0 = 0_int64
        do rg = 1_int64, num_row_groups
            block
                integer(int32), allocatable :: b_i32s(:), b_i32v(:, :)
                integer(int64), allocatable :: b_i64s(:), b_i64v(:, :)
                real(real32), allocatable :: b_f32s(:), b_f32v(:, :)
                real(real64), allocatable :: b_f64s(:), b_f64v(:, :)
                logical, allocatable :: b_logs(:), b_logv(:, :)
                character(len=8), allocatable :: b_strs(:), b_strv(:, :)

                call parquet_get_chunk_size(reader, rg_size, row_group=rg)
                allocate(b_i32s(rg_size), b_i64s(rg_size), b_f32s(rg_size), b_f64s(rg_size), b_logs(rg_size), &
                    b_strs(rg_size))
                allocate(b_i32v(2, rg_size), b_i64v(2, rg_size), b_f32v(2, rg_size), b_f64v(2, rg_size), &
                    b_logv(2, rg_size), b_strv(2, rg_size))

                call parquet_read_column_chunk(reader, "i32s", rg, b_i32s, null_value=i32_null)
                call parquet_read_column_chunk(reader, "i32v", rg, b_i32v, null_value=i32_null)
                call parquet_read_column_chunk(reader, "i64s", rg, b_i64s, null_value=i64_null)
                call parquet_read_column_chunk(reader, "i64v", rg, b_i64v, null_value=i64_null)
                call parquet_read_column_chunk(reader, "f32s", rg, b_f32s, null_value=f32_null)
                call parquet_read_column_chunk(reader, "f32v", rg, b_f32v, null_value=f32_null)
                call parquet_read_column_chunk(reader, "f64s", rg, b_f64s, null_value=f64_null)
                call parquet_read_column_chunk(reader, "f64v", rg, b_f64v, null_value=f64_null)
                call parquet_read_column_chunk(reader, "logs", rg, b_logs, null_value=log_null)
                call parquet_read_column_chunk(reader, "logv", rg, b_logv, null_value=log_null)
                call parquet_read_column_chunk(reader, "strs", rg, b_strs, null_value=str_null)
                call parquet_read_column_chunk(reader, "strv", rg, b_strv, null_value=str_null)

                do i = 1_int64, rg_size
                    ok = ok .and. b_i32s(i) == exp_i32s(row0+i) .and. all(b_i32v(:, i) == exp_i32v(:, row0+i))
                    ok = ok .and. b_i64s(i) == exp_i64s(row0+i) .and. all(b_i64v(:, i) == exp_i64v(:, row0+i))
                    ok = ok .and. b_f32s(i) == exp_f32s(row0+i) .and. all(b_f32v(:, i) == exp_f32v(:, row0+i))
                    ok = ok .and. b_f64s(i) == exp_f64s(row0+i) .and. all(b_f64v(:, i) == exp_f64v(:, row0+i))
                    ok = ok .and. (b_logs(i) .eqv. exp_logs(row0+i)) .and. all(b_logv(:, i) .eqv. exp_logv(:, row0+i))
                    ok = ok .and. b_strs(i) == exp_strs(row0+i) .and. all(b_strv(:, i) == exp_strv(:, row0+i))
                end do
                row0 = row0 + rg_size
            end block
        end do
        ok = ok .and. (row0 == int(nrows, kind=int64))

        ! Pass 2: same file/data, but row_group as integer(int32) throughout (parquet_get_chunk_size/
        ! parquet_read_column_chunk's int32 row_group kind-specifics) -- every type/shape here except
        ! the plain int32 scalar column is otherwise only ever exercised with an int64 row_group.
        row0 = 0_int64
        do rg = 1_int64, num_row_groups
            rg32 = int(rg, kind=int32)
            block
                integer(int32), allocatable :: b_i32s(:), b_i32v(:, :)
                integer(int64), allocatable :: b_i64s(:), b_i64v(:, :)
                real(real32), allocatable :: b_f32s(:), b_f32v(:, :)
                real(real64), allocatable :: b_f64s(:), b_f64v(:, :)
                logical, allocatable :: b_logs(:), b_logv(:, :)
                character(len=8), allocatable :: b_strs(:), b_strv(:, :)

                call parquet_get_chunk_size(reader, rg_size32, row_group=rg32)
                allocate(b_i32s(rg_size32), b_i64s(rg_size32), b_f32s(rg_size32), b_f64s(rg_size32), &
                    b_logs(rg_size32), b_strs(rg_size32))
                allocate(b_i32v(2, rg_size32), b_i64v(2, rg_size32), b_f32v(2, rg_size32), b_f64v(2, rg_size32), &
                    b_logv(2, rg_size32), b_strv(2, rg_size32))

                call parquet_read_column_chunk(reader, "i32s", rg32, b_i32s, null_value=i32_null)
                call parquet_read_column_chunk(reader, "i32v", rg32, b_i32v, null_value=i32_null)
                call parquet_read_column_chunk(reader, "i64s", rg32, b_i64s, null_value=i64_null)
                call parquet_read_column_chunk(reader, "i64v", rg32, b_i64v, null_value=i64_null)
                call parquet_read_column_chunk(reader, "f32s", rg32, b_f32s, null_value=f32_null)
                call parquet_read_column_chunk(reader, "f32v", rg32, b_f32v, null_value=f32_null)
                call parquet_read_column_chunk(reader, "f64s", rg32, b_f64s, null_value=f64_null)
                call parquet_read_column_chunk(reader, "f64v", rg32, b_f64v, null_value=f64_null)
                call parquet_read_column_chunk(reader, "logs", rg32, b_logs, null_value=log_null)
                call parquet_read_column_chunk(reader, "logv", rg32, b_logv, null_value=log_null)
                call parquet_read_column_chunk(reader, "strs", rg32, b_strs, null_value=str_null)
                call parquet_read_column_chunk(reader, "strv", rg32, b_strv, null_value=str_null)

                do i = 1_int64, int(rg_size32, kind=int64)
                    ok = ok .and. b_i32s(i) == exp_i32s(row0+i) .and. all(b_i32v(:, i) == exp_i32v(:, row0+i))
                    ok = ok .and. b_i64s(i) == exp_i64s(row0+i) .and. all(b_i64v(:, i) == exp_i64v(:, row0+i))
                    ok = ok .and. b_f32s(i) == exp_f32s(row0+i) .and. all(b_f32v(:, i) == exp_f32v(:, row0+i))
                    ok = ok .and. b_f64s(i) == exp_f64s(row0+i) .and. all(b_f64v(:, i) == exp_f64v(:, row0+i))
                    ok = ok .and. (b_logs(i) .eqv. exp_logs(row0+i)) .and. all(b_logv(:, i) .eqv. exp_logv(:, row0+i))
                    ok = ok .and. b_strs(i) == exp_strs(row0+i) .and. all(b_strv(:, i) == exp_strv(:, row0+i))
                end do
                row0 = row0 + int(rg_size32, kind=int64)
            end block
        end do
        ok = ok .and. (row0 == int(nrows, kind=int64))
        call parquet_close_reader(reader)

        call check(error, ok, "chunked read did not round-trip one or more types/shapes/row groups correctly")
    end subroutine test_read_column_chunk_all_types_roundtrip

    !> Exercises the row_group=integer(int64) specific of parquet_read_column_chunk/
    !> parquet_get_chunk_size, plus parquet_get_num_row_groups's own int64 specific, on a small
    !> 2-row-group file.
    subroutine test_read_column_chunk_int64_row_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_read_chunk_int64_rg.parquet"
        integer(int32) :: values(4), back1(2), back2(2)
        integer(int64) :: num_row_groups, chunk_size1, chunk_size2
        logical :: ok

        values = [10, 20, 30, 40]
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_num_row_groups(reader, num_row_groups)
        ok = (num_row_groups == 2_int64)

        call parquet_get_chunk_size(reader, chunk_size1, row_group=1_int64)
        call parquet_get_chunk_size(reader, chunk_size2, row_group=2_int64)
        ok = ok .and. chunk_size1 == 2_int64 .and. chunk_size2 == 2_int64

        call parquet_read_column_chunk(reader, "v", 1_int64, back1)
        call parquet_read_column_chunk(reader, "v", 2_int64, back2)
        call parquet_close_reader(reader)

        ok = ok .and. all(back1 == [10, 20]) .and. all(back2 == [30, 40])
        call check(error, ok, "int64 row_group-kind chunked read did not round-trip correctly")
    end subroutine test_read_column_chunk_int64_row_group

    !> A chunked read converts between numeric kinds exactly as a whole-column read does.
    !>
    !> Nothing asserted this in either direction, and the guide claimed the opposite -- that
    !> `values`' kind had to match the stored type exactly on this path. It does not: the four
    !> numeric chunk readers in parquet_wrapper.cpp call the same convert_values_to_* helpers the
    !> whole-column path uses, and the Fortran side adds no type check.
    !>
    !> Boolean and string chunk reads are the exception and DO check strictly. That half cannot be
    !> asserted here because it aborts; the negative control is the out-of-process scenario
    !> `chunk_read_bool_type_mismatch` (`test/error_scenarios.f90`), which shows the chunk path
    !> still rejects a genuine type error rather than converting everything. The logical arm below
    !> only confirms a bool column round-trips as itself.
    subroutine test_read_column_chunk_numeric_conversion(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_read_chunk_numeric_conversion.parquet"
        real(real64) :: dvals(4)
        integer(int32) :: ivals(4)
        real(real32) :: narrowed(2)
        integer(int64) :: widened(2)
        real(real64) :: widened_real(2)
        logical :: flags(4), flags_back(2)

        dvals = [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64]
        ivals = [10_int32, 20_int32, 30_int32, 40_int32]
        flags = [.true., .false., .true., .false.]
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "d", dvals)
        call parquet_write_column(writer, "i", ivals)
        call parquet_write_column(writer, "b", flags)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        ! Stored float64, read into real32: narrowing, and the values chosen are exact in both.
        call parquet_read_column_chunk(reader, "d", 2_int64, narrowed)
        call check(error, all(narrowed == [3.5_real32, 4.5_real32]), &
            "a float64 column's chunk did not convert into a real32 array")
        if (allocated(error)) return
        ! Stored int32, read into int64: widening.
        call parquet_read_column_chunk(reader, "i", 1_int64, widened)
        call check(error, all(widened == [10_int64, 20_int64]), &
            "an int32 column's chunk did not convert into an int64 array")
        if (allocated(error)) return
        ! Stored int32, read into float64: across families, as the whole-column path also allows.
        call parquet_read_column_chunk(reader, "i", 2_int64, widened_real)
        call check(error, all(widened_real == [30.0_real64, 40.0_real64]), &
            "an int32 column's chunk did not convert into a real64 array")
        if (allocated(error)) return
        ! The negative control: boolean stays exact-match, and reads back as itself.
        call parquet_read_column_chunk(reader, "b", 1_int64, flags_back)
        call check(error, all(flags_back .eqv. [.true., .false.]), &
            "a logical column's chunk did not round-trip")
        call parquet_close_reader(reader)
    end subroutine test_read_column_chunk_numeric_conversion

    !> parquet_close_reader(check_complete=.true.) must not warn/abort when every row group of
    !> every chunk-read column was actually read.
    subroutine test_read_column_chunk_check_complete_pass(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_read_chunk_complete_pass.parquet"
        integer(int32) :: values(4), back1(2), back2(2)

        values = [1, 2, 3, 4]
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column_chunk(reader, "v", 1, back1)
        call parquet_read_column_chunk(reader, "v", 2, back2)
        call parquet_close_reader(reader, check_complete=.true.)

        call check(error, all(back1 == [1, 2]) .and. all(back2 == [3, 4]), &
            "check_complete=.true. unexpectedly disrupted a complete chunked read")
    end subroutine test_read_column_chunk_check_complete_pass

    !> parquet_close_reader(check_complete=.true., check_hard=.false.)
    !> on a genuinely incomplete chunked read (only row group 1 of 2 read) must print a WARNING
    !> and continue, not abort -- the `hard` counterpart of this is already covered by
    !> scenario_read_chunk_check_complete_hard_aborts in error_scenarios.f90 (default
    !> check_hard=.true., which does abort). No abort here, so this is a plain in-process test:
    !> the reader closing cleanly (and the already-read row group's data staying correct) is
    !> what proves the soft path ran instead of the hard one.
    subroutine test_read_column_chunk_check_complete_soft_warns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_read_chunk_complete_soft_warns.parquet"
        integer(int32) :: values(4), back1(2)

        values = [1, 2, 3, 4]
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column_chunk(reader, "v", 1, back1)
        call parquet_close_reader(reader, check_complete=.true., check_hard=.false.)

        call check(error, all(back1 == [1, 2]), &
            "check_complete=.true., check_hard=.false. on an incomplete chunked read unexpectedly " // &
            "aborted or disrupted the already-read row group")
    end subroutine test_read_column_chunk_check_complete_soft_warns

    !> Regression coverage for the "List index overflow" crash parquet_get_col_size/
    !> parquet_get_column_total_elements/parquet_read_array_row_mode/parquet_read_array_element_
    !> mode used to hit once a vector column's total element count (nrows * col_size) exceeded
    !> 2^31-1 -- all four used to read the *whole* column just to answer a size query, fetch one
    !> row, or fetch one element position across all rows. Exercising this for real would need a
    !> genuine multi-billion-element column, far too slow/large for this suite -- so the actual
    !> proof runs out-of-process as
    !> scenario_col_size_and_row_mode_avoid_whole_column_read in error_scenarios.f90 (same pattern
    !> as test_list_element_count_auto_multi_row_group_roundtrip in test_writing.f90), against a
    !> tiny fixture with a test-only hook that forces a whole-column read to abort; see that
    !> scenario's own comment (and its negative control,
    !> scenario_whole_column_read_forced_error_control) for why this proves the fix rather than
    !> just "nothing happened to call the old path anyway". Note parquet_read_array_element_mode's
    !> fix is streaming row group by row group, not skipping all but one -- it inherently needs
    !> every row group's data (see stream_element_mode_row_groups's own comment in
    !> parquet_wrapper.cpp), unlike parquet_read_array_row_mode which only ever needs one.
    subroutine test_col_size_and_row_mode_avoid_whole_column_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "col_size_and_row_mode_avoid_whole_column_read", expect_abort=.false., &
            failure_message="parquet_get_col_size/parquet_get_column_total_elements/" // &
                "parquet_read_array_row_mode/parquet_read_array_element_mode did not all avoid a whole-column read")
    end subroutine test_col_size_and_row_mode_avoid_whole_column_read
    !
    !> The same guarantee for a PLAIN LIST/LARGE_LIST column, which the scenario above cannot
    !> reach: it writes its fixture with this library's own writer, so every column there is a
    !> FIXED_SIZE_LIST whose width is a schema constant and no data is read at all. A plain
    !> variable-length list is the only shape whose width lives in the data, so it is the only one
    !> where these two queries can read anything -- and therefore the only one where reading too
    !> much is possible. parquet_get_column_total_elements did exactly that until it was moved onto
    !> list_width_verified, the screen-then-prove helper parquet_get_col_size already used;
    !> reverting that change makes this test fail. Negative control:
    !> scenario_whole_column_read_forced_error_control, which proves the forcing hook fires.
    subroutine test_plain_list_size_queries_avoid_whole_column_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "plain_list_size_queries_avoid_whole_column_read", expect_abort=.false., &
            failure_message="parquet_get_col_size/parquet_get_column_total_elements read a whole " // &
                "plain LIST column instead of measuring it one row group at a time")
    end subroutine test_plain_list_size_queries_avoid_whole_column_read
    !
    !> Read-time widening support for Arrow physical types this library's own
    !> writer never produces (see CONTRIBUTING.md's "Additional scalar types"
    !> note and doc/pages/types/supported-data-types.md): INT8/16, UINT8/16/32/64,
    !> HALF_FLOAT, and DECIMAL32/64/128/256, all converted via
    !> convert_values_to_int32/int64/float32/float64 in parquet_wrapper.cpp.
    !> doc/pages/types/supported-data-types.md's "Reading a column into a different numeric kind"
    !> states that integer-to-real conversions are **not** checked for precision loss, unlike the
    !> real-to-integer direction, which aborts on a fractional or out-of-range value. That is a
    !> documented ABSENCE of a check, so nothing about it fails on its own: if someone later added
    !> a precision guard to convert_values_to_float32/float64 (src/parquet_wrapper.cpp), every
    !> existing extended-type test would stay green -- their fixture values are all small -- while
    !> the page silently became wrong. This pins it with values chosen to be exactly on the edge.
    !>
    !> Each half carries its own negative control, and the control is the SAME column read into a
    !> second kind. int64 2**53+1 read as real64 must come back rounded to 2**53, while read as
    !> int64 it must come back exact -- so the fixture demonstrably holds the odd value and the
    !> difference is the conversion rather than the write. Likewise int32 2**24+1 as real32 (rounds
    !> to 2**24) against the same column as int32 (exact). Without the control halves, a test that
    !> merely read a small value into a real array would pass against a build that had started
    !> rejecting the lossy read outright, since it would never reach one.
    subroutine test_int_to_real_precision_is_unchecked(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/precision_loss_unchecked.parquet"
        ! 2**53 + 1 is the smallest positive integer real64 cannot represent; 2**24 + 1 is the
        ! smallest real32 cannot. Both round DOWN to the power of two immediately below them.
        integer(int64), parameter :: big64 = 9007199254740993_int64      ! 2**53 + 1
        integer(int64), parameter :: big64_rounded = 9007199254740992_int64  ! 2**53
        integer(int32), parameter :: big32 = 16777217_int32              ! 2**24 + 1
        integer(int32), parameter :: big32_rounded = 16777216_int32      ! 2**24
        integer(int64) :: src64(2), back64(2)
        integer(int32) :: src32(2), back32(2)
        real(real64) :: as_r64(2)
        real(real32) :: as_r32(2)

        src64 = [big64, 1_int64]
        src32 = [big32, 1_int32]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "wide", src64)
        call parquet_write_column(writer, "narrow", src32)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "wide", back64)      ! control: exact
        call parquet_read_column(reader, "wide", as_r64)      ! lossy, and must not abort
        call parquet_read_column(reader, "narrow", back32)    ! control: exact
        call parquet_read_column(reader, "narrow", as_r32)    ! lossy, and must not abort
        call parquet_close_reader(reader)

        ! Controls first: the file really does hold the un-representable values.
        call check(error, back64(1) == big64, &
            "int64 control read did not return 2**53 + 1 exactly, so the fixture is wrong")
        if (allocated(error)) return
        call check(error, back32(1) == big32, &
            "int32 control read did not return 2**24 + 1 exactly, so the fixture is wrong")
        if (allocated(error)) return
        ! The documented behaviour: silently rounded, no abort, no warning.
        call check(error, int(as_r64(1), int64) == big64_rounded, &
            "int64 -> real64 read did not silently round 2**53 + 1 down to 2**53")
        if (allocated(error)) return
        call check(error, int(as_r32(1), int32) == big32_rounded, &
            "int32 -> real32 read did not silently round 2**24 + 1 down to 2**24")
        if (allocated(error)) return
        ! The representable rows are unaffected on every path.
        call check(error, back64(2) == 1_int64 .and. as_r64(2) == 1.0_real64 .and. &
            back32(2) == 1_int32 .and. as_r32(2) == 1.0_real32, &
            "a representable row did not round-trip on one of the four reads")
    end subroutine test_int_to_real_precision_is_unchecked
    !
    !> Exercises at least one int-target and one real-target read per source
    !> type (except v_decimal_scaled, genuinely fractional -- real-target
    !> only, since a fractional value read into an int array is an error, not
    !> a success case; see the "extended types abort" scenarios in
    !> error_scenarios.f90 for that side of it) against
    !> test/fixtures/extended_types.parquet (see its own generation comment
    !> in tools/generate_fixtures.cpp for the full column layout).
    subroutine test_read_extended_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=*), parameter :: in_file = "test/fixtures/extended_types.parquet"
        logical :: exists
        integer :: nrows
        integer(int32) :: i32(3)
        integer(int64) :: i64(3)
        real(real32) :: r32(3)
        real(real64) :: r64(3)
        !
        inquire(file=in_file, exist=exists)
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test/fixtures/extended_types.parquet")
            return
        end if
        !
        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 3, &
            "nrows == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in extended_types fixture")
            return
        end if
        !
        ! INT8/INT16/UINT8/UINT16 -> int32, all always-exact widenings.
        call parquet_read_column(reader, "v_int8", i32)
        call check(error, all(i32 == [5_int32, -128_int32, 127_int32]), &
            "all(i32 == [5_int32, -128_int32, 127_int32])")
        call fail_if_error(error, reader, "v_int8 -> int32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_int16", i32)
        call check(error, all(i32 == [100_int32, -32768_int32, 32767_int32]), &
            "all(i32 == [100_int32, -32768_int32, 32767_int32])")
        call fail_if_error(error, reader, "v_int16 -> int32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_uint8", i32)
        call check(error, all(i32 == [10_int32, 0_int32, 255_int32]), &
            "all(i32 == [10_int32, 0_int32, 255_int32])")
        call fail_if_error(error, reader, "v_uint8 -> int32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_uint16", i32)
        call check(error, all(i32 == [1000_int32, 0_int32, 65535_int32]), &
            "all(i32 == [1000_int32, 0_int32, 65535_int32])")
        call fail_if_error(error, reader, "v_uint16 -> int32")
        if (allocated(error)) return
        !
        ! UINT32 -> both int32 (fits, since every fixture value <= 2000000000)
        ! and int64.
        call parquet_read_column(reader, "v_uint32", i32)
        call check(error, all(i32 == [1000_int32, 0_int32, 2000000000_int32]), &
            "all(i32 == [1000_int32, 0_int32, 2000000000_int32])")
        call fail_if_error(error, reader, "v_uint32 -> int32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_uint32", i64)
        call check(error, all(i64 == [1000_int64, 0_int64, 2000000000_int64]), &
            "all(i64 == [1000_int64, 0_int64, 2000000000_int64])")
        call fail_if_error(error, reader, "v_uint32 -> int64")
        if (allocated(error)) return
        !
        ! UINT64 -> int64, and widened into real64 and real32.
        call parquet_read_column(reader, "v_uint64", i64)
        call check(error, all(i64 == [1000_int64, 0_int64, 2000000000_int64]), &
            "all(i64 == [1000_int64, 0_int64, 2000000000_int64])")
        call fail_if_error(error, reader, "v_uint64 -> int64")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_uint64", r64)
        call check(error, all(abs(r64 - [1000.0_real64, 0.0_real64, 2000000000.0_real64]) < 1.0e-6_real64), &
            "all(abs(r64 - [1000.0_real64, 0.0_real64, 2000000000.0_real64]) < 1.0e-6_real64)")
        call fail_if_error(error, reader, "v_uint64 -> real64")
        if (allocated(error)) return
        ! convert_values_to_float32's own UINT64 case had never fired
        ! (only the real64-target UINT64 case, above, had).
        call parquet_read_column(reader, "v_uint64", r32)
        call check(error, all(abs(r32 - [1000.0_real32, 0.0_real32, 2000000000.0_real32]) < 1.0_real32), &
            "all(abs(r32 - [1000.0_real32, 0.0_real32, 2000000000.0_real32]) < 1.0_real32)")
        call fail_if_error(error, reader, "v_uint64 -> real32")
        if (allocated(error)) return
        !
        ! HALF_FLOAT -> int32 (every fixture value is exactly integral) and
        ! real32/real64.
        call parquet_read_column(reader, "v_half_float", i32)
        call check(error, all(i32 == [2_int32, -3_int32, 100_int32]), &
            "all(i32 == [2_int32, -3_int32, 100_int32])")
        call fail_if_error(error, reader, "v_half_float -> int32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_half_float", r32)
        call check(error, all(abs(r32 - [2.0_real32, -3.0_real32, 100.0_real32]) < 1.0e-3_real32), &
            "all(abs(r32 - [2.0_real32, -3.0_real32, 100.0_real32]) < 1.0e-3_real32)")
        call fail_if_error(error, reader, "v_half_float -> real32")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_half_float", r64)
        call check(error, all(abs(r64 - [2.0_real64, -3.0_real64, 100.0_real64]) < 1.0e-3_real64), &
            "all(abs(r64 - [2.0_real64, -3.0_real64, 100.0_real64]) < 1.0e-3_real64)")
        call fail_if_error(error, reader, "v_half_float -> real64")
        if (allocated(error)) return
        !
        ! DECIMAL32/64/128/256 (all scale 0 -- an "accidentally decimal"
        ! integer column) -> int64 and real64, one column per width so every
        ! branch of decimal_to_int64_checked/decimal_value_at is exercised.
        call parquet_read_column(reader, "v_decimal32", i64)
        call check(error, all(i64 == [12_int64, -34_int64, 999_int64]), &
            "all(i64 == [12_int64, -34_int64, 999_int64])")
        call fail_if_error(error, reader, "v_decimal32 -> int64")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_decimal32", r64)
        call check(error, all(abs(r64 - [12.0_real64, -34.0_real64, 999.0_real64]) < 1.0e-6_real64), &
            "all(abs(r64 - [12.0_real64, -34.0_real64, 999.0_real64]) < 1.0e-6_real64)")
        call fail_if_error(error, reader, "v_decimal32 -> real64")
        if (allocated(error)) return
        ! convert_values_to_float32's own DECIMAL32/64/128/256 case
        ! (a single shared line for all four widths) had never fired -- only the real64-target
        ! sibling in convert_values_to_float64, above, had.
        call parquet_read_column(reader, "v_decimal32", r32)
        call check(error, all(abs(r32 - [12.0_real32, -34.0_real32, 999.0_real32]) < 1.0e-3_real32), &
            "all(abs(r32 - [12.0_real32, -34.0_real32, 999.0_real32]) < 1.0e-3_real32)")
        call fail_if_error(error, reader, "v_decimal32 -> real32")
        if (allocated(error)) return
        !
        call parquet_read_column(reader, "v_decimal64", i64)
        call check(error, all(i64 == [123456_int64, -7890_int64, 999999999_int64]), &
            "all(i64 == [123456_int64, -7890_int64, 999999999_int64])")
        call fail_if_error(error, reader, "v_decimal64 -> int64")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_decimal64", r64)
        call check(error, all(abs(r64 - [123456.0_real64, -7890.0_real64, 999999999.0_real64]) < 1.0e-3_real64), &
            "all(abs(r64 - [123456.0_real64, -7890.0_real64, 999999999.0_real64]) < 1.0e-3_real64)")
        call fail_if_error(error, reader, "v_decimal64 -> real64")
        if (allocated(error)) return
        !
        call parquet_read_column(reader, "v_decimal128", i64)
        call check(error, all(i64 == [123456789012_int64, -1_int64, 999999999999_int64]), &
            "all(i64 == [123456789012_int64, -1_int64, 999999999999_int64])")
        call fail_if_error(error, reader, "v_decimal128 -> int64")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_decimal128", r64)
        call check(error, all(abs(r64 - [123456789012.0_real64, -1.0_real64, 999999999999.0_real64]) < 1.0_real64), &
            "all(abs(r64 - [123456789012.0_real64, -1.0_real64, 999999999999.0_real64]) < 1.0_real64)")
        call fail_if_error(error, reader, "v_decimal128 -> real64")
        if (allocated(error)) return
        !
        call parquet_read_column(reader, "v_decimal256", i64)
        call check(error, all(i64 == [123456789012345_int64, -1_int64, 999999999999999_int64]), &
            "all(i64 == [123456789012345_int64, -1_int64, 999999999999999_int64])")
        call fail_if_error(error, reader, "v_decimal256 -> int64")
        if (allocated(error)) return
        call parquet_read_column(reader, "v_decimal256", r64)
        call check(error, all(abs(r64 - [123456789012345.0_real64, -1.0_real64, 999999999999999.0_real64]) < 1.0_real64), &
            "all(abs(r64 - [123456789012345.0_real64, -1.0_real64, 999999999999999.0_real64]) < 1.0_real64)")
        call fail_if_error(error, reader, "v_decimal256 -> real64")
        if (allocated(error)) return
        !
        ! DECIMAL128(10, 2), genuinely fractional -- real-target only.
        call parquet_read_column(reader, "v_decimal_scaled", r64)
        call check(error, all(abs(r64 - [1.00_real64, -2.00_real64, 123.45_real64]) < 1.0e-6_real64), &
            "all(abs(r64 - [1.00_real64, -2.00_real64, 123.45_real64]) < 1.0e-6_real64)")
        call fail_if_error(error, reader, "v_decimal_scaled -> real64")
        if (allocated(error)) return
        !
        call parquet_close_reader(reader)
    end subroutine test_read_extended_types
    !
    !> convert_values_to_float64's own FLOAT case had never fired --
    !> no fixture has a plain real32 column read back as real64 (extended_types.parquet has no
    !> FLOAT source column at all, only HALF_FLOAT/DOUBLE/UINT64/DECIMAL*). A fresh write/read
    !> round trip, no fixture needed.
    subroutine test_read_float32_column_as_float64(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real32) :: values(3)
        real(real64) :: back(3)
        character(len=*), parameter :: out_file = "test_run/read_float32_as_float64.parquet"
        !
        values = [1.5_real32, -2.25_real32, 100.75_real32]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        !
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", back)
        call check(error, all(abs(back - [1.5_real64, -2.25_real64, 100.75_real64]) < 1.0e-6_real64), &
            "all(abs(back - [1.5_real64, -2.25_real64, 100.75_real64]) < 1.0e-6_real64)")
        call fail_if_error(error, reader, "v (real32) -> real64")
        if (allocated(error)) return
        call parquet_close_reader(reader)
    end subroutine test_read_float32_column_as_float64
    !
    !> Proves eval_filter_clause's extension to the new read-time source
    !> types (see is_small_integer_family's own comment in
    !> parquet_wrapper.cpp) actually filters rather than silently rejecting
    !> the column or matching nothing. v_int8 in extended_types.parquet is
    !> [5, -128, 127] (rows 1-3); "v_int8 > 0" should keep rows 1 and 3 only.
    subroutine test_filter_extended_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer :: nrows
        integer(int32), allocatable :: id(:)
        !
        call filt%add("v_int8 > 0")
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, &
            "nrows == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "filtering v_int8 > 0 did not keep exactly 2 rows")
            return
        end if
        !
        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)
        call parquet_close_reader(reader)
        call check(error, all(id == [1_int32, 3_int32]), &
            "all(id == [1_int32, 3_int32])")
        if (allocated(error)) then
            call test_failed(error, "filtering v_int8 > 0 did not keep the expected rows (id 1 and 3)")
            return
        end if
    end subroutine test_filter_extended_type
    !
    !> eval_filter_clause's FLOAT/DOUBLE/HALF_FLOAT successful
    !> match-loop (parquet_wrapper.cpp) had only ever been exercised by malformed-value
    !> scenarios, never a clause that actually matches rows. v_half_float in
    !> extended_types.parquet is [2.0, -3.0, 100.0] (rows 1-3); "v_half_float > 0" should keep
    !> rows 1 and 3 only -- same shape/expected rows as test_filter_extended_type, above.
    subroutine test_filter_extended_type_half_float(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer :: nrows
        integer(int32), allocatable :: id(:)
        !
        call filt%add("v_half_float > 0")
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, &
            "nrows == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "filtering v_half_float > 0 did not keep exactly 2 rows")
            return
        end if
        !
        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)
        call parquet_close_reader(reader)
        call check(error, all(id == [1_int32, 3_int32]), &
            "all(id == [1_int32, 3_int32])")
        if (allocated(error)) then
            call test_failed(error, "filtering v_half_float > 0 did not keep the expected rows (id 1 and 3)")
            return
        end if
    end subroutine test_filter_extended_type_half_float
    !
    !> eval_filter_clause's UINT64 successful match-loop had never
    !> fired either. v_uint64 in extended_types.parquet is [1000, 0, 2000000000] (rows 1-3);
    !> "v_uint64 >= 500" should keep rows 1 and 3 only.
    subroutine test_filter_extended_type_uint64(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer :: nrows
        integer(int32), allocatable :: id(:)
        !
        call filt%add("v_uint64 >= 500")
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, &
            "nrows == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "filtering v_uint64 >= 500 did not keep exactly 2 rows")
            return
        end if
        !
        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)
        call parquet_close_reader(reader)
        call check(error, all(id == [1_int32, 3_int32]), &
            "all(id == [1_int32, 3_int32])")
        if (allocated(error)) then
            call test_failed(error, "filtering v_uint64 >= 500 did not keep the expected rows (id 1 and 3)")
            return
        end if
    end subroutine test_filter_extended_type_uint64
    !
    !> eval_filter_clause's DECIMAL32/64/128/256 successful
    !> match-loop had never fired either (all four kinds share this one source line, so a
    !> single decimal32 test covers it regardless of decimal width). v_decimal32 in
    !> extended_types.parquet is [12, -34, 999] (rows 1-3); "v_decimal32 > 0" should keep
    !> rows 1 and 3 only.
    subroutine test_filter_extended_type_decimal32(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer :: nrows
        integer(int32), allocatable :: id(:)
        !
        call filt%add("v_decimal32 > 0")
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, &
            "nrows == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "filtering v_decimal32 > 0 did not keep exactly 2 rows")
            return
        end if
        !
        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)
        call parquet_close_reader(reader)
        call check(error, all(id == [1_int32, 3_int32]), &
            "all(id == [1_int32, 3_int32])")
        if (allocated(error)) then
            call test_failed(error, "filtering v_decimal32 > 0 did not keep the expected rows (id 1 and 3)")
            return
        end if
    end subroutine test_filter_extended_type_decimal32
    !
    !> Reads test/fixtures/nested_struct.parquet (see tools/generate_fixtures.cpp's
    !> generate_nested_struct_fixture for the exact schema/row layout) via dotted struct-field
    !> paths: "main.id" (a direct scalar leaf), "main.inner.name"/"main.inner.age" (2 levels deep),
    !> "main.inner.deep.value" (3 levels deep, proving arbitrary nesting depth), and
    !> "vecdata.spectrum" (a FIXED_SIZE_LIST vector-column leaf resolved through a struct path).
    !> Checks that each leaf's combined validity correctly reflects every independent null source
    !> along its own path (the struct itself null, an intermediate struct null, or the leaf itself
    !> null -- see CLAUDE.md's nested-struct-field design notes): row 2 has "main" itself null
    !> (every leaf under it invalid), row 3 has "main.inner" null (name/age/deep.value all invalid,
    !> "main.id" still valid), row 4 has "main.inner.age" itself null only (name/deep.value stay
    !> valid), row 5 has "main.inner.deep" null only (name/age stay valid) -- proving the
    !> combination is independent per branch, not just per row.
    subroutine test_read_nested_struct_leaves(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=*), parameter :: in_file = "test/fixtures/nested_struct.parquet"
        logical :: exists
        integer :: nrows, col_size
        integer(int32) :: id(5), age(5), deep_value(5)
        character(len=16) :: name(5)
        logical :: id_valid(5), name_valid(5), age_valid(5), deep_valid(5)
        real(real64) :: spectrum(3, 5), spectrum_row(3)

        inquire(file=in_file, exist=exists)
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected test/fixtures/nested_struct.parquet")
            return
        end if

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 5, &
            "nrows == 5")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unexpected number of rows in nested_struct fixture")
            return
        end if

        call parquet_read_column(reader, "main.id", id, null_value=-1_int32, is_valid=id_valid)
        call check(error, all(id_valid .eqv. [.true., .false., .true., .true., .true.]), &
            "all(id_valid .eqv. [.true., .false., .true., .true., .true.])")
        call fail_if_error(error, reader, "main.id is_valid (root-level null only)")
        if (allocated(error)) return
        call check(error, id(1) == 1_int32 .and. id(3) == 3_int32 .and. id(4) == 4_int32 .and. id(5) == 5_int32, &
            "id(1) == 1_int32 .and. id(3) == 3_int32 .and. id(4) == 4_int32 .and. id(5) == 5_int32")
        call fail_if_error(error, reader, "main.id values")
        if (allocated(error)) return

        call parquet_read_column(reader, "main.inner.name", name, null_value="MISSING", is_valid=name_valid)
        call check(error, all(name_valid .eqv. [.true., .false., .false., .true., .true.]), &
            "all(name_valid .eqv. [.true., .false., .false., .true., .true.])")
        call fail_if_error(error, reader, "main.inner.name is_valid (root + mid-level null combination)")
        if (allocated(error)) return
        call check(error, trim(name(1)) == "Alice" .and. trim(name(4)) == "Dave" .and. trim(name(5)) == "Eve", &
            "trim(name(1)) == ""Alice"" .and. trim(name(4)) == ""Dave"" .and. trim(name(5)) == ""Eve""")
        call fail_if_error(error, reader, "main.inner.name values")
        if (allocated(error)) return

        call parquet_read_column(reader, "main.inner.age", age, null_value=-1_int32, is_valid=age_valid)
        call check(error, all(age_valid .eqv. [.true., .false., .false., .false., .true.]), &
            "all(age_valid .eqv. [.true., .false., .false., .false., .true.])")
        call fail_if_error(error, reader, "main.inner.age is_valid (root + mid-level + leaf-level null combination)")
        if (allocated(error)) return
        call check(error, age(1) == 30_int32 .and. age(5) == 50_int32, &
            "age(1) == 30_int32 .and. age(5) == 50_int32")
        call fail_if_error(error, reader, "main.inner.age values")
        if (allocated(error)) return

        call parquet_read_column(reader, "main.inner.deep.value", deep_value, null_value=-1_int32, is_valid=deep_valid)
        call check(error, all(deep_valid .eqv. [.true., .false., .false., .true., .false.]), &
            "all(deep_valid .eqv. [.true., .false., .false., .true., .false.])")
        call fail_if_error(error, reader, "main.inner.deep.value is_valid (3-level-deep null combination)")
        if (allocated(error)) return
        call check(error, deep_value(1) == 100_int32 .and. deep_value(4) == 400_int32, &
            "deep_value(1) == 100_int32 .and. deep_value(4) == 400_int32")
        call fail_if_error(error, reader, "main.inner.deep.value values")
        if (allocated(error)) return

        ! "vecdata.spectrum": a FIXED_SIZE_LIST vector-column leaf resolved through a struct path
        ! -- always valid, values row i = [i.0, i.1, i.2] (see the fixture generator).
        call parquet_get_col_size(reader, "vecdata.spectrum", col_size)
        call check(error, col_size == 3, &
            "col_size == 3")
        call fail_if_error(error, reader, "vecdata.spectrum col_size")
        if (allocated(error)) return

        call parquet_read_column(reader, "vecdata.spectrum", spectrum)
        call check(error, all(abs(spectrum(:, 1) - [1.0_real64, 1.1_real64, 1.2_real64]) < 1.0e-9_real64), &
            "all(abs(spectrum(:, 1) - [1.0_real64, 1.1_real64, 1.2_real64]) < 1.0e-9_real64)")
        call fail_if_error(error, reader, "vecdata.spectrum full-column values (row 1)")
        if (allocated(error)) return

        call parquet_read_array_row_mode(reader, "vecdata.spectrum", spectrum_row, 5)
        call parquet_close_reader(reader)
        call check(error, all(abs(spectrum_row - [5.0_real64, 5.1_real64, 5.2_real64]) < 1.0e-9_real64), &
            "all(abs(spectrum_row - [5.0_real64, 5.1_real64, 5.2_real64]) < 1.0e-9_real64)")
        if (allocated(error)) then
            call test_failed(error, "vecdata.spectrum row-mode read (row 5) did not match expected values")
            return
        end if
    end subroutine test_read_nested_struct_leaves

    !> Regression test for a confirmed use-after-free: reading a struct-nested string leaf
    !! ("main.inner.name") into a compact `parquet_string_column` (the offsets/data/validity
    !! buffer-handoff path behind parquet_read_column) used to read back as all-Null for every
    !! row, because unwrap_struct_path's freshly synthesized combined-validity array was never
    !! retained anywhere past the end of parquet_read_string_column_buffers's own local variable --
    !! the returned validity pointer aimed at memory freed the moment that C++ function returned to
    !! Fortran. Fixed by pinning the array in ParquetReaderHandle::last_whole_column_buffers_array,
    !! mirroring the row-group-scoped chunk read's existing last_chunk_buffers_array. This is
    !! distinct from the fixed-width parquet_read_column read of the same column already covered by
    !! test_read_nested_struct_leaves, above, which goes through a different (unaffected) code path.
    subroutine test_read_nested_struct_string_compact(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=*), parameter :: in_file = "test/fixtures/nested_struct.parquet"
        type(parquet_string_column) :: name

        call parquet_open_reader(reader, in_file)
        call parquet_read_column(reader, "main.inner.name", name)
        call parquet_close_reader(reader)

        call check(error, name%size() == 5_int64, &
            "name%size() == 5_int64")
        if (allocated(error)) then
            call test_failed(error, "main.inner.name (compact) row count mismatch")
            return
        end if

        call check(error, (.not. name%is_null(1_int64)) .and. name%is_null(2_int64) .and. name%is_null(3_int64) .and. (.not. &
            name%is_null(4_int64)) .and. (.not. name%is_null(5_int64)), &
            "(.not. name%is_null(1_int64)) .and. name%is_null(2_int64) .and. name%is_null(3_int64) .and. (.not. name%is_null(4_int")
        if (allocated(error)) then
            call test_failed(error, "main.inner.name (compact) is_null pattern incorrect -- " // &
                "the use-after-free regression would show every row as null")
            return
        end if

        block
            character(len=:), allocatable :: s
            call name%get(1_int64, s)
            call check(error, s == "Alice", &
                "s == ""Alice""")
            if (allocated(error)) then
                call test_failed(error, "main.inner.name (compact) row 1 value mismatch")
                return
            end if
            call name%get(4_int64, s)
            call check(error, s == "Dave", &
                "s == ""Dave""")
            if (allocated(error)) then
                call test_failed(error, "main.inner.name (compact) row 4 value mismatch")
                return
            end if
            call name%get(5_int64, s)
            call check(error, s == "Eve", &
                "s == ""Eve""")
            if (allocated(error)) then
                call test_failed(error, "main.inner.name (compact) row 5 value mismatch")
                return
            end if
        end block
    end subroutine test_read_nested_struct_string_compact

    !> Row filtering against a dotted struct-field path ("main.inner.age"): of the 5 rows, only
    !> row 1 (age=30) and row 5 (age=50) have a valid (non-null) age at all, and only row 5's value
    !> exceeds 35 -- rows with a null age (2, 3, 4, via three different null sources -- see
    !> test_read_nested_struct_leaves) must be excluded from the filtered result, not misread as
    !> matching or crash the filter evaluation.
    subroutine test_filter_nested_struct_leaf(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer :: nrows
        integer(int32), allocatable :: id(:)

        call filt%add("main.inner.age > 35")
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 1, &
            "nrows == 1")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "filtering main.inner.age > 35 did not keep exactly 1 row")
            return
        end if

        allocate(id(nrows))
        call parquet_read_column(reader, "main.id", id)
        call parquet_close_reader(reader)
        call check(error, all(id == [5_int32]), &
            "all(id == [5_int32])")
        if (allocated(error)) then
            call test_failed(error, "filtering main.inner.age > 35 did not keep the expected row (id 5)")
            return
        end if
    end subroutine test_filter_nested_struct_leaf

    !> Shared failure-reporting helper for test_read_extended_types: closes
    !> the reader and fails the test with a message naming which column/target
    !> combination produced the wrong values, without repeating that
    !> boilerplate after every parquet_read_column call above.
    subroutine fail_if_error(error, reader, what)
        type(error_type), allocatable, intent(inout) :: error
        type(parquet_reader), intent(inout) :: reader
        character(len=*), intent(in) :: what
        !
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, trim(what) // " did not read the expected values")
        end if
    end subroutine fail_if_error
    !
    ! ------------------------------------------------------------------------------
    ! date/time/timestamp (parquet_temporal) read integration
    ! ------------------------------------------------------------------------------
    !
    !> Row-mode (one row's element vector, both int32 and int64 row_index) and element-mode
    !> (one element position across all rows) reads on a vector timestamp/date column, each
    !> with an interior Null -- see parquet_read_array_row_mode/parquet_read_array_element_mode's
    !> temporal specifics.
    subroutine test_datetime_row_element_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_timestamp) :: ts(3, 4), row(3), elem(4)
        type(parquet_date) :: d(2, 4), drow(2), delem(4)
        type(parquet_time) :: t(2, 4), trow(2), telem(4)
        type(parquet_date) :: drow_i32(2)
        type(parquet_time) :: trow_i64(2)
        type(parquet_timestamp) :: row_i64(3)
        integer :: i, j
        character(len=*), parameter :: out_file = "test_run/test_datetime_row_element_mode.parquet"

        do j = 1, 4
            do i = 1, 3
                call ts(i, j)%set(2000 + j, i, 1, 0, 0, 0)
            end do
            do i = 1, 2
                call d(i, j)%set(2010 + j, i + 5, 10)
                call t(i, j)%set(mod(i + j, 24), 0, 0)
            end do
        end do
        call ts(2, 3)%set_null()
        call d(1, 2)%set_null()
        call t(2, 3)%set_null()

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "ts", ts)
        call parquet_write_column(writer, "d", d)
        call parquet_write_column(writer, "t", t)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_row_mode(reader, "ts", row, 3_int32)      ! int32 row_index specific
        call parquet_read_array_row_mode(reader, "d", drow, 2_int64)      ! int64 row_index specific
        call parquet_read_array_row_mode(reader, "t", trow, 3_int32)
        ! the complementary row_index kind per type, so both specifics of each are exercised
        call parquet_read_array_row_mode(reader, "d", drow_i32, 2_int32)
        call parquet_read_array_row_mode(reader, "t", trow_i64, 3_int64)
        call parquet_read_array_row_mode(reader, "ts", row_i64, 3_int64)
        call parquet_read_array_element_mode(reader, "ts", elem, 2)
        call parquet_read_array_element_mode(reader, "d", delem, 1)
        call parquet_read_array_element_mode(reader, "t", telem, 2)
        call parquet_close_reader(reader)

        call check(error, row(1) == ts(1, 3) .and. row(3) == ts(3, 3) .and. row(2)%is_null(), &
            "row mode: row 3 of the timestamp vector column")
        if (allocated(error)) return
        call check(error, drow(1)%is_null() .and. drow(2) == d(2, 2), &
            "row mode: row 2 of the date vector column")
        if (allocated(error)) return
        call check(error, trow(2)%is_null() .and. trow(1) == t(1, 3), &
            "row mode: row 3 of the time vector column")
        if (allocated(error)) return
        ! the complementary row_index kind must agree with the first call for each type
        call check(error, drow_i32(1)%is_null() .and. drow_i32(2) == d(2, 2), &
            "row mode (int32 row_index): row 2 of the date vector column")
        if (allocated(error)) return
        call check(error, trow_i64(2)%is_null() .and. trow_i64(1) == t(1, 3), &
            "row mode (int64 row_index): row 3 of the time vector column")
        if (allocated(error)) return
        call check(error, row_i64(1) == ts(1, 3) .and. row_i64(3) == ts(3, 3) .and. row_i64(2)%is_null(), &
            "row mode (int64 row_index): row 3 of the timestamp vector column")
        if (allocated(error)) return
        call check(error, elem(1) == ts(2, 1) .and. elem(4) == ts(2, 4) .and. elem(3)%is_null(), &
            "element mode: element 2 of the timestamp vector column")
        if (allocated(error)) return
        call check(error, delem(2)%is_null() .and. delem(4) == d(1, 4), &
            "element mode: element 1 of the date vector column")
        if (allocated(error)) return
        call check(error, telem(3)%is_null() .and. telem(4) == t(2, 4), &
            "element mode: element 2 of the time vector column")
    end subroutine test_datetime_row_element_mode

    !> A genuinely zero-row vector column (not a filter matching zero rows, which is a
    !> different code path -- reader_handle->filter_mask short-circuits before ever reaching
    !> the row-group-streaming code below) read via parquet_read_array_element_mode must not
    !> crash. Covers read_list_primitive_element's/read_temporal_element's own "no row group
    !> had any rows" fallback (parquet_wrapper.cpp), including read_temporal_element's
    !> unit_out branch for a zero-row timestamp vector column (unit has no values to be
    !> derived from, so it falls back to the schema-declared leaf type). Also exercises
    !> close_parquet_writer's auto-chunk-size "num_rows == 0" fallback on the write side,
    !> since chunk_size= is deliberately left unspecified here.
    subroutine test_read_array_element_mode_zero_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(2, 0)
        type(parquet_timestamp) :: ts(2, 0)
        integer(int32) :: v_elem(0)
        type(parquet_timestamp) :: ts_elem(0)
        integer :: nrows
        character(len=*), parameter :: out_file = "test_run/test_array_element_mode_zero_rows.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_array_element_mode(reader, "v", v_elem, 1)
        call parquet_read_array_element_mode(reader, "ts", ts_elem, 1)
        call parquet_close_reader(reader)

        call check(error, nrows == 0, "zero-row vector column file did not report nrows == 0")
        if (allocated(error)) return
        call check(error, size(v_elem) == 0 .and. size(ts_elem) == 0, &
            "zero-row element-mode read returned a non-empty array")
    end subroutine test_read_array_element_mode_zero_rows
    !
    !> The actual assertions run out-of-process (scenario_temporal_foreign_int96_roundtrip/
    !> scenario_temporal_foreign_tz_roundtrip in error_scenarios.f90, each error-stopping on any
    !> mismatch) since building the fixtures needs a test-only debug hook -- this just checks
    !> both scenarios exit cleanly. See those scenarios' own comments for what each verifies:
    !> a legacy INT96 timestamp column and a real non-UTC IANA timezone, neither ever produced
    !> by this library's own writer.
    subroutine test_datetime_foreign_fixtures(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "temporal_foreign_int96_roundtrip", expect_abort=.false., &
            failure_message="a legacy INT96 timestamp fixture did not round-trip correctly")
        if (allocated(error)) return
        call check_scenario_exit_status(error, "temporal_foreign_tz_roundtrip", expect_abort=.false., &
            failure_message="a non-UTC timezone fixture did not round-trip correctly")
    end subroutine test_datetime_foreign_fixtures
    !
    !> Row-mode/element-mode reads of a temporal vector column under
    !> an active row filter -- read_temporal_row/read_temporal_element's own `filter_mask` branch
    !> (parquet_wrapper.cpp), never previously exercised since no existing temporal test opens a
    !> reader with a filter= argument. row_index/nrows below index into the *filtered* result,
    !> not the physical file row (see parquet_reader_set_filter's own doc-comment).
    subroutine test_datetime_array_filtered(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_timestamp) :: ts(3, 4), row(3), elem(2)
        integer(int32) :: id(4) = [1_int32, 2_int32, 3_int32, 4_int32]
        integer(int64) :: nrows
        integer :: i, j
        character(len=*), parameter :: out_file = "test_run/test_datetime_array_filtered.parquet"

        do j = 1, 4
            do i = 1, 3
                call ts(i, j)%set(2000 + j, i, 1, 0, 0, 0)
            end do
        end do

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)

        call filt%add("id > 2")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2_int64, "filtered reader did not keep the expected 2 rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        call parquet_read_array_row_mode(reader, "ts", row, 1)
        call parquet_read_array_element_mode(reader, "ts", elem, 2)
        call parquet_close_reader(reader)

        call check(error, all(row == ts(:, 3)), &
            "filtered row mode: filtered row 1 should be the original file's row 3")
        if (allocated(error)) return
        call check(error, elem(1) == ts(2, 3) .and. elem(2) == ts(2, 4), &
            "filtered element mode: element 2 across the 2 filtered rows")
    end subroutine test_datetime_array_filtered
    !
    !> parquet_read_*_array_row/_element's `filter_mask` branch, under an
    !> active row filter, for int32/int64/float32/float64 element-mode (read_list_primitive_element
    !> -- row-mode was already covered by test_filter_leaves_vector_column_readable in the "writing"
    !> suite, via its int64 column, but element-mode never was) and for boolean/string row-mode AND
    !> element-mode (both are their own separately-coded, non-templated functions, so neither is
    !> covered by the numeric case above). read_list_primitive_element's four `CType` specifics
    !> dispatch their conversion call via `if constexpr`, which -- unlike the shared runtime
    !> `if (filter_mask)` above it -- compiles a distinct line into each instantiation, so covering
    !> int32 alone leaves int64/float32/float64's own conversion lines uncovered; all four are
    !> exercised below for that reason. row_index/col_index index into the *filtered* result, not
    !> the physical file row (see parquet_reader_set_filter's own doc-comment) -- same convention as
    !> test_datetime_array_filtered above.
    subroutine test_array_row_element_mode_filtered(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(4) = [1_int32, 2_int32, 3_int32, 4_int32]
        integer(int32) :: ivec(3, 4), ivec_row(3), ivec_elem(2)
        integer(int64) :: i64vec(2, 4), i64vec_elem(2)
        real(real32) :: f32vec(2, 4), f32vec_elem(2)
        real(real64) :: f64vec(2, 4), f64vec_elem(2)
        logical :: boolvec(2, 4), boolvec_row(2), boolvec_elem(2)
        character(len=8) :: strvec(2, 4), strvec_row(2), strvec_elem(2)
        integer(int64) :: nrows
        integer :: i, j
        character(len=*), parameter :: out_file = "test_run/test_array_row_element_mode_filtered.parquet"

        do j = 1, 4
            do i = 1, 3
                ivec(i, j) = j * 10 + i
            end do
            do i = 1, 2
                i64vec(i, j) = int(j * 100 + i, int64)
                f32vec(i, j) = real(j * 10 + i, real32) + 0.5_real32
                f64vec(i, j) = real(j * 10 + i, real64) + 0.25_real64
                boolvec(i, j) = mod(i + j, 2) == 0
            end do
        end do
        strvec = reshape([character(len=8) :: &
            "r1c1", "r1c2", "r2c1", "r2c2", "r3c1", "r3c2", "r4c1", "r4c2"], [2, 4])

        call schema%init(table="array_filter_table")
        call schema%add_field("id", "int32")
        call schema%add_field("ivec", "int32", col_size=3)
        call schema%add_field("i64vec", "int64", col_size=2)
        call schema%add_field("f32vec", "float32", col_size=2)
        call schema%add_field("f64vec", "float64", col_size=2)
        call schema%add_field("boolvec", "boolean", col_size=2)
        call schema%add_field("strvec", "string", array_size=8, col_size=2)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "ivec", ivec)
        call parquet_write_column(writer, "i64vec", i64vec)
        call parquet_write_column(writer, "f32vec", f32vec)
        call parquet_write_column(writer, "f64vec", f64vec)
        call parquet_write_column(writer, "boolvec", boolvec)
        call parquet_write_column(writer, "strvec", strvec)
        call parquet_close_writer(writer)

        call filt%add("id > 2")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2_int64, "filtered reader did not keep the expected 2 rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if

        call parquet_read_array_row_mode(reader, "ivec", ivec_row, 1)
        call parquet_read_array_element_mode(reader, "ivec", ivec_elem, 2)
        call parquet_read_array_element_mode(reader, "i64vec", i64vec_elem, 2)
        call parquet_read_array_element_mode(reader, "f32vec", f32vec_elem, 2)
        call parquet_read_array_element_mode(reader, "f64vec", f64vec_elem, 2)
        call parquet_read_array_row_mode(reader, "boolvec", boolvec_row, 1)
        call parquet_read_array_element_mode(reader, "boolvec", boolvec_elem, 2)
        call parquet_read_array_row_mode(reader, "strvec", strvec_row, 1)
        call parquet_read_array_element_mode(reader, "strvec", strvec_elem, 2)
        call parquet_close_reader(reader)

        call check(error, all(ivec_row == ivec(:, 3)), &
            "filtered row mode (int32): filtered row 1 should be the original file's row 3")
        if (allocated(error)) return
        call check(error, ivec_elem(1) == ivec(2, 3) .and. ivec_elem(2) == ivec(2, 4), &
            "filtered element mode (int32): element 2 across the 2 filtered rows")
        if (allocated(error)) return
        call check(error, i64vec_elem(1) == i64vec(2, 3) .and. i64vec_elem(2) == i64vec(2, 4), &
            "filtered element mode (int64): element 2 across the 2 filtered rows")
        if (allocated(error)) return
        call check(error, abs(f32vec_elem(1) - f32vec(2, 3)) < 1.0e-6_real32 .and. &
                          abs(f32vec_elem(2) - f32vec(2, 4)) < 1.0e-6_real32, &
            "filtered element mode (float32): element 2 across the 2 filtered rows")
        if (allocated(error)) return
        call check(error, abs(f64vec_elem(1) - f64vec(2, 3)) < 1.0e-9_real64 .and. &
                          abs(f64vec_elem(2) - f64vec(2, 4)) < 1.0e-9_real64, &
            "filtered element mode (float64): element 2 across the 2 filtered rows")
        if (allocated(error)) return
        call check(error, all(boolvec_row .eqv. boolvec(:, 3)), &
            "filtered row mode (boolean): filtered row 1 should be the original file's row 3")
        if (allocated(error)) return
        call check(error, boolvec_elem(1) .eqv. boolvec(2, 3) .and. boolvec_elem(2) .eqv. boolvec(2, 4), &
            "filtered element mode (boolean): element 2 across the 2 filtered rows")
        if (allocated(error)) return
        call check(error, all(strvec_row == strvec(:, 3)), &
            "filtered row mode (string): filtered row 1 should be the original file's row 3")
        if (allocated(error)) return
        call check(error, trim(strvec_elem(1)) == trim(strvec(2, 3)) .and. trim(strvec_elem(2)) == trim(strvec(2, 4)), &
            "filtered element mode (string): element 2 across the 2 filtered rows")
    end subroutine test_array_row_element_mode_filtered
    !
    !> parquet_open_reader's sample_fraction=: omitted, exactly 1.0, and above 1.0 must all behave
    !> identically to not passing sample_fraction at all -- every row is read, in original order.
    !> Freezes parquet_open_reader(..., sample_fraction=)'s row mapping against the generated
    !> oracle in test_random_vectors, with no reader involved at all -- the reader's agreement with
    !> the same mapping is a separate test.
    !>
    !> Three choices are pinned HERE and nowhere else: the label, the stream index, and the fact
    !> that a physical row indexes the DRAW axis rather than the stream axis. Every uniform in the
    !> table is a perfectly valid pf_random_at output under any of those choices, which is why the
    !> vectors come from an independent Python oracle rather than from a Fortran run -- a table read
    !> back out of the implementation could only confirm that the implementation agrees with itself.
    !> Fraction 0.0 is in the table on purpose: it must keep nothing by arithmetic (u is in [0, 1),
    !> so u < 0.0 is never true) rather than by a special case anyone could delete.
    subroutine test_sample_mapping_golden(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: k, f
        integer(int64) :: key
        real(real64) :: u, frac
        character(len=64) :: at_row
        character(len=:), allocatable :: where_

        call check(error, parquet_sample_label == samp_label, &
            "parquet_sample_label must equal the oracle's frozen samp_label")
        if (allocated(error)) return
        call check(error, parquet_sample_algorithm == "sample:bernoulli-u<p/philox/v1", &
            "parquet_sample_algorithm must name the frozen mapping")
        if (allocated(error)) return

        do k = 1, n_samp
            ! Built once per row rather than once per check: CLAUDE.md's character-temporary rule.
            write (at_row, '(a,i0,a,i0)') "seed ", samp_seed(k), ", row ", samp_row(k)
            where_ = trim(at_row)
            key = pf_random_key(samp_seed(k), parquet_sample_label)
            u = pf_random_at(key, 0_int64, samp_row(k))
            call check(error, transfer(u, 0_int64) == samp_u_bits(k), &
                "sample uniform must match the oracle bit for bit at " // where_)
            if (allocated(error)) return
            do f = 1, n_samp_frac
                frac = transfer(samp_frac_bits(f), 0.0_real64)
                call check(error, (u < frac) .eqv. samp_keep((f - 1) * n_samp + k), &
                    "sample keep/drop must match the oracle at " // where_)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_sample_mapping_golden

    subroutine test_sample_fraction_no_op(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(20), back(20)
        integer(int64) :: nrows
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_fraction_no_op.parquet"

        id = [(i, i=1,20)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        ! Absent.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 20_int64, "sample_fraction absent should read every row")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "id", back)
        call parquet_close_reader(reader)
        call check(error, all(back == id), "sample_fraction absent should preserve every row's original order/content")
        if (allocated(error)) return

        ! Exactly 1.0.
        call parquet_open_reader(reader, out_file, sample_fraction=1.0_real64)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 20_int64, "sample_fraction=1.0 should read every row")
        call parquet_close_reader(reader)
        if (allocated(error)) return

        ! Above 1.0.
        call parquet_open_reader(reader, out_file, sample_fraction=2.5_real64)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 20_int64, "sample_fraction > 1.0 should read every row")
        call parquet_close_reader(reader)
    end subroutine test_sample_fraction_no_op
    !
    !> sample_fraction == 0.0 yields zero rows every time, not just with overwhelming probability
    !> -- and by ARITHMETIC rather than by a special case: parquet_apply_sample's uniforms are in
    !> [0, 1), so `u < 0.0` is never true (see parquet_sample_algorithm, parquet_core.f90). The one
    !> special case that remains at fraction 0.0 is the reported SEED, which stays 0 because there
    !> is nothing to reproduce; scenario_print_stat_sampled_rows covers that half.
    subroutine test_sample_fraction_zero(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(20)
        integer(int64) :: nrows
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_fraction_zero.parquet"

        id = [(i, i=1,20)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.0_real64)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 0_int64, "sample_fraction=0.0 should deterministically yield zero rows")
    end subroutine test_sample_fraction_zero
    !
    !> sample_seed > 0 makes the Bernoulli draw reproducible: opening the same file twice with the
    !> same sample_fraction/sample_seed must select the exact same rows both times.
    subroutine test_sample_fraction_seed_reproducible(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(200), back1(200), back2(200)
        integer(int64) :: nrows1, nrows2
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_fraction_seed_reproducible.parquet"

        id = [(i, i=1,200)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.3_real64, sample_seed=1234_int64)
        call parquet_get_nrows(reader, nrows1)
        call parquet_read_column(reader, "id", back1(1:nrows1))
        call parquet_close_reader(reader)

        call parquet_open_reader(reader, out_file, sample_fraction=0.3_real64, sample_seed=1234_int64)
        call parquet_get_nrows(reader, nrows2)
        call parquet_read_column(reader, "id", back2(1:nrows2))
        call parquet_close_reader(reader)

        call check(error, nrows1 > 0_int64 .and. nrows1 < 200_int64, &
            "sample_fraction=0.3 on 200 rows should select some but not all rows (extremely unlikely to fail by chance)")
        if (allocated(error)) return
        call check(error, nrows1 == nrows2, "the same sample_fraction/sample_seed should select the same row count")
        if (allocated(error)) return
        call check(error, all(back1(1:nrows1) == back2(1:nrows2)), &
            "the same sample_fraction/sample_seed should select the exact same rows")
    end subroutine test_sample_fraction_seed_reproducible
    !
    !> `sample_seed` is `integer(int64)` and the whole width reaches the draw.
    !>
    !> The discriminating pair is the point: `s` and `s + 2**32` differ in NOTHING below bit 32, so
    !> any truncation to 32 bits anywhere on the path -- the argument, the binding, the key
    !> derivation -- makes them select the IDENTICAL rows. A single large seed would not show that;
    !> it would reproduce happily against a truncating implementation. The second assertion then
    !> checks a seed far above huge(int32) is usable at all, i.e. reproduces across two opens.
    subroutine test_sample_seed_is_full_width(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(200), lo_bits(200), hi_bits(200), again(200)
        integer(int64) :: n_lo, n_hi, n_again
        integer :: i
        logical :: same
        integer(int64), parameter :: base = 1234_int64
        integer(int64), parameter :: shifted = 1234_int64 + 4294967296_int64   ! base + 2**32
        character(len=*), parameter :: out_file = "test_run/test_sample_seed_full_width.parquet"

        id = [(i, i=1,200)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64, sample_seed=base)
        call parquet_get_nrows(reader, n_lo)
        call parquet_read_column(reader, "id", lo_bits(1:n_lo))
        call parquet_close_reader(reader)

        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64, sample_seed=shifted)
        call parquet_get_nrows(reader, n_hi)
        call parquet_read_column(reader, "id", hi_bits(1:n_hi))
        call parquet_close_reader(reader)

        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64, sample_seed=shifted)
        call parquet_get_nrows(reader, n_again)
        call parquet_read_column(reader, "id", again(1:n_again))
        call parquet_close_reader(reader)

        ! Both comparisons are NESTED rather than written as `count == count .and. all(...)`.
        ! Fortran does not short-circuit `.and.`, so the one-line form evaluates the `all()` even
        ! when the two counts differ -- and then compares two sections of DIFFERENT extent, which is
        ! non-conforming. It is also the normal case here rather than an edge case: two seeds are
        ! meant to select different row COUNTS, so the guard fails almost every run. gfortran runs
        ! straight past it; nagfor's -C=array (the `nagdeb` profile) aborts with
        ! "Rank 1 of HI_BITS(1:N_HI) has extent 102 instead of 99". See CLAUDE.md, "`.and.` does not
        ! short-circuit".
        same = .false.
        if (n_lo == n_hi) same = all(lo_bits(1:n_lo) == hi_bits(1:n_hi))
        call check(error, .not. same, &
            "seeds differing only above bit 31 must select different rows -- identical rows mean " // &
            "the seed is being truncated to 32 bits somewhere on the path")
        if (allocated(error)) return
        same = .false.
        if (n_hi == n_again) same = all(hi_bits(1:n_hi) == again(1:n_again))
        call check(error, same, &
            "a seed above huge(int32) must still reproduce exactly across two opens")
    end subroutine test_sample_seed_is_full_width
    !
    !> The deferred draw and the immediate one select the SAME rows.
    !>
    !> A `filter=` with clauses makes parquet_apply_sample hand the mask over to be held rather
    !> than installed, and parquet_reader_set_filter folds it in later; without one the mask is
    !> installed straight away. Those are two different code paths through the C++ side, and the
    !> row set must not depend on which was taken. The filter here keeps every row, so the two
    !> results are directly comparable: any difference is the deferral, not the filter.
    !>
    !> This is what the coordinate-addressed draw buys. Under the sequential engine it replaced,
    !> the two paths agreed only because the fold loop was careful to step the engine once per
    !> physical row even for rows it discarded; now they agree by construction.
    subroutine test_sample_deferred_equals_immediate(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: keeps_everything
        integer(int32) :: id(200), immediate(200), deferred(200)
        integer(int64) :: n_immediate, n_deferred
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_deferred_equals_immediate.parquet"

        id = [(i, i=1,200)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.4_real64, sample_seed=99_int64)
        call parquet_get_nrows(reader, n_immediate)
        call parquet_read_column(reader, "id", immediate(1:n_immediate))
        call parquet_close_reader(reader)

        call keeps_everything%add("id >= 1")
        call parquet_open_reader(reader, out_file, filter=keeps_everything, &
            sample_fraction=0.4_real64, sample_seed=99_int64)
        call parquet_get_nrows(reader, n_deferred)
        call parquet_read_column(reader, "id", deferred(1:n_deferred))
        call parquet_close_reader(reader)

        call check(error, n_immediate > 0_int64 .and. n_immediate < 200_int64, &
            "sample_fraction=0.4 on 200 rows should select some but not all of them")
        if (allocated(error)) return
        call check(error, n_immediate == n_deferred, &
            "the deferred and immediate sample paths must select the same number of rows")
        if (allocated(error)) return
        call check(error, all(immediate(1:n_immediate) == deferred(1:n_deferred)), &
            "the deferred and immediate sample paths must select the exact same rows")
    end subroutine test_sample_deferred_equals_immediate
    !
    !> sample_seed absent (or <= 0) draws a fresh seed from entropy each time -- two opens of the
    !> same file/sample_fraction should (overwhelmingly likely, for n=200/fraction=0.5) select
    !> different row sets. Not a flaky test in practice: the chance of two independent draws over
    !> 200 rows landing on the identical subset is astronomically small.
    subroutine test_sample_fraction_entropy_seed_differs(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(200), back1(200), back2(200)
        integer(int64) :: nrows1, nrows2
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_fraction_entropy_seed_differs.parquet"

        id = [(i, i=1,200)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64)
        call parquet_get_nrows(reader, nrows1)
        call parquet_read_column(reader, "id", back1(1:nrows1))
        call parquet_close_reader(reader)

        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64)
        call parquet_get_nrows(reader, nrows2)
        call parquet_read_column(reader, "id", back2(1:nrows2))
        call parquet_close_reader(reader)

        if (nrows1 /= nrows2) then
            call check(error, .true., &
                "two entropy-seeded sample draws over 200 rows should not select the identical row set")
        else
            call check(error, any(back1(1:nrows1) /= back2(1:nrows2)), &
                "two entropy-seeded sample draws over 200 rows should not select the identical row set")
        end if
    end subroutine test_sample_fraction_entropy_seed_differs
    !
    !> Regression test for a real bug found during development: applying the sample mask
    !> immediately (before a filter='s own clause evaluation) made the filter's referenced column
    !> come back already sample-compacted mid-evaluation, crashing Arrow's Filter kernel on a
    !> length mismatch (see parquet_reader_set_sample's `pending_sample_keep` deferral in
    !> parquet_wrapper.cpp, which fixes this). filter= must apply on top of the downsample: every
    !> row in the final result must satisfy both the filter clause and have been selected by the
    !> sample, and the combined result must never exceed what the filter alone would have kept.
    subroutine test_sample_fraction_with_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(200), back(200), filter_only_nrows
        integer(int64) :: nrows
        integer :: i
        character(len=*), parameter :: out_file = "test_run/test_sample_fraction_with_filter.parquet"

        id = [(i, i=1,200)]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call filt%add("id > 100")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        filter_only_nrows = int(nrows, int32)

        call parquet_open_reader(reader, out_file, filter=filt, sample_fraction=0.5_real64, sample_seed=99_int64)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows > 0_int64, "sample_fraction=0.5 combined with a selective filter should still keep some rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "id", back(1:nrows))
        call parquet_close_reader(reader)

        call check(error, all(back(1:nrows) > 100), &
            "every row of a sample_fraction+filter combined read must satisfy the filter clause")
        if (allocated(error)) return
        call check(error, nrows <= int(filter_only_nrows, int64), &
            "sample_fraction+filter combined must never keep more rows than the filter alone would")
    end subroutine test_sample_fraction_with_filter
    !
    !> get_col_size/flatten_for_stats/parquet_reader_get_string_length's
    !> plain LIST/LARGE_LIST branches -- this library's own writer only ever emits FIXED_SIZE_LIST,
    !> so these are only reachable through a foreign-written file, built by
    !> parquet_debug_write_list_fixture (a test-only C++ hook, see its own comment). The actual
    !> assertions run out-of-process (scenario_list_type_foreign_fixture in error_scenarios.f90,
    !> error-stopping on any mismatch, same convention as test_datetime_foreign_fixtures above) --
    !> this just checks the scenario exits cleanly.
    subroutine test_list_type_foreign_fixture(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "list_type_foreign_fixture", expect_abort=.false., &
            failure_message="LIST/LARGE_LIST foreign-fixture columns did not report the expected sizes/lengths")
    end subroutine test_list_type_foreign_fixture
    !
    !> Builds a small schema with all 9 canonical types (int32/int64/float32/float64/boolean/
    !> string/date/time/timestamp), then exercises parquet_column_exists/parquet_get_column_type:
    !> existence with/without a types= filter, every single canonical type, group aliases ("int"/
    !> "float"/"temporal"), a comma-separated multi-token filter, case-insensitivity, a missing
    !> column (with and without a filter), a dotted struct-leaf path
    !> (test/fixtures/nested_struct.parquet), and a foreign-typed column
    !> (test/fixtures/extended_types.parquet's v_uint32, outside the 9 canonical tokens) which
    !> exists but never matches a types= filter. The error-path (unrecognized types= token,
    !> parquet_get_column_type on a missing/unsupported-type column) is covered separately by
    !> test_errors.f90/error_scenarios.f90, since those abort the process.
    subroutine test_column_exists_and_get_column_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_column_exists.parquet"
        integer(int32) :: i32(2)
        integer(int64) :: i64(2)
        real(real32) :: f32(2)
        real(real64) :: f64(2)
        logical :: bools(2)
        character(len=8) :: strs(2)
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2), t_ms(2)
        type(parquet_timestamp) :: ts(2)
        integer(int32) :: i32_vec(2, 2)
        character(len=:), allocatable :: type_name

        i32 = [1_int32, 2_int32]
        i64 = [1_int64, 2_int64]
        f32 = [1.0_real32, 2.0_real32]
        f64 = [1.0_real64, 2.0_real64]
        bools = [.true., .false.]
        strs = ["abc     ", "de      "]
        i32_vec = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2])
        call d(1)%set(2024, 7, 16); call d(2)%set(1970, 1, 1)
        call t(1)%set(6, 30, 15); call t(2)%set(23, 59, 59)
        call t_ms(1)%set(6, 30, 15); call t_ms(2)%set(23, 59, 59)
        call ts(1)%set(2024, 7, 16, 12, 0, 0); call ts(2)%set(1999, 1, 1, 0, 0, 0)

        call schema%init(table="t")
        call schema%add_field("c_i32", "int32")
        call schema%add_field("c_i32_vec", "int32", col_size=2)
        call schema%add_field("c_i64", "int64")
        call schema%add_field("c_f32", "float32")
        call schema%add_field("c_f64", "float64")
        call schema%add_field("c_bool", "boolean")
        call schema%add_field("c_str", "string", array_size=8)
        call schema%add_field("c_date", "date")
        call schema%add_field("c_time", "time")
        call schema%add_field("c_time_ms", "time[ms]")
        call schema%add_field("c_ts", "timestamp")

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "c_i32", i32)
        call parquet_write_column(writer, "c_i32_vec", i32_vec)
        call parquet_write_column(writer, "c_i64", i64)
        call parquet_write_column(writer, "c_f32", f32)
        call parquet_write_column(writer, "c_f64", f64)
        call parquet_write_column(writer, "c_bool", bools)
        call parquet_write_column(writer, "c_str", strs)
        call parquet_write_column(writer, "c_date", d)
        call parquet_write_column(writer, "c_time", t)
        call parquet_write_column(writer, "c_time_ms", t_ms)
        call parquet_write_column(writer, "c_ts", ts)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)

        ! Plain existence, no filter.
        call check(error, parquet_column_exists(reader, "c_i32"), "c_i32 should exist (no filter)")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "does_not_exist"), &
            "does_not_exist should not exist")
        if (allocated(error)) return

        ! Single canonical type match/mismatch, one per type, plus case-insensitivity on one.
        call check(error, parquet_column_exists(reader, "c_i32", types="int32"), "c_i32 matches int32")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "c_i32", types="int64"), &
            "c_i32 should not match int64")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_i64", types="INT64"), &
            "c_i64 matches INT64 (case-insensitive)")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_f32", types="float32"), "c_f32 matches float32")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_f64", types="float64"), "c_f64 matches float64")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_bool", types="boolean"), "c_bool matches boolean")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_str", types="string"), "c_str matches string")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_date", types="date"), "c_date matches date")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_time", types="time"), "c_time matches time")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_time_ms", types="time"), &
            "c_time_ms (an explicit millisecond/TIME32 column) also matches time")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_ts", types="timestamp"), "c_ts matches timestamp")
        if (allocated(error)) return

        ! A vector (FIXED_SIZE_LIST) column reports its element type, not a distinct token.
        call check(error, parquet_column_exists(reader, "c_i32_vec", types="int32"), &
            "a vector int32 column should also match int32 (element type, not col_size)")
        if (allocated(error)) return
        call parquet_get_column_type(reader, "c_i32_vec", type_name)
        call check(error, trim(type_name) == "int32", "c_i32_vec's resolved type should be int32")
        if (allocated(error)) return

        ! A missing column with a types= filter is still just .false. (no abort).
        call check(error, .not. parquet_column_exists(reader, "does_not_exist", types="int"), &
            "missing column with a types= filter should be .false., not an error")
        if (allocated(error)) return

        ! Group aliases.
        call check(error, parquet_column_exists(reader, "c_i32", types="int") .and. &
            parquet_column_exists(reader, "c_i64", types="int"), "int alias matches int32/int64")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "c_f32", types="int"), &
            "int alias should not match c_f32")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_f32", types="float") .and. &
            parquet_column_exists(reader, "c_f64", types="float"), "float alias matches float32/float64")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "c_date", types="temporal") .and. &
            parquet_column_exists(reader, "c_time", types="temporal") .and. &
            parquet_column_exists(reader, "c_ts", types="temporal"), &
            "temporal alias matches date/time/timestamp")
        if (allocated(error)) return

        ! Comma-separated multi-token filter (with whitespace).
        call check(error, parquet_column_exists(reader, "c_bool", types="int, float, boolean"), &
            "multi-token filter matches boolean via its own token")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "c_str", types="int, float, boolean"), &
            "multi-token filter should not match c_str")
        if (allocated(error)) return

        ! parquet_get_column_type resolves each column's canonical token.
        call parquet_get_column_type(reader, "c_i32", type_name)
        call check(error, trim(type_name) == "int32", "c_i32's resolved type should be int32")
        if (allocated(error)) return
        call parquet_get_column_type(reader, "c_ts", type_name)
        call check(error, trim(type_name) == "timestamp", "c_ts's resolved type should be timestamp")
        if (allocated(error)) return

        call parquet_close_reader(reader)

        ! A dotted struct-leaf path (test/fixtures/nested_struct.parquet) works the same way.
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call check(error, parquet_column_exists(reader, "main.id", types="int32"), &
            "struct-leaf path main.id should match int32")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "main.inner.name", types="string"), &
            "struct-leaf path main.inner.name should match string")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "main.nope"), &
            "a nonexistent struct-leaf path should be .false.")
        if (allocated(error)) return

        call parquet_close_reader(reader)

        ! A column whose physical type is not itself one of the 9 tokens (test/fixtures/
        ! extended_types.parquet's v_uint32) exists with no filter, and MATCHES a types= filter
        ! naming the kind it is read into -- types= asks "can I read this as that?", not "is the
        ! stored type literally that?". The full mapping is exercised by
        ! test_column_type_narrowest_lossless_mapping below.
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call check(error, parquet_column_exists(reader, "v_uint32"), &
            "v_uint32 should exist when checked with no types= filter")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "v_uint32", types="int"), &
            "v_uint32 is an integer column, so the int alias should match it")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "v_uint32", types="int64"), &
            "v_uint32's narrowest lossless target is int64, so int64 should match it")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "v_uint32", types="int32"), &
            "v_uint32 does not fit int32, so int32 should not match it")
        if (allocated(error)) return

        call parquet_close_reader(reader)
    end subroutine test_column_exists_and_get_column_type

    !> Every row of parquet_get_column_type's narrowest-lossless mapping, including the two
    !> deliberately LOSSY rows (uint64 and the decimals) and the "unknown" fallthrough.
    !>
    !> The mapping answers "what do I declare?", which is not the same as the column's physical
    !> type: all four numeric targets accept the same 15 physical types, so which kind a read uses
    !> is chosen by the caller's declaration. Pinning every row here is what stops the table in the
    !> doc-comment and the switch in parquet_wrapper.cpp drifting apart -- nothing else compares
    !> them.
    subroutine test_column_type_narrowest_lossless_mapping(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=:), allocatable :: type_name
        integer :: i
        character(len=20), parameter :: cols(*) = [character(len=20) :: &
            "id", "v_int8", "v_int16", "v_uint8", "v_uint16", &
            "v_uint32", "v_uint64", &
            "v_half_float", &
            "v_decimal32", "v_decimal64", "v_decimal128", "v_decimal256", "v_decimal_scaled", &
            "v_double_fractional"]
        character(len=10), parameter :: want(*) = [character(len=10) :: &
            "int32", "int32", "int32", "int32", "int32", &
            "int64", "int64", &
            "float32", &
            "float64", "float64", "float64", "float64", "float64", &
            "float64"]

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        do i = 1, size(cols)
            call parquet_get_column_type(reader, trim(cols(i)), type_name)
            call check(error, trim(type_name) == trim(want(i)), &
                "column " // trim(cols(i)) // " should read into " // trim(want(i)) // &
                ", got " // trim(type_name))
            if (allocated(error)) return
        end do
        call parquet_close_reader(reader)

        ! The fallthrough: a MAP is not readable by this library at all, so the answer is
        ! "unknown" -- and, crucially, the call RETURNS rather than aborting. That is the whole
        ! point of the query: a caller asks it to find out whether a column can be read.
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_get_column_type(reader, "map_col", type_name)
        call check(error, trim(type_name) == "unknown", &
            "a MAP column should report 'unknown' rather than aborting, got " // trim(type_name))
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "map_col"), &
            "a MAP column still exists when checked with no types= filter")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "map_col", types="int, float, string"), &
            "an unreadable column should match no types= token")
        call parquet_close_reader(reader)
    end subroutine test_column_type_narrowest_lossless_mapping

    !> types= means "can this column be read as one of these?", and the alias/member asymmetry
    !> that follows from it.
    !>
    !> `float` matches an integer column, because an integer IS readable into a float array;
    !> `float64` does not match that same column, because its narrowest lossless target is int32.
    !> An alias is therefore NOT the union of its member tokens. The two ask different questions
    !> and both are useful, so this test pins the asymmetry deliberately -- someone reading only
    !> the code would take it for a bug and "fix" it.
    subroutine test_column_exists_types_alias_asymmetry(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")

        ! The asymmetry itself, on one column: id is int32.
        call check(error, parquet_column_exists(reader, "id", types="float"), &
            "the float alias should match an int32 column -- an integer is readable as a float")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "id", types="float64"), &
            "the float64 token should NOT match an int32 column: its target kind is int32")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "id", types="float32"), &
            "the float32 token should NOT match an int32 column either")
        if (allocated(error)) return

        ! `int` covers every integer physical type, narrow and unsigned alike.
        call check(error, parquet_column_exists(reader, "v_int8", types="int") .and. &
            parquet_column_exists(reader, "v_uint8", types="int") .and. &
            parquet_column_exists(reader, "v_uint16", types="int") .and. &
            parquet_column_exists(reader, "v_uint32", types="int") .and. &
            parquet_column_exists(reader, "v_uint64", types="int"), &
            "the int alias should match every integer physical type")
        if (allocated(error)) return

        ! `float` covers every numeric column, decimals included, since all of them convert to
        ! float64 -- while `int` must not be dragged along with them.
        call check(error, parquet_column_exists(reader, "v_decimal128", types="float") .and. &
            parquet_column_exists(reader, "v_half_float", types="float") .and. &
            parquet_column_exists(reader, "v_uint64", types="float"), &
            "the float alias should match decimal, half_float and uint64 columns")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "v_decimal128", types="int"), &
            "a decimal column reads into float64, so the int alias must not match it")
        if (allocated(error)) return

        ! A narrow integer's target really is int32, not its own width.
        call check(error, parquet_column_exists(reader, "v_int8", types="int32") .and. &
            .not. parquet_column_exists(reader, "v_int8", types="int64"), &
            "v_int8's target is int32 exactly, not int64")
        call parquet_close_reader(reader)
    end subroutine test_column_exists_types_alias_asymmetry
    !
    !> parquet_get_column_names lists every column in schema order, expanding a nested STRUCT
    !> into one dotted leaf path per leaf (to any depth) and never emitting the bare struct
    !> name, and lists LIST/MAP columns too even though they cannot be read.
    subroutine test_get_column_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=:), allocatable :: names(:), type_name
        integer :: i
        logical :: found_bare_struct

        ! Every name this returns must be usable as-is by the rest of the API.
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call parquet_get_column_names(reader, names)
        call check(error, size(names) == 5, "nested_struct.parquet should list 5 leaf columns")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "main.id", "names(1) should be main.id")
        if (allocated(error)) return
        call check(error, trim(names(2)) == "main.inner.name", "names(2) should be main.inner.name")
        if (allocated(error)) return
        call check(error, trim(names(4)) == "main.inner.deep.value", &
            "names(4) should be the doubly-nested leaf main.inner.deep.value")
        if (allocated(error)) return
        call check(error, trim(names(5)) == "vecdata.spectrum", &
            "names(5) should be the vector leaf vecdata.spectrum")
        if (allocated(error)) return

        found_bare_struct = .false.
        do i = 1, size(names)
            if (trim(names(i)) == "main" .or. trim(names(i)) == "vecdata") found_bare_struct = .true.
        end do
        call check(error, .not. found_bare_struct, &
            "a bare struct name must not be listed -- it is not readable")
        if (allocated(error)) return

        ! Every listed name must resolve through the ordinary lookup path.
        do i = 1, size(names)
            call check(error, parquet_column_exists(reader, trim(names(i))), &
                "every name from parquet_get_column_names must exist: " // trim(names(i)))
            if (allocated(error)) return
        end do
        call parquet_close_reader(reader)

        ! A flat file: names come back in schema order, and a column whose physical type is
        ! outside the nine canonical tokens is still listed (parquet_get_column_type is what
        ! reports it as unsupported, not omission from the listing).
        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_get_column_names(reader, names)
        call check(error, size(names) > 0, "extended_types.parquet should list at least one column")
        if (allocated(error)) return
        found_bare_struct = .false.
        do i = 1, size(names)
            if (trim(names(i)) == "v_uint32") found_bare_struct = .true.
        end do
        call check(error, found_bare_struct, &
            "a foreign-typed column (v_uint32) must still appear in the listing")
        if (allocated(error)) return
        call parquet_get_column_type(reader, trim(names(1)), type_name)
        call check(error, len(type_name) > 0, "the first listed column should have a resolvable type")
        if (allocated(error)) return
        call parquet_close_reader(reader)
    end subroutine test_get_column_names
    !
    !> parquet_release_column frees a column's decoded buffers without changing any result:
    !> a re-read after a release returns exactly the same values, and releasing an unknown
    !> name or a never-read column is a silent no-op.
    subroutine test_release_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32), allocatable :: before(:), after(:)
        integer :: nrows
        character(len=*), parameter :: in_file = "test/fixtures/nested_struct.parquet"

        call parquet_open_reader(reader, in_file)
        call parquet_get_nrows(reader, nrows)
        allocate(before(nrows), after(nrows))

        ! main.id carries Nulls, so null_value= is required -- and using a Null-containing
        ! column here is deliberate: it proves a release/re-read preserves validity too, not
        ! just values.
        call parquet_read_column(reader, "main.id", before, null_value=-1_int32)
        call parquet_release_column(reader, "main.id")
        ! Re-reading after a release must re-decode transparently and agree exactly.
        call parquet_read_column(reader, "main.id", after, null_value=-1_int32)
        call check(error, all(before == after), &
            "a column re-read after parquet_release_column must be identical")
        if (allocated(error)) return

        ! Three no-op cases: a name that does not exist, a column never read, and a column
        ! released a second time. The last one matters to any caller that releases per column
        ! rather than in one pass, since it cannot always know whether an earlier release
        ! already dropped the same (top-level) array.
        call parquet_release_column(reader, "no_such_column_at_all")
        call parquet_release_column(reader, "main.inner.age")
        call parquet_release_column(reader, "main.id")
        call check(error, .true., &
            "releasing an unknown, never-read or already-released column must not error")
        if (allocated(error)) return

        ! Still readable afterwards.
        call parquet_read_column(reader, "main.id", after, null_value=-1_int32)
        call check(error, all(before == after), &
            "reads must still work after a no-op release")
        if (allocated(error)) return
        call parquet_close_reader(reader)
    end subroutine test_release_column
    !
    !> `parquet_measure_list_width`'s two tiers, checked SEPARATELY -- the footer screen against the
    !! proof -- because each catches cases the other does not and the proven answer alone hides the
    !! screen entirely.
    !!
    !! `proven=.false.` reads no column data: per row group it compares the mean elements per row,
    !! rejecting a non-integral mean or two row groups that disagree. `proven=.true.` additionally
    !! walks the covered row groups. The pair of columns that pins the difference down:
    !!
    !! * `avg_ok` (rows alternating 3, 1) has an integral, perfectly consistent mean of 2, so the
    !!   screen returns 2 -- a WRONG width that only the proof rejects. This is why anything acting
    !!   on a candidate must be able to survive it being wrong.
    !! * `ragged` (non-integral mean) and `late` (uniform except in its final row group) are both
    !!   settled by the screen alone, for free. `late` is the one that needs the cross-row-group
    !!   comparison rather than mere divisibility -- every one of its row groups divides evenly.
    subroutine test_list_width_screen_and_proof(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer :: w
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        !
        call parquet_open_reader(reader, f)
        !
        ! --- the footer screen alone (no column data read) ---
        call parquet_measure_list_width(reader, "uniform", 0, 0, .false., w)
        call check(error, w == 3, "the screen should offer 3 for a genuinely uniform column")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "avg_ok", 0, 0, .false., w)
        call check(error, w == 2, &
            "the screen should offer 2 for rows of length 3,1 -- an integral mean it cannot reject")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "ragged", 0, 0, .false., w)
        call check(error, w == 1, "a non-integral mean should be rejected by the screen alone")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "late", 0, 0, .false., w)
        call check(error, w == 1, &
            "row groups that disagree should be rejected by the screen alone, even though each " // &
            "one divides evenly on its own")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "with_null", 0, 0, .false., w)
        call check(error, w == 1, "a column with a null row should be rejected by the screen")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "with_empty", 0, 0, .false., w)
        call check(error, w == 1, "a column with an empty row should be rejected by the screen")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "null_avg", 0, 0, .false., w)
        call check(error, w == 4, &
            "the screen should offer 4 for rows of length 5,5,5,NULL -- the null row's own slot " // &
            "makes the mean a whole number")
        if (allocated(error)) return
        !
        ! --- and the proof, which must correct the one the screen got wrong ---
        call parquet_measure_list_width(reader, "uniform", 0, 0, .true., w)
        call check(error, w == 3, "the proof should confirm 3 for the uniform column")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "avg_ok", 0, 0, .true., w)
        call check(error, w == 1, "the proof must reject the screen's candidate of 2 for avg_ok")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "null_avg", 0, 0, .true., w)
        call check(error, w == 1, &
            "the proof must reject the screen's candidate of 4 for null_avg, on the strength of " // &
            "the null row alone")
        if (allocated(error)) return
        !
        ! --- row-group scoping: `late` is uniform within either half, but not across the file ---
        call parquet_measure_list_width(reader, "late", 1, 3, .true., w)
        call check(error, w == 3, "row groups 1..3 of late are uniformly width 3")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "late", 4, 4, .true., w)
        call check(error, w == 2, "row group 4 of late is uniformly width 2")
        if (allocated(error)) return
        call parquet_close_reader(reader)
        !
        ! --- with a filter active, list_width_verified takes its whole-column (masked) branch
        !     instead of the row-group scan, since a mask no longer aligns with row-group boundaries.
        !     "scalar >= 0" keeps every row, so this exercises the branch itself, not its filtering. ---
        block
            type(parquet_filter) :: filt
            call filt%add("scalar >= 0")
            call parquet_open_reader(reader, f, filter=filt)
            call parquet_measure_list_width(reader, "uniform", 0, 0, .true., w)
            call check(error, w == 3, &
                "the masked whole-column proof must confirm 3 for a genuinely uniform column")
            if (allocated(error)) return
            call parquet_measure_list_width(reader, "avg_ok", 0, 0, .true., w)
            call check(error, w == 1, &
                "the masked whole-column proof must reject avg_ok's screen candidate of 2, same as " // &
                "the unmasked row-group scan does")
            if (allocated(error)) return
            call parquet_close_reader(reader)
        end block
        !
        ! --- a filter matching ZERO rows drives the masked whole-column array itself to length 0,
        !     which list_uniform_width reports as width -1 ("no information") rather than a
        !     mismatch -- list_width_verified must still settle on 1, not treat -1 as a real width ---
        block
            type(parquet_filter) :: filt2
            call filt2%add("scalar < 0")
            call parquet_open_reader(reader, f, filter=filt2)
            call parquet_measure_list_width(reader, "uniform", 0, 0, .true., w)
            call check(error, w == 1, &
                "a filter matching zero rows must settle a masked list-width proof at 1, not -1")
            if (allocated(error)) return
            call parquet_close_reader(reader)
        end block
        call parquet_open_reader(reader, f)
        !
        ! --- and a non-list column is answered from the schema either way ---
        call check(error, .not. parquet_column_width_needs_data(reader, "scalar"), &
            "a scalar column's width should not need data")
        if (allocated(error)) return
        call check(error, parquet_column_width_needs_data(reader, "uniform"), &
            "a plain LIST column's width should need data")
        if (allocated(error)) return
        call parquet_measure_list_width(reader, "scalar", 0, 0, .true., w)
        call check(error, w == 1, "a scalar column should measure as width 1")
        call parquet_close_reader(reader)
    end subroutine test_list_width_screen_and_proof

    !> Requesting `is_valid=` for a column that has no Nulls must fill the mask with `.true.`.
    !!
    !! The C++ side short-circuits this: when Arrow already knows the null count is zero it fills the
    !! buffer in one `memset` instead of testing every element. The output is meant to be identical
    !! either way, and the failure mode if it is not is a mask that reads as all-INVALID -- which
    !! would make every row of a perfectly good column look Null. Worth its own test because
    !! parquet_table no longer requests a mask for a clean column at all, so nothing else reaches
    !! this path.
    subroutine test_is_valid_on_clean_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer, parameter :: N = 70 !! more than one 64-bit block, so a per-block bug shows up.
        real(real64) :: v(N)
        real(real64), allocatable :: back(:)
        logical, allocatable :: ok(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/reading_is_valid_clean.parquet"
        !
        do i = 1, N
            v(i) = real(i, real64)
        end do
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        allocate(back(N), ok(N))
        call parquet_read_column(r, "v", back, is_valid=ok)
        call parquet_close_reader(r)
        call check(error, all(ok), "every element of a null-free column must report valid")
        if (allocated(error)) return
        call check(error, count(ok) == N, "and the mask must be fully populated, not partly")
        if (allocated(error)) return
        call check(error, all(back == v), "the values must survive alongside the mask")
    end subroutine test_is_valid_on_clean_column

end module test_reading
