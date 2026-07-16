!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Standalone helper program that deliberately triggers `error stop` paths in
!> the parquet module. It is invoked as a subprocess (via `fpm test
!> error_scenarios -- <scenario>`) from test_errors.f90, because a Fortran
!> `error stop` aborts the whole process and cannot be caught in-process by
!> test-drive. The exit code (0 = no error stop reached, nonzero = aborted)
!> is the observable result.
program error_scenarios
    use parquet
    use parquet_maml_base, only: parquet_maml_file, get_parquet_maml
    use parquet_strings, only : parquet_string_column, parquet_string
    use iso_fortran_env, only : int32, int64, real32, real64
    !$ use omp_lib, only : omp_get_max_threads, omp_get_thread_num
    implicit none

    character(len=64) :: scenario
    integer :: nargs

    nargs = command_argument_count()
    if (nargs < 1) then
        ! No scenario requested: this happens when `fpm test` auto-runs every
        ! test target with no arguments. Exit cleanly and silently rather than
        ! failing, since this program is only meant to be driven (with an
        ! explicit scenario) as a subprocess from test_errors.f90.
        stop
    end if
    call get_command_argument(1, scenario)

    select case (trim(scenario))
    case ("ok")
        continue
    case ("print_stat_smoke")
        call scenario_print_stat_smoke()
    case ("large_string_roundtrip")
        call scenario_large_string_roundtrip()
    case ("col_size_overflow")
        call scenario_col_size_overflow()
    case ("col_size_and_row_mode_avoid_whole_column_read")
        call scenario_col_size_and_row_mode_avoid_whole_column_read()
    case ("whole_column_read_forced_error_control")
        call scenario_whole_column_read_forced_error_control()
    case ("list_element_count_auto_multi_row_group")
        call scenario_list_element_count_auto_multi_row_group()
    case ("list_element_count_explicit_chunk_size_overflow")
        call scenario_list_element_count_explicit_chunk_size_overflow()
    case ("row_group_explicit_nrows_overflow")
        call scenario_row_group_explicit_nrows_overflow()
    case ("row_group_dangling_at_close")
        call scenario_row_group_dangling_at_close()
    case ("row_group_whole_column_undercovered")
        call scenario_row_group_whole_column_undercovered()
    case ("row_group_whole_column_overrun")
        call scenario_row_group_whole_column_overrun()
    case ("row_group_new_column_after_first")
        call scenario_row_group_new_column_after_first()
    case ("column_count_overflow")
        call scenario_column_count_overflow()
    case ("write_undeclared_column")
        call scenario_write_undeclared_column()
    case ("write_undeclared_column_int64")
        call scenario_write_undeclared_column_int64()
    case ("write_undeclared_column_float32")
        call scenario_write_undeclared_column_float32()
    case ("write_undeclared_column_float64")
        call scenario_write_undeclared_column_float64()
    case ("write_undeclared_column_logical")
        call scenario_write_undeclared_column_logical()
    case ("write_undeclared_column_string")
        call scenario_write_undeclared_column_string()
    case ("write_undeclared_column_int32_matrix")
        call scenario_write_undeclared_column_int32_matrix()
    case ("write_undeclared_column_int64_matrix")
        call scenario_write_undeclared_column_int64_matrix()
    case ("write_undeclared_column_float32_matrix")
        call scenario_write_undeclared_column_float32_matrix()
    case ("write_undeclared_column_float64_matrix")
        call scenario_write_undeclared_column_float64_matrix()
    case ("write_undeclared_column_logical_matrix")
        call scenario_write_undeclared_column_logical_matrix()
    case ("write_undeclared_column_string_matrix")
        call scenario_write_undeclared_column_string_matrix()
    case ("write_not_divisible_int64")
        call scenario_write_not_divisible_int64()
    case ("write_not_divisible_float32")
        call scenario_write_not_divisible_float32()
    case ("write_not_divisible_float64")
        call scenario_write_not_divisible_float64()
    case ("write_not_divisible_logical")
        call scenario_write_not_divisible_logical()
    case ("write_not_divisible_string")
        call scenario_write_not_divisible_string()
    case ("write_array_mismatch_int32_matrix")
        call scenario_write_array_mismatch_int32_matrix()
    case ("write_array_mismatch_int64_matrix")
        call scenario_write_array_mismatch_int64_matrix()
    case ("write_array_mismatch_float32_matrix")
        call scenario_write_array_mismatch_float32_matrix()
    case ("write_array_mismatch_float64_matrix")
        call scenario_write_array_mismatch_float64_matrix()
    case ("write_array_mismatch_logical_matrix")
        call scenario_write_array_mismatch_logical_matrix()
    case ("write_array_mismatch_string_matrix")
        call scenario_write_array_mismatch_string_matrix()
    case ("write_chunk_undeclared_column_int32")
        call scenario_write_chunk_undeclared_column_int32()
    case ("write_chunk_undeclared_column_int64")
        call scenario_write_chunk_undeclared_column_int64()
    case ("write_chunk_undeclared_column_float32")
        call scenario_write_chunk_undeclared_column_float32()
    case ("write_chunk_undeclared_column_float64")
        call scenario_write_chunk_undeclared_column_float64()
    case ("write_chunk_undeclared_column_logical")
        call scenario_write_chunk_undeclared_column_logical()
    case ("write_chunk_undeclared_column_string")
        call scenario_write_chunk_undeclared_column_string()
    case ("write_chunk_undeclared_column_int32_matrix")
        call scenario_write_chunk_undeclared_column_int32_matrix()
    case ("write_chunk_undeclared_column_int64_matrix")
        call scenario_write_chunk_undeclared_column_int64_matrix()
    case ("write_chunk_undeclared_column_float32_matrix")
        call scenario_write_chunk_undeclared_column_float32_matrix()
    case ("write_chunk_undeclared_column_float64_matrix")
        call scenario_write_chunk_undeclared_column_float64_matrix()
    case ("write_chunk_undeclared_column_logical_matrix")
        call scenario_write_chunk_undeclared_column_logical_matrix()
    case ("write_chunk_undeclared_column_string_matrix")
        call scenario_write_chunk_undeclared_column_string_matrix()
    case ("write_chunk_not_divisible_int32")
        call scenario_write_chunk_not_divisible_int32()
    case ("write_chunk_not_divisible_int64")
        call scenario_write_chunk_not_divisible_int64()
    case ("write_chunk_not_divisible_float32")
        call scenario_write_chunk_not_divisible_float32()
    case ("write_chunk_not_divisible_float64")
        call scenario_write_chunk_not_divisible_float64()
    case ("write_chunk_not_divisible_logical")
        call scenario_write_chunk_not_divisible_logical()
    case ("write_chunk_not_divisible_string")
        call scenario_write_chunk_not_divisible_string()
    case ("write_chunk_array_mismatch_int32_matrix")
        call scenario_write_chunk_array_mismatch_int32_matrix()
    case ("write_chunk_array_mismatch_int64_matrix")
        call scenario_write_chunk_array_mismatch_int64_matrix()
    case ("write_chunk_array_mismatch_float32_matrix")
        call scenario_write_chunk_array_mismatch_float32_matrix()
    case ("write_chunk_array_mismatch_float64_matrix")
        call scenario_write_chunk_array_mismatch_float64_matrix()
    case ("write_chunk_array_mismatch_logical_matrix")
        call scenario_write_chunk_array_mismatch_logical_matrix()
    case ("write_chunk_array_mismatch_string_matrix")
        call scenario_write_chunk_array_mismatch_string_matrix()
    case ("write_chunk_string_matrix_exceeds_array_size")
        call scenario_write_chunk_string_matrix_exceeds_array_size()
    case ("write_chunk_string_exceeds_array_size")
        call scenario_write_chunk_string_exceeds_array_size()
    case ("write_chunk_no_row_group_open")
        call scenario_write_chunk_no_row_group_open()
    case ("write_chunk_row_count_mismatch")
        call scenario_write_chunk_row_count_mismatch()
    case ("write_chunk_type_mismatch")
        call scenario_write_chunk_type_mismatch()
    case ("read_chunk_with_filter")
        call scenario_read_chunk_with_filter()
    case ("read_chunk_qc_hard_aborts")
        call scenario_read_chunk_qc_hard_aborts()
    case ("read_chunk_qc_soft_warns")
        call scenario_read_chunk_qc_soft_warns()
    case ("read_chunk_check_complete_hard_aborts")
        call scenario_read_chunk_check_complete_hard_aborts()
    case ("read_chunk_row_group_out_of_range")
        call scenario_read_chunk_row_group_out_of_range()
    case ("get_chunk_size_row_group_out_of_range")
        call scenario_get_chunk_size_row_group_out_of_range()
    case ("write_type_mismatch")
        call scenario_write_type_mismatch()
    case ("write_column_twice")
        call scenario_write_column_twice()
    case ("write_column_twice_no_schema")
        call scenario_write_column_twice_no_schema()
    case ("validate_bad_data_type")
        call scenario_validate_bad_data_type()
    case ("validate_excluded_date_type")
        call scenario_validate_bad_data_type_named("date")
    case ("validate_excluded_timestamp_type")
        call scenario_validate_bad_data_type_named("timestamp")
    case ("validate_excluded_decimal_type")
        call scenario_validate_bad_data_type_named("decimal")
    case ("validate_duplicate_name")
        call scenario_validate_duplicate_name()
    case ("validate_missing_table")
        call scenario_validate_missing_table()
    case ("validate_no_fields")
        call scenario_validate_no_fields()
    case ("validate_trailing_depends_no_fields")
        call scenario_validate_trailing_depends_no_fields()
    case ("validate_trailing_keywords_no_fields")
        call scenario_validate_trailing_keywords_no_fields()
    case ("validate_unknown_top_level_section")
        call scenario_validate_unknown_top_level_section()
    case ("validate_unknown_field_subkey")
        call scenario_validate_unknown_field_subkey()
    case ("validate_unknown_qc_subkey")
        call scenario_validate_unknown_qc_subkey()
    case ("validate_user_maml_unknown_column")
        call scenario_validate_user_maml_unknown_column()
    case ("validate_col_map_unknown_internal")
        call scenario_validate_col_map_unknown_internal()
    case ("validate_col_map_duplicate_internal")
        call scenario_validate_col_map_duplicate_internal()
    case ("validate_col_map_output_collision")
        call scenario_validate_col_map_output_collision()
    case ("validate_col_map_output_not_declared")
        call scenario_validate_col_map_output_not_declared()
    case ("validate_col_map_internal_also_in_fields")
        call scenario_validate_col_map_internal_also_in_fields()
    case ("validate_col_map_output_matches_other_field")
        call scenario_validate_col_map_output_matches_other_field()
    case ("set_column_available_deactivated")
        call scenario_set_column_available_deactivated()
    case ("set_column_unavailable_deactivated")
        call scenario_set_column_unavailable_deactivated()
    case ("get_column_index_not_found")
        call scenario_get_column_index_not_found()
    case ("get_field_name_index_too_low")
        call scenario_get_field_name_index_too_low()
    case ("get_field_name_index_too_high")
        call scenario_get_field_name_index_too_high()
    case ("write_maml_without_metadata")
        call scenario_write_maml_without_metadata()
    case ("read_column_with_nulls")
        call scenario_read_column_with_nulls()
    case ("read_unsupported_physical_type")
        call scenario_read_unsupported_physical_type()
    case ("prefetch_unknown_column")
        call scenario_prefetch_unknown_column()
    case ("filter_unknown_column")
        call scenario_filter_unknown_column()
    case ("filter_vector_column")
        call scenario_filter_vector_column()
    case ("filter_malformed_rule")
        call scenario_filter_malformed_rule()
    case ("filter_rule_too_long")
        call scenario_filter_rule_too_long()
    case ("filter_bad_numeric_value")
        call scenario_filter_bad_numeric_value()
    case ("filter_unquoted_string_value")
        call scenario_filter_unquoted_string_value()
    case ("filter_bad_boolean_value")
        call scenario_filter_bad_boolean_value()
    case ("filter_bool_ordering_not_supported")
        call scenario_filter_bool_ordering_not_supported()
    case ("qc_range_violation_warns")
        call scenario_qc_range_violation_warns()
    case ("qc_maml_stray_no_colon_line")
        call scenario_qc_maml_stray_no_colon_line()
    case ("qc_null_violation_warns")
        call scenario_qc_null_violation_warns()
    case ("qc_range_violation_hard_aborts")
        call scenario_qc_range_violation_hard_aborts()
    case ("qc_null_violation_hard_aborts")
        call scenario_qc_null_violation_hard_aborts()
    case ("qc_miss_null_no_warning")
        call scenario_qc_miss_null_no_warning()
    case ("qc_existing_null_abort_unchanged")
        call scenario_qc_existing_null_abort_unchanged()
    case ("qc_column_not_in_file")
        call scenario_qc_column_not_in_file()
    case ("qc_disabled_explicit_no_warning")
        call scenario_qc_disabled_explicit_no_warning()
    case ("qc_maml_bad_miss_value")
        call scenario_qc_maml_bad_miss_value()
    case ("qc_maml_duplicate_field")
        call scenario_qc_maml_duplicate_field()
    case ("qc_maml_missing_name")
        call scenario_qc_maml_missing_name()
    case ("qc_maml_unknown_subkey")
        call scenario_qc_maml_unknown_subkey()
    case ("write_row_count_mismatch")
        call scenario_write_row_count_mismatch()
    case ("read_row_count_mismatch")
        call scenario_read_row_count_mismatch()
    case ("read_before_open")
        call scenario_read_before_open()
    case ("write_before_open")
        call scenario_write_before_open()
    case ("get_nrows_before_open")
        call scenario_get_nrows_before_open()
    case ("close_reader_before_open")
        call scenario_close_reader_before_open()
    case ("close_writer_before_open")
        call scenario_close_writer_before_open()
    case ("close_writer_missing_write")
        call scenario_close_writer_missing_write()
    case ("close_writer_missing_write_unnamed_schema")
        call scenario_close_writer_missing_write_unnamed_schema()
    case ("read_unknown_column")
        call scenario_read_unknown_column()
    case ("open_reader_missing_file")
        call scenario_open_reader_missing_file()
    case ("open_reader_nrows_zero_rows")
        call scenario_open_reader_nrows_zero_rows()
    case ("open_writer_bad_path")
        call scenario_open_writer_bad_path()
    case ("write_string_matrix_exceeds_array_size")
        call scenario_write_string_matrix_exceeds_array_size()
    case ("write_string_exceeds_array_size")
        call scenario_write_string_exceeds_array_size()
    case ("validate_protected_cols_unknown_name")
        call scenario_validate_protected_cols_unknown_name()
    case ("write_protected_column_with_null")
        call scenario_write_protected_column_with_null()
    case ("validate_qc_min_not_numeric")
        call scenario_validate_qc_min_not_numeric()
    case ("validate_qc_min_non_integral_for_int32")
        call scenario_validate_qc_min_non_integral_for_int32()
    case ("validate_qc_min_out_of_int32_range")
        call scenario_validate_qc_min_out_of_int32_range()
    case ("validate_qc_min_wrong_operator")
        call scenario_validate_qc_min_wrong_operator()
    case ("validate_qc_max_wrong_operator")
        call scenario_validate_qc_max_wrong_operator()
    case ("qc_maml_min_wrong_operator")
        call scenario_qc_maml_min_wrong_operator()
    case ("qc_maml_max_wrong_operator")
        call scenario_qc_maml_max_wrong_operator()
    case ("add_col_qc_min_reversed_operator")
        call scenario_add_col_qc_min_reversed_operator()
    case ("add_col_qc_max_reversed_operator")
        call scenario_add_col_qc_max_reversed_operator()
    case ("add_col_qc_operator_without_value")
        call scenario_add_col_qc_operator_without_value()
    case ("add_col_qc_max_operator_without_value")
        call scenario_add_col_qc_max_operator_without_value()
    case ("add_col_qc_bad_miss_value")
        call scenario_add_col_qc_bad_miss_value()
    case ("add_col_qc_too_many_fields")
        call scenario_add_col_qc_too_many_fields()
    case ("add_col_qc_empty_column_name")
        call scenario_add_col_qc_empty_column_name()
    case ("add_col_qc_duplicate_column")
        call scenario_add_col_qc_duplicate_column()
    case ("add_col_qc_duplicate_column_single_quoted")
        call scenario_add_col_qc_duplicate_column_single_quoted()
    case ("add_col_qc_duplicate_column_double_quoted")
        call scenario_add_col_qc_duplicate_column_double_quoted()
    case ("get_col_qc_reversed_operator")
        call scenario_get_col_qc_reversed_operator()
    case ("qc_warning_numeric")
        call scenario_qc_warning_numeric()
    case ("qc_warning_fractional_bound")
        call scenario_qc_warning_fractional_bound()
    case ("qc_warning_string")
        call scenario_qc_warning_string()
    case ("qc_silently_ignored_for_boolean")
        call scenario_qc_silently_ignored_for_boolean()
    case ("write_unknown_compression")
        call scenario_write_unknown_compression()
    case ("write_overwrite_false_existing_file")
        call scenario_write_overwrite_false_existing_file()
    case ("write_values_not_divisible_by_col_size")
        call scenario_write_values_not_divisible_by_col_size()
    case ("write_int64_to_int32_overflow")
        call scenario_write_int64_to_int32_overflow()
    case ("write_float_to_int32_non_integral")
        call scenario_write_float_to_int32_non_integral()
    case ("write_float_to_int32_out_of_range")
        call scenario_write_float_to_int32_out_of_range()
    case ("write_float_to_int64_non_integral")
        call scenario_write_float_to_int64_non_integral()
    case ("write_float_to_int64_out_of_range")
        call scenario_write_float_to_int64_out_of_range()
    case ("set_max_threads_below_one")
        call scenario_set_max_threads_below_one()
    case ("concurrent_calls_into_shared_reader")
        call scenario_concurrent_calls_into_shared_reader()
    case ("concurrent_calls_into_shared_writer")
        call scenario_concurrent_calls_into_shared_writer()
    case ("get_metadata_missing_key_no_default")
        call scenario_get_metadata_missing_key_no_default()
    case ("get_metadata_conversion_failure_no_default")
        call scenario_get_metadata_conversion_failure_no_default()
    case ("get_metadata_missing_int64_no_default")
        call scenario_get_metadata_missing_int64_no_default()
    case ("get_metadata_missing_float32_no_default")
        call scenario_get_metadata_missing_float32_no_default()
    case ("get_metadata_missing_float64_no_default")
        call scenario_get_metadata_missing_float64_no_default()
    case ("get_metadata_missing_logical_no_default")
        call scenario_get_metadata_missing_logical_no_default()
    case ("get_metadata_missing_string_no_default")
        call scenario_get_metadata_missing_string_no_default()
    case ("get_metadata_conversion_int64_no_default")
        call scenario_get_metadata_conversion_int64_no_default()
    case ("get_metadata_conversion_float32_no_default")
        call scenario_get_metadata_conversion_float32_no_default()
    case ("get_metadata_conversion_float64_no_default")
        call scenario_get_metadata_conversion_float64_no_default()
    case ("get_metadata_conversion_logical_no_default")
        call scenario_get_metadata_conversion_logical_no_default()
    case ("get_metadata_missing_int32_array_no_default")
        call scenario_get_metadata_missing_int32_array_no_default()
    case ("get_metadata_conversion_int32_array_no_default")
        call scenario_get_metadata_conversion_int32_array_no_default()
    case ("get_metadata_missing_int64_array_no_default")
        call scenario_get_metadata_missing_int64_array_no_default()
    case ("get_metadata_conversion_int64_array_no_default")
        call scenario_get_metadata_conversion_int64_array_no_default()
    case ("get_metadata_missing_float32_array_no_default")
        call scenario_get_metadata_missing_float32_array_no_default()
    case ("get_metadata_conversion_float32_array_no_default")
        call scenario_get_metadata_conversion_float32_array_no_default()
    case ("get_metadata_missing_float64_array_no_default")
        call scenario_get_metadata_missing_float64_array_no_default()
    case ("get_metadata_conversion_float64_array_no_default")
        call scenario_get_metadata_conversion_float64_array_no_default()
    case ("get_metadata_missing_logical_array_no_default")
        call scenario_get_metadata_missing_logical_array_no_default()
    case ("get_metadata_conversion_logical_array_no_default")
        call scenario_get_metadata_conversion_logical_array_no_default()
    case ("get_metadata_missing_string_array_no_default")
        call scenario_get_metadata_missing_string_array_no_default()
    case ("schema_add_field_before_init")
        call scenario_schema_add_field_before_init()
    case ("schema_init_twice")
        call scenario_schema_init_twice()
    case ("schema_init_empty_table")
        call scenario_schema_init_empty_table()
    case ("schema_add_field_empty_name")
        call scenario_schema_add_field_empty_name()
    case ("schema_add_field_duplicate_name")
        call scenario_schema_add_field_duplicate_name()
    case ("schema_add_field_invalid_data_type")
        call scenario_schema_add_field_invalid_data_type()
    case ("schema_add_field_qc_min_reversed_operator")
        call scenario_schema_add_field_qc_min_reversed_operator()
    case ("schema_add_field_qc_max_reversed_operator")
        call scenario_schema_add_field_qc_max_reversed_operator()
    case ("schema_add_field_qc_operator_without_value")
        call scenario_schema_add_field_qc_operator_without_value()
    case ("schema_add_field_qc_max_operator_without_value")
        call scenario_schema_add_field_qc_max_operator_without_value()
    case ("schema_add_field_bad_qc_miss_value")
        call scenario_schema_add_field_bad_qc_miss_value()
    case ("string_column_index_out_of_range")
        call scenario_string_column_index_out_of_range()
    case ("string_column_get_null")
        call scenario_string_column_get_null()
    case ("string_column_to_character_null")
        call scenario_string_column_to_character_null()
    case ("string_handle_unassociated")
        call scenario_string_handle_unassociated()
    case ("string_handle_stale_index")
        call scenario_string_handle_stale_index()
    case default
        ! Deliberately a distinctive, otherwise-unused exit code (not 0, and
        ! not the plain 1 that `error stop "message"` produces) -- callers
        ! checking exit status can tell "the scenario name doesn't exist
        ! (typo?)" apart from "the scenario ran and genuinely aborted",
        ! which a plain `stop 1` here could not be told apart from.
        print '(a)', "unknown scenario: "//trim(scenario)
        stop 97
    end select

contains

    subroutine scenario_write_undeclared_column()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_undeclared.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column

    !> Schema shared by the write_undeclared_column_*/write_not_divisible_*/
    !> write_array_mismatch_* scenarios below: one col_size=3 vector field per
    !> supported type, so each scenario just needs to write badly-named or
    !> badly-shaped data to it (idx==0 / not-divisible / array-mismatch
    !> checks are otherwise identical copy-pasted logic per type, only ever
    !> exercised for int32 elsewhere).
    function multitype_vector_schema() result(schema)
        type(parquet_schema) :: schema

        call schema%init(table="multitype_table")
        call schema%add_field("i32", "int32", col_size=3)
        call schema%add_field("i64", "int64", col_size=3)
        call schema%add_field("f32", "float32", col_size=3)
        call schema%add_field("f64", "float64", col_size=3)
        call schema%add_field("lg", "boolean", col_size=3)
        call schema%add_field("str", "string", col_size=3, array_size=8)
        call parquet_parse_maml(schema)
    end function multitype_vector_schema

    subroutine scenario_write_undeclared_column_int64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(3) = [1_int64, 2_int64, 3_int64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_int64.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_int64

    subroutine scenario_write_undeclared_column_float32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(3) = [1.0_real32, 2.0_real32, 3.0_real32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_float32.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_float32

    subroutine scenario_write_undeclared_column_float64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(3) = [1.0_real64, 2.0_real64, 3.0_real64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_float64.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_float64

    subroutine scenario_write_undeclared_column_logical()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(3) = [.true., .false., .true.]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_logical.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_logical

    subroutine scenario_write_undeclared_column_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(3) = ["aa     ", "bb     ", "cc     "]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_string.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_string

    subroutine scenario_write_undeclared_column_int32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(3,2)

        data = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_int32_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_int32_matrix

    subroutine scenario_write_undeclared_column_int64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(3,2)

        data = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_int64_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_int64_matrix

    subroutine scenario_write_undeclared_column_float32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(3,2)

        data = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_float32_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_float32_matrix

    subroutine scenario_write_undeclared_column_float64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(3,2)

        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_float64_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_float64_matrix

    subroutine scenario_write_undeclared_column_logical_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(3,2)

        data = reshape([.true., .false., .true., .false., .true., .false.], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_logical_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_logical_matrix

    subroutine scenario_write_undeclared_column_string_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(3,2)

        data = reshape(["aa     ", "bb     ", "cc     ", "dd     ", "ee     ", "ff     "], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_string_matrix.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_string_matrix

    subroutine scenario_write_not_divisible_int64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(4) = [1_int64, 2_int64, 3_int64, 4_int64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_not_divisible_int64.parquet", schema)
        call parquet_write_column(writer, "i64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_not_divisible_int64

    subroutine scenario_write_not_divisible_float32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(4) = [1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_not_divisible_float32.parquet", schema)
        call parquet_write_column(writer, "f32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_not_divisible_float32

    subroutine scenario_write_not_divisible_float64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_not_divisible_float64.parquet", schema)
        call parquet_write_column(writer, "f64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_not_divisible_float64

    subroutine scenario_write_not_divisible_logical()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(4) = [.true., .false., .true., .false.]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_not_divisible_logical.parquet", schema)
        call parquet_write_column(writer, "lg", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_not_divisible_logical

    subroutine scenario_write_not_divisible_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(4) = ["aa     ", "bb     ", "cc     ", "dd     "]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_not_divisible_string.parquet", schema)
        call parquet_write_column(writer, "str", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_not_divisible_string

    subroutine scenario_write_array_mismatch_int32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(2,3)

        data = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_int32_matrix.parquet", schema)
        call parquet_write_column(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_int32_matrix

    subroutine scenario_write_array_mismatch_int64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(2,3)

        data = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_int64_matrix.parquet", schema)
        call parquet_write_column(writer, "i64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_int64_matrix

    subroutine scenario_write_array_mismatch_float32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(2,3)

        data = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_float32_matrix.parquet", schema)
        call parquet_write_column(writer, "f32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_float32_matrix

    subroutine scenario_write_array_mismatch_float64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(2,3)

        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_float64_matrix.parquet", schema)
        call parquet_write_column(writer, "f64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_float64_matrix

    subroutine scenario_write_array_mismatch_logical_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(2,3)

        data = reshape([.true., .false., .true., .false., .true., .false.], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_logical_matrix.parquet", schema)
        call parquet_write_column(writer, "lg", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_logical_matrix

    subroutine scenario_write_array_mismatch_string_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(2,3)

        data = reshape(["aa     ", "bb     ", "cc     ", "dd     ", "ee     ", "ff     "], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_array_mismatch_string_matrix.parquet", schema)
        call parquet_write_column(writer, "str", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_array_mismatch_string_matrix

    !> The parquet_write_column_chunk counterparts of the scenario_write_undeclared_column_*/
    !> scenario_write_not_divisible_*/scenario_write_array_mismatch_* scenarios above: the same
    !> three guard checks (column not defined / values size not divisible by col_size / matrix
    !> array size mismatch), but reached through parquet_write_*_column_chunk's own copy of each
    !> check instead of the batch parquet_write_*_column's. All three guards fire before a row
    !> group needs to be open (see parquet_check_row_group_row_count's own doc-comment in
    !> parquet_write.f90), so none of these need parquet_new_row_group first.
    subroutine scenario_write_chunk_undeclared_column_int32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(3) = [1_int32, 2_int32, 3_int32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_int32.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_int32

    subroutine scenario_write_chunk_undeclared_column_int64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(3) = [1_int64, 2_int64, 3_int64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_int64.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_int64

    subroutine scenario_write_chunk_undeclared_column_float32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(3) = [1.0_real32, 2.0_real32, 3.0_real32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_float32.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_float32

    subroutine scenario_write_chunk_undeclared_column_float64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(3) = [1.0_real64, 2.0_real64, 3.0_real64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_float64.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_float64

    subroutine scenario_write_chunk_undeclared_column_logical()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(3) = [.true., .false., .true.]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_logical.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_logical

    subroutine scenario_write_chunk_undeclared_column_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(3) = ["aa     ", "bb     ", "cc     "]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_string.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_string

    subroutine scenario_write_chunk_undeclared_column_int32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(3,2)

        data = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_int32_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_int32_matrix

    subroutine scenario_write_chunk_undeclared_column_int64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(3,2)

        data = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_int64_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_int64_matrix

    subroutine scenario_write_chunk_undeclared_column_float32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(3,2)

        data = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_float32_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_float32_matrix

    subroutine scenario_write_chunk_undeclared_column_float64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(3,2)

        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_float64_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_float64_matrix

    subroutine scenario_write_chunk_undeclared_column_logical_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(3,2)

        data = reshape([.true., .false., .true., .false., .true., .false.], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_logical_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_logical_matrix

    subroutine scenario_write_chunk_undeclared_column_string_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(3,2)

        data = reshape(["aa     ", "bb     ", "cc     ", "dd     ", "ee     ", "ff     "], [3, 2])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_undeclared_string_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_string_matrix

    subroutine scenario_write_chunk_not_divisible_int32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(4) = [1_int32, 2_int32, 3_int32, 4_int32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_int32.parquet", schema)
        call parquet_write_column_chunk(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_int32

    subroutine scenario_write_chunk_not_divisible_int64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(4) = [1_int64, 2_int64, 3_int64, 4_int64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_int64.parquet", schema)
        call parquet_write_column_chunk(writer, "i64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_int64

    subroutine scenario_write_chunk_not_divisible_float32()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(4) = [1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_float32.parquet", schema)
        call parquet_write_column_chunk(writer, "f32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_float32

    subroutine scenario_write_chunk_not_divisible_float64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_float64.parquet", schema)
        call parquet_write_column_chunk(writer, "f64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_float64

    subroutine scenario_write_chunk_not_divisible_logical()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(4) = [.true., .false., .true., .false.]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_logical.parquet", schema)
        call parquet_write_column_chunk(writer, "lg", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_logical

    subroutine scenario_write_chunk_not_divisible_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(4) = ["aa     ", "bb     ", "cc     ", "dd     "]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_not_divisible_string.parquet", schema)
        call parquet_write_column_chunk(writer, "str", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_not_divisible_string

    subroutine scenario_write_chunk_array_mismatch_int32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(2,3)

        data = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_int32_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_int32_matrix

    subroutine scenario_write_chunk_array_mismatch_int64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: data(2,3)

        data = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_int64_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "i64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_int64_matrix

    subroutine scenario_write_chunk_array_mismatch_float32_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: data(2,3)

        data = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32, 5.0_real32, 6.0_real32], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_float32_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "f32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_float32_matrix

    subroutine scenario_write_chunk_array_mismatch_float64_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: data(2,3)

        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_float64_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "f64", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_float64_matrix

    subroutine scenario_write_chunk_array_mismatch_logical_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(2,3)

        data = reshape([.true., .false., .true., .false., .true., .false.], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_logical_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "lg", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_logical_matrix

    subroutine scenario_write_chunk_array_mismatch_string_matrix()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: data(2,3)

        data = reshape(["aa     ", "bb     ", "cc     ", "dd     ", "ee     ", "ff     "], [2, 3])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_array_mismatch_string_matrix.parquet", schema)
        call parquet_write_column_chunk(writer, "str", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_array_mismatch_string_matrix

    !> parquet_write_column_chunk counterpart of scenario_write_string_matrix_exceeds_array_size.
    subroutine scenario_write_chunk_string_matrix_exceeds_array_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=20) :: values(2, 1)

        schema%maml%name = "string_matrix_array_size_chunk.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: string_matrix_chunk_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 5", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        values(1, 1) = "short"
        values(2, 1) = "this_is_way_too_long"

        call parquet_open_writer(writer, "test_run/error_scenario_chunk_string_matrix_array_size.parquet", schema)
        call parquet_write_column_chunk(writer, "s", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an over-length string into a fixed-size string matrix chunk column " // &
            "without error"
    end subroutine scenario_write_chunk_string_matrix_exceeds_array_size

    !> parquet_read_column_chunk is disallowed outright on a reader opened with an active
    !> filter=, since the filter mask is a single flat mask sized to the whole unfiltered file
    !> with no row-group structure of its own -- see check_reader_no_filter's own comment in
    !> parquet_read.f90.
    subroutine scenario_read_chunk_with_filter()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: v(4), back(2)

        v = [1, 2, 3, 4]
        call parquet_open_writer(writer, "test_run/read_chunk_filter.parquet", chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call filt%add("v >= 0")
        call parquet_open_reader(reader, "test_run/read_chunk_filter.parquet", filter=filt)
        call parquet_read_column_chunk(reader, "v", 1, back)
        print '(a)', "unexpectedly read a chunk on a filtered reader without aborting"
    end subroutine scenario_read_chunk_with_filter

    !> Default (qc_soft=.false., hard): a chunk read whose own row group contains an
    !> out-of-range value with qc active aborts the process, naming that row group -- reusing
    !> the exact same run_qc_range_check the whole-column path uses, just scoped to one row
    !> group's own array (see get_row_group_chunk_array's own comment in parquet_wrapper.cpp).
    subroutine scenario_read_chunk_qc_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(2)

        ra = [10, 400, 20, 30] ! 400 (in row group 1) is outside [0, 360]

        call parquet_open_writer(writer, "test_run/read_chunk_qc_hard.parquet", chunk_size=2)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/read_chunk_qc_hard.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/read_chunk_qc_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/read_chunk_qc_hard.maml"), qc=.true.)
        call parquet_read_column_chunk(reader, "ra", 1, ra_back)
        print '(a)', "unexpectedly read an out-of-range chunk without aborting in hard qc mode"
    end subroutine scenario_read_chunk_qc_hard_aborts

    !> Soft (qc_soft=.true.): a violation found in one row group's chunk WARNs (not aborts),
    !> exactly like the whole-column path -- and the WARNING fires at most once per column even
    !> across multiple violating row groups (reusing qc_range_warned, the same per-column
    !> throttling set the whole-column path already uses), avoiding "one warning per chunk"
    !> spam. Both row groups here violate the bound; only one WARNING must be printed.
    subroutine scenario_read_chunk_qc_soft_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(2)

        ra = [400, 500, -5, -10] ! every row violates [0, 360], across both row groups

        call parquet_open_writer(writer, "test_run/read_chunk_qc_soft.parquet", chunk_size=2)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/read_chunk_qc_soft.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/read_chunk_qc_soft.parquet", &
            schema=parquet_load_qc_maml_file("test_run/read_chunk_qc_soft.maml"), qc=.true., qc_soft=.true.)
        call parquet_read_column_chunk(reader, "ra", 1, ra_back)
        call parquet_read_column_chunk(reader, "ra", 2, ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_read_chunk_qc_soft_warns

    !> Default (check_hard=.true.): parquet_close_reader(check_complete=.true.) aborts if a
    !> column was read via parquet_read_column_chunk but not every row group was read for it --
    !> here row group 2 (of 2) is never read.
    subroutine scenario_read_chunk_check_complete_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(4), back(2)

        v = [1, 2, 3, 4]
        call parquet_open_writer(writer, "test_run/read_chunk_incomplete.parquet", chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/read_chunk_incomplete.parquet")
        call parquet_read_column_chunk(reader, "v", 1, back)
        call parquet_close_reader(reader, check_complete=.true.)
        print '(a)', "unexpectedly closed an incomplete chunked read without aborting"
    end subroutine scenario_read_chunk_check_complete_hard_aborts

    !> parquet_read_column_chunk/parquet_get_chunk_size(reader,...) both abort on a row_group
    !> outside [1, num_row_groups] rather than reading garbage or silently clamping.
    subroutine scenario_read_chunk_row_group_out_of_range()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(4), back(2)

        v = [1, 2, 3, 4]
        call parquet_open_writer(writer, "test_run/read_chunk_out_of_range.parquet", chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/read_chunk_out_of_range.parquet")
        call parquet_read_column_chunk(reader, "v", 3, back)
        print '(a)', "unexpectedly read row_group=3 of a 2-row-group file without aborting"
    end subroutine scenario_read_chunk_row_group_out_of_range

    !> parquet_get_chunk_size(reader, ...) has its own row_group bounds check (separate from
    !> parquet_read_column_chunk's -- see check_row_group_valid vs parquet_get_chunk_size_reader_impl
    !> in parquet_read.f90), so it must be exercised on its own: calling it directly with an
    !> out-of-range row_group, without ever calling parquet_read_column_chunk, must still abort.
    subroutine scenario_get_chunk_size_row_group_out_of_range()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(4)
        integer(int64) :: chunk_size

        v = [1, 2, 3, 4]
        call parquet_open_writer(writer, "test_run/get_chunk_size_out_of_range.parquet", chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/get_chunk_size_out_of_range.parquet")
        call parquet_get_chunk_size(reader, chunk_size, row_group=3_int64)
        print '(a)', "unexpectedly queried chunk_size for row_group=3 of a 2-row-group file without aborting"
    end subroutine scenario_get_chunk_size_row_group_out_of_range

    !> parquet_write_column_chunk counterpart of scenario_write_string_exceeds_array_size (the
    !> rank-1/flat form, dispatched for both a plain scalar string column and a flattened
    !> string-vector column).
    subroutine scenario_write_chunk_string_exceeds_array_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=20) :: values(2)

        schema%maml%name = "string_flat_array_size_chunk.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: string_flat_chunk_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 5", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        values(1) = "short"
        values(2) = "this_is_way_too_long"

        call parquet_open_writer(writer, "test_run/error_scenario_chunk_string_flat_array_size.parquet", schema)
        call parquet_write_column_chunk(writer, "s", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an over-length string into a fixed-size string vector chunk column " // &
            "(flat form) without error"
    end subroutine scenario_write_chunk_string_exceeds_array_size


    !> parquet_check_row_group_row_count's own two guard checks (used by every
    !> parquet_write_*_column_chunk specific), reached directly rather than via any of the
    !> scenarios above: calling parquet_write_column_chunk before any parquet_new_row_group, and
    !> calling it with a row count that doesn't match the currently open row group's own nrows.
    !> Both checks fire before either needs a row group open except the second (by construction).
    subroutine scenario_write_chunk_no_row_group_open()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(3) = [1_int32, 2_int32, 3_int32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_no_row_group.parquet", schema)
        call parquet_write_column_chunk(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_no_row_group_open

    subroutine scenario_write_chunk_row_count_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_row_count_mismatch.parquet", schema)
        call parquet_new_row_group(writer, 3)
        ! "i32" has col_size=3, so 6 values = 2 rows -- mismatches the row group's declared 3 rows.
        call parquet_write_column_chunk(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_row_count_mismatch

    !> parquet_assert_column_type_exact's own type-mismatch check, reached via
    !> parquet_write_column_chunk -- unlike parquet_write_column, there is no cross-numeric-kind
    !> conversion on this path, so a logical chunk written to an int32-declared column is a
    !> mismatch (fires before any row group needs to be open, so no parquet_new_row_group here).
    subroutine scenario_write_chunk_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(3) = [.true., .false., .true.]

        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_chunk_type_mismatch.parquet", schema)
        call parquet_write_column_chunk(writer, "i32", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_type_mismatch


    !> An in-memory schema (parquet_schema(...), not loaded from a .maml file)
    !> declares three columns but only two are written before parquet_close_writer
    !> -- checks the missing-write abort also prints the output filename and
    !> the schema's name, "internal:demo" here since %init/parquet_schema(...)
    !> name an in-memory schema "internal:<table>".
    subroutine scenario_close_writer_missing_write()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        schema = parquet_schema(table="demo")
        call schema%add_field("col_a", "int32")
        call schema%add_field("col_b", "int32")
        call schema%add_field("col_c", "int32")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_missing_write.parquet", schema)
        call parquet_write_column(writer, "col_a", data)
        call parquet_write_column(writer, "col_b", data)
        ! col_c is never written.
        call parquet_close_writer(writer)
    end subroutine scenario_close_writer_missing_write

    !> Same as scenario_close_writer_missing_write, but the schema is built
    !> fully by hand (schema%maml%lines set directly, never going through
    !> %init/parquet_schema(...)) -- schema%maml%name is therefore left
    !> unallocated, so writer%maml_name stays unallocated too, and the
    !> missing-write abort must fall back to printing "(unnamed, built
    !> in-memory)" instead of a schema name.
    subroutine scenario_close_writer_missing_write_unnamed_schema()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        schema%maml%lines = [character(len=40) :: &
            "table: unnamed_table", &
            "fields:", &
            "- name: col_a", &
            "  data_type: int32", &
            "- name: col_b", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_missing_write_unnamed.parquet", schema)
        call parquet_write_column(writer, "col_a", data)
        ! col_b is never written.
        call parquet_close_writer(writer)
    end subroutine scenario_close_writer_missing_write_unnamed_schema

    subroutine scenario_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(1) = [.true.]

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        ! "id0" is declared as int32 in the MAML schema; writing a logical is a type mismatch.
        call parquet_open_writer(writer, "test_run/error_scenario_type_mismatch.parquet", schema)
        call parquet_write_column(writer, "id0", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_type_mismatch

    subroutine scenario_write_column_twice()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        ! The "written more than once" check only applies when a schema (cinfo) is
        ! enforced; a schema-less writer silently allows writing the same column twice.
        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_twice.parquet", schema)
        call parquet_write_column(writer, "id0", data)
        call parquet_write_column(writer, "id0", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_column_twice

    subroutine scenario_write_maml_without_metadata()
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        ! write_maml=.true. requires a schema populated by parquet_parse_maml;
        ! a schema-less writer has no source MAML content to save.
        call parquet_open_writer(writer, "test_run/error_scenario_write_maml.parquet", write_maml=.true.)
        call parquet_write_column(writer, "id", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_maml_without_metadata

    subroutine scenario_validate_bad_data_type()
        type(parquet_maml_file) :: maml

        maml%name = "bad_data_type.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: not_a_real_type" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_bad_data_type

    !> Locks in the specific type exclusions documented in the README's
    !> Limitations section (no date/timestamp/decimal support): unlike
    !> scenario_validate_bad_data_type's generic garbage token, this uses the
    !> real excluded type name, so a future accidental addition of one of
    !> these types to valid_maml_data_types would be caught here.
    subroutine scenario_validate_bad_data_type_named(type_name)
        character(len=*), intent(in) :: type_name
        type(parquet_maml_file) :: maml

        maml%name = "excluded_data_type.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: "//trim(type_name) ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_bad_data_type_named

    subroutine scenario_validate_duplicate_name()
        type(parquet_maml_file) :: maml

        maml%name = "duplicate_name.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_duplicate_name

    subroutine scenario_validate_missing_table()
        type(parquet_maml_file) :: maml

        maml%name = "missing_table.maml"
        maml%lines = [character(len=40) :: &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_missing_table

    subroutine scenario_validate_no_fields()
        type(parquet_maml_file) :: maml

        maml%name = "no_fields.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_no_fields

    !> depends:'s per-item state (survey/dataset/table/version) is normally
    !> flushed to metadata either when the next top-level section appears, or
    !> -- if depends: is the very last section in the file, as here -- via a
    !> dedicated end-of-loop flush once parquet_parse_maml_lines runs out of
    !> lines (src/parquet_metadata.f90). Since fields: must always be last in
    !> a real MAML (nothing can follow it -- the parser exits its loop the
    !> moment a non-indented, non-"- " line appears after fields: starts),
    !> depends: can only ever be "last" in a MAML with no fields: at all,
    !> which always fails validation regardless -- so this specifically
    !> checks that trailing, unclosed depends: content doesn't crash or
    !> otherwise misbehave on the way to the (expected) "no fields defined"
    !> abort, rather than checking the flushed content itself (unobservable
    !> once the process aborts).
    subroutine scenario_validate_trailing_depends_no_fields()
        type(parquet_maml_file) :: maml

        maml%name = "trailing_depends.maml"
        maml%lines = [character(len=40) :: &
            "table: trailing_depends_table", &
            "depends:", &
            "- survey: Some Survey", &
            "  dataset: Some Dataset" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_trailing_depends_no_fields

    !> Same as scenario_validate_trailing_depends_no_fields, but for
    !> keywords:'s end-of-loop flush (parquet_flush_keywords).
    subroutine scenario_validate_trailing_keywords_no_fields()
        type(parquet_maml_file) :: maml

        maml%name = "trailing_keywords.maml"
        maml%lines = [character(len=40) :: &
            "table: trailing_keywords_table", &
            "keywords:", &
            "- tag_one", &
            "- tag_two" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_trailing_keywords_no_fields

    subroutine scenario_validate_unknown_top_level_section()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_top_level_section.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "not_a_real_section: something", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_top_level_section

    subroutine scenario_validate_unknown_field_subkey()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_field_subkey.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  not_a_real_subkey: something" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_field_subkey

    subroutine scenario_validate_unknown_qc_subkey()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_qc_subkey.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1", &
            "    max: 100", &
            "    not_a_real_qc_key: something" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_qc_subkey

    subroutine scenario_validate_user_maml_unknown_column()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_user_maml_unknown_column

    subroutine scenario_validate_col_map_unknown_internal()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - not_a_real_internal_column: b", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_unknown_internal

    !> Two col_map entries both mapping internal column "a" to different
    !> output names ("b" and "c"). Neither "b" nor "c" is actually declared
    !> under fields: here (deliberately -- if either were, parquet_parse_maml_lines
    !> would rename that field's %name back to "a", making it collide with
    !> the other, which would trip parquet_validate_maml's own generic
    !> "duplicate field name" check first and mask the col_map-specific
    !> "duplicate internal column" check this scenario exists to exercise).
    subroutine scenario_validate_col_map_duplicate_internal()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: b", &
            "  - a: c", &
            "fields:", &
            "- name: unrelated", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_duplicate_internal

    subroutine scenario_validate_col_map_output_collision()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: shared_name", &
            "  - b: shared_name", &
            "fields:", &
            "- name: shared_name", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_collision

    subroutine scenario_validate_col_map_output_not_declared()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: my_a", &
            "fields:", &
            "- name: not_my_a", &
            "  data_type: int32" ]

        ! col_map renames "a" to "my_a", but no field named "my_a" is
        ! actually declared in fields: -- the rename has nothing to apply to.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_not_declared

    subroutine scenario_validate_col_map_internal_also_in_fields()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: my_a", &
            "fields:", &
            "- name: my_a", &
            "  data_type: int32", &
            "- name: a", &
            "  data_type: int32" ]

        ! col_map remaps "a", but "a" is also directly (un-renamed) declared
        ! as its own field in fields: -- ambiguous.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_internal_also_in_fields

    subroutine scenario_validate_col_map_output_matches_other_field()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: b", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        ! col_map renames "a" to output name "b", but the base schema already
        ! has a *different*, unrelated column genuinely named "b" -- even
        ! though it isn't separately declared here, activating it later
        ! (e.g. via set_available) would collide with the renamed field.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_matches_other_field

    !> parquet_column_info%set_available (bound as schema%set_column_available)
    !> rejects toggling a column that's deactivated -- merged in as an
    !> inactive placeholder because a base MAML declared it but the user
    !> MAML excluded it (see is_deactivated's doc comment in src/parquet.f90).
    !> Base declares "a"/"b"; the user MAML only declares "a", so "b" merges
    !> in deactivated.
    subroutine scenario_set_column_available_deactivated()
        type(parquet_maml_file) :: base_maml
        type(parquet_schema) :: schema

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        schema%maml%name = "user.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, schema%maml)
        call parquet_parse_maml(schema)

        call schema%set_column_available("b")
        print '(a)', "unexpectedly toggled a deactivated column available by name"
    end subroutine scenario_set_column_available_deactivated

    !> Same as scenario_set_column_available_deactivated, but for
    !> set_column_unavailable (schema%cinfo%set_unavailable).
    subroutine scenario_set_column_unavailable_deactivated()
        type(parquet_maml_file) :: base_maml
        type(parquet_schema) :: schema

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        schema%maml%name = "user.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, schema%maml)
        call parquet_parse_maml(schema)

        call schema%set_column_unavailable("b")
        print '(a)', "unexpectedly toggled a deactivated column unavailable by name"
    end subroutine scenario_set_column_unavailable_deactivated

    subroutine scenario_get_column_index_not_found()
        type(parquet_schema) :: schema
        integer :: idx

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        idx = schema%get_column_index("not_a_real_column")
        print '(a,i0)', "unexpectedly found index: ", idx
    end subroutine scenario_get_column_index_not_found

    subroutine scenario_get_field_name_index_too_low()
        type(parquet_schema) :: schema
        character(len=:), allocatable :: name

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call schema%get_field_name(0, name)
        print '(a,a)', "unexpectedly found name: ", name
    end subroutine scenario_get_field_name_index_too_low

    subroutine scenario_get_field_name_index_too_high()
        type(parquet_schema) :: schema
        character(len=:), allocatable :: name

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call schema%get_field_name(schema%get_num_fields() + 1, name)
        print '(a,a)', "unexpectedly found name: ", name
    end subroutine scenario_get_field_name_index_too_high

    !> test/fixtures/has_null.parquet is a fixture this library cannot write
    !> itself (it never calls Arrow's AppendNull anywhere on the write path):
    !> it was produced by a standalone Arrow/Parquet C++ program with a
    !> genuine Null in row 2 of "id_with_null", to exercise the read-side
    !> Null guard against a real Parquet Null (a validity-bitmap Null, not a
    !> sentinel value) rather than just reasoning about it.
    subroutine scenario_read_column_with_nulls()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_read_column(reader, "id_with_null", values)
        print '(a)', "unexpectedly read a column containing Null values without error"
    end subroutine scenario_read_column_with_nulls

    !> test/fixtures/unsupported_type.parquet has a column ("d") of Arrow's
    !> date32 type -- one of the physical types outside this library's six
    !> supported types (see README's Limitations). This exercises that
    !> documented failure mode: parquet_wrapper.cpp's scalar read functions
    !> now catch the resulting type-mismatch exception at their own
    !> extern "C" boundary and abort cleanly (see report_fatal_error), rather
    !> than letting an uncaught C++ exception reach std::terminate().
    subroutine scenario_read_unsupported_physical_type()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/unsupported_type.parquet")
        call parquet_read_column(reader, "d", values)
        print '(a)', "unexpectedly read a column of an unsupported physical type without error"
    end subroutine scenario_read_unsupported_physical_type

    !> Arrow/Parquet requires every column in a table to have the same number
    !> of rows. parquet_write_column now records the row count of the first
    !> column written and error stops with a dedicated message the moment a
    !> later column's row count disagrees -- rather than letting this reach
    !> parquet_close_writer, where it used to surface as Arrow's own uncaught
    !> "table.Validate()" exception inside WriteTable, aborting the process.
    subroutine scenario_write_row_count_mismatch()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/row_count_mismatch.parquet")
        call parquet_write_column(writer, "a", [1, 2, 3, 4, 5])
        call parquet_write_column(writer, "b", [10, 20, 30])
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote columns with mismatched row counts without error"
    end subroutine scenario_write_row_count_mismatch

    !> reader%handle is c_null_ptr until parquet_open_reader is called; every
    !> C++ read entry point used to dereference it unconditionally (see
    !> ConcurrencyGuard in parquet_wrapper.cpp), so calling parquet_read_column
    !> on an unopened reader crashed with an unhelpful, message-less SIGSEGV.
    !> parquet_read_column now checks this itself first and error stops.
    subroutine scenario_read_before_open()
        type(parquet_reader) :: reader
        integer :: values(3)

        call parquet_read_column(reader, "a", values)
        print '(a)', "unexpectedly read from an unopened reader without error"
    end subroutine scenario_read_before_open

    !> Same issue as scenario_read_before_open, but for the write side:
    !> writer%handle is c_null_ptr until parquet_open_writer is called.
    subroutine scenario_write_before_open()
        type(parquet_writer) :: writer

        call parquet_write_column(writer, "a", [1, 2, 3])
        print '(a)', "unexpectedly wrote to an unopened writer without error"
    end subroutine scenario_write_before_open

    !> parquet_close_reader used to silently no-op on a reader that was never
    !> opened (or already closed) -- matching the automatic finalizer's own
    !> safe-no-op behavior, but leaving a real user mistake (closing something
    !> that was never opened) undetected. It now error stops instead; the
    !> finalizer itself (reader_finalize) is untouched and still no-ops, since
    !> that path legitimately runs on every never-opened reader that goes out
    !> of scope and must not crash the program.
    subroutine scenario_close_reader_before_open()
        type(parquet_reader) :: reader

        call parquet_close_reader(reader)
        print '(a)', "unexpectedly closed a never-opened reader without error"
    end subroutine scenario_close_reader_before_open

    !> Same issue as scenario_close_reader_before_open, but for the writer
    !> side.
    subroutine scenario_close_writer_before_open()
        type(parquet_writer) :: writer

        call parquet_close_writer(writer)
        print '(a)', "unexpectedly closed a never-opened writer without error"
    end subroutine scenario_close_writer_before_open

    !> parquet_read_column (and every other reader procedure naming a column:
    !> parquet_get_col_size, parquet_get_column_total_elements,
    !> parquet_get_string_length, parquet_read_array_row_mode,
    !> parquet_read_array_element_mode) now validates the column name against
    !> the file's actual schema before reading any data, and error stops with
    !> a dedicated message -- the same class of fix already applied to
    !> parquet_prefetch_columns. Previously this reached the underlying C++
    !> "Column not found" exception uncaught, aborting with SIGABRT.
    subroutine scenario_read_unknown_column()
        type(parquet_reader) :: reader
        integer :: values(3)

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_read_column(reader, "not_a_real_column", values)
        print '(a)', "unexpectedly read an unknown column without error"
    end subroutine scenario_read_unknown_column

    !> The same "reader has not been opened" guard now covers every other
    !> reader-taking procedure too (parquet_prefetch_columns, parquet_get_nrows/
    !> parquet_get_col_size/parquet_get_column_total_elements/
    !> parquet_get_string_length, parquet_read_array_row_mode/
    !> parquet_read_array_element_mode), not just parquet_read_column.
    !> parquet_get_nrows here is just one representative of that group.
    subroutine scenario_get_nrows_before_open()
        type(parquet_reader) :: reader
        integer(int64) :: nrows

        call parquet_get_nrows(reader, nrows)
        print '(a,i0)', "unexpectedly read nrows from an unopened reader without error: ", nrows
    end subroutine scenario_get_nrows_before_open

    !> parquet_read_column now validates the given `values` array's row count
    !> against the file's actual row count before reading any data, and error
    !> stops with a dedicated message -- rather than letting the underlying
    !> C++ read call's own "nrows mismatch" check run, which reports a clean
    !> diagnostic but via std::abort() (see report_fatal_error in
    !> parquet_wrapper.cpp), not a Fortran error stop.
    subroutine scenario_read_row_count_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer :: a_read(3) ! file has 5 rows

        call parquet_open_writer(writer, "test_run/read_row_count_mismatch.parquet")
        call parquet_write_column(writer, "a", [1, 2, 3, 4, 5])
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/read_row_count_mismatch.parquet")
        call parquet_read_column(reader, "a", a_read)
        print '(a)', "unexpectedly read a column into a wrong-size array without error"
    end subroutine scenario_read_row_count_mismatch

    !> parquet_prefetch_columns validates every requested name against the
    !> file's actual schema before doing any Arrow read, and reports an
    !> ordinary Fortran error stop naming the missing column -- rather than
    !> letting the underlying C++ "Column not found" exception escape
    !> uncaught across the Fortran/C++ boundary (which would abort the
    !> process with a raw libc++abi/SIGABRT message instead).
    subroutine scenario_prefetch_unknown_column()
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_prefetch_columns(reader, ["not_a_real_column"])
        print '(a)', "unexpectedly prefetched an unknown column without error"
    end subroutine scenario_prefetch_unknown_column

    !> print_stat=.true. always prints to stdout -- run out-of-process (like
    !> every other scenario here) specifically so that output lands in the
    !> subprocess's own captured stdout (check_scenario_exit_status redirects
    !> it to /dev/null) instead of interleaving with test-drive's own
    !> progress lines in the visible `fpm test` console output. Checks that
    !> print_stat=.true. (with a mix of a prefetched-only column, a column
    !> actually read, and a column nobody touched at all) doesn't disturb the
    !> close itself or the data already read back, and that the file is left
    !> in a normal, readable state afterwards -- error stops (a genuine
    !> failure, not just "printed something") if either check fails.
    subroutine scenario_print_stat_smoke()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5), c_values(5)
        integer(int32) :: a_back(5)
        character(len=*), parameter :: out_file = "test_run/scenario_print_stat.parquet"
        integer :: i
        integer(int32) :: nrows

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]
        c_values = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_write_column(writer, "c", c_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_prefetch_columns(reader, ["b"])
        call parquet_read_column(reader, "a", a_back)
        ! "c" is deliberately never prefetched or read, to exercise the
        ! "untouched columns are left out of the report" behavior.
        call parquet_close_reader(reader, print_stat=.true.)

        if (.not. all(a_back == a_values)) then
            error stop "print_stat=.true. disturbed a column already read back before the close"
        end if

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        if (nrows /= 5) then
            error stop "file was left in a bad state after parquet_close_reader(print_stat=.true.)"
        end if
    end subroutine scenario_print_stat_smoke

    !> Proves the arrow::large_utf8() write/read path (added for a string/string-vector column
    !> whose byte payload would overflow Arrow's real int32 STRING-offset limit, ~2GiB -- see
    !> would_overflow_string_offset_limit in parquet_wrapper.cpp) actually round-trips
    !> correctly, not just "doesn't crash". A genuine >2GiB column takes tens of seconds to
    !> build (measured directly while diagnosing the original SIGBUS this feature fixes), far
    !> too slow for the normal fpm test suite -- so this scenario instead calls
    !> parquet_debug_set_string_offset_limit (a process-global, test-only hook declared locally
    !> below, not part of the public Fortran API -- see its own comment in parquet_wrapper.cpp)
    !> to shrink the threshold to a few dozen bytes, forcing a tiny fixture through the same
    !> large_utf8 code path. Safe as a process-global specifically because this scenario always
    !> runs as its own isolated subprocess: it can never race with a concurrently-running
    !> test-drive test's own string columns the way a shared-process global would.
    subroutine scenario_large_string_roundtrip()
        interface
            subroutine parquet_debug_set_string_offset_limit(n) &
                bind(C, name="parquet_debug_set_string_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! byte threshold to use instead of the real 2^31-1 limit; <=0 restores it.
            end subroutine parquet_debug_set_string_offset_limit
        end interface

        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_large_string.parquet"
        character(len=10) :: s_values(6) = [character(len=10) :: &
            "alpha", "bravo", "charlie", "delta", "echo", "foxtrot"]
        character(len=10) :: s_back(6)
        character(len=6) :: v_values(2, 6), v_back(2, 6)
        integer :: strlen_max
        integer(int64) :: nrows

        v_values = reshape([character(len=6) :: &
            "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12"], [2, 6])

        ! Both columns must share the same row count (6, matching "s") -- Arrow/Parquet
        ! requires every column in a table to have equal length. 6 rows * 10 bytes = 60 > 40,
        ! and 6 rows * 2 * 6 bytes = 72 > 40: both overflow this shrunk threshold, forcing both
        ! parquet_append_string_column and parquet_append_string_array_column onto the
        ! arrow::large_utf8() path.
        call parquet_debug_set_string_offset_limit(40_int64)

        call schema%init(table="large_string_table")
        call schema%add_field("s", "string", array_size=10)
        call schema%add_field("v", "string", col_size=2, array_size=6)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "s", s_values)
        call parquet_write_column(writer, "v", v_values)
        call parquet_close_writer(writer)

        ! Restore the real production limit right after writing -- defensive, since this
        ! scenario is a one-shot subprocess that exits right after anyway, but keeps this
        ! correct if a later edit ever adds more writes to this same scenario.
        call parquet_debug_set_string_offset_limit(0_int64)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "s", s_back)
        call parquet_read_column(reader, "v", v_back)
        call parquet_get_string_length(reader, "s", strlen_max)
        call parquet_close_reader(reader, print_stat=.true.)

        if (.not. all(s_back == s_values)) then
            error stop "scalar string column did not round-trip through the arrow::large_utf8() path"
        end if
        if (.not. all(v_back == v_values)) then
            error stop "vector string column did not round-trip through the arrow::large_utf8() path"
        end if
        if (strlen_max /= 7) then
            error stop "parquet_get_string_length was wrong for a large_utf8 scalar string column"
        end if

        ! Row-filtering (eval_filter_clause) on a large_utf8 scalar string column.
        call filt%add('s == "charlie"')
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        if (nrows /= 1_int64) then
            error stop "row filter on a large_utf8 scalar string column did not match exactly one row"
        end if
    end subroutine scenario_large_string_roundtrip

    !> Arrow's arrow::FixedSizeListBuilder/fixed_size_list() take a vector column's per-row
    !> width (col_size) as a plain int32_t -- unlike the string byte-offset limit above, there
    !> is no "large" variant to auto-upgrade to, so check_col_size_fits_arrow_limit in
    !> parquet_wrapper.cpp aborts cleanly (via report_fatal_error) rather than silently
    !> truncating col_size and corrupting the written column (see the README's Limitations
    !> section). A genuine col_size beyond 2^31-1 needs far too much memory per row to build in
    !> the normal fpm test suite, so this scenario instead calls parquet_debug_set_col_size_limit
    !> (a process-global, test-only hook declared locally below, not part of the public Fortran
    !> API -- see its own comment in parquet_wrapper.cpp) to shrink the threshold to a handful of
    !> elements, forcing a tiny fixture through the same abort path. Safe as a process-global for
    !> the same subprocess-isolation reason as scenario_large_string_roundtrip's own
    !> parquet_debug_set_string_offset_limit use, above.
    subroutine scenario_col_size_overflow()
        interface
            subroutine parquet_debug_set_col_size_limit(n) &
                bind(C, name="parquet_debug_set_col_size_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! col_size threshold to use instead of the real 2^31-1 limit; <=0 restores it.
            end subroutine parquet_debug_set_col_size_limit
        end interface

        type(parquet_writer) :: writer
        integer(int32) :: data(6, 1)

        data = reshape([1, 2, 3, 4, 5, 6], [6, 1])
        call parquet_debug_set_col_size_limit(5_int64)

        call parquet_open_writer(writer, "test_run/error_scenario_col_size_overflow.parquet")
        call parquet_write_column(writer, "v", data)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a column with col_size exceeding the (shrunk) Arrow limit without error"
    end subroutine scenario_col_size_overflow

    !> Regression coverage for the "List index overflow" crash parquet_get_col_size/
    !> parquet_get_column_total_elements/parquet_read_array_row_mode/parquet_read_array_element_
    !> mode used to hit once a vector column's total element count (nrows * col_size) exceeded
    !> 2^31-1: all four used to materialize the *whole* column (via get_single_chunk_array's
    !> ReadColumn) just to answer a size query, fetch one row, or fetch one element position
    !> across all rows -- which is exactly what Arrow's own int32 list-index ceiling trips over on
    !> a genuinely huge column (see CLAUDE.md's "Guarding a hard Arrow int32-only ceiling"). The
    !> fix makes col_size/total_elements read the FIXED_SIZE_LIST width straight off the schema
    !> (no data read at all), makes row-mode reads fetch only the one row group the requested row
    !> lives in (get_row_group_chunk_array), and makes element-mode reads stream row group by row
    !> group (every row group contributes one element per row, so none can be skipped the way
    !> row-mode skips all but one -- see stream_element_mode_row_groups's own comment in
    !> parquet_wrapper.cpp) -- instead of any of the four ever materializing the whole column.
    !> A genuine >2^31-element column is far too slow/large to build in a fast scenario, so this
    !> instead uses parquet_debug_set_force_whole_column_read_error (a process-global, test-only
    !> hook declared locally below, not part of the public Fortran API -- see its own comment in
    !> parquet_wrapper.cpp) to force get_single_chunk_array to abort the instant it would actually
    !> read a whole column -- on a tiny fixture, this scenario finishing without aborting proves
    !> none of the four calls below ever took that path. scenario_whole_column_read_forced_error_
    !> control, just below, is the negative control proving the hook itself actually fires (so
    !> this scenario's "no abort" isn't simply because the hook is a no-op). Safe as a
    !> process-global for the same subprocess-isolation reason as
    !> scenario_large_string_roundtrip's own parquet_debug_set_string_offset_limit use, above.
    subroutine scenario_col_size_and_row_mode_avoid_whole_column_read()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_col_size_and_row_mode_avoid_whole_column_read.parquet"
        integer(int32) :: vec_data(3, 4), row_buf(3), elem_buf(4)
        integer :: col_size_back
        integer(int64) :: total_elems

        vec_data = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4])

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", vec_data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)

        ! Forced on *before* any of the four calls below, so their success (rather than the
        ! forced abort) is the whole point of this scenario.
        call parquet_debug_set_force_whole_column_read_error(1)

        call parquet_get_col_size(reader, "vec", col_size_back)
        if (col_size_back /= 3) error stop "col_size mismatch for vector column"

        call parquet_get_column_total_elements(reader, "vec", total_elems)
        if (total_elems /= 12_int64) error stop "total element count mismatch for vector column"

        call parquet_read_array_row_mode(reader, "vec", row_buf, 2)
        if (any(row_buf /= [4, 5, 6])) error stop "row_mode values mismatch for row 2"

        call parquet_read_array_element_mode(reader, "vec", elem_buf, 2)
        if (any(elem_buf /= [2, 5, 8, 11])) error stop "element_mode values mismatch for element 2"

        call parquet_debug_set_force_whole_column_read_error(0)
        call parquet_close_reader(reader)
        print '(a)', "parquet_get_col_size/parquet_get_column_total_elements/" // &
            "parquet_read_array_row_mode/parquet_read_array_element_mode all avoided a whole-column read, as expected"
    end subroutine scenario_col_size_and_row_mode_avoid_whole_column_read

    !> Negative control for scenario_col_size_and_row_mode_avoid_whole_column_read, above: proves
    !> parquet_debug_set_force_whole_column_read_error actually does something, by calling a
    !> function that legitimately still takes get_single_chunk_array's whole-column path
    !> (parquet_read_column on a plain scalar column) while the forced error is active, and
    !> expecting it to abort.
    subroutine scenario_whole_column_read_forced_error_control()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_whole_column_read_forced_error_control.parquet"
        integer(int32) :: scalar_data(4), scalar_back(4)

        scalar_data = [1, 2, 3, 4]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "s", scalar_data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_debug_set_force_whole_column_read_error(1)
        call parquet_read_column(reader, "s", scalar_back)
        print '(a)', "unexpectedly read a plain scalar column without triggering the forced whole-column-read error"
    end subroutine scenario_whole_column_read_forced_error_control

    !> Separate from col_size alone (scenario_col_size_overflow, above): Parquet's own
    !> repetition/definition-level generation for list-typed columns walks every flattened
    !> element *of a single row group* (row_group_rows * col_size) with a plain int32_t counter.
    !> This is scoped to one row group, not the whole file -- close_parquet_writer's auto-sizing
    !> path (chunk_size never passed by the caller) silently clamps its own computed row-group
    !> size down to whatever is safe for the widest vector column present (see
    !> max_fixed_size_list_col_size/kArrowInt32ListElementCountLimit in parquet_wrapper.cpp), so a
    !> vector column whose *total* nrows * col_size exceeds 2^31-1 now writes successfully, split
    !> across multiple row groups, rather than aborting (see the README's Limitations section --
    !> only col_size alone, or an *explicit* chunk_size that conflicts with col_size, i.e.
    !> scenario_list_element_count_explicit_chunk_size_overflow below, still abort). A genuine
    !> nrows * col_size beyond 2^31-1 needs far too much memory to build in the normal fpm test
    !> suite, so this scenario instead calls parquet_debug_set_list_element_count_limit (a
    !> process-global, test-only hook declared locally below, not part of the public Fortran API
    !> -- see its own comment in parquet_wrapper.cpp) to shrink the threshold to a handful of
    !> elements, forcing a tiny fixture through the same clamp with more than one row group.
    !> Safe as a process-global for the same subprocess-isolation reason as
    !> scenario_large_string_roundtrip's own parquet_debug_set_string_offset_limit use, above.
    subroutine scenario_list_element_count_auto_multi_row_group()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! per-row-group nrows*col_size threshold; <=0 restores the real 2^31-1 limit.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/error_scenario_list_element_count_multi_row_group.parquet"
        integer(int32) :: data(2, 3), data_back(2, 3)

        data = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        ! col_size=2, limit=5 -> max_safe_chunk_size = max(5/2, 1) = 2 rows/group, forcing this
        ! 3-row column to split into (at least) two row groups (e.g. 2 rows then 1) instead of
        ! the single row group it would otherwise get for a table this tiny.
        call parquet_debug_set_list_element_count_limit(5_int64)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", data)
        call parquet_close_writer(writer)

        ! Restore the real production limit right after writing -- defensive, since this
        ! scenario is a one-shot subprocess that exits right after anyway, but keeps this
        ! correct if a later edit ever adds more writes to this same scenario.
        call parquet_debug_set_list_element_count_limit(0_int64)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", data_back)
        call parquet_close_reader(reader)

        if (.not. all(data_back == data)) then
            error stop "vector column split across multiple row groups did not round-trip correctly"
        end if
    end subroutine scenario_list_element_count_auto_multi_row_group

    !> Counterpart to scenario_list_element_count_auto_multi_row_group, above: an *explicit*
    !> chunk_size (parquet_open_writer(..., chunk_size=)/parquet_set_writer_options) that
    !> conflicts with a vector column's col_size is validated rather than silently overridden --
    !> silently shrinking a caller's explicit request would be a surprising, hard-to-notice
    !> performance change, unlike the auto-sized path above where nothing was explicitly
    !> requested to deviate from. check_explicit_chunk_size_fits_arrow_limit in
    !> parquet_wrapper.cpp aborts cleanly (via report_fatal_error) instead. Uses the same
    !> process-global debug hook as the scenario above, for the same reason.
    subroutine scenario_list_element_count_explicit_chunk_size_overflow()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! per-row-group nrows*col_size threshold; <=0 restores the real 2^31-1 limit.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface

        type(parquet_writer) :: writer
        integer(int32) :: data(2, 3)

        data = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        ! col_size=2, explicit chunk_size=3 -> 3*2=6 > 5 (the shrunk limit): conflicts.
        call parquet_debug_set_list_element_count_limit(5_int64)

        call parquet_open_writer(writer, &
            "test_run/error_scenario_list_element_count_explicit_chunk_size_overflow.parquet", chunk_size=3)
        call parquet_write_column(writer, "v", data)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a vector column with an explicit chunk_size*col_size exceeding the " // &
            "(shrunk) Arrow limit without error"
    end subroutine scenario_list_element_count_explicit_chunk_size_overflow

    !> An explicit parquet_new_row_group(writer, nrows) whose nrows, combined with a
    !> schema-declared vector column's col_size, would exceed Arrow/Parquet's int32
    !> per-row-group element-count limit aborts cleanly (via report_fatal_error), the same way
    !> an explicit chunk_size does for the batch (WriteTable) path -- see
    !> scenario_list_element_count_explicit_chunk_size_overflow, above, for why this uses a
    !> shrunk test-only threshold.
    subroutine scenario_row_group_explicit_nrows_overflow()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! per-row-group nrows*col_size threshold; <=0 restores the real 2^31-1 limit.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface

        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(2, 3)

        data = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        ! col_size=2, nrows=3 -> 3*2=6 > 5 (the shrunk limit): conflicts.
        call parquet_debug_set_list_element_count_limit(5_int64)

        call schema%init(table="row_group_overflow_table")
        call schema%add_field("v", "int32", col_size=2)
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_row_group_explicit_nrows_overflow.parquet", schema)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "v", data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly started a row group with an explicit nrows*col_size exceeding the (shrunk) " // &
            "Arrow limit without error"
    end subroutine scenario_row_group_explicit_nrows_overflow

    !> Calling parquet_close_writer while a row group is still open (parquet_new_row_group
    !> called but parquet_finish_row_group never was) aborts -- see close_streaming_writer in
    !> parquet_wrapper.cpp.
    subroutine scenario_row_group_dangling_at_close()
        type(parquet_writer) :: writer
        integer(int32) :: data(3)

        data = [1, 2, 3]
        call parquet_open_writer(writer, "test_run/error_scenario_row_group_dangling_at_close.parquet", chunk_size=3)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "v", data)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly closed a writer with a dangling open row group without error"
    end subroutine scenario_row_group_dangling_at_close

    !> Closing a writer where a whole (parquet_write_column) column's row count exceeds what the
    !> streaming row-group API actually covered aborts -- see close_streaming_writer in
    !> parquet_wrapper.cpp.
    subroutine scenario_row_group_whole_column_undercovered()
        type(parquet_writer) :: writer
        integer(int32) :: whole_data(5), chunk_data(3)

        whole_data = [1, 2, 3, 4, 5]
        chunk_data = [10, 20, 30]
        call parquet_open_writer(writer, &
            "test_run/error_scenario_row_group_whole_column_undercovered.parquet", chunk_size=3)
        call parquet_write_column(writer, "whole", whole_data)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "chunked", chunk_data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly closed a writer with an under-covered whole column without error"
    end subroutine scenario_row_group_whole_column_undercovered

    !> A row group whose rows would read past the end of an already-whole (parquet_write_column)
    !> column aborts immediately at parquet_finish_row_group, rather than waiting until close --
    !> see parquet_finish_row_group in parquet_wrapper.cpp.
    subroutine scenario_row_group_whole_column_overrun()
        type(parquet_writer) :: writer
        integer(int32) :: whole_data(3), chunk_data(5)

        whole_data = [1, 2, 3]
        chunk_data = [10, 20, 30, 40, 50]
        call parquet_open_writer(writer, "test_run/error_scenario_row_group_whole_column_overrun.parquet", &
            chunk_size=5)
        call parquet_write_column(writer, "whole", whole_data)
        call parquet_new_row_group(writer, 5)
        call parquet_write_column_chunk(writer, "chunked", chunk_data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly finished a row group that reads past a whole column's own row count without error"
    end subroutine scenario_row_group_whole_column_overrun

    !> A column introduced (via its first parquet_write_column_chunk call) after the first row
    !> group has already been written aborts -- a Parquet file's schema is fixed from that point
    !> on. See check_column_chunk_write_preconditions in parquet_wrapper.cpp.
    subroutine scenario_row_group_new_column_after_first()
        type(parquet_writer) :: writer
        integer(int32) :: a_data(2), b_data(2)

        a_data = [1, 2]
        b_data = [10, 20]
        call parquet_open_writer(writer, "test_run/error_scenario_row_group_new_column_after_first.parquet", &
            chunk_size=2)
        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "a", a_data)
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "a", a_data)
        call parquet_write_column_chunk(writer, "b", b_data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly introduced a new column after the first row group without error"
    end subroutine scenario_row_group_new_column_after_first

    !> Arrow's arrow::Schema::num_fields()/GetFieldIndex() both return a plain int32_t internally
    !> -- unlike row count there is no "large" variant for column count at all, so
    !> check_column_count_fits_arrow_limit in parquet_wrapper.cpp (called from both the
    !> schema-based parquet_add_column_metadata and the schema-less append_column registration
    !> paths) aborts cleanly (via report_fatal_error) before the table's column count could ever
    !> reach that limit, rather than risking Arrow's own field-count bookkeeping silently
    !> wrapping/corrupting (see the README's Limitations section). A genuine >2^31-1-column table
    !> needs far too much memory/time to build in the normal fpm test suite, so this scenario
    !> instead calls parquet_debug_set_column_count_limit (a process-global, test-only hook
    !> declared locally below, not part of the public Fortran API -- see its own comment in
    !> parquet_wrapper.cpp) to shrink the threshold to a handful of columns, forcing a tiny
    !> fixture through the same abort path. Safe as a process-global for the same
    !> subprocess-isolation reason as scenario_large_string_roundtrip's own
    !> parquet_debug_set_string_offset_limit use, further above.
    subroutine scenario_column_count_overflow()
        interface
            subroutine parquet_debug_set_column_count_limit(n) &
                bind(C, name="parquet_debug_set_column_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! column-count threshold to use instead of the real 2^31-1 limit; <=0 restores it.
            end subroutine parquet_debug_set_column_count_limit
        end interface

        type(parquet_writer) :: writer

        call parquet_debug_set_column_count_limit(3_int64)

        call parquet_open_writer(writer, "test_run/error_scenario_column_count_overflow.parquet")
        call parquet_write_column(writer, "c1", [1, 2, 3])
        call parquet_write_column(writer, "c2", [1, 2, 3])
        call parquet_write_column(writer, "c3", [1, 2, 3])
        call parquet_write_column(writer, "c4", [1, 2, 3])
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a table with column count exceeding the (shrunk) Arrow limit without error"
    end subroutine scenario_column_count_overflow

    !> parquet_open_reader(..., filter=) validates every filter column name
    !> against the file's actual schema before applying it, the same as
    !> parquet_prefetch_columns does for its own names -- an unknown column
    !> reports a clean Fortran error stop naming it, rather than reaching
    !> Arrow's own uncaught "Column not found" exception.
    subroutine scenario_filter_unknown_column()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("not_a_real_column > 5")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter naming an unknown column"
    end subroutine scenario_filter_unknown_column

    !> Filtering only supports plain scalar columns (col_size == 1): a rule
    !> naming a vector/list column reports a clean error stop instead of
    !> silently picking (or crashing on) some undefined per-row semantics.
    subroutine scenario_filter_vector_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: vec(2,3)

        vec(:,1) = [1_int64, 2_int64]
        vec(:,2) = [3_int64, 4_int64]
        vec(:,3) = [5_int64, 6_int64]

        call parquet_open_writer(writer, "test_run/filter_vector_column.parquet")
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call filt%add("vec > 3")
        call parquet_open_reader(reader, "test_run/filter_vector_column.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter naming a vector column"
    end subroutine scenario_filter_vector_column

    !> parquet_tokenize_filter_rule (parquet_read.f90) rejects a rule that
    !> doesn't have the "<column> <op> [value]" shape (here: no operator at
    !> all) with a clean error stop, before ever reaching the C++ side.
    subroutine scenario_filter_malformed_rule()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("ra")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a malformed filter rule"
    end subroutine scenario_filter_malformed_rule

    !> parquet_filter%add rejects a rule longer than the fixed 512-character
    !> per-rule storage, erroring inside %add itself (before any open_reader).
    subroutine scenario_filter_rule_too_long()
        type(parquet_filter) :: filt

        call filt%add(repeat("a", 513))
        print '(a)', "unexpectedly accepted a filter rule longer than 512 characters"
    end subroutine scenario_filter_rule_too_long

    !> A rule whose shape is fine ("<column> <op> <value>") but whose value
    !> isn't a valid number for a numeric column reports a clean error stop
    !> naming the bad value and the column, from parquet_reader_set_filter
    !> (parquet_wrapper.cpp) -- distinct from scenario_filter_malformed_rule,
    !> which is a Fortran-side syntax/shape rejection before the value is
    !> ever inspected against the actual column type.
    subroutine scenario_filter_bad_numeric_value()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("id_with_null > abc")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a non-numeric value against a numeric filter column"
    end subroutine scenario_filter_bad_numeric_value

    !> A string column's filter value must be double-quoted; a bare,
    !> unquoted word is rejected rather than silently treated as a string.
    subroutine scenario_filter_unquoted_string_value()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_unquoted_string.parquet")
        call parquet_write_column(writer, "name", ["abc", "def"])
        call parquet_close_writer(writer)

        call filt%add("name == abc")
        call parquet_open_reader(reader, "test_run/filter_unquoted_string.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an unquoted value against a string filter column"
    end subroutine scenario_filter_unquoted_string_value

    !> A boolean column's filter value must be the literal true/false; any
    !> other value (numeric, quoted, or otherwise) is rejected.
    subroutine scenario_filter_bad_boolean_value()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_bad_boolean.parquet")
        call parquet_write_column(writer, "flag", [.true., .false.])
        call parquet_close_writer(writer)

        call filt%add("flag == 5")
        call parquet_open_reader(reader, "test_run/filter_bad_boolean.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an invalid boolean value in a filter rule"
    end subroutine scenario_filter_bad_boolean_value

    !> Ordering comparisons (>, >=, <, <=) don't have a meaningful definition
    !> for a boolean column -- only ==/=/= are accepted; an ordering operator
    !> against a boolean column is rejected.
    subroutine scenario_filter_bool_ordering_not_supported()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_bool_ordering.parquet")
        call parquet_write_column(writer, "flag", [.true., .false.])
        call parquet_close_writer(writer)

        call filt%add("flag > true")
        call parquet_open_reader(reader, "test_run/filter_bool_ordering.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an ordering comparison against a boolean filter column"
    end subroutine scenario_filter_bool_ordering_not_supported

    !> Writes `lines` verbatim to `path`, one per record -- used by the
    !> read-time qc scenarios below to produce a throwaway qc-maml file
    !> (parquet_load_qc_maml_file only reads from disk, no in-memory
    !> constructor exists for a qc-maml, same as every other maml in this
    !> codebase).
    subroutine write_text_file(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        integer :: unit, i

        open(newunit=unit, file=path, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine write_text_file

    !> parquet_open_reader(..., schema=) with a qc: min:/max: declared for
    !> "ra" must print exactly one aggregate WARNING (matching the writer's
    !> own qc: wording) when parquet_read_column reads an out-of-range
    !> value, but must NOT abort -- with qc_soft=.true. qc is diagnostic-only.
    !> (The default, qc_soft=.false., aborts instead -- see
    !> scenario_qc_range_violation_hard_aborts.)
    subroutine scenario_qc_range_violation_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300] ! 400 and -5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_range.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_range_violation_warns

    !> A stray line with no colon inside a qc-maml field block (not blank,
    !> not "#"-prefixed) is silently skipped by parquet_split_key_value's
    !> no-colon branch (src/parquet_metadata.f90) -- parsing continues
    !> normally rather than erroring, matching the deliberate leniency every
    !> other MAML-parsing call site of parquet_split_key_value relies on
    !> (only 2 of its 17 call sites pre-check for a colon; the rest, like
    !> this one, just treat the resulting empty key as "skip this line").
    !> Uses an out-of-range value so the qc: min: bound declared *after* the
    !> stray line still triggers its usual WARNING -- proving the stray line
    !> didn't corrupt or swallow the qc: block that follows it, not just
    !> that nothing crashed.
    subroutine scenario_qc_maml_stray_no_colon_line()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(3), ra_back(3)

        ra = [10, -5, 30] ! -5 violates the qc: min: 0 bound below

        call parquet_open_writer(writer, "test_run/qc_stray_line.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_stray_line.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  some garbage text", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_stray_line.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_stray_line.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_maml_stray_no_colon_line

    !> A column with a genuine Parquet Null, read with is_valid= (so the
    !> read itself doesn't abort), against a qc-maml field with a bare
    !> "miss:" key (present but no value after the colon) must behave
    !> exactly like omitting miss: entirely -- Nulls still unexpected by
    !> default -- and print exactly one aggregate Null-presence WARNING with
    !> qc_soft=.true. (The default, qc_soft=.false., aborts instead -- see
    !> scenario_qc_null_violation_hard_aborts.) The bare "miss:" line
    !> exercises parquet_parse_qc_maml's empty-value branch specifically
    !> (src/parquet_metadata.f90), distinct from miss: being absent.
    subroutine scenario_qc_null_violation_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_null.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_null.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    min: 0", "    miss:"])

        call parquet_open_reader(reader, "test_run/qc_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_null.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_null_violation_warns

    !> Default (qc_soft=.false., hard): reading an out-of-range value with
    !> qc active aborts the process, via the report_fatal_error convention
    !> (stderr diagnostic + SIGABRT), the same class of clean read-side
    !> abort as the Null/type-mismatch checks.
    subroutine scenario_qc_range_violation_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300] ! 400 and -5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range_hard.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_hard.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_range_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_hard.maml"))
        call parquet_read_column(reader, "ra", ra_back)
        print '(a)', "unexpectedly read an out-of-range value without aborting in hard qc mode"
    end subroutine scenario_qc_range_violation_hard_aborts

    !> Default (qc_soft=.false., hard): an unexpected Null (miss: not
    !> Null/NA) with qc active aborts the process, even when is_valid= was
    !> passed so the read itself would otherwise succeed.
    subroutine scenario_qc_null_violation_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_null_hard.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_null_hard.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_null_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_null_hard.maml"))
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        print '(a)', "unexpectedly read an unexpected Null without aborting in hard qc mode"
    end subroutine scenario_qc_null_violation_hard_aborts

    !> Same as scenario_qc_null_violation_warns, except this qc-maml field
    !> declares qc: miss: Null -- Nulls are expected here, so no WARNING
    !> should ever print (checked as an ABSENCE by the test, since this
    !> scenario's whole point is that nothing unusual happens).
    subroutine scenario_qc_miss_null_no_warning()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_miss_null.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_miss_null.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    miss: Null"])

        call parquet_open_reader(reader, "test_run/qc_miss_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_miss_null.maml"))
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_miss_null_no_warning

    !> qc being active must never change the existing strict-by-default Null
    !> behavior: reading a column with a genuine Null and no null_value=/
    !> is_valid= still aborts with the same message as without any qc-maml
    !> at all (see scenario_read_column_with_nulls).
    subroutine scenario_qc_existing_null_abort_unchanged()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_abort_unchanged.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_abort_unchanged.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    miss: Null"])

        call parquet_open_reader(reader, "test_run/qc_abort_unchanged.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_abort_unchanged.maml"))
        call parquet_read_column(reader, "id", id_back) ! no null_value=/is_valid= -- still expected to abort
        print '(a)', "unexpectedly read a column with a genuine Null without error, even with qc active"
    end subroutine scenario_qc_existing_null_abort_unchanged

    !> A qc-maml is explicitly allowed to declare fields that don't exist in
    !> the actual parquet file -- parquet_open_reader must succeed cleanly,
    !> simply ignoring the unmatched field.
    subroutine scenario_qc_column_not_in_file()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(3)

        id = [1, 2, 3]

        call parquet_open_writer(writer, "test_run/qc_missing_col.parquet")
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_missing_col.maml", [character(len=32) :: &
            "fields:", "- name: does_not_exist", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_missing_col.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_missing_col.maml"))
        call parquet_close_reader(reader)
    end subroutine scenario_qc_column_not_in_file

    !> qc=.false. always wins over a maml being present: no warning should
    !> print even for a column that would otherwise clearly violate its
    !> declared qc: bounds.
    subroutine scenario_qc_disabled_explicit_no_warning()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300]

        call parquet_open_writer(writer, "test_run/qc_disabled.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_disabled.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_disabled.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_disabled.maml"), qc=.false.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_disabled_explicit_no_warning

    !> An unrecognized qc: miss: value (anything other than Null/NA,
    !> case-insensitive, or empty) is rejected as invalid qc-maml syntax at
    !> parquet_open_reader time, before the parquet file is even touched.
    subroutine scenario_qc_maml_bad_miss_value()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_bad_miss.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    miss: garbage"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_bad_miss.maml"))
        print '(a)', "unexpectedly opened a reader with an unrecognized qc: miss: value"
    end subroutine scenario_qc_maml_bad_miss_value

    !> Two fields:  entries sharing the same name are ambiguous for qc
    !> purposes and rejected, the same as parquet_validate_maml_internal
    !> already rejects a duplicate field name for a schema-authoring maml.
    subroutine scenario_qc_maml_duplicate_field()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_dup_field.maml", [character(len=32) :: &
            "fields:", "- name: ra", "- name: ra"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_dup_field.maml"))
        print '(a)', "unexpectedly opened a reader with a duplicate qc-maml field name"
    end subroutine scenario_qc_maml_duplicate_field

    !> A qc-maml's qc: min: must be a lower bound (>= or >). A reversed
    !> '<'/'<=' operator is rejected when parquet_open_reader parses the
    !> qc-maml, the same rule parquet_validate_maml enforces on the write side.
    subroutine scenario_qc_maml_min_wrong_operator()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_min_wrong_op.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: '< 5'"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_min_wrong_op.maml"))
        print '(a)', "unexpectedly opened a reader with a reversed qc: min: operator"
    end subroutine scenario_qc_maml_min_wrong_operator

    !> Same as scenario_qc_maml_min_wrong_operator, but for qc: max:, which
    !> must be an upper bound (<= or <) -- a reversed '>'/'>=' operator is
    !> rejected the same way.
    subroutine scenario_qc_maml_max_wrong_operator()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_max_wrong_op.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    max: '> 5'"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_max_wrong_op.maml"))
        print '(a)', "unexpectedly opened a reader with a reversed qc: max: operator"
    end subroutine scenario_qc_maml_max_wrong_operator

    !> maml%add_col_qc rejects a reversed min: operator ('<'/'<=' is an upper
    !> bound), the same rule the qc-maml parser and the write-side validator
    !> enforce.
    subroutine scenario_add_col_qc_min_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, <5", col_name)
        print '(a)', "unexpectedly accepted a reversed qc min operator in add_col_qc"
    end subroutine scenario_add_col_qc_min_reversed_operator

    !> Mirror of the min case for the max bound: max: accepts only < / <=, so a
    !> '>' operator on the max bound (third field) must be rejected. Covers the
    !> max-direction branch of check_bound, distinct from the min one above.
    subroutine scenario_add_col_qc_max_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, , >5", col_name)
        print '(a)', "unexpectedly accepted a reversed qc max operator in add_col_qc"
    end subroutine scenario_add_col_qc_max_reversed_operator

    !> maml%add_col_qc rejects a bound that is only an operator with no value.
    subroutine scenario_add_col_qc_operator_without_value()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, >", col_name)
        print '(a)', "unexpectedly accepted an operator with no value in add_col_qc"
    end subroutine scenario_add_col_qc_operator_without_value

    !> As above but for the max bound: an operator with no value (e.g. "<" with
    !> nothing after it) on the max field must also be rejected -- covers the
    !> max branch of check_bound's empty-value guard, distinct from the min one.
    subroutine scenario_add_col_qc_max_operator_without_value()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, , <", col_name)
        print '(a)', "unexpectedly accepted a max operator with no value in add_col_qc"
    end subroutine scenario_add_col_qc_max_operator_without_value

    !> maml%add_col_qc rejects a miss value other than Null/NA/empty.
    subroutine scenario_add_col_qc_bad_miss_value()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra,,, garbage", col_name)
        print '(a)', "unexpectedly accepted an invalid qc miss value in add_col_qc"
    end subroutine scenario_add_col_qc_bad_miss_value

    !> maml%add_col_qc rejects a qc_input with more than four comma-separated fields.
    subroutine scenario_add_col_qc_too_many_fields()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, 1, 2, Null, extra", col_name)
        print '(a)', "unexpectedly accepted more than four fields in add_col_qc"
    end subroutine scenario_add_col_qc_too_many_fields

    !> maml%add_col_qc rejects an empty first field (a leading comma / empty name).
    subroutine scenario_add_col_qc_empty_column_name()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc(", >0", col_name)
        print '(a)', "unexpectedly accepted an empty column name in add_col_qc"
    end subroutine scenario_add_col_qc_empty_column_name

    !> maml%add_col_qc rejects a column already declared earlier in the same maml.
    subroutine scenario_add_col_qc_duplicate_column()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, >0", col_name)
        call maml%add_col_qc("ra, <10", col_name)
        print '(a)', "unexpectedly accepted a duplicate column name in add_col_qc"
    end subroutine scenario_add_col_qc_duplicate_column

    !> maml_field_name_exists strips a single pair of surrounding quotes off a
    !> "- name: ..." value before comparing -- exercised here with a
    !> single-quoted existing entry ("- name: 'dup'"), so add_col_qc must still
    !> recognize "dup" as already declared and reject it as a duplicate.
    subroutine scenario_add_col_qc_duplicate_column_single_quoted()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        maml%lines = [character(len=20) :: "fields:", "- name: 'dup'"]
        call maml%add_col_qc("dup, >0", col_name)
        print '(a)', "unexpectedly accepted a duplicate column name (single-quoted) in add_col_qc"
    end subroutine scenario_add_col_qc_duplicate_column_single_quoted

    !> Same as scenario_add_col_qc_duplicate_column_single_quoted, but for a
    !> double-quoted existing entry ('- name: "dup"'), so both quote styles
    !> the qc-maml parser accepts for a name: value are covered.
    subroutine scenario_add_col_qc_duplicate_column_double_quoted()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        maml%lines = [character(len=20) :: "fields:", '- name: "dup"']
        call maml%add_col_qc("dup, >0", col_name)
        print '(a)', "unexpectedly accepted a duplicate column name (double-quoted) in add_col_qc"
    end subroutine scenario_add_col_qc_duplicate_column_double_quoted

    !> The get_col_qc in-place form shares add_col_qc's worker, so it enforces
    !> the same validation -- e.g. a reversed min: operator aborts here too.
    subroutine scenario_get_col_qc_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        col_name = "ra, <5"
        call maml%get_col_qc(col_name)
        print '(a)', "unexpectedly accepted a reversed qc min operator in get_col_qc"
    end subroutine scenario_get_col_qc_reversed_operator

    !> A fields: entry with no name: at all is rejected -- name is the one
    !> required attribute for a qc-maml field (everything else, including
    !> qc: itself, is optional).
    subroutine scenario_qc_maml_missing_name()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_missing_name.maml", [character(len=32) :: &
            "fields:", "- unit: cm"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_missing_name.maml"))
        print '(a)', "unexpectedly opened a reader with a qc-maml field missing 'name'"
    end subroutine scenario_qc_maml_missing_name

    !> parquet_parse_qc_maml reuses parquet_validate_maml_sections (the same
    !> section/sub-key schema every other maml validation path checks), so a
    !> typo'd qc: sub-key (here "minimum" instead of "min") is caught the
    !> same way it would be for a schema-authoring maml.
    subroutine scenario_qc_maml_unknown_subkey()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_bad_subkey.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    minimum: 5"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_bad_subkey.maml"))
        print '(a)', "unexpectedly opened a reader with an unknown qc: sub-key"
    end subroutine scenario_qc_maml_unknown_subkey

    !> Opening a nonexistent file for reading previously called Arrow's
    !> ValueOrDie() with no status check first, which aborts the process
    !> directly (not a catchable C++ exception) with a generic message.
    !> create_parquet_reader (parquet_wrapper.cpp) now checks Arrow's status
    !> first and reports a clean, specific diagnostic before aborting.
    subroutine scenario_open_reader_missing_file()
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, "test_run/does_not_exist_xyz_123.parquet")
        print '(a)', "unexpectedly opened a nonexistent file for reading without error"
    end subroutine scenario_open_reader_missing_file

    !> parquet_open_reader(nrows=) internally calls parquet_get_nrows with
    !> check_positive=.true., so a filter matching zero rows must abort at
    !> open time instead of silently returning nrows=0 -- see the nrows
    !> doc comment on parquet_open_reader in src/parquet.f90.
    subroutine scenario_open_reader_nrows_zero_rows()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = "test_run/open_reader_nrows_zero_rows.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", [1, 2, 3])
        call parquet_close_writer(writer)

        call filt%add("id > 100")
        call parquet_open_reader(reader, out_file, filter=filt, nrows=nrows)
        print '(a,i0)', "unexpectedly opened a reader with zero filtered rows, nrows=", nrows
    end subroutine scenario_open_reader_nrows_zero_rows

    !> Same as scenario_open_reader_missing_file but for the write side: a
    !> path under a nonexistent directory can never be opened for writing.
    subroutine scenario_open_writer_bad_path()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/no_such_directory_xyz/out.parquet")
        print '(a)', "unexpectedly opened a bad path for writing without error"
    end subroutine scenario_open_writer_bad_path

    !> parquet_write_string_column (scalar) checks a string's trimmed length
    !> against the schema's declared array_size and error stops if exceeded;
    !> parquet_write_string_matrix_column (this scenario) previously had no
    !> equivalent check, silently truncating an over-length string in a
    !> fixed-length string vector/matrix column instead of erroring.
    subroutine scenario_write_string_matrix_exceeds_array_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=20) :: values(2, 1)

        schema%maml%name = "string_matrix_array_size.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: string_matrix_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 5", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        values(1, 1) = "short"
        values(2, 1) = "this_is_way_too_long"

        call parquet_open_writer(writer, "test_run/error_scenario_string_matrix_array_size.parquet", schema)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an over-length string into a fixed-size string matrix column without error"
    end subroutine scenario_write_string_matrix_exceeds_array_size

    !> Same check as scenario_write_string_matrix_exceeds_array_size, but for
    !> parquet_write_string_column's 1D/flat form (values(:), dispatched to
    !> for a rank-1 actual argument -- used for both a plain scalar string
    !> column and a flattened string-vector column like this one). Its
    !> max_item_len = maxval(len_trim(values)) check (src/parquet_write.f90)
    !> is the same pattern as the matrix form's, but had no dedicated test.
    subroutine scenario_write_string_exceeds_array_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=20) :: values(2)

        schema%maml%name = "string_flat_array_size.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: string_flat_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 5", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        values(1) = "short"
        values(2) = "this_is_way_too_long"

        call parquet_open_writer(writer, "test_run/error_scenario_string_flat_array_size.parquet", schema)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an over-length string into a fixed-size string vector column (flat form) without error"
    end subroutine scenario_write_string_exceeds_array_size

    subroutine scenario_validate_protected_cols_unknown_name()
        type(parquet_maml_file) :: maml

        maml%name = "protected_unknown.maml"
        maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: not_a_real_column", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        ! "not_a_real_column" is not declared under fields: in this same
        ! MAML, so it must be rejected as a dangling reference.
        call parquet_validate_maml(maml)
    end subroutine scenario_validate_protected_cols_unknown_name

    subroutine scenario_write_protected_column_with_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        schema%maml%name = "protected_write.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_protected_write.parquet", schema)
        call parquet_write_column(writer, "a", values, is_valid=is_valid)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a Null into a protected column without error"
    end subroutine scenario_write_protected_column_with_null

    subroutine scenario_validate_qc_min_not_numeric()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_not_numeric.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: not_a_number" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_not_numeric

    subroutine scenario_validate_qc_min_non_integral_for_int32()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_non_integral.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1.5" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_non_integral_for_int32

    subroutine scenario_validate_qc_min_out_of_int32_range()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_out_of_range.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 5000000000" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_out_of_int32_range

    !> qc: min: must be a lower bound: a '<'/'<=' operator on min: is a
    !> reversed, nonsensical bound and is rejected by parquet_validate_maml.
    subroutine scenario_validate_qc_min_wrong_operator()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_wrong_operator.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: '< 5'" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_wrong_operator

    !> qc: max: must be an upper bound: a '>'/'>=' operator on max: is a
    !> reversed, nonsensical bound and is rejected by parquet_validate_maml.
    subroutine scenario_validate_qc_max_wrong_operator()
        type(parquet_maml_file) :: maml

        maml%name = "qc_max_wrong_operator.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    max: '>= 5'" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_max_wrong_operator

    !> Not an error scenario: qc=.true. only ever prints a WARNING and lets
    !> the write proceed. This scenario exits cleanly (exit 0); the
    !> corresponding test (test_writing.f90) captures stdout via a subprocess
    !> and checks for the WARNING text, since test-drive itself can't
    !> observe stdout produced by an in-process print statement reliably.
    subroutine scenario_qc_warning_numeric()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(5) = [1_int32, 5_int32, 1500_int32, -3_int32, 10_int32]

        schema%maml%name = "qc_warning.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1", &
            "    max: 1000" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "a", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_numeric

    !> Same as scenario_qc_warning_numeric, but with a genuinely fractional
    !> qc: min: bound ("0.5") rather than a whole number -- every existing
    !> numeric qc-warning scenario uses whole-number bounds, so the WARNING
    !> text's bounds_desc always went through parquet_qc_format_real's
    !> whole-number (i0) branch, never its fractional (g0.7) one.
    subroutine scenario_qc_warning_fractional_bound()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: values(3) = [1.0_real32, 0.1_real32, 2.0_real32]

        schema%maml%name = "qc_warning_fractional.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: f", &
            "  data_type: float32", &
            "  qc:", &
            "    min: 0.5" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning_fractional.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "f", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_fractional_bound

    subroutine scenario_qc_warning_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: values(3) = ["banana  ", "apple   ", "cherry  "]

        ! Both min: and max: are declared so the violation warning's bounds
        ! description includes its "max ..." clause (exercises the has_qc_max
        ! branch of parquet_check_qc_string's bounds_desc). "apple" still
        ! violates min:'banana'; max:'cherry' is satisfied by every value.
        schema%maml%name = "qc_warning_string.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 10", &
            "  qc:", &
            "    min: 'banana'", &
            "    max: 'cherry'" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning_string.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_string

    !> qc: on a boolean field is accepted by validation but never enforced;
    !> this must write/close without error and without printing a WARNING.
    subroutine scenario_qc_silently_ignored_for_boolean()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: values(3) = [.true., .false., .true.]

        schema%maml%name = "qc_boolean.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: b", &
            "  data_type: boolean", &
            "  qc:", &
            "    min: 0", &
            "    max: 0" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_boolean.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "b", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_silently_ignored_for_boolean

    subroutine scenario_write_unknown_compression()
        type(parquet_writer) :: writer
        integer(int32) :: values(1) = [1_int32]

        call parquet_open_writer(writer, "test_run/error_scenario_unknown_compression.parquet", &
            compression="not_a_real_codec")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly opened a writer with an unknown compression codec without error"
    end subroutine scenario_write_unknown_compression

    !> parquet_open_writer(..., overwrite=.false.) must error stop rather than truncate an
    !> existing file at that path -- write the file once normally, then reopen it with
    !> overwrite=.false. and expect that second open to abort.
    subroutine scenario_write_overwrite_false_existing_file()
        type(parquet_writer) :: writer
        integer(int32) :: values(1) = [1_int32]
        character(len=*), parameter :: out_file = "test_run/error_scenario_overwrite_false.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_writer(writer, out_file, overwrite=.false.)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly opened a writer with overwrite=.false. over an existing file without error"
    end subroutine scenario_write_overwrite_false_existing_file

    !> "v" is declared with col_size: 2 (a vector column), so a 1D values(:)
    !> array passed to parquet_write_column must have a length divisible by
    !> 2; length 3 is not, and used to hit a plain `stop` (exit code 0, no
    !> actual failure signaled) instead of `error stop` -- this scenario
    !> guards against that regressing.
    subroutine scenario_write_values_not_divisible_by_col_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]

        schema%maml%name = "col_size_mismatch.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: col_size_mismatch_table", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_col_size_mismatch.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a values(:) array whose length isn't divisible by col_size without error"
    end subroutine scenario_write_values_not_divisible_by_col_size

    !> Writing to a schema-declared int32 column with an int64 values(:)
    !> array is normally allowed (parquet_narrow_int64_to_int32 converts), but
    !> a value outside int32's range must error stop rather than silently
    !> wrapping.
    subroutine scenario_write_int64_to_int32_overflow()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: values(1) = [huge(0_int32) + 1_int64]

        call schema%init(table="int64_overflow_table")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_int64_to_int32_overflow.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an out-of-int32-range int64 value to an int32 schema column without error"
    end subroutine scenario_write_int64_to_int32_overflow

    !> A float64 values(:) array written to an int32 schema column converts
    !> only if every value is integral (src(i) == anint(src(i))); a
    !> non-integral value must error stop rather than silently truncating.
    subroutine scenario_write_float_to_int32_non_integral()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [3.5_real64]

        call schema%init(table="float_non_integral_table")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int32_non_integral.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a non-integral float64 value to an int32 schema column without error"
    end subroutine scenario_write_float_to_int32_non_integral

    !> Same conversion as above, but the value is integral and simply out of
    !> int32's representable range -- also must error stop.
    subroutine scenario_write_float_to_int32_out_of_range()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [real(huge(0_int32), real64) + 1.0_real64]

        call schema%init(table="float_out_of_range_table")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int32_out_of_range.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an out-of-int32-range float64 value to an int32 schema column without error"
    end subroutine scenario_write_float_to_int32_out_of_range

    !> Same non-integral check as scenario_write_float_to_int32_non_integral,
    !> but for the int64 schema-column conversion path
    !> (parquet_float64_to_int64), which has its own identical check.
    subroutine scenario_write_float_to_int64_non_integral()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [3.5_real64]

        call schema%init(table="float_non_integral_i64_table")
        call schema%add_field("v", "int64")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int64_non_integral.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a non-integral float64 value to an int64 schema column without error"
    end subroutine scenario_write_float_to_int64_non_integral

    !> Same out-of-range check as scenario_write_float_to_int32_out_of_range,
    !> but for the int64 schema-column conversion path
    !> (parquet_float64_to_int64), which has its own identical check.
    subroutine scenario_write_float_to_int64_out_of_range()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [real(huge(0_int64), real64) * 2.0_real64]

        call schema%init(table="float_out_of_range_i64_table")
        call schema%add_field("v", "int64")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int64_out_of_range.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an out-of-int64-range float64 value to an int64 schema column without error"
    end subroutine scenario_write_float_to_int64_out_of_range

    !> n < 1 is not a valid thread pool capacity -- must error stop rather
    !> than silently passing an invalid value down to Arrow.
    subroutine scenario_set_max_threads_below_one()
        call parquet_set_max_threads(0)
        print '(a)', "unexpectedly accepted parquet_set_max_threads(0) without error"
    end subroutine scenario_set_max_threads_below_one

    !> A schema-enforced writer (cinfo given) already error stops on this via
    !> parquet_mark_column_written's write_counts tracking. A schema-less
    !> writer (no cinfo) previously had no such tracking at all: the C++ side
    !> only detects a duplicate name via column_metadata, which is only
    !> populated from cinfo -- so this used to silently write a file with two
    !> columns both named "id", and only fail much later, on read, with an
    !> uncaught "Column not found: id" exception (Arrow's GetFieldIndex
    !> returns -1 for an ambiguous/duplicate name). parquet_mark_column_written
    !> now tracks written names itself for the schema-less case too, so this
    !> is caught immediately, at the second parquet_write_column call.
    subroutine scenario_write_column_twice_no_schema()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/write_column_twice_no_schema.parquet")
        call parquet_write_column(writer, "id", [1, 2, 3])
        call parquet_write_column(writer, "id", [10, 20, 30])
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote the same column twice on a schema-less writer without error"
    end subroutine scenario_write_column_twice_no_schema

    !> Deliberately violates the documented rule that each thread must use
    !> its own independent parquet_reader (see README's Thread safety
    !> section): every thread here calls parquet_read_column on the *same*
    !> shared reader instance. The reader's internal column cache has no
    !> synchronization, so this must be caught by the ConcurrencyGuard in
    !> parquet_wrapper.cpp rather than silently racing/corrupting memory.
    !> Uses !$omp parallel (not parallel do): every thread runs the *entire*
    !> loop itself, all hammering the same shared reader for many iterations.
    !> A work-shared "parallel do" split across threads turned out not to
    !> reliably overlap in practice (each thread only touching the reader a
    !> handful of times). Beyond that, a *single* barrier right at the start
    !> also turned out not to be reliable enough on its own: how simultaneous
    !> the very first round of calls actually is depends on details like
    !> compiler flags (e.g. -frecursive changes per-call overhead enough to
    !> visibly change contention odds) -- so this re-synchronizes every
    !> thread with a fresh barrier before *every* batch of calls, giving many
    !> repeated chances at genuine overlap throughout the run instead of
    !> just one, regardless of exactly how simultaneous any single batch is.
    subroutine scenario_concurrent_calls_into_shared_reader()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(5) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]
        integer(int32) :: read_back(5)
        integer, parameter :: batches = 200
        integer, parameter :: iterations_per_batch = 500
        integer :: b, i, nthreads

        ! Self-adapting: the race only exists under genuine multi-threading.
        ! Without an OpenMP flag (or with a single thread) the parallel region
        ! below runs serially, the guard cannot fire, and the 100k-iteration
        ! loop would be a pointless "unexpectedly finished" run. Skip cleanly.
        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) then
            print '(a)', "SKIPPED: OpenMP not active (omp_get_max_threads() <= 1); shared-reader race cannot occur"
            return
        end if

        call parquet_open_writer(writer, "test_run/error_scenario_shared_reader.parquet")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/error_scenario_shared_reader.parquet")

        !$omp parallel default(shared) private(b, i, read_back)
        do b = 1, batches
            !$omp barrier
            do i = 1, iterations_per_batch
                call parquet_read_column(reader, "v", read_back)
            end do
        end do
        !$omp end parallel

        call parquet_close_reader(reader)
        print '(a)', "unexpectedly finished concurrent reads of a shared reader without the concurrency guard firing"
    end subroutine scenario_concurrent_calls_into_shared_reader

    !> Same idea as scenario_concurrent_calls_into_shared_reader (including
    !> the repeated-barrier-per-batch rationale above), but for the writer
    !> side: every thread calls parquet_write_column on the *same* shared
    !> writer instance, each writing its own column names (thread index +
    !> batch + iteration) so a successful call would never legitimately fail
    !> for an unrelated reason like a duplicate column name.
    subroutine scenario_concurrent_calls_into_shared_writer()
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer, parameter :: batches = 50
        integer, parameter :: iterations_per_batch = 100
        integer :: b, i, tid, nthreads
        character(len=32) :: colname

        ! Self-adapting: see scenario_concurrent_calls_into_shared_reader.
        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) then
            print '(a)', "SKIPPED: OpenMP not active (omp_get_max_threads() <= 1); shared-writer race cannot occur"
            return
        end if

        call parquet_open_writer(writer, "test_run/error_scenario_shared_writer.parquet")

        !$omp parallel default(shared) private(b, i, tid, colname)
        tid = 0
        !$ tid = omp_get_thread_num()
        do b = 1, batches
            !$omp barrier
            do i = 1, iterations_per_batch
                write(colname, '(A,I0,A,I0,A,I0)') "col_", tid, "_", b, "_", i
                call parquet_write_column(writer, trim(colname), values)
            end do
        end do
        !$omp end parallel

        call parquet_close_writer(writer)
        print '(a)', "unexpectedly finished concurrent writes into a shared writer without the concurrency guard firing"
    end subroutine scenario_concurrent_calls_into_shared_writer

    !> parquet_get_metadata with no `default` given error stops the moment
    !> the requested key isn't present in the file's table metadata (see
    !> parquet_metadata_stop_missing in parquet_metadata.f90).
    subroutine scenario_get_metadata_missing_key_no_default()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(1) = [1_int32]
        integer(int32) :: value

        call parquet_open_writer(writer, "test_run/error_scenario_get_metadata_missing.parquet")
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/error_scenario_get_metadata_missing.parquet")
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing metadata key with no default without error: ", value
    end subroutine scenario_get_metadata_missing_key_no_default

    !> parquet_get_metadata with no `default` given error stops when the
    !> key is present but its stored text cannot be converted to the
    !> requested type (see parquet_metadata_stop_conversion in parquet_metadata.f90).
    subroutine scenario_get_metadata_conversion_failure_no_default()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id(1) = [1_int32]
        integer(int32) :: value

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")
        call schema%add_metadata("not_a_number", "not_a_number")

        call parquet_open_writer(writer, "test_run/error_scenario_get_metadata_bad_type.parquet", schema)
        call parquet_write_column(writer, "id0", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/error_scenario_get_metadata_bad_type.parquet")
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable metadata value with no default without error: ", value
    end subroutine scenario_get_metadata_conversion_failure_no_default

    !> Writes a small parquet file carrying one metadata entry whose value
    !> ("not_a_number") is unparsable as any numeric/logical type, and opens a
    !> reader on it. Shared by the per-type get_metadata abort scenarios below:
    !> a missing-key abort reads an absent key from it, a conversion abort reads
    !> the "not_a_number" key -- both with no `default`, so the specific scalar
    !> variant's stop_missing / stop_conversion call site fires.
    subroutine open_metadata_abort_reader(filename, reader)
        character(len=*), intent(in) :: filename
        type(parquet_reader), intent(out) :: reader
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: id(1) = [1_int32]

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")
        call schema%add_metadata("not_a_number", "not_a_number")

        call parquet_open_writer(writer, filename, schema)
        call parquet_write_column(writer, "id0", id)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, filename)
    end subroutine open_metadata_abort_reader

    subroutine scenario_get_metadata_missing_int64_no_default()
        type(parquet_reader) :: reader
        integer(int64) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_i64.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing int64 metadata key with no default without error: ", value
    end subroutine scenario_get_metadata_missing_int64_no_default

    subroutine scenario_get_metadata_missing_float32_no_default()
        type(parquet_reader) :: reader
        real(real32) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_f32.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,g0)', "unexpectedly read a missing float32 metadata key with no default without error: ", value
    end subroutine scenario_get_metadata_missing_float32_no_default

    subroutine scenario_get_metadata_missing_float64_no_default()
        type(parquet_reader) :: reader
        real(real64) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_f64.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,g0)', "unexpectedly read a missing float64 metadata key with no default without error: ", value
    end subroutine scenario_get_metadata_missing_float64_no_default

    subroutine scenario_get_metadata_missing_logical_no_default()
        type(parquet_reader) :: reader
        logical :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_lg.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,l1)', "unexpectedly read a missing logical metadata key with no default without error: ", value
    end subroutine scenario_get_metadata_missing_logical_no_default

    subroutine scenario_get_metadata_missing_string_no_default()
        type(parquet_reader) :: reader
        character(len=:), allocatable :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_str.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,a)', "unexpectedly read a missing string metadata key with no default without error: ", trim(value)
    end subroutine scenario_get_metadata_missing_string_no_default

    subroutine scenario_get_metadata_conversion_int64_no_default()
        type(parquet_reader) :: reader
        integer(int64) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_i64.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable int64 metadata value with no default without error: ", value
    end subroutine scenario_get_metadata_conversion_int64_no_default

    subroutine scenario_get_metadata_conversion_float32_no_default()
        type(parquet_reader) :: reader
        real(real32) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_f32.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,g0)', "unexpectedly read an unparsable float32 metadata value with no default without error: ", value
    end subroutine scenario_get_metadata_conversion_float32_no_default

    subroutine scenario_get_metadata_conversion_float64_no_default()
        type(parquet_reader) :: reader
        real(real64) :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_f64.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,g0)', "unexpectedly read an unparsable float64 metadata value with no default without error: ", value
    end subroutine scenario_get_metadata_conversion_float64_no_default

    subroutine scenario_get_metadata_conversion_logical_no_default()
        type(parquet_reader) :: reader
        logical :: value

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_lg.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,l1)', "unexpectedly read an unparsable logical metadata value with no default without error: ", value
    end subroutine scenario_get_metadata_conversion_logical_no_default

    !> Array-typed counterparts of the scalar get_metadata no-default abort
    !> scenarios above: parquet_metadata_stop_missing/parquet_metadata_stop_conversion
    !> are called from the *_array getters too, but that was previously only
    !> ever exercised for scalars. Reuses open_metadata_abort_reader's fixture
    !> unchanged -- parquet_metadata_split_array treats a plain (non-bracketed,
    !> comma-less) value like "not_a_number" as a single-element array, so the
    !> same "not_a_number" key that fails to parse as a scalar also fails to
    !> parse as a one-element array.
    subroutine scenario_get_metadata_missing_int32_array_no_default()
        type(parquet_reader) :: reader
        integer(int32), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_i32_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing int32 array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_int32_array_no_default

    subroutine scenario_get_metadata_conversion_int32_array_no_default()
        type(parquet_reader) :: reader
        integer(int32), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_i32_arr.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable int32 array metadata value with no default without error: ", &
            size(value)
    end subroutine scenario_get_metadata_conversion_int32_array_no_default

    subroutine scenario_get_metadata_missing_int64_array_no_default()
        type(parquet_reader) :: reader
        integer(int64), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_i64_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing int64 array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_int64_array_no_default

    subroutine scenario_get_metadata_conversion_int64_array_no_default()
        type(parquet_reader) :: reader
        integer(int64), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_i64_arr.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable int64 array metadata value with no default without error: ", &
            size(value)
    end subroutine scenario_get_metadata_conversion_int64_array_no_default

    subroutine scenario_get_metadata_missing_float32_array_no_default()
        type(parquet_reader) :: reader
        real(real32), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_f32_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing float32 array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_float32_array_no_default

    subroutine scenario_get_metadata_conversion_float32_array_no_default()
        type(parquet_reader) :: reader
        real(real32), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_f32_arr.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable float32 array metadata value with no default without error: ", &
            size(value)
    end subroutine scenario_get_metadata_conversion_float32_array_no_default

    subroutine scenario_get_metadata_missing_float64_array_no_default()
        type(parquet_reader) :: reader
        real(real64), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_f64_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing float64 array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_float64_array_no_default

    subroutine scenario_get_metadata_conversion_float64_array_no_default()
        type(parquet_reader) :: reader
        real(real64), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_f64_arr.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable float64 array metadata value with no default without error: ", &
            size(value)
    end subroutine scenario_get_metadata_conversion_float64_array_no_default

    subroutine scenario_get_metadata_missing_logical_array_no_default()
        type(parquet_reader) :: reader
        logical, allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_lg_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing logical array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_logical_array_no_default

    subroutine scenario_get_metadata_conversion_logical_array_no_default()
        type(parquet_reader) :: reader
        logical, allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_conv_lg_arr.parquet", reader)
        call parquet_get_metadata(reader, "not_a_number", value)
        print '(a,i0)', "unexpectedly read an unparsable logical array metadata value with no default without error: ", &
            size(value)
    end subroutine scenario_get_metadata_conversion_logical_array_no_default

    !> String arrays have no "conversion failure" abort path (every token is
    !> already a valid string, nothing to parse) -- only missing-key.
    subroutine scenario_get_metadata_missing_string_array_no_default()
        type(parquet_reader) :: reader
        character(len=:), allocatable :: value(:)

        call open_metadata_abort_reader("test_run/error_scenario_get_metadata_missing_str_arr.parquet", reader)
        call parquet_get_metadata(reader, "does_not_exist", value)
        print '(a,i0)', "unexpectedly read a missing string array metadata key with no default without error: ", size(value)
    end subroutine scenario_get_metadata_missing_string_array_no_default

    !> schema%add_field error stops if schema%init was never called first --
    !> there is no "fields:" header (or even a %maml) to append to yet.
    subroutine scenario_schema_add_field_before_init()
        type(parquet_schema) :: schema

        call schema%add_field("x", "int32")
        print '(a)', "unexpectedly added a field to a never-initialized schema without error"
    end subroutine scenario_schema_add_field_before_init

    !> schema%init error stops if called a second time on the same schema --
    !> otherwise a second call would silently discard any fields already
    !> added via the first init/add_field sequence.
    subroutine scenario_schema_init_twice()
        type(parquet_schema) :: schema

        call schema%init(table="t1")
        call schema%init(table="t2")
        print '(a)', "unexpectedly re-initialized an already-initialized schema without error"
    end subroutine scenario_schema_init_twice

    !> schema%init error stops on an empty table: value, the one top-level
    !> key parquet_validate_maml requires to be non-empty.
    subroutine scenario_schema_init_empty_table()
        type(parquet_schema) :: schema

        call schema%init(table="")
        print '(a)', "unexpectedly initialized a schema with an empty table without error"
    end subroutine scenario_schema_init_empty_table

    !> schema%add_field error stops on an empty field name.
    subroutine scenario_schema_add_field_empty_name()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("", "int32")
        print '(a)', "unexpectedly added a field with an empty name without error"
    end subroutine scenario_schema_add_field_empty_name

    !> schema%add_field error stops on a field name already declared earlier
    !> in the same schema.
    subroutine scenario_schema_add_field_duplicate_name()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64")
        call schema%add_field("ra", "int32")
        print '(a)', "unexpectedly added a duplicate field name without error"
    end subroutine scenario_schema_add_field_duplicate_name

    !> schema%add_field error stops on a data_type that isn't one of the
    !> supported types (int32/int64/string/boolean/float32/float64).
    subroutine scenario_schema_add_field_invalid_data_type()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "not_a_real_type")
        print '(a)', "unexpectedly added a field with an invalid data_type without error"
    end subroutine scenario_schema_add_field_invalid_data_type

    !> schema%add_field rejects a qc_min with a reversed ('<'/'<=') operator,
    !> the same rule %add_col_qc enforces for its own min: field.
    subroutine scenario_schema_add_field_qc_min_reversed_operator()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64", qc_min="<5")
        print '(a)', "unexpectedly accepted a reversed qc_min operator in schema%add_field"
    end subroutine scenario_schema_add_field_qc_min_reversed_operator

    !> schema%add_field rejects a qc_max with a reversed ('>'/'>=') operator,
    !> the same rule %add_col_qc enforces for its own max: field.
    subroutine scenario_schema_add_field_qc_max_reversed_operator()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64", qc_max=">360")
        print '(a)', "unexpectedly accepted a reversed qc_max operator in schema%add_field"
    end subroutine scenario_schema_add_field_qc_max_reversed_operator

    !> schema%add_field rejects a qc_min/qc_max that is only an operator with
    !> no value following it.
    subroutine scenario_schema_add_field_qc_operator_without_value()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64", qc_min=">")
        print '(a)', "unexpectedly accepted an operator with no value for qc_min in schema%add_field"
    end subroutine scenario_schema_add_field_qc_operator_without_value

    !> Same as scenario_schema_add_field_qc_operator_without_value, but for
    !> qc_max -- validate_qc_bound's two branches (is_min vs. not) are
    !> separate specific checks, so only testing qc_min leaves qc_max's own
    !> "bad qc_max value provided" error stop uncovered.
    subroutine scenario_schema_add_field_qc_max_operator_without_value()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64", qc_max="<")
        print '(a)', "unexpectedly accepted an operator with no value for qc_max in schema%add_field"
    end subroutine scenario_schema_add_field_qc_max_operator_without_value

    !> schema%add_field rejects a qc_miss value other than Null/NA/empty.
    subroutine scenario_schema_add_field_bad_qc_miss_value()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("ra", "float64", qc_miss="garbage")
        print '(a)', "unexpectedly accepted an invalid qc_miss value in schema%add_field"
    end subroutine scenario_schema_add_field_bad_qc_miss_value

    !> parquet_string_column: indexing out of range aborts (check_index).
    subroutine scenario_string_column_index_out_of_range()
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("a")
        call col%append_string("b")
        call col%get(5, s)   ! index 5 > nrows 2 -> aborts
        print '(a)', "unexpectedly read an out-of-range index: "//s
    end subroutine scenario_string_column_index_out_of_range

    !> parquet_string_column: get on a null element with no null option aborts (fail_null).
    subroutine scenario_string_column_get_null()
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("a")
        call col%append_null()
        call col%get(2, s)   ! null, no null_value/allow_null -> aborts
        print '(a)', "unexpectedly read a null element: "//s
    end subroutine scenario_string_column_get_null

    !> parquet_string_column: to_character on a null-containing column with no null_value aborts.
    subroutine scenario_string_column_to_character_null()
        type(parquet_string_column) :: col
        character(len=:), allocatable :: arr(:)
        call col%append_string("a")
        call col%append_null()
        call col%to_character(arr)   ! null present, no null_value -> aborts
        print '(a)', "unexpectedly materialized a null-containing column without null_value"
    end subroutine scenario_string_column_to_character_null

    !> parquet_string handle: using an unassociated handle aborts (check_handle).
    subroutine scenario_string_handle_unassociated()
        type(parquet_string) :: h
        integer(int64) :: n
        n = h%length()   ! handle never associated with a column -> aborts
        print '(a,i0)', "unexpectedly used an unassociated handle, length=", n
    end subroutine scenario_string_handle_unassociated

    !> parquet_string handle: a handle whose index no longer exists aborts (check_handle).
    subroutine scenario_string_handle_stale_index()
        type(parquet_string_column), target :: col
        type(parquet_string) :: h
        integer(int64) :: n
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("c")
        h = col%view(3)
        call col%erase(1)
        call col%erase(1)   ! nrows now 1; handle still refers to index 3
        n = h%length()      ! idx 3 > nrows 1 -> aborts
        print '(a,i0)', "unexpectedly used a stale handle, length=", n
    end subroutine scenario_string_handle_stale_index

end program error_scenarios
