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
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp, &
        parquet_unit_seconds, parquet_unit_millis, parquet_unit_nanos
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
    case ("print_stat_all_types")
        call scenario_print_stat_all_types()
    case ("print_stat_default_scalar_type")
        call scenario_print_stat_default_scalar_type()
    case ("print_stat_filtered_rows")
        call scenario_print_stat_filtered_rows()
    case ("large_string_roundtrip")
        call scenario_large_string_roundtrip()
    case ("string_view_roundtrip")
        call scenario_string_view_roundtrip()
    case ("string_view_compact_read_unsupported")
        call scenario_string_view_compact_read_unsupported()
    case ("col_size_overflow")
        call scenario_col_size_overflow()
    case ("col_size_and_row_mode_avoid_whole_column_read")
        call scenario_col_size_and_row_mode_avoid_whole_column_read()
    case ("whole_column_read_forced_error_control")
        call scenario_whole_column_read_forced_error_control()
    case ("read_array_full_bool_type_mismatch")
        call scenario_read_array_full_bool_type_mismatch()
    case ("read_array_full_string_type_mismatch")
        call scenario_read_array_full_string_type_mismatch()
    case ("read_array_row_mode_bool_type_mismatch")
        call scenario_read_array_row_mode_bool_type_mismatch()
    case ("read_array_row_mode_string_type_mismatch")
        call scenario_read_array_row_mode_string_type_mismatch()
    case ("read_array_element_mode_filtered_bool_col_index_out_of_range")
        call scenario_read_array_em_filt_bool_oob()
    case ("read_array_element_mode_filtered_bool_type_mismatch")
        call scenario_read_array_em_filt_bool_tm()
    case ("read_array_element_mode_filtered_string_col_index_out_of_range")
        call scenario_read_array_em_filt_string_oob()
    case ("read_array_element_mode_filtered_string_type_mismatch")
        call scenario_read_array_em_filt_string_tm()
    case ("read_array_element_mode_bool_col_index_out_of_range")
        call scenario_read_array_em_bool_oob()
    case ("read_array_element_mode_bool_type_mismatch")
        call scenario_read_array_em_bool_tm()
    case ("read_array_element_mode_string_col_index_out_of_range")
        call scenario_read_array_em_string_oob()
    case ("read_array_element_mode_string_type_mismatch")
        call scenario_read_array_em_string_tm()
    case ("read_array_column_chunk_bool_type_mismatch")
        call scenario_read_array_column_chunk_bool_type_mismatch()
    case ("read_array_column_chunk_string_type_mismatch")
        call scenario_read_array_column_chunk_string_type_mismatch()
    case ("read_array_element_mode_filtered_int32_col_index_out_of_range")
        call scenario_read_array_em_filt_int32_oob()
    case ("read_array_element_mode_int32_col_index_out_of_range")
        call scenario_read_array_em_int32_oob()
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
    case ("row_group_whole_column_after_streaming_started")
        call scenario_row_group_whole_column_after_streaming_started()
    case ("row_group_started_while_open")
        call scenario_row_group_started_while_open()
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
    case ("write_undeclared_column_string_compact")
        call scenario_write_undeclared_column_string_compact()
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
    case ("write_chunk_undeclared_column_string_compact")
        call scenario_write_chunk_undeclared_column_string_compact()
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
    case ("validate_excluded_decimal_type")
        call scenario_validate_bad_data_type_named("decimal")
    case ("validate_empty_field_name")
        call scenario_validate_empty_field_name()
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
    case ("filter_int32_value_out_of_range")
        call scenario_filter_int32_value_out_of_range()
    case ("filter_bad_numeric_value_float")
        call scenario_filter_bad_numeric_value_float()
    case ("filter_unquoted_string_value")
        call scenario_filter_unquoted_string_value()
    case ("filter_bad_boolean_value")
        call scenario_filter_bad_boolean_value()
    case ("filter_boolean_value_must_be_unquoted")
        call scenario_filter_boolean_value_must_be_unquoted()
    case ("filter_bool_ordering_not_supported")
        call scenario_filter_bool_ordering_not_supported()
    case ("filter_unsupported_column_type")
        call scenario_filter_unsupported_column_type()
    case ("sample_negative_fraction")
        call scenario_sample_negative_fraction()
    case ("sample_nan_fraction")
        call scenario_sample_nan_fraction()
    case ("read_chunk_with_sample")
        call scenario_read_chunk_with_sample()
    case ("print_stat_sampled_rows")
        call scenario_print_stat_sampled_rows()
    case ("sample_mask_build_error")
        call scenario_sample_mask_build_error()
    case ("string_length_on_non_string_column")
        call scenario_string_length_on_non_string_column()
    case ("qc_range_violation_warns")
        call scenario_qc_range_violation_warns()
    case ("extended_qc_range_violation_warns")
        call scenario_extended_qc_range_violation_warns()
    case ("qc_range_violation_int64_warns")
        call scenario_qc_range_violation_int64_warns()
    case ("qc_maml_stray_no_colon_line")
        call scenario_qc_maml_stray_no_colon_line()
    case ("qc_null_violation_warns")
        call scenario_qc_null_violation_warns()
    case ("qc_range_violation_hard_aborts")
        call scenario_qc_range_violation_hard_aborts()
    case ("qc_range_violation_string_warns")
        call scenario_qc_range_violation_string_warns()
    case ("qc_range_violation_string_hard_aborts")
        call scenario_qc_range_violation_string_hard_aborts()
    case ("qc_range_violation_float_warns")
        call scenario_qc_range_violation_float_warns()
    case ("qc_range_violation_float_hard_aborts")
        call scenario_qc_range_violation_float_hard_aborts()
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
    case ("read_nested_struct_field_not_found")
        call scenario_read_nested_struct_field_not_found()
    case ("read_nested_struct_path_not_a_struct")
        call scenario_read_nested_struct_path_not_a_struct()
    case ("read_nested_struct_intermediate_not_leaf")
        call scenario_read_nested_struct_intermediate_not_leaf()
    case ("nested_struct_shares_cached_read")
        call scenario_nested_struct_shares_cached_read()
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
    case ("validate_qc_max_not_numeric")
        call scenario_validate_qc_max_not_numeric()
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
    case ("set_col_qc_reversed_operator")
        call scenario_set_col_qc_reversed_operator()
    case ("qc_warning_numeric")
        call scenario_qc_warning_numeric()
    case ("qc_warning_fractional_bound")
        call scenario_qc_warning_fractional_bound()
    case ("qc_warning_string")
        call scenario_qc_warning_string()
    case ("qc_silently_ignored_for_boolean")
        call scenario_qc_silently_ignored_for_boolean()
    case ("qc_miss_default_active_numeric_warns")
        call scenario_qc_miss_default_active_numeric_warns()
    case ("qc_miss_declared_null_no_warning")
        call scenario_qc_miss_declared_null_no_warning()
    case ("qc_miss_string_warns")
        call scenario_qc_miss_string_warns()
    case ("qc_miss_temporal_warns")
        call scenario_qc_miss_temporal_warns()
    case ("add_field_qc_drives_reader_enforcement")
        call scenario_add_field_qc_drives_reader_enforcement()
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
    case ("schema_init_after_maml_parse")
        call scenario_schema_init_after_maml_parse()
    case ("parse_maml_file_after_init")
        call scenario_parse_maml_file_after_init()
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
    case ("get_field_by_name_not_found")
        call scenario_get_field_by_name_not_found()
    case ("get_field_by_index_out_of_range")
        call scenario_get_field_by_index_out_of_range()
    case ("add_field_from_source_not_found")
        call scenario_add_field_from_source_not_found()
    case ("print_schema_info_no_unit_no_filename")
        call scenario_print_schema_info_no_unit_no_filename()
    case ("print_schema_info_unit_not_open")
        call scenario_print_schema_info_unit_not_open()
    case ("print_schema_info_unit_read_only")
        call scenario_print_schema_info_unit_read_only()
    case ("print_schema_info_unit_filename_mismatch")
        call scenario_print_schema_info_unit_filename_mismatch()
    case ("print_schema_info_uninitialized_schema")
        call scenario_print_schema_info_uninitialized_schema()
    case ("print_schema_info_open_failure")
        call scenario_print_schema_info_open_failure()
    case ("schema_add_metadata_before_parse")
        call scenario_schema_add_metadata_before_parse()
    case ("string_column_index_out_of_range")
        call scenario_string_column_index_out_of_range()
    case ("string_column_view_all_size_mismatch")
        call scenario_string_column_view_all_size_mismatch()
    case ("string_column_get_null")
        call scenario_string_column_get_null()
    case ("string_column_to_character_null")
        call scenario_string_column_to_character_null()
    case ("string_handle_unassociated")
        call scenario_string_handle_unassociated()
    case ("string_handle_stale_index")
        call scenario_string_handle_stale_index()
    case ("string_set_null_unassociated")
        call scenario_string_set_null_unassociated()
    case ("string_set_null_stale_index")
        call scenario_string_set_null_stale_index()
    case ("string_slice_invalid_range")
        call scenario_string_slice_invalid_range()
    case ("string_view_slice_invalid_range")
        call scenario_string_view_slice_invalid_range()
    case ("string_view_slice_size_mismatch")
        call scenario_string_view_slice_size_mismatch()
    case ("string_build_from_self_alias")
        call scenario_string_build_from_self_alias()
    case ("string_build_from_unassociated")
        call scenario_string_build_from_unassociated()
    case ("string_build_from_stale_index")
        call scenario_string_build_from_stale_index()
    case ("string_column_append_buffers_offset_not_zero")
        call scenario_string_column_append_buffers_offset_not_zero()
    case ("string_column_append_buffers_offset_not_zero_int32")
        call scenario_string_column_append_buffers_offset_not_zero_int32()
    case ("compact_string_write_requires_scalar_column")
        call scenario_compact_string_write_requires_scalar_column()
    case ("compact_string_write_chunk_requires_scalar_column")
        call scenario_compact_string_write_chunk_requires_scalar_column()
    case ("extended_uint32_overflow_int32")
        call scenario_extended_uint32_overflow_int32()
    case ("extended_uint64_overflow_int32")
        call scenario_extended_uint64_overflow_int32()
    case ("extended_uint64_overflow_int64")
        call scenario_extended_uint64_overflow_int64()
    case ("extended_real_nonintegral_int32")
        call scenario_extended_real_nonintegral_int32()
    case ("extended_real_overflow_int32")
        call scenario_extended_real_overflow_int32()
    case ("extended_real_nonintegral_int64")
        call scenario_extended_real_nonintegral_int64()
    case ("extended_real_overflow_int64")
        call scenario_extended_real_overflow_int64()
    case ("extended_decimal_nonintegral_int32")
        call scenario_extended_decimal_nonintegral_int32()
    case ("extended_decimal_overflow_int32")
        call scenario_extended_decimal_overflow_int32()
    case ("extended_decimal_nonintegral_int64")
        call scenario_extended_decimal_nonintegral_int64()
    case ("extended_decimal_overflow_int64")
        call scenario_extended_decimal_overflow_int64()
    case ("temporal_date_set_invalid_month")
        call scenario_temporal_date_set_invalid_month()
    case ("temporal_date_set_invalid_day")
        call scenario_temporal_date_set_invalid_day()
    case ("temporal_date_set_out_of_range")
        call scenario_temporal_date_set_out_of_range()
    case ("temporal_date_set_mjd_out_of_range")
        call scenario_temporal_date_set_mjd_out_of_range()
    case ("temporal_date_parse_invalid")
        call scenario_temporal_date_parse_invalid()
    case ("temporal_date_null_get")
        call scenario_temporal_date_null_get()
    case ("temporal_date_null_comparison")
        call scenario_temporal_date_null_comparison()
    case ("temporal_time_set_invalid_hour")
        call scenario_temporal_time_set_invalid_hour()
    case ("temporal_time_set_invalid_nanosecond")
        call scenario_temporal_time_set_invalid_nanosecond()
    case ("temporal_time_set_raw_out_of_range")
        call scenario_temporal_time_set_raw_out_of_range()
    case ("temporal_time_parse_invalid")
        call scenario_temporal_time_parse_invalid()
    case ("temporal_time_null_get")
        call scenario_temporal_time_null_get()
    case ("temporal_ts_set_invalid_day")
        call scenario_temporal_ts_set_invalid_day()
    case ("temporal_ts_parse_invalid")
        call scenario_temporal_ts_parse_invalid()
    case ("temporal_ts_null_to_unix")
        call scenario_temporal_ts_null_to_unix()
    case ("temporal_ts_null_comparison")
        call scenario_temporal_ts_null_comparison()
    case ("temporal_ts_to_unix_precision_loss")
        call scenario_temporal_ts_to_unix_precision_loss()
    case ("temporal_ts_to_unix_overflow")
        call scenario_temporal_ts_to_unix_overflow()
    case ("temporal_ts_set_mjd_out_of_range")
        call scenario_temporal_ts_set_mjd_out_of_range()
    case ("temporal_ts_set_raw_invalid_nanoseconds")
        call scenario_temporal_ts_set_raw_invalid_nanoseconds()
    case ("temporal_invalid_time_unit")
        call scenario_temporal_invalid_time_unit()
    case ("temporal_ts_get_year_overflow")
        call scenario_temporal_ts_get_year_overflow()
    case ("temporal_write_time_precision_loss")
        call scenario_temporal_write_time_precision_loss()
    case ("temporal_protected_col_null")
        call scenario_temporal_protected_col_null()
    case ("temporal_read_date_via_int32")
        call scenario_temporal_read_date_via_int32()
    case ("temporal_read_int32_via_date")
        call scenario_temporal_read_int32_via_date()
    case ("temporal_foreign_int96_roundtrip")
        call scenario_temporal_foreign_int96_roundtrip()
    case ("temporal_foreign_tz_roundtrip")
        call scenario_temporal_foreign_tz_roundtrip()
    case ("list_type_foreign_fixture")
        call scenario_list_type_foreign_fixture()
    case ("schema_add_field_date_with_unit")
        call scenario_schema_add_field_date_with_unit()
    case ("validate_qc_on_temporal_column")
        call scenario_validate_qc_on_temporal_column()
    case ("schema_add_field_seconds_unit_rejected")
        call scenario_schema_add_field_seconds_unit_rejected()
    case ("schema_add_field_time_utc_rejected")
        call scenario_schema_add_field_time_utc_rejected()
    case ("schema_add_field_unclosed_bracket_rejected")
        call scenario_schema_add_field_unclosed_bracket_rejected()
    case ("temporal_date_eq_null")
        call scenario_temporal_date_eq_null()
    case ("temporal_time_get_null")
        call scenario_temporal_time_get_null()
    case ("temporal_time_minute_null")
        call scenario_temporal_time_minute_null()
    case ("temporal_time_second_null")
        call scenario_temporal_time_second_null()
    case ("temporal_time_nanosecond_null")
        call scenario_temporal_time_nanosecond_null()
    case ("temporal_time_eq_null")
        call scenario_temporal_time_eq_null()
    case ("temporal_time_lt_null")
        call scenario_temporal_time_lt_null()
    case ("temporal_date_year_null")
        call scenario_temporal_date_year_null()
    case ("temporal_date_month_null")
        call scenario_temporal_date_month_null()
    case ("temporal_date_day_null")
        call scenario_temporal_date_day_null()
    case ("temporal_date_to_mjd_null")
        call scenario_temporal_date_to_mjd_null()
    case ("temporal_date_to_string_null")
        call scenario_temporal_date_to_string_null()
    case ("temporal_time_to_string_null")
        call scenario_temporal_time_to_string_null()
    case ("temporal_ts_set_invalid_month")
        call scenario_temporal_ts_set_invalid_month()
    case ("temporal_ts_set_invalid_nanosecond")
        call scenario_temporal_ts_set_invalid_nanosecond()
    case ("temporal_ts_get_null")
        call scenario_temporal_ts_get_null()
    case ("temporal_ts_get_date_range_exceeded")
        call scenario_temporal_ts_get_date_range_exceeded()
    case ("temporal_ts_to_unix_overflow_negative")
        call scenario_temporal_ts_to_unix_overflow_negative()
    case ("temporal_ts_to_mjd_null")
        call scenario_temporal_ts_to_mjd_null()
    case ("temporal_ts_to_string_null")
        call scenario_temporal_ts_to_string_null()
    case ("temporal_ts_lt_null")
        call scenario_temporal_ts_lt_null()
    case ("temporal_date_diff_null")
        call scenario_temporal_date_diff_null()
    case ("temporal_date_offset_null")
        call scenario_temporal_date_offset_null()
    case ("temporal_date_offset_out_of_range")
        call scenario_temporal_date_offset_out_of_range()
    case ("temporal_date_offset_int64_overflow_positive")
        call scenario_temporal_date_offset_int64_overflow_positive()
    case ("temporal_date_offset_int64_overflow_negative")
        call scenario_temporal_date_offset_int64_overflow_negative()
    case ("temporal_date_sub_int64_min")
        call scenario_temporal_date_sub_int64_min()
    case ("temporal_time_diff_null")
        call scenario_temporal_time_diff_null()
    case ("temporal_time_offset_null")
        call scenario_temporal_time_offset_null()
    case ("temporal_time_offset_magnitude_add")
        call scenario_temporal_time_offset_magnitude_add()
    case ("temporal_time_offset_magnitude_sub")
        call scenario_temporal_time_offset_magnitude_sub()
    case ("temporal_ts_diff_ns_null")
        call scenario_temporal_ts_diff_ns_null()
    case ("temporal_ts_diff_ns_overflow")
        call scenario_temporal_ts_diff_ns_overflow()
    case ("temporal_ts_diff_seconds_null")
        call scenario_temporal_ts_diff_seconds_null()
    case ("temporal_ts_offset_null")
        call scenario_temporal_ts_offset_null()
    case ("temporal_ts_offset_ns_overflow")
        call scenario_temporal_ts_offset_ns_overflow()
    case ("temporal_ts_offset_seconds_overflow_positive")
        call scenario_temporal_ts_offset_seconds_overflow_positive()
    case ("temporal_ts_offset_seconds_overflow_negative")
        call scenario_temporal_ts_offset_seconds_overflow_negative()
    case ("temporal_ts_sub_int64_min")
        call scenario_temporal_ts_sub_int64_min()
    case ("temporal_write_column_not_defined")
        call scenario_temporal_write_column_not_defined()
    case ("temporal_write_array_size_mismatch")
        call scenario_temporal_write_array_size_mismatch()
    case ("temporal_write_type_mismatch")
        call scenario_temporal_write_type_mismatch()
    case ("temporal_chunk_column_not_defined")
        call scenario_temporal_chunk_column_not_defined()
    case ("temporal_chunk_array_size_mismatch")
        call scenario_temporal_chunk_array_size_mismatch()
    case ("temporal_chunk_type_mismatch")
        call scenario_temporal_chunk_type_mismatch()
    case ("mask_row_mask_after_write_started")
        call scenario_mask_row_mask_after_write_started()
    case ("mask_row_mask_shape_mismatch")
        call scenario_mask_row_mask_shape_mismatch()
    case ("mask_row_mask_zero_length")
        call scenario_mask_row_mask_zero_length()
    case ("mask_row_mask_called_twice")
        call scenario_mask_row_mask_called_twice()
    case ("mask_chunk_row_mask_after_row_mask")
        call scenario_mask_chunk_row_mask_after_row_mask()
    case ("mask_row_mask_after_chunk_row_mask")
        call scenario_mask_row_mask_after_chunk_row_mask()
    case ("mask_chunk_row_mask_after_whole_column")
        call scenario_mask_chunk_row_mask_after_whole_column()
    case ("mask_chunk_row_mask_not_used_every_group")
        call scenario_mask_chunk_row_mask_not_used_every_group()
    case ("mask_chunk_row_mask_introduced_late")
        call scenario_mask_chunk_row_mask_introduced_late()
    case ("mask_chunk_row_mask_called_twice")
        call scenario_mask_chunk_row_mask_called_twice()
    case ("mask_chunk_row_mask_size_mismatch")
        call scenario_mask_chunk_row_mask_size_mismatch()
    case ("mask_row_mask_window_exhausted")
        call scenario_mask_row_mask_window_exhausted()
    case ("mask_row_mask_not_fully_consumed")
        call scenario_mask_row_mask_not_fully_consumed()
    case ("mask_chunk_row_mask_no_row_group_open")
        call scenario_mask_chunk_row_mask_no_row_group_open()
    case ("mask_chunk_row_mask_scheme_declined")
        call scenario_mask_chunk_row_mask_scheme_declined()
    case ("mask_row_group_no_writes_at_all")
        call scenario_mask_row_group_no_writes_at_all()
    case ("get_version_invalid_mode")
        call scenario_get_version_invalid_mode()
    case ("column_exists_bad_type_token")
        call scenario_column_exists_bad_type_token()
    case ("column_exists_empty_type_filter")
        call scenario_column_exists_empty_type_filter()
    case ("get_column_type_unsupported")
        call scenario_get_column_type_unsupported()
    case ("col_size_malformed_value")
        call scenario_col_size_malformed_value()
    case ("array_size_malformed_value")
        call scenario_array_size_malformed_value()
    case ("array_size_auto_on_non_string")
        call scenario_array_size_auto_on_non_string()
    case ("set_col_size_non_positive")
        call scenario_set_col_size_non_positive()
    case ("set_col_size_already_resolved_no_force")
        call scenario_set_col_size_already_resolved_no_force()
    case ("set_array_size_non_string_column")
        call scenario_set_array_size_non_string_column()
    case ("set_array_size_non_positive")
        call scenario_set_array_size_non_positive()
    case ("set_array_size_already_resolved_no_force")
        call scenario_set_array_size_already_resolved_no_force()
    case ("flat_write_col_size_still_auto")
        call scenario_flat_write_col_size_still_auto()
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

    !> Same as scenario_write_undeclared_column_string, but via a compact (parquet_string_column)
    !! write -- a distinct source line/subroutine (parquet_write_string_column_compact), so needs
    !! its own scenario for coverage.
    subroutine scenario_write_undeclared_column_string_compact()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: col

        call col%append_string("aa")
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_string_compact.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", col)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_string_compact

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

    !> Same as scenario_write_chunk_undeclared_column_string, but via a compact
    !! (parquet_string_column) chunk write -- a distinct source line/subroutine
    !! (parquet_write_string_column_chunk_compact), so needs its own scenario for coverage.
    subroutine scenario_write_chunk_undeclared_column_string_compact()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: col

        call col%append_string("aa")
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, &
            "test_run/error_scenario_chunk_undeclared_string_compact.parquet", schema)
        call parquet_write_column_chunk(writer, "not_a_real_column", col)
        call parquet_close_writer(writer)
    end subroutine scenario_write_chunk_undeclared_column_string_compact

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

    !> Locks in the "decimal" type exclusion documented in the README's Limitations section
    !> (date/timestamp were also excluded here once, but are now supported -- see
    !> src/parquet_temporal.f90 -- so those two cases were removed from this scenario): unlike
    !> scenario_validate_bad_data_type's generic garbage token, this uses the real excluded type
    !> name, so a future accidental addition of "decimal" to valid_maml_data_types would be
    !> caught here.
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

    !> A fields: entry with no name: sub-key at all leaves cinfo%col(i)%name
    !> at its default "" -- parquet_validate_maml_internal's own empty-name
    !> check (parquet_metadata_validate.f90) must catch this before it ever
    !> reaches type_ok/duplicate-name checks that assume a real name.
    subroutine scenario_validate_empty_field_name()
        type(parquet_maml_file) :: maml

        maml%name = "empty_field_name.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_empty_field_name

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

    !> The 11 scenarios below each exercise exactly one report_fatal_error
    !> call site added to convert_values_to_int32/int64 (parquet_wrapper.cpp)
    !> for the extended read-time source types (INT8/16, UINT8/16/32/64,
    !> HALF_FLOAT, DECIMAL32/64/128/256 -- see CONTRIBUTING.md's "Additional
    !> scalar types" note and doc/pages/supported-data-types.md). Each reads
    !> one column of test/fixtures/extended_types.parquet (see its own
    !> generation comment in tools/generate_fixtures.cpp) whose row 3 was
    !> deliberately built to trigger exactly one of: an unsigned/real/decimal
    !> source value overflowing the requested int32/int64 target width, or a
    !> real/decimal source value with a nonzero fractional part (which this
    !> feature always rejects with a hard error rather than truncating).

    !> UINT32 value 4294967295 (> int32 max) read into an int32 array.
    subroutine scenario_extended_uint32_overflow_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_uint32_ovf", values)
        print '(a)', "unexpectedly read a uint32 value exceeding int32 range without error"
    end subroutine scenario_extended_uint32_overflow_int32

    !> UINT64 value 5000000000 (fits int64, not int32) read into an int32 array.
    subroutine scenario_extended_uint64_overflow_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_uint64_ovf32", values)
        print '(a)', "unexpectedly read a uint64 value exceeding int32 range without error"
    end subroutine scenario_extended_uint64_overflow_int32

    !> UINT64 value 18446744073709551615 (UINT64_MAX, > int64 max) read into an int64 array.
    subroutine scenario_extended_uint64_overflow_int64()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_uint64_ovf64", values)
        print '(a)', "unexpectedly read a uint64 value exceeding int64 range without error"
    end subroutine scenario_extended_uint64_overflow_int64

    !> DOUBLE value 3.14 (nonzero fractional part) read into an int32 array.
    subroutine scenario_extended_real_nonintegral_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_double_fractional", values)
        print '(a)', "unexpectedly read a non-integral double value into int32 without error"
    end subroutine scenario_extended_real_nonintegral_int32

    !> DOUBLE value 5.0e9 (fits int64, not int32) read into an int32 array.
    subroutine scenario_extended_real_overflow_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_double_ovf32", values)
        print '(a)', "unexpectedly read a double value exceeding int32 range without error"
    end subroutine scenario_extended_real_overflow_int32

    !> DOUBLE value 3.14 (nonzero fractional part) read into an int64 array.
    subroutine scenario_extended_real_nonintegral_int64()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_double_fractional", values)
        print '(a)', "unexpectedly read a non-integral double value into int64 without error"
    end subroutine scenario_extended_real_nonintegral_int64

    !> DOUBLE value 1.0e20 (> int64 max) read into an int64 array.
    subroutine scenario_extended_real_overflow_int64()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_double_ovf64", values)
        print '(a)', "unexpectedly read a double value exceeding int64 range without error"
    end subroutine scenario_extended_real_overflow_int64

    !> DECIMAL128(10, 2) value 123.45 (nonzero fractional part) read into an int32 array.
    subroutine scenario_extended_decimal_nonintegral_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_decimal_scaled", values)
        print '(a)', "unexpectedly read a non-integral decimal value into int32 without error"
    end subroutine scenario_extended_decimal_nonintegral_int32

    !> DECIMAL128(20, 0) value 5000000000 (fits int64, not int32) read into an int32 array.
    subroutine scenario_extended_decimal_overflow_int32()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_decimal_ovf32", values)
        print '(a)', "unexpectedly read a decimal value exceeding int32 range without error"
    end subroutine scenario_extended_decimal_overflow_int32

    !> DECIMAL128(10, 2) value 123.45 (nonzero fractional part) read into an int64 array.
    subroutine scenario_extended_decimal_nonintegral_int64()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_decimal_scaled", values)
        print '(a)', "unexpectedly read a non-integral decimal value into int64 without error"
    end subroutine scenario_extended_decimal_nonintegral_int64

    !> DECIMAL128(30, 0) value 1e20 (> int64 max) read into an int64 array.
    subroutine scenario_extended_decimal_overflow_int64()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_decimal_ovf64", values)
        print '(a)', "unexpectedly read a decimal value exceeding int64 range without error"
    end subroutine scenario_extended_decimal_overflow_int64

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

    !> A dotted struct-field path whose top-level segment exists but an intermediate/leaf field
    !> name does not (typo: "inr" instead of "inner") is rejected the same way as any other
    !> unknown column -- check_column_exists/parquet_reader_has_column (backed by
    !> struct_path_exists in parquet_wrapper.cpp) catches this before any read is attempted, same
    !> clean error_stop message class as scenario_read_unknown_column, not a crash.
    subroutine scenario_read_nested_struct_field_not_found()
        type(parquet_reader) :: reader
        integer(int32) :: age(5)

        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call parquet_read_column(reader, "main.inr.age", age)
        print '(a)', "unexpectedly read a nonexistent nested struct field without error"
    end subroutine scenario_read_nested_struct_field_not_found

    !> A dotted path where a middle segment resolves to a scalar leaf rather than continuing to
    !> nest (e.g. "main.id.extra" -- "id" is int32, not a struct) is rejected as "not found", the
    !> same class of error as a plain typo -- not a crash from treating a non-struct array as a
    !> StructArray (struct_path_exists's schema-level walk in parquet_wrapper.cpp requires every
    !> non-terminal path segment to be a STRUCT field).
    subroutine scenario_read_nested_struct_path_not_a_struct()
        type(parquet_reader) :: reader
        integer(int32) :: extra(5)

        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call parquet_read_column(reader, "main.id.extra", extra)
        print '(a)', "unexpectedly read a struct path through a non-struct segment without error"
    end subroutine scenario_read_nested_struct_path_not_a_struct

    !> A dotted path that resolves exactly to an intermediate STRUCT (not a leaf) is rejected --
    !> reading "main.inner" directly is not supported (this library has no struct/record output
    !> type); the caller must name a path all the way down to a scalar/vector leaf column (see
    !> struct_path_exists's terminal-type gate in parquet_wrapper.cpp, and CLAUDE.md's
    !> nested-struct-field design notes).
    subroutine scenario_read_nested_struct_intermediate_not_leaf()
        type(parquet_reader) :: reader
        integer(int32) :: bogus(5)

        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call parquet_read_column(reader, "main.inner", bogus)
        print '(a)', "unexpectedly read an intermediate struct column without error"
    end subroutine scenario_read_nested_struct_intermediate_not_leaf

    !> Regression check for the nested-struct-field design: reading two different leaf paths
    !> under the same physical top-level struct column ("main.id" then "main.inner.age") must
    !> only trigger ONE genuine disk read of "main" -- proving struct-path resolution shares
    !> get_single_chunk_array's existing column_cache instead of re-reading per leaf path (see
    !> CLAUDE.md's nested-struct-field design notes, and
    !> parquet_debug_get_physical_column_read_count's own comment in parquet_wrapper.cpp).
    !> error stops (a hard scenario failure, not just a soft print) if the count is anything other
    !> than 1 -- this scenario is expected to exit cleanly (expect_abort=0 in
    !> tools/run_error_scenarios.sh), so reaching the error stop is itself the failure signal.
    subroutine scenario_nested_struct_shares_cached_read()
        interface
            function parquet_debug_get_physical_column_read_count() result(n) &
                bind(C, name="parquet_debug_get_physical_column_read_count")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: n
            end function parquet_debug_get_physical_column_read_count

            subroutine parquet_debug_reset_physical_column_read_count() &
                bind(C, name="parquet_debug_reset_physical_column_read_count")
            end subroutine parquet_debug_reset_physical_column_read_count
        end interface

        type(parquet_reader) :: reader
        integer(int32) :: id(5), age(5)
        integer(int64) :: count_after
        character(len=32) :: count_str

        call parquet_debug_reset_physical_column_read_count()
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet")
        call parquet_read_column(reader, "main.id", id, null_value=-1_int32)
        call parquet_read_column(reader, "main.inner.age", age, null_value=-1_int32)
        call parquet_close_reader(reader)

        count_after = parquet_debug_get_physical_column_read_count()
        if (count_after /= 1) then
            write(count_str, '(i0)') count_after
            error stop "scenario_nested_struct_shares_cached_read: expected exactly 1 physical read of 'main', got " // &
                trim(count_str)
        end if
    end subroutine scenario_nested_struct_shares_cached_read

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

    !> scenario_print_stat_smoke above only ever runs three
    !> integer(int32) columns through parquet_close_reader(print_stat=.true.), so
    !> format_stat_scalar's INT64/FLOAT/DOUBLE/STRING cases, the boolean True/False count
    !> display, the qc bound display column, and the active-filter display column had never
    !> fired. This scenario adds one column of each of those kinds, all read (not just
    !> prefetched, so run_qc_checks/mark_read populate every display column
    !> parquet_reader_print_stat looks at), plus a qc rule (declared but not violated -- a
    !> violation is exercised elsewhere, e.g. scenario_qc_range_violation_warns; here the point
    !> is just to populate the qcmin/qcmax display columns) and an active row filter, so every
    !> row (format_stat_scalar's INT64/FLOAT/DOUBLE/STRING branches, the BOOL True/False count
    !> branch, the qc-bound display block, and the filter-clause display block) in
    !> parquet_reader_print_stat's per-column table fires at least once.
    subroutine scenario_print_stat_all_types()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: qc_col(3), qc_back(3)
        integer(int64) :: i64_values(3), i64_back(3)
        real(real32) :: f32_values(3), f32_back(3)
        real(real64) :: f64_values(3), f64_back(3)
        character(len=8) :: s_values(3), s_back(3)
        logical :: flag_values(3), flag_back(3)
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_all_types.parquet"
        character(len=*), parameter :: maml_file = "test_run/error_scenario_print_stat_all_types.maml"

        qc_col = [1, 2, 3] ! within the [0, 100] qc bound declared below -- no violation intended
        i64_values = [10_int64, 20_int64, 30_int64]
        f32_values = [1.5_real32, 2.5_real32, 3.5_real32]
        f64_values = [1.25_real64, 2.25_real64, 3.25_real64]
        s_values = [character(len=8) :: "alpha", "beta", "gamma"]
        flag_values = [.true., .false., .true.]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "qc_col", qc_col)
        call parquet_write_column(writer, "i64", i64_values)
        call parquet_write_column(writer, "f32", f32_values)
        call parquet_write_column(writer, "f64", f64_values)
        call parquet_write_column(writer, "s", s_values)
        call parquet_write_column(writer, "flag", flag_values)
        call parquet_close_writer(writer)

        call write_text_file(maml_file, [character(len=32) :: &
            "fields:", "- name: qc_col", "  qc:", "    min: 0", "    max: 100"])

        call filt%add("i64 >= 0")
        call parquet_open_reader(reader, out_file, &
            schema=parquet_load_qc_maml_file(maml_file), filter=filt)
        call parquet_read_column(reader, "qc_col", qc_back)
        call parquet_read_column(reader, "i64", i64_back)
        call parquet_read_column(reader, "f32", f32_back)
        call parquet_read_column(reader, "f64", f64_back)
        call parquet_read_column(reader, "s", s_back)
        call parquet_read_column(reader, "flag", flag_back)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered int64/float32/float64/string/boolean/qc/filter columns"
    end subroutine scenario_print_stat_all_types

    !> format_stat_scalar's `default: return
    !> s->ToString();` branch (parquet_wrapper.cpp) fires for any scalar min/max type it doesn't
    !> special-case -- e.g. UINT64 (this library's own writer never produces one, but the read
    !> side widens it -- see CONTRIBUTING.md's "Additional scalar types" note). Reads
    !> test/fixtures/extended_types.parquet's v_uint64 column (values 1000, 0, 2000000000, all
    !> safely within int64 -- see that fixture's own header comment) so
    !> parquet_reader_print_stat's compute_stat_min_max sees the column's native UInt64Array
    !> type (the Fortran-side int64 widening doesn't change what's cached/reported).
    subroutine scenario_print_stat_default_scalar_type()
        type(parquet_reader) :: reader
        integer(int64) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_read_column(reader, "v_uint64", values)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered format_stat_scalar's default (UINT64) branch"
    end subroutine scenario_print_stat_default_scalar_type

    !> Every other print_stat_* scenario above uses a filter
    !> (scenario_print_stat_all_types) that keeps every row, so parquet_reader_print_stat's
    !> "rows: N (of M total)" branch (taken when the active filter actually excludes at least
    !> one row) had never fired -- only its "rows: N" (no filter, or a no-op filter) sibling had.
    !> Writes 5 rows, filters down to 3, and closes with print_stat=.true. so nrows (3) !=
    !> total_nrows (5).
    subroutine scenario_print_stat_filtered_rows()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: a_values(5), a_back(3)
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_filtered_rows.parquet"

        a_values = [(i, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_close_writer(writer)

        call filt%add("a >= 3")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_column(reader, "a", a_back)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the filtered 'rows: N (of M total)' summary branch"
    end subroutine scenario_print_stat_filtered_rows

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
        character(len=:), allocatable :: type_name

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

        ! parquet_column_exists/parquet_get_column_type report "s" (a LARGE_STRING column) the
        ! same as an ordinary STRING column -- this library's own MAML vocabulary doesn't
        ! distinguish string/large_string (see supported-data-types.md's "Large string columns"),
        ! and this is the only place LARGE_STRING's own case in
        ! parquet_reader_get_column_type_name's switch (parquet_wrapper.cpp) gets exercised.
        call parquet_open_reader(reader, out_file)
        if (.not. parquet_column_exists(reader, "s", types="string")) then
            error stop "parquet_column_exists did not match 'string' for a large_utf8 scalar string column"
        end if
        call parquet_get_column_type(reader, "s", type_name)
        if (trim(type_name) /= "string") then
            error stop "parquet_get_column_type did not resolve 'string' for a large_utf8 scalar string column"
        end if
        call parquet_close_reader(reader)
    end subroutine scenario_large_string_roundtrip

    !> Proves the arrow::Type::STRING_VIEW read path (is_string_like_type/make_string_like_accessor's
    !> STRING_VIEW branches, added alongside STRING/LARGE_STRING in parquet_wrapper.cpp) round-trips
    !> correctly. Unlike LARGE_STRING (reachable by shrinking a real threshold via
    !> parquet_debug_set_string_offset_limit -- see scenario_large_string_roundtrip, above), this
    !> library's own writer never produces STRING_VIEW at all: it can only arrive from a file written
    !> by another Arrow-based tool whose stored Arrow schema declared the column as utf8_view() (see
    !> is_string_like_type's own comment). So this scenario instead calls
    !> parquet_debug_write_string_view_fixture (a test-only hook declared locally below, not part of
    !> the public Fortran API -- see its own comment in parquet_wrapper.cpp) to build such a file
    !> directly with Arrow's own StringViewBuilder, bypassing this library's writer entirely.
    subroutine scenario_string_view_roundtrip()
        interface
            subroutine parquet_debug_write_string_view_fixture(path, column_name) &
                bind(C, name="parquet_debug_write_string_view_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char), intent(in) :: path(*) !! null-terminated output file path.
                character(kind=c_char), intent(in) :: column_name(*) !! null-terminated STRING_VIEW column name.
            end subroutine parquet_debug_write_string_view_fixture
        end interface

        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_string_view.parquet"
        character(len=45) :: s_back(5)
        character(len=45), parameter :: s_expect(5) = [character(len=45) :: &
            "short", "", "", "this value exceeds twelve bytes for sure", "exactly12chr"]
        logical :: is_valid(5)
        integer :: strlen_max
        integer(int64) :: nrows

        ! Five fixed rows deliberately cover StringView's inlined-vs-out-of-line boundary -- see
        ! parquet_debug_write_string_view_fixture's own comment in parquet_wrapper.cpp: a short
        ! inlined value, an empty inlined value, a Null, a long out-of-line value, and a value
        ! exactly at the 12-byte inline boundary.
        call parquet_debug_write_string_view_fixture( &
            "test_run/error_scenario_string_view.parquet"//char(0), "sv"//char(0))

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "sv", s_back, is_valid=is_valid)
        call parquet_get_string_length(reader, "sv", strlen_max)
        call parquet_close_reader(reader, print_stat=.true.)

        if (.not. all(s_back == s_expect)) then
            error stop "STRING_VIEW column did not round-trip correctly through parquet_read_column"
        end if
        if (.not. (is_valid(1) .and. is_valid(2) .and. .not. is_valid(3) &
                .and. is_valid(4) .and. is_valid(5))) then
            error stop "STRING_VIEW column's Null (row 3) was not reported correctly via is_valid"
        end if
        if (strlen_max /= 40) then
            error stop "parquet_get_string_length was wrong for a STRING_VIEW scalar string column"
        end if

        ! Row-filtering (eval_filter_clause) on a STRING_VIEW scalar string column.
        call filt%add('sv == "short"')
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        if (nrows /= 1_int64) then
            error stop "row filter on a STRING_VIEW scalar string column did not match exactly one row"
        end if
    end subroutine scenario_string_view_roundtrip

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

    !> The 16 scenarios below cover array-mode (vector-column) read strict typing and
    !> col_index bounds checking across all four read access patterns (whole-column,
    !> row-mode, element-mode, chunk-mode), plus two extra element-mode col_index-bounds
    !> scenarios (numeric, not just logical/string). Every fixture below writes a plain int32 vector column
    !> (col_size=3, nrows=2) and then reads it back through a mismatched Fortran type (logical
    !> or character) or an out-of-range col_index/elem_index, mirroring the existing scalar
    !> strict-typing scenarios (e.g. scenario_temporal_read_date_via_int32, above) for the
    !> vector-column read entry points.

    !> parquet_read_column dispatched to the logical (bool8) array-full specific
    !> (parquet_read_logical_array_full -> parquet_read_bool8_array_column) on a column that is
    !> actually int32-typed aborts with a type mismatch.
    subroutine scenario_read_array_full_bool_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        logical :: bool_back(3, 2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_full_bool_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "vec", bool_back)   ! -> aborts (type mismatch: expected bool, got int32)
        print '(a)', "unexpectedly read an int32 vector column as a logical array"
    end subroutine scenario_read_array_full_bool_type_mismatch

    !> Converse of scenario_read_array_full_bool_type_mismatch: the string array-full specific
    !> (parquet_read_string_array_full -> parquet_read_string_array_column) on an int32 column.
    subroutine scenario_read_array_full_string_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        character(len=8) :: string_back(3, 2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_full_string_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "vec", string_back)   ! -> aborts (type mismatch: expected string, got int32)
        print '(a)', "unexpectedly read an int32 vector column as a string array"
    end subroutine scenario_read_array_full_string_type_mismatch

    !> parquet_read_array_row_mode dispatched to logical (parquet_read_bool8_array_row) on an
    !> int32 vector column aborts with a type mismatch.
    subroutine scenario_read_array_row_mode_bool_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        logical :: row_back(3)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_row_mode_bool_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_row_mode(reader, "vec", row_back, 1)   ! -> aborts (type mismatch: expected bool, got int32)
        print '(a)', "unexpectedly read one row of an int32 vector column as logical"
    end subroutine scenario_read_array_row_mode_bool_type_mismatch

    !> Converse of scenario_read_array_row_mode_bool_type_mismatch: parquet_read_string_array_row.
    subroutine scenario_read_array_row_mode_string_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        character(len=8) :: row_back(3)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_row_mode_string_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_row_mode(reader, "vec", row_back, 1)   ! -> aborts (type mismatch: expected string, got int32)
        print '(a)', "unexpectedly read one row of an int32 vector column as string"
    end subroutine scenario_read_array_row_mode_string_type_mismatch

    !> parquet_read_array_element_mode's logical specific, on a reader opened with an active
    !> filter (so parquet_read_bool8_array_element takes its filtered/whole-column branch),
    !> with an elem_index (col_index) past col_size aborts with "col_index out of bounds"
    !> before any type check runs.
    subroutine scenario_read_array_em_filt_bool_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(2), data(3, 2)
        logical :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_filtered_bool_col_index_out_of_range.parquet"

        id = [1, 2]
        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call filt%add("id >= 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on a filtered logical reader"
    end subroutine scenario_read_array_em_filt_bool_oob

    !> Same filtered branch as above, but with a valid col_index -- reaches the type-mismatch
    !> check instead (the int32 column read via the logical specific).
    subroutine scenario_read_array_em_filt_bool_tm()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(2), data(3, 2)
        logical :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_filtered_bool_type_mismatch.parquet"

        id = [1, 2]
        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call filt%add("id >= 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 2)   ! -> aborts (type mismatch: expected bool, got int32)
        print '(a)', "unexpectedly read element_mode of an int32 vector column as logical on a filtered reader"
    end subroutine scenario_read_array_em_filt_bool_tm

    !> Converse of scenario_read_array_element_mode_filtered_bool_col_index_out_of_range: the
    !> string specific (parquet_read_string_array_element), filtered, out-of-range col_index.
    subroutine scenario_read_array_em_filt_string_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(2), data(3, 2)
        character(len=8) :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_filtered_string_col_index_out_of_range.parquet"

        id = [1, 2]
        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call filt%add("id >= 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on a filtered string reader"
    end subroutine scenario_read_array_em_filt_string_oob

    !> Same filtered branch as above, but with a valid col_index -- reaches the type-mismatch
    !> check instead (the int32 column read via the string specific).
    subroutine scenario_read_array_em_filt_string_tm()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(2), data(3, 2)
        character(len=8) :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_filtered_string_type_mismatch.parquet"

        id = [1, 2]
        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call filt%add("id >= 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 2)   ! -> aborts (type mismatch: expected string, got int32)
        print '(a)', "unexpectedly read element_mode of an int32 vector column as string on a filtered reader"
    end subroutine scenario_read_array_em_filt_string_tm

    !> parquet_read_array_element_mode's logical specific, unfiltered (so
    !> parquet_read_bool8_array_element takes its row-group-streamed branch via
    !> resolve_element_mode_col_size/stream_element_mode_row_groups), with an out-of-range
    !> col_index aborts before any row group is ever streamed.
    subroutine scenario_read_array_em_bool_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        logical :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_bool_col_index_out_of_range.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on an unfiltered logical reader"
    end subroutine scenario_read_array_em_bool_oob

    !> Same unfiltered/streamed branch as above, but with a valid col_index -- reaches the
    !> type-mismatch check inside stream_element_mode_row_groups's per-row-group callback.
    subroutine scenario_read_array_em_bool_tm()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        logical :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_bool_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 2)   ! -> aborts (type mismatch: expected bool, got int32)
        print '(a)', "unexpectedly read element_mode of an int32 vector column as logical on an unfiltered reader"
    end subroutine scenario_read_array_em_bool_tm

    !> Converse of scenario_read_array_element_mode_bool_col_index_out_of_range: the string
    !> specific, unfiltered/streamed, out-of-range col_index.
    subroutine scenario_read_array_em_string_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        character(len=8) :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_string_col_index_out_of_range.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on an unfiltered string reader"
    end subroutine scenario_read_array_em_string_oob

    !> Same unfiltered/streamed branch as above, but with a valid col_index -- reaches the
    !> type-mismatch check inside stream_element_mode_row_groups's per-row-group callback.
    subroutine scenario_read_array_em_string_tm()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        character(len=8) :: elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_string_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 2)   ! -> aborts (type mismatch: expected string, got int32)
        print '(a)', "unexpectedly read element_mode of an int32 vector column as string on an unfiltered reader"
    end subroutine scenario_read_array_em_string_tm

    !> parquet_read_column_chunk dispatched to logical (parquet_read_bool8_array_column_chunk)
    !> on a chunk-written int32 vector column aborts with a type mismatch.
    subroutine scenario_read_array_column_chunk_bool_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        logical :: chunk_back(3, 2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_column_chunk_bool_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "vec", data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column_chunk(reader, "vec", 1, chunk_back)   ! -> aborts (type mismatch: expected bool, got int32)
        print '(a)', "unexpectedly read a chunked int32 vector column as logical"
    end subroutine scenario_read_array_column_chunk_bool_type_mismatch

    !> Converse of scenario_read_array_column_chunk_bool_type_mismatch: the string chunk
    !> specific (parquet_read_string_array_column_chunk).
    subroutine scenario_read_array_column_chunk_string_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2)
        character(len=8) :: chunk_back(3, 2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_column_chunk_string_type_mismatch.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "vec", data)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column_chunk(reader, "vec", 1, chunk_back)   ! -> aborts (type mismatch: expected string, got int32)
        print '(a)', "unexpectedly read a chunked int32 vector column as string"
    end subroutine scenario_read_array_column_chunk_string_type_mismatch

    !> read_list_primitive_element's own col_index bounds check (shared by every numeric
    !> parquet_read_*_array_element specific, not just logical/string) was completely untested,
    !> filtered branch included -- this is the filtered-branch half.
    subroutine scenario_read_array_em_filt_int32_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(2), data(3, 2), elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_filtered_int32_col_index_out_of_range.parquet"

        id = [1, 2]
        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call filt%add("id >= 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on a filtered int32 reader"
    end subroutine scenario_read_array_em_filt_int32_oob

    !> Unfiltered/streamed counterpart of scenario_read_array_element_mode_filtered_int32_col_index_out_of_range.
    subroutine scenario_read_array_em_int32_oob()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3, 2), elem_back(2)
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_read_array_element_mode_int32_col_index_out_of_range.parquet"

        data = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 4)   ! -> aborts (col_index out of bounds)
        print '(a)', "unexpectedly read element_mode with an out-of-range col_index on an unfiltered int32 reader"
    end subroutine scenario_read_array_em_int32_oob

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

    !> The converse of scenario_row_group_new_column_after_first
    !> above (a row-group-chunk column introduced late) -- once the streaming row-group API has
    !> locked the file's schema (parquet_finish_row_group's first successful call sets
    !> writer_handle->row_group_writer), a *whole-column* (parquet_write_column) write is never
    !> valid again, even for a column never touched by the streaming API at all.
    subroutine scenario_row_group_whole_column_after_streaming_started()
        type(parquet_writer) :: writer
        integer(int32) :: a_data(2), b_data(2)

        a_data = [1, 2]
        b_data = [10, 20]
        call parquet_open_writer(writer, &
            "test_run/error_scenario_row_group_whole_column_after_streaming_started.parquet", chunk_size=2)
        call parquet_new_row_group(writer, 2)
        call parquet_write_column_chunk(writer, "a", a_data)
        call parquet_finish_row_group(writer)

        call parquet_write_column(writer, "b", b_data)
        print '(a)', "unexpectedly wrote a whole column after the streaming row-group API already started"
    end subroutine scenario_row_group_whole_column_after_streaming_started

    !> parquet_new_row_group must not be called again while a row group is already open (i.e.
    !> without an intervening parquet_finish_row_group) -- see the writer%in_row_group guard in
    !> parquet_new_row_group_impl.
    subroutine scenario_row_group_started_while_open()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]

        call parquet_open_writer(writer, "test_run/error_scenario_row_group_started_while_open.parquet", chunk_size=2)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)
        call parquet_new_row_group(writer, 2_int64)   ! -> aborts, previous row group never finished
        print '(a)', "unexpectedly started a new row group while one was already open"
    end subroutine scenario_row_group_started_while_open

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

    !> A filter value that parses as an integer but doesn't fit
    !> int32's range reports a clean error stop -- distinct from scenario_filter_bad_numeric_value
    !> above, which uses a value that fails to parse as a number at all. id_with_null is int32.
    subroutine scenario_filter_int32_value_out_of_range()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("id_with_null > 99999999999")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an out-of-int32-range filter value"
    end subroutine scenario_filter_int32_value_out_of_range

    !> eval_filter_clause's FLOAT/DOUBLE/HALF_FLOAT/UINT64/DECIMAL*
    !> value-parse-failure branch had never fired -- scenario_filter_bad_numeric_value above only
    !> ever targets an int32 column, hitting the separate integer-family parse-failure branch.
    subroutine scenario_filter_bad_numeric_value_float()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_bad_numeric_value_float.parquet")
        call parquet_write_column(writer, "v", [1.5_real64, 2.5_real64])
        call parquet_close_writer(writer)

        call filt%add("v > abc")
        call parquet_open_reader(reader, "test_run/filter_bad_numeric_value_float.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a non-numeric value against a float filter column"
    end subroutine scenario_filter_bad_numeric_value_float

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

    !> eval_filter_clause's BOOL branch rejects a double-quoted value ("true"/"false" must be
    !> unquoted bare words, matching every other non-string filter value convention) --
    !> distinct from scenario_filter_bad_boolean_value above, which uses an unquoted-but-invalid
    !> word ("flag == 5"), never a quoted one.
    subroutine scenario_filter_boolean_value_must_be_unquoted()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_boolean_quoted.parquet")
        call parquet_write_column(writer, "flag", [.true., .false.])
        call parquet_close_writer(writer)

        call filt%add('flag == "true"')
        call parquet_open_reader(reader, "test_run/filter_boolean_quoted.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a double-quoted boolean filter value"
    end subroutine scenario_filter_boolean_value_must_be_unquoted

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

    !> eval_filter_clause's `default:` branch (parquet_wrapper.cpp) --
    !> a column type filtering doesn't support at all -- had no scenario. A temporal (date)
    !> column isn't in eval_filter_clause's type switch (only INT*/FLOAT*/UINT64/DECIMAL*/BOOL/
    !> STRING* are), so filtering on one reaches this fallback. Aborts via a clean Fortran
    !> `error stop` (parquet_apply_filter, parquet_read.f90), not report_fatal_error -- exit
    !> code 1, not SIGABRT.
    subroutine scenario_filter_unsupported_column_type()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: day(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_unsupported_column_type.parquet"

        call day(1)%set(2024, 1, 1)
        call day(2)%set(2024, 1, 2)
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "day", day)
        call parquet_close_writer(writer)

        call filt%add("day == 2024")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter clause against a temporal column"
    end subroutine scenario_filter_unsupported_column_type

    !> parquet_open_reader's sample_fraction < 0.0 aborts immediately -- see
    !> parquet_open_reader_base's NaN/negative checks (parquet_read.f90).
    subroutine scenario_sample_negative_fraction()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(3) = [1, 2, 3]
        character(len=*), parameter :: out_file = "test_run/error_scenario_sample_negative_fraction.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=-0.1_real64)
        print '(a)', "unexpectedly opened a reader with a negative sample_fraction"
    end subroutine scenario_sample_negative_fraction

    !> parquet_open_reader's sample_fraction NaN aborts immediately, checked before any relational
    !> comparison (NaN < 0.0 and NaN < 1.0 are both false, so a NaN would otherwise silently fall
    !> through as a no-op instead of reaching an error stop) -- see parquet_open_reader_base
    !> (parquet_read.f90).
    subroutine scenario_sample_nan_fraction()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(3) = [1, 2, 3]
        character(len=*), parameter :: out_file = "test_run/error_scenario_sample_nan_fraction.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a)', "unexpectedly opened a reader with a NaN sample_fraction"
    end subroutine scenario_sample_nan_fraction

    !> parquet_read_column_chunk is disallowed on a reader opened with sample_fraction < 1.0 alone
    !> (no filter= at all) -- sampling shares filter_mask/parquet_reader_has_filter with filter=,
    !> so check_reader_no_filter's guard fires the same way (parquet_read.f90).
    subroutine scenario_read_chunk_with_sample()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(4), back(2)

        v = [1, 2, 3, 4]
        call parquet_open_writer(writer, "test_run/read_chunk_sample.parquet", chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/read_chunk_sample.parquet", sample_fraction=0.5_real64)
        call parquet_read_column_chunk(reader, "v", 1, back)
        print '(a)', "unexpectedly read a chunk on a sampled reader without aborting"
    end subroutine scenario_read_chunk_with_sample

    !> parquet_reader_print_stat's "sample:" line (added alongside the pre-existing "rows: N (of M
    !> total)" summary -- see scenario_print_stat_filtered_rows above for that one). sample_seed=42
    !> (> 0) so the reported seed is the exact caller-supplied value, not an entropy-drawn one --
    !> a fraction of exactly 0.0 would also be deterministic, but skips the draw entirely and
    !> always reports seed=0 regardless of sample_seed (see parquet_reader_set_sample's own
    !> comment), which wouldn't prove a caller-supplied seed round-trips into this line at all.
    subroutine scenario_print_stat_sampled_rows()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5)
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_sampled_rows.parquet"

        a_values = [(i, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file, sample_fraction=0.4_real64, sample_seed=42)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the sample: fraction=... summary line"
    end subroutine scenario_print_stat_sampled_rows

    !> parquet_reader_set_sample's failure return (parquet_wrapper.cpp) -- and the Fortran-side
    !> error stop that surfaces it (parquet_apply_sample, parquet_read.f90) -- forced via a
    !> debug-only hook (g_debug_force_sample_mask_error, reachable only through the local bind(C)
    !> interface declared below) rather than a genuine BooleanBuilder allocation failure, which
    !> isn't fixture-triggerable in practice. Same process-global/subprocess-isolation reasoning as
    !> scenario_col_size_and_row_mode_avoid_whole_column_read's own debug hook use, above.
    subroutine scenario_sample_mask_build_error()
        interface
            subroutine parquet_debug_set_force_sample_mask_error(enable) &
                bind(C, name="parquet_debug_set_force_sample_mask_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next sample mask build to fail; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_sample_mask_error
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(3) = [1, 2, 3]
        character(len=*), parameter :: out_file = "test_run/error_scenario_sample_mask_build_error.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_debug_set_force_sample_mask_error(1)
        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64)
        print '(a)', "unexpectedly opened a sampled reader despite the forced sample-mask-build error"
    end subroutine scenario_sample_mask_build_error

    !> parquet_reader_get_string_length's `default:` fallback
    !> (parquet_wrapper.cpp) for a column that isn't string-like/LIST/LARGE_LIST/FIXED_SIZE_LIST
    !> at all -- calling it against a plain int32 column reaches this. NOTE: unlike most
    !> `report_fatal_error` gaps, this line is a
    !> `throw std::runtime_error(...)` with no catch anywhere in its call chain (confirmed: this
    !> whole file has exactly one `catch` block, in parquet_reader_set_filter, unrelated to this
    !> function) -- expected to cross the extern "C" boundary uncaught and abort via
    !> std::terminate(), not a clean `error stop`.
    subroutine scenario_string_length_on_non_string_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: data(3) = [1_int32, 2_int32, 3_int32]
        integer(int32) :: strlen_max
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_string_length_on_non_string_column.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "n", data)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_string_length(reader, "n", strlen_max)
        print '(a,i0)', "unexpectedly got a string length for a non-string column: ", strlen_max
    end subroutine scenario_string_length_on_non_string_column

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

    !> Same as scenario_qc_range_violation_warns, but against one of the
    !> extended read-time source types (UINT16 -- see run_qc_range_check's
    !> is_small_integer_family branch in parquet_wrapper.cpp) instead of a
    !> plain INT32 column, proving qc range checking was actually extended to
    !> cover these types rather than silently never firing for them (see
    !> CONTRIBUTING.md's "Additional scalar types" note). Reads
    !> test/fixtures/extended_types.parquet's v_uint16 column (values 1000,
    !> 0, 65535) against a declared max: 1000 -- row 3's 65535 violates.
    subroutine scenario_extended_qc_range_violation_warns()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call write_text_file("test_run/qc_extended.maml", [character(len=32) :: &
            "fields:", "- name: v_uint16", "  qc:", "    max: 1000"])

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_extended.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "v_uint16", values)
        call parquet_close_reader(reader)
    end subroutine scenario_extended_qc_range_violation_warns

    !> Same as scenario_qc_range_violation_warns, but against a plain INT64 column instead of
    !> INT32 -- proves run_qc_range_check's is_small_integer_family branch actually reaches
    !> small_integer_value_at's own INT64 case arm (parquet_wrapper.cpp). Every other qc range
    !> scenario uses INT32/UINT16/STRING/FLOAT columns; eval_filter_clause and the array
    !> conversion helpers all special-case INT64 directly and never call small_integer_value_at
    !> with it, so this scenario is the only way to reach that case arm.
    subroutine scenario_qc_range_violation_int64_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int64) :: v(3), v_back(3)

        v = [10_int64, 4000000000_int64, -5_int64] ! 4000000000 and -5 are outside [0, 1000]

        call parquet_open_writer(writer, "test_run/qc_range_int64.parquet")
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_int64.maml", [character(len=32) :: &
            "fields:", "- name: v", "  qc:", "    min: 0", "    max: 1000"])

        call parquet_open_reader(reader, "test_run/qc_range_int64.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_int64.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "v", v_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_range_violation_int64_warns

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

    !> scenario_qc_range_violation_warns/_hard_aborts above only ever
    !> use an INT32 column, so run_qc_range_check's STRING/LARGE_STRING/STRING_VIEW branch
    !> (parquet_wrapper.cpp) had never fired. Same shape as scenario_qc_range_violation_warns,
    !> but a string column with a qc min/max bound.
    subroutine scenario_qc_range_violation_string_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=8) :: sv(4), sv_back(4)

        sv = [character(len=8) :: "mango", "apple", "banana", "zebra"] ! "apple" and "zebra" are outside [banana, mango]

        call parquet_open_writer(writer, "test_run/qc_range_string.parquet")
        call parquet_write_column(writer, "sv", sv)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_string.maml", [character(len=32) :: &
            "fields:", "- name: sv", "  qc:", "    min: banana", "    max: mango"])

        call parquet_open_reader(reader, "test_run/qc_range_string.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_string.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "sv", sv_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_range_violation_string_warns

    !> Default (qc_soft=.false., hard) counterpart of scenario_qc_range_violation_string_warns.
    subroutine scenario_qc_range_violation_string_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=8) :: sv(4), sv_back(4)

        sv = [character(len=8) :: "mango", "apple", "banana", "zebra"] ! "apple" and "zebra" are outside [banana, mango]

        call parquet_open_writer(writer, "test_run/qc_range_string_hard.parquet")
        call parquet_write_column(writer, "sv", sv)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_string_hard.maml", [character(len=32) :: &
            "fields:", "- name: sv", "  qc:", "    min: banana", "    max: mango"])

        call parquet_open_reader(reader, "test_run/qc_range_string_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_string_hard.maml"))
        call parquet_read_column(reader, "sv", sv_back)
        print '(a)', "unexpectedly read an out-of-range string value without aborting in hard qc mode"
    end subroutine scenario_qc_range_violation_string_hard_aborts

    !> run_qc_range_check's FLOAT/DOUBLE/DECIMAL* bounds-description
    !> formatting branch (parquet_wrapper.cpp) had never fired either -- same shape as
    !> scenario_qc_range_violation_warns, but a real64 column with a qc min/max bound.
    subroutine scenario_qc_range_violation_float_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64) :: fv(4), fv_back(4)

        fv = [10.5_real64, 400.25_real64, -5.5_real64, 300.0_real64] ! 400.25 and -5.5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range_float.parquet")
        call parquet_write_column(writer, "fv", fv)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_float.maml", [character(len=32) :: &
            "fields:", "- name: fv", "  qc:", "    min: 0.0", "    max: 360.0"])

        call parquet_open_reader(reader, "test_run/qc_range_float.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_float.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "fv", fv_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_range_violation_float_warns

    !> Default (qc_soft=.false., hard) counterpart of scenario_qc_range_violation_float_warns.
    subroutine scenario_qc_range_violation_float_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64) :: fv(4), fv_back(4)

        fv = [10.5_real64, 400.25_real64, -5.5_real64, 300.0_real64] ! 400.25 and -5.5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range_float_hard.parquet")
        call parquet_write_column(writer, "fv", fv)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_float_hard.maml", [character(len=32) :: &
            "fields:", "- name: fv", "  qc:", "    min: 0.0", "    max: 360.0"])

        call parquet_open_reader(reader, "test_run/qc_range_float_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_float_hard.maml"))
        call parquet_read_column(reader, "fv", fv_back)
        print '(a)', "unexpectedly read an out-of-range float value without aborting in hard qc mode"
    end subroutine scenario_qc_range_violation_float_hard_aborts

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

    !> The set_col_qc in-place form shares add_col_qc's worker, so it enforces
    !> the same validation -- e.g. a reversed min: operator aborts here too.
    subroutine scenario_set_col_qc_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        col_name = "ra, <5"
        call maml%set_col_qc(col_name)
        print '(a)', "unexpectedly accepted a reversed qc min operator in set_col_qc"
    end subroutine scenario_set_col_qc_reversed_operator

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

    !> Mirrors scenario_validate_qc_min_not_numeric but for qc: max: -- exercises the
    !> separate has_qc_max/qc_max_raw numeric-convertibility check in
    !> parquet_validate_maml_internal (parquet_metadata_validate.f90), distinct from the
    !> qc_min one above.
    subroutine scenario_validate_qc_max_not_numeric()
        type(parquet_maml_file) :: maml

        maml%name = "qc_max_not_numeric.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    max: not_a_number" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_max_not_numeric

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

    !> qc: miss: not declared (the default) means Nulls are NOT expected: writing a Null through
    !! an is_valid= mask must print a WARNING naming the column -- checked here with NO explicit
    !! qc= passed to parquet_open_writer at all, to prove qc now defaults to present(schema)
    !! rather than needing an explicit qc=.true. (see parquet_open_writer's qc doc comment).
    subroutine scenario_qc_miss_default_active_numeric_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        call schema%init(table="qc_miss_table")
        call schema%add_field("id", "int32")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_default.parquet", schema)
        call parquet_write_column(writer, "id", values, is_valid=is_valid)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_default_active_numeric_warns

    !> Same as scenario_qc_miss_default_active_numeric_warns, but the field declares
    !! qc: miss: Null -- Nulls are expected here, so no WARNING should print.
    subroutine scenario_qc_miss_declared_null_no_warning()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        call schema%init(table="qc_miss_table")
        call schema%add_field("id", "int32", qc_miss="Null")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_allowed.parquet", schema)
        call parquet_write_column(writer, "id", values, is_valid=is_valid)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_declared_null_no_warning

    !> Same miss-not-declared-warns shape as scenario_qc_miss_default_active_numeric_warns, but
    !! for a compact (parquet_string_column) string write -- the write path that reaches
    !! parquet_check_qc_miss via parquet_write_string.f90's is_null()-derived is_valid_flat rather
    !! than a caller-supplied is_valid= mask.
    subroutine scenario_qc_miss_string_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: values

        call schema%init(table="qc_miss_table")
        call schema%add_field("s", "string")
        call parquet_parse_maml(schema)

        call values%append_string("apple")
        call values%append_null()
        call values%append_string("cherry")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_string.parquet", schema)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_string_warns

    !> Same miss-not-declared-warns shape, for a parquet_date column -- the write path that
    !! reaches parquet_check_qc_miss via parquet_write_temporal.f90's temporal_valid_ptr.
    subroutine scenario_qc_miss_temporal_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_date) :: values(3)

        call values(1)%set(2024, 1, 1)
        ! values(2) left default-initialized -- a null date.
        call values(3)%set(2024, 1, 3)

        call schema%init(table="qc_miss_table")
        call schema%add_field("d", "date")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_temporal.parquet", schema)
        call parquet_write_column(writer, "d", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_temporal_warns

    !> A schema built entirely via %init/%add_field (qc_min/qc_max, no separate add_col_qc
    !! qc-maml) must drive real read-time qc enforcement when passed as schema= to
    !! parquet_open_reader -- not just write-time enforcement. Exercises the same
    !! parquet_parse_qc_maml/run_qc_range_check path add_col_qc already had full coverage for,
    !! but sourced from an add_field-embedded qc: block instead of a hand-written qc-maml.
    subroutine scenario_add_field_qc_drives_reader_enforcement()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(3) = [10_int32, 400_int32, 300_int32] ! 400 is outside [0, 360]
        integer(int32) :: ra_back(3)

        call schema%init(table="add_field_qc_table")
        call schema%add_field("ra", "int32", qc_min="0", qc_max="360")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_add_field_qc_reader.parquet", schema, qc=.false.)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/error_scenario_add_field_qc_reader.parquet", schema=schema, &
            qc_soft=.true.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_add_field_qc_drives_reader_enforcement

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

    !> schema%init error stops if called (without force=.true.) on a schema already populated
    !> via a MAML parse -- schema%is_init() is .true. here even though %init was never called,
    !> so the "already initialized" guard must catch this too, not just a literal second %init.
    subroutine scenario_schema_init_after_maml_parse()
        type(parquet_schema) :: schema

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%init(table="t2")
        print '(a)', "unexpectedly re-initialized a MAML-parsed schema without error"
    end subroutine scenario_schema_init_after_maml_parse

    !> The file form of parquet_parse_maml error stops if `schema` is already initialized
    !> (via %init or an earlier parse) -- otherwise it would silently discard whatever the
    !> schema held before, with no diagnostic.
    subroutine scenario_parse_maml_file_after_init()
        type(parquet_schema) :: schema

        call schema%init(table="t1")
        call schema%add_field("x", "int32")
        call parquet_parse_maml("schemas/maml_example.maml", schema)
        print '(a)', "unexpectedly loaded a .maml file into an already-initialized schema without error"
    end subroutine scenario_parse_maml_file_after_init

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

    !> schema%get_field(name=) error stops if the field doesn't exist.
    subroutine scenario_get_field_by_name_not_found()
        type(parquet_schema) :: schema
        character(len=:), allocatable :: data_type

        call schema%init(table="t")
        call schema%add_field("ra", "float64")
        call parquet_parse_maml(schema)
        call schema%get_field("does_not_exist", data_type=data_type)
        print '(a)', "unexpectedly found a non-existent field via schema%get_field(name=)"
    end subroutine scenario_get_field_by_name_not_found

    !> schema%get_field(index=) error stops if index is out of range.
    subroutine scenario_get_field_by_index_out_of_range()
        type(parquet_schema) :: schema
        character(len=:), allocatable :: name

        call schema%init(table="t")
        call schema%add_field("ra", "float64")
        call parquet_parse_maml(schema)
        call schema%get_field(5, name)
        print '(a)', "unexpectedly resolved an out-of-range index via schema%get_field(index=)"
    end subroutine scenario_get_field_by_index_out_of_range

    !> schema%add_field_from error stops if the named field doesn't exist on source_schema
    !> (via %get_field's own not-found error).
    subroutine scenario_add_field_from_source_not_found()
        type(parquet_schema) :: source, target

        call source%init(table="src")
        call source%add_field("ra", "float64")
        call parquet_parse_maml(source)

        call target%init(table="dst")
        call target%add_field_from(source, "does_not_exist")
        print '(a)', "unexpectedly copied a non-existent field via schema%add_field_from"
    end subroutine scenario_add_field_from_source_not_found

    !> schema%print_schema_info error stops if given neither unit nor filename -- there is
    !> nowhere to write to.
    subroutine scenario_print_schema_info_no_unit_no_filename()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        call schema%print_schema_info()
        print '(a)', "unexpectedly printed schema info with neither unit nor filename given"
    end subroutine scenario_print_schema_info_no_unit_no_filename

    !> schema%print_schema_info error stops if the given unit is not already open.
    subroutine scenario_print_schema_info_unit_not_open()
        type(parquet_schema) :: schema
        integer :: u

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        ! Reserve a genuinely unused unit number by opening then closing it.
        open(newunit=u, file="test_run/print_schema_info_unit_not_open_scratch.txt", status="replace")
        close(u)

        call schema%print_schema_info(unit=u)
        print '(a)', "unexpectedly printed schema info to a unit that was not open"
    end subroutine scenario_print_schema_info_unit_not_open

    !> schema%print_schema_info error stops if the given unit is open for reading only.
    subroutine scenario_print_schema_info_unit_read_only()
        type(parquet_schema) :: schema
        integer :: u

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        open(newunit=u, file="schemas/maml_example.maml", status="old", action="read")
        call schema%print_schema_info(unit=u)
        print '(a)', "unexpectedly printed schema info to a unit open for reading only"
    end subroutine scenario_print_schema_info_unit_read_only

    !> schema%print_schema_info error stops if unit and filename are both given but filename
    !> does not match the file the unit is actually connected to.
    subroutine scenario_print_schema_info_unit_filename_mismatch()
        type(parquet_schema) :: schema
        integer :: u

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        open(newunit=u, file="test_run/print_schema_info_mismatch_actual.txt", status="replace", &
            action="write", form="formatted")
        call schema%print_schema_info(unit=u, filename="test_run/print_schema_info_mismatch_other.txt")
        print '(a)', "unexpectedly printed schema info despite a unit/filename mismatch"
    end subroutine scenario_print_schema_info_unit_filename_mismatch

    !> schema%print_schema_info error stops by default if the schema has not been parsed
    !> (%cinfo not populated), unless allow_uninitialized=.true. is given.
    subroutine scenario_print_schema_info_uninitialized_schema()
        type(parquet_schema) :: schema
        integer :: u

        open(newunit=u, file="test_run/print_schema_info_uninitialized_scratch.txt", status="replace", &
            action="write", form="formatted")
        call schema%print_schema_info(unit=u)
        print '(a)', "unexpectedly printed schema info for a never-initialized schema"
    end subroutine scenario_print_schema_info_uninitialized_schema

    !> schema%print_schema_info error stops if filename= cannot be opened for writing (here,
    !! because its parent directory does not exist).
    subroutine scenario_print_schema_info_open_failure()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        call schema%print_schema_info(filename="test_run/no_such_subdir/print_schema_info_open_failure.txt")
        print '(a)', "unexpectedly printed schema info to a filename that could not be opened"
    end subroutine scenario_print_schema_info_open_failure

    !> schema%add_metadata error stops if called before the schema has been parsed --
    !> right after %init/%add_field but before parquet_parse_maml has populated %cinfo/
    !> %metadata%items, which would otherwise silently discard the entry once that parse runs.
    subroutine scenario_schema_add_metadata_before_parse()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call schema%add_metadata("k", 1_int32)
        print '(a)', "unexpectedly added metadata to a schema that has not been parsed yet"
    end subroutine scenario_schema_add_metadata_before_parse

    !> parquet_string_column: indexing out of range aborts (check_index).
    subroutine scenario_string_column_index_out_of_range()
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("a")
        call col%append_string("b")
        call col%get(5, s)   ! index 5 > nrows 2 -> aborts
        print '(a)', "unexpectedly read an out-of-range index: "//s
    end subroutine scenario_string_column_index_out_of_range

    !> parquet_string_column%view_all: a data_string array whose size doesn't match self%size()
    !! aborts rather than silently populating only the shorter length.
    subroutine scenario_string_column_view_all_size_mismatch()
        type(parquet_string_column), target :: col
        type(parquet_string) :: handles(3)
        call col%append_string("a")
        call col%append_string("b")
        call col%view_all(handles)   ! 3 handles, but col%size() == 2 -> aborts
        print '(a)', "unexpectedly filled a mismatched-size view_all array"
    end subroutine scenario_string_column_view_all_size_mismatch

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

    !> parquet_string handle: set_null on an unassociated handle aborts (check_handle), same
    !! convention as every other handle accessor.
    subroutine scenario_string_set_null_unassociated()
        type(parquet_string) :: h
        call h%set_null()   ! handle never associated with a column -> aborts
        print '(a)', "unexpectedly called set_null on an unassociated handle"
    end subroutine scenario_string_set_null_unassociated

    !> parquet_string handle: set_null on a stale handle (index shifted out of range by an erase)
    !! aborts (check_handle).
    subroutine scenario_string_set_null_stale_index()
        type(parquet_string_column), target :: col
        type(parquet_string) :: h
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("c")
        h = col%view(3)
        call col%erase(1)
        call col%erase(1)   ! nrows now 1; handle still refers to index 3
        call h%set_null()   ! idx 3 > nrows 1 -> aborts
        print '(a)', "unexpectedly called set_null on a stale handle"
    end subroutine scenario_string_set_null_stale_index

    !> parquet_string_column%slice: first > last is an invalid range (check_range) and aborts --
    !! there is no valid "empty slice" call shape.
    subroutine scenario_string_slice_invalid_range()
        type(parquet_string_column) :: col, dest
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("c")
        call col%slice(3_int64, 2_int64, dest)   ! first > last -> aborts
        print '(a,i0)', "unexpectedly accepted an invalid slice range, dest size=", dest%size()
    end subroutine scenario_string_slice_invalid_range

    !> parquet_string_column%view_slice: last > self%size() is an invalid range (check_range) and
    !! aborts -- a distinct call site from slice's own check_range coverage above.
    subroutine scenario_string_view_slice_invalid_range()
        type(parquet_string_column), target :: col
        type(parquet_string) :: handles(3)
        call col%append_string("a")
        call col%append_string("b")
        call col%view_slice(1_int64, 3_int64, handles)   ! last 3 > size() 2 -> aborts
        print '(a)', "unexpectedly accepted an out-of-range view_slice"
    end subroutine scenario_string_view_slice_invalid_range

    !> parquet_string_column%view_slice: a data_string array whose size doesn't match
    !! last-first+1 aborts rather than silently populating only the shorter length -- same
    !! convention as view_all's own size-mismatch guard.
    subroutine scenario_string_view_slice_size_mismatch()
        type(parquet_string_column), target :: col
        type(parquet_string) :: handles(3)
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("c")
        call col%view_slice(1_int64, 2_int64, handles)   ! range size 2, array size 3 -> aborts
        print '(a)', "unexpectedly filled a mismatched-size view_slice array"
    end subroutine scenario_string_view_slice_size_mismatch

    !> parquet_string_column%build_from: a handle that aliases the destination column aborts
    !! before self is cleared -- the exact scenario feature_stringcolumn.md's design flags as
    !! forbidden (build_from clearing self would otherwise silently destroy the handles' own
    !! source data before they could be read).
    subroutine scenario_string_build_from_self_alias()
        type(parquet_string_column), target :: global_string
        type(parquet_string) :: my_string_array(2)
        call global_string%append_string("a")
        call global_string%append_string("b")
        my_string_array(1) = global_string%view(1)
        my_string_array(2) = global_string%view(2)
        call global_string%build_from(my_string_array)   ! handles alias the destination -> aborts
        print '(a,i0)', "unexpectedly overwrote a column aliased by its own build_from input, size=", &
            global_string%size()
    end subroutine scenario_string_build_from_self_alias

    !> parquet_string_column%build_from: an unassociated handle in the input array is a distinct
    !! programmer error and aborts before self is cleared.
    subroutine scenario_string_build_from_unassociated()
        type(parquet_string_column), target :: src, dest
        type(parquet_string) :: handles(2)
        call src%append_string("a")
        handles(1) = src%view(1)
        ! handles(2) left unassociated
        call dest%build_from(handles)
        print '(a,i0)', "unexpectedly gathered an unassociated handle, dest size=", dest%size()
    end subroutine scenario_string_build_from_unassociated

    !> parquet_string_column%build_from: a stale handle (index shifted out of range by an erase)
    !! in the input array aborts before self is cleared -- the caller must explicitly set_null a
    !! handle before it goes stale (see set_null above), not rely on build_from tolerating it.
    subroutine scenario_string_build_from_stale_index()
        type(parquet_string_column), target :: src, dest
        type(parquet_string) :: handles(1)
        call src%append_string("a")
        call src%append_string("b")
        handles(1) = src%view(2)
        call src%erase(2)   ! nrows now 1; handle still refers to index 2
        call dest%build_from(handles)
        print '(a,i0)', "unexpectedly gathered a stale handle, dest size=", dest%size()
    end subroutine scenario_string_build_from_stale_index

    !> parquet_string_column%append_buffers: a source offsets buffer whose first entry isn't 0
    !! (e.g. straight from a sliced Arrow array, not rebased by the caller) aborts rather than
    !! silently misplacing every element's bytes -- see the precondition documented on
    !! append_buffers itself.
    subroutine scenario_string_column_append_buffers_offset_not_zero()
        use, intrinsic :: iso_c_binding, only : c_loc, c_null_ptr
        type(parquet_string_column) :: col
        integer(int64), target :: off64(3)
        character(len=1), target :: dat(4)
        off64 = [1_int64, 3_int64, 4_int64]   ! first entry is 1, not 0 -> not rebased
        dat = [character(len=1) :: "a", "b", "c", "d"]
        call col%append_buffers(2_int64, 3_int64, c_loc(off64), c_loc(dat), c_null_ptr, .false.)
        print '(a,i0)', "unexpectedly accepted un-rebased offsets, size=", col%size()
    end subroutine scenario_string_column_append_buffers_offset_not_zero

    !> Same as scenario_string_column_append_buffers_offset_not_zero, but with int32 source
    !! offsets (offsets_int32=.true.) instead of int64 -- a distinct source line (append_buffers
    !! has one guard per branch), so needs its own scenario for coverage.
    subroutine scenario_string_column_append_buffers_offset_not_zero_int32()
        use, intrinsic :: iso_c_binding, only : c_loc, c_null_ptr
        type(parquet_string_column) :: col
        integer(int32), target :: off32(3)
        character(len=1), target :: dat(4)
        off32 = [1_int32, 3_int32, 4_int32]   ! first entry is 1, not 0 -> not rebased
        dat = [character(len=1) :: "a", "b", "c", "d"]
        call col%append_buffers(2_int64, 3_int64, c_loc(off32), c_loc(dat), c_null_ptr, .true.)
        print '(a,i0)', "unexpectedly accepted un-rebased int32 offsets, size=", col%size()
    end subroutine scenario_string_column_append_buffers_offset_not_zero_int32

    !> Compact (parquet_string_column) write into a schema-declared vector (col_size>1) string
    !> column aborts -- the compact path is scalar-only (see parquet_write_column's own doc
    !> comment in parquet.f90).
    subroutine scenario_compact_string_write_requires_scalar_column()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: col

        call col%append_string("a")
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_compact_string_vector_column.parquet", schema)
        call parquet_write_column(writer, "str", col)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a compact string column into a vector (col_size>1) schema field"
    end subroutine scenario_compact_string_write_requires_scalar_column

    !> Same as scenario_compact_string_write_requires_scalar_column, but via the chunked
    !> (parquet_write_column_chunk) path.
    subroutine scenario_compact_string_write_chunk_requires_scalar_column()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: col

        call col%append_string("a")
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, &
            "test_run/error_scenario_compact_string_chunk_vector_column.parquet", schema)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "str", col)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a compact string column chunk into a vector (col_size>1) schema field"
    end subroutine scenario_compact_string_write_chunk_requires_scalar_column

    !> parquet_read_string_column_buffers/parquet_read_string_column_chunk_buffers (the compact
    !> buffer-handoff path behind reading a STRING_VIEW column into a parquet_string_column) only
    !> understand the two offset-based string representations (STRING/LARGE_STRING) -- a
    !> STRING_VIEW array has no offsets buffer at all, so is_offset_string_type in
    !> parquet_wrapper.cpp rejects it with a clear error rather than misreading it (see that
    !> function's own comment). Proves that error fires cleanly instead of the fixed-width
    !> parquet_read_column path used by scenario_string_view_roundtrip, above.
    subroutine scenario_string_view_compact_read_unsupported()
        interface
            subroutine parquet_debug_write_string_view_fixture(path, column_name) &
                bind(C, name="parquet_debug_write_string_view_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char), intent(in) :: path(*) !! null-terminated output file path.
                character(kind=c_char), intent(in) :: column_name(*) !! null-terminated STRING_VIEW column name.
            end subroutine parquet_debug_write_string_view_fixture
        end interface

        type(parquet_reader) :: reader
        type(parquet_string_column) :: col
        character(len=*), parameter :: out_file = "test_run/error_scenario_string_view_compact.parquet"

        call parquet_debug_write_string_view_fixture(out_file//char(0), "sv"//char(0))

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "sv", col)
        call parquet_close_reader(reader)
        print '(a)', "unexpectedly read a STRING_VIEW column into a compact parquet_string_column"
    end subroutine scenario_string_view_compact_read_unsupported

    !> parquet_date: set with a month outside 1..12 aborts.
    subroutine scenario_temporal_date_set_invalid_month()
        type(parquet_date) :: d
        call d%set(2024, 13, 1)   ! month 13 -> aborts
        print '(a)', "unexpectedly accepted month 13"
    end subroutine scenario_temporal_date_set_invalid_month

    !> parquet_date: set with a day invalid for its month/year aborts (1900 is not a leap year).
    subroutine scenario_temporal_date_set_invalid_day()
        type(parquet_date) :: d
        call d%set(1900, 2, 29)   ! century non-leap -> aborts
        print '(a)', "unexpectedly accepted 1900-02-29"
    end subroutine scenario_temporal_date_set_invalid_day

    !> parquet_date: set with a year whose day count exceeds the int32 range aborts.
    subroutine scenario_temporal_date_set_out_of_range()
        type(parquet_date) :: d
        call d%set(6000000, 1, 1)   ! ~2.2e9 days > huge(int32) -> aborts
        print '(a)', "unexpectedly accepted a date beyond the representable range"
    end subroutine scenario_temporal_date_set_out_of_range

    !> parquet_date: set_mjd beyond the int32 day range aborts.
    subroutine scenario_temporal_date_set_mjd_out_of_range()
        type(parquet_date) :: d
        call d%set_mjd(3000000000_int64)   ! beyond huge(int32) days -> aborts
        print '(a)', "unexpectedly accepted an out-of-range MJD"
    end subroutine scenario_temporal_date_set_mjd_out_of_range

    !> parquet_date: parse failure without the optional success argument aborts.
    subroutine scenario_temporal_date_parse_invalid()
        type(parquet_date) :: d
        call d%parse("not-a-date")   ! no success= -> aborts
        print '(a)', "unexpectedly parsed garbage as a date"
    end subroutine scenario_temporal_date_parse_invalid

    !> parquet_date: a semantic accessor (get) on a null element aborts.
    subroutine scenario_temporal_date_null_get()
        type(parquet_date) :: d   ! default-initialized -> null
        integer(int32) :: y, m, dd
        call d%get(y, m, dd)   ! null -> aborts
        print '(a,3i0)', "unexpectedly read a null date: ", y, m, dd
    end subroutine scenario_temporal_date_null_get

    !> parquet_date: comparing against a null element aborts.
    subroutine scenario_temporal_date_null_comparison()
        type(parquet_date) :: a, b
        logical :: res
        call a%set(2024, 7, 16)   ! b stays null
        res = a < b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null date: ", res
    end subroutine scenario_temporal_date_null_comparison

    !> parquet_time: set with an hour outside 0..23 aborts.
    subroutine scenario_temporal_time_set_invalid_hour()
        type(parquet_time) :: t
        call t%set(24, 0, 0)   ! hour 24 -> aborts
        print '(a)', "unexpectedly accepted hour 24"
    end subroutine scenario_temporal_time_set_invalid_hour

    !> parquet_time: set with a nanosecond part outside 0..999999999 aborts.
    subroutine scenario_temporal_time_set_invalid_nanosecond()
        type(parquet_time) :: t
        call t%set(12, 0, 0, 1000000000)   ! 1e9 ns -> aborts
        print '(a)', "unexpectedly accepted nanosecond 1000000000"
    end subroutine scenario_temporal_time_set_invalid_nanosecond

    !> parquet_time: set_raw with a value outside a day aborts.
    subroutine scenario_temporal_time_set_raw_out_of_range()
        type(parquet_time) :: t
        call t%set_raw(86400000000000_int64)   ! == a full day -> aborts
        print '(a)', "unexpectedly accepted an out-of-day raw time"
    end subroutine scenario_temporal_time_set_raw_out_of_range

    !> parquet_time: parse failure without the optional success argument aborts.
    subroutine scenario_temporal_time_parse_invalid()
        type(parquet_time) :: t
        call t%parse("25:99:99")   ! no success= -> aborts
        print '(a)', "unexpectedly parsed garbage as a time"
    end subroutine scenario_temporal_time_parse_invalid

    !> parquet_time: a semantic accessor (hour) on a null element aborts.
    subroutine scenario_temporal_time_null_get()
        type(parquet_time) :: t   ! default-initialized -> null
        integer(int32) :: h
        h = t%hour()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null time: ", h
    end subroutine scenario_temporal_time_null_get

    !> parquet_timestamp: set with a day invalid for its month aborts.
    subroutine scenario_temporal_ts_set_invalid_day()
        type(parquet_timestamp) :: ts
        call ts%set(2024, 2, 30, 0, 0, 0)   ! Feb 30 -> aborts
        print '(a)', "unexpectedly accepted 2024-02-30"
    end subroutine scenario_temporal_ts_set_invalid_day

    !> parquet_timestamp: parse failure without the optional success argument aborts.
    subroutine scenario_temporal_ts_parse_invalid()
        type(parquet_timestamp) :: ts
        call ts%parse("2024-07-16")   ! date without time, no success= -> aborts
        print '(a)', "unexpectedly parsed a timeless string as a timestamp"
    end subroutine scenario_temporal_ts_parse_invalid

    !> parquet_timestamp: to_unix on a null element aborts.
    subroutine scenario_temporal_ts_null_to_unix()
        type(parquet_timestamp) :: ts   ! default-initialized -> null
        integer(int64) :: v
        v = ts%to_unix(parquet_unit_seconds)   ! null -> aborts
        print '(a,i0)', "unexpectedly converted a null timestamp: ", v
    end subroutine scenario_temporal_ts_null_to_unix

    !> parquet_timestamp: comparing against a null element aborts.
    subroutine scenario_temporal_ts_null_comparison()
        type(parquet_timestamp) :: a, b
        logical :: res
        call a%set(2024, 7, 16, 0, 0, 0)   ! b stays null
        res = a == b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null timestamp: ", res
    end subroutine scenario_temporal_ts_null_comparison

    !> parquet_timestamp: to_unix into a coarser unit than the value carries aborts by default.
    subroutine scenario_temporal_ts_to_unix_precision_loss()
        type(parquet_timestamp) :: ts
        integer(int64) :: v
        call ts%set(2024, 7, 16, 0, 0, 0, 123456789)
        v = ts%to_unix(parquet_unit_millis)   ! sub-millisecond precision, no exact=.false. -> aborts
        print '(a,i0)', "unexpectedly lost precision silently: ", v
    end subroutine scenario_temporal_ts_to_unix_precision_loss

    !> parquet_timestamp: to_unix whose result does not fit int64 in the requested unit aborts.
    subroutine scenario_temporal_ts_to_unix_overflow()
        type(parquet_timestamp) :: ts
        integer(int64) :: v
        call ts%set(3000, 1, 1, 0, 0, 0)   ! year 3000 in nanoseconds overflows int64
        v = ts%to_unix(parquet_unit_nanos)   ! -> aborts
        print '(a,i0)', "unexpectedly overflowed silently: ", v
    end subroutine scenario_temporal_ts_to_unix_overflow

    !> parquet_timestamp: set_mjd beyond the representable range aborts.
    subroutine scenario_temporal_ts_set_mjd_out_of_range()
        type(parquet_timestamp) :: ts
        call ts%set_mjd(2.0e14_real64)   ! beyond the int64 seconds range -> aborts
        print '(a)', "unexpectedly accepted an out-of-range MJD"
    end subroutine scenario_temporal_ts_set_mjd_out_of_range

    !> parquet_timestamp: set_raw with a non-normalized nanosecond part aborts.
    subroutine scenario_temporal_ts_set_raw_invalid_nanoseconds()
        type(parquet_timestamp) :: ts
        call ts%set_raw(0_int64, -1_int32)   ! ns must be 0..999999999 -> aborts
        print '(a)', "unexpectedly accepted a negative nanosecond part"
    end subroutine scenario_temporal_ts_set_raw_invalid_nanoseconds

    !> parquet_temporal: an unrecognized time-unit selector aborts.
    subroutine scenario_temporal_invalid_time_unit()
        type(parquet_timestamp) :: ts
        integer(int64) :: v
        call ts%set(2024, 7, 16, 0, 0, 0)
        v = ts%to_unix(99)   ! not a parquet_unit_* constant -> aborts
        print '(a,i0)', "unexpectedly accepted an invalid unit: ", v
    end subroutine scenario_temporal_invalid_time_unit

    !> parquet_timestamp: get on an instant whose civil year exceeds int32 aborts (to_string,
    !> which prints the year in full, still works for such instants -- get's int32 year cannot).
    subroutine scenario_temporal_ts_get_year_overflow()
        type(parquet_timestamp) :: ts
        integer(int32) :: y, mo, d, h, mi, s
        call ts%set_unix(huge(0_int64), parquet_unit_seconds)   ! year ~2.9e11 > huge(int32)
        call ts%get(y, mo, d, h, mi, s)   ! -> aborts
        print '(a,i0)', "unexpectedly returned an overflowed year: ", y
    end subroutine scenario_temporal_ts_get_year_overflow

    !> Writing a parquet_time value with sub-unit precision (nanoseconds) into a column
    !> declared time[ms] aborts -- this precision check lives entirely on the C++ side
    !> (build_time_array in parquet_wrapper.cpp), unlike parquet_timestamp's own to_unix, which
    !> already guards this at the Fortran level (see the temporal_ts_to_unix_precision_loss
    !> scenario) -- so this is genuinely separate coverage, not a duplicate.
    subroutine scenario_temporal_write_time_precision_loss()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_time) :: values(1)

        schema%maml%lines = [character(len=32) :: &
            "table: t", &
            "fields:", &
            "- name: clock", &
            "  data_type: time[ms]" ]
        call parquet_parse_maml(schema)
        call values(1)%set(12, 0, 0, 123456789)   ! full nanosecond precision, declared unit is ms

        call parquet_open_writer(writer, "test_run/error_scenario_time_precision_loss.parquet", schema)
        call parquet_write_column(writer, "clock", values)   ! -> aborts (C++ build_time_array)
        print '(a)', "unexpectedly wrote a sub-millisecond time value into a time[ms] column"
    end subroutine scenario_temporal_write_time_precision_loss

    !> Writing a null parquet_timestamp element into a MAML-protected column aborts, exactly
    !> like the existing (numeric) protected_cols enforcement -- exercised here for the temporal
    !> write path's own validity-gathering (temporal_valid_ptr in parquet_write.f90).
    subroutine scenario_temporal_protected_col_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: values(2)

        schema%maml%lines = [character(len=32) :: &
            "table: t", &
            "extra:", &
            "  protected_cols: ev", &
            "fields:", &
            "- name: ev", &
            "  data_type: timestamp" ]
        call parquet_parse_maml(schema)
        call values(1)%set(2024, 7, 16, 0, 0, 0)
        call values(2)%set_null()

        call parquet_open_writer(writer, "test_run/error_scenario_protected_null.parquet", schema)
        call parquet_write_column(writer, "ev", values)   ! -> aborts (protected column has a Null)
        print '(a)', "unexpectedly wrote a Null into a protected timestamp column"
    end subroutine scenario_temporal_protected_col_null

    !> Reading a DATE column via the plain int32 parquet_read_column entry point aborts (strict
    !> typing: a temporal column is only readable through its matching parquet_date/time/timestamp
    !> type, per CLAUDE.md's "Strict reads" decision).
    subroutine scenario_temporal_read_date_via_int32()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_date) :: dvals(1)
        integer(int32) :: ivals(1)
        character(len=*), parameter :: out_file = "test_run/error_scenario_read_date_via_int32.parquet"

        call dvals(1)%set(2024, 7, 16)
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "d", dvals)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "d", ivals)   ! -> aborts (type mismatch: expected int, got date)
        print '(a,i0)', "unexpectedly read a date column as int32: ", ivals(1)
    end subroutine scenario_temporal_read_date_via_int32

    !> The converse of the previous scenario: reading a plain int32 column via the
    !> parquet_date-typed entry point aborts too (strict typing both directions).
    subroutine scenario_temporal_read_int32_via_date()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ivals(1) = [42_int32]
        type(parquet_date) :: dvals(1)
        character(len=*), parameter :: out_file = "test_run/error_scenario_read_int32_via_date.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "n", ivals)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "n", dvals)   ! -> aborts (type mismatch: expected date, got int32)
        print '(a)', "unexpectedly read an int32 column as a date"
    end subroutine scenario_temporal_read_int32_via_date

    !> Reads back a legacy INT96-encoded timestamp column (nanosecond precision), built directly
    !> via parquet_debug_write_datetime_fixture since this library's own writer never produces
    !> INT96 -- see that hook's own comment in parquet_wrapper.cpp. Arrow decodes INT96 to
    !> timestamp[ns] transparently, so this exercises that read path plus the Null in row 3.
    !> A control scenario ("ok"-like: exits cleanly), asserting via error stop on any mismatch.
    subroutine scenario_temporal_foreign_int96_roundtrip()
        interface
            subroutine parquet_debug_write_datetime_fixture(path, column_name, variant) &
                    bind(C, name="parquet_debug_write_datetime_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char) :: path(*)
                character(kind=c_char) :: column_name(*)
                character(kind=c_char) :: variant(*)
            end subroutine parquet_debug_write_datetime_fixture
        end interface
        type(parquet_reader) :: reader
        type(parquet_timestamp) :: values(5)
        character(len=*), parameter :: out_file = "test_run/error_scenario_int96_fixture.parquet"
        integer :: unit
        integer(int64) :: ns_expected(5) = [1615714013123456789_int64, 1647250013123456789_int64, &
            0_int64, 1710408413123456789_int64, 1741944413123456789_int64]
        integer :: i

        call parquet_debug_write_datetime_fixture(out_file//char(0), "ev"//char(0), "int96"//char(0))

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "ev", values)
        call parquet_get_column_time_info(reader, "ev", unit=unit)
        call parquet_close_reader(reader)

        if (unit /= parquet_unit_nanos) error stop "INT96 fixture: expected unit=nanos"
        do i = 1, 5
            if (i == 3) then
                if (.not. values(i)%is_null()) error stop "INT96 fixture: row 3 should be Null"
            else
                if (values(i)%is_null()) error stop "INT96 fixture: unexpected Null"
                if (values(i)%to_unix(parquet_unit_nanos) /= ns_expected(i)) &
                    error stop "INT96 fixture: value mismatch"
            end if
        end do
    end subroutine scenario_temporal_foreign_int96_roundtrip

    !> Reads back a real, non-UTC IANA timezone ("America/New_York") -- this library's own
    !> writer only ever produces UTC or naive (no explicit tz), so this exercises the general
    !> tz-string path of parquet_get_column_time_info against a file from another Arrow-based
    !> tool. Control scenario, same assert-via-error-stop convention as the INT96 one above.
    subroutine scenario_temporal_foreign_tz_roundtrip()
        interface
            subroutine parquet_debug_write_datetime_fixture(path, column_name, variant) &
                    bind(C, name="parquet_debug_write_datetime_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char) :: path(*)
                character(kind=c_char) :: column_name(*)
                character(kind=c_char) :: variant(*)
            end subroutine parquet_debug_write_datetime_fixture
        end interface
        type(parquet_reader) :: reader
        type(parquet_timestamp) :: values(5)
        character(len=*), parameter :: out_file = "test_run/error_scenario_tz_fixture.parquet"
        integer :: unit
        character(len=:), allocatable :: tz

        call parquet_debug_write_datetime_fixture(out_file//char(0), "ev"//char(0), "tz"//char(0))

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "ev", values)
        call parquet_get_column_time_info(reader, "ev", unit=unit, timezone=tz)
        call parquet_close_reader(reader)

        if (unit /= parquet_unit_micros) error stop "tz fixture: expected unit=micros"
        if (tz /= "America/New_York") error stop "tz fixture: expected tz='America/New_York', got '"//tz//"'"
        if (.not. values(3)%is_null()) error stop "tz fixture: row 3 should be Null"
        if (values(1)%is_null()) error stop "tz fixture: row 1 should not be Null"
    end subroutine scenario_temporal_foreign_tz_roundtrip

    !> get_col_size/flatten_for_stats/parquet_reader_get_string_length's
    !> LIST/LARGE_LIST branches -- this library's own writer only ever emits FIXED_SIZE_LIST for
    !> vector columns, so plain LIST/LARGE_LIST columns only exist in foreign-written files, built
    !> here via parquet_debug_write_list_fixture (see that hook's own comment in
    !> parquet_wrapper.cpp for what each variant contains). Control scenario, same
    !> assert-via-error-stop convention as the temporal foreign-fixture scenarios above.
    subroutine scenario_list_type_foreign_fixture()
        interface
            subroutine parquet_debug_write_list_fixture(path, variant) &
                    bind(C, name="parquet_debug_write_list_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char) :: path(*)
                character(kind=c_char) :: variant(*)
            end subroutine parquet_debug_write_list_fixture
        end interface
        type(parquet_reader) :: reader
        integer :: col_size, large_col_size, strlen_lst, strlen_large_lst
        character(len=*), parameter :: mismatch_file = "test_run/error_scenario_list_mismatch.parquet"
        character(len=*), parameter :: empty_file = "test_run/error_scenario_list_empty.parquet"
        character(len=*), parameter :: strings_file = "test_run/error_scenario_list_strings.parquet"

        ! "mismatch": row widths differ (0, 2, 3 elements) -- get_col_size's heterogeneous-width
        ! branch, both list kinds.
        call parquet_debug_write_list_fixture(mismatch_file//char(0), "mismatch"//char(0))
        call parquet_open_reader(reader, mismatch_file)
        call parquet_get_col_size(reader, "lst", col_size)
        call parquet_get_col_size(reader, "large_lst", large_col_size)
        call parquet_close_reader(reader)
        if (col_size /= 1) error stop "list fixture: mismatched-width LIST column should report col_size=1"
        if (large_col_size /= 1) error stop "list fixture: mismatched-width LARGE_LIST column should report col_size=1"

        ! "empty": both columns have zero rows -- get_col_size's whole-array-empty branch, both
        ! list kinds (distinct from an individual row's list being empty, already exercised above).
        call parquet_debug_write_list_fixture(empty_file//char(0), "empty"//char(0))
        call parquet_open_reader(reader, empty_file)
        call parquet_get_col_size(reader, "lst", col_size)
        call parquet_get_col_size(reader, "large_lst", large_col_size)
        call parquet_close_reader(reader)
        if (col_size /= 0) error stop "list fixture: zero-row LIST column should report col_size=0"
        if (large_col_size /= 0) error stop "list fixture: zero-row LARGE_LIST column should report col_size=0"

        ! "strings": uniform-width LIST<utf8>/LARGE_LIST<utf8> with a Null element -- exercises
        ! parquet_get_string_length's LIST/LARGE_LIST branches (longest non-null value is
        ! "charlie", 7 bytes) plus flatten_for_stats's LIST branch via print_stat.
        call parquet_debug_write_list_fixture(strings_file//char(0), "strings"//char(0))
        call parquet_open_reader(reader, strings_file)
        call parquet_prefetch_columns(reader, "lst_str,large_lst_str")
        call parquet_get_string_length(reader, "lst_str", strlen_lst)
        call parquet_get_string_length(reader, "large_lst_str", strlen_large_lst)
        call parquet_close_reader(reader, print_stat=.true.)
        if (strlen_lst /= 7) error stop "list fixture: LIST<utf8> get_string_length expected 7 ('charlie')"
        if (strlen_large_lst /= 7) error stop "list fixture: LARGE_LIST<utf8> get_string_length expected 7 ('charlie')"
    end subroutine scenario_list_type_foreign_fixture

    !> schema%add_field rejects "date" with a unit/utc suffix -- date is unitless (see
    !> parquet_parse_temporal_type in parquet_metadata.f90).
    subroutine scenario_schema_add_field_date_with_unit()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "date[us]")
        print '(a)', "unexpectedly added a date field with a unit suffix without error"
    end subroutine scenario_schema_add_field_date_with_unit

    !> parquet_validate_maml rejects qc: min:/max: on a date/time/timestamp field -- qc is not
    !> supported for temporal columns yet (deferred scope, see feature_temporal.md).
    subroutine scenario_validate_qc_on_temporal_column()
        type(parquet_maml_file) :: maml

        maml%lines = [character(len=32) :: &
            "table: t", &
            "fields:", &
            "- name: ev", &
            "  data_type: timestamp", &
            "  qc:", &
            "    min: '>=0'" ]
        call parquet_validate_maml(maml)
        print '(a)', "unexpectedly accepted qc: on a timestamp field"
    end subroutine scenario_validate_qc_on_temporal_column

    !> schema%add_field rejects "timestamp[s]"/an explicit seconds unit -- Parquet's physical
    !> format has no seconds-resolution TIME/TIMESTAMP encoding (only MILLIS/MICROS/NANOS), so
    !> honoring a declared "s" unit is impossible; see apply_temporal_unit_token's own comment
    !> in parquet_metadata.f90 for the silent-mismatch bug this specifically prevents.
    subroutine scenario_schema_add_field_seconds_unit_rejected()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "timestamp[s]")
        print '(a)', "unexpectedly added a timestamp[s] field without error"
    end subroutine scenario_schema_add_field_seconds_unit_rejected

    !> schema%add_field rejects "time[utc]" -- the ",utc" suffix is only meaningful for
    !> "timestamp" (a UTC-adjusted instant); "time" (a time-of-day with no date part) has no
    !> timezone concept, so this exercises apply_temporal_unit_token's allow_utc=.false. branch.
    subroutine scenario_schema_add_field_time_utc_rejected()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "time[ms,utc]")
        print '(a)', "unexpectedly added a time[ms,utc] field without error"
    end subroutine scenario_schema_add_field_time_utc_rejected

    !> schema%add_field rejects a malformed/unclosed unit bracket ("time[ms", no closing "]")
    !> -- exercises parse_temporal_suffix's has_bracket-but-not-closed guard in
    !> parquet_metadata.f90.
    subroutine scenario_schema_add_field_unclosed_bracket_rejected()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "time[ms")
        print '(a)', "unexpectedly added a time[ms (unclosed bracket) field without error"
    end subroutine scenario_schema_add_field_unclosed_bracket_rejected

    !> parquet_date: operator(==) with a null operand aborts (distinct from operator(<), which
    !> is separately tested by scenario_temporal_date_null_comparison -- date_le/gt/ge delegate
    !> to date_lt internally, and date_ne delegates to date_eq, so == and < together cover all
    !> six operators' own guards).
    subroutine scenario_temporal_date_eq_null()
        type(parquet_date) :: a, b
        logical :: res
        call a%set(2024, 7, 16)   ! b stays null
        res = a == b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null date: ", res
    end subroutine scenario_temporal_date_eq_null

    !> parquet_time: get on a null element aborts (distinct from %hour(), already tested by
    !> scenario_temporal_time_null_get).
    subroutine scenario_temporal_time_get_null()
        type(parquet_time) :: t   ! default-initialized -> null
        integer(int32) :: h, mi, s
        call t%get(h, mi, s)   ! null -> aborts
        print '(a,3i0)', "unexpectedly read a null time: ", h, mi, s
    end subroutine scenario_temporal_time_get_null

    subroutine scenario_temporal_time_minute_null()
        type(parquet_time) :: t
        integer(int32) :: mi
        mi = t%minute()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null time minute: ", mi
    end subroutine scenario_temporal_time_minute_null

    subroutine scenario_temporal_time_second_null()
        type(parquet_time) :: t
        integer(int32) :: s
        s = t%second()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null time second: ", s
    end subroutine scenario_temporal_time_second_null

    subroutine scenario_temporal_time_nanosecond_null()
        type(parquet_time) :: t
        integer(int32) :: ns
        ns = t%nanosecond()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null time nanosecond: ", ns
    end subroutine scenario_temporal_time_nanosecond_null

    !> parquet_time: operator(==) with a null operand aborts.
    subroutine scenario_temporal_time_eq_null()
        type(parquet_time) :: a, b
        logical :: res
        call a%set(12, 0, 0)   ! b stays null
        res = a == b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null time: ", res
    end subroutine scenario_temporal_time_eq_null

    !> parquet_time: operator(<) with a null operand aborts.
    subroutine scenario_temporal_time_lt_null()
        type(parquet_time) :: a, b
        logical :: res
        call a%set(12, 0, 0)   ! b stays null
        res = a < b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null time: ", res
    end subroutine scenario_temporal_time_lt_null

    subroutine scenario_temporal_date_year_null()
        type(parquet_date) :: d
        integer(int32) :: y
        y = d%year()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null date year: ", y
    end subroutine scenario_temporal_date_year_null

    subroutine scenario_temporal_date_month_null()
        type(parquet_date) :: d
        integer(int32) :: m
        m = d%month()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null date month: ", m
    end subroutine scenario_temporal_date_month_null

    subroutine scenario_temporal_date_day_null()
        type(parquet_date) :: d
        integer(int32) :: dd
        dd = d%day()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null date day: ", dd
    end subroutine scenario_temporal_date_day_null

    subroutine scenario_temporal_date_to_mjd_null()
        type(parquet_date) :: d
        integer(int64) :: m
        m = d%to_mjd()   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null date to_mjd: ", m
    end subroutine scenario_temporal_date_to_mjd_null

    subroutine scenario_temporal_date_to_string_null()
        type(parquet_date) :: d
        character(len=:), allocatable :: s
        call d%to_string(s)   ! null -> aborts
        print '(a)', "unexpectedly formatted a null date: "//s
    end subroutine scenario_temporal_date_to_string_null

    subroutine scenario_temporal_time_to_string_null()
        type(parquet_time) :: t
        character(len=:), allocatable :: s
        call t%to_string(s)   ! null -> aborts
        print '(a)', "unexpectedly formatted a null time: "//s
    end subroutine scenario_temporal_time_to_string_null

    !> parquet_timestamp: set with a month outside 1..12 aborts (distinct from the day check,
    !> already tested by scenario_temporal_ts_set_invalid_day).
    subroutine scenario_temporal_ts_set_invalid_month()
        type(parquet_timestamp) :: ts
        call ts%set(2024, 13, 1, 0, 0, 0)   ! month 13 -> aborts
        print '(a)', "unexpectedly accepted month 13"
    end subroutine scenario_temporal_ts_set_invalid_month

    subroutine scenario_temporal_ts_set_invalid_nanosecond()
        type(parquet_timestamp) :: ts
        call ts%set(2024, 7, 16, 0, 0, 0, 1000000000)   ! 1e9 ns -> aborts
        print '(a)', "unexpectedly accepted nanosecond 1000000000"
    end subroutine scenario_temporal_ts_set_invalid_nanosecond

    subroutine scenario_temporal_ts_get_null()
        type(parquet_timestamp) :: ts
        integer(int32) :: y, mo, d, h, mi, s
        call ts%get(y, mo, d, h, mi, s)   ! null -> aborts
        print '(a,i0)', "unexpectedly read a null timestamp: ", y
    end subroutine scenario_temporal_ts_get_null

    !> parquet_timestamp%get_date aborts if the date part falls outside parquet_date's
    !> representable range (+-5.8 million years) -- reachable since parquet_timestamp itself
    !> spans a far larger range (+-292 billion years) than parquet_date can.
    subroutine scenario_temporal_ts_get_date_range_exceeded()
        type(parquet_timestamp) :: ts
        type(parquet_date) :: d
        call ts%set_unix(huge(0_int64), parquet_unit_seconds)   ! year ~2.9e11, far beyond parquet_date
        d = ts%get_date()   ! -> aborts
        print '(a,i0)', "unexpectedly converted an out-of-range date part: ", d%raw()
    end subroutine scenario_temporal_ts_get_date_range_exceeded

    !> parquet_timestamp%to_unix overflow with a large-magnitude NEGATIVE (far pre-epoch)
    !> instant -- distinct code branch from scenario_temporal_ts_to_unix_overflow, which only
    !> exercises the positive-seconds overflow check.
    subroutine scenario_temporal_ts_to_unix_overflow_negative()
        type(parquet_timestamp) :: ts
        integer(int64) :: v
        call ts%set_raw(-9223372037_int64, 0)   ! seconds < INT64_MIN/1e9 -> overflows in nanos
        v = ts%to_unix(parquet_unit_nanos)   ! -> aborts
        print '(a,i0)', "unexpectedly overflowed silently (negative branch): ", v
    end subroutine scenario_temporal_ts_to_unix_overflow_negative

    subroutine scenario_temporal_ts_to_mjd_null()
        type(parquet_timestamp) :: ts
        real(real64) :: m
        m = ts%to_mjd()   ! null -> aborts
        print '(a,f0.3)', "unexpectedly read a null timestamp to_mjd: ", m
    end subroutine scenario_temporal_ts_to_mjd_null

    subroutine scenario_temporal_ts_to_string_null()
        type(parquet_timestamp) :: ts
        character(len=:), allocatable :: s
        call ts%to_string(s)   ! null -> aborts
        print '(a)', "unexpectedly formatted a null timestamp: "//s
    end subroutine scenario_temporal_ts_to_string_null

    !> parquet_timestamp: operator(<) with a null operand aborts (distinct from operator(==),
    !> already tested by scenario_temporal_ts_null_comparison).
    subroutine scenario_temporal_ts_lt_null()
        type(parquet_timestamp) :: a, b
        logical :: res
        call a%set(2024, 7, 16, 0, 0, 0)   ! b stays null
        res = a < b   ! null operand -> aborts
        print '(a,l1)', "unexpectedly compared against a null timestamp: ", res
    end subroutine scenario_temporal_ts_lt_null

    !> parquet_date: operator(-) between two dates with a null operand aborts.
    subroutine scenario_temporal_date_diff_null()
        type(parquet_date) :: a, b
        integer(int64) :: n
        call a%set(2024, 7, 16)   ! b stays null
        n = a - b   ! null operand -> aborts
        print '(a,i0)', "unexpectedly diffed against a null date: ", n
    end subroutine scenario_temporal_date_diff_null

    !> parquet_date: day-offset operator(+) with a null operand aborts.
    subroutine scenario_temporal_date_offset_null()
        type(parquet_date) :: d, c   ! d stays null
        c = d + 1_int32   ! null operand -> aborts
        print '(a,i0)', "unexpectedly shifted a null date, raw = ", c%raw()
    end subroutine scenario_temporal_date_offset_null

    !> parquet_date: day-offset arithmetic whose result falls outside the representable
    !> +-5.8 million year range aborts (the domain range check, not the int64-safety guards).
    subroutine scenario_temporal_date_offset_out_of_range()
        type(parquet_date) :: d, c
        call d%set_raw(0_int32)
        c = d + (int(huge(0_int32), int64) + 1_int64)   ! one day beyond DATE_DAYS_MAX -> aborts
        print '(a,i0)', "unexpectedly produced an out-of-range date, raw = ", c%raw()
    end subroutine scenario_temporal_date_offset_out_of_range

    !> parquet_date: a day offset whose magnitude would overflow the int64 addition itself
    !> (self%days + delta), positive branch, aborts before the domain range check ever runs.
    subroutine scenario_temporal_date_offset_int64_overflow_positive()
        type(parquet_date) :: d, c
        call d%set_raw(100_int32)
        c = d + huge(0_int64)   ! self%days + delta would overflow int64 -> aborts
        print '(a,i0)', "unexpectedly survived an int64-overflowing date offset, raw = ", c%raw()
    end subroutine scenario_temporal_date_offset_int64_overflow_positive

    !> parquet_date: same as above, negative branch (a large negative offset).
    subroutine scenario_temporal_date_offset_int64_overflow_negative()
        type(parquet_date) :: d, c
        call d%set_raw(-100_int32)
        c = d + (-huge(0_int64) - 1_int64)   ! self%days + delta would underflow int64 -> aborts
        print '(a,i0)', "unexpectedly survived an int64-underflowing date offset, raw = ", c%raw()
    end subroutine scenario_temporal_date_offset_int64_overflow_negative

    !> parquet_date: operator(-) with an int64 offset of exactly INT64_MIN aborts up front
    !> (negating it to reuse operator(+) would itself overflow int64).
    subroutine scenario_temporal_date_sub_int64_min()
        type(parquet_date) :: d, c
        call d%set(2024, 7, 16)
        c = d - (-huge(0_int64) - 1_int64)   ! n == INT64_MIN -> aborts
        print '(a,i0)', "unexpectedly survived subtracting INT64_MIN from a date, raw = ", c%raw()
    end subroutine scenario_temporal_date_sub_int64_min

    !> parquet_time: operator(-) between two times with a null operand aborts.
    subroutine scenario_temporal_time_diff_null()
        type(parquet_time) :: a, b
        integer(int64) :: n
        call a%set(12, 0, 0)   ! b stays null
        n = a - b   ! null operand -> aborts
        print '(a,i0)', "unexpectedly diffed against a null time: ", n
    end subroutine scenario_temporal_time_diff_null

    !> parquet_time: ns-offset operator(+) with a null operand aborts.
    subroutine scenario_temporal_time_offset_null()
        type(parquet_time) :: t, c   ! t stays null
        c = t + 1_int32   ! null operand -> aborts
        print '(a,i0)', "unexpectedly shifted a null time, raw = ", c%raw()
    end subroutine scenario_temporal_time_offset_null

    !> parquet_time: operator(+) with an offset exceeding 24h of nanoseconds aborts.
    subroutine scenario_temporal_time_offset_magnitude_add()
        type(parquet_time) :: t, c
        call t%set(12, 0, 0)
        c = t + 86400000000001_int64   ! > 86400e9 ns -> aborts
        print '(a,i0)', "unexpectedly survived a >24h time offset (+), raw = ", c%raw()
    end subroutine scenario_temporal_time_offset_magnitude_add

    !> parquet_time: operator(-) with an offset exceeding 24h of nanoseconds aborts (the guard
    !> is checked on the raw, not-yet-negated input, so this is a distinct source line from the
    !> operator(+) case above).
    subroutine scenario_temporal_time_offset_magnitude_sub()
        type(parquet_time) :: t, c
        call t%set(12, 0, 0)
        c = t - 86400000000001_int64   ! > 86400e9 ns -> aborts
        print '(a,i0)', "unexpectedly survived a >24h time offset (-), raw = ", c%raw()
    end subroutine scenario_temporal_time_offset_magnitude_sub

    !> parquet_timestamp: operator(-) between two timestamps with a null operand aborts.
    subroutine scenario_temporal_ts_diff_ns_null()
        type(parquet_timestamp) :: a, b
        integer(int64) :: n
        call a%set(2024, 7, 16, 0, 0, 0)   ! b stays null
        n = a - b   ! null operand -> aborts
        print '(a,i0)', "unexpectedly diffed against a null timestamp: ", n
    end subroutine scenario_temporal_ts_diff_ns_null

    !> parquet_timestamp: operator(-) between two instants more than ~292.3 years apart aborts
    !> (the elapsed time cannot be represented as an int64 nanosecond count).
    subroutine scenario_temporal_ts_diff_ns_overflow()
        type(parquet_timestamp) :: a, b
        integer(int64) :: n
        call a%set_raw(9223372036_int64, 0)
        call b%set_raw(0_int64, 0)
        n = a - b   ! elapsed time beyond ~292.3 years -> aborts
        print '(a,i0)', "unexpectedly diffed two far-apart timestamps as ns: ", n
    end subroutine scenario_temporal_ts_diff_ns_overflow

    !> parquet_timestamp: diff_seconds with a null operand aborts (unlike operator(-), it never
    !> aborts on magnitude -- only on a null operand).
    subroutine scenario_temporal_ts_diff_seconds_null()
        type(parquet_timestamp) :: a, b
        real(real64) :: v
        call a%set(2024, 7, 16, 0, 0, 0)   ! b stays null
        v = a%diff_seconds(b)   ! null operand -> aborts
        print '(a,f0.3)', "unexpectedly diffed against a null timestamp (seconds): ", v
    end subroutine scenario_temporal_ts_diff_seconds_null

    !> parquet_timestamp: ns-offset operator(+) with a null operand aborts.
    subroutine scenario_temporal_ts_offset_null()
        type(parquet_timestamp) :: ts, c   ! ts stays null
        c = ts + 1_int32   ! null operand -> aborts
        print '(a,i0)', "unexpectedly shifted a null timestamp, raw seconds = ", c%to_unix(parquet_unit_seconds)
    end subroutine scenario_temporal_ts_offset_null

    !> parquet_timestamp: an ns offset so large it would overflow the int64 addition of
    !> self%nanoseconds + offset itself (before any carry into seconds) aborts.
    subroutine scenario_temporal_ts_offset_ns_overflow()
        type(parquet_timestamp) :: ts, c
        call ts%set(2024, 7, 16, 0, 0, 0, 999999999)
        c = ts + (huge(0_int64) - 500000000_int64)   ! nanoseconds + offset would overflow int64 -> aborts
        print '(a,i0)', "unexpectedly survived an int64-overflowing ts ns offset, raw seconds = ", &
            c%to_unix(parquet_unit_seconds)
    end subroutine scenario_temporal_ts_offset_ns_overflow

    !> parquet_timestamp: an ns offset that carries into a seconds value beyond int64 range
    !> aborts (positive branch: self%seconds is near huge(int64) and the carry pushes it over).
    subroutine scenario_temporal_ts_offset_seconds_overflow_positive()
        type(parquet_timestamp) :: ts, c
        call ts%set_raw(huge(0_int64) - 100_int64, 0)
        c = ts + (huge(0_int64) - 500000000_int64)   ! seconds carry would overflow int64 -> aborts
        print '(a,i0)', "unexpectedly survived an int64-overflowing ts seconds carry, raw seconds = ", &
            c%to_unix(parquet_unit_seconds)
    end subroutine scenario_temporal_ts_offset_seconds_overflow_positive

    !> parquet_timestamp: same as above, negative branch (self%seconds is near INT64_MIN and a
    !> large negative offset carries it under).
    subroutine scenario_temporal_ts_offset_seconds_overflow_negative()
        type(parquet_timestamp) :: ts, c
        call ts%set_raw(-huge(0_int64) - 1_int64 + 100_int64, 0)
        c = ts - (huge(0_int64) - 500000000_int64)   ! seconds carry would underflow int64 -> aborts
        print '(a,i0)', "unexpectedly survived an int64-underflowing ts seconds carry, raw seconds = ", &
            c%to_unix(parquet_unit_seconds)
    end subroutine scenario_temporal_ts_offset_seconds_overflow_negative

    !> parquet_timestamp: operator(-) with an int64 offset of exactly INT64_MIN aborts up front
    !> (negating it to reuse the shared offset worker would itself overflow int64).
    subroutine scenario_temporal_ts_sub_int64_min()
        type(parquet_timestamp) :: ts, c
        call ts%set(2024, 7, 16, 0, 0, 0)
        c = ts - (-huge(0_int64) - 1_int64)   ! n == INT64_MIN -> aborts
        print '(a,i0)', "unexpectedly survived subtracting INT64_MIN from a timestamp, raw seconds = ", &
            c%to_unix(parquet_unit_seconds)
    end subroutine scenario_temporal_ts_sub_int64_min

    !> Writing a temporal column not declared in the schema aborts (parquet_write_column's
    !> schema-enforcement preamble, temporal_write_preamble in parquet_write.f90).
    subroutine scenario_temporal_write_column_not_defined()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_date) :: values(1)

        call schema%init(table="t")
        call schema%add_field("day", "date")
        call parquet_parse_maml(schema)
        call values(1)%set(2024, 7, 16)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_write_undefined.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", values)   ! -> aborts
        print '(a)', "unexpectedly wrote an undeclared temporal column"
    end subroutine scenario_temporal_write_column_not_defined

    !> Writing a temporal vector column whose col_size doesn't match the schema's declared
    !> col_size aborts.
    subroutine scenario_temporal_write_array_size_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: values(3, 1)

        call schema%init(table="t")
        call schema%add_field("ev", "timestamp", col_size=2)
        call parquet_parse_maml(schema)
        call values(1,1)%set(2024, 7, 16, 0, 0, 0)
        call values(2,1)%set(2024, 7, 16, 0, 0, 1)
        call values(3,1)%set(2024, 7, 16, 0, 0, 2)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_write_size_mismatch.parquet", schema)
        call parquet_write_column(writer, "ev", values)   ! col_size 3 != declared 2 -> aborts
        print '(a)', "unexpectedly wrote a mismatched-col_size temporal column"
    end subroutine scenario_temporal_write_array_size_mismatch

    !> Writing a parquet_timestamp array against a column the schema declares as "date" aborts
    !> (exact data_type match is required, same as any other type).
    subroutine scenario_temporal_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: values(1)

        call schema%init(table="t")
        call schema%add_field("day", "date")
        call parquet_parse_maml(schema)
        call values(1)%set(2024, 7, 16, 0, 0, 0)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_write_type_mismatch.parquet", schema)
        call parquet_write_column(writer, "day", values)   ! timestamp values into a date column -> aborts
        print '(a)', "unexpectedly wrote a timestamp array into a date column"
    end subroutine scenario_temporal_write_type_mismatch

    !> Streaming-chunk counterpart of scenario_temporal_write_column_not_defined (exercises
    !> temporal_chunk_preamble, not temporal_write_preamble).
    subroutine scenario_temporal_chunk_column_not_defined()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_date) :: values(1)

        call schema%init(table="t")
        call schema%add_field("day", "date")
        call parquet_parse_maml(schema)
        call values(1)%set(2024, 7, 16)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_chunk_undefined.parquet", schema)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "not_a_real_column", values)   ! -> aborts
        print '(a)', "unexpectedly chunk-wrote an undeclared temporal column"
    end subroutine scenario_temporal_chunk_column_not_defined

    !> Streaming-chunk counterpart of scenario_temporal_write_array_size_mismatch.
    subroutine scenario_temporal_chunk_array_size_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: values(3, 1)

        call schema%init(table="t")
        call schema%add_field("ev", "timestamp", col_size=2)
        call parquet_parse_maml(schema)
        call values(1,1)%set(2024, 7, 16, 0, 0, 0)
        call values(2,1)%set(2024, 7, 16, 0, 0, 1)
        call values(3,1)%set(2024, 7, 16, 0, 0, 2)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_chunk_size_mismatch.parquet", schema)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "ev", values)   ! col_size 3 != declared 2 -> aborts
        print '(a)', "unexpectedly chunk-wrote a mismatched-col_size temporal column"
    end subroutine scenario_temporal_chunk_array_size_mismatch

    !> Streaming-chunk counterpart of scenario_temporal_write_type_mismatch.
    subroutine scenario_temporal_chunk_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: values(1)

        call schema%init(table="t")
        call schema%add_field("day", "date")
        call parquet_parse_maml(schema)
        call values(1)%set(2024, 7, 16, 0, 0, 0)

        call parquet_open_writer(writer, "test_run/error_scenario_temporal_chunk_type_mismatch.parquet", schema)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "day", values)   ! timestamp values into a date column -> aborts
        print '(a)', "unexpectedly chunk-wrote a timestamp array into a date column"
    end subroutine scenario_temporal_chunk_type_mismatch

    ! ==================================================================================
    ! Row-filtering ("mask") scenarios -- parquet_write_row_mask/parquet_write_chunk_row_mask.
    ! See feature_write_mask.md for the full design this implements.
    ! ==================================================================================

    !> parquet_write_row_mask must be called before the writer's first write/row group.
    subroutine scenario_mask_row_mask_after_write_started()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: mask(2) = [.true., .false.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_after_started.parquet")
        call parquet_write_column(writer, "id", values)
        call parquet_write_row_mask(writer, mask)   ! -> aborts, writer already started
        print '(a)', "unexpectedly accepted parquet_write_row_mask after the writer had started"
    end subroutine scenario_mask_row_mask_after_write_started

    !> A whole-column write's row count must match the file_mask's own size exactly.
    subroutine scenario_mask_row_mask_shape_mismatch()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: mask(3) = [.true., .false., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_shape_mismatch.parquet")
        call parquet_write_row_mask(writer, mask)
        call parquet_write_column(writer, "id", values)   ! -> aborts, 2 values vs. mask of 3
        print '(a)', "unexpectedly wrote a column whose row count didn't match the mask"
    end subroutine scenario_mask_row_mask_shape_mismatch

    !> parquet_write_row_mask rejects a zero-length mask.
    subroutine scenario_mask_row_mask_zero_length()
        type(parquet_writer) :: writer
        logical, allocatable :: mask(:)

        allocate(mask(0))
        call parquet_open_writer(writer, "test_run/error_scenario_mask_zero_length.parquet")
        call parquet_write_row_mask(writer, mask)   ! -> aborts
        print '(a)', "unexpectedly accepted a zero-length parquet_write_row_mask mask"
    end subroutine scenario_mask_row_mask_zero_length

    !> parquet_write_row_mask can only be set once per writer -- a second call, even before any
    !> write has happened, must not silently overwrite the first mask.
    subroutine scenario_mask_row_mask_called_twice()
        type(parquet_writer) :: writer
        logical :: mask_a(2) = [.true., .false.]
        logical :: mask_b(2) = [.false., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_row_mask_called_twice.parquet")
        call parquet_write_row_mask(writer, mask_a)
        call parquet_write_row_mask(writer, mask_b)   ! -> aborts, already set
        print '(a)', "unexpectedly accepted a second parquet_write_row_mask call for the same writer"
    end subroutine scenario_mask_row_mask_called_twice

    !> parquet_write_chunk_row_mask is unavailable once parquet_write_row_mask has been used.
    subroutine scenario_mask_chunk_row_mask_after_row_mask()
        type(parquet_writer) :: writer
        logical :: file_mask(2) = [.true., .true.]
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_chunk_after_row.parquet")
        call parquet_write_row_mask(writer, file_mask)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts
        print '(a)', "unexpectedly accepted parquet_write_chunk_row_mask after parquet_write_row_mask"
    end subroutine scenario_mask_chunk_row_mask_after_row_mask

    !> parquet_write_row_mask is unavailable once the writer has committed to per-row-group
    !> masking (ordering already blocks this via write_started, exercised here via new_row_group).
    subroutine scenario_mask_row_mask_after_chunk_row_mask()
        type(parquet_writer) :: writer
        logical :: chunk_mask(2) = [.true., .true.]
        logical :: file_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_row_after_chunk.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)
        call parquet_write_row_mask(writer, file_mask)   ! -> aborts, writer already started
        print '(a)', "unexpectedly accepted parquet_write_row_mask after the writer had started row groups"
    end subroutine scenario_mask_row_mask_after_chunk_row_mask

    !> parquet_write_chunk_row_mask is unavailable once any column has been written whole.
    subroutine scenario_mask_chunk_row_mask_after_whole_column()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_chunk_after_whole.parquet")
        call parquet_write_column(writer, "id", values)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts
        print '(a)', "unexpectedly accepted parquet_write_chunk_row_mask after a whole-column write"
    end subroutine scenario_mask_chunk_row_mask_after_whole_column

    !> Once parquet_write_chunk_row_mask is used for a writer's first row group, it must be used
    !> for every subsequent row group too.
    subroutine scenario_mask_chunk_row_mask_not_used_every_group()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_not_every_group.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)
        call parquet_write_column_chunk(writer, "id", values)
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)   ! -> aborts, no chunk_row_mask this group
        print '(a)', "unexpectedly accepted a row group without parquet_write_chunk_row_mask after an earlier one used it"
    end subroutine scenario_mask_chunk_row_mask_not_used_every_group

    !> parquet_write_chunk_row_mask must be called before this row group's first
    !> parquet_write_column_chunk call.
    subroutine scenario_mask_chunk_row_mask_introduced_late()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_introduced_late.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts, too late for this group
        print '(a)', "unexpectedly accepted parquet_write_chunk_row_mask after this row group's first chunk write"
    end subroutine scenario_mask_chunk_row_mask_introduced_late

    !> parquet_write_chunk_row_mask can only be set once per row group -- a second call for the
    !> same still-open row group must not be silently accepted.
    subroutine scenario_mask_chunk_row_mask_called_twice()
        type(parquet_writer) :: writer
        logical :: chunk_mask_a(2) = [.true., .true.]
        logical :: chunk_mask_b(2) = [.true., .false.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_chunk_row_mask_called_twice.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask_a)
        call parquet_write_chunk_row_mask(writer, chunk_mask_b)   ! -> aborts, already set for this row group
        print '(a)', "unexpectedly accepted a second parquet_write_chunk_row_mask call for the same row group"
    end subroutine scenario_mask_chunk_row_mask_called_twice

    !> parquet_write_chunk_row_mask's mask must be exactly the open row group's own nrows long.
    subroutine scenario_mask_chunk_row_mask_size_mismatch()
        type(parquet_writer) :: writer
        logical :: chunk_mask(3) = [.true., .true., .false.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_chunk_size_mismatch.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts, mask is 3 long, row group is 2
        print '(a)', "unexpectedly accepted a parquet_write_chunk_row_mask mask of the wrong size"
    end subroutine scenario_mask_chunk_row_mask_size_mismatch

    !> A row group's nrows window must not claim more positions than the shared file_mask has left.
    subroutine scenario_mask_row_mask_window_exhausted()
        type(parquet_writer) :: writer
        logical :: file_mask(3) = [.true., .true., .false.]
        integer(int32) :: values(2) = [1_int32, 2_int32]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_window_exhausted.parquet")
        call parquet_write_row_mask(writer, file_mask)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2_int64)   ! -> aborts, only 1 position left in file_mask
        print '(a)', "unexpectedly claimed more mask positions than parquet_write_row_mask provided"
    end subroutine scenario_mask_row_mask_window_exhausted

    !> parquet_close_writer aborts if the shared file_mask (parquet_write_row_mask) was not fully
    !> consumed by the writer's row groups.
    subroutine scenario_mask_row_mask_not_fully_consumed()
        type(parquet_writer) :: writer
        logical :: file_mask(3) = [.true., .true., .false.]
        integer(int32) :: values(2) = [1_int32, 2_int32]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_not_fully_consumed.parquet")
        call parquet_write_row_mask(writer, file_mask)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)
        call parquet_finish_row_group(writer)

        call parquet_close_writer(writer)   ! -> aborts, 1 mask position never claimed
        print '(a)', "unexpectedly closed a writer with an unconsumed parquet_write_row_mask tail"
    end subroutine scenario_mask_row_mask_not_fully_consumed

    !> parquet_write_chunk_row_mask requires an open row group.
    subroutine scenario_mask_chunk_row_mask_no_row_group_open()
        type(parquet_writer) :: writer
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_no_row_group_open.parquet")
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts, no row group open yet
        print '(a)', "unexpectedly accepted parquet_write_chunk_row_mask with no row group open"
    end subroutine scenario_mask_chunk_row_mask_no_row_group_open

    !> Once a writer's first row group has *declined* per-row-group masking (no
    !> parquet_write_chunk_row_mask call before its first parquet_write_column_chunk), it can never
    !> be introduced for a later row group either -- the mirror image of
    !> scenario_mask_chunk_row_mask_not_used_every_group (which starts by *using* it).
    subroutine scenario_mask_chunk_row_mask_scheme_declined()
        type(parquet_writer) :: writer
        integer(int32) :: values(2) = [1_int32, 2_int32]
        logical :: chunk_mask(2) = [.true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_mask_scheme_declined.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "id", values)   ! no chunk_row_mask -- scheme decided: 2 (never)
        call parquet_finish_row_group(writer)

        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_chunk_row_mask(writer, chunk_mask)   ! -> aborts, scheme already declined
        print '(a)', "unexpectedly introduced parquet_write_chunk_row_mask after an earlier row group declined it"
    end subroutine scenario_mask_chunk_row_mask_scheme_declined

    !> A row group opened and finished with no column ever written at all (no mask, no chunk
    !> write) still aborts exactly as it did before masking existed ("no columns have been
    !> written"), via parquet_finish_row_group's own fallback commit of the (otherwise still
    !> deferred) underlying C++ row group.
    subroutine scenario_mask_row_group_no_writes_at_all()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/error_scenario_mask_no_writes_at_all.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_finish_row_group(writer)   ! -> aborts, nothing was ever written for this row group
        print '(a)', "unexpectedly finished a row group with no column ever written for it"
    end subroutine scenario_mask_row_group_no_writes_at_all

    !> parquet_get_version(mode=...) rejects any value other than "internal"/"arrow"/"parquet".
    subroutine scenario_get_version_invalid_mode()
        character(len=:), allocatable :: ver_string

        call parquet_get_version(ver_string, mode="bogus")
        print '(a)', "unexpectedly returned a version string for an invalid mode"
    end subroutine scenario_get_version_invalid_mode

    !> parquet_column_exists error stops on an unrecognized types= token (typo "itn32"), checked
    !> up front before the existence check itself -- see parquet_read.f90's
    !> type_filter_token_valid/split_type_filter_tokens.
    subroutine scenario_column_exists_bad_type_token()
        type(parquet_reader) :: reader
        logical :: exists

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        exists = parquet_column_exists(reader, "id_with_null", types="itn32")
        print '(a)', "unexpectedly accepted an unrecognized types= token without error"
    end subroutine scenario_column_exists_bad_type_token

    !> parquet_column_exists error stops if types= is given but blank/all-whitespace, rather than
    !> silently matching nothing.
    subroutine scenario_column_exists_empty_type_filter()
        type(parquet_reader) :: reader
        logical :: exists

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        exists = parquet_column_exists(reader, "id_with_null", types="   ")
        print '(a)', "unexpectedly accepted a blank types= filter without error"
    end subroutine scenario_column_exists_empty_type_filter

    !> parquet_get_column_type error stops on a column whose physical type falls outside the nine
    !> canonical tokens (valid_query_data_types) -- test/fixtures/extended_types.parquet's
    !> v_uint32 is UINT32, not one of int32/int64/float32/float64/boolean/string/date/time/
    !> timestamp. Unlike parquet_column_exists (which just reports .false. for this case, see
    !> test_column_exists_and_get_column_type in test_reading.f90), this procedure's whole
    !> contract is "give me the type", so it cannot return silently.
    subroutine scenario_get_column_type_unsupported()
        type(parquet_reader) :: reader
        character(len=:), allocatable :: type_name

        call parquet_open_reader(reader, "test/fixtures/extended_types.parquet")
        call parquet_get_column_type(reader, "v_uint32", type_name)
        print '(a)', "unexpectedly resolved a canonical type for a column outside the 9 recognized tokens"
    end subroutine scenario_get_column_type_unsupported

    !> A MAML col_size: value that is neither blank, "auto", nor a valid positive integer (a
    !> typo like "5O", letter-O for zero) must be rejected by parquet_validate_maml with a clear
    !> message, not silently coerced to a scalar column the way it used to be.
    subroutine scenario_col_size_malformed_value()
        type(parquet_schema) :: schema

        schema%maml%name = "col_size_malformed.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: col_size_malformed_table", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  col_size: 5O" ]

        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly accepted a malformed col_size value"
    end subroutine scenario_col_size_malformed_value

    !> Same as scenario_col_size_malformed_value, for array_size: -- an explicit non-positive
    !> value is also rejected (not silently coerced to 1).
    subroutine scenario_array_size_malformed_value()
        type(parquet_schema) :: schema

        schema%maml%name = "array_size_malformed.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: array_size_malformed_table", &
            "fields:", &
            "- name: v", &
            "  data_type: string", &
            "  array_size: -3" ]

        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly accepted a malformed array_size value"
    end subroutine scenario_array_size_malformed_value

    !> array_size: auto only applies to string columns -- declaring it on a numeric field can
    !> never be resolved (no write path for a non-string column ever touches array_size), so
    !> parquet_validate_maml rejects it up front instead of silently leaving it unresolved.
    subroutine scenario_array_size_auto_on_non_string()
        type(parquet_schema) :: schema

        schema%maml%name = "array_size_auto_on_non_string.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: array_size_auto_on_non_string", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  array_size: auto" ]

        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly accepted array_size: auto on a non-string column"
    end subroutine scenario_array_size_auto_on_non_string

    !> schema%set_col_size rejects a non-positive col_size outright, regardless of whether the
    !> target column is currently "auto".
    subroutine scenario_set_col_size_non_positive()
        type(parquet_schema) :: schema

        call schema%init(table="set_col_size_non_positive_table")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)

        call schema%set_col_size("v", 0)
        print '(a)', "unexpectedly accepted a non-positive col_size"
    end subroutine scenario_set_col_size_non_positive

    !> schema%set_col_size refuses to override a column whose col_size is already concretely
    !> resolved (not "auto") unless force=.true. is passed.
    subroutine scenario_set_col_size_already_resolved_no_force()
        type(parquet_schema) :: schema

        call schema%init(table="set_col_size_already_resolved_table")
        call schema%add_field("v", "int32", col_size=3)
        call parquet_parse_maml(schema)

        call schema%set_col_size("v", 5)
        print '(a)', "unexpectedly overrode an already-resolved col_size without force=.true."
    end subroutine scenario_set_col_size_already_resolved_no_force

    !> schema%set_array_size only applies to string columns.
    subroutine scenario_set_array_size_non_string_column()
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_non_string_table")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)

        call schema%set_array_size("v", 10)
        print '(a)', "unexpectedly accepted set_array_size on a non-string column"
    end subroutine scenario_set_array_size_non_string_column

    !> schema%set_array_size rejects a non-positive array_size outright, regardless of whether
    !> the target column is currently "auto" (mirrors scenario_set_col_size_non_positive).
    subroutine scenario_set_array_size_non_positive()
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_non_positive_table")
        call schema%add_field("txt", "string")
        call parquet_parse_maml(schema)

        call schema%set_array_size("txt", 0)
        print '(a)', "unexpectedly accepted a non-positive array_size"
    end subroutine scenario_set_array_size_non_positive

    !> schema%set_array_size refuses to override a column whose array_size is already
    !> concretely resolved (not "auto") unless force=.true. is passed (mirrors
    !> scenario_set_col_size_already_resolved_no_force).
    subroutine scenario_set_array_size_already_resolved_no_force()
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_already_resolved_table")
        call schema%add_field("txt", "string", array_size=8)
        call parquet_parse_maml(schema)

        call schema%set_array_size("txt", 12)
        print '(a)', "unexpectedly overrode an already-resolved array_size without force=.true."
    end subroutine scenario_set_array_size_already_resolved_no_force

    !> A flat (1-D) parquet_write_column call cannot resolve a col_size: auto placeholder itself
    !> (it needs col_size already known to divide its own flat array into rows) -- it must error
    !> stop with a clear message rather than silently misinterpreting the array's shape.
    subroutine scenario_flat_write_col_size_still_auto()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(6) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]

        schema%maml%name = "flat_write_col_size_still_auto.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: flat_write_col_size_still_auto", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  col_size: auto" ]
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_flat_write_auto.parquet", schema)
        call parquet_write_column(writer, "v", data)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a flat column whose schema col_size is still 'auto'"
    end subroutine scenario_flat_write_col_size_still_auto

end program error_scenarios
