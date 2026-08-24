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
    ! parquet_emit_info is deliberately PRIVATE in the `parquet` facade (it is an output
    ! channel, not user API), so the informational-channel scenario imports it from the
    ! settings module directly.
    use parquet_settings, only : parquet_emit_info, parquet_emit_warning
    use parquet_columns
    use parquet_table_example, only : parquet_table_test
    use parquet_tables
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp, &
        parquet_unit_seconds, parquet_unit_millis, parquet_unit_nanos
    use iso_fortran_env, only : int32, int64, real32, real64
    !$ use omp_lib, only : omp_get_max_threads, omp_get_thread_num
    implicit none

    !> A second concrete table type, existing only so the %clone same-type guard has something to
    !! be told apart from. Stands in for what tools/generate_parquet_table.sh will emit later: a
    !! table extended with predefined-column accessors, which a clone must not silently drop.
    type, extends(parquet_table) :: extended_table
        integer :: marker = 0 !! never read; the type's identity is the whole point.
    end type extended_table

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
    case ("print_stat_screened_rows")
        call scenario_print_stat_screened_rows()
    case ("large_string_roundtrip")
        call scenario_large_string_roundtrip()
    case ("streamed_string_is_large_utf8")
        call scenario_streamed_string_is_large_utf8()
    case ("whole_string_is_plain_utf8")
        call scenario_whole_string_is_plain_utf8()
    case ("string_view_roundtrip")
        call scenario_string_view_roundtrip()
    case ("string_view_compact_read")
        call scenario_string_view_compact_read()
    case ("col_size_overflow")
        call scenario_col_size_overflow()
    case ("col_size_and_row_mode_avoid_whole_column_read")
        call scenario_col_size_and_row_mode_avoid_whole_column_read()
    case ("plain_list_size_queries_avoid_whole_column_read")
        call scenario_plain_list_size_queries_avoid_whole_column_read()
    case ("list_width_never_reads_whole_column")
        call scenario_list_width_never_reads_whole_column()
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
    case ("filter_remap_size_mismatch")
        call scenario_filter_remap_size_mismatch()
    case ("filter_remap_name_too_long")
        call scenario_filter_remap_name_too_long()
    case ("filter_remap_rule_too_long")
        call scenario_filter_remap_rule_too_long()
    case ("sortkey_remap_size_mismatch")
        call scenario_sortkey_remap_size_mismatch()
    case ("sortkey_remap_name_too_long")
        call scenario_sortkey_remap_name_too_long()
    case ("set_arrow_threads_zero")
        call scenario_set_arrow_threads_zero()
    case ("settings_bad_codec")
        call scenario_settings_bad_codec()
    case ("settings_negative_sort_threads")
        call scenario_settings_negative_sort_threads()
    case ("settings_negative_bucket_limit")
        call scenario_settings_negative_bucket_limit()
    case ("settings_negative_row_group_bytes")
        call scenario_settings_negative_row_group_bytes()
    case ("settings_env_bad_token")
        call scenario_settings_env_bad_token()
    case ("settings_env_not_an_integer")
        call scenario_settings_env_not_an_integer()
    case ("settings_env_too_long")
        call scenario_settings_env_too_long()
    case ("settings_env_int32_out_of_range")
        call scenario_settings_env_int32_out_of_range()
    case ("settings_env_long_value_preview")
        call scenario_settings_env_long_value_preview()
    case ("settings_emit_info_channel")
        call scenario_settings_emit_info_channel()
    case ("size_queries_read_no_column_data")
        call scenario_size_queries_read_no_column_data()
    case ("sort_affinity_clamp_warns")
        call scenario_sort_affinity_clamp_warns()
    case ("sort_affinity_clamp_silent")
        call scenario_sort_affinity_clamp_silent()
    case ("sort_affinity_clamp_absent")
        call scenario_sort_affinity_clamp_absent()
    case ("strings_reindex_duplicate_index")
        call scenario_strings_reindex_duplicate_index()
    case ("strings_reindex_trusted_length_mismatch")
        call scenario_strings_reindex_trusted_length_mismatch()
    case ("strings_copy_buffers_offsets_too_short")
        call scenario_strings_copy_buffers_offsets_too_short()
    case ("strings_copy_buffers_data_too_short")
        call scenario_strings_copy_buffers_data_too_short()
    case ("weighted_contract_failure")
        call scenario_weighted_contract_failure()
    case ("weighted_negative_weight")
        call scenario_weighted_negative_weight()
    case ("weighted_all_zero")
        call scenario_weighted_all_zero()
    case ("weighted_nan_weight")
        call scenario_weighted_nan_weight()
    case ("weighted_next_uninitialised")
        call scenario_weighted_next_uninitialised()
    case ("weighted_init_twice")
        call scenario_weighted_init_twice()
    case ("weighted_subset_too_large")
        call scenario_weighted_subset_too_large()
    case ("weighted_perm_size_mismatch")
        call scenario_weighted_perm_size_mismatch()
    case ("settings_env_two_numbers")
        call scenario_settings_env_two_numbers()
    case ("settings_env_out_of_range")
        call scenario_settings_env_out_of_range()
    case ("settings_env_bad_boolean")
        call scenario_settings_env_bad_boolean()
    case ("settings_env_clean_run")
        call scenario_settings_env_clean_run()
    case ("settings_set_threads_zero")
        call scenario_settings_set_threads_zero()
    case ("settings_negative_prefetch_threads")
        call scenario_settings_negative_prefetch_threads()
    case ("settings_warning_normal")
        call scenario_settings_warning(level="normal", stream="stdout")
    case ("settings_warning_errors_only")
        call scenario_settings_warning(level="errors_only", stream="stdout")
    case ("settings_warning_on_stderr")
        call scenario_settings_warning(level="normal", stream="stderr")
    case ("settings_print_stat_normal")
        call scenario_settings_print_stat(level="normal")
    case ("settings_print_stat_silent")
        call scenario_settings_print_stat(level="silent")
    case ("reader_print_stat_normal")
        call scenario_reader_print_stat_verbosity(level="normal")
    case ("reader_print_stat_silent")
        call scenario_reader_print_stat_verbosity(level="silent")
    case ("settings_cpp_warning_normal")
        call scenario_settings_cpp_warning(level="normal")
    case ("settings_cpp_warning_errors_only")
        call scenario_settings_cpp_warning(level="errors_only")
    case ("settings_cpp_warning_silenced_after_open")
        call scenario_settings_cpp_warning(level="errors_only", after_open=.true.)
    case ("settings_bad_verbosity")
        call scenario_settings_bad_verbosity()
    case ("settings_bad_stream")
        call scenario_settings_bad_stream()
    case ("settings_error_survives_silence")
        call scenario_settings_error_survives_silence()
    case ("read_qc_entry_too_long")
        call scenario_read_qc_entry_too_long()
    case ("read_qc_remap_size_mismatch")
        call scenario_read_qc_remap_size_mismatch()
    case ("read_qc_remap_entry_too_long")
        call scenario_read_qc_remap_entry_too_long()
    case ("filter_bad_numeric_value")
        call scenario_filter_bad_numeric_value()
    case ("filter_bad_numeric_value_scoped")
        call scenario_filter_bad_numeric_value_scoped()
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
    case ("filter_is_nan_non_float_column")
        call scenario_filter_is_nan_non_float_column()
    case ("filter_nan_literal_rejected")
        call scenario_filter_nan_literal_rejected()
    case ("filter_is_nan_missing_combinator")
        call scenario_filter_is_nan_missing_combinator()
    case ("filter_unsupported_column_type")
        call scenario_filter_unsupported_column_type()
    case ("sort_unsupported_column_type")
        call scenario_sort_unsupported_column_type()
    case ("filter_temporal_value_not_quoted")
        call scenario_filter_temporal_value_not_quoted()
    case ("filter_unbalanced_parens")
        call scenario_filter_unbalanced_parens()
    case ("filter_stray_close_paren")
        call scenario_filter_stray_close_paren()
    case ("filter_empty_parens")
        call scenario_filter_empty_parens()
    case ("filter_dangling_and")
        call scenario_filter_dangling_and()
    case ("filter_leading_or")
        call scenario_filter_leading_or()
    case ("filter_not_without_operand")
        call scenario_filter_not_without_operand()
    case ("filter_missing_combinator")
        call scenario_filter_missing_combinator()
    case ("filter_nesting_too_deep")
        call scenario_filter_nesting_too_deep()
    case ("filter_too_many_nodes")
        call scenario_filter_too_many_nodes()
    case ("filter_temporal_bad_iso_literal")
        call scenario_filter_temporal_bad_iso_literal()
    case ("filter_temporal_literal_too_precise")
        call scenario_filter_temporal_literal_too_precise()
    case ("filter_set_filter_after_read")
        call scenario_filter_set_filter_after_read()
    case ("filter_set_filter_twice")
        call scenario_filter_set_filter_twice()
    case ("filter_unterminated_quote")
        call scenario_filter_unterminated_quote()
    case ("filter_empty_rule")
        call scenario_filter_empty_rule()
    case ("filter_expected_close_paren")
        call scenario_filter_expected_close_paren()
    case ("filter_close_paren_as_clause")
        call scenario_filter_close_paren_as_clause()
    case ("filter_leaf_too_long")
        call scenario_filter_leaf_too_long()
    case ("filter_too_many_nodes_across_adds")
        call scenario_filter_too_many_nodes_across_adds()
    case ("filter_scope_out_of_range")
        call scenario_filter_scope_out_of_range()
    case ("filter_scope_reversed")
        call scenario_filter_scope_reversed()
    case ("chunk_read_bool_type_mismatch")
        call scenario_chunk_read_bool_type_mismatch()
    case ("filter_row_range_outside_row_groups")
        call scenario_filter_row_range_outside_row_groups()
    case ("filter_all_row_groups_bounded")
        call scenario_filter_all_row_groups_bounded()
    case ("filter_row_range_out_of_range")
        call scenario_filter_row_range_out_of_range()
    case ("filter_row_element_mode_no_whole_column_read")
        call scenario_filter_row_element_mode_no_whole_column_read()
    case ("filter_scoped_reads_no_whole_column")
        call scenario_filter_scoped_reads_no_whole_column()
    case ("sorting_nth_out_of_range")
        call scenario_sorting_nth_out_of_range()
    case ("sorting_partial_negative_n")
        call scenario_sorting_partial_negative_n()
    case ("sorting_quantile_out_of_range")
        call scenario_sorting_quantile_out_of_range()
    case ("sorting_quantile_bad_rounding")
        call scenario_sorting_quantile_bad_rounding()
    case ("sorting_quantile_all_null")
        call scenario_sorting_quantile_all_null()
    case ("sorting_permute_index_out_of_range")
        call scenario_sorting_permute_index_out_of_range()
    case ("sorting_permute_duplicate_index")
        call scenario_sorting_permute_duplicate_index()
    case ("sorting_permute_length_mismatch")
        call scenario_sorting_permute_length_mismatch()
    case ("sorting_valid_length_mismatch")
        call scenario_sorting_valid_length_mismatch()
    case ("sorting_keys_row_count_mismatch")
        call scenario_sorting_keys_row_count_mismatch()
    case ("sorting_keys_empty")
        call scenario_sorting_keys_empty()
    case ("sorting_partial_keys_empty")
        call scenario_sorting_partial_keys_empty()
    case ("sorting_partial_keys_empty_i64")
        call scenario_sorting_partial_keys_empty_i64()
    case ("sorting_column_vector")
        call scenario_sorting_column_vector()
    case ("sorting_search_unsorted")
        call scenario_sorting_search_unsorted()
    case ("sorting_search_target_too_long")
        call scenario_sorting_search_target_too_long()
    case ("sorting_rank_bad_method")
        call scenario_sorting_rank_bad_method()
    case ("sorting_minmax_all_null")
        call scenario_sorting_minmax_all_null()
    case ("sorting_minmax_all_null_i32")
        call scenario_sorting_minmax_all_null_i32()
    case ("sorting_minmax_all_null_i64")
        call scenario_sorting_minmax_all_null_i64()
    case ("sorting_minmax_all_null_f32")
        call scenario_sorting_minmax_all_null_f32()
    case ("sorting_minmax_all_null_chr")
        call scenario_sorting_minmax_all_null_chr()
    case ("sorting_minmax_all_null_date")
        call scenario_sorting_minmax_all_null_date()
    case ("sorting_minmax_all_null_time")
        call scenario_sorting_minmax_all_null_time()
    case ("sorting_minmax_all_null_ts")
        call scenario_sorting_minmax_all_null_ts()
    case ("sorting_minmax_all_null_strcol")
        call scenario_sorting_minmax_all_null_strcol()
    case ("sorting_argminmax_all_null_col")
        call scenario_sorting_argminmax_all_null_col()
    case ("sorting_keys_empty_i64")
        call scenario_sorting_keys_empty_i64()
    case ("sorting_is_sorted_keys_empty")
        call scenario_sorting_is_sorted_keys_empty()
    case ("sorting_column_no_kind")
        call scenario_sorting_column_no_kind()
    case ("sorting_merge_unsorted")
        call scenario_sorting_merge_unsorted()
    case ("random_stream_exhausted")
        call scenario_random_stream_exhausted()
    case ("random_stream_rewind_below_one")
        call scenario_random_stream_rewind_below_one()
    case ("random_gamma_shape_not_positive")
        call scenario_random_gamma_shape_not_positive()
    case ("random_gamma_shape_nan")
        call scenario_random_gamma_shape_nan()
    case ("random_poisson_lambda_negative")
        call scenario_random_poisson_lambda_negative()
    case ("random_poisson_lambda_nan")
        call scenario_random_poisson_lambda_nan()
    case ("random_poisson_lambda_too_large")
        call scenario_random_poisson_lambda_too_large()
    case ("random_poisson_int32_overflow")
        call scenario_random_poisson_int32_overflow()
    case ("random_resample_empty_population")
        call scenario_random_resample_empty_population()
    case ("random_resample_int32_too_narrow")
        call scenario_random_resample_int32_too_narrow()
    case ("random_subset_larger_than_population")
        call scenario_random_subset_larger_than_population()
    case ("random_subset_empty_population")
        call scenario_random_subset_empty_population()
    case ("random_subset_int32_too_narrow")
        call scenario_random_subset_int32_too_narrow()
    case ("sort_unknown_column")
        call scenario_sort_unknown_column()
    case ("sort_vector_column")
        call scenario_sort_vector_column()
    case ("sort_empty_key")
        call scenario_sort_empty_key()
    case ("sort_bad_direction")
        call scenario_sort_bad_direction()
    case ("sort_minus_only")
        call scenario_sort_minus_only()
    case ("sort_name_too_long")
        call scenario_sort_name_too_long()
    case ("sort_two_direction_words")
        call scenario_sort_two_direction_words()
    case ("sort_minus_and_direction")
        call scenario_sort_minus_and_direction()
    case ("sort_key_too_long")
        call scenario_sort_key_too_long()
    case ("sort_too_many_keys")
        call scenario_sort_too_many_keys()
    case ("sort_chunked_read")
        call scenario_sort_chunked_read()
    case ("sort_chunked_read_string")
        call scenario_sort_chunked_read_string()
    case ("sort_chunked_read_vector")
        call scenario_sort_chunked_read_vector()
    case ("sort_chunked_read_temporal")
        call scenario_sort_chunked_read_temporal()
    case ("sort_get_chunk_size")
        call scenario_sort_get_chunk_size()
    case ("sort_set_sort_twice")
        call scenario_sort_set_sort_twice()
    case ("sort_set_sort_after_read")
        call scenario_sort_set_sort_after_read()
    case ("sort_set_sort_after_chunked_read")
        call scenario_sort_set_sort_after_chunked_read()
    case ("filter_set_filter_after_chunked_read")
        call scenario_filter_set_filter_after_chunked_read()
    case ("filter_set_filter_after_sort")
        call scenario_filter_set_filter_after_sort()
    case ("read_ragged_list_column")
        call scenario_read_ragged_list_column()
    case ("table_read_ragged_list_column")
        call scenario_table_read_ragged_list_column()
    case ("table_read_avg_ok_list_column")
        call scenario_table_read_avg_ok_list_column()
    case ("adopt_transform_onto_transformed")
        call scenario_adopt_transform_onto_transformed()
    case ("adopt_transform_after_read")
        call scenario_adopt_transform_after_read()
    case ("adopt_transform_other_file")
        call scenario_adopt_transform_other_file()
    case ("sample_negative_fraction")
        call scenario_sample_negative_fraction()
    case ("sample_nan_fraction")
        call scenario_sample_nan_fraction()
    case ("print_stat_sampled_rows")
        call scenario_print_stat_sampled_rows()
    case ("print_stat_released_column")
        call scenario_print_stat_released_column()
    case ("print_stat_sorted_rows")
        call scenario_print_stat_sorted_rows()
    case ("sample_mask_build_error")
        call scenario_sample_mask_build_error()
    case ("sample_mask_length_mismatch")
        call scenario_sample_mask_length_mismatch()
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
    case ("qc_miss_omitted_no_read_violation")
        call scenario_qc_miss_omitted_no_read_violation()
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
    case ("maml_line_too_long")
        call scenario_maml_line_too_long()
    case ("embedded_maml_unknown_name")
        call scenario_embedded_maml_unknown_name()
    case ("metadata_datatype_key_collision")
        call scenario_metadata_datatype_key_collision()
    case ("metadata_datatype_no_collision_control")
        call scenario_metadata_datatype_no_collision_control()
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
    case ("close_writer_no_columns_written")
        call scenario_close_writer_no_columns_written()
    case ("close_writer_zero_length_writes_quiet")
        call scenario_close_writer_zero_length_writes_quiet()
    case ("close_writer_no_columns_with_mask")
        call scenario_close_writer_no_columns_with_mask()
    case ("close_writer_missing_write")
        call scenario_close_writer_missing_write()
    case ("close_writer_missing_write_silenced")
        call scenario_close_writer_missing_write_silenced()
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
    case ("write_protected_vector_element_null")
        call scenario_write_protected_vector_element_null()
    case ("protected_col_map_output_name")
        call scenario_protected_col_map_output_name()
    case ("protected_col_map_internal_name_rejected")
        call scenario_protected_col_map_internal_name_rejected()
    case ("write_protected_temporal_null")
        call scenario_write_protected_temporal_null()
    case ("write_protected_string_column_null")
        call scenario_write_protected_string_column_null()
    case ("chunk_mask_dropped_after_first_row_group")
        call scenario_chunk_mask_dropped_after_first_row_group()
    case ("chunk_mask_added_after_first_row_group")
        call scenario_chunk_mask_added_after_first_row_group()
    case ("set_protected_unknown_column")
        call scenario_set_protected_unknown_column()
    case ("set_protected_unprotect_warns")
        call scenario_set_protected_unprotect_warns()
    case ("validate_qc_min_not_numeric")
        call scenario_validate_qc_min_not_numeric()
    case ("validate_qc_max_not_numeric")
        call scenario_validate_qc_max_not_numeric()
    case ("validate_qc_min_non_integral_for_int32")
        call scenario_validate_qc_min_non_integral_for_int32()
    case ("validate_qc_min_out_of_int32_range")
        call scenario_validate_qc_min_out_of_int32_range()
    case ("validate_qc_min_overflows_int64")
        call scenario_validate_qc_min_overflows_int64()
    case ("validate_qc_min_wrong_operator")
        call scenario_validate_qc_min_wrong_operator()
    case ("validate_qc_max_wrong_operator")
        call scenario_validate_qc_max_wrong_operator()
    case ("validate_qc_miss_bad_value")
        call scenario_validate_qc_miss_bad_value()
    case ("validate_qc_miss_valid_values")
        call scenario_validate_qc_miss_valid_values()
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
    case ("qc_int64_beyond_float64_precision")
        call scenario_qc_int64_beyond_float64_precision()
    case ("qc_int64_exact_bound_violation")
        call scenario_qc_int64_exact_bound_violation()
    case ("qc_int64_exact_bound_no_false_violation")
        call scenario_qc_int64_exact_bound_no_false_violation()
    case ("qc_int64_strict_operators")
        call scenario_qc_int64_strict_operators()
    case ("qc_int64_values_fractional_bound")
        call scenario_qc_int64_values_fractional_bound()
    case ("qc_warning_float64")
        call scenario_qc_warning_float64()
    case ("qc_warning_string")
        call scenario_qc_warning_string()
    case ("qc_min_max_ignored_for_boolean")
        call scenario_qc_min_max_ignored_for_boolean()
    case ("qc_miss_enforced_for_boolean")
        call scenario_qc_miss_enforced_for_boolean()
    case ("qc_miss_default_active_numeric_warns")
        call scenario_qc_miss_default_active_numeric_warns()
    case ("qc_miss_absent_no_warning")
        call scenario_qc_miss_absent_no_warning()
    case ("qc_miss_declared_null_no_warning")
        call scenario_qc_miss_declared_null_no_warning()
    case ("qc_miss_string_warns")
        call scenario_qc_miss_string_warns()
    case ("qc_miss_temporal_warns")
        call scenario_qc_miss_temporal_warns()
    case ("qc_miss_temporal_declared_null_no_warning")
        call scenario_qc_miss_temporal_declared_null_no_warning()
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
    case ("write_float_nan_to_int32")
        call scenario_write_float_nan_to_int32()
    case ("write_float_to_int64_negative_out_of_range")
        call scenario_write_float_to_int64_negative_out_of_range()
    case ("write_float_to_int32_non_integral_and_out_of_range")
        call scenario_write_float_to_int32_non_integral_and_out_of_range()
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
    case ("writer_guard_sequential_handoff")
        call scenario_writer_guard_sequential_handoff()
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
    case ("schema_add_metadata_before_init")
        call scenario_schema_add_metadata_before_init()
    case ("schema_add_field_validates_field_rules")
        call scenario_schema_add_field_validates_field_rules()
    case ("columns_data_ptr_kind_mismatch")
        call scenario_columns_data_ptr_kind_mismatch()
    case ("columns_uninitialized_append_nulls")
        call scenario_columns_uninitialized_append_nulls()
    case ("columns_get_at_index_out_of_range")
        call scenario_columns_get_at_index_out_of_range()
    case ("columns_append_kind_mismatch")
        call scenario_columns_append_kind_mismatch()
    case ("columns_append_row_of_width_mismatch")
        call scenario_columns_append_row_of_width_mismatch()
    case ("columns_append_row_of_row_out_of_range")
        call scenario_columns_append_row_of_row_out_of_range()
    case ("columns_element_index_out_of_range")
        call scenario_columns_element_index_out_of_range()
    case ("columns_set_validity_elem_shape_mismatch")
        call scenario_columns_set_validity_elem_shape_mismatch()
    case ("columns_set_validity_row_count_mismatch")
        call scenario_columns_set_validity_row_count_mismatch()
    case ("columns_clear_null_elem_temporal")
        call scenario_columns_clear_null_elem_temporal()
    case ("table_append_row_validates_first")
        call scenario_table_append_row_validates_first()
    case ("columns_append_width_mismatch")
        call scenario_columns_append_width_mismatch()
    case ("string_column_reindex_length_mismatch")
        call scenario_string_column_reindex_length_mismatch()
    case ("string_column_reindex_out_of_range")
        call scenario_string_column_reindex_out_of_range()
    case ("string_column_delete_by_mask_length_mismatch")
        call scenario_string_column_delete_by_mask_length_mismatch()
    case ("string_column_append_nulls_negative")
        call scenario_string_column_append_nulls_negative()
    case ("columns_string_column_wrong_kind")
        call scenario_columns_string_column_wrong_kind()
    case ("columns_init_container_kind")
        call scenario_columns_init_container_kind()
    case ("columns_init_width_on_scalar_kind")
        call scenario_columns_init_width_on_scalar_kind()
    case ("columns_adopt_not_allocated_i32")
        call scenario_columns_adopt_not_allocated_i32()
    case ("columns_adopt_not_allocated_i64")
        call scenario_columns_adopt_not_allocated_i64()
    case ("columns_adopt_not_allocated_f32")
        call scenario_columns_adopt_not_allocated_f32()
    case ("columns_adopt_not_allocated_f64")
        call scenario_columns_adopt_not_allocated_f64()
    case ("columns_adopt_not_allocated_bool")
        call scenario_columns_adopt_not_allocated_bool()
    case ("columns_adopt_not_allocated_date")
        call scenario_columns_adopt_not_allocated_date()
    case ("columns_adopt_not_allocated_time")
        call scenario_columns_adopt_not_allocated_time()
    case ("columns_adopt_not_allocated_ts")
        call scenario_columns_adopt_not_allocated_ts()
    case ("columns_adopt_not_allocated_i32v")
        call scenario_columns_adopt_not_allocated_i32v()
    case ("columns_adopt_not_allocated_i64v")
        call scenario_columns_adopt_not_allocated_i64v()
    case ("columns_adopt_not_allocated_f32v")
        call scenario_columns_adopt_not_allocated_f32v()
    case ("columns_adopt_not_allocated_f64v")
        call scenario_columns_adopt_not_allocated_f64v()
    case ("columns_adopt_not_allocated_boolv")
        call scenario_columns_adopt_not_allocated_boolv()
    case ("columns_adopt_not_allocated_datev")
        call scenario_columns_adopt_not_allocated_datev()
    case ("columns_adopt_not_allocated_timev")
        call scenario_columns_adopt_not_allocated_timev()
    case ("columns_adopt_not_allocated_tsv")
        call scenario_columns_adopt_not_allocated_tsv()
    case ("columns_set_all_length_mismatch")
        call scenario_columns_set_all_length_mismatch()
    case ("columns_get_at_width_mismatch")
        call scenario_columns_get_at_width_mismatch()
    case ("columns_delete_by_mask_length_mismatch")
        call scenario_columns_delete_by_mask_length_mismatch()
    case ("columns_reindex_length_mismatch")
        call scenario_columns_reindex_length_mismatch()
    case ("columns_clear_null_temporal")
        call scenario_columns_clear_null_temporal()
    case ("columns_reindex_duplicate_index")
        call scenario_columns_reindex_duplicate_index()
    case ("columns_paste_kind_mismatch")
        call scenario_columns_paste_kind_mismatch()
    case ("columns_paste_width_mismatch")
        call scenario_columns_paste_width_mismatch()
    case ("columns_paste_string_kind")
        call scenario_columns_paste_string_kind()
    case ("columns_paste_source_index_below_one")
        call scenario_columns_paste_source_index_below_one()
    case ("columns_paste_negative_count")
        call scenario_columns_paste_negative_count()
    case ("columns_paste_source_past_end")
        call scenario_columns_paste_source_past_end()
    case ("columns_paste_destination_index_below_one")
        call scenario_columns_paste_destination_index_below_one()
    case ("columns_paste_destination_past_end")
        call scenario_columns_paste_destination_past_end()
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
    case ("string_build_from_character_mask_length")
        call scenario_string_build_from_character_mask_length()
    case ("string_append_values_mask_length")
        call scenario_string_append_values_mask_length()
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
    case ("time_info_on_date_column")
        call scenario_time_info_on_date_column()
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
    case ("get_version_arrow_mode_removed")
        call scenario_get_version_arrow_mode_removed()
    case ("get_arrow_version_invalid_mode")
        call scenario_get_arrow_version_invalid_mode()
    case ("column_exists_bad_type_token")
        call scenario_column_exists_bad_type_token()
    case ("column_exists_bad_type_token_missing_column")
        call scenario_column_exists_bad_type_token_missing_column()
    case ("column_exists_empty_type_filter")
        call scenario_column_exists_empty_type_filter()
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
    case ("extra_section_capitalized")
        call scenario_extra_section_capitalized(.true.)
    case ("extra_section_lowercase_control")
        call scenario_extra_section_capitalized(.false.)
    case ("compact_write_exceeds_array_size_warns")
        call scenario_compact_write_exceeds_array_size_warns()
    case ("table_assignment_blocked")
        call scenario_table_assignment_blocked()
    case ("table_not_opened")
        call scenario_table_not_opened()
    case ("table_column_position_out_of_range")
        call scenario_table_column_position_out_of_range()
    case ("table_get_element_row_out_of_range")
        call scenario_table_get_element_row_out_of_range()
    case ("table_get_element_kind_mismatch")
        call scenario_table_get_element_kind_mismatch()
    case ("table_get_element_kind_mismatch_i32v")
        call scenario_table_get_element_kind_mismatch_i32v()
    case ("table_get_element_kind_mismatch_i64v")
        call scenario_table_get_element_kind_mismatch_i64v()
    case ("table_get_element_kind_mismatch_f32v")
        call scenario_table_get_element_kind_mismatch_f32v()
    case ("table_get_element_kind_mismatch_f64v")
        call scenario_table_get_element_kind_mismatch_f64v()
    case ("table_get_element_kind_mismatch_boolv")
        call scenario_table_get_element_kind_mismatch_boolv()
    case ("table_get_element_kind_mismatch_datev")
        call scenario_table_get_element_kind_mismatch_datev()
    case ("table_get_element_kind_mismatch_timev")
        call scenario_table_get_element_kind_mismatch_timev()
    case ("table_get_element_kind_mismatch_tsv")
        call scenario_table_get_element_kind_mismatch_tsv()
    case ("table_get_element_missing_column")
        call scenario_table_get_element_missing_column()
    case ("col_handle_stale_after_mutation")
        call scenario_col_handle_stale_after_mutation()
    case ("col_handle_row_out_of_range")
        call scenario_col_handle_row_out_of_range()
    case ("col_handle_ref_after_mutation")
        call scenario_col_handle_ref_after_mutation()
    case ("col_handle_ref_kind_mismatch")
        call scenario_col_handle_ref_kind_mismatch()
    case ("row_handle_foreign_column")
        call scenario_row_handle_foreign_column()
    case ("row_handle_stale_after_sort")
        call scenario_row_handle_stale_after_sort()
    case ("table_append_row_self")
        call scenario_table_append_row_self()
    case ("table_append_row_stale_source")
        call scenario_table_append_row_stale_source()
    case ("col_handle_never_attached")
        call scenario_col_handle_never_attached()
    case ("table_pointer_kind_mismatch")
        call scenario_table_pointer_kind_mismatch()
    case ("table_get_array_kind_mismatch")
        call scenario_table_get_array_kind_mismatch()
    case ("table_col_ptr_kind_mismatch_i32")
        call scenario_table_col_ptr_kind_mismatch_i32()
    case ("table_col_ptr_kind_mismatch_f32")
        call scenario_table_col_ptr_kind_mismatch_f32()
    case ("table_col_ptr_kind_mismatch_f64")
        call scenario_table_col_ptr_kind_mismatch_f64()
    case ("table_col_ptr_kind_mismatch_bool")
        call scenario_table_col_ptr_kind_mismatch_bool()
    case ("table_col_ptr_kind_mismatch_date")
        call scenario_table_col_ptr_kind_mismatch_date()
    case ("table_col_ptr_kind_mismatch_time")
        call scenario_table_col_ptr_kind_mismatch_time()
    case ("table_col_ptr_kind_mismatch_ts")
        call scenario_table_col_ptr_kind_mismatch_ts()
    case ("table_col_ptr_kind_mismatch_i32v")
        call scenario_table_col_ptr_kind_mismatch_i32v()
    case ("table_col_ptr_kind_mismatch_i64v")
        call scenario_table_col_ptr_kind_mismatch_i64v()
    case ("table_col_ptr_kind_mismatch_f32v")
        call scenario_table_col_ptr_kind_mismatch_f32v()
    case ("table_col_ptr_kind_mismatch_f64v")
        call scenario_table_col_ptr_kind_mismatch_f64v()
    case ("table_col_ptr_kind_mismatch_boolv")
        call scenario_table_col_ptr_kind_mismatch_boolv()
    case ("table_col_ptr_kind_mismatch_datev")
        call scenario_table_col_ptr_kind_mismatch_datev()
    case ("table_col_ptr_kind_mismatch_timev")
        call scenario_table_col_ptr_kind_mismatch_timev()
    case ("table_col_ptr_kind_mismatch_tsv")
        call scenario_table_col_ptr_kind_mismatch_tsv()
    case ("table_get_array_kind_mismatch_i64")
        call scenario_table_get_array_kind_mismatch_i64()
    case ("table_get_array_kind_mismatch_f32")
        call scenario_table_get_array_kind_mismatch_f32()
    case ("table_get_array_kind_mismatch_f64")
        call scenario_table_get_array_kind_mismatch_f64()
    case ("table_get_array_kind_mismatch_bool")
        call scenario_table_get_array_kind_mismatch_bool()
    case ("table_get_array_kind_mismatch_date")
        call scenario_table_get_array_kind_mismatch_date()
    case ("table_get_array_kind_mismatch_time")
        call scenario_table_get_array_kind_mismatch_time()
    case ("table_get_array_kind_mismatch_ts")
        call scenario_table_get_array_kind_mismatch_ts()
    case ("table_get_array_kind_mismatch_i32v")
        call scenario_table_get_array_kind_mismatch_i32v()
    case ("table_get_array_kind_mismatch_i64v")
        call scenario_table_get_array_kind_mismatch_i64v()
    case ("table_get_array_kind_mismatch_f32v")
        call scenario_table_get_array_kind_mismatch_f32v()
    case ("table_get_array_kind_mismatch_f64v")
        call scenario_table_get_array_kind_mismatch_f64v()
    case ("table_get_array_kind_mismatch_boolv")
        call scenario_table_get_array_kind_mismatch_boolv()
    case ("table_get_array_kind_mismatch_datev")
        call scenario_table_get_array_kind_mismatch_datev()
    case ("table_get_array_kind_mismatch_timev")
        call scenario_table_get_array_kind_mismatch_timev()
    case ("table_get_array_kind_mismatch_tsv")
        call scenario_table_get_array_kind_mismatch_tsv()
    case ("table_unknown_column")
        call scenario_table_unknown_column()
    case ("table_unsupported_column_read")
        call scenario_table_unsupported_column_read()
    case ("table_prefetch_unknown_column")
        call scenario_table_prefetch_unknown_column()
    case ("table_prefetch_unsupported_column")
        call scenario_table_prefetch_unsupported_column()
    case ("table_add_column_row_mismatch")
        call scenario_table_add_column_row_mismatch()
    case ("table_add_column_kindless")
        call scenario_table_add_column_kindless()
    case ("table_add_column_duplicate")
        call scenario_table_add_column_duplicate()
    case ("table_add_column_duplicate_force_false")
        call scenario_table_add_column_duplicate_force_false()
    case ("table_row_group_bounds_in_memory")
        call scenario_table_row_group_bounds_in_memory()
    case ("table_set_kind_mismatch")
        call scenario_table_set_kind_mismatch()
    case ("table_get_file_metadata_in_memory")
        call scenario_table_get_file_metadata_in_memory()
    case ("table_get_file_metadata_missing_key")
        call scenario_table_get_file_metadata_missing_key()
    case ("table_kind_unknown_column")
        call scenario_table_kind_unknown_column()
    case ("table_set_length_mismatch")
        call scenario_table_set_length_mismatch()
    case ("table_write_missing_column")
        call scenario_table_write_missing_column()
    case ("table_write_unsupported_column")
        call scenario_table_write_unsupported_column()
    case ("table_write_no_overwrite")
        call scenario_table_write_no_overwrite()
    case ("table_write_schemaless_empty_maml")
        call scenario_table_write_schemaless_empty_maml()
    case ("table_write_unbuilt_schema")
        call scenario_table_write_unbuilt_schema()
    case ("table_row_index_after_detach")
        call scenario_table_row_index_after_detach()
    case ("table_row_index_shadowed_warning")
        call scenario_table_row_index_shadowed_warning()
    case ("table_evict_in_memory")
        call scenario_table_evict_in_memory()
    case ("table_evict_detached")
        call scenario_table_evict_detached()
    case ("table_set_is_valid_length")
        call scenario_table_set_is_valid_length()
    case ("table_copy_metadata_unknown_key")
        call scenario_table_copy_metadata_unknown_key()
    case ("table_copy_metadata_in_memory")
        call scenario_table_copy_metadata_in_memory()
    case ("table_copy_metadata_both_forms")
        call scenario_table_copy_metadata_both_forms()
    case ("table_slice_below_first_row")
        call scenario_table_slice_below_first_row()
    case ("table_slice_past_last_row")
        call scenario_table_slice_past_last_row()
    case ("table_slice_inverted")
        call scenario_table_slice_inverted()
    case ("table_row_index_out_of_range")
        call scenario_table_row_index_out_of_range()
    case ("table_get_slice_out_of_range")
        call scenario_table_get_slice_out_of_range()
    case ("table_slice_zero_step")
        call scenario_table_slice_zero_step()
    case ("table_reload_in_memory_column")
        call scenario_table_reload_in_memory_column()
    case ("table_reload_not_file_backed")
        call scenario_table_reload_not_file_backed()
    case ("table_evict_user_populated")
        call scenario_table_evict_user_populated()
    case ("table_reload_user_populated")
        call scenario_table_reload_user_populated()
    case ("table_set_user_populated_not_resident")
        call scenario_table_set_user_populated_not_resident()
    case ("table_row_unknown_column")
        call scenario_table_row_unknown_column()
    case ("table_row_unattached")
        call scenario_table_row_unattached()
    case ("table_row_unsupported_column")
        call scenario_table_row_unsupported_column()
    case ("table_detached_read_unmaterialized")
        call scenario_table_detached_read_unmaterialized()
    case ("table_slice_mutate_then_read")
        call scenario_table_slice_mutate_then_read()
    case ("table_detached_prefetch")
        call scenario_table_detached_prefetch()
    case ("table_detached_materialize_all")
        call scenario_table_detached_materialize_all()
    case ("table_detached_reload")
        call scenario_table_detached_reload()
    case ("table_detached_row_group_bounds")
        call scenario_table_detached_row_group_bounds()
    case ("table_row_group_bounds_sorted")
        call scenario_table_row_group_bounds_sorted()
    case ("table_mutate_unmaterialized_column")
        call scenario_table_mutate_unmaterialized_column()
    case ("table_mutate_unsupported_column")
        call scenario_table_mutate_unsupported_column()
    case ("table_filter_rows_mask_length")
        call scenario_table_filter_rows_mask_length()
    case ("table_delete_rows_out_of_range")
        call scenario_table_delete_rows_out_of_range()
    case ("table_truncate_negative")
        call scenario_table_truncate_negative()
    case ("table_append_null_rows_negative")
        call scenario_table_append_null_rows_negative()
    case ("table_sort_by_no_keys")
        call scenario_table_sort_by_no_keys()
    case ("table_sort_by_flag_count_mismatch")
        call scenario_table_sort_by_flag_count_mismatch()
    case ("table_sort_by_nulls_first_count_mismatch")
        call scenario_table_sort_by_nulls_first_count_mismatch()
    case ("table_sort_by_unknown_column")
        call scenario_table_sort_by_unknown_column()
    case ("table_sort_by_vector_column")
        call scenario_table_sort_by_vector_column()
    case ("table_argsort_by_vector_column")
        call scenario_table_argsort_by_vector_column()
    case ("table_argsort_by_no_keys")
        call scenario_table_argsort_by_no_keys()
    case ("table_argsort_by_group_nkeys_too_many")
        call scenario_table_argsort_by_group_nkeys_too_many()
    case ("table_argsort_by_group_nkeys_zero")
        call scenario_table_argsort_by_group_nkeys_zero()
    case ("table_argsort_by_group_nkeys_without_offsets")
        call scenario_table_argsort_by_group_nkeys_without_offsets()
    case ("table_argsort_partial_negative_n")
        call scenario_table_argsort_partial_negative_n()
    case ("table_top_n_negative_n")
        call scenario_table_top_n_negative_n()
    case ("table_top_n_unknown_column")
        call scenario_table_top_n_unknown_column()
    case ("table_top_n_detached_read")
        call scenario_table_top_n_detached_read()
    case ("column_gather_out_of_range")
        call scenario_column_gather_out_of_range()
    case ("string_column_gather_out_of_range")
        call scenario_string_column_gather_out_of_range()
    case ("table_append_unknown_column")
        call scenario_table_append_unknown_column()
    case ("table_append_kind_mismatch")
        call scenario_table_append_kind_mismatch()
    case ("table_append_width_mismatch")
        call scenario_table_append_width_mismatch()
    case ("table_append_unit_mismatch")
        call scenario_table_append_unit_mismatch()
    case ("table_append_row_no_common_column")
        call scenario_table_append_row_no_common_column()
    case ("table_set_element_row_out_of_range")
        call scenario_table_set_element_row_out_of_range()
    case ("table_set_element_kind_mismatch")
        call scenario_table_set_element_kind_mismatch()
    case ("table_set_null_mask_wrong_length")
        call scenario_table_set_null_mask_wrong_length()
    case ("table_set_null_mask_wrong_shape")
        call scenario_table_set_null_mask_wrong_shape()
    case ("table_set_null_row_out_of_range")
        call scenario_table_set_null_row_out_of_range()
    case ("table_rename_duplicate_name")
        call scenario_table_rename_duplicate_name()
    case ("table_rename_blank_name")
        call scenario_table_rename_blank_name()
    case ("table_copy_unsupported_kind")
        call scenario_table_copy_unsupported_kind()
    case ("table_copy_duplicate_name")
        call scenario_table_copy_duplicate_name()
    case ("table_copy_blank_name")
        call scenario_table_copy_blank_name()
    case ("table_copy_lossy_value")
        call scenario_table_copy_lossy_value()
    case ("table_cast_non_numeric")
        call scenario_table_cast_non_numeric()
    case ("table_cast_rank_change")
        call scenario_table_cast_rank_change()
    case ("table_cast_int_overflow")
        call scenario_table_cast_int_overflow()
    case ("table_cast_fractional")
        call scenario_table_cast_fractional()
    case ("table_cast_float_overflow")
        call scenario_table_cast_float_overflow()
    case ("table_bind_predefined_size_mismatch")
        call scenario_table_bind_predefined_size_mismatch()
    case ("table_bind_predefined_units_size_mismatch")
        call scenario_table_bind_predefined_units_size_mismatch()
    case ("table_bind_predefined_lossy_warning")
        call scenario_table_bind_predefined_lossy_warning()
    case ("table_bind_predefined_computed_name_taken")
        call scenario_table_bind_predefined_computed_name_taken()
    case ("table_set_rows_valid_length_mismatch")
        call scenario_table_set_rows_valid_length_mismatch()
    case ("table_set_elem_valid_shape_mismatch")
        call scenario_table_set_elem_valid_shape_mismatch()
    case ("table_set_rows_elem_valid_shape_mismatch")
        call scenario_table_set_rows_elem_valid_shape_mismatch()
    case ("table_set_slice_size_mismatch")
        call scenario_table_set_slice_size_mismatch()
    case ("table_cast_exact_i32_to_f32")
        call scenario_table_cast_exact_i32_to_f32()
    case ("table_cast_exact_i64_to_f32")
        call scenario_table_cast_exact_i64_to_f32()
    case ("table_cast_exact_i64_to_f64")
        call scenario_table_cast_exact_i64_to_f64()
    case ("table_cast_f32_fractional")
        call scenario_table_cast_f32_fractional()
    case ("table_cast_exact_precision")
        call scenario_table_cast_exact_precision()
    case ("table_cast_unsupported_column")
        call scenario_table_cast_unsupported_column()
    case ("table_clone_type_mismatch")
        call scenario_table_clone_type_mismatch()
    case ("table_row_string_kind_mismatch")
        call scenario_table_row_string_kind_mismatch()
    case ("table_row_kind_mismatch")
        call scenario_table_row_kind_mismatch()
    case ("table_get_slice_kind_mismatch")
        call scenario_table_get_slice_kind_mismatch()
    case ("table_remap_unknown_file_column")
        call scenario_table_remap_unknown_file_column()
    case ("table_remap_duplicate_internal")
        call scenario_table_remap_duplicate_internal()
    case ("table_qc_violation")
        call scenario_table_qc_violation()
    case ("table_slice_maml_sort")
        call scenario_table_slice_maml_sort()
    case ("table_filter_unknown_column")
        call scenario_table_filter_unknown_column()
    case ("table_first_touch_in_parallel")
        call scenario_table_first_touch_in_parallel()
    case ("table_resolve_width_in_parallel")
        call scenario_table_resolve_width_in_parallel()
    case ("table_first_touch_in_parallel_single")
        call scenario_table_first_touch_in_parallel_single()
    case ("table_resolve_width_in_parallel_single")
        call scenario_table_resolve_width_in_parallel_single()
    case ("table_mutate_shared_in_parallel")
        call scenario_table_mutate_shared_in_parallel()
    case ("table_compact_shared_in_parallel")
        call scenario_table_compact_shared_in_parallel()
    case ("table_reserve_negative")
        call scenario_table_reserve_negative()
    case ("table_reserve_columns_negative")
        call scenario_table_reserve_columns_negative()
    case ("table_reserve_columns_shared_in_parallel")
        call scenario_table_reserve_columns_shared_in_parallel()
    case ("table_key_direction_conflict")
        call scenario_table_key_direction_conflict()
    case ("table_key_list_long_preview")
        call scenario_table_key_list_long_preview()
    case ("table_key_list_empty")
        call scenario_table_key_list_empty()
    case ("table_key_list_bad_direction")
        call scenario_table_key_list_bad_direction()
    case ("table_require_columns_missing")
        call scenario_table_require_columns_missing()
    case ("table_require_columns_long_name")
        call scenario_table_require_columns_long_name()
    case ("table_require_columns_many_missing")
        call scenario_table_require_columns_many_missing()
    case ("filter_bool_ordering")
        call scenario_filter_bool_ordering()
    case ("table_read_during_append")
        call scenario_table_read_during_append()
    case ("table_append_during_read")
        call scenario_table_append_during_read()
    case ("table_add_column_shared_in_parallel")
        call scenario_table_add_column_shared_in_parallel()
    case ("table_write_shared_in_parallel")
        call scenario_table_write_shared_in_parallel()
    case ("table_row_index_shared_in_parallel")
        call scenario_table_row_index_shared_in_parallel()
    case ("table_set_null_no_validity_in_parallel")
        call scenario_table_set_null_no_validity_in_parallel()
    case ("table_string_write_shared_in_parallel")
        call scenario_table_string_write_shared_in_parallel()
    case ("table_bind_missing_column")
        call scenario_table_bind_missing_column()
    case ("table_bind_width_mismatch")
        call scenario_table_bind_width_mismatch()
    case ("table_bind_kind_refused")
        call scenario_table_bind_kind_refused()
    case ("table_drop_predefined")
        call scenario_table_drop_predefined()
    case ("codegen_row_index_out_of_range")
        call scenario_codegen_row_index_out_of_range()
    case ("codegen_range_out_of_range")
        call scenario_codegen_range_out_of_range()
    case ("codegen_missing_file_column")
        call scenario_codegen_missing_file_column()
    case ("codegen_init_exact_refuses")
        call scenario_codegen_init_exact_refuses()
    case ("codegen_init_exact_control")
        call scenario_codegen_init_exact_control()
    case ("codegen_computed_roundtrip")
        call scenario_codegen_computed_roundtrip()
    case ("reindex_trusted_length_mismatch")
        call scenario_reindex_trusted_length_mismatch()
    case ("permute_assume_valid_short_perm")
        call scenario_permute_assume_valid_short_perm()
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

    !> parquet_assert_column_type's type-mismatch check, reached via parquet_write_column_chunk.
    !> The chunked path converts between numeric kinds exactly as parquet_write_column does, so
    !> what is left to refuse is a pair parquet_is_type_compatible rejects outright: a logical
    !> chunk written to an int32-declared column. Keep the pair INCOMPATIBLE if this fixture is
    !> ever changed -- an int32 chunk into a float64 column is now a supported conversion and
    !> would make this scenario exit 0 (fires before any row group needs to be open, so no
    !> parquet_new_row_group here).
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
    !> A schema-enforced writer closed with NOTHING written must not abort: it writes every declared
    !! column with 0 rows and says so with a WARNING. Exits 0; the wrapper asserts the warning text.
    !! This is the case an analysis stage that legitimately produced no rows lands in.
    subroutine scenario_close_writer_no_columns_written()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer

        schema = parquet_schema(table="empty_ok")
        call schema%add_field("col_a", "int32")
        call schema%add_field("col_b", "string", array_size=4)

        call parquet_open_writer(writer, "test_run/error_scenario_no_columns_written.parquet", schema)
        call parquet_close_writer(writer)
        print '(a)', "closed with no columns written, as expected"
    end subroutine scenario_close_writer_no_columns_written

    !> The NEGATIVE CONTROL for the warning above: a caller who writes the zero-length arrays
    !! himself must get the same file and NO warning, since the library did not have to step in.
    !! Without this, the warning test would pass against an implementation that warned on every
    !! zero-row close. Exits 0; the wrapper asserts the warning text is ABSENT.
    subroutine scenario_close_writer_zero_length_writes_quiet()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: no_ints(0)
        character(len=4) :: no_strs(0)

        schema = parquet_schema(table="empty_explicit")
        call schema%add_field("col_a", "int32")
        call schema%add_field("col_b", "string", array_size=4)

        call parquet_open_writer(writer, "test_run/error_scenario_zero_length_writes.parquet", schema)
        call parquet_write_column(writer, "col_a", no_ints)
        call parquet_write_column(writer, "col_b", no_strs)
        call parquet_close_writer(writer)
        print '(a)', "closed after explicit zero-length writes, as expected"
    end subroutine scenario_close_writer_zero_length_writes_quiet

    !> A row mask means rows were EXPECTED, so the empty-close path deliberately does not apply and
    !! the missing-write abort stands. Keeps the mask's own contract intact: the mask must be fully
    !! consumed by what is written, and writing nothing cannot consume it.
    subroutine scenario_close_writer_no_columns_with_mask()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: mask(3) = [.true., .false., .true.]

        schema = parquet_schema(table="empty_masked")
        call schema%add_field("col_a", "int32")

        call parquet_open_writer(writer, "test_run/error_scenario_no_columns_masked.parquet", schema)
        call parquet_write_row_mask(writer, mask)
        call parquet_close_writer(writer)   ! -> aborts: a mask was set, so rows were expected
        print '(a)', "unexpectedly closed a masked writer with no columns written"
    end subroutine scenario_close_writer_no_columns_with_mask

    subroutine scenario_close_writer_missing_write()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        schema = parquet_schema(table="demo")
        call schema%add_field("col_a", "int32")
        call schema%add_field("col_b", "int32")
        call schema%add_field("col_c", "int32")

        call parquet_open_writer(writer, "test_run/error_scenario_missing_write.parquet", schema)
        call parquet_write_column(writer, "col_a", data)
        call parquet_write_column(writer, "col_b", data)
        ! col_c is never written.
        call parquet_close_writer(writer)
    end subroutine scenario_close_writer_missing_write

    !> Same as scenario_close_writer_missing_write, but with BOTH output knobs turned against it:
    !> verbosity="errors_only" and message_stream="stderr".
    !>
    !> "errors_only" rather than "silent" on purpose, and the distinction is the whole point of the
    !> control below: the levels are normal(0) < silent(1) < errors_only(2), and parquet_emit_warning
    !> returns only at `cfg_verbosity >= verb_errors_only`. "silent" quiets informational and
    !> solicited output and leaves warnings alone, so a control emitted under it would still print
    !> and would prove nothing about the knob being in force.
    !>
    !> Neither may reach the error-context lines. parquet_emit_error_context
    !> (src/parquet_settings_base.f90) writes to output_unit unconditionally -- it does not test
    !> cfg_verbosity and does not consult message_unit() -- because those lines carry the filename
    !> and schema name that parquet_close_writer's abort message deliberately leaves out. Routing
    !> them through parquet_emit_warning instead would let verbosity="errors_only" produce an abort
    !> naming no file at all, which is the failure this arrangement exists to prevent.
    !>
    !> So the abort here must still print its two context lines on STDOUT while the ERROR STOP goes
    !> to stderr, exactly as it does with both knobs at their defaults. The warning emitted first is
    !> the negative control and must vanish: it proves the knobs were really in force, which is what
    !> separates "this channel ignores the settings" from "the settings did nothing at all".
    subroutine scenario_close_writer_missing_write_silenced()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        call parquet_set_verbosity("errors_only")
        call parquet_set_message_stream("stderr")
        ! The control: a warning emitted under these settings, which must reach neither stream.
        call parquet_emit_warning("control warning that must be suppressed")

        schema = parquet_schema(table="demo")
        call schema%add_field("col_a", "int32")
        call schema%add_field("col_b", "int32")

        call parquet_open_writer(writer, "test_run/error_scenario_missing_write_silenced.parquet", schema)
        call parquet_write_column(writer, "col_a", data)
        ! col_b is never written, so the close below aborts -- after printing its context.
        call parquet_close_writer(writer)
    end subroutine scenario_close_writer_missing_write_silenced

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
    !> MAML excluded it (see is_deactivated's doc comment in src/parquet_core.f90).
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
    !> scalar types" note and doc/pages/types/supported-data-types.md). Each reads
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

    !> parquet_reader_print_stat's "screened:" line, which only appears when the row-group
    !> statistics pre-screen actually skipped something -- so it needs a MULTI-row-group fixture
    !> and a filter selective enough to rule some of them out, which the filtered-rows scenario
    !> above (one row group, five rows) cannot provide.
    subroutine scenario_print_stat_screened_rows()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: a_values(40), a_back(5)
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_screened_rows.parquet"

        a_values = [(i, i=1,40)]

        call parquet_open_writer(writer, out_file, chunk_size=10)
        call parquet_write_column(writer, "a", a_values)
        call parquet_close_writer(writer)

        ! Row groups hold 1..10, 11..20, 21..30, 31..40; "a > 35" can only match in the last one,
        ! so three of the four are ruled out from the footer alone.
        call filt%add("a > 35")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_read_column(reader, "a", a_back)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the 'screened: N of M row groups skipped' branch"
    end subroutine scenario_print_stat_screened_rows

    !> A STREAMED string column is stored as arrow::large_utf8() however small it is, unlike the
    !> whole-column write, which picks utf8/large_utf8 from the column's own byte payload -- see
    !> doc/pages/types/supported-data-types.md's "Large string columns", and
    !> parquet_write_string_column_chunk's own comment in parquet_wrapper.cpp for why (the field is
    !> fixed when the first row group locks the schema, long before every row group's bytes have
    !> been seen, so it takes the 64-bit form rather than risk rejecting a later row group).
    !>
    !> Runs out of process because the only observable is parquet_close_reader(print_stat=.true.)'s
    !> `parquet_type` column on stdout, which an in-process test-drive test cannot capture. Exits 0:
    !> nothing here aborts. Its NEGATIVE CONTROL is a separate scenario,
    !> whole_string_is_plain_utf8, which writes the very same six strings through
    !> parquet_write_column and must report a plain `string` -- kept separate rather than combined
    !> into one scenario (as this test was first sketched) because scenario_capture_contains does a
    !> plain substring search over the whole capture, and "string" occurs inside "large_string", so
    !> two tables in one capture cannot be told apart.
    subroutine scenario_streamed_string_is_large_utf8()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/error_scenario_streamed_string_large.parquet"
        character(len=8) :: s_values(6) = [character(len=8) :: &
            "alpha", "bravo", "charlie", "delta", "echo", "foxtrot"]
        character(len=8) :: s_back(6)

        ! Six short strings: ~40 bytes of payload, nowhere near the real ~2GiB offset limit, so a
        ! whole-column write of these same values stays on plain utf8 (see the control scenario).
        call parquet_open_writer(writer, out_file)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "streamed", s_values(1:3))
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "streamed", s_values(4:6))
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "streamed", s_back)
        if (any(s_back /= s_values)) error stop "streamed string column did not round-trip"
        ! print_stat's parquet_type column is what carries the answer to stdout.
        call parquet_close_reader(reader, print_stat=.true.)
    end subroutine scenario_streamed_string_is_large_utf8

    !> The negative control for scenario_streamed_string_is_large_utf8, above: the same six short
    !> strings written whole rather than streamed must be stored as plain utf8, so print_stat
    !> reports `string` and NOT `large_string`. Without this half, the streamed assertion would
    !> pass just as happily against a build that had started promoting every string column.
    subroutine scenario_whole_string_is_plain_utf8()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/error_scenario_whole_string_plain.parquet"
        character(len=8) :: s_values(6) = [character(len=8) :: &
            "alpha", "bravo", "charlie", "delta", "echo", "foxtrot"]
        character(len=8) :: s_back(6)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "whole", s_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "whole", s_back)
        if (any(s_back /= s_values)) error stop "whole-column string did not round-trip"
        call parquet_close_reader(reader, print_stat=.true.)
    end subroutine scenario_whole_string_is_plain_utf8

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
        integer(int32) :: vec_data(3, 4), row_buf(3), elem_buf(4), sca_data(4)
        integer :: col_size_back
        integer(int64) :: total_elems

        vec_data = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4])
        sca_data = [10, 20, 30, 40]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "vec", vec_data)
        call parquet_write_column(writer, "sca", sca_data)
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

        ! The SCALAR column is the case parquet_open_table depends on: it asks for col_size on
        ! every column to tell a scalar from a vector, so a scalar column that still decoded its
        ! data here would make a lazy open cost a full read of the whole file -- silently, since
        ! the data is discarded again immediately and only the timing gives it away.
        call parquet_get_col_size(reader, "sca", col_size_back)
        if (col_size_back /= 1) error stop "col_size mismatch for scalar column"

        call parquet_get_column_total_elements(reader, "sca", total_elems)
        if (total_elems /= 4_int64) error stop "total element count mismatch for scalar column"

        call parquet_close_reader(reader)
        ! And the same thing end to end: opening a table classifies every column, and must do so
        ! without decoding any of them. Declared in a block so the table is finalized here, while
        ! the forced error is still armed, rather than at the end of the subroutine.
        block
            type(parquet_table) :: tbl
            call parquet_open_table(tbl, out_file)
            if (tbl%ncols() /= 2) error stop "table should see both columns"
            if (tbl%width("sca") /= 1) error stop "scalar column width mismatch after table open"
            if (tbl%width("vec") /= 3) error stop "vector column width mismatch after table open"
        end block

        call parquet_debug_set_force_whole_column_read_error(0)
        print '(a)', "parquet_get_col_size/parquet_get_column_total_elements/" // &
            "parquet_read_array_row_mode/parquet_read_array_element_mode all avoided a whole-column read, as expected"
    end subroutine scenario_col_size_and_row_mode_avoid_whole_column_read

    !> The same guarantee as scenario_col_size_and_row_mode_avoid_whole_column_read, but with a
    !> ROW FILTER active -- which used to be the one case where both modes deliberately gave it up
    !> and read the whole (filtered) column, because `row_index`/`elem_index` address the filtered
    !> result rather than a physical file row and nothing mapped one to the other.
    !>
    !> They now map it by walking each row group's SURVIVING row count instead of its physical one
    !> (resolve_row_group_for_row / stream_element_mode_row_groups in parquet_wrapper.cpp), so the
    !> filtered case is row-group-scoped exactly like the unfiltered one. All four entry-point
    !> families are exercised, since three of them carry their own copy of that logic rather than
    !> sharing one: the int32 template, the hand-written logical and string pair, and the temporal
    !> template.
    !>
    !> Same mechanism as the scenario above: parquet_debug_set_force_whole_column_read_error is
    !> armed AFTER the reader is opened (opening with filter= legitimately reads the filter column
    !> whole-file to build the mask), so this scenario finishing without aborting is the assertion.
    !> Its negative control is the shared scenario_whole_column_read_forced_error_control.
    subroutine scenario_filter_row_element_mode_no_whole_column_read()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_filter_row_element_mode_no_whole_column_read.parquet"
        integer(int32) :: id(8), vec(3, 8), row_buf(3), elem_buf(6)
        logical :: flg(2, 8), flg_row(2), flg_elem(6)
        character(len=8) :: txt(2, 8), txt_row(2), txt_elem(6)
        type(parquet_date) :: dt(2, 8), dt_row(2), dt_elem(6)
        integer(int64) :: nrows, total_elems
        integer :: i, col_size_back

        do i = 1, 8
            id(i) = i
            vec(:, i) = [100 * i + 1, 100 * i + 2, 100 * i + 3]
            flg(1, i) = mod(i, 2) == 0
            flg(2, i) = i > 4
            write(txt(1, i), '(a,i0)') "a", i
            write(txt(2, i), '(a,i0)') "b", i
            dt(1, i) = parquet_date(2024, 1, i)
            dt(2, i) = parquet_date(2024, 6, i)
        end do

        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", vec)
        call parquet_write_column(writer, "flg", flg)
        call parquet_write_column(writer, "txt", txt)
        call parquet_write_column(writer, "dt", dt)
        call parquet_close_writer(writer)

        ! "id > 2" leaves physical rows 3..8, i.e. row group 1 empty -- so filtered row 1 is
        ! physical row 3, and a mapping still walking physical counts would return row 1's values.
        call filt%add("id > 2")
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        if (nrows /= 6_int64) error stop "filtered reader should report 6 surviving rows"

        call parquet_debug_set_force_whole_column_read_error(1)

        call parquet_get_col_size(reader, "vec", col_size_back)
        if (col_size_back /= 3) error stop "col_size mismatch for the filtered vector column"
        call parquet_get_column_total_elements(reader, "vec", total_elems)
        if (total_elems /= 18_int64) error stop "total element count mismatch for the filtered vector column"

        call parquet_read_array_row_mode(reader, "vec", row_buf, 1)
        if (any(row_buf /= [301, 302, 303])) error stop "filtered row 1 should be physical row 3"
        call parquet_read_array_element_mode(reader, "vec", elem_buf, 2)
        if (any(elem_buf /= [302, 402, 502, 602, 702, 802])) error stop "filtered element 2 should span rows 3..8"

        call parquet_read_array_row_mode(reader, "flg", flg_row, 1)
        if (flg_row(1) .or. flg_row(2)) error stop "filtered logical row 1 should be physical row 3's flags"
        call parquet_read_array_element_mode(reader, "flg", flg_elem, 1)
        if (.not. all(flg_elem .eqv. [.false., .true., .false., .true., .false., .true.])) &
            error stop "filtered logical element 1 should span rows 3..8"

        call parquet_read_array_row_mode(reader, "txt", txt_row, 1)
        if (txt_row(1) /= "a3" .or. txt_row(2) /= "b3") error stop "filtered string row 1 should be physical row 3"
        call parquet_read_array_element_mode(reader, "txt", txt_elem, 2)
        if (txt_elem(1) /= "b3" .or. txt_elem(6) /= "b8") error stop "filtered string element 2 should span rows 3..8"

        call parquet_read_array_row_mode(reader, "dt", dt_row, 1)
        if (.not. (dt_row(1) == parquet_date(2024, 1, 3))) error stop "filtered date row 1 should be physical row 3"
        call parquet_read_array_element_mode(reader, "dt", dt_elem, 1)
        if (.not. (dt_elem(6) == parquet_date(2024, 1, 8))) error stop "filtered date element 1 should end at row 8"

        call parquet_close_reader(reader)
        call parquet_debug_set_force_whole_column_read_error(0)
        print '(a)', "row mode and element mode on a filtered reader avoided a whole-column read, as expected"
    end subroutine scenario_filter_row_element_mode_no_whole_column_read

    !> ---- Read-time sort (parquet_sortkey / sort_by=) abort paths ----
    !>
    !> Every rejection below is an error stop or a C++-side abort, so none of them can live in
    !> test/test_sort.f90 -- the process dies. They share one tiny fixture helper so each scenario
    !> is just the offending call.

    !> Writes the small fixture every sort scenario below sorts: a scalar key `v`, a vector column
    !> `vec` (an invalid sort key), and a string column `txt`.
    subroutine write_sort_scenario_fixture(out_file)
        character(len=*), intent(in) :: out_file !! fixture path (one per scenario).
        type(parquet_writer) :: writer
        integer(int32) :: v(4) = [30, 10, 40, 20]
        integer(int32) :: vec(2, 4)
        character(len=4) :: txt(4) = ["dd  ", "bb  ", "aa  ", "cc  "]
        integer :: i

        do i = 1, 4
            vec(:, i) = [10 * i + 1, 10 * i + 2]
        end do
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "vec", vec)
        call parquet_write_column(writer, "txt", txt)
        call parquet_close_writer(writer)
    end subroutine write_sort_scenario_fixture

    !> A sort key naming a column the file does not have aborts, naming the column.
    subroutine scenario_sort_unknown_column()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_unknown_column.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("nosuch asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (unknown column in sort key)
        print '(a)', "unexpectedly sorted by a column that does not exist"
    end subroutine scenario_sort_unknown_column

    !> A vector column has no single value per row to order by, so it is rejected -- from the
    !> schema, before any data is read.
    subroutine scenario_sort_vector_column()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_vector_column.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("vec asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (vector column)
        print '(a)', "unexpectedly sorted by a vector column"
    end subroutine scenario_sort_vector_column

    !> ---- Process-global settings (parquet_settings) abort paths ----

    !> A thread-pool capacity below 1 is rejected in Fortran, before the C++ side is called at all
    !> (which is what makes the C++ side's own identical check dead code -- see the GCOVR_EXCL note
    !> on parquet_set_arrow_threads in parquet_wrapper.cpp).
    subroutine scenario_set_arrow_threads_zero()

        call parquet_set_arrow_threads(0)   ! -> aborts (capacity must be >= 1)
        print '(a)', "unexpectedly accepted a thread-pool capacity of 0"
    end subroutine scenario_set_arrow_threads_zero

    !> A codec name outside the supported set. Validated against the same list parquet_open_writer
    !> checks its own compression= argument against, so a codec cannot be settable as a default and
    !> rejected as an argument, or the reverse.
    subroutine scenario_settings_bad_codec()

        call parquet_set_default_compression("lzma")   ! -> aborts (unknown codec)
        print '(a)', "unexpectedly accepted an unknown default compression codec"
    end subroutine scenario_settings_bad_codec

    !> A negative thread cap. 0 is legal and means "automatic"; below that is meaningless.
    subroutine scenario_settings_negative_sort_threads()

        call parquet_set_sort_threads(-1)   ! -> aborts (must be >= 0)
        print '(a)', "unexpectedly accepted a negative sort thread cap"
    end subroutine scenario_settings_negative_sort_threads


    !> The counting path's bucket ceiling, same guard and same negative control.
    subroutine scenario_settings_negative_bucket_limit()

        call parquet_set_sort_counting_bucket_limit(0)     ! legal: restores the built-in default
        call parquet_set_sort_counting_bucket_limit(-8)    ! -> aborts (must be >= 0)
        print '(a)', "unexpectedly accepted a negative sort_counting_bucket_limit"
    end subroutine scenario_settings_negative_bucket_limit

    !> The row-group byte target, same guard and same negative control. Uses the int64 form, so the
    !> two kinds of the same generic are between them covered by an abort test as well.
    subroutine scenario_settings_negative_row_group_bytes()

        call parquet_set_target_row_group_bytes(0_int64)       ! legal: restores the built-in default
        call parquet_set_target_row_group_bytes(-1024_int64)   ! -> aborts (must be >= 0)
        print '(a)', "unexpectedly accepted a negative target_row_group_bytes"
    end subroutine scenario_settings_negative_row_group_bytes

    !> Sets an environment variable for this process. Fortran cannot, so this is POSIX `setenv`
    !> through a local bind(C) interface -- test-only, exactly as every parquet_debug_* hook here is,
    !> so no src/ file gains a POSIX dependency. Shared by the six env scenarios below.
    subroutine scenario_setenv(name, value)
        use iso_c_binding, only : c_int
        character(len=*), intent(in) :: name !! variable to set.
        character(len=*), intent(in) :: value !! its value.
        interface
            function c_setenv(nm, val, overwrite) bind(C, name="setenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*) !! NUL-terminated name.
                character(kind=c_char), intent(in) :: val(*) !! NUL-terminated value.
                integer(c_int), value :: overwrite !! nonzero replaces an existing value.
                integer(c_int) :: rc !! 0 on success.
            end function c_setenv
        end interface
        integer :: rc

        rc = int(c_setenv(name // char(0), value // char(0), int(1, kind=c_int)))
        ! Checked rather than discarded: a failed setenv leaves the old value in place, and the
        ! scenario would then exercise an environment nobody set.
        if (rc /= 0) error stop "set_env: setenv failed for '" // name // "'"
    end subroutine scenario_setenv

    !> An unknown token. The abort must name the VARIABLE, not just the setter -- a config-style
    !> feature whose error does not say which key was wrong is most of the way to useless.
    subroutine scenario_settings_env_bad_token()

        call scenario_setenv("PARQUET_FORTRAN_VERBOSITY", "loud")
        call parquet_settings_from_env()   ! -> aborts naming the variable
        print '(a)', "unexpectedly accepted an unknown verbosity token from the environment"
    end subroutine scenario_settings_env_bad_token

    !> A value that is not a number at all.
    subroutine scenario_settings_env_not_an_integer()

        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", "many")
        call parquet_settings_from_env()   ! -> aborts (not an integer)
        print '(a)', "unexpectedly accepted a non-numeric integer from the environment"
    end subroutine scenario_settings_env_not_an_integer

    !> **The trap a list-directed read would fall into.** `read(text, *, iostat=)` accepts "4 8"
    !> with iostat == 0 and yields 4, so a stray copy-paste or a shell variable that expanded to two
    !> words would silently set the cap to 4 and report success. The strict parser must reject it.
    subroutine scenario_settings_env_two_numbers()

        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", "4 8")
        call parquet_settings_from_env()   ! -> aborts (not an integer)
        print '(a)', "unexpectedly accepted two numbers as one integer from the environment"
    end subroutine scenario_settings_env_two_numbers

    !> A build that cannot reproduce the frozen transform is refused rather than returning
    !> quietly different permutations.
    !!
    !! **The negative control is the first call and is the point of the scenario.** A guard that
    !! fired unconditionally would pass every abort test ever written for it; running the same call
    !! successfully before the hook is set is what proves the abort is caused by the forced failure
    !! and not by the call itself.
    !!
    !! The real trigger is a build with `-ffast-math`, or ifx at its default `-fp-model=fast`, which
    !! no in-process test can produce -- hence the hook.
    subroutine scenario_weighted_contract_failure()
        real(real64) :: w(4)
        integer :: perm(4)

        w = 1.0_real64
        call pf_weighted_permutation(perm, w, 1_int64)   ! negative control: must SUCCEED
        call parquet_debug_set_exp_key_contract(.false.)
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (frozen transform not reproduced)
        print '(a)', "unexpectedly accepted a build that fails the frozen-transform check"
    end subroutine scenario_weighted_contract_failure

    !> A negative weight is refused rather than silently drawn FIRST.
    !!
    !! The key of a negative-weight item is negative, so the race would place it ahead of every
    !! real item -- the exact opposite of any sane reading of "this item is unlikely".
    subroutine scenario_weighted_negative_weight()
        type(pf_weighted_draw) :: d
        real(real64) :: w(4)

        w = [1.0_real64, 2.0_real64, -0.5_real64, 1.0_real64]
        call d%init(w, 1_int64)            ! -> aborts (a weight is negative)
        print '(a)', "unexpectedly accepted a negative weight"
    end subroutine scenario_weighted_negative_weight

    !> An all-zero weight vector names no distribution and is refused.
    subroutine scenario_weighted_all_zero()
        type(pf_weighted_draw) :: d
        real(real64) :: w(4)

        w = 0.0_real64
        call d%init(w, 1_int64)            ! -> aborts (every weight is zero)
        print '(a)', "unexpectedly accepted an all-zero weight vector"
    end subroutine scenario_weighted_all_zero

    !> A NaN weight is refused rather than quietly filed as a zero.
    !!
    !! A NaN compares false against every bound, so `w > 0` and `w < 0` are both false and a guard
    !! written without an explicit NaN test would classify it as zero-weight and drop it to the
    !! tail. That is a silently wrong answer, which is why the test exists at all.
    subroutine scenario_weighted_nan_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_weighted_draw) :: d
        real(real64) :: w(3)

        w = [1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64]
        call d%init(w, 1_int64)            ! -> aborts (a weight is NaN)
        print '(a)', "unexpectedly accepted a NaN weight"
    end subroutine scenario_weighted_nan_weight

    !> Drawing from a sampler that was never initialised aborts instead of reading garbage.
    subroutine scenario_weighted_next_uninitialised()
        type(pf_weighted_draw) :: d
        integer :: item
        logical :: ok

        call d%next(item, ok)              ! -> aborts (never initialised)
        print '(a,i0)', "unexpectedly drew from an uninitialised sampler: ", item
    end subroutine scenario_weighted_next_uninitialised

    !> A second `%init` is refused, so a sequence in progress cannot be discarded by accident.
    !!
    !! `%reseed` is the supported way to reuse a sampler and is `O(k log n)` where a rebuild is
    !! `O(n)`, so a caller reaching for `%init` twice is nearly always reaching for the wrong one.
    subroutine scenario_weighted_init_twice()
        type(pf_weighted_draw) :: d
        real(real64) :: w(4)

        w = 1.0_real64
        call d%init(w, 1_int64)
        call d%init(w, 2_int64)            ! -> aborts (already initialised)
        print '(a)', "unexpectedly accepted a second %init"
    end subroutine scenario_weighted_init_twice

    !> A subset larger than its population is refused.
    subroutine scenario_weighted_subset_too_large()
        real(real64) :: w(3)
        integer :: idx(5)

        w = 1.0_real64
        call pf_weighted_subset(idx, w, 1_int64)   ! -> aborts (5 items from a population of 3)
        print '(a,i0)', "unexpectedly drew a subset larger than its population: ", idx(1)
    end subroutine scenario_weighted_subset_too_large

    !> A permutation array that does not match the weights is refused.
    subroutine scenario_weighted_perm_size_mismatch()
        real(real64) :: w(6)
        integer :: perm(4)

        w = 1.0_real64
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (4 slots for 6 items)
        print '(a,i0)', "unexpectedly returned a short permutation: ", perm(1)
    end subroutine scenario_weighted_perm_size_mismatch

    !> An environment variable longer than the fixed buffer the reader uses is refused rather than
    !> applied truncated -- a truncated number is a plausible-looking wrong value, which is exactly
    !> the failure this whole strict-parsing family exists to prevent.
    subroutine scenario_settings_env_too_long()
        character(len=5000) :: huge_value

        huge_value = repeat("1", 5000)
        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", trim(huge_value))
        call parquet_settings_from_env()   ! -> aborts (longer than the buffer)
        print '(a)', "unexpectedly accepted an over-long environment value"
    end subroutine scenario_settings_env_too_long

    !> A knob whose Fortran type is a default INTEGER is parsed in int64 first and then range-checked,
    !> so that a value beyond int32 is refused rather than wrapping into a plausible small number.
    !> The int64 parse succeeding is what makes this a different failure from "not an integer".
    subroutine scenario_settings_env_int32_out_of_range()

        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", "3000000000")
        call parquet_settings_from_env()   ! parses as int64, does not fit int32 -> aborts
        print '(a)', "unexpectedly accepted a value beyond the range of a default INTEGER"
    end subroutine scenario_settings_env_int32_out_of_range

    !> The rejection message echoes the offending value, which is caller-controlled text of
    !> unbounded length -- and ifx's ERROR STOP runtime corrupts the heap once the composed message
    !> reaches 8192 bytes, so the guard caps it to a short preview (CLAUDE.md). This is the value
    !> that exercises the capping: long enough to be truncated, short enough to reach the parser.
    subroutine scenario_settings_env_long_value_preview()
        character(len=300) :: long_value

        long_value = repeat("x", 300)
        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", trim(long_value))
        call parquet_settings_from_env()   ! -> aborts, with the value shown truncated
        print '(a)', "unexpectedly accepted a long non-numeric environment value"
    end subroutine scenario_settings_env_long_value_preview

    !> Not an error scenario: `parquet_emit_info` is the INFORMATIONAL output channel, the quietest
    !> of the three, and `verbosity="silent"` is meant to suppress it while leaving real warnings
    !> alone. Its only call site in the library is a development-build remark that a released build
    !> never reaches, so nothing exercised the channel itself.
    !>
    !> Both halves are printed here: the message at the default verbosity, then a second one with
    !> `verbosity="silent"` set, which must NOT appear. The second is the negative control -- a
    !> channel that ignored the setting would still pass a test that only looked for the first.
    !> Two claims from `doc/pages/operating/performance.md`'s memory section, in one process because
    !> they share one observable.
    !>
    !> **T1: the size queries read NO column data.** `parquet_get_col_size` and
    !> `parquet_get_column_total_elements` answer from the schema for every column this library
    !> writes. Three scenarios already assert that neither takes the WHOLE-column path
    !> (`col_size_and_row_mode_avoid_whole_column_read` and its two siblings), which is strictly
    !> weaker: a helper that decoded every row group one at a time would satisfy all three. This
    !> asserts the stronger thing the page actually claims, using the physical-read counter.
    !>
    !> **T2: a column stays cached in the reader, so re-reading it is free.**
    !> `nested_struct_shares_cached_read` asserts one physical read for two struct LEAF PATHS, which
    !> is a different property -- nothing asserted it for a plain column read twice.
    !>
    !> **Both negative controls are the same counter moving.** An assertion that a counter is 0, or
    !> that it did not change, passes perfectly against a counter that never increments at all; so
    !> the first `parquet_read_column` must take it above 0, and reading a DIFFERENT column must
    !> take it up again. Without those two, deleting the counter's increment would leave this green.
    !>
    !> Expected exit is 0, so reaching any `error stop` here is itself the failure signal -- the
    !> same shape `nested_struct_shares_cached_read` uses.
    subroutine scenario_size_queries_read_no_column_data()
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

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_size_queries_read_no_column_data.parquet"
        integer(int32) :: id(8), vec(3, 8), got_id(8), got_id2(8), got_vec(3, 8)
        integer(int64) :: total_elems, after_size, after_first, after_repeat, after_other
        integer :: i, col_size_back
        character(len=32) :: n_str

        do i = 1, 8
            id(i) = i
            vec(:, i) = [100 * i + 1, 100 * i + 2, 100 * i + 3]
        end do
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call parquet_debug_reset_physical_column_read_count()
        call parquet_open_reader(reader, out_file)

        ! T1. A scalar column's width is 1 by construction and a FIXED_SIZE_LIST's is in the schema,
        ! so neither of these four calls may touch the data.
        call parquet_get_col_size(reader, "id", col_size_back)
        if (col_size_back /= 1) error stop "size_queries: scalar col_size should be 1"
        call parquet_get_col_size(reader, "vec", col_size_back)
        if (col_size_back /= 3) error stop "size_queries: vector col_size should be 3"
        call parquet_get_column_total_elements(reader, "id", total_elems)
        if (total_elems /= 8_int64) error stop "size_queries: scalar total_elements should be 8"
        call parquet_get_column_total_elements(reader, "vec", total_elems)
        if (total_elems /= 24_int64) error stop "size_queries: vector total_elements should be 24"

        after_size = parquet_debug_get_physical_column_read_count()
        if (after_size /= 0_int64) then
            write(n_str, '(i0)') after_size
            error stop "size_queries: the size queries read column data (" // trim(n_str) // &
                " physical read(s)); they must answer from the schema alone"
        end if

        ! Negative control for T1, and the one that makes the 0 above mean anything: the counter
        ! must be able to move at all.
        call parquet_read_column(reader, "id", got_id)
        after_first = parquet_debug_get_physical_column_read_count()
        if (after_first < 1_int64) &
            error stop "size_queries: the physical-read counter never moved, so asserting 0 proved nothing"

        ! T2: the same column again is served from the reader's cache.
        call parquet_read_column(reader, "id", got_id2)
        after_repeat = parquet_debug_get_physical_column_read_count()
        if (after_repeat /= after_first) then
            write(n_str, '(i0)') after_repeat - after_first
            error stop "size_queries: re-reading a cached column cost " // trim(n_str) // &
                " further physical read(s); it must be free"
        end if
        if (any(got_id2 /= got_id)) error stop "size_queries: the cached re-read returned different values"

        ! Negative control for T2: a DIFFERENT column is not in the cache and must cost a read.
        call parquet_read_column(reader, "vec", got_vec)
        after_other = parquet_debug_get_physical_column_read_count()
        if (after_other <= after_repeat) &
            error stop "size_queries: reading a second column cost no physical read, so the cache " // &
                "assertion above could not have failed either"

        call parquet_close_reader(reader)
        print '(a)', "size queries and the read cache exercised"
    end subroutine scenario_size_queries_read_no_column_data
    !
    !> The affinity-clamp warning: it fires, its text is what the guide quotes, and it fires
    !> **once per process**.
    !>
    !> **Why an error scenario for something that does not abort.** The observable is a line on the
    !> library's message stream, and a test-drive test cannot see its own process's output. Expected
    !> exit is 0; `test_errors.f90`'s wrapper is what reads the streams.
    !>
    !> **How "once per process" is asserted without counting lines.** The two provocations are sent
    !> to DIFFERENT streams: the first with `message_stream="stderr"`, the second with `"stdout"`.
    !> A correctly claimed warning therefore appears on stderr and **not** on stdout, which
    !> `check_scenario_streams` already asserts in both directions -- and its absence half is the
    !> whole point, since a warning that fired twice would put the second line on stdout. No
    !> line-counting helper is needed, and the assertion is stronger than a count would be, because
    !> it also pins which stream the setting sent it to.
    !>
    !> **The clamp is forced, not provoked.** `parquet_debug_set_affinity_procs(2)` is the only way
    !> a test can make it bite: a real clamp needs a process bound to fewer processors than
    !> `OMP_NUM_THREADS` asks for, and a process cannot bind itself after it has started. `threads=97`
    !> with 5000 rows is what is asked for. 5000 so the `count > nrows` clamp, which runs FIRST,
    !> leaves the 97 alone; and **97 rather than a round number so the message's "although N were
    !> requested" is pinned to the REQUEST**. It used to report `omp_get_max_threads()` there, which
    !> on a machine whose ICV happens to equal the request is indistinguishable from correct.
    subroutine scenario_sort_affinity_clamp_warns()
        real(real64) :: a(5000)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i

        do i = 1_int64, 5000_int64
            a(i) = real(mod(i*7919_int64, 5000_int64), real64)
        end do
        call parquet_debug_reset_affinity_warning()
        call parquet_debug_set_affinity_procs(2)

        call parquet_set_message_stream("stderr")
        call pf_argsort(a, perm, threads=97)

        ! Second provocation, on the OTHER stream. If the once-per-process claim holds, nothing
        ! lands here -- and that absence is what the wrapper checks.
        call parquet_set_message_stream("stdout")
        call pf_argsort(a, perm, threads=97)

        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
        print '(a)', "affinity clamp warning exercised"
    end subroutine scenario_sort_affinity_clamp_warns
    !
    !> Negative control 1: the same forced clamp under `verbosity="silent"` says nothing at all.
    !>
    !> Without this, the positive scenario passes just as happily against a warning that ignores the
    !> verbosity setting -- which is exactly what `doc/pages/operating/performance.md` promises it
    !> does not. The marker goes to stdout through `print`, which no setting governs, so the wrapper
    !> can tell "the scenario ran and said nothing" from "the scenario did not run".
    subroutine scenario_sort_affinity_clamp_silent()
        real(real64) :: a(5000)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i

        do i = 1_int64, 5000_int64
            a(i) = real(mod(i*7919_int64, 5000_int64), real64)
        end do
        call parquet_debug_reset_affinity_warning()
        call parquet_debug_set_affinity_procs(2)
        call parquet_set_verbosity("silent")
        call pf_argsort(a, perm, threads=97)
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
        print '(a)', "affinity clamp silent control exercised"
    end subroutine scenario_sort_affinity_clamp_silent
    !
    !> Negative control 2, and the important one: with NO clamp the identical sort says nothing.
    !>
    !> A warning that fired unconditionally would pass every assertion the positive scenario makes.
    !> This runs the same `pf_argsort(a, perm, threads=97)` against a mask WIDER than the request, so
    !> the only difference between the two scenarios is whether the mask is narrower than what was
    !> asked for.
    subroutine scenario_sort_affinity_clamp_absent()
        real(real64) :: a(5000)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i

        do i = 1_int64, 5000_int64
            a(i) = real(mod(i*7919_int64, 5000_int64), real64)
        end do
        call parquet_debug_reset_affinity_warning()
        ! A mask provably WIDER than the request, rather than the machine's real one: with the real
        ! mask this control would pass on a machine with 97 or more processors and fail on a smaller
        ! one, making it a statement about the runner rather than about the library.
        call parquet_debug_set_affinity_procs(1024)
        call pf_argsort(a, perm, threads=97)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
        print '(a)', "affinity clamp absent control exercised"
    end subroutine scenario_sort_affinity_clamp_absent
    !
    subroutine scenario_settings_emit_info_channel()

        call parquet_emit_info("info-channel-marker-visible")
        call parquet_set_verbosity("silent")
        call parquet_emit_info("info-channel-marker-suppressed")
        call parquet_reset_settings()
        print '(a)', "emit_info channel exercised"
    end subroutine scenario_settings_emit_info_channel

    !> `%reindex` validates its permutation before applying it: an out-of-range entry and a
    !> DUPLICATE entry are separate mistakes with separate messages, because a duplicate is the one
    !> that would otherwise silently drop an element and copy another twice, leaving a column that
    !> still validates. The valid reindex first is the negative control.
    subroutine scenario_strings_reindex_duplicate_index()
        type(parquet_string_column) :: c

        call c%append_string("a")
        call c%append_string("b")
        call c%append_string("c")
        call c%reindex([3_int64, 1_int64, 2_int64])   ! a real permutation: must NOT abort
        print '(a,i0)', "applied a valid permutation, size=", c%size()
        call c%reindex([1_int64, 2_int64, 2_int64])   ! 2 twice, 3 never -> aborts
        print '(a)', "unexpectedly accepted a permutation with a duplicate index"
    end subroutine scenario_strings_reindex_duplicate_index

    !> `%reindex_trusted` skips the O(n) range and duplicate scan for a caller that has already
    !> established the permutation is one -- but it keeps the O(1) LENGTH check, because a
    !> wrong-length permutation is a caller bug no amount of trust makes safe.
    subroutine scenario_strings_reindex_trusted_length_mismatch()
        type(parquet_string_column) :: c

        call c%append_string("a")
        call c%append_string("b")
        call c%append_string("c")
        call c%reindex_trusted([3_int64, 1_int64, 2_int64])   ! right length: must NOT abort
        print '(a,i0)', "applied a trusted permutation, size=", c%size()
        call c%reindex_trusted([1_int64, 2_int64])            ! two entries for three rows -> aborts
        print '(a)', "unexpectedly accepted a trusted permutation of the wrong length"
    end subroutine scenario_strings_reindex_trusted_length_mismatch

    !> `%copy_buffers` writes into caller-supplied arrays, so it checks both of them before writing
    !> anything -- an undersized offsets array is an out-of-bounds write, which a plain Fortran
    !> build does not catch. The correctly-sized call first is the negative control.
    subroutine scenario_strings_copy_buffers_offsets_too_short()
        type(parquet_string_column) :: c
        integer(int64) :: ok_offs(3), small_offs(2)
        character(len=1) :: data(3)

        call c%append_string("ab")
        call c%append_string("c")
        call c%copy_buffers(ok_offs, data)      ! size()+1 = 3: must NOT abort
        print '(a,i0)', "copied into a correctly sized offsets array, last offset=", ok_offs(3)
        call c%copy_buffers(small_offs, data)   ! 2 entries for size()+1 = 3 -> aborts
        print '(a)', "unexpectedly accepted an offsets array shorter than size()+1"
    end subroutine scenario_strings_copy_buffers_offsets_too_short

    !> The payload half of the same check; separate because the two arrays are sized from different
    !> quantities (element count against byte count) and a caller can get either one wrong alone.
    subroutine scenario_strings_copy_buffers_data_too_short()
        type(parquet_string_column) :: c
        integer(int64) :: offs(3)
        character(len=1) :: ok_data(3), small_data(2)

        call c%append_string("ab")
        call c%append_string("c")
        call c%copy_buffers(offs, ok_data)      ! character_size() = 3: must NOT abort
        print '(a,i0)', "copied into a correctly sized data array, bytes=", c%character_size()
        call c%copy_buffers(offs, small_data)   ! 2 bytes for 3 -> aborts
        print '(a)', "unexpectedly accepted a data array shorter than character_size()"
    end subroutine scenario_strings_copy_buffers_data_too_short

    !> A well-formed number the SETTER rejects. Proves the setter still does the range checking, so
    !> an environment value and a direct call fail identically rather than through two different
    !> guards that could drift.
    subroutine scenario_settings_env_out_of_range()

        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", "-1")
        call parquet_settings_from_env()   ! -> aborts with parquet_set_sort_threads' own message
        print '(a)', "unexpectedly accepted a negative sort thread cap from the environment"
    end subroutine scenario_settings_env_out_of_range

    !> A boolean spelling outside the accepted four. The message has to list them, or a user who
    !> typed "yes" has no way to learn what to type instead.
    subroutine scenario_settings_env_bad_boolean()

        call scenario_setenv("PARQUET_FORTRAN_STATISTICS_PRESCREEN", "yes")
        call parquet_settings_from_env()   ! -> aborts (not a boolean)
        print '(a)', "unexpectedly accepted 'yes' as a boolean from the environment"
    end subroutine scenario_settings_env_bad_boolean

    !> **The negative control for all five above**, and the assertion that a successful run is
    !> SILENT. Every one of the aborting scenarios would pass just as happily against a
    !> parquet_settings_from_env that aborted unconditionally; this one applies three valid values
    !> and must exit 0 having printed nothing at all.
    subroutine scenario_settings_env_clean_run()
        character(len=:), allocatable :: token

        call scenario_setenv("PARQUET_FORTRAN_SORT_THREADS", "4")
        call scenario_setenv("PARQUET_FORTRAN_VERBOSITY", "normal")
        call scenario_setenv("PARQUET_FORTRAN_STATISTICS_PRESCREEN", "true")
        call parquet_settings_from_env()
        call parquet_get_verbosity(token)
        if (parquet_get_sort_threads() /= 4 .or. token /= "normal" .or. &
            .not. parquet_get_statistics_prescreen()) then
            print '(a)', "valid environment values did not reach their knobs"
        end if
    end subroutine scenario_settings_env_clean_run

    !> `parquet_set_threads(0)` aborts, even though parquet_set_sort_threads(0) and
    !> parquet_set_prefetch_threads(0) are both legal and mean "automatic". Arrow's pool has no
    !> automatic value, so one argument cannot mean both things -- the valid case is exercised first
    !> so this cannot pass against a guard that rejects everything.
    subroutine scenario_settings_set_threads_zero()

        call parquet_set_threads(2)   ! legal
        call parquet_set_threads(0)   ! -> aborts (must be >= 1)
        print '(a)', "unexpectedly accepted parquet_set_threads(0)"
    end subroutine scenario_settings_set_threads_zero

    !> The prefetch cap's own version of the same guard.
    subroutine scenario_settings_negative_prefetch_threads()

        call parquet_set_prefetch_threads(-4)   ! -> aborts (must be >= 0)
        print '(a)', "unexpectedly accepted a negative prefetch thread cap"
    end subroutine scenario_settings_negative_prefetch_threads

    !> Provokes a FORTRAN-side warning (a qc violation on write) at a chosen verbosity and message
    !> stream, so the test can assert both whether it appeared and where.
    !>
    !> **The fixture path is derived from the arguments, because this helper backs THREE scenario
    !> names and `tools/run_error_scenarios.sh` runs scenarios concurrently** (`xargs -P`) -- see
    !> `scenario_settings_cpp_warning` for what a shared path does.
    subroutine scenario_settings_warning(level, stream)
        character(len=*), intent(in) :: level  !! verbosity to set first.
        character(len=*), intent(in) :: stream !! message stream to set first.
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        character(len=:), allocatable :: out_file
        integer(int32) :: v(4) = [1, 2, 3, 400]

        out_file = "test_run/scenario_settings_warning_" // trim(level) // "_" // trim(stream) // ".parquet"

        schema%maml%name = "settings_warn.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: warn_demo", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  qc:", &
            "    max: 10" ]
        call parquet_parse_maml(schema)

        call parquet_set_verbosity(level)
        call parquet_set_message_stream(stream)
        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "v", v)   ! -> qc violation warning
        call parquet_close_writer(writer)
        call parquet_reset_settings()
    end subroutine scenario_settings_warning

    !> Calls a SOLICITED printer (%print_stat) at a chosen verbosity, so the test can assert that
    !> "silent" turns an explicitly-requested print into a no-op.
    !>
    !> Its fixture path is derived from `level` for the reason given on
    !> `scenario_settings_cpp_warning`: two scenario names share this helper and run concurrently.
    subroutine scenario_settings_print_stat(level)
        character(len=*), intent(in) :: level !! verbosity to set first.
        type(parquet_table) :: t
        character(len=:), allocatable :: out_file
        integer(int32) :: v(4) = [1, 2, 3, 4]

        out_file = "test_run/scenario_settings_print_stat_" // trim(level) // ".parquet"
        call parquet_new_table(t)
        call t%add_column("v", v)
        call parquet_write_table(t, out_file)

        call parquet_set_verbosity(level)
        call t%print_stat()
        call parquet_reset_settings()
    end subroutine scenario_settings_print_stat

    !> The READER's own solicited printer at a chosen verbosity: parquet_close_reader(
    !> print_stat=.true.), which is parquet_reader_print_stat in parquet_wrapper.cpp.
    !>
    !> Deliberately NOT a duplicate of scenario_settings_print_stat above. That one exercises
    !> %print_stat, i.e. table_print_stat -- a separate FORTRAN implementation in
    !> parquet_tables_query.f90 -- so every assertion it makes passes against a C++ half that
    !> ignores the mirrored verbosity completely. This is the C++ printer's own gate, and before
    !> this scenario existed, deleting `if (output_is_suppressed()) return;` from
    !> parquet_reader_print_stat broke nothing in the whole suite. Same class of gap as the one
    !> scenario_settings_cpp_warning closes for the warning channel (feature_risks.md Risk-42).
    !>
    !> The column is read before the verbosity is set, so the report has a populated row to print
    !> and the "silent" arm is suppressing real output rather than an empty table.
    !>
    !> **The fixture path must stay derived from `level`**: two scenario names share this helper
    !> and tools/run_error_scenarios.sh runs them concurrently (xargs -P), so a shared path means
    !> one process writes while the other reads, Arrow throws `IOError: Couldn't deserialize
    !> thrift` across the extern "C" boundary uncaught, and the run dies with exit 134 instead of
    !> the clean exit 0 this scenario expects. See scenario_settings_cpp_warning's own note.
    subroutine scenario_reader_print_stat_verbosity(level)
        character(len=*), intent(in) :: level !! verbosity to set before closing the reader.
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=:), allocatable :: out_file
        integer(int32) :: v(4)
        integer(int32) :: back(4)

        v = [1, 2, 3, 4]
        out_file = "test_run/scenario_reader_print_stat_" // trim(level) // ".parquet"
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        ! **The verbosity is set BEFORE the reader is opened, and that ordering is the contract.**
        ! The mirrored settings reach C++ when a reader or writer is opened
        ! (parquet_push_settings_to_cpp), not when a setter is called -- which is what lets each
        ! knob's setter live in parquet_settings_base beside its state, where an Arrow-free module
        ! can re-export it. `parquet_reader_print_stat` prints from the C++ side, so a verbosity set
        ! after this reader was opened would not reach the report this scenario is about.
        ! doc/pages/operating/settings.md states the same rule for user code.
        call parquet_set_verbosity(level)
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", back)
        call parquet_close_reader(reader, print_stat=.true.)
        call parquet_reset_settings()
    end subroutine scenario_reader_print_stat_verbosity

    !> Provokes a C++-side warning (qc soft mode on read) at a chosen verbosity.
    !>
    !> This is the scenario that catches the Fortran and C++ copies of the verbosity setting
    !> drifting apart: every Fortran-side assertion passes against a C++ half that ignores the
    !> mirror entirely, because the Fortran warnings would still be suppressed correctly.
    !>
    !> **The fixture path must stay derived from `level`.** `tools/run_error_scenarios.sh` runs
    !> scenarios concurrently (`xargs -P`), so the two names backed by this helper are two
    !> PROCESSES over one file: with a shared path, one writes while the other reads, the reader
    !> gets a half-written file, and Arrow throws `IOError: Couldn't deserialize thrift` across the
    !> `extern "C"` boundary -- an uncaught exception, so `std::terminate` and exit 134 rather than
    !> a clean abort. It is timing-dependent, so it passed locally for a long time and failed only
    !> on CI. Same rule as CLAUDE.md's "Tests run concurrently: never share a fixture file path
    !> between two tests", which applies to this runner too and not only to test-drive.
    subroutine scenario_settings_cpp_warning(level, after_open)
        character(len=*), intent(in) :: level !! verbosity to set.
        logical, intent(in), optional :: after_open !! .true. silences AFTER the reader is opened.
        logical :: late
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        character(len=:), allocatable :: out_file
        integer(int32) :: v(4) = [1, 2, 3, 400]
        integer(int32) :: got(4)

        late = .false.
        if (present(after_open)) late = after_open
        ! The fixture path carries BOTH arguments, because three scenario names share this helper
        ! and tools/run_error_scenarios.sh runs them concurrently -- two processes writing one path
        ! is the collision CLAUDE.md's "never share a fixture file path" note describes.
        out_file = "test_run/scenario_settings_cpp_warning_" // trim(level) // &
            merge("_late", "_open", late) // ".parquet"

        schema%maml%name = "settings_cpp_warn.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: cpp_warn_demo", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  qc:", &
            "    max: 10" ]
        call parquet_parse_maml(schema)

        ! Written with qc off, so the file exists and only the READ complains -- which is the C++
        ! side's own warning path rather than the writer's Fortran one.
        call parquet_open_writer(writer, out_file, schema, qc=.false.)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        ! **`late` is the NEGATIVE CONTROL for push-at-point-of-use, and it asserts a deliberate
        ! behaviour change rather than tolerating one.** The mirrored settings reach C++ when a
        ! reader or writer is opened, never when a setter is called -- that is what let each knob's
        ! setter follow its state into parquet_settings_base, where an Arrow-free module can
        ! re-export it. So silencing AFTER this reader was opened must leave the C++ warning
        ! PRINTING: the mirror this reader is running against was taken at open time.
        !
        ! Without this control, restoring push-at-set later would look like a bug fix and would
        ! silently re-couple every sorting setter to parquet_bindings.
        if (.not. late) call parquet_set_verbosity(level)
        call parquet_open_reader(reader, out_file, schema=schema, qc=.true., qc_soft=.true.)
        if (late) call parquet_set_verbosity(level)
        call parquet_read_column(reader, "v", got)   ! -> qc soft warning, printed from C++
        call parquet_close_reader(reader)
        call parquet_reset_settings()
    end subroutine scenario_settings_cpp_warning

    !> An unknown verbosity token.
    subroutine scenario_settings_bad_verbosity()

        call parquet_set_verbosity("quiet")   ! -> aborts (unknown level)
        print '(a)', "unexpectedly accepted an unknown verbosity level"
    end subroutine scenario_settings_bad_verbosity

    !> An unknown message-stream token. A Fortran unit number is deliberately not accepted.
    subroutine scenario_settings_bad_stream()

        call parquet_set_message_stream("logfile")   ! -> aborts (unknown stream)
        print '(a)', "unexpectedly accepted an unknown message stream"
    end subroutine scenario_settings_bad_stream

    !> The one guarantee neither output setting may break: with everything silenced and messages
    !> redirected, an abort still reports itself on stderr.
    subroutine scenario_settings_error_survives_silence()
        type(parquet_reader) :: reader

        call parquet_set_verbosity("errors_only")
        call parquet_set_message_stream("stderr")
        call parquet_open_reader(reader, "test_run/definitely_not_a_file.parquet")
        print '(a)', "unexpectedly opened a nonexistent file"
    end subroutine scenario_settings_error_survives_silence

    !> An empty key names no column.
    subroutine scenario_sort_empty_key()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_empty_key.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("   ")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (empty sort key)
        print '(a)', "unexpectedly accepted an empty sort key"
    end subroutine scenario_sort_empty_key

    !> A key that is nothing but the '-' shorthand names no column to order by.
    subroutine scenario_sort_minus_only()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_minus_only.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("-")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (names no column)
        print '(a)', "unexpectedly accepted a sort key that is only the '-' shorthand"
    end subroutine scenario_sort_minus_only

    !> A key whose COLUMN NAME exceeds the packed width the bind(C) boundary carries. Distinct
    !> from scenario_sort_key_too_long, which trips parquet_sortkey%add's own whole-key cap
    !> before the parser ever sees it -- this one is a legal-length key with an over-long name.
    subroutine scenario_sort_name_too_long()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_name_too_long.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add(repeat("x", 100) // " asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (column name is too long)
        print '(a)', "unexpectedly accepted a sort key with an over-long column name"
    end subroutine scenario_sort_name_too_long

    !> A direction word that is neither asc nor desc is a typo worth reporting, not something to
    !> guess at.
    subroutine scenario_sort_bad_direction()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_bad_direction.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v sideways")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (unrecognized direction)
        print '(a)', "unexpectedly accepted an unrecognized sort direction"
    end subroutine scenario_sort_bad_direction

    !> More than one direction word is ambiguous rather than merely redundant.
    subroutine scenario_sort_two_direction_words()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_two_direction_words.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc desc")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (more than one direction word)
        print '(a)', "unexpectedly accepted two direction words in one sort key"
    end subroutine scenario_sort_two_direction_words

    !> The '-' shorthand and an explicit direction could equally be read as agreeing or as
    !> cancelling out, so combining them is rejected rather than silently resolved.
    subroutine scenario_sort_minus_and_direction()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_minus_and_direction.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("-v desc")
        call parquet_open_reader(reader, out_file, sort_by=srt)   ! -> aborts (shorthand plus explicit direction)
        print '(a)', "unexpectedly accepted '-' together with an explicit direction"
    end subroutine scenario_sort_minus_and_direction

    !> parquet_sortkey%add's own caps: a key longer than sortkey_max_key_len.
    subroutine scenario_sort_key_too_long()
        type(parquet_sortkey) :: srt
        character(len=400) :: long_key

        long_key = repeat("x", 400)
        call srt%add(long_key)   ! -> aborts (key exceeds the maximum supported length)
        print '(a)', "unexpectedly accepted an over-long sort key"
    end subroutine scenario_sort_key_too_long

    !> parquet_sortkey%add's other cap: more keys than sortkey_max_keys.
    subroutine scenario_sort_too_many_keys()
        type(parquet_sortkey) :: srt
        character(len=16) :: key
        integer :: i

        do i = 1, 40
            write(key, '(a,i0)') "c", i
            call srt%add(trim(key))   ! -> aborts once past the cap
        end do
        print '(a)', "unexpectedly accepted more sort keys than the cap allows"
    end subroutine scenario_sort_too_many_keys

    !> A chunked read is meaningless under a sort: a sorted row can come from any row group, so
    !> there is no coherent "row group N of the sorted output". This is the guard that, if
    !> forgotten, would silently hand back physically ordered rows instead of aborting.
    subroutine scenario_sort_chunked_read()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: back(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_chunked_read.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_read_column_chunk(reader, "v", 1, back)   ! -> aborts (active sort)
        print '(a)', "unexpectedly read a chunk on a sorted reader"
    end subroutine scenario_sort_chunked_read

    !> The same ban, reached through the STRING chunk specific. The guard is one identical line at
    !> every chunked-read entry point, and the three type-family submodules each carry their own
    !> copies, so one scenario per family is what keeps a dropped line from going unnoticed --
    !> verified by deleting a single site and watching exactly one of these stop aborting.
    subroutine scenario_sort_chunked_read_string()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=4) :: back(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_chunked_read_string.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_read_column_chunk(reader, "txt", 1, back)   ! -> aborts (active sort)
        print '(a)', "unexpectedly read a string chunk on a sorted reader"
    end subroutine scenario_sort_chunked_read_string

    !> The same ban, reached through the VECTOR (matrix) chunk specific -- a different shape again,
    !> in the same submodule as the scalar one.
    subroutine scenario_sort_chunked_read_vector()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: back(2, 2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_chunked_read_vector.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_read_column_chunk(reader, "vec", 1, back)   ! -> aborts (active sort)
        print '(a)', "unexpectedly read a vector chunk on a sorted reader"
    end subroutine scenario_sort_chunked_read_vector

    !> The same ban, reached through the TEMPORAL chunk specific (parquet_read_temporal.f90's own
    !> copies of the guard).
    subroutine scenario_sort_chunked_read_temporal()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        type(parquet_date) :: d(4), back(2)
        integer(int32) :: v(4) = [30, 10, 40, 20]
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_chunked_read_temporal.parquet"

        do i = 1, 4
            d(i) = parquet_date(2024, 1, i)
        end do
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "d", d)
        call parquet_close_writer(writer)

        call srt%add("v asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_read_column_chunk(reader, "d", 1, back)   ! -> aborts (active sort)
        print '(a)', "unexpectedly read a date chunk on a sorted reader"
    end subroutine scenario_sort_chunked_read_temporal

    !> Same reasoning for the row-group row count: it describes a row grouping the sorted result
    !> no longer has.
    subroutine scenario_sort_get_chunk_size()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int64) :: chunk_rows
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_get_chunk_size.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_get_chunk_size(reader, chunk_rows, row_group=1_int64)   ! -> aborts (active sort)
        print '(a)', "unexpectedly asked for a chunk size on a sorted reader"
    end subroutine scenario_sort_get_chunk_size

    !> A second parquet_reader_set_sort on one reader: add every key to one parquet_sortkey
    !> instead, since a second permutation would have to compose with the first.
    subroutine scenario_sort_set_sort_twice()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: first, second
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_set_sort_twice.parquet"

        call write_sort_scenario_fixture(out_file)
        call first%add("v asc")
        call second%add("txt asc")
        call parquet_open_reader(reader, out_file)
        call parquet_reader_set_sort(reader, first)
        call parquet_reader_set_sort(reader, second)   ! -> aborts (already has an active sort)
        print '(a)', "unexpectedly applied a second sort to one reader"
    end subroutine scenario_sort_set_sort_twice

    !> parquet_reader_set_sort after a column has been read: the rows already handed back could
    !> not be aligned with anything read afterwards.
    subroutine scenario_sort_set_sort_after_read()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: back(4)
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_set_sort_after_read.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", back)
        call parquet_reader_set_sort(reader, srt)   ! -> aborts (a column has already been read)
        print '(a)', "unexpectedly applied a sort after reading a column"
    end subroutine scenario_sort_set_sort_after_read

    !> A CHUNKED read before parquet_reader_set_sort. Distinct from scenario_sort_set_sort_after_read
    !> above and NOT covered by it: a chunked read frees each row group's array and caches nothing,
    !> so the decoded-columns predicate stays 0 and only parquet_reader_has_chunk_reads sees it.
    !> Deleting that second guard makes this scenario pass silently while the reader hands back rows
    !> it already returned in physical order.
    subroutine scenario_sort_set_sort_after_chunked_read()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: chunk(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_after_chunked.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call parquet_open_reader(reader, out_file)
        ! Negative control, in-scenario: a sort is legal until something has actually been read, so
        ! this same call on an untouched reader must succeed. Without it the scenario would pass
        ! just as happily against a guard that refuses unconditionally.
        block
            type(parquet_reader) :: control
            type(parquet_sortkey) :: control_srt

            call control_srt%add("v asc")
            call parquet_open_reader(control, out_file)
            call parquet_reader_set_sort(control, control_srt)
            call parquet_close_reader(control)
        end block
        call parquet_read_column_chunk(reader, "v", 1, chunk)
        call parquet_reader_set_sort(reader, srt)   ! -> aborts (a chunked read has already been done)
        print '(a)', "unexpectedly applied a sort after a chunked read"
    end subroutine scenario_sort_set_sort_after_chunked_read

    !> The filter twin of the scenario above -- same blind spot, same guard, same failure mode.
    subroutine scenario_filter_set_filter_after_chunked_read()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: chunk(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_after_chunked.parquet"

        call write_sort_scenario_fixture(out_file)
        call filt%add("v >= 20")
        call parquet_open_reader(reader, out_file)
        block
            type(parquet_reader) :: control
            type(parquet_filter) :: control_filt

            call control_filt%add("v >= 20")
            call parquet_open_reader(control, out_file)
            call parquet_reader_set_filter(control, control_filt)
            call parquet_close_reader(control)
        end block
        call parquet_read_column_chunk(reader, "v", 1, chunk)
        call parquet_reader_set_filter(reader, filt)   ! -> aborts (a chunked read has already been done)
        print '(a)', "unexpectedly applied a filter after a chunked read"
    end subroutine scenario_filter_set_filter_after_chunked_read

    !> A filter applied to an already-SORTED reader. apply_row_transform masks first and permutes
    !> second, so a permutation's length is the post-filter row count -- filter first, then sort.
    !> The decoded-columns guard would refuse this anyway (applying a sort decodes its key columns),
    !> which is exactly why the has-sort check sits AHEAD of it: this scenario asserts the message
    !> that names the caller's real mistake, so moving the check back below would fail it while a
    !> plain "did it abort?" test would not notice.
    subroutine scenario_filter_set_filter_after_sort()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_after_sort.parquet"

        call write_sort_scenario_fixture(out_file)
        call srt%add("v asc")
        call filt%add("v >= 20")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_reader_set_filter(reader, filt)   ! -> aborts (this reader already has an active sort)
        print '(a)', "unexpectedly applied a filter to a sorted reader"
    end subroutine scenario_filter_set_filter_after_sort

    !> Reading a RAGGED plain-LIST column aborts, at reader level.
    !>
    !> A plain `list<int32>` carries no width in the schema, so whether it is a usable vector column
    !> is a property of the data (see test/fixtures/list_widths.parquet's own generator header).
    !> `uniform` -- every row length 3 -- IS an ordinary width-3 vector column; `ragged` -- lengths
    !> 1,2,3,4 -- has no single width and is rejected when read. The whole suite measured this
    !> distinction (parquet_measure_list_width, %width) and never once READ a ragged column, which
    !> is how a test that did read one reached main and aborted the whole table suite.
    !>
    !> The `uniform` read below is the NEGATIVE CONTROL and is the point of the scenario: without
    !> it this passes just as happily against a library that rejects every plain LIST column, which
    !> is the opposite defect and the more likely one.
    subroutine scenario_read_ragged_list_column()
        type(parquet_reader) :: reader
        integer(int32), allocatable :: wide(:,:)
        integer(int64) :: nrows
        integer :: cs
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"

        call parquet_open_reader(reader, f)
        call parquet_get_nrows(reader, nrows)
        ! NEGATIVE CONTROL: the same call sequence on a plain LIST whose rows really are uniform
        ! must succeed. Without it this passes against a library that rejects every plain LIST.
        call parquet_measure_list_width(reader, "uniform", 0, 0, .false., cs)
        if (cs /= 3) error stop "control failed: uniform's unproven width should be 3"
        allocate(wide(cs, nrows))
        call parquet_read_column(reader, "uniform", wide)
        if (size(wide, 1) /= 3) error stop "control failed: uniform did not read back at width 3"
        deallocate(wide)
        ! avg_ok's rows alternate 3,1 so the footer mean is exactly 2 and the SCREEN cannot reject
        ! it: the unproven measurement hands back a candidate of 2, which is wrong. Reading at that
        ! candidate is what catches it -- get_uniform_list_values checks every row's length against
        ! the width it was given. This is the reader-level half of the read-is-the-proof design.
        call parquet_measure_list_width(reader, "avg_ok", 0, 0, .false., cs)
        if (cs /= 2) error stop "expected avg_ok's unproven screen candidate to be 2"
        allocate(wide(cs, nrows))
        call parquet_read_column(reader, "avg_ok", wide)   ! -> aborts (shape mismatch)
        print '(a)', "unexpectedly read a list column at a width its rows do not have"
    end subroutine scenario_read_ragged_list_column

    !> The table-level twin of the scenario above -- the path that actually broke.
    subroutine scenario_table_read_ragged_list_column()
        type(parquet_table) :: t
        integer(int32), allocatable :: wide(:,:), flat(:)
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"

        call parquet_open_table(t, f)
        ! NEGATIVE CONTROL, as above: the uniform column is a real vector column and reads.
        if (.not. t%is_supported("uniform")) error stop "control failed: uniform should be supported"
        if (t%width("uniform") /= 3) error stop "control failed: uniform should have width 3"
        call t%get("uniform", wide)
        if (size(wide, 1) /= 3) error stop "control failed: uniform did not read back at width 3"
        ! %is_supported answers .true. for the ragged column too, and that is correct rather than a
        ! wart -- it reports on the TYPE, which is readable, and cannot know about raggedness
        ! without reading. The read is where it is caught.
        if (.not. t%is_supported("ragged")) error stop "ragged's element type should still report supported"
        call t%get("ragged", flat)   ! -> aborts
        print '(a)', "unexpectedly read a ragged list column through a table"
    end subroutine scenario_table_read_ragged_list_column

    !> `avg_ok` is a DIFFERENT mechanism from `ragged`, not a second example of it, and it is the
    !> one worth having.
    !>
    !> Its rows alternate 3,1,3,1, so the footer screen's mean is exactly 2 and the screen CANNOT
    !> reject it -- it yields a candidate of 2. A table read then resolves the width with that
    !> UNPROVEN candidate on purpose (table_touch passes proven=.false.), because
    !> get_uniform_list_values checks every row's length against the width it was handed, so the
    !> read that was going to happen anyway doubles as the proof and the scan is never paid for.
    !> This scenario is what pins that: it is the only place the read-is-the-proof design is
    !> exercised end to end. Trusting the candidate without that check would mis-shape the column
    !> instead of aborting.
    subroutine scenario_table_read_avg_ok_list_column()
        type(parquet_table) :: t
        integer(int32), allocatable :: flat(:)
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"

        call parquet_open_table(t, f)
        call t%get("avg_ok", flat)   ! -> aborts (shape mismatch)
        print '(a)', "unexpectedly read a list column whose screen candidate was not its real width"
    end subroutine scenario_table_read_avg_ok_list_column

    !> parquet_reader_adopt_transform onto a reader that already has a transform of its own: the two
    !> would have to compose, and the adopted mask indexes rows the reader's own mask has already
    !> removed.
    subroutine scenario_adopt_transform_onto_transformed()
        type(parquet_reader) :: src, dst
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_adopt_onto_transformed.parquet"

        call write_sort_scenario_fixture(out_file)
        call filt%add("v >= 2")
        call parquet_open_reader(src, out_file, filter=filt)
        call parquet_open_reader(dst, out_file, filter=filt)
        call parquet_reader_adopt_transform(dst, src)   ! -> aborts (already has one of its own)
        print '(a)', "unexpectedly adopted a transform onto an already-transformed reader"
    end subroutine scenario_adopt_transform_onto_transformed

    !> parquet_reader_adopt_transform after a column has been read: that column was read UNMASKED
    !> and could never be lined up with the rows the adopted mask keeps.
    subroutine scenario_adopt_transform_after_read()
        type(parquet_reader) :: src, dst
        type(parquet_filter) :: filt
        integer(int32) :: back(4)
        character(len=*), parameter :: out_file = "test_run/error_scenario_adopt_after_read.parquet"

        call write_sort_scenario_fixture(out_file)
        call filt%add("v >= 2")
        call parquet_open_reader(src, out_file, filter=filt)
        call parquet_open_reader(dst, out_file)
        call parquet_read_column(dst, "v", back)
        call parquet_reader_adopt_transform(dst, src)   ! -> aborts (a column has already been read)
        print '(a)', "unexpectedly adopted a transform after reading a column"
    end subroutine scenario_adopt_transform_after_read

    !> parquet_reader_adopt_transform between readers on DIFFERENT files: the mask describes one
    !> file's rows and would silently mis-select the other's.
    subroutine scenario_adopt_transform_other_file()
        type(parquet_reader) :: src, dst
        type(parquet_filter) :: filt
        type(parquet_writer) :: w
        integer(int32) :: v(9)
        integer :: i
        character(len=*), parameter :: src_file = "test_run/error_scenario_adopt_other_src.parquet"
        character(len=*), parameter :: dst_file = "test_run/error_scenario_adopt_other_dst.parquet"

        call write_sort_scenario_fixture(src_file)
        do i = 1, 9
            v(i) = int(i, int32)
        end do
        call parquet_open_writer(w, dst_file)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call filt%add("v >= 2")
        call parquet_open_reader(src, src_file, filter=filt)
        call parquet_open_reader(dst, dst_file)
        call parquet_reader_adopt_transform(dst, src)   ! -> aborts (different files)
        print '(a)', "unexpectedly adopted a transform across two different files"
    end subroutine scenario_adopt_transform_other_file

    !> The point of a row-group-SCOPED filter (`parquet_reader_set_filter(reader, filt, lo, hi)`),
    !> asserted rather than assumed: building the mask must not read any column whole-file. That is
    !> the whole reason the scoped form exists -- it is what lets a file larger than memory be
    !> filtered at all -- so it is worth a direct proof rather than an indirect one.
    !>
    !> It lives out of process, with the same forced-whole-column-read hook the two scenarios above
    !> use, because the obvious in-process alternative does not work: comparing
    !> parquet_get_arrow_bytes_allocated before and after reads a PROCESS-GLOBAL counter, and
    !> test-drive runs a suite's tests concurrently, so every other test's Arrow allocations land in
    !> the same number. An earlier version of this check lived in test/test_filter.f90 and failed
    !> roughly one run in three for exactly that reason. Here nothing else is running.
    !>
    !> Negative control: scenario_whole_column_read_forced_error_control.
    subroutine scenario_filter_scoped_reads_no_whole_column()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_filter_scoped_reads_no_whole_column.parquet"
        integer(int32) :: v(40), back(8)
        integer(int64) :: nrows
        integer :: i

        do i = 1, 40
            v(i) = i
        end do
        call parquet_open_writer(writer, out_file, chunk_size=10)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call filt%add("v > 12")
        call parquet_open_reader(reader, out_file)
        ! Armed before the filter is built: the scoped path evaluates row group by row group and
        ! must never reach get_single_chunk_array. (The unscoped path deliberately does -- it warms
        ! every filter column whole-file first -- which is why this scenario scopes.)
        call parquet_debug_set_force_whole_column_read_error(1)
        call parquet_reader_set_filter(reader, filt, 2, 3)
        call parquet_debug_set_force_whole_column_read_error(0)

        ! Row groups 2 and 3 hold physical rows 11..30; "v > 12" keeps 13..30, and rows outside the
        ! scoped range are dropped -- so 18 rows survive, and reading them back is a whole-column
        ! read, which is fine now that the hook is disarmed again.
        call parquet_get_nrows(reader, nrows)
        if (nrows /= 18_int64) error stop "scoped filter should leave 18 surviving rows"
        call parquet_read_column_chunk(reader, "v", 2, back)
        if (any(back /= [13, 14, 15, 16, 17, 18, 19, 20])) error stop "scoped filter: row group 2 should yield 13..20"
        call parquet_close_reader(reader)
        print '(a)', "a row-group-scoped filter avoided a whole-column read, as expected"
    end subroutine scenario_filter_scoped_reads_no_whole_column

    !> Resolving a plain LIST column's deferred width must never take the whole-column read path,
    !> however many columns are asked about.
    !>
    !> This is the guarantee that makes parquet_open_table cheap on a file of such columns. The
    !> width is found from the file footer where possible and otherwise by reading ONE ROW GROUP AT
    !> A TIME, so `g_debug_force_whole_column_read_error` -- which aborts the instant
    !> get_single_chunk_array would decode a whole column -- must not fire for any of them. The
    !> scenario finishing at all is the assertion; it runs with the hook armed from before the table
    !> is even opened, so an open-time classification read would trip it too.
    !>
    !> Its own negative control is scenario_whole_column_read_forced_error_control, which proves the
    !> hook does fire on a path that genuinely reads a whole column.
    subroutine scenario_list_width_never_reads_whole_column()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        character(len=:), allocatable :: names(:)
        integer :: i

        call parquet_debug_set_force_whole_column_read_error(1)
        block
            type(parquet_table) :: t
            call parquet_open_table(t, f)
            call t%column_names(names)
            do i = 1, size(names)
                if (.not. t%is_supported(trim(names(i)))) cycle
                ! Both queries resolve a deferred width for real (proven), so between them they
                ! exercise the footer screen and the row-group scan across every shape in the
                ! fixture -- uniform, ragged, integral-mean-but-ragged, late-violation, null and
                ! empty rows.
                if (t%kind(trim(names(i))) == 0) error stop "unexpected PK_NONE for a supported column"
                if (t%width(trim(names(i))) < 1) error stop "unexpected non-positive width"
            end do
        end block
        ! And a slice, whose measurement is scoped to its own row groups.
        block
            type(parquet_table) :: t
            call parquet_open_table(t, f, 1_int64, 12_int64)
            if (t%width("late") /= 3) error stop "slice-local width mismatch for the late column"
        end block
        call parquet_debug_set_force_whole_column_read_error(0)
        print '(a)', "resolving every deferred LIST width avoided the whole-column read path, as expected"
    end subroutine scenario_list_width_never_reads_whole_column
    !
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
    !> filter, with an elem_index (col_index) past col_size aborts with "col_index out of bounds"
    !> before any type check runs. The filtered reader is the point: it once took a separate
    !> whole-column branch with its own copy of this bounds check, and now shares the
    !> row-group-streaming path's -- so this keeps the filtered case covered either way.
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

    !> Same filtered reader as above, but with a valid col_index -- reaches the type-mismatch
    !> check instead (the int32 column read via the logical specific), which now runs per row
    !> group rather than once over a whole-column array.
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

    !> Same filtered reader as above, but with a valid col_index -- reaches the type-mismatch
    !> check instead (the int32 column read via the string specific), which now runs per row group
    !> rather than once over a whole-column array.
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
    !> the filtered case included -- this is the filtered half.
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

        call filt%add(repeat("a", 8193))
        print '(a)', "unexpectedly accepted a filter rule longer than the supported maximum"
    end subroutine scenario_filter_rule_too_long

    !> from and to are parallel arrays, so a size mismatch cannot be resolved -- there is no
    !> sensible reading of "rename these three names to these two".
    subroutine scenario_filter_remap_size_mismatch()
        type(parquet_filter) :: filt

        call filt%add("v > 5")
        call filt%remap_column_names(["v", "w"], ["a"])
        print '(a)', "unexpectedly accepted a filter remap with mismatched from/to sizes"
    end subroutine scenario_filter_remap_size_mismatch

    !> A replacement longer than filter_leaf_name_len would be silently TRUNCATED into the packed
    !> leaf-name array, i.e. the filter would quietly apply to a different column than asked for.
    subroutine scenario_filter_remap_name_too_long()
        type(parquet_filter) :: filt

        call filt%add("v > 5")
        call filt%remap_column_names(["v"], [repeat("x", 65)])
        print '(a)', "unexpectedly accepted a filter remap to an over-long column name"
    end subroutine scenario_filter_remap_name_too_long

    !> A rule that fits filter_max_rule_len in its original names but not after renaming. The
    !> message must name the remapping: the caller's own rule was within the limit, so %add's
    !> generic length error would point at a limit they never exceeded.
    subroutine scenario_filter_remap_rule_too_long()
        type(parquet_filter) :: filt
        character(len=:), allocatable :: rule
        integer :: i

        ! 200 clauses: about 2 kB in the one-character name, about 15 kB once every name is 64
        ! characters long -- comfortably either side of the 8192-character cap.
        rule = "a > 1"
        do i = 2, 200
            rule = rule // " and a > 1"
        end do
        call filt%add(rule)
        call filt%remap_column_names(["a"], [repeat("x", 64)])
        print '(a)', "unexpectedly accepted a filter rule that grew past the cap when remapped"
    end subroutine scenario_filter_remap_rule_too_long

    !> The sort twin of scenario_filter_remap_size_mismatch.
    subroutine scenario_sortkey_remap_size_mismatch()
        type(parquet_sortkey) :: srt

        call srt%add("v asc")
        call srt%remap_column_names(["v", "w"], ["a"])
        print '(a)', "unexpectedly accepted a sortkey remap with mismatched from/to sizes"
    end subroutine scenario_sortkey_remap_size_mismatch

    !> The sort twin of scenario_filter_remap_name_too_long, against sort_key_name_len.
    subroutine scenario_sortkey_remap_name_too_long()
        type(parquet_sortkey) :: srt

        call srt%add("v asc")
        call srt%remap_column_names(["v"], [repeat("x", 65)])
        print '(a)', "unexpectedly accepted a sortkey remap to an over-long column name"
    end subroutine scenario_sortkey_remap_name_too_long

    !> The parquet_read_qc counterpart of scenario_filter_rule_too_long: a sanity bound against
    !> accidentally-huge input, not a design limit on how much qc one column can declare.
    subroutine scenario_read_qc_entry_too_long()
        type(parquet_read_qc) :: qc

        call qc%add(repeat("a", 1025))
        print '(a)', "unexpectedly accepted a read_qc entry longer than the supported maximum"
    end subroutine scenario_read_qc_entry_too_long

    !> from and to are parallel arrays; a size mismatch has no sensible reading.
    subroutine scenario_read_qc_remap_size_mismatch()
        type(parquet_read_qc) :: qc

        call qc%add("mass, >0")
        call qc%remap_column_names(["mass", "flag"], ["m"])
        print '(a)', "unexpectedly accepted a read_qc remap with mismatched from/to sizes"
    end subroutine scenario_read_qc_remap_size_mismatch

    !> An entry that fits read_qc_max_entry_len in its original name but not once renamed. The
    !> message must name the remapping: the caller's own entry was within the limit.
    subroutine scenario_read_qc_remap_entry_too_long()
        type(parquet_read_qc) :: qc

        call qc%add("a," // repeat("0", 1020))
        call qc%remap_column_names(["a"], [repeat("y", 10)])
        print '(a)', "unexpectedly accepted a read_qc entry that grew past the cap when remapped"
    end subroutine scenario_read_qc_remap_entry_too_long

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

    !> The row-group-SCOPED sibling of scenario_filter_bad_numeric_value above: the same bad value
    !! caught by the SCOPED evaluation path (parquet_reader_set_filter's row_group_lo/row_group_hi
    !! form) inside its own per-row-group loop, rather than the unscoped, whole-file path every
    !! other filter-error scenario exercises. See scenario_filter_scoped_reads_no_whole_column above
    !! for the scoped path's own successful case.
    subroutine scenario_filter_bad_numeric_value_scoped()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call filt%add("id_with_null > abc")
        call parquet_reader_set_filter(reader, filt, 1, 1)
        print '(a)', "unexpectedly applied a scoped filter with a non-numeric value against a numeric column"
    end subroutine scenario_filter_bad_numeric_value_scoped

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

    !> is_nan/is_not_nan are restricted to the three column types that can actually hold a NaN
    !> (float32/float64/half_float). An integer column can never hold one, so the operator would
    !> answer a constant for every row -- rejected instead, since it is far more likely to be a
    !> mistyped column name than a deliberate request for "all rows"/"no rows".
    subroutine scenario_filter_is_nan_non_float_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_is_nan_non_float.parquet")
        call parquet_write_column(writer, "id", [1_int32, 2_int32])
        call parquet_close_writer(writer)

        call filt%add("id is_nan")
        call parquet_open_reader(reader, "test_run/filter_is_nan_non_float.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with is_nan against an integer filter column"
    end subroutine scenario_filter_is_nan_non_float_column

    !> strtod parses "nan" as happily as it parses "inf", but a comparison against a NaN is never
    !> meaningful -- every IEEE comparison against it is false and every /= is true, so the clause
    !> can only ever match nothing or everything. Rejected, pointing the caller at is_nan; an
    !> infinity is a real bound and stays accepted (covered in test_filter.f90, not here).
    subroutine scenario_filter_nan_literal_rejected()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_nan_literal.parquet")
        call parquet_write_column(writer, "v", [1.5_real64, 2.5_real64])
        call parquet_close_writer(writer)

        call filt%add("v == nan")
        call parquet_open_reader(reader, "test_run/filter_nan_literal.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a NaN literal as a filter comparison value"
    end subroutine scenario_filter_nan_literal_rejected

    !> is_nan/is_not_nan take no value, so the clause scanner must end the clause at the operator
    !> and report the next bare name as a missing combinator -- exactly as it already does for
    !> is_null. Getting that wrong swallows the name as this clause's value instead, which surfaces
    !> as a different message ("is_nan takes no value"), so the message asserted by this scenario's
    !> wrapper is what actually pins the behaviour down.
    subroutine scenario_filter_is_nan_missing_combinator()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_is_nan_missing_combinator.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "x", [1.5_real64, 2.5_real64])
        call parquet_close_writer(writer)

        call filt%add("x is_nan x > 1")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an is_nan clause and no combinator after it"
    end subroutine scenario_filter_is_nan_missing_combinator

    !> eval_filter_clause's `default:` branch (parquet_wrapper.cpp) -- a column type filtering
    !> doesn't support at all. Every physical type this project has an ordinary fixture for is
    !> filterable (the nine canonical types plus the extended int/uint/half-float/decimal read
    !> types), so this scenario writes a BINARY (byte-array) column through the debug hook
    !> parquet_debug_write_binary_fixture and filters on that. Aborts via a clean Fortran
    !> `error stop` (parquet_apply_filter, parquet_read.f90), not report_fatal_error -- exit
    !> code 1, not SIGABRT.
    subroutine scenario_filter_unsupported_column_type()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_unsupported_column_type.parquet"
        interface
            subroutine parquet_debug_write_binary_fixture(path, column_name) &
                bind(C, name="parquet_debug_write_binary_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char), intent(in) :: path(*) !! null-terminated output file path.
                character(kind=c_char), intent(in) :: column_name(*) !! null-terminated BINARY column name.
            end subroutine parquet_debug_write_binary_fixture
        end interface

        call parquet_debug_write_binary_fixture(out_file//char(0), "blob"//char(0))
        call filt%add("blob == 1")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter clause against a binary column"
    end subroutine scenario_filter_unsupported_column_type

    !> sort_bind_arrow_key's own `return false` fallback (parquet_wrapper.cpp) -- the sort-key
    !! counterpart of scenario_filter_unsupported_column_type above, same BINARY column, same
    !! reasoning (every ordinary fixture's physical type is orderable, so a genuinely unsupported
    !! type needs the same debug hook). A BINARY column is not a vector type, so it passes the
    !! earlier FIXED_SIZE_LIST/LIST/LARGE_LIST rejection in parquet_reader_set_sort and reaches
    !! sort_bind_arrow_key itself.
    subroutine scenario_sort_unsupported_column_type()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: out_file = "test_run/error_scenario_sort_unsupported_column_type.parquet"
        interface
            subroutine parquet_debug_write_binary_fixture(path, column_name) &
                bind(C, name="parquet_debug_write_binary_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char), intent(in) :: path(*) !! null-terminated output file path.
                character(kind=c_char), intent(in) :: column_name(*) !! null-terminated BINARY column name.
            end subroutine parquet_debug_write_binary_fixture
        end interface

        call parquet_debug_write_binary_fixture(out_file//char(0), "blob"//char(0))
        call srt%add("blob asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        print '(a)', "unexpectedly sorted by a binary column"
    end subroutine scenario_sort_unsupported_column_type

    !> A temporal column IS filterable, but only against a double-quoted ISO-8601 literal: a bare
    !> number would silently mean "days" for one column and "microseconds since the epoch" for
    !> another, with nothing in the rule to say which. convert_temporal_filter_values
    !> (parquet_read.f90) rejects the unquoted form before the value ever reaches C++.
    subroutine scenario_filter_temporal_value_not_quoted()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: day(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_temporal_value_not_quoted.parquet"

        call day(1)%set(2024, 1, 1)
        call day(2)%set(2024, 1, 2)
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "day", day)
        call parquet_close_writer(writer)

        call filt%add("day == 2024")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unquoted value against a temporal column"
    end subroutine scenario_filter_temporal_value_not_quoted

    !> Writes the small int32 fixture every syntax scenario below filters against, so each one
    !> exercises the parser rather than a missing file. Shared rather than duplicated because the
    !> file's contents are irrelevant -- the abort always happens before any row is examined.
    subroutine write_filter_syntax_fixture(out_file)
        character(len=*), intent(in) :: out_file !! parquet file to (re)create.
        type(parquet_writer) :: writer
        integer(int32) :: v(3) = [1, 2, 3]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine write_filter_syntax_fixture

    !> An expression with more '(' than ')' is rejected by the parser (parquet_read_filter.f90)
    !> before any column is read.
    subroutine scenario_filter_unbalanced_parens()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_unbalanced_parens.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("(v > 1 and v < 3")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unbalanced filter expression"
    end subroutine scenario_filter_unbalanced_parens

    !> A ')' with no matching '(' -- the mirror of the scenario above, and a different parser
    !> branch (parse_primary's TK_RPAREN arm, not the unterminated-group one).
    subroutine scenario_filter_stray_close_paren()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_stray_close_paren.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1) and v < 3")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a stray ')' in the filter expression"
    end subroutine scenario_filter_stray_close_paren

    !> An empty group "()" has no clause to evaluate.
    subroutine scenario_filter_empty_parens()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_empty_parens.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1 and ()")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an empty group in the filter expression"
    end subroutine scenario_filter_empty_parens

    !> "and" with nothing after it: the parser runs out of tokens where a clause must start.
    subroutine scenario_filter_dangling_and()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_dangling_and.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1 and")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a dangling 'and' in the filter expression"
    end subroutine scenario_filter_dangling_and

    !> "or" with nothing BEFORE it -- a different parser branch from the dangling-and scenario
    !> above (parse_primary sees the combinator where a clause should begin).
    subroutine scenario_filter_leading_or()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_leading_or.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("or v > 1")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a leading 'or' in the filter expression"
    end subroutine scenario_filter_leading_or

    !> "not" with no operand at all.
    subroutine scenario_filter_not_without_operand()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_not_without_operand.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1 and not")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a 'not' that has no operand"
    end subroutine scenario_filter_not_without_operand

    !> Two clauses with no combinator between them: the first parses, and the leftover tokens are
    !> reported rather than silently ignored (which would apply half the filter the caller wrote).
    subroutine scenario_filter_missing_combinator()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_missing_combinator.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1 v < 3")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with two filter clauses and no combinator"
    end subroutine scenario_filter_missing_combinator

    !> Nesting deeper than filter_max_depth (parquet_core.f90) aborts cleanly instead of overflowing
    !> the recursive-descent parser's own call stack, which would be a signal with no message.
    subroutine scenario_filter_nesting_too_deep()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_nesting_too_deep.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add(repeat("(", 40)//"v > 1"//repeat(")", 40))
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter expression nested past the limit"
    end subroutine scenario_filter_nesting_too_deep

    !> More expression terms than filter_max_nodes (parquet_core.f90) allows. 700 clauses become 1399
    !> nodes (700 leaves + 699 'and's), comfortably past the 1024 cap.
    subroutine scenario_filter_too_many_nodes()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=:), allocatable :: rule
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_too_many_nodes.parquet"

        call write_filter_syntax_fixture(out_file)
        rule = "v > 0"
        do i = 2, 700
            rule = rule//" and v > 0"
        end do
        call filt%add(rule)
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter expression past the node limit"
    end subroutine scenario_filter_too_many_nodes

    !> A quoted value that is not a valid ISO-8601 literal for the column's temporal type.
    subroutine scenario_filter_temporal_bad_iso_literal()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: day(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_temporal_bad_iso.parquet"

        call day(1)%set(2024, 1, 1)
        call day(2)%set(2024, 1, 2)
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "day", day)
        call parquet_close_writer(writer)

        call filt%add('day == "2024-13-99"')
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an invalid ISO-8601 date literal"
    end subroutine scenario_filter_temporal_bad_iso_literal

    !> A literal carrying finer precision than the column's stored unit can represent: the
    !> microsecond digits below cannot be expressed in a timestamp[ms] column, so any answer would
    !> be a lie about a value the file does not hold. Rejected rather than truncated
    !> (convert_temporal_filter_values, parquet_read.f90).
    subroutine scenario_filter_temporal_literal_too_precise()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_schema) :: schema
        type(parquet_timestamp) :: ts(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_temporal_precise.parquet"

        call ts(1)%set(2024, 1, 31, 12, 30, 0)
        call ts(2)%set(2024, 1, 31, 12, 30, 1)
        call schema%init("ts_ms")
        call schema%add_field("t", "timestamp[ms]")
        call parquet_open_writer(writer, out_file, schema=schema)
        call parquet_write_column(writer, "t", ts)
        call parquet_close_writer(writer)

        call filt%add('t > "2024-01-31T12:30:00.123456"')
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a literal finer than the column's stored unit"
    end subroutine scenario_filter_temporal_literal_too_precise

    !> parquet_reader_set_filter refuses a reader that has already decoded a column: the data
    !> already handed back covers the unfiltered rows, so nothing read afterwards could line up
    !> with it.
    subroutine scenario_filter_set_filter_after_read()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: v(3)
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_set_after_read.parquet"

        call write_filter_syntax_fixture(out_file)
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", v)
        call filt%add("v > 1")
        call parquet_reader_set_filter(reader, filt)
        print '(a)', "unexpectedly applied a filter to a reader that had already read a column"
    end subroutine scenario_filter_set_filter_after_read

    !> parquet_reader_set_filter refuses a reader that is already filtered: the second mask would
    !> be indexed by the first one's surviving rows, so the two do not simply AND together.
    subroutine scenario_filter_set_filter_twice()
        type(parquet_reader) :: reader
        type(parquet_filter) :: first, second
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_set_twice.parquet"

        call write_filter_syntax_fixture(out_file)
        call first%add("v > 1")
        call parquet_open_reader(reader, out_file, filter=first)
        call second%add("v < 3")
        call parquet_reader_set_filter(reader, second)
        print '(a)', "unexpectedly applied a second filter to an already-filtered reader"
    end subroutine scenario_filter_set_filter_twice

    !> A quoted value whose closing quote is missing: the lexer consumes to the end of the rule
    !> looking for it and reports the run rather than silently treating the rest as a value.
    subroutine scenario_filter_unterminated_quote()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_unterminated_quote.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add('v == "abc')
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unterminated quoted filter value"
    end subroutine scenario_filter_unterminated_quote

    !> An empty (or all-whitespace) rule has no clause to evaluate at all.
    subroutine scenario_filter_empty_rule()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_empty_rule.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("   ")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an empty filter rule"
    end subroutine scenario_filter_empty_rule

    !> A group that parses a clause and then meets something other than ')' -- distinct from the
    !> unbalanced-'(' scenario, where the group simply runs out of tokens.
    subroutine scenario_filter_expected_close_paren()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_expected_close.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("(v > 1 v < 3)")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a group missing its ')'"
    end subroutine scenario_filter_expected_close_paren

    !> A ')' where a clause must begin. Unlike scenario_filter_stray_close_paren, which trails a
    !> complete expression, this one is reached inside the parser's own clause position.
    subroutine scenario_filter_close_paren_as_clause()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_close_as_clause.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1 and ) v < 3")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a ')' where a clause was expected"
    end subroutine scenario_filter_close_paren_as_clause

    !> A column name longer than the packed per-leaf width the bind(C) convention allows (64
    !> characters). Caught Fortran-side, before anything is packed for the C++ evaluator.
    subroutine scenario_filter_leaf_too_long()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_leaf_too_long.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add(repeat("a", 65)//" > 1")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an over-long filter column name"
    end subroutine scenario_filter_leaf_too_long

    !> The node cap counts across every %add call of one filter, including the `and` nodes that
    !> join them: 512 clauses (1023 nodes) plus one more clause plus the joining `and` is 1025.
    !> Distinct from scenario_filter_too_many_nodes, where one rule alone exceeds the cap.
    subroutine scenario_filter_too_many_nodes_across_adds()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=:), allocatable :: rule
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_nodes_across_adds.parquet"

        call write_filter_syntax_fixture(out_file)
        rule = "v > 0"
        do i = 2, 512
            rule = rule//" and v > 0"
        end do
        call filt%add(rule)
        call filt%add("v > 0")
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader whose combined %add rules exceed the node limit"
    end subroutine scenario_filter_too_many_nodes_across_adds

    !> A row-group-scoped filter whose range runs past the file's own row-group count. Validated
    !> C++-side (parquet_reader_set_filter), where the row-group count lives, and reported through
    !> the same clean Fortran error stop as every other filter rejection.
    subroutine scenario_filter_scope_out_of_range()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_scope_range.parquet"

        call write_filter_syntax_fixture(out_file)
        call parquet_open_reader(reader, out_file)
        call filt%add("v > 1")
        call parquet_reader_set_filter(reader, filt, 1, 99)
        print '(a)', "unexpectedly applied a filter scoped past the file's last row group"
    end subroutine scenario_filter_scope_out_of_range

    !> The row-BOUNDED form's own range check, which is a separate one from the row-group scope
    !> above: it bounds physical ROWS, so it is validated against the file's row count rather than
    !> its row-group count. A range running past the last row would otherwise index the mask out of
    !> bounds while building it.
    !>
    !> Negative control first: a valid row range over the same reader must be accepted, so a check
    !> that refused every row range would fail here instead of passing.
    subroutine scenario_filter_row_range_out_of_range()
        type(parquet_reader) :: reader, reader2
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_row_range.parquet"

        call write_filter_syntax_fixture(out_file)
        call filt%add("v > 1")
        ! A range well inside the file: permitted. On its own reader, because a filter can only be
        ! installed once and only before any column is read.
        call parquet_open_reader(reader2, out_file)
        call parquet_reader_set_filter(reader2, filt, 0_int64, 0_int64, 1_int64, 2_int64)
        print '(a)', "a filter bounded to rows 1..2 was accepted"
        call parquet_open_reader(reader, out_file)
        call parquet_reader_set_filter(reader, filt, 0_int64, 0_int64, 1_int64, 999999_int64)
        print '(a)', "unexpectedly applied a filter bounded past the file's last row"
    end subroutine scenario_filter_row_range_out_of_range

    !> The same validation from the other end: a reversed range (lo > hi) names no row groups at
    !> all, which is a caller mistake rather than an empty result.
    subroutine scenario_filter_scope_reversed()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_scope_reversed.parquet"

        call write_filter_syntax_fixture(out_file)
        call parquet_open_reader(reader, out_file)
        call filt%add("v > 1")
        call parquet_reader_set_filter(reader, filt, 2, 1)
        print '(a)', "unexpectedly applied a filter with a reversed row-group range"
    end subroutine scenario_filter_scope_reversed

    !> The chunked read path converts freely between NUMERIC kinds -- it shares the whole-column
    !> path's convert_values_to_* helpers -- but a boolean chunk read is strict: it requires the
    !> stored column to actually be BOOL. This is the negative control for
    !> `test_read_column_chunk_numeric_conversion` (`test/test_reading.f90`), which asserts the
    !> converting half; without it that test would pass equally against a chunk path that had no
    !> type checking left at all.
    subroutine scenario_chunk_read_bool_type_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(4) = [1, 2, 3, 4]
        logical :: flags(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_chunk_bool_mismatch.parquet"

        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column_chunk(reader, "v", 1_int64, flags)
        print '(a)', "unexpectedly chunk-read an int32 column into a logical array"
    end subroutine scenario_chunk_read_bool_type_mismatch

    !> Writes a 12-row file with two rows per row group, i.e. six row groups whose spans are
    !> 1..2, 3..4, 5..6, 7..8, 9..10, 11..12. Both containment scenarios below need a layout whose
    !> row-group boundaries are known exactly; taking the filename as an argument is what keeps
    !> two scenarios running concurrently under xargs -P from truncating each other's fixture.
    subroutine write_row_group_layout_fixture(out_file)
        character(len=*), intent(in) :: out_file !! parquet file to (re)create.
        type(parquet_writer) :: writer
        integer(int32) :: v(12), i

        v = [(i, i = 1, 12)]
        call parquet_open_writer(writer, out_file, chunk_size=2)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine write_row_group_layout_fixture

    !> The row-BOUNDED form's containment check: the physical row range must lie INSIDE the rows
    !> its row-group range spans. Both ranges can be individually valid and still describe
    !> disjoint parts of the file, in which case the caller used to receive their intersection --
    !> possibly empty, and an empty result is indistinguishable from a selective filter that
    !> matched nothing, so the mistake reported as data rather than as an error
    !> (feature_risks.md Risk-81).
    !>
    !> Negative control first: row groups 2..4 span rows 3..8, which DOES contain rows 5..8, so a
    !> guard that fired unconditionally would fail here instead of passing.
    subroutine scenario_filter_row_range_outside_row_groups()
        type(parquet_reader) :: reader, reader2
        type(parquet_filter) :: filt
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_row_range_span.parquet"

        call write_row_group_layout_fixture(out_file)
        call filt%add("v > 0")
        call parquet_open_reader(reader2, out_file)
        call parquet_reader_set_filter(reader2, filt, 2_int64, 4_int64, 5_int64, 8_int64)
        print '(a)', "a filter whose row range lies inside its row groups was accepted"
        ! Row groups 2..3 span rows 3..6, so rows 5..8 run past their last row. This is the
        ! maintainer's own example (2, 3, 5, 8), and the fixture's two-rows-per-row-group layout
        ! is what makes those literal numbers non-contained.
        call parquet_open_reader(reader, out_file)
        call parquet_reader_set_filter(reader, filt, 2_int64, 3_int64, 5_int64, 8_int64)
        print '(a)', "unexpectedly applied a filter whose row range runs outside its row groups"
    end subroutine scenario_filter_row_range_outside_row_groups

    !> `row_group_lo = 0` means "all row groups" on the memory-bounded engine, and the engine is
    !> chosen by whether the row-group arguments were supplied at all rather than by their value.
    !> Asserts both halves of that, since either alone passes against the wrong implementation:
    !>
    !>  - the two forms agree on the ANSWER (same surviving row count over the same file), and
    !>  - they differ in what they leave CACHED, which is the whole point of the distinction.
    !>
    !> The observable is parquet_debug_get_physical_column_read_count around a READ of the filter
    !> column after the filter is installed: cached, that read is served from column_cache and the
    !> counter does not move; uncached, it is a genuine whole-column read and the counter reaches 1.
    !> Measuring at the set_filter call instead does not work -- the whole-file engine reads its
    !> filter columns through its own batched, thread-parallel path rather than through
    !> get_single_chunk_array, so both forms read 0 there and the difference is invisible.
    !>
    !> Out-of-process rather than a test-drive test because that counter is process-global and
    !> test-drive runs a suite's tests concurrently. Expected to exit cleanly
    !> (expect_abort=0 in tools/run_error_scenarios.sh), so reaching an error stop is the failure.
    subroutine scenario_filter_all_row_groups_bounded()
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

        type(parquet_reader) :: reader_all, reader_base, reader_open
        type(parquet_filter) :: filt
        integer(int64) :: nrows_all, nrows_base, nrows_open, reads_all, reads_base, reads_open
        integer(int32), allocatable :: v_all(:), v_base(:), v_open(:)
        character(len=64) :: buf
        character(len=*), parameter :: out_file = "test_run/error_scenario_filter_all_row_groups.parquet"

        call write_row_group_layout_fixture(out_file)
        call filt%add("v > 8")

        ! (0, 0): all row groups, one at a time, nothing cached -- so reading the filter column
        ! afterwards is a genuine whole-column read.
        call parquet_open_reader(reader_all, out_file)
        call parquet_reader_set_filter(reader_all, filt, 0_int64, 0_int64)
        call parquet_get_nrows(reader_all, nrows_all)
        allocate(v_all(nrows_all))
        call parquet_debug_reset_physical_column_read_count()
        call parquet_read_column(reader_all, "v", v_all)
        reads_all = parquet_debug_get_physical_column_read_count()
        call parquet_close_reader(reader_all)

        ! The negative control: no row-group arguments at all, so the whole-file engine, which
        ! leaves the filter column decoded -- the same read costs no disk read at all.
        call parquet_open_reader(reader_base, out_file)
        call parquet_reader_set_filter(reader_base, filt)
        call parquet_get_nrows(reader_base, nrows_base)
        allocate(v_base(nrows_base))
        call parquet_debug_reset_physical_column_read_count()
        call parquet_read_column(reader_base, "v", v_base)
        reads_base = parquet_debug_get_physical_column_read_count()
        call parquet_close_reader(reader_base)

        ! The third arm exists because the sentinel travels through a SECOND call site: an
        ! open-time filter= shares parquet_apply_filter with the post-open form, so a 0 passed
        ! there instead of no_row_group_scope silently moves every open-time filter onto the
        ! bounded engine. That changes no answer anywhere -- only what stays cached -- and was
        ! confirmed to be caught by no test in the suite before this arm was added.
        call parquet_open_reader(reader_open, out_file, filter=filt)
        call parquet_get_nrows(reader_open, nrows_open)
        allocate(v_open(nrows_open))
        call parquet_debug_reset_physical_column_read_count()
        call parquet_read_column(reader_open, "v", v_open)
        reads_open = parquet_debug_get_physical_column_read_count()
        call parquet_close_reader(reader_open)

        if (nrows_open /= 4_int64 .or. .not. all(v_open == v_all)) then
            error stop "scenario_filter_all_row_groups_bounded: an open-time filter= must select the same rows"
        end if
        if (reads_open /= 0_int64) then
            write(buf, '(i0)') reads_open
            error stop "scenario_filter_all_row_groups_bounded: an open-time filter= must leave the filter " // &
                "column cached, but reading it cost " // trim(buf) // " whole-column read(s)"
        end if

        if (nrows_all /= 4_int64 .or. nrows_base /= nrows_all) then
            write(buf, '(i0,a,i0)') nrows_all, " vs ", nrows_base
            error stop "scenario_filter_all_row_groups_bounded: expected 4 surviving rows from both forms, got " // &
                trim(buf)
        end if
        if (.not. all(v_all == [9_int32, 10_int32, 11_int32, 12_int32]) .or. .not. all(v_base == v_all)) then
            error stop "scenario_filter_all_row_groups_bounded: the two forms must select the same rows (9..12)"
        end if
        if (reads_all /= 1_int64) then
            write(buf, '(i0)') reads_all
            error stop "scenario_filter_all_row_groups_bounded: (0,0) must cache nothing, so reading the filter " // &
                "column should cost exactly 1 whole-column read, got " // trim(buf)
        end if
        if (reads_base /= 0_int64) then
            write(buf, '(i0)') reads_base
            error stop "scenario_filter_all_row_groups_bounded: the two-argument form must leave the filter " // &
                "column cached, but reading it cost " // trim(buf) // " whole-column read(s)"
        end if
    end subroutine scenario_filter_all_row_groups_bounded

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

    !> parquet_reader_print_stat's "sample:" line (added alongside the pre-existing "rows: N (of M
    !> total)" summary -- see scenario_print_stat_filtered_rows above for that one). sample_seed=42_int64
    !> (> 0) so the reported seed is the exact caller-supplied value, not an entropy-drawn one --
    !> a fraction of exactly 0.0 would also be deterministic, but skips the draw entirely and
    !> always reports seed=0 regardless of sample_seed (see parquet_apply_sample's own comment in
    !> parquet_read.f90), which wouldn't prove a caller-supplied seed round-trips into this line.
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

        call parquet_open_reader(reader, out_file, sample_fraction=0.4_real64, sample_seed=42_int64)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the sample: fraction=... summary line"
    end subroutine scenario_print_stat_sampled_rows

    !> parquet_reader_print_stat's "released" branch (parquet_wrapper.cpp): a column that was read
    !! (so it is "touched" and gets a row in the report) but then freed via parquet_release_column
    !! before the close, so its cached array is gone by the time print_stat walks the touched list.
    !! Every value-derived cell (col_size/len_str/min/max/...) has to be reported some other way
    !! than looking the array up -- an unconditional lookup there would be a use of a released
    !! entry, and reporting nothing at all would silently drop the fact that the column was ever
    !! touched. Distinct from every other print_stat_* scenario above, none of which release
    !! anything before closing.
    subroutine scenario_print_stat_released_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5)
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_released_column.parquet"

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "a", a_values)
        call parquet_release_column(reader, "a")
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the released-column row (col_size/len_str left blank, marked released)"
    end subroutine scenario_print_stat_released_column

    !> parquet_reader_print_stat's "sort: %s\n" line -- the sort-key text as %add received it,
    !! re-rendered in full, printed only when a sort is active. None of the other print_stat_*
    !! scenarios apply a sort (scenario_print_stat_sampled_rows is the closest sibling, doing the
    !! same for "sample:").
    subroutine scenario_print_stat_sorted_rows()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: a_values(5)
        integer :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_sorted_rows.parquet"

        a_values = [(6 - i, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_close_writer(writer)

        call srt%add("a asc")
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a)', "print_stat covered the sort: <key text> summary line"
    end subroutine scenario_print_stat_sorted_rows

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

    !> parquet_reader_set_sample's keep_len guard (parquet_wrapper.cpp): the mask Fortran hands
    !> over must cover exactly the file's physical rows, or the C++ side would read past its end.
    !>
    !> The guard CANNOT fire through the public API -- parquet_apply_sample sizes the mask from
    !> `parquet_reader_get_total_nrows` on the very handle the C++ side then compares against, so
    !> the two values come from one source and cannot disagree. It exists because a future change
    !> could give the length a different origin (a row range, a slice) and the failure would then
    !> be a silent out-of-bounds read rather than an error; tools/check_bindc_boundary.py checks
    !> signatures and never buffer lengths, so nothing else covers it. The debug hook below is what
    !> makes it testable instead of defensive code no fixture can reach: it makes the guard expect
    !> one row MORE than the file has, so the correct mask Fortran built is rejected.
    !>
    !> The control arm matters as much as the abort: the same open runs cleanly with the hook clear
    !> first, which is what proves the guard is not simply firing unconditionally.
    subroutine scenario_sample_mask_length_mismatch()
        interface
            subroutine parquet_debug_set_force_sample_len_mismatch(enable) &
                bind(C, name="parquet_debug_set_force_sample_len_mismatch")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero makes the keep_len guard expect one row too many; 0 restores.
            end subroutine parquet_debug_set_force_sample_len_mismatch
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: v(3) = [1, 2, 3]
        character(len=*), parameter :: out_file = "test_run/error_scenario_sample_mask_length_mismatch.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        ! Negative control: with the hook clear the identical open must succeed.
        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64, sample_seed=5_int64)
        call parquet_close_reader(reader)
        print '(a)', "control: the sampled open succeeded with the length hook clear"

        call parquet_debug_set_force_sample_len_mismatch(1)
        call parquet_open_reader(reader, out_file, sample_fraction=0.5_real64, sample_seed=5_int64)
        print '(a)', "unexpectedly opened a sampled reader despite the forced mask-length mismatch"
    end subroutine scenario_sample_mask_length_mismatch

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

    !> A column with a genuine Parquet Null, read with is_valid= (so the read itself doesn't
    !> abort), against a qc-maml field with a bare "miss:" key (present but no value after the
    !> colon) must print exactly one aggregate Null-presence WARNING with qc_soft=.true. (The
    !> default, qc_soft=.false., aborts instead -- see scenario_qc_null_violation_hard_aborts.)
    !>
    !> The bare "miss:" line is what asks for the check: it exercises parquet_parse_qc_maml's
    !> empty-value branch, which is the ONLY branch that sets null_values_allowed = .false.
    !> Omitting miss: entirely is a different case with the opposite outcome -- nothing is checked
    !> -- so the two must not be conflated here.
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

    !> The third read-side qc: miss: state, and the only one that had no test: a qc: block that
    !> declares min:/max: but NO miss: says nothing about Nulls, so reading a Null in that column is
    !> not a violation. parquet_qc_rule%null_values_allowed defaults to .true. for exactly this case,
    !> and its own doc-comment calls that default load-bearing.
    !>
    !> The scenario carries its own control, over one file and one read: `omitted` declares no miss:
    !> and must stay silent, while `explicit` declares an EMPTY miss: over data holding a Null in the
    !> same position and must warn. Without that second column a checker whose read-side Null check
    !> never ran at all would pass this scenario. Both columns are read with null_value= so the
    !> strict-Null read guard does not fire first and mask the question.
    !>
    !> Exits cleanly (exit 0): qc_soft=.true. makes a violation a WARNING rather than an abort.
    !> This is the state the user guide had backwards -- it claimed omitting miss: made a Null a
    !> violation, which is what an explicit EMPTY miss: does.
    subroutine scenario_qc_miss_omitted_no_read_violation()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: omitted(4), explicit(4), back(4)
        logical :: valid(4)

        omitted = [1, 2, 3, 4]
        explicit = [5, 6, 7, 8]
        valid = [.true., .false., .true., .true.]   ! element 2 is Null in both columns

        call parquet_open_writer(writer, "test_run/qc_miss_omitted.parquet")
        call parquet_write_column(writer, "omitted", omitted, is_valid=valid)
        call parquet_write_column(writer, "explicit", explicit, is_valid=valid)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_miss_omitted.maml", [character(len=32) :: &
            "fields:", &
            "- name: omitted", &
            "  qc:", &
            "    min: 0", &
            "- name: explicit", &
            "  qc:", &
            "    min: 0", &
            "    miss:"])

        call parquet_open_reader(reader, "test_run/qc_miss_omitted.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_miss_omitted.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "omitted", back, null_value=-1_int32)
        call parquet_read_column(reader, "explicit", back, null_value=-1_int32)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_miss_omitted_no_read_violation

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

    !> Default (qc_soft=.false., hard): an unexpected Null with qc active aborts the process, even
    !> when is_valid= was passed so the read itself would otherwise succeed. "Unexpected" means the
    !> field declares an explicit, EMPTY qc: miss: -- the bare "miss:" line in the maml below is
    !> load-bearing, not decoration. Drop it and this scenario stops aborting, because a field with
    !> no miss: at all says nothing about Nulls and none are checked (see
    !> parquet_column_type%qc_allow_null); scenario_qc_miss_absent_no_warning is that case, and is
    !> this scenario's negative control on the read side.
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
            "fields:", "- name: id", "  qc:", "    min: 0", "    miss:"])

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

    !> Regression scenario for feature_doc.md point 7's F1 finding: a MAML source line longer
    !! than parquet_metadata's maml_max_line_len (1024 characters) used to be silently truncated
    !! by a fixed-length `read(unit,'(A)')` (iostat still 0), producing incomplete metadata with
    !! no diagnostic. parquet_metadata_maml.f90's parquet_read_maml_source_lines now detects this
    !! via a non-advancing read + size= and aborts naming the offending line number, instead.
    subroutine scenario_maml_line_too_long()
        type(parquet_schema) :: schema
        character(len=1100) :: long_info

        long_info = repeat("x", 1100)
        call write_text_file("test_run/maml_line_too_long.maml", [character(len=1200) :: &
            "table: line_too_long_test", "fields:", "- name: id0", "  data_type: int32", &
            "  info: " // trim(long_info)])

        call parquet_parse_maml("test_run/maml_line_too_long.maml", schema)
        print '(a)', "unexpectedly parsed a MAML file with a line exceeding the length limit"
    end subroutine scenario_maml_line_too_long

    !> get_parquet_maml resolves an embedded fixture by name and error stops on
    !> one it does not have -- the `case default` arm of the select case
    !> tools/generate_parquet_maml.sh emits. The same template serves both of
    !> that script's modes, so this also pins the message a DOWNSTREAM project's
    !> generated parquet_maml produces (see
    !> doc/pages/utilities/embedding-maml-schemas.md); nothing in this repository
    !> compiles that mode, which is what tools/check_downstream_maml_module.sh is
    !> for.
    !>
    !> The successful lookup first is the negative control, and it is what makes
    !> the abort evidence: without it the scenario would pass just as happily if
    !> get_parquet_maml aborted on EVERY name, or if the module had stopped
    !> embedding anything at all. It also exercises the extension-optional
    !> matching from the other side -- "maml_example" without the .maml.
    subroutine scenario_embedded_maml_unknown_name()
        type(parquet_maml_file) :: maml

        maml = get_parquet_maml("maml_example")
        if (.not. allocated(maml%lines)) then
            print '(a)', "control lookup returned a fixture with no lines"
            return
        end if
        print '(a,i0,a)', "control: get_parquet_maml('maml_example') returned ", &
            size(maml%lines), " lines"

        maml = get_parquet_maml("no_such_embedded_schema")
        print '(a)', "unexpectedly resolved an embedded MAML name that does not exist"
    end subroutine scenario_embedded_maml_unknown_name

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
    !> doc comment on parquet_open_reader in src/parquet_core.f90.
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

    !> A protected VECTOR column with a single null ELEMENT must abort.
    !>
    !> New COVERAGE, not new behaviour: parquet_check_protected takes the FLATTENED mask, so it has
    !> always rejected one null element, and the existing protected scenarios only ever exercised
    !> scalar columns. Worth having precisely because element-granular validity makes a
    !> one-element-null vector column reachable through the table layer for the first time, so
    !> this path is about to be used in ways it was not before.
    subroutine scenario_write_protected_vector_element_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3, 2)
        logical :: is_valid(3, 2)

        values = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [3, 2])
        is_valid = .true.
        is_valid(2, 2) = .false.   ! ONE element, in the middle of its row

        schema%maml%name = "protected_vec_write.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  col_size: 3" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_protected_vec.parquet", schema)
        call parquet_write_column(writer, "a", values, is_valid=is_valid)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a Null element into a protected vector column"
    end subroutine scenario_write_protected_vector_element_null

    !> `extra: protected_cols:` is matched against a column's OUTPUT name -- the name written to
    !> the file -- so under a `col_map:` rename the list must carry the renamed name, not the
    !> stable internal one Fortran code calls parquet_write_column with. See
    !> doc/pages/types/supported-data-types.md's "Null values", and the output_name comparison in
    !> parquet_validate_maml_internal (parquet_metadata_maml.f90).
    !>
    !> Here `internal` is written under the output name `published`, `protected_cols:` lists
    !> `published`, and a .false. mask entry must abort. Its NEGATIVE CONTROL is the separate
    !> scenario protected_col_map_internal_name_rejected below, which lists `internal` instead --
    !> without it, this scenario would pass equally against an implementation that protected every
    !> column, or that matched on either name.
    subroutine scenario_protected_col_map_output_name()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        schema%maml%name = "protected_col_map.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_map_table", &
            "extra:", &
            "  col_map:", &
            "  - internal: published", &
            "  protected_cols: published", &
            "fields:", &
            "- name: published", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        ! The write uses the INTERNAL name; the protection was declared under the output name.
        call parquet_open_writer(writer, "test_run/error_scenario_protected_col_map.parquet", schema)
        call parquet_write_column(writer, "internal", values, is_valid=is_valid)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a Null into a column protected under its output name"
    end subroutine scenario_protected_col_map_output_name

    !> The negative control for scenario_protected_col_map_output_name, above: the same MAML with
    !> `protected_cols:` listing the INTERNAL name instead of the output name.
    !>
    !> The interesting part is that this does not merely fail to protect the column -- it fails
    !> validation outright, because parquet_validate_maml requires every protected_cols: entry to
    !> match one of this MAML's own declared output names. So getting the name wrong is LOUD, not
    !> silent, which is why no feature_risks.md entry is proposed for it. The two scenarios
    !> together pin the matching rule in both directions: the output name protects, and the
    !> internal name is rejected rather than quietly ignored.
    subroutine scenario_protected_col_map_internal_name_rejected()
        type(parquet_schema) :: schema

        schema%maml%name = "protected_col_map_internal.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_map_table", &
            "extra:", &
            "  col_map:", &
            "  - internal: published", &
            "  protected_cols: internal", &
            "fields:", &
            "- name: published", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly accepted a protected_cols: entry naming an internal column name"
    end subroutine scenario_protected_col_map_internal_name_rejected

    !> A protected column may hold no Null WHATEVER ARGUMENT EXPRESSED IT. A date/time/timestamp
    !> column takes no is_valid= at all -- its null state lives in the element -- and it is still
    !> covered, because parquet_check_protected is reached from the temporal write path too
    !> (temporal_valid_ptr, parquet_write_temporal.f90). Documented in
    !> doc/pages/types/supported-data-types.md's "Null values".
    !>
    !> The CONTROL runs first, in the same process: an all-set timestamp column written to its own
    !> file must succeed. Without it, a guard that refused every temporal write into a protected
    !> column would pass this scenario just as well.
    subroutine scenario_write_protected_temporal_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_timestamp) :: ts(3)

        schema%maml%name = "protected_temporal.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_ts_table", &
            "extra:", &
            "  protected_cols: t", &
            "fields:", &
            "- name: t", &
            "  data_type: timestamp[us]" ]

        call parquet_parse_maml(schema)

        ! Control: every element set, so the column carries no null and the write must succeed.
        call ts(1)%set(2024, 1, 31, 12, 0, 0)
        call ts(2)%set(2024, 2, 1, 12, 0, 0)
        call ts(3)%set(2024, 2, 2, 12, 0, 0)
        call parquet_open_writer(writer, "test_run/error_scenario_protected_ts_ok.parquet", schema)
        call parquet_write_column(writer, "t", ts)
        call parquet_close_writer(writer)

        ! Now the same column with element 2 left default-initialized, i.e. null.
        call ts(2)%set_null()
        call parquet_open_writer(writer, "test_run/error_scenario_protected_ts_null.parquet", schema)
        call parquet_write_column(writer, "t", ts)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a null timestamp element into a protected column"
    end subroutine scenario_write_protected_temporal_null

    !> The parquet_string_column half of the same rule: that container carries its own per-element
    !> null state and takes no is_valid= either, and a protected column must still refuse an
    !> %append_null(). Reached via write_string_compact_tail's own parquet_check_protected call
    !> (parquet_write_string.f90), which derives the mask from %is_null.
    !>
    !> Same shape as the temporal scenario above: the null-free control writes first and must
    !> succeed, so this cannot pass against a guard that refuses every compact string write.
    subroutine scenario_write_protected_string_column_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: names

        schema%maml%name = "protected_strcol.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_str_table", &
            "extra:", &
            "  protected_cols: s", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 8" ]

        call parquet_parse_maml(schema)

        ! Control: no nulls in the container, so the write must succeed.
        call names%append_string("alpha")
        call names%append_string("bravo")
        call parquet_open_writer(writer, "test_run/error_scenario_protected_strcol_ok.parquet", schema)
        call parquet_write_column(writer, "s", names)
        call parquet_close_writer(writer)

        ! One genuine null in the container, expressed by %append_null rather than by any mask.
        call names%clear()
        call names%append_string("alpha")
        call names%append_null()
        call parquet_open_writer(writer, "test_run/error_scenario_protected_strcol_null.parquet", schema)
        call parquet_write_column(writer, "s", names)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a null into a protected parquet_string_column"
    end subroutine scenario_write_protected_string_column_null

    !> A streamed column's nullability is fixed by its FIRST row group, so every later row group
    !> must use the same masked or unmasked form. Here row group 1 passes an is_valid mask and row
    !> group 2 omits it -- which would silently discard the caller's stated intent to allow Nulls.
    !>
    !> The CONTROL is inside the scenario: row group 1's masked write must succeed. Without it a
    !> guard that rejected every masked chunk write would pass this scenario too.
    subroutine scenario_chunk_mask_dropped_after_first_row_group()
        type(parquet_writer) :: writer
        integer(int32) :: v(3) = [1_int32, 2_int32, 3_int32]
        logical :: mask(3) = [.true., .true., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_chunk_mask_dropped.parquet")
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "c", v, is_valid=mask)   ! control: must succeed
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "c", v)                  ! mask dropped -> aborts
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly dropped an is_valid mask after the first row group"
    end subroutine scenario_chunk_mask_dropped_after_first_row_group

    !> The other direction, and the one that would otherwise corrupt rather than merely surprise:
    !> row group 1 passes no mask, so the column's field is fixed NON-nullable, and row group 2
    !> then passes one. Were this allowed, a .false. entry in that later mask would be a Null
    !> written into a field whose schema forbids it.
    !>
    !> Control, again inside the scenario: row group 1's unmasked write must succeed first.
    subroutine scenario_chunk_mask_added_after_first_row_group()
        type(parquet_writer) :: writer
        integer(int32) :: v(3) = [1_int32, 2_int32, 3_int32]
        logical :: mask(3) = [.true., .false., .true.]

        call parquet_open_writer(writer, "test_run/error_scenario_chunk_mask_added.parquet")
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "c", v)                  ! control: must succeed
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 3)
        call parquet_write_column_chunk(writer, "c", v, is_valid=mask)   ! mask added -> aborts
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly added an is_valid mask after an unmasked first row group"
    end subroutine scenario_chunk_mask_added_after_first_row_group

    !> schema%set_protected on a column the schema does not declare aborts, rather than silently
    !> protecting nothing -- a typo there would otherwise leave the caller believing a column is
    !> Null-protected when it is not.
    !>
    !> The control is the successful call on a real column immediately before it.
    subroutine scenario_set_protected_unknown_column()
        type(parquet_schema) :: schema

        schema%maml%name = "set_protected_unknown.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: sp_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call schema%set_protected("a")          ! control: a real column, must succeed
        call schema%set_protected("no_such")    ! -> aborts
        print '(a)', "unexpectedly accepted set_protected on an unknown column"
    end subroutine scenario_set_protected_unknown_column

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

    !> A qc: min: that is a plain run of digits too large for int64 must be REJECTED, not accepted
    !> with whatever the failed read left behind.
    !>
    !> This is the one input that reaches parquet_qc_bound_as_int64_text's own failure arm. That
    !> helper checks the text's SHAPE by hand first -- an optional sign then nothing but digits --
    !> so by the time it runs its list-directed read, a nonzero iostat can mean one thing only: the
    !> value does not fit in int64. Every other rejected bound in this file is turned away earlier,
    !> by the shape test (not_a_number) or by the real64 route that follows (1.5, 5000000000 --
    !> which is digits, fits int64 easily, and is rejected for being outside int32).
    !>
    !> huge(int64) + 1 rather than a longer run of nines, because it is exactly the first value
    !> that does not fit: a fixture one digit wider would pass just as well against a helper whose
    !> ceiling had moved. gfortran reports iostat 5010 here and leaves a GARBAGE value in the
    !> integer, which is what makes the arm's `value = 0` load-bearing rather than tidy -- the
    !> caller reads that variable when the function answers .true.
    !>
    !> The column is int64 deliberately: on an int32 column the real64 route that follows would
    !> reject the value for being outside int32 whatever the int64 helper had answered, so the
    !> abort would not be evidence about this arm at all.
    subroutine scenario_validate_qc_min_overflows_int64()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_overflows_int64.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int64", &
            "  qc:", &
            "    min: 9223372036854775808" ]

        call parquet_validate_maml(maml)
        print '(a)', "unexpectedly accepted a qc: min: that does not fit in int64"
    end subroutine scenario_validate_qc_min_overflows_int64

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

    !> qc: miss: accepts exactly empty, Null or NA (the latter two
    !> case-insensitively). Anything else is rejected by parquet_validate_maml,
    !> naming the offending text. This matters more than a syntax check usually
    !> would: before this was enforced, an unrecognized value resolved silently
    !> to "Nulls are NOT expected", so a typo such as "miss: none" switched Null
    !> validation ON for a column whose author was declaring the opposite. Its
    !> negative control is scenario_validate_qc_miss_valid_values, which must
    !> keep every legal form working. See feature_risks.md.
    subroutine scenario_validate_qc_miss_bad_value()
        type(parquet_maml_file) :: maml

        maml%name = "qc_miss_bad_value.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    miss: none" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_miss_bad_value

    !> Negative control for scenario_validate_qc_miss_bad_value: every LEGAL
    !> qc: miss: form must still validate and parse. Without this, that
    !> scenario passes just as happily against a check that rejects every
    !> miss: value, including the three the format actually defines.
    !> Exits cleanly (exit 0) and prints nothing.
    subroutine scenario_validate_qc_miss_valid_values()
        type(parquet_schema) :: schema

        schema%maml%name = "qc_miss_valid_values.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    miss: Null", &
            "- name: b", &
            "  data_type: int32", &
            "  qc:", &
            "    miss: null", &
            "- name: c", &
            "  data_type: int32", &
            "  qc:", &
            "    miss: NA", &
            "- name: d", &
            "  data_type: int32", &
            "  qc:", &
            "    miss: na", &
            "- name: e", &
            "  data_type: int32", &
            "  qc:", &
            "    miss:", &
            "- name: f", &
            "  data_type: int32" ]

        call parquet_validate_maml(schema%maml)
        call parquet_parse_maml(schema)
    end subroutine scenario_validate_qc_miss_valid_values

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

    !> An int64 qc bound must be judged in int64, not after widening every value to real64.
    !> Two columns, one assertion each way:
    !>
    !>   `over` holds 2**53 + 1 against `max: 2**53`. real64 cannot represent 2**53 + 1, so
    !>   widening it rounds it DOWN onto the bound and the value looks compliant -- which is
    !>   exactly what this library used to do, silently passing a value that violates. It must
    !>   now warn.
    !>
    !>   `at` holds 2**53 itself against the same bound, which genuinely satisfies `<=`. It must
    !>   NOT warn -- the negative control, without which a checker that simply warns about every
    !>   int64 column would pass this scenario.
    subroutine scenario_qc_int64_beyond_float64_precision()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: over(1) = [9007199254740993_int64]  !! 2**53 + 1: not representable in real64.
        integer(int64) :: at(1) = [9007199254740992_int64]    !! 2**53 exactly: representable, and compliant.

        schema%maml%name = "qc_int64_precision.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: over", &
            "  data_type: int64", &
            "  qc:", &
            "    max: 9007199254740992", &
            "- name: at", &
            "  data_type: int64", &
            "  qc:", &
            "    max: 9007199254740992" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_precision.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "over", over)
        call parquet_write_column(writer, "at", at)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_beyond_float64_precision

    !> A qc bound is judged as an exact int64 whenever the declared text IS a plain integer, so a
    !> bound PAST 2**53 constrains what it says rather than a rounded copy of itself. This is the
    !> positive half: 9007199254740994 genuinely exceeds a max of 9007199254740993, so exactly one
    !> WARNING must appear -- and both numbers must be rendered exactly, since a message built from
    !> real64 copies would print 0.9007199E+16 for the bound, the value AND the range, i.e. would
    !> quote a different number from the one the comparison used. Its negative half is
    !> scenario_qc_int64_exact_bound_no_false_violation. See feature_risks.md Risk-68.
    subroutine scenario_qc_int64_exact_bound_violation()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: over(1) = [9007199254740994_int64]  !! one past a bound of 2**53 + 1.

        schema%maml%name = "qc_int64_exact_bound.maml"
        schema%maml%lines = [character(len=48) :: &
            "table: qc_table", &
            "fields:", &
            "- name: over", &
            "  data_type: int64", &
            "  qc:", &
            "    max: 9007199254740993" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_exact_bound.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "over", over)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_exact_bound_violation

    !> Negative control for the scenario above, and the half that actually catches the defect this
    !> pair exists for. Two things must produce NO output at all:
    !>   * a value exactly EQUAL to a max: past 2**53 is compliant -- routing the bound through
    !>     real64 rounded 9007199254740993 down to ...992, so the value equal to its own declared
    !>     bound was reported as a violation;
    !>   * max: 9223372036854775807 (= huge(int64)) is a legal bound -- the real64 range test used
    !>     to reject it outright at parquet_validate_maml time, since that value and huge+1 are the
    !>     same real64, so this scenario would have ABORTED rather than merely warned.
    !> Exits cleanly (exit 0) and prints nothing.
    subroutine scenario_qc_int64_exact_bound_no_false_violation()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: at(1) = [9007199254740993_int64]    !! exactly the declared bound: compliant.
        integer(int64) :: small(1) = [1_int64]                !! well inside a huge(int64) bound.

        schema%maml%name = "qc_int64_exact_bound_ok.maml"
        schema%maml%lines = [character(len=48) :: &
            "table: qc_table", &
            "fields:", &
            "- name: at", &
            "  data_type: int64", &
            "  qc:", &
            "    max: 9007199254740993", &
            "- name: small", &
            "  data_type: int64", &
            "  qc:", &
            "    max: 9223372036854775807" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_exact_bound_ok.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "at", at)
        call parquet_write_column(writer, "small", small)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_exact_bound_no_false_violation

    !> qc_int64_satisfies applies the declared comparison operator to an int64 value and an
    !> exactly-int64 bound. Every existing scenario declares its bounds plainly, which parses to
    !> the inclusive ">=" / "<=" defaults, so the two STRICT arms were never taken -- and a
    !> strict operator silently behaving as its inclusive twin is a wrong answer nothing else
    !> would notice: it accepts exactly the boundary value the declaration excludes.
    !>
    !> `strict` holds the two boundary values, which violate `> 0` and `< 100` and must warn.
    !> `inside` is the negative control, one step in from each boundary: it satisfies both, so a
    !> checker that had degraded either strict operator into an inclusive one would still warn
    !> about `strict`, but one that warned about everything could not stay silent here.
    subroutine scenario_qc_int64_strict_operators()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: strict(2) = [0_int64, 100_int64]  !! exactly the excluded boundaries.
        integer(int64) :: inside(2) = [1_int64, 99_int64]   !! one step inside both: compliant.

        schema%maml%name = "qc_int64_strict.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: strict", &
            "  data_type: int64", &
            "  qc:", &
            "    min: > 0", &
            "    max: < 100", &
            "- name: inside", &
            "  data_type: int64", &
            "  qc:", &
            "    min: > 0", &
            "    max: < 100" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_strict.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "strict", strict)
        call parquet_write_column(writer, "inside", inside)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_strict_operators

    !> A fractional qc bound cannot be converted to an exact int64 one, so qc_numeric_i64 falls
    !> back to comparing in real64 instead of using qc_int64_satisfies. That fallback is only
    !> reachable the way this scenario reaches it: the SCHEMA type is float64 (which is what
    !> permits a fractional bound at all -- an int32/int64 schema type rejects one outright), and
    !> the caller hands the column int64 values, which the writer widens on the way out.
    !>
    !> `frac` violates at both ends and must warn. `frac_ok` is the negative control: the same
    !> declaration with values strictly between the two bounds, which must stay silent.
    subroutine scenario_qc_int64_values_fractional_bound()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: frac(2) = [1_int64, 10_int64]    !! below 1.5 and above 9.5.
        integer(int64) :: frac_ok(2) = [2_int64, 9_int64]  !! inside [1.5, 9.5]: compliant.

        schema%maml%name = "qc_int64_fractional.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: frac", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1.5", &
            "    max: 9.5", &
            "- name: frac_ok", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1.5", &
            "    max: 9.5" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_fractional.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "frac", frac)
        call parquet_write_column(writer, "frac_ok", frac_ok)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_values_fractional_bound

    !> Write-time qc has one checker per value kind, and float64's (qc_numeric_r64) was the only
    !> one never observed reporting a violation -- the existing scenarios cover int32, int64 and
    !> float32. A checker that computed the violation count but never reported it would satisfy
    !> every other qc test in the suite while telling a real64 caller nothing at all.
    !>
    !> `f64` violates; `f64_ok` is the negative control on the same declaration.
    subroutine scenario_qc_warning_float64()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: f64(3) = [0.25_real64, 5.0_real64, 12.5_real64]  !! first and last are out of range.
        real(real64) :: f64_ok(3) = [1.0_real64, 5.0_real64, 10.0_real64] !! all within [1, 10].

        schema%maml%name = "qc_warning_float64.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: f64", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1", &
            "    max: 10", &
            "- name: f64_ok", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1", &
            "    max: 10" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning_float64.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "f64", f64)
        call parquet_write_column(writer, "f64_ok", f64_ok)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_float64

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

    !> qc: min:/max: on a boolean field are accepted by validation -- including a reversed operator,
    !> since the direction check exempts boolean -- and then never enforced: this must write/close
    !> without error and without printing a WARNING.
    !>
    !> Scope note, and the reason this scenario's name changed: it covers min:/max: ONLY. A qc: miss:
    !> on a boolean field IS enforced, exactly as on any other type, because write_logical_flat calls
    !> parquet_check_qc_miss like the four numeric paths do. This scenario declares no miss: and
    !> writes no Null, so it can never observe that -- scenario_qc_miss_enforced_for_boolean is what
    !> covers it. An earlier name ("silently_ignored") claimed the whole qc: block was ignored here,
    !> which was wrong and had been copied into the user guide.
    subroutine scenario_qc_min_max_ignored_for_boolean()
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
    end subroutine scenario_qc_min_max_ignored_for_boolean

    !> The other half of boolean qc, and the half that was never covered: an explicit, EMPTY
    !> qc: miss: IS enforced on a boolean column, exactly as on any other type -- write_logical_flat
    !> calls parquet_check_qc_miss just as the four numeric write paths do.
    !>
    !> Both halves are in one file so the assertion pair is a true control: `banned` declares an
    !> empty miss: and must warn, `allowed` declares miss: Null over the SAME data and must not.
    !> A checker that warned about every boolean column, or one that had stopped enforcing miss:
    !> on booleans, fails exactly one of the two. See feature_risks.md.
    subroutine scenario_qc_miss_enforced_for_boolean()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: values(4) = [.true., .false., .true., .true.]
        logical :: valid(4) = [.true., .false., .true., .true.]  !! element 2 is Null.

        schema%maml%name = "qc_boolean_miss.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: banned", &
            "  data_type: boolean", &
            "  qc:", &
            "    miss:", &
            "- name: allowed", &
            "  data_type: boolean", &
            "  qc:", &
            "    miss: Null" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_boolean_miss.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "banned", values, is_valid=valid)
        call parquet_write_column(writer, "allowed", values, is_valid=valid)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_enforced_for_boolean

    !> An EXPLICIT, EMPTY qc: miss: declares that Nulls are NOT expected: writing a Null through
    !! an is_valid= mask must then print a WARNING naming the column -- checked here with NO
    !! explicit qc= passed to parquet_open_writer at all, to prove qc defaults to present(schema)
    !! rather than needing an explicit qc=.true. (see parquet_open_writer's qc doc comment).
    !!
    !! qc_miss="" is what declares that, and the emptiness is the point: %add_field tests
    !! present(qc_miss), not its length, precisely so this call is distinguishable from omitting
    !! the argument. Omitting it means the schema says nothing about Nulls and NONE are checked --
    !! that is scenario_qc_miss_absent_no_warning, this scenario's negative control.
    subroutine scenario_qc_miss_default_active_numeric_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        call schema%init(table="qc_miss_table")
        call schema%add_field("id", "int32", qc_miss="")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_default.parquet", schema)
        call parquet_write_column(writer, "id", values, is_valid=is_valid)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_default_active_numeric_warns

    !> Unprotecting a currently-protected column prints a WARNING and does NOT abort, whatever the
    !! protection's origin -- the origin is not recorded, so the message must not claim one. Both
    !! halves are exercised here on a schema with no .maml file anywhere: "p" is protected in code
    !! and then unprotected (must warn), while "q" is never protected and is set protected=.false.
    !! anyway (must stay silent).
    !!
    !! **"q" is the negative control and is what gives the scenario its teeth**: a guard that warned
    !! on every set_protected(..., .false.) call, rather than only on a real relaxation, would pass a
    !! test that only ever looked at "p". The wrapper asserts a clean exit plus the warning text, and
    !! the absence of any second WARNING line is what "q" contributes.
    subroutine scenario_set_protected_unprotect_warns()
        type(parquet_schema) :: schema

        call schema%init(table="protect_table")
        call schema%add_field("p", "int32")
        call schema%add_field("q", "int32")

        call schema%set_protected("q", .false.)  ! control: never protected -> silent
        call schema%set_protected("p")           ! protect in code (no MAML involved at all)
        call schema%set_protected("p", .false.)  ! -> the one WARNING this scenario expects
    end subroutine scenario_set_protected_unprotect_warns

    !> **The negative control for scenario_qc_miss_default_active_numeric_warns, and the test that
    !! pins the rule rather than one side of it.** Identical in every respect except that qc_miss is
    !! not passed at all: a field declaring no qc: miss: says nothing about Nulls, so writing one
    !! must print NOTHING -- on either side. Without this, the whole "absent means allowed" rule is
    !! asserted by no test, and a guard that warned unconditionally would still pass its sibling.
    !!
    !! Checked as an ABSENCE (required_stderr is not given; the wrapper asserts the WARNING does not
    !! appear), which is why the write happens twice here: once with a Null through is_valid=, and
    !! once through a parquet_string_column's own %append_null, so the two distinct paths into
    !! parquet_check_qc_miss are both covered by the absence.
    subroutine scenario_qc_miss_absent_no_warning()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]
        type(parquet_string_column) :: svalues

        call schema%init(table="qc_miss_table")
        call schema%add_field("id", "int32")     ! no qc_miss= -- the whole point
        call schema%add_field("s", "string")     ! ditto

        call svalues%append_string("apple")
        call svalues%append_null()
        call svalues%append_string("cherry")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_absent.parquet", schema)
        call parquet_write_column(writer, "id", values, is_valid=is_valid)
        call parquet_write_column(writer, "s", svalues)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_absent_no_warning

    !> Same as scenario_qc_miss_default_active_numeric_warns, but the field declares
    !! qc: miss: Null -- Nulls are expected here, so no WARNING should print.
    subroutine scenario_qc_miss_declared_null_no_warning()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        call schema%init(table="qc_miss_table")
        call schema%add_field("id", "int32", qc_miss="Null")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_allowed.parquet", schema)
        call parquet_write_column(writer, "id", values, is_valid=is_valid)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_declared_null_no_warning

    !> Same empty-qc_miss-warns shape as scenario_qc_miss_default_active_numeric_warns, but
    !! for a compact (parquet_string_column) string write -- the write path that reaches
    !! parquet_check_qc_miss via parquet_write_string.f90's is_null()-derived is_valid_flat rather
    !! than a caller-supplied is_valid= mask.
    subroutine scenario_qc_miss_string_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: values

        call schema%init(table="qc_miss_table")
        call schema%add_field("s", "string", qc_miss="")

        call values%append_string("apple")
        call values%append_null()
        call values%append_string("cherry")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_string.parquet", schema)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_string_warns

    !> Same empty-qc_miss-warns shape, for a parquet_date column -- the write path that
    !! reaches parquet_check_qc_miss via parquet_write_temporal.f90's temporal_valid_ptr.
    subroutine scenario_qc_miss_temporal_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_date) :: values(3)

        call values(1)%set(2024, 1, 1)
        ! values(2) left default-initialized -- a null date.
        call values(3)%set(2024, 1, 3)

        call schema%init(table="qc_miss_table")
        call schema%add_field("d", "date", qc_miss="")

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

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int32_out_of_range.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an out-of-int32-range float64 value to an int32 schema column without error"
    end subroutine scenario_write_float_to_int32_out_of_range

    !> A value that is BOTH non-integral and out of int32 range must report the
    !> **integrality** error, not the range one. The two checks are separate `if`s in
    !> parquet_float64_to_int32 and integrality comes first, so which message appears is
    !> purely a property of that ordering -- and nothing else in the suite pins it, because
    !> the two existing scenarios each trigger only one of the checks. Guards the ordering
    !> against a rewrite of the integrality test (see parquet_is_whole_number): swapping the
    !> two `if`s, or folding them into one, changes this message while leaving both
    !> single-condition scenarios green.
    subroutine scenario_write_float_to_int32_non_integral_and_out_of_range()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        ! Both wrong at once: 3.0e9 exceeds huge(int32), and .5 is below 2**52 so the value
        ! genuinely carries fractional bits rather than being a large-magnitude whole number.
        real(real64) :: values(1) = [3000000000.5_real64]

        call schema%init(table="float_both_wrong_table")
        call schema%add_field("v", "int32")

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int32_both_wrong.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a non-integral out-of-range float64 value without error"
    end subroutine scenario_write_float_to_int32_non_integral_and_out_of_range

    !> Writing a NaN to an integer schema column: there is no integer a NaN could become, so it
    !> takes the non-integral path, because `anint(NaN)` is a NaN and a NaN equals nothing --
    !> including itself. Pins the message a user gets, and pins it against any S7-9-style rewrite:
    !> a replacement that reaches an `int(NaN, int64)` conversion has undefined behaviour here,
    !> and one that classes a NaN as whole would let it through to be written as garbage.
    subroutine scenario_write_float_nan_to_int32()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1)

        values(1) = ieee_value(0.0_real64, ieee_quiet_nan)
        call schema%init(table="float_nan_table")
        call schema%add_field("v", "int32")

        call parquet_open_writer(writer, "test_run/error_scenario_float_nan_to_int32.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a NaN to an int32 schema column without error"
    end subroutine scenario_write_float_nan_to_int32

    !> A LARGE NEGATIVE out-of-int64-range float must report the range error, not the
    !> integrality one, and its positive twin above cannot see the difference. This matters for
    !> any S7-9-style rewrite of the integrality test: such a test needs a magnitude arm, and if
    !> that arm is not taken on the ABSOLUTE value, a large negative misses it and falls into an
    !> out-of-range `int()` conversion -- which answers "not whole" and produces the wrong
    !> message here while leaving every other scenario in this file green. Verified to catch
    !> exactly that defect in a candidate implementation.
    subroutine scenario_write_float_to_int64_negative_out_of_range()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [-real(huge(0_int64), real64) * 2.0_real64]

        call schema%init(table="float_neg_out_of_range_table")
        call schema%add_field("v", "int64")

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int64_neg_out_of_range.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a large negative out-of-int64-range float64 value without error"
    end subroutine scenario_write_float_to_int64_negative_out_of_range

    !> Same non-integral check as scenario_write_float_to_int32_non_integral,
    !> but for the int64 schema-column conversion path
    !> (parquet_float64_to_int64), which has its own identical check.
    subroutine scenario_write_float_to_int64_non_integral()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: values(1) = [3.5_real64]

        call schema%init(table="float_non_integral_i64_table")
        call schema%add_field("v", "int64")

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

        call parquet_open_writer(writer, "test_run/error_scenario_float_to_int64_out_of_range.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an out-of-int64-range float64 value to an int64 schema column without error"
    end subroutine scenario_write_float_to_int64_out_of_range

    !> n < 1 is not a valid thread pool capacity -- must error stop rather
    !> than silently passing an invalid value down to Arrow.
    subroutine scenario_set_max_threads_below_one()
        call parquet_set_arrow_threads(0)
        print '(a)', "unexpectedly accepted parquet_set_arrow_threads(0) without error"
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
    !> for an unrelated reason like a duplicate column name. Uses the same
    !> batches/iterations_per_batch magnitude as the reader scenario above
    !> (200 x 500, not the originally-much-smaller 50 x 100): the smaller
    !> writer-side count gave the ConcurrencyGuard far fewer chances to
    !> actually observe two threads inside the guarded region at once and
    !> was confirmed to intermittently finish without the race ever firing
    !> on a real CI runner, so it is now sized the same as the reader
    !> scenario's own already-tuned count.
    subroutine scenario_concurrent_calls_into_shared_writer()
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer, parameter :: batches = 200
        integer, parameter :: iterations_per_batch = 500
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

    !> The NEGATIVE control for the two scenarios above, and the only thing that covers the write
    !> path's early RETURNs: hands one writer between two threads *sequentially* and expects the
    !> concurrency guard NOT to fire (exit 0).
    !>
    !> **What would break without it.** Every write entry point claims the guard at its first
    !> statement via writer_lock (parquet_core.f90) and relies on Fortran finalizing that local to
    !> release it -- on the normal path and on each of the 64 early RETURNs across those procedures.
    !> A release that stopped happening would be invisible to the owning thread, which may re-enter a
    !> handle it already holds: everything single-threaded keeps passing, and the damage only appears
    !> when some *other* thread later touches that writer legitimately and aborts with a message
    !> blaming a concurrent access that never happened. So the leak has to be provoked on one thread
    !> and observed from another, which is exactly what this does.
    !>
    !> Thread 0 deliberately takes an early-return path before writing anything real -- a
    !> schema-defined column that is not `is_set`, which returns while holding the claim, since the
    !> claim is taken at the entry point's first statement -- and then writes a real column. Thread 1
    !> writes the remaining column and the file is closed. The `!$omp barrier` between the two halves
    !> is what makes this a hand-off rather than a race: the two blocks can never overlap, so the
    !> guard has nothing legitimate to complain about and any abort here is a real leak.
    !>
    !> Self-adapting like its two siblings: with only one thread available, both halves would run on
    !> the same thread, where a leaked claim is admitted as ordinary re-entry and proves nothing.
    subroutine scenario_writer_guard_sequential_handoff()
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer :: tid, nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) then
            print '(a)', "SKIPPED: OpenMP not active (omp_get_max_threads() <= 1); a hand-off needs two threads"
            return
        end if

        schema%maml%name = "handoff.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: handoff_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: skipped", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call schema%set_column_unavailable("skipped")

        call parquet_open_writer(writer, "test_run/error_scenario_writer_handoff.parquet", schema=schema)

        tid = 0
        !$omp parallel num_threads(2) default(shared) private(tid)
        !$ tid = omp_get_thread_num()
        if (tid == 0) then
            ! Returns early (the column is not is_set) while holding this entry point's claim.
            call parquet_write_column(writer, "skipped", values)
            call parquet_write_column(writer, "a", values)
        end if
        !$omp barrier
        if (tid == 1) then
            call parquet_write_column(writer, "b", values)
        end if
        !$omp end parallel

        call parquet_close_writer(writer)
        print '(a)', "sequential hand-off of one writer between two threads completed without the guard firing"
    end subroutine scenario_writer_guard_sequential_handoff

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
        call schema%get_field("does_not_exist", data_type=data_type)
        print '(a)', "unexpectedly found a non-existent field via schema%get_field(name=)"
    end subroutine scenario_get_field_by_name_not_found

    !> schema%get_field(index=) error stops if index is out of range.
    subroutine scenario_get_field_by_index_out_of_range()
        type(parquet_schema) :: schema
        character(len=:), allocatable :: name

        call schema%init(table="t")
        call schema%add_field("ra", "float64")
        call schema%get_field(5, name)
        print '(a)', "unexpectedly resolved an out-of-range index via schema%get_field(index=)"
    end subroutine scenario_get_field_by_index_out_of_range

    !> schema%add_field_from error stops if the named field doesn't exist on source_schema
    !> (via %get_field's own not-found error).
    subroutine scenario_add_field_from_source_not_found()
        type(parquet_schema) :: source, target

        call source%init(table="src")
        call source%add_field("ra", "float64")

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
        call schema%print_schema_info()
        print '(a)', "unexpectedly printed schema info with neither unit nor filename given"
    end subroutine scenario_print_schema_info_no_unit_no_filename

    !> schema%print_schema_info error stops if the given unit is not already open.
    subroutine scenario_print_schema_info_unit_not_open()
        type(parquet_schema) :: schema
        integer :: u

        call schema%init(table="t")
        call schema%add_field("x", "int32")

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

        call schema%print_schema_info(filename="test_run/no_such_subdir/print_schema_info_open_failure.txt")
        print '(a)', "unexpectedly printed schema info to a filename that could not be opened"
    end subroutine scenario_print_schema_info_open_failure

    !> %add_field keeps %cinfo in step incrementally instead of leaving it to an explicit
    !! parquet_parse_maml, so the per-field rules parquet_validate_maml used to catch at parse
    !! time have to be caught here or not at all -- an in-code schema may now never be validated
    !! as a whole document. It runs parquet_validate_field_rules, the same code the document
    !! validator calls, on the field it just parsed. The whole-document rules stay skipped, which
    !! the negative control below demonstrates: a one-field sub-document has no table: line of
    !! its own, so running the full validator on it would abort on every %add_field call ever
    !! made.
    subroutine scenario_schema_add_field_validates_field_rules()
        type(parquet_schema) :: ok_schema, schema

        call ok_schema%init(table="t")
        call ok_schema%add_field("fine", "int32", qc_min="0", qc_max="10")
        print '(a,i0)', "fields accepted with no whole-document validation: ", ok_schema%get_num_fields()
        call schema%init(table="t")
        call schema%add_field("x", "int32", qc_min="3.7")
        print '(a)', "unexpectedly accepted a qc_min that is not integral for an int32 column"
    end subroutine scenario_schema_add_field_validates_field_rules

    !> %add_metadata needs a metadata table to add to, which schema%init (or a parse) is what
    !! establishes -- before either, the entry would be discarded by whichever of them runs
    !! next rather than merely arriving early. Adding it AFTER %init but before any parse is
    !! deliberately legal and is covered positively by test_metadata.f90's
    !! test_add_metadata_interleaved_with_add_field, so the negative control below runs that
    !! call first and only then reaches the abort.
    subroutine scenario_schema_add_metadata_before_init()
        type(parquet_schema) :: ok_schema, schema

        call ok_schema%init(table="t")
        call ok_schema%add_metadata("k", 1_int32) ! negative control: legal, no parse in sight
        print '(a,i0)', "metadata entries after add_metadata with no parse: ", size(ok_schema%metadata%items)
        call schema%add_metadata("k", 1_int32)
        print '(a)', "unexpectedly added metadata to a schema that has not been initialized"
    end subroutine scenario_schema_add_metadata_before_init

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

    !> parquet_string_column%build_from, character-array form: an is_null mask whose length does
    !! not match `values` aborts rather than being read past its end. The check is cheap and the
    !! failure it prevents is not: the mask is walked once per element in the sizing pass and again
    !! when the null bits are written, so a short mask reads uninitialised memory twice and decides
    !! nullness from it -- a column that validates, with the wrong rows null and no symptom.
    subroutine scenario_string_build_from_character_mask_length()
        type(parquet_string_column) :: col
        character(len=4) :: vals(3)
        logical :: mask(2)
        vals = [character(len=4) :: "aa", "bb", "cc"]
        mask = [.true., .false.]
        call col%build_from(vals, is_null=mask)   ! 3 values, 2 mask entries -> aborts
        print '(a,i0)', "unexpectedly built from a short mask, null_count=", col%null_count()
    end subroutine scenario_string_build_from_character_mask_length

    !> parquet_string_column%append_values: the same mask-length check as build_from's character
    !! form, on the appending entry point. Separate scenario because it is a separate guard on a
    !! separate procedure -- deleting either one leaves the other's test passing.
    subroutine scenario_string_append_values_mask_length()
        type(parquet_string_column) :: col
        character(len=4) :: vals(3)
        logical :: mask(2)
        vals = [character(len=4) :: "aa", "bb", "cc"]
        mask = [.true., .false.]
        call col%append_values(vals, is_null=mask)   ! 3 values, 2 mask entries -> aborts
        print '(a,i0)', "unexpectedly appended with a short mask, size=", col%size()
    end subroutine scenario_string_append_values_mask_length

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
    !> comment in parquet_core.f90).
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

    !> A STRING_VIEW column reads into a compact parquet_string_column correctly -- values, nulls
    !> and lengths all intact.
    !>
    !> This USED to assert the opposite. The compact path hands Fortran an offsets/data/validity
    !> triple straight from the Arrow array (extract_string_buffers), and a view array has neither
    !> a single offsets array nor a single contiguous data buffer, so it was refused outright. It
    !> is now converted to arrow::large_utf8() first (recache_coerced_string_view), which is the
    !> same cast the row filter has always applied for its own Arrow-kernel gap -- so the refusal
    !> is gone and this scenario asserts the round trip instead of the error.
    !>
    !> Out of process only because the fixture needs parquet_debug_write_string_view_fixture, a
    !> test-only C++ hook (this library's own writer can never produce a STRING_VIEW column).
    !> Expected exit status is 0; assertions are by error stop, the usual convention for a
    !> non-aborting scenario.
    subroutine scenario_string_view_compact_read()
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
        character(len=:), allocatable :: text
        character(len=*), parameter :: out_file = "test_run/error_scenario_string_view_compact.parquet"

        call parquet_debug_write_string_view_fixture(out_file//char(0), "sv"//char(0))

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "sv", col)
        ! Read a second time: the cast REPLACES the cache entry, so this one finds a
        ! LARGE_STRING array already there and must give the identical answer.
        call parquet_read_column(reader, "sv", col)
        call parquet_close_reader(reader)

        ! The fixture is "short", "", Null, a 39-byte value, then "exactly12chr" -- deliberately
        ! spanning both view representations, since a value of 12 bytes or fewer is stored INLINE
        ! in the view struct while a longer one lives in a separate data buffer. A conversion that
        ! mishandled either kind would show up here as a wrong value rather than a crash.
        if (col%size() /= 5_int64) error stop "string_view compact read: wrong row count"
        call col%get(1, text)
        if (text /= "short") error stop "string_view compact read: row 1 wrong"
        call col%get(2, text)
        if (text /= "") error stop "string_view compact read: row 2 (empty string) wrong"
        ! The null must survive as a null, not as an empty string -- the two are different, and
        ! the validity bitmap is the buffer most likely to be misaligned by a bad conversion.
        if (.not. col%is_null(3)) error stop "string_view compact read: row 3 lost its null"
        if (col%is_null(2)) error stop "string_view compact read: an empty string became a null"
        call col%get(4, text)
        if (text /= "this value exceeds twelve bytes for sure") &
            error stop "string_view compact read: row 4 (non-inline value) wrong"
        call col%get(5, text)
        if (text /= "exactly12chr") error stop "string_view compact read: row 5 (inline boundary) wrong"
        if (col%null_count() /= 1_int64) error stop "string_view compact read: wrong null count"
    end subroutine scenario_string_view_compact_read

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

    !> parquet_get_column_time_info aborts for a column that is not a TIME/TIMESTAMP, which
    !> includes a `date` column -- the surprising case, since a date IS temporal and yet has no
    !> unit to report (resolve_temporal_value_type's default arm, parquet_wrapper.cpp).
    !>
    !> The query on "ev" first is the NEGATIVE CONTROL and is what makes the scenario mean
    !> anything: it goes through the identical entry point on the same reader and must return
    !> micros without aborting. Without it, an implementation whose time-info query aborted
    !> unconditionally would pass. Deliberately a `date` column rather than an int32 one --
    !> an int32 column would also pass against a guard that merely asked "is this temporal?".
    subroutine scenario_time_info_on_date_column()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_date) :: day(2)
        type(parquet_timestamp) :: ev(2)
        character(len=*), parameter :: out_file = "test_run/error_scenario_time_info_date.parquet"
        integer :: unit

        call schema%init(table="time_info_table")
        call schema%add_field("day", "date")
        call schema%add_field("ev", "timestamp[us]")

        call day(1)%set(2024, 7, 16)
        call day(2)%set(2024, 7, 17)
        call ev(1)%set(2024, 7, 16, 12, 0, 0)
        call ev(2)%set(2024, 7, 17, 12, 0, 0)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "day", day)
        call parquet_write_column(writer, "ev", ev)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        ! Negative control: the timestamp column answers normally.
        call parquet_get_column_time_info(reader, "ev", unit=unit)
        if (unit /= parquet_unit_micros) error stop "time_info control: expected unit=micros for 'ev'"
        ! The abort under test: a date column has no unit.
        call parquet_get_column_time_info(reader, "day", unit=unit)
        print '(a)', "unexpectedly queried a date column's time unit without error"
        call parquet_close_reader(reader)
    end subroutine scenario_time_info_on_date_column

    !> The qc: miss: half of the temporal qc rule: `min:`/`max:` bounds are rejected for a
    !> date/time/timestamp field, but `miss:` is accepted and active. This scenario declares
    !> qc_miss="Null" on a `date` column holding a null element and must therefore print NO
    !> WARNING; scenario_qc_miss_temporal_warns above is its negative control (same shape, no
    !> qc_miss declared, one WARNING). Asserting only this half would pass against a build that
    !> had stopped miss-checking temporal columns altogether.
    subroutine scenario_qc_miss_temporal_declared_null_no_warning()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_date) :: values(3)

        call values(1)%set(2024, 1, 1)
        ! values(2) left default-initialized -- a null date.
        call values(3)%set(2024, 1, 3)

        call schema%init(table="qc_miss_table")
        call schema%add_field("d", "date", qc_miss="Null")

        call parquet_open_writer(writer, "test_run/error_scenario_qc_miss_temporal_allowed.parquet", schema)
        call parquet_write_column(writer, "d", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_miss_temporal_declared_null_no_warning

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
                character(kind=c_char), intent(in) :: path(*) !! NUL-terminated output path.
                character(kind=c_char), intent(in) :: variant(*) !! NUL-terminated fixture variant.
            end subroutine parquet_debug_write_list_fixture
        end interface
        type(parquet_reader) :: reader
        integer :: col_size, large_col_size, strlen_lst, strlen_large_lst
        integer(int64) :: total_elem, large_total_elem
        character(len=*), parameter :: mismatch_file = "test_run/error_scenario_list_mismatch.parquet"
        character(len=*), parameter :: empty_file = "test_run/error_scenario_list_empty.parquet"
        character(len=*), parameter :: strings_file = "test_run/error_scenario_list_strings.parquet"

        ! "mismatch": row widths differ (0, 2, 3 elements) -- get_col_size's heterogeneous-width
        ! branch, both list kinds.
        call parquet_debug_write_list_fixture(mismatch_file//char(0), "mismatch"//char(0))
        call parquet_open_reader(reader, mismatch_file)
        call parquet_get_col_size(reader, "lst", col_size)
        call parquet_get_col_size(reader, "large_lst", large_col_size)
        ! parquet_get_col_size (above) is answered by the footer screen alone here, which already
        ! settles "mismatch" without ever calling get_col_size itself (a non-integral mean rejects
        ! it for free). parquet_get_column_total_elements has no such screen for a plain LIST/
        ! LARGE_LIST column -- it always reads the whole column and calls get_col_size directly, so
        ! this is what actually exercises get_col_size's own heterogeneous-row-width branch (as
        ! opposed to list_width_candidate's footer-only one) for both list kinds.
        call parquet_get_column_total_elements(reader, "lst", total_elem)
        call parquet_get_column_total_elements(reader, "large_lst", large_total_elem)
        call parquet_close_reader(reader)
        if (col_size /= 1) error stop "list fixture: mismatched-width LIST column should report col_size=1"
        if (large_col_size /= 1) error stop "list fixture: mismatched-width LARGE_LIST column should report col_size=1"
        if (total_elem /= 3) error stop "list fixture: mismatched-width LIST column should report 3 total elements"
        if (large_total_elem /= 3) &
            error stop "list fixture: mismatched-width LARGE_LIST column should report 3 total elements"

        ! "empty": both columns have zero rows -- get_col_size's whole-array-empty branch, both
        ! list kinds (distinct from an individual row's list being empty, already exercised above).
        call parquet_debug_write_list_fixture(empty_file//char(0), "empty"//char(0))
        call parquet_open_reader(reader, empty_file)
        call parquet_get_col_size(reader, "lst", col_size)
        call parquet_get_col_size(reader, "large_lst", large_col_size)
        ! Same reasoning as above: get_col_size's own whole-array-empty branch (as opposed to
        ! list_width_candidate's, already covered by the parquet_get_col_size calls) for both kinds.
        call parquet_get_column_total_elements(reader, "lst", total_elem)
        call parquet_get_column_total_elements(reader, "large_lst", large_total_elem)
        call parquet_close_reader(reader)
        if (col_size /= 0) error stop "list fixture: zero-row LIST column should report col_size=0"
        if (large_col_size /= 0) error stop "list fixture: zero-row LARGE_LIST column should report col_size=0"
        if (total_elem /= 0) error stop "list fixture: zero-row LIST column should report 0 total elements"
        if (large_total_elem /= 0) &
            error stop "list fixture: zero-row LARGE_LIST column should report 0 total elements"

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

    !> Neither size query materializes a whole PLAIN LIST/LARGE_LIST column.
    !>
    !> scenario_col_size_and_row_mode_avoid_whole_column_read makes the same guarantee, but builds
    !> its fixture with this library's own writer -- so every column there is a FIXED_SIZE_LIST,
    !> whose width is a schema constant, and the plain-LIST branch it never enters is precisely the
    !> one that has to read data at all. That is the gap this scenario closes.
    !>
    !> A plain variable-length list has no schema-level width (this library never writes one, but
    !> another producer may), so the width is a property of the DATA. Both queries answer it
    !> through list_width_verified: a footer screen (num_values / num_rows per row group) followed
    !> by a proof that reads one row group at a time, never the whole column.
    !> parquet_get_column_total_elements used to call get_single_chunk_array here instead and
    !> decode every row group at once -- the exact peak-memory hazard the guard exists to prevent,
    !> and silent, because the ANSWER was right either way. Only this hook can see the difference:
    !> reverting that fix makes the total_elements call below abort.
    !>
    !> The negative control is scenario_whole_column_read_forced_error_control, which already
    !> exists and proves the hook itself fires -- so this scenario's "no abort" cannot simply mean
    !> the hook is a no-op. The "strings" fixture variant is a uniform-width plain LIST<utf8> /
    !> LARGE_LIST<utf8>: 3 rows of 2 elements each, so col_size is 2 and total_elements is 6.
    !> parquet_get_string_length is deliberately NOT called here -- it always reads the whole
    !> column by design, so it would trip the hook and prove nothing.
    subroutine scenario_plain_list_size_queries_avoid_whole_column_read()
        interface
            subroutine parquet_debug_write_list_fixture(path, variant) &
                bind(C, name="parquet_debug_write_list_fixture")
                use iso_c_binding, only : c_char
                character(kind=c_char), intent(in) :: path(*) !! NUL-terminated output path.
                character(kind=c_char), intent(in) :: variant(*) !! NUL-terminated fixture variant.
            end subroutine parquet_debug_write_list_fixture
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero aborts the next whole-column read; 0 restores normal behavior.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_plain_list_size_queries.parquet"
        integer :: col_size, large_col_size
        integer(int64) :: total_elem, large_total_elem

        call parquet_debug_write_list_fixture(out_file//char(0), "strings"//char(0))
        call parquet_open_reader(reader, out_file)

        ! Armed BEFORE the four queries, so their completing (rather than the forced abort) is
        ! the whole assertion.
        call parquet_debug_set_force_whole_column_read_error(1)

        call parquet_get_col_size(reader, "lst_str", col_size)
        if (col_size /= 2) error stop "plain LIST col_size expected 2"
        call parquet_get_col_size(reader, "large_lst_str", large_col_size)
        if (large_col_size /= 2) error stop "plain LARGE_LIST col_size expected 2"

        call parquet_get_column_total_elements(reader, "lst_str", total_elem)
        if (total_elem /= 6_int64) error stop "plain LIST total_elements expected 6"
        call parquet_get_column_total_elements(reader, "large_lst_str", large_total_elem)
        if (large_total_elem /= 6_int64) error stop "plain LARGE_LIST total_elements expected 6"

        call parquet_debug_set_force_whole_column_read_error(0)
        call parquet_close_reader(reader)
        print '(a)', "parquet_get_col_size/parquet_get_column_total_elements avoided a " // &
            "whole-column read on a plain LIST column, as expected"
    end subroutine scenario_plain_list_size_queries_avoid_whole_column_read

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

    !> parquet_get_version(mode=...) rejects any value other than "internal".
    subroutine scenario_get_version_invalid_mode()
        character(len=:), allocatable :: ver_string

        ! Negative control first: the one accepted mode must still work, so that a guard which
        ! fired unconditionally could not pass this scenario.
        call parquet_get_version(ver_string, mode="internal")
        if (len_trim(ver_string) == 0) then
            print '(a)', "mode='internal' returned an empty string"
            return
        end if
        call parquet_get_version(ver_string, mode="bogus")
        print '(a)', "unexpectedly returned a version string for an invalid mode"
    end subroutine scenario_get_version_invalid_mode

    !> parquet_get_version no longer answers mode="arrow"/"parquet" -- those moved to
    !> parquet_get_arrow_version when the library version moved into the Arrow-free leaf module
    !> parquet_version. The abort names the replacement, so a caller migrating from the old
    !> spelling is told where to go rather than merely told "invalid".
    subroutine scenario_get_version_arrow_mode_removed()
        character(len=:), allocatable :: ver_string

        call parquet_get_version(ver_string, mode="arrow")
        print '(a)', "unexpectedly returned a version string for the removed mode='arrow'"
    end subroutine scenario_get_version_arrow_mode_removed

    !> parquet_get_arrow_version(mode=...) rejects any value other than "arrow"/"parquet".
    subroutine scenario_get_arrow_version_invalid_mode()
        character(len=:), allocatable :: ver_string

        ! Negative control: both accepted modes answer before the invalid one is tried.
        call parquet_get_arrow_version(ver_string)
        call parquet_get_arrow_version(ver_string, mode="parquet")
        if (len_trim(ver_string) == 0) then
            print '(a)', "mode='parquet' returned an empty string"
            return
        end if
        call parquet_get_arrow_version(ver_string, mode="internal")
        print '(a)', "unexpectedly returned a version string for an invalid mode"
    end subroutine scenario_get_arrow_version_invalid_mode

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

    !> The ORDER of parquet_column_exists's two checks: the types= tokens are validated before the
    !> column is looked up, so a typo is reported even for a column that does not exist.
    !>
    !> This is what makes the claim testable at all. Its sibling above names `id_with_null`, a
    !> column that IS in the fixture, so it aborts with the same message whichever check runs
    !> first -- it proves the token is rejected, and says nothing about the ordering that
    !> doc/pages/io/reading.md documents. Here the column is absent, so an implementation that
    !> checked existence first would return .false. quietly (parquet_column_exists reports a
    !> missing column rather than aborting on it) and this scenario would print its "unexpectedly"
    !> line and exit 0 instead of aborting. The pair is the test; neither half is alone.
    !>
    !> See parquet_read.f90's parquet_column_exists: the `do i = 1, size(tokens)` validation loop
    !> precedes the parquet_reader_has_column call.
    subroutine scenario_column_exists_bad_type_token_missing_column()
        type(parquet_reader) :: reader
        logical :: exists

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        exists = parquet_column_exists(reader, "no_such_column_at_all", types="itn32")
        print '(a)', "unexpectedly checked column existence before validating the types= tokens"
    end subroutine scenario_column_exists_bad_type_token_missing_column

    !> parquet_column_exists error stops if types= is given but blank/all-whitespace, rather than
    !> silently matching nothing.
    subroutine scenario_column_exists_empty_type_filter()
        type(parquet_reader) :: reader
        logical :: exists

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        exists = parquet_column_exists(reader, "id_with_null", types="   ")
        print '(a)', "unexpectedly accepted a blank types= filter without error"
    end subroutine scenario_column_exists_empty_type_filter


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

        call schema%set_col_size("v", 0)
        print '(a)', "unexpectedly accepted a non-positive col_size"
    end subroutine scenario_set_col_size_non_positive

    !> schema%set_col_size refuses to override a column whose col_size is already concretely
    !> resolved (not "auto") unless force=.true. is passed.
    subroutine scenario_set_col_size_already_resolved_no_force()
        type(parquet_schema) :: schema

        call schema%init(table="set_col_size_already_resolved_table")
        call schema%add_field("v", "int32", col_size=3)

        call schema%set_col_size("v", 5)
        print '(a)', "unexpectedly overrode an already-resolved col_size without force=.true."
    end subroutine scenario_set_col_size_already_resolved_no_force

    !> schema%set_array_size only applies to string columns.
    subroutine scenario_set_array_size_non_string_column()
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_non_string_table")
        call schema%add_field("v", "int32")

        call schema%set_array_size("v", 10)
        print '(a)', "unexpectedly accepted set_array_size on a non-string column"
    end subroutine scenario_set_array_size_non_string_column

    !> schema%set_array_size rejects a non-positive array_size outright, regardless of whether
    !> the target column is currently "auto" (mirrors scenario_set_col_size_non_positive).
    subroutine scenario_set_array_size_non_positive()
        type(parquet_schema) :: schema

        call schema%init(table="set_array_size_non_positive_table")
        call schema%add_field("txt", "string")

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

    !> A MAML key is case-insensitive, and the block headers have to obey that as much as the
    !! scalar keys do. They did not: `parquet_find_maml_section` lowercased both sides while every
    !! block LOCATOR compared against a lowercase literal, so a MAML spelling its section `Extra:`
    !! validated cleanly with its nested `protected_cols:`/`col_map:`/`remap:`/`filter:`/`sort:`
    !! never found at all -- no warning, and a file whose Null protection had quietly vanished.
    !! See feature_risks.md Risk-91.
    !!
    !! Both spellings run the SAME body, because "the block was found" is only observable through
    !! something the block does: here a `protected_cols:` naming a column that does not exist, which
    !! validation rejects only if it read the block in the first place. The two scenarios must
    !! therefore behave identically -- and a test asserting only the capitalized one would pass
    !! against a library that had stopped reading `extra:` altogether, which is why the lowercase
    !! twin is registered as its own scenario rather than being assumed.
    subroutine scenario_extra_section_capitalized(capitalized)
        logical, intent(in) :: capitalized !! .true. spells the section `Extra:`, .false. `extra:`.
        type(parquet_maml_file) :: maml

        maml%name = "extra_section_case.maml"
        maml%lines = [character(len=40) :: &
            "table: extra_section_case_table", &
            "extra:", &
            "  protected_cols: nosuchcolumn", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]
        if (capitalized) maml%lines(2) = "Extra:"

        call parquet_validate_maml(maml)
        print '(a)', "unexpectedly accepted a protected_cols: naming a column that is not declared"
    end subroutine scenario_extra_section_capitalized

    !> A parquet_string_column write does not enforce a declared array_size -- it stores each
    !! element's own bytes, and a reader takes each length from the data -- so writing longer
    !! elements than the schema declares is accepted. It is not silent, though: one WARNING names
    !! the column, the declaration and the actual length. This scenario is the out-of-process half,
    !! because a warning goes to the message channel and only a captured run can read it.
    !!
    !! Three things are asserted from one run (see test_errors.f90):
    !!   * the write is ACCEPTED -- the scenario exits 0, and a stray abort would fail the scenario
    !!     rather than the assertion;
    !!   * the warning names the offending column and its 20-character element;
    !!   * it is emitted ONCE per column, not once per chunk. Chunk 3 carries a 25-character element
    !!     -- longer again -- so a per-chunk warning would put "25" in the output. The test asserts
    !!     that it is absent, which is a precise once-only check using only "does this text appear".
    !!
    !! The second column is the negative control: it stays within its declaration and must draw no
    !! warning at all, so a scenario that warned about every string column would fail.
    subroutine scenario_compact_write_exceeds_array_size_warns()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_string_column) :: over1, over2, over3, fits1, fits2, fits3

        call over1%append_string("abc")                          ! within the declared 5
        call over2%append_string("a_twenty_char_value!")         ! 20 -- the one warning
        call over3%append_string("a_twenty_five_char_value!")    ! 25 -- must NOT warn again
        call fits1%append_string("ab")
        call fits2%append_string("cd")
        call fits3%append_string("ef")

        schema%maml%name = "compact_exceeds_array_size.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: compact_exceeds_array_size_table", &
            "fields:", &
            "- name: over", &
            "  data_type: string", &
            "  array_size: 5", &
            "- name: within", &
            "  data_type: string", &
            "  array_size: 8" ]
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_compact_exceeds.parquet", schema, &
            write_maml=.true.)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "over", over1)
        call parquet_write_column_chunk(writer, "within", fits1)
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "over", over2)
        call parquet_write_column_chunk(writer, "within", fits2)
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 1_int64)
        call parquet_write_column_chunk(writer, "over", over3)
        call parquet_write_column_chunk(writer, "within", fits3)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)

        print '(a)', "compact-exceeds-write-completed"
    end subroutine scenario_compact_write_exceeds_array_size_warns

    !> parquet_column%data_ptr is EXACT-kind by design (DD2): it aliases raw storage, so a
    !! pointer of a different kind would reinterpret the bytes rather than convert them. Asking
    !! an int32 column for an int64 pointer must abort, not widen.
    subroutine scenario_columns_data_ptr_kind_mismatch()
        type(parquet_column), target :: col
        integer(int64), pointer :: p(:)
        call col%init(PK_INT32, 3_int64)
        call col%set_all([1_int32, 2_int32, 3_int32])
        call col%data_ptr(p)   ! int64 pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through an int64 pointer, size=", size(p)
    end subroutine scenario_columns_data_ptr_kind_mismatch

    !> A default-initialized parquet_column has no kind and no storage; a structural mutation on
    !! it must say so rather than silently doing nothing.
    subroutine scenario_columns_uninitialized_append_nulls()
        type(parquet_column) :: col
        call col%append_nulls(2_int64)   ! PK_NONE -> aborts
        print '(a,i0)', "unexpectedly appended null rows to a column with no kind, length=", col%length()
    end subroutine scenario_columns_uninitialized_append_nulls

    !> Row indices are checked against the column's own length on every element access.
    subroutine scenario_columns_get_at_index_out_of_range()
        type(parquet_column) :: col
        real(real64) :: v
        call col%init(PK_FLOAT64, 3_int64)
        call col%set_all([1.0_real64, 2.0_real64, 3.0_real64])
        call col%get_at(99_int64, v)   ! index 99 > nrows 3 -> aborts
        print '(a,f0.1)', "unexpectedly read an out-of-range row: ", v
    end subroutine scenario_columns_get_at_index_out_of_range

    !> The ELEMENT axis has its own guard and its own message, deliberately different from the row
    !! one: the commonest way to get this wrong is to pass a FLAT element position where a row and
    !! an element index were wanted, and on a width-2 column that flat position is a perfectly
    !! valid row index, so a shared message would send the reader looking at the wrong axis.
    !!
    !! Element 3 of a width-2 column is asked for AFTER a valid `(2, 2)` read, which is the
    !! negative control -- a guard that rejected every element index would abort on that instead
    !! and the scenario would fail with the wrong output.
    subroutine scenario_columns_element_index_out_of_range()
        type(parquet_column) :: col
        integer(int32) :: v
        call col%init(PK_INT32_VEC, 3_int64, 2_int32)
        call col%set_at(2_int64, [10_int32, 20_int32])
        call col%get_elem(2_int64, 2_int64, v)          ! in range: must NOT abort
        print '(a,i0)', "read the last element of row 2: ", v
        call col%get_elem(2_int64, 3_int64, v)          ! element 3 of a width-2 column -> aborts
        print '(a,i0)', "unexpectedly read an out-of-range element: ", v
    end subroutine scenario_columns_element_index_out_of_range

    !> A rank-2 validity mask is per ELEMENT and must be shaped exactly (width, nrows). Accepting a
    !! mismatched one would read the mask off its own edge, or -- worse, and the reason the check is
    !! on both extents rather than on the total size -- silently transpose a square mask.
    !!
    !! The correctly-shaped call first is the negative control.
    subroutine scenario_columns_set_validity_elem_shape_mismatch()
        type(parquet_column) :: col
        logical :: ok_mask(2, 3), bad_mask(3, 2)
        call col%init(PK_INT32_VEC, 3_int64, 2_int32)
        ok_mask = .true.
        ok_mask(1, 2) = .false.
        call col%set_validity(ok_mask)                  ! correctly shaped: must NOT abort
        print '(a,l1)', "the correctly shaped mask was accepted, row 2 null: ", col%is_null(2_int64)
        bad_mask = .true.
        bad_mask(1, 1) = .false.
        call col%set_validity(bad_mask)                 ! (3, 2) on a (2, 3) column -> aborts
        print '(a)', "unexpectedly accepted a transposed element mask"
    end subroutine scenario_columns_set_validity_elem_shape_mismatch

    !> A rank-1 validity mask is per ROW and must have exactly nrows entries. The message names both
    !! counts, because the usual cause is a mask built for a different column and the two numbers
    !! are what identify which.
    !!
    !! The correctly-sized call first is the negative control.
    subroutine scenario_columns_set_validity_row_count_mismatch()
        type(parquet_column) :: col
        logical :: ok_mask(3), bad_mask(5)
        call col%init(PK_INT32, 3_int64)
        call col%set_all([1_int32, 2_int32, 3_int32])
        ok_mask = [.true., .false., .true.]
        call col%set_validity(ok_mask)                  ! correctly sized: must NOT abort
        print '(a,l1)', "the correctly sized mask was accepted, row 2 null: ", col%is_null(2_int64)
        bad_mask = .true.
        call col%set_validity(bad_mask)                 ! 5 entries for 3 rows -> aborts
        print '(a)', "unexpectedly accepted a row mask of the wrong length"
    end subroutine scenario_columns_set_validity_row_count_mismatch

    !> A temporal element carries its own null state, so there is no bitmap bit to clear and no way
    !! to make it valid without giving it a value -- `%clear_null` would have to invent one. It
    !! therefore refuses rather than silently leaving the element null while reporting it valid,
    !! which is what a no-op implementation would do.
    !!
    !! The same call on a bitmap kind first is the negative control: clear_null is a perfectly
    !! ordinary operation there, so a refusal that fired for every kind could not pass this.
    subroutine scenario_columns_clear_null_elem_temporal()
        type(parquet_column) :: num, col
        type(parquet_date) :: d(2)
        integer :: i
        call num%init(PK_INT32, 2_int64)
        call num%set_all([1_int32, 2_int32])
        call num%set_null(2_int64, 1_int64)
        call num%clear_null(2_int64, 1_int64)           ! a bitmap kind: must NOT abort
        print '(a,l1)', "cleared a bitmap column's element null, still null: ", num%is_null(2_int64, 1_int64)
        do i = 1, 2
            call d(i)%set(2026, 8, 10 + i)
        end do
        call col%init(PK_DATE, 2_int64)
        call col%set_all(d)
        call col%set_null(2_int64, 1_int64)
        call col%clear_null(2_int64, 1_int64)           ! a temporal element -> aborts
        print '(a)', "unexpectedly cleared a temporal element's null without writing a value"
    end subroutine scenario_columns_clear_null_elem_temporal

    !> append requires identical kinds: silently widening an int32 source into a float64 target
    !! would change the target column's storage kind, which section E of feature_table.md rules
    !! out (use %copy_column instead).
    subroutine scenario_columns_append_kind_mismatch()
        type(parquet_column) :: a, b
        call a%init(PK_FLOAT64, 2_int64)
        call a%set_all([1.0_real64, 2.0_real64])
        call b%init(PK_INT32, 2_int64)
        call b%set_all([3_int32, 4_int32])
        call a%append(b)   ! int32 into float64 -> aborts
        print '(a,i0)', "unexpectedly appended a column of a different kind, length=", a%length()
    end subroutine scenario_columns_append_kind_mismatch

    !> Two vector columns of the same kind but different widths describe different row shapes;
    !! concatenating them would produce rows of two different lengths in one column.
    subroutine scenario_columns_append_width_mismatch()
        type(parquet_column) :: a, b
        call a%init(PK_FLOAT64_VEC, 1_int64, width=3_int32)
        call b%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call a%append(b)   ! width 2 into width 3 -> aborts
        print '(a,i0)', "unexpectedly appended a vector column of a different width, length=", a%length()
    end subroutine scenario_columns_append_width_mismatch

    !> `%append_row_of` re-checks kind and width on every call, even though its only in-library
    !! caller (the table's row append) has already validated both.
    !!
    !! It is a public binding -- Fortran offers no narrower visibility for something
    !! `parquet_tables` has to reach -- so a caller who validated nothing can get here, and two
    !! integer comparisons against a call that copies a whole row is the wrong place to save time.
    !! Without this scenario, deleting the check is invisible: the table path validates first, so
    !! every other test still passes.
    subroutine scenario_columns_append_row_of_width_mismatch()
        type(parquet_column) :: a, b
        call a%init(PK_FLOAT64_VEC, 1_int64, width=3_int32)
        call b%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call a%append_row_of(b, 1_int64)   ! width 2 row into a width 3 column -> aborts
        print '(a,i0)', "unexpectedly appended a row of a different width, length=", a%length()
    end subroutine scenario_columns_append_row_of_width_mismatch

    !> `append_row_of` names a row of the SOURCE, and that index is the one argument neither the
    !! kind check nor the width check can vet -- both compare the two columns' metadata and say
    !! nothing about whether row `irow` exists in `b`.
    !!
    !! Without the guard the read runs off the end of the source's storage: a plain `fpm test` has
    !! no bounds checking, so it would copy whatever follows the array and the appended row would
    !! hold garbage that still looks like a valid value of that kind. The negative control comes
    !! first -- the in-range append must succeed, or a guard that rejected every index would pass
    !! this scenario while breaking the operation.
    subroutine scenario_columns_append_row_of_row_out_of_range()
        type(parquet_column) :: a, b
        call b%init(PK_FLOAT64, 2_int64)
        call b%set_all([1.0_real64, 2.0_real64])
        call a%init(PK_FLOAT64, 0_int64)
        call a%append_row_of(b, 2_int64)   ! in range: must be accepted
        call a%append_row_of(b, 3_int64)   ! one past the source's last row -> aborts
        print '(a,i0)', "unexpectedly appended a row past the source's end, length=", a%length()
    end subroutine scenario_columns_append_row_of_row_out_of_range

    !> A row append validates EVERY column before writing ANY of them.
    !!
    !! The abort itself is not the point -- the point is WHICH abort. `a` matches, `b` does not,
    !! and `b` is checked only after `a` would already have been appended if the worker validated
    !! as it went. So the message proves the ordering: the validation message names the incompatible
    !! column, while a worker that mutated first would get as far as appending `a` and then abort
    !! from `parquet_columns` instead, leaving one column one row longer than the other -- a state
    !! with no diagnostic and no way back.
    subroutine scenario_table_append_row_validates_first()
        type(parquet_table) :: dst, src
        type(parquet_table_row) :: r
        call parquet_new_table(dst)
        call dst%add_column("a", [1_int32, 2_int32])
        call dst%add_column("b", [1.0_real64, 2.0_real64])
        call parquet_new_table(src)
        call src%add_column("a", [7_int32])
        call src%add_column("b", [9_int32])   ! int32 where dst holds float64
        r = src%row(1)
        call dst%append(r)   ! 'b' is incompatible -> aborts BEFORE 'a' is appended
        print '(a,i0)', "unexpectedly appended an incompatible row, nrows=", dst%nrows()
    end subroutine scenario_table_append_row_validates_first

    !> paste writes into rows that already exist, so -- unlike append -- a mismatched kind cannot
    !! be absorbed by growing the destination. It is the same check append makes, at the same
    !! point, for the same reason: the storage arrays being copied between are of different types.
    subroutine scenario_columns_paste_kind_mismatch()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 2_int64)
        call b%init(PK_FLOAT64, 2_int64)
        call a%paste(b, 1_int64)   ! float64 into int32 -> aborts
        print '(a,i0)', "unexpectedly pasted a column of a different kind, length=", a%length()
    end subroutine scenario_columns_paste_kind_mismatch

    !> Same kind, different vector width: the rows are not the same shape, so pasting one over
    !! the other would write the wrong number of elements per row.
    subroutine scenario_columns_paste_width_mismatch()
        type(parquet_column) :: a, b
        call a%init(PK_FLOAT64_VEC, 2_int64, width=3_int32)
        call b%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call a%paste(b, 1_int64)   ! width 2 into width 3 -> aborts
        print '(a,i0)', "unexpectedly pasted a vector column of a different width, length=", a%length()
    end subroutine scenario_columns_paste_width_mismatch

    !> A string column's values live in one packed variable-length store, so there is no fixed
    !! slot for row i to be overwritten in place -- the whole point paste relies on. Rather than
    !! silently doing something else, it says so and names append as the operation that works.
    subroutine scenario_columns_paste_string_kind()
        type(parquet_column) :: a, b
        call a%init(PK_STRING, 2_int64)
        call a%set_all(["x", "y"])
        call b%init(PK_STRING, 1_int64)
        call b%set_all(["z"])
        call a%paste(b, 1_int64)   ! string kind -> aborts
        print '(a,i0)', "unexpectedly pasted into a string column, length=", a%length()
    end subroutine scenario_columns_paste_string_kind

    !> from= is 1-based like every other row index in this library, so 0 is not "the beginning".
    subroutine scenario_columns_paste_source_index_below_one()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 2_int64)
        call b%init(PK_INT32, 2_int64)
        call a%paste(b, 1_int64, 0_int64)   ! from=0 -> aborts
        print '(a,i0)', "unexpectedly pasted from source row 0, length=", a%length()
    end subroutine scenario_columns_paste_source_index_below_one

    !> count=0 is a legitimate no-op, but a negative count is a caller-side arithmetic mistake
    !! (a subtraction that came out backwards), and silently treating it as 0 would hide it.
    subroutine scenario_columns_paste_negative_count()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 2_int64)
        call b%init(PK_INT32, 2_int64)
        call a%paste(b, 1_int64, 1_int64, -1_int64)   ! count < 0 -> aborts
        print '(a,i0)', "unexpectedly pasted a negative row count, length=", a%length()
    end subroutine scenario_columns_paste_negative_count

    !> Reading past the end of the SOURCE: from+count-1 exceeds what the source actually holds.
    subroutine scenario_columns_paste_source_past_end()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 4_int64)
        call b%init(PK_INT32, 2_int64)
        call a%paste(b, 1_int64, 2_int64, 2_int64)   ! source rows 2..3 of a 2-row column -> aborts
        print '(a,i0)', "unexpectedly pasted past the end of the source, length=", a%length()
    end subroutine scenario_columns_paste_source_past_end

    !> at= is 1-based too, and paste never grows the destination, so there is no row 0 to write.
    subroutine scenario_columns_paste_destination_index_below_one()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 2_int64)
        call b%init(PK_INT32, 1_int64)
        call a%paste(b, 0_int64)   ! at=0 -> aborts
        print '(a,i0)', "unexpectedly pasted at destination row 0, length=", a%length()
    end subroutine scenario_columns_paste_destination_index_below_one

    !> The one that matters most: paste does NOT grow the destination, so a range running off the
    !! end is a caller error rather than an append. Silently growing instead would turn a
    !! miscomputed cursor into a longer column that still looks plausible.
    subroutine scenario_columns_paste_destination_past_end()
        type(parquet_column) :: a, b
        call a%init(PK_INT32, 3_int64)
        call b%init(PK_INT32, 2_int64)
        call a%paste(b, 3_int64)   ! rows 3..4 of a 3-row column -> aborts
        print '(a,i0)', "unexpectedly pasted past the end of the destination, length=", a%length()
    end subroutine scenario_columns_paste_destination_past_end

    !> reindex validates its permutation IN FULL before touching any storage, so a bad
    !! permutation aborts with the column still intact rather than half rebuilt. A duplicated
    !! index is the interesting case: every entry is in range, yet the result would silently
    !! drop a row and duplicate another.
    subroutine scenario_columns_reindex_duplicate_index()
        type(parquet_column) :: col
        call col%init(PK_INT32, 3_int64)
        call col%set_all([1_int32, 2_int32, 3_int32])
        call col%reindex([1_int64, 2_int64, 2_int64])   ! 3 is missing, 2 appears twice -> aborts
        print '(a,i0)', "unexpectedly reindexed with a duplicated index, length=", col%length()
    end subroutine scenario_columns_reindex_duplicate_index

    !> A temporal element becomes valid by having a value written to it -- there is no separate
    !! "mark valid" state to flip, because the null flag lives inside the element. clear_null on
    !! such a column therefore says so rather than silently doing nothing.
    subroutine scenario_columns_clear_null_temporal()
        type(parquet_column) :: col
        call col%init(PK_DATE, 2_int64)
        call col%clear_null(1_int64)   ! temporal kind -> aborts
        print '(a,l1)', "unexpectedly cleared a temporal element's null state, is_null=", col%is_null(1_int64)
    end subroutine scenario_columns_clear_null_temporal

    !> The container kinds are declared so the type's layout is final, but their internals are
    !! deferred to feature_map_list_struct.md -- asking for one now must say so plainly rather
    !! than produce a column with no storage.
    subroutine scenario_columns_init_container_kind()
        type(parquet_column) :: col
        call col%init(PK_LIST, 2_int64)   ! reserved kind -> aborts
        print '(a,i0)', "unexpectedly initialized a reserved container kind, length=", col%length()
    end subroutine scenario_columns_init_container_kind

    !> width > 1 only means something for a *_VEC kind; silently ignoring it on a scalar kind
    !! would give the caller a column shaped differently from the one they asked for.
    subroutine scenario_columns_init_width_on_scalar_kind()
        type(parquet_column) :: col
        call col%init(PK_INT32, 2_int64, width=4_int32)   ! scalar kind + width -> aborts
        print '(a,i0)', "unexpectedly gave a scalar column a width of ", col%colwidth()
    end subroutine scenario_columns_init_width_on_scalar_kind

    !> %adopt takes over an allocatable array via move_alloc, so an unallocated array has no
    !! storage to take over -- every one of the 16 adopt_* specifics guards this with its own
    !! "not allocated" check on its own source line, so each needs its own abort to cover it.
    !! %adopt is reached through the public `adopt` generic even though every specific behind it
    !! is a private binding (parquet_columns.f90's "generic :: adopt => ..." has no explicit
    !! accessibility, so it defaults to public regardless of its specifics).
    subroutine scenario_columns_adopt_not_allocated_i32()
        type(parquet_column) :: col
        integer(int32), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated int32 array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_i32

    subroutine scenario_columns_adopt_not_allocated_i64()
        type(parquet_column) :: col
        integer(int64), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated int64 array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_i64

    subroutine scenario_columns_adopt_not_allocated_f32()
        type(parquet_column) :: col
        real(real32), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated float32 array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_f32

    subroutine scenario_columns_adopt_not_allocated_f64()
        type(parquet_column) :: col
        real(real64), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated float64 array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_f64

    subroutine scenario_columns_adopt_not_allocated_bool()
        type(parquet_column) :: col
        logical, allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated logical array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_bool

    subroutine scenario_columns_adopt_not_allocated_date()
        type(parquet_column) :: col
        type(parquet_date), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated date array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_date

    subroutine scenario_columns_adopt_not_allocated_time()
        type(parquet_column) :: col
        type(parquet_time), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated time array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_time

    subroutine scenario_columns_adopt_not_allocated_ts()
        type(parquet_column) :: col
        type(parquet_timestamp), allocatable :: v(:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated timestamp array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_ts

    subroutine scenario_columns_adopt_not_allocated_i32v()
        type(parquet_column) :: col
        integer(int32), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated int32_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_i32v

    subroutine scenario_columns_adopt_not_allocated_i64v()
        type(parquet_column) :: col
        integer(int64), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated int64_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_i64v

    subroutine scenario_columns_adopt_not_allocated_f32v()
        type(parquet_column) :: col
        real(real32), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated float32_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_f32v

    subroutine scenario_columns_adopt_not_allocated_f64v()
        type(parquet_column) :: col
        real(real64), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated float64_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_f64v

    subroutine scenario_columns_adopt_not_allocated_boolv()
        type(parquet_column) :: col
        logical, allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated logical_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_boolv

    subroutine scenario_columns_adopt_not_allocated_datev()
        type(parquet_column) :: col
        type(parquet_date), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated date_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_datev

    subroutine scenario_columns_adopt_not_allocated_timev()
        type(parquet_column) :: col
        type(parquet_time), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated time_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_timev

    subroutine scenario_columns_adopt_not_allocated_tsv()
        type(parquet_column) :: col
        type(parquet_timestamp), allocatable :: v(:,:)
        call col%adopt(v)   ! v never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated timestamp_vec array, length=", col%length()
    end subroutine scenario_columns_adopt_not_allocated_tsv

    !> A whole-column set must supply exactly one value per row: a shorter array would leave
    !! part of the column silently stale, a longer one would drop values.
    subroutine scenario_columns_set_all_length_mismatch()
        type(parquet_column) :: col
        call col%init(PK_INT32, 3_int64)
        call col%set_all([1_int32, 2_int32])   ! 2 values for 3 rows -> aborts
        print '(a,i0)', "unexpectedly set a column from a mismatched value count, length=", col%length()
    end subroutine scenario_columns_set_all_length_mismatch

    !> A vector row access must match the column's width exactly.
    subroutine scenario_columns_get_at_width_mismatch()
        type(parquet_column) :: col
        real(real64) :: row(2)
        call col%init(PK_FLOAT64_VEC, 2_int64, width=3_int32)
        call col%get_at(1_int64, row)   ! 2-element buffer for a width-3 column -> aborts
        print '(a,f0.1)', "unexpectedly read a width-3 row into a 2-element buffer, first=", row(1)
    end subroutine scenario_columns_get_at_width_mismatch

    !> delete_by_mask needs one mask entry per row; a short mask would silently keep the tail.
    subroutine scenario_columns_delete_by_mask_length_mismatch()
        type(parquet_column) :: col
        call col%init(PK_INT32, 3_int64)
        call col%delete_by_mask([.true., .false.])   ! 2 entries for 3 rows -> aborts
        print '(a,i0)', "unexpectedly filtered a column with a short mask, length=", col%length()
    end subroutine scenario_columns_delete_by_mask_length_mismatch

    !> reindex needs a full permutation; a short one cannot describe where every row goes.
    subroutine scenario_columns_reindex_length_mismatch()
        type(parquet_column) :: col
        call col%init(PK_INT32, 3_int64)
        call col%reindex([2_int64, 1_int64])   ! 2 entries for 3 rows -> aborts
        print '(a,i0)', "unexpectedly reindexed a column with a short permutation, length=", col%length()
    end subroutine scenario_columns_reindex_length_mismatch

    !> string_column hands back the embedded parquet_string_column, which only the string kinds
    !! have -- asking a numeric column for one must say so rather than return a null pointer the
    !! caller would then dereference.
    subroutine scenario_columns_string_column_wrong_kind()
        type(parquet_column), target :: col
        type(parquet_string_column), pointer :: sp
        call col%init(PK_FLOAT64, 2_int64)
        call col%string_column(sp)   ! not a string kind -> aborts
        print '(a,i0)', "unexpectedly obtained a string store from a float64 column, size=", sp%size()
    end subroutine scenario_columns_string_column_wrong_kind

    !> parquet_string_column%reindex validates the permutation before touching any buffer, so a
    !! wrong-length permutation aborts with the column intact.
    subroutine scenario_string_column_reindex_length_mismatch()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%reindex([1_int64])   ! 1 entry for 2 elements -> aborts
        print '(a,i0)', "unexpectedly reindexed with a short permutation, size=", col%size()
    end subroutine scenario_string_column_reindex_length_mismatch

    !> An in-range check on every permutation entry, for the same reason.
    subroutine scenario_string_column_reindex_out_of_range()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%reindex([1_int64, 9_int64])   ! entry 9 > size 2 -> aborts
        print '(a,i0)', "unexpectedly reindexed with an out-of-range entry, size=", col%size()
    end subroutine scenario_string_column_reindex_out_of_range

    !> delete_by_mask needs exactly one mask entry per element.
    subroutine scenario_string_column_delete_by_mask_length_mismatch()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%delete_by_mask([.true.])   ! 1 entry for 2 elements -> aborts
        print '(a,i0)', "unexpectedly filtered with a short mask, size=", col%size()
    end subroutine scenario_string_column_delete_by_mask_length_mismatch

    !> A negative bulk-null count is a caller bug, not an empty append.
    subroutine scenario_string_column_append_nulls_negative()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_nulls(-3_int64)   ! negative count -> aborts
        print '(a,i0)', "unexpectedly appended a negative number of nulls, size=", col%size()
    end subroutine scenario_string_column_append_nulls_negative

    !> Writes the small fixture the parquet_table scenarios below open.
    subroutine write_table_scenario_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: ids(3)
        real(real64) :: vals(3)
        ids = [1_int32, 2_int32, 3_int32]
        vals = [1.5_real64, 2.5_real64, 3.5_real64]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", ids)
        call parquet_write_column(w, "val", vals)
        call parquet_close_writer(w)
    end subroutine write_table_scenario_fixture

    !> The column store lives behind a pointer, so intrinsic assignment would leave two tables
    !! sharing (and later double-freeing) one store. It must abort, not silently alias.
    subroutine scenario_table_assignment_blocked()
        type(parquet_table) :: a, b
        call write_table_scenario_fixture("test_run/es_table_assign.parquet")
        call parquet_open_table(a, "test_run/es_table_assign.parquet")
        b = a   ! -> aborts
        print '(a,i0)', "unexpectedly copied a table by assignment, ncols=", b%ncols()
    end subroutine scenario_table_assignment_blocked

    !> Every accessor guards a never-opened table rather than dereferencing a null store.
    subroutine scenario_table_not_opened()
        type(parquet_table) :: t
        integer(int64) :: n
        n = t%nrows()   ! never opened -> aborts
        print '(a,i0)', "unexpectedly read a row count from an unopened table, n=", n
    end subroutine scenario_table_not_opened
    !
    !> A by-position query with no `found=` must abort rather than answer for some other column.
    !! The negative control matters here: the same call one position lower must succeed first, or
    !! a validator that refused everything would pass this scenario.
    subroutine scenario_table_column_position_out_of_range()
        type(parquet_table) :: t
        real(real64) :: d(2)
        integer :: k
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        k = t%kind(1)   ! negative control: the one valid position must answer
        print '(a,i0)', "in-range position answered, kind=", k
        k = t%kind(2)   ! one past the end, no found= -> aborts
        print '(a,i0)', "unexpectedly read a kind past the last column, kind=", k
    end subroutine scenario_table_column_position_out_of_range
    !
    !> %get_element with a row index past the last row. The negative control is the in-range read
    !! on the line before: a bounds check that refused everything would pass this without it.
    subroutine scenario_table_get_element_row_out_of_range()
        type(parquet_table) :: t
        real(real64) :: d(2), v
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        call t%get_element("a", 2_int64, v)   ! negative control: the last valid row
        print '(a,f8.3)', "in-range get_element answered, v=", v
        call t%get_element("a", 3_int64, v)   ! one past the end -> aborts
        print '(a,f8.3)', "unexpectedly read a row past the last, v=", v
    end subroutine scenario_table_get_element_row_out_of_range
    !
    !> %get_element asking for a kind the column cannot serve. This exercises the SHARED body
    !! `col_fetch_f64`, which both the table's %get_element and a column handle's %get call --
    !! so this one scenario covers the kind-error path of both entry points.

    !> `%get_element` into a VECTOR receiver whose kind is not the column's. Each vector kind has
    !! its own shared fetch body carrying its own kind check, and the scalar mismatch scenario
    !! above reaches none of them -- the eight below are one per body.
    !!
    !! Each makes a matching-kind call FIRST, as the negative control: a check that fired
    !! unconditionally would pass the abort half while breaking every ordinary vector read.
    subroutine scenario_table_get_element_kind_mismatch_i32v()
        type(parquet_table) :: t
        integer(int32) :: good(2, 2)
        real(real64) :: other(2, 2)
        integer(int32), allocatable :: v(:)
        good = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        other = reshape([mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (an int32 vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as an int32 vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_i32v

    !> i64v takes the ONLY source that is neither its own kind nor the kind it widens from: an
    !! int32 vector is legal here (that is the widening arm), so this one uses a real64 vector.
    subroutine scenario_table_get_element_kind_mismatch_i64v()
        type(parquet_table) :: t
        integer(int64) :: good(2, 2)
        real(real64) :: other(2, 2)
        integer(int64), allocatable :: v(:)
        good = reshape([mismatch_i64v_a(), mismatch_i64v_a(), mismatch_i64v_a(), mismatch_i64v_a()], [2, 2])
        other = reshape([mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (an int64 vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as an int64 vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_i64v

    subroutine scenario_table_get_element_kind_mismatch_f32v()
        type(parquet_table) :: t
        real(real32) :: good(2, 2)
        integer(int32) :: other(2, 2)
        real(real32), allocatable :: v(:)
        good = reshape([mismatch_f32v_a(), mismatch_f32v_a(), mismatch_f32v_a(), mismatch_f32v_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a real32 vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a real32 vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_f32v

    subroutine scenario_table_get_element_kind_mismatch_f64v()
        type(parquet_table) :: t
        real(real64) :: good(2, 2)
        integer(int32) :: other(2, 2)
        real(real64), allocatable :: v(:)
        good = reshape([mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a(), mismatch_f64v_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a real64 vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a real64 vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_f64v

    subroutine scenario_table_get_element_kind_mismatch_boolv()
        type(parquet_table) :: t
        logical :: good(2, 2)
        integer(int32) :: other(2, 2)
        logical, allocatable :: v(:)
        good = reshape([mismatch_boolv_a(), mismatch_boolv_a(), mismatch_boolv_a(), mismatch_boolv_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a logical vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a logical vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_boolv

    subroutine scenario_table_get_element_kind_mismatch_datev()
        type(parquet_table) :: t
        type(parquet_date) :: good(2, 2)
        integer(int32) :: other(2, 2)
        type(parquet_date), allocatable :: v(:)
        good = reshape([mismatch_datev_a(), mismatch_datev_a(), mismatch_datev_a(), mismatch_datev_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a date vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a date vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_datev

    subroutine scenario_table_get_element_kind_mismatch_timev()
        type(parquet_table) :: t
        type(parquet_time) :: good(2, 2)
        integer(int32) :: other(2, 2)
        type(parquet_time), allocatable :: v(:)
        good = reshape([mismatch_timev_a(), mismatch_timev_a(), mismatch_timev_a(), mismatch_timev_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a time vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a time vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_timev

    subroutine scenario_table_get_element_kind_mismatch_tsv()
        type(parquet_table) :: t
        type(parquet_timestamp) :: good(2, 2)
        integer(int32) :: other(2, 2)
        type(parquet_timestamp), allocatable :: v(:)
        good = reshape([mismatch_tsv_a(), mismatch_tsv_a(), mismatch_tsv_a(), mismatch_tsv_a()], [2, 2])
        other = reshape([mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a(), mismatch_i32v_a()], [2, 2])
        call parquet_new_table(t)
        call t%add_column("ok", good)
        call t%add_column("other", other)
        call t%get_element("ok", 1_int64, v)     ! negative control: the kind that does serve
        print '(a,i0)', "matching vector kind answered, size=", size(v)
        call t%get_element("other", 1_int64, v)  ! -> aborts (a timestamp vector cannot read that column)
        print '(a,i0)', "unexpectedly read a mismatched column as a timestamp vector, size=", size(v)
    end subroutine scenario_table_get_element_kind_mismatch_tsv
    !> One value of each vector kind, for the eight scenarios above. Written as functions rather
    !! than literals so that each scenario's fixture stays a single readable line.
    function mismatch_i32v_a() result(v)
        integer(int32) :: v !! one representative value of this kind.
        v = 1_int32
    end function mismatch_i32v_a

    function mismatch_i64v_a() result(v)
        integer(int64) :: v !! one representative value of this kind.
        v = 1_int64
    end function mismatch_i64v_a

    function mismatch_f32v_a() result(v)
        real(real32) :: v !! one representative value of this kind.
        v = 1.5_real32
    end function mismatch_f32v_a

    function mismatch_f64v_a() result(v)
        real(real64) :: v !! one representative value of this kind.
        v = 1.5_real64
    end function mismatch_f64v_a

    function mismatch_boolv_a() result(v)
        logical :: v !! one representative value of this kind.
        v = .true.
    end function mismatch_boolv_a

    function mismatch_datev_a() result(v)
        type(parquet_date) :: v !! one representative value of this kind.
        v = parquet_date(2026, 1, 1)
    end function mismatch_datev_a

    function mismatch_timev_a() result(v)
        type(parquet_time) :: v !! one representative value of this kind.
        v = parquet_time(1, 2, 3)
    end function mismatch_timev_a

    function mismatch_tsv_a() result(v)
        type(parquet_timestamp) :: v !! one representative value of this kind.
        v = parquet_timestamp(2026, 1, 1, 1, 2, 3)
    end function mismatch_tsv_a
    subroutine scenario_table_get_element_kind_mismatch()
        type(parquet_table) :: t
        integer(int32) :: iv(2)
        real(real64) :: dv(2), v
        iv = [1_int32, 2_int32]
        dv = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("f", dv)
        call t%add_column("s", iv)
        call t%get_element("f", 1_int64, v)   ! negative control: the kind that does serve
        print '(a,f8.3)', "matching kind answered, v=", v
        call t%get_element("s", 1_int64, v)   ! int32 cannot be read as float64 -> aborts
        print '(a,f8.3)', "unexpectedly read an int32 column as float64, v=", v
    end subroutine scenario_table_get_element_kind_mismatch
    !
    !> %get_element on a name the table does not have, with no `found=` to report it through.
    subroutine scenario_table_get_element_missing_column()
        type(parquet_table) :: t
        real(real64) :: d(2), v
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        call t%get_element("a", 1_int64, v)   ! negative control
        print '(a,f8.3)', "existing column answered, v=", v
        call t%get_element("nope", 1_int64, v)   ! absent, no found= -> aborts
        print '(a,f8.3)', "unexpectedly read a missing column, v=", v
    end subroutine scenario_table_get_element_missing_column
    !
    !> USING a stale column handle must abort. `%is_valid()` only reports; this is the guard that
    !! stops a handle reading a slot that has been renumbered underneath it, and nothing else
    !! exercises `col_resolve`.
    subroutine scenario_col_handle_stale_after_mutation()
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        real(real64) :: d(2), v
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        call t%add_column("b", d)
        call t%column("a", c)
        call c%get(1_int64, v)          ! negative control: the handle works before the change
        print '(a,f8.3)', "fresh handle read, v=", v
        call t%drop_column("b")         ! renumbers the slots -> the handle is stale
        call c%get(1_int64, v)          ! aborts
        print '(a,f8.3)', "unexpectedly read through a stale handle, v=", v
    end subroutine scenario_col_handle_stale_after_mutation
    !
    !> A handle's own row bounds check, which is separate from the table's.
    subroutine scenario_col_handle_row_out_of_range()
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        real(real64) :: d(2), v
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        call t%column("a", c)
        call c%get(2_int64, v)          ! negative control: the last valid row
        print '(a,f8.3)', "in-range handle read, v=", v
        call c%get(3_int64, v)          ! one past the end -> aborts
        print '(a,f8.3)', "unexpectedly read past the last row through a handle, v=", v
    end subroutine scenario_col_handle_row_out_of_range
    !
    !> `%ref` on a stale handle is the worst thing this feature can do — it would hand back a raw
    !! pointer into storage the mutation reallocated, which no later check can catch. Its own
    !! `col_resolve` call is the only thing preventing it, and deleting that one line is invisible
    !! to every value test, so it gets a scenario of its own rather than relying on `%get`'s.
    subroutine scenario_col_handle_ref_after_mutation()
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        real(real64) :: d(2)
        real(real64), pointer :: p(:)
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("a", d)
        call t%add_column("b", d)
        call t%column("a", c)
        call c%ref(p)                   ! negative control: the handle works before the change
        print '(a,i0)', "fresh handle ref, size=", size(p)
        call t%delete_rows([1_int64])   ! reallocates every column's storage -> the handle is stale
        call c%ref(p)                   ! aborts
        print '(a,i0)', "unexpectedly aliased storage through a stale handle, size=", size(p)
    end subroutine scenario_col_handle_ref_after_mutation
    !
    !> `%ref` never widens, exactly as `%col` never does, and must say so in the same words.
    subroutine scenario_col_handle_ref_kind_mismatch()
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        integer(int32) :: iv(2)
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        iv = [1_int32, 2_int32]
        call parquet_new_table(t)
        call t%add_column("a", iv)
        call t%column("a", c)
        call c%ref(p32)                 ! negative control: the matching kind works
        print '(a,i0)', "matching-kind ref, size=", size(p32)
        call c%ref(p64)                 ! int64 pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through an int64 pointer, size=", size(p64)
    end subroutine scenario_col_handle_ref_kind_mismatch
    !
    !> USING a stale row handle must abort. This is the behaviour that did not exist before the
    !! handle gained a generation stamp: the handle used to keep a by-value row scope and read
    !! whatever now sat at its index, which is a wrong answer rather than an error.
    subroutine scenario_row_handle_stale_after_sort()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        real(real64) :: d(3), v
        d = [3.0_real64, 1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("x", d)
        r = t%row(1_int64)
        call r%get("x", v)              ! negative control: the handle works before the change
        print '(a,f8.3)', "fresh row handle read, v=", v
        call t%sort_by(["x"])           ! reorders the rows -> the handle is stale
        call r%get("x", v)              ! aborts
        print '(a,f8.3)', "unexpectedly read through a stale row handle, v=", v
    end subroutine scenario_row_handle_stale_after_sort
    !
    !> `%append` given a row handle on the DESTINATION gets its own message, not the generic
    !! staleness one: the append is what invalidated the handle, so "re-fetch it" would send the
    !! caller round the same loop. The advice has to be to snapshot the table instead.
    subroutine scenario_table_append_row_self()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        real(real64) :: d(2)
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t)
        call t%add_column("x", d)
        r = t%row(1_int64)
        call t%append(r)                ! negative control: the first append is legitimate
        print '(a,i0)', "first self-append ok, nrows=", t%nrows()
        call t%append(r)                ! the handle is now stale, and on THIS table -> aborts
        print '(a,i0)', "unexpectedly appended twice from a self handle, nrows=", t%nrows()
    end subroutine scenario_table_append_row_self
    !
    !> The other half of the Q10 split: a handle on ANOTHER table that has since changed gets the
    !! ordinary staleness message, whose "re-fetch it" advice does work there.
    subroutine scenario_table_append_row_stale_source()
        type(parquet_table) :: t, src
        type(parquet_table_row) :: r
        real(real64) :: d(2)
        ! Deliberately out of order, so the sort below really does reorder the rows.
        d = [2.0_real64, 1.0_real64]
        call parquet_new_table(src)
        call src%add_column("x", d)
        call parquet_new_table(t)
        call t%add_column("x", d)
        r = src%row(1_int64)
        call t%append(r)                ! negative control: a current source handle appends fine
        print '(a,i0)', "append from a current source handle ok, nrows=", t%nrows()
        call src%sort_by(["x"])         ! the SOURCE moved -> the handle is stale
        call t%append(r)                ! aborts
        print '(a,i0)', "unexpectedly appended from a stale source handle, nrows=", t%nrows()
    end subroutine scenario_table_append_row_stale_source
    !
    !> A column handle from ANOTHER table is not stale and not detached — it is a perfectly valid
    !! handle on a different object, so nothing else in the library would object to it. Without
    !! this check `r%get(c, v)` would read that other table's column at this row's index and
    !! return a plausible number, which is the one failure mode in the handle design that produces
    !! a wrong answer rather than an error.
    subroutine scenario_row_handle_foreign_column()
        type(parquet_table) :: t1, t2
        type(parquet_table_row) :: r
        type(parquet_table_col) :: c1, c2
        real(real64) :: d(2), v
        d = [1.0_real64, 2.0_real64]
        call parquet_new_table(t1)
        call t1%add_column("a", d)
        call parquet_new_table(t2)
        call t2%add_column("a", d * 10.0_real64)
        r = t1%row(1_int64)
        call t1%column("a", c1)
        call t2%column("a", c2)
        call r%get(c1, v)               ! negative control: this table's own handle works
        print '(a,f8.3)', "own-table handle read, v=", v
        call r%get(c2, v)               ! a handle on the OTHER table -> aborts
        print '(a,f8.3)', "unexpectedly read another table's column through a row handle, v=", v
    end subroutine scenario_row_handle_foreign_column
    !
    !> A handle that was never produced by %column points at nothing and must say so.
    subroutine scenario_col_handle_never_attached()
        type(parquet_table_col) :: c
        real(real64) :: v
        print '(a,l1)', "is_valid on a never-attached handle: ", c%is_valid()
        call c%get(1_int64, v)          ! never attached -> aborts
        print '(a,f8.3)', "unexpectedly read through a never-attached handle, v=", v
    end subroutine scenario_col_handle_never_attached

    !> The pointer path is exact-kind by design (it aliases raw storage), so asking for an
    !! int64 pointer into an int32 column must abort rather than silently widening.
    subroutine scenario_table_pointer_kind_mismatch()
        type(parquet_table) :: t
        integer(int64), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr.parquet")
        call t%col("id", p)   ! int64 pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through an int64 pointer, size=", size(p)
    end subroutine scenario_table_pointer_kind_mismatch

    !> The copy-out path widens int32->int64/float32->float64 but nothing else, so a float64
    !! column copied into an int32 array must abort rather than truncate or reinterpret it.
    subroutine scenario_table_get_array_kind_mismatch()
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr.parquet")
        call t%get("val", v)   ! float64 column into an int32 array -> aborts
        print '(a,i0)', "unexpectedly copied a float64 column into an int32 array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch

    !> scenario_table_pointer_kind_mismatch above only exercises col_ptr_i64's own mismatch
    !! branch (an int64 pointer into an int32 column). Every other col_ptr_* specific has the
    !! identical guard on its own source line, so each needs its own abort to cover it -- one
    !! scenario per remaining kind, all built over the same two-column ("id" int32, "val"
    !! float64) fixture, picking whichever of the two columns is NOT of the pointer's own kind.
    subroutine scenario_table_col_ptr_kind_mismatch_i32()
        type(parquet_table) :: t
        integer(int32), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_i32.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_i32.parquet")
        call t%col("val", p)   ! int32 pointer into a float64 column -> aborts
        print '(a,i0)', "unexpectedly aliased a float64 column through an int32 pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_i32

    subroutine scenario_table_col_ptr_kind_mismatch_f32()
        type(parquet_table) :: t
        real(real32), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_f32.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_f32.parquet")
        call t%col("id", p)   ! float32 pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a float32 pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_f32

    subroutine scenario_table_col_ptr_kind_mismatch_f64()
        type(parquet_table) :: t
        real(real64), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_f64.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_f64.parquet")
        call t%col("id", p)   ! float64 pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a float64 pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_f64

    subroutine scenario_table_col_ptr_kind_mismatch_bool()
        type(parquet_table) :: t
        logical, pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_bool.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_bool.parquet")
        call t%col("id", p)   ! logical pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a logical pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_bool

    subroutine scenario_table_col_ptr_kind_mismatch_date()
        type(parquet_table) :: t
        type(parquet_date), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_date.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_date.parquet")
        call t%col("id", p)   ! date pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a date pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_date

    subroutine scenario_table_col_ptr_kind_mismatch_time()
        type(parquet_table) :: t
        type(parquet_time), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_time.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_time.parquet")
        call t%col("id", p)   ! time pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a time pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_time

    subroutine scenario_table_col_ptr_kind_mismatch_ts()
        type(parquet_table) :: t
        type(parquet_timestamp), pointer :: p(:)
        call write_table_scenario_fixture("test_run/es_table_ptr_ts.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_ts.parquet")
        call t%col("id", p)   ! timestamp pointer into an int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased an int32 column through a timestamp pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_ts

    subroutine scenario_table_col_ptr_kind_mismatch_i32v()
        type(parquet_table) :: t
        integer(int32), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_i32v.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_i32v.parquet")
        call t%col("id", p)   ! int32_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through an int32_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_i32v

    subroutine scenario_table_col_ptr_kind_mismatch_i64v()
        type(parquet_table) :: t
        integer(int64), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_i64v.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_i64v.parquet")
        call t%col("id", p)   ! int64_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through an int64_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_i64v

    subroutine scenario_table_col_ptr_kind_mismatch_f32v()
        type(parquet_table) :: t
        real(real32), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_f32v.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_f32v.parquet")
        call t%col("id", p)   ! float32_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a float32_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_f32v

    subroutine scenario_table_col_ptr_kind_mismatch_f64v()
        type(parquet_table) :: t
        real(real64), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_f64v.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_f64v.parquet")
        call t%col("id", p)   ! float64_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a float64_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_f64v

    subroutine scenario_table_col_ptr_kind_mismatch_boolv()
        type(parquet_table) :: t
        logical, pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_boolv.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_boolv.parquet")
        call t%col("id", p)   ! logical_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a logical_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_boolv

    subroutine scenario_table_col_ptr_kind_mismatch_datev()
        type(parquet_table) :: t
        type(parquet_date), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_datev.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_datev.parquet")
        call t%col("id", p)   ! date_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a date_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_datev

    subroutine scenario_table_col_ptr_kind_mismatch_timev()
        type(parquet_table) :: t
        type(parquet_time), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_timev.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_timev.parquet")
        call t%col("id", p)   ! time_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a time_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_timev

    subroutine scenario_table_col_ptr_kind_mismatch_tsv()
        type(parquet_table) :: t
        type(parquet_timestamp), pointer :: p(:,:)
        call write_table_scenario_fixture("test_run/es_table_ptr_tsv.parquet")
        call parquet_open_table(t, "test_run/es_table_ptr_tsv.parquet")
        call t%col("id", p)   ! timestamp_vec pointer into a scalar int32 column -> aborts
        print '(a,i0)', "unexpectedly aliased a scalar column through a timestamp_vec pointer, size=", size(p)
    end subroutine scenario_table_col_ptr_kind_mismatch_tsv

    !> scenario_table_get_array_kind_mismatch above only exercises get_arr_i32's own default
    !! (mismatch) branch. Every other get_arr_* specific has the same guard on its own source
    !! line -- including, for the two kinds with a widening case (i64/i64v widen from i32/i32v,
    !! f64/f64v widen from f32/f32v), a SEPARATE default branch below that widening case -- so
    !! each needs its own abort. All built over the same two-column fixture as the col_ptr
    !! scenarios above; "val" (float64) is used only where "id" (int32) would hit a widening
    !! case instead of the mismatch default.
    subroutine scenario_table_get_array_kind_mismatch_i64()
        type(parquet_table) :: t
        integer(int64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_i64.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_i64.parquet")
        call t%get("val", v)   ! float64 column into an int64 array -> aborts
        print '(a,i0)', "unexpectedly copied a float64 column into an int64 array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_i64

    subroutine scenario_table_get_array_kind_mismatch_f32()
        type(parquet_table) :: t
        real(real32), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_f32.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_f32.parquet")
        call t%get("id", v)   ! int32 column into a float32 array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a float32 array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_f32

    subroutine scenario_table_get_array_kind_mismatch_f64()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_f64.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_f64.parquet")
        call t%get("id", v)   ! int32 column into a float64 array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a float64 array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_f64

    subroutine scenario_table_get_array_kind_mismatch_bool()
        type(parquet_table) :: t
        logical, allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_bool.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_bool.parquet")
        call t%get("id", v)   ! int32 column into a logical array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a logical array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_bool

    subroutine scenario_table_get_array_kind_mismatch_date()
        type(parquet_table) :: t
        type(parquet_date), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_date.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_date.parquet")
        call t%get("id", v)   ! int32 column into a date array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a date array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_date

    subroutine scenario_table_get_array_kind_mismatch_time()
        type(parquet_table) :: t
        type(parquet_time), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_time.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_time.parquet")
        call t%get("id", v)   ! int32 column into a time array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a time array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_time

    subroutine scenario_table_get_array_kind_mismatch_ts()
        type(parquet_table) :: t
        type(parquet_timestamp), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_ts.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_ts.parquet")
        call t%get("id", v)   ! int32 column into a timestamp array -> aborts
        print '(a,i0)', "unexpectedly copied an int32 column into a timestamp array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_ts

    subroutine scenario_table_get_array_kind_mismatch_i32v()
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_i32v.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_i32v.parquet")
        call t%get("id", v)   ! scalar int32 column into an int32_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into an int32_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_i32v

    subroutine scenario_table_get_array_kind_mismatch_i64v()
        type(parquet_table) :: t
        integer(int64), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_i64v.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_i64v.parquet")
        call t%get("id", v)   ! scalar int32 column into an int64_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into an int64_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_i64v

    subroutine scenario_table_get_array_kind_mismatch_f32v()
        type(parquet_table) :: t
        real(real32), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_f32v.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_f32v.parquet")
        call t%get("id", v)   ! scalar int32 column into a float32_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a float32_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_f32v

    subroutine scenario_table_get_array_kind_mismatch_f64v()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_f64v.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_f64v.parquet")
        call t%get("id", v)   ! scalar int32 column into a float64_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a float64_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_f64v

    subroutine scenario_table_get_array_kind_mismatch_boolv()
        type(parquet_table) :: t
        logical, allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_boolv.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_boolv.parquet")
        call t%get("id", v)   ! scalar int32 column into a logical_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a logical_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_boolv

    subroutine scenario_table_get_array_kind_mismatch_datev()
        type(parquet_table) :: t
        type(parquet_date), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_datev.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_datev.parquet")
        call t%get("id", v)   ! scalar int32 column into a date_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a date_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_datev

    subroutine scenario_table_get_array_kind_mismatch_timev()
        type(parquet_table) :: t
        type(parquet_time), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_timev.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_timev.parquet")
        call t%get("id", v)   ! scalar int32 column into a time_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a time_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_timev

    subroutine scenario_table_get_array_kind_mismatch_tsv()
        type(parquet_table) :: t
        type(parquet_timestamp), allocatable :: v(:,:)
        call write_table_scenario_fixture("test_run/es_table_get_arr_tsv.parquet")
        call parquet_open_table(t, "test_run/es_table_get_arr_tsv.parquet")
        call t%get("id", v)   ! scalar int32 column into a timestamp_vec array -> aborts
        print '(a,i0)', "unexpectedly copied a scalar column into a timestamp_vec array, size=", size(v)
    end subroutine scenario_table_get_array_kind_mismatch_tsv

    !> Without found=, a missing column is fatal rather than quietly empty.
    subroutine scenario_table_unknown_column()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_unknown.parquet")
        call parquet_open_table(t, "test_run/es_table_unknown.parquet")
        call t%get("no_such_column", v)   ! -> aborts
        print '(a,i0)', "unexpectedly read a column that does not exist, size=", size(v)
    end subroutine scenario_table_unknown_column

    !> A column whose physical type this library cannot read gets a slot so it still shows up in
    !! a listing, but reading its values must say so rather than hand back an empty column.
    subroutine scenario_table_unsupported_column_read()
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:)
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        call t%get("map_col", v)   ! a MAP column, unreadable -> aborts
        print '(a,i0)', "unexpectedly read an unsupported column, size=", size(v)
    end subroutine scenario_table_unsupported_column_read

    !> %prefetch resolves its name through table_prefetch_resolve, a separate procedure from
    !! table_resolve -- so, like %kind's table_lookup_or_fail above, it needs its own scenario
    !! for the "no found=, missing column" abort rather than relying on table_unknown_column.
    subroutine scenario_table_prefetch_unknown_column()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_prefetch_unknown.parquet")
        call parquet_open_table(t, "test_run/es_table_prefetch_unknown.parquet")
        call t%prefetch("no_such_column")   ! no found= -> aborts
        print '(a)', "unexpectedly prefetched a column that does not exist"
    end subroutine scenario_table_prefetch_unknown_column

    !> Asking to prefetch a column this library cannot read is a mistake, not a quiet no-op,
    !! even though skipping it would be harmless -- so table_prefetch_resolve rejects it
    !! regardless of found=.
    subroutine scenario_table_prefetch_unsupported_column()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        call t%prefetch("map_col")   ! a MAP column, unreadable -> aborts
        print '(a)', "unexpectedly prefetched an unsupported column"
    end subroutine scenario_table_prefetch_unsupported_column

    !> Every column of a table must have the same number of rows.
    subroutine scenario_table_add_column_row_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%add_column("b", [1_int32, 2_int32])   ! wrong length -> aborts
        print '(a,i0)', "unexpectedly added a column of the wrong length, nrows=", t%nrows()
    end subroutine scenario_table_add_column_row_mismatch

    !> Replacing a column silently would lose data, so it needs an explicit force=.
    !> `%add_column` taking a whole `parquet_column` is the only form that can be handed something
    !! with no kind: every other one takes its kind from the TYPE of the array it is given, while
    !! this one reads it off the column. Without the guard the table gains a `PK_NONE` slot that no
    !! accessor can read or write, and the failure surfaces at the first `%get` -- far from the
    !! `%add_column` that caused it, and with nothing naming the column that was never filled.
    !!
    !! The column added first is the negative control: it proves the guard rejects a KINDLESS
    !! column rather than every column, which a guard that simply always fired would also pass.
    subroutine scenario_table_add_column_kindless()
        type(parquet_table) :: t
        type(parquet_column) :: good, empty
        call good%init(PK_INT32, 2_int64)
        call good%set_all([1_int32, 2_int32])
        call parquet_new_table(t)
        call t%add_column("a", good)     ! a real column: accepted
        call t%add_column("b", empty)    ! never %init'd -> aborts
        print '(a,i0)', "unexpectedly added a kindless column, ncols=", t%ncols()
    end subroutine scenario_table_add_column_kindless

    subroutine scenario_table_add_column_duplicate()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%add_column("a", [4_int32, 5_int32, 6_int32])   ! no force= -> aborts
        print '(a,i0)', "unexpectedly replaced a column without force=, ncols=", t%ncols()
    end subroutine scenario_table_add_column_duplicate

    !> The same guard, but with force= passed explicitly as .false. rather than omitted --
    !! table_new_slot has its own separate branch for each of the two cases.
    subroutine scenario_table_add_column_duplicate_force_false()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%add_column("a", [4_int32, 5_int32, 6_int32], force=.false.)   ! -> aborts
        print '(a,i0)', "unexpectedly replaced a column with force=.false., ncols=", t%ncols()
    end subroutine scenario_table_add_column_duplicate_force_false

    !> A table opened without a slice never populates rg_bounds at open time, so
    !! %row_group_bounds must reject an in-memory table (which has no reader to ask either).
    subroutine scenario_table_row_group_bounds_in_memory()
        type(parquet_table) :: t
        integer(int64), allocatable :: bounds(:,:)
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%row_group_bounds(bounds)   ! no file behind this table -> aborts
        print '(a,i0)', "unexpectedly reported row group bounds for an in-memory table, n=", size(bounds, 2)
    end subroutine scenario_table_row_group_bounds_in_memory

    !> %set is same-length AND same-kind: table_require_kind is the shared guard behind every
    !! set_arr_* specific, so one abort here exercises all of them.
    subroutine scenario_table_set_kind_mismatch()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_set_kind.parquet")
        call parquet_open_table(t, "test_run/es_table_set_kind.parquet")
        call t%set("id", [1.5_real64, 2.5_real64, 3.5_real64])   ! int32 column, float64 array -> aborts
        print '(a)', "unexpectedly set an int32 column from a float64 array"
    end subroutine scenario_table_set_kind_mismatch

    !> Without found=, asking an in-memory table for file metadata is fatal -- there is no file
    !! to have metadata, and quietly returning "" would look like a present-but-empty value.
    subroutine scenario_table_get_file_metadata_in_memory()
        type(parquet_table) :: t
        character(len=:), allocatable :: val
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%get_file_metadata("some_key", val)   ! no file behind this table -> aborts
        print '(a,a)', "unexpectedly read file metadata from an in-memory table, val=", val
    end subroutine scenario_table_get_file_metadata_in_memory

    !> Without found=, an unknown metadata key on a real file is fatal rather than a silent "".
    subroutine scenario_table_get_file_metadata_missing_key()
        type(parquet_table) :: t
        character(len=:), allocatable :: val
        call write_table_scenario_fixture("test_run/es_table_meta_miss.parquet")
        call parquet_open_table(t, "test_run/es_table_meta_miss.parquet")
        call t%get_file_metadata("no_such_key", val)   ! -> aborts
        print '(a,a)', "unexpectedly read an unknown metadata key, val=", val
    end subroutine scenario_table_get_file_metadata_missing_key

    !> table_lookup_or_fail is the shared guard behind %kind/%width/%unit/%residency/
    !! %is_supported -- a separate procedure from table_resolve, so it needs its own scenario
    !! rather than relying on table_unknown_column above (which only exercises table_resolve).
    subroutine scenario_table_kind_unknown_column()
        type(parquet_table) :: t
        integer :: k
        call write_table_scenario_fixture("test_run/es_table_kind_unknown.parquet")
        call parquet_open_table(t, "test_run/es_table_kind_unknown.parquet")
        k = t%kind("no_such_column")   ! no found= -> aborts
        print '(a,i0)', "unexpectedly reported a kind for a column that does not exist, k=", k
    end subroutine scenario_table_kind_unknown_column

    !> %set replaces values, never the row set, so a different-length array must abort.
    subroutine scenario_table_set_length_mismatch()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_set.parquet")
        call parquet_open_table(t, "test_run/es_table_set.parquet")
        call t%set("val", [1.0_real64, 2.0_real64])   ! 2 values into a 3-row column -> aborts
        print '(a,i0)', "unexpectedly set a column from a shorter array, nrows=", t%nrows()
    end subroutine scenario_table_set_length_mismatch

    !> A schema naming a column the table does not have is a typo, not an empty output column.
    subroutine scenario_table_write_missing_column()
        type(parquet_table) :: t
        type(parquet_schema) :: s
        call write_table_scenario_fixture("test_run/es_table_wmiss_in.parquet")
        call parquet_open_table(t, "test_run/es_table_wmiss_in.parquet")
        call s%init("wmiss")
        call s%add_field("id", "int32")
        call s%add_field("absent", "float64")
        call parquet_write_table(t, "test_run/es_table_wmiss_out.parquet", s)   ! -> aborts
        print '(a)', "unexpectedly wrote a table missing a schema column"
    end subroutine scenario_table_write_missing_column

    !> A schema naming a column the table DOES have, but whose values were never readable in the
    !! first place (a foreign physical type), is a different mistake from a missing column and
    !! has its own guard/message in parquet_write_table.
    subroutine scenario_table_write_unsupported_column()
        type(parquet_table) :: t
        type(parquet_schema) :: s
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        call s%init("wunsupported")
        call s%add_field("map_col", "int32")
        call parquet_write_table(t, "test_run/es_table_wunsupported_out.parquet", s)   ! -> aborts
        print '(a)', "unexpectedly wrote a table's unsupported column"
    end subroutine scenario_table_write_unsupported_column

    !> `overwrite=` is forwarded to parquet_open_writer untouched, which is the whole contract of
    !! the writer options on parquet_write_table: a table write over an existing file must fail in
    !! exactly the place, and with exactly the message, a hand-written open would.
    subroutine scenario_table_write_no_overwrite()
        type(parquet_table) :: t
        type(parquet_schema) :: s
        call write_table_scenario_fixture("test_run/es_table_nowr_in.parquet")
        call parquet_open_table(t, "test_run/es_table_nowr_in.parquet")
        call s%init("nowr")
        call s%add_field("id", "int32")
        call parquet_write_table(t, "test_run/es_table_nowr_out.parquet", s)
        call parquet_write_table(t, "test_run/es_table_nowr_out.parquet", s, overwrite=.false.)   ! -> aborts
        print '(a)', "unexpectedly overwrote an existing file with overwrite=.false."
    end subroutine scenario_table_write_no_overwrite

    !> A schema-less write of a table with nothing resident produces a valid EMPTY file -- but a
    !! MAML cannot describe zero columns, so asking for a sidecar alongside it is a request the
    !! library cannot satisfy. It says so rather than silently skipping the file it was asked for.
    subroutine scenario_table_write_schemaless_empty_maml()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_slessmaml_in.parquet")
        call parquet_open_table(t, "test_run/es_table_slessmaml_in.parquet")   ! reads nothing
        call parquet_write_table(t, "test_run/es_table_slessmaml_out.parquet", write_maml=.true.)
        print '(a)', "unexpectedly wrote a sidecar MAML for a zero-column table"
    end subroutine scenario_table_write_schemaless_empty_maml

    !> The row index says which row of the SOURCE FILE each row came from, so a table that has
    !! cut its file loose can no longer produce it. Materializing it before the mutation is the
    !! documented way round, which is what the message says.
    !> Materializing the automatic row index on a table whose file carried its OWN column of that
    !! name warns, and a table whose file did not stays silent.
    !!
    !! Both halves run in one process on purpose: the second is the negative control, and a
    !! warning that fires unconditionally would pass every assertion written for the first.
    !! Neither half aborts -- the expected exit status is 0 -- so this scenario is about what
    !! reaches the stream, not about a guard.
    subroutine scenario_table_row_index_shadowed_warning()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        integer(int64), allocatable :: ri(:)
        character(len=*), parameter :: shadowed = "test_run/es_rowindex_shadowed.parquet"
        character(len=*), parameter :: clean = "test_run/es_rowindex_clean.parquet"

        ! A file carrying its own parquet_row_index column. The open-time warning fires here.
        call parquet_open_writer(w, shadowed)
        call parquet_write_column(w, "a", [10_int32, 20_int32, 30_int32])
        call parquet_write_column(w, PARQUET_ROW_INDEX, [7_int32, 8_int32, 9_int32])
        call parquet_close_writer(w)

        call parquet_open_table(t, shadowed)
        print '(a)', "opened the shadowed file"
        call t%get(PARQUET_ROW_INDEX, ri)
        print '(a,i0,a,i0)', "shadowed row index n=", size(ri), " first=", ri(1)

        ! The control: same read, on a file with no such column of its own.
        call parquet_open_writer(w, clean)
        call parquet_write_column(w, "a", [10_int32, 20_int32, 30_int32])
        call parquet_close_writer(w)

        call parquet_open_table(t, clean)
        deallocate(ri)
        call t%get(PARQUET_ROW_INDEX, ri)
        print '(a,i0,a,i0)', "clean row index n=", size(ri), " first=", ri(1)
    end subroutine scenario_table_row_index_shadowed_warning

    subroutine scenario_table_row_index_after_detach()
        type(parquet_table) :: t
        integer(int64), allocatable :: ri(:)
        call write_table_scenario_fixture("test_run/es_rowindex_detached.parquet")
        call parquet_open_table(t, "test_run/es_rowindex_detached.parquet")
        call t%materialize_all()
        call t%truncate(2)
        call t%get(PARQUET_ROW_INDEX, ri)   ! -> aborts
        print '(a,i0)', "unexpectedly produced a row index after detaching, n=", size(ri)
    end subroutine scenario_table_row_index_after_detach

    !> A column built in memory has no file to be read back from, so evicting its values would
    !! be data loss rather than a memory saving.
    subroutine scenario_table_evict_in_memory()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%evict_column("a")   ! -> aborts
        print '(a)', "unexpectedly evicted an in-memory column"
    end subroutine scenario_table_evict_in_memory

    !> A detached table can never read a column back, so evicting one is data loss too.
    subroutine scenario_table_evict_detached()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_evict_detached.parquet")
        call parquet_open_table(t, "test_run/es_evict_detached.parquet")
        call t%materialize_all()
        call t%truncate(2)
        call t%evict_column("id")   ! -> aborts
        print '(a)', "unexpectedly evicted a column of a detached table"
    end subroutine scenario_table_evict_detached

    !> A caller-supplied validity mask must have one entry per row: a short one would silently
    !! leave the tail of the column at whatever nulls it had, which is exactly the mistake.
    subroutine scenario_table_set_is_valid_length()
        type(parquet_table) :: t
        logical :: valid(2)
        call write_table_scenario_fixture("test_run/es_isvalid_len.parquet")
        call parquet_open_table(t, "test_run/es_isvalid_len.parquet")
        valid = .true.
        call t%set("id", [7_int32, 8_int32, 9_int32], is_valid=valid)   ! 3 rows, 2 entries -> aborts
        print '(a)', "unexpectedly accepted an is_valid mask of the wrong length"
    end subroutine scenario_table_set_is_valid_length

    !> Writes a fixture carrying two metadata keys, for the copy_metadata scenarios below.
    subroutine write_metadata_scenario_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        real(real64) :: v(3)
        v = [1.0_real64, 2.0_real64, 3.0_real64]
        call s%init("meta_src")
        call s%add_field("v", "float64")
        call s%add_metadata("origin", "survey_A")
        call parquet_open_writer(w, fname, s)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
    end subroutine write_metadata_scenario_fixture

    !> Naming a metadata key the source file does not carry is a mistake, not an omission: the
    !! output would silently lack what the caller asked to preserve.
    subroutine scenario_table_copy_metadata_unknown_key()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call write_metadata_scenario_fixture("test_run/es_meta_key_in.parquet")
        call parquet_open_table(t, "test_run/es_meta_key_in.parquet")
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_write_table(t, "test_run/es_meta_key_out.parquet", out_s, &
            metadata_keys=["no_such_key"])   ! -> aborts
        print '(a)', "unexpectedly carried a metadata key the source file does not have"
    end subroutine scenario_table_copy_metadata_unknown_key

    !> A table built in memory has no source file, so there is no metadata to carry from one.
    subroutine scenario_table_copy_metadata_in_memory()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call parquet_new_table(t)
        call t%add_column("v", [1.0_real64, 2.0_real64, 3.0_real64])
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_write_table(t, "test_run/es_meta_mem_out.parquet", out_s, &
            copy_metadata=.true.)   ! -> aborts
        print '(a)', "unexpectedly copied source metadata for a table with no source file"
    end subroutine scenario_table_copy_metadata_in_memory

    !> copy_metadata=.true. means "every key" and metadata_keys= means "these"; asking for both at
    !! once has no consistent reading, so it is refused rather than silently preferring one.
    subroutine scenario_table_copy_metadata_both_forms()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call write_metadata_scenario_fixture("test_run/es_meta_both_in.parquet")
        call parquet_open_table(t, "test_run/es_meta_both_in.parquet")
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_write_table(t, "test_run/es_meta_both_out.parquet", out_s, &
            copy_metadata=.true., metadata_keys=["origin"])   ! -> aborts
        print '(a)', "unexpectedly accepted copy_metadata= and metadata_keys= together"
    end subroutine scenario_table_copy_metadata_both_forms

    !> A schema that was never built at all cannot be parsed into anything, and writing with it
    !! would otherwise read uninitialized state and run away. Reported as its own mistake rather
    !! than as a parse failure of MAML text that does not exist.
    subroutine scenario_table_write_unbuilt_schema()
        type(parquet_table) :: t
        type(parquet_schema) :: s
        call write_table_scenario_fixture("test_run/es_table_unbuilt_in.parquet")
        call parquet_open_table(t, "test_run/es_table_unbuilt_in.parquet")
        ! Never %init'd, so there is no MAML text to parse and nothing naming a column. An
        ! unparsed but BUILT schema is not an error at all any more -- parquet_write_table parses
        ! it itself (test_write_table_parses_schema).
        call parquet_write_table(t, "test_run/es_table_unbuilt_out.parquet", s)   ! -> aborts
        print '(a)', "unexpectedly wrote a table with a schema that was never built"
    end subroutine scenario_table_write_unbuilt_schema

    !> A slice starting before row 1 cannot be satisfied, and quietly clamping it would hand
    !! back a table whose row 1 is not the row the caller asked for.
    subroutine scenario_table_slice_below_first_row()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_slice_lo.parquet")
        call parquet_open_table(t, "test_run/es_table_slice_lo.parquet", 0, 2)   ! -> aborts
        print '(a,i0)', "unexpectedly opened a slice starting below row 1, nrows=", t%nrows()
    end subroutine scenario_table_slice_below_first_row

    !> Likewise past the end: the rows simply are not there.
    subroutine scenario_table_slice_past_last_row()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_slice_hi.parquet")
        call parquet_open_table(t, "test_run/es_table_slice_hi.parquet", 2, 99)   ! -> aborts
        print '(a,i0)', "unexpectedly opened a slice past the last row, nrows=", t%nrows()
    end subroutine scenario_table_slice_past_last_row

    !> An inverted slice would silently be an empty table, which is never what was meant.
    subroutine scenario_table_slice_inverted()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_slice_inv.parquet")
        call parquet_open_table(t, "test_run/es_table_slice_inv.parquet", 3, 1)   ! -> aborts
        print '(a,i0)', "unexpectedly opened an inverted slice, nrows=", t%nrows()
    end subroutine scenario_table_slice_inverted

    !> A row handle is checked where it is made, not at its first read: a handle that can never
    !! work should fail at the mistake, not several calls later.
    subroutine scenario_table_row_index_out_of_range()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        call write_table_scenario_fixture("test_run/es_table_rowidx.parquet")
        call parquet_open_table(t, "test_run/es_table_rowidx.parquet")
        r = t%row(9)   ! only 3 rows -> aborts
        print '(a,i0)', "unexpectedly made a handle on a row that does not exist, i=", r%index()
    end subroutine scenario_table_row_index_out_of_range

    !> Every index a slice selects is validated before anything indexes with it, so an
    !! out-of-range one is a clear message rather than a bounds abort inside the value store.
    subroutine scenario_table_get_slice_out_of_range()
        type(parquet_table) :: t
        type(parquet_slice) :: s
        real(real64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_slice_range.parquet")
        call parquet_open_table(t, "test_run/es_table_slice_range.parquet")
        s = parquet_slice_list([1, 7])   ! only 3 rows
        call t%get_slice("val", s, v)    ! -> aborts
        print '(a,i0)', "unexpectedly sliced past the last row, size=", size(v)
    end subroutine scenario_table_get_slice_out_of_range

    !> A zero step is rejected when the slice is built: every use of it would either loop
    !! forever or select nothing.
    subroutine scenario_table_slice_zero_step()
        type(parquet_slice) :: s
        s = parquet_slice_range(1, 3, 0)   ! -> aborts
        print '(a)', "unexpectedly built a slice with a zero step"
    end subroutine scenario_table_slice_zero_step

    !> A column added in memory has no file behind it, so there is nothing to reload from --
    !! and keeping its current values would make %reload look like it had worked.
    subroutine scenario_table_reload_in_memory_column()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_reload_col.parquet")
        call parquet_open_table(t, "test_run/es_table_reload_col.parquet")
        call t%add_column("computed", [1.0_real64, 2.0_real64, 3.0_real64])
        call t%reload("computed")   ! -> aborts
        print '(a,i0)', "unexpectedly reloaded an in-memory column, ncols=", t%ncols()
    end subroutine scenario_table_reload_in_memory_column

    !> The same, one level up: a table that never came from a file has nothing to reload at all.
    subroutine scenario_table_reload_not_file_backed()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%reload("a")   ! -> aborts
        print '(a,i0)', "unexpectedly reloaded a column of an in-memory table, ncols=", t%ncols()
    end subroutine scenario_table_reload_not_file_backed

    !> Evicting a column the caller has written into would silently restore the FILE's values on
    !! the next read, which is data loss with nothing to notice -- so it is refused unless
    !! force=.true. says otherwise. The negative controls (an unedited column evicts freely, and
    !! force=.true. really does get through) are test_user_populated_guard in test_table.f90.
    subroutine scenario_table_evict_user_populated()
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        call write_table_scenario_fixture("test_run/es_table_evict_userpop.parquet")
        call parquet_open_table(t, "test_run/es_table_evict_userpop.parquet")
        call t%get("val", g)
        call t%set("val", [9.0_real64, 9.0_real64, 9.0_real64])
        call t%evict_column("val")   ! -> aborts
        print '(a,i0)', "unexpectedly evicted a column holding local edits, ncols=", t%ncols()
    end subroutine scenario_table_evict_user_populated

    !> The same rule for %reload: discarding the caller's edits is what it is FOR, but it says so
    !! rather than assuming, so that %reload and %evict_column are one rule instead of two.
    subroutine scenario_table_reload_user_populated()
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        call write_table_scenario_fixture("test_run/es_table_reload_userpop.parquet")
        call parquet_open_table(t, "test_run/es_table_reload_userpop.parquet")
        call t%get("val", g)
        call t%set("val", [9.0_real64, 9.0_real64, 9.0_real64])
        call t%reload("val")   ! -> aborts
        print '(a,i0)', "unexpectedly reloaded a column holding local edits, ncols=", t%ncols()
    end subroutine scenario_table_reload_user_populated

    !> Claiming a column that holds no values: there is nothing to protect, and the claim would
    !! outlive the read that eventually fills the slot. The %set_user_populated(..., .false.) call
    !! just before it is the negative control INSIDE the scenario -- clearing is always allowed,
    !! on any residency, so a guard that refused every non-resident column would abort there
    !! instead and the stderr check below would not match.
    subroutine scenario_table_set_user_populated_not_resident()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_setuserpop.parquet")
        call parquet_open_table(t, "test_run/es_table_setuserpop.parquet")
        call t%set_user_populated("val", .false.)
        call t%set_user_populated("val", .true.)   ! -> aborts
        print '(a,i0)', "unexpectedly claimed a column holding no values, ncols=", t%ncols()
    end subroutine scenario_table_set_user_populated_not_resident

    !> A row handle resolves its column by name on every access, so a name that is not there is
    !! as fatal as it is on the table itself.
    subroutine scenario_table_row_unknown_column()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        real(real64) :: v
        call write_table_scenario_fixture("test_run/es_table_row_unknown.parquet")
        call parquet_open_table(t, "test_run/es_table_row_unknown.parquet")
        r = t%row(1)
        call r%get("no_such_column", v)   ! -> aborts
        print '(a,f0.1)', "unexpectedly read a row of a column that does not exist, v=", v
    end subroutine scenario_table_row_unknown_column

    !> A row read into a variable whose kind the column cannot be widened into must say so
    !! rather than hand back a converted-looking value.
    subroutine scenario_table_row_kind_mismatch()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        logical :: v
        call write_table_scenario_fixture("test_run/es_table_row_kind.parquet")
        call parquet_open_table(t, "test_run/es_table_row_kind.parquet")
        r = t%row(1)
        call r%get("val", v)   ! float64 column into a logical -> aborts
        print '(a,l1)', "unexpectedly read a float64 row into a logical, v=", v
    end subroutine scenario_table_row_kind_mismatch

    !> A default-initialized row handle is not attached to any table (its %cache pointer is
    !! never associated except by t%row(i)), so any access through it must say so rather than
    !! dereference a null cache.
    subroutine scenario_table_row_unattached()
        type(parquet_table_row) :: r
        real(real64) :: v
        call r%get("val", v)   ! r was never obtained from t%row(i) -> aborts
        print '(a,f0.1)', "unexpectedly read a column through an unattached row handle, v=", v
    end subroutine scenario_table_row_unattached

    !> A row handle resolves its column the same way the table itself does: a column this
    !! library cannot read is a mistake, not a quiet no-op, even through the row path.
    subroutine scenario_table_row_unsupported_column()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        integer(int32) :: v
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        r = t%row(1)
        call r%get("map_col", v)   ! a MAP column, unreadable -> aborts
        print '(a,i0)', "unexpectedly read an unsupported column through a row handle, v=", v
    end subroutine scenario_table_row_unsupported_column

    !> row_get_str/row_get_strv go through row_require_kind rather than the inline
    !! select-case/row_kind_error every numeric row_get_* specific uses (see
    !! table_row_kind_mismatch above, which only exercises the numeric path) -- so the string
    !! specific's own kind guard needs its own scenario.
    subroutine scenario_table_row_string_kind_mismatch()
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        character(len=:), allocatable :: v
        call write_table_scenario_fixture("test_run/es_table_row_str_kind.parquet")
        call parquet_open_table(t, "test_run/es_table_row_str_kind.parquet")
        r = t%row(1)
        call r%get("val", v)   ! float64 column into a character variable -> aborts
        print '(a,a)', "unexpectedly read a float64 row into a character variable, v=", v
    end subroutine scenario_table_row_string_kind_mismatch

    !> The same for the sliced copy path: widening is the only conversion on offer.
    subroutine scenario_table_get_slice_kind_mismatch()
        type(parquet_table) :: t
        type(parquet_slice) :: s
        logical, allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_slice_kind.parquet")
        call parquet_open_table(t, "test_run/es_table_slice_kind.parquet")
        s = parquet_slice_range(1, 2)
        call t%get_slice("val", s, v)   ! float64 column into a logical array -> aborts
        print '(a,i0)', "unexpectedly sliced a float64 column into a logical array, size=", size(v)
    end subroutine scenario_table_get_slice_kind_mismatch

    !> Writes `lines` to `fname`, one per record -- the read-in (Role-B) MAML fixtures the remap
    !! scenarios below open a table with (parquet_open_table(maml=) takes a file path).
    subroutine write_scenario_maml_file(fname, lines)
        character(len=*), intent(in) :: fname    !! file to write.
        character(len=*), intent(in) :: lines(:) !! MAML source, one array element per line.
        integer :: unit, i
        open(newunit=unit, file=fname, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, "(a)") trim(lines(i))
        end do
        close(unit)
    end subroutine write_scenario_maml_file

    !> extra: remap: must name a column the file actually has. Naming one it does not is a plain
    !! mistake (a typo, or a MAML written against a different file), and it has to abort at open
    !! rather than silently producing a column that can never be read.
    subroutine scenario_table_remap_unknown_file_column()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_remap_unknown.parquet")
        call write_scenario_maml_file("test_run/es_table_remap_unknown.maml", [character(len=40) :: &
            "table: remap_unknown", &
            "extra:", &
            "  remap:", &
            "  - mass: not_a_real_column" ])
        call parquet_open_table(t, "test_run/es_table_remap_unknown.parquet", &
            maml="test_run/es_table_remap_unknown.maml")   ! -> aborts
        print '(a,i0)', "unexpectedly opened a table remapping a nonexistent file column, ncols=", t%ncols()
    end subroutine scenario_table_remap_unknown_file_column

    !> Two remap entries claiming the SAME internal name are ambiguous -- there is no rule that
    !! could pick between them -- so this aborts. Note the opposite case is deliberately ALLOWED:
    !! two internal names may target one file column (see parquet_tables_maml).
    subroutine scenario_table_remap_duplicate_internal()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_remap_dup.parquet")
        call write_scenario_maml_file("test_run/es_table_remap_dup.maml", [character(len=40) :: &
            "table: remap_dup", &
            "extra:", &
            "  remap:", &
            "  - mass: id", &
            "  - mass: val" ])
        call parquet_open_table(t, "test_run/es_table_remap_dup.parquet", &
            maml="test_run/es_table_remap_dup.maml")   ! -> aborts
        print '(a,i0)', "unexpectedly opened a table with a duplicated remap internal name, ncols=", t%ncols()
    end subroutine scenario_table_remap_duplicate_internal

    !> A code-declared qc bound the data violates aborts, exactly as a MAML-declared one does --
    !! qc= is not a softer kind of declaration, it just names its columns differently.
    !!
    !! Note WHERE it aborts: qc runs when a column is actually read, and opening a parquet_table
    !! reads no column data at all, so the abort lands on the first touch rather than on the open.
    !! That is the lazy table's normal behaviour applied to qc, not a weaker guarantee -- but it
    !! does mean a violated bound on a column a program never touches is never reported.
    subroutine scenario_table_qc_violation()
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer(int32), allocatable :: ids(:)
        call write_table_scenario_fixture("test_run/es_table_qc_violation.parquet")
        call qc%add("id, >=100")
        call parquet_open_table(t, "test_run/es_table_qc_violation.parquet", qc=qc)
        call t%get("id", ids)   ! -> aborts: first touch is where qc runs
        print '(a,i0)', "unexpectedly read a column whose data violates its qc bound, n=", size(ids)
    end subroutine scenario_table_qc_violation

    !> Sorting is not available in the slice regime: a sort reorders rows across the whole file,
    !! so [row_lo, row_hi] would name a different set of rows than the caller chose it to.
    !!
    !! A code-supplied `sort=` cannot even be written -- the slice specifics of parquet_open_table
    !! have no such argument, so it is a compile error rather than an abort. A MAML's own
    !! `extra: sort:` list is the case that has to be caught at runtime, which is this one. It
    !! aborts on the OPEN, before the file is read, unlike the qc scenario above.
    subroutine scenario_table_slice_maml_sort()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_slice_sort.parquet")
        call write_scenario_maml_file("test_run/es_table_slice_sort.maml", [character(len=40) :: &
            "table: slice_sort", &
            "extra:", &
            "  sort:", &
            '  - "id desc"' ])
        call parquet_open_table(t, "test_run/es_table_slice_sort.parquet", 1_int64, 2_int64, &
            maml="test_run/es_table_slice_sort.maml")   ! -> aborts
        print '(a,i0)', "unexpectedly opened a sorted slice-regime table, ncols=", t%ncols()
    end subroutine scenario_table_slice_maml_sort

    !> A filter naming a column the table does not have aborts at open. The message comes from the
    !! reader and names the column in FILE terms, since the internal name has already been
    !! translated by then -- for an unremapped column, which is the common case, the two are the
    !! same string, and for a remapped one the file name is what the caller has to fix anyway.
    subroutine scenario_table_filter_unknown_column()
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        call write_table_scenario_fixture("test_run/es_table_filter_unknown.parquet")
        call filt%add("not_a_real_column > 1")
        call parquet_open_table(t, "test_run/es_table_filter_unknown.parquet", filter=filt)  ! -> aborts
        print '(a,i0)', "unexpectedly opened a table filtered on a nonexistent column, nrows=", t%nrows()
    end subroutine scenario_table_filter_unknown_column

    !> A lazy first touch inside a parallel region publishes shared state with no ordering
    !! behind it, so it is forbidden outright rather than guarded by a lock on the read path
    !! (RF4). Needs a real OpenMP build to trigger: without -fopenmp there is no region to be
    !! inside of, and the scenario simply reads the column and exits 0.
    subroutine scenario_table_first_touch_in_parallel()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        integer :: i
        call write_table_scenario_fixture("test_run/es_table_omp_touch.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_touch.parquet")
        !$omp parallel do default(shared) private(i, v)
        do i = 1, 2
            call t%get("val", v)   ! first touch inside the region -> aborts
        end do
        !$omp end parallel do
        print '(a)', "unexpectedly first-touched a column inside a parallel region"
    end subroutine scenario_table_first_touch_in_parallel

    !> %kind/%width resolve a deferred-width (plain LIST) column by calling table_resolve_width
    !! directly, NOT through table_touch -- so they need their own parallel-region guard, and
    !! their own scenario: table_first_touch_in_parallel above never reaches this branch, since
    !! its column is not deferred-width and table_touch's own guard (checked first) already
    !! aborts before table_resolve_width would ever run.
    subroutine scenario_table_resolve_width_in_parallel()
        type(parquet_table) :: t
        integer :: i, w
        call parquet_open_table(t, "test/fixtures/list_widths.parquet")
        !$omp parallel do default(shared) private(i, w)
        do i = 1, 2
            w = t%width("uniform")   ! deferred-width column, first resolve inside the region -> aborts
        end do
        !$omp end parallel do
        print '(a,i0)', "unexpectedly resolved a deferred column's width inside a parallel region, w=", w
    end subroutine scenario_table_resolve_width_in_parallel

    !> A deterministic (non-racy) sibling of table_first_touch_in_parallel above: exactly one
    !! thread performs the guarded first touch (via !$omp single), so it reliably runs the abort's
    !! own message-building lines rather than racing a second thread into the same abort machinery
    !! at once -- which is what makes the plain concurrency scenario above only "best-effort" (see
    !! run_error_scenarios.sh; two threads calling error stop simultaneously has been observed to
    !! SIGSEGV instead of cleanly aborting, discarding that run's coverage data entirely). A team
    !! of 2 is still requested so the region is genuinely "active" (omp_in_parallel() answers
    !! true even though only one thread ever reaches the guarded call) -- without -fopenmp this is
    !! just an ordinary first touch and exits 0, same fallback as the racy scenario above.
    subroutine scenario_table_first_touch_in_parallel_single()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_omp_touch_single.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_touch_single.parquet")
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%get("val", v)   ! sole first touch inside the region -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly first-touched a column inside a parallel region, size=", size(v)
    end subroutine scenario_table_first_touch_in_parallel_single

    !> The deterministic sibling of scenario_table_resolve_width_in_parallel, for the same reason
    !! scenario_table_first_touch_in_parallel_single is a deterministic sibling of
    !! scenario_table_first_touch_in_parallel above.
    subroutine scenario_table_resolve_width_in_parallel_single()
        type(parquet_table) :: t
        integer :: w
        call parquet_open_table(t, "test/fixtures/list_widths.parquet")
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        w = t%width("uniform")   ! sole first resolve inside the region -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly resolved a deferred column's width inside a parallel region, w=", w
    end subroutine scenario_table_resolve_width_in_parallel_single

    !> A structural change to a table another thread may be using is refused.
    !!
    !! The table is opened OUTSIDE the region and every column made resident there, so nothing
    !! here is a first touch -- this reaches the mutation guard specifically, not
    !! `unsafe_first_touch`. `!$omp single` for the same determinism reason as the `_single`
    !! scenarios above: exactly one thread runs the abort.
    subroutine scenario_table_mutate_shared_in_parallel()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_omp_mutate.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_mutate.parquet")
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%drop_column("val")   ! structural change to a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly dropped a column of a shared table in a region, ncols=", t%ncols()
    end subroutine scenario_table_mutate_shared_in_parallel

    !> %compact reallocates every resident column's storage, so it is refused on a shared table
    !! exactly as every other non-append mutation is.
    !!
    !! Worth its own scenario rather than being assumed to follow from
    !! `scenario_table_mutate_shared_in_parallel`: %compact is the ONLY procedure in
    !! `parquet_tables_mutate` that reallocates storage, so it is the only one there whose guard
    !! protects pointer stability rather than the row set, and nothing else exercises that.
    !! `test_table_private_mutation_allowed` (test/test_openmp.f90) is its negative control --
    !! without that, a guard that fired unconditionally would pass this scenario while making
    !! "reserve, fill, compact" unusable on a thread-private table.
    subroutine scenario_table_compact_shared_in_parallel()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_omp_compact.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_compact.parquet")
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%compact()   ! reallocates storage under another thread's pointers -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly compacted a shared table in a region, nrows=", t%nrows()
    end subroutine scenario_table_compact_shared_in_parallel

    !> %reserve takes a row count, so a negative one is a caller error rather than a no-op --
    !! silently treating it as 0 would hide a sign mistake in a computed argument.
    subroutine scenario_table_reserve_negative()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_reserve_neg.parquet")
        call parquet_open_table(t, "test_run/es_table_reserve_neg.parquet")
        call t%materialize_all()
        call t%reserve(-5)   ! not a row count -> aborts
        print '(a,i0)', "unexpectedly reserved a negative row count, nrows=", t%nrows()
    end subroutine scenario_table_reserve_negative

    !> Same reasoning as %reserve's own negative guard, on the column count: a negative capacity is
    !! a sign mistake in a computed argument, not a request to reserve nothing.
    !!
    !! The negative CONTROL is in the same scenario: a valid reserve first, so a guard that fired
    !! unconditionally would not reach the abort at all and this would fail rather than pass.
    subroutine scenario_table_reserve_columns_negative()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_reserve_cols_neg.parquet")
        call parquet_open_table(t, "test_run/es_table_reserve_cols_neg.parquet")
        call t%materialize_all()
        call t%reserve_columns(t%ncols() + 4)   ! the permitted case, first
        call t%reserve_columns(-5)              ! not a column count -> aborts
        print '(a,i0)', "unexpectedly reserved a negative column count, ncols=", t%ncols()
    end subroutine scenario_table_reserve_columns_negative

    !> Growing the slot array relocates every descriptor under any pointer another thread holds,
    !! which is exactly what %add_column and %compact are refused for -- so %reserve_columns is
    !! refused on a shared table too. Guarded at the same place, for the same reason.
    subroutine scenario_table_reserve_columns_shared_in_parallel()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_omp_reserve_cols.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_reserve_cols.parquet")
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%reserve_columns(64)   ! relocates descriptors under another thread -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly reserved columns on a shared table, ncols=", t%ncols()
    end subroutine scenario_table_reserve_columns_shared_in_parallel

    !> A direction token in the key string and a descending= argument say the same thing twice and
    !! can disagree, so giving both is refused for the whole call.
    !!
    !! The negative control is the first call: the same key string WITHOUT a token, with
    !! descending= given, must be accepted -- otherwise a guard that refused descending= outright
    !! would pass this scenario while breaking every ordinary call.
    subroutine scenario_table_key_direction_conflict()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)
        call write_table_scenario_fixture("test_run/es_table_key_conflict.parquet")
        call parquet_open_table(t, "test_run/es_table_key_conflict.parquet")
        call t%argsort_by("id", perm, descending=[.true.])   ! no token: permitted
        print '(a,i0)', "descending= without a direction token was accepted, rows=", size(perm)
        call t%argsort_by("-id", perm, descending=[.true.])  ! token AND descending= -> aborts
        print '(a,i0)', "unexpectedly accepted both a direction token and descending=", size(perm)
    end subroutine scenario_table_key_direction_conflict

    !> The same conflict, but with a key list long enough that quoting it whole would build a
    !! multi-kilobyte abort message. ifx's ERROR STOP runtime corrupts the heap past 8192 bytes,
    !! so the preview is clipped -- and the trailing "...'" is what proves the clip happened
    !! rather than the message merely being short by luck.
    !!
    !! Negative control first: the same conflict with a SHORT key list, whose preview must come
    !! back unclipped, so a preview that always clipped would fail here instead of passing.
    subroutine scenario_table_key_list_long_preview()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)
        character(len=*), parameter :: long_keys = &
            "id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id," // &
            "id,id,id,id,id,id,id,id,-val"
        call write_table_scenario_fixture("test_run/es_table_key_preview.parquet")
        call parquet_open_table(t, "test_run/es_table_key_preview.parquet")
        if (len(long_keys) <= 100) then
            print '(a)', "the long key list is not actually long; this scenario proves nothing"
            return
        end if
        call t%argsort_by("id", perm)   ! ordinary short list, no conflict: permitted
        print '(a,i0)', "a short key list was accepted, rows=", size(perm)
        call t%argsort_by(long_keys, perm, descending=[.true.])   ! conflict, long list -> aborts
        print '(a,i0)', "unexpectedly accepted a long conflicting key list", size(perm)
    end subroutine scenario_table_key_list_long_preview

    !> A key string that tokenizes to nothing is a caller mistake, not an empty sort: silently
    !! leaving the table alone would look like the sort succeeded.
    !!
    !! Negative control first: a list whose separators are the same but which does carry a name
    !! must be accepted, so a splitter that rejected all punctuation would fail here.
    subroutine scenario_table_key_list_empty()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_key_empty.parquet")
        call parquet_open_table(t, "test_run/es_table_key_empty.parquet")
        call t%sort_by(" id ; ")   ! same punctuation, one real name: permitted
        print '(a,i0)', "a key list with trailing separators was accepted, rows=", t%nrows()
        call t%sort_by(" , ; ")    ! no names at all -> aborts
        print '(a,i0)', "unexpectedly sorted by an empty key list, rows=", t%nrows()
    end subroutine scenario_table_key_list_empty

    !> An unparseable direction word must name the offending token, not the whole list -- the
    !! parser's own message is passed through for exactly that reason.
    !!
    !! Negative control first: the recognized spelling of the same shape must be accepted.
    subroutine scenario_table_key_list_bad_direction()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_key_baddir.parquet")
        call parquet_open_table(t, "test_run/es_table_key_baddir.parquet")
        call t%sort_by("id desc")      ! a recognized direction word: permitted
        print '(a,i0)', "a recognized direction word was accepted, rows=", t%nrows()
        call t%sort_by("id sideways")  ! not a direction word -> aborts
        print '(a,i0)', "unexpectedly sorted by an unrecognized direction, rows=", t%nrows()
    end subroutine scenario_table_key_list_bad_direction

    !> One missing name long enough that quoting it whole would start bloating the message. The
    !! preview clips each NAME at 64 characters, independently of how many names it shows.
    !!
    !! The unclipped control is `table_require_columns_missing`, which asserts a short name comes
    !! through whole -- so a preview that always clipped fails there while this one passes, and one
    !! that never clipped fails here. Neither scenario alone pins the behaviour.
    subroutine scenario_table_require_columns_long_name()
        type(parquet_table) :: t
        character(len=*), parameter :: long_name = &
            "a_column_name_long_enough_that_quoting_it_whole_would_bloat_the_message_0123456789"
        call write_table_scenario_fixture("test_run/es_table_require_long.parquet")
        call parquet_open_table(t, "test_run/es_table_require_long.parquet")
        if (len(long_name) <= 64) then
            print '(a)', "the long name is not actually long; this scenario proves nothing"
            return
        end if
        call t%require_columns("id,val")    ! everything present: permitted
        print '(a,i0)', "a request naming only present columns was accepted, ncols=", t%ncols()
        call t%require_columns(long_name)   ! missing and long -> aborts with a clipped preview
        print '(a)', "unexpectedly required a missing column"
    end subroutine scenario_table_require_columns_long_name

    !> More missing names than the preview shows. It lists the first ten and counts the rest, so
    !! the message stays bounded however many columns a caller asks for.
    !!
    !! Negative control first: a request with FEWER missing names than the cap must not be
    !! summarized at all -- `table_require_columns_missing` asserts that two missing names come
    !! through as names, with no "and N more" tail.
    subroutine scenario_table_require_columns_many_missing()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_require_many.parquet")
        call parquet_open_table(t, "test_run/es_table_require_many.parquet")
        call t%require_columns("id;val")   ! both present: permitted
        print '(a,i0)', "a request naming only present columns was accepted, ncols=", t%ncols()
        ! Twelve missing names: ten shown, two counted.
        call t%require_columns("m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12")   ! -> aborts
        print '(a)', "unexpectedly required twelve missing columns"
    end subroutine scenario_table_require_columns_many_missing

    !> %require_columns names EVERY missing column, which is the whole reason it exists -- a
    !! hand-written %has_column loop reports one per run.
    !!
    !! Negative control first: a request naming only columns that exist must pass straight through.
    subroutine scenario_table_require_columns_missing()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_require_cols.parquet")
        call parquet_open_table(t, "test_run/es_table_require_cols.parquet")
        call t%require_columns("id")            ! present: permitted
        print '(a)', "require_columns accepted a column that exists"
        call t%require_columns("id,nope,alsonope")  ! two missing -> aborts naming both
        print '(a,i0)', "unexpectedly required missing columns, ncols=", t%ncols()
    end subroutine scenario_table_require_columns_missing

    !> %add_column reallocates cols(:), so it is guarded at table_new_slot -- the choke point every
    !! per-kind specific goes through, which is why one scenario covers all 18 of them.
    subroutine scenario_table_add_column_shared_in_parallel()
        type(parquet_table) :: t
        real(real64) :: extra(3)
        call write_table_scenario_fixture("test_run/es_table_omp_addcol.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_addcol.parquet")
        call t%materialize_all()
        extra = [1.0_real64, 2.0_real64, 3.0_real64]
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%add_column("extra", extra)   ! reallocates cols(:) on a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly added a column to a shared table in a region, ncols=", t%ncols()
    end subroutine scenario_table_add_column_shared_in_parallel
    !
    !> Asking a SHARED table for `parquet_row_index` for the first time inside a parallel region.
    !!
    !! The name resolves to a column that does not exist yet, so the first request MATERIALIZES
    !! it -- which adds a slot and reallocates `cols(:)`, exactly as `%add_column` would. It is
    !! refused for that reason, and the message has to say so in the caller's own terms: it names
    !! the procedure the user actually called (`get` here) and the column, rather than reporting
    !! `add_column`, which appears nowhere in their code.
    !!
    !! The negative control is inside the same process: the row index is asked for BEFORE the
    !! region on a second table, proving the refusal is about sharing rather than about the name.
    subroutine scenario_table_row_index_shared_in_parallel()
        type(parquet_table) :: t, early
        integer(int64), allocatable :: got(:)
        character(len=*), parameter :: src = "test_run/es_table_omp_rowindex.parquet"
        call write_table_scenario_fixture(src)
        ! Control: the same request, before any region, must succeed.
        call parquet_open_table(early, src)
        call early%get(PARQUET_ROW_INDEX, got)
        print '(a,i0)', "row index materialized before the region, rows=", size(got)
        call parquet_open_table(t, src)
        call t%materialize_all()
        ! Both requests live in ONE `block`, because `!$omp single` takes a structured block --
        ! the same constraint scenario_table_write_shared_in_parallel documents.
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        block
            type(parquet_table) :: mine        ! block-local, NOT private() -- see CLAUDE.md
            integer(int64), allocatable :: mine_rows(:)
            ! Second negative control, and the one that matters: this thread opened `mine` itself
            ! inside the region, so it is thread-private and materializing its row index is
            ! permitted. A guard rewritten to key on `omp_in_parallel()` rather than on ownership
            ! would refuse this too and still pass every abort assertion (feature_risks.md
            ! Risk-134).
            call parquet_open_table(mine, src)
            call mine%get(PARQUET_ROW_INDEX, mine_rows)
            print '(a,i0)', "thread-private row index inside the region succeeded, rows=", size(mine_rows)
            ! `t` was opened OUTSIDE the region, so it may be shared -> aborts.
            call t%get(PARQUET_ROW_INDEX, got)
        end block
        !$omp end single
        !$omp end parallel
        print '(a)', "unexpectedly materialized the row index on a shared table inside a region"
    end subroutine scenario_table_row_index_shared_in_parallel
    !
    !> parquet_write_table on a SHARED table inside a parallel region aborts.
    !!
    !! A write counts as a structural change rather than a read because `release=` evicts each
    !! column once it has been written, so another thread reading the same table would have storage
    !! pulled out from under it. `doc/pages/operating/thread-safety.md` lists `parquet_write_table`
    !! among the calls a shared table refuses, and `doc/pages/tables/table-write.md` now says so on
    !! the page too.
    !!
    !! **The negative control is in this same process, and it has to be**: `table_check_not_shared`
    !! keys on OWNERSHIP, not on `omp_in_parallel()`, so a guard that fired whenever a region was
    !! open would pass an abort test while breaking every legitimate write. The scenario therefore
    !! writes a table the SAME thread opened inside the region first -- which must succeed and
    !! prints a marker the wrapper checks for -- and only then writes the shared one. Without the
    !! marker, "it aborted" is equally consistent with a guard that refuses everything.
    subroutine scenario_table_write_shared_in_parallel()
        type(parquet_table) :: shared
        character(len=*), parameter :: src = "test_run/es_table_omp_write.parquet"
        character(len=*), parameter :: out = "test_run/es_table_omp_write_out.parquet"
        character(len=*), parameter :: out2 = "test_run/es_table_omp_write_shared.parquet"
        call write_table_scenario_fixture(src)
        call parquet_open_table(shared, src)
        call shared%materialize_all()
        ! Both writes live in ONE `block`, because `!$omp single` takes a structured block: with
        ! `block` as the first statement of two, gfortran pairs the directive with the block
        ! construct alone and then rejects the trailing `!$omp end single`.
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        block
            type(parquet_table) :: mine        ! block-local, NOT private() -- see CLAUDE.md
            ! Negative control: this thread opened `mine` itself inside the region, so it is
            ! thread-private and the write is permitted.
            call parquet_open_table(mine, src)
            call mine%materialize_all()
            call parquet_write_table(mine, out)
            print '(a)', "private write inside the region succeeded"
            ! `shared` was opened OUTSIDE the region, so it may be shared -> aborts.
            call parquet_write_table(shared, out2)
        end block
        !$omp end single
        !$omp end parallel
        print '(a)', "unexpectedly wrote a shared table from inside a parallel region"
    end subroutine scenario_table_write_shared_in_parallel

    !> Nulling an element of a column with no validity storage yet ALLOCATES that storage, and two
    !! threads doing it race with no diagnostic. Refused, naming %ensure_validity -- which is the
    !! way to make the allocation happen up front so concurrent nulling is safe.
    subroutine scenario_table_set_null_no_validity_in_parallel()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_omp_setnull.parquet")
        call parquet_open_table(t, "test_run/es_table_omp_setnull.parquet")
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%set_null("val", 1)   ! first null on a shared column -> would allocate -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,l1)', "unexpectedly nulled a shared column with no validity storage, is_null=", &
            t%is_null("val", 1)
    end subroutine scenario_table_set_null_no_validity_in_parallel

    !> An ordering comparison on a BOOLEAN column is rejected when the filter is parsed.
    !!
    !! `>`/`>=`/`<`/`<=` have no meaning on a boolean, and the row-group statistics screen has its
    !! own arm declining them -- but that arm is unreachable in practice, because this abort fires
    !! first, at open time, before a single row group is screened. This scenario is what makes the
    !! REAL behaviour testable (test/test_filter_screen.f90 says so where the missing test would
    !! otherwise be), and it pins the message, which names the column and the offending operators.
    subroutine scenario_filter_bool_ordering()
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_filter) :: filt
        logical :: flag(6)
        integer(int32) :: u(6)
        integer :: i
        character(len=*), parameter :: f = "test_run/es_filter_bool_ordering.parquet"
        do i = 1, 6
            flag(i) = i > 3
            u(i) = i
        end do
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "flag", flag)
        call parquet_write_column(w, "u", u)
        call parquet_close_writer(w)
        call filt%add("flag > false")
        call parquet_open_reader(r, f, filter=filt)   ! -> aborts
        print '(a)', "unexpectedly opened a reader with an ordering comparison on a boolean column"
        call parquet_close_reader(r)
    end subroutine scenario_filter_bool_ordering

    !> Reading a table while another thread is appending to it is refused.
    !!
    !! **No threads here, deliberately.** The guard fires on whatever thread finds the counter
    !! non-zero, so `parquet_debug_table_set_inflight` -- the test-only hook that exists for exactly
    !! this (feature_risks.md Risk-6) -- makes the abort deterministic on one thread. Provoking it
    !! for real would need two threads to overlap on demand, and a timing-dependent scenario is
    !! worse than none: it passes on a quiet machine, fails on a busy one, and gets disabled.
    !!
    !! The successful read BEFORE the hook is set is the negative control, and it is load-bearing: a
    !! guard that fired unconditionally would produce this same stderr, so without it the scenario
    !! would pass against a guard that makes every read on every table abort.
    subroutine scenario_table_read_during_append()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_table_scenario_fixture("test_run/es_table_read_during_append.parquet")
        call parquet_open_table(t, "test_run/es_table_read_during_append.parquet")
        call t%materialize_all()
        call t%get("val", v)        ! negative control: no append in flight, so this must work
        print '(a,i0)', "control read succeeded with no append in flight, n=", size(v)
        call parquet_debug_table_set_inflight(t, appending=.true.)
        call t%get("val", v)        ! -> aborts
        print '(a,i0)', "unexpectedly read a table while an append was in flight, n=", size(v)
    end subroutine scenario_table_read_during_append

    !> Appending to a table while another thread is reading it is refused -- the other direction of
    !! the same contract, and the one that would corrupt the READER: an append reallocates every
    !! column's storage out from under it. Same hook, same negative control, same reason.
    subroutine scenario_table_append_during_read()
        type(parquet_table) :: t, batch
        call write_table_scenario_fixture("test_run/es_table_append_during_read.parquet")
        call parquet_open_table(t, "test_run/es_table_append_during_read.parquet")
        call t%materialize_all()
        call t%clone_structure(batch)
        call batch%append_null_rows(1)
        call parquet_debug_table_set_inflight(t, reading=.false.)
        call t%append(batch)        ! negative control: no read in flight, so this must work
        print '(a,i0)', "control append succeeded with no read in flight, nrows=", t%nrows()
        call parquet_debug_table_set_inflight(t, reading=.true.)
        call t%append(batch)        ! -> aborts
        print '(a,i0)', "unexpectedly appended to a table while a read was in flight, nrows=", t%nrows()
    end subroutine scenario_table_append_during_read

    !> A string column's rows share one packed store, so writing any element can move the whole
    !! payload -- "disjoint row ranges" is not a meaningful division of it, and any write to one on
    !! a shared table is refused whether or not it creates a null.
    subroutine scenario_table_string_write_shared_in_parallel()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=8) :: names(3)
        names = ["aa      ", "bbb     ", "c       "]
        call parquet_open_writer(w, "test_run/es_table_omp_strwrite.parquet")
        call parquet_write_column(w, "name", names)
        call parquet_close_writer(w)
        call parquet_open_table(t, "test_run/es_table_omp_strwrite.parquet")
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%set_element("name", 1, "zz")   ! packed store on a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a)', "unexpectedly wrote a string element of a shared table inside a region"
    end subroutine scenario_table_string_write_shared_in_parallel

    !> A generated table type declares its columns up front, so one the file does not have is a
    !! mismatch between schema and data -- not something to discover at the first accessor call.
    !! The message points at `source: computed`, which is how a column the program fills is
    !! declared.
    subroutine scenario_table_bind_missing_column()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_bind_missing.parquet")
        call parquet_open_table(t, "test_run/es_table_bind_missing.parquet")
        ! "id" and "val" exist; "flux" does not -> aborts
        call t%bind_predefined([character(len=4) :: "id", "flux"], [PK_INT32, PK_FLOAT64], &
            [1, 1], [.true., .true.], context="es_bind.maml")
        print '(a,i0)', "unexpectedly bound a predefined column the file lacks, ncols=", t%ncols()
    end subroutine scenario_table_bind_missing_column

    !> A declared col_size that disagrees with the file means the schema and the data describe
    !! different columns. Checked BEFORE the kind conversion, so the message names the real
    !! problem rather than a conversion that was never the point.
    subroutine scenario_table_bind_width_mismatch()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_bind_width.parquet")
        call parquet_open_table(t, "test_run/es_table_bind_width.parquet")
        ! "id" is a scalar column; declaring col_size 3 for it -> aborts
        call t%bind_predefined([character(len=2) :: "id"], [PK_INT32_VEC], [3], [.true.], &
            context="es_bind.maml")
        print '(a,i0)', "unexpectedly bound a predefined column of the wrong width, ncols=", t%ncols()
    end subroutine scenario_table_bind_width_mismatch

    !> Only the numeric kinds convert into one another. Declaring a numeric kind over a string
    !! column is a schema mistake, and the message says which schema, since that is the half the
    !! user can edit.
    subroutine scenario_table_bind_kind_refused()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=4) :: names(3)
        names = ["a   ", "bcd ", "ef  "]
        call parquet_open_writer(w, "test_run/es_table_bind_kind.parquet")
        call parquet_write_column(w, "label", names)
        call parquet_close_writer(w)
        call parquet_open_table(t, "test_run/es_table_bind_kind.parquet")
        call t%bind_predefined([character(len=5) :: "label"], [PK_FLOAT64], [1], [.true.], &
            context="es_bind.maml")
        print '(a,i0)', "unexpectedly bound a string column as float64, ncols=", t%ncols()
    end subroutine scenario_table_bind_kind_refused

    !> A predefined column is one a generated accessor exists for, so dropping it silently would
    !! leave that accessor failing later, far from the cause. It needs force=.true. -- see
    !! test_table.f90's own negative control, which proves the guard does not simply always fire.
    subroutine scenario_table_drop_predefined()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_table_drop_predef.parquet")
        call parquet_open_table(t, "test_run/es_table_drop_predef.parquet")
        call t%bind_predefined([character(len=2) :: "id"], [PK_INT32], [1], [.true.], &
            context="es_bind.maml")
        call t%drop_column("id")   ! predefined, no force= -> aborts
        print '(a,i0)', "unexpectedly dropped a predefined column, ncols=", t%ncols()
    end subroutine scenario_table_drop_predefined

    !> Writes the fixture the generated-table scenarios open, matching table_types/maml_example4.maml's
    !! file columns. Kept minimal: these scenarios are about the guards, not the data.
    subroutine write_codegen_scenario_fixture(fname, crd_float64)
        character(len=*), intent(in) :: fname !! file to write.
        !> Present: store `crd` as float64 carrying these values, instead of the float32 the
        !! schema declares. That is what makes the declared narrowing lossy, for the `exact=`
        !! scenarios; every other caller omits it and gets a file matching the schema exactly.
        real(real64), intent(in), optional :: crd_float64(:,:)
        type(parquet_writer) :: w
        integer(int64) :: uberid(3)
        integer(int32) :: idx(3), counts(2, 3)
        logical :: flag(3), passed(2, 3)
        character(len=5) :: name(3)
        character(len=4) :: tags(2, 3)
        real(real64) :: ra(3), dec(3)
        real(real32) :: crd(3, 3)
        type(parquet_date) :: obsdate(3)
        type(parquet_time) :: obstime(3)
        type(parquet_timestamp) :: obsstamp(3)
        integer :: i, e
        do i = 1, 3
            uberid(i) = int(i, int64)
            idx(i) = i
            flag(i) = mod(i, 2) == 1
            ra(i) = real(i, real64)
            dec(i) = real(i, real64)
            obsdate(i) = parquet_date(2026, 3, i)
            obstime(i) = parquet_time(10, 20, i)
            obsstamp(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            do e = 1, 3
                crd(e, i) = real(e, real32)
            end do
            do e = 1, 2
                counts(e, i) = e
                passed(e, i) = mod(e, 2) == 0
            end do
        end do
        name = ["a    ", "bcd  ", "ef   "]
        tags(1, :) = "a"
        tags(2, :) = "bcde"
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "uberid", uberid)
        call parquet_write_column(w, "idx", idx)
        call parquet_write_column(w, "flag", flag)
        call parquet_write_column(w, "name", name)
        call parquet_write_column(w, "ra", ra)
        call parquet_write_column(w, "dec", dec)
        if (present(crd_float64)) then
            call parquet_write_column(w, "crd", crd_float64)
        else
            call parquet_write_column(w, "crd", crd)
        end if
        call parquet_write_column(w, "counts", counts)
        call parquet_write_column(w, "passed", passed)
        call parquet_write_column(w, "obsdate", obsdate)
        call parquet_write_column(w, "obstime", obstime)
        call parquet_write_column(w, "obsstamp", obsstamp)
        call parquet_write_column(w, "tags", tags)
        call parquet_close_writer(w)
    end subroutine write_codegen_scenario_fixture

    !> An indexed accessor returns a POINTER to one element, so an out-of-range index would be
    !! undefined behaviour rather than a wrong answer. The generated guard must abort first.
    !> `%reindex_trusted` skips the O(n) contents scan but NOT the O(1) length check: a
    !! permutation of the wrong length would make the gather read outside the column, and no
    !! promise from the caller can make that defined.
    subroutine scenario_reindex_trusted_length_mismatch()
        type(parquet_column) :: c
        integer(int64) :: perm(2) = [2_int64, 1_int64]
        integer(int32) :: got
        call c%init(PK_INT32, 3_int64)
        call c%set_all([10_int32, 20_int32, 30_int32])
        call c%reindex_trusted(perm)   ! 2 entries for a 3-row column -> aborts
        call c%get_at(1_int64, got)
        print '(a,i0)', "unexpectedly reindexed with a short permutation, value=", got
    end subroutine scenario_reindex_trusted_length_mismatch

    !> The same guard reached through `pf_permute(..., assume_valid=.true.)`, which is the other
    !! supported way into the trusted path. Before the length check was split out of the contents
    !! check, this read past the end of `values` instead of aborting.
    subroutine scenario_permute_assume_valid_short_perm()
        integer(int32) :: v(4) = [1_int32, 2_int32, 3_int32, 4_int32]
        integer(int32) :: perm(2) = [2_int32, 1_int32]
        call pf_permute(v, perm, assume_valid=.true.)   ! 2 entries for 4 values -> aborts
        print '(a,i0)', "unexpectedly permuted with a short permutation, first=", v(1)
    end subroutine scenario_permute_assume_valid_short_perm

    subroutine scenario_codegen_row_index_out_of_range()
        type(parquet_table_test) :: t
        real(real64), pointer :: p
        call write_codegen_scenario_fixture("test_run/es_codegen_row.parquet")
        call t%init("test_run/es_codegen_row.parquet")
        p => t%ra(99)   ! the fixture has 3 rows -> aborts
        print '(a,f8.3)', "unexpectedly aliased an out-of-range row, value=", p
    end subroutine scenario_codegen_row_index_out_of_range

    !> The same guard for the range form.
    subroutine scenario_codegen_range_out_of_range()
        type(parquet_table_test) :: t
        real(real64), pointer :: p(:)
        call write_codegen_scenario_fixture("test_run/es_codegen_range.parquet")
        call t%init("test_run/es_codegen_range.parquet")
        p => t%ra(2, 99)   ! the fixture has 3 rows -> aborts
        print '(a,i0)', "unexpectedly aliased an out-of-range row range, size=", size(p)
    end subroutine scenario_codegen_range_out_of_range

    !> A generated type declares its columns up front, so opening it on a file that lacks one is a
    !! mismatch between schema and data -- caught by %init, not by the first accessor call.
    !> `%init(exact=.true.)` refuses a value that would not survive the declared kind conversion.
    !!
    !! `maml_example4.maml` declares `crd` as float32; writing it as float64 with a value no
    !! float32 can represent makes the narrowing lossy for that value in particular. Without
    !! `exact=` the same file opens with a warning, which is what
    !! `codegen_init_exact_control` asserts -- and that control is the whole point: an abort test
    !! alone would pass just as happily against an `%init` that refused every narrowing, or that
    !! refused this file for some entirely different reason.
    subroutine scenario_codegen_init_exact_refuses()
        type(parquet_table_test) :: t
        call write_codegen_exact_fixture("test_run/es_codegen_exact.parquet")
        call t%init("test_run/es_codegen_exact.parquet", exact=.true.)   ! -> aborts
        print '(a,i0)', "unexpectedly opened with exact=.true. over a lossy value, ncols=", t%ncols()
    end subroutine scenario_codegen_init_exact_refuses

    !> The negative control for `codegen_init_exact_refuses`: the same file, the same declaration,
    !! no `exact=`. Must warn and succeed, exit 0.
    subroutine scenario_codegen_init_exact_control()
        type(parquet_table_test) :: t
        real(real32), pointer :: p(:,:)
        call write_codegen_exact_fixture("test_run/es_codegen_exact_ctl.parquet")
        call t%init("test_run/es_codegen_exact_ctl.parquet")
        p => t%crd()
        print '(a,i0,a,i0)', "opened without exact=: ncols=", t%ncols(), " width=", size(p, 1)
    end subroutine scenario_codegen_init_exact_control

    !> Writes the fixture both `exact=` scenarios open: `maml_example4.maml`'s columns, but with
    !! `crd` stored as float64 carrying a value no float32 can hold exactly.
    subroutine write_codegen_exact_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        real(real64) :: crd64(3, 3)
        integer :: i, e
        do i = 1, 3
            do e = 1, 3
                crd64(e, i) = 1.0000000001_real64 * real(e, real64)
            end do
        end do
        call write_codegen_scenario_fixture(fname, crd_float64=crd64)
    end subroutine write_codegen_exact_fixture

    !> A generated table writes its `source: computed` column out like any other, so the file it
    !! produces has a column the schema declares as NOT coming from the file -- and `%init` on that
    !! file aborts. Documented on doc/pages/utilities/generated-tables.md under "Writing one out".
    !!
    !! **If this refusal is ever lifted** -- by teaching `%bind_predefined` to adopt an existing
    !! column whose kind and width match the declaration -- this scenario does not simply get
    !! deleted: it becomes an in-process test asserting that the round trip SUCCEEDS and that
    !! `flux`'s values survived it, and the page's "Writing one out" section has to change with it.
    subroutine scenario_codegen_computed_roundtrip()
        type(parquet_table_test) :: t
        call t%init_empty(3)
        call t%set("uberid", [1_int64, 2_int64, 3_int64])
        call parquet_write_table(t, "test_run/es_codegen_roundtrip.parquet", overwrite=.true.)
        call t%init("test_run/es_codegen_roundtrip.parquet")   ! the file now HAS flux -> aborts
        print '(a,i0)', "unexpectedly reopened a generated table's own output, ncols=", t%ncols()
    end subroutine scenario_codegen_computed_roundtrip

    subroutine scenario_codegen_missing_file_column()
        type(parquet_table_test) :: t
        type(parquet_writer) :: w
        integer(int32) :: idx(3)
        idx = [1_int32, 2_int32, 3_int32]
        call parquet_open_writer(w, "test_run/es_codegen_missing.parquet")
        call parquet_write_column(w, "idx", idx)
        call parquet_close_writer(w)
        call t%init("test_run/es_codegen_missing.parquet")   ! no "uberid" column -> aborts
        print '(a,i0)', "unexpectedly opened a generated table on a file missing a column, ncols=", t%ncols()
    end subroutine scenario_codegen_missing_file_column

    ! ==== stage 3c: mutation, detach, sort, clone ==========================================

    !> Writes the shared 3c fixture: two scalar columns of three rows.
    subroutine write_mutate_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        call write_table_scenario_fixture(fname)
    end subroutine write_mutate_fixture

    !> THE detach message. Once a row-structural change has moved the rows, a column still in
    !! the file can never be lined up with the ones in memory, so reading it is refused rather
    !! than answered with misaligned data.
    subroutine scenario_table_detached_read_unmaterialized()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_mutate_fixture("test_run/es_tbl_det_read.parquet")
        call parquet_open_table(t, "test_run/es_tbl_det_read.parquet")
        call t%prefetch("id")   ! only "id" is resident; "val" was never read
        call t%truncate(2)      ! -> detaches, leaving "val" behind for good
        call t%get("val", v)    ! -> aborts
        print '(a,i0)', "unexpectedly read from a detached table, size=", size(v)
    end subroutine scenario_table_detached_read_unmaterialized

    !> The slice counterpart of table_detached_read_unmaterialized, and the case where two
    !! mechanisms meet: the mutation SKIPS the column it cannot see (which is what lets a lazy
    !! table drop rows at all), and detaching then rewrites the slice's own row scope away, so the
    !! skipped column can never be lined up with what is left. The abort is the only thing that
    !! reports the loss.
    subroutine scenario_table_slice_mutate_then_read()
        type(parquet_table) :: t
        real(real64), allocatable :: v(:)
        call write_mutate_fixture("test_run/es_tbl_slice_mut.parquet")
        call parquet_open_table(t, "test_run/es_tbl_slice_mut.parquet", 2_int64, 3_int64)
        call t%prefetch("id")     ! only "id" is resident; "val" was never read
        call t%delete_rows([1_int64])   ! -> mutates the SLICE, skips "val", detaches
        call t%get("val", v)      ! -> aborts
        print '(a,i0)', "unexpectedly read a slice's stranded column, size=", size(v)
    end subroutine scenario_table_slice_mutate_then_read

    subroutine scenario_table_detached_prefetch()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_det_pre.parquet")
        call parquet_open_table(t, "test_run/es_tbl_det_pre.parquet")
        call t%prefetch("id")
        call t%truncate(2)
        call t%prefetch("val")   ! -> aborts
        print '(a,i0)', "unexpectedly prefetched on a detached table, ncols=", t%ncols()
    end subroutine scenario_table_detached_prefetch

    subroutine scenario_table_detached_materialize_all()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_det_matall.parquet")
        call parquet_open_table(t, "test_run/es_tbl_det_matall.parquet")
        call t%prefetch("id")
        call t%truncate(2)
        call t%materialize_all()   ! -> aborts
        print '(a,i0)', "unexpectedly materialized a detached table, ncols=", t%ncols()
    end subroutine scenario_table_detached_materialize_all

    subroutine scenario_table_detached_reload()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_det_reload.parquet")
        call parquet_open_table(t, "test_run/es_tbl_det_reload.parquet")
        call t%materialize_all()
        call t%truncate(2)
        call t%reload("id")   ! -> aborts
        print '(a,i0)', "unexpectedly reloaded a column of a detached table, ncols=", t%ncols()
    end subroutine scenario_table_detached_reload

    !> Row-group bounds describe FILE rows, which a detached table no longer tracks.
    subroutine scenario_table_detached_row_group_bounds()
        type(parquet_table) :: t
        integer(int64), allocatable :: b(:,:)
        call write_mutate_fixture("test_run/es_tbl_det_rgb.parquet")
        call parquet_open_table(t, "test_run/es_tbl_det_rgb.parquet")
        call t%materialize_all()
        call t%truncate(2)
        call t%row_group_bounds(b)   ! -> aborts
        print '(a,i0)', "unexpectedly reported row-group bounds after detaching, n=", size(b, 2)
    end subroutine scenario_table_detached_row_group_bounds

    !> A sort leaves the table's own rows with no row-group structure at all -- row 5 can come
    !! from any row group -- so the default form of %row_group_bounds refuses. The refusal is the
    !! table's own: before it existed this surfaced from parquet_get_chunk_size, naming a
    !! procedure and a reader the caller never used. (physical=.true. still answers, and that half
    !! is tested in process -- test_row_group_bounds_sorted.)
    subroutine scenario_table_row_group_bounds_sorted()
        type(parquet_table) :: t
        type(parquet_sortkey) :: srt
        integer(int64), allocatable :: b(:,:)
        call write_table_scenario_fixture("test_run/es_tbl_rgb_sorted.parquet")
        call srt%add("-id")
        call parquet_open_table(t, "test_run/es_tbl_rgb_sorted.parquet", sort=srt)
        call t%row_group_bounds(b)   ! -> aborts
        print '(a,i0)', "unexpectedly reported row-group bounds for a sorted table, n=", size(b, 2)
    end subroutine scenario_table_row_group_bounds_sorted

    !> A column left unread when the table detached is unreadable for good, and %prefetch says
    !! so with the detach message rather than trying to read through a reader that is gone.
    subroutine scenario_table_mutate_unmaterialized_column()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_mut_unmat.parquet")
        call parquet_open_table(t, "test_run/es_tbl_mut_unmat.parquet")
        call t%truncate(2)      ! nothing was resident: every column is left behind
        call t%prefetch("id")   ! -> aborts
        print '(a,i0)', "unexpectedly read a column stranded by a detach, nrows=", t%nrows()
    end subroutine scenario_table_mutate_unmaterialized_column

    !> Sorting by a column whose type this library cannot read is refused: there are no values to
    !! order by, and silently treating every row as equal would be worse than saying so.
    subroutine scenario_table_mutate_unsupported_column()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        call t%sort_by(["map_col"])   ! a MAP column, unreadable -> aborts
        print '(a,i0)', "unexpectedly sorted by an unsupported column, nrows=", t%nrows()
    end subroutine scenario_table_mutate_unsupported_column

    subroutine scenario_table_filter_rows_mask_length()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_mask_len.parquet")
        call parquet_open_table(t, "test_run/es_tbl_mask_len.parquet")
        call t%materialize_all()
        call t%filter_rows([.true., .false.])   ! 3 rows, 2 entries -> aborts
        print '(a,i0)', "unexpectedly filtered with a short mask, nrows=", t%nrows()
    end subroutine scenario_table_filter_rows_mask_length

    subroutine scenario_table_delete_rows_out_of_range()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_del_range.parquet")
        call parquet_open_table(t, "test_run/es_tbl_del_range.parquet")
        call t%materialize_all()
        call t%delete_rows([1, 9])   ! only 3 rows -> aborts, with nothing deleted
        print '(a,i0)', "unexpectedly deleted a row that does not exist, nrows=", t%nrows()
    end subroutine scenario_table_delete_rows_out_of_range

    subroutine scenario_table_truncate_negative()
        type(parquet_table) :: t
        call write_mutate_fixture("test_run/es_tbl_trunc_neg.parquet")
        call parquet_open_table(t, "test_run/es_tbl_trunc_neg.parquet")
        call t%materialize_all()
        call t%truncate(-1)   ! -> aborts
        print '(a,i0)', "unexpectedly truncated to a negative row count, nrows=", t%nrows()
    end subroutine scenario_table_truncate_negative

    subroutine scenario_table_append_null_rows_negative()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%append_null_rows(-3)   ! -> aborts
        print '(a,i0)', "unexpectedly appended a negative number of rows, nrows=", t%nrows()
    end subroutine scenario_table_append_null_rows_negative

    subroutine scenario_table_sort_by_no_keys()
        type(parquet_table) :: t
        character(len=4) :: keys(0)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%sort_by(keys)   ! -> aborts
        print '(a,i0)', "unexpectedly sorted with no key at all, nrows=", t%nrows()
    end subroutine scenario_table_sort_by_no_keys

    !> descending=/nulls_first= take ONE entry per key; a shorter or longer array is a mistake
    !! that would otherwise be read past the end of.
    subroutine scenario_table_sort_by_flag_count_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%add_column("b", [1_int32, 2_int32])
        call t%sort_by(["a", "b"], descending=[.true.])   ! 2 keys, 1 flag -> aborts
        print '(a,i0)', "unexpectedly sorted with a short descending= list, nrows=", t%nrows()
    end subroutine scenario_table_sort_by_flag_count_mismatch

    !> The nulls_first= sibling of the descending= check above: its own guard, on its own
    !! source line, needs its own abort.
    subroutine scenario_table_sort_by_nulls_first_count_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%add_column("b", [1_int32, 2_int32])
        call t%sort_by(["a", "b"], nulls_first=[.true.])   ! 2 keys, 1 flag -> aborts
        print '(a,i0)', "unexpectedly sorted with a short nulls_first= list, nrows=", t%nrows()
    end subroutine scenario_table_sort_by_nulls_first_count_mismatch

    subroutine scenario_table_sort_by_unknown_column()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%sort_by(["nope"])   ! -> aborts
        print '(a,i0)', "unexpectedly sorted by a column that does not exist, nrows=", t%nrows()
    end subroutine scenario_table_sort_by_unknown_column

    !> There is no defined order on a whole vector row, so a *_VEC column cannot be a sort key.
    subroutine scenario_table_sort_by_vector_column()
        type(parquet_table) :: t
        real(real64) :: v(2, 3)
        v = 1.0_real64
        call parquet_new_table(t)
        call t%add_column("vv", v)
        call t%sort_by(["vv"])   ! -> aborts
        print '(a,i0)', "unexpectedly sorted by a vector column, nrows=", t%nrows()
    end subroutine scenario_table_sort_by_vector_column

    !> The refusals %argsort_by shares with %sort_by must name argsort_by, not sort_by -- they go
    !! through one helper, and a `proc` argument is the only thing keeping the message honest.
    subroutine scenario_table_argsort_by_vector_column()
        type(parquet_table) :: t
        real(real64) :: v(2, 3)
        integer(int64), allocatable :: perm(:)
        v = 1.0_real64
        call parquet_new_table(t)
        call t%add_column("vv", v)
        call t%argsort_by(["vv"], perm)   ! -> aborts
        print '(a,i0)', "unexpectedly argsorted by a vector column, n=", size(perm)
    end subroutine scenario_table_argsort_by_vector_column

    subroutine scenario_table_argsort_by_no_keys()
        type(parquet_table) :: t
        character(len=4) :: keys(0)
        integer(int64), allocatable :: perm(:)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%argsort_by(keys, perm)   ! -> aborts
        print '(a,i0)', "unexpectedly argsorted with no key, n=", size(perm)
    end subroutine scenario_table_argsort_by_no_keys

    !> group_nkeys counts key NAMES, so more than there are is a caller error rather than a clamp.
    subroutine scenario_table_argsort_by_group_nkeys_too_many()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:), go(:)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%argsort_by(["a"], perm, group_offsets=go, group_nkeys=2)   ! -> aborts
        print '(a,i0)', "unexpectedly grouped on more keys than were given, ngroups=", size(go) - 1
    end subroutine scenario_table_argsort_by_group_nkeys_too_many

    !> Zero groups nothing, so it is a mistake rather than a synonym for "all the keys".
    subroutine scenario_table_argsort_by_group_nkeys_zero()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:), go(:)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%argsort_by(["a"], perm, group_offsets=go, group_nkeys=0)   ! -> aborts
        print '(a,i0)', "unexpectedly grouped on zero keys, ngroups=", size(go) - 1
    end subroutine scenario_table_argsort_by_group_nkeys_zero

    !> On its own group_nkeys changes nothing, so passing it alone is an error, not a no-op --
    !! silently ignoring it would hide a caller who thought they had asked for boundaries.
    subroutine scenario_table_argsort_by_group_nkeys_without_offsets()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%argsort_by(["a"], perm, group_nkeys=1)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted group_nkeys with no group_offsets, n=", size(perm)
    end subroutine scenario_table_argsort_by_group_nkeys_without_offsets

    !> Too LARGE an n clamps; a negative one cannot mean anything and aborts.
    subroutine scenario_table_argsort_partial_negative_n()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%argsort_partial(["a"], perm, -1)   ! -> aborts
        print '(a,i0)', "unexpectedly ordered a negative number of rows, n=", size(perm)
    end subroutine scenario_table_argsort_partial_negative_n

    !> `%top_n` clamps too large an n and refuses a negative one -- and the message must name
    !! `top_n`, not the `argsort_partial` machinery underneath it. That is the whole reason
    !! `sort_partial_check_n` takes a procedure name, and passing the wrong one is invisible from
    !! inside the library.
    subroutine scenario_table_top_n_negative_n()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%top_n(["a"], -1)   ! -> aborts
        print '(a,i0)', "unexpectedly kept a negative number of rows, nrows=", t%nrows()
    end subroutine scenario_table_top_n_negative_n

    !> The other half of the naming check: this refusal comes from `sort_lookup_key` rather than
    !! `sort_partial_check_n`, so it proves the procedure name reaches the shared key validator too.
    subroutine scenario_table_top_n_unknown_column()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2_int32, 1_int32])
        call t%top_n(["nope"], 1)   ! -> aborts
        print '(a,i0)', "unexpectedly selected on a column that does not exist, nrows=", t%nrows()
    end subroutine scenario_table_top_n_unknown_column

    !> A column left in the file when `%top_n` drops rows can never be lined up with the rows in
    !! memory again, so reading it afterwards is a named error rather than silently wrong data.
    subroutine scenario_table_top_n_detached_read()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: f = "test_run/es_table_top_n_detach.parquet"
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "a", [3_int32, 1_int32, 2_int32])
        call parquet_write_column(w, "b", [30_int32, 10_int32, 20_int32])
        call parquet_close_writer(w)
        call parquet_open_table(t, f)
        call t%prefetch("a")
        call t%top_n(["a"], 2)
        call t%get("b", got)   ! -> aborts: "b" was never read, and the rows have moved
        print '(a,i0)', "unexpectedly read a column stranded by top_n, n=", size(got)
    end subroutine scenario_table_top_n_detached_read

    !> `%gather` permits repeats and any order, but an index outside the column is a bounds
    !! violation it must refuse rather than read past its own storage.
    subroutine scenario_column_gather_out_of_range()
        type(parquet_column) :: c
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call c%gather([1_int64, 9_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered a row outside the column, n=", c%length()
    end subroutine scenario_column_gather_out_of_range

    !> The string store's own gather is reachable directly, so it carries its own range check
    !! rather than relying on parquet_column's.
    subroutine scenario_string_column_gather_out_of_range()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%gather([2_int64, 5_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered an element outside the column, n=", col%size()
    end subroutine scenario_string_column_gather_out_of_range

    !> A column the appended table has and this one does not is never silently dropped.
    subroutine scenario_table_append_unknown_column()
        type(parquet_table) :: a, b
        call parquet_new_table(a)
        call a%add_column("x", [1_int32])
        call parquet_new_table(b)
        call b%add_column("x", [2_int32])
        call b%add_column("y", [3_int32])
        call a%append(b)   ! -> aborts: "y" would have nowhere to go
        print '(a,i0)', "unexpectedly appended a table with an extra column, nrows=", a%nrows()
    end subroutine scenario_table_append_unknown_column

    subroutine scenario_table_append_kind_mismatch()
        type(parquet_table) :: a, b
        call parquet_new_table(a)
        call a%add_column("x", [1_int32])
        call parquet_new_table(b)
        call b%add_column("x", [2.0_real64])
        call a%append(b)   ! -> aborts rather than silently widening
        print '(a,i0)', "unexpectedly appended a column of another kind, nrows=", a%nrows()
    end subroutine scenario_table_append_kind_mismatch

    subroutine scenario_table_append_width_mismatch()
        type(parquet_table) :: a, b
        real(real64) :: wide(3, 1), narrow(2, 1)
        wide = 1.0_real64
        narrow = 2.0_real64
        call parquet_new_table(a)
        call a%add_column("v", wide)
        call parquet_new_table(b)
        call b%add_column("v", narrow)
        call a%append(b)   ! -> aborts
        print '(a,i0)', "unexpectedly appended a vector column of another width, nrows=", a%nrows()
    end subroutine scenario_table_append_width_mismatch

    !> This library does not convert units, so concatenating "m/s" rows with "km/h" rows would
    !! make a column whose rows mean different things with nothing recording it.
    subroutine scenario_table_append_unit_mismatch()
        type(parquet_table) :: a, b
        call parquet_new_table(a)
        call a%add_column("speed", [1.0_real64], unit="m/s")
        call parquet_new_table(b)
        call b%add_column("speed", [2.0_real64], unit="km/h")
        call a%append(b)   ! -> aborts
        print '(a,i0)', "unexpectedly appended rows in another unit, nrows=", a%nrows()
    end subroutine scenario_table_append_unit_mismatch

    subroutine scenario_table_append_row_no_common_column()
        type(parquet_table) :: a, b
        type(parquet_table_row) :: r
        call parquet_new_table(a)
        call a%add_column("x", [1_int32])
        call parquet_new_table(b)
        call b%add_column("q", [7_int32])
        r = b%row(1)
        call a%append(r)   ! -> aborts: nothing in common to append
        print '(a,i0)', "unexpectedly appended a row with nothing in common, nrows=", a%nrows()
    end subroutine scenario_table_append_row_no_common_column

    subroutine scenario_table_set_element_row_out_of_range()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%set_element("a", 9, 5_int32)   ! -> aborts
        print '(a,i0)', "unexpectedly wrote past the last row, nrows=", t%nrows()
    end subroutine scenario_table_set_element_row_out_of_range

    subroutine scenario_table_set_element_kind_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%set_element("a", 1, 5.0_real64)   ! int32 column, float64 value -> aborts
        print '(a,i0)', "unexpectedly wrote a value of another kind, nrows=", t%nrows()
    end subroutine scenario_table_set_element_kind_mismatch

    subroutine scenario_table_set_null_row_out_of_range()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%set_null("a", 0)   ! -> aborts
        print '(a,i0)', "unexpectedly nulled row 0, nrows=", t%nrows()
    end subroutine scenario_table_set_null_row_out_of_range

    !> The ROW mask form of `%set_null` takes one entry per table row, and a mask of the wrong
    !! length is the mistake a caller makes after adding or deleting rows and reusing an old mask.
    !!
    !! Without the check the loop is bounded by `self%row_count` rather than by the mask, so a
    !! SHORT mask is read past its end -- unbounded on a build with no bounds checking, which a
    !! plain `fpm test` is -- and a LONG one silently ignores its tail. Both would quietly null
    !! the wrong rows. The in-range call first is the negative control.
    subroutine scenario_table_set_null_mask_wrong_length()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32])
        call t%set_null("a", [.true., .false., .true.])       ! right length: must be accepted
        call t%set_null("a", [.true., .false.])               ! one entry short -> aborts
        print '(a,i0)', "unexpectedly accepted a short validity mask, nrows=", t%nrows()
    end subroutine scenario_table_set_null_mask_wrong_length

    !> The ELEMENT mask form takes a `(width, rows)` mask, so it has two ways to be wrong and the
    !! message has to say which. This one is the transposed mask -- shaped `(rows, width)` -- which
    !! is the easy mistake to make and, on a square column, the one no shape check would catch.
    !! The fixture is deliberately NOT square (width 2, 3 rows) so the two shapes really differ.
    subroutine scenario_table_set_null_mask_wrong_shape()
        type(parquet_table) :: t
        real(real64) :: vv(2, 3)
        logical :: ok(2, 3), swapped(3, 2)
        vv = 1.0_real64
        ok = .true.
        swapped = .true.
        call parquet_new_table(t)
        call t%add_column("v", vv)
        call t%set_null("v", ok)        ! correctly shaped (width, rows): must be accepted
        call t%set_null("v", swapped)   ! transposed -> aborts
        print '(a,i0)', "unexpectedly accepted a transposed element mask, nrows=", t%nrows()
    end subroutine scenario_table_set_null_mask_wrong_shape

    subroutine scenario_table_rename_duplicate_name()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32])
        call t%add_column("b", [2_int32])
        call t%rename_column("a", "b")   ! -> aborts
        print '(a,i0)', "unexpectedly renamed onto an existing name, ncols=", t%ncols()
    end subroutine scenario_table_rename_duplicate_name

    subroutine scenario_table_rename_blank_name()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32])
        call t%rename_column("a", "   ")   ! -> aborts
        print '(a,i0)', "unexpectedly renamed a column to a blank name, ncols=", t%ncols()
    end subroutine scenario_table_rename_blank_name

    subroutine scenario_table_copy_unsupported_kind()
        type(parquet_table) :: t
        character(len=4) :: s(2)
        s = ["ab  ", "cd  "]
        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%copy_column("s", "si", PK_INT64)   ! strings do not cast -> aborts
        print '(a,i0)', "unexpectedly copied a string column, ncols=", t%ncols()
    end subroutine scenario_table_copy_unsupported_kind

    subroutine scenario_table_copy_duplicate_name()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32])
        call t%add_column("b", [2_int32])
        call t%copy_column("a", "b", PK_INT64)   ! -> aborts
        print '(a,i0)', "unexpectedly copied onto an existing column name, ncols=", t%ncols()
    end subroutine scenario_table_copy_duplicate_name

    !> Checked AFTER the duplicate-name guard (a blank name can never collide with an existing
    !! one, so that check alone would never catch it) and before any value is read.
    subroutine scenario_table_copy_blank_name()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32])
        call t%copy_column("a", "   ", PK_INT64)   ! -> aborts
        print '(a,i0)', "unexpectedly copied a column to a blank name, ncols=", t%ncols()
    end subroutine scenario_table_copy_blank_name

    !> A cast the caller asked for by name must not silently truncate: the whole column is
    !! checked before anything is written, so the table is left exactly as it was.
    subroutine scenario_table_copy_lossy_value()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1.5_real64, 2.0_real64])
        call t%copy_column("a", "ai", PK_INT32)   ! 1.5 is not a whole number -> aborts
        print '(a,i0)', "unexpectedly cast a value that cannot be represented, ncols=", t%ncols()
    end subroutine scenario_table_copy_lossy_value

    !> %cast is restricted to the kinds the reader and writer already convert between, so a
    !! string column has no conversion to offer and the request is refused outright.
    subroutine scenario_table_cast_non_numeric()
        type(parquet_table) :: t
        character(len=4) :: s(2)
        s = ["ab  ", "cd  "]
        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%cast("s", PK_INT64)   ! strings do not convert -> aborts
        print '(a,i0)', "unexpectedly cast a string column, kind=", t%kind("s")
    end subroutine scenario_table_cast_non_numeric

    !> Scalar and vector kinds differ in how many values a row holds, so converting one into the
    !! other is a reshape rather than a conversion and is refused.
    subroutine scenario_table_cast_rank_change()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%cast("a", PK_INT32_VEC)   ! -> aborts
        print '(a,i0)', "unexpectedly reshaped a column by casting, width=", t%width("a")
    end subroutine scenario_table_cast_rank_change

    !> An integer that does not fit the target width aborts rather than wrapping -- the same rule
    !! the reader and the writer both apply to an int64 narrowed to int32.
    subroutine scenario_table_cast_int_overflow()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int64, 3000000000_int64])
        call t%cast("a", PK_INT32)   ! 3e9 does not fit int32 -> aborts
        print '(a,i0)', "unexpectedly narrowed an out-of-range integer, kind=", t%kind("a")
    end subroutine scenario_table_cast_int_overflow

    !> A real with a fractional part converted to an integer kind aborts even under the default
    !! (lossy-allowed) rules: rounding a caller's data silently is never the intent.
    subroutine scenario_table_cast_fractional()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1.5_real64, 2.0_real64])
        call t%cast("a", PK_INT32)   ! 1.5 is not a whole number -> aborts
        print '(a,i0)', "unexpectedly cast a fractional value to an integer kind, kind=", t%kind("a")
    end subroutine scenario_table_cast_fractional

    !> A FINITE real64 too large for real32 aborts rather than quietly becoming infinity. Losing
    !! digits is what the default rules allow; losing the number itself is not.
    subroutine scenario_table_cast_float_overflow()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1.0_real64, 1.0e300_real64])
        call t%cast("a", PK_FLOAT32)   ! 1e300 overflows real32 -> aborts
        print '(a,i0)', "unexpectedly overflowed a float32 cast, kind=", t%kind("a")
    end subroutine scenario_table_cast_float_overflow

    !> exact=.true. additionally refuses the precision loss the default makes silently.
    !> `exact=` on an int32 -> real32 cast. An int32 above 2**24 has more significant bits than
    !! real32's 24-bit mantissa, so it comes back changed -- the integer direction of the same
    !! precision loss `table_cast_exact_precision` covers for real64 -> real32.
    !!
    !! Column `a` carries 2**24 itself, which real32 DOES hold exactly, and is cast first under
    !! the same `exact=.true.`: that is the negative control, and it must be a separate column
    !! rather than a round trip through the same one -- casting `b` down and back would round the
    !! offending value away and leave the scenario asserting nothing.
    !> `bind_predefined` takes four parallel arrays describing a generated type's columns. They are
    !! written by a code generator, so a length disagreement means the generator is out of step
    !! with itself -- and a bind that read past the shortest of them would attach a column to
    !! another column's kind, silently.
    !!
    !! A correctly-sized bind first is the negative control.
    subroutine scenario_table_bind_predefined_size_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%bind_predefined([character(len=4) :: "a", "b"], [PK_INT32, PK_INT64], [1, 1], &
            [.false., .false.])                                   ! consistent: must NOT abort
        print '(a,i0)', "bound two predefined columns, ncols=", t%ncols()
        call parquet_new_table(t)
        call t%bind_predefined([character(len=4) :: "a", "b"], [PK_INT32], [1, 1], &
            [.false., .false.])                                   ! kinds is one short -> aborts
        print '(a)', "unexpectedly accepted mismatched predefined array sizes"
    end subroutine scenario_table_bind_predefined_size_mismatch

    !> `units=` is optional, so its length is checked separately from the four required arrays --
    !! and therefore has its own way of going wrong.
    subroutine scenario_table_bind_predefined_units_size_mismatch()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%bind_predefined([character(len=4) :: "a", "b"], [PK_INT32, PK_INT64], [1, 1], &
            [.false., .false.], units=[character(len=2) :: "m", "s"])   ! matched: must NOT abort
        print '(a,i0)', "bound two predefined columns with units, ncols=", t%ncols()
        call parquet_new_table(t)
        call t%bind_predefined([character(len=4) :: "a", "b"], [PK_INT32, PK_INT64], [1, 1], &
            [.false., .false.], units=[character(len=2) :: "m"])        ! one unit for two -> aborts
        print '(a)', "unexpectedly accepted a units array of the wrong length"
    end subroutine scenario_table_bind_predefined_units_size_mismatch

    !> A `source: computed` field creates a column that has no file behind it, so the name must be
    !! free. Binding one whose name is already taken would otherwise either overwrite a column the
    !! table already holds or leave two columns answering to one name.
    !!
    !! The first bind is the negative control: the same declaration on a free name succeeds.
    !> Not an error scenario: a predefined column declared NARROWER than the file holds is a
    !! warning, not an abort. The declaration is a contract the file does not have to honour
    !! exactly, and a caller who declared the narrower kind may well have meant it -- so the bind
    !! proceeds and says so, rather than refusing.
    !!
    !! The warning is derived from the KIND PAIR before a byte is read, which is what keeps the
    !! single-pass decode the deferred cast exists for. The WIDENING bind first is the negative
    !! control: int32 -> int64 loses nothing, so it must stay silent.
    subroutine scenario_table_bind_predefined_lossy_warning()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=*), parameter :: out_file = "test_run/error_scenario_bind_lossy.parquet"

        call parquet_open_writer(w, out_file)
        call parquet_write_column(w, "narrow", [1_int32, 2_int32, 3_int32])
        call parquet_write_column(w, "wide", [1_int64, 2_int64, 3_int64])
        call parquet_close_writer(w)

        call parquet_open_table(t, out_file)
        ! int32 in the file, declared int64: a widening, so no warning.
        call t%bind_predefined([character(len=8) :: "narrow"], [PK_INT64], [1], [.true.])
        ! int64 in the file, declared int32: values may not fit -- warns, and still binds.
        call t%bind_predefined([character(len=8) :: "wide"], [PK_INT32], [1], [.true.])
        print '(a,i0)', "bound both columns; wide is now kind ", t%kind("wide")
    end subroutine scenario_table_bind_predefined_lossy_warning

    subroutine scenario_table_bind_predefined_computed_name_taken()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call t%bind_predefined([character(len=4) :: "b"], [PK_INT32], [1], [.false.])  ! free name
        print '(a,i0)', "bound a computed column on a free name, ncols=", t%ncols()
        call t%bind_predefined([character(len=4) :: "a"], [PK_INT32], [1], [.false.])  ! taken -> aborts
        print '(a)', "unexpectedly bound a computed column over an existing one"
    end subroutine scenario_table_bind_predefined_computed_name_taken

    !> Writing a scattered row selection with `is_valid=` pairs one validity entry with each
    !! SELECTED row, not with each row of the table. A mismatch means the caller built the mask
    !! against the wrong thing, and applying it anyway would null rows it never named.
    !!
    !! The matched call first is the negative control.
    subroutine scenario_table_set_rows_valid_length_mismatch()
        type(parquet_table) :: t
        type(parquet_slice) :: s
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32, 4_int32])
        s = parquet_slice_list([3, 1])
        call t%set_slice("a", s, [30_int32, 10_int32], is_valid=[.true., .false.])  ! matched
        print '(a,l1)', "wrote a two-row selection with a two-entry mask, row 1 null: ", t%is_null("a", 1_int64)
        call t%set_slice("a", s, [30_int32, 10_int32], is_valid=[.true., .false., .true.])
        print '(a)', "unexpectedly accepted an is_valid longer than the selection"
    end subroutine scenario_table_set_rows_valid_length_mismatch

    !> A rank-2 `is_valid=` on a whole-column write is per ELEMENT and must be shaped exactly
    !! (width, nrows). Checked on both extents rather than on the total, so a transposed mask is
    !! rejected instead of being applied the wrong way round.
    subroutine scenario_table_set_elem_valid_shape_mismatch()
        type(parquet_table) :: t
        integer(int32) :: v(2, 3)
        logical :: ok_mask(2, 3), bad_mask(3, 2)
        v = 1_int32
        call parquet_new_table(t)
        call t%add_column("v", v)
        ok_mask = .true.
        ok_mask(1, 2) = .false.
        call t%set("v", v, is_valid=ok_mask)          ! correctly shaped: must NOT abort
        print '(a,l1)', "wrote a (2,3) column with a (2,3) mask, element null: ", t%is_null("v", 2_int64, 1_int64)
        bad_mask = .true.
        call t%set("v", v, is_valid=bad_mask)         ! (3,2) for a (2,3) column -> aborts
        print '(a)', "unexpectedly accepted a transposed element mask"
    end subroutine scenario_table_set_elem_valid_shape_mismatch

    !> The two rules above at once: a scattered selection of a VECTOR column, whose `is_valid=` is
    !! shaped (width, selected rows). Its own check, because neither of the other two knows about
    !! both axes.
    subroutine scenario_table_set_rows_elem_valid_shape_mismatch()
        type(parquet_table) :: t
        type(parquet_slice) :: s
        integer(int32) :: v(2, 4), w(2, 2)
        logical :: ok_mask(2, 2), bad_mask(2, 3)
        v = 1_int32
        w = 5_int32
        call parquet_new_table(t)
        call t%add_column("v", v)
        s = parquet_slice_list([3, 1])
        ok_mask = .true.
        ok_mask(2, 1) = .false.
        call t%set_slice("v", s, w, is_valid=ok_mask)    ! (2,2) for a two-row selection: OK
        print '(a,l1)', "wrote a two-row vector selection, element null: ", t%is_null("v", 3_int64, 2_int64)
        bad_mask = .true.
        call t%set_slice("v", s, w, is_valid=bad_mask)   ! (2,3) for a two-row selection -> aborts
        print '(a)', "unexpectedly accepted an element mask wider than the selection"
    end subroutine scenario_table_set_rows_elem_valid_shape_mismatch

    !> A slice write pairs one value with each selected row. A length disagreement is the caller
    !! having built the array against a different selection, and writing what fits would put values
    !! on rows they were never meant for -- the values are all legal, so nothing downstream notices.
    subroutine scenario_table_set_slice_size_mismatch()
        type(parquet_table) :: t
        type(parquet_slice) :: s
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32, 3_int32, 4_int32])
        s = parquet_slice_list([3, 1])
        call t%set_slice("a", s, [30_int32, 10_int32])       ! two values, two rows: must NOT abort
        print '(a,i0)', "wrote a two-row selection, ncols=", t%ncols()
        call t%set_slice("a", s, [30_int32, 10_int32, 99_int32])  ! three values, two rows -> aborts
        print '(a)', "unexpectedly accepted an array longer than the selection"
    end subroutine scenario_table_set_slice_size_mismatch

    subroutine scenario_table_cast_exact_i32_to_f32()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 16777216_int32])
        call t%add_column("b", [1_int32, 16777217_int32])
        call t%cast("a", PK_FLOAT32, exact=.true.)   ! 2**24 itself survives: accepted
        call t%cast("b", PK_FLOAT32, exact=.true.)   ! 2**24 + 1 has no exact real32 form -> aborts
        print '(a,i0)', "unexpectedly accepted a lossy exact= int32 cast, kind=", t%kind("b")
    end subroutine scenario_table_cast_exact_i32_to_f32

    !> `exact=` on an int64 -> real32 cast: the same mantissa argument, from the wider integer.
    !! Checked through `int64_survives_real` rather than a round trip through real64, because an
    !! int64 large enough to matter cannot be compared by converting the real back.
    subroutine scenario_table_cast_exact_i64_to_f32()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int64, 16777216_int64])
        call t%add_column("b", [1_int64, 16777217_int64])
        call t%cast("a", PK_FLOAT32, exact=.true.)   ! 2**24 itself survives: accepted
        call t%cast("b", PK_FLOAT32, exact=.true.)   ! 2**24 + 1 -> aborts
        print '(a,i0)', "unexpectedly accepted a lossy exact= int64 -> real32 cast, kind=", t%kind("b")
    end subroutine scenario_table_cast_exact_i64_to_f32

    !> `exact=` on an int64 -> real64 cast. real64 has 53 mantissa bits, so this needs a much
    !! larger value than its real32 siblings -- and it is the one case where the DEFAULT rules
    !! silently lose an integer that a user is most likely to assume is safe, real64 being the
    !! widest kind on offer.
    subroutine scenario_table_cast_exact_i64_to_f64()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1_int64, 9007199254740992_int64])   ! 2**53
        call t%add_column("b", [1_int64, 9007199254740993_int64])   ! 2**53 + 1
        call t%cast("a", PK_FLOAT64, exact=.true.)   ! 2**53 itself survives: accepted
        call t%cast("b", PK_FLOAT64, exact=.true.)   ! 2**53 + 1 -> aborts
        print '(a,i0)', "unexpectedly accepted a lossy exact= int64 -> real64 cast, kind=", t%kind("b")
    end subroutine scenario_table_cast_exact_i64_to_f64

    !> A real32 column with a fractional value cast to an integer kind. `table_cast_fractional`
    !! covers the real64 source; this is the real32 one, which reaches a different checker --
    !! `chk_from_f32`, whose integer-target arm is the only branch it has.
    !!
    !! No `exact=` here on purpose: rounding a caller's data is refused under the DEFAULT rules,
    !! and the accepted whole-number cast first is what proves the arm is not simply refusing
    !! every real32 -> integer conversion.
    subroutine scenario_table_cast_f32_fractional()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [2.0_real32, 4.0_real32])
        call t%cast("a", PK_INT32)                   ! whole numbers: accepted
        call t%cast("a", PK_FLOAT32)
        call t%add_column("b", [1.5_real32, 2.0_real32])
        call t%cast("b", PK_INT64)                   ! 1.5 is not a whole number -> aborts
        print '(a,i0)', "unexpectedly cast a fractional real32 to an integer kind, kind=", t%kind("b")
    end subroutine scenario_table_cast_f32_fractional

    subroutine scenario_table_cast_exact_precision()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1.0_real64, 0.1_real64])
        call t%cast("a", PK_FLOAT32, exact=.true.)   ! 0.1 has no exact real32 form -> aborts
        print '(a,i0)', "unexpectedly accepted a lossy exact= cast, kind=", t%kind("a")
    end subroutine scenario_table_cast_exact_precision

    !> A column whose physical type the table layer cannot read holds no values to convert, and
    !! %cast has to say so itself: it looks the column up WITHOUT resolving it, precisely so that
    !! the deferred path can decide before any read happens.
    subroutine scenario_table_cast_unsupported_column()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/map_list_types.parquet")
        call t%cast("map_col", PK_FLOAT64)   ! -> aborts
        print '(a,i0)', "unexpectedly cast an unsupported column, ncols=", t%ncols()
    end subroutine scenario_table_cast_unsupported_column

    !> Cloning into a different table type would silently produce a copy without the extended
    !! type's own accessors, so it is refused.
    subroutine scenario_table_clone_type_mismatch()
        type(parquet_table) :: plain
        type(extended_table) :: ext
        call parquet_new_table(plain)
        call plain%add_column("a", [1_int32])
        call plain%clone(ext)   ! -> aborts
        print '(a,i0)', "unexpectedly cloned into another table type, ncols=", ext%ncols()
    end subroutine scenario_table_clone_type_mismatch

    ! ---- parquet_sorting (pf_argsort / pf_sort / pf_permute / pf_is_sorted) ----

    !> A rank outside 1..n names an element that does not exist.
    subroutine scenario_sorting_nth_out_of_range()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32) :: val
        call pf_nth_element(v, 9, val)   ! -> aborts (rank 9 of 4)
        print '(a,i0)', "unexpectedly resolved an out-of-range rank, value=", val
    end subroutine scenario_sorting_nth_out_of_range

    !> `n` past the end CLAMPS, deliberately -- but a negative n is a caller error, not a boundary.
    !! The clamping half is asserted in the ordinary suite; this is only the refusal.
    subroutine scenario_sorting_partial_negative_n()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32), allocatable :: sorted(:)
        ! The permitted neighbour first, as the negative control: without it a guard that fired
        ! unconditionally would pass this scenario just as happily.
        call pf_partial_sort(v, sorted, 99)
        print '(a,i0)', "clamped n=99 to size ", size(sorted)
        call pf_partial_sort(v, sorted, -1)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative n, size=", size(sorted)
    end subroutine scenario_sorting_partial_negative_n

    !> The quantile scale is 0-1, not 0-100 -- and 50 is a plausible thing for a caller to write.
    subroutine scenario_sorting_quantile_out_of_range()
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: q
        call pf_nth_quantile(v, 50.0_real64, q)   ! -> aborts (0-1 scale)
        print '(a,f0.3)', "unexpectedly accepted a 0-100 quantile, q=", q
    end subroutine scenario_sorting_quantile_out_of_range

    !> An unrecognized rounding token aborts naming the valid ones, rather than silently
    !! defaulting -- which is the whole reason a string selector is acceptable at all.
    subroutine scenario_sorting_quantile_bad_rounding()
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: q
        call pf_nth_quantile(v, 0.5_real64, q, rounding="downwards")   ! -> aborts
        print '(a,f0.3)', "unexpectedly accepted an unknown rounding token, q=", q
    end subroutine scenario_sorting_quantile_bad_rounding

    !> Every value null: there is no value to return, and no sentinel exists across all ten types.
    !! Returning an undefined p_value would be the silent-wrong-answer case this library refuses.
    subroutine scenario_sorting_quantile_all_null()
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        logical :: none(3) = [.false., .false., .false.]
        logical :: some(3) = [.true., .false., .false.]
        real(real64) :: q
        integer(int64) :: nn
        ! Negative control: a PARTIALLY null column still answers, and reports the null count.
        call pf_nth_quantile(v, 0.5_real64, q, is_valid=some, n_null=nn)
        print '(a,f0.3,a,i0)', "partial nulls answered q=", q, " n_null=", nn
        call pf_nth_quantile(v, 0.5_real64, q, is_valid=none)   ! -> aborts
        print '(a,f0.3)', "unexpectedly quantiled an all-null array, q=", q
    end subroutine scenario_sorting_quantile_all_null

    !> An index outside 1..n would read past the array being permuted. Caught before anything is
    !! written, so the array is never left half-rearranged.
    subroutine scenario_sorting_permute_index_out_of_range()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32) :: perm(4) = [1, 2, 9, 4]
        call pf_permute(v, perm)   ! -> aborts (9 is outside 1..4)
        print '(a,i0)', "unexpectedly permuted by an out-of-range index, v(1)=", v(1)
    end subroutine scenario_sorting_permute_index_out_of_range

    !> A repeated index is the dangerous case: it is in range, so nothing crashes -- the array
    !! simply ends up with one element duplicated and another silently dropped. This is the whole
    !! reason pf_permute validates by default rather than trusting its caller.
    subroutine scenario_sorting_permute_duplicate_index()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32) :: perm(4) = [1, 2, 2, 4]
        call pf_permute(v, perm)   ! -> aborts (index 2 appears twice)
        print '(a,i0)', "unexpectedly permuted by a duplicated index, v(3)=", v(3)
    end subroutine scenario_sorting_permute_duplicate_index

    !> A permutation of the wrong length cannot describe this array at all.
    subroutine scenario_sorting_permute_length_mismatch()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32) :: perm(3) = [1, 2, 3]
        call pf_permute(v, perm)   ! -> aborts (3 indices for 4 values)
        print '(a,i0)', "unexpectedly permuted by a short permutation, v(1)=", v(1)
    end subroutine scenario_sorting_permute_length_mismatch

    !> An is_valid mask of the wrong length would silently mark the wrong rows null.
    subroutine scenario_sorting_valid_length_mismatch()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        logical :: ok(3) = [.true., .false., .true.]
        integer(int32), allocatable :: perm(:)
        call pf_argsort(v, perm, is_valid=ok)   ! -> aborts (3 flags for 4 values)
        print '(a,i0)', "unexpectedly sorted with a short is_valid mask, size=", size(perm)
    end subroutine scenario_sorting_valid_length_mismatch

    !> A pf_sort_keys whose keys describe different row counts cannot be applied to anything.
    subroutine scenario_sorting_keys_row_count_mismatch()
        type(pf_sort_keys) :: k
        integer(int32) :: a(4) = [1, 2, 3, 4]
        integer(int32) :: b(3) = [1, 2, 3]
        call k%add(a)
        call k%add(b)   ! -> aborts (3 rows where the first key has 4)
        print '(a,i0)', "unexpectedly combined keys of different lengths, nkeys=", k%nkeys_added()
    end subroutine scenario_sorting_keys_row_count_mismatch

    !> Sorting by an empty key list has no defined answer; returning the identity would quietly
    !! look like a successful sort.
    subroutine scenario_sorting_keys_empty()
        type(pf_sort_keys) :: k
        integer(int32), allocatable :: perm(:)
        call pf_argsort(k, perm)   ! -> aborts (no key added)
        print '(a,i0)', "unexpectedly sorted an empty key list, size=", size(perm)
    end subroutine scenario_sorting_keys_empty

    !> The same rejection, reached through pf_partial_argsort instead of pf_argsort. It is a
    !! SEPARATE generated body carrying its own copy of the guard, so scenario_sorting_keys_empty
    !! above says nothing about it; a partial argsort that quietly returned the first n identity
    !! indices would look like a successful sort of an unsorted array.
    subroutine scenario_sorting_partial_keys_empty()
        type(pf_sort_keys) :: k
        integer(int32), allocatable :: perm(:)
        call pf_partial_argsort(k, perm, 3)   ! -> aborts (no key added)
        print '(a,i0)', "unexpectedly partial-sorted an empty key list, size=", size(perm)
    end subroutine scenario_sorting_partial_keys_empty

    !> And once more for the int64 index form, which is a third copy of the same guard.
    subroutine scenario_sorting_partial_keys_empty_i64()
        type(pf_sort_keys) :: k
        integer(int64), allocatable :: perm(:)
        call pf_partial_argsort(k, perm, 3)   ! -> aborts (no key added)
        print '(a,i0)', "unexpectedly partial-sorted an empty key list, size=", size(perm)
    end subroutine scenario_sorting_partial_keys_empty_i64

    !> A vector column has no defined order on a whole row, so it cannot be a sort key -- the
    !! same rule parquet_table%sort_by applies, enforced here for a bare parquet_column.
    subroutine scenario_sorting_column_vector()
        type(parquet_column) :: col
        integer(int32), allocatable :: perm(:)
        call col%init(PK_INT32_VEC, 3_int64, width=2)
        call pf_argsort(col, perm)   ! -> aborts (a vector column cannot be a sort key)
        print '(a,i0)', "unexpectedly sorted a vector column, size=", size(perm)
    end subroutine scenario_sorting_column_vector

    !> **The worst failure this module could have**, and the reason the check is on by default: a
    !! binary search over unsorted input returns a plausible index with no abort and no symptom at
    !! all. The sorted call first is the negative control -- without it, a check that fired
    !! unconditionally would pass this scenario just as happily while making the feature unusable.
    subroutine scenario_sorting_search_unsorted()
        integer(int32) :: sorted_v(5) = [10, 20, 30, 40, 50]
        integer(int32) :: jumbled(5) = [10, 40, 20, 50, 30]
        integer(int32) :: pos
        call pf_lower_bound(sorted_v, 30_int32, pos)
        print '(a,i0)', "sorted input answered pos=", pos
        call pf_lower_bound(jumbled, 30_int32, pos)   ! -> aborts (not sorted)
        print '(a,i0)', "unexpectedly searched unsorted input, pos=", pos
    end subroutine scenario_sorting_search_unsorted

    !> A `character` target is compared at the ARRAY's element length, so a target carrying
    !! non-blank characters past that length has no exact answer -- it is refused rather than
    !! truncated into a different value. A target that FITS is the control.
    subroutine scenario_sorting_search_target_too_long()
        character(len=3) :: v(3) = ["aaa", "bbb", "ccc"]
        integer(int32) :: pos
        call pf_lower_bound(v, "bb", pos)
        print '(a,i0)', "a shorter target answered pos=", pos
        call pf_lower_bound(v, "bbbb", pos)   ! -> aborts (4 non-blank characters, 3 per element)
        print '(a,i0)', "unexpectedly searched with an over-long target, pos=", pos
    end subroutine scenario_sorting_search_target_too_long

    !> An unrecognized `method=` token aborts naming the valid ones, exactly as `rounding=` does --
    !! a string selector is only acceptable because an unknown value fails loudly.
    subroutine scenario_sorting_rank_bad_method()
        integer(int32) :: v(4) = [10, 20, 20, 30]
        integer(int32), allocatable :: r(:)
        call pf_rank(v, r, method="Dense")
        print '(a,i0)', "a known token answered, largest rank=", maxval(r)
        call pf_rank(v, r, method="modified competition")   ! -> aborts
        print '(a,i0)', "unexpectedly ranked with an unknown method, largest rank=", maxval(r)
    end subroutine scenario_sorting_rank_bad_method

    !> No value to return and no sentinel that works across nine types -- the same unanswerable
    !! question `pf_nth_quantile` refuses, answered the same way. A NaN counts as absent here too,
    !! which is why the aborting array holds one rather than being purely null.
    subroutine scenario_sorting_minmax_all_null()
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        real(real64) :: v(3)
        logical :: none(3) = [.false., .false., .true.]
        logical :: some(3) = [.true., .false., .false.]
        real(real64) :: lo, hi
        v(1) = 1.0_real64
        v(2) = 2.0_real64
        v(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        ! Negative control: a partially null array still answers.
        call pf_minmax(v, lo, hi, is_valid=some)
        print '(a,f0.3,a,f0.3)', "partial nulls answered lo=", lo, " hi=", hi
        call pf_minmax(v, lo, hi, is_valid=none)   ! -> aborts (the only non-null value is a NaN)
        print '(a,f0.3)', "unexpectedly reduced an all-null array, lo=", lo
    end subroutine scenario_sorting_minmax_all_null

    !> The same rejection for each of the OTHER nine value families. `pf_minmax` has one shared
    !! worker per family, each carrying its own copy of the guard, so the real64 scenario above
    !! says nothing about any of them -- and the failure a missing guard produces is not an abort
    !! but an ANSWER: whatever the engine left in the first slot, reported as a minimum.
    !!
    !! Every one of these makes a partially-null call FIRST, as the negative control: a guard that
    !! fired unconditionally would pass the abort half of the test while breaking the ordinary case.
    subroutine scenario_sorting_minmax_all_null_i32()
        integer(int32) :: v(3) = [1_int32, 2_int32, 3_int32]
        logical :: none(3) = [.false., .false., .false.]
        logical :: some(3) = [.true., .false., .false.]
        integer(int32) :: lo, hi
        call pf_minmax(v, lo, hi, is_valid=some)
        print '(a,i0,a,i0)', "partial nulls answered lo=", lo, " hi=", hi
        call pf_minmax(v, lo, hi, is_valid=none)   ! -> aborts (nothing left to reduce)
        print '(a,i0)', "unexpectedly reduced an all-null int32 array, lo=", lo
    end subroutine scenario_sorting_minmax_all_null_i32

    !> int64 counterpart of the scenario above.
    subroutine scenario_sorting_minmax_all_null_i64()
        integer(int64) :: v(3) = [1_int64, 2_int64, 3_int64]
        logical :: none(3) = [.false., .false., .false.]
        logical :: some(3) = [.true., .false., .false.]
        integer(int64) :: lo, hi
        call pf_minmax(v, lo, hi, is_valid=some)
        print '(a,i0,a,i0)', "partial nulls answered lo=", lo, " hi=", hi
        call pf_minmax(v, lo, hi, is_valid=none)   ! -> aborts (nothing left to reduce)
        print '(a,i0)', "unexpectedly reduced an all-null int64 array, lo=", lo
    end subroutine scenario_sorting_minmax_all_null_i64

    !> real32 counterpart of the scenario above.
    subroutine scenario_sorting_minmax_all_null_f32()
        real(real32) :: v(3) = [1.0_real32, 2.0_real32, 3.0_real32]
        logical :: none(3) = [.false., .false., .false.]
        logical :: some(3) = [.true., .false., .false.]
        real(real32) :: lo, hi
        call pf_minmax(v, lo, hi, is_valid=some)
        print '(a,f0.3,a,f0.3)', "partial nulls answered lo=", lo, " hi=", hi
        call pf_minmax(v, lo, hi, is_valid=none)   ! -> aborts (nothing left to reduce)
        print '(a,f0.3)', "unexpectedly reduced an all-null real32 array, lo=", lo
    end subroutine scenario_sorting_minmax_all_null_f32

    !> character counterpart of the scenario above.
    subroutine scenario_sorting_minmax_all_null_chr()
        character(len=2) :: v(3) = [character(len=2) :: "aa", "bb", "cc"]
        logical :: none(3) = [.false., .false., .false.]
        logical :: some(3) = [.true., .false., .false.]
        character(len=:), allocatable :: lo, hi
        call pf_minmax(v, lo, hi, is_valid=some)
        print '(a,a,a,a)', "partial nulls answered lo=", lo, " hi=", hi
        call pf_minmax(v, lo, hi, is_valid=none)   ! -> aborts (nothing left to reduce)
        print '(a,a)', "unexpectedly reduced an all-null character array, lo=", lo
    end subroutine scenario_sorting_minmax_all_null_chr

    !> parquet_date counterpart. A temporal element carries its own null, so the all-null array is
    !! simply one nothing has been written to -- which is also the shape a caller most easily
    !! reaches by accident.
    subroutine scenario_sorting_minmax_all_null_date()
        type(parquet_date) :: v(3), some(3), lo, hi
        call some(1)%set_raw(1000_int32)
        call pf_minmax(some, lo, hi)
        print '(a,i0)', "a partially null date array answered lo=", lo%raw()
        call pf_minmax(v, lo, hi)   ! -> aborts (every element is still null)
        print '(a,i0)', "unexpectedly reduced an all-null date array, lo=", lo%raw()
    end subroutine scenario_sorting_minmax_all_null_date

    !> parquet_time counterpart of the scenario above.
    subroutine scenario_sorting_minmax_all_null_time()
        type(parquet_time) :: v(3), some(3), lo, hi
        call some(1)%set_raw(2000_int64)
        call pf_minmax(some, lo, hi)
        print '(a,i0)', "a partially null time array answered lo=", lo%raw()
        call pf_minmax(v, lo, hi)   ! -> aborts (every element is still null)
        print '(a,i0)', "unexpectedly reduced an all-null time array, lo=", lo%raw()
    end subroutine scenario_sorting_minmax_all_null_time

    !> parquet_timestamp counterpart of the scenario above.
    subroutine scenario_sorting_minmax_all_null_ts()
        type(parquet_timestamp) :: v(3), some(3), lo, hi
        integer(int64) :: secs
        integer(int32) :: nanos
        call some(1)%set_raw(3000_int64, 7_int32)
        call pf_minmax(some, lo, hi)
        call lo%get_raw(secs, nanos)
        print '(a,i0)', "a partially null timestamp array answered lo=", secs
        call pf_minmax(v, lo, hi)   ! -> aborts (every element is still null)
        print '(a)', "unexpectedly reduced an all-null timestamp array"
    end subroutine scenario_sorting_minmax_all_null_ts

    !> parquet_string_column counterpart: the store owns its validity, so the nulls are appended.
    subroutine scenario_sorting_minmax_all_null_strcol()
        type(parquet_string_column) :: v, some
        character(len=:), allocatable :: lo, hi
        call some%append_string("aa")
        call some%append_null()
        call pf_minmax(some, lo, hi)
        print '(a,a)', "a partially null string column answered lo=", lo
        call v%append_null()
        call v%append_null()
        call pf_minmax(v, lo, hi)   ! -> aborts (every element is null)
        print '(a,a)', "unexpectedly reduced an all-null string column, lo=", lo
    end subroutine scenario_sorting_minmax_all_null_strcol

    !> parquet_column counterpart. pf_minmax has no parquet_column form -- a value out-argument
    !! needs a compile-time element type -- so this is pf_argminmax, which shares the same worker
    !! and the same guard.
    subroutine scenario_sorting_argminmax_all_null_col()
        type(parquet_column) :: c
        integer :: imin, imax
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call c%set_null(2_int64)
        call pf_argminmax(c, imin, imax)
        print '(a,i0,a,i0)', "a partially null column answered imin=", imin, " imax=", imax
        call c%set_null(1_int64)
        call c%set_null(3_int64)
        call pf_argminmax(c, imin, imax)   ! -> aborts (every row is null)
        print '(a,i0)', "unexpectedly reduced an all-null column, imin=", imin
    end subroutine scenario_sorting_argminmax_all_null_col

    !> The empty-key-list rejection reached through the INT64 pf_argsort form, which is a separate
    !! generated body from the int32 one scenario_sorting_keys_empty drives.
    subroutine scenario_sorting_keys_empty_i64()
        type(pf_sort_keys) :: k
        integer(int64), allocatable :: perm(:)
        call pf_argsort(k, perm)   ! -> aborts (no key added)
        print '(a,i0)', "unexpectedly sorted an empty key list, size=", size(perm)
    end subroutine scenario_sorting_keys_empty_i64

    !> And through pf_is_sorted, whose own copy of the guard is a third one. Answering .true. for
    !! a key list with no keys would be the plausible wrong behaviour -- vacuously sorted.
    subroutine scenario_sorting_is_sorted_keys_empty()
        type(pf_sort_keys) :: k
        logical :: answer
        call pf_is_sorted(k, answer)   ! -> aborts (no key added)
        print '(a,l1)', "unexpectedly answered for an empty key list, answer=", answer
    end subroutine scenario_sorting_is_sorted_keys_empty

    !> A column that has no kind yet cannot be a sort key. A default-initialized parquet_column
    !! reports width 1, so it passes the vector-column guard and reaches the kind switch, where
    !! there is nothing to extract -- the one column state that gets this far.
    subroutine scenario_sorting_column_no_kind()
        type(parquet_column) :: c
        integer(int32), allocatable :: perm(:)
        call pf_argsort(c, perm)   ! -> aborts (the column has no element kind)
        print '(a,i0)', "unexpectedly sorted a kindless column, size=", size(perm)
    end subroutine scenario_sorting_column_no_kind

    !> `pf_merge` checks BOTH inputs, not just the first -- an unsorted second input is the same
    !! silent-wrong-answer class as an unsorted array in a binary search. The all-sorted call is
    !! the control, and the message must name WHICH argument was wrong.
    subroutine scenario_sorting_merge_unsorted()
        integer(int32) :: a(3) = [1, 3, 5]
        integer(int32) :: b(3) = [2, 6, 4]
        integer(int32) :: b_ok(3) = [2, 4, 6]
        integer(int32), allocatable :: m(:)
        call pf_merge(a, b_ok, m)
        print '(a,i0)', "two sorted inputs merged, size=", size(m)
        call pf_merge(a, b, m)   ! -> aborts (b is not sorted)
        print '(a,i0)', "unexpectedly merged an unsorted input, size=", size(m)
    end subroutine scenario_sorting_merge_unsorted

    !> A stream addresses `2**63` words and no more, so a draw whose words would pass that bound
    !! has nowhere to come from. The guard exists as much for the arithmetic as for the caller:
    !! an unguarded `pos + 2` here would be a deliberate signed-overflow site on a hot path, and
    !! Risk-94 records this module being caught with a compiler using exactly that kind of
    !! undefinedness to delete a branch far away. The first draw is the control -- it must succeed
    !! from a position one word below the ceiling.
    subroutine scenario_random_stream_exhausted()
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%rewind(huge(1_int64) - 2_int64)
        call rng%uniform(x)                       ! control: the last pair that still fits
        print '(a,i0)', "drew the final pair, position now ", rng%position()
        call rng%uniform(x)   ! -> aborts (no words left)
        print '(a,f8.5)', "unexpectedly drew past the end of the stream: ", x
    end subroutine scenario_random_stream_exhausted

    !> Positions are 1-based, so `%rewind` accepts exactly what `%position` gives and nothing
    !! below it. Absorbing a 0 would silently answer from position 1 and hide a caller's
    !! off-by-one in code whose whole purpose is reproducibility.
    subroutine scenario_random_stream_rewind_below_one()
        type(pf_random_stream) :: rng
        call rng%seed(1_int64, 1_int64)
        call rng%rewind(1_int64)                  ! control: the lowest valid position
        print '(a,i0)', "rewound to position ", rng%position()
        call rng%rewind(0_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly rewound below the start, position ", rng%position()
    end subroutine scenario_random_stream_rewind_below_one

    !> A Gamma distribution is not defined for a non-positive shape, so the draw refuses rather
    !! than returning something plausible. The control is the smallest shape the boost branch is
    !! written for, which is any positive value at all.
    subroutine scenario_random_gamma_shape_not_positive()
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%gamma(0.001_real64, x)           ! control: a tiny but legal shape
        print '(a,es12.5)', "drew a gamma with shape 0.001: ", x
        call rng%gamma(0.0_real64, x)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a gamma with shape 0: ", x
    end subroutine scenario_random_gamma_shape_not_positive

    !> A NaN shape must abort rather than loop or return a NaN. The guard is written `.not. (shape
    !! > 0)` rather than `shape <= 0` for exactly this reason: every comparison against a NaN is
    !! false, so the second form would wave it through.
    subroutine scenario_random_gamma_shape_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%gamma(2.0_real64, x)             ! control: an ordinary shape
        print '(a,es12.5)', "drew a gamma with shape 2: ", x
        call rng%gamma(ieee_value(0.0_real64, ieee_quiet_nan), x)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a gamma with a NaN shape: ", x
    end subroutine scenario_random_gamma_shape_nan

    !> A Poisson mean cannot be negative. `lambda = 0` is legal and always gives 0, which is the
    !! control here -- a guard written `lambda > 0` would refuse it wrongly.
    subroutine scenario_random_poisson_lambda_negative()
        type(pf_random_stream) :: rng
        integer(int64) :: k
        call rng%seed(1_int64, 1_int64)
        call rng%poisson(0.0_real64, k)           ! control: lambda 0 is legal, and always draws 0
        print '(a,i0)', "drew a poisson with lambda 0: ", k
        call rng%poisson(-1.0_real64, k)   ! -> aborts
        print '(a,i0)', "unexpectedly drew a poisson with a negative lambda: ", k
    end subroutine scenario_random_poisson_lambda_negative

    !> A NaN mean must abort. `.not. (lambda >= 0)` is what catches it; `lambda < 0` would not.
    subroutine scenario_random_poisson_lambda_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_random_stream) :: rng
        integer(int64) :: k
        call rng%seed(1_int64, 1_int64)
        call rng%poisson(4.0_real64, k)           ! control: an ordinary mean
        print '(a,i0)', "drew a poisson with lambda 4: ", k
        call rng%poisson(ieee_value(0.0_real64, ieee_quiet_nan), k)   ! -> aborts
        print '(a,i0)', "unexpectedly drew a poisson with a NaN lambda: ", k
    end subroutine scenario_random_poisson_lambda_nan

    !> A mean so large that a drawn count could overflow `integer(int64)` is refused up front,
    !! rather than silently returning a wrapped one. The control is a mean far larger than anything
    !! a real model uses and still safely inside the bound.
    subroutine scenario_random_poisson_lambda_too_large()
        type(pf_random_stream) :: rng
        integer(int64) :: k
        call rng%seed(1_int64, 1_int64)
        call rng%poisson(1.0e12_real64, k)        ! control: enormous, and still representable
        print '(a,i0)', "drew a poisson with lambda 1e12: ", k
        call rng%poisson(1.0e19_real64, k)   ! -> aborts
        print '(a,i0)', "unexpectedly drew a poisson with an unrepresentable lambda: ", k
    end subroutine scenario_random_poisson_lambda_too_large

    !> An `integer(int32)` result refuses a count that does not fit rather than narrowing it. The
    !! control is the same mean into an `integer(int64)`, which is what the caller should use.
    subroutine scenario_random_poisson_int32_overflow()
        type(pf_random_stream) :: rng
        integer(int64) :: k64
        integer(int32) :: k32
        call rng%seed(1_int64, 1_int64)
        call rng%poisson(4.0e9_real64, k64)       ! control: the same draw, wide enough to hold it
        print '(a,i0)', "drew a poisson with lambda 4e9 into an int64: ", k64
        call rng%seed(1_int64, 1_int64)
        call rng%poisson(4.0e9_real64, k32)   ! -> aborts
        print '(a,i0)', "unexpectedly narrowed a poisson count into an int32: ", k32
    end subroutine scenario_random_poisson_int32_overflow

    !> `m < 1` names no population to draw from. The control is `m == 1`, the smallest that exists.
    !!
    !! **The control also carries this procedure's own distinguishing case**: it draws four values
    !! from a population of one, which is `size(idx) > m` -- legal here and refused by
    !! `pf_random_subset`. So a guard copied across from `subset_check`, which is the most likely way
    !! for this one to go wrong, aborts on the control line and fails the scenario rather than
    !! passing it vacuously.
    subroutine scenario_random_resample_empty_population()
        integer(int64) :: idx(4)
        call pf_random_resample(idx, 1_int64, 1_int64)   ! control: n > m is legal WITH replacement
        print '(a,i0)', "resampled 4 values from a population of 1, first ", idx(1)
        call pf_random_resample(idx, 0_int64, 1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly resampled from an empty population: ", idx(1)
    end subroutine scenario_random_resample_empty_population

    !> A resampled value may be anything in `[1, m]`, so an `integer(int32)` array cannot serve an
    !! `m` above `huge(int32)` -- the failure would otherwise be a silent narrowing wrap, a plausible
    !! negative index that containment downstream would not catch. The control is `m == huge(int32)`
    !! exactly, the largest that still fits.
    subroutine scenario_random_resample_int32_too_narrow()
        integer(int32) :: idx(4)
        call pf_random_resample(idx, int(huge(1_int32), int64), 1_int64)   ! control: the largest that fits
        print '(a,i0)', "resampled from a population of huge(int32), first element ", idx(1)
        call pf_random_resample(idx, int(huge(1_int32), int64) + 1_int64, 1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly resampled int32 values from a wider population: ", idx(1)
    end subroutine scenario_random_resample_int32_too_narrow

    !> A subset is drawn without replacement, so it cannot be larger than its population. Clamping
    !! to `m` would be the tempting alternative and is the wrong one: it silently hands back fewer
    !! elements than the caller's array has room for, leaving the tail whatever it was. The control
    !! asks for exactly `m`, which is the largest legal request and is the permutation itself.
    subroutine scenario_random_subset_larger_than_population()
        integer(int64) :: idx(10), full(10)
        call pf_random_subset(full, 10_int64, 1_int64)   ! control: n == m is legal
        print '(a,i0)', "drew a full-population subset, first element ", full(1)
        call pf_random_subset(idx, 9_int64, 1_int64)   ! -> aborts (10 elements from 9)
        print '(a,i0)', "unexpectedly drew more elements than the population has: ", idx(1)
    end subroutine scenario_random_subset_larger_than_population

    !> `m < 1` names no population at all. The control is `m == 1`, the smallest one that exists.
    subroutine scenario_random_subset_empty_population()
        integer(int64) :: idx(1)
        call pf_random_subset(idx, 1_int64, 1_int64)     ! control: the one-element population
        print '(a,i0)', "drew from the one-element population, got ", idx(1)
        call pf_random_subset(idx, 0_int64, 1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly drew from an empty population: ", idx(1)
    end subroutine scenario_random_subset_empty_population

    !> An element of the permutation may be any value in `[1, m]`, so an `integer(int32)` result
    !! array cannot serve an `m` above `huge(int32)` -- and the failure would otherwise be a silent
    !! narrowing wrap rather than a missing element, which no bijectivity check downstream could
    !! see. The control is `m == huge(int32)` exactly, the largest that still fits.
    subroutine scenario_random_subset_int32_too_narrow()
        integer(int32) :: idx(4)
        call pf_random_subset(idx, int(huge(1_int32), int64), 1_int64)   ! control: the largest that fits
        print '(a,i0)', "drew from a population of huge(int32), first element ", idx(1)
        call pf_random_subset(idx, int(huge(1_int32), int64) + 1_int64, 1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly drew int32 elements from a wider population: ", idx(1)
    end subroutine scenario_random_subset_int32_too_narrow

    !> A typed keyword makes the writer synthesize a "<KEY>.datatype" entry, so a caller's own
    !! entry of that name would be a second writer of the same key and the file would carry two,
    !! free to disagree. The explicit one wins, with a warning, and the write still succeeds --
    !! aborting over a metadata naming clash would be disproportionate for an otherwise valid file.
    subroutine scenario_metadata_datatype_key_collision()
        call metadata_datatype_collision_case(.true., "test_run/es_meta_dt_collide.parquet")
    end subroutine scenario_metadata_datatype_key_collision

    !> The negative control for the scenario above: the same program without the colliding key
    !! must NOT warn. A guard that fires unconditionally passes every test written for the
    !! collision itself, so the absence of the warning is what has teeth here.
    subroutine scenario_metadata_datatype_no_collision_control()
        call metadata_datatype_collision_case(.false., "test_run/es_meta_dt_control.parquet")
    end subroutine scenario_metadata_datatype_no_collision_control

    !> Writes a file with a typed NSIDE, optionally alongside a caller-supplied NSIDE.datatype,
    !! and aborts unless the result carries exactly ONE companion with the expected value. Both
    !! scenarios above go through here, each with its own output path -- they run concurrently
    !! under xargs -P, so a shared fixture path would truncate under its sibling.
    subroutine metadata_datatype_collision_case(collide, out_file)
        logical, intent(in) :: collide            !! add the caller's own NSIDE.datatype entry too.
        character(len=*), intent(in) :: out_file  !! this scenario's own output file.
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: rd
        integer(int32) :: id0(1) = [1_int32]
        character(len=:), allocatable :: keys(:), vals(:), token
        integer :: i, n

        call s%init(table="dt_collision_table")
        call s%add_field("id0", "int32")
        call s%add_metadata("NSIDE", 1024_int32)
        if (collide) call s%add_metadata("NSIDE.datatype", "user_supplied")

        call parquet_open_writer(w, out_file, s)
        call parquet_write_column(w, "id0", id0)
        call parquet_close_writer(w)

        call parquet_open_reader(rd, out_file)
        call parquet_get_metadata_items(rd, keys, vals)
        call parquet_close_reader(rd)

        n = 0
        token = ""
        do i = 1, size(keys)
            if (trim(keys(i)) /= "NSIDE.datatype") cycle
            n = n + 1
            token = trim(vals(i))
        end do
        if (n /= 1) error stop "expected exactly one NSIDE.datatype entry in the written file"
        if (collide) then
            if (token /= "user_supplied") error stop "the caller's explicit NSIDE.datatype should have won"
        else
            if (token /= "int32") error stop "the synthesized NSIDE.datatype should carry int32"
        end if
        print '(a,a)', "NSIDE.datatype=", token
    end subroutine metadata_datatype_collision_case

end program error_scenarios
