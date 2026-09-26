!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Error scenarios for the Parquet file layer: the writer, the reader, schema and MAML
!> validation, run-time settings, `%print_stat`, read-time filters, and the table verbs
!> (`row_mask`, `fillna`/`dropna`, `parse`, `explode`, ...) whose refusals mirror the
!> reader's own.
!!
!! One of the four `error_scenarios_*` group modules. They exist for COMPILE TIME, not for
!! taste: as a single program with every scenario contained in it this file took about 75 s
!! under ifx -- the longest single compile in the project, and on the critical path of every
!! build -- because one host scope held more than two thousand internal procedures. Four
!! modules of roughly equal size compile in parallel and none of them is the pole any more.
!! Splitting further would buy little; splitting `src/` this way is not possible, since a
!! module's structure is the library's public shape.
!!
!! `error_scenarios.f90` walks the four `dispatch_*` procedures in turn and stops at the first
!! that reports the name as its own, so a scenario name must be unique across all four. The tests
!! driving this group's scenarios are the suite named after it: `io` -> `errors` (test_errors.f90),
!! and `<group>` -> `<group>_errors` for the other three. A suite driving scenarios MUST end in
!! `_errors`, which is how `suite_is_safe_to_parallelize` knows to run it serially.
module error_scenarios_io
    use parquet
    use parquet_maml_base, only: parquet_maml_file, get_parquet_maml
    use parquet_strings, only : parquet_string_column
    ! parquet_emit_info is deliberately PRIVATE in the `parquet` facade (it is an output
    ! channel, not user API), so the informational-channel scenario imports it from the
    ! settings module directly.
    use parquet_settings, only : parquet_emit_info, parquet_emit_warning
    use parquet_columns
    use parquet_list, only : parquet_list_column
    use parquet_table_example, only : parquet_table_test
    use parquet_tables
    use parquet_temporal, only : parquet_date, parquet_timestamp
    use iso_fortran_env, only : int32, int64, real32, real64
    use error_scenarios_support, only : multitype_vector_schema, scenario_setenv, spatial_sky_cloud, &
        write_list_scenario_fixture, write_print_rows_fixture, write_text_file
    implicit none
    private

    public :: dispatch_error_scenarios_io

contains

    !> Run `scenario` if it is one of this module's, and report whether it was.
    !!
    !! `handled` is `.false.` for a name this group does not own, which is how
    !! `error_scenarios.f90` walks the four groups in turn without any of them knowing
    !! what the others hold.
    subroutine dispatch_error_scenarios_io(scenario, handled)
        character(len=*), intent(in) :: scenario
        logical, intent(out) :: handled

        handled = .true.
        select case (trim(scenario))
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
        case ("string_read_over_offset_limit")
            call scenario_string_read_over_offset_limit()
        case ("col_size_overflow")
            call scenario_col_size_overflow()
        case ("col_size_and_row_mode_avoid_whole_column_read")
            call scenario_col_size_and_row_mode_avoid_whole_column_read()
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
        case ("write_undeclared_column_list")
            call scenario_write_undeclared_column_list()
        case ("write_undeclared_column_map")
            call scenario_write_undeclared_column_map()
        case ("write_undeclared_column_struct")
            call scenario_write_undeclared_column_struct()
        case ("write_map_nested_value")
            call scenario_write_map_nested_value()
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
        case ("read_dictionary_binary_unsupported")
            call scenario_read_dictionary_binary_unsupported()
        case ("print_rows_negative_count")
            call scenario_print_rows_negative_count()
        case ("print_rows_rows_with_first")
            call scenario_print_rows_rows_with_first()
        case ("print_rows_slice_out_of_range")
            call scenario_print_rows_slice_out_of_range()
        case ("print_rows_missing_column")
            call scenario_print_rows_missing_column()
        case ("print_rows_bad_digits")
            call scenario_print_rows_bad_digits()
        case ("print_rows_bad_width")
            call scenario_print_rows_bad_width()
        case ("print_rows_bad_max_columns")
            call scenario_print_rows_bad_max_columns()
        case ("print_rows_unopened")
            call scenario_print_rows_unopened()
        case ("prefetch_unknown_column")
            call scenario_prefetch_unknown_column()
        case ("filter_unknown_column")
            call scenario_filter_unknown_column()
        case ("filter_vector_column")
            call scenario_filter_vector_column()
        case ("filter_list_column")
            call scenario_filter_list_column()
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
        case ("filter_set_unbound_name")
            call scenario_filter_set_unbound_name()
        case ("filter_set_missing_at")
            call scenario_filter_set_missing_at()
        case ("filter_set_no_value")
            call scenario_filter_set_no_value()
        case ("filter_set_wrong_family")
            call scenario_filter_set_wrong_family()
        case ("filter_set_on_string_column")
            call scenario_filter_set_on_string_column()
        case ("filter_set_nan_member")
            call scenario_filter_set_nan_member()
        case ("filter_set_duplicate_name")
            call scenario_filter_set_duplicate_name()
        case ("filter_set_name_with_space")
            call scenario_filter_set_name_with_space()
        case ("filter_set_name_with_at")
            call scenario_filter_set_name_with_at()
        case ("filter_set_blank_name")
            call scenario_filter_set_blank_name()
        case ("filter_set_mask_length")
            call scenario_filter_set_mask_length()
        case ("filter_set_too_many")
            call scenario_filter_set_too_many()
        case ("filter_set_vector_column")
            call scenario_filter_set_vector_column()
        case ("filter_set_control")
            call scenario_filter_set_control()
        case ("filter_list_empty")
            call scenario_filter_list_empty()
        case ("filter_list_quoted_on_numeric")
            call scenario_filter_list_quoted_on_numeric()
        case ("filter_list_unquoted_on_string")
            call scenario_filter_list_unquoted_on_string()
        case ("filter_list_bad_number")
            call scenario_filter_list_bad_number()
        case ("filter_list_non_integer")
            call scenario_filter_list_non_integer()
        case ("filter_list_nan_member")
            call scenario_filter_list_nan_member()
        case ("filter_list_trailing_comma")
            call scenario_filter_list_trailing_comma()
        case ("filter_list_nested_paren")
            call scenario_filter_list_nested_paren()
        case ("filter_list_unclosed")
            call scenario_filter_list_unclosed()
        case ("filter_list_unclosed_quote")
            call scenario_filter_list_unclosed_quote()
        case ("filter_list_on_bool_column")
            call scenario_filter_list_on_bool_column()
        case ("filter_set_temporal_column")
            call scenario_filter_set_temporal_column()
        case ("is_finite_on_int_column")
            call scenario_is_finite_on_int_column()
        case ("is_finite_takes_no_value")
            call scenario_is_finite_takes_no_value()
        case ("filter_list_expr_text")
            call scenario_filter_list_expr_text()
        case ("filter_list_bare_dot")
            call scenario_filter_list_bare_dot()
        case ("filter_list_fortran_exponent")
            call scenario_filter_list_fortran_exponent()
        case ("filter_list_two_numbers")
            call scenario_filter_list_two_numbers()
        case ("filter_list_two_reals")
            call scenario_filter_list_two_reals()
        case ("filter_list_control")
            call scenario_filter_list_control()
        case ("row_mask_wrong_size")
            call scenario_row_mask_wrong_size()
        case ("row_mask_bad_rule")
            call scenario_row_mask_bad_rule()
        case ("row_mask_unknown_column")
            call scenario_row_mask_unknown_column()
        case ("row_mask_vector_column")
            call scenario_row_mask_vector_column()
        case ("row_mask_int32_range")
            call scenario_row_mask_int32_range()
        case ("row_mask_bad_integer")
            call scenario_row_mask_bad_integer()
        case ("row_mask_nan_bound")
            call scenario_row_mask_nan_bound()
        case ("row_mask_bool_ordering")
            call scenario_row_mask_bool_ordering()
        case ("row_mask_unquoted_string")
            call scenario_row_mask_unquoted_string()
        case ("row_mask_is_nan_on_int")
            call scenario_row_mask_is_nan_on_int()
        case ("row_mask_starts_with_on_int")
            call scenario_row_mask_starts_with_on_int()
        case ("row_mask_unbound_set")
            call scenario_row_mask_unbound_set()
        case ("row_mask_list_quoted")
            call scenario_row_mask_list_quoted()
        case ("row_mask_temporal_precision")
            call scenario_row_mask_temporal_precision()
        case ("row_mask_list_column")
            call scenario_row_mask_list_column()
        case ("row_mask_map_column")
            call scenario_row_mask_map_column()
        case ("row_mask_struct_column")
            call scenario_row_mask_struct_column()
        case ("row_mask_quoted_integer")
            call scenario_row_mask_quoted_integer()
        case ("row_mask_quoted_real")
            call scenario_row_mask_quoted_real()
        case ("row_mask_bad_real")
            call scenario_row_mask_bad_real()
        case ("row_mask_quoted_bool")
            call scenario_row_mask_quoted_bool()
        case ("row_mask_bad_bool")
            call scenario_row_mask_bad_bool()
        case ("row_mask_unquoted_match")
            call scenario_row_mask_unquoted_match()
        case ("row_mask_unquoted_temporal")
            call scenario_row_mask_unquoted_temporal()
        case ("table_filter_rows_shared")
            call scenario_table_filter_rows_shared()
        case ("row_mask_control")
            call scenario_row_mask_control()
        case ("fillna_real_into_integer")
            call scenario_fillna_real_into_integer()
        case ("fillna_logical_into_numeric")
            call scenario_fillna_logical_into_numeric()
        case ("fillna_string_into_numeric")
            call scenario_fillna_string_into_numeric()
        case ("fillna_integer_into_string")
            call scenario_fillna_integer_into_string()
        case ("fillna_int32_range")
            call scenario_fillna_int32_range()
        case ("fillna_container_column")
            call scenario_fillna_container_column()
        case ("fillna_unsupported_column")
            call scenario_fillna_unsupported_column()
        case ("fillna_unknown_column")
            call scenario_fillna_unknown_column()
        case ("ffill_limit_zero")
            call scenario_ffill_limit_zero()
        case ("ffill_limit_negative")
            call scenario_ffill_limit_negative()
        case ("ffill_container_column")
            call scenario_ffill_container_column()
        case ("dropna_how_and_min_valid")
            call scenario_dropna_how_and_min_valid()
        case ("dropna_bad_how")
            call scenario_dropna_bad_how()
        case ("dropna_min_valid_range")
            call scenario_dropna_min_valid_range()
        case ("dropna_min_valid_negative")
            call scenario_dropna_min_valid_negative()
        case ("dropna_bad_how_nothing_resident")
            call scenario_dropna_bad_how_nothing_resident()
        case ("fillna_shared")
            call scenario_fillna_shared()
        case ("fill_control")
            call scenario_fill_control()
        case ("get_matrix_string_column")
            call scenario_get_matrix_bad_column("s")
        case ("get_matrix_vector_column")
            call scenario_get_matrix_bad_column("vec")
        case ("get_matrix_kind_mismatch")
            call scenario_get_matrix_bad_column("i")
        case ("get_matrix_unknown_column")
            call scenario_get_matrix_bad_column("nosuch")
        case ("set_matrix_no_widening")
            call scenario_set_matrix_no_widening()
        case ("set_matrix_wrong_ncols")
            call scenario_set_matrix_shape("ncols")
        case ("set_matrix_wrong_nrows")
            call scenario_set_matrix_shape("nrows")
        case ("set_matrix_mask_shape")
            call scenario_set_matrix_shape("mask")
        case ("drop_columns_missing")
            call scenario_drop_columns_missing()
        case ("drop_columns_predefined")
            call scenario_drop_columns_predefined()
        case ("keep_columns_missing")
            call scenario_keep_columns_missing()
        case ("keep_columns_predefined")
            call scenario_keep_columns_predefined()
        case ("drop_columns_shared")
            call scenario_drop_columns_shared()
        case ("matrix_control")
            call scenario_matrix_control()
        case ("parse_column_non_string")
            call scenario_parse_column_bad_source("i")
        case ("parse_column_vector_source")
            call scenario_parse_column_bad_source("vec")
        case ("parse_column_bad_target")
            call scenario_parse_column_bad_target()
        case ("parse_column_invalid_token")
            call scenario_parse_column_invalid_token()
        case ("parse_column_malformed")
            call scenario_parse_column_malformed(.false.)
        case ("parse_column_malformed_long")
            call scenario_parse_column_malformed(.true.)
        case ("parse_column_to_name_exists")
            call scenario_parse_column_to_name_exists()
        case ("parse_column_shared")
            call scenario_parse_column_shared()
        case ("reload_after_parse_column")
            call scenario_reload_after_parse_column()
        case ("format_column_string_source")
            call scenario_format_column_bad_source("s")
        case ("format_column_vector_source")
            call scenario_format_column_bad_source("vec")
        case ("format_column_fmt_on_temporal")
            call scenario_format_column_fmt_on_temporal()
        case ("parse_column_predefined")
            call scenario_parse_column_predefined()
        case ("format_column_predefined")
            call scenario_format_column_predefined(.false.)
        case ("format_column_predefined_false")
            call scenario_format_column_predefined(.true.)
        case ("cast_predefined")
            call scenario_cast_predefined(.false.)
        case ("cast_predefined_false")
            call scenario_cast_predefined(.true.)
        case ("convert_control")
            call scenario_convert_control()
        case ("explode_wrong_length")
            call scenario_explode_wrong_length()
        case ("explode_negative_count")
            call scenario_explode_negative_count()
        case ("explode_row_count_overflow")
            call scenario_explode_row_count_overflow()
        case ("explode_shared")
            call scenario_explode_shared()
        case ("duplicated_bad_keep")
            call scenario_bad_keep("duplicated")
        case ("drop_duplicates_bad_keep")
            call scenario_bad_keep("drop_duplicates")
        case ("duplicated_all_unorderable")
            call scenario_duplicated_all_unorderable()
        case ("duplicated_all_nothing_resident")
            call scenario_duplicated_all_nothing_resident()
        case ("duplicated_unknown_column")
            call scenario_duplicated_unknown_column()
        case ("sort_by_values_wrong_length")
            call scenario_sort_by_values_wrong_length()
        case ("argsort_by_values_wrong_length")
            call scenario_argsort_by_values_wrong_length()
        case ("drop_duplicates_shared")
            call scenario_drop_duplicates_shared()
        case ("sort_by_values_shared")
            call scenario_sort_by_values_shared()
        case ("rowverbs_control")
            call scenario_rowverbs_control()
        case ("remap_length_mismatch")
            call scenario_remap_length_mismatch()
        case ("remap_duplicate_key")
            call scenario_remap_duplicate_key()
        case ("remap_unmapped_no_policy")
            call scenario_remap_unmapped_no_policy()
        case ("value_counts_count_name_collision")
            call scenario_value_counts_count_name_collision()
        case ("value_counts_unknown_column")
            call scenario_value_counts_unknown_column()
        case ("value_counts_unorderable")
            call scenario_value_counts_unorderable()
        case ("counting_control")
            call scenario_counting_control()
        case ("sortkey_remap_size_mismatch")
            call scenario_sortkey_remap_size_mismatch()
        case ("sortkey_remap_name_too_long")
            call scenario_sortkey_remap_name_too_long()
        case ("set_arrow_threads_zero")
            call scenario_set_arrow_threads_zero()
        case ("settings_bad_codec")
            call scenario_settings_bad_codec()
        case ("settings_file_date_wrong_shape")
            call scenario_settings_file_date("2020-01-02 03:04:05")
        case ("settings_file_date_too_short")
            call scenario_settings_file_date("2020-01-02T03:04")
        case ("settings_file_date_not_a_digit")
            call scenario_settings_file_date("2020-01-0xT03:04:05")
        case ("settings_file_date_bad_date_sep")
            call scenario_settings_file_date("2020/01-02T03:04:05")
        case ("settings_file_date_bad_time_sep")
            call scenario_settings_file_date("2020-01-02T03.04:05")
        case ("settings_file_date_out_of_range")
            call scenario_settings_file_date("2020-13-02T03:04:05")
        case ("settings_file_date_bad_day")
            call scenario_settings_file_date("2020-01-32T03:04:05")
        case ("settings_file_date_bad_hour")
            call scenario_settings_file_date("2020-01-02T24:04:05")
        case ("settings_file_date_bad_minute")
            call scenario_settings_file_date("2020-01-02T03:60:05")
        case ("settings_file_date_bad_second")
            call scenario_settings_file_date("2020-01-02T03:04:60")
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
        case ("weighted_infinite_weight")
            call scenario_weighted_infinite_weight()
        case ("weighted_race_nan_weight")
            call scenario_weighted_race_nan_weight()
        case ("weighted_race_infinite_weight")
            call scenario_weighted_race_infinite_weight()
        case ("weighted_race_negative_weight")
            call scenario_weighted_race_negative_weight()
        case ("weighted_race_all_zero")
            call scenario_weighted_race_all_zero()
        case ("weighted_next_population_too_big")
            call scenario_weighted_next_population_too_big()
        case ("weighted_subset_population_too_big")
            call scenario_weighted_subset_population_too_big()
        case ("weighted_perm_population_too_big")
            call scenario_weighted_perm_population_too_big()
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
        case ("settings_advice_normal")
            call scenario_settings_advice(level="normal")
        case ("settings_advice_silent")
            call scenario_settings_advice(level="silent")
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
        case ("filter_starts_with_non_string_column")
            call scenario_filter_starts_with_non_string_column()
        case ("filter_starts_with_unquoted_value")
            call scenario_filter_starts_with_unquoted_value()
        case ("filter_starts_with_on_temporal_column")
            call scenario_filter_starts_with_on_temporal_column()
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
        case ("shape_queries_avoid_whole_column_read")
            call scenario_shape_queries_avoid_whole_column_read()
        case ("filter_row_element_mode_no_whole_column_read")
            call scenario_filter_row_element_mode_no_whole_column_read()
        case ("filter_scoped_reads_no_whole_column")
            call scenario_filter_scoped_reads_no_whole_column()
        case ("sort_unknown_column")
            call scenario_sort_unknown_column()
        case ("sort_vector_column")
            call scenario_sort_vector_column()
        case ("sort_list_column")
            call scenario_sort_list_column()
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
        case ("filter_pre_leaf_row_count_mismatch")
            call scenario_filter_pre_leaf_row_count_mismatch()
        case ("string_length_on_non_string_column")
            call scenario_string_length_on_non_string_column()
        case ("write_row_count_mismatch")
            call scenario_write_row_count_mismatch()
        case ("read_row_count_mismatch")
            call scenario_read_row_count_mismatch()
        case ("read_before_open")
            call scenario_read_before_open()
        case ("write_before_open")
            call scenario_write_before_open()
        case ("new_row_group_before_open")
            call scenario_new_row_group_before_open()
        case ("get_metadata_before_open")
            call scenario_get_metadata_before_open()
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
        case default
            handled = .false.
        end select
    end subroutine dispatch_error_scenarios_io

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

    !> The three CONTAINER write paths carry their own copy of the schema-enforced name check, so
    !> each needs its own scenario: a list, a map and a struct column written under a name the
    !> schema never declared.
    subroutine scenario_write_undeclared_column_list()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_list_column) :: col

        call col%init(PK_INT32)
        call col%append_row([1_int32, 2_int32])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_list.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", col)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_list

    subroutine scenario_write_undeclared_column_map()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_map_column) :: col

        call col%init(PK_INT32)
        call col%append_row(["a"], [1_int32])
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_map.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", col)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_map

    subroutine scenario_write_undeclared_column_struct()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: col

        call col%init(["v"], [PK_INT32], 1_int64)
        schema = multitype_vector_schema()
        call parquet_open_writer(writer, "test_run/error_scenario_undeclared_struct.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", col)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column_struct

    !> A map whose VALUE is itself a container. Phase 7 widened `%adopt_*` so such a column can be
    !> built -- from a file or by hand -- while there is no nested MAML token to declare one with, so
    !> the refusal is by KIND here rather than by the type-mismatch guard further down: that guard
    !> would fire too, but its message points at the schema instead of at the real reason.
    subroutine scenario_write_map_nested_value()
        type(parquet_writer) :: writer
        type(parquet_map_column) :: outer
        type(parquet_list_column) :: inner
        type(parquet_string_column) :: keys
        type(parquet_column) :: ipay, kcol, vcol
        class(parquet_container_column), allocatable :: cc
        integer(int64), allocatable :: io(:), offs(:)
        integer :: k

        ! `map<string, list<int32>>`, built the only way there is: no MAML token declares a nested
        ! container, so %adopt_rows over a value column that already holds one is the whole route.
        call ipay%init(PK_INT32, nrows=4_int64)
        do k = 1, 4
            call ipay%set_at(int(k, int64), int(k, int32))
        end do
        io = [0_int64, 2_int64, 4_int64]
        call inner%adopt_rows(io, ipay)
        allocate(cc, source=inner)
        call vcol%adopt_container(cc)
        call keys%append_string("a")
        call keys%append_string("b")
        call kcol%adopt_string_column(keys)
        offs = [0_int64, 1_int64, 2_int64]
        call outer%adopt_rows(offs, kcol, vcol)
        call parquet_open_writer(writer, "test_run/error_scenario_map_nested_value.parquet")
        call parquet_write_column(writer, "m", outer)
        call parquet_close_writer(writer)
    end subroutine scenario_write_map_nested_value

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

    !> A dictionary column over BINARY values, read as strings.
    !>
    !> Decoding a dictionary column (a pandas `category`) on read deliberately does not widen what
    !> this library can read: Arrow restores a stored dictionary type over binary values as
    !> readily as over strings, and a dictionary over binary must be refused exactly as a plain
    !> binary column is. Its type query answers "unknown" (asserted in the `reading` suite), and
    !> the read itself aborts here.
    subroutine scenario_read_dictionary_binary_unsupported()
        type(parquet_reader) :: reader
        character(len=16) :: values(8)

        call parquet_open_reader(reader, "test/fixtures/dictionary_types.parquet")
        call parquet_read_column(reader, "cat_bytes", values)
        print '(a)', "unexpectedly read a dictionary column over binary values without error"
    end subroutine scenario_read_dictionary_binary_unsupported

    ! ---- %print_rows argument guards --------------------------------------------------------
    !
    ! Every guard %print_rows has, one scenario each. They matter as a GROUP: all eight abort from
    ! the same procedure, so an exit status alone cannot tell a guard that fired from the wrong
    ! guard firing, which is why each test in test_errors.f90 asserts a fragment of the message
    ! rather than only the status. The permitted EDGE of each bound (digits=1, digits=17,
    ! max_width=8, max_columns=1, first=0/last=0) is asserted in process, in the `table_display`
    ! suite -- an out-of-process scenario can only show that something aborted.
    !
    ! Their fixture is write_print_rows_fixture, in error_scenarios_support.f90.

    !> A negative row count is a caller mistake, not an empty display.
    subroutine scenario_print_rows_negative_count()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_negative_count.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(first=-1)
        print '(a)', "unexpectedly accepted a negative first="
    end subroutine scenario_print_rows_negative_count

    !> `rows=` names the rows explicitly, so `first=`/`last=` alongside it has no reading that is
    !> obviously right -- and a caller who wrote both meant one of them.
    subroutine scenario_print_rows_rows_with_first()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_rows_with_first.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(rows=parquet_slice_list([1_int32, 2_int32]), first=2)
        print '(a)', "unexpectedly accepted rows= together with first="
    end subroutine scenario_print_rows_rows_with_first

    !> A slice row outside the table is reported by the slice machinery itself, so the message is
    !> the one every other slice consumer produces rather than a second copy of it.
    subroutine scenario_print_rows_slice_out_of_range()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_slice_range.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(rows=parquet_slice_list([99_int32]))
        print '(a)', "unexpectedly accepted a slice row outside the table"
    end subroutine scenario_print_rows_slice_out_of_range

    !> A `columns=` name matching no column, reported through %require_columns so that EVERY
    !> missing name is listed rather than only the first.
    subroutine scenario_print_rows_missing_column()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_missing_column.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows("id,nosuchcolumn")
        print '(a)', "unexpectedly accepted a column name the table does not have"
    end subroutine scenario_print_rows_missing_column

    !> Zero significant digits would render every real as nothing at all.
    subroutine scenario_print_rows_bad_digits()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_bad_digits.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(digits=0)
        print '(a)', "unexpectedly accepted digits=0"
    end subroutine scenario_print_rows_bad_digits

    !> Below eight characters the "..." marker is most of the cell.
    subroutine scenario_print_rows_bad_width()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_bad_width.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(max_width=3)
        print '(a)', "unexpectedly accepted max_width=3"
    end subroutine scenario_print_rows_bad_width

    !> Showing no columns at all is what %print_stat is for; zero is not a display.
    subroutine scenario_print_rows_bad_max_columns()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_bad_maxcols.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(max_columns=0)
        print '(a)', "unexpectedly accepted max_columns=0"
    end subroutine scenario_print_rows_bad_max_columns

    !> The open check sits ABOVE the verbosity check, so an unopened table reports that mistake
    !> rather than silently doing nothing for the wrong reason.
    subroutine scenario_print_rows_unopened()
        type(parquet_table) :: t

        call t%print_rows()
        print '(a)', "unexpectedly printed rows of a table that was never opened"
    end subroutine scenario_print_rows_unopened

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

    !> The SAME guard as scenario_write_before_open, reached from a different public entry point,
    !> and the pair is what the scenario is for.
    !!
    !! check_writer_open used to take no context argument and hard-coded "parquet_write_column" into
    !! its message, so calling parquet_new_row_group on an unopened writer named a procedure the
    !! caller never wrote. That is invisible to scenario_write_before_open, which calls
    !! parquet_write_column and therefore reads the right name either way. **Both scenarios are
    !! needed**: this one asserts the message names parquet_new_row_group, the other that it still
    !! names parquet_write_column -- and only together do they show the context is threaded through
    !! rather than hard-coded to something else.
    subroutine scenario_new_row_group_before_open()
        type(parquet_writer) :: writer

        call parquet_new_row_group(writer, 100)
        print '(a)', "unexpectedly started a row group on an unopened writer without error"
    end subroutine scenario_new_row_group_before_open

    !> parquet_get_metadata was the ONE procedure taking a parquet_reader that did not check the
    !> reader was open, and its failure was silent rather than loud: reader%metadata is unallocated
    !> on a reader that was never opened, parquet_metadata_find_index guards that with an
    !> `allocated` test and answers 0, so with default= present the call returned the default and
    !> the program carried on. A use-before-open was indistinguishable from a key that is genuinely
    !> absent.
    !!
    !! `default=` is passed deliberately. Without it the call already aborted -- but through
    !! parquet_metadata_stop_missing, blaming the KEY. This scenario is the one that could not abort
    !! at all before the guard, so it is the one that proves the guard is there.
    subroutine scenario_get_metadata_before_open()
        type(parquet_reader) :: reader
        integer :: v

        call parquet_get_metadata(reader, "NROWS", v, default=-1)
        print '(a,i0)', "unexpectedly read metadata from an unopened reader, got=", v
    end subroutine scenario_get_metadata_before_open

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

    !> Proves that a plain `string` column whose byte payload exceeds Arrow's int32 offset ceiling
    !> (2^31-1 bytes, ~2 GiB) reads back intact instead of aborting the process. Arrow's Parquet
    !> decoder cannot hold such a column in one `utf8` array, so it hands it over in several
    !> chunks, and arrow::Concatenate cannot rebuild it as `utf8` either ("offset overflow while
    !> concatenating arrays"); combine_column_chunks (parquet_wrapper.cpp) widens the chunks to
    !> arrow::large_utf8() first -- see widen_string_chunks_if_needed there. A genuine case needs
    !> more than 2 GiB of strings, so this scenario reaches the same path on a tiny fixture through
    !> two test-only, process-global hooks declared locally below (not part of the public Fortran
    !> API): parquet_debug_set_string_offset_limit shrinks the ceiling to a few bytes, and
    !> parquet_debug_set_force_chunk_split re-slices every column into several chunks. Safe as
    !> process-globals for the same reason as scenario_large_string_roundtrip's use of the first,
    !> above: this scenario always runs as its own isolated subprocess.
    !>
    !> Every whole-column read funnels through that one function, so every entry point is driven
    !> here: the lazy single-column read, `prefetch=.true.`, parquet_prefetch_columns, a row filter
    !> on the column, a sort on it, a row-group chunk read, the compact parquet_string_column read,
    !> and parquet_table%materialize_all -- the call that aborted in the field. Values are compared
    !> row by row, nulls and an empty string included, with the split chunks cut mid-bitmap-byte
    !> (a wrong offset base shows exactly at a chunk boundary), and the widening is proven both
    !> present and invisible: two counters say which branch ran, while parquet_get_column_type,
    !> parquet_get_column_arrow_type and %kind keep answering for a `string` column.
    !>
    !> Two negative controls on the same fixture come first. With the real ceiling in force, the
    !> split column is combined WITHOUT being widened -- the decision keys on the payload, never on
    !> the chunk count, or every multi-chunk column would silently pay for 64-bit offsets. With the
    !> ceiling shrunk but no split, a single-chunk column never reaches the check at all.
    subroutine scenario_string_read_over_offset_limit()
        interface
            subroutine parquet_debug_set_string_offset_limit(n) &
                bind(C, name="parquet_debug_set_string_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! byte threshold to use instead of the real 2^31-1 limit; <=0 restores it.
            end subroutine parquet_debug_set_string_offset_limit

            subroutine parquet_debug_set_force_chunk_split(n) &
                bind(C, name="parquet_debug_set_force_chunk_split")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! chunks every read column is re-sliced into; <=1 restores normal reads.
            end subroutine parquet_debug_set_force_chunk_split

            function parquet_debug_get_chunk_concat_count() result(n) &
                bind(C, name="parquet_debug_get_chunk_concat_count")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: n !! columns rebuilt from several chunks so far, widened or not.
            end function parquet_debug_get_chunk_concat_count

            function parquet_debug_get_string_offset_widen_count() result(n) &
                bind(C, name="parquet_debug_get_string_offset_widen_count")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: n !! columns widened to 64-bit offsets before being combined so far.
            end function parquet_debug_get_string_offset_widen_count
        end interface

        character(len=*), parameter :: out_file = "test_run/error_scenario_string_read_over_offset_limit.parquet"
        integer, parameter :: nrows = 12
        integer(int64), parameter :: split = 3_int64 !! chunks per column under the hook.
        integer(int64), parameter :: small_limit = 4_int64 !! bytes: any two rows of `name` exceed it.
        ! Shortest value first and a longer one later (testing.md's "sized from the first element"
        ! rule), and deliberately NOT in sorted order, so the sort below has something to do.
        ! 50 bytes in all: any two rows exceed the shrunk ceiling, the column is far below the real one.
        character(len=8) :: names(nrows) = [character(len=8) :: &
            "k", "bb", "hhhhhhhh", "a", "eeeee", "jjj", "ccc", "llllllll", "ii", "dddd", "ggggggg", "ffffff"]
        ! Rows 3 and 7 are null; row 4 is an EMPTY string, which must stay distinct from a null.
        character(len=8) :: notes(nrows) = [character(len=8) :: &
            "x", "yy", "", "", "zzzzz", "q", "", "rrrrrrrr", "s", "tt", "uuu", "vvvvvvvv"]
        logical :: note_valid(nrows) = [.true., .true., .false., .true., .true., .true., .false., &
            .true., .true., .true., .true., .true.]
        integer(int32) :: ids(nrows) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        ! `id` in ascending `name` order: a(4) bb(2) ccc(7) dddd(10) eeeee(5) ffffff(12) ggggggg(11)
        ! hhhhhhhh(3) ii(9) jjj(6) k(1) llllllll(8).
        integer(int32) :: ids_by_name(nrows) = [4, 2, 7, 10, 5, 12, 11, 3, 9, 6, 1, 8]

        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        type(parquet_string_column) :: col
        type(parquet_table) :: t
        character(len=8) :: back(nrows), note_back(nrows)
        character(len=8), allocatable :: chunk_back(:)
        character(len=:), allocatable :: chr(:), type_name, arrow_type, text
        logical :: note_back_valid(nrows)
        logical, allocatable :: vmask(:)
        integer(int32) :: ids_back(nrows), id_hit(1)
        integer(int64) :: concat0, widen0, nrows_back, num_row_groups, rg, rg_size
        integer :: strlen_max, i

        ! 1. The fixture, written with the real ceiling in force so the file stores plain utf8,
        !    in three row groups (5, 5 and 2 rows).
        call schema%init(table="string_read_over_offset_limit")
        call schema%add_field("id", "int32")
        call schema%add_field("name", "string", array_size=8)
        call schema%add_field("note", "string", array_size=8)
        call parquet_open_writer(writer, out_file, schema, chunk_size=5)
        call parquet_write_column(writer, "id", ids)
        call parquet_write_column(writer, "name", names)
        call parquet_write_column(writer, "note", notes, is_valid=note_valid)
        call parquet_close_writer(writer)

        ! 2. Negative control A: the real ceiling, every column split into three chunks. The
        !    column is combined (the concat counter moves) and not widened (the widen counter
        !    does not), and comes back intact through a plain utf8 Concatenate.
        call parquet_debug_set_force_chunk_split(split)
        concat0 = parquet_debug_get_chunk_concat_count()
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)
        if (parquet_debug_get_chunk_concat_count() /= concat0 + 1) error stop &
            "control A: the split column was never combined from several chunks -- nothing below tests that path"
        if (parquet_debug_get_string_offset_widen_count() /= widen0) error stop &
            "control A: a multi-chunk string column that fits int32 offsets was widened -- the check keys on chunk count"
        if (any(back /= names)) error stop "control A: a multi-chunk utf8 column did not round-trip"

        ! 3. Negative control B: the ceiling shrunk to 4 bytes, no split. A single-chunk column
        !    returns before the payload test, whatever the ceiling says.
        call parquet_debug_set_force_chunk_split(0_int64)
        call parquet_debug_set_string_offset_limit(small_limit)
        concat0 = parquet_debug_get_chunk_concat_count()
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)
        if (parquet_debug_get_chunk_concat_count() /= concat0) error stop "control B: a single-chunk column was combined"
        if (parquet_debug_get_string_offset_widen_count() /= widen0) error stop "control B: a single-chunk column was widened"
        if (any(back /= names)) error stop "control B: a single-chunk utf8 column did not round-trip"

        ! 4. The case: the shrunk ceiling AND three chunks, through every whole-column entry point.
        call parquet_debug_set_force_chunk_split(split)

        ! 4a. The lazy single-column read, with nulls, the string-length query and the two type
        !     queries; print_stat runs its min/max over the widened array. Three columns are
        !     combined; the two string ones are widened and the int32 one must not be.
        concat0 = parquet_debug_get_chunk_concat_count()
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "name", back)
        call parquet_read_column(reader, "note", note_back, null_value="<null>", is_valid=note_back_valid)
        call parquet_read_column(reader, "id", ids_back)
        call parquet_get_string_length(reader, "name", strlen_max)
        call parquet_get_column_type(reader, "name", type_name)
        call parquet_get_column_arrow_type(reader, "name", arrow_type)
        call parquet_close_reader(reader, print_stat=.true.)
        if (parquet_debug_get_chunk_concat_count() /= concat0 + 3) error stop &
            "read_column: not every column was combined from several chunks"
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 2) error stop &
            "read_column: the two string columns over the ceiling were not both widened (and the int32 one must not be)"
        if (any(back /= names)) error stop "read_column: the widened string column did not round-trip"
        if (any(ids_back /= ids)) error stop "read_column: the int32 column beside it did not round-trip"
        if (any(note_back_valid .neqv. note_valid)) error stop "read_column: the nulls did not survive the widening"
        do i = 1, nrows
            if (note_valid(i) .and. note_back(i) /= notes(i)) error stop &
                "read_column: a valid row of the null-bearing widened column is wrong"
            if (.not. note_valid(i) .and. note_back(i) /= "<null>") error stop "read_column: a null row did not take null_value"
        end do
        if (strlen_max /= 8) error stop "parquet_get_string_length was wrong for a widened string column"
        if (type_name /= "string") error stop "parquet_get_column_type changed its answer for a widened column: " // type_name
        if (arrow_type /= "string") error stop &
            "parquet_get_column_arrow_type must report the file's stored type, got " // arrow_type

        ! 4b. prefetch=.true. (every column at once, parquet_reader_prefetch_all_columns).
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file, prefetch=.true.)
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 2) error stop &
            "prefetch=.true. did not widen both string columns"
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)
        if (any(back /= names)) error stop "prefetch=.true.: the widened string column did not round-trip"

        ! 4c. parquet_prefetch_columns (a named subset, parquet_reader_prefetch_columns).
        call parquet_open_reader(reader, out_file)
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_prefetch_columns(reader, ["name"])
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 1) error stop &
            "parquet_prefetch_columns did not widen the string column"
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)
        if (any(back /= names)) error stop "parquet_prefetch_columns: the widened string column did not round-trip"

        ! 4d. A row filter on the column: parquet_reader_set_filter decodes it whole first.
        call filt%add('name == "ccc"')
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file, filter=filt)
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 1) error stop "filter=: the filter column was not widened"
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= 1_int64) error stop "filter= on the widened column did not select exactly one row"
        call parquet_read_column(reader, "id", id_hit)
        call parquet_close_reader(reader)
        if (id_hit(1) /= 7) error stop "filter= on the widened column selected the wrong row"

        ! 4e. A sort on the column. The key is fetched (widened once) to build the permutation,
        !     and installing it releases every decoded column so it is re-read through the
        !     permutation -- so reading `name` afterwards widens it a second time.
        call srt%add("name")
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_open_reader(reader, out_file, sort_by=srt)
        call parquet_read_column(reader, "id", ids_back)
        call parquet_read_column(reader, "name", back)
        call parquet_close_reader(reader)
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 2) error stop &
            "sort_by=: expected the key column widened for the sort and again for the read through the permutation"
        if (any(ids_back /= ids_by_name)) error stop "sort_by= on the widened column put the rows in the wrong order"
        do i = 1, nrows
            if (back(i) /= names(ids_by_name(i))) error stop "sort_by=: the sorted widened column itself is wrong"
        end do

        ! 4f. Row-group chunk reads: each row group is combined on its own. The two 5-row groups
        !     split and widen; the 2-row group is shorter than the split and stays one chunk.
        call parquet_open_reader(reader, out_file)
        call parquet_get_num_row_groups(reader, num_row_groups)
        if (num_row_groups /= 3_int64) error stop "fixture: expected three row groups"
        widen0 = parquet_debug_get_string_offset_widen_count()
        do rg = 1_int64, num_row_groups
            call parquet_get_chunk_size(reader, rg_size, row_group=rg)
            allocate(chunk_back(rg_size))
            call parquet_read_column_chunk(reader, "name", rg, chunk_back)
            do i = 1, int(rg_size)
                if (chunk_back(i) /= names(int(rg - 1) * 5 + i)) error stop &
                    "read_column_chunk: a widened row-group chunk is wrong"
            end do
            deallocate(chunk_back)
        end do
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 2) error stop &
            "read_column_chunk: expected the two 5-row groups widened and the 2-row group left alone"

        ! 4g. The compact parquet_string_column read hands the widened buffers straight to Fortran
        !     (extract_string_buffers' 64-bit offsets path), nulls and the empty string included.
        widen0 = parquet_debug_get_string_offset_widen_count()
        call parquet_read_column(reader, "note", col)
        call parquet_close_reader(reader)
        if (parquet_debug_get_string_offset_widen_count() /= widen0 + 1) error stop &
            "compact read: the string column was not widened"
        if (col%size() /= int(nrows, int64)) error stop "compact read: wrong row count"
        if (col%null_count() /= 2_int64) error stop "compact read: wrong null count"
        do i = 1, nrows
            if (col%is_null(i) .neqv. .not. note_valid(i)) error stop "compact read: a null landed on the wrong row"
            if (note_valid(i)) then
                call col%get(i, text)
                if (text /= trim(notes(i))) error stop "compact read: a value is wrong"
            end if
        end do

        ! 4h. parquet_table: %materialize_all (the call that aborted in the field), %kind and %get.
        !     The table may read per row group on several threads, so only "widened at least once"
        !     is asserted on the counter.
        call parquet_open_table(t, out_file)
        widen0 = parquet_debug_get_string_offset_widen_count()
        call t%materialize_all()
        if (parquet_debug_get_string_offset_widen_count() <= widen0) error stop "%materialize_all: no string column was widened"
        if (t%nrows() /= int(nrows, int64)) error stop "%materialize_all: wrong row count"
        if (t%kind("name") /= PK_STRING) error stop "%kind of a widened column is not PK_STRING"
        if (t%kind("note") /= PK_STRING) error stop "%kind of a widened null-bearing column is not PK_STRING"
        call t%get("name", chr)
        do i = 1, nrows
            if (trim(chr(i)) /= trim(names(i))) error stop "%get: the widened string column is wrong"
        end do
        call t%get("note", chr, is_valid=vmask)
        if (any(vmask .neqv. note_valid)) error stop "%get: the nulls of the widened column are wrong"
        do i = 1, nrows
            if (note_valid(i) .and. trim(chr(i)) /= trim(notes(i))) error stop &
                "%get: a valid row of the widened null-bearing column is wrong"
        end do

        ! Restore both hooks -- defensive in a one-shot subprocess, but it keeps this correct if a
        ! later edit ever adds more reads to this scenario.
        call parquet_debug_set_force_chunk_split(0_int64)
        call parquet_debug_set_string_offset_limit(0_int64)
        print '(a)', "string_read_over_offset_limit: every entry point read the widened column intact"
    end subroutine scenario_string_read_over_offset_limit

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
        character(len=:), allocatable :: type_name, shape_name, arrow_type

        ! Five fixed rows deliberately cover StringView's inlined-vs-out-of-line boundary -- see
        ! parquet_debug_write_string_view_fixture's own comment in parquet_wrapper.cpp: a short
        ! inlined value, an empty inlined value, a Null, a long out-of-line value, and a value
        ! exactly at the 12-byte inline boundary.
        call parquet_debug_write_string_view_fixture( &
            "test_run/error_scenario_string_view.parquet"//char(0), "sv"//char(0))

        call parquet_open_reader(reader, out_file)
        ! STRING_VIEW is deliberately absent from arrow_leaf_family, so parquet_get_column_type
        ! answers "unknown" for a column every read path below reads perfectly well. That pairing is
        ! what doc/pages/io/reading.md's "One readable type answers "unknown" here" paragraph
        ! documents, and it is asserted here so the deliberate answer cannot be changed silently --
        ! in either direction. The shape query and the arrow-type query are the two that DO describe
        ! the column, and they are pinned beside it for the same reason.
        call parquet_get_column_type(reader, "sv", type_name)
        call parquet_get_column_shape(reader, "sv", shape_name)
        call parquet_get_column_arrow_type(reader, "sv", arrow_type)
        call parquet_read_column(reader, "sv", s_back, is_valid=is_valid)
        call parquet_get_string_length(reader, "sv", strlen_max)
        call parquet_close_reader(reader, print_stat=.true.)

        if (type_name /= "unknown") then
            error stop "parquet_get_column_type no longer answers 'unknown' for a STRING_VIEW " // &
                "column -- doc/pages/io/reading.md's mapping table and its string_view paragraph " // &
                "both describe that answer, and got: " // type_name
        end if
        if (shape_name /= "scalar") then
            error stop "parquet_get_column_shape must call a STRING_VIEW column a scalar, got: " // shape_name
        end if
        if (arrow_type /= "string_view") then
            error stop "parquet_get_column_arrow_type is the query that names a view column, got: " // arrow_type
        end if

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
    !> a genuinely huge column (see `.claude/rules/cpp-wrapper.md`'s "Guarding a hard Arrow int32-only ceiling"). The
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

    !> The two SCHEMA-ONLY container queries must read no column data, whatever they answer.
    !>
    !! `parquet_get_column_shape` and `parquet_get_map_value_type` both document themselves as
    !! schema-only, and for the shape query that is load-bearing rather than a nicety: it is what
    !! lets parquet_open_table classify every column of a file without decoding any of it. A
    !! regression would be silent -- the answers stay correct, the data is discarded again
    !! immediately, and only the timing on a large file would ever show it.
    !!
    !! Same mechanism as scenario_col_size_and_row_mode_avoid_whole_column_read: the forced error
    !! is armed BEFORE the calls, so this scenario finishing without aborting IS the assertion.
    !! Its negative control is the shared scenario_whole_column_read_forced_error_control, which
    !! proves the hook fires at all.
    !!
    !! Deliberately does NOT call parquet_get_col_size on the list column: that query legitimately
    !! reads a plain LIST one row group at a time, so including it would arm a trap for correct
    !! behaviour. The columns covered are chosen to reach every arm of the shape switch that a
    !! Parquet file can produce -- scalar, list, map, and a struct-nested leaf.
    subroutine scenario_shape_queries_avoid_whole_column_read()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores it.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface
        type(parquet_reader) :: reader
        character(len=:), allocatable :: shape, value_type

        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_debug_set_force_whole_column_read_error(1)

        call parquet_get_column_shape(reader, "list_col", shape)
        if (shape /= "list") error stop "shape query: list_col should be a list"
        call parquet_get_column_shape(reader, "map_col", shape)
        if (shape /= "map") error stop "shape query: map_col should be a map"
        call parquet_get_column_shape(reader, "struct_of_struct.label", shape)
        if (shape /= "scalar") error stop "shape query: a struct-nested string leaf should be a scalar"
        call parquet_get_column_shape(reader, "struct_of_list.values", shape)
        if (shape /= "list") error stop "shape query: a struct-nested list leaf should be a list"

        call parquet_get_map_value_type(reader, "map_col", value_type)
        if (value_type /= "int32") error stop "map value type query: map_col values are int32"
        call parquet_get_map_value_type(reader, "list_col", value_type)
        if (value_type /= "unknown") error stop "map value type query: a non-map answers unknown"

        call parquet_debug_set_force_whole_column_read_error(0)
        call parquet_close_reader(reader)
        print '(a)', "parquet_get_column_shape/parquet_get_map_value_type avoided a whole-column read, as expected"
    end subroutine scenario_shape_queries_avoid_whole_column_read

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

    !> A VARIABLE-LENGTH list column as a sort key. The same guard as scenario_sort_vector_column
    !> above, but its other arm: that one writes a 2-D array, i.e. a FIXED_SIZE_LIST, so the
    !> guard's LIST/LARGE_LIST arm was reached by no test at all until this one.
    !>
    !! The distinction is not cosmetic. A ragged list column's col_size is 1, so anything keyed on
    !! "col_size > 1" -- which is how the refusal read on the guide page until this was found --
    !! lets exactly this column through. What a leaked container does next is not an abort but a
    !! wrong answer, one entry per ELEMENT against a row-shaped result.
    !!
    !! Asserts the shape word too, which is the other half: the message used to call this a
    !! "vector column", which it is not.
    subroutine scenario_sort_list_column()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt

        call srt%add("list_col asc")
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", sort_by=srt)
        print '(a)', "unexpectedly sorted by a variable-length list column"
    end subroutine scenario_sort_list_column

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

    !> One scenario per way a file date can be wrong, all through one helper.
    !!
    !! `parquet_set_file_date` rejects a value on ten separate grounds -- five about its SHAPE
    !! (length, a non-digit where a digit belongs, either separator group, the `T`) and five about
    !! its FIELD RANGES -- and each names the part at fault rather than only saying "invalid". The
    !! two halves are genuinely independent: `2020-13-02T03:04:05` is nineteen characters with every
    !! digit and separator where it belongs and names a month that does not exist, so a shape-only
    !! validator accepts it.
    !!
    !! **The two CONTROLS run first, and each rules out a different failure.** A valid date must be
    !! accepted and round-trip, or the abort would show only that the setter rejects everything; and
    !! an EMPTY string must be accepted too, since that is the documented way back to reading the
    !! clock and is exactly what a guard written for "non-empty" would reject.
    !!
    !! One helper rather than ten near-identical bodies, taking the offending value from its own
    !! `case` entry -- there is no fixture file involved, so the usual rule about a shared helper
    !! deriving its filename from its arguments does not apply here.
    subroutine scenario_settings_file_date(bad)
        character(len=*), intent(in) :: bad !! the value that must be refused.
        character(len=:), allocatable :: got

        call parquet_set_file_date("2020-12-02T03:04:05")
        call parquet_get_file_date(got)
        print '(a)', "control: accepted and read back '" // got // "'"
        call parquet_set_file_date("")
        call parquet_get_file_date(got)
        print '(a,i0)', "control: empty accepted, length now ", len(got)
        call parquet_set_file_date(bad)   ! -> aborts, naming the part at fault
        print '(a)', "unexpectedly accepted the file date '" // bad // "'"
    end subroutine scenario_settings_file_date

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

    !> An infinite weight is refused: the total weight would be infinite and every draw degenerate.
    !!
    !! The third of `%init`'s three value guards, and the one no scenario reached. It is separate
    !! from the NaN guard rather than folded into it because the two fail differently -- a NaN
    !! compares false against every bound and would be filed as zero-weight, while an infinity
    !! compares TRUE against every bound and would take every draw.
    subroutine scenario_weighted_infinite_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        type(pf_weighted_draw) :: d
        real(real64) :: w(3)

        w = [1.0_real64, ieee_value(1.0_real64, ieee_positive_inf), 1.0_real64]
        call d%init(w, 1_int64)            ! -> aborts (a weight is infinite)
        print '(a)', "unexpectedly accepted an infinite weight"
    end subroutine scenario_weighted_infinite_weight

    !> The RACE validates its own weights, and its four guards are not `%init`'s.
    !!
    !! `pf_weighted_permutation` does not build a `pf_weighted_draw` -- it runs the exponential race
    !! -- so it carries its own copy of the NaN / infinite / negative / all-zero tests, with its own
    !! messages naming the different failure each one causes in a race rather than in a tree. All
    !! four were unreached: every existing weight-validation scenario goes through `%init`.
    subroutine scenario_weighted_race_nan_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: w(3)
        integer(int64) :: perm(3)

        w = [1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64]
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (a weight is NaN)
        print '(a)', "unexpectedly raced a NaN weight"
    end subroutine scenario_weighted_race_nan_weight

    !> The race's infinite-weight guard; see `scenario_weighted_race_nan_weight`.
    subroutine scenario_weighted_race_infinite_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        real(real64) :: w(3)
        integer(int64) :: perm(3)

        w = [1.0_real64, ieee_value(1.0_real64, ieee_positive_inf), 1.0_real64]
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (a weight is infinite)
        print '(a)', "unexpectedly raced an infinite weight"
    end subroutine scenario_weighted_race_infinite_weight

    !> The race's negative-weight guard; see `scenario_weighted_race_nan_weight`.
    subroutine scenario_weighted_race_negative_weight()
        real(real64) :: w(4)
        integer(int64) :: perm(4)

        w = [1.0_real64, 2.0_real64, -0.5_real64, 1.0_real64]
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (a weight is negative)
        print '(a)', "unexpectedly raced a negative weight"
    end subroutine scenario_weighted_race_negative_weight

    !> The race's all-zero guard; see `scenario_weighted_race_nan_weight`.
    subroutine scenario_weighted_race_all_zero()
        real(real64) :: w(4)
        integer(int64) :: perm(4)

        w = 0.0_real64
        call pf_weighted_permutation(perm, w, 1_int64)   ! -> aborts (every weight is zero)
        print '(a)', "unexpectedly raced an all-zero weight vector"
    end subroutine scenario_weighted_race_all_zero

    !> A population that cannot be named by an `integer(int32)` item index is refused.
    !!
    !! **Three separate guards ask this, and none could be reached from a test.** The real ceiling
    !! is `huge(int32)` items -- 17 GB of `real64` weights before anything is drawn -- so all three
    !! would ship with every mutation to them surviving. `parquet_debug_set_weighted_int32_limit`
    !! lowers the ceiling so a four-element fixture reaches them, which is the shape CLAUDE.md's
    !! "Guarding a hard Arrow int32-only ceiling" prescribes.
    !!
    !! **The negative control comes first in every one of the three**: the same call is made under
    !! the real ceiling and must succeed. Without it a guard that fired unconditionally would pass
    !! all three scenarios while breaking every ordinary caller.
    !!
    !! **The control is taken after a set-then-RESTORE round trip, not from the untouched default.**
    !! Restoring is the half of the hook that a scenario ending in an abort never reaches, and it is
    !! also the stronger control: a hook whose restore arm did nothing would leave the ceiling at 3
    !! and the control call would fail here rather than at some later, unrelated point.
    subroutine scenario_weighted_next_population_too_big()
        type(pf_weighted_draw) :: d
        real(real64) :: w(4)
        integer(int32) :: item

        w = 1.0_real64
        call d%init(w, 1_int64)
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call parquet_debug_set_weighted_int32_limit(-1_int64)   ! and back to the real ceiling
        call d%next(item)                                   ! control: the real ceiling admits 4 items
        print '(a,i0)', "control drew item ", item
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call d%next(item)                                   ! -> aborts (population 4 exceeds the forced 3)
        call parquet_debug_set_weighted_int32_limit(-1_int64)
        print '(a)', "unexpectedly drew into an int32 item from an over-large population"
    end subroutine scenario_weighted_next_population_too_big

    !> `pf_weighted_subset`'s own int32-index ceiling; see `scenario_weighted_next_population_too_big`.
    subroutine scenario_weighted_subset_population_too_big()
        real(real64) :: w(4)
        integer(int32) :: idx(2)

        w = 1.0_real64
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call parquet_debug_set_weighted_int32_limit(-1_int64)   ! and back to the real ceiling
        call pf_weighted_subset(idx, w, 1_int64)            ! control: the real ceiling admits 4 items
        print '(a,i0)', "control drew item ", idx(1)
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call pf_weighted_subset(idx, w, 1_int64)            ! -> aborts (population 4 exceeds the forced 3)
        call parquet_debug_set_weighted_int32_limit(-1_int64)
        print '(a)', "unexpectedly filled an int32 subset from an over-large population"
    end subroutine scenario_weighted_subset_population_too_big

    !> `pf_weighted_permutation`'s int32-index ceiling; see `scenario_weighted_next_population_too_big`.
    subroutine scenario_weighted_perm_population_too_big()
        real(real64) :: w(4)
        integer(int32) :: perm(4)

        w = 1.0_real64
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call parquet_debug_set_weighted_int32_limit(-1_int64)   ! and back to the real ceiling
        call pf_weighted_permutation(perm, w, 1_int64)      ! control: the real ceiling admits 4 items
        print '(a,i0)', "control placed item ", perm(1)
        call parquet_debug_set_weighted_int32_limit(3_int64)
        call pf_weighted_permutation(perm, w, 1_int64)      ! -> aborts (population 4 exceeds the forced 3)
        call parquet_debug_set_weighted_int32_limit(-1_int64)
        print '(a)', "unexpectedly raced into an int32 permutation from an over-large population"
    end subroutine scenario_weighted_perm_population_too_big

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

    !> Emits one ADVICE message and one WARNING in the same process, at a chosen verbosity, so the
    !> test can assert that `"silent"` separates the two CLASSES rather than silencing output
    !> wholesale.
    !>
    !> **The negative control is inside the scenario, not beside it.** Advice that is absent at
    !> `"silent"` proves nothing on its own: a broken channel, a build that never coarsens an
    !> `nside=`, and a correctly suppressed message all look identical from outside the process.
    !> The qc warning raised by the same run is what tells them apart -- it must still be there,
    !> because a warning about the data survives to `"errors_only"`.
    !>
    !> Its fixture path is derived from `level` for the reason given on
    !> `scenario_settings_cpp_warning`: two scenario names share this helper and run concurrently.
    subroutine scenario_settings_advice(level)
        character(len=*), intent(in) :: level !! verbosity to set first.
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        character(len=:), allocatable :: out_file
        integer(int32) :: v(4) = [1, 2, 3, 400]

        out_file = "test_run/scenario_settings_advice_" // trim(level) // ".parquet"

        schema%maml%name = "settings_advice.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: advice_demo", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  qc:", &
            "    max: 10" ]
        call parquet_parse_maml(schema)

        call spatial_sky_cloud(64, ra, dec)

        call parquet_set_verbosity(level)
        ! Advice: 64 points allow 19 pixels, so the explicit nside=8 (768) is coarsened and said so.
        call sk%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX, nside=8_int64)
        ! Warning: a qc violation on the same run, which goes quiet only at "errors_only".
        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
        call parquet_reset_settings()
    end subroutine scenario_settings_advice

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
    !> scenario_settings_cpp_warning closes for the warning channel.
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
        ! is the collision `.claude/rules/testing.md`'s "Tests run concurrently" note describes.
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

    !> A VARIABLE-LENGTH list column in a filter rule -- the LIST/LARGE_LIST arm of the same guard
    !> scenario_filter_vector_column covers the FIXED_SIZE_LIST arm of. See scenario_sort_list_column
    !> for why the two arms need separate scenarios and what a leaked container would do.
    subroutine scenario_filter_list_column()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("list_col > 0")
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", filter=filt)
        print '(a)', "unexpectedly filtered on a variable-length list column"
    end subroutine scenario_filter_list_column

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

    !> A rule naming a set nothing was bound under. The name is reported, because a typo in it is
    !> the overwhelmingly likely cause and nothing else in the message would identify it.
    subroutine scenario_filter_set_unbound_name()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%bind("wanted", [1_int32, 2_int32])
        call filt%add("ra in @wnated")            ! the name is misspelt
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader naming a set that is not bound"
    end subroutine scenario_filter_set_unbound_name

    !> `in` takes a BOUND SET named with a leading '@', never a literal. Refused in the tokenizer,
    !> so the message names the operator and shows the offending text.
    subroutine scenario_filter_set_missing_at()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("ra in 5")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an 'in' clause taking a literal"
    end subroutine scenario_filter_set_missing_at

    !> `in` with nothing after it at all.
    subroutine scenario_filter_set_no_value()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("ra in")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a valueless 'in' clause"
    end subroutine scenario_filter_set_no_value

    !> A REAL set against an INTEGER column, refused exactly as `"id == 1.5"` is: the set's element
    !> family decides which columns it may be compared against.
    subroutine scenario_filter_set_wrong_family()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_set_wrong_family.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", [1_int32, 2_int32, 3_int32])
        call parquet_close_writer(writer)

        call filt%add_in("id", [1.5_real64, 2.5_real64])
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a real set against an integer column"
    end subroutine scenario_filter_set_wrong_family

    !> An INTEGER set against a STRING column: the same family rule from the other side.
    subroutine scenario_filter_set_on_string_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_set_string_column.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "name", ["alpha", "beta ", "gamma"])
        call parquet_close_writer(writer)

        call filt%add_in("name", [1_int32, 2_int32])
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an integer set against a string column"
    end subroutine scenario_filter_set_on_string_column

    !> A NaN INSIDE a bound real set, refused at %bind rather than at apply: the filter compares a
    !> NaN as IEEE does, so a NaN member could never match any row and is almost certainly a
    !> mistake. The same reason `"x == nan"` is refused as a literal.
    subroutine scenario_filter_set_nan_member()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_filter) :: filt
        real(real64) :: members(3)

        members = [1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 2.0_real64]
        call filt%bind("s", members)
        print '(a)', "unexpectedly accepted a NaN inside a bound real set"
    end subroutine scenario_filter_set_nan_member

    !> Two sets bound under one name: the second would silently shadow or be shadowed by the first,
    !> and which of the two a rule then meant would depend on lookup order.
    subroutine scenario_filter_set_duplicate_name()
        type(parquet_filter) :: filt

        call filt%bind("s", [1_int32, 2_int32])
        call filt%bind("s", [3_int32, 4_int32])
        print '(a)', "unexpectedly accepted two sets bound under one name"
    end subroutine scenario_filter_set_duplicate_name

    !> A set name containing a space could never be written in a rule, since the lexer splits on
    !> whitespace -- so it is refused where it is created rather than where it fails to parse.
    subroutine scenario_filter_set_name_with_space()
        type(parquet_filter) :: filt

        call filt%bind("two words", [1_int32])
        print '(a)', "unexpectedly accepted a set name containing a space"
    end subroutine scenario_filter_set_name_with_space

    !> The '@' belongs in the rule text, not in the name -- accepting it here would make
    !> `%bind("@s", ...)` and `%bind("s", ...)` two different sets that look identical in a rule.
    subroutine scenario_filter_set_name_with_at()
        type(parquet_filter) :: filt

        call filt%bind("@s", [1_int32])
        print '(a)', "unexpectedly accepted a set name containing '@'"
    end subroutine scenario_filter_set_name_with_at

    !> A blank set name.
    subroutine scenario_filter_set_blank_name()
        type(parquet_filter) :: filt

        call filt%bind("   ", [1_int32])
        print '(a)', "unexpectedly accepted a blank set name"
    end subroutine scenario_filter_set_blank_name

    !> An is_valid= mask that does not match the array it masks -- there is no reading of "these
    !> five values, four of which are valid".
    subroutine scenario_filter_set_mask_length()
        type(parquet_filter) :: filt

        call filt%bind("s", [1_int32, 2_int32, 3_int32], is_valid=[.true., .false.])
        print '(a)', "unexpectedly accepted an is_valid mask of the wrong length"
    end subroutine scenario_filter_set_mask_length

    !> More bound sets than parquet_max_filter_sets. A sanity bound on the number of DISTINCT sets,
    !> not on any set's length: one set of ten million identifiers is ordinary use.
    subroutine scenario_filter_set_too_many()
        type(parquet_filter) :: filt
        character(len=8) :: name
        integer :: k

        do k = 1, parquet_max_filter_sets + 1
            write(name, '(a,i0)') "s", k
            call filt%bind(trim(name), [int(k, int32)])
        end do
        print '(a)', "unexpectedly accepted more bound sets than the published limit"
    end subroutine scenario_filter_set_too_many

    !> A set clause on a VECTOR column, refused for the reason every filter column is: a vector row
    !> holds no single value to compare against a set.
    subroutine scenario_filter_set_vector_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: vec(2, 3)
        character(len=*), parameter :: file = "test_run/filter_set_vector_column.parquet"

        vec(:, 1) = [1_int64, 2_int64]
        vec(:, 2) = [3_int64, 4_int64]
        vec(:, 3) = [5_int64, 6_int64]
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call filt%add_in("vec", [1_int64, 2_int64])
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a set clause on a vector column"
    end subroutine scenario_filter_set_vector_column

    !> A literal list must reach print_stat's expression line EXACTLY as the caller wrote it,
    !> unusual spacing included. That is what the plan asked for and what the verbatim capture in
    !> capture_literal_list (src/parquet_read_filter.f90) delivers -- reassembling the list from its
    !> tokens would compile, run, select the right rows, and quietly print `( 1,2,3 )` instead. The
    !> only observable is this line, so nothing else in the suite can see it.
    subroutine scenario_filter_list_expr_text()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_list_expr_text.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", [1_int32, 2_int32, 3_int32, 4_int32])
        call parquet_close_writer(writer)

        ! Deliberately irregular spacing, so a reassembled list could not reproduce it by accident.
        call filt%add("id in ( 1,3 , 4)")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader, print_stat=.true.)
        print '(a,i0)', "literal list expr_text scenario read rows = ", nrows
    end subroutine scenario_filter_list_expr_text

    !> A bare "." as a list element. gfortran and nagfor reject it through `read`'s own iostat;
    !> FLANG accepts it as 0.0 with iostat 0 (measured), so without the mantissa-digit check this
    !> rule would silently mean `x in (0.0)` on one compiler in the fleet and abort on the others.
    !> The scenario therefore discriminates only under flang -- and asserts the right behaviour
    !> everywhere.
    subroutine scenario_filter_list_bare_dot()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_bare_dot.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("x in (.)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a bare '.' as a list element"
    end subroutine scenario_filter_list_bare_dot

    !> Fortran's own `d` exponent is refused, which is a NARROWING and deliberately so: C++'s
    !> strtod does not accept `x == 1d3` for a bare literal, so accepting it inside a list would
    !> make the list the more permissive of the two parsers -- the divergence this shape check
    !> exists to forbid. `read` would happily accept it, so only the shape check refuses.
    subroutine scenario_filter_list_fortran_exponent()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_fortran_exponent.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("x in (1d3)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a Fortran 'd' exponent in a list element"
    end subroutine scenario_filter_list_fortran_exponent

    !> Two numbers in ONE element -- `(1 2)` rather than `(1, 2)`. The hazard CLAUDE.md records for
    !> a list-directed `read`: it accepts "5 6" with iostat 0 and quietly yields 5, so without the
    !> hand-rolled shape check this rule would silently mean `id in (1)` and select a row set nobody
    !> asked for. The abort is what proves the shape check runs before the read.
    subroutine scenario_filter_list_two_numbers()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_two_numbers.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1 2)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with two numbers in one integer list element"
    end subroutine scenario_filter_list_two_numbers

    !> The same hazard on the REAL parser, which has its own shape check: a process aborts once, so
    !> the integer scenario above cannot also cover this one.
    subroutine scenario_filter_list_two_reals()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_two_reals.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("x in (1.0 2.0)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with two numbers in one real list element"
    end subroutine scenario_filter_list_two_reals

    !> An empty literal list is refused rather than silently matching nothing: a set that matches
    !> nothing is expressible (%bind with a zero-length array), so `()` in rule text is a typo.
    subroutine scenario_filter_list_empty()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_empty.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in ()")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an empty literal list"
    end subroutine scenario_filter_list_empty

    !> A quoted element against a numeric column: the quoting must agree with the column, which is
    !> the one thing about a list element that IS read off the text rather than the column type.
    subroutine scenario_filter_list_quoted_on_numeric()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_quoted_numeric.parquet"

        call write_list_scenario_fixture(file)
        call filt%add('id in ("2")')
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a quoted list element on an integer column"
    end subroutine scenario_filter_list_quoted_on_numeric

    !> The mirror: a bare element against a string column.
    subroutine scenario_filter_list_unquoted_on_string()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_unquoted_string.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("name in (a, b)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unquoted list element on a string column"
    end subroutine scenario_filter_list_unquoted_on_string

    !> An element that is not a number at all. The message names the element and its position, so a
    !> long list does not have to be read back character by character to find the offender.
    subroutine scenario_filter_list_bad_number()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_bad_number.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1, zz)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a non-numeric list element"
    end subroutine scenario_filter_list_bad_number

    !> A fractional element against an INTEGER column, which strtoll refuses for `id == 1.5` too --
    !> the list's element grammar follows the column's family, so it refuses it for the same reason.
    subroutine scenario_filter_list_non_integer()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_non_integer.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1, 1.5)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a fractional element on an integer column"
    end subroutine scenario_filter_list_non_integer

    !> A NaN member is refused for the reason `x == nan` is: every comparison against a NaN is
    !> false, so it could only ever match nothing. Refusing it is also what makes the NaN ROW rule
    !> hold by construction -- no NaN pattern is ever a key, so a NaN row's lookup finds nothing.
    subroutine scenario_filter_list_nan_member()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_nan_member.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("x in (1.0, nan)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a NaN inside a literal list"
    end subroutine scenario_filter_list_nan_member

    !> A trailing comma leaves an empty element, which is reported as such rather than silently
    !> dropped -- a dropped one would quietly change what the filter selects.
    subroutine scenario_filter_list_trailing_comma()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_trailing_comma.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1, 2, )")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a trailing comma in a literal list"
    end subroutine scenario_filter_list_trailing_comma

    !> A '(' inside a list is refused rather than read as a grouping operator: inside a list the
    !> parenthesis has already changed meaning once, and letting it change back would make
    !> `id in (1, (2))` parse as something no reader could predict.
    subroutine scenario_filter_list_nested_paren()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_nested_paren.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1, (2))")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a nested parenthesis in a literal list"
    end subroutine scenario_filter_list_nested_paren

    !> An unclosed list runs to the end of the rule and is reported there.
    subroutine scenario_filter_list_unclosed()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_unclosed.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id in (1, 2")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unclosed literal list"
    end subroutine scenario_filter_list_unclosed

    !> An element that opens a quote and does not close it before the comma or the ')'.
    subroutine scenario_filter_list_unclosed_quote()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_unclosed_quote.parquet"

        call write_list_scenario_fixture(file)
        call filt%add('name in ("a" b)')
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with an unterminated quoted list element"
    end subroutine scenario_filter_list_unclosed_quote

    !> A BOOLEAN column takes no set clause at all: a boolean set is `==` with extra steps, so
    !> accepting one would add a spelling with no meaning of its own.
    subroutine scenario_filter_list_on_bool_column()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_list_bool.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("flag in (1)")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a set clause on a boolean column"
    end subroutine scenario_filter_list_on_bool_column

    !> A set clause on a TEMPORAL column is refused by the set rules, naming the column's type --
    !> not by the temporal literal conversion, which would otherwise report a missing ISO-8601
    !> literal and say nothing about the set.
    subroutine scenario_filter_set_temporal_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_schema) :: sch
        type(parquet_date) :: d(3)
        character(len=*), parameter :: file = "test_run/filter_set_temporal.parquet"

        call d(1)%set(2024, 1, 1)
        call d(2)%set(2024, 1, 2)
        call d(3)%set(2024, 1, 3)
        call sch%init("temporal_set")
        call sch%add_field("d", "date")
        call parquet_parse_maml(sch)
        call parquet_open_writer(writer, file, sch)
        call parquet_write_column(writer, "d", d)
        call parquet_close_writer(writer)

        call filt%add_in("d", [1_int32, 2_int32])
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a set clause on a date column"
    end subroutine scenario_filter_set_temporal_column

    !> is_finite is restricted to floating-point columns, exactly as is_nan is: no integer value
    !> could ever be non-finite, so accepting one would answer a constant for what is almost
    !> certainly a mistyped column name.
    subroutine scenario_is_finite_on_int_column()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_is_finite_int.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("id is_finite")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with is_finite on an integer column"
    end subroutine scenario_is_finite_on_int_column

    !> is_finite takes no value, so a value after it is a syntax error rather than a silently
    !> ignored token.
    subroutine scenario_is_finite_takes_no_value()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=*), parameter :: file = "test_run/filter_is_finite_value.parquet"

        call write_list_scenario_fixture(file)
        call filt%add("x is_finite 3")
        call parquet_open_reader(reader, file, filter=filt)
        print '(a)', "unexpectedly opened a reader with a value after is_finite"
    end subroutine scenario_is_finite_takes_no_value

    !> The NEGATIVE CONTROL for the eighteen refusals above: a well-formed literal list and a
    !> well-formed is_finite clause in one rule must open cleanly and exit 0. Without it every
    !> assertion above would pass just as happily against a guard that refused everything.
    subroutine scenario_filter_list_control()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_list_control.parquet"

        call write_list_scenario_fixture(file)
        call filt%add('id in (1, 3) and x is_finite and name not_in ("d")')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        print '(a,i0)', "a well-formed literal list and is_finite opened cleanly, rows = ", nrows
    end subroutine scenario_filter_list_control

    ! ---- The table's in-memory filter evaluator: %row_mask and %filter_rows(expr/filter) --------
    !
    ! Every one of these asserts that the TABLE reports a refusal the reader also makes, with the
    ! table's own context attached ("parquet_table: row_mask: ..."). That pairing is the point: the
    ! two engines share the checks (parquet_resolve_set_payload, parquet_check_set_column_shape,
    ! the literal parsers), so a scenario here proves the shared half is reached from this side and
    ! that the message is not silently the reader's.

    !> The fixture every row_mask scenario reads: one column of each shape a clause can name.
    subroutine write_row_mask_fixture(file)
        character(len=*), intent(in) :: file !! this scenario's own fixture path.
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: vec(2, 4)
        integer :: i

        do i = 1, 4
            vec(1, i) = i
            vec(2, i) = 10 + i
        end do
        call schema%init("row_mask_scenarios")
        call schema%add_field("id", "int32")
        call schema%add_field("x", "float64")
        call schema%add_field("flag", "boolean")
        call schema%add_field("name", "string")
        call schema%add_field("pair", "int32", col_size=2)
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "id", [1_int32, 2_int32, 3_int32, 4_int32])
        call parquet_write_column(writer, "x", [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call parquet_write_column(writer, "flag", [.true., .false., .true., .false.])
        call parquet_write_column(writer, "name", ["a   ", "b   ", "c   ", "d   "])
        call parquet_write_column(writer, "pair", vec)
        call parquet_close_writer(writer)
    end subroutine write_row_mask_fixture

    !> Opens the shared fixture as a table and applies one rule through %row_mask.
    subroutine row_mask_scenario(file, rule)
        character(len=*), intent(in) :: file !! this scenario's own fixture path.
        character(len=*), intent(in) :: rule !! the rule to apply.
        type(parquet_table) :: t
        logical, allocatable :: keep(:)

        call write_row_mask_fixture(file)
        call parquet_open_table(t, file)
        allocate(keep(t%nrows()))
        call t%row_mask(rule, keep)
        print '(a,i0)', "%row_mask was accepted, kept = ", count(keep)
    end subroutine row_mask_scenario

    !> A mask whose length disagrees with the table -- the evaluator's own first guard.
    subroutine scenario_row_mask_wrong_size()
        type(parquet_table) :: t
        logical :: keep(3)
        character(len=*), parameter :: file = "test_run/row_mask_wrong_size.parquet"

        call write_row_mask_fixture(file)
        call parquet_open_table(t, file)
        call t%row_mask("id > 1", keep)
        print '(a,i0)', "a short mask was accepted, kept = ", count(keep)
    end subroutine scenario_row_mask_wrong_size

    !> A syntactically invalid rule, reported through the table rather than the reader.
    subroutine scenario_row_mask_bad_rule()
        call row_mask_scenario("test_run/row_mask_bad_rule.parquet", "id >")
    end subroutine scenario_row_mask_bad_rule

    !> A clause naming a column the table does not have.
    subroutine scenario_row_mask_unknown_column()
        call row_mask_scenario("test_run/row_mask_unknown_column.parquet", "nope > 1")
    end subroutine scenario_row_mask_unknown_column

    !> A VECTOR column has no single value per row to compare, which is why every filter refuses
    !> one -- here through the shape token the evaluator derives from the resident column.
    subroutine scenario_row_mask_vector_column()
        call row_mask_scenario("test_run/row_mask_vector_column.parquet", "pair > 1")
    end subroutine scenario_row_mask_vector_column

    !> An integer bound no int32 can hold is a mistake in the rule, not a filter matching nothing.
    subroutine scenario_row_mask_int32_range()
        call row_mask_scenario("test_run/row_mask_int32_range.parquet", "id > 3000000000")
    end subroutine scenario_row_mask_int32_range

    !> A bound that is not a whole number against an integer column.
    subroutine scenario_row_mask_bad_integer()
        call row_mask_scenario("test_run/row_mask_bad_integer.parquet", "id > 1.5")
    end subroutine scenario_row_mask_bad_integer

    !> `nan` as a comparison bound: it can only ever match nothing, so both engines refuse it and
    !> point at is_nan instead.
    subroutine scenario_row_mask_nan_bound()
        call row_mask_scenario("test_run/row_mask_nan_bound.parquet", "x > nan")
    end subroutine scenario_row_mask_nan_bound

    !> Ordering comparisons are meaningless on a boolean column.
    subroutine scenario_row_mask_bool_ordering()
        call row_mask_scenario("test_run/row_mask_bool_ordering.parquet", "flag > true")
    end subroutine scenario_row_mask_bool_ordering

    !> A string column's bound must be double-quoted, or the rule is comparing against a name.
    subroutine scenario_row_mask_unquoted_string()
        call row_mask_scenario("test_run/row_mask_unquoted_string.parquet", "name == a")
    end subroutine scenario_row_mask_unquoted_string

    !> The four value-class operators are floating-point only.
    subroutine scenario_row_mask_is_nan_on_int()
        call row_mask_scenario("test_run/row_mask_is_nan_on_int.parquet", "id is_nan")
    end subroutine scenario_row_mask_is_nan_on_int

    !> The in-memory twin of filter_starts_with_non_string_column: the table's engine refuses a
    !> matcher on a non-string column too, and says the same thing about it. Two engines answer one
    !> grammar, so a refusal proved on the reader alone is proved on half the library.
    subroutine scenario_row_mask_starts_with_on_int()
        call row_mask_scenario("test_run/row_mask_starts_with_on_int.parquet", 'id starts_with "1"')
    end subroutine scenario_row_mask_starts_with_on_int

    !> A `@name` clause needs the set bound to it, and a bare expression cannot carry one -- so the
    !> string form of %row_mask reports the unbound name exactly as the reader does.
    subroutine scenario_row_mask_unbound_set()
        call row_mask_scenario("test_run/row_mask_unbound_set.parquet", "id in @wanted")
    end subroutine scenario_row_mask_unbound_set

    !> A literal list whose elements are quoted against a numeric column: the shared payload
    !> resolution reporting through the table's context rather than the reader's.
    subroutine scenario_row_mask_list_quoted()
        call row_mask_scenario("test_run/row_mask_list_quoted.parquet", 'id in ("1", "2")')
    end subroutine scenario_row_mask_list_quoted

    !> A temporal literal finer than the column's stored unit is refused rather than truncated --
    !> the reader's rule, reached here through the unit the table recorded at classification.
    subroutine scenario_row_mask_temporal_precision()
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_table) :: t
        type(parquet_timestamp) :: ts(3)
        logical, allocatable :: keep(:)
        integer :: i
        character(len=*), parameter :: file = "test_run/row_mask_temporal_precision.parquet"

        do i = 1, 3
            call ts(i)%set(2024, 1, 31, 12, 30, i - 1)
        end do
        call schema%init("row_mask_temporal")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)

        call parquet_open_table(t, file)
        allocate(keep(t%nrows()))
        call t%row_mask('ts > "2024-01-31T12:30:00.0005"', keep)
        print '(a,i0)', "a sub-millisecond literal was accepted, kept = ", count(keep)
    end subroutine scenario_row_mask_temporal_precision

    !> The three CONTAINER shapes, which the evaluator names from the resident column's kind rather
    !> than from a schema. A list, a map and a struct each have many values per row, so a clause on
    !> one has nothing to compare -- the same refusal a vector column gets, in the words
    !> `parquet_check_set_column_shape` keeps for each shape. Three scenarios rather than one
    !> because the three words are three separate arms, and a shape that fell through to the
    !> default would still abort, just with the token instead of the phrase.
    !>
    !> Built IN MEMORY, and both halves of that are forced rather than chosen. A top-level struct is
    !> never enumerated under its own name -- `parquet_get_column_names` expands one into a dotted
    !> path per leaf -- so no file-backed table ever classifies a struct column at all. And a list
    !> whose rows all have the same length becomes an ordinary vector column under the default
    !> `list_columns="auto"`, which would prove the vector arm a second time instead of the list one;
    !> the rows here are 1, 2 and 3 long so that the column stays a list however it is opened.
    subroutine row_mask_container_scenario(rule)
        character(len=*), intent(in) :: rule !! the rule to apply.
        type(parquet_table) :: t
        type(parquet_list_column) :: lc
        type(parquet_map_column) :: mc
        type(parquet_struct_column) :: sc
        logical, allocatable :: keep(:)
        character(len=8) :: fields(1)
        integer :: kinds(1)

        call lc%init(PK_INT32)
        call lc%append_row([10_int32])
        call lc%append_row([20_int32, 30_int32])
        call lc%append_row([40_int32, 50_int32, 60_int32])
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_row(["b"], [2_int32])
        call mc%append_row(["c"], [3_int32])
        fields(1) = "v"
        kinds(1) = PK_INT32
        call sc%init(fields, kinds, 3_int64)

        call parquet_new_table(t)
        call t%add_column("lst", lc)
        call t%add_column("mp", mc)
        call t%add_column("st", sc)
        allocate(keep(t%nrows()))
        call t%row_mask(rule, keep)
        print '(a,i0)', "%row_mask on a container column was accepted, kept = ", count(keep)
    end subroutine row_mask_container_scenario

    !> A clause on a variable-length LIST column.
    subroutine scenario_row_mask_list_column()
        call row_mask_container_scenario("lst > 1")
    end subroutine scenario_row_mask_list_column

    !> A clause on a MAP column.
    subroutine scenario_row_mask_map_column()
        call row_mask_container_scenario("mp > 1")
    end subroutine scenario_row_mask_map_column

    !> A clause on a STRUCT column.
    subroutine scenario_row_mask_struct_column()
        call row_mask_container_scenario("st > 1")
    end subroutine scenario_row_mask_struct_column

    !> A QUOTED bound against an integer column. The quoting is the whole message: `id == "5"` is a
    !> caller comparing a number against text, and taking the quotes off silently would make the
    !> two engines disagree with the C++ one, which reads the same rule as a string comparison.
    subroutine scenario_row_mask_quoted_integer()
        call row_mask_scenario("test_run/row_mask_quoted_integer.parquet", 'id == "5"')
    end subroutine scenario_row_mask_quoted_integer

    !> The same mistake against a floating-point column, which has its own arm and its own wording.
    subroutine scenario_row_mask_quoted_real()
        call row_mask_scenario("test_run/row_mask_quoted_real.parquet", 'x == "1.5"')
    end subroutine scenario_row_mask_quoted_real

    !> An unquoted bound on a float column that is not a number and not a NaN spelling either. The
    !> NaN spellings get their own message (scenario_row_mask_nan_bound); everything else lands
    !> here, and the two arms are separate precisely so that `nan` can point at is_nan instead.
    subroutine scenario_row_mask_bad_real()
        call row_mask_scenario("test_run/row_mask_bad_real.parquet", "x > abc")
    end subroutine scenario_row_mask_bad_real

    !> A quoted bound on a BOOLEAN column: true and false are written unquoted, and a quoted one is
    !> a string comparison against a column that holds no strings.
    subroutine scenario_row_mask_quoted_bool()
        call row_mask_scenario("test_run/row_mask_quoted_bool.parquet", 'flag == "true"')
    end subroutine scenario_row_mask_quoted_bool

    !> An unquoted bound on a boolean column that is not true or false. Accepting a number here (C's
    !> "nonzero is true") would answer a rule the library never promised to read that way.
    subroutine scenario_row_mask_bad_bool()
        call row_mask_scenario("test_run/row_mask_bad_bool.parquet", "flag == 7")
    end subroutine scenario_row_mask_bad_bool

    !> A matcher's pattern must be double-quoted even on a string column -- `starts_with a` names
    !> a bare token, and the three matchers share one guard for it.
    subroutine scenario_row_mask_unquoted_match()
        call row_mask_scenario("test_run/row_mask_unquoted_match.parquet", "name starts_with a")
    end subroutine scenario_row_mask_unquoted_match

    !> An UNQUOTED bound against a temporal column. A temporal literal is ISO-8601 text, so the
    !> refusal names the spelling it wants rather than trying to read 5 as an instant.
    subroutine scenario_row_mask_unquoted_temporal()
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_table) :: t
        type(parquet_timestamp) :: ts(3)
        logical, allocatable :: keep(:)
        integer :: i
        character(len=*), parameter :: file = "test_run/row_mask_unquoted_temporal.parquet"

        do i = 1, 3
            call ts(i)%set(2024, 1, 31, 12, 30, i - 1)
        end do
        call schema%init("row_mask_unquoted_temporal")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)

        call parquet_open_table(t, file)
        allocate(keep(t%nrows()))
        call t%row_mask("ts > 5", keep)
        print '(a,i0)', "an unquoted temporal bound was accepted, kept = ", count(keep)
    end subroutine scenario_row_mask_unquoted_temporal

    !> %filter_rows is row-structural, so a table shared across threads refuses it -- and the two
    !! EXPRESSION forms have to run that guard themselves, because they build the mask first and
    !! reach table_apply_keep only afterwards. A copy that forgot the guard would read every column
    !! the rule names before discovering it may not change anything.
    !!
    !! In the concurrency bucket rather than the strict list, for the reason that bucket exists:
    !! without a real -fopenmp build there is no region to be inside, the change simply succeeds
    !! and the scenario exits 0. `!$omp single` makes it deterministic when OpenMP IS active.
    subroutine scenario_table_filter_rows_shared()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/table_filter_rows_shared.parquet"

        call write_row_mask_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%filter_rows("id > 1")   ! row-structural change to a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly filtered a shared table in a region, nrows=", t%nrows()
    end subroutine scenario_table_filter_rows_shared

    !> The negative control for every scenario above: the same fixture, a rule that names a scalar
    !> column of each family, applied through both entry points. Without it, a guard that refused
    !> everything would pass all fourteen.
    subroutine scenario_row_mask_control()
        type(parquet_table) :: t
        logical, allocatable :: keep(:)
        character(len=*), parameter :: file = "test_run/row_mask_control.parquet"

        call write_row_mask_fixture(file)
        call parquet_open_table(t, file)
        allocate(keep(t%nrows()))
        call t%row_mask('id > 1 and x < 4.0 and flag == true and name /= "d" and id in (2, 3)', keep)
        call t%filter_rows("id > 1")
        print '(a,i0,a,i0)', "%row_mask kept ", count(keep), " and %filter_rows left ", t%nrows()
    end subroutine scenario_row_mask_control

    ! ---- The missing-data family (%fillna / %ffill / %bfill / %dropna) -------------------------
    !
    ! Every fixture below is written by write_fill_fixture, one file per scenario name because the
    ! runner dispatches these concurrently.

    !> Two columns with nulls in the same rows, one int32 and one float64, plus a null-free one.
    subroutine write_fill_fixture(file)
        character(len=*), intent(in) :: file !! this scenario's own fixture path.
        type(parquet_writer) :: writer
        integer(int32) :: v(4) = [1, 0, 3, 0]
        real(real64) :: x(4) = [1.0_real64, 0.0_real64, 3.0_real64, 0.0_real64]
        integer(int32) :: u(4) = [1, 2, 3, 4]
        logical :: valid(4) = [.true., .false., .true., .false.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "x", x, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
    end subroutine write_fill_fixture

    !> A real sentinel reaching an integer column is a mistake worth naming, and the message has
    !! to name the COLUMN: the whole point of %fillna taking a name list is that one call fills
    !! columns of several kinds, so "a real value cannot fill an int32 column" without a name
    !! would leave the caller to find which of forty-four it was.
    subroutine scenario_fillna_real_into_integer()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_real_into_integer.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("v", -999.9_real64)   ! a real value, an int32 column -> aborts
        print '(a,i0)', "unexpectedly filled an integer column with a real, nrows=", t%nrows()
    end subroutine scenario_fillna_real_into_integer

    !> A logical fills only a logical column. Its own family and nothing else.
    subroutine scenario_fillna_logical_into_numeric()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_logical_into_numeric.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("v", .true.)   ! -> aborts
        print '(a,i0)', "unexpectedly filled a numeric column with a logical, nrows=", t%nrows()
    end subroutine scenario_fillna_logical_into_numeric

    !> ... and a character fills only a string column.
    subroutine scenario_fillna_string_into_numeric()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_string_into_numeric.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("x", "-999")   ! -> aborts
        print '(a,i0)', "unexpectedly filled a real column with a string, nrows=", t%nrows()
    end subroutine scenario_fillna_string_into_numeric

    !> The other direction of the same rule: a number does not fill a string column either, so
    !! there is no implicit formatting on the way in.
    subroutine scenario_fillna_integer_into_string()
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_integer_into_string.parquet"
        character(len=4) :: s(2) = ["ab  ", "    "]
        logical :: valid(2) = [.true., .false.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "s", s, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%fillna("s", 0_int32)   ! -> aborts
        print '(a,i0)', "unexpectedly filled a string column with an integer, nrows=", t%nrows()
    end subroutine scenario_fillna_integer_into_string

    !> The one check that is about the VALUE rather than the families. An int64 that does not fit
    !! an int32 column would wrap silently, which is a quiet wrong answer of exactly the kind the
    !! rest of this library refuses to produce -- and the families alone cannot catch it, since an
    !! integer value IS accepted for an integer column.
    subroutine scenario_fillna_int32_range()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_int32_range.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("v", 3000000000_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an int32 column out of range, nrows=", t%nrows()
    end subroutine scenario_fillna_int32_range

    !> There is no meaning to replacing a missing LIST with a scalar: filling it with -999 would
    !! have to invent a length as well as a value.
    subroutine scenario_fillna_container_column()
        type(parquet_table) :: t

        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="container")
        call t%materialize_all()
        call t%fillna("ragged", -999_int32)   ! -> aborts
        print '(a,i0)', "unexpectedly filled a container column, nrows=", t%nrows()
    end subroutine scenario_fillna_container_column

    !> A column whose file type this library cannot read has no values to fill, so the message has
    !! to say that rather than report a kind mismatch against PK_NONE.
    subroutine scenario_fillna_unsupported_column()
        type(parquet_table) :: t

        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%fillna("m_intkey", 0_int32)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an unsupported column, ncols=", t%ncols()
    end subroutine scenario_fillna_unsupported_column

    !> A misspelt name in the list aborts naming it, rather than filling the columns that do exist
    !! and saying nothing about the one that does not.
    subroutine scenario_fillna_unknown_column()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_unknown_column.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("v, nope", 0_int32)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an unknown column, nrows=", t%nrows()
    end subroutine scenario_fillna_unknown_column

    !> `limit=0` fills nothing, which is a call that cannot have been meant: leaving the argument
    !! out is how a caller asks for no cap, so zero is a mistake rather than a degenerate request.
    subroutine scenario_ffill_limit_zero()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/ffill_limit_zero.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%ffill("v", 0)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted limit=0, nrows=", t%nrows()
    end subroutine scenario_ffill_limit_zero

    !> A negative limit likewise. -1 is the INTERNAL spelling of "no cap" and must not be reachable
    !! from a caller, or `limit=-1` would silently mean the opposite of what it says.
    subroutine scenario_ffill_limit_negative()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/ffill_limit_negative.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%bfill("v", -1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative limit, nrows=", t%nrows()
    end subroutine scenario_ffill_limit_negative

    !> A container row cannot be carried from its neighbour either -- unlike %fillna there is no
    !! value to convert, but there is still no defined copy of one list row onto another here.
    subroutine scenario_ffill_container_column()
        type(parquet_table) :: t

        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="container")
        call t%materialize_all()
        call t%ffill("ragged")   ! -> aborts
        print '(a,i0)', "unexpectedly forward-filled a container column, nrows=", t%nrows()
    end subroutine scenario_ffill_container_column

    !> min_valid REPLACES how rather than refining it, so a call carrying both has two answers and
    !! the library must not pick one. pandas refuses the same combination.
    subroutine scenario_dropna_how_and_min_valid()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/dropna_how_and_min_valid.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna("v, x", min_valid=1, how="all")   ! -> aborts
        print '(a,i0)', "unexpectedly accepted how= and min_valid= together, nrows=", t%nrows()
    end subroutine scenario_dropna_how_and_min_valid

    !> An unrecognised `how` token has to name what was expected: a silent fall-back to "any"
    !! would make a typo drop far more rows than the caller asked for.
    subroutine scenario_dropna_bad_how()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/dropna_bad_how.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna("v, x", how="either")   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a bad how token, nrows=", t%nrows()
    end subroutine scenario_dropna_bad_how

    !> A threshold above the number of columns named can never be met, so it empties the table --
    !! which is a mistake rather than a request, and is refused naming both numbers.
    subroutine scenario_dropna_min_valid_range()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/dropna_min_valid_range.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna("v, x", min_valid=3)   ! only two columns named -> aborts
        print '(a,i0)', "unexpectedly accepted an out-of-range min_valid, nrows=", t%nrows()
    end subroutine scenario_dropna_min_valid_range

    !> A negative threshold is nonsense at any column count, so it is checked before the column
    !! count is even known -- which is what stops the range check from being the only guard.
    subroutine scenario_dropna_min_valid_negative()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/dropna_min_valid_negative.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna("v, x", min_valid=-1)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative min_valid, nrows=", t%nrows()
    end subroutine scenario_dropna_min_valid_negative

    !> The same typo as `dropna_bad_how`, but on a table where NOTHING is resident -- so there is
    !! no work for %dropna to do and an argument check placed after the "nothing to look at" early
    !! return would silently accept it. A guard that fires on some tables and not others is worse
    !! than no guard, because the first run that misses it teaches the caller the token is fine.
    subroutine scenario_dropna_bad_how_nothing_resident()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/dropna_bad_how_nothing_resident.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%dropna(how="either")   ! no resident column, but still a bad token -> aborts
        print '(a,i0)', "unexpectedly accepted a bad how token on a lazy table, nrows=", t%nrows()
    end subroutine scenario_dropna_bad_how_nothing_resident

    !> %fillna drops a column's null bitmap when the last null goes, which is a change to the
    !! column's STORAGE and not merely to its values -- a concurrent %is_null would read through
    !! it. Refused on a shared table for the same reason %compact_validity is.
    !!
    !! In the concurrency bucket, for the reason that bucket exists: without a real -fopenmp build
    !! there is no region to be inside, so the fill simply succeeds and the scenario exits 0.
    subroutine scenario_fillna_shared()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fillna_shared.parquet"

        call write_fill_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%fillna("v", 0_int32)   ! storage change to a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly filled a shared table in a region, nrows=", t%nrows()
    end subroutine scenario_fillna_shared

    !> The negative control for every fill scenario above: the same fixture, filled through each
    !> of the three storage classes, scanned in both directions with and without a limit, and
    !> dropped by both policies. Without it a guard that refused everything would pass all
    !> fourteen.
    subroutine scenario_fill_control()
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/fill_control.parquet"
        character(len=4) :: s(4) = ["ab  ", "    ", "cd  ", "    "]
        type(parquet_date) :: d(4)
        logical :: valid(4) = [.true., .false., .true., .false.]
        integer(int32) :: v(4) = [1, 0, 3, 0]
        real(real64) :: x(4) = [1.0_real64, 0.0_real64, 3.0_real64, 0.0_real64]

        call d(1)%set(2020, 1, 1)
        call d(3)%set(2020, 3, 3)
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "x", x, is_valid=valid)
        call parquet_write_column(writer, "s", s, is_valid=valid)
        call parquet_write_column(writer, "d", d)
        call parquet_close_writer(writer)

        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%fillna("v", -9_int32)        ! bitmap class, exact kind
        call t%fillna("x", -9_int32)        ! bitmap class, widened
        call t%fillna("s", "")              ! string class
        call t%ffill("d")                   ! temporal class, carried forward
        call t%bfill("d", 1)                ! ... and back, with a cap
        call t%dropna("v, x", how="all")
        call t%dropna(["v"], min_valid=1)
        print '(a,i0,a,l1)', "every fill path ran, nrows=", t%nrows(), " nulls in v: ", t%has_nulls("v")
    end subroutine scenario_fill_control

    !> The fixture every matrix scenario works from: five columns covering the four ways a
    !> column can be refused -- an exact-kind pair to succeed with, an int32 (right family,
    !> wrong kind), a string (wrong family altogether) and a vector (right element kind, wrong
    !> rank). Built in memory rather than written, since none of these guards reads a file.
    subroutine build_matrix_fixture(t)
        type(parquet_table), intent(out) :: t !! the table to build.
        real(real64) :: vec(2, 3)

        vec = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
        call parquet_new_table(t)
        call t%add_column("a", [1.0_real64, 2.0_real64, 3.0_real64])
        call t%add_column("b", [4.0_real64, 5.0_real64, 6.0_real64])
        call t%add_column("i", [7_int32, 8_int32, 9_int32])
        call t%add_column("f", [1.5_real32, 2.5_real32, 3.5_real32])
        call t%add_column("s", [character(len=2) :: "aa", "bb", "cc"])
        call t%add_column("vec", vec)
    end subroutine build_matrix_fixture

    !> %get_matrix over a real64 matrix naming one column it cannot carry. Four scenarios share
    !> this body, because the four refusals differ only in which column is named and every one
    !> must leave the matrix unallocated -- which is what the print below would report if a
    !> guard ever stopped firing.
    subroutine scenario_get_matrix_bad_column(bad)
        character(len=*), intent(in) :: bad !! the column to name beside a legal one.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)

        call build_matrix_fixture(t)
        ! An explicit type-spec, because an array constructor takes its element length from the
        ! first element otherwise -- which silently truncated "nosuch" to "nos" and made the
        ! unknown-column scenario assert a message about a column nobody named.
        call t%get_matrix([character(len=16) :: "a", bad], m)
        print '(a,i0,a,i0)', "unexpectedly built a matrix, shape ", size(m, 1), " x ", size(m, 2)
    end subroutine scenario_get_matrix_bad_column

    !> The other half of the widening rule: %get_matrix accepts a float32 column into a real64
    !> matrix, and %set_matrix must NOT accept it back, because that would narrow silently.
    !> Without this scenario "no widening on the way back" is a doc-comment nothing checks.
    subroutine scenario_set_matrix_no_widening()
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)

        call build_matrix_fixture(t)
        call t%get_matrix("a, f", m)        ! this direction is legal ...
        call t%set_matrix("a, f", m)        ! ... and this one must not be
        print '(a,i0)', "unexpectedly narrowed a real64 matrix into a float32 column, ncols=", t%ncols()
    end subroutine scenario_set_matrix_no_widening

    !> %set_matrix's three shape guards, which all fire before any value is written.
    subroutine scenario_set_matrix_shape(which)
        character(len=*), intent(in) :: which !! "ncols", "nrows" or "mask".
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        logical, allocatable :: mask(:,:)

        call build_matrix_fixture(t)
        select case (which)
        case ("ncols")
            allocate(m(3, 3))
            m = 0.0_real64
            call t%set_matrix("a, b", m)     ! three matrix rows, two names
        case ("nrows")
            allocate(m(2, 4))
            m = 0.0_real64
            call t%set_matrix("a, b", m)     ! four rows of values, three table rows
        case default
            allocate(m(2, 3), mask(2, 2))
            m = 0.0_real64
            mask = .true.
            call t%set_matrix("a, b", m, is_valid=mask)
        end select
        print '(a,a)', "unexpectedly accepted a mis-shaped set_matrix: ", which
    end subroutine scenario_set_matrix_shape

    !> %drop_columns naming a column the table does not have, with the default policy. The
    !> message must name EVERY absent one, so two are named here rather than one.
    subroutine scenario_drop_columns_missing()
        type(parquet_table) :: t

        call build_matrix_fixture(t)
        call t%drop_columns("a, nosuch, alsonot")
        print '(a,i0)', "unexpectedly dropped an absent column, ncols=", t%ncols()
    end subroutine scenario_drop_columns_missing

    !> R8 through the list form: a predefined column may be dropped only on purpose.
    subroutine scenario_drop_columns_predefined()
        type(parquet_table_test) :: t

        call t%init_empty(2_int32)
        call t%drop_columns("ra, dec")
        print '(a,i0)', "unexpectedly dropped predefined columns, ncols=", t%ncols()
    end subroutine scenario_drop_columns_predefined

    !> %keep_columns naming something that is not there. There is no ignore_missing here, so
    !> this is the only policy it has.
    subroutine scenario_keep_columns_missing()
        type(parquet_table) :: t

        call build_matrix_fixture(t)
        call t%keep_columns("a, nosuch")
        print '(a,i0)', "unexpectedly projected onto an absent column, ncols=", t%ncols()
    end subroutine scenario_keep_columns_missing

    !> R8 reached from the other side: %keep_columns drops a predefined column by NOT naming it,
    !> which is the one route %drop_column cannot express and the reason %keep_columns has a
    !> force= of its own.
    subroutine scenario_keep_columns_predefined()
        type(parquet_table_test) :: t

        call t%init_empty(2_int32)
        call t%keep_columns("uberid")
        print '(a,i0)', "unexpectedly projected away predefined columns, ncols=", t%ncols()
    end subroutine scenario_keep_columns_predefined

    !> A slot shift on a table another thread may be holding: refused, like every other
    !> structural change.
    subroutine scenario_drop_columns_shared()
        type(parquet_table) :: t

        call build_matrix_fixture(t)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%drop_columns("a")   ! slot shift on a shared table -> aborts
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly dropped a column of a shared table, ncols=", t%ncols()
    end subroutine scenario_drop_columns_shared

    !> The negative control for every matrix scenario above: the same fixture, read and written
    !> through both matrix verbs and projected by both drop verbs. Without it a guard that
    !> refused everything would pass all thirteen.
    subroutine scenario_matrix_control()
        type(parquet_table) :: t
        type(parquet_table_test) :: g
        real(real64), allocatable :: m(:,:)
        logical, allocatable :: mask(:,:)

        call build_matrix_fixture(t)
        call t%get_matrix("a, b", m, is_valid=mask)      ! exact kind, plus the mask
        call t%set_matrix("a, b", m, is_valid=mask)      ! and straight back again
        call t%get_matrix("a, f", m)                     ! the one widening that is allowed
        call t%drop_columns("s, nosuch", ignore_missing=.true.)
        call t%keep_columns("a, b, i")
        call g%init_empty(2_int32)
        call g%drop_columns("ra", force=.true.)          ! R8, satisfied
        call g%keep_columns("uberid, flux", force=.true.)
        print '(a,i0,a,i0)', "every matrix path ran, ncols=", t%ncols(), " predefined left=", g%ncols()
    end subroutine scenario_matrix_control

    !> The fixture every conversion scenario works from: a text column that parses, one that
    !> does not, and the three kinds of column neither verb will take -- a numeric one (nothing
    !> to parse), a vector one (no per-row scalar to render) and a date (nothing a `fmt` could
    !> vary). Built in memory, since none of these guards reads a file.
    subroutine build_convert_fixture(t)
        type(parquet_table), intent(out) :: t !! the table to build.
        character(len=4) :: vec(2, 3)
        type(parquet_date) :: d(3)
        integer :: k

        vec = reshape([character(len=4) :: "a", "b", "c", "d", "e", "f"], [2, 3])
        do k = 1, 3
            call d(k)%set(2024, 1, k)
        end do
        call parquet_new_table(t)
        call t%add_column("s", [character(len=4) :: "1", "2", "3"])
        call t%add_column("bad", [character(len=4) :: "1", "2 3", "z"])
        call t%add_column("i", [7_int32, 8_int32, 9_int32])
        call t%add_column("vec", vec)
        call t%add_column("d", d)
    end subroutine build_convert_fixture

    !> %parse_column naming a column that is not a scalar string one. Two scenarios share this
    !> body: a numeric column and a string VECTOR, which are refused by the same guard for the
    !> same reason and must both name the kind they actually hold.
    subroutine scenario_parse_column_bad_source(bad)
        character(len=*), intent(in) :: bad !! the column to name.
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%parse_column(bad, PK_INT64)
        print '(a,i0)', "unexpectedly parsed a non-string column, kind is now ", t%kind(bad)
    end subroutine scenario_parse_column_bad_source

    !> A target kind %parse_column has no parser for. PK_STRING is the sharpest case: it is a
    !> real kind, and asking for it is the mistake of calling this verb when %format_column was
    !> meant, so the message says which is which.
    subroutine scenario_parse_column_bad_target()
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%parse_column("s", PK_STRING)
        print '(a,i0)', "unexpectedly parsed into an unsupported target, kind is now ", t%kind("s")
    end subroutine scenario_parse_column_bad_target

    !> An `invalid=` token that is neither "error" nor "null". Accepting it silently would apply
    !> the DEFAULT policy, i.e. abort on the first bad row -- the opposite of what a caller
    !> writing invalid="skip" is asking for.
    subroutine scenario_parse_column_invalid_token()
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%parse_column("s", PK_INT64, invalid="skip")
        print '(a,i0)', "unexpectedly accepted an unknown invalid= policy, kind is now ", t%kind("s")
    end subroutine scenario_parse_column_invalid_token

    !> The default policy meeting text it cannot read. The message must name the ROW, the COLUMN
    !> and the TEXT: a caller fixing a data file needs all three, and a message naming only the
    !> column is one they cannot act on.
    !!
    !! `long` picks the overlong-text case instead, which exercises the preview cap. That cap is
    !! not cosmetic -- ifx's ERROR STOP runtime corrupts the heap once the composed message
    !! reaches 8192 bytes, and a "this text is not a number" guard is by construction reached
    !! with text the caller controls.
    subroutine scenario_parse_column_malformed(long)
        logical, intent(in) :: long !! .true. to use a 300-character unreadable value.
        type(parquet_table) :: t
        character(len=300) :: wide(3)

        if (long) then
            wide = repeat("z", 300)
            call parquet_new_table(t)
            call t%add_column("bad", wide)
        else
            call build_convert_fixture(t)
        end if
        call t%parse_column("bad", PK_INT32)
        print '(a,i0)', "unexpectedly parsed malformed text, kind is now ", t%kind("bad")
    end subroutine scenario_parse_column_malformed

    !> `to_name` naming a column that already exists. %copy_column's rule, reached through a
    !> different verb: a new column never silently replaces one that is there.
    subroutine scenario_parse_column_to_name_exists()
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%parse_column("s", PK_INT64, to_name="i")
        print '(a,i0)', "unexpectedly wrote over an existing column, ncols=", t%ncols()
    end subroutine scenario_parse_column_to_name_exists

    !> A conversion on a table another thread may be using. Both verbs replace a column's
    !> storage, which is exactly the class of change table_check_not_shared refuses.
    subroutine scenario_parse_column_shared()
        type(parquet_table) :: t

        call build_convert_fixture(t)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%parse_column("s", PK_INT64)
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly converted a column of a shared table, kind is now ", t%kind("s")
    end subroutine scenario_parse_column_shared

    !> %reload on a parsed column. The abort is %reload's own user_populated guard, and this is
    !> what makes the claim %parse_column stakes observable: the file holds text, so re-reading
    !> it into the parsed kind is not something the reader can do.
    subroutine scenario_reload_after_parse_column()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=*), parameter :: file = "test_run/convert_reload_scenario.parquet"

        call parquet_open_writer(w, file)
        call parquet_write_column(w, "s", [character(len=4) :: "1", "2", "3"])
        call parquet_close_writer(w)
        call parquet_open_table(t, file)
        call t%parse_column("s", PK_INT32)
        call t%reload("s")
        print '(a,i0)', "unexpectedly reloaded a parsed column, kind is now ", t%kind("s")
    end subroutine scenario_reload_after_parse_column

    !> %format_column naming a column it cannot render one string per row from. Two scenarios
    !> share this body: a column that is already text (nothing to render, and a `fmt` would be
    !> silently ignored) and a vector column (no per-row scalar at all).
    subroutine scenario_format_column_bad_source(bad)
        character(len=*), intent(in) :: bad !! the column to name.
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%format_column(bad)
        print '(a,i0)', "unexpectedly rendered an unsupported column, kind is now ", t%kind(bad)
    end subroutine scenario_format_column_bad_source

    !> `fmt=` on a temporal column. Refused rather than ignored: %to_string writes ISO-8601 and
    !> has no format to vary, so accepting the argument would silently discard it.
    subroutine scenario_format_column_fmt_on_temporal()
        type(parquet_table) :: t

        call build_convert_fixture(t)
        call t%format_column("d", fmt="(i0)")
        print '(a,i0)', "unexpectedly accepted fmt= on a temporal column, kind is now ", t%kind("d")
    end subroutine scenario_format_column_fmt_on_temporal

    !> An in-place `%parse_column` of a PREDEFINED column: refused, because a generated table
    !> type's accessor has the column's kind compiled in.
    subroutine scenario_parse_column_predefined()
        type(parquet_table_test) :: t

        call t%init_empty(2_int32)
        call t%parse_column("name", PK_INT64)
        print '(a,i0)', "unexpectedly converted a predefined column, kind is now ", t%kind("name")
    end subroutine scenario_parse_column_predefined

    !> The same for `%format_column`, with `spelled_out` choosing between an absent `force=` and
    !> an explicit `force=.false.`. The two must behave identically: a caller who writes the
    !> default out is not asking for anything, and a guard keyed on `present(force)` rather than
    !> on its value would pass the first arm and let the second through.
    subroutine scenario_format_column_predefined(spelled_out)
        logical, intent(in) :: spelled_out
        type(parquet_table_test) :: t

        call t%init_empty(2_int32)
        if (spelled_out) then
            call t%format_column("uberid", force=.false.)
        else
            call t%format_column("uberid")
        end if
        print '(a,i0)', "unexpectedly rendered a predefined column, kind is now ", t%kind("uberid")
    end subroutine scenario_format_column_predefined

    !> `%cast` reaches the same hazard by a third route, and carries the same guard. `spelled_out`
    !> chooses between an absent `force=` and an explicit `force=.false.`, for the reason given
    !> above: the two must behave identically, and a guard keyed on `present(force)` would not.
    subroutine scenario_cast_predefined(spelled_out)
        logical, intent(in) :: spelled_out
        type(parquet_table_test) :: t

        call t%init_empty(2_int32)
        if (spelled_out) then
            call t%cast("uberid", PK_FLOAT64, force=.false.)
        else
            call t%cast("uberid", PK_FLOAT64)
        end if
        print '(a,i0)', "unexpectedly cast a predefined column, kind is now ", t%kind("uberid")
    end subroutine scenario_cast_predefined

    !> The negative control for every conversion scenario above: the same fixture, taken through
    !> both verbs, both policies, both `to_name` forms and a `fmt`. Without it a guard that
    !> refused everything would pass all twelve.
    subroutine scenario_convert_control()
        type(parquet_table) :: t
        character(len=:), allocatable :: txt(:)

        call build_convert_fixture(t)
        call t%parse_column("s", PK_INT64)                     ! in place, default policy
        call t%parse_column("bad", PK_INT32, invalid="null")   ! the malformed rows become Null
        call t%format_column("s", to_name="s_txt")             ! a new column beside it
        call t%format_column("i", fmt="(i4.4)")                ! in place, with a format
        call t%format_column("d")                              ! ISO-8601, no fmt
        call t%get("s_txt", txt)
        print '(a,l1,a,a)', "every conversion path ran, bad has nulls: ", t%has_nulls("bad"), &
            ", first text: ", trim(txt(1))
    end subroutine scenario_convert_control

    !> A six-row table with one group of repeats and one vector column, for the row-set verbs.
    !> `%duplicated()` naming nothing must refuse `vec` rather than quietly leave it out.
    subroutine build_rowverbs_fixture(t)
        type(parquet_table), intent(out) :: t !! the table to build.
        integer(int32) :: vec(2, 4)

        vec = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, 7_int32, 8_int32], [2, 4])
        call parquet_new_table(t)
        call t%add_column("id", [1_int32, 2_int32, 1_int32, 3_int32])
        call t%add_column("x", [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call t%add_column("vec", vec)
    end subroutine build_rowverbs_fixture

    !> %explode handed a count list that is not one entry per row.
    subroutine scenario_explode_wrong_length()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        call t%explode([1_int64, 2_int64])
        print '(a,i0)', "unexpectedly exploded with a short count list, rows now ", t%nrows()
    end subroutine scenario_explode_wrong_length

    !> A negative count. Nothing sensible can be built from it and a silent max(0, c) would drop
    !> the row without saying so, which is why it is refused rather than clamped.
    subroutine scenario_explode_negative_count()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        call t%explode([1_int64, -2_int64, 1_int64, 1_int64])
        print '(a,i0)', "unexpectedly exploded with a negative count, rows now ", t%nrows()
    end subroutine scenario_explode_negative_count

    !> Counts whose sum passes huge(0_int64). The counts are huge(0_int64) itself, so the correct
    !> path overflows nothing at all: row 1 leaves the total at huge, and row 2 is refused because
    !> its count exceeds `huge - total`, which is zero.
    !>
    !> **This scenario cannot tell the check's PLACEMENT from a `total < 0` test after the
    !> addition, and no scenario can.** A single count is at most huge, so a running total can
    !> never jump from below 2**63 to past 2**64 in one step -- it must land in between first,
    !> where it reads as negative -- so the two forms answer identically under wrapping arithmetic.
    !> The reason the check sits BEFORE the addition is therefore not observable behaviour but
    !> undefined behaviour: signed overflow is UB, and an optimiser entitled to assume it does not
    !> happen may delete a branch that tests for it (CLAUDE.md's own compiler hazard). That belongs
    !> in the comment beside the check, which is where it is; this scenario covers the refusal.
    subroutine scenario_explode_row_count_overflow()
        type(parquet_table) :: t
        integer(int64) :: big

        call build_rowverbs_fixture(t)
        big = huge(0_int64)
        call t%explode([big, big, big, big])
        print '(a,i0)', "unexpectedly exploded past huge(int64), rows now ", t%nrows()
    end subroutine scenario_explode_row_count_overflow

    !> %explode on a table another thread could be reading: refused like every row-structural verb.
    subroutine scenario_explode_shared()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%explode([2_int64, 1_int64, 1_int64, 1_int64])
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly exploded a shared table, rows now ", t%nrows()
    end subroutine scenario_explode_shared

    !> An unknown `keep` token. Both verbs share the guard, and both scenarios run it, because the
    !> message names the caller and a message blaming the wrong verb is what this pins.
    subroutine scenario_bad_keep(which)
        character(len=*), intent(in) :: which !! "duplicated" or "drop_duplicates".
        type(parquet_table) :: t
        logical, allocatable :: mask(:)

        call build_rowverbs_fixture(t)
        if (which == "duplicated") then
            call t%duplicated(["id"], mask, keep="middle")
            print '(a,l1)', "unexpectedly accepted keep=middle, first entry ", mask(1)
        else
            call t%drop_duplicates(["id"], keep="middle")
            print '(a,i0)', "unexpectedly accepted keep=middle, rows now ", t%nrows()
        end if
    end subroutine scenario_bad_keep

    !> `%duplicated()` naming no column over a table with a resident VECTOR column. Refused rather
    !> than silently leaving that column out of the comparison -- see resident_key_names.
    subroutine scenario_duplicated_all_unorderable()
        type(parquet_table) :: t
        logical, allocatable :: mask(:)

        call build_rowverbs_fixture(t)
        call t%duplicated(mask)
        print '(a,l1)', "unexpectedly compared every resident column, first entry ", mask(1)
    end subroutine scenario_duplicated_all_unorderable

    !> `%drop_duplicates()` naming no column on a table nothing has read yet. Refused rather than
    !> answered: comparing rows on no columns would make every row equal to every other and leave
    !> ONE row, which is the opposite of what the caller asked for.
    subroutine scenario_duplicated_all_nothing_resident()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=*), parameter :: file = "test_run/rowverbs_nothing_resident.parquet"

        call parquet_open_writer(w, file)
        call parquet_write_column(w, "id", [1_int32, 2_int32, 1_int32])
        call parquet_close_writer(w)
        call parquet_open_table(t, file)
        call t%drop_duplicates()
        print '(a,i0)', "unexpectedly deduplicated on no columns at all, rows now ", t%nrows()
    end subroutine scenario_duplicated_all_nothing_resident

    !> A key column that does not exist. The refusal is the sort's own lookup, and the message
    !> must name `duplicated` rather than `sort_by`.
    subroutine scenario_duplicated_unknown_column()
        type(parquet_table) :: t
        logical, allocatable :: mask(:)

        call build_rowverbs_fixture(t)
        call t%duplicated(["nope"], mask)
        print '(a,l1)', "unexpectedly grouped by a column that does not exist, entry ", mask(1)
    end subroutine scenario_duplicated_unknown_column

    !> %sort_by_values handed a value list that is not one value per row.
    subroutine scenario_sort_by_values_wrong_length()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        call t%sort_by_values([3_int32, 1_int32])
        print '(a,i0)', "unexpectedly sorted by a short value list, rows now ", t%nrows()
    end subroutine scenario_sort_by_values_wrong_length

    !> The same guard on the non-mutating twin, which reaches it by its own name.
    subroutine scenario_argsort_by_values_wrong_length()
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)

        call build_rowverbs_fixture(t)
        call t%argsort_by_values([3.0_real64, 1.0_real64], perm)
        print '(a,i0)', "unexpectedly ordered by a short value list, entries ", size(perm)
    end subroutine scenario_argsort_by_values_wrong_length

    !> %drop_duplicates on a shared table.
    subroutine scenario_drop_duplicates_shared()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%drop_duplicates(["id"])
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly deduplicated a shared table, rows now ", t%nrows()
    end subroutine scenario_drop_duplicates_shared

    !> %sort_by_values on a shared table.
    subroutine scenario_sort_by_values_shared()
        type(parquet_table) :: t

        call build_rowverbs_fixture(t)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call t%sort_by_values([4_int32, 3_int32, 2_int32, 1_int32])
        !$omp end single
        !$omp end parallel
        print '(a,i0)', "unexpectedly reordered a shared table, rows now ", t%nrows()
    end subroutine scenario_sort_by_values_shared

    !> The negative control for every guard above: each permitted spelling on the same fixture,
    !> so a guard that fired unconditionally would be caught here rather than pass every scenario.
    !> A lookup table with more keys than values cannot answer, and the mismatch is refused
    !> BEFORE the sort rather than after O(n log n) of work the caller cannot use.
    subroutine scenario_remap_length_mismatch()
        integer(int32), allocatable :: got(:)

        call pf_remap([1_int32, 2_int32], [1_int32, 2_int32, 3_int32], [10_int32, 20_int32], got)
        print '(a,i0)', "unexpectedly remapped through a mismatched lookup table, got(1)=", got(1)
    end subroutine scenario_remap_length_mismatch

    !> A repeated key has no defined value, so picking one silently is the failure this family
    !> exists to remove. The message names both positions.
    subroutine scenario_remap_duplicate_key()
        integer(int32), allocatable :: got(:)

        call pf_remap([10_int32, 20_int32], [10_int32, 20_int32, 10_int32], &
            [1_int32, 2_int32, 3_int32], got)
        print '(a,i0)', "unexpectedly remapped through a repeated key, got(1)=", got(1)
    end subroutine scenario_remap_duplicate_key

    !> With neither `default=` nor `found=` the caller has no way to learn an element was
    !> unmapped, so handing them one silently is refused. The message names the position.
    subroutine scenario_remap_unmapped_no_policy()
        integer(int32), allocatable :: got(:)

        call pf_remap([10_int32, 99_int32], [10_int32, 20_int32], [1_int32, 2_int32], got)
        print '(a,i0)', "unexpectedly remapped an element with no key, got(2)=", got(2)
    end subroutine scenario_remap_unmapped_no_policy

    !> A result carrying two columns of one name is unusable, so counting a column that is
    !> already called `count` is refused rather than built.
    subroutine scenario_value_counts_count_name_collision()
        type(parquet_table) :: t, vc

        call parquet_new_table(t)
        call t%add_column("count", [1_int32, 2_int32, 1_int32])
        call t%value_counts("count", vc)
        print '(a,i0)', "unexpectedly counted a column called count, rows ", vc%nrows()
    end subroutine scenario_value_counts_count_name_collision

    !> The key is resolved by %value_counts itself, so the message names THIS verb rather than
    !> the %argsort_by underneath it.
    subroutine scenario_value_counts_unknown_column()
        type(parquet_table) :: t, vc

        call build_rowverbs_fixture(t)
        call t%value_counts("nosuch", vc)
        print '(a,i0)', "unexpectedly counted a column that does not exist, rows ", vc%nrows()
    end subroutine scenario_value_counts_unknown_column

    !> A vector column has no ordering, so it cannot be grouped and cannot be counted.
    subroutine scenario_value_counts_unorderable()
        type(parquet_table) :: t, vc

        call build_rowverbs_fixture(t)
        call t%value_counts("vec", vc)
        print '(a,i0)', "unexpectedly counted a vector column, rows ", vc%nrows()
    end subroutine scenario_value_counts_unorderable

    !> The negative control for the six above: every counting and mapping call in its well-formed
    !> shape, so a guard that fired unconditionally would fail HERE rather than pass everywhere.
    subroutine scenario_counting_control()
        type(parquet_table) :: t, vc
        integer(int32), allocatable :: dist(:), got(:)
        integer(int64), allocatable :: cnts(:)
        logical, allocatable :: fnd(:)

        call pf_value_counts([3_int32, 1_int32, 3_int32], dist, cnts)
        call pf_remap([1_int32, 9_int32], [1_int32, 2_int32], [10_int32, 20_int32], got, default=-1_int32)
        call pf_remap([1_int32, 9_int32], [1_int32, 2_int32], [10_int32, 20_int32], got, found=fnd)
        call build_rowverbs_fixture(t)
        call t%value_counts("id", vc)
        call t%value_counts("id", vc, dropna=.false., descending=.false., count_name="n")
        print '(a,i0,a,i0,a,i0)', "counting ran: distinct ", size(dist), ", remapped ", got(1), &
            ", value_counts rows ", vc%nrows()
    end subroutine scenario_counting_control

    subroutine scenario_rowverbs_control()
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        integer(int64), allocatable :: perm(:), origin(:)

        call build_rowverbs_fixture(t)
        call t%duplicated(["id"], mask, keep="first")           ! every keep token is accepted
        call t%duplicated(["id"], mask, keep="last")
        call t%duplicated("id", mask, keep="none")
        call t%argsort_by_values([4.0_real64, 3.0_real64, 2.0_real64, 1.0_real64], perm)
        call t%explode([1_int64, 2_int64, 1_int64, 1_int64], origin=origin)
        call t%sort_by_values([5_int32, 4_int32, 3_int32, 2_int32, 1_int32])
        call t%drop_duplicates(["id"], keep="last")
        print '(a,i0,a,i0)', "every row-set verb ran, rows now ", t%nrows(), ", origin entries ", &
            size(origin)
    end subroutine scenario_rowverbs_control

    subroutine scenario_filter_set_control()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_set_control.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", [1_int32, 2_int32, 3_int32, 4_int32])
        call parquet_close_writer(writer)

        call filt%bind("wanted", [1_int32, 2_int32, 3_int32], is_valid=[.true., .true., .false.])
        call filt%add("id in @wanted")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        print '(a,i0)', "a well-formed set clause opened cleanly, rows = ", nrows
    end subroutine scenario_filter_set_control

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
    !> starts_with/ends_with/contains match part of a STRING, so a numeric column is refused
    !> rather than answered. The message names the operator and the column's actual type, exactly
    !> as the is_nan family's does for a non-floating-point column.
    subroutine scenario_filter_starts_with_non_string_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_starts_with_non_string.parquet")
        call parquet_write_column(writer, "id", [1_int32, 2_int32])
        call parquet_close_writer(writer)

        call filt%add('id starts_with "1"')
        call parquet_open_reader(reader, "test_run/filter_starts_with_non_string.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with starts_with against an integer filter column"
    end subroutine scenario_filter_starts_with_non_string_column

    !> A matcher's pattern is a string literal and must be double-quoted, exactly as an equality
    !> comparison's value on the same column must be. An unquoted pattern is refused rather than
    !> read as a bare token.
    subroutine scenario_filter_starts_with_unquoted_value()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=4) :: s(2) = ["ab  ", "cd  "]

        call parquet_open_writer(writer, "test_run/filter_starts_with_unquoted.parquet")
        call parquet_write_column(writer, "s", s)
        call parquet_close_writer(writer)

        call filt%add("s starts_with ab")
        call parquet_open_reader(reader, "test_run/filter_starts_with_unquoted.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an unquoted starts_with pattern"
    end subroutine scenario_filter_starts_with_unquoted_value

    !> A matcher on a date/time/timestamp column is refused for NOT BEING A STRING COLUMN, and the
    !> message says so.
    !>
    !> This is the negative control for convert_temporal_filter_values' positive operator test. That
    !> routine rewrites a temporal column's quoted ISO-8601 literal into the raw integer the column
    !> stores; a new value-carrying operator that fell through it would have its pattern converted
    !> too -- reporting a malformed ISO-8601 literal for a pattern the caller never meant as an
    !> instant, or, for a pattern that happens to parse, silently converting it into an integer.
    !> Asking what the operator IS, rather than listing what to skip, is what keeps this message
    !> about starts_with instead of about ISO-8601.
    subroutine scenario_filter_starts_with_on_temporal_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: day(2)
        character(len=*), parameter :: out_file = "test_run/filter_starts_with_temporal.parquet"

        call day(1)%set(2024, 1, 1)
        call day(2)%set(2024, 1, 2)
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "day", day)
        call parquet_close_writer(writer)

        ! A pattern that WOULD parse as a valid ISO-8601 date if it reached the temporal
        ! conversion, so the scenario fails loudly if the skip is ever lost.
        call filt%add('day starts_with "2024-01-01"')
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly opened a reader with starts_with against a date filter column"
    end subroutine scenario_filter_starts_with_on_temporal_column

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
    !> matched nothing, so the mistake reported as data rather than as an error.
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

    !> parquet_reader_set_filter's pre-evaluated-leaf row-count guard (parquet_wrapper.cpp): the
    !> verdict array a set-valued (`in`/`not_in`) clause hands over is indexed by PHYSICAL row, and
    !> the row and row-group counts it was built from must be the file's own.
    !>
    !> The guard CANNOT fire through the public API -- parquet_prepare_set_leaves (parquet_read.f90)
    !> derives both counts from a freshly opened PRIVATE reader on the same file, which carries no
    !> transform of its own, so they are the physical ones by construction. It exists because the
    !> reader has three coordinate systems (physical, live after the statistics screen, surviving
    !> after the mask) and a verdict array sliced in the wrong one is misaligned by exactly a pruned
    !> row group's length while still producing a plausible row count -- a silent wrong answer, not
    !> a crash. The debug hook below is what makes it testable instead of defensive code no fixture
    !> can reach: it makes the guard expect one row MORE than the file has, so the correct payload
    !> Fortran built is rejected.
    !>
    !> The control arm matters as much as the abort: the same open runs cleanly with the hook clear
    !> first, which is what proves the guard is not simply firing on every set-valued clause.
    subroutine scenario_filter_pre_leaf_row_count_mismatch()
        interface
            subroutine parquet_debug_set_force_pre_leaf_mismatch(enable) &
                bind(C, name="parquet_debug_set_force_pre_leaf_mismatch")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero makes the guard expect one row too many; 0 restores.
            end subroutine parquet_debug_set_force_pre_leaf_mismatch
        end interface

        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: v(5) = [1, 2, 3, 4, 5]
        integer(int64) :: nrows
        character(len=*), parameter :: out_file = &
            "test_run/error_scenario_filter_pre_leaf_row_count_mismatch.parquet"

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        ! Negative control: with the hook clear the identical set-valued open must succeed, and must
        ! match the two rows it names -- an open that matched nothing would pass a bare status check.
        call filt%add_in("v", [2_int32, 4_int32])
        call parquet_open_reader(reader, out_file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        if (nrows /= 2_int64) then
            error stop "control: the set-valued filter must keep exactly two rows with the hook clear"
        end if
        print '(a)', "control: the set-valued open succeeded with the row-count hook clear"

        call parquet_debug_set_force_pre_leaf_mismatch(1)
        call parquet_open_reader(reader, out_file, filter=filt)
        print '(a)', "unexpectedly applied a set-valued filter despite the forced row-count mismatch"
    end subroutine scenario_filter_pre_leaf_row_count_mismatch

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

end module error_scenarios_io
