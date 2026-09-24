!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Error scenarios for the column and table layer: quality-control rules, column adders and
!> sinks, schema and metadata handling, string and temporal columns, write row masks, the
!> column containers, and the `parquet_table` type with its generated accessors.
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
module error_scenarios_table
    use parquet
    use parquet_maml_base, only: parquet_maml_file, get_parquet_maml
    use parquet_strings, only : parquet_string_column, parquet_string
    use parquet_columns
    use parquet_list, only : parquet_list_column, parquet_list_row
    use parquet_table_example, only : parquet_table_test
    use parquet_tables
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp, &
        parquet_unit_seconds, parquet_unit_millis, parquet_unit_nanos
    use iso_fortran_env, only : int32, int64, real32, real64
    !$ use omp_lib, only : omp_get_max_threads, omp_get_thread_num
    use error_scenarios_support, only : multitype_vector_schema, write_scenario_maml_file, write_text_file
    implicit none
    private

    public :: dispatch_error_scenarios_table

    !> A second concrete table type, existing only so the %clone same-type guard has something to
    !! be told apart from. Stands in for what tools/generate_parquet_table.sh will emit later: a
    !! table extended with predefined-column accessors, which a clone must not silently drop.
    type, extends(parquet_table) :: extended_table
        integer :: marker = 0 !! never read; the type's identity is the whole point.
    end type extended_table

contains

    !> Run `scenario` if it is one of this module's, and report whether it was.
    !!
    !! `handled` is `.false.` for a name this group does not own, which is how
    !! `error_scenarios.f90` walks the four groups in turn without any of them knowing
    !! what the others hold.
    subroutine dispatch_error_scenarios_table(scenario, handled)
        character(len=*), intent(in) :: scenario
        logical, intent(out) :: handled

        handled = .true.
        select case (trim(scenario))
        case ("string_view_compact_read")
            call scenario_string_view_compact_read()
        case ("plain_list_size_queries_avoid_whole_column_read")
            call scenario_plain_list_size_queries_avoid_whole_column_read()
        case ("write_column_twice_no_schema")
            call scenario_write_column_twice_no_schema()
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
        case ("open_reader_missing_file")
            call scenario_open_reader_missing_file()
        case ("open_reader_nrows_zero_rows")
            call scenario_open_reader_nrows_zero_rows()
        case ("open_writer_bad_path")
            call scenario_open_writer_bad_path()
        case ("open_writer_empty_schema")
            call scenario_open_writer_empty_schema()
        case ("write_string_matrix_exceeds_array_size")
            call scenario_write_string_matrix_exceeds_array_size()
        case ("write_string_exceeds_array_size")
            call scenario_write_string_exceeds_array_size()
        case ("validate_protected_cols_unknown_name")
            call scenario_validate_protected_cols_unknown_name()
        case ("validate_nullable_cols_unknown_name")
            call scenario_validate_nullable_cols_unknown_name()
        case ("validate_protected_and_nullable_overlap")
            call scenario_validate_protected_and_nullable_overlap()
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
        case ("set_nullable_unknown_column")
            call scenario_set_nullable_unknown_column()
        case ("set_nullable_on_protected_column")
            call scenario_set_nullable_on_protected_column()
        case ("set_protected_on_nullable_column")
            call scenario_set_protected_on_nullable_column()
        case ("set_protected_unprotect_warns")
            call scenario_set_protected_unprotect_warns()
        case ("set_nullable_undeclare_is_silent")
            call scenario_set_nullable_undeclare_is_silent()
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
        case ("qc_int64_bound_exact_via_real")
            call scenario_qc_int64_bound_exact_via_real()
        case ("qc_warning_float64")
            call scenario_qc_warning_float64()
        case ("qc_warning_float64_nan")
            call scenario_qc_warning_float64_nan()
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
        case ("set_random_parallel_min_elements_negative")
            call scenario_set_random_parallel_min_elements_negative()
        case ("write_table_schema_init_no_fields")
            call scenario_write_table_schema_init_no_fields()
        case ("derive_schema_needs_a_resident_column")
            call scenario_derive_schema_needs_a_resident_column()
        case ("open_writer_like_needs_a_resident_column")
            call scenario_open_writer_like_needs_a_resident_column()
        case ("write_table_chunk_zero_rows")
            call scenario_write_table_chunk_zero_rows()
        case ("write_table_chunk_row_group_already_open")
            call scenario_write_table_chunk_row_group_already_open()
        case ("write_table_chunk_schema_names_a_missing_column")
            call scenario_write_table_chunk_schema_names_a_missing_column()
        case ("write_table_chunk_protected_null")
            call scenario_write_table_chunk_protected_null()
        case ("sink_extra_column_refused")
            call scenario_sink_extra_column_refused()
        case ("sink_kind_mismatch_refused")
            call scenario_sink_kind_mismatch_refused()
        case ("sink_double_close")
            call scenario_sink_double_close()
        case ("sink_use_after_close")
            call scenario_sink_use_after_close()
        case ("sink_assignment_refused")
            call scenario_sink_assignment_refused()
        case ("sink_schema_names_a_missing_column")
            call scenario_sink_schema_names_a_missing_column()
        case ("sink_never_opened")
            call scenario_sink_never_opened()
        case ("sink_chunk_size_not_positive")
            call scenario_sink_chunk_size_not_positive()
        case ("sink_template_has_no_column")
            call scenario_sink_template_has_no_column()
        case ("sink_shared_in_parallel")
            call scenario_sink_shared_in_parallel()
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
        case ("string_column_set_validity_length_mismatch")
            call scenario_string_column_set_validity_length_mismatch()
        case ("string_column_set_where_length_mismatch")
            call scenario_string_column_set_where_length_mismatch()
        case ("string_column_append_nulls_negative")
            call scenario_string_column_append_nulls_negative()
        case ("columns_string_column_wrong_kind")
            call scenario_columns_string_column_wrong_kind()
        case ("columns_init_container_kind")
            call scenario_columns_init_container_kind()
        case ("list_init_unsupported_payload")
            call scenario_list_init_unsupported_payload()
        case ("list_append_before_init")
            call scenario_list_append_before_init()
        case ("list_append_wrong_kind")
            call scenario_list_append_wrong_kind()
        case ("list_append_mask_length")
            call scenario_list_append_mask_length()
        case ("list_view_out_of_range")
            call scenario_list_view_out_of_range()
        case ("list_unassociated_handle")
            call scenario_list_unassociated_handle()
        case ("list_get_wrong_kind")
            call scenario_list_get_wrong_kind()
        case ("list_gather_out_of_range")
            call scenario_list_gather_out_of_range()
        case ("list_adopt_not_allocated")
            call scenario_list_adopt_not_allocated()
        case ("list_column_paste_refused")
            call scenario_list_column_paste_refused()
        case ("container_append_wrong_container_kind")
            call scenario_container_append_wrong_container_kind()
        case ("container_append_wrong_element_kind")
            call scenario_container_append_wrong_element_kind()
        case ("container_append_struct_field_mismatch")
            call scenario_container_append_struct_field_mismatch()
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
        case ("arrow_type_unknown_column")
            call scenario_arrow_type_unknown_column()
        case ("table_print_stat_unsupported_column")
            call scenario_table_print_stat_unsupported_column()
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
        case ("get_version_invalid_mode_long")
            call scenario_get_version_invalid_mode_long()
        case ("get_arrow_version_invalid_mode_long")
            call scenario_get_arrow_version_invalid_mode_long()
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
        case ("table_write_row_index_with_schema")
            call scenario_table_write_row_index_with_schema()
        case ("table_write_row_index_blank")
            call scenario_table_write_row_index_blank()
        case ("table_write_row_index_collides")
            call scenario_table_write_row_index_collides()
        case ("table_write_row_index_in_memory")
            call scenario_table_write_row_index_in_memory()
        case ("table_write_row_index_detached")
            call scenario_table_write_row_index_detached()
        case ("table_write_row_index_reserved_warning")
            call scenario_table_write_row_index_reserved_warning()
        case ("table_copy_metadata_both_forms")
            call scenario_table_copy_metadata_both_forms()
        case ("table_copy_metadata_regenerated_key")
            call scenario_table_copy_metadata_regenerated_key()
        case ("table_copy_metadata_regenerated_control")
            call scenario_table_copy_metadata_regenerated_control()
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
        case ("column_gather_from_out_of_range")
            call scenario_column_gather_from_out_of_range()
        case ("column_gather_from_mask_length_mismatch")
            call scenario_column_gather_from_mask_length_mismatch()
        case ("column_gather_from_container_source")
            call scenario_column_gather_from_container_source()
        case ("string_column_gather_from_out_of_range")
            call scenario_string_column_gather_from_out_of_range()
        case ("string_column_gather_from_length_mismatch")
            call scenario_string_column_gather_from_length_mismatch()
        case ("table_append_self")
            call scenario_table_append_self()
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
        case ("container_sort_key")
            call scenario_container_sort_key()
        case ("container_bad_list_columns")
            call scenario_container_bad_list_columns()
        case ("container_print_stat_lengths")
            call scenario_container_print_stat_lengths()
        case ("container_skipped_by_mutation")
            call scenario_container_skipped_by_mutation()
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
        case ("table_print_stat_nan")
            call scenario_table_print_stat_nan()
        case ("table_print_stat_scan_serial")
            call scenario_table_print_stat_scan(threads=1)
        case ("table_print_stat_scan_parallel")
            call scenario_table_print_stat_scan(threads=0)
        case ("table_print_stat_no_stats")
            call scenario_table_print_stat_no_stats()
        case ("column_row_validity_range_out_of_range")
            call scenario_column_row_validity_range_out_of_range()
        case ("column_row_validity_range_short_mask")
            call scenario_column_row_validity_range_short_mask()
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
            handled = .false.
        end select
    end subroutine dispatch_error_scenarios_table

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
    !> tools/generate_parquet_maml_base.sh emits. Both MAML generators emit it
    !> from the same shared template, so this also pins the message a DOWNSTREAM
    !> project's generated parquet_maml produces (see
    !> doc/pages/utilities/embedding-maml-schemas.md); nothing in this repository
    !> compiles that one, which is what tools/check_downstream_maml_module.sh is
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

    !> A schema variable that was DECLARED and never built is the one shape `parquet_open_writer`
    !! cannot make sense of, and it must say so rather than proceed.
    !!
    !! The guard fires on a schema that is neither parsed nor carrying any MAML text -- i.e.
    !! nothing called `%init`/`%add_field`, nothing loaded a `.maml` file, and nothing populated
    !! `%maml%lines` directly. That is exactly a default-initialized `type(parquet_schema)`, which
    !! is what a caller has the moment they declare one and forget to fill it in, so the abort is
    !! a plain user mistake rather than an internal invariant.
    !!
    !! **Without the guard this does not fail loudly**: `parquet_parse_maml` would be handed no
    !! lines at all and the writer would go on to size its column arrays from an empty `%cinfo`,
    !! producing a file with no columns while every call returned successfully. The message names
    !! both ways out and the output file, per the writer-context convention.
    !!
    !! The control call comes first and uses the SAME writer variable with no schema at all, which
    !! is a supported call: it proves the abort below is the empty schema and not the path, the
    !! filename or the writer.
    subroutine scenario_open_writer_empty_schema()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/error_scenario_empty_schema_control.parquet")
        call parquet_close_writer(writer)
        print '(a)', "control: a schema-less writer opened and closed"
        call parquet_open_writer(writer, "test_run/error_scenario_empty_schema.parquet", schema)
        print '(a)', "unexpectedly opened a writer with a schema that was never built"
    end subroutine scenario_open_writer_empty_schema

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

    !> extra: nullable_cols: is checked against this MAML's own fields: exactly as protected_cols:
    !> is -- an unknown name is a typo or a dangling reference, and a declaration nobody can see
    !> take effect is worse than a refusal.
    subroutine scenario_validate_nullable_cols_unknown_name()
        type(parquet_maml_file) :: maml

        maml%name = "nullable_unknown.maml"
        maml%lines = [character(len=40) :: &
            "table: nullable_table", &
            "extra:", &
            "  nullable_cols: not_a_real_column", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_nullable_cols_unknown_name

    !> One column under BOTH extra: protected_cols: and extra: nullable_cols: is a contradiction:
    !> one says the field must reject every Null, the other that it must be written able to hold
    !> one. Refused rather than resolved by a precedence nobody wrote down.
    !>
    !> `b` is the control: it is listed under nullable_cols: only, so a build that refused the
    !> whole file whenever both keys appear would fail this scenario's own premise rather than
    !> catch the overlap.
    subroutine scenario_validate_protected_and_nullable_overlap()
        type(parquet_maml_file) :: maml

        maml%name = "nullable_overlap.maml"
        maml%lines = [character(len=40) :: &
            "table: overlap_table", &
            "extra:", &
            "  protected_cols: a", &
            "  nullable_cols: a;b", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_protected_and_nullable_overlap

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

    !> schema%set_nullable resolves its column through the same get_column_index as every other
    !> setter, so an unknown name aborts there rather than declaring nothing quietly.
    subroutine scenario_set_nullable_unknown_column()
        type(parquet_schema) :: schema

        schema%maml%name = "set_nullable_unknown.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: sn_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call schema%set_nullable("a")           ! control: a real column, must succeed
        call schema%set_nullable("no_such")     ! -> aborts
        print '(a)', "unexpectedly accepted set_nullable on an unknown column"
    end subroutine scenario_set_nullable_unknown_column

    !> The two declarations are opposites, so raising one while the other is up is refused in code
    !> exactly as a MAML naming one column in both lists is. This is the set_nullable side.
    subroutine scenario_set_nullable_on_protected_column()
        type(parquet_schema) :: schema

        schema%maml%name = "set_nullable_protected.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: sn_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call schema%set_protected("a")
        call schema%set_nullable("b")   ! control: an unprotected column takes the declaration
        call schema%set_nullable("a")   ! -> aborts
        print '(a)', "unexpectedly declared a protected column nullable"
    end subroutine scenario_set_nullable_on_protected_column

    !> ...and the set_protected side of the same rule, which is a separate guard in a separate
    !> procedure and would otherwise be untested.
    subroutine scenario_set_protected_on_nullable_column()
        type(parquet_schema) :: schema

        schema%maml%name = "set_protected_nullable.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: sp_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call schema%set_nullable("a")
        call schema%set_protected("b")   ! control: an undeclared column takes the protection
        call schema%set_protected("a")   ! -> aborts
        print '(a)', "unexpectedly protected a column declared nullable"
    end subroutine scenario_set_protected_on_nullable_column

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

    !> The OTHER half of `qc_bound_as_int64`: the exact conversion and the range guard, neither of
    !! which the sibling scenario above can reach.
    !!
    !! `qc_numeric_i64` takes its exact int64 bound from the declared TEXT first
    !! (`parquet_qc_bound_as_int64_text`), which accepts an optional sign and digits and nothing
    !! else -- so `10.0` is rejected by it even though the value is a whole number, and
    !! `qc_bound_as_int64` is what recovers the exact bound by rounding. The scenario above reaches
    !! that function only with a genuinely FRACTIONAL bound, where it returns at its very first
    !! test; everything past that test needs a bound whose text is not a plain integer but whose
    !! value is.
    !!
    !! **The four columns are the two answers the function can give, each with its control.**
    !!
    !! `mixed` declares a min PAST 2**53 written as a real (recovered exactly) beside a fractional
    !! max (not recoverable) -- the only shape in which one bound is exact and the other is not,
    !! which is the report arm that renders the minimum as an integer and the maximum as the real
    !! it is. The magnitude is what makes the two answers tell each other apart: the real
    !! formatter prints a bare integer only below 1e15, so an unrecovered bound of this size comes
    !! out as `0.9007199E+16` while a recovered one comes out in full.
    !!
    !! `past53`/`at53` prove the recovered bound is what the COMPARISON used, not merely what the
    !! message printed. Both declare `max: 9007199254740992.0`; `past53` holds 2**53 + 1, which
    !! violates in int64 and does NOT violate once widened to real64 (the two are the same real),
    !! so it must warn -- and `at53` holds 2**53 exactly, which violates under neither, so it must
    !! stay silent. A checker that warned about every column could not pass both.
    !!
    !! `too_big` declares a bound beyond int64's range, which the range guard must decline rather
    !! than convert: a conversion that ignored the range would wrap it and judge every value
    !! against a nonsense bound while still warning about something.
    !!
    !! Every column is declared `float64` and written from `integer(int64)` values, because that is
    !! the only way a non-integer bound and an int64 value meet at all -- an int32/int64 schema
    !! type rejects such a bound outright at parse time.
    subroutine scenario_qc_int64_bound_exact_via_real()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: mixed(2) = [5_int64, 200_int64]      !! below 2**53 and above 99.5: both violate.
        !> Two rows each, like every other column here -- a writer holds one row count for the
        !! whole file, so a one-row column would abort before qc ever ran.
        integer(int64) :: past53(2) = [9007199254740993_int64, 9007199254740993_int64] !! 2**53 + 1.
        integer(int64) :: at53(2) = [9007199254740992_int64, 9007199254740992_int64]   !! 2**53 exactly.
        integer(int64) :: too_big(2) = [5_int64, 200_int64]     !! both below 1.0e20: both violate its min.

        schema%maml%name = "qc_int64_bound_exact.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: mixed", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 9007199254740992.0", &
            "    max: 99.5", &
            "- name: past53", &
            "  data_type: float64", &
            "  qc:", &
            "    max: 9007199254740992.0", &
            "- name: at53", &
            "  data_type: float64", &
            "  qc:", &
            "    max: 9007199254740992.0", &
            "- name: too_big", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1.0e20" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_int64_bound_exact.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "mixed", mixed)
        call parquet_write_column(writer, "past53", past53)
        call parquet_write_column(writer, "at53", at53)
        call parquet_write_column(writer, "too_big", too_big)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_int64_bound_exact_via_real

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

    !> The same write-time float64 qc check, over data holding a NaN.
    !!
    !! **A NaN is a violation and must be REPORTED, and reporting it means ordering the data.**
    !! `qc_numeric_r64` accumulates the observed range with `min`/`max`, which compile to x86
    !! `minsd`/`maxsd` -- and those raise IEEE_INVALID for a quiet-NaN operand, so under nagfor's
    !! default `-ieee=stop` the writer died on the way to the warning instead of naming the column.
    !! Only an optimised build reaches it (`fpm test --profile release`); at `-O0` the clamp stays
    !! a pair of branches.
    !!
    !! Two columns, because the two arms differ: `f64` has a NaN beside real values, so the range
    !! is over the survivors, and `f64_all_nan` has nothing else, so the range is the NaN itself
    !! rather than the zero the accumulators would otherwise still hold.
    subroutine scenario_qc_warning_float64_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real64) :: nan64, f64(3), f64_all_nan(3)

        nan64 = ieee_value(1.0_real64, ieee_quiet_nan)
        ! The NaN is in the middle again: it is neither the element that seeds the range nor the
        ! last one, so a screen covering only the first element would still trap.
        f64 = [2.0_real64, nan64, 9.0_real64]
        f64_all_nan = [nan64, nan64, nan64]

        schema%maml%name = "qc_warning_float64_nan.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: f64", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1", &
            "    max: 10", &
            "- name: f64_all_nan", &
            "  data_type: float64", &
            "  qc:", &
            "    min: 1", &
            "    max: 10" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning_float64_nan.parquet", &
            schema, qc=.true.)
        call parquet_write_column(writer, "f64", f64)
        call parquet_write_column(writer, "f64_all_nan", f64_all_nan)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_float64_nan

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

    !> The mirror of the scenario above, on the opposite declaration: relaxing a NULLABLE
    !! declaration is silent. Undeclaring relaxes nothing a caller could have relied on -- the
    !! column simply goes back to the default, where the values decide -- so unlike unprotecting
    !! there is nothing to report. Documented in doc/pages/schema/building-schema-in-code.md
    !! ("undeclaring is silent") and in set_nullable's own doc-comment in parquet_core.f90.
    !!
    !! **A claimed ABSENCE needs a positive control in the same run, and "p" is it**: a build whose
    !! output capture is broken, or a scenario that aborted before reaching either call, would
    !! satisfy "nothing mentions column 'a'" trivially. So this scenario relaxes BOTH kinds of
    !! declaration and the wrapper reads one line present and the other absent from one capture.
    subroutine scenario_set_nullable_undeclare_is_silent()
        type(parquet_schema) :: schema

        call schema%init(table="nullable_table")
        call schema%add_field("a", "int32")
        call schema%add_field("p", "int32")

        call schema%set_nullable("a")            ! declare, then take it off again -> must be silent
        call schema%set_nullable("a", .false.)
        call schema%set_protected("p")           ! positive control: this relaxation DOES warn
        call schema%set_protected("p", .false.)
    end subroutine scenario_set_nullable_undeclare_is_silent

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

    !> The random bulk-draw work floor takes `0` and refuses a NEGATIVE value, and the two are a
    !! pair rather than one rule: `0` is the documented way to disable the floor entirely (how a
    !! test asks for a team on a small array), so a guard written as `n <= 0` would reject the one
    !! value the API promises to accept.
    !!
    !! Both accepted values are exercised as the control before the abort -- the factory default is
    !! restored afterwards so nothing later in this process inherits a disabled floor -- which is
    !! what separates "a negative value was rejected" from "this setter rejects everything".
    subroutine scenario_set_random_parallel_min_elements_negative()
        call parquet_set_random_parallel_min_elements(0)
        print '(a,i0)', "control: 0 accepted, floor now ", parquet_get_random_parallel_min_elements()
        call parquet_set_random_parallel_min_elements(1000)
        print '(a,i0)', "control: 1000 accepted, floor now ", parquet_get_random_parallel_min_elements()
        call parquet_set_random_parallel_min_elements(-1)
        print '(a)', "unexpectedly accepted a negative work floor"
    end subroutine scenario_set_random_parallel_min_elements_negative

    !> `parquet_write_table` parses a schema that was initialized but never parsed, rather than
    !! reading an unpopulated `%cinfo` -- which `%get_num_fields` would turn into a runaway
    !! allocation and an OOM kill instead of a diagnosable failure.
    !!
    !! **This is the only state that reaches that parse call**, and it follows from what the two
    !! readiness queries read (see the comment above the guard in src/parquet_tables_write.f90):
    !! `%add_field` parses as it goes, an embedded or loaded MAML arrives parsed, and a `%maml`
    !! assigned directly never had `%init` called so it takes the neighbouring "not built" abort
    !! instead. `%init` with no field added yet is what is left, and its MAML is header-only -- so
    !! the parse always fails validation, naming the real problem.
    !!
    !! The control is a schema built the ordinary way, written to a different file first: without
    !! it, an abort here would not distinguish an unparsed schema from a table that cannot be
    !! written at all.
    subroutine scenario_write_table_schema_init_no_fields()
        type(parquet_schema) :: good, bare
        type(parquet_table) :: t

        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call good%init(table="write_table_control")
        call good%add_field("a", "int32")
        call parquet_write_table(t, "test_run/error_scenario_write_table_control.parquet", good)
        print '(a)', "control: a table wrote through an ordinary schema"
        call bare%init(table="write_table_bare")
        call parquet_write_table(t, "test_run/error_scenario_write_table_bare.parquet", bare)
        print '(a)', "unexpectedly wrote a table through a schema declaring no fields"
    end subroutine scenario_write_table_schema_init_no_fields

    !> parquet_derive_schema builds one field per RESIDENT column, and MAML requires at least one
    !> field -- so a table that has read nothing has no schema to hand back. A schema-less
    !> parquet_write_table of the same table writes an empty file instead; this is the one place
    !> the two answer differently, because a schema with no field is not a valid schema.
    !>
    !> The control derives a schema from the same data with one column resident, and the abort is
    !> provoked on a lazily opened file-backed table that has touched no column.
    subroutine scenario_derive_schema_needs_a_resident_column()
        type(parquet_table) :: t, lazy
        type(parquet_schema) :: s
        character(len=*), parameter :: src = "test_run/error_scenario_derive_schema_needs_column.parquet"

        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call parquet_derive_schema(t, s)
        print '(a)', "control: a table with a resident column derived a schema"
        call parquet_write_table(t, src)
        call parquet_open_table(lazy, src)
        call parquet_derive_schema(lazy, s)
        print '(a)', "unexpectedly derived a schema from a table with no resident column"
    end subroutine scenario_derive_schema_needs_a_resident_column

    !> parquet_open_writer_like with no schema= derives one from the table's resident columns and
    !> refuses a table with none: an open writer with no column is nothing a caller can use, and
    !> the refusal names the alternative (materialize, or pass schema=). The control opens, writes
    !> and closes a writer from the same data with the column resident.
    subroutine scenario_open_writer_like_needs_a_resident_column()
        type(parquet_table) :: t, lazy
        type(parquet_writer) :: w
        integer(int32) :: a(2)
        character(len=*), parameter :: src = "test_run/error_scenario_open_writer_like_needs_column.parquet"
        character(len=*), parameter :: ctrl = "test_run/error_scenario_open_writer_like_control.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_open_writer_like_bad.parquet"

        a = [1_int32, 2_int32]
        call parquet_new_table(t)
        call t%add_column("a", a)
        call parquet_open_writer_like(w, ctrl, t)
        call parquet_write_column(w, "a", a)
        call parquet_close_writer(w)
        print '(a)', "control: a table with a resident column opened a writer"
        call parquet_write_table(t, src)
        call parquet_open_table(lazy, src)
        call parquet_open_writer_like(w, bad, lazy)
        print '(a)', "unexpectedly opened a writer from a table with no resident column"
    end subroutine scenario_open_writer_like_needs_a_resident_column

    !> parquet_write_table_chunk refuses a table with no rows: a Parquet row group holds at least
    !> one, and quietly writing nothing would hide a loop that produced no rows. The control
    !> writes a two-row table as the first row group; the empty table is the same table truncated
    !> to nothing.
    subroutine scenario_write_table_chunk_zero_rows()
        type(parquet_table) :: t, empty
        type(parquet_writer) :: w
        character(len=*), parameter :: out = "test_run/error_scenario_write_table_chunk_zero_rows.parquet"

        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call parquet_new_table(empty)
        call empty%add_column("a", [3_int32, 4_int32])
        call empty%truncate(0)
        call parquet_open_writer_like(w, out, t)
        call parquet_write_table_chunk(w, t)
        print '(a)', "control: a two-row table was written as a row group"
        call parquet_write_table_chunk(w, empty)
        print '(a)', "unexpectedly wrote a zero-row table as a row group"
    end subroutine scenario_write_table_chunk_zero_rows

    !> A row group already open is the WRITER's refusal, and parquet_write_table_chunk leaves it
    !> that way: the message names parquet_finish_row_group, the call that has to come first. The
    !> control is a complete chunk write on the same writer.
    subroutine scenario_write_table_chunk_row_group_already_open()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=*), parameter :: out = "test_run/error_scenario_write_table_chunk_rg_open.parquet"

        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call parquet_open_writer_like(w, out, t)
        call parquet_write_table_chunk(w, t)
        print '(a)', "control: a chunk was written while no row group was open"
        call parquet_new_row_group(w, 2)
        call parquet_write_table_chunk(w, t)
        print '(a)', "unexpectedly wrote a chunk into a writer with a row group already open"
    end subroutine scenario_write_table_chunk_row_group_already_open

    !> A schema field naming a column the table does not have is refused with the chunk write's
    !> own name in the message -- the same refusal parquet_write_table raises, through the shared
    !> locator. The control writes a table that has both columns through the same schema.
    subroutine scenario_write_table_chunk_schema_names_a_missing_column()
        type(parquet_table) :: full, part
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        character(len=*), parameter :: ctrl = "test_run/error_scenario_write_table_chunk_missing_control.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_write_table_chunk_missing_bad.parquet"

        call s%init("chunked")
        call s%add_field("a", "int32")
        call s%add_field("b", "int32")
        call parquet_new_table(full)
        call full%add_column("a", [1_int32, 2_int32])
        call full%add_column("b", [3_int32, 4_int32])
        call parquet_new_table(part)
        call part%add_column("a", [5_int32, 6_int32])
        call parquet_open_writer(w, ctrl, s)
        call parquet_write_table_chunk(w, full)
        call parquet_close_writer(w)
        print '(a)', "control: a table holding every declared column was written as a row group"
        call parquet_open_writer(w, bad, s)
        call parquet_write_table_chunk(w, part)
        print '(a)', "unexpectedly wrote a chunk through a schema naming a column the table lacks"
    end subroutine scenario_write_table_chunk_schema_names_a_missing_column

    !> A Null in a protected column aborts on the chunked path as on the whole-column one, and the
    !> message names parquet_write_column_chunk rather than parquet_write_column. The control is a
    !> Null-free chunk through the same protected schema, on the same writer.
    subroutine scenario_write_table_chunk_protected_null()
        type(parquet_table) :: clean, dirty
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        character(len=*), parameter :: out = "test_run/error_scenario_write_table_chunk_protected.parquet"

        call parquet_new_table(clean)
        call clean%add_column("x", [1.0_real64, 2.0_real64])
        call parquet_new_table(dirty)
        call dirty%add_column("x", [3.0_real64, 4.0_real64])
        call dirty%set_null("x", 1_int64)
        call parquet_derive_schema(clean, s)
        call s%set_protected("x")
        call parquet_open_writer_like(w, out, clean, schema=s)
        call parquet_write_table_chunk(w, clean)
        print '(a)', "control: a Null-free chunk of a protected column was written"
        call parquet_write_table_chunk(w, dirty)
        print '(a)', "unexpectedly wrote a Null into a protected column through a chunk write"
    end subroutine scenario_write_table_chunk_protected_null

    !> A two-column in-memory table for the sink scenarios: `a` int32, and `b` float64 unless
    !> `only_a`.
    subroutine sink_scenario_table(t, only_a)
        type(parquet_table), intent(out) :: t
        logical, intent(in), optional :: only_a
        logical :: with_b
        with_b = .true.
        if (present(only_a)) with_b = .not. only_a
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        if (with_b) call t%add_column("b", [1.5_real64, 2.5_real64])
    end subroutine sink_scenario_table

    !> With the columns fixed from the template, an appended table's resident column the output
    !> does not declare is refused with the sink's own message naming the file -- silently
    !> dropping data is the worse answer. The control is an append with exactly the template's
    !> columns.
    subroutine scenario_sink_extra_column_refused()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, same, wider
        character(len=*), parameter :: f = "test_run/error_scenario_sink_extra.parquet"

        call sink_scenario_table(seed, only_a=.true.)
        call sink_scenario_table(same, only_a=.true.)
        call sink_scenario_table(wider)
        call parquet_open_table_writer(out, f, seed)
        call out%append(same)
        print '(a)', "control: a table with the template's columns was appended"
        call out%append(wider)
        print '(a)', "unexpectedly appended a table with a column the output does not declare"
    end subroutine scenario_sink_extra_column_refused

    !> A kind mismatch is `%append`'s own refusal, with its own message: no silent widening.
    subroutine scenario_sink_kind_mismatch_refused()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, same, other
        character(len=*), parameter :: f = "test_run/error_scenario_sink_kind.parquet"

        call sink_scenario_table(seed, only_a=.true.)
        call sink_scenario_table(same, only_a=.true.)
        call parquet_new_table(other)
        call other%add_column("a", [1.0_real64, 2.0_real64])
        call parquet_open_table_writer(out, f, seed)
        call out%append(same)
        print '(a)', "control: a table of the template's kind was appended"
        call out%append(other)
        print '(a)', "unexpectedly appended a column of another kind"
    end subroutine scenario_sink_kind_mismatch_refused

    !> A second close is refused, not idempotent (api-conventions.md's mutation-guard rule).
    subroutine scenario_sink_double_close()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed
        character(len=*), parameter :: f = "test_run/error_scenario_sink_double_close.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(out, f, seed)
        call out%append(seed)
        call parquet_close_table_writer(out)
        print '(a)', "control: the sink was closed once"
        call parquet_close_table_writer(out)
        print '(a)', "unexpectedly closed a sink twice"
    end subroutine scenario_sink_double_close

    !> Every mutating entry point refuses a closed sink, naming the call and the file.
    subroutine scenario_sink_use_after_close()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed
        character(len=*), parameter :: f = "test_run/error_scenario_sink_after_close.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(out, f, seed)
        call out%append(seed)
        call parquet_close_table_writer(out)
        print '(a)', "control: the sink accepted rows and was closed"
        call out%append(seed)
        print '(a)', "unexpectedly appended to a closed sink"
    end subroutine scenario_sink_use_after_close

    !> The writer's handle is a plain pointer with no reference counting, so a copy would
    !> double-close the file: assignment aborts and names the way to have a second output.
    subroutine scenario_sink_assignment_refused()
        type(parquet_table_writer) :: out, copy
        type(parquet_table) :: seed
        character(len=*), parameter :: f = "test_run/error_scenario_sink_assign.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(out, f, seed)
        print '(a)', "control: the sink was opened"
        copy = out   ! -> aborts
        print '(a)', "unexpectedly assigned a parquet_table_writer"
    end subroutine scenario_sink_assignment_refused

    !> With `schema=`, every enabled field must name a template column, checked at open --
    !> field by field, before the writer is opened -- so the abort names the column and no
    !> output file exists (question 15). The control opens the same schema over a template that
    !> has both columns.
    subroutine scenario_sink_schema_names_a_missing_column()
        type(parquet_table_writer) :: out
        type(parquet_table) :: full, part
        type(parquet_schema) :: s
        character(len=*), parameter :: ctrl = "test_run/error_scenario_sink_schema_control.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_sink_schema_bad.parquet"

        call s%init("sink")
        call s%add_field("a", "int32")
        call s%add_field("b", "float64")
        call sink_scenario_table(full)
        call sink_scenario_table(part, only_a=.true.)
        call parquet_open_table_writer(out, ctrl, full, schema=s)
        call out%append(full)
        call parquet_close_table_writer(out)
        print '(a)', "control: the schema opened over a template that has every field"
        call parquet_open_table_writer(out, bad, part, schema=s)
        print '(a)', "unexpectedly opened a sink whose schema names a column the template lacks"
    end subroutine scenario_sink_schema_names_a_missing_column

    !> An append on a sink that was never opened names the call that has to come first.
    subroutine scenario_sink_never_opened()
        type(parquet_table_writer) :: out, fresh
        type(parquet_table) :: seed
        character(len=*), parameter :: f = "test_run/error_scenario_sink_never_opened.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(out, f, seed)
        call out%append(seed)
        call parquet_close_table_writer(out)
        print '(a)', "control: an opened sink accepted rows"
        call fresh%append(seed)
        print '(a)', "unexpectedly appended to a sink that was never opened"
    end subroutine scenario_sink_never_opened

    !> `chunk_size` is the flush threshold, so it must be positive; the control is 1.
    subroutine scenario_sink_chunk_size_not_positive()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed
        character(len=*), parameter :: ctrl = "test_run/error_scenario_sink_chunk_control.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_sink_chunk_bad.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(out, ctrl, seed, chunk_size=1)
        call out%append(seed)
        call parquet_close_table_writer(out)
        print '(a)', "control: chunk_size=1 opened, wrote and closed"
        call parquet_open_table_writer(out, bad, seed, chunk_size=0)
        print '(a)', "unexpectedly opened a sink with chunk_size=0"
    end subroutine scenario_sink_chunk_size_not_positive

    !> Without `schema=` the template's resident columns are the output's; a template that has
    !> read nothing has none, so there is nothing to write, and the refusal names the two ways
    !> out. The control is the same file with a column read.
    subroutine scenario_sink_template_has_no_column()
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, read_one, lazy
        character(len=*), parameter :: src = "test_run/error_scenario_sink_template_src.parquet"
        character(len=*), parameter :: ctrl = "test_run/error_scenario_sink_template_control.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_sink_template_bad.parquet"

        call sink_scenario_table(seed)
        call parquet_write_table(seed, src)
        call parquet_open_table(read_one, src)
        call read_one%prefetch("a")
        call parquet_open_table_writer(out, ctrl, read_one)
        call parquet_close_table_writer(out)
        print '(a)', "control: a template with one resident column opened a sink"
        call parquet_open_table(lazy, src)
        call parquet_open_table_writer(out, bad, lazy)
        print '(a)', "unexpectedly opened a sink from a template with no resident column"
    end subroutine scenario_sink_template_has_no_column

    !> A sink is single-thread-owned: one opened OUTSIDE a parallel region and appended to from
    !> inside it may be shared between threads, so the append is refused with the sink's own
    !> message. The negative control is in the same process and has to be: the guard keys on
    !> OWNERSHIP, not on `omp_in_parallel()`, so a sink this thread opened inside the region is
    !> appended to first and prints a marker the wrapper checks for.
    !> `mine` is declared HERE and not in the block below, although only the block uses it: a
    !> `parquet_table_writer` has an allocatable component, and ifx segfaults on such a type
    !> declared in a `block` lexically inside a parallel region (`fortran-gotchas.md`, ifx). The
    !> ownership guard is unaffected -- it keys on the thread and region recorded on the sink's
    !> buffer table when `parquet_open_table_writer` runs, which is still inside the region and
    !> still this thread, not on where the variable is declared. `rows` stays block-local because
    !> `parquet_table` has no allocatable component and is exempt.
    subroutine scenario_sink_shared_in_parallel()
        type(parquet_table_writer) :: shared
        type(parquet_table_writer) :: mine
        type(parquet_table) :: seed
        character(len=*), parameter :: out_shared = "test_run/es_sink_omp_shared.parquet"
        character(len=*), parameter :: out_mine = "test_run/es_sink_omp_mine.parquet"

        call sink_scenario_table(seed)
        call parquet_open_table_writer(shared, out_shared, seed)
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        block
            type(parquet_table) :: rows
            call sink_scenario_table(rows)
            call parquet_open_table_writer(mine, out_mine, rows)
            call mine%append(rows)
            call parquet_close_table_writer(mine)
            print '(a)', "private sink inside the region succeeded"
            call shared%append(rows)
        end block
        !$omp end single
        !$omp end parallel
        print '(a)', "unexpectedly appended to a shared sink from inside a parallel region"
    end subroutine scenario_sink_shared_in_parallel

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

    !> schema%print_schema_info with neither unit nor filename writes to the `message_stream`
    !> unit; it used to abort, because it was the one printer with no default destination.
    !!
    !! The schema here is built IN CODE (%init + %add_field) rather than parsed from MAML, which
    !! is a different path into the printer than scenario_print_schema_info_default_stream's --
    !! that one covers the parsed schema and asserts which stream the text lands on, this one
    !! covers the in-code schema and asserts the call completes at all. Exits 0.
    subroutine scenario_print_schema_info_no_unit_no_filename()
        type(parquet_schema) :: schema

        call schema%init(table="t")
        call schema%add_field("x", "int32")
        call schema%print_schema_info()
        print '(a)', "control: printed schema info with neither unit nor filename given"
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
    !> type, per `.claude/rules/columns-tables.md`'s "`parquet_temporal` and the containers" decision).
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
    !> parquet_get_column_arrow_type aborts on a name that does not exist. That is its ONLY
    !> failure mode: a type the library cannot read is an ANSWER for this query rather than an
    !> error, which is the whole reason it exists, so a missing name is the one thing left to
    !> refuse.
    !>
    !> The query on "cat_bytes" first is the NEGATIVE CONTROL, and is what makes the scenario mean
    !> anything. It is a dictionary over `binary` -- the column every other reader query in this
    !> library calls "unknown" -- so a build that aborted on any type it did not recognise, rather
    !> than only on a missing name, would fail here instead of passing.
    subroutine scenario_arrow_type_unknown_column()
        type(parquet_reader) :: reader
        character(len=:), allocatable :: at

        call parquet_open_reader(reader, "test/fixtures/dictionary_types.parquet")
        call parquet_get_column_arrow_type(reader, "cat_bytes", at)
        if (index(at, "dictionary<values=binary") /= 1) then
            error stop "the negative control failed: an unreadable column did not report its stored type"
        end if
        print '(a)', "control: cat_bytes is " // at
        call parquet_get_column_arrow_type(reader, "no_such_column", at)   ! -> aborts
        print '(a)', "unexpectedly reported a type for a column that does not exist: " // at
    end subroutine scenario_arrow_type_unknown_column
    !> %print_stat(all=.true.) names the STORED Arrow type of a column the table layer cannot
    !> read, rather than PK_NONE's kind name. Every unsupported column has the same PK_NONE kind,
    !> so without this the listing reports them all identically and a reader learns only that
    !> something is wrong, not what.
    !>
    !> Runs to completion (expected exit 0): this scenario exists because %print_stat writes to
    !> stdout via `print`, which an in-process test cannot capture -- the scenario harness can,
    !> so the assertion lives in test_errors.f90 against the captured output. The `cat` column
    !> beside it is the NEGATIVE CONTROL: it is a dictionary too, but over strings, so the table
    !> DOES support it and it must still report a kind name (`PK_STRING`'s) rather than its
    !> dictionary type.
    subroutine scenario_table_print_stat_unsupported_column()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/dictionary_types.parquet")
        call t%print_stat(all=.true.)
        if (t%is_supported("cat_bytes")) error stop "the fixture no longer has an unsupported column"
        if (.not. t%is_supported("cat")) error stop "the negative control failed: cat should be supported"
    end subroutine scenario_table_print_stat_unsupported_column

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
        ! it for free). parquet_get_column_total_elements asks the OTHER question of the same
        ! ragged column and cannot share that answer: the rows hold 0, 2 and 3 elements, so the
        ! width is 1 by contract while the element count is 5. It sums the rows' own lengths from
        ! the offsets, one row group at a time (list_payload_elements) -- the footer cannot supply
        ! it, because a null or empty list occupies a leaf slot and num_values sums to 6 here.
        !
        ! The 5-versus-3 distinction is the whole point of this assertion: reverting to the old
        ! nrows * list_width_verified spelling answers 3 -- the ROW count -- for exactly the
        ! columns whose element count a caller would ask about.
        call parquet_get_column_total_elements(reader, "lst", total_elem)
        call parquet_get_column_total_elements(reader, "large_lst", large_total_elem)
        call parquet_close_reader(reader)
        if (col_size /= 1) error stop "list fixture: mismatched-width LIST column should report col_size=1"
        if (large_col_size /= 1) error stop "list fixture: mismatched-width LARGE_LIST column should report col_size=1"
        if (total_elem /= 5) error stop "list fixture: mismatched-width LIST column should report 5 total elements"
        if (large_total_elem /= 5) &
            error stop "list fixture: mismatched-width LARGE_LIST column should report 5 total elements"

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

    !> An invalid `mode` is quoted back CAPPED, never in full -- the long half of the pair above.
    !!
    !! `mode` is caller-supplied and unbounded, and ifx 2026.1.1's ERROR STOP runtime corrupts the
    !! heap once the composed message reaches 8192 bytes, so a guard that reported the offending
    !! value verbatim would turn a clean abort into a crash on exactly the input that triggers it.
    !! The cap is 100 characters plus an ellipsis.
    !!
    !! The tail marker is what makes this checkable: the wrapper asserts the first 100 characters
    !! and the ellipsis are present AND that the marker beyond them is not, which a cap that
    !! silently did nothing would fail.
    subroutine scenario_get_version_invalid_mode_long()
        character(len=:), allocatable :: ver_string

        call parquet_get_version(ver_string, mode=repeat("x", 100) // "TAIL_MUST_NOT_APPEAR")
        print '(a)', "unexpectedly returned a version string for a very long invalid mode"
    end subroutine scenario_get_version_invalid_mode_long

    !> The same cap on parquet_get_arrow_version's own invalid-mode message; see
    !! scenario_get_version_invalid_mode_long for why it exists and what the tail marker is for.
    !! The two guards are separate code in separate modules (parquet_version and
    !! parquet_settings), so one being capped says nothing about the other.
    subroutine scenario_get_arrow_version_invalid_mode_long()
        character(len=:), allocatable :: ver_string

        call parquet_get_arrow_version(ver_string, mode=repeat("y", 100) // "TAIL_MUST_NOT_APPEAR")
        print '(a)', "unexpectedly returned a version string for a very long invalid mode"
    end subroutine scenario_get_arrow_version_invalid_mode_long

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
    !! Enforced statically by `check_maml_keys_case_insensitive`.
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

    !> A list column's payload must be one of the nine SCALAR value kinds. A *_VEC or container
    !! payload is nesting, which %init has no way to be told the shape of -- so it is refused
    !! rather than silently accepted and misread later.
    subroutine scenario_list_init_unsupported_payload()
        type(parquet_list_column) :: lc
        call lc%init(PK_INT32_VEC)   ! a vector payload is nesting -> aborts
        print '(a,i0)', "unexpectedly initialized a list column with payload kind ", lc%element_kind()
    end subroutine scenario_list_init_unsupported_payload

    !> Every mutating operation needs the payload kind %init fixes; without it there is nothing to
    !! append the values INTO, and the guard is what turns that into a message rather than a
    !! failure inside the payload column.
    subroutine scenario_list_append_before_init()
        type(parquet_list_column) :: lc
        call lc%append_row([1_int32, 2_int32])   ! no %init -> aborts
        print '(a,i0)', "unexpectedly appended to an uninitialized list column, rows=", lc%size()
    end subroutine scenario_list_append_before_init

    !> The payload kind is fixed at %init and cannot change: appending int64 values to an int32
    !! list column is a caller mistake, and accepting it would need a second payload column.
    subroutine scenario_list_append_wrong_kind()
        type(parquet_list_column) :: lc
        call lc%init(PK_INT32)
        call lc%append_row([1_int64, 2_int64])   ! wrong element type -> aborts
        print '(a,i0)', "unexpectedly appended int64 values to an int32 list column, rows=", lc%size()
    end subroutine scenario_list_append_wrong_kind

    !> is_valid is per ELEMENT, so a mask of a different length cannot be applied to the row.
    !! Silently padding or clipping it would mark the wrong elements null.
    subroutine scenario_list_append_mask_length()
        type(parquet_list_column) :: lc
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32, 3_int32], is_valid=[.true., .false.])   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a short validity mask, rows=", lc%size()
    end subroutine scenario_list_append_mask_length

    !> A row index out of range is a programming error, exactly as it is for
    !! parquet_string_column%view -- the sibling type this handle is modelled on.
    subroutine scenario_list_view_out_of_range()
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        row = lc%view(2_int64)   ! only one row exists -> aborts
        print '(a,i0)', "unexpectedly viewed a row that does not exist, index=", row%row_index()
    end subroutine scenario_list_view_out_of_range

    !> A default-constructed handle refers to no column. %is_valid() reports that without
    !! aborting; every accessor aborts rather than answering plausibly about nothing.
    subroutine scenario_list_unassociated_handle()
        type(parquet_list_row) :: row
        integer(int64) :: n
        if (row%is_valid()) print '(a)', "an unassigned handle unexpectedly reported itself valid"
        n = row%length()   ! no column -> aborts
        print '(a,i0)', "unexpectedly read a length from an unassigned handle: ", n
    end subroutine scenario_list_unassociated_handle

    !> Reading a float64 list column into an int32 array would silently reinterpret the values,
    !! so the payload kind is checked before anything is copied.
    subroutine scenario_list_get_wrong_kind()
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        call lc%init(PK_FLOAT64)
        call lc%append_row([1.5_real64])
        row = lc%view(1_int64)
        call row%get(v)   ! int32 array, float64 payload -> aborts
        print '(a,i0)', "unexpectedly read a float64 list column into int32 values, n=", size(v)
    end subroutine scenario_list_get_wrong_kind

    !> gather_rows names a source row per destination row; an index outside the column would
    !! read past the offsets and build a plausible column out of nothing.
    subroutine scenario_list_gather_out_of_range()
        type(parquet_list_column) :: lc
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%gather_rows([1_int64, 3_int64])   ! row 3 does not exist -> aborts
        print '(a,i0)', "unexpectedly gathered a row that does not exist, rows=", lc%size()
    end subroutine scenario_list_gather_out_of_range

    !> %adopt_container takes over an allocatable object via move_alloc, so an unallocated one has
    !! nothing to take over -- the same guard every one of the 16 array adopt_* specifics carries.
    subroutine scenario_list_adopt_not_allocated()
        type(parquet_column) :: col
        class(parquet_container_column), allocatable :: cc
        call col%adopt_container(cc)   ! never allocated -> aborts
        print '(a,i0)', "unexpectedly adopted an unallocated container, kind=", col%kindof()
    end subroutine scenario_list_adopt_not_allocated

    !> %paste overwrites a fixed row range in place, which a container column has no way to do:
    !! row i's element count is data, so replacing it moves every following row's payload.
    subroutine scenario_list_column_paste_refused()
        type(parquet_column) :: col, src
        type(parquet_list_column) :: lc
        class(parquet_container_column), allocatable :: cc
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%clone_into(cc)
        call col%adopt_container(cc)
        call lc%clone_into(cc)
        call src%adopt_container(cc)
        call col%paste(src, 1_int64)   ! a container cannot be overwritten in place -> aborts
        print '(a,i0)', "unexpectedly pasted into a container column, rows=", col%length()
    end subroutine scenario_list_column_paste_refused

    !> `append_from` checks the DYNAMIC TYPE of what it is being handed. A map's offsets index
    !! key/value entries while a list's index payload elements, so reading one as the other would
    !! produce a column that passes %validate and holds the wrong rows -- which is why this is an
    !! abort and not a best-effort conversion.
    !!
    !! Reached through parquet_column%append rather than the binding directly, because that is the
    !! route the table layer takes and the kind check there fires first for two columns of
    !! different PK_* kinds; a list column and a map column both reach append_from only when the
    !! caller has built them by hand, as here.
    subroutine scenario_container_append_wrong_container_kind()
        type(parquet_list_column) :: lc
        type(parquet_map_column) :: mc
        class(parquet_container_column), allocatable :: cc
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%clone_into(cc)
        call lc%append_from(cc)   ! a map onto a list -> aborts
        print '(a,i0)', "unexpectedly appended a map onto a list, rows=", lc%size()
    end subroutine scenario_container_append_wrong_container_kind

    !> Two list columns can agree on everything except their payload's kind, and appending across
    !! that difference would hand parquet_column%append two columns of different kinds one level
    !! down -- so the check belongs here, where the message can name both element types.
    subroutine scenario_container_append_wrong_element_kind()
        type(parquet_list_column) :: dst, src
        class(parquet_container_column), allocatable :: cc
        call dst%init(PK_INT32)
        call dst%append_row([1_int32])
        call src%init(PK_INT64)
        call src%append_row([1_int64])
        call src%clone_into(cc)
        call dst%append_from(cc)   ! list<int64> onto list<int32> -> aborts
        print '(a,i0)', "unexpectedly appended across element kinds, rows=", dst%size()
    end subroutine scenario_container_append_wrong_element_kind

    !> The sharp struct case: two columns with the SAME field count and the same field kinds, in a
    !! different ORDER. Appending them would transpose the two field columns silently, so the
    !! check is on field name at each position rather than on the set of names.
    subroutine scenario_container_append_struct_field_mismatch()
        type(parquet_struct_column) :: dst, src
        class(parquet_container_column), allocatable :: cc
        call dst%init(["a", "b"], [PK_INT32, PK_INT32])
        call dst%append_row()
        call src%init(["b", "a"], [PK_INT32, PK_INT32])
        call src%append_row()
        call src%clone_into(cc)
        call dst%append_from(cc)   ! struct<b,a> onto struct<a,b> -> aborts
        print '(a,i0)', "unexpectedly appended a transposed struct, rows=", dst%size()
    end subroutine scenario_container_append_struct_field_mismatch

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

    !> set_validity needs exactly one mask entry per element. The matching mask first is the
    !> control: it must be accepted, or the abort below would be asserting nothing about length.
    subroutine scenario_string_column_set_validity_length_mismatch()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%set_validity([.false., .true.])
        print '(a,i0)', "control: a matching set_validity mask was accepted, nulls=", col%null_count()
        call col%set_validity([.false.])   ! 1 entry for 2 elements -> aborts
        print '(a,i0)', "unexpectedly accepted a short set_validity mask, size=", col%size()
    end subroutine scenario_string_column_set_validity_length_mismatch

    !> set_where needs exactly one mask entry per element; same control as above.
    subroutine scenario_string_column_set_where_length_mismatch()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%set_where([.true., .false.], "xyz")
        print '(a,i0)', "control: a matching set_where mask was accepted, chars=", col%character_size()
        call col%set_where([.true.], "xyz")   ! 1 entry for 2 elements -> aborts
        print '(a,i0)', "unexpectedly accepted a short set_where mask, size=", col%size()
    end subroutine scenario_string_column_set_where_length_mismatch

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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%get("m_intkey", v)   ! a map with INT32 KEYS: v1 reads string keys only -> aborts
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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%prefetch("m_intkey")   ! a map with INT32 KEYS: v1 reads string keys only -> aborts
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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call s%init("wunsupported")
        call s%add_field("m_intkey", "int32")
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

    !> A schema states which columns the output has and what they are called, so row_index_name=
    !! adding one to it says two things at once and is refused rather than obeyed.
    subroutine scenario_table_write_row_index_with_schema()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call write_table_scenario_fixture("test_run/es_rowidx_schema_in.parquet")
        call parquet_open_table(t, "test_run/es_rowidx_schema_in.parquet")
        call t%materialize_all()
        call out_s%init("dest")
        call out_s%add_field("id", "int32")
        call parquet_write_table(t, "test_run/es_rowidx_schema_out.parquet", out_s, &
            row_index_name="src_row")   ! -> aborts
        print '(a)', "unexpectedly accepted row_index_name= alongside a schema"
    end subroutine scenario_table_write_row_index_with_schema

    !> row_index_name= is the name the row numbers are written under, so a blank one names nothing.
    subroutine scenario_table_write_row_index_blank()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_rowidx_blank_in.parquet")
        call parquet_open_table(t, "test_run/es_rowidx_blank_in.parquet")
        call t%materialize_all()
        call parquet_write_table(t, "test_run/es_rowidx_blank_out.parquet", &
            row_index_name="   ")   ! -> aborts
        print '(a)', "unexpectedly accepted a blank row_index_name="
    end subroutine scenario_table_write_row_index_blank

    !> Two fields of one name is not a schema; refused here, where both names are in hand, rather
    !! than left to %add_field's duplicate refusal naming a procedure the caller never invoked.
    subroutine scenario_table_write_row_index_collides()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_rowidx_collide_in.parquet")
        call parquet_open_table(t, "test_run/es_rowidx_collide_in.parquet")
        call t%materialize_all()
        call parquet_write_table(t, "test_run/es_rowidx_collide_out.parquet", &
            row_index_name="id")   ! "id" is already being written -> aborts
        print '(a)', "unexpectedly accepted a row_index_name= that names a written column"
    end subroutine scenario_table_write_row_index_collides

    !> A table built in memory was never read from a file, so there is no file row to record and
    !! no ordering of the program's calls that would have produced one.
    subroutine scenario_table_write_row_index_in_memory()
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("v", [1.0_real64, 2.0_real64, 3.0_real64])
        call parquet_write_table(t, "test_run/es_rowidx_mem_out.parquet", &
            row_index_name="src_row")   ! -> aborts
        print '(a)', "unexpectedly wrote file row numbers for a table with no source file"
    end subroutine scenario_table_write_row_index_in_memory

    !> A detached table HAD a file and lost it, so the row numbers are no longer derivable; the
    !! message says to ask before the mutation rather than after (feature_risks.md Risk-23).
    subroutine scenario_table_write_row_index_detached()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_rowidx_detached_in.parquet")
        call parquet_open_table(t, "test_run/es_rowidx_detached_in.parquet")
        call t%materialize_all()
        call t%truncate(2)
        call parquet_write_table(t, "test_run/es_rowidx_detached_out.parquet", &
            row_index_name="src_row")   ! -> aborts
        print '(a)', "unexpectedly wrote file row numbers for a detached table"
    end subroutine scenario_table_write_row_index_detached

    !> The reserved name is a WARNING, not a refusal: the file is valid and other readers see the
    !! column. This exits 0 -- the negative control for the four refusals above.
    subroutine scenario_table_write_row_index_reserved_warning()
        type(parquet_table) :: t
        call write_table_scenario_fixture("test_run/es_rowidx_reserved_in.parquet")
        call parquet_open_table(t, "test_run/es_rowidx_reserved_in.parquet")
        call t%materialize_all()
        call parquet_write_table(t, "test_run/es_rowidx_reserved_out.parquet", &
            row_index_name=PARQUET_ROW_INDEX)   ! warns, then writes
        print '(a)', "wrote the row numbers under the reserved name"
    end subroutine scenario_table_write_row_index_reserved_warning

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

    !> A key the writer generates itself from the output schema cannot be copied from the source.
    !!
    !! The source file HAS this key -- every written file carries a DATE -- so the "does the source
    !! have it?" check above passes and the refusal is this one. Skipping it silently instead would
    !! make naming a key a no-op, against parquet_write_table's own "naming a key is a claim" rule;
    !! carrying it would put two entries of that name in one file. See writer_regenerates_key in
    !! src/parquet_tables_write.f90 for which half of that pair a reader would then get.
    subroutine scenario_table_copy_metadata_regenerated_key()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call write_metadata_scenario_fixture("test_run/es_meta_regen_in.parquet")
        call parquet_open_table(t, "test_run/es_meta_regen_in.parquet")
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_write_table(t, "test_run/es_meta_regen_out.parquet", out_s, &
            metadata_keys=["DATE"])   ! -> aborts
        print '(a)', "unexpectedly carried a metadata key the writer generates itself"
    end subroutine scenario_table_copy_metadata_regenerated_key

    !> Negative control for the scenario above: an ordinary key of the same fixture, named the same
    !! way, must still be carried. Without this a refusal that fired unconditionally would pass the
    !! abort test while making metadata_keys= useless.
    subroutine scenario_table_copy_metadata_regenerated_control()
        type(parquet_table) :: t
        type(parquet_schema) :: out_s
        call write_metadata_scenario_fixture("test_run/es_meta_regenctl_in.parquet")
        call parquet_open_table(t, "test_run/es_meta_regenctl_in.parquet")
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_write_table(t, "test_run/es_meta_regenctl_out.parquet", out_s, &
            metadata_keys=["origin"])   ! -> must NOT abort
    end subroutine scenario_table_copy_metadata_regenerated_control

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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        r = t%row(1)
        call r%get("m_intkey", v)   ! a map with INT32 KEYS: v1 reads string keys only -> aborts
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

    !> %print_stat over float columns holding a NaN: it must print, not die, and the NaN must not
    !! enter the min/max.
    !!
    !! **Out of process because it prints, and an in-process assertion could not see the defect
    !! anyway.** `min`/`max` over a NaN compile to x86 `minsd`/`maxsd`, which raise IEEE_INVALID
    !! for a quiet-NaN operand -- so under nagfor's default `-ieee=stop` this scenario aborted with
    !! "Arithmetic exception: Floating invalid operation" before printing a single row, and only in
    !! an optimised build (`fpm test --profile release`). Every other compiler in the fleet masks
    !! the traps and answers a processor-dependent value instead, which is the second half of the
    !! bug: the wrong answer is what the assertions below pin.
    !!
    !! All four float specifics are exercised -- scalar and vector, float32 and float64 -- because
    !! each is generated separately by tools/generate_parquet_tables.py and a fix applied to one
    !! template arm and not the other would leave three of them trapping.
    subroutine scenario_table_print_stat_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_table) :: t
        real(real64) :: nan64, f64(3), f64v(2, 3), allnan(3)
        real(real32) :: nan32, f32(3), f32v(2, 3)

        nan64 = ieee_value(1.0_real64, ieee_quiet_nan)
        nan32 = ieee_value(1.0_real32, ieee_quiet_nan)
        ! The NaN sits in the MIDDLE, so it is neither the value that seeds the range nor the last
        ! one seen: a screen that only skipped the first element would still trap here.
        f64 = [1.0_real64, nan64, 3.0_real64]
        f32 = [11.0_real32, nan32, 13.0_real32]
        f64v(:, 1) = [21.0_real64, 22.0_real64]
        f64v(:, 2) = [nan64, 24.0_real64]
        f64v(:, 3) = [25.0_real64, 26.0_real64]
        f32v(:, 1) = [31.0_real32, 32.0_real32]
        f32v(:, 2) = [nan32, 34.0_real32]
        f32v(:, 3) = [35.0_real32, 36.0_real32]
        allnan = [nan64, nan64, nan64]

        call parquet_new_table(t)
        call t%add_column("d", f64)
        call t%add_column("s", f32)
        call t%add_column("dv", f64v)
        call t%add_column("sv", f32v)
        call t%add_column("allnan", allnan)
        call t%print_stat()
    end subroutine scenario_table_print_stat_nan

    !> The block-wise statistics scan behind %print_stat, on a fixture longer than two of its blocks.
    !!
    !! `STAT_BLOCK` (src/parquet_tables_access.f90) is 4096 rows; 10007 rows here make three blocks,
    !! the last one partial, and every null and NaN sits on one side or the other of a block
    !! boundary (rows 4096/4097 and 8192/8193), in the first row, or in the last -- the places a
    !! mask carried over from the previous block, or a range bound off by one, would move a count or
    !! an extreme without anything aborting (feature_risks.md Risk-223). One column per kind family,
    !! each with a null count and two extremes that no other column's line can be mistaken for.
    !!
    !! `threads` goes to parquet_set_table_threads: 1 forces the scan serial, 0 lets it open a team,
    !! which it does because the widest column (`i32v`, 16 x 10007 elements) is above the work floor.
    !! The trailing "parallel scan:" line says which happened, read back through the table-threads
    !! observation hook, so the wrapper can tell the two runs apart rather than asserting the same
    !! path twice.
    subroutine scenario_table_print_stat_scan(threads)
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: threads !! parquet_set_table_threads' cap: 1 serial, 0 automatic.
        integer, parameter :: nr = 10007
        type(parquet_table) :: t
        integer(int32) :: i32(nr), i32v(16, nr)
        integer(int64) :: i64(nr)
        real(real64) :: f64(nr), f64v(2, nr), nan64
        real(real32) :: f32(nr), nan32
        logical :: bool(nr), boolv(2, nr)
        character(len=6) :: str(nr)
        character(len=7) :: strv(2, nr)
        type(parquet_date) :: date(nr), datev(2, nr), d0, null_date
        integer :: i, e
        interface
            !> Threads the last table-layer team resolved to; 1 when it ran serially.
            function parquet_debug_get_table_threads_used() result(res) &
                bind(C, name="parquet_debug_get_table_threads_used")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function parquet_debug_get_table_threads_used
        end interface

        nan64 = ieee_value(1.0_real64, ieee_quiet_nan)
        nan32 = ieee_value(1.0_real32, ieee_quiet_nan)
        d0 = parquet_date(2020, 1, 1)
        do i = 1, nr
            i32(i) = 1000000 + i
            i64(i) = 2000000000000_int64 + int(i, int64)
            f64(i) = 0.5_real64*real(i, real64)
            f32(i) = nan32
            bool(i) = mod(i, 3) == 0
            write(str(i), '(a1, i5.5)') "m", i
            do e = 1, 16
                i32v(e, i) = 100*i + e
            end do
            do e = 1, 2
                f64v(e, i) = 0.25_real64*real(i, real64) + real(e, real64)
                boolv(e, i) = (e == 1) .and. mod(i, 2) == 0
                write(strv(e, i), '(a1, i5.5, a1)') "v", i, achar(iachar("a") + e - 1)
            end do
            date(i) = d0 + (i - 1)
            datev(1, i) = d0 + (i - 1)
            datev(2, i) = d0 + (i - 1 + 20000)
        end do
        ! NaNs are values, never nulls, and go on both sides of the second boundary and in the
        ! first block; the string extremes sit just past a boundary each; a temporal null is a
        ! default-initialised element and rides in with the values.
        f64(3) = nan64
        f64(4096) = nan64
        f64(8193) = nan64
        f64v(1, 8192) = nan64
        str(4097) = "zzz"
        str(8193) = "aaa"
        date(4096) = null_date
        date(8193) = null_date
        datev(2, 5) = null_date

        call parquet_new_table(t)
        call t%add_column("i32", i32)
        call t%add_column("i64", i64)
        call t%add_column("f64", f64)
        call t%add_column("f32", f32)
        call t%add_column("bool", bool)
        call t%add_column("str", str)
        call t%add_column("i32v", i32v)
        call t%add_column("f64v", f64v)
        call t%add_column("boolv", boolv)
        call t%add_column("date", date)
        call t%add_column("datev", datev)
        call t%add_column("strv", strv)
        ! Nulls: the first row, both sides of both boundaries, the last row, and a few inside.
        call t%set_null("i32", 1)
        call t%set_null("i32", 4096)
        call t%set_null("i32", 4097)
        call t%set_null("i32", 8192)
        call t%set_null("i32", 8193)
        call t%set_null("i32", nr)
        call t%set_null("i64", 4095)
        call t%set_null("i64", 4098)
        call t%set_null("f64", 4097)
        call t%set_null("f64", nr)
        call t%set_null("f32", 5)
        call t%set_null("f32", 6)
        call t%set_null("bool", 4096)
        call t%set_null("bool", 4097)
        call t%set_null("str", 2)
        call t%set_null("str", 8192)
        call t%set_null("str", nr)
        ! A vector row is null when ANY element is: one element each, at a boundary and at the end.
        call t%set_null("i32v", 4096, 2)
        call t%set_null("i32v", nr, 16)
        call t%set_null("f64v", 1)
        call t%set_null("boolv", 3)
        call t%set_null("strv", 4097, 2)

        call parquet_set_table_threads(threads)
        call t%print_stat()
        if (parquet_debug_get_table_threads_used() > 1_c_int64_t) then
            print '(a)', "parallel scan: yes"
        else
            print '(a)', "parallel scan: no"
        end if
    end subroutine scenario_table_print_stat_scan

    !> %print_stat(stats=.false.): the listing without its null count, minimum and maximum, over a
    !! resident column that was written into (so the edited marker has to find its new place after
    !! the width) and a column left unread.
    subroutine scenario_table_print_stat_no_stats()
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: a(3), b(3)
        character(len=*), parameter :: out_file = "test_run/error_scenario_print_stat_no_stats.parquet"

        a = [1_int32, 2_int32, 3_int32]
        b = [4_int32, 5_int32, 6_int32]
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a)
        call parquet_write_column(writer, "b", b)
        call parquet_close_writer(writer)

        call parquet_open_table(t, out_file)
        call t%set("a", [10_int32, 20_int32, 30_int32])
        call t%print_stat(all=.true., stats=.false.)
    end subroutine scenario_table_print_stat_no_stats

    !> row_validity_range must refuse a range reaching past the column, after accepting one inside it.
    subroutine scenario_column_row_validity_range_out_of_range()
        type(parquet_column) :: c
        logical :: valid(8)

        call c%init(PK_INT32, 5_int64)
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call c%set_null(2_int64)
        call c%row_validity_range(1_int64, 5_int64, valid)
        if (valid(2) .or. .not. all(valid([1, 3, 4, 5]))) then
            error stop "row_validity_range: the in-range control did not report row 2 null and the rest valid"
        end if
        print '(a)', "row_validity_range accepted a range inside the column"
        call c%row_validity_range(2_int64, 6_int64, valid)
        print '(a)', "row_validity_range accepted a range past the end -- it must abort"
    end subroutine scenario_column_row_validity_range_out_of_range

    !> row_validity_range must refuse a mask shorter than the range, after filling one that fits.
    subroutine scenario_column_row_validity_range_short_mask()
        type(parquet_column) :: c
        logical :: fits(5), short(3)

        call c%init(PK_INT32, 5_int64)
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call c%row_validity_range(1_int64, 5_int64, fits)
        if (.not. all(fits)) error stop "row_validity_range: the fitting-mask control reported a null"
        print '(a)', "row_validity_range accepted a mask as long as the range"
        call c%row_validity_range(1_int64, 5_int64, short)
        print '(a)', "row_validity_range accepted a mask shorter than the range -- it must abort"
    end subroutine scenario_column_row_validity_range_short_mask

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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%sort_by(["m_intkey"])   ! a map with INT32 KEYS: v1 reads string keys only -> aborts
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

    !> `%gather_from` refuses a source row outside the SOURCE, as `%gather` refuses one outside
    !! the column, and names the source's row count -- the destination has none yet. The in-range
    !! call first is the control: it must be accepted, or the abort below would say nothing about
    !! the range.
    subroutine scenario_column_gather_from_out_of_range()
        type(parquet_column) :: c, d
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call d%gather_from(c, [3_int64, 1_int64])
        print '(a,i0)', "control: an in-range gather_from was accepted, n=", d%length()
        call d%gather_from(c, [1_int64, 9_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered a row outside the source, n=", d%length()
    end subroutine scenario_column_gather_from_out_of_range

    !> `%gather_from`'s mask has one entry per DESTINATION row -- the index list's length, not the
    !! source's row count, which is the mistake a caller thinking of `%set_validity` would make.
    subroutine scenario_column_gather_from_mask_length_mismatch()
        type(parquet_column) :: c, d
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call d%gather_from(c, [1_int64, 2_int64], valid=[.true., .false.])
        print '(a,i0)', "control: a matching gather_from mask was accepted, n=", d%length()
        call d%gather_from(c, [1_int64, 2_int64], valid=[.true., .false., .true.])   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a mask of the source's length, n=", d%length()
    end subroutine scenario_column_gather_from_mask_length_mismatch

    !> A container column is gathered in place with `%gather`, which is how a join carries one
    !! (feature_risks.md Risk-188); `%gather_from` refuses it as a source rather than copying a
    !! layout only the container knows. The in-place gather first is the control.
    subroutine scenario_column_gather_from_container_source()
        type(parquet_column) :: c, d
        type(parquet_list_column), allocatable :: lc
        class(parquet_container_column), allocatable :: cc
        allocate(lc)
        call lc%init(PK_INT32)
        call lc%append_row([4_int32, 5_int32])
        call lc%append_row([6_int32])
        call move_alloc(lc, cc)
        call c%adopt_container(cc)
        call c%gather([2_int64, 1_int64])
        print '(a,i0)', "control: the container column gathers in place, rows=", c%length()
        call d%gather_from(c, [1_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered from a container column, n=", d%length()
    end subroutine scenario_column_gather_from_container_source

    !> The string store's `gather_from` carries its own range check, as its `gather` does; the
    !! in-range call first is the control.
    subroutine scenario_string_column_gather_from_out_of_range()
        type(parquet_string_column) :: col, dst
        call col%append_string("a")
        call col%append_string("bc")
        call dst%gather_from(col, [2_int64, 1_int64])
        print '(a,i0)', "control: an in-range gather_from was accepted, n=", dst%size()
        call dst%gather_from(col, [2_int64, 5_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered an element outside the source, n=", dst%size()
    end subroutine scenario_string_column_gather_from_out_of_range

    !> The string store's `gather_from` mask has one entry per selected element; the matching mask
    !! first is the control.
    subroutine scenario_string_column_gather_from_length_mismatch()
        type(parquet_string_column) :: col, dst
        call col%append_string("a")
        call col%append_string("bc")
        call dst%gather_from(col, [2_int64, 1_int64], valid=[.true., .false.])
        print '(a,i0)', "control: a matching gather_from mask was accepted, nulls=", dst%null_count()
        call dst%gather_from(col, [2_int64, 1_int64], valid=[.true.])   ! 1 entry for 2 elements -> aborts
        print '(a,i0)', "unexpectedly accepted a short gather_from mask, n=", dst%size()
    end subroutine scenario_string_column_gather_from_length_mismatch

    !> The string store's own gather is reachable directly, so it carries its own range check
    !! rather than relying on parquet_column's.
    subroutine scenario_string_column_gather_out_of_range()
        type(parquet_string_column) :: col
        call col%append_string("a")
        call col%append_string("bc")
        call col%gather([2_int64, 5_int64])   ! -> aborts
        print '(a,i0)', "unexpectedly gathered an element outside the column, n=", col%size()
    end subroutine scenario_string_column_gather_out_of_range

    !> Appending a table to ITSELF is refused: it would argument-associate one object with an
    !! intent(inout) and an intent(in) dummy, and one level down hand parquet_column%append the same
    !! column as both operands (F2018 15.5.2.13). No compiler here diagnoses it, and it usually
    !! appears to work, which is exactly why it is refused rather than left to the optimiser.
    subroutine scenario_table_append_self()
        type(parquet_table) :: t, other
        call parquet_new_table(t)
        call t%add_column("x", [1_int32, 2_int32])
        ! Negative control first: appending a DIFFERENT table with the same columns is legitimate
        ! and must stay so, or a guard that refused every %append would pass this scenario too.
        call parquet_new_table(other)
        call other%add_column("x", [3_int32])
        call t%append(other)
        print '(a,i0)', "append of another table ok, nrows=", t%nrows()
        call t%append(t)   ! -> aborts
        print '(a,i0)', "unexpectedly appended a table to itself, nrows=", t%nrows()
    end subroutine scenario_table_append_self

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
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%cast("m_intkey", PK_FLOAT64)   ! -> aborts
        print '(a,i0)', "unexpectedly cast an unsupported column, ncols=", t%ncols()
    end subroutine scenario_table_cast_unsupported_column

    !> There is no total order on a list, a map or a struct that this library could have chosen --
    !! and the sort must keep reproducing arrow::compute::SortIndices, which has none for them
    !! either. Sorting the same table by a SCALAR key works and carries the container along; that
    !! is asserted by test_container_row_alignment, this scenario's negative control.
    subroutine scenario_container_sort_key()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="container")
        call t%materialize_all()
        call t%sort_by(["ragged"])   ! a PK_LIST sort key -> aborts
        print '(a,i0)', "unexpectedly sorted by a container column, nrows=", t%nrows()
    end subroutine scenario_container_sort_key

    !> `list_columns=` takes a token, so a mistyped one has to name what was expected rather than
    !! merely say it was wrong -- and it is validated BEFORE the reader opens, so the abort happens
    !! with no live Arrow object in scope.
    subroutine scenario_container_bad_list_columns()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="containers")
        print '(a,i0)', "unexpectedly opened a table with an unrecognized list_columns, ncols=", t%ncols()
    end subroutine scenario_container_bad_list_columns

    !> %print_stat's container arm, printed so its VALUES can be asserted.
    !!
    !! It is out of process because %print_stat writes to stdout and parquet_set_message_stream
    !! takes "stdout"/"stderr" and no file, so nothing in process can capture it. Exits cleanly:
    !! the assertion is on what it printed, not on an abort.
    !!
    !! `null_avg` is the discriminating column -- its present rows are all length 5 and four of its
    !! rows are NULL, so a null counted as length zero would print a minimum of 0.
    subroutine scenario_container_print_stat_lengths()
        type(parquet_table) :: t
        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="container")
        call t%materialize_all()
        call t%print_stat()
    end subroutine scenario_container_print_stat_lengths

    !> A row-structural mutation SKIPS a column that is not resident -- container or not -- and the
    !! detach guard is the only thing that reports it afterwards.
    !!
    !! This is feature_container_phase6.md's Q2, resolved to "skip, and let the detach guard catch
    !! the read". The column is left RES_EMPTY, so it holds no storage that could be misaligned;
    !! what makes that safe is that every value accessor routes through table_resolve, which runs
    !! table_check_not_detached. An accessor written any other way would read the skipped column
    !! instead of aborting, and this scenario is what would notice.
    !!
    !! Its negative control is test/test_table_container.f90's test_container_row_alignment, where
    !! the same mutations run on a MATERIALIZED container column and every row stays aligned.
    subroutine scenario_container_skipped_by_mutation()
        type(parquet_table) :: t
        type(parquet_list_column), pointer :: p
        call parquet_open_table(t, "test/fixtures/list_widths.parquet", list_columns="container")
        call t%prefetch("scalar")     ! the container column is deliberately left unread
        call t%delete_rows([1_int64]) ! skips it, and detaches the table from its file
        call t%col("ragged", p)       ! -> aborts rather than handing back a stale column
        print '(a,i0)', "unexpectedly read a container column a mutation had skipped, nrows=", p%nrows()
    end subroutine scenario_container_skipped_by_mutation

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

end module error_scenarios_table
