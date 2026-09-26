!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Error scenarios for what the library computes over a table: sorting, statistics, joins and
!> random draws; spherical, sky-coordinate, spatial and HEALPix geometry; the nested
!> list/map/struct containers; and the logging and TOML configuration layers.
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
module error_scenarios_analysis
    use parquet
    use parquet_strings, only : parquet_string_column
    use parquet_columns
    use parquet_list, only : parquet_list_column, parquet_list_row
    ! `pf_query_disc_runs` is deliberately NOT reachable through the `parquet` facade --
    ! src/parquet.f90 privatises it -- so naming its own module is how a caller reaches it,
    ! and this import is the documented route rather than a workaround.
    use parquet_healpix, only : pf_query_disc_runs
    ! The optimisation scenarios' objectives: module procedures and module-level types shared with
    ! test_optimize.f90, so a scenario and a test name the same objective and neither reaches an
    ! internal procedure.
    use test_optimize_support, only : sphere
    use parquet_tables
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use iso_fortran_env, only : int32, int64, real32, real64
    use error_scenarios_support, only : join_fixture, scenario_setenv, spatial_sky_cloud, write_text_file
    implicit none
    private

    public :: dispatch_error_scenarios_analysis

contains

    !> Run `scenario` if it is one of this module's, and report whether it was.
    !!
    !! `handled` is `.false.` for a name this group does not own, which is how
    !! `error_scenarios.f90` walks the four groups in turn without any of them knowing
    !! what the others hold.
    subroutine dispatch_error_scenarios_analysis(scenario, handled)
        character(len=*), intent(in) :: scenario
        logical, intent(out) :: handled

        handled = .true.
        select case (trim(scenario))
        case ("list_read_not_a_list")
            call scenario_list_read_not_a_list()
        case ("list_read_nested_payload")
            call scenario_list_read_nested_payload()
        case ("list_read_struct_payload")
            call scenario_list_read_struct_payload()
        case ("list_chunk_refuses_sort")
            call scenario_list_chunk_refuses_sort()
        case ("list_chunk_row_group_out_of_range")
            call scenario_list_chunk_row_group_out_of_range()
        case ("list_chunk_incomplete_aborts")
            call scenario_list_chunk_incomplete_aborts()
        case ("list_chunk_complete_ok")
            call scenario_list_chunk_complete_ok()
        case ("list_read_marks_read")
            call scenario_list_read_marks_read()
        case ("list_adopt_rows_offset_mismatch")
            call scenario_list_adopt_rows_offset_mismatch()
        case ("list_adopt_rows_not_monotonic")
            call scenario_list_adopt_rows_not_monotonic()
        case ("list_adopt_rows_bad_payload_kind")
            call scenario_list_adopt_rows_bad_payload_kind()
        case ("list_adopt_rows_mask_length")
            call scenario_list_adopt_rows_mask_length()
        case ("struct_init_no_fields")
            call scenario_struct_init_no_fields()
        case ("struct_init_duplicate_name")
            call scenario_struct_init_duplicate_name()
        case ("struct_init_dotted_name")
            call scenario_struct_init_dotted_name()
        case ("struct_init_bad_kind")
            call scenario_struct_init_bad_kind()
        case ("struct_append_from_field_count")
            call scenario_struct_append_from_field_count()
        case ("struct_append_from_field_kind")
            call scenario_struct_append_from_field_kind()
        case ("struct_append_from_not_struct")
            call scenario_struct_append_from_not_struct()
        case ("struct_gather_rows_out_of_range")
            call scenario_struct_gather_rows_out_of_range()
        case ("struct_field_kind_not_narrowed")
            call scenario_struct_field_kind_not_narrowed()
        case ("struct_nested_not_narrowed")
            call scenario_struct_nested_not_narrowed()
        case ("struct_adopt_dotted_name")
            call scenario_struct_adopt_dotted_name()
        case ("struct_adopt_duplicate_name")
            call scenario_struct_adopt_duplicate_name()
        case ("struct_adopt_bad_kind")
            call scenario_struct_adopt_bad_kind()
        case ("struct_adopt_ragged_rows")
            call scenario_struct_adopt_ragged_rows()
        case ("struct_adopt_row_valid_length")
            call scenario_struct_adopt_row_valid_length()
        case ("struct_set_field_uninitialized")
            call scenario_struct_set_field_uninitialized()
        case ("struct_field_index_out_of_range")
            call scenario_struct_field_index_out_of_range()
        case ("struct_handle_unassociated")
            call scenario_struct_handle_unassociated()
        case ("struct_handle_stale_row")
            call scenario_struct_handle_stale_row()
        case ("struct_get_wrong_kind")
            call scenario_struct_get_wrong_kind()
        case ("struct_get_not_narrowed")
            call scenario_struct_get_not_narrowed()
        case ("struct_field_unknown")
            call scenario_struct_field_unknown()
        case ("struct_field_unknown_warn_ok")
            call scenario_struct_field_unknown_warn_ok()
        case ("struct_write_uninitialized")
            call scenario_struct_write_uninitialized()
        case ("struct_write_type_mismatch")
            call scenario_struct_write_type_mismatch()
        case ("struct_col_size_rejected")
            call scenario_struct_col_size_rejected()
        case ("struct_col_size_auto_rejected")
            call scenario_struct_col_size_auto_rejected()
        case ("struct_set_col_size_rejected")
            call scenario_struct_set_col_size_rejected()
        case ("struct_qc_rejected")
            call scenario_struct_qc_rejected()
        case ("struct_protected_row_null")
            call scenario_struct_protected_row_null()
        case ("struct_protected_field_null")
            call scenario_struct_protected_field_null()
        case ("struct_protected_ok")
            call scenario_struct_protected_ok()
        case ("struct_timestamp_precision")
            call scenario_struct_timestamp_precision()
        case ("map_write_millisecond_precision")
            call scenario_map_write_millisecond_precision()
        case ("qc_all_nan_range")
            call scenario_qc_all_nan_range()
        case ("struct_time_precision")
            call scenario_struct_time_precision()
        case ("struct_temporal_precision_ok")
            call scenario_struct_temporal_precision_ok()
        case ("struct_read_nested_field")
            call scenario_struct_read_nested_field()
        case ("struct_read_not_a_struct")
            call scenario_struct_read_not_a_struct()
        case ("struct_view_out_of_range")
            call scenario_struct_view_out_of_range()
        case ("struct_set_field_unknown")
            call scenario_struct_set_field_unknown()
        case ("struct_set_field_wrong_kind")
            call scenario_struct_set_field_wrong_kind()
        case ("map_init_bad_kind")
            call scenario_map_init_bad_kind()
        case ("write_nested_list")
            call scenario_write_nested_list()
        case ("write_nested_list_control")
            call scenario_write_nested_list_control()
        case ("write_nested_struct_field")
            call scenario_write_nested_struct_field()
        case ("qc_descent_path")
            call scenario_qc_descent_path()
        case ("qc_descent_path_control")
            call scenario_qc_descent_path_control()
        case ("maml_list_bare_token")
            call scenario_maml_list_bare_token()
        case ("list_row_index_unassigned")
            call scenario_list_row_index_unassigned()
        case ("map_row_index_unassigned")
            call scenario_map_row_index_unassigned()
        case ("list_row_index_live")
            call scenario_list_row_index_live()
        case ("filter_descent_path")
            call scenario_filter_descent_path()
        case ("filter_descent_path_control")
            call scenario_filter_descent_path_control()
        case ("sort_key_descent_path")
            call scenario_sort_key_descent_path()
        case ("map_init_nested_value")
            call scenario_map_init_nested_value()
        case ("list_init_nested_payload")
            call scenario_list_init_nested_payload()
        case ("struct_init_nested_field")
            call scenario_struct_init_nested_field()
        case ("struct_read_nested_struct_field")
            call scenario_struct_read_nested_struct_field()
        case ("map_append_wrong_kind")
            call scenario_map_append_wrong_kind()
        case ("map_append_length_mismatch")
            call scenario_map_append_length_mismatch()
        case ("map_get_wrong_kind")
            call scenario_map_get_wrong_kind()
        case ("map_get_missing_key")
            call scenario_map_get_missing_key()
        case ("map_get_missing_key_warn_ok")
            call scenario_map_get_missing_key_warn_ok()
        case ("map_get_occurrence_zero")
            call scenario_map_get_occurrence_zero()
        case ("map_get_at_out_of_range")
            call scenario_map_get_at_out_of_range()
        case ("map_view_out_of_range")
            call scenario_map_view_out_of_range()
        case ("map_read_not_a_map")
            call scenario_map_read_not_a_map()
        case ("map_read_int_key")
            call scenario_map_read_int_key()
        case ("map_read_nested_value")
            call scenario_map_read_nested_value()
        case ("map_write_uninitialized")
            call scenario_map_write_uninitialized()
        case ("map_write_type_mismatch")
            call scenario_map_write_type_mismatch()
        case ("map_col_size_rejected")
            call scenario_map_col_size_rejected()
        case ("map_col_size_auto_rejected")
            call scenario_map_col_size_auto_rejected()
        case ("map_qc_rejected")
            call scenario_map_qc_rejected()
        case ("map_protected_row_null")
            call scenario_map_protected_row_null()
        case ("map_protected_value_null")
            call scenario_map_protected_value_null()
        case ("map_protected_ok")
            call scenario_map_protected_ok()
        case ("map_entry_limit")
            call scenario_map_entry_limit()
        case ("map_adopt_rows_offset_mismatch")
            call scenario_map_adopt_rows_offset_mismatch()
        case ("map_adopt_rows_bad_key_kind")
            call scenario_map_adopt_rows_bad_key_kind()
        case ("list_stale_handle")
            call scenario_list_stale_handle()
        case ("map_gather_out_of_range")
            call scenario_map_gather_out_of_range()
        case ("map_append_from_kind_mismatch")
            call scenario_map_append_from_kind_mismatch()
        case ("map_append_from_not_a_map")
            call scenario_map_append_from_not_a_map()
        case ("map_adopt_rows_bad_value_kind")
            call scenario_map_adopt_rows_bad_value_kind()
        case ("map_adopt_rows_length_mismatch")
            call scenario_map_adopt_rows_length_mismatch()
        case ("map_adopt_rows_row_valid_length")
            call scenario_map_adopt_rows_row_valid_length()
        case ("map_append_from_uninitialized")
            call scenario_map_append_from_uninitialized()
        case ("map_stale_handle")
            call scenario_map_stale_handle()
        case ("map_append_is_valid_length")
            call scenario_map_append_is_valid_length()
        case ("map_missing_key_preview")
            call scenario_map_missing_key_preview()
        case ("map_chunk_refuses_sort")
            call scenario_map_chunk_refuses_sort()
        case ("maml_map_bare_token")
            call scenario_maml_map_bare_token()
        case ("maml_map_nested_value")
            call scenario_maml_map_nested_value()
        case ("maml_text_list_bad_element")
            call scenario_maml_text_list_bad_element()
        case ("maml_text_map_bad_value")
            call scenario_maml_text_map_bad_value()
        case ("list_write_type_mismatch")
            call scenario_list_write_type_mismatch()
        case ("list_write_uninitialized")
            call scenario_list_write_uninitialized()
        case ("list_write_col_size_rejected")
            call scenario_list_write_col_size_rejected()
        case ("list_col_size_auto_rejected")
            call scenario_list_col_size_auto_rejected()
        case ("list_set_col_size_forced_rejected")
            call scenario_list_set_col_size_forced_rejected()
        case ("list_write_qc_rejected")
            call scenario_list_write_qc_rejected()
        case ("list_write_bad_token")
            call scenario_list_write_bad_token()
        case ("list_write_unknown_element")
            call scenario_list_write_unknown_element()
        case ("list_write_protected_row_null")
            call scenario_list_write_protected_row_null()
        case ("list_write_protected_element_null")
            call scenario_list_write_protected_element_null()
        case ("list_write_protected_ok")
            call scenario_list_write_protected_ok()
        case ("list_write_row_too_long")
            call scenario_list_write_row_too_long()
        case ("list_write_chunk_too_many_elements")
            call scenario_list_write_chunk_too_many_elements()
        case ("list_write_explicit_chunk_size_too_big")
            call scenario_list_write_explicit_chunk_size_too_big()
        case ("list_write_ceiling_ok")
            call scenario_list_write_ceiling_ok()
        case ("list_write_large_list_roundtrip")
            call scenario_list_write_large_list_roundtrip()
        case ("list_write_large_string_child")
            call scenario_list_write_large_string_child()
        case ("list_write_large_list_chunked")
            call scenario_list_write_large_list_chunked()
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
        case ("sorting_quantile_ok_still_checks_range")
            call scenario_sorting_quantile_ok_still_checks_range()
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
        case ("stats_is_valid_length_mismatch")
            call scenario_stats_is_valid_length_mismatch()
        case ("stats_weights_length_mismatch")
            call scenario_stats_weights_length_mismatch()
        case ("stats_negative_weight")
            call scenario_stats_negative_weight()
        case ("stats_nan_weight")
            call scenario_stats_nan_weight()
        case ("stats_infinite_weight")
            call scenario_stats_infinite_weight()
        case ("stats_unknown_weight_type")
            call scenario_stats_unknown_weight_type()
        case ("stats_object_query_before_compute")
            call scenario_stats_object_query_before_compute()
        case ("stats_object_merge_retain_mismatch")
            call scenario_stats_object_merge_retain_mismatch()
        case ("stats_object_merge_weight_type_mismatch")
            call scenario_stats_object_merge_weight_type_mismatch()
        case ("stats_object_merge_skipnan_mismatch")
            call scenario_stats_object_merge_skipnan_mismatch()
        case ("stats_object_gmean_without_retain")
            call scenario_stats_object_gmean_without_retain()
        case ("stats_object_hmean_without_retain")
            call scenario_stats_object_hmean_without_retain()
        case ("stats_object_merge_uncomputed_source")
            call scenario_stats_object_merge_uncomputed_source()
        case ("stats_column_string_kind")
            call scenario_stats_column_string_kind()
        case ("stats_column_vector_width")
            call scenario_stats_column_vector_width()
        case ("stats_column_is_valid_conflict")
            call scenario_stats_column_is_valid_conflict()
        case ("stats_quantile_bad_probability")
            call scenario_stats_quantile_bad_probability()
        case ("stats_quantile_bad_method")
            call scenario_stats_quantile_bad_method()
        case ("stats_quantiles_size_mismatch")
            call scenario_stats_quantiles_size_mismatch()
        case ("stats_trim_mean_bad_prop")
            call scenario_stats_trim_mean_bad_prop()
        case ("stats_score_not_finite")
            call scenario_stats_score_not_finite()
        case ("stats_score_bad_kind")
            call scenario_stats_score_bad_kind()
        case ("stats_median_on_streaming")
            call scenario_stats_median_on_streaming()
        case ("stats_mad_bad_scale")
            call scenario_stats_mad_bad_scale()
        case ("stats_mad_center_not_finite")
            call scenario_stats_mad_center_not_finite()
        case ("stats_mode_string_column_is_valid")
            call scenario_stats_mode_string_column_is_valid()
        case ("stats_corr_bad_method")
            call scenario_stats_corr_bad_method()
        case ("stats_normal_scores_bad_method")
            call scenario_stats_normal_scores_bad_method()
        case ("stats_probit_fit_bad_method")
            call scenario_stats_probit_fit_bad_method()
        case ("stats_probit_scale_bad_prob")
            call scenario_stats_probit_scale_bad_prob()
        case ("stats_probit_mean_bad_weight")
            call scenario_stats_probit_mean_bad_weight()
        case ("stats_object_probit_mean_without_retain")
            call scenario_stats_object_probit_mean_without_retain()
        case ("stats_spearman_with_weights")
            call scenario_stats_spearman_with_weights()
        case ("stats_pair_size_mismatch")
            call scenario_stats_pair_size_mismatch()
        case ("stats_clip_bad_cenfunc")
            call scenario_stats_clip_bad_cenfunc()
        case ("stats_clip_bad_sigma")
            call scenario_stats_clip_bad_sigma()
        case ("stats_zscore_size_mismatch")
            call scenario_stats_zscore_size_mismatch()
        case ("stats_cumsum_size_mismatch")
            call scenario_stats_cumsum_size_mismatch()
        case ("stats_cum_out_valid_mismatch")
            call scenario_stats_cum_out_valid_mismatch()
        case ("stats_edges_too_few")
            call scenario_stats_edges_too_few()
        case ("stats_edges_not_increasing")
            call scenario_stats_edges_not_increasing()
        case ("stats_edges_nan")
            call scenario_stats_edges_nan()
        case ("stats_histogram_counts_size")
            call scenario_stats_histogram_counts_size()
        case ("stats_bin_edges_nbins")
            call scenario_stats_bin_edges_nbins()
        case ("stats_bin_edges_size")
            call scenario_stats_bin_edges_size()
        case ("stats_bucketize_codes_size")
            call scenario_stats_bucketize_codes_size()
        case ("stats_bin_linear_grid_too_short")
            call scenario_stats_bin_linear_grid_too_short()
        case ("stats_bin_linear_grid_not_increasing")
            call scenario_stats_bin_linear_grid_not_increasing()
        case ("stats_bin_linear_nan_grid_point")
            call scenario_stats_bin_linear_nan_grid_point()
        case ("stats_bin_linear_infinite_grid_point")
            call scenario_stats_bin_linear_infinite_grid_point()
        case ("stats_bin_linear_spacing_overflows")
            call scenario_stats_bin_linear_spacing_overflows()
        case ("stats_bin_linear_mass_size")
            call scenario_stats_bin_linear_mass_size()
        case ("stats_bin_linear_negative_weight")
            call scenario_stats_bin_linear_negative_weight()
        case ("stats_bin_linear_weights_size")
            call scenario_stats_bin_linear_weights_size()
        case ("stats_bin_linear_logical_column")
            call scenario_stats_bin_linear_logical_column()
        case ("stats_zscore_out_valid_size")
            call scenario_stats_zscore_out_valid_size()
        case ("stats_normal_scores_size")
            call scenario_stats_normal_scores_size()
        case ("stats_normal_scores_out_valid_size")
            call scenario_stats_normal_scores_out_valid_size()
        case ("stats_sigma_clip_keep_size")
            call scenario_stats_sigma_clip_keep_size()
        case ("stats_obj_order_without_population")
            call scenario_stats_obj_order_without_population()
        case ("stats_obj_quantiles_out_size")
            call scenario_stats_obj_quantiles_out_size()
        case ("stats_obj_trim_mean_prop")
            call scenario_stats_obj_trim_mean_prop()
        case ("stats_obj_percentile_score_non_finite")
            call scenario_stats_obj_percentile_score_non_finite()
        case ("stats_print_default_stream")
            call scenario_stats_print_default_stream()
        case ("sorting_column_vector")
            call scenario_sorting_column_vector()
        case ("sorting_match_kind_mismatch")
            call scenario_sorting_match_kind_mismatch()
        case ("join_self")
            call scenario_join_self()
        case ("join_kind_mismatch")
            call scenario_join_kind_mismatch()
        case ("join_require_m1")
            call scenario_join_engine_twin("sort", "require_m1")
        case ("join_max_rows")
            call scenario_join_engine_twin("sort", "max_rows")
        case ("join_max_rows_arr_i32")
            call scenario_join_engine_twin("sort", "max_rows_arr_i32")
        case ("join_max_rows_arr_i64")
            call scenario_join_engine_twin("sort", "max_rows_arr_i64")
        case ("join_max_rows_str_i32")
            call scenario_join_engine_twin("sort", "max_rows_str_i32")
        case ("join_max_rows_str_i64")
            call scenario_join_engine_twin("sort", "max_rows_str_i64")
        case ("join_require_1m")
            call scenario_join_engine_twin("sort", "require_1m")
        case ("join_require_m1_hash")
            call scenario_join_engine_twin("hash", "require_m1")
        case ("join_require_1m_hash")
            call scenario_join_engine_twin("hash", "require_1m")
        case ("join_require_m1_threaded")
            call scenario_join_engine_twin("sort", "require_m1", threads=4)
        case ("join_require_1m_threaded")
            call scenario_join_engine_twin("sort", "require_1m", threads=4)
        case ("join_max_rows_hash")
            call scenario_join_engine_twin("hash", "max_rows")
        case ("join_max_rows_arr_i32_hash")
            call scenario_join_engine_twin("hash", "max_rows_arr_i32")
        case ("join_max_rows_arr_i64_hash")
            call scenario_join_engine_twin("hash", "max_rows_arr_i64")
        case ("join_max_rows_str_i32_hash")
            call scenario_join_engine_twin("hash", "max_rows_str_i32")
        case ("join_max_rows_str_i64_hash")
            call scenario_join_engine_twin("hash", "max_rows_str_i64")
        case ("join_pair_count_overflow")
            call scenario_join_pair_count_overflow()
        case ("join_bad_require")
            call scenario_join_bad_require()
        case ("join_bad_order")
            call scenario_join_bad_order()
        case ("join_bad_how")
            call scenario_join_bad_how()
        case ("join_other_on_size")
            call scenario_join_other_on_size()
        case ("join_no_key")
            call scenario_join_no_key()
        case ("join_columns_with_semi")
            call scenario_join_columns_with_semi()
        case ("join_left_container_outer")
            call scenario_join_left_container_outer()
        case ("join_key_direction")
            call scenario_join_key_direction()
        case ("join_key_direction_dash")
            call scenario_join_key_direction_dash()
        case ("join_container_payload")
            call scenario_join_container_payload()
        case ("join_container_key")
            call scenario_join_container_key()
        case ("join_columns_unknown")
            call scenario_join_columns_unknown()
        case ("join_suffix_clash")
            call scenario_join_suffix_clash()
        case ("join_blank_suffix")
            call scenario_join_blank_suffix()
        case ("join_detached_column")
            call scenario_join_detached_column()
        case ("sorting_search_unsorted")
            call scenario_sorting_search_unsorted()
        case ("sorting_search_target_too_long")
            call scenario_sorting_search_target_too_long()
        case ("sorting_search_many_answer_length")
            call scenario_sorting_search_many_answer_length()
        case ("sorting_search_many_target_too_long")
            call scenario_sorting_search_many_target_too_long()
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
        case ("random_normal_truncated_sigma_not_positive")
            call scenario_random_normal_truncated_sigma_not_positive()
        case ("random_normal_truncated_sigma_nan")
            call scenario_random_normal_truncated_sigma_nan()
        case ("random_normal_truncated_bounds_reversed")
            call scenario_random_normal_truncated_bounds_reversed()
        case ("random_normal_truncated_bounds_nan")
            call scenario_random_normal_truncated_bounds_nan()
        case ("random_normal_truncated_bounds_collapse")
            call scenario_random_normal_truncated_bounds_collapse()
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
        case ("random_disc_radius_negative")
            call scenario_random_disc_radius_negative()
        case ("random_disc_radius_nan")
            call scenario_random_disc_radius_nan()
        case ("random_disc_centre_zero")
            call scenario_random_disc_centre_zero()
        case ("random_disc_centre_nan")
            call scenario_random_disc_centre_nan()
        case ("random_disc_inner_exceeds_radius")
            call scenario_random_disc_inner_exceeds_radius()
        case ("random_disc_inner_above_half_turn")
            call scenario_random_disc_inner_above_half_turn()
        case ("random_disc_inner_nan")
            call scenario_random_disc_inner_nan()
        case ("random_disc_radec_dec_out_of_range")
            call scenario_random_disc_radec_dec_out_of_range()
        case ("random_disc_radec_centre_nan")
            call scenario_random_disc_radec_centre_nan()
        case ("random_disc_radec_radius_negative")
            call scenario_random_disc_radec_radius_negative()
        case ("random_ball_radius_negative")
            call scenario_random_ball_radius_negative()
        case ("random_ball_inner_exceeds_radius")
            call scenario_random_ball_inner_exceeds_radius()
        case ("random_vmf_kappa_negative")
            call scenario_random_vmf_kappa_negative()
        case ("random_vmf_kappa_nan")
            call scenario_random_vmf_kappa_nan()
        case ("random_vmf_mu_zero")
            call scenario_random_vmf_mu_zero()
        case ("random_vmf_radec_sigma_not_positive")
            call scenario_random_vmf_radec_sigma_not_positive()
        case ("random_vmf_radec_sigma_nan")
            call scenario_random_vmf_radec_sigma_nan()
        case ("random_fill_direction_bad_shape")
            call scenario_random_fill_direction_bad_shape()
        case ("random_fill_radec_size_mismatch")
            call scenario_random_fill_radec_size_mismatch()
        case ("random_sphere_draw_beyond_2p62")
            call scenario_random_sphere_draw_beyond_2p62()
        case ("random_fill_direction_draw_beyond_2p62")
            call scenario_random_fill_direction_draw_beyond_2p62()
        case ("random_stream_disc_inner_exceeds_radius")
            call scenario_random_stream_disc_inner_exceeds_radius()
        case ("random_disc_cap_unprepared")
            call scenario_random_disc_cap_unprepared()
        case ("sphere_polygon_too_few_vertices")
            call scenario_sphere_polygon_too_few_vertices()
        case ("sphere_polygon_size_mismatch")
            call scenario_sphere_polygon_size_mismatch()
        case ("sphere_polygon_nonfinite_vertex")
            call scenario_sphere_polygon_nonfinite_vertex()
        case ("sphere_polygon_nan_vertex")
            call scenario_sphere_polygon_nan_vertex()
        case ("sphere_polygon_extreme_vertex")
            call scenario_sphere_polygon_extreme_vertex()
        case ("sphere_polygon_strict_short_way")
            call scenario_sphere_polygon_strict_short_way()
        case ("sphere_polygon_dec_out_of_range")
            call scenario_sphere_polygon_dec_out_of_range()
        case ("sphere_polygon_bad_edge_rule")
            call scenario_sphere_polygon_bad_edge_rule()
        case ("sphere_polygon_ra_extent_over_360")
            call scenario_sphere_polygon_ra_extent_over_360()
        case ("sphere_polygon_not_in_hemisphere")
            call scenario_sphere_polygon_not_in_hemisphere()
        case ("sphere_polygon_vertices_cancel")
            call scenario_sphere_polygon_vertices_cancel()
        case ("sphere_polygon_zero_area")
            call scenario_sphere_polygon_zero_area()
        case ("sphere_polygon_below_acceptance_floor")
            call scenario_sphere_polygon_below_acceptance_floor()
        case ("sphere_polygon_init_twice")
            call scenario_sphere_polygon_init_twice()
        case ("sphere_polygon_contains_before_init")
            call scenario_sphere_polygon_contains_before_init()
        case ("sphere_polygon_is_simple_before_init")
            call scenario_sphere_polygon_is_simple_before_init()
        case ("sphere_polygon_area_before_init")
            call scenario_sphere_polygon_area_before_init()
        case ("sphere_polygon_area_deg2_before_init")
            call scenario_sphere_polygon_area_deg2_before_init()
        case ("sphere_polygon_acceptance_before_init")
            call scenario_sphere_polygon_acceptance_before_init()
        case ("sphere_polygon_bounds_before_init")
            call scenario_sphere_polygon_bounds_before_init()
        case ("sphere_polygon_random_before_init")
            call scenario_sphere_polygon_random_before_init()
        case ("sphere_polygon_fill_before_init")
            call scenario_sphere_polygon_fill_before_init()
        case ("sphere_polygon_fill_size_mismatch")
            call scenario_sphere_polygon_fill_size_mismatch()
        case ("sphere_polygon_fill_draw_overflow")
            call scenario_sphere_polygon_fill_draw_overflow()
        case ("sphere_polygon_candidate_cap_reached")
            call scenario_sphere_polygon_candidate_cap_reached()
        case ("sphere_stream_exhausted")
            call scenario_sphere_stream_exhausted()
        case ("sphere_pixel_grid_not_built")
            call scenario_sphere_pixel_grid_not_built()
        case ("sphere_pixel_nside_over_limit")
            call scenario_sphere_pixel_nside_over_limit()
        case ("sphere_pixel_ipix_out_of_range")
            call scenario_sphere_pixel_ipix_out_of_range()
        case ("sphere_mask_empty_list")
            call scenario_sphere_mask_empty_list()
        case ("sphere_mask_entry_out_of_range")
            call scenario_sphere_mask_entry_out_of_range()
        case ("sphere_mask_entry_out_of_range_int32")
            call scenario_sphere_mask_entry_out_of_range_int32()
        case ("sphere_fill_mask_bad_shape")
            call scenario_sphere_fill_mask_bad_shape()
        case ("sphere_fill_mask_radec_size_mismatch")
            call scenario_sphere_fill_mask_radec_size_mismatch()
        case ("sphere_fill_mask_bad_shape_int32")
            call scenario_sphere_fill_mask_bad_shape_int32()
        case ("sphere_fill_mask_radec_size_mismatch_int32")
            call scenario_sphere_fill_mask_radec_size_mismatch_int32()
        case ("sphere_fill_mask_entry_out_of_range")
            call scenario_sphere_fill_mask_entry_out_of_range()
        case ("sphere_fill_mask_radec_entry_out_of_range")
            call scenario_sphere_fill_mask_radec_entry_out_of_range()
        case ("sphere_fill_mask_draw_overflow")
            call scenario_sphere_fill_mask_draw_overflow()
        case ("sphere_offset_dec_out_of_range")
            call scenario_sphere_offset_dec_out_of_range()
        case ("sphere_offset_negative_separation")
            call scenario_sphere_offset_negative_separation()
        case ("skycoord_convert_unknown_system")
            call scenario_skycoord_convert_unknown_system()
        case ("skycoord_system_name_not_a_selector")
            call scenario_skycoord_system_name_not_a_selector()
        case ("skycoord_radec2str_width_overflow")
            call scenario_skycoord_radec2str_width_overflow()
        case ("skycoord_text_bad_separator")
            call scenario_skycoord_text_bad_separator()
        case ("skycoord_text_precision_negative")
            call scenario_skycoord_text_precision_negative()
        case ("skycoord_zcmb_unknown_system")
            call scenario_skycoord_zcmb_unknown_system()
        case ("skycoord_rotation_apply_before_init")
            call scenario_skycoord_rotation_apply_before_init()
        case ("skycoord_rotation_init_unknown_system")
            call scenario_skycoord_rotation_init_unknown_system()
        case ("skycoord_apply_pm_dec_out_of_range")
            call scenario_skycoord_apply_pm_dec_out_of_range()
        case ("skycoord_radec2tan_dec0_out_of_range")
            call scenario_skycoord_radec2tan_dec0_out_of_range()
        case ("skycoord_tan2radec_dec0_out_of_range")
            call scenario_skycoord_tan2radec_dec0_out_of_range()
        case ("skycoord_zcmb2zhel_unknown_system")
            call scenario_skycoord_zcmb2zhel_unknown_system()
        case ("sphere_fibonacci_n_not_positive")
            call scenario_sphere_fibonacci_n_not_positive()
        case ("sphere_fibonacci_bad_shape")
            call scenario_sphere_fibonacci_bad_shape()
        case ("sphere_fibonacci_bad_frame")
            call scenario_sphere_fibonacci_bad_frame()
        case ("sphere_fibonacci_radec_bad_size")
            call scenario_sphere_fibonacci_radec_bad_size()
        case ("sphere_radec2vec_bad_frame")
            call scenario_sphere_radec2vec_bad_frame()
        case ("sphere_vec2radec_bad_frame")
            call scenario_sphere_vec2radec_bad_frame()
        case ("metadata_datatype_key_collision")
            call scenario_metadata_datatype_key_collision()
        case ("metadata_datatype_no_collision_control")
            call scenario_metadata_datatype_no_collision_control()
        case ("spatial_query_before_build")
            call scenario_spatial_query_before_build()
        case ("spatial_length_mismatch")
            call scenario_spatial_length_mismatch()
        case ("spatial_build_nan_coord")
            call scenario_spatial_build_nan_coord()
        case ("spatial_build_inf_coord")
            call scenario_spatial_build_inf_coord()
        case ("spatial_build_nan_radius")
            call scenario_spatial_build_nan_radius()
        case ("spatial_build_sky_nan_ra")
            call scenario_spatial_build_sky_nan_ra()
        case ("spatial_rebuild_nan_coord")
            call scenario_spatial_rebuild_nan_coord()
        case ("spatial_query_nan_point")
            call scenario_spatial_query_nan_point()
        case ("spatial_segment_nan_endpoint")
            call scenario_spatial_segment_nan_endpoint()
        case ("spatial_axis_length_overflows")
            call scenario_spatial_axis_length_overflows()
        case ("spatial_nearest_nan_point")
            call scenario_spatial_nearest_nan_point()
        case ("spatial_sky_query_nan_dec")
            call scenario_spatial_sky_query_nan_dec()
        case ("spatial_bulk_nan_radius")
            call scenario_spatial_bulk_nan_radius()
        case ("spatial_bulk_nan_inner_radius")
            call scenario_spatial_bulk_nan_inner_radius()
        case ("spatial_rebuild_for_nan_radius")
            call scenario_spatial_rebuild_for_nan_radius()
        case ("spatial_radius_not_positive")
            call scenario_spatial_radius_not_positive()
        case ("spatial_box_needs_both")
            call scenario_spatial_box_needs_both()
        case ("spatial_radius_exceeds_half_box")
            call scenario_spatial_radius_exceeds_half_box()
        case ("spatial_query_rank_mismatch")
            call scenario_spatial_query_rank_mismatch()
        case ("spatial_rebuild_needs_copy")
            call scenario_spatial_rebuild_needs_copy()
        case ("spatial_bulk_radius_length")
            call scenario_spatial_bulk_radius_length()
        case ("spatial_pairs_bad_combine")
            call scenario_spatial_pairs_bad_combine()
        case ("spatial_sky_pairs_sum_too_large")
            call scenario_spatial_sky_pairs_sum_too_large()
        case ("spatial_pairs_int32_rows")
            call scenario_spatial_pairs_int32_rows()
        case ("spatial_csr_int32_offsets")
            call scenario_spatial_csr_int32_offsets()
        case ("spatial_los_on_sky")
            call scenario_spatial_los_on_sky()
        case ("spatial_los_on_2d")
            call scenario_spatial_los_on_2d()
        case ("spatial_los_on_periodic")
            call scenario_spatial_los_on_periodic()
        case ("spatial_los_point_at_observer")
            call scenario_spatial_los_point_at_observer()
        case ("spatial_within_los_point_at_observer")
            call scenario_spatial_within_los_point_at_observer()
        case ("spatial_los_length_mismatch")
            call scenario_spatial_los_length_mismatch()
        case ("spatial_los_lengths_not_per_point")
            call scenario_spatial_los_lengths_not_per_point()
        case ("spatial_los_negative_length")
            call scenario_spatial_los_negative_length()
        case ("spatial_los_bad_combine")
            call scenario_spatial_los_bad_combine()
        case ("spatial_within_los_needs_los_p")
            call scenario_spatial_within_los_needs_los_p()
        case ("spatial_within_los_los_p_refused")
            call scenario_spatial_within_los_los_p_refused()
        case ("spatial_within_los_zero_length")
            call scenario_spatial_within_los_zero_length()
        case ("spatial_within_los_rank")
            call scenario_spatial_within_los_rank()
        case ("spatial_within_los_los_p_nan")
            call scenario_spatial_within_los_los_p_nan()
        case ("spatial_los_constant")
            call scenario_spatial_los_constant()
        case ("spatial_los_nan")
            call scenario_spatial_los_nan()
        case ("spatial_los_length")
            call scenario_spatial_los_length()
        case ("spatial_los_build_on_2d")
            call scenario_spatial_los_build_on_2d()
        case ("spatial_los_build_on_periodic")
            call scenario_spatial_los_build_on_periodic()
        case ("spatial_los_observer_size")
            call scenario_spatial_los_observer_size()
        case ("spatial_los_observer_nan")
            call scenario_spatial_los_observer_nan()
        case ("spatial_los_build_point_at_observer")
            call scenario_spatial_los_build_point_at_observer()
        case ("spatial_rebuild_los_missing")
            call scenario_spatial_rebuild_los_missing()
        case ("spatial_rebuild_los_unexpected")
            call scenario_spatial_rebuild_los_unexpected()
        case ("spatial_rebuild_los_length")
            call scenario_spatial_rebuild_los_length()
        case ("spatial_los_not_a_function_warns")
            call scenario_spatial_los_not_a_function_warns()
        case ("spatial_los_window_spans_catalogue_warns")
            call scenario_spatial_los_window_spans_catalogue_warns()
        case ("spatial_copy_false_strided")
            call scenario_spatial_copy_false_strided()
        case ("spatial_threads_below_one")
            call scenario_spatial_threads_below_one()
        case ("healpix_disc_nside_not_power2")
            call scenario_healpix_disc_nside_not_power2()
        case ("healpix_disc_nside_zero")
            call scenario_healpix_disc_nside_zero()
        case ("healpix_disc_nside_int32_overflow")
            call scenario_healpix_disc_nside_int32_overflow()
        case ("healpix_disc_nside_int64_overflow")
            call scenario_healpix_disc_nside_int64_overflow()
        case ("healpix_disc_radius_nan")
            call scenario_healpix_disc_radius_nan()
        case ("healpix_disc_radius_negative")
            call scenario_healpix_disc_radius_negative()
        case ("healpix_disc_vector_zero")
            call scenario_healpix_disc_vector_zero()
        case ("healpix_disc_vector_nan")
            call scenario_healpix_disc_vector_nan()
        case ("healpix_disc_bad_scheme")
            call scenario_healpix_disc_bad_scheme()
        case ("healpix_disc_buffer_too_small")
            call scenario_healpix_disc_buffer_too_small()
        case ("healpix_disc_runs_bad_rows")
            call scenario_healpix_disc_runs_bad_rows()
        case ("healpix_disc_count_bad_nside")
            call scenario_healpix_disc_count_bad_nside()
        case ("healpix_disc_alloc_bad_scheme")
            call scenario_healpix_disc_alloc_bad_scheme()
        case ("healpix_disc_max_count_bad_nside")
            call scenario_healpix_disc_max_count_bad_nside()
        case ("healpix_disc_max_count_negative_radius")
            call scenario_healpix_disc_max_count_negative_radius()
        case ("healpix_bulk_nside_invalid")
            call scenario_healpix_bulk_nside_invalid()
        case ("healpix_bulk_size_mismatch")
            call scenario_healpix_bulk_size_mismatch()
        case ("healpix_bulk_threads_zero")
            call scenario_healpix_bulk_threads_zero()
        case ("healpix_bulk_vec_shape")
            call scenario_healpix_bulk_vec_shape()
        case ("healpix_grid_init_nside_zero")
            call scenario_healpix_grid_init_nside_zero()
        case ("healpix_grid_init_nside_not_power")
            call scenario_healpix_grid_init_nside_not_power()
        case ("healpix_grid_init_nside_int32_ceiling")
            call scenario_healpix_grid_init_nside_int32_ceiling()
        case ("healpix_grid_init_bad_scheme")
            call scenario_healpix_grid_init_bad_scheme()
        case ("healpix_grid_init_bad_frame")
            call scenario_healpix_grid_init_bad_frame()
        case ("healpix_grid_disc_unset")
            call scenario_healpix_grid_disc_unset()
        case ("healpix_grid_bulk_unset")
            call scenario_healpix_grid_bulk_unset()
        case ("healpix_grid_npix_int32_overflow")
            call scenario_healpix_grid_npix_int32_overflow()
        case ("healpix_grid_disc_int32_too_fine")
            call scenario_healpix_grid_disc_int32_too_fine()
        case ("spatial_sky_query_on_euclidean")
            call scenario_spatial_sky_query_on_euclidean()
        case ("spatial_euclidean_query_on_sky")
            call scenario_spatial_euclidean_query_on_sky()
        case ("spatial_sky_bulk_refused")
            call scenario_spatial_sky_bulk_refused()
        case ("spatial_sky_bulk_on_euclidean")
            call scenario_spatial_sky_bulk_on_euclidean()
        case ("spatial_sky_rsky_too_large")
            call scenario_spatial_sky_rsky_too_large()
        case ("spatial_sky_dec_out_of_range")
            call scenario_spatial_sky_dec_out_of_range()
        case ("spatial_rebuild_for_sky_too_large")
            call scenario_spatial_rebuild_for_sky_too_large()
        case ("spatial_sky_bad_backend")
            call scenario_spatial_sky_bad_backend()
        case ("spatial_sky_cell_with_healpix")
            call scenario_spatial_sky_cell_with_healpix()
        case ("spatial_sky_nside_without_healpix")
            call scenario_spatial_sky_nside_without_healpix()
        case ("spatial_sky_nside_not_power2")
            call scenario_spatial_sky_nside_not_power2()
        case ("spatial_sky_nside_zero")
            call scenario_spatial_sky_nside_zero()
        case ("spatial_sky_rebuild_refused")
            call scenario_spatial_sky_rebuild_refused()
        case ("spatial_axis_on_periodic")
            call scenario_spatial_axis_on_periodic()
        case ("spatial_axis_before_build")
            call scenario_spatial_axis_before_build()
        case ("spatial_axis_radius_negative")
            call scenario_spatial_axis_radius_negative()
        case ("spatial_annulus_inner_exceeds_outer")
            call scenario_spatial_annulus_inner_exceeds_outer()
        case ("spatial_annulus_inner_negative")
            call scenario_spatial_annulus_inner_negative()
        case ("spatial_bulk_inner_length")
            call scenario_spatial_bulk_inner_length()
        case ("spatial_bulk_inner_exceeds_outer")
            call scenario_spatial_bulk_inner_exceeds_outer()
        case ("spatial_sky_annulus_too_large")
            call scenario_spatial_sky_annulus_too_large()
        case ("spatial_axis_point_rank")
            call scenario_spatial_axis_point_rank()
        case ("spatial_nearest_k_below_one")
            call scenario_spatial_nearest_k_below_one()
        case ("spatial_nearest_periodic_unreachable")
            call scenario_spatial_nearest_periodic_unreachable()
        case ("spatial_nearest_sky_on_euclidean")
            call scenario_spatial_nearest_sky_on_euclidean()
        case ("spatial_kth_k_below_one")
            call scenario_spatial_kth_k_below_one()
        case ("spatial_kth_k_too_large")
            call scenario_spatial_kth_k_too_large()
        case ("spatial_kth_on_sky_index")
            call scenario_spatial_kth_on_sky_index()
        case ("spatial_kth_sky_on_euclidean")
            call scenario_spatial_kth_sky_on_euclidean()
        case ("spatial_components_length")
            call scenario_spatial_components_length()
        case ("spatial_components_endpoint_range")
            call scenario_spatial_components_endpoint_range()
        case ("spatial_components_min_size_zero")
            call scenario_spatial_components_min_size_zero()
        case ("spatial_components_nvert_negative")
            call scenario_spatial_components_nvert_negative()
        case ("spatial_rebuild_advice_normal")
            call scenario_spatial_rebuild_advice(level="normal")
        case ("spatial_rebuild_advice_silent")
            call scenario_spatial_rebuild_advice(level="silent")
        case ("spatial_sky_query_before_build")
            call scenario_spatial_sky_query_before_build()
        case ("spatial_sky_radius_nan")
            call scenario_spatial_sky_radius_nan()
        case ("spatial_sky_inner_radius_nan")
            call scenario_spatial_sky_inner_radius_nan()
        case ("spatial_sky_radii_vector_nan")
            call scenario_spatial_sky_radii_vector_nan()
        case ("spatial_nearest_sky_before_build")
            call scenario_spatial_nearest_sky_before_build()
        case ("spatial_query_radius_nan")
            call scenario_spatial_query_radius_nan()
        case ("spatial_query_radius_half_box")
            call scenario_spatial_query_radius_half_box()
        case ("spatial_build_sky_length_mismatch")
            call scenario_spatial_build_sky_length_mismatch()
        case ("spatial_build_sky_no_radius")
            call scenario_spatial_build_sky_no_radius()
        case ("spatial_build_sky_radius_not_positive")
            call scenario_spatial_build_sky_radius_not_positive()
        case ("spatial_rebuild_before_build")
            call scenario_spatial_rebuild_before_build()
        case ("spatial_rebuild_z_rank")
            call scenario_spatial_rebuild_z_rank()
        case ("spatial_rebuild_length_mismatch")
            call scenario_spatial_rebuild_length_mismatch()
        case ("spatial_rebuild_at_observer")
            call scenario_spatial_rebuild_at_observer()
        case ("spatial_rebuild_for_before_build")
            call scenario_spatial_rebuild_for_before_build()
        case ("spatial_rebuild_for_half_box")
            call scenario_spatial_rebuild_for_half_box()
        case ("spatial_build_box_rank")
            call scenario_spatial_build_box_rank()
        case ("spatial_build_box_not_strict")
            call scenario_spatial_build_box_not_strict()
        case ("spatial_copy_false_z_strided")
            call scenario_spatial_copy_false_z_strided()
        case ("spatial_bulk_before_build")
            call scenario_spatial_bulk_before_build()
        case ("spatial_los_before_build")
            call scenario_spatial_los_before_build()
        case ("spatial_los_bperp_negative")
            call scenario_spatial_los_bperp_negative()
        case ("spatial_kth_before_build")
            call scenario_spatial_kth_before_build()
        case ("spatial_components_i32_length")
            call scenario_spatial_components_i32_length()
        case ("spatial_components_nvert_int32")
            call scenario_spatial_components_nvert_int32()
        case ("spatial_within_int32_ceiling")
            call scenario_spatial_within_int32_ceiling()
        case ("spatial_axis_int32_ceiling")
            call scenario_spatial_axis_int32_ceiling()
        case ("spatial_nearest_int32_ceiling")
            call scenario_spatial_nearest_int32_ceiling()
        case ("spatial_grid_int32_ceiling")
            call scenario_spatial_grid_int32_ceiling()
        case ("spatial_debug_work_no_radius")
            call scenario_spatial_debug_work_no_radius()
        case ("spatial_nside_coarsened_warns")
            call scenario_spatial_nside_coarsened_warns()
        case ("spatial_sky_rebuild_warns")
            call scenario_spatial_sky_rebuild_warns()
        case ("logging_unknown_layout_field")
            call scenario_logging_unknown_layout_field()
        case ("logging_second_console_sink")
            call scenario_logging_second_console_sink()
        case ("logging_too_many_sinks")
            call scenario_logging_too_many_sinks()
        case ("logging_sink_and_name_together")
            call scenario_logging_sink_and_name_together()
        case ("logging_rank_filter_without_rank")
            call scenario_logging_rank_filter_without_rank()
        case ("logging_pop_context_token_mismatch")
            call scenario_logging_pop_context_token_mismatch()
        case ("logging_unknown_level_name")
            call scenario_logging_unknown_level_name()
        case ("logging_fatal")
            call scenario_logging_fatal()
        case ("logging_fatal_omp")
            call scenario_logging_fatal_omp()
        case ("logging_write_to_closed_sink")
            call scenario_logging_write_to_closed_sink()
        case ("logging_path_too_long")
            call scenario_logging_path_too_long()
        case ("logging_file_cannot_open")
            call scenario_logging_file_cannot_open()
        case ("logging_implicit_console")
            call scenario_logging_implicit_console()
        case ("logging_unset_level_empty_name")
            call scenario_logging_unset_level_empty_name()
        case ("logging_pop_name_token_mismatch")
            call scenario_logging_pop_name_token_mismatch()
        case ("logging_template_too_long")
            call scenario_logging_template_too_long()
        case ("logging_template_unclosed_brace")
            call scenario_logging_template_unclosed_brace()
        case ("logging_template_too_many_ops")
            call scenario_logging_template_too_many_ops()
        case ("logging_add_console_bad_stream")
            call scenario_logging_add_console_bad_stream()
        case ("logging_add_unit_not_connected")
            call scenario_logging_add_unit_not_connected()
        case ("logging_add_unit_not_writable")
            call scenario_logging_add_unit_not_writable()
        case ("logging_add_unit_unformatted")
            call scenario_logging_add_unit_unformatted()
        case ("logging_set_level_name_too_long")
            call scenario_logging_set_level_name_too_long()
        case ("logging_too_many_name_rules")
            call scenario_logging_too_many_name_rules()
        case ("logging_unset_level_name_too_long")
            call scenario_logging_unset_level_name_too_long()
        case ("logging_set_color_bad_policy")
            call scenario_logging_set_color_bad_policy()
        case ("logging_bad_sink_id")
            call scenario_logging_bad_sink_id()
        case ("logging_set_name_too_long")
            call scenario_logging_set_name_too_long()
        case ("logging_set_rank_negative")
            call scenario_logging_set_rank_negative()
        case ("logging_thread_mode_bad")
            call scenario_logging_thread_mode_bad()
        case ("logging_thread_mode_slot_too_small")
            call scenario_logging_thread_mode_slot_too_small()
        case ("logging_set_context_too_long")
            call scenario_logging_set_context_too_long()
        case ("logging_push_name_empty")
            call scenario_logging_push_name_empty()
        case ("logging_push_name_too_deep")
            call scenario_logging_push_name_too_deep()
        case ("logging_push_name_too_long")
            call scenario_logging_push_name_too_long()
        case ("logging_env_bad_level")
            call scenario_logging_env_bad_level()
        case ("logging_implicit_print")
            call scenario_logging_implicit_print()
        case ("logging_push_name_with_dot")
            call scenario_logging_push_name_with_dot()
        case ("logging_composed_name_too_long")
            call scenario_logging_composed_name_too_long()
        case ("logging_control")
            call scenario_logging_control()
        case ("toml_wrong_type")
            call scenario_toml_wrong_type()
        case ("toml_missing_key")
            call scenario_toml_missing_key()
        case ("toml_int_overflow")
            call scenario_toml_int_overflow()
        case ("toml_array_length_short")
            call scenario_toml_array_length(2)
        case ("toml_array_length_long")
            call scenario_toml_array_length(4)
        case ("toml_string_too_long")
            call scenario_toml_string_too_long()
        case ("toml_array_required")
            call scenario_toml_array_required()
        case ("toml_missing_section")
            call scenario_toml_missing_section()
        case ("toml_section_not_array")
            call scenario_toml_section_not_array()
        case ("toml_entry_out_of_range")
            call scenario_toml_entry_out_of_range()
        case ("toml_retired_key")
            call scenario_toml_retired_key()
        case ("toml_unknown_key")
            call scenario_toml_unknown_key()
        case ("toml_unknown_section")
            call scenario_toml_unknown_section()
        case ("toml_bad_level")
            call scenario_toml_bad_level()
        case ("toml_closed_handle")
            call scenario_toml_closed_handle()
        case ("toml_default_size")
            call scenario_toml_default_size()
        case ("toml_strings_count_short")
            call scenario_toml_strings_count_short()
        case ("toml_strings_count_long")
            call scenario_toml_strings_count_long()
        case ("toml_dump_not_owner")
            call scenario_toml_dump_not_owner()
        case ("toml_set_existing")
            call scenario_toml_set_existing()
        case ("toml_update_missing")
            call scenario_toml_update_missing()
        case ("toml_close_not_owner")
            call scenario_toml_close_not_owner()
        case ("toml_parse_error")
            call scenario_toml_parse_error()
        case ("toml_open_error")
            call scenario_toml_open_error()
        case ("toml_strings_range")
            call scenario_toml_strings_range()
        case ("toml_key_too_long")
            call scenario_toml_key_too_long()
        case ("toml_value_not_list")
            call scenario_toml_value_not_list()
        case ("toml_name_not_a_section")
            call scenario_toml_name_not_a_section()
        case ("toml_path_too_long")
            call scenario_toml_path_too_long()
        case ("toml_report_fatal")
            call scenario_toml_report_fatal()
        case ("toml_bad_severity")
            call scenario_toml_bad_severity()
        case ("toml_list_not_whole_numbers")
            call scenario_toml_list_not_whole_numbers()
        case ("toml_list_not_numbers")
            call scenario_toml_list_not_numbers()
        case ("toml_list_not_logicals")
            call scenario_toml_list_not_logicals()
        case ("toml_list_not_strings")
            call scenario_toml_list_not_strings()
        case ("toml_require_missing")
            call scenario_toml_require_missing()
        case ("toml_handle_never_opened")
            call scenario_toml_handle_never_opened()
        case ("toml_save_not_owner")
            call scenario_toml_save_not_owner()
        case ("toml_save_write_error")
            call scenario_toml_save_write_error()
        case ("toml_default_string_too_long")
            call scenario_toml_default_string_too_long()
        case ("toml_no_entries_at_all")
            call scenario_toml_no_entries_at_all()
        case ("toml_control")
            call scenario_toml_control()
        case default
            handled = .false.
        end select
    end subroutine dispatch_error_scenarios_analysis

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

    !> `ok=` reports an empty POPULATION; it never excuses a bad ARGUMENT. Passing it alongside a
    !! quantile outside 0-1 must therefore still abort, and must abort even when the population is
    !! empty too -- so a caller that has adopted `ok` still hears about its own bug rather than
    !! being told, misleadingly, that everything was null. That ordering is the whole reason the
    !! decision lives in `quantile_rank`, after the range check, rather than in the callers.
    subroutine scenario_sorting_quantile_ok_still_checks_range()
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        logical :: none(3) = [.false., .false., .false.]
        real(real64) :: q
        logical :: ok
        ! Negative control: with `ok` present and the quantile in range, an all-null population
        ! RETURNS. Without this line a range check that fired unconditionally would pass.
        call pf_nth_quantile(v, 0.5_real64, q, is_valid=none, ok=ok)
        print '(a,l1)', "an all-null population with ok= present returned, ok=", ok
        call pf_nth_quantile(v, 50.0_real64, q, is_valid=none, ok=ok)   ! -> aborts anyway
        print '(a,f0.3)', "unexpectedly accepted a 0-100 quantile because ok= was present, q=", q
    end subroutine scenario_sorting_quantile_ok_still_checks_range

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

    !> A mask of the wrong length is a MISUSE, not a data condition: the caller believes it is
    !! describing this array and is describing a different one, so every count that follows is
    !! silently wrong. parquet_stats returns NaN for a data condition and aborts only for this class.
    subroutine scenario_stats_is_valid_length_mismatch()
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        integer(int64) :: n
        call pf_count_valid(v, n, is_valid=[.true., .false.])   ! -> aborts (2 vs 4)
        print '(a,i0)', "unexpectedly accepted a short is_valid, n=", n
    end subroutine scenario_stats_is_valid_length_mismatch

    !> The same rule for the weight array, checked separately: a guard written for one of the two
    !! optional arrays and not the other passes every test written for the one it covers.
    subroutine scenario_stats_weights_length_mismatch()
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        integer(int64) :: n
        call pf_count_valid(v, n, weights=[1.0_real64, 1.0_real64, 1.0_real64])   ! -> aborts (3 vs 4)
        print '(a,i0)', "unexpectedly accepted a short weights array, n=", n
    end subroutine scenario_stats_weights_length_mismatch

    !> A negative weight cannot be meant. Zero is legal and drops the element; anything below it is
    !! a broken weight computation, and failing at the weight names the column that produced it
    !! rather than leaving a wrong mean to be noticed three steps downstream.
    subroutine scenario_stats_negative_weight()
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        integer(int64) :: n
        call pf_count_valid(v, n, weights=[1.0_real64, -1.0_real64, 1.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative weight, n=", n
    end subroutine scenario_stats_negative_weight

    !> A NaN weight aborts -- and the message must name the INDEX, because the whole value of
    !! failing here rather than downstream is that it points at the row that produced it.
    subroutine scenario_stats_nan_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: w(3)
        integer(int64) :: n
        w = [1.0_real64, 1.0_real64, 1.0_real64]
        w(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_count_valid(v, n, weights=w)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN weight, n=", n
    end subroutine scenario_stats_nan_weight

    !> An infinite weight aborts too. It is a separate scenario from the NaN one because the two
    !! are separate statements in the guard -- Fortran does not short-circuit, so they cannot be
    !! one test, and a guard covering only NaN would pass the NaN scenario while letting an
    !! infinity through to make every weighted answer NaN.
    subroutine scenario_stats_infinite_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: w(3)
        integer(int64) :: n
        w = [1.0_real64, 1.0_real64, 1.0_real64]
        w(2) = ieee_value(1.0_real64, ieee_positive_inf)
        call pf_count_valid(v, n, weights=w)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an infinite weight, n=", n
    end subroutine scenario_stats_infinite_weight

    !> An unrecognised `weight_type` token aborts, listing both accepted spellings.
    !!
    !! This is a misuse rather than a data condition, and the reason it cannot be waved through
    !! with a default is that the two conventions give DIFFERENT variances for the same weights --
    !! silently picking one would hand a caller who asked for the other a plausible wrong number.
    !! The message caps the echoed token, because ifx's ERROR STOP runtime corrupts the heap once
    !! the composed message reaches 8192 bytes and the caller controls this string's length.
    subroutine scenario_stats_unknown_weight_type()
        real(real64) :: v(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: w(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: var
        call pf_variance(v, var, weights=w, weight_type="inverse-variance")   ! -> aborts
        print '(a,es12.5)', "unexpectedly accepted an unknown weight_type, var=", var
    end subroutine scenario_stats_unknown_weight_type

    !> Reading a statistic off an accumulator that holds nothing is MISUSE, not a data condition.
    !!
    !! The distinction this scenario pins is the one a reader is most likely to get wrong: an
    !! accumulator armed by `%init` holds an EMPTY population and answers NaN, because a per-group
    !! loop meets an empty group on real data. A default-initialised one holds no population at
    !! all, and answering NaN there would silently turn a forgotten `%compute` into a plausible
    !! result. The `%init` call below is the negative control -- it proves the guard is keyed on
    !! the object having been armed and not simply on the population being empty.
    subroutine scenario_stats_object_query_before_compute()
        type(pf_stats) :: armed, fresh
        real(real64) :: m

        call armed%init()
        m = armed%mean()               ! an EMPTY population: NaN, no abort
        if (m == m) print '(a)', "the mean of an empty population was expected to be NaN"
        m = fresh%mean()               ! -> aborts (never computed, never armed)
        print '(a,es12.5)', "unexpectedly read a statistic off an uncomputed pf_stats, mean=", m
    end subroutine scenario_stats_object_query_before_compute

    !> Merging a streaming accumulator into a retained one would corrupt the retained population.
    !!
    !! The destination would go on believing its retained values describe its whole population
    !! while they described only the part that came from retained sources -- so the moments would
    !! be right and every order statistic of a later phase silently wrong. That is precisely the
    !! class this library refuses rather than documents.
    subroutine scenario_stats_object_merge_retain_mismatch()
        type(pf_stats) :: keeper, streamer

        call keeper%compute([1.0_real64, 2.0_real64, 3.0_real64])
        call streamer%init(retain=.false.)
        call streamer%update([4.0_real64, 5.0_real64])
        call keeper%merge(streamer)    ! -> aborts (retain disagrees)
        print '(a,i0)', "unexpectedly merged across a retain mismatch, n=", keeper%n()
    end subroutine scenario_stats_object_merge_retain_mismatch

    !> The second of `%merge`'s three policy guards: the weight convention.
    !!
    !! Both accumulators hold perfectly good populations and the fold itself would succeed --
    !! `weight_type` does not enter the Chan/Pebay combination at all. It enters every QUERY made
    !! afterwards, because it decides which count `ddof` is charged against, so a merged object
    !! would answer with one convention over a population assembled under two.
    subroutine scenario_stats_object_merge_weight_type_mismatch()
        type(pf_stats) :: rel, freq

        call rel%init()
        call freq%init(weight_type="frequency")
        call rel%update([1.0_real64, 2.0_real64], weights=[1.0_real64, 2.0_real64])
        call freq%update([3.0_real64, 4.0_real64], weights=[1.0_real64, 2.0_real64])
        call rel%merge(freq)           ! -> aborts (weight_type disagrees)
        print '(a,es12.5)', "unexpectedly merged across a weight_type mismatch, var=", rel%variance()
    end subroutine scenario_stats_object_merge_weight_type_mismatch

    !> The third policy guard, and the one that used to be missing.
    !!
    !! `skipnan` decides what the POPULATION IS: one accumulator dropped its NaNs and counted them
    !! in `%n_nan()`, the other kept them and is poisoned by construction. Folding the first into
    !! the second produces a number describing neither convention, with every count still
    !! plausible -- which is why the guard is worth as much as the other two.
    subroutine scenario_stats_object_merge_skipnan_mismatch()
        type(pf_stats) :: skipper, keeper

        call skipper%init()
        call keeper%init(skipnan=.false.)
        call skipper%update([1.0_real64, 2.0_real64])
        call keeper%update([3.0_real64, 4.0_real64])
        call skipper%merge(keeper)     ! -> aborts (skipnan disagrees)
        print '(a,es12.5)', "unexpectedly merged across a skipnan mismatch, mean=", skipper%mean()
    end subroutine scenario_stats_object_merge_skipnan_mismatch

    !> `%gmean` needs the retained values, which a streaming accumulator does not have.
    !!
    !! A log-sum is a fifth quantity the four central moments do not contain, so this is not a
    !! statistic that could be answered approximately from the accumulator -- it cannot be
    !! answered at all. The message says so and names the fix.
    subroutine scenario_stats_object_gmean_without_retain()
        type(pf_stats) :: streamer

        call streamer%init(retain=.false.)
        call streamer%update([1.0_real64, 2.0_real64, 8.0_real64])
        print '(a,es12.5)', "unexpectedly read a geometric mean off a streaming pf_stats, g=", &
            streamer%gmean()           ! -> aborts (retain=.false.)
    end subroutine scenario_stats_object_gmean_without_retain

    !> `%hmean`'s half of the same guard, so neither binding can lose it alone.
    subroutine scenario_stats_object_hmean_without_retain()
        type(pf_stats) :: streamer

        call streamer%init(retain=.false.)
        call streamer%update([1.0_real64, 2.0_real64, 8.0_real64])
        print '(a,es12.5)', "unexpectedly read a harmonic mean off a streaming pf_stats, h=", &
            streamer%hmean()           ! -> aborts (retain=.false.)
    end subroutine scenario_stats_object_hmean_without_retain

    !> A source that was never computed contributes nothing and almost certainly means a bug.
    !!
    !! An EMPTY source is a no-op and must stay one -- a threaded fold over slots a short loop
    !! never filled is ordinary -- so the guard is on the source never having been armed, and the
    !! armed-but-empty merge below is the negative control for that distinction.
    subroutine scenario_stats_object_merge_uncomputed_source()
        type(pf_stats) :: total, empty, never

        call total%compute([1.0_real64, 2.0_real64, 3.0_real64])
        call empty%init()
        call total%merge(empty)        ! an armed but empty source: a no-op
        if (total%n() /= 3_int64) print '(a)', "merging an empty source was expected to change nothing"
        call total%merge(never)        ! -> aborts (never computed, never armed)
        print '(a,i0)', "unexpectedly merged an uncomputed source, n=", total%n()
    end subroutine scenario_stats_object_merge_uncomputed_source

    !> A string column has no numeric statistics, and the message must name the kind it found.
    !!
    !! The numeric column first is the negative control: without it, a dispatch that refused every
    !! column would pass this scenario while making the whole `parquet_column` entry layer unusable.
    subroutine scenario_stats_column_string_kind()
        type(parquet_column) :: num, txt
        real(real64) :: m

        call num%init(PK_INT32, 3_int64)
        call num%set_all([1_int32, 2_int32, 3_int32])
        call pf_mean(num, m)                    ! a numeric column: accepted
        if (m /= 2.0_real64) print '(a)', "the mean of a numeric column was expected to be 2"
        call txt%init(PK_STRING, 3_int64)
        call pf_mean(txt, m)                    ! -> aborts (no numeric statistics for a string)
        print '(a,es12.5)', "unexpectedly averaged a string column, mean=", m
    end subroutine scenario_stats_column_string_kind

    !> Flattening a vector column into one population is a DIFFERENT statistic, so it is refused.
    !!
    !! A caller who wants one element position across all rows has `%get_elem`; one who genuinely
    !! wants the flattened population can pass the flattened array and thereby say so. Answering
    !! silently would give a plausible number for a question nobody asked.
    subroutine scenario_stats_column_vector_width()
        type(parquet_column) :: c
        real(real64) :: m

        call c%init(PK_INT32_VEC, 3_int64, width=2)
        call pf_mean(c, m)                      ! -> aborts (width 2)
        print '(a,es12.5)', "unexpectedly averaged a vector column, mean=", m
    end subroutine scenario_stats_column_vector_width

    !> A column carries its own validity, so a second source of nullness beside it is refused.
    !!
    !! Two sources that can disagree is a shape this repository has been bitten by before; the
    !! null-free call first is the negative control for the guard being keyed on PRESENCE.
    subroutine scenario_stats_column_is_valid_conflict()
        type(parquet_column) :: c
        logical :: mask(3) = [.true., .false., .true.]
        real(real64) :: m

        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call pf_mean(c, m)                      ! no is_valid=: accepted
        if (m /= 2.0_real64) print '(a)', "the mean of a numeric column was expected to be 2"
        call pf_mean(c, m, is_valid=mask)       ! -> aborts (two sources of nullness)
        print '(a,es12.5)', "unexpectedly accepted is_valid= beside a column, mean=", m
    end subroutine scenario_stats_column_is_valid_conflict

    !> A probability outside [0, 1] is misuse, not a data condition, so it aborts.
    subroutine scenario_stats_quantile_bad_probability()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: q

        call pf_quantile(x, 0.5_real64, q)          ! in range: accepted
        if (q /= 2.5_real64) print '(a)', "the median of 1..4 was expected to be 2.5"
        call pf_quantile(x, 1.5_real64, q)          ! -> aborts (probability out of range)
        print '(a,es12.5)', "unexpectedly accepted p=1.5, q=", q
    end subroutine scenario_stats_quantile_bad_probability

    !> An unrecognised method token aborts listing all six, rather than silently picking one.
    subroutine scenario_stats_quantile_bad_method()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: q

        call pf_quantile(x, 0.5_real64, q, method="linear")   ! a real token: accepted
        if (q /= 2.5_real64) print '(a)', "method=linear was expected to give 2.5"
        call pf_quantile(x, 0.5_real64, q, method="type7")    ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted method=type7, q=", q
    end subroutine scenario_stats_quantile_bad_method

    !> `out` and `probs` must be the same size; a mismatch is a caller error.
    subroutine scenario_stats_quantiles_size_mismatch()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: two(2), three(3)

        call pf_quantiles(x, [0.25_real64, 0.75_real64], two)   ! matched: accepted
        if (two(1) >= two(2)) print '(a)', "the quartiles came back out of order"
        call pf_quantiles(x, [0.25_real64, 0.75_real64], three) ! -> aborts (size mismatch)
        print '(a,es12.5)', "unexpectedly accepted a size mismatch, first=", three(1)
    end subroutine scenario_stats_quantiles_size_mismatch

    !> `prop` outside [0, 0.5) would trim everything away, which is a mistake rather than a request.
    subroutine scenario_stats_trim_mean_bad_prop()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: m

        call pf_trim_mean(x, 0.2_real64, m)   ! in range: accepted
        if (m /= 2.5_real64) print '(a)', "trimming 0 from each end of 1..4 was expected to give 2.5"
        call pf_trim_mean(x, 0.5_real64, m)   ! -> aborts (prop must be < 0.5)
        print '(a,es12.5)', "unexpectedly accepted prop=0.5, m=", m
    end subroutine scenario_stats_trim_mean_bad_prop

    !> A NaN score can only come from the caller's own arithmetic, unlike a NaN in the population.
    subroutine scenario_stats_score_not_finite()
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: p

        call pf_percentile_of_score(x, 2.0_real64, p)   ! finite: accepted
        if (p < 0.0_real64 .or. p > 1.0_real64) print '(a)', "a percentile of score left [0, 1]"
        call pf_percentile_of_score(x, ieee_value(1.0_real64, ieee_quiet_nan), p)  ! -> aborts
        print '(a,es12.5)', "unexpectedly accepted a NaN score, p=", p
    end subroutine scenario_stats_score_not_finite

    !> An unrecognised `kind` token aborts listing all four.
    subroutine scenario_stats_score_bad_kind()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: p

        call pf_percentile_of_score(x, 2.0_real64, p, kind="weak")   ! a real token: accepted
        if (p <= 0.0_real64) print '(a)', "kind=weak was expected to count at least one value"
        call pf_percentile_of_score(x, 2.0_real64, p, kind="middle") ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted kind=middle, p=", p
    end subroutine scenario_stats_score_bad_kind

    !> A streaming accumulator kept no values, so it has nothing to order.
    subroutine scenario_stats_median_on_streaming()
        type(pf_stats) :: s, r
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        real(real64) :: m

        ! A RETAINED accumulator answers, which is the negative control: the abort below is about
        ! retain=.false., not about %median being broken.
        call r%compute(x)
        m = r%median()
        if (m /= 2.5_real64) print '(a)', "a retained median of 1..4 was expected to be 2.5"

        call s%init(retain=.false.)
        call s%update(x)
        m = s%median()                         ! -> aborts (nothing was retained to order)
        print '(a,es12.5)', "unexpectedly took a median of a streaming accumulator, m=", m
    end subroutine scenario_stats_median_on_streaming

    !> An unrecognised scale token aborts naming both, rather than silently leaving it unscaled.
    subroutine scenario_stats_mad_bad_scale()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 8.0_real64]
        real(real64) :: m

        call pf_mad(x, m, scale="raw")        ! a real token: accepted
        if (m /= 1.0_real64) print '(a)', "the raw MAD of 1,2,3,8 was expected to be 1"
        call pf_mad(x, m, scale="mad_std")    ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted scale=mad_std, m=", m
    end subroutine scenario_stats_mad_bad_scale

    !> A NaN centre can only come from the caller's own arithmetic, unlike a NaN in the population.
    subroutine scenario_stats_mad_center_not_finite()
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 8.0_real64]
        real(real64) :: m

        call pf_mad(x, m, scale="raw", center=2.0_real64)   ! finite: accepted
        if (m /= 1.0_real64) print '(a)', "the raw MAD about 2 was expected to be 1"
        call pf_mad(x, m, center=ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,es12.5)', "unexpectedly accepted a NaN centre, m=", m
    end subroutine scenario_stats_mad_center_not_finite

    !> A string column carries its own validity, so `is_valid=` beside one is two sources of truth.
    subroutine scenario_stats_mode_string_column_is_valid()
        type(parquet_string_column) :: col
        character(len=:), allocatable :: m
        logical :: mask(3)

        call col%append_string("b")
        call col%append_string("a")
        call col%append_string("a")
        call pf_mode(col, m)                          ! without is_valid=: accepted
        if (m /= "a") print '(a)', "the mode of b,a,a was expected to be a"
        mask = [.true., .true., .false.]
        call pf_mode(col, m, is_valid=mask)           ! -> aborts (two sources of nullness)
        print '(a,a)', "unexpectedly accepted is_valid= beside a string column, m=", m
    end subroutine scenario_stats_mode_string_column_is_valid

    !> An unrecognised correlation method aborts naming both, rather than silently picking one.
    subroutine scenario_stats_corr_bad_method()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: y(4) = [2.0_real64, 1.0_real64, 4.0_real64, 3.0_real64]
        real(real64) :: r

        call pf_corr(x, y, r, method="spearman")   ! a real token: accepted
        if (r < -1.0_real64 .or. r > 1.0_real64) print '(a)', "a correlation left [-1, 1]"
        call pf_corr(x, y, r, method="kendall")    ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted method=kendall, r=", r
    end subroutine scenario_stats_corr_bad_method

    !> An unrecognised plotting-position token aborts, naming all six.
    !!
    !! The token is resolved BEFORE the population is looked at, so this aborts on any input at
    !! all rather than only on one that reaches the branch reading it.
    subroutine scenario_stats_normal_scores_bad_method()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: s(4)

        call pf_normal_scores(x, s, method="hazen")     ! a real token: accepted
        if (s(1) >= s(2)) print '(a)', "the scores left their order"
        call pf_normal_scores(x, s, method="rankit")    ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted method=rankit, s(1)=", s(1)
    end subroutine scenario_stats_normal_scores_bad_method

    !> `pf_probit_fit` reaches the SAME plotting-position resolver `pf_normal_scores` does, from a
    !! different submodule. That edge is the point of this scenario: the resolver is a separate
    !! module procedure precisely so the two cannot come to disagree about the six tokens, and a
    !! copy made in `parquet_stats_order` would pass every in-process test while accepting -- or
    !! refusing -- a different set here.
    subroutine scenario_stats_probit_fit_bad_method()
        real(real64) :: x(6) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64, 8.0_real64, &
            13.0_real64]
        real(real64) :: loc, sigma

        call pf_probit_fit(x, loc, sigma, method="cunnane")   ! a real token: accepted
        if (sigma <= 0.0_real64) print '(a)', "the fitted slope is not positive"
        call pf_probit_fit(x, loc, sigma, method="rankit")    ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted method=rankit, sigma=", sigma
    end subroutine scenario_stats_probit_fit_bad_method

    !> `prob = 0.5` asks for a zero range over a zero divisor. The negative control is a legal
    !! `prob` first, so a `pf_probit_scale` that refused every value would not pass this.
    subroutine scenario_stats_probit_scale_bad_prob()
        real(real64) :: x(6) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64, 8.0_real64, &
            13.0_real64]
        real(real64) :: sigma

        call pf_probit_scale(x, sigma, prob=0.1_real64)   ! inside (0, 0.5): accepted
        if (sigma <= 0.0_real64) print '(a)', "the implied scale is not positive"
        call pf_probit_scale(x, sigma, prob=0.5_real64)   ! -> aborts (0/0)
        print '(a,es12.5)', "unexpectedly accepted prob=0.5, sigma=", sigma
    end subroutine scenario_stats_probit_scale_bad_prob

    !> A negative weight is the family's standing misuse, and `pf_probit_mean` must inherit the
    !! refusal rather than reimplement the weight rules.
    subroutine scenario_stats_probit_mean_bad_weight()
        real(real64) :: p(3) = [0.2_real64, 0.5_real64, 0.8_real64]
        real(real64) :: good(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: bad(3) = [1.0_real64, -2.0_real64, 3.0_real64]
        real(real64) :: m

        call pf_probit_mean(p, m, weights=good)      ! non-negative: accepted
        if (m <= 0.0_real64 .or. m >= 1.0_real64) print '(a)', "the probit mean left [0, 1]"
        call pf_probit_mean(p, m, weights=bad)       ! -> aborts (negative weight)
        print '(a,es12.5)', "unexpectedly accepted a negative weight, m=", m
    end subroutine scenario_stats_probit_mean_bad_weight

    !> `%probit_mean` needs the retained values for `%gmean`'s reason: a probit-sum is a fifth
    !! accumulated quantity the streaming accumulator does not carry.
    subroutine scenario_stats_object_probit_mean_without_retain()
        type(pf_stats) :: keeper, streamer

        call keeper%init(retain=.true.)
        call keeper%update([0.2_real64, 0.5_real64, 0.8_real64])
        if (keeper%probit_mean() /= keeper%probit_mean()) print '(a)', "the retaining form is NaN"
        call streamer%init(retain=.false.)
        call streamer%update([0.2_real64, 0.5_real64, 0.8_real64])
        print '(a,es12.5)', "unexpectedly read a probit mean off a streaming pf_stats, m=", &
            streamer%probit_mean()     ! -> aborts (retain=.false.)
    end subroutine scenario_stats_object_probit_mean_without_retain

    !> A weighted midrank is a definitional choice no reference library makes, so this refuses.
    subroutine scenario_stats_spearman_with_weights()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: y(4) = [2.0_real64, 1.0_real64, 4.0_real64, 3.0_real64]
        real(real64) :: w(4) = [1.0_real64, 2.0_real64, 1.0_real64, 2.0_real64]
        real(real64) :: r

        call pf_corr(x, y, r, weights=w)                        ! Pearson weighted: accepted
        if (r /= r) print '(a)', "a weighted Pearson correlation was expected to be a number"
        call pf_corr(x, y, r, weights=w, method="spearman")     ! -> aborts (weighted Spearman)
        print '(a,es12.5)', "unexpectedly accepted a weighted Spearman, r=", r
    end subroutine scenario_stats_spearman_with_weights

    !> Two samples of different lengths have no pairs, so there is nothing to correlate.
    subroutine scenario_stats_pair_size_mismatch()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: y(3) = [2.0_real64, 1.0_real64, 4.0_real64]
        real(real64) :: c

        call pf_cov(x, x, c)          ! matched: accepted
        if (c <= 0.0_real64) print '(a)', "the covariance of a sample with itself was not positive"
        call pf_cov(x, y, c)          ! -> aborts (size mismatch)
        print '(a,es12.5)', "unexpectedly accepted two samples of different size, c=", c
    end subroutine scenario_stats_pair_size_mismatch

    !> An unrecognised cenfunc token aborts naming both.
    subroutine scenario_stats_clip_bad_cenfunc()
        real(real64) :: x(6) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
            60.0_real64]
        real(real64) :: m, md, sd

        call pf_sigma_clipped_stats(x, m, md, sd, cenfunc="mean")   ! a real token: accepted
        if (m /= m) print '(a)', "cenfunc=mean was expected to give a number"
        call pf_sigma_clipped_stats(x, m, md, sd, cenfunc="mode")   ! -> aborts (unknown token)
        print '(a,es12.5)', "unexpectedly accepted cenfunc=mode, m=", m
    end subroutine scenario_stats_clip_bad_cenfunc

    !> A negative clip width can only be a caller mistake, so it aborts rather than clipping all.
    subroutine scenario_stats_clip_bad_sigma()
        real(real64) :: x(6) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
            60.0_real64]
        real(real64) :: m, md, sd

        call pf_sigma_clipped_stats(x, m, md, sd, sigma=2.0_real64)   ! positive: accepted
        if (m /= m) print '(a)', "sigma=2 was expected to give a number"
        call pf_sigma_clipped_stats(x, m, md, sd, sigma=-1.0_real64)  ! -> aborts (negative width)
        print '(a,es12.5)', "unexpectedly accepted sigma=-1, m=", m
    end subroutine scenario_stats_clip_bad_sigma

    !> `z` must be the same size as `values`; a mismatch is a caller error, not a data condition.
    subroutine scenario_stats_zscore_size_mismatch()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: four(4), three(3)

        call pf_zscore(x, four)     ! matched: accepted
        if (four(1) >= four(4)) print '(a)', "the z-scores came back out of order"
        call pf_zscore(x, three)    ! -> aborts (size mismatch)
        print '(a,es12.5)', "unexpectedly accepted a short output array, z=", three(1)
    end subroutine scenario_stats_zscore_size_mismatch

    !> A cumulative procedure writes one output per input, so a short `out` is a caller error.
    subroutine scenario_stats_cumsum_size_mismatch()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: four(4), three(3)

        call pf_cumsum(x, four)     ! matched: accepted
        if (four(4) /= 11.0_real64) print '(a)', "the running sum came back wrong"
        call pf_cumsum(x, three)    ! -> aborts (size mismatch)
        print '(a,es12.5)', "unexpectedly accepted a short output array, out=", three(1)
    end subroutine scenario_stats_cumsum_size_mismatch

    !> `out_valid` is checked separately from `out`: getting one right does not excuse the other,
    !! and a short mask would be written past its end rather than merely reported incompletely.
    subroutine scenario_stats_cum_out_valid_mismatch()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: four(4)
        logical :: mask4(4), mask2(2)

        call pf_cummax(x, four, out_valid=mask4)   ! matched: accepted
        if (.not. all(mask4)) print '(a)', "every element was usable and should be marked valid"
        call pf_cummax(x, four, out_valid=mask2)   ! -> aborts (out_valid size mismatch)
        print '(a,l1)', "unexpectedly accepted a short mask, first=", mask2(1)
    end subroutine scenario_stats_cum_out_valid_mismatch

    !> One edge describes no bin at all, so there is nothing the call could answer.
    subroutine scenario_stats_edges_too_few()
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: two(2) = [0.0_real64, 4.0_real64]
        real(real64) :: one(1) = [0.0_real64]
        integer(int32) :: codes(3)

        call pf_bucketize(x, two, codes)   ! two edges: one bin, accepted
        if (any(codes /= 1_int32)) print '(a)', "all three values should have landed in bin 1"
        call pf_bucketize(x, one, codes)   ! -> aborts (fewer than two edges)
        print '(a,i0)', "unexpectedly accepted a single edge, codes(1)=", codes(1)
    end subroutine scenario_stats_edges_too_few

    !> An equal adjacent pair describes a bin no value can reach, which has no useful reading.
    subroutine scenario_stats_edges_not_increasing()
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: good(4) = [0.0_real64, 1.5_real64, 2.5_real64, 4.0_real64]
        real(real64) :: flat(4) = [0.0_real64, 1.5_real64, 1.5_real64, 4.0_real64]
        real(real64) :: counts(3)

        call pf_histogram(x, good, counts)   ! strictly increasing: accepted
        if (sum(counts) /= 3.0_real64) print '(a)', "all three values should have been counted"
        call pf_histogram(x, flat, counts)   ! -> aborts (not strictly increasing)
        print '(a,es12.5)', "unexpectedly accepted a repeated edge, counts(1)=", counts(1)
    end subroutine scenario_stats_edges_not_increasing

    !> A NaN edge is reported as a NaN and not as "not increasing" -- it fails the ordering test
    !! too, since every comparison against it is false, but that message sends the reader to the
    !! wrong half of their edge array.
    subroutine scenario_stats_edges_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: good(3) = [0.0_real64, 2.0_real64, 4.0_real64]
        real(real64) :: bad(3)
        integer(int32) :: codes(3)

        call pf_bucketize(x, good, codes)   ! finite edges: accepted
        if (codes(3) /= 2_int32) print '(a)', "3.0 should have landed in the upper bin"
        bad = good
        bad(2) = ieee_value(0.0_real64, ieee_quiet_nan)
        call pf_bucketize(x, bad, codes)    ! -> aborts (NaN edge)
        print '(a,i0)', "unexpectedly accepted a NaN edge, codes(1)=", codes(1)
    end subroutine scenario_stats_edges_nan

    !> `counts` holds one entry per BIN, which is one fewer than the number of edges -- the
    !! off-by-one a caller sizing it from `size(edges)` makes, and worth its own message.
    subroutine scenario_stats_histogram_counts_size()
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: edges(4) = [0.0_real64, 1.5_real64, 2.5_real64, 4.0_real64]
        real(real64) :: three(3), four(4)

        call pf_histogram(x, edges, three)   ! four edges, three bins: accepted
        if (sum(three) /= 3.0_real64) print '(a)', "all three values should have been counted"
        call pf_histogram(x, edges, four)    ! -> aborts (one entry per bin, not per edge)
        print '(a,es12.5)', "unexpectedly accepted one count per edge, counts(1)=", four(1)
    end subroutine scenario_stats_histogram_counts_size

    !> Zero bins describe nothing, so there is no edge array `pf_bin_edges` could fill.
    subroutine scenario_stats_bin_edges_nbins()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: two(2), three(3)

        call pf_bin_edges(x, 1, two)    ! one bin: accepted
        if (two(2) <= two(1)) print '(a)', "one bin should still give increasing edges"
        call pf_bin_edges(x, 0, three)  ! -> aborts (nbins must be at least 1)
        print '(a,es12.5)', "unexpectedly accepted nbins=0, edges(1)=", three(1)
    end subroutine scenario_stats_bin_edges_nbins

    !> `edges` holds one boundary MORE than the bin count -- the off-by-one a caller sizing it
    !! from `nbins` makes, and the mirror of `pf_histogram`'s `counts` check.
    subroutine scenario_stats_bin_edges_size()
        real(real64) :: x(4) = [1.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
        real(real64) :: four(4), three(3)

        call pf_bin_edges(x, 3, four)   ! three bins, four edges: accepted
        if (four(4) /= 5.0_real64) print '(a)', "the top edge should be the population maximum"
        call pf_bin_edges(x, 3, three)  ! -> aborts (three bins need four boundaries)
        print '(a,es12.5)', "unexpectedly accepted one edge per bin, edges(1)=", three(1)
    end subroutine scenario_stats_bin_edges_size

    !> `codes` is one per ELEMENT, so a caller sizing it from the bin count instead of the
    !! population aborts. Same class as the `is_valid` and `weights` length checks: the caller
    !! believes it is describing this array and is describing a different one.
    subroutine scenario_stats_bucketize_codes_size()
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: edges(4) = [0.0_real64, 3.0_real64, 6.0_real64, 10.0_real64]
        integer(int32) :: five(5), three(3)

        call pf_bucketize(x, edges, five)   ! one code per element: accepted
        if (five(1) < 1) print '(a)', "the first element should land in a bin"
        call pf_bucketize(x, edges, three)  ! -> aborts (three codes for five elements)
        print '(a,i0)', "unexpectedly accepted one code per bin, codes(1)=", three(1)
    end subroutine scenario_stats_bucketize_codes_size

    !> One grid point describes no cell, so there is nothing a value could be split across.
    subroutine scenario_stats_bin_linear_grid_too_short()
        real(real64) :: x(3) = [0.5_real64, 2.0_real64, 3.5_real64]
        real(real64) :: two(2) = [0.0_real64, 4.0_real64]
        real(real64) :: one(1) = [0.0_real64]
        real(real64) :: mass2(2), mass1(1)

        call pf_bin_linear(x, two, mass2)   ! two points: one cell, accepted
        if (sum(mass2) /= 3.0_real64) print '(a)', "all three values should have been deposited"
        call pf_bin_linear(x, one, mass1)   ! -> aborts (fewer than two points)
        print '(a,es12.5)', "unexpectedly accepted a single grid point, mass(1)=", mass1(1)
    end subroutine scenario_stats_bin_linear_grid_too_short

    !> A repeated grid point describes a cell of zero width, across which no value can be split.
    subroutine scenario_stats_bin_linear_grid_not_increasing()
        real(real64) :: x(3) = [0.75_real64, 2.0_real64, 3.25_real64]
        real(real64) :: good(4) = [0.0_real64, 1.5_real64, 2.5_real64, 4.0_real64]
        real(real64) :: flat(4) = [0.0_real64, 1.5_real64, 1.5_real64, 4.0_real64]
        real(real64) :: mass(4)

        call pf_bin_linear(x, good, mass)   ! strictly increasing: accepted
        if (sum(mass) /= 3.0_real64) print '(a)', "all three values should have been deposited"
        call pf_bin_linear(x, flat, mass)   ! -> aborts (a repeated point)
        print '(a,es12.5)', "unexpectedly accepted a repeated grid point, mass(1)=", mass(1)
    end subroutine scenario_stats_bin_linear_grid_not_increasing

    !> A NaN grid point is reported as a NaN, not as "not increasing", which it also is.
    subroutine scenario_stats_bin_linear_nan_grid_point()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: good(3) = [0.0_real64, 2.0_real64, 4.0_real64]
        real(real64) :: bad(3), mass(3)

        call pf_bin_linear(x, good, mass)   ! finite points: accepted
        if (mass(2) /= 2.0_real64) print '(a)', "the middle point should hold two of the three values"
        bad = good
        bad(2) = ieee_value(0.0_real64, ieee_quiet_nan)
        call pf_bin_linear(x, bad, mass)    ! -> aborts (a NaN point)
        print '(a,es12.5)', "unexpectedly accepted a NaN grid point, mass(1)=", mass(1)
    end subroutine scenario_stats_bin_linear_nan_grid_point

    !> An infinite grid point is refused, although `pf_histogram` accepts the same array as edges:
    !! an open-ended bin means something, a share of the distance to an infinite point does not.
    subroutine scenario_stats_bin_linear_infinite_grid_point()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        real(real64) :: x(3) = [1.0_real64, 2.0_real64, 3.0_real64]
        real(real64) :: good(3) = [0.0_real64, 2.0_real64, 4.0_real64]
        real(real64) :: openended(3), mass(3), counts(2)

        openended = good
        openended(3) = ieee_value(0.0_real64, ieee_positive_inf)
        call pf_histogram(x, openended, counts)  ! an infinite outer EDGE: accepted there
        if (sum(counts) /= 3.0_real64) print '(a)', "the open-ended histogram should count all three"
        call pf_bin_linear(x, good, mass)        ! finite points: accepted
        if (mass(2) /= 2.0_real64) print '(a)', "the middle point should hold two of the three values"
        call pf_bin_linear(x, openended, mass)   ! -> aborts (an infinite point)
        print '(a,es12.5)', "unexpectedly accepted an infinite grid point, mass(1)=", mass(1)
    end subroutine scenario_stats_bin_linear_infinite_grid_point

    !> Two finite neighbours whose difference overflows are refused: `[-1e308, 1e308]` passes
    !! every other grid test, and would still turn each split between them into 0 or NaN.
    subroutine scenario_stats_bin_linear_spacing_overflows()
        real(real64) :: x(3) = [-1.0e307_real64, 0.0_real64, 1.0e307_real64]
        real(real64) :: wide(3) = [-1.0e308_real64, 0.0_real64, 1.0e308_real64]
        real(real64) :: gap(2) = [-1.0e308_real64, 1.0e308_real64]
        real(real64) :: mass3(3), mass2(2)

        call pf_bin_linear(x, wide, mass3)            ! spacings of 1e308: accepted
        if (.not. (mass3(2) > 2.0_real64)) print '(a)', "the middle point should hold most of the weight"
        call pf_bin_linear([0.0_real64], gap, mass2)  ! -> aborts (a spacing of 2e308)
        print '(a,es12.5)', "unexpectedly accepted an overflowing spacing, mass(1)=", mass2(1)
    end subroutine scenario_stats_bin_linear_spacing_overflows

    !> `mass` holds one entry per grid POINT. Sized from the cells, as `pf_histogram`'s `counts`
    !! is sized from the bins, it is one entry short -- the off-by-one this contract invites.
    subroutine scenario_stats_bin_linear_mass_size()
        real(real64) :: x(3) = [0.75_real64, 2.0_real64, 3.25_real64]
        real(real64) :: grid(4) = [0.0_real64, 1.5_real64, 2.5_real64, 4.0_real64]
        real(real64) :: four(4), three(3)

        call pf_bin_linear(x, grid, four)    ! four points, four entries: accepted
        if (sum(four) /= 3.0_real64) print '(a)', "all three values should have been deposited"
        call pf_bin_linear(x, grid, three)   ! -> aborts (one entry per cell, not per point)
        print '(a,es12.5)', "unexpectedly accepted one mass entry per cell, mass(1)=", three(1)
    end subroutine scenario_stats_bin_linear_mass_size

    !> `pf_bin_linear` validates its weights through the module's shared validator.
    subroutine scenario_stats_bin_linear_negative_weight()
        real(real64) :: x(3) = [0.5_real64, 1.0_real64, 1.5_real64]
        real(real64) :: grid(3) = [0.0_real64, 1.0_real64, 2.0_real64]
        real(real64) :: w(3) = [1.0_real64, 2.0_real64, 1.0_real64]
        real(real64) :: mass(3)

        call pf_bin_linear(x, grid, mass, weights=w)   ! non-negative weights: accepted
        if (mass(2) /= 3.0_real64) print '(a)', "the middle point should hold three units of weight"
        w(2) = -2.0_real64
        call pf_bin_linear(x, grid, mass, weights=w)   ! -> aborts (a negative weight)
        print '(a,es12.5)', "unexpectedly accepted a negative weight, mass(2)=", mass(2)
    end subroutine scenario_stats_bin_linear_negative_weight

    !> One weight per VALUE: a `weights` array of another length aborts before anything is read.
    subroutine scenario_stats_bin_linear_weights_size()
        real(real64) :: x(3) = [0.5_real64, 1.0_real64, 1.5_real64]
        real(real64) :: grid(3) = [0.0_real64, 1.0_real64, 2.0_real64]
        real(real64) :: w3(3) = [1.0_real64, 1.0_real64, 1.0_real64]
        real(real64) :: w2(2) = [1.0_real64, 1.0_real64]
        real(real64) :: mass(3)

        call pf_bin_linear(x, grid, mass, weights=w3)   ! one weight per value: accepted
        if (mass(2) /= 2.0_real64) print '(a)', "the middle point should hold two units of weight"
        call pf_bin_linear(x, grid, mass, weights=w2)   ! -> aborts (two weights for three values)
        print '(a,es12.5)', "unexpectedly accepted a short weights array, mass(2)=", mass(2)
    end subroutine scenario_stats_bin_linear_weights_size

    !> A logical column is refused by name. `pf_bin_linear` has no logical form, and its column
    !! form must not deposit the 0s and 1s that every other generic here widens one to -- which
    !! the `pf_histogram` call below shows it would otherwise do without complaint.
    subroutine scenario_stats_bin_linear_logical_column()
        type(parquet_column) :: numeric, flags
        real(real64) :: grid(3) = [0.0_real64, 1.0_real64, 2.0_real64]
        real(real64) :: mass(3), counts(2)

        call numeric%init(PK_FLOAT64, 3_int64)
        call numeric%set_all([0.5_real64, 1.0_real64, 1.5_real64])
        call pf_bin_linear(numeric, grid, mass)   ! a float64 column: accepted
        if (mass(2) /= 2.0_real64) print '(a)', "the middle point should hold two of the three values"
        call flags%init(PK_LOGICAL, 3_int64)
        call flags%set_all([.true., .false., .true.])
        call pf_histogram(flags, [-0.5_real64, 0.5_real64, 1.5_real64], counts)   ! accepted there
        if (counts(2) /= 2.0_real64) print '(a)', "the histogram should count the two .true. values"
        call pf_bin_linear(flags, grid, mass)     ! -> aborts (a logical column)
        print '(a,es12.5)', "unexpectedly accepted a logical column, mass(1)=", mass(1)
    end subroutine scenario_stats_bin_linear_logical_column

    !> `out_valid` is a per-element OUTPUT, so a short one would be written past its end or leave
    !! the caller reading exclusions that belong to other elements. Checked separately from `z`
    !! because they are separate guards, and one written for `z` alone passes every `z` test.
    subroutine scenario_stats_zscore_out_valid_size()
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: z(5)
        logical :: five(5), three(3)

        call pf_zscore(x, z, out_valid=five)    ! one flag per element: accepted
        if (.not. five(1)) print '(a)', "a finite element should be marked valid"
        call pf_zscore(x, z, out_valid=three)   ! -> aborts (three flags for five elements)
        print '(a,l1)', "unexpectedly accepted a short out_valid, out_valid(1)=", three(1)
    end subroutine scenario_stats_zscore_out_valid_size

    !> `pf_normal_scores` fills one score per element, so a short `s` is the same misuse.
    subroutine scenario_stats_normal_scores_size()
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: five(5), three(3)

        call pf_normal_scores(x, five)   ! one score per element: accepted
        if (five(1) > five(3)) print '(a)', "the smallest value should take the smallest score"
        call pf_normal_scores(x, three)  ! -> aborts (three scores for five elements)
        print '(a,es12.5)', "unexpectedly accepted a short s, s(1)=", three(1)
    end subroutine scenario_stats_normal_scores_size

    !> And its own `out_valid`, which is a third guard again rather than the second one reused.
    subroutine scenario_stats_normal_scores_out_valid_size()
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: s(5)
        logical :: five(5), three(3)

        call pf_normal_scores(x, s, out_valid=five)    ! accepted
        if (.not. five(1)) print '(a)', "a finite element should be marked valid"
        call pf_normal_scores(x, s, out_valid=three)   ! -> aborts
        print '(a,l1)', "unexpectedly accepted a short out_valid, out_valid(1)=", three(1)
    end subroutine scenario_stats_normal_scores_out_valid_size

    !> `keep` names the survivors of the clip ELEMENT BY ELEMENT against the original array, so a
    !! short one would report a different population's survivors -- and the whole point of the
    !! mask is to be indexed back into the caller's own data.
    subroutine scenario_stats_sigma_clip_keep_size()
        real(real64) :: x(6) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 99.0_real64]
        real(real64) :: m, med, sd
        logical :: six(6), three(3)

        call pf_sigma_clipped_stats(x, m, med, sd, keep=six)    ! one flag per element: accepted
        if (six(6)) print '(a)', "the outlier should have been clipped away"
        call pf_sigma_clipped_stats(x, m, med, sd, keep=three)  ! -> aborts
        print '(a,l1)', "unexpectedly accepted a short keep, keep(1)=", three(1)
    end subroutine scenario_stats_sigma_clip_keep_size

    !> An order statistic off an accumulator that holds NOTHING is a programming error rather than
    !! an empty population: `%compute` or `%init` has not run, so there is no population to be
    !! empty. Answering NaN would hide the missing call behind a number the caller might use.
    subroutine scenario_stats_obj_order_without_population()
        type(pf_stats) :: s
        real(real64) :: q
        q = s%quantile(0.5_real64)   ! -> aborts (never computed or initialised)
        print '(a,es12.5)', "unexpectedly read a quantile off an empty accumulator, q=", q
    end subroutine scenario_stats_obj_order_without_population

    !> `%quantiles` fills one output per probability, and the two arrays are the caller's -- so a
    !! mismatch is the same misuse the one-shot forms refuse.
    subroutine scenario_stats_obj_quantiles_out_size()
        type(pf_stats) :: s
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: three(3), two(2)

        call s%compute(x)
        call s%quantiles([0.1_real64, 0.5_real64, 0.9_real64], three)   ! accepted
        if (three(1) > three(3)) print '(a)', "the quantiles should be non-decreasing"
        call s%quantiles([0.1_real64, 0.5_real64, 0.9_real64], two)     ! -> aborts (2 vs 3)
        print '(a,es12.5)', "unexpectedly accepted a short out, out(1)=", two(1)
    end subroutine scenario_stats_obj_quantiles_out_size

    !> Trimming half or more from each tail leaves nothing to average, so `prop >= 0.5` aborts --
    !! the object binding's own copy of the check the one-shot `pf_trim_mean` makes.
    subroutine scenario_stats_obj_trim_mean_prop()
        type(pf_stats) :: s
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: m

        call s%compute(x)
        m = s%trim_mean(0.25_real64)   ! accepted
        if (m /= m) print '(a)', "a quarter-trimmed mean should be a number"
        m = s%trim_mean(0.5_real64)    ! -> aborts
        print '(a,es12.5)', "unexpectedly trimmed half from each tail, m=", m
    end subroutine scenario_stats_obj_trim_mean_prop

    !> A NaN or infinite `score` can only come from the caller's own arithmetic, so locating it in
    !! the population is meaningless and the object says so rather than answering NaN.
    subroutine scenario_stats_obj_percentile_score_non_finite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_stats) :: s
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        real(real64) :: p

        call s%compute(x)
        p = s%percentile_of_score(4.0_real64)                          ! accepted
        if (p < 0.0_real64) print '(a)', "a percentile should not be negative"
        p = s%percentile_of_score(ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,es12.5)', "unexpectedly located a NaN score, p=", p
    end subroutine scenario_stats_obj_percentile_score_non_finite

    !> `%print` with no `unit=` resolves the destination from `parquet_message_stream`.
    !!
    !! Out of process because that setting takes "stdout"/"stderr" and no file, so nothing in
    !! process can capture either destination -- the same reason `%print_stat`'s scenario exists.
    !! Exits cleanly: the assertion is that both tokens are honoured and neither aborts.
    subroutine scenario_stats_print_default_stream()
        type(pf_stats) :: s
        real(real64) :: x(5) = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 9.0_real64]
        character(len=:), allocatable :: saved

        call parquet_get_message_stream(saved)
        call s%compute(x)
        call parquet_set_message_stream("stdout")
        call s%print(name="on stdout")
        call parquet_set_message_stream("stderr")
        call s%print(name="on stderr")
        ! And the uncomputed arm, which returns after one line rather than reading anything.
        block
            type(pf_stats) :: empty
            call empty%print(name="empty, on stderr")
        end block
        call parquet_set_message_stream(saved)
    end subroutine scenario_stats_print_default_stream

    !> A vector column has no defined order on a whole row, so it cannot be a sort key -- the
    !! same rule parquet_table%sort_by applies, enforced here for a bare parquet_column.
    !> `pf_match` over two `parquet_column`s of DIFFERENT kinds is refused rather than compared.
    !!
    !! Both kinds reach the engine as plain integers, so an int32 5 and an int64 5 would match on
    !! the raw value and produce a full-looking, wrong answer -- which is exactly the promotion
    !! astropy performs and this library refuses, because a 64-bit catalogue identifier above
    !! 2**53 does not survive it. The matching pair first is the negative control: without it a
    !! check that refused every `parquet_column` would pass this scenario just as happily.
    !> A table cannot be joined to itself: one table would be argument-associated with an
    !! `intent(inout)` and an `intent(in)` dummy, which F2018 15.5.2.13 forbids as soon as either
    !! is defined and which no compiler in this project's fleet diagnoses. Joining two DIFFERENT
    !! tables first is the negative control -- a guard that refused every join would pass this
    !! scenario just as happily while making the feature unusable.
    subroutine scenario_join_self()
        type(parquet_table) :: a, b, w
        call join_fixture(a, [10_int64, 20_int64, 30_int64])
        call join_fixture(b, [20_int64, 30_int64])
        ! The control joins a CLONE, so `a` reaches the failing call in the state it was built
        ! in -- %join mutates its left table, and a control that spent the fixture would leave
        ! the assertion below about a table the scenario never described.
        !
        ! The failing call below really does make the association the standard forbids, which is
        ! the point: it is what a caller writes, and the guard fires before either dummy is
        ! defined, so nothing is written through the alias. Reproducing it any other way would
        ! test a guard against a call nobody makes.
        call a%clone(w)
        call w%join(b, ["id"])
        print '(a,i0)', "two different tables joined ok, rows=", w%nrows()
        call a%join(a, ["id"])
        print '(a,i0)', "unexpectedly joined a table to itself, rows=", a%nrows()
    end subroutine scenario_join_self

    !> Two key columns of different kinds are refused rather than promoted. Both reach the sort
    !! engine as plain integers, so without the check an int32 5 would match an int64 5 -- which
    !! is astropy's promotion, and what loses a catalogue identifier above 2**53. The same-kind
    !! join first is the control.
    subroutine scenario_join_kind_mismatch()
        type(parquet_table) :: a, b, c, w
        call join_fixture(a, [10_int64, 20_int64, 30_int64])
        call join_fixture(c, [20_int64, 30_int64])
        call a%clone(w)
        call w%join(c, ["id"])
        print '(a,i0)', "same-kind keys joined ok, rows=", w%nrows()
        call parquet_new_table(b)
        call b%add_column("id", [20_int32, 30_int32])
        call a%join(b, ["id"])
        print '(a,i0)', "unexpectedly joined two key kinds, rows=", a%nrows()
    end subroutine scenario_join_kind_mismatch

    !> `require="m:1"` asserts the RIGHT key is unique -- the lookup-table annotation, and the one
    !! that turns "the output is 40x the size you expected" into a named error at the call site.
    !! The unique right key first is the control.
    subroutine scenario_join_require_m1(threads)
        integer, intent(in), optional :: threads !! forwarded to both joins; absent = automatic.
        type(parquet_table) :: a, b, w
        call join_fixture(a, [10_int64, 20_int64, 30_int64])
        call join_fixture(b, [20_int64, 30_int64])
        call a%clone(w)
        call w%join(b, ["id"], require="m:1", threads=threads)
        print '(a,i0)', "a unique right key satisfied require=m:1, rows=", w%nrows()
        call join_fixture(b, [20_int64, 20_int64])
        call a%join(b, ["id"], require="m:1", threads=threads)
        print '(a,i0)', "unexpectedly accepted a duplicate right key, rows=", a%nrows()
    end subroutine scenario_join_require_m1

    !> `max_rows=` refuses a join whose size is known before anything is allocated -- the counting
    !! pass runs first precisely so this can abort rather than run out of memory. A ceiling the
    !! join fits under is the control.
    subroutine scenario_join_max_rows()
        type(parquet_table) :: a, b, w
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        call join_fixture(b, [10_int64, 10_int64, 10_int64])
        call a%clone(w)
        call w%join(b, ["id"], max_rows=9_int64)
        print '(a,i0)', "the join fitted under its ceiling, rows=", w%nrows()
        call a%join(b, ["id"], max_rows=8_int64)
        print '(a,i0)', "unexpectedly built an over-sized join, rows=", a%nrows()
    end subroutine scenario_join_max_rows

    !> `max_rows=` reaches the counting pass from EACH of `%join`'s four ceiling-carrying
    !! specifics: the array and separated-string key forms, in both integer kinds.
    !!
    !! **Four scenarios rather than one, because four entry points forward the same argument
    !! independently and a forward that is simply missing is invisible to every in-process test.**
    !! A ceiling that never arrives refuses nothing, so the join it was meant to stop succeeds and
    !! every assertion about the result still holds; the abort is the only observable, and a
    !! process can only abort once. Mutation confirmed it: dropping the argument from one specific
    !! survived the whole suite while the other three were covered.
    !!
    !! Every form runs first at EXACTLY the nine rows the join emits, which is the negative
    !! control -- and also pins the comparison as `>` rather than `>=`, the one place an
    !! off-by-one would refuse a join that fits.
    subroutine scenario_join_max_rows_form(form)
        integer, intent(in) :: form !! 1 array/int32, 2 array/int64, 3 string/int32, 4 string/int64
        type(parquet_table) :: a, b
        call join_fixture(b, [10_int64, 10_int64, 10_int64])
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        call a%join(b, ["id"], max_rows=9)
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        call a%join(b, ["id"], max_rows=9_int64)
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        call a%join(b, "id", max_rows=9)
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        call a%join(b, "id", max_rows=9_int64)
        print '(a,i0)', "every form ran at exactly its ceiling, rows=", a%nrows()
        call join_fixture(a, [10_int64, 10_int64, 10_int64])
        select case (form)
        case (1)
            call a%join(b, ["id"], max_rows=8)       ! -> aborts (9 rows over an 8-row ceiling)
        case (2)
            call a%join(b, ["id"], max_rows=8_int64) ! -> aborts
        case (3)
            call a%join(b, "id", max_rows=8)         ! -> aborts
        case default
            call a%join(b, "id", max_rows=8_int64)   ! -> aborts
        end select
        print '(a,i0)', "unexpectedly built an over-sized join, rows=", a%nrows()
    end subroutine scenario_join_max_rows_form

    !> `require="1:m"` asserts the LEFT key is unique -- the mirror of `join_require_m1`, and the
    !! branch that scenario cannot reach, since the two sides are separate call sites. A left
    !! table with no duplicate is the control.
    subroutine scenario_join_require_1m(threads)
        integer, intent(in), optional :: threads !! forwarded to both joins; absent = automatic.
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [10_int64, 10_int64])
        call a%join(b, "id", require="1:m", threads=threads)
        print '(a,i0)', "a unique left key satisfied require=1:m, rows=", a%nrows()
        call join_fixture(a, [10_int64, 10_int64, 20_int64])
        call a%join(b, "id", require="1:m", threads=threads)   ! -> aborts (two left rows share key 10)
        print '(a,i0)', "unexpectedly accepted a duplicate left key, rows=", a%nrows()
    end subroutine scenario_join_require_1m

    !> A join abort scenario on ONE named engine: the base scenario run with the sort engine or
    !! the hash engine forced through the test-only hook, so every abort both engines implement
    !! -- the cardinality assertion and the `max_rows=` ceiling, clause by clause in
    !! src/parquet_tables_join.f90 and src/parquet_tables_join_hash.f90 -- is proved on both. The
    !! plain `join_require_m1` and its siblings force the sort engine (the automatic choice is
    !! the hash engine, so an unforced scenario would never reach the sort engine's abort); the
    !! `_hash` twins force the hash engine.
    !!
    !! Two controls before the base scenario runs. A join with the hook CLEAR prints the engine
    !! the automatic rule chose, and one with the hook SET prints the forced engine, so the
    !! wrapper asserts from stdout that the engine it names really was in force when the base
    !! scenario's abort came -- without that line, a twin whose hook did nothing would pass on
    !! the other engine's abort. The hook is reached through a local `bind(C)` interface, as every
    !! `parquet_debug_*` hook is.
    !!
    !! **With `threads`, the sort engine's cardinality check is driven on a TEAM**: the tail floor
    !! is lowered so a three-row fixture opens one, a control join prints the team the engine's
    !! group passes recorded, and the base scenario's joins take the same `threads=` -- so the
    !! abort that follows comes from the chunked check and its post-region raise, which the
    !! serial twin never reaches. The wrapper asserts the printed team as it asserts the engine.
    subroutine scenario_join_engine_twin(engine, which, threads)
        use iso_c_binding, only : c_int64_t
        character(len=*), intent(in) :: engine !! "sort" or "hash".
        character(len=*), intent(in) :: which  !! the base scenario, by the tail of its name.
        integer, intent(in), optional :: threads !! a team for the base scenario's joins.
        interface
            subroutine set_join_engine(mode) bind(C, name="parquet_debug_set_join_engine")
                import :: c_int64_t
                integer(c_int64_t), value :: mode !! 0 automatic, 1 the sort engine, 2 the hash engine.
            end subroutine set_join_engine
            function join_engine_used() result(e) bind(C, name="parquet_debug_join_engine_used")
                import :: c_int64_t
                integer(c_int64_t) :: e !! the engine the last join ran on.
            end function join_engine_used
            function join_group_threads() result(n) bind(C, name="parquet_debug_get_join_group_threads_used")
                import :: c_int64_t
                integer(c_int64_t) :: n !! the team the sort engine's group passes ran on.
            end function join_group_threads
        end interface
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64, 30_int64])
        call join_fixture(b, [20_int64, 30_int64])
        call set_join_engine(0_c_int64_t)
        call a%join(b, ["id"])
        print '(a,i0)', "join engine used with the hook clear=", join_engine_used()
        if (engine == "sort") then
            call set_join_engine(1_c_int64_t)
        else
            call set_join_engine(2_c_int64_t)
        end if
        call join_fixture(a, [10_int64, 20_int64, 30_int64])
        call a%join(b, ["id"])
        print '(a,i0)', "join engine used with the hook set=", join_engine_used()
        if (present(threads)) then
            call parquet_debug_set_sort_tail_min_rows(1_int64)
            call join_fixture(a, [10_int64, 20_int64, 30_int64])
            call a%join(b, ["id"], threads=threads)
            print '(a,i0)', "join group passes team=", join_group_threads()
        end if
        select case (which)
        case ("require_m1")
            call scenario_join_require_m1(threads)
        case ("require_1m")
            call scenario_join_require_1m(threads)
        case ("max_rows")
            call scenario_join_max_rows()
        case ("max_rows_arr_i32")
            call scenario_join_max_rows_form(1)
        case ("max_rows_arr_i64")
            call scenario_join_max_rows_form(2)
        case ("max_rows_str_i32")
            call scenario_join_max_rows_form(3)
        case default
            call scenario_join_max_rows_form(4)
        end select
    end subroutine scenario_join_engine_twin

    !> An unrecognized `require=` token aborts naming every accepted value, rather than falling
    !! back to no assertion -- which would be the worst available answer, since a caller who
    !! wrote `require=` has said the cardinality matters. An upper-cased recognized token is the
    !! control, and exercises the case folding at the same time.
    subroutine scenario_join_bad_require()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64, 10_int64])
        call join_fixture(b, [10_int64, 20_int64])
        call a%join(b, "id", require="M:1")
        print '(a,i0)', "require=M:1 folded and ran, rows=", a%nrows()
        call join_fixture(a, [10_int64, 20_int64, 10_int64])
        call a%join(b, "id", require="1:2")   ! -> aborts (not one of the four tokens)
        print '(a,i0)', "unexpectedly accepted require=1:2, rows=", a%nrows()
    end subroutine scenario_join_bad_require

    !> The join's running pair count refuses the sum that would wrap a 64-bit integer. No fixture
    !> can reach it -- it needs more than 9.2e18 pairs -- so the guard is driven through its debug
    !> hook. The control first proves the largest legal total is accepted and comes back exact,
    !> and every result is printed, so that no compiler may treat the call as dead.
    subroutine scenario_join_pair_count_overflow()
        integer(int64) :: s
        s = parquet_debug_join_add_checked(huge(0_int64) - 1_int64, 1_int64)
        print '(a,i0)', "control: the boundary pair count was accepted, s=", s
        s = parquet_debug_join_add_checked(huge(0_int64) - 1_int64, 2_int64)   ! one past it -> aborts
        print '(a,i0)', "unexpectedly accepted a wrapping pair count, s=", s
    end subroutine scenario_join_pair_count_overflow

    !> An unrecognized `order=` token aborts naming both accepted values rather than silently
    !! leaving the default ordering in place. The upper-cased recognized token is the control.
    subroutine scenario_join_bad_order()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [10_int64, 20_int64])
        call a%join(b, "id", order="KEY")
        print '(a,i0)', "order=KEY folded and ran, rows=", a%nrows()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", order="middle")   ! -> aborts (not 'left' or 'key')
        print '(a,i0)', "unexpectedly accepted order=middle, rows=", a%nrows()
    end subroutine scenario_join_bad_order

    !> An unrecognized `how=` token aborts naming every accepted value, rather than falling back
    !! to a default the caller did not ask for. A recognized token is the control.
    subroutine scenario_join_bad_how()
        type(parquet_table) :: a, b, w
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%clone(w)
        call w%join(b, ["id"], how="OUTER")
        print '(a,i0)', "how=OUTER folded and ran, rows=", w%nrows()
        call a%join(b, ["id"], how="cross")
        print '(a,i0)', "unexpectedly accepted how=cross, rows=", a%nrows()
    end subroutine scenario_join_bad_how

    !> `other_on=` takes one right-hand key name per left-hand one; a mismatched length aborts
    !! rather than pairing what it can. The matching length is the control.
    subroutine scenario_join_other_on_size()
        type(parquet_table) :: a, b, w
        call parquet_new_table(a)
        call a%add_column("f", [1_int64, 2_int64])
        call a%add_column("g", [3_int64, 4_int64])
        call parquet_new_table(b)
        call b%add_column("p", [1_int64])
        call b%add_column("q", [3_int64])
        call a%clone(w)
        call w%join(b, ["f", "g"], other_on=["p", "q"])
        print '(a,i0)', "two keys against two other_on names joined ok, rows=", w%nrows()
        call a%join(b, ["f", "g"], other_on=["p"])
        print '(a,i0)', "unexpectedly accepted a short other_on=, rows=", a%nrows()
    end subroutine scenario_join_other_on_size

    !> A join with no key at all is refused. There is no useful default -- a keyless join is a
    !! cross product, which this library does not offer and would not offer by accident.
    subroutine scenario_join_no_key()
        type(parquet_table) :: a, b, w
        character(len=8), allocatable :: nokeys(:)
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%clone(w)
        call w%join(b, ["id"])
        print '(a,i0)', "one key joined ok, rows=", w%nrows()
        allocate(nokeys(0))
        call a%join(b, nokeys)
        print '(a,i0)', "unexpectedly joined with no key, rows=", a%nrows()
    end subroutine scenario_join_no_key

    !> `columns=` is refused with `how='semi'` and `how='anti'`: those two keep or drop rows of
    !! the left table and carry no column at all, so a list naming columns is a request the call
    !! cannot honour, and ignoring it would leave the caller believing columns had arrived. The
    !! same `how` without `columns=` is the control, and so is the same `columns=` with a `how`
    !! that does carry payload.
    !!
    !! Replaced `scenario_join_how_unsupported`, which asserted that `right`/`outer`/`semi`/`anti`
    !! were refused by name until the column rewrite could carry them out. It said in its own
    !! comment that it was meant to be deleted along with that refusal; S6 P6 did both.
    subroutine scenario_join_columns_with_semi()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="semi")
        print '(a,i0)', "how=semi selected rows without columns=, rows=", a%nrows()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="left", columns="payload")
        print '(a,i0)', "columns= carried payload on a how that has one, cols=", a%ncols()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="anti", columns="payload")   ! -> aborts
        print '(a,i0)', "unexpectedly accepted columns= with how=anti, cols=", a%ncols()
    end subroutine scenario_join_columns_with_semi

    !> A container column on the LEFT is refused under `how='right'`/`'outer'` -- the mirror of
    !! `join_container_payload`'s refusal on the incoming side, and for the same reason: those two
    !! `how` values emit rows with no counterpart here, so every column here has to be fillable
    !! with nulls and `%set_validity` has no arm for a container kind.
    !!
    !! The refusal is by KIND and SIDE, never by whether this particular join has an unmatched
    !! row, so the fixture below deliberately matches every row -- and is still refused. Both
    !! controls matter: the same table joins fine under `how='left'`, and a `how='outer'` join of
    !! a table WITHOUT a container column runs.
    subroutine scenario_join_left_container_outer()
        type(parquet_table) :: a, b
        type(parquet_list_column) :: lc
        call join_fixture(b, [10_int64, 20_int64])
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="outer")
        print '(a,i0)', "how=outer on a container-free table ran, rows=", a%nrows()
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32])
        call join_fixture(a, [10_int64, 20_int64])
        call a%add_column("tags", lc)
        call a%join(b, "id", how="left")
        print '(a,i0)', "a container column survived a how=left join, cols=", a%ncols()
        call join_fixture(a, [10_int64, 20_int64])
        call a%add_column("tags", lc)
        call a%join(b, "id", how="outer")   ! -> aborts (a list column cannot be null-filled)
        print '(a,i0)', "unexpectedly outer-joined over a container column, rows=", a%nrows()
    end subroutine scenario_join_left_container_outer

    !> A join key is a column NAME and has no direction: `on="-id"` is a sort key, and accepting
    !! it would silently join on a column called "-id" that does not exist, or worse would look
    !! like the ordering mattered. The plain name first is the control.
    subroutine scenario_join_key_direction()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left")
        print '(a,i0)', "a plain key name joined ok, rows=", a%nrows()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id desc", how="left")   ! -> aborts (a join key has no direction)
        print '(a,i0)', "unexpectedly accepted a sort key as a join key, rows=", a%nrows()
    end subroutine scenario_join_key_direction

    !> The `-name` shorthand is refused for the same reason `"name desc"` is, and it needs its own
    !! scenario because the two spellings reach the refusal down separate arms of one test: drop
    !! the leading-dash arm and every assertion about the trailing-word arm still passes. Confirmed
    !! by mutation. The plain key name first is the control.
    subroutine scenario_join_key_direction_dash()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left")
        print '(a,i0)', "a plain key name joined ok, rows=", a%nrows()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "-id", how="left")   ! -> aborts (a join key has no direction)
        print '(a,i0)', "unexpectedly accepted the -name shorthand as a join key, rows=", a%nrows()
    end subroutine scenario_join_key_direction_dash

    !> A container column cannot be carried across a join. An output row with no counterpart has
    !! to be null-filled, and a container's own row gather has no established behaviour for that
    !! -- so the refusal is by KIND and side, never by whether this particular join happens to
    !! have an unmatched row, or a program's join would start failing when its input changed.
    !! The same join without the container is the control.
    subroutine scenario_join_container_payload()
        type(parquet_table) :: a, b
        type(parquet_list_column) :: lc
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left")
        print '(a,i0)', "a scalar payload came across ok, rows=", a%nrows()
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call b%add_column("tags", lc)
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="left")   ! -> aborts (a list column cannot be carried)
        print '(a,i0)', "unexpectedly carried a container column across, cols=", a%ncols()
    end subroutine scenario_join_container_payload

    !> A container column cannot be a join KEY either, and the message must say "join key" rather
    !! than "sort key".
    !!
    !! The rule is shared with `%sort_by` -- both resolve a key name through `table_lookup_sort_key`
    !! precisely so that two copies of "which columns can be a key" cannot drift -- and only the
    !! WORDING differs. That is why this scenario exists beside `container_sort_key`: the shared
    !! guard is reached by two callers, and a join that stopped calling it would lose the refusal
    !! with `container_sort_key` still green. The sort's own advice is also actively wrong here,
    !! since a container cannot be carried across a join at all, so a caller told to "sort by a
    !! scalar column and the container is carried along with it" would walk into a second abort.
    !!
    !! The scalar join first is the control: without it the abort below would be satisfied by a
    !! `%join` that refused every key.
    subroutine scenario_join_container_key()
        type(parquet_table) :: a, b
        type(parquet_list_column) :: lc
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left")
        print '(a,i0)', "a scalar join key worked, rows=", a%nrows()
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32])
        call join_fixture(a, [10_int64, 20_int64])
        call a%add_column("tags", lc)
        call a%join(b, "tags", how="left")   ! -> aborts (a container cannot be a join key)
        print '(a,i0)', "unexpectedly joined on a container column, rows=", a%nrows()
    end subroutine scenario_join_container_key

    !> `columns=` naming a column the right table does not have is a mistake worth hearing about:
    !! skipping it would leave the caller believing a column had arrived, and a join keeps no link
    !! to the right table's file, so a column that did not come across is gone. The real name is
    !! the control.
    subroutine scenario_join_columns_unknown()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left", columns="payload")
        print '(a,i0)', "columns=payload came across ok, cols=", a%ncols()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="left", columns="mag_r")   ! -> aborts (no such column)
        print '(a,i0)', "unexpectedly accepted an unknown columns= name, cols=", a%ncols()
    end subroutine scenario_join_columns_unknown

    !> A suffixed incoming name that STILL clashes is an error naming both, not a second suffix:
    !! `payload_2_2` is a name nobody asked for, and quietly renaming twice hides the collision
    !! the caller needs to know about. The single collision, which the suffix resolves, is the
    !! control.
    subroutine scenario_join_suffix_clash()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left")
        print '(a,i0)', "one collision took the suffix ok, cols=", a%ncols()
        call join_fixture(a, [10_int64, 20_int64])
        call a%add_column("payload_2", [7_int64, 8_int64])
        call a%join(b, "id", how="left")   ! -> aborts (payload and payload_2 both taken)
        print '(a,i0)', "unexpectedly resolved a doubly-clashing name, cols=", a%ncols()
    end subroutine scenario_join_suffix_clash

    !> A blank `other_suffix=` leaves a clashing incoming column no name to take, so it is refused
    !! up front rather than reported later as a name that collides with itself. A real suffix is
    !! the control.
    subroutine scenario_join_blank_suffix()
        type(parquet_table) :: a, b
        call join_fixture(a, [10_int64, 20_int64])
        call join_fixture(b, [20_int64])
        call a%join(b, "id", how="left", other_suffix="_r")
        print '(a,i0)', "other_suffix=_r joined ok, cols=", a%ncols()
        call join_fixture(a, [10_int64, 20_int64])
        call a%join(b, "id", how="left", other_suffix="")   ! -> aborts (nothing to suffix with)
        print '(a,i0)', "unexpectedly accepted a blank other_suffix, cols=", a%ncols()
    end subroutine scenario_join_blank_suffix

    !> A left column that was never read is SKIPPED by the join's gather, and the table has
    !! detached -- so reading it afterwards is the detach guard's named error rather than a column
    !! of the wrong length silently aligned to nothing. This is the loud half of the residency
    !! rule, and the reason the guide tells a caller to materialize what they need first. Reading
    !! the key column, which the join itself read, is the control.
    !!
    !! **The right key is DUPLICATED on purpose, and the scenario is vacuous without it.** A join
    !! that leaves every left row exactly once and in place does not detach at all (`Risk-184`), so
    !! against a unique key this reads the skipped column perfectly happily -- which is what it did
    !! until P4 landed and this scenario stopped aborting. The duplicate is what makes it a join
    !! that rewrites the row set.
    subroutine scenario_join_detached_column()
        character(len=*), parameter :: f = "test_run/scen_join_detached.parquet"
        type(parquet_table) :: src, a, b
        integer(int64), allocatable :: got(:)
        call join_fixture(src, [10_int64, 20_int64])
        call parquet_write_table(src, f, overwrite=.true.)
        call parquet_open_table(a, f)
        call join_fixture(b, [20_int64, 20_int64])
        call a%join(b, "id", how="left")
        call a%get("id", got)
        print '(a,i0)', "the key column the join read is still readable, rows=", size(got)
        call a%get("payload", got)   ! -> aborts (never read, and the table has detached)
        print '(a,i0)', "unexpectedly read a skipped column after a join, rows=", size(got)
    end subroutine scenario_join_detached_column

    subroutine scenario_sorting_match_kind_mismatch()
        type(parquet_column) :: a, b, c
        integer(int64), allocatable :: m(:)
        call a%init(PK_INT32, 3_int64)
        call a%set_all([10_int32, 20_int32, 30_int32])
        call c%init(PK_INT32, 2_int64)
        call c%set_all([30_int32, 10_int32])
        call pf_match(a, c, m)
        print '(a,i0)', "same-kind columns matched ok, first answer=", m(1)
        call b%init(PK_INT64, 2_int64)
        call b%set_all([30_int64, 10_int64])
        call pf_match(a, b, m)   ! -> aborts (an int32 column cannot match an int64 one)
        print '(a,i0)', "unexpectedly matched two kinds, first answer=", m(1)
    end subroutine scenario_sorting_match_kind_mismatch

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

    !> A BULK search whose answer array is not one entry per target is refused rather than filling
    !! what fits. A short array would leave the caller with answers for some targets and whatever
    !! the buffer held for the rest -- indistinguishable from a real position.
    subroutine scenario_sorting_search_many_answer_length()
        integer(int32) :: v(4) = [10, 20, 30, 40]
        integer(int32) :: t(3) = [15, 25, 35]
        integer(int64) :: fits(3), short(2)
        call pf_lower_bound(v, t, fits)          ! control: one entry per target
        print '(a,i0)', "control: the matching answer array gave pos(1)=", fits(1)
        call pf_lower_bound(v, t, short)         ! -> aborts
        print '(a,i0)', "a short answer array was accepted, pos(1)=", short(1)
    end subroutine scenario_sorting_search_many_answer_length

    !> `scenario_sorting_search_target_too_long` for the BULK form, whose padding is done per
    !! target in a separate worker from the scalar form's.
    subroutine scenario_sorting_search_many_target_too_long()
        character(len=3) :: v(3) = ["aaa", "bbb", "ccc"]
        character(len=4) :: ok_t(2) = ["bb  ", "cc  "]
        character(len=4) :: bad_t(2) = ["bb  ", "bbbb"]
        integer(int64) :: pos(2)
        call pf_lower_bound(v, ok_t, pos)        ! control: both targets fit once trimmed
        print '(a,i0)', "control: the shorter targets gave pos(1)=", pos(1)
        call pf_lower_bound(v, bad_t, pos)       ! -> aborts (4 non-blank characters, 3 per element)
        print '(a,i0)', "an over-long bulk target was accepted, pos(2)=", pos(2)
    end subroutine scenario_sorting_search_many_target_too_long

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

    !> `%normal_truncated` with a non-positive sigma must abort rather than divide by it.
    !!
    !! The control draws with a tiny but legal sigma first, so a run that aborted before reaching
    !! the guarded call is distinguishable from one the guard caught. Every scenario here prints
    !! its draw: the guard sits inside a `pure` subroutine, and gfortran deletes an unused pure
    !! call at -O1 and above, which would exit 0 (`check_scenario_uses_a_pure_result`).
    subroutine scenario_random_normal_truncated_sigma_not_positive()
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%normal_truncated(-1.0_real64, 1.0_real64, x, sigma=1.0e-30_real64)
        print '(a,es12.5)', "drew a truncated normal with sigma 1e-30: ", x
        call rng%normal_truncated(-1.0_real64, 1.0_real64, x, sigma=0.0_real64)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a truncated normal with sigma 0: ", x
    end subroutine scenario_random_normal_truncated_sigma_not_positive

    !> A NaN sigma must abort rather than return a NaN. The guard is written `.not. (sigma > 0)`
    !! rather than `sigma <= 0` for exactly this reason: every comparison against a NaN is false,
    !! so the second form would wave it through.
    subroutine scenario_random_normal_truncated_sigma_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%normal_truncated(-1.0_real64, 1.0_real64, x, sigma=2.0_real64)
        print '(a,es12.5)', "drew a truncated normal with sigma 2: ", x
        call rng%normal_truncated(-1.0_real64, 1.0_real64, x, &
            sigma=ieee_value(0.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a truncated normal with a NaN sigma: ", x
    end subroutine scenario_random_normal_truncated_sigma_nan

    !> Bounds the wrong way round must abort, NOT be swapped.
    !!
    !! `%int_range` swaps them, and that is the right call there because both orders name the same
    !! set of integers. Here they name different things and one of them names nothing, so a swap
    !! would answer a question the caller did not ask.
    subroutine scenario_random_normal_truncated_bounds_reversed()
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%normal_truncated(-1.0_real64, 2.0_real64, x)
        print '(a,es12.5)', "drew a truncated normal on [-1, 2]: ", x
        call rng%normal_truncated(2.0_real64, -1.0_real64, x)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a truncated normal on [2, -1]: ", x
    end subroutine scenario_random_normal_truncated_bounds_reversed

    !> A NaN bound must abort. Caught by the same negated comparison as the NaN sigma, and by the
    !! same reasoning; a NaN `mu` is caught here too, because it makes both standardised bounds NaN.
    subroutine scenario_random_normal_truncated_bounds_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%normal_truncated(-1.0_real64, 2.0_real64, x)
        print '(a,es12.5)', "drew a truncated normal on [-1, 2]: ", x
        call rng%normal_truncated(-1.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), x)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a truncated normal with a NaN upper bound: ", x
    end subroutine scenario_random_normal_truncated_bounds_nan

    !> An interval that is non-empty in data units but COLLAPSES under the centring must abort.
    !!
    !! This is the only scenario asserting that the standardisation happens BEFORE the guard rather
    !! than the guard reading the raw arguments: `lo < hi` holds here, and
    !! `(lo - mu)/sigma < (hi - mu)/sigma` does not. It asserts the same message as the
    !! reversed-bounds scenario and is kept for that reason -- the message names the centring and
    !! scaling precisely so this case is explicable to whoever hits it.
    !!
    !! **The collapse comes from `mu`, not from `sigma`.** Dividing by a large scale keeps the two
    !! bounds distinct, because division preserves relative spacing; SUBTRACTING a large `mu` does
    !! not, because `1 - 1e10` and `nearest(1, 2) - 1e10` are one double. That asymmetry is why
    !! this fixture is built the way it is and not the more obvious way.
    subroutine scenario_random_normal_truncated_bounds_collapse()
        type(pf_random_stream) :: rng
        real(real64) :: x
        call rng%seed(1_int64, 1_int64)
        call rng%normal_truncated(1.0_real64, 1.0_real64 + 1.0e-3_real64, x, sigma=1.0e-3_real64)
        print '(a,es12.5)', "drew a truncated normal on a one-sigma-wide interval: ", x
        ! Two adjacent doubles, offset by a mu large enough that both centre onto one value.
        call rng%normal_truncated(1.0_real64, nearest(1.0_real64, 2.0_real64), x, &
            mu=1.0e10_real64, sigma=1.0e-3_real64)   ! -> aborts
        print '(a,es12.5)', "unexpectedly drew a truncated normal on a collapsed interval: ", x
    end subroutine scenario_random_normal_truncated_bounds_collapse

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

    ! ---- parquet_random: points on a sphere ----
    !
    ! Every refusal here sits inside a `pure` procedure, so every scenario PRINTS the value it drew,
    ! control and refused call alike: gfortran deletes a pure call whose result is unused at -O1 and
    ! above, and the scenario would then exit 0 (`check_scenario_uses_a_pure_result`). Each control is
    ! the nearest legal call, so a guard drawn one step too tight aborts on the control instead.
    !
    ! Each centre is a named `parameter` rather than an array constructor written at the call: these
    ! `centre(3)`/`mu(3)` dummies are explicit-shape, so a constructor -- a value with no address of
    ! its own -- is argument-associated through a temporary, which ifx reports as `warning (406)` on
    ! every call under --profile debug, into the stderr these scenarios assert on. See
    ! .claude/rules/fortran-gotchas.md.

    !> A negative angular radius is refused. The control is radius 0, which is legal and is the centre.
    subroutine scenario_random_disc_radius_negative()
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.0_real64)
        print '(a,3es12.4)', "drew a disc of radius 0: ", v
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, -0.1_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a disc of negative radius: ", v
    end subroutine scenario_random_disc_radius_negative

    !> A NaN radius is refused. The control is a radius above the half turn, which clamps rather than
    !! aborts -- so a guard written `radius <= pi` would fail the control.
    subroutine scenario_random_disc_radius_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 3.5_real64)
        print '(a,3es12.4)', "drew a disc of radius 3.5, the whole sphere: ", v
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, ieee_value(0.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a disc of NaN radius: ", v
    end subroutine scenario_random_disc_radius_nan

    !> A zero centre names no direction. The control is a centre of length about 1e-300, which does.
    subroutine scenario_random_disc_centre_zero()
        real(real64) :: v(3)
        real(real64), parameter :: TINY_CENTRE(3) = [1.0e-300_real64, 0.0_real64, 1.0e-300_real64]
        real(real64), parameter :: ZERO3(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, TINY_CENTRE, 0.5_real64)
        print '(a,3es12.4)', "drew a disc about a centre of length 1e-300: ", v
        v = pf_random_disc_at(1_int64, 1_int64, ZERO3, 0.5_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a disc about the zero vector: ", v
    end subroutine scenario_random_disc_centre_zero

    !> A centre with a NaN component is refused before anything could compare against it.
    subroutine scenario_random_disc_centre_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: v(3), nan_centre(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.5_real64)
        print '(a,3es12.4)', "drew a disc about +z: ", v
        nan_centre = [ieee_value(0.0_real64, ieee_quiet_nan), 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, nan_centre, 0.5_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a disc about a NaN centre: ", v
    end subroutine scenario_random_disc_centre_nan

    !> An inner radius beyond the outer names an empty ring. The control is `r_inner = radius`, the
    !! circle itself, which is legal.
    subroutine scenario_random_disc_inner_exceeds_radius()
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.5_real64, 1_int64, 0.5_real64)
        print '(a,3es12.4)', "drew the circle r_inner = radius: ", v
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.5_real64, 1_int64, 0.6_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a ring whose inner radius exceeds its outer: ", v
    end subroutine scenario_random_disc_inner_exceeds_radius

    !> An inner radius past the half turn is refused rather than clamped: clamping leaves a ring of
    !! zero width whose every draw is the antipode. The control is `r_inner = pi`, the antipode
    !! circle, which is the largest legal one.
    subroutine scenario_random_disc_inner_above_half_turn()
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        real(real64), parameter :: PI = 3.14159265358979323846264338327950288_real64
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, PI, 1_int64, PI)
        print '(a,3es12.4)', "drew the antipode circle r_inner = radius = pi: ", v
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 5.0_real64, 1_int64, 4.0_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a ring whose inner radius passes the half turn: ", v
    end subroutine scenario_random_disc_inner_above_half_turn

    !> A NaN inner radius is refused by its own screen, ahead of the range test it would slip through.
    subroutine scenario_random_disc_inner_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.5_real64, 1_int64, 0.0_real64)
        print '(a,3es12.4)', "drew a disc with r_inner = 0: ", v
        v = pf_random_disc_at(1_int64, 1_int64, NORTH, 0.5_real64, 1_int64, &
                              ieee_value(0.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a disc with a NaN inner radius: ", v
    end subroutine scenario_random_disc_inner_nan

    !> A declination outside [-90, 90] is refused rather than read as the direction it names: a cap
    !! about a mirrored position is a plausible wrong answer. The control is the pole itself.
    subroutine scenario_random_disc_radec_dec_out_of_range()
        real(real64) :: ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, 10.0_real64, 90.0_real64, 1.0_real64, ra, dec)
        print '(a,2es12.4)', "drew a disc about the pole: ", ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, 10.0_real64, 90.5_real64, 1.0_real64, ra, dec)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly drew a disc about declination 90.5: ", ra, dec
    end subroutine scenario_random_disc_radec_dec_out_of_range

    !> A NaN right ascension is refused. The control is one past two turns, which is legal.
    subroutine scenario_random_disc_radec_centre_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, 725.0_real64, -90.0_real64, 1.0_real64, ra, dec)
        print '(a,2es12.4)', "drew a disc about ra0 725: ", ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, ieee_value(0.0_real64, ieee_quiet_nan), 10.0_real64, &
                                     1.0_real64, ra, dec)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly drew a disc about a NaN right ascension: ", ra, dec
    end subroutine scenario_random_disc_radec_centre_nan

    !> The RA/Dec disc names its radius `radius_deg`, and so does its refusal. The control is 180
    !! degrees, the whole sky.
    subroutine scenario_random_disc_radec_radius_negative()
        real(real64) :: ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, 180.0_real64, ra, dec)
        print '(a,2es12.4)', "drew a disc of radius 180 degrees: ", ra, dec
        call pf_random_disc_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, -1.0_real64, ra, dec)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly drew a disc of radius -1 degree: ", ra, dec
    end subroutine scenario_random_disc_radec_radius_negative

    !> A negative ball radius is refused. The control is radius 0, the origin.
    subroutine scenario_random_ball_radius_negative()
        real(real64) :: p(3)
        p = pf_random_ball_at(1_int64, 1_int64, 0.0_real64)
        print '(a,3es12.4)', "drew a point in a ball of radius 0: ", p
        p = pf_random_ball_at(1_int64, 1_int64, -1.0_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a point in a ball of negative radius: ", p
    end subroutine scenario_random_ball_radius_negative

    !> A shell whose inner radius exceeds its outer is refused. The control is the sphere `r_inner = radius`.
    subroutine scenario_random_ball_inner_exceeds_radius()
        real(real64) :: p(3)
        p = pf_random_ball_at(1_int64, 1_int64, 2.0_real64, 1_int64, 2.0_real64)
        print '(a,3es12.4)', "drew a point on the sphere r_inner = radius: ", p
        p = pf_random_ball_at(1_int64, 1_int64, 2.0_real64, 1_int64, 2.5_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a point in an inverted shell: ", p
    end subroutine scenario_random_ball_inner_exceeds_radius

    !> A negative concentration is refused. The control is `kappa = 0`, the uniform direction. The
    !! refused value is large enough that its message takes the three-digit exponent form.
    subroutine scenario_random_vmf_kappa_negative()
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_vmf_at(1_int64, 1_int64, NORTH, 0.0_real64)
        print '(a,3es12.4)', "drew a vMF with kappa 0: ", v
        v = pf_random_vmf_at(1_int64, 1_int64, NORTH, -1.0e300_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a vMF with a negative kappa: ", v
    end subroutine scenario_random_vmf_kappa_negative

    !> A NaN concentration is refused. The control is `kappa = 1e-30`, which must still draw.
    subroutine scenario_random_vmf_kappa_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        v = pf_random_vmf_at(1_int64, 1_int64, NORTH, 1.0e-30_real64)
        print '(a,3es12.4)', "drew a vMF with kappa 1e-30: ", v
        v = pf_random_vmf_at(1_int64, 1_int64, NORTH, ieee_value(0.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a vMF with a NaN kappa: ", v
    end subroutine scenario_random_vmf_kappa_nan

    !> A zero mean direction is refused, through the same check the disc's centre goes through.
    subroutine scenario_random_vmf_mu_zero()
        real(real64) :: v(3)
        real(real64), parameter :: TINY_MU(3) = [0.0_real64, 1.0e-300_real64, 0.0_real64]
        real(real64), parameter :: ZERO3(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        v = pf_random_vmf_at(1_int64, 1_int64, TINY_MU, 5.0_real64)
        print '(a,3es12.4)', "drew a vMF about a mean of length 1e-300: ", v
        v = pf_random_vmf_at(1_int64, 1_int64, ZERO3, 5.0_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew a vMF about the zero vector: ", v
    end subroutine scenario_random_vmf_mu_zero

    !> A width of zero is refused: it names no distribution. The control is a width of 1e-300 degrees.
    subroutine scenario_random_vmf_radec_sigma_not_positive()
        real(real64) :: ra, dec
        call pf_random_vmf_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, 1.0e-300_real64, ra, dec)
        print '(a,2es12.4)', "drew a vMF of width 1e-300 degrees: ", ra, dec
        call pf_random_vmf_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, 0.0_real64, ra, dec)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly drew a vMF of width 0: ", ra, dec
    end subroutine scenario_random_vmf_radec_sigma_not_positive

    !> A NaN width is refused by its own screen. The control is a width of 1e155 degrees, the uniform limit.
    subroutine scenario_random_vmf_radec_sigma_nan()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: ra, dec
        call pf_random_vmf_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, 1.0e155_real64, ra, dec)
        print '(a,2es12.4)', "drew a vMF of width 1e155 degrees: ", ra, dec
        call pf_random_vmf_radec_at(1_int64, 1_int64, 10.0_real64, 20.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), &
                                    ra, dec)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly drew a vMF of NaN width: ", ra, dec
    end subroutine scenario_random_vmf_radec_sigma_nan

    !> A direction fill needs three rows. The controls are a (3, 0) fill -- a no-op -- and a (3, 2) one.
    subroutine scenario_random_fill_direction_bad_shape()
        real(real64) :: none(3, 0), good(3, 2), bad(2, 4)
        call pf_random_fill_direction(1_int64, 1_int64, none)
        call pf_random_fill_direction(1_int64, 1_int64, good)
        print '(a,3es12.4)', "filled three rows: ", good(:, 2)
        call pf_random_fill_direction(1_int64, 1_int64, bad)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly filled two rows: ", bad(:, 1)
    end subroutine scenario_random_fill_direction_bad_shape

    !> An RA/Dec fill needs two arrays of one size. The control fills two of size 4.
    subroutine scenario_random_fill_radec_size_mismatch()
        real(real64) :: ra(4), dec(4), dec3(3)
        call pf_random_fill_radec(1_int64, 1_int64, ra, dec)
        print '(a,2es12.4)', "filled four positions, the last ", ra(4), dec(4)
        call pf_random_fill_radec(1_int64, 1_int64, ra, dec3)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly filled arrays of sizes 4 and 3: ", ra(1), dec3(1)
    end subroutine scenario_random_fill_radec_size_mismatch

    !> Draw `2**62 + 1` would read block `2**62`, whose index sets bit 62 and so lands in another word
    !! space silently; it is refused. The control is draw `2**62`, the last that exists.
    subroutine scenario_random_sphere_draw_beyond_2p62()
        integer(int64), parameter :: LAST = 4611686018427387904_int64
        real(real64) :: v(3)
        v = pf_random_direction_at(1_int64, 1_int64, LAST)
        print '(a,3es12.4)', "drew direction 2**62: ", v
        v = pf_random_direction_at(1_int64, 1_int64, LAST + 1_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew direction 2**62 + 1: ", v
    end subroutine scenario_random_sphere_draw_beyond_2p62

    !> A fill is bounded by its LAST draw, through its own check. The control ends exactly on `2**62`.
    subroutine scenario_random_fill_direction_draw_beyond_2p62()
        integer(int64), parameter :: LAST = 4611686018427387904_int64
        real(real64) :: v(3, 4)
        call pf_random_fill_direction(1_int64, 1_int64, v, LAST - 3_int64)
        print '(a,3es12.4)', "filled up to direction 2**62, the last ", v(:, 4)
        call pf_random_fill_direction(1_int64, 1_int64, v, LAST - 2_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly filled past direction 2**62: ", v(:, 4)
    end subroutine scenario_random_fill_direction_draw_beyond_2p62

    !> The stream form names itself in its refusal. The control is the circle `r_inner = radius`.
    subroutine scenario_random_stream_disc_inner_exceeds_radius()
        type(pf_random_stream) :: rng
        real(real64) :: v(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        call rng%seed(1_int64, 1_int64)
        call rng%disc(NORTH, 0.5_real64, v, r_inner=0.5_real64)
        print '(a,3es12.4)', "drew the circle r_inner = radius from a stream: ", v
        call rng%disc(NORTH, 0.5_real64, v, r_inner=0.6_real64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew an inverted ring from a stream: ", v
    end subroutine scenario_random_stream_disc_inner_exceeds_radius

    !> `%at` on a cap `%prepare` has never run aborts, rather than drawing from a zero frame.
    !!
    !! An unprepared cap holds a zero centre and a zero ring width, so without the guard `%at`
    !! would hand back a plausible-looking vector built from nothing the caller asked for.
    !!
    !! **The control is the same call on a PREPARED cap**, and it earns its place: without it the
    !! scenario would pass just as well on a build where `%at` aborted unconditionally. It also
    !! exercises both specifics of the `%at` generic, the `int32` stream index and the `int64` one,
    !! since a caller reaching for a cap inside a rejection walk passes whichever its loop index is.
    !! The results are printed because `%at` is `pure` and gfortran deletes an unused pure call at
    !! `-O1` and above (`check_scenario_uses_a_pure_result`).
    subroutine scenario_random_disc_cap_unprepared()
        type(pf_random_disc_cap) :: ready, blank
        real(real64) :: v(3), w(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        call ready%prepare(NORTH, 0.3_real64)
        v = ready%at(1_int64, 1_int64, 1_int64)
        w = ready%at(1_int64, 1, 1_int64)
        print '(a,3es12.4,a,l1,a,l1)', "drew from a prepared cap: ", v, "  int32 index agrees: ", &
            all(v == w), "  blank is_set: ", blank%is_set()
        v = blank%at(1_int64, 1_int64, 1_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew from a cap %prepare has not run: ", v
    end subroutine scenario_random_disc_cap_unprepared

    !> A polygon needs three vertices. The control is a triangle.
    subroutine scenario_sphere_polygon_too_few_vertices()
        type(pf_sky_polygon) :: tri, two
        call tri%init([0.0_real64, 10.0_real64, 10.0_real64], [0.0_real64, 0.0_real64, 10.0_real64])
        print '(a,es12.4)', "built a triangle of area ", tri%area()
        call two%init([0.0_real64, 10.0_real64], [0.0_real64, 0.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon of vertex count ", two%size()
    end subroutine scenario_sphere_polygon_too_few_vertices

    !> The vertex arrays must be of one size. The control is a rectangle.
    subroutine scenario_sphere_polygon_size_mismatch()
        type(pf_sky_polygon) :: rect, bad
        call rect%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,es12.4)', "built a rectangle of area ", rect%area()
        call bad%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon from mismatched arrays, vertex count ", bad%size()
    end subroutine scenario_sphere_polygon_size_mismatch

    !> A non-finite vertex names no position. The control writes its right ascensions far below 0,
    !! which is legal.
    subroutine scenario_sphere_polygon_nonfinite_vertex()
        use ieee_arithmetic, only: ieee_value, ieee_positive_inf
        type(pf_sky_polygon) :: far, bad
        call far%init([-400.0_real64, -390.0_real64, -390.0_real64], [0.0_real64, 0.0_real64, 10.0_real64])
        print '(a,es12.4)', "built a triangle written at ra -400 of area ", far%area()
        call bad%init([0.0_real64, ieee_value(0.0_real64, ieee_positive_inf), 10.0_real64], &
                      [0.0_real64, 0.0_real64, 10.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon with an infinite vertex, vertex count ", bad%size()
    end subroutine scenario_sphere_polygon_nonfinite_vertex

    !> The other two non-finite spellings, which the message has to render rather than propagate: a
    !> NaN right ascension and a MINUS-infinite declination on the same vertex, so one abort prints
    !> both. `sphere_polygon_nonfinite_vertex` above covers only plus infinity, and a renderer that
    !> handled that one alone would print a blank or a compiler-dependent word for these.
    subroutine scenario_sphere_polygon_nan_vertex()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_negative_inf
        type(pf_sky_polygon) :: bad
        call bad%init([0.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), 10.0_real64], &
                      [0.0_real64, ieee_value(0.0_real64, ieee_negative_inf), 10.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon with a NaN vertex, vertex count ", bad%size()
    end subroutine scenario_sphere_polygon_nan_vertex

    !> A vertex whose magnitude leaves the fixed-point range the message prints in. The renderer
    !> switches to an exponent form beyond 1e100 and below 1e-99, and both ends are on this vertex:
    !> a right ascension of 1e-200 and a declination of 1e200, which is what makes it out of range.
    subroutine scenario_sphere_polygon_extreme_vertex()
        type(pf_sky_polygon) :: bad
        call bad%init([0.0_real64, 1.0e-200_real64, 10.0_real64], &
                      [0.0_real64, 1.0e200_real64, 10.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon with a 1e200 declination, vertex count ", bad%size()
    end subroutine scenario_sphere_polygon_extreme_vertex

    !> `strict = .true.` refusing a band written the SHORT way round: an RA extent above 180 degrees
    !> whose vertices sit in two clusters, one within 90 degrees of each end. Such a polygon names
    !> the complement of what the caller meant, and the flag exists to say so rather than to measure
    !> the wrong region in silence. Next to a pole so the extent still fits inside one hemisphere.
    subroutine scenario_sphere_polygon_strict_short_way()
        type(pf_sky_polygon) :: taken, refused
        ! The control: the SAME vertices without strict=, which are taken as written.
        call taken%init([0.0_real64, 5.0_real64, 205.0_real64, 200.0_real64], &
                        [84.0_real64, 88.0_real64, 88.0_real64, 84.0_real64])
        print '(a,es12.4)', "the same vertices without strict= built an area of ", taken%area()
        call refused%init([0.0_real64, 5.0_real64, 205.0_real64, 200.0_real64], &
                          [84.0_real64, 88.0_real64, 88.0_real64, 84.0_real64], strict=.true.)   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a short-way band under strict=, area ", refused%area()
    end subroutine scenario_sphere_polygon_strict_short_way

    !> A declination beyond a pole is refused. The control is the whole sky, both poles included.
    subroutine scenario_sphere_polygon_dec_out_of_range()
        type(pf_sky_polygon) :: whole, bad
        call whole%init([0.0_real64, 360.0_real64, 360.0_real64, 0.0_real64], &
                        [-90.0_real64, -90.0_real64, 90.0_real64, 90.0_real64])
        print '(a,es12.4)', "built the whole sky, area ", whole%area()
        call bad%init([0.0_real64, 10.0_real64, 10.0_real64], [80.0_real64, 80.0_real64, 90.5_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon past the pole, vertex count ", bad%size()
    end subroutine scenario_sphere_polygon_dec_out_of_range

    !> The edge rule is one of two constants. The control uses the other one.
    subroutine scenario_sphere_polygon_bad_edge_rule()
        type(pf_sky_polygon) :: gc, bad
        call gc%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], &
                     [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64], PF_EDGE_GREAT_CIRCLE)
        print '(a,es12.4)', "built a great-circle quadrilateral of area ", gc%area()
        call bad%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], &
                      [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64], 2)   ! -> aborts
        print '(a,i0)', "unexpectedly built a polygon with edge rule ", bad%edges()
    end subroutine scenario_sphere_polygon_bad_edge_rule

    !> Vertices are read as written, so an RA extent above a turn is refused. The control spans exactly
    !! 360, a band round the sky.
    subroutine scenario_sphere_polygon_ra_extent_over_360()
        type(pf_sky_polygon) :: band, bad
        call band%init([0.0_real64, 360.0_real64, 360.0_real64, 0.0_real64], [-10.0_real64, -10.0_real64, 10.0_real64, 10.0_real64])
        print '(a,es12.4)', "built a band round the sky of area ", band%area()
        call bad%init([0.0_real64, 360.5_real64, 360.5_real64, 0.0_real64], &
                      [-10.0_real64, -10.0_real64, 10.0_real64, 10.0_real64])   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a polygon spanning 360.5 degrees, area ", bad%area()
    end subroutine scenario_sphere_polygon_ra_extent_over_360

    !> Great-circle edges need the polygon inside an open hemisphere. The control spans 160 degrees.
    subroutine scenario_sphere_polygon_not_in_hemisphere()
        type(pf_sky_polygon) :: wide, bad
        call wide%init([0.0_real64, 80.0_real64, 160.0_real64], [0.0_real64, 30.0_real64, 0.0_real64], PF_EDGE_GREAT_CIRCLE)
        print '(a,es12.4)', "built a great-circle triangle spanning 160 degrees, area ", wide%area()
        call bad%init([0.0_real64, 100.0_real64, 200.0_real64], [0.0_real64, 0.0_real64, 0.0_real64], &
                      PF_EDGE_GREAT_CIRCLE)   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a great-circle polygon spanning 200 degrees, area ", bad%area()
    end subroutine scenario_sphere_polygon_not_in_hemisphere

    !> Vertices whose unit vectors sum to exactly zero have no mean direction. The control is a small
    !! polygon at a pole.
    subroutine scenario_sphere_polygon_vertices_cancel()
        type(pf_sky_polygon) :: cap, bad
        call cap%init([0.0_real64, 90.0_real64, 180.0_real64, 270.0_real64], [80.0_real64, 81.0_real64, 82.0_real64, 83.0_real64], &
                      PF_EDGE_GREAT_CIRCLE)
        print '(a,es12.4)', "built a great-circle polygon about the pole, area ", cap%area()
        call bad%init([0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [90.0_real64, -90.0_real64, 90.0_real64, -90.0_real64], &
                      PF_EDGE_GREAT_CIRCLE)   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a polygon from cancelling directions, area ", bad%area()
    end subroutine scenario_sphere_polygon_vertices_cancel

    !> Every vertex at one declination is a polygon of no area. The control is a slanted triangle.
    subroutine scenario_sphere_polygon_zero_area()
        type(pf_sky_polygon) :: tri, flat
        call tri%init([0.0_real64, 10.0_real64, 20.0_real64], [10.0_real64, 11.0_real64, 10.0_real64])
        print '(a,es12.4)', "built a slanted triangle of area ", tri%area()
        call flat%init([0.0_real64, 10.0_real64, 20.0_real64], [10.0_real64, 10.0_real64, 10.0_real64])   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a flat polygon, area ", flat%area()
    end subroutine scenario_sphere_polygon_zero_area

    !> A thin diagonal sliver would take thousands of candidates a point. The control covers 5e-3 of
    !! its box.
    subroutine scenario_sphere_polygon_below_acceptance_floor()
        type(pf_sky_polygon) :: thin, sliver
        call thin%init([0.0_real64, 10.0_real64, 10.0_real64], [0.0_real64, 10.0_real64, 9.9_real64])
        print '(a,es12.4)', "built a thin triangle of acceptance ", thin%acceptance()
        call sliver%init([0.0_real64, 10.0_real64, 10.0_real64], [0.0_real64, 10.0_real64, 9.995_real64])   ! -> aborts
        print '(a,es12.4)', "unexpectedly built a sliver of acceptance ", sliver%acceptance()
    end subroutine scenario_sphere_polygon_below_acceptance_floor

    !> `%init` on a built polygon is refused. The control clears it first, which is legal.
    subroutine scenario_sphere_polygon_init_twice()
        type(pf_sky_polygon) :: poly
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call poly%clear()
        call poly%init([0.0_real64, 10.0_real64, 10.0_real64], [0.0_real64, 0.0_real64, 10.0_real64])
        print '(a,i0)', "built, cleared and rebuilt a polygon of vertex count ", poly%size()
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], &
                       [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly rebuilt a polygon without clearing it, vertex count ", poly%size()
    end subroutine scenario_sphere_polygon_init_twice

    !> `%contains` needs a built polygon. The control asks a built one.
    subroutine scenario_sphere_polygon_contains_before_init()
        type(pf_sky_polygon) :: built, poly
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,l1)', "a built polygon answers ", built%contains(20.0_real64, 0.0_real64)
        print '(a,l1)', "unexpectedly, an unbuilt polygon answers ", poly%contains(20.0_real64, 0.0_real64)   ! -> aborts
    end subroutine scenario_sphere_polygon_contains_before_init

    !> `%area` needs a built polygon. The control asks a built one.
    subroutine scenario_sphere_polygon_is_simple_before_init()
        type(pf_sky_polygon) :: built, poly
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,l1)', "a built polygon answers ", built%is_simple()
        print '(a,l1)', "unexpectedly, an unbuilt polygon answers ", poly%is_simple()   ! -> aborts
    end subroutine scenario_sphere_polygon_is_simple_before_init

    subroutine scenario_sphere_polygon_area_before_init()
        type(pf_sky_polygon) :: built, poly
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,es12.4)', "a built polygon answers ", built%area()
        print '(a,es12.4)', "unexpectedly, an unbuilt polygon answers ", poly%area()   ! -> aborts
    end subroutine scenario_sphere_polygon_area_before_init

    !> `%area_deg2` needs a built polygon. The control asks a built one.
    subroutine scenario_sphere_polygon_area_deg2_before_init()
        type(pf_sky_polygon) :: built, poly
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,es12.4)', "a built polygon answers ", built%area_deg2()
        print '(a,es12.4)', "unexpectedly, an unbuilt polygon answers ", poly%area_deg2()   ! -> aborts
    end subroutine scenario_sphere_polygon_area_deg2_before_init

    !> `%acceptance` needs a built polygon. The control asks a built one.
    subroutine scenario_sphere_polygon_acceptance_before_init()
        type(pf_sky_polygon) :: built, poly
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        print '(a,es12.4)', "a built polygon answers ", built%acceptance()
        print '(a,es12.4)', "unexpectedly, an unbuilt polygon answers ", poly%acceptance()   ! -> aborts
    end subroutine scenario_sphere_polygon_acceptance_before_init

    !> `%bounds` needs a built polygon. The control asks a built one.
    subroutine scenario_sphere_polygon_bounds_before_init()
        type(pf_sky_polygon) :: built, poly
        real(real64) :: ra_lo, ra_hi, dec_lo, dec_hi
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call built%bounds(ra_lo, ra_hi, dec_lo, dec_hi)
        print '(a,4f8.2)', "a built polygon's box is ", ra_lo, ra_hi, dec_lo, dec_hi
        call poly%bounds(ra_lo, ra_hi, dec_lo, dec_hi)   ! -> aborts
        print '(a,4f8.2)', "unexpectedly, an unbuilt polygon's box is ", ra_lo, ra_hi, dec_lo, dec_hi
    end subroutine scenario_sphere_polygon_bounds_before_init

    !> A draw needs a built polygon. The control draws from a built one.
    subroutine scenario_sphere_polygon_random_before_init()
        type(pf_sky_polygon) :: built, poly
        real(real64) :: ra, dec
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call built%random_at(1_int64, 1_int64, ra, dec)
        print '(a,2f10.4)', "drew from a built polygon: ", ra, dec
        call poly%random_at(1_int64, 1_int64, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly drew from an unbuilt polygon: ", ra, dec
    end subroutine scenario_sphere_polygon_random_before_init

    !> A fill needs a built polygon, through its own check, even when it would fill nothing. The
    !! control fills from a built one.
    subroutine scenario_sphere_polygon_fill_before_init()
        type(pf_sky_polygon) :: built, poly
        real(real64) :: ra(2), dec(2)
        call built%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call built%random_fill(1_int64, 1_int64, ra, dec)
        print '(a,2f10.4)', "filled from a built polygon, the last ", ra(2), dec(2)
        call poly%random_fill(1_int64, 1_int64, ra(1:0), dec(1:0))   ! -> aborts
        print '(a)', "unexpectedly filled nothing from an unbuilt polygon"
        print '(a,2f10.4)', "the arrays hold ", ra(1), dec(1)
    end subroutine scenario_sphere_polygon_fill_before_init

    !> A fill needs arrays of one size. The control fills two of size 3.
    subroutine scenario_sphere_polygon_fill_size_mismatch()
        type(pf_sky_polygon) :: poly
        real(real64) :: ra(3), dec(3), dec2(2)
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call poly%random_fill(1_int64, 1_int64, ra, dec)
        print '(a,2f10.4)', "filled three points, the last ", ra(3), dec(3)
        call poly%random_fill(1_int64, 1_int64, ra, dec2)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly filled arrays of sizes 3 and 2: ", ra(1), dec2(1)
    end subroutine scenario_sphere_polygon_fill_size_mismatch

    !> A fill's last draw must be representable. The control ends exactly on `huge(int64)`.
    subroutine scenario_sphere_polygon_fill_draw_overflow()
        type(pf_sky_polygon) :: poly
        real(real64) :: ra(2), dec(2)
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call poly%random_fill(1_int64, 1_int64, ra, dec, huge(1_int64) - 1_int64)
        print '(a,2f10.4)', "filled up to draw huge(int64), the last ", ra(2), dec(2)
        call poly%random_fill(1_int64, 1_int64, ra, dec, huge(1_int64))   ! -> aborts
        print '(a,2f10.4)', "unexpectedly filled past huge(int64): ", ra(2), dec(2)
    end subroutine scenario_sphere_polygon_fill_draw_overflow

    !> The candidate cap, reached through the test-only floor override. The control draws once with the
    !! hook clear; then the floor drops to 1e-13, a sliver covering about 5e-12 of its box is admitted,
    !! and its first draw misses 100000 times.
    subroutine scenario_sphere_polygon_candidate_cap_reached()
        type(pf_sky_polygon) :: rect, sliver
        real(real64) :: ra, dec
        call rect%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call rect%random_at(1_int64, 1_int64, ra, dec)
        print '(a,2f10.4)', "drew from a rectangle with the floor at its default: ", ra, dec
        call parquet_debug_set_sphere_acceptance_floor(1.0e-13_real64)
        call sliver%init([0.0_real64, 10.0_real64, 10.0_real64], [0.0_real64, 10.0_real64, 10.0_real64 - 1.0e-10_real64])
        print '(a,es12.4)', "admitted a sliver of acceptance ", sliver%acceptance()
        call sliver%random_at(1_int64, 1_int64, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly drew from the sliver: ", ra, dec
    end subroutine scenario_sphere_polygon_candidate_cap_reached

    !> A stream near the end of its 2**63 words cannot give a block. The control takes the last block
    !! this form admits.
    subroutine scenario_sphere_stream_exhausted()
        type(pf_sky_polygon) :: poly
        type(pf_random_stream) :: rng
        real(real64) :: ra, dec
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call rng%seed(1_int64, 1_int64)
        call rng%rewind(huge(1_int64) - 7_int64)
        call poly%random_next(rng, ra, dec)
        print '(a,2f10.4,a,i0)', "drew near the stream's end: ", ra, dec, ", now at word ", rng%position()
        call poly%random_next(rng, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly drew from an exhausted stream: ", ra, dec
    end subroutine scenario_sphere_stream_exhausted

    !> A pixel means nothing without a grid. The control draws on a built one.
    subroutine scenario_sphere_pixel_grid_not_built()
        type(pf_healpix_grid) :: built, grid
        real(real64) :: v(3)
        call built%init(8_int64, PF_HP_RING)
        v = pf_random_pixel_at(built, 1_int64, 1_int64, 5_int64)
        print '(a,3es12.4)', "drew in a pixel of a built grid: ", v
        v = pf_random_pixel_at(grid, 1_int64, 1_int64, 5_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew on an unbuilt grid: ", v
    end subroutine scenario_sphere_pixel_grid_not_built

    !> Above `nside = 2**24` a direction cannot name a pixel near a pole. The control draws at `2**24`.
    subroutine scenario_sphere_pixel_nside_over_limit()
        type(pf_healpix_grid) :: fine, finer
        real(real64) :: v(3)
        call fine%init(16777216_int64, PF_HP_NEST)
        v = pf_random_pixel_at(fine, 1_int64, 1_int64, 5_int64)
        print '(a,3es12.4)', "drew in a pixel at nside 2**24: ", v
        call finer%init(33554432_int64, PF_HP_NEST)
        v = pf_random_pixel_at(finer, 1_int64, 1_int64, 5_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew at nside 2**25: ", v
    end subroutine scenario_sphere_pixel_nside_over_limit

    !> The pixel index runs 0 to npix - 1. The control draws in the last pixel, through the elemental
    !! RA/Dec form.
    subroutine scenario_sphere_pixel_ipix_out_of_range()
        type(pf_healpix_grid) :: grid
        real(real64) :: ra, dec
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_pixel_radec_at(grid, 1_int64, 1_int64, 767_int32, ra, dec)
        print '(a,2f10.4)', "drew in the last pixel: ", ra, dec
        call pf_random_pixel_radec_at(grid, 1_int64, 1_int64, 768_int32, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly drew in pixel 768: ", ra, dec
    end subroutine scenario_sphere_pixel_ipix_out_of_range

    !> A mask of no pixels has no points. The control lists one.
    subroutine scenario_sphere_mask_empty_list()
        type(pf_healpix_grid) :: grid
        integer(int64) :: none(0)
        real(real64) :: v(3)
        call grid%init(8_int64, PF_HP_RING)
        v = pf_random_mask_at(grid, 1_int64, 1_int64, [5_int64])
        print '(a,3es12.4)', "drew over a one-pixel mask: ", v
        v = pf_random_mask_at(grid, 1_int64, 1_int64, none)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew over an empty mask: ", v
    end subroutine scenario_sphere_mask_empty_list

    !> A mask draw checks the entry it chooses, an `int64` list. The control lists the last pixel.
    subroutine scenario_sphere_mask_entry_out_of_range()
        type(pf_healpix_grid) :: grid
        real(real64) :: v(3)
        call grid%init(8_int64, PF_HP_RING)
        v = pf_random_mask_at(grid, 1_int64, 1_int64, [767_int64])
        print '(a,3es12.4)', "drew over the last pixel: ", v
        v = pf_random_mask_at(grid, 1_int64, 1_int64, [768_int64])   ! -> aborts
        print '(a,3es12.4)', "unexpectedly drew over pixel 768: ", v
    end subroutine scenario_sphere_mask_entry_out_of_range

    !> The `int32` list's own entry check, through the RA/Dec form. The control lists pixel 0.
    subroutine scenario_sphere_mask_entry_out_of_range_int32()
        type(pf_healpix_grid) :: grid
        real(real64) :: ra, dec
        call grid%init(8_int64, PF_HP_NEST)
        call pf_random_mask_radec_at(grid, 1_int64, 1_int32, [0_int32], ra, dec)
        print '(a,2f10.4)', "drew over pixel 0: ", ra, dec
        call pf_random_mask_radec_at(grid, 1_int64, 1_int32, [-1_int32], ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly drew over pixel -1: ", ra, dec
    end subroutine scenario_sphere_mask_entry_out_of_range_int32

    !> A mask fill needs three rows, for an `int64` list. The controls are a (3, 0) fill -- a no-op --
    !! and a (3, 2) one.
    subroutine scenario_sphere_fill_mask_bad_shape()
        type(pf_healpix_grid) :: grid
        real(real64) :: none(3, 0), good(3, 2), bad(2, 4)
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int64, 6_int64], none)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int64, 6_int64], good)
        print '(a,3es12.4)', "filled three rows: ", good(:, 2)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int64, 6_int64], bad)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly filled two rows: ", bad(:, 1)
    end subroutine scenario_sphere_fill_mask_bad_shape

    !> A mask RA/Dec fill needs arrays of one size, for an `int64` list. The control fills two of size
    !! 4.
    subroutine scenario_sphere_fill_mask_radec_size_mismatch()
        type(pf_healpix_grid) :: grid
        real(real64) :: ra(4), dec(4), dec3(3)
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, [5_int64, 6_int64], ra, dec)
        print '(a,2f10.4)', "filled four positions, the last ", ra(4), dec(4)
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, [5_int64, 6_int64], ra, dec3)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly filled arrays of sizes 4 and 3: ", ra(1), dec3(1)
    end subroutine scenario_sphere_fill_mask_radec_size_mismatch

    !> A mask fill needs three rows, for an `int32` list. The controls are a (3, 0) fill -- a no-op --
    !! and a (3, 2) one.
    subroutine scenario_sphere_fill_mask_bad_shape_int32()
        type(pf_healpix_grid) :: grid
        real(real64) :: none(3, 0), good(3, 2), bad(2, 4)
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int32, 6_int32], none)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int32, 6_int32], good)
        print '(a,3es12.4)', "filled three rows: ", good(:, 2)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int32, 6_int32], bad)   ! -> aborts
        print '(a,2es12.4)', "unexpectedly filled two rows: ", bad(:, 1)
    end subroutine scenario_sphere_fill_mask_bad_shape_int32

    !> A mask RA/Dec fill needs arrays of one size, for an `int32` list. The control fills two of size
    !! 4.
    subroutine scenario_sphere_fill_mask_radec_size_mismatch_int32()
        type(pf_healpix_grid) :: grid
        real(real64) :: ra(4), dec(4), dec3(3)
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, [5_int32, 6_int32], ra, dec)
        print '(a,2f10.4)', "filled four positions, the last ", ra(4), dec(4)
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, [5_int32, 6_int32], ra, dec3)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly filled arrays of sizes 4 and 3: ", ra(1), dec3(1)
    end subroutine scenario_sphere_fill_mask_radec_size_mismatch_int32

    !> A fill checks every entry before it draws, although a scalar draw would reach entry 1000 only
    !! when it chose it. The control is a scalar draw over the same list that chooses another entry.
    subroutine scenario_sphere_fill_mask_entry_out_of_range()
        type(pf_healpix_grid) :: grid
        integer(int32) :: list(1000)
        real(real64) :: v(3), fill(3, 1)
        integer :: k
        call grid%init(8_int64, PF_HP_RING)
        do k = 1, 999
            list(k) = int(modulo(k, 768), int32)
        end do
        list(1000) = 768_int32
        v = pf_random_mask_at(grid, 1_int64, 1_int64, list, 2_int64)
        print '(a,3es12.4)', "drew over the list, choosing a valid entry: ", v
        call pf_random_fill_mask(grid, 1_int64, 1_int64, list, fill, 2_int64)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly filled over a list holding pixel 768: ", fill(:, 1)
    end subroutine scenario_sphere_fill_mask_entry_out_of_range

    !> The `int64` list's whole-list check, through the RA/Dec fill. The control fills over the valid
    !! prefix.
    subroutine scenario_sphere_fill_mask_radec_entry_out_of_range()
        type(pf_healpix_grid) :: grid
        integer(int64) :: list(3)
        real(real64) :: ra(2), dec(2)
        call grid%init(8_int64, PF_HP_RING)
        list = [10_int64, 20_int64, -3_int64]
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, list(1:2), ra, dec)
        print '(a,2f10.4)', "filled over the valid prefix, the last ", ra(2), dec(2)
        call pf_random_fill_mask_radec(grid, 1_int64, 1_int64, list, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly filled over a list holding pixel -3: ", ra(1), dec(1)
    end subroutine scenario_sphere_fill_mask_radec_entry_out_of_range

    !> A mask fill's last draw must be representable. The control ends exactly on `huge(int64)`.
    subroutine scenario_sphere_fill_mask_draw_overflow()
        type(pf_healpix_grid) :: grid
        real(real64) :: v(3, 2)
        call grid%init(8_int64, PF_HP_RING)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int64, 6_int64], v, huge(1_int64) - 1_int64)
        print '(a,3es12.4)', "filled up to draw huge(int64), the last ", v(:, 2)
        call pf_random_fill_mask(grid, 1_int64, 1_int64, [5_int64, 6_int64], v, huge(1_int64))   ! -> aborts
        print '(a,3es12.4)', "unexpectedly filled past huge(int64): ", v(:, 2)
    end subroutine scenario_sphere_fill_mask_draw_overflow

    !> A centre's declination must lie in [-90, 90]. The control offsets from the pole itself.
    subroutine scenario_sphere_offset_dec_out_of_range()
        real(real64) :: ra, dec
        call pf_offset_radec(10.0_real64, 90.0_real64, 0.0_real64, 1.0_real64, ra, dec)
        print '(a,2f10.4)', "offset from the pole: ", ra, dec
        call pf_offset_radec(10.0_real64, 90.5_real64, 0.0_real64, 1.0_real64, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly offset from declination 90.5: ", ra, dec
    end subroutine scenario_sphere_offset_dec_out_of_range

    !> A separation is at least 0. The control offsets by 0.
    subroutine scenario_sphere_offset_negative_separation()
        real(real64) :: ra, dec
        call pf_offset_radec(10.0_real64, 20.0_real64, 30.0_real64, 0.0_real64, ra, dec)
        print '(a,2f10.4)', "offset by 0: ", ra, dec
        call pf_offset_radec(10.0_real64, 20.0_real64, 30.0_real64, -1.0_real64, ra, dec)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly offset by -1 degree: ", ra, dec
    end subroutine scenario_sphere_offset_negative_separation

    !> A conversion names two of the four systems, and the sentinel is not one of them -- not even
    !! from itself to itself, where the identity must not be taken before the selectors are read.
    !! The control converts ICRS to Galactic.
    subroutine scenario_skycoord_convert_unknown_system()
        real(real64) :: lon, lat
        call pf_sky_convert(10.0_real64, 20.0_real64, PF_COORD_ICRS, PF_COORD_GALACTIC, lon, lat)
        print '(a,2f10.4)', "converted ICRS to Galactic: ", lon, lat
        call pf_sky_convert(10.0_real64, 20.0_real64, PF_COORD_UNKNOWN, PF_COORD_UNKNOWN, lon, lat)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly converted out of the unknown system: ", lon, lat
    end subroutine scenario_skycoord_convert_unknown_system

    !> Only a selector has a token. The control spells the sentinel, which has one.
    subroutine scenario_skycoord_system_name_not_a_selector()
        character(len=:), allocatable :: name
        call pf_coord_system_name(PF_COORD_UNKNOWN, name)
        print '(a,a)', "the sentinel's token: ", name
        call pf_coord_system_name(7, name)   ! -> aborts
        print '(a,a)', "unexpectedly named system 7: ", name
    end subroutine scenario_skycoord_system_name_not_a_selector

    !> A writer's precision is at most nine decimals of an arcsecond, which with the right ascension's
    !! one more fills the widest text it writes. The control writes at nine.
    subroutine scenario_skycoord_radec2str_width_overflow()
        character(len=:), allocatable :: text
        call pf_radec2str(187.5_real64, -12.5_real64, text, precision=9)
        print '(a,a)', "wrote at precision 9: ", text
        call pf_radec2str(187.5_real64, -12.5_real64, text, precision=10)   ! -> aborts
        print '(a,a)', "unexpectedly wrote at precision 10: ", text
    end subroutine scenario_skycoord_radec2str_width_overflow

    !> A writer's separator is a colon, a blank or the letters. The control writes the letters.
    subroutine scenario_skycoord_text_bad_separator()
        character(len=:), allocatable :: text
        call pf_dec2str(-12.5_real64, text, sep="hms")
        print '(a,a)', "wrote with the letters: ", text
        call pf_dec2str(-12.5_real64, text, sep="/")   ! -> aborts
        print '(a,a)', "unexpectedly wrote with a slash: ", text
    end subroutine scenario_skycoord_text_bad_separator

    !> The position's system is one of the four, as for a conversion. The control names Galactic.
    !! The result is printed each time: the procedure is `pure`, and an unused call may be deleted.
    subroutine scenario_skycoord_text_precision_negative()
        character(len=:), allocatable :: text
        call pf_dec2str(-12.5_real64, text, precision=0)
        print '(a,a)', "wrote at precision 0: ", text
        call pf_dec2str(-12.5_real64, text, precision=-1)   ! -> aborts
        print '(a,a)', "unexpectedly wrote at precision -1: ", text
    end subroutine scenario_skycoord_text_precision_negative

    subroutine scenario_skycoord_zcmb_unknown_system()
        real(real64) :: z
        z = pf_zhel2zcmb(10.0_real64, 20.0_real64, 0.1_real64, PF_COORD_GALACTIC)
        print '(a,es24.16)', "the redshift of a Galactic position: ", z
        z = pf_zhel2zcmb(10.0_real64, 20.0_real64, 0.1_real64, PF_COORD_UNKNOWN)   ! -> aborts
        print '(a,es24.16)', "unexpectedly gave a redshift in the unknown system: ", z
    end subroutine scenario_skycoord_zcmb_unknown_system

    !> A rotation object answers only once `%init` has named its two systems. The control prepares one
    !! and applies it; a fresh one is then applied. The outputs are printed each time: the binding is
    !! `pure`, and a call whose results nobody reads may be deleted.
    subroutine scenario_skycoord_rotation_apply_before_init()
        type(pf_sky_rotation) :: rot, fresh
        real(real64) :: lon, lat
        call rot%init(PF_COORD_ICRS, PF_COORD_GALACTIC)
        call rot%apply(10.0_real64, 20.0_real64, lon, lat)
        print '(a,2f10.4)', "rotated ICRS to Galactic: ", lon, lat
        call fresh%apply(10.0_real64, 20.0_real64, lon, lat)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly rotated with no %init: ", lon, lat
    end subroutine scenario_skycoord_rotation_apply_before_init

    !> A rotation object is prepared between two of the five systems, as a conversion is. The control
    !! prepares FK5 J2000 to supergalactic.
    subroutine scenario_skycoord_rotation_init_unknown_system()
        type(pf_sky_rotation) :: rot
        real(real64) :: lon, lat
        call rot%init(PF_COORD_FK5, PF_COORD_SUPERGALACTIC)
        call rot%apply(10.0_real64, 20.0_real64, lon, lat)
        print '(a,2f10.4)', "rotated FK5 to supergalactic: ", lon, lat
        call rot%init(PF_COORD_GALACTIC, 6)   ! -> aborts
        call rot%apply(10.0_real64, 20.0_real64, lon, lat)
        print '(a,2f10.4)', "unexpectedly prepared a rotation into system 6: ", lon, lat
    end subroutine scenario_skycoord_rotation_init_unknown_system

    !> A proper motion moves a position on the sphere, whose declination is an offset's centre. The
    !! control moves a position away from the north pole itself.
    subroutine scenario_skycoord_apply_pm_dec_out_of_range()
        real(real64) :: ra, dec
        call pf_apply_pm(10.0_real64, 90.0_real64, 100.0_real64, 0.0_real64, 10.0_real64, ra, dec)
        print '(a,2f14.8)', "moved from the pole: ", ra, dec
        call pf_apply_pm(10.0_real64, 90.5_real64, 100.0_real64, 0.0_real64, 10.0_real64, ra, dec)   ! -> aborts
        print '(a,2f14.8)', "unexpectedly moved from beyond the pole: ", ra, dec
    end subroutine scenario_skycoord_apply_pm_dec_out_of_range

    !> A tangent point beyond a pole: `pf_radec2tan` refuses it, as `pf_offset_radec`'s centre is
    !! refused, because a centre past a pole mirrors the local north and east and charts a
    !! plausible wrong field.
    subroutine scenario_skycoord_radec2tan_dec0_out_of_range()
        real(real64) :: x, y
        call pf_radec2tan(11.0_real64, 21.0_real64, 10.0_real64, 90.0_real64, x, y)
        print '(a,2f14.8)', "projected about the pole: ", x, y
        call pf_radec2tan(11.0_real64, 21.0_real64, 10.0_real64, 90.5_real64, x, y)   ! -> aborts
        print '(a,2f14.8)', "unexpectedly projected about a centre beyond the pole: ", x, y
    end subroutine scenario_skycoord_radec2tan_dec0_out_of_range

    !> The same centre, refused by the inverse in its own name.
    subroutine scenario_skycoord_tan2radec_dec0_out_of_range()
        real(real64) :: ra, dec
        call pf_tan2radec(0.5_real64, 0.25_real64, 10.0_real64, -90.0_real64, ra, dec)
        print '(a,2f14.8)', "unprojected about the south pole: ", ra, dec
        call pf_tan2radec(0.5_real64, 0.25_real64, 10.0_real64, -90.5_real64, ra, dec)   ! -> aborts
        print '(a,2f14.8)', "unexpectedly unprojected about a centre beyond the pole: ", ra, dec
    end subroutine scenario_skycoord_tan2radec_dec0_out_of_range

    !> `pf_zcmb2zhel` refuses a selector that is not a system, in its own name, as `pf_zhel2zcmb`
    !! does: the message says which procedure the caller called.
    subroutine scenario_skycoord_zcmb2zhel_unknown_system()
        real(real64) :: z
        z = pf_zcmb2zhel(10.0_real64, 20.0_real64, 0.1_real64, PF_COORD_GALACTIC)
        print '(a,es24.16)', "the heliocentric redshift of a Galactic position: ", z
        z = pf_zcmb2zhel(10.0_real64, 20.0_real64, 0.1_real64, PF_COORD_UNKNOWN)   ! -> aborts
        print '(a,es24.16)', "unexpectedly gave a redshift in the unknown system: ", z
    end subroutine scenario_skycoord_zcmb2zhel_unknown_system

    !> A grid of no points has no answer. The control is a grid of one.
    subroutine scenario_sphere_fibonacci_n_not_positive()
        real(real64) :: one(3, 1), none(3, 0)
        call pf_fibonacci_grid(1_int32, one)
        print '(a,3es12.4)', "built a one-point grid: ", one(:, 1)
        call pf_fibonacci_grid(0_int32, none)   ! -> aborts
        print '(a,i0)', "unexpectedly built a grid of no points, columns ", size(none, 2)
    end subroutine scenario_sphere_fibonacci_n_not_positive

    !> The array must hold exactly `n` columns of three. The control fills four into four.
    subroutine scenario_sphere_fibonacci_bad_shape()
        real(real64) :: vec(3, 4)
        call pf_fibonacci_grid(4_int64, vec)
        print '(a,3es12.4)', "built a four-point grid, the last ", vec(:, 4)
        call pf_fibonacci_grid(5_int64, vec)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly built a five-point grid into four columns: ", vec(:, 4)
    end subroutine scenario_sphere_fibonacci_bad_shape

    !> The frame is one of two selectors. The control uses the mirrored one.
    subroutine scenario_sphere_fibonacci_bad_frame()
        real(real64) :: vec(3, 3)
        call pf_fibonacci_grid(3_int64, vec, frame=PF_HP_DEC_SOUTH)
        print '(a,3es12.4)', "built a grid in the mirrored frame, the first ", vec(:, 1)
        call pf_fibonacci_grid(3_int64, vec, frame=2)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly built a grid in frame 2: ", vec(:, 1)
    end subroutine scenario_sphere_fibonacci_bad_frame

    !> Both arrays must be sized `n`. The control fills three into three.
    subroutine scenario_sphere_fibonacci_radec_bad_size()
        real(real64) :: ra(3), dec(3), dec2(2)
        call pf_fibonacci_grid_radec(3_int32, ra, dec)
        print '(a,2f10.4)', "built a three-point grid, the last ", ra(3), dec(3)
        call pf_fibonacci_grid_radec(3_int32, ra, dec2)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly built a grid into a short declination array: ", ra(1), dec2(1)
    end subroutine scenario_sphere_fibonacci_radec_bad_size

    !> The frame is one of two selectors. The control names the standard one.
    subroutine scenario_sphere_radec2vec_bad_frame()
        real(real64) :: v(3)
        call pf_radec2vec(10.0_real64, 20.0_real64, v, frame=PF_HP_DEC_NORTH)
        print '(a,3es12.4)', "converted in the standard frame: ", v
        call pf_radec2vec(10.0_real64, 20.0_real64, v, frame=-1)   ! -> aborts
        print '(a,3es12.4)', "unexpectedly converted in frame -1: ", v
    end subroutine scenario_sphere_radec2vec_bad_frame

    !> The frame is one of two selectors. The control names the mirrored one.
    subroutine scenario_sphere_vec2radec_bad_frame()
        real(real64) :: ra, dec
        ! Named for `pf_vec2radec`'s explicit-shape `vec(3)` dummy, as the sphere scenarios above.
        real(real64), parameter :: DIAG(3) = [1.0_real64, 1.0_real64, 1.0_real64]
        call pf_vec2radec(DIAG, ra, dec, frame=PF_HP_DEC_SOUTH)
        print '(a,2f10.4)', "converted in the mirrored frame: ", ra, dec
        call pf_vec2radec(DIAG, ra, dec, frame=7)   ! -> aborts
        print '(a,2f10.4)', "unexpectedly converted in frame 7: ", ra, dec
    end subroutine scenario_sphere_vec2radec_bad_frame

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

    ! ---- parquet_spatial ----

    !> A deterministic cloud for the spatial scenarios below.
    subroutine spatial_cloud(n, x, y, z)
        integer, intent(in) :: n !! how many points.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        integer :: i

        allocate (x(n), y(n), z(n))
        do i = 1, n
            x(i) = pf_random_at(1234_int64, i, 1_int64)
            y(i) = pf_random_at(1234_int64, i, 2_int64)
            z(i) = pf_random_at(1234_int64, i, 3_int64)
        end do
    end subroutine spatial_cloud

    !> Querying an index that has never been built.
    subroutine scenario_spatial_query_before_build()
        type(pf_spatial_index) :: sx
        integer(int64) :: got(4), m

        m = sx%within([0.0_real64, 0.0_real64, 0.0_real64], 1.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly queried an unbuilt index, m=", m
    end subroutine scenario_spatial_query_before_build

    !> Coordinate arrays of different lengths.
    subroutine scenario_spatial_length_mismatch()
        type(pf_spatial_index) :: sx
        real(real64) :: x(10), y(9), z(10)

        x = 0.0_real64
        y = 0.0_real64
        z = 0.0_real64
        call sx%build(x, y, z, radius=1.0_real64)   ! -> aborts
        print '(a)', "unexpectedly built an index from mismatched coordinate arrays"
    end subroutine scenario_spatial_length_mismatch

    !> A radius hint of zero. The hint is mandatory precisely so that the cell size is never chosen
    !> in the dark, and a zero would make the cost model meaningless rather than merely coarse.
    subroutine scenario_spatial_radius_not_positive()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.0_real64)   ! -> aborts
        print '(a)', "unexpectedly accepted radius= 0"
    end subroutine scenario_spatial_radius_not_positive

    !
    ! ---- Non-finite coordinates, points and radii ----
    !
    ! Eight scenarios for one rule: nothing the grid has to PLACE may be a NaN or an infinity.
    ! The index computes a cell as `int((v - lo) * inv)` and clamps its walk with `min`/`max`, and
    ! both `cvttsd2si` and `minsd`/`maxsd` signal on a quiet NaN -- so under nagfor's default
    ! `-ieee=stop` every one of these aborted with "Arithmetic exception: Floating invalid
    ! operation", naming neither the entry point nor the argument, and only in an optimised build.
    ! Every other compiler in the fleet masks the traps and answers from a garbage cell index.
    !
    ! There is one check (`spatial_check_finite`) and eight call sites, which is why there are
    ! eight scenarios: the realistic future defect is a dropped call site, not a broken check.
    ! Their NEGATIVE CONTROL is the whole `spatial` suite -- 74 tests that build, rebuild and
    ! query with finite data -- so a check that fired unconditionally could not reach here.

    !> %build with a NaN coordinate.
    subroutine scenario_spatial_build_nan_coord()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        x(7) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sx%build(x, y, z, radius=0.2_real64)   ! -> aborts
        print '(a)', "unexpectedly built an index over a NaN coordinate"
    end subroutine scenario_spatial_build_nan_coord

    !> %build with an INFINITE coordinate: the same check, the other half of "finite".
    subroutine scenario_spatial_build_inf_coord()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        z(11) = ieee_value(1.0_real64, ieee_positive_inf)
        call sx%build(x, y, z, radius=0.2_real64)   ! -> aborts
        print '(a)', "unexpectedly built an index over an infinite coordinate"
    end subroutine scenario_spatial_build_inf_coord

    !> A NaN `radius=`. Distinct from scenario_spatial_radius_not_positive above: the check used to
    !! be `any(radii <= 0)`, and a NaN answers .false. to that comparison as it does to every
    !! other, so this input walked straight past the guard that exists to catch it.
    subroutine scenario_spatial_build_nan_radius()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a)', "unexpectedly accepted a NaN radius="
    end subroutine scenario_spatial_build_nan_radius

    !> %build_sky with a NaN `ra`. `dec` was already screened by its [-90, 90] range test; `ra` has
    !! no range to be in, so it needs a check of its own -- and it cannot be left to the walk,
    !! because an infinite `ra` raises inside `cos` before any of this reaches the grid.
    subroutine scenario_spatial_build_sky_nan_ra()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64) :: ra(32), dec(32)
        integer :: i

        do i = 1, 32
            ra(i) = real(i, real64) * 5.0_real64
            dec(i) = real(i, real64) * 0.5_real64
        end do
        ra(9) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sx%build_sky(ra, dec, radius_deg=1.5_real64)   ! -> aborts
        print '(a)', "unexpectedly built a sky index over a NaN ra"
    end subroutine scenario_spatial_build_sky_nan_ra

    !> %rebuild with a NaN coordinate: the same data reaching the same grid by the other door.
    subroutine scenario_spatial_rebuild_nan_coord()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        y(4) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sx%rebuild(x, y, z)   ! -> aborts
        print '(a)', "unexpectedly rebuilt an index over a NaN coordinate"
    end subroutine scenario_spatial_rebuild_nan_coord

    !> %within with a NaN query point.
    subroutine scenario_spatial_query_nan_point()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: nan
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        m = sx%within([nan, 0.5_real64, 0.5_real64], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly searched about a NaN point, m=", m
    end subroutine scenario_spatial_query_nan_point

    !> %within_segment with a NaN axis endpoint: the axis-shaped queries have their own choke
    !! point, so a check on the ball search's would not cover them.
    subroutine scenario_spatial_segment_nan_endpoint()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: nan
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        m = sx%within_segment([0.1_real64, 0.1_real64, 0.1_real64], &
            [0.9_real64, 0.9_real64, nan], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly searched about a NaN axis endpoint, m=", m
    end subroutine scenario_spatial_segment_nan_endpoint

    !> An axis so long that its squared length is not representable.
    !>
    !> Both endpoints are finite, so the finite screens pass; `|p2 - p1|**2` is what overflows.
    !> Without the guard every parameter along the axis comes out zero and the capsule answers as
    !> a ball about `p1` -- a wrong answer with nothing to show for it.
    subroutine scenario_spatial_axis_length_overflows()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        ! The negative control: an axis long enough to be unusual but whose square still fits.
        m = sx%within_segment([0.0_real64, 0.5_real64, 0.5_real64], &
            [1.0e150_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)
        print '(a,i0)', "a 1e150-long axis was answered, m=", m
        ! And one past `2**511`, whose squared length is formed at a power-of-two scale and fits.
        m = sx%within_segment([0.0_real64, 0.5_real64, 0.5_real64], &
            [1.0e154_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)
        print '(a,i0)', "a 1e154-long axis was answered, m=", m
        m = sx%within_segment([0.0_real64, 0.5_real64, 0.5_real64], &
            [1.0e200_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly searched about an axis whose squared length overflows, m=", m
    end subroutine scenario_spatial_axis_length_overflows

    !> %nearest with a NaN query point: the expanding-ball search is the third choke point.
    subroutine scenario_spatial_nearest_nan_point()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: nan
        integer(int64) :: got(4), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        m = sx%nearest([0.5_real64, nan, 0.5_real64], 3_int64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly found nearest neighbours of a NaN point, m=", m
    end subroutine scenario_spatial_nearest_nan_point

    !> %within_sky with a NaN dec. Screened at the entry point rather than on the vector it builds,
    !! because an infinite argument raises inside the conversion itself.
    subroutine scenario_spatial_sky_query_nan_dec()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64) :: ra(32), dec(32)
        integer(int64) :: got(8), m
        integer :: i

        do i = 1, 32
            ra(i) = real(i, real64) * 5.0_real64
            dec(i) = real(i, real64) * 0.5_real64
        end do
        call sx%build_sky(ra, dec, radius_deg=1.5_real64)
        m = sx%within_sky(10.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 1.5_real64, got)
        print '(a,i0)', "unexpectedly searched about a NaN dec, m=", m
    end subroutine scenario_spatial_sky_query_nan_dec

    !> A NaN in a per-point `radius=` array for a BULK query. The bulk path has its own radius
    !! guard, written `any(radii < 0)`, which a NaN walks past exactly as %build's did -- and the
    !! very next statements are `minval(radii)` and the tuner, both of which trap on one.
    subroutine scenario_spatial_bulk_nan_radius()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), radii(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        allocate(radii(64))
        radii = 0.2_real64
        radii(5) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sx%count_all_within(radii, counts)   ! -> aborts
        print '(a)', "unexpectedly ran a bulk query with a NaN radius"
    end subroutine scenario_spatial_bulk_nan_radius

    !> A NaN in a bulk query's INNER radius. Its own guard, one statement below the outer one and
    !! of the same shape, so it went the same way; the message names it separately.
    subroutine scenario_spatial_bulk_nan_inner_radius()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%count_all_within(0.2_real64, counts, &
            r_inner=ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a)', "unexpectedly ran a bulk query with a NaN inner radius"
    end subroutine scenario_spatial_bulk_nan_inner_radius

    !> A NaN radius handed to %rebuild_for, whose guard had the same shape.
    subroutine scenario_spatial_rebuild_for_nan_radius()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%rebuild_for(ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a)', "unexpectedly retuned for a NaN radius"
    end subroutine scenario_spatial_rebuild_for_nan_radius

    !> Half a periodic box. Periodicity is a property of the box, so it needs both corners.
    subroutine scenario_spatial_box_needs_both()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.1_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64])   ! -> aborts
        print '(a)', "unexpectedly accepted box_lo= without box_hi="
    end subroutine scenario_spatial_box_needs_both

    !> A periodic search radius above half the box.
    !>
    !> **Not inaccuracy but ill-definition**: beyond `L/2` a point can be its own neighbour through
    !> two images, so there is no answer to give. This is the one periodic guard that survives the
    !> "wrap silently" rule, because it is not about bad input.
    subroutine scenario_spatial_radius_exceeds_half_box()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.75_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])   ! -> aborts
        print '(a)', "unexpectedly accepted a periodic radius above half the box"
    end subroutine scenario_spatial_radius_exceeds_half_box

    !> A 2D index queried with a three-coordinate point.
    subroutine scenario_spatial_query_rank_mismatch()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, radius=0.2_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly queried a 2D index with a 3D point, m=", m
    end subroutine scenario_spatial_query_rank_mismatch

    !> An axis-shaped query on a PERIODIC index is refused rather than approximated.
    !>
    !> **When this refusal lifts, this becomes an equality test, not a deletion**: assert that the
    !> capsule agrees with a minimum-image brute-force scan, with the same translation-invariance
    !> fixture the ball search uses. What it asserts today is that the refusal is real.
    subroutine scenario_spatial_axis_on_periodic()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], &
            [0.8_real64, 0.8_real64, 0.8_real64], 0.1_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly ran an axis query on a periodic index, m=", m
    end subroutine scenario_spatial_axis_on_periodic

    !> An axis-shaped query before `%build` is refused, naming the query that was attempted.
    subroutine scenario_spatial_axis_before_build()
        type(pf_spatial_index) :: sx
        integer(int64) :: got(8), m

        m = sx%within_cone([0.0_real64, 0.0_real64, 0.0_real64], &
            [1.0_real64, 0.0_real64, 0.0_real64], 0.1_real64, 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly queried an unbuilt index, m=", m
    end subroutine scenario_spatial_axis_before_build

    !> A negative radius on an axis query is refused, as it is on the ball search.
    subroutine scenario_spatial_axis_radius_negative()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        ! The FAR radius is the negative one, so this also pins that both are checked.
        m = sx%within_cone([0.2_real64, 0.2_real64, 0.2_real64], &
            [0.8_real64, 0.8_real64, 0.8_real64], 0.1_real64, -0.1_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative cone radius, m=", m
    end subroutine scenario_spatial_axis_radius_negative

    !> A deterministic sky catalogue for the scenarios below.
    !> An inner radius above the outer one is a shape with nothing in it, not an empty answer.
    subroutine scenario_spatial_annulus_inner_exceeds_outer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got, r_inner=0.5_real64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an inner radius above the outer one, m=", m
    end subroutine scenario_spatial_annulus_inner_exceeds_outer

    !> A negative inner radius.
    subroutine scenario_spatial_annulus_inner_negative()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got, r_inner=-0.1_real64)  ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative inner radius, m=", m
    end subroutine scenario_spatial_annulus_inner_negative

    !> A bulk inner-radius vector that is neither one value nor one per point.
    subroutine scenario_spatial_bulk_inner_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%count_all_within(spread(0.2_real64, 1, 64), counts, &
            r_inner=[0.1_real64, 0.1_real64, 0.1_real64])                                          ! -> aborts
        print '(a,i0)', "unexpectedly accepted a three-entry inner radius for 64 points, n=", size(counts)
    end subroutine scenario_spatial_bulk_inner_length

    !> A scalar inner radius above the SMALLEST of a per-point outer radius vector.
    subroutine scenario_spatial_bulk_inner_exceeds_outer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), radii(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        allocate (radii(64))
        radii = 0.3_real64
        radii(17) = 0.05_real64
        call sx%build(x, y, z, radius=radii)
        call sx%count_all_within(radii, counts, r_inner=[0.1_real64])                              ! -> aborts
        print '(a,i0)', "unexpectedly accepted an inner radius above one point's outer one, n=", size(counts)
    end subroutine scenario_spatial_bulk_inner_exceeds_outer

    !> An inner angular radius above the outer one.
    subroutine scenario_spatial_sky_annulus_too_large()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64) :: got(8), m

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=5.0_real64)
        m = sx%within_sky(10.0_real64, 10.0_real64, 5.0_real64, got, r_inner_deg=6.0_real64)       ! -> aborts
        print '(a,i0)', "unexpectedly accepted an inner angular radius above the outer one, m=", m
    end subroutine scenario_spatial_sky_annulus_too_large

    !> An `axis_point` buffer whose first extent is not the index's rank.
    subroutine scenario_spatial_axis_point_rank()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: ap(2, 8)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], &
            [0.8_real64, 0.8_real64, 0.8_real64], 0.1_real64, got, axis_point=ap)                  ! -> aborts
        print '(a,i0)', "unexpectedly accepted a two-row axis_point on a 3D index, m=", m
    end subroutine scenario_spatial_axis_point_rank

    !> `%nearest` with k below one.
    subroutine scenario_spatial_nearest_k_below_one()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], 0_int32, got)                          ! -> aborts
        print '(a,i0)', "unexpectedly accepted k = 0, m=", m
    end subroutine scenario_spatial_nearest_k_below_one

    !> A periodic index asked for more neighbours than half the box can hold.
    subroutine scenario_spatial_nearest_periodic_unreachable()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(64), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        ! The inscribed sphere is about 52% of the box, so 63 of 64 points cannot be reached
        ! without a radius past L/2 -- where a periodic ball is undefined rather than imprecise.
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], 63_int32, got)                         ! -> aborts
        print '(a,i0)', "unexpectedly answered a periodic nearest past half the box, m=", m
    end subroutine scenario_spatial_nearest_periodic_unreachable

    !> `%nearest_sky` on a Euclidean index.
    subroutine scenario_spatial_nearest_sky_on_euclidean()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%nearest_sky(10.0_real64, 10.0_real64, 3_int32, got)                                  ! -> aborts
        print '(a,i0)', "unexpectedly answered a sky nearest on a Euclidean index, m=", m
    end subroutine scenario_spatial_nearest_sky_on_euclidean

    !> `%kth_distance` with k below one.
    subroutine scenario_spatial_kth_k_below_one()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%kth_distance(0_int32, d)                                                            ! -> aborts
        print '(a,i0)', "unexpectedly accepted k = 0 for kth_distance, n=", size(d)
    end subroutine scenario_spatial_kth_k_below_one

    !> `%kth_distance` with k at the catalogue size, where no point has that many OTHERS.
    subroutine scenario_spatial_kth_k_too_large()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%kth_distance(64_int32, d)                                                           ! -> aborts
        print '(a,i0)', "unexpectedly accepted k = n for kth_distance, n=", size(d)
    end subroutine scenario_spatial_kth_k_too_large

    !> `%kth_distance` on a sky index, which would answer in chords to a caller reading degrees.
    subroutine scenario_spatial_kth_on_sky_index()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:), d(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=5.0_real64)
        call sx%kth_distance(2_int32, d)                                                            ! -> aborts
        print '(a,i0)', "unexpectedly answered kth_distance in chords on a sky index, n=", size(d)
    end subroutine scenario_spatial_kth_on_sky_index

    !> `%kth_distance_sky` on a Euclidean index.
    subroutine scenario_spatial_kth_sky_on_euclidean()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%kth_distance_sky(2_int32, d)                                                        ! -> aborts
        print '(a,i0)', "unexpectedly answered kth_distance_sky on a Euclidean index, n=", size(d)
    end subroutine scenario_spatial_kth_sky_on_euclidean

    !> Two endpoint arrays of different lengths.
    subroutine scenario_spatial_components_length()
        integer(int64), allocatable :: labels(:)

        call pf_connected_components([1_int64, 2_int64, 3_int64], [2_int64, 3_int64], 4_int64, labels)
        print '(a,i0)', "unexpectedly accepted mismatched endpoint arrays, n=", size(labels)
    end subroutine scenario_spatial_components_length

    !> An edge naming a vertex the graph does not have.
    subroutine scenario_spatial_components_endpoint_range()
        integer(int64), allocatable :: labels(:)

        call pf_connected_components([1_int64, 2_int64], [2_int64, 9_int64], 4_int64, labels)
        print '(a,i0)', "unexpectedly accepted an out-of-range edge endpoint, n=", size(labels)
    end subroutine scenario_spatial_components_endpoint_range

    !> `min_size = 0`, which would label a component with no vertices in it.
    subroutine scenario_spatial_components_min_size_zero()
        integer(int64), allocatable :: labels(:)

        call pf_connected_components([1_int64], [2_int64], 4_int64, labels, min_size=0)
        print '(a,i0)', "unexpectedly accepted min_size = 0, n=", size(labels)
    end subroutine scenario_spatial_components_min_size_zero

    !> A negative vertex count.
    subroutine scenario_spatial_components_nvert_negative()
        integer(int64), allocatable :: labels(:)

        call pf_connected_components([1_int64], [2_int64], -3_int64, labels)
        print '(a,i0)', "unexpectedly accepted a negative vertex count, n=", size(labels)
    end subroutine scenario_spatial_components_nvert_negative

    !> `%within_sky` on a Euclidean index is refused rather than answered in the wrong units.
    subroutine scenario_spatial_sky_query_on_euclidean()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within_sky(10.0_real64, 20.0_real64, 1.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a sky query on a Euclidean index, m=", m
    end subroutine scenario_spatial_sky_query_on_euclidean

    !> A Euclidean query on a sky index is refused: it would answer in chords, not degrees.
    subroutine scenario_spatial_euclidean_query_on_sky()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64) :: got(8), m

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        m = sx%within([1.0_real64, 0.0_real64, 0.0_real64], 0.02_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a Euclidean query on a sky index, m=", m
    end subroutine scenario_spatial_euclidean_query_on_sky

    !> A PLAIN bulk sweep on a sky index is refused, and points at the `_sky` forms.
    !>
    !> The bulk sky forms have landed, so this no longer guards an absent feature -- it guards the
    !> units. `%all_within(0.02)` on a sky index would be answered in chords to a caller who is
    !> almost certainly thinking in degrees, and 0.02 chords is about 1.15 degrees: a plausible
    !> number, wrong by a factor of 57. The companion is `spatial_sky_bulk_on_euclidean`, which
    !> checks the same guard from the other side.
    subroutine scenario_spatial_sky_bulk_refused()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: offs(:), nb(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        call sx%all_within(0.02_real64, offs, nb)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a bulk sweep on a sky index, n=", size(nb)
    end subroutine scenario_spatial_sky_bulk_refused

    !> A `_sky` bulk form on a Euclidean index is refused: it would take degrees for a distance.
    subroutine scenario_spatial_sky_bulk_on_euclidean()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%count_all_within_sky(1.0_real64, counts)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a sky bulk sweep on a Euclidean index, n=", size(counts)
    end subroutine scenario_spatial_sky_bulk_on_euclidean

    !> An angular radius past a hemisphere is refused, since the grid then prunes nothing.
    subroutine scenario_spatial_sky_rsky_too_large()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64) :: got(8), m

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        m = sx%within_sky(10.0_real64, 20.0_real64, 120.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a 120-degree sky radius, m=", m
    end subroutine scenario_spatial_sky_rsky_too_large

    !> `%rebuild_for` on a sky index takes DEGREES, so it inherits the 90-degree ceiling.
    !>
    !> The ceiling arrives with the shared conversion rather than being restated here, which is the
    !> point of doing the conversion in the binding: a re-tune radius past a hemisphere is as
    !> meaningless as a build radius past one, and nothing had to be written twice to say so.
    subroutine scenario_spatial_rebuild_for_sky_too_large()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        call sx%rebuild_for(120.0_real64)   ! -> aborts
        print '(a,f0.3)', "unexpectedly re-tuned for a 120-degree radius, eff=", sx%effective_radius()
    end subroutine scenario_spatial_rebuild_for_sky_too_large

    !> `backend=` naming neither of the two constants is refused rather than defaulted.
    subroutine scenario_spatial_sky_bad_backend()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64, backend=7)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an unknown backend, backend=", sx%backend()
    end subroutine scenario_spatial_sky_bad_backend

    !> `cell=` describes the 3D grid and means nothing for a pixelisation, so it is refused.
    subroutine scenario_spatial_sky_cell_with_healpix()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX, &
                          cell=0.05_real64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted cell= on a HEALPix index, nside=", sx%nside()
    end subroutine scenario_spatial_sky_cell_with_healpix

    !> `nside=` on a 3D-grid index is refused rather than ignored, which is the direction that
    !> matters: a silently ignored resolution would leave the caller believing they had tuned it.
    subroutine scenario_spatial_sky_nside_without_healpix()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64, nside=16_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted nside= on a 3D-grid index, backend=", sx%backend()
    end subroutine scenario_spatial_sky_nside_without_healpix

    !> A resolution that is not a power of two is not a HEALPix resolution at all.
    subroutine scenario_spatial_sky_nside_not_power2()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX, &
                          nside=12_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted nside=12, nside=", sx%nside()
    end subroutine scenario_spatial_sky_nside_not_power2

    !> Zero is caught by the range half of the same guard, before the power-of-two half --
    !> which matters, because `iand(0, -1)` is zero and would otherwise read as a power of two.
    subroutine scenario_spatial_sky_nside_zero()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX, &
                          nside=0_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted nside=0, nside=", sx%nside()
    end subroutine scenario_spatial_sky_nside_zero

    !> A declination outside [-90, 90] is refused at build time.
    subroutine scenario_spatial_sky_dec_out_of_range()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        dec(7) = 91.0_real64
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)   ! -> aborts
        print '(a,i0)', "unexpectedly built a sky index with |dec| > 90, n=", sx%size()
    end subroutine scenario_spatial_sky_dec_out_of_range

    !> `%rebuild` on a sky index is refused: it takes Cartesian coordinates.
    subroutine scenario_spatial_sky_rebuild_refused()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:), x(:), y(:), z(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        call spatial_cloud(64, x, y, z)
        call sx%rebuild(x, y, z)   ! -> aborts
        print '(a,i0)', "unexpectedly rebuilt a sky index from Cartesian arrays, n=", sx%size()
    end subroutine scenario_spatial_sky_rebuild_refused

    !> `%rebuild` on an index that holds no copy to compare against.
    subroutine scenario_spatial_rebuild_needs_copy()
        type(pf_spatial_index) :: sx
        real(real64), allocatable, target :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, copy=.false.)
        call sx%rebuild(x, y, z)   ! -> aborts
        print '(a)', "unexpectedly rebuilt a copy=.false. index"
    end subroutine scenario_spatial_rebuild_needs_copy

    !> A per-point radius array whose length is neither 1 nor the point count.
    subroutine scenario_spatial_bulk_radius_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%count_all_within([0.1_real64, 0.2_real64, 0.3_real64], counts)   ! -> aborts
        print '(a)', "unexpectedly accepted a radius array of the wrong length"
    end subroutine scenario_spatial_bulk_radius_length

    !> A `combine=` value that names no rule.
    !>
    !> Refused rather than silently treated as the default: the argument selects which pairs come
    !> back, so a mistyped constant that fell through to `PF_LINK_MAX` would answer a different
    !> question from the one asked and look entirely ordinary doing it.
    subroutine scenario_spatial_pairs_bad_combine()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_cloud(64, x, y, z)
        allocate (rv(size(x)))
        rv = 0.2_real64
        call sx%build(x, y, z, radius=rv)
        call sx%pairs_within(rv, pi, pj, combine=99)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an unknown combine= value, pairs=", size(pi)
    end subroutine scenario_spatial_pairs_bad_combine

    !> `PF_LINK_SUM` on the sky past the 45-degree limit its doubled walk imposes.
    !>
    !> The general sky ceiling is 90 degrees and this rule walks twice each radius, so 60 degrees
    !> would ask the walk for 120 -- past the point where a ball prunes anything at all. The check
    !> runs BEFORE the degrees-to-chord conversion so that the message names the rule's own limit
    !> rather than the general one, which is the half a caller can act on.
    subroutine scenario_spatial_sky_pairs_sum_too_large()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_sky_cloud(64, ra, dec)
        allocate (rv(size(ra)))
        rv = 60.0_real64
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        call sx%pairs_within_sky(rv, pi, pj, combine=PF_LINK_SUM)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a 60-degree radius under PF_LINK_SUM, pairs=", size(pi)
    end subroutine scenario_spatial_sky_pairs_sum_too_large

    !> An `int32` pair list over an index holding more rows than an `int32` can name.
    !!
    !! **The ceiling is lowered by `parquet_debug_set_spatial_int32_ceiling`**, because the real
    !! one needs an index of more than two billion points. The `int64` call above it is the
    !! negative control: the ceiling governs which ANSWERS are refused and nothing else, so that
    !! one must come back normally in the same process.
    subroutine scenario_spatial_pairs_int32_rows()
        type(pf_spatial_index) :: sx
        real(real64) :: x(200), y(200), z(200)
        integer(int64), allocatable :: i64(:), j64(:)
        integer(int32), allocatable :: i32(:), j32(:)
        integer :: i

        do i = 1, 200
            x(i) = real(mod(i * 7, 20), real64) / 20.0_real64
            y(i) = real(mod(i * 11, 20), real64) / 20.0_real64
            z(i) = real(mod(i * 13, 20), real64) / 20.0_real64
        end do
        call sx%build(x, y, z, radius=0.2_real64)
        call parquet_debug_set_spatial_int32_ceiling(50_int64)
        call sx%pairs_within(0.2_real64, i64, j64)
        print '(a,i0)', "int64 pairs under a lowered ceiling=", size(i64)
        call sx%pairs_within(0.2_real64, i32, j32)
        print '(a,i0)', "unexpectedly answered pairs_within in int32, pairs=", size(i32)
    end subroutine scenario_spatial_pairs_int32_rows

    !> An `int32` CSR whose ROW count fits but whose neighbour list does not.
    !!
    !! **This is the guard that would be wrong if it were copied from its neighbour.** The ceiling
    !! is set above the row count and below the neighbour total, so only a check written against
    !! the total fires; one written against the row count would return wrapped offsets instead.
    subroutine scenario_spatial_csr_int32_offsets()
        type(pf_spatial_index) :: sx
        real(real64) :: x(200), y(200), z(200)
        integer(int64), allocatable :: off64(:), nb64(:)
        integer(int32), allocatable :: off32(:), nb32(:)
        integer :: i

        do i = 1, 200
            x(i) = real(mod(i * 7, 20), real64) / 20.0_real64
            y(i) = real(mod(i * 11, 20), real64) / 20.0_real64
            z(i) = real(mod(i * 13, 20), real64) / 20.0_real64
        end do
        call sx%build(x, y, z, radius=0.3_real64)
        call sx%all_within(0.3_real64, off64, nb64)
        print '(a,i0,a,i0)', "rows=", size(off64) - 1, " neighbours=", size(nb64)
        ! Above the 200 rows, below the neighbour total printed above.
        call parquet_debug_set_spatial_int32_ceiling(300_int64)
        call sx%all_within(0.3_real64, off32, nb32)
        print '(a,i0)', "unexpectedly answered all_within in int32, offsets=", size(off32)
    end subroutine scenario_spatial_csr_int32_offsets

    !> A deterministic wedge for the line-of-sight scenarios: `n` points at distances 500..1500
    !> from the origin inside a few degrees, so every point has a line of sight, with the distance
    !> handed back beside the coordinates for a scenario that wants a `los` derived from it.
    subroutine spatial_los_cloud(n, x, y, z, d)
        integer, intent(in) :: n !! how many points.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        real(real64), allocatable, intent(out) :: d(:) !! distance from the origin of every point.
        integer :: i
        real(real64) :: ra, dec

        allocate (x(n), y(n), z(n), d(n))
        do i = 1, n
            ra = 0.1_real64 * (pf_random_at(4321_int64, i, 1_int64) - 0.5_real64)
            dec = 0.1_real64 * (pf_random_at(4321_int64, i, 2_int64) - 0.5_real64)
            d(i) = 500.0_real64 + 1000.0_real64 * pf_random_at(4321_int64, i, 3_int64)
            x(i) = d(i) * cos(dec) * cos(ra)
            y(i) = d(i) * cos(dec) * sin(ra)
            z(i) = d(i) * sin(dec)
        end do
    end subroutine spatial_los_cloud

    !> `%pairs_within_los` on a sky index: unit vectors carry no distance from an observer.
    subroutine scenario_spatial_los_on_sky()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_sky_cloud(64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.0_real64)
        call sx%pairs_within_los(0.1_real64, 0.1_real64, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly answered a line-of-sight query on a sky index, pairs=", size(pi)
    end subroutine scenario_spatial_los_on_sky

    !> `%pairs_within_los` on a 2D index: a line of sight needs three coordinates.
    subroutine scenario_spatial_los_on_2d()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, radius=0.2_real64)
        call sx%pairs_within_los(0.1_real64, 0.1_real64, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly answered a line-of-sight query on a 2D index, pairs=", size(pi)
    end subroutine scenario_spatial_los_on_2d

    !> `%pairs_within_los` on a periodic index: the minimum image has no observer.
    subroutine scenario_spatial_los_on_periodic()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
                      box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        call sx%pairs_within_los(0.1_real64, 0.1_real64, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly answered a line-of-sight query on a periodic index, pairs=", size(pi)
    end subroutine scenario_spatial_los_on_periodic

    !> A stored point sitting on the observer has no line of sight. Refused at the QUERY on an index
    !> built without `los=`, because a plain %build has always accepted a point at the origin.
    subroutine scenario_spatial_los_point_at_observer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        x(5) = 0.0_real64
        y(5) = 0.0_real64
        z(5) = 0.0_real64
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los(10.0_real64, 30.0_real64, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly swept a catalogue holding a point at the observer, pairs=", size(pi)
    end subroutine scenario_spatial_los_point_at_observer

    !> `%within_los` with the query point on the observer: no direction to search along.
    subroutine scenario_spatial_within_los_point_at_observer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        m = sx%within_los([0.0_real64, 0.0_real64, 0.0_real64], 10.0_real64, 30.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly answered a query from the observer itself, m=", m
    end subroutine scenario_spatial_within_los_point_at_observer

    !> Transverse and parallel length lists of different lengths.
    subroutine scenario_spatial_los_length_mismatch()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        allocate (bp(64), bl(63))
        bp = 10.0_real64
        bl = 30.0_real64
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los(bp, bl, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted length lists of different lengths, pairs=", size(pi)
    end subroutine scenario_spatial_los_length_mismatch

    !> Length lists that are neither one value nor one per point.
    subroutine scenario_spatial_los_lengths_not_per_point()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los([10.0_real64, 10.0_real64, 10.0_real64], &
                                 [30.0_real64, 30.0_real64, 30.0_real64], pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted three lengths for 64 points, pairs=", size(pi)
    end subroutine scenario_spatial_los_lengths_not_per_point

    !> A negative parallel length.
    subroutine scenario_spatial_los_negative_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        allocate (bp(64), bl(64))
        bp = 10.0_real64
        bl = 30.0_real64
        bl(5) = -1.0_real64
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los(bp, bl, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative parallel length, pairs=", size(pi)
    end subroutine scenario_spatial_los_negative_length

    !> A `combine=` value that names no rule, on the cylinder sweep.
    subroutine scenario_spatial_los_bad_combine()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        allocate (bp(64), bl(64))
        bp = 10.0_real64
        bl = 30.0_real64
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los(bp, bl, pi, pj, combine=99)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an unknown combine= value on the cylinder, pairs=", size(pi)
    end subroutine scenario_spatial_los_bad_combine

    !> `%within_los` without `los_p=` on an index that carries `los=`: the query point's own parallel
    !> coordinate is the one thing the library cannot derive.
    subroutine scenario_spatial_within_los_needs_los_p()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d)
        m = sx%within_los([x(1), y(1), z(1)], 10.0_real64, 30.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly answered without los_p on a los= index, m=", m
    end subroutine scenario_spatial_within_los_needs_los_p

    !> `%within_los` with `los_p=` on an index built without `los=`: the value would be compared with
    !> distances, silently.
    subroutine scenario_spatial_within_los_los_p_refused()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        m = sx%within_los([x(1), y(1), z(1)], 10.0_real64, 30.0_real64, got, los_p=d(1))   ! -> aborts
        print '(a,i0)', "unexpectedly accepted los_p on an index without los=, m=", m
    end subroutine scenario_spatial_within_los_los_p_refused

    !> `%within_los` with a zero transverse length: the normalised distance would divide by it.
    subroutine scenario_spatial_within_los_zero_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        m = sx%within_los([x(1), y(1), z(1)], 0.0_real64, 30.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a zero transverse length, m=", m
    end subroutine scenario_spatial_within_los_zero_length

    !> `%within_los` with a two-coordinate query point on a 3D index.
    subroutine scenario_spatial_within_los_rank()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        m = sx%within_los([x(1), y(1)], 10.0_real64, 30.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a two-coordinate query point, m=", m
    end subroutine scenario_spatial_within_los_rank

    !> `%within_los` with a NaN `los_p=`.
    subroutine scenario_spatial_within_los_los_p_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64) :: got(8), m

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d)
        m = sx%within_los([x(1), y(1), z(1)], 10.0_real64, 30.0_real64, got, &
                          los_p=ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN los_p, m=", m
    end subroutine scenario_spatial_within_los_los_p_nan

    !> A constant `los=`: the parallel test could never separate two points.
    subroutine scenario_spatial_los_constant()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        d = 1.0_real64
        call sx%build(x, y, z, radius=20.0_real64, los=d)   ! -> aborts
        print '(a,i0)', "unexpectedly built an index over a constant los, n=", sx%size()
    end subroutine scenario_spatial_los_constant

    !> A NaN in `los=`.
    subroutine scenario_spatial_los_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        d(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sx%build(x, y, z, radius=20.0_real64, los=d)   ! -> aborts
        print '(a,i0)', "unexpectedly built an index over a NaN los, n=", sx%size()
    end subroutine scenario_spatial_los_nan

    !> A `los=` shorter than the catalogue.
    subroutine scenario_spatial_los_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d(1:63))   ! -> aborts
        print '(a,i0)', "unexpectedly built an index over a short los, n=", sx%size()
    end subroutine scenario_spatial_los_length

    !> `los=` on a 2D build.
    subroutine scenario_spatial_los_build_on_2d()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, radius=20.0_real64, los=d)   ! -> aborts
        print '(a,i0)', "unexpectedly built a 2D index with los=, n=", sx%size()
    end subroutine scenario_spatial_los_build_on_2d

    !> `observer=` on a periodic build.
    subroutine scenario_spatial_los_build_on_periodic()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
                      box_hi=[1.0_real64, 1.0_real64, 1.0_real64], &
                      observer=[0.5_real64, 0.5_real64, 0.5_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly built a periodic index with observer=, n=", sx%size()
    end subroutine scenario_spatial_los_build_on_periodic

    !> An `observer=` with two coordinates.
    subroutine scenario_spatial_los_observer_size()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, observer=[1.0_real64, 2.0_real64])   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a two-coordinate observer, n=", sx%size()
    end subroutine scenario_spatial_los_observer_size

    !> A NaN in `observer=`.
    subroutine scenario_spatial_los_observer_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        real(real64) :: o(3)

        call spatial_los_cloud(64, x, y, z, d)
        o = [ieee_value(1.0_real64, ieee_quiet_nan), 0.0_real64, 0.0_real64]
        call sx%build(x, y, z, radius=20.0_real64, observer=o)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN observer coordinate, n=", sx%size()
    end subroutine scenario_spatial_los_observer_nan

    !> A point on the observer, refused at BUILD when `los=` declares the line-of-sight intent.
    subroutine scenario_spatial_los_build_point_at_observer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        x(5) = 0.0_real64
        y(5) = 0.0_real64
        z(5) = 0.0_real64
        call sx%build(x, y, z, radius=20.0_real64, los=d)   ! -> aborts
        print '(a,i0)', "unexpectedly built a los= index holding a point at the observer, n=", sx%size()
    end subroutine scenario_spatial_los_build_point_at_observer

    !> `%rebuild` without `los=` on an index that carries one.
    subroutine scenario_spatial_rebuild_los_missing()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d)
        call sx%rebuild(x, y, z)   ! -> aborts
        print '(a,i0)', "unexpectedly rebuilt a los= index without new los values, n=", sx%size()
    end subroutine scenario_spatial_rebuild_los_missing

    !> `%rebuild` with `los=` on an index built without one.
    subroutine scenario_spatial_rebuild_los_unexpected()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%rebuild(x, y, z, los=d)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted los= on %rebuild of an index without one, n=", sx%size()
    end subroutine scenario_spatial_rebuild_los_unexpected

    !> `%rebuild` with a `los=` shorter than the coordinates.
    subroutine scenario_spatial_rebuild_los_length()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d)
        call sx%rebuild(x, y, z, los=d(1:63))   ! -> aborts
        print '(a,i0)', "unexpectedly rebuilt with a short los, n=", sx%size()
    end subroutine scenario_spatial_rebuild_los_length

    !> A `los=` unrelated to the distance from the observer is accepted with a warning, on the
    !> stream the settings name: its small-scale slope is thousands of times the catalogue-wide one.
    subroutine scenario_spatial_los_not_a_function_warns()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:), los(:)
        integer :: i

        call spatial_los_cloud(64, x, y, z, d)
        allocate (los(64))
        do i = 1, 64
            los(i) = pf_random_at(8765_int64, i, 1_int64)
        end do
        call parquet_set_message_stream("stderr")
        call sx%build(x, y, z, radius=20.0_real64, los=los)   ! -> warns, does not abort
        call parquet_reset_settings()
        print '(a,i0)', "built with a warning; n=", sx%size()
    end subroutine scenario_spatial_los_not_a_function_warns

    !> A parallel length given in the coordinates' units against a redshift-like `los=` makes every
    !> line-of-sight walk span the whole catalogue: said, on the stream the settings name, never
    !> refused, since the sweep is still exact and the window can be meant.
    subroutine scenario_spatial_los_window_spans_catalogue_warns()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        ! A redshift-like coordinate: a thousandth of the distance, so its slope is a thousand.
        call sx%build(x, y, z, radius=20.0_real64, los=0.001_real64 * d)
        call parquet_set_message_stream("stderr")
        call sx%pairs_within_los(10.0_real64, 30.0_real64, pi, pj)   ! -> warns, does not abort
        call parquet_reset_settings()
        print '(a,i0)', "swept with a warning; pairs=", size(pi)
    end subroutine scenario_spatial_los_window_spans_catalogue_warns

    !> `copy=.false.` over a strided section.
    !>
    !> A `contiguous` dummy would copy such an actual into a temporary that dies at the end of the
    !> call, so the index would point at freed memory the moment it was built -- and nothing later
    !> could detect that. Refusing is the only safe answer.
    subroutine scenario_spatial_copy_false_strided()
        type(pf_spatial_index) :: sx
        real(real64), allocatable, target :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x(1:63:2), y(1:63:2), z(1:63:2), radius=0.2_real64, copy=.false.)   ! -> aborts
        print '(a)', "unexpectedly accepted a strided section with copy=.false."
    end subroutine scenario_spatial_copy_false_strided

    !> An explicit thread count below one.
    subroutine scenario_spatial_threads_below_one()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%count_all_within(0.2_real64, counts, threads=0)   ! -> aborts
        print '(a)', "unexpectedly accepted threads= 0"
    end subroutine scenario_spatial_threads_below_one

    !> The automatic-rebuild ADVICE, at two verbosities. Exits cleanly either way -- what differs
    !> is whether the library says anything, and the printed `rebuilds=` count is what proves the
    !> rebuild itself happened in both runs.
    !>
    !> This pair replaced the observed-effect test for `spatial_rebuild_warning`, the per-message
    !> knob that used to control exactly this one line. The message is advice now, so the class-wide
    !> control is what governs it -- and the property worth pinning is unchanged: silencing the
    !> message must not silence the rebuild.
    !>
    !> Its fixture is built in memory, so the two scenario names share no path and may run
    !> concurrently.
    subroutine scenario_spatial_rebuild_advice(level)
        character(len=*), intent(in) :: level !! verbosity to set first.
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)

        call parquet_set_message_stream("stderr")
        call parquet_set_verbosity(level)
        call spatial_cloud(3000, x, y, z)
        call sx%build(x, y, z, radius=0.005_real64)
        ! A radius two orders of magnitude from the hint: far enough that the cell the model would
        ! choose for it differs by more than the rebuild factor, so the index re-tunes itself.
        call sx%count_all_within(0.5_real64, counts)
        ! Printed AFTER the settings are put back, because "silent" turns every print procedure into
        ! a no-op -- and this line is the control that says the rebuild happened at all.
        call parquet_reset_settings()
        print '(a,i0)', "rebuilds=", parquet_debug_spatial_rebuilds()
    end subroutine scenario_spatial_rebuild_advice

    !
    ! ---- Abort paths reached only through a second entry point ----
    !
    ! Each of these guards sits behind a binding whose SIBLING already has a scenario above: the
    ! sky single-point walk beside the Euclidean one, `%rebuild`'s shape checks beside `%build`'s,
    ! the transverse line-of-sight length beside the parallel one. A guard written twice is two
    ! guards, and only the one with a scenario is known to fire.
    !

    !> A sky query on an index that has never been built.
    subroutine scenario_spatial_sky_query_before_build()
        type(pf_spatial_index) :: sx
        integer(int64) :: got(4), m

        m = sx%within_sky(10.0_real64, 20.0_real64, 1.0_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a sky query on an unbuilt index, m=", m
    end subroutine scenario_spatial_sky_query_before_build

    !> A NaN angular search radius.
    subroutine scenario_spatial_sky_radius_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64) :: got(8), m

        call spatial_sky_cloud(64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=1.0_real64)
        m = sk%within_sky(10.0_real64, 20.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN angular radius, m=", m
    end subroutine scenario_spatial_sky_radius_nan

    !> A NaN inner angular radius.
    subroutine scenario_spatial_sky_inner_radius_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64) :: got(8), m

        call spatial_sky_cloud(64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=1.0_real64)
        m = sk%within_sky(10.0_real64, 20.0_real64, 2.0_real64, got, &
            r_inner_deg=ieee_value(1.0_real64, ieee_quiet_nan))   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN inner angular radius, m=", m
    end subroutine scenario_spatial_sky_inner_radius_nan

    !> A NaN entry in a per-point angular radius LIST, which is screened element by element.
    subroutine scenario_spatial_sky_radii_vector_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:), rd(:)
        integer(int64), allocatable :: offs(:), nbrs(:)

        call spatial_sky_cloud(64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=1.0_real64)
        allocate (rd(64))
        rd = 1.0_real64
        rd(7) = ieee_value(1.0_real64, ieee_quiet_nan)
        call sk%all_within_sky(rd, offs, nbrs)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN in an angular radius list, n=", size(nbrs)
    end subroutine scenario_spatial_sky_radii_vector_nan

    !> `%nearest_sky` on an index that has never been built.
    subroutine scenario_spatial_nearest_sky_before_build()
        type(pf_spatial_index) :: sx
        integer(int64) :: got(4), m

        m = sx%nearest_sky(10.0_real64, 20.0_real64, 2_int64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly ran a sky nearest query on an unbuilt index, m=", m
    end subroutine scenario_spatial_nearest_sky_before_build

    !> A NaN query radius on the ball walk, which screens the radius its own callers do not.
    subroutine scenario_spatial_query_radius_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(8), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], &
            ieee_value(1.0_real64, ieee_quiet_nan), got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a NaN query radius, m=", m
    end subroutine scenario_spatial_query_radius_nan

    !> A periodic query radius above half the box, refused by the WALK rather than by %build.
    subroutine scenario_spatial_query_radius_half_box()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(64), m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.75_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a periodic query radius above half the box, m=", m
    end subroutine scenario_spatial_query_radius_half_box

    !> `%build_sky` with ra and dec of different lengths.
    subroutine scenario_spatial_build_sky_length_mismatch()
        type(pf_spatial_index) :: sk
        real(real64) :: ra(10), dec(9)

        ra = 0.0_real64
        dec = 0.0_real64
        call sk%build_sky(ra, dec, radius_deg=1.0_real64)   ! -> aborts
        print '(a)', "unexpectedly accepted ra and dec of different lengths"
    end subroutine scenario_spatial_build_sky_length_mismatch

    !> `%build_sky` with an empty `radius_deg=` list.
    subroutine scenario_spatial_build_sky_no_radius()
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)
        real(real64) :: none(0)

        call spatial_sky_cloud(64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=none)   ! -> aborts
        print '(a)', "unexpectedly accepted an empty radius_deg= list"
    end subroutine scenario_spatial_build_sky_no_radius

    !> `%build_sky` with a radius that is not positive.
    subroutine scenario_spatial_build_sky_radius_not_positive()
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=[1.0_real64, 0.0_real64])   ! -> aborts
        print '(a)', "unexpectedly accepted a radius_deg= of zero"
    end subroutine scenario_spatial_build_sky_radius_not_positive

    !> `%rebuild` on an index that has never been built.
    subroutine scenario_spatial_rebuild_before_build()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%rebuild(x, y, z)   ! -> aborts
        print '(a)', "unexpectedly rebuilt an index that was never built"
    end subroutine scenario_spatial_rebuild_before_build

    !> `%rebuild` dropping the `z` the index was built with.
    subroutine scenario_spatial_rebuild_z_rank()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%rebuild(x, y)   ! -> aborts
        print '(a)', "unexpectedly rebuilt a 3D index without z"
    end subroutine scenario_spatial_rebuild_z_rank

    !> `%rebuild` with x and y of different lengths.
    subroutine scenario_spatial_rebuild_length_mismatch()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%rebuild(x, y(1:63), z)   ! -> aborts
        print '(a)', "unexpectedly rebuilt from x and y of different lengths"
    end subroutine scenario_spatial_rebuild_length_mismatch

    !> `%rebuild` moving a point onto the observer, which leaves it no line of sight.
    subroutine scenario_spatial_rebuild_at_observer()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:)

        call spatial_los_cloud(64, x, y, z, d)
        call sx%build(x, y, z, radius=20.0_real64, los=d)
        x(3) = 0.0_real64
        y(3) = 0.0_real64
        z(3) = 0.0_real64
        call sx%rebuild(x, y, z, los=d)   ! -> aborts
        print '(a)', "unexpectedly rebuilt with a point at the observer"
    end subroutine scenario_spatial_rebuild_at_observer

    !> `%rebuild_for` on an index that has never been built.
    subroutine scenario_spatial_rebuild_for_before_build()
        type(pf_spatial_index) :: sx

        call sx%rebuild_for([0.2_real64])   ! -> aborts
        print '(a)', "unexpectedly re-tuned an index that was never built"
    end subroutine scenario_spatial_rebuild_for_before_build

    !> `%rebuild_for` asking a PERIODIC index for a radius above half its box.
    subroutine scenario_spatial_rebuild_for_half_box()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        call sx%rebuild_for([0.9_real64])   ! -> aborts
        print '(a)', "unexpectedly re-tuned a periodic index for a radius above half the box"
    end subroutine scenario_spatial_rebuild_for_half_box

    !> `%build` with `box_lo=`/`box_hi=` of the wrong rank for the coordinates.
    subroutine scenario_spatial_build_box_rank()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64])   ! -> aborts
        print '(a)', "unexpectedly accepted a box with fewer entries than coordinates"
    end subroutine scenario_spatial_build_box_rank

    !> `%build` with a box whose upper corner is not strictly above its lower one.
    subroutine scenario_spatial_build_box_not_strict()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 0.0_real64, 1.0_real64])   ! -> aborts
        print '(a)', "unexpectedly accepted a box with zero extent on an axis"
    end subroutine scenario_spatial_build_box_not_strict

    !> `copy=.false.` with a strided `z`, which is screened separately from `x` and `y`.
    subroutine scenario_spatial_copy_false_z_strided()
        type(pf_spatial_index) :: sx
        real(real64), allocatable, target :: x(:), y(:), z(:)

        call spatial_cloud(64, x, y, z)
        call sx%build(x(1:32), y(1:32), z(1:63:2), radius=0.2_real64, copy=.false.)   ! -> aborts
        print '(a)', "unexpectedly accepted a strided z with copy=.false."
    end subroutine scenario_spatial_copy_false_z_strided

    !> A bulk sweep on an index that has never been built.
    subroutine scenario_spatial_bulk_before_build()
        type(pf_spatial_index) :: sx
        integer(int64), allocatable :: counts(:)

        call sx%count_all_within(0.2_real64, counts)   ! -> aborts
        print '(a)', "unexpectedly swept an index that was never built"
    end subroutine scenario_spatial_bulk_before_build

    !> A line-of-sight sweep on an index that has never been built.
    subroutine scenario_spatial_los_before_build()
        type(pf_spatial_index) :: sx
        integer(int64), allocatable :: pi(:), pj(:)

        call sx%pairs_within_los(10.0_real64, 30.0_real64, pi, pj)   ! -> aborts
        print '(a)', "unexpectedly ran a line-of-sight sweep on an unbuilt index"
    end subroutine scenario_spatial_los_before_build

    !> A negative TRANSVERSE length, screened separately from the parallel one.
    subroutine scenario_spatial_los_bperp_negative()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:), d(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)

        call spatial_los_cloud(64, x, y, z, d)
        allocate (bp(64), bl(64))
        bp = 10.0_real64
        bl = 30.0_real64
        bp(5) = -1.0_real64
        call sx%build(x, y, z, radius=20.0_real64)
        call sx%pairs_within_los(bp, bl, pi, pj)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a negative transverse length, pairs=", size(pi)
    end subroutine scenario_spatial_los_bperp_negative

    !> `%kth_distance` on an index that has never been built.
    subroutine scenario_spatial_kth_before_build()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: dist(:)

        call sx%kth_distance(1_int32, dist)   ! -> aborts
        print '(a)', "unexpectedly ran kth_distance on an unbuilt index"
    end subroutine scenario_spatial_kth_before_build

    !> `pf_connected_components` with int32 endpoint arrays of different lengths.
    subroutine scenario_spatial_components_i32_length()
        integer(int32), allocatable :: labels(:)

        call pf_connected_components([1_int32, 2_int32], [2_int32], 4_int64, labels)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted int32 endpoint arrays of different lengths, n=", size(labels)
    end subroutine scenario_spatial_components_i32_length

    !> `pf_connected_components` asked for an int32 answer over more vertices than it can name.
    subroutine scenario_spatial_components_nvert_int32()
        integer(int32), allocatable :: labels(:)

        call parquet_debug_set_spatial_int32_ceiling(8_int64)
        call pf_connected_components([1_int32, 2_int32], [2_int32, 3_int32], 40_int64, labels)   ! -> aborts
        print '(a,i0)', "unexpectedly named 40 vertices in an int32 answer, n=", size(labels)
    end subroutine scenario_spatial_components_nvert_int32

    !> `%within` into an int32 buffer on an index holding more rows than int32 can name.
    subroutine scenario_spatial_within_int32_ceiling()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: got(8)
        integer(int64) :: m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        ! The negative control: the same call under the shipped ceiling must answer rather than
        ! abort, so the abort below is the hook's doing and not the query's.
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)
        print '(a,i0)', "under the shipped ceiling the int32 query answered, m=", m
        call parquet_debug_set_spatial_int32_ceiling(8_int64)
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an int32 buffer over the ceiling, m=", m
    end subroutine scenario_spatial_within_int32_ceiling

    !> `%within_segment` into an int32 buffer over the same ceiling, on the AXIS walk.
    subroutine scenario_spatial_axis_int32_ceiling()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: got(8)
        integer(int64) :: m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], &
            [0.8_real64, 0.8_real64, 0.8_real64], 0.2_real64, got)
        print '(a,i0)', "under the shipped ceiling the int32 axis query answered, m=", m
        call parquet_debug_set_spatial_int32_ceiling(8_int64)
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], &
            [0.8_real64, 0.8_real64, 0.8_real64], 0.2_real64, got)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an int32 axis buffer over the ceiling, m=", m
    end subroutine scenario_spatial_axis_int32_ceiling

    !> `%nearest` into an int32 buffer over the same ceiling.
    subroutine scenario_spatial_nearest_int32_ceiling()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: got(4)
        integer(int64) :: m

        call spatial_cloud(64, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], 4_int32, got)
        print '(a,i0)', "under the shipped ceiling the int32 nearest query answered, m=", m
        call parquet_debug_set_spatial_int32_ceiling(8_int64)
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], 4_int32, got)   ! -> aborts
        print '(a,i0)', "unexpectedly filled an int32 nearest buffer over the ceiling, m=", m
    end subroutine scenario_spatial_nearest_int32_ceiling

    !> `%grid` narrowed to int32 when a cell count does not fit.
    subroutine scenario_spatial_grid_int32_ceiling()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: nx, ny, nz

        ! Large enough that the cells-per-point ceiling leaves a grid of more than four cells on
        ! an axis: with 64 points the grid is clamped to two and the narrowing below succeeds.
        call spatial_cloud(4000, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64, cell=0.05_real64)
        call sx%grid(nx, ny, nz)
        print '(a,i0)', "under the shipped ceiling the int32 grid answered, nx=", nx
        call parquet_debug_set_spatial_int32_ceiling(4_int64)
        call sx%grid(nx, ny, nz)   ! -> aborts
        print '(a,i0)', "unexpectedly narrowed a cell count over the ceiling, nx=", nx
    end subroutine scenario_spatial_grid_int32_ceiling

    !> The work hook with an empty radius list.
    subroutine scenario_spatial_debug_work_no_radius()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: none(0)
        integer(int64) :: cells, points

        call spatial_cloud(200, x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call parquet_debug_spatial_work(sx, 0.2_real64, none, cells, points)   ! -> aborts
        print '(a,i0)', "unexpectedly accepted an empty radius list, cells=", cells
    end subroutine scenario_spatial_debug_work_no_radius

    !> An explicit `nside=` coarsened by the buckets-per-point ceiling, which says so.
    !>
    !> Exits 0: the resolution is a tuning choice and the answer stays exact, so the caller is
    !> told rather than refused.
    subroutine scenario_spatial_nside_coarsened_warns()
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)

        call spatial_sky_cloud(64, ra, dec)
        ! 64 points allow 19 pixels; nside 8 asks for 768, so the build coarsens it and warns.
        call sk%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX, nside=8_int64)
        print '(a,i0)', "nside after coarsening=", sk%nside()
    end subroutine scenario_spatial_nside_coarsened_warns

    !> A SKY index re-tuned by a bulk query far from its built radius. Exits 0.
    !>
    !> The message must be in DEGREES: the index accumulates chords, and a caller who asked in
    !> degrees would otherwise be told about a radius in units they never used.
    subroutine scenario_spatial_sky_rebuild_warns()
        type(pf_spatial_index) :: sk
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: counts(:)

        call spatial_sky_cloud(400, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=0.05_real64)
        call sk%count_all_within_sky(8.0_real64, counts)
        print '(a,i0)', "rebuilds=", parquet_debug_spatial_rebuilds()
    end subroutine scenario_spatial_sky_rebuild_warns

    !> ==== Variable-length LIST reads ====
    !
    !> A list read of a column that is not a list at all: the shape query refuses before anything
    !> is allocated, naming the type it actually found.
    subroutine scenario_list_read_not_a_list()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/list_widths.parquet")
        call parquet_read_column(reader, "scalar", lc)
        call parquet_close_reader(reader)
    end subroutine scenario_list_read_not_a_list

    !> A list whose ELEMENTS are themselves a container (`list<list<int32>>`), which Phase 7 reads.
    !!
    !! Inverted rather than deleted: it was the refusal test until nesting landed, and it is worth
    !! more as an out-of-process assertion that the ASSEMBLY is right. The row lengths are checked
    !! rather than only the row count, because an assembly that used the outer offsets for the
    !! inner container -- the likeliest way to get this wrong -- still produces the right number of
    !! rows.
    subroutine scenario_list_read_nested_payload()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        character(len=:), allocatable :: kt
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "list_of_list", lc)
        call lc%kind_text(kt)
        if (kt /= "list<list<int32>>") error stop "list_read_nested_payload: kind_text is "//kt
        if (lc%nrows() /= 3_int64) error stop "list_read_nested_payload: expected three rows"
        if (lc%element_kind() /= PK_LIST) error stop "list_read_nested_payload: payload is not a list"
        call parquet_close_reader(reader)
    end subroutine scenario_list_read_nested_payload

    !> The same, for a `list<struct<...>>` -- a different element type reaching the same assembly,
    !> so this is not keyed on one Arrow type id.
    subroutine scenario_list_read_struct_payload()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        character(len=:), allocatable :: kt
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "list_of_struct", lc)
        call lc%kind_text(kt)
        if (kt /= "list<struct<x:int32,y:string>>") error stop "list_read_struct_payload: kind_text is "//kt
        if (lc%length(1_int64) /= 1_int64) error stop "list_read_struct_payload: row 1 length"
        if (lc%length(2_int64) /= 0_int64) error stop "list_read_struct_payload: row 2 length"
        if (lc%length(3_int64) /= 2_int64) error stop "list_read_struct_payload: row 3 length"
        call parquet_close_reader(reader)
    end subroutine scenario_list_read_struct_payload

    !> A chunked list read while a read-time sort is installed. Every row-group-scoped operation
    !> refuses, because a permutation destroys row-group locality -- the list specific inherits
    !> that and must not weaken it.
    subroutine scenario_list_chunk_refuses_sort()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        type(parquet_list_column) :: lc
        call srt%add("scalar desc")
        call parquet_open_reader(reader, "test/fixtures/list_widths.parquet", sort_by=srt)
        call parquet_read_column_chunk(reader, "ragged", 1_int64, lc)
        call parquet_close_reader(reader)
    end subroutine scenario_list_chunk_refuses_sort

    !> A chunked list read of a row group that does not exist.
    subroutine scenario_list_chunk_row_group_out_of_range()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/list_widths.parquet")
        call parquet_read_column_chunk(reader, "ragged", 99_int64, lc)
        call parquet_close_reader(reader)
    end subroutine scenario_list_chunk_row_group_out_of_range

    !> A chunked list read that skips a row group, then a close that checks completeness. This is
    !> the only in-library observation that the chunked list path registers the row groups it read
    !> -- and so the only thing that would notice if it stopped.
    subroutine scenario_list_chunk_incomplete_aborts()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/list_widths.parquet")
        call parquet_read_column_chunk(reader, "ragged", 1_int64, lc)
        call parquet_read_column_chunk(reader, "ragged", 2_int64, lc)
        call parquet_close_reader(reader, check_complete=.true.)
    end subroutine scenario_list_chunk_incomplete_aborts

    !> The negative control for the scenario above: reading EVERY row group closes cleanly. Without
    !> it, the abort would hold just as well against a check that fired unconditionally.
    subroutine scenario_list_chunk_complete_ok()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        integer(int64) :: rg, ngroups
        call parquet_open_reader(reader, "test/fixtures/list_widths.parquet")
        call parquet_get_num_row_groups(reader, ngroups)
        do rg = 1_int64, ngroups
            call parquet_read_column_chunk(reader, "ragged", rg, lc)
        end do
        call parquet_close_reader(reader, check_complete=.true.)
        print '(a)', "a fully chunk-read LIST column passes the completeness check, as expected"
    end subroutine scenario_list_chunk_complete_ok

    !> A whole-column list read must MARK the column read, so parquet_reader_print_stat reports it.
    !> print_stat writes to stdout, so this has to be an out-of-process scenario; the wrapper
    !> asserts the column's row says it was read.
    subroutine scenario_list_read_marks_read()
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/list_payloads.parquet")
        call parquet_read_column(reader, "i32", lc)
        call parquet_close_reader(reader, print_stat=.true.)
    end subroutine scenario_list_read_marks_read

    !> %adopt_rows refuses offsets whose final entry does not account for the payload it was handed.
    !> The invariant is what every later query depends on, so it is checked once, here, rather than
    !> discovered as a wrong row length much later.
    subroutine scenario_list_adopt_rows_offset_mismatch()
        type(parquet_list_column) :: lc
        type(parquet_column) :: payload
        integer(int64), allocatable :: offsets(:)
        integer(int32), allocatable :: v(:)
        v = [1_int32, 2_int32, 3_int32]
        call payload%adopt(v)
        offsets = [0_int64, 1_int64, 2_int64]   ! claims 2 elements for a 3-element payload
        call lc%adopt_rows(offsets, payload)
    end subroutine scenario_list_adopt_rows_offset_mismatch

    !> %adopt_rows refuses non-monotonic offsets: a row whose end precedes its start would yield a
    !> negative length, which every read of that row would then carry.
    subroutine scenario_list_adopt_rows_not_monotonic()
        type(parquet_list_column) :: lc
        type(parquet_column) :: payload
        integer(int64), allocatable :: offsets(:)
        integer(int32), allocatable :: v(:)
        v = [1_int32, 2_int32, 3_int32]
        call payload%adopt(v)
        offsets = [0_int64, 2_int64, 1_int64, 3_int64]
        call lc%adopt_rows(offsets, payload)
    end subroutine scenario_list_adopt_rows_not_monotonic

    !> %adopt_rows refuses a payload whose kind cannot be a list element (here a VECTOR kind, whose
    !> width is exactly what a list row expresses instead).
    subroutine scenario_list_adopt_rows_bad_payload_kind()
        type(parquet_list_column) :: lc
        type(parquet_column) :: payload
        integer(int64), allocatable :: offsets(:)
        integer(int32), allocatable :: v(:,:)
        allocate(v(2, 3))
        v = 0_int32
        call payload%adopt(v)
        offsets = [0_int64, 1_int64, 2_int64, 3_int64]
        call lc%adopt_rows(offsets, payload)
    end subroutine scenario_list_adopt_rows_bad_payload_kind

    !> %adopt_rows refuses a row_valid mask of the wrong length -- a mask one short would silently
    !> leave the last row's nullness to whatever the bitmap happened to hold.
    subroutine scenario_list_adopt_rows_mask_length()
        type(parquet_list_column) :: lc
        type(parquet_column) :: payload
        integer(int64), allocatable :: offsets(:)
        integer(int32), allocatable :: v(:)
        v = [1_int32, 2_int32, 3_int32]
        call payload%adopt(v)
        offsets = [0_int64, 1_int64, 2_int64, 3_int64]
        call lc%adopt_rows(offsets, payload, row_valid=[.true., .true.])
    end subroutine scenario_list_adopt_rows_mask_length

    !> Writing a list column into a schema column declared with a DIFFERENT element type. There is
    !> deliberately no widening between list element kinds the way there is between scalar numeric
    !> kinds (parquet_is_type_compatible), so this is a clean refusal naming both tokens rather
    !> than a silent conversion of the payload.
    !> ---- STRUCT column scenarios ----
    !>
    !> `%init` with no fields at all. Arrow cannot construct a zero-field struct and a struct
    !> column carrying no data is a mistake in every case a caller could reach it, so one clean
    !> abort here beats an obscure Arrow failure at close time.
    subroutine scenario_struct_init_no_fields()
        type(parquet_struct_column) :: sc
        character(len=4) :: names(0)
        integer :: kinds(0)
        call sc%init(names, kinds)
    end subroutine scenario_struct_init_no_fields

    !> `%init` with two field names the same. A duplicate would make `%field_index` answer about
    !> one of them arbitrarily and `%set_field(name)` write to that one for good.
    subroutine scenario_struct_init_duplicate_name()
        type(parquet_struct_column) :: sc
        call sc%init(["v", "v"], [PK_INT32, PK_INT32])
    end subroutine scenario_struct_init_duplicate_name

    !> `%init` with a field name containing a dot. A dot would make this column's own leaf path
    !> ambiguous against the dotted-path struct reader, which addresses exactly `col.field`.
    subroutine scenario_struct_init_dotted_name()
        type(parquet_struct_column) :: sc
        call sc%init(["a.b"], [PK_INT32])
    end subroutine scenario_struct_init_dotted_name

    !> `%init` with a field kind this type cannot hold. `PK_INT32_VEC` is a fixed-width vector,
    !> which inside a struct is `struct<fixed_size_list<...>>` -- Phase 7's nesting, not a width.
    subroutine scenario_struct_init_bad_kind()
        type(parquet_struct_column) :: sc
        call sc%init(["v"], [PK_INT32_VEC])
    end subroutine scenario_struct_init_bad_kind

    !> `%append_from` onto a struct with a different number of fields. Two columns can look alike
    !! -- both structs, both with rows -- and still have layouts that cannot be concatenated, so
    !! each part of the layout is checked separately and reported by name.
    subroutine scenario_struct_append_from_field_count()
        type(parquet_struct_column) :: dst, src

        call dst%init(["a  ", "b  "], [PK_INT32, PK_INT64])
        call src%init(["a  "], [PK_INT32])
        call src%append_row()
        call dst%append_from(src)
        print '(a)', "append_from accepted a struct with a different field count"
    end subroutine scenario_struct_append_from_field_count
    !
    !> The same names in the same order but a different KIND at one position: the sharp case, since
    !! nothing about the field set looks wrong until a value is read back at the other type.
    subroutine scenario_struct_append_from_field_kind()
        type(parquet_struct_column) :: dst, src

        call dst%init(["a  "], [PK_INT32])
        call src%init(["a  "], [PK_INT64])
        call src%append_row()
        call dst%append_from(src)
        print '(a)', "append_from accepted a struct whose field kind differs"
    end subroutine scenario_struct_append_from_field_kind
    !
    !> A container that is not a struct at all. `%append_from` takes the container base, so the
    !! type test is what keeps a list from being concatenated onto a struct.
    subroutine scenario_struct_append_from_not_struct()
        type(parquet_struct_column) :: dst
        type(parquet_list_column) :: src

        call dst%init(["a  "], [PK_INT32])
        call src%init(PK_INT32)
        call src%append_row([1_int32, 2_int32])
        call dst%append_from(src)
        print '(a)', "append_from accepted a list column onto a struct"
    end subroutine scenario_struct_append_from_not_struct
    !
    !> `%gather_rows` naming a source row the column does not have. Every index is checked BEFORE
    !! anything is rebuilt, so a bad one aborts rather than leaving a half-rebuilt column.
    subroutine scenario_struct_gather_rows_out_of_range()
        type(parquet_struct_column) :: sc
        integer(int64) :: ok_idx(2), bad_idx(2)

        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        call sc%append_row()
        ok_idx = [2_int64, 1_int64]
        bad_idx = [1_int64, 3_int64]
        call sc%gather_rows(ok_idx)              ! control: a legal permutation
        print '(a,i0)', "control: the permutation rebuilt the column, rows=", sc%size()
        call sc%gather_rows(bad_idx)             ! -> aborts
        print '(a)', "gather_rows accepted a source row index out of range"
    end subroutine scenario_struct_gather_rows_out_of_range
    !
    !> `%field_kind` on a handle that still denotes a whole row. The handle answers about one field
    !! once narrowed, and there is no sensible answer before that -- picking the first field would
    !! be a plausible wrong one.
    subroutine scenario_struct_field_kind_not_narrowed()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, slot
        integer :: k

        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        slot = h%field("v")
        k = slot%field_kind()                    ! control: a narrowed handle answers
        print '(a,i0)', "control: the narrowed handle reported kind=", k
        k = h%field_kind()                       ! -> aborts
        print '(a,i0)', "an un-narrowed handle reported field_kind=", k
    end subroutine scenario_struct_field_kind_not_narrowed
    !
    !> `%nested` on a handle that still denotes a whole row, for the same reason as `%field_kind`.
    subroutine scenario_struct_nested_not_narrowed()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        class(parquet_container_column), pointer :: inner
        integer(int64) :: row

        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        call h%nested(inner, row)                ! -> aborts
        print '(a,l1)', "an un-narrowed handle returned a nested container, assoc=", &
            associated(inner)
    end subroutine scenario_struct_nested_not_narrowed
    !
    !> Every `%adopt_fields` guard below refuses a layout that would otherwise build a column whose
    !! fields disagree with its names or with each other. They are separate guards with separate
    !! messages, so each gets its own scenario rather than one standing for the rest.
    !!
    !! A field name carrying a dot: the same refusal `%init` makes, because a dotted name is how a
    !! nested path is written and a field called `a.b` would be unaddressable.
    subroutine scenario_struct_adopt_dotted_name()
        type(parquet_struct_column) :: sc
        type(parquet_column), allocatable :: fields(:)
        character(len=3) :: names(1)

        allocate(fields(1))
        call fields(1)%init(PK_INT32, 2_int64)
        names(1) = "a.b"
        call sc%adopt_fields(names, fields)
        print '(a)', "adopt_fields accepted a dotted field name"
    end subroutine scenario_struct_adopt_dotted_name
    !
    !> Two fields of the same name: every lookup by name would answer the first, silently.
    subroutine scenario_struct_adopt_duplicate_name()
        type(parquet_struct_column) :: sc
        type(parquet_column), allocatable :: fields(:)
        character(len=3) :: names(2)

        allocate(fields(2))
        call fields(1)%init(PK_INT32, 2_int64)
        call fields(2)%init(PK_INT64, 2_int64)
        names(1) = "dup"
        names(2) = "dup"
        call sc%adopt_fields(names, fields)
        print '(a)', "adopt_fields accepted a duplicate field name"
    end subroutine scenario_struct_adopt_duplicate_name
    !
    !> A field column of a kind a struct field cannot be. The message names the kind.
    subroutine scenario_struct_adopt_bad_kind()
        type(parquet_struct_column) :: sc
        type(parquet_column), allocatable :: fields(:)
        character(len=3) :: names(1)

        allocate(fields(1))
        call fields(1)%init(PK_INT32_VEC, 2_int64, width=3_int32)
        names(1) = "v  "
        call sc%adopt_fields(names, fields)
        print '(a)', "adopt_fields accepted an unsupported field kind"
    end subroutine scenario_struct_adopt_bad_kind
    !
    !> Field columns of different lengths: the struct would have no single row count, and every
    !! read past the shorter field would be out of bounds.
    subroutine scenario_struct_adopt_ragged_rows()
        type(parquet_struct_column) :: sc
        type(parquet_column), allocatable :: fields(:)
        character(len=3) :: names(2)

        allocate(fields(2))
        call fields(1)%init(PK_INT32, 3_int64)
        call fields(2)%init(PK_INT64, 2_int64)
        names(1) = "a  "
        names(2) = "b  "
        call sc%adopt_fields(names, fields)
        print '(a)', "adopt_fields accepted fields of different lengths"
    end subroutine scenario_struct_adopt_ragged_rows
    !
    !> A `row_valid` mask of the wrong length: silently padding or truncating it would mark the
    !! wrong rows null.
    subroutine scenario_struct_adopt_row_valid_length()
        type(parquet_struct_column) :: sc
        type(parquet_column), allocatable :: fields(:)
        character(len=3) :: names(1)
        logical :: rv(2)

        allocate(fields(1))
        call fields(1)%init(PK_INT32, 3_int64)
        names(1) = "v  "
        rv = [.true., .false.]
        call sc%adopt_fields(names, fields, row_valid=rv)
        print '(a)', "adopt_fields accepted a row_valid of the wrong length"
    end subroutine scenario_struct_adopt_row_valid_length
    !
    !> Writing a field of a column whose field set was never fixed: there is nowhere to put the
    !! value, and inventing a field here would make `%init` optional.
    subroutine scenario_struct_set_field_uninitialized()
        type(parquet_struct_column) :: sc

        call sc%set_field(1_int64, 1, 5_int32)
        print '(a)', "set_field was accepted on an uninitialized struct column"
    end subroutine scenario_struct_set_field_uninitialized
    !
    !> A field index outside `1 .. %field_count()`. The index form is the primitive and takes no
    !! name to check against, so this bound is the only thing standing between a loop that ran one
    !! step too far and a write into a neighbouring field's storage.
    subroutine scenario_struct_field_index_out_of_range()
        type(parquet_struct_column) :: sc

        call sc%init(["a  ", "b  "], [PK_INT32, PK_INT64])
        call sc%append_row()
        call sc%set_field(1_int64, 2, 7_int64)     ! control: the last declared field
        print '(a,i0)', "control: the in-range field was written, fields=", sc%field_count()
        call sc%set_field(1_int64, 3, 9_int64)     ! -> aborts
        print '(a)', "set_field accepted a field index past the last one"
    end subroutine scenario_struct_field_index_out_of_range
    !
    !> A row handle that was never given a column. Default-initialized handles exist, so reading
    !! one has to be refused rather than answering about nothing.
    subroutine scenario_struct_handle_unassociated()
        type(parquet_struct_row) :: h
        logical :: isnull

        isnull = h%is_null()
        print '(a,l1)', "an unassociated row handle answered is_null=", isnull
    end subroutine scenario_struct_handle_unassociated
    !
    !> A handle whose row no longer exists. A handle borrows from its column, so clearing the
    !! column leaves it pointing at a row that is gone -- answering from the old index would read
    !! whatever the storage now holds.
    subroutine scenario_struct_handle_stale_row()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        logical :: isnull

        call sc%init(["v  "], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        isnull = h%is_null()                       ! control: the row is still there
        print '(a,l1)', "control: the live handle answered is_null=", isnull
        call sc%clear()
        isnull = h%is_null()                       ! -> aborts
        print '(a,l1)', "a stale row handle answered is_null=", isnull
    end subroutine scenario_struct_handle_stale_row
    !
    !> Reading a field through the wrong `%get` specific. A type mismatch, never a lookup failure,
    !> so it aborts with no soft-fail option -- the campaign's error-handling convention.
    subroutine scenario_struct_get_wrong_kind()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        call sc%init(["nm"], [PK_STRING])
        call sc%append_row()
        h = sc%view(1)
        call h%get_field("nm", v)
    end subroutine scenario_struct_get_wrong_kind

    !> `%get` on a handle that has not been narrowed. A whole-row handle has no one value to
    !> materialize, so this is a wrong-kind access rather than a lookup that could soft-fail.
    subroutine scenario_struct_get_not_narrowed()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        integer(int32) :: v
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        call h%get(v)
    end subroutine scenario_struct_get_not_narrowed

    !> `%field` on a name the column does not declare, with no `warn=`. The message lists every
    !> declared name, because a misspelled or reordered field is the overwhelmingly common cause.
    subroutine scenario_struct_field_unknown()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, slot
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        slot = h%field("nope")
    end subroutine scenario_struct_field_unknown

    !> The NEGATIVE CONTROL for the guard above: the same call with `warn=.true.` must NOT abort,
    !> and must hand back an invalid handle. Without this, a guard that fired unconditionally
    !> would pass every abort scenario written for it.
    subroutine scenario_struct_field_unknown_warn_ok()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h, slot
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        h = sc%view(1)
        slot = h%field("nope", warn=.true.)
        if (slot%is_valid()) error stop "scenario_struct_field_unknown_warn_ok: expected an invalid handle"
        slot = h%field("v", warn=.true.)
        if (.not. slot%is_valid()) error stop "scenario_struct_field_unknown_warn_ok: expected a valid handle"
    end subroutine scenario_struct_field_unknown_warn_ok

    !> Writing an uninitialized struct column. Nothing declares its field set, so there is no
    !> Arrow field to build.
    subroutine scenario_struct_write_uninitialized()
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        call parquet_open_writer(writer, "test_run/error_scenario_struct_uninit.parquet")
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_write_uninitialized

    !> Writing a struct column into a schema slot declared as something else.
    subroutine scenario_struct_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        call schema%init(table="struct_mismatch")
        call schema%add_field("s", "int32")
        call parquet_parse_maml(schema)
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        call parquet_open_writer(writer, "test_run/error_scenario_struct_mismatch.parquet", schema)
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_write_type_mismatch

    !> `col_size:` on a struct column. A struct row is ONE instance, so there is no width for the
    !> key to declare. Rejected at schema-validation time, before a writer can ever see it.
    subroutine scenario_struct_col_size_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="struct_colsize")
        call schema%add_field("s", "struct", col_size=3)
        call parquet_parse_maml(schema)
    end subroutine scenario_struct_col_size_rejected

    !> `col_size: auto` on a struct column -- the OTHER arm of the same rule, with its own message.
    !!
    !! Separate from scenario_struct_col_size_rejected (which passes col_size=3) because the two
    !! arms emit different text and the shared tail is all either test used to assert, so one
    !! scenario could not tell them apart. Breaking this arm is SILENT rather than loud: every
    !! container write passes asize = 1 to parquet_resolve_or_check_col_size, so an unrefused
    !! `auto` would be RESOLVED to 1 instead of aborting, and the column would acquire a width it
    !! does not have.
    subroutine scenario_struct_col_size_auto_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="struct_colsize_auto")
        call schema%add_field("s", "struct", col_size=parquet_size_auto)
        call parquet_parse_maml(schema)
    end subroutine scenario_struct_col_size_auto_rejected

    !> schema%set_col_size on a struct column. The setter writes %cinfo directly and reaches no
    !! validator, so it is the one route into a container width that %add_field cannot guard.
    subroutine scenario_struct_set_col_size_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="struct_setcolsize")
        call schema%add_field("s", "struct")
        call schema%set_col_size("s", 4)
    end subroutine scenario_struct_set_col_size_rejected

    !> `qc: min:`/`max:` on a struct column. qc stays scalar-leaf-only by design; refusing the
    !> declaration is what keeps the read and write sides agreeing, since with no such declaration
    !> possible a reader can never be handed one. `qc: miss:` IS supported and applies to ROW
    !> nullness, which is the same concept at the same granularity.
    subroutine scenario_struct_qc_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="struct_qc")
        call schema%add_field("s", "struct", qc_min="0")
        call parquet_parse_maml(schema)
    end subroutine scenario_struct_qc_rejected

    !> A protected struct column holding a null ROW. A protected column may contain no Null at any
    !> level, and the row level is parquet_check_protected's ordinary job.
    subroutine scenario_struct_protected_row_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        call schema%init(table="struct_prot_row")
        call schema%add_field("s", "struct")
        call parquet_parse_maml(schema)
        call schema%set_protected("s")
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 1_int32)
        call sc%append_null_row()
        call parquet_open_writer(writer, "test_run/error_scenario_struct_prot_row.parquet", schema)
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_protected_row_null

    !> A protected struct column holding a null FIELD of a PRESENT row. The row level is clean, so
    !> only the field-level half of the protected check can catch this -- and its message names
    !> which field failed.
    subroutine scenario_struct_protected_field_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        call schema%init(table="struct_prot_fld")
        call schema%add_field("s", "struct")
        call parquet_parse_maml(schema)
        call schema%set_protected("s")
        call sc%init(["v", "w"], [PK_INT32, PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 1_int32)   ! `w` left null: the row is present, the field is not
        call parquet_open_writer(writer, "test_run/error_scenario_struct_prot_fld.parquet", schema)
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_protected_field_null

    !> The NEGATIVE CONTROL for both protected guards: a protected struct column with no Null at
    !> EITHER level must write cleanly. Without it, a guard that fired unconditionally would pass
    !> the two scenarios above while breaking every legitimate protected write.
    subroutine scenario_struct_protected_ok()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        call schema%init(table="struct_prot_ok")
        call schema%add_field("s", "struct")
        call parquet_parse_maml(schema)
        call schema%set_protected("s")
        call sc%init(["v", "w"], [PK_INT32, PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 1_int32)
        call sc%set_field(1, "w", 2_int32)
        call parquet_open_writer(writer, "test_run/error_scenario_struct_prot_ok.parquet", schema)
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_protected_ok

    !> A struct TIMESTAMP field carrying sub-microsecond precision must be refused on write.
    !!
    !! A struct field's temporal unit cannot be declared -- the MAML token is a bare `struct` --
    !! so the write is always at microseconds and a nanosecond-of-second that is not a whole
    !! microsecond has nowhere to go. It aborts rather than truncating, and the guard in
    !! push_struct_field (src/parquet_write_struct.f90) is what makes the message name the field
    !! and the file instead of naming parquet_timestamp%to_unix and suggesting an `exact=.false.`
    !! argument this path cannot pass. Its negative control is
    !! scenario_struct_temporal_precision_ok, which writes the same field one nanosecond-count
    !! later -- a whole microsecond -- and must exit cleanly.
    subroutine scenario_struct_timestamp_precision()
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        type(parquet_timestamp) :: ts
        call ts%set(2024, 3, 1, 12, 0, 0, 123456789)
        call sc%init(["when"], [PK_TIMESTAMP])
        call sc%append_row()
        call sc%set_field(1, "when", ts)
        call parquet_open_writer(writer, "test_run/error_scenario_struct_ts_precision.parquet")
        call parquet_write_column(writer, "ev", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_timestamp_precision

    !> The same guard with a MILLISECOND unit, which only a map write can declare.
    !>
    !> A struct field's unit cannot be declared at all, so `scenario_struct_timestamp_precision`
    !> above always measures against microseconds -- the unit a bare `timestamp` resolves to. The
    !> divisor comes from a per-unit table that is a second copy of `unit_scale`
    !> (src/parquet_temporal.f90), and only a declared unit tells the two apart: 500 MICROseconds is
    !> a whole number of microseconds, so it goes through the scenario above and must be refused
    !> here. The control is the same instant on a whole millisecond, written first.
    subroutine scenario_map_write_millisecond_precision()
        type(parquet_writer) :: writer
        type(parquet_schema) :: sch
        type(parquet_map_column) :: mc
        type(parquet_timestamp) :: ts(1)

        call sch%init("map_ms")
        call sch%add_field("m", "map[timestamp[ms]]")
        call parquet_parse_maml(sch)
        call ts(1)%set(2024, 3, 1, 12, 0, 0, 7000000)   ! 7 ms exactly: accepted
        call mc%init(PK_TIMESTAMP)
        call mc%append_row(["a"], ts(1:1))
        call parquet_open_writer(writer, "test_run/error_scenario_map_ms_ok.parquet", sch)
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
        print '(a)', "a whole-millisecond map value was written"

        call ts(1)%set(2024, 3, 1, 12, 0, 0, 500000)    ! 500 us: whole microseconds, not milliseconds
        call mc%clear()
        call mc%init(PK_TIMESTAMP)
        call mc%append_row(["a"], ts(1:1))
        call parquet_open_writer(writer, "test_run/error_scenario_map_ms_bad.parquet", sch)
        call parquet_write_column(writer, "m", mc)   ! -> aborts
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a sub-millisecond map value"
    end subroutine scenario_map_write_millisecond_precision

    !> **Every valid element a NaN, under a declared qc range.** A NaN is already a violation --
    !> every comparison against one is false -- but it is not a number an observed range can order,
    !> and it is deliberately kept out of the running min/max: `min`/`max` over a quiet NaN compile
    !> to `minsd`/`maxsd`, which raise IEEE_INVALID and end the process under nagfor's default
    !> traps, in an optimised build only. So when nothing else was seen the range has to be
    !> RECONSTRUCTED from the NaN that was set aside, or the warning would report `[0, 0]` -- a real
    !> observed range, and the wrong one, for a column that holds no number at all.
    !>
    !> A warning, not an abort: the exit status is 0 and the message is the whole assertion.
    subroutine scenario_qc_all_nan_range()
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        real(real32) :: v(3)

        v = ieee_value(0.0_real32, ieee_quiet_nan)
        call schema%init(table="qc_all_nan")
        call schema%add_field("x", "float32", qc_min="0", qc_max="10")
        call parquet_parse_maml(schema)
        call parquet_open_writer(writer, "test_run/error_scenario_qc_all_nan.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "x", v)
        call parquet_close_writer(writer)
        print '(a)', "the all-NaN qc column was written"
    end subroutine scenario_qc_all_nan_range

    !> The same refusal for a struct TIME field, which reaches it by a different route.
    !!
    !! A time field crosses to C++ as canonical nanoseconds-of-day and is refused by
    !! build_time_array (src/parquet_wrapper.cpp), so this aborts through the C++ fatal path
    !! (exit 134) where its timestamp sibling aborts through Fortran. Both are asserted, because
    !! it is the pair that shows the two kinds now fail with the same shape of message.
    subroutine scenario_struct_time_precision()
        type(parquet_writer) :: writer
        type(parquet_struct_column) :: sc
        type(parquet_time) :: tm
        call tm%set(12, 34, 56, 123456789)
        call sc%init(["when"], [PK_TIME])
        call sc%append_row()
        call sc%set_field(1, "when", tm)
        call parquet_open_writer(writer, "test_run/error_scenario_struct_time_precision.parquet")
        call parquet_write_column(writer, "ev", sc)
        call parquet_close_writer(writer)
    end subroutine scenario_struct_time_precision

    !> The NEGATIVE CONTROL for the two scenarios above: microsecond-exact temporal fields write.
    !!
    !! Without it both refusals pass just as happily against a struct write that rejected every
    !! temporal field, which is the failure this pair exists to distinguish. It writes BOTH kinds
    !! in one struct, so a regression in either guard is caught here as well as by its own
    !! scenario, and it reads the timestamp back to show the value survived rather than merely
    !! that nothing aborted.
    subroutine scenario_struct_temporal_precision_ok()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_struct_column) :: sc
        type(parquet_struct_column), target :: back
        type(parquet_struct_row) :: row
        type(parquet_timestamp) :: ts, got
        type(parquet_time) :: tm
        integer(int64) :: secs
        integer(int32) :: nanos
        call ts%set(2024, 3, 1, 12, 0, 0, 123456000)
        call tm%set(12, 34, 56, 123456000)
        call sc%init(["when", "clok"], [PK_TIMESTAMP, PK_TIME])
        call sc%append_row()
        call sc%set_field(1, "when", ts)
        call sc%set_field(1, "clok", tm)
        call parquet_open_writer(writer, "test_run/error_scenario_struct_temporal_ok.parquet")
        call parquet_write_column(writer, "ev", sc)
        call parquet_close_writer(writer)
        call parquet_open_reader(reader, "test_run/error_scenario_struct_temporal_ok.parquet")
        call parquet_read_column(reader, "ev", back)
        call parquet_close_reader(reader)
        row = back%view(1_int64)
        call row%get_field("when", got)
        call got%get_raw(secs, nanos)
        if (nanos /= 123456000) then
            print '(a,i0)', "microsecond-exact timestamp field did not survive the round trip: ", nanos
        end if
    end subroutine scenario_struct_temporal_precision_ok

    ! ==================================================================================
    ! MAP column scenarios
    ! ==================================================================================
    !
    ! A map has TWO null levels (the row, and each value) where a struct has 1 + M, and a key is
    ! never null -- so the protected trio below checks the row and the value and nothing else.
    ! Every soft-fail guard has BOTH arms here: the hard abort, and a `_warn_ok` negative control
    ! proving the same call returns when asked to.

    !> `%init` with a value kind a map cannot hold at all.
    !!
    !! Uses a `*_VEC` kind rather than a container kind, because the two are refused by the same
    !! gate with DIFFERENT messages since Phase 7: a container value is nesting and points the
    !! caller at `%adopt_values` (see `map_init_nested_value` beside this), while a vector value is
    !! `map<string,fixed_size_list<...>>` and has no route at all. Testing only one of them would
    !! leave the other's message unasserted.
    subroutine scenario_map_init_bad_kind()
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32_VEC)
    end subroutine scenario_map_init_bad_kind

    ! ==================================================================================
    ! Nested-container refusals (Phase 7: nesting is READ-ONLY, and descent paths are not
    ! filterable). Every one has a NEGATIVE CONTROL beside it, because a guard that fired
    ! unconditionally would pass each of these while breaking the case it was meant to allow.
    ! ==================================================================================

    !> Writing a `list<struct<...>>` read back from a file. Reading it is supported; writing is not.
    subroutine scenario_write_nested_list()
        type(parquet_reader) :: reader
        type(parquet_writer) :: w
        type(parquet_schema) :: sch
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "list_of_struct", lc)
        call parquet_close_reader(reader)
        call sch%init(table="t")
        call sch%add_field("c", "list[int32]")
        call parquet_parse_maml(sch)
        call parquet_open_writer(w, "test_run/error_scenario_write_nested_list.parquet", sch)
        call parquet_write_column(w, "c", lc)
        call parquet_close_writer(w)
    end subroutine scenario_write_nested_list

    !> The NEGATIVE CONTROL: the same write of a NON-nested list must still succeed. Without it,
    !> a guard that refused every list column would pass the scenario above.
    subroutine scenario_write_nested_list_control()
        type(parquet_reader) :: reader
        type(parquet_writer) :: w
        type(parquet_schema) :: sch
        type(parquet_list_column) :: lc
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "list_col", lc)
        call parquet_close_reader(reader)
        call sch%init(table="t")
        call sch%add_field("c", "list[int32]")
        call parquet_parse_maml(sch)
        call parquet_open_writer(w, "test_run/error_scenario_write_nested_list_ok.parquet", sch)
        call parquet_write_column(w, "c", lc)
        call parquet_close_writer(w)
    end subroutine scenario_write_nested_list_control

    !> Writing a struct whose FIELD is a list. The message must name the field, not only the column.
    subroutine scenario_write_nested_struct_field()
        type(parquet_reader) :: reader
        type(parquet_writer) :: w
        type(parquet_schema) :: sch
        type(parquet_struct_column) :: sc
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "struct_of_list", sc)
        call parquet_close_reader(reader)
        call sch%init(table="t")
        call sch%add_field("c", "struct")
        call parquet_parse_maml(sch)
        call parquet_open_writer(w, "test_run/error_scenario_write_nested_struct.parquet", sch)
        call parquet_write_column(w, "c", sc)
        call parquet_close_writer(w)
    end subroutine scenario_write_nested_struct_field

    !> A `parquet_filter` rule naming a DESCENT path. `qc:` and `parquet_filter` are scalar-leaf-only
    !> permanently, so the grammar Phase 7 added must not leak into either.
    !> A qc: rule naming a DESCENT path. Refused by `parquet_reader_set_qc`
    !> (src/parquet_wrapper.cpp), the third of three identical `path_has_descent` guards -- the
    !> other two sit on the filter and sort-key paths and each already had a scenario.
    !>
    !! A descent path has one entry per ELEMENT while qc is evaluated per ROW, so an accepted rule
    !! would check a different population than the caller asked about. It is a hard error rather
    !! than a skip, because an ignored qc rule is a check the caller believes is running.
    !!
    !! The path must EXIST in the file for the guard to be reached at all: `parquet_reader_set_qc`
    !! skips a qc-maml entry naming an absent column before it tests for descent.
    subroutine scenario_qc_descent_path()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_descent.maml", [character(len=48) :: &
            "fields:", "- name: list_of_struct[].x", "  qc:", "    min: '>= 0'"])

        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_descent.maml"))
        print '(a)', "unexpectedly opened a reader with a qc: rule on a descent path"
    end subroutine scenario_qc_descent_path

    !> The NEGATIVE CONTROL for `qc_descent_path`: the same qc: rule on an ORDINARY scalar leaf of
    !> the same file is accepted, so the refusal above is about the descent path and not about the
    !> fixture, the qc-maml or the bound.
    subroutine scenario_qc_descent_path_control()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_descent_ctl.maml", [character(len=48) :: &
            "fields:", "- name: struct_of_struct.inner.a", "  qc:", "    min: '>= 0'"])

        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_descent_ctl.maml"))
        call parquet_close_reader(reader)
    end subroutine scenario_qc_descent_path_control

    !> The bare `list` token, the twin of `maml_map_bare_token`.
    !>
    !! `parse_container_token` treats "list" and "map" identically, so the map scenario covers the
    !! shared path today -- but the bare `struct` token is already a special case inside that same
    !! function, so a future special case for `list` would otherwise go unnoticed.
    subroutine scenario_maml_list_bare_token()
        type(parquet_schema) :: schema
        call schema%init(table="list_bare")
        call schema%add_field("v", "list")
        call parquet_parse_maml(schema)
    end subroutine scenario_maml_list_bare_token

    !> `%row_index` on a default-constructed `parquet_list_row`.
    !>
    !! It was `pure` and therefore could not `error stop`, so it answered 0 -- a plausible-looking
    !! wrong answer, and the one accessor on the handle that did not guard. It is now impure and
    !! guarded, matching `parquet_struct_row%row_index`.
    subroutine scenario_list_row_index_unassigned()
        type(parquet_list_row) :: row
        integer(int64) :: i
        i = row%row_index()
        print '(a,i0)', "unexpectedly read a row index from an unassigned list handle: ", i
    end subroutine scenario_list_row_index_unassigned

    !> The map twin of `list_row_index_unassigned`; `pmr_row_index` carried the same defect.
    subroutine scenario_map_row_index_unassigned()
        type(parquet_map_row) :: row
        integer(int64) :: i
        i = row%row_index()
        print '(a,i0)', "unexpectedly read a row index from an unassigned map handle: ", i
    end subroutine scenario_map_row_index_unassigned

    !> The NEGATIVE CONTROL for both: `%row_index` on a LIVE handle still answers, so the guard
    !> fires on a dead handle rather than unconditionally.
    subroutine scenario_list_row_index_live()
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32])
        row = lc%view(2_int64)
        if (row%row_index() /= 2_int64) then
            print '(a)', "a live handle reported the wrong row index"
            error stop 1
        end if
    end subroutine scenario_list_row_index_live

    subroutine scenario_filter_descent_path()
        type(parquet_reader) :: reader
        type(parquet_filter) :: flt
        call flt%add("list_of_struct[].x > 1")
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", filter=flt)
        call parquet_close_reader(reader)
    end subroutine scenario_filter_descent_path

    !> The NEGATIVE CONTROL: a filter on an ORDINARY scalar leaf of the same file still works.
    subroutine scenario_filter_descent_path_control()
        type(parquet_reader) :: reader
        type(parquet_filter) :: flt
        call flt%add("struct_of_struct.inner.a > 1")
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", filter=flt)
        call parquet_close_reader(reader)
    end subroutine scenario_filter_descent_path_control

    !> A read-time sort key naming a DESCENT path: it has one entry per ELEMENT, not per row.
    subroutine scenario_sort_key_descent_path()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        call srt%add("list_of_struct[].x asc")
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet", sort_by=srt)
        call parquet_close_reader(reader)
    end subroutine scenario_sort_key_descent_path

    !> `%init` with a CONTAINER value kind: refused, and the message names the route that works.
    !!
    !! `%init` is handed one `PK_*` discriminator; a nested value is a kind PLUS an inner schema,
    !! so `%init(PK_LIST)` could only produce a map whose values are a list column with no payload
    !! kind -- an unusable state with no way out of it. Nesting is reachable through
    !! `%adopt_container` + `%adopt_rows` instead.
    subroutine scenario_map_init_nested_value()
        type(parquet_map_column) :: mc
        call mc%init(PK_LIST)
    end subroutine scenario_map_init_nested_value

    !> The list twin of `map_init_nested_value`.
    subroutine scenario_list_init_nested_payload()
        type(parquet_list_column) :: lc
        call lc%init(PK_STRUCT)
    end subroutine scenario_list_init_nested_payload

    !> The struct twin of `map_init_nested_value`, which additionally names the offending FIELD.
    subroutine scenario_struct_init_nested_field()
        type(parquet_struct_column) :: sc
        call sc%init(["v", "w"], [PK_INT32, PK_LIST])
    end subroutine scenario_struct_init_nested_field

    !> A struct field that is ITSELF a struct, read from a file.
    !!
    !! Not one of the four shapes Phase 7's scope (c) covers, and it cannot be added without making
    !! an intermediate struct addressable by a dotted path -- which this library refuses on purpose.
    !! The refusal must therefore name the mechanism that DOES work rather than merely decline.
    subroutine scenario_struct_read_nested_struct_field()
        type(parquet_reader) :: reader
        type(parquet_struct_column) :: sc
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "struct_of_struct", sc)
        print '(a)', "unexpectedly read a struct whose field is itself a struct"
    end subroutine scenario_struct_read_nested_struct_field

    !> `%append_row` with values of a kind the column was not initialized to. A wrong VALUE KIND is
    !> a type mismatch rather than a lookup failure, so it is always hard.
    subroutine scenario_map_append_wrong_kind()
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1.5_real64])
    end subroutine scenario_map_append_wrong_kind

    !> `%append_row` with more keys than values. The two arrays are index-aligned by contract, and
    !> a mismatch would otherwise show up later as a wrong answer rather than as an error here.
    subroutine scenario_map_append_length_mismatch()
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32])
    end subroutine scenario_map_append_length_mismatch

    !> `%get` with a variable of the wrong type. Always hard, with no `warn=`/`found=` escape:
    !> softening it would let a caller read an int32 map with a real64 accessor and be told only
    !> that the key was "not found".
    subroutine scenario_map_get_wrong_kind()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        real(real64) :: v
        logical :: got
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(1_int64)
        ! found= is supplied deliberately: it must NOT soften a kind mismatch.
        call row%get("a", v, found=got)
    end subroutine scenario_map_get_wrong_kind

    !> A missing key with NEITHER `warn=` nor `found=`. Aborting is the only truthful outcome --
    !> returning silently would leave the caller with no way to learn the lookup failed at all.
    subroutine scenario_map_get_missing_key()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(1_int64)
        call row%get("nope", v)
    end subroutine scenario_map_get_missing_key

    !> The NEGATIVE CONTROL for every soft-fail guard in this module: the same three lookups that
    !> abort above must RETURN when `warn=`/`found=` is given. Without it a guard that fired
    !> unconditionally would pass every abort scenario while breaking every legitimate lookup.
    subroutine scenario_map_get_missing_key_warn_ok()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        character(len=:), allocatable :: k
        logical :: got
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(1_int64)
        call row%get("nope", v, warn=.true.)
        call row%get("nope", v, found=got)
        call row%get_at(9, v, warn=.true.)
        call row%key_at(9, k, found=got)
        ! And the permitted cases still work, which is the other half of the control.
        call row%get("a", v, found=got)
        if (.not. got .or. v /= 1_int32) error stop "map_get_missing_key_warn_ok: a present key stopped working"
    end subroutine scenario_map_get_missing_key_warn_ok

    !> `occurrence=0` is a caller mistake rather than a failed lookup, so it is hard whatever
    !> `warn=`/`found=` say -- the same split the wrong-value-kind guard makes.
    subroutine scenario_map_get_occurrence_zero()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        logical :: got
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(1_int64)
        call row%get("a", v, occurrence=0, found=got)
    end subroutine scenario_map_get_occurrence_zero

    !> `%get_at` past the end of the row, with neither soft-fail argument.
    subroutine scenario_map_get_at_out_of_range()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(1_int64)
        call row%get_at(5, v)
    end subroutine scenario_map_get_at_out_of_range

    !> `%view` on a row index the column does not have.
    subroutine scenario_map_view_out_of_range()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        row = mc%view(7_int64)
    end subroutine scenario_map_view_out_of_range

    !> Reading a column that is not a map at all into a `parquet_map_column`.
    subroutine scenario_map_read_not_a_map()
        type(parquet_reader) :: reader
        type(parquet_map_column) :: mc
        call parquet_open_reader(reader, "test/fixtures/map_payloads.parquet")
        call parquet_read_column(reader, "rowid", mc)
        call parquet_close_reader(reader)
    end subroutine scenario_map_read_not_a_map

    !> A map keyed by something other than a string. V1 keys are strings, and rendering an int32
    !> key as text would silently change the data -- `1`, `01` and `1.0` are three different keys
    !> -- so this is a clean refusal NAMING THE KEY TYPE rather than a coercion.
    !>
    !> **When non-string keys are ever supported, this becomes a positive read test** asserting
    !> that `m_intkey`'s two entries come back with their integer keys, not a deleted scenario;
    !> the fixture column exists for exactly that succession.
    subroutine scenario_map_read_int_key()
        type(parquet_reader) :: reader
        type(parquet_map_column) :: mc
        call parquet_open_reader(reader, "test/fixtures/map_payloads.parquet")
        call parquet_read_column(reader, "m_intkey", mc)
        call parquet_close_reader(reader)
    end subroutine scenario_map_read_int_key

    !> A map whose VALUE is itself a container (`map<string, list<int32>>`), which Phase 7 reads.
    !!
    !! Inverted rather than deleted, as its two list twins were. The KEYS are asserted alongside the
    !! value kind on purpose: a map's keys and values cross the boundary through different calls,
    !! and the nested path changes only the second, so a keys regression would otherwise be
    !! invisible here.
    subroutine scenario_map_read_nested_value()
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        character(len=:), allocatable :: kt, k1
        type(parquet_map_row) :: h
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "map_of_list", mc)
        call mc%kind_text(kt)
        if (kt /= "map<string,list<int32>>") error stop "map_read_nested_value: kind_text is "//kt
        if (mc%nrows() /= 3_int64) error stop "map_read_nested_value: expected three rows"
        h = mc%view(1_int64)
        if (h%size() > 0_int64) then
            call h%key_at(1, k1)
            if (len_trim(k1) == 0) error stop "map_read_nested_value: row 1 key is empty"
        end if
        call parquet_close_reader(reader)
    end subroutine scenario_map_read_nested_value

    !> Writing a map column that `%init` has never been called on: there is no value kind to
    !> declare, so the file's schema cannot be built.
    subroutine scenario_map_write_uninitialized()
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call parquet_open_writer(writer, "test_run/error_scenario_map_uninit.parquet")
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_write_uninitialized

    !> A schema declaring `map[int32]` handed a `map<string,string>` column. A map's value type
    !> must match exactly; there is no widening between map value kinds.
    subroutine scenario_map_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call schema%init(table="map_mismatch")
        call schema%add_field("m", "map[int32]")
        call parquet_parse_maml(schema)
        call mc%init(PK_STRING)
        call mc%append_row(["a"], ["x"])
        call parquet_open_writer(writer, "test_run/error_scenario_map_mismatch.parquet", schema)
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_write_type_mismatch

    !> `col_size:` on a map column. A map row's entry count comes from the DATA, so there is
    !> nothing for col_size to declare and nothing for it to be resolved from -- the same rule a
    !> list column's declaration follows.
    subroutine scenario_map_col_size_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="map_colsize")
        call schema%add_field("m", "map[int32]", col_size=3)
        call parquet_parse_maml(schema)
    end subroutine scenario_map_col_size_rejected

    !> `col_size: auto` on a map column; the map arm of the pair described on
    !! scenario_struct_col_size_auto_rejected.
    subroutine scenario_map_col_size_auto_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="map_colsize_auto")
        call schema%add_field("m", "map[int32]", col_size=parquet_size_auto)
        call parquet_parse_maml(schema)
    end subroutine scenario_map_col_size_auto_rejected

    !> `qc: min:`/`max:` on a map column. qc stays scalar-leaf-only by design; `qc: miss:` IS
    !> supported and applies to ROW nullness, which is the same concept at the same granularity.
    subroutine scenario_map_qc_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="map_qc")
        call schema%add_field("m", "map[int32]", qc_min="0")
        call parquet_parse_maml(schema)
    end subroutine scenario_map_qc_rejected

    !> A protected map column holding a null ROW.
    subroutine scenario_map_protected_row_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call schema%init(table="map_prot_row")
        call schema%add_field("m", "map[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("m")
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_null_row()
        call parquet_open_writer(writer, "test_run/error_scenario_map_prot_row.parquet", schema)
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_protected_row_null

    !> A protected map column holding a null VALUE. At the Parquet level that IS a Null in this
    !> map's value leaf column, so the weaker reading would declare a non-nullable field for a
    !> column that can contain nulls -- the invariant build_field's safety comment forbids.
    subroutine scenario_map_protected_value_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call schema%init(table="map_prot_val")
        call schema%add_field("m", "map[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("m")
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [0_int32], is_valid=[.false.])
        call parquet_open_writer(writer, "test_run/error_scenario_map_prot_val.parquet", schema)
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_protected_value_null

    !> The NEGATIVE CONTROL for both protected guards: a protected map column with no Null at
    !> EITHER level must write cleanly. A present-but-EMPTY row is deliberately included -- it is
    !> not a null row, and a guard that treated "no entries" as "absent" would refuse it.
    subroutine scenario_map_protected_ok()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call schema%init(table="map_prot_ok")
        call schema%add_field("m", "map[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("m")
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_empty_row()
        call parquet_open_writer(writer, "test_run/error_scenario_map_prot_ok.parquet", schema)
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_protected_ok

    !> The int32 entry ceiling, reached with a tiny fixture through the test-only override.
    !>
    !> **Unlike the list and string ceilings this one is a dead end rather than a fork**: Arrow
    !> addresses a map's entries with an int32 offsets buffer and provides NO large_map, so the
    !> only truthful outcomes are "it fits" and "this cannot be written". The list equivalent of
    !> this scenario asserts a WIDENING; this one asserts a refusal.
    subroutine scenario_map_entry_limit()
        interface
            subroutine parquet_debug_set_map_offset_limit(n) bind(C, name="parquet_debug_set_map_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n
            end subroutine parquet_debug_set_map_offset_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32)
        call mc%append_row(["a", "b", "c"], [1_int32, 2_int32, 3_int32])
        ! Two entries allowed, three present: the guard must fire before the narrowing cast, since
        ! a silent wrap there would write a file whose offsets are garbage.
        call parquet_debug_set_map_offset_limit(2_int64)
        call parquet_open_writer(writer, "test_run/error_scenario_map_limit.parquet")
        call parquet_write_column(writer, "m", mc)
        call parquet_close_writer(writer)
    end subroutine scenario_map_entry_limit

    !> `%adopt_rows` whose final offset does not account for the entries handed over. Every
    !> precondition there is fatal because each would otherwise show up later as a wrong answer.
    subroutine scenario_map_adopt_rows_offset_mismatch()
        type(parquet_map_column) :: mc
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        allocate(offs(2))
        offs = [0_int64, 5_int64]     ! claims five entries
        call keys%init(PK_STRING, 0_int64)
        call keys%append_values(["a"])
        call vals%init(PK_INT32, 0_int64)
        call vals%append_values([1_int32])   ! but only one was supplied
        call mc%adopt_rows(offs, keys, vals)
    end subroutine scenario_map_adopt_rows_offset_mismatch

    !> `%adopt_rows` handed a non-string keys column. V1 keys are strings, and the check is here
    !> rather than at the first lookup so the message names the kind that was actually passed.
    subroutine scenario_map_adopt_rows_bad_key_kind()
        type(parquet_map_column) :: mc
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        allocate(offs(2))
        offs = [0_int64, 1_int64]
        call keys%init(PK_INT32, 0_int64)
        call keys%append_values([1_int32])
        call vals%init(PK_INT32, 0_int64)
        call vals%append_values([2_int32])
        call mc%adopt_rows(offs, keys, vals)
    end subroutine scenario_map_adopt_rows_bad_key_kind

    !> A row handle whose row has been dropped out from under it. The handle stores an INDEX, not
    !> a reference, so a structural change can leave it naming a row that no longer exists --
    !> which is a stale read rather than a crash, and is why every accessor is guarded.
    subroutine scenario_list_stale_handle()
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: h
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%append_row([2_int32])
        h = lc%view(2_int64)
        call lc%gather_rows([1_int64])     ! row 2 is gone; the handle still names it
        print '(a,i0)', "the handle now names row ", h%row_index()   ! -> aborts
    end subroutine scenario_list_stale_handle

    !> `%gather_rows` naming a source row that does not exist. Every index is checked BEFORE
    !> anything is rebuilt, so a refused gather leaves the column exactly as it was.
    subroutine scenario_map_gather_out_of_range()
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%gather_rows([1_int64, 7_int64])   ! -> aborts (no row 7)
        print '(a,i0)', "unexpectedly gathered a row that does not exist, nrows=", mc%nrows()
    end subroutine scenario_map_gather_out_of_range

    !> `%append_from` between two maps whose VALUE kinds differ. The two entry columns are
    !> appended wholesale, so a mismatch would leave the destination holding values of two kinds
    !> with nothing recording which row has which -- the message names both.
    subroutine scenario_map_append_from_kind_mismatch()
        type(parquet_map_column) :: dst, src
        call dst%init(PK_INT32)
        call dst%append_row(["a"], [1_int32])
        call src%init(PK_FLOAT64)
        call src%append_row(["b"], [2.0_real64])
        call dst%append_from(src)     ! -> aborts
        print '(a,i0)', "unexpectedly appended a float64 map onto an int32 one, nrows=", dst%nrows()
    end subroutine scenario_map_append_from_kind_mismatch

    !> `%append_from` handed a container that is not a map at all. The argument is declared
    !> `class(parquet_container_column)` so the table layer can walk containers generically, which
    !> is exactly what makes this reachable: the `class default` arm is the type check.
    subroutine scenario_map_append_from_not_a_map()
        type(parquet_map_column) :: dst
        type(parquet_list_column) :: src
        call dst%init(PK_INT32)
        call src%init(PK_INT32)
        call src%append_row([1_int32])
        call dst%append_from(src)     ! -> aborts
        print '(a,i0)', "unexpectedly appended a list onto a map, nrows=", dst%nrows()
    end subroutine scenario_map_append_from_not_a_map

    !> `%adopt_rows` handed a values column whose kind cannot be a map value at all.
    subroutine scenario_map_adopt_rows_bad_value_kind()
        type(parquet_map_column) :: mc
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        allocate(offs(2))
        offs = [0_int64, 1_int64]
        call keys%init(PK_STRING, 0_int64)
        call keys%append_values(["a"])
        ! `vals` is left DEFAULT-INITIALIZED, which is a PK_NONE column -- the one kind that is
        ! neither a supported scalar value nor a container, and so the only one this arm sees.
        call mc%adopt_rows(offs, keys, vals)   ! -> aborts
        print '(a,i0)', "unexpectedly adopted an unsupported value kind, nrows=", mc%nrows()
    end subroutine scenario_map_adopt_rows_bad_value_kind

    !> `%adopt_rows` handed keys and values of different lengths. Entry `j` is key `j` paired with
    !> value `j`, so a length mismatch means some entries have one half and not the other -- and
    !> nothing downstream could tell which.
    subroutine scenario_map_adopt_rows_length_mismatch()
        type(parquet_map_column) :: mc
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        allocate(offs(2))
        offs = [0_int64, 1_int64]
        call keys%init(PK_STRING, 0_int64)
        call keys%append_values(["a", "b"])   ! two keys
        call vals%init(PK_INT32, 0_int64)
        call vals%append_values([1_int32])    ! one value
        call mc%adopt_rows(offs, keys, vals)  ! -> aborts
        print '(a,i0)', "unexpectedly adopted mismatched entry counts, nrows=", mc%nrows()
    end subroutine scenario_map_adopt_rows_length_mismatch

    !> `%adopt_rows` handed a `row_valid` of the wrong length. It is one flag per ROW, and a short
    !> one would leave the trailing rows' nullness read from whatever the caller's array did not
    !> cover.
    subroutine scenario_map_adopt_rows_row_valid_length()
        type(parquet_map_column) :: mc
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        allocate(offs(3))
        offs = [0_int64, 1_int64, 2_int64]    ! two rows
        call keys%init(PK_STRING, 0_int64)
        call keys%append_values(["a", "b"])
        call vals%init(PK_INT32, 0_int64)
        call vals%append_values([1_int32, 2_int32])
        call mc%adopt_rows(offs, keys, vals, row_valid=[.true.])   ! -> aborts (one flag, two rows)
        print '(a,i0)', "unexpectedly adopted a short row_valid, nrows=", mc%nrows()
    end subroutine scenario_map_adopt_rows_row_valid_length

    !> `%append_from` onto a column that has never been `%init`ed. The value kind is what makes an
    !> append meaningful, and the source's cannot supply it: a destination that silently took the
    !> source's kind would make the first append decide the column's type.
    subroutine scenario_map_append_from_uninitialized()
        type(parquet_map_column) :: dst, src
        call src%init(PK_INT32)
        call src%append_row(["a"], [1_int32])
        call dst%append_from(src)     ! -> aborts (dst has no value kind)
        print '(a,i0)', "unexpectedly appended onto an uninitialized map, nrows=", dst%nrows()
    end subroutine scenario_map_append_from_uninitialized

    !> A map row handle whose row has been dropped out from under it; the map twin of
    !> `scenario_list_stale_handle`, and a separate guard in a separate type.
    subroutine scenario_map_stale_handle()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: h
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_row(["b"], [2_int32])
        h = mc%view(2_int64)
        call mc%gather_rows([1_int64])     ! row 2 is gone; the handle still names it
        print '(a,i0)', "the handle now names row ", h%row_index()   ! -> aborts
    end subroutine scenario_map_stale_handle

    !> `%append_row` handed an `is_valid` of a different length from the values. It is one flag per
    !> VALUE, and a short one would leave the trailing entries' nullness undefined.
    subroutine scenario_map_append_is_valid_length()
        type(parquet_map_column) :: mc
        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32, 2_int32], is_valid=[.true.])   ! -> aborts
        print '(a,i0)', "unexpectedly accepted a short is_valid, entries=", mc%total_entries()
    end subroutine scenario_map_append_is_valid_length

    !> A lookup message quoting a key longer than the cap. Caller-supplied text inside an
    !> `error stop` message is truncated, per CLAUDE.md: ifx's ERROR STOP runtime corrupts the heap
    !> once the composed message reaches 8192 bytes, and the key is entirely the caller's.
    subroutine scenario_map_missing_key_preview()
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: h
        character(len=300) :: longkey
        integer(int32) :: v
        longkey = repeat("k", 300)
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        h = mc%view(1_int64)
        call h%get(longkey, v)     ! -> aborts, quoting a truncated key
        print '(a,i0)', "unexpectedly found a 300-character key, v=", v
    end subroutine scenario_map_missing_key_preview

    !> A chunked map read while a read-time sort is installed. Every row-group-scoped operation
    !> refuses, because a permutation destroys row-group locality -- the map specific inherits
    !> that and must not weaken it.
    subroutine scenario_map_chunk_refuses_sort()
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        type(parquet_map_column) :: mc
        call srt%add("rowid desc")
        call parquet_open_reader(reader, "test/fixtures/map_payloads.parquet", sort_by=srt)
        call parquet_read_column_chunk(reader, "m_int32", 1_int64, mc)
        call parquet_close_reader(reader)
    end subroutine scenario_map_chunk_refuses_sort

    !> The bare `map` token, with no value type. Unlike the bare `struct` token it is INVALID: a
    !> struct's field layout genuinely cannot be expressed in MAML, whereas a map's value type is
    !> a single token the schema can perfectly well carry -- and a declared-but-unwritten map
    !> column has to be written with zero rows at close, which cannot invent a value kind.
    subroutine scenario_maml_map_bare_token()
        type(parquet_schema) :: schema
        call schema%init(table="map_bare")
        call schema%add_field("m", "map")
        call parquet_parse_maml(schema)
    end subroutine scenario_maml_map_bare_token

    !> A map token whose value is itself a container. `valid_maml_data_types` deliberately holds
    !> no container token, which is what rejects this for free -- and is the same mechanism that
    !> keeps `list[struct]` out.
    subroutine scenario_maml_map_nested_value()
        type(parquet_schema) :: schema
        call schema%init(table="map_nested")
        call schema%add_field("m", "map[list[int32]]")
        call parquet_parse_maml(schema)
    end subroutine scenario_maml_map_nested_value

    !> The same two refusals reached through MAML TEXT rather than through `%add_field`.
    !>
    !> `%add_field` validates its token before it writes a single line, so a container token it
    !> refuses never reaches the line parser at all -- and the line parser has its own arm for one:
    !> a well-shaped `list[...]`/`map[...]` whose inner type is not a type is stored VERBATIM, so
    !> that `parquet_validate_maml` can name what was written instead of a canonical form nobody
    !> asked for. A `.maml` file on disk is exactly this route, which is why the arm exists.
    subroutine scenario_maml_text_list_bad_element()
        type(parquet_schema) :: schema
        schema%maml%name = "text_list_bad.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: text_list_bad", &
            "fields:", &
            "- name: l", &
            "  data_type: list[list[int32]]" ]
        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly parsed a list token whose element is a container"
    end subroutine scenario_maml_text_list_bad_element

    !> The map twin of the scenario above, one level of container up.
    subroutine scenario_maml_text_map_bad_value()
        type(parquet_schema) :: schema
        schema%maml%name = "text_map_bad.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: text_map_bad", &
            "fields:", &
            "- name: m", &
            "  data_type: map[nosuchtype]" ]
        call parquet_parse_maml(schema)
        print '(a)', "unexpectedly parsed a map token whose value is not a type"
    end subroutine scenario_maml_text_map_bad_value

    !> Reading a struct column whose field is a LIST. Phase 4 refused this and Phase 7 reads it.
    !!
    !! **This scenario was inverted rather than deleted, and it earns its place as the NEGATIVE
    !! CONTROL for `struct_read_nested_struct_field`.** Those two differ in exactly one way -- the
    !! field's own type -- so together they show the surviving refusal is about an intermediate
    !! STRUCT not being addressable by a dotted path, and not about "a struct field that is a
    !! container". Either one alone would leave that ambiguous.
    subroutine scenario_struct_read_nested_field()
        type(parquet_reader) :: reader
        type(parquet_struct_column) :: sc
        call parquet_open_reader(reader, "test/fixtures/map_list_types.parquet")
        call parquet_read_column(reader, "struct_of_list", sc)
        if (sc%nrows() /= 3_int64) error stop "struct_read_nested_field: expected three rows"
        if (sc%field_kind(2) /= PK_LIST) error stop "struct_read_nested_field: field 2 is not a list"
        call parquet_close_reader(reader)
    end subroutine scenario_struct_read_nested_field

    !> Reading a column that is not a struct at all into a parquet_struct_column.
    subroutine scenario_struct_read_not_a_struct()
        type(parquet_reader) :: reader
        type(parquet_struct_column) :: sc
        call parquet_open_reader(reader, "test/fixtures/struct_payloads.parquet")
        call parquet_read_column(reader, "rowid", sc)
        call parquet_close_reader(reader)
    end subroutine scenario_struct_read_not_a_struct

    !> `%view` on a row index the column does not have.
    subroutine scenario_struct_view_out_of_range()
        type(parquet_struct_column), target :: sc
        type(parquet_struct_row) :: h
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        h = sc%view(5)
    end subroutine scenario_struct_view_out_of_range

    !> `%set_field` naming a field the column does not declare. A WRITE has no sensible soft
    !> outcome -- unlike a read, which %field(name, warn=) can decline -- so it always aborts.
    subroutine scenario_struct_set_field_unknown()
        type(parquet_struct_column) :: sc
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "nope", 1_int32)
    end subroutine scenario_struct_set_field_unknown

    !> `%set_field` writing the wrong type into a declared field.
    subroutine scenario_struct_set_field_wrong_kind()
        type(parquet_struct_column) :: sc
        call sc%init(["v"], [PK_INT32])
        call sc%append_row()
        call sc%set_field(1, "v", 1.5_real64)
    end subroutine scenario_struct_set_field_wrong_kind

    subroutine scenario_list_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call schema%init(table="list_write_mismatch")
        call schema%add_field("lst", "list[int64]")
        call parquet_parse_maml(schema)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call parquet_open_writer(writer, "test_run/error_scenario_list_mismatch.parquet", schema)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_type_mismatch

    !> Writing a list column that was never %init'd. Its payload kind is PK_NONE, so there is no
    !> element type to declare, no buffer to hand over and nothing to write -- reported as such
    !> rather than as a type mismatch against a token nobody wrote.
    subroutine scenario_list_write_uninitialized()
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call parquet_open_writer(writer, "test_run/error_scenario_list_uninit.parquet")
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_uninitialized

    !> `col_size:` on a list column. A list row's length comes from the data, so there is nothing
    !> for the key to declare -- and accepting it would leave a file claiming a width no row has
    !> to honour. Rejected at schema-validation time, before a writer can ever see it.
    subroutine scenario_list_write_col_size_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="list_write_colsize")
        call schema%add_field("lst", "list[int32]", col_size=3)
        call parquet_parse_maml(schema)
    end subroutine scenario_list_write_col_size_rejected

    !> `col_size: auto` on a list column; the list arm of the pair described on
    !! scenario_struct_col_size_auto_rejected.
    subroutine scenario_list_col_size_auto_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="list_colsize_auto")
        call schema%add_field("lst", "list[int32]", col_size=parquet_size_auto)
        call parquet_parse_maml(schema)
    end subroutine scenario_list_col_size_auto_rejected

    !> schema%set_col_size(..., force=.true.) on a list column -- the back door that force= used to
    !! open. Before the refusal, this produced a schema parquet_validate_maml would have rejected,
    !! and parquet_write_column then failed with `array size mismatch`, a message naming the
    !! caller's data rather than the declaration. force=.true. is the point: without it the
    !! "already resolved" guard fires first and this would test that instead.
    subroutine scenario_list_set_col_size_forced_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="list_setcolsize")
        call schema%add_field("lst", "list[int32]")
        call schema%set_col_size("lst", 5, force=.true.)
    end subroutine scenario_list_set_col_size_forced_rejected

    !> `qc: min:`/`max:` on a list column, refused on the same terms a temporal column's is: qc
    !> stays scalar-leaf-only by design, and refusing here is what keeps the read and write sides
    !> agreeing -- with no such declaration possible, a reader can never be handed one.
    subroutine scenario_list_write_qc_rejected()
        type(parquet_schema) :: schema
        call schema%init(table="list_write_qc")
        call schema%add_field("lst", "list[int32]", qc_min="0")
        call parquet_parse_maml(schema)
    end subroutine scenario_list_write_qc_rejected

    !> A malformed list token. `list[]` names no element type, and the element type is REQUIRED --
    !> a declared-but-unwritten list column has to be written with zero rows at close, which
    !> cannot invent a payload kind.
    subroutine scenario_list_write_bad_token()
        type(parquet_schema) :: schema
        call schema%init(table="list_write_badtoken")
        call schema%add_field("lst", "list[]")
        call parquet_parse_maml(schema)
    end subroutine scenario_list_write_bad_token

    !> An unknown element type inside an otherwise well-formed list token. Kept separate from
    !> scenario_list_write_bad_token so the two malformed shapes -- no element type, and an
    !> element type nothing recognises -- are both known to be refused.
    subroutine scenario_list_write_unknown_element()
        type(parquet_schema) :: schema
        call schema%init(table="list_write_unknownelem")
        call schema%add_field("lst", "list[complex64]")
        call parquet_parse_maml(schema)
    end subroutine scenario_list_write_unknown_element

    !> A protected list column holding a null ROW. `protected_cols:` declares that the column
    !> contains no Null at all, and a null row is one.
    subroutine scenario_list_write_protected_row_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call schema%init(table="list_write_protrow")
        call schema%add_field("lst", "list[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("lst")
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%append_null_row()
        call parquet_open_writer(writer, "test_run/error_scenario_list_protrow.parquet", schema)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_protected_row_null

    !> A protected list column holding a null ELEMENT inside a present row. At the Parquet level a
    !> null element IS a Null in the same leaf column the row nullness lives in, so the weaker
    !> reading of `protected_cols:` would declare a non-nullable element field for a column that
    !> can contain null elements -- exactly the invariant build_field's safety comment forbids.
    !> Its own message, separate from the row-level one, so a caller is not sent looking at rows.
    subroutine scenario_list_write_protected_element_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call schema%init(table="list_write_protelem")
        call schema%add_field("lst", "list[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("lst")
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32], is_valid=[.true., .false.])
        call parquet_open_writer(writer, "test_run/error_scenario_list_protelem.parquet", schema)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_protected_element_null

    !> A protected list column with no Null at either level: the NEGATIVE CONTROL for the two
    !> scenarios above. Without it, a guard that fired unconditionally would pass both of them
    !> while making every protected list column unwritable.
    subroutine scenario_list_write_protected_ok()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc, back
        call schema%init(table="list_write_protok")
        call schema%add_field("lst", "list[int32]")
        call parquet_parse_maml(schema)
        call schema%set_protected("lst")
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32])
        call parquet_open_writer(writer, "test_run/error_scenario_list_protok.parquet", schema)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
        call parquet_open_reader(reader, "test_run/error_scenario_list_protok.parquet")
        call parquet_read_column(reader, "lst", back)
        call parquet_close_reader(reader)
        if (back%size() /= 2_int64) error stop "a protected null-free list column must write normally"
        if (back%length(1_int64) /= 2_int64) error stop "its first row must keep both elements"
    end subroutine scenario_list_write_protected_ok

    !> One list ROW holding more elements than Parquet's own repetition/definition-level
    !> generation can address. A row is never split across row groups, so no row-group size can
    !> rescue this and it must abort where the array is built -- the list column's counterpart to
    !> a vector column's col_size ceiling. Reached with a tiny fixture by shrinking the limit
    !> through parquet_debug_set_list_element_count_limit, a process-global test-only hook that is
    !> safe here because this scenario is its own isolated subprocess.
    subroutine scenario_list_write_row_too_long()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element ceiling to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call parquet_debug_set_list_element_count_limit(3_int64)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32, 3_int32, 4_int32])
        call parquet_open_writer(writer, "test_run/error_scenario_list_rowlong.parquet")
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_row_too_long

    !> One STREAMED row group holding more elements than that same ceiling. The caller chose this
    !> row group's row count through parquet_new_row_group, so -- like an explicit chunk_size --
    !> it is validated rather than silently overridden, and the message names what to make smaller.
    subroutine scenario_list_write_chunk_too_many_elements()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element ceiling to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        call parquet_debug_set_list_element_count_limit(3_int64)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32, 4_int32])
        call parquet_open_writer(writer, "test_run/error_scenario_list_chunkelems.parquet")
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "lst", lc)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_chunk_too_many_elements

    !> An EXPLICIT chunk_size that would put too many list elements in one row group. Checked at
    !> close, by walking the column's actual offsets at chunk_size stride -- exact, so a caller
    !> whose chunk_size really does fit is never refused however ragged the column is.
    subroutine scenario_list_write_explicit_chunk_size_too_big()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element ceiling to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_list_column) :: lc
        integer(int64) :: i
        call parquet_debug_set_list_element_count_limit(5_int64)
        call lc%init(PK_INT32)
        do i = 1_int64, 8_int64
            call lc%append_row([int(i, int32), int(i, int32) + 100_int32])
        end do
        call parquet_open_writer(writer, "test_run/error_scenario_list_chunksize.parquet", chunk_size=8)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
    end subroutine scenario_list_write_explicit_chunk_size_too_big

    !> The NEGATIVE CONTROL for the three ceiling scenarios above: the same shrunk limit with a
    !> chunk_size that fits. Without it, a guard that fired unconditionally would pass all three
    !> while making every list column unwritable.
    subroutine scenario_list_write_ceiling_ok()
        interface
            subroutine parquet_debug_set_list_element_count_limit(n) &
                bind(C, name="parquet_debug_set_list_element_count_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element ceiling to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_element_count_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_list_column) :: lc, back
        integer(int64) :: i
        call parquet_debug_set_list_element_count_limit(5_int64)
        call lc%init(PK_INT32)
        do i = 1_int64, 8_int64
            call lc%append_row([int(i, int32), int(i, int32) + 100_int32])
        end do
        call parquet_open_writer(writer, "test_run/error_scenario_list_ceilok.parquet", chunk_size=2)
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
        call parquet_debug_set_list_element_count_limit(0_int64)
        call parquet_open_reader(reader, "test_run/error_scenario_list_ceilok.parquet")
        call parquet_read_column(reader, "lst", back)
        call parquet_close_reader(reader)
        if (back%size() /= 8_int64) error stop "a chunk_size within the ceiling must write every row"
        if (back%total_elements() /= 16_int64) error stop "and every element"
    end subroutine scenario_list_write_ceiling_ok

    !> Proves the arrow::large_list() write path round-trips, rather than merely not crashing. A
    !> genuine 2-billion-element column is far too large for the suite, so this shrinks the int32
    !> offsets threshold through parquet_debug_set_list_offset_limit -- the list counterpart of
    !> scenario_large_string_roundtrip's own parquet_debug_set_string_offset_limit use, and safe
    !> as a process-global for the same reason: this scenario is its own isolated subprocess.
    !>
    !> The shape query is what makes the assertion meaningful: a large_list and a list are
    !> indistinguishable in the Parquet file itself (same leaf path, same repetition and definition
    !> levels), so only the values coming back correctly says the wider offsets were written and
    !> read correctly.
    subroutine scenario_list_write_large_list_roundtrip()
        interface
            subroutine parquet_debug_set_list_offset_limit(n) &
                bind(C, name="parquet_debug_set_list_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element threshold to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_offset_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        ! TARGET on `back`: %view hands back a handle whose %col points at it, and F2018 15.5.2.4
        ! leaves that pointer UNDEFINED on return when the actual argument is not a target. Only
        ! nagfor's -C=dangling sees it (`.claude/rules/fortran-gotchas.md`, "nagfor-specific gotchas").
        type(parquet_list_column) :: lc
        type(parquet_list_column), target :: back
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        character(len=:), allocatable :: shape
        call parquet_debug_set_list_offset_limit(3_int64)
        call lc%init(PK_INT32)
        call lc%append_row([10_int32, 11_int32])
        call lc%append_null_row()
        call lc%append_row([30_int32, 31_int32, 32_int32])
        call parquet_open_writer(writer, "test_run/error_scenario_list_large.parquet")
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
        call parquet_debug_set_list_offset_limit(0_int64)

        call parquet_open_reader(reader, "test_run/error_scenario_list_large.parquet")
        call parquet_get_column_shape(reader, "lst", shape)
        call parquet_read_column(reader, "lst", back)
        call parquet_close_reader(reader)
        if (shape /= "list") error stop "a large_list column must still report shape 'list'"
        if (back%size() /= 3_int64) error stop "the large_list write did not round-trip its rows"
        if (.not. back%is_null(2_int64)) error stop "the large_list write lost a null row"
        row = back%view(3_int64)
        call row%get(v)
        if (size(v) /= 3) error stop "the large_list write lost an element"
        if (v(1) /= 30_int32 .or. v(3) /= 32_int32) error stop "the large_list write lost a value"
    end subroutine scenario_list_write_large_list_roundtrip

    !> Proves the arrow::large_utf8() PAYLOAD arm of a list column round-trips: the child array of a
    !> `list<string>` whose byte payload cannot fit int32 offsets is built with 64-bit offsets, and
    !> read back through the same widened accessor.
    !>
    !> The list counterpart of scenario_large_string_roundtrip, one level down: that one widens a
    !> whole string COLUMN, this one widens a list column's string CHILD, which is a separate
    !> builder and a separate measurement on the way back (a list column's payload byte total is
    !> read from the child's own offsets, and that read has one arm per offset width). A genuine
    !> >2 GiB payload is far too large for the suite, so the threshold is shrunk through
    !> parquet_debug_set_string_offset_limit -- safe as a process-global for the same reason: this
    !> scenario is its own isolated subprocess.
    !>
    !> Nothing a caller can observe distinguishes the two widths, so the round trip is the assertion:
    !> a child built with int64 offsets and read as int32 would hand back garbage, and one measured
    !> with the wrong arm would abort on the byte-count check inside the fill call.
    subroutine scenario_list_write_large_string_child()
        interface
            subroutine parquet_debug_set_string_offset_limit(n) &
                bind(C, name="parquet_debug_set_string_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! byte threshold to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_string_offset_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        ! TARGET on `back`: %view hands back a handle whose %col points at it, and F2018 15.5.2.4
        ! leaves that pointer UNDEFINED on return when the actual argument is not a target. Only
        ! nagfor's -C=dangling sees it (`.claude/rules/fortran-gotchas.md`, "nagfor-specific gotchas").
        type(parquet_list_column) :: lc
        type(parquet_list_column), target :: back
        type(parquet_list_row) :: row
        character(len=:), allocatable :: es(:)
        character(len=:), allocatable :: shape, arrow_type
        ! 5 + 5 + 7 + 5 + 4 = 26 bytes of payload against a 20-byte ceiling, so the child must widen.
        call parquet_debug_set_string_offset_limit(20_int64)
        call lc%init(PK_STRING)
        call lc%append_row([character(len=7) :: "alpha", "bravo"])
        call lc%append_null_row()
        call lc%append_row([character(len=7) :: "charlie", "delta", "echo"])
        call parquet_open_writer(writer, "test_run/error_scenario_list_large_string.parquet")
        call parquet_write_column(writer, "lst", lc)
        call parquet_close_writer(writer)
        call parquet_debug_set_string_offset_limit(0_int64)

        call parquet_open_reader(reader, "test_run/error_scenario_list_large_string.parquet")
        call parquet_get_column_shape(reader, "lst", shape)
        call parquet_get_column_arrow_type(reader, "lst", arrow_type)
        call parquet_read_column(reader, "lst", back)
        call parquet_close_reader(reader)
        if (shape /= "list") error stop "a list column with a large_utf8 child must still report shape 'list'"
        ! The one query that NAMES the stored type, and the reason this scenario is not merely a
        ! round trip: 26 bytes fit int32 offsets perfectly well, so a writer that ignored the
        ! shrunk ceiling and built a plain utf8 child would round-trip just as cleanly. This is what
        ! makes the widening itself observable.
        if (index(arrow_type, "large_string") == 0) then
            error stop "the shrunk string-offset ceiling must have widened the list child to " // &
                "large_utf8, and the stored type says: " // arrow_type
        end if
        if (back%size() /= 3_int64) error stop "the large_utf8 child write did not round-trip its rows"
        if (.not. back%is_null(2_int64)) error stop "the large_utf8 child write lost a null row"
        row = back%view(1_int64)
        call row%get(es)
        if (size(es) /= 2) error stop "the large_utf8 child write lost an element of row 1"
        if (trim(es(1)) /= "alpha" .or. trim(es(2)) /= "bravo") &
            error stop "the large_utf8 child write lost a value of row 1"
        row = back%view(3_int64)
        call row%get(es)
        if (size(es) /= 3) error stop "the large_utf8 child write lost an element of row 3"
        if (trim(es(1)) /= "charlie" .or. trim(es(3)) /= "echo") &
            error stop "the large_utf8 child write lost a value of row 3"
    end subroutine scenario_list_write_large_string_child

    !> The large_list arm across TWO row groups, where the first is narrow enough for int32 offsets
    !> and the second is not. This is the case that makes the streamed path's rule -- assemble every
    !> chunk with the type the FIELD already carries, fixed by the first row group, rather than
    !> recomputing an offset width per chunk -- observable at all.
    !>
    !> Recomputing per chunk is not a compile error and not a wrong flag: align_array_to_field would
    !> restamp the second chunk's large_list array as a list, leaving its int64 offsets buffer to be
    !> read as int32. Every value in that row group is then garbage while the file still validates.
    !> A mutation doing exactly that survived the whole suite until this scenario existed, because
    !> nothing else ever crosses the threshold twice in one column.
    subroutine scenario_list_write_large_list_chunked()
        interface
            subroutine parquet_debug_set_list_offset_limit(n) &
                bind(C, name="parquet_debug_set_list_offset_limit")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! element threshold to use instead of 2^31-1; <=0 restores it.
            end subroutine parquet_debug_set_list_offset_limit
        end interface
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_list_column) :: g1, g2
        type(parquet_list_column), target :: back !! TARGET because %view is called on it; see above.
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i
        character(len=*), parameter :: out_file = "test_run/error_scenario_list_large_chunked.parquet"

        ! Two elements in the first row group, four in the second, with the threshold between them.
        call parquet_debug_set_list_offset_limit(2_int64)
        call g1%init(PK_INT32)
        call g1%append_row([10_int32])
        call g1%append_row([20_int32])
        call g2%init(PK_INT32)
        call g2%append_row([30_int32, 31_int32])
        call g2%append_row([40_int32, 41_int32])

        call parquet_open_writer(writer, out_file)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "lst", g1)
        call parquet_finish_row_group(writer)
        call parquet_new_row_group(writer, 2_int64)
        call parquet_write_column_chunk(writer, "lst", g2)
        call parquet_finish_row_group(writer)
        call parquet_close_writer(writer)
        call parquet_debug_set_list_offset_limit(0_int64)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "lst", back)
        call parquet_close_reader(reader)
        if (back%size() /= 4_int64) error stop "the chunked large_list write lost a row"
        if (back%total_elements() /= 6_int64) error stop "the chunked large_list write lost an element"
        do i = 1_int64, 2_int64
            row = back%view(i)
            call row%get(v)
            if (size(v) /= 1) error stop "a first-row-group row changed length"
            if (v(1) /= int(i, int32)*10_int32) error stop "a first-row-group value is wrong"
        end do
        row = back%view(3_int64)
        call row%get(v)
        if (size(v) /= 2) error stop "a second-row-group row changed length"
        if (v(1) /= 30_int32 .or. v(2) /= 31_int32) error stop "a second-row-group value is wrong"
        row = back%view(4_int64)
        call row%get(v)
        if (v(1) /= 40_int32 .or. v(2) /= 41_int32) error stop "a second-row-group value is wrong"
    end subroutine scenario_list_write_large_list_chunked

    ! ---- parquet_healpix ----
    !
    ! Every abort `pf_query_disc` can produce. The conversions deliberately have none -- they are
    ! `pure elemental` and total, so an nside that is not a power of two gives them a documented
    ! garbage answer rather than an error, and validation lives here in the once-per-query entry
    ! point instead. See src/parquet_healpix.f90's own header.

    !> An nside that is not a power of two.
    subroutine scenario_healpix_disc_nside_not_power2()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(100_int64, NORTH, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted nside= 100"
    end subroutine scenario_healpix_disc_nside_not_power2

    !> An nside of zero, which the power-of-two bit test alone would accept.
    ! ---- pf_healpix_grid ----

    !> `%init` with an nside of zero.
    subroutine scenario_healpix_grid_init_nside_zero()
        type(pf_healpix_grid) :: g

        call g%init(0_int64, PF_HP_RING)
        print '(a)', "unexpectedly accepted nside= 0"
    end subroutine scenario_healpix_grid_init_nside_zero

    !> `%init` with an nside that is not a power of two.
    subroutine scenario_healpix_grid_init_nside_not_power()
        type(pf_healpix_grid) :: g

        call g%init(100_int64, PF_HP_RING)
        print '(a)', "unexpectedly accepted nside= 100"
    end subroutine scenario_healpix_grid_init_nside_not_power

    !> `%init` in the int32 kind, above what a 32-bit pixel index can address.
    subroutine scenario_healpix_grid_init_nside_int32_ceiling()
        type(pf_healpix_grid) :: g

        call g%init(16384_int32, PF_HP_RING)
        print '(a)', "unexpectedly accepted nside= 16384 in the int32 kind"
    end subroutine scenario_healpix_grid_init_nside_int32_ceiling

    !> `%init` with a scheme that is neither selector.
    subroutine scenario_healpix_grid_init_bad_scheme()
        type(pf_healpix_grid) :: g

        call g%init(64_int64, 7)
        print '(a)', "unexpectedly accepted scheme= 7"
    end subroutine scenario_healpix_grid_init_bad_scheme

    !> `%init` with a declination convention that is neither selector.
    subroutine scenario_healpix_grid_init_bad_frame()
        type(pf_healpix_grid) :: g

        call g%init(64_int64, PF_HP_RING, frame=7)
        print '(a)', "unexpectedly accepted frame= 7"
    end subroutine scenario_healpix_grid_init_bad_frame

    !> A disc query on a grid `%init` has never run on.
    subroutine scenario_healpix_grid_disc_unset()
        type(pf_healpix_grid) :: g
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call g%query_disc(NORTH, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly queried a disc on an unbuilt grid"
    end subroutine scenario_healpix_grid_disc_unset

    !> A bulk conversion on a grid `%init` has never run on.
    subroutine scenario_healpix_grid_bulk_unset()
        type(pf_healpix_grid) :: g
        real(real64) :: theta(4), phi(4)
        integer(int64) :: ipix(4)

        theta = 1.0_real64
        phi = 0.5_real64
        call g%ang2pix_bulk(theta, phi, ipix)
        print '(a)', "unexpectedly converted in bulk on an unbuilt grid"
    end subroutine scenario_healpix_grid_bulk_unset

    !> `%get_npix` into an int32, on a grid whose pixel count does not fit one.
    !>
    !> nside 16384 is legal for the int64 API and gives npix = 3221225472, which is above
    !> `huge(0_int32)` = 2147483647. This is the reachable half of the kind-matching rule: asking
    !> for a value in a kind that cannot hold it aborts rather than wrapping.
    subroutine scenario_healpix_grid_npix_int32_overflow()
        type(pf_healpix_grid) :: g
        integer(int32) :: npix

        call g%init(16384_int64, PF_HP_RING)
        call g%get_npix(npix)
        print '(a)', "unexpectedly narrowed npix= 3221225472 into an integer(int32)"
    end subroutine scenario_healpix_grid_npix_int32_overflow

    !> An int32 disc query on a grid too fine for a 32-bit pixel index.
    subroutine scenario_healpix_grid_disc_int32_too_fine()
        type(pf_healpix_grid) :: g
        integer(int32) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call g%init(16384_int64, PF_HP_RING)
        call g%query_disc(NORTH, 0.0001_real64, listpix, nlist)
        print '(a)', "unexpectedly ran an int32 disc query on an nside= 16384 grid"
    end subroutine scenario_healpix_grid_disc_int32_too_fine

    subroutine scenario_healpix_disc_nside_zero()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(0_int64, NORTH, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted nside= 0"
    end subroutine scenario_healpix_disc_nside_zero

    !> An nside above what a 32-bit pixel index can address, asked for in the int32 kind.
    subroutine scenario_healpix_disc_nside_int32_overflow()
        integer(int32) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(16384_int32, NORTH, 0.001_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted nside= 16384 in the int32 kind"
    end subroutine scenario_healpix_disc_nside_int32_overflow

    !> An nside above the module's own ceiling, asked for in the int64 kind.
    subroutine scenario_healpix_disc_nside_int64_overflow()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(1073741824_int64, NORTH, 1.0e-9_real64, &
                           listpix, nlist)
        print '(a)', "unexpectedly accepted nside= 2**30"
    end subroutine scenario_healpix_disc_nside_int64_overflow

    !> A NaN radius. Built at run time, so no constant expression can fold it away.
    subroutine scenario_healpix_disc_radius_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        integer(int64) :: listpix(64), nlist
        real(real64) :: nan
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        call pf_query_disc(4_int64, NORTH, nan, listpix, nlist)
        print '(a)', "unexpectedly accepted a NaN radius"
    end subroutine scenario_healpix_disc_radius_nan

    !> A negative radius.
    subroutine scenario_healpix_disc_radius_negative()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(4_int64, NORTH, -0.5_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted a negative radius"
    end subroutine scenario_healpix_disc_radius_negative

    !> A centre vector of zero length, which names no direction at all.
    subroutine scenario_healpix_disc_vector_zero()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: ZERO3(3) = [0.0_real64, 0.0_real64, 0.0_real64]

        call pf_query_disc(4_int64, ZERO3, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted a zero-length centre vector"
    end subroutine scenario_healpix_disc_vector_zero

    !> A centre vector holding a NaN.
    subroutine scenario_healpix_disc_vector_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        integer(int64) :: listpix(64), nlist
        real(real64) :: nan, centre(3)

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        centre = [nan, 0.0_real64, 1.0_real64]
        call pf_query_disc(4_int64, centre, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted a NaN in the centre vector"
    end subroutine scenario_healpix_disc_vector_nan

    !> A scheme selector that is neither PF_HP_RING nor PF_HP_NEST.
    subroutine scenario_healpix_disc_bad_scheme()
        integer(int64) :: listpix(64), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(4_int64, NORTH, 0.1_real64, listpix, nlist, &
                           scheme=7)
        print '(a)', "unexpectedly accepted scheme= 7"
    end subroutine scenario_healpix_disc_bad_scheme

    !> A listpix buffer too small for the disc, which must abort rather than truncate.
    subroutine scenario_healpix_disc_buffer_too_small()
        integer(int64) :: listpix(4), nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc(16_int64, NORTH, 0.5_real64, listpix, nlist)
        print '(a)', "unexpectedly filled a buffer too small for the result"
    end subroutine scenario_healpix_disc_buffer_too_small

    !> A run buffer with the module's INTERNAL four-row shape rather than the public two-row one.
    !>
    !> The four-row form is what the walk records for `pf_query_disc_alloc`, so it is exactly the
    !> shape a reader of this module would reach for -- and it is the one wrong shape that would
    !> otherwise look like a working call, filling two rows and leaving two undefined.
    subroutine scenario_healpix_disc_runs_bad_rows()
        integer(int64) :: runs(4, 64), nruns
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc_runs(16_int64, NORTH, 0.5_real64, &
                                runs, nruns)
        print '(a)', "unexpectedly accepted a run buffer with the wrong number of rows"
    end subroutine scenario_healpix_disc_runs_bad_rows

    ! ---- Tier B ----
    !
    ! **Six scenarios, not twenty-seven, and the difference is deliberate.** The nine validation
    ! rules `pf_query_disc` aborts on are the SHARED validator's, and the ten scenarios above
    ! already cover them nine ways. Re-running all nine against each new entry point would test
    ! the same code three times and the new plumbing zero times more. What is actually new is that
    ! three entry points now share one validator and each must name ITSELF in the message, and
    ! that the bulk forms have three rules of their own that nothing else has.

    !> `pf_query_disc_count` on an nside that is not a power of two: the message must name it.
    subroutine scenario_healpix_disc_count_bad_nside()
        integer(int64) :: nlist
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc_count(6_int64, NORTH, 0.1_real64, nlist)
        print '(a)', "unexpectedly accepted nside= 6"
    end subroutine scenario_healpix_disc_count_bad_nside

    !> `pf_query_disc_max_count` validates `nside` and names ITSELF, rather than answering -1 the
    !> way the `pure elemental` arithmetic does. A sizing routine that returned -1 would have the
    !> caller allocate a zero-length buffer and meet the complaint one call later.
    subroutine scenario_healpix_disc_max_count_bad_nside()
        integer(int64) :: nmax

        nmax = pf_query_disc_max_count(6_int64, 0.1_real64)
        print '(a,i0)', "unexpectedly accepted nside= 6, nmax=", nmax
    end subroutine scenario_healpix_disc_max_count_bad_nside

    !> And it validates `radius` on the same shared checker, so a negative one aborts here too.
    subroutine scenario_healpix_disc_max_count_negative_radius()
        integer(int64) :: nmax

        nmax = pf_query_disc_max_count(64_int64, -0.5_real64)
        print '(a,i0)', "unexpectedly accepted a negative radius, nmax=", nmax
    end subroutine scenario_healpix_disc_max_count_negative_radius

    !> `pf_query_disc_alloc` on an unknown scheme: the message must name it, not its sibling.
    subroutine scenario_healpix_disc_alloc_bad_scheme()
        integer(int64) :: nlist
        integer(int64), allocatable :: listpix(:)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        call pf_query_disc_alloc(4_int64, NORTH, 0.1_real64, &
                                 listpix, nlist, scheme=9)
        print '(a)', "unexpectedly accepted scheme= 9"
    end subroutine scenario_healpix_disc_alloc_bad_scheme

    !> A bulk form on an invalid nside. The elemental form it wraps would return nonsense here.
    subroutine scenario_healpix_bulk_nside_invalid()
        real(real64) :: theta(3), phi(3)
        integer(int64) :: ipix(3)

        theta = [0.1_real64, 0.5_real64, 1.0_real64]
        phi = [0.0_real64, 1.0_real64, 2.0_real64]
        call pf_ang2pix_ring_bulk(0_int64, theta, phi, ipix)
        print '(a)', "unexpectedly accepted nside= 0"
    end subroutine scenario_healpix_bulk_nside_invalid

    !> Three arrays that do not conform.
    subroutine scenario_healpix_bulk_size_mismatch()
        real(real64) :: theta(3), phi(3)
        integer(int64) :: ipix(2)

        theta = [0.1_real64, 0.5_real64, 1.0_real64]
        phi = [0.0_real64, 1.0_real64, 2.0_real64]
        call pf_ang2pix_ring_bulk(16_int64, theta, phi, ipix)
        print '(a)', "unexpectedly accepted a short output array"
    end subroutine scenario_healpix_bulk_size_mismatch

    !> An explicit thread count below one.
    subroutine scenario_healpix_bulk_threads_zero()
        real(real64) :: theta(3), phi(3)
        integer(int64) :: ipix(3)

        theta = [0.1_real64, 0.5_real64, 1.0_real64]
        phi = [0.0_real64, 1.0_real64, 2.0_real64]
        call pf_ang2pix_ring_bulk(16_int64, theta, phi, ipix, threads=0)
        print '(a)', "unexpectedly accepted threads= 0"
    end subroutine scenario_healpix_bulk_threads_zero

    !> A vector array whose first extent is not 3.
    subroutine scenario_healpix_bulk_vec_shape()
        real(real64) :: vec(2, 3)
        integer(int64) :: ipix(3)

        vec = 1.0_real64
        call pf_vec2pix_ring_bulk(16_int64, vec, ipix)
        print '(a)', "unexpectedly accepted a vec array of the wrong shape"
    end subroutine scenario_healpix_bulk_vec_shape

    !> A layout template naming a field that does not exist aborts at configuration time.
    subroutine scenario_logging_unknown_layout_field()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_badfield.txt", format="{stamp} {nosuchfield}")
        print '(a)', "unexpectedly accepted a layout template naming an unknown field"
    end subroutine scenario_logging_unknown_layout_field

    !> A second console sink on the same stream would double every line, so it is refused.
    subroutine scenario_logging_second_console_sink()
        type(pf_logger) :: lg

        call lg%init()
        call lg%add_console(stream=PF_LOG_STDOUT)
        print '(a)', "unexpectedly attached a second console sink on the same stream"
    end subroutine scenario_logging_second_console_sink

    !> Attaching more sinks than PF_LOG_MAX_SINKS aborts rather than silently dropping one.
    subroutine scenario_logging_too_many_sinks()
        type(pf_logger) :: lg
        integer :: i
        character(len=64) :: path

        call lg%init(console=.false.)
        do i = 1, PF_LOG_MAX_SINKS + 1
            write (path, '("test_run/es_log_many_",i0,".txt")') i
            call lg%add_file(trim(path))
        end do
        print '(a)', "unexpectedly attached more than PF_LOG_MAX_SINKS sinks"
    end subroutine scenario_logging_too_many_sinks

    !> set_level's two selectors have no combined meaning and are refused rather than guessed at.
    subroutine scenario_logging_sink_and_name_together()
        type(pf_logger) :: lg
        integer :: s

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_sel.txt", sink=s)
        call lg%set_level(PF_LEVEL_ERROR, sink=s, name="a.b")
        print '(a)', "unexpectedly accepted set_level with both sink= and name="
    end subroutine scenario_logging_sink_and_name_together

    !> A rank-filtered sink added before the logger has a rank would silently discard every
    !> record, so it is refused at configuration time instead.
    subroutine scenario_logging_rank_filter_without_rank()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_rank.txt", only_rank=0)
        print '(a)', "unexpectedly added a rank-filtered sink with no rank set"
    end subroutine scenario_logging_rank_filter_without_rank

    !> A pop whose frame token does not match the current depth means a push and a pop are
    !> unbalanced somewhere, which would otherwise mislabel every later record on this thread.
    subroutine scenario_logging_pop_context_token_mismatch()
        integer :: frame

        call pf_log_clear_context()
        call pf_log_push_context("outer", frame)
        call pf_log_push_context("inner")
        call pf_log_pop_context(frame)
        print '(a)', "unexpectedly accepted a pop_context token that did not match the depth"
    end subroutine scenario_logging_pop_context_token_mismatch

    !> Without the optional `ok`, an unrecognised level name aborts.
    subroutine scenario_logging_unknown_level_name()
        integer :: lev

        call pf_log_level_from_name("verbose", lev)
        print '(a,i0)', "unexpectedly converted an unknown level name: ", lev
    end subroutine scenario_logging_unknown_level_name

    !> pf_log_fatal emits at CRITICAL, flushes every sink, then error stops -- the flush before
    !> the abort being the whole point, since an unflushed file loses the record explaining why.
    subroutine scenario_logging_fatal()

        call pf_log_init(console=.false.)
        call pf_log_add_file("test_run/es_log_fatal.txt", append=.false., format="{message}")
        call pf_log_fatal("the run cannot continue")
        print '(a)', "unexpectedly returned from pf_log_fatal"
    end subroutine scenario_logging_fatal

    !> Several threads reach a fatal at once -- what a guard inside a parallel loop does whenever
    !> more than one element offends. The abort must still be ONE abort: `ERROR STOP` is `exit()`,
    !> and concurrent `exit()` calls are undefined behaviour that leave ifx reporting a
    !> nondeterministic process status, including 0 for a run that plainly aborted.
    !>
    !> Two things are observable from outside, and the test asserts both. The status must be
    !> nonzero -- the defect itself, which an unfixed build only fails intermittently. And the
    !> fatal record must appear EXACTLY ONCE, which is the deterministic half: unserialised, every
    !> thread emits its own copy before dying, so the count alone separates a fixed build from a
    !> broken one on a single run.
    !>
    !> The message goes to the console rather than a file so that the captured output carries it.
    !> Built without OpenMP the loop is serial and the first iteration aborts, which is the same
    !> one record and the same nonzero status -- the scenario degrades to the plain fatal case
    !> rather than becoming vacuous.
    subroutine scenario_logging_fatal_omp()
        integer :: i

        call pf_log_init(console=.true., format="{message}")
        !$omp parallel do default(shared) private(i) num_threads(8)
        do i = 1, 8
            call pf_log_fatal("the parallel run cannot continue")
        end do
        !$omp end parallel do
        print '(a)', "unexpectedly returned from a concurrent pf_log_fatal"
    end subroutine scenario_logging_fatal_omp

    !> A logger is copyable -- that is what `firstprivate` does -- so two copies name the same
    !> unit and closing one closes the file the other is still writing to. Ownership cannot be
    !> inferred from a value type, so instead the misuse is made LOUD: the next write's `iostat`
    !> catches it and aborts naming the path, rather than the records silently going nowhere.
    subroutine scenario_logging_write_to_closed_sink()
        type(pf_logger) :: a, b

        call a%init(console=.false.)
        call a%add_file("test_run/es_log_closed.txt", append=.false., format="{message}")
        b = a
        call b%info("written through the copy while the unit is open")
        call a%close()
        call b%info("written after the other copy closed the unit")
        print '(a)', "unexpectedly wrote a record to a sink whose unit had been closed"
    end subroutine scenario_logging_write_to_closed_sink

    !> A path longer than `PF_LOG_MAX_PATH` is refused at configuration time rather than
    !> truncated into a path that names a different file.
    subroutine scenario_logging_path_too_long()
        type(pf_logger) :: lg
        character(len=PF_LOG_MAX_PATH + 8) :: long_path

        long_path = "test_run/" // repeat("p", PF_LOG_MAX_PATH)
        call lg%init(console=.false.)
        call lg%add_file(long_path)
        print '(a)', "unexpectedly accepted a path longer than PF_LOG_MAX_PATH"
    end subroutine scenario_logging_path_too_long

    !> A file that cannot be opened aborts naming the path and the runtime's own reason, rather
    !> than attaching a sink whose every write then fails.
    subroutine scenario_logging_file_cannot_open()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        ! A directory component that is not a directory: portable, needs no permissions, and
        ! cannot succeed by accident on a machine running the suite as root.
        call lg%add_file("test_run/es_log_control.txt/inside.log")
        print '(a)', "unexpectedly opened a file under a non-directory path"
    end subroutine scenario_logging_file_cannot_open

    !> The default logger writes to stdout at INFO before it is configured, and stops the moment
    !> it is. Neither half can be asserted in process -- reading stdout back needs a subprocess --
    !> so this is where D3's central contract is actually tested.
    subroutine scenario_logging_implicit_console()

        call pf_log_info("implicit-console-record")
        call pf_log_debug("implicit-debug-record")   ! below INFO: dropped even implicitly
        ! Configuring the default logger retires the implicit console for good. With no sink
        ! attached, this record has nowhere to go and must not reappear on stdout.
        call pf_log_init(console=.false.)
        call pf_log_info("after-configuration-record")
    end subroutine scenario_logging_implicit_console

    !> `%unset_level` validates its name exactly as `%set_level` does, so a name neither can
    !> accept is refused by both rather than silently reporting "no such override".
    subroutine scenario_logging_unset_level_empty_name()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%unset_level("")
        print '(a)', "unexpectedly accepted an empty name= in unset_level"
    end subroutine scenario_logging_unset_level_empty_name

    !> A pop whose token does not match the current depth means a push and a pop are unbalanced
    !> somewhere -- silently removing the wrong frame is what the token exists to prevent.
    subroutine scenario_logging_pop_name_token_mismatch()
        integer :: frame

        call pf_log_clear_names()
        call pf_log_push_name("io", frame)
        call pf_log_push_name("stat")          ! a callee that forgot to pop
        call pf_log_pop_name(frame)
        print '(a)', "unexpectedly accepted a name pop whose token did not match the depth"
    end subroutine scenario_logging_pop_name_token_mismatch
    !
    !> A layout template longer than `PF_LOG_MAX_FORMAT` is refused at configuration time, rather
    !> than silently truncated into a template that renders something else.
    subroutine scenario_logging_template_too_long()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_fmt_long.txt", format=repeat("x", PF_LOG_MAX_FORMAT + 1))
        print '(a)', "unexpectedly accepted a layout template longer than PF_LOG_MAX_FORMAT"
    end subroutine scenario_logging_template_too_long
    !
    !> A `{` with no closing `}` is a typo, not an empty field: rendering it as a literal would
    !> hide the mistake in every line the sink ever writes.
    subroutine scenario_logging_template_unclosed_brace()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_fmt_brace.txt", format="{time} {message")
        print '(a)', "unexpectedly accepted a layout template with an unclosed brace"
    end subroutine scenario_logging_template_unclosed_brace
    !
    !> More parsed steps than `PF_LOG_MAX_FORMAT_OPS` is refused. `{level}` is used rather than a
    !> longer field name so that the template stays inside `PF_LOG_MAX_FORMAT` and this guard,
    !> not the length guard above, is the one that fires.
    subroutine scenario_logging_template_too_many_ops()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_fmt_ops.txt", &
            format=repeat("{level}", PF_LOG_MAX_FORMAT_OPS + 1))
        print '(a)', "unexpectedly accepted a layout template with too many steps"
    end subroutine scenario_logging_template_too_many_ops
    !
    !> `add_console` takes one of two published stream constants; anything else is a caller error
    !> rather than a third stream.
    subroutine scenario_logging_add_console_bad_stream()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_console(stream=99)
        print '(a)', "unexpectedly accepted a console stream that is neither stdout nor stderr"
    end subroutine scenario_logging_add_console_bad_stream
    !
    !> `add_unit` takes a unit the CALLER owns, so it validates what it was handed: a unit nothing
    !> has opened would otherwise fail on the first record rather than at configuration.
    subroutine scenario_logging_add_unit_not_connected()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_unit(87)
        print '(a)', "unexpectedly attached a unit that is not connected"
    end subroutine scenario_logging_add_unit_not_connected
    !
    !> A unit opened for reading cannot take records. See `scenario_logging_add_unit_not_connected`.
    subroutine scenario_logging_add_unit_not_writable()
        type(pf_logger) :: lg
        integer :: u

        open (newunit=u, file="test_run/es_log_readonly.txt", action="write", status="replace")
        write (u, '(a)') "seed"
        close (u)
        open (newunit=u, file="test_run/es_log_readonly.txt", action="read", status="old")
        call lg%init(console=.false.)
        call lg%add_unit(u)
        print '(a)', "unexpectedly attached a read-only unit"
    end subroutine scenario_logging_add_unit_not_writable
    !
    !> A record is formatted text, so an unformatted unit is refused rather than written to.
    subroutine scenario_logging_add_unit_unformatted()
        type(pf_logger) :: lg
        integer :: u

        open (newunit=u, file="test_run/es_log_unformatted.bin", action="write", &
              form="unformatted", status="replace")
        call lg%init(console=.false.)
        call lg%add_unit(u)
        print '(a)', "unexpectedly attached an unformatted unit"
    end subroutine scenario_logging_add_unit_unformatted
    !
    !> A per-name override key longer than `PF_LOG_MAX_NAME` is refused, since a truncated key
    !> would silently govern a different set of records from the one the caller named.
    subroutine scenario_logging_set_level_name_too_long()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_level(PF_LEVEL_DEBUG, name=repeat("n", PF_LOG_MAX_NAME + 1))
        print '(a)', "unexpectedly accepted a name= longer than PF_LOG_MAX_NAME"
    end subroutine scenario_logging_set_level_name_too_long
    !
    !> More per-name overrides than one logger holds is refused rather than dropping one.
    subroutine scenario_logging_too_many_name_rules()
        type(pf_logger) :: lg
        character(len=8) :: nm
        integer :: i

        call lg%init(console=.false.)
        do i = 1, PF_LOG_MAX_NAME_RULES + 1
            write (nm, '(a,i0)') "rule", i
            call lg%set_level(PF_LEVEL_DEBUG, name=trim(nm))
        end do
        print '(a)', "unexpectedly accepted more than PF_LOG_MAX_NAME_RULES overrides"
    end subroutine scenario_logging_too_many_name_rules
    !
    !> `%unset_level` validates the key's LENGTH as `%set_level` does, the companion to
    !> `scenario_logging_unset_level_empty_name`.
    subroutine scenario_logging_unset_level_name_too_long()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%unset_level(repeat("n", PF_LOG_MAX_NAME + 1))
        print '(a)', "unexpectedly accepted an over-long name= in unset_level"
    end subroutine scenario_logging_unset_level_name_too_long
    !
    !> The colour policy is one of three published constants.
    subroutine scenario_logging_set_color_bad_policy()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_color(7)
        print '(a)', "unexpectedly accepted a colour policy outside AUTO/NEVER/ALWAYS"
    end subroutine scenario_logging_set_color_bad_policy
    !
    !> A `sink=` id no sink has is a caller error: silently configuring nothing would leave the
    !> caller believing a sink had been changed.
    subroutine scenario_logging_bad_sink_id()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_badsink.txt")
        call lg%set_format("{message}", sink=9)
        print '(a)', "unexpectedly accepted a sink id no sink has"
    end subroutine scenario_logging_bad_sink_id
    !
    !> A logger name longer than `PF_LOG_MAX_NAME` is refused rather than truncated, since the
    !> name is also what per-name overrides key on.
    subroutine scenario_logging_set_name_too_long()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_name(repeat("n", PF_LOG_MAX_NAME + 1))
        print '(a)', "unexpectedly accepted a logger name longer than PF_LOG_MAX_NAME"
    end subroutine scenario_logging_set_name_too_long
    !
    !> A rank is a non-negative identity or the published "no rank" sentinel; anything else is a
    !> mistake rather than a third meaning.
    subroutine scenario_logging_set_rank_negative()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_rank(-5)
        print '(a)', "unexpectedly accepted a negative rank that is not PF_LOG_RANK_ANY"
    end subroutine scenario_logging_set_rank_negative
    !
    !> The threading mode is one of two published constants.
    subroutine scenario_logging_thread_mode_bad()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_thread_mode(5)
        print '(a)', "unexpectedly accepted a thread mode that is neither direct nor buffered"
    end subroutine scenario_logging_thread_mode_bad
    !
    !> A collector slot below `PF_LOG_MIN_BUFFER_BYTES` could not hold one ordinary record, so the
    !> floor is enforced at configuration rather than discovered at the first emission.
    subroutine scenario_logging_thread_mode_slot_too_small()
        type(pf_logger) :: lg

        call lg%init(console=.false.)
        call lg%set_thread_mode(PF_LOG_THREAD_BUFFERED, slot_bytes=16)
        print '(a)', "unexpectedly accepted slot_bytes below PF_LOG_MIN_BUFFER_BYTES"
    end subroutine scenario_logging_thread_mode_slot_too_small
    !
    !> The shared base context is bounded like every other configured string. The per-frame
    !> budget SATURATES instead, deliberately -- see the module header -- so only this one aborts.
    subroutine scenario_logging_set_context_too_long()

        call pf_log_set_context(repeat("c", PF_LOG_MAX_CONTEXT + 1))
        print '(a)', "unexpectedly accepted a base context longer than PF_LOG_MAX_CONTEXT"
    end subroutine scenario_logging_set_context_too_long
    !
    !> An empty name frame would compose a doubled dot and could not be told from no frame at all.
    subroutine scenario_logging_push_name_empty()

        call pf_log_clear_names()
        call pf_log_push_name("   ")
        print '(a)', "unexpectedly accepted an empty name frame"
    end subroutine scenario_logging_push_name_empty
    !
    !> The name stack is bounded by DEPTH, and the bound aborts rather than saturating: a dropped
    !> frame would leave the composed name naming the wrong scope.
    subroutine scenario_logging_push_name_too_deep()
        character(len=4) :: nm
        integer :: i

        call pf_log_clear_names()
        do i = 1, PF_LOG_MAX_NAME_DEPTH + 1
            write (nm, '(a,i0)') "f", i
            call pf_log_push_name(trim(nm))
        end do
        print '(a)', "unexpectedly pushed more than PF_LOG_MAX_NAME_DEPTH name frames"
    end subroutine scenario_logging_push_name_too_deep
    !
    !> And by the composed LENGTH, which is a separate bound reached at a shallower depth. The
    !> frames below are twelve characters each, so the sixth crosses PF_LOG_MAX_NAME while the
    !> depth is still well inside PF_LOG_MAX_NAME_DEPTH.
    subroutine scenario_logging_push_name_too_long()
        integer :: i

        call pf_log_clear_names()
        do i = 1, PF_LOG_MAX_NAME_DEPTH
            call pf_log_push_name(repeat("f", 12))
        end do
        print '(a)', "unexpectedly pushed name frames longer than PF_LOG_MAX_NAME in total"
    end subroutine scenario_logging_push_name_too_long
    !
    !> A `<prefix>LEVEL` that is not a level name or number aborts naming the value, rather than
    !> falling back to a default the caller did not ask for.
    subroutine scenario_logging_env_bad_level()

        call scenario_setenv("PF_LOG_LEVEL", "verbose-ish")
        call pf_log_configure_from_env()
        print '(a)', "unexpectedly accepted a PF_LOG_LEVEL that names no level"
    end subroutine scenario_logging_env_bad_level
    !
    !> The UNCONFIGURED default logger describes itself, answers `%enabled` from its implicit
    !> INFO threshold, and writes blank lines to its implicit console.
    !>
    !> Every one of those arms exists only while `g_default` has never been configured, and the
    !> first `pf_log_init`/`pf_log_add_*` retires them for the life of the process -- so they
    !> cannot be reached from a test suite that configures the default logger anywhere.
    subroutine scenario_logging_implicit_print()
        integer :: u
        logical :: on, off

        on = pf_log_enabled(PF_LEVEL_WARNING)
        off = pf_log_enabled(PF_LEVEL_DEBUG)
        if (.not. on) error stop "implicit default: WARNING should be enabled"
        if (off) error stop "implicit default: DEBUG should be disabled"
        call pf_log_blank(2)
        open (newunit=u, file="test_run/es_log_implicit_print.txt", action="write", status="replace")
        call pf_log_print(unit=u)
        close (u)
        print '(a)', "implicit default logger described itself"
    end subroutine scenario_logging_implicit_print

    !> One push, one segment: a frame carrying its own dot could not be popped off as a unit.
    subroutine scenario_logging_push_name_with_dot()

        call pf_log_clear_names()
        call pf_log_push_name("io.stat")
        print '(a)', "unexpectedly accepted a name frame containing a dot"
    end subroutine scenario_logging_push_name_with_dot

    !> A composed name over PF_LOG_MAX_NAME aborts rather than truncating: the name decides which
    !> records are emitted, so a silently shortened one changes behaviour with nothing to report
    !> it. This is the overflow push_name cannot catch at its own site, since how long the name
    !> comes out depends on which logger emits.
    subroutine scenario_logging_composed_name_too_long()
        type(pf_logger) :: lg
        character(len=PF_LOG_MAX_NAME) :: long_base

        long_base = repeat("b", PF_LOG_MAX_NAME - 2)
        call pf_log_clear_names()
        call lg%init(console=.false., name=trim(long_base))
        call lg%add_file("test_run/es_log_longname.txt", append=.false., format="{name}")
        call pf_log_push_name("frame")
        call lg%info("this record cannot carry the composed name")
        print '(a)', "unexpectedly emitted a record whose composed name exceeds PF_LOG_MAX_NAME"
    end subroutine scenario_logging_composed_name_too_long

    !> The negative control for every scenario above: the same configuration calls, made
    !> correctly, must exit cleanly. Without it each abort test would pass just as happily
    !> against a guard that fired unconditionally.
    subroutine scenario_logging_control()
        type(pf_logger) :: lg
        integer :: s, frame, lev
        logical :: ok
        character(len=PF_LOG_MAX_NAME) :: gotname

        call lg%init(console=.false.)
        call lg%add_file("test_run/es_log_control.txt", append=.false., format="{stamp} {message}", &
            sink=s)
        call lg%set_level(PF_LEVEL_ERROR, sink=s)
        call lg%set_level(PF_LEVEL_ERROR, name="a.b")
        call lg%set_rank(0)
        call lg%add_console(stream=PF_LOG_STDERR, only_rank=0)
        call pf_log_clear_context()
        call pf_log_push_context("outer", frame)
        call pf_log_pop_context(frame)
        call lg%unset_level("a.b")
        call lg%unset_level()
        call pf_log_clear_names()
        call pf_log_push_name("io", frame)
        call pf_log_pop_name(frame)
        call pf_log_pop_name()
        call lg%get_name(gotname)
        call pf_log_level_from_name("verbose", lev, ok)
        if (ok) print '(a)', "control: an unknown level name reported success"
        call lg%error("control record")
        call lg%close()
    end subroutine scenario_logging_control

    ! ================================================================================
    ! parquet_toml
    !
    ! Every one of these builds its document with pf_toml_loads from a STRING. That is deliberate
    ! and it is about this harness rather than about taste: tools/run_error_scenarios.sh dispatches
    ! scenarios with `xargs -P`, so several of these processes run at once, and a shared fixture
    ! PATH between two of them is the collision the project's own rule warns about. A string
    ! literal cannot collide. The one scenario that names a file names one that does not exist.
    !
    ! scenario_toml_control is the negative control for the whole group: it walks the same setup
    ! every scenario above uses and exits 0, so a scenario that "aborted as expected" cannot have
    ! been aborting in the shared preamble instead of at the call under test.
    ! ================================================================================

    !> The document every scenario in this group reads.
    subroutine toml_sample(text)
        character(len=:), allocatable, intent(out) :: text  !! Receives the TOML document.
        character(len=1) :: nl

        nl = new_line("a")
        text = 'title = "example"' // nl // &
               '[general]' // nl // &
               'nproc = 4' // nl // &
               'factor = 2.5' // nl // &
               'name = "run one"' // nl // &
               'limits = [1, 2, 3]' // nl // &
               'files = ["a", "bc", "a much longer third"]' // nl // &
               'level = "WARNING"' // nl // &
               '[plain]' // nl // &
               'x = 1' // nl // &
               '[[region]]' // nl // &
               'id = 1' // nl
    end subroutine toml_sample

    !> A value that is there but of the wrong type must abort, not leave the variable undefined.
    subroutine scenario_toml_wrong_type()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: n

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "factor", n)
        print '(a)', "scenario_toml_wrong_type: reading 2.5 as an integer should have aborted"
    end subroutine scenario_toml_wrong_type

    !> A key with no default and no `required = .false.` must abort when the file omits it.
    subroutine scenario_toml_missing_key()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: n

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "not_in_the_file", n)
        print '(a)', "scenario_toml_missing_key: a required key should have aborted"
    end subroutine scenario_toml_missing_key

    !> A TOML integer too large for `integer(int32)` must be reported, never wrapped.
    subroutine scenario_toml_int_overflow()
        type(pf_toml) :: conf
        integer(int32) :: n

        call pf_toml_loads(conf, "big = 9223372036854775807" // new_line("a"))
        call pf_toml_get(conf, "big", n)
        print '(a)', "scenario_toml_int_overflow: a value beyond int32 should have aborted"
    end subroutine scenario_toml_int_overflow

    !> A list of the wrong length must abort in BOTH directions, never use a prefix.
    subroutine scenario_toml_array_length(n)
        integer, intent(in) :: n   !! Size of the caller's array: 2 (too short) or 4 (too long).
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer, allocatable :: got(:)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        allocate(got(n))
        got = 0
        call pf_toml_get(gen, "limits", got)
        print '(a)', "scenario_toml_array_length: a length mismatch should have aborted"
    end subroutine scenario_toml_array_length

    !> A string element longer than the caller's declared length must abort, never be clipped.
    subroutine scenario_toml_string_too_long()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        character(len=4) :: files(3)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "files", files)
        print '(a)', "scenario_toml_string_too_long: a clipped file name should have aborted"
    end subroutine scenario_toml_string_too_long

    !> An array getter's bare call requires its key, exactly as a scalar's does.
    subroutine scenario_toml_array_required()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: got(3)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        got = 0
        call pf_toml_get(gen, "no_such_list", got)
        print '(a)', "scenario_toml_array_required: an absent list on a bare call should have aborted"
    end subroutine scenario_toml_array_required

    !> A section the program needs and the file does not have must abort.
    subroutine scenario_toml_missing_section()
        type(pf_toml) :: conf, sect
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "not_there", sect)
        print '(a)', "scenario_toml_missing_section: a required section should have aborted"
    end subroutine scenario_toml_missing_section

    !> Counting the entries of a plain `[table]` is a programming error, not an answer of 1.
    subroutine scenario_toml_section_not_array()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text
        integer :: n

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        n = pf_toml_section_count(conf, "plain")
        print '(a)', "scenario_toml_section_not_array: counting a plain table should have aborted"
    end subroutine scenario_toml_section_not_array

    !> An entry index outside `1 .. count` must abort when the call is a BARE one.
    !>
    !> `required = .false.` deliberately does the opposite and returns a closed handle, which
    !> `test_optional_entry_out_of_range` (test/test_toml.f90) pins. The two halves are the
    !> same rule the named form already followed: absence honours `required =`, and only a
    !> name of the wrong shape is fatal whatever it says.
    subroutine scenario_toml_entry_out_of_range()
        type(pf_toml) :: conf, ent
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "region", 7, ent)
        print '(a)', "scenario_toml_entry_out_of_range: entry 7 of 1 should have aborted"
    end subroutine scenario_toml_entry_out_of_range

    !> A retired key that is still set must stop the run, not be ignored.
    subroutine scenario_toml_retired_key()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_retire(gen, "nproc", "set nproc_openmp instead")
        print '(a)', "scenario_toml_retired_key: a retired key should have aborted"
    end subroutine scenario_toml_retired_key

    !> A key nobody read must abort under the default severity.
    subroutine scenario_toml_unknown_key()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: n

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", n)
        call pf_toml_check(gen)
        print '(a)', "scenario_toml_unknown_key: an unread key should have aborted"
    end subroutine scenario_toml_unknown_key

    !> A whole SECTION nobody opened must abort under `pf_toml_check_all`.
    !!
    !! This is the one-level-up form of a misspelt key, and the failure `pf_toml_check` alone
    !! cannot see: nothing in the program ever mentions `[plain]`, so no per-section sweep runs
    !! over it.
    subroutine scenario_toml_unknown_section()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        integer :: n, i, id
        real(real64) :: f
        integer :: limits(3)
        character(len=32) :: files(3)
        integer :: lev
        type(pf_toml) :: ent

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_get(conf, "title", name)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", n)
        call pf_toml_get(gen, "factor", f)
        call pf_toml_get(gen, "name", name)
        call pf_toml_get(gen, "limits", limits)
        call pf_toml_get(gen, "files", files)
        call pf_toml_get_level(gen, "level", lev)
        do i = 1, pf_toml_section_count(conf, "region")
            call pf_toml_section(conf, "region", i, ent)
            call pf_toml_get(ent, "id", id)
        end do
        ! Everything read EXCEPT [plain], which is what check_all must report.
        call pf_toml_check_all(conf)
        print '(a)', "scenario_toml_unknown_section: an unopened section should have aborted"
    end subroutine scenario_toml_unknown_section

    !> A log level name nothing recognises must abort, listing the names that work.
    subroutine scenario_toml_bad_level()
        type(pf_toml) :: conf
        integer :: lev

        call pf_toml_loads(conf, 'level = "LOUD"' // new_line("a"))
        call pf_toml_get_level(conf, "level", lev)
        print '(a)', "scenario_toml_bad_level: an unrecognised level name should have aborted"
    end subroutine scenario_toml_bad_level

    !> Reading from an optional section that was not found must abort, never return a default.
    subroutine scenario_toml_closed_handle()
        type(pf_toml) :: conf, sect
        character(len=:), allocatable :: text
        integer :: n
        logical :: found

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "not_there", sect, required = .false., found = found)
        ! `found` is .false. here; a program that ignores it and reads anyway must be stopped
        ! rather than handed a default, which would make an absent section indistinguishable from
        ! an empty one.
        call pf_toml_get(sect, "anything", n, default = 1)
        print '(a)', "scenario_toml_closed_handle: reading a closed section should have aborted"
    end subroutine scenario_toml_closed_handle

    !> `pf_toml_set` ADDS: a key that already exists must be refused, naming `pf_toml_update`.
    subroutine scenario_toml_set_existing()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_set(gen, "nproc", 9)
        print '(a)', "scenario_toml_set_existing: setting an existing key should have aborted"
    end subroutine scenario_toml_set_existing

    !> `pf_toml_update` CHANGES: a key that is not there must be refused, naming `pf_toml_set`.
    subroutine scenario_toml_update_missing()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_update(gen, "brand_new", 9)
        print '(a)', "scenario_toml_update_missing: updating an absent key should have aborted"
    end subroutine scenario_toml_update_missing

    !> Closing a SECTION handle would free a document other handles still borrow.
    subroutine scenario_toml_close_not_owner()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_close(gen)
        print '(a)', "scenario_toml_close_not_owner: closing a section handle should have aborted"
    end subroutine scenario_toml_close_not_owner

    !> A rank-1 `default` must hold exactly as many entries as the array it would fill.
    !!
    !! The mistake this catches is a default written once and an array later resized, which would
    !! otherwise be a shape-mismatch assignment inside the library rather than a named failure.
    subroutine scenario_toml_default_size()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: got(3)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "no_such_list", got, default = [1, 2])
        print '(a)', "scenario_toml_default_size: a default of the wrong size should have aborted"
    end subroutine scenario_toml_default_size

    !> `count` is fatal when the file's list is too SHORT.
    subroutine scenario_toml_strings_count_short()
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files, count = 4)
        print '(a)', "scenario_toml_strings_count_short: a list shorter than count should have aborted"
    end subroutine scenario_toml_strings_count_short

    !> `count` is fatal when the file's list is too LONG, which is the direction a caller is most
    !> tempted to allow: a prefix of a too-long list pairs each value with the wrong slot.
    subroutine scenario_toml_strings_count_long()
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files, count = 2)
        print '(a)', "scenario_toml_strings_count_long: a list longer than count should have aborted"
    end subroutine scenario_toml_strings_count_long

    !> `pf_toml_dump` writes a whole document, so a section handle is a mistake, not a subset.
    subroutine scenario_toml_dump_not_owner()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_dump(gen, "test_run/toml_dump_not_owner.toml")
        print '(a)', "scenario_toml_dump_not_owner: dumping a section handle should have aborted"
    end subroutine scenario_toml_dump_not_owner

    !> Without `status`, text that is not TOML aborts, with toml-f's own diagnostic first.
    subroutine scenario_toml_parse_error()
        type(pf_toml) :: conf

        call pf_toml_loads(conf, "this is not = = valid toml" // new_line("a"))
        print '(a)', "scenario_toml_parse_error: malformed TOML should have aborted"
    end subroutine scenario_toml_parse_error

    !> Without `status`, a file that cannot be opened aborts.
    subroutine scenario_toml_open_error()
        type(pf_toml) :: conf

        call pf_toml_load(conf, "test_run/no_such_file_for_the_open_scenario.toml")
        print '(a)', "scenario_toml_open_error: an unopenable file should have aborted"
    end subroutine scenario_toml_open_error

    !> A `pf_toml_strings` index outside `1 .. count` must abort, naming both numbers.
    subroutine scenario_toml_strings_range()
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text, one

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files)
        call files%get(9, one)
        print '(a)', "scenario_toml_strings_range: element 9 of 3 should have aborted"
    end subroutine scenario_toml_strings_range

    !> A key name longer than PF_TOML_MAX_KEY must abort, never be silently truncated.
    !!
    !! Truncation is the dangerous outcome rather than merely untidy: two distinct keys that
    !! truncate to the same text would compare equal in the accumulator, so one would hide the
    !! other from the unknown-key sweep.
    subroutine scenario_toml_key_too_long()
        type(pf_toml) :: conf
        character(len=PF_TOML_MAX_KEY + 1) :: huge_key
        integer :: n

        huge_key = repeat("k", PF_TOML_MAX_KEY + 1)
        call pf_toml_loads(conf, 'a = 1' // new_line("a"))
        call pf_toml_get(conf, huge_key, n, default = 0)
        print '(a)', "scenario_toml_key_too_long: an over-long key should have aborted"
    end subroutine scenario_toml_key_too_long

    !> A scalar read as a list must abort rather than leave the caller's array as it found it: an
    !! array that keeps its entry values is indistinguishable from one the file legitimately set.
    subroutine scenario_toml_value_not_list()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: v(3)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        v = 0
        call pf_toml_get(gen, "limits", v)     ! control: a key that really is a list
        print '(a,i0)', "control: the list read gave v(1)=", v(1)
        call pf_toml_get(gen, "nproc", v)      ! -> aborts: a scalar is not a list
        print '(a,i0)', "a scalar was accepted as a list, v(1)=", v(1)
    end subroutine scenario_toml_value_not_list

    !> Asking for a `[name]` section whose name is already a plain value must abort: handing back
    !! an empty section would make a misspelt configuration look like an omitted one.
    subroutine scenario_toml_name_not_a_section()
        type(pf_toml) :: conf, gen, sect

        call pf_toml_loads(conf, 'a = 1' // new_line("a") // '[real_one]' // new_line("a") // &
            'b = 2' // new_line("a"))
        call pf_toml_new_section(conf, "real_one", gen)   ! control: an existing section is reused
        print '(a,l1)', "control: the existing section was opened, is_open=", pf_toml_is_open(gen)
        call pf_toml_new_section(conf, "a", sect)         ! -> aborts: `a` is a value
        print '(a,l1)', "a value was accepted as a section, is_open=", pf_toml_is_open(sect)
    end subroutine scenario_toml_name_not_a_section

    !> A section.key path longer than the accumulator can hold must abort rather than truncate:
    !! two distinct paths truncated to the same text compare equal, so one would hide the other
    !! from the unknown-key sweep -- a silent under-report by the very check that exists to
    !! prevent one.
    subroutine scenario_toml_path_too_long()
        type(pf_toml) :: conf, short, s1, s2
        character(len=PF_TOML_MAX_KEY) :: long_name

        call pf_toml_new(conf, "paths")
        long_name = repeat("s", PF_TOML_MAX_KEY)
        call pf_toml_new_section(conf, "short", short)    ! control: a path well inside the limit
        print '(a,l1)', "control: the short section was created, is_open=", pf_toml_is_open(short)
        ! Each nesting level appends a name and a dot, so two maximum-length names run past
        ! PF_TOML_MAX_PATH without either name being over-long on its own. Separate handles at
        ! each level: one variable as both the parent and the result would alias an intent(in)
        ! dummy onto an intent(out) one, which is not conforming (F2018 15.5.2.13).
        call pf_toml_new_section(conf, long_name, s1)
        call pf_toml_new_section(s1, long_name, s2)       ! -> aborts
        print '(a,l1)', "an over-long section path was accepted, is_open=", pf_toml_is_open(s2)
    end subroutine scenario_toml_path_too_long

    !> `pf_toml_report` at its default severity is FATAL: a validation complaint the calling
    !! program raises stops the run, exactly as a type error found by this module does.
    subroutine scenario_toml_report_fatal()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_report(gen, "nproc", "nproc is only a warning here", &
            severity = PF_TOML_WARN)     ! control: a warning returns
        print '(a)', "control: the warning severity returned"
        call pf_toml_report(gen, "nproc", "nproc must be at least ten")   ! -> aborts
        print '(a)', "a fatal report was accepted"
    end subroutine scenario_toml_report_fatal

    !> A severity that is none of the three constants must abort rather than pick one.
    subroutine scenario_toml_bad_severity()
        type(pf_toml) :: conf

        call pf_toml_loads(conf, 'a = 1' // new_line("a"))
        call pf_toml_check(conf, severity = 42)
        print '(a)', "scenario_toml_bad_severity: an unknown severity should have aborted"
    end subroutine scenario_toml_bad_severity

    !> The document the four list-element scenarios read: one list of each TOML element type.
    !!
    !! Separate from `toml_sample` because what these need is a list of every type, to read a list
    !! of the WRONG type out of; the shared document has no reason to carry one.
    subroutine toml_lists(text)
        character(len=:), allocatable, intent(out) :: text  !! Receives the TOML document.
        character(len=1) :: nl

        nl = new_line("a")
        text = 'ints = [1, 2]' // nl // &
               'reals = [1.5, 2.5]' // nl // &
               'words = ["x", "y"]' // nl // &
               'flags = [true, false]' // nl
    end subroutine toml_lists

    !> A list whose ELEMENTS are of the wrong type aborts, as a wrong-typed scalar does.
    !!
    !! **The list path is not the scalar path with a loop around it**: it opens the value as an
    !! array, reads it into a temporary and then checks that the temporary came back allocated, so
    !! a list `toml-f` refuses leaves a different trail from a scalar it refuses. These four
    !! scenarios pin the four things this module can say about a list; every other site that says
    !! one of them is marked `GCOVR_EXCL_LINE`, and `fail_value`'s own doc-comment says why.
    !!
    !! Each reads a list that really IS of its type first, so a failure in the setup cannot be
    !! mistaken for the abort under test.
    subroutine scenario_toml_list_not_whole_numbers()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text
        integer :: v(2)

        call toml_lists(text)
        call pf_toml_loads(conf, text)
        v = 0
        call pf_toml_get(conf, "ints", v)      ! control: a list that really is whole numbers
        print '(a,i0)', "control: the integer list read gave v(1)=", v(1)
        call pf_toml_get(conf, "reals", v)     ! -> aborts: 1.5 is not a whole number
        print '(a,i0)', "a list of reals was accepted as whole numbers, v(1)=", v(1)
    end subroutine scenario_toml_list_not_whole_numbers

    !> A list of strings read as a list of numbers aborts. See `scenario_toml_list_not_whole_numbers`.
    subroutine scenario_toml_list_not_numbers()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text
        real(real64) :: v(2)

        call toml_lists(text)
        call pf_toml_loads(conf, text)
        v = 0.0_real64
        call pf_toml_get(conf, "reals", v)     ! control: a list that really is numbers
        print '(a,f6.3)', "control: the real list read gave v(1)=", v(1)
        call pf_toml_get(conf, "words", v)     ! -> aborts: "x" is not a number
        print '(a,f6.3)', "a list of strings was accepted as numbers, v(1)=", v(1)
    end subroutine scenario_toml_list_not_numbers

    !> A list of integers read as a list of true/false values aborts.
    subroutine scenario_toml_list_not_logicals()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text
        logical :: v(2)

        call toml_lists(text)
        call pf_toml_loads(conf, text)
        v = .false.
        call pf_toml_get(conf, "flags", v)     ! control: a list that really is true/false
        print '(a,l1)', "control: the logical list read gave v(1)=", v(1)
        call pf_toml_get(conf, "ints", v)      ! -> aborts: 1 is not true/false
        print '(a,l1)', "a list of integers was accepted as true/false, v(1)=", v(1)
    end subroutine scenario_toml_list_not_logicals

    !> A list of integers read as a list of strings aborts.
    subroutine scenario_toml_list_not_strings()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text
        character(len=8) :: v(2)

        call toml_lists(text)
        call pf_toml_loads(conf, text)
        v = ""
        call pf_toml_get(conf, "words", v)     ! control: a list that really is strings
        print '(a,a)', "control: the string list read gave v(1)=", trim(v(1))
        call pf_toml_get(conf, "ints", v)      ! -> aborts: 1 is not a string
        print '(a,a)', "a list of integers was accepted as strings, v(1)=", trim(v(1))
    end subroutine scenario_toml_list_not_strings

    !> `pf_toml_require` names EVERY missing key, then stops -- it does not stop at the first.
    !!
    !! Two absent keys, because the loop counts them and the summary line reports that count: a
    !! version that stopped at the first would pass a one-key test and silently lose the second
    !! name, which is the whole reason this procedure exists rather than a `pf_toml_get` each.
    subroutine scenario_toml_require_missing()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_require(gen, "nproc")        ! control: a key the file does set
        print '(a)', "control: a key that is set passed pf_toml_require"
        call pf_toml_require(gen, "alpha;beta")   ! -> aborts, naming both
        print '(a)', "scenario_toml_require_missing: two absent required keys should have aborted"
    end subroutine scenario_toml_require_missing

    !> A handle no `pf_toml_load`, `pf_toml_loads` or `pf_toml_new` has ever filled aborts.
    !!
    !! Distinct from `toml_closed_handle`, and the difference is the advice: that one has a
    !! document and merely failed to find its section, so it is told about `pf_toml_is_open`; this
    !! one has no document at all and is told to open one. `require_open` picks the arm by whether
    !! `%doc` is associated, so only a handle that was never opened reaches this text.
    subroutine scenario_toml_handle_never_opened()
        type(pf_toml) :: never
        integer :: n

        n = -1
        call pf_toml_get(never, "anything", n)
        print '(a,i0)', "an unopened handle was read from, n=", n
    end subroutine scenario_toml_handle_never_opened

    !> `pf_toml_save` writes the WHOLE configuration, so a section handle is refused.
    !!
    !! The twin of `toml_dump_not_owner`, and not a duplicate of it: the two name themselves in
    !! their own messages, and they write different documents (the effective one against the
    !! parsed one), so a caller handed the wrong refusal is being told to fix the wrong call.
    subroutine scenario_toml_save_not_owner()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_save(gen, "test_run/toml_save_not_owner.toml")
        print '(a)', "scenario_toml_save_not_owner: saving from a section should have aborted"
    end subroutine scenario_toml_save_not_owner

    !> A write that fails is FATAL and names the file, rather than being reported through a status.
    !!
    !! A configuration written back is usually the record of what a run actually used, so a save
    !! that quietly did nothing would leave that record missing exactly when it is wanted.
    !! `pf_toml_dump`'s identical three lines are marked `GCOVR_EXCL_START` rather than given a
    !! scenario of their own; the note there says why.
    subroutine scenario_toml_save_write_error()
        type(pf_toml) :: conf
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_save(conf, "test_run/no_such_directory_for_toml/out.toml")
        print '(a)', "scenario_toml_save_write_error: an unwritable path should have aborted"
    end subroutine scenario_toml_save_write_error

    !> An over-long element of a `default =` list is refused exactly as one read from the file is.
    !!
    !! `toml_string_too_long` is the file half. This is the other half of what
    !! `pf_toml_get_str_arr` promises, and it is the half the CALLER owns entirely: a default
    !! quietly clipped to the caller's own element length is a value nothing in the file explains,
    !! and nothing in the program says either.
    subroutine scenario_toml_default_string_too_long()
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        character(len=4) :: got(2)
        character(len=16) :: dflt(2)

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        dflt = [character(len=16) :: "ok", "far too long"]
        got = ""
        call pf_toml_get(gen, "absent_list", got, default = dflt)
        print '(a,a)', "an over-long default element was accepted, got(2)=", trim(got(2))
    end subroutine scenario_toml_default_string_too_long

    !> A required `[[name]]` the file has no entries of at all aborts, in the `[[...]]` wording.
    !!
    !! `toml_missing_section` is the `[name]` half. The two share one helper and differ only in the
    !! headline it builds, so the wording is exactly what this pins -- a reader told the file has
    !! no `[region]` section when it is `[[region]]` entries that are wanted looks for the wrong
    !! thing.
    subroutine scenario_toml_no_entries_at_all()
        type(pf_toml) :: conf, ent
        character(len=:), allocatable :: text

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "region", 1, ent)   ! control: [[region]] does have an entry
        print '(a,l1)', "control: the existing entry opened, is_open=", pf_toml_is_open(ent)
        call pf_toml_section(conf, "no_such_entries", 1, ent, required = .true.)
        print '(a,l1)', "an absent [[name]] was accepted, is_open=", pf_toml_is_open(ent)
    end subroutine scenario_toml_no_entries_at_all

    !> NEGATIVE CONTROL for the whole group: the same setup, every legal path, exit 0.
    !!
    !! Without this, a scenario above could be aborting in `toml_sample` or `pf_toml_loads` rather
    !! than at the call it names, and every one of them would still report "aborted as expected".
    subroutine scenario_toml_control()
        type(pf_toml) :: conf, gen, ent, sect
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text, name, one
        character(len=32) :: fixed(3)
        integer :: n, i, id, lev, limits(3), status
        real(real64) :: f
        logical :: found

        call toml_sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_get(conf, "title", name)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", n)
        call pf_toml_get(gen, "factor", f)
        call pf_toml_get(gen, "name", name)
        call pf_toml_get(gen, "limits", limits)
        call pf_toml_get(gen, "files", fixed)
        call pf_toml_get_strings(gen, "files", files)
        call files%get(2, one)
        call pf_toml_get_level(gen, "level", lev)
        call pf_toml_get(gen, "absent", n, default = 3)
        call pf_toml_retire(gen, "gone_key", "nothing to do")
        call pf_toml_require(gen, "nproc;factor")
        call pf_toml_check(gen)
        call pf_toml_section(conf, "not_there", sect, required = .false., found = found)
        call pf_toml_section(conf, "plain", sect)
        call pf_toml_get(sect, "x", n)
        do i = 1, pf_toml_section_count(conf, "region")
            call pf_toml_section(conf, "region", i, ent)
            call pf_toml_get(ent, "id", id)
        end do
        call pf_toml_check_all(conf)
        call pf_toml_update(gen, "nproc", 5)
        call pf_toml_set(gen, "brand_new", 6)
        call pf_toml_close(conf)

        call pf_toml_load(conf, "test_run/no_such_file_for_the_control.toml", status)
        if (status /= PF_TOML_ERR_OPEN) then
            print '(a)', "scenario_toml_control: a missing file should report PF_TOML_ERR_OPEN"
            error stop "scenario_toml_control: wrong status"
        end if
        call pf_toml_close(conf)
        print '(a)', "scenario_toml_control: every legal parquet_toml path completed"
    end subroutine scenario_toml_control

end module error_scenarios_analysis
