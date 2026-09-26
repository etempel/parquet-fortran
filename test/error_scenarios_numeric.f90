!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Error scenarios for the index containers and the numeric tier: `pf_index_map`/`pf_index_pool`/
!> `pf_index_multimap` and the table's grouping, indexing and filtering verbs, the message-stream
!> routing of `%print_rows`/`%print_stat`, then quadrature, interpolation, cosmology,
!> optimisation, root finding, transforms and KDE.
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
module error_scenarios_numeric
    use parquet
    use parquet_strings, only : parquet_string_column, parquet_string
    use parquet_columns
    use parquet_list, only : parquet_list_column
    use parquet_index, only : pf_index_map, pf_index_pool, pf_index_max_components, pf_index_multimap, &
        parquet_debug_index_partition, parquet_debug_index_spills, &
        parquet_debug_set_index_pair_limit, parquet_debug_set_index_string_hash_bits
    ! The quadrature scenarios' integrands: module procedures shared with test_integrate.f90,
    ! so a scenario and a test can name the same integrand and neither reaches an internal one.
    use test_integrate_support, only : runge
    ! NOT re-exported by the `parquet` facade: a caller that wants a cosmology from a
    ! configuration file names this module, as it already names `parquet_toml`.
    use parquet_cosmology_config, only : pf_cosmology_from_toml
    ! The interpolation scenarios' non-finite fixtures, built with `ieee_value` so no fixture's own
    ! arithmetic raises the flag the library is being asked about.
    use test_interpolate_support, only : nan_value, positive_infinity
    ! The optimisation scenarios' objectives: module procedures and module-level types shared with
    ! test_optimize.f90, so a scenario and a test name the same objective and neither reaches an
    ! internal procedure.
    use test_optimize_support, only : quad1d, sphere, always_nan, nan_beyond_two, unit_disc, &
        dist12, outside_disc, negative_count_disc, nan_constraint_disc, nan_value_disc
    ! The root-finding scenarios' functions: module procedures shared with test_root.f90, so a
    ! scenario and a test name the same function and neither reaches an internal procedure.
    use test_root_support, only : root_sq2, root_line_03, root_nan_beyond_two
    use parquet_prima, only : pf_minimize_bobyqa, pf_minimize_lincoa, pf_minimize_cobyla, &
        pf_bobyqa_solver
    use parquet_optimize, only : pf_minimize_scalar, pf_minimize_simplex, pf_minimize_de, &
        pf_minimize_multistart, pf_simplex_solver
    use parquet_tables
    ! The grouping scenarios' callbacks: module procedures, since an internal one of this
    ! program cannot be passed as a callback under every supported compiler.
    use test_group_callbacks, only : scenario_group_row_sum, scenario_group_two, scenario_group_reducer, &
        scenario_group_col_mean
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use iso_fortran_env, only : int32, int64, real32, real64
    use error_scenarios_support, only : join_fixture, write_list_scenario_fixture, write_print_rows_fixture, &
        write_scenario_maml_file
    implicit none
    private

    public :: dispatch_error_scenarios_numeric

contains

    !> Run `scenario` if it is one of this module's, and report whether it was.
    !!
    !! `handled` is `.false.` for a name this group does not own, which is how
    !! `error_scenarios.f90` walks the four groups in turn without any of them knowing
    !! what the others hold.
    subroutine dispatch_error_scenarios_numeric(scenario, handled)
        character(len=*), intent(in) :: scenario
        logical, intent(out) :: handled

        handled = .true.
        select case (trim(scenario))
        case ("print_rows_negative_last")
            call scenario_print_rows_negative_last()
        case ("print_rows_follows_message_stream")
            call scenario_print_rows_follows_message_stream()
        case ("print_stat_follows_message_stream")
            call scenario_print_stat_follows_message_stream()
        case ("string_print_follows_message_stream")
            call scenario_string_print_follows_message_stream()
        case ("print_settings_follows_message_stream")
            call scenario_print_settings_follows_message_stream()
        case ("print_schema_info_default_stream")
            call scenario_print_schema_info_default_stream()
        case ("error_context_follows_message_stream")
            call scenario_error_context_follows_message_stream()
        case ("reader_print_stat_follows_message_stream")
            call scenario_reader_print_stat_follows_message_stream()
        case ("close_reader_print_stat_silenced_late")
            call scenario_close_reader_print_stat_silenced_late()
        case ("string_print_unbound_handle_silent")
            call scenario_string_print_unbound_handle_silent()
        case ("get_matrix_unsupported_column")
            call scenario_get_matrix_unsupported_column()
        case ("keep_columns_many_missing")
            call scenario_keep_columns_many_missing()
        case ("join_columns_unsupported")
            call scenario_join_columns_unsupported()
        case ("derive_schema_blank_name")
            call scenario_derive_schema_blank_name()
        case ("open_writer_like_nothing_resident")
            call scenario_open_writer_like_nothing_resident()
        case ("sink_schema_names_an_unreadable_column")
            call scenario_sink_schema_names_an_unreadable_column()
        case ("agg_int64_sum_overflow")
            call scenario_agg_int64_sum_overflow()
        case ("join_max_rows_hash_string_key")
            call scenario_join_max_rows_hash_keyshape("string")
        case ("join_max_rows_hash_tuple_key")
            call scenario_join_max_rows_hash_keyshape("tuple")
        case ("healpix_disc_vector_infinite")
            call scenario_healpix_disc_vector_infinite()
        case ("index_build_duplicate_direct")
            call scenario_index_build_duplicate_direct()
        case ("index_build_duplicate_hash")
            call scenario_index_build_duplicate_hash()
        case ("index_build_duplicate_sorted")
            call scenario_index_build_duplicate_sorted()
        case ("index_build_duplicate_tuple_direct")
            call scenario_index_build_duplicate_tuple_direct()
        case ("index_build_duplicate_tuple_hash")
            call scenario_index_build_duplicate_tuple_hash()
        case ("index_build_value_zero")
            call scenario_index_build_value_zero()
        case ("index_build_values_length")
            call scenario_index_build_values_length()
        case ("index_get_many_length")
            call scenario_index_get_many_length()
        case ("index_tuple_width_mismatch")
            call scenario_index_tuple_width_mismatch()
        case ("index_scalar_on_composite")
            call scenario_index_scalar_on_composite()
        case ("index_sorted_set")
            call scenario_index_sorted_set()
        case ("index_sorted_remove")
            call scenario_index_sorted_remove()
        case ("index_sorted_get_or_add_absent")
            call scenario_index_sorted_get_or_add_absent(many=.false.)
        case ("index_sorted_get_or_add_many_absent")
            call scenario_index_sorted_get_or_add_absent(many=.true.)
        case ("index_sorted_composite")
            call scenario_index_sorted_composite()
        case ("index_init_direct")
            call scenario_index_init_direct()
        case ("index_init_sorted")
            call scenario_index_init_sorted()
        case ("index_bad_method")
            call scenario_index_bad_method()
        case ("index_direct_set_out_of_range")
            call scenario_index_direct_set_out_of_range()
        case ("index_direct_get_or_add_out_of_range")
            call scenario_index_direct_get_or_add_out_of_range(many=.false.)
        case ("index_direct_get_or_add_many_out_of_range")
            call scenario_index_direct_get_or_add_out_of_range(many=.true.)
        case ("index_ncomp_too_large")
            call scenario_index_ncomp_too_large()
        case ("index_direct_range_too_wide")
            call scenario_index_direct_range_too_wide()
        case ("index_remove_absent")
            call scenario_index_remove_absent()
        case ("index_get_many_int32_overflow")
            call scenario_index_get_many_int32_overflow()
        case ("index_build_threads_zero")
            call scenario_index_build_threads_zero()
        case ("index_get_many_valid_length")
            call scenario_index_get_many_valid_length()
        case ("index_get_many_threads_zero")
            call scenario_index_get_many_threads_zero()
        case ("index_get_or_add_many_length")
            call scenario_index_get_or_add_many_length()
        case ("index_get_or_add_many_valid_length")
            call scenario_index_get_or_add_many_valid_length()
        case ("index_get_or_add_many_int32_overflow")
            call scenario_index_get_or_add_many_int32_overflow()
        case ("index_build_valid_length")
            call scenario_index_build_valid_length()
        case ("index_keys_rank1_on_composite")
            call scenario_index_keys_rank1_on_composite()
        case ("index_masked_duplicate_direct")
            call scenario_index_masked_duplicate_direct()
        case ("index_masked_duplicate_tuple_direct")
            call scenario_index_masked_duplicate_tuple_direct()
        case ("index_set_value_zero")
            call scenario_index_set_value_zero()
        case ("index_method_token_too_long")
            call scenario_index_method_token_too_long()
        case ("index_get_many_on_string_map")
            call scenario_index_get_many_on_string_map()
        case ("index_get_many_ncomp_mismatch")
            call scenario_index_get_many_ncomp_mismatch()
        case ("index_keys_rank2_on_string_map")
            call scenario_index_keys_rank2_on_string_map()
        case ("index_composite_direct_product_too_wide")
            call scenario_index_composite_direct_product_too_wide()
        case ("index_direct_alloc_refused")
            call scenario_index_direct_alloc_refused()
        case ("index_control")
            call scenario_index_control()
        case ("index_concurrent_abort")
            call scenario_index_concurrent_abort()
        case ("index_spill_duplicate")
            call scenario_index_spill_duplicate()
        case ("index_partitioned_duplicate")
            call scenario_index_partitioned_duplicate()
        case ("index_partitioned_duplicate_one_column")
            call scenario_index_partitioned_duplicate_one_column()
        case ("index_partitioned_duplicate_tuple")
            call scenario_index_partitioned_duplicate_tuple()
        case ("index_str_partitioned_duplicate")
            call scenario_index_str_partitioned_duplicate()
        case ("index_get_or_add_many_threads_zero")
            call scenario_index_get_or_add_many_threads_zero()
        case ("index_partition_control")
            call scenario_index_partition_control()
        case ("multimap_build_values_length")
            call scenario_multimap_build_values_length()
        case ("multimap_build_value_zero")
            call scenario_multimap_build_value_zero()
        case ("multimap_build_valid_length")
            call scenario_multimap_build_valid_length()
        case ("multimap_build_threads_zero")
            call scenario_multimap_build_threads_zero()
        case ("multimap_build_bad_method")
            call scenario_multimap_build_bad_method()
        case ("multimap_build_sorted_composite")
            call scenario_multimap_build_sorted_composite()
        case ("multimap_get_first_many_length")
            call scenario_multimap_get_first_many_length()
        case ("multimap_get_first_many_valid_length")
            call scenario_multimap_get_first_many_valid_length()
        case ("multimap_get_first_many_threads_zero")
            call scenario_multimap_get_first_many_threads_zero()
        case ("multimap_get_many_length")
            call scenario_multimap_get_many_length()
        case ("multimap_probe_many_valid_length")
            call scenario_multimap_probe_many_valid_length()
        case ("multimap_probe_many_threads_zero")
            call scenario_multimap_probe_many_threads_zero()
        case ("multimap_probe_many_pair_overflow")
            call scenario_multimap_probe_many_pair_overflow()
        case ("multimap_int32_answer_overflow")
            call scenario_multimap_int32_answer_overflow()
        case ("multimap_get_all_int32_overflow")
            call scenario_multimap_get_all_int32_overflow()
        case ("multimap_tuple_width_mismatch")
            call scenario_multimap_tuple_width_mismatch()
        case ("multimap_scalar_on_composite")
            call scenario_multimap_scalar_on_composite()
        case ("multimap_keys_rank1_on_composite")
            call scenario_multimap_keys_rank1_on_composite()
        case ("multimap_probe_shape_mismatch")
            call scenario_multimap_probe_shape_mismatch()
        case ("multimap_tuple_on_string_map")
            call scenario_multimap_tuple_on_string_map()
        case ("multimap_integer_on_string_map")
            call scenario_multimap_integer_on_string_map()
        case ("multimap_get_first_many_on_string_map")
            call scenario_multimap_get_first_many_on_string_map()
        case ("multimap_get_first_many_ncomp_mismatch")
            call scenario_multimap_get_first_many_ncomp_mismatch()
        case ("multimap_probe_many_on_string_map")
            call scenario_multimap_probe_many_on_string_map()
        case ("multimap_direct_range_too_wide")
            call scenario_multimap_direct_range_too_wide()
        case ("multimap_composite_direct_product_too_wide")
            call scenario_multimap_composite_direct_product_too_wide()
        case ("multimap_direct_alloc_refused")
            call scenario_multimap_direct_alloc_refused()
        case ("multimap_control")
            call scenario_multimap_control()
        case ("index_string_on_integer_map")
            call scenario_index_string_on_integer_map()
        case ("index_integer_on_string_map")
            call scenario_index_integer_on_string_map()
        case ("index_tuple_on_string_map")
            call scenario_index_tuple_on_string_map()
        case ("index_string_get_many_on_integer_map")
            call scenario_index_string_get_many_on_integer_map()
        case ("index_string_method_direct")
            call scenario_index_string_method_direct()
        case ("index_string_method_sorted")
            call scenario_index_string_method_sorted()
        case ("index_string_build_duplicate")
            call scenario_index_string_build_duplicate()
        case ("index_string_keys_rank1")
            call scenario_index_string_keys_rank1()
        case ("index_string_keys_column_on_integer")
            call scenario_index_string_keys_column_on_integer()
        case ("index_string_init_ncomp")
            call scenario_index_string_init_ncomp()
        case ("index_string_set_on_integer_map")
            call scenario_index_string_set_on_integer_map()
        case ("index_string_remove_absent")
            call scenario_index_string_remove_absent()
        case ("index_string_get_many_length")
            call scenario_index_string_get_many_length()
        case ("multimap_string_on_integer")
            call scenario_multimap_string_on_integer()
        case ("multimap_integer_on_string")
            call scenario_multimap_integer_on_string()
        case ("multimap_string_method_sorted")
            call scenario_multimap_string_method_sorted()
        case ("multimap_string_probe_on_integer")
            call scenario_multimap_string_probe_on_integer()
        case ("multimap_string_keys_rank1")
            call scenario_multimap_string_keys_rank1()
        case ("index_keys_int32_negative")
            call scenario_index_keys_int32_negative()
        case ("index_keys_rank2_int32_negative")
            call scenario_index_keys_rank2_int32_negative()
        case ("multimap_csr_int32_value")
            call scenario_multimap_csr_int32_value()
        case ("index_keys_int32_on_string_map")
            call scenario_index_keys_int32_on_string_map()
        case ("index_keys_rank1_int32_on_composite")
            call scenario_index_keys_rank1_int32_on_composite()
        case ("index_keys_rank2_int32_on_string_map")
            call scenario_index_keys_rank2_int32_on_string_map()
        case ("multimap_keys_rank2_on_string_map")
            call scenario_multimap_keys_rank2_on_string_map()
        case ("multimap_keys_int32_on_string_map")
            call scenario_multimap_keys_int32_on_string_map()
        case ("multimap_keys_rank1_int32_on_composite")
            call scenario_multimap_keys_rank1_int32_on_composite()
        case ("multimap_keys_rank2_int32_on_string_map")
            call scenario_multimap_keys_rank2_int32_on_string_map()
        case ("multimap_string_keys_column_on_integer")
            call scenario_multimap_string_keys_column_on_integer()
        case ("multimap_string_bulk_length")
            call scenario_multimap_string_bulk_length()
        case ("multimap_string_bulk_on_integer")
            call scenario_multimap_string_bulk_on_integer()
        case ("index_pool_reserve_negative")
            call scenario_index_pool_reserve_negative()
        case ("index_get_or_add_many_threaded_int32_overflow")
            call scenario_index_get_or_add_many_threaded_int32_overflow()
        case ("index_string_control")
            call scenario_index_string_control()
        case ("index_string_chain_warning")
            call scenario_index_string_chain(narrow=.true.)
        case ("index_string_chain_quiet")
            call scenario_index_string_chain(narrow=.false.)
        case ("table_index_string_key_on_int")
            call scenario_table_index_string_key_on_int()
        case ("table_index_int_key_on_string")
            call scenario_table_index_int_key_on_string()
        case ("table_index_string_column")
            call scenario_table_index_string_column()
        case ("table_index_bool_column")
            call scenario_table_index_bool_column()
        case ("table_index_vector_column")
            call scenario_table_index_vector_column()
        case ("table_index_duplicate_unique")
            call scenario_table_index_duplicate_unique()
        case ("table_index_missing_column")
            call scenario_table_index_missing_column()
        case ("table_index_stale_find")
            call scenario_table_index_stale_find()
        case ("table_index_stale_find_all")
            call scenario_table_index_stale_find_all()
        case ("table_index_stale_find_many")
            call scenario_table_index_stale_find_many()
        case ("table_index_stale_count")
            call scenario_table_index_stale_count()
        case ("table_index_never_built")
            call scenario_table_index_never_built()
        case ("table_index_kind_mismatch")
            call scenario_table_index_kind_mismatch()
        case ("table_index_kind_mismatch_temporal")
            call scenario_table_index_kind_mismatch_temporal()
        case ("table_index_date_key_on_int")
            call scenario_table_index_date_key_on_int("date")
        case ("table_index_time_key_on_int")
            call scenario_table_index_date_key_on_int("time")
        case ("table_index_find_many_length")
            call scenario_table_index_find_many_length()
        case ("table_index_threads_zero")
            call scenario_table_index_threads_zero()
        case ("table_index_control")
            call scenario_table_index_control()
        case ("table_group_no_key")
            call scenario_table_group_no_key()
        case ("table_group_unknown_key")
            call scenario_table_group_unknown_key()
        case ("table_group_direction_token")
            call scenario_table_group_direction_token()
        case ("table_group_vector_column")
            call scenario_table_group_vector_column()
        case ("table_group_container_column")
            call scenario_table_group_container_column()
        case ("table_group_stale_size")
            call scenario_table_group_stale_size()
        case ("table_group_stale_rows")
            call scenario_table_group_stale_rows()
        case ("table_group_stale_csr")
            call scenario_table_group_stale_csr()
        case ("table_group_stale_first_rows")
            call scenario_table_group_stale_first_rows()
        case ("table_group_stale_group_ids")
            call scenario_table_group_stale_group_ids()
        case ("table_group_stale_key_table")
            call scenario_table_group_stale_key_table()
        case ("table_group_stale_count")
            call scenario_table_group_stale_count()
        case ("table_group_never_built")
            call scenario_table_group_never_built()
        case ("table_group_out_of_range")
            call scenario_table_group_out_of_range()
        case ("table_group_size_name_clash")
            call scenario_table_group_size_name_clash()
        case ("table_group_key_table_reserve_negative")
            call scenario_table_group_key_table_reserve_negative()
        case ("table_group_add_agg_target_is_source")
            call scenario_table_group_add_agg_target_is_source()
        case ("table_group_add_agg_rows_mismatch")
            call scenario_table_group_add_agg_rows_mismatch()
        case ("table_group_add_agg_as_two_names")
            call scenario_table_group_add_agg_as_two_names()
        case ("table_group_add_agg_nan_to_null_exact")
            call scenario_table_group_add_agg_nan_to_null_exact()
        case ("table_group_add_agg_weights_exact")
            call scenario_table_group_add_agg_weights_exact()
        case ("column_container_wrong_kind")
            call scenario_column_container_wrong_kind()
        case ("column_append_row_of_container")
            call scenario_column_append_row_of_container()
        case ("filter_set_name_too_long")
            call scenario_filter_set_name_too_long()
        case ("filter_bare_at_set_name")
            call scenario_filter_bare_at_set_name()
        case ("skycoord_text_long_separator")
            call scenario_skycoord_text_long_separator()
        case ("table_group_stale_add_agg")
            call scenario_table_group_stale_add_agg()
        case ("table_group_stale_add_size")
            call scenario_table_group_stale_add_size()
        case ("table_group_add_agg_unknown_token")
            call scenario_table_group_add_agg_unknown_token()
        case ("table_group_add_size_name_taken")
            call scenario_table_group_add_size_name_taken()
        case ("table_group_add_apply_target_is_source")
            call scenario_table_group_add_apply_target_is_source()
        case ("table_group_add_apply_no_name")
            call scenario_table_group_add_apply_no_name()
        case ("table_group_add_apply_duplicate_name")
            call scenario_table_group_add_apply_duplicate_name()
        case ("table_group_add_apply_name_taken")
            call scenario_table_group_add_apply_name_taken()
        case ("table_group_stale_add_apply")
            call scenario_table_group_stale_add_apply()
        case ("table_group_add_apply_threads_zero")
            call scenario_table_group_add_apply_threads_zero()
        case ("table_group_rows_g32_out_of_range")
            call scenario_table_group_rows_g32_out_of_range()
        case ("table_group_gather_g32_short_buffer")
            call scenario_table_group_gather_g32_short_buffer()
        case ("table_group_stale_apply")
            call scenario_table_group_stale_apply()
        case ("table_group_stale_apply_object")
            call scenario_table_group_stale_apply_object()
        case ("table_group_apply_nout")
            call scenario_table_group_apply_nout()
        case ("table_group_apply_threads_zero")
            call scenario_table_group_apply_threads_zero()
        case ("table_group_stale_agg")
            call scenario_table_group_stale_agg()
        case ("table_group_stale_nunique")
            call scenario_table_group_stale_nunique()
        case ("table_group_agg_unknown_token")
            call scenario_table_group_agg_unknown_token()
        case ("table_group_agg_quantile_needs_q")
            call scenario_table_group_agg_quantile_needs_q()
        case ("table_group_agg_option_refused")
            call scenario_table_group_agg_option_refused()
        case ("table_group_agg_int_real_column")
            call scenario_table_group_agg_int_real_column()
        case ("table_group_agg_int_sum_overflow")
            call scenario_table_group_agg_int_sum_overflow()
        case ("table_group_agg_int_all_null")
            call scenario_table_group_agg_int_all_null()
        case ("table_group_agg_string_column")
            call scenario_table_group_agg_string_column()
        case ("table_group_agg_weights_length")
            call scenario_table_group_agg_weights_length()
        case ("table_group_agg_negative_weight")
            call scenario_table_group_agg_negative_weight()
        case ("table_group_agg_both_weights")
            call scenario_table_group_agg_both_weights()
        case ("table_group_agg_weight_column_string")
            call scenario_table_group_agg_weight_column_string()
        case ("table_group_agg_weight_column_vector")
            call scenario_table_group_agg_weight_column_vector()
        case ("table_group_agg_weights_ignored")
            call scenario_table_group_agg_weights_ignored()
        case ("table_group_nunique_vector_column")
            call scenario_table_group_nunique_vector_column()
        case ("table_group_stale_broadcast")
            call scenario_table_group_stale_broadcast()
        case ("table_group_stale_gather")
            call scenario_table_group_stale_gather()
        case ("table_group_gather_short_buffer")
            call scenario_table_group_gather_short_buffer()
        case ("table_group_gather_short_valid")
            call scenario_table_group_gather_short_valid()
        case ("table_group_gather_out_of_range")
            call scenario_table_group_gather_out_of_range()
        case ("table_group_gather_kind")
            call scenario_table_group_gather_kind()
        case ("table_group_broadcast_length")
            call scenario_table_group_broadcast_length()
        case ("table_group_blank_key")
            call scenario_table_group_blank_key()
        case ("table_group_direction_word")
            call scenario_table_group_direction_word()
        case ("table_group_query_unknown_column")
            call scenario_table_group_query_unknown_column()
        case ("table_group_query_unsupported_column")
            call scenario_table_group_query_unsupported_column()
        case ("table_group_agg_exact_unknown_token")
            call scenario_table_group_agg_exact_unknown_token()
        case ("table_group_agg_method_refused")
            call scenario_table_group_agg_method_refused()
        case ("table_group_agg_ddof_refused")
            call scenario_table_group_agg_ddof_refused()
        case ("table_group_agg_scale_refused")
            call scenario_table_group_agg_scale_refused()
        case ("table_group_agg_nan_weight")
            call scenario_table_group_agg_nan_weight()
        case ("table_group_agg_infinite_weight")
            call scenario_table_group_agg_infinite_weight()
        case ("filter_temporal_set_mismatch")
            call scenario_filter_temporal_set_mismatch()
        case ("filter_time_set_on_int")
            call scenario_filter_time_set_on_int()
        case ("filter_timestamp_set_on_int")
            call scenario_filter_timestamp_set_on_int()
        case ("filter_temporal_literal_list")
            call scenario_filter_temporal_literal_list()
        case ("pool_double_free")
            call scenario_pool_double_free()
        case ("pool_free_never_issued")
            call scenario_pool_free_never_issued()
        case ("pool_free_zero")
            call scenario_pool_free_zero()
        case ("pool_control")
            call scenario_pool_control()
        case ("bounded_table_no_whole_column_read")
            call scenario_bounded_table_no_whole_column_read()
        case ("bounded_clone_no_whole_column_read")
            call scenario_bounded_clone_no_whole_column_read()
        case ("default_filter_reads_whole_column")
            call scenario_default_filter_reads_whole_column()
        case ("bounded_with_sort_refused")
            call scenario_bounded_with_sort_refused()
        case ("bounded_with_maml_sort_refused")
            call scenario_bounded_with_maml_sort_refused()
        case ("bounded_qc_hard_at_first_touch")
            call scenario_bounded_qc_hard_at_first_touch()
        case ("bounded_qc_soft_warns")
            call scenario_bounded_qc_soft_warns()
        case ("bounded_arrow_pool")
            call scenario_bounded_arrow_pool()
        case ("integrate_negative_rtol")
            call scenario_integrate_negative_rtol()
        case ("integrate_nan_rtol")
            call scenario_integrate_nan_rtol()
        case ("integrate_negative_atol")
            call scenario_integrate_negative_atol()
        case ("integrate_zero_tolerances")
            call scenario_integrate_zero_tolerances()
        case ("integrate_rtol_below_floor")
            call scenario_integrate_rtol_below_floor()
        case ("integrate_bad_max_neval")
            call scenario_integrate_bad_max_neval()
        case ("integrate_huge_max_neval")
            call scenario_integrate_huge_max_neval()
        case ("integrate_nan_bound")
            call scenario_integrate_nan_bound()
        case ("integrate_reversed_bounds")
            call scenario_integrate_reversed_bounds()
        case ("integrate_bad_max_panels")
            call scenario_integrate_bad_max_panels()
        case ("integrate_max_panels_finite")
            call scenario_integrate_max_panels_finite()
        case ("integrate_same_infinity")
            call scenario_integrate_same_infinity()
        case ("integrate_log_base_infinite")
            call scenario_integrate_log_base_infinite()
        case ("integrate_log_base_nonpositive")
            call scenario_integrate_log_base_nonpositive()
        case ("integrate_breakpoints_nan")
            call scenario_integrate_breakpoints_nan()
        case ("integrate_breakpoints_outside")
            call scenario_integrate_breakpoints_outside()
        case ("integrate_breakpoints_duplicate")
            call scenario_integrate_breakpoints_duplicate()
        case ("integrate_context_reported")
            call scenario_integrate_context_reported()
        case ("integrate_context_capped")
            call scenario_integrate_context_capped()
        case ("interpolate_size_mismatch")
            call scenario_interpolate_size_mismatch()
        case ("interpolate_is_valid_size")
            call scenario_interpolate_is_valid_size()
        case ("interpolate_unknown_method")
            call scenario_interpolate_unknown_method()
        case ("interpolate_unknown_outside")
            call scenario_interpolate_unknown_outside()
        case ("interpolate_too_few_points")
            call scenario_interpolate_too_few_points()
        case ("interpolate_too_few_after_is_valid")
            call scenario_interpolate_too_few_after_is_valid()
        case ("interpolate_repeated_x")
            call scenario_interpolate_repeated_x()
        case ("interpolate_unsorted_x")
            call scenario_interpolate_unsorted_x()
        case ("interpolate_nan_in_x")
            call scenario_interpolate_nan_in_x()
        case ("interpolate_inf_in_x")
            call scenario_interpolate_inf_in_x()
        case ("interpolate_nan_in_y")
            call scenario_interpolate_nan_in_y()
        case ("interpolate_inf_in_y")
            call scenario_interpolate_inf_in_y()
        case ("cosmology_eval_before_init")
            call scenario_cosmology_eval_before_init()
        case ("cosmology_eval_after_clear")
            call scenario_cosmology_eval_after_clear()
        case ("cosmology_init_unknown_name")
            call scenario_cosmology_init_unknown_name()
        case ("cosmology_init_h0_out_of_range")
            call scenario_cosmology_init_h0_out_of_range()
        case ("cosmology_init_om0_negative")
            call scenario_cosmology_init_om0_negative()
        case ("cosmology_init_tcmb0_negative")
            call scenario_cosmology_init_tcmb0_negative()
        case ("cosmology_init_m_nu_size")
            call scenario_cosmology_init_m_nu_size()
        case ("cosmology_init_w0_out_of_range")
            call scenario_cosmology_init_w0_out_of_range()
        case ("cosmology_init_wa_out_of_range")
            call scenario_cosmology_init_wa_out_of_range()
        case ("cosmology_init_ode0_not_finite")
            call scenario_cosmology_init_ode0_not_finite()
        case ("cosmology_init_neff_negative")
            call scenario_cosmology_init_neff_negative()
        case ("cosmology_init_m_nu_negative")
            call scenario_cosmology_init_m_nu_negative()
        case ("cosmology_init_context_capped")
            call scenario_cosmology_init_context_capped()
        case ("cosmology_init_zmax_out_of_range")
            call scenario_cosmology_init_zmax_out_of_range()
        case ("cosmology_init_zmin_out_of_range")
            call scenario_cosmology_init_zmin_out_of_range()
        case ("cosmology_init_ob0_above_om0")
            call scenario_cosmology_init_ob0_above_om0()
        case ("cosmology_init_density_too_large")
            call scenario_cosmology_init_density_too_large()
        case ("cosmology_init_tcmb0_too_hot")
            call scenario_cosmology_init_tcmb0_too_hot()
        case ("cosmology_init_no_big_bang")
            call scenario_cosmology_init_no_big_bang()
        case ("cosmology_init_table_not_converged")
            call scenario_cosmology_init_table_not_converged()
        case ("cosmology_config_missing_section")
            call scenario_cosmology_config_missing_section()
        case ("cosmology_config_named_with_parameters")
            call scenario_cosmology_config_named_with_parameters()
        case ("cosmology_config_array_of_tables")
            call scenario_cosmology_config_array_of_tables()
        case ("cosmology_config_bad_type")
            call scenario_cosmology_config_bad_type()
        case ("cosmology_config_mnu_length")
            call scenario_cosmology_config_mnu_length()
        case ("cosmology_config_out_of_range")
            call scenario_cosmology_config_out_of_range()
        case ("cosmology_config_label_without_parameters")
            call scenario_cosmology_config_label_without_parameters()
        case ("interpolate_eval_before_init")
            call scenario_interpolate_eval_before_init()
        case ("interpolate_context_reported")
            call scenario_interpolate_context_reported()
        case ("interpolate_context_capped")
            call scenario_interpolate_context_capped()
        case ("interpolate_one_shot_size_mismatch")
            call scenario_interpolate_one_shot_size_mismatch()
        case ("interpolate_bc_with_linear")
            call scenario_interpolate_bc_with_linear()
        case ("interpolate_unknown_bc")
            call scenario_interpolate_unknown_bc()
        case ("interpolate_clamped_without_slopes")
            call scenario_interpolate_clamped_without_slopes()
        case ("interpolate_slopes_without_clamped")
            call scenario_interpolate_slopes_without_clamped()
        case ("interpolate_slopes_size")
            call scenario_interpolate_slopes_size()
        case ("interpolate_slopes_nan")
            call scenario_interpolate_slopes_nan()
        case ("interpolate_slopes_inf")
            call scenario_interpolate_slopes_inf()
        case ("interpolate_not_a_knot_too_few")
            call scenario_interpolate_not_a_knot_too_few()
        case ("interpolate_linear_too_few")
            call scenario_interpolate_linear_too_few()
        case ("interpolate_pchip_too_few")
            call scenario_interpolate_pchip_too_few()
        case ("interpolate_derivative_before_init")
            call scenario_interpolate_derivative_before_init()
        case ("interpolate_integral_before_init")
            call scenario_interpolate_integral_before_init()
        case ("interpolate_bad_order")
            call scenario_interpolate_bad_order()
        case ("interpolate_2d_shape")
            call scenario_interpolate_2d_shape()
        case ("interpolate_2d_unknown_method")
            call scenario_interpolate_2d_unknown_method()
        case ("interpolate_2d_pchip")
            call scenario_interpolate_2d_pchip()
        case ("interpolate_2d_bc_with_linear")
            call scenario_interpolate_2d_bc_with_linear()
        case ("interpolate_2d_unknown_bc")
            call scenario_interpolate_2d_unknown_bc()
        case ("interpolate_2d_clamped")
            call scenario_interpolate_2d_clamped()
        case ("interpolate_2d_unknown_outside")
            call scenario_interpolate_2d_unknown_outside()
        case ("interpolate_2d_too_few_x")
            call scenario_interpolate_2d_too_few_x()
        case ("interpolate_2d_not_a_knot_too_few")
            call scenario_interpolate_2d_not_a_knot_too_few()
        case ("interpolate_2d_x_not_monotonic")
            call scenario_interpolate_2d_x_not_monotonic()
        case ("interpolate_2d_y_not_monotonic")
            call scenario_interpolate_2d_y_not_monotonic()
        case ("interpolate_2d_inf_in_x")
            call scenario_interpolate_2d_inf_in_x()
        case ("interpolate_2d_inf_in_y")
            call scenario_interpolate_2d_inf_in_y()
        case ("interpolate_2d_nan_in_z")
            call scenario_interpolate_2d_nan_in_z()
        case ("interpolate_2d_eval_before_init")
            call scenario_interpolate_2d_eval_before_init()
        case ("interpolate_2d_one_shot_shape")
            call scenario_interpolate_2d_one_shot_shape()
        case ("interpolate_2d_query_sizes")
            call scenario_interpolate_2d_query_sizes()
        case ("interpolate_spline_too_wide")
            call scenario_interpolate_spline_too_wide()
        case ("interpolate_spline_overflows")
            call scenario_interpolate_spline_overflows()
        case ("interpolate_2d_spline_too_wide_x")
            call scenario_interpolate_2d_spline_too_wide_x()
        case ("interpolate_2d_spline_too_wide_y")
            call scenario_interpolate_2d_spline_too_wide_y()
        case ("interpolate_2d_spline_overflows")
            call scenario_interpolate_2d_spline_overflows()
        case ("interpolate_long_token")
            call scenario_interpolate_long_token()
        case ("interpolate_eval_array_before_init")
            call scenario_interpolate_eval_array_before_init()
        case ("optimize_size_zero")
            call scenario_optimize_size_zero()
        case ("optimize_budget_zero")
            call scenario_optimize_budget_zero()
        case ("optimize_budget_ceiling")
            call scenario_optimize_budget_ceiling()
        case ("optimize_tolerance_nonfinite")
            call scenario_optimize_tolerance_nonfinite()
        case ("optimize_scalar_bad_bracket")
            call scenario_optimize_scalar_bad_bracket()
        case ("optimize_scalar_bracket_width")
            call scenario_optimize_scalar_bracket_width()
        case ("optimize_scalar_nonfinite_value")
            call scenario_optimize_scalar_nonfinite_value()
        case ("optimize_scalar_constraints_not_honoured")
            call scenario_optimize_scalar_constraints_not_honoured()
        case ("optimize_simplex_step_zero")
            call scenario_optimize_simplex_step_zero()
        case ("optimize_simplex_step_size")
            call scenario_optimize_simplex_step_size()
        case ("optimize_simplex_no_tolerance")
            call scenario_optimize_simplex_no_tolerance()
        case ("optimize_simplex_nan_start")
            call scenario_optimize_simplex_nan_start()
        case ("optimize_simplex_nonfinite_value")
            call scenario_optimize_simplex_nonfinite_value()
        case ("optimize_simplex_constraints_not_honoured")
            call scenario_optimize_simplex_constraints_not_honoured()
        case ("optimize_threads_zero")
            call scenario_optimize_threads_zero()
        case ("optimize_de_bounds_size")
            call scenario_optimize_de_bounds_size()
        case ("optimize_de_bounds_order")
            call scenario_optimize_de_bounds_order()
        case ("optimize_de_bounds_nonfinite")
            call scenario_optimize_de_bounds_nonfinite()
        case ("optimize_de_box_width")
            call scenario_optimize_de_box_width()
        case ("optimize_de_np_small")
            call scenario_optimize_de_np_small()
        case ("optimize_de_f_weight_range")
            call scenario_optimize_de_f_weight_range()
        case ("optimize_de_cr_range")
            call scenario_optimize_de_cr_range()
        case ("optimize_de_max_gen_zero")
            call scenario_optimize_de_max_gen_zero()
        case ("optimize_de_constraints_not_honoured")
            call scenario_optimize_de_constraints_not_honoured()
        case ("optimize_multistart_nstart_zero")
            call scenario_optimize_multistart_nstart_zero()
        case ("optimize_multistart_merge_tol_negative")
            call scenario_optimize_multistart_merge_tol_negative()
        case ("optimize_simplex_solver_negative_budget")
            call scenario_optimize_simplex_solver_negative_budget()
        case ("prima_bobyqa_solver_negative_budget")
            call scenario_prima_bobyqa_solver_negative_budget()
        case ("optimize_multistart_constraints_not_honoured")
            call scenario_optimize_multistart_constraints_not_honoured()
        case ("optimize_multistart_nonfinite_threaded")
            call scenario_optimize_multistart_nonfinite_threaded()
        case ("prima_size_zero")
            call scenario_prima_size_zero()
        case ("prima_start_nan")
            call scenario_prima_start_nan()
        case ("prima_start_infinite")
            call scenario_prima_start_infinite()
        case ("prima_bounds_size")
            call scenario_prima_bounds_size()
        case ("prima_bound_nan")
            call scenario_prima_bound_nan()
        case ("prima_rhobeg_too_wide")
            call scenario_prima_rhobeg_too_wide()
        case ("prima_no_space_between_bounds")
            call scenario_prima_no_space_between_bounds()
        case ("prima_start_outside_bounds")
            call scenario_prima_start_outside_bounds()
        case ("prima_rho_order")
            call scenario_prima_rho_order()
        case ("prima_npt_range")
            call scenario_prima_npt_range()
        case ("prima_scale_size")
            call scenario_prima_scale_size()
        case ("prima_scale_nonpositive")
            call scenario_prima_scale_nonpositive()
        case ("prima_budget_zero")
            call scenario_prima_budget_zero()
        case ("prima_budget_ceiling")
            call scenario_prima_budget_ceiling()
        case ("prima_nonfinite_value")
            call scenario_prima_nonfinite_value()
        case ("prima_bobyqa_constraints_not_honoured")
            call scenario_prima_bobyqa_constraints_not_honoured()
        case ("prima_lincoa_constraints_not_honoured")
            call scenario_prima_lincoa_constraints_not_honoured()
        case ("prima_lincoa_shape")
            call scenario_prima_lincoa_shape()
        case ("prima_zero_constraint_row")
            call scenario_prima_zero_constraint_row()
        case ("prima_lincoa_infeasible_start")
            call scenario_prima_lincoa_infeasible_start()
        case ("prima_ctol_negative")
            call scenario_prima_ctol_negative()
        case ("prima_cobyla_negative_count")
            call scenario_prima_cobyla_negative_count()
        case ("prima_constraint_nonfinite")
            call scenario_prima_constraint_nonfinite()
        case ("optimize_de_no_tolerance")
            call scenario_optimize_de_no_tolerance()
        case ("optimize_context_is_reported")
            call scenario_optimize_context_is_reported()
        case ("optimize_context_is_capped")
            call scenario_optimize_context_is_capped()
        case ("prima_cobyla_nonfinite_value")
            call scenario_prima_cobyla_nonfinite_value()
        case ("prima_ctol_nonfinite")
            call scenario_prima_ctol_nonfinite()
        case ("prima_context_is_reported")
            call scenario_prima_context_is_reported()
        case ("prima_context_is_capped")
            call scenario_prima_context_is_capped()
        case ("prima_lincoa_infeasible_start_context")
            call scenario_prima_lincoa_infeasible_start_context()
        case ("root_reversed_bracket")
            call scenario_root_reversed_bracket()
        case ("root_nan_bracket_end")
            call scenario_root_nan_bracket_end()
        case ("root_bracket_width")
            call scenario_root_bracket_width()
        case ("root_negative_atol")
            call scenario_root_negative_atol()
        case ("root_negative_rtol")
            call scenario_root_negative_rtol()
        case ("root_bad_max_neval")
            call scenario_root_bad_max_neval()
        case ("root_bad_expansion_mode")
            call scenario_root_bad_expansion_mode()
        case ("root_bad_expansion_factor")
            call scenario_root_bad_expansion_factor()
        case ("root_bad_expansion_tries")
            call scenario_root_bad_expansion_tries()
        case ("root_limits_inside_the_bracket")
            call scenario_root_limits_inside_the_bracket()
        case ("root_nonfinite_expansion_limit")
            call scenario_root_nonfinite_expansion_limit()
        case ("root_function_returns_nan")
            call scenario_root_function_returns_nan()
        case ("root_context_reported")
            call scenario_root_context_reported()
        case ("root_context_capped")
            call scenario_root_context_capped()
        case ("transform_empty_sequence")
            call scenario_transform_empty_sequence()
        case ("transform_length_not_pow2")
            call scenario_transform_length_not_pow2()
        case ("transform_size_mismatch")
            call scenario_transform_size_mismatch()
        case ("transform_idct_size_mismatch")
            call scenario_transform_idct_size_mismatch()
        case ("transform_bad_norm_token")
            call scenario_transform_bad_norm_token()
        case ("transform_next_pow2_nonpositive")
            call scenario_transform_next_pow2_nonpositive()
        case ("transform_next_pow2_too_large")
            call scenario_transform_next_pow2_too_large()
        case ("transform_context_reported")
            call scenario_transform_context_reported()
        case ("transform_context_capped")
            call scenario_transform_context_capped()
        case ("transform_dst_length_not_pow2")
            call scenario_transform_dst_length_not_pow2()
        case ("transform_dst_size_mismatch")
            call scenario_transform_dst_size_mismatch()
        case ("transform_idst_length_not_pow2")
            call scenario_transform_idst_length_not_pow2()
        case ("transform_idst_bad_norm_token")
            call scenario_transform_idst_bad_norm_token()
        case ("kde_bandwidth_zero")
            call scenario_kde_bandwidth_zero()
        case ("kde_bandwidth_nan")
            call scenario_kde_bandwidth_nan()
        case ("kde_bandwidth_and_rule")
            call scenario_kde_bandwidth_and_rule()
        case ("kde_unknown_rule")
            call scenario_kde_unknown_rule()
        case ("kde_adjust_negative")
            call scenario_kde_adjust_negative()
        case ("kde_unknown_kernel")
            call scenario_kde_unknown_kernel()
        case ("kde_bound_infinite")
            call scenario_kde_bound_infinite()
        case ("kde_lower_not_below_upper")
            call scenario_kde_lower_not_below_upper()
        case ("kde_boundary_without_bound")
            call scenario_kde_boundary_without_bound()
        case ("kde_unknown_boundary")
            call scenario_kde_unknown_boundary()
        case ("kde_weights_size")
            call scenario_kde_weights_size()
        case ("kde_is_valid_size")
            call scenario_kde_is_valid_size()
        case ("kde_negative_weight")
            call scenario_kde_negative_weight()
        case ("kde_weight_type")
            call scenario_kde_weight_type()
        case ("kde_real32_weights_size")
            call scenario_kde_real32_weights_size()
        case ("kde_threads_zero")
            call scenario_kde_threads_zero()
        case ("kde_query_unfitted")
            call scenario_kde_query_unfitted()
        case ("kde_query_after_clear")
            call scenario_kde_query_after_clear()
        case ("kde_accessor_unfitted")
            call scenario_kde_accessor_unfitted()
        case ("kde_pdf_size")
            call scenario_kde_pdf_size()
        case ("kde_cdf_size")
            call scenario_kde_cdf_size()
        case ("kde_quantile_size")
            call scenario_kde_quantile_size()
        case ("kde_quantile_p_above_one")
            call scenario_kde_quantile_p_above_one()
        case ("kde_quantile_p_nan")
            call scenario_kde_quantile_p_nan()
        case ("kde_curve_size")
            call scenario_kde_curve_size()
        case ("kde_curve_reversed_range")
            call scenario_kde_curve_reversed_range()
        case ("kde_curve_negative_cut")
            call scenario_kde_curve_negative_cut()
        case ("kde_curve_nonfinite_end")
            call scenario_kde_curve_nonfinite_end()
        case ("kde_grid_ncells")
            call scenario_kde_grid_ncells()
        case ("kde_grid_range_nan")
            call scenario_kde_grid_range_nan()
        case ("kde_grid_range_reversed")
            call scenario_kde_grid_range_reversed()
        case ("kde_grid_cell_width")
            call scenario_kde_grid_cell_width()
        case ("kde_grid_bandwidth")
            call scenario_kde_grid_bandwidth()
        case ("kde_grid_bandwidth_subnormal")
            call scenario_kde_grid_bandwidth_subnormal()
        case ("kde_grid_bandwidth_unusable")
            call scenario_kde_grid_bandwidth_unusable()
        case ("kde_grid_query_unfinished")
            call scenario_kde_grid_query_unfinished()
        case ("kde_grid_method_token")
            call scenario_kde_grid_method_token()
        case ("kde_grid_binned_too_long")
            call scenario_kde_grid_binned_too_long()
        case ("kde_curve_binned_one_point")
            call scenario_kde_curve_binned_one_point()
        case ("kde_fit_method_token")
            call scenario_kde_fit_method_token()
        case ("kde_curve_method_on_binned")
            call scenario_kde_curve_method_on_binned()
        case ("kde_grid_add_after_finish")
            call scenario_kde_grid_add_after_finish()
        case ("kde_grid_merge_after_finish")
            call scenario_kde_grid_merge_after_finish()
        case ("kde_grid_pilot_unfinished")
            call scenario_kde_grid_pilot_unfinished()
        case ("kde_grid_unknown_kernel")
            call scenario_kde_grid_unknown_kernel()
        case ("kde_grid_boundary_without_bound")
            call scenario_kde_grid_boundary_without_bound()
        case ("kde_grid_outside_support")
            call scenario_kde_grid_outside_support()
        case ("kde_grid_linear_range_not_at_bound")
            call scenario_kde_grid_linear_range_not_at_bound()
        case ("kde_grid_linear_narrow")
            call scenario_kde_grid_linear_narrow()
        case ("kde_grid_add_uninitialised")
            call scenario_kde_grid_add_uninitialised()
        case ("kde_grid_threads_zero")
            call scenario_kde_grid_threads_zero()
        case ("kde_grid_weights_size")
            call scenario_kde_grid_weights_size()
        case ("kde_grid_real32_weights_size")
            call scenario_kde_grid_real32_weights_size()
        case ("kde_grid_negative_weight")
            call scenario_kde_grid_negative_weight()
        case ("kde_grid_density_size")
            call scenario_kde_grid_density_size()
        case ("kde_grid_density_x_size")
            call scenario_kde_grid_density_x_size()
        case ("kde_grid_centres_size")
            call scenario_kde_grid_centres_size()
        case ("kde_grid_pdf_size")
            call scenario_kde_grid_pdf_size()
        case ("kde_grid_cdf_size")
            call scenario_kde_grid_cdf_size()
        case ("kde_grid_quantile_size")
            call scenario_kde_grid_quantile_size()
        case ("kde_grid_quantile_p")
            call scenario_kde_grid_quantile_p()
        case ("kde_grid_query_uninitialised")
            call scenario_kde_grid_query_uninitialised()
        case ("kde_grid_accessor_uninitialised")
            call scenario_kde_grid_accessor_uninitialised()
        case ("kde_grid_merge_uninitialised")
            call scenario_kde_grid_merge_uninitialised()
        case ("kde_grid_merge_cells")
            call scenario_kde_grid_merge_cells()
        case ("kde_grid_merge_range")
            call scenario_kde_grid_merge_range()
        case ("kde_grid_merge_bandwidth")
            call scenario_kde_grid_merge_bandwidth()
        case ("kde_grid_merge_kernel")
            call scenario_kde_grid_merge_kernel()
        case ("kde_grid_merge_support")
            call scenario_kde_grid_merge_support()
        case ("kde_grid_merge_boundary")
            call scenario_kde_grid_merge_boundary()
        case ("kde_grid_merge_method")
            call scenario_kde_grid_merge_method()
        case ("kde_alpha_without_adaptive")
            call scenario_kde_alpha_without_adaptive()
        case ("kde_bandwidth_max_without_adaptive")
            call scenario_kde_bandwidth_max_without_adaptive()
        case ("kde_alpha_above_one")
            call scenario_kde_alpha_above_one()
        case ("kde_alpha_nan")
            call scenario_kde_alpha_nan()
        case ("kde_bandwidth_max_zero")
            call scenario_kde_bandwidth_max_zero()
        case ("kde_spread_max_without_adaptive")
            call scenario_kde_spread_max_without_adaptive()
        case ("kde_spread_max_below_one")
            call scenario_kde_spread_max_below_one()
        case ("kde_spread_cap_advice_default")
            call scenario_kde_spread_cap_advice_default()
        case ("kde_spread_cap_advice_explicit")
            call scenario_kde_spread_cap_advice_explicit()
        case ("kde_zone_advice_capped")
            call scenario_kde_zone_advice_capped()
        case ("kde_zone_advice_at_bound")
            call scenario_kde_zone_advice_at_bound()
        case ("kde_zone_advice_at_spread")
            call scenario_kde_zone_advice_at_spread()
        case ("kde_zone_advice_fixed")
            call scenario_kde_zone_advice_fixed()
        case ("kde_bandwidths_size")
            call scenario_kde_bandwidths_size()
        case ("kde_bandwidths_x_size")
            call scenario_kde_bandwidths_x_size()
        case ("kde_bandwidth_at_size")
            call scenario_kde_bandwidth_at_size()
        case ("kde_pilot_not_adaptive")
            call scenario_kde_pilot_not_adaptive()
        case ("kde_grid_alpha_without_pilot")
            call scenario_kde_grid_alpha_without_pilot()
        case ("kde_grid_bandwidth_max_without_pilot")
            call scenario_kde_grid_bandwidth_max_without_pilot()
        case ("kde_grid_alpha_negative")
            call scenario_kde_grid_alpha_negative()
        case ("kde_grid_bandwidth_max_nan")
            call scenario_kde_grid_bandwidth_max_nan()
        case ("kde_grid_pilot_uninitialised")
            call scenario_kde_grid_pilot_uninitialised()
        case ("kde_grid_merge_pilot")
            call scenario_kde_grid_merge_pilot()
        case ("kde_grid_merge_fixed")
            call scenario_kde_grid_merge_fixed()
        case ("kde_grid_merge_alpha")
            call scenario_kde_grid_merge_alpha()
        case ("kde_pdf_threads_zero")
            call scenario_kde_pdf_threads_zero()
        case ("kde_cdf_threads_zero")
            call scenario_kde_cdf_threads_zero()
        case ("kde_quantile_threads_zero")
            call scenario_kde_quantile_threads_zero()
        case ("kde_curve_threads_zero")
            call scenario_kde_curve_threads_zero()
        case ("kde_sample_threads_zero")
            call scenario_kde_sample_threads_zero()
        case ("kde_sample_unfitted")
            call scenario_kde_sample_unfitted()
        case ("kde_grid_sample_threads_zero")
            call scenario_kde_grid_sample_threads_zero()
        case ("kde_grid_sample_uninitialised")
            call scenario_kde_grid_sample_uninitialised()
        case ("kde_fit_logical_column")
            call scenario_kde_fit_logical_column()
        case ("kde_fit_column_is_valid")
            call scenario_kde_fit_column_is_valid()
        case ("kde_grid_add_vector_column")
            call scenario_kde_grid_add_vector_column()
        case ("kde_grid_add_column_is_valid")
            call scenario_kde_grid_add_column_is_valid()
        case ("kde_isj_cells_not_pow2")
            call scenario_kde_isj_cells_not_pow2()
        case ("kde_isj_cells_out_of_range")
            call scenario_kde_isj_cells_out_of_range()
        case ("kde_fit_pilot_without_adaptive")
            call scenario_kde_fit_pilot_without_adaptive()
        case ("kde_fit_pilot_unfinished")
            call scenario_kde_fit_pilot_unfinished()
        case ("kde_fit_pilot_kernel")
            call scenario_kde_fit_pilot_kernel()
        case ("kde_fit_pilot_boundary")
            call scenario_kde_fit_pilot_boundary()
        case ("kde_fit_pilot_support")
            call scenario_kde_fit_pilot_support()
        case ("kde_fit_pilot_support_side")
            call scenario_kde_fit_pilot_support_side()
        case ("kde_fit_pilot_upper")
            call scenario_kde_fit_pilot_upper()
        case ("kde_spread_max_not_finite")
            call scenario_kde_spread_max_not_finite()
        case ("kde_curve_one_end_collapses")
            call scenario_kde_curve_one_end_collapses()
        case ("kde_curve_default_range_collapses")
            call scenario_kde_curve_default_range_collapses()
        case ("kde_grid_cell_width_underflow")
            call scenario_kde_grid_cell_width_underflow()
        case ("kde_grid_linear_range_not_at_upper")
            call scenario_kde_grid_linear_range_not_at_upper()
        case ("kde_grid_spread_max_not_finite")
            call scenario_kde_grid_spread_max_not_finite()
        case ("kde_grid_spread_max_below_one")
            call scenario_kde_grid_spread_max_below_one()
        case ("kde_fit_cells_clamped_advice")
            call scenario_kde_fit_cells_clamped_advice()
        case ("kde_zone_advice_tiny_support")
            call scenario_kde_zone_advice_tiny_support()
        case ("kde_grid_binned_transform_length")
            call scenario_kde_grid_binned_transform_length()
        case ("kde_overreach_warning")
            call scenario_kde_overreach_warning("normal")
        case ("kde_overreach_warning_silent")
            call scenario_kde_overreach_warning("silent")
        case ("kde_overreach_warning_errors_only")
            call scenario_kde_overreach_warning("errors_only")
        case default
            handled = .false.
        end select
    end subroutine dispatch_error_scenarios_numeric

    ! ---- parquet_index: pf_index_map ----
    !
    !> A duplicate key is an error rather than a policy choice, on every backend, because the
    !> alternative is a map that silently answers for one of two rows and gives the caller no way
    !> to find out which. Each backend detects it by its own mechanism -- the direct arm by
    !> counting occupied slots after a scatter that may have been threaded, the hash arm on the
    !> insert that finds the key already there, the sorted arm on an adjacency scan after the
    !> sort -- so all three are exercised separately.
    subroutine scenario_index_build_duplicate_direct()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64, 3_int64, 2_int64], method="direct")
        print '(a)', "built a direct map with a duplicate key"
    end subroutine scenario_index_build_duplicate_direct
    !
    !> See `scenario_index_build_duplicate_direct`. The hash arm names the offender from its
    !> insert loop.
    subroutine scenario_index_build_duplicate_hash()
        type(pf_index_map) :: m

        call m%build([10_int64, 20_int64, 30_int64, 20_int64], method="hash")
        print '(a)', "built a hash map with a duplicate key"
    end subroutine scenario_index_build_duplicate_hash
    !
    !> See `scenario_index_build_duplicate_direct`. The sorted arm finds duplicates adjacent.
    subroutine scenario_index_build_duplicate_sorted()
        type(pf_index_map) :: m

        call m%build([5_int64, 9_int64, 5_int64], method="sorted")
        print '(a)', "built a sorted map with a duplicate key"
    end subroutine scenario_index_build_duplicate_sorted
    !
    !> A duplicate TUPLE, which is the composite form of the same rule: the individual components
    !> may repeat as much as they like, and only the whole tuple has to be unique.
    subroutine scenario_index_build_duplicate_tuple_direct()
        type(pf_index_map) :: m
        integer(int64) :: pairs(3, 2)

        pairs(:, 1) = [1_int64, 2_int64, 1_int64]
        pairs(:, 2) = [7_int64, 8_int64, 7_int64]
        call m%build(pairs, method="direct")
        print '(a)', "built a direct composite map with a duplicate tuple"
    end subroutine scenario_index_build_duplicate_tuple_direct
    !
    !> See `scenario_index_build_duplicate_tuple_direct`.
    subroutine scenario_index_build_duplicate_tuple_hash()
        type(pf_index_map) :: m
        integer(int64) :: pairs(3, 2)

        pairs(:, 1) = [1_int64, 2_int64, 1_int64]
        pairs(:, 2) = [7_int64, 8_int64, 7_int64]
        call m%build(pairs, method="hash")
        print '(a)', "built a hash composite map with a duplicate tuple"
    end subroutine scenario_index_build_duplicate_tuple_hash
    !
    !> A stored value below 1 is refused, because 0 is how every lookup reports "not found": a map
    !> that stored 0 would answer "absent" for a key that is present, which is precisely the silent
    !> wrong answer the whole values contract exists to make impossible.
    subroutine scenario_index_build_value_zero()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64], [1_int64, 0_int64])
        print '(a)', "built a map storing the value 0"
    end subroutine scenario_index_build_value_zero
    !
    !> `values=` must have exactly one element per key.
    subroutine scenario_index_build_values_length()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64, 3_int64], [1_int64, 2_int64])
        print '(a)', "built a map with too few values"
    end subroutine scenario_index_build_values_length
    !
    !> A bulk lookup whose answer array is the wrong length is refused rather than filling what
    !> fits: a short array would leave the caller with answers for some keys and stale memory for
    !> the rest, with nothing to say which.
    subroutine scenario_index_get_many_length()
        type(pf_index_map) :: m
        integer(int64) :: out(2)

        call m%build([1_int64, 2_int64, 3_int64])
        call m%get_many([1_int64, 2_int64, 3_int64], out)
        print '(a)', "a mismatched get_many was accepted"
    end subroutine scenario_index_get_many_length
    !
    !> A lookup presenting the wrong number of components is refused. The check is one integer
    !> comparison, which is why it is affordable on the hot path.
    !!
    !! **`got` is PRINTED, and that is what makes this scenario test anything.** `%get` is `pure`,
    !! so a compiler may delete a call whose result is never read -- gfortran does, from `-O1`
    !! upward, which took the abort away under `--profile release` while every unoptimised build
    !! still passed. Assigning the result is not enough; it has to be USED. See CLAUDE.md's
    !! "A scenario whose abort is inside a `pure` function must USE the result".
    subroutine scenario_index_tuple_width_mismatch()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2), got

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build(pairs)
        got = m%get([1_int64, 3_int64, 5_int64])
        print '(a,i0)', "a three-component lookup on a two-component map was accepted, got=", got
    end subroutine scenario_index_tuple_width_mismatch
    !
    !> A scalar key on a composite map is refused rather than matching on the first component.
    !!
    !! `got` is printed for the same reason as in `scenario_index_tuple_width_mismatch` above: `%get`
    !! is `pure`, so an unread result lets the optimiser delete the call and the abort with it.
    subroutine scenario_index_scalar_on_composite()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2), got

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build(pairs)
        got = m%get(1_int64)
        print '(a,i0)', "a scalar lookup on a composite map was accepted, got=", got
    end subroutine scenario_index_scalar_on_composite
    !
    !> A sorted map is frozen once built: keeping an exact-fit sorted array in order through an
    !> insertion is O(n) per key, and that exact fit is what buys the backend its footprint.
    subroutine scenario_index_sorted_set()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64, 3_int64], method="sorted")
        call m%set(4_int64, 4_int64)
        print '(a)', "a sorted map accepted a new key"
    end subroutine scenario_index_sorted_set
    !
    !> See `scenario_index_sorted_set`; removal is refused for the same reason.
    subroutine scenario_index_sorted_remove()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64, 3_int64], method="sorted")
        call m%remove(2_int64)
        print '(a)', "a sorted map accepted a removal"
    end subroutine scenario_index_sorted_remove
    !
    !> A sorted map answers `%get_or_add` for a key it holds (`test_sorted_get_or_add_present`,
    !> test/test_index.f90) and refuses only a NEW key, which would unfreeze it -- naming the entry
    !> the caller used, not the `%set` that would have stored the key.
    subroutine scenario_index_sorted_get_or_add_absent(many)
        logical, intent(in) :: many !! whether to ask through `%get_or_add_many`.
        type(pf_index_map) :: m
        integer(int64) :: idx, codes(2)

        call m%build([1_int64, 2_int64, 3_int64], method="sorted")
        if (many) then
            call m%get_or_add_many([2_int64, 4_int64], codes, threads=1)
            print '(a,i0)', "a sorted map added a new key through get_or_add_many, code=", codes(2)
        else
            call m%get_or_add(4_int64, idx)
            print '(a,i0)', "a sorted map added a new key through get_or_add, idx=", idx
        end if
    end subroutine scenario_index_sorted_get_or_add_absent
    !
    !> The sorted backend is single-component in v1 and says so, rather than silently indexing on
    !> the first component alone.
    subroutine scenario_index_sorted_composite()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2)

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build(pairs, method="sorted")
        print '(a)', "a composite sorted map was built"
    end subroutine scenario_index_sorted_composite
    !
    !> `%init` starts an incremental map, and only the hash backend can be one: the direct backend
    !> needs the whole key range up front to size its array.
    subroutine scenario_index_init_direct()
        type(pf_index_map) :: m

        call m%init(method="direct")
        print '(a)', "init accepted method=direct"
    end subroutine scenario_index_init_direct
    !
    !> See `scenario_index_init_direct`; sorted is refused because it is frozen once built.
    subroutine scenario_index_init_sorted()
        type(pf_index_map) :: m

        call m%init(method="sorted")
        print '(a)', "init accepted method=sorted"
    end subroutine scenario_index_init_sorted
    !
    !> An unknown `method=` token lists the accepted set rather than falling back to a default,
    !> which would give a caller who mistyped one the performance of a backend they did not choose.
    subroutine scenario_index_bad_method()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64], method="btree")
        print '(a)', "an unknown method token was accepted"
    end subroutine scenario_index_bad_method
    !
    !> A `%set` whose key falls outside a direct map's built range is refused, and the message says
    !> what to do about it. Silently migrating the map to the hash backend would change its memory
    !> behaviour behind the caller's back.
    subroutine scenario_index_direct_set_out_of_range()
        type(pf_index_map) :: m

        call m%build([10_int64, 11_int64, 12_int64], method="direct")
        call m%set(9999_int64, 4_int64)
        print '(a)', "a direct map accepted a key outside its range"
    end subroutine scenario_index_direct_set_out_of_range

    !> `%get_or_add`/`%get_or_add_many` on a direct map, with a key outside the built range.
    !!
    !! **The pair is what pins the refusal to the entry the CALLER used.** `many=.false.` goes
    !! through `ix_goa_scalar` and must name `%get_or_add`; `many=.true.` passes TUPLES, so it goes
    !! through `ix_goa_tuple` -- the other worker -- and must name `%get_or_add_many`. One scenario
    !! cannot show that, because a single message is consistent with the name being hard-coded.
    !!
    !! Until both workers refused the direct backend themselves, neither did: the refusal fell
    !! through to `ix_set_scalar`/`ix_set_tuple`, which name `%set` on every path, so all four
    !! combinations blamed a procedure the caller had not called.
    !! `index_direct_set_out_of_range` is the control that keeps `%set` naming `%set`.
    subroutine scenario_index_direct_get_or_add_out_of_range(many)
        logical, intent(in) :: many !! whether to ask through `%get_or_add_many` over tuples.
        type(pf_index_map) :: m
        integer(int64) :: idx, pairs(3, 2), probe(2, 2), codes(2)

        if (many) then
            pairs(:, 1) = [1_int64, 2_int64, 3_int64]
            pairs(:, 2) = [1_int64, 1_int64, 1_int64]
            call m%build(pairs, method="direct")
            probe(:, 1) = [1_int64, 9999_int64]
            probe(:, 2) = [1_int64, 1_int64]
            call m%get_or_add_many(probe, codes, threads=1)
            print '(a,i0)', "a direct map added an out-of-range key through get_or_add_many, code=", codes(2)
        else
            call m%build([10_int64, 11_int64, 12_int64], method="direct")
            call m%get_or_add(9999_int64, idx)
            print '(a,i0)', "a direct map added an out-of-range key through get_or_add, idx=", idx
        end if
    end subroutine scenario_index_direct_get_or_add_out_of_range
    !
    !> More components than the module's published maximum is refused at build, because the tuple
    !> paths widen into a fixed-size stack buffer of exactly that width.
    subroutine scenario_index_ncomp_too_large()
        type(pf_index_map) :: m
        integer(int64) :: wide(2, pf_index_max_components + 1)
        integer :: j

        do j = 1, pf_index_max_components + 1
            wide(1, j) = int(j, int64)
            wide(2, j) = int(j, int64) + 100_int64
        end do
        call m%build(wide)
        print '(a)', "a key wider than pf_index_max_components was accepted"
    end subroutine scenario_index_ncomp_too_large
    !
    !> An explicit `method="direct"` over a key range wider than the whole int64 domain is refused
    !> rather than wrapped. The automatic choice can never reach this -- its whole job is to keep
    !> the span inside a budget -- so only a caller who asked for direct by name gets here.
    subroutine scenario_index_direct_range_too_wide()
        type(pf_index_map) :: m

        call m%build([-huge(0_int64), huge(0_int64)], method="direct")
        print '(a)', "a direct map spanning the whole int64 domain was built"
    end subroutine scenario_index_direct_range_too_wide
    !
    !> `%remove` without `found=` treats an absent key as an error, on the same reasoning as every
    !> other mutating procedure in this library: a caller who did not ask to be told about absence
    !> is asserting the key is there.
    subroutine scenario_index_remove_absent()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64], method="hash")
        call m%remove(77_int64)
        print '(a)', "removing an absent key was accepted"
    end subroutine scenario_index_remove_absent
    !
    !> A stored value too large for an `int32` answer aborts rather than truncating silently.
    !>
    !> Reachable with a ONE-key map -- it needs a large stored *value*, not two billion keys -- so
    !> this costs nothing to test and is a real trap for a caller whose values index into something
    !> larger than an `int32` can address. `%get` is unaffected: every specific returns `int64`.
    !>
    !> The line before the abort is the boundary control: exactly `huge(int32)` must be accepted, so
    !> a guard that fired one value early would abort HERE and fail the stderr match rather than
    !> passing as if it had caught the right thing.
    subroutine scenario_index_get_many_int32_overflow()
        type(pf_index_map) :: m
        integer(int32) :: out32(1)

        call m%build([1_int64], [int(huge(0_int32), int64)])
        call m%get_many([1_int64], out32)   ! control: the largest value an int32 answer can hold
        print '(a,i0)', "int32 answer at the boundary was accepted, got=", out32(1)
        call m%build([1_int64], [int(huge(0_int32), int64) + 1_int64])
        call m%get_many([1_int64], out32)   ! -> aborts
        ! The result is printed so the trailing message is a diagnostic rather than a claim.
        ! (`%get_many` was `pure` until it threaded, and then this was also what kept the
        ! optimiser from deleting the call -- `.claude/rules/testing.md`, "A scenario whose abort is inside a
        ! `pure` function must USE the result"; the habit costs nothing to keep.)
        print '(a,i0)', "an oversized value narrowed into an int32 answer, got=", out32(1)
    end subroutine scenario_index_get_many_int32_overflow
    !
    !> `threads=0` is refused rather than silently treated as "automatic".
    !>
    !> Zero is the value a caller reaches by computing a team size that came out empty, and taking
    !> it as "use the default" would hide that arithmetic. The sibling bulk paths refuse it the same
    !> way -- see `scenario_spatial_count_all_threads_zero` and
    !> `scenario_healpix_ang2pix_bulk_threads_zero`.
    subroutine scenario_index_build_threads_zero()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64], threads=1)   ! control: the smallest team that is allowed
        print '(a)', "threads=1 was accepted"
        call m%build([1_int64, 2_int64], threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted"
    end subroutine scenario_index_build_threads_zero
    !
    !> A `valid=` mask of the wrong length on a bulk lookup is refused rather than read past its
    !> end or applied to a prefix of the keys. The control call first: a mask of the right length
    !> must be accepted, or a guard that fired on every mask would pass here.
    subroutine scenario_index_get_many_valid_length()
        type(pf_index_map) :: m
        integer(int64) :: out(3)

        call m%build([1_int64, 2_int64, 3_int64])
        call m%get_many([1_int64, 2_int64, 3_int64], out, valid=[.true., .false., .true.])
        print '(a,i0)', "a mask of the right length was accepted, out(2)=", out(2)
        call m%get_many([1_int64, 2_int64, 3_int64], out, valid=[.true., .false.])   ! -> aborts
        print '(a)', "a get_many mask of the wrong length was accepted"
    end subroutine scenario_index_get_many_valid_length
    !
    !> `threads=0` on a bulk lookup is refused for the reason `scenario_index_build_threads_zero`
    !> gives: the lookup resolves its team through the same rule as the build.
    subroutine scenario_index_get_many_threads_zero()
        type(pf_index_map) :: m
        integer(int64) :: out(2)

        call m%build([1_int64, 2_int64])
        call m%get_many([1_int64, 2_int64], out, threads=1)   ! control: the smallest team allowed
        print '(a,i0)', "threads=1 was accepted, out(1)=", out(1)
        call m%get_many([1_int64, 2_int64], out, threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted on get_many"
    end subroutine scenario_index_get_many_threads_zero
    !
    !> A `%get_or_add_many` whose code array is the wrong length is refused, as `%get_many`'s is:
    !> a short array would leave some keys added and uncoded, with nothing to say which.
    subroutine scenario_index_get_or_add_many_length()
        type(pf_index_map) :: m
        integer(int64) :: codes(2)

        call m%init()
        call m%get_or_add_many([1_int64, 2_int64, 3_int64], codes)
        print '(a)', "a mismatched get_or_add_many was accepted"
    end subroutine scenario_index_get_or_add_many_length
    !
    !> See `scenario_index_get_many_valid_length`; the bulk dictionary encoder checks its mask the
    !> same way, and the control call shows a matching mask is accepted.
    subroutine scenario_index_get_or_add_many_valid_length()
        type(pf_index_map) :: m
        integer(int64) :: codes(3)

        call m%init()
        call m%get_or_add_many([1_int64, 2_int64, 3_int64], codes, valid=[.true., .true., .false.])
        print '(a,i0)', "a mask of the right length was accepted, codes(3)=", codes(3)
        call m%get_or_add_many([1_int64, 2_int64, 3_int64], codes, valid=[.true.])   ! -> aborts
        print '(a)', "a get_or_add_many mask of the wrong length was accepted"
    end subroutine scenario_index_get_or_add_many_valid_length
    !
    !> A code too large for an `int32` answer aborts rather than truncating, on the bulk encoder
    !> as on `%get_many`. Reachable with a one-key map whose stored value is `huge(int32)`: the
    !> control reads that value back through an int32 code, and the abort is on the NEXT new key,
    !> whose code is one above it.
    subroutine scenario_index_get_or_add_many_int32_overflow()
        type(pf_index_map) :: m
        integer(int32) :: c32(1)

        call m%build([1_int64], [int(huge(0_int32), int64)], method="hash")
        call m%get_or_add_many([1_int64], c32)   ! control: the largest code an int32 can hold
        print '(a,i0)', "int32 code at the boundary was accepted, got=", c32(1)
        call m%get_or_add_many([2_int64], c32)   ! -> aborts
        print '(a,i0)', "an oversized code narrowed into an int32 answer, got=", c32(1)
    end subroutine scenario_index_get_or_add_many_int32_overflow
    !
    !> A `valid=` mask of the wrong length on a build is refused before anything is stored.
    subroutine scenario_index_build_valid_length()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64, 3_int64], valid=[.true., .false., .true.])   ! control
        print '(a,i0)', "a build mask of the right length was accepted, nkeys=", m%nkeys()
        call m%build([1_int64, 2_int64, 3_int64], valid=[.true., .false.])   ! -> aborts
        print '(a)', "a build mask of the wrong length was accepted"
    end subroutine scenario_index_build_valid_length
    !
    !> Asking a composite map for a rank-1 key list aborts rather than answering for one component.
    !>
    !> The generic dispatches on the RANK of the array the caller supplies, so this is a mistake a
    !> caller makes by declaring the wrong variable rather than by calling the wrong name -- which
    !> is why the message says which rank to ask for instead of merely refusing.
    subroutine scenario_index_keys_rank1_on_composite()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2)
        integer(int64), allocatable :: flat(:), rows(:,:)

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build(pairs, method="hash")
        call m%keys(rows)                   ! control: the rank the map actually has
        print '(a,i0)', "the rank-2 key list was returned, rows=", size(rows, 1)
        call m%keys(flat)                   ! -> aborts
        ! `%keys` is `pure` and reports only through its argument, so the result is used here for
        ! the same reason as in the int32 scenario above.
        print '(a,i0)', "a rank-1 key list was returned for a composite map, n=", size(flat)
    end subroutine scenario_index_keys_rank1_on_composite
    !
    !> A duplicate in a MASKED direct build is still named, at its own position in the caller's
    !> array. The threaded scatter detects a duplicate by counting occupied slots, and the serial
    !> re-pass that turns that count into a message has to skip the masked rows exactly as the
    !> scatter did -- a re-pass that counted them would name the wrong row, or a row that was never
    !> stored at all.
    subroutine scenario_index_masked_duplicate_direct()
        type(pf_index_map) :: m
        logical :: mask(5)

        ! Row 2 is masked, so 9 appears once among the unmasked rows; rows 4 and 5 both hold 3.
        mask = [.true., .false., .true., .true., .true.]
        call m%build([1_int64, 9_int64, 9_int64, 3_int64, 7_int64], method="direct", valid=mask)
        print '(a,i0)', "control: the masked build stored keys=", m%nkeys()
        mask = [.true., .false., .true., .true., .true.]
        call m%build([1_int64, 9_int64, 9_int64, 3_int64, 3_int64], method="direct", valid=mask)
        print '(a)', "built a masked direct map with a duplicate key"
    end subroutine scenario_index_masked_duplicate_direct
    !
    !> `scenario_index_masked_duplicate_direct` for key tuples, whose re-pass walks the mixed-radix
    !> offset instead of the scalar one.
    subroutine scenario_index_masked_duplicate_tuple_direct()
        type(pf_index_map) :: m
        integer(int64) :: pairs(5, 2)
        logical :: mask(5)

        pairs(:, 1) = [1_int64, 2_int64, 2_int64, 3_int64, 3_int64]
        pairs(:, 2) = [7_int64, 8_int64, 8_int64, 9_int64, 1_int64]
        mask = [.true., .false., .true., .true., .true.]
        call m%build(pairs, method="direct", valid=mask)   ! control: the repeat is masked out
        print '(a,i0)', "control: the masked composite build stored keys=", m%nkeys()
        pairs(5, 2) = 9_int64                              ! now rows 4 and 5 are the same tuple
        call m%build(pairs, method="direct", valid=mask)
        print '(a)', "built a masked direct composite map with a duplicate tuple"
    end subroutine scenario_index_masked_duplicate_tuple_direct
    !
    !> `%set` refuses the value 0 for the same reason `%build` refuses it: 0 is how every lookup in
    !> this module reports "not found", so storing it would make the key unfindable while `%nkeys`
    !> still counted it.
    subroutine scenario_index_set_value_zero()
        type(pf_index_map) :: m

        call m%init(method="hash")
        call m%set(7_int64, 1_int64)         ! control: the smallest legal value
        print '(a,i0)', "control: set stored value 1, nkeys=", m%nkeys()
        call m%set(8_int64, 0_int64)         ! -> aborts
        print '(a)', "a set storing the value 0 was accepted"
    end subroutine scenario_index_set_value_zero
    !
    !> A method token longer than the buffer the resolver compares in is refused before it is
    !> truncated into that buffer, where it could silently become a DIFFERENT token: a 17-character
    !> string beginning "hash" must not resolve to "hash".
    subroutine scenario_index_method_token_too_long()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64], method="hash")   ! control: the token it starts with
        print '(a,i0)', "control: the short token built a map, nkeys=", m%nkeys()
        call m%build([1_int64, 2_int64], method="hashhashhashhashhash")   ! -> aborts
        print '(a)', "an over-long method token was accepted"
    end subroutine scenario_index_method_token_too_long
    !
    !> A bulk integer lookup on a string-keyed map is refused. The scalar form is covered by
    !> `scenario_index_integer_on_string_map`; this is the bulk one, which reaches a different
    !> guard and would otherwise probe the string table's internal `(hash, occurrence)` keys.
    subroutine scenario_index_get_many_on_string_map()
        type(pf_index_map) :: m
        integer(int64) :: out(2)

        call m%build(["a", "b"])
        call m%get_many([1_int64, 2_int64], out)   ! -> aborts
        print '(a,i0)', "a bulk integer lookup on a string map was accepted, got=", out(1)
    end subroutine scenario_index_get_many_on_string_map
    !
    !> A bulk lookup presenting the wrong number of components is refused rather than comparing the
    !> components it was given and ignoring the rest.
    subroutine scenario_index_get_many_ncomp_mismatch()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2), triples(2, 3), out(2)

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        triples(:, 1) = [1_int64, 2_int64]
        triples(:, 2) = [3_int64, 4_int64]
        triples(:, 3) = [5_int64, 6_int64]
        call m%build(pairs, method="hash")
        call m%get_many(pairs, out)                ! control: the width the map has
        print '(a,i0)', "control: the two-component bulk lookup answered ", out(1)
        call m%get_many(triples, out)              ! -> aborts
        print '(a,i0)', "a three-component bulk lookup on a two-component map was accepted, got=", out(1)
    end subroutine scenario_index_get_many_ncomp_mismatch
    !
    !> A rank-2 integer key list asked of a string-keyed map is refused, as the rank-1 form is
    !> (`scenario_index_string_keys_rank1`): the caller is handed a `parquet_string_column` or
    !> nothing, never the internal integer keys the string table is built on.
    subroutine scenario_index_keys_rank2_on_string_map()
        type(pf_index_map) :: m
        integer(int64), allocatable :: rows(:,:)

        call m%build(["a", "b"])
        call m%keys(rows)                          ! -> aborts
        ! `%keys` is `pure` and reports only through its argument, so the result is used here or
        ! the optimiser deletes the call and the abort with it.
        print '(a,i0)', "a rank-2 integer key list from a string map was accepted, rows=", size(rows, 1)
    end subroutine scenario_index_keys_rank2_on_string_map
    !
    !> `method="direct"` over composite keys whose component spans MULTIPLY past the int64 domain is
    !> refused. Each span on its own is representable here; only their product is not, which is the
    !> case a per-component check alone would let through into an overflowing slot count.
    subroutine scenario_index_composite_direct_product_too_wide()
        type(pf_index_map) :: m
        integer(int64) :: wide(2, 2), near(2, 2)
        integer(int64), parameter :: BIG = 2_int64**40

        near(:, 1) = [1_int64, 2_int64]
        near(:, 2) = [1_int64, 2_int64]
        wide(:, 1) = [-BIG, BIG]
        wide(:, 2) = [-BIG, BIG]
        call m%build(near, method="direct")        ! control: a product that fits easily
        print '(a,i0)', "control: the narrow composite direct map holds ", m%nkeys()
        call m%build(wide, method="direct")        ! -> aborts
        print '(a)', "a composite direct map whose spans overflow was built"
    end subroutine scenario_index_composite_direct_product_too_wide
    !
    !> A direct build whose slot count is representable but far larger than any machine can allocate
    !> reports what it asked for and what to do instead, rather than dying in the runtime's own
    !> allocation failure. The automatic choice cannot reach this -- it keeps the span inside its
    !> budget -- so only an explicit `method="direct"` gets here.
    subroutine scenario_index_direct_alloc_refused()
        type(pf_index_map) :: m
        integer(int64), parameter :: HUGE_SPAN = 2_int64**50

        call m%build([1_int64, 4_int64], method="direct")   ! control: a span that allocates
        print '(a,i0)', "control: the small direct map holds ", m%nkeys()
        call m%build([1_int64, HUGE_SPAN], method="direct") ! -> aborts
        print '(a)', "a direct map of 2**50 slots was allocated"
    end subroutine scenario_index_direct_alloc_refused
    !
    !> The shared negative control for every map scenario above: the same calls, made correctly,
    !> must run to completion. Without it a guard that fired unconditionally would satisfy every
    !> one of the abort scenarios while breaking the library outright.
    subroutine scenario_index_control()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2), out(3)
        logical :: found

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build([1_int64, 2_int64, 3_int64], method="direct")
        call m%get_many([1_int64, 2_int64, 3_int64], out)
        call m%get_many([1_int64, 2_int64, 3_int64], out, valid=[.true., .false., .true.], threads=1)
        if (out(2) /= 0_int64 .or. out(3) /= 3_int64) error stop "control: masked get_many"
        call m%set(2_int64, 9_int64)
        call m%remove(2_int64)
        call m%remove(2_int64, found)
        call m%build([1_int64, 2_int64, 3_int64], [3_int64, 2_int64, 1_int64], method="sorted")
        call m%build(pairs, method="hash")
        if (m%get([1_int64, 3_int64]) /= 1_int64) error stop "control: composite lookup"
        call m%init(method="hash")
        call m%set(1_int64, 1_int64)
        call m%get_or_add_many([5_int64, 1_int64, 5_int64], out, valid=[.true., .true., .false.])
        if (out(1) /= 2_int64 .or. out(2) /= 1_int64 .or. out(3) /= 0_int64) &
            error stop "control: get_or_add_many"
        call m%build([1_int64, 2_int64, 2_int64], method="hash", valid=[.true., .true., .false.])
        if (m%nkeys() /= 2_int64) error stop "control: masked build"
        print '(a)', "index control finished"
    end subroutine scenario_index_control
    !
    !> Two threads each build a map with a duplicate key at the same moment, so that two aborts
    !> race. A `%build` runs outside the map's lock, and `ix_abort` -- the module's serialised
    !> reporter -- is what lets exactly one thread reach `error stop`, so the run exits 1 with the
    !> message intact rather than with two threads terminating at once (an undefined exit status
    !> under ifx). Without OpenMP the region is one thread and the first build aborts alone.
    subroutine scenario_index_concurrent_abort()
        type(pf_index_map) :: maps(2)
        integer :: t

        !$omp parallel do num_threads(2) default(shared) private(t)
        do t = 1, 2
            call maps(t)%build([10_int64, 20_int64, 30_int64, 20_int64], method="hash")
        end do
        print '(a)', "two concurrent builds with duplicate keys were accepted"
    end subroutine scenario_index_concurrent_abort
    !
    !> A duplicate key whose second copy is DEFERRED to the partitioned build's spill pass is
    !> still named at its position. The two keys are crafted through
    !> `parquet_debug_index_partition` to sit on the last slot of the one partition a three-key
    !> table has, so the second and third keys' walks meet the range boundary at once and both
    !> are deferred; the serial spill pass places the first copy and meets it again with the
    !> second. A control build without the repeat comes first and proves the crafted keys reached
    !> the spill pass at all -- it prints and exits 0 otherwise, which the test reads as a
    !> failure. On a one-processor machine, where no build partitions, the serial loop names the
    !> same duplicate, so the message holds there too.
    subroutine scenario_index_spill_duplicate()
        type(pf_index_map) :: m
        integer(int64) :: c, home, part, cap, a, k
        integer :: found

        found = 0
        a = 1_int64
        k = 2_int64
        part = 0_int64
        do c = 1_int64, 100000_int64
            call parquet_debug_index_partition(c, 3_int64, home, part, cap, threads=2)
            if (part == 0_int64) exit
            if (mod(home, part) /= part - 1_int64) cycle
            found = found + 1
            if (found == 1) a = c
            if (found == 2) then
                k = c
                exit
            end if
        end do
        if (part > 0_int64) then
            if (found < 2) then
                print '(a)', "crafting: no two keys on a partition's last slot among 100000 candidates"
                stop
            end if
            call m%build([a, k], method="hash", threads=2)
            if (parquet_debug_index_spills() < 1_int64) then
                print '(a)', "control: the crafted keys did not reach the spill pass"
                stop
            end if
            print '(a,i0,a,i0)', "control: keys deferred to the spill pass: ", parquet_debug_index_spills(), &
                "; table slots: ", cap
        else
            print '(a)', "one processor: the serial loop names the duplicate instead"
        end if
        call m%build([a, k, k], method="hash", threads=2)   ! -> aborts, naming k at position 3
        print '(a)', "a duplicate deferred to the spill pass was accepted"
    end subroutine scenario_index_spill_duplicate
    !
    !> A duplicate in a build large enough to partition on the team is named at its position:
    !> the partitioned pass detects it and the serial loop, re-run over the emptied table, names
    !> it exactly as a serial build would.
    subroutine scenario_index_partitioned_duplicate()
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:)
        integer(int64) :: i

        allocate(keys(20001))
        do i = 1_int64, 20000_int64
            keys(i) = i * 3_int64
        end do
        keys(20001) = 30_int64
        call m%build(keys(1:20000), method="hash", threads=64)   ! control: no duplicate
        print '(a,i0)', "control: 20000 keys built, nkeys=", m%nkeys()
        call m%build(keys, method="hash", threads=64)   ! -> aborts, naming 30 at position 20001
        print '(a)', "a duplicate in a partitioned build was accepted"
    end subroutine scenario_index_partitioned_duplicate
    !
    !> `scenario_index_partitioned_duplicate` for a ONE-column rank-2 key array, whose partitioned
    !> arm is the scalar insert reached through the composite build. The serial re-pass that names
    !> the offender is a separate call there from the one the rank-1 build makes.
    subroutine scenario_index_partitioned_duplicate_one_column()
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:,:)
        integer(int64) :: i

        allocate(keys(20001, 1))
        do i = 1_int64, 20000_int64
            keys(i, 1) = i * 3_int64
        end do
        keys(20001, 1) = 30_int64
        call m%build(keys(1:20000, :), method="hash", threads=64)   ! control: no duplicate
        print '(a,i0)', "control: 20000 one-column keys built, nkeys=", m%nkeys()
        call m%build(keys, method="hash", threads=64)   ! -> aborts, naming 30 at position 20001
        print '(a)', "a duplicate in a partitioned one-column build was accepted"
    end subroutine scenario_index_partitioned_duplicate_one_column
    !
    !> `scenario_index_partitioned_duplicate` for key tuples.
    subroutine scenario_index_partitioned_duplicate_tuple()
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:,:)
        integer(int64) :: i

        allocate(keys(20001, 2))
        do i = 1_int64, 20000_int64
            keys(i, 1) = 7_int64
            keys(i, 2) = i * 3_int64
        end do
        keys(20001, 1) = 7_int64
        keys(20001, 2) = 30_int64
        call m%build(keys(1:20000, :), method="hash", threads=64)   ! control: no duplicate
        print '(a,i0)', "control: 20000 tuples built, nkeys=", m%nkeys()
        call m%build(keys, method="hash", threads=64)   ! -> aborts, naming [7, 30] at position 20001
        print '(a)', "a duplicate tuple in a partitioned build was accepted"
    end subroutine scenario_index_partitioned_duplicate_tuple
    !
    !> A repeated string in a build large enough to partition is named at its position: the
    !> tuple pass reports equal tuples, the build falls back to the serial loop, and that loop
    !> names the duplicate as it always has.
    subroutine scenario_index_str_partitioned_duplicate()
        type(pf_index_map) :: m
        type(parquet_string_column) :: keys
        integer(int64) :: i
        character(len=16) :: txt

        do i = 1_int64, 20000_int64
            write (txt, "(a,i0)") "obj_", i
            call keys%append_string(trim(txt))
        end do
        call m%build(keys, threads=64)   ! control: no duplicate
        print '(a,i0)', "control: 20000 strings built, nkeys=", m%nkeys()
        call keys%append_string("obj_5")
        call m%build(keys, threads=64)   ! -> aborts, naming "obj_5" at position 20001
        print '(a)', "a duplicate string in a partitioned build was accepted"
    end subroutine scenario_index_str_partitioned_duplicate
    !
    !> `threads=0` on `%get_or_add_many` is refused for the reason `scenario_index_build_threads_zero`
    !> gives, and the message names the call rather than the build.
    subroutine scenario_index_get_or_add_many_threads_zero()
        type(pf_index_map) :: m
        integer(int64) :: codes(2)

        call m%init()
        call m%get_or_add_many([1_int64, 2_int64], codes, threads=1)   ! control
        print '(a,i0)', "threads=1 was accepted, codes(2)=", codes(2)
        call m%get_or_add_many([1_int64, 2_int64], codes, threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted on get_or_add_many"
    end subroutine scenario_index_get_or_add_many_threads_zero
    !
    !> Every legal shape of the partitioned passes completes: a threaded hash build of scalar
    !> keys, of tuples and of strings, a threaded `%get_or_add_many` of each, and the two debug
    !> hooks. A guard in those passes that fired unconditionally would satisfy the abort scenarios
    !> above while breaking every threaded build.
    subroutine scenario_index_partition_control()
        type(pf_index_map) :: m
        type(parquet_string_column) :: sc
        integer(int64), allocatable :: keys(:), pairs(:,:), codes(:)
        integer(int64) :: i, home, part, cap
        character(len=16) :: txt

        allocate(keys(30000), pairs(30000, 2), codes(30000))
        do i = 1_int64, 30000_int64
            keys(i) = i * 5_int64
            pairs(i, 1) = 3_int64
            pairs(i, 2) = i * 5_int64
            write (txt, "(a,i0)") "s", i
            call sc%append_string(trim(txt))
        end do
        call m%build(keys, method="hash", threads=64)
        if (m%get(150_int64) /= 30_int64) error stop "control: partitioned scalar build"
        call m%build(pairs, method="hash", threads=64)
        if (m%get([3_int64, 150_int64]) /= 30_int64) error stop "control: partitioned tuple build"
        call m%build(sc, threads=64)
        if (m%get("s30") /= 30_int64) error stop "control: partitioned string build"
        call m%init()
        call m%get_or_add_many(keys, codes, threads=64)
        if (m%nkeys() /= 30000_int64 .or. minval(codes) /= 1_int64 .or. maxval(codes) /= 30000_int64) &
            error stop "control: threaded get_or_add_many"
        call m%init(ncomp=2)
        call m%get_or_add_many(pairs, codes, threads=64)
        if (m%nkeys() /= 30000_int64) error stop "control: threaded composite get_or_add_many"
        call m%init(strings=.true.)
        call m%get_or_add_many(sc, codes, threads=64)
        if (m%nkeys() /= 30000_int64 .or. m%get("s7") /= codes(7)) error stop "control: threaded string get_or_add_many"
        call parquet_debug_index_partition(150_int64, 30000_int64, home, part, cap, threads=64)
        if (cap < 30000_int64 .or. home < 0_int64 .or. home >= cap) error stop "control: partition hook"
        print '(a,i0,a,i0)', "index partition control finished; spills=", parquet_debug_index_spills(), " part=", part
    end subroutine scenario_index_partition_control

    ! ---- parquet_index: pf_index_multimap ----
    !
    !> A `values=` array of the wrong length is refused, as the map's is, and the message names
    !> the multimap: its build shares the map's guard and hands it its own prefix.
    subroutine scenario_multimap_build_values_length()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 2_int64], [1_int64, 2_int64, 3_int64])   ! control
        print '(a,i0)', "a values array of the right length was accepted, ngroups=", mm%ngroups()
        call mm%build([1_int64, 2_int64, 2_int64], [1_int64, 2_int64])           ! -> aborts
        print '(a)', "a multimap values array of the wrong length was accepted"
    end subroutine scenario_multimap_build_values_length
    !
    !> A stored value of 0 is refused: 0 is how every lookup on the multimap reports "not found".
    subroutine scenario_multimap_build_value_zero()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 2_int64], [1_int64, 0_int64, 2_int64])
        print '(a)', "a multimap value of 0 was accepted"
    end subroutine scenario_multimap_build_value_zero
    !
    !> A `valid=` mask of the wrong length is refused before any row is read.
    subroutine scenario_multimap_build_valid_length()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 2_int64], valid=[.true., .false., .true.])   ! control
        print '(a,i0)', "a mask of the right length was accepted, nkeys=", mm%nkeys()
        call mm%build([1_int64, 2_int64, 2_int64], valid=[.true., .false.])           ! -> aborts
        print '(a)', "a multimap build mask of the wrong length was accepted"
    end subroutine scenario_multimap_build_valid_length
    !
    !> `threads=0` is refused rather than read as "automatic", as on the map.
    subroutine scenario_multimap_build_threads_zero()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 2_int64], threads=1)   ! control
        print '(a,i0)', "threads=1 was accepted, ngroups=", mm%ngroups()
        call mm%build([1_int64, 2_int64, 2_int64], threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted on a multimap build"
    end subroutine scenario_multimap_build_threads_zero
    !
    !> An unknown backend token is refused, naming the multimap rather than the map whose
    !> resolver it shares.
    subroutine scenario_multimap_build_bad_method()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 2_int64], method="btree")
        print '(a)', "an unknown multimap method was accepted"
    end subroutine scenario_multimap_build_bad_method
    !
    !> The sorted backend takes single-component keys only, on the multimap as on the map.
    subroutine scenario_multimap_build_sorted_composite()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2)

        pairs(:, 1) = [1_int64, 2_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        call mm%build(pairs, method="sorted")
        print '(a)', "a composite sorted multimap build was accepted"
    end subroutine scenario_multimap_build_sorted_composite
    !
    !> A `%get_first_many` answer array of the wrong length is refused.
    subroutine scenario_multimap_get_first_many_length()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(2)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%get_first_many([1_int64, 2_int64, 3_int64], out)
        print '(a)', "a get_first_many length mismatch was accepted"
    end subroutine scenario_multimap_get_first_many_length
    !
    !> A `%get_first_many` mask of the wrong length is refused.
    subroutine scenario_multimap_get_first_many_valid_length()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(3)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%get_first_many([1_int64, 2_int64, 3_int64], out, valid=[.true., .false., .true.])
        print '(a,i0)', "a mask of the right length was accepted, out(1)=", out(1)
        call mm%get_first_many([1_int64, 2_int64, 3_int64], out, valid=[.true., .false.])   ! -> aborts
        print '(a)', "a get_first_many mask of the wrong length was accepted"
    end subroutine scenario_multimap_get_first_many_valid_length
    !
    !> `threads=0` on `%get_first_many` is refused.
    subroutine scenario_multimap_get_first_many_threads_zero()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(3)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%get_first_many([1_int64, 2_int64, 3_int64], out, threads=1)   ! control
        print '(a,i0)', "threads=1 was accepted, out(2)=", out(2)
        call mm%get_first_many([1_int64, 2_int64, 3_int64], out, threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted on get_first_many"
    end subroutine scenario_multimap_get_first_many_threads_zero
    !
    !> A `%get_many` answer array of the wrong length is refused.
    subroutine scenario_multimap_get_many_length()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(2)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%get_many([1_int64, 2_int64, 3_int64], out)
        print '(a)', "a multimap get_many length mismatch was accepted"
    end subroutine scenario_multimap_get_many_length
    !
    !> A `%probe_many` mask of the wrong length is refused.
    subroutine scenario_multimap_probe_many_valid_length()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: off(:), m(:)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%probe_many([1_int64, 2_int64, 3_int64], off, m, valid=[.true., .false., .true.])
        print '(a,i0)', "a mask of the right length was accepted, pairs=", size(m)
        call mm%probe_many([1_int64, 2_int64, 3_int64], off, m, valid=[.true., .false.])   ! -> aborts
        print '(a)', "a probe_many mask of the wrong length was accepted"
    end subroutine scenario_multimap_probe_many_valid_length
    !
    !> `threads=0` on `%probe_many` is refused.
    subroutine scenario_multimap_probe_many_threads_zero()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: off(:), m(:)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%probe_many([1_int64, 2_int64, 3_int64], off, m, threads=1)   ! control
        print '(a,i0)', "threads=1 was accepted, pairs=", size(m)
        call mm%probe_many([1_int64, 2_int64, 3_int64], off, m, threads=0)   ! -> aborts
        print '(a)', "threads=0 was accepted on probe_many"
    end subroutine scenario_multimap_probe_many_threads_zero
    !
    !> A pair count the answer could not hold is refused rather than wrapped. The real ceiling
    !> is the int64 domain, which no scenario can fill, so the test-only hook lowers it: two
    !> groups of five probed five times is 25 pairs against a ceiling of 20.
    subroutine scenario_multimap_probe_many_pair_overflow()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: off(:), m(:)

        call mm%build([1_int64, 1_int64, 1_int64, 1_int64, 1_int64, 2_int64, 2_int64, 2_int64, 2_int64, 2_int64])
        call parquet_debug_set_index_pair_limit(20_int64)
        call mm%probe_many([1_int64, 2_int64], off, m)               ! control: 10 pairs fit
        print '(a,i0)', "ten pairs under a ceiling of twenty were accepted, pairs=", size(m)
        call mm%probe_many([1_int64, 2_int64, 1_int64, 2_int64, 1_int64], off, m)   ! -> aborts
        print '(a,i0)', "a pair count over the ceiling was accepted, pairs=", size(m)
    end subroutine scenario_multimap_probe_many_pair_overflow
    !
    !> A stored value too large for an `int32` answer aborts the bulk form up front, rather than
    !> truncating or failing half-way through the array.
    subroutine scenario_multimap_int32_answer_overflow()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(2)
        integer(int32) :: out32(2)

        call mm%build([1_int64, 2_int64, 2_int64], [int(huge(0_int32), int64) + 1_int64, 5_int64, 6_int64])
        call mm%get_first_many([1_int64, 2_int64], out)     ! control: the int64 form answers
        print '(a,i0)', "the int64 answer form accepted the value, out(1)=", out(1)
        call mm%get_first_many([1_int64, 2_int64], out32)   ! -> aborts
        print '(a,i0)', "a value too large for int32 was accepted, out32(1)=", out32(1)
    end subroutine scenario_multimap_int32_answer_overflow
    !
    !> The same refusal on the scalar `%get_all`, whose check is inline in a pure body.
    subroutine scenario_multimap_get_all_int32_overflow()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: rows(:)
        integer(int32), allocatable :: rows32(:)

        call mm%build([1_int64, 2_int64, 2_int64], [int(huge(0_int32), int64) + 1_int64, 5_int64, 6_int64])
        call mm%get_all(1_int64, rows)     ! control: the int64 form answers
        print '(a,i0)', "the int64 get_all accepted the value, size=", size(rows)
        call mm%get_all(1_int64, rows32)   ! -> aborts
        print '(a,i0)', "a value too large for int32 was accepted by get_all, size=", size(rows32)
    end subroutine scenario_multimap_get_all_int32_overflow
    !
    !> A tuple of the wrong width is refused by every scalar lookup. The result is printed
    !> because `%count` is `pure` and a discarded pure result may be deleted with its abort.
    subroutine scenario_multimap_tuple_width_mismatch()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2), n

        pairs(:, 1) = [1_int64, 2_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        call mm%build(pairs)
        n = mm%count([2_int64, 1_int64])                  ! control: the right width
        print '(a,i0)', "a two-component lookup was accepted, count=", n
        n = mm%count([1_int64, 2_int64, 3_int64])         ! -> aborts
        print '(a,i0)', "a three-component lookup on a pair multimap was accepted, count=", n
    end subroutine scenario_multimap_tuple_width_mismatch
    !
    !> A scalar key presented to a composite multimap is refused. The result is printed for
    !> the reason `scenario_multimap_tuple_width_mismatch` gives.
    subroutine scenario_multimap_scalar_on_composite()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2), g

        pairs(:, 1) = [1_int64, 2_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        call mm%build(pairs)
        g = mm%get(1_int64)
        print '(a,i0)', "a scalar lookup on a composite multimap was accepted, group=", g
    end subroutine scenario_multimap_scalar_on_composite
    !
    !> A rank-1 key list asked of a composite multimap is refused, naming the rank to ask for.
    subroutine scenario_multimap_keys_rank1_on_composite()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2)
        integer(int64), allocatable :: list(:)

        pairs(:, 1) = [1_int64, 2_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        call mm%build(pairs)
        call mm%keys(list)
        print '(a,i0)', "a rank-1 key list of a composite multimap was accepted, size=", size(list)
    end subroutine scenario_multimap_keys_rank1_on_composite
    !
    !> Probing a pair multimap with triples is refused before any probe is looked up.
    subroutine scenario_multimap_probe_shape_mismatch()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2), triples(2, 3)
        integer(int64), allocatable :: off(:), m(:)

        pairs(:, 1) = [1_int64, 2_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        triples = 1_int64
        call mm%build(pairs)
        call mm%probe_many(triples, off, m)
        print '(a,i0)', "a probe of the wrong width was accepted, pairs=", size(m)
    end subroutine scenario_multimap_probe_shape_mismatch
    !
    !> A key TUPLE on a string-keyed multimap is refused, as a scalar key is. The tuple form is the
    !> dangerous one: a string multimap's keys are `(hash, occurrence)` pairs underneath, so a
    !> two-component probe matches the internal width exactly and would otherwise be looked up.
    subroutine scenario_multimap_tuple_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int64) :: g

        call mm%build(["a", "b", "a"])
        g = mm%get([1_int64, 0_int64])              ! -> aborts
        ! `%get` is `pure`, so the result is used here or the call is deleted with its abort.
        print '(a,i0)', "a key tuple on a string multimap was accepted, got=", g
    end subroutine scenario_multimap_tuple_on_string_map
    !
    !> A scalar integer key on a string-keyed multimap is refused, as it is on a string-keyed map.
    subroutine scenario_multimap_integer_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int64) :: g

        call mm%build(["a", "b", "a"])
        g = mm%get(1_int64)                         ! -> aborts
        ! `%get` is `pure`, so the result is used here or the call is deleted with its abort.
        print '(a,i0)', "an integer key on a string multimap was accepted, got=", g
    end subroutine scenario_multimap_integer_on_string_map
    !
    !> And the bulk form of it, which reaches a different guard from the scalar one above.
    subroutine scenario_multimap_get_first_many_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int64) :: out(2)

        call mm%build(["a", "b", "a"])
        call mm%get_first_many([1_int64, 2_int64], out)   ! -> aborts
        print '(a,i0)', "a bulk integer lookup on a string multimap was accepted, got=", out(1)
    end subroutine scenario_multimap_get_first_many_on_string_map
    !
    !> A bulk multimap lookup presenting the wrong number of components is refused rather than
    !> comparing the components it was given and ignoring the rest.
    subroutine scenario_multimap_get_first_many_ncomp_mismatch()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2), triples(3, 3), out(3)

        pairs(:, 1) = [1_int64, 2_int64, 1_int64]
        pairs(:, 2) = [3_int64, 4_int64, 3_int64]
        triples(:, 1) = pairs(:, 1)
        triples(:, 2) = pairs(:, 2)
        triples(:, 3) = [5_int64, 6_int64, 5_int64]
        call mm%build(pairs, method="hash")
        call mm%get_first_many(pairs, out)          ! control: the width the multimap has
        print '(a,i0)', "control: the two-component bulk lookup answered ", out(1)
        call mm%get_first_many(triples, out)        ! -> aborts
        print '(a,i0)', "a three-component bulk multimap lookup was accepted, got=", out(1)
    end subroutine scenario_multimap_get_first_many_ncomp_mismatch
    !
    !> `%probe_many` with integer keys on a string-keyed multimap is refused; its shape check is a
    !> separate guard from the one the m:1 bulk forms take.
    subroutine scenario_multimap_probe_many_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: offsets(:), matches(:)

        call mm%build(["a", "b", "a"])
        call mm%probe_many([1_int64, 2_int64], offsets, matches)   ! -> aborts
        print '(a,i0)', "an integer probe of a string multimap was accepted, n=", size(offsets)
    end subroutine scenario_multimap_probe_many_on_string_map
    !
    !> `method="direct"` over a multimap key range spanning the whole int64 domain is refused, as it
    !> is for `pf_index_map` (`scenario_index_direct_range_too_wide`).
    subroutine scenario_multimap_direct_range_too_wide()
        type(pf_index_multimap) :: mm

        call mm%build([1_int64, 2_int64, 1_int64], method="direct")   ! control
        print '(a,i0)', "control: the narrow direct multimap has groups ", mm%ngroups()
        call mm%build([-huge(0_int64), huge(0_int64)], method="direct")   ! -> aborts
        print '(a)', "a direct multimap spanning the whole int64 domain was built"
    end subroutine scenario_multimap_direct_range_too_wide
    !
    !> And the composite form, whose component spans multiply past the int64 domain.
    subroutine scenario_multimap_composite_direct_product_too_wide()
        type(pf_index_multimap) :: mm
        integer(int64) :: wide(2, 2), near(2, 2)
        integer(int64), parameter :: BIG = 2_int64**40

        near(:, 1) = [1_int64, 2_int64]
        near(:, 2) = [1_int64, 2_int64]
        wide(:, 1) = [-BIG, BIG]
        wide(:, 2) = [-BIG, BIG]
        call mm%build(near, method="direct")        ! control: a product that fits easily
        print '(a,i0)', "control: the narrow composite direct multimap has groups ", mm%ngroups()
        call mm%build(wide, method="direct")        ! -> aborts
        print '(a)', "a composite direct multimap whose spans overflow was built"
    end subroutine scenario_multimap_composite_direct_product_too_wide
    !
    !> A direct multimap build whose slot count is representable but unallocatable reports what it
    !> asked for and what to do instead. `scenario_index_direct_alloc_refused` is the map's twin;
    !> the multimap allocates its grouping slots through a separate path.
    subroutine scenario_multimap_direct_alloc_refused()
        type(pf_index_multimap) :: mm
        integer(int64), parameter :: HUGE_SPAN = 2_int64**50

        call mm%build([1_int64, 4_int64, 1_int64], method="direct")   ! control
        print '(a,i0)', "control: the small direct multimap has groups ", mm%ngroups()
        call mm%build([1_int64, HUGE_SPAN], method="direct")          ! -> aborts
        print '(a)', "a direct multimap of 2**50 slots was allocated"
    end subroutine scenario_multimap_direct_alloc_refused
    !
    !> The negative control for every multimap guard above: each legal shape of every call
    !> completes, so a guard that fired on a legal call would fail here.
    subroutine scenario_multimap_control()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(4, 2), out(3), lo, hi, nm
        integer(int32) :: out32(3)
        integer(int64), allocatable :: rows(:), off(:), m(:), list(:), tuples(:,:)
        logical, allocatable :: hit(:)
        character(len=:), allocatable :: tok

        call mm%build([5_int64, 7_int64, 5_int64, 9_int64], method="direct")
        if (mm%count(5_int64) /= 2_int64 .or. mm%get_first(5_int64) /= 1_int64) &
            error stop "control: direct count/get_first"
        call mm%get_all(5_int64, rows)
        if (size(rows) /= 2 .or. rows(2) /= 3_int64) error stop "control: get_all"
        call mm%get_range(7_int64, lo, hi)
        if (hi - lo /= 0_int64) error stop "control: get_range"
        call mm%get_first_many([5_int64, 6_int64, 9_int64], out, valid=[.true., .true., .false.], threads=1)
        if (out(1) /= 1_int64 .or. out(2) /= 0_int64 .or. out(3) /= 0_int64) &
            error stop "control: masked get_first_many"
        call mm%get_first_many([5_int64, 6_int64, 9_int64], out32)
        if (out32(3) /= 4_int32) error stop "control: int32 get_first_many"
        call mm%get_many([5_int64, 6_int64, 9_int64], out, threads=1)
        if (out(1) /= 1_int64 .or. out(2) /= 0_int64) error stop "control: get_many"
        call mm%probe_many([5_int64, 6_int64, 9_int64], off, m, threads=1, n_matched=nm, group_hit=hit)
        if (size(m) /= 3 .or. nm /= 2_int64 .or. .not. hit(1) .or. hit(2)) error stop "control: probe_many"
        call mm%csr(off, rows)
        if (size(off) /= 4 .or. size(rows) /= 4) error stop "control: csr"
        call mm%keys(list)
        if (size(list) /= 3) error stop "control: keys"
        call mm%build([5_int64, 7_int64, 5_int64, 9_int64], [4_int64, 3_int64, 2_int64, 1_int64], method="hash", &
            valid=[.true., .true., .true., .false.])
        if (mm%nkeys() /= 3_int64 .or. mm%get_first(5_int64) /= 4_int64) error stop "control: masked hash build"
        call mm%build([5_int64, 7_int64, 5_int64, 9_int64], method="sorted", threads=1)
        call mm%get_method(tok)
        if (tok /= "sorted" .or. mm%max_multiplicity() /= 2_int64) error stop "control: sorted build"
        pairs(:, 1) = [1_int64, 2_int64, 1_int64, 2_int64]
        pairs(:, 2) = [1_int64, 1_int64, 1_int64, 2_int64]
        call mm%build(pairs, method="hash")
        if (mm%count([1_int64, 1_int64]) /= 2_int64) error stop "control: composite count"
        call mm%keys(tuples)
        if (size(tuples, 1) /= 3) error stop "control: composite keys"
        call mm%probe_many(pairs, off, m)
        if (size(m) /= 6) error stop "control: composite probe_many"
        call parquet_debug_set_index_pair_limit(5_int64)
        call parquet_debug_set_index_pair_limit(0_int64)
        call mm%probe_many(pairs, off, m)
        if (size(m) /= 6) error stop "control: probe_many after the limit was reset"
        call mm%clear()
        if (mm%ngroups() /= 0_int64 .or. mm%get([1_int64, 1_int64]) /= 0_int64) error stop "control: clear"
        print '(a)', "multimap control finished"
    end subroutine scenario_multimap_control

    ! ---- parquet_index: string keys (both types) ----
    !
    !> A string key presented to a map holding integer keys is refused by name, before the key
    !> could be hashed against a table whose slots hold integers: the two are different maps
    !> underneath, and a silent 0 would read as "absent".
    subroutine scenario_index_string_on_integer_map()
        type(pf_index_map) :: m
        integer(int64) :: v

        call m%build([1_int64, 2_int64, 3_int64])
        v = m%get("a")
        print '(a,i0)', "a string key on an integer map was accepted, got=", v
    end subroutine scenario_index_string_on_integer_map
    !
    !> The reverse: an integer key on a string map. A string map IS a two-component tuple map
    !> underneath, so this guard runs before the composite-key one, or the message would send
    !> the caller off to build a tuple.
    subroutine scenario_index_integer_on_string_map()
        type(pf_index_map) :: m
        integer(int64) :: v

        call m%build(["a", "b"])
        v = m%get(1_int64)
        print '(a,i0)', "an integer key on a string map was accepted, got=", v
    end subroutine scenario_index_integer_on_string_map
    !
    !> And a tuple, which would otherwise match the internal `(hash, occurrence)` width exactly
    !> and probe the table as if it were an ordinary composite map.
    subroutine scenario_index_tuple_on_string_map()
        type(pf_index_map) :: m
        integer(int64) :: v

        call m%build(["a", "b"])
        v = m%get([1_int64, 0_int64])
        print '(a,i0)', "a key tuple on a string map was accepted, got=", v
    end subroutine scenario_index_tuple_on_string_map
    !
    !> The bulk form's guard, which is a separate check from the scalar one.
    subroutine scenario_index_string_get_many_on_integer_map()
        type(pf_index_map) :: m
        integer(int64) :: out(2)

        call m%build([1_int64, 2_int64])
        call m%get_many(["a", "b"], out)
        print '(a,i0)', "string keys in get_many on an integer map were accepted, got=", out(1)
    end subroutine scenario_index_string_get_many_on_integer_map
    !
    !> A string map is the hash table with the strings beside it; the direct and sorted
    !> backends have no meaning for a key that is hashed before it is stored, and are refused
    !> by name rather than silently ignored.
    subroutine scenario_index_string_method_direct()
        type(pf_index_map) :: m

        call m%build(["a", "b"], method="direct")
        print '(a)', "method=direct on a string build was accepted"
    end subroutine scenario_index_string_method_direct
    !
    !> See `scenario_index_string_method_direct`.
    subroutine scenario_index_string_method_sorted()
        type(pf_index_map) :: m

        call m%build(["a", "b"], method="sorted")
        print '(a)', "method=sorted on a string build was accepted"
    end subroutine scenario_index_string_method_sorted
    !
    !> A repeated string key under `%build` is refused, naming the key TRIMMED (the array's
    !> elements are trimmed on the way in, so "aa " and "aa" are one key) and its position.
    subroutine scenario_index_string_build_duplicate()
        type(pf_index_map) :: m

        call m%build(["aa ", "bb ", "aa "])
        print '(a)', "a duplicate string key was accepted"
    end subroutine scenario_index_string_build_duplicate
    !
    !> `%keys` into an integer list on a string map is refused naming the form to ask for: the
    !> internal tuples would otherwise come back as if they were the caller's keys.
    subroutine scenario_index_string_keys_rank1()
        type(pf_index_map) :: m
        integer(int64), allocatable :: list(:)

        call m%build(["a", "b"])
        call m%keys(list)
        print '(a,i0)', "an integer key list from a string map was accepted, n=", size(list)
    end subroutine scenario_index_string_keys_rank1
    !
    !> And the reverse, a string column asked of an integer map.
    subroutine scenario_index_string_keys_column_on_integer()
        type(pf_index_map) :: m
        type(parquet_string_column) :: list

        call m%build([1_int64, 2_int64])
        call m%keys(list)
        print '(a,i0)', "a string key column from an integer map was accepted, n=", list%size()
    end subroutine scenario_index_string_keys_column_on_integer
    !
    !> A string key has exactly one component from the caller's side, whatever the tuple
    !> underneath is, so `ncomp=` and `strings=.true.` cannot both be meant.
    subroutine scenario_index_string_init_ncomp()
        type(pf_index_map) :: m

        call m%init(strings=.true., ncomp=2)
        print '(a)', "init(strings=.true., ncomp=2) was accepted"
    end subroutine scenario_index_string_init_ncomp
    !
    !> A string `%set` on a map holding integer keys is refused; on a FRESH map it would have
    !> started a string map, which is the control's business.
    subroutine scenario_index_string_set_on_integer_map()
        type(pf_index_map) :: m

        call m%build([1_int64, 2_int64])
        call m%set("a", 3_int64)
        print '(a)', "a string set on an integer map was accepted"
    end subroutine scenario_index_string_set_on_integer_map
    !
    !> Removing an absent string key without `found=` aborts, as the integer form does.
    subroutine scenario_index_string_remove_absent()
        type(pf_index_map) :: m

        call m%build(["a", "b"])
        call m%remove("zz")
        print '(a)', "removing an absent string key was accepted"
    end subroutine scenario_index_string_remove_absent
    !
    !> The string bulk lookup checks its answer array's length, as the integer one does.
    subroutine scenario_index_string_get_many_length()
        type(pf_index_map) :: m
        integer(int64) :: out(1)

        call m%build(["a", "b"])
        call m%get_many(["a", "b"], out)
        print '(a,i0)', "a string get_many with a short answer array was accepted, got=", out(1)
    end subroutine scenario_index_string_get_many_length
    !
    !> The multimap's guards name the multimap, as its integer guards do.
    subroutine scenario_multimap_string_on_integer()
        type(pf_index_multimap) :: mm
        integer(int64) :: n

        call mm%build([1_int64, 2_int64, 2_int64])
        n = mm%count("a")
        print '(a,i0)', "a string key on an integer multimap was accepted, got=", n
    end subroutine scenario_multimap_string_on_integer
    !
    !> See `scenario_multimap_string_on_integer`.
    subroutine scenario_multimap_integer_on_string()
        type(pf_index_multimap) :: mm
        integer(int64) :: n

        call mm%build(["a", "b", "a"])
        n = mm%count(1_int64)
        print '(a,i0)', "an integer key on a string multimap was accepted, got=", n
    end subroutine scenario_multimap_integer_on_string
    !
    !> The map's method refusal, handed the multimap's prefix.
    subroutine scenario_multimap_string_method_sorted()
        type(pf_index_multimap) :: mm

        call mm%build(["a", "b", "a"], method="sorted")
        print '(a)', "method=sorted on a string multimap build was accepted"
    end subroutine scenario_multimap_string_method_sorted
    !
    !> The probe's own guard, which is a separate check from the scalar one.
    subroutine scenario_multimap_string_probe_on_integer()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: off(:), m(:)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%probe_many(["a", "b"], off, m)
        print '(a,i0)', "string probes on an integer multimap were accepted, pairs=", size(m)
    end subroutine scenario_multimap_string_probe_on_integer
    !
    !> `%keys` into an integer list on a string multimap is refused naming the form to ask for.
    subroutine scenario_multimap_string_keys_rank1()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: list(:)

        call mm%build(["a", "b", "a"])
        call mm%keys(list)
        print '(a,i0)', "an integer key list from a string multimap was accepted, n=", size(list)
    end subroutine scenario_multimap_string_keys_rank1

    !> `%keys` into an `int32` list over a map holding a key below what `int32` can hold.
    !!
    !! **The two-sided half of the bound check.** A stored index value is never negative, so the
    !! map's own narrowing tests one end only; a KEY is the caller's own value, and a one-sided
    !! check would turn this one into a plausible positive key instead of refusing it. The `int64`
    !! call above is the negative control.
    subroutine scenario_index_keys_int32_negative()
        type(pf_index_map) :: m
        integer(int64) :: keys(4), vals(4)
        integer(int64), allocatable :: got64(:)
        integer(int32), allocatable :: got32(:)

        keys = [1_int64, 2_int64, -3_int64, -3000000000_int64]
        vals = [1_int64, 2_int64, 3_int64, 4_int64]
        ! Hashed explicitly: a direct backend over a key range this wide would try to allocate it.
        call m%build(keys, vals, method="hash")
        call m%keys(got64)
        print '(a,i0)', "int64 keys=", size(got64)
        call m%keys(got32)
        print '(a,i0)', "unexpectedly answered keys in int32, n=", size(got32)
    end subroutine scenario_index_keys_int32_negative

    !> `%keys` into an `int32` RANK-2 list over a composite map holding a key below `int32`.
    !!
    !! **`scenario_index_keys_int32_negative`'s composite twin, and not covered by it.** The two
    !! ranks narrow through separate procedures -- `ix_narrow_keys_1` and `ix_narrow_keys_n` --
    !! so the rank-2 bound check had no scenario of its own until this one, which is how the
    !! rank-1 check went on to be deleted outright by a compiler without anything going red
    !! twice. A dropped check here answers a plausible positive key rather than failing visibly.
    subroutine scenario_index_keys_rank2_int32_negative()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2)
        integer(int64), allocatable :: got64(:,:)
        integer(int32), allocatable :: got32(:,:)

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, -3000000000_int64]
        ! Hashed explicitly: a direct backend over a key range this wide would try to allocate it.
        call m%build(pairs, method="hash")
        call m%keys(got64)
        print '(a,i0)', "int64 rank-2 keys rows=", size(got64, 1)
        call m%keys(got32)
        print '(a,i0)', "unexpectedly answered rank-2 keys in int32, rows=", size(got32, 1)
    end subroutine scenario_index_keys_rank2_int32_negative

    !> `%csr` into `int32` arrays over a multimap holding a value above what `int32` can hold.
    subroutine scenario_multimap_csr_int32_value()
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(4), vals(4)
        integer(int64), allocatable :: off64(:), rows64(:)
        integer(int32), allocatable :: off32(:), rows32(:)

        keys = [1_int64, 1_int64, 2_int64, 2_int64]
        vals = [1_int64, 2_int64, 3_int64, 3000000000_int64]
        call mm%build(keys, vals, method="hash")
        call mm%csr(off64, rows64)
        print '(a,i0)', "int64 csr rows=", size(rows64)
        call mm%csr(off32, rows32)
        print '(a,i0)', "unexpectedly answered csr in int32, rows=", size(rows32)
    end subroutine scenario_multimap_csr_int32_value
    !
    !> `%keys` into an `int32` RANK-1 list, asked of a map holding string keys.
    !>
    !> The `int32` forms repeat their `int64` siblings' guards rather than delegating to them, so
    !> each copy needs its own scenario: a dropped check here answers an integer list of hashes for
    !> a map whose keys are strings, which is a plausible array rather than a visible failure.
    subroutine scenario_index_keys_int32_on_string_map()
        type(pf_index_map) :: m
        integer(int32), allocatable :: list(:)

        call m%build(["a", "b"])
        call m%keys(list)
        print '(a,i0)', "an int32 key list from a string map was accepted, n=", size(list)
    end subroutine scenario_index_keys_int32_on_string_map
    !
    !> `%keys` into an `int32` RANK-1 list, asked of a map with composite keys.
    subroutine scenario_index_keys_rank1_int32_on_composite()
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2)
        integer(int32), allocatable :: flat(:)

        pairs(:, 1) = [1_int64, 2_int64]
        pairs(:, 2) = [3_int64, 4_int64]
        call m%build(pairs, method="hash")
        call m%keys(flat)
        print '(a,i0)', "an int32 rank-1 key list was returned for a composite map, n=", size(flat)
    end subroutine scenario_index_keys_rank1_int32_on_composite
    !
    !> `%keys` into an `int32` RANK-2 list, asked of a map holding string keys.
    subroutine scenario_index_keys_rank2_int32_on_string_map()
        type(pf_index_map) :: m
        integer(int32), allocatable :: rows(:,:)

        call m%build(["a", "b"])
        call m%keys(rows)
        print '(a,i0)', "an int32 rank-2 key list from a string map was accepted, rows=", size(rows, 1)
    end subroutine scenario_index_keys_rank2_int32_on_string_map
    !
    !> `%keys` into an `int64` RANK-2 list, asked of a multimap holding string keys.
    subroutine scenario_multimap_keys_rank2_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: rows(:,:)

        call mm%build(["a", "b", "a"])
        call mm%keys(rows)
        print '(a,i0)', "a rank-2 key list from a string multimap was accepted, rows=", size(rows, 1)
    end subroutine scenario_multimap_keys_rank2_on_string_map
    !
    !> `%keys` into an `int32` RANK-1 list, asked of a multimap holding string keys.
    subroutine scenario_multimap_keys_int32_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int32), allocatable :: list(:)

        call mm%build(["a", "b", "a"])
        call mm%keys(list)
        print '(a,i0)', "an int32 key list from a string multimap was accepted, n=", size(list)
    end subroutine scenario_multimap_keys_int32_on_string_map
    !
    !> `%keys` into an `int32` RANK-1 list, asked of a multimap with composite keys.
    subroutine scenario_multimap_keys_rank1_int32_on_composite()
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(3, 2)
        integer(int32), allocatable :: flat(:)

        pairs(:, 1) = [1_int64, 2_int64, 1_int64]
        pairs(:, 2) = [3_int64, 4_int64, 3_int64]
        call mm%build(pairs, method="hash")
        call mm%keys(flat)
        print '(a,i0)', "an int32 rank-1 key list was returned for a composite multimap, n=", size(flat)
    end subroutine scenario_multimap_keys_rank1_int32_on_composite
    !
    !> `%keys` into an `int32` RANK-2 list, asked of a multimap holding string keys.
    subroutine scenario_multimap_keys_rank2_int32_on_string_map()
        type(pf_index_multimap) :: mm
        integer(int32), allocatable :: rows(:,:)

        call mm%build(["a", "b", "a"])
        call mm%keys(rows)
        print '(a,i0)', "an int32 rank-2 key list from a string multimap was accepted, rows=", size(rows, 1)
    end subroutine scenario_multimap_keys_rank2_int32_on_string_map
    !
    !> A `parquet_string_column` asked of a multimap holding INTEGER keys: the mirror of
    !> `scenario_multimap_string_keys_rank1`, and a separate guard from it.
    subroutine scenario_multimap_string_keys_column_on_integer()
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: list

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%keys(list)
        print '(a,i0)', "a string key column from an integer multimap was accepted, n=", list%size()
    end subroutine scenario_multimap_string_keys_column_on_integer
    !
    !> A string bulk lookup whose answer array is a different length from its keys.
    subroutine scenario_multimap_string_bulk_length()
        type(pf_index_multimap) :: mm
        integer(int64) :: rows(3)

        call mm%build(["a", "b", "a"])
        call mm%get_first_many(["a", "b"], rows(1:3))
        print '(a,i0)', "a mismatched string bulk lookup was accepted, rows1=", rows(1)
    end subroutine scenario_multimap_string_bulk_length
    !
    !> A string bulk lookup on a multimap holding integer keys: the bulk forms carry their own
    !> guard, separate from the scalar one `scenario_multimap_string_on_integer` reaches.
    subroutine scenario_multimap_string_bulk_on_integer()
        type(pf_index_multimap) :: mm
        integer(int64) :: rows(2)

        call mm%build([1_int64, 2_int64, 2_int64])
        call mm%get_first_many(["a", "b"], rows)
        print '(a,i0)', "a string bulk lookup on an integer multimap was accepted, rows1=", rows(1)
    end subroutine scenario_multimap_string_bulk_on_integer
    !
    !> `pf_index_pool%reserve` refuses a negative count rather than treating it as zero.
    subroutine scenario_index_pool_reserve_negative()
        type(pf_index_pool) :: p

        call p%reserve(4)               ! control: a legal count
        print '(a,i0)', "a reserve of 4 was accepted, memory=", p%memory_bytes()
        call p%reserve(-1)              ! -> aborts
        print '(a,i0)', "a negative reserve was accepted, memory=", p%memory_bytes()
    end subroutine scenario_index_pool_reserve_negative
    !
    !> The THREADED `%get_or_add_many` checks its codes in ONE pass once the team has finished,
    !> rather than row by row, so the refusal has a second implementation reaching the same
    !> message. Without OpenMP the thread rule answers 1 and the serial per-row check refuses the
    !> same call, which is why the wrapper asserts the message rather than the path.
    subroutine scenario_index_get_or_add_many_threaded_int32_overflow()
        type(pf_index_map) :: m
        integer(int64) :: keys(4096), i
        integer(int32) :: codes(4096)

        do i = 1_int64, 4096_int64
            keys(i) = i + 1_int64
        end do
        call m%build([1_int64], [int(huge(0_int32), int64)], method="hash")
        call m%get_or_add_many(keys, codes, threads=2)   ! -> aborts
        print '(a,i0)', "oversized codes were narrowed into an int32 answer, got=", codes(1)
    end subroutine scenario_index_get_or_add_many_threaded_int32_overflow
    !
    !> Every legal string-key path of both types, in one process: proves the guards above do not
    !> fire on the forms they must not, on every entry the scenarios refuse one shape of.
    subroutine scenario_index_string_control()
        type(pf_index_map) :: m
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: sc, list
        integer(int64) :: out(3), idx
        integer(int32) :: out32(3)
        integer(int64), allocatable :: off(:), pairs(:), rows(:)
        logical :: found

        call sc%clear()
        call sc%append_string("a")
        call sc%append_null()
        call sc%append_string("bb")
        call m%build(["a ", "bb", "c "], method="hash", threads=1)
        call m%build(sc)
        if (m%get("a") /= 1_int64 .or. m%get("bb") /= 3_int64 .or. m%nkeys() /= 2_int64) &
            error stop "control: string build from a column"
        call m%get_many(["a ", "zz", "bb"], out, threads=1)
        call m%get_many(sc, out32, valid=[.true., .true., .false.])
        if (out(1) /= 1_int64 .or. out(2) /= 0_int64 .or. out32(3) /= 0_int32) error stop "control: string get_many"
        call m%set("d", 9_int64)
        call m%get_or_add("e", idx)
        if (idx /= 10_int64) error stop "control: string get_or_add"
        call m%get_or_add_many(["a", "f", "a"], out, valid=[.true., .true., .false.])
        if (out(1) /= 1_int64 .or. out(2) /= 11_int64 .or. out(3) /= 0_int64) error stop "control: string goam"
        call m%remove("d")
        call m%remove("d", found)
        call m%keys(list)
        if (found .or. list%size() /= m%nkeys()) error stop "control: string remove or keys"
        call m%init(strings=.true., capacity=4)
        call m%reserve(8_int64)
        call m%set("x", 1_int32)
        call m%reset()
        call m%clear()
        call mm%build(["a", "b", "a"], [1_int64, 2_int64, 3_int64], method="hash", threads=1)
        call mm%build(sc, valid=[.true., .true., .true.])
        if (mm%count("a") /= 1_int64 .or. mm%get_first("bb") /= 3_int64 .or. mm%ngroups() /= 2_int64) &
            error stop "control: string multimap build"
        call mm%get_all("a", rows)
        call mm%get_first_many([character(len=2) :: "a", "zz", "bb"], out, threads=1)
        call mm%get_many(sc, out32)
        call mm%probe_many(sc, off, pairs, threads=1)
        call mm%keys(list)
        if (size(rows) /= 1 .or. out(2) /= 0_int64 .or. size(pairs) /= 2 .or. list%size() /= 2_int64) &
            error stop "control: string multimap lookups"
        print '(a)', "string index control finished"
    end subroutine scenario_index_string_control
    !
    !> A hundred string keys hashed to ONE bit share two hashes, fifty-odd keys each: the build
    !> warns once, as the run reaches 32, naming the procedure -- and only once, although both runs
    !> pass 32 and every later insert lengthens one. At full width (`narrow=.false.`) the same keys
    !> share no hash and nothing is said. The longest run is printed after the hook is put back,
    !> so the wrapper can see both that the run was long and that the control's was not.
    subroutine scenario_index_string_chain(narrow)
        logical, intent(in) :: narrow !! whether to narrow the hash to one bit first.
        type(pf_index_map) :: m
        character(len=8) :: keys(100)
        integer(int64) :: maxp, chain
        integer :: i

        do i = 1, size(keys)
            write (keys(i), "(a,i0)") "key", i
        end do
        call parquet_set_message_stream("stderr")
        if (narrow) call parquet_debug_set_index_string_hash_bits(1)
        call m%build(keys, threads=1)
        call m%probe_stats(maxp, max_hash_chain=chain)
        call parquet_debug_set_index_string_hash_bits(0)
        print '(a,i0)', "max_hash_chain=", chain
    end subroutine scenario_index_string_chain

    ! ---- parquet_index: pf_index_pool ----
    !
    !> Freeing an index twice is refused. Accepting it silently would put the same index on the
    !> free list twice, so two later callers would each be handed it and each believe they owned
    !> the slot it names -- a wrong answer with no symptom at the point of the mistake.
    subroutine scenario_pool_double_free()
        type(pf_index_pool) :: p
        integer(int64) :: a

        a = p%get_index()
        call p%free_index(a)
        call p%free_index(a)
        print '(a)', "a double free was accepted"
    end subroutine scenario_pool_double_free
    !
    !> Freeing an index the pool never handed out is refused, and carries a different message from
    !> the double free: an index above the watermark is usually a different bug from one released
    !> twice.
    subroutine scenario_pool_free_never_issued()
        type(pf_index_pool) :: p
        integer(int64) :: a

        a = p%get_index()
        call p%free_index(a + 5_int64)
        print '(a)', "freeing a never-issued index was accepted"
    end subroutine scenario_pool_free_never_issued
    !
    !> Zero is not an index this pool ever issues, so freeing it is the never-issued case.
    subroutine scenario_pool_free_zero()
        type(pf_index_pool) :: p
        integer(int64) :: a

        a = p%get_index()
        call p%free_index(0_int64)
        print '(a)', "freeing index 0 was accepted"
    end subroutine scenario_pool_free_zero
    !
    !> The negative control for the three pool guards: taking an index, giving it back, and taking
    !> it again is legal and must stay legal. A double-free guard keyed on "has this index ever
    !> been freed" rather than "is it free now" would pass all three scenarios above and fail here.
    subroutine scenario_pool_control()
        type(pf_index_pool) :: p
        integer(int64) :: a, b

        a = p%get_index()
        b = p%get_index()
        call p%free_index(a)
        a = p%get_index()
        call p%free_index(a)
        call p%free_index(b)
        call p%compact()
        if (p%get_max_index() /= 0_int64) error stop "control: compact should empty the pool"
        print '(a)', "pool control finished"
    end subroutine scenario_pool_control

    ! ============ bounded=.true.: the memory-bounded whole-file read ============
    !
    ! The two scenarios below are a PAIR, and neither means anything alone. Every in-suite test of
    ! this feature (test/test_table.f90) is an A/B against the default engine, which passes just as
    ! happily against a `bounded=` that did nothing at all -- so what proves the bounded path was
    ! actually taken is that the whole-column read hook fires on the default engine and not on the
    ! bounded one. That hook is a process-global C++ flag, so it can only live out here.

    !> Writes the fixture the bounded scenarios read: `key` cycling 0..3 so that no row group's
    !! statistics can rule any value out (the row-group screen must prune NOTHING, or the scenario
    !! measures the pruned case instead), plus one float and one string payload column.
    subroutine write_bounded_scenario_fixture(fname, n, chunk)
        character(len=*), intent(in) :: fname !! file to write.
        integer, intent(in) :: n              !! rows.
        integer, intent(in) :: chunk          !! rows per row group.
        type(parquet_writer) :: w
        integer(int32), allocatable :: key(:), idx(:)
        real(real64), allocatable :: x(:)
        character(len=8), allocatable :: s(:)
        integer :: i

        allocate(key(n), idx(n), x(n), s(n))
        do i = 1, n
            key(i) = int(mod(i - 1, 4), int32)
            idx(i) = int(i, int32)
            x(i) = real(i, real64) * 1.5_real64
            write(s(i), '(a,i0)') "r", i
        end do
        s(1) = "a"
        call parquet_open_writer(w, fname, chunk_size=chunk)
        call parquet_write_column(w, "key", key)
        call parquet_write_column(w, "idx", idx)
        call parquet_write_column(w, "x", x)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
    end subroutine write_bounded_scenario_fixture

    !> A bounded table never takes a whole-column read: not to install its filter, and not to
    !! materialize any column.
    !!
    !! The hook is armed BEFORE the table is opened, so an open-time whole-column read would trip
    !! it too, and the scenario finishing at all is the assertion. Its control is
    !! `default_filter_reads_whole_column` below, which proves the hook does fire on the engine
    !! this one avoids -- without that pair, a hook that had quietly stopped working would make
    !! this scenario pass for the wrong reason.
    subroutine scenario_bounded_table_no_whole_column_read()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_table) :: t
        type(parquet_filter) :: filt
        character(len=*), parameter :: f = "test_run/es_bounded_no_whole_read.parquet"
        integer(int32), allocatable :: idx(:)
        character(len=:), allocatable :: sv(:)

        call write_bounded_scenario_fixture(f, 32, 4)
        call filt%add("key < 1")
        call parquet_debug_set_force_whole_column_read_error(1)
        call parquet_open_table(t, f, filter=filt, bounded=.true.)
        ! Every assembly shape: the paste arm (a numeric column), and the grow-and-append arm
        ! (a string column). materialize_all covers the rest.
        call t%get("idx", idx)
        call t%get("s", sv)
        call t%materialize_all()
        call parquet_debug_set_force_whole_column_read_error(0)
        if (t%nrows() /= 8_int64) error stop "bounded scenario: expected 8 surviving rows"
        if (size(idx) /= 8) error stop "bounded scenario: expected 8 values in idx"
        print '(a)', "a bounded table installed its filter and read every column without a whole-column read"
    end subroutine scenario_bounded_table_no_whole_column_read

    !> A CLONE of a bounded table is bounded too -- the mechanism half of test_bounded_clone.
    !!
    !! `%clone` reopens the file through the same helper `parquet_open_table` uses, and that helper
    !! reads the flag off the cache to pick the filter engine. Left behind, the clone would reattach
    !! through the caching whole-file engine: same answers, no memory bound, nothing to announce it.
    !! The hook is armed across the clone's own open AND its first touch, which is where a dropped
    !! flag would show.
    subroutine scenario_bounded_clone_no_whole_column_read()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_table) :: t, c
        type(parquet_filter) :: filt
        character(len=*), parameter :: f = "test_run/es_bounded_clone_no_whole_read.parquet"
        integer(int32), allocatable :: idx(:)

        call write_bounded_scenario_fixture(f, 32, 4)
        call filt%add("key < 1")
        call parquet_debug_set_force_whole_column_read_error(1)
        call parquet_open_table(t, f, filter=filt, bounded=.true.)
        ! Cloned before anything is read, so every column of the clone is still lazy and its own
        ! first touch is what has to stay bounded.
        call t%clone(c)
        call c%materialize_all()
        call c%get("idx", idx)
        call parquet_debug_set_force_whole_column_read_error(0)
        if (size(idx) /= 8) error stop "bounded clone scenario: expected 8 values in idx"
        print '(a)', "a bounded table's clone read every column without a whole-column read"
    end subroutine scenario_bounded_clone_no_whole_column_read

    !> The CONTROL for the two scenarios above: the DEFAULT engine does take a whole-column read to
    !! install a filter, so with the hook armed it must abort.
    !!
    !! This is what makes their exit-0 meaningful. It also pins the difference the feature exists
    !! for: the caching install reads every filter column over the live row groups in one batched
    !! pass, which is the read the bounded engine replaces with one row group at a time.
    subroutine scenario_default_filter_reads_whole_column()
        interface
            subroutine parquet_debug_set_force_whole_column_read_error(enable) &
                bind(C, name="parquet_debug_set_force_whole_column_read_error")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the next whole-column read to abort; 0 restores.
            end subroutine parquet_debug_set_force_whole_column_read_error
        end interface

        type(parquet_table) :: t
        type(parquet_filter) :: filt
        character(len=*), parameter :: f = "test_run/es_bounded_control.parquet"

        call write_bounded_scenario_fixture(f, 32, 4)
        call filt%add("key < 1")
        call parquet_debug_set_force_whole_column_read_error(1)
        call parquet_open_table(t, f, filter=filt)   ! -> aborts: the caching engine reads it whole
        print '(a,i0)', "unexpectedly installed a default filter with no whole-column read, nrows=", t%nrows()
    end subroutine scenario_default_filter_reads_whole_column

    !> `bounded=.true.` with a `sort=` is refused at open.
    !!
    !! A sort is a row PERMUTATION: sorted row 5 can come from any row group, so there is no chunk
    !! to assemble it from and every row-group-scoped read refuses one outright. Refused at the
    !! table layer instead, before any reader exists, so the message can name the remedy.
    subroutine scenario_bounded_with_sort_refused()
        type(parquet_table) :: t
        type(parquet_sortkey) :: srt
        character(len=*), parameter :: f = "test_run/es_bounded_sort.parquet"

        call write_bounded_scenario_fixture(f, 16, 4)
        call srt%add("idx desc")
        call parquet_open_table(t, f, sort=srt, bounded=.true.)   ! -> aborts
        print '(a,i0)', "unexpectedly opened a bounded table with a sort, nrows=", t%nrows()
    end subroutine scenario_bounded_with_sort_refused

    !> The same refusal reached through a read-in MAML's own `extra: sort:` list rather than the
    !! `sort=` argument. Both spellings are merged into one composed sort before the refusal, which
    !! is why one guard covers both -- and why this scenario exists to prove it does.
    subroutine scenario_bounded_with_maml_sort_refused()
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/es_bounded_mamlsort.parquet"
        character(len=*), parameter :: m = "test_run/es_bounded_mamlsort.maml"

        call write_bounded_scenario_fixture(f, 16, 4)
        call write_scenario_maml_file(m, [character(len=40) :: &
            "table: bounded_sort", &
            "extra:", &
            "  sort:", &
            "  - idx desc" ])
        call parquet_open_table(t, f, maml=m, bounded=.true.)   ! -> aborts
        print '(a,i0)', "unexpectedly opened a bounded table with a maml sort, nrows=", t%nrows()
    end subroutine scenario_bounded_with_maml_sort_refused

    !> A hard `qc=` violation under `bounded` aborts on FIRST TOUCH, not at open.
    !!
    !! Under the bounded engine qc runs per row group as each chunk is read (the chunk reader
    !! applies that row group's mask segment and then checks), so a violation in a row group other
    !! than the first still has to be caught. The bound here is violated only by rows in the LAST
    !! row group, so an implementation that checked only the first chunk would pass silently.
    !!
    !! That it happens at the read rather than at the open is what the print before it establishes:
    !! reaching that line proves the open itself did not abort.
    subroutine scenario_bounded_qc_hard_at_first_touch()
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer(int32), allocatable :: idx(:)
        character(len=*), parameter :: f = "test_run/es_bounded_qc_hard.parquet"

        call write_bounded_scenario_fixture(f, 20, 4)
        ! idx runs 1..20 in row groups of 4, so only the last row group violates this bound.
        call qc%add("idx, >=1, <=16")
        call parquet_open_table(t, f, qc=qc, bounded=.true.)
        print '(a)', "bounded qc: the open itself did not abort, as expected"
        call t%get("idx", idx)   ! -> aborts here, on the row group that violates the bound
        print '(a,i0)', "unexpectedly read a bounded qc-violating column, n=", size(idx)
    end subroutine scenario_bounded_qc_hard_at_first_touch

    !> The SOFT half of the same qc rule: `qc_soft=.true.` warns instead of aborting, and the read
    !! still returns every surviving row.
    !!
    !! Its exit status is 0, so what this scenario asserts is its own answer; the warning text is
    !! asserted by the test-drive wrapper, which is the only side that can read the stream.
    subroutine scenario_bounded_qc_soft_warns()
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer(int32), allocatable :: idx(:)
        character(len=*), parameter :: f = "test_run/es_bounded_qc_soft.parquet"

        call write_bounded_scenario_fixture(f, 20, 4)
        call qc%add("idx, >=1, <=16")
        call parquet_open_table(t, f, qc=qc, qc_soft=.true., bounded=.true.)
        call t%get("idx", idx)
        if (size(idx) /= 20) error stop "bounded soft qc: every row should still be returned"
        if (idx(20) /= 20_int32) error stop "bounded soft qc: the last row should be unchanged"
        print '(a)', "a bounded soft qc violation warned and returned every row"
    end subroutine scenario_bounded_qc_soft_warns

    !> What the two engines hold in Arrow's own memory pool, printed for a human and asserted where
    !! it is not vacuous.
    !!
    !! **Its own process, deliberately.** The pool counter is process-global and test-drive runs a
    !! suite's tests concurrently, so an in-suite assertion would be perturbed by its neighbours.
    !!
    !! **The assertion that carries the weight is the BOUNDED figure's own size**, not the gap
    !! between the two: after a bounded open the pool must hold less than one whole column, which
    !! it does by construction because the scoped install caches nothing. That is what makes this
    !! non-vacuous rather than an A/B that would pass against a flag doing nothing -- a `bounded=`
    !! that were silently ignored would leave the DEFAULT engine's figure here, and the default's
    !! is measured above one whole column. The gap is asserted too, in the direction the design
    !! predicts, and both figures are printed so a failure says which half moved.
    !!
    !! **What is NOT asserted:** the PEAK. That the bounded engine never holds more than one row
    !! group's worth of one column is the headline property, and it is not observable from Fortran:
    !! `parquet_get_arrow_bytes_allocated` reports what is allocated NOW, and both engines' peaks
    !! are transient, inside a call. It is measured instead by
    !! `bench/benchmark_table.sh --mode=read_filtered`, which reports RSS per arm in its own
    !! process; asserting it would need `arrow::default_memory_pool()->max_memory()` as a second
    !! maintainer hook beside this one.
    subroutine scenario_bounded_arrow_pool()
        interface
            function parquet_get_arrow_bytes_allocated() &
                    bind(C, name="parquet_get_arrow_bytes_allocated") result(bytes)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: bytes !! bytes currently allocated from Arrow's default pool.
            end function parquet_get_arrow_bytes_allocated
        end interface

        type(parquet_table) :: t
        type(parquet_filter) :: filt
        character(len=*), parameter :: f = "test_run/es_bounded_pool.parquet"
        integer(int64) :: after_default, after_bounded, column_bytes
        integer(int32), allocatable :: a(:), b(:)
        integer, parameter :: N = 4000, CH = 100

        ! One int32 column's worth of rows, which is what "a whole column" costs here.
        column_bytes = int(N, int64) * 4_int64
        call write_bounded_scenario_fixture(f, N, CH)
        call filt%add("key < 1")

        block
            type(parquet_table) :: d
            call parquet_open_table(d, f, filter=filt)
            after_default = parquet_get_arrow_bytes_allocated()
            call d%get("idx", a)
        end block
        call parquet_open_table(t, f, filter=filt, bounded=.true.)
        after_bounded = parquet_get_arrow_bytes_allocated()
        call t%get("idx", b)

        print '(a,i0,a)', "arrow pool after a default filtered open : ", after_default, " bytes"
        print '(a,i0,a)', "arrow pool after a bounded filtered open : ", after_bounded, " bytes"
        print '(a,i0,a)', "one whole int32 column would be           : ", column_bytes, " bytes"
        ! THE load-bearing assertion: nothing the scoped install did is still resident, so the
        ! figure is the mask and its bookkeeping rather than any column. A bounded= that was
        ! silently ignored would leave the default engine's figure here, which is larger than one
        ! whole column on every run measured.
        if (after_bounded >= column_bytes) then
            error stop "a bounded open left a whole column's worth of Arrow memory resident"
        end if
        ! And the gap, in the direction the design predicts. Measured at roughly 48x on this
        ! fixture; asserted only as an inequality, because the default figure is Arrow's own
        ! bookkeeping after the open's release sweep and is not a number to pin.
        if (after_default <= after_bounded) then
            error stop "a bounded open did not hold less Arrow memory than a default one"
        end if
        if (size(a) /= size(b)) error stop "the two engines disagreed on the surviving row count"
        if (.not. all(a == b)) error stop "the two engines disagreed on the surviving rows"
        print '(a)', "a bounded open left less than one column resident, below the default's, same rows"
    end subroutine scenario_bounded_arrow_pool

    ! ---- %build_index and parquet_table_index ---------------------------------------------------
    !
    !> A five-row in-memory table for the table-index scenarios: `id` (int32, distinct), `x`
    !! (real64), `d` (date), `s` (string), `b` (logical) and `v` (a 2-wide int32 vector).
    subroutine table_index_scenario_fixture(t)
        type(parquet_table), intent(out) :: t !! the table.
        type(parquet_date) :: d(5)
        integer(int32) :: v(2, 5)
        character(len=3) :: s(5)
        integer :: i
        do i = 1, 5
            call d(i)%set(2024, 6, i)
        end do
        v(1, :) = [1, 2, 3, 4, 5]
        v(2, :) = [6, 7, 8, 9, 10]
        s = ["aa ", "bb ", "cc ", "dd ", "ee "]
        call parquet_new_table(t)
        call t%add_column("id", [40_int32, 10_int32, 30_int32, 20_int32, 50_int32])
        call t%add_column("x", [4.0_real64, 1.0_real64, 3.0_real64, 2.0_real64, 5.0_real64])
        call t%add_column("d", d)
        call t%add_column("s", s)
        call t%add_column("b", [.true., .false., .true., .false., .true.])
        call t%add_column("v", v)
    end subroutine table_index_scenario_fixture
    !
    !> A string column indexes through the map's own string keys: this was the refusal scenario
    !! until the map gained them, and is now the CONTROL it said it would become -- every string
    !! query form over both engines, in one process.
    subroutine scenario_table_index_string_column()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_string_column) :: probes
        integer(int64) :: row, rows(3), nf
        integer(int32) :: row32, rows32(2)
        integer(int64), allocatable :: all64(:)
        integer(int32), allocatable :: all32(:)
        call table_index_scenario_fixture(t)
        call probes%clear()
        call probes%append_string("dd")
        call probes%append_null()
        call t%build_index("s", ix)
        call ix%find("cc", row)
        call ix%find("dd", row32)
        call ix%find_all("cc", all64)
        call ix%find_all("zz", all32)
        call ix%find_many(["aa ", "zz ", "ee "], rows, n_found=nf)
        call ix%find_many(probes, rows32, threads=1)
        if (row /= 3_int64 .or. row32 /= 4 .or. size(all64) /= 1 .or. size(all32) /= 0 .or. nf /= 2_int64) &
            error stop "string index: wrong answers"
        if (rows32(1) /= 4 .or. rows32(2) /= 0 .or. ix%count("cc") /= 1_int64 .or. ix%count("cc ") /= 0_int64) &
            error stop "string index: wrong bulk or count answers"
        if (ix%kind() /= PK_STRING .or. ix%nkeys() /= 5_int64) error stop "string index: introspection"
        call t%build_index("s", ix, unique=.false., threads=1)
        call ix%find_all("bb", all64)
        if (size(all64) /= 1 .or. all64(1) /= 2_int64 .or. ix%count("bb") /= 1_int64) &
            error stop "string multimap index: wrong answers"
        print '(a)', "a string column was indexed and answered"
    end subroutine scenario_table_index_string_column
    !
    !> A string key on an index over an integer column is refused naming both, exactly as a real
    !> key is: a string could only ever match nothing there.
    subroutine scenario_table_index_string_key_on_int()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call ix%find("30", row)
        print '(a,i0)', "a string key on an integer index was accepted, row=", row
    end subroutine scenario_table_index_string_key_on_int
    !
    !> And the reverse: an integer key on a string index.
    subroutine scenario_table_index_int_key_on_string()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        call table_index_scenario_fixture(t)
        call t%build_index("s", ix)
        call ix%find(3_int32, row)
        print '(a,i0)', "an integer key on a string index was accepted, row=", row
    end subroutine scenario_table_index_int_key_on_string
    !
    !> A boolean key is `==` with extra steps, the filter's rule for a boolean set, applied here.
    subroutine scenario_table_index_bool_column()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call table_index_scenario_fixture(t)
        call t%build_index("b", ix)
        print '(a,i0)', "a boolean column was indexed, nkeys=", ix%nkeys()
    end subroutine scenario_table_index_bool_column
    !
    !> A vector column has no single value per row to key.
    subroutine scenario_table_index_vector_column()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call table_index_scenario_fixture(t)
        call t%build_index("v", ix)
        print '(a,i0)', "a vector column was indexed, nkeys=", ix%nkeys()
    end subroutine scenario_table_index_vector_column
    !
    !> A repeated key under `unique=.true.` is the map's own duplicate refusal, which names the
    !! key and both of its positions -- table rows here.
    subroutine scenario_table_index_duplicate_unique()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call parquet_new_table(t)
        call t%add_column("k", [7_int64, 3_int64, 7_int64])
        call t%build_index("k", ix)
        print '(a,i0)', "a repeated key was indexed as unique, nkeys=", ix%nkeys()
    end subroutine scenario_table_index_duplicate_unique
    !
    !> A column the table does not have is refused by the ordinary name lookup.
    subroutine scenario_table_index_missing_column()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call table_index_scenario_fixture(t)
        call t%build_index("nope", ix)
        print '(a,i0)', "a missing column was indexed, nkeys=", ix%nkeys()
    end subroutine scenario_table_index_missing_column
    !
    !> The stale index, one scenario per query family: after a row change every query aborts
    !! naming the table and both generations. The row change is a %filter_rows that keeps
    !! every row but one -- the answers would stay in range, which is exactly the case a
    !! cached flag would answer wrongly.
    subroutine scenario_table_index_stale_find()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call ix%find(30_int32, row)
        print '(a,i0)', "a stale index answered find, row=", row
    end subroutine scenario_table_index_stale_find
    !
    !> See `scenario_table_index_stale_find`.
    subroutine scenario_table_index_stale_find_all()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64), allocatable :: rows(:)
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix, unique=.false.)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call ix%find_all(30_int32, rows)
        print '(a,i0)', "a stale index answered find_all, rows=", size(rows)
    end subroutine scenario_table_index_stale_find_all
    !
    !> See `scenario_table_index_stale_find`.
    subroutine scenario_table_index_stale_find_many()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: rows(2)
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call ix%find_many([30_int32, 10_int32], rows)
        print '(a,i0)', "a stale index answered find_many, rows(1)=", rows(1)
    end subroutine scenario_table_index_stale_find_many
    !
    !> See `scenario_table_index_stale_find`.
    subroutine scenario_table_index_stale_count()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        print '(a,i0)', "a stale index answered count, n=", ix%count(30_int32)
    end subroutine scenario_table_index_stale_count
    !
    !> A query on an object nothing has built into yet is refused rather than answering 0 for
    !! everything -- which is what a default-initialised engine would silently do.
    subroutine scenario_table_index_never_built()
        type(parquet_table_index) :: ix
        integer(int64) :: row
        call ix%find(1_int64, row)
        print '(a,i0)', "a never-built index answered find, row=", row
    end subroutine scenario_table_index_never_built
    !
    !> A real key against an integer column is refused naming both, rather than being rounded or
    !! widened into a key that quietly matches nothing.
    subroutine scenario_table_index_kind_mismatch()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call ix%find(30.0_real64, row)
        print '(a,i0)', "a real key was accepted on an integer index, row=", row
    end subroutine scenario_table_index_kind_mismatch
    !
    !> A timestamp key against a date column is refused, exactly as a timestamp SET against a
    !! date column is by the filter: two temporal types are two families, not one widened.
    subroutine scenario_table_index_kind_mismatch_temporal()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_timestamp) :: ts
        call table_index_scenario_fixture(t)
        call t%build_index("d", ix)
        call ts%set(2024, 6, 3, 0, 0, 0)
        print '(a,i0)', "a timestamp key was accepted on a date index, n=", ix%count(ts)
    end subroutine scenario_table_index_kind_mismatch_temporal
    !
    !> `rows` must have one slot per key; a shorter array would be an out-of-bounds write.
    subroutine scenario_table_index_find_many_length()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: rows(2)
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call ix%find_many([30_int32, 10_int32, 50_int32], rows)
        print '(a,i0)', "find_many accepted a short rows array, rows(1)=", rows(1)
    end subroutine scenario_table_index_find_many_length
    !
    !> `threads=0` reaches the engine's own build and is refused there, exactly as on the map.
    subroutine scenario_table_index_threads_zero()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        call table_index_scenario_fixture(t)
        call t%build_index("id", ix, threads=0)
        print '(a,i0)', "threads=0 was accepted by build_index, nkeys=", ix%nkeys()
    end subroutine scenario_table_index_threads_zero
    !
    !> The negative control for every table-index guard above: each legal shape of every call
    !! completes, so a guard that fired on a legal call would fail here.
    subroutine scenario_table_index_control()
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_date) :: d
        type(parquet_time) :: tm(5), tq
        type(parquet_timestamp) :: ts(5), tsq
        integer(int64) :: row, rows(3), nf
        integer(int32) :: row32, rows32(3)
        integer(int64), allocatable :: all64(:)
        integer(int32), allocatable :: all32(:)
        character(len=:), allocatable :: nm
        call table_index_scenario_fixture(t)
        call tm(1)%set(1, 2, 3)
        call tm(2)%set(4, 5, 6)
        call tm(3)%set(1, 2, 3)
        call tm(4)%set(7, 8, 9)
        call tm(5)%set(10, 11, 12)
        call ts(1)%set(2024, 6, 1, 1, 2, 3)
        call ts(2)%set(2024, 6, 1, 4, 5, 6)
        call ts(3)%set(2024, 6, 1, 1, 2, 3)
        call ts(4)%set(2024, 6, 1, 7, 8, 9)
        call ts(5)%set(2024, 6, 1, 10, 11, 12)
        call t%add_column("tm", tm)
        call t%add_column("ts", ts)
        ! Every key family, unique and not, every query form and both row kinds.
        call t%build_index("id", ix)
        call ix%find(30_int32, row)
        call ix%find(30_int64, row32)
        call ix%find_all(30_int32, all64)
        call ix%find_all(30_int64, all32)
        call ix%find_many([30_int32, 10_int32, 99_int32], rows, n_found=nf)
        call ix%find_many([30_int64, 10_int64, 99_int64], rows32, threads=1)
        if (row /= 3_int64 .or. row32 /= 3 .or. size(all64) /= 1 .or. size(all32) /= 1 .or. nf /= 2_int64) &
            error stop "integer index: wrong answers"
        if (ix%count(30_int32) /= 1_int64 .or. ix%count(30_int64) /= 1_int64) error stop "integer count"
        call t%build_index("x", ix, unique=.false., threads=2)
        call ix%find(3.0_real32, row)
        call ix%find(3.0_real64, row32)
        call ix%find_all(3.0_real32, all64)
        call ix%find_all(3.0_real64, all32)
        call ix%find_many([3.0_real32, 9.0_real32], rows(1:2))
        call ix%find_many([3.0_real64, 9.0_real64], rows32(1:2), threads=2, n_found=nf)
        if (row /= 3_int64 .or. row32 /= 3 .or. ix%count(3.0_real32) /= 1_int64 .or. ix%count(3.0_real64) /= 1_int64) &
            error stop "real index: wrong answers"
        call t%build_index("d", ix)
        call d%set(2024, 6, 4)
        call ix%find(d, row)
        call ix%find(d, row32)
        call ix%find_all(d, all64)
        call ix%find_all(d, all32)
        call ix%find_many([d, d], rows(1:2))
        call ix%find_many([d, d], rows32(1:2))
        if (row /= 4_int64 .or. ix%count(d) /= 1_int64) error stop "date index: wrong answers"
        call t%build_index("tm", ix, unique=.false.)
        call tq%set(1, 2, 3)
        call ix%find(tq, row)
        call ix%find(tq, row32)
        call ix%find_all(tq, all64)
        call ix%find_all(tq, all32)
        call ix%find_many([tq, tq], rows(1:2))
        call ix%find_many([tq, tq], rows32(1:2))
        if (row /= 1_int64 .or. ix%count(tq) /= 2_int64 .or. size(all64) /= 2) error stop "time index: wrong answers"
        call t%build_index("ts", ix, unique=.false.)
        call tsq%set(2024, 6, 1, 1, 2, 3)
        call ix%find(tsq, row)
        call ix%find(tsq, row32)
        call ix%find_all(tsq, all64)
        call ix%find_all(tsq, all32)
        call ix%find_many([tsq, tsq], rows(1:2))
        call ix%find_many([tsq, tsq], rows32(1:2))
        if (row /= 1_int64 .or. ix%count(tsq) /= 2_int64 .or. size(all32) /= 2) error stop "timestamp index: wrong answers"
        call ix%name(nm)
        if (nm /= "ts" .or. ix%kind() /= PK_TIMESTAMP .or. ix%is_unique() .or. ix%nkeys() /= 4_int64) &
            error stop "introspection: wrong answers"
        ! A value write and a fill leave it current; a clear leaves it unbuilt without an abort.
        call t%set_element("x", 2_int64, 11.0_real64)
        if (.not. ix%is_current()) error stop "a value write staled the index"
        call ix%clear()
        if (ix%is_current()) error stop "clear left the index current"
        print '(a)', "table index control finished"
    end subroutine scenario_table_index_control
    !
    ! ---- %group_by and parquet_grouping ---------------------------------------------------------
    !
    !> A five-row in-memory table for the grouping scenarios: `key` (int32, with ties: 3 at rows
    !! 2 and 4, 7 at rows 1 and 3, 9 at row 5), `x` (real64, row-distinct), `s` (string) and `v`
    !! (a 2-wide int32 vector).
    subroutine table_group_scenario_fixture(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: v(2, 5)
        character(len=2) :: s(5)
        v(1, :) = [1, 2, 3, 4, 5]
        v(2, :) = [6, 7, 8, 9, 10]
        s = ["aa", "bb", "cc", "dd", "ee"]
        call parquet_new_table(t)
        call t%add_column("key", [7_int32, 3_int32, 7_int32, 3_int32, 9_int32])
        call t%add_column("x", [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64])
        call t%add_column("s", s)
        call t%add_column("v", v)
    end subroutine table_group_scenario_fixture
    !
    !> No key at all is refused naming the verb, rather than answering one group of every row or
    !! falling through to %argsort_by's own "no sort key" message. A grouping over a real key
    !! first is the negative control.
    subroutine scenario_table_group_no_key()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        print '(a,i0)', "group_by over one key ran, groups=", grp%ngroups()
        call t%group_by("", grp)
        print '(a,i0)', "unexpectedly grouped by no key, groups=", grp%ngroups()
    end subroutine scenario_table_group_no_key
    !
    !> A column the table does not have is refused by the ordinary name lookup, naming group_by.
    subroutine scenario_table_group_unknown_key()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by(["key"], grp)
        print '(a,i0)', "group_by over a real column ran, groups=", grp%ngroups()
        call t%group_by(["nope"], grp)
        print '(a,i0)', "unexpectedly grouped by a missing column, groups=", grp%ngroups()
    end subroutine scenario_table_group_unknown_key
    !
    !> A key that reads as a sort key is refused: the group order is a contract, not an option.
    subroutine scenario_table_group_direction_token()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        print '(a,i0)', "group_by over a bare name ran, groups=", grp%ngroups()
        call t%group_by("-key", grp)
        print '(a,i0)', "unexpectedly accepted a direction token, groups=", grp%ngroups()
    end subroutine scenario_table_group_direction_token
    !
    !> A vector column has no single value per row to group on -- the sort's refusal, worded
    !! for a grouping.
    subroutine scenario_table_group_vector_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by(["x"], grp)
        print '(a,i0)', "group_by over a scalar column ran, groups=", grp%ngroups()
        call t%group_by(["v"], grp)
        print '(a,i0)', "unexpectedly grouped by a vector column, groups=", grp%ngroups()
    end subroutine scenario_table_group_vector_column
    !
    !> A container column has no defined order, so no grouping either; the same table groups by
    !! its scalar key first.
    subroutine scenario_table_group_container_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        type(parquet_list_column) :: lc
        call table_group_scenario_fixture(t)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32])
        call lc%append_row([4_int32, 5_int32, 6_int32])
        call lc%append_row([7_int32])
        call lc%append_row([8_int32, 9_int32])
        call t%add_column("tags", lc)
        call t%group_by(["key"], grp)
        print '(a,i0)', "group_by beside a list column ran, groups=", grp%ngroups()
        call t%group_by(["tags"], grp)
        print '(a,i0)', "unexpectedly grouped by a list column, groups=", grp%ngroups()
    end subroutine scenario_table_group_container_column
    !
    !> The stale grouping, one scenario per query family: after a row change every per-group
    !! query aborts naming the table and both generations.
    !! The row change is a %filter_rows that keeps every row but one -- the answers would stay
    !! in range, which is exactly the case a cached flag would answer wrongly. Each queries once
    !! before the change, as its control.
    subroutine scenario_table_group_stale_size()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: counts(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%size(counts)
        print '(a,i0)', "size answered before the change, groups=", size(counts)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%size(counts)
        print '(a,i0)', "a stale grouping answered size, groups=", size(counts)
    end subroutine scenario_table_group_stale_size
    !
    !> See `scenario_table_group_stale_size`.
    subroutine scenario_table_group_stale_rows()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: rows(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%rows(1_int64, rows)
        print '(a,i0)', "rows answered before the change, n=", size(rows)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%rows(1_int64, rows)
        print '(a,i0)', "a stale grouping answered rows, n=", size(rows)
    end subroutine scenario_table_group_stale_rows
    !
    !> See `scenario_table_group_stale_size`.
    subroutine scenario_table_group_stale_csr()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: offsets(:), rows(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%csr(offsets, rows)
        print '(a,i0)', "csr answered before the change, n=", size(rows)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%csr(offsets, rows)
        print '(a,i0)', "a stale grouping answered csr, n=", size(rows)
    end subroutine scenario_table_group_stale_csr
    !
    !> See `scenario_table_group_stale_size`; `%last_rows` shares the guard with `%first_rows`.
    subroutine scenario_table_group_stale_first_rows()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: rows(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%first_rows(rows)
        call grp%last_rows(rows)
        print '(a,i0)', "first_rows and last_rows answered before the change, n=", size(rows)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%first_rows(rows)
        print '(a,i0)', "a stale grouping answered first_rows, n=", size(rows)
    end subroutine scenario_table_group_stale_first_rows
    !
    !> See `scenario_table_group_stale_size`.
    subroutine scenario_table_group_stale_group_ids()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: codes(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%group_ids(codes)
        print '(a,i0)', "group_ids answered before the change, n=", size(codes)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%group_ids(codes)
        print '(a,i0)', "a stale grouping answered group_ids, n=", size(codes)
    end subroutine scenario_table_group_stale_group_ids
    !
    !> See `scenario_table_group_stale_size`.
    subroutine scenario_table_group_stale_key_table()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        print '(a,i0)', "key_table answered before the change, rows=", kt%nrows()
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%key_table(kt)
        print '(a,i0)', "a stale grouping answered key_table, rows=", kt%nrows()
    end subroutine scenario_table_group_stale_key_table
    !
    !> See `scenario_table_group_stale_size`.
    subroutine scenario_table_group_stale_count()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: counts(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%count("x", counts)
        print '(a,i0)', "count answered before the change, groups=", size(counts)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%count("x", counts)
        print '(a,i0)', "a stale grouping answered count, groups=", size(counts)
    end subroutine scenario_table_group_stale_count
    !
    !> A per-group query on an object nothing has built into yet is refused rather than
    !! answering zero groups -- which is what a default-initialised object would silently do. A
    !! built grouping answering first is the control.
    subroutine scenario_table_group_never_built()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp, fresh
        integer(int64), allocatable :: counts(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%size(counts)
        print '(a,i0)', "a built grouping answered size, groups=", size(counts)
        call fresh%size(counts)
        print '(a,i0)', "a never-built grouping answered size, groups=", size(counts)
    end subroutine scenario_table_group_never_built
    !
    !> A group number past the last group is refused naming the range; the last group first.
    subroutine scenario_table_group_out_of_range()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: rows(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%rows(3_int64, rows)
        print '(a,i0)', "the last group answered, n=", size(rows)
        call grp%rows(4_int64, rows)
        print '(a,i0)', "unexpectedly answered a group past the last, n=", size(rows)
    end subroutine scenario_table_group_out_of_range
    !
    !> A `size_name=` that is a key column's name would give the key table two columns of one
    !! name; refused before anything is gathered. A distinct name first is the control.
    subroutine scenario_table_group_size_name_clash()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt, size_name="n")
        print '(a,i0)', "key_table with a distinct size_name ran, cols=", kt%ncols()
        call grp%key_table(kt, size_name="key")
        print '(a,i0)', "unexpectedly accepted a size_name that names a key, cols=", kt%ncols()
    end subroutine scenario_table_group_size_name_clash
    !
    !> `%key_table(reserve=)` is a count of spare slots, so a negative one is refused rather
    !! than silently reserving nothing. The control reserves four.
    subroutine scenario_table_group_key_table_reserve_negative()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt, reserve=4)
        print '(a,i0)', "key_table with a reserve of four ran, spare=", kt%column_capacity(free=.true.)
        call grp%key_table(kt, reserve=-1)
        print '(a,i0)', "unexpectedly accepted a negative reserve, spare=", kt%column_capacity(free=.true.)
    end subroutine scenario_table_group_key_table_reserve_negative
    !
    !> A per-group column belongs on a table with one row per group, and the grouping's OWN table
    !! is the one target that would otherwise fail silently: `%add_column` on it would relocate
    !! the slots the grouping points into, inside the same call, and the column would be
    !! `%ngroups()` values spread over `%nrows()` rows. Refused before anything is computed; the
    !! key table is the control.
    subroutine scenario_table_group_add_agg_target_is_source()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("x", "mean", kt, "m")
        print '(a,i0)', "add_agg onto the key table ran, cols=", kt%ncols()
        call grp%add_agg("x", "mean", t, "m")
        print '(a,i0)', "unexpectedly wrote a per-group column onto the source table, cols=", t%ncols()
    end subroutine scenario_table_group_add_agg_target_is_source
    !
    !> A target of the wrong LENGTH is the quiet half of the same rule: the column is well formed
    !! and every value is about a different group. Here the target is another grouping's key
    !! table, which has one row per group of ITS partition; the right one is the control.
    subroutine scenario_table_group_add_agg_rows_mismatch()
        type(parquet_table) :: t, kt, other
        type(parquet_grouping) :: grp, grp2
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("x", "mean", kt, "m")
        print '(a,i0)', "add_agg onto its own key table ran, rows=", kt%nrows()
        call t%group_by("s", grp2)
        call grp2%key_table(other)
        print '(a,i0,a,i0)', "the other grouping has groups=", grp2%ngroups(), ", this one=", grp%ngroups()
        call grp%add_agg("x", "mean", other, "m")
        print '(a,i0)', "unexpectedly wrote onto a target of the wrong length, cols=", other%ncols()
    end subroutine scenario_table_group_add_agg_rows_mismatch
    !
    !> `as=` on `%add_agg` names the ONE column the call adds, so a list is a mistake rather than
    !! a request for several: `%add_apply` is the form that takes a list. One name is the control.
    subroutine scenario_table_group_add_agg_as_two_names()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("x", "mean", kt, "m")
        print '(a,i0)', "add_agg with one name ran, cols=", kt%ncols()
        call grp%add_agg("x", "mean", kt, "m2,m3")
        print '(a,i0)', "unexpectedly accepted two names in as=, cols=", kt%ncols()
    end subroutine scenario_table_group_add_agg_as_two_names
    !
    !> `nan_to_null=` decides what happens to a NaN answer, and the exact `int64` family never
    !! answers NaN -- it aborts where no exact answer exists. Giving both is a misunderstanding
    !! the library refuses rather than ignores. `exact=` alone is the control.
    subroutine scenario_table_group_add_agg_nan_to_null_exact()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("key", "sum", kt, "s", exact=.true.)
        print '(a,i0)', "add_agg exact= ran, cols=", kt%ncols()
        call grp%add_agg("key", "sum", kt, "s2", exact=.true., nan_to_null=.true.)
        print '(a,i0)', "unexpectedly accepted nan_to_null= with exact=, cols=", kt%ncols()
    end subroutine scenario_table_group_add_agg_nan_to_null_exact

    !> `%container_ptr` on a column that holds no container. Every caller of it is about to
    !> dereference what it hands back, so refusing by NAME is the only answer that faults where the
    !> mistake is; a null pointer would fault somewhere else entirely, in code that is correct.
    subroutine scenario_column_container_wrong_kind()
        type(parquet_column) :: col
        class(parquet_container_column), pointer :: p

        call col%init(PK_INT32, nrows=2_int64)
        call col%container_ptr(p)   ! an int32 column is not a container -> aborts
        print '(a,i0)', "unexpectedly aliased a container in an int32 column, rows=", col%length()
    end subroutine scenario_column_container_wrong_kind

    !> `%append_row_of` from a CONTAINER column. Copying one row of a list, map or struct means
    !> copying a whole variable-length row into the destination's payload, which this primitive has
    !> no route for yet -- so it says that, rather than falling through to the message about a
    !> column with no active storage, which is not what is wrong.
    subroutine scenario_column_append_row_of_container()
        type(parquet_column) :: dst, src
        type(parquet_list_column) :: lsrc, ldst
        class(parquet_container_column), allocatable :: cc

        ! BOTH columns are lists of the same payload kind, so the kind-agreement guard above the
        ! storage switch passes and the container arm itself is what refuses.
        call lsrc%init(PK_INT32)
        call lsrc%append_row([1_int32, 2_int32])
        allocate(cc, source=lsrc)
        call src%adopt_container(cc)
        call ldst%init(PK_INT32)
        call ldst%append_row([9_int32])
        allocate(cc, source=ldst)
        call dst%adopt_container(cc)
        call dst%append_row_of(src, 1_int64)   ! -> aborts
        print '(a,i0)', "unexpectedly appended a container row, rows=", dst%length()
    end subroutine scenario_column_append_row_of_container

    !> A set name past the 64-character limit. The store keys sets by a fixed-width name, so a
    !> longer one would be silently truncated onto another set's -- which is why the limit is a
    !> refusal rather than a trim.
    subroutine scenario_filter_set_name_too_long()
        type(parquet_filter) :: filt

        call filt%bind("a_set_name_that_is_deliberately_far_longer_than_the_sixty_four_character_limit", &
                       [1_int32, 2_int32])   ! -> aborts
        call filt%add("v in @a_set_name_that_is_deliberately_far_longer_than_the_sixty_four_character_limit")
        print '(a)', "unexpectedly bound a set under an over-long name"
    end subroutine scenario_filter_set_name_too_long

    !> A bare `@` with no set name after it. The tokenizer has a separate arm for it because the
    !> general "not a set" message names the literal the caller wrote, and `@` alone is not a
    !> literal -- it is a name that was never typed.
    subroutine scenario_filter_bare_at_set_name()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_bare_at.parquet"

        ! `%add` only stores the rule; it is applied that reads it, so the abort comes at open.
        call write_list_scenario_fixture(file)
        call filt%add("id in @")
        call parquet_open_reader(reader, file, filter=filt)   ! -> aborts
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        print '(a,i0)', "unexpectedly parsed a rule whose set name is a bare @, rows=", nrows
    end subroutine scenario_filter_bare_at_set_name

    !> A separator far longer than any the writers accept. The refusal quotes what it was given, and
    !> caps that quotation at 100 characters so a caller who passed a whole buffer by mistake gets a
    !> message rather than the buffer.
    subroutine scenario_skycoord_text_long_separator()
        character(len=:), allocatable :: text

        call pf_dec2str(-12.5_real64, text, sep=repeat("x", 150))   ! -> aborts
        print '(a,a)', "unexpectedly wrote with a 150-character separator: ", text
    end subroutine scenario_skycoord_text_long_separator

    !> `weights=`/`weight_column=` with `exact=.true.`. The exact int64 family is unweighted by
    !> definition, so an option it cannot honour is refused rather than ignored -- the twin of
    !> scenario_table_group_add_agg_nan_to_null_exact, and its own arm of the same guard.
    subroutine scenario_table_group_add_agg_weights_exact()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("key", "sum", kt, "s", exact=.true.)
        print '(a,i0)', "add_agg exact= ran, cols=", kt%ncols()
        call grp%add_agg("key", "sum", kt, "s2", exact=.true., weight_column="x")
        print '(a,i0)', "unexpectedly accepted weight_column= with exact=, cols=", kt%ncols()
    end subroutine scenario_table_group_add_agg_weights_exact
    !
    !> `%add_agg` is a per-group query, so it runs the generation check every other one runs: a
    !! stale grouping would put in-range row numbers' answers onto a summary whose key column
    !! names the wrong groups, and nothing downstream could question the column. An add before
    !! the row change is the control.
    subroutine scenario_table_group_stale_add_agg()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("x", "mean", kt, "m")
        print '(a,i0)', "add_agg ran before the change, cols=", kt%ncols()
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%add_agg("x", "mean", kt, "m2")
        print '(a,i0)', "a stale grouping answered add_agg, cols=", kt%ncols()
    end subroutine scenario_table_group_stale_add_agg
    !
    !> `%add_size` runs the same check, and it is the one of the three where a missing check would
    !! be hardest to notice: the column it writes is a count, so group sizes belonging to a
    !! partition the table has outrun look exactly like group sizes that belong to it. An add
    !! before the row change is the control.
    subroutine scenario_table_group_stale_add_size()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_size(kt, "n")
        print '(a,i0)', "add_size ran before the change, cols=", kt%ncols()
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%add_size(kt, "n2")
        print '(a,i0)', "a stale grouping answered add_size, cols=", kt%ncols()
    end subroutine scenario_table_group_stale_add_size
    !
    !> Every refusal `%agg` raises is raised by `%add_agg` too, and names the binding the caller
    !! actually wrote rather than `agg` -- the reason the shared bodies take the calling binding's
    !! name as an argument. An unknown token is the example; the same call with a real token is
    !! the control.
    subroutine scenario_table_group_add_agg_unknown_token()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_agg("x", "mean", kt, "m")
        print '(a,i0)', "add_agg mean ran, cols=", kt%ncols()
        call grp%add_agg("x", "medain", kt, "m2")
        print '(a,i0)', "unexpectedly accepted an unknown token, cols=", kt%ncols()
    end subroutine scenario_table_group_add_agg_unknown_token
    !
    !> A name the target already carries is refused rather than replaced, so a summary cannot
    !! lose a column to a second call that meant a new one; `force=.true.` is how a replacement
    !! is asked for. Here the key table already has its count column from `size_name=`, and
    !! `%add_size` under a free name is the control.
    subroutine scenario_table_group_add_size_name_taken()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt, size_name="n")
        call grp%add_size(kt, "n2")
        print '(a,i0)', "add_size under a free name ran, cols=", kt%ncols()
        call grp%add_size(kt, "n")
        print '(a,i0)', "unexpectedly replaced a column without force=, cols=", kt%ncols()
    end subroutine scenario_table_group_add_size_name_taken
    !
    !> `%add_apply` runs the same target check `%add_agg` does, at its own site and under its own
    !! name: the grouping's own table is refused before the callback is called even once. The key
    !! table is the control.
    subroutine scenario_table_group_add_apply_target_is_source()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_apply(scenario_group_two, kt, "a,b")
        print '(a,i0)', "add_apply onto the key table ran, cols=", kt%ncols()
        call grp%add_apply(scenario_group_two, t, "a,b")
        print '(a,i0)', "unexpectedly wrote per-group columns onto the source table, cols=", t%ncols()
    end subroutine scenario_table_group_add_apply_target_is_source
    !
    !> `as=` on `%add_apply` is the list of names the call adds, and its length is how many
    !! results per group the callback is asked for -- so a list that names nothing is a request
    !! for no columns from no results, refused rather than quietly doing nothing. Two names are
    !! the control.
    subroutine scenario_table_group_add_apply_no_name()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_apply(scenario_group_two, kt, "a,b")
        print '(a,i0)', "add_apply with two names ran, cols=", kt%ncols()
        call grp%add_apply(scenario_group_two, kt, " , ")
        print '(a,i0)', "unexpectedly accepted an as= that names nothing, cols=", kt%ncols()
    end subroutine scenario_table_group_add_apply_no_name
    !
    !> A name repeated in `as=` would ask one table for two columns of one name: the second add
    !! would replace the first and the caller would read one result twice. Refused before
    !! anything is written; two distinct names are the control.
    subroutine scenario_table_group_add_apply_duplicate_name()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_apply(scenario_group_two, kt, "a,b")
        print '(a,i0)', "add_apply with two distinct names ran, cols=", kt%ncols()
        call grp%add_apply(scenario_group_two, kt, "m,m")
        print '(a,i0)', "unexpectedly accepted one name twice in as=, cols=", kt%ncols()
    end subroutine scenario_table_group_add_apply_duplicate_name
    !
    !> Every name is checked against the target BEFORE the first column is written, so a call
    !! refused on its LAST name leaves the target exactly as it was: the count printed just
    !! before the abort is the count the control left, and no `new` column was written on the way
    !! to discovering that `n` is taken. The object form is the control.
    subroutine scenario_table_group_add_apply_name_taken()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        type(scenario_group_reducer) :: red
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt, size_name="n")
        call grp%add_apply(red, kt, "a,b")
        print '(a,i0)', "add_apply under free names ran, cols=", kt%ncols()
        print '(a,i0)', "the target carries before the refused call, cols=", kt%ncols()
        call grp%add_apply(scenario_group_two, kt, "new,n")
        print '(a,i0)', "unexpectedly wrote a column before the last name was checked, cols=", kt%ncols()
    end subroutine scenario_table_group_add_apply_name_taken
    !
    !> `%add_apply` is a per-group query, so it runs the generation check every other one runs,
    !! before the callback is called and before the target is touched. An add before the row
    !! change is the control.
    subroutine scenario_table_group_stale_add_apply()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_apply(scenario_group_two, kt, "a,b")
        print '(a,i0)', "add_apply ran before the change, cols=", kt%ncols()
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%add_apply(scenario_group_two, kt, "c,d")
        print '(a,i0)', "a stale grouping answered add_apply, cols=", kt%ncols()
    end subroutine scenario_table_group_stale_add_apply
    !
    !> `threads=` below 1 is refused at the forwarding site too, naming `add_apply` rather than
    !! the `apply` loop it forwards to: the argument is the caller's re-entrancy declaration, and
    !! a team of none declares nothing. `threads=1` is the control.
    subroutine scenario_table_group_add_apply_threads_zero()
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%key_table(kt)
        call grp%add_apply(scenario_group_two, kt, "a,b", threads=1)
        print '(a,i0)', "add_apply with threads=1 ran, cols=", kt%ncols()
        call grp%add_apply(scenario_group_two, kt, "c,d", threads=0)
        print '(a,i0)', "unexpectedly accepted threads=0, cols=", kt%ncols()
    end subroutine scenario_table_group_add_apply_threads_zero
    !
    !> The `int32` `%rows` forwards onto the same range check, so a group past the last is
    !! refused with the same text -- `g` is widened BEFORE the check, which is why the message
    !! names the number the caller wrote rather than a converted one. The last group is the
    !! control.
    subroutine scenario_table_group_rows_g32_out_of_range()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int32), allocatable :: rows(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%rows(3_int32, rows)
        print '(a,i0)', "the last group answered through an int32 g, n=", size(rows)
        call grp%rows(7_int32, rows)
        print '(a,i0)', "unexpectedly answered a group past the last through an int32 g, n=", size(rows)
    end subroutine scenario_table_group_rows_g32_out_of_range
    !
    !> The `int32` `%gather` forwards onto the same buffer check: a buffer shorter than the group
    !! aborts rather than truncating, whichever kind `g` was written in. A buffer the size of the
    !! largest group is the control.
    subroutine scenario_table_group_gather_g32_short_buffer()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: buf2(2), buf1(1)
        integer(int32) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("x", 1_int32, buf2, n)
        print '(a,i0)', "a buffer the size of the largest group answered through an int32 g, n=", n
        call grp%gather("x", 1_int32, buf1, n)
        print '(a,i0)', "unexpectedly truncated a two-row group into a one-entry buffer, n=", n
    end subroutine scenario_table_group_gather_g32_short_buffer
    !
    !> `%apply` on a stale grouping, in its procedure form: the generation check runs before the
    !! callback is called even once, so no procedure of the caller's computes anything from rows
    !! that name the wrong galaxies. An apply before the row change is the control.
    subroutine scenario_table_group_stale_apply()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%apply(scenario_group_row_sum, out)
        print '(a,i0)', "apply answered before the change, groups=", size(out)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%apply(scenario_group_row_sum, out)
        print '(a,i0)', "a stale grouping answered apply, groups=", size(out)
    end subroutine scenario_table_group_stale_apply
    !
    !> `%apply` on a stale grouping, in its object form: the reducer's `%reduce` is never reached
    !! either. The object form before the row change is the control.
    subroutine scenario_table_group_stale_apply_object()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        type(scenario_group_reducer) :: red
        real(real64), allocatable :: out(:, :)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%apply(red, 2, out)
        print '(a,i0)', "the object form answered before the change, groups=", size(out, 2)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%apply(red, 2, out)
        print '(a,i0)', "a stale grouping answered the object form, groups=", size(out, 2)
    end subroutine scenario_table_group_stale_apply_object
    !
    !> `nout` below 1 is refused before the loop: a result row with no entries is a mistake, not
    !! a zero-length answer. The two-value form first is the control.
    subroutine scenario_table_group_apply_nout()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:, :)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%apply(scenario_group_two, 2, out)
        print '(a,i0)', "apply with nout=2 ran, groups=", size(out, 2)
        call grp%apply(scenario_group_two, 0, out)
        print '(a,i0)', "unexpectedly accepted nout=0, groups=", size(out, 2)
    end subroutine scenario_table_group_apply_nout
    !
    !> `threads=` below 1 on a callback form is refused, naming the serial default: the argument
    !! is the caller's re-entrancy declaration, and a team of none declares nothing. `threads=1`
    !! first is the control.
    subroutine scenario_table_group_apply_threads_zero()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%apply(scenario_group_row_sum, out, threads=1)
        print '(a,i0)', "apply with threads=1 ran, groups=", size(out)
        call grp%apply(scenario_group_row_sum, out, threads=0)
        print '(a,i0)', "unexpectedly accepted threads=0, groups=", size(out)
    end subroutine scenario_table_group_apply_threads_zero
    !
    !> `%agg` on a stale grouping: the generation check runs before any column is gathered. The
    !! same call before the row change is the control.
    subroutine scenario_table_group_stale_agg()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out)
        print '(a,i0)', "agg answered before the change, groups=", size(out)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%agg("x", "mean", out)
        print '(a,i0)', "a stale grouping answered agg, groups=", size(out)
    end subroutine scenario_table_group_stale_agg
    !
    !> `%nunique` on a stale grouping; the same call before the row change is the control.
    subroutine scenario_table_group_stale_nunique()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%nunique("x", out)
        print '(a,i0)', "nunique answered before the change, groups=", size(out)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%nunique("x", out)
        print '(a,i0)', "a stale grouping answered nunique, groups=", size(out)
    end subroutine scenario_table_group_stale_nunique
    !
    !> An unknown statistic token is refused listing the vocabulary; `"mean"` first is the control.
    subroutine scenario_table_group_agg_unknown_token()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out)
        print '(a,i0)', "agg mean ran, groups=", size(out)
        call grp%agg("x", "medain", out)
        print '(a,i0)', "unexpectedly accepted an unknown token, groups=", size(out)
    end subroutine scenario_table_group_agg_unknown_token
    !
    !> `"quantile"` without `q=` is refused; with `q=` first is the control.
    subroutine scenario_table_group_agg_quantile_needs_q()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "quantile", out, q=0.5_real64)
        print '(a,i0)', "quantile with q= ran, groups=", size(out)
        call grp%agg("x", "quantile", out)
        print '(a,i0)', "unexpectedly accepted quantile without q=, groups=", size(out)
    end subroutine scenario_table_group_agg_quantile_needs_q
    !
    !> An option a token does not take is refused rather than ignored: `q=` on `"mean"`. The
    !! same option on `"quantile"` first is the control.
    subroutine scenario_table_group_agg_option_refused()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "quantile", out, q=0.5_real64)
        print '(a,i0)', "q= on quantile ran, groups=", size(out)
        call grp%agg("x", "mean", out, q=0.5_real64)
        print '(a,i0)', "unexpectedly accepted q= on mean, groups=", size(out)
    end subroutine scenario_table_group_agg_option_refused
    !
    !> The exact int64 family on a real column is refused naming the kind; on the int32 key
    !! first is the control.
    subroutine scenario_table_group_agg_int_real_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("key", "sum", out)
        print '(a,i0)', "the exact sum of an int32 column ran, groups=", size(out)
        call grp%agg("x", "sum", out)
        print '(a,i0)', "unexpectedly accepted the exact family on a real column, groups=", size(out)
    end subroutine scenario_table_group_agg_int_real_column
    !
    !> The exact `"sum"` aborts on overflow naming the group, never wraps: two values of 2**62
    !! in one group, whose sum is 2**63, one above `huge`. The exact sum of the key first is the
    !! control.
    subroutine scenario_table_group_agg_int_sum_overflow()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        integer(int64) :: h(5)
        call table_group_scenario_fixture(t)
        h = [2_int64**62, 1_int64, 2_int64**62, 2_int64, 3_int64]
        call t%add_column("h", h)
        call t%group_by("key", grp)
        call grp%agg("key", "sum", out)
        print '(a,i0)', "the exact sum of the key ran, groups=", size(out)
        call grp%agg("h", "sum", out)
        print '(a,i0)', "unexpectedly accepted an overflowing exact sum, out(2)=", out(2)
    end subroutine scenario_table_group_agg_int_sum_overflow
    !
    !> The exact `"min"` of a group with no non-null value has no answer and aborts naming the
    !! group; the exact `"sum"` of the same column (0 there) first is the control.
    subroutine scenario_table_group_agg_int_all_null()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%add_column("n", [1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call t%set_null("n", 2_int64)
        call t%set_null("n", 4_int64)
        call t%group_by("key", grp)
        call grp%agg("n", "sum", out)
        print '(a,i0)', "the exact sum over an all-null group ran, out(1)=", out(1)
        call grp%agg("n", "min", out)
        print '(a,i0)', "unexpectedly answered the exact min of an all-null group, out(1)=", out(1)
    end subroutine scenario_table_group_agg_int_all_null
    !
    !> A string column under a value statistic is refused naming the kind, with the pointers at
    !! `%count`, `%nunique` and `%first_rows`; the same statistic over `x` first is the control.
    subroutine scenario_table_group_agg_string_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out)
        print '(a,i0)', "mean of x ran, groups=", size(out)
        call grp%agg("s", "mean", out)
        print '(a,i0)', "unexpectedly accepted a string column, groups=", size(out)
    end subroutine scenario_table_group_agg_string_column
    !
    !> `weights=` of the wrong length is refused naming both lengths; the right length first.
    subroutine scenario_table_group_agg_weights_length()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weights=[1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a,i0)', "weights of the right length ran, groups=", size(out)
        call grp%agg("x", "mean", out, weights=[1.0_real64, 1.0_real64, 1.0_real64])
        print '(a,i0)', "unexpectedly accepted short weights, groups=", size(out)
    end subroutine scenario_table_group_agg_weights_length
    !
    !> A negative weight is refused naming its table row, before any group is computed; a zero
    !! weight (a row leaving the population) first is the control.
    subroutine scenario_table_group_agg_negative_weight()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weights=[1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a,i0)', "a zero weight ran, groups=", size(out)
        call grp%agg("x", "mean", out, weights=[1.0_real64, -1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a,i0)', "unexpectedly accepted a negative weight, groups=", size(out)
    end subroutine scenario_table_group_agg_negative_weight
    !
    !> `weights=` and `weight_column=` together are refused; each alone first is the control.
    subroutine scenario_table_group_agg_both_weights()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weight_column="x")
        call grp%agg("x", "mean", out, weights=[1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64])
        print '(a,i0)', "each weight form alone ran, groups=", size(out)
        call grp%agg("x", "mean", out, weights=[1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
            weight_column="x")
        print '(a,i0)', "unexpectedly accepted both weight forms, groups=", size(out)
    end subroutine scenario_table_group_agg_both_weights
    !
    !> A string weight column is refused naming its kind; a numeric one first is the control.
    subroutine scenario_table_group_agg_weight_column_string()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weight_column="key")
        print '(a,i0)', "an int32 weight column ran, groups=", size(out)
        call grp%agg("x", "mean", out, weight_column="s")
        print '(a,i0)', "unexpectedly accepted a string weight column, groups=", size(out)
    end subroutine scenario_table_group_agg_weight_column_string
    !
    !> A vector weight column is refused naming its kind; the procedure form with a numeric one
    !! first is the control (the weight rules are shared by both forms).
    subroutine scenario_table_group_agg_weight_column_vector()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", scenario_group_col_mean, out, weight_column="key")
        print '(a,i0)', "the procedure form with an int32 weight column ran, groups=", size(out)
        call grp%agg("x", scenario_group_col_mean, out, weight_column="v")
        print '(a,i0)', "unexpectedly accepted a vector weight column, groups=", size(out)
    end subroutine scenario_table_group_agg_weight_column_vector
    !
    !> Weights given to a statistic they cannot affect (`"size"`) are refused rather than
    !! ignored; `"mean"` with the same weights first is the control.
    subroutine scenario_table_group_agg_weights_ignored()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weight_column="x")
        print '(a,i0)', "a weighted mean ran, groups=", size(out)
        call grp%agg("x", "size", out, weight_column="x")
        print '(a,i0)', "unexpectedly accepted weights on size, groups=", size(out)
    end subroutine scenario_table_group_agg_weights_ignored
    !
    !> `%nunique` over a vector column is refused through the sort's own lookup, worded for
    !! this verb; over a scalar column first is the control.
    subroutine scenario_table_group_nunique_vector_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%nunique("s", out)
        print '(a,i0)', "nunique of a string column ran, groups=", size(out)
        call grp%nunique("v", out)
        print '(a,i0)', "unexpectedly counted a vector column, groups=", size(out)
    end subroutine scenario_table_group_nunique_vector_column
    subroutine scenario_table_group_stale_broadcast()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: counts(:), per_row(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%size(counts)
        call grp%broadcast(counts, per_row)
        print '(a,i0)', "broadcast answered before the change, rows=", size(per_row)
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%broadcast(counts, per_row)
        print '(a,i0)', "a stale grouping answered broadcast, rows=", size(per_row)
    end subroutine scenario_table_group_stale_broadcast
    subroutine scenario_table_group_stale_gather()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: buf(2)
        integer(int64) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("x", 1_int64, buf, n)
        print '(a,i0)', "gather answered before the change, n=", n
        call t%filter_rows([.true., .false., .true., .true., .true.])
        call grp%gather("x", 1_int64, buf, n)
        print '(a,i0)', "a stale grouping answered gather, n=", n
    end subroutine scenario_table_group_stale_gather
    subroutine scenario_table_group_gather_short_buffer()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: buf2(2), buf1(1)
        integer(int64) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("x", 1_int64, buf2, n)
        print '(a,i0)', "a buffer the size of the largest group answered, n=", n
        call grp%gather("x", 1_int64, buf1, n)
        print '(a,i0)', "unexpectedly truncated a two-row group into a one-entry buffer, n=", n
    end subroutine scenario_table_group_gather_short_buffer
    subroutine scenario_table_group_gather_short_valid()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: buf(2)
        logical :: ok2(2), ok1(1)
        integer(int64) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("x", 1_int64, buf, n, is_valid=ok2)
        print '(a,i0)', "an is_valid the size of the group answered, n=", n
        call grp%gather("x", 1_int64, buf, n, is_valid=ok1)
        print '(a,i0)', "unexpectedly truncated the validity of a two-row group, n=", n
    end subroutine scenario_table_group_gather_short_valid
    subroutine scenario_table_group_gather_out_of_range()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: buf(2)
        integer(int64) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("x", 3_int64, buf, n)
        print '(a,i0)', "the last group answered, n=", n
        call grp%gather("x", 4_int64, buf, n)
        print '(a,i0)', "unexpectedly gathered a group past the last, n=", n
    end subroutine scenario_table_group_gather_out_of_range
    subroutine scenario_table_group_gather_kind()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int32) :: ibuf(2)
        integer(int64) :: n
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%gather("key", 1_int64, ibuf, n)
        print '(a,i0)', "the int32 key column gathered into an int32 buffer, n=", n
        call grp%gather("x", 1_int64, ibuf, n)
        print '(a,i0)', "unexpectedly narrowed a real64 column into an int32 buffer, n=", n
    end subroutine scenario_table_group_gather_kind
    subroutine scenario_table_group_broadcast_length()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: per_row(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%broadcast([10_int64, 20_int64, 30_int64], per_row)
        print '(a,i0)', "one value per group broadcast, rows=", size(per_row)
        call grp%broadcast([10_int64, 20_int64], per_row)
        print '(a,i0)', "unexpectedly broadcast two values over three groups, rows=", size(per_row)
    end subroutine scenario_table_group_broadcast_length
    subroutine scenario_table_group_blank_key()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by([character(len=3) :: "key"], grp)
        print '(a,i0)', "a named key grouped, groups=", grp%ngroups()
        call t%group_by([character(len=3) :: "key", "   "], grp)
        print '(a,i0)', "unexpectedly accepted a blank key name, groups=", grp%ngroups()
    end subroutine scenario_table_group_blank_key
    subroutine scenario_table_group_direction_word()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        print '(a,i0)', "a bare key grouped, groups=", grp%ngroups()
        call t%group_by("key desc", grp)
        print '(a,i0)', "unexpectedly accepted a direction word, groups=", grp%ngroups()
    end subroutine scenario_table_group_direction_word
    subroutine scenario_table_group_query_unknown_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%count("x", out)
        print '(a,i0)', "a resident column counted, groups=", size(out)
        call grp%count("no_such_column", out)
        print '(a,i0)', "unexpectedly counted a column that does not exist, groups=", size(out)
    end subroutine scenario_table_group_query_unknown_column
    subroutine scenario_table_group_query_unsupported_column()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%group_by("rowid", grp)
        call grp%count("rowid", out)
        print '(a,i0)', "a supported column counted, groups=", size(out)
        call grp%count("m_intkey", out)
        print '(a,i0)', "unexpectedly counted an unsupported column, groups=", size(out)
    end subroutine scenario_table_group_query_unsupported_column
    subroutine scenario_table_group_agg_exact_unknown_token()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("key", "sum", out)
        print '(a,i0)', "an exact statistic ran, groups=", size(out)
        call grp%agg("key", "median", out)
        print '(a,i0)', "unexpectedly ran a real64-only token into an int64 out, groups=", size(out)
    end subroutine scenario_table_group_agg_exact_unknown_token
    subroutine scenario_table_group_agg_method_refused()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "median", out, method="linear")
        print '(a,i0)', "method= reached the median, groups=", size(out)
        call grp%agg("x", "mean", out, method="linear")
        print '(a,i0)', "unexpectedly accepted method= on the mean, groups=", size(out)
    end subroutine scenario_table_group_agg_method_refused
    subroutine scenario_table_group_agg_ddof_refused()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "std", out, ddof=0)
        print '(a,i0)', "ddof= reached the standard deviation, groups=", size(out)
        call grp%agg("x", "mean", out, ddof=0)
        print '(a,i0)', "unexpectedly accepted ddof= on the mean, groups=", size(out)
    end subroutine scenario_table_group_agg_ddof_refused
    subroutine scenario_table_group_agg_scale_refused()
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mad", out, scale="raw")
        print '(a,i0)', "scale= reached the mad, groups=", size(out)
        call grp%agg("x", "mean", out, scale="raw")
        print '(a,i0)', "unexpectedly accepted scale= on the mean, groups=", size(out)
    end subroutine scenario_table_group_agg_scale_refused
    subroutine scenario_table_group_agg_nan_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        real(real64) :: w(5)
        w = 1.0_real64
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weights=w)
        print '(a,i0)', "finite weights ran, groups=", size(out)
        w(3) = ieee_value(0.0_real64, ieee_quiet_nan)
        call grp%agg("x", "mean", out, weights=w)
        print '(a,i0)', "unexpectedly accepted a NaN weight, groups=", size(out)
    end subroutine scenario_table_group_agg_nan_weight
    subroutine scenario_table_group_agg_infinite_weight()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: out(:)
        real(real64) :: w(5)
        w = 1.0_real64
        call table_group_scenario_fixture(t)
        call t%group_by("key", grp)
        call grp%agg("x", "mean", out, weights=w)
        print '(a,i0)', "finite weights ran, groups=", size(out)
        w(4) = ieee_value(0.0_real64, ieee_positive_inf)
        call grp%agg("x", "mean", out, weights=w)
        print '(a,i0)', "unexpectedly accepted an infinite weight, groups=", size(out)
    end subroutine scenario_table_group_agg_infinite_weight
    !
    !> A date set against a timestamp column is refused at apply, naming both -- two temporal
    !! types are two families, and an element of the wrong type has no instant to convert.
    subroutine scenario_filter_temporal_set_mismatch()
        type(parquet_writer) :: w
        type(parquet_schema) :: schema
        type(parquet_reader) :: r
        type(parquet_filter) :: filt
        type(parquet_timestamp) :: ts(2)
        type(parquet_date) :: d(1)
        integer(int64) :: n
        call ts(1)%set(2024, 1, 31, 12, 30, 0)
        call ts(2)%set(2024, 1, 31, 12, 30, 1)
        call d(1)%set(2024, 1, 31)
        call schema%init("es_temporal_set")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_open_writer(w, "test_run/es_filter_temporal_set.parquet", schema=schema)
        call parquet_write_column(w, "ts", ts)
        call parquet_close_writer(w)
        call filt%add_in("ts", d)
        call parquet_open_reader(r, "test_run/es_filter_temporal_set.parquet", filter=filt)
        call parquet_get_nrows(r, n)
        print '(a,i0)', "a date set was applied to a timestamp column, nrows=", n
    end subroutine scenario_filter_temporal_set_mismatch
    !
    !> The other two temporal families against a column that is not temporal at all. The refusal
    !> names the SET's family, and each family has its own word -- so a set whose word was wrong
    !> would still refuse the clause, with a message pointing at the wrong kind of set.
    subroutine scenario_filter_time_set_on_int()
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_filter) :: filt
        type(parquet_time) :: tm(1)
        integer(int64) :: n
        call tm(1)%set(12, 30, 0)
        call parquet_open_writer(w, "test_run/es_filter_time_set_on_int.parquet")
        call parquet_write_column(w, "v", [1_int32, 2_int32])
        call parquet_close_writer(w)
        call filt%add_in("v", tm)
        call parquet_open_reader(r, "test_run/es_filter_time_set_on_int.parquet", filter=filt)
        call parquet_get_nrows(r, n)
        print '(a,i0)', "a time set was applied to an int32 column, nrows=", n
    end subroutine scenario_filter_time_set_on_int
    !
    subroutine scenario_filter_timestamp_set_on_int()
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_filter) :: filt
        type(parquet_timestamp) :: ts(1)
        integer(int64) :: n
        call ts(1)%set(2024, 1, 31, 12, 30, 0)
        call parquet_open_writer(w, "test_run/es_filter_ts_set_on_int.parquet")
        call parquet_write_column(w, "v", [1_int32, 2_int32])
        call parquet_close_writer(w)
        call filt%add_in("v", ts)
        call parquet_open_reader(r, "test_run/es_filter_ts_set_on_int.parquet", filter=filt)
        call parquet_get_nrows(r, n)
        print '(a,i0)', "a timestamp set was applied to an int32 column, nrows=", n
    end subroutine scenario_filter_timestamp_set_on_int
    !
    !> A literal list on a temporal column is refused naming the %bind route: a member would need
    !! the column's stored unit to convert, which is the text path's job and not a list's.
    subroutine scenario_filter_temporal_literal_list()
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_filter) :: filt
        type(parquet_date) :: d(2)
        integer(int64) :: n
        call d(1)%set(2024, 1, 1)
        call d(2)%set(2024, 1, 2)
        call parquet_open_writer(w, "test_run/es_filter_temporal_list.parquet")
        call parquet_write_column(w, "d", d)
        call parquet_close_writer(w)
        call filt%add('d in ("2024-01-01", "2024-01-02")')
        call parquet_open_reader(r, "test_run/es_filter_temporal_list.parquet", filter=filt)
        call parquet_get_nrows(r, n)
        print '(a,i0)', "a literal list was applied to a date column, nrows=", n
    end subroutine scenario_filter_temporal_literal_list

    !> `%print_rows(last=-1)` is refused exactly as `first=-1` is, and by its OWN guard.
    !!
    !! The two bounds are validated by two separate `if` blocks, each writing the offending value
    !! into the message, so a `last=` guard that had been dropped -- or that reported `first`'s
    !! value -- would leave `print_rows_negative_count` green. The positive `last=` is the control.
    subroutine scenario_print_rows_negative_last()
        type(parquet_table) :: t
        character(len=*), parameter :: file = "test_run/print_rows_negative_last.parquet"

        call write_print_rows_fixture(file)
        call parquet_open_table(t, file)
        call t%print_rows(last=2)
        print '(a)', "control: a positive last= printed"
        call t%print_rows(last=-1)
        print '(a)', "unexpectedly accepted a negative last="
    end subroutine scenario_print_rows_negative_last

    !> `columns=` naming a column whose TYPE this library cannot read is refused where it is
    !! named, rather than producing an output column holding nothing.
    !!
    !! `m_intkey` in the map fixture is an integer-keyed map, which this library does not read at
    !! all -- the table classifies it as unsupported and holds no values for it. Carrying it
    !! across a join would have nothing to carry, so the refusal names it. An ordinary readable
    !! column carried by the same call is the control -- deliberately a SCALAR one, since a map
    !! column is refused a step earlier for being a container at all, which is a different guard
    !! with a different message.
    subroutine scenario_join_columns_unsupported()
        type(parquet_table) :: a, b
        integer(int64), parameter :: IDS(4) = [10_int64, 20_int64, 30_int64, 40_int64]

        call parquet_open_table(b, "test/fixtures/map_payloads.parquet")
        call b%add_column("id", IDS)
        call b%add_column("ok", IDS)
        call join_fixture(a, IDS)
        call a%join(b, "id", columns="ok", how="left")
        print '(a,i0)', "control: a readable payload column came across, cols=", a%ncols()
        call join_fixture(a, IDS)
        call a%join(b, "id", columns="m_intkey", how="left")   ! -> aborts (holds no values)
        print '(a,i0)', "unexpectedly carried an unreadable column across, cols=", a%ncols()
    end subroutine scenario_join_columns_unsupported

    !> `parquet_derive_schema(name="")` is refused here rather than left to `%init`, whose message
    !! would name a procedure the caller never called. MAML requires a `table:` value.
    !!
    !! A non-blank `name=` is the control, so the abort is about the blank and not about `name=`
    !! being present at all.
    subroutine scenario_derive_schema_blank_name()
        type(parquet_table) :: t
        type(parquet_schema) :: s

        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call parquet_derive_schema(t, s, name="named")
        print '(a)', "control: a non-blank name= derived a schema"
        call parquet_derive_schema(t, s, name="   ")
        print '(a)', "unexpectedly accepted a blank name="
    end subroutine scenario_derive_schema_blank_name

    !> `parquet_open_writer_like` on a table with nothing resident says THAT, rather than writing
    !! a file with no columns in it.
    !!
    !! A schema cannot be derived from columns that have not been read, and a caller who has just
    !! opened a file has read nothing -- so the message names both ways out, materializing or
    !! passing `schema=`. The materialized table is the control.
    subroutine scenario_open_writer_like_nothing_resident()
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=*), parameter :: src = "test_run/error_scenario_writerlike_src.parquet"
        character(len=*), parameter :: ctrl = "test_run/error_scenario_writerlike_ctrl.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_writerlike_bad.parquet"
        type(parquet_writer) :: sw

        call parquet_open_writer(sw, src)
        call parquet_write_column(sw, "a", [1_int32, 2_int32])
        call parquet_close_writer(sw)
        call parquet_open_table(t, src)
        call t%materialize_all()
        call parquet_open_writer_like(w, ctrl, t)
        call parquet_close_writer(w)
        print '(a)', "control: a materialized table derived a schema"
        call parquet_open_table(t, src)
        call parquet_open_writer_like(w, bad, t)   ! -> aborts (nothing resident)
        print '(a)', "unexpectedly derived a schema from a table with nothing resident"
    end subroutine scenario_open_writer_like_nothing_resident

    !> A sink schema naming a template column that holds NO VALUES is refused at open, not at the
    !! first append.
    !!
    !! `m_intkey` is an integer-keyed map, which this library cannot read -- the template has the
    !! column but no values for it, so there would be nothing to write. That is a different guard
    !! from `sink_schema_names_a_missing_column`, whose column is absent altogether, and the
    !! readable column opened first is the control that tells the two apart. That control is a
    !! plain int32 column added to the template rather than one of the fixture's own maps: a map
    !! declared as a scalar field is refused further in, by the writer's own column init, so it
    !! would abort the control instead of passing it.
    subroutine scenario_sink_schema_names_an_unreadable_column()
        type(parquet_table_writer) :: out
        type(parquet_table) :: tmpl
        type(parquet_schema) :: s, s2
        character(len=*), parameter :: ctrl = "test_run/error_scenario_sink_unreadable_ctrl.parquet"
        character(len=*), parameter :: bad = "test_run/error_scenario_sink_unreadable_bad.parquet"

        call parquet_open_table(tmpl, "test/fixtures/map_payloads.parquet")
        call tmpl%add_column("ok", [1_int32, 2_int32, 3_int32, 4_int32])
        call s%init("sink")
        call s%add_field("ok", "int32")
        call parquet_open_table_writer(out, ctrl, tmpl, schema=s)
        call parquet_close_table_writer(out)
        print '(a)', "control: a schema naming a readable column opened"
        ! A SECOND schema object: %init refuses to re-initialize one that is already built.
        call s2%init("sink")
        call s2%add_field("m_intkey", "int32")
        call parquet_open_table_writer(out, bad, tmpl, schema=s2)   ! -> aborts (holds no values)
        print '(a)', "unexpectedly opened a sink over a column that holds no values"
    end subroutine scenario_sink_schema_names_an_unreadable_column

    !> An int64 group sum that overflows is REFUSED rather than wrapped, and the message names
    !! the group and points at the real64 form.
    !!
    !! Wrapping is the failure this guard exists to prevent: a wrapped sum is a plausible negative
    !! number that no caller would question. The group that does not overflow is the control, and
    !! it shares the aggregate call with the one that does -- so a guard that refused every int64
    !! sum would fail the control instead.
    subroutine scenario_agg_int64_sum_overflow()
        type(parquet_table) :: t
        type(parquet_grouping) :: g
        integer(int64) :: vals(4)
        integer(int64), allocatable :: sums(:)
        integer(int32) :: keys(4)

        ! Group 1 sums to 2 and group 2 overflows: two values just over half of huge(int64).
        keys = [1_int32, 1_int32, 2_int32, 2_int32]
        vals = [1_int64, 1_int64, huge(0_int64) / 2_int64 + 1_int64, huge(0_int64) / 2_int64 + 1_int64]
        call parquet_new_table(t)
        call t%add_column("k", keys(1:2))
        call t%add_column("v", vals(1:2))
        call t%group_by(["k"], g)
        call g%agg("v", "sum", sums)
        print '(a,i0)', "control: a group whose int64 sum fits was summed, s=", sums(1)
        call parquet_new_table(t)
        call t%add_column("k", keys)
        call t%add_column("v", vals)
        call t%group_by(["k"], g)
        call g%agg("v", "sum", sums)   ! -> aborts (group 2 overflows)
        print '(a)', "unexpectedly wrapped an overflowing int64 group sum"
    end subroutine scenario_agg_int64_sum_overflow

    !> `%print_rows` with no `unit=` follows `message_stream`, so setting it to stderr moves the
    !! whole table there.
    !!
    !! Out-of-process because `message_stream` is process-global: a unit test that set it would
    !! change where every concurrently running suite's output went. Nothing aborts here -- the
    !! assertion is WHICH stream the table landed on, which is what `check_scenario_streams` can
    !! see and the exit status cannot.
    !!
    !! Both settings are exercised in one run, so the scenario also shows the setting is what
    !! decides: the marker column name appears on stderr while the stdout line printed after the
    !! switch back does not carry it.
    subroutine scenario_print_rows_follows_message_stream()
        type(parquet_table) :: t

        call parquet_new_table(t)
        call t%add_column("streammarker", [1_int32, 2_int32])
        call parquet_set_message_stream("stderr")
        call t%print_rows()
        call parquet_set_message_stream("stdout")
        print '(a)', "control: the stream was put back to stdout"
    end subroutine scenario_print_rows_follows_message_stream

    !> `%print_stat` with no `unit=` follows `message_stream`.
    !!
    !! Out-of-process for the same reason `%print_rows`' scenario is: neither stream can be
    !! captured from inside the process, and the setting is global to it.
    subroutine scenario_print_stat_follows_message_stream()
        type(parquet_table) :: t

        call parquet_new_table(t)
        call t%add_column("statmarker", [1_int32, 2_int32])
        call parquet_set_message_stream("stderr")
        call t%print_stat()
        call parquet_set_message_stream("stdout")
        print '(a)', "control: the stream was put back to stdout"
    end subroutine scenario_print_stat_follows_message_stream

    !> Both `parquet_strings` printers follow `message_stream` with no `unit=`.
    !!
    !! One scenario for the pair because they share a destination rule; the marker is the column's
    !! own content, so a half-applied change (the column printer moved, the handle printer not)
    !! leaves the handle's line on stdout and fails the "must not also appear on the other
    !! stream" half of the assertion.
    subroutine scenario_string_print_follows_message_stream()
        ! `target` because %view hands back a handle that points into the column: F2018 15.5.2.4
        ! leaves that pointer undefined otherwise, and only nagfor's -C=dangling reports it.
        type(parquet_string_column), target :: col
        type(parquet_string) :: h

        call col%append_string("strmarker")
        call parquet_set_message_stream("stderr")
        call col%print()
        h = col%view(1_int64)
        call h%print()
        call parquet_set_message_stream("stdout")
        print '(a)', "control: the stream was put back to stdout"
    end subroutine scenario_string_print_follows_message_stream

    !> `parquet_print_settings` follows `message_stream` too: its exemption is from `verbosity`,
    !! not from where the text goes.
    !!
    !! Run at `verbosity="silent"`, which is the exemption that matters -- the dump must still
    !! appear, and it must appear on the stream the setting names.
    subroutine scenario_print_settings_follows_message_stream()
        call parquet_set_verbosity("silent")
        call parquet_set_message_stream("stderr")
        call parquet_print_settings()
        call parquet_reset_settings()
        print '(a)', "control: the settings were reset"
    end subroutine scenario_print_settings_follows_message_stream

    !> `%print_schema_info` called with NEITHER `unit=` nor `filename=` writes to the
    !! `message_stream` unit instead of aborting.
    !!
    !! This call form did not exist before: it used to `error stop` with "either unit or filename
    !! must be given". The scenario exits cleanly, which is half the assertion; the other half is
    !! that the listing lands on stderr.
    subroutine scenario_print_schema_info_default_stream()
        type(parquet_schema) :: schema

        schema%maml%name = "schema_default_stream.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: schemamarker", &
            "fields:", &
            "- name: v", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call parquet_set_message_stream("stderr")
        call schema%print_schema_info()
        call parquet_set_message_stream("stdout")
        print '(a)', "control: the stream was put back to stdout"
    end subroutine scenario_print_schema_info_default_stream

    !> The context lines a failing `parquet_close_writer` prints follow `message_stream`.
    !!
    !! The writer is closed with a column declared and never written, which is the path that emits
    !! the context line naming the output file before aborting. The scenario therefore ABORTS
    !! (exit 1); what is asserted is which stream carried the filename.
    subroutine scenario_error_context_follows_message_stream()
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema

        schema%maml%name = "ctx_stream.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: ctxmarker", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]
        call parquet_parse_maml(schema)
        call parquet_set_message_stream("stderr")
        call parquet_open_writer(writer, "test_run/scenario_ctx_stream.parquet", schema)
        call parquet_write_column(writer, "a", [1_int32, 2_int32])
        call parquet_close_writer(writer)   ! -> aborts naming the file (column 'b' was never written)
        print '(a)', "unexpectedly closed a writer with a column missing"
    end subroutine scenario_error_context_follows_message_stream

    !> `parquet_close_reader(print_stat=.true.)` prints its report from C++, and that report
    !! follows the mirrored `message_stream`.
    !!
    !! The marker is the fixture's own filename, which the report's "file:" line carries.
    subroutine scenario_reader_print_stat_follows_message_stream()
        type(parquet_reader) :: reader

        ! Nothing is read: the report's header and its "file:" line are printed whatever was
        ! touched, and the marker this asserts on is that filename.
        call parquet_set_message_stream("stderr")
        call parquet_open_reader(reader, "test/fixtures/element_nulls.parquet")
        call parquet_close_reader(reader, print_stat=.true.)
        call parquet_set_message_stream("stdout")
        print '(a)', "control: the stream was put back to stdout"
    end subroutine scenario_reader_print_stat_follows_message_stream

    !> `verbosity="silent"` set AFTER the reader was opened still silences its `print_stat` report.
    !!
    !! The report is printed by C++ from a mirrored copy of the setting, and that mirror is
    !! refreshed when a reader is opened -- so without a refresh at close time, this sequence
    !! printed the report in full. The marker is the fixture's filename: it must appear on
    !! neither stream.
    subroutine scenario_close_reader_print_stat_silenced_late()
        type(parquet_reader) :: reader

        ! The verbosity is set AFTER the open, which is the whole point: the C++ mirror is
        ! refreshed at open, so before the push at close time this printed the report in full.
        call parquet_open_reader(reader, "test/fixtures/element_nulls.parquet")
        call parquet_set_verbosity("silent")
        call parquet_close_reader(reader, print_stat=.true.)
        call parquet_reset_settings()
        print '(a)', "control: the reader closed and the settings were reset"
    end subroutine scenario_close_reader_print_stat_silenced_late

    !> A `parquet_string` handle that was never bound is reported at EVERY verbosity.
    !!
    !! `%print` checks the handle before it consults the verbosity, so `"silent"` suppresses the
    !! output and not the diagnosis. With the two lines the other way round this call returns
    !! quietly and the scenario exits 0 instead of aborting.
    subroutine scenario_string_print_unbound_handle_silent()
        type(parquet_string) :: h

        call parquet_set_verbosity("silent")
        call h%print()   ! -> aborts (the handle was never bound)
        print '(a)', "unexpectedly printed through an unbound parquet_string handle"
    end subroutine scenario_string_print_unbound_handle_silent

    !> `%get_matrix` naming a column whose type this library cannot read is refused by the shared
    !! `matrix_prepare`, which `%set_matrix` uses as well.
    !!
    !! The readable pair read first is the control: without it the abort would be satisfied by a
    !! `%get_matrix` that refused every call.
    subroutine scenario_get_matrix_unsupported_column()
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)

        call parquet_open_table(t, "test/fixtures/map_payloads.parquet")
        call t%add_column("a", [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call t%add_column("b", [5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64])
        call t%get_matrix(["a", "b"], m)
        print '(a,i0)', "control: a readable pair came back, rows=", size(m, 2)
        call t%get_matrix(["m_intkey"], m)   ! -> aborts (holds no values)
        print '(a)', "unexpectedly read a column that holds no values"
    end subroutine scenario_get_matrix_unsupported_column

    !> A long list of absent column names is TRUNCATED in the message rather than printed whole.
    !!
    !! `%keep_columns` reports EVERY absent name at once -- unlike `%get_matrix`, which resolves
    !! one name at a time and stops at the first -- so it is the verb whose message can run long,
    !! and the listing is capped at 100 characters and closed with an ellipsis. A caller who
    !! mistyped a whole name array gets a readable message rather than a screenful.
    !!
    !! The `%keep_columns` that succeeds is the control: without it the abort would be satisfied
    !! by a verb that refused every call.
    subroutine scenario_keep_columns_many_missing()
        type(parquet_table) :: t
        character(len=12) :: many(20)
        integer :: k

        ! Twelve characters is exactly what `many` holds: "absent_col" is ten, leaving room for a
        ! two-digit index. Twenty of them run well past the 100-character cap under test.
        do k = 1, 20
            write(many(k), '(a,i0)') "absent_col", k
        end do
        call parquet_new_table(t)
        call t%add_column("a", [1.0_real64, 2.0_real64])
        call t%add_column("b", [3.0_real64, 4.0_real64])
        call t%keep_columns(["a"])
        print '(a,i0)', "control: keep_columns kept the named column, ncols=", t%ncols()
        call t%keep_columns(many)   ! -> aborts, listing truncated at 100 chars
        print '(a)', "unexpectedly kept twenty columns that do not exist"
    end subroutine scenario_keep_columns_many_missing

    !> The hash engine's `max_rows=` refusal over a STRING key and over a TWO-COLUMN key.
    !!
    !! Reporting a refused join's largest key group (`hash_biggest`) probes the multimap in
    !! whichever shape the keys were built in, and the three shapes are three different
    !! `%get_many` calls: a `parquet_string_column` for a lone string key, a tuple array for a
    !! composite key, and a plain code array for everything else. Every existing `max_rows`
    !! scenario joins on one int64 column, so only the third ever runs.
    !!
    !! `which` picks the shape; both halves force the hash engine first, since the sort engine
    !! computes the same number by a different route and would leave these arms unrun.
    subroutine scenario_join_max_rows_hash_keyshape(which)
        use iso_c_binding, only : c_int64_t
        character(len=*), intent(in) :: which !! "string" or "tuple".
        interface
            subroutine set_join_engine(mode) bind(C, name="parquet_debug_set_join_engine")
                import :: c_int64_t
                integer(c_int64_t), value :: mode !! 0 automatic, 1 sort, 2 hash.
            end subroutine set_join_engine
        end interface
        type(parquet_table) :: a, b, w
        !
        call set_join_engine(2_c_int64_t)
        ! Three rows on each side sharing ONE key value: nine pairs, so a ceiling of 8 refuses and
        ! a ceiling of 9 does not. The control join is what shows the refusal is the ceiling's.
        if (which == "string") then
            call keyshape_string_fixture(a)
            call keyshape_string_fixture(b)
            call a%clone(w)
            call w%join(b, ["k"], max_rows=9_int64)
            print '(a,i0)', "the string-keyed join fitted under its ceiling, rows=", w%nrows()
            call a%join(b, ["k"], max_rows=8_int64)
        else
            call keyshape_tuple_fixture(a)
            call keyshape_tuple_fixture(b)
            call a%clone(w)
            call w%join(b, ["k1", "k2"], max_rows=9_int64)
            print '(a,i0)', "the two-column join fitted under its ceiling, rows=", w%nrows()
            call a%join(b, ["k1", "k2"], max_rows=8_int64)
        end if
        print '(a,i0)', "unexpectedly built an over-sized join, rows=", a%nrows()
    end subroutine scenario_join_max_rows_hash_keyshape

    !> Three rows under one string key, for the hash engine's string-key probe.
    subroutine keyshape_string_fixture(t)
        type(parquet_table), intent(out) :: t !! the table.
        call parquet_new_table(t)
        call t%add_column("k", [character(len=3) :: "aa", "aa", "aa"])
        call t%add_column("p", [1_int64, 2_int64, 3_int64])
    end subroutine keyshape_string_fixture

    !> Three rows under one COMPOSITE key, for the hash engine's tuple probe.
    subroutine keyshape_tuple_fixture(t)
        type(parquet_table), intent(out) :: t !! the table.
        call parquet_new_table(t)
        call t%add_column("k1", [1_int64, 1_int64, 1_int64])
        call t%add_column("k2", [2_int64, 2_int64, 2_int64])
        call t%add_column("p", [1_int64, 2_int64, 3_int64])
    end subroutine keyshape_tuple_fixture

    !> A DATE key offered to an index whose column is not a date, and the same for a TIME key.
    !!
    !! The refusal names the key class the CALLER asked for -- "a parquet_date", "a parquet_time"
    !! -- and each class has its own word in the message. `table_index_kind_mismatch_temporal`
    !! reaches the timestamp word by offering a timestamp to a date index; the date and time words
    !! are reached only by offering those, and a message that named the wrong type would send a
    !! caller looking at the wrong half of their code.
    !!
    !! The int32 column is the target in both halves precisely because it matches NEITHER, so the
    !! word under test cannot be the column's own. The matching lookup first is the control.
    subroutine scenario_table_index_date_key_on_int(which)
        character(len=*), intent(in) :: which !! "date" or "time".
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_date) :: d
        type(parquet_time) :: tm
        integer(int64) :: row

        call table_index_scenario_fixture(t)
        call t%build_index("id", ix)
        call ix%find(30_int32, row)
        print '(a,i0)', "control: the int key the index is built on was found, row=", row
        if (which == "date") then
            call d%set(2024, 6, 3)
            call ix%find(d, row)   ! -> aborts (a parquet_date key on an integer index)
        else
            call tm%set(12, 0, 0)
            call ix%find(tm, row)  ! -> aborts (a parquet_time key on an integer index)
        end if
        print '(a,i0)', "unexpectedly accepted a temporal key on an integer index, row=", row
    end subroutine scenario_table_index_date_key_on_int

    !> An INFINITE component in the disc centre is refused by its own guard, distinct from the
    !! NaN one above.
    !!
    !! The two are separate checks because they are separate mistakes, and because `max` does not
    !! propagate a NaN the way it propagates an infinity -- `scenario_healpix_disc_vector_nan`
    !! reaches the "holds a NaN" message and can never reach this one, which tests the largest
    !! COMPONENT for finiteness. That component test is deliberate: the centre is scale invariant,
    !! so a squared-length test would reject `[1e-300, 0, 1e-300]` as zero and raise
    !! IEEE_UNDERFLOW doing it. The finite vector accepted first is the control.
    subroutine scenario_healpix_disc_vector_infinite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        integer(int64) :: listpix(64), nlist
        real(real64) :: inf, centre(3)
        real(real64), parameter :: NORTH(3) = [0.0_real64, 0.0_real64, 1.0_real64]

        inf = ieee_value(0.0_real64, ieee_positive_inf)
        ! A radius that actually selects pixels: a control returning an empty disc would show
        ! only that the call did not abort, not that it did the work.
        call pf_query_disc(4_int64, NORTH, 0.5_real64, listpix, nlist)
        print '(a,i0)', "control: a finite centre vector was accepted, nlist=", nlist
        centre = [inf, 0.0_real64, 1.0_real64]
        call pf_query_disc(4_int64, centre, 0.1_real64, listpix, nlist)
        print '(a)', "unexpectedly accepted an infinite component in the centre vector"
    end subroutine scenario_healpix_disc_vector_infinite

    !
    ! ---- pf_integrate: every caller contract it refuses -------------------------------------
    !
    !> Each of these calls is the smallest one that provokes exactly one of `pf_integrate`'s
    !> aborts, and each prints the result afterwards. Printing it is what makes the scenario
    !> meaningful: a scenario that discarded the result would pass whether or not the guard fired,
    !> because the compiler may delete a call whose value nothing uses.
    subroutine scenario_integrate_negative_rtol()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, -1.0_real64)
        print '(a, es22.15)', "accepted a negative rtol: ", r
    end subroutine scenario_integrate_negative_rtol
    !
    !> See `scenario_integrate_negative_rtol`. A NaN tolerance is refused by the same guard, and
    !> is built with `ieee_value` rather than by arithmetic so the FIXTURE does not trap first.
    subroutine scenario_integrate_nan_rtol()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a, es22.15)', "accepted a NaN rtol: ", r
    end subroutine scenario_integrate_nan_rtol
    !
    !> A negative absolute tolerance is as meaningless as a negative relative one.
    subroutine scenario_integrate_negative_atol()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                         rtol=1.0e-8_real64, atol=-1.0_real64)
        print '(a, es22.15)', "accepted a negative atol: ", r
    end subroutine scenario_integrate_negative_atol
    !
    !> Both tolerances zero asks for an exact answer, which no quadrature can report reaching.
    subroutine scenario_integrate_zero_tolerances()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 0.0_real64)
        print '(a, es22.15)', "accepted a zero tolerance: ", r
    end subroutine scenario_integrate_zero_tolerances
    !
    !> QUADPACK's own floor: below `50*epsilon` a relative tolerance cannot be met by the
    !> arithmetic, and without a positive `atol` there is nothing else for convergence to rest on.
    subroutine scenario_integrate_rtol_below_floor()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-15_real64)
        print '(a, es22.15)', "accepted an rtol below the floor: ", r
    end subroutine scenario_integrate_rtol_below_floor
    !
    !> A budget of zero evaluations cannot buy the one rule application every call makes.
    subroutine scenario_integrate_bad_max_neval()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, max_neval=0)
        print '(a, es22.15)', "accepted a zero max_neval: ", r
    end subroutine scenario_integrate_bad_max_neval
    !
    !> Above `huge(1)/42` the subinterval count the budget implies does not fit in a default
    !> integer, so the budget is refused rather than silently overflowed.
    subroutine scenario_integrate_huge_max_neval()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, max_neval=huge(1))
        print '(a, es22.15)', "accepted a huge max_neval: ", r
    end subroutine scenario_integrate_huge_max_neval
    !
    !> A NaN bound has no ordering against the other, so neither the range nor its refusal could
    !> be decided; it is refused before anything else looks at the bounds.
    subroutine scenario_integrate_nan_bound()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: r

        r = pf_integrate(runge, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64, 1.0e-8_real64)
        print '(a, es22.15)', "accepted a NaN bound: ", r
    end subroutine scenario_integrate_nan_bound
    !
    !> Reversed bounds are refused rather than silently negated: a caller who wrote them the wrong
    !> way round meant something, and it was not the negative of the integral.
    subroutine scenario_integrate_reversed_bounds()
        real(real64) :: r

        r = pf_integrate(runge, 2.0_real64, 1.0_real64, 1.0e-8_real64)
        print '(a, es22.15)', "accepted reversed bounds: ", r
    end subroutine scenario_integrate_reversed_bounds
    !
    !> A walk needs at least one panel, so a cap of zero asks for an integral of nothing.
    subroutine scenario_integrate_bad_max_panels()
        real(real64) :: r

        r = pf_integrate(runge, 1.0_real64, pf_infinity(), 1.0e-8_real64, max_panels=0)
        print '(a, es22.15)', "accepted max_panels = 0: ", r
    end subroutine scenario_integrate_bad_max_panels
    !
    !> A finite range has no walk to cap, so `max_panels` on one is a call that does not mean what
    !> it says -- most often an infinite bound that did not survive the caller's own arithmetic.
    subroutine scenario_integrate_max_panels_finite()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, max_panels=3)
        print '(a, es22.15)', "accepted max_panels on a finite range: ", r
    end subroutine scenario_integrate_max_panels_finite
    !
    !> `[+inf, +inf]` is not a zero-width range but a range with no points in it at all, and its
    !> integral is not zero: it has no value. The finite `a == b` returns zero.
    subroutine scenario_integrate_same_infinity()
        real(real64) :: r

        r = pf_integrate(runge, pf_infinity(), pf_infinity(), 1.0e-8_real64)
        print '(a, es22.15)', "accepted both bounds as the same infinity: ", r
    end subroutine scenario_integrate_same_infinity
    !
    !> `log_base` substitutes `x = exp(u)` over the range the caller named, which needs two finite
    !> bounds; an infinite range is walked in `log x` already, by the walk's own transform.
    subroutine scenario_integrate_log_base_infinite()
        real(real64) :: r

        r = pf_integrate(runge, 1.0_real64, pf_infinity(), 1.0e-8_real64, log_base=.true.)
        print '(a, es22.15)', "accepted log_base on an infinite range: ", r
    end subroutine scenario_integrate_log_base_infinite
    !
    !> Integrating in `log x` needs a positive lower bound, because `log(0)` is where the
    !> transformed range would start.
    subroutine scenario_integrate_log_base_nonpositive()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, log_base=.true.)
        print '(a, es22.15)', "accepted log_base from a non-positive bound: ", r
    end subroutine scenario_integrate_log_base_nonpositive
    !
    !> A breakpoint is a point of the range, and a NaN or an infinity is not one: the sort would
    !> not order it and the piece it bounds would have no width to integrate over.
    subroutine scenario_integrate_breakpoints_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, &
                         breakpoints=[0.5_real64, ieee_value(1.0_real64, ieee_quiet_nan)])
        print '(a, es22.15)', "accepted a NaN breakpoint: ", r
    end subroutine scenario_integrate_breakpoints_nan
    !
    !> A breakpoint outside the range, or ON a bound, cuts nothing: the piece it would make is
    !> empty or lies where the caller never asked for an integral.
    subroutine scenario_integrate_breakpoints_outside()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, &
                         breakpoints=[1.5_real64])
        print '(a, es22.15)', "accepted a breakpoint outside the range: ", r
    end subroutine scenario_integrate_breakpoints_outside
    !
    !> Two equal breakpoints make a zero-width piece, which is a call that does not mean what it
    !> says rather than a range worth integrating.
    subroutine scenario_integrate_breakpoints_duplicate()
        real(real64) :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, &
                         breakpoints=[0.5_real64, 0.5_real64])
        print '(a, es22.15)', "accepted two equal breakpoints: ", r
    end subroutine scenario_integrate_breakpoints_duplicate
    !
    !> `context=` identifies the call site in the abort message, which is the whole of its job.
    subroutine scenario_integrate_context_reported()
        real(real64) :: r

        r = pf_integrate(runge, 2.0_real64, 1.0_real64, 1.0e-8_real64, context="my_call_site")
        print '(a, es22.15)', "accepted reversed bounds with a context: ", r
    end subroutine scenario_integrate_context_reported
    !
    !> Caller text inside a message is capped, so that a context built from a long path or a whole
    !> query cannot push the message itself out of a terminal or a log line.
    subroutine scenario_integrate_context_capped()
        real(real64) :: r

        r = pf_integrate(runge, 2.0_real64, 1.0_real64, 1.0e-8_real64, &
                         context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "accepted reversed bounds with a long context: ", r
    end subroutine scenario_integrate_context_capped
    !
    !> A table whose two halves differ in length: five abscissae, four ordinates. qfeet's module once
    !> sized its storage from one and assigned the other, and answered from the mismatch.
    subroutine scenario_interpolate_size_mismatch()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                    [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64])
        print '(a, l1)', "accepted x and y of different sizes, built: ", c%is_initialised()
    end subroutine scenario_interpolate_size_mismatch
    !
    !> A mask shorter than the table it masks.
    subroutine scenario_interpolate_is_valid_size()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                    [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64, 25.0_real64], &
                    is_valid=[.true., .true., .false., .true.])
        print '(a, l1)', "accepted a mask of the wrong size, built: ", c%is_initialised()
    end subroutine scenario_interpolate_is_valid_size
    !
    !> A method token the module does not know.
    subroutine scenario_interpolate_unknown_method()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], &
                    method="quadratic")
        print '(a, l1)', "accepted an unknown method, built: ", c%is_initialised()
    end subroutine scenario_interpolate_unknown_method
    !
    !> An out-of-range policy token the module does not know.
    subroutine scenario_interpolate_unknown_outside()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], &
                    outside="wrap")
        print '(a, l1)', "accepted an unknown outside policy, built: ", c%is_initialised()
    end subroutine scenario_interpolate_unknown_outside
    !
    !> A single point, which has no segment. qfeet's module once read past it and divided by zero.
    subroutine scenario_interpolate_too_few_points()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64], [5.0_real64])
        print '(a, l1)', "accepted a single point, built: ", c%is_initialised()
    end subroutine scenario_interpolate_too_few_points
    !
    !> Three points, two of them masked out: the count is judged after the mask, not before it.
    subroutine scenario_interpolate_too_few_after_is_valid()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], &
                    is_valid=[.false., .true., .false.])
        print '(a, l1)', "accepted one surviving point, built: ", c%is_initialised()
    end subroutine scenario_interpolate_too_few_after_is_valid
    !
    !> A repeated abscissa: a segment of zero width. qfeet's module once built this without complaint
    !> and then answered NaN from every evaluation.
    subroutine scenario_interpolate_repeated_x()
        type(pf_interp_1d) :: c

        call c%init([0.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 3.0_real64], &
                    [0.0_real64, 1.0_real64, 1.0_real64, 4.0_real64, 9.0_real64])
        print '(a, l1)', "accepted a repeated abscissa, built: ", c%is_initialised()
    end subroutine scenario_interpolate_repeated_x
    !
    !> Abscissae that are neither increasing nor decreasing.
    subroutine scenario_interpolate_unsorted_x()
        type(pf_interp_1d) :: c

        call c%init([0.0_real64, 2.0_real64, 1.0_real64, 3.0_real64], &
                    [0.0_real64, 4.0_real64, 1.0_real64, 9.0_real64])
        print '(a, l1)', "accepted unsorted abscissae, built: ", c%is_initialised()
    end subroutine scenario_interpolate_unsorted_x
    !
    !> A NaN abscissa, which compares false both ways and so fails the ordering; refused by that
    !> check's message, and before any ordered comparison is made against it.
    subroutine scenario_interpolate_nan_in_x()
        type(pf_interp_1d) :: c
        real(real64) :: x(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        x(3) = nan_value()
        call c%init(x, [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64, 25.0_real64])
        print '(a, l1)', "accepted a NaN abscissa, built: ", c%is_initialised()
    end subroutine scenario_interpolate_nan_in_x
    !
    !> An infinite end knot, which passes the ordering and would make its segment infinitely wide.
    subroutine scenario_interpolate_inf_in_x()
        type(pf_interp_1d) :: c
        real(real64) :: x(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        x(5) = positive_infinity()
        call c%init(x, [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64, 25.0_real64])
        print '(a, l1)', "accepted an infinite abscissa, built: ", c%is_initialised()
    end subroutine scenario_interpolate_inf_in_x
    !
    !> A NaN ordinate, which would reach the spline solve and every later evaluation.
    subroutine scenario_interpolate_nan_in_y()
        type(pf_interp_1d) :: c
        real(real64) :: y(5)

        y = [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64, 25.0_real64]
        y(3) = nan_value()
        call c%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], y)
        print '(a, l1)', "accepted a NaN ordinate, built: ", c%is_initialised()
    end subroutine scenario_interpolate_nan_in_y
    !
    !> An infinite ordinate, refused by the same check as a NaN one.
    subroutine scenario_interpolate_inf_in_y()
        type(pf_interp_1d) :: c
        real(real64) :: y(5)

        y = [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64, 25.0_real64]
        y(3) = positive_infinity()
        call c%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], y)
        print '(a, l1)', "accepted an infinite ordinate, built: ", c%is_initialised()
    end subroutine scenario_interpolate_inf_in_y
    !
    !> Evaluating an object that was never built. `%eval` is `pure`, so its result is printed: an
    !> unused pure call may be deleted, and the abort with it.
    !> %comoving_distance on a fresh object.
    subroutine scenario_cosmology_eval_before_init()
        type(pf_cosmology) :: c
        real(real64) :: v

        v = c%comoving_distance(1.0_real64)
        print '(a, es22.15)', "evaluated a cosmology that was never built: ", v
    end subroutine scenario_cosmology_eval_before_init
    !
    !> The same after `%clear`.
    subroutine scenario_cosmology_eval_after_clear()
        type(pf_cosmology) :: c
        real(real64) :: v

        call c%init("Planck18")
        call c%clear()
        v = c%age(0.0_real64)
        print '(a, es22.15)', "evaluated a cosmology that was cleared: ", v
    end subroutine scenario_cosmology_eval_after_clear
    !
    !> A name that is not one of the eight.
    subroutine scenario_cosmology_init_unknown_name()
        type(pf_cosmology) :: c

        call c%init("Plank18")
        print '(a, l1)', "accepted an unknown cosmology name, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_unknown_name
    !
    !> `h0` outside `[1e-10, 1e10]`.
    subroutine scenario_cosmology_init_h0_out_of_range()
        type(pf_cosmology) :: c

        call c%init(h0=1.0e11_real64, om0=0.3_real64)
        print '(a, l1)', "accepted an h0 above the range, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_h0_out_of_range
    !
    !> A negative `om0`.
    subroutine scenario_cosmology_init_om0_negative()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=-0.1_real64)
        print '(a, l1)', "accepted a negative om0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_om0_negative
    !
    !> A negative `tcmb0`.
    subroutine scenario_cosmology_init_tcmb0_negative()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, tcmb0=-1.0_real64)
        print '(a, l1)', "accepted a negative tcmb0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_tcmb0_negative
    !
    !> `m_nu` carrying other than `floor(neff)` masses.
    subroutine scenario_cosmology_init_m_nu_size()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, neff=3.046_real64, &
                    m_nu=[0.0_real64, 0.06_real64])
        print '(a, l1)', "accepted an m_nu of the wrong size, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_m_nu_size
    !
    !> `w0` outside `[-3, 3]`.
    subroutine scenario_cosmology_init_w0_out_of_range()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, w0=-4.0_real64)
        print '(a, l1)', "accepted a w0 below the range, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_w0_out_of_range
    !
    !> `wa` outside `[-3, 3]`. Its own branch, not `w0`'s: the two are validated separately and a
    !> guard written for one alone lets the other through.
    subroutine scenario_cosmology_init_wa_out_of_range()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, wa=4.0_real64)
        print '(a, l1)', "accepted a wa above the range, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_wa_out_of_range
    !
    !> An `ode0` that is not a number. Unlike every other density it has no RANGE -- a negative
    !> `ode0` is an admitted model -- so finiteness is the whole of its guard.
    subroutine scenario_cosmology_init_ode0_not_finite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_cosmology) :: c
        real(real64) :: bad

        bad = ieee_value(bad, ieee_quiet_nan)
        call c%init(h0=70.0_real64, om0=0.3_real64, ode0=bad)
        print '(a, l1)', "accepted a NaN ode0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_ode0_not_finite
    !
    !> A negative `neff`. `floor(neff)` becomes the species count, so a negative one would
    !> allocate nothing and every neutrino sum would run over an empty range.
    subroutine scenario_cosmology_init_neff_negative()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, neff=-1.0_real64)
        print '(a, l1)', "accepted a negative neff, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_neff_negative
    !
    !> An `m_nu` of the right LENGTH carrying a negative mass: the per-entry guard, which the
    !> length guard beside it does not cover.
    subroutine scenario_cosmology_init_m_nu_negative()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, neff=3.046_real64, &
                    m_nu=[0.0_real64, -0.1_real64, 0.0_real64])
        print '(a, l1)', "accepted a negative neutrino mass, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_m_nu_negative
    !
    !> A 150-character `context` is reproduced to its first 100 characters and elided.
    subroutine scenario_cosmology_init_context_capped()
        type(pf_cosmology) :: c

        call c%init(h0=-1.0_real64, om0=0.3_real64, context=repeat("abcdefghij", 15))
        print '(a, l1)', "accepted a negative h0 with a long context, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_context_capped
    !
    !> `zmax` above `1e10`.
    subroutine scenario_cosmology_init_zmax_out_of_range()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, zmax=1.0e11_real64)
        print '(a, l1)', "accepted a zmax above the ceiling, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_zmax_out_of_range
    !
    !> `zmin` at or below `-1`, where no redshift exists.
    subroutine scenario_cosmology_init_zmin_out_of_range()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, zmin=-1.0_real64)
        print '(a, l1)', "accepted a zmin at the pole, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_zmin_out_of_range
    !
    !> `ob0` greater than `om0`.
    subroutine scenario_cosmology_init_ob0_above_om0()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, ob0=0.4_real64)
        print '(a, l1)', "accepted an ob0 above om0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_ob0_above_om0
    !
    !> `ode0 = 1e300` passes every per-argument check and is refused by the DERIVED one.

    !! Without that check this is an `IEEE_OVERFLOW` inside `E^2` at the redshift ceiling, which
    !! ends the process under nagfor and is silent under gfortran and ifx.
    subroutine scenario_cosmology_init_density_too_large()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, ode0=1.0e300_real64)
        print '(a, l1)', "accepted an absurd ode0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_density_too_large
    !
    !> `tcmb0 = 1e5` K, refused through `ogamma0` rather than a ceiling on `tcmb0`.

    !! The negative control proving the check is on the DERIVED value.
    subroutine scenario_cosmology_init_tcmb0_too_hot()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, tcmb0=1.0e5_real64)
        print '(a, l1)', "accepted an absurd tcmb0, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_tcmb0_too_hot
    !
    !> A bouncing closed model whose `E^2` is negative on the table's range.
    subroutine scenario_cosmology_init_no_big_bang()
        type(pf_cosmology) :: c

        call c%init(h0=70.0_real64, om0=0.3_real64, ode0=2.0_real64)
        print '(a, l1)', "accepted a cosmology with no big bang, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_no_big_bang
    !
    !> An interval budget too small to converge, through the test-only hook.

    !! `parquet_debug_set_cosmology_max_neval` is public for this alone; the suite's negative
    !! control is every other `%init` here, made with the hook clear.
    subroutine scenario_cosmology_init_table_not_converged()
        type(pf_cosmology) :: c

        call parquet_debug_set_cosmology_max_neval(21)
        call c%init("Planck18")
        print '(a, l1)', "accepted a table that did not converge, built: ", c%is_initialised()
    end subroutine scenario_cosmology_init_table_not_converged
    !
    ! ---- parquet_cosmology_config: the [cosmology] section of a configuration file ------------
    !
    !> Every fixture below is a STRING rather than a file: scenarios run concurrently, one process
    !> per name, so a string needs neither a unique path nor cleanup. `name = "run.toml"` is what
    !> the messages quote, and it is what proves the context this module composes reaches them.

    !> A configuration with no `[cosmology]` section, and no `found=` to make it optional.
    subroutine scenario_cosmology_config_missing_section()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[general]' // nl // 'nproc = 1' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted a configuration with no [cosmology] section, built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_missing_section
    !
    !> `name` naming one of the eight BESIDE a model parameter says two different things.
    subroutine scenario_cosmology_config_named_with_parameters()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[cosmology]' // nl // 'name = "Planck18"' // nl // &
                                 'om0 = 0.25' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted a named cosmology given parameters too, built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_named_with_parameters
    !
    !> A `[[cosmology]]` ARRAY of tables is not this section -- and `found=` does not soften it.
    !!
    !! `found=` is the opt-out for an ABSENT section. Reaching the refusal WITH it present is the
    !! whole point: absence is a configuration choice and a name of the wrong shape is a
    !! programming error, so only the first is answerable. A run that printed instead of aborting
    !! would mean the shape had become answerable too.
    subroutine scenario_cosmology_config_array_of_tables()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        logical :: there
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[general]' // nl // 'nproc = 1' // nl // &
                                 '[[cosmology]]' // nl // 'name = "Planck18"' // nl // &
                                 '[[cosmology]]' // nl // 'name = "WMAP9"' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c, found = there)
        print '(a, l1, a, l1)', "accepted a [[cosmology]] array of tables, found: ", there, &
            ", built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_array_of_tables
    !
    !> A key of the wrong type is `parquet_toml`'s abort, with the offending line quoted.
    subroutine scenario_cosmology_config_bad_type()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[cosmology]' // nl // 'h0 = "seventy"' // nl // &
                                 'om0 = 0.3' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted h0 as a string, built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_bad_type
    !
    !> An `m_nu` of the wrong length is `%init`'s abort, reached through the file.
    subroutine scenario_cosmology_config_mnu_length()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[cosmology]' // nl // 'h0 = 70.0' // nl // 'om0 = 0.3' // nl // &
                                 'neff = 3.046' // nl // 'm_nu = [0.0, 0.06]' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted an m_nu of the wrong size from a file, built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_mnu_length
    !
    !> A parameter outside its range is `%init`'s abort, carrying THIS module's context: the file
    !> and the section, so the message says where the bad number came from.
    subroutine scenario_cosmology_config_out_of_range()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[cosmology]' // nl // 'h0 = -1.0' // nl // 'om0 = 0.3' // nl, &
                           name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted a negative h0 from a file, built: ", c%is_initialised()
    end subroutine scenario_cosmology_config_out_of_range
    !
    !> A `name` that is not one of the eight, in a section that gives neither `h0` nor `om0`.
    !!
    !! Almost always a MISSPELT cosmology, so it gets a message that lists the eight rather than
    !! `pf_toml_require`'s "these keys are missing", which would send the reader looking for the
    !! parameters they never meant to give.
    subroutine scenario_cosmology_config_label_without_parameters()
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=1) :: nl

        nl = new_line("a")
        call pf_toml_loads(conf, '[cosmology]' // nl // 'name = "Plank18"' // nl // &
                                 'zmax = 50.0' // nl, name = "run.toml")
        call pf_cosmology_from_toml(conf, c)
        print '(a, l1)', "accepted a misspelt cosmology name with no parameters, built: ", &
            c%is_initialised()
    end subroutine scenario_cosmology_config_label_without_parameters
    !
    subroutine scenario_interpolate_eval_before_init()
        type(pf_interp_1d) :: c
        real(real64) :: v

        v = c%eval(1.0_real64)
        print '(a, es22.15)', "evaluated an interpolant that was never built: ", v
    end subroutine scenario_interpolate_eval_before_init
    !
    !> `context=` identifies the call site in the abort message.
    subroutine scenario_interpolate_context_reported()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64], context="my_call_site")
        print '(a, l1)', "accepted x and y of different sizes with a context, built: ", c%is_initialised()
    end subroutine scenario_interpolate_context_reported
    !
    !> A 150-character context is reproduced to its first 100 characters and elided.
    subroutine scenario_interpolate_context_capped()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64], &
                    context=repeat("abcdefghij", 15))
        print '(a, l1)', "accepted x and y of different sizes with a long context, built: ", c%is_initialised()
    end subroutine scenario_interpolate_context_capped
    !
    !> The one-shot form validates through the same body under its own name.
    subroutine scenario_interpolate_one_shot_size_mismatch()
        real(real64) :: v

        v = pf_interp([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64], 1.5_real64)
        print '(a, es22.15)', "accepted x and y of different sizes in one shot: ", v
    end subroutine scenario_interpolate_one_shot_size_mismatch
    !
    !> An end condition given with a method that has none: straight lines have no end rows.
    subroutine scenario_interpolate_bc_with_linear()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], &
                    method="linear", bc="natural")
        print '(a, l1)', "accepted an end condition for linear interpolation, built: ", c%is_initialised()
    end subroutine scenario_interpolate_bc_with_linear
    !
    !> An end-condition token the module does not know.
    subroutine scenario_interpolate_unknown_bc()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="periodic")
        print '(a, l1)', "accepted an unknown end condition, built: ", c%is_initialised()
    end subroutine scenario_interpolate_unknown_bc
    !
    !> The clamped end condition with no slopes to clamp to.
    subroutine scenario_interpolate_clamped_without_slopes()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="clamped")
        print '(a, l1)', "accepted bc=""clamped"" without slopes, built: ", c%is_initialised()
    end subroutine scenario_interpolate_clamped_without_slopes
    !
    !> End slopes given with the natural end condition, which would silently ignore them.
    subroutine scenario_interpolate_slopes_without_clamped()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="natural", &
                    slopes=[0.0_real64, 0.0_real64])
        print '(a, l1)', "accepted slopes with bc=""natural"", built: ", c%is_initialised()
    end subroutine scenario_interpolate_slopes_without_clamped
    !
    !> One slope for two ends: the second would be read past the end of the array.
    subroutine scenario_interpolate_slopes_size()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="clamped", &
                    slopes=[2.0_real64])
        print '(a, l1)', "accepted one slope for two ends, built: ", c%is_initialised()
    end subroutine scenario_interpolate_slopes_size
    !
    !> A NaN end slope, which would reach the spline solve and every later evaluation.
    subroutine scenario_interpolate_slopes_nan()
        type(pf_interp_1d) :: c
        real(real64) :: slopes(2)

        slopes = [0.0_real64, 0.0_real64]
        slopes(1) = nan_value()
        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="clamped", &
                    slopes=slopes)
        print '(a, l1)', "accepted a NaN end slope, built: ", c%is_initialised()
    end subroutine scenario_interpolate_slopes_nan
    !
    !> An infinite end slope, refused by the same check as a NaN one.
    subroutine scenario_interpolate_slopes_inf()
        type(pf_interp_1d) :: c
        real(real64) :: slopes(2)

        slopes = [0.0_real64, 0.0_real64]
        slopes(2) = positive_infinity()
        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="clamped", &
                    slopes=slopes)
        print '(a, l1)', "accepted an infinite end slope, built: ", c%is_initialised()
    end subroutine scenario_interpolate_slopes_inf
    !
    !> Three points for a not-a-knot spline, whose two end conditions are distinct rows only from four.
    subroutine scenario_interpolate_not_a_knot_too_few()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], bc="not_a_knot")
        print '(a, l1)', "accepted three points for bc=""not_a_knot"", built: ", c%is_initialised()
    end subroutine scenario_interpolate_not_a_knot_too_few
    !
    !> ONE point for `method="linear"`. Two is the floor for every method but the not-a-knot spline,
    !> and the message names the method the caller asked for rather than the floor alone -- so each
    !> method has its own arm, and each arm needs its own scenario to be seen at all.
    subroutine scenario_interpolate_linear_too_few()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64], [1.0_real64], method="linear")
        print '(a, l1)', "accepted one point for method=""linear"", built: ", c%is_initialised()
    end subroutine scenario_interpolate_linear_too_few
    !
    !> The same for `method="pchip"`, whose slopes need a segment to be slopes of.
    subroutine scenario_interpolate_pchip_too_few()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64], [1.0_real64], method="pchip")
        print '(a, l1)', "accepted one point for method=""pchip"", built: ", c%is_initialised()
    end subroutine scenario_interpolate_pchip_too_few
    !
    !> Differentiating an object that was never built. `%derivative` is `pure`, so its result is
    !> printed: an unused pure call may be deleted, and the abort with it.
    subroutine scenario_interpolate_derivative_before_init()
        type(pf_interp_1d) :: c
        real(real64) :: v

        v = c%derivative(1.0_real64)
        print '(a, es22.15)', "differentiated an interpolant that was never built: ", v
    end subroutine scenario_interpolate_derivative_before_init
    !
    !> Integrating an object that was never built. `%integral` is `pure`, so its result is printed.
    subroutine scenario_interpolate_integral_before_init()
        type(pf_interp_1d) :: c
        real(real64) :: v

        v = c%integral(0.0_real64, 1.0_real64)
        print '(a, es22.15)', "integrated an interpolant that was never built: ", v
    end subroutine scenario_interpolate_integral_before_init
    !
    !> A third derivative, which no interpolant here offers. `%derivative` is `pure`, so its result is
    !> printed.
    subroutine scenario_interpolate_bad_order()
        type(pf_interp_1d) :: c
        real(real64) :: v

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64])
        v = c%derivative(1.5_real64, order=3)
        print '(a, es22.15)', "accepted a third derivative: ", v
    end subroutine scenario_interpolate_bad_order
    !
    !> Values laid out for the transposed grid: seven rows of five for five lines along x and seven
    !> along y, which the shape check refuses whenever the two axes differ in length.
    subroutine scenario_interpolate_2d_shape()
        type(pf_interp_2d) :: g
        real(real64) :: z(7, 5)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                    [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64], z)
        print '(a, l1)', "accepted values shaped for the transposed grid, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_shape
    !
    !> A method token the grid does not know, answered with the grid's own list of methods.
    subroutine scenario_interpolate_2d_unknown_method()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, method="quadratic")
        print '(a, l1)', "accepted an unknown grid method, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_unknown_method
    !
    !> The shape-preserving method, which has no grid form: a surface is not shape-preserving because
    !> its curves along each axis are.
    subroutine scenario_interpolate_2d_pchip()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, method="pchip")
        print '(a, l1)', "accepted method=""pchip"" on a grid, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_pchip
    !
    !> An end condition given with bilinear interpolation, which has none.
    subroutine scenario_interpolate_2d_bc_with_linear()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, method="linear", &
                    bc="natural")
        print '(a, l1)', "accepted an end condition for bilinear interpolation, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_bc_with_linear
    !
    !> An end-condition token the grid does not know, answered with the grid's own list.
    subroutine scenario_interpolate_2d_unknown_bc()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, bc="periodic")
        print '(a, l1)', "accepted an unknown grid end condition, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_unknown_bc
    !
    !> The clamped end condition, which has no grid form: it would need slopes along every edge.
    subroutine scenario_interpolate_2d_clamped()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, bc="clamped")
        print '(a, l1)', "accepted bc=""clamped"" on a grid, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_clamped
    !
    !> An out-of-range policy token the grid does not know.
    subroutine scenario_interpolate_2d_unknown_outside()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, outside="wrap")
        print '(a, l1)', "accepted an unknown grid outside policy, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_unknown_outside
    !
    !> A single line along x, which has no cell.
    subroutine scenario_interpolate_2d_too_few_x()
        type(pf_interp_2d) :: g
        real(real64) :: z(1, 3)

        z = 1.0_real64
        call g%init([1.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z)
        print '(a, l1)', "accepted one line along x, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_too_few_x
    !
    !> Three lines along y for a not-a-knot spline, which needs four along each axis; the five along x
    !> pass.
    subroutine scenario_interpolate_2d_not_a_knot_too_few()
        type(pf_interp_2d) :: g
        real(real64) :: z(5, 3)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, &
                    bc="not_a_knot")
        print '(a, l1)', "accepted three lines along y for bc=""not_a_knot"", built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_not_a_knot_too_few
    !
    !> Lines along x that are neither increasing nor decreasing.
    subroutine scenario_interpolate_2d_x_not_monotonic()
        type(pf_interp_2d) :: g
        real(real64) :: z(4, 3)

        z = 1.0_real64
        call g%init([0.0_real64, 2.0_real64, 1.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z)
        print '(a, l1)', "accepted unsorted lines along x, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_x_not_monotonic
    !
    !> A repeated line along y: a row of cells of zero height.
    subroutine scenario_interpolate_2d_y_not_monotonic()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 4)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [0.0_real64, 1.0_real64, 1.0_real64, 2.0_real64], z)
        print '(a, l1)', "accepted a repeated line along y, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_y_not_monotonic
    !
    !> An infinite last line along x, which passes the ordering and would make its cells infinitely wide.
    subroutine scenario_interpolate_2d_inf_in_x()
        type(pf_interp_2d) :: g
        real(real64) :: x(4), z(4, 3)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        x(4) = positive_infinity()
        z = 1.0_real64
        call g%init(x, [1.0_real64, 2.0_real64, 3.0_real64], z)
        print '(a, l1)', "accepted an infinite line along x, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_inf_in_x
    !
    !> An infinite last line along y, refused by its own axis's check.
    subroutine scenario_interpolate_2d_inf_in_y()
        type(pf_interp_2d) :: g
        real(real64) :: y(4), z(3, 4)

        y = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        y(4) = positive_infinity()
        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], y, z)
        print '(a, l1)', "accepted an infinite line along y, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_inf_in_y
    !
    !> A NaN value, which would reach the tables of second derivatives and every later evaluation.
    subroutine scenario_interpolate_2d_nan_in_z()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 4)

        z = 1.0_real64
        z(2, 3) = nan_value()
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], z)
        print '(a, l1)', "accepted a NaN grid value, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_nan_in_z
    !
    !> Evaluating a grid object that was never built. `%eval` is `pure`, so its result is printed: an
    !> unused pure call may be deleted, and the abort with it.
    subroutine scenario_interpolate_2d_eval_before_init()
        type(pf_interp_2d) :: g
        real(real64) :: v

        v = g%eval(1.0_real64, 1.0_real64)
        print '(a, es22.15)', "evaluated a grid interpolant that was never built: ", v
    end subroutine scenario_interpolate_2d_eval_before_init
    !
    !> The grid's one-shot form validates through the same body under its own name, and carries the
    !> caller's context.
    subroutine scenario_interpolate_2d_one_shot_shape()
        real(real64) :: z(7, 5), v

        z = 1.0_real64
        v = pf_interp([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                      [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64], z, &
                      1.5_real64, 1.5_real64, context="my_grid")
        print '(a, es22.15)', "accepted values shaped for the transposed grid in one shot: ", v
    end subroutine scenario_interpolate_2d_one_shot_shape
    !
    !> Three x coordinates and two y coordinates: no query can be formed from the third x.
    subroutine scenario_interpolate_2d_query_sizes()
        real(real64) :: z(3, 3), v(3)

        z = 1.0_real64
        v = pf_interp([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 2.0_real64, 3.0_real64], z, &
                      [1.5_real64, 2.0_real64, 2.5_real64], [1.5_real64, 2.5_real64])
        print '(a, 3es22.15)', "accepted query coordinates of different sizes: ", v
    end subroutine scenario_interpolate_2d_query_sizes
    !
    !> A cubic spline over knots `1e160` apart: its segment formula squares the spacing, which
    !> overflows, so every value would be a NaN. Nothing overflows before the refusal, so the scenario
    !> reaches it under nagfor's default `-ieee=stop` too.
    subroutine scenario_interpolate_spline_too_wide()
        type(pf_interp_1d) :: c

        call c%init([0.0_real64, 1.0e160_real64, 2.0e160_real64, 3.0e160_real64], &
                    [0.0_real64, 1.0_real64, 0.0_real64, 1.0_real64])
        print '(a, l1)', "accepted knots too far apart for a cubic spline, built: ", c%is_initialised()
    end subroutine scenario_interpolate_spline_too_wide
    !
    !> A cubic spline over knots `1e-160` apart and ordinates of order one: its second derivatives are
    !> about `1e320`, which overflow in the solve.
    subroutine scenario_interpolate_spline_overflows()
        type(pf_interp_1d) :: c

        call c%init([0.0_real64, 1.0e-160_real64, 2.0e-160_real64, 3.0e-160_real64], &
                    [0.0_real64, 1.0_real64, 0.0_real64, 1.0_real64])
        print '(a, l1)', "accepted a cubic spline whose second derivatives overflow, built: ", c%is_initialised()
    end subroutine scenario_interpolate_spline_overflows
    !
    !> A bicubic grid whose lines along x lie `1e160` apart, refused along x.
    subroutine scenario_interpolate_2d_spline_too_wide_x()
        type(pf_interp_2d) :: g
        real(real64) :: z(4, 3)

        z = 1.0_real64
        call g%init([0.0_real64, 1.0e160_real64, 2.0e160_real64, 3.0e160_real64], [1.0_real64, 2.0_real64, 3.0_real64], z)
        print '(a, l1)', "accepted grid lines along x too far apart for a bicubic spline, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_spline_too_wide_x
    !
    !> A bicubic grid whose lines along y lie `1e160` apart, refused by its own axis's check.
    subroutine scenario_interpolate_2d_spline_too_wide_y()
        type(pf_interp_2d) :: g
        real(real64) :: z(3, 4)

        z = 1.0_real64
        call g%init([1.0_real64, 2.0_real64, 3.0_real64], [0.0_real64, 1.0e160_real64, 2.0e160_real64, 3.0e160_real64], z)
        print '(a, l1)', "accepted grid lines along y too far apart for a bicubic spline, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_spline_too_wide_y
    !
    !> A bicubic grid whose lines along x lie `1e-160` apart, over values alternating between 0 and 1
    !> along x: the second derivatives along x overflow in the solve.
    subroutine scenario_interpolate_2d_spline_overflows()
        type(pf_interp_2d) :: g
        real(real64) :: z(4, 3)
        integer :: i

        do i = 1, 4
            z(i, :) = real(mod(i, 2), real64)
        end do
        call g%init([0.0_real64, 1.0e-160_real64, 2.0e-160_real64, 3.0e-160_real64], [1.0_real64, 2.0_real64, 3.0_real64], z)
        print '(a, l1)', "accepted a bicubic spline whose second derivatives overflow, built: ", g%is_initialised()
    end subroutine scenario_interpolate_2d_spline_overflows
    !
    !> A method token longer than any name can be, with blanks inside it: `"linear"`, a hundred blanks
    !> and an `x`, which a fold keeping the first hundred and one characters trimmed to `"linear"`.
    subroutine scenario_interpolate_long_token()
        type(pf_interp_1d) :: c

        call c%init([1.0_real64, 2.0_real64, 3.0_real64], [1.0_real64, 4.0_real64, 9.0_real64], &
                    method="linear"//repeat(" ", 100)//"x")
        print '(a, l1)', "accepted an over-long method token with blanks inside it, built: ", c%is_initialised()
    end subroutine scenario_interpolate_long_token
    !
    !> Evaluating an object that was never built at a rank-1 array holding no query, which is refused
    !> as a single query is, although there is nothing to evaluate. An elemental `%eval` would be
    !> invoked for no element and answer an empty array. The answers' count is printed, which uses the
    !> result of the `pure` call.
    subroutine scenario_interpolate_eval_array_before_init()
        type(pf_interp_1d) :: c
        real(real64) :: none(0)
        real(real64), allocatable :: v(:)

        v = c%eval(none)
        print '(a, i0)', "evaluated an interpolant that was never built at no queries, answers: ", size(v)
    end subroutine scenario_interpolate_eval_array_before_init
    !
    !> A zero-length start point: the simplex needs at least one variable.
    subroutine scenario_optimize_size_zero()
        real(real64) :: x(0), step(0), fmin

        call pf_minimize_simplex(sphere, x, fmin, step, 1.0e-8_real64)
        print '(a, es22.15)', "accepted a zero-length start point: ", fmin
    end subroutine scenario_optimize_size_zero

    !> A zero evaluation budget.
    subroutine scenario_optimize_budget_zero()
        real(real64) :: x(1), fmin

        x = 5.0_real64
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64], 1.0e-8_real64, max_neval=0)
        print '(a, es22.15)', "accepted a zero max_neval: ", fmin
    end subroutine scenario_optimize_budget_zero

    !> A budget above `huge(1)/2`, which would let a count overflow its default integer.
    subroutine scenario_optimize_budget_ceiling()
        real(real64) :: x(1), fmin

        x = 5.0_real64
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64], 1.0e-8_real64, max_neval=huge(1))
        print '(a, es22.15)', "accepted a max_neval above huge(1)/2: ", fmin
    end subroutine scenario_optimize_budget_ceiling

    !> A NaN tolerance, the case that would otherwise burn the whole budget silently.
    subroutine scenario_optimize_tolerance_nonfinite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(1), fmin, bad_ftol

        x = 5.0_real64
        bad_ftol = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64], bad_ftol)
        print '(a, es22.15)', "accepted a NaN rtol: ", fmin
    end subroutine scenario_optimize_tolerance_nonfinite

    !> A bracket whose ends are the wrong way round.
    subroutine scenario_optimize_scalar_bad_bracket()
        real(real64) :: x, fmin

        call pf_minimize_scalar(quad1d, 2.0_real64, -2.0_real64, x, fmin)
        print '(a, 2es22.15)', "accepted a reversed bracket: ", x, fmin
    end subroutine scenario_optimize_scalar_bad_bracket

    !> A bracket whose ends are finite but whose WIDTH is not, which `b - a` overflows on.
    !!
    !! The engine's own tolerance is measured against that width, and every quantity it forms from
    !! the bracket inherits the infinity; the objective is then blamed for a value it never
    !! returned. Refused before the first evaluation instead.
    subroutine scenario_optimize_scalar_bracket_width()
        real(real64) :: x, fmin

        call pf_minimize_scalar(quad1d, -1.0e308_real64, 1.0e308_real64, x, fmin)
        print '(a, 2es22.15)', "accepted a bracket whose width overflows: ", x, fmin
    end subroutine scenario_optimize_scalar_bracket_width

    !> An objective that is NaN everywhere, refused by the scalar engine before it compares.
    subroutine scenario_optimize_scalar_nonfinite_value()
        real(real64) :: x, fmin

        call pf_minimize_scalar(always_nan, -2.0_real64, 2.0_real64, x, fmin)
        print '(a, 2es22.15)', "accepted a non-finite objective value: ", x, fmin
    end subroutine scenario_optimize_scalar_nonfinite_value

    !> A constrained objective handed to Brent, which honours no constraint.
    subroutine scenario_optimize_scalar_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x, fmin

        call pf_minimize_scalar(obj, -2.0_real64, 2.0_real64, x, fmin)
        print '(a, 2es22.15)', "accepted a constrained objective in Brent: ", x, fmin
    end subroutine scenario_optimize_scalar_constraints_not_honoured

    !> A step with a zero element, which would put two vertices on top of each other.
    subroutine scenario_optimize_simplex_step_zero()
        real(real64) :: x(2), fmin

        x = 5.0_real64
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64, 0.0_real64], 1.0e-8_real64)
        print '(a, es22.15)', "accepted a zero step element: ", fmin
    end subroutine scenario_optimize_simplex_step_zero

    !> A step of a different length from the start point.
    subroutine scenario_optimize_simplex_step_size()
        real(real64) :: x(2), fmin

        x = 5.0_real64
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64], 1.0e-8_real64)
        print '(a, es22.15)', "accepted a step of the wrong size: ", fmin
    end subroutine scenario_optimize_simplex_step_size

    !> Both tolerances zero, so no convergence test could ever fire.
    subroutine scenario_optimize_simplex_no_tolerance()
        real(real64) :: x(1), fmin

        x = 5.0_real64
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64], 0.0_real64, atol=0.0_real64)
        print '(a, es22.15)', "accepted two zero tolerances: ", fmin
    end subroutine scenario_optimize_simplex_no_tolerance

    !> A NaN in the start point, which would make every simplex comparison meaningless.
    subroutine scenario_optimize_simplex_nan_start()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(2), fmin

        x(1) = 5.0_real64
        x(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_minimize_simplex(sphere, x, fmin, [0.5_real64, 0.5_real64], 1.0e-8_real64)
        print '(a, es22.15)', "accepted a NaN start point: ", fmin
    end subroutine scenario_optimize_simplex_nan_start

    !> An objective that turns NaN only once the simplex steps outside a region.
    !!
    !! The starting simplex is well inside it, so this is the MID-RUN abort: a screen that ran
    !! only over the starting simplex, as qfeet's does, would not see it.
    subroutine scenario_optimize_simplex_nonfinite_value()
        real(real64) :: x(2), fmin

        x = [0.0_real64, 0.0_real64]
        call pf_minimize_simplex(nan_beyond_two, x, fmin, [0.5_real64, 0.5_real64], 0.0_real64, &
                                 atol=1.0e-12_real64)
        print '(a, es22.15)', "accepted a non-finite value mid-run: ", fmin
    end subroutine scenario_optimize_simplex_nonfinite_value

    !> A constrained objective handed to the simplex, which honours no constraint.
    subroutine scenario_optimize_simplex_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x(2), fmin

        x = 0.0_real64
        call pf_minimize_simplex(obj, x, fmin, [0.5_real64, 0.5_real64], 1.0e-8_real64)
        print '(a, es22.15)', "accepted a constrained objective in the simplex: ", fmin
    end subroutine scenario_optimize_simplex_constraints_not_honoured

    !> A zero `threads=` request, which asks for no team at all.
    subroutine scenario_optimize_threads_zero()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, threads=0)
        print '(a, es22.15)', "accepted threads = 0: ", fmin
    end subroutine scenario_optimize_threads_zero

    !> A box whose corners are a different length from the point.
    subroutine scenario_optimize_de_bounds_size()
        real(real64) :: x(3), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin)
        print '(a, es22.15)', "accepted a box of the wrong size: ", fmin
    end subroutine scenario_optimize_de_bounds_size

    !> A box with a lower bound at or above its upper bound, which has no interior.
    subroutine scenario_optimize_de_bounds_order()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = [-2.0_real64, 3.0_real64]
        hi = [2.0_real64, 3.0_real64]
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin)
        print '(a, es22.15)', "accepted a lower bound not below its upper bound: ", fmin
    end subroutine scenario_optimize_de_bounds_order

    !> Both tolerances at zero, which asks the search to stop at an accuracy it cannot reach.
    !!
    !! Each is separately legal -- `validate_tolerance` admits zero, so that a caller may drive
    !! the stop on the other one alone -- and it is the PAIR that is refused. Without the
    !! refusal the run would spend its whole generation budget and report a limit stop, which
    !! looks like a hard problem rather than a contradictory request.
    subroutine scenario_optimize_de_no_tolerance()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, rtol=0.0_real64, atol=0.0_real64)
        print '(a, es22.15)', "accepted rtol and atol both zero: ", fmin
    end subroutine scenario_optimize_de_no_tolerance

    !> A `parquet_optimize` refusal whose message carries the caller's `context`.
    !!
    !! `optimize_abort` is this tier's shared abort, below `pf_minimize_de`, `_simplex`,
    !! `_scalar` and `_multistart`; its context-carrying arm is a different line from the
    !! plain one, and the `parquet_prima` tier has its own `prima_abort` covered separately.
    subroutine scenario_optimize_context_is_reported()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = [-2.0_real64, 3.0_real64]
        hi = [2.0_real64, 3.0_real64]
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, context="sweeping the grid")
        print '(a, es22.15)', "accepted a degenerate box, with context: ", fmin
    end subroutine scenario_optimize_context_is_reported

    !> A `context` longer than the hundred characters `optimize_abort` carries, truncated.
    subroutine scenario_optimize_context_is_capped()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        ! 150 characters, and the cap falls on a repeat boundary, so the truncated message ends
        ! with a whole "abcdefghij" followed by the ellipsis -- which is what the test asserts.
        lo = [-2.0_real64, 3.0_real64]
        hi = [2.0_real64, 3.0_real64]
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "accepted a degenerate box, with a long context: ", fmin
    end subroutine scenario_optimize_context_is_capped

    !> An infinite bound, which no Latin hypercube can be laid out over.
    subroutine scenario_optimize_de_bounds_nonfinite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi(1) = 2.0_real64
        hi(2) = ieee_value(1.0_real64, ieee_positive_inf)
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin)
        print '(a, es22.15)', "accepted an infinite bound: ", fmin
    end subroutine scenario_optimize_de_bounds_nonfinite

    !> Bounds that are finite but whose WIDTH is not, which no population can be laid out over.
    !!
    !! Every stratum of the Latin hypercube is a fraction of `upper - lower`, so an infinite width
    !! puts the whole population at infinity, where every value is equal: the spread test fires at
    !! once and the run reports convergence at a point no objective was meaningfully asked about.
    subroutine scenario_optimize_de_box_width()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -1.0e308_real64
        hi = 1.0e308_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin)
        print '(a, es22.15)', "accepted a box whose width overflows: ", fmin
    end subroutine scenario_optimize_de_box_width

    !> A population too small for DE/rand/1, which needs three donors distinct from the target.
    subroutine scenario_optimize_de_np_small()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, np=3)
        print '(a, es22.15)', "accepted a population of three: ", fmin
    end subroutine scenario_optimize_de_np_small

    !> A differential weight outside `(0, 2]`, where the mutation would be no mutation at all.
    subroutine scenario_optimize_de_f_weight_range()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, f_weight=0.0_real64)
        print '(a, es22.15)', "accepted a zero differential weight: ", fmin
    end subroutine scenario_optimize_de_f_weight_range

    !> A crossover probability outside `[0, 1]`.
    subroutine scenario_optimize_de_cr_range()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, cr=1.5_real64)
        print '(a, es22.15)', "accepted a crossover probability above one: ", fmin
    end subroutine scenario_optimize_de_cr_range

    !> A generation budget of zero, which is not a run.
    subroutine scenario_optimize_de_max_gen_zero()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(sphere, lo, hi, 1_int64, x, fmin, max_gen=0)
        print '(a, es22.15)', "accepted a zero generation budget: ", fmin
    end subroutine scenario_optimize_de_max_gen_zero

    !> A constrained objective handed to DE, which honours no constraint.
    subroutine scenario_optimize_de_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_de(obj, lo, hi, 1_int64, x, fmin, max_gen=2)
        print '(a, 2es22.15)', "accepted a constrained objective in DE: ", x(1), fmin
    end subroutine scenario_optimize_de_constraints_not_honoured

    !> A multistart run with no starts.
    subroutine scenario_optimize_multistart_nstart_zero()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=0)
        print '(a, es22.15)', "accepted zero starts: ", fmin
    end subroutine scenario_optimize_multistart_nstart_zero

    !> A negative merge radius, which no pair of minima could be within.
    subroutine scenario_optimize_multistart_merge_tol_negative()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=4, merge_tol=-1.0e-6_real64)
        print '(a, es22.15)', "accepted a negative merge_tol: ", fmin
    end subroutine scenario_optimize_multistart_merge_tol_negative

    !> A NEGATIVE `max_neval` in a `pf_simplex_solver`; the control is `0`, which is the default
    !! and means the simplex's own 5000.
    !!
    !! The two calls differ in that one component alone, so the abort can only be the budget rule.
    subroutine scenario_optimize_simplex_solver_negative_budget()
        type(pf_simplex_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        solver%max_neval = 0
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=1, solver=solver)
        print '(a, es22.15)', "simplex solver control ran: ", fmin
        solver%max_neval = -1
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=1, solver=solver)
        print '(a, es22.15)', "accepted a negative pf_simplex_solver%max_neval: ", fmin
    end subroutine scenario_optimize_simplex_solver_negative_budget
    !
    !> A NEGATIVE `max_neval` in a `pf_bobyqa_solver`; the control is `0`, which means `500*n`.
    !!
    !! Until V5 of the solver vocabulary, any value at or below zero was read as the default here,
    !! so a negative budget was silently accepted. The control proves the zero still means what it
    !! meant.
    subroutine scenario_prima_bobyqa_solver_negative_budget()
        type(pf_bobyqa_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        solver%max_neval = 0
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=1, solver=solver)
        print '(a, es22.15)', "bobyqa solver control ran: ", fmin
        solver%max_neval = -1
        call pf_minimize_multistart(sphere, lo, hi, 1_int64, x, fmin, nstart=1, solver=solver)
        print '(a, es22.15)', "accepted a negative pf_bobyqa_solver%max_neval: ", fmin
    end subroutine scenario_prima_bobyqa_solver_negative_budget
    !
    !> A constrained objective handed to the multistart driver, which honours no constraint.
    !!
    !! **The driver's OWN refusal is what this proves**, which is why its wrapper asserts the
    !! entry point's name: without it the local solver would refuse the same objective a moment
    !! later, the process would still abort, and a scenario keyed on the exit status alone would
    !! pass with the guard deleted.
    subroutine scenario_optimize_multistart_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_multistart(obj, lo, hi, 1_int64, x, fmin, nstart=2)
        print '(a, 2es22.15)', "accepted a constrained objective in the multistart driver: ", x(1), fmin
    end subroutine scenario_optimize_multistart_constraints_not_honoured

    !> A NaN from the objective inside the multistart driver's own team.
    !!
    !! **The abort happens on a worker thread**, which is what `optimize_abort`'s named `critical`
    !! exists for: two threads reaching `ERROR STOP` at once leave the exit status
    !! nondeterministic, including 0, under ifx. Needs a real OpenMP build to reach more than one
    !! thread, so it lives in the `concurrency_scenarios` bucket.
    subroutine scenario_optimize_multistart_nonfinite_threaded()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_multistart(always_nan, lo, hi, 1_int64, x, fmin, nstart=64, threads=4)
        print '(a, es22.15)', "accepted a NaN objective under threads: ", fmin
    end subroutine scenario_optimize_multistart_nonfinite_threaded

    !> No variables at all: `pf_minimize_bobyqa` with a zero-length start.
    subroutine scenario_prima_size_zero()
        real(real64) :: x(0), fmin

        call pf_minimize_bobyqa(sphere, x, fmin)
        print '(a, es22.15)', "accepted a zero-length start in BOBYQA: ", fmin
    end subroutine scenario_prima_size_zero

    !> A NaN coordinate in the start point, which PRIMA would replace by zero and carry on.
    subroutine scenario_prima_start_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(2), fmin

        x(1) = 0.5_real64
        x(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_minimize_bobyqa(sphere, x, fmin)
        print '(a, es22.15)', "accepted a NaN start in BOBYQA: ", fmin
    end subroutine scenario_prima_start_nan

    !> An infinite coordinate in the start point, which the driver would clamp to `BOUNDMAX`.
    !!
    !! Clamped, it becomes a start the caller did not ask for, the objective is evaluated at a
    !! number of order `1e307`, and a value that overflows there is reported as the objective's
    !! fault.
    subroutine scenario_prima_start_infinite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        real(real64) :: x(2), fmin

        x(1) = 0.5_real64
        x(2) = ieee_value(1.0_real64, ieee_positive_inf)
        call pf_minimize_bobyqa(sphere, x, fmin)
        print '(a, es22.15)', "accepted an infinite start in BOBYQA: ", fmin
    end subroutine scenario_prima_start_infinite

    !> Bounds of a different length from the start point.
    subroutine scenario_prima_bounds_size()
        real(real64) :: x(2), fmin, lo(3), hi(3)

        x = 0.5_real64
        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi)
        print '(a, es22.15)', "accepted bounds of the wrong length in BOBYQA: ", fmin
    end subroutine scenario_prima_bounds_size

    !> A NaN bound, which every range test answers "false" for and so lets through.
    !!
    !! Upstream treats it as an absent bound; here it is refused, because a bound the caller wrote
    !! and the engine dropped is a different problem from the one they posed -- and the NaN
    !! otherwise reaches the objective, or comes back as `info%cstrv`.
    subroutine scenario_prima_bound_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x(2), fmin, lo(2), hi(2)

        x = 0.5_real64
        lo(1) = ieee_value(1.0_real64, ieee_quiet_nan)
        lo(2) = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi)
        print '(a, es22.15)', "accepted a NaN bound in BOBYQA: ", fmin
    end subroutine scenario_prima_bound_nan

    !> An initial trust-region radius wider than half the narrowest side of the box.
    !!
    !! BOBYQA's model needs two distinct points per coordinate within the bounds, so upstream
    !! quietly reduces such a `rhobeg` to a quarter of that width and warns. Refused here: from a
    !! start ON a bound the initial points coincide and the run ends after a handful of
    !! evaluations, at the start, reporting rounding error.
    subroutine scenario_prima_rhobeg_too_wide()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        x = 0.0_real64
        lo = 0.0_real64
        hi = 1.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=1.0_real64)
        print '(a, es22.15)', "accepted a rhobeg wider than half the box: ", fmin
    end subroutine scenario_prima_rhobeg_too_wide

    !> A bound pair with no room between them, PRIMA's `NO_SPACE_BETWEEN_BOUNDS`.
    !!
    !! PRIMA returns that code and leaves the start untouched; here it is refused, because a
    !! status nobody reads looks exactly like a minimisation that found nothing to improve.
    subroutine scenario_prima_no_space_between_bounds()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        x = 0.0_real64
        lo = 0.0_real64
        hi = [2.0_real64, epsilon(1.0_real64)]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi)
        print '(a, es22.15)', "accepted a bound pair with no room between them: ", fmin
    end subroutine scenario_prima_no_space_between_bounds

    !> A start point outside the bounds, which PRIMA would move onto them.
    subroutine scenario_prima_start_outside_bounds()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        x = [0.5_real64, 3.0_real64]
        lo = -2.0_real64
        hi = 2.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi)
        print '(a, es22.15)', "accepted a start outside the bounds: ", fmin
    end subroutine scenario_prima_start_outside_bounds

    !> A final trust-region radius above the initial one, which PRIMA would swap.
    subroutine scenario_prima_rho_order()
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(sphere, x, fmin, rhobeg=1.0e-6_real64, rhoend=1.0_real64)
        print '(a, es22.15)', "accepted rhoend above rhobeg: ", fmin
    end subroutine scenario_prima_rho_order

    !> An interpolation-set size outside `[n+2, (n+1)(n+2)/2]`, which PRIMA would clamp.
    subroutine scenario_prima_npt_range()
        real(real64) :: x(3), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(sphere, x, fmin, npt=3)
        print '(a, es22.15)', "accepted an npt below n+2: ", fmin
    end subroutine scenario_prima_npt_range

    !> A `scale` of a different length from the start point.
    subroutine scenario_prima_scale_size()
        real(real64) :: x(2), fmin, sc(3)

        x = 0.5_real64
        sc = 1.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, scale=sc)
        print '(a, es22.15)', "accepted a scale of the wrong length: ", fmin
    end subroutine scenario_prima_scale_size

    !> A zero element in `scale`, which would divide the start by zero.
    subroutine scenario_prima_scale_nonpositive()
        real(real64) :: x(2), fmin, sc(2)

        x = 0.5_real64
        sc = [1.0_real64, 0.0_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, scale=sc)
        print '(a, es22.15)', "accepted a zero scale: ", fmin
    end subroutine scenario_prima_scale_nonpositive

    !> A zero evaluation budget for BOBYQA.
    subroutine scenario_prima_budget_zero()
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(sphere, x, fmin, max_neval=0)
        print '(a, es22.15)', "accepted a zero budget in BOBYQA: ", fmin
    end subroutine scenario_prima_budget_zero

    !> An evaluation budget above the ceiling every count in this tier is bounded by.
    subroutine scenario_prima_budget_ceiling()
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(sphere, x, fmin, max_neval=huge(1))
        print '(a, es22.15)', "accepted a budget above huge(1)/2 in BOBYQA: ", fmin
    end subroutine scenario_prima_budget_ceiling

    !> A NaN from the objective, which PRIMA's moderated extreme barrier would absorb.
    !!
    !! Upstream replaces the value by a large finite one and carries on, so the search continues
    !! against a value the objective never returned; here it aborts.
    subroutine scenario_prima_nonfinite_value()
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(always_nan, x, fmin)
        print '(a, es22.15)', "accepted a NaN objective value in BOBYQA: ", fmin
    end subroutine scenario_prima_nonfinite_value

    !> A constrained objective handed to BOBYQA, which honours bounds but not `c(x) <= 0`.
    subroutine scenario_prima_bobyqa_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_bobyqa(obj, x, fmin)
        print '(a, 2es22.15)', "accepted a constrained objective in BOBYQA: ", x(1), fmin
    end subroutine scenario_prima_bobyqa_constraints_not_honoured

    !> A constrained objective handed to LINCOA, which honours linear constraints but not
    !! `c(x) <= 0`.
    subroutine scenario_prima_lincoa_constraints_not_honoured()
        type(unit_disc) :: obj
        real(real64) :: x(2), fmin

        x = 0.5_real64
        call pf_minimize_lincoa(obj, x, fmin)
        print '(a, 2es22.15)', "accepted a constrained objective in LINCOA: ", x(1), fmin
    end subroutine scenario_prima_lincoa_constraints_not_honoured

    !> A constraint matrix whose shape does not match `x` and `b_ineq`.
    subroutine scenario_prima_lincoa_shape()
        real(real64) :: x(2), fmin, a(1, 3), b(1)

        x = 0.0_real64
        a = 1.0_real64
        b = 1.0_real64
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a, b_ineq=b)
        print '(a, es22.15)', "accepted a constraint matrix of the wrong shape: ", fmin
    end subroutine scenario_prima_lincoa_shape

    !> A constraint row of all zeros, PRIMA's `ZERO_LINEAR_CONSTRAINT`, which upstream drops with
    !! a warning.
    subroutine scenario_prima_zero_constraint_row()
        real(real64) :: x(2), fmin, a(2, 2), b(2)

        x = 0.0_real64
        a(1, :) = [1.0_real64, 1.0_real64]
        a(2, :) = 0.0_real64
        b = 1.0_real64
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a, b_ineq=b)
        print '(a, es22.15)', "accepted an all-zero constraint row: ", fmin
    end subroutine scenario_prima_zero_constraint_row

    !> A start point that violates the linear constraints, which PRIMA would admit by relaxing
    !! their right-hand sides.
    subroutine scenario_prima_lincoa_infeasible_start()
        real(real64) :: x(2), fmin, a(1, 2), b(1)

        x = [2.0_real64, 2.0_real64]
        a(1, :) = [1.0_real64, 1.0_real64]
        b = 1.0_real64
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a, b_ineq=b)
        print '(a, es22.15)', "accepted an infeasible start in LINCOA: ", fmin
    end subroutine scenario_prima_lincoa_infeasible_start

    !> A negative feasibility tolerance, which PRIMA would replace by its default.
    subroutine scenario_prima_ctol_negative()
        type(outside_disc) :: obj
        real(real64) :: x(2), fmin

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin, ctol=-1.0_real64)
        print '(a, es22.15)', "accepted a negative ctol: ", fmin
    end subroutine scenario_prima_ctol_negative

    !> An objective whose `n_constraints` answers a negative number.
    subroutine scenario_prima_cobyla_negative_count()
        type(negative_count_disc) :: obj
        real(real64) :: x(2), fmin

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin)
        print '(a, es22.15)', "accepted a negative n_constraints: ", fmin
    end subroutine scenario_prima_cobyla_negative_count

    !> A constraint value that is a NaN, which PRIMA's `moderatec` would clamp and carry on with.
    subroutine scenario_prima_constraint_nonfinite()
        type(nan_constraint_disc) :: obj
        real(real64) :: x(2), fmin

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin)
        print '(a, es22.15)', "accepted a NaN constraint value: ", fmin
    end subroutine scenario_prima_constraint_nonfinite

    !> A NaN OBJECTIVE value from a constrained objective, screened by `evaluate_fc`.
    !!
    !! `prima_nonfinite_value` covers the same screen in `evaluate`, which is the unconstrained
    !! route; only a `pf_constrained_objective` reaches `evaluate_fc`, and its objective screen
    !! runs before it looks at the constraints at all. The `context=` here is what makes
    !! `state_abort` take its context-carrying arm, so the message must name the call site too.
    subroutine scenario_prima_cobyla_nonfinite_value()
        type(nan_value_disc) :: obj
        real(real64) :: x(2), fmin

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin, context="fitting the disc model")
        print '(a, es22.15)', "accepted a NaN objective value in COBYLA: ", fmin
    end subroutine scenario_prima_cobyla_nonfinite_value

    !> A `ctol` that is not finite, the companion of `prima_ctol_negative`.
    !!
    !! An infinite tolerance would declare every point feasible, including one whose violation is
    !! itself infinite, so `info%cstrv > ctol` could never fire and `PF_OPT_INFEASIBLE` would
    !! become unreachable. The finiteness test is its own statement ahead of the sign test,
    !! because an ordered comparison against a NaN signals `IEEE_INVALID` even where it answers.
    subroutine scenario_prima_ctol_nonfinite()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_positive_inf
        type(outside_disc) :: obj
        real(real64) :: x(2), fmin

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin, ctol=ieee_value(1.0_real64, ieee_positive_inf))
        print '(a, es22.15)', "accepted a non-finite ctol: ", fmin
    end subroutine scenario_prima_ctol_nonfinite

    !> A refusal whose message carries the caller's `context`, short enough to appear whole.
    !!
    !! Every abort in this tier reproduces the entry point and the optional context, and the
    !! with-context arm is a different line from the without-context one. This is
    !! `pf_minimize_bobyqa`'s own driver abort -- the `rhobeg` test -- rather than
    !! `refuse_bad_call`'s, so the two abort sites are covered by two scenarios rather than one.
    subroutine scenario_prima_context_is_reported()
        real(real64) :: x(2), fmin, lo(2), hi(2)

        x = 0.0_real64
        lo = 0.0_real64
        hi = 1.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=1.0_real64, &
                                context="calibrating the response curve")
        print '(a, es22.15)', "accepted a rhobeg wider than half the box, with context: ", fmin
    end subroutine scenario_prima_context_is_reported

    !> A `context` longer than the hundred characters the message carries, which is truncated.
    !!
    !! The cap is `parquet_optimize`'s, so a caller who passes a whole rendered expression as
    !! their context cannot push the rest of the message off a terminal. This one goes through
    !! `refuse_bad_call`, whose with-context arm is a different site from the driver's above.
    subroutine scenario_prima_context_is_capped()
        real(real64) :: x(2), fmin

        ! 150 characters, and the cap falls on a repeat boundary, so the truncated message ends
        ! with a whole "abcdefghij" followed by the ellipsis -- which is what the test asserts.
        x = 0.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, npt=2, context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "accepted an npt below n+2, with a long context: ", fmin
    end subroutine scenario_prima_context_is_capped

    !> LINCOA's own feasibility refusal, with the caller's `context` attached.
    !!
    !! `prima_lincoa_infeasible_start` covers the same refusal without one; this reaches the
    !! context-carrying arm of the abort helper that `pf_minimize_lincoa` contains, which is
    !! separate from the one in `refuse_bad_call`.
    subroutine scenario_prima_lincoa_infeasible_start_context()
        real(real64) :: x(2), fmin, a(1, 2), b(1)

        x = [2.0_real64, 2.0_real64]
        a(1, :) = [1.0_real64, 1.0_real64]
        b = 1.0_real64
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a, b_ineq=b, &
                                context="projecting onto the budget plane")
        print '(a, es22.15)', "accepted an infeasible start in LINCOA, with context: ", fmin
    end subroutine scenario_prima_lincoa_infeasible_start_context
    !
    ! ---- pf_find_root: every caller contract it refuses ------------------------------------
    !
    !> Each `pf_find_root` scenario first makes the nearest LEGAL call -- the refused value's
    !> boundary neighbour -- and prints "root control solved", then makes the smallest call that
    !> provokes exactly one abort and prints what it accepted. The control is what shows the guard
    !> refuses the bad value rather than the whole call; the wrapper asserts both lines' evidence.
    subroutine scenario_root_reversed_bracket()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 2.0_real64, 1.0_real64, x)
        print '(a, es22.15)', "accepted a reversed bracket: ", x
    end subroutine scenario_root_reversed_bracket
    !
    !> A NaN end is refused by the bracket guard, built with `ieee_value` so the FIXTURE does not
    !> trap first.
    subroutine scenario_root_nan_bracket_end()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), x)
        print '(a, es22.15)', "accepted a NaN bracket end: ", x
    end subroutine scenario_root_nan_bracket_end
    !
    !> Two finite ends whose difference overflows; the control is a bracket just as wide as the
    !> arithmetic allows, which must be solved rather than refused.
    subroutine scenario_root_bracket_width()
        real(real64) :: x

        call pf_find_root(root_line_03, -0.8e308_real64, 0.8e308_real64, x)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_line_03, -1.0e308_real64, 1.0e308_real64, x)
        print '(a, es22.15)', "accepted a bracket whose width overflows: ", x
    end subroutine scenario_root_bracket_width
    !
    !> A negative absolute tolerance; the control is zero, the default.
    subroutine scenario_root_negative_atol()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, atol=0.0_real64)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, atol=-1.0e-10_real64)
        print '(a, es22.15)', "accepted a negative atol: ", x
    end subroutine scenario_root_negative_atol
    !
    !> A negative relative tolerance; the control is zero, which is raised to the floor.
    subroutine scenario_root_negative_rtol()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, rtol=0.0_real64)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, rtol=-1.0e-10_real64)
        print '(a, es22.15)', "accepted a negative rtol: ", x
    end subroutine scenario_root_negative_rtol
    !
    !> A budget of no evaluations; the control is one, which evaluates `a` and stops.
    subroutine scenario_root_bad_max_neval()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, max_neval=1)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, max_neval=0)
        print '(a, es22.15)', "accepted a zero max_neval: ", x
    end subroutine scenario_root_bad_max_neval
    !
    !> A mode that is none of the four codes; the control is the highest code there is.
    subroutine scenario_root_bad_expansion_mode()
        type(pf_bracket_expansion) :: grow
        real(real64) :: x

        grow%mode = PF_EXPAND_BOTH
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "root control solved: ", x
        grow%mode = PF_EXPAND_BOTH + 1
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "accepted an unknown expansion mode: ", x
    end subroutine scenario_root_bad_expansion_mode
    !
    !> A factor of exactly one, which cannot grow anything; the control is the next double up.
    subroutine scenario_root_bad_expansion_factor()
        type(pf_bracket_expansion) :: grow
        real(real64) :: x

        grow%mode = PF_EXPAND_UP
        grow%factor = nearest(1.0_real64, 2.0_real64)
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "root control solved: ", x
        grow%factor = 1.0_real64
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "accepted an expansion factor of one: ", x
    end subroutine scenario_root_bad_expansion_factor
    !
    !> A negative try count; the control is zero, which expands nothing.
    subroutine scenario_root_bad_expansion_tries()
        type(pf_bracket_expansion) :: grow
        real(real64) :: x

        grow%mode = PF_EXPAND_UP
        grow%max_tries = 0
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "root control solved: ", x
        grow%max_tries = -1
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "accepted a negative max_tries: ", x
    end subroutine scenario_root_bad_expansion_tries
    !
    !> An upper limit below the bracket's upper end; the control puts both limits ON the ends.
    subroutine scenario_root_limits_inside_the_bracket()
        type(pf_bracket_expansion) :: grow
        real(real64) :: x

        grow%mode = PF_EXPAND_BOTH
        grow%lower_limit = 1.0_real64
        grow%upper_limit = 2.0_real64
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "root control solved: ", x
        grow%upper_limit = 1.5_real64
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "accepted an upper limit inside the bracket: ", x
    end subroutine scenario_root_limits_inside_the_bracket
    !
    !> A NaN lower limit, which would pass both comparisons against the bracket; the control keeps
    !> the default limits. Built with `ieee_value` so the FIXTURE does not trap first.
    subroutine scenario_root_nonfinite_expansion_limit()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_bracket_expansion) :: grow
        real(real64) :: x

        grow%mode = PF_EXPAND_DOWN
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "root control solved: ", x
        grow%lower_limit = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, expand=grow)
        print '(a, es22.15)', "accepted a NaN expansion limit: ", x
    end subroutine scenario_root_nonfinite_expansion_limit
    !
    !> A function returning a NaN inside the bracket; the control solves the same function on a
    !> bracket where it has none.
    subroutine scenario_root_function_returns_nan()
        real(real64) :: x

        call pf_find_root(root_nan_beyond_two, 0.0_real64, 2.0_real64, x)
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_nan_beyond_two, 0.0_real64, 3.0_real64, x)
        print '(a, es22.15)', "accepted a NaN from the function: ", x
    end subroutine scenario_root_function_returns_nan
    !
    !> The caller's `context=` is carried into the message, through the PLAIN-FUNCTION form, which
    !> forwards it to the object form: the one forwarding site between a caller and the abort.
    subroutine scenario_root_context_reported()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, context="my_call_site")
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 2.0_real64, 1.0_real64, x, context="my_call_site")
        print '(a, es22.15)', "accepted a reversed bracket with a context: ", x
    end subroutine scenario_root_context_reported
    !
    !> Caller text inside a message is capped at 100 characters, so that a context built from a
    !> long path cannot push the message itself out of a terminal or a log line.
    subroutine scenario_root_context_capped()
        real(real64) :: x

        call pf_find_root(root_sq2, 1.0_real64, 2.0_real64, x, context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "root control solved: ", x
        call pf_find_root(root_sq2, 2.0_real64, 1.0_real64, x, context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "accepted a reversed bracket with a long context: ", x
    end subroutine scenario_root_context_capped
    !
    ! ---- pf_dct, pf_idct and pf_next_pow2: every caller contract they refuse -------------------
    !
    !> Each `parquet_transform` scenario first makes the nearest LEGAL call -- the refused value's
    !> boundary neighbour -- and prints a "transform control" line, then makes the smallest call
    !> that provokes exactly one abort and prints what it accepted. The control is what shows the
    !> guard refuses the bad value rather than the whole call; the wrapper asserts both lines'
    !> evidence.
    subroutine scenario_transform_empty_sequence()
        real(real64) :: x(1), y(1), empty_x(0), empty_y(0)

        x = 2.0_real64
        call pf_dct(x, y)
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(empty_x, empty_y)
        print '(a)', "accepted an empty sequence"
    end subroutine scenario_transform_empty_sequence
    !
    !> A length of 1000 is refused, and the message names the 1024 `pf_next_pow2` gives; a length
    !> of 1024 is the control.
    subroutine scenario_transform_length_not_pow2()
        real(real64) :: x(1024), y(1024)

        x = 1.0_real64
        call pf_dct(x, y)
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(x(1:1000), y(1:1000))
        print '(a)', "accepted a length of 1000"
    end subroutine scenario_transform_length_not_pow2
    !
    subroutine scenario_transform_size_mismatch()
        real(real64) :: x(8), y(8)

        x = 1.0_real64
        call pf_dct(x, y)
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(x, y(1:4))
        print '(a)', "accepted y shorter than x"
    end subroutine scenario_transform_size_mismatch
    !
    !> `pf_idct` validates too, under its own name: without this scenario, a `pf_idct` that skipped
    !> its validation would pass every other test in the suite.
    subroutine scenario_transform_idct_size_mismatch()
        real(real64) :: y(8), x(8)

        y = 1.0_real64
        call pf_idct(y, x)
        print '(a, es22.15)', "transform control transformed: ", x(1)
        call pf_idct(y, x(1:4))
        print '(a)', "accepted x shorter than y"
    end subroutine scenario_transform_idct_size_mismatch
    !
    !> scipy's `"forward"`, a token this library does not offer, is refused; `"ORTHO"`, a token it
    !> does offer in another case, is the control.
    subroutine scenario_transform_bad_norm_token()
        real(real64) :: x(8), y(8)

        x = 1.0_real64
        call pf_dct(x, y, norm="ORTHO")
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(x, y, norm="forward")
        print '(a)', 'accepted norm="forward"'
    end subroutine scenario_transform_bad_norm_token
    !
    subroutine scenario_transform_next_pow2_nonpositive()
        integer :: n

        n = pf_next_pow2(1)
        print '(a, i0)', "transform control length: ", n
        n = pf_next_pow2(0)
        print '(a, i0)', "accepted a count of 0, giving ", n
    end subroutine scenario_transform_next_pow2_nonpositive
    !
    !> The largest power of two a default integer holds is the control; one more is refused.
    subroutine scenario_transform_next_pow2_too_large()
        integer :: n, top

        top = 2**(digits(1) - 1)
        n = pf_next_pow2(top)
        print '(a, i0)', "transform control length: ", n
        n = pf_next_pow2(top + 1)
        print '(a, i0)', "accepted a count above the largest power of two, giving ", n
    end subroutine scenario_transform_next_pow2_too_large
    !
    subroutine scenario_transform_context_reported()
        real(real64) :: x(8), y(8)

        x = 1.0_real64
        call pf_dct(x, y, context="my_call_site")
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(x, y(1:4), context="my_call_site")
        print '(a)', "accepted y shorter than x with a context"
    end subroutine scenario_transform_context_reported
    !
    subroutine scenario_transform_context_capped()
        real(real64) :: x(8), y(8)

        x = 1.0_real64
        call pf_dct(x, y, context=repeat("abcdefghij", 15))
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dct(x, y(1:4), context=repeat("abcdefghij", 15))
        print '(a)', "accepted y shorter than x with a long context"
    end subroutine scenario_transform_context_capped
    !
    ! The sine pair delegates to the cosine pair, so each of these would report `pf_dct:` or
    ! `pf_idct:` if the wrapper delegated before validating. That is the whole point of them.
    !
    subroutine scenario_transform_dst_length_not_pow2()
        real(real64) :: x(1024), y(1024)

        x = 1.0_real64
        call pf_dst(x, y)
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dst(x(1:1000), y(1:1000))
        print '(a)', "accepted a length of 1000"
    end subroutine scenario_transform_dst_length_not_pow2
    !
    subroutine scenario_transform_dst_size_mismatch()
        real(real64) :: x(8), y(8)

        x = 1.0_real64
        call pf_dst(x, y, context="my_call_site")
        print '(a, es22.15)', "transform control transformed: ", y(1)
        call pf_dst(x, y(1:4), context="my_call_site")
        print '(a)', "accepted y shorter than x"
    end subroutine scenario_transform_dst_size_mismatch
    !
    subroutine scenario_transform_idst_length_not_pow2()
        real(real64) :: y(1024), x(1024)

        y = 1.0_real64
        call pf_idst(y, x)
        print '(a, es22.15)', "transform control transformed: ", x(1)
        call pf_idst(y(1:1000), x(1:1000))
        print '(a)', "accepted a length of 1000"
    end subroutine scenario_transform_idst_length_not_pow2
    !
    subroutine scenario_transform_idst_bad_norm_token()
        real(real64) :: y(8), x(8)

        y = 1.0_real64
        call pf_idst(y, x, norm="ortho")
        print '(a, es22.15)', "transform control transformed: ", x(1)
        call pf_idst(y, x, norm="orthonormal")
        print '(a)', "accepted a norm token it does not offer"
    end subroutine scenario_transform_idst_bad_norm_token
    !
    ! ---- pf_kde: every caller contract it refuses -----------------------------------------
    !
    !> Each `pf_kde` scenario first makes the nearest LEGAL call and prints a "kde control" line,
    !> then makes the smallest call that provokes exactly one abort and prints what it accepted.
    !> The control is what shows the guard refuses the bad value rather than the whole call.
    !
    !> Proves that pf_kde%fit refuses a zero bandwidth.
    subroutine scenario_kde_bandwidth_zero()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=1.0e-300_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, bandwidth=0.0_real64)
        print '(a)', "accepted a zero bandwidth"
    end subroutine scenario_kde_bandwidth_zero
    !
    !> Proves that pf_kde%fit refuses a NaN bandwidth.
    subroutine scenario_kde_bandwidth_nan()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
            ieee_negative_inf
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=1.0_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, bandwidth=ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a)', "accepted a NaN bandwidth"
    end subroutine scenario_kde_bandwidth_nan
    !
    !> Proves that pf_kde%fit refuses bandwidth= and rule= together.
    subroutine scenario_kde_bandwidth_and_rule()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, rule="scott")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, bandwidth=1.0_real64, rule="scott")
        print '(a)', "accepted a bandwidth and a rule"
    end subroutine scenario_kde_bandwidth_and_rule
    !
    !> Proves that pf_kde%fit refuses an unknown rule.
    subroutine scenario_kde_unknown_rule()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, rule="Scott")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, rule="sheather")
        print '(a)', "accepted an unknown rule"
    end subroutine scenario_kde_unknown_rule
    !
    !> Proves that pf_kde%fit refuses a negative adjust.
    subroutine scenario_kde_adjust_negative()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, adjust=0.5_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adjust=-1.0_real64)
        print '(a)', "accepted a negative adjust"
    end subroutine scenario_kde_adjust_negative
    !
    !> Proves that pf_kde%fit refuses an unknown kernel.
    subroutine scenario_kde_unknown_kernel()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, kernel="Box")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, kernel="triangle")
        print '(a)', "accepted an unknown kernel"
    end subroutine scenario_kde_unknown_kernel
    !
    !> Proves that pf_kde%fit refuses an infinite bound.
    subroutine scenario_kde_bound_infinite()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
            ieee_negative_inf
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, lower=0.0_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, upper=ieee_value(1.0_real64, ieee_positive_inf))
        print '(a)', "accepted an infinite bound"
    end subroutine scenario_kde_bound_infinite
    !
    !> Proves that pf_kde%fit refuses lower >= upper.
    subroutine scenario_kde_lower_not_below_upper()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, lower=0.0_real64, upper=5.0_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, lower=5.0_real64, upper=5.0_real64)
        print '(a)', "accepted lower equal to upper"
    end subroutine scenario_kde_lower_not_below_upper
    !
    !> Proves that pf_kde%fit refuses boundary= without a bound.
    subroutine scenario_kde_boundary_without_bound()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, lower=0.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, boundary="reflect")
        print '(a)', "accepted boundary= without a bound"
    end subroutine scenario_kde_boundary_without_bound
    !
    !> Proves that pf_kde%fit refuses an unknown boundary.
    subroutine scenario_kde_unknown_boundary()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, lower=0.0_real64, boundary="Reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, lower=0.0_real64, boundary="mirror")
        print '(a)', "accepted an unknown boundary"
    end subroutine scenario_kde_unknown_boundary
    !
    !> Proves that pf_kde%fit refuses weights of the wrong length, in the family's words.
    subroutine scenario_kde_weights_size()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, weights=[1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, weights=[1.0_real64, 1.0_real64, 1.0_real64])
        print '(a)', "accepted three weights for four values"
    end subroutine scenario_kde_weights_size
    !
    !> Proves that pf_kde%fit refuses is_valid of the wrong length, in the family's words.
    subroutine scenario_kde_is_valid_size()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, is_valid=[.true., .true., .true., .true.])
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, is_valid=[.true., .true., .true., .true., .true.])
        print '(a)', "accepted five flags for four values"
    end subroutine scenario_kde_is_valid_size
    !
    !> Proves that pf_kde%fit refuses a negative weight, in the family's words.
    subroutine scenario_kde_negative_weight()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, weights=[1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64])
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, weights=[1.0_real64, -1.0_real64, 1.0_real64, 1.0_real64])
        print '(a)', "accepted a negative weight"
    end subroutine scenario_kde_negative_weight
    !
    !> Proves that pf_kde%fit refuses an unknown weight_type, in the family's words.
    subroutine scenario_kde_weight_type()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, weights=[1.0_real64, 2.0_real64, 1.0_real64, 1.0_real64], weight_type="frequency")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, weights=[1.0_real64, 2.0_real64, 1.0_real64, 1.0_real64], weight_type="bogus")
        print '(a)', "accepted an unknown weight_type"
    end subroutine scenario_kde_weight_type
    !
    !> Proves that pf_kde%fit's real32 form refuses weights of the wrong length.
    subroutine scenario_kde_real32_weights_size()
        type(pf_kde) :: k
        real(real32) :: x(4)

        x = [1.0, 2.0, 3.0, 4.0]
        call k%fit(x, weights=[1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, weights=[1.0_real64, 1.0_real64])
        print '(a)', "accepted two weights for four values"
    end subroutine scenario_kde_real32_weights_size
    !
    !> Proves that pf_kde%fit refuses threads=0.
    subroutine scenario_kde_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, threads=1)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, threads=0)
        print '(a)', "accepted threads=0"
    end subroutine scenario_kde_threads_zero
    !
    !> Proves that pf_kde%pdf refuses an object that was never fitted.
    subroutine scenario_kde_query_unfitted()
        type(pf_kde) :: k, fresh
        real(real64) :: x(4), f

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%pdf(2.0_real64, f)
        print '(a, es22.15)', "kde control density: ", f
        call fresh%pdf(2.0_real64, f)
        print '(a, es22.15)', "answered an unfitted query: ", f
    end subroutine scenario_kde_query_unfitted
    !
    !> Proves that pf_kde%cdf refuses an object that %clear unfitted.
    subroutine scenario_kde_query_after_clear()
        type(pf_kde) :: k
        real(real64) :: x(4), p

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%cdf(2.0_real64, p)
        print '(a, es22.15)', "kde control probability: ", p
        call k%clear()
        call k%cdf(2.0_real64, p)
        print '(a, es22.15)', "answered a query after %clear: ", p
    end subroutine scenario_kde_query_after_clear
    !
    !> Proves that pf_kde%bandwidth refuses an object that was never fitted.
    subroutine scenario_kde_accessor_unfitted()
        type(pf_kde) :: k, fresh
        real(real64) :: x(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        print '(a, es22.15)', "answered an unfitted bandwidth: ", fresh%bandwidth()
    end subroutine scenario_kde_accessor_unfitted
    !
    !> Proves that pf_kde%pdf refuses an output of the wrong size.
    subroutine scenario_kde_pdf_size()
        type(pf_kde) :: k
        real(real64) :: x(4), q(3), f(3)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        q = [1.0_real64, 2.0_real64, 3.0_real64]
        call k%pdf(q, f)
        print '(a, es22.15)', "kde control density: ", f(1)
        call k%pdf(q, f(1:2))
        print '(a)', "accepted two outputs for three points"
    end subroutine scenario_kde_pdf_size
    !
    !> Proves that pf_kde%cdf refuses an output of the wrong size.
    subroutine scenario_kde_cdf_size()
        type(pf_kde) :: k
        real(real64) :: x(4), q(3), p(3)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        q = [1.0_real64, 2.0_real64, 3.0_real64]
        call k%cdf(q, p)
        print '(a, es22.15)', "kde control probability: ", p(1)
        call k%cdf(q, p(1:2))
        print '(a)', "accepted two outputs for three points"
    end subroutine scenario_kde_cdf_size
    !
    !> Proves that pf_kde%quantile refuses an output of the wrong size.
    subroutine scenario_kde_quantile_size()
        type(pf_kde) :: k
        real(real64) :: x(4), p(3), q(3)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        p = [0.1_real64, 0.5_real64, 0.9_real64]
        call k%quantile(p, q)
        print '(a, es22.15)', "kde control quantile: ", q(1)
        call k%quantile(p, q(1:2))
        print '(a)', "accepted two outputs for three probabilities"
    end subroutine scenario_kde_quantile_size
    !
    !> Proves that pf_kde%quantile refuses p above one.
    subroutine scenario_kde_quantile_p_above_one()
        type(pf_kde) :: k
        real(real64) :: x(4), q

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%quantile(1.0_real64, q)
        print '(a, es22.15)', "kde control quantile: ", q
        call k%quantile(1.5_real64, q)
        print '(a, es22.15)', "accepted p = 1.5: ", q
    end subroutine scenario_kde_quantile_p_above_one
    !
    !> Proves that pf_kde%quantile refuses a NaN p.
    subroutine scenario_kde_quantile_p_nan()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
            ieee_negative_inf
        type(pf_kde) :: k
        real(real64) :: x(4), q

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%quantile(0.0_real64, q)
        print '(a, es22.15)', "kde control quantile: ", q
        call k%quantile(ieee_value(1.0_real64, ieee_quiet_nan), q)
        print '(a, es22.15)', "accepted a NaN p: ", q
    end subroutine scenario_kde_quantile_p_nan
    !
    !> Proves that pf_kde%curve refuses x and f of different sizes.
    subroutine scenario_kde_curve_size()
        type(pf_kde) :: k
        real(real64) :: x(4), xg(5), fg(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%curve(xg, fg)
        print '(a, es22.15)', "kde control curve: ", fg(3)
        call k%curve(xg, fg(1:4))
        print '(a)', "accepted a curve of mismatched sizes"
    end subroutine scenario_kde_curve_size
    !
    !> Proves that pf_kde%curve refuses xmin >= xmax.
    subroutine scenario_kde_curve_reversed_range()
        type(pf_kde) :: k
        real(real64) :: x(4), xg(5), fg(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%curve(xg, fg, xmin=0.0_real64, xmax=5.0_real64)
        print '(a, es22.15)', "kde control curve: ", fg(3)
        call k%curve(xg, fg, xmin=5.0_real64, xmax=5.0_real64)
        print '(a)', "accepted an empty range"
    end subroutine scenario_kde_curve_reversed_range
    !
    !> Proves that pf_kde%curve refuses a negative cut.
    subroutine scenario_kde_curve_negative_cut()
        type(pf_kde) :: k
        real(real64) :: x(4), xg(5), fg(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%curve(xg, fg, cut=0.0_real64)
        print '(a, es22.15)', "kde control curve: ", fg(3)
        call k%curve(xg, fg, cut=-1.0_real64)
        print '(a)', "accepted a negative cut"
    end subroutine scenario_kde_curve_negative_cut
    !
    !> Proves that pf_kde%curve refuses an infinite end.
    subroutine scenario_kde_curve_nonfinite_end()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
            ieee_negative_inf
        type(pf_kde) :: k
        real(real64) :: x(4), xg(5), fg(5)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x)
        call k%curve(xg, fg, xmin=-10.0_real64)
        print '(a, es22.15)', "kde control curve: ", fg(3)
        call k%curve(xg, fg, xmin=ieee_value(1.0_real64, ieee_negative_inf))
        print '(a)', "accepted an infinite end"
    end subroutine scenario_kde_curve_nonfinite_end
    !
    ! ---- pf_kde_grid: every caller contract it refuses ----------------------------------
    !
    !> Each scenario first makes the nearest LEGAL call and prints a "kde control" line, as the
    !> `pf_kde` scenarios above do, then the smallest call that provokes exactly one abort.
    !
    !> Proves that pf_kde_grid%init refuses ncells=0.
    subroutine scenario_kde_grid_ncells()
        type(pf_kde_grid) :: g

        call g%init(1, 0.0_real64, 1.0_real64, 0.1_real64)
        print '(a, i0)', "kde control initialised: ", g%ncells()
        call g%init(0, 0.0_real64, 1.0_real64, 0.1_real64)
        print '(a)', "accepted ncells=0"
    end subroutine scenario_kde_grid_ncells
    !
    !> Proves that pf_kde_grid%init refuses a NaN xmin.
    subroutine scenario_kde_grid_range_nan()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        type(pf_kde_grid) :: g

        call g%init(4, -1.0_real64, 1.0_real64, 0.1_real64)
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(4, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64, 0.1_real64)
        print '(a)', "accepted a NaN xmin"
    end subroutine scenario_kde_grid_range_nan
    !
    !> Proves that pf_kde_grid%init refuses xmin == xmax.
    subroutine scenario_kde_grid_range_reversed()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(4, 1.0_real64, 1.0_real64, 0.1_real64)
        print '(a)', "accepted an empty range"
    end subroutine scenario_kde_grid_range_reversed
    !
    !> Proves that pf_kde_grid%init refuses a range wider than the largest number.
    subroutine scenario_kde_grid_cell_width()
        type(pf_kde_grid) :: g

        call g%init(1, -0.25_real64*huge(1.0_real64), 0.25_real64*huge(1.0_real64), 1.0_real64)
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(1, -huge(1.0_real64), huge(1.0_real64), 1.0_real64)
        print '(a)', "accepted a range wider than the largest number"
    end subroutine scenario_kde_grid_cell_width
    !
    !> Proves that pf_kde_grid%init refuses a zero bandwidth.
    subroutine scenario_kde_grid_bandwidth()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 1.0e-300_real64)
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, 0.0_real64, 1.0_real64, 0.0_real64)
        print '(a)', "accepted a zero bandwidth"
    end subroutine scenario_kde_grid_bandwidth

    !> `pf_kde_grid%init` refuses a SUBNORMAL bandwidth: its kernel's whole support lies inside the
    !! gap between two neighbouring numbers, so the density would be zero wherever it is asked for
    !! while the distribution function still stepped from 0 to 1. The control is the smallest
    !! normal number, which is admissible.
    subroutine scenario_kde_grid_bandwidth_subnormal()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, tiny(1.0_real64))
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, 0.0_real64, 1.0_real64, 0.5_real64*tiny(1.0_real64))
        print '(a)', "accepted a subnormal bandwidth"
    end subroutine scenario_kde_grid_bandwidth_subnormal

    !> `pf_kde_grid%init` refuses a bandwidth whose kernel reach is not finite: the kernel's radius
    !! times it overflows, so no window around a query point can be formed. The control is a
    !! bandwidth a whole radius below that, which is admissible.
    subroutine scenario_kde_grid_bandwidth_unusable()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64*huge(1.0_real64))
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, 0.0_real64, 1.0_real64, 0.5_real64*huge(1.0_real64))
        print '(a)', "accepted a bandwidth whose kernel reach overflows"
    end subroutine scenario_kde_grid_bandwidth_unusable

    !> A query before `%finish` aborts: a grid has no `%fit` to be the seam between filling it and
    !! reading it, so the seam is named, and reading a half-filled accumulation is a mistake rather
    !! than a partial answer. The control is the same query on the same grid, finished.
    subroutine scenario_kde_grid_query_unfinished()
        type(pf_kde_grid) :: g, ok
        real(real64) :: f(4)

        call ok%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call ok%add([0.5_real64], finish=.true.)
        call ok%density(f)
        print '(a, es22.15)', "kde control queried: ", f(1)
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%density(f)
        print '(a)', "queried a grid that was never finished"
    end subroutine scenario_kde_grid_query_unfinished

    !> `pf_kde_grid%init` refuses a `method=` token it does not know, naming the two it accepts.
    !! The control is the same call with a token it does, in the other case.
    subroutine scenario_kde_grid_method_token()
        type(pf_kde_grid) :: g
        character(len=:), allocatable :: token

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, method="BINNED")
        call g%method(token)
        print '(a, a)', "kde control initialised: ", token
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, method="fft")
        print '(a)', "accepted a method token that is neither exact nor binned"
    end subroutine scenario_kde_grid_method_token

    !> `method="binned"` refuses a bandwidth whose padded transform would be longer than
    !! `KDE_BINNED_L_MAX` times the cells: padding grows the length with the kernel's reach over
    !! the range, and a bandwidth about twice the whole range reaches the ceiling. The control is
    !! the same grid at a bandwidth a tenth as wide, which is admitted.
    subroutine scenario_kde_grid_binned_too_long()
        type(pf_kde_grid) :: g

        call g%init(64, 0.0_real64, 1.0_real64, 0.1_real64, method="binned")
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(64, 0.0_real64, 1.0_real64, 40.0_real64, method="binned")
        print '(a)', "accepted a binned grid whose transform is longer than the ceiling"
    end subroutine scenario_kde_grid_binned_too_long

    !> `pf_kde%curve(method="binned")` refuses a single point: the binned curve is one transform
    !! over the whole curve, so it needs two points to have a spacing at all. The control is the
    !! same single-point curve computed exactly, which is well defined.
    subroutine scenario_kde_curve_binned_one_point()
        type(pf_kde) :: k
        real(real64) :: x(1), f(1)

        call k%fit([0.25_real64, 0.5_real64, 0.75_real64], bandwidth=0.1_real64)
        call k%curve(x, f, xmin=0.0_real64, xmax=1.0_real64)
        print '(a, es22.15)', "kde control curved: ", f(1)
        call k%curve(x, f, xmin=0.0_real64, xmax=1.0_real64, method="binned")
        print '(a)', "accepted a binned curve of one point"
    end subroutine scenario_kde_curve_binned_one_point

    !> `pf_kde%fit` refuses a `method=` token it does not know, naming the two it accepts, through
    !! the same resolver `pf_kde_grid%init` uses. The control is the same call with a token it does.
    subroutine scenario_kde_fit_method_token()
        type(pf_kde) :: k
        character(len=:), allocatable :: token

        call k%fit([0.25_real64, 0.5_real64, 0.75_real64], bandwidth=0.1_real64, method="BINNED")
        call k%method(token)
        print '(a, a)', "kde control fitted: ", token
        call k%fit([0.25_real64, 0.5_real64, 0.75_real64], bandwidth=0.1_real64, method="fft")
        print '(a)', "accepted a fit method token that is neither exact nor binned"
    end subroutine scenario_kde_fit_method_token

    !> `pf_kde%curve` refuses a `method=` on a fit made with `method="binned"`: that object IS its
    !! grid, so there is no exact sum for a curve of it to choose instead, and the choice was made
    !! at `%fit`. The control is the same curve with no `method=`, which the grid fills.
    subroutine scenario_kde_curve_method_on_binned()
        type(pf_kde) :: k
        real(real64) :: x(8), f(8)

        call k%fit([0.25_real64, 0.5_real64, 0.75_real64], bandwidth=0.1_real64, method="binned")
        call k%curve(x, f, xmin=0.0_real64, xmax=1.0_real64)
        print '(a, es22.15)', "kde control curved: ", f(1)
        call k%curve(x, f, xmin=0.0_real64, xmax=1.0_real64, method="exact")
        print '(a)', "accepted a method= on a curve of a binned fit"
    end subroutine scenario_kde_curve_method_on_binned

    !> `%add` after `%finish` aborts, naming `%clear` as the way back. The control is the same
    !! `%add` before it.
    subroutine scenario_kde_grid_add_after_finish()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        print '(a, i0)', "kde control added: ", g%n()
        call g%finish()
        call g%add([0.25_real64])
        print '(a)', "added to a finished grid"
    end subroutine scenario_kde_grid_add_after_finish

    !> `%merge` after `%finish` aborts for the same reason as `%add`. The control is the same
    !! `%merge` before it.
    subroutine scenario_kde_grid_merge_after_finish()
        type(pf_kde_grid) :: g, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call other%add([0.5_real64])
        call g%merge(other)
        print '(a, i0)', "kde control merged: ", g%n()
        call g%finish()
        call g%merge(other)
        print '(a)', "merged into a finished grid"
    end subroutine scenario_kde_grid_merge_after_finish

    !> `%init(pilot=)` refuses a pilot that was never finished: its table would be read off a
    !! half-filled accumulation. The control is the same pilot, finished.
    subroutine scenario_kde_grid_pilot_unfinished()
        type(pf_kde_grid) :: pilot, open_pilot, g

        call pilot%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call pilot%add([0.2_real64, 0.5_real64, 0.8_real64], finish=.true.)
        call g%init(8, 0.0_real64, 1.0_real64, 0.1_real64, pilot=pilot)
        print '(a, i0)', "kde control initialised: ", g%ncells()
        call open_pilot%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call open_pilot%add([0.2_real64, 0.5_real64, 0.8_real64])
        call g%init(8, 0.0_real64, 1.0_real64, 0.1_real64, pilot=open_pilot)
        print '(a)', "accepted a pilot that was never finished"
    end subroutine scenario_kde_grid_pilot_unfinished
    !
    !> Proves that pf_kde_grid%init refuses an unknown kernel.
    subroutine scenario_kde_grid_unknown_kernel()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, kernel="Box")
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, kernel="triangle")
        print '(a)', "accepted an unknown kernel"
    end subroutine scenario_kde_grid_unknown_kernel
    !
    !> Proves that pf_kde_grid%init refuses boundary= without a bound.
    subroutine scenario_kde_grid_boundary_without_bound()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, boundary="reflect")
        print '(a)', "accepted boundary= without a bound"
    end subroutine scenario_kde_grid_boundary_without_bound
    !
    !> Proves that pf_kde_grid%init refuses a range reaching outside the support.
    subroutine scenario_kde_grid_outside_support()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(4, -0.5_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        print '(a)', "accepted a range reaching below the support"
    end subroutine scenario_kde_grid_outside_support
    !
    !> Proves that pf_kde_grid%init refuses a range that does not start at the bound under
    !> boundary="linear" (R1), having accepted the same grid whose range does.
    subroutine scenario_kde_grid_linear_range_not_at_bound()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 3.0_real64, 0.1_real64, lower=0.0_real64, boundary="linear")
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(4, 0.5_real64, 3.0_real64, 0.1_real64, lower=0.0_real64, boundary="linear")
        print '(a)', "accepted a linear grid starting above its lower bound"
    end subroutine scenario_kde_grid_linear_range_not_at_bound
    !
    !> Proves that pf_kde_grid%init refuses a range narrower than one kernel reach where an edge is
    !> free under boundary="linear" (R2), having accepted a grid of the same width bounded at BOTH
    !> ends, which has no free edge and which R2 does not apply to.
    subroutine scenario_kde_grid_linear_narrow()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 0.2_real64, 0.1_real64, lower=0.0_real64, upper=0.2_real64, &
            boundary="linear")
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(4, 0.0_real64, 0.2_real64, 0.1_real64, lower=0.0_real64, boundary="linear")
        print '(a)', "accepted a linear grid narrower than one kernel reach with a free edge"
    end subroutine scenario_kde_grid_linear_narrow
    !
    !> Proves that pf_kde_grid%add refuses a grid that was never initialised.
    subroutine scenario_kde_grid_add_uninitialised()
        type(pf_kde_grid) :: g, fresh

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        print '(a, i0)', "kde control added: ", g%n()
        call fresh%add([0.5_real64])
        print '(a)', "added to a grid that was never initialised"
    end subroutine scenario_kde_grid_add_uninitialised
    !
    !> Proves that pf_kde_grid%add refuses threads=0.
    subroutine scenario_kde_grid_threads_zero()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64], threads=1)
        print '(a, i0)', "kde control added: ", g%n()
        call g%add([0.5_real64], threads=0)
        print '(a)', "accepted threads=0"
    end subroutine scenario_kde_grid_threads_zero
    !
    !> Proves that pf_kde_grid%add refuses weights of the wrong length, in the family's words.
    subroutine scenario_kde_grid_weights_size()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], weights=[1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a, i0)', "kde control added: ", g%n()
        call g%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], weights=[1.0_real64, 1.0_real64, 1.0_real64])
        print '(a)', "accepted three weights for four values"
    end subroutine scenario_kde_grid_weights_size
    !
    !> Proves that pf_kde_grid%add's real32 form refuses weights of the wrong length.
    subroutine scenario_kde_grid_real32_weights_size()
        type(pf_kde_grid) :: g
        real(real32) :: x(4)

        x = [0.1, 0.2, 0.3, 0.4]
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add(x, weights=[1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64])
        print '(a, i0)', "kde control added: ", g%n()
        call g%add(x, weights=[1.0_real64, 1.0_real64])
        print '(a)', "accepted two weights for four values"
    end subroutine scenario_kde_grid_real32_weights_size
    !
    !> Proves that pf_kde_grid%add refuses a negative weight, in the family's words.
    subroutine scenario_kde_grid_negative_weight()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.1_real64, 0.2_real64, 0.3_real64], weights=[1.0_real64, 0.0_real64, 1.0_real64])
        print '(a, i0)', "kde control added: ", g%n()
        call g%add([0.1_real64, 0.2_real64, 0.3_real64], weights=[1.0_real64, -1.0_real64, 1.0_real64])
        print '(a)', "accepted a negative weight"
    end subroutine scenario_kde_grid_negative_weight
    !
    !> Proves that pf_kde_grid%density refuses an output of the wrong size.
    subroutine scenario_kde_grid_density_size()
        type(pf_kde_grid) :: g
        real(real64) :: f4(4), f3(3)

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%density(f4)
        print '(a, es22.15)', "kde control density: ", f4(2)
        call g%density(f3)
        print '(a, es22.15)', "accepted three densities for four cells: ", f3(1)
    end subroutine scenario_kde_grid_density_size
    !
    !> Proves that pf_kde_grid%density refuses centres of the wrong size.
    subroutine scenario_kde_grid_density_x_size()
        type(pf_kde_grid) :: g
        real(real64) :: f4(4), x4(4), x5(5)

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%density(f4, x=x4)
        print '(a, es22.15)', "kde control density: ", x4(2)
        call g%density(f4, x=x5)
        print '(a, es22.15)', "accepted five centres for four cells: ", x5(1)
    end subroutine scenario_kde_grid_density_x_size
    !
    !> Proves that pf_kde_grid%grid refuses an output of the wrong size.
    subroutine scenario_kde_grid_centres_size()
        type(pf_kde_grid) :: g
        real(real64) :: x4(4), x3(3)

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%grid(x4)
        print '(a, es22.15)', "kde control centres: ", x4(2)
        call g%grid(x3)
        print '(a, es22.15)', "accepted three centres for four cells: ", x3(1)
    end subroutine scenario_kde_grid_centres_size
    !
    !> Proves that pf_kde_grid%pdf refuses an output of the wrong size.
    subroutine scenario_kde_grid_pdf_size()
        type(pf_kde_grid) :: g
        real(real64) :: t(3), f3(3), f2(2)

        t = [0.2_real64, 0.5_real64, 0.8_real64]
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%pdf(t, f3)
        print '(a, es22.15)', "kde control density: ", f3(2)
        call g%pdf(t, f2)
        print '(a, es22.15)', "accepted two densities for three points: ", f2(1)
    end subroutine scenario_kde_grid_pdf_size
    !
    !> Proves that pf_kde_grid%cdf refuses an output of the wrong size.
    subroutine scenario_kde_grid_cdf_size()
        type(pf_kde_grid) :: g
        real(real64) :: t(3), p3(3), p2(2)

        t = [0.2_real64, 0.5_real64, 0.8_real64]
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%cdf(t, p3)
        print '(a, es22.15)', "kde control probability: ", p3(2)
        call g%cdf(t, p2)
        print '(a, es22.15)', "accepted two probabilities for three points: ", p2(1)
    end subroutine scenario_kde_grid_cdf_size
    !
    !> Proves that pf_kde_grid%quantile refuses an output of the wrong size.
    subroutine scenario_kde_grid_quantile_size()
        type(pf_kde_grid) :: g
        real(real64) :: p(3), q3(3), q2(2)

        p = [0.2_real64, 0.5_real64, 0.8_real64]
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%quantile(p, q3)
        print '(a, es22.15)', "kde control quantile: ", q3(2)
        call g%quantile(p, q2)
        print '(a, es22.15)', "accepted two quantiles for three probabilities: ", q2(1)
    end subroutine scenario_kde_grid_quantile_size
    !
    !> Proves that pf_kde_grid%quantile refuses p above one.
    subroutine scenario_kde_grid_quantile_p()
        type(pf_kde_grid) :: g
        real(real64) :: q

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%quantile(1.0_real64, q)
        print '(a, es22.15)', "kde control quantile: ", q
        call g%quantile(1.5_real64, q)
        print '(a, es22.15)', "accepted p above one: ", q
    end subroutine scenario_kde_grid_quantile_p
    !
    !> Proves that pf_kde_grid%pdf refuses a grid that was never initialised.
    subroutine scenario_kde_grid_query_uninitialised()
        type(pf_kde_grid) :: g, fresh
        real(real64) :: f

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.5_real64])
        call g%finish()
        call g%pdf(0.5_real64, f)
        print '(a, es22.15)', "kde control density: ", f
        call fresh%pdf(0.5_real64, f)
        print '(a, es22.15)', "answered from a grid that was never initialised: ", f
    end subroutine scenario_kde_grid_query_uninitialised
    !
    !> Proves that pf_kde_grid%ncells refuses a grid that was never initialised.
    subroutine scenario_kde_grid_accessor_uninitialised()
        type(pf_kde_grid) :: g, fresh

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        print '(a, i0)', "kde control cells: ", g%ncells()
        print '(a, i0)', "answered from a grid that was never initialised: ", fresh%ncells()
    end subroutine scenario_kde_grid_accessor_uninitialised
    !
    !> Proves that pf_kde_grid%merge refuses another grid that was never initialised.
    subroutine scenario_kde_grid_merge_uninitialised()
        type(pf_kde_grid) :: g, other, fresh

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(other)
        print '(a, i0)', "kde control merged: ", g%n()
        call g%merge(fresh)
        print '(a)', "merged a grid that was never initialised"
    end subroutine scenario_kde_grid_merge_uninitialised
    !
    !> Proves that pf_kde_grid%merge refuses a different number of cells.
    subroutine scenario_kde_grid_merge_cells()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(5, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(other)
        print '(a)', "merged grids with a different number of cells"
    end subroutine scenario_kde_grid_merge_cells
    !
    !> Proves that pf_kde_grid%merge refuses a different range.
    subroutine scenario_kde_grid_merge_range()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 2.0_real64, 0.1_real64)
        call g%merge(other)
        print '(a)', "merged grids with a different range"
    end subroutine scenario_kde_grid_merge_range
    !
    !> Proves that pf_kde_grid%merge refuses a different bandwidth.
    subroutine scenario_kde_grid_merge_bandwidth()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 1.0_real64, 0.2_real64)
        call g%merge(other)
        print '(a)', "merged grids with a different bandwidth"
    end subroutine scenario_kde_grid_merge_bandwidth
    !
    !> Proves that pf_kde_grid%merge refuses a different kernel.
    subroutine scenario_kde_grid_merge_kernel()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, kernel="box")
        call g%merge(other)
        print '(a)', "merged grids with a different kernel"
    end subroutine scenario_kde_grid_merge_kernel
    !
    !> Proves that pf_kde_grid%merge refuses a different support.
    subroutine scenario_kde_grid_merge_support()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        call g%merge(other)
        print '(a)', "merged grids with a different support"
    end subroutine scenario_kde_grid_merge_support
    !
    !> Proves that pf_kde_grid%merge refuses a different boundary correction.
    subroutine scenario_kde_grid_merge_boundary()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        ! `g` took the default correction, which is `"reflect"`; `other` names another, so that the
        ! two differ however the default moves.
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64, boundary="renormalise")
        call g%merge(other)
        print '(a)', "merged grids with a different boundary correction"
    end subroutine scenario_kde_grid_merge_boundary
    !
    !> Proves that pf_kde_grid%merge refuses a grid filled by another method.
    !!
    !! The control is deliberately stronger than its siblings' above: both grids carry points, so it
    !! asserts that a merge which PASSES the guard adds the counts (3 + 4 = 7) rather than merely
    !! returning. The method guard is the one whose failure would mix a binned grid's bins into an
    !! exact grid's cells, so the merge it admits has to be shown to move the data.
    subroutine scenario_kde_grid_merge_method()
        type(pf_kde_grid) :: g, same, other

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.1_real64, 0.2_real64, 0.3_real64])
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call same%add([0.4_real64, 0.5_real64, 0.6_real64, 0.7_real64])
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        ! `g` took the default method, which is `"exact"`; `other` names the other one, so that the
        ! two differ however the default moves. Nothing `kde_binned_setup` fixes is read by a guard
        ! ahead of the method's, so this pair reaches that guard and no earlier one.
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, method="binned")
        call g%merge(other)
        print '(a)', "merged a binned grid into an exact one"
    end subroutine scenario_kde_grid_merge_method
    !
    ! ---- parquet_kde: the adaptive kernel's caller contracts, in both forms ----------------
    !
    !> Each scenario first makes the nearest LEGAL call and prints a "kde control" line, as the
    !> scenarios above do, then the smallest call that provokes exactly one abort.
    !
    !> Proves that pf_kde%fit refuses alpha= without adaptive=.true..
    subroutine scenario_kde_alpha_without_adaptive()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true., alpha=0.5_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, alpha=0.5_real64)
        print '(a)', "accepted alpha= without adaptive=.true."
    end subroutine scenario_kde_alpha_without_adaptive
    !
    !> Proves that pf_kde%fit refuses bandwidth_max= with adaptive=.false..
    subroutine scenario_kde_bandwidth_max_without_adaptive()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., bandwidth_max=0.2_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.false., bandwidth_max=0.2_real64)
        print '(a)', "accepted bandwidth_max= with adaptive=.false."
    end subroutine scenario_kde_bandwidth_max_without_adaptive
    !
    !> Proves that pf_kde%fit refuses alpha above one.
    subroutine scenario_kde_alpha_above_one()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true., alpha=1.0_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true., alpha=1.5_real64)
        print '(a)', "accepted alpha = 1.5"
    end subroutine scenario_kde_alpha_above_one
    !
    !> Proves that pf_kde%fit refuses a NaN alpha.
    subroutine scenario_kde_alpha_nan()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true., alpha=0.0_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., alpha=ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a)', "accepted a NaN alpha"
    end subroutine scenario_kde_alpha_nan
    !
    !> Proves that pf_kde%fit refuses a zero bandwidth_max.
    !> `spread_max=` without `adaptive=.true.` is refused, like the other two adaptive settings.
    subroutine scenario_kde_spread_max_without_adaptive()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., spread_max=10.0_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.false., spread_max=10.0_real64)
        print '(a)', "accepted spread_max= with adaptive=.false."
    end subroutine scenario_kde_spread_max_without_adaptive

    !> A spread below one asks the widest kernel to be narrower than the narrowest, which is not a
    !> cap but a contradiction; exactly one is the tightest cap that means anything.
    subroutine scenario_kde_spread_max_below_one()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., spread_max=1.0_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., spread_max=0.5_real64)
        print '(a)', "accepted a spread_max below one"
    end subroutine scenario_kde_spread_max_below_one

    !> The DEFAULT spread cap binding is advised; the same fit with the caller's own cap is silent.
    !>
    !> Both arms are scenarios rather than assertions in the suite because an advice goes to the
    !> message stream of a whole process. The two differ in one argument, so a difference in what
    !> they print is the advice and nothing else.
    subroutine scenario_kde_spread_cap_advice_default()
        type(pf_kde) :: k
        type(pf_kde_grid) :: pilot

        call spread_cap_pilot(pilot)
        call k%fit(spread_cap_sample(), bandwidth=0.05_real64, adaptive=.true., pilot=pilot)
        print '(a, l1)', "kde default-cap fit: ", k%is_adaptive()
    end subroutine scenario_kde_spread_cap_advice_default

    !> The silent arm: the same sample and the same pilot, with the caller naming the cap.
    subroutine scenario_kde_spread_cap_advice_explicit()
        type(pf_kde) :: k
        type(pf_kde_grid) :: pilot

        call spread_cap_pilot(pilot)
        call k%fit(spread_cap_sample(), bandwidth=0.05_real64, adaptive=.true., pilot=pilot, &
            spread_max=100.0_real64)
        print '(a, l1)', "kde explicit-cap fit: ", k%is_adaptive()
    end subroutine scenario_kde_spread_cap_advice_explicit

    !> The zone advice names the bandwidth and the spread a caller must stay under, and this arm
    !> emits it: an adaptive fit whose corrected zones cover the whole support, with a
    !> `bandwidth_max=` that the caller chose and that is not narrow enough.
    !>
    !> The two numbers are fixed by the fixture rather than by arithmetic a compiler could move.
    !> `bandwidth_max` is `width/(2 R)` less a percent -- `2/(4 sqrt(3))` for this support and
    !> kernel, which is `0.2887` -- and the spread is that over the narrowest bandwidth the rule
    !> gives this sample. Both sit a fifth of a percent from the nearest rounding boundary, which
    !> is a thousand times any difference a library's `exp` and `log` could introduce.
    subroutine scenario_kde_zone_advice_capped()
        type(pf_kde) :: k

        call k%fit(zone_advice_sample(), bandwidth=0.2_real64, adaptive=.true., &
            bandwidth_max=0.5_real64, lower=0.0_real64, upper=2.0_real64, boundary="linear", &
            kernel="bspline")
        print '(a, l1)', "kde zone-advice fit: ", k%is_adaptive()
    end subroutine scenario_kde_zone_advice_capped

    !> The silent arm for the bandwidth the advice names: the same fit at `bandwidth_max=0.286`
    !> says nothing, which is what makes that number a remedy rather than a decoration. A rendering
    !> that rounded the bound UP would name a value this arm still emits at.
    subroutine scenario_kde_zone_advice_at_bound()
        type(pf_kde) :: k

        call k%fit(zone_advice_sample(), bandwidth=0.2_real64, adaptive=.true., &
            bandwidth_max=0.286_real64, lower=0.0_real64, upper=2.0_real64, boundary="linear", &
            kernel="bspline")
        print '(a, l1)', "kde zone-advice bounded fit: ", k%is_adaptive()
    end subroutine scenario_kde_zone_advice_at_bound

    !> The silent arm for the spread the advice names, which is the same claim about the other
    !> argument: the two are one bound expressed in two units and both have to hold.
    subroutine scenario_kde_zone_advice_at_spread()
        type(pf_kde) :: k

        call k%fit(zone_advice_sample(), bandwidth=0.2_real64, adaptive=.true., &
            spread_max=2.58_real64, lower=0.0_real64, upper=2.0_real64, boundary="linear", &
            kernel="bspline")
        print '(a, l1)', "kde zone-advice spread fit: ", k%is_adaptive()
    end subroutine scenario_kde_zone_advice_at_spread

    !> The same zones over a FIXED bandwidth, where `spread_max=` and `bandwidth_max=` are not
    !> arguments `%fit` accepts at all -- it refuses both without `adaptive=.true.`. The advice
    !> must name `bandwidth=` here and must not name a cap, because advice whose remedy aborts the
    !> call is worse than advice that names no argument.
    subroutine scenario_kde_zone_advice_fixed()
        type(pf_kde) :: k

        call k%fit(zone_advice_sample(), bandwidth=0.5_real64, lower=0.0_real64, upper=2.0_real64, &
            boundary="linear", kernel="bspline")
        print '(a, l1)', "kde zone-advice fixed fit: ", k%is_adaptive()
    end subroutine scenario_kde_zone_advice_fixed

    !> A sample over `[0, 2]` crowded towards the lower bound, so that the adaptive rule spreads
    !> the bandwidths widely enough for the corrected zones to meet across the support.
    function zone_advice_sample() result(x)
        real(real64) :: x(400) !! the sample
        integer :: i

        do i = 1, 400
            x(i) = 2.0_real64*((real(i, real64) - 0.5_real64)/400.0_real64)**2
        end do
    end function zone_advice_sample

    !> A pilot measured over `[0, 1]` alone, which the sample below reaches well beyond.
    subroutine spread_cap_pilot(pilot)
        type(pf_kde_grid), intent(inout) :: pilot !! the pilot, finished
        real(real64) :: y(200)
        integer :: i

        do i = 1, 200
            y(i) = (real(i, real64) - 0.5_real64)/200.0_real64
        end do
        call pilot%init(64, 0.0_real64, 1.0_real64, 0.05_real64)
        call pilot%add(y)
        call pilot%finish()
    end subroutine spread_cap_pilot

    !> A sample whose upper half lies outside that pilot's range, where it reads no density at all
    !> and the rule answers with the cap -- so the cap binds for certain rather than by arithmetic
    !> that a change to the pilot grid could move.
    function spread_cap_sample() result(x)
        real(real64) :: x(100) !! the sample
        integer :: i

        do i = 1, 100
            x(i) = 0.02_real64*real(i, real64)
        end do
    end function spread_cap_sample

    subroutine scenario_kde_bandwidth_max_zero()
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., bandwidth_max=1.0e-300_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., bandwidth_max=0.0_real64)
        print '(a)', "accepted a zero bandwidth_max"
    end subroutine scenario_kde_bandwidth_max_zero
    !
    !> Proves that pf_kde%bandwidths refuses an output of the wrong size.
    subroutine scenario_kde_bandwidths_size()
        type(pf_kde) :: k
        real(real64) :: h4(4), h3(3)

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true.)
        call k%bandwidths(h4)
        print '(a, es22.15)', "kde control bandwidths: ", h4(1)
        call k%bandwidths(h3)
        print '(a, es22.15)', "accepted three bandwidths for four points: ", h3(1)
    end subroutine scenario_kde_bandwidths_size
    !
    !> Proves that pf_kde%bandwidths refuses points of the wrong size.
    subroutine scenario_kde_bandwidths_x_size()
        type(pf_kde) :: k
        real(real64) :: h4(4), x4(4), x5(5)

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true.)
        call k%bandwidths(h4, x=x4)
        print '(a, es22.15)', "kde control bandwidths: ", x4(1)
        call k%bandwidths(h4, x=x5)
        print '(a, es22.15)', "accepted five points for four: ", x5(1)
    end subroutine scenario_kde_bandwidths_x_size
    !
    !> Proves that pf_kde%bandwidth_at refuses an output of the wrong size.
    subroutine scenario_kde_bandwidth_at_size()
        type(pf_kde) :: k
        real(real64) :: h2(2), h3(3)

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true.)
        call k%bandwidth_at([0.1_real64, 0.2_real64], h2)
        print '(a, es22.15)', "kde control bandwidth_at: ", h2(1)
        call k%bandwidth_at([0.1_real64, 0.2_real64], h3)
        print '(a, es22.15)', "accepted three bandwidths for two points: ", h3(1)
    end subroutine scenario_kde_bandwidth_at_size
    !
    !> Proves that pf_kde%pilot refuses a fit that is not adaptive.
    subroutine scenario_kde_pilot_not_adaptive()
        type(pf_kde) :: k
        type(pf_kde_grid) :: g

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, adaptive=.true.)
        call k%pilot(g)
        print '(a, i0)', "kde control pilot: ", g%ncells()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64)
        call k%pilot(g)
        print '(a, l1)', "handed back a pilot of a fixed fit: ", g%is_initialised()
    end subroutine scenario_kde_pilot_not_adaptive
    !
    !> Proves that pf_kde_grid%init refuses alpha= without pilot=.
    subroutine scenario_kde_grid_alpha_without_pilot()
        type(pf_kde_grid) :: g, p

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64)
        print '(a, l1)', "kde control initialised: ", g%is_adaptive()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, alpha=0.5_real64)
        print '(a)', "accepted alpha= without pilot="
    end subroutine scenario_kde_grid_alpha_without_pilot
    !
    !> Proves that pf_kde_grid%init refuses bandwidth_max= without pilot=.
    subroutine scenario_kde_grid_bandwidth_max_without_pilot()
        type(pf_kde_grid) :: g, p

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, bandwidth_max=0.2_real64)
        print '(a, l1)', "kde control initialised: ", g%is_adaptive()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, bandwidth_max=0.2_real64)
        print '(a)', "accepted bandwidth_max= without pilot="
    end subroutine scenario_kde_grid_bandwidth_max_without_pilot
    !
    !> Proves that pf_kde_grid%init refuses a negative alpha.
    subroutine scenario_kde_grid_alpha_negative()
        type(pf_kde_grid) :: g, p

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, alpha=0.0_real64)
        print '(a, l1)', "kde control initialised: ", g%is_adaptive()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, alpha=-0.1_real64)
        print '(a)', "accepted alpha = -0.1"
    end subroutine scenario_kde_grid_alpha_negative
    !
    !> Proves that pf_kde_grid%init refuses a NaN bandwidth_max.
    subroutine scenario_kde_grid_bandwidth_max_nan()
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        type(pf_kde_grid) :: g, p

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, bandwidth_max=0.2_real64)
        print '(a, l1)', "kde control initialised: ", g%is_adaptive()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, bandwidth_max=ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a)', "accepted a NaN bandwidth_max"
    end subroutine scenario_kde_grid_bandwidth_max_nan
    !
    !> Proves that pf_kde_grid%init refuses a pilot that was never initialised.
    subroutine scenario_kde_grid_pilot_uninitialised()
        type(pf_kde_grid) :: g, p, fresh

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        print '(a, l1)', "kde control initialised: ", g%is_adaptive()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=fresh)
        print '(a)', "accepted a pilot that was never initialised"
    end subroutine scenario_kde_grid_pilot_uninitialised
    !
    !> Proves that pf_kde%fit refuses pilot= without adaptive=.true.
    subroutine scenario_kde_fit_pilot_without_adaptive()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64)
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, pilot=p)
        print '(a)', "accepted a pilot without the adaptive kernel"
    end subroutine scenario_kde_fit_pilot_without_adaptive
    !
    !> Proves that pf_kde%fit refuses a pilot that is still accumulating.
    subroutine scenario_kde_fit_pilot_unfinished()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p, open_grid
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64)
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call open_grid%init(16, 0.0_real64, 7.0_real64, 0.5_real64)
        call open_grid%add(x)
        call k%fit(x, adaptive=.true., pilot=open_grid)
        print '(a)', "accepted a pilot that was never finished"
    end subroutine scenario_kde_fit_pilot_unfinished
    !
    !> Proves that pf_kde%fit refuses a pilot built with another kernel: a bandwidth read from it
    !> is a number from a different density.
    subroutine scenario_kde_fit_pilot_kernel()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64, kernel="epanechnikov")
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p, kernel="epanechnikov")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adaptive=.true., pilot=p, kernel="gaussian")
        print '(a)', "accepted a pilot built with another kernel"
    end subroutine scenario_kde_fit_pilot_kernel
    !
    !> Proves that pf_kde%fit refuses a pilot built under another boundary correction.
    subroutine scenario_kde_fit_pilot_boundary()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64, lower=0.0_real64, boundary="reflect")
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.0_real64, boundary="renormalise")
        print '(a)', "accepted a pilot built under another boundary correction"
    end subroutine scenario_kde_fit_pilot_boundary
    !
    !> Proves that pf_kde%fit refuses a pilot built over another support.
    subroutine scenario_kde_fit_pilot_support()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64, lower=0.0_real64, boundary="reflect")
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.5_real64, boundary="reflect")
        print '(a)', "accepted a pilot built over another support"
    end subroutine scenario_kde_fit_pilot_support
    !
    !> Proves that pf_kde%fit refuses a pilot bounded on a DIFFERENT SIDE from the fit, having
    !> accepted the same pilot where the two agree. `kde_fit_pilot_support` is its sibling: there
    !> the two agree on which sides are bounded and differ in the lower bound's VALUE.
    subroutine scenario_kde_fit_pilot_support_side()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64, lower=0.0_real64, boundary="reflect")
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adaptive=.true., pilot=p, lower=0.0_real64, upper=7.0_real64, boundary="reflect")
        print '(a)', "accepted a pilot bounded on one side by a fit bounded on both"
    end subroutine scenario_kde_fit_pilot_support_side
    !
    !> Proves that pf_kde%fit refuses a pilot whose UPPER bound differs from the fit's, having
    !> accepted the same pilot where the two agree. `kde_fit_pilot_support` covers the lower one.
    subroutine scenario_kde_fit_pilot_upper()
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(6)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        call p%init(16, 0.0_real64, 7.0_real64, 0.5_real64, upper=7.0_real64, boundary="reflect")
        call p%add(x)
        call p%finish()
        call k%fit(x, adaptive=.true., pilot=p, upper=7.0_real64, boundary="reflect")
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(x, adaptive=.true., pilot=p, upper=6.5_real64, boundary="reflect")
        print '(a)', "accepted a pilot built to another upper bound"
    end subroutine scenario_kde_fit_pilot_upper
    !
    !> Proves that pf_kde%fit refuses a spread_max that is not finite, having accepted a finite one
    !> above 1. `kde_spread_max_below_one` covers the other half of the same rule.
    subroutine scenario_kde_spread_max_not_finite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf
        type(pf_kde) :: k

        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., spread_max=2.0_real64)
        print '(a, l1)', "kde control fitted: ", k%is_adaptive()
        call k%fit([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64], bandwidth=0.1_real64, &
            adaptive=.true., spread_max=ieee_value(1.0_real64, ieee_positive_inf))
        print '(a)', "accepted an infinite spread_max"
    end subroutine scenario_kde_spread_max_not_finite
    !
    !> Proves that pf_kde%curve refuses an xmax BELOW the default range's own start, which leaves
    !> no range although the caller passed only one end; the control passes an xmax above it.
    !> `kde_curve_reversed_range` is the sibling where the caller passed both ends.
    subroutine scenario_kde_curve_one_end_collapses()
        type(pf_kde) :: k
        real(real64) :: g(8), f(8)

        call k%fit([1.0_real64, 2.0_real64, 3.0_real64], bandwidth=0.5_real64)
        call k%curve(g, f, xmax=5.0_real64)
        print '(a, es22.15)', "kde control curved: ", g(1)
        call k%curve(g, f, xmax=-100.0_real64)
        print '(a)', "accepted an xmax below the default range's start"
    end subroutine scenario_kde_curve_one_end_collapses
    !
    !> Proves that pf_kde%curve refuses a DEFAULT range that collapses to one point -- a sample of
    !> one value at cut=0 -- with the other of the binding's two texts, which names what actually
    !> collapsed rather than the ends the caller did not pass. The control is the same sample at a
    !> cut that leaves a range.
    subroutine scenario_kde_curve_default_range_collapses()
        type(pf_kde) :: k
        real(real64) :: g(8), f(8)

        call k%fit([3.0_real64, 3.0_real64, 3.0_real64], bandwidth=0.5_real64)
        call k%curve(g, f, cut=1.0_real64)
        print '(a, es22.15)', "kde control curved: ", g(1)
        call k%curve(g, f, cut=0.0_real64)
        print '(a)', "accepted a default range that collapsed to one point"
    end subroutine scenario_kde_curve_default_range_collapses
    !
    !> Proves that pf_kde_grid%init refuses a cell width that UNDERFLOWS to zero, which the range
    !> check before it cannot see: the range is positive, and one cell holds it, but two do not.
    !> `kde_grid_cell_width` is the sibling at the other end, where the range is wider than the
    !> largest number.
    !!
    !! **The range is the smallest one THIS BUILD can halve to zero**, and that is not one number
    !! under the two underflow models. Where underflow is gradual the smallest subnormal is the
    !! only such range, since the smallest normal halves to a perfectly good subnormal; where
    !! subnormals are flushed to zero -- ifx turns flush-to-zero and denormals-are-zero on at
    !! `-O1` and above, which is what a flagless `fpm test` selects -- the smallest normal already
    !! halves to zero, and a SUBNORMAL range would not survive the journey: denormals-are-zero
    !! collapses it on the comparison that reads it, so `%init` would see a range of zero and
    !! refuse it by the range rule above instead, leaving this rule untested and the control call
    !! aborting. Choosing by the mode asks the same question of the library at either floor.
    subroutine scenario_kde_grid_cell_width_underflow()
        use, intrinsic :: ieee_arithmetic, only : ieee_support_underflow_control, ieee_get_underflow_mode
        type(pf_kde_grid) :: g
        real(real64) :: range
        logical :: gradual

        gradual = .true.
        if (ieee_support_underflow_control(1.0_real64)) call ieee_get_underflow_mode(gradual)
        if (gradual) then
            range = tiny(1.0_real64)*epsilon(1.0_real64)
        else
            range = tiny(1.0_real64)
        end if
        call g%init(1, 0.0_real64, range, 1.0_real64)
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(2, 0.0_real64, range, 1.0_real64)
        print '(a)', "accepted a cell width that underflowed to zero"
    end subroutine scenario_kde_grid_cell_width_underflow
    !
    !> Proves that pf_kde_grid%init applies R1 at the UPPER bound too: the range must END at it,
    !> having accepted the same grid whose range does. `kde_grid_linear_range_not_at_bound` is the
    !> lower half of the same rule.
    subroutine scenario_kde_grid_linear_range_not_at_upper()
        type(pf_kde_grid) :: g

        call g%init(4, 0.0_real64, 3.0_real64, 0.1_real64, upper=3.0_real64, boundary="linear")
        print '(a, es22.15)', "kde control initialised: ", g%step()
        call g%init(4, 0.0_real64, 2.5_real64, 0.1_real64, upper=3.0_real64, boundary="linear")
        print '(a)', "accepted a linear grid ending below its upper bound"
    end subroutine scenario_kde_grid_linear_range_not_at_upper
    !
    !> Proves that pf_kde_grid%init refuses a spread_max that is not a number, having accepted a
    !> finite one above 1; `kde_grid_spread_max_below_one` covers the rule's other half. The fit's
    !> own siblings are `kde_spread_max_not_finite` and `kde_spread_max_below_one`.
    subroutine scenario_kde_grid_spread_max_not_finite()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(pf_kde_grid) :: p, g

        call spread_max_pilot(p)
        call g%init(16, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, spread_max=2.0_real64)
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(16, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, &
            spread_max=ieee_value(1.0_real64, ieee_quiet_nan))
        print '(a)', "accepted a NaN spread_max"
    end subroutine scenario_kde_grid_spread_max_not_finite
    !
    !> Proves that pf_kde_grid%init refuses a spread_max below one -- a widest kernel narrower than
    !> the narrowest, which is a contradiction and not a cap -- having accepted one above it.
    subroutine scenario_kde_grid_spread_max_below_one()
        type(pf_kde_grid) :: p, g

        call spread_max_pilot(p)
        call g%init(16, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, spread_max=2.0_real64)
        print '(a, es22.15)', "kde control initialised: ", g%bandwidth()
        call g%init(16, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, spread_max=0.5_real64)
        print '(a)', "accepted a spread_max below one"
    end subroutine scenario_kde_grid_spread_max_below_one
    !
    !> The finished pilot both grid spread_max scenarios read: a uniform sample over the unit
    !> interval, which is all `%init` needs to accept `spread_max=` at all.
    subroutine spread_max_pilot(p)
        type(pf_kde_grid), intent(out) :: p !! the finished pilot
        real(real64) :: x(64)
        integer :: i

        do i = 1, 64
            x(i) = (real(i, real64) - 0.5_real64)/64.0_real64
        end do
        call p%init(32, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add(x)
        call p%finish()
    end subroutine spread_max_pilot
    !
    !> Says that a `method = "binned"` fit took fewer cells than its narrowest kernel asked for.
    !>
    !> Exits 0 -- a clamped cell count is a remark about the configuration, not an abort. A
    !> scenario rather than an assertion in the suite for the reason the other advice scenarios
    !> give: an advice goes to the message stream of a whole process. Four thousand points one
    !> apart at a thousandth of a bandwidth ask for about 64 million cells against a cap of 65536.
    subroutine scenario_kde_fit_cells_clamped_advice()
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        integer :: i
        logical :: ok

        allocate(x(4000))
        do i = 1, 4000
            x(i) = real(i, real64)
        end do
        call k%fit(x, bandwidth=1.0e-3_real64, method="binned", ok=ok)
        print '(a, l1)', "kde binned fit clamped: ", ok
    end subroutine scenario_kde_fit_cells_clamped_advice
    !
    !> The zone advice on a support so narrow that the bandwidth it names falls outside the range a
    !> fixed rendering reads back, so the advice writes it in the exponent form instead.
    !>
    !> Exits 0, as the other zone advice scenarios do: the fit is right, only far dearer than its
    !> arguments suggest.
    subroutine scenario_kde_zone_advice_tiny_support()
        type(pf_kde) :: k
        real(real64) :: x(64)
        integer :: i
        logical :: ok

        do i = 1, 64
            x(i) = 1.0e-6_real64*(real(i, real64) - 0.5_real64)/64.0_real64
        end do
        call k%fit(x, bandwidth=1.0e-3_real64, lower=0.0_real64, upper=1.0e-6_real64, &
            boundary="linear", ok=ok)
        print '(a, l1)', "kde tiny-support fit: ", ok
    end subroutine scenario_kde_zone_advice_tiny_support
    !
    !> Proves that pf_kde_grid%init refuses a binned geometry whose transform length, ROUNDED UP to
    !> a power of two, passes what its cells may carry -- the third of the three lengths
    !> `kde_binned_setup` tests, where the pad and the cells themselves both fit. Seven cells over
    !> the unit interval want 61 pad cells at this bandwidth: 129 in all, which rounds to 256
    !> against a ceiling of 224. The control is the same grid at a bandwidth whose rounded length
    !> is 128.
    subroutine scenario_kde_grid_binned_transform_length()
        type(pf_kde_grid) :: g

        call g%init(7, 0.0_real64, 1.0_real64, 2.0_real64, method="binned")
        print '(a, i0)', "kde control initialised: ", g%ncells()
        call g%init(7, 0.0_real64, 1.0_real64, 2.45_real64, method="binned")
        print '(a)', "accepted a transform whose rounded length passes the cells' ceiling"
    end subroutine scenario_kde_grid_binned_transform_length
    !
    !> Emits R3's WARNING at a chosen verbosity, so the wrapper can assert the CLASS: a finding
    !> about the caller's data survives `"silent"` and goes quiet only at `"errors_only"`, unlike
    !> advice, which `"silent"` already suppresses.
    !>
    !> Exits 0 -- R3 is a data condition, not an abort. The control is inside the process: the
    !> `%n_overreach` line is printed on every arm, so an arm with no WARNING is told apart from a
    !> build where R3 never fired at all, which would silence the message for the wrong reason.
    subroutine scenario_kde_overreach_warning(level)
        character(len=*), intent(in) :: level !! verbosity to set first.
        type(pf_kde_grid) :: pilot, g
        real(real64) :: xe(200)
        integer :: i, n_in

        do i = 1, 200
            xe(i) = -log(1.0_real64 - (real(i, real64) - 0.5_real64)/200.0_real64)
        end do
        n_in = count(xe <= 3.0_real64)
        call pilot%init(200, 0.0_real64, 3.0_real64, 0.3_real64, lower=0.0_real64, boundary="linear")
        call pilot%add(xe(1:n_in))
        call pilot%finish()

        call parquet_set_verbosity(level)
        call g%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call g%add(xe(1:n_in))
        call parquet_reset_settings()
        print '(a, i0)', "kde overreach counted: ", g%n_overreach()
    end subroutine scenario_kde_overreach_warning
    !
    !> Proves that pf_kde_grid%merge refuses a grid read from another pilot.
    subroutine scenario_kde_grid_merge_pilot()
        type(pf_kde_grid) :: g, same, other, p, q

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call q%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call q%add([0.5_real64, 0.6_real64, 0.7_real64, 0.8_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call q%finish()
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=q)
        call g%merge(other)
        print '(a)', "merged grids read from different pilots"
    end subroutine scenario_kde_grid_merge_pilot
    !
    !> Proves that pf_kde_grid%merge refuses a fixed grid into an adaptive one.
    subroutine scenario_kde_grid_merge_fixed()
        type(pf_kde_grid) :: g, same, other, p, q

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call q%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call q%add([0.5_real64, 0.6_real64, 0.7_real64, 0.8_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%merge(other)
        print '(a)', "merged a fixed grid into an adaptive one"
    end subroutine scenario_kde_grid_merge_fixed
    !
    !> Proves that pf_kde_grid%merge refuses a grid with another alpha.
    subroutine scenario_kde_grid_merge_alpha()
        type(pf_kde_grid) :: g, same, other, p, q

        call p%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call p%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])
        call q%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call q%add([0.5_real64, 0.6_real64, 0.7_real64, 0.8_real64])
        call p%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call same%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p)
        call g%merge(same)
        print '(a, i0)', "kde control merged: ", g%n()
        call other%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=p, alpha=0.4_real64)
        call g%merge(other)
        print '(a)', "merged grids with different alpha"
    end subroutine scenario_kde_grid_merge_alpha
    !
    !> Proves that pf_kde%pdf refuses threads=0.
    subroutine scenario_kde_pdf_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4), f(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%pdf(x, f, threads=1)
        print '(a, es22.15)', "kde control density: ", f(2)
        call k%pdf(x, f, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", f(2)
    end subroutine scenario_kde_pdf_threads_zero
    !
    !> Proves that pf_kde%cdf refuses threads=0.
    subroutine scenario_kde_cdf_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4), p(4)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%cdf(x, p, threads=1)
        print '(a, es22.15)', "kde control probability: ", p(2)
        call k%cdf(x, p, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", p(2)
    end subroutine scenario_kde_cdf_threads_zero
    !
    !> Proves that pf_kde%quantile refuses threads=0.
    subroutine scenario_kde_quantile_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4), q(2)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%quantile([0.25_real64, 0.75_real64], q, threads=1)
        print '(a, es22.15)', "kde control quantile: ", q(1)
        call k%quantile([0.25_real64, 0.75_real64], q, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", q(1)
    end subroutine scenario_kde_quantile_threads_zero
    !
    !> Proves that pf_kde%curve refuses threads=0.
    subroutine scenario_kde_curve_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4), xg(8), fg(8)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%curve(xg, fg, threads=1)
        print '(a, es22.15)', "kde control curve: ", fg(4)
        call k%curve(xg, fg, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", fg(4)
    end subroutine scenario_kde_curve_threads_zero
    !
    !> Proves that pf_kde%sample refuses threads=0.
    subroutine scenario_kde_sample_threads_zero()
        type(pf_kde) :: k
        real(real64) :: x(4), v(8)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%sample(v, 1_int64, threads=1)
        print '(a, es22.15)', "kde control draw: ", v(1)
        call k%sample(v, 1_int64, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", v(1)
    end subroutine scenario_kde_sample_threads_zero
    !
    !> Proves that pf_kde%sample refuses an object that was never fitted.
    subroutine scenario_kde_sample_unfitted()
        type(pf_kde) :: k, fresh
        real(real64) :: x(4), v(8)

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call k%fit(x, bandwidth=0.5_real64)
        call k%sample(v, 1_int64)
        print '(a, es22.15)', "kde control draw: ", v(1)
        call fresh%sample(v, 1_int64)
        print '(a, es22.15)', "sampled an unfitted estimate: ", v(1)
    end subroutine scenario_kde_sample_unfitted
    !
    !> Proves that pf_kde_grid%sample refuses threads=0.
    subroutine scenario_kde_grid_sample_threads_zero()
        type(pf_kde_grid) :: g
        real(real64) :: v(8)

        call g%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.2_real64, 0.4_real64, 0.6_real64])
        call g%finish()
        call g%sample(v, 1_int64, threads=1)
        print '(a, es22.15)', "kde control draw: ", v(1)
        call g%sample(v, 1_int64, threads=0)
        print '(a, es22.15)', "accepted threads=0: ", v(1)
    end subroutine scenario_kde_grid_sample_threads_zero
    !
    !> Proves that pf_kde_grid%sample refuses a grid that was never initialised.
    subroutine scenario_kde_grid_sample_uninitialised()
        type(pf_kde_grid) :: g, fresh
        real(real64) :: v(8)

        call g%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.2_real64, 0.4_real64, 0.6_real64])
        call g%finish()
        call g%sample(v, 1_int64)
        print '(a, es22.15)', "kde control draw: ", v(1)
        call fresh%sample(v, 1_int64)
        print '(a, es22.15)', "sampled an uninitialised grid: ", v(1)
    end subroutine scenario_kde_grid_sample_uninitialised
    !
    !> Proves that pf_kde%fit refuses a logical column, naming its kind.
    subroutine scenario_kde_fit_logical_column()
        type(pf_kde) :: k
        type(parquet_column) :: c

        call c%init(PK_FLOAT64, 4_int64)
        call c%set_all([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call k%fit(c, bandwidth=0.5_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call c%init(PK_LOGICAL, 4_int64)
        call c%set_all([.true., .false., .true., .true.])
        call k%fit(c, bandwidth=0.5_real64)
        print '(a, es22.15)', "fitted a logical column: ", k%bandwidth()
    end subroutine scenario_kde_fit_logical_column
    !
    !> Proves that pf_kde%fit refuses is_valid= beside a column, which carries its own validity.
    subroutine scenario_kde_fit_column_is_valid()
        type(pf_kde) :: k
        type(parquet_column) :: c

        call c%init(PK_FLOAT64, 4_int64)
        call c%set_all([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call k%fit(c, bandwidth=0.5_real64)
        print '(a, es22.15)', "kde control fitted: ", k%bandwidth()
        call k%fit(c, bandwidth=0.5_real64, is_valid=[.true., .true., .true., .true.])
        print '(a, es22.15)', "accepted is_valid= beside a column: ", k%bandwidth()
    end subroutine scenario_kde_fit_column_is_valid
    !
    !> Proves that pf_kde_grid%add refuses a vector column, naming its kind.
    subroutine scenario_kde_grid_add_vector_column()
        type(pf_kde_grid) :: g
        type(parquet_column) :: c

        call g%init(8, 0.0_real64, 5.0_real64, 0.5_real64)
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call g%add(c)
        print '(a, i0)', "kde control added: ", g%n_valid()
        call c%init(PK_FLOAT64_VEC, 3_int64, width=2_int32)
        call g%add(c)
        print '(a, i0)', "added a vector column: ", g%n_valid()
    end subroutine scenario_kde_grid_add_vector_column
    !
    !> Proves that pf_kde_grid%add refuses is_valid= beside a column, which carries its own validity.
    subroutine scenario_kde_grid_add_column_is_valid()
        type(pf_kde_grid) :: g
        type(parquet_column) :: c

        call g%init(8, 0.0_real64, 5.0_real64, 0.5_real64)
        call c%init(PK_INT64, 3_int64)
        call c%set_all([1_int64, 2_int64, 3_int64])
        call g%add(c)
        print '(a, i0)', "kde control added: ", g%n_valid()
        call g%add(c, is_valid=[.true., .true., .true.])
        print '(a, i0)', "accepted is_valid= beside a column: ", g%n_valid()
    end subroutine scenario_kde_grid_add_column_is_valid
    !
    !> Proves that parquet_debug_set_kde_isj_cells refuses a count that is not a power of two.
    subroutine scenario_kde_isj_cells_not_pow2()
        call parquet_debug_set_kde_isj_cells(1024)
        call parquet_debug_set_kde_isj_cells(0)
        print '(a)', "kde control: 1024 cells accepted"
        call parquet_debug_set_kde_isj_cells(1000)
        print '(a)', "accepted 1000 cells"
    end subroutine scenario_kde_isj_cells_not_pow2
    !
    !> Proves that parquet_debug_set_kde_isj_cells refuses a power of two beyond 2**20.
    subroutine scenario_kde_isj_cells_out_of_range()
        call parquet_debug_set_kde_isj_cells(1048576)
        call parquet_debug_set_kde_isj_cells(0)
        print '(a)', "kde control: 2**20 cells accepted"
        call parquet_debug_set_kde_isj_cells(2097152)
        print '(a)', "accepted 2**21 cells"
    end subroutine scenario_kde_isj_cells_out_of_range
    !

end module error_scenarios_numeric
