!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Abort-path tests for the index containers and the numeric tier, driving the scenarios in
!> `error_scenarios_numeric.f90`: `pf_index_map`/`pf_index_pool`/`pf_index_multimap` and the
!> table's grouping, indexing and filtering verbs, the message-stream routing of
!> `%print_rows`/`%print_stat`, then quadrature, interpolation, cosmology, optimisation, root
!> finding, transforms and KDE.
!!
!! One of three modules split out of `test_errors.f90`, which keeps the subprocess-driving
!! machinery (`run_error_scenario`, the `check_scenario_*` helpers, `prime_error_scenarios`)
!! and the tests for `error_scenarios_io.f90`. The split is for COMPILE TIME: at about 23000
!! lines the one module took some 27 s under ifx, second only to `error_scenarios.f90` itself.
!! Each module's tests mirror one `error_scenarios_*` group, so a scenario and the test that
!! drives it stay in files with the same name.
module test_numeric_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_no_output, &
        check_scenario_exit_status_and_stderr, check_scenario_streams, file_contains, run_error_scenario, &
        scenario_capture_contains
    !
    implicit none
    private
    public :: collect_tests_parquet_numeric_errors
    !
contains

    subroutine collect_tests_parquet_numeric_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end, and every part is capped at 255
        ! continuation lines -- the two traps that shape apart are written out in full in
        ! test_errors.f90's own collect_tests_parquet_errors. WHEN A PART IS FULL, ADD A NEW
        ! ONE rather than growing an existing one, and close it by removing the COMMA, not
        ! the `&`. check_statement_continuation_lines (tools/check_source_conventions.py)
        ! fails the lint stage before nagfor ever sees it.
        type(unittest_type), allocatable :: p1(:), p2(:), p3(:), p4(:), p5(:), p6(:)

        p1 = [ &
            new_unittest("%print_rows refuses a negative last=", test_print_rows_negative_last_aborts), &
            new_unittest("a parquet_date key on an integer index is refused by name", &
                test_table_index_date_key_on_int_aborts), &
            new_unittest("a parquet_time key on an integer index is refused by name", &
                test_table_index_time_key_on_int_aborts), &
            new_unittest("%print_rows follows message_stream when no unit= is given", &
                test_print_rows_follows_message_stream), &
            new_unittest("%print_stat follows message_stream when no unit= is given", &
                test_print_stat_follows_message_stream), &
            new_unittest("both parquet_strings printers follow message_stream", &
                test_string_print_follows_message_stream), &
            new_unittest("parquet_print_settings follows message_stream even at verbosity=silent", &
                test_print_settings_follows_message_stream), &
            new_unittest("%print_schema_info with neither unit= nor filename= writes to the stream", &
                test_print_schema_info_default_stream), &
            new_unittest("a failing close's context lines follow message_stream", &
                test_error_context_follows_message_stream), &
            new_unittest("the C++ reader report follows message_stream", &
                test_reader_print_stat_follows_message_stream), &
            new_unittest("verbosity=silent set after the open still silences the reader report", &
                test_close_reader_print_stat_silenced_late), &
            new_unittest("an unbound parquet_string handle is reported at verbosity=silent too", &
                test_string_print_unbound_handle_silent_aborts), &
            new_unittest("%get_matrix refuses a column that holds no values", &
                test_get_matrix_unsupported_column_aborts), &
            new_unittest("%keep_columns truncates a long list of absent names", &
                test_keep_columns_many_missing_aborts), &
            new_unittest("%join refuses a columns= name whose type cannot be read", &
                test_join_columns_unsupported_aborts), &
            new_unittest("parquet_derive_schema refuses a blank name=", &
                test_derive_schema_blank_name_aborts), &
            new_unittest("parquet_open_writer_like refuses a table with nothing resident", &
                test_open_writer_like_nothing_resident_aborts), &
            new_unittest("a sink schema naming a column that holds no values is refused", &
                test_sink_schema_names_an_unreadable_column_aborts), &
            new_unittest("an overflowing int64 group sum is refused, not wrapped", &
                test_agg_int64_sum_overflow_aborts), &
            new_unittest("an infinite disc centre component is refused by its own guard", &
                test_healpix_disc_vector_infinite_aborts), &
            new_unittest("add_agg with weights= and exact= together is refused", &
                test_table_group_add_agg_weights_exact_aborts), &
            new_unittest("%container_ptr on a non-container column is refused by name", &
                test_column_container_wrong_kind_aborts), &
            new_unittest("%append_row_of from a container column says it is not implemented", &
                test_column_append_row_of_container_aborts), &
            new_unittest("a filter set name past the 64-character limit is refused", &
                test_filter_set_name_too_long_aborts), &
            new_unittest("a filter rule whose set name is a bare @ is refused", &
                test_filter_bare_at_set_name_aborts), &
            new_unittest("an over-long sep is quoted back capped at 100 characters", &
                test_skycoord_text_long_separator_aborts), &
            new_unittest("a duplicate key aborts on the direct backend", &
                test_index_build_duplicate_direct_aborts), &
            new_unittest("a duplicate key aborts on the hash backend", &
                test_index_build_duplicate_hash_aborts), &
            new_unittest("a duplicate key aborts on the sorted backend", &
                test_index_build_duplicate_sorted_aborts), &
            new_unittest("a duplicate tuple aborts on the direct backend", &
                test_index_build_duplicate_tuple_direct_aborts), &
            new_unittest("a duplicate tuple aborts on the hash backend", &
                test_index_build_duplicate_tuple_hash_aborts), &
            new_unittest("storing the value 0 aborts", &
                test_index_build_value_zero_aborts), &
            new_unittest("a values array of the wrong length aborts", &
                test_index_build_values_length_aborts), &
            new_unittest("a get_many length mismatch aborts", &
                test_index_get_many_length_aborts), &
            new_unittest("a lookup of the wrong tuple width aborts", &
                test_index_tuple_width_mismatch_aborts), &
            new_unittest("a scalar lookup on a composite map aborts", &
                test_index_scalar_on_composite_aborts), &
            new_unittest("setting a key on a sorted map aborts", &
                test_index_sorted_set_aborts), &
            new_unittest("removing a key from a sorted map aborts", &
                test_index_sorted_remove_aborts), &
            new_unittest("get_or_add of a new key on a sorted map aborts, naming get_or_add", &
                test_index_sorted_get_or_add_absent_aborts), &
            new_unittest("a composite sorted build aborts", &
                test_index_sorted_composite_aborts), &
            new_unittest("init with method=direct aborts", &
                test_index_init_direct_aborts), &
            new_unittest("init with method=sorted aborts", &
                test_index_init_sorted_aborts), &
            new_unittest("an unknown index method token aborts", &
                test_index_bad_method_aborts), &
            new_unittest("setting outside a direct map's range aborts", &
                test_index_direct_set_out_of_range_aborts), &
            new_unittest("get_or_add outside a direct map's range aborts naming the caller's entry", &
                test_index_direct_get_or_add_out_of_range_aborts), &
            new_unittest("a key wider than the component limit aborts", &
                test_index_ncomp_too_large_aborts), &
            new_unittest("an explicit direct map over the whole int64 range aborts", &
                test_index_direct_range_too_wide_aborts), &
            new_unittest("removing an absent key without found= aborts", &
                test_index_remove_absent_aborts), &
            new_unittest("a stored value too large for an int32 answer aborts", &
                test_index_get_many_int32_overflow_aborts), &
            new_unittest("threads=0 on a build aborts", &
                test_index_build_threads_zero_aborts), &
            new_unittest("a get_many mask of the wrong length aborts", &
                test_index_get_many_valid_length_aborts), &
            new_unittest("threads=0 on a get_many aborts", &
                test_index_get_many_threads_zero_aborts), &
            new_unittest("a get_or_add_many code array of the wrong length aborts", &
                test_index_get_or_add_many_length_aborts), &
            new_unittest("a get_or_add_many mask of the wrong length aborts", &
                test_index_get_or_add_many_valid_length_aborts), &
            new_unittest("a code too large for an int32 answer aborts on get_or_add_many", &
                test_index_get_or_add_many_int32_overflow_aborts), &
            new_unittest("a build mask of the wrong length aborts", &
                test_index_build_valid_length_aborts), &
            new_unittest("a rank-1 key list from a composite map aborts", &
                test_index_keys_rank1_on_composite_aborts), &
            new_unittest("a duplicate in a masked direct build aborts", &
                test_index_masked_duplicate_direct_aborts), &
            new_unittest("a duplicate tuple in a masked direct build aborts", &
                test_index_masked_duplicate_tuple_direct_aborts), &
            new_unittest("storing the value 0 through set aborts", &
                test_index_set_value_zero_aborts), &
            new_unittest("a method token longer than the resolver's buffer aborts", &
                test_index_method_token_too_long_aborts), &
            new_unittest("a bulk integer lookup on a string map aborts", &
                test_index_get_many_on_string_map_aborts), &
            new_unittest("a bulk lookup of the wrong component count aborts", &
                test_index_get_many_ncomp_mismatch_aborts), &
            new_unittest("a rank-2 key list from a string map aborts", &
                test_index_keys_rank2_on_string_map_aborts), &
            new_unittest("a composite direct build whose spans multiply too far aborts", &
                test_index_composite_direct_product_too_wide_aborts), &
            new_unittest("a direct build of unallocatable size aborts with its slot count", &
                test_index_direct_alloc_refused_aborts), &
            new_unittest("every legal pf_index_map path completes", &
                test_index_control_completes), &
            new_unittest("a multimap values array of the wrong length aborts", &
                test_multimap_build_values_length_aborts), &
            new_unittest("storing the value 0 in a multimap aborts", &
                test_multimap_build_value_zero_aborts), &
            new_unittest("a multimap build mask of the wrong length aborts", &
                test_multimap_build_valid_length_aborts), &
            new_unittest("threads=0 on a multimap build aborts", &
                test_multimap_build_threads_zero_aborts), &
            new_unittest("an unknown multimap method token aborts", &
                test_multimap_build_bad_method_aborts), &
            new_unittest("a composite sorted multimap build aborts", &
                test_multimap_build_sorted_composite_aborts), &
            new_unittest("a get_first_many length mismatch aborts", &
                test_multimap_get_first_many_length_aborts), &
            new_unittest("a get_first_many mask of the wrong length aborts", &
                test_multimap_get_first_many_valid_length_aborts), &
            new_unittest("threads=0 on a get_first_many aborts", &
                test_multimap_get_first_many_threads_zero_aborts), &
            new_unittest("a multimap get_many length mismatch aborts", &
                test_multimap_get_many_length_aborts), &
            new_unittest("a probe_many mask of the wrong length aborts", &
                test_multimap_probe_many_valid_length_aborts), &
            new_unittest("threads=0 on a probe_many aborts", &
                test_multimap_probe_many_threads_zero_aborts), &
            new_unittest("a probe_many pair count over the ceiling aborts", &
                test_multimap_probe_many_pair_overflow_aborts), &
            new_unittest("a stored value too large for an int32 bulk answer aborts", &
                test_multimap_int32_answer_overflow_aborts), &
            new_unittest("a stored value too large for an int32 get_all aborts", &
                test_multimap_get_all_int32_overflow_aborts), &
            new_unittest("a multimap lookup of the wrong tuple width aborts", &
                test_multimap_tuple_width_mismatch_aborts), &
            new_unittest("a scalar lookup on a composite multimap aborts", &
                test_multimap_scalar_on_composite_aborts), &
            new_unittest("a rank-1 key list from a composite multimap aborts", &
                test_multimap_keys_rank1_on_composite_aborts), &
            new_unittest("a probe of the wrong tuple width aborts", &
                test_multimap_probe_shape_mismatch_aborts), &
            new_unittest("a key tuple on a string multimap aborts", &
                test_multimap_tuple_on_string_map_aborts), &
            new_unittest("an integer key on a string multimap aborts", &
                test_multimap_integer_on_string_map_aborts), &
            new_unittest("a bulk integer lookup on a string multimap aborts", &
                test_multimap_get_first_many_on_string_map_aborts), &
            new_unittest("a bulk multimap lookup of the wrong component count aborts", &
                test_multimap_get_first_many_ncomp_mismatch_aborts), &
            new_unittest("an integer probe of a string multimap aborts", &
                test_multimap_probe_many_on_string_map_aborts), &
            new_unittest("a direct multimap spanning the int64 domain aborts", &
                test_multimap_direct_range_too_wide_aborts), &
            new_unittest("a composite direct multimap whose spans multiply too far aborts", &
                test_multimap_composite_direct_product_too_wide_aborts), &
            new_unittest("a direct multimap of unallocatable size aborts with its slot count", &
                test_multimap_direct_alloc_refused_aborts), &
            new_unittest("every legal pf_index_multimap path completes", &
                test_multimap_control_completes), &
            new_unittest("a string lookup on an integer map aborts", &
                test_index_string_on_integer_map_aborts), &
            new_unittest("an integer lookup on a string map aborts", &
                test_index_integer_on_string_map_aborts), &
            new_unittest("a tuple lookup on a string map aborts", &
                test_index_tuple_on_string_map_aborts), &
            new_unittest("a string get_many on an integer map aborts", &
                test_index_string_get_many_on_integer_map_aborts), &
            new_unittest("a string build with method=direct aborts", &
                test_index_string_method_direct_aborts), &
            new_unittest("a string build with method=sorted aborts", &
                test_index_string_method_sorted_aborts) &
            ]
        p2 = [ &
            new_unittest("a duplicate string key aborts, named and trimmed", &
                test_index_string_build_duplicate_aborts), &
            new_unittest("an integer key list from a string map aborts", &
                test_index_string_keys_rank1_aborts), &
            new_unittest("a string key column from an integer map aborts", &
                test_index_string_keys_column_on_integer_aborts), &
            new_unittest("init(strings=.true.) with ncomp=2 aborts", &
                test_index_string_init_ncomp_aborts), &
            new_unittest("a string set on an integer map aborts", &
                test_index_string_set_on_integer_map_aborts), &
            new_unittest("removing an absent string key without found= aborts", &
                test_index_string_remove_absent_aborts), &
            new_unittest("a string get_many length mismatch aborts", &
                test_index_string_get_many_length_aborts), &
            new_unittest("a string lookup on an integer multimap aborts", &
                test_multimap_string_on_integer_aborts), &
            new_unittest("an integer lookup on a string multimap aborts", &
                test_multimap_integer_on_string_aborts), &
            new_unittest("a string multimap build with method=sorted aborts", &
                test_multimap_string_method_sorted_aborts), &
            new_unittest("a string probe_many on an integer multimap aborts", &
                test_multimap_string_probe_on_integer_aborts), &
            new_unittest("an integer key list from a string multimap aborts", &
                test_multimap_string_keys_rank1_aborts), &
            new_unittest("an int32 keys answer over a key below int32 aborts", &
                test_index_keys_int32_negative_aborts), &
            new_unittest("a rank-2 int32 keys answer over a key below int32 aborts", &
                test_index_keys_rank2_int32_negative_aborts), &
            new_unittest("an int32 csr over a stored value above int32 aborts", &
                test_multimap_csr_int32_value_aborts), &
            new_unittest("every legal string-key path of both types completes", &
                test_index_string_control_completes), &
            new_unittest("a long run of string keys sharing a hash is reported once", &
                test_index_string_chain_warning), &
            new_unittest("a string key on an integer index aborts", &
                test_table_index_string_key_on_int_aborts), &
            new_unittest("an integer key on a string index aborts", &
                test_table_index_int_key_on_string_aborts), &
            new_unittest("a pool double free aborts", &
                test_pool_double_free_aborts), &
            new_unittest("freeing a never-issued index aborts", &
                test_pool_free_never_issued_aborts), &
            new_unittest("freeing index 0 aborts", &
                test_pool_free_zero_aborts), &
            new_unittest("every legal pf_index_pool path completes", &
                test_pool_control_completes), &
            new_unittest("a bounded table with a sort is refused at open", &
                test_bounded_with_sort_refused), &
            new_unittest("a bounded table with a maml sort list is refused at open", &
                test_bounded_with_maml_sort_refused), &
            new_unittest("a bounded qc violation aborts at first touch, not at open", &
                test_bounded_qc_hard_at_first_touch), &
            new_unittest("a bounded soft qc violation warns and returns every row", &
                test_bounded_qc_soft_warns), &
            new_unittest("build_index on a string column completes and answers", &
                test_table_index_string_column_completes), &
            new_unittest("build_index on a boolean column aborts", &
                test_table_index_bool_column_aborts), &
            new_unittest("build_index on a vector column aborts", &
                test_table_index_vector_column_aborts), &
            new_unittest("build_index on a repeated key under unique=.true. aborts", &
                test_table_index_duplicate_unique_aborts), &
            new_unittest("build_index on a missing column aborts", &
                test_table_index_missing_column_aborts), &
            new_unittest("find on a stale index aborts", &
                test_table_index_stale_find_aborts), &
            new_unittest("find_all on a stale index aborts", &
                test_table_index_stale_find_all_aborts), &
            new_unittest("find_many on a stale index aborts", &
                test_table_index_stale_find_many_aborts), &
            new_unittest("count on a stale index aborts", &
                test_table_index_stale_count_aborts), &
            new_unittest("a query on a never-built index aborts", &
                test_table_index_never_built_aborts), &
            new_unittest("a real key on an integer index aborts", &
                test_table_index_kind_mismatch_aborts), &
            new_unittest("a timestamp key on a date index aborts", &
                test_table_index_kind_mismatch_temporal_aborts), &
            new_unittest("find_many with too few answer slots aborts", &
                test_table_index_find_many_length_aborts), &
            new_unittest("threads=0 on build_index aborts", &
                test_table_index_threads_zero_aborts), &
            new_unittest("every legal build_index and parquet_table_index path completes", &
                test_table_index_control_completes), &
            new_unittest("a date set against a timestamp column aborts", &
                test_filter_temporal_set_mismatch_aborts), &
            new_unittest("a time set against an int32 column is refused, naming the set's family", &
                test_filter_time_set_on_int_aborts), &
            new_unittest("a timestamp set against an int32 column is refused, naming the set's family", &
                test_filter_timestamp_set_on_int_aborts), &
            new_unittest("a literal list on a temporal column aborts", &
                test_filter_temporal_literal_list_aborts), &
            new_unittest("two concurrent duplicate-key builds abort once, cleanly", &
                test_index_concurrent_abort_aborts), &
            new_unittest("a duplicate deferred to the spill pass aborts naming its position", &
                test_index_spill_duplicate_aborts), &
            new_unittest("a duplicate in a partitioned build aborts naming its position", &
                test_index_partitioned_duplicate_aborts), &
            new_unittest("a duplicate tuple in a partitioned build aborts naming its position", &
                test_index_partitioned_duplicate_tuple_aborts), &
            new_unittest("a duplicate in a partitioned one-column build aborts", &
                test_index_partitioned_duplicate_one_column_aborts), &
            new_unittest("a duplicate string in a partitioned build aborts naming its position", &
                test_index_str_partitioned_duplicate_aborts), &
            new_unittest("threads=0 on get_or_add_many aborts", &
                test_index_get_or_add_many_threads_zero_aborts), &
            new_unittest("every partitioned pass completes on its legal inputs", &
                test_index_partition_control_completes), &
            new_unittest("group_by with no key aborts", &
                test_table_group_no_key_aborts), &
            new_unittest("group_by on a missing column aborts", &
                test_table_group_unknown_key_aborts), &
            new_unittest("group_by on a key carrying a direction aborts", &
                test_table_group_direction_token_aborts), &
            new_unittest("group_by on a vector column aborts", &
                test_table_group_vector_column_aborts), &
            new_unittest("group_by on a list column aborts", &
                test_table_group_container_column_aborts), &
            new_unittest("size on a stale grouping aborts", &
                test_table_group_stale_size_aborts), &
            new_unittest("rows on a stale grouping aborts", &
                test_table_group_stale_rows_aborts), &
            new_unittest("csr on a stale grouping aborts", &
                test_table_group_stale_csr_aborts), &
            new_unittest("first_rows on a stale grouping aborts", &
                test_table_group_stale_first_rows_aborts), &
            new_unittest("group_ids on a stale grouping aborts", &
                test_table_group_stale_group_ids_aborts), &
            new_unittest("key_table on a stale grouping aborts", &
                test_table_group_stale_key_table_aborts), &
            new_unittest("count on a stale grouping aborts", &
                test_table_group_stale_count_aborts), &
            new_unittest("a query on a never-built grouping aborts", &
                test_table_group_never_built_aborts), &
            new_unittest("rows with a group number out of range aborts", &
                test_table_group_out_of_range_aborts), &
            new_unittest("key_table with a size_name that names a key aborts", &
                test_table_group_size_name_clash_aborts), &
            new_unittest("apply on a stale grouping aborts", &
                test_table_group_stale_apply_aborts), &
            new_unittest("apply's object form on a stale grouping aborts", &
                test_table_group_stale_apply_object_aborts), &
            new_unittest("apply with nout below 1 aborts", &
                test_table_group_apply_nout_aborts), &
            new_unittest("apply with threads below 1 aborts", &
                test_table_group_apply_threads_zero_aborts), &
            new_unittest("agg on a stale grouping aborts", &
                test_table_group_stale_agg_aborts), &
            new_unittest("nunique on a stale grouping aborts", &
                test_table_group_stale_nunique_aborts), &
            new_unittest("agg with an unknown statistic aborts listing the vocabulary", &
                test_table_group_agg_unknown_token_aborts), &
            new_unittest("agg quantile without q= aborts", &
                test_table_group_agg_quantile_needs_q_aborts), &
            new_unittest("agg with an option its statistic does not take aborts", &
                test_table_group_agg_option_refused_aborts), &
            new_unittest("agg's exact family on a real column aborts", &
                test_table_group_agg_int_real_column_aborts), &
            new_unittest("agg's exact sum aborts on overflow", &
                test_table_group_agg_int_sum_overflow_aborts), &
            new_unittest("agg's exact min of an all-null group aborts", &
                test_table_group_agg_int_all_null_aborts), &
            new_unittest("agg on a string column aborts", &
                test_table_group_agg_string_column_aborts), &
            new_unittest("agg with weights of the wrong length aborts", &
                test_table_group_agg_weights_length_aborts), &
            new_unittest("agg with a negative weight aborts naming the row", &
                test_table_group_agg_negative_weight_aborts), &
            new_unittest("agg with weights= and weight_column= together aborts", &
                test_table_group_agg_both_weights_aborts), &
            new_unittest("agg with a string weight column aborts", &
                test_table_group_agg_weight_column_string_aborts), &
            new_unittest("agg with a vector weight column aborts", &
                test_table_group_agg_weight_column_vector_aborts), &
            new_unittest("agg with weights on a statistic they cannot affect aborts", &
                test_table_group_agg_weights_ignored_aborts), &
            new_unittest("nunique of a vector column aborts", &
                test_table_group_nunique_vector_column_aborts), &
            new_unittest("broadcast on a stale grouping aborts", test_table_group_stale_broadcast_aborts), &
            new_unittest("gather on a stale grouping aborts", test_table_group_stale_gather_aborts), &
            new_unittest("gather into a buffer shorter than the group aborts", &
                test_table_group_gather_short_buffer_aborts), &
            new_unittest("gather with an is_valid shorter than the group aborts", &
                test_table_group_gather_short_valid_aborts), &
            new_unittest("gather of a group number out of range aborts", &
                test_table_group_gather_out_of_range_aborts), &
            new_unittest("gather of a real64 column into an int32 buffer aborts", &
                test_table_group_gather_kind_aborts), &
            new_unittest("broadcast of a per_group of the wrong length aborts", &
                test_table_group_broadcast_length_aborts), &
            new_unittest("group_by with a blank key name aborts", test_table_group_blank_key_aborts), &
            new_unittest("group_by with a direction word aborts", test_table_group_direction_word_aborts), &
            new_unittest("a query naming a column that does not exist aborts", &
                test_table_group_query_unknown_column_aborts), &
            new_unittest("a query naming an unsupported column aborts", &
                test_table_group_query_unsupported_column_aborts), &
            new_unittest("agg's exact form with a real64-only token aborts", &
                test_table_group_agg_exact_unknown_token_aborts), &
            new_unittest("agg with method= on a statistic that takes none aborts", &
                test_table_group_agg_method_refused_aborts) &
            ]
        p3 = [ &
            new_unittest("agg with ddof= on a statistic that takes none aborts", &
                test_table_group_agg_ddof_refused_aborts), &
            new_unittest("agg with scale= outside mad aborts", test_table_group_agg_scale_refused_aborts), &
            new_unittest("agg with a NaN weight aborts", test_table_group_agg_nan_weight_aborts), &
            new_unittest("agg with an infinite weight aborts", test_table_group_agg_infinite_weight_aborts), &
            new_unittest("key_table with a negative reserve aborts", &
                test_table_group_key_table_reserve_negative_aborts), &
            new_unittest("add_agg onto the grouping's own table aborts", &
                test_table_group_add_agg_target_is_source_aborts), &
            new_unittest("add_agg onto a target whose row count is not the group count aborts", &
                test_table_group_add_agg_rows_mismatch_aborts), &
            new_unittest("add_agg with two names in as= aborts", &
                test_table_group_add_agg_as_two_names_aborts), &
            new_unittest("add_agg with nan_to_null= and exact= together aborts", &
                test_table_group_add_agg_nan_to_null_exact_aborts), &
            new_unittest("add_agg on a stale grouping aborts", test_table_group_stale_add_agg_aborts), &
            new_unittest("add_size on a stale grouping aborts", test_table_group_stale_add_size_aborts), &
            new_unittest("add_agg with an unknown statistic token aborts, naming add_agg", &
                test_table_group_add_agg_unknown_token_aborts), &
            new_unittest("add_size under a name the target already carries aborts", &
                test_table_group_add_size_name_taken_aborts), &
            new_unittest("add_apply onto the grouping's own table aborts", &
                test_table_group_add_apply_target_is_source_aborts), &
            new_unittest("add_apply with an as= that names nothing aborts", &
                test_table_group_add_apply_no_name_aborts), &
            new_unittest("add_apply naming one column twice in as= aborts", &
                test_table_group_add_apply_duplicate_name_aborts), &
            new_unittest("add_apply whose last name the target already carries aborts", &
                test_table_group_add_apply_name_taken_aborts), &
            new_unittest("add_apply on a stale grouping aborts", test_table_group_stale_add_apply_aborts), &
            new_unittest("add_apply with threads=0 aborts, naming add_apply", &
                test_table_group_add_apply_threads_zero_aborts), &
            new_unittest("an int32 group number past the last group aborts", &
                test_table_group_rows_g32_out_of_range_aborts), &
            new_unittest("a short gather buffer reached through an int32 g aborts", &
                test_table_group_gather_g32_short_buffer_aborts), &
            new_unittest("an int32 key list from a string map aborts", &
                test_index_keys_int32_on_string_map_aborts), &
            new_unittest("an int32 rank-1 key list from a composite map aborts", &
                test_index_keys_rank1_int32_on_composite_aborts), &
            new_unittest("an int32 rank-2 key list from a string map aborts", &
                test_index_keys_rank2_int32_on_string_map_aborts), &
            new_unittest("a threaded int32 get_or_add_many with an oversized code aborts", &
                test_index_get_or_add_many_threaded_int32_overflow_aborts), &
            new_unittest("a reserve of a negative count on a pool aborts", &
                test_index_pool_reserve_negative_aborts), &
            new_unittest("a rank-2 key list from a string multimap aborts", &
                test_multimap_keys_rank2_on_string_map_aborts), &
            new_unittest("an int32 key list from a string multimap aborts", &
                test_multimap_keys_int32_on_string_map_aborts), &
            new_unittest("an int32 rank-1 key list from a composite multimap aborts", &
                test_multimap_keys_rank1_int32_on_composite_aborts), &
            new_unittest("an int32 rank-2 key list from a string multimap aborts", &
                test_multimap_keys_rank2_int32_on_string_map_aborts), &
            new_unittest("a string key column from an integer multimap aborts", &
                test_multimap_string_keys_column_on_integer_aborts), &
            new_unittest("a string bulk lookup of the wrong length aborts", &
                test_multimap_string_bulk_length_aborts), &
            new_unittest("a string bulk lookup on an integer multimap aborts", &
                test_multimap_string_bulk_on_integer_aborts), &
            new_unittest("a negative rtol is refused", &
                test_integrate_negative_rtol_aborts), &
            new_unittest("a NaN rtol is refused", &
                test_integrate_nan_rtol_aborts), &
            new_unittest("a negative atol is refused", &
                test_integrate_negative_atol_aborts), &
            new_unittest("two zero tolerances are refused", &
                test_integrate_zero_tolerances_aborts), &
            new_unittest("an rtol below 50*epsilon with no atol is refused", &
                test_integrate_rtol_below_floor_aborts), &
            new_unittest("a zero max_neval is refused", &
                test_integrate_bad_max_neval_aborts), &
            new_unittest("a max_neval above huge(1)/42 is refused", &
                test_integrate_huge_max_neval_aborts), &
            new_unittest("a NaN integration bound is refused", &
                test_integrate_nan_bound_aborts), &
            new_unittest("reversed integration bounds are refused", &
                test_integrate_reversed_bounds_aborts), &
            new_unittest("a max_panels of zero is refused", &
                test_integrate_bad_max_panels_aborts), &
            new_unittest("max_panels on a finite range is refused", &
                test_integrate_max_panels_finite_aborts), &
            new_unittest("both bounds the same infinity is refused", &
                test_integrate_same_infinity_aborts), &
            new_unittest("log_base on an infinite range is refused", &
                test_integrate_log_base_infinite_aborts), &
            new_unittest("log_base from a non-positive lower bound is refused", &
                test_integrate_log_base_nonpositive_aborts), &
            new_unittest("a NaN breakpoint is refused", &
                test_integrate_breakpoints_nan_aborts), &
            new_unittest("a breakpoint outside the range is refused", &
                test_integrate_breakpoints_outside_aborts), &
            new_unittest("two equal breakpoints are refused", &
                test_integrate_breakpoints_duplicate_aborts), &
            new_unittest("a context reaches the abort message", &
                test_integrate_context_reported_aborts), &
            new_unittest("a long context is capped in the abort message", &
                test_integrate_context_capped_aborts), &
            new_unittest("a zero-length start point is refused", &
                test_optimize_size_zero_aborts), &
            new_unittest("a zero max_neval is refused by the simplex", &
                test_optimize_budget_zero_aborts), &
            new_unittest("a max_neval above huge(1)/2 is refused", &
                test_optimize_budget_ceiling_aborts), &
            new_unittest("a NaN tolerance is refused", &
                test_optimize_tolerance_nonfinite_aborts), &
            new_unittest("a reversed bracket is refused", &
                test_optimize_scalar_bad_bracket_aborts), &
            new_unittest("a bracket whose width overflows is refused", &
                test_optimize_scalar_bracket_width_aborts), &
            new_unittest("a non-finite objective value is refused by Brent", &
                test_optimize_scalar_nonfinite_value_aborts), &
            new_unittest("Brent refuses a constrained objective", &
                test_optimize_scalar_constraints_not_honoured_aborts), &
            new_unittest("a zero step element is refused", &
                test_optimize_simplex_step_zero_aborts), &
            new_unittest("a step of the wrong size is refused", &
                test_optimize_simplex_step_size_aborts), &
            new_unittest("two zero tolerances are refused by the simplex", &
                test_optimize_simplex_no_tolerance_aborts), &
            new_unittest("a NaN start point is refused", &
                test_optimize_simplex_nan_start_aborts), &
            new_unittest("a non-finite objective value mid-run is refused by the simplex", &
                test_optimize_simplex_nonfinite_value_aborts), &
            new_unittest("the simplex refuses a constrained objective", &
                test_optimize_simplex_constraints_not_honoured_aborts), &
            new_unittest("a zero threads= is refused", &
                test_optimize_threads_zero_aborts), &
            new_unittest("a box of the wrong size is refused", &
                test_optimize_de_bounds_size_aborts), &
            new_unittest("a lower bound not below its upper bound is refused", &
                test_optimize_de_bounds_order_aborts), &
            new_unittest("an infinite bound is refused", &
                test_optimize_de_bounds_nonfinite_aborts), &
            new_unittest("a box whose width overflows is refused", &
                test_optimize_de_box_width_aborts), &
            new_unittest("a population below four is refused", &
                test_optimize_de_np_small_aborts), &
            new_unittest("a differential weight outside its range is refused", &
                test_optimize_de_f_weight_range_aborts), &
            new_unittest("a crossover probability outside its range is refused", &
                test_optimize_de_cr_range_aborts), &
            new_unittest("a zero generation budget is refused", &
                test_optimize_de_max_gen_zero_aborts), &
            new_unittest("DE refuses a constrained objective", &
                test_optimize_de_constraints_not_honoured_aborts), &
            new_unittest("rtol and atol both zero are refused as a pair", &
                test_optimize_de_no_tolerance_aborts), &
            new_unittest("a parquet_optimize refusal carries the caller's context", &
                test_optimize_context_is_reported_aborts), &
            new_unittest("a parquet_optimize context longer than the cap is truncated", &
                test_optimize_context_is_capped_aborts), &
            new_unittest("zero starts are refused", &
                test_optimize_multistart_nstart_zero_aborts), &
            new_unittest("a negative merge_tol is refused", &
                test_optimize_multistart_merge_tol_negative_aborts), &
            new_unittest("the multistart driver refuses a constrained objective by name", &
                test_optimize_multistart_constraints_not_honoured_aborts), &
            new_unittest("a negative pf_simplex_solver%max_neval is refused", &
                test_optimize_simplex_solver_negative_budget_aborts), &
            new_unittest("a negative pf_bobyqa_solver%max_neval is refused", &
                test_prima_bobyqa_solver_negative_budget_aborts), &
            new_unittest("BOBYQA refuses a zero-length start", &
                test_prima_size_zero_aborts), &
            new_unittest("BOBYQA refuses a NaN in the start point", &
                test_prima_start_nan_aborts), &
            new_unittest("BOBYQA refuses an infinity in the start point", &
                test_prima_start_infinite_aborts), &
            new_unittest("BOBYQA refuses bounds of the wrong length", &
                test_prima_bounds_size_aborts), &
            new_unittest("a NaN bound is refused rather than dropped", &
                test_prima_bound_nan_aborts), &
            new_unittest("a rhobeg wider than half the box is refused rather than reduced", &
                test_prima_rhobeg_too_wide_aborts), &
            new_unittest("a bound pair with no room between them is refused", &
                test_prima_no_space_between_bounds_aborts), &
            new_unittest("a start outside the bounds is refused rather than moved", &
                test_prima_start_outside_bounds_aborts), &
            new_unittest("rhoend above rhobeg is refused rather than swapped", &
                test_prima_rho_order_aborts), &
            new_unittest("an npt outside its range is refused rather than clamped", &
                test_prima_npt_range_aborts), &
            new_unittest("a scale of the wrong length is refused", &
                test_prima_scale_size_aborts), &
            new_unittest("a zero or negative scale is refused", &
                test_prima_scale_nonpositive_aborts), &
            new_unittest("BOBYQA refuses a zero budget", &
                test_prima_budget_zero_aborts), &
            new_unittest("BOBYQA refuses a budget above the ceiling", &
                test_prima_budget_ceiling_aborts), &
            new_unittest("a non-finite objective value is refused rather than moderated", &
                test_prima_nonfinite_value_aborts), &
            new_unittest("BOBYQA refuses a constrained objective by name", &
                test_prima_bobyqa_constraints_not_honoured_aborts), &
            new_unittest("LINCOA refuses a constrained objective by name", &
                test_prima_lincoa_constraints_not_honoured_aborts), &
            new_unittest("a constraint matrix of the wrong shape is refused", &
                test_prima_lincoa_shape_aborts), &
            new_unittest("an all-zero constraint row is refused rather than dropped", &
                test_prima_zero_constraint_row_aborts), &
            new_unittest("an infeasible start is refused rather than admitted by relaxation", &
                test_prima_lincoa_infeasible_start_aborts), &
            new_unittest("a negative ctol is refused", &
                test_prima_ctol_negative_aborts) &
            ]
        p4 = [ &
            new_unittest("a negative n_constraints is refused", &
                test_prima_cobyla_negative_count_aborts), &
            new_unittest("a non-finite constraint value is refused rather than moderated", &
                test_prima_constraint_nonfinite_aborts), &
            new_unittest("a non-finite objective value from a constrained objective is refused", &
                test_prima_cobyla_nonfinite_value_aborts), &
            new_unittest("a non-finite ctol is refused", &
                test_prima_ctol_nonfinite_aborts), &
            new_unittest("a refusal carries the caller's context", &
                test_prima_context_is_reported_aborts), &
            new_unittest("a context longer than the cap is truncated", &
                test_prima_context_is_capped_aborts), &
            new_unittest("LINCOA's feasibility refusal carries the caller's context", &
                test_prima_lincoa_infeasible_start_context_aborts), &
            new_unittest("x and y of different sizes are refused", &
                test_interpolate_size_mismatch_aborts), &
            new_unittest("a mask of the wrong size is refused", &
                test_interpolate_is_valid_size_aborts), &
            new_unittest("an unknown interpolation method is refused", &
                test_interpolate_unknown_method_aborts), &
            new_unittest("an unknown out-of-range policy is refused", &
                test_interpolate_unknown_outside_aborts), &
            new_unittest("a single-point table is refused", &
                test_interpolate_too_few_points_aborts), &
            new_unittest("a table the mask leaves one point of is refused", &
                test_interpolate_too_few_after_is_valid_aborts), &
            new_unittest("a repeated abscissa is refused", &
                test_interpolate_repeated_x_aborts), &
            new_unittest("unsorted abscissae are refused", &
                test_interpolate_unsorted_x_aborts), &
            new_unittest("a NaN abscissa is refused by the ordering check", &
                test_interpolate_nan_in_x_aborts), &
            new_unittest("an infinite abscissa is refused", &
                test_interpolate_inf_in_x_aborts), &
            new_unittest("a NaN ordinate is refused", &
                test_interpolate_nan_in_y_aborts), &
            new_unittest("an infinite ordinate is refused", &
                test_interpolate_inf_in_y_aborts), &
            new_unittest("evaluating an interpolant that was never built aborts", &
                test_interpolate_eval_before_init_aborts), &
                new_unittest("cosmology_eval_before_init aborts", &
                test_cosmology_eval_before_init_aborts), &
                new_unittest("cosmology_eval_after_clear aborts", &
                test_cosmology_eval_after_clear_aborts), &
                new_unittest("cosmology_init_unknown_name aborts", &
                test_cosmology_init_unknown_name_aborts), &
                new_unittest("cosmology_init_h0_out_of_range aborts", &
                test_cosmology_init_h0_out_of_range_aborts), &
                new_unittest("cosmology_init_om0_negative aborts", &
                test_cosmology_init_om0_negative_aborts), &
                new_unittest("cosmology_init_tcmb0_negative aborts", &
                test_cosmology_init_tcmb0_negative_aborts), &
                new_unittest("cosmology_init_m_nu_size aborts", &
                test_cosmology_init_m_nu_size_aborts), &
                new_unittest("cosmology_init_w0_out_of_range aborts", &
                test_cosmology_init_w0_out_of_range_aborts), &
                new_unittest("cosmology_init_wa_out_of_range aborts", &
                test_cosmology_init_wa_out_of_range_aborts), &
                new_unittest("cosmology_init_ode0_not_finite aborts", &
                test_cosmology_init_ode0_not_finite_aborts), &
                new_unittest("cosmology_init_neff_negative aborts", &
                test_cosmology_init_neff_negative_aborts), &
                new_unittest("cosmology_init_m_nu_negative aborts", &
                test_cosmology_init_m_nu_negative_aborts), &
                new_unittest("cosmology_init_context_capped aborts", &
                test_cosmology_init_context_capped_aborts), &
                new_unittest("cosmology_config_label_without_parameters aborts", &
                test_cosmology_config_label_without_parameters_aborts), &
                new_unittest("cosmology_init_zmax_out_of_range aborts", &
                test_cosmology_init_zmax_out_of_range_aborts), &
                new_unittest("cosmology_init_zmin_out_of_range aborts", &
                test_cosmology_init_zmin_out_of_range_aborts), &
                new_unittest("cosmology_init_ob0_above_om0 aborts", &
                test_cosmology_init_ob0_above_om0_aborts), &
                new_unittest("cosmology_init_density_too_large aborts", &
                test_cosmology_init_density_too_large_aborts), &
                new_unittest("cosmology_init_tcmb0_too_hot aborts", &
                test_cosmology_init_tcmb0_too_hot_aborts), &
                new_unittest("cosmology_init_no_big_bang aborts", &
                test_cosmology_init_no_big_bang_aborts), &
                new_unittest("cosmology_init_table_not_converged aborts", &
                test_cosmology_init_table_not_converged_aborts), &
                new_unittest("a configuration with no [cosmology] section and no found= aborts", &
                test_cosmology_config_missing_section_aborts), &
                new_unittest("a named cosmology given parameters as well aborts", &
                test_cosmology_config_named_with_parameters_aborts), &
                new_unittest("a [[cosmology]] array of tables aborts even with found=", &
                test_cosmology_config_array_of_tables_aborts), &
                new_unittest("a [cosmology] key of the wrong type aborts", &
                test_cosmology_config_bad_type_aborts), &
                new_unittest("an m_nu of the wrong length in a file aborts", &
                test_cosmology_config_mnu_length_aborts), &
                new_unittest("a [cosmology] parameter outside its range aborts with the file named", &
                test_cosmology_config_out_of_range_aborts), &
            new_unittest("a context reaches the interpolation abort message", &
                test_interpolate_context_reported_aborts), &
            new_unittest("a long context is capped in the interpolation abort message", &
                test_interpolate_context_capped_aborts), &
            new_unittest("pf_interp refuses x and y of different sizes under its own name", &
                test_interpolate_one_shot_size_mismatch_aborts), &
            new_unittest("an end condition for linear interpolation is refused", &
                test_interpolate_bc_with_linear_aborts), &
            new_unittest("an unknown interpolation end condition is refused", &
                test_interpolate_unknown_bc_aborts), &
            new_unittest("a clamped spline without slopes is refused", &
                test_interpolate_clamped_without_slopes_aborts), &
            new_unittest("slopes without a clamped spline are refused", &
                test_interpolate_slopes_without_clamped_aborts), &
            new_unittest("a clamped spline given one slope is refused", &
                test_interpolate_slopes_size_aborts), &
            new_unittest("a NaN end slope is refused", &
                test_interpolate_slopes_nan_aborts), &
            new_unittest("an infinite end slope is refused", &
                test_interpolate_slopes_inf_aborts), &
            new_unittest("a not-a-knot spline over three points is refused", &
                test_interpolate_not_a_knot_too_few_aborts), &
            new_unittest("a linear interpolant over one point is refused", &
                test_interpolate_linear_too_few_aborts), &
            new_unittest("a pchip interpolant over one point is refused", &
                test_interpolate_pchip_too_few_aborts), &
            new_unittest("differentiating an interpolant that was never built aborts", &
                test_interpolate_derivative_before_init_aborts), &
            new_unittest("integrating an interpolant that was never built aborts", &
                test_interpolate_integral_before_init_aborts), &
            new_unittest("a third derivative of an interpolant aborts", &
                test_interpolate_bad_order_aborts), &
            new_unittest("values shaped for the transposed grid are refused", &
                test_interpolate_2d_shape_aborts), &
            new_unittest("an unknown grid interpolation method is refused", &
                test_interpolate_2d_unknown_method_aborts), &
            new_unittest("a shape-preserving grid is refused", &
                test_interpolate_2d_pchip_aborts), &
            new_unittest("an end condition for bilinear interpolation is refused", &
                test_interpolate_2d_bc_with_linear_aborts), &
            new_unittest("an unknown grid end condition is refused", &
                test_interpolate_2d_unknown_bc_aborts), &
            new_unittest("a clamped grid spline is refused", &
                test_interpolate_2d_clamped_aborts), &
            new_unittest("an unknown out-of-range policy for a grid is refused", &
                test_interpolate_2d_unknown_outside_aborts), &
            new_unittest("a grid with one line along x is refused", &
                test_interpolate_2d_too_few_x_aborts), &
            new_unittest("a not-a-knot grid spline over three lines along y is refused", &
                test_interpolate_2d_not_a_knot_too_few_aborts), &
            new_unittest("unsorted lines along x are refused", &
                test_interpolate_2d_x_not_monotonic_aborts), &
            new_unittest("a repeated line along y is refused", &
                test_interpolate_2d_y_not_monotonic_aborts), &
            new_unittest("an infinite line along x is refused", &
                test_interpolate_2d_inf_in_x_aborts), &
            new_unittest("an infinite line along y is refused", &
                test_interpolate_2d_inf_in_y_aborts), &
            new_unittest("a NaN grid value is refused", &
                test_interpolate_2d_nan_in_z_aborts), &
            new_unittest("evaluating a grid interpolant that was never built aborts", &
                test_interpolate_2d_eval_before_init_aborts), &
            new_unittest("pf_interp refuses a misshaped grid under its own name, with the context", &
                test_interpolate_2d_one_shot_shape_aborts), &
            new_unittest("pf_interp refuses grid coordinates of different sizes", &
                test_interpolate_2d_query_sizes_aborts), &
            new_unittest("a cubic spline over knots too far apart is refused", &
                test_interpolate_spline_too_wide_aborts), &
            new_unittest("a cubic spline whose second derivatives overflow is refused", &
                test_interpolate_spline_overflows_aborts), &
            new_unittest("a bicubic spline over grid lines along x too far apart is refused", &
                test_interpolate_2d_spline_too_wide_x_aborts), &
            new_unittest("a bicubic spline over grid lines along y too far apart is refused", &
                test_interpolate_2d_spline_too_wide_y_aborts), &
            new_unittest("a bicubic spline whose second derivatives overflow is refused", &
                test_interpolate_2d_spline_overflows_aborts), &
            new_unittest("an over-long method token is refused, whatever it trims to", &
                test_interpolate_long_token_aborts), &
            new_unittest("evaluating an interpolant that was never built at no queries aborts", &
                test_interpolate_eval_array_before_init_aborts), &
            new_unittest("pf_find_root refuses a reversed bracket", &
                test_root_reversed_bracket_aborts), &
            new_unittest("pf_find_root refuses a NaN bracket end", &
                test_root_nan_bracket_end_aborts), &
            new_unittest("pf_find_root refuses a bracket whose width overflows", &
                test_root_bracket_width_aborts), &
            new_unittest("pf_find_root refuses a negative tol", &
                test_root_negative_atol_aborts), &
            new_unittest("pf_find_root refuses a negative rtol", &
                test_root_negative_rtol_aborts), &
            new_unittest("pf_find_root refuses a zero max_neval", &
                test_root_bad_max_neval_aborts), &
            new_unittest("pf_find_root refuses an unknown expansion mode", &
                test_root_bad_expansion_mode_aborts), &
            new_unittest("pf_find_root refuses an expansion factor of one", &
                test_root_bad_expansion_factor_aborts), &
            new_unittest("pf_find_root refuses a negative max_tries", &
                test_root_bad_expansion_tries_aborts), &
            new_unittest("pf_find_root refuses an expansion limit inside the bracket", &
                test_root_limits_inside_the_bracket_aborts), &
            new_unittest("pf_find_root refuses a NaN expansion limit", &
                test_root_nonfinite_expansion_limit_aborts), &
            new_unittest("pf_find_root refuses a function that returns a NaN", &
                test_root_function_returns_nan_aborts), &
            new_unittest("pf_find_root carries the caller's context into its message", &
                test_root_context_reported_aborts), &
            new_unittest("pf_find_root caps the caller's context at 100 characters", &
                test_root_context_capped_aborts) &
            ]
        p5 = [ &
            new_unittest("pf_dct refuses an empty sequence", &
                test_transform_empty_sequence_aborts), &
            new_unittest("pf_dct refuses a length that is not a power of two", &
                test_transform_length_not_pow2_aborts), &
            new_unittest("pf_dct refuses x and y of different sizes", &
                test_transform_size_mismatch_aborts), &
            new_unittest("pf_idct refuses y and x of different sizes", &
                test_transform_idct_size_mismatch_aborts), &
            new_unittest("pf_dct refuses a norm token it does not offer", &
                test_transform_bad_norm_token_aborts), &
            new_unittest("pf_next_pow2 refuses a count below one", &
                test_transform_next_pow2_nonpositive_aborts), &
            new_unittest("pf_next_pow2 refuses a count above the largest power of two", &
                test_transform_next_pow2_too_large_aborts), &
            new_unittest("pf_dct carries the caller's context into its message", &
                test_transform_context_reported_aborts), &
            new_unittest("pf_dct caps the caller's context at 100 characters", &
                test_transform_context_capped_aborts), &
            new_unittest("pf_dst refuses a length that is not a power of two, in its own name", &
                test_transform_dst_length_not_pow2_aborts), &
            new_unittest("pf_dst refuses x and y of different sizes, in its own name", &
                test_transform_dst_size_mismatch_aborts), &
            new_unittest("pf_idst refuses a length that is not a power of two, in its own name", &
                test_transform_idst_length_not_pow2_aborts), &
            new_unittest("pf_idst refuses a norm token it does not offer, in its own name", &
                test_transform_idst_bad_norm_token_aborts), &
            new_unittest("pf_kde%fit refuses a zero bandwidth", &
                test_kde_bandwidth_zero_aborts), &
            new_unittest("pf_kde%fit refuses a NaN bandwidth", &
                test_kde_bandwidth_nan_aborts), &
            new_unittest("pf_kde%fit refuses bandwidth= and rule= together", &
                test_kde_bandwidth_and_rule_aborts), &
            new_unittest("pf_kde%fit refuses an unknown rule", &
                test_kde_unknown_rule_aborts), &
            new_unittest("pf_kde%fit refuses a negative adjust", &
                test_kde_adjust_negative_aborts), &
            new_unittest("pf_kde%fit refuses an unknown kernel", &
                test_kde_unknown_kernel_aborts), &
            new_unittest("pf_kde%fit refuses an infinite bound", &
                test_kde_bound_infinite_aborts), &
            new_unittest("pf_kde%fit refuses lower >= upper", &
                test_kde_lower_not_below_upper_aborts), &
            new_unittest("pf_kde%fit refuses boundary= without a bound", &
                test_kde_boundary_without_bound_aborts), &
            new_unittest("pf_kde%fit refuses an unknown boundary", &
                test_kde_unknown_boundary_aborts), &
            new_unittest("pf_kde%fit refuses weights of the wrong length, in the family's words", &
                test_kde_weights_size_aborts), &
            new_unittest("pf_kde%fit refuses is_valid of the wrong length, in the family's words", &
                test_kde_is_valid_size_aborts), &
            new_unittest("pf_kde%fit refuses a negative weight, in the family's words", &
                test_kde_negative_weight_aborts), &
            new_unittest("pf_kde%fit refuses an unknown weight_type, in the family's words", &
                test_kde_weight_type_aborts), &
            new_unittest("pf_kde%fit's real32 form refuses weights of the wrong length", &
                test_kde_real32_weights_size_aborts), &
            new_unittest("pf_kde%fit refuses threads=0", &
                test_kde_threads_zero_aborts), &
            new_unittest("pf_kde%pdf refuses an object that was never fitted", &
                test_kde_query_unfitted_aborts), &
            new_unittest("pf_kde%cdf refuses an object that %clear unfitted", &
                test_kde_query_after_clear_aborts), &
            new_unittest("pf_kde%bandwidth refuses an object that was never fitted", &
                test_kde_accessor_unfitted_aborts), &
            new_unittest("pf_kde%pdf refuses an output of the wrong size", &
                test_kde_pdf_size_aborts), &
            new_unittest("pf_kde%cdf refuses an output of the wrong size", &
                test_kde_cdf_size_aborts), &
            new_unittest("pf_kde%quantile refuses an output of the wrong size", &
                test_kde_quantile_size_aborts), &
            new_unittest("pf_kde%quantile refuses p above one", &
                test_kde_quantile_p_above_one_aborts), &
            new_unittest("pf_kde%quantile refuses a NaN p", &
                test_kde_quantile_p_nan_aborts), &
            new_unittest("pf_kde%curve refuses x and f of different sizes", &
                test_kde_curve_size_aborts), &
            new_unittest("pf_kde%curve refuses xmin >= xmax", &
                test_kde_curve_reversed_range_aborts), &
            new_unittest("pf_kde%curve refuses a negative cut", &
                test_kde_curve_negative_cut_aborts), &
            new_unittest("pf_kde%curve refuses an infinite end", &
                test_kde_curve_nonfinite_end_aborts), &
            new_unittest("pf_kde_grid%init refuses ncells=0", &
                test_kde_grid_ncells_aborts), &
            new_unittest("pf_kde_grid%init refuses a NaN xmin", &
                test_kde_grid_range_nan_aborts), &
            new_unittest("pf_kde_grid%init refuses xmin == xmax", &
                test_kde_grid_range_reversed_aborts), &
            new_unittest("pf_kde_grid%init refuses a range wider than the largest number", &
                test_kde_grid_cell_width_aborts), &
            new_unittest("pf_kde_grid%init refuses a zero bandwidth", &
                test_kde_grid_bandwidth_aborts), &
            new_unittest("pf_kde_grid%init refuses a subnormal bandwidth", &
                test_kde_grid_bandwidth_subnormal_aborts), &
            new_unittest("pf_kde_grid%init refuses a bandwidth whose reach overflows", &
                test_kde_grid_bandwidth_unusable_aborts), &
            new_unittest("a query on an unfinished grid aborts", &
                test_kde_grid_query_unfinished_aborts), &
            new_unittest('pf_kde_grid%init refuses an unknown method token', &
                test_kde_grid_method_token_aborts), &
            new_unittest('method="binned" refuses a transform past the ceiling', &
                test_kde_grid_binned_too_long_aborts), &
            new_unittest('%curve(method="binned") refuses a single point', &
                test_kde_curve_binned_one_point_aborts), &
            new_unittest('pf_kde%fit refuses an unknown method token', &
                test_kde_fit_method_token_aborts), &
            new_unittest('%curve refuses a method= on a binned fit', &
                test_kde_curve_method_on_binned_aborts), &
            new_unittest("pf_kde_grid%add after %finish aborts", &
                test_kde_grid_add_after_finish_aborts), &
            new_unittest("pf_kde_grid%merge after %finish aborts", &
                test_kde_grid_merge_after_finish_aborts), &
            new_unittest("pf_kde_grid%init refuses an unfinished pilot", &
                test_kde_grid_pilot_unfinished_aborts), &
            new_unittest("pf_kde_grid%init refuses an unknown kernel", &
                test_kde_grid_unknown_kernel_aborts), &
            new_unittest("pf_kde_grid%init refuses boundary= without a bound", &
                test_kde_grid_boundary_without_bound_aborts), &
            new_unittest("pf_kde_grid%init refuses a range reaching outside the support", &
                test_kde_grid_outside_support_aborts), &
            new_unittest("pf_kde_grid%init refuses a linear grid whose range leaves its bound", &
                test_kde_grid_linear_range_not_at_bound_aborts), &
            new_unittest("pf_kde_grid%init refuses a linear grid narrower than a reach with a free edge", &
                test_kde_grid_linear_narrow_aborts), &
            new_unittest("pf_kde_grid%add refuses a grid that was never initialised", &
                test_kde_grid_add_uninitialised_aborts), &
            new_unittest("pf_kde_grid%add refuses threads=0", &
                test_kde_grid_threads_zero_aborts), &
            new_unittest("pf_kde_grid%add refuses weights of the wrong length, in the family's words", &
                test_kde_grid_weights_size_aborts), &
            new_unittest("pf_kde_grid%add's real32 form refuses weights of the wrong length", &
                test_kde_grid_real32_weights_size_aborts), &
            new_unittest("pf_kde_grid%add refuses a negative weight, in the family's words", &
                test_kde_grid_negative_weight_aborts), &
            new_unittest("pf_kde_grid%density refuses an output of the wrong size", &
                test_kde_grid_density_size_aborts), &
            new_unittest("pf_kde_grid%density refuses centres of the wrong size", &
                test_kde_grid_density_x_size_aborts), &
            new_unittest("pf_kde_grid%grid refuses an output of the wrong size", &
                test_kde_grid_centres_size_aborts), &
            new_unittest("pf_kde_grid%pdf refuses an output of the wrong size", &
                test_kde_grid_pdf_size_aborts), &
            new_unittest("pf_kde_grid%cdf refuses an output of the wrong size", &
                test_kde_grid_cdf_size_aborts), &
            new_unittest("pf_kde_grid%quantile refuses an output of the wrong size", &
                test_kde_grid_quantile_size_aborts), &
            new_unittest("pf_kde_grid%quantile refuses p above one", &
                test_kde_grid_quantile_p_aborts), &
            new_unittest("pf_kde_grid%pdf refuses a grid that was never initialised", &
                test_kde_grid_query_uninitialised_aborts), &
            new_unittest("pf_kde_grid%ncells refuses a grid that was never initialised", &
                test_kde_grid_accessor_uninitialised_aborts), &
            new_unittest("pf_kde_grid%merge refuses another grid that was never initialised", &
                test_kde_grid_merge_uninitialised_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different number of cells", &
                test_kde_grid_merge_cells_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different range", &
                test_kde_grid_merge_range_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different bandwidth", &
                test_kde_grid_merge_bandwidth_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different kernel", &
                test_kde_grid_merge_kernel_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different support", &
                test_kde_grid_merge_support_aborts), &
            new_unittest("pf_kde_grid%merge refuses a different boundary correction", &
                test_kde_grid_merge_boundary_aborts), &
            new_unittest("pf_kde_grid%merge refuses a grid filled by another method", &
                test_kde_grid_merge_method_aborts), &
            new_unittest("pf_kde%fit refuses alpha= without adaptive=.true.", &
                test_kde_alpha_without_adaptive_aborts), &
            new_unittest("pf_kde%fit refuses bandwidth_max= with adaptive=.false.", &
                test_kde_bandwidth_max_without_adaptive_aborts), &
            new_unittest("pf_kde%fit refuses spread_max= with adaptive=.false.", &
                test_kde_spread_max_without_adaptive_aborts), &
            new_unittest("pf_kde%fit refuses a spread_max below one", &
                test_kde_spread_max_below_one_aborts), &
            new_unittest("the default spread cap says when it binds, and the caller's own does not", &
                test_kde_spread_cap_advice), &
            new_unittest("the zone advice names a bandwidth_max and a spread_max that silence it", &
                test_kde_zone_advice_values), &
            new_unittest("the zone advice names bandwidth= where no cap can be passed", &
                test_kde_zone_advice_fixed), &
            new_unittest("pf_kde%fit refuses alpha above one", &
                test_kde_alpha_above_one_aborts), &
            new_unittest("pf_kde%fit refuses a NaN alpha", &
                test_kde_alpha_nan_aborts), &
            new_unittest("pf_kde%fit refuses a zero bandwidth_max", &
                test_kde_bandwidth_max_zero_aborts), &
            new_unittest("pf_kde%bandwidths refuses an output of the wrong size", &
                test_kde_bandwidths_size_aborts), &
            new_unittest("pf_kde%bandwidths refuses points of the wrong size", &
                test_kde_bandwidths_x_size_aborts), &
            new_unittest("pf_kde%bandwidth_at refuses an output of the wrong size", &
                test_kde_bandwidth_at_size_aborts), &
            new_unittest("pf_kde%pilot refuses a fit that is not adaptive", &
                test_kde_pilot_not_adaptive_aborts), &
            new_unittest("pf_kde_grid%init refuses alpha= without pilot=", &
                test_kde_grid_alpha_without_pilot_aborts), &
            new_unittest("pf_kde_grid%init refuses bandwidth_max= without pilot=", &
                test_kde_grid_bandwidth_max_without_pilot_aborts), &
            new_unittest("pf_kde_grid%init refuses a negative alpha", &
                test_kde_grid_alpha_negative_aborts) &
            ]
        p6 = [ &
            new_unittest("pf_kde_grid%init refuses a NaN bandwidth_max", &
                test_kde_grid_bandwidth_max_nan_aborts), &
            new_unittest("pf_kde_grid%init refuses a pilot that was never initialised", &
                test_kde_grid_pilot_uninitialised_aborts), &
            new_unittest("pf_kde_grid%merge refuses a grid read from another pilot", &
                test_kde_grid_merge_pilot_aborts), &
            new_unittest("pf_kde_grid%merge refuses a fixed grid into an adaptive one", &
                test_kde_grid_merge_fixed_aborts), &
            new_unittest("pf_kde_grid%merge refuses a grid with another alpha", &
                test_kde_grid_merge_alpha_aborts), &
            new_unittest("pf_kde%pdf refuses threads=0", &
                test_kde_pdf_threads_zero_aborts), &
            new_unittest("pf_kde%cdf refuses threads=0", &
                test_kde_cdf_threads_zero_aborts), &
            new_unittest("pf_kde%quantile refuses threads=0", &
                test_kde_quantile_threads_zero_aborts), &
            new_unittest("pf_kde%curve refuses threads=0", &
                test_kde_curve_threads_zero_aborts), &
            new_unittest("pf_kde%sample refuses threads=0", &
                test_kde_sample_threads_zero_aborts), &
            new_unittest("pf_kde%sample refuses an unfitted estimate", &
                test_kde_sample_unfitted_aborts), &
            new_unittest("pf_kde_grid%sample refuses threads=0", &
                test_kde_grid_sample_threads_zero_aborts), &
            new_unittest("pf_kde_grid%sample refuses an uninitialised grid", &
                test_kde_grid_sample_uninitialised_aborts), &
            new_unittest("pf_kde%fit refuses a logical column, naming its kind", &
                test_kde_fit_logical_column_aborts), &
            new_unittest("pf_kde%fit refuses is_valid= beside a column", &
                test_kde_fit_column_is_valid_aborts), &
            new_unittest("pf_kde_grid%add refuses a vector column, naming its kind", &
                test_kde_grid_add_vector_column_aborts), &
            new_unittest("pf_kde_grid%add refuses is_valid= beside a column", &
                test_kde_grid_add_column_is_valid_aborts), &
            new_unittest("parquet_debug_set_kde_isj_cells refuses a count that is not a power of two", &
                test_kde_isj_cells_not_pow2_aborts), &
            new_unittest("parquet_debug_set_kde_isj_cells refuses a power of two beyond 2**20", &
                test_kde_isj_cells_out_of_range_aborts), &
            new_unittest("R3's warning is a data finding: it survives silent and goes at errors_only", &
                test_kde_overreach_warning_class), &
            new_unittest("pf_kde%fit refuses pilot= without the adaptive kernel", &
                test_kde_fit_pilot_without_adaptive_aborts), &
            new_unittest("pf_kde%fit refuses a pilot that was never finished", &
                test_kde_fit_pilot_unfinished_aborts), &
            new_unittest("pf_kde%fit refuses a pilot built with another kernel", &
                test_kde_fit_pilot_kernel_aborts), &
            new_unittest("pf_kde%fit refuses a pilot built under another boundary correction", &
                test_kde_fit_pilot_boundary_aborts), &
            new_unittest("pf_kde%fit refuses a pilot built over another support", &
                test_kde_fit_pilot_support_aborts), &
            new_unittest("pf_kde%fit refuses a pilot bounded on another side", &
                test_kde_fit_pilot_support_side_aborts), &
            new_unittest("pf_kde%fit refuses a pilot built to another upper bound", &
                test_kde_fit_pilot_upper_aborts), &
            new_unittest("pf_kde%fit refuses a spread_max that is not finite", &
                test_kde_spread_max_not_finite_aborts), &
            new_unittest("pf_kde%curve refuses one end that collapses the range", &
                test_kde_curve_one_end_collapses_aborts), &
            new_unittest("pf_kde%curve names what collapsed when the caller passed no ends", &
                test_kde_curve_default_range_collapses_aborts), &
            new_unittest("pf_kde_grid%init refuses a cell width that underflowed to zero", &
                test_kde_grid_cell_width_underflow_aborts), &
            new_unittest("pf_kde_grid%init applies R1 at the upper bound", &
                test_kde_grid_linear_range_not_at_upper_aborts), &
            new_unittest("pf_kde_grid%init refuses a spread_max that is not a number", &
                test_kde_grid_spread_max_not_finite_aborts), &
            new_unittest("pf_kde_grid%init refuses a spread_max below one", &
                test_kde_grid_spread_max_below_one_aborts), &
            new_unittest("the clamped cell count is advised and the fit still answers", &
                test_kde_fit_cells_clamped_advice), &
            new_unittest("the zone advice writes a tiny bound in the exponent form", &
                test_kde_zone_advice_tiny_support), &
            new_unittest("pf_kde_grid%init refuses a transform whose rounded length is too long", &
                test_kde_grid_binned_transform_length_aborts) &
            ]
        testsuite = [p1, p2, p3, p4, p5, p6]
    end subroutine collect_tests_parquet_numeric_errors

    !> A read-time sort orders ROWS; a descent path has one entry per element, so there is no row
    !> for its values to order.
    !> See `scenario_index_build_duplicate_direct` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_duplicate_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_duplicate_direct", expect_abort=.true., &
            failure_message="a duplicate key on the direct backend was expected to abort", &
            required_stderr="duplicate key")
    end subroutine test_index_build_duplicate_direct_aborts
    !
    !> See `scenario_index_build_duplicate_hash` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_duplicate_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_duplicate_hash", expect_abort=.true., &
            failure_message="a duplicate key on the hash backend was expected to abort", &
            required_stderr="duplicate key")
    end subroutine test_index_build_duplicate_hash_aborts
    !
    !> See `scenario_index_build_duplicate_sorted` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_duplicate_sorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_duplicate_sorted", expect_abort=.true., &
            failure_message="a duplicate key on the sorted backend was expected to abort", &
            required_stderr="duplicate key")
    end subroutine test_index_build_duplicate_sorted_aborts
    !
    !> See `scenario_index_build_duplicate_tuple_direct` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_duplicate_tuple_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_duplicate_tuple_direct", expect_abort=.true., &
            failure_message="a duplicate tuple on the direct backend was expected to abort", &
            required_stderr="duplicate key")
    end subroutine test_index_build_duplicate_tuple_direct_aborts
    !
    !> See `scenario_index_build_duplicate_tuple_hash` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_duplicate_tuple_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_duplicate_tuple_hash", expect_abort=.true., &
            failure_message="a duplicate tuple on the hash backend was expected to abort", &
            required_stderr="duplicate key")
    end subroutine test_index_build_duplicate_tuple_hash_aborts
    !
    !> See `scenario_index_build_value_zero` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_value_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_value_zero", expect_abort=.true., &
            failure_message="storing the value 0 was expected to abort", &
            required_stderr="must be >= 1")
    end subroutine test_index_build_value_zero_aborts
    !
    !> See `scenario_index_build_values_length` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_build_values_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_values_length", expect_abort=.true., &
            failure_message="a values array of the wrong length was expected to abort", &
            required_stderr="one element per key")
    end subroutine test_index_build_values_length_aborts
    !
    !> See `scenario_index_get_many_length` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_get_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_length", expect_abort=.true., &
            failure_message="a get_many length mismatch was expected to abort", &
            required_stderr="same length")
    end subroutine test_index_get_many_length_aborts
    !
    !> See `scenario_index_tuple_width_mismatch` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_tuple_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_tuple_width_mismatch", expect_abort=.true., &
            failure_message="a lookup of the wrong tuple width was expected to abort", &
            required_stderr="component count")
    end subroutine test_index_tuple_width_mismatch_aborts
    !
    !> See `scenario_index_scalar_on_composite` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_scalar_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_scalar_on_composite", expect_abort=.true., &
            failure_message="a scalar lookup on a composite map was expected to abort", &
            required_stderr="composite keys")
    end subroutine test_index_scalar_on_composite_aborts
    !
    !> See `scenario_index_sorted_set` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_sorted_set_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_sorted_set", expect_abort=.true., &
            failure_message="setting a key on a sorted map was expected to abort", &
            required_stderr="frozen once built")
    end subroutine test_index_sorted_set_aborts
    !
    !> See `scenario_index_sorted_remove` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_sorted_remove_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_sorted_remove", expect_abort=.true., &
            failure_message="removing a key from a sorted map was expected to abort", &
            required_stderr="frozen once built")
    end subroutine test_index_sorted_remove_aborts
    !
    !> See `scenario_index_sorted_get_or_add_absent` (test/error_scenarios.f90).
    subroutine test_index_sorted_get_or_add_absent_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_sorted_get_or_add_absent", expect_abort=.true., &
            failure_message="get_or_add of a new key on a sorted map was expected to abort", &
            required_stderr="pf_index_map%get_or_add: a sorted map is frozen once built")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "index_sorted_get_or_add_many_absent", &
            expect_abort=.true., &
            failure_message="get_or_add_many of a new key on a sorted map was expected to abort", &
            required_stderr="pf_index_map%get_or_add_many: a sorted map is frozen once built")
    end subroutine test_index_sorted_get_or_add_absent_aborts
    !
    !> See `scenario_index_sorted_composite` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_sorted_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_sorted_composite", expect_abort=.true., &
            failure_message="a composite sorted build was expected to abort", &
            required_stderr="single-component keys only")
    end subroutine test_index_sorted_composite_aborts
    !
    !> See `scenario_index_init_direct` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_init_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_init_direct", expect_abort=.true., &
            failure_message="init with method=direct was expected to abort", &
            required_stderr="only available on %build")
    end subroutine test_index_init_direct_aborts
    !
    !> See `scenario_index_init_sorted` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_init_sorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_init_sorted", expect_abort=.true., &
            failure_message="init with method=sorted was expected to abort", &
            required_stderr="only available on %build")
    end subroutine test_index_init_sorted_aborts
    !
    !> See `scenario_index_bad_method` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_bad_method", expect_abort=.true., &
            failure_message="an unknown method token was expected to abort", &
            required_stderr="unknown method")
    end subroutine test_index_bad_method_aborts
    !
    !> See `scenario_index_direct_set_out_of_range` (test/error_scenarios.f90) for why this is refused.
    !!
    !! The required text names `%set` as well as the reason, because this is the CONTROL for
    !! `test_index_direct_get_or_add_out_of_range_aborts`: the pair is only evidence that a refusal
    !! names the entry the caller used if this one is pinned to `%set` at the same time.
    subroutine test_index_direct_set_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_direct_set_out_of_range", expect_abort=.true., &
            failure_message="setting outside a direct map's range was expected to abort", &
            required_stderr="pf_index_map%set: this key is outside the range")
    end subroutine test_index_direct_set_out_of_range_aborts
    !
    !> See `scenario_index_direct_get_or_add_out_of_range` (test/error_scenarios.f90).
    !!
    !! Both arms assert the PROCEDURE NAME, which is the whole point: the two go through different
    !! workers (`ix_goa_scalar` and `ix_goa_tuple`) and must each name the entry the caller used
    !! rather than `%set`, which is what the refusal said before both workers guarded the direct
    !! backend themselves.
    subroutine test_index_direct_get_or_add_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_direct_get_or_add_out_of_range", &
            expect_abort=.true., &
            failure_message="get_or_add of an out-of-range key on a direct map was expected to abort", &
            required_stderr="pf_index_map%get_or_add: this key is outside the range")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "index_direct_get_or_add_many_out_of_range", &
            expect_abort=.true., &
            failure_message="get_or_add_many of an out-of-range key on a direct map was expected to abort", &
            required_stderr="pf_index_map%get_or_add_many: this key is outside the range")
    end subroutine test_index_direct_get_or_add_out_of_range_aborts
    !
    !> See `scenario_index_ncomp_too_large` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_ncomp_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_ncomp_too_large", expect_abort=.true., &
            failure_message="a key wider than the component limit was expected to abort", &
            required_stderr="at most")
    end subroutine test_index_ncomp_too_large_aborts
    !
    !> See `scenario_index_direct_range_too_wide` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_direct_range_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_direct_range_too_wide", expect_abort=.true., &
            failure_message="an explicit direct map over the whole int64 range was expected to abort", &
            required_stderr="cannot cover")
    end subroutine test_index_direct_range_too_wide_aborts
    !
    !> See `scenario_index_remove_absent` (test/error_scenarios.f90) for why this is refused.
    subroutine test_index_remove_absent_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_remove_absent", expect_abort=.true., &
            failure_message="removing an absent key without found= was expected to abort", &
            required_stderr="not in the map")
    end subroutine test_index_remove_absent_aborts
    !
    !> See `scenario_index_get_many_int32_overflow` (test/error_scenarios.f90). The scenario makes
    !> the boundary call first, so this passing means the guard fired on the value one past it and
    !> not on the largest legal one.
    subroutine test_index_get_many_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_int32_overflow", &
            expect_abort=.true., &
            failure_message="a stored value above huge(int32) was expected to abort an int32 answer", &
            required_stderr="too large for an int32 result")
    end subroutine test_index_get_many_int32_overflow_aborts
    !
    !> See `scenario_index_build_threads_zero` (test/error_scenarios.f90).
    subroutine test_index_build_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on %build was expected to abort", &
            required_stderr="threads= must be at least 1")
    end subroutine test_index_build_threads_zero_aborts
    !
    !> See `scenario_index_get_many_valid_length` (test/error_scenarios.f90). The scenario makes a
    !> correctly sized call first, so this passing means the guard fired on the short mask only.
    subroutine test_index_get_many_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_valid_length", &
            expect_abort=.true., &
            failure_message="a get_many mask of the wrong length was expected to abort", &
            required_stderr="valid= must have exactly one element per key")
    end subroutine test_index_get_many_valid_length_aborts
    !
    !> See `scenario_index_get_many_threads_zero` (test/error_scenarios.f90).
    subroutine test_index_get_many_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on %get_many was expected to abort", &
            required_stderr="get_many: threads= must be at least 1")
    end subroutine test_index_get_many_threads_zero_aborts
    !
    !> See `scenario_index_get_or_add_many_length` (test/error_scenarios.f90).
    subroutine test_index_get_or_add_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_or_add_many_length", &
            expect_abort=.true., &
            failure_message="a get_or_add_many length mismatch was expected to abort", &
            required_stderr="get_or_add_many: the keys and the answer array must have the same length")
    end subroutine test_index_get_or_add_many_length_aborts
    !
    !> See `scenario_index_get_or_add_many_valid_length` (test/error_scenarios.f90).
    subroutine test_index_get_or_add_many_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_or_add_many_valid_length", &
            expect_abort=.true., &
            failure_message="a get_or_add_many mask of the wrong length was expected to abort", &
            required_stderr="get_or_add_many: valid= must have exactly one element per key")
    end subroutine test_index_get_or_add_many_valid_length_aborts
    !
    !> See `scenario_index_get_or_add_many_int32_overflow` (test/error_scenarios.f90). The
    !> scenario reads the boundary value back first, so this passing means the guard fired on the
    !> code one past it and not on the largest legal one.
    subroutine test_index_get_or_add_many_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_or_add_many_int32_overflow", &
            expect_abort=.true., &
            failure_message="a code above huge(int32) was expected to abort an int32 get_or_add_many", &
            required_stderr="too large for an int32 result")
    end subroutine test_index_get_or_add_many_int32_overflow_aborts
    !
    !> See `scenario_index_keys_int32_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_keys_int32_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_int32_on_string_map", &
            expect_abort=.true., &
            failure_message="an int32 key list from a string map was expected to abort", &
            required_stderr="this map holds string keys")
    end subroutine test_index_keys_int32_on_string_map_aborts
    !
    !> See `scenario_index_keys_rank1_int32_on_composite` (test/error_scenarios.f90).
    subroutine test_index_keys_rank1_int32_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_rank1_int32_on_composite", &
            expect_abort=.true., &
            failure_message="an int32 rank-1 key list from a composite map was expected to abort", &
            required_stderr="this map has composite keys")
    end subroutine test_index_keys_rank1_int32_on_composite_aborts
    !
    !> See `scenario_index_keys_rank2_int32_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_keys_rank2_int32_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_rank2_int32_on_string_map", &
            expect_abort=.true., &
            failure_message="an int32 rank-2 key list from a string map was expected to abort", &
            required_stderr="this map holds string keys")
    end subroutine test_index_keys_rank2_int32_on_string_map_aborts
    !
    !> See `scenario_index_get_or_add_many_threaded_int32_overflow` (test/error_scenarios.f90).
    !> The threaded pass checks every code once the team has finished rather than row by row, so
    !> this is a second guard reaching the same message, not the one
    !> `test_index_get_or_add_many_int32_overflow_aborts` covers.
    subroutine test_index_get_or_add_many_threaded_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_or_add_many_threaded_int32_overflow", &
            expect_abort=.true., &
            failure_message="a threaded int32 get_or_add_many with oversized codes was expected to abort", &
            required_stderr="too large for an int32 result")
    end subroutine test_index_get_or_add_many_threaded_int32_overflow_aborts
    !
    !> See `scenario_index_pool_reserve_negative` (test/error_scenarios.f90). The scenario reserves
    !> a legal count first, so this passing means the guard fired on the negative one alone.
    subroutine test_index_pool_reserve_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_pool_reserve_negative", &
            expect_abort=.true., &
            failure_message="a negative pool reserve was expected to abort", &
            required_stderr="pf_index_pool%reserve: n must be >= 0")
    end subroutine test_index_pool_reserve_negative_aborts
    !
    !> See `scenario_multimap_keys_rank2_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_keys_rank2_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_keys_rank2_on_string_map", &
            expect_abort=.true., &
            failure_message="a rank-2 key list from a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys")
    end subroutine test_multimap_keys_rank2_on_string_map_aborts
    !
    !> See `scenario_multimap_keys_int32_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_keys_int32_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_keys_int32_on_string_map", &
            expect_abort=.true., &
            failure_message="an int32 key list from a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys")
    end subroutine test_multimap_keys_int32_on_string_map_aborts
    !
    !> See `scenario_multimap_keys_rank1_int32_on_composite` (test/error_scenarios.f90).
    subroutine test_multimap_keys_rank1_int32_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_keys_rank1_int32_on_composite", &
            expect_abort=.true., &
            failure_message="an int32 rank-1 key list from a composite multimap was expected to abort", &
            required_stderr="this multimap has composite keys")
    end subroutine test_multimap_keys_rank1_int32_on_composite_aborts
    !
    !> See `scenario_multimap_keys_rank2_int32_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_keys_rank2_int32_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_keys_rank2_int32_on_string_map", &
            expect_abort=.true., &
            failure_message="an int32 rank-2 key list from a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys")
    end subroutine test_multimap_keys_rank2_int32_on_string_map_aborts
    !
    !> See `scenario_multimap_string_keys_column_on_integer` (test/error_scenarios.f90).
    subroutine test_multimap_string_keys_column_on_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_keys_column_on_integer", &
            expect_abort=.true., &
            failure_message="a string key column from an integer multimap was expected to abort", &
            required_stderr="this multimap holds integer keys")
    end subroutine test_multimap_string_keys_column_on_integer_aborts
    !
    !> See `scenario_multimap_string_bulk_length` (test/error_scenarios.f90).
    subroutine test_multimap_string_bulk_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_bulk_length", &
            expect_abort=.true., &
            failure_message="a string bulk lookup of the wrong length was expected to abort", &
            required_stderr="the keys and the answer array must have the same length")
    end subroutine test_multimap_string_bulk_length_aborts
    !
    !> See `scenario_multimap_string_bulk_on_integer` (test/error_scenarios.f90).
    subroutine test_multimap_string_bulk_on_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_bulk_on_integer", &
            expect_abort=.true., &
            failure_message="a string bulk lookup on an integer multimap was expected to abort", &
            required_stderr="look up with integer keys")
    end subroutine test_multimap_string_bulk_on_integer_aborts
    !
    !> See `scenario_index_build_valid_length` (test/error_scenarios.f90).
    subroutine test_index_build_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_build_valid_length", &
            expect_abort=.true., &
            failure_message="a build mask of the wrong length was expected to abort", &
            required_stderr="build: valid= must have exactly one element per key")
    end subroutine test_index_build_valid_length_aborts
    !
    !> See `scenario_index_keys_rank1_on_composite` (test/error_scenarios.f90).
    subroutine test_index_keys_rank1_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_rank1_on_composite", &
            expect_abort=.true., &
            failure_message="a rank-1 key list from a composite map was expected to abort", &
            required_stderr="this map has composite keys")
    end subroutine test_index_keys_rank1_on_composite_aborts
    !
    !> See `scenario_index_masked_duplicate_direct` (test/error_scenarios.f90).
    subroutine test_index_masked_duplicate_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_masked_duplicate_direct", &
            expect_abort=.true., &
            failure_message="a duplicate in a masked direct build was expected to abort", &
            required_stderr="duplicate key 3 at position 5")
    end subroutine test_index_masked_duplicate_direct_aborts
    !
    !> See `scenario_index_masked_duplicate_tuple_direct` (test/error_scenarios.f90).
    subroutine test_index_masked_duplicate_tuple_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_masked_duplicate_tuple_direct", &
            expect_abort=.true., &
            failure_message="a duplicate tuple in a masked direct build was expected to abort", &
            required_stderr="at position 5")
    end subroutine test_index_masked_duplicate_tuple_direct_aborts
    !
    !> See `scenario_index_set_value_zero` (test/error_scenarios.f90).
    subroutine test_index_set_value_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_set_value_zero", &
            expect_abort=.true., &
            failure_message="a set storing the value 0 was expected to abort", &
            required_stderr="stored values must be >= 1")
    end subroutine test_index_set_value_zero_aborts
    !
    !> See `scenario_index_method_token_too_long` (test/error_scenarios.f90).
    subroutine test_index_method_token_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_method_token_too_long", &
            expect_abort=.true., &
            failure_message="an over-long method token was expected to abort", &
            required_stderr="unknown method (accepted:")
    end subroutine test_index_method_token_too_long_aborts
    !
    !> See `scenario_index_get_many_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_get_many_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_on_string_map", &
            expect_abort=.true., &
            failure_message="a bulk integer lookup on a string map was expected to abort", &
            required_stderr="this map holds string keys; look up with string keys")
    end subroutine test_index_get_many_on_string_map_aborts
    !
    !> See `scenario_index_get_many_ncomp_mismatch` (test/error_scenarios.f90).
    subroutine test_index_get_many_ncomp_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_many_ncomp_mismatch", &
            expect_abort=.true., &
            failure_message="a bulk lookup of the wrong component count was expected to abort", &
            required_stderr="the keys' component count does not match this map's")
    end subroutine test_index_get_many_ncomp_mismatch_aborts
    !
    !> See `scenario_index_keys_rank2_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_keys_rank2_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_rank2_on_string_map", &
            expect_abort=.true., &
            failure_message="a rank-2 key list from a string map was expected to abort", &
            required_stderr="ask for a parquet_string_column")
    end subroutine test_index_keys_rank2_on_string_map_aborts
    !
    !> See `scenario_index_composite_direct_product_too_wide` (test/error_scenarios.f90).
    subroutine test_index_composite_direct_product_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_composite_direct_product_too_wide", &
            expect_abort=.true., &
            failure_message="a composite direct build whose spans overflow was expected to abort", &
            required_stderr="their product exceeds the int64 domain")
    end subroutine test_index_composite_direct_product_too_wide_aborts
    !
    !> See `scenario_index_direct_alloc_refused` (test/error_scenarios.f90).
    subroutine test_index_direct_alloc_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_direct_alloc_refused", &
            expect_abort=.true., &
            failure_message="a direct build of unallocatable size was expected to abort", &
            required_stderr="slots for the direct backend")
    end subroutine test_index_direct_alloc_refused_aborts
    !
    !> The negative control: every legal call must run to completion, or a guard that fired
    !> unconditionally would satisfy every abort scenario above while breaking the library.
    subroutine test_index_control_completes(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_control", expect_abort=.false., &
            failure_message="every legal pf_index_map call was expected to complete", &
            required_stderr="index control finished")
    end subroutine test_index_control_completes
    !
    !> See `scenario_index_concurrent_abort` (test/error_scenarios.f90): two builds abort at once
    !> and the serialised reporter lets one through, with the duplicate named.
    subroutine test_index_concurrent_abort_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_concurrent_abort", expect_abort=.true., &
            failure_message="two concurrent builds with a duplicate key were expected to abort", &
            required_stderr="duplicate key 20")
    end subroutine test_index_concurrent_abort_aborts
    !
    !> See `scenario_index_spill_duplicate` (test/error_scenarios.f90): the second copy of a key
    !> crafted onto a partition's last slot is deferred to the spill pass and still named.
    subroutine test_index_spill_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_spill_duplicate", expect_abort=.true., &
            failure_message="a duplicate deferred to the spill pass was expected to abort (a clean exit " // &
            "also means the crafted keys never reached the spill pass -- read the scenario's stdout)", &
            required_stderr="at position 3 (every key must be unique)")
    end subroutine test_index_spill_duplicate_aborts
    !
    !> See `scenario_index_partitioned_duplicate` (test/error_scenarios.f90).
    subroutine test_index_partitioned_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_partitioned_duplicate", expect_abort=.true., &
            failure_message="a duplicate in a partitioned build was expected to abort", &
            required_stderr="duplicate key 30 at position 20001")
    end subroutine test_index_partitioned_duplicate_aborts
    !
    !> See `scenario_index_partitioned_duplicate_tuple` (test/error_scenarios.f90).
    subroutine test_index_partitioned_duplicate_tuple_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_partitioned_duplicate_tuple", &
            expect_abort=.true., &
            failure_message="a duplicate tuple in a partitioned build was expected to abort", &
            required_stderr="duplicate key [7, 30] at position 20001")
    end subroutine test_index_partitioned_duplicate_tuple_aborts
    !
    !> See `scenario_index_partitioned_duplicate_one_column` (test/error_scenarios.f90).
    subroutine test_index_partitioned_duplicate_one_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, &
            "index_partitioned_duplicate_one_column", expect_abort=.true., &
            failure_message="a duplicate in a partitioned one-column build was expected to abort", &
            required_stderr="duplicate key [30] at position 20001")
    end subroutine test_index_partitioned_duplicate_one_column_aborts
    !
    !> See `scenario_index_str_partitioned_duplicate` (test/error_scenarios.f90).
    subroutine test_index_str_partitioned_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_str_partitioned_duplicate", &
            expect_abort=.true., &
            failure_message="a duplicate string in a partitioned build was expected to abort", &
            required_stderr="duplicate key ""obj_5"" at position 20001")
    end subroutine test_index_str_partitioned_duplicate_aborts
    !
    !> See `scenario_index_get_or_add_many_threads_zero` (test/error_scenarios.f90).
    subroutine test_index_get_or_add_many_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_get_or_add_many_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on %get_or_add_many was expected to abort", &
            required_stderr="get_or_add_many: threads= must be at least 1")
    end subroutine test_index_get_or_add_many_threads_zero_aborts
    !
    !> See `scenario_index_partition_control` (test/error_scenarios.f90): the positive control
    !> of the partitioned passes.
    subroutine test_index_partition_control_completes(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_partition_control", expect_abort=.false., &
            failure_message="every threaded index build and get_or_add_many was expected to complete", &
            required_stderr="index partition control finished")
    end subroutine test_index_partition_control_completes
    !
    !> See `scenario_multimap_build_values_length` (test/error_scenarios.f90).
    subroutine test_multimap_build_values_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_values_length", &
            expect_abort=.true., &
            failure_message="a multimap values array of the wrong length was expected to abort", &
            required_stderr="pf_index_multimap%build: values= must have exactly one element per key")
    end subroutine test_multimap_build_values_length_aborts
    !
    !> See `scenario_multimap_build_value_zero` (test/error_scenarios.f90).
    subroutine test_multimap_build_value_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_value_zero", &
            expect_abort=.true., &
            failure_message="a multimap value of 0 was expected to abort", &
            required_stderr="pf_index_multimap%build: values(2) is 0")
    end subroutine test_multimap_build_value_zero_aborts
    !
    !> See `scenario_multimap_build_valid_length` (test/error_scenarios.f90).
    subroutine test_multimap_build_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_valid_length", &
            expect_abort=.true., &
            failure_message="a multimap build mask of the wrong length was expected to abort", &
            required_stderr="pf_index_multimap%build: valid= must have exactly one element per key")
    end subroutine test_multimap_build_valid_length_aborts
    !
    !> See `scenario_multimap_build_threads_zero` (test/error_scenarios.f90).
    subroutine test_multimap_build_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on a multimap build was expected to abort", &
            required_stderr="pf_index_multimap%build: threads= must be at least 1")
    end subroutine test_multimap_build_threads_zero_aborts
    !
    !> See `scenario_multimap_build_bad_method` (test/error_scenarios.f90).
    subroutine test_multimap_build_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_bad_method", &
            expect_abort=.true., &
            failure_message="an unknown multimap method was expected to abort", &
            required_stderr="pf_index_multimap%build: unknown method")
    end subroutine test_multimap_build_bad_method_aborts
    !
    !> See `scenario_multimap_build_sorted_composite` (test/error_scenarios.f90).
    subroutine test_multimap_build_sorted_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_build_sorted_composite", &
            expect_abort=.true., &
            failure_message="a composite sorted multimap build was expected to abort", &
            required_stderr="pf_index_multimap%build: method=""sorted"" supports single-component keys only")
    end subroutine test_multimap_build_sorted_composite_aborts
    !
    !> See `scenario_multimap_get_first_many_length` (test/error_scenarios.f90).
    subroutine test_multimap_get_first_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_first_many_length", &
            expect_abort=.true., &
            failure_message="a get_first_many length mismatch was expected to abort", &
            required_stderr="pf_index_multimap%get_first_many: the keys and the answer array must have the same length")
    end subroutine test_multimap_get_first_many_length_aborts
    !
    !> See `scenario_multimap_get_first_many_valid_length` (test/error_scenarios.f90).
    subroutine test_multimap_get_first_many_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_first_many_valid_length", &
            expect_abort=.true., &
            failure_message="a get_first_many mask of the wrong length was expected to abort", &
            required_stderr="pf_index_multimap%get_first_many: valid= must have exactly one element per key")
    end subroutine test_multimap_get_first_many_valid_length_aborts
    !
    !> See `scenario_multimap_get_first_many_threads_zero` (test/error_scenarios.f90).
    subroutine test_multimap_get_first_many_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_first_many_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on get_first_many was expected to abort", &
            required_stderr="pf_index_multimap%get_first_many: threads= must be at least 1")
    end subroutine test_multimap_get_first_many_threads_zero_aborts
    !
    !> See `scenario_multimap_get_many_length` (test/error_scenarios.f90).
    subroutine test_multimap_get_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_many_length", &
            expect_abort=.true., &
            failure_message="a multimap get_many length mismatch was expected to abort", &
            required_stderr="pf_index_multimap%get_many: the keys and the answer array must have the same length")
    end subroutine test_multimap_get_many_length_aborts
    !
    !> See `scenario_multimap_probe_many_valid_length` (test/error_scenarios.f90).
    subroutine test_multimap_probe_many_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_probe_many_valid_length", &
            expect_abort=.true., &
            failure_message="a probe_many mask of the wrong length was expected to abort", &
            required_stderr="pf_index_multimap%probe_many: valid= must have exactly one element per key")
    end subroutine test_multimap_probe_many_valid_length_aborts
    !
    !> See `scenario_multimap_probe_many_threads_zero` (test/error_scenarios.f90).
    subroutine test_multimap_probe_many_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_probe_many_threads_zero", &
            expect_abort=.true., &
            failure_message="threads=0 on probe_many was expected to abort", &
            required_stderr="pf_index_multimap%probe_many: threads= must be at least 1")
    end subroutine test_multimap_probe_many_threads_zero_aborts
    !
    !> See `scenario_multimap_probe_many_pair_overflow` (test/error_scenarios.f90).
    subroutine test_multimap_probe_many_pair_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_probe_many_pair_overflow", &
            expect_abort=.true., &
            failure_message="a pair count over the ceiling was expected to abort", &
            required_stderr="pf_index_multimap%probe_many: the pair count exceeds")
    end subroutine test_multimap_probe_many_pair_overflow_aborts
    !
    !> See `scenario_multimap_int32_answer_overflow` (test/error_scenarios.f90).
    subroutine test_multimap_int32_answer_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_int32_answer_overflow", &
            expect_abort=.true., &
            failure_message="a value too large for an int32 answer was expected to abort", &
            required_stderr="pf_index_multimap%get_first_many: a stored value is too large for an int32 answer")
    end subroutine test_multimap_int32_answer_overflow_aborts
    !
    !> See `scenario_multimap_get_all_int32_overflow` (test/error_scenarios.f90).
    subroutine test_multimap_get_all_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_all_int32_overflow", &
            expect_abort=.true., &
            failure_message="a value too large for an int32 get_all was expected to abort", &
            required_stderr="pf_index_multimap%get_all: a stored value is too large for an int32 answer")
    end subroutine test_multimap_get_all_int32_overflow_aborts
    !
    !> See `scenario_multimap_tuple_width_mismatch` (test/error_scenarios.f90).
    subroutine test_multimap_tuple_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_tuple_width_mismatch", &
            expect_abort=.true., &
            failure_message="a lookup of the wrong tuple width on a multimap was expected to abort", &
            required_stderr="pf_index_multimap%count: the key tuple's length does not match this multimap's component count")
    end subroutine test_multimap_tuple_width_mismatch_aborts
    !
    !> See `scenario_multimap_scalar_on_composite` (test/error_scenarios.f90).
    subroutine test_multimap_scalar_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_scalar_on_composite", &
            expect_abort=.true., &
            failure_message="a scalar lookup on a composite multimap was expected to abort", &
            required_stderr="pf_index_multimap%get: this multimap has composite keys")
    end subroutine test_multimap_scalar_on_composite_aborts
    !
    !> See `scenario_multimap_keys_rank1_on_composite` (test/error_scenarios.f90).
    subroutine test_multimap_keys_rank1_on_composite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_keys_rank1_on_composite", &
            expect_abort=.true., &
            failure_message="a rank-1 key list from a composite multimap was expected to abort", &
            required_stderr="pf_index_multimap%keys: this multimap has composite keys; ask for a rank-2 list")
    end subroutine test_multimap_keys_rank1_on_composite_aborts
    !
    !> See `scenario_multimap_probe_shape_mismatch` (test/error_scenarios.f90).
    subroutine test_multimap_probe_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_probe_shape_mismatch", &
            expect_abort=.true., &
            failure_message="a probe of the wrong tuple width was expected to abort", &
            required_stderr="pf_index_multimap%probe_many: the keys' component count does not match this multimap's")
    end subroutine test_multimap_probe_shape_mismatch_aborts
    !
    !> See `scenario_multimap_tuple_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_tuple_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_tuple_on_string_map", &
            expect_abort=.true., &
            failure_message="a key tuple on a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; look up with a string key")
    end subroutine test_multimap_tuple_on_string_map_aborts
    !
    !> See `scenario_multimap_integer_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_integer_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_integer_on_string_map", &
            expect_abort=.true., &
            failure_message="an integer key on a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; look up with a string key")
    end subroutine test_multimap_integer_on_string_map_aborts
    !
    !> See `scenario_multimap_get_first_many_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_get_first_many_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_first_many_on_string_map", &
            expect_abort=.true., &
            failure_message="a bulk integer lookup on a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; look up with string keys")
    end subroutine test_multimap_get_first_many_on_string_map_aborts
    !
    !> See `scenario_multimap_get_first_many_ncomp_mismatch` (test/error_scenarios.f90).
    subroutine test_multimap_get_first_many_ncomp_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_get_first_many_ncomp_mismatch", &
            expect_abort=.true., &
            failure_message="a bulk multimap lookup of the wrong width was expected to abort", &
            required_stderr="the keys' component count does not match this multimap's")
    end subroutine test_multimap_get_first_many_ncomp_mismatch_aborts
    !
    !> See `scenario_multimap_probe_many_on_string_map` (test/error_scenarios.f90).
    subroutine test_multimap_probe_many_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_probe_many_on_string_map", &
            expect_abort=.true., &
            failure_message="an integer probe of a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; probe with string keys")
    end subroutine test_multimap_probe_many_on_string_map_aborts
    !
    !> See `scenario_multimap_direct_range_too_wide` (test/error_scenarios.f90).
    subroutine test_multimap_direct_range_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_direct_range_too_wide", &
            expect_abort=.true., &
            failure_message="a direct multimap spanning the int64 domain was expected to abort", &
            required_stderr="it exceeds the whole int64 domain")
    end subroutine test_multimap_direct_range_too_wide_aborts
    !
    !> See `scenario_multimap_composite_direct_product_too_wide` (test/error_scenarios.f90).
    subroutine test_multimap_composite_direct_product_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, &
            "multimap_composite_direct_product_too_wide", expect_abort=.true., &
            failure_message="a composite direct multimap whose spans overflow was expected to abort", &
            required_stderr="their product exceeds the int64 domain")
    end subroutine test_multimap_composite_direct_product_too_wide_aborts
    !
    !> See `scenario_multimap_direct_alloc_refused` (test/error_scenarios.f90).
    subroutine test_multimap_direct_alloc_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_direct_alloc_refused", &
            expect_abort=.true., &
            failure_message="a direct multimap of unallocatable size was expected to abort", &
            required_stderr="slots for the direct backend")
    end subroutine test_multimap_direct_alloc_refused_aborts
    !
    !> See `scenario_multimap_control` (test/error_scenarios.f90).
    subroutine test_multimap_control_completes(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_control", expect_abort=.false., &
            failure_message="every legal pf_index_multimap call was expected to complete", &
            required_stderr="multimap control finished")
    end subroutine test_multimap_control_completes
    !
    !> See `scenario_index_string_on_integer_map` (test/error_scenarios.f90).
    subroutine test_index_string_on_integer_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_on_integer_map", expect_abort=.true., &
            failure_message="a string key on an integer map was expected to abort", &
            required_stderr="this map holds integer keys; look up with an integer key")
    end subroutine test_index_string_on_integer_map_aborts
    !
    !> See `scenario_index_integer_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_integer_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_integer_on_string_map", expect_abort=.true., &
            failure_message="an integer key on a string map was expected to abort", &
            required_stderr="this map holds string keys; look up with a string key")
    end subroutine test_index_integer_on_string_map_aborts
    !
    !> See `scenario_index_tuple_on_string_map` (test/error_scenarios.f90).
    subroutine test_index_tuple_on_string_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_tuple_on_string_map", expect_abort=.true., &
            failure_message="a key tuple on a string map was expected to abort", &
            required_stderr="this map holds string keys; look up with a string key")
    end subroutine test_index_tuple_on_string_map_aborts
    !
    !> See `scenario_index_string_get_many_on_integer_map` (test/error_scenarios.f90).
    subroutine test_index_string_get_many_on_integer_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_get_many_on_integer_map", expect_abort=.true., &
            failure_message="string keys in get_many on an integer map were expected to abort", &
            required_stderr="this map holds integer keys; look up with integer keys")
    end subroutine test_index_string_get_many_on_integer_map_aborts
    !
    !> See `scenario_index_string_method_direct` (test/error_scenarios.f90).
    subroutine test_index_string_method_direct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_method_direct", expect_abort=.true., &
            failure_message="method=direct on a string build was expected to abort", &
            required_stderr="a string-keyed map is always hashed")
    end subroutine test_index_string_method_direct_aborts
    !
    !> See `scenario_index_string_method_sorted` (test/error_scenarios.f90).
    subroutine test_index_string_method_sorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_method_sorted", expect_abort=.true., &
            failure_message="method=sorted on a string build was expected to abort", &
            required_stderr="a string-keyed map is always hashed")
    end subroutine test_index_string_method_sorted_aborts
    !
    !> See `scenario_index_string_build_duplicate` (test/error_scenarios.f90).
    subroutine test_index_string_build_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_build_duplicate", expect_abort=.true., &
            failure_message="a duplicate string key was expected to abort", &
            required_stderr="duplicate key ""aa"" at position 3")
    end subroutine test_index_string_build_duplicate_aborts
    !
    !> See `scenario_index_string_keys_rank1` (test/error_scenarios.f90).
    subroutine test_index_string_keys_rank1_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_keys_rank1", expect_abort=.true., &
            failure_message="keys() into an integer list on a string map was expected to abort", &
            required_stderr="this map holds string keys; ask for a parquet_string_column")
    end subroutine test_index_string_keys_rank1_aborts
    !
    !> See `scenario_index_string_keys_column_on_integer` (test/error_scenarios.f90).
    subroutine test_index_string_keys_column_on_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_keys_column_on_integer", expect_abort=.true., &
            failure_message="keys() into a string column on an integer map was expected to abort", &
            required_stderr="this map holds integer keys; ask for an integer list")
    end subroutine test_index_string_keys_column_on_integer_aborts
    !
    !> See `scenario_index_string_init_ncomp` (test/error_scenarios.f90).
    subroutine test_index_string_init_ncomp_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_init_ncomp", expect_abort=.true., &
            failure_message="a string init with two components was expected to abort", &
            required_stderr="a string key has one component")
    end subroutine test_index_string_init_ncomp_aborts
    !
    !> See `scenario_index_string_set_on_integer_map` (test/error_scenarios.f90).
    subroutine test_index_string_set_on_integer_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_set_on_integer_map", expect_abort=.true., &
            failure_message="set with a string key on an integer map was expected to abort", &
            required_stderr="this map holds integer keys; use an integer key")
    end subroutine test_index_string_set_on_integer_map_aborts
    !
    !> See `scenario_index_string_remove_absent` (test/error_scenarios.f90).
    subroutine test_index_string_remove_absent_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_remove_absent", expect_abort=.true., &
            failure_message="removing an absent string key was expected to abort", &
            required_stderr="this key is not in the map; pass found=")
    end subroutine test_index_string_remove_absent_aborts
    !
    !> See `scenario_index_string_get_many_length` (test/error_scenarios.f90).
    subroutine test_index_string_get_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_get_many_length", expect_abort=.true., &
            failure_message="a string get_many with a short answer array was expected to abort", &
            required_stderr="the keys and the answer array must have the same length")
    end subroutine test_index_string_get_many_length_aborts
    !
    !> See `scenario_multimap_string_on_integer` (test/error_scenarios.f90).
    subroutine test_multimap_string_on_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_on_integer", expect_abort=.true., &
            failure_message="a string key on an integer multimap was expected to abort", &
            required_stderr="this multimap holds integer keys; look up with an integer key")
    end subroutine test_multimap_string_on_integer_aborts
    !
    !> See `scenario_multimap_integer_on_string` (test/error_scenarios.f90).
    subroutine test_multimap_integer_on_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_integer_on_string", expect_abort=.true., &
            failure_message="an integer key on a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; look up with a string key")
    end subroutine test_multimap_integer_on_string_aborts
    !
    !> See `scenario_multimap_string_method_sorted` (test/error_scenarios.f90).
    subroutine test_multimap_string_method_sorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_method_sorted", expect_abort=.true., &
            failure_message="method=sorted on a string multimap build was expected to abort", &
            required_stderr="pf_index_multimap%build: a string-keyed map is always hashed")
    end subroutine test_multimap_string_method_sorted_aborts
    !
    !> See `scenario_multimap_string_probe_on_integer` (test/error_scenarios.f90).
    subroutine test_multimap_string_probe_on_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_probe_on_integer", expect_abort=.true., &
            failure_message="string probes on an integer multimap were expected to abort", &
            required_stderr="this multimap holds integer keys; probe with integer keys")
    end subroutine test_multimap_string_probe_on_integer_aborts
    !
    !> See `scenario_multimap_string_keys_rank1` (test/error_scenarios.f90).
    subroutine test_multimap_string_keys_rank1_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_string_keys_rank1", expect_abort=.true., &
            failure_message="keys() into an integer list on a string multimap was expected to abort", &
            required_stderr="this multimap holds string keys; ask for a parquet_string_column")
    end subroutine test_multimap_string_keys_rank1_aborts

    !> See `scenario_index_keys_int32_negative` (test/error_scenarios.f90). A key BELOW the int32
    !> range, which only a two-sided bound check refuses.
    subroutine test_index_keys_int32_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_int32_negative", expect_abort=.true., &
            failure_message="a key below the int32 range was expected to be refused by %keys", &
            required_stderr="a stored key is outside the range an int32 answer can hold")
    end subroutine test_index_keys_int32_negative_aborts

    !> See `scenario_index_keys_rank2_int32_negative` (test/error_scenarios.f90). The rank-2
    !> narrowing is a separate procedure from the rank-1 one, so it needs its own control.
    subroutine test_index_keys_rank2_int32_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_keys_rank2_int32_negative", expect_abort=.true., &
            failure_message="a key below the int32 range was expected to be refused by the rank-2 %keys", &
            required_stderr="a stored key is outside the range an int32 answer can hold")
    end subroutine test_index_keys_rank2_int32_negative_aborts

    !> See `scenario_multimap_csr_int32_value` (test/error_scenarios.f90).
    subroutine test_multimap_csr_int32_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "multimap_csr_int32_value", expect_abort=.true., &
            failure_message="a stored value above the int32 range was expected to be refused by %csr", &
            required_stderr="a stored value is too large for an int32 answer")
    end subroutine test_multimap_csr_int32_value_aborts
    !
    !> See `scenario_index_string_control` (test/error_scenarios.f90).
    subroutine test_index_string_control_completes(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_control", expect_abort=.false., &
            failure_message="every legal string-key call was expected to complete", &
            required_stderr="string index control finished")
    end subroutine test_index_string_control_completes
    !
    !> See `scenario_index_string_chain` (test/error_scenarios.f90). The depth in the message is
    !> what proves the warning is said once: the run passes 32 by some twenty keys, and every
    !> insert past the first crossing would otherwise report "33", "34", ...
    subroutine test_index_string_chain_warning(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "index_string_chain_warning", expect_abort=.false., &
            failure_message="a run of 32 string keys sharing a hash was expected to be reported", &
            required_stderr="WARNING: pf_index_map%build: 32 string keys share one 64-bit hash")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "index_string_chain_warning", expect_abort=.false., &
            failure_message="the long-run warning was expected once per map, not once per insert", &
            forbidden_text="33 string keys share one")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "index_string_chain_warning", expect_abort=.false., &
            failure_message="max_hash_chain was expected to report the run of fifty-odd keys", &
            required_stderr="max_hash_chain=50")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "index_string_chain_quiet", expect_abort=.false., &
            failure_message="keys sharing no hash were expected to raise no warning", &
            forbidden_text="share one 64-bit hash")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "index_string_chain_quiet", expect_abort=.false., &
            failure_message="at full width the hundred keys were expected to share no hash", &
            required_stderr="max_hash_chain=1")
    end subroutine test_index_string_chain_warning
    !
    !> See `scenario_table_index_string_key_on_int` (test/error_scenarios.f90).
    subroutine test_table_index_string_key_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_index_string_key_on_int", expect_abort=.true., &
            failure_message="a string key on an integer index was expected to abort", &
            required_stderr="was asked for a string key")
    end subroutine test_table_index_string_key_on_int_aborts
    !
    !> See `scenario_table_index_int_key_on_string` (test/error_scenarios.f90).
    subroutine test_table_index_int_key_on_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_index_int_key_on_string", expect_abort=.true., &
            failure_message="an integer key on a string index was expected to abort", &
            required_stderr="this index is over string column 's', and was asked for an integer key")
    end subroutine test_table_index_int_key_on_string_aborts
    !
    !> See `scenario_pool_double_free` (test/error_scenarios.f90) for why this is refused.
    subroutine test_pool_double_free_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "pool_double_free", expect_abort=.true., &
            failure_message="a double free was expected to abort", &
            required_stderr="already free")
    end subroutine test_pool_double_free_aborts
    !
    !> See `scenario_pool_free_never_issued` (test/error_scenarios.f90) for why this is refused.
    subroutine test_pool_free_never_issued_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "pool_free_never_issued", expect_abort=.true., &
            failure_message="freeing a never-issued index was expected to abort", &
            required_stderr="never handed out")
    end subroutine test_pool_free_never_issued_aborts
    !
    !> See `scenario_pool_free_zero` (test/error_scenarios.f90) for why this is refused.
    subroutine test_pool_free_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "pool_free_zero", expect_abort=.true., &
            failure_message="freeing index 0 was expected to abort", &
            required_stderr="never handed out")
    end subroutine test_pool_free_zero_aborts
    !
    !> The negative control: every legal call must run to completion, or a guard that fired
    !> unconditionally would satisfy every abort scenario above while breaking the library.
    !> `bounded=.true.` and a sort are mutually exclusive, and the refusal names the remedy.
    !!
    !! A sort reorders rows across the whole file, so a sorted row belongs to no row group and no
    !! column can be assembled one row group at a time. The message is asserted rather than only
    !! the abort, because what makes this refusal acceptable is that it tells the caller what to do
    !! instead -- only the library's own text is matched, never the runtime's `ERROR STOP` prefix,
    !! which differs between compilers.
    subroutine test_bounded_with_sort_refused(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "bounded_with_sort_refused", expect_abort=.true., &
            failure_message="bounded=.true. with a sort= was expected to be refused at open", &
            required_stderr="bounded=.true. cannot be combined with a sort")
    end subroutine test_bounded_with_sort_refused

    !> The same refusal reached through a read-in MAML's `extra: sort:` list rather than `sort=`.
    !! Both are merged into one composed sort before the guard sees them, so this proves the single
    !! guard really does cover both spellings.
    subroutine test_bounded_with_maml_sort_refused(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "bounded_with_maml_sort_refused", expect_abort=.true., &
            failure_message="bounded=.true. with a maml sort list was expected to be refused at open", &
            required_stderr="bounded=.true. cannot be combined with a sort")
    end subroutine test_bounded_with_maml_sort_refused

    !> A hard `qc=` bound under `bounded` is enforced when the column is READ, per row group.
    !!
    !! The scenario prints a line after the open and before the read, so the abort message naming a
    !! row group is what proves both halves: that the open did not enforce it, and that a violation
    !! outside the FIRST row group is still caught -- an implementation checking only the first
    !! chunk would pass this fixture silently.
    subroutine test_bounded_qc_hard_at_first_touch(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "bounded_qc_hard_at_first_touch", expect_abort=.true., &
            failure_message="a bounded qc violation in a later row group was expected to abort on first touch", &
            required_stderr="qc violation for column 'idx [row group 5]'")
    end subroutine test_bounded_qc_hard_at_first_touch

    !> The soft counterpart: `qc_soft=.true.` under `bounded` warns rather than aborting, and the
    !! read completes. Both halves matter -- a guard that aborted regardless would pass the hard
    !! test alone, and one that never fired would pass neither.
    subroutine test_bounded_qc_soft_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "bounded_qc_soft_warns", expect_abort=.false., &
            failure_message="a bounded soft qc violation was expected to warn rather than abort", &
            required_stderr="qc violation for column 'idx [row group 5]'")
    end subroutine test_bounded_qc_soft_warns

    subroutine test_pool_control_completes(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "pool_control", expect_abort=.false., &
            failure_message="every legal pf_index_pool call was expected to complete", &
            required_stderr="pool control finished")
    end subroutine test_pool_control_completes
    !
    !> See scenario_table_group_add_agg_weights_exact.
    subroutine test_table_group_add_agg_weights_exact_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_weights_exact", expect_abort=.true., &
            failure_message="add_agg with weight_column= and exact= together was expected to abort", &
            required_stderr="weights= and weight_column= have no meaning with exact=.true.")
    end subroutine test_table_group_add_agg_weights_exact_aborts
    !
    !> See scenario_column_container_wrong_kind.
    subroutine test_column_container_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_container_wrong_kind", expect_abort=.true., &
            failure_message="%container_ptr on an int32 column was expected to abort", &
            required_stderr="container: column kind is PK_INT32, but this call requires a container kind")
    end subroutine test_column_container_wrong_kind_aborts
    !
    !> See scenario_column_append_row_of_container.
    subroutine test_column_append_row_of_container_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_append_row_of_container", expect_abort=.true., &
            failure_message="%append_row_of from a list column was expected to abort", &
            required_stderr="append_row_of: appending a container row is not implemented yet")
    end subroutine test_column_append_row_of_container_aborts
    !
    !> See scenario_filter_set_name_too_long.
    subroutine test_filter_set_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "filter_set_name_too_long", expect_abort=.true., &
            failure_message="an over-long set name was expected to abort", &
            required_stderr="is longer than the 64-character limit")
    end subroutine test_filter_set_name_too_long_aborts
    !
    !> See scenario_filter_bare_at_set_name.
    subroutine test_filter_bare_at_set_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "filter_bare_at_set_name", expect_abort=.true., &
            failure_message="a bare @ set name was expected to abort", &
            required_stderr="'@' must be followed by a set name")
    end subroutine test_filter_bare_at_set_name_aborts
    !
    !> See scenario_skycoord_text_long_separator.
    subroutine test_skycoord_text_long_separator_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_text_long_separator", expect_abort=.true., &
            failure_message="a 150-character separator was expected to abort", &
            required_stderr="xxx..."")")
    end subroutine test_skycoord_text_long_separator_aborts

    ! ---- %build_index and parquet_table_index ---------------------------------------------------
    !
    !> See `scenario_table_index_string_column` (test/error_scenarios.f90): the refusal this once
    !! asserted lifted with the map's string keys, and the scenario is now a control.
    subroutine test_table_index_string_column_completes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_string_column", expect_abort=.false., &
            failure_message="build_index over a string column was expected to complete and answer", &
            required_stderr="a string column was indexed and answered")
    end subroutine test_table_index_string_column_completes
    !
    !> See `scenario_table_index_bool_column` (test/error_scenarios.f90).
    subroutine test_table_index_bool_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_bool_column", expect_abort=.true., &
            failure_message="build_index over a boolean column was expected to abort", &
            required_stderr="is a boolean column, which cannot be indexed")
    end subroutine test_table_index_bool_column_aborts
    !
    !> See `scenario_table_index_vector_column` (test/error_scenarios.f90).
    subroutine test_table_index_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_vector_column", expect_abort=.true., &
            failure_message="build_index over a vector column was expected to abort", &
            required_stderr="is a vector column; an index needs one value per row")
    end subroutine test_table_index_vector_column_aborts
    !
    !> See `scenario_table_index_duplicate_unique` (test/error_scenarios.f90): the engine's own
    !! refusal, naming the key.
    subroutine test_table_index_duplicate_unique_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_duplicate_unique", expect_abort=.true., &
            failure_message="a repeated key under unique=.true. was expected to abort", &
            required_stderr="duplicate key 7")
    end subroutine test_table_index_duplicate_unique_aborts
    !
    !> See `scenario_table_index_missing_column` (test/error_scenarios.f90).
    subroutine test_table_index_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_missing_column", expect_abort=.true., &
            failure_message="build_index over a missing column was expected to abort", &
            required_stderr="build_index: no column of this name")
    end subroutine test_table_index_missing_column_aborts
    !
    !> See `scenario_table_index_stale_find` (test/error_scenarios.f90): the generation check on
    !! every query, one scenario per query family.
    subroutine test_table_index_stale_find_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_stale_find", expect_abort=.true., &
            failure_message="find on a stale index was expected to abort", &
            required_stderr="parquet_table_index: find: this table has changed structurally since the index was built")
    end subroutine test_table_index_stale_find_aborts
    !
    !> See `scenario_table_index_stale_find_all` (test/error_scenarios.f90).
    subroutine test_table_index_stale_find_all_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_stale_find_all", expect_abort=.true., &
            failure_message="find_all on a stale index was expected to abort", &
            required_stderr="parquet_table_index: find_all: this table has changed structurally")
    end subroutine test_table_index_stale_find_all_aborts
    !
    !> See `scenario_table_index_stale_find_many` (test/error_scenarios.f90).
    subroutine test_table_index_stale_find_many_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_stale_find_many", expect_abort=.true., &
            failure_message="find_many on a stale index was expected to abort", &
            required_stderr="parquet_table_index: find_many: this table has changed structurally")
    end subroutine test_table_index_stale_find_many_aborts
    !
    !> See `scenario_table_index_stale_count` (test/error_scenarios.f90).
    subroutine test_table_index_stale_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_stale_count", expect_abort=.true., &
            failure_message="count on a stale index was expected to abort", &
            required_stderr="parquet_table_index: count: this table has changed structurally")
    end subroutine test_table_index_stale_count_aborts
    !
    !> See `scenario_table_index_never_built` (test/error_scenarios.f90).
    subroutine test_table_index_never_built_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_never_built", expect_abort=.true., &
            failure_message="a query on a never-built index was expected to abort", &
            required_stderr="no index has been built into this object")
    end subroutine test_table_index_never_built_aborts
    !
    !> See `scenario_table_index_kind_mismatch` (test/error_scenarios.f90).
    subroutine test_table_index_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_kind_mismatch", expect_abort=.true., &
            failure_message="a real key on an integer index was expected to abort", &
            required_stderr="this index is over int32 column 'id', and was asked for a real key")
    end subroutine test_table_index_kind_mismatch_aborts
    !
    !> See `scenario_table_index_kind_mismatch_temporal` (test/error_scenarios.f90).
    subroutine test_table_index_kind_mismatch_temporal_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_kind_mismatch_temporal", expect_abort=.true., &
            failure_message="a timestamp key on a date index was expected to abort", &
            required_stderr="this index is over date column 'd', and was asked for a parquet_timestamp key")
    end subroutine test_table_index_kind_mismatch_temporal_aborts
    !
    !> See `scenario_table_index_find_many_length` (test/error_scenarios.f90).
    subroutine test_table_index_find_many_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_find_many_length", expect_abort=.true., &
            failure_message="find_many with too few answer slots was expected to abort", &
            required_stderr="find_many: rows has 2 entries but keys has 3")
    end subroutine test_table_index_find_many_length_aborts
    !
    !> See `scenario_table_index_threads_zero` (test/error_scenarios.f90): the engine's own guard.
    subroutine test_table_index_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_threads_zero", expect_abort=.true., &
            failure_message="threads=0 on build_index was expected to abort", &
            required_stderr="threads= must be at least 1")
    end subroutine test_table_index_threads_zero_aborts
    !
    !> THE NEGATIVE CONTROL for every table-index guard above: every legal path completes.
    subroutine test_table_index_control_completes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_index_control", expect_abort=.false., &
            failure_message="every legal build_index and parquet_table_index call was expected to complete", &
            required_stderr="table index control finished")
    end subroutine test_table_index_control_completes
    !
    !> See `scenario_table_group_no_key` (test/error_scenarios.f90).
    subroutine test_table_group_no_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_no_key", expect_abort=.true., &
            failure_message="group_by with no key was expected to abort", &
            required_stderr="parquet_table: group_by: no key column was given")
    end subroutine test_table_group_no_key_aborts
    !
    !> See `scenario_table_group_unknown_key` (test/error_scenarios.f90).
    subroutine test_table_group_unknown_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_unknown_key", expect_abort=.true., &
            failure_message="group_by on a missing column was expected to abort", &
            required_stderr="parquet_table: group_by: no column of this name")
    end subroutine test_table_group_unknown_key_aborts
    !
    !> See `scenario_table_group_direction_token` (test/error_scenarios.f90).
    subroutine test_table_group_direction_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_direction_token", expect_abort=.true., &
            failure_message="group_by on a direction token was expected to abort", &
            required_stderr="reads as a sort key, and a group key has no direction")
    end subroutine test_table_group_direction_token_aborts
    !
    !> See `scenario_table_group_vector_column` (test/error_scenarios.f90).
    subroutine test_table_group_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_vector_column", expect_abort=.true., &
            failure_message="group_by on a vector column was expected to abort", &
            required_stderr="cannot be a group key; there is no defined order on a whole vector row")
    end subroutine test_table_group_vector_column_aborts
    !
    !> See `scenario_table_group_container_column` (test/error_scenarios.f90).
    subroutine test_table_group_container_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_container_column", expect_abort=.true., &
            failure_message="group_by on a list column was expected to abort", &
            required_stderr="cannot be a group key; there is no defined order on a list, a map or a struct")
    end subroutine test_table_group_container_column_aborts
    !
    !> See `scenario_table_group_stale_size` (test/error_scenarios.f90).
    subroutine test_table_group_stale_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_size", expect_abort=.true., &
            failure_message="size on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: size: this table has changed structurally since the grouping was built")
    end subroutine test_table_group_stale_size_aborts
    !
    !> See `scenario_table_group_stale_rows` (test/error_scenarios.f90).
    subroutine test_table_group_stale_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_rows", expect_abort=.true., &
            failure_message="rows on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: rows: this table has changed structurally")
    end subroutine test_table_group_stale_rows_aborts
    !
    !> See `scenario_table_group_stale_csr` (test/error_scenarios.f90).
    subroutine test_table_group_stale_csr_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_csr", expect_abort=.true., &
            failure_message="csr on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: csr: this table has changed structurally")
    end subroutine test_table_group_stale_csr_aborts
    !
    !> See `scenario_table_group_stale_first_rows` (test/error_scenarios.f90).
    subroutine test_table_group_stale_first_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_first_rows", expect_abort=.true., &
            failure_message="first_rows on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: first_rows: this table has changed structurally")
    end subroutine test_table_group_stale_first_rows_aborts
    !
    !> See `scenario_table_group_stale_group_ids` (test/error_scenarios.f90).
    subroutine test_table_group_stale_group_ids_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_group_ids", expect_abort=.true., &
            failure_message="group_ids on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: group_ids: this table has changed structurally")
    end subroutine test_table_group_stale_group_ids_aborts
    !
    !> See `scenario_table_group_stale_key_table` (test/error_scenarios.f90).
    subroutine test_table_group_stale_key_table_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_key_table", expect_abort=.true., &
            failure_message="key_table on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: key_table: this table has changed structurally")
    end subroutine test_table_group_stale_key_table_aborts
    !
    !> See `scenario_table_group_stale_count` (test/error_scenarios.f90).
    subroutine test_table_group_stale_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_count", expect_abort=.true., &
            failure_message="count on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: count: this table has changed structurally")
    end subroutine test_table_group_stale_count_aborts
    !
    !> See `scenario_table_group_never_built` (test/error_scenarios.f90).
    subroutine test_table_group_never_built_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_never_built", expect_abort=.true., &
            failure_message="a query on a never-built grouping was expected to abort", &
            required_stderr="no grouping has been built into this object")
    end subroutine test_table_group_never_built_aborts
    !
    !> See `scenario_table_group_out_of_range` (test/error_scenarios.f90).
    subroutine test_table_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_out_of_range", expect_abort=.true., &
            failure_message="rows with a group number out of range was expected to abort", &
            required_stderr="parquet_grouping: rows: group 4 is out of range")
    end subroutine test_table_group_out_of_range_aborts
    !
    !> See `scenario_table_group_size_name_clash` (test/error_scenarios.f90).
    subroutine test_table_group_size_name_clash_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_size_name_clash", expect_abort=.true., &
            failure_message="key_table with a size_name naming a key was expected to abort", &
            required_stderr="is already a key column's name")
    end subroutine test_table_group_size_name_clash_aborts
    !
    !> See `scenario_table_group_key_table_reserve_negative` (test/error_scenarios.f90).
    subroutine test_table_group_key_table_reserve_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_key_table_reserve_negative", &
            expect_abort=.true., &
            failure_message="key_table with a negative reserve was expected to abort", &
            required_stderr="parquet_grouping: key_table: reserve= must be at least 0, got -1")
    end subroutine test_table_group_key_table_reserve_negative_aborts
    !
    !> See `scenario_table_group_add_agg_target_is_source` (test/error_scenarios.f90).
    subroutine test_table_group_add_agg_target_is_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_target_is_source", &
            expect_abort=.true., &
            failure_message="add_agg onto the grouping's own table was expected to abort", &
            required_stderr="parquet_grouping: add_agg: the target table is the one this grouping was built from")
    end subroutine test_table_group_add_agg_target_is_source_aborts
    !
    !> See `scenario_table_group_add_agg_rows_mismatch` (test/error_scenarios.f90).
    subroutine test_table_group_add_agg_rows_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_rows_mismatch", &
            expect_abort=.true., &
            failure_message="add_agg onto a target of the wrong length was expected to abort", &
            required_stderr="parquet_grouping: add_agg: the target table has 5 rows and this grouping has 3 groups")
    end subroutine test_table_group_add_agg_rows_mismatch_aborts
    !
    !> See `scenario_table_group_add_agg_as_two_names` (test/error_scenarios.f90).
    subroutine test_table_group_add_agg_as_two_names_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_as_two_names", &
            expect_abort=.true., &
            failure_message="add_agg with two names in as= was expected to abort", &
            required_stderr="parquet_grouping: add_agg: as= must name exactly one column, got 2 names")
    end subroutine test_table_group_add_agg_as_two_names_aborts
    !
    !> See `scenario_table_group_add_agg_nan_to_null_exact` (test/error_scenarios.f90).
    subroutine test_table_group_add_agg_nan_to_null_exact_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_nan_to_null_exact", &
            expect_abort=.true., &
            failure_message="add_agg with nan_to_null= and exact= together was expected to abort", &
            required_stderr="parquet_grouping: add_agg: nan_to_null= has no meaning with exact=.true.")
    end subroutine test_table_group_add_agg_nan_to_null_exact_aborts
    !
    !> See `scenario_table_group_stale_add_agg` (test/error_scenarios.f90).
    subroutine test_table_group_stale_add_agg_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_add_agg", expect_abort=.true., &
            failure_message="add_agg on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: add_agg: this table has changed structurally")
    end subroutine test_table_group_stale_add_agg_aborts
    !
    !> See `scenario_table_group_stale_add_size` (test/error_scenarios.f90).
    subroutine test_table_group_stale_add_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_add_size", expect_abort=.true., &
            failure_message="add_size on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: add_size: this table has changed structurally")
    end subroutine test_table_group_stale_add_size_aborts
    !
    !> See `scenario_table_group_add_agg_unknown_token` (test/error_scenarios.f90).
    subroutine test_table_group_add_agg_unknown_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_agg_unknown_token", &
            expect_abort=.true., &
            failure_message="add_agg with an unknown statistic token was expected to abort", &
            required_stderr="parquet_grouping: add_agg: unknown statistic 'medain'")
    end subroutine test_table_group_add_agg_unknown_token_aborts
    !
    !> See `scenario_table_group_add_size_name_taken` (test/error_scenarios.f90).
    subroutine test_table_group_add_size_name_taken_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_size_name_taken", &
            expect_abort=.true., &
            failure_message="add_size under a name the target already carries was expected to abort", &
            required_stderr="""n"" is already a column of the target table; pass force=.true. to replace it")
    end subroutine test_table_group_add_size_name_taken_aborts
    !
    !> See `scenario_table_group_add_apply_target_is_source` (test/error_scenarios.f90).
    subroutine test_table_group_add_apply_target_is_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_apply_target_is_source", &
            expect_abort=.true., &
            failure_message="add_apply onto the grouping's own table was expected to abort", &
            required_stderr="parquet_grouping: add_apply: the target table is the one this grouping was built from")
    end subroutine test_table_group_add_apply_target_is_source_aborts
    !
    !> See `scenario_table_group_add_apply_no_name` (test/error_scenarios.f90).
    subroutine test_table_group_add_apply_no_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_apply_no_name", &
            expect_abort=.true., &
            failure_message="add_apply with an as= that names nothing was expected to abort", &
            required_stderr="parquet_grouping: add_apply: as= names no column; give one name per result")
    end subroutine test_table_group_add_apply_no_name_aborts
    !
    !> See `scenario_table_group_add_apply_duplicate_name` (test/error_scenarios.f90).
    subroutine test_table_group_add_apply_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_apply_duplicate_name", &
            expect_abort=.true., &
            failure_message="add_apply naming one column twice in as= was expected to abort", &
            required_stderr="parquet_grouping: add_apply: as= names ""m"" twice")
    end subroutine test_table_group_add_apply_duplicate_name_aborts
    !
    !> See `scenario_table_group_add_apply_name_taken` (test/error_scenarios.f90).
    subroutine test_table_group_add_apply_name_taken_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_apply_name_taken", &
            expect_abort=.true., &
            failure_message="add_apply whose last name the target already carries was expected to abort", &
            required_stderr="parquet_grouping: add_apply: ""n"" is already a column of the target table")
    end subroutine test_table_group_add_apply_name_taken_aborts
    !
    !> See `scenario_table_group_stale_add_apply` (test/error_scenarios.f90).
    subroutine test_table_group_stale_add_apply_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_add_apply", expect_abort=.true., &
            failure_message="add_apply on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: add_apply: this table has changed structurally")
    end subroutine test_table_group_stale_add_apply_aborts
    !
    !> See `scenario_table_group_add_apply_threads_zero` (test/error_scenarios.f90).
    subroutine test_table_group_add_apply_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_add_apply_threads_zero", &
            expect_abort=.true., &
            failure_message="add_apply with threads=0 was expected to abort", &
            required_stderr="parquet_grouping: add_apply: threads= must be at least 1, got 0")
    end subroutine test_table_group_add_apply_threads_zero_aborts
    !
    !> See `scenario_table_group_rows_g32_out_of_range` (test/error_scenarios.f90).
    subroutine test_table_group_rows_g32_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_rows_g32_out_of_range", &
            expect_abort=.true., &
            failure_message="an int32 group number past the last group was expected to abort", &
            required_stderr="parquet_grouping: rows: group 7 is out of range; this grouping has 3 groups")
    end subroutine test_table_group_rows_g32_out_of_range_aborts
    !
    !> See `scenario_table_group_gather_g32_short_buffer` (test/error_scenarios.f90).
    subroutine test_table_group_gather_g32_short_buffer_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_gather_g32_short_buffer", &
            expect_abort=.true., &
            failure_message="a short buffer reached through an int32 g was expected to abort, not truncate", &
            required_stderr="buf is 1 long and group 1 has 2 rows")
    end subroutine test_table_group_gather_g32_short_buffer_aborts
    !
    !> See `scenario_table_group_stale_apply` (test/error_scenarios.f90).
    subroutine test_table_group_stale_apply_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_apply", expect_abort=.true., &
            failure_message="apply on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: apply: this table has changed structurally")
    end subroutine test_table_group_stale_apply_aborts
    !
    !> See `scenario_table_group_stale_apply_object` (test/error_scenarios.f90).
    subroutine test_table_group_stale_apply_object_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_apply_object", expect_abort=.true., &
            failure_message="apply's object form on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: apply: this table has changed structurally")
    end subroutine test_table_group_stale_apply_object_aborts
    !
    !> See `scenario_table_group_apply_nout` (test/error_scenarios.f90).
    subroutine test_table_group_apply_nout_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_apply_nout", expect_abort=.true., &
            failure_message="apply with nout=0 was expected to abort", &
            required_stderr="parquet_grouping: apply: nout = 0 is not positive")
    end subroutine test_table_group_apply_nout_aborts
    !
    !> See `scenario_table_group_apply_threads_zero` (test/error_scenarios.f90).
    subroutine test_table_group_apply_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_apply_threads_zero", expect_abort=.true., &
            failure_message="apply with threads=0 was expected to abort", &
            required_stderr="parquet_grouping: apply: threads= must be at least 1, got 0")
    end subroutine test_table_group_apply_threads_zero_aborts
    !
    !> See `scenario_table_group_stale_agg` (test/error_scenarios.f90).
    subroutine test_table_group_stale_agg_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_agg", expect_abort=.true., &
            failure_message="agg on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: agg: this table has changed structurally")
    end subroutine test_table_group_stale_agg_aborts
    !
    !> See `scenario_table_group_stale_nunique` (test/error_scenarios.f90).
    subroutine test_table_group_stale_nunique_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_nunique", expect_abort=.true., &
            failure_message="nunique on a stale grouping was expected to abort", &
            required_stderr="parquet_grouping: nunique: this table has changed structurally")
    end subroutine test_table_group_stale_nunique_aborts
    !
    !> See `scenario_table_group_agg_unknown_token` (test/error_scenarios.f90).
    subroutine test_table_group_agg_unknown_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_unknown_token", expect_abort=.true., &
            failure_message="an unknown statistic token was expected to abort", &
            required_stderr="unknown statistic 'medain'; the tokens are: size, count, sum, mean")
    end subroutine test_table_group_agg_unknown_token_aborts
    !
    !> See `scenario_table_group_agg_quantile_needs_q` (test/error_scenarios.f90).
    subroutine test_table_group_agg_quantile_needs_q_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_quantile_needs_q", expect_abort=.true., &
            failure_message="quantile without q= was expected to abort", &
            required_stderr='"quantile" needs q=')
    end subroutine test_table_group_agg_quantile_needs_q_aborts
    !
    !> See `scenario_table_group_agg_option_refused` (test/error_scenarios.f90).
    subroutine test_table_group_agg_option_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_option_refused", expect_abort=.true., &
            failure_message="q= on mean was expected to abort", &
            required_stderr='q= belongs to "quantile" alone')
    end subroutine test_table_group_agg_option_refused_aborts
    !
    !> See `scenario_table_group_agg_int_real_column` (test/error_scenarios.f90).
    subroutine test_table_group_agg_int_real_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_int_real_column", expect_abort=.true., &
            failure_message="the exact family on a real column was expected to abort", &
            required_stderr="the exact int64 family takes an integer or logical column")
    end subroutine test_table_group_agg_int_real_column_aborts
    !
    !> See `scenario_table_group_agg_int_sum_overflow` (test/error_scenarios.f90).
    subroutine test_table_group_agg_int_sum_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_int_sum_overflow", expect_abort=.true., &
            failure_message="an overflowing exact sum was expected to abort", &
            required_stderr="the int64 sum of 'h' over group 2 overflows")
    end subroutine test_table_group_agg_int_sum_overflow_aborts
    !
    !> See `scenario_table_group_agg_int_all_null` (test/error_scenarios.f90).
    subroutine test_table_group_agg_int_all_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_int_all_null", expect_abort=.true., &
            failure_message="the exact min of an all-null group was expected to abort", &
            required_stderr="group 1 has no non-null value of 'n', so its exact min does not exist")
    end subroutine test_table_group_agg_int_all_null_aborts
    !
    !> See `scenario_table_group_agg_string_column` (test/error_scenarios.f90).
    subroutine test_table_group_agg_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_string_column", expect_abort=.true., &
            failure_message="a string column under a value statistic was expected to abort", &
            required_stderr="column has no numeric statistics; %agg takes a scalar numeric or logical column")
    end subroutine test_table_group_agg_string_column_aborts
    !
    !> See `scenario_table_group_agg_weights_length` (test/error_scenarios.f90).
    subroutine test_table_group_agg_weights_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_weights_length", expect_abort=.true., &
            failure_message="weights of the wrong length were expected to abort", &
            required_stderr="weights has 3 entries but the table has 5 rows")
    end subroutine test_table_group_agg_weights_length_aborts
    !
    !> See `scenario_table_group_agg_negative_weight` (test/error_scenarios.f90).
    subroutine test_table_group_agg_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_negative_weight", expect_abort=.true., &
            failure_message="a negative weight was expected to abort", &
            required_stderr="the weight of row 2 is negative")
    end subroutine test_table_group_agg_negative_weight_aborts
    !
    !> See `scenario_table_group_agg_both_weights` (test/error_scenarios.f90).
    subroutine test_table_group_agg_both_weights_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_both_weights", expect_abort=.true., &
            failure_message="both weight forms together were expected to abort", &
            required_stderr="weights= and weight_column= were both given")
    end subroutine test_table_group_agg_both_weights_aborts
    !
    !> See `scenario_table_group_agg_weight_column_string` (test/error_scenarios.f90).
    subroutine test_table_group_agg_weight_column_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_weight_column_string", &
            expect_abort=.true., failure_message="a string weight column was expected to abort", &
            required_stderr="weight_column= names a PK_STRING column")
    end subroutine test_table_group_agg_weight_column_string_aborts
    !
    !> See `scenario_table_group_agg_weight_column_vector` (test/error_scenarios.f90).
    subroutine test_table_group_agg_weight_column_vector_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_weight_column_vector", &
            expect_abort=.true., failure_message="a vector weight column was expected to abort", &
            required_stderr="weight_column= names a")
    end subroutine test_table_group_agg_weight_column_vector_aborts
    !
    !> See `scenario_table_group_agg_weights_ignored` (test/error_scenarios.f90).
    subroutine test_table_group_agg_weights_ignored_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_weights_ignored", expect_abort=.true., &
            failure_message="weights on size were expected to abort", &
            required_stderr='weights have no effect on "size"')
    end subroutine test_table_group_agg_weights_ignored_aborts
    !
    !> See `scenario_table_group_nunique_vector_column` (test/error_scenarios.f90).
    subroutine test_table_group_nunique_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_nunique_vector_column", expect_abort=.true., &
            failure_message="nunique of a vector column was expected to abort", &
            required_stderr="cannot be a column %nunique counts")
    end subroutine test_table_group_nunique_vector_column_aborts
    !
    !> See `scenario_table_group_stale_broadcast` (test/error_scenarios.f90).
    subroutine test_table_group_stale_broadcast_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_broadcast", expect_abort=.true., &
            failure_message="broadcast on a stale grouping was expected to abort", &
            required_stderr="has changed structurally since the grouping was built")
    end subroutine test_table_group_stale_broadcast_aborts
    !
    !> See `scenario_table_group_stale_gather` (test/error_scenarios.f90).
    subroutine test_table_group_stale_gather_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_stale_gather", expect_abort=.true., &
            failure_message="gather on a stale grouping was expected to abort", &
            required_stderr="has changed structurally since the grouping was built")
    end subroutine test_table_group_stale_gather_aborts
    !
    !> See `scenario_table_group_gather_short_buffer` (test/error_scenarios.f90).
    subroutine test_table_group_gather_short_buffer_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_gather_short_buffer", expect_abort=.true., &
            failure_message="a buffer shorter than the group was expected to abort, not truncate", &
            required_stderr="buf is 1 long and group 1 has 2 rows")
    end subroutine test_table_group_gather_short_buffer_aborts
    !
    !> See `scenario_table_group_gather_short_valid` (test/error_scenarios.f90).
    subroutine test_table_group_gather_short_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_gather_short_valid", expect_abort=.true., &
            failure_message="an is_valid shorter than the group was expected to abort", &
            required_stderr="is_valid is 1 long and group 1 has 2 rows")
    end subroutine test_table_group_gather_short_valid_aborts
    !
    !> See `scenario_table_group_gather_out_of_range` (test/error_scenarios.f90).
    subroutine test_table_group_gather_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_gather_out_of_range", expect_abort=.true., &
            failure_message="gather of a group past the last was expected to abort", &
            required_stderr="group 4 is out of range")
    end subroutine test_table_group_gather_out_of_range_aborts
    !
    !> See `scenario_table_group_gather_kind` (test/error_scenarios.f90).
    subroutine test_table_group_gather_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_gather_kind", expect_abort=.true., &
            failure_message="a real64 column into an int32 buffer was expected to abort", &
            required_stderr="cannot be copied into an integer(int32) buffer")
    end subroutine test_table_group_gather_kind_aborts
    !
    !> See `scenario_table_group_broadcast_length` (test/error_scenarios.f90).
    subroutine test_table_group_broadcast_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_broadcast_length", expect_abort=.true., &
            failure_message="a per_group of the wrong length was expected to abort", &
            required_stderr="per_group has 2 entries and this grouping has 3 groups")
    end subroutine test_table_group_broadcast_length_aborts
    !
    !> See `scenario_table_group_blank_key` (test/error_scenarios.f90).
    subroutine test_table_group_blank_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_blank_key", expect_abort=.true., &
            failure_message="a blank key name was expected to abort", required_stderr="a key name is blank")
    end subroutine test_table_group_blank_key_aborts
    !
    !> See `scenario_table_group_direction_word` (test/error_scenarios.f90).
    subroutine test_table_group_direction_word_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_direction_word", expect_abort=.true., &
            failure_message="a trailing direction word was expected to abort", &
            required_stderr="reads as a sort key")
    end subroutine test_table_group_direction_word_aborts
    !
    !> See `scenario_table_group_query_unknown_column` (test/error_scenarios.f90).
    subroutine test_table_group_query_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_query_unknown_column", &
            expect_abort=.true., failure_message="an unknown query column was expected to abort", &
            required_stderr="no column of this name")
    end subroutine test_table_group_query_unknown_column_aborts
    !
    !> See `scenario_table_group_query_unsupported_column` (test/error_scenarios.f90).
    subroutine test_table_group_query_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_query_unsupported_column", &
            expect_abort=.true., failure_message="an unsupported query column was expected to abort", &
            required_stderr="grouping count")
    end subroutine test_table_group_query_unsupported_column_aborts
    !
    !> See `scenario_table_group_agg_exact_unknown_token` (test/error_scenarios.f90).
    subroutine test_table_group_agg_exact_unknown_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_exact_unknown_token", &
            expect_abort=.true., failure_message="a real64-only token in the exact form was expected to abort", &
            required_stderr="is not a statistic of the exact int64 family")
    end subroutine test_table_group_agg_exact_unknown_token_aborts
    !
    !> See `scenario_table_group_agg_method_refused` (test/error_scenarios.f90).
    subroutine test_table_group_agg_method_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_method_refused", expect_abort=.true., &
            failure_message="method= on the mean was expected to abort", &
            required_stderr='method= belongs to "median", "quantile" and "iqr"')
    end subroutine test_table_group_agg_method_refused_aborts
    !
    !> See `scenario_table_group_agg_ddof_refused` (test/error_scenarios.f90).
    subroutine test_table_group_agg_ddof_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_ddof_refused", expect_abort=.true., &
            failure_message="ddof= on the mean was expected to abort", &
            required_stderr='ddof= belongs to "var", "std" and "sem"')
    end subroutine test_table_group_agg_ddof_refused_aborts
    !
    !> See `scenario_table_group_agg_scale_refused` (test/error_scenarios.f90).
    subroutine test_table_group_agg_scale_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_scale_refused", expect_abort=.true., &
            failure_message="scale= on the mean was expected to abort", &
            required_stderr='scale= belongs to "mad" alone')
    end subroutine test_table_group_agg_scale_refused_aborts
    !
    !> See `scenario_table_group_agg_nan_weight` (test/error_scenarios.f90).
    subroutine test_table_group_agg_nan_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_nan_weight", expect_abort=.true., &
            failure_message="a NaN weight was expected to abort", required_stderr="is NaN; weights must be")
    end subroutine test_table_group_agg_nan_weight_aborts
    !
    !> See `scenario_table_group_agg_infinite_weight` (test/error_scenarios.f90).
    subroutine test_table_group_agg_infinite_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "table_group_agg_infinite_weight", expect_abort=.true., &
            failure_message="an infinite weight was expected to abort", &
            required_stderr="is infinite; weights must be")
    end subroutine test_table_group_agg_infinite_weight_aborts
    !
    !> See `scenario_filter_temporal_set_mismatch` (test/error_scenarios.f90).
    subroutine test_filter_temporal_set_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "filter_temporal_set_mismatch", expect_abort=.true., &
            failure_message="a date set against a timestamp column was expected to abort", &
            required_stderr="the 'date' set in the filter clause on column 'ts' cannot be compared")
    end subroutine test_filter_temporal_set_mismatch_aborts
    !
    !> See `scenario_filter_time_set_on_int` (test/error_scenarios.f90).
    subroutine test_filter_time_set_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "filter_time_set_on_int", expect_abort=.true., &
            failure_message="a time set against an int32 column was expected to abort", &
            required_stderr="the 'time' set in the filter clause on column 'v' cannot be compared")
    end subroutine test_filter_time_set_on_int_aborts
    !
    !> See `scenario_filter_timestamp_set_on_int` (test/error_scenarios.f90).
    subroutine test_filter_timestamp_set_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "filter_timestamp_set_on_int", expect_abort=.true., &
            failure_message="a timestamp set against an int32 column was expected to abort", &
            required_stderr="the 'timestamp' set in the filter clause on column 'v' cannot be compared")
    end subroutine test_filter_timestamp_set_on_int_aborts
    !
    !> See `scenario_filter_temporal_literal_list` (test/error_scenarios.f90).
    subroutine test_filter_temporal_literal_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "filter_temporal_literal_list", expect_abort=.true., &
            failure_message="a literal list on a date column was expected to abort", &
            required_stderr="is not supported on a date column -- bind the members as an array of parquet_date")
    end subroutine test_filter_temporal_literal_list_aborts

    !> last= has its own negative guard, separate from first='s.
    subroutine test_print_rows_negative_last_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_negative_last", expect_abort=.true., &
            failure_message="%print_rows(last=-1) was expected to abort", &
            required_stderr="got last=-1")
    end subroutine test_print_rows_negative_last_aborts

    !> A column this library cannot read has no values to carry across a join.
    subroutine test_join_columns_unsupported_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_columns_unsupported", expect_abort=.true., &
            failure_message="%join(columns=) naming an unreadable column was expected to abort", &
            required_stderr="nothing to carry across")
    end subroutine test_join_columns_unsupported_aborts

    !> MAML requires a table: value, so a blank name= is refused where it is given.
    subroutine test_derive_schema_blank_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "derive_schema_blank_name", expect_abort=.true., &
            failure_message="parquet_derive_schema(name='') was expected to abort", &
            required_stderr="name= must not be blank")
    end subroutine test_derive_schema_blank_name_aborts

    !> A schema cannot be derived from columns nothing has read.
    subroutine test_open_writer_like_nothing_resident_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "open_writer_like_nothing_resident", &
            expect_abort=.true., &
            failure_message="parquet_open_writer_like on an unread table was expected to abort", &
            required_stderr="no resident column")
    end subroutine test_open_writer_like_nothing_resident_aborts

    !> A sink schema naming a column that holds no values is refused at open.
    subroutine test_sink_schema_names_an_unreadable_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sink_schema_names_an_unreadable_column", &
            expect_abort=.true., &
            failure_message="a sink schema naming an unreadable column was expected to abort", &
            required_stderr="holds no values")
    end subroutine test_sink_schema_names_an_unreadable_column_aborts

    !> An overflowing int64 group sum is refused rather than wrapped.
    subroutine test_agg_int64_sum_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "agg_int64_sum_overflow", expect_abort=.true., &
            failure_message="an overflowing int64 group sum was expected to abort", &
            required_stderr="overflows")
    end subroutine test_agg_int64_sum_overflow_aborts

    !> With no unit=, %print_rows writes where message_stream says, which is process-global.
    subroutine test_print_rows_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "print_rows_follows_message_stream", &
            "streammarker", expect_on="stderr", &
            failure_message="message_stream=stderr should move %print_rows's output to stderr")
    end subroutine test_print_rows_follows_message_stream

    !> The same rule for %print_stat, which used to be pinned to standard output.
    !!
    !! The mutation this must catch is a restored `u = output_unit` default in table_print_stat:
    !! the listing then appears on the stream the caller did not choose, at default settings looks
    !! exactly right, and nothing else in the suite notices.
    subroutine test_print_stat_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "print_stat_follows_message_stream", &
            "statmarker", expect_on="stderr", &
            failure_message="message_stream=stderr should move %print_stat's listing to stderr")
    end subroutine test_print_stat_follows_message_stream

    !> `parquet_string_column%print` and `parquet_string%print` together.
    !!
    !! The marker is the stored string, which both printers emit, so a change applied to only one
    !! of the two leaves it on stdout and trips the "must NOT also appear on the other stream" half.
    subroutine test_string_print_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "string_print_follows_message_stream", &
            "strmarker", expect_on="stderr", &
            failure_message="message_stream=stderr should move both parquet_strings printers to stderr")
    end subroutine test_string_print_follows_message_stream

    !> `parquet_print_settings` is exempt from `verbosity`, not from `message_stream`.
    !!
    !! Run at "silent", so the test asserts both halves of that sentence at once: the dump appears
    !! (the verbosity exemption) and it appears on stderr (the routing that is not exempt).
    subroutine test_print_settings_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "print_settings_follows_message_stream", &
            "parquet-fortran settings", expect_on="stderr", &
            failure_message="parquet_print_settings must print at silent, on the message_stream unit")
    end subroutine test_print_settings_follows_message_stream

    !> `%print_schema_info` with neither `unit=` nor `filename=` is a call form that used to abort.
    !!
    !! Two things at once: the scenario exits 0 (so the `error stop` really is gone) and the
    !! listing lands on the stream. A restored abort fails the exit check inside
    !! check_scenario_streams before either stream is examined.
    subroutine test_print_schema_info_default_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "print_schema_info_default_stream", &
            "schemamarker", expect_on="stderr", &
            failure_message="%print_schema_info with no destination should write to the message_stream unit")
    end subroutine test_print_schema_info_default_stream

    !> The context lines a failing `parquet_close_writer` prints follow `message_stream`.
    !!
    !! These were pinned to standard output. The scenario aborts, which is the path being tested:
    !! the context has to reach the same stream as everything else the library said, so a caller
    !! who routed output to stderr does not have to read two streams to diagnose one failure.
    subroutine test_error_context_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "error_context_follows_message_stream", &
            "scenario_ctx_stream", expect_on="stderr", &
            failure_message="a failing close's context lines should follow message_stream")
    end subroutine test_error_context_follows_message_stream

    !> The reader report is printed by C++, from the mirrored copy of the setting.
    !!
    !! This is the one destination assertion that cannot be satisfied by a Fortran-side change:
    !! the report has no `unit=` and every line of it is a `std::fprintf`. A mutation that restores
    !! `stdout` at any single line of the report leaves that line on the other stream and trips the
    !! "must NOT also appear" half, which is why the marker is the filename the report's own
    !! "file:" line carries.
    subroutine test_reader_print_stat_follows_message_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "reader_print_stat_follows_message_stream", &
            "fixtures/element_nulls.parquet", expect_on="stderr", &
            failure_message="the C++ reader report should follow the mirrored message_stream")
    end subroutine test_reader_print_stat_follows_message_stream

    !> `verbosity="silent"` set AFTER the open still silences the reader's print_stat report.
    !!
    !! The mirror C++ reads is refreshed when a reader is opened, so before the push at close time
    !! this exact sequence printed the report in full. Asserted by absence on BOTH streams, since a
    !! report that merely moved would be just as wrong.
    subroutine test_close_reader_print_stat_silenced_late(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: on_out, on_err

        call run_error_scenario("close_reader_print_stat_silenced_late", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "the scenario should close the reader cleanly")
        if (allocated(error)) return
        call file_contains(out_file, "=== parquet_reader stats ===", on_out)
        call file_contains(err_file, "=== parquet_reader stats ===", on_err)
        call check(error, .not. on_out .and. .not. on_err, &
            'verbosity="silent" set after the open must still silence the C++ reader report')
        if (allocated(error)) return
        ! The control: without it, a scenario that failed to run at all would satisfy the absence
        ! check above for the wrong reason.
        call file_contains(out_file, "control: the reader closed", on_out)
        call check(error, on_out, "the scenario did not reach its control line")
    end subroutine test_close_reader_print_stat_silenced_late

    !> `%print` on an unbound `parquet_string` aborts at `verbosity="silent"` as it does at normal.
    !!
    !! The mutation: put the suppression test back above `check_handle`. The call then returns
    !! quietly, the scenario exits 0 instead of aborting, and a caller's programming error is
    !! reported or not depending on an output setting.
    subroutine test_string_print_unbound_handle_silent_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "string_print_unbound_handle_silent", expect_abort=.true., &
            failure_message="an unbound parquet_string handle must be reported even at verbosity=silent")
    end subroutine test_string_print_unbound_handle_silent_aborts

    !> The same guard, reached through the matrix verbs' own prepare step.
    subroutine test_get_matrix_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_matrix_unsupported_column", &
            expect_abort=.true., &
            failure_message="%get_matrix on a column that holds no values was expected to abort", &
            required_stderr="m_intkey")
    end subroutine test_get_matrix_unsupported_column_aborts

    !> A long list of missing names is capped and ellipsised rather than printed whole.
    subroutine test_keep_columns_many_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "keep_columns_many_missing", &
            expect_abort=.true., &
            failure_message="%keep_columns naming twenty absent columns was expected to abort", &
            required_stderr="absent_col1, absent_col2")
    end subroutine test_keep_columns_many_missing_aborts

    !> The refusal names the key class the caller asked for, not the column's.
    subroutine test_table_index_date_key_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_index_date_key_on_int", &
            expect_abort=.true., &
            failure_message="a parquet_date key on an integer index was expected to abort", &
            required_stderr="a parquet_date")
    end subroutine test_table_index_date_key_on_int_aborts

    !> And the time class has its own word.
    subroutine test_table_index_time_key_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_index_time_key_on_int", &
            expect_abort=.true., &
            failure_message="a parquet_time key on an integer index was expected to abort", &
            required_stderr="a parquet_time")
    end subroutine test_table_index_time_key_on_int_aborts

    !> An infinite centre component is a different guard from the NaN one: `max` propagates an
    !! infinity where it does not propagate a NaN, so the NaN scenario can never reach this arm.
    subroutine test_healpix_disc_vector_infinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_vector_infinite", &
            expect_abort=.true., &
            failure_message="an infinite disc centre component was expected to abort", &
            required_stderr="is not finite")
    end subroutine test_healpix_disc_vector_infinite_aborts

    !
    ! ---- pf_integrate abort paths ------------------------------------------------------------
    !
    !> Every one of `pf_integrate`'s caller-contract refusals, asserted by the exact text the
    !> guide page's table publishes. Each names the scenario that provokes it; see
    !> `test/error_scenarios.f90` for why each call is refused rather than answered.
    subroutine test_integrate_negative_rtol_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_negative_rtol", expect_abort=.true., &
            failure_message="a negative rtol was expected to abort", &
            required_stderr="rtol must be a finite, non-negative number")
    end subroutine test_integrate_negative_rtol_aborts
    !
    subroutine test_integrate_nan_rtol_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_nan_rtol", expect_abort=.true., &
            failure_message="a NaN rtol was expected to abort", &
            required_stderr="rtol must be a finite, non-negative number")
    end subroutine test_integrate_nan_rtol_aborts
    !
    subroutine test_integrate_negative_atol_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_negative_atol", expect_abort=.true., &
            failure_message="a negative atol was expected to abort", &
            required_stderr="atol must be a finite, non-negative number")
    end subroutine test_integrate_negative_atol_aborts
    !
    subroutine test_integrate_zero_tolerances_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_zero_tolerances", expect_abort=.true., &
            failure_message="two zero tolerances were expected to abort", &
            required_stderr="at least one of rtol and atol must be positive")
    end subroutine test_integrate_zero_tolerances_aborts
    !
    subroutine test_integrate_rtol_below_floor_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_rtol_below_floor", expect_abort=.true., &
            failure_message="an rtol below 50*epsilon with no atol was expected to abort", &
            required_stderr="rtol below 50*epsilon needs a positive atol")
    end subroutine test_integrate_rtol_below_floor_aborts
    !
    subroutine test_integrate_bad_max_neval_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_bad_max_neval", expect_abort=.true., &
            failure_message="a zero max_neval was expected to abort", &
            required_stderr="max_neval must be positive")
    end subroutine test_integrate_bad_max_neval_aborts
    !
    subroutine test_integrate_huge_max_neval_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_huge_max_neval", expect_abort=.true., &
            failure_message="a max_neval above the ceiling was expected to abort", &
            required_stderr="max_neval must not exceed huge(1)/42")
    end subroutine test_integrate_huge_max_neval_aborts
    !
    subroutine test_integrate_nan_bound_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_nan_bound", expect_abort=.true., &
            failure_message="a NaN bound was expected to abort", &
            required_stderr="integration bounds must not be NaN")
    end subroutine test_integrate_nan_bound_aborts
    !
    subroutine test_integrate_reversed_bounds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_reversed_bounds", expect_abort=.true., &
            failure_message="reversed bounds were expected to abort", &
            required_stderr="lower bound must not exceed the upper bound")
    end subroutine test_integrate_reversed_bounds_aborts
    !
    subroutine test_integrate_bad_max_panels_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_bad_max_panels", expect_abort=.true., &
            failure_message="a max_panels of zero was expected to abort", &
            required_stderr="max_panels must be positive")
    end subroutine test_integrate_bad_max_panels_aborts
    !
    subroutine test_integrate_max_panels_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_max_panels_finite", expect_abort=.true., &
            failure_message="max_panels on a finite range was expected to abort", &
            required_stderr="max_panels applies only to an infinite range")
    end subroutine test_integrate_max_panels_finite_aborts
    !
    subroutine test_integrate_same_infinity_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_same_infinity", expect_abort=.true., &
            failure_message="both bounds the same infinity was expected to abort", &
            required_stderr="bounds must not both be the same infinity")
    end subroutine test_integrate_same_infinity_aborts
    !
    subroutine test_integrate_log_base_infinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_log_base_infinite", expect_abort=.true., &
            failure_message="log_base on an infinite range was expected to abort", &
            required_stderr="log_base applies only to a finite range")
    end subroutine test_integrate_log_base_infinite_aborts
    !
    subroutine test_integrate_log_base_nonpositive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_log_base_nonpositive", expect_abort=.true., &
            failure_message="log_base from a non-positive bound was expected to abort", &
            required_stderr="lower bound must be positive when integrating in log x")
    end subroutine test_integrate_log_base_nonpositive_aborts
    !
    subroutine test_integrate_breakpoints_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_breakpoints_nan", expect_abort=.true., &
            failure_message="a NaN breakpoint was expected to abort", &
            required_stderr="breakpoints must be finite")
    end subroutine test_integrate_breakpoints_nan_aborts
    !
    subroutine test_integrate_breakpoints_outside_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_breakpoints_outside", expect_abort=.true., &
            failure_message="a breakpoint outside the range was expected to abort", &
            required_stderr="breakpoints must lie strictly inside the range")
    end subroutine test_integrate_breakpoints_outside_aborts
    !
    subroutine test_integrate_breakpoints_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_breakpoints_duplicate", expect_abort=.true., &
            failure_message="two equal breakpoints were expected to abort", &
            required_stderr="breakpoints must be distinct")
    end subroutine test_integrate_breakpoints_duplicate_aborts
    !
    subroutine test_integrate_context_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_context_reported", expect_abort=.true., &
            failure_message="reversed bounds with a context were expected to abort", &
            required_stderr="context: my_call_site")
    end subroutine test_integrate_context_reported_aborts
    !
    !
    subroutine test_optimize_size_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_size_zero", expect_abort=.true., &
            failure_message="a zero-length start point was expected to error stop", &
            required_stderr="at least one variable is required")
    end subroutine test_optimize_size_zero_aborts
    !
    subroutine test_optimize_budget_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_budget_zero", expect_abort=.true., &
            failure_message="a zero max_neval was expected to error stop", &
            required_stderr="max_neval must be positive")
    end subroutine test_optimize_budget_zero_aborts
    !
    subroutine test_optimize_budget_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_budget_ceiling", expect_abort=.true., &
            failure_message="a max_neval above huge(1)/2 was expected to error stop", &
            required_stderr="max_neval must not exceed huge(1)/2")
    end subroutine test_optimize_budget_ceiling_aborts
    !
    subroutine test_optimize_tolerance_nonfinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_tolerance_nonfinite", expect_abort=.true., &
            failure_message="a NaN rtol was expected to error stop", &
            required_stderr="rtol must be a finite, non-negative number")
    end subroutine test_optimize_tolerance_nonfinite_aborts
    !
    subroutine test_optimize_scalar_bad_bracket_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_scalar_bad_bracket", expect_abort=.true., &
            failure_message="a reversed bracket was expected to error stop", &
            required_stderr="the bracket must satisfy a < b with finite ends")
    end subroutine test_optimize_scalar_bad_bracket_aborts
    !
    subroutine test_optimize_scalar_bracket_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_scalar_bracket_width", expect_abort=.true., &
            failure_message="a bracket whose width overflows was expected to error stop", &
            required_stderr="the bracket width must be finite")
    end subroutine test_optimize_scalar_bracket_width_aborts
    !
    subroutine test_optimize_scalar_nonfinite_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_scalar_nonfinite_value", expect_abort=.true., &
            failure_message="a NaN objective value was expected to error stop", &
            required_stderr="the objective returned a non-finite value")
    end subroutine test_optimize_scalar_nonfinite_value_aborts
    !
    subroutine test_optimize_scalar_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_scalar_constraints_not_honoured", expect_abort=.true., &
            failure_message="a constrained objective in Brent was expected to error stop", &
            required_stderr="this engine does not honour nonlinear constraints; use pf_minimize_cobyla")
    end subroutine test_optimize_scalar_constraints_not_honoured_aborts
    !
    subroutine test_optimize_simplex_step_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_step_zero", expect_abort=.true., &
            failure_message="a zero step element was expected to error stop", &
            required_stderr="step must not contain zero or NaN")
    end subroutine test_optimize_simplex_step_zero_aborts
    !
    subroutine test_optimize_simplex_step_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_step_size", expect_abort=.true., &
            failure_message="a step of the wrong size was expected to error stop", &
            required_stderr="step and x must have the same size")
    end subroutine test_optimize_simplex_step_size_aborts
    !
    subroutine test_optimize_simplex_no_tolerance_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_no_tolerance", expect_abort=.true., &
            failure_message="two zero tolerances were expected to error stop", &
            required_stderr="at least one of rtol and atol must be positive")
    end subroutine test_optimize_simplex_no_tolerance_aborts
    !
    subroutine test_optimize_simplex_nan_start_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_nan_start", expect_abort=.true., &
            failure_message="a NaN start point was expected to error stop", &
            required_stderr="the start point must not contain NaN")
    end subroutine test_optimize_simplex_nan_start_aborts
    !
    subroutine test_optimize_simplex_nonfinite_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_nonfinite_value", expect_abort=.true., &
            failure_message="a mid-run NaN value was expected to error stop", &
            required_stderr="the objective returned a non-finite value")
    end subroutine test_optimize_simplex_nonfinite_value_aborts
    !
    subroutine test_optimize_simplex_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_constraints_not_honoured", expect_abort=.true., &
            failure_message="a constrained objective in the simplex was expected to error stop", &
            required_stderr="this engine does not honour nonlinear constraints; use pf_minimize_cobyla")
    end subroutine test_optimize_simplex_constraints_not_honoured_aborts
    !
    subroutine test_optimize_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_threads_zero", expect_abort=.true., &
            failure_message="a zero threads= was expected to error stop", &
            required_stderr="threads must be positive")
    end subroutine test_optimize_threads_zero_aborts
    !
    subroutine test_optimize_de_bounds_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_bounds_size", expect_abort=.true., &
            failure_message="a box of the wrong size was expected to error stop", &
            required_stderr="lower, upper and x must have the same size")
    end subroutine test_optimize_de_bounds_size_aborts
    !
    subroutine test_optimize_de_bounds_order_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_bounds_order", expect_abort=.true., &
            failure_message="a box with no interior was expected to error stop", &
            required_stderr="every lower bound must be below its upper bound")
    end subroutine test_optimize_de_bounds_order_aborts
    !
    subroutine test_optimize_de_bounds_nonfinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_bounds_nonfinite", expect_abort=.true., &
            failure_message="an infinite bound was expected to error stop", &
            required_stderr="bounds must be finite")
    end subroutine test_optimize_de_bounds_nonfinite_aborts
    !
    subroutine test_optimize_de_box_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_box_width", expect_abort=.true., &
            failure_message="a box whose width overflows was expected to error stop", &
            required_stderr="every box width must be finite")
    end subroutine test_optimize_de_box_width_aborts
    !
    subroutine test_optimize_de_np_small_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_np_small", expect_abort=.true., &
            failure_message="a population of three was expected to error stop", &
            required_stderr="np must be at least 4")
    end subroutine test_optimize_de_np_small_aborts
    !
    subroutine test_optimize_de_f_weight_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_f_weight_range", expect_abort=.true., &
            failure_message="a zero differential weight was expected to error stop", &
            required_stderr="f_weight must be in (0, 2]")
    end subroutine test_optimize_de_f_weight_range_aborts
    !
    subroutine test_optimize_de_cr_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_cr_range", expect_abort=.true., &
            failure_message="a crossover probability above one was expected to error stop", &
            required_stderr="cr must be in [0, 1]")
    end subroutine test_optimize_de_cr_range_aborts
    !
    subroutine test_optimize_de_max_gen_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_max_gen_zero", expect_abort=.true., &
            failure_message="a zero generation budget was expected to error stop", &
            required_stderr="max_gen must be positive")
    end subroutine test_optimize_de_max_gen_zero_aborts
    !
    subroutine test_optimize_de_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_constraints_not_honoured", expect_abort=.true., &
            failure_message="a constrained objective in DE was expected to error stop", &
            required_stderr="pf_minimize_de: this engine does not honour nonlinear constraints")
    end subroutine test_optimize_de_constraints_not_honoured_aborts
    !
    subroutine test_optimize_de_no_tolerance_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_de_no_tolerance", expect_abort=.true., &
            failure_message="rtol and atol both zero were expected to error stop", &
            required_stderr="at least one of rtol and atol must be positive")
    end subroutine test_optimize_de_no_tolerance_aborts
    !
    subroutine test_optimize_context_is_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_context_is_reported", expect_abort=.true., &
            failure_message="a degenerate box with a context was expected to error stop", &
            required_stderr="(context: sweeping the grid)")
    end subroutine test_optimize_context_is_reported_aborts
    !
    subroutine test_optimize_context_is_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_context_is_capped", expect_abort=.true., &
            failure_message="a 150-character context was expected to abort with a capped message", &
            required_stderr="abcdefghij...")
    end subroutine test_optimize_context_is_capped_aborts
    !
    subroutine test_optimize_multistart_nstart_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_multistart_nstart_zero", expect_abort=.true., &
            failure_message="zero starts were expected to error stop", &
            required_stderr="nstart must be positive")
    end subroutine test_optimize_multistart_nstart_zero_aborts
    !
    subroutine test_optimize_multistart_merge_tol_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_multistart_merge_tol_negative", expect_abort=.true., &
            failure_message="a negative merge_tol was expected to error stop", &
            required_stderr="merge_tol must be a finite, non-negative number")
    end subroutine test_optimize_multistart_merge_tol_negative_aborts
    !
    ! The BINDING's name is asserted, type-qualified, because the two solver objects give the same
    ! message and only the name says which refused.
    subroutine test_optimize_simplex_solver_negative_budget_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_simplex_solver_negative_budget", &
            expect_abort=.true., &
            failure_message="a negative pf_simplex_solver%max_neval was expected to error stop", &
            required_stderr="pf_simplex_solver%run: max_neval must not be negative")
    end subroutine test_optimize_simplex_solver_negative_budget_aborts
    !
    subroutine test_prima_bobyqa_solver_negative_budget_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_bobyqa_solver_negative_budget", &
            expect_abort=.true., &
            failure_message="a negative pf_bobyqa_solver%max_neval was expected to error stop", &
            required_stderr="pf_bobyqa_solver%run: max_neval must not be negative")
    end subroutine test_prima_bobyqa_solver_negative_budget_aborts
    !
    ! The entry point's OWN name is asserted, not only the refusal text: the local solver refuses
    ! the same objective a moment later, so a scenario keyed on the exit status alone -- or on the
    ! message text alone -- would pass with the driver's own guard deleted.
    subroutine test_optimize_multistart_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "optimize_multistart_constraints_not_honoured", &
            expect_abort=.true., &
            failure_message="a constrained objective in the multistart driver was expected to error stop", &
            required_stderr="pf_minimize_multistart: this engine does not honour nonlinear constraints")
    end subroutine test_optimize_multistart_constraints_not_honoured_aborts
    !
    subroutine test_prima_size_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_size_zero", expect_abort=.true., &
            failure_message="a zero-length start was expected to error stop", &
            required_stderr="at least one variable is required")
    end subroutine test_prima_size_zero_aborts
    !
    subroutine test_prima_start_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_start_nan", expect_abort=.true., &
            failure_message="a NaN start was expected to error stop", &
            required_stderr="the start point must not contain NaN")
    end subroutine test_prima_start_nan_aborts
    !
    subroutine test_prima_start_infinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_start_infinite", expect_abort=.true., &
            failure_message="an infinite start was expected to error stop", &
            required_stderr="the start point must be finite")
    end subroutine test_prima_start_infinite_aborts
    !
    subroutine test_prima_bound_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_bound_nan", expect_abort=.true., &
            failure_message="a NaN bound was expected to error stop", &
            required_stderr="the bounds must not contain NaN")
    end subroutine test_prima_bound_nan_aborts
    !
    subroutine test_prima_rhobeg_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_rhobeg_too_wide", expect_abort=.true., &
            failure_message="a rhobeg wider than half the box was expected to error stop", &
            required_stderr="rhobeg must not exceed half the narrowest distance between the bounds")
    end subroutine test_prima_rhobeg_too_wide_aborts
    !
    subroutine test_prima_bounds_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_bounds_size", expect_abort=.true., &
            failure_message="bounds of the wrong length were expected to error stop", &
            required_stderr="lower, upper and x must have the same size")
    end subroutine test_prima_bounds_size_aborts
    !
    subroutine test_prima_no_space_between_bounds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_no_space_between_bounds", expect_abort=.true., &
            failure_message="a bound pair with no room was expected to error stop", &
            required_stderr="every upper bound must exceed its lower bound by more than 2*epsilon")
    end subroutine test_prima_no_space_between_bounds_aborts
    !
    subroutine test_prima_start_outside_bounds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_start_outside_bounds", expect_abort=.true., &
            failure_message="a start outside the bounds was expected to error stop", &
            required_stderr="the start point must lie within the bounds")
    end subroutine test_prima_start_outside_bounds_aborts
    !
    subroutine test_prima_rho_order_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_rho_order", expect_abort=.true., &
            failure_message="rhoend above rhobeg was expected to error stop", &
            required_stderr="rhobeg and rhoend must be finite and positive with rhoend <= rhobeg")
    end subroutine test_prima_rho_order_aborts
    !
    subroutine test_prima_npt_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_npt_range", expect_abort=.true., &
            failure_message="an npt below n+2 was expected to error stop", &
            required_stderr="npt must be in [n+2, (n+1)(n+2)/2]")
    end subroutine test_prima_npt_range_aborts
    !
    subroutine test_prima_scale_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_scale_size", expect_abort=.true., &
            failure_message="a scale of the wrong length was expected to error stop", &
            required_stderr="scale and x must have the same size")
    end subroutine test_prima_scale_size_aborts
    !
    subroutine test_prima_scale_nonpositive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_scale_nonpositive", expect_abort=.true., &
            failure_message="a zero scale was expected to error stop", &
            required_stderr="scale must be finite and positive")
    end subroutine test_prima_scale_nonpositive_aborts
    !
    subroutine test_prima_budget_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_budget_zero", expect_abort=.true., &
            failure_message="a zero budget was expected to error stop", &
            required_stderr="max_neval must be positive")
    end subroutine test_prima_budget_zero_aborts
    !
    subroutine test_prima_budget_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_budget_ceiling", expect_abort=.true., &
            failure_message="a budget above the ceiling was expected to error stop", &
            required_stderr="max_neval must not exceed huge(1)/2")
    end subroutine test_prima_budget_ceiling_aborts
    !
    subroutine test_prima_nonfinite_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_nonfinite_value", expect_abort=.true., &
            failure_message="a NaN objective value was expected to error stop", &
            required_stderr="the objective returned a non-finite value")
    end subroutine test_prima_nonfinite_value_aborts
    !
    subroutine test_prima_bobyqa_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_bobyqa_constraints_not_honoured", expect_abort=.true., &
            failure_message="a constrained objective in BOBYQA was expected to error stop", &
            required_stderr="pf_minimize_bobyqa: this engine does not honour nonlinear constraints")
    end subroutine test_prima_bobyqa_constraints_not_honoured_aborts
    !
    subroutine test_prima_lincoa_constraints_not_honoured_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_lincoa_constraints_not_honoured", &
            expect_abort=.true., &
            failure_message="a constrained objective in LINCOA was expected to error stop", &
            required_stderr="pf_minimize_lincoa: this engine does not honour nonlinear constraints")
    end subroutine test_prima_lincoa_constraints_not_honoured_aborts
    !
    subroutine test_prima_lincoa_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_lincoa_shape", expect_abort=.true., &
            failure_message="a constraint matrix of the wrong shape was expected to error stop", &
            required_stderr="a_ineq must have size(x) columns and size(b_ineq) rows")
    end subroutine test_prima_lincoa_shape_aborts
    !
    subroutine test_prima_zero_constraint_row_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_zero_constraint_row", expect_abort=.true., &
            failure_message="an all-zero constraint row was expected to error stop", &
            required_stderr="a linear constraint must not have an all-zero row")
    end subroutine test_prima_zero_constraint_row_aborts
    !
    subroutine test_prima_lincoa_infeasible_start_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_lincoa_infeasible_start", expect_abort=.true., &
            failure_message="an infeasible start in LINCOA was expected to error stop", &
            required_stderr="the start point must satisfy the linear constraints")
    end subroutine test_prima_lincoa_infeasible_start_aborts
    !
    subroutine test_prima_ctol_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_ctol_negative", expect_abort=.true., &
            failure_message="a negative ctol was expected to error stop", &
            required_stderr="ctol must be a finite, non-negative number")
    end subroutine test_prima_ctol_negative_aborts
    !
    subroutine test_prima_cobyla_negative_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_cobyla_negative_count", expect_abort=.true., &
            failure_message="a negative n_constraints was expected to error stop", &
            required_stderr="n_constraints must be non-negative")
    end subroutine test_prima_cobyla_negative_count_aborts
    !
    subroutine test_prima_constraint_nonfinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_constraint_nonfinite", expect_abort=.true., &
            failure_message="a NaN constraint value was expected to error stop", &
            required_stderr="a constraint returned a non-finite value")
    end subroutine test_prima_constraint_nonfinite_aborts
    !
    subroutine test_prima_cobyla_nonfinite_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_cobyla_nonfinite_value", expect_abort=.true., &
            failure_message="a NaN objective value in COBYLA was expected to error stop", &
            required_stderr="the objective returned a non-finite value (context: fitting the disc model)")
    end subroutine test_prima_cobyla_nonfinite_value_aborts
    !
    subroutine test_prima_ctol_nonfinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_ctol_nonfinite", expect_abort=.true., &
            failure_message="a non-finite ctol was expected to error stop", &
            required_stderr="ctol must be a finite, non-negative number")
    end subroutine test_prima_ctol_nonfinite_aborts
    !
    subroutine test_prima_context_is_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_context_is_reported", expect_abort=.true., &
            failure_message="a too-wide rhobeg with a context was expected to error stop", &
            required_stderr="(context: calibrating the response curve)")
    end subroutine test_prima_context_is_reported_aborts
    !
    subroutine test_prima_context_is_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_context_is_capped", expect_abort=.true., &
            failure_message="a 150-character context was expected to abort with a capped message", &
            required_stderr="abcdefghij...")
    end subroutine test_prima_context_is_capped_aborts
    !
    subroutine test_prima_lincoa_infeasible_start_context_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "prima_lincoa_infeasible_start_context", &
            expect_abort=.true., &
            failure_message="an infeasible LINCOA start with a context was expected to error stop", &
            required_stderr="(context: projecting onto the budget plane)")
    end subroutine test_prima_lincoa_infeasible_start_context_aborts
    !
    subroutine test_integrate_context_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "integrate_context_capped", expect_abort=.true., &
            failure_message="a 150-character context was expected to abort with a capped message", &
            required_stderr="abcdefghij...")
    end subroutine test_integrate_context_capped_aborts
    !
    ! ---- pf_find_root abort paths --------------------------------------------------------------
    !
    !> Runs one `pf_find_root` scenario and asserts its whole shape from the one run: it aborted,
    !! its control call succeeded first (the "root control solved" line), and the abort carried the
    !! library's own message. The control is what makes the abort evidence that the guard refuses
    !! the bad value rather than the whole call.
    subroutine check_root_scenario(error, scenario, required)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle
        character(len=*), intent(in)               :: scenario !! the scenario's name
        character(len=*), intent(in)               :: required !! the message, after `pf_find_root: `
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        ! 97 is error_scenarios.f90's unknown-name exit; 124 and 137 are the timeout cap's, which
        ! would otherwise read as an abort (see check_scenario_exit_status_and_stderr).
        call check(error, exitstat /= 97, "scenario name not recognized by error_scenarios.f90: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 124 .and. exitstat /= 137, "scenario TIMED OUT and was killed: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario "//scenario//" was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "root control solved", found)
        call check(error, found, scenario//": the control call must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "pf_find_root: "//required, found)
        call check(error, found, scenario//": expected the message 'pf_find_root: "//required//"'")
    end subroutine check_root_scenario
    !
    !> Every one of `pf_find_root`'s refusals, asserted by the exact text the guide page's table
    !> publishes; see `test/error_scenarios.f90` for each control and the call refused.
    subroutine test_root_reversed_bracket_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_reversed_bracket", &
            "the bracket must satisfy a < b with finite ends")
    end subroutine test_root_reversed_bracket_aborts
    !
    subroutine test_root_nan_bracket_end_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_nan_bracket_end", &
            "the bracket must satisfy a < b with finite ends")
    end subroutine test_root_nan_bracket_end_aborts
    !
    subroutine test_root_bracket_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_bracket_width", "the bracket width must be finite")
    end subroutine test_root_bracket_width_aborts
    !
    subroutine test_root_negative_atol_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_negative_atol", "atol must be a finite, non-negative number")
    end subroutine test_root_negative_atol_aborts
    !
    subroutine test_root_negative_rtol_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_negative_rtol", "rtol must be a finite, non-negative number")
    end subroutine test_root_negative_rtol_aborts
    !
    subroutine test_root_bad_max_neval_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_bad_max_neval", "max_neval must be positive")
    end subroutine test_root_bad_max_neval_aborts
    !
    subroutine test_root_bad_expansion_mode_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_bad_expansion_mode", &
            "expand%mode must be one of PF_EXPAND_NONE, PF_EXPAND_UP, PF_EXPAND_DOWN, PF_EXPAND_BOTH")
    end subroutine test_root_bad_expansion_mode_aborts
    !
    subroutine test_root_bad_expansion_factor_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_bad_expansion_factor", &
            "expand%factor must be a finite number greater than 1")
    end subroutine test_root_bad_expansion_factor_aborts
    !
    subroutine test_root_bad_expansion_tries_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_bad_expansion_tries", "expand%max_tries must not be negative")
    end subroutine test_root_bad_expansion_tries_aborts
    !
    subroutine test_root_limits_inside_the_bracket_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_limits_inside_the_bracket", &
            "the expansion limits must be finite and lie outside the initial bracket")
    end subroutine test_root_limits_inside_the_bracket_aborts
    !
    subroutine test_root_nonfinite_expansion_limit_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_nonfinite_expansion_limit", &
            "the expansion limits must be finite and lie outside the initial bracket")
    end subroutine test_root_nonfinite_expansion_limit_aborts
    !
    subroutine test_root_function_returns_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_function_returns_nan", "the function returned a NaN")
    end subroutine test_root_function_returns_nan_aborts
    !
    subroutine test_root_context_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_context_reported", &
            "the bracket must satisfy a < b with finite ends (context: my_call_site)")
    end subroutine test_root_context_reported_aborts
    !
    !> Exactly 100 characters of the caller's text survive, then `...`: ten repetitions, not
    !> eleven, so a cap moved in either direction fails here.
    subroutine test_root_context_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_root_scenario(error, "root_context_capped", &
            "the bracket must satisfy a < b with finite ends (context: "//repeat("abcdefghij", 10)//"...)")
    end subroutine test_root_context_capped_aborts
    !
    ! ---- pf_dct, pf_idct and pf_next_pow2 abort paths ------------------------------------------
    !
    !> Runs one `parquet_transform` scenario and asserts its whole shape from the one run: it
    !! aborted, its control call succeeded first (the "transform control" line), and the abort
    !! carried the library's own message. The control is what makes the abort evidence that the
    !! guard refuses the bad value rather than the whole call.
    subroutine check_transform_scenario(error, scenario, required)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle
        character(len=*), intent(in)               :: scenario !! the scenario's name
        character(len=*), intent(in)               :: required !! the message, from the entry point's name
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        ! 97 is error_scenarios.f90's unknown-name exit; 124 and 137 are the timeout cap's, which
        ! would otherwise read as an abort (see check_scenario_exit_status_and_stderr).
        call check(error, exitstat /= 97, "scenario name not recognized by error_scenarios.f90: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 124 .and. exitstat /= 137, "scenario TIMED OUT and was killed: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario "//scenario//" was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "transform control", found)
        call check(error, found, scenario//": the control call must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, required, found)
        call check(error, found, scenario//": expected the message '"//required//"'")
    end subroutine check_transform_scenario
    !
    !> Every one of `parquet_transform`'s refusals, asserted by the exact text the guide page's
    !> table publishes; see `test/error_scenarios.f90` for each control and the call refused.
    subroutine test_transform_empty_sequence_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_empty_sequence", "pf_dct: the sequence must not be empty")
    end subroutine test_transform_empty_sequence_aborts
    !
    subroutine test_transform_length_not_pow2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_length_not_pow2", &
            "pf_dct: the sequence length must be a power of two (got 1000; pf_next_pow2 gives 1024)")
    end subroutine test_transform_length_not_pow2_aborts
    !
    subroutine test_transform_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_size_mismatch", "pf_dct: x and y must have the same size")
    end subroutine test_transform_size_mismatch_aborts
    !
    subroutine test_transform_idct_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_idct_size_mismatch", "pf_idct: x and y must have the same size")
    end subroutine test_transform_idct_size_mismatch_aborts
    !
    subroutine test_transform_bad_norm_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_bad_norm_token", 'pf_dct: norm must be "none" or "ortho"')
    end subroutine test_transform_bad_norm_token_aborts
    !
    subroutine test_transform_next_pow2_nonpositive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_next_pow2_nonpositive", "pf_next_pow2: n must be positive")
    end subroutine test_transform_next_pow2_nonpositive_aborts
    !
    !> The limit is printed as a number, the largest power of two a default integer holds.
    subroutine test_transform_next_pow2_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=24) :: top

        write (top, '(i0)') 2**(digits(1) - 1)
        call check_transform_scenario(error, "transform_next_pow2_too_large", &
            "pf_next_pow2: n must not exceed "//trim(top)//", the largest power of two a default integer holds")
    end subroutine test_transform_next_pow2_too_large_aborts
    !
    subroutine test_transform_context_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_context_reported", &
            "pf_dct: x and y must have the same size (context: my_call_site)")
    end subroutine test_transform_context_reported_aborts
    !
    !> Exactly 100 characters of the caller's text survive, then `...`: ten repetitions, not
    !> eleven, so a cap moved in either direction fails here.
    subroutine test_transform_context_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_context_capped", &
            "pf_dct: x and y must have the same size (context: "//repeat("abcdefghij", 10)//"...)")
    end subroutine test_transform_context_capped_aborts
    !
    !> The four below are the negative controls for the sine pair's delegation: `pf_dst` and
    !> `pf_idst` are built on `pf_dct` and `pf_idct`, so a wrapper that delegated before
    !> validating would report the cosine procedure's name here and fail every one of them.
    !
    subroutine test_transform_dst_length_not_pow2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_dst_length_not_pow2", &
            "pf_dst: the sequence length must be a power of two (got 1000; pf_next_pow2 gives 1024)")
    end subroutine test_transform_dst_length_not_pow2_aborts
    !
    subroutine test_transform_dst_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_dst_size_mismatch", &
            "pf_dst: x and y must have the same size (context: my_call_site)")
    end subroutine test_transform_dst_size_mismatch_aborts
    !
    subroutine test_transform_idst_length_not_pow2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_idst_length_not_pow2", &
            "pf_idst: the sequence length must be a power of two (got 1000; pf_next_pow2 gives 1024)")
    end subroutine test_transform_idst_length_not_pow2_aborts
    !
    subroutine test_transform_idst_bad_norm_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_transform_scenario(error, "transform_idst_bad_norm_token", &
            'pf_idst: norm must be "none" or "ortho"')
    end subroutine test_transform_idst_bad_norm_token_aborts
    !
    subroutine test_interpolate_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_size_mismatch", expect_abort=.true., &
            failure_message="x and y of different sizes were expected to error stop", &
            required_stderr="pf_interp_1d%init: x and y differ in size: 5 and 4")
    end subroutine test_interpolate_size_mismatch_aborts
    !
    subroutine test_interpolate_is_valid_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_is_valid_size", expect_abort=.true., &
            failure_message="an is_valid mask of the wrong size was expected to error stop", &
            required_stderr="pf_interp_1d%init: is_valid has 4 elements for 5 points")
    end subroutine test_interpolate_is_valid_size_aborts
    !
    subroutine test_interpolate_unknown_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_unknown_method", expect_abort=.true., &
            failure_message="an unknown method token was expected to error stop", &
            required_stderr="pf_interp_1d%init: unknown method ""quadratic""; expected ""linear"", ""cubic"" or ""pchip""")
    end subroutine test_interpolate_unknown_method_aborts
    !
    subroutine test_interpolate_unknown_outside_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_unknown_outside", expect_abort=.true., &
            failure_message="an unknown outside token was expected to error stop", &
            required_stderr="pf_interp_1d%init: unknown outside ""wrap""; expected ""clamp"", ""extrapolate"" or ""nan""")
    end subroutine test_interpolate_unknown_outside_aborts
    !
    subroutine test_interpolate_too_few_points_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_too_few_points", expect_abort=.true., &
            failure_message="a one-point table was expected to error stop", &
            required_stderr="pf_interp_1d%init: at least 2 points are needed for method ""cubic""; got 1")
    end subroutine test_interpolate_too_few_points_aborts
    !
    subroutine test_interpolate_too_few_after_is_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_too_few_after_is_valid", expect_abort=.true., &
            failure_message="a table with one point surviving its mask was expected to error stop", &
            required_stderr="pf_interp_1d%init: at least 2 points are needed for method ""cubic""; got 1")
    end subroutine test_interpolate_too_few_after_is_valid_aborts
    !
    subroutine test_interpolate_repeated_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_repeated_x", expect_abort=.true., &
            failure_message="a repeated abscissa was expected to error stop", &
            required_stderr="pf_interp_1d%init: x must be strictly increasing or strictly decreasing")
    end subroutine test_interpolate_repeated_x_aborts
    !
    subroutine test_interpolate_unsorted_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_unsorted_x", expect_abort=.true., &
            failure_message="unsorted abscissae were expected to error stop", &
            required_stderr="pf_interp_1d%init: x must be strictly increasing or strictly decreasing")
    end subroutine test_interpolate_unsorted_x_aborts
    !
    subroutine test_interpolate_nan_in_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_nan_in_x", expect_abort=.true., &
            failure_message="a NaN abscissa was expected to error stop", &
            required_stderr="pf_interp_1d%init: x must be strictly increasing or strictly decreasing")
    end subroutine test_interpolate_nan_in_x_aborts
    !
    subroutine test_interpolate_inf_in_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_inf_in_x", expect_abort=.true., &
            failure_message="an infinite abscissa was expected to error stop", &
            required_stderr="pf_interp_1d%init: x must be finite")
    end subroutine test_interpolate_inf_in_x_aborts
    !
    subroutine test_interpolate_nan_in_y_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_nan_in_y", expect_abort=.true., &
            failure_message="a NaN ordinate was expected to error stop", &
            required_stderr="pf_interp_1d%init: y must be finite")
    end subroutine test_interpolate_nan_in_y_aborts
    !
    subroutine test_interpolate_inf_in_y_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_inf_in_y", expect_abort=.true., &
            failure_message="an infinite ordinate was expected to error stop", &
            required_stderr="pf_interp_1d%init: y must be finite")
    end subroutine test_interpolate_inf_in_y_aborts
    !
    subroutine test_cosmology_eval_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_eval_before_init", expect_abort=.true., &
            failure_message="evaluating an unbuilt cosmology was expected to error stop", &
            required_stderr="pf_cosmology%comoving_distance: the cosmology is not initialised")
    end subroutine test_cosmology_eval_before_init_aborts
    !
    subroutine test_cosmology_eval_after_clear_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_eval_after_clear", expect_abort=.true., &
            failure_message="evaluating a cleared cosmology was expected to error stop", &
            required_stderr="pf_cosmology%age: the cosmology is not initialised")
    end subroutine test_cosmology_eval_after_clear_aborts
    !
    subroutine test_cosmology_init_unknown_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_unknown_name", expect_abort=.true., &
            failure_message="an unknown cosmology name was expected to error stop", &
            required_stderr="pf_cosmology%init: unknown cosmology")
    end subroutine test_cosmology_init_unknown_name_aborts
    !
    subroutine test_cosmology_init_h0_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_h0_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range h0 was expected to error stop", &
            required_stderr="pf_cosmology%init: h0 must be finite and within [1e-10, 1e10] km/s/Mpc")
    end subroutine test_cosmology_init_h0_out_of_range_aborts
    !
    subroutine test_cosmology_init_om0_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_om0_negative", expect_abort=.true., &
            failure_message="a negative om0 was expected to error stop", &
            required_stderr="pf_cosmology%init: om0 must be finite and non-negative")
    end subroutine test_cosmology_init_om0_negative_aborts
    !
    subroutine test_cosmology_init_tcmb0_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_tcmb0_negative", expect_abort=.true., &
            failure_message="a negative tcmb0 was expected to error stop", &
            required_stderr="pf_cosmology%init: tcmb0 must be finite and non-negative")
    end subroutine test_cosmology_init_tcmb0_negative_aborts
    !
    subroutine test_cosmology_init_m_nu_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_m_nu_size", expect_abort=.true., &
            failure_message="an m_nu of the wrong size was expected to error stop", &
            required_stderr="pf_cosmology%init: m_nu needs one finite, non-negative mass per species")
    end subroutine test_cosmology_init_m_nu_size_aborts
    !
    subroutine test_cosmology_init_w0_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_w0_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range w0 was expected to error stop", &
            required_stderr="pf_cosmology%init: w0 must be finite and within [-3, 3]")
    end subroutine test_cosmology_init_w0_out_of_range_aborts
    !
    subroutine test_cosmology_init_wa_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_wa_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range wa was expected to error stop", &
            required_stderr="pf_cosmology%init: wa must be finite and within [-3, 3]")
    end subroutine test_cosmology_init_wa_out_of_range_aborts
    !
    subroutine test_cosmology_init_ode0_not_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_ode0_not_finite", expect_abort=.true., &
            failure_message="a NaN ode0 was expected to error stop", &
            required_stderr="pf_cosmology%init: ode0 must be finite")
    end subroutine test_cosmology_init_ode0_not_finite_aborts
    !
    subroutine test_cosmology_init_neff_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_neff_negative", expect_abort=.true., &
            failure_message="a negative neff was expected to error stop", &
            required_stderr="pf_cosmology%init: neff must be finite and non-negative")
    end subroutine test_cosmology_init_neff_negative_aborts
    !
    !> The PER-ENTRY m_nu guard. The message names the offending entry, which is what tells it
    !> apart from the LENGTH guard `cosmology_init_m_nu_size` asserts.
    subroutine test_cosmology_init_m_nu_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_m_nu_negative", expect_abort=.true., &
            failure_message="a negative neutrino mass was expected to error stop", &
            required_stderr="per species: entry")
    end subroutine test_cosmology_init_m_nu_negative_aborts
    !
    !> A 150-character context reaches the message cut to 100 characters and elided.
    subroutine test_cosmology_init_context_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_context_capped", expect_abort=.true., &
            failure_message="a negative h0 with a long context was expected to error stop", &
            required_stderr="abcdefghijabcdefghij...]")
    end subroutine test_cosmology_init_context_capped_aborts
    !
    subroutine test_cosmology_config_label_without_parameters_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_label_without_parameters", &
            expect_abort=.true., &
            failure_message="a misspelt cosmology name with no parameters was expected to error stop", &
            required_stderr="a name that is not one of the eight named cosmologies is a label")
    end subroutine test_cosmology_config_label_without_parameters_aborts
    !
    subroutine test_cosmology_init_zmax_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_zmax_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range zmax was expected to error stop", &
            required_stderr="pf_cosmology%init: zmax must be finite, positive and at most 1e10")
    end subroutine test_cosmology_init_zmax_out_of_range_aborts
    !
    subroutine test_cosmology_init_zmin_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_zmin_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range zmin was expected to error stop", &
            required_stderr="pf_cosmology%init: zmin must be finite and within (-1, 0]")
    end subroutine test_cosmology_init_zmin_out_of_range_aborts
    !
    subroutine test_cosmology_init_ob0_above_om0_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_ob0_above_om0", expect_abort=.true., &
            failure_message="an ob0 above om0 was expected to error stop", &
            required_stderr="pf_cosmology%init: ob0 must be finite, non-negative and at most om0")
    end subroutine test_cosmology_init_ob0_above_om0_aborts
    !
    subroutine test_cosmology_init_density_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_density_too_large", expect_abort=.true., &
            failure_message="an absurd ode0 was expected to error stop", &
            required_stderr="pf_cosmology%init: every density parameter must be at most 1e6 in magnitude")
    end subroutine test_cosmology_init_density_too_large_aborts
    !
    subroutine test_cosmology_init_tcmb0_too_hot_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_tcmb0_too_hot", expect_abort=.true., &
            failure_message="an absurd tcmb0 was expected to error stop", &
            required_stderr="pf_cosmology%init: every density parameter must be at most 1e6 in magnitude")
    end subroutine test_cosmology_init_tcmb0_too_hot_aborts
    !
    subroutine test_cosmology_init_no_big_bang_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_no_big_bang", expect_abort=.true., &
            failure_message="a cosmology with no big bang was expected to error stop", &
            required_stderr="pf_cosmology%init: this cosmology has no big bang")
    end subroutine test_cosmology_init_no_big_bang_aborts
    !
    subroutine test_cosmology_init_table_not_converged_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_init_table_not_converged", expect_abort=.true., &
            failure_message="a table that could not converge was expected to error stop", &
            required_stderr="table did not converge")
    end subroutine test_cosmology_init_table_not_converged_aborts
    !
    !> An absent `[cosmology]` is `pf_toml_section`'s own required path: this feature adds no
    !> message of its own for it, because `found=` is how a caller says the section is optional.
    subroutine test_cosmology_config_missing_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_missing_section", &
            expect_abort=.true., &
            failure_message="a configuration with no [cosmology] section was expected to error stop", &
            required_stderr="config section not found")
    end subroutine test_cosmology_config_missing_section_aborts
    !
    !> `found=` answers for an ABSENT section; a section of the wrong SHAPE is still fatal.
    !!
    !! The guide says so on `doc/pages/utilities/configuration-files.md`, and nothing asserted it.
    !! The message is `parquet_toml`'s, because that is where the shape is decided.
    subroutine test_cosmology_config_array_of_tables_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_array_of_tables", &
            expect_abort=.true., &
            failure_message="a [[cosmology]] array of tables was expected to error stop even " // &
                            "with found= present", &
            required_stderr="config name is not a section: cosmology")
    end subroutine test_cosmology_config_array_of_tables_aborts
    !
    !> The one message this module writes itself.
    subroutine test_cosmology_config_named_with_parameters_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_named_with_parameters", &
            expect_abort=.true., &
            failure_message="a named cosmology given parameters was expected to error stop", &
            required_stderr="pf_cosmology_from_toml: a named cosmology cannot be given parameters as well")
    end subroutine test_cosmology_config_named_with_parameters_aborts
    !
    !> A wrong-typed value is `parquet_toml`'s abort, unchanged by this module.
    subroutine test_cosmology_config_bad_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_bad_type", &
            expect_abort=.true., &
            failure_message="h0 given as a string was expected to error stop", &
            required_stderr="config value has the wrong type: h0")
    end subroutine test_cosmology_config_bad_type_aborts
    !
    !> Every model validation stays `%init`'s: this module adds no second set of rules.
    subroutine test_cosmology_config_mnu_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_mnu_length", &
            expect_abort=.true., &
            failure_message="an m_nu of the wrong length in a file was expected to error stop", &
            required_stderr="pf_cosmology%init: m_nu needs one finite, non-negative mass per species")
    end subroutine test_cosmology_config_mnu_length_aborts
    !
    !> And the context this module composes reaches that message, so it says WHERE the bad number
    !> came from -- which is the whole reason the reader passes a `context=` at all.
    subroutine test_cosmology_config_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "cosmology_config_out_of_range", &
            expect_abort=.true., &
            failure_message="a negative h0 in a file was expected to error stop", &
            required_stderr="configuration file run.toml, section [cosmology]")
    end subroutine test_cosmology_config_out_of_range_aborts
    !
    subroutine test_interpolate_eval_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_eval_before_init", expect_abort=.true., &
            failure_message="evaluating an unbuilt interpolant was expected to error stop", &
            required_stderr="pf_interp_1d%eval: the interpolant is not initialised")
    end subroutine test_interpolate_eval_before_init_aborts
    !
    subroutine test_interpolate_context_reported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_context_reported", expect_abort=.true., &
            failure_message="a size mismatch with a context was expected to abort", &
            required_stderr="x and y differ in size: 3 and 2 (context: my_call_site)")
    end subroutine test_interpolate_context_reported_aborts
    !
    subroutine test_interpolate_context_capped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_context_capped", expect_abort=.true., &
            failure_message="a 150-character context was expected to abort with a capped message", &
            required_stderr="(context: " // repeat("abcdefghij", 10) // "...)")
    end subroutine test_interpolate_context_capped_aborts
    !
    subroutine test_interpolate_one_shot_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_one_shot_size_mismatch", expect_abort=.true., &
            failure_message="a one-shot call with x and y of different sizes was expected to error stop", &
            required_stderr="pf_interp: x and y differ in size: 3 and 2")
    end subroutine test_interpolate_one_shot_size_mismatch_aborts
    !
    subroutine test_interpolate_bc_with_linear_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_bc_with_linear", expect_abort=.true., &
            failure_message="an end condition for linear interpolation was expected to error stop", &
            required_stderr="pf_interp_1d%init: bc applies only to method ""cubic""")
    end subroutine test_interpolate_bc_with_linear_aborts
    !
    subroutine test_interpolate_unknown_bc_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_unknown_bc", expect_abort=.true., &
            failure_message="an unknown end-condition token was expected to error stop", &
            required_stderr="pf_interp_1d%init: unknown bc ""periodic""; expected ""natural"", ""not_a_knot"" or ""clamped""")
    end subroutine test_interpolate_unknown_bc_aborts
    !
    subroutine test_interpolate_clamped_without_slopes_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_clamped_without_slopes", expect_abort=.true., &
            failure_message="bc=""clamped"" without slopes was expected to error stop", &
            required_stderr="pf_interp_1d%init: bc ""clamped"" needs slopes")
    end subroutine test_interpolate_clamped_without_slopes_aborts
    !
    subroutine test_interpolate_slopes_without_clamped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_slopes_without_clamped", expect_abort=.true., &
            failure_message="slopes with bc=""natural"" were expected to error stop", &
            required_stderr="pf_interp_1d%init: slopes apply only to bc ""clamped""")
    end subroutine test_interpolate_slopes_without_clamped_aborts
    !
    subroutine test_interpolate_slopes_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_slopes_size", expect_abort=.true., &
            failure_message="one slope for a clamped spline was expected to error stop", &
            required_stderr="pf_interp_1d%init: slopes must hold exactly 2 values, one per end; got 1")
    end subroutine test_interpolate_slopes_size_aborts
    !
    subroutine test_interpolate_slopes_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_slopes_nan", expect_abort=.true., &
            failure_message="a NaN end slope was expected to error stop", &
            required_stderr="pf_interp_1d%init: slopes must be finite")
    end subroutine test_interpolate_slopes_nan_aborts
    !
    subroutine test_interpolate_slopes_inf_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_slopes_inf", expect_abort=.true., &
            failure_message="an infinite end slope was expected to error stop", &
            required_stderr="pf_interp_1d%init: slopes must be finite")
    end subroutine test_interpolate_slopes_inf_aborts
    !
    subroutine test_interpolate_not_a_knot_too_few_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_not_a_knot_too_few", expect_abort=.true., &
            failure_message="a not-a-knot spline over three points was expected to error stop", &
            required_stderr="pf_interp_1d%init: at least 4 points are needed for method ""cubic"" with bc ""not_a_knot""; got 3")
    end subroutine test_interpolate_not_a_knot_too_few_aborts
    !
    subroutine test_interpolate_linear_too_few_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_linear_too_few", expect_abort=.true., &
            failure_message="a linear interpolant over one point was expected to error stop", &
            required_stderr="pf_interp_1d%init: at least 2 points are needed for method ""linear""; got 1")
    end subroutine test_interpolate_linear_too_few_aborts
    !
    subroutine test_interpolate_pchip_too_few_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_pchip_too_few", expect_abort=.true., &
            failure_message="a pchip interpolant over one point was expected to error stop", &
            required_stderr="pf_interp_1d%init: at least 2 points are needed for method ""pchip""; got 1")
    end subroutine test_interpolate_pchip_too_few_aborts
    !
    subroutine test_interpolate_derivative_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_derivative_before_init", expect_abort=.true., &
            failure_message="differentiating an unbuilt interpolant was expected to error stop", &
            required_stderr="pf_interp_1d%derivative: the interpolant is not initialised")
    end subroutine test_interpolate_derivative_before_init_aborts
    !
    subroutine test_interpolate_integral_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_integral_before_init", expect_abort=.true., &
            failure_message="integrating an unbuilt interpolant was expected to error stop", &
            required_stderr="pf_interp_1d%integral: the interpolant is not initialised")
    end subroutine test_interpolate_integral_before_init_aborts
    !
    subroutine test_interpolate_bad_order_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_bad_order", expect_abort=.true., &
            failure_message="a third derivative was expected to error stop", &
            required_stderr="pf_interp_1d%derivative: order must be 1 or 2")
    end subroutine test_interpolate_bad_order_aborts
    !
    subroutine test_interpolate_2d_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_shape", expect_abort=.true., &
            failure_message="values shaped (7, 5) for a 5 by 7 grid were expected to error stop", &
            required_stderr="pf_interp_2d%init: z must be shaped (size(x), size(y)): got (7, 5) for (5, 7)")
    end subroutine test_interpolate_2d_shape_aborts
    !
    subroutine test_interpolate_2d_unknown_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_unknown_method", expect_abort=.true., &
            failure_message="an unknown grid method token was expected to error stop", &
            required_stderr="pf_interp_2d%init: unknown method ""quadratic""; expected ""linear"" or ""cubic""")
    end subroutine test_interpolate_2d_unknown_method_aborts
    !
    subroutine test_interpolate_2d_pchip_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_pchip", expect_abort=.true., &
            failure_message="method=pchip on a grid was expected to error stop", &
            required_stderr="pf_interp_2d%init: method ""pchip"" is not offered in two dimensions")
    end subroutine test_interpolate_2d_pchip_aborts
    !
    subroutine test_interpolate_2d_bc_with_linear_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_bc_with_linear", expect_abort=.true., &
            failure_message="an end condition for bilinear interpolation was expected to error stop", &
            required_stderr="pf_interp_2d%init: bc applies only to method ""cubic""")
    end subroutine test_interpolate_2d_bc_with_linear_aborts
    !
    subroutine test_interpolate_2d_unknown_bc_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_unknown_bc", expect_abort=.true., &
            failure_message="an unknown grid end-condition token was expected to error stop", &
            required_stderr="pf_interp_2d%init: unknown bc ""periodic""; expected ""natural"" or ""not_a_knot""")
    end subroutine test_interpolate_2d_unknown_bc_aborts
    !
    subroutine test_interpolate_2d_clamped_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_clamped", expect_abort=.true., &
            failure_message="bc=clamped on a grid was expected to error stop", &
            required_stderr="pf_interp_2d%init: bc ""clamped"" is not offered in two dimensions")
    end subroutine test_interpolate_2d_clamped_aborts
    !
    subroutine test_interpolate_2d_unknown_outside_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_unknown_outside", expect_abort=.true., &
            failure_message="an unknown grid outside token was expected to error stop", &
            required_stderr="pf_interp_2d%init: unknown outside ""wrap""; expected ""clamp"", ""extrapolate"" or ""nan""")
    end subroutine test_interpolate_2d_unknown_outside_aborts
    !
    subroutine test_interpolate_2d_too_few_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_too_few_x", expect_abort=.true., &
            failure_message="a grid with one line along x was expected to error stop", &
            required_stderr="pf_interp_2d%init: at least 2 points are needed along x for method ""cubic""; got 1")
    end subroutine test_interpolate_2d_too_few_x_aborts
    !
    subroutine test_interpolate_2d_not_a_knot_too_few_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_not_a_knot_too_few", expect_abort=.true., &
            failure_message="a not-a-knot grid spline over three lines along y was expected to error stop", &
            required_stderr="pf_interp_2d%init: at least 4 points are needed along y for method ""cubic"" " // &
                "with bc ""not_a_knot""; got 3")
    end subroutine test_interpolate_2d_not_a_knot_too_few_aborts
    !
    subroutine test_interpolate_2d_x_not_monotonic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_x_not_monotonic", expect_abort=.true., &
            failure_message="unsorted lines along x were expected to error stop", &
            required_stderr="pf_interp_2d%init: x must be strictly increasing or strictly decreasing")
    end subroutine test_interpolate_2d_x_not_monotonic_aborts
    !
    subroutine test_interpolate_2d_y_not_monotonic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_y_not_monotonic", expect_abort=.true., &
            failure_message="a repeated line along y was expected to error stop", &
            required_stderr="pf_interp_2d%init: y must be strictly increasing or strictly decreasing")
    end subroutine test_interpolate_2d_y_not_monotonic_aborts
    !
    subroutine test_interpolate_2d_inf_in_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_inf_in_x", expect_abort=.true., &
            failure_message="an infinite line along x was expected to error stop", &
            required_stderr="pf_interp_2d%init: x must be finite")
    end subroutine test_interpolate_2d_inf_in_x_aborts
    !
    subroutine test_interpolate_2d_inf_in_y_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_inf_in_y", expect_abort=.true., &
            failure_message="an infinite line along y was expected to error stop", &
            required_stderr="pf_interp_2d%init: y must be finite")
    end subroutine test_interpolate_2d_inf_in_y_aborts
    !
    subroutine test_interpolate_2d_nan_in_z_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_nan_in_z", expect_abort=.true., &
            failure_message="a NaN grid value was expected to error stop", &
            required_stderr="pf_interp_2d%init: z must be finite")
    end subroutine test_interpolate_2d_nan_in_z_aborts
    !
    subroutine test_interpolate_2d_eval_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_eval_before_init", expect_abort=.true., &
            failure_message="evaluating an unbuilt grid interpolant was expected to error stop", &
            required_stderr="pf_interp_2d%eval: the interpolant is not initialised")
    end subroutine test_interpolate_2d_eval_before_init_aborts
    !
    subroutine test_interpolate_2d_one_shot_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_one_shot_shape", expect_abort=.true., &
            failure_message="a misshaped grid in one shot was expected to error stop", &
            required_stderr="pf_interp: z must be shaped (size(x), size(y)): got (7, 5) for (5, 7) (context: my_grid)")
    end subroutine test_interpolate_2d_one_shot_shape_aborts
    !
    subroutine test_interpolate_2d_query_sizes_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_query_sizes", expect_abort=.true., &
            failure_message="query coordinates of different sizes were expected to error stop", &
            required_stderr="pf_interp: xq and yq differ in size: 3 and 2")
    end subroutine test_interpolate_2d_query_sizes_aborts
    !
    subroutine test_interpolate_spline_too_wide_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_spline_too_wide", expect_abort=.true., &
            failure_message="a cubic spline over knots 1e160 apart was expected to error stop", &
            required_stderr="pf_interp_1d%init: x has a spacing of 1.000E+160, above the cubic spline's limit of " // &
            "6.704E+153; rescale x, or use method ""linear"" or ""pchip""")
    end subroutine test_interpolate_spline_too_wide_aborts
    !
    !> The refusal is detected FROM the overflow, so the solve that raises it runs with the overflow
    !> and invalid halting modes held off and both flags put back (`interp_1d_build`). Two things
    !> follow, and this asserts each: the run reaches the library's own message, which a build that
    !> traps on overflow could not, and it leaves no raised flag behind.
    !>
    !> The forbidden text is gfortran's end-of-run note, the one runtime here that reports a flag
    !> left raised; it is what fails if the restore is dropped. ifx and flang print no such note, so
    !> on those the second assertion costs nothing and the first carries the test. Under nagfor the
    !> FIRST assertion is the real one -- with the hold-off gone, `-ieee=stop` ends the scenario
    !> inside the solve and the required message never appears.
    subroutine test_interpolate_spline_overflows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_spline_overflows", expect_abort=.true., &
            failure_message="a cubic spline whose second derivatives overflow was expected to error stop", &
            required_stderr="pf_interp_1d%init: the cubic spline's second derivatives overflow, with x as closely " // &
            "spaced as 1.000E-160; rescale the table, or use method ""linear"" or ""pchip""", &
            forbidden_stderr="IEEE_OVERFLOW_FLAG")
    end subroutine test_interpolate_spline_overflows_aborts
    !
    subroutine test_interpolate_2d_spline_too_wide_x_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_spline_too_wide_x", expect_abort=.true., &
            failure_message="a bicubic spline over lines along x 1e160 apart was expected to error stop", &
            required_stderr="pf_interp_2d%init: x has a spacing of 1.000E+160, above the bicubic spline's limit of " // &
            "6.704E+153; rescale x, or use method ""linear""")
    end subroutine test_interpolate_2d_spline_too_wide_x_aborts
    !
    subroutine test_interpolate_2d_spline_too_wide_y_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_spline_too_wide_y", expect_abort=.true., &
            failure_message="a bicubic spline over lines along y 1e160 apart was expected to error stop", &
            required_stderr="pf_interp_2d%init: y has a spacing of 1.000E+160, above the bicubic spline's limit of " // &
            "6.704E+153; rescale y, or use method ""linear""")
    end subroutine test_interpolate_2d_spline_too_wide_y_aborts
    !
    !> The grid's mirror of `test_interpolate_spline_overflows_aborts`, which says what both assert.
    !> Its three solves are held off together, so one escaped flag from any of them fails this.
    subroutine test_interpolate_2d_spline_overflows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_2d_spline_overflows", expect_abort=.true., &
            failure_message="a bicubic spline whose second derivatives overflow was expected to error stop", &
            required_stderr="pf_interp_2d%init: the bicubic spline's second derivatives overflow, with grid lines " // &
            "as closely spaced as 1.000E-160; rescale the grid, or use method ""linear""", &
            forbidden_stderr="IEEE_OVERFLOW_FLAG")
    end subroutine test_interpolate_2d_spline_overflows_aborts
    !
    subroutine test_interpolate_long_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_long_token", expect_abort=.true., &
            failure_message="a method token of 107 characters was expected to error stop", &
            required_stderr="pf_interp_1d%init: unknown method ""linear")
    end subroutine test_interpolate_long_token_aborts
    !
    subroutine test_interpolate_eval_array_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "interpolate_eval_array_before_init", expect_abort=.true., &
            failure_message="evaluating an unbuilt interpolant at a rank-1 array of no queries was expected to error stop", &
            required_stderr="pf_interp_1d%eval: the interpolant is not initialised")
    end subroutine test_interpolate_eval_array_before_init_aborts
    !
    ! ---- pf_kde abort paths --------------------------------------------------------------------
    !
    !> Whether this build flushes subnormals to zero, so no scenario can present one.
    !!
    !! **ifx turns flush-to-zero and denormals-are-zero on by default at `-O1` and above**, which
    !! is what a flagless `fpm test` selects; gfortran, flang and nagfor leave gradual underflow in
    !! force. It is a process-wide MXCSR setting, and the scenario binary is built the same way as
    !! this one, so the answer here is the answer there. Building the value from its bit pattern
    !! rather than by arithmetic does not help: denormals-are-zero collapses it again on the
    !! comparison that reads it. Measured rather than asked, for the reason
    !! `subnormals_are_flushed` in `test_utils.f90` gives.
    function subnormals_are_flushed() result(res)
        logical :: res !! `.true.` when underflow is abrupt, so subnormal inputs read as zero.
        real(real64), volatile :: half

        half = tiny(1.0_real64)
        half = 0.5_real64 * half
        res = .not. (half > 0.0_real64)
    end function subnormals_are_flushed
    !
    !> Runs one `parquet_kde` scenario and asserts its whole shape from the one run: it aborted,
    !! its control call succeeded first (the "kde control" line), and the abort carried the
    !! library's own message, binding and all. The control is what makes the abort evidence that
    !! the guard refuses the bad value rather than the whole call.
    subroutine check_kde_scenario(error, scenario, required, control)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle
        character(len=*), intent(in)               :: scenario !! the scenario's name
        character(len=*), intent(in)               :: required !! the message, from the binding's name
        character(len=*), intent(in), optional     :: control  !! the control's line in full, where the
                                                               !! control asserts a VALUE and not only
                                                               !! that it ran; absent, the marker alone
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        ! 97 is error_scenarios.f90's unknown-name exit; 124 and 137 are the timeout cap's, which
        ! would otherwise read as an abort (see check_scenario_exit_status_and_stderr).
        call check(error, exitstat /= 97, "scenario name not recognized by error_scenarios.f90: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 124 .and. exitstat /= 137, "scenario TIMED OUT and was killed: "//scenario)
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario "//scenario//" was expected to abort")
        if (allocated(error)) return
        if (present(control)) then
            call scenario_capture_contains(out_file, err_file, control, found)
            call check(error, found, scenario//": the control call must succeed first and print '"//control//"'")
        else
            call scenario_capture_contains(out_file, err_file, "kde control", found)
            call check(error, found, scenario//": the control call must succeed first, or the abort proves nothing")
        end if
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, required, found)
        call check(error, found, scenario//": expected the message '"//required//"'")
    end subroutine check_kde_scenario
    !
    !> Every one of `pf_kde`'s refusals, asserted by the exact text the guide page's table
    !> publishes; see `test/error_scenarios.f90` for each control and the call refused. The
    !> three written in the family's words prove `parquet_stats`' checkers are called with
    !> `what = "pf_kde%fit"`.
    subroutine test_kde_bandwidth_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_zero", &
            "pf_kde%fit: bandwidth must be a finite, positive number")
    end subroutine test_kde_bandwidth_zero_aborts
    !
    subroutine test_kde_bandwidth_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_nan", &
            "pf_kde%fit: bandwidth must be a finite, positive number")
    end subroutine test_kde_bandwidth_nan_aborts
    !
    subroutine test_kde_bandwidth_and_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_and_rule", &
            "pf_kde%fit: bandwidth= and rule= cannot both be given; use adjust= to scale a rule")
    end subroutine test_kde_bandwidth_and_rule_aborts
    !
    subroutine test_kde_unknown_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_unknown_rule", &
            "pf_kde%fit: rule must be ""isj"", ""lscv"", ""silverman"" or ""scott""")
    end subroutine test_kde_unknown_rule_aborts
    !
    subroutine test_kde_adjust_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_adjust_negative", &
            "pf_kde%fit: adjust must be a finite, positive number")
    end subroutine test_kde_adjust_negative_aborts
    !
    subroutine test_kde_unknown_kernel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_unknown_kernel", &
            "pf_kde%fit: kernel must be ""gaussian"", ""epanechnikov"", ""bspline"" or ""box""")
    end subroutine test_kde_unknown_kernel_aborts
    !
    subroutine test_kde_bound_infinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bound_infinite", &
            "pf_kde%fit: lower and upper must be finite")
    end subroutine test_kde_bound_infinite_aborts
    !
    subroutine test_kde_lower_not_below_upper_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_lower_not_below_upper", &
            "pf_kde%fit: lower must be below upper")
    end subroutine test_kde_lower_not_below_upper_aborts
    !
    subroutine test_kde_boundary_without_bound_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_boundary_without_bound", &
            "pf_kde%fit: boundary= needs lower= or upper=")
    end subroutine test_kde_boundary_without_bound_aborts
    !
    subroutine test_kde_unknown_boundary_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_unknown_boundary", &
            "pf_kde%fit: boundary must be ""renormalise"", ""reflect"" or ""linear""")
    end subroutine test_kde_unknown_boundary_aborts
    !
    subroutine test_kde_weights_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_weights_size", &
            "pf_kde%fit: weights has 3 elements but values has 4")
    end subroutine test_kde_weights_size_aborts
    !
    subroutine test_kde_is_valid_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_is_valid_size", &
            "pf_kde%fit: is_valid has 5 elements but values has 4")
    end subroutine test_kde_is_valid_size_aborts
    !
    subroutine test_kde_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_negative_weight", &
            "pf_kde%fit: weight 2 is negative; weights must be finite and non-negative")
    end subroutine test_kde_negative_weight_aborts
    !
    subroutine test_kde_weight_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_weight_type", &
            "pf_kde%fit: weight_type ""bogus"" is not recognised")
    end subroutine test_kde_weight_type_aborts
    !
    subroutine test_kde_real32_weights_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_real32_weights_size", &
            "pf_kde%fit: weights has 2 elements but values has 4")
    end subroutine test_kde_real32_weights_size_aborts
    !
    subroutine test_kde_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_threads_zero", &
            "pf_kde%fit: threads must be positive")
    end subroutine test_kde_threads_zero_aborts
    !
    subroutine test_kde_query_unfitted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_query_unfitted", &
            "pf_kde%pdf: the estimate has not been fitted")
    end subroutine test_kde_query_unfitted_aborts
    !
    subroutine test_kde_query_after_clear_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_query_after_clear", &
            "pf_kde%cdf: the estimate has not been fitted")
    end subroutine test_kde_query_after_clear_aborts
    !
    subroutine test_kde_accessor_unfitted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_accessor_unfitted", &
            "pf_kde%bandwidth: the estimate has not been fitted")
    end subroutine test_kde_accessor_unfitted_aborts
    !
    subroutine test_kde_pdf_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_pdf_size", &
            "pf_kde%pdf: f must have one element per point of x")
    end subroutine test_kde_pdf_size_aborts
    !
    subroutine test_kde_cdf_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_cdf_size", &
            "pf_kde%cdf: p must have one element per point of x")
    end subroutine test_kde_cdf_size_aborts
    !
    subroutine test_kde_quantile_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_quantile_size", &
            "pf_kde%quantile: x must have one element per element of p")
    end subroutine test_kde_quantile_size_aborts
    !
    subroutine test_kde_quantile_p_above_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_quantile_p_above_one", &
            "pf_kde%quantile: p must lie in [0, 1]")
    end subroutine test_kde_quantile_p_above_one_aborts
    !
    subroutine test_kde_quantile_p_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_quantile_p_nan", &
            "pf_kde%quantile: p must lie in [0, 1]")
    end subroutine test_kde_quantile_p_nan_aborts
    !
    subroutine test_kde_curve_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_size", &
            "pf_kde%curve: x and f must have the same size")
    end subroutine test_kde_curve_size_aborts
    !
    subroutine test_kde_curve_reversed_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_reversed_range", &
            "pf_kde%curve: xmin must be below xmax")
    end subroutine test_kde_curve_reversed_range_aborts
    !
    subroutine test_kde_curve_negative_cut_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_negative_cut", &
            "pf_kde%curve: cut must not be negative")
    end subroutine test_kde_curve_negative_cut_aborts
    !
    subroutine test_kde_curve_nonfinite_end_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_nonfinite_end", &
            "pf_kde%curve: xmin and xmax must be finite")
    end subroutine test_kde_curve_nonfinite_end_aborts
    !
    !
    !> Every one of `pf_kde_grid`'s refusals, asserted by the exact text the guide page's table
    !> publishes; see `test/error_scenarios.f90` for each control and the call refused. The two in
    !> the family's words prove `%add` hands `parquet_stats`' checkers `what = "pf_kde_grid%add"`.
    subroutine test_kde_grid_ncells_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_ncells", &
            "pf_kde_grid%init: ncells must be positive")
    end subroutine test_kde_grid_ncells_aborts
    !
    subroutine test_kde_grid_range_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_range_nan", &
            "pf_kde_grid%init: xmin and xmax must be finite")
    end subroutine test_kde_grid_range_nan_aborts
    !
    subroutine test_kde_grid_range_reversed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_range_reversed", &
            "pf_kde_grid%init: xmin must be below xmax")
    end subroutine test_kde_grid_range_reversed_aborts
    !
    subroutine test_kde_grid_cell_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_cell_width", &
            "pf_kde_grid%init: the cell width (xmax - xmin)/ncells must be a finite, positive number")
    end subroutine test_kde_grid_cell_width_aborts
    !
    subroutine test_kde_grid_bandwidth_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_bandwidth", &
            "pf_kde_grid%init: bandwidth must be a finite, positive number")
    end subroutine test_kde_grid_bandwidth_aborts
    !
    !> **Skipped where the build flushes subnormals to zero** (ifx at `-O1` and above turns
    !! flush-to-zero and denormals-are-zero on by default). The scenario is a separate process
    !! built the same way, so its subnormal bandwidth reaches `%init` already collapsed to zero
    !! and the abort is the ZERO-bandwidth one asserted by `test_kde_grid_bandwidth_aborts`. The
    !! call still aborts; it is the message this test names that the build has taken away, and a
    !! test that accepted either message would no longer distinguish the two rules. This is the
    !! only place the loss of the subnormal rule is reported -- `test_bandwidth_admission`
    !! (`test_kde.f90`) drops its subnormal arm silently to keep its other one running.
    subroutine test_kde_grid_bandwidth_subnormal_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        if (subnormals_are_flushed()) then
            call skip_test(error, "needs gradual underflow: this build flushes subnormals to zero, " // &
                "so the scenario's subnormal bandwidth reaches pf_kde_grid%init as 0 and is refused " // &
                "by the zero-bandwidth rule instead")
            return
        end if
        call check_kde_scenario(error, "kde_grid_bandwidth_subnormal", &
            "pf_kde_grid%init: bandwidth must be a normal positive number whose kernel reach is finite")
    end subroutine test_kde_grid_bandwidth_subnormal_aborts
    !
    subroutine test_kde_grid_bandwidth_unusable_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_bandwidth_unusable", &
            "pf_kde_grid%init: bandwidth must be a normal positive number whose kernel reach is finite")
    end subroutine test_kde_grid_bandwidth_unusable_aborts
    !
    subroutine test_kde_grid_query_unfinished_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_query_unfinished", &
            "pf_kde_grid%density: the grid has not been finished; call %finish before querying it")
    end subroutine test_kde_grid_query_unfinished_aborts
    !
    subroutine test_kde_grid_method_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_method_token", &
            'pf_kde_grid%init: method must be "exact" or "binned"')
    end subroutine test_kde_grid_method_token_aborts
    !
    subroutine test_kde_grid_binned_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_binned_too_long", &
            'pf_kde_grid%init: method="binned" needs a transform longer than')
    end subroutine test_kde_grid_binned_too_long_aborts
    !
    subroutine test_kde_curve_binned_one_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_binned_one_point", &
            'pf_kde%curve: method="binned" needs at least two points')
    end subroutine test_kde_curve_binned_one_point_aborts
    !
    subroutine test_kde_fit_method_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_method_token", &
            'pf_kde%fit: method must be "exact" or "binned"')
    end subroutine test_kde_fit_method_token_aborts
    !
    subroutine test_kde_curve_method_on_binned_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_method_on_binned", &
            'pf_kde%curve: method= cannot be given for a fit made with method="binned"')
    end subroutine test_kde_curve_method_on_binned_aborts
    !
    subroutine test_kde_grid_add_after_finish_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_add_after_finish", &
            "pf_kde_grid%add: the grid has been finished; call %clear to accumulate into it again")
    end subroutine test_kde_grid_add_after_finish_aborts
    !
    subroutine test_kde_grid_merge_after_finish_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_after_finish", &
            "pf_kde_grid%merge: the grid has been finished; call %clear to accumulate into it again")
    end subroutine test_kde_grid_merge_after_finish_aborts
    !
    subroutine test_kde_grid_pilot_unfinished_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_pilot_unfinished", &
            "pf_kde_grid%init: pilot must be a finished grid; call %finish on it")
    end subroutine test_kde_grid_pilot_unfinished_aborts
    !
    subroutine test_kde_grid_unknown_kernel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_unknown_kernel", &
            "pf_kde_grid%init: kernel must be ""gaussian"", ""epanechnikov"", ""bspline"" or ""box""")
    end subroutine test_kde_grid_unknown_kernel_aborts
    !
    subroutine test_kde_grid_boundary_without_bound_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_boundary_without_bound", &
            "pf_kde_grid%init: boundary= needs lower= or upper=")
    end subroutine test_kde_grid_boundary_without_bound_aborts
    !
    subroutine test_kde_grid_outside_support_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_outside_support", &
            "pf_kde_grid%init: the grid's range must lie inside the support")
    end subroutine test_kde_grid_outside_support_aborts
    !
    subroutine test_kde_grid_linear_range_not_at_bound_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_linear_range_not_at_bound", &
            "pf_kde_grid%init: under boundary=""linear"" the grid's range must start at lower and end at upper")
    end subroutine test_kde_grid_linear_range_not_at_bound_aborts
    !
    subroutine test_kde_grid_linear_narrow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_linear_narrow", &
            "pf_kde_grid%init: under boundary=""linear"" the grid must be at least one kernel reach wide")
    end subroutine test_kde_grid_linear_narrow_aborts
    !
    subroutine test_kde_grid_add_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_add_uninitialised", &
            "pf_kde_grid%add: the grid has not been initialised")
    end subroutine test_kde_grid_add_uninitialised_aborts
    !
    subroutine test_kde_grid_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_threads_zero", &
            "pf_kde_grid%add: threads must be positive")
    end subroutine test_kde_grid_threads_zero_aborts
    !
    subroutine test_kde_grid_weights_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_weights_size", &
            "pf_kde_grid%add: weights has 3 elements but values has 4")
    end subroutine test_kde_grid_weights_size_aborts
    !
    subroutine test_kde_grid_real32_weights_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_real32_weights_size", &
            "pf_kde_grid%add: weights has 2 elements but values has 4")
    end subroutine test_kde_grid_real32_weights_size_aborts
    !
    subroutine test_kde_grid_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_negative_weight", &
            "pf_kde_grid%add: weight 2 is negative; weights must be finite and non-negative")
    end subroutine test_kde_grid_negative_weight_aborts
    !
    subroutine test_kde_grid_density_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_density_size", &
            "pf_kde_grid%density: f must have one element per cell")
    end subroutine test_kde_grid_density_size_aborts
    !
    subroutine test_kde_grid_density_x_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_density_x_size", &
            "pf_kde_grid%density: x must have one element per cell")
    end subroutine test_kde_grid_density_x_size_aborts
    !
    subroutine test_kde_grid_centres_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_centres_size", &
            "pf_kde_grid%grid: x must have one element per cell")
    end subroutine test_kde_grid_centres_size_aborts
    !
    subroutine test_kde_grid_pdf_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_pdf_size", &
            "pf_kde_grid%pdf: f must have one element per point of x")
    end subroutine test_kde_grid_pdf_size_aborts
    !
    subroutine test_kde_grid_cdf_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_cdf_size", &
            "pf_kde_grid%cdf: p must have one element per point of x")
    end subroutine test_kde_grid_cdf_size_aborts
    !
    subroutine test_kde_grid_quantile_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_quantile_size", &
            "pf_kde_grid%quantile: x must have one element per element of p")
    end subroutine test_kde_grid_quantile_size_aborts
    !
    subroutine test_kde_grid_quantile_p_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_quantile_p", &
            "pf_kde_grid%quantile: p must lie in [0, 1]")
    end subroutine test_kde_grid_quantile_p_aborts
    !
    subroutine test_kde_grid_query_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_query_uninitialised", &
            "pf_kde_grid%pdf: the grid has not been initialised")
    end subroutine test_kde_grid_query_uninitialised_aborts
    !
    subroutine test_kde_grid_accessor_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_accessor_uninitialised", &
            "pf_kde_grid%ncells: the grid has not been initialised")
    end subroutine test_kde_grid_accessor_uninitialised_aborts
    !
    subroutine test_kde_grid_merge_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_uninitialised", &
            "pf_kde_grid%merge: the other grid has not been initialised")
    end subroutine test_kde_grid_merge_uninitialised_aborts
    !
    subroutine test_kde_grid_merge_cells_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_cells", &
            "pf_kde_grid%merge: the two grids differ in cells")
    end subroutine test_kde_grid_merge_cells_aborts
    !
    subroutine test_kde_grid_merge_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_range", &
            "pf_kde_grid%merge: the two grids differ in range")
    end subroutine test_kde_grid_merge_range_aborts
    !
    subroutine test_kde_grid_merge_bandwidth_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_bandwidth", &
            "pf_kde_grid%merge: the two grids differ in bandwidth")
    end subroutine test_kde_grid_merge_bandwidth_aborts
    !
    subroutine test_kde_grid_merge_kernel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_kernel", &
            "pf_kde_grid%merge: the two grids differ in kernel")
    end subroutine test_kde_grid_merge_kernel_aborts
    !
    subroutine test_kde_grid_merge_support_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_support", &
            "pf_kde_grid%merge: the two grids differ in support")
    end subroutine test_kde_grid_merge_support_aborts
    !
    subroutine test_kde_grid_merge_boundary_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_boundary", &
            "pf_kde_grid%merge: the two grids differ in boundary")
    end subroutine test_kde_grid_merge_boundary_aborts
    !
    !> The eighth of `%merge`'s eight refusals. `control=` in full, not the bare marker: this
    !> scenario's control merges two grids that BOTH carry points, so the line it prints is the sum
    !> and a merge which passed the guard without moving the other grid's counts fails here.
    subroutine test_kde_grid_merge_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_method", &
            "pf_kde_grid%merge: the two grids differ in method", control="kde control merged: 7")
    end subroutine test_kde_grid_merge_method_aborts
    !
    !
    !> Every one of the adaptive kernel's refusals, in both forms, asserted by the exact text the
    !> guide page's table publishes; see `test/error_scenarios.f90` for each control and the call
    !> refused.
    subroutine test_kde_alpha_without_adaptive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_alpha_without_adaptive", &
            "pf_kde%fit: alpha=, bandwidth_max= and spread_max= need adaptive=.true.")
    end subroutine test_kde_alpha_without_adaptive_aborts
    !
    subroutine test_kde_bandwidth_max_without_adaptive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_max_without_adaptive", &
            "pf_kde%fit: alpha=, bandwidth_max= and spread_max= need adaptive=.true.")
    end subroutine test_kde_bandwidth_max_without_adaptive_aborts
    !
    subroutine test_kde_spread_max_without_adaptive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_spread_max_without_adaptive", &
            "pf_kde%fit: alpha=, bandwidth_max= and spread_max= need adaptive=.true.")
    end subroutine test_kde_spread_max_without_adaptive_aborts
    !
    subroutine test_kde_spread_max_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_spread_max_below_one", &
            "pf_kde%fit: spread_max must be a finite number of at least 1")
    end subroutine test_kde_spread_max_below_one_aborts
    !
    !> Both arms, because an advice nothing has been seen to emit is a comment: the default cap
    !> binding is said, and the same binding cap named by the caller is not.
    subroutine test_kde_spread_cap_advice(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "kde_spread_cap_advice_default", &
            expect_abort=.false., &
            failure_message="the default spread cap scenario was not expected to abort", &
            required_stderr="NOTE: pf_kde%fit: the default spread cap bound at")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "kde_spread_cap_advice_explicit", &
            expect_abort=.false., &
            failure_message="the explicit spread cap scenario was not expected to abort", &
            forbidden_text="the default spread cap bound at")
    end subroutine test_kde_spread_cap_advice
    !
    !> The zone advice names the value that silences it, and the value works.
    !>
    !> Four arms, because the claim is not that a number is printed but that THAT number is a
    !> remedy: the first reads both bounds out of the message, and the next two run the same fit at
    !> each of them and must say nothing. A rendering that rounded either bound up, or a formula
    !> that named the wrong unit, prints a plausible number and fails here rather than in a user's
    !> log. "still" is asserted too -- the caller of the first arm passed `bandwidth_max=`, and
    !> advice that reads as though they had not is what this change was made to remove.
    subroutine test_kde_zone_advice_values(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "kde_zone_advice_capped", &
            expect_abort=.false., &
            failure_message="the zone advice scenario was not expected to abort", &
            required_stderr="the zones still span about")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "kde_zone_advice_capped", &
            expect_abort=.false., &
            failure_message="the zone advice scenario was not expected to abort", &
            required_stderr="at bandwidth_max=0.286 or below")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "kde_zone_advice_capped", &
            expect_abort=.false., &
            failure_message="the zone advice scenario was not expected to abort", &
            required_stderr="spread_max=2.58 or below")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "kde_zone_advice_at_bound", &
            expect_abort=.false., &
            failure_message="the bounded zone scenario was not expected to abort", &
            forbidden_text="times the whole support")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "kde_zone_advice_at_spread", &
            expect_abort=.false., &
            failure_message="the bounded-spread zone scenario was not expected to abort", &
            forbidden_text="times the whole support")
    end subroutine test_kde_zone_advice_values
    !
    !> A fixed-bandwidth fit is told to narrow `bandwidth=`, never a cap it cannot pass.
    subroutine test_kde_zone_advice_fixed(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "kde_zone_advice_fixed", &
            expect_abort=.false., &
            failure_message="the fixed zone advice scenario was not expected to abort", &
            required_stderr="at bandwidth=0.286 or below")
        if (allocated(error)) return
        ! `%fit` refuses either cap without `adaptive=.true.`, so naming one here would be a
        ! remedy that aborts the call it is offered for.
        call check_scenario_exit_status_and_no_output(error, "kde_zone_advice_fixed", &
            expect_abort=.false., &
            failure_message="the fixed zone advice scenario was not expected to abort", &
            forbidden_text="spread_max=")
    end subroutine test_kde_zone_advice_fixed
    !
    subroutine test_kde_alpha_above_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_alpha_above_one", &
            "pf_kde%fit: alpha must lie in [0, 1]")
    end subroutine test_kde_alpha_above_one_aborts
    !
    subroutine test_kde_alpha_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_alpha_nan", &
            "pf_kde%fit: alpha must lie in [0, 1]")
    end subroutine test_kde_alpha_nan_aborts
    !
    subroutine test_kde_bandwidth_max_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_max_zero", &
            "pf_kde%fit: bandwidth_max must be a finite, positive number")
    end subroutine test_kde_bandwidth_max_zero_aborts
    !
    subroutine test_kde_bandwidths_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidths_size", &
            "pf_kde%bandwidths: h must have one element per retained point")
    end subroutine test_kde_bandwidths_size_aborts
    !
    subroutine test_kde_bandwidths_x_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidths_x_size", &
            "pf_kde%bandwidths: x must have one element per retained point")
    end subroutine test_kde_bandwidths_x_size_aborts
    !
    subroutine test_kde_bandwidth_at_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_bandwidth_at_size", &
            "pf_kde%bandwidth_at: h must have one element per point of x")
    end subroutine test_kde_bandwidth_at_size_aborts
    !
    subroutine test_kde_pilot_not_adaptive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_pilot_not_adaptive", &
            "pf_kde%pilot: the fit is not adaptive")
    end subroutine test_kde_pilot_not_adaptive_aborts
    !
    subroutine test_kde_grid_alpha_without_pilot_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_alpha_without_pilot", &
            "pf_kde_grid%init: alpha=, bandwidth_max= and spread_max= need pilot=")
    end subroutine test_kde_grid_alpha_without_pilot_aborts
    !
    subroutine test_kde_grid_bandwidth_max_without_pilot_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_bandwidth_max_without_pilot", &
            "pf_kde_grid%init: alpha=, bandwidth_max= and spread_max= need pilot=")
    end subroutine test_kde_grid_bandwidth_max_without_pilot_aborts
    !
    subroutine test_kde_grid_alpha_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_alpha_negative", &
            "pf_kde_grid%init: alpha must lie in [0, 1]")
    end subroutine test_kde_grid_alpha_negative_aborts
    !
    subroutine test_kde_grid_bandwidth_max_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_bandwidth_max_nan", &
            "pf_kde_grid%init: bandwidth_max must be a finite, positive number")
    end subroutine test_kde_grid_bandwidth_max_nan_aborts
    !
    subroutine test_kde_grid_pilot_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_pilot_uninitialised", &
            "pf_kde_grid%init: pilot must be an initialised grid")
    end subroutine test_kde_grid_pilot_uninitialised_aborts
    !
    subroutine test_kde_grid_merge_pilot_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_pilot", &
            "pf_kde_grid%merge: the two grids differ in pilot")
    end subroutine test_kde_grid_merge_pilot_aborts
    !
    subroutine test_kde_grid_merge_fixed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_fixed", &
            "pf_kde_grid%merge: the two grids differ in pilot")
    end subroutine test_kde_grid_merge_fixed_aborts
    !
    subroutine test_kde_grid_merge_alpha_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_merge_alpha", &
            "pf_kde_grid%merge: the two grids differ in pilot")
    end subroutine test_kde_grid_merge_alpha_aborts
    !
    subroutine test_kde_pdf_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_pdf_threads_zero", &
            "pf_kde%pdf: threads must be positive")
    end subroutine test_kde_pdf_threads_zero_aborts
    !
    subroutine test_kde_cdf_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_cdf_threads_zero", &
            "pf_kde%cdf: threads must be positive")
    end subroutine test_kde_cdf_threads_zero_aborts
    !
    subroutine test_kde_quantile_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_quantile_threads_zero", &
            "pf_kde%quantile: threads must be positive")
    end subroutine test_kde_quantile_threads_zero_aborts
    !
    subroutine test_kde_curve_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_threads_zero", &
            "pf_kde%curve: threads must be positive")
    end subroutine test_kde_curve_threads_zero_aborts
    !
    subroutine test_kde_sample_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_sample_threads_zero", &
            "pf_kde%sample: threads must be positive")
    end subroutine test_kde_sample_threads_zero_aborts
    !
    subroutine test_kde_sample_unfitted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_sample_unfitted", &
            "pf_kde%sample: the estimate has not been fitted")
    end subroutine test_kde_sample_unfitted_aborts
    !
    subroutine test_kde_grid_sample_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_sample_threads_zero", &
            "pf_kde_grid%sample: threads must be positive")
    end subroutine test_kde_grid_sample_threads_zero_aborts
    !
    subroutine test_kde_grid_sample_uninitialised_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_sample_uninitialised", &
            "pf_kde_grid%sample: the grid has not been initialised")
    end subroutine test_kde_grid_sample_uninitialised_aborts
    !
    subroutine test_kde_fit_logical_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_logical_column", &
            "pf_kde%fit: a column of kind PK_LOGICAL holds no numbers to place kernels at; " // &
            "the column must be int32, int64, float32 or float64")
    end subroutine test_kde_fit_logical_column_aborts
    !
    subroutine test_kde_fit_column_is_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_column_is_valid", &
            "pf_kde%fit: is_valid= cannot be given alongside a parquet_column")
    end subroutine test_kde_fit_column_is_valid_aborts
    !
    subroutine test_kde_grid_add_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_add_vector_column", &
            "pf_kde_grid%add: a column of kind PK_FLOAT64_VEC holds no numbers to place kernels at; " // &
            "the column must be int32, int64, float32 or float64")
    end subroutine test_kde_grid_add_vector_column_aborts
    !
    subroutine test_kde_grid_add_column_is_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_add_column_is_valid", &
            "pf_kde_grid%add: is_valid= cannot be given alongside a parquet_column")
    end subroutine test_kde_grid_add_column_is_valid_aborts
    !
    subroutine test_kde_isj_cells_not_pow2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_isj_cells_not_pow2", &
            "parquet_debug_set_kde_isj_cells: n must be a power of two from 16 to 1048576")
    end subroutine test_kde_isj_cells_not_pow2_aborts
    !
    !> `pilot=` transfers a smoothing measured on another sample, so a pilot describing a DIFFERENT
    !> estimate -- another kernel, another support, another correction -- would hand this fit
    !> numbers from a density it is not building. Each refusal is asserted by its own text.
    subroutine test_kde_fit_pilot_without_adaptive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_without_adaptive", &
            "pf_kde%fit: pilot= needs adaptive=.true.")
    end subroutine test_kde_fit_pilot_without_adaptive_aborts
    !
    subroutine test_kde_fit_pilot_unfinished_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_unfinished", &
            "pf_kde%fit: pilot must be a finished grid; call %finish on it")
    end subroutine test_kde_fit_pilot_unfinished_aborts
    !
    subroutine test_kde_fit_pilot_kernel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_kernel", &
            "pf_kde%fit: the pilot must use the same kernel as this fit")
    end subroutine test_kde_fit_pilot_kernel_aborts
    !
    subroutine test_kde_fit_pilot_boundary_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_boundary", &
            "pf_kde%fit: the pilot must use the same boundary correction as this fit")
    end subroutine test_kde_fit_pilot_boundary_aborts
    !
    subroutine test_kde_fit_pilot_support_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_support", &
            "pf_kde%fit: the pilot must have the same support as this fit")
    end subroutine test_kde_fit_pilot_support_aborts
    !
    subroutine test_kde_fit_pilot_support_side_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_support_side", &
            "pf_kde%fit: the pilot must have the same support as this fit")
    end subroutine test_kde_fit_pilot_support_side_aborts
    !
    subroutine test_kde_fit_pilot_upper_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_fit_pilot_upper", &
            "pf_kde%fit: the pilot must have the same support as this fit")
    end subroutine test_kde_fit_pilot_upper_aborts
    !
    subroutine test_kde_spread_max_not_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_spread_max_not_finite", &
            "pf_kde%fit: spread_max must be a finite number of at least 1")
    end subroutine test_kde_spread_max_not_finite_aborts
    !
    subroutine test_kde_curve_one_end_collapses_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_one_end_collapses", &
            "pf_kde%curve: xmin must be below xmax")
    end subroutine test_kde_curve_one_end_collapses_aborts
    !
    subroutine test_kde_curve_default_range_collapses_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_curve_default_range_collapses", &
            "pf_kde%curve: the default range has collapsed to one point")
    end subroutine test_kde_curve_default_range_collapses_aborts
    !
    subroutine test_kde_grid_cell_width_underflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_cell_width_underflow", &
            "pf_kde_grid%init: the cell width (xmax - xmin)/ncells must be a finite, positive number")
    end subroutine test_kde_grid_cell_width_underflow_aborts
    !
    subroutine test_kde_grid_linear_range_not_at_upper_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_linear_range_not_at_upper", &
            "the grid's range must start at lower and end at upper")
    end subroutine test_kde_grid_linear_range_not_at_upper_aborts
    !
    subroutine test_kde_grid_spread_max_not_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_spread_max_not_finite", &
            "pf_kde_grid%init: spread_max must be a finite number of at least 1")
    end subroutine test_kde_grid_spread_max_not_finite_aborts
    !
    subroutine test_kde_grid_spread_max_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_spread_max_below_one", &
            "pf_kde_grid%init: spread_max must be a finite number of at least 1")
    end subroutine test_kde_grid_spread_max_below_one_aborts
    !
    !> A `method = "binned"` fit whose narrowest kernel asks for more cells than the grid may hold
    !> says so and still answers: the count is clamped, not refused.
    subroutine test_kde_fit_cells_clamped_advice(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "kde_fit_cells_clamped_advice", &
            expect_abort=.false., &
            failure_message="the clamped-cells scenario was not expected to abort", &
            required_stderr="is held to 65536")
    end subroutine test_kde_fit_cells_clamped_advice
    !
    !> The zone advice renders a bound outside the range a fixed rendering reads back in the
    !> exponent form, with three exponent digits, whose spelling is the same under every compiler.
    subroutine test_kde_zone_advice_tiny_support(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "kde_zone_advice_tiny_support", &
            expect_abort=.false., &
            failure_message="the tiny-support zone advice scenario was not expected to abort", &
            required_stderr="at bandwidth=1.429E-007 or below")
    end subroutine test_kde_zone_advice_tiny_support
    !
    subroutine test_kde_grid_binned_transform_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_grid_binned_transform_length", &
            'pf_kde_grid%init: method="binned" needs a transform longer than 7 cells can carry')
    end subroutine test_kde_grid_binned_transform_length_aborts
    !
    !> R3 leaves the grid undefined quietly and says so. The message's CLASS is the assertion:
    !> a finding about the caller's DATA goes through `parquet_emit_warning`, so `"silent"` leaves
    !> it standing and only `"errors_only"` takes it. Advice would have gone at `"silent"` already,
    !> so the silent arm is what tells the two channels apart.
    !>
    !> Every arm also prints its `%n_overreach` count, so an arm with no WARNING cannot pass
    !> because R3 never fired.
    subroutine test_kde_overreach_warning_class(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "kde_overreach_warning", expect_abort=.false., &
            failure_message="R3 is a data condition and must not abort", &
            required_stderr="WARNING: pf_kde_grid%add:")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "kde_overreach_warning_silent", expect_abort=.false., &
            failure_message="R3 is a data condition and must not abort", &
            required_stderr="WARNING: pf_kde_grid%add:")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "kde_overreach_warning_errors_only", &
            expect_abort=.false., &
            failure_message="R3 is a data condition and must not abort", &
            forbidden_text="WARNING:")
    end subroutine test_kde_overreach_warning_class
    !
    subroutine test_kde_isj_cells_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_kde_scenario(error, "kde_isj_cells_out_of_range", &
            "parquet_debug_set_kde_isj_cells: n must be a power of two from 16 to 1048576")
    end subroutine test_kde_isj_cells_out_of_range_aborts
    !

end module test_numeric_errors
