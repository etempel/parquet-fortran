!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Abort-path tests for what the library computes over a table, driving the scenarios in
!> `error_scenarios_analysis.f90`: sorting, statistics, joins and random draws; spherical,
!> sky-coordinate, spatial and HEALPix geometry; the nested list/map/struct containers; and
!> the logging and TOML configuration layers.
!!
!! One of three modules split out of `test_errors.f90`, which keeps the subprocess-driving
!! machinery (`run_error_scenario`, the `check_scenario_*` helpers, `prime_error_scenarios`)
!! and the tests for `error_scenarios_io.f90`. The split is for COMPILE TIME: at about 23000
!! lines the one module took some 27 s under ifx, second only to `error_scenarios.f90` itself.
!! Each module's tests mirror one `error_scenarios_*` group, so a scenario and the test that
!! drives it stay in files with the same name.
module test_analysis_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use iso_fortran_env, only : real64
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_no_output, &
        check_scenario_exit_status_and_stderr, check_scenario_streams, file_contains, file_count_containing, &
        run_error_scenario
    !
    implicit none
    private
    public :: collect_tests_parquet_analysis_errors
    !
contains

    subroutine collect_tests_parquet_analysis_errors(testsuite)
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
            new_unittest("sorting: an out-of-range rank aborts", &
                test_sorting_nth_out_of_range_aborts), &
            new_unittest("sorting: a negative n aborts (n > size clamps)", &
                test_sorting_partial_negative_n_aborts), &
            new_unittest("sorting: a 0-100 quantile aborts", &
                test_sorting_quantile_out_of_range_aborts), &
            new_unittest("sorting: an unknown rounding token aborts", &
                test_sorting_quantile_bad_rounding_aborts), &
            new_unittest("sorting: an all-null quantile aborts", &
                test_sorting_quantile_all_null_aborts), &
            new_unittest("sorting: ok= does not excuse an out-of-range quantile", &
                test_sorting_quantile_ok_still_checks_range_aborts), &
            new_unittest("sorting: out-of-range permutation index aborts", &
                test_sorting_permute_index_out_of_range_aborts), &
            new_unittest("sorting: duplicated permutation index aborts", &
                test_sorting_permute_duplicate_index_aborts), &
            new_unittest("sorting: wrong-length permutation aborts", &
                test_sorting_permute_length_mismatch_aborts), &
            new_unittest("sorting: wrong-length is_valid mask aborts", &
                test_sorting_valid_length_mismatch_aborts), &
            new_unittest("sorting: keys of different row counts abort", &
                test_sorting_keys_row_count_mismatch_aborts), &
            new_unittest("sorting: an empty key list aborts", &
                test_sorting_keys_empty_aborts), &
            new_unittest("sorting: an empty key list aborts a partial argsort", &
                test_sorting_partial_keys_empty_aborts), &
            new_unittest("sorting: an empty key list aborts an int64 partial argsort", &
                test_sorting_partial_keys_empty_i64_aborts), &
            new_unittest("sorting: a vector column key aborts", &
                test_sorting_column_vector_aborts), &
            new_unittest("sorting: matching two columns of different kinds aborts", &
                test_sorting_match_kind_mismatch_aborts), &
            new_unittest("join: joining a table to itself aborts", &
                test_join_self_aborts), &
            new_unittest("join: two key columns of different kinds abort", &
                test_join_kind_mismatch_aborts), &
            new_unittest("join: require='m:1' on a duplicate right key aborts", &
                test_join_require_m1_aborts), &
            new_unittest("join: exceeding max_rows aborts", &
                test_join_max_rows_aborts), &
            new_unittest("join: max_rows= on the array key form, int32, aborts", &
                test_join_max_rows_arr_i32_aborts), &
            new_unittest("join: max_rows= on the array key form, int64, aborts", &
                test_join_max_rows_arr_i64_aborts), &
            new_unittest("join: max_rows= on the string key form, int32, aborts", &
                test_join_max_rows_str_i32_aborts), &
            new_unittest("join: max_rows= on the string key form, int64, aborts", &
                test_join_max_rows_str_i64_aborts), &
            new_unittest("join: require='1:m' on a duplicate left key aborts", &
                test_join_require_1m_aborts), &
            new_unittest("join: an unrecognized require= token aborts", &
                test_join_bad_require_aborts), &
            new_unittest("join: an unrecognized order= token aborts", &
                test_join_bad_order_aborts), &
            new_unittest("join: an unrecognized how= token aborts", &
                test_join_bad_how_aborts), &
            new_unittest("join: a mismatched other_on= length aborts", &
                test_join_other_on_size_aborts), &
            new_unittest("join: a join with no key aborts", &
                test_join_no_key_aborts), &
            new_unittest("join: columns= with how='anti' aborts", &
                test_join_columns_with_semi_aborts), &
            new_unittest("join: a container column here under how='outer' aborts", &
                test_join_left_container_outer_aborts), &
            new_unittest("join: a sort key given as a join key aborts", &
                test_join_key_direction_aborts), &
            new_unittest("join: the -name sort-key shorthand as a join key aborts", &
                test_join_key_direction_dash_aborts), &
            new_unittest("join: carrying a container column across aborts", &
                test_join_container_payload_aborts), &
            new_unittest("join: a container column as a join key aborts", &
                test_join_container_key_aborts), &
            new_unittest("join: an unknown columns= name aborts", &
                test_join_columns_unknown_aborts), &
            new_unittest("join: a suffixed incoming name that still clashes aborts", &
                test_join_suffix_clash_aborts), &
            new_unittest("join: a blank other_suffix= aborts", &
                test_join_blank_suffix_aborts), &
            new_unittest("join: reading a left column the join skipped aborts", &
                test_join_detached_column_aborts), &
            new_unittest("sorting: searching unsorted input aborts", &
                test_sorting_search_unsorted_aborts), &
            new_unittest("sorting: an over-long search target aborts", &
                test_sorting_search_target_too_long_aborts), &
            new_unittest("sorting: a bulk search answer array of the wrong length aborts", &
                test_sorting_search_many_answer_length_aborts), &
            new_unittest("sorting: an over-long bulk search target aborts", &
                test_sorting_search_many_target_too_long_aborts), &
            new_unittest("sorting: an unknown rank method aborts", &
                test_sorting_rank_bad_method_aborts), &
            new_unittest("sorting: an all-null minmax aborts", &
                test_sorting_minmax_all_null_aborts), &
            new_unittest("sorting: an all-null int32 array aborts pf_minmax", &
                test_sorting_minmax_all_null_i32_aborts), &
            new_unittest("sorting: an all-null int64 array aborts pf_minmax", &
                test_sorting_minmax_all_null_i64_aborts), &
            new_unittest("sorting: an all-null real32 array aborts pf_minmax", &
                test_sorting_minmax_all_null_f32_aborts), &
            new_unittest("sorting: an all-null character array aborts pf_minmax", &
                test_sorting_minmax_all_null_chr_aborts), &
            new_unittest("sorting: an all-null parquet_date array aborts pf_minmax", &
                test_sorting_minmax_all_null_date_aborts), &
            new_unittest("sorting: an all-null parquet_time array aborts pf_minmax", &
                test_sorting_minmax_all_null_time_aborts), &
            new_unittest("sorting: an all-null parquet_timestamp array aborts pf_minmax", &
                test_sorting_minmax_all_null_ts_aborts), &
            new_unittest("sorting: an all-null parquet_string_column array aborts pf_minmax", &
                test_sorting_minmax_all_null_strcol_aborts), &
            new_unittest("sorting: an all-null column aborts pf_argminmax", &
                test_sorting_argminmax_all_null_col_aborts), &
            new_unittest("sorting: an empty key list aborts an int64 sort", &
                test_sorting_keys_empty_i64_aborts), &
            new_unittest("sorting: an empty key list aborts pf_is_sorted", &
                test_sorting_is_sorted_keys_empty_aborts), &
            new_unittest("sorting: a column with no element kind aborts", &
                test_sorting_column_no_kind_aborts), &
            new_unittest("sorting: merging unsorted input aborts", &
                test_sorting_merge_unsorted_aborts), &
            new_unittest("a stream refuses to draw past its last addressable word", &
                test_random_stream_exhausted_aborts), &
            new_unittest("a stream refuses a rewind below position 1", &
                test_random_stream_rewind_below_one_aborts), &
            new_unittest("a gamma draw refuses a shape that is not strictly positive", &
                test_random_gamma_shape_not_positive_aborts), &
            new_unittest("a gamma draw refuses a NaN shape", &
                test_random_gamma_shape_nan_aborts), &
            new_unittest("a truncated normal refuses a sigma that is not strictly positive", &
                test_random_normal_truncated_sigma_not_positive_aborts), &
            new_unittest("a truncated normal refuses a NaN sigma", &
                test_random_normal_truncated_sigma_nan_aborts), &
            new_unittest("a truncated normal refuses reversed bounds rather than swapping them", &
                test_random_normal_truncated_bounds_reversed_aborts), &
            new_unittest("a truncated normal refuses a NaN bound", &
                test_random_normal_truncated_bounds_nan_aborts), &
            new_unittest("a truncated normal refuses an interval that collapses under its scale", &
                test_random_normal_truncated_bounds_collapse_aborts), &
            new_unittest("a poisson draw refuses a negative lambda", &
                test_random_poisson_lambda_negative_aborts), &
            new_unittest("a poisson draw refuses a NaN lambda", &
                test_random_poisson_lambda_nan_aborts), &
            new_unittest("a poisson draw refuses a lambda whose count could overflow int64", &
                test_random_poisson_lambda_too_large_aborts), &
            new_unittest("a poisson count that does not fit an int32 is refused, not narrowed", &
                test_random_poisson_int32_overflow_aborts), &
            new_unittest("a resample of an empty population aborts", &
                test_random_resample_empty_population_aborts), &
            new_unittest("an int32 resample of a wider-than-int32 population aborts", &
                test_random_resample_int32_too_narrow_aborts), &
            new_unittest("a subset larger than its population aborts", &
                test_random_subset_larger_than_population_aborts), &
            new_unittest("a subset of an empty population aborts", &
                test_random_subset_empty_population_aborts), &
            new_unittest("an int32 subset of a wider-than-int32 population aborts", &
                test_random_subset_int32_too_narrow_aborts), &
            new_unittest("metadata: a caller's own <KEY>.datatype wins, with a warning", &
                test_metadata_datatype_key_collision_warns), &
            new_unittest("metadata: no <KEY>.datatype collision means no warning", &
                test_metadata_datatype_no_collision_is_quiet), &
            new_unittest("querying an unbuilt spatial index aborts", test_spatial_query_before_build_aborts), &
            new_unittest("mismatched coordinate lengths abort", test_spatial_length_mismatch_aborts), &
            new_unittest("a radius hint of zero aborts", test_spatial_radius_not_positive_aborts), &
            new_unittest("a NaN coordinate is refused by %build", &
                test_spatial_build_nan_coord_aborts), &
            new_unittest("an infinite coordinate is refused by %build", &
                test_spatial_build_inf_coord_aborts), &
            new_unittest("a NaN radius= is refused by %build", &
                test_spatial_build_nan_radius_aborts), &
            new_unittest("a NaN ra is refused by %build_sky", &
                test_spatial_build_sky_nan_ra_aborts), &
            new_unittest("a NaN coordinate is refused by %rebuild", &
                test_spatial_rebuild_nan_coord_aborts), &
            new_unittest("a NaN query point is refused by %within", &
                test_spatial_query_nan_point_aborts), &
            new_unittest("a NaN axis endpoint is refused by %within_segment", &
                test_spatial_segment_nan_endpoint_aborts), &
            new_unittest("an axis whose squared length overflows is refused", &
                test_spatial_axis_length_overflows_aborts), &
            new_unittest("a NaN query point is refused by %nearest", &
                test_spatial_nearest_nan_point_aborts), &
            new_unittest("a NaN dec is refused by %within_sky", &
                test_spatial_sky_query_nan_dec_aborts), &
            new_unittest("a NaN in a per-point radius array is refused", &
                test_spatial_bulk_nan_radius_aborts), &
            new_unittest("a NaN inner radius is refused by a bulk query", &
                test_spatial_bulk_nan_inner_radius_aborts), &
            new_unittest("a NaN radius is refused by %rebuild_for", &
                test_spatial_rebuild_for_nan_radius_aborts), &
            new_unittest("box_lo= without box_hi= aborts", test_spatial_box_needs_both_aborts), &
            new_unittest("a periodic radius above half the box aborts", test_spatial_half_box_aborts), &
            new_unittest("querying a 2D index with a 3D point aborts", test_spatial_query_rank_aborts), &
            new_unittest("rebuilding a copy=.false. index aborts", test_spatial_rebuild_needs_copy_aborts), &
            new_unittest("a radius array of the wrong length aborts", test_spatial_bulk_radius_aborts), &
            new_unittest("an unknown combine= value aborts", test_spatial_pairs_bad_combine_aborts), &
            new_unittest("an int32 pair list over too many rows aborts", &
                test_spatial_pairs_int32_rows_aborts), &
            new_unittest("an int32 CSR over too long a neighbour list aborts", &
                test_spatial_csr_int32_offsets_aborts), &
            new_unittest("PF_LINK_SUM past 45 degrees on the sky aborts", &
                         test_spatial_sky_pairs_sum_too_large_aborts), &
            new_unittest("copy=.false. over a strided section aborts", test_spatial_copy_false_strided_aborts) &
            ]
        p2 = [ &
            new_unittest("threads= below one aborts", test_spatial_threads_below_one_aborts), &
            new_unittest("an axis query on a periodic index aborts", test_spatial_axis_periodic_aborts), &
            new_unittest("an nside that is not a power of two aborts a disc query", &
                         test_healpix_nside_not_power2_aborts), &
            new_unittest("an nside of zero aborts a disc query", test_healpix_nside_zero_aborts), &
            new_unittest("pf_healpix_grid%init refuses an invalid nside, scheme or frame", &
                         test_healpix_grid_init_aborts), &
            new_unittest("a grid that has not been built refuses every query", &
                         test_healpix_grid_unbuilt_aborts), &
            new_unittest("a grid value too large for an int32 aborts rather than wrapping", &
                         test_healpix_grid_int32_aborts), &
            new_unittest("an nside past the int32 pixel index aborts", &
                         test_healpix_nside_int32_aborts), &
            new_unittest("an nside past the module ceiling aborts", test_healpix_nside_int64_aborts), &
            new_unittest("a NaN disc radius aborts", test_healpix_radius_nan_aborts), &
            new_unittest("a negative disc radius aborts", test_healpix_radius_negative_aborts), &
            new_unittest("a zero-length disc centre aborts", test_healpix_vector_zero_aborts), &
            new_unittest("a NaN in the disc centre aborts", test_healpix_vector_nan_aborts), &
            new_unittest("an unknown scheme selector aborts", test_healpix_bad_scheme_aborts), &
            new_unittest("a listpix too small for the disc aborts", test_healpix_buffer_aborts), &
            new_unittest("a run buffer with the wrong number of rows aborts", &
                         test_healpix_runs_rows_abort), &
            new_unittest("query_disc_count names ITSELF when it rejects an nside", &
                         test_healpix_count_names_itself), &
            new_unittest("query_disc_alloc names ITSELF when it rejects a scheme", &
                         test_healpix_alloc_names_itself), &
            new_unittest("query_disc_max_count rejects an nside rather than answering -1", &
                         test_healpix_max_count_nside_aborts), &
            new_unittest("query_disc_max_count rejects a negative radius", &
                         test_healpix_max_count_radius_aborts), &
            new_unittest("a bulk form rejects an invalid nside", test_healpix_bulk_nside_aborts), &
            new_unittest("a bulk form rejects arrays that do not conform", &
                         test_healpix_bulk_size_aborts), &
            new_unittest("a bulk form rejects threads= below one", &
                         test_healpix_bulk_threads_aborts), &
            new_unittest("a bulk form rejects a vec array of the wrong shape", &
                         test_healpix_bulk_vec_shape_aborts), &
            new_unittest("a sky query on a Euclidean index aborts", test_spatial_sky_on_euclidean_aborts), &
            new_unittest("a Euclidean query on a sky index aborts", test_spatial_euclidean_on_sky_aborts), &
            new_unittest("a plain bulk sweep on a sky index aborts", test_spatial_sky_bulk_aborts), &
            new_unittest("a sky bulk sweep on a Euclidean index aborts", test_spatial_sky_bulk_euclidean_aborts), &
            new_unittest("a sky radius past a hemisphere aborts", test_spatial_sky_rsky_aborts), &
            new_unittest("a declination outside [-90, 90] aborts", test_spatial_sky_dec_aborts), &
            new_unittest("rebuild_for past a hemisphere aborts", &
                         test_spatial_rebuild_for_sky_too_large_aborts), &
            new_unittest("rebuilding a sky index aborts", test_spatial_sky_rebuild_aborts), &
            new_unittest("an unknown backend= aborts rather than defaulting", &
                         test_spatial_sky_bad_backend_aborts), &
            new_unittest("cell= on a HEALPix sky index aborts", &
                         test_spatial_sky_cell_healpix_aborts), &
            new_unittest("nside= without the HEALPix backend aborts", &
                         test_spatial_sky_nside_no_healpix_aborts), &
            new_unittest("an nside= that is not a power of two aborts", &
                         test_spatial_sky_nside_power2_aborts), &
            new_unittest("nside=0 aborts, which the power-of-two test alone would admit", &
                         test_spatial_sky_nside_zero_aborts), &
            new_unittest("an axis query before build aborts", test_spatial_axis_before_build_aborts), &
            new_unittest("a negative cone radius aborts", test_spatial_axis_radius_aborts), &
            new_unittest("an inner radius above the outer one aborts", test_spatial_annulus_inner_aborts), &
            new_unittest("a negative inner radius aborts", test_spatial_annulus_negative_aborts), &
            new_unittest("a mis-sized bulk inner radius aborts", test_spatial_bulk_inner_length_aborts), &
            new_unittest("a bulk inner radius above an outer one aborts", test_spatial_bulk_inner_exceeds_aborts), &
            new_unittest("an inner angular radius above the outer one aborts", test_spatial_sky_annulus_aborts), &
            new_unittest("a mis-shaped axis_point buffer aborts", test_spatial_axis_point_rank_aborts), &
            new_unittest("nearest with k below one aborts", test_spatial_nearest_k_aborts), &
            new_unittest("a periodic nearest past half the box aborts", test_spatial_nearest_periodic_aborts), &
            new_unittest("nearest_sky on a Euclidean index aborts", test_spatial_nearest_sky_euclidean_aborts), &
            new_unittest("kth_distance with k below one aborts", test_spatial_kth_k_low_aborts), &
            new_unittest("kth_distance with k at the size aborts", test_spatial_kth_k_high_aborts), &
            new_unittest("kth_distance on a sky index aborts", test_spatial_kth_on_sky_aborts), &
            new_unittest("kth_distance_sky on a Euclidean index aborts", test_spatial_kth_sky_euclidean_aborts), &
            new_unittest("mismatched component endpoints abort", test_spatial_components_length_aborts), &
            new_unittest("an out-of-range edge endpoint aborts", test_spatial_components_range_aborts), &
            new_unittest("min_size = 0 aborts", test_spatial_components_min_size_aborts), &
            new_unittest("a negative vertex count aborts", test_spatial_components_nvert_aborts), &
            new_unittest("the automatic-rebuild advice is said, and can be silenced", &
                test_spatial_rebuild_advice), &
            new_unittest("pairs_within_los on a sky index aborts", test_spatial_los_on_sky_aborts), &
            new_unittest("pairs_within_los on a 2D index aborts", test_spatial_los_on_2d_aborts), &
            new_unittest("pairs_within_los on a periodic index aborts", test_spatial_los_on_periodic_aborts), &
            new_unittest("a stored point on the observer aborts a line-of-sight sweep", &
                test_spatial_los_point_at_observer_aborts), &
            new_unittest("within_los from the observer itself aborts", &
                test_spatial_within_los_point_at_observer_aborts), &
            new_unittest("length lists of different lengths abort pairs_within_los", &
                test_spatial_los_length_mismatch_aborts), &
            new_unittest("three lengths for many points abort pairs_within_los", &
                test_spatial_los_lengths_not_per_point_aborts), &
            new_unittest("a negative parallel length aborts pairs_within_los", &
                test_spatial_los_negative_length_aborts), &
            new_unittest("an unknown combine= aborts pairs_within_los", test_spatial_los_bad_combine_aborts), &
            new_unittest("within_los without los_p on a los= index aborts", &
                test_spatial_within_los_needs_los_p_aborts), &
            new_unittest("within_los with los_p on an index without los= aborts", &
                test_spatial_within_los_los_p_refused_aborts), &
            new_unittest("a zero length aborts within_los", test_spatial_within_los_zero_length_aborts), &
            new_unittest("a two-coordinate point aborts within_los", test_spatial_within_los_rank_aborts), &
            new_unittest("a NaN los_p aborts within_los", test_spatial_within_los_los_p_nan_aborts), &
            new_unittest("a constant los= aborts %build", test_spatial_los_constant_aborts), &
            new_unittest("a NaN in los= aborts %build", test_spatial_los_nan_aborts), &
            new_unittest("a short los= aborts %build", test_spatial_los_length_aborts), &
            new_unittest("los= on a 2D build aborts", test_spatial_los_build_on_2d_aborts), &
            new_unittest("observer= on a periodic build aborts", test_spatial_los_build_on_periodic_aborts), &
            new_unittest("a two-coordinate observer= aborts", test_spatial_los_observer_size_aborts), &
            new_unittest("a NaN observer= aborts", test_spatial_los_observer_nan_aborts), &
            new_unittest("a point on the observer aborts a los= build", &
                test_spatial_los_build_point_at_observer_aborts), &
            new_unittest("rebuild without los= on a los= index aborts", test_spatial_rebuild_los_missing_aborts), &
            new_unittest("rebuild with los= on an index without one aborts", &
                test_spatial_rebuild_los_unexpected_aborts), &
            new_unittest("rebuild with a short los= aborts", test_spatial_rebuild_los_length_aborts), &
            new_unittest("a los= that is not a function of the distance warns at build", &
                test_spatial_los_not_a_function_warns), &
            new_unittest("a parallel window spanning the catalogue warns at the sweep", &
                test_spatial_los_window_spans_catalogue_warns), &
            new_unittest("reading a non-list column into a list column aborts", test_list_read_not_a_list_aborts), &
            new_unittest("a nested list payload reads", test_list_read_nested_payload_aborts), &
            new_unittest("a struct list payload reads", test_list_read_struct_payload_aborts), &
            new_unittest("a chunked list read refuses an active sort", test_list_chunk_refuses_sort_aborts), &
            new_unittest("a chunked list read checks its row group", test_list_chunk_row_group_out_of_range_aborts), &
            new_unittest("a partially chunk-read list column fails the completeness check", &
                test_list_chunk_incomplete_aborts), &
            new_unittest("a fully chunk-read list column passes it", test_list_chunk_complete_ok), &
            new_unittest("a whole-column list read marks the column read", test_list_read_marks_read), &
            new_unittest("adopt_rows rejects a final offset short of the payload", &
                test_list_adopt_rows_offset_mismatch_aborts), &
            new_unittest("adopt_rows rejects non-monotonic offsets", test_list_adopt_rows_not_monotonic_aborts), &
            new_unittest("adopt_rows rejects a vector-kind payload", test_list_adopt_rows_bad_payload_kind_aborts), &
            new_unittest("adopt_rows rejects a short row_valid mask", test_list_adopt_rows_mask_length_aborts), &
            new_unittest("writing a list column into a differently-typed declaration aborts", &
                test_list_write_type_mismatch_aborts), &
            new_unittest("writing an uninitialized list column aborts", test_list_write_uninitialized_aborts), &
            new_unittest("col_size on a list column is rejected", test_list_write_col_size_rejected_aborts), &
            new_unittest("col_size: auto on a list column is rejected", test_list_col_size_auto_rejected), &
            new_unittest("set_col_size(force) on a list column is rejected", &
                test_list_set_col_size_forced_rejected), &
            new_unittest("qc: min:/max: on a list column is rejected", test_list_write_qc_rejected_aborts), &
            new_unittest("a list token with no element type is rejected", test_list_write_bad_token_aborts), &
            new_unittest("a list token with an unknown element type is rejected", &
                test_list_write_unknown_element_aborts), &
            new_unittest("a protected list column with a null row aborts", &
                test_list_write_protected_row_null_aborts), &
            new_unittest("a protected list column with a null element aborts", &
                test_list_write_protected_element_null_aborts), &
            new_unittest("a map column of an unsupported value kind aborts", test_map_init_bad_kind_aborts), &
            new_unittest("a map %init with a container value names the adopt route", &
                test_map_init_nested_value_aborts), &
            new_unittest("writing a nested list is refused, naming the payload kind", &
                test_write_nested_list_aborts), &
            new_unittest("writing a NON-nested list still works", test_write_nested_list_control), &
            new_unittest("writing a struct with a container field names the field", &
                test_write_nested_struct_field_aborts), &
            new_unittest("a qc rule on a descent path is refused", test_qc_descent_path_aborts), &
            new_unittest("a qc rule on an ordinary leaf still works", test_qc_descent_path_control), &
            new_unittest("the bare list token is rejected", test_maml_list_bare_token_aborts), &
            new_unittest("row_index on an unassigned list handle aborts", test_list_row_index_unassigned), &
            new_unittest("row_index on an unassigned map handle aborts", test_map_row_index_unassigned), &
            new_unittest("row_index on a live handle still answers", test_list_row_index_live), &
            new_unittest("a filter on a descent path is refused", test_filter_descent_path_aborts), &
            new_unittest("a filter on an ordinary leaf still works", test_filter_descent_path_control), &
            new_unittest("a sort key on a descent path is refused", test_sort_key_descent_path_aborts), &
            new_unittest("a list %init with a container payload names the adopt route", &
                test_list_init_nested_payload_aborts), &
            new_unittest("a struct %init with a container field names the field and the route", &
                test_struct_init_nested_field_aborts), &
            new_unittest("a struct field that is itself a struct names the dotted-path route", &
                test_struct_read_nested_struct_field_aborts), &
            new_unittest("appending the wrong value type to a map aborts", test_map_append_wrong_kind_aborts), &
            new_unittest("appending mismatched key/value arrays to a map aborts", &
                test_map_append_length_mismatch_aborts), &
            new_unittest("reading a map value through the wrong specific aborts", test_map_get_wrong_kind_aborts), &
            new_unittest("a missing map key with no warn=/found= aborts", test_map_get_missing_key_aborts), &
            new_unittest("a missing map key with warn=/found= returns instead", test_map_get_missing_key_warn_ok), &
            new_unittest("occurrence below 1 on a map lookup aborts", test_map_get_occurrence_zero_aborts), &
            new_unittest("%get_at past the end of a map row aborts", test_map_get_at_out_of_range_aborts), &
            new_unittest("%view past the last map row aborts", test_map_view_out_of_range_aborts), &
            new_unittest("reading a non-map column into a map column aborts", test_map_read_not_a_map_aborts), &
            new_unittest("reading a map with non-string keys aborts", test_map_read_int_key_aborts), &
            new_unittest("reading a map with a container value works", test_map_read_nested_value_aborts), &
            new_unittest("writing an uninitialized map column aborts", test_map_write_uninitialized_aborts), &
            new_unittest("writing a map column into a differently-typed slot aborts", &
                test_map_write_type_mismatch_aborts), &
            new_unittest("col_size on a map column is rejected", test_map_col_size_rejected), &
            new_unittest("col_size: auto on a map column is rejected", test_map_col_size_auto_rejected), &
            new_unittest("qc min/max on a map column is rejected", test_map_qc_rejected), &
            new_unittest("a protected map column with a null row aborts", test_map_protected_row_null_aborts), &
            new_unittest("a protected map column with a null value aborts", test_map_protected_value_null_aborts), &
            new_unittest("a protected null-free map column writes normally", test_map_protected_ok), &
            new_unittest("a map column past the int32 entry ceiling is refused", test_map_entry_limit_aborts), &
            new_unittest("map %adopt_rows with a wrong final offset aborts", &
                test_map_adopt_rows_offset_mismatch_aborts), &
            new_unittest("map %adopt_rows with non-string keys aborts", test_map_adopt_rows_bad_key_kind_aborts), &
            new_unittest("a list row handle outliving its row aborts", test_list_stale_handle_aborts), &
            new_unittest("map %gather_rows naming a row that does not exist aborts", &
                test_map_gather_out_of_range_aborts), &
            new_unittest("map %append_from between different value kinds aborts", &
                test_map_append_from_kind_mismatch_aborts), &
            new_unittest("map %append_from handed a list aborts", test_map_append_from_not_a_map_aborts), &
            new_unittest("map %adopt_rows with an unsupported value kind aborts", &
                test_map_adopt_rows_bad_value_kind_aborts) &
            ]
        p3 = [ &
            new_unittest("map %adopt_rows with mismatched key and value counts aborts", &
                test_map_adopt_rows_length_mismatch_aborts), &
            new_unittest("map %adopt_rows with a short row_valid aborts", &
                test_map_adopt_rows_row_valid_length_aborts), &
            new_unittest("map %append_from onto a column with no value kind aborts", &
                test_map_append_from_uninitialized_aborts), &
            new_unittest("a map row handle outliving its row aborts", test_map_stale_handle_aborts), &
            new_unittest("map %append_row with a short is_valid aborts", &
                test_map_append_is_valid_length_aborts), &
            new_unittest("a lookup message truncates a very long key", &
                test_map_missing_key_preview_aborts), &
            new_unittest("a chunked map read under an active sort aborts", test_map_chunk_refuses_sort_aborts), &
            new_unittest("the bare map token is rejected", test_maml_map_bare_token_aborts), &
            new_unittest("a map token with a container value is rejected", test_maml_map_nested_value_aborts), &
            new_unittest("a list token with a container element in MAML text is rejected", &
                test_maml_text_list_bad_element_aborts), &
            new_unittest("a map token with an unknown value type in MAML text is rejected", &
                test_maml_text_map_bad_value_aborts), &
            new_unittest("a struct column with no fields aborts", test_struct_init_no_fields_aborts), &
            new_unittest("a struct column with duplicate field names aborts", &
                test_struct_init_duplicate_name_aborts), &
            new_unittest("a struct field name containing a dot aborts", test_struct_init_dotted_name_aborts), &
            new_unittest("a struct field of an unsupported kind aborts", test_struct_init_bad_kind_aborts), &
            new_unittest("append_from with a different field count aborts", &
                test_struct_append_from_field_count_aborts), &
            new_unittest("append_from with a different field kind aborts", &
                test_struct_append_from_field_kind_aborts), &
            new_unittest("append_from of a non-struct container aborts", &
                test_struct_append_from_not_struct_aborts), &
            new_unittest("gather_rows with a source index out of range aborts", &
                test_struct_gather_rows_out_of_range_aborts), &
            new_unittest("field_kind on an un-narrowed struct handle aborts", &
                test_struct_field_kind_not_narrowed_aborts), &
            new_unittest("nested on an un-narrowed struct handle aborts", &
                test_struct_nested_not_narrowed_aborts), &
            new_unittest("adopt_fields with a dotted field name aborts", &
                test_struct_adopt_dotted_name_aborts), &
            new_unittest("adopt_fields with a duplicate field name aborts", &
                test_struct_adopt_duplicate_name_aborts), &
            new_unittest("adopt_fields with an unsupported field kind aborts", &
                test_struct_adopt_bad_kind_aborts), &
            new_unittest("adopt_fields with fields of different lengths aborts", &
                test_struct_adopt_ragged_rows_aborts), &
            new_unittest("adopt_fields with a row_valid of the wrong length aborts", &
                test_struct_adopt_row_valid_length_aborts), &
            new_unittest("set_field on an uninitialized struct column aborts", &
                test_struct_set_field_uninitialized_aborts), &
            new_unittest("set_field with a field index out of range aborts", &
                test_struct_field_index_out_of_range_aborts), &
            new_unittest("an unassociated struct row handle aborts", &
                test_struct_handle_unassociated_aborts), &
            new_unittest("a stale struct row handle aborts", &
                test_struct_handle_stale_row_aborts), &
            new_unittest("reading a struct field through the wrong specific aborts", &
                test_struct_get_wrong_kind_aborts), &
            new_unittest("%get on an un-narrowed struct handle aborts", test_struct_get_not_narrowed_aborts), &
            new_unittest("%field on an unknown struct field name aborts", test_struct_field_unknown_aborts), &
            new_unittest("%field with warn= returns an invalid handle instead", test_struct_field_unknown_warn_ok), &
            new_unittest("writing an uninitialized struct column aborts", test_struct_write_uninitialized_aborts), &
            new_unittest("writing a struct column into a non-struct slot aborts", &
                test_struct_write_type_mismatch_aborts), &
            new_unittest("col_size on a struct column is rejected", test_struct_col_size_rejected), &
            new_unittest("col_size: auto on a struct column is rejected", test_struct_col_size_auto_rejected), &
            new_unittest("set_col_size on a struct column is rejected", test_struct_set_col_size_rejected), &
            new_unittest("qc min/max on a struct column is rejected", test_struct_qc_rejected), &
            new_unittest("a protected struct column with a null row aborts", &
                test_struct_protected_row_null_aborts), &
            new_unittest("a protected struct column with a null field aborts", &
                test_struct_protected_field_null_aborts), &
            new_unittest("a protected null-free struct column writes normally", test_struct_protected_ok), &
            new_unittest("a sub-microsecond timestamp in a struct field aborts on write", &
                test_struct_timestamp_precision_aborts), &
            new_unittest("a sub-microsecond time in a struct field aborts on write", &
                test_struct_time_precision_aborts), &
            new_unittest("microsecond-exact temporal struct fields write and read back", &
                test_struct_temporal_precision_ok), &
            new_unittest("reading a struct whose field is a list works", &
                test_struct_read_nested_field_aborts), &
            new_unittest("%view past the last struct row aborts", test_struct_view_out_of_range_aborts), &
            new_unittest("%set_field naming an undeclared field aborts", test_struct_set_field_unknown_aborts), &
            new_unittest("%set_field writing the wrong type aborts", test_struct_set_field_wrong_kind_aborts), &
            new_unittest("a protected null-free list column writes normally", test_list_write_protected_ok), &
            new_unittest("one over-long list row aborts", test_list_write_row_too_long_aborts), &
            new_unittest("a streamed row group over the element ceiling aborts", &
                test_list_write_chunk_too_many_elements_aborts), &
            new_unittest("an explicit chunk_size over the element ceiling aborts", &
                test_list_write_explicit_chunk_size_too_big_aborts), &
            new_unittest("a chunk_size within the element ceiling writes normally", &
                test_list_write_ceiling_ok), &
            new_unittest("the large_list write path round-trips", test_list_write_large_list_roundtrip), &
            new_unittest("a list column's large_utf8 string child round-trips", &
                test_list_write_large_string_child), &
            new_unittest("a streamed column that crosses the offset threshold keeps one width", &
                test_list_write_large_list_chunked), &
            new_unittest("parquet_logging refuses every configuration mistake", &
                test_logging_configuration_aborts), &
            new_unittest("an unbalanced context pop is caught by its frame token", &
                test_logging_pop_token_mismatch_aborts), &
            new_unittest("a write to a closed sink aborts, naming the path", &
                test_logging_write_to_closed_sink_aborts), &
            new_unittest("pf_log_fatal aborts after flushing", test_logging_fatal_aborts), &
            new_unittest("concurrent pf_log_fatal aborts exactly once", &
                         test_logging_fatal_omp_aborts_once), &
            new_unittest("an over-long log path is refused, not truncated", &
                test_logging_path_too_long_aborts), &
            new_unittest("a log file that cannot be opened aborts", &
                test_logging_file_cannot_open_aborts), &
            new_unittest("the default logger prints to stdout until it is configured", &
                test_logging_implicit_console), &
            new_unittest("configuring the default logger retires the implicit console", &
                test_logging_implicit_console_retires), &
            new_unittest("unset_level validates its name as set_level does", &
                test_logging_unset_level_empty_name), &
            new_unittest("an unbalanced name pop is caught by its frame token", &
                test_logging_pop_name_token_mismatch), &
            new_unittest("a name frame containing a dot is refused", &
                test_logging_push_name_with_dot), &
            new_unittest("a composed name over PF_LOG_MAX_NAME aborts, not truncates", &
                test_logging_composed_name_too_long), &
            new_unittest("the same logging calls made correctly do not abort", &
                test_logging_control_does_not_abort), &
            new_unittest("a mismatched is_valid is refused by pf_count_valid", &
                test_stats_is_valid_length_mismatch_aborts), &
            new_unittest("a mismatched weights array is refused by pf_count_valid", &
                test_stats_weights_length_mismatch_aborts), &
            new_unittest("a negative weight aborts, naming its index", &
                test_stats_negative_weight_aborts), &
            new_unittest("a NaN weight aborts, naming its index", &
                test_stats_nan_weight_aborts), &
            new_unittest("an infinite weight aborts, naming its index", &
                test_stats_infinite_weight_aborts), &
            new_unittest("an unrecognised weight_type aborts, listing both accepted tokens", &
                test_stats_unknown_weight_type_aborts), &
            new_unittest("querying a pf_stats that holds no population aborts", &
                test_stats_object_query_before_compute_aborts), &
            new_unittest("merging across a retain mismatch aborts", &
                test_stats_object_merge_retain_mismatch_aborts), &
            new_unittest("merging across a weight_type mismatch aborts", &
                test_stats_object_merge_weight_type_mismatch_aborts), &
            new_unittest("merging across a skipnan mismatch aborts", &
                test_stats_object_merge_skipnan_mismatch_aborts), &
            new_unittest("%gmean on a streaming pf_stats aborts", &
                test_stats_object_gmean_without_retain_aborts), &
            new_unittest("%hmean on a streaming pf_stats aborts", &
                test_stats_object_hmean_without_retain_aborts), &
            new_unittest("merging a pf_stats that holds no population aborts", &
                test_stats_object_merge_uncomputed_source_aborts), &
            new_unittest("a string column has no numeric statistics and aborts naming its kind", &
                test_stats_column_string_kind_aborts), &
            new_unittest("a vector column aborts naming its width, not its kind", &
                test_stats_column_vector_width_aborts), &
            new_unittest("is_valid= alongside a parquet_column aborts", &
                test_stats_column_is_valid_conflict_aborts), &
            new_unittest("a probability outside [0, 1] aborts", &
                test_stats_quantile_bad_probability_aborts), &
            new_unittest("an unrecognised quantile method aborts listing all six", &
                test_stats_quantile_bad_method_aborts), &
            new_unittest("pf_quantiles with mismatched probs and out aborts", &
                test_stats_quantiles_size_mismatch_aborts), &
            new_unittest("pf_trim_mean with prop outside [0, 0.5) aborts", &
                test_stats_trim_mean_bad_prop_aborts), &
            new_unittest("a NaN score aborts, unlike a NaN in the population", &
                test_stats_score_not_finite_aborts), &
            new_unittest("an unrecognised percentile-of-score kind aborts", &
                test_stats_score_bad_kind_aborts), &
            new_unittest("an order statistic on a streaming accumulator aborts", &
                test_stats_median_on_streaming_aborts), &
            new_unittest("an unrecognised MAD scale token aborts", &
                test_stats_mad_bad_scale_aborts), &
            new_unittest("a NaN MAD centre aborts, unlike a NaN in the population", &
                test_stats_mad_center_not_finite_aborts), &
            new_unittest("is_valid= beside a string column aborts in pf_mode", &
                test_stats_mode_string_column_is_valid_aborts), &
            new_unittest("an unrecognised correlation method aborts", &
                test_stats_corr_bad_method_aborts), &
            new_unittest("an unrecognised plotting-position method aborts", &
                test_stats_normal_scores_bad_method_aborts), &
            new_unittest("pf_probit_fit reaches the same plotting-position resolver", &
                test_stats_probit_fit_bad_method_aborts), &
            new_unittest("a probit-scale prob outside (0, 0.5) aborts", &
                test_stats_probit_scale_bad_prob_aborts), &
            new_unittest("a negative weight aborts pf_probit_mean", &
                test_stats_probit_mean_bad_weight_aborts), &
            new_unittest("%probit_mean on a streaming pf_stats aborts", &
                test_stats_object_probit_mean_without_retain_aborts), &
            new_unittest("a weighted Spearman correlation aborts", &
                test_stats_spearman_with_weights_aborts), &
            new_unittest("two samples of different size abort", &
                test_stats_pair_size_mismatch_aborts), &
            new_unittest("an unrecognised sigma-clip cenfunc aborts", &
                test_stats_clip_bad_cenfunc_aborts), &
            new_unittest("a negative sigma-clip width aborts", &
                test_stats_clip_bad_sigma_aborts), &
            new_unittest("a short pf_zscore output array aborts", &
                test_stats_zscore_size_mismatch_aborts), &
            new_unittest("a short cumulative output array aborts", &
                test_stats_cumsum_size_mismatch_aborts), &
            new_unittest("a short cumulative out_valid mask aborts", &
                test_stats_cum_out_valid_mismatch_aborts), &
            new_unittest("fewer than two bin edges aborts", &
                test_stats_edges_too_few_aborts) &
            ]
        p4 = [ &
            new_unittest("bin edges that are not strictly increasing abort", &
                test_stats_edges_not_increasing_aborts), &
            new_unittest("a NaN bin edge aborts, naming the NaN rather than the ordering", &
                test_stats_edges_nan_aborts), &
            new_unittest("one histogram count per EDGE rather than per bin aborts", &
                test_stats_histogram_counts_size_aborts), &
            new_unittest("asking pf_bin_edges for zero bins aborts", &
                test_stats_bin_edges_nbins_aborts), &
            new_unittest("one pf_bin_edges boundary per bin rather than one more aborts", &
                test_stats_bin_edges_size_aborts), &
            new_unittest("a wrong-typed config value aborts rather than leaving it undefined", &
                test_toml_wrong_type_aborts), &
            new_unittest("a required config key the file omits aborts", &
                test_toml_missing_key_aborts), &
            new_unittest("a config integer beyond int32 is reported, not wrapped", &
                test_toml_int_overflow_aborts), &
            new_unittest("a config list of the wrong length aborts in both directions", &
                test_toml_array_length_aborts), &
            new_unittest("a config string too long for its slot aborts rather than clipping", &
                test_toml_string_too_long_aborts), &
            new_unittest("an absent config list on a bare call aborts", &
                test_toml_array_required_aborts), &
            new_unittest("a required config section the file omits aborts", &
                test_toml_missing_section_aborts), &
            new_unittest("counting the entries of a plain config table aborts", &
                test_toml_section_not_array_aborts), &
            new_unittest("a config entry index out of range aborts", &
                test_toml_entry_out_of_range_aborts), &
            new_unittest("a retired config key that is still set aborts", &
                test_toml_retired_key_aborts), &
            new_unittest("a config key nobody read aborts under the default severity", &
                test_toml_unknown_key_aborts), &
            new_unittest("a config section nobody opened aborts under check_all", &
                test_toml_unknown_section_aborts), &
            new_unittest("an unrecognised log level name in a config file aborts", &
                test_toml_bad_level_aborts), &
            new_unittest("reading a config section that was not found aborts", &
                test_toml_closed_handle_aborts), &
            new_unittest("a rank-1 default of the wrong size aborts", &
                test_toml_default_size_aborts), &
            new_unittest("get_strings count aborts on a list that is too short", &
                test_toml_strings_count_short_aborts), &
            new_unittest("get_strings count aborts on a list that is too long", &
                test_toml_strings_count_long_aborts), &
            new_unittest("pf_toml_dump on a section handle rather than the document aborts", &
                test_toml_dump_not_owner_aborts), &
            new_unittest("pf_toml_set on a key that exists aborts, naming pf_toml_update", &
                test_toml_set_existing_aborts), &
            new_unittest("pf_toml_update on a key that does not exist aborts, naming pf_toml_set", &
                test_toml_update_missing_aborts), &
            new_unittest("closing a config section handle rather than the document aborts", &
                test_toml_close_not_owner_aborts), &
            new_unittest("config text that is not TOML aborts without status=", &
                test_toml_parse_error_aborts), &
            new_unittest("a config file that cannot be opened aborts without status=", &
                test_toml_open_error_aborts), &
            new_unittest("a config string-list index out of range aborts", &
                test_toml_strings_range_aborts), &
            new_unittest("an over-long config key name aborts rather than truncating", &
                test_toml_key_too_long_aborts), &
            new_unittest("an unknown parquet_toml severity aborts", &
                test_toml_bad_severity_aborts), &
            new_unittest("reading a config scalar as a list aborts", &
                test_toml_value_not_list_aborts), &
            new_unittest("opening a config value as a section aborts", &
                test_toml_name_not_a_section_aborts), &
            new_unittest("an over-long config section path aborts rather than truncating", &
                test_toml_path_too_long_aborts), &
            new_unittest("a fatal parquet_toml report aborts", &
                test_toml_report_fatal_aborts), &
            new_unittest("a config list of the wrong element type aborts, one text per type", &
                test_toml_list_element_type_aborts), &
            new_unittest("pf_toml_require names every missing key and then aborts", &
                test_toml_require_missing_aborts), &
            new_unittest("reading from a handle that was never opened aborts", &
                test_toml_handle_never_opened_aborts), &
            new_unittest("pf_toml_save from a section handle aborts", &
                test_toml_save_not_owner_aborts), &
            new_unittest("a config file that cannot be written aborts", &
                test_toml_save_write_error_aborts), &
            new_unittest("an over-long default config string aborts rather than clipping", &
                test_toml_default_string_too_long_aborts), &
            new_unittest("a required [[name]] the file has no entries of aborts", &
                test_toml_no_entries_at_all_aborts), &
            new_unittest("every legal parquet_toml path completes", &
                test_toml_control_completes), &
            new_unittest("a sub-millisecond value under map[timestamp[ms]] is refused at the declared unit", &
                test_map_write_millisecond_precision_aborts), &
            new_unittest("an all-NaN qc column reports NaN as its observed range, and completes", &
                test_qc_all_nan_range_reports_nan), &
            new_unittest("a join pair count that would wrap a 64-bit integer aborts", &
                test_join_pair_count_overflow_aborts), &
            new_unittest("join: require='m:1' on a duplicate right key aborts on the hash engine", &
                test_join_require_m1_hash_aborts), &
            new_unittest("join: require='1:m' on a duplicate left key aborts on the hash engine", &
                test_join_require_1m_hash_aborts), &
            new_unittest("join: exceeding max_rows aborts on the hash engine", &
                test_join_max_rows_hash_aborts), &
            new_unittest("join: max_rows= on the array key form, int32, aborts on the hash engine", &
                test_join_max_rows_arr_i32_hash_aborts), &
            new_unittest("join: max_rows= on the array key form, int64, aborts on the hash engine", &
                test_join_max_rows_arr_i64_hash_aborts), &
            new_unittest("join: max_rows= on the string key form, int32, aborts on the hash engine", &
                test_join_max_rows_str_i32_hash_aborts), &
            new_unittest("join: max_rows= on the string key form, int64, aborts on the hash engine", &
                test_join_max_rows_str_i64_hash_aborts), &
            new_unittest("join: require='m:1' aborts from the sort engine's check on a team", &
                test_join_require_m1_threaded_aborts), &
            new_unittest("join: require='1:m' aborts from the sort engine's check on a team", &
                test_join_require_1m_threaded_aborts), &
            new_unittest("a layout template over PF_LOG_MAX_FORMAT is refused", &
                test_logging_template_too_long_aborts), &
            new_unittest("a layout template with an unclosed brace is refused", &
                test_logging_template_unclosed_brace_aborts), &
            new_unittest("a layout template with too many steps is refused", &
                test_logging_template_too_many_ops_aborts), &
            new_unittest("add_console refuses a stream that is neither console", &
                test_logging_add_console_bad_stream_aborts), &
            new_unittest("add_unit refuses a unit nothing has opened", &
                test_logging_add_unit_not_connected_aborts), &
            new_unittest("add_unit refuses a read-only unit", &
                test_logging_add_unit_not_writable_aborts), &
            new_unittest("add_unit refuses an unformatted unit", &
                test_logging_add_unit_unformatted_aborts), &
            new_unittest("set_level refuses an over-long name=", &
                test_logging_set_level_name_too_long_aborts), &
            new_unittest("a logger refuses more overrides than it holds", &
                test_logging_too_many_name_rules_aborts), &
            new_unittest("unset_level refuses an over-long name=", &
                test_logging_unset_level_name_too_long_aborts), &
            new_unittest("set_color refuses a policy outside the three constants", &
                test_logging_set_color_bad_policy_aborts), &
            new_unittest("a sink= id no sink has is refused", &
                test_logging_bad_sink_id_aborts), &
            new_unittest("set_name refuses a name over PF_LOG_MAX_NAME", &
                test_logging_set_name_too_long_aborts), &
            new_unittest("set_rank refuses a negative rank that is not the sentinel", &
                test_logging_set_rank_negative_aborts), &
            new_unittest("set_thread_mode refuses an unknown mode", &
                test_logging_thread_mode_bad_aborts), &
            new_unittest("set_thread_mode refuses slot_bytes below the floor", &
                test_logging_thread_mode_slot_too_small_aborts), &
            new_unittest("the base context is bounded and aborts, unlike a frame", &
                test_logging_set_context_too_long_aborts), &
            new_unittest("an empty name frame is refused", &
                test_logging_push_name_empty_aborts), &
            new_unittest("the name stack's depth bound aborts, not saturates", &
                test_logging_push_name_too_deep_aborts), &
            new_unittest("the name stack's length bound is separate from its depth", &
                test_logging_push_name_too_long_aborts), &
            new_unittest("an environment level naming no level aborts", &
                test_logging_env_bad_level_aborts), &
            new_unittest("the unconfigured default logger describes itself", &
                test_logging_implicit_print), &
            new_unittest("one pf_bucketize code per BIN rather than per element aborts", &
                test_stats_bucketize_codes_size_aborts), &
            new_unittest("a short pf_zscore out_valid aborts", &
                test_stats_zscore_out_valid_size_aborts), &
            new_unittest("a short pf_normal_scores s aborts", &
                test_stats_normal_scores_size_aborts), &
            new_unittest("a short pf_normal_scores out_valid aborts", &
                test_stats_normal_scores_out_valid_size_aborts), &
            new_unittest("a short sigma-clip keep mask aborts", &
                test_stats_sigma_clip_keep_size_aborts), &
            new_unittest("an order statistic off an accumulator that was never computed aborts", &
                test_stats_obj_order_without_population_aborts), &
            new_unittest("%quantiles with fewer outputs than probabilities aborts", &
                test_stats_obj_quantiles_out_size_aborts), &
            new_unittest("%trim_mean trimming half from each tail aborts", &
                test_stats_obj_trim_mean_prop_aborts), &
            new_unittest("%percentile_of_score refuses a non-finite score", &
                test_stats_obj_percentile_score_non_finite_aborts), &
            new_unittest("%print with no unit= resolves the message stream", &
                test_stats_print_default_stream), &
            new_unittest("a sky query on an unbuilt index aborts", &
                test_spatial_sky_query_before_build_aborts), &
            new_unittest("a NaN angular search radius aborts", &
                test_spatial_sky_radius_nan_aborts), &
            new_unittest("a NaN inner angular radius aborts", &
                test_spatial_sky_inner_radius_nan_aborts), &
            new_unittest("a NaN in an angular radius list aborts", &
                test_spatial_sky_radii_vector_nan_aborts), &
            new_unittest("nearest_sky on an unbuilt index aborts", &
                test_spatial_nearest_sky_before_build_aborts), &
            new_unittest("a NaN query radius aborts", &
                test_spatial_query_radius_nan_aborts), &
            new_unittest("a periodic query radius above half the box aborts", &
                test_spatial_query_radius_half_box_aborts), &
            new_unittest("build_sky with ra and dec of different lengths aborts", &
                test_spatial_build_sky_length_mismatch_aborts), &
            new_unittest("build_sky with an empty radius_deg list aborts", &
                test_spatial_build_sky_no_radius_aborts), &
            new_unittest("build_sky with a radius_deg of zero aborts", &
                test_spatial_build_sky_radius_not_positive_aborts), &
            new_unittest("rebuild on an unbuilt index aborts", &
                test_spatial_rebuild_before_build_aborts), &
            new_unittest("rebuild dropping z aborts", &
                test_spatial_rebuild_z_rank_aborts), &
            new_unittest("rebuild with x and y of different lengths aborts", &
                test_spatial_rebuild_length_mismatch_aborts) &
            ]
        p5 = [ &
            new_unittest("rebuild moving a point onto the observer aborts", &
                test_spatial_rebuild_at_observer_aborts), &
            new_unittest("rebuild_for on an unbuilt index aborts", &
                test_spatial_rebuild_for_before_build_aborts), &
            new_unittest("rebuild_for above half a periodic box aborts", &
                test_spatial_rebuild_for_half_box_aborts), &
            new_unittest("a box with fewer entries than coordinates aborts", &
                test_spatial_build_box_rank_aborts), &
            new_unittest("a box with zero extent on an axis aborts", &
                test_spatial_build_box_not_strict_aborts), &
            new_unittest("copy=.false. with a strided z aborts", &
                test_spatial_copy_false_z_strided_aborts), &
            new_unittest("a bulk sweep on an unbuilt index aborts", &
                test_spatial_bulk_before_build_aborts), &
            new_unittest("a line-of-sight sweep on an unbuilt index aborts", &
                test_spatial_los_before_build_aborts), &
            new_unittest("a negative transverse length aborts", &
                test_spatial_los_bperp_negative_aborts), &
            new_unittest("kth_distance on an unbuilt index aborts", &
                test_spatial_kth_before_build_aborts), &
            new_unittest("int32 component endpoints of different lengths abort", &
                test_spatial_components_i32_length_aborts), &
            new_unittest("more vertices than an int32 component answer can name aborts", &
                test_spatial_components_nvert_int32_aborts), &
            new_unittest("an int32 within buffer over the row ceiling aborts", &
                test_spatial_within_int32_ceiling_aborts), &
            new_unittest("an int32 axis buffer over the row ceiling aborts", &
                test_spatial_axis_int32_ceiling_aborts), &
            new_unittest("an int32 nearest buffer over the row ceiling aborts", &
                test_spatial_nearest_int32_ceiling_aborts), &
            new_unittest("a cell count over the int32 ceiling aborts", &
                test_spatial_grid_int32_ceiling_aborts), &
            new_unittest("the work hook with no radius aborts", &
                test_spatial_debug_work_no_radius_aborts), &
            new_unittest("a coarsened nside= warns and carries on", &
                test_spatial_nside_coarsened_warns), &
            new_unittest("a sky index re-tuned by a distant radius warns in degrees", &
                test_spatial_sky_rebuild_warns), &
            new_unittest("a disc refuses a negative angular radius", &
                test_random_disc_radius_negative_aborts), &
            new_unittest("a disc refuses a NaN angular radius", &
                test_random_disc_radius_nan_aborts), &
            new_unittest("a disc refuses the zero vector as its centre", &
                test_random_disc_centre_zero_aborts), &
            new_unittest("a disc refuses a centre with a NaN component", &
                test_random_disc_centre_nan_aborts), &
            new_unittest("a disc refuses an inner radius beyond its outer", &
                test_random_disc_inner_exceeds_radius_aborts), &
            new_unittest("a disc refuses an inner radius past the half turn", &
                test_random_disc_inner_above_half_turn_aborts), &
            new_unittest("a disc refuses a NaN inner radius", &
                test_random_disc_inner_nan_aborts), &
            new_unittest("a sky disc refuses a declination outside [-90, 90]", &
                test_random_disc_radec_dec_out_of_range_aborts), &
            new_unittest("a sky disc refuses a NaN right ascension", &
                test_random_disc_radec_centre_nan_aborts), &
            new_unittest("a sky disc refuses a negative radius_deg, under that name", &
                test_random_disc_radec_radius_negative_aborts), &
            new_unittest("a ball refuses a negative radius", &
                test_random_ball_radius_negative_aborts), &
            new_unittest("a shell refuses an inner radius beyond its outer", &
                test_random_ball_inner_exceeds_radius_aborts), &
            new_unittest("a vMF draw refuses a negative concentration", &
                test_random_vmf_kappa_negative_aborts), &
            new_unittest("a vMF draw refuses a NaN concentration", &
                test_random_vmf_kappa_nan_aborts), &
            new_unittest("a vMF draw refuses the zero vector as its mean direction", &
                test_random_vmf_mu_zero_aborts), &
            new_unittest("a sky vMF draw refuses a width of zero", &
                test_random_vmf_radec_sigma_not_positive_aborts), &
            new_unittest("a sky vMF draw refuses a NaN width", &
                test_random_vmf_radec_sigma_nan_aborts), &
            new_unittest("a direction fill refuses an array without three rows", &
                test_random_fill_direction_bad_shape_aborts), &
            new_unittest("an RA/Dec fill refuses arrays of different sizes", &
                test_random_fill_radec_size_mismatch_aborts), &
            new_unittest("a sphere draw refuses draw 2**62 + 1, which would read another word space", &
                test_random_sphere_draw_beyond_2p62_aborts), &
            new_unittest("a direction fill refuses a last draw beyond 2**62", &
                test_random_fill_direction_draw_beyond_2p62_aborts), &
            new_unittest("a stream's disc names itself when it refuses an inverted ring", &
                test_random_stream_disc_inner_exceeds_radius_aborts), &
            new_unittest("a disc cap refuses %at before %prepare, and draws after it", &
                test_random_disc_cap_unprepared_aborts), &
            new_unittest("a sky polygon refuses fewer than three vertices", &
                test_sphere_polygon_too_few_vertices_aborts), &
            new_unittest("a sky polygon refuses ra and dec of different sizes", &
                test_sphere_polygon_size_mismatch_aborts), &
            new_unittest("a sky polygon refuses an infinite vertex", &
                test_sphere_polygon_nonfinite_vertex_aborts), &
            new_unittest("a sky polygon refuses a NaN vertex, and names it", &
                test_sphere_polygon_nan_vertex_aborts), &
            new_unittest("a sky polygon names a vertex beyond the fixed-point range in exponent form", &
                test_sphere_polygon_extreme_vertex_aborts), &
            new_unittest("strict= refuses a band written the short way round", &
                test_sphere_polygon_strict_short_way_aborts), &
            new_unittest("a sky polygon refuses a declination outside [-90, 90]", &
                test_sphere_polygon_dec_out_of_range_aborts), &
            new_unittest("a sky polygon refuses an unknown edge rule", &
                test_sphere_polygon_bad_edge_rule_aborts), &
            new_unittest("a chart polygon refuses an RA extent above 360", &
                test_sphere_polygon_ra_extent_over_360_aborts), &
            new_unittest("a great-circle polygon refuses a vertex 89.9 degrees or more from the mean", &
                test_sphere_polygon_not_in_hemisphere_aborts), &
            new_unittest("a great-circle polygon refuses vertices whose directions cancel", &
                test_sphere_polygon_vertices_cancel_aborts), &
            new_unittest("a sky polygon refuses zero area", &
                test_sphere_polygon_zero_area_aborts), &
            new_unittest("a sky polygon refuses to cover less than 1e-3 of its bounding box", &
                test_sphere_polygon_below_acceptance_floor_aborts), &
            new_unittest("a sky polygon refuses %init twice without %clear", &
                test_sphere_polygon_init_twice_aborts), &
            new_unittest("a sky polygon's %contains refuses before %init", &
                test_sphere_polygon_contains_before_init_aborts), &
            new_unittest("a sky polygon's %area refuses before %init", &
                test_sphere_polygon_area_before_init_aborts), &
            new_unittest("a sky polygon's %area_deg2 refuses before %init", &
                test_sphere_polygon_area_deg2_before_init_aborts), &
            new_unittest("a sky polygon's %acceptance refuses before %init", &
                test_sphere_polygon_acceptance_before_init_aborts), &
            new_unittest("a sky polygon's %bounds refuses before %init", &
                test_sphere_polygon_bounds_before_init_aborts), &
            new_unittest("a sky polygon's %random_at refuses before %init", &
                test_sphere_polygon_random_before_init_aborts), &
            new_unittest("a sky polygon's %random_fill refuses before %init", &
                test_sphere_polygon_fill_before_init_aborts), &
            new_unittest("a sky polygon's %random_fill refuses arrays of different sizes", &
                test_sphere_polygon_fill_size_mismatch_aborts), &
            new_unittest("a sky polygon's %random_fill refuses a last draw beyond huge(int64)", &
                test_sphere_polygon_fill_draw_overflow_aborts), &
            new_unittest("a sky polygon draw aborts after 100000 rejected candidates", &
                test_sphere_polygon_candidate_cap_reached_aborts), &
            new_unittest("a sky polygon's %random_next refuses a stream at its end", &
                test_sphere_stream_exhausted_aborts), &
            new_unittest("a pixel draw refuses a grid that was never built", &
                test_sphere_pixel_grid_not_built_aborts), &
            new_unittest("a pixel draw refuses a grid finer than nside 2**24", &
                test_sphere_pixel_nside_over_limit_aborts), &
            new_unittest("a pixel draw refuses an index outside the grid", &
                test_sphere_pixel_ipix_out_of_range_aborts), &
            new_unittest("a mask draw refuses an empty pixel list", &
                test_sphere_mask_empty_list_aborts), &
            new_unittest("a mask draw refuses the entry it chooses when it is out of range", &
                test_sphere_mask_entry_out_of_range_aborts), &
            new_unittest("a mask draw refuses a negative entry it chooses from an int32 list", &
                test_sphere_mask_entry_out_of_range_int32_aborts), &
            new_unittest("a mask fill refuses an array without three rows (int64 list)", &
                test_sphere_fill_mask_bad_shape_aborts), &
            new_unittest("a mask RA/Dec fill refuses arrays of different sizes (int64 list)", &
                test_sphere_fill_mask_radec_size_mismatch_aborts), &
            new_unittest("a mask fill refuses an array without three rows (int32 list)", &
                test_sphere_fill_mask_bad_shape_int32_aborts), &
            new_unittest("a mask RA/Dec fill refuses arrays of different sizes (int32 list)", &
                test_sphere_fill_mask_radec_size_mismatch_int32_aborts), &
            new_unittest("a mask fill refuses an out-of-range entry before drawing (int32 list)", &
                test_sphere_fill_mask_entry_out_of_range_aborts), &
            new_unittest("a mask RA/Dec fill refuses an out-of-range entry before drawing (int64 list)", &
                test_sphere_fill_mask_radec_entry_out_of_range_aborts), &
            new_unittest("a mask fill refuses a last draw beyond huge(int64)", &
                test_sphere_fill_mask_draw_overflow_aborts), &
            new_unittest("an offset refuses a centre past a pole", &
                test_sphere_offset_dec_out_of_range_aborts), &
            new_unittest("an offset refuses a negative separation", &
                test_sphere_offset_negative_separation_aborts), &
            new_unittest("pf_sky_convert refuses the unknown system, even to itself", &
                test_skycoord_convert_unknown_system_aborts), &
            new_unittest("pf_coord_system_name refuses an integer that is not a selector", &
                test_skycoord_system_name_not_a_selector_aborts), &
            new_unittest("pf_radec2str refuses a precision past nine decimals", &
                test_skycoord_radec2str_width_overflow_aborts), &
            new_unittest("pf_dec2str refuses a separator that is not a colon, a blank or the letters", &
                test_skycoord_text_bad_separator_aborts), &
            new_unittest("pf_zhel2zcmb refuses the unknown system", &
                test_skycoord_zcmb_unknown_system_aborts), &
            new_unittest("pf_sky_rotation%apply refuses to run before %init", &
                test_skycoord_rotation_apply_before_init_aborts), &
            new_unittest("pf_sky_rotation%init refuses a selector that is not a system", &
                test_skycoord_rotation_init_unknown_system_aborts), &
            new_unittest("pf_apply_pm refuses a declination past a pole", &
                test_skycoord_apply_pm_dec_out_of_range_aborts), &
            new_unittest("skycoord: a tangent point beyond a pole aborts", &
                test_skycoord_radec2tan_dec0_out_of_range_aborts), &
            new_unittest("skycoord: the tangent plane's inverse refuses the same centre", &
                test_skycoord_tan2radec_dec0_out_of_range_aborts), &
            new_unittest("skycoord: pf_zcmb2zhel with a selector that is not a system aborts", &
                test_skycoord_zcmb2zhel_unknown_system_aborts), &
            new_unittest("a Fibonacci grid refuses n below 1", &
                test_sphere_fibonacci_n_not_positive_aborts), &
            new_unittest("a Fibonacci grid refuses an array not shaped (3, n)", &
                test_sphere_fibonacci_bad_shape_aborts), &
            new_unittest("a Fibonacci grid refuses an unknown frame", &
                test_sphere_fibonacci_bad_frame_aborts), &
            new_unittest("a Fibonacci RA/Dec grid refuses arrays not sized n", &
                test_sphere_fibonacci_radec_bad_size_aborts), &
            new_unittest("pf_radec2vec refuses an unknown frame", &
                test_sphere_radec2vec_bad_frame_aborts), &
            new_unittest("pf_vec2radec refuses an unknown frame", &
                test_sphere_vec2radec_bad_frame_aborts), &
            new_unittest("pf_bin_linear refuses a grid of fewer than two points", &
                test_stats_bin_linear_grid_too_short_aborts), &
            new_unittest("pf_bin_linear refuses a grid that is not strictly increasing", &
                test_stats_bin_linear_grid_not_increasing_aborts) &
            ]
        p6 = [ &
            new_unittest("pf_bin_linear refuses a NaN grid point, naming it as a NaN", &
                test_stats_bin_linear_nan_grid_point_aborts), &
            new_unittest("pf_bin_linear refuses an infinite grid point that pf_histogram accepts", &
                test_stats_bin_linear_infinite_grid_point_aborts), &
            new_unittest("pf_bin_linear refuses two grid points whose spacing overflows", &
                test_stats_bin_linear_spacing_overflows_aborts), &
            new_unittest("pf_bin_linear refuses a mass array of one entry per cell", &
                test_stats_bin_linear_mass_size_aborts), &
            new_unittest("pf_bin_linear refuses a negative weight", &
                test_stats_bin_linear_negative_weight_aborts), &
            new_unittest("pf_bin_linear refuses weights of the wrong length", &
                test_stats_bin_linear_weights_size_aborts), &
            new_unittest("pf_bin_linear refuses a logical column by name", &
                test_stats_bin_linear_logical_column_aborts) &
            ]
        testsuite = [p1, p2, p3, p4, p5, p6]
    end subroutine collect_tests_parquet_analysis_errors

    !
    ! ---- Variable-length LIST read abort paths ----
    !
    !> Reading a non-list column into a parquet_list_column: the shape query refuses before
    !! anything is allocated, and names the type it actually found.
    subroutine test_list_read_not_a_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_read_not_a_list", expect_abort=.true., &
            failure_message="reading a scalar column into a list column was expected to abort", &
            required_stderr="expected a list column, got int32")
    end subroutine test_list_read_not_a_list_aborts

    !> A list whose elements are themselves a list: Phase 7 READS it, so this asserts the read.
    !!
    !! It was an abort test until then. The scenario checks the row LENGTHS rather than the row
    !! count, because the likeliest assembly defect -- using the outer offsets for the inner
    !! container -- leaves the count right. See feature_container_phase7.md's D4.
    subroutine test_list_read_nested_payload_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_read_nested_payload", expect_abort=.false., &
            failure_message="reading a list<list<...>> column was expected to succeed")
    end subroutine test_list_read_nested_payload_aborts

    !> The same assembly reached through a different element type, so it is not keyed on one
    !! Arrow type id.
    subroutine test_list_read_struct_payload_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_read_struct_payload", expect_abort=.false., &
            failure_message="reading a list<struct<...>> column was expected to succeed")
    end subroutine test_list_read_struct_payload_aborts

    !> The chunked list specific inherits the sort refusal every row-group-scoped operation makes,
    !! and must not weaken it: a sort permutation destroys row-group locality.
    subroutine test_list_chunk_refuses_sort_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_chunk_refuses_sort", expect_abort=.true., &
            failure_message="a chunked list read under an active sort was expected to abort", &
            required_stderr="not supported on a reader with an active sort")
    end subroutine test_list_chunk_refuses_sort_aborts

    !> And the row-group bounds check.
    subroutine test_list_chunk_row_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_chunk_row_group_out_of_range", &
            expect_abort=.true., &
            failure_message="a chunked list read of a nonexistent row group was expected to abort", &
            required_stderr="row_group 99 out of range")
    end subroutine test_list_chunk_row_group_out_of_range_aborts

    !> The chunked list path must register the row groups it reads, or the completeness check
    !! silently stops covering it. This is the only observation of that bookkeeping.
    subroutine test_list_chunk_incomplete_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_chunk_incomplete_aborts", expect_abort=.true., &
            failure_message="a partially chunk-read list column was expected to fail the completeness check", &
            required_stderr="missing row group(s): 3, 4")
    end subroutine test_list_chunk_incomplete_aborts

    !> Its negative control: reading every row group closes cleanly. Without this, the abort above
    !! would hold just as well against a check that fired unconditionally.
    subroutine test_list_chunk_complete_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "list_chunk_complete_ok", &
            "a fully chunk-read LIST column passes the completeness check", expect_on="stdout", &
            failure_message="reading every row group of a list column should pass the completeness check")
    end subroutine test_list_chunk_complete_ok

    !> A whole-column list read must MARK the column read, so %print_stat reports it. print_stat
    !! writes to stdout, so this is the out-of-process half of that assertion.
    subroutine test_list_read_marks_read(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "list_read_marks_read", "list<int32>", expect_on="stdout", &
            failure_message="print_stat should report the list column's parquet type after a list read")
    end subroutine test_list_read_marks_read

    !
    ! ---- parquet_list_column%adopt_rows precondition aborts ----
    !
    !> Offsets whose final entry does not account for the payload handed over.
    subroutine test_list_adopt_rows_offset_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_adopt_rows_offset_mismatch", expect_abort=.true., &
            failure_message="adopt_rows with a final offset short of the payload was expected to abort", &
            required_stderr="the final offset does not match the payload element count")
    end subroutine test_list_adopt_rows_offset_mismatch_aborts

    !> Non-monotonic offsets, which would give some row a negative length.
    subroutine test_list_adopt_rows_not_monotonic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_adopt_rows_not_monotonic", expect_abort=.true., &
            failure_message="adopt_rows with non-monotonic offsets was expected to abort", &
            required_stderr="adopt_rows: offsets are not monotonic")
    end subroutine test_list_adopt_rows_not_monotonic_aborts

    !> A payload kind a list row cannot hold -- here a VECTOR kind, whose width is exactly the
    !! thing a list row expresses instead.
    subroutine test_list_adopt_rows_bad_payload_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_adopt_rows_bad_payload_kind", expect_abort=.true., &
            failure_message="adopt_rows with a vector-kind payload was expected to abort", &
            required_stderr="is not a supported list payload kind")
    end subroutine test_list_adopt_rows_bad_payload_kind_aborts

    !> A row_valid mask of the wrong length, which would leave some row's nullness undecided.
    subroutine test_list_adopt_rows_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_adopt_rows_mask_length", expect_abort=.true., &
            failure_message="adopt_rows with a short row_valid mask was expected to abort", &
            required_stderr="row_valid has a different length from the row count")
    end subroutine test_list_adopt_rows_mask_length_aborts

    !
    ! ---- Variable-length LIST write abort paths ----
    !
    !> A list column written into a schema column declared with a different element type. There is
    !! deliberately no widening between list element kinds, so the message has to name both.
    subroutine test_list_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_type_mismatch", expect_abort=.true., &
            failure_message="writing a list[int32] column into a list[int64] declaration was expected to abort", &
            required_stderr="expected list[int32], got list[int64]")
    end subroutine test_list_write_type_mismatch_aborts

    !> A never-%init'd list column has no payload kind, so there is no element type to declare and
    !! nothing to write -- reported as such rather than as a type mismatch against no token.
    subroutine test_list_write_uninitialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_uninitialized", expect_abort=.true., &
            failure_message="writing an uninitialized list column was expected to abort", &
            required_stderr="has not been initialized")
    end subroutine test_list_write_uninitialized_aborts

    !> col_size: declares a fixed per-row width, which a list column does not have.
    subroutine test_list_write_col_size_rejected_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! "declares col_size > 1" pins THIS arm. The tail alone appears in the auto arm too, so
        ! asserting only that made this test and test_list_col_size_auto_rejected indistinguishable.
        call check_scenario_exit_status_and_stderr(error, "list_write_col_size_rejected", expect_abort=.true., &
            failure_message="col_size on a list column was expected to be rejected", &
            required_stderr="declares col_size > 1, which does not apply to a list column")
    end subroutine test_list_write_col_size_rejected_aborts

    !> `col_size: auto` on a list column: the OTHER arm of the same rule, with its own message.
    !! Paired with test_list_write_col_size_rejected_aborts above, which now pins "col_size > 1" --
    !! between them the two arms are distinguishable, which neither was on its own.
    subroutine test_list_col_size_auto_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_col_size_auto_rejected", expect_abort=.true., &
            failure_message="col_size: auto on a list column was expected to be rejected", &
            required_stderr="declares col_size: auto, which does not apply to a list column")
    end subroutine test_list_col_size_auto_rejected

    !> schema%set_col_size(..., force=.true.) on a list column. The setter reaches no validator, so
    !! force= used to let a width through and the writer failed later with "array size mismatch",
    !! naming the caller's data instead of the declaration.
    subroutine test_list_set_col_size_forced_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_set_col_size_forced_rejected", expect_abort=.true., &
            failure_message="set_col_size(force=.true.) on a list column was expected to be rejected", &
            required_stderr="col_size does not apply to list column")
    end subroutine test_list_set_col_size_forced_rejected

    !> qc: min:/max: stays scalar-leaf-only by design; the message says qc: miss: still applies.
    subroutine test_list_write_qc_rejected_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_qc_rejected", expect_abort=.true., &
            failure_message="qc: min: on a list column was expected to be rejected", &
            required_stderr="not supported for a list column (qc: miss: is)")
    end subroutine test_list_write_qc_rejected_aborts

    !> `list[]` names no element type, and the element type is required.
    subroutine test_list_write_bad_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_bad_token", expect_abort=.true., &
            failure_message="a list token with no element type was expected to be rejected", &
            required_stderr="invalid data_type 'list[]'")
    end subroutine test_list_write_bad_token_aborts

    !> A well-formed list token whose element type nothing recognises -- the other malformed shape.
    subroutine test_list_write_unknown_element_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_unknown_element", expect_abort=.true., &
            failure_message="a list token with an unknown element type was expected to be rejected", &
            required_stderr="invalid data_type 'list[complex64]'")
    end subroutine test_list_write_unknown_element_aborts

    !> protected_cols: means no Null at either level, and a null ROW is one.
    subroutine test_list_write_protected_row_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_protected_row_null", expect_abort=.true., &
            failure_message="a protected list column with a null row was expected to abort", &
            required_stderr="is protected (extra: protected_cols:) and cannot contain Null values")
    end subroutine test_list_write_protected_row_null_aborts

    !> The other level, with its own message so a caller is not sent looking at rows. At the
    !! Parquet level a null ELEMENT is a Null in the same leaf column, so the weaker reading of
    !! protected_cols: would declare a non-nullable element field for a column that can hold one.

    !> ---- STRUCT column scenarios ----
    !>
    !> Every one asserts the LIBRARY's own message, not just the abort. That matters most for the
    !> two protected guards: with the field-level check removed, Arrow catches the resulting
    !> non-nullable-field-containing-nulls array at close time and the process still dies (exit
    !> 134) -- so an exit-status-only assertion cannot tell the guard from its absence. Measured,
    !> by removing the check and running the scenario. The message is what distinguishes them.
    subroutine test_struct_init_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_init_no_fields", expect_abort=.true., &
            failure_message="a struct column declaring no fields was expected to abort", &
            required_stderr="must declare at least one field")
    end subroutine test_struct_init_no_fields_aborts

    subroutine test_struct_init_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_init_duplicate_name", expect_abort=.true., &
            failure_message="a struct column with two identically named fields was expected to abort", &
            required_stderr="duplicate field name")
    end subroutine test_struct_init_duplicate_name_aborts

    subroutine test_struct_init_dotted_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_init_dotted_name", expect_abort=.true., &
            failure_message="a struct field name containing '.' was expected to abort", &
            required_stderr="would collide with the dotted path")
    end subroutine test_struct_init_dotted_name_aborts

    ! ---- MAP column error scenarios ----
    !
    ! Every one asserts the MESSAGE, not just the exit status. Arrow catches several of the same
    ! defects with the same status, so an exit-status-only assertion cannot distinguish this
    ! library's guard from Arrow's after-the-fact catch -- the lesson Phase 4's mutation F left.

    subroutine test_map_init_bad_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_init_bad_kind", expect_abort=.true., &
            failure_message="a map column of an unsupported value kind was expected to abort", &
            required_stderr="not a supported map value kind")
    end subroutine test_map_init_bad_kind_aborts

    !> Nesting is READ-ONLY (feature_container_phase7.md's Q2), and these pin the write half.
    !!
    !! **Each refusal has a NEGATIVE CONTROL beside it**, which is the whole reason there are six
    !! tests here rather than three: D2 deliberately widened the in-memory gate so a nested column
    !! can be BUILT, so a write-side guard that fired on every container column would satisfy every
    !! abort test ever written for it while breaking the ordinary non-nested write this library
    !! ships. The controls are what distinguish the two.
    subroutine test_write_nested_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_nested_list", expect_abort=.true., &
            failure_message="writing a list whose payload is a container was expected to abort", &
            required_stderr="has a nested payload")
    end subroutine test_write_nested_list_aborts

    subroutine test_write_nested_list_control(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "write_nested_list_control", expect_abort=.false., &
            failure_message="writing an ordinary non-nested list column was expected to succeed")
    end subroutine test_write_nested_list_control

    !> The struct form names the FIELD as well as the column: a struct can carry one offending
    !> field among several, and "this struct is nested" is not enough for a caller to act on.
    subroutine test_write_nested_struct_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_nested_struct_field", &
            expect_abort=.true., &
            failure_message="writing a struct with a container field was expected to abort", &
            required_stderr="field 'values' of struct column")
    end subroutine test_write_nested_struct_field_aborts

    !> `qc:` and `parquet_filter` are scalar-leaf-only PERMANENTLY, so the descent grammar must not
    !> leak into them. A descent path RESOLVES, so without an explicit refusal the clause would be
    !> evaluated against a container's flattened child -- one answer per ELEMENT, silently
    !> misaligned with every other column. That is a wrong answer, not an error, which is why the
    !> refusal is asserted rather than assumed.
    subroutine test_qc_descent_path_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "qc_descent_path", expect_abort=.true., &
            failure_message="a qc: rule naming a descent path was expected to abort", &
            required_stderr="is not a qc target")
    end subroutine test_qc_descent_path_aborts

    subroutine test_qc_descent_path_control(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "qc_descent_path_control", expect_abort=.false., &
            failure_message="a qc: rule on an ordinary dotted struct leaf was expected to work")
    end subroutine test_qc_descent_path_control

    subroutine test_maml_list_bare_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "maml_list_bare_token", expect_abort=.true., &
            failure_message="a bare list data_type was expected to be rejected", &
            required_stderr="has invalid data_type 'list'")
    end subroutine test_maml_list_bare_token_aborts

    subroutine test_list_row_index_unassigned(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_row_index_unassigned", expect_abort=.true., &
            failure_message="row_index on an unassigned list handle was expected to abort", &
            required_stderr="row_index: this row handle is not associated with a column")
    end subroutine test_list_row_index_unassigned

    subroutine test_map_row_index_unassigned(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_row_index_unassigned", expect_abort=.true., &
            failure_message="row_index on an unassigned map handle was expected to abort", &
            required_stderr="row_index: this row handle is not associated with a column")
    end subroutine test_map_row_index_unassigned

    subroutine test_list_row_index_live(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_row_index_live", expect_abort=.false., &
            failure_message="row_index on a live handle was expected to answer")
    end subroutine test_list_row_index_live

    subroutine test_filter_descent_path_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "filter_descent_path", expect_abort=.true., &
            failure_message="a filter naming a descent path was expected to abort", &
            required_stderr="is not filterable")
    end subroutine test_filter_descent_path_aborts

    subroutine test_filter_descent_path_control(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "filter_descent_path_control", expect_abort=.false., &
            failure_message="a filter on an ordinary dotted struct leaf was expected to work")
    end subroutine test_filter_descent_path_control
    !
    subroutine test_sort_key_descent_path_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sort_key_descent_path", expect_abort=.true., &
            failure_message="a sort key naming a descent path was expected to abort", &
            required_stderr="cannot be a sort key")
    end subroutine test_sort_key_descent_path_aborts

    !> The three `%init`-refuses-a-container tests, and what makes them worth having separately
    !> from `map_init_bad_kind` above: since Phase 7 the same gate refuses a `*_VEC` kind and a
    !> CONTAINER kind with different messages, because only one of them has a route that works.
    !> Asserting the message -- not merely the abort -- is what pins that the caller is told about
    !> `%adopt_*` rather than simply told no. See feature_container_phase7.md's D1.
    subroutine test_map_init_nested_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_init_nested_value", expect_abort=.true., &
            failure_message="a map %init with a container value was expected to abort", &
            required_stderr="is a nested value and cannot be declared here")
    end subroutine test_map_init_nested_value_aborts

    subroutine test_list_init_nested_payload_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_init_nested_payload", expect_abort=.true., &
            failure_message="a list %init with a container payload was expected to abort", &
            required_stderr="is a nested payload and cannot be declared here")
    end subroutine test_list_init_nested_payload_aborts

    !> The struct form additionally names the offending FIELD, which the other two have no
    !> equivalent of -- a struct's `kinds(:)` array can carry one bad entry among several.
    subroutine test_struct_init_nested_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_init_nested_field", expect_abort=.true., &
            failure_message="a struct %init with a container field was expected to abort", &
            required_stderr="field 'w' is a nested")
    end subroutine test_struct_init_nested_field_aborts

    !> A struct field that is itself a struct: out of Phase 7's scope (c), and the refusal must
    !> point at the dotted-path mechanism that reads its leaves at any depth rather than merely
    !> decline. A message that only said "unsupported" would leave the caller with no next step.
    subroutine test_struct_read_nested_struct_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_read_nested_struct_field", &
            expect_abort=.true., &
            failure_message="reading a struct whose field is itself a struct was expected to abort", &
            required_stderr="read its leaves by their own dotted paths")
    end subroutine test_struct_read_nested_struct_field_aborts

    subroutine test_map_append_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_wrong_kind", expect_abort=.true., &
            failure_message="appending the wrong value type to a map was expected to abort", &
            required_stderr="values cannot be appended to it")
    end subroutine test_map_append_wrong_kind_aborts

    subroutine test_map_append_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_length_mismatch", expect_abort=.true., &
            failure_message="mismatched key/value arrays were expected to abort", &
            required_stderr="keys and values have different lengths")
    end subroutine test_map_append_length_mismatch_aborts

    !> A wrong VALUE KIND is a type mismatch rather than a lookup failure, so it aborts even
    !! though the scenario supplies `found=` -- which is exactly what this asserts.
    subroutine test_map_get_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_get_wrong_kind", expect_abort=.true., &
            failure_message="reading a map value through the wrong specific was expected to abort", &
            required_stderr="its values cannot be read into")
    end subroutine test_map_get_wrong_kind_aborts

    subroutine test_map_get_missing_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_get_missing_key", expect_abort=.true., &
            failure_message="a missing map key with neither warn= nor found= was expected to abort", &
            required_stderr="no entry with key")
    end subroutine test_map_get_missing_key_aborts

    !> The NEGATIVE CONTROL for every soft-fail guard: a guard that fired unconditionally would
    !! pass every abort test above while breaking every legitimate lookup.
    subroutine test_map_get_missing_key_warn_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "map_get_missing_key_warn_ok", expect_abort=.false., &
            failure_message="a soft-fail map lookup was expected to return rather than abort")
    end subroutine test_map_get_missing_key_warn_ok

    subroutine test_map_get_occurrence_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_get_occurrence_zero", expect_abort=.true., &
            failure_message="occurrence below 1 was expected to abort even with found=", &
            required_stderr="occurrence must be 1 or greater")
    end subroutine test_map_get_occurrence_zero_aborts

    subroutine test_map_get_at_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_get_at_out_of_range", expect_abort=.true., &
            failure_message="%get_at past the end of a map row was expected to abort", &
            required_stderr="is out of range; this map row holds")
    end subroutine test_map_get_at_out_of_range_aborts

    subroutine test_map_view_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_view_out_of_range", expect_abort=.true., &
            failure_message="%view past the last map row was expected to abort", &
            required_stderr="row index is out of range")
    end subroutine test_map_view_out_of_range_aborts

    subroutine test_map_read_not_a_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_read_not_a_map", expect_abort=.true., &
            failure_message="reading a non-map column into a map column was expected to abort", &
            required_stderr="expected a map column")
    end subroutine test_map_read_not_a_map_aborts

    !> V1 keys are strings, and the message must NAME THE KEY TYPE -- a caller whose file is
    !! keyed by an integer needs to know that is why, not merely that something failed.
    !!
    !! **When non-string keys are ever supported this test does not disappear**: the scenario
    !! becomes a positive read of `m_intkey`'s two entries, and this wrapper becomes the assertion
    !! that they came back with their integer keys.
    subroutine test_map_read_int_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_read_int_key", expect_abort=.true., &
            failure_message="reading a map with non-string keys was expected to abort", &
            required_stderr="only string keys are supported")
    end subroutine test_map_read_int_key_aborts

    !> A map whose value is a container: Phase 7 reads it. The scenario asserts the KEYS as well,
    !! since only the values path changed and a keys regression would otherwise hide here.
    subroutine test_map_read_nested_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "map_read_nested_value", expect_abort=.false., &
            failure_message="reading a map with a container value was expected to succeed")
    end subroutine test_map_read_nested_value_aborts

    subroutine test_map_write_uninitialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_write_uninitialized", expect_abort=.true., &
            failure_message="writing an uninitialized map column was expected to abort", &
            required_stderr="has not been initialized")
    end subroutine test_map_write_uninitialized_aborts

    subroutine test_map_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_write_type_mismatch", expect_abort=.true., &
            failure_message="writing a map column into a differently-typed slot was expected to abort", &
            required_stderr="a map column's value type must match exactly")
    end subroutine test_map_write_type_mismatch_aborts

    subroutine test_map_col_size_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_col_size_rejected", expect_abort=.true., &
            failure_message="col_size on a map column was expected to be rejected", &
            required_stderr="declares col_size > 1, which does not apply to a map column")
    end subroutine test_map_col_size_rejected

    !> The map arm of the col_size: auto pair; see test_list_col_size_auto_rejected.
    subroutine test_map_col_size_auto_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_col_size_auto_rejected", expect_abort=.true., &
            failure_message="col_size: auto on a map column was expected to be rejected", &
            required_stderr="declares col_size: auto, which does not apply to a map column")
    end subroutine test_map_col_size_auto_rejected

    subroutine test_map_qc_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_qc_rejected", expect_abort=.true., &
            failure_message="qc min/max on a map column was expected to be rejected", &
            required_stderr="not supported for a map column")
    end subroutine test_map_qc_rejected

    subroutine test_map_protected_row_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_protected_row_null", expect_abort=.true., &
            failure_message="a protected map column with a null row was expected to abort", &
            required_stderr="cannot contain Null values")
    end subroutine test_map_protected_row_null_aborts

    !> The VALUE level gets its own message, so the abort says which of a map's two null levels
    !! failed rather than only that one did.
    subroutine test_map_protected_value_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_protected_value_null", expect_abort=.true., &
            failure_message="a protected map column with a null value was expected to abort", &
            required_stderr="one of its map values is a Null")
    end subroutine test_map_protected_value_null_aborts

    subroutine test_map_protected_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "map_protected_ok", expect_abort=.false., &
            failure_message="a protected map column with no Null anywhere was expected to write cleanly")
    end subroutine test_map_protected_ok

    !> The int32 entry ceiling. The message must say there is NO large_map to widen into: that is
    !! the whole difference from the string and list ceilings, which widen instead of refusing,
    !! and a caller who does not know it will go looking for the option that does not exist.
    subroutine test_map_entry_limit_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_entry_limit", expect_abort=.true., &
            failure_message="a map column past the int32 entry ceiling was expected to be refused", &
            required_stderr="Arrow has no large_map to widen into")
    end subroutine test_map_entry_limit_aborts

    subroutine test_map_adopt_rows_offset_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_adopt_rows_offset_mismatch", expect_abort=.true., &
            failure_message="%adopt_rows with a wrong final offset was expected to abort", &
            required_stderr="final offset does not match the entry count")
    end subroutine test_map_adopt_rows_offset_mismatch_aborts

    subroutine test_map_adopt_rows_bad_key_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_adopt_rows_bad_key_kind", expect_abort=.true., &
            failure_message="%adopt_rows with non-string keys was expected to abort", &
            required_stderr="map keys must be a string column")
    end subroutine test_map_adopt_rows_bad_key_kind_aborts

    subroutine test_list_stale_handle_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_stale_handle", expect_abort=.true., &
            failure_message="a list row handle outliving its row was expected to abort", &
            required_stderr="no longer refers to a valid row")
    end subroutine test_list_stale_handle_aborts

    subroutine test_map_gather_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_gather_out_of_range", expect_abort=.true., &
            failure_message="%gather_rows naming a row that does not exist was expected to abort", &
            required_stderr="gather_rows: source row index out of range")
    end subroutine test_map_gather_out_of_range_aborts

    subroutine test_map_append_from_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_from_kind_mismatch", &
            expect_abort=.true., &
            failure_message="%append_from between maps of different value kinds was expected to abort", &
            required_stderr="cannot append a map<string,float64>")
    end subroutine test_map_append_from_kind_mismatch_aborts

    subroutine test_map_append_from_not_a_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_from_not_a_map", &
            expect_abort=.true., &
            failure_message="%append_from handed a list was expected to abort", &
            required_stderr="cannot append a list<int32> onto a map")
    end subroutine test_map_append_from_not_a_map_aborts

    subroutine test_map_adopt_rows_bad_value_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_adopt_rows_bad_value_kind", &
            expect_abort=.true., &
            failure_message="%adopt_rows with an unsupported value kind was expected to abort", &
            required_stderr="is not a supported map value kind")
    end subroutine test_map_adopt_rows_bad_value_kind_aborts

    subroutine test_map_adopt_rows_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_adopt_rows_length_mismatch", &
            expect_abort=.true., &
            failure_message="%adopt_rows with mismatched key and value counts was expected to abort", &
            required_stderr="keys and values hold different entry counts")
    end subroutine test_map_adopt_rows_length_mismatch_aborts

    subroutine test_map_adopt_rows_row_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_adopt_rows_row_valid_length", &
            expect_abort=.true., &
            failure_message="%adopt_rows with a short row_valid was expected to abort", &
            required_stderr="row_valid has a different length from the row count")
    end subroutine test_map_adopt_rows_row_valid_length_aborts

    subroutine test_map_append_from_uninitialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_from_uninitialized", &
            expect_abort=.true., &
            failure_message="%append_from onto a column with no value kind was expected to abort", &
            required_stderr="has no value kind; call %init first")
    end subroutine test_map_append_from_uninitialized_aborts

    subroutine test_map_stale_handle_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_stale_handle", expect_abort=.true., &
            failure_message="a map row handle outliving its row was expected to abort", &
            required_stderr="no longer refers to a valid row")
    end subroutine test_map_stale_handle_aborts

    subroutine test_map_append_is_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_append_is_valid_length", &
            expect_abort=.true., &
            failure_message="%append_row with a short is_valid was expected to abort", &
            required_stderr="is_valid has a different length from values")
    end subroutine test_map_append_is_valid_length_aborts

    !> The key inside the message is TRUNCATED, so the assertion is that the quoted text stops
    !> short of the 300 characters the scenario passed while still naming the key.
    subroutine test_map_missing_key_preview_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_missing_key_preview", &
            expect_abort=.true., &
            failure_message="a lookup for a 300-character key was expected to abort", &
            required_stderr="...")
    end subroutine test_map_missing_key_preview_aborts

    subroutine test_map_chunk_refuses_sort_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "map_chunk_refuses_sort", expect_abort=.true., &
            failure_message="a chunked map read under an active sort was expected to abort", &
            required_stderr="not supported on a reader with an active sort")
    end subroutine test_map_chunk_refuses_sort_aborts

    !> The bare `map` token is INVALID where the bare `struct` token is valid, and the asymmetry
    !! is deliberate: a struct's field layout cannot be expressed in MAML at all, while a map's
    !! value type is a single token the schema can carry -- and must, since a declared-but-
    !! unwritten map column is written with zero rows at close and cannot invent a value kind.
    subroutine test_maml_map_bare_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "maml_map_bare_token", expect_abort=.true., &
            failure_message="the bare map token was expected to be rejected", &
            required_stderr="has invalid data_type 'map'")
    end subroutine test_maml_map_bare_token_aborts

    subroutine test_maml_map_nested_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "maml_map_nested_value", expect_abort=.true., &
            failure_message="a map token with a container value was expected to be rejected", &
            required_stderr="has invalid data_type 'map[list[int32]]'")
    end subroutine test_maml_map_nested_value_aborts

    subroutine test_maml_text_list_bad_element_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "maml_text_list_bad_element", expect_abort=.true., &
            failure_message="a list token with a container element in MAML text was expected to be rejected", &
            required_stderr="has invalid data_type 'list[list[int32]]'")
    end subroutine test_maml_text_list_bad_element_aborts

    subroutine test_maml_text_map_bad_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "maml_text_map_bad_value", expect_abort=.true., &
            failure_message="a map token with an unknown value type in MAML text was expected to be rejected", &
            required_stderr="has invalid data_type 'map[nosuchtype]'")
    end subroutine test_maml_text_map_bad_value_aborts

    subroutine test_struct_init_bad_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_init_bad_kind", expect_abort=.true., &
            failure_message="a struct field of an unsupported kind was expected to abort", &
            required_stderr="not a supported struct field kind")
    end subroutine test_struct_init_bad_kind_aborts

    subroutine test_struct_append_from_field_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_append_from_field_count", &
            expect_abort=.true., &
            failure_message="append_from with a different field count was expected to abort", &
            required_stderr="fields onto one with")
    end subroutine test_struct_append_from_field_count_aborts

    subroutine test_struct_append_from_field_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_append_from_field_kind", &
            expect_abort=.true., &
            failure_message="append_from with a different field kind was expected to abort", &
            required_stderr="in the source and")
    end subroutine test_struct_append_from_field_kind_aborts

    subroutine test_struct_append_from_not_struct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_append_from_not_struct", &
            expect_abort=.true., &
            failure_message="append_from of a non-struct container was expected to abort", &
            required_stderr="cannot append a")
    end subroutine test_struct_append_from_not_struct_aborts

    subroutine test_struct_gather_rows_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_gather_rows_out_of_range", &
            expect_abort=.true., &
            failure_message="gather_rows with an out-of-range index was expected to abort", &
            required_stderr="source row index out of range")
    end subroutine test_struct_gather_rows_out_of_range_aborts

    subroutine test_struct_field_kind_not_narrowed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_field_kind_not_narrowed", &
            expect_abort=.true., &
            failure_message="field_kind on an un-narrowed handle was expected to abort", &
            required_stderr="narrow it with %field first")
    end subroutine test_struct_field_kind_not_narrowed_aborts

    subroutine test_struct_nested_not_narrowed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_nested_not_narrowed", &
            expect_abort=.true., &
            failure_message="nested on an un-narrowed handle was expected to abort", &
            required_stderr="narrow it with %field first")
    end subroutine test_struct_nested_not_narrowed_aborts

    subroutine test_struct_adopt_dotted_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_adopt_dotted_name", &
            expect_abort=.true., &
            failure_message="adopt_fields with a dotted field name was expected to abort", &
            required_stderr="a field name contains '.'")
    end subroutine test_struct_adopt_dotted_name_aborts

    subroutine test_struct_adopt_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_adopt_duplicate_name", &
            expect_abort=.true., &
            failure_message="adopt_fields with a duplicate field name was expected to abort", &
            required_stderr="duplicate field name")
    end subroutine test_struct_adopt_duplicate_name_aborts

    subroutine test_struct_adopt_bad_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_adopt_bad_kind", &
            expect_abort=.true., &
            failure_message="adopt_fields with an unsupported field kind was expected to abort", &
            required_stderr="is not a supported struct field kind")
    end subroutine test_struct_adopt_bad_kind_aborts

    subroutine test_struct_adopt_ragged_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_adopt_ragged_rows", &
            expect_abort=.true., &
            failure_message="adopt_fields with ragged field lengths was expected to abort", &
            required_stderr="has a different row count")
    end subroutine test_struct_adopt_ragged_rows_aborts

    subroutine test_struct_adopt_row_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_adopt_row_valid_length", &
            expect_abort=.true., &
            failure_message="adopt_fields with a short row_valid was expected to abort", &
            required_stderr="row_valid has a different length")
    end subroutine test_struct_adopt_row_valid_length_aborts

    subroutine test_struct_set_field_uninitialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_set_field_uninitialized", &
            expect_abort=.true., &
            failure_message="set_field on an uninitialized column was expected to abort", &
            required_stderr="has no fields; call %init first")
    end subroutine test_struct_set_field_uninitialized_aborts

    subroutine test_struct_field_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_field_index_out_of_range", &
            expect_abort=.true., &
            failure_message="set_field with an out-of-range field index was expected to abort", &
            required_stderr="field index is out of range")
    end subroutine test_struct_field_index_out_of_range_aborts

    subroutine test_struct_handle_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_handle_unassociated", &
            expect_abort=.true., &
            failure_message="an unassociated struct row handle was expected to abort", &
            required_stderr="not associated with a column")
    end subroutine test_struct_handle_unassociated_aborts

    subroutine test_struct_handle_stale_row_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_handle_stale_row", &
            expect_abort=.true., &
            failure_message="a stale struct row handle was expected to abort", &
            required_stderr="no longer refers to a valid row")
    end subroutine test_struct_handle_stale_row_aborts

    subroutine test_struct_get_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_get_wrong_kind", expect_abort=.true., &
            failure_message="reading a struct field through the wrong specific was expected to abort", &
            required_stderr="it cannot be read into a")
    end subroutine test_struct_get_wrong_kind_aborts

    subroutine test_struct_get_not_narrowed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_get_not_narrowed", expect_abort=.true., &
            failure_message="%get on an un-narrowed struct handle was expected to abort", &
            required_stderr="narrow it to a field")
    end subroutine test_struct_get_not_narrowed_aborts

    !> The message lists every declared name -- a misspelled or reordered field is the
    !! overwhelmingly common cause, so the list is the useful half of the message.
    subroutine test_struct_field_unknown_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_field_unknown", expect_abort=.true., &
            failure_message="%field on an unknown struct field name was expected to abort", &
            required_stderr="declared fields: v")
    end subroutine test_struct_field_unknown_aborts

    !> The NEGATIVE CONTROL for the guard above: `warn=` must NOT abort, and must hand back an
    !! invalid handle. Without it, a guard firing unconditionally would pass the abort test.
    subroutine test_struct_field_unknown_warn_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "struct_field_unknown_warn_ok", expect_abort=.false., &
            failure_message="%field(warn=.true.) on an unknown name was expected to warn, not abort")
    end subroutine test_struct_field_unknown_warn_ok

    subroutine test_struct_write_uninitialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_write_uninitialized", expect_abort=.true., &
            failure_message="writing an uninitialized struct column was expected to abort", &
            required_stderr="has not been initialized")
    end subroutine test_struct_write_uninitialized_aborts

    subroutine test_struct_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_write_type_mismatch", expect_abort=.true., &
            failure_message="writing a struct column into an int32 schema slot was expected to abort", &
            required_stderr="expected struct, got int32")
    end subroutine test_struct_write_type_mismatch_aborts

    !> A struct TIMESTAMP field with sub-microsecond precision is refused, naming the field.
    !!
    !! The assertion is on the message the push_struct_field guard emits, NOT on the one
    !! parquet_timestamp%to_unix would emit -- that is the whole point of the guard, so asserting
    !! "precision loss" instead would pass against the defect it was written to remove.
    subroutine test_struct_timestamp_precision_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_timestamp_precision", expect_abort=.true., &
            failure_message="a sub-microsecond timestamp in a struct field was expected to be refused", &
            required_stderr="timestamp value has finer precision than the column's declared unit for field when")
    end subroutine test_struct_timestamp_precision_aborts

    !> The same for a struct TIME field, which is refused on the C++ side instead.
    subroutine test_struct_time_precision_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_time_precision", expect_abort=.true., &
            failure_message="a sub-microsecond time in a struct field was expected to be refused", &
            required_stderr="time value has finer precision than the column's declared unit")
    end subroutine test_struct_time_precision_aborts

    !> The NEGATIVE CONTROL for the two above: microsecond-exact temporal fields must write.
    !!
    !! Without it both refusals are satisfied by a struct write that rejects every temporal field.
    !! The scenario also reads the timestamp back and prints on mismatch, so a value that survived
    !! the guard but not the round trip fails here rather than passing quietly.
    subroutine test_struct_temporal_precision_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "struct_temporal_precision_ok", expect_abort=.false., &
            failure_message="microsecond-exact temporal struct fields were expected to write cleanly")
    end subroutine test_struct_temporal_precision_ok

    subroutine test_struct_col_size_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_col_size_rejected", expect_abort=.true., &
            failure_message="col_size: on a struct column was expected to be rejected", &
            required_stderr="declares col_size > 1, which does not apply to a struct column")
    end subroutine test_struct_col_size_rejected

    !> The struct arm of the col_size: auto pair; see test_list_col_size_auto_rejected.
    subroutine test_struct_col_size_auto_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_col_size_auto_rejected", expect_abort=.true., &
            failure_message="col_size: auto on a struct column was expected to be rejected", &
            required_stderr="declares col_size: auto, which does not apply to a struct column")
    end subroutine test_struct_col_size_auto_rejected

    !> schema%set_col_size on a struct column, without force=: the container refusal runs before
    !! the "already resolved" guard, so it is this message and not that one that comes back.
    subroutine test_struct_set_col_size_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_set_col_size_rejected", expect_abort=.true., &
            failure_message="set_col_size on a struct column was expected to be rejected", &
            required_stderr="col_size does not apply to struct column")
    end subroutine test_struct_set_col_size_rejected

    subroutine test_struct_qc_rejected(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_qc_rejected", expect_abort=.true., &
            failure_message="qc: min:/max: on a struct column was expected to be rejected", &
            required_stderr="not supported for a struct column")
    end subroutine test_struct_qc_rejected

    subroutine test_struct_protected_row_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_protected_row_null", expect_abort=.true., &
            failure_message="a protected struct column with a null row was expected to abort", &
            required_stderr="cannot contain Null values")
    end subroutine test_struct_protected_row_null_aborts

    !> **The stderr assertion here is load-bearing, not decoration.** With the field-level
    !! protected check removed, Arrow rejects the resulting array at close time and the process
    !! still aborts -- so only the message separates the library's own guard from Arrow's
    !! after-the-fact catch. Naming the FIELD is what the guard buys.
    subroutine test_struct_protected_field_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_protected_field_null", expect_abort=.true., &
            failure_message="a protected struct column with a null field was expected to abort", &
            required_stderr="field 'w' holds a Null")
    end subroutine test_struct_protected_field_null_aborts

    !> The NEGATIVE CONTROL for the two above.
    subroutine test_struct_protected_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "struct_protected_ok", expect_abort=.false., &
            failure_message="a protected struct column with no Null anywhere was expected to write cleanly")
    end subroutine test_struct_protected_ok

    !> Phase 7 reads a struct whose field is a LIST, so this asserts the READ rather than a refusal.
    !!
    !! It was an abort test until then, and inverting it rather than deleting it is deliberate: it
    !! is the negative control for `struct_read_nested_struct_field`, which still aborts. The two
    !! differ only in the field's type, so the pair shows what the surviving refusal is actually
    !! about. See feature_container_phase7.md's D4.
    subroutine test_struct_read_nested_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "struct_read_nested_field", expect_abort=.false., &
            failure_message="reading a struct whose field is a list was expected to succeed")
    end subroutine test_struct_read_nested_field_aborts

    subroutine test_struct_view_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_view_out_of_range", expect_abort=.true., &
            failure_message="%view past the last struct row was expected to abort", &
            required_stderr="row index is out of range")
    end subroutine test_struct_view_out_of_range_aborts

    subroutine test_struct_set_field_unknown_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_set_field_unknown", expect_abort=.true., &
            failure_message="%set_field naming an undeclared field was expected to abort", &
            required_stderr="no field named 'nope'")
    end subroutine test_struct_set_field_unknown_aborts

    subroutine test_struct_set_field_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "struct_set_field_wrong_kind", expect_abort=.true., &
            failure_message="%set_field writing the wrong type was expected to abort", &
            required_stderr="value cannot be written into it")
    end subroutine test_struct_set_field_wrong_kind_aborts

    subroutine test_list_write_protected_element_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_protected_element_null", expect_abort=.true., &
            failure_message="a protected list column with a null element was expected to abort", &
            required_stderr="one of its list ELEMENTS is Null")
    end subroutine test_list_write_protected_element_null_aborts

    !> The NEGATIVE CONTROL for the two above: without it, a guard firing unconditionally would
    !! pass both while making every protected list column unwritable.
    subroutine test_list_write_protected_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_write_protected_ok", expect_abort=.false., &
            failure_message="a protected list column with no Null anywhere was expected to write cleanly")
    end subroutine test_list_write_protected_ok

    !> One row over the per-row-group element ceiling: no row-group size can rescue it, because a
    !! row is never split across row groups.
    subroutine test_list_write_row_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_row_too_long", expect_abort=.true., &
            failure_message="a list row over the element ceiling was expected to abort", &
            required_stderr="no row-group size can accommodate this")
    end subroutine test_list_write_row_too_long_aborts

    !> A streamed row group over that ceiling: validated rather than silently overridden, because
    !! the caller chose this row group's size through parquet_new_row_group.
    subroutine test_list_write_chunk_too_many_elements_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_chunk_too_many_elements", &
            expect_abort=.true., &
            failure_message="a streamed row group over the element ceiling was expected to abort", &
            required_stderr="pass a smaller nrows to parquet_new_row_group")
    end subroutine test_list_write_chunk_too_many_elements_aborts

    !> An explicit chunk_size that would put too many elements in one row group, checked at close
    !! by walking the column's own offsets at chunk_size stride.
    subroutine test_list_write_explicit_chunk_size_too_big_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_write_explicit_chunk_size_too_big", &
            expect_abort=.true., &
            failure_message="an explicit chunk_size over the element ceiling was expected to abort", &
            required_stderr="list elements in one row group")
    end subroutine test_list_write_explicit_chunk_size_too_big_aborts

    !> The NEGATIVE CONTROL for the three ceiling scenarios: the same shrunk limit with a
    !! chunk_size that fits must write every row and every element.
    subroutine test_list_write_ceiling_ok(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_write_ceiling_ok", expect_abort=.false., &
            failure_message="a chunk_size within the element ceiling was expected to write cleanly")
    end subroutine test_list_write_ceiling_ok

    !> The arrow::large_list() write path, forced with a tiny fixture by shrinking the int32
    !! offsets threshold. Asserts the VALUES come back, not merely that nothing crashed: a
    !! large_list and a list are indistinguishable in the Parquet file itself.
    subroutine test_list_write_large_list_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_write_large_list_roundtrip", expect_abort=.false., &
            failure_message="the large_list write path was expected to round-trip cleanly")
    end subroutine test_list_write_large_list_roundtrip

    !> A `list<string>` whose byte payload cannot fit int32 offsets writes and reads its child
    !! through the 64-bit arm.
    !!
    !! See `scenario_list_write_large_string_child` (test/error_scenarios_analysis.f90) for why the
    !! round trip is the assertion and why the threshold is shrunk rather than met.
    subroutine test_list_write_large_string_child(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        call check_scenario_exit_status(error, "list_write_large_string_child", expect_abort=.false., &
            failure_message="a list column's large_utf8 string child was expected to round-trip cleanly")
    end subroutine test_list_write_large_string_child

    !> Two row groups either side of the int32 offsets threshold. The streamed path assembles every
    !! chunk with the type its FIELD already carries, fixed by the first row group; recomputing the
    !! width per chunk instead would have align_array_to_field restamp the second chunk's int64
    !! offsets as int32, so every value in it comes back garbage while the file still validates.
    !! A mutation doing exactly that survived the whole suite until this scenario existed.
    subroutine test_list_write_large_list_chunked(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "list_write_large_list_chunked", expect_abort=.false., &
            failure_message="a streamed list column crossing the offset threshold was expected to round-trip")
    end subroutine test_list_write_large_list_chunked

    !
    ! ---- parquet_spatial abort paths ----
    !
    subroutine test_spatial_query_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_query_before_build", expect_abort=.true., &
            failure_message="querying an unbuilt spatial index was expected to abort", &
            required_stderr="this index has not been built")
    end subroutine test_spatial_query_before_build_aborts

    subroutine test_spatial_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_length_mismatch", expect_abort=.true., &
            failure_message="building from mismatched coordinate arrays was expected to abort", &
            required_stderr="x and y must be the same length")
    end subroutine test_spatial_length_mismatch_aborts

    ! ---- parquet_healpix ----
    !
    ! Each asserts the LIBRARY's own message text and a nonzero exit, never the runtime's
    ! `ERROR STOP` prefix or a particular status: both are processor-dependent and differ across
    ! the three compilers this project builds with.

    subroutine test_healpix_nside_not_power2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_nside_not_power2", &
            expect_abort=.true., &
            failure_message="an nside of 100 was expected to abort a disc query", &
            required_stderr="nside must be a positive power of two")
    end subroutine test_healpix_nside_not_power2_aborts

    !> `%init` validates its three arguments, and each fault names itself.
    subroutine test_healpix_grid_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_init_nside_zero", &
            expect_abort=.true., &
            failure_message="pf_healpix_grid%init was expected to refuse nside= 0", &
            required_stderr="nside must be a positive power of two")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_init_nside_not_power", &
            expect_abort=.true., &
            failure_message="pf_healpix_grid%init was expected to refuse nside= 100", &
            required_stderr="nside must be a positive power of two")
        if (allocated(error)) return
        ! The message must name the INT32 ceiling: 16384 is legal for the pixelisation and illegal
        ! only for the caller's integer kind, so naming 2**29 would send someone looking in the
        ! wrong place -- the same reasoning as the free procedures' own int32 message.
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_init_nside_int32_ceiling", &
            expect_abort=.true., &
            failure_message="pf_healpix_grid%init was expected to refuse nside= 16384 in int32", &
            required_stderr="at most 8192")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_init_bad_scheme", &
            expect_abort=.true., &
            failure_message="pf_healpix_grid%init was expected to refuse scheme= 7", &
            required_stderr="scheme must be PF_HP_RING")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_init_bad_frame", &
            expect_abort=.true., &
            failure_message="pf_healpix_grid%init was expected to refuse frame= 7", &
            required_stderr="frame must be PF_HP_DEC_NORTH")
    end subroutine test_healpix_grid_init_aborts

    !> Every once-per-query binding refuses a grid `%init` has never run on.
    !>
    !> The elemental bindings cannot: they are `pure`, so they report -1 or -999 instead, which
    !> `test_healpix_grid`'s own suite asserts in process. This covers the half that aborts.
    subroutine test_healpix_grid_unbuilt_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_disc_unset", &
            expect_abort=.true., &
            failure_message="a disc query on an unbuilt grid was expected to abort", &
            required_stderr="this grid has not been built")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_bulk_unset", &
            expect_abort=.true., &
            failure_message="a bulk conversion on an unbuilt grid was expected to abort", &
            required_stderr="this grid has not been built")
    end subroutine test_healpix_grid_unbuilt_aborts

    !> Asking for a grid quantity in a kind that cannot hold it aborts rather than wrapping.
    subroutine test_healpix_grid_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! nside 16384 is legal in int64 and gives npix = 3221225472, above huge(0_int32). This is
        ! the reachable half of the kind-matching rule; %get_nside's own guard is not reachable
        ! through %init, since the nside ceiling 2**29 is itself below huge(0_int32).
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_npix_int32_overflow", &
            expect_abort=.true., &
            failure_message="narrowing npix into an int32 was expected to abort", &
            required_stderr="exceeds the largest integer(int32)")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "healpix_grid_disc_int32_too_fine", &
            expect_abort=.true., &
            failure_message="an int32 disc query on an nside= 16384 grid was expected to abort", &
            required_stderr="the largest an integer(int32) index can address")
    end subroutine test_healpix_grid_int32_aborts

    subroutine test_healpix_nside_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_nside_zero", &
            expect_abort=.true., &
            failure_message="an nside of zero was expected to abort a disc query", &
            required_stderr="nside must be a positive power of two")
    end subroutine test_healpix_nside_zero_aborts

    subroutine test_healpix_nside_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message must name the INT32 ceiling, not the module's own: the value is legal for the
        ! pixelisation and illegal only for the caller's integer kind, and a message naming 2**29
        ! here would send someone looking for a defect in the wrong place.
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_nside_int32_overflow", &
            expect_abort=.true., &
            failure_message="nside 16384 was expected to abort in the int32 kind", &
            required_stderr="at most 8192")
    end subroutine test_healpix_nside_int32_aborts

    subroutine test_healpix_nside_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_nside_int64_overflow", &
            expect_abort=.true., &
            failure_message="nside 2**30 was expected to abort", &
            required_stderr="at most 536870912")
    end subroutine test_healpix_nside_int64_aborts

    subroutine test_healpix_radius_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_radius_nan", &
            expect_abort=.true., &
            failure_message="a NaN radius was expected to abort", &
            required_stderr="radius is NaN")
    end subroutine test_healpix_radius_nan_aborts

    subroutine test_healpix_radius_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_radius_negative", &
            expect_abort=.true., &
            failure_message="a negative radius was expected to abort", &
            required_stderr="radius must be at least zero")
    end subroutine test_healpix_radius_negative_aborts

    subroutine test_healpix_vector_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_vector_zero", &
            expect_abort=.true., &
            failure_message="a zero-length centre vector was expected to abort", &
            required_stderr="has zero length")
    end subroutine test_healpix_vector_zero_aborts

    subroutine test_healpix_vector_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_vector_nan", &
            expect_abort=.true., &
            failure_message="a NaN in the centre vector was expected to abort", &
            required_stderr="holds a NaN")
    end subroutine test_healpix_vector_nan_aborts

    subroutine test_healpix_bad_scheme_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_bad_scheme", &
            expect_abort=.true., &
            failure_message="an unknown scheme selector was expected to abort", &
            required_stderr="scheme must be PF_HP_RING (0) or PF_HP_NEST (1)")
    end subroutine test_healpix_bad_scheme_aborts

    subroutine test_healpix_buffer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message has to carry enough to size the buffer without guessing, which is the whole
        ! reason this aborts rather than truncating -- so the assertion names the count reached.
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_buffer_too_small", &
            expect_abort=.true., &
            failure_message="a listpix too small for the disc was expected to abort", &
            required_stderr="listpix holds 4 elements but the disc needs")
    end subroutine test_healpix_buffer_aborts

    subroutine test_healpix_runs_rows_abort(error)
        type(error_type), allocatable, intent(out) :: error
        ! The row count IS the run form's contract, so a four-row buffer -- the shape this module
        ! records internally, and so the one a reader would reach for -- must be refused rather
        ! than half-filled. The message names the shape it wanted and the shape it got.
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_runs_bad_rows", &
            expect_abort=.true., &
            failure_message="a run buffer with four rows was expected to abort", &
            required_stderr="runs must have exactly 2 rows (first pixel, length), got 4")
    end subroutine test_healpix_runs_rows_abort

    ! ---- Tier B ----
    !
    ! The first two assert the PLUMBING rather than the rule: three entry points share one
    ! validator, and each must name itself in its messages, or a caller is sent to read the
    ! documentation of a routine they never called. The nine rules themselves are covered nine
    ! ways by the scenarios above and are deliberately not re-tested per entry point.

    subroutine test_healpix_count_names_itself(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_count_bad_nside", &
            expect_abort=.true., &
            failure_message="query_disc_count was expected to reject an nside of 6", &
            required_stderr="pf_query_disc_count: nside must be a positive power of two")
    end subroutine test_healpix_count_names_itself

    subroutine test_healpix_alloc_names_itself(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_alloc_bad_scheme", &
            expect_abort=.true., &
            failure_message="query_disc_alloc was expected to reject scheme= 9", &
            required_stderr="pf_query_disc_alloc: scheme must be PF_HP_RING (0) or PF_HP_NEST (1)")
    end subroutine test_healpix_alloc_names_itself

    subroutine test_healpix_max_count_nside_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! It ABORTS where pf_max_pixrad and pf_nside2npix return -1, because a sizing routine that
        ! answered -1 would have the caller allocate a zero-length buffer and meet the real
        ! complaint one call later, naming the query rather than the mistake.
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_max_count_bad_nside", &
            expect_abort=.true., &
            failure_message="query_disc_max_count was expected to reject an nside of 6", &
            required_stderr="pf_query_disc_max_count: nside must be a positive power of two")
    end subroutine test_healpix_max_count_nside_aborts

    subroutine test_healpix_max_count_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_disc_max_count_negative_radius", &
            expect_abort=.true., &
            failure_message="query_disc_max_count was expected to reject a negative radius", &
            required_stderr="pf_query_disc_max_count: radius must be at least zero")
    end subroutine test_healpix_max_count_radius_aborts

    subroutine test_healpix_bulk_nside_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The bulk forms validate where the elemental forms they wrap do not: the check is once
        ! per array, and a bad nside read from a file would otherwise become a whole array of
        ! silently wrong pixels rather than one loud failure.
        call check_scenario_exit_status_and_stderr(error, "healpix_bulk_nside_invalid", &
            expect_abort=.true., &
            failure_message="a bulk call with nside 0 was expected to abort", &
            required_stderr="pf_ang2pix_ring_bulk: nside must be a positive power of two")
    end subroutine test_healpix_bulk_nside_aborts

    subroutine test_healpix_bulk_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_bulk_size_mismatch", &
            expect_abort=.true., &
            failure_message="a bulk call with a short output array was expected to abort", &
            required_stderr="every array must have the same extent")
    end subroutine test_healpix_bulk_size_aborts

    subroutine test_healpix_bulk_threads_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_bulk_threads_zero", &
            expect_abort=.true., &
            failure_message="a bulk call with threads= 0 was expected to abort", &
            required_stderr="threads= must be at least 1")
    end subroutine test_healpix_bulk_threads_aborts

    subroutine test_healpix_bulk_vec_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "healpix_bulk_vec_shape", &
            expect_abort=.true., &
            failure_message="a bulk call with a (2, n) vec array was expected to abort", &
            required_stderr="vec must be shaped (3, n)")
    end subroutine test_healpix_bulk_vec_shape_aborts

    !
    ! ---- Non-finite coordinates, points and radii (pf_spatial_index) ----
    !
    ! One check, `spatial_check_finite`, and eight call sites; a ninth test covers the separate
    ! `radius=` fix. The realistic future defect is a DROPPED CALL SITE rather than a broken check,
    ! which is why each entry point gets its own test and each asserts the message names it.
    !
    ! What these replace is worth stating, because it is why they are worth nine tests: before the
    ! check, every one of these inputs reached `int()` of a NaN or a `min`/`max` over one -- both
    ! instructions signal on a quiet NaN -- so under nagfor's default `-ieee=stop` the process died
    ! with "Arithmetic exception: Floating invalid operation", naming neither the entry point nor
    ! the argument, and only in an optimised build. Under every other compiler in the fleet the
    ! traps are masked and the search silently answered from a garbage cell index.
    !
    ! Their negative control is the `spatial` suite itself: 74 tests that build, rebuild and query
    ! with finite data, none of which a check that fired unconditionally could survive.

    subroutine test_spatial_build_nan_coord_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_nan_coord", expect_abort=.true., &
            failure_message="a NaN coordinate was expected to be refused by %build", &
            required_stderr="%build: every x coordinate must be a finite number")
    end subroutine test_spatial_build_nan_coord_aborts

    subroutine test_spatial_build_inf_coord_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_inf_coord", expect_abort=.true., &
            failure_message="an infinite coordinate was expected to be refused by %build", &
            required_stderr="%build: every z coordinate must be a finite number")
    end subroutine test_spatial_build_inf_coord_aborts

    subroutine test_spatial_build_nan_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_nan_radius", expect_abort=.true., &
            failure_message="a NaN radius= was expected to be refused", &
            required_stderr="every radius= must be > 0")
    end subroutine test_spatial_build_nan_radius_aborts

    subroutine test_spatial_build_sky_nan_ra_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_sky_nan_ra", expect_abort=.true., &
            failure_message="a NaN ra was expected to be refused by %build_sky", &
            required_stderr="%build_sky: every ra must be a finite number")
    end subroutine test_spatial_build_sky_nan_ra_aborts

    subroutine test_spatial_rebuild_nan_coord_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_nan_coord", expect_abort=.true., &
            failure_message="a NaN coordinate was expected to be refused by %rebuild", &
            required_stderr="%rebuild: every y coordinate must be a finite number")
    end subroutine test_spatial_rebuild_nan_coord_aborts

    subroutine test_spatial_query_nan_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_query_nan_point", expect_abort=.true., &
            failure_message="a NaN query point was expected to be refused by %within", &
            required_stderr="every query point coordinate must be a finite number")
    end subroutine test_spatial_query_nan_point_aborts

    subroutine test_spatial_segment_nan_endpoint_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_segment_nan_endpoint", expect_abort=.true., &
            failure_message="a NaN axis endpoint was expected to be refused by %within_segment", &
            required_stderr="%within_segment: every axis endpoint coordinate must be a finite number")
    end subroutine test_spatial_segment_nan_endpoint_aborts

    subroutine test_spatial_axis_length_overflows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_length_overflows", expect_abort=.true., &
            failure_message="an axis whose squared length overflows was expected to be refused", &
            required_stderr="the axis is too long for its squared length to be formed")
        if (allocated(error)) return
        ! The control past `2**511` must be answered, or a guard refusing every long axis passes too.
        call check_scenario_streams(error, "spatial_axis_length_overflows", "a 1e154-long axis was answered", &
            expect_on="stdout", failure_message="an axis whose squared length fits was expected to be answered")
    end subroutine test_spatial_axis_length_overflows_aborts

    subroutine test_spatial_nearest_nan_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_nan_point", expect_abort=.true., &
            failure_message="a NaN query point was expected to be refused by %nearest", &
            required_stderr="%nearest: every query point coordinate must be a finite number")
    end subroutine test_spatial_nearest_nan_point_aborts

    subroutine test_spatial_sky_query_nan_dec_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_query_nan_dec", expect_abort=.true., &
            failure_message="a NaN dec was expected to be refused by %within_sky", &
            required_stderr="%within_sky: every query ra/dec must be a finite number")
    end subroutine test_spatial_sky_query_nan_dec_aborts

    subroutine test_spatial_bulk_nan_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_nan_radius", expect_abort=.true., &
            failure_message="a NaN in a per-point radius array was expected to be refused", &
            required_stderr="every radius must be >= 0")
    end subroutine test_spatial_bulk_nan_radius_aborts

    subroutine test_spatial_bulk_nan_inner_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_nan_inner_radius", expect_abort=.true., &
            failure_message="a NaN inner radius was expected to be refused", &
            required_stderr="every inner radius must be >= 0")
    end subroutine test_spatial_bulk_nan_inner_radius_aborts

    subroutine test_spatial_rebuild_for_nan_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_for_nan_radius", expect_abort=.true., &
            failure_message="a NaN radius handed to %rebuild_for was expected to be refused", &
            required_stderr="%rebuild_for: every radius must be > 0")
    end subroutine test_spatial_rebuild_for_nan_radius_aborts

    subroutine test_spatial_radius_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_radius_not_positive", expect_abort=.true., &
            failure_message="a radius hint of zero was expected to abort", &
            required_stderr="every radius= must be > 0")
    end subroutine test_spatial_radius_not_positive_aborts

    subroutine test_spatial_box_needs_both_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_box_needs_both", expect_abort=.true., &
            failure_message="box_lo= without box_hi= was expected to abort", &
            required_stderr="need both box_lo= and box_hi=")
    end subroutine test_spatial_box_needs_both_aborts

    !> Beyond `L/2` the minimum image is ambiguous, so this must abort rather than clamp: a point
    !> can be its own neighbour through two images and there is no answer to give.
    subroutine test_spatial_half_box_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_radius_exceeds_half_box", expect_abort=.true., &
            failure_message="a periodic radius above half the box was expected to abort", &
            required_stderr="must not exceed half the box on any axis")
    end subroutine test_spatial_half_box_aborts

    subroutine test_spatial_query_rank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_query_rank_mismatch", expect_abort=.true., &
            failure_message="querying a 2D index with a 3D point was expected to abort", &
            required_stderr="as many coordinates as the index was built with")
    end subroutine test_spatial_query_rank_aborts

    subroutine test_spatial_axis_periodic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_on_periodic", expect_abort=.true., &
            failure_message="an axis query on a periodic index was expected to abort", &
            required_stderr="not supported on a periodic index")
    end subroutine test_spatial_axis_periodic_aborts

    subroutine test_spatial_sky_on_euclidean_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_query_on_euclidean", expect_abort=.true., &
            failure_message="a sky query on a Euclidean index was expected to abort", &
            required_stderr="built with %build, not %build_sky")
    end subroutine test_spatial_sky_on_euclidean_aborts

    subroutine test_spatial_euclidean_on_sky_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_euclidean_query_on_sky", expect_abort=.true., &
            failure_message="a Euclidean query on a sky index was expected to abort", &
            required_stderr="use %within_sky, which answers in degrees")
    end subroutine test_spatial_euclidean_on_sky_aborts

    subroutine test_spatial_sky_bulk_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_bulk_refused", expect_abort=.true., &
            failure_message="a bulk sweep on a sky index was expected to abort", &
            required_stderr="use the _sky bulk forms")
    end subroutine test_spatial_sky_bulk_aborts

    subroutine test_spatial_sky_bulk_euclidean_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_bulk_on_euclidean", expect_abort=.true., &
            failure_message="a sky bulk sweep on a Euclidean index was expected to abort", &
            required_stderr="this is a Euclidean index; use the plain bulk forms")
    end subroutine test_spatial_sky_bulk_euclidean_aborts

    subroutine test_spatial_sky_rsky_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_rsky_too_large", expect_abort=.true., &
            failure_message="an angular radius past a hemisphere was expected to abort", &
            required_stderr="above 90 degrees is not a neighbour search")
    end subroutine test_spatial_sky_rsky_aborts

    subroutine test_spatial_rebuild_for_sky_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_for_sky_too_large", expect_abort=.true., &
            failure_message="a %rebuild_for radius past a hemisphere was expected to abort", &
            required_stderr="above 90 degrees is not a neighbour search")
    end subroutine test_spatial_rebuild_for_sky_too_large_aborts

    subroutine test_spatial_sky_dec_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_dec_out_of_range", expect_abort=.true., &
            failure_message="a declination outside [-90, 90] was expected to abort", &
            required_stderr="every dec must lie in [-90, 90] degrees")
    end subroutine test_spatial_sky_dec_aborts

    subroutine test_spatial_sky_rebuild_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_rebuild_refused", expect_abort=.true., &
            failure_message="rebuilding a sky index was expected to abort", &
            required_stderr="rebuild it with %build_sky")
    end subroutine test_spatial_sky_rebuild_aborts

    subroutine test_spatial_sky_bad_backend_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_bad_backend", expect_abort=.true., &
            failure_message="an unknown backend= was expected to abort", &
            required_stderr="backend= must be PF_SKY_GRID3D or PF_SKY_HEALPIX")
    end subroutine test_spatial_sky_bad_backend_aborts

    subroutine test_spatial_sky_cell_healpix_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_cell_with_healpix", expect_abort=.true., &
            failure_message="cell= on a HEALPix index was expected to abort", &
            required_stderr="has no meaning for backend=PF_SKY_HEALPIX")
    end subroutine test_spatial_sky_cell_healpix_aborts

    subroutine test_spatial_sky_nside_no_healpix_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_nside_without_healpix", expect_abort=.true., &
            failure_message="nside= on a 3D-grid index was expected to abort", &
            required_stderr="nside= is the HEALPix resolution and needs")
    end subroutine test_spatial_sky_nside_no_healpix_aborts

    subroutine test_spatial_sky_nside_power2_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_nside_not_power2", expect_abort=.true., &
            failure_message="an nside= that is not a power of two was expected to abort", &
            required_stderr="nside= must be a power of two in 1 .. 2**29")
    end subroutine test_spatial_sky_nside_power2_aborts

    subroutine test_spatial_sky_nside_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_nside_zero", expect_abort=.true., &
            failure_message="nside=0 was expected to abort", &
            required_stderr="nside= must be a power of two in 1 .. 2**29")
    end subroutine test_spatial_sky_nside_zero_aborts

    subroutine test_spatial_axis_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_before_build", expect_abort=.true., &
            failure_message="an axis query before %build was expected to abort", &
            required_stderr="within_cone: this index has not been built")
    end subroutine test_spatial_axis_before_build_aborts

    subroutine test_spatial_axis_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_radius_negative", expect_abort=.true., &
            failure_message="a negative cone radius was expected to abort", &
            required_stderr="every radius must be >= 0")
    end subroutine test_spatial_axis_radius_aborts

    subroutine test_spatial_annulus_inner_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_annulus_inner_exceeds_outer", expect_abort=.true., &
            failure_message="an inner radius above the outer one was expected to abort", &
            required_stderr="the inner radius must not exceed the outer radius")
    end subroutine test_spatial_annulus_inner_aborts

    subroutine test_spatial_annulus_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_annulus_inner_negative", expect_abort=.true., &
            failure_message="a negative inner radius was expected to abort", &
            required_stderr="the inner radius must be >= 0")
    end subroutine test_spatial_annulus_negative_aborts

    subroutine test_spatial_bulk_inner_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_inner_length", expect_abort=.true., &
            failure_message="a mis-sized bulk inner radius was expected to abort", &
            required_stderr="the inner radius must be one value or one per point")
    end subroutine test_spatial_bulk_inner_length_aborts

    subroutine test_spatial_bulk_inner_exceeds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_inner_exceeds_outer", expect_abort=.true., &
            failure_message="a bulk inner radius above one point's outer radius was expected to abort", &
            required_stderr="must not exceed the outer radius for that point")
    end subroutine test_spatial_bulk_inner_exceeds_aborts

    subroutine test_spatial_sky_annulus_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_annulus_too_large", expect_abort=.true., &
            failure_message="an inner angular radius above the outer one was expected to abort", &
            required_stderr="the inner angular radius must not exceed the outer one")
    end subroutine test_spatial_sky_annulus_aborts

    subroutine test_spatial_axis_point_rank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_point_rank", expect_abort=.true., &
            failure_message="a mis-shaped axis_point buffer was expected to abort", &
            required_stderr="axis_point's first extent must equal %ndim()")
    end subroutine test_spatial_axis_point_rank_aborts

    subroutine test_spatial_nearest_k_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_k_below_one", expect_abort=.true., &
            failure_message="k below one was expected to abort", &
            required_stderr="nearest: k must be >= 1")
    end subroutine test_spatial_nearest_k_aborts

    subroutine test_spatial_nearest_periodic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_periodic_unreachable", expect_abort=.true., &
            failure_message="a periodic nearest needing more than half the box was expected to abort", &
            required_stderr="half the box does not hold that many neighbours")
    end subroutine test_spatial_nearest_periodic_aborts

    subroutine test_spatial_nearest_sky_euclidean_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_sky_on_euclidean", expect_abort=.true., &
            failure_message="%nearest_sky on a Euclidean index was expected to abort", &
            required_stderr="not %build_sky; use %nearest")
    end subroutine test_spatial_nearest_sky_euclidean_aborts

    subroutine test_spatial_kth_k_low_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_kth_k_below_one", expect_abort=.true., &
            failure_message="kth_distance with k below one was expected to abort", &
            required_stderr="kth_distance: k must be >= 1")
    end subroutine test_spatial_kth_k_low_aborts

    subroutine test_spatial_kth_k_high_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_kth_k_too_large", expect_abort=.true., &
            failure_message="kth_distance with k at the catalogue size was expected to abort", &
            required_stderr="k must be at most %size()-1")
    end subroutine test_spatial_kth_k_high_aborts

    subroutine test_spatial_kth_on_sky_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_kth_on_sky_index", expect_abort=.true., &
            failure_message="%kth_distance on a sky index was expected to abort", &
            required_stderr="use %kth_distance_sky")
    end subroutine test_spatial_kth_on_sky_aborts

    subroutine test_spatial_kth_sky_euclidean_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_kth_sky_on_euclidean", expect_abort=.true., &
            failure_message="%kth_distance_sky on a Euclidean index was expected to abort", &
            required_stderr="this is a Euclidean index; use %kth_distance")
    end subroutine test_spatial_kth_sky_euclidean_aborts

    subroutine test_spatial_components_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_length", expect_abort=.true., &
            failure_message="mismatched endpoint arrays were expected to abort", &
            required_stderr="the two endpoint arrays must be the same length")
    end subroutine test_spatial_components_length_aborts

    subroutine test_spatial_components_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_endpoint_range", expect_abort=.true., &
            failure_message="an out-of-range edge endpoint was expected to abort", &
            required_stderr="every edge endpoint must be a vertex in 1..nvert")
    end subroutine test_spatial_components_range_aborts

    subroutine test_spatial_components_min_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_min_size_zero", expect_abort=.true., &
            failure_message="min_size = 0 was expected to abort", &
            required_stderr="min_size must be >= 1")
    end subroutine test_spatial_components_min_size_aborts

    subroutine test_spatial_components_nvert_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_nvert_negative", expect_abort=.true., &
            failure_message="a negative vertex count was expected to abort", &
            required_stderr="nvert must be >= 0")
    end subroutine test_spatial_components_nvert_aborts

    subroutine test_spatial_sky_query_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_query_before_build", expect_abort=.true., &
            failure_message="a sky query on an unbuilt index was expected to abort", &
            required_stderr="this index has not been built; call %build_sky first")
    end subroutine test_spatial_sky_query_before_build_aborts

    subroutine test_spatial_sky_radius_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_radius_nan", expect_abort=.true., &
            failure_message="a NaN angular radius was expected to abort", &
            required_stderr="the angular radius must be >= 0 and not NaN")
    end subroutine test_spatial_sky_radius_nan_aborts

    subroutine test_spatial_sky_inner_radius_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_inner_radius_nan", expect_abort=.true., &
            failure_message="a NaN inner angular radius was expected to abort", &
            required_stderr="the inner angular radius must be >= 0 and not NaN")
    end subroutine test_spatial_sky_inner_radius_nan_aborts

    subroutine test_spatial_sky_radii_vector_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_radii_vector_nan", expect_abort=.true., &
            failure_message="a NaN in an angular radius list was expected to abort", &
            required_stderr="every angular radius must be >= 0 and not NaN")
    end subroutine test_spatial_sky_radii_vector_nan_aborts

    subroutine test_spatial_nearest_sky_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_sky_before_build", expect_abort=.true., &
            failure_message="nearest_sky on an unbuilt index was expected to abort", &
            required_stderr="this index has not been built; call %build_sky first")
    end subroutine test_spatial_nearest_sky_before_build_aborts

    subroutine test_spatial_query_radius_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_query_radius_nan", expect_abort=.true., &
            failure_message="a NaN query radius was expected to abort", &
            required_stderr="the search radius must be >= 0 and not NaN")
    end subroutine test_spatial_query_radius_nan_aborts

    subroutine test_spatial_query_radius_half_box_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_query_radius_half_box", expect_abort=.true., &
            failure_message="a periodic query radius above half the box was expected to abort", &
            required_stderr="a periodic search radius must not exceed half the box")
    end subroutine test_spatial_query_radius_half_box_aborts

    subroutine test_spatial_build_sky_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_sky_length_mismatch", expect_abort=.true., &
            failure_message="ra and dec of different lengths were expected to abort", &
            required_stderr="ra and dec must be the same length")
    end subroutine test_spatial_build_sky_length_mismatch_aborts

    subroutine test_spatial_build_sky_no_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_sky_no_radius", expect_abort=.true., &
            failure_message="an empty radius_deg= list was expected to abort", &
            required_stderr="radius_deg= must name at least one radius")
    end subroutine test_spatial_build_sky_no_radius_aborts

    subroutine test_spatial_build_sky_radius_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_sky_radius_not_positive", expect_abort=.true., &
            failure_message="a radius_deg= of zero was expected to abort", &
            required_stderr="every radius_deg= must be > 0")
    end subroutine test_spatial_build_sky_radius_not_positive_aborts

    subroutine test_spatial_rebuild_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_before_build", expect_abort=.true., &
            failure_message="rebuild on an unbuilt index was expected to abort", &
            required_stderr="pf_spatial_index%rebuild: this index has not been built; call %build first")
    end subroutine test_spatial_rebuild_before_build_aborts

    subroutine test_spatial_rebuild_z_rank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_z_rank", expect_abort=.true., &
            failure_message="rebuild without z was expected to abort", &
            required_stderr="z must be supplied exactly as it was to %build")
    end subroutine test_spatial_rebuild_z_rank_aborts

    subroutine test_spatial_rebuild_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_length_mismatch", expect_abort=.true., &
            failure_message="rebuild from x and y of different lengths was expected to abort", &
            required_stderr="pf_spatial_index%rebuild: x and y must be the same length")
    end subroutine test_spatial_rebuild_length_mismatch_aborts

    subroutine test_spatial_rebuild_at_observer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_at_observer", expect_abort=.true., &
            failure_message="a point rebuilt onto the observer was expected to abort", &
            required_stderr="a point coincides with the observer")
    end subroutine test_spatial_rebuild_at_observer_aborts

    subroutine test_spatial_rebuild_for_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_for_before_build", expect_abort=.true., &
            failure_message="rebuild_for on an unbuilt index was expected to abort", &
            required_stderr="pf_spatial_index%rebuild_for: this index has not been built; call %build first")
    end subroutine test_spatial_rebuild_for_before_build_aborts

    subroutine test_spatial_rebuild_for_half_box_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_for_half_box", expect_abort=.true., &
            failure_message="rebuild_for above half a periodic box was expected to abort", &
            required_stderr="a periodic search radius must not exceed half the box")
    end subroutine test_spatial_rebuild_for_half_box_aborts

    subroutine test_spatial_build_box_rank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_box_rank", expect_abort=.true., &
            failure_message="a box with too few entries was expected to abort", &
            required_stderr="box_lo=/box_hi= must have one entry per coordinate")
    end subroutine test_spatial_build_box_rank_aborts

    subroutine test_spatial_build_box_not_strict_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_build_box_not_strict", expect_abort=.true., &
            failure_message="a box with zero extent was expected to abort", &
            required_stderr="box_hi= must be strictly above box_lo= on every axis")
    end subroutine test_spatial_build_box_not_strict_aborts

    subroutine test_spatial_copy_false_z_strided_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_copy_false_z_strided", expect_abort=.true., &
            failure_message="a strided z with copy=.false. was expected to abort", &
            required_stderr="copy=.false. needs a contiguous z")
    end subroutine test_spatial_copy_false_z_strided_aborts

    subroutine test_spatial_bulk_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_before_build", expect_abort=.true., &
            failure_message="a bulk sweep on an unbuilt index was expected to abort", &
            required_stderr="this index has not been built; call %build first")
    end subroutine test_spatial_bulk_before_build_aborts

    subroutine test_spatial_los_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_before_build", expect_abort=.true., &
            failure_message="a line-of-sight sweep on an unbuilt index was expected to abort", &
            required_stderr="this index has not been built; call %build first")
    end subroutine test_spatial_los_before_build_aborts

    subroutine test_spatial_los_bperp_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_bperp_negative", expect_abort=.true., &
            failure_message="a negative transverse length was expected to abort", &
            required_stderr="every b_perp and b_par must be >= 0 and not NaN")
    end subroutine test_spatial_los_bperp_negative_aborts

    subroutine test_spatial_kth_before_build_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_kth_before_build", expect_abort=.true., &
            failure_message="kth_distance on an unbuilt index was expected to abort", &
            required_stderr="pf_spatial_index%kth_distance: this index has not been built; call %build first")
    end subroutine test_spatial_kth_before_build_aborts

    subroutine test_spatial_components_i32_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_i32_length", expect_abort=.true., &
            failure_message="int32 endpoint arrays of different lengths were expected to abort", &
            required_stderr="the two endpoint arrays must be the same length")
    end subroutine test_spatial_components_i32_length_aborts

    subroutine test_spatial_components_nvert_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_components_nvert_int32", expect_abort=.true., &
            failure_message="an int32 answer over too many vertices was expected to abort", &
            required_stderr="nvert is larger than an int32 answer can name")
    end subroutine test_spatial_components_nvert_int32_aborts

    subroutine test_spatial_within_int32_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_int32_ceiling", expect_abort=.true., &
            failure_message="an int32 within buffer over the ceiling was expected to abort", &
            required_stderr="this index holds more rows than an int32 buffer can name")
    end subroutine test_spatial_within_int32_ceiling_aborts

    subroutine test_spatial_axis_int32_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_axis_int32_ceiling", expect_abort=.true., &
            failure_message="an int32 axis buffer over the ceiling was expected to abort", &
            required_stderr="this index holds more rows than an int32 buffer can name")
    end subroutine test_spatial_axis_int32_ceiling_aborts

    subroutine test_spatial_nearest_int32_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nearest_int32_ceiling", expect_abort=.true., &
            failure_message="an int32 nearest buffer over the ceiling was expected to abort", &
            required_stderr="this index holds more rows than an int32 buffer can name")
    end subroutine test_spatial_nearest_int32_ceiling_aborts

    subroutine test_spatial_grid_int32_ceiling_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_grid_int32_ceiling", expect_abort=.true., &
            failure_message="a cell count over the int32 ceiling was expected to abort", &
            required_stderr="above the largest int32 answer")
    end subroutine test_spatial_grid_int32_ceiling_aborts

    subroutine test_spatial_sky_rebuild_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_rebuild_warns", expect_abort=.false., &
            failure_message="a sky index re-tuned by a distant radius was expected to warn", &
            required_stderr="rebuilt for a query radius far from the one it was built for")
    end subroutine test_spatial_sky_rebuild_warns

    subroutine test_spatial_nside_coarsened_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_nside_coarsened_warns", expect_abort=.false., &
            failure_message="a coarsened nside= was expected to warn and carry on", &
            required_stderr="nside= was coarsened from")
    end subroutine test_spatial_nside_coarsened_warns

    subroutine test_spatial_debug_work_no_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_debug_work_no_radius", expect_abort=.true., &
            failure_message="the work hook with no radius was expected to abort", &
            required_stderr="parquet_debug_spatial_work: name at least one radius")
    end subroutine test_spatial_debug_work_no_radius_aborts

    subroutine test_spatial_rebuild_needs_copy_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_needs_copy", expect_abort=.true., &
            failure_message="rebuilding a copy=.false. index was expected to abort", &
            required_stderr="holds no copy to compare against")
    end subroutine test_spatial_rebuild_needs_copy_aborts

    subroutine test_spatial_bulk_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_bulk_radius_length", expect_abort=.true., &
            failure_message="a radius array of the wrong length was expected to abort", &
            required_stderr="radius must be one value or one per point")
    end subroutine test_spatial_bulk_radius_aborts

    subroutine test_spatial_pairs_bad_combine_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_pairs_bad_combine", expect_abort=.true., &
            failure_message="an unknown combine= value was expected to abort", &
            required_stderr="combine= must be PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN or PF_LINK_SUM")
    end subroutine test_spatial_pairs_bad_combine_aborts

    !> See `scenario_spatial_pairs_int32_rows` (test/error_scenarios.f90). The scenario answers the
    !> same query in `int64` first, so a run that aborts proves the ceiling governs the ANSWER's
    !> kind and not the query.
    subroutine test_spatial_pairs_int32_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_pairs_int32_rows", expect_abort=.true., &
            failure_message="an int32 pair list over an index too large to name was expected to abort", &
            required_stderr="%pairs_within: this index holds more rows than an int32 answer can name")
    end subroutine test_spatial_pairs_int32_rows_aborts

    !> See `scenario_spatial_csr_int32_offsets` (test/error_scenarios.f90). The required text is
    !> what pins the guard to the NEIGHBOUR TOTAL: a guard written against the row count would let
    !> this call through, since the ceiling is deliberately above the row count.
    subroutine test_spatial_csr_int32_offsets_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_csr_int32_offsets", expect_abort=.true., &
            failure_message="an int32 CSR over too long a neighbour list was expected to abort", &
            required_stderr="the neighbour list is longer than an int32 offset can name")
    end subroutine test_spatial_csr_int32_offsets_aborts

    !> The message must name the rule's own 45-degree limit, not the general 90-degree ceiling:
    !> the check runs ahead of the chord conversion precisely so that a caller reads the number
    !> that applies to the call they made.
    subroutine test_spatial_sky_pairs_sum_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_sky_pairs_sum_too_large", expect_abort=.true., &
            failure_message="PF_LINK_SUM past 45 degrees on the sky was expected to abort", &
            required_stderr="every angular radius must be <= 45 degrees")
    end subroutine test_spatial_sky_pairs_sum_too_large_aborts

    ! ---- The line-of-sight cylinder: %within_los, %pairs_within_los, observer= and los= ----

    subroutine test_spatial_los_on_sky_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_on_sky", expect_abort=.true., &
            failure_message="pairs_within_los on a sky index was expected to abort", &
            required_stderr="built with %build_sky; a line-of-sight cylinder needs Cartesian coordinates")
    end subroutine test_spatial_los_on_sky_aborts

    subroutine test_spatial_los_on_2d_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_on_2d", expect_abort=.true., &
            failure_message="pairs_within_los on a 2D index was expected to abort", &
            required_stderr="this index is two-dimensional; a line of sight needs three coordinates")
    end subroutine test_spatial_los_on_2d_aborts

    subroutine test_spatial_los_on_periodic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_on_periodic", expect_abort=.true., &
            failure_message="pairs_within_los on a periodic index was expected to abort", &
            required_stderr="this index is periodic; a line of sight has no meaning under the minimum image")
    end subroutine test_spatial_los_on_periodic_aborts

    !> The query-time half of the point-on-the-observer refusal: an index built without `los=` keeps
    !> accepting a point at the origin, as every %build always has, and the sweep is what refuses.
    subroutine test_spatial_los_point_at_observer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_point_at_observer", expect_abort=.true., &
            failure_message="a stored point on the observer was expected to abort the sweep", &
            required_stderr="pairs_within_los: a stored point coincides with the observer (distance 0)")
    end subroutine test_spatial_los_point_at_observer_aborts

    subroutine test_spatial_within_los_point_at_observer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_point_at_observer", expect_abort=.true., &
            failure_message="a query from the observer itself was expected to abort", &
            required_stderr="the query point coincides with the observer, so it has no line of sight")
    end subroutine test_spatial_within_los_point_at_observer_aborts

    subroutine test_spatial_los_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_length_mismatch", expect_abort=.true., &
            failure_message="length lists of different lengths were expected to abort", &
            required_stderr="b_perp and b_par must be the same length")
    end subroutine test_spatial_los_length_mismatch_aborts

    subroutine test_spatial_los_lengths_not_per_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_lengths_not_per_point", expect_abort=.true., &
            failure_message="three lengths for 64 points were expected to abort", &
            required_stderr="b_perp and b_par must be one value each or one per point each")
    end subroutine test_spatial_los_lengths_not_per_point_aborts

    subroutine test_spatial_los_negative_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_negative_length", expect_abort=.true., &
            failure_message="a negative parallel length was expected to abort", &
            required_stderr="every b_perp and b_par must be >= 0 and not NaN")
    end subroutine test_spatial_los_negative_length_aborts

    subroutine test_spatial_los_bad_combine_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_bad_combine", expect_abort=.true., &
            failure_message="an unknown combine= on the cylinder sweep was expected to abort", &
            required_stderr="pairs_within_los: combine= must be PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN or PF_LINK_SUM")
    end subroutine test_spatial_los_bad_combine_aborts

    subroutine test_spatial_within_los_needs_los_p_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_needs_los_p", expect_abort=.true., &
            failure_message="within_los without los_p on a los= index was expected to abort", &
            required_stderr="so los_p= is required")
    end subroutine test_spatial_within_los_needs_los_p_aborts

    subroutine test_spatial_within_los_los_p_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_los_p_refused", expect_abort=.true., &
            failure_message="within_los with los_p on an index without los= was expected to abort", &
            required_stderr="so los_p= has no meaning here")
    end subroutine test_spatial_within_los_los_p_refused_aborts

    subroutine test_spatial_within_los_zero_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_zero_length", expect_abort=.true., &
            failure_message="a zero length on within_los was expected to abort", &
            required_stderr="b_perp and b_par must both be > 0 and not NaN")
    end subroutine test_spatial_within_los_zero_length_aborts

    subroutine test_spatial_within_los_rank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_rank", expect_abort=.true., &
            failure_message="a two-coordinate point on within_los was expected to abort", &
            required_stderr="within_los: the query point must have three coordinates")
    end subroutine test_spatial_within_los_rank_aborts

    subroutine test_spatial_within_los_los_p_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_within_los_los_p_nan", expect_abort=.true., &
            failure_message="a NaN los_p was expected to abort", &
            required_stderr="los_p must be a finite number")
    end subroutine test_spatial_within_los_los_p_nan_aborts

    subroutine test_spatial_los_constant_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_constant", expect_abort=.true., &
            failure_message="a constant los= was expected to abort %build", &
            required_stderr="build: los= is constant, or varies by less than 64 ulps")
    end subroutine test_spatial_los_constant_aborts

    subroutine test_spatial_los_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_nan", expect_abort=.true., &
            failure_message="a NaN in los= was expected to abort %build", &
            required_stderr="build: every los value must be a finite number")
    end subroutine test_spatial_los_nan_aborts

    subroutine test_spatial_los_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_length", expect_abort=.true., &
            failure_message="a short los= was expected to abort %build", &
            required_stderr="build: los= must have one entry per point")
    end subroutine test_spatial_los_length_aborts

    subroutine test_spatial_los_build_on_2d_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_build_on_2d", expect_abort=.true., &
            failure_message="los= on a 2D build was expected to abort", &
            required_stderr="observer= and los= describe a line of sight")
    end subroutine test_spatial_los_build_on_2d_aborts

    subroutine test_spatial_los_build_on_periodic_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_build_on_periodic", expect_abort=.true., &
            failure_message="observer= on a periodic build was expected to abort", &
            required_stderr="observer= and los= describe a line of sight")
    end subroutine test_spatial_los_build_on_periodic_aborts

    subroutine test_spatial_los_observer_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_observer_size", expect_abort=.true., &
            failure_message="a two-coordinate observer= was expected to abort", &
            required_stderr="observer= must have exactly three coordinates")
    end subroutine test_spatial_los_observer_size_aborts

    subroutine test_spatial_los_observer_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_observer_nan", expect_abort=.true., &
            failure_message="a NaN observer= was expected to abort", &
            required_stderr="every observer coordinate must be a finite number")
    end subroutine test_spatial_los_observer_nan_aborts

    !> The build-time half of the point-on-the-observer refusal: `los=` declares the intent, so the
    !> build refuses rather than leaving it to the first sweep.
    subroutine test_spatial_los_build_point_at_observer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_los_build_point_at_observer", expect_abort=.true., &
            failure_message="a point on the observer was expected to abort a los= build", &
            required_stderr="build: a point coincides with the observer (distance 0)")
    end subroutine test_spatial_los_build_point_at_observer_aborts

    subroutine test_spatial_rebuild_los_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_los_missing", expect_abort=.true., &
            failure_message="rebuild without los= on a los= index was expected to abort", &
            required_stderr="so %rebuild needs los= with the new values")
    end subroutine test_spatial_rebuild_los_missing_aborts

    subroutine test_spatial_rebuild_los_unexpected_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_los_unexpected", expect_abort=.true., &
            failure_message="rebuild with los= on an index without one was expected to abort", &
            required_stderr="so %rebuild takes none")
    end subroutine test_spatial_rebuild_los_unexpected_aborts

    subroutine test_spatial_rebuild_los_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_los_length", expect_abort=.true., &
            failure_message="rebuild with a short los= was expected to abort", &
            required_stderr="rebuild: los= must have one entry per point")
    end subroutine test_spatial_rebuild_los_length_aborts

    !> The warning must land on the stream the settings name and nowhere else, or it is decorative
    !> (`feature_risks.md` Risk-41's shape); the scenario itself exits cleanly, since a `los` that
    !> is not a function of the distance is accepted.
    subroutine test_spatial_los_not_a_function_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "spatial_los_not_a_function_warns", &
            "los= is not a function of the distance from the observer", &
            "stderr", "a los= unrelated to the distance must warn at %build")
    end subroutine test_spatial_los_not_a_function_warns

    subroutine test_spatial_los_window_spans_catalogue_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "spatial_los_window_spans_catalogue_warns", &
            "the parallel window spans the whole catalogue along every line of sight", &
            "stderr", "a parallel window spanning the catalogue must warn at the sweep")
    end subroutine test_spatial_los_window_spans_catalogue_warns

    !> A `contiguous` dummy would copy a strided actual into a temporary that dies at the end of the
    !> call, leaving the index pointing at freed memory with nothing able to detect it -- so this
    !> must be refused at build time rather than diagnosed later.
    subroutine test_spatial_copy_false_strided_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_copy_false_strided", expect_abort=.true., &
            failure_message="copy=.false. over a strided section was expected to abort", &
            required_stderr="needs contiguous x and y")
    end subroutine test_spatial_copy_false_strided_aborts

    subroutine test_spatial_threads_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "spatial_threads_below_one", expect_abort=.true., &
            failure_message="threads= 0 on a bulk query was expected to abort", &
            required_stderr="threads= must be >= 1")
    end subroutine test_spatial_threads_below_one_aborts

    !> The rebuild advice, said and silenced, WITH its negative control.
    !>
    !> Both scenarios rebuild -- each prints `rebuilds=1` -- so the only thing that differs is
    !> whether the library says so. Asserting only the first half would pass just as happily
    !> against a verbosity level that silenced the rebuild itself.
    !>
    !> This replaced the observed-effect test for `spatial_rebuild_warning`: the message that knob
    !> governed is advice now, so `verbosity` governs it with every other piece of advice.
    subroutine test_spatial_rebuild_advice(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_advice_normal", expect_abort=.false., &
            failure_message="the rebuild-advice scenario was not expected to abort", &
            required_stderr="NOTE: pf_spatial_index: rebuilt for a query radius far from the one it was built for")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_advice_normal", expect_abort=.false., &
            failure_message="the rebuild-advice scenario was expected to rebuild", &
            required_stderr="rebuilds=1")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "spatial_rebuild_advice_silent", expect_abort=.false., &
            failure_message="the silenced rebuild-advice scenario was not expected to abort", &
            forbidden_text="rebuilt for a query radius")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "spatial_rebuild_advice_silent", expect_abort=.false., &
            failure_message="silencing the advice must not silence the rebuild it describes", &
            required_stderr="rebuilds=1")
    end subroutine test_spatial_rebuild_advice

    !> Every configuration mistake `parquet_logging` refuses, and the negative control that
    !> proves the refusals are not simply firing unconditionally.
    subroutine test_logging_configuration_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_unknown_layout_field", expect_abort=.true., &
            failure_message="a layout template naming an unknown field was expected to abort", &
            required_stderr="unknown field")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "logging_second_console_sink", expect_abort=.true., &
            failure_message="a second console sink on one stream was expected to abort", &
            required_stderr="would double every line")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "logging_too_many_sinks", expect_abort=.true., &
            failure_message="attaching more than PF_LOG_MAX_SINKS sinks was expected to abort", &
            required_stderr="PF_LOG_MAX_SINKS")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "logging_sink_and_name_together", expect_abort=.true., &
            failure_message="set_level with both selectors was expected to abort", &
            required_stderr="cannot be combined")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "logging_rank_filter_without_rank", expect_abort=.true., &
            failure_message="a rank-filtered sink with no rank set was expected to abort", &
            required_stderr="%set_rank")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "logging_unknown_level_name", expect_abort=.true., &
            failure_message="an unknown level name with no ok= was expected to abort", &
            required_stderr="is not a level")
    end subroutine test_logging_configuration_aborts

    !> An unbalanced push and pop is caught at the site by the frame token, rather than silently
    !> mislabelling every later record on that thread.
    subroutine test_logging_pop_token_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_pop_context_token_mismatch", expect_abort=.true., &
            failure_message="a pop_context token that did not match the depth was expected to abort", &
            required_stderr="unbalanced")
    end subroutine test_logging_pop_token_mismatch_aborts

    !> Writing through a copy whose unit another copy has closed aborts naming the path, rather
    !> than losing every later record silently.
    subroutine test_logging_write_to_closed_sink_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_write_to_closed_sink", expect_abort=.true., &
            failure_message="writing to a sink whose unit was closed by another copy was expected to abort", &
            required_stderr="es_log_closed.txt")
    end subroutine test_logging_write_to_closed_sink_aborts

    !> pf_log_fatal aborts, and says what it was told to say.
    subroutine test_logging_fatal_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_fatal", expect_abort=.true., &
            failure_message="pf_log_fatal was expected to abort", &
            required_stderr="the run cannot continue")
    end subroutine test_logging_fatal_aborts

    !> Eight threads call pf_log_fatal at once and the process still aborts ONCE.
    !>
    !> `ERROR STOP` is `exit()`, and two threads calling `exit()` at once is undefined behaviour.
    !> Under ifx it leaves the exit status nondeterministic -- measured at 50 runs in 300 exiting
    !> **0** for a run that had plainly aborted, which is a fatal error reporting success to
    !> whatever spawned it. gfortran is deterministic here, so this cannot be caught by a
    !> gfortran-only CI and needs its own test.
    !>
    !> The status assertion states the defect but cannot alone be trusted, since an unfixed build
    !> fails it only intermittently. The record count is the deterministic half: serialised,
    !> the losing threads block before they can emit, so stdout carries exactly ONE fatal record;
    !> unserialised, every thread emits its own copy first. One run separates the two.
    subroutine test_logging_fatal_omp_aborts_once(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        character(len=32) :: num
        integer :: exitstat, cmdstat, nrec
        logical :: on_err

        call run_error_scenario("logging_fatal_omp", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): logging_fatal_omp")
        if (allocated(error)) return
        ! A concurrent abort can wedge rather than exit; see check_scenario_exit_status on why 124
        ! and 137 have to be told apart from a genuine abort before the status is read as one.
        call check(error, exitstat /= 124 .and. exitstat /= 137, &
            "scenario TIMED OUT and was killed (neither finished nor aborted): logging_fatal_omp")
        if (allocated(error)) return

        call check(error, exitstat /= 0, &
            "a concurrent pf_log_fatal was expected to abort, but the process exited 0")
        if (allocated(error)) return

        call file_contains(err_file, "the parallel run cannot continue", on_err)
        call check(error, on_err, "the abort did not carry the message it was given")
        if (allocated(error)) return

        call file_count_containing(out_file, "the parallel run cannot continue", nrec)
        write(num,'(i0)') nrec
        call check(error, nrec == 1, &
            "eight concurrent pf_log_fatal calls must leave exactly one fatal record, not " &
            // trim(num) // " -- the abort is not serialised")
    end subroutine test_logging_fatal_omp_aborts_once

    !> A path over PF_LOG_MAX_PATH is refused rather than truncated -- truncation would open a
    !> different file from the one the caller named, and log to it silently.
    subroutine test_logging_path_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_path_too_long", expect_abort=.true., &
            failure_message="a path longer than PF_LOG_MAX_PATH was expected to abort", &
            required_stderr="PF_LOG_MAX_PATH")
    end subroutine test_logging_path_too_long_aborts

    !> A file that cannot be opened aborts at add_file, naming the path -- rather than attaching a
    !> sink whose every later write fails somewhere else entirely.
    subroutine test_logging_file_cannot_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_file_cannot_open", expect_abort=.true., &
            failure_message="add_file on an unopenable path was expected to abort", &
            required_stderr="cannot open")
    end subroutine test_logging_file_cannot_open_aborts

    !> The default logger writes to STDOUT at INFO before anything configures it.
    !>
    !> This is the only test of that contract, and it has to be out of process: the implicit
    !> console writes to `output_unit`, which no in-process assertion can read back. Asserting the
    !> stream rather than mere presence is what gives it teeth -- the record must be on stdout and
    !> NOT on stderr, since a logger that sent everything to stderr would satisfy a presence check.
    subroutine test_logging_implicit_console(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "logging_implicit_console", "implicit-console-record", &
            "stdout", "the unconfigured default logger was expected to print to stdout")
    end subroutine test_logging_implicit_console

    !> Two halves of the same contract, both of which would pass vacuously on their own: a record
    !> BELOW the implicit INFO threshold never appears, and no record appears at all once
    !> `pf_log_init(console = .false.)` has retired the implicit console.
    subroutine test_logging_implicit_console_retires(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_no_output(error, "logging_implicit_console", &
            expect_abort=.false., &
            failure_message="the implicit console emitted a record it should not have", &
            forbidden_text="after-configuration-record")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "logging_implicit_console", &
            expect_abort=.false., &
            failure_message="the implicit console emitted below its INFO threshold", &
            forbidden_text="implicit-debug-record")
    end subroutine test_logging_implicit_console_retires

    !> unset_level refuses a name set_level would also refuse, rather than answering "no such
    !> override" for a name that could never have had one.
    subroutine test_logging_unset_level_empty_name(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_unset_level_empty_name", &
            expect_abort=.true., &
            failure_message="unset_level with an empty name= was expected to abort", &
            required_stderr="name= is empty")
    end subroutine test_logging_unset_level_empty_name

    !> An unbalanced name pop aborts at the site that can fix it, rather than silently removing
    !> a callee's frame and leaving the caller's to leak.
    subroutine test_logging_pop_name_token_mismatch(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_pop_name_token_mismatch", &
            expect_abort=.true., &
            failure_message="a name pop with a stale frame token was expected to abort", &
            required_stderr="unbalanced")
    end subroutine test_logging_pop_name_token_mismatch

    !> See `scenario_logging_template_too_long` (test/error_scenarios.f90).
    subroutine test_logging_template_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_template_too_long", &
            expect_abort=.true., &
            failure_message="an over-long layout template was expected to abort", &
            required_stderr="longer than PF_LOG_MAX_FORMAT")
    end subroutine test_logging_template_too_long_aborts

    !> See `scenario_logging_template_unclosed_brace` (test/error_scenarios.f90).
    subroutine test_logging_template_unclosed_brace_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_template_unclosed_brace", &
            expect_abort=.true., &
            failure_message="a template with an unclosed brace was expected to abort", &
            required_stderr="unclosed '{'")
    end subroutine test_logging_template_unclosed_brace_aborts

    !> See `scenario_logging_template_too_many_ops` (test/error_scenarios.f90).
    subroutine test_logging_template_too_many_ops_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_template_too_many_ops", &
            expect_abort=.true., &
            failure_message="a template over PF_LOG_MAX_FORMAT_OPS was expected to abort", &
            required_stderr="PF_LOG_MAX_FORMAT_OPS")
    end subroutine test_logging_template_too_many_ops_aborts

    !> See `scenario_logging_add_console_bad_stream` (test/error_scenarios.f90).
    subroutine test_logging_add_console_bad_stream_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_add_console_bad_stream", &
            expect_abort=.true., &
            failure_message="an unknown console stream was expected to abort", &
            required_stderr="must be PF_LOG_STDOUT or PF_LOG_STDERR")
    end subroutine test_logging_add_console_bad_stream_aborts

    !> See `scenario_logging_add_unit_not_connected` (test/error_scenarios.f90).
    subroutine test_logging_add_unit_not_connected_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_add_unit_not_connected", &
            expect_abort=.true., &
            failure_message="an unconnected unit was expected to abort", &
            required_stderr="is not connected")
    end subroutine test_logging_add_unit_not_connected_aborts

    !> See `scenario_logging_add_unit_not_writable` (test/error_scenarios.f90).
    subroutine test_logging_add_unit_not_writable_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_add_unit_not_writable", &
            expect_abort=.true., &
            failure_message="a read-only unit was expected to abort", &
            required_stderr="is not writable")
    end subroutine test_logging_add_unit_not_writable_aborts

    !> See `scenario_logging_add_unit_unformatted` (test/error_scenarios.f90).
    subroutine test_logging_add_unit_unformatted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_add_unit_unformatted", &
            expect_abort=.true., &
            failure_message="an unformatted unit was expected to abort", &
            required_stderr="is not a formatted unit")
    end subroutine test_logging_add_unit_unformatted_aborts

    !> See `scenario_logging_set_level_name_too_long` (test/error_scenarios.f90).
    subroutine test_logging_set_level_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_set_level_name_too_long", &
            expect_abort=.true., &
            failure_message="an over-long override key was expected to abort", &
            required_stderr="name= is longer than PF_LOG_MAX_NAME")
    end subroutine test_logging_set_level_name_too_long_aborts

    !> See `scenario_logging_too_many_name_rules` (test/error_scenarios.f90).
    subroutine test_logging_too_many_name_rules_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_too_many_name_rules", &
            expect_abort=.true., &
            failure_message="more than PF_LOG_MAX_NAME_RULES overrides was expected to abort", &
            required_stderr="PF_LOG_MAX_NAME_RULES")
    end subroutine test_logging_too_many_name_rules_aborts

    !> See `scenario_logging_unset_level_name_too_long` (test/error_scenarios.f90).
    subroutine test_logging_unset_level_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_unset_level_name_too_long", &
            expect_abort=.true., &
            failure_message="an over-long name= in unset_level was expected to abort", &
            required_stderr="name= is longer than PF_LOG_MAX_NAME")
    end subroutine test_logging_unset_level_name_too_long_aborts

    !> See `scenario_logging_set_color_bad_policy` (test/error_scenarios.f90).
    subroutine test_logging_set_color_bad_policy_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_set_color_bad_policy", &
            expect_abort=.true., &
            failure_message="an unknown colour policy was expected to abort", &
            required_stderr="PF_LOG_COLOR_AUTO/_NEVER/_ALWAYS")
    end subroutine test_logging_set_color_bad_policy_aborts

    !> See `scenario_logging_bad_sink_id` (test/error_scenarios.f90).
    subroutine test_logging_bad_sink_id_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_bad_sink_id", &
            expect_abort=.true., &
            failure_message="an unknown sink id was expected to abort", &
            required_stderr="no sink with id")
    end subroutine test_logging_bad_sink_id_aborts

    !> See `scenario_logging_set_name_too_long` (test/error_scenarios.f90).
    subroutine test_logging_set_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_set_name_too_long", &
            expect_abort=.true., &
            failure_message="an over-long logger name was expected to abort", &
            required_stderr="name is longer than PF_LOG_MAX_NAME")
    end subroutine test_logging_set_name_too_long_aborts

    !> See `scenario_logging_set_rank_negative` (test/error_scenarios.f90).
    subroutine test_logging_set_rank_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_set_rank_negative", &
            expect_abort=.true., &
            failure_message="a negative rank was expected to abort", &
            required_stderr="must be non-negative")
    end subroutine test_logging_set_rank_negative_aborts

    !> See `scenario_logging_thread_mode_bad` (test/error_scenarios.f90).
    subroutine test_logging_thread_mode_bad_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_thread_mode_bad", &
            expect_abort=.true., &
            failure_message="an unknown thread mode was expected to abort", &
            required_stderr="PF_LOG_THREAD_DIRECT or PF_LOG_THREAD_BUFFERED")
    end subroutine test_logging_thread_mode_bad_aborts

    !> See `scenario_logging_thread_mode_slot_too_small` (test/error_scenarios.f90).
    subroutine test_logging_thread_mode_slot_too_small_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_thread_mode_slot_too_small", &
            expect_abort=.true., &
            failure_message="slot_bytes below PF_LOG_MIN_BUFFER_BYTES was expected to abort", &
            required_stderr="below PF_LOG_MIN_BUFFER_BYTES")
    end subroutine test_logging_thread_mode_slot_too_small_aborts

    !> See `scenario_logging_set_context_too_long` (test/error_scenarios.f90).
    subroutine test_logging_set_context_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_set_context_too_long", &
            expect_abort=.true., &
            failure_message="an over-long base context was expected to abort", &
            required_stderr="longer than PF_LOG_MAX_CONTEXT")
    end subroutine test_logging_set_context_too_long_aborts

    !> See `scenario_logging_push_name_empty` (test/error_scenarios.f90).
    subroutine test_logging_push_name_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_push_name_empty", &
            expect_abort=.true., &
            failure_message="an empty name frame was expected to abort", &
            required_stderr="text is empty")
    end subroutine test_logging_push_name_empty_aborts

    !> See `scenario_logging_push_name_too_deep` (test/error_scenarios.f90).
    subroutine test_logging_push_name_too_deep_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_push_name_too_deep", &
            expect_abort=.true., &
            failure_message="more than PF_LOG_MAX_NAME_DEPTH frames was expected to abort", &
            required_stderr="PF_LOG_MAX_NAME_DEPTH")
    end subroutine test_logging_push_name_too_deep_aborts

    !> See `scenario_logging_push_name_too_long` (test/error_scenarios.f90).
    subroutine test_logging_push_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_push_name_too_long", &
            expect_abort=.true., &
            failure_message="name frames over PF_LOG_MAX_NAME was expected to abort", &
            required_stderr="longer than PF_LOG_MAX_NAME characters")
    end subroutine test_logging_push_name_too_long_aborts

    !> See `scenario_logging_env_bad_level` (test/error_scenarios.f90).
    subroutine test_logging_env_bad_level_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_env_bad_level", &
            expect_abort=.true., &
            failure_message="an unparseable PF_LOG_LEVEL was expected to abort", &
            required_stderr="is not a level")
    end subroutine test_logging_env_bad_level_aborts

    !> See `scenario_logging_implicit_print` (test/error_scenarios.f90). The scenario asserts the
    !> implicit threshold itself and exits 0; a regression there shows up as a nonzero exit.
    subroutine test_logging_implicit_print(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_implicit_print", &
            expect_abort=.false., &
            failure_message="the unconfigured default logger was expected to describe itself", &
            required_stderr="")
    end subroutine test_logging_implicit_print

    !> push_name takes one segment: a dotted frame could not be popped off as a unit.
    subroutine test_logging_push_name_with_dot(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_push_name_with_dot", &
            expect_abort=.true., &
            failure_message="a name frame containing a dot was expected to be refused", &
            required_stderr="one segment")
    end subroutine test_logging_push_name_with_dot

    !> The composed name aborts rather than truncating, because a shortened name silently changes
    !> which per-name override matches.
    subroutine test_logging_composed_name_too_long(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "logging_composed_name_too_long", &
            expect_abort=.true., &
            failure_message="a composed name over PF_LOG_MAX_NAME was expected to abort", &
            required_stderr="PF_LOG_MAX_NAME")
    end subroutine test_logging_composed_name_too_long

    !> The negative control: the same calls made correctly must not abort.
    subroutine test_logging_control_does_not_abort(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "logging_control", expect_abort=.false., &
            failure_message="the logging control scenario was expected to exit cleanly")
    end subroutine test_logging_control_does_not_abort

    !> join abort path: see scenario_join_pair_count_overflow in test/error_scenarios.f90 for the
    !> control that keeps this assertion honest. The message names both counts.
    subroutine test_join_pair_count_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "join_pair_count_overflow", &
            expect_abort=.true., &
            failure_message="a pair count one past huge(int64) was expected to abort", &
            required_stderr="join: the output would have more rows than a 64-bit count can hold " // &
                "(9223372036854775806 so far, plus 2)")
    end subroutine test_join_pair_count_overflow_aborts

    !> pf_nth_element abort path: see scenario_sorting_nth_out_of_range in
    !> test/error_scenarios.f90 for what it does and why that state is rejected.
    subroutine test_sorting_nth_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_nth_out_of_range", &
            expect_abort=.true., &
            failure_message="asking for a rank the array does not have was expected to abort", &
            required_stderr="nth is 9, which is outside 1..4")
    end subroutine test_sorting_nth_out_of_range_aborts

    !> pf_partial_sort abort path: see scenario_sorting_partial_negative_n in
    !> test/error_scenarios.f90. The scenario clamps n = 99 FIRST, so this also proves the guard
    !> does not simply refuse every n -- without that control it would pass against one that did.
    subroutine test_sorting_partial_negative_n_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_partial_negative_n", &
            expect_abort=.true., &
            failure_message="a negative n was expected to abort", &
            required_stderr="n is -1, which is negative")
    end subroutine test_sorting_partial_negative_n_aborts

    !> parquet_stats abort path: see scenario_stats_is_valid_length_mismatch in
    !> test/error_scenarios.f90. A mask of the wrong length is the one class this module aborts on
    !> -- misuse. Every DATA condition it meets (an empty population, an all-null column, all-zero
    !> weights) returns instead, which is what makes a per-group loop usable.
    subroutine test_stats_is_valid_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_is_valid_length_mismatch", &
            expect_abort=.true., &
            failure_message="a mismatched is_valid was expected to abort", &
            required_stderr="pf_count_valid: is_valid has 2 elements but values has 4")
    end subroutine test_stats_is_valid_length_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_weights_length_mismatch in
    !> test/error_scenarios.f90. Asserted separately from the is_valid case, because a guard
    !> written for one of the two optional arrays and not the other passes every test written
    !> for the one it covers.
    subroutine test_stats_weights_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_weights_length_mismatch", &
            expect_abort=.true., &
            failure_message="a mismatched weights array was expected to abort", &
            required_stderr="pf_count_valid: weights has 3 elements but values has 4")
    end subroutine test_stats_weights_length_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_negative_weight in test/error_scenarios.f90.
    !> The message must NAME THE INDEX: the entire value of failing at the weight rather than
    !> downstream is that it points at the row whose weight computation is broken.
    subroutine test_stats_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_negative_weight", &
            expect_abort=.true., &
            failure_message="a negative weight was expected to abort", &
            required_stderr="pf_count_valid: weight 2 is negative")
    end subroutine test_stats_negative_weight_aborts

    !> parquet_stats abort path: see scenario_stats_nan_weight in test/error_scenarios.f90.
    subroutine test_stats_nan_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_nan_weight", &
            expect_abort=.true., &
            failure_message="a NaN weight was expected to abort", &
            required_stderr="pf_count_valid: weight 3 is NaN")
    end subroutine test_stats_nan_weight_aborts

    !> parquet_stats abort path: see scenario_stats_infinite_weight in test/error_scenarios.f90.
    !> Separate from the NaN case because the guard's three tests are three STATEMENTS -- Fortran
    !> does not short-circuit, and a NaN compared with `<` raises IEEE_INVALID, which nagfor's
    !> default -ieee=stop turns into a dead process. A guard covering only NaN would pass that
    !> scenario while letting an infinity through to make every weighted answer NaN.
    subroutine test_stats_infinite_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_infinite_weight", &
            expect_abort=.true., &
            failure_message="an infinite weight was expected to abort", &
            required_stderr="pf_count_valid: weight 2 is infinite")
    end subroutine test_stats_infinite_weight_aborts

    !> parquet_stats abort path: see scenario_stats_unknown_weight_type in
    !> test/error_scenarios.f90. The message must LIST the accepted tokens rather than only reject
    !> the one supplied: the two conventions differ in what they divide by, so a caller who reached
    !> for a third spelling needs to be told which of the two they meant.
    subroutine test_stats_unknown_weight_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_unknown_weight_type", &
            expect_abort=.true., &
            failure_message="an unrecognised weight_type was expected to abort", &
            required_stderr="pf_variance: weight_type ""inverse-variance"" is not recognised")
    end subroutine test_stats_unknown_weight_type_aborts

    !> pf_stats abort path: see scenario_stats_object_query_before_compute in
    !> test/error_scenarios.f90. The message has to say how to fix it, because the two states this
    !> guard separates -- an empty population and an uncomputed object -- look identical from the
    !> call site and only one of them is an error.
    subroutine test_stats_object_query_before_compute_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_query_before_compute", &
            expect_abort=.true., &
            failure_message="querying an uncomputed pf_stats was expected to abort", &
            required_stderr="pf_stats%mean: this pf_stats holds no population")
    end subroutine test_stats_object_query_before_compute_aborts

    !> pf_stats abort path: see scenario_stats_object_merge_retain_mismatch in
    !> test/error_scenarios.f90. The message says what would go wrong rather than only that the
    !> two disagree: the moments of such a merge would be perfectly correct, and only the retained
    !> values -- which nothing in tier A reads -- would be short.
    subroutine test_stats_object_merge_retain_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_merge_retain_mismatch", &
            expect_abort=.true., &
            failure_message="merging across a retain mismatch was expected to abort", &
            required_stderr="pf_stats%merge: the destination and the source disagree on retain")
    end subroutine test_stats_object_merge_retain_mismatch_aborts

    !> pf_stats abort path: see scenario_stats_object_merge_weight_type_mismatch in
    !> test/error_scenarios.f90.
    subroutine test_stats_object_merge_weight_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, &
            "stats_object_merge_weight_type_mismatch", expect_abort=.true., &
            failure_message="merging across a weight_type mismatch was expected to abort", &
            required_stderr="pf_stats%merge: the destination and the source disagree on weight_type")
    end subroutine test_stats_object_merge_weight_type_mismatch_aborts

    !> pf_stats abort path: see scenario_stats_object_merge_skipnan_mismatch in
    !> test/error_scenarios.f90. The third of the three policy flags, and the one `%merge` used to
    !> accept silently -- so this scenario is what stops the guard being lost again.
    subroutine test_stats_object_merge_skipnan_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_merge_skipnan_mismatch", &
            expect_abort=.true., &
            failure_message="merging across a skipnan mismatch was expected to abort", &
            required_stderr="pf_stats%merge: the destination and the source disagree on skipnan")
    end subroutine test_stats_object_merge_skipnan_mismatch_aborts

    !> pf_stats abort path: see scenario_stats_object_gmean_without_retain in
    !> test/error_scenarios.f90.
    subroutine test_stats_object_gmean_without_retain_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_gmean_without_retain", &
            expect_abort=.true., &
            failure_message="%gmean on a streaming pf_stats was expected to abort", &
            required_stderr="pf_stats%gmean: this accumulator was created with retain=.false.")
    end subroutine test_stats_object_gmean_without_retain_aborts

    !> pf_stats abort path: see scenario_stats_object_hmean_without_retain in
    !> test/error_scenarios.f90.
    subroutine test_stats_object_hmean_without_retain_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_hmean_without_retain", &
            expect_abort=.true., &
            failure_message="%hmean on a streaming pf_stats was expected to abort", &
            required_stderr="pf_stats%hmean: this accumulator was created with retain=.false.")
    end subroutine test_stats_object_hmean_without_retain_aborts

    !> pf_stats abort path: see scenario_stats_object_merge_uncomputed_source in
    !> test/error_scenarios.f90.
    subroutine test_stats_object_merge_uncomputed_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_merge_uncomputed_source", &
            expect_abort=.true., &
            failure_message="merging an uncomputed source was expected to abort", &
            required_stderr="pf_stats%merge: the source holds no population")
    end subroutine test_stats_object_merge_uncomputed_source_aborts

    !> parquet_stats abort path: see scenario_stats_column_string_kind in
    !> test/error_scenarios.f90. The message must name the kind it FOUND -- a caller reaching a
    !> column entry point by mistake usually has the wrong column, not the wrong procedure.
    subroutine test_stats_column_string_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_column_string_kind", &
            expect_abort=.true., &
            failure_message="a string column was expected to abort", &
            required_stderr="pf_mean: a column of kind PK_STRING has no numeric statistics")
    end subroutine test_stats_column_string_kind_aborts

    !> parquet_stats abort path: see scenario_stats_column_vector_width in
    !> test/error_scenarios.f90. **The message must name the WIDTH, not the kind**, and that is
    !> the whole point of the assertion: a vector column's elements are perfectly numeric, so a
    !> "no numeric statistics" message would send the reader looking for the wrong problem.
    subroutine test_stats_column_vector_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_column_vector_width", &
            expect_abort=.true., &
            failure_message="a vector column was expected to abort", &
            required_stderr="pf_mean: this column is 2 elements wide")
    end subroutine test_stats_column_vector_width_aborts

    !> parquet_stats abort path: see scenario_stats_column_is_valid_conflict in
    !> test/error_scenarios.f90.
    subroutine test_stats_column_is_valid_conflict_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_column_is_valid_conflict", &
            expect_abort=.true., &
            failure_message="is_valid= beside a column was expected to abort", &
            required_stderr="pf_mean: is_valid= cannot be given alongside a parquet_column")
    end subroutine test_stats_column_is_valid_conflict_aborts

    !> parquet_stats abort path: see scenario_stats_quantile_bad_probability in
    !> test/error_scenarios.f90.
    subroutine test_stats_quantile_bad_probability_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_quantile_bad_probability", &
            expect_abort=.true., &
            failure_message="a probability outside [0, 1] was expected to abort", &
            required_stderr="every probability must lie in [0, 1]")
    end subroutine test_stats_quantile_bad_probability_aborts

    !> parquet_stats abort path: see scenario_stats_quantile_bad_method in
    !> test/error_scenarios.f90. The message lists every accepted token, because a caller who
    !> reached for "type7" needs to be told what this library calls it.
    subroutine test_stats_quantile_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_quantile_bad_method", &
            expect_abort=.true., &
            failure_message="an unrecognised method token was expected to abort", &
            required_stderr="is not recognised; use ""linear""")
    end subroutine test_stats_quantile_bad_method_aborts

    !> parquet_stats abort path: see scenario_stats_quantiles_size_mismatch in
    !> test/error_scenarios.f90.
    subroutine test_stats_quantiles_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_quantiles_size_mismatch", &
            expect_abort=.true., &
            failure_message="mismatched probs and out were expected to abort", &
            required_stderr="pf_quantiles: out has 3 elements but probs has 2")
    end subroutine test_stats_quantiles_size_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_trim_mean_bad_prop in
    !> test/error_scenarios.f90.
    subroutine test_stats_trim_mean_bad_prop_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_trim_mean_bad_prop", &
            expect_abort=.true., &
            failure_message="prop=0.5 was expected to abort", &
            required_stderr="pf_trim_mean: prop must satisfy 0 <= prop < 0.5")
    end subroutine test_stats_trim_mean_bad_prop_aborts

    !> parquet_stats abort path: see scenario_stats_score_not_finite in test/error_scenarios.f90.
    !>
    !> The asymmetry is deliberate and is worth the scenario: a NaN in the POPULATION is an
    !> ordinary data condition this module excludes silently, while a NaN SCORE can only be the
    !> caller's own broken arithmetic.
    subroutine test_stats_score_not_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_score_not_finite", &
            expect_abort=.true., &
            failure_message="a NaN score was expected to abort", &
            required_stderr="pf_percentile_of_score: score must be finite")
    end subroutine test_stats_score_not_finite_aborts

    !> parquet_stats abort path: see scenario_stats_score_bad_kind in test/error_scenarios.f90.
    subroutine test_stats_score_bad_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_score_bad_kind", &
            expect_abort=.true., &
            failure_message="an unrecognised kind token was expected to abort", &
            required_stderr="is not recognised; use ""rank""")
    end subroutine test_stats_score_bad_kind_aborts

    !> parquet_stats abort path: see scenario_stats_median_on_streaming in
    !> test/error_scenarios.f90. The message names the fix, because "retain" is the one decision a
    !> caller makes early and meets the consequence of much later.
    subroutine test_stats_median_on_streaming_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_median_on_streaming", &
            expect_abort=.true., &
            failure_message="an order statistic on a streaming accumulator was expected to abort", &
            required_stderr="was created with retain=.false.")
    end subroutine test_stats_median_on_streaming_aborts

    !> parquet_stats abort path: see scenario_stats_mad_bad_scale in test/error_scenarios.f90.
    !> The two tokens are named in the message, because "mad_std" -- astropy's name for exactly
    !> this quantity -- is the token a reader of that library would try first.
    subroutine test_stats_mad_bad_scale_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_mad_bad_scale", &
            expect_abort=.true., &
            failure_message="an unrecognised MAD scale token was expected to abort", &
            required_stderr="unrecognised scale")
    end subroutine test_stats_mad_bad_scale_aborts

    !> parquet_stats abort path: see scenario_stats_mad_center_not_finite in
    !> test/error_scenarios.f90. This is the one place the module treats a NaN as MISUSE rather
    !> than as data, and the message says which of the two it is looking at.
    subroutine test_stats_mad_center_not_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_mad_center_not_finite", &
            expect_abort=.true., &
            failure_message="a NaN MAD centre was expected to abort", &
            required_stderr="center must be finite")
    end subroutine test_stats_mad_center_not_finite_aborts

    !> parquet_stats abort path: see scenario_stats_mode_string_column_is_valid in
    !> test/error_scenarios.f90. Same rule the numeric `parquet_column` entry points apply: two
    !> sources of nullness that can disagree is the shape this repository has been bitten by.
    subroutine test_stats_mode_string_column_is_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_mode_string_column_is_valid", &
            expect_abort=.true., &
            failure_message="is_valid= beside a string column was expected to abort", &
            required_stderr="carries its own validity")
    end subroutine test_stats_mode_string_column_is_valid_aborts

    !> parquet_stats abort path: see scenario_stats_corr_bad_method in test/error_scenarios.f90.
    !> Both tokens are named, because "kendall" is the third correlation a reader of scipy would
    !> reach for and this library does not have it.
    subroutine test_stats_corr_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_corr_bad_method", &
            expect_abort=.true., &
            failure_message="an unrecognised correlation method was expected to abort", &
            required_stderr="unrecognised method")
    end subroutine test_stats_corr_bad_method_aborts

    !> parquet_stats abort path: see scenario_stats_normal_scores_bad_method in
    !> test/error_scenarios.f90. "rankit" is the token chosen because it is what a reader of the
    !> literature would most plausibly try -- the scores ARE rankits, but the token is "blom".
    !> The scenario's own negative control is the accepted `method="hazen"` call above it.
    subroutine test_stats_normal_scores_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_normal_scores_bad_method", &
            expect_abort=.true., &
            failure_message="an unrecognised plotting-position method was expected to abort", &
            required_stderr="unrecognised method")
    end subroutine test_stats_normal_scores_bad_method_aborts

    !> parquet_stats abort path: see scenario_stats_probit_fit_bad_method in
    !> test/error_scenarios.f90.
    subroutine test_stats_probit_fit_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_probit_fit_bad_method", &
            expect_abort=.true., &
            failure_message="an unrecognised plotting-position method was expected to abort " // &
                "pf_probit_fit, which reaches the resolver from a different submodule than " // &
                "pf_normal_scores does", &
            required_stderr="pf_probit_fit: unrecognised method")
    end subroutine test_stats_probit_fit_bad_method_aborts

    !> parquet_stats abort path: see scenario_stats_probit_scale_bad_prob in
    !> test/error_scenarios.f90.
    subroutine test_stats_probit_scale_bad_prob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_probit_scale_bad_prob", &
            expect_abort=.true., &
            failure_message="pf_probit_scale(prob=0.5) was expected to abort", &
            required_stderr="prob must satisfy 0 < prob < 0.5")
    end subroutine test_stats_probit_scale_bad_prob_aborts

    !> parquet_stats abort path: see scenario_stats_probit_mean_bad_weight in
    !> test/error_scenarios.f90.
    subroutine test_stats_probit_mean_bad_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_probit_mean_bad_weight", &
            expect_abort=.true., &
            failure_message="a negative weight was expected to abort pf_probit_mean", &
            required_stderr="pf_probit_mean")
    end subroutine test_stats_probit_mean_bad_weight_aborts

    !> pf_stats abort path: see scenario_stats_object_probit_mean_without_retain in
    !> test/error_scenarios.f90.
    subroutine test_stats_object_probit_mean_without_retain_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_object_probit_mean_without_retain", &
            expect_abort=.true., &
            failure_message="%probit_mean on a streaming pf_stats was expected to abort", &
            required_stderr="pf_stats%probit_mean: this accumulator was created with retain=.false.")
    end subroutine test_stats_object_probit_mean_without_retain_aborts

    !> parquet_stats abort path: see scenario_stats_spearman_with_weights in
    !> test/error_scenarios.f90. The scenario's own negative control is the weighted PEARSON call
    !> above it, so a guard that refused every weighted correlation would fail there instead.
    subroutine test_stats_spearman_with_weights_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_spearman_with_weights", &
            expect_abort=.true., &
            failure_message="a weighted Spearman correlation was expected to abort", &
            required_stderr="does not accept weights")
    end subroutine test_stats_spearman_with_weights_aborts

    !> parquet_stats abort path: see scenario_stats_pair_size_mismatch in
    !> test/error_scenarios.f90. A two-sample statistic over vectors of different lengths is not a
    !> number, so this is the one size check in the module that is about meaning rather than memory.
    subroutine test_stats_pair_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_pair_size_mismatch", &
            expect_abort=.true., &
            failure_message="two samples of different size were expected to abort", &
            required_stderr="must be the same size")
    end subroutine test_stats_pair_size_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_clip_bad_cenfunc in test/error_scenarios.f90.
    subroutine test_stats_clip_bad_cenfunc_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_clip_bad_cenfunc", &
            expect_abort=.true., &
            failure_message="an unrecognised sigma-clip cenfunc was expected to abort", &
            required_stderr="unrecognised cenfunc")
    end subroutine test_stats_clip_bad_cenfunc_aborts

    !> parquet_stats abort path: see scenario_stats_clip_bad_sigma in test/error_scenarios.f90.
    !> A negative clip width would keep nothing, which is never what a caller means -- so this is
    !> misuse and aborts, where an empty RESULT would have been an ordinary data condition.
    subroutine test_stats_clip_bad_sigma_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_clip_bad_sigma", &
            expect_abort=.true., &
            failure_message="a negative sigma-clip width was expected to abort", &
            required_stderr="must be non-negative numbers")
    end subroutine test_stats_clip_bad_sigma_aborts

    !> parquet_stats abort path: see scenario_stats_zscore_size_mismatch in
    !> test/error_scenarios.f90.
    subroutine test_stats_zscore_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_zscore_size_mismatch", &
            expect_abort=.true., &
            failure_message="a short pf_zscore output array was expected to abort", &
            required_stderr="elements but values has")
    end subroutine test_stats_zscore_size_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_cumsum_size_mismatch in
    !> test/error_scenarios.f90.
    subroutine test_stats_cumsum_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_cumsum_size_mismatch", &
            expect_abort=.true., &
            failure_message="a short pf_cumsum output array was expected to abort", &
            required_stderr="pf_cumsum: out has")
    end subroutine test_stats_cumsum_size_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_cum_out_valid_mismatch in
    !> test/error_scenarios.f90.
    subroutine test_stats_cum_out_valid_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_cum_out_valid_mismatch", &
            expect_abort=.true., &
            failure_message="a short cumulative out_valid mask was expected to abort", &
            required_stderr="pf_cummax: out_valid has")
    end subroutine test_stats_cum_out_valid_mismatch_aborts

    !> parquet_stats abort path: see scenario_stats_edges_too_few in test/error_scenarios.f90.
    subroutine test_stats_edges_too_few_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_edges_too_few", &
            expect_abort=.true., &
            failure_message="a single bin edge was expected to abort", &
            required_stderr="at least two entries to describe one bin")
    end subroutine test_stats_edges_too_few_aborts

    !> parquet_stats abort path: see scenario_stats_edges_not_increasing in
    !> test/error_scenarios.f90.
    subroutine test_stats_edges_not_increasing_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_edges_not_increasing", &
            expect_abort=.true., &
            failure_message="a repeated bin edge was expected to abort", &
            required_stderr="edges must be strictly increasing")
    end subroutine test_stats_edges_not_increasing_aborts

    !> parquet_stats abort path: see scenario_stats_edges_nan in test/error_scenarios.f90.
    !>
    !> The required text is what makes this test worth having separately from the ordering one:
    !> a NaN edge fails the ordering test too, so a check that dropped the NaN test would still
    !> abort -- with a message pointing at the wrong half of the caller's edge array.
    subroutine test_stats_edges_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_edges_nan", &
            expect_abort=.true., &
            failure_message="a NaN bin edge was expected to abort", &
            required_stderr="edges(2) is a NaN")
    end subroutine test_stats_edges_nan_aborts

    !> parquet_stats abort path: see scenario_stats_histogram_counts_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_histogram_counts_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_histogram_counts_size", &
            expect_abort=.true., &
            failure_message="one histogram count per edge was expected to abort", &
            required_stderr="edges describe")
    end subroutine test_stats_histogram_counts_size_aborts

    !> parquet_stats abort path: see scenario_stats_bin_edges_nbins in test/error_scenarios.f90.
    subroutine test_stats_bin_edges_nbins_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_edges_nbins", &
            expect_abort=.true., &
            failure_message="pf_bin_edges with nbins=0 was expected to abort", &
            required_stderr="nbins must be at least 1")
    end subroutine test_stats_bin_edges_nbins_aborts

    !> parquet_stats abort path: see scenario_stats_bin_edges_size in test/error_scenarios.f90.
    subroutine test_stats_bin_edges_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_edges_size", &
            expect_abort=.true., &
            failure_message="one pf_bin_edges boundary per bin was expected to abort", &
            required_stderr="bins need one more boundary than that")
    end subroutine test_stats_bin_edges_size_aborts

    !> parquet_stats abort path: see scenario_stats_bucketize_codes_size in test/error_scenarios.f90.
    subroutine test_stats_bucketize_codes_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bucketize_codes_size", &
            expect_abort=.true., &
            failure_message="one pf_bucketize code per bin was expected to abort", &
            required_stderr="pf_bucketize: codes has")
    end subroutine test_stats_bucketize_codes_size_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_grid_too_short in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_grid_too_short_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_grid_too_short", &
            expect_abort=.true., &
            failure_message="a one-point pf_bin_linear grid was expected to abort", &
            required_stderr="pf_bin_linear: grid must hold at least two points to describe one " // &
                "cell, but holds 1")
    end subroutine test_stats_bin_linear_grid_too_short_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_grid_not_increasing in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_grid_not_increasing_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_grid_not_increasing", &
            expect_abort=.true., &
            failure_message="a repeated pf_bin_linear grid point was expected to abort", &
            required_stderr="pf_bin_linear: grid must be strictly increasing, but grid(2) is not " // &
                "less than grid(3)")
    end subroutine test_stats_bin_linear_grid_not_increasing_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_nan_grid_point in
    !> test/error_scenarios.f90.
    !>
    !> The required text is what makes this separate from the ordering test: a NaN point fails
    !> the ordering test too, so a grid check that lost its NaN screen would still abort -- with
    !> a message pointing at the wrong thing.
    subroutine test_stats_bin_linear_nan_grid_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_nan_grid_point", &
            expect_abort=.true., &
            failure_message="a NaN pf_bin_linear grid point was expected to abort", &
            required_stderr="pf_bin_linear: grid(2) is a NaN")
    end subroutine test_stats_bin_linear_nan_grid_point_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_infinite_grid_point in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_infinite_grid_point_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_infinite_grid_point", &
            expect_abort=.true., &
            failure_message="an infinite pf_bin_linear grid point was expected to abort", &
            required_stderr="pf_bin_linear: grid(3) is infinite, and a value cannot be split in " // &
                "proportion to its distance from an infinite point")
    end subroutine test_stats_bin_linear_infinite_grid_point_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_spacing_overflows in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_spacing_overflows_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_spacing_overflows", &
            expect_abort=.true., &
            failure_message="an overflowing pf_bin_linear grid spacing was expected to abort", &
            required_stderr="pf_bin_linear: grid(1) and grid(2) are further apart than a real64 " // &
                "can hold")
    end subroutine test_stats_bin_linear_spacing_overflows_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_mass_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_mass_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_mass_size", &
            expect_abort=.true., &
            failure_message="one pf_bin_linear mass entry per cell was expected to abort", &
            required_stderr="pf_bin_linear: mass has 3 elements but grid has 4 points; mass holds " // &
                "one entry per grid point, not one per cell")
    end subroutine test_stats_bin_linear_mass_size_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_negative_weight in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_negative_weight", &
            expect_abort=.true., &
            failure_message="a negative pf_bin_linear weight was expected to abort", &
            required_stderr="pf_bin_linear: weight 2 is negative")
    end subroutine test_stats_bin_linear_negative_weight_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_weights_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_weights_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_weights_size", &
            expect_abort=.true., &
            failure_message="a short pf_bin_linear weights array was expected to abort", &
            required_stderr="pf_bin_linear: weights has 2 elements but values has 3")
    end subroutine test_stats_bin_linear_weights_size_aborts

    !> parquet_stats abort path: see scenario_stats_bin_linear_logical_column in
    !> test/error_scenarios.f90.
    subroutine test_stats_bin_linear_logical_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_bin_linear_logical_column", &
            expect_abort=.true., &
            failure_message="pf_bin_linear over a logical column was expected to abort", &
            required_stderr="pf_bin_linear: a logical column has no position on a grid to be " // &
                "split between two points")
    end subroutine test_stats_bin_linear_logical_column_aborts

    !> parquet_stats abort path: see scenario_stats_zscore_out_valid_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_zscore_out_valid_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_zscore_out_valid_size", &
            expect_abort=.true., &
            failure_message="a short pf_zscore out_valid was expected to abort", &
            required_stderr="pf_zscore: out_valid has")
    end subroutine test_stats_zscore_out_valid_size_aborts

    !> parquet_stats abort path: see scenario_stats_normal_scores_size in test/error_scenarios.f90.
    subroutine test_stats_normal_scores_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_normal_scores_size", &
            expect_abort=.true., &
            failure_message="a short pf_normal_scores s was expected to abort", &
            required_stderr="pf_normal_scores: s has")
    end subroutine test_stats_normal_scores_size_aborts

    !> parquet_stats abort path: see scenario_stats_normal_scores_out_valid_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_normal_scores_out_valid_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_normal_scores_out_valid_size", &
            expect_abort=.true., &
            failure_message="a short pf_normal_scores out_valid was expected to abort", &
            required_stderr="pf_normal_scores: out_valid has")
    end subroutine test_stats_normal_scores_out_valid_size_aborts

    !> parquet_stats abort path: see scenario_stats_sigma_clip_keep_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_sigma_clip_keep_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_sigma_clip_keep_size", &
            expect_abort=.true., &
            failure_message="a short sigma-clip keep mask was expected to abort", &
            required_stderr="pf_sigma_clipped_stats: keep has")
    end subroutine test_stats_sigma_clip_keep_size_aborts

    !> parquet_stats abort path: see scenario_stats_obj_order_without_population in
    !> test/error_scenarios.f90.
    subroutine test_stats_obj_order_without_population_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_obj_order_without_population", &
            expect_abort=.true., &
            failure_message="a quantile off an uncomputed accumulator was expected to abort", &
            required_stderr="holds no population yet")
    end subroutine test_stats_obj_order_without_population_aborts

    !> parquet_stats abort path: see scenario_stats_obj_quantiles_out_size in
    !> test/error_scenarios.f90.
    subroutine test_stats_obj_quantiles_out_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_obj_quantiles_out_size", &
            expect_abort=.true., &
            failure_message="%quantiles with a short out was expected to abort", &
            required_stderr="pf_stats%quantiles: out has")
    end subroutine test_stats_obj_quantiles_out_size_aborts

    !> parquet_stats abort path: see scenario_stats_obj_trim_mean_prop in test/error_scenarios.f90.
    subroutine test_stats_obj_trim_mean_prop_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "stats_obj_trim_mean_prop", &
            expect_abort=.true., &
            failure_message="%trim_mean(0.5) was expected to abort", &
            required_stderr="prop must satisfy 0 <= prop < 0.5")
    end subroutine test_stats_obj_trim_mean_prop_aborts

    !> parquet_stats abort path: see scenario_stats_obj_percentile_score_non_finite in
    !> test/error_scenarios.f90.
    subroutine test_stats_obj_percentile_score_non_finite_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, &
            "stats_obj_percentile_score_non_finite", expect_abort=.true., &
            failure_message="%percentile_of_score of a NaN was expected to abort", &
            required_stderr="score must be finite")
    end subroutine test_stats_obj_percentile_score_non_finite_aborts

    !> `%print` with no `unit=` resolves the destination from `parquet_message_stream` and exits
    !> cleanly: see scenario_stats_print_default_stream in test/error_scenarios.f90. Out of
    !> process because that setting names a console stream and no file, so nothing in process can
    !> capture either destination.
    subroutine test_stats_print_default_stream(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "stats_print_default_stream", &
            expect_abort=.false., &
            failure_message="%print with no unit= was expected to write and exit cleanly")
    end subroutine test_stats_print_default_stream

    !> pf_nth_quantile abort path: see scenario_sorting_quantile_out_of_range in
    !> test/error_scenarios.f90. The message names the scale, since 50 is exactly what a caller
    !> thinking in percentiles would write and 0.5 is valid on both scales meaning different things.
    subroutine test_sorting_quantile_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_quantile_out_of_range", &
            expect_abort=.true., &
            failure_message="a quantile outside 0-1 was expected to abort", &
            required_stderr="quantile must lie on a 0-1 scale")
    end subroutine test_sorting_quantile_out_of_range_aborts

    !> pf_nth_quantile abort path: see scenario_sorting_quantile_bad_rounding in
    !> test/error_scenarios.f90. Asserts the message NAMES the valid tokens -- a string selector is
    !> only acceptable because an unknown value fails loudly instead of defaulting.
    subroutine test_sorting_quantile_bad_rounding_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_quantile_bad_rounding", &
            expect_abort=.true., &
            failure_message="an unrecognized rounding token was expected to abort", &
            required_stderr="'nearest' (the default), 'down' or 'up'")
    end subroutine test_sorting_quantile_bad_rounding_aborts

    !> pf_nth_quantile abort path: see scenario_sorting_quantile_all_null in
    !> test/error_scenarios.f90. The scenario answers a PARTIALLY null array first, which is the
    !> control proving n_null still works and only the all-null case is refused.
    subroutine test_sorting_quantile_all_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_quantile_all_null", &
            expect_abort=.true., &
            failure_message="a quantile of an all-null array was expected to abort", &
            required_stderr="every value is null, so no quantile exists")
    end subroutine test_sorting_quantile_all_null_aborts

    subroutine test_sorting_quantile_ok_still_checks_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_quantile_ok_still_checks_range", &
            expect_abort=.true., &
            failure_message="passing ok= was expected to leave the 0-1 range check in force", &
            required_stderr="quantile must lie on a 0-1 scale")
    end subroutine test_sorting_quantile_ok_still_checks_range_aborts

    !> pf_permute abort path: see scenario_sorting_permute_index_out_of_range in
    !> test/error_scenarios.f90 for what it does and why that state is rejected.
    subroutine test_sorting_permute_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_permute_index_out_of_range", &
            expect_abort=.true., &
            failure_message="permuting by an out-of-range index was expected to abort", &
            required_stderr="which is outside the valid index range")
    end subroutine test_sorting_permute_index_out_of_range_aborts

    !> pf_permute abort path: see scenario_sorting_permute_duplicate_index in
    !> test/error_scenarios.f90 for what it does and why that state is rejected. This is the one
    !> that matters most -- a duplicated index is in range, so without the check the array would
    !> come back silently wrong rather than failing.
    subroutine test_sorting_permute_duplicate_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_permute_duplicate_index", &
            expect_abort=.true., &
            failure_message="permuting by a duplicated index was expected to abort", &
            required_stderr="perm is not a permutation")
    end subroutine test_sorting_permute_duplicate_index_aborts

    !> pf_permute abort path: see scenario_sorting_permute_length_mismatch in
    !> test/error_scenarios.f90 for what it does and why that state is rejected.
    subroutine test_sorting_permute_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_permute_length_mismatch", &
            expect_abort=.true., &
            failure_message="permuting by a wrong-length permutation was expected to abort", &
            required_stderr="perm has 3 elements but the values have 4")
    end subroutine test_sorting_permute_length_mismatch_aborts

    !> pf_argsort abort path: see scenario_sorting_valid_length_mismatch in
    !> test/error_scenarios.f90 for what it does and why that state is rejected.
    subroutine test_sorting_valid_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_valid_length_mismatch", &
            expect_abort=.true., &
            failure_message="sorting with a wrong-length is_valid mask was expected to abort", &
            required_stderr="is_valid has 3 elements but the values have 4")
    end subroutine test_sorting_valid_length_mismatch_aborts

    !> pf_sort_keys%add abort path: see scenario_sorting_keys_row_count_mismatch in
    !> test/error_scenarios.f90 for what it does and why that state is rejected.
    subroutine test_sorting_keys_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_keys_row_count_mismatch", &
            expect_abort=.true., &
            failure_message="adding a key of a different length was expected to abort", &
            required_stderr="every key must describe the same number of rows")
    end subroutine test_sorting_keys_row_count_mismatch_aborts

    !> pf_argsort abort path: see scenario_sorting_keys_empty in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sorting_keys_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_keys_empty", expect_abort=.true., &
            failure_message="sorting by an empty key list was expected to abort", &
            required_stderr="this pf_sort_keys has no key")
    end subroutine test_sorting_keys_empty_aborts

    !> pf_partial_argsort abort path: see scenario_sorting_partial_keys_empty in
    !> test/error_scenarios.f90 for why this needs its own scenario rather than being covered by
    !> test_sorting_keys_empty_aborts above.
    subroutine test_sorting_partial_keys_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_partial_keys_empty", expect_abort=.true., &
            failure_message="a partial argsort of an empty key list was expected to abort", &
            required_stderr="this pf_sort_keys has no key")
    end subroutine test_sorting_partial_keys_empty_aborts

    !> The int64 index form of the scenario above -- a third copy of the same guard.
    subroutine test_sorting_partial_keys_empty_i64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_partial_keys_empty_i64", expect_abort=.true., &
            failure_message="a partial argsort of an empty key list was expected to abort", &
            required_stderr="this pf_sort_keys has no key")
    end subroutine test_sorting_partial_keys_empty_i64_aborts

    !> join abort path: see scenario_join_self in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_self_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_self", expect_abort=.true., &
            failure_message="joining a table to itself was expected to abort", &
            required_stderr="a table cannot be joined to itself")
    end subroutine test_join_self_aborts

    !> join abort path: see scenario_join_kind_mismatch in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_kind_mismatch", expect_abort=.true., &
            failure_message="joining an int32 key to an int64 key was expected to abort", &
            required_stderr="a join compares like with like")
    end subroutine test_join_kind_mismatch_aborts

    !> join abort path: see scenario_join_require_m1 in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_require_m1_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_m1", expect_abort=.true., &
            failure_message="a duplicate right key under require='m:1' was expected to abort", &
            required_stderr="asserts the right key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_m1", "1")
    end subroutine test_join_require_m1_aborts

    !> join abort path: see scenario_join_max_rows in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_max_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows", expect_abort=.true., &
            failure_message="a join over its max_rows ceiling was expected to abort", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows", "1")
    end subroutine test_join_max_rows_aborts

    !> join abort path: see scenario_join_max_rows_form in test/error_scenarios.f90 for what the
    !> four of these do, and for why each of `%join`'s ceiling-carrying specifics needs its own.
    subroutine test_join_max_rows_arr_i32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_arr_i32", &
            expect_abort=.true., &
            failure_message="an int32 max_rows= on the array key form was expected to abort", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_arr_i32", "1")
    end subroutine test_join_max_rows_arr_i32_aborts

    !> join abort path: see scenario_join_max_rows_form in test/error_scenarios.f90.
    subroutine test_join_max_rows_arr_i64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_arr_i64", &
            expect_abort=.true., &
            failure_message="an int64 max_rows= on the array key form was expected to abort", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_arr_i64", "1")
    end subroutine test_join_max_rows_arr_i64_aborts

    !> join abort path: see scenario_join_max_rows_form in test/error_scenarios.f90.
    subroutine test_join_max_rows_str_i32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_str_i32", &
            expect_abort=.true., &
            failure_message="an int32 max_rows= on the string key form was expected to abort", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_str_i32", "1")
    end subroutine test_join_max_rows_str_i32_aborts

    !> join abort path: see scenario_join_max_rows_form in test/error_scenarios.f90.
    subroutine test_join_max_rows_str_i64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_str_i64", &
            expect_abort=.true., &
            failure_message="an int64 max_rows= on the string key form was expected to abort", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_str_i64", "1")
    end subroutine test_join_max_rows_str_i64_aborts

    !> join abort path: see scenario_join_require_1m in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_require_1m_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_1m", expect_abort=.true., &
            failure_message="a duplicate left key under require='1:m' was expected to abort", &
            required_stderr="asserts the left key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_1m", "1")
    end subroutine test_join_require_1m_aborts

    !> Asserts a join scenario's stdout shows the engine hook SET to `engine` (1 the sort engine,
    !> 2 the hash engine) before the base scenario ran -- so the abort it then raises is that
    !> engine's. Without this a twin whose hook did nothing would pass on the other engine's
    !> abort. The "hook clear" line the scenario prints beside it is the negative control and is
    !> not asserted: it reports the automatic choice, which the rule owns, not this test.
    subroutine check_engine_was_forced(error, scenario, engine)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario !! the scenario's name.
        character(len=*), intent(in) :: engine   !! "1" or "2", as the hook reports it.

        call check_scenario_streams(error, scenario, "join engine used with the hook set=" // engine, &
            "stdout", "the scenario must show engine " // engine // " in force before its abort")
    end subroutine check_engine_was_forced

    !> join abort path, the sort engine's cardinality check on a TEAM: see scenario_join_engine_twin
    !> in test/error_scenarios.f90 for the tail floor it lowers and the team line it prints, which
    !> is asserted here so the abort is known to come from the chunked check and not its serial arm.
    subroutine test_join_require_m1_threaded_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_m1_threaded", expect_abort=.true., &
            failure_message="a duplicate right key under require='m:1' was expected to abort on a team", &
            required_stderr="asserts the right key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_m1_threaded", "1")
        if (allocated(error)) return
        call check_scenario_streams(error, "join_require_m1_threaded", "join group passes team=4", "stdout", &
            "the scenario must show the sort engine's group passes on a team of 4 before its abort")
    end subroutine test_join_require_m1_threaded_aborts

    !> join abort path, the sort engine's cardinality check on a TEAM, left side: the mirror of
    !> the test above, on the other clause of the chunked check.
    subroutine test_join_require_1m_threaded_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_1m_threaded", expect_abort=.true., &
            failure_message="a duplicate left key under require='1:m' was expected to abort on a team", &
            required_stderr="asserts the left key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_1m_threaded", "1")
        if (allocated(error)) return
        call check_scenario_streams(error, "join_require_1m_threaded", "join group passes team=4", "stdout", &
            "the scenario must show the sort engine's group passes on a team of 4 before its abort")
    end subroutine test_join_require_1m_threaded_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90 for
    !> the two engine controls it prints before the base scenario runs.
    subroutine test_join_require_m1_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_m1_hash", expect_abort=.true., &
            failure_message="a duplicate right key under require='m:1' was expected to abort on the hash engine", &
            required_stderr="asserts the right key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_m1_hash", "2")
    end subroutine test_join_require_m1_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_require_1m_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_require_1m_hash", expect_abort=.true., &
            failure_message="a duplicate left key under require='1:m' was expected to abort on the hash engine", &
            required_stderr="asserts the left key is unique")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_require_1m_hash", "2")
    end subroutine test_join_require_1m_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_max_rows_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_hash", expect_abort=.true., &
            failure_message="a join over its max_rows ceiling was expected to abort on the hash engine", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_hash", "2")
    end subroutine test_join_max_rows_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_max_rows_arr_i32_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_arr_i32_hash", &
            expect_abort=.true., &
            failure_message="an int32 max_rows= on the array key form was expected to abort on the hash engine", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_arr_i32_hash", "2")
    end subroutine test_join_max_rows_arr_i32_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_max_rows_arr_i64_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_arr_i64_hash", &
            expect_abort=.true., &
            failure_message="an int64 max_rows= on the array key form was expected to abort on the hash engine", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_arr_i64_hash", "2")
    end subroutine test_join_max_rows_arr_i64_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_max_rows_str_i32_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_str_i32_hash", &
            expect_abort=.true., &
            failure_message="an int32 max_rows= on the string key form was expected to abort on the hash engine", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_str_i32_hash", "2")
    end subroutine test_join_max_rows_str_i32_hash_aborts

    !> join abort path, hash engine: see scenario_join_hash_twin in test/error_scenarios.f90.
    subroutine test_join_max_rows_str_i64_hash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_str_i64_hash", &
            expect_abort=.true., &
            failure_message="an int64 max_rows= on the string key form was expected to abort on the hash engine", &
            required_stderr="over the max_rows=")
        if (allocated(error)) return
        call check_engine_was_forced(error, "join_max_rows_str_i64_hash", "2")
    end subroutine test_join_max_rows_str_i64_hash_aborts

    !> join abort path: see scenario_join_bad_require in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_bad_require_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_bad_require", &
            expect_abort=.true., &
            failure_message="an unrecognized require= token was expected to abort", &
            required_stderr="is not one of 'm:m', '1:1'")
    end subroutine test_join_bad_require_aborts

    !> join abort path: see scenario_join_bad_order in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_bad_order_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_bad_order", expect_abort=.true., &
            failure_message="an unrecognized order= token was expected to abort", &
            required_stderr="is not one of 'left' or 'key'")
    end subroutine test_join_bad_order_aborts

    !> join abort path: see scenario_join_bad_how in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_bad_how_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_bad_how", expect_abort=.true., &
            failure_message="an unrecognized how= token was expected to abort", &
            required_stderr="is not one of 'inner', 'left'")
    end subroutine test_join_bad_how_aborts

    !> join abort path: see scenario_join_other_on_size in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_other_on_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_other_on_size", expect_abort=.true., &
            failure_message="a short other_on= was expected to abort", &
            required_stderr="it takes one right-hand key name per left-hand one")
    end subroutine test_join_other_on_size_aborts

    !> join abort path: see scenario_join_no_key in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_no_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_no_key", expect_abort=.true., &
            failure_message="a keyless join was expected to abort", &
            required_stderr="no join key was given")
    end subroutine test_join_no_key_aborts

    !> join abort path: see scenario_join_columns_with_semi in test/error_scenarios.f90 for what
    !> it does, and for the two negative controls that keep this assertion honest.
    subroutine test_join_columns_with_semi_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_columns_with_semi", &
            expect_abort=.true., &
            failure_message="columns= with how='anti' was expected to abort", &
            required_stderr="columns= cannot be used with how='semi' or how='anti'")
    end subroutine test_join_columns_with_semi_aborts

    !> join abort path: see scenario_join_left_container_outer in test/error_scenarios.f90 for
    !> what it does, and for the two negative controls that keep this assertion honest.
    subroutine test_join_left_container_outer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_left_container_outer", &
            expect_abort=.true., &
            failure_message="a container column here under how='outer' was expected to abort", &
            required_stderr="has to be fillable with nulls")
    end subroutine test_join_left_container_outer_aborts

    !> join abort path: see scenario_join_key_direction in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_key_direction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_key_direction", expect_abort=.true., &
            failure_message="a sort key given as a join key was expected to abort", &
            required_stderr="a join key has no direction")
    end subroutine test_join_key_direction_aborts

    !> join abort path: see scenario_join_key_direction_dash in test/error_scenarios.f90 for what
    !> it does, why it is a separate scenario, and for its negative control.
    subroutine test_join_key_direction_dash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_key_direction_dash", expect_abort=.true., &
            failure_message="the -name shorthand as a join key was expected to abort", &
            required_stderr="a join key has no direction")
    end subroutine test_join_key_direction_dash_aborts

    !> join abort path: see scenario_join_container_payload in test/error_scenarios.f90 for what
    !> it does, and for the negative control that keeps this assertion honest.
    subroutine test_join_container_payload_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_container_payload", expect_abort=.true., &
            failure_message="carrying a list column across a join was expected to abort", &
            required_stderr="cannot be carried across a join")
    end subroutine test_join_container_payload_aborts

    !> join abort path: see scenario_join_container_key in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    !>
    !> The assertion is on the word "join key". The refusal itself is shared with `%sort_by`
    !> (`sort_lookup_key`, src/parquet_tables_sort.f90) and `container_sort_key` covers that
    !> spelling; what is only reachable through `%join` is that the message names the operation
    !> the caller actually asked for, and that it does not repeat the sort's advice to let the
    !> container be "carried along", which a join cannot do.
    subroutine test_join_container_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_container_key", expect_abort=.true., &
            failure_message="joining on a container column was expected to abort", &
            required_stderr="cannot be a join key")
    end subroutine test_join_container_key_aborts

    !> join abort path: see scenario_join_columns_unknown in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_columns_unknown_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_columns_unknown", expect_abort=.true., &
            failure_message="an unknown columns= name was expected to abort", &
            required_stderr="has no column of")
    end subroutine test_join_columns_unknown_aborts

    !> join abort path: see scenario_join_suffix_clash in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_suffix_clash_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_suffix_clash", expect_abort=.true., &
            failure_message="a doubly-clashing incoming name was expected to abort", &
            required_stderr="clashes with a column already here")
    end subroutine test_join_suffix_clash_aborts

    !> join abort path: see scenario_join_blank_suffix in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_blank_suffix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_blank_suffix", expect_abort=.true., &
            failure_message="a blank other_suffix= was expected to abort", &
            required_stderr="other_suffix= is blank")
    end subroutine test_join_blank_suffix_aborts

    !> join abort path: see scenario_join_detached_column in test/error_scenarios.f90 for what it
    !> does, and for the negative control that keeps this assertion honest.
    subroutine test_join_detached_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_detached_column", expect_abort=.true., &
            failure_message="reading a column the join skipped was expected to abort", &
            required_stderr="has been detached")
    end subroutine test_join_detached_column_aborts

    !> pf_match abort path: see scenario_sorting_match_kind_mismatch in test/error_scenarios.f90
    !> for the promotion this refusal exists to prevent, and for its negative control.
    subroutine test_sorting_match_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_match_kind_mismatch", &
            expect_abort=.true., &
            failure_message="matching two columns of different kinds was expected to abort", &
            required_stderr="the two columns hold different kinds")
    end subroutine test_sorting_match_kind_mismatch_aborts

    !> pf_argsort abort path: see scenario_sorting_column_vector in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sorting_column_vector_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_column_vector", expect_abort=.true., &
            failure_message="sorting a vector column was expected to abort", &
            required_stderr="a vector column cannot be a sort key")
    end subroutine test_sorting_column_vector_aborts

    !> pf_lower_bound abort path: see scenario_sorting_search_unsorted in test/error_scenarios.f90.
    !> The scenario searches SORTED input first, which is the control proving the check is not
    !> firing unconditionally -- a guard that always fired would pass this test while making every
    !> search unusable.
    subroutine test_sorting_search_unsorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_search_unsorted", &
            expect_abort=.true., &
            failure_message="searching unsorted input was expected to abort", &
            required_stderr="values is not sorted")
    end subroutine test_sorting_search_unsorted_aborts

    !> pf_lower_bound abort path: see scenario_sorting_search_target_too_long in
    !> test/error_scenarios.f90. A shorter target answers first, as the control.
    subroutine test_sorting_search_target_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_search_target_too_long", &
            expect_abort=.true., &
            failure_message="an over-long search target was expected to abort", &
            required_stderr="so no exact comparison exists")
    end subroutine test_sorting_search_target_too_long_aborts

    !> Bulk search abort path: see scenario_sorting_search_many_answer_length in
    !> test/error_scenarios.f90. A correctly sized array answers first, as the control.
    subroutine test_sorting_search_many_answer_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_search_many_answer_length", &
            expect_abort=.true., &
            failure_message="a bulk search answer array of the wrong length was expected to abort", &
            required_stderr="it takes one per target")
    end subroutine test_sorting_search_many_answer_length_aborts

    !> Bulk search abort path: see scenario_sorting_search_many_target_too_long in
    !> test/error_scenarios.f90. Shorter targets answer first, as the control.
    subroutine test_sorting_search_many_target_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_search_many_target_too_long", &
            expect_abort=.true., &
            failure_message="an over-long bulk search target was expected to abort", &
            required_stderr="so no exact comparison exists")
    end subroutine test_sorting_search_many_target_too_long_aborts

    !> pf_rank abort path: see scenario_sorting_rank_bad_method in test/error_scenarios.f90.
    !> Asserts the message NAMES the valid tokens, the same standard `rounding=` is held to.
    subroutine test_sorting_rank_bad_method_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_rank_bad_method", &
            expect_abort=.true., &
            failure_message="an unrecognized rank method was expected to abort", &
            required_stderr="'competition' (the default), 'dense' or 'ordinal'")
    end subroutine test_sorting_rank_bad_method_aborts

    !> pf_minmax abort path: see scenario_sorting_minmax_all_null in test/error_scenarios.f90.
    !> The scenario answers a partially null array first, as the control.
    subroutine test_sorting_minmax_all_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null", &
            expect_abort=.true., &
            failure_message="reducing an all-null array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_aborts

    !> pf_minmax abort path, once per value family: see the scenario_sorting_minmax_all_null_*
    !> group in test/error_scenarios.f90 for why one family's scenario says nothing about the
    !> others, and for the negative control each of them makes first.
    subroutine test_sorting_minmax_all_null_i32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_i32", &
            expect_abort=.true., &
            failure_message="reducing an all-null int32 array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_i32_aborts

    subroutine test_sorting_minmax_all_null_i64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_i64", &
            expect_abort=.true., &
            failure_message="reducing an all-null int64 array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_i64_aborts

    subroutine test_sorting_minmax_all_null_f32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_f32", &
            expect_abort=.true., &
            failure_message="reducing an all-null real32 array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_f32_aborts

    subroutine test_sorting_minmax_all_null_chr_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_chr", &
            expect_abort=.true., &
            failure_message="reducing an all-null character array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_chr_aborts

    subroutine test_sorting_minmax_all_null_date_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_date", &
            expect_abort=.true., &
            failure_message="reducing an all-null parquet_date array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_date_aborts

    subroutine test_sorting_minmax_all_null_time_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_time", &
            expect_abort=.true., &
            failure_message="reducing an all-null parquet_time array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_time_aborts

    subroutine test_sorting_minmax_all_null_ts_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_ts", &
            expect_abort=.true., &
            failure_message="reducing an all-null parquet_timestamp array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_ts_aborts

    subroutine test_sorting_minmax_all_null_strcol_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_minmax_all_null_strcol", &
            expect_abort=.true., &
            failure_message="reducing an all-null parquet_string_column array was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_minmax_all_null_strcol_aborts

    !> The parquet_column form of the same guard, reached through pf_argminmax.
    subroutine test_sorting_argminmax_all_null_col_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_argminmax_all_null_col", &
            expect_abort=.true., &
            failure_message="reducing an all-null column was expected to abort", &
            required_stderr="every value is null or NaN")
    end subroutine test_sorting_argminmax_all_null_col_aborts

    !> pf_argsort abort path, int64 form: a separate generated body from the int32 one that
    !> test_sorting_keys_empty_aborts drives.
    subroutine test_sorting_keys_empty_i64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_keys_empty_i64", expect_abort=.true., &
            failure_message="an int64 sort of an empty key list was expected to abort", &
            required_stderr="this pf_sort_keys has no key")
    end subroutine test_sorting_keys_empty_i64_aborts

    !> pf_is_sorted abort path: answering .true. for a key list with no keys would be vacuous.
    subroutine test_sorting_is_sorted_keys_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_is_sorted_keys_empty", &
            expect_abort=.true., &
            failure_message="asking whether an empty key list is sorted was expected to abort", &
            required_stderr="this pf_sort_keys has no key")
    end subroutine test_sorting_is_sorted_keys_empty_aborts

    !> pf_argsort abort path: see scenario_sorting_column_no_kind in test/error_scenarios.f90 for
    !> why a kindless column reaches the kind switch rather than the vector-column guard above it.
    subroutine test_sorting_column_no_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_column_no_kind", expect_abort=.true., &
            failure_message="sorting by a column with no element kind was expected to abort", &
            required_stderr="column cannot be a sort key")
    end subroutine test_sorting_column_no_kind_aborts

    !> pf_merge abort path: see scenario_sorting_merge_unsorted in test/error_scenarios.f90.
    !> Requires the message to name `b`, since a merge has two inputs and saying only "not sorted"
    !> leaves the caller to guess which one.
    subroutine test_sorting_merge_unsorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sorting_merge_unsorted", &
            expect_abort=.true., &
            failure_message="merging unsorted input was expected to abort", &
            required_stderr="b is not sorted")
    end subroutine test_sorting_merge_unsorted_aborts

    !> Stream exhaustion: see scenario_random_stream_exhausted in test/error_scenarios.f90.
    !> The scenario draws successfully from one word below the ceiling first, so a guard that
    !> fired unconditionally would fail there rather than passing this test vacuously.
    subroutine test_random_stream_exhausted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_stream_exhausted", &
            expect_abort=.true., &
            failure_message="drawing past the end of a stream was expected to abort", &
            required_stderr="the stream is exhausted")
    end subroutine test_random_stream_exhausted_aborts

    !> `%rewind(0)`: see scenario_random_stream_rewind_below_one.
    subroutine test_random_stream_rewind_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_stream_rewind_below_one", &
            expect_abort=.true., &
            failure_message="rewinding below position 1 was expected to abort", &
            required_stderr="position must be at least 1")
    end subroutine test_random_stream_rewind_below_one_aborts

    !> `%gamma(0.0)`: see scenario_random_gamma_shape_not_positive.
    subroutine test_random_gamma_shape_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_gamma_shape_not_positive", &
            expect_abort=.true., &
            failure_message="a gamma draw with shape 0 was expected to abort", &
            required_stderr="shape parameter must be strictly positive")
    end subroutine test_random_gamma_shape_not_positive_aborts

    !> `%gamma(NaN)`: see scenario_random_gamma_shape_nan. Distinct from the scenario above
    !> because a `shape <= 0` guard would pass a NaN through while a `.not. (shape > 0)` one does
    !> not, and only a NaN fixture can tell the two spellings apart.
    subroutine test_random_gamma_shape_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_gamma_shape_nan", &
            expect_abort=.true., &
            failure_message="a gamma draw with a NaN shape was expected to abort", &
            required_stderr="shape parameter must be strictly positive")
    end subroutine test_random_gamma_shape_nan_aborts

    !> `%poisson(-1.0)`: see scenario_random_poisson_lambda_negative.
    subroutine test_random_poisson_lambda_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_poisson_lambda_negative", &
            expect_abort=.true., &
            failure_message="a poisson draw with a negative lambda was expected to abort", &
            required_stderr="lambda must be at least 0")
    end subroutine test_random_poisson_lambda_negative_aborts

    !> `%normal_truncated` abort path: see scenario_random_normal_truncated_sigma_not_positive.
    subroutine test_random_normal_truncated_sigma_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_normal_truncated_sigma_not_positive", &
            expect_abort=.true., &
            failure_message="a truncated normal with sigma 0 was expected to abort", &
            required_stderr="sigma must be strictly positive")
    end subroutine test_random_normal_truncated_sigma_not_positive_aborts

    !> `%normal_truncated(sigma=NaN)`: see scenario_random_normal_truncated_sigma_nan. Distinct
    !> from the scenario above because a NaN passes a `<=` test and only a negated `>` refuses it.
    subroutine test_random_normal_truncated_sigma_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_normal_truncated_sigma_nan", &
            expect_abort=.true., &
            failure_message="a truncated normal with a NaN sigma was expected to abort", &
            required_stderr="sigma must be strictly positive")
    end subroutine test_random_normal_truncated_sigma_nan_aborts

    !> `%normal_truncated` abort path: see scenario_random_normal_truncated_bounds_reversed.
    subroutine test_random_normal_truncated_bounds_reversed_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_normal_truncated_bounds_reversed", &
            expect_abort=.true., &
            failure_message="a truncated normal with reversed bounds was expected to abort rather than swap them", &
            required_stderr="lower bound must be strictly")
    end subroutine test_random_normal_truncated_bounds_reversed_aborts

    !> `%normal_truncated` abort path: see scenario_random_normal_truncated_bounds_nan.
    subroutine test_random_normal_truncated_bounds_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_normal_truncated_bounds_nan", &
            expect_abort=.true., &
            failure_message="a truncated normal with a NaN bound was expected to abort", &
            required_stderr="lower bound must be strictly")
    end subroutine test_random_normal_truncated_bounds_nan_aborts

    !> `%normal_truncated` abort path: see scenario_random_normal_truncated_bounds_collapse. The
    !> same message as the reversed-bounds pair, from an interval that is non-empty in data units:
    !> this is the assertion that the guard reads the STANDARDISED bounds.
    subroutine test_random_normal_truncated_bounds_collapse_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_normal_truncated_bounds_collapse", &
            expect_abort=.true., &
            failure_message="an interval that collapses under its own scale was expected to abort", &
            required_stderr="after centring and scaling")
    end subroutine test_random_normal_truncated_bounds_collapse_aborts

    !> `%poisson(NaN)`: see scenario_random_poisson_lambda_nan, and the NaN note on the gamma pair.
    subroutine test_random_poisson_lambda_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_poisson_lambda_nan", &
            expect_abort=.true., &
            failure_message="a poisson draw with a NaN lambda was expected to abort", &
            required_stderr="lambda must be at least 0")
    end subroutine test_random_poisson_lambda_nan_aborts

    !> `%poisson(1e19)`: see scenario_random_poisson_lambda_too_large.
    subroutine test_random_poisson_lambda_too_large_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_poisson_lambda_too_large", &
            expect_abort=.true., &
            failure_message="a poisson draw with an unrepresentable lambda was expected to abort", &
            required_stderr="lambda is so large that a drawn count could overflow")
    end subroutine test_random_poisson_lambda_too_large_aborts

    !> `%poisson` into an int32 that cannot hold the count: see
    !> scenario_random_poisson_int32_overflow.
    subroutine test_random_poisson_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_poisson_int32_overflow", &
            expect_abort=.true., &
            failure_message="a poisson count too large for an int32 was expected to abort", &
            required_stderr="does not fit in an integer(int32)")
    end subroutine test_random_poisson_int32_overflow_aborts

    !> `m < 1` on a resample: see scenario_random_resample_empty_population. The control draws
    !> FOUR values from a population of one -- `size(idx) > m`, which this procedure allows and its
    !> sibling forbids -- so a guard copied from `subset_check` aborts on the control instead.
    subroutine test_random_resample_empty_population_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_resample_empty_population", &
            expect_abort=.true., &
            failure_message="a resample of an empty population was expected to abort", &
            required_stderr="population size m must be at least 1")
    end subroutine test_random_resample_empty_population_aborts

    !> An `integer(int32)` result array with `m` above `huge(int32)`: see
    !> scenario_random_resample_int32_too_narrow. Without the guard the failure is a silent
    !> narrowing wrap rather than anything a caller could notice.
    subroutine test_random_resample_int32_too_narrow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_resample_int32_too_narrow", &
            expect_abort=.true., &
            failure_message="an int32 resample of a wider population was expected to abort", &
            required_stderr="exceeds huge(int32)")
    end subroutine test_random_resample_int32_too_narrow_aborts

    !> A subset larger than its population: see scenario_random_subset_larger_than_population.
    !> The scenario draws a legal `n == m` subset first, so a guard that fired on every call --
    !> the easiest way to get this wrong -- would fail there rather than passing this vacuously.
    subroutine test_random_subset_larger_than_population_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_subset_larger_than_population", &
            expect_abort=.true., &
            failure_message="a subset larger than its population was expected to abort", &
            required_stderr="exceeds population size")
    end subroutine test_random_subset_larger_than_population_aborts

    !> `m < 1`: see scenario_random_subset_empty_population. Control is the `m == 1` population.
    subroutine test_random_subset_empty_population_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_subset_empty_population", &
            expect_abort=.true., &
            failure_message="a subset of an empty population was expected to abort", &
            required_stderr="population size m must be at least 1")
    end subroutine test_random_subset_empty_population_aborts

    !> An `integer(int32)` result array with `m` above `huge(int32)`: see
    !> scenario_random_subset_int32_too_narrow. Without this guard the failure is a silent
    !> narrowing wrap -- a plausible negative index -- rather than anything a caller could notice.
    subroutine test_random_subset_int32_too_narrow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "random_subset_int32_too_narrow", &
            expect_abort=.true., &
            failure_message="an int32 subset of a wider population was expected to abort", &
            required_stderr="exceeds huge(int32)")
    end subroutine test_random_subset_int32_too_narrow_aborts

    !> A caller's own "<KEY>.datatype" entry beside a typed <KEY>: see
    !> scenario_metadata_datatype_key_collision in test/error_scenarios.f90. The write SUCCEEDS
    !> (exit 0) -- a metadata naming clash is not worth refusing an otherwise valid file over --
    !> so the observable results are the warning and the file itself, and the scenario aborts on
    !> its own if the file does not end up with exactly one companion carrying the caller's value.
    subroutine test_metadata_datatype_key_collision_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "metadata_datatype_key_collision", &
            expect_abort=.false., &
            failure_message="an explicit <KEY>.datatype entry should warn but still write the file", &
            required_stderr="was added explicitly, so the type recorded for 'NSIDE'")
    end subroutine test_metadata_datatype_key_collision_warns

    !> The negative control for the test above: without the colliding key there must be no
    !> warning at all. A guard that fires unconditionally satisfies the collision test on its own,
    !> so this is the half that proves the guard actually discriminates.
    subroutine test_metadata_datatype_no_collision_is_quiet(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "metadata_datatype_no_collision_control", &
            expect_abort=.false., &
            failure_message="writing a typed keyword with no collision should succeed", &
            forbidden_text="was added explicitly, so the type recorded for")
    end subroutine test_metadata_datatype_no_collision_is_quiet

    !
    ! ---- parquet_toml abort paths ----
    !
    ! Every one asserts the LIBRARY's own message text and never the runtime's `ERROR STOP` prefix
    ! or its exit status: both are processor-dependent, and all four compilers in this project's
    ! fleet differ on them (CLAUDE.md, "A test must not assert a compiler's ERROR STOP spelling or
    ! exit status"). `check_scenario_exit_status_and_stderr` compares against `/= 0`.
    !

    !> A value of the wrong type aborts instead of leaving the caller's variable undefined.
    subroutine test_toml_wrong_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_wrong_type", expect_abort=.true., &
            failure_message="a wrong-typed config value was expected to abort", &
            required_stderr="is not a whole number")
    end subroutine test_toml_wrong_type_aborts

    !> A key with no default and no opt-out aborts when the file omits it.
    subroutine test_toml_missing_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_missing_key", expect_abort=.true., &
            failure_message="a required config key was expected to abort", &
            required_stderr="config key not found and no default given")
    end subroutine test_toml_missing_key_aborts

    !> A TOML integer beyond `integer(int32)` is reported rather than silently wrapped.
    subroutine test_toml_int_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_int_overflow", expect_abort=.true., &
            failure_message="an out-of-range config integer was expected to abort", &
            required_stderr="does not fit")
    end subroutine test_toml_int_overflow_aborts

    !> A list of the wrong length aborts whether it is too SHORT or too LONG.
    !!
    !! Both directions, because the tempting relaxation is one-sided: a file list longer than the
    !! target looks harmless until you notice that using a prefix of it pairs every value with the
    !! wrong slot.
    subroutine test_toml_array_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_array_length_short", expect_abort=.true., &
            failure_message="a config list read into a shorter array was expected to abort", &
            required_stderr="config list has the wrong number of entries")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "toml_array_length_long", expect_abort=.true., &
            failure_message="a config list read into a longer array was expected to abort", &
            required_stderr="config list has the wrong number of entries")
    end subroutine test_toml_array_length_aborts

    !> A string longer than the caller's element length aborts rather than being clipped.
    subroutine test_toml_string_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_string_too_long", expect_abort=.true., &
            failure_message="an over-long config string was expected to abort", &
            required_stderr="config list entry is too long")
    end subroutine test_toml_string_too_long_aborts

    !> An array getter's bare call requires its key, exactly as a scalar's does.
    subroutine test_toml_array_required_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_array_required", expect_abort=.true., &
            failure_message="an absent config list on a bare call was expected to abort", &
            required_stderr="config key not found and no default given")
    end subroutine test_toml_array_required_aborts

    !> A required section the file does not have aborts, naming the section.
    subroutine test_toml_missing_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_missing_section", expect_abort=.true., &
            failure_message="a required config section was expected to abort", &
            required_stderr="config section not found")
    end subroutine test_toml_missing_section_aborts

    !> Counting the entries of a plain `[table]` aborts rather than answering.
    subroutine test_toml_section_not_array_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_section_not_array", expect_abort=.true., &
            failure_message="counting a plain config table was expected to abort", &
            required_stderr="config name is not an array of sections")
    end subroutine test_toml_section_not_array_aborts

    !> An entry index outside `1 .. count` aborts, quoting both numbers.
    subroutine test_toml_entry_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_entry_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range config entry index was expected to abort", &
            required_stderr="config section index out of range")
    end subroutine test_toml_entry_out_of_range_aborts

    !> A retired key still set stops the run and quotes the advice verbatim.
    subroutine test_toml_retired_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_retired_key", expect_abort=.true., &
            failure_message="a retired config key was expected to abort", &
            required_stderr="set nproc_openmp instead")
    end subroutine test_toml_retired_key_aborts

    !> A key nobody read aborts under `pf_toml_check`'s default severity.
    subroutine test_toml_unknown_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_unknown_key", expect_abort=.true., &
            failure_message="an unread config key was expected to abort", &
            required_stderr="Unknown key in the configuration file")
    end subroutine test_toml_unknown_key_aborts

    !> A whole section nobody opened aborts under `pf_toml_check_all`.
    !!
    !! This is the failure `pf_toml_check` alone cannot see, because no per-section sweep ever runs
    !! over a section the program does not mention.
    subroutine test_toml_unknown_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_unknown_section", expect_abort=.true., &
            failure_message="an unopened config section was expected to abort", &
            required_stderr="Section never read: [plain]")
    end subroutine test_toml_unknown_section_aborts

    !> An unrecognised log level name aborts, listing the names that work.
    subroutine test_toml_bad_level_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_bad_level", expect_abort=.true., &
            failure_message="an unrecognised log level name was expected to abort", &
            required_stderr="unrecognised log level")
    end subroutine test_toml_bad_level_aborts

    !> Reading from an optional section that was absent aborts rather than returning a default.
    subroutine test_toml_closed_handle_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_closed_handle", expect_abort=.true., &
            failure_message="reading a config section that was not found was expected to abort", &
            required_stderr="pf_toml_is_open")
    end subroutine test_toml_closed_handle_aborts

    !> A rank-1 `default` whose size differs from the array it fills is named, not assigned.
    subroutine test_toml_default_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_default_size", expect_abort=.true., &
            failure_message="a rank-1 default of the wrong size was expected to abort", &
            required_stderr="the default has 2 entries")
    end subroutine test_toml_default_size_aborts

    !> `count` is fatal when the file's list has too few entries.
    subroutine test_toml_strings_count_short_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_strings_count_short", expect_abort=.true., &
            failure_message="a string list shorter than count= was expected to abort", &
            required_stderr="the program needs 4")
    end subroutine test_toml_strings_count_short_aborts

    !> `count` is fatal in the other direction too: a prefix would pair values with wrong slots.
    subroutine test_toml_strings_count_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_strings_count_long", expect_abort=.true., &
            failure_message="a string list longer than count= was expected to abort", &
            required_stderr="the program needs 2")
    end subroutine test_toml_strings_count_long_aborts

    !> `pf_toml_dump` writes a whole document, so a section handle is refused as `pf_toml_save` is.
    subroutine test_toml_dump_not_owner_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_dump_not_owner", expect_abort=.true., &
            failure_message="pf_toml_dump on a section handle was expected to abort", &
            required_stderr="not the document handle")
    end subroutine test_toml_dump_not_owner_aborts

    !> `pf_toml_set` refuses a key that exists, and says which procedure to use instead.
    subroutine test_toml_set_existing_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_set_existing", expect_abort=.true., &
            failure_message="pf_toml_set on an existing key was expected to abort", &
            required_stderr="use pf_toml_update to change one that exists")
    end subroutine test_toml_set_existing_aborts

    !> `pf_toml_update` refuses a key that does not exist, and says which procedure to use instead.
    subroutine test_toml_update_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_update_missing", expect_abort=.true., &
            failure_message="pf_toml_update on an absent key was expected to abort", &
            required_stderr="use pf_toml_set to add one that does not")
    end subroutine test_toml_update_missing_aborts

    !> Closing a section handle would free a document other handles still borrow.
    subroutine test_toml_close_not_owner_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_close_not_owner", expect_abort=.true., &
            failure_message="closing a config section handle was expected to abort", &
            required_stderr="does not own its document")
    end subroutine test_toml_close_not_owner_aborts

    !> Malformed TOML aborts without `status`, with toml-f's own diagnostic first.
    subroutine test_toml_parse_error_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_parse_error", expect_abort=.true., &
            failure_message="malformed TOML was expected to abort", &
            required_stderr="is not valid TOML")
    end subroutine test_toml_parse_error_aborts

    !> A file that cannot be opened aborts without `status`, and is told apart from a parse error.
    subroutine test_toml_open_error_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_open_error", expect_abort=.true., &
            failure_message="an unopenable config file was expected to abort", &
            required_stderr="cannot open configuration file")
    end subroutine test_toml_open_error_aborts

    !> A `pf_toml_strings` index outside `1 .. count` aborts, naming both numbers.
    subroutine test_toml_strings_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_strings_range", expect_abort=.true., &
            failure_message="an out-of-range string-list index was expected to abort", &
            required_stderr="element index out of range")
    end subroutine test_toml_strings_range_aborts

    !> An over-long key name aborts rather than truncating into the accumulator.
    subroutine test_toml_key_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_key_too_long", expect_abort=.true., &
            failure_message="an over-long config key name was expected to abort", &
            required_stderr="key name is too long")
    end subroutine test_toml_key_too_long_aborts

    !> A severity that is none of the three constants aborts rather than picking one.
    subroutine test_toml_bad_severity_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_bad_severity", expect_abort=.true., &
            failure_message="an unknown parquet_toml severity was expected to abort", &
            required_stderr="unknown severity")
    end subroutine test_toml_bad_severity_aborts

    !> See `scenario_toml_value_not_list` (test/error_scenarios.f90).
    subroutine test_toml_value_not_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_value_not_list", &
            expect_abort=.true., &
            failure_message="reading a config scalar as a list was expected to abort", &
            required_stderr="config value is not a list")
    end subroutine test_toml_value_not_list_aborts

    !> See `scenario_toml_name_not_a_section` (test/error_scenarios.f90).
    subroutine test_toml_name_not_a_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_name_not_a_section", &
            expect_abort=.true., &
            failure_message="opening a config value as a section was expected to abort", &
            required_stderr="config name is not a section")
    end subroutine test_toml_name_not_a_section_aborts

    !> See `scenario_toml_path_too_long` (test/error_scenarios.f90).
    subroutine test_toml_path_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_path_too_long", &
            expect_abort=.true., &
            failure_message="an over-long config section path was expected to abort", &
            required_stderr="path is too long")
    end subroutine test_toml_path_too_long_aborts

    !> See `scenario_toml_report_fatal` (test/error_scenarios.f90).
    subroutine test_toml_report_fatal_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_report_fatal", &
            expect_abort=.true., &
            failure_message="a fatal parquet_toml report was expected to abort", &
            required_stderr="nproc must be at least ten")
    end subroutine test_toml_report_fatal_aborts

    !> A list of the wrong ELEMENT type aborts, and each of the four wordings is asserted.
    !!
    !! One scenario per distinct `expected` text rather than one per getter: `fail_value`'s own
    !! doc-comment in `src/parquet_toml.f90` records that trade and what it gives up. The four
    !! `required_stderr` strings below are the whole point -- a caller told a list "is not a list
    !! of numbers" when it is a list of STRINGS that is wanted looks in the wrong place.
    subroutine test_toml_list_element_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_list_not_whole_numbers", &
            expect_abort=.true., &
            failure_message="a list of reals read as whole numbers was expected to abort", &
            required_stderr="is not a list of whole numbers")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "toml_list_not_numbers", &
            expect_abort=.true., &
            failure_message="a list of strings read as numbers was expected to abort", &
            required_stderr="is not a list of numbers")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "toml_list_not_logicals", &
            expect_abort=.true., &
            failure_message="a list of integers read as true/false was expected to abort", &
            required_stderr="is not a list of true/false values")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "toml_list_not_strings", &
            expect_abort=.true., &
            failure_message="a list of integers read as strings was expected to abort", &
            required_stderr="is not a list of strings")
    end subroutine test_toml_list_element_type_aborts

    !> `pf_toml_require` reports EVERY missing key, and the count, before it stops.
    !!
    !! The count is what the assertion is on: a version that stopped at the first missing key
    !! would still abort, still name a key, and still pass a test that only looked for the abort.
    subroutine test_toml_require_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_require_missing", &
            expect_abort=.true., &
            failure_message="two absent required config keys were expected to abort", &
            required_stderr="2 required key(s) missing")
    end subroutine test_toml_require_missing_aborts

    !> A handle nothing has opened is refused with the advice to open one.
    subroutine test_toml_handle_never_opened_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_handle_never_opened", &
            expect_abort=.true., &
            failure_message="reading from an unopened config handle was expected to abort", &
            required_stderr="this handle has not been opened")
    end subroutine test_toml_handle_never_opened_aborts

    !> `pf_toml_save` names ITSELF when refused a section handle, not `pf_toml_dump`.
    subroutine test_toml_save_not_owner_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_save_not_owner", &
            expect_abort=.true., &
            failure_message="pf_toml_save from a section handle was expected to abort", &
            required_stderr="pf_toml_save: not the document handle")
    end subroutine test_toml_save_not_owner_aborts

    !> A configuration file that cannot be written is fatal and names the path.
    subroutine test_toml_save_write_error_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_save_write_error", &
            expect_abort=.true., &
            failure_message="saving a config file to an unwritable path was expected to abort", &
            required_stderr="cannot write configuration file")
    end subroutine test_toml_save_write_error_aborts

    !> An over-long `default =` element aborts, exactly as an over-long file value does.
    subroutine test_toml_default_string_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_default_string_too_long", &
            expect_abort=.true., &
            failure_message="an over-long default config string was expected to abort", &
            required_stderr="config list entry is too long")
    end subroutine test_toml_default_string_too_long_aborts

    !> The `[[name]]` wording, which is the half `toml_missing_section` does not reach.
    subroutine test_toml_no_entries_at_all_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_no_entries_at_all", &
            expect_abort=.true., &
            failure_message="a required [[name]] with no entries was expected to abort", &
            required_stderr="has no [[")
    end subroutine test_toml_no_entries_at_all_aborts

    !> THE NEGATIVE CONTROL for all of the above: the same setup, every legal path, exit 0.
    !!
    !! Without it, each scenario above could be aborting in its shared preamble rather than at the
    !! call it names, and every one would still report "aborted as expected".
    subroutine test_toml_control_completes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "toml_control", expect_abort=.false., &
            failure_message="the parquet_toml control scenario was expected to exit cleanly", &
            required_stderr="every legal parquet_toml path completed")
    end subroutine test_toml_control_completes
    !
    !> See `scenario_map_write_millisecond_precision` (test/error_scenarios.f90).
    subroutine test_map_write_millisecond_precision_aborts(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "map_write_millisecond_precision", expect_abort=.true., &
            failure_message="a sub-millisecond value in a map[timestamp[ms]] column was expected to abort", &
            required_stderr="timestamp value has finer precision than the column's declared unit")
    end subroutine test_map_write_millisecond_precision_aborts
    !
    !> See `scenario_qc_all_nan_range` (test/error_scenarios.f90). A WARNING, so the exit status is
    !> 0 and the message is the assertion: `[NaN, NaN]` rather than the `[0, 0]` the accumulators
    !> still hold when every valid element was set aside as a NaN.
    subroutine test_qc_all_nan_range_reports_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        call check_scenario_exit_status_and_stderr(error, "qc_all_nan_range", expect_abort=.false., &
            failure_message="an all-NaN qc column was expected to warn and complete", &
            required_stderr="data range [NaN, NaN], 3 of 3 valid element(s) out of range")
    end subroutine test_qc_all_nan_range_reports_nan
    !
    ! ---- parquet_random: points on a sphere ----
    !
    !> See scenario_random_disc_radius_negative.
    subroutine test_random_disc_radius_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_radius_negative", expect_abort=.true., &
            failure_message="a disc of negative radius was expected to abort", &
            required_stderr="pf_random_disc_at: radius must be finite and at least 0 (got " // &
            "-1.0000000E-01)")
    end subroutine test_random_disc_radius_negative_aborts
    !
    !> See scenario_random_disc_radius_nan.
    subroutine test_random_disc_radius_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_radius_nan", expect_abort=.true., &
            failure_message="a disc of NaN radius was expected to abort", &
            required_stderr="pf_random_disc_at: radius must be finite and at least 0 (got NaN)")
    end subroutine test_random_disc_radius_nan_aborts
    !
    !> See scenario_random_disc_centre_zero.
    subroutine test_random_disc_centre_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_centre_zero", expect_abort=.true., &
            failure_message="a disc about the zero vector was expected to abort", &
            required_stderr="pf_random_disc_at: the centre must be a nonzero, finite direction")
    end subroutine test_random_disc_centre_zero_aborts
    !
    !> See scenario_random_disc_centre_nan.
    subroutine test_random_disc_centre_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_centre_nan", expect_abort=.true., &
            failure_message="a disc about a NaN centre was expected to abort", &
            required_stderr="pf_random_disc_at: the centre must be a nonzero, finite direction")
    end subroutine test_random_disc_centre_nan_aborts
    !
    !> See scenario_random_disc_inner_exceeds_radius.
    subroutine test_random_disc_inner_exceeds_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_inner_exceeds_radius", expect_abort=.true., &
            failure_message="a ring whose inner radius exceeds its outer was expected to abort", &
            required_stderr="pf_random_disc_at: r_inner must lie in [0, radius] (got 6.0000000E-01 " // &
            "against 5.0000000E-01)")
    end subroutine test_random_disc_inner_exceeds_radius_aborts
    !
    !> See scenario_random_disc_inner_above_half_turn.
    subroutine test_random_disc_inner_above_half_turn_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_inner_above_half_turn", expect_abort=.true., &
            failure_message="a ring whose inner radius passes the half turn was expected to abort", &
            required_stderr="pf_random_disc_at: r_inner must not exceed 3.1415927E+00 (got 4.0000000E+00); " // &
            "a ring whose inner radius passes the half turn is the antipode alone")
    end subroutine test_random_disc_inner_above_half_turn_aborts
    !
    !> See scenario_random_disc_inner_nan.
    subroutine test_random_disc_inner_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_inner_nan", expect_abort=.true., &
            failure_message="a ring with a NaN inner radius was expected to abort", &
            required_stderr="pf_random_disc_at: r_inner must lie in [0, radius] (got NaN against " // &
            "5.0000000E-01)")
    end subroutine test_random_disc_inner_nan_aborts
    !
    !> See scenario_random_disc_radec_dec_out_of_range.
    subroutine test_random_disc_radec_dec_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_radec_dec_out_of_range", expect_abort=.true., &
            failure_message="a disc about declination 90.5 was expected to abort", &
            required_stderr="pf_random_disc_radec_at: the centre (ra0, dec0) must be finite with " // &
            "dec0 in [-90, 90] (got 1.0000000E+01, 9.0500000E+01)")
    end subroutine test_random_disc_radec_dec_out_of_range_aborts
    !
    !> See scenario_random_disc_radec_centre_nan.
    subroutine test_random_disc_radec_centre_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_radec_centre_nan", expect_abort=.true., &
            failure_message="a disc about a NaN right ascension was expected to abort", &
            required_stderr="pf_random_disc_radec_at: the centre (ra0, dec0) must be finite with " // &
            "dec0 in [-90, 90] (got NaN, 1.0000000E+01)")
    end subroutine test_random_disc_radec_centre_nan_aborts
    !
    !> See scenario_random_disc_radec_radius_negative.
    subroutine test_random_disc_radec_radius_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_radec_radius_negative", expect_abort=.true., &
            failure_message="a disc of radius -1 degree was expected to abort", &
            required_stderr="pf_random_disc_radec_at: radius_deg must be finite and at least 0 (got " // &
            "-1.0000000E+00)")
    end subroutine test_random_disc_radec_radius_negative_aborts
    !
    !> See scenario_random_ball_radius_negative.
    subroutine test_random_ball_radius_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_ball_radius_negative", expect_abort=.true., &
            failure_message="a ball of negative radius was expected to abort", &
            required_stderr="pf_random_ball_at: radius must be finite and at least 0 (got " // &
            "-1.0000000E+00)")
    end subroutine test_random_ball_radius_negative_aborts
    !
    !> See scenario_random_ball_inner_exceeds_radius.
    subroutine test_random_ball_inner_exceeds_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_ball_inner_exceeds_radius", expect_abort=.true., &
            failure_message="an inverted shell was expected to abort", &
            required_stderr="pf_random_ball_at: r_inner must lie in [0, radius] (got 2.5000000E+00 " // &
            "against 2.0000000E+00)")
    end subroutine test_random_ball_inner_exceeds_radius_aborts
    !
    !> See scenario_random_vmf_kappa_negative.
    subroutine test_random_vmf_kappa_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_vmf_kappa_negative", expect_abort=.true., &
            failure_message="a vMF with kappa -1e300 was expected to abort", &
            required_stderr="pf_random_vmf_at: kappa must be finite and at least 0 (got " // &
            "-1.0000000E+300)")
    end subroutine test_random_vmf_kappa_negative_aborts
    !
    !> See scenario_random_vmf_kappa_nan.
    subroutine test_random_vmf_kappa_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_vmf_kappa_nan", expect_abort=.true., &
            failure_message="a vMF with a NaN kappa was expected to abort", &
            required_stderr="pf_random_vmf_at: kappa must be finite and at least 0 (got NaN)")
    end subroutine test_random_vmf_kappa_nan_aborts
    !
    !> See scenario_random_vmf_mu_zero.
    subroutine test_random_vmf_mu_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_vmf_mu_zero", expect_abort=.true., &
            failure_message="a vMF about the zero vector was expected to abort", &
            required_stderr="pf_random_vmf_at: the centre must be a nonzero, finite direction")
    end subroutine test_random_vmf_mu_zero_aborts
    !
    !> See scenario_random_vmf_radec_sigma_not_positive.
    subroutine test_random_vmf_radec_sigma_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_vmf_radec_sigma_not_positive", expect_abort=.true., &
            failure_message="a vMF of width 0 was expected to abort", &
            required_stderr="pf_random_vmf_radec_at: sigma_deg must be finite and strictly positive " // &
            "(got 0.0000000E+00)")
    end subroutine test_random_vmf_radec_sigma_not_positive_aborts
    !
    !> See scenario_random_vmf_radec_sigma_nan.
    subroutine test_random_vmf_radec_sigma_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_vmf_radec_sigma_nan", expect_abort=.true., &
            failure_message="a vMF of NaN width was expected to abort", &
            required_stderr="pf_random_vmf_radec_at: sigma_deg must be finite and strictly positive " // &
            "(got NaN)")
    end subroutine test_random_vmf_radec_sigma_nan_aborts
    !
    !> See scenario_random_fill_direction_bad_shape.
    subroutine test_random_fill_direction_bad_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_fill_direction_bad_shape", expect_abort=.true., &
            failure_message="a direction fill into two rows was expected to abort", &
            required_stderr="pf_random_fill_direction: v must be shaped (3, n) (got 2 rows)")
    end subroutine test_random_fill_direction_bad_shape_aborts
    !
    !> See scenario_random_fill_radec_size_mismatch.
    subroutine test_random_fill_radec_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_fill_radec_size_mismatch", expect_abort=.true., &
            failure_message="an RA/Dec fill into arrays of sizes 4 and 3 was expected to abort", &
            required_stderr="pf_random_fill_radec: ra and dec must have the same size (got 4 and 3)")
    end subroutine test_random_fill_radec_size_mismatch_aborts
    !
    !> See scenario_random_sphere_draw_beyond_2p62.
    subroutine test_random_sphere_draw_beyond_2p62_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_sphere_draw_beyond_2p62", expect_abort=.true., &
            failure_message="direction 2**62 + 1 was expected to abort", &
            required_stderr="pf_random_direction_at: draw must be at most 2**62 (got " // &
            "4611686018427387905); the sphere family addresses one block per draw")
    end subroutine test_random_sphere_draw_beyond_2p62_aborts
    !
    !> See scenario_random_fill_direction_draw_beyond_2p62.
    subroutine test_random_fill_direction_draw_beyond_2p62_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_fill_direction_draw_beyond_2p62", expect_abort=.true., &
            failure_message="a direction fill ending past 2**62 was expected to abort", &
            required_stderr="pf_random_fill_direction: draw must be at most 2**62 (got " // &
            "4611686018427387902 + 4 - 1)")
    end subroutine test_random_fill_direction_draw_beyond_2p62_aborts
    !
    !> See scenario_random_stream_disc_inner_exceeds_radius.
    subroutine test_random_stream_disc_inner_exceeds_radius_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_stream_disc_inner_exceeds_radius", expect_abort=.true., &
            failure_message="an inverted ring from a stream was expected to abort", &
            required_stderr="pf_random_stream%disc: r_inner must lie in [0, radius] (got " // &
            "6.0000000E-01 against 5.0000000E-01)")
    end subroutine test_random_stream_disc_inner_exceeds_radius_aborts
    !
    !> See scenario_random_disc_cap_unprepared.
    subroutine test_random_disc_cap_unprepared_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "random_disc_cap_unprepared", expect_abort=.true., &
            failure_message="drawing from a cap %prepare has not run was expected to abort", &
            required_stderr="pf_random_disc_cap%at: %prepare has not run")
        if (allocated(error)) return
        ! THE CONTROL, and it is not decoration: a build whose %at aborted UNCONDITIONALLY would
        ! pass the assertion above, because the scenario's FIRST call would raise the same message
        ! with the same exit status. This needle is printed only once a prepared cap has drawn
        ! through both specifics of the %at generic and they agreed, so it pins the control and the
        ! int32 stream index together.
        call check_scenario_streams(error, "random_disc_cap_unprepared", "int32 index agrees: T", &
            expect_on="stdout", &
            failure_message="a prepared cap did not draw, or its int32 and int64 stream indices " // &
            "disagreed, so the abort assertion above proves nothing")
    end subroutine test_random_disc_cap_unprepared_aborts
    !
    !> See scenario_sphere_polygon_too_few_vertices.
    subroutine test_sphere_polygon_too_few_vertices_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_too_few_vertices", expect_abort=.true., &
            failure_message="a two-vertex polygon was expected to abort", &
            required_stderr="pf_sky_polygon%init: at least 3 vertices are needed, ra and dec of one size (got 2 and 2)")
    end subroutine test_sphere_polygon_too_few_vertices_aborts
    !
    !> See scenario_sphere_polygon_size_mismatch.
    subroutine test_sphere_polygon_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_size_mismatch", expect_abort=.true., &
            failure_message="vertex arrays of sizes 4 and 3 were expected to abort", &
            required_stderr="pf_sky_polygon%init: at least 3 vertices are needed, ra and dec of one size (got 4 and 3)")
    end subroutine test_sphere_polygon_size_mismatch_aborts
    !
    !> See scenario_sphere_polygon_nonfinite_vertex.
    subroutine test_sphere_polygon_nonfinite_vertex_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_nonfinite_vertex", expect_abort=.true., &
            failure_message="a polygon with an infinite right ascension was expected to abort", &
            required_stderr="pf_sky_polygon%init: vertex 2 must be finite with dec in [-90, 90] (got Infinity, " // &
            "0.0000000E+00)")
    end subroutine test_sphere_polygon_nonfinite_vertex_aborts
    !
    !> See scenario_sphere_polygon_nan_vertex.
    subroutine test_sphere_polygon_nan_vertex_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_nan_vertex", expect_abort=.true., &
            failure_message="a polygon with a NaN right ascension was expected to abort", &
            required_stderr="(got NaN, -Infinity)")
    end subroutine test_sphere_polygon_nan_vertex_aborts
    !
    !> See scenario_sphere_polygon_extreme_vertex.
    subroutine test_sphere_polygon_extreme_vertex_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_extreme_vertex", expect_abort=.true., &
            failure_message="a polygon with a 1e200 declination was expected to abort", &
            required_stderr="(got 1.0000000E-200, 1.0000000E+200)")
    end subroutine test_sphere_polygon_extreme_vertex_aborts
    !
    !> See scenario_sphere_polygon_strict_short_way.
    subroutine test_sphere_polygon_strict_short_way_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_strict_short_way", expect_abort=.true., &
            failure_message="strict= was expected to refuse a suspected short-way RA band", &
            required_stderr="degrees of RA in two clusters at the ends of that span, which names the band the")
    end subroutine test_sphere_polygon_strict_short_way_aborts
    !
    !> See scenario_sphere_polygon_dec_out_of_range.
    subroutine test_sphere_polygon_dec_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_dec_out_of_range", expect_abort=.true., &
            failure_message="a polygon with a vertex at declination 90.5 was expected to abort", &
            required_stderr="pf_sky_polygon%init: vertex 3 must be finite with dec in [-90, 90] (got 1.0000000E+01, " // &
            "9.0500000E+01)")
    end subroutine test_sphere_polygon_dec_out_of_range_aborts
    !
    !> See scenario_sphere_polygon_bad_edge_rule.
    subroutine test_sphere_polygon_bad_edge_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_bad_edge_rule", expect_abort=.true., &
            failure_message="edge rule 2 was expected to abort", &
            required_stderr="pf_sky_polygon%init: edges must be PF_EDGE_RADEC (0) or PF_EDGE_GREAT_CIRCLE (1), got 2")
    end subroutine test_sphere_polygon_bad_edge_rule_aborts
    !
    !> See scenario_sphere_polygon_ra_extent_over_360.
    subroutine test_sphere_polygon_ra_extent_over_360_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_ra_extent_over_360", expect_abort=.true., &
            failure_message="a polygon spanning 360.5 degrees of RA was expected to abort", &
            required_stderr="pf_sky_polygon%init: the vertices span 3.6050000E+02 degrees of RA; write a polygon " // &
            "crossing RA = 0 continuously (350, 370), and no polygon may span more than 360")
    end subroutine test_sphere_polygon_ra_extent_over_360_aborts
    !
    !> See scenario_sphere_polygon_not_in_hemisphere.
    subroutine test_sphere_polygon_not_in_hemisphere_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_not_in_hemisphere", expect_abort=.true., &
            failure_message="a great-circle polygon spanning 200 degrees was expected to abort", &
            required_stderr="pf_sky_polygon%init: vertex 1 is 1.0000000E+02 degrees from the vertices' mean direction; " // &
            "every vertex must be within 89.9 degrees of it (the polygon must fit an open hemisphere) " // &
            "-- split it")
    end subroutine test_sphere_polygon_not_in_hemisphere_aborts
    !
    !> See scenario_sphere_polygon_vertices_cancel.
    subroutine test_sphere_polygon_vertices_cancel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_vertices_cancel", expect_abort=.true., &
            failure_message="a polygon whose vertex directions sum to zero was expected to abort", &
            required_stderr="pf_sky_polygon%init: the vertices' unit vectors sum to exactly zero, so they name " // &
            "no mean direction; every vertex must be within 89.9 degrees of it (the polygon must fit an open " // &
            "hemisphere) -- split it")
    end subroutine test_sphere_polygon_vertices_cancel_aborts
    !
    !> See scenario_sphere_polygon_zero_area.
    subroutine test_sphere_polygon_zero_area_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_zero_area", expect_abort=.true., &
            failure_message="a polygon with every vertex at one declination was expected to abort", &
            required_stderr="pf_sky_polygon%init: the polygon has zero area")
    end subroutine test_sphere_polygon_zero_area_aborts
    !
    !> See scenario_sphere_polygon_below_acceptance_floor.
    subroutine test_sphere_polygon_below_acceptance_floor_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_below_acceptance_floor", expect_abort=.true., &
            failure_message="a sliver covering 2.5e-4 of its box was expected to abort", &
            required_stderr="of its bounding box, below the 1e-3 floor; split it into pieces")
    end subroutine test_sphere_polygon_below_acceptance_floor_aborts
    !
    !> See scenario_sphere_polygon_init_twice.
    subroutine test_sphere_polygon_init_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_init_twice", expect_abort=.true., &
            failure_message="a second %init was expected to abort", &
            required_stderr="pf_sky_polygon%init: already initialised; call %clear first")
    end subroutine test_sphere_polygon_init_twice_aborts
    !
    !> See scenario_sphere_polygon_contains_before_init.
    subroutine test_sphere_polygon_contains_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_contains_before_init", expect_abort=.true., &
            failure_message="%contains on an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%contains: %init has not run")
    end subroutine test_sphere_polygon_contains_before_init_aborts
    !
    !> See scenario_sphere_polygon_area_before_init.
    subroutine test_sphere_polygon_area_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_area_before_init", expect_abort=.true., &
            failure_message="%area on an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%area: %init has not run")
    end subroutine test_sphere_polygon_area_before_init_aborts
    !
    !> See scenario_sphere_polygon_area_deg2_before_init.
    subroutine test_sphere_polygon_area_deg2_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_area_deg2_before_init", expect_abort=.true., &
            failure_message="%area_deg2 on an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%area_deg2: %init has not run")
    end subroutine test_sphere_polygon_area_deg2_before_init_aborts
    !
    !> See scenario_sphere_polygon_acceptance_before_init.
    subroutine test_sphere_polygon_acceptance_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_acceptance_before_init", expect_abort=.true., &
            failure_message="%acceptance on an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%acceptance: %init has not run")
    end subroutine test_sphere_polygon_acceptance_before_init_aborts
    !
    !> See scenario_sphere_polygon_bounds_before_init.
    subroutine test_sphere_polygon_bounds_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_bounds_before_init", expect_abort=.true., &
            failure_message="%bounds on an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%bounds: %init has not run")
    end subroutine test_sphere_polygon_bounds_before_init_aborts
    !
    !> See scenario_sphere_polygon_random_before_init.
    subroutine test_sphere_polygon_random_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_random_before_init", expect_abort=.true., &
            failure_message="a draw from an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%random_at: %init has not run")
    end subroutine test_sphere_polygon_random_before_init_aborts
    !
    !> See scenario_sphere_polygon_fill_before_init.
    subroutine test_sphere_polygon_fill_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_fill_before_init", expect_abort=.true., &
            failure_message="a fill from an unbuilt polygon was expected to abort", &
            required_stderr="pf_sky_polygon%random_fill: %init has not run")
    end subroutine test_sphere_polygon_fill_before_init_aborts
    !
    !> See scenario_sphere_polygon_fill_size_mismatch.
    subroutine test_sphere_polygon_fill_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_fill_size_mismatch", expect_abort=.true., &
            failure_message="a polygon fill into arrays of sizes 3 and 2 was expected to abort", &
            required_stderr="pf_sky_polygon%random_fill: ra and dec must have the same size (got 3 and 2)")
    end subroutine test_sphere_polygon_fill_size_mismatch_aborts
    !
    !> See scenario_sphere_polygon_fill_draw_overflow.
    subroutine test_sphere_polygon_fill_draw_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_fill_draw_overflow", expect_abort=.true., &
            failure_message="a polygon fill ending past huge(int64) was expected to abort", &
            required_stderr="pf_sky_polygon%random_fill: draw + n - 1 must not exceed huge(int64) (got " // &
            "9223372036854775807 + 2 - 1)")
    end subroutine test_sphere_polygon_fill_draw_overflow_aborts
    !
    !> See scenario_sphere_polygon_candidate_cap_reached.
    subroutine test_sphere_polygon_candidate_cap_reached_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_polygon_candidate_cap_reached", expect_abort=.true., &
            failure_message="a draw from a sliver below the floor was expected to reach the candidate cap and abort", &
            required_stderr="pf_sky_polygon%random_at: 100000 candidates rejected -- the acceptance rate %init " // &
            "admitted cannot produce this; report it")
    end subroutine test_sphere_polygon_candidate_cap_reached_aborts
    !
    !> See scenario_sphere_stream_exhausted.
    subroutine test_sphere_stream_exhausted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_stream_exhausted", expect_abort=.true., &
            failure_message="a region draw from an exhausted stream was expected to abort", &
            required_stderr="pf_sky_polygon%random_next: the stream is exhausted -- it addresses at most 2**63 words")
    end subroutine test_sphere_stream_exhausted_aborts
    !
    !> See scenario_sphere_pixel_grid_not_built.
    subroutine test_sphere_pixel_grid_not_built_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_pixel_grid_not_built", expect_abort=.true., &
            failure_message="a pixel draw on an unbuilt grid was expected to abort", &
            required_stderr="pf_random_pixel_at: the grid has not been built; call %init(nside, scheme) first")
    end subroutine test_sphere_pixel_grid_not_built_aborts
    !
    !> See scenario_sphere_pixel_nside_over_limit.
    subroutine test_sphere_pixel_nside_over_limit_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_pixel_nside_over_limit", expect_abort=.true., &
            failure_message="a pixel draw at nside 2**25 was expected to abort", &
            required_stderr="pf_random_pixel_at: nside 33554432 exceeds 2**24, above which a unit vector cannot name a " // &
            "pixel near a pole (see the HEALPix page)")
    end subroutine test_sphere_pixel_nside_over_limit_aborts
    !
    !> See scenario_sphere_pixel_ipix_out_of_range.
    subroutine test_sphere_pixel_ipix_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_pixel_ipix_out_of_range", expect_abort=.true., &
            failure_message="a draw in pixel 768 of nside 8 was expected to abort", &
            required_stderr="pf_random_pixel_radec_at: ipix 768 is outside [0, 768)")
    end subroutine test_sphere_pixel_ipix_out_of_range_aborts
    !
    !> See scenario_sphere_mask_empty_list.
    subroutine test_sphere_mask_empty_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_mask_empty_list", expect_abort=.true., &
            failure_message="a mask draw over no pixels was expected to abort", &
            required_stderr="pf_random_mask_at: pixels must be non-empty with every entry in [0, 768) (the list is " // &
            "empty)")
    end subroutine test_sphere_mask_empty_list_aborts
    !
    !> See scenario_sphere_mask_entry_out_of_range.
    subroutine test_sphere_mask_entry_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_mask_entry_out_of_range", expect_abort=.true., &
            failure_message="a mask draw choosing pixel 768 of nside 8 was expected to abort", &
            required_stderr="pf_random_mask_at: pixels must be non-empty with every entry in [0, 768) (entry 1 is 768)")
    end subroutine test_sphere_mask_entry_out_of_range_aborts
    !
    !> See scenario_sphere_mask_entry_out_of_range_int32.
    subroutine test_sphere_mask_entry_out_of_range_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_mask_entry_out_of_range_int32", expect_abort=.true., &
            failure_message="a mask draw choosing pixel -1 was expected to abort", &
            required_stderr="pf_random_mask_radec_at: pixels must be non-empty with every entry in [0, 768) (entry 1 " // &
            "is -1)")
    end subroutine test_sphere_mask_entry_out_of_range_int32_aborts
    !
    !> See scenario_sphere_fill_mask_bad_shape.
    subroutine test_sphere_fill_mask_bad_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_bad_shape", expect_abort=.true., &
            failure_message="a mask fill into two rows was expected to abort", &
            required_stderr="pf_random_fill_mask: v must be shaped (3, n) (got 2 rows)")
    end subroutine test_sphere_fill_mask_bad_shape_aborts
    !
    !> See scenario_sphere_fill_mask_radec_size_mismatch.
    subroutine test_sphere_fill_mask_radec_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_radec_size_mismatch", expect_abort=.true., &
            failure_message="a mask RA/Dec fill into arrays of sizes 4 and 3 was expected to abort", &
            required_stderr="pf_random_fill_mask_radec: ra and dec must have the same size (got 4 and 3)")
    end subroutine test_sphere_fill_mask_radec_size_mismatch_aborts
    !
    !> See scenario_sphere_fill_mask_bad_shape_int32.
    subroutine test_sphere_fill_mask_bad_shape_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_bad_shape_int32", expect_abort=.true., &
            failure_message="a mask fill into two rows was expected to abort", &
            required_stderr="pf_random_fill_mask: v must be shaped (3, n) (got 2 rows)")
    end subroutine test_sphere_fill_mask_bad_shape_int32_aborts
    !
    !> See scenario_sphere_fill_mask_radec_size_mismatch_int32.
    subroutine test_sphere_fill_mask_radec_size_mismatch_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_radec_size_mismatch_int32", expect_abort=.true., &
            failure_message="a mask RA/Dec fill into arrays of sizes 4 and 3 was expected to abort", &
            required_stderr="pf_random_fill_mask_radec: ra and dec must have the same size (got 4 and 3)")
    end subroutine test_sphere_fill_mask_radec_size_mismatch_int32_aborts
    !
    !> See scenario_sphere_fill_mask_entry_out_of_range.
    subroutine test_sphere_fill_mask_entry_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_entry_out_of_range", expect_abort=.true., &
            failure_message="a mask fill over a list holding pixel 768 was expected to abort", &
            required_stderr="pf_random_fill_mask: pixels must be non-empty with every entry in [0, 768) (entry 1000 is " // &
            "768)")
    end subroutine test_sphere_fill_mask_entry_out_of_range_aborts
    !
    !> See scenario_sphere_fill_mask_radec_entry_out_of_range.
    subroutine test_sphere_fill_mask_radec_entry_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_radec_entry_out_of_range", expect_abort=.true., &
            failure_message="a mask RA/Dec fill over a list holding pixel -3 was expected to abort", &
            required_stderr="pf_random_fill_mask_radec: pixels must be non-empty with every entry in [0, 768) (entry 3 " // &
            "is -3)")
    end subroutine test_sphere_fill_mask_radec_entry_out_of_range_aborts
    !
    !> See scenario_sphere_fill_mask_draw_overflow.
    subroutine test_sphere_fill_mask_draw_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fill_mask_draw_overflow", expect_abort=.true., &
            failure_message="a mask fill ending past huge(int64) was expected to abort", &
            required_stderr="pf_random_fill_mask: draw + n - 1 must not exceed huge(int64) (got 9223372036854775807 + " // &
            "2 - 1)")
    end subroutine test_sphere_fill_mask_draw_overflow_aborts
    !
    !> See scenario_sphere_offset_dec_out_of_range.
    subroutine test_sphere_offset_dec_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_offset_dec_out_of_range", expect_abort=.true., &
            failure_message="an offset from declination 90.5 was expected to abort", &
            required_stderr="pf_offset_radec: dec0 must be in [-90, 90] and sep_deg at least 0 (got dec0 = " // &
            "9.0500000E+01, sep_deg = 1.0000000E+00)")
    end subroutine test_sphere_offset_dec_out_of_range_aborts
    !
    !> See scenario_sphere_offset_negative_separation.
    subroutine test_sphere_offset_negative_separation_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_offset_negative_separation", expect_abort=.true., &
            failure_message="an offset by -1 degree was expected to abort", &
            required_stderr="pf_offset_radec: dec0 must be in [-90, 90] and sep_deg at least 0 (got dec0 = " // &
            "2.0000000E+01, sep_deg = -1.0000000E+00)")
    end subroutine test_sphere_offset_negative_separation_aborts
    !
    !> See scenario_skycoord_convert_unknown_system.
    subroutine test_skycoord_convert_unknown_system_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_convert_unknown_system", expect_abort=.true., &
            failure_message="a conversion out of PF_COORD_UNKNOWN was expected to abort", &
            required_stderr="pf_sky_convert: from and to must each be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
            "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got from = 0, to = 0)")
    end subroutine test_skycoord_convert_unknown_system_aborts
    !
    !> See scenario_skycoord_system_name_not_a_selector.
    subroutine test_skycoord_system_name_not_a_selector_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_system_name_not_a_selector", expect_abort=.true., &
            failure_message="naming system 7 was expected to abort", &
            required_stderr="pf_coord_system_name: system must be a PF_COORD_* selector (got 7)")
    end subroutine test_skycoord_system_name_not_a_selector_aborts
    !
    !> See scenario_skycoord_radec2str_width_overflow.
    subroutine test_skycoord_radec2str_width_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_radec2str_width_overflow", expect_abort=.true., &
            failure_message="writing at precision 10 was expected to abort", &
            required_stderr="pf_radec2str: precision must be in [0, 9] (got 10)")
    end subroutine test_skycoord_radec2str_width_overflow_aborts
    !
    !> See scenario_skycoord_text_bad_separator.
    subroutine test_skycoord_text_bad_separator_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_text_bad_separator", expect_abort=.true., &
            failure_message="writing with a slash between the fields was expected to abort", &
            required_stderr='pf_dec2str: sep must be ":", " " or "hms" (got "/")')
    end subroutine test_skycoord_text_bad_separator_aborts
    !
    !> See scenario_skycoord_zcmb_unknown_system.
    subroutine test_skycoord_zcmb_unknown_system_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_zcmb_unknown_system", expect_abort=.true., &
            failure_message="a redshift in PF_COORD_UNKNOWN was expected to abort", &
            required_stderr="pf_zhel2zcmb: system must be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
            "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got 0)")
    end subroutine test_skycoord_zcmb_unknown_system_aborts
    !
    !> See scenario_skycoord_radec2tan_dec0_out_of_range.
    subroutine test_skycoord_radec2tan_dec0_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_radec2tan_dec0_out_of_range", expect_abort=.true., &
            failure_message="a tangent point beyond a pole was expected to abort", &
            required_stderr="pf_radec2tan: dec0 must be in [-90, 90] (got")
    end subroutine test_skycoord_radec2tan_dec0_out_of_range_aborts

    !> See scenario_skycoord_tan2radec_dec0_out_of_range.
    subroutine test_skycoord_tan2radec_dec0_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_tan2radec_dec0_out_of_range", expect_abort=.true., &
            failure_message="a tangent point beyond a pole was expected to abort the inverse", &
            required_stderr="pf_tan2radec: dec0 must be in [-90, 90] (got")
    end subroutine test_skycoord_tan2radec_dec0_out_of_range_aborts

    !> See scenario_skycoord_zcmb2zhel_unknown_system.
    subroutine test_skycoord_zcmb2zhel_unknown_system_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_zcmb2zhel_unknown_system", expect_abort=.true., &
            failure_message="a heliocentric redshift in PF_COORD_UNKNOWN was expected to abort", &
            required_stderr="pf_zcmb2zhel: system must be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
            "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got 0)")
    end subroutine test_skycoord_zcmb2zhel_unknown_system_aborts

    !> See scenario_skycoord_rotation_apply_before_init.
    subroutine test_skycoord_rotation_apply_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_rotation_apply_before_init", expect_abort=.true., &
            failure_message="applying a rotation with no %init was expected to abort", &
            required_stderr="pf_sky_rotation%apply: %init has not run")
    end subroutine test_skycoord_rotation_apply_before_init_aborts
    !
    !> See scenario_skycoord_rotation_init_unknown_system.
    subroutine test_skycoord_rotation_init_unknown_system_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_rotation_init_unknown_system", expect_abort=.true., &
            failure_message="preparing a rotation into system 6 was expected to abort", &
            required_stderr="pf_sky_rotation%init: from and to must each be PF_COORD_ICRS (1), PF_COORD_GALACTIC (2), " // &
            "PF_COORD_ECLIPTIC (3), PF_COORD_SUPERGALACTIC (4) or PF_COORD_FK5 (5) (got from = 2, to = 6)")
    end subroutine test_skycoord_rotation_init_unknown_system_aborts
    !
    !> See scenario_skycoord_apply_pm_dec_out_of_range.
    subroutine test_skycoord_apply_pm_dec_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "skycoord_apply_pm_dec_out_of_range", expect_abort=.true., &
            failure_message="a proper motion from declination 90.5 was expected to abort", &
            required_stderr="pf_apply_pm: dec must be in [-90, 90] (got 9.0500000E+01)")
    end subroutine test_skycoord_apply_pm_dec_out_of_range_aborts
    !
    !> See scenario_sphere_fibonacci_n_not_positive.
    subroutine test_sphere_fibonacci_n_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fibonacci_n_not_positive", expect_abort=.true., &
            failure_message="a Fibonacci grid of no points was expected to abort", &
            required_stderr="pf_fibonacci_grid: n must be at least 1 and vec shaped (3, n) (got n = 0, 3 x 0)")
    end subroutine test_sphere_fibonacci_n_not_positive_aborts
    !
    !> See scenario_sphere_fibonacci_bad_shape.
    subroutine test_sphere_fibonacci_bad_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fibonacci_bad_shape", expect_abort=.true., &
            failure_message="a five-point grid into four columns was expected to abort", &
            required_stderr="pf_fibonacci_grid: n must be at least 1 and vec shaped (3, n) (got n = 5, 3 x 4)")
    end subroutine test_sphere_fibonacci_bad_shape_aborts
    !
    !> See scenario_sphere_fibonacci_bad_frame.
    subroutine test_sphere_fibonacci_bad_frame_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fibonacci_bad_frame", expect_abort=.true., &
            failure_message="a Fibonacci grid in frame 2 was expected to abort", &
            required_stderr="pf_fibonacci_grid: frame must be PF_HP_DEC_NORTH (0) or PF_HP_DEC_SOUTH (1), got 2")
    end subroutine test_sphere_fibonacci_bad_frame_aborts
    !
    !> See scenario_sphere_fibonacci_radec_bad_size.
    subroutine test_sphere_fibonacci_radec_bad_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_fibonacci_radec_bad_size", expect_abort=.true., &
            failure_message="a three-point RA/Dec grid into a declination array of 2 was expected to abort", &
            required_stderr="pf_fibonacci_grid_radec: n must be at least 1 and ra and dec sized n (got n = 3, 3 and 2)")
    end subroutine test_sphere_fibonacci_radec_bad_size_aborts
    !
    !> See scenario_sphere_radec2vec_bad_frame.
    subroutine test_sphere_radec2vec_bad_frame_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_radec2vec_bad_frame", expect_abort=.true., &
            failure_message="a conversion in frame -1 was expected to abort", &
            required_stderr="pf_radec2vec: frame must be PF_HP_DEC_NORTH (0) or PF_HP_DEC_SOUTH (1), got -1")
    end subroutine test_sphere_radec2vec_bad_frame_aborts
    !
    !> See scenario_sphere_vec2radec_bad_frame.
    subroutine test_sphere_vec2radec_bad_frame_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "sphere_vec2radec_bad_frame", expect_abort=.true., &
            failure_message="a conversion in frame 7 was expected to abort", &
            required_stderr="pf_vec2radec: frame must be PF_HP_DEC_NORTH (0) or PF_HP_DEC_SOUTH (1), got 7")
    end subroutine test_sphere_vec2radec_bad_frame_aborts
    !

end module test_analysis_errors
