!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests that the library correctly aborts (via Fortran `error stop`) on
!> invalid input. Because `error stop` terminates the whole process, each
!> scenario is run in a subprocess (the "error_scenarios" helper test
!> program) and only the process exit status is observed here.
!> NB: always pass the failure message directly to `check(error, cond, message)`.
!> Do NOT follow a failed `check` with a separate `test_failed` call on the same
!> `error` variable: `check` already allocates `error` internally when `cond` is
!> false, and passing that already-allocated `error` into another `intent(out)`
!> argument (as `test_failed` also expects) triggers Fortran's automatic
!> finalization of the old value before reassignment, which for test-drive's
!> `error_type` calls its FINAL `escalate_error` and aborts the whole process.
module test_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    !$ use omp_lib, only : omp_get_max_threads
    !
    implicit none
    private
    public :: collect_tests_parquet_errors
    ! Exposed for test_writing.f90 to reuse (avoids a second copy of the same
    ! subprocess-driving helper -- see check_scenario_exit_status below).
    public :: check_scenario_exit_status
    public :: check_scenario_exit_status_and_stderr
    public :: check_scenario_streams
    public :: check_scenario_exit_status_and_no_output
    ! Exposed for test_writing.f90's own qc-warning scenario helpers -- see
    ! run_error_scenario below.
    public :: run_error_scenario
    public :: scenario_capture_contains
    ! Exposed for run_tester.f90, which is the only place that knows whether
    ! this run is worth pre-running the whole scenario set for -- see
    ! prime_error_scenarios below.
    public :: prime_error_scenarios
    !
    ! Cached path to the built error_scenarios helper binary -- see
    ! get_error_scenarios_bin below.
    character(len=:), allocatable, save :: g_error_scenarios_bin
    logical, save :: g_error_scenarios_bin_ready = .false.
    !
    ! Priming state -- see prime_error_scenarios below. g_prime_ok is the only
    ! one run_error_scenario consults, and it is set by THIS process only after
    ! it has wiped and repopulated prime_dir, so a directory left behind by an
    ! earlier run (a different binary, different expectations) can never be
    ! mistaken for this run's results.
    logical, save :: g_prime_done = .false.
    logical, save :: g_prime_ok = .false.
    ! Where prime_error_scenarios writes one .out/.err/.status triple per scenario.
    character(len=*), parameter :: prime_dir = "test_run/.primed"
    ! The file prime_error_scenarios reads the scenario list out of. It is the
    ! shell runner's own `scenarios=(...)` array rather than a second copy of
    ! the names here: that array is already maintained as the complete mirror
    ! of error_scenarios.f90's `select case` (tools/check_source_conventions.py
    ! now fails when it drifts), and a second list would be one more thing to
    ! forget. Its `concurrency_scenarios=(...)` array is deliberately NOT read
    ! -- those need a real OpenMP race and are left to run one at a time.
    character(len=*), parameter :: scenario_list_file = "tools/run_error_scenarios.sh"
    !
contains
    !
    subroutine collect_tests_parquet_errors(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end. Neither obvious alternative works:
        ! a single array constructor exceeds the standard's 255-continuation-line limit (this
        ! list is far longer than that), and the self-referential append form
        ! `testsuite = [testsuite, ...]` compiles everywhere but makes nagfor 7.2 double-free
        ! each entry's allocatable name string at run time ("Invalid deallocation of size N:
        ! block was already deallocated"), aborting the suite. Both traps are invisible under
        ! gfortran, so keep this shape: assign each part, then concatenate once.
        type(unittest_type), allocatable :: p1(:), p2(:), p3(:), p4(:), p5(:), p6(:), p7(:)

        p1 = [ &
            new_unittest("control scenario exits cleanly", test_ok_scenario_exits_cleanly), &
            new_unittest("write to undeclared column aborts", test_write_undeclared_column_aborts), &
            new_unittest("writing an undeclared int64 column aborts", &
                test_write_undeclared_column_int64_aborts), &
            new_unittest("writing an undeclared float32 column aborts", &
                test_write_undeclared_column_float32_aborts), &
            new_unittest("writing an undeclared float64 column aborts", &
                test_write_undeclared_column_float64_aborts), &
            new_unittest("writing an undeclared logical column aborts", &
                test_write_undeclared_column_logical_aborts), &
            new_unittest("writing an undeclared string column aborts", &
                test_write_undeclared_column_string_aborts), &
            new_unittest("writing an undeclared column via a compact string write aborts", &
                test_write_undeclared_column_string_compact_aborts), &
            new_unittest("writing an undeclared int32 matrix column aborts", &
                test_write_undeclared_column_int32_matrix_aborts), &
            new_unittest("writing an undeclared int64 matrix column aborts", &
                test_write_undeclared_column_int64_matrix_aborts), &
            new_unittest("writing an undeclared float32 matrix column aborts", &
                test_write_undeclared_column_float32_matrix_aborts), &
            new_unittest("writing an undeclared float64 matrix column aborts", &
                test_write_undeclared_column_float64_matrix_aborts), &
            new_unittest("writing an undeclared logical matrix column aborts", &
                test_write_undeclared_column_logical_matrix_aborts), &
            new_unittest("writing an undeclared string matrix column aborts", &
                test_write_undeclared_column_string_matrix_aborts), &
            new_unittest("writing a int64 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_int64_aborts), &
            new_unittest("writing a float32 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_float32_aborts), &
            new_unittest("writing a float64 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_float64_aborts), &
            new_unittest("writing a logical values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_logical_aborts), &
            new_unittest("writing a string values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_string_aborts), &
            new_unittest("writing a int32 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_int32_matrix_aborts), &
            new_unittest("writing a int64 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_int64_matrix_aborts), &
            new_unittest("writing a float32 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_float32_matrix_aborts), &
            new_unittest("writing a float64 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_float64_matrix_aborts), &
            new_unittest("writing a logical matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_logical_matrix_aborts), &
            new_unittest("writing a string matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_string_matrix_aborts), &
            new_unittest("streaming: writing an undeclared int32 chunk column aborts", &
                test_write_chunk_undeclared_column_int32_aborts), &
            new_unittest("streaming: writing an undeclared int64 chunk column aborts", &
                test_write_chunk_undeclared_column_int64_aborts), &
            new_unittest("streaming: writing an undeclared float32 chunk column aborts", &
                test_write_chunk_undeclared_column_float32_aborts), &
            new_unittest("streaming: writing an undeclared float64 chunk column aborts", &
                test_write_chunk_undeclared_column_float64_aborts), &
            new_unittest("streaming: writing an undeclared logical chunk column aborts", &
                test_write_chunk_undeclared_column_logical_aborts), &
            new_unittest("streaming: writing an undeclared string chunk column aborts", &
                test_write_chunk_undeclared_column_string_aborts), &
            new_unittest("streaming: writing an undeclared column via a compact string chunk write aborts", &
                test_write_chunk_undeclared_column_string_compact_aborts), &
            new_unittest("streaming: writing an undeclared int32 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_int32_matrix_aborts), &
            new_unittest("streaming: writing an undeclared int64 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_int64_matrix_aborts), &
            new_unittest("streaming: writing an undeclared float32 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_float32_matrix_aborts), &
            new_unittest("streaming: writing an undeclared float64 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_float64_matrix_aborts), &
            new_unittest("streaming: writing an undeclared logical matrix chunk column aborts", &
                test_write_chunk_undeclared_column_logical_matrix_aborts), &
            new_unittest("streaming: writing an undeclared string matrix chunk column aborts", &
                test_write_chunk_undeclared_column_string_matrix_aborts), &
            new_unittest("streaming: writing a int32 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_int32_aborts), &
            new_unittest("streaming: writing a int64 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_int64_aborts), &
            new_unittest("streaming: writing a float32 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_float32_aborts), &
            new_unittest("streaming: writing a float64 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_float64_aborts), &
            new_unittest("streaming: writing a logical chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_logical_aborts), &
            new_unittest("streaming: writing a string chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_string_aborts), &
            new_unittest("streaming: writing a int32 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_int32_matrix_aborts), &
            new_unittest("streaming: writing a int64 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_int64_matrix_aborts), &
            new_unittest("streaming: writing a float32 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_float32_matrix_aborts), &
            new_unittest("streaming: writing a float64 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_float64_matrix_aborts), &
            new_unittest("streaming: writing a logical chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_logical_matrix_aborts), &
            new_unittest("streaming: writing a string chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_string_matrix_aborts), &
            new_unittest("streaming: writing an over-length string into a fixed-size string matrix chunk " // &
                "column aborts", test_write_chunk_string_matrix_exceeds_array_size_aborts), &
            new_unittest("streaming: writing an over-length string into a fixed-size string vector chunk " // &
                "column (flat form) aborts", test_write_chunk_string_exceeds_array_size_aborts), &
            new_unittest("streaming: writing a chunk column before parquet_new_row_group aborts", &
                test_write_chunk_no_row_group_open_aborts), &
            new_unittest("streaming: writing a chunk whose row count doesn't match the open row group aborts", &
                test_write_chunk_row_count_mismatch_aborts), &
            new_unittest("streaming: writing a chunk with type mismatch aborts", &
                test_write_chunk_type_mismatch_aborts), &
            new_unittest("chunked read: hard qc violation in one row group aborts, naming that row group", &
                test_read_chunk_qc_hard_aborts), &
            new_unittest("chunked read: soft qc violation warns once per column across multiple row groups", &
                test_read_chunk_qc_soft_warns), &
            new_unittest("chunked read: check_complete=.true. aborts on a column missing a row group", &
                test_read_chunk_check_complete_hard_aborts), &
            new_unittest("chunked read: an out-of-range row_group aborts", &
                test_read_chunk_row_group_out_of_range_aborts), &
            new_unittest("parquet_get_chunk_size with an out-of-range row_group aborts", &
                test_get_chunk_size_row_group_out_of_range_aborts), &
            new_unittest("write with type mismatch aborts", test_write_type_mismatch_aborts), &
            new_unittest("writing the same column twice aborts", test_write_column_twice_aborts), &
            new_unittest("writing the same column twice on a schema-less writer aborts", &
                test_write_column_twice_no_schema_aborts), &
            new_unittest("validating an invalid data_type aborts", test_validate_bad_data_type_aborts), &
            new_unittest("validating data_type: decimal aborts", test_validate_excluded_decimal_type_aborts), &
            new_unittest("validating a fields: entry with no name: sub-key aborts", &
                test_validate_empty_field_name_aborts), &
            new_unittest("validating a duplicate field name aborts", test_validate_duplicate_name_aborts), &
            new_unittest("validating a MAML without table: aborts", test_validate_missing_table_aborts), &
            new_unittest("validating a MAML without fields aborts", test_validate_no_fields_aborts), &
            new_unittest("a MAML with a trailing, unclosed depends: list and no fields: aborts", &
                test_validate_trailing_depends_no_fields_aborts), &
            new_unittest("a MAML with a trailing, unclosed keywords: list and no fields: aborts", &
                test_validate_trailing_keywords_no_fields_aborts), &
            new_unittest("validating a MAML with an unknown top-level section aborts", &
                test_validate_unknown_top_level_section_aborts), &
            new_unittest("validating a MAML with an unknown field sub-key aborts", &
                test_validate_unknown_field_subkey_aborts), &
            new_unittest("validating a MAML with an unknown qc: sub-key aborts", &
                test_validate_unknown_qc_subkey_aborts), &
            new_unittest("user MAML with unknown column aborts", test_validate_user_maml_unknown_column_aborts), &
            new_unittest("col_map: referencing an unknown internal column aborts", &
                test_validate_col_map_unknown_internal_aborts), &
            new_unittest("col_map: duplicate internal column key aborts", &
                test_validate_col_map_duplicate_internal_aborts), &
            new_unittest("col_map: two internal columns colliding on the same output name aborts", &
                test_validate_col_map_output_collision_aborts), &
            new_unittest("col_map: renamed output not declared in fields: aborts", &
                test_validate_col_map_output_not_declared_aborts), &
            new_unittest("col_map: remapped internal column also declared un-renamed in fields: aborts", &
                test_validate_col_map_internal_also_in_fields_aborts), &
            new_unittest("col_map: output name coincides with an existing base column aborts", &
                test_validate_col_map_output_matches_other_field_aborts), &
            new_unittest("set_column_available on a deactivated column aborts", &
                test_set_column_available_deactivated_aborts), &
            new_unittest("set_column_unavailable on a deactivated column aborts", &
                test_set_column_unavailable_deactivated_aborts), &
            new_unittest("get_column_index on unknown column aborts", test_get_column_index_not_found_aborts), &
            new_unittest("get_field_name with an index below 1 aborts", test_get_field_name_index_too_low_aborts), &
            new_unittest("get_field_name with an index past get_num_fields aborts", &
                test_get_field_name_index_too_high_aborts), &
            new_unittest("write_maml without metadata aborts", test_write_maml_without_metadata_aborts), &
            new_unittest("reading a column with genuine Null values aborts", &
                test_read_column_with_nulls_aborts), &
            new_unittest("reading a column of an unsupported physical type aborts", &
                test_read_unsupported_physical_type_aborts), &
            new_unittest("array-mode whole-column read of an int32 vector column as logical aborts", &
                test_read_array_full_bool_type_mismatch_aborts), &
            new_unittest("array-mode whole-column read of an int32 vector column as string aborts", &
                test_read_array_full_string_type_mismatch_aborts), &
            new_unittest("array-mode row_mode read of an int32 vector column as logical aborts", &
                test_read_array_row_mode_bool_type_mismatch_aborts), &
            new_unittest("array-mode row_mode read of an int32 vector column as string aborts", &
                test_read_array_row_mode_string_type_mismatch_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (logical) aborts", &
                test_read_array_em_filt_bool_oob_aborts), &
            new_unittest("array-mode element_mode (filtered) read of an int32 vector column as logical aborts", &
                test_read_array_em_filt_bool_tm_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (string) aborts", &
                test_read_array_em_filt_string_oob_aborts), &
            new_unittest("array-mode element_mode (filtered) read of an int32 vector column as string aborts", &
                test_read_array_em_filt_string_tm_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (logical) aborts", &
                test_read_array_em_bool_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) read of an int32 vector column as logical aborts", &
                test_read_array_em_bool_tm_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (string) aborts", &
                test_read_array_em_string_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) read of an int32 vector column as string aborts", &
                test_read_array_em_string_tm_aborts), &
            new_unittest("array-mode chunk read of an int32 vector column as logical aborts", &
                test_read_array_column_chunk_bool_type_mismatch_aborts), &
            new_unittest("array-mode chunk read of an int32 vector column as string aborts", &
                test_read_array_column_chunk_string_type_mismatch_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (int32) aborts", &
                test_read_array_em_filt_int32_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (int32) aborts", &
                test_read_array_em_int32_oob_aborts), &
            new_unittest("reading an out-of-int32-range uint32 value aborts", &
                test_extended_uint32_overflow_int32_aborts) &
            ]
        p2 = [ &
            new_unittest("reading an out-of-int32-range uint64 value aborts", &
                test_extended_uint64_overflow_int32_aborts), &
            new_unittest("reading an out-of-int64-range uint64 value aborts", &
                test_extended_uint64_overflow_int64_aborts), &
            new_unittest("reading a non-integral double value into int32 aborts", &
                test_extended_real_nonintegral_int32_aborts), &
            new_unittest("reading an out-of-int32-range double value aborts", &
                test_extended_real_overflow_int32_aborts), &
            new_unittest("reading a non-integral double value into int64 aborts", &
                test_extended_real_nonintegral_int64_aborts), &
            new_unittest("reading an out-of-int64-range double value aborts", &
                test_extended_real_overflow_int64_aborts), &
            new_unittest("reading a non-integral decimal value into int32 aborts", &
                test_extended_decimal_nonintegral_int32_aborts), &
            new_unittest("reading an out-of-int32-range decimal value aborts", &
                test_extended_decimal_overflow_int32_aborts), &
            new_unittest("reading a non-integral decimal value into int64 aborts", &
                test_extended_decimal_nonintegral_int64_aborts), &
            new_unittest("reading an out-of-int64-range decimal value aborts", &
                test_extended_decimal_overflow_int64_aborts), &
            new_unittest("writing a vector column with col_size exceeding Arrow's FixedSizeListType limit aborts", &
                test_write_col_size_overflow_aborts), &
            new_unittest("writing a vector column with an explicit chunk_size*col_size exceeding Arrow/Parquet's " // &
                "list-element-count limit aborts", test_write_list_element_count_chunk_size_overflow_aborts), &
            new_unittest("parquet_new_row_group with an explicit nrows*col_size exceeding Arrow/Parquet's " // &
                "list-element-count limit aborts", test_row_group_explicit_nrows_overflow_aborts), &
            new_unittest("closing a writer with a dangling open row group aborts", &
                test_row_group_dangling_at_close_aborts), &
            new_unittest("closing a writer with an under-covered whole column aborts", &
                test_row_group_whole_column_undercovered_aborts), &
            new_unittest("a row group reading past a whole column's own row count aborts", &
                test_row_group_whole_column_overrun_aborts), &
            new_unittest("introducing a new column after the first row group aborts", &
                test_row_group_new_column_after_first_aborts), &
            new_unittest("a whole-column write after the streaming row-group API already started aborts", &
                test_row_group_whole_column_after_streaming_started_aborts), &
            new_unittest("starting a new row group while one is already open aborts", &
                test_row_group_started_while_open_aborts), &
            new_unittest("writing a table with column count exceeding Arrow's Schema field-count limit aborts", &
                test_write_column_count_overflow_aborts), &
            new_unittest("prefetching an unknown column aborts", &
                test_prefetch_unknown_column_aborts), &
            new_unittest("filter: unknown column aborts", &
                test_filter_unknown_column_aborts), &
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
            new_unittest("sorting: searching unsorted input aborts", &
                test_sorting_search_unsorted_aborts), &
            new_unittest("sorting: an over-long search target aborts", &
                test_sorting_search_target_too_long_aborts), &
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
            new_unittest("sort: unknown column aborts", &
                test_sort_unknown_column_aborts), &
            new_unittest("sort: vector column key aborts", &
                test_sort_vector_column_aborts), &
            new_unittest("sort: empty key aborts", &
                test_sort_empty_key_aborts), &
            new_unittest("sort: unrecognized direction aborts", &
                test_sort_bad_direction_aborts), &
            new_unittest("sort: two direction words abort", &
                test_sort_two_direction_words_aborts), &
            new_unittest("sort: '-' plus an explicit direction aborts", &
                test_sort_minus_and_direction_aborts), &
            new_unittest("sort: over-long key aborts", &
                test_sort_key_too_long_aborts), &
            new_unittest("sort: too many keys aborts", &
                test_sort_too_many_keys_aborts), &
            new_unittest("sort: chunked read on a sorted reader aborts", &
                test_sort_chunked_read_aborts), &
            new_unittest("sort: parquet_get_chunk_size on a sorted reader aborts", &
                test_sort_get_chunk_size_aborts), &
            new_unittest("sort: a second parquet_reader_set_sort aborts", &
                test_sort_set_sort_twice_aborts), &
            new_unittest("sort: parquet_reader_set_sort after a read aborts", &
                test_sort_set_sort_after_read_aborts), &
            new_unittest("errors: filter/sort ordering guards abort (chunked read, sorted reader)", &
                test_set_transform_ordering_aborts), &
            new_unittest("errors: reading a non-uniform plain LIST column aborts", &
                test_non_uniform_list_read_aborts), &
            new_unittest("adopt_transform onto an already-transformed reader aborts", &
                test_adopt_transform_onto_transformed_aborts), &
            new_unittest("adopt_transform after a column has been read aborts", &
                test_adopt_transform_after_read_aborts), &
            new_unittest("adopt_transform across two different files aborts", &
                test_adopt_transform_other_file_aborts), &
            new_unittest("filter: vector column aborts", &
                test_filter_vector_column_aborts), &
            new_unittest("filter: malformed rule aborts", &
                test_filter_malformed_rule_aborts), &
            new_unittest("filter: rule longer than the supported maximum aborts", &
                test_filter_rule_too_long_aborts), &
            new_unittest("filter: remap_column_names with mismatched from/to sizes aborts", &
                test_filter_remap_size_mismatch_aborts), &
            new_unittest("filter: remap_column_names to an over-long column name aborts", &
                test_filter_remap_name_too_long_aborts), &
            new_unittest("filter: a rule that grows past the cap when remapped aborts", &
                test_filter_remap_rule_too_long_aborts), &
            new_unittest("sortkey: remap_column_names with mismatched from/to sizes aborts", &
                test_sortkey_remap_size_mismatch_aborts), &
            new_unittest("sortkey: remap_column_names to an over-long column name aborts", &
                test_sortkey_remap_name_too_long_aborts), &
            new_unittest("settings: a thread-pool capacity below 1 aborts", &
                test_set_arrow_threads_zero_aborts), &
            new_unittest("settings: an unknown default compression codec aborts", &
                test_settings_bad_codec_aborts), &
            new_unittest("settings: a negative sort thread cap aborts", &
                test_settings_negative_sort_threads_aborts), &
            new_unittest("a negative sort_counting_bucket_limit aborts", &
                test_settings_negative_bucket_limit_aborts), &
            new_unittest("a negative target_row_group_bytes aborts", &
                test_settings_negative_row_group_bytes_aborts), &
            new_unittest("settings: an unknown token from the environment aborts naming the variable", &
                test_settings_env_bad_token_aborts), &
            new_unittest("settings: a non-numeric integer from the environment aborts", &
                test_settings_env_not_an_integer_aborts), &
            new_unittest("settings: two numbers in one integer variable aborts", &
                test_settings_env_two_numbers_aborts), &
            new_unittest("a build failing the frozen-transform check aborts", &
                test_weighted_contract_failure_aborts), &
            new_unittest("a negative weight aborts", test_weighted_negative_weight_aborts), &
            new_unittest("an all-zero weight vector aborts", test_weighted_all_zero_aborts), &
            new_unittest("a NaN weight aborts rather than counting as zero", test_weighted_nan_aborts), &
            new_unittest("%next on an uninitialised sampler aborts", test_weighted_uninit_aborts), &
            new_unittest("a second %init aborts", test_weighted_init_twice_aborts), &
            new_unittest("a WEIGHTED subset larger than its population aborts", test_weighted_subset_big_aborts), &
            new_unittest("a mismatched permutation array aborts", test_weighted_perm_size_aborts), &
            new_unittest("an infinite weight aborts", test_weighted_infinite_aborts), &
            new_unittest("the RACE validates its own weights, on all four of its guards", &
                test_weighted_race_weight_guards), &
            new_unittest("a population too large for an int32 item index aborts, on all three guards", &
                test_weighted_int32_population_guards) &
            ]
        p3 = [ &
            new_unittest("an environment value longer than the buffer aborts", &
                test_settings_env_too_long_aborts), &
            new_unittest("an environment value beyond a default INTEGER's range aborts", &
                test_settings_env_int32_out_of_range_aborts), &
            new_unittest("a long environment value is echoed truncated, not whole", &
                test_settings_env_long_value_preview_aborts), &
            new_unittest("the informational channel prints, and verbosity=silent suppresses it", &
                test_settings_emit_info_channel), &
            new_unittest("string column reindex with a duplicate index aborts", &
                test_strings_reindex_duplicate_index_aborts), &
            new_unittest("string column reindex_trusted of the wrong length aborts", &
                test_strings_reindex_trusted_length_mismatch_aborts), &
            new_unittest("copy_buffers with an offsets array shorter than size()+1 aborts", &
                test_strings_copy_buffers_offsets_too_short_aborts), &
            new_unittest("copy_buffers with a data array shorter than character_size() aborts", &
                test_strings_copy_buffers_data_too_short_aborts), &
            new_unittest("settings: an out-of-range environment value aborts through its own setter", &
                test_settings_env_out_of_range_aborts), &
            new_unittest("settings: an unaccepted boolean spelling aborts listing the accepted ones", &
                test_settings_env_bad_boolean_aborts), &
            new_unittest("settings: parquet_set_threads(0) aborts, though two of the three take 0", &
                test_settings_set_threads_zero_aborts), &
            new_unittest("settings: a negative prefetch thread cap aborts", &
                test_settings_negative_prefetch_threads_aborts), &
            new_unittest("settings: an unknown verbosity level aborts", &
                test_settings_bad_verbosity_aborts), &
            new_unittest("settings: an unknown message stream aborts", &
                test_settings_bad_stream_aborts), &
            new_unittest("settings: errors_only prints a warning at normal and not at errors_only", &
                test_settings_verbosity_gates_warning), &
            new_unittest("settings: silent turns %print_stat into a no-op", &
                test_settings_silent_gates_print_stat), &
            new_unittest("settings: silent turns parquet_close_reader(print_stat=) into a no-op", &
                test_settings_silent_gates_reader_print_stat), &
            new_unittest("settings: the C++ half honours the mirrored verbosity", &
                test_settings_verbosity_reaches_cpp), &
            new_unittest("settings: silencing AFTER an open does not reach that reader's C++ side", &
                test_settings_mirror_is_taken_at_open), &
            new_unittest("settings: message_stream moves a warning off stdout", &
                test_settings_message_stream_moves_warning), &
            new_unittest("settings: an abort still reports itself when everything is silenced", &
                test_settings_error_survives_silence), &
            new_unittest("read_qc: an entry longer than the supported maximum aborts", &
                test_read_qc_entry_too_long_aborts), &
            new_unittest("read_qc: remap_column_names with mismatched from/to sizes aborts", &
                test_read_qc_remap_size_mismatch_aborts), &
            new_unittest("read_qc: an entry that grows past the cap when remapped aborts", &
                test_read_qc_remap_entry_too_long_aborts), &
            new_unittest("table: a code-declared qc bound the data violates aborts", &
                test_table_qc_violation_aborts), &
            new_unittest("table: a maml extra: sort: on a slice-regime open aborts", &
                test_table_slice_maml_sort_aborts), &
            new_unittest("table: a filter naming a column the file lacks aborts", &
                test_table_filter_unknown_column_aborts), &
            new_unittest("filter: non-numeric value against a numeric column aborts", &
                test_filter_bad_numeric_value_aborts), &
            new_unittest("filter: non-numeric value against a numeric column aborts under a scoped filter", &
                test_filter_bad_numeric_value_scoped_aborts), &
            new_unittest("filter: int32 value out of range aborts", &
                test_filter_int32_value_out_of_range_aborts), &
            new_unittest("filter: non-numeric value against a float column aborts", &
                test_filter_bad_numeric_value_float_aborts), &
            new_unittest("filter: unquoted value against a string column aborts", &
                test_filter_unquoted_string_value_aborts), &
            new_unittest("filter: invalid boolean value aborts", &
                test_filter_bad_boolean_value_aborts), &
            new_unittest("filter: a double-quoted boolean value aborts", &
                test_filter_boolean_value_must_be_unquoted_aborts), &
            new_unittest("filter: ordering comparison against a boolean column aborts", &
                test_filter_bool_ordering_not_supported_aborts), &
            new_unittest("filter: is_nan against a non-floating-point column aborts", &
                test_filter_is_nan_non_float_column_aborts), &
            new_unittest("filter: a NaN literal as a comparison value aborts", &
                test_filter_nan_literal_rejected_aborts), &
            new_unittest("filter: a clause after is_nan with no combinator aborts", &
                test_filter_is_nan_missing_combinator_aborts), &
            new_unittest("filter: filtering an unsupported (binary) column type aborts", &
                test_filter_unsupported_column_type_aborts), &
            new_unittest("sort: sorting by an unsupported (binary) column type aborts", &
                test_sort_unsupported_column_type_aborts), &
            new_unittest("filter: unquoted value against a temporal column aborts", &
                test_filter_temporal_value_not_quoted_aborts), &
            new_unittest("filter: unbalanced '(' in an expression aborts", &
                test_filter_unbalanced_parens_aborts), &
            new_unittest("filter: stray ')' in an expression aborts", &
                test_filter_stray_close_paren_aborts), &
            new_unittest("filter: an empty '()' group aborts", &
                test_filter_empty_parens_aborts), &
            new_unittest("filter: a dangling 'and' aborts", &
                test_filter_dangling_and_aborts), &
            new_unittest("filter: a leading 'or' aborts", &
                test_filter_leading_or_aborts), &
            new_unittest("filter: 'not' with no operand aborts", &
                test_filter_not_without_operand_aborts), &
            new_unittest("filter: two clauses with no combinator aborts", &
                test_filter_missing_combinator_aborts), &
            new_unittest("filter: nesting past the depth limit aborts", &
                test_filter_nesting_too_deep_aborts), &
            new_unittest("filter: more expression terms than the node limit aborts", &
                test_filter_too_many_nodes_aborts), &
            new_unittest("filter: an invalid ISO-8601 temporal literal aborts", &
                test_filter_temporal_bad_iso_literal_aborts), &
            new_unittest("filter: a temporal literal finer than the column's unit aborts", &
                test_filter_temporal_literal_too_precise_aborts), &
            new_unittest("parquet_reader_set_filter after a column was read aborts", &
                test_filter_set_filter_after_read_aborts), &
            new_unittest("parquet_reader_set_filter on an already-filtered reader aborts", &
                test_filter_set_filter_twice_aborts), &
            new_unittest("filter: an unterminated quoted value aborts", &
                test_filter_unterminated_quote_aborts), &
            new_unittest("filter: an empty rule aborts", &
                test_filter_empty_rule_aborts), &
            new_unittest("filter: a group missing its ')' aborts", &
                test_filter_expected_close_paren_aborts), &
            new_unittest("filter: a ')' where a clause was expected aborts", &
                test_filter_close_paren_as_clause_aborts), &
            new_unittest("filter: an over-long column name aborts", &
                test_filter_leaf_too_long_aborts), &
            new_unittest("filter: combined %add rules past the node limit abort", &
                test_filter_too_many_nodes_across_adds_aborts), &
            new_unittest("filter: a row-group scope past the last row group aborts", &
                test_filter_scope_out_of_range_aborts), &
            new_unittest("filter: a reversed row-group scope aborts", &
                test_filter_scope_reversed_aborts), &
            new_unittest("filter: a row range past the last row aborts", &
                test_filter_row_range_out_of_range_aborts), &
            new_unittest("chunked read: a bool read of a non-bool column aborts", &
                test_chunk_read_bool_type_mismatch_aborts), &
            new_unittest("filter: a row range outside its own row groups aborts", &
                test_filter_row_range_outside_row_groups_aborts), &
            new_unittest("filter: row_group_lo=0 filters every row group without caching", &
                test_filter_all_row_groups_bounded), &
            new_unittest("sample_fraction: negative value aborts", &
                test_sample_negative_fraction_aborts), &
            new_unittest("sample_fraction: NaN value aborts", &
                test_sample_nan_fraction_aborts), &
            new_unittest("print_stat reports the sample: fraction=.../seed=... line", &
                test_print_stat_sampled_rows), &
            new_unittest("a forced sample-mask-build failure aborts via a clean error stop", &
                test_sample_mask_build_error_aborts), &
            new_unittest("a sample mask whose length disagrees with the file's row count aborts", &
                test_sample_mask_length_mismatch_aborts), &
            new_unittest("string length query on a non-string column aborts", &
                test_string_length_on_non_string_column_aborts), &
            new_unittest("qc-maml: unrecognized miss: value aborts", &
                test_qc_maml_bad_miss_value_aborts), &
            new_unittest("qc-maml: duplicate field name aborts", &
                test_qc_maml_duplicate_field_aborts), &
            new_unittest("qc-maml: field missing name aborts", &
                test_qc_maml_missing_name_aborts), &
            new_unittest("qc-maml: unknown qc: sub-key aborts", &
                test_qc_maml_unknown_subkey_aborts), &
            new_unittest("maml: a source line exceeding the length limit aborts (not silently truncated)", &
                test_maml_line_too_long_aborts), &
            new_unittest("embedded maml: an unknown fixture name aborts (after a control lookup succeeds)", &
                test_embedded_maml_unknown_name_aborts), &
            new_unittest("qc: existing Null-abort behavior is unchanged", &
                test_qc_existing_null_abort_unchanged_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation", &
                test_qc_range_violation_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation (string column)", &
                test_qc_range_violation_string_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation (float column)", &
                test_qc_range_violation_float_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on an unexpected Null", &
                test_qc_null_violation_hard_aborts), &
            new_unittest("writing columns with mismatched row counts aborts", &
                test_write_row_count_mismatch_aborts), &
            new_unittest("reading a column into a wrong-size array aborts", &
                test_read_row_count_mismatch_aborts), &
            new_unittest("reading from an unopened reader aborts", &
                test_read_before_open_aborts), &
            new_unittest("writing to an unopened writer aborts", &
                test_write_before_open_aborts), &
            new_unittest("calling parquet_get_nrows on an unopened reader aborts", &
                test_get_nrows_before_open_aborts), &
            new_unittest("closing a never-opened reader aborts", &
                test_close_reader_before_open_aborts), &
            new_unittest("closing a never-opened writer aborts", &
                test_close_writer_before_open_aborts), &
            new_unittest("closing a writer with an unwritten enabled column aborts", &
                test_close_writer_missing_write_aborts), &
            new_unittest("closing a hand-built-schema writer with an unwritten enabled column " // &
                "aborts with an unnamed-schema message", test_close_writer_missing_write_unnamed_schema_aborts), &
            new_unittest("error context reaches stdout even at verbosity=errors_only, " // &
                "message_stream=stderr", &
                test_error_context_ignores_output_settings), &
            new_unittest("the two failure classes exit with the two documented statuses", &
                test_failure_classes_exit_statuses), &
            new_unittest("closing a writer with NO column written writes them empty and warns", &
                test_close_writer_no_columns_written_warns), &
            new_unittest("explicit zero-length writes produce no empty-close warning", &
                test_close_writer_zero_length_writes_quiet), &
            new_unittest("closing a writer with a row mask but no column written still aborts", &
                test_close_writer_no_columns_with_mask_aborts), &
            new_unittest("reading an unknown column via parquet_read_column aborts", &
                test_read_unknown_column_aborts), &
            new_unittest("reading a nested struct-field path with a mid-path typo aborts", &
                test_read_nested_struct_field_not_found_aborts), &
            new_unittest("reading a nested struct-field path through a non-struct segment aborts", &
                test_read_nested_struct_path_not_a_struct_aborts), &
            new_unittest("reading a nested struct-field path that resolves to an intermediate struct aborts", &
                test_read_nested_struct_intermediate_not_leaf_aborts), &
            new_unittest("reading two nested struct-field leaves under one struct shares one physical read", &
                test_nested_struct_shares_cached_read), &
            new_unittest("opening a nonexistent file for reading aborts", &
                test_open_reader_missing_file_aborts), &
            new_unittest("parquet_open_reader(nrows=) with a filter matching zero rows aborts", &
                test_open_reader_nrows_zero_rows_aborts), &
            new_unittest("opening a writer at a bad path aborts", &
                test_open_writer_bad_path_aborts) &
            ]
        p4 = [ &
            new_unittest("writing an over-length string into a fixed-size string matrix column aborts", &
                test_write_string_matrix_exceeds_array_size_aborts), &
            new_unittest("writing an over-length string into a fixed-size string vector column (flat form) aborts", &
                test_write_string_exceeds_array_size_aborts), &
            new_unittest("protected_cols: referencing an unknown field aborts", &
                test_validate_protected_cols_unknown_name_aborts), &
            new_unittest("a capitalized Extra: block is read, so its protected_cols: is still checked", &
                test_extra_section_capitalized_aborts), &
            new_unittest("the lowercase extra: twin behaves identically (the case control)", &
                test_extra_section_lowercase_control_aborts), &
            new_unittest("a compact string write over its declared array_size is accepted, with one warning", &
                test_compact_write_exceeds_array_size_warns), &
            new_unittest("writing a Null into a protected column aborts", &
                test_write_protected_column_with_null_aborts), &
            new_unittest("a null element of a protected vector column aborts", &
                test_write_protected_vector_element_null_aborts), &
            new_unittest("protected_cols: matches a column's OUTPUT name under a col_map: rename", &
                test_protected_col_map_output_name_aborts), &
            new_unittest("protected_cols: naming the INTERNAL name of a col_map:-renamed column is rejected", &
                test_protected_col_map_internal_name_rejected), &
            new_unittest("a null element of a protected timestamp column aborts, with no is_valid in sight", &
                test_write_protected_temporal_null_aborts), &
            new_unittest("an %append_null in a protected parquet_string_column aborts", &
                test_write_protected_string_column_null_aborts), &
            new_unittest("dropping an is_valid mask after the first row group aborts", &
                test_chunk_mask_dropped_after_first_row_group_aborts), &
            new_unittest("adding an is_valid mask after an unmasked first row group aborts", &
                test_chunk_mask_added_after_first_row_group_aborts), &
            new_unittest("schema%set_protected on an unknown column aborts", &
                test_set_protected_unknown_column_aborts), &
            new_unittest("unprotecting a protected column warns without naming a MAML, and " // &
                "never warns for a column that was not protected", test_set_protected_unprotect_warns), &
            new_unittest("qc: min value that does not parse as a number aborts", &
                test_validate_qc_min_not_numeric_aborts), &
            new_unittest("qc: max value that does not parse as a number aborts", &
                test_validate_qc_max_not_numeric_aborts), &
            new_unittest("qc: min value with a fractional part on an int32 field aborts", &
                test_validate_qc_min_non_integral_for_int32_aborts), &
            new_unittest("qc: min value out of int32 range aborts", &
                test_validate_qc_min_out_of_int32_range_aborts), &
            new_unittest("qc: min value too large for int64 aborts rather than reading back garbage", &
                test_validate_qc_min_overflows_int64_aborts), &
            new_unittest("qc: min value with a reversed (</<=) operator aborts", &
                test_validate_qc_min_wrong_operator_aborts), &
            new_unittest("qc: max value with a reversed (>/>=) operator aborts", &
                test_validate_qc_max_wrong_operator_aborts), &
            new_unittest("qc: an unrecognized miss: value aborts, naming the value", &
                test_validate_qc_miss_bad_value_aborts), &
            new_unittest("qc: every legal miss: form still validates (control)", &
                test_validate_qc_miss_valid_values_accepted), &
            new_unittest("qc-maml: min value with a reversed (</<=) operator aborts", &
                test_qc_maml_min_wrong_operator_aborts), &
            new_unittest("qc-maml: max value with a reversed (>/>=) operator aborts", &
                test_qc_maml_max_wrong_operator_aborts), &
            new_unittest("schema%add_field: qc_max operator with no value aborts", &
                test_schema_add_field_qc_max_operator_without_value_aborts), &
            new_unittest("add_col_qc: reversed min operator aborts", &
                test_add_col_qc_min_reversed_operator_aborts), &
            new_unittest("add_col_qc: reversed max operator aborts", &
                test_add_col_qc_max_reversed_operator_aborts), &
            new_unittest("add_col_qc: operator with no value aborts", &
                test_add_col_qc_operator_without_value_aborts), &
            new_unittest("add_col_qc: max operator with no value aborts", &
                test_add_col_qc_max_operator_without_value_aborts), &
            new_unittest("add_col_qc: invalid miss value aborts", &
                test_add_col_qc_bad_miss_value_aborts), &
            new_unittest("add_col_qc: more than four fields aborts", &
                test_add_col_qc_too_many_fields_aborts), &
            new_unittest("add_col_qc: empty column name aborts", &
                test_add_col_qc_empty_column_name_aborts), &
            new_unittest("add_col_qc: duplicate column name aborts", &
                test_add_col_qc_duplicate_column_aborts), &
            new_unittest("add_col_qc: duplicate column name (single-quoted existing entry) aborts", &
                test_add_col_qc_duplicate_column_single_quoted_aborts), &
            new_unittest("add_col_qc: duplicate column name (double-quoted existing entry) aborts", &
                test_add_col_qc_duplicate_column_double_quoted_aborts), &
            new_unittest("set_col_qc: reversed min operator aborts (shared validation)", &
                test_set_col_qc_reversed_operator_aborts), &
            new_unittest("writing values(:) not divisible by col_size aborts", &
                test_write_values_not_divisible_by_col_size_aborts), &
            new_unittest("writing an out-of-int32-range int64 value to an int32 schema column aborts", &
                test_write_int64_to_int32_overflow_aborts), &
            new_unittest("writing a non-integral float64 value to an int32 schema column aborts", &
                test_write_float_to_int32_non_integral_aborts), &
            new_unittest("writing an out-of-int32-range float64 value to an int32 schema column aborts", &
                test_write_float_to_int32_out_of_range_aborts), &
            new_unittest("a float value both non-integral and out of int32 range reports the non-integral error", &
                test_write_float_to_int32_non_integral_and_out_of_range_aborts), &
            new_unittest("a large negative out-of-int64-range float reports the range error", &
                test_write_float_to_int64_negative_out_of_range_aborts), &
            new_unittest("writing a NaN to an int32 schema column aborts", &
                test_write_float_nan_to_int32_aborts), &
            new_unittest("writing a non-integral float64 value to an int64 schema column aborts", &
                test_write_float_to_int64_non_integral_aborts), &
            new_unittest("writing an out-of-int64-range float64 value to an int64 schema column aborts", &
                test_write_float_to_int64_out_of_range_aborts), &
            new_unittest("parquet_set_arrow_threads(0) aborts", &
                test_set_max_threads_below_one_aborts), &
            new_unittest("concurrent calls into a shared parquet_reader abort", &
                test_concurrent_calls_into_shared_reader_aborts), &
            new_unittest("concurrent calls into a shared parquet_writer abort", &
                test_concurrent_calls_into_shared_writer_aborts), &
            new_unittest("a sequential hand-off of one writer between threads succeeds", &
                test_writer_guard_sequential_handoff_succeeds), &
            new_unittest("parquet_column data_ptr with a mismatched pointer kind aborts", &
                test_columns_data_ptr_kind_mismatch_aborts), &
            new_unittest("parquet_column structural mutation on a kindless column aborts", &
                test_columns_uninitialized_append_nulls_aborts), &
            new_unittest("parquet_column get_at past the last row aborts", &
                test_columns_get_at_index_out_of_range_aborts), &
            new_unittest("parquet_column append of a different kind aborts", &
                test_columns_append_kind_mismatch_aborts), &
            new_unittest("parquet_column append of a different vector width aborts", &
                test_columns_append_width_mismatch_aborts), &
            new_unittest("columns: append_row_of with a mismatched width aborts", &
                test_columns_append_row_of_width_mismatch_aborts), &
            new_unittest("parquet_column append_row_of past the source's last row aborts", &
                test_columns_append_row_of_row_out_of_range_aborts), &
            new_unittest("parquet_column element index past the column's width aborts", &
                test_columns_element_index_out_of_range_aborts), &
            new_unittest("set_validity with a transposed element mask aborts", &
                test_columns_set_validity_elem_shape_mismatch_aborts), &
            new_unittest("set_validity with a row mask of the wrong length aborts", &
                test_columns_set_validity_row_count_mismatch_aborts), &
            new_unittest("clear_null on a temporal element aborts", &
                test_columns_clear_null_elem_temporal_aborts), &
            new_unittest("table: append(row) validates every column before writing any", &
                test_table_append_row_validates_first_aborts), &
            new_unittest("table: reserve_columns of a negative count aborts", &
                test_table_reserve_columns_negative_aborts), &
            new_unittest("table: reserve_columns on a shared table in a region aborts", &
                test_table_reserve_columns_shared_aborts), &
            new_unittest("the size queries read no column data, and a re-read is free", &
                test_size_queries_read_no_column_data), &
            new_unittest("the affinity clamp warns, on the right stream, exactly once", &
                test_affinity_clamp_warns), &
            new_unittest("verbosity=silent suppresses the affinity-clamp warning", &
                test_affinity_clamp_silent), &
            new_unittest("no clamp, no affinity warning", test_affinity_clamp_absent), &
            new_unittest("table: parquet_write_table on a shared table in a region aborts", &
                test_table_write_shared_aborts), &
            new_unittest("table: a first parquet_row_index read on a shared table names the caller", &
                test_table_row_index_shared_aborts), &
            new_unittest("table: a direction token plus descending= aborts", &
                test_table_key_direction_conflict_aborts), &
            new_unittest("table: a long conflicting key list is clipped in the message", &
                test_table_key_list_long_preview_aborts), &
            new_unittest("table: a key list naming nothing aborts", &
                test_table_key_list_empty_aborts), &
            new_unittest("table: an unrecognized direction word aborts", &
                test_table_key_list_bad_direction_aborts), &
            new_unittest("table: require_columns names every missing column", &
                test_table_require_columns_missing_aborts), &
            new_unittest("table: a long missing name is clipped in the message", &
                test_table_require_columns_long_name_aborts), &
            new_unittest("table: more missing names than the preview shows are counted", &
                test_table_require_columns_many_missing_aborts), &
            new_unittest("parquet_column paste of a different kind aborts", &
                test_columns_paste_kind_mismatch_aborts), &
            new_unittest("parquet_column paste of a different vector width aborts", &
                test_columns_paste_width_mismatch_aborts), &
            new_unittest("parquet_column paste on a string kind aborts", &
                test_columns_paste_string_kind_aborts), &
            new_unittest("parquet_column paste with from= below 1 aborts", &
                test_columns_paste_source_index_below_one_aborts), &
            new_unittest("parquet_column paste with a negative count aborts", &
                test_columns_paste_negative_count_aborts), &
            new_unittest("parquet_column paste reading past the source end aborts", &
                test_columns_paste_source_past_end_aborts), &
            new_unittest("parquet_column paste with at= below 1 aborts", &
                test_columns_paste_destination_index_below_one_aborts), &
            new_unittest("parquet_column paste past the destination end aborts", &
                test_columns_paste_destination_past_end_aborts), &
            new_unittest("parquet_column reindex with a duplicated index aborts", &
                test_columns_reindex_duplicate_index_aborts), &
            new_unittest("parquet_column clear_null on a temporal kind aborts", &
                test_columns_clear_null_temporal_aborts), &
            new_unittest("parquet_string_column reindex with a short permutation aborts", &
                test_string_column_reindex_length_mismatch_aborts), &
            new_unittest("parquet_string_column reindex with an out-of-range entry aborts", &
                test_string_column_reindex_out_of_range_aborts), &
            new_unittest("parquet_string_column delete_by_mask with a short mask aborts", &
                test_string_column_delete_by_mask_length_mismatch_aborts), &
            new_unittest("parquet_string_column append_nulls with a negative count aborts", &
                test_string_column_append_nulls_negative_aborts), &
            new_unittest("parquet_column string_column on a non-string kind aborts", &
                test_columns_string_column_wrong_kind_aborts), &
            new_unittest("parquet_column init of a reserved container kind aborts", &
                test_columns_init_container_kind_aborts), &
            new_unittest("parquet_column init with a width on a scalar kind aborts", &
                test_columns_init_width_on_scalar_kind_aborts), &
            new_unittest("parquet_column adopt of an unallocated array aborts, on every kind", &
                test_columns_adopt_not_allocated_aborts), &
            new_unittest("parquet_column set_all with a mismatched value count aborts", &
                test_columns_set_all_length_mismatch_aborts), &
            new_unittest("parquet_column get_at with a mismatched row width aborts", &
                test_columns_get_at_width_mismatch_aborts), &
            new_unittest("parquet_column delete_by_mask with a short mask aborts", &
                test_columns_delete_by_mask_length_mismatch_aborts), &
            new_unittest("parquet_column reindex with a short permutation aborts", &
                test_columns_reindex_length_mismatch_aborts), &
            new_unittest("copying a parquet_table by assignment aborts", &
                test_table_assignment_blocked_aborts), &
            new_unittest("an ordering comparison on a boolean column aborts", &
                test_filter_bool_ordering_aborts), &
            new_unittest("reading a parquet_table while an append is in flight aborts", &
                test_table_read_during_append_aborts), &
            new_unittest("appending to a parquet_table while a read is in flight aborts", &
                test_table_append_during_read_aborts), &
            new_unittest("using a never-opened parquet_table aborts", &
                test_table_not_opened_aborts), &
            new_unittest("a by-position parquet_table query past the last column aborts", &
                test_table_column_position_out_of_range_aborts), &
            new_unittest("%get_element past the last row aborts, naming get_element", &
                test_table_get_element_row_out_of_range_aborts), &
            new_unittest("%get_element on a mismatched kind aborts through the shared body", &
                test_table_get_element_kind_mismatch_aborts) &
            ]
        p5 = [ &
            new_unittest("table: %get_element into an int32 vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_i32v_aborts), &
            new_unittest("table: %get_element into an int64 vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_i64v_aborts), &
            new_unittest("table: %get_element into a real32 vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_f32v_aborts), &
            new_unittest("table: %get_element into a real64 vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_f64v_aborts), &
            new_unittest("table: %get_element into a logical vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_boolv_aborts), &
            new_unittest("table: %get_element into a date vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_datev_aborts), &
            new_unittest("table: %get_element into a time vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_timev_aborts), &
            new_unittest("table: %get_element into a timestamp vector of the wrong kind aborts", &
                test_table_get_element_kind_mismatch_tsv_aborts), &
            new_unittest("%get_element on a missing column aborts", &
                test_table_get_element_missing_column_aborts), &
            new_unittest("using a column handle after a structural change aborts", &
                test_col_handle_stale_aborts), &
            new_unittest("a column handle's own row bounds check aborts", &
                test_col_handle_row_out_of_range_aborts), &
            new_unittest("a column handle's %ref after a structural change aborts", &
                test_col_handle_ref_stale_aborts), &
            new_unittest("a column handle's %ref with a mismatched pointer kind aborts", &
                test_col_handle_ref_kind_mismatch_aborts), &
            new_unittest("a row handle given another table's column handle aborts", &
                test_row_handle_foreign_column_aborts), &
            new_unittest("using a row handle after a structural change aborts", &
                test_row_handle_stale_aborts), &
            new_unittest("%append from a row handle on the destination itself aborts", &
                test_table_append_row_self_aborts), &
            new_unittest("%append from a stale source row handle aborts", &
                test_table_append_row_stale_source_aborts), &
            new_unittest("a never-attached column handle aborts when used", &
                test_col_handle_never_attached_aborts), &
            new_unittest("parquet_table %col with a mismatched pointer kind aborts", &
                test_table_pointer_kind_mismatch_aborts), &
            new_unittest("parquet_table %col mismatch aborts on every remaining col_ptr_* kind", &
                test_table_col_ptr_kind_mismatch_aborts), &
            new_unittest("parquet_table %get mismatch aborts on every remaining get_arr_* kind", &
                test_table_get_array_kind_mismatch_more_aborts), &
            new_unittest("parquet_table read of an unknown column aborts", &
                test_table_unknown_column_aborts), &
            new_unittest("parquet_table read of an unsupported column type aborts", &
                test_table_unsupported_column_read_aborts), &
            new_unittest("parquet_table add_column with a mismatched row count aborts", &
                test_table_add_column_row_mismatch_aborts), &
            new_unittest("add_column of a parquet_column with no kind aborts", &
                test_table_add_column_kindless_aborts), &
            new_unittest("parquet_table add_column of an existing name without force= aborts", &
                test_table_add_column_duplicate_aborts), &
            new_unittest("parquet_table %set with a mismatched array length aborts", &
                test_table_set_length_mismatch_aborts), &
            new_unittest("parquet_write_table with a schema column the table lacks aborts", &
                test_table_write_missing_column_aborts), &
            new_unittest("parquet_write_table with a schema naming an unsupported column aborts", &
                test_table_write_unsupported_column_aborts), &
            new_unittest("parquet_write_table with overwrite=.false. over an existing file aborts", &
                test_table_write_no_overwrite_aborts), &
            new_unittest("a sidecar MAML for a zero-column schema-less write aborts", &
                test_table_write_schemaless_empty_maml_aborts), &
            new_unittest("parquet_write_table with a schema that was never built aborts", &
                test_table_write_unbuilt_schema_aborts), &
            new_unittest("asking for the row index after a detach aborts", &
                test_table_row_index_after_detach_aborts), &
            new_unittest("materializing the row index warns when the file had its own column of that name", &
                test_table_row_index_shadowed_warns), &
            new_unittest("evicting an in-memory column aborts", &
                test_table_evict_in_memory_aborts), &
            new_unittest("evicting a column of a detached table aborts", &
                test_table_evict_detached_aborts), &
            new_unittest("a wrong-length is_valid mask on %set aborts", &
                test_table_set_is_valid_length_aborts), &
            new_unittest("copy_metadata with an unknown key aborts", &
                test_table_copy_metadata_unknown_key_aborts), &
            new_unittest("copy_metadata on an in-memory table aborts", &
                test_table_copy_metadata_in_memory_aborts), &
            new_unittest("copy_metadata=.true. with metadata_keys= aborts", &
                test_table_copy_metadata_both_forms_aborts), &
            new_unittest("opening a table slice starting before row 1 aborts", &
                test_table_slice_below_first_row_aborts), &
            new_unittest("opening a table slice past the last row aborts", &
                test_table_slice_past_last_row_aborts), &
            new_unittest("opening an inverted table slice aborts", &
                test_table_slice_inverted_aborts), &
            new_unittest("a row handle on a row that does not exist aborts", &
                test_table_row_index_out_of_range_aborts), &
            new_unittest("get_slice selecting a row outside the table aborts", &
                test_table_get_slice_out_of_range_aborts), &
            new_unittest("parquet_slice_range with a zero step aborts", &
                test_table_slice_zero_step_aborts), &
            new_unittest("reloading an in-memory column aborts", &
                test_table_reload_in_memory_column_aborts), &
            new_unittest("reloading a column of a table with no file aborts", &
                test_table_reload_not_file_backed_aborts), &
            new_unittest("evicting a column holding local edits aborts", &
                test_table_evict_user_populated_aborts), &
            new_unittest("reloading a column holding local edits aborts", &
                test_table_reload_user_populated_aborts), &
            new_unittest("claiming a column that holds no values aborts", &
                test_table_set_user_populated_not_resident_aborts), &
            new_unittest("a row handle reading an unknown column aborts", &
                test_table_row_unknown_column_aborts), &
            new_unittest("reading through an unattached row handle aborts", &
                test_table_row_unattached_aborts), &
            new_unittest("reading a column left behind by a detach aborts", &
                test_table_detached_read_unmaterialized_aborts), &
            new_unittest("prefetching on a detached table aborts", &
                test_table_detached_prefetch_aborts), &
            new_unittest("reading a slice's stranded column after a row mutation aborts", &
                test_table_slice_mutate_then_read_aborts), &
            new_unittest("materialize_all on a detached table aborts", &
                test_table_detached_materialize_all_aborts), &
            new_unittest("reloading a column of a detached table aborts", &
                test_table_detached_reload_aborts), &
            new_unittest("row_group_bounds on a detached table aborts", &
                test_table_detached_row_group_bounds_aborts), &
            new_unittest("row_group_bounds on a sorted table aborts with the table's own message", &
                test_table_row_group_bounds_sorted_aborts), &
            new_unittest("a column stranded by a detach cannot be read afterwards", &
                test_table_mutate_unmaterialized_column_aborts), &
            new_unittest("sorting by an unsupported column aborts", &
                test_table_mutate_unsupported_column_aborts), &
            new_unittest("argsort_by on a vector column aborts naming argsort_by", &
                test_table_argsort_by_vector_column_aborts), &
            new_unittest("argsort_by with no keys aborts", &
                test_table_argsort_by_no_keys_aborts), &
            new_unittest("group_nkeys above the key count aborts", &
                test_table_argsort_by_group_nkeys_too_many_aborts), &
            new_unittest("group_nkeys of zero aborts", &
                test_table_argsort_by_group_nkeys_zero_aborts), &
            new_unittest("group_nkeys without group_offsets aborts", &
                test_table_argsort_by_group_nkeys_without_offsets_aborts), &
            new_unittest("argsort_partial with a negative n aborts", &
                test_table_argsort_partial_negative_n_aborts), &
            new_unittest("top_n with a negative n aborts, naming top_n", &
                test_table_top_n_negative_n_aborts), &
            new_unittest("top_n on an unknown column aborts, naming top_n", &
                test_table_top_n_unknown_column_aborts), &
            new_unittest("reading a column stranded by top_n aborts", &
                test_table_top_n_detached_read_aborts), &
            new_unittest("column gather with an out-of-range index aborts", &
                test_column_gather_out_of_range_aborts), &
            new_unittest("string column gather with an out-of-range index aborts", &
                test_string_column_gather_out_of_range_aborts), &
            new_unittest("filter_rows with a wrong-length mask aborts", &
                test_table_filter_rows_mask_length_aborts), &
            new_unittest("delete_rows with an out-of-range index aborts", &
                test_table_delete_rows_out_of_range_aborts), &
            new_unittest("truncate to a negative row count aborts", &
                test_table_truncate_negative_aborts), &
            new_unittest("append_null_rows with a negative count aborts", &
                test_table_append_null_rows_negative_aborts), &
            new_unittest("sort_by with no key aborts", &
                test_table_sort_by_no_keys_aborts), &
            new_unittest("sort_by with a short descending= list aborts", &
                test_table_sort_by_flag_count_mismatch_aborts), &
            new_unittest("sort_by with a short nulls_first= list aborts", &
                test_table_sort_by_nulls_first_count_mismatch_aborts), &
            new_unittest("sort_by on a missing column aborts", &
                test_table_sort_by_unknown_column_aborts), &
            new_unittest("sort_by on a vector column aborts", &
                test_table_sort_by_vector_column_aborts), &
            new_unittest("append with an extra column in the source aborts", &
                test_table_append_unknown_column_aborts), &
            new_unittest("append with a kind mismatch aborts", &
                test_table_append_kind_mismatch_aborts), &
            new_unittest("append with a vector width mismatch aborts", &
                test_table_append_width_mismatch_aborts), &
            new_unittest("append with a unit mismatch aborts", &
                test_table_append_unit_mismatch_aborts), &
            new_unittest("append(row) with no column in common aborts", &
                test_table_append_row_no_common_column_aborts), &
            new_unittest("set_element past the last row aborts", &
                test_table_set_element_row_out_of_range_aborts), &
            new_unittest("set_element with the wrong value kind aborts", &
                test_table_set_element_kind_mismatch_aborts), &
            new_unittest("set_null on a row that does not exist aborts", &
                test_table_set_null_row_out_of_range_aborts), &
            new_unittest("set_null with a wrong-length row mask aborts", &
                test_table_set_null_mask_wrong_length_aborts), &
            new_unittest("set_null with a transposed element mask aborts", &
                test_table_set_null_mask_wrong_shape_aborts), &
            new_unittest("rename_column onto an existing name aborts", &
                test_table_rename_duplicate_name_aborts), &
            new_unittest("rename_column to a blank name aborts", &
                test_table_rename_blank_name_aborts), &
            new_unittest("copy_column on a non-numeric column aborts", &
                test_table_copy_unsupported_kind_aborts), &
            new_unittest("copy_column onto an existing name aborts", &
                test_table_copy_duplicate_name_aborts), &
            new_unittest("copy_column to a blank name aborts", &
                test_table_copy_blank_name_aborts), &
            new_unittest("copy_column on a value that would lose information aborts", &
                test_table_copy_lossy_value_aborts), &
            new_unittest("cast of a non-numeric column aborts", &
                test_table_cast_non_numeric_aborts), &
            new_unittest("cast between a scalar and a vector kind aborts", &
                test_table_cast_rank_change_aborts), &
            new_unittest("cast narrowing an out-of-range integer aborts", &
                test_table_cast_int_overflow_aborts) &
            ]
        p6 = [ &
            new_unittest("cast of a fractional value to an integer kind aborts", &
                test_table_cast_fractional_aborts), &
            new_unittest("cast overflowing float32 aborts", &
                test_table_cast_float_overflow_aborts), &
            new_unittest("cast(exact=.true.) on a value that loses precision aborts", &
                test_table_cast_exact_precision_aborts), &
            new_unittest("cast(exact=) refuses an int32 real32 cannot hold exactly", &
                test_table_cast_exact_i32_to_f32_aborts), &
            new_unittest("bind_predefined with mismatched array sizes aborts", &
                test_table_bind_predefined_size_mismatch_aborts), &
            new_unittest("bind_predefined with a units array of the wrong length aborts", &
                test_table_bind_predefined_units_size_mismatch_aborts), &
            new_unittest("bind_predefined warns, and still binds, on a narrowing declaration", &
                test_table_bind_predefined_lossy_warning), &
            new_unittest("bind_predefined of a computed column over an existing name aborts", &
                test_table_bind_predefined_computed_name_taken_aborts), &
            new_unittest("a slice write whose is_valid is longer than the selection aborts", &
                test_table_set_rows_valid_length_mismatch_aborts), &
            new_unittest("a whole-column write with a transposed element mask aborts", &
                test_table_set_elem_valid_shape_mismatch_aborts), &
            new_unittest("a vector slice write with a mis-shaped element mask aborts", &
                test_table_set_rows_elem_valid_shape_mismatch_aborts), &
            new_unittest("a slice write with more values than selected rows aborts", &
                test_table_set_slice_size_mismatch_aborts), &
            new_unittest("cast(exact=) refuses an int64 real32 cannot hold exactly", &
                test_table_cast_exact_i64_to_f32_aborts), &
            new_unittest("cast(exact=) refuses an int64 real64 cannot hold exactly", &
                test_table_cast_exact_i64_to_f64_aborts), &
            new_unittest("cast of a fractional real32 to an integer kind aborts", &
                test_table_cast_f32_fractional_aborts), &
            new_unittest("cast of an unsupported column aborts", &
                test_table_cast_unsupported_column_aborts), &
            new_unittest("clone into another table type aborts", &
                test_table_clone_type_mismatch_aborts), &
            new_unittest("a row handle reading an unsupported column aborts", &
                test_table_row_unsupported_column_aborts), &
            new_unittest("a row handle reading a non-string column into a string aborts", &
                test_table_row_string_kind_mismatch_aborts), &
            new_unittest("a row handle reading into an incompatible kind aborts", &
                test_table_row_kind_mismatch_aborts), &
            new_unittest("get_slice into an incompatible kind aborts", &
                test_table_get_slice_kind_mismatch_aborts), &
            new_unittest("extra: remap: naming a nonexistent file column aborts", &
                test_table_remap_unknown_file_column_aborts), &
            new_unittest("extra: remap: repeating an internal name aborts", &
                test_table_remap_duplicate_internal_aborts), &
            new_unittest("get into an incompatible array kind aborts", &
                test_table_get_array_kind_mismatch_aborts), &
            new_unittest("add_column(force=.false.) on a duplicate name aborts", &
                test_table_add_column_duplicate_force_false_aborts), &
            new_unittest("row_group_bounds on an in-memory table aborts", &
                test_table_row_group_bounds_in_memory_aborts), &
            new_unittest("binding a predefined column the file lacks aborts", &
                test_table_bind_missing_column_aborts), &
            new_unittest("binding a predefined column of the wrong width aborts", &
                test_table_bind_width_mismatch_aborts), &
            new_unittest("binding a string column as a numeric kind aborts", &
                test_table_bind_kind_refused_aborts), &
            new_unittest("dropping a predefined column without force= aborts", &
                test_table_drop_predefined_aborts), &
            new_unittest("a generated accessor's out-of-range row index aborts", &
                test_codegen_row_index_out_of_range_aborts), &
            new_unittest("a generated accessor's out-of-range row range aborts", &
                test_codegen_range_out_of_range_aborts), &
            new_unittest("opening a generated table on a file missing a column aborts", &
                test_codegen_missing_file_column_aborts), &
            new_unittest("init(exact=.true.) refuses a value the declared kind cannot hold", &
                test_codegen_init_exact_refuses_aborts), &
            new_unittest("the same file without exact= warns and opens", &
                test_codegen_init_exact_control), &
            new_unittest("a generated table cannot reopen its own output while it holds a computed column", &
                test_codegen_computed_roundtrip_aborts), &
            new_unittest("reindex_trusted still checks the permutation LENGTH", &
                test_reindex_trusted_length_aborts), &
            new_unittest("pf_permute checks the length even under assume_valid", &
                test_permute_assume_valid_length_aborts), &
            new_unittest("set into an incompatible column kind aborts", &
                test_table_set_kind_mismatch_aborts), &
            new_unittest("get_file_metadata on an in-memory table aborts", &
                test_table_get_file_metadata_in_memory_aborts), &
            new_unittest("get_file_metadata with an unknown key aborts", &
                test_table_get_file_metadata_missing_key_aborts), &
            new_unittest("kind of an unknown column aborts", &
                test_table_kind_unknown_column_aborts), &
            new_unittest("table: prefetching an unknown column aborts", &
                test_table_prefetch_unknown_column_aborts), &
            new_unittest("prefetching an unsupported column aborts", &
                test_table_prefetch_unsupported_column_aborts), &
            new_unittest("parquet_string_column indexing out of range aborts", &
                test_string_column_index_out_of_range_aborts), &
            new_unittest("parquet_string_column view_all with a mismatched-size array aborts", &
                test_string_column_view_all_size_mismatch_aborts), &
            new_unittest("parquet_string_column get on a null aborts", &
                test_string_column_get_null_aborts), &
            new_unittest("parquet_string_column to_character on a null aborts", &
                test_string_column_to_character_null_aborts), &
            new_unittest("parquet_string unassociated handle aborts", &
                test_string_handle_unassociated_aborts), &
            new_unittest("parquet_string stale handle index aborts", &
                test_string_handle_stale_index_aborts), &
            new_unittest("parquet_string set_null on an unassociated handle aborts", &
                test_string_set_null_unassociated_aborts), &
            new_unittest("parquet_string set_null on a stale handle aborts", &
                test_string_set_null_stale_index_aborts), &
            new_unittest("parquet_string_column slice with first > last aborts", &
                test_string_slice_invalid_range_aborts), &
            new_unittest("parquet_string_column view_slice with an out-of-range last aborts", &
                test_string_view_slice_invalid_range_aborts), &
            new_unittest("parquet_string_column view_slice with a mismatched-size array aborts", &
                test_string_view_slice_size_mismatch_aborts), &
            new_unittest("parquet_string_column build_from with a self-aliasing handle aborts", &
                test_string_build_from_self_alias_aborts), &
            new_unittest("parquet_string_column build_from with an unassociated handle aborts", &
                test_string_build_from_unassociated_aborts), &
            new_unittest("parquet_string_column build_from with a stale handle aborts", &
                test_string_build_from_stale_index_aborts), &
            new_unittest("parquet_string_column build_from(character) with a short mask aborts", &
                test_string_build_from_character_mask_length_aborts), &
            new_unittest("parquet_string_column append_values with a short mask aborts", &
                test_string_append_values_mask_length_aborts), &
            new_unittest("parquet_string_column append_buffers with un-rebased offsets aborts", &
                test_string_column_append_buffers_offset_not_zero_aborts), &
            new_unittest("parquet_string_column append_buffers with un-rebased int32 offsets aborts", &
                test_string_column_append_buffers_offset_not_zero_int32_aborts), &
            new_unittest("compact string write into a vector (col_size>1) schema column aborts", &
                test_compact_string_write_requires_scalar_column_aborts), &
            new_unittest("compact string chunk write into a vector (col_size>1) schema column aborts", &
                test_compact_string_write_chunk_requires_scalar_column_aborts), &
            new_unittest("a STRING_VIEW column reads into a compact parquet_string_column", &
                test_string_view_compact_read), &
            new_unittest("parquet_date set with an invalid month aborts", &
                test_temporal_date_set_invalid_month_aborts), &
            new_unittest("parquet_date set with an invalid day aborts", &
                test_temporal_date_set_invalid_day_aborts), &
            new_unittest("parquet_date set beyond the representable range aborts", &
                test_temporal_date_set_out_of_range_aborts), &
            new_unittest("parquet_date set_mjd beyond the representable range aborts", &
                test_temporal_date_set_mjd_out_of_range_aborts), &
            new_unittest("parquet_date parse failure without success= aborts", &
                test_temporal_date_parse_invalid_aborts), &
            new_unittest("parquet_date get on a null element aborts", &
                test_temporal_date_null_get_aborts), &
            new_unittest("parquet_date comparison with a null element aborts", &
                test_temporal_date_null_comparison_aborts), &
            new_unittest("parquet_time set with an invalid hour aborts", &
                test_temporal_time_set_invalid_hour_aborts), &
            new_unittest("parquet_time set with an invalid nanosecond aborts", &
                test_temporal_time_set_invalid_nanosecond_aborts), &
            new_unittest("parquet_time set_raw outside a day aborts", &
                test_temporal_time_set_raw_out_of_range_aborts), &
            new_unittest("parquet_time parse failure without success= aborts", &
                test_temporal_time_parse_invalid_aborts), &
            new_unittest("parquet_time accessor on a null element aborts", &
                test_temporal_time_null_get_aborts), &
            new_unittest("parquet_timestamp set with an invalid day aborts", &
                test_temporal_ts_set_invalid_day_aborts), &
            new_unittest("parquet_timestamp parse failure without success= aborts", &
                test_temporal_ts_parse_invalid_aborts), &
            new_unittest("parquet_timestamp to_unix on a null element aborts", &
                test_temporal_ts_null_to_unix_aborts), &
            new_unittest("parquet_timestamp comparison with a null element aborts", &
                test_temporal_ts_null_comparison_aborts), &
            new_unittest("parquet_timestamp to_unix precision loss aborts by default", &
                test_temporal_ts_to_unix_precision_loss_aborts), &
            new_unittest("parquet_timestamp to_unix int64 overflow aborts", &
                test_temporal_ts_to_unix_overflow_aborts), &
            new_unittest("parquet_timestamp set_mjd beyond the representable range aborts", &
                test_temporal_ts_set_mjd_out_of_range_aborts), &
            new_unittest("parquet_timestamp set_raw with an invalid nanosecond part aborts", &
                test_temporal_ts_set_raw_invalid_nanoseconds_aborts), &
            new_unittest("an invalid time-unit selector aborts", &
                test_temporal_invalid_time_unit_aborts), &
            new_unittest("parquet_timestamp get with a year beyond int32 aborts", &
                test_temporal_ts_get_year_overflow_aborts), &
            new_unittest("writing a time value finer than its declared unit aborts", &
                test_temporal_write_time_precision_loss_aborts), &
            new_unittest("parquet_get_column_time_info on a date column aborts", &
                test_temporal_time_info_on_date_column_aborts), &
            new_unittest("qc: miss: declared on a temporal column suppresses the null WARNING", &
                test_temporal_qc_miss_declared_null_no_warning), &
            new_unittest("writing a Null into a protected timestamp column aborts", &
                test_temporal_protected_col_null_aborts), &
            new_unittest("reading a date column via the plain int32 reader aborts", &
                test_temporal_read_date_via_int32_aborts), &
            new_unittest("reading an int32 column via the date reader aborts", &
                test_temporal_read_int32_via_date_aborts), &
            new_unittest("parquet_date operator(==) with a null operand aborts", test_temporal_date_eq_null_aborts), &
            new_unittest("parquet_time get on a null element aborts", test_temporal_time_get_null_aborts), &
            new_unittest("parquet_time minute on a null element aborts", test_temporal_time_minute_null_aborts), &
            new_unittest("parquet_time second on a null element aborts", test_temporal_time_second_null_aborts), &
            new_unittest("parquet_time nanosecond on a null element aborts", &
                test_temporal_time_nanosecond_null_aborts), &
            new_unittest("parquet_time operator(==) with a null operand aborts", test_temporal_time_eq_null_aborts), &
            new_unittest("parquet_time operator(<) with a null operand aborts", test_temporal_time_lt_null_aborts), &
            new_unittest("parquet_date year on a null element aborts", test_temporal_date_year_null_aborts), &
            new_unittest("parquet_date month on a null element aborts", test_temporal_date_month_null_aborts), &
            new_unittest("parquet_date day on a null element aborts", test_temporal_date_day_null_aborts), &
            new_unittest("parquet_date to_mjd on a null element aborts", test_temporal_date_to_mjd_null_aborts), &
            new_unittest("parquet_date to_string on a null element aborts", test_temporal_date_to_string_null_aborts), &
            new_unittest("parquet_time to_string on a null element aborts", test_temporal_time_to_string_null_aborts), &
            new_unittest("parquet_timestamp set with an invalid month aborts", &
                test_temporal_ts_set_invalid_month_aborts), &
            new_unittest("parquet_timestamp set with an invalid nanosecond aborts", &
                test_temporal_ts_set_invalid_nanosecond_aborts), &
            new_unittest("parquet_timestamp get on a null element aborts", test_temporal_ts_get_null_aborts) &
            ]
        p7 = [ &
            new_unittest("parquet_timestamp get_date beyond parquet_date's range aborts", &
                test_temporal_ts_get_date_range_exceeded_aborts), &
            new_unittest("parquet_timestamp to_unix negative-branch overflow aborts", &
                test_temporal_ts_to_unix_overflow_negative_aborts), &
            new_unittest("parquet_timestamp to_mjd on a null element aborts", test_temporal_ts_to_mjd_null_aborts), &
            new_unittest("parquet_timestamp to_string on a null element aborts", &
                test_temporal_ts_to_string_null_aborts), &
            new_unittest("parquet_timestamp operator(<) with a null operand aborts", test_temporal_ts_lt_null_aborts), &
            new_unittest("parquet_date operator(-) diff with a null operand aborts", test_temporal_date_diff_null_aborts), &
            new_unittest("parquet_date day-offset operator(+) with a null operand aborts", &
                test_temporal_date_offset_null_aborts), &
            new_unittest("parquet_date day-offset arithmetic out of the +-5.8M year range aborts", &
                test_temporal_date_offset_out_of_range_aborts), &
            new_unittest("parquet_date day-offset int64-overflow guard (positive branch) aborts", &
                test_temporal_date_offset_int64_overflow_positive_aborts), &
            new_unittest("parquet_date day-offset int64-overflow guard (negative branch) aborts", &
                test_temporal_date_offset_int64_overflow_negative_aborts), &
            new_unittest("parquet_date operator(-) with an INT64_MIN offset aborts", &
                test_temporal_date_sub_int64_min_aborts), &
            new_unittest("parquet_time operator(-) diff with a null operand aborts", test_temporal_time_diff_null_aborts), &
            new_unittest("parquet_time ns-offset operator(+) with a null operand aborts", &
                test_temporal_time_offset_null_aborts), &
            new_unittest("parquet_time operator(+) offset exceeding 24h aborts", &
                test_temporal_time_offset_magnitude_add_aborts), &
            new_unittest("parquet_time operator(-) offset exceeding 24h aborts", &
                test_temporal_time_offset_magnitude_sub_aborts), &
            new_unittest("parquet_timestamp operator(-) diff with a null operand aborts", &
                test_temporal_ts_diff_ns_null_aborts), &
            new_unittest("parquet_timestamp operator(-) diff beyond ~292.3 years aborts", &
                test_temporal_ts_diff_ns_overflow_aborts), &
            new_unittest("parquet_timestamp diff_seconds with a null operand aborts", &
                test_temporal_ts_diff_seconds_null_aborts), &
            new_unittest("parquet_timestamp ns-offset operator(+) with a null operand aborts", &
                test_temporal_ts_offset_null_aborts), &
            new_unittest("parquet_timestamp ns-offset int64 nanosecond-addition overflow aborts", &
                test_temporal_ts_offset_ns_overflow_aborts), &
            new_unittest("parquet_timestamp ns-offset int64 seconds-carry overflow (positive) aborts", &
                test_temporal_ts_offset_seconds_overflow_positive_aborts), &
            new_unittest("parquet_timestamp ns-offset int64 seconds-carry overflow (negative) aborts", &
                test_temporal_ts_offset_seconds_overflow_negative_aborts), &
            new_unittest("parquet_timestamp operator(-) with an INT64_MIN offset aborts", &
                test_temporal_ts_sub_int64_min_aborts), &
            new_unittest("writing an undeclared temporal column aborts", &
                test_temporal_write_column_not_defined_aborts), &
            new_unittest("writing a temporal vector column with the wrong col_size aborts", &
                test_temporal_write_array_size_mismatch_aborts), &
            new_unittest("writing a temporal column of the wrong data_type aborts", &
                test_temporal_write_type_mismatch_aborts), &
            new_unittest("chunk-writing an undeclared temporal column aborts", &
                test_temporal_chunk_column_not_defined_aborts), &
            new_unittest("chunk-writing a temporal vector column with the wrong col_size aborts", &
                test_temporal_chunk_array_size_mismatch_aborts), &
            new_unittest("chunk-writing a temporal column of the wrong data_type aborts", &
                test_temporal_chunk_type_mismatch_aborts), &
            new_unittest("parquet_write_row_mask after the writer has started aborts", &
                test_mask_row_mask_after_write_started_aborts), &
            new_unittest("a whole-column write whose row count doesn't match the mask aborts", &
                test_mask_row_mask_shape_mismatch_aborts), &
            new_unittest("parquet_write_row_mask with a zero-length mask aborts", &
                test_mask_row_mask_zero_length_aborts), &
            new_unittest("calling parquet_write_row_mask twice for the same writer aborts", &
                test_mask_row_mask_called_twice_aborts), &
            new_unittest("parquet_write_chunk_row_mask after parquet_write_row_mask aborts", &
                test_mask_chunk_row_mask_after_row_mask_aborts), &
            new_unittest("parquet_write_row_mask after the writer has started row groups aborts", &
                test_mask_row_mask_after_chunk_row_mask_aborts), &
            new_unittest("parquet_write_chunk_row_mask after a whole-column write aborts", &
                test_mask_chunk_row_mask_after_whole_column_aborts), &
            new_unittest("parquet_write_chunk_row_mask not used for every row group aborts", &
                test_mask_chunk_row_mask_not_used_every_group_aborts), &
            new_unittest("parquet_write_chunk_row_mask introduced after the row group's first chunk write aborts", &
                test_mask_chunk_row_mask_introduced_late_aborts), &
            new_unittest("calling parquet_write_chunk_row_mask twice for the same row group aborts", &
                test_mask_chunk_row_mask_called_twice_aborts), &
            new_unittest("parquet_write_chunk_row_mask with the wrong mask size aborts", &
                test_mask_chunk_row_mask_size_mismatch_aborts), &
            new_unittest("a row group claiming more mask positions than parquet_write_row_mask provided aborts", &
                test_mask_row_mask_window_exhausted_aborts), &
            new_unittest("closing a writer with an unconsumed parquet_write_row_mask tail aborts", &
                test_mask_row_mask_not_fully_consumed_aborts), &
            new_unittest("parquet_write_chunk_row_mask with no row group open aborts", &
                test_mask_chunk_row_mask_no_row_group_open_aborts), &
            new_unittest("parquet_write_chunk_row_mask after an earlier row group declined it aborts", &
                test_mask_chunk_row_mask_scheme_declined_aborts), &
            new_unittest("finishing a row group with no column ever written for it aborts", &
                test_mask_row_group_no_writes_at_all_aborts), &
            new_unittest("parquet_get_version with an invalid mode aborts", &
                test_get_version_invalid_mode_aborts), &
            new_unittest("parquet_get_version with the removed mode='arrow' aborts", &
                test_get_version_arrow_mode_removed_aborts), &
            new_unittest("parquet_get_arrow_version with an invalid mode aborts", &
                test_get_arrow_version_invalid_mode_aborts), &
            new_unittest("parquet_column_exists with an unrecognized types= token aborts", &
                test_column_exists_bad_type_token_aborts), &
            new_unittest("parquet_column_exists validates types= before checking the column exists", &
                test_column_exists_bad_type_token_missing_column_aborts), &
            new_unittest("parquet_column_exists with a blank types= filter aborts", &
                test_column_exists_empty_type_filter_aborts) &
            ]
        testsuite = [p1, p2, p3, p4, p5, p6, p7]
    end subroutine collect_tests_parquet_errors

    !
    ! ---- parquet_columns foundation abort paths ----
    !
    subroutine test_table_assignment_blocked_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_assignment_blocked", expect_abort=.true., &
            failure_message="copying a parquet_table by intrinsic assignment was expected to abort", &
            required_stderr="parquet_table: assignment is not supported")
    end subroutine test_table_assignment_blocked_aborts

    !> `>`/`<` on a boolean are rejected when the filter is parsed, which is also why the row-group
    !! screen's own boolean ordering-reject arm is unreachable -- see test/test_filter_screen.f90.
    subroutine test_filter_bool_ordering_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "filter_bool_ordering", &
            expect_abort=.true., &
            failure_message="an ordering comparison on a boolean column was expected to abort", &
            required_stderr="ordering comparisons ('>', '>=', '<', '<=') are not supported for boolean")
    end subroutine test_filter_bool_ordering_aborts

    !> The append/read contract's two aborts, made deterministic by the in-flight debug hook.
    !!
    !! Both scenarios do a successful call FIRST, with the hook clear, and only then set it -- that
    !! is the negative control, and without it either test would pass just as happily against a
    !! guard that fired on every call. Neither uses threads: the guard reads a counter and does not
    !! care which thread set it, which is exactly what makes the hook enough (feature_risks.md
    !! Risk-6).
    subroutine test_table_read_during_append_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_read_during_append", &
            expect_abort=.true., &
            failure_message="reading a table while an append was in flight was expected to abort", &
            required_stderr="another thread is appending to this table right now")
    end subroutine test_table_read_during_append_aborts

    subroutine test_table_append_during_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_during_read", &
            expect_abort=.true., &
            failure_message="appending to a table while a read was in flight was expected to abort", &
            required_stderr="another thread is reading this table right now")
    end subroutine test_table_append_during_read_aborts

    subroutine test_table_not_opened_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_not_opened", expect_abort=.true., &
            failure_message="querying a never-opened parquet_table was expected to abort", &
            required_stderr="parquet_table: nrows: table has not been opened")
    end subroutine test_table_not_opened_aborts

    subroutine test_table_column_position_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message names both the offending position and the table's width, because an
        ! out-of-range position is almost always a loop bound gone stale against %ncols().
        call check_scenario_exit_status_and_stderr(error, "table_column_position_out_of_range", &
            expect_abort=.true., &
            failure_message="a by-position query past the last column was expected to abort", &
            required_stderr="parquet_table: kind: column position 2 is outside this table's 1..1 columns")
    end subroutine test_table_column_position_out_of_range_aborts

    subroutine test_table_get_element_row_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message must name `get_element`, not whatever internal body served it. A refactor
        ! that routes this accessor through a column handle changes it to "column handle: get:"
        ! -- which happened once and no test noticed, which is why this one exists.
        call check_scenario_exit_status_and_stderr(error, "table_get_element_row_out_of_range", &
            expect_abort=.true., &
            failure_message="%get_element past the last row was expected to abort", &
            required_stderr="parquet_table: get_element: row index 3 is outside this table's 1..2 rows")
    end subroutine test_table_get_element_row_out_of_range_aborts

    subroutine test_table_get_element_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch", &
            expect_abort=.true., &
            failure_message="%get_element on a mismatched kind was expected to abort", &
            required_stderr="parquet_table: get_element: column kind is PK_INT32, not PK_FLOAT64")
    end subroutine test_table_get_element_kind_mismatch_aborts

    !> %get_element abort path, once per VECTOR kind: see the
    !> scenario_table_get_element_kind_mismatch_* group in test/error_scenarios.f90 for why the
    !> scalar scenario above says nothing about any of the eight vector fetch bodies.
    subroutine test_table_get_element_kind_mismatch_i32v_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_i32v", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as an int32 vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_i32v_aborts

    subroutine test_table_get_element_kind_mismatch_i64v_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_i64v", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as an int64 vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_i64v_aborts

    subroutine test_table_get_element_kind_mismatch_f32v_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_f32v", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a real32 vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_f32v_aborts

    subroutine test_table_get_element_kind_mismatch_f64v_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_f64v", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a real64 vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_f64v_aborts

    subroutine test_table_get_element_kind_mismatch_boolv_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_boolv", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a logical vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_boolv_aborts

    subroutine test_table_get_element_kind_mismatch_datev_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_datev", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a date vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_datev_aborts

    subroutine test_table_get_element_kind_mismatch_timev_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_timev", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a time vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_timev_aborts

    subroutine test_table_get_element_kind_mismatch_tsv_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_get_element_kind_mismatch_tsv", &
            expect_abort=.true., &
            failure_message="reading a mismatched column as a timestamp vector was expected to abort", &
            required_stderr="column kind")
    end subroutine test_table_get_element_kind_mismatch_tsv_aborts

    subroutine test_table_get_element_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_element_missing_column", &
            expect_abort=.true., &
            failure_message="%get_element on a missing column was expected to abort", &
            required_stderr="parquet_table: get_element: no column of this name")
    end subroutine test_table_get_element_missing_column_aborts

    subroutine test_col_handle_stale_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message must name the REMEDY, because the cause is usually several statements away
        ! from where it is noticed.
        call check_scenario_exit_status_and_stderr(error, "col_handle_stale_after_mutation", &
            expect_abort=.true., &
            failure_message="using a stale column handle was expected to abort", &
            required_stderr="re-fetch it with %column(...)")
    end subroutine test_col_handle_stale_aborts

    subroutine test_col_handle_row_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "col_handle_row_out_of_range", &
            expect_abort=.true., &
            failure_message="a column handle read past the last row was expected to abort", &
            required_stderr="column handle: get: row index 3 is outside this table's 1..2 rows")
    end subroutine test_col_handle_row_out_of_range_aborts

    subroutine test_col_handle_ref_stale_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! %ref hands back a raw pointer, so a stale one would alias reallocated storage and no
        ! later check could notice. This is the only test of col_ref_*'s own col_resolve call.
        call check_scenario_exit_status_and_stderr(error, "col_handle_ref_after_mutation", &
            expect_abort=.true., &
            failure_message="%ref through a stale column handle was expected to abort", &
            required_stderr="column handle: ref: this table has changed structurally")
    end subroutine test_col_handle_ref_stale_aborts

    subroutine test_col_handle_ref_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The same wording %col produces, from the same helper -- two spellings of one operation.
        call check_scenario_exit_status_and_stderr(error, "col_handle_ref_kind_mismatch", &
            expect_abort=.true., &
            failure_message="%ref with a mismatched pointer kind was expected to abort", &
            required_stderr="ref: pointer kind does not match the stored kind")
    end subroutine test_col_handle_ref_kind_mismatch_aborts

    subroutine test_row_handle_foreign_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The only guard in the handle design whose absence gives a WRONG ANSWER rather than an
        ! error: the foreign handle is valid, just not this table's.
        call check_scenario_exit_status_and_stderr(error, "row_handle_foreign_column", &
            expect_abort=.true., &
            failure_message="a row handle given another table's column handle was expected to abort", &
            required_stderr="belongs to a different table than this row handle")
    end subroutine test_row_handle_foreign_column_aborts

    subroutine test_row_handle_stale_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The behaviour that did not exist before the row handle gained a stamp: it used to read
        ! whatever now sat at its index, which is a wrong answer rather than an error.
        call check_scenario_exit_status_and_stderr(error, "row_handle_stale_after_sort", &
            expect_abort=.true., &
            failure_message="using a stale row handle was expected to abort", &
            required_stderr="re-fetch it with %row(...)")
    end subroutine test_row_handle_stale_aborts

    subroutine test_table_append_row_self_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! Its OWN message, not the generic staleness one -- "re-fetch it" cannot work when the
        ! append is what invalidates the handle.
        call check_scenario_exit_status_and_stderr(error, "table_append_row_self", &
            expect_abort=.true., &
            failure_message="%append from a handle on the destination itself was expected to abort", &
            required_stderr="a table cannot be grown from a handle on itself")
    end subroutine test_table_append_row_self_aborts

    subroutine test_table_append_row_stale_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The other half of the split: here "re-fetch it" IS the right advice, so the generic
        ! message is what must appear.
        call check_scenario_exit_status_and_stderr(error, "table_append_row_stale_source", &
            expect_abort=.true., &
            failure_message="%append from a stale source row handle was expected to abort", &
            required_stderr="re-fetch it with %row(...)")
    end subroutine test_table_append_row_stale_source_aborts

    subroutine test_col_handle_never_attached_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "col_handle_never_attached", &
            expect_abort=.true., &
            failure_message="a never-attached column handle was expected to abort when used", &
            required_stderr="this handle is not attached to a table")
    end subroutine test_col_handle_never_attached_aborts

    subroutine test_table_pointer_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_pointer_kind_mismatch", expect_abort=.true., &
            failure_message="aliasing an int32 table column through an int64 pointer was expected to abort", &
            required_stderr="parquet_table: col: pointer kind does not match the stored kind (PK_INT32)")
    end subroutine test_table_pointer_kind_mismatch_aborts

    subroutine test_table_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_unknown_column", expect_abort=.true., &
            failure_message="reading a column that does not exist was expected to abort", &
            required_stderr="parquet_table: get: no column of this name")
    end subroutine test_table_unknown_column_aborts

    subroutine test_table_unsupported_column_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_unsupported_column_read", expect_abort=.true., &
            failure_message="reading a column of an unsupported physical type was expected to abort", &
            required_stderr="this column's type is not supported by parquet_table")
    end subroutine test_table_unsupported_column_read_aborts

    subroutine test_table_add_column_row_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_add_column_row_mismatch", expect_abort=.true., &
            failure_message="adding a column of a different length was expected to abort", &
            required_stderr="parquet_table: add_column: every column must have the same number of rows")
    end subroutine test_table_add_column_row_mismatch_aborts

    !> The one failure mode the array forms of `%add_column` cannot have. The scenario adds a real
    !! column first, so a guard that rejected every column would fail it rather than pass it.
    subroutine test_table_add_column_kindless_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_add_column_kindless", expect_abort=.true., &
            failure_message="adding a parquet_column that was never given a kind was expected to abort", &
            required_stderr="add_column: this parquet_column has no kind yet")
    end subroutine test_table_add_column_kindless_aborts

    subroutine test_table_add_column_duplicate_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_add_column_duplicate", expect_abort=.true., &
            failure_message="adding a second column of the same name without force= was expected to abort", &
            required_stderr="a column of this name already exists; pass force=.true. to replace it")
    end subroutine test_table_add_column_duplicate_aborts

    subroutine test_table_set_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_length_mismatch", expect_abort=.true., &
            failure_message="setting a column from a shorter array was expected to abort", &
            required_stderr="this replaces values, never the row set")
    end subroutine test_table_set_length_mismatch_aborts

    subroutine test_table_write_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_missing_column", expect_abort=.true., &
            failure_message="writing with a schema naming an absent column was expected to abort", &
            required_stderr="the schema declares a column the table does not have")
    end subroutine test_table_write_missing_column_aborts

    subroutine test_table_write_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_unsupported_column", expect_abort=.true., &
            failure_message="writing a schema-named unsupported column was expected to abort", &
            required_stderr="the schema declares a column that holds no values")
    end subroutine test_table_write_unsupported_column_aborts

    subroutine test_table_write_no_overwrite_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_no_overwrite", expect_abort=.true., &
            failure_message="a table write with overwrite=.false. over an existing file was expected to abort", &
            required_stderr="file already exists and overwrite=.false.")
    end subroutine test_table_write_no_overwrite_aborts

    subroutine test_table_write_schemaless_empty_maml_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_schemaless_empty_maml", expect_abort=.true., &
            failure_message="a sidecar MAML for a zero-column schema-less write was expected to abort", &
            required_stderr="cannot describe zero columns")
    end subroutine test_table_write_schemaless_empty_maml_aborts

    subroutine test_table_row_index_after_detach_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_index_after_detach", expect_abort=.true., &
            failure_message="the row index after a detach was expected to abort", &
            required_stderr="materialize it BEFORE the mutation that detaches")
    end subroutine test_table_row_index_after_detach_aborts

    !> The automatic row index warns, on first use, when the file carried its own column of that
    !! name -- and stays silent when it did not.
    !!
    !! The open-time warning is not enough on its own: it fires whether or not the program ever
    !! asks for the name, so a program that scrolled past it holds row numbers where it may have
    !! meant the file's data. Nothing downstream can report that -- the values ARE valid row
    !! numbers -- so this warning is the only thing standing between the collision and a silently
    !! wrong answer.
    !!
    !! The second half is the negative control, and it is what makes the first half mean anything:
    !! a warning emitted for every table would satisfy the first assertion while telling a reader
    !! nothing. Both halves run in one scenario process, over two files, so the two observations
    !! are made under identical conditions.
    subroutine test_table_row_index_shadowed_warns(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned, saw_clean, saw_shadowed

        call run_error_scenario("table_row_index_shadowed_warning", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a shadowed row-index column must warn, never abort")
        if (allocated(error)) return

        ! Both reads really happened -- otherwise a warning count proves nothing.
        call scenario_capture_contains(out_file, err_file, "shadowed row index n=3 first=1", saw_shadowed)
        call check(error, saw_shadowed, &
            "the automatic row index must still be produced for the shadowed file, and hold row numbers")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "clean row index n=3 first=1", saw_clean)
        call check(error, saw_clean, "the control read must have happened too")
        if (allocated(error)) return

        ! The whole tail, file included: the open-time warning names the same file, so a probe on
        ! the filename alone could not tell the two apart.
        call scenario_capture_contains(out_file, err_file, &
            "extra: remap: (file 'test_run/es_rowindex_shadowed.parquet')", warned)
        call check(error, warned, &
            "materializing the row index on a table whose file had its own column of that name " // &
            "must say so, name the file, and say how to reach the file's column")
        if (allocated(error)) return

        ! The control: the same warning must NOT be there for the file that has no such column.
        call scenario_capture_contains(out_file, err_file, &
            "extra: remap: (file 'test_run/es_rowindex_clean.parquet')", warned)
        call check(error, .not. warned, &
            "a table whose file has no column of that name must not warn -- a warning on every " // &
            "table would tell a reader nothing")
    end subroutine test_table_row_index_shadowed_warns

    subroutine test_table_evict_in_memory_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_evict_in_memory", expect_abort=.true., &
            failure_message="evicting an in-memory column was expected to abort", &
            required_stderr="its values are the only copy there is")
    end subroutine test_table_evict_in_memory_aborts

    subroutine test_table_evict_detached_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_evict_detached", expect_abort=.true., &
            failure_message="evicting a column of a detached table was expected to abort", &
            required_stderr="an evicted column could never be read back")
    end subroutine test_table_evict_detached_aborts

    subroutine test_table_set_is_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_is_valid_length", expect_abort=.true., &
            failure_message="a wrong-length is_valid mask was expected to abort", &
            required_stderr="is_valid has 2 entries but the table has 3 rows")
    end subroutine test_table_set_is_valid_length_aborts

    subroutine test_table_copy_metadata_unknown_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_metadata_unknown_key", expect_abort=.true., &
            failure_message="carrying a metadata key the source lacks was expected to abort", &
            required_stderr="which this table's source file does not have")
    end subroutine test_table_copy_metadata_unknown_key_aborts

    subroutine test_table_copy_metadata_in_memory_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_metadata_in_memory", expect_abort=.true., &
            failure_message="copying source metadata for an in-memory table was expected to abort", &
            required_stderr="has no source metadata to copy")
    end subroutine test_table_copy_metadata_in_memory_aborts

    subroutine test_table_copy_metadata_both_forms_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_metadata_both_forms", expect_abort=.true., &
            failure_message="copy_metadata= with metadata_keys= was expected to abort", &
            required_stderr="copy_metadata=.true. and metadata_keys= cannot both be given")
    end subroutine test_table_copy_metadata_both_forms_aborts

    subroutine test_table_write_unbuilt_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_unbuilt_schema", expect_abort=.true., &
            failure_message="writing with a schema that was never built was expected to abort", &
            required_stderr="this schema has not been built")
    end subroutine test_table_write_unbuilt_schema_aborts

    subroutine test_table_slice_below_first_row_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_slice_below_first_row", expect_abort=.true., &
            failure_message="a slice starting below row 1 was expected to abort", &
            required_stderr="row slice [0, 2] is not inside this file's 1..3 rows")
    end subroutine test_table_slice_below_first_row_aborts

    subroutine test_table_slice_past_last_row_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_slice_past_last_row", expect_abort=.true., &
            failure_message="a slice past the last row was expected to abort", &
            required_stderr="row slice [2, 99] is not inside this file's 1..3 rows")
    end subroutine test_table_slice_past_last_row_aborts

    subroutine test_table_slice_inverted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_slice_inverted", expect_abort=.true., &
            failure_message="an inverted slice was expected to abort", &
            required_stderr="row slice [3, 1] is not inside this file's 1..3 rows")
    end subroutine test_table_slice_inverted_aborts

    subroutine test_table_row_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_index_out_of_range", expect_abort=.true., &
            failure_message="a row handle on a nonexistent row was expected to abort", &
            required_stderr="row: row index 9 is outside this table's 1..3 rows")
    end subroutine test_table_row_index_out_of_range_aborts

    subroutine test_table_get_slice_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_slice_out_of_range", expect_abort=.true., &
            failure_message="a slice selecting a row past the end was expected to abort", &
            required_stderr="slice selects row 7, outside the table's 1..3 rows")
    end subroutine test_table_get_slice_out_of_range_aborts

    subroutine test_table_slice_zero_step_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_slice_zero_step", expect_abort=.true., &
            failure_message="a zero-step slice was expected to abort", &
            required_stderr="parquet_slice_range: step must not be zero")
    end subroutine test_table_slice_zero_step_aborts

    subroutine test_table_reload_in_memory_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_reload_in_memory_column", expect_abort=.true., &
            failure_message="reloading an in-memory column was expected to abort", &
            required_stderr="this column was not read from a file, so there is nothing to reload it from")
    end subroutine test_table_reload_in_memory_column_aborts

    subroutine test_table_reload_not_file_backed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_reload_not_file_backed", expect_abort=.true., &
            failure_message="reloading a column of a fileless table was expected to abort", &
            required_stderr="this column was not read from a file, so there is nothing to reload it from")
    end subroutine test_table_reload_not_file_backed_aborts

    subroutine test_table_evict_user_populated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_evict_user_populated", expect_abort=.true., &
            failure_message="evicting a column holding local edits was expected to abort", &
            required_stderr="this column holds values written into the table")
    end subroutine test_table_evict_user_populated_aborts

    subroutine test_table_reload_user_populated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_reload_user_populated", expect_abort=.true., &
            failure_message="reloading a column holding local edits was expected to abort", &
            required_stderr="reloading would replace them with the file's own")
    end subroutine test_table_reload_user_populated_aborts

    subroutine test_table_set_user_populated_not_resident_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_user_populated_not_resident", &
            expect_abort=.true., &
            failure_message="claiming a column that holds no values was expected to abort", &
            required_stderr="this column holds no values to claim")
    end subroutine test_table_set_user_populated_not_resident_aborts

    subroutine test_table_row_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_unknown_column", expect_abort=.true., &
            failure_message="a row handle reading an unknown column was expected to abort", &
            required_stderr="row get: no column of this name")
    end subroutine test_table_row_unknown_column_aborts

    subroutine test_table_row_unattached_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_unattached", expect_abort=.true., &
            failure_message="reading through an unattached row handle was expected to abort", &
            required_stderr="this row handle is not attached to a table")
    end subroutine test_table_row_unattached_aborts

    subroutine test_table_row_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_unsupported_column", expect_abort=.true., &
            failure_message="a row handle reading an unsupported column was expected to abort", &
            required_stderr="this column's type is not supported")
    end subroutine test_table_row_unsupported_column_aborts

    subroutine test_table_row_string_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_string_kind_mismatch", expect_abort=.true., &
            failure_message="reading a float64 row into a character variable was expected to abort", &
            required_stderr="row get: column kind is")
    end subroutine test_table_row_string_kind_mismatch_aborts

    subroutine test_table_row_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_kind_mismatch", expect_abort=.true., &
            failure_message="a row read into an incompatible kind was expected to abort", &
            required_stderr="cannot be copied into this variable")
    end subroutine test_table_row_kind_mismatch_aborts

    subroutine test_table_get_slice_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_slice_kind_mismatch", expect_abort=.true., &
            failure_message="a sliced read into an incompatible kind was expected to abort", &
            required_stderr="cannot be copied into this array")
    end subroutine test_table_get_slice_kind_mismatch_aborts

    subroutine test_table_remap_unknown_file_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_remap_unknown_file_column", expect_abort=.true., &
            failure_message="a remap naming a column the file does not have was expected to abort", &
            required_stderr="does not exist in this file")
    end subroutine test_table_remap_unknown_file_column_aborts

    subroutine test_table_remap_duplicate_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_remap_duplicate_internal", expect_abort=.true., &
            failure_message="a remap declaring one internal name twice was expected to abort", &
            required_stderr="is declared more than once")
    end subroutine test_table_remap_duplicate_internal_aborts

    subroutine test_table_get_array_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_array_kind_mismatch", expect_abort=.true., &
            failure_message="a %get into an incompatible array kind was expected to abort", &
            required_stderr="cannot be copied into this array")
    end subroutine test_table_get_array_kind_mismatch_aborts

    !> test_table_pointer_kind_mismatch_aborts above only reaches col_ptr_i64's own mismatch
    !! branch. Every other col_ptr_* specific (one per PK_* kind) has the identical guard on
    !! its own source line, so each needs its own subprocess abort to cover it -- driven here
    !! from one loop over the 15 scenarios in
    !! scenario_table_col_ptr_kind_mismatch_* (error_scenarios.f90).
    subroutine test_table_col_ptr_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        type(error_type), allocatable :: e2
        character(len=36), parameter :: scenarios(15) = [character(len=36) :: &
            "table_col_ptr_kind_mismatch_i32", "table_col_ptr_kind_mismatch_f32", &
            "table_col_ptr_kind_mismatch_f64", "table_col_ptr_kind_mismatch_bool", &
            "table_col_ptr_kind_mismatch_date", "table_col_ptr_kind_mismatch_time", &
            "table_col_ptr_kind_mismatch_ts", "table_col_ptr_kind_mismatch_i32v", &
            "table_col_ptr_kind_mismatch_i64v", "table_col_ptr_kind_mismatch_f32v", &
            "table_col_ptr_kind_mismatch_f64v", "table_col_ptr_kind_mismatch_boolv", &
            "table_col_ptr_kind_mismatch_datev", "table_col_ptr_kind_mismatch_timev", &
            "table_col_ptr_kind_mismatch_tsv"]
        integer :: k
        !
        do k = 1, size(scenarios)
            call check_scenario_exit_status_and_stderr(e2, trim(scenarios(k)), expect_abort=.true., &
                failure_message="a %col pointer of a mismatched kind was expected to abort: " // &
                    trim(scenarios(k)), &
                required_stderr="col: pointer kind does not match the stored kind")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
    end subroutine test_table_col_ptr_kind_mismatch_aborts

    !> test_table_get_array_kind_mismatch_aborts above only reaches get_arr_i32's own default
    !! (mismatch) branch. Every other get_arr_* specific -- including the separate default
    !! branch each of get_arr_i64v/get_arr_f64v has below their own widening case -- needs its
    !! own subprocess abort, driven here from one loop over the 15 scenarios in
    !! scenario_table_get_array_kind_mismatch_* (error_scenarios.f90).
    subroutine test_table_get_array_kind_mismatch_more_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        type(error_type), allocatable :: e2
        character(len=36), parameter :: scenarios(15) = [character(len=36) :: &
            "table_get_array_kind_mismatch_i64", "table_get_array_kind_mismatch_f32", &
            "table_get_array_kind_mismatch_f64", "table_get_array_kind_mismatch_bool", &
            "table_get_array_kind_mismatch_date", "table_get_array_kind_mismatch_time", &
            "table_get_array_kind_mismatch_ts", "table_get_array_kind_mismatch_i32v", &
            "table_get_array_kind_mismatch_i64v", "table_get_array_kind_mismatch_f32v", &
            "table_get_array_kind_mismatch_f64v", "table_get_array_kind_mismatch_boolv", &
            "table_get_array_kind_mismatch_datev", "table_get_array_kind_mismatch_timev", &
            "table_get_array_kind_mismatch_tsv"]
        integer :: k
        !
        do k = 1, size(scenarios)
            call check_scenario_exit_status_and_stderr(e2, trim(scenarios(k)), expect_abort=.true., &
                failure_message="a %get into a mismatched array kind was expected to abort: " // &
                    trim(scenarios(k)), &
                required_stderr="cannot be copied into this array")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
    end subroutine test_table_get_array_kind_mismatch_more_aborts

    subroutine test_table_add_column_duplicate_force_false_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_add_column_duplicate_force_false", &
            expect_abort=.true., &
            failure_message="add_column(force=.false.) on a duplicate name was expected to abort", &
            required_stderr="pass force=.true. to replace it")
    end subroutine test_table_add_column_duplicate_force_false_aborts

    subroutine test_table_row_group_bounds_in_memory_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_group_bounds_in_memory", expect_abort=.true., &
            failure_message="row_group_bounds on an in-memory table was expected to abort", &
            required_stderr="this table was not opened from a file")
    end subroutine test_table_row_group_bounds_in_memory_aborts

    subroutine test_table_bind_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_missing_column", expect_abort=.true., &
            failure_message="binding a predefined column the file lacks was expected to abort", &
            required_stderr="but this file has no such column")
    end subroutine test_table_bind_missing_column_aborts

    subroutine test_table_bind_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_width_mismatch", expect_abort=.true., &
            failure_message="binding a predefined column of the wrong width was expected to abort", &
            required_stderr="but the file holds 1 value(s) per row")
    end subroutine test_table_bind_width_mismatch_aborts

    subroutine test_table_bind_kind_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_kind_refused", expect_abort=.true., &
            failure_message="binding a string column as float64 was expected to abort", &
            required_stderr="only the numeric kinds convert into one another")
    end subroutine test_table_bind_kind_refused_aborts

    subroutine test_table_drop_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_drop_predefined", expect_abort=.true., &
            failure_message="dropping a predefined column without force= was expected to abort", &
            required_stderr="this is a predefined column")
    end subroutine test_table_drop_predefined_aborts

    subroutine test_codegen_row_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_row_index_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range row index on a generated accessor was expected to abort", &
            required_stderr="row index out of range for column 'ra'")
    end subroutine test_codegen_row_index_out_of_range_aborts

    subroutine test_codegen_range_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_range_out_of_range", expect_abort=.true., &
            failure_message="an out-of-range row range on a generated accessor was expected to abort", &
            required_stderr="row range out of range for column 'ra'")
    end subroutine test_codegen_range_out_of_range_aborts

    !> `%init(exact=.true.)` must refuse a value the declared kind cannot represent.
    !!
    !! Nothing else exercises `exact=` through a generated `%init` at all -- the argument is
    !! forwarded to `%cast`, which is disqualified from its deferred path by `exact=` precisely so
    !! it has the values in hand to check.
    subroutine test_codegen_init_exact_refuses_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_init_exact_refuses", &
            expect_abort=.true., &
            failure_message="init(exact=.true.) over a lossy value was expected to abort", &
            required_stderr="cannot be represented exactly as PK_FLOAT32_VEC")
    end subroutine test_codegen_init_exact_refuses_aborts

    !> The negative control for the test above: same file, same declaration, no `exact=`. Without
    !! this, that test would pass against an `%init` that refused the file for any reason at all.
    subroutine test_codegen_init_exact_control(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_init_exact_control", &
            expect_abort=.false., &
            failure_message="the same file without exact= was expected to warn and open", &
            required_stderr="values may lose precision or range")
    end subroutine test_codegen_init_exact_control

    !> A generated table writes its `source: computed` column like any other, so `%init` on the
    !! file it just produced finds a column the schema says will not be there, and aborts.
    !! Documented on doc/pages/utilities/generated-tables.md under "Writing one out".
    !!
    !! **If that refusal is ever lifted**, this does not simply get deleted: it becomes an
    !! in-process test asserting the round trip SUCCEEDS and that the computed column's values
    !! survived, and the page's "Writing one out" section changes with it.
    subroutine test_codegen_computed_roundtrip_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_computed_roundtrip", &
            expect_abort=.true., &
            failure_message="reopening a generated table's own output was expected to abort", &
            required_stderr="but the table already has a column of that name")
    end subroutine test_codegen_computed_roundtrip_aborts

    subroutine test_codegen_missing_file_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "codegen_missing_file_column", expect_abort=.true., &
            failure_message="opening a generated table on a file missing a declared column was expected to abort", &
            required_stderr="but this file has no such column")
    end subroutine test_codegen_missing_file_column_aborts

    subroutine test_reindex_trusted_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "reindex_trusted_length_mismatch", expect_abort=.true., &
            failure_message="reindex_trusted with a wrong-length permutation was expected to abort", &
            required_stderr="reindex_trusted: permutation length does not match")
    end subroutine test_reindex_trusted_length_aborts

    subroutine test_permute_assume_valid_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "permute_assume_valid_short_perm", expect_abort=.true., &
            failure_message="pf_permute(assume_valid=.true.) with a short permutation was expected to abort", &
            required_stderr="elements but the values have")
    end subroutine test_permute_assume_valid_length_aborts

    subroutine test_table_set_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_kind_mismatch", expect_abort=.true., &
            failure_message="setting an int32 column from a float64 array was expected to abort", &
            required_stderr="column kind is")
    end subroutine test_table_set_kind_mismatch_aborts

    subroutine test_table_get_file_metadata_in_memory_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_file_metadata_in_memory", expect_abort=.true., &
            failure_message="get_file_metadata on an in-memory table was expected to abort", &
            required_stderr="this table was not opened from a file")
    end subroutine test_table_get_file_metadata_in_memory_aborts

    subroutine test_table_get_file_metadata_missing_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_get_file_metadata_missing_key", expect_abort=.true., &
            failure_message="get_file_metadata with an unknown key was expected to abort", &
            required_stderr="no metadata key")
    end subroutine test_table_get_file_metadata_missing_key_aborts

    subroutine test_table_kind_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_kind_unknown_column", expect_abort=.true., &
            failure_message="%kind on an unknown column without found= was expected to abort", &
            required_stderr="no column of this name")
    end subroutine test_table_kind_unknown_column_aborts

    subroutine test_table_prefetch_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_prefetch_unknown_column", expect_abort=.true., &
            failure_message="prefetching an unknown column without found= was expected to abort", &
            required_stderr="no column of this name")
    end subroutine test_table_prefetch_unknown_column_aborts

    subroutine test_table_prefetch_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_prefetch_unsupported_column", expect_abort=.true., &
            failure_message="prefetching an unsupported column was expected to abort", &
            required_stderr="this column's type is not supported")
    end subroutine test_table_prefetch_unsupported_column_aborts

    subroutine test_columns_data_ptr_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_data_ptr_kind_mismatch", expect_abort=.true., &
            failure_message="aliasing an int32 column through an int64 pointer was expected to abort", &
            required_stderr="parquet_columns: data_ptr: column kind is PK_INT32")
    end subroutine test_columns_data_ptr_kind_mismatch_aborts

    subroutine test_columns_uninitialized_append_nulls_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_uninitialized_append_nulls", expect_abort=.true., &
            failure_message="appending null rows to a column with no kind was expected to abort", &
            required_stderr="parquet_columns: append_nulls: column has no kind assigned")
    end subroutine test_columns_uninitialized_append_nulls_aborts

    subroutine test_columns_get_at_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_get_at_index_out_of_range", expect_abort=.true., &
            failure_message="reading past the last row was expected to abort", &
            required_stderr="parquet_columns: get_at: row index out of range")
    end subroutine test_columns_get_at_index_out_of_range_aborts

    subroutine test_columns_append_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_append_kind_mismatch", expect_abort=.true., &
            failure_message="appending a column of a different kind was expected to abort", &
            required_stderr="parquet_columns: append: column kinds differ")
    end subroutine test_columns_append_kind_mismatch_aborts

    subroutine test_columns_append_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_append_width_mismatch", expect_abort=.true., &
            failure_message="appending a vector column of a different width was expected to abort", &
            required_stderr="parquet_columns: append: column widths differ")
    end subroutine test_columns_append_width_mismatch_aborts

    subroutine test_columns_append_row_of_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_append_row_of_width_mismatch", &
            expect_abort=.true., &
            failure_message="appending one row of a different-width column was expected to abort", &
            required_stderr="parquet_columns: append_row_of: column widths differ")
    end subroutine test_columns_append_row_of_width_mismatch_aborts
    !
    !> The source row index is the one `append_row_of` argument the kind and width checks cannot
    !! vet. The scenario appends an in-range row first, so a guard that refused every index would
    !! fail it rather than pass it.
    subroutine test_columns_append_row_of_row_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_append_row_of_row_out_of_range", &
            expect_abort=.true., &
            failure_message="appending a row past the source's last row was expected to abort", &
            required_stderr="parquet_columns: append_row_of: source row index out of range")
    end subroutine test_columns_append_row_of_row_out_of_range_aborts
    !
    !> The element axis has its own guard and its own message; see the scenario's own comment for
    !! why sharing the row one would point the reader at the wrong axis. The scenario reads a valid
    !! element first, so a guard that refused every element index would fail rather than pass.
    subroutine test_columns_element_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_element_index_out_of_range", &
            expect_abort=.true., &
            failure_message="asking for an element past the column's width was expected to abort", &
            required_stderr="parquet_columns: get_elem: element index out of range")
    end subroutine test_columns_element_index_out_of_range_aborts
    !
    !> A rank-2 validity mask must match (width, nrows) on BOTH extents, so a transposed square-ish
    !! mask is rejected rather than silently applied the wrong way round. The scenario applies a
    !! correctly shaped mask first as its negative control.
    subroutine test_columns_set_validity_elem_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_set_validity_elem_shape_mismatch", &
            expect_abort=.true., &
            failure_message="a transposed element validity mask was expected to abort", &
            required_stderr="parquet_columns: set_validity: mask shape does not match the column")
    end subroutine test_columns_set_validity_elem_shape_mismatch_aborts
    !
    !> A rank-1 validity mask must have exactly nrows entries, and the message names both counts.
    !! The scenario applies a correctly sized mask first as its negative control.
    subroutine test_columns_set_validity_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_set_validity_row_count_mismatch", &
            expect_abort=.true., &
            failure_message="a row validity mask of the wrong length was expected to abort", &
            required_stderr="parquet_columns: set_validity: mask has 5 entries but the column has 3 rows")
    end subroutine test_columns_set_validity_row_count_mismatch_aborts
    !
    !> A temporal element becomes valid only by having a value written to it, so clear_null refuses
    !! rather than reporting an element valid while it still holds no value. The scenario clears a
    !! BITMAP column's element null first, which is the negative control: the refusal must be about
    !! the kind, not about the operation.
    subroutine test_columns_clear_null_elem_temporal_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_clear_null_elem_temporal", &
            expect_abort=.true., &
            failure_message="clearing a temporal element's null was expected to abort", &
            required_stderr="parquet_columns: clear_null: a temporal element becomes valid by writing a value to it")
    end subroutine test_columns_clear_null_elem_temporal_aborts

    !> Asserts the ORDER of a row append, not merely that it fails.
    !>
    !> The required message is the table layer's own compatibility error. A worker that appended
    !> as it validated would abort from `parquet_columns` instead ("append_row_of: column kinds
    !> differ") -- same exit status, different message, and one column already one row longer than
    !> its neighbours. The message is the only thing that tells the two apart from outside the
    !> process, which is why this asserts it rather than just the abort.
    subroutine test_table_append_row_validates_first_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_row_validates_first", &
            expect_abort=.true., &
            failure_message="appending a row with an incompatible column was expected to abort", &
            required_stderr="convert it first (%cast)")
    end subroutine test_table_append_row_validates_first_aborts

    subroutine test_table_reserve_columns_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_reserve_columns_negative", &
            expect_abort=.true., &
            failure_message="reserving a negative column count was expected to abort", &
            required_stderr="reserve_columns: cannot reserve -5 columns")
    end subroutine test_table_reserve_columns_negative_aborts

    !> `doc/pages/operating/performance.md`'s two memory claims: the size queries read no column
    !> data, and a column already read is served from the reader's cache.
    !>
    !> Out of process only because the observable is a C++-side counter the scenario compares
    !> against itself and `error stop`s on; expected exit is 0, so a nonzero exit IS the failure.
    !> No OpenMP is involved, so this needs no skip guard.
    subroutine test_size_queries_read_no_column_data(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "size_queries_read_no_column_data", &
            expect_abort=.false., &
            failure_message="size queries must read no column data, and a cached re-read must be free")
    end subroutine test_size_queries_read_no_column_data
    !
    !> `doc/pages/operating/performance.md`'s affinity-clamp warning: that it fires at all, that its
    !> text is the one the page quotes, and that it fires **once per process**.
    !>
    !> **The once-per-process half is what `check_scenario_streams` buys here.** The scenario
    !> provokes the clamp twice, sending the first to stderr and the second to stdout; a single
    !> claimed warning therefore lands on stderr and is ABSENT from stdout, and that helper asserts
    !> both directions. A warning that fired per operation would put the second line on stdout and
    !> fail the absence half. No line counting is involved, and the assertion additionally pins that
    !> `message_stream` reached this message at all.
    !>
    !> **The asserted text includes "although 97 were requested", deliberately.** That clause used to
    !> report `omp_get_max_threads()` rather than what the caller asked for, so on a machine whose
    !> thread count happens to equal the request the two are indistinguishable. 97 is a request no
    !> machine's ICV will match by accident.
    !>
    !> **Skipped without OpenMP**, where the clamp in `resolve_thread_count` is compiled out
    !> entirely: the scenario then sorts serially, says nothing, and the assertions below would hold
    !> for a reason that has nothing to do with the clamp -- a vacuous pass, which is worse than a
    !> failure because nothing prompts anyone to look.
    subroutine test_affinity_clamp_warns(error)
        type(error_type), allocatable, intent(out) :: error
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: resolve_thread_count's affinity clamp is inside " // &
            "#ifdef _OPENMP, so the scenario sorts serially and emits nothing")
        return
#endif
        call check_scenario_streams(error, "sort_affinity_clamp_warns", &
            "sorting is limited to 2 thread(s) because this process's CPU affinity allows no " // &
            "more, although 97 were requested", &
            "stderr", "the affinity clamp must warn once, on the stream that was set when it bit")
    end subroutine test_affinity_clamp_warns
    !
    !> Negative control 1 for the warning: `parquet_set_verbosity("silent")` suppresses it.
    !>
    !> The page says so, and without this the positive test above passes against a warning that
    !> ignores verbosity entirely. The forbidden text is the message's own opening, so a warning
    !> emitted on either stream fails.
    subroutine test_affinity_clamp_silent(error)
        type(error_type), allocatable, intent(out) :: error
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the clamp never bites, so silencing it " // &
            "would be asserted against a configuration that emits nothing anyway")
        return
#endif
        call check_scenario_exit_status_and_no_output(error, "sort_affinity_clamp_silent", &
            expect_abort=.false., &
            failure_message="verbosity=silent must suppress the affinity-clamp warning", &
            forbidden_text="is limited to")
    end subroutine test_affinity_clamp_silent
    !
    !> Negative control 2, and the one that stops the warning being unconditional: with the clamp
    !> NOT biting, the identical sort says nothing.
    !>
    !> The two scenarios differ in exactly one call -- `parquet_debug_set_affinity_procs(2)` against
    !> `(0)` -- so a warning that fired on every threaded sort passes the positive test and fails
    !> this one.
    subroutine test_affinity_clamp_absent(error)
        type(error_type), allocatable, intent(out) :: error
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the positive case this controls for cannot run either")
        return
#endif
        call check_scenario_exit_status_and_no_output(error, "sort_affinity_clamp_absent", &
            expect_abort=.false., &
            failure_message="an unclamped sort must not emit the affinity warning", &
            forbidden_text="is limited to")
    end subroutine test_affinity_clamp_absent
    !
    subroutine test_table_reserve_columns_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! **Preconditions, declared rather than assumed.** The scenario opens a table OUTSIDE a
        ! parallel region and reserves columns on it INSIDE one, which the shared-table guard can
        ! only notice when there is a real region to be inside. Without OpenMP the reserve simply
        ! succeeds and the scenario exits 0 -- a build that cannot run the test, not a guard that
        ! failed to fire, and the two must not look alike. `tools/run_error_scenarios.sh` says the
        ! same thing for the same five scenarios in its `concurrency_scenarios` comment.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the shared-table guard has no parallel region to " // &
            "fire in, so the scenario reserves the columns successfully and exits 0")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, &
            "table_reserve_columns_shared_in_parallel", expect_abort=.true., &
            failure_message="reserving columns on a shared table in a region was expected to abort", &
            required_stderr="reserve_columns")
    end subroutine test_table_reserve_columns_shared_aborts

    !> `parquet_write_table` on a table that may be shared aborts, and on a thread-private one
    !! does not.
    !!
    !! **Both halves are asserted, and the second is the one that earns the test.**
    !! `table_check_not_shared` keys on whether THIS thread opened the table inside the current
    !! region, not on `omp_in_parallel()`. A guard rewritten to key on the latter would abort here
    !! too, so an abort assertion alone cannot tell a correct guard from one that refuses every
    !! write made anywhere inside a region -- which would break the documented per-thread slice
    !! pattern that `doc/pages/tables/table-open.md` teaches. The scenario therefore writes a
    !! thread-private table first and prints a marker; this test requires that marker as well as
    !! the abort.
    subroutine test_table_write_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: seen
        ! Same precondition as the sibling above: without OpenMP there is no region for the guard
        ! to notice, both writes simply succeed, and the scenario exits 0. That is a build which
        ! cannot run the test, not a guard that failed to fire.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the shared-table guard has no parallel region to " // &
            "fire in, so the scenario writes both tables successfully and exits 0")
        return
#endif
        ! Driven through run_error_scenario rather than check_scenario_exit_status_and_stderr so
        ! that ONE scenario process yields both observations: the abort, and the marker proving
        ! the private write preceded it.
        call run_error_scenario("table_write_shared_in_parallel", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "writing a shared table from inside a parallel region was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "this table was not opened by this thread inside the parallel region", seen)
        call check(error, seen, &
            "the abort must name why the table may be shared, not merely that something failed")
        if (allocated(error)) return
        ! The negative control: the private write must have got through first.
        call scenario_capture_contains(out_file, err_file, &
            "private write inside the region succeeded", seen)
        call check(error, seen, &
            "the guard aborted the shared write but also blocked the thread-private one, so it " // &
            "is keying on the region rather than on ownership")
    end subroutine test_table_write_shared_aborts

    !> The reserved row-index name is materialized on FIRST USE, which adds a column -- so asking
    !! for it on a shared table inside a region is refused. What this test is really about is the
    !! MESSAGE: the refusal used to come out of `table_new_slot` naming `add_column`, a procedure
    !! the caller never invoked and cannot find in their own source, and the advice that followed
    !! was the generic one about structural mutation rather than the specific one that fixes it.
    !!
    !! Two further things are asserted because neither follows from the abort alone. The message
    !! must name the CALLER (`get`), which is the whole point of threading `proc` through. And the
    !! guard must run BEFORE the reader is touched: on a filtered or sorted table the values come
    !! from `parquet_get_physical_row_indices`, so a guard placed after it let two threads into
    !! the shared reader first and the C++ concurrency guard reported a reader collision instead.
    !! The marker from before the region is the negative control -- it proves the refusal is about
    !! sharing, not about the name being unavailable in this build.
    subroutine test_table_row_index_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: seen
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with no region the row index is simply materialized " // &
            "on first use and the scenario exits 0, which is a build that cannot run the test")
        return
#endif
        call run_error_scenario("table_row_index_shared_in_parallel", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "a first parquet_row_index read on a shared table in a region was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "get: 'parquet_row_index'", seen)
        call check(error, seen, &
            "the abort must name the procedure the caller invoked and the column, not add_column")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "ADDS a column", seen)
        call check(error, seen, &
            "the abort must say WHY a read is refused -- that materializing this name adds a column")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "row index materialized before the region", seen)
        call check(error, seen, &
            "the same request outside the region must have succeeded, or the refusal is about " // &
            "the name rather than about the table being shared")
        if (allocated(error)) return
        ! The control that earns the test: a table this thread opened INSIDE the region is
        ! thread-private, and materializing its row index must still be allowed. A guard keying on
        ! omp_in_parallel() instead of on ownership would abort here too and satisfy every
        ! assertion above -- feature_risks.md Risk-134.
        call scenario_capture_contains(out_file, err_file, &
            "thread-private row index inside the region succeeded", seen)
        call check(error, seen, &
            "the guard refused the shared table but also the thread-private one, so it is keying " // &
            "on the region rather than on ownership")
    end subroutine test_table_row_index_shared_aborts

    !> The message must name `descending=`, since that is what the caller has to remove -- an
    !! abort saying only "conflict" leaves them guessing which half to drop.
    subroutine test_table_key_direction_conflict_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_key_direction_conflict", &
            expect_abort=.true., &
            failure_message="a direction token plus descending= was expected to abort", &
            required_stderr="descending= cannot be given as well")
    end subroutine test_table_key_direction_conflict_aborts

    !> The assertion is on the clip marker, not on the message text: a preview that quoted the
    !! whole key list would still contain every other word of this message, so only the trailing
    !! "...'" distinguishes a clipped preview from an unbounded one.
    subroutine test_table_key_list_long_preview_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_key_list_long_preview", &
            expect_abort=.true., &
            failure_message="a long conflicting key list was expected to abort", &
            required_stderr="...' already says which way to sort")
    end subroutine test_table_key_list_long_preview_aborts

    !> "no sort key was given" rather than a parser message: the list tokenized cleanly, it just
    !! held no names, and blaming the parser would send the caller looking at the wrong thing.
    subroutine test_table_key_list_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_key_list_empty", &
            expect_abort=.true., &
            failure_message="a key list naming no column was expected to abort", &
            required_stderr="sort_by: no sort key was given")
    end subroutine test_table_key_list_empty_aborts

    !> The parser's own message is passed through whole, so the OFFENDING TOKEN is named rather
    !! than the whole list -- that is what the assertion pins.
    subroutine test_table_key_list_bad_direction_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_key_list_bad_direction", &
            expect_abort=.true., &
            failure_message="an unrecognized direction word was expected to abort", &
            required_stderr="'id sideways' has an unrecognized direction 'sideways'")
    end subroutine test_table_key_list_bad_direction_aborts

    !> EVERY missing name, not just the first -- which is the whole reason this binding exists, so
    !! the second one is what the assertion is really about.
    subroutine test_table_require_columns_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_require_columns_missing", &
            expect_abort=.true., &
            failure_message="requiring a missing column was expected to abort", &
            required_stderr="'alsonope'")
    end subroutine test_table_require_columns_missing_aborts

    !! The assertion carries the whole 64-character prefix plus the ellipsis, so a clip at the
    !! wrong width fails as loudly as no clip at all -- asserting on "...'" alone would pass
    !! against any cut-off point.
    subroutine test_table_require_columns_long_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_require_columns_long_name", &
            expect_abort=.true., &
            failure_message="requiring a missing column with a long name was expected to abort", &
            required_stderr="'a_column_name_long_enough_that_quoting_it_whole_would_bloat_the_...'")
    end subroutine test_table_require_columns_long_name_aborts

    !! "and 2 more" rather than just "more": the COUNT is what a caller needs, and an off-by-one
    !! in it would otherwise be invisible.
    subroutine test_table_require_columns_many_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_require_columns_many_missing", &
            expect_abort=.true., &
            failure_message="requiring twelve missing columns was expected to abort", &
            required_stderr="'m10' and 2 more")
    end subroutine test_table_require_columns_many_missing_aborts

    subroutine test_columns_paste_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_kind_mismatch", expect_abort=.true., &
            failure_message="pasting a column of a different kind was expected to abort", &
            required_stderr="parquet_columns: paste: column kinds differ")
    end subroutine test_columns_paste_kind_mismatch_aborts

    subroutine test_columns_paste_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_width_mismatch", expect_abort=.true., &
            failure_message="pasting a vector column of a different width was expected to abort", &
            required_stderr="parquet_columns: paste: column widths differ")
    end subroutine test_columns_paste_width_mismatch_aborts

    subroutine test_columns_paste_string_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_string_kind", expect_abort=.true., &
            failure_message="pasting into a string column was expected to abort", &
            required_stderr="parquet_columns: paste: the string kinds cannot be overwritten in place")
    end subroutine test_columns_paste_string_kind_aborts

    subroutine test_columns_paste_source_index_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_source_index_below_one", &
            expect_abort=.true., &
            failure_message="pasting from source row 0 was expected to abort", &
            required_stderr="parquet_columns: paste: source row index is below 1")
    end subroutine test_columns_paste_source_index_below_one_aborts

    subroutine test_columns_paste_negative_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_negative_count", expect_abort=.true., &
            failure_message="pasting a negative row count was expected to abort", &
            required_stderr="parquet_columns: paste: negative row count")
    end subroutine test_columns_paste_negative_count_aborts

    subroutine test_columns_paste_source_past_end_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_source_past_end", expect_abort=.true., &
            failure_message="pasting past the end of the source was expected to abort", &
            required_stderr="parquet_columns: paste: source row range extends past the end of the source column")
    end subroutine test_columns_paste_source_past_end_aborts

    subroutine test_columns_paste_destination_index_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_destination_index_below_one", &
            expect_abort=.true., &
            failure_message="pasting at destination row 0 was expected to abort", &
            required_stderr="parquet_columns: paste: destination row index is below 1")
    end subroutine test_columns_paste_destination_index_below_one_aborts

    subroutine test_columns_paste_destination_past_end_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_paste_destination_past_end", &
            expect_abort=.true., &
            failure_message="pasting past the end of the destination was expected to abort", &
            required_stderr="parquet_columns: paste: destination row range extends past the end of the column")
    end subroutine test_columns_paste_destination_past_end_aborts

    subroutine test_columns_reindex_duplicate_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_reindex_duplicate_index", expect_abort=.true., &
            failure_message="reindex with a duplicated permutation index was expected to abort", &
            required_stderr="parquet_columns: reindex: permutation contains a duplicate index")
    end subroutine test_columns_reindex_duplicate_index_aborts

    subroutine test_string_column_reindex_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_reindex_length_mismatch", &
            expect_abort=.true., &
            failure_message="reindex with a short permutation was expected to abort", &
            required_stderr="parquet_strings: reindex: permutation length does not match the row count")
    end subroutine test_string_column_reindex_length_mismatch_aborts

    subroutine test_string_column_reindex_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_reindex_out_of_range", &
            expect_abort=.true., &
            failure_message="reindex with an out-of-range permutation entry was expected to abort", &
            required_stderr="parquet_strings: reindex: permutation entry out of range")
    end subroutine test_string_column_reindex_out_of_range_aborts

    subroutine test_string_column_delete_by_mask_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_delete_by_mask_length_mismatch", &
            expect_abort=.true., &
            failure_message="delete_by_mask with a short mask was expected to abort", &
            required_stderr="parquet_strings: delete_by_mask: mask length does not match the row count")
    end subroutine test_string_column_delete_by_mask_length_mismatch_aborts

    subroutine test_string_column_append_nulls_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_append_nulls_negative", &
            expect_abort=.true., &
            failure_message="append_nulls with a negative count was expected to abort", &
            required_stderr="parquet_strings: append_nulls: negative element count")
    end subroutine test_string_column_append_nulls_negative_aborts

    subroutine test_columns_string_column_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_string_column_wrong_kind", expect_abort=.true., &
            failure_message="asking a float64 column for its string store was expected to abort", &
            required_stderr="parquet_columns: string_column: column kind is PK_FLOAT64")
    end subroutine test_columns_string_column_wrong_kind_aborts

    subroutine test_columns_init_container_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_init_container_kind", expect_abort=.true., &
            failure_message="initializing a reserved container kind was expected to abort", &
            required_stderr="parquet_columns: init: container kinds are reserved and not implemented yet")
    end subroutine test_columns_init_container_kind_aborts

    subroutine test_columns_init_width_on_scalar_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_init_width_on_scalar_kind", expect_abort=.true., &
            failure_message="giving a scalar kind a width > 1 was expected to abort", &
            required_stderr="parquet_columns: init: width > 1 requires a vector (*_VEC) kind")
    end subroutine test_columns_init_width_on_scalar_kind_aborts

    !> Every adopt_* specific (one per PK_* kind) has its own "not allocated" guard on its own
    !! source line, so each needs its own subprocess abort to cover it -- driven here from one
    !! loop over the 16 scenarios in scenario_columns_adopt_not_allocated_* (error_scenarios.f90).
    subroutine test_columns_adopt_not_allocated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        type(error_type), allocatable :: e2
        character(len=34), parameter :: scenarios(16) = [character(len=34) :: &
            "columns_adopt_not_allocated_i32", "columns_adopt_not_allocated_i64", &
            "columns_adopt_not_allocated_f32", "columns_adopt_not_allocated_f64", &
            "columns_adopt_not_allocated_bool", "columns_adopt_not_allocated_date", &
            "columns_adopt_not_allocated_time", "columns_adopt_not_allocated_ts", &
            "columns_adopt_not_allocated_i32v", "columns_adopt_not_allocated_i64v", &
            "columns_adopt_not_allocated_f32v", "columns_adopt_not_allocated_f64v", &
            "columns_adopt_not_allocated_boolv", "columns_adopt_not_allocated_datev", &
            "columns_adopt_not_allocated_timev", "columns_adopt_not_allocated_tsv"]
        integer :: k
        !
        do k = 1, size(scenarios)
            call check_scenario_exit_status_and_stderr(e2, trim(scenarios(k)), expect_abort=.true., &
                failure_message="adopting an unallocated array was expected to abort: " // trim(scenarios(k)), &
                required_stderr="adopt: the array to adopt is not allocated")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
    end subroutine test_columns_adopt_not_allocated_aborts

    subroutine test_columns_set_all_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_set_all_length_mismatch", expect_abort=.true., &
            failure_message="set_all with the wrong number of values was expected to abort", &
            required_stderr="parquet_columns: set_all: value count does not match the column's row count")
    end subroutine test_columns_set_all_length_mismatch_aborts

    subroutine test_columns_get_at_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_get_at_width_mismatch", expect_abort=.true., &
            failure_message="reading a vector row into a mismatched buffer was expected to abort", &
            required_stderr="parquet_columns: get_at: value count per row does not match the column width")
    end subroutine test_columns_get_at_width_mismatch_aborts

    subroutine test_columns_delete_by_mask_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_delete_by_mask_length_mismatch", &
            expect_abort=.true., &
            failure_message="delete_by_mask with a short mask was expected to abort", &
            required_stderr="parquet_columns: delete_by_mask: mask length does not match the column's row count")
    end subroutine test_columns_delete_by_mask_length_mismatch_aborts

    subroutine test_columns_reindex_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_reindex_length_mismatch", expect_abort=.true., &
            failure_message="reindex with a short permutation was expected to abort", &
            required_stderr="parquet_columns: reindex: permutation length does not match the column's row count")
    end subroutine test_columns_reindex_length_mismatch_aborts

    subroutine test_columns_clear_null_temporal_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "columns_clear_null_temporal", expect_abort=.true., &
            failure_message="clear_null on a temporal column was expected to abort", &
            required_stderr="parquet_columns: clear_null: a temporal element becomes valid by writing a value")
    end subroutine test_columns_clear_null_temporal_aborts

    subroutine test_string_column_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_index_out_of_range", expect_abort=.true., &
            failure_message="indexing a parquet_string_column out of range was expected to abort", &
            required_stderr="parquet_strings: index out of range in get")
    end subroutine test_string_column_index_out_of_range_aborts

    subroutine test_string_column_view_all_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_view_all_size_mismatch", expect_abort=.true., &
            failure_message="view_all with a mismatched-size data_string array was expected to abort", &
            required_stderr="parquet_strings: view_all: size(data_string) does not match self%size()")
    end subroutine test_string_column_view_all_size_mismatch_aborts

    subroutine test_string_column_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_get_null", expect_abort=.true., &
            failure_message="get on a null element with no null option was expected to abort", &
            required_stderr="parquet_strings: null element accessed in get")
    end subroutine test_string_column_get_null_aborts

    subroutine test_string_column_to_character_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_to_character_null", expect_abort=.true., &
            failure_message="to_character on a null-containing column with no null_value was expected to abort", &
            required_stderr="parquet_strings: null element accessed in to_character")
    end subroutine test_string_column_to_character_null_aborts

    subroutine test_string_handle_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_handle_unassociated", expect_abort=.true., &
            failure_message="using an unassociated parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: unassociated string handle in length")
    end subroutine test_string_handle_unassociated_aborts

    subroutine test_string_handle_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_handle_stale_index", expect_abort=.true., &
            failure_message="using a parquet_string handle whose index no longer exists was expected to abort", &
            required_stderr="parquet_strings: string handle index out of range (column changed?) in length")
    end subroutine test_string_handle_stale_index_aborts

    subroutine test_string_set_null_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_set_null_unassociated", expect_abort=.true., &
            failure_message="set_null on an unassociated parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: unassociated string handle in set_null")
    end subroutine test_string_set_null_unassociated_aborts

    subroutine test_string_set_null_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_set_null_stale_index", expect_abort=.true., &
            failure_message="set_null on a stale parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: string handle index out of range (column changed?) in set_null")
    end subroutine test_string_set_null_stale_index_aborts

    subroutine test_string_slice_invalid_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_slice_invalid_range", expect_abort=.true., &
            failure_message="slice with first > last was expected to abort", &
            required_stderr="parquet_strings: invalid row range in slice")
    end subroutine test_string_slice_invalid_range_aborts

    subroutine test_string_view_slice_invalid_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_view_slice_invalid_range", expect_abort=.true., &
            failure_message="view_slice with last > self%size() was expected to abort", &
            required_stderr="parquet_strings: invalid row range in view_slice")
    end subroutine test_string_view_slice_invalid_range_aborts

    subroutine test_string_view_slice_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_view_slice_size_mismatch", expect_abort=.true., &
            failure_message="view_slice with a mismatched-size data_string array was expected to abort", &
            required_stderr="parquet_strings: view_slice: size(data_string) does not match last-first+1")
    end subroutine test_string_view_slice_size_mismatch_aborts

    subroutine test_string_build_from_self_alias_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_self_alias", expect_abort=.true., &
            failure_message="build_from with a handle aliasing the destination column was expected to abort", &
            required_stderr="parquet_strings: build_from: handle aliases the destination column self")
    end subroutine test_string_build_from_self_alias_aborts

    subroutine test_string_build_from_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_unassociated", expect_abort=.true., &
            failure_message="build_from with an unassociated handle in the input array was expected to abort", &
            required_stderr="parquet_strings: build_from: unassociated handle in input array")
    end subroutine test_string_build_from_unassociated_aborts

    subroutine test_string_build_from_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_stale_index", expect_abort=.true., &
            failure_message="build_from with a stale handle in the input array was expected to abort", &
            required_stderr="parquet_strings: build_from: stale or out-of-range handle in input array")
    end subroutine test_string_build_from_stale_index_aborts

    subroutine test_string_build_from_character_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_character_mask_length", &
            expect_abort=.true., &
            failure_message="build_from(character) with a mask shorter than values was expected to abort", &
            required_stderr="parquet_strings: build_from: is_null must have the same length as values")
    end subroutine test_string_build_from_character_mask_length_aborts

    subroutine test_string_append_values_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_append_values_mask_length", &
            expect_abort=.true., &
            failure_message="append_values with a mask shorter than values was expected to abort", &
            required_stderr="parquet_strings: append_values: is_null must have the same length as values")
    end subroutine test_string_append_values_mask_length_aborts

    subroutine test_string_column_append_buffers_offset_not_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_append_buffers_offset_not_zero", &
            expect_abort=.true., &
            failure_message="append_buffers with a source offsets(1) /= 0 was expected to abort", &
            required_stderr="parquet_strings: append_buffers: source offsets(1) must be 0")
    end subroutine test_string_column_append_buffers_offset_not_zero_aborts

    subroutine test_string_column_append_buffers_offset_not_zero_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_append_buffers_offset_not_zero_int32", &
            expect_abort=.true., &
            failure_message="append_buffers with a source int32 offsets(1) /= 0 was expected to abort", &
            required_stderr="parquet_strings: append_buffers: source offsets(1) must be 0")
    end subroutine test_string_column_append_buffers_offset_not_zero_int32_aborts

    subroutine test_compact_string_write_requires_scalar_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "compact_string_write_requires_scalar_column", &
            expect_abort=.true., &
            failure_message="a compact string write into a vector (col_size>1) column was expected to abort", &
            required_stderr="a parquet_string_column write requires a scalar (col_size=1) column")
    end subroutine test_compact_string_write_requires_scalar_column_aborts

    subroutine test_compact_string_write_chunk_requires_scalar_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "compact_string_write_chunk_requires_scalar_column", &
            expect_abort=.true., &
            failure_message="a compact string chunk write into a vector (col_size>1) column was expected to abort", &
            required_stderr="a parquet_string_column write requires a scalar (col_size=1) column")
    end subroutine test_compact_string_write_chunk_requires_scalar_column_aborts

    !> A STRING_VIEW column read into a compact parquet_string_column round-trips: values, the
    !> null, and the row count. This test asserted the OPPOSITE until the compact path learned to
    !> convert a view array to large_utf8 first -- so it is converted rather than deleted, which is
    !> what its own former doc-comment could not tell anyone to do, since the rule requiring a
    !> refusal test to say what replaces it post-dated it.
    !>
    !> The scenario asserts by error stop, so a wrong value shows up here as a non-zero exit.
    subroutine test_string_view_compact_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "string_view_compact_read", expect_abort=.false., &
            failure_message="a STRING_VIEW column did not round-trip into a compact parquet_string_column")
    end subroutine test_string_view_compact_read

    subroutine test_ok_scenario_exits_cleanly(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "ok", expect_abort=.false., &
            failure_message="control scenario 'ok' was expected to exit cleanly")
    end subroutine test_ok_scenario_exits_cleanly

    !> Also checks the error names both the output file and the schema's
    !> maml -- see writer_context_suffix in src/parquet_write.f90.
    subroutine test_write_undeclared_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_undeclared_column", expect_abort=.true., &
            failure_message="writing an undeclared column was expected to error stop", &
            required_stderr="parquet_write_column: column not defined in parquet_open_writer: not_a_real_column " // &
                "(file: test_run/error_scenario_undeclared.parquet, maml: maml_example.maml)")
    end subroutine test_write_undeclared_column_aborts

    subroutine test_write_undeclared_column_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int64", expect_abort=.true., &
            failure_message="writing an undeclared int64 column was expected to error stop")
    end subroutine test_write_undeclared_column_int64_aborts

    subroutine test_write_undeclared_column_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float32", expect_abort=.true., &
            failure_message="writing an undeclared float32 column was expected to error stop")
    end subroutine test_write_undeclared_column_float32_aborts

    subroutine test_write_undeclared_column_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float64", expect_abort=.true., &
            failure_message="writing an undeclared float64 column was expected to error stop")
    end subroutine test_write_undeclared_column_float64_aborts

    subroutine test_write_undeclared_column_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_logical", expect_abort=.true., &
            failure_message="writing an undeclared logical column was expected to error stop")
    end subroutine test_write_undeclared_column_logical_aborts

    subroutine test_write_undeclared_column_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string", expect_abort=.true., &
            failure_message="writing an undeclared string column was expected to error stop")
    end subroutine test_write_undeclared_column_string_aborts

    subroutine test_write_undeclared_column_string_compact_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string_compact", expect_abort=.true., &
            failure_message="writing an undeclared column via a compact string write was expected to error stop")
    end subroutine test_write_undeclared_column_string_compact_aborts

    subroutine test_write_undeclared_column_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int32 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_int32_matrix_aborts

    subroutine test_write_undeclared_column_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int64 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_int64_matrix_aborts

    subroutine test_write_undeclared_column_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float32 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_float32_matrix_aborts

    subroutine test_write_undeclared_column_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float64 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_float64_matrix_aborts

    subroutine test_write_undeclared_column_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_logical_matrix", expect_abort=.true., &
            failure_message="writing an undeclared logical matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_logical_matrix_aborts

    subroutine test_write_undeclared_column_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string_matrix", expect_abort=.true., &
            failure_message="writing an undeclared string matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_string_matrix_aborts

    subroutine test_write_not_divisible_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_int64", expect_abort=.true., &
            failure_message="writing a int64 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_int64_aborts

    subroutine test_write_not_divisible_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_float32", expect_abort=.true., &
            failure_message="writing a float32 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_float32_aborts

    subroutine test_write_not_divisible_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_float64", expect_abort=.true., &
            failure_message="writing a float64 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_float64_aborts

    subroutine test_write_not_divisible_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_logical", expect_abort=.true., &
            failure_message="writing a logical values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_logical_aborts

    subroutine test_write_not_divisible_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_string", expect_abort=.true., &
            failure_message="writing a string values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_string_aborts

    subroutine test_write_array_mismatch_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_int32_matrix", expect_abort=.true., &
            failure_message="writing a int32 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_int32_matrix_aborts

    subroutine test_write_array_mismatch_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_int64_matrix", expect_abort=.true., &
            failure_message="writing a int64 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_int64_matrix_aborts

    subroutine test_write_array_mismatch_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_float32_matrix", expect_abort=.true., &
            failure_message="writing a float32 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_float32_matrix_aborts

    subroutine test_write_array_mismatch_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_float64_matrix", expect_abort=.true., &
            failure_message="writing a float64 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_float64_matrix_aborts

    subroutine test_write_array_mismatch_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_logical_matrix", expect_abort=.true., &
            failure_message="writing a logical matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_logical_matrix_aborts

    subroutine test_write_array_mismatch_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_string_matrix", expect_abort=.true., &
            failure_message="writing a string matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_string_matrix_aborts

    !> streaming (parquet_write_column_chunk) counterparts of the undeclared-column/
    !! not-divisible/array-mismatch tests above -- one deep check_scenario_exit_status_and_stderr
    !! (int32 scalar) plus exit-status-only checks for the rest, matching the batch API's own
    !! mix above.
    subroutine test_write_chunk_undeclared_column_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_undeclared_column_int32", expect_abort=.true., &
            failure_message="writing an undeclared int32 chunk column was expected to error stop", &
            required_stderr="parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                "not_a_real_column (file: test_run/error_scenario_chunk_undeclared_int32.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_undeclared_column_int32_aborts

    subroutine test_write_chunk_undeclared_column_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int64", expect_abort=.true., &
            failure_message="writing an undeclared int64 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int64_aborts

    subroutine test_write_chunk_undeclared_column_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float32", expect_abort=.true., &
            failure_message="writing an undeclared float32 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float32_aborts

    subroutine test_write_chunk_undeclared_column_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float64", expect_abort=.true., &
            failure_message="writing an undeclared float64 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float64_aborts

    subroutine test_write_chunk_undeclared_column_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_logical", expect_abort=.true., &
            failure_message="writing an undeclared logical chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_logical_aborts

    subroutine test_write_chunk_undeclared_column_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string", expect_abort=.true., &
            failure_message="writing an undeclared string chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_aborts

    subroutine test_write_chunk_undeclared_column_string_compact_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string_compact", expect_abort=.true., &
            failure_message="writing an undeclared column via a compact string chunk write was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_compact_aborts

    subroutine test_write_chunk_undeclared_column_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int32 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int32_matrix_aborts

    subroutine test_write_chunk_undeclared_column_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int64 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int64_matrix_aborts

    subroutine test_write_chunk_undeclared_column_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float32 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float32_matrix_aborts

    subroutine test_write_chunk_undeclared_column_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float64 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float64_matrix_aborts

    subroutine test_write_chunk_undeclared_column_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_logical_matrix", expect_abort=.true., &
            failure_message="writing an undeclared logical matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_logical_matrix_aborts

    subroutine test_write_chunk_undeclared_column_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string_matrix", expect_abort=.true., &
            failure_message="writing an undeclared string matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_matrix_aborts

    subroutine test_write_chunk_not_divisible_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_int32", expect_abort=.true., &
            failure_message="writing a int32 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_int32_aborts

    subroutine test_write_chunk_not_divisible_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_int64", expect_abort=.true., &
            failure_message="writing a int64 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_int64_aborts

    subroutine test_write_chunk_not_divisible_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_float32", expect_abort=.true., &
            failure_message="writing a float32 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_float32_aborts

    subroutine test_write_chunk_not_divisible_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_float64", expect_abort=.true., &
            failure_message="writing a float64 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_float64_aborts

    subroutine test_write_chunk_not_divisible_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_logical", expect_abort=.true., &
            failure_message="writing a logical chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_logical_aborts

    subroutine test_write_chunk_not_divisible_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_string", expect_abort=.true., &
            failure_message="writing a string chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_string_aborts

    subroutine test_write_chunk_array_mismatch_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_int32_matrix", expect_abort=.true., &
            failure_message="writing a int32 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_int32_matrix_aborts

    subroutine test_write_chunk_array_mismatch_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_int64_matrix", expect_abort=.true., &
            failure_message="writing a int64 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_int64_matrix_aborts

    subroutine test_write_chunk_array_mismatch_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_float32_matrix", expect_abort=.true., &
            failure_message="writing a float32 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_float32_matrix_aborts

    subroutine test_write_chunk_array_mismatch_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_float64_matrix", expect_abort=.true., &
            failure_message="writing a float64 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_float64_matrix_aborts

    subroutine test_write_chunk_array_mismatch_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_logical_matrix", expect_abort=.true., &
            failure_message="writing a logical chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_logical_matrix_aborts

    subroutine test_write_chunk_array_mismatch_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_string_matrix", expect_abort=.true., &
            failure_message="writing a string chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_string_matrix_aborts

    subroutine test_write_chunk_string_matrix_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_string_matrix_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string matrix chunk column " // &
                "was expected to error stop")
    end subroutine test_write_chunk_string_matrix_exceeds_array_size_aborts

    !> Same check as test_write_chunk_string_matrix_exceeds_array_size_aborts, but for
    !! parquet_write_string_column_chunk's 1D/flat form (values(:), dispatched for a rank-1
    !! actual argument).
    subroutine test_write_chunk_string_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_string_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string vector chunk column " // &
                "(flat form) was expected to error stop")
    end subroutine test_write_chunk_string_exceeds_array_size_aborts

    subroutine test_write_chunk_no_row_group_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_no_row_group_open", expect_abort=.true., &
            failure_message="writing a chunk column before parquet_new_row_group was expected to error stop", &
            required_stderr="parquet_write_column_chunk: no row group is open (call parquet_new_row_group " // &
                "first) for column i32 (file: test_run/error_scenario_chunk_no_row_group.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_no_row_group_open_aborts

    subroutine test_write_chunk_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_row_count_mismatch", expect_abort=.true., &
            failure_message="writing a chunk with a row count mismatch was expected to error stop", &
            required_stderr="parquet_write_column_chunk: row count mismatch for column i32: the open row " // &
                "group has 3 rows but this chunk has 2 " // &
                "(file: test_run/error_scenario_chunk_row_count_mismatch.parquet, maml: internal:multitype_table)")
    end subroutine test_write_chunk_row_count_mismatch_aborts

    !> What the chunked path still refuses now that it converts between numeric types exactly as
    !> parquet_write_column does: a pair parquet_is_type_compatible rejects outright. A logical
    !> chunk written to an int32 column is such a pair, and the message is now the same one the
    !> whole-column path gives, differing only in the procedure it names -- which is the point, so
    !> this assertion is what fails if the two paths' diagnostics drift apart again.
    subroutine test_write_chunk_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_type_mismatch", expect_abort=.true., &
            failure_message="writing a chunk whose kind is not compatible with the schema was expected to abort", &
            required_stderr="parquet_write_column_chunk: type mismatch for column i32 (expected boolean, " // &
                "got int32) (file: test_run/error_scenario_chunk_type_mismatch.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_type_mismatch_aborts

    subroutine test_read_chunk_qc_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_qc_hard_aborts", expect_abort=.true., &
            failure_message="a hard qc violation in a chunk read was expected to abort", &
            required_stderr="qc violation for column 'ra [row group 1]'")
    end subroutine test_read_chunk_qc_hard_aborts

    subroutine test_read_chunk_qc_soft_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_qc_soft_warns", expect_abort=.false., &
            failure_message="a soft qc violation in a chunk read was expected to warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra [row group 1]'")
    end subroutine test_read_chunk_qc_soft_warns

    subroutine test_read_chunk_check_complete_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_check_complete_hard_aborts", expect_abort=.true., &
            failure_message="closing an incomplete chunked read with check_complete=.true. was expected to abort", &
            required_stderr="column 'v' was read via parquet_read_column_chunk but not every row group was read")
    end subroutine test_read_chunk_check_complete_hard_aborts

    subroutine test_read_chunk_row_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_row_group_out_of_range", expect_abort=.true., &
            failure_message="reading an out-of-range row_group was expected to error stop", &
            required_stderr="row_group 3 out of range (file has 2 row group(s))")
    end subroutine test_read_chunk_row_group_out_of_range_aborts

    !> parquet_get_chunk_size has its own row_group bounds check, independent of
    !> parquet_read_column_chunk's (see check_row_group_valid vs
    !> parquet_get_chunk_size_reader_impl in parquet_read.f90) -- must abort even when
    !> parquet_read_column_chunk itself is never called.
    subroutine test_get_chunk_size_row_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_chunk_size_row_group_out_of_range", &
            expect_abort=.true., &
            failure_message="parquet_get_chunk_size with an out-of-range row_group was expected to error stop", &
            required_stderr="parquet_get_chunk_size: row_group 3 out of range (file has 2 row group(s))")
    end subroutine test_get_chunk_size_row_group_out_of_range_aborts

    subroutine test_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_type_mismatch", expect_abort=.true., &
            failure_message="writing a type-mismatched column was expected to error stop")
    end subroutine test_write_type_mismatch_aborts

    subroutine test_write_column_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_column_twice", expect_abort=.true., &
            failure_message="writing the same column twice was expected to error stop")
    end subroutine test_write_column_twice_aborts

    !> Same rule as test_write_column_twice_aborts, but for a schema-less
    !> writer (no cinfo), which previously had no tracking at all for this.
    subroutine test_write_column_twice_no_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_column_twice_no_schema", expect_abort=.true., &
            failure_message="writing the same column twice on a schema-less writer was expected to error stop", &
            required_stderr="parquet_write_column: column written more than once: id")
    end subroutine test_write_column_twice_no_schema_aborts

    subroutine test_validate_bad_data_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_bad_data_type", expect_abort=.true., &
            failure_message="validating an invalid data_type was expected to error stop")
    end subroutine test_validate_bad_data_type_aborts

    !> Locks in the "decimal" type exclusion from the README's Limitations section (date/
    !> timestamp were excluded here too once, but are now supported -- see
    !> src/parquet_temporal.f90 -- so those two tests were removed) so that accidentally adding
    !> it to the allowed type list would be caught here, not just a generic invalid-token check.
    subroutine test_validate_excluded_decimal_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_excluded_decimal_type", expect_abort=.true., &
            failure_message="'decimal' is documented as unsupported and was expected to error stop")
    end subroutine test_validate_excluded_decimal_type_aborts

    subroutine test_validate_empty_field_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_empty_field_name", expect_abort=.true., &
            failure_message="validating a fields: entry with no name: sub-key was expected to error stop")
    end subroutine test_validate_empty_field_name_aborts

    subroutine test_validate_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_duplicate_name", expect_abort=.true., &
            failure_message="validating a duplicate field name was expected to error stop")
    end subroutine test_validate_duplicate_name_aborts

    subroutine test_validate_missing_table_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_missing_table", expect_abort=.true., &
            failure_message="validating a MAML file without 'table:' was expected to error stop")
    end subroutine test_validate_missing_table_aborts

    subroutine test_validate_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_no_fields", expect_abort=.true., &
            failure_message="validating a MAML file without fields was expected to error stop")
    end subroutine test_validate_no_fields_aborts

    subroutine test_validate_trailing_depends_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_trailing_depends_no_fields", expect_abort=.true., &
            failure_message="a MAML with a trailing, unclosed depends: list and no fields: " // &
                "was expected to error stop")
    end subroutine test_validate_trailing_depends_no_fields_aborts

    subroutine test_validate_trailing_keywords_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_trailing_keywords_no_fields", expect_abort=.true., &
            failure_message="a MAML with a trailing, unclosed keywords: list and no fields: " // &
                "was expected to error stop")
    end subroutine test_validate_trailing_keywords_no_fields_aborts

    subroutine test_validate_unknown_top_level_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_top_level_section", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown top-level section was expected to error stop")
    end subroutine test_validate_unknown_top_level_section_aborts

    subroutine test_validate_unknown_field_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_field_subkey", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown field sub-key was expected to error stop")
    end subroutine test_validate_unknown_field_subkey_aborts

    subroutine test_validate_unknown_qc_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_qc_subkey", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown qc: sub-key was expected to error stop")
    end subroutine test_validate_unknown_qc_subkey_aborts

    subroutine test_validate_user_maml_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_user_maml_unknown_column", expect_abort=.true., &
            failure_message="validating a user MAML with an unknown column was expected to error stop")
    end subroutine test_validate_user_maml_unknown_column_aborts

    subroutine test_validate_col_map_unknown_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_unknown_internal", expect_abort=.true., &
            failure_message="col_map: referencing an internal column not in the base MAML was expected to error stop")
    end subroutine test_validate_col_map_unknown_internal_aborts

    !> Asserts the specific "duplicate internal column" message (not just
    !> that something aborted), since this scenario's fixture is carefully
    !> built to avoid tripping the (unrelated) generic "duplicate field
    !> name" check first -- see its doc comment in error_scenarios.f90.
    subroutine test_validate_col_map_duplicate_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_col_map_duplicate_internal", expect_abort=.true., &
            failure_message="col_map: a duplicate internal column key was expected to error stop", &
            required_stderr="col_map: duplicate internal column 'a'")
    end subroutine test_validate_col_map_duplicate_internal_aborts

    subroutine test_validate_col_map_output_collision_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_collision", expect_abort=.true., &
            failure_message="col_map: two internal columns mapped to the same output name was expected to error stop")
    end subroutine test_validate_col_map_output_collision_aborts

    subroutine test_validate_col_map_output_not_declared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_not_declared", expect_abort=.true., &
            failure_message="col_map: a renamed output not declared in fields: was expected to error stop")
    end subroutine test_validate_col_map_output_not_declared_aborts

    subroutine test_validate_col_map_internal_also_in_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_internal_also_in_fields", expect_abort=.true., &
            failure_message="col_map: a remapped internal column also declared un-renamed in fields: " // &
                "was expected to error stop")
    end subroutine test_validate_col_map_internal_also_in_fields_aborts

    subroutine test_validate_col_map_output_matches_other_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_matches_other_field", expect_abort=.true., &
            failure_message="col_map: output name coinciding with an existing base column was expected to error stop")
    end subroutine test_validate_col_map_output_matches_other_field_aborts

    subroutine test_set_column_available_deactivated_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_column_available_deactivated", expect_abort=.true., &
            failure_message="set_column_available on a deactivated column was expected to error stop")
    end subroutine test_set_column_available_deactivated_aborts

    subroutine test_set_column_unavailable_deactivated_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_column_unavailable_deactivated", expect_abort=.true., &
            failure_message="set_column_unavailable on a deactivated column was expected to error stop")
    end subroutine test_set_column_unavailable_deactivated_aborts

    subroutine test_get_column_index_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_column_index_not_found", expect_abort=.true., &
            failure_message="get_column_index on an unknown column was expected to error stop")
    end subroutine test_get_column_index_not_found_aborts

    subroutine test_get_field_name_index_too_low_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_name_index_too_low", expect_abort=.true., &
            failure_message="get_field_name(0) was expected to error stop")
    end subroutine test_get_field_name_index_too_low_aborts

    subroutine test_get_field_name_index_too_high_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_name_index_too_high", expect_abort=.true., &
            failure_message="get_field_name(get_num_fields()+1) was expected to error stop")
    end subroutine test_get_field_name_index_too_high_aborts

    subroutine test_write_maml_without_metadata_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_maml_without_metadata", expect_abort=.true., &
            failure_message="write_maml=.true. without metadata was expected to error stop")
    end subroutine test_write_maml_without_metadata_aborts

    !> A genuine Parquet Null with no null_value/is_valid given now aborts via
    !> report_fatal_error (print + std::abort()), same as the type-mismatch
    !> case below, instead of an uncaught C++ exception reaching
    !> std::terminate() -- still a C++-level abort, not a Fortran error stop
    !> (see README's Null values section), just a clean, diagnosable one.
    subroutine test_read_column_with_nulls_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_column_with_nulls", expect_abort=.true., &
            failure_message="reading a column with a genuine Parquet Null was expected to abort", &
            required_stderr="column contains Null value(s), which is not supported: id_with_null")
    end subroutine test_read_column_with_nulls_aborts

    !> README's Limitations section documents that reading a column whose
    !> physical Parquet type doesn't match what was requested aborts the
    !> process via a C++-level abort (not a clean Fortran error stop).
    !> parquet_wrapper.cpp's read functions report that failure via
    !> report_fatal_error (print + std::abort()) instead of letting an
    !> uncaught C++ exception reach std::terminate() -- this just checks the
    !> process still aborts (nonzero exit), which is all this out-of-process
    !> harness can observe either way.
    subroutine test_read_unsupported_physical_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_unsupported_physical_type", expect_abort=.true., &
            failure_message="reading a column of an unsupported physical Parquet type was expected to abort")
    end subroutine test_read_unsupported_physical_type_aborts

    !> The 16 tests below are the Fortran-side counterpart of 16 scenarios added to
    !> error_scenarios.f90 covering array-mode bool8/string type-mismatch + col_index bounds
    !> across whole-column/row-mode/element-mode/chunk-mode reads, plus two extra element-mode
    !> col_index-bounds tests (numeric) -- see error_scenarios.f90's own comment above those
    !> 16 scenarios.

    subroutine test_read_array_full_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_full_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading an int32 vector column via the whole-column logical array specific " // &
            "was expected to abort")
    end subroutine test_read_array_full_bool_type_mismatch_aborts

    subroutine test_read_array_full_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_full_string_type_mismatch", expect_abort=.true., &
            failure_message="reading an int32 vector column via the whole-column string array specific " // &
            "was expected to abort")
    end subroutine test_read_array_full_string_type_mismatch_aborts

    subroutine test_read_array_row_mode_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_row_mode_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading one row of an int32 vector column via the logical row_mode specific " // &
            "was expected to abort")
    end subroutine test_read_array_row_mode_bool_type_mismatch_aborts

    subroutine test_read_array_row_mode_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_row_mode_string_type_mismatch", expect_abort=.true., &
            failure_message="reading one row of an int32 vector column via the string row_mode specific " // &
            "was expected to abort")
    end subroutine test_read_array_row_mode_string_type_mismatch_aborts

    subroutine test_read_array_em_filt_bool_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_bool_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "logical reader was expected to abort")
    end subroutine test_read_array_em_filt_bool_oob_aborts

    subroutine test_read_array_em_filt_bool_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_bool_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as logical on a " // &
            "filtered reader was expected to abort")
    end subroutine test_read_array_em_filt_bool_tm_aborts

    subroutine test_read_array_em_filt_string_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_string_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "string reader was expected to abort")
    end subroutine test_read_array_em_filt_string_oob_aborts

    subroutine test_read_array_em_filt_string_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_string_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as string on a " // &
            "filtered reader was expected to abort")
    end subroutine test_read_array_em_filt_string_tm_aborts

    subroutine test_read_array_em_bool_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_bool_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "logical reader was expected to abort")
    end subroutine test_read_array_em_bool_oob_aborts

    subroutine test_read_array_em_bool_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_bool_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as logical on an " // &
            "unfiltered reader was expected to abort")
    end subroutine test_read_array_em_bool_tm_aborts

    subroutine test_read_array_em_string_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_string_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "string reader was expected to abort")
    end subroutine test_read_array_em_string_oob_aborts

    subroutine test_read_array_em_string_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_string_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as string on an " // &
            "unfiltered reader was expected to abort")
    end subroutine test_read_array_em_string_tm_aborts

    subroutine test_read_array_column_chunk_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_column_chunk_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading a chunked int32 vector column as logical was expected to abort")
    end subroutine test_read_array_column_chunk_bool_type_mismatch_aborts

    subroutine test_read_array_column_chunk_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_column_chunk_string_type_mismatch", expect_abort=.true., &
            failure_message="reading a chunked int32 vector column as string was expected to abort")
    end subroutine test_read_array_column_chunk_string_type_mismatch_aborts

    subroutine test_read_array_em_filt_int32_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_int32_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "int32 reader was expected to abort")
    end subroutine test_read_array_em_filt_int32_oob_aborts

    subroutine test_read_array_em_int32_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_int32_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "int32 reader was expected to abort")
    end subroutine test_read_array_em_int32_oob_aborts

    !> The 11 tests below are the Fortran-side counterpart of
    !> error_scenarios.f90's own "extended_..." scenarios documented there --
    !> each checks the exact stderr message for one report_fatal_error call
    !> site added to convert_values_to_int32/int64 (parquet_wrapper.cpp) for
    !> the extended read-time source types (see doc/pages/types/supported-data-types.md
    !> and CONTRIBUTING.md's "Additional scalar types" note).

    subroutine test_extended_uint32_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint32_overflow_int32", expect_abort=.true., &
            failure_message="reading a uint32 value exceeding int32 range was expected to error stop", &
            required_stderr="uint32->int32 overflow for column: v_uint32_ovf")
    end subroutine test_extended_uint32_overflow_int32_aborts

    subroutine test_extended_uint64_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint64_overflow_int32", expect_abort=.true., &
            failure_message="reading a uint64 value exceeding int32 range was expected to error stop", &
            required_stderr="uint64->int32 overflow for column: v_uint64_ovf32")
    end subroutine test_extended_uint64_overflow_int32_aborts

    subroutine test_extended_uint64_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint64_overflow_int64", expect_abort=.true., &
            failure_message="reading a uint64 value exceeding int64 range was expected to error stop", &
            required_stderr="uint64->int64 overflow for column: v_uint64_ovf64")
    end subroutine test_extended_uint64_overflow_int64_aborts

    subroutine test_extended_real_nonintegral_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_nonintegral_int32", expect_abort=.true., &
            failure_message="reading a non-integral double value into int32 was expected to error stop", &
            required_stderr="double value has a fractional part, cannot convert to int32 for column: v_double_fractional")
    end subroutine test_extended_real_nonintegral_int32_aborts

    subroutine test_extended_real_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_overflow_int32", expect_abort=.true., &
            failure_message="reading an out-of-int32-range double value was expected to error stop", &
            required_stderr="double->int32 overflow for column: v_double_ovf32")
    end subroutine test_extended_real_overflow_int32_aborts

    subroutine test_extended_real_nonintegral_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_nonintegral_int64", expect_abort=.true., &
            failure_message="reading a non-integral double value into int64 was expected to error stop", &
            required_stderr="double value has a fractional part, cannot convert to int64 for column: v_double_fractional")
    end subroutine test_extended_real_nonintegral_int64_aborts

    subroutine test_extended_real_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_overflow_int64", expect_abort=.true., &
            failure_message="reading an out-of-int64-range double value was expected to error stop", &
            required_stderr="double->int64 overflow for column: v_double_ovf64")
    end subroutine test_extended_real_overflow_int64_aborts

    subroutine test_extended_decimal_nonintegral_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_nonintegral_int32", expect_abort=.true., &
            failure_message="reading a non-integral decimal value into int32 was expected to error stop", &
            required_stderr="value has a fractional part, cannot convert to int32 for column: v_decimal_scaled")
    end subroutine test_extended_decimal_nonintegral_int32_aborts

    subroutine test_extended_decimal_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_overflow_int32", expect_abort=.true., &
            failure_message="reading an out-of-int32-range decimal value was expected to error stop", &
            required_stderr="->int32 overflow for column: v_decimal_ovf32")
    end subroutine test_extended_decimal_overflow_int32_aborts

    subroutine test_extended_decimal_nonintegral_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_nonintegral_int64", expect_abort=.true., &
            failure_message="reading a non-integral decimal value into int64 was expected to error stop", &
            required_stderr="value has a fractional part, cannot convert to int64 for column: v_decimal_scaled")
    end subroutine test_extended_decimal_nonintegral_int64_aborts

    subroutine test_extended_decimal_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_overflow_int64", expect_abort=.true., &
            failure_message="reading an out-of-int64-range decimal value was expected to error stop", &
            required_stderr="->int64 overflow for column: v_decimal_ovf64")
    end subroutine test_extended_decimal_overflow_int64_aborts

    !> check_col_size_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment a vector column's col_size
    !> would exceed Arrow's FixedSizeListType int32_t limit -- see the README's Limitations
    !> section and scenario_col_size_overflow's own comment for why this is tested with a
    !> shrunk test-only threshold rather than a genuinely oversized column.
    subroutine test_write_col_size_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "col_size_overflow", expect_abort=.true., &
            failure_message="writing a vector column with col_size exceeding the (shrunk) Arrow limit " // &
                "was expected to abort", &
            required_stderr="parquet_append_column: column 'v': col_size (6) exceeds 2147483647, " // &
                "the maximum vector-column width Arrow's FixedSizeListType supports")
    end subroutine test_write_col_size_overflow_aborts

    !> check_explicit_chunk_size_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment an *explicit* chunk_size
    !> (parquet_open_writer(..., chunk_size=)/parquet_set_writer_options) combined with a vector
    !> column's col_size would exceed Arrow/Parquet's int32_t per-row-group list-element-count
    !> limit -- distinct from col_size alone (test_write_col_size_overflow_aborts, above). An
    !> *auto*-sized chunk_size no longer aborts for this (see
    !> scenario_list_element_count_auto_multi_row_group in test_writing.f90's
    !> test_list_element_count_auto_multi_row_group_roundtrip): only a caller-chosen chunk_size
    !> that conflicts with col_size is validated rather than silently overridden. See the
    !> README's Limitations section and
    !> scenario_list_element_count_explicit_chunk_size_overflow's own comment for why this is
    !> tested with a shrunk test-only threshold rather than a genuinely oversized
    !> (chunk_size * col_size > 2^31-1) vector column.
    subroutine test_write_list_element_count_chunk_size_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "list_element_count_explicit_chunk_size_overflow", &
            expect_abort=.true., &
            failure_message="writing a vector column with an explicit chunk_size*col_size exceeding the " // &
                "(shrunk) Arrow limit was expected to abort", &
            required_stderr="close_parquet_writer: column 'v': chunk_size (3) * col_size (2) exceeds 2147483647, " // &
                "the maximum per-row-group element count Arrow/Parquet's list-column level generation supports")
    end subroutine test_write_list_element_count_chunk_size_overflow_aborts

    !> Counterpart to test_write_list_element_count_chunk_size_overflow_aborts, above, for the
    !> streaming row-group API: an explicit parquet_new_row_group(writer, nrows) whose nrows
    !> conflicts with a schema-declared vector column's col_size aborts via a C++-level
    !> report_fatal_error, the same way an explicit chunk_size does for the batch path. See
    !> scenario_row_group_explicit_nrows_overflow's own comment for why this is tested with a
    !> shrunk test-only threshold.
    subroutine test_row_group_explicit_nrows_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_explicit_nrows_overflow", expect_abort=.true., &
            failure_message="starting a row group with an explicit nrows*col_size exceeding the (shrunk) Arrow " // &
                "limit was expected to abort", &
            required_stderr="parquet_new_row_group: column 'v': nrows (3) * col_size (2) exceeds 2147483647, " // &
                "the maximum per-row-group element count Arrow/Parquet's list-column level generation supports")
    end subroutine test_row_group_explicit_nrows_overflow_aborts

    !> close_streaming_writer in parquet_wrapper.cpp aborts (a C++-level uncaught
    !> std::runtime_error, not a clean Fortran error stop) if a row group was started via
    !> parquet_new_row_group but parquet_finish_row_group was never called before
    !> parquet_close_writer -- see scenario_row_group_dangling_at_close's own comment.
    subroutine test_row_group_dangling_at_close_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_dangling_at_close", expect_abort=.true., &
            failure_message="closing a writer with a dangling open row group was expected to abort", &
            required_stderr="A row group was started via parquet_new_row_group but never finished via " // &
                "parquet_finish_row_group before close")
    end subroutine test_row_group_dangling_at_close_aborts

    !> close_streaming_writer aborts if a whole (parquet_write_column) column's row count
    !> exceeds what the streaming row-group API actually covered by the time the writer closes
    !> -- see scenario_row_group_whole_column_undercovered's own comment.
    subroutine test_row_group_whole_column_undercovered_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_undercovered", &
            expect_abort=.true., &
            failure_message="closing a writer with an under-covered whole column was expected to abort", &
            required_stderr="column 'whole' has 5 rows (written via parquet_write_column), but only 3 were " // &
                "covered by row groups written via parquet_new_row_group/parquet_write_column_chunk/" // &
                "parquet_finish_row_group")
    end subroutine test_row_group_whole_column_undercovered_aborts

    !> parquet_finish_row_group in parquet_wrapper.cpp aborts (via report_fatal_error)
    !> immediately -- rather than waiting until close -- if a row group's own rows would read
    !> past the end of an already-whole (parquet_write_column) column. See
    !> scenario_row_group_whole_column_overrun's own comment.
    subroutine test_row_group_whole_column_overrun_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_overrun", expect_abort=.true., &
            failure_message="a row group reading past a whole column's own row count was expected to abort", &
            required_stderr="parquet_finish_row_group: column 'whole' has 3 rows (written via " // &
                "parquet_write_column), but row groups have already covered 0 of them and this row group " // &
                "would add 5 more, exceeding the column's own row count")
    end subroutine test_row_group_whole_column_overrun_aborts

    !> check_column_chunk_write_preconditions in parquet_wrapper.cpp aborts (via
    !> report_fatal_error) if a column is introduced (its first parquet_write_column_chunk call)
    !> after the first row group has already been written -- a Parquet file's schema is fixed
    !> from that point on. See scenario_row_group_new_column_after_first's own comment.
    subroutine test_row_group_new_column_after_first_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_new_column_after_first", expect_abort=.true., &
            failure_message="introducing a new column after the first row group was expected to abort", &
            required_stderr="parquet_write_column_chunk: column 'b': introduced after the first row group " // &
                "was already written -- every column must appear in the first row group, since a Parquet " // &
                "file's schema is fixed once the first row group is written")
    end subroutine test_row_group_new_column_after_first_aborts

    !> The converse of the test above: append_column (the whole-column parquet_write_column
    !> path) throws once the streaming row-group API has already started, even for a column
    !> the streaming API never touched. Unlike
    !> test_row_group_new_column_after_first_aborts above, this is a plain `throw`, not
    !> report_fatal_error.
    subroutine test_row_group_whole_column_after_streaming_started_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_after_streaming_started", &
            expect_abort=.true., &
            failure_message="a whole-column write after the streaming row-group API already started " // &
                "was expected to abort", &
            required_stderr="Column written via parquet_write_column after the streaming row-group API " // &
                "already started")
    end subroutine test_row_group_whole_column_after_streaming_started_aborts

    !> parquet_new_row_group must not be called again while a row group is already open --
    !> see the writer%in_row_group guard added to parquet_new_row_group_impl.
    subroutine test_row_group_started_while_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_started_while_open", expect_abort=.true., &
            failure_message="starting a new row group while one was already open was expected to abort", &
            required_stderr="parquet_new_row_group: a row group is already open")
    end subroutine test_row_group_started_while_open_aborts

    !> check_column_count_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment a table's column count would
    !> exceed Arrow's Schema int32_t field-count limit -- see the README's Limitations section
    !> and scenario_column_count_overflow's own comment for why this is tested with a shrunk
    !> test-only threshold rather than a genuinely huge number of columns.
    subroutine test_write_column_count_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "column_count_overflow", expect_abort=.true., &
            failure_message="writing a table with column count exceeding the (shrunk) Arrow limit " // &
                "was expected to abort", &
            required_stderr="parquet_append_column: column 'c4': this table would have 4 columns, " // &
                "exceeding 2147483647, the maximum column count Arrow's Schema supports")
    end subroutine test_write_column_count_overflow_aborts

    !> parquet_prefetch_columns now validates names against the file's schema
    !> up front and error stops with a dedicated message, instead of letting
    !> the C++ "Column not found" exception escape uncaught.
    subroutine test_prefetch_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "prefetch_unknown_column", expect_abort=.true., &
            failure_message="prefetching an unknown column was expected to abort", &
            required_stderr="parquet_prefetch_columns: column not found in parquet file: not_a_real_column")
    end subroutine test_prefetch_unknown_column_aborts

    !> Read-time sort abort path: see scenario_sort_unknown_column in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_unknown_column", expect_abort=.true., &
            failure_message="opening a reader sorted by an unknown column was expected to abort", &
            required_stderr="unknown column in sort key: nosuch")
    end subroutine test_sort_unknown_column_aborts

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

    !> Read-time sort abort path: see scenario_sort_vector_column in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_vector_column", expect_abort=.true., &
            failure_message="opening a reader sorted by a vector column was expected to abort", &
            required_stderr="sort key 'vec' is a vector column")
    end subroutine test_sort_vector_column_aborts

    !> Read-time sort abort path: see scenario_sort_empty_key in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_empty_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_empty_key", expect_abort=.true., &
            failure_message="an empty sort key was expected to abort", &
            required_stderr="empty sort key")
    end subroutine test_sort_empty_key_aborts

    !> Read-time sort abort path: see scenario_sort_bad_direction in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_bad_direction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_bad_direction", expect_abort=.true., &
            failure_message="an unrecognized sort direction was expected to abort", &
            required_stderr="has an unrecognized direction 'sideways'")
    end subroutine test_sort_bad_direction_aborts

    !> Read-time sort abort path: see scenario_sort_two_direction_words in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_two_direction_words_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_two_direction_words", expect_abort=.true., &
            failure_message="two direction words in one sort key were expected to abort", &
            required_stderr="has more than one direction word")
    end subroutine test_sort_two_direction_words_aborts

    !> Read-time sort abort path: see scenario_sort_minus_and_direction in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_minus_and_direction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_minus_and_direction", expect_abort=.true., &
            failure_message="combining the '-' shorthand with an explicit direction was expected to abort", &
            required_stderr="combines the '-' shorthand with an explicit direction")
    end subroutine test_sort_minus_and_direction_aborts

    !> Read-time sort abort path: see scenario_sort_key_too_long in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_key_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_key_too_long", expect_abort=.true., &
            failure_message="a sort key longer than the supported maximum was expected to abort", &
            required_stderr="parquet_sortkey%add: key exceeds the maximum supported length")
    end subroutine test_sort_key_too_long_aborts

    !> Read-time sort abort path: see scenario_sort_too_many_keys in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_too_many_keys_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_too_many_keys", expect_abort=.true., &
            failure_message="more sort keys than the cap allows was expected to abort", &
            required_stderr="parquet_sortkey%add: too many sort keys")
    end subroutine test_sort_too_many_keys_aborts

    !> Read-time sort abort path: see scenario_sort_chunked_read in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_chunked_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_chunked_read", expect_abort=.true., &
            failure_message="a chunked read on a sorted reader was expected to abort", &
            required_stderr="parquet_read_column_chunk: not supported on a reader with an active sort")
    end subroutine test_sort_chunked_read_aborts

    !> Read-time sort abort path: see scenario_sort_get_chunk_size in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_get_chunk_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_get_chunk_size", expect_abort=.true., &
            failure_message="asking for a chunk size on a sorted reader was expected to abort", &
            required_stderr="parquet_get_chunk_size: not supported on a reader with an active sort")
    end subroutine test_sort_get_chunk_size_aborts

    !> Read-time sort abort path: see scenario_sort_set_sort_twice in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_set_sort_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_set_sort_twice", expect_abort=.true., &
            failure_message="applying a second sort to one reader was expected to abort", &
            required_stderr="this reader already has an active sort")
    end subroutine test_sort_set_sort_twice_aborts

    !> Read-time sort abort path: see scenario_sort_set_sort_after_read in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_set_sort_after_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_set_sort_after_read", expect_abort=.true., &
            failure_message="applying a sort after reading a column was expected to abort", &
            required_stderr="a column has already been read on this reader")
    end subroutine test_sort_set_sort_after_read_aborts

    !> The three ordering guards a chunked read and an active sort would otherwise slip past: see
    !> scenario_sort_set_sort_after_chunked_read, scenario_filter_set_filter_after_chunked_read and
    !> scenario_filter_set_filter_after_sort in test/error_scenarios.f90.
    !>
    !> Each asserts its OWN message rather than merely that the process aborted, and for the last
    !> one that is the whole point: a sorted reader also trips the decoded-columns guard (applying
    !> a sort decodes its key columns), so a test asserting only "it aborted" would keep passing if
    !> the has-sort check were removed or moved back below that guard.
    subroutine test_set_transform_ordering_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_set_sort_after_chunked_read", expect_abort=.true., &
            failure_message="applying a sort after a chunked read was expected to abort", &
            required_stderr="a chunked read has already been done on this reader")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "filter_set_filter_after_chunked_read", expect_abort=.true., &
            failure_message="applying a filter after a chunked read was expected to abort", &
            required_stderr="a chunked read has already been done on this reader")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "filter_set_filter_after_sort", expect_abort=.true., &
            failure_message="applying a filter to a sorted reader was expected to abort", &
            required_stderr="this reader already has an active sort; apply the filter before the sort")
    end subroutine test_set_transform_ordering_aborts

    !> Reading a plain LIST column whose rows do NOT all hold the same number of elements aborts
    !> cleanly. See scenario_read_ragged_list_column, scenario_table_read_ragged_list_column and
    !> scenario_table_read_avg_ok_list_column in test/error_scenarios.f90.
    !>
    !> The suite measured this distinction exhaustively (parquet_measure_list_width, %width against
    !> every column of test/fixtures/list_widths.parquet) and never once READ a non-uniform one --
    !> which is how a test that did read one reached main and aborted the whole table suite.
    !>
    !> Two DIFFERENT mechanisms are covered, not one example twice, and the messages are what tell
    !> them apart -- so each is asserted by message rather than by "did it abort":
    !>
    !>   * `avg_ok` (rows 3,1,3,1) has an integral mean of 2, so the footer screen cannot reject it
    !>     and hands back a candidate of 2. The READ is what catches it, which is the design: a
    !>     table resolves a deferred width with the unproven candidate on purpose, because
    !>     get_uniform_list_values checks every row against the width it was handed. Hence
    !>     "shape mismatch", from the ARRAY entry point, at both reader and table level.
    !>   * `ragged` (rows 1,2,3,4) has a non-integral mean, so the screen rejects it for free and
    !>     the width resolves to 1 -- the column is then classified SCALAR, and the scalar read
    !>     path rejects the list type. Hence "type mismatch", from a different entry point.
    !>
    !> Each scenario carries its own in-process negative control on `uniform` (rows all length 3),
    !> which is a genuine width-3 vector column and must read normally; without it these would pass
    !> against a library that refused every plain LIST column, which is the opposite defect.
    subroutine test_non_uniform_list_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_ragged_list_column", expect_abort=.true., &
            failure_message="reading a list column at a width its rows do not have was expected to abort", &
            required_stderr="parquet_read_int32_array_column: shape mismatch for column: avg_ok")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "table_read_avg_ok_list_column", expect_abort=.true., &
            failure_message="a table read at an unproven width candidate was expected to abort", &
            required_stderr="parquet_read_int32_array_column: shape mismatch for column: avg_ok")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "table_read_ragged_list_column", expect_abort=.true., &
            failure_message="reading a screen-rejected ragged list column was expected to abort", &
            required_stderr="type mismatch for column: ragged")
    end subroutine test_non_uniform_list_read_aborts

    !> parquet_reader_adopt_transform abort paths: see the three scenario_adopt_transform_*
    !> subroutines in test/error_scenarios.f90 for what each does and why that state is rejected.
    !>
    !> All three guard the same property from different sides -- that an adopted mask must describe
    !> exactly the rows the adopting reader will hand back. The primitive puts a reader into a state
    !> it cannot reach on its own, which is why its preconditions are checked rather than documented.
    subroutine test_adopt_transform_onto_transformed_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "adopt_transform_onto_transformed", &
            expect_abort=.true., &
            failure_message="adopting onto an already-transformed reader was expected to abort", &
            required_stderr="already has a filter, a sample or a sort of its own")
    end subroutine test_adopt_transform_onto_transformed_aborts

    !> See test_adopt_transform_onto_transformed_aborts for the shared reasoning.
    subroutine test_adopt_transform_after_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "adopt_transform_after_read", &
            expect_abort=.true., &
            failure_message="adopting a transform after reading a column was expected to abort", &
            required_stderr="a column has already been read on this reader")
    end subroutine test_adopt_transform_after_read_aborts

    !> See test_adopt_transform_onto_transformed_aborts for the shared reasoning.
    subroutine test_adopt_transform_other_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "adopt_transform_other_file", &
            expect_abort=.true., &
            failure_message="adopting a transform across two different files was expected to abort", &
            required_stderr="describe different files")
    end subroutine test_adopt_transform_other_file_aborts

    !> parquet_open_reader(..., filter=) validates every filter column name
    !> against the schema before applying it -- an unknown column aborts
    !> cleanly rather than reaching Arrow's own uncaught exception.
    subroutine test_filter_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unknown_column", expect_abort=.true., &
            failure_message="opening a reader with a filter naming an unknown column was expected to abort", &
            required_stderr="unknown column in filter: not_a_real_column")
    end subroutine test_filter_unknown_column_aborts

    !> Filtering only supports scalar columns; naming a vector column in a
    !> filter rule aborts cleanly instead of silently doing something
    !> undefined per-row.
    subroutine test_filter_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_vector_column", expect_abort=.true., &
            failure_message="opening a reader with a filter naming a vector column was expected to abort", &
            required_stderr="filter column 'vec' is a vector column")
    end subroutine test_filter_vector_column_aborts

    !> A filter rule missing its operator ("<column> <op> [value]" shape)
    !> aborts with a clean, syntax-specific message from the Fortran-side
    !> tokenizer, before ever reaching the C++ side.
    subroutine test_filter_malformed_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_malformed_rule", expect_abort=.true., &
            failure_message="opening a reader with a malformed filter rule was expected to abort", &
            required_stderr="invalid filter rule")
    end subroutine test_filter_malformed_rule_aborts

    subroutine test_filter_rule_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_rule_too_long", expect_abort=.true., &
            failure_message="adding a filter rule longer than the supported maximum was expected to abort", &
            required_stderr="parquet_filter%add: rule exceeds the maximum supported length")
    end subroutine test_filter_rule_too_long_aborts

    subroutine test_filter_remap_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_remap_size_mismatch", expect_abort=.true., &
            failure_message="a filter remap with mismatched from/to sizes was expected to abort", &
            required_stderr="parquet_filter%remap_column_names: from and to must have the same size")
    end subroutine test_filter_remap_size_mismatch_aborts

    subroutine test_filter_remap_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_remap_name_too_long", expect_abort=.true., &
            failure_message="a filter remap to an over-long column name was expected to abort", &
            required_stderr="parquet_filter%remap_column_names: replacement column name")
    end subroutine test_filter_remap_name_too_long_aborts

    !> The message must name the REMAPPING, not just the length cap: the rule the caller wrote was
    !> within the limit, and only the rename pushed it over.
    subroutine test_filter_remap_rule_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_remap_rule_too_long", expect_abort=.true., &
            failure_message="a filter rule that grew past the cap when remapped was expected to abort", &
            required_stderr="characters) after remapping its column names")
    end subroutine test_filter_remap_rule_too_long_aborts

    subroutine test_sortkey_remap_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sortkey_remap_size_mismatch", expect_abort=.true., &
            failure_message="a sortkey remap with mismatched from/to sizes was expected to abort", &
            required_stderr="parquet_sortkey%remap_column_names: from and to must have the same size")
    end subroutine test_sortkey_remap_size_mismatch_aborts

    subroutine test_sortkey_remap_name_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sortkey_remap_name_too_long", expect_abort=.true., &
            failure_message="a sortkey remap to an over-long column name was expected to abort", &
            required_stderr="parquet_sortkey%remap_column_names: replacement column name")
    end subroutine test_sortkey_remap_name_too_long_aborts

    subroutine test_set_arrow_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_arrow_threads_zero", expect_abort=.true., &
            failure_message="a thread-pool capacity of 0 was expected to abort", &
            required_stderr="parquet_set_arrow_threads: n must be >= 1")
    end subroutine test_set_arrow_threads_zero_aborts

    subroutine test_settings_bad_codec_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_bad_codec", expect_abort=.true., &
            failure_message="an unknown default compression codec was expected to abort", &
            required_stderr="parquet_set_default_compression: unknown compression codec 'lzma'")
    end subroutine test_settings_bad_codec_aborts

    subroutine test_settings_negative_sort_threads_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_negative_sort_threads", expect_abort=.true., &
            failure_message="a negative sort thread cap was expected to abort", &
            required_stderr="parquet_set_sort_threads: n must be >= 0")
    end subroutine test_settings_negative_sort_threads_aborts


    subroutine test_settings_negative_bucket_limit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_negative_bucket_limit", expect_abort=.true., &
            failure_message="a negative sort_counting_bucket_limit was expected to abort", &
            required_stderr="parquet_set_sort_counting_bucket_limit: n must be >= 0")
    end subroutine test_settings_negative_bucket_limit_aborts

    subroutine test_settings_negative_row_group_bytes_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_negative_row_group_bytes", expect_abort=.true., &
            failure_message="a negative target_row_group_bytes was expected to abort", &
            required_stderr="parquet_set_target_row_group_bytes: n must be >= 0")
    end subroutine test_settings_negative_row_group_bytes_aborts

    subroutine test_settings_env_bad_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        ! The VARIABLE name is the point: a bare "parquet_set_verbosity: ..." would leave the user
        ! to work out which of thirteen variables was wrong.
        call check_scenario_exit_status_and_stderr(error, "settings_env_bad_token", expect_abort=.true., &
            failure_message="an unknown verbosity token from the environment was expected to abort", &
            required_stderr="parquet_settings_from_env: PARQUET_FORTRAN_VERBOSITY='loud'")
    end subroutine test_settings_env_bad_token_aborts

    subroutine test_settings_env_not_an_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_env_not_an_integer", expect_abort=.true., &
            failure_message="a non-numeric integer from the environment was expected to abort", &
            required_stderr="PARQUET_FORTRAN_SORT_THREADS='many' is not an integer")
    end subroutine test_settings_env_not_an_integer_aborts

    !> The negative control for `exp_key_contract_ok`, without which that guard could be vacuous.
    !!
    !! The scenario runs the same call successfully BEFORE forcing the check to fail, so a guard
    !! that fired unconditionally would abort on the first call and never reach the second — and
    !! this assertion would still see an abort. That is why the scenario, not the wrapper, carries
    !! the control: the exit status alone cannot tell the two apart, but the stderr text can, and
    !! the message asserted here is only reachable from the forced failure.
    subroutine test_weighted_contract_failure_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_contract_failure", expect_abort=.true., &
            failure_message="a build that cannot reproduce the frozen transform was expected to abort", &
            required_stderr="does not reproduce the frozen -log(u) transform")
    end subroutine test_weighted_contract_failure_aborts

    !> A negative weight would be drawn FIRST, so it is refused at the door.
    subroutine test_weighted_negative_weight_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_negative_weight", expect_abort=.true., &
            failure_message="a negative weight was expected to abort, not to be drawn first", &
            required_stderr="a weight is negative")
    end subroutine test_weighted_negative_weight_aborts

    !> An all-zero weight vector names no distribution.
    subroutine test_weighted_all_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_all_zero", expect_abort=.true., &
            failure_message="an all-zero weight vector was expected to abort", &
            required_stderr="every weight is zero")
    end subroutine test_weighted_all_zero_aborts

    !> The one that guards a SILENT failure: a NaN compares false against every bound, so a guard
    !! written without an explicit NaN test files it as zero-weight and drops it to the tail.
    subroutine test_weighted_nan_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_nan_weight", expect_abort=.true., &
            failure_message="a NaN weight was expected to abort, not to be treated as zero", &
            required_stderr="a weight is NaN")
    end subroutine test_weighted_nan_aborts

    !> Drawing from a sampler that was never built reads uninitialised state.
    subroutine test_weighted_uninit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_next_uninitialised", expect_abort=.true., &
            failure_message="%next on an uninitialised sampler was expected to abort", &
            required_stderr="has not been initialised")
    end subroutine test_weighted_uninit_aborts

    !> A second %init would discard a sequence in progress without saying so.
    subroutine test_weighted_init_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_init_twice", expect_abort=.true., &
            failure_message="a second %init was expected to abort and point at %reseed", &
            required_stderr="already called for this sampler")
    end subroutine test_weighted_init_twice_aborts

    !> A subset drawn without replacement cannot exceed its population.
    subroutine test_weighted_subset_big_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_subset_too_large", expect_abort=.true., &
            failure_message="a subset larger than its population was expected to abort", &
            required_stderr="more items requested than there are weights")
    end subroutine test_weighted_subset_big_aborts

    !> A permutation returns every item once, so its array must match the weights.
    subroutine test_weighted_perm_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_perm_size_mismatch", expect_abort=.true., &
            failure_message="a mismatched permutation array was expected to abort", &
            required_stderr="must have the same size")
    end subroutine test_weighted_perm_size_aborts

    !> `%init`'s third value guard: an infinite weight makes every draw degenerate.
    subroutine test_weighted_infinite_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_infinite_weight", expect_abort=.true., &
            failure_message="an infinite weight was expected to abort", &
            required_stderr="a weight is infinite")
    end subroutine test_weighted_infinite_aborts

    !> `pf_weighted_permutation` carries its OWN four weight guards, not `pf_weighted_draw%init`'s.
    !!
    !! The race never builds a sampler, so the two families validate independently -- and each
    !! message names the failure that family would suffer, which is why the assertions below check
    !! the RACE's wording rather than only that something aborted. Every existing weight-validation
    !! scenario goes through `%init`, so all four of these were unreached.
    subroutine test_weighted_race_weight_guards(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "weighted_race_nan_weight", expect_abort=.true., &
            failure_message="a NaN weight in the race was expected to abort", &
            required_stderr="pf_weighted_permutation: a weight is NaN")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "weighted_race_infinite_weight", expect_abort=.true., &
            failure_message="an infinite weight in the race was expected to abort", &
            required_stderr="pf_weighted_permutation: a weight is infinite")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "weighted_race_negative_weight", expect_abort=.true., &
            failure_message="a negative weight in the race was expected to abort", &
            required_stderr="pf_weighted_permutation: a weight is negative")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "weighted_race_all_zero", expect_abort=.true., &
            failure_message="an all-zero weight vector in the race was expected to abort", &
            required_stderr="pf_weighted_permutation: every weight is zero")
    end subroutine test_weighted_race_weight_guards

    !> The three "this population cannot be named by an int32 item index" refusals.
    !!
    !! Each scenario makes the same call twice -- once under the real ceiling, which must succeed
    !! and prints a control line, and once under a forced one, which must abort. Asserting the
    !! control line as well as the abort is what separates a working guard from one that fires
    !! unconditionally; the abort alone would pass against either.
    subroutine test_weighted_int32_population_guards(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found
        integer :: k
        character(len=40) :: names(3)
        character(len=60) :: wanted(3)

        names(1) = "weighted_next_population_too_big"
        names(2) = "weighted_subset_population_too_big"
        names(3) = "weighted_perm_population_too_big"
        wanted(1) = "pf_weighted_draw%next: the population exceeds"
        wanted(2) = "pf_weighted_subset: the population exceeds"
        wanted(3) = "pf_weighted_permutation: the population exceeds"

        do k = 1, 3
            call run_error_scenario(trim(names(k)), exitstat, cmdstat, out_file, err_file)
            call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
            if (allocated(error)) return
            call check(error, exitstat /= 0, &
                "a population above the forced int32 ceiling was expected to abort: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, "control ", found)
            call check(error, found, &
                "the same call under the REAL ceiling must succeed first, or the guard fires " // &
                "unconditionally and the abort proves nothing: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, trim(wanted(k)), found)
            call check(error, found, &
                "the abort must come from this guard and name its own procedure: " // trim(names(k)))
            if (allocated(error)) return
        end do
    end subroutine test_weighted_int32_population_guards

    subroutine test_settings_env_two_numbers_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        ! A list-directed read accepts "4 8" and yields 4, silently. This is the assertion that
        ! stops anyone replacing the strict parser with one.
        call check_scenario_exit_status_and_stderr(error, "settings_env_two_numbers", expect_abort=.true., &
            failure_message="two numbers in one integer variable were expected to abort, not to apply the first", &
            required_stderr="PARQUET_FORTRAN_SORT_THREADS='4 8' is not an integer")
    end subroutine test_settings_env_two_numbers_aborts

    !> A truncated number is a plausible-looking wrong value, so an over-long variable is refused
    !! rather than applied. The message names the cap rather than echoing the value.
    subroutine test_settings_env_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_env_too_long", expect_abort=.true., &
            failure_message="an environment value longer than the buffer was expected to abort", &
            required_stderr="is longer than 4096 characters; refusing to apply a truncated value")
    end subroutine test_settings_env_too_long_aborts

    !> Parsed in int64 first and then range-checked, so a value beyond int32 is refused rather than
    !! wrapping into a plausible small number. Distinct from "not an integer": the parse succeeds.
    subroutine test_settings_env_int32_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_env_int32_out_of_range", &
            expect_abort=.true., &
            failure_message="an environment value beyond int32 was expected to abort, not to wrap", &
            required_stderr="='3000000000' does not fit in a default INTEGER")
    end subroutine test_settings_env_int32_out_of_range_aborts

    !> The offending value is echoed TRUNCATED. ifx's ERROR STOP runtime corrupts the heap once the
    !! composed message reaches 8192 bytes, so a guard that echoed an unbounded caller-supplied
    !! value would crash on exactly the input that triggers it (CLAUDE.md). The assertion is on the
    !! ellipsis, which is what distinguishes a capped message from a whole one.
    subroutine test_settings_env_long_value_preview_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_env_long_value_preview", &
            expect_abort=.true., &
            failure_message="a long environment value was expected to abort with a truncated echo", &
            required_stderr="xxx...' is not an integer")
    end subroutine test_settings_env_long_value_preview_aborts

    !> Not an abort: the informational channel is the quietest of the three output channels, and
    !! `verbosity="silent"` must suppress it while leaving real warnings alone. The scenario emits
    !! one message at the default verbosity and one after going silent; the second not appearing is
    !! the negative control, without which a channel that ignored the setting would still pass.
    subroutine test_settings_emit_info_channel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: saw_visible, saw_suppressed

        call run_error_scenario("settings_emit_info_channel", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "emitting an informational message must not abort")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "info-channel-marker-visible", saw_visible)
        call check(error, saw_visible, "an informational message must be emitted at the default verbosity")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "info-channel-marker-suppressed", saw_suppressed)
        call check(error, .not. saw_suppressed, &
            "verbosity=""silent"" must suppress the informational channel -- otherwise the setting " // &
            "is stored and never read")
    end subroutine test_settings_emit_info_channel

    !> A duplicate index is a separate mistake from an out-of-range one, and the more dangerous:
    !! it silently drops one element and copies another twice, leaving a column that still passes
    !! every structural invariant. The scenario applies a real permutation first.
    subroutine test_strings_reindex_duplicate_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "strings_reindex_duplicate_index", &
            expect_abort=.true., &
            failure_message="a permutation with a duplicate index was expected to abort", &
            required_stderr="reindex: permutation contains a duplicate index")
    end subroutine test_strings_reindex_duplicate_index_aborts

    !> reindex_trusted skips the O(n) validation but keeps the O(1) length check, because a
    !! wrong-length permutation is a caller bug no amount of trust makes safe.
    subroutine test_strings_reindex_trusted_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "strings_reindex_trusted_length_mismatch", &
            expect_abort=.true., &
            failure_message="a trusted permutation of the wrong length was expected to abort", &
            required_stderr="reindex_trusted: permutation length does not match the row count")
    end subroutine test_strings_reindex_trusted_length_mismatch_aborts

    !> copy_buffers writes into caller-supplied arrays, so an undersized one is an out-of-bounds
    !! write that a plain Fortran build does not catch. The two arrays are sized from different
    !! quantities, so each is checked -- and tested -- on its own.
    subroutine test_strings_copy_buffers_offsets_too_short_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "strings_copy_buffers_offsets_too_short", &
            expect_abort=.true., &
            failure_message="an offsets array shorter than size()+1 was expected to abort", &
            required_stderr="copy_buffers: the offsets array is shorter than size()+1")
    end subroutine test_strings_copy_buffers_offsets_too_short_aborts

    !> The payload half of the same check.
    subroutine test_strings_copy_buffers_data_too_short_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "strings_copy_buffers_data_too_short", &
            expect_abort=.true., &
            failure_message="a data array shorter than character_size() was expected to abort", &
            required_stderr="copy_buffers: the data array is shorter than character_size()")
    end subroutine test_strings_copy_buffers_data_too_short_aborts

    subroutine test_settings_env_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        ! The SETTER's own message, not from_env's -- range checking stays in one place.
        call check_scenario_exit_status_and_stderr(error, "settings_env_out_of_range", expect_abort=.true., &
            failure_message="a negative sort thread cap from the environment was expected to abort", &
            required_stderr="parquet_set_sort_threads: n must be >= 0")
    end subroutine test_settings_env_out_of_range_aborts

    subroutine test_settings_env_bad_boolean_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_env_bad_boolean", expect_abort=.true., &
            failure_message="an unaccepted boolean spelling was expected to abort", &
            required_stderr="is not a boolean (expected one of: true, false, 1, 0)")
    end subroutine test_settings_env_bad_boolean_aborts

    subroutine test_settings_set_threads_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        ! The message has to explain WHY 0 is refused here when the sort and prefetch caps accept it,
        ! or it reads as an inconsistency rather than a deliberate rule.
        call check_scenario_exit_status_and_stderr(error, "settings_set_threads_zero", expect_abort=.true., &
            failure_message="parquet_set_threads(0) was expected to abort", &
            required_stderr="parquet_set_threads: n must be >= 1")
    end subroutine test_settings_set_threads_zero_aborts

    subroutine test_settings_negative_prefetch_threads_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_negative_prefetch_threads", expect_abort=.true., &
            failure_message="a negative prefetch thread cap was expected to abort", &
            required_stderr="parquet_set_prefetch_threads: n must be >= 0")
    end subroutine test_settings_negative_prefetch_threads_aborts

    subroutine test_settings_bad_verbosity_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_bad_verbosity", expect_abort=.true., &
            failure_message="an unknown verbosity level was expected to abort", &
            required_stderr="parquet_set_verbosity: unknown level 'quiet'")
    end subroutine test_settings_bad_verbosity_aborts

    subroutine test_settings_bad_stream_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_bad_stream", expect_abort=.true., &
            failure_message="an unknown message stream was expected to abort", &
            required_stderr="parquet_set_message_stream: unknown stream 'logfile'")
    end subroutine test_settings_bad_stream_aborts

    !> Both directions. The "normal" half is the positive control: without it, a helper that
    !> suppressed unconditionally would pass the "errors_only" half perfectly.
    subroutine test_settings_verbosity_gates_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_warning_normal", expect_abort=.false., &
            failure_message="the qc warning scenario was not expected to abort", &
            required_stderr="WARNING: qc violation for column 'v'")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "settings_warning_errors_only", expect_abort=.false., &
            failure_message="the qc warning scenario was not expected to abort", &
            forbidden_text="qc violation for column 'v'")
    end subroutine test_settings_verbosity_gates_warning

    !> Decision 7: "silent" silences even a print the caller explicitly asked for.
    subroutine test_settings_silent_gates_print_stat(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_print_stat_normal", expect_abort=.false., &
            failure_message="the print_stat scenario was not expected to abort", &
            required_stderr="parquet_table:")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "settings_print_stat_silent", expect_abort=.false., &
            failure_message="the print_stat scenario was not expected to abort", &
            forbidden_text="parquet_table:")
    end subroutine test_settings_silent_gates_print_stat

    !> The same gate one layer down, on the READER's printer.
    !>
    !> test_settings_silent_gates_print_stat above exercises %print_stat, which is table_print_stat
    !> -- Fortran (parquet_tables_query.f90). parquet_close_reader(print_stat=.true.) is
    !> parquet_reader_print_stat, C++, with its own output_is_suppressed() call, and nothing
    !> asserted it: deleting that line broke no test before this one existed. Same shape as
    !> test_settings_verbosity_reaches_cpp, which closes the equivalent gap for the warning
    !> channel (feature_risks.md Risk-42).
    subroutine test_settings_silent_gates_reader_print_stat(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "reader_print_stat_normal", expect_abort=.false., &
            failure_message="the reader print_stat scenario was not expected to abort", &
            required_stderr="=== parquet_reader stats ===")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "reader_print_stat_silent", expect_abort=.false., &
            failure_message="the reader print_stat scenario was not expected to abort", &
            forbidden_text="=== parquet_reader stats ===")
    end subroutine test_settings_silent_gates_reader_print_stat

    !> The mirror. Every Fortran-side assertion above passes against a C++ half that ignores the
    !> pushed verbosity entirely, so this is the only test that would catch the two drifting apart
    !> (feature_risks.md Risk-42). The warning provoked here is printed from parquet_wrapper.cpp.
    !> The mirrored settings reach C++ at reader/writer OPEN, not when a setter is called -- so a
    !> knob changed while a reader is already open does not apply to that reader.
    !>
    !> **This is the negative control for that relocation, and it asserts a deliberate behaviour
    !> change rather than merely tolerating one.** `test_settings_verbosity_reaches_cpp` above is
    !> the positive half: set first, then open, and the C++ warning is gone. This is the other half:
    !> open first, then set, and the C++ warning is still there. Together they say the mirror is
    !> taken at open and nowhere else.
    !>
    !> Why it has to exist: push-at-point-of-use is what let every sorting setter follow its state
    !> into `parquet_settings_base`, where `parquet_sorting` can re-export it -- the visibility
    !> requirement behind the module restructuring. Without this control, someone restoring
    !> push-at-set later would see only a test that passes either way, would read the change as a
    !> bug fix, and would silently re-couple those setters to `parquet_bindings`.
    !>
    !> `doc/pages/operating/settings.md` already tells users to apply settings before opening
    !> anything, so nothing that follows the documented contract can observe this.
    subroutine test_settings_mirror_is_taken_at_open(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_cpp_warning_silenced_after_open", &
            expect_abort=.false., &
            failure_message="the qc soft-mode read scenario was not expected to abort", &
            required_stderr="WARNING:")
    end subroutine test_settings_mirror_is_taken_at_open

    subroutine test_settings_verbosity_reaches_cpp(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_cpp_warning_normal", expect_abort=.false., &
            failure_message="the qc soft-mode read scenario was not expected to abort", &
            required_stderr="WARNING:")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "settings_cpp_warning_errors_only", expect_abort=.false., &
            failure_message="the qc soft-mode read scenario was not expected to abort", &
            forbidden_text="WARNING:")
    end subroutine test_settings_verbosity_reaches_cpp

    !> Needs SEPARATED streams: with the merged capture every other helper uses, a message that
    !> moved from stdout to stderr looks identical and this would assert nothing.
    subroutine test_settings_message_stream_moves_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_streams(error, "settings_warning_normal", "WARNING: qc violation", "stdout", &
            "the default message stream should put a warning on stdout")
        if (allocated(error)) return
        call check_scenario_streams(error, "settings_warning_on_stderr", "WARNING: qc violation", "stderr", &
            "message_stream='stderr' should move the warning off stdout")
    end subroutine test_settings_message_stream_moves_warning

    !> The one guarantee neither output setting may break.
    subroutine test_settings_error_survives_silence(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_error_survives_silence", expect_abort=.true., &
            failure_message="opening a nonexistent file was expected to abort", &
            required_stderr="parquet-fortran")
    end subroutine test_settings_error_survives_silence

    subroutine test_read_qc_entry_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_qc_entry_too_long", expect_abort=.true., &
            failure_message="adding a read_qc entry longer than the supported maximum was expected to abort", &
            required_stderr="parquet_read_qc%add: entry exceeds the maximum supported length")
    end subroutine test_read_qc_entry_too_long_aborts

    subroutine test_read_qc_remap_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_qc_remap_size_mismatch", expect_abort=.true., &
            failure_message="a read_qc remap with mismatched from/to sizes was expected to abort", &
            required_stderr="parquet_read_qc%remap_column_names: from and to must have the same size")
    end subroutine test_read_qc_remap_size_mismatch_aborts

    !> As on the filter side, the message must name the REMAPPING, not just the length cap.
    subroutine test_read_qc_remap_entry_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_qc_remap_entry_too_long", expect_abort=.true., &
            failure_message="a read_qc entry that grew past the cap when remapped was expected to abort", &
            required_stderr="supported length after remapping its column name")
    end subroutine test_read_qc_remap_entry_too_long_aborts

    subroutine test_table_qc_violation_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_qc_violation", expect_abort=.true., &
            failure_message="a table opened with a violated qc= bound was expected to abort", &
            required_stderr="qc violation for column")
    end subroutine test_table_qc_violation_aborts

    subroutine test_table_slice_maml_sort_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_slice_maml_sort", expect_abort=.true., &
            failure_message="a slice-regime open whose maml declares extra: sort: was expected to abort", &
            required_stderr="sort is not allowed in the slice regime")
    end subroutine test_table_slice_maml_sort_aborts

    subroutine test_table_filter_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "table_filter_unknown_column", expect_abort=.true., &
            failure_message="a table filtered on a nonexistent column was expected to abort", &
            required_stderr="not_a_real_column")
    end subroutine test_table_filter_unknown_column_aborts

    !> A non-numeric value against a numeric filter column (rule shape is
    !> fine, the value itself isn't) aborts with a message naming the value
    !> and the column, distinct from the shape-only rejection above.
    subroutine test_filter_bad_numeric_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value", expect_abort=.true., &
            failure_message="a non-numeric filter value against a numeric column was expected to abort", &
            required_stderr="filter rule: value 'abc' is not a valid integer for column 'id_with_null'")
    end subroutine test_filter_bad_numeric_value_aborts

    !> See scenario_filter_bad_numeric_value_scoped in error_scenarios.f90 for why this exists
    !> (the same bad-value abort, caught by the row-group-SCOPED filter evaluation path instead of
    !> the unscoped, whole-file one every other filter-error scenario exercises).
    subroutine test_filter_bad_numeric_value_scoped_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value_scoped", expect_abort=.true., &
            failure_message="a non-numeric filter value against a numeric column was expected to abort " // &
            "under a row-group-scoped filter", &
            required_stderr="filter rule: value 'abc' is not a valid integer for column 'id_with_null'")
    end subroutine test_filter_bad_numeric_value_scoped_aborts

    subroutine test_filter_int32_value_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_int32_value_out_of_range", expect_abort=.true., &
            failure_message="an out-of-int32-range filter value was expected to abort", &
            required_stderr="is out of int32 range for column 'id_with_null'")
    end subroutine test_filter_int32_value_out_of_range_aborts

    subroutine test_filter_bad_numeric_value_float_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value_float", expect_abort=.true., &
            failure_message="a non-numeric filter value against a float column was expected to abort", &
            required_stderr="value 'abc' is not a valid number for column 'v'")
    end subroutine test_filter_bad_numeric_value_float_aborts

    !> A string column's filter value must be double-quoted -- a bare,
    !> unquoted word aborts rather than being silently treated as a string.
    subroutine test_filter_unquoted_string_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unquoted_string_value", expect_abort=.true., &
            failure_message="an unquoted value against a string filter column was expected to abort", &
            required_stderr="must be double-quoted")
    end subroutine test_filter_unquoted_string_value_aborts

    !> A boolean column's filter value must be the literal true/false.
    subroutine test_filter_bad_boolean_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_boolean_value", expect_abort=.true., &
            failure_message="an invalid boolean filter value was expected to abort", &
            required_stderr="is not true/false for boolean column")
    end subroutine test_filter_bad_boolean_value_aborts

    !> A double-quoted boolean filter value ('flag == "true"') is rejected too -- distinct from
    !> test_filter_bad_boolean_value_aborts above, which uses an unquoted-but-invalid word.
    subroutine test_filter_boolean_value_must_be_unquoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_boolean_value_must_be_unquoted", &
            expect_abort=.true., &
            failure_message="a double-quoted boolean filter value was expected to abort", &
            required_stderr="must be true or false (unquoted)")
    end subroutine test_filter_boolean_value_must_be_unquoted_aborts

    !> Ordering comparisons (>, >=, <, <=) aren't supported against a boolean
    !> filter column -- only ==//= are.
    subroutine test_filter_bool_ordering_not_supported_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bool_ordering_not_supported", expect_abort=.true., &
            failure_message="an ordering comparison against a boolean filter column was expected to abort", &
            required_stderr="ordering comparisons")
    end subroutine test_filter_bool_ordering_not_supported_aborts

    !> is_nan/is_not_nan only mean something for a column that can hold a NaN, so anything outside
    !> float32/float64/half_float is rejected rather than answered with a constant.
    subroutine test_filter_is_nan_non_float_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_is_nan_non_float_column", expect_abort=.true., &
            failure_message="is_nan against an integer filter column was expected to abort", &
            required_stderr="only supported for floating-point columns")
    end subroutine test_filter_is_nan_non_float_column_aborts

    !> A bare "nan" parses as a number but can never be a meaningful comparison bound, so it is
    !> rejected with a pointer at the operators that say what the caller meant.
    subroutine test_filter_nan_literal_rejected_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_nan_literal_rejected", expect_abort=.true., &
            failure_message="a NaN literal as a filter comparison value was expected to abort", &
            required_stderr="use the 'is_nan'/'is_not_nan' operators instead")
    end subroutine test_filter_nan_literal_rejected_aborts

    !> A valueless operator has to end its clause, so the next bare name is reported as a missing
    !> combinator rather than swallowed as that operator's value. The message is the assertion.
    subroutine test_filter_is_nan_missing_combinator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_is_nan_missing_combinator", expect_abort=.true., &
            failure_message="a clause following is_nan with no combinator was expected to abort", &
            required_stderr="(a missing and/or?)")
    end subroutine test_filter_is_nan_missing_combinator_aborts

    !> eval_filter_clause's `default:` branch -- filtering a column type (temporal) it doesn't
    !> support at all.
    subroutine test_filter_unsupported_column_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unsupported_column_type", expect_abort=.true., &
            failure_message="filtering a binary column was expected to abort", &
            required_stderr="has a type that filtering does not support")
    end subroutine test_filter_unsupported_column_type_aborts

    !> sort_bind_arrow_key's own `return false` fallback -- sorting by a column type (binary) it
    !> doesn't support at all. See scenario_sort_unsupported_column_type in error_scenarios.f90.
    subroutine test_sort_unsupported_column_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_unsupported_column_type", expect_abort=.true., &
            failure_message="sorting by a binary column was expected to abort", &
            required_stderr="has an unsupported column type")
    end subroutine test_sort_unsupported_column_type_aborts

    !> A temporal column is filterable, but only against a double-quoted ISO-8601 literal -- a
    !> bare number would mean different things for a date and a timestamp[us] column.
    subroutine test_filter_temporal_value_not_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_temporal_value_not_quoted", expect_abort=.true., &
            failure_message="an unquoted value against a temporal column was expected to abort", &
            required_stderr="must be a double-quoted ISO-8601 literal")
    end subroutine test_filter_temporal_value_not_quoted_aborts

    !> The eight syntax scenarios below each exercise one rejection in the recursive-descent
    !> parser (parquet_read_filter.f90), which reports every one of them as a clean Fortran
    !> error stop before any column is read.
    subroutine test_filter_unbalanced_parens_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unbalanced_parens", expect_abort=.true., &
            failure_message="an unbalanced '(' was expected to abort", &
            required_stderr="has an unbalanced '('")
    end subroutine test_filter_unbalanced_parens_aborts

    subroutine test_filter_stray_close_paren_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_stray_close_paren", expect_abort=.true., &
            failure_message="a stray ')' was expected to abort", &
            required_stderr="unexpected ')'")
    end subroutine test_filter_stray_close_paren_aborts

    subroutine test_filter_empty_parens_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_empty_parens", expect_abort=.true., &
            failure_message="an empty '()' group was expected to abort", &
            required_stderr="has an empty '()' group")
    end subroutine test_filter_empty_parens_aborts

    subroutine test_filter_dangling_and_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_dangling_and", expect_abort=.true., &
            failure_message="a dangling 'and' was expected to abort", &
            required_stderr="ends after an operator; a clause is missing")
    end subroutine test_filter_dangling_and_aborts

    subroutine test_filter_leading_or_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_leading_or", expect_abort=.true., &
            failure_message="a leading 'or' was expected to abort", &
            required_stderr="has a dangling 'or'")
    end subroutine test_filter_leading_or_aborts

    subroutine test_filter_not_without_operand_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_not_without_operand", expect_abort=.true., &
            failure_message="a 'not' with no operand was expected to abort", &
            required_stderr="ends after an operator; a clause is missing")
    end subroutine test_filter_not_without_operand_aborts

    subroutine test_filter_missing_combinator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_missing_combinator", expect_abort=.true., &
            failure_message="two clauses with no combinator were expected to abort", &
            required_stderr="(a missing and/or?)")
    end subroutine test_filter_missing_combinator_aborts

    subroutine test_filter_nesting_too_deep_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_nesting_too_deep", expect_abort=.true., &
            failure_message="an over-nested filter expression was expected to abort", &
            required_stderr="nested deeper than the supported limit")
    end subroutine test_filter_nesting_too_deep_aborts

    subroutine test_filter_too_many_nodes_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_too_many_nodes", expect_abort=.true., &
            failure_message="a filter expression past the node limit was expected to abort", &
            required_stderr="more terms than the supported limit")
    end subroutine test_filter_too_many_nodes_aborts

    !> A quoted value that is not a valid ISO-8601 literal for the column's own temporal type.
    subroutine test_filter_temporal_bad_iso_literal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_temporal_bad_iso_literal", expect_abort=.true., &
            failure_message="an invalid ISO-8601 temporal literal was expected to abort", &
            required_stderr="is not a valid ISO-8601 date for column")
    end subroutine test_filter_temporal_bad_iso_literal_aborts

    !> Sub-millisecond digits against a timestamp[ms] column: rejected rather than truncated,
    !> since truncating would answer a question about a value the file cannot hold.
    subroutine test_filter_temporal_literal_too_precise_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_temporal_literal_too_precise", expect_abort=.true., &
            failure_message="a temporal literal finer than the column's unit was expected to abort", &
            required_stderr="more precise than that column's stored unit can represent")
    end subroutine test_filter_temporal_literal_too_precise_aborts

    !> parquet_reader_set_filter's two guards: a reader that already decoded a column, and one
    !> that is already filtered.
    subroutine test_filter_set_filter_after_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_filter_after_read", expect_abort=.true., &
            failure_message="setting a filter after reading a column was expected to abort", &
            required_stderr="a column has already been read on this reader")
    end subroutine test_filter_set_filter_after_read_aborts

    subroutine test_filter_set_filter_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_filter_twice", expect_abort=.true., &
            failure_message="setting a second filter was expected to abort", &
            required_stderr="already has an active filter")
    end subroutine test_filter_set_filter_twice_aborts

    !> The six below close the remaining parser/packing rejections: an unterminated quoted value,
    !> an empty rule, the two distinct ')' failures, a name too long for the packed per-leaf
    !> width, and the node cap counted across several %add calls rather than within one rule.
    subroutine test_filter_unterminated_quote_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unterminated_quote", expect_abort=.true., &
            failure_message="an unterminated quoted value was expected to abort", &
            required_stderr="has an unterminated quoted value")
    end subroutine test_filter_unterminated_quote_aborts

    subroutine test_filter_empty_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_empty_rule", expect_abort=.true., &
            failure_message="an empty filter rule was expected to abort", &
            required_stderr="empty filter rule")
    end subroutine test_filter_empty_rule_aborts

    subroutine test_filter_expected_close_paren_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_expected_close_paren", expect_abort=.true., &
            failure_message="a group missing its ')' was expected to abort", &
            required_stderr="expected ')' but found")
    end subroutine test_filter_expected_close_paren_aborts

    subroutine test_filter_close_paren_as_clause_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_close_paren_as_clause", expect_abort=.true., &
            failure_message="a ')' where a clause was expected was expected to abort", &
            required_stderr="has an unbalanced ')'")
    end subroutine test_filter_close_paren_as_clause_aborts

    subroutine test_filter_leaf_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_leaf_too_long", expect_abort=.true., &
            failure_message="an over-long filter column name was expected to abort", &
            required_stderr="filter rule exceeds an internal length limit")
    end subroutine test_filter_leaf_too_long_aborts

    subroutine test_filter_too_many_nodes_across_adds_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_too_many_nodes_across_adds", expect_abort=.true., &
            failure_message="combined %add rules past the node limit were expected to abort", &
            required_stderr="more terms than the supported limit")
    end subroutine test_filter_too_many_nodes_across_adds_aborts

    !> Both ends of the scoped filter's row-group range validation.
    subroutine test_filter_scope_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_scope_out_of_range", expect_abort=.true., &
            failure_message="a filter scoped past the last row group was expected to abort", &
            required_stderr="filter row-group range")
    end subroutine test_filter_scope_out_of_range_aborts

    subroutine test_filter_scope_reversed_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_scope_reversed", expect_abort=.true., &
            failure_message="a reversed row-group scope was expected to abort", &
            required_stderr="filter row-group range")
    end subroutine test_filter_scope_reversed_aborts

    !> "row range", not "row-group range": the two forms are validated against different counts
    !! (the file's rows and its row groups), so the message has to say which one the caller got
    !! wrong. Asserting the shared prefix would pass against either check firing.
    subroutine test_filter_row_range_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_row_range_out_of_range", &
            expect_abort=.true., &
            failure_message="a filter bounded past the file's last row was expected to abort", &
            required_stderr="filter row range 1..999999 is out of range (file has 3 row(s))")
    end subroutine test_filter_row_range_out_of_range_aborts

    !> The strict half of the chunked read path's type handling: numeric kinds convert freely
    !! (asserted by test_read_column_chunk_numeric_conversion in test/test_reading.f90), but a
    !! boolean read still requires a genuinely BOOL column. Without this the converting test would
    !! pass just as happily against a chunk path with no type checking at all.
    subroutine test_chunk_read_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "chunk_read_bool_type_mismatch", &
            expect_abort=.true., &
            failure_message="chunk-reading an int32 column into a logical array was expected to abort", &
            required_stderr="type mismatch for column: v (expected bool, got int32)")
    end subroutine test_chunk_read_bool_type_mismatch_aborts

    !> A physical row range that is individually valid but lies outside the rows its own row-group
    !! range spans. The required text names both ranges and the span, because the two
    !! individually-valid ranges are exactly what makes this mistake hard to see -- an error saying
    !! only "out of range" would be indistinguishable from the two checks above it, both of which
    !! this call passes. Asserting the span (3..6) also pins the fixture's row-group layout, which
    !! is what makes the chosen numbers non-contained in the first place.
    subroutine test_filter_row_range_outside_row_groups_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_row_range_outside_row_groups", &
            expect_abort=.true., &
            failure_message="a filter row range outside its own row groups was expected to abort", &
            required_stderr="filter row range 5..8 is not contained in row groups 2..3, which span rows 3..6")
    end subroutine test_filter_row_range_outside_row_groups_aborts

    !> parquet_reader_set_filter(reader, filt, 0, 0) -- "all row groups" on the memory-bounded
    !! engine. The scenario asserts both that it agrees with the two-argument form's answer and
    !! that it leaves nothing cached where that form caches the filter column, and error stops on
    !! either, so it is expected to exit cleanly here.
    subroutine test_filter_all_row_groups_bounded(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "filter_all_row_groups_bounded", expect_abort=.false., &
            failure_message="row_group_lo=0 should filter every row group without caching the filter column")
    end subroutine test_filter_all_row_groups_bounded

    !> parquet_open_reader's sample_fraction < 0.0 aborts immediately.
    subroutine test_sample_negative_fraction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_negative_fraction", expect_abort=.true., &
            failure_message="a negative sample_fraction was expected to abort", &
            required_stderr="sample_fraction must not be negative")
    end subroutine test_sample_negative_fraction_aborts

    !> parquet_open_reader's sample_fraction NaN aborts immediately -- checked before any relational
    !> comparison, since NaN compares false against every threshold and would otherwise silently
    !> fall through as a no-op (see parquet_open_reader_base's own comment, parquet_read.f90).
    subroutine test_sample_nan_fraction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_nan_fraction", expect_abort=.true., &
            failure_message="a NaN sample_fraction was expected to abort", &
            required_stderr="sample_fraction must not be NaN")
    end subroutine test_sample_nan_fraction_aborts

    !> parquet_reader_print_stat's "sample: fraction=... seed=..." line.
    subroutine test_print_stat_sampled_rows(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_stat_sampled_rows", expect_abort=.false., &
            failure_message="print_stat was expected to run cleanly on a sampled reader", &
            required_stderr="sample: fraction=0.4 seed=42")
    end subroutine test_print_stat_sampled_rows

    !> parquet_reader_set_sample's failure return (parquet_wrapper.cpp) surfaces as a clean
    !> Fortran error stop from parquet_apply_sample (parquet_read.f90), forced here via a
    !> debug-only hook since a genuine BooleanBuilder allocation failure isn't fixture-triggerable.
    subroutine test_sample_mask_build_error_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_mask_build_error", expect_abort=.true., &
            failure_message="a forced sample-mask-build failure was expected to abort", &
            required_stderr="parquet_open_reader: forced debug error: sample mask build failed")
    end subroutine test_sample_mask_build_error_aborts

    !> parquet_reader_set_sample's keep_len guard: the mask Fortran builds must cover exactly the
    !> file's physical rows. Unreachable through the public API (both sides take the count from one
    !> handle), so the scenario forces the guard to expect one row too many -- and runs the same
    !> open with the hook clear first, which is what proves the guard is not firing unconditionally.
    subroutine test_sample_mask_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: saw_control, saw_message

        call run_error_scenario("sample_mask_length_mismatch", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "a sample mask disagreeing with the file's row count was expected to abort")
        if (allocated(error)) return

        ! The negative control comes FIRST, and is the half that gives the abort its meaning: a
        ! guard that fired unconditionally would pass the assertion above just as happily.
        call scenario_capture_contains(out_file, err_file, &
            "control: the sampled open succeeded with the length hook clear", saw_control)
        call check(error, saw_control, &
            "the identical open must succeed with the length hook clear, or the abort below " // &
            "proves only that the guard fires, not that it fires for the right reason")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, &
            "sample mask covers 3 rows but the file has 4", saw_message)
        call check(error, saw_message, &
            "the abort must name both the mask's length and the file's row count")
    end subroutine test_sample_mask_length_mismatch_aborts

    !> parquet_reader_get_string_length's `default:` fallback for a column that isn't
    !> string-like/LIST-typed at all -- reached via a plain `throw`, not report_fatal_error, but
    !> still uncaught: terminates via std::terminate(), not a clean Fortran `error stop`.
    subroutine test_string_length_on_non_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "string_length_on_non_string_column", &
            expect_abort=.true., &
            failure_message="querying string length of a non-string column was expected to abort", &
            required_stderr="Column is not string-like")
    end subroutine test_string_length_on_non_string_column_aborts

    !> qc: miss: must be Null/NA (case-insensitive) or empty -- anything
    !> else is rejected as invalid qc-maml syntax before the parquet file is
    !> even touched.
    subroutine test_qc_maml_bad_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_bad_miss_value", expect_abort=.true., &
            failure_message="an unrecognized qc: miss: value was expected to abort", &
            required_stderr="is not recognized")
    end subroutine test_qc_maml_bad_miss_value_aborts

    !> Two qc-maml fields: entries sharing the same name are ambiguous and
    !> rejected, the same as a schema-authoring maml already rejects a
    !> duplicate field name.
    subroutine test_qc_maml_duplicate_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_duplicate_field", expect_abort=.true., &
            failure_message="a duplicate qc-maml field name was expected to abort", &
            required_stderr="duplicate field name")
    end subroutine test_qc_maml_duplicate_field_aborts

    !> name is the one required attribute for a qc-maml field.
    subroutine test_qc_maml_missing_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_missing_name", expect_abort=.true., &
            failure_message="a qc-maml field missing 'name' was expected to abort", &
            required_stderr="missing required 'name'")
    end subroutine test_qc_maml_missing_name_aborts

    !> parquet_parse_qc_maml reuses the same section/sub-key name schema
    !> every other maml validation path checks -- an unknown qc: sub-key
    !> (here a typo, "minimum" instead of "min") is still caught.
    subroutine test_qc_maml_unknown_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_unknown_subkey", expect_abort=.true., &
            failure_message="an unknown qc: sub-key was expected to abort", &
            required_stderr="unknown sub-key")
    end subroutine test_qc_maml_unknown_subkey_aborts

    !> feature_doc.md point 7's F1 finding: a MAML source line over 1024 characters used to be
    !! silently truncated by a fixed-length read (iostat still 0, no diagnostic). Now aborts,
    !! naming the offending line number, instead of producing incomplete metadata silently.
    subroutine test_maml_line_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "maml_line_too_long", expect_abort=.true., &
            failure_message="a MAML source line exceeding the length limit was expected to abort", &
            required_stderr="exceeds")
    end subroutine test_maml_line_too_long_aborts

    !> get_parquet_maml's `case default` arm -- the one tools/generate_parquet_maml.sh emits into
    !! both of its modes from the same template, so this pins the message a downstream project's
    !! own generated parquet_maml produces too (doc/pages/utilities/embedding-maml-schemas.md).
    !! The scenario looks a real fixture up first, so an implementation that aborted on every name
    !! would fail its control rather than pass this test.
    subroutine test_embedded_maml_unknown_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "embedded_maml_unknown_name", &
            expect_abort=.true., &
            failure_message="an unknown embedded MAML fixture name was expected to abort", &
            required_stderr="get_parquet_maml: unknown internal MAML file: no_such_embedded_schema")
    end subroutine test_embedded_maml_unknown_name_aborts

    !> qc being active must never change the existing strict-by-default Null
    !> behavior: reading a column with a genuine Null and no null_value=/
    !> is_valid= still aborts with the very same message as without any
    !> qc-maml at all.
    subroutine test_qc_existing_null_abort_unchanged_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_existing_null_abort_unchanged", expect_abort=.true., &
            failure_message="a genuine Null with no null_value=/is_valid= was expected to abort even with qc active", &
            required_stderr="column contains Null value(s), which is not supported: id")
    end subroutine test_qc_existing_null_abort_unchanged_aborts

    !> The DEFAULT read-time qc mode (qc_soft=.false., hard): an out-of-range
    !> value aborts the process (via report_fatal_error) rather than merely
    !> warning -- the diagnostic reaches stderr and names the offending column.
    subroutine test_qc_range_violation_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_hard_aborts", expect_abort=.true., &
            failure_message="an out-of-range value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'ra'")
    end subroutine test_qc_range_violation_hard_aborts

    !> Same as test_qc_range_violation_hard_aborts, but proves run_qc_range_check's
    !> STRING/LARGE_STRING/STRING_VIEW branch under hard qc mode.
    subroutine test_qc_range_violation_string_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_string_hard_aborts", &
            expect_abort=.true., &
            failure_message="an out-of-range string value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'sv'")
    end subroutine test_qc_range_violation_string_hard_aborts

    !> Same as test_qc_range_violation_hard_aborts, but proves run_qc_range_check's
    !> FLOAT/DOUBLE/DECIMAL* bounds-description formatting branch under hard qc mode.
    subroutine test_qc_range_violation_float_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_float_hard_aborts", &
            expect_abort=.true., &
            failure_message="an out-of-range float value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'fv'")
    end subroutine test_qc_range_violation_float_hard_aborts

    !> The DEFAULT read-time qc mode (qc_soft=.false., hard): an unexpected
    !> Null aborts even when is_valid= was passed (so the read itself would
    !> otherwise have succeeded).
    subroutine test_qc_null_violation_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_null_violation_hard_aborts", expect_abort=.true., &
            failure_message="an unexpected Null under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'id'")
    end subroutine test_qc_null_violation_hard_aborts

    !> parquet_write_column now catches a row-count mismatch itself and error
    !> stops with a dedicated message, instead of letting Arrow's own
    !> "table.Validate()" exception escape uncaught inside WriteTable.
    subroutine test_write_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_row_count_mismatch", expect_abort=.true., &
            failure_message="writing columns with mismatched row counts was expected to abort", &
            required_stderr="parquet_write_column: row count mismatch for column b: " // &
                "expected 5 rows (from an earlier column) but got 3")
    end subroutine test_write_row_count_mismatch_aborts

    !> parquet_read_column now catches a values-array/file row-count mismatch
    !> itself and error stops with a dedicated message, instead of letting the
    !> C++ side's own "nrows mismatch" check abort the process.
    subroutine test_read_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_row_count_mismatch", expect_abort=.true., &
            failure_message="reading a column into a wrong-size array was expected to abort", &
            required_stderr="parquet_read_column: row count mismatch for column a: " // &
                "file has 5 rows but the values array implies 3")
    end subroutine test_read_row_count_mismatch_aborts

    !> parquet_read_column now checks c_associated(reader%handle) itself and
    !> error stops, instead of dereferencing a null handle (a message-less
    !> SIGSEGV) inside the C++ ConcurrencyGuard.
    subroutine test_read_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_before_open", expect_abort=.true., &
            failure_message="reading from an unopened reader was expected to abort", &
            required_stderr="parquet_read_column: reader has not been opened (call parquet_open_reader first)")
    end subroutine test_read_before_open_aborts

    !> Same as test_read_before_open_aborts, but for the write side.
    subroutine test_write_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_before_open", expect_abort=.true., &
            failure_message="writing to an unopened writer was expected to abort", &
            required_stderr="parquet_write_column: writer has not been opened (call parquet_open_writer first)")
    end subroutine test_write_before_open_aborts

    !> Representative of the same guard now applied to every other
    !> reader-taking procedure (parquet_prefetch_columns, parquet_get_col_size,
    !> parquet_get_column_total_elements, parquet_get_string_length,
    !> parquet_read_array_row_mode, parquet_read_array_element_mode), not just
    !> parquet_read_column/parquet_get_nrows.
    subroutine test_get_nrows_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_nrows_before_open", expect_abort=.true., &
            failure_message="calling parquet_get_nrows on an unopened reader was expected to abort", &
            required_stderr="parquet_get_nrows: reader has not been opened (call parquet_open_reader first)")
    end subroutine test_get_nrows_before_open_aborts

    !> parquet_close_reader now error stops on a reader that was never
    !> opened, instead of silently no-oping.
    subroutine test_close_reader_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_reader_before_open", expect_abort=.true., &
            failure_message="closing a never-opened reader was expected to abort", &
            required_stderr="parquet_close_reader: reader has not been opened, or was already closed")
    end subroutine test_close_reader_before_open_aborts

    !> Same as test_close_reader_before_open_aborts, but for the writer side.
    subroutine test_close_writer_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_before_open", expect_abort=.true., &
            failure_message="closing a never-opened writer was expected to abort", &
            required_stderr="parquet_close_writer: writer has not been opened, or was already closed")
    end subroutine test_close_writer_before_open_aborts

    !> parquet_close_writer's missing-write abort names the schema as
    !> "internal:<table>" for a schema built via parquet_schema(...) (never
    !> loaded from a .maml file) -- see schema_init in src/parquet_metadata.f90
    !> and parquet_close_writer in src/parquet_write.f90.
    !> The two failure classes exit with the two statuses
    !> doc/pages/operating/error-handling.md documents: 1 for a Fortran `error stop`, 134 for a
    !> C++-side failure.
    !!
    !! **Every other assertion in this file checks `exitstat /= 0`.** That is the right question for
    !! "did this abort", and it is why nothing here pinned either number until the error-handling
    !! page started printing them -- at which point they became a contract with no test behind them.
    !! A reader writing a wrapper around this library will branch on these.
    !!
    !! **The negative control is each scenario asserted against the OTHER class's number.** Checking
    !! only that the Fortran one exits 1 would pass just as happily if the C++ path exited 1 too,
    !! and the whole value of the pair is that they DIFFER -- that is what lets a script tell a
    !! precondition failure from an Arrow-side one without parsing stderr.
    !!
    !! **What this does NOT protect**, and the doc-comment says so rather than letting a future
    !! reader assume otherwise: reverting `fatal_exit`'s `std::_Exit(134)` to `std::abort()` would
    !! still exit 134 here, because a single-threaded abort reports the same status. What catches
    !! that is the concurrency scenarios hanging and being killed by the per-scenario timeout cap
    !! (see tools/run_error_scenarios.sh's header). This test pins the user-visible contract, not
    !! the mechanism behind it.
    subroutine test_failure_classes_exit_statuses(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat

        ! Fortran side: an unopened reader, which check_reader_open rejects before anything else.
        call run_error_scenario("read_before_open", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        ! **Nonzero-and-not-134, not `== 1`.** The status a Fortran `error stop` produces is
        ! processor-dependent -- gfortran and flang exit 1, NAG 2 -- so pinning 1 tested the
        ! compiler rather than the library. What the error-handling page actually promises, and what
        ! a script can rely on, is that the two failure classes are distinguishable: the C++ side is
        ! always exactly 134 because `fatal_exit()` calls `std::_Exit(134)`, and the Fortran side is
        ! never 134.
        call check(error, exitstat /= 0, &
            "a Fortran `error stop` must terminate with a nonzero status")
        if (allocated(error)) return
        call check(error, exitstat /= 134, &
            "a Fortran `error stop` must NOT exit 134 -- that is the C++ class, and the page tells " // &
            "readers the two can be told apart by exit status")
        if (allocated(error)) return

        ! C++ side: a type mismatch detected inside parquet_wrapper.cpp, which reaches fatal_exit.
        call run_error_scenario("read_array_full_bool_type_mismatch", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 134, &
            "a C++-side failure must exit with status 134, as the error-handling page documents")
        if (allocated(error)) return
        call check(error, exitstat /= 1, &
            "a C++-side failure must NOT exit 1 -- that is the Fortran class")
    end subroutine test_failure_classes_exit_statuses

    !> Error CONTEXT lines reach stdout even when both output settings are turned against them --
    !> `verbosity="errors_only"`, the strictest level, and `message_stream="stderr"`.
    !!
    !! `parquet_emit_error_context` (src/parquet_settings_base.f90) carries a contract its own
    !! doc-comment states outright -- *"Never suppressed and never redirected"* -- and nothing
    !! tested it. The lines it prints are the ones parquet_close_writer's abort message
    !! deliberately leaves out, the output filename and the schema name, so losing them turns a
    !! diagnosable failure into one that names nothing.
    !!
    !! **This needs check_scenario_streams, not the usual helper.** Every other assertion here goes
    !! through scenario_capture_contains, which searches BOTH captured streams -- so a context line
    !! that started honouring `message_stream` and moved to stderr would look byte-identical to it.
    !! check_scenario_streams asserts the text is on one stream *and absent from the other*, which
    !! is the half with teeth (feature_risks.md Risk-41's decorative-knob rule).
    !!
    !! **The negative control is the warning the scenario emits first.** Under the same two settings
    !! it must reach neither stream. Without it, this test would pass just as happily against a
    !! build where `parquet_set_verbosity` did nothing at all -- it is the warning vanishing that
    !! proves the knobs were really in force while the context lines ignored them.
    subroutine test_error_context_ignores_output_settings(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call check_scenario_streams(error, "close_writer_missing_write_silenced", &
            "parquet_close_writer: output file:", "stdout", &
            "the error-context filename must stay on stdout at verbosity=errors_only, message_stream=stderr")
        if (allocated(error)) return

        ! **The library's own text, NOT the compiler's `ERROR STOP` prefix.** The assertion is that
        ! the abort message goes to stderr whatever `message_stream` says; the prefix in front of it
        ! is the Fortran runtime's and is processor-dependent -- gfortran writes
        ! `ERROR STOP <msg>`, NAG `ERROR STOP: <msg>`, flang `Fortran ERROR STOP: <msg>`. Searching
        ! for gfortran's spelling made this a gfortran-only test that failed on the other two while
        ! the library was behaving correctly.
        call check_scenario_streams(error, "close_writer_missing_write_silenced", &
            "parquet_close_writer: missing write for enabled column", "stderr", &
            "the abort message itself belongs on stderr")
        if (allocated(error)) return

        ! The negative control: a warning emitted under the same settings must reach NEITHER stream.
        call run_error_scenario("close_writer_missing_write_silenced", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control warning that must be suppressed", found)
        call check(error, .not. found, &
            "verbosity=errors_only must suppress the warning -- if it did not, this test proves " // &
            "nothing about the error-context channel ignoring the same setting")
    end subroutine test_error_context_ignores_output_settings

    subroutine test_close_writer_missing_write_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_missing_write", expect_abort=.true., &
            failure_message="closing a writer with an unwritten enabled column was expected to abort", &
            required_stderr="parquet_close_writer: schema: internal:demo")
    end subroutine test_close_writer_missing_write_aborts

    !> Same as test_close_writer_missing_write_aborts, but for a schema built
    !> fully by hand (schema%maml%lines set directly, never going through
    !> %init/parquet_schema(...)) -- the missing-write abort must fall back
    !> to "(unnamed, built in-memory)" since such a schema's %maml%name is
    !> never set.
    subroutine test_close_writer_missing_write_unnamed_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_missing_write_unnamed_schema", &
            expect_abort=.true., &
            failure_message="closing a hand-built-schema writer with an unwritten enabled column " // &
                "was expected to abort", &
            required_stderr="parquet_close_writer: schema: (unnamed, built in-memory)")
    end subroutine test_close_writer_missing_write_unnamed_schema_aborts

    !> A schema-enforced writer closed with NOTHING written no longer aborts -- it writes every
    !> declared column with 0 rows and warns. These three are one group and only mean something
    !> together: the first asserts the new behaviour and its warning, the second is the warning's
    !> negative control (a caller who writes the zero-length arrays himself must get no warning at
    !> all, or the first test would pass against an implementation that warned on every zero-row
    !> close), and the third pins the one case that still aborts.
    !>
    !> Asserted by MESSAGE, not merely by exit status: exit 0 alone would pass against a close that
    !> silently wrote nothing, which is the failure this whole path exists to avoid.
    subroutine test_close_writer_no_columns_written_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_no_columns_written", &
            expect_abort=.false., &
            failure_message="closing a writer with no column written was expected to succeed", &
            required_stderr="parquet_close_writer: no column was written; writing every declared column with 0 rows")
    end subroutine test_close_writer_no_columns_written_warns

    subroutine test_close_writer_zero_length_writes_quiet(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "close_writer_zero_length_writes_quiet", &
            expect_abort=.false., &
            failure_message="explicit zero-length writes were expected to close cleanly", &
            forbidden_text="no column was written")
    end subroutine test_close_writer_zero_length_writes_quiet

    subroutine test_close_writer_no_columns_with_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_no_columns_with_mask", &
            expect_abort=.true., &
            failure_message="a masked writer closed with no column written was expected to abort", &
            required_stderr="parquet_close_writer: missing write for enabled column: col_a")
    end subroutine test_close_writer_no_columns_with_mask_aborts

    !> parquet_read_column now validates the column name against the file's
    !> schema and error stops, instead of letting the C++ side's uncaught
    !> "Column not found" exception abort the process. Also checks the error
    !> names the file being read -- see reader_filename_suffix in
    !> src/parquet_read.f90.
    subroutine test_read_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_unknown_column", expect_abort=.true., &
            failure_message="reading an unknown column via parquet_read_column was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: not_a_real_column " // &
                "(file: test/fixtures/has_null.parquet)")
    end subroutine test_read_unknown_column_aborts

    !> A dotted struct-field path with a mid-path typo ("main.inr.age") is rejected the same way
    !> as any other unknown column name -- check_column_exists catches it before any read is
    !> attempted (struct_path_exists's schema-level walk in parquet_wrapper.cpp), same clean
    !> error_stop message class as test_read_unknown_column_aborts, not a crash.
    subroutine test_read_nested_struct_field_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_field_not_found", expect_abort=.true., &
            failure_message="reading a nested struct-field path with a mid-path typo was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.inr.age " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_field_not_found_aborts

    !> A dotted path where a middle segment resolves to a scalar leaf rather than continuing to
    !> nest ("main.id.extra" -- "id" is int32, not a struct) is rejected as "not found", not a
    !> crash from treating a non-struct array as a StructArray.
    subroutine test_read_nested_struct_path_not_a_struct_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_path_not_a_struct", expect_abort=.true., &
            failure_message="reading a nested struct-field path through a non-struct segment was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.id.extra " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_path_not_a_struct_aborts

    !> A dotted path that resolves exactly to an intermediate STRUCT ("main.inner", not a leaf)
    !> is rejected -- this library has no struct/record output type, so a path must always name a
    !> scalar/vector leaf column (struct_path_exists's terminal-type gate in parquet_wrapper.cpp).
    subroutine test_read_nested_struct_intermediate_not_leaf_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_intermediate_not_leaf", expect_abort=.true., &
            failure_message="reading a nested struct-field path resolving to an intermediate struct was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.inner " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_intermediate_not_leaf_aborts

    !> Confirms (via a process-global disk-read counter, parquet_debug_get_physical_column_read_count
    !> -- see its own comment in parquet_wrapper.cpp) that reading two different leaf paths under
    !> the same physical top-level struct column only triggers one real disk read of that struct --
    !> i.e. struct-path resolution shares get_single_chunk_array's existing column_cache rather
    !> than re-reading per leaf path. This scenario itself error stops if the invariant is
    !> violated, so it is expected to exit cleanly here (expect_abort=.false.).
    subroutine test_nested_struct_shares_cached_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "nested_struct_shares_cached_read", expect_abort=.false., &
            failure_message="reading two leaves under the same struct should share one physical disk read")
    end subroutine test_nested_struct_shares_cached_read

    !> create_parquet_reader (parquet_wrapper.cpp) now checks Arrow's file-open
    !> status directly instead of calling ValueOrDie() unchecked, so a missing
    !> file aborts cleanly with a diagnostic instead of an unconditional abort
    !> with no context.
    subroutine test_open_reader_missing_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "open_reader_missing_file", expect_abort=.true., &
            failure_message="opening a nonexistent file for reading was expected to abort")
    end subroutine test_open_reader_missing_file_aborts

    !> parquet_open_reader(nrows=) implies check_positive=.true., so a filter
    !> matching zero rows must abort at open time, exactly like
    !> parquet_get_nrows(..., check_positive=.true.) would.
    subroutine test_open_reader_nrows_zero_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "open_reader_nrows_zero_rows", expect_abort=.true., &
            failure_message="parquet_open_reader(nrows=) with a filter matching zero rows was expected to abort", &
            required_stderr="parquet_get_nrows: file test_run/open_reader_nrows_zero_rows.parquet " // &
                "has zero rows after filtering (3 total)")
    end subroutine test_open_reader_nrows_zero_rows_aborts

    subroutine test_open_writer_bad_path_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "open_writer_bad_path", expect_abort=.true., &
            failure_message="opening a writer at a path under a nonexistent directory was expected to abort")
    end subroutine test_open_writer_bad_path_aborts

    !> parquet_write_string_matrix_column previously had no array_size check
    !> (unlike the scalar parquet_write_string_column), silently truncating
    !> an over-length string in a fixed-size string matrix column.
    subroutine test_write_string_matrix_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_string_matrix_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string matrix column was expected to error stop")
    end subroutine test_write_string_matrix_exceeds_array_size_aborts

    !> Same check as test_write_string_matrix_exceeds_array_size_aborts, but
    !> for parquet_write_string_column's 1D/flat form (a flattened
    !> string-vector column written as values(:) rather than values(:,:)).
    subroutine test_write_string_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_string_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string vector column " // &
                "(flat form) was expected to error stop")
    end subroutine test_write_string_exceeds_array_size_aborts

    subroutine test_validate_protected_cols_unknown_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_protected_cols_unknown_name", expect_abort=.true., &
            failure_message="protected_cols: referencing a column not declared in fields: " // &
                "was expected to error stop")
    end subroutine test_validate_protected_cols_unknown_name_aborts

    !> A MAML key is case-insensitive, block headers included -- so `Extra:` must be read as the
    !! `extra:` section, and the `protected_cols:` inside it must still be validated. Before this
    !! was fixed the capitalized spelling validated cleanly, because the block was never found:
    !! silent, and it took a column's Null protection with it. See feature_risks.md Risk-91.
    !!
    !! The message is asserted, not only the abort, because "validation rejected this MAML" is a
    !! thing several unrelated defects could also produce -- only naming the unknown column proves
    !! the protected_cols: line itself was read.
    subroutine test_extra_section_capitalized_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extra_section_capitalized", expect_abort=.true., &
            failure_message="a capitalized Extra: section's protected_cols: should still be " // &
                "validated (it was silently ignored before MAML keys became case-insensitive)", &
            required_stderr="protected_cols: unknown column")
    end subroutine test_extra_section_capitalized_aborts

    !> The control for the test above: the identical MAML spelled `extra:`. Asserting only the
    !! capitalized spelling would pass just as happily against a library that had stopped reading
    !! the `extra:` section altogether, so both spellings are asserted to abort the same way.
    subroutine test_extra_section_lowercase_control_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extra_section_lowercase_control", expect_abort=.true., &
            failure_message="the lowercase extra: twin should abort identically -- if it does not, " // &
                "the capitalized test above proves nothing", &
            required_stderr="protected_cols: unknown column")
    end subroutine test_extra_section_lowercase_control_aborts

    !> The compact string path does not enforce a declared array_size, by decision: it stores each
    !! element's own bytes and a reader never needs the declaration to read the column back. The
    !! write is therefore accepted — but not silently, and the metadata it produces describes the
    !! data rather than repeating a declaration the data has outgrown.
    !!
    !! Four assertions off one captured run, and the last two are what make it more than a smoke
    !! test: the warning is emitted ONCE per column (chunk 3's longer 25-character element must not
    !! produce a second one, so "25" must be absent), and the column that stayed within its
    !! declaration must draw no warning at all.
    subroutine test_compact_write_exceeds_array_size_warns(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: saw_completed, saw_warning, saw_second_warning, saw_within

        call run_error_scenario("compact_write_exceeds_array_size_warns", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "a parquet_string_column write longer than the declared array_size must be ACCEPTED, " // &
            "not aborted -- that path does not enforce array_size")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "compact-exceeds-write-completed", saw_completed)
        call check(error, saw_completed, "the scenario did not run to completion")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "element of 20 characters", saw_warning)
        call check(error, saw_warning, &
            "exceeding a declared array_size must emit a WARNING naming the actual element length")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "element of 25 characters", saw_second_warning)
        call check(error, .not. saw_second_warning, &
            "the warning must be emitted once per COLUMN, not once per chunk -- a later, longer " // &
            "chunk (25 characters) must not produce a second one")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'within'", saw_within)
        call check(error, .not. saw_within, &
            "a column that stays within its declared array_size must draw no warning -- the " // &
            "control that rules out warning about every string column")
    end subroutine test_compact_write_exceeds_array_size_warns

    subroutine test_write_protected_column_with_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_protected_column_with_null", expect_abort=.true., &
            failure_message="writing a Null (via is_valid) into a protected column was expected to error stop")
    end subroutine test_write_protected_column_with_null_aborts

    !> The element-granular counterpart: ONE null element of a protected vector column, which
    !> could not reach the check at all until validity stopped being widened to the row.
    subroutine test_write_protected_vector_element_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_protected_vector_element_null", expect_abort=.true., &
            failure_message="one Null ELEMENT of a protected vector column was expected to error stop")
    end subroutine test_write_protected_vector_element_null_aborts

    !> protected_cols: is matched against a column's OUTPUT name -- the name that reaches the file
    !> -- not the internal name Fortran code writes with. Under a col_map: rename the two differ,
    !> which is the only situation where the distinction is observable at all.
    subroutine test_protected_col_map_output_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "protected_col_map_output_name", expect_abort=.true., &
            failure_message="a Null written into a column protected under its col_map: output name " // &
                "was expected to error stop", &
            required_stderr="is protected (extra: protected_cols:) and cannot contain Null values")
    end subroutine test_protected_col_map_output_name_aborts

    !> The negative control for the above, and the more interesting half: listing the INTERNAL
    !> name does not quietly leave the column unprotected -- parquet_validate_maml rejects it,
    !> because every protected_cols: entry must match one of the MAML's own declared output names.
    !> The distinct stderr is what tells the two apart; asserting only the exit status would let a
    !> build that aborted for any reason at all pass both scenarios.
    subroutine test_protected_col_map_internal_name_rejected(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "protected_col_map_internal_name_rejected", &
            expect_abort=.true., &
            failure_message="protected_cols: naming a col_map:-renamed column's internal name was " // &
                "expected to fail validation", &
            required_stderr="protected_cols: unknown column 'internal'")
    end subroutine test_protected_col_map_internal_name_rejected

    !> A protected column may hold no Null however the Null was expressed. A timestamp column
    !> takes no is_valid= at all, and is still covered -- the scenario's null-free control write
    !> runs first, so this cannot pass against a guard that refuses every temporal write.
    subroutine test_write_protected_temporal_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_protected_temporal_null", expect_abort=.true., &
            failure_message="a null element of a protected timestamp column was expected to error stop", &
            required_stderr="is protected (extra: protected_cols:) and cannot contain Null values")
    end subroutine test_write_protected_temporal_null_aborts

    !> The parquet_string_column half of the same rule: a null reaching the file as %append_null,
    !> with no mask argument anywhere. Same control-first shape as the temporal scenario.
    subroutine test_write_protected_string_column_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_protected_string_column_null", &
            expect_abort=.true., &
            failure_message="an %append_null in a protected parquet_string_column was expected to error stop", &
            required_stderr="is protected (extra: protected_cols:) and cannot contain Null values")
    end subroutine test_write_protected_string_column_null_aborts

    !> A streamed column's nullability is fixed by its first row group, so the masked/unmasked
    !> form must not change afterwards. The two directions get separate scenarios and separate
    !> stderr assertions because they fail for different reasons and the messages say so --
    !> asserting only the exit status would let one implementation satisfy both while handling
    !> only one.
    subroutine test_chunk_mask_dropped_after_first_row_group_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "chunk_mask_dropped_after_first_row_group", &
            expect_abort=.true., &
            failure_message="dropping an is_valid mask after the first row group was expected to abort", &
            required_stderr="this row group passes no is_valid mask, but the first row group did")
    end subroutine test_chunk_mask_dropped_after_first_row_group_aborts

    !> The direction that would otherwise write a Null into a field declared non-nullable.
    subroutine test_chunk_mask_added_after_first_row_group_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "chunk_mask_added_after_first_row_group", &
            expect_abort=.true., &
            failure_message="adding an is_valid mask after an unmasked first row group was expected to abort", &
            required_stderr="this row group passes an is_valid mask, but the first row group did not")
    end subroutine test_chunk_mask_added_after_first_row_group_aborts

    !> A typo in schema%set_protected must not silently protect nothing.
    subroutine test_set_protected_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_protected_unknown_column", expect_abort=.true., &
            failure_message="schema%set_protected on an unknown column was expected to abort", &
            required_stderr="no_such")
    end subroutine test_set_protected_unknown_column_aborts

    !> Relaxing a protection warns and does not abort, and the message must not blame a MAML: the
    !! scenario's schema is built entirely with %init/%add_field, so there is no .maml file for the
    !! protection to have come from.
    !!
    !! **Both directions are asserted here, which is why this does not use
    !! check_scenario_exit_status_and_stderr.** That helper only checks a string is PRESENT, and the
    !! interesting half of this guard is the string that must be ABSENT: column 'q' was never
    !! protected, so setting it protected=.false. must say nothing. A guard that warned on every
    !! set_protected(..., .false.) call would satisfy the presence check alone.
    subroutine test_set_protected_unprotect_warns(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_p, warned_q, blamed_maml

        call run_error_scenario("set_protected_unprotect_warns", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "unprotecting a protected column should warn, not abort")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'p' is currently protected", warned_p)
        call check(error, warned_p, "unprotecting the protected column 'p' should have printed a WARNING")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'q'", warned_q)
        call check(error, .not. warned_q, &
            "column 'q' was never protected, so set_protected(q, .false.) must print nothing")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "protected_cols", blamed_maml)
        call check(error, .not. blamed_maml, &
            "the warning must not blame a MAML's extra: protected_cols: -- this schema has no MAML file")
    end subroutine test_set_protected_unprotect_warns

    subroutine test_validate_qc_min_not_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_not_numeric", expect_abort=.true., &
            failure_message="qc: min: value that does not parse as a number was expected to error stop")
    end subroutine test_validate_qc_min_not_numeric_aborts

    subroutine test_validate_qc_max_not_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_max_not_numeric", expect_abort=.true., &
            failure_message="qc: max: value that does not parse as a number was expected to error stop")
    end subroutine test_validate_qc_max_not_numeric_aborts

    subroutine test_validate_qc_min_non_integral_for_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_non_integral_for_int32", expect_abort=.true., &
            failure_message="qc: min: value with a fractional part on an int32 field was expected to error stop")
    end subroutine test_validate_qc_min_non_integral_for_int32_aborts

    subroutine test_validate_qc_min_out_of_int32_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_out_of_int32_range", expect_abort=.true., &
            failure_message="qc: min: value out of int32 range was expected to error stop")
    end subroutine test_validate_qc_min_out_of_int32_range_aborts

    !> The abort must NAME the offending bound, not merely happen: a helper that answered .true.
    !! with the failed read's garbage left in `value` would abort here too -- the garbage is far
    !! outside int64 range as well -- so the message text is what separates the two outcomes.
    subroutine test_validate_qc_min_overflows_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("validate_qc_min_overflows_int64", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "a qc: min: of huge(int64)+1 on an int64 field was expected to error stop")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "invalid qc: min value '9223372036854775808' for data_type int64", found)
        call check(error, found, &
            "the abort should name the bound that does not fit rather than blaming something else")
    end subroutine test_validate_qc_min_overflows_int64_aborts

    subroutine test_validate_qc_min_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_wrong_operator", expect_abort=.true., &
            failure_message="qc: min: with a reversed (</<=) operator was expected to error stop")
    end subroutine test_validate_qc_min_wrong_operator_aborts

    subroutine test_validate_qc_miss_bad_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_qc_miss_bad_value", &
            expect_abort=.true., &
            failure_message="an unrecognized qc: miss: value was expected to error stop, naming the value", &
            required_stderr="invalid qc: miss value 'none' (expected Null/NA or empty)")
    end subroutine test_validate_qc_miss_bad_value_aborts

    !> Negative control for the test above: rejecting every miss: value would pass it.
    subroutine test_validate_qc_miss_valid_values_accepted(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_miss_valid_values", expect_abort=.false., &
            failure_message="the legal qc: miss: forms (Null/null/NA/na/empty/absent) must all still validate")
    end subroutine test_validate_qc_miss_valid_values_accepted

    subroutine test_validate_qc_max_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_max_wrong_operator", expect_abort=.true., &
            failure_message="qc: max: with a reversed (>/>=) operator was expected to error stop")
    end subroutine test_validate_qc_max_wrong_operator_aborts

    subroutine test_qc_maml_min_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_maml_min_wrong_operator", expect_abort=.true., &
            failure_message="qc-maml qc: min: with a reversed (</<=) operator was expected to error stop")
    end subroutine test_qc_maml_min_wrong_operator_aborts

    subroutine test_qc_maml_max_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_maml_max_wrong_operator", expect_abort=.true., &
            failure_message="qc-maml qc: max: with a reversed (>/>=) operator was expected to error stop")
    end subroutine test_qc_maml_max_wrong_operator_aborts

    subroutine test_schema_add_field_qc_max_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_max_operator_without_value", expect_abort=.true., &
            failure_message="schema%add_field with a qc_max operator but no value was expected to error stop")
    end subroutine test_schema_add_field_qc_max_operator_without_value_aborts

    subroutine test_add_col_qc_min_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_min_reversed_operator", expect_abort=.true., &
            failure_message="add_col_qc with a reversed min operator was expected to error stop")
    end subroutine test_add_col_qc_min_reversed_operator_aborts

    subroutine test_add_col_qc_max_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_max_reversed_operator", expect_abort=.true., &
            failure_message="add_col_qc with a reversed max operator was expected to error stop")
    end subroutine test_add_col_qc_max_reversed_operator_aborts

    subroutine test_add_col_qc_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_operator_without_value", expect_abort=.true., &
            failure_message="add_col_qc with an operator but no value was expected to error stop")
    end subroutine test_add_col_qc_operator_without_value_aborts

    subroutine test_add_col_qc_max_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_max_operator_without_value", expect_abort=.true., &
            failure_message="add_col_qc with a max operator but no value was expected to error stop")
    end subroutine test_add_col_qc_max_operator_without_value_aborts

    subroutine test_add_col_qc_bad_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_bad_miss_value", expect_abort=.true., &
            failure_message="add_col_qc with an invalid miss value was expected to error stop")
    end subroutine test_add_col_qc_bad_miss_value_aborts

    subroutine test_add_col_qc_too_many_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_too_many_fields", expect_abort=.true., &
            failure_message="add_col_qc with more than four fields was expected to error stop")
    end subroutine test_add_col_qc_too_many_fields_aborts

    subroutine test_add_col_qc_empty_column_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_empty_column_name", expect_abort=.true., &
            failure_message="add_col_qc with an empty column name was expected to error stop")
    end subroutine test_add_col_qc_empty_column_name_aborts

    subroutine test_add_col_qc_duplicate_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_aborts

    !> A pre-existing "- name: 'dup'" entry (single-quoted) must still be
    !> recognized as declaring "dup" -- the duplicate-name check strips a
    !> single pair of surrounding quotes before comparing.
    subroutine test_add_col_qc_duplicate_column_single_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column_single_quoted", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name behind a single-quoted " // &
                "existing entry was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_single_quoted_aborts

    !> Same as above, for a double-quoted existing entry ('- name: "dup"').
    subroutine test_add_col_qc_duplicate_column_double_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column_double_quoted", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name behind a double-quoted " // &
                "existing entry was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_double_quoted_aborts

    subroutine test_set_col_qc_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_col_qc_reversed_operator", expect_abort=.true., &
            failure_message="set_col_qc with a reversed min operator was expected to error stop")
    end subroutine test_set_col_qc_reversed_operator_aborts

    !> Also checks the error now names the column, output file and maml --
    !> previously it named none of those (see writer_context_suffix in
    !> src/parquet_write.f90).
    subroutine test_write_values_not_divisible_by_col_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_values_not_divisible_by_col_size", expect_abort=.true., &
            failure_message="writing a values(:) array whose length isn't divisible by col_size was expected to error stop", &
            required_stderr="parquet_write_int32_column: values size is not divisible by col_size for column v " // &
                "(file: test_run/error_scenario_col_size_mismatch.parquet, maml: col_size_mismatch.maml)")
    end subroutine test_write_values_not_divisible_by_col_size_aborts

    subroutine test_write_int64_to_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_int64_to_int32_overflow", expect_abort=.true., &
            failure_message="writing an out-of-int32-range int64 value to an int32 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: int64 value out of int32 range for column v")
    end subroutine test_write_int64_to_int32_overflow_aborts

    subroutine test_write_float_to_int32_non_integral_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int32_non_integral", expect_abort=.true., &
            failure_message="writing a non-integral float64 value to an int32 schema column was expected to error stop", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_to_int32_non_integral_aborts

    subroutine test_write_float_to_int32_non_integral_and_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        ! Pins the ORDER of the two checks, which neither single-condition scenario can see:
        ! the value violates both, and integrality is tested first, so this exact message is
        ! what a caller gets. Asserting the range message here instead would pass against a
        ! swapped implementation, which is the whole point of the test.
        call check_scenario_exit_status_and_stderr(error, "write_float_to_int32_non_integral_and_out_of_range", &
            expect_abort=.true., &
            failure_message="a float64 value that is both non-integral and out of int32 range was expected " // &
                "to error stop naming the non-integral check, which is the one performed first", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_to_int32_non_integral_and_out_of_range_aborts

    subroutine test_write_float_nan_to_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_nan_to_int32", expect_abort=.true., &
            failure_message="writing a NaN to an int32 schema column was expected to error stop", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_nan_to_int32_aborts

    subroutine test_write_float_to_int64_negative_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int64_negative_out_of_range", &
            expect_abort=.true., &
            failure_message="a large NEGATIVE out-of-int64-range float64 value was expected to error stop " // &
                "naming the range check; reporting the integrality check instead means the integrality " // &
                "test lost its abs() and ran an out-of-range int conversion", &
            required_stderr="parquet_write_column: float value out of int64 range for column v")
    end subroutine test_write_float_to_int64_negative_out_of_range_aborts

    subroutine test_write_float_to_int32_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int32_out_of_range", expect_abort=.true., &
            failure_message="writing an out-of-int32-range float64 value to an int32 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: float value out of int32 range for column v")
    end subroutine test_write_float_to_int32_out_of_range_aborts

    subroutine test_write_float_to_int64_non_integral_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int64_non_integral", expect_abort=.true., &
            failure_message="writing a non-integral float64 value to an int64 schema column was expected to error stop", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_to_int64_non_integral_aborts

    subroutine test_write_float_to_int64_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int64_out_of_range", expect_abort=.true., &
            failure_message="writing an out-of-int64-range float64 value to an int64 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: float value out of int64 range for column v")
    end subroutine test_write_float_to_int64_out_of_range_aborts

    subroutine test_set_max_threads_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_max_threads_below_one", expect_abort=.true., &
            failure_message="parquet_set_arrow_threads(0) was expected to error stop")
    end subroutine test_set_max_threads_below_one_aborts

    !> These two concurrency tests are self-adapting: the race they check can
    !> only occur when this build genuinely runs multi-threaded (OpenMP flag
    !> supplied via FPM_FFLAGS, and more than one thread available). When it
    !> doesn't -- plain `fpm test` with no OpenMP flag, or OMP_NUM_THREADS=1 --
    !> the shared-reader/writer race cannot happen, the guard cannot fire, and
    !> running the scenario would just report a misleading failure. So we skip
    !> (pass trivially) when omp_get_max_threads() <= 1, and only assert the
    !> abort when real concurrency is available. The subprocess spawned by the
    !> scenario inherits the same build/environment, so its thread count
    !> matches what we observe here.
    subroutine test_concurrent_calls_into_shared_reader_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) return   ! no real concurrency -> skip (pass)

        ! README's Thread safety section explicitly promises a diagnostic on
        ! stderr for this case, not just a bare abort -- check that promise
        ! from the same run as the exit-status check, rather than re-running
        ! the (race-dependent) scenario a second time.
        call check_scenario_exit_status_and_stderr(error, "concurrent_calls_into_shared_reader", &
            expect_abort=.true., &
            failure_message="concurrent parquet_read_column calls into one shared reader were expected to abort", &
            required_stderr="concurrent access to a single parquet_reader detected")
    end subroutine test_concurrent_calls_into_shared_reader_aborts

    subroutine test_concurrent_calls_into_shared_writer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) return   ! no real concurrency -> skip (pass)

        call check_scenario_exit_status_and_stderr(error, "concurrent_calls_into_shared_writer", &
            expect_abort=.true., &
            failure_message="concurrent parquet_write_column calls into one shared writer were expected to abort", &
            required_stderr="concurrent access to a single parquet_writer detected")
    end subroutine test_concurrent_calls_into_shared_writer_aborts

    !> The negative control for the two tests above: a guard that fired unconditionally would pass
    !> both of them while making the library unusable. This hands one writer between two threads
    !> sequentially and requires it to SUCCEED -- which is also what covers the release of the
    !> writer's guard on the early-RETURN paths, since a leaked claim is only ever observable from a
    !> second thread (see scenario_writer_guard_sequential_handoff for the full reasoning).
    subroutine test_writer_guard_sequential_handoff_succeeds(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) return   ! no second thread to hand off to -> skip (pass)

        call check_scenario_exit_status(error, "writer_guard_sequential_handoff", &
            expect_abort=.false., &
            failure_message="a sequential hand-off of one writer between two threads was expected to succeed")
    end subroutine test_writer_guard_sequential_handoff_succeeds

    !> Resolves the path to the built error_scenarios helper binary once and caches it for the
    !> rest of this process.
    !>
    !> **The fast path is argument 0.** fpm launches a test target through a project-root-relative
    !> path (`build/gfortran_<hash>/test/run_tester` -- confirm with
    !> `fpm test run_tester --runner echo`), and the sibling `error_scenarios` target sits in that
    !> same directory. fpm builds every test target before running any of them, so that binary is
    !> always exactly as fresh as the one asking for it -- which is what makes the `fpm build
    !> --tests` this used to run on every startup (measured 1.8-2.1s even fully up to date) pure
    !> waste. Nothing in test/ chdirs, so the relative path stays valid for the whole run, and a
    !> coverage build under its own FPM_BUILD_DIR is tracked for free: argument 0 names whichever
    !> tree this process was actually launched from.
    !>
    !> **The fallback is the old build-and-find**, kept for when argument 0 cannot answer --
    !> someone running the built run_tester directly rather than through fpm, or an unusual
    !> runner. Note what it costs: `find ... | head -n 1` picks whichever `build/gfortran_<hash>/`
    !> tree the filesystem returns first, which is a guess as soon as more than one exists (see
    !> CLAUDE.md's "Stale `fpm` build cache"). Avoiding that guess is half the reason the
    !> argument-0 path is primary -- do not demote it back.
    !>
    !> Written as a subroutine with an allocatable `character` argument rather than a function
    !> returning one, per CLAUDE.md's "Build & compiler notes": gfortran's codegen for receiving
    !> such a function result is not reliably thread-safe, and this is reachable from suites that
    !> test-drive runs concurrently.
    subroutine get_error_scenarios_bin(bin)
        character(len=:), allocatable, intent(out) :: bin
        character(len=*), parameter :: path_file = "test_run/.error_scenarios_bin_path"
        character(len=:), allocatable :: cand
        integer :: unit, ios, cstat, arglen, argstat, slash
        character(len=4096) :: arg0
        character(len=1024) :: line
        logical :: exists

        if (.not. g_error_scenarios_bin_ready) then
            g_error_scenarios_bin = ""

            arg0 = ""
            call get_command_argument(0, arg0, arglen, argstat)
            if (argstat == 0 .and. arglen > 0 .and. arglen <= len(arg0)) then
                slash = index(arg0(1:arglen), "/", back=.true.)
                if (slash > 0) then
                    cand = arg0(1:slash) // "error_scenarios"
                    inquire(file=cand, exist=exists)
                    if (exists) g_error_scenarios_bin = cand
                end if
            end if

            if (len_trim(g_error_scenarios_bin) == 0) then
                ! Every call below passes cmdstat= even though its value is never
                ! inspected: a processor is required to initiate ERROR TERMINATION
                ! when a condition that would set cmdstat nonzero occurs and the
                ! argument is absent, and flang classifies "the command exited
                ! nonzero" as exactly such a condition. Without cmdstat=, a
                ! non-clean `fpm build` here would kill the whole test binary with
                ! an opaque runtime message instead of failing a check.
                call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cstat)
                call execute_command_line("fpm build --tests > /dev/null 2>&1", wait=.true., cmdstat=cstat)
                call execute_command_line( &
                    "find ""${FPM_BUILD_DIR:-build}"" -type f -name error_scenarios 2>/dev/null | head -n 1 > "// &
                    path_file, &
                    wait=.true., cmdstat=cstat)
                line = ""
                open(newunit=unit, file=path_file, status="old", action="read", iostat=ios)
                if (ios == 0) then
                    read(unit, '(a)', iostat=ios) line
                    close(unit)
                end if
                g_error_scenarios_bin = trim(adjustl(line))
            end if
            g_error_scenarios_bin_ready = .true.
        end if
        bin = g_error_scenarios_bin
    end subroutine get_error_scenarios_bin

    !> Runs every scenario named in tools/run_error_scenarios.sh's `scenarios=(...)` array ONCE,
    !> up front and in parallel, capturing each one's exit status and its two output streams under
    !> prime_dir. run_error_scenario then answers from those files instead of spawning.
    !>
    !> **Why this exists.** Measured on an 8-core machine: the ~630 scenario subprocesses a full
    !> `fpm test` drives account for ~60s of its ~72s, and they run strictly one at a time --
    !> the four suites that drive them are excluded from test-drive's own parallelism (see
    !> run_tester.f90's suite_is_safe_to_parallelize; forking from inside a live OpenMP region is
    !> unsafe under libiomp5). The same 686 scenarios through `xargs -P8` take 15.5s. This routine
    !> buys that without touching the exclusion: it makes exactly ONE execute_command_line call,
    !> from run_tester before any suite starts, i.e. with no OpenMP team active at all. The
    !> parallelism happens in the shell, outside this process entirely.
    !>
    !> **Every failure here degrades to the old behaviour, never to a wrong answer.** A scenario
    !> absent from the list, a shell that could not run, a missing capture file -- each simply
    !> leaves run_error_scenario spawning that scenario itself. That is what keeps a stale list
    !> costing speed rather than coverage. `PARQUET_TEST_NO_PRIME=1` disables priming entirely
    !> (for debugging one scenario without 686 others running first);
    !> `PARQUET_TEST_PRIME_JOBS=<n>` overrides the concurrency, which defaults to one per CPU.
    !>
    !> **prime_dir is wiped here, not reused.** A triple left behind by an earlier run would
    !> otherwise be consumed as this run's result: every affected test would then pass while
    !> asserting against a binary that no longer exists -- a vacuous pass, the failure mode this
    !> project treats as the worst kind. g_prime_ok is likewise per-process and set only after
    !> this routine has repopulated the directory itself.
    subroutine prime_error_scenarios()
        character(len=*), parameter :: dq = '"'
        character(len=:), allocatable :: bin, cmd
        character(len=8) :: off
        integer :: cstat, length, status
        logical :: exists

        if (g_prime_done) return
        g_prime_done = .true.

        off = ""
        call get_environment_variable("PARQUET_TEST_NO_PRIME", off, length, status)
        if (length > 0) return

        inquire(file=scenario_list_file, exist=exists)
        if (.not. exists) return

        call get_error_scenarios_bin(bin)
        if (len_trim(bin) == 0) return

        call execute_command_line("rm -rf " // prime_dir // " && mkdir -p " // prime_dir, &
            wait=.true., cmdstat=cstat)
        if (cstat /= 0) return

        ! `xargs -n 1 sh -c '<script>'` puts the one scenario name in the inner shell's $0 -- the
        ! `-I` replacement form is deliberately avoided, since BSD xargs caps a constructed
        ! argument at 255 bytes and this script plus a long scenario name substituted four times
        ! would cross it (on CI's GNU xargs it would not, so the failure would be macOS-only).
        ! `exit 0` is equally load-bearing: most scenarios die by SIGABRT, and BSD xargs stops
        ! dispatching entirely the moment a child is killed by a signal.
        ! A per-scenario wall-clock cap, resolved once here and exported so the inner `sh -c`
        ! can use it. Without it one wedged scenario stalls the whole priming step and, with
        ! it, every suite that follows -- observed when a concurrency scenario hung inside
        ! glibc's abort() under ifx at -O0. SIGKILL rather than the default SIGTERM, because
        ! the Fortran runtimes catch SIGTERM to print a traceback and a wedged process cannot
        ! run that handler either. An absent `timeout` (stock macOS has none; coreutils calls
        ! it `gtimeout`) leaves the variable empty, which runs the scenario unguarded rather
        ! than breaking the run. `timeout` reports 124 when it fires, which is the sentinel
        ! check_scenario_exit_status and its siblings key on.
        !
        ! **The probe RUNS the command rather than asking whether it exists, and the difference
        ! is not academic.** MacPorts ships a BSD-syntax `/opt/local/bin/timeout` whose usage is
        ! `timeout [-signal] time command` -- it exists, so `command -v timeout` succeeds, and it
        ! then rejects `-s KILL`. Every scenario's captured stderr becomes
        ! "usage: timeout [-signal] time command..." with status 1, which is indistinguishable
        ! from a scenario that really did print that and exit 1: the fallback never fires, and
        ! 541 of 664 error-scenario tests fail with messages blaming the library. Probing with
        ! the real flags falls through to `gtimeout` (GNU coreutils) instead, and covers the
        ! absent case for free, since a missing command also exits nonzero.
        cmd = "PF_SCENARIO_TIMEOUT=''; " // &
            "if timeout -s KILL 1 true >/dev/null 2>&1; then PF_SCENARIO_TIMEOUT=" // dq // &
            "timeout -s KILL ${PARQUET_SCENARIO_TIMEOUT:-120}" // dq // "; " // &
            "elif gtimeout -s KILL 1 true >/dev/null 2>&1; then PF_SCENARIO_TIMEOUT=" // dq // &
            "gtimeout -s KILL ${PARQUET_SCENARIO_TIMEOUT:-120}" // dq // "; fi; " // &
            "export PF_SCENARIO_TIMEOUT; "
        cmd = trim(cmd) // " sed -n '/^scenarios=(/,/^)/p' " // scenario_list_file // &
            " | grep -oE '" // dq // "[a-z0-9_]+:[01]" // dq // "' | tr -d '" // dq // "' | cut -d: -f1" // &
            " | xargs -P " // dq // "${PARQUET_TEST_PRIME_JOBS:-" // &
            "$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}" // dq // " -n 1 sh -c '" // &
            "${PF_SCENARIO_TIMEOUT} " // trim(bin) // " " // dq // "$0" // dq // " > " // prime_dir // "/" // &
            dq // "$0" // dq // ".out" // &
            " 2> " // prime_dir // "/" // dq // "$0" // dq // ".err" // &
            " ; echo $? > " // prime_dir // "/" // dq // "$0" // dq // ".status ; exit 0'"
        call execute_command_line(cmd, wait=.true., cmdstat=cstat)
        if (cstat /= 0) return

        g_prime_ok = .true.
    end subroutine prime_error_scenarios

    !> Reads back the `echo $?` status file written by prime_error_scenarios or by
    !> run_error_scenario's own spawn. Reports -1 when the file is absent or unreadable, which
    !> both callers treat as "this run did not happen" rather than as an exit status.
    subroutine read_scenario_status(path, status_value)
        character(len=*), intent(in) :: path
        integer, intent(out) :: status_value
        integer :: unit, ios

        status_value = -1
        open(newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        read(unit, *, iostat=ios) status_value
        close(unit)
        if (ios /= 0) status_value = -1
    end subroutine read_scenario_status

    !> Searches BOTH captured streams for `text`.
    !>
    !> This replaces the old `2>&1` merge. Both streams are now always captured separately --
    !> priming has to, and keeping the primed and spawned paths byte-identical in shape is what
    !> stops the two drifting -- so a caller wanting the old "appeared somewhere" semantics asks
    !> about both files instead of one merged one. Line-oriented searching makes the two
    !> equivalent: every line comes from exactly one stream either way.
    subroutine scenario_capture_contains(out_file, err_file, text, found)
        character(len=*), intent(in) :: out_file, err_file, text
        logical, intent(out) :: found
        logical :: on_err

        call file_contains(out_file, text, found)
        if (found) return
        call file_contains(err_file, text, on_err)
        found = on_err
    end subroutine scenario_capture_contains

    !> Reports `scenario`'s exit status and the paths its stdout and stderr were captured to.
    !>
    !> **Answers from prime_error_scenarios' pre-run results when this process primed them**, and
    !> otherwise spawns the helper binary itself exactly as it always did. A caller cannot tell
    !> the two apart and must not try to: the fallback is what keeps a scenario missing from the
    !> prime list working rather than failing.
    !>
    !> `out_file`/`err_file` are OUT arguments rather than something the caller names, because the
    !> two paths differ -- a primed capture lives under prime_dir and must not be rewritten. Both
    !> streams are always captured separately; a caller wanting the old merged `2>&1` view
    !> searches both with scenario_capture_contains.
    !>
    !> **The child's status is captured through the shell (`; echo $? > file`)
    !> rather than read from execute_command_line's own EXITSTAT/CMDSTAT,
    !> because neither of those two is portable across compilers here.** Every
    !> scenario this drives is expected either to exit cleanly or to terminate
    !> abnormally, so the disagreement is not an edge case -- it is the normal
    !> path:
    !>
    !> - **CMDSTAT.** gfortran documents cmdstat == 0 whenever the command line
    !>   was executed, whatever its exit status was; flang instead treats a
    !>   nonzero exit as an "error condition" and reports cmdstat 5/6/7
    !>   (not-found / nonzero exit / killed by signal). Read literally, every
    !>   `expect_abort=.true.` scenario then looks like a failure to invoke the
    !>   binary at all.
    !> - **EXITSTAT.** For a child killed by a signal -- how report_fatal_error's
    !>   std::abort() ends a scenario -- gfortran reports the signal number
    !>   while flang reports 0. Trusting it there would silently turn an
    !>   abort-expecting test into a false PASS, which is worse than the loud
    !>   cmdstat failure.
    !>
    !> `$?` from the shell answers both uniformly: the scenario's own `error
    !> stop` code, or 128+signo for a signal death, on any compiler. The
    !> composed command's own exit status is then always that of `echo`, i.e.
    !> 0, so cmdstat is 0 everywhere and this subroutine's reported cmdstat
    !> goes back to meaning only what its callers actually test it for -- "the
    !> helper binary could not be invoked". No new dependency on a POSIX shell
    !> is introduced: the redirections, the `${FPM_BUILD_DIR:-build}` expansion
    !> and the `find | head` pipeline above are already shell syntax.
    !>
    !> Every file this touches is keyed on the scenario name, which is unique across
    !> every call site (the same assumption check_scenario_exit_status_and_stderr
    !> already makes for its own capture) -- so this
    !> stays safe under test-drive's concurrent execution of a suite.
    subroutine run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        character(len=*), intent(in) :: scenario
        integer, intent(out) :: exitstat, cmdstat
        character(len=:), allocatable, intent(out) :: out_file, err_file
        character(len=:), allocatable :: bin, status_file
        integer :: unit, ios, ecl_exit, ecl_cmd, status_value
        logical :: have_out, have_err, have_status

        ! Default to "could not invoke": every success path below has to say so
        ! explicitly, so a new early return can't accidentally report a pass.
        exitstat = -1
        cmdstat = 1

        ! Primed result, if this process produced one for this scenario. Anything missing or
        ! unreadable falls through to the spawn below rather than failing -- see
        ! prime_error_scenarios for why that direction is the only safe one.
        if (g_prime_ok) then
            out_file = prime_dir // "/" // trim(scenario) // ".out"
            err_file = prime_dir // "/" // trim(scenario) // ".err"
            status_file = prime_dir // "/" // trim(scenario) // ".status"
            inquire(file=out_file, exist=have_out)
            inquire(file=err_file, exist=have_err)
            inquire(file=status_file, exist=have_status)
            if (have_out .and. have_err .and. have_status) then
                call read_scenario_status(status_file, status_value)
                if (status_value >= 0 .and. status_value /= 127) then
                    exitstat = status_value
                    cmdstat = 0
                    return
                end if
            end if
        end if

        out_file = "test_run/" // trim(scenario) // "_o.txt"
        err_file = "test_run/" // trim(scenario) // "_e.txt"
        status_file = "test_run/." // trim(scenario) // "_status.txt"

        call get_error_scenarios_bin(bin)
        if (len_trim(bin) == 0) return

        ! Drop any status file left behind by an earlier run: if the command
        ! below fails to execute at all, a stale file would otherwise be read
        ! back as this run's result.
        open(newunit=unit, file=status_file, status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")

        call execute_command_line( &
            trim(bin)//" "//trim(scenario)//" > "//out_file//" 2> "//err_file// &
            " ; echo $? > "//status_file, &
            wait=.true., exitstat=ecl_exit, cmdstat=ecl_cmd)

        call read_scenario_status(status_file, status_value)

        ! No status written -> the shell itself never ran the command.
        if (status_value < 0) return
        ! 127 is the shell's own "command not found", i.e. a bad/stale binary
        ! path rather than a scenario that aborted. No scenario exits 127 --
        ! error_scenarios.f90 uses only `error stop` (1) and the 97 sentinel --
        ! so this can be attributed to invocation without ambiguity.
        if (status_value == 127) return

        exitstat = status_value
        cmdstat = 0
    end subroutine run_error_scenario

    subroutine check_scenario_exit_status(error, scenario, expect_abort, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: aborted

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        ! exitstat == 97 means error_scenarios.f90's `case default` was hit --
        ! i.e. `scenario` doesn't match any case there (a typo, or a case
        ! renamed on one side but not the other). Distinguishing this from a
        ! genuine abort (which this scenario name should never produce) is
        ! the whole point of that distinctive exit code -- otherwise this
        ! check could pass "by accident" while silently testing nothing.
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        ! 124 and 137 are what `timeout` reports when the per-scenario cap fires: the scenario
        ! neither finished nor aborted. Both have to be singled out, because every check below
        ! reads a nonzero exit as "aborted" -- so a hung scenario would otherwise PASS as a
        ! successful abort, which is the failure mode the cap exists to expose rather than hide.
        ! 124 is the documented code; 137 (128+9) is what comes back instead when the cap kills
        ! with SIGKILL, which is what this harness asks for because the Fortran runtimes catch
        ! SIGTERM and a wedged process cannot run that handler either. Keying on 124 alone was
        ! tried and let a forced timeout report PASS.
        call check(error, exitstat /= 124 .and. exitstat /= 137, &
            "scenario TIMED OUT and was killed (neither finished nor aborted): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
    end subroutine check_scenario_exit_status

    !> Like check_scenario_exit_status, but also asserts stderr (captured
    !> from the same single run) contains `required_stderr`. Used only where
    !> the diagnostic text itself is an explicit documented guarantee (e.g.
    !> the concurrency guard's stderr message in the README's Thread safety
    !> section) -- not applied broadly to every scenario, since most
    !> diagnostic wording isn't a documented contract and shouldn't be
    !> locked down by regression tests.
    subroutine check_scenario_exit_status_and_stderr(error, scenario, expect_abort, failure_message, required_stderr)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message, required_stderr
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: aborted, found

        ! The assertion below runs over BOTH captured streams, despite this procedure's name.
        ! That is deliberate and is what ~390 call sites want ("this text appeared somewhere"),
        ! but it means this helper cannot tell the two streams apart -- a message moved from one
        ! to the other looks identical to it. Anything asserting about a specific stream needs
        ! check_scenario_streams below instead.
        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        ! See check_scenario_exit_status's identical check for why: exit
        ! code 97 means the scenario name itself was not recognized.
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        ! 124 and 137 are what `timeout` reports when the per-scenario cap fires: the scenario
        ! neither finished nor aborted. Both have to be singled out, because every check below
        ! reads a nonzero exit as "aborted" -- so a hung scenario would otherwise PASS as a
        ! successful abort, which is the failure mode the cap exists to expose rather than hide.
        ! 124 is the documented code; 137 (128+9) is what comes back instead when the cap kills
        ! with SIGKILL, which is what this harness asks for because the Fortran runtimes catch
        ! SIGTERM and a wedged process cannot run that handler either. Keying on 124 alone was
        ! tried and let a forced timeout report PASS.
        call check(error, exitstat /= 124 .and. exitstat /= 137, &
            "scenario TIMED OUT and was killed (neither finished nor aborted): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, required_stderr, found)

        call check(error, found, &
            "expected stderr to contain '" // trim(required_stderr) // "' for scenario '" // trim(scenario) // "'")
    end subroutine check_scenario_exit_status_and_stderr

    !> Runs `scenario` capturing stdout and stderr SEPARATELY, and asserts `text` appears on the
    !> stream named by `expect_on` and is absent from the other.
    !>
    !> The absence half is the entire point. Every other helper here redirects with `2>&1`, so a
    !> message that moved from stdout to stderr produces a byte-identical capture -- a
    !> `message_stream` test written against those helpers passes whether the setting works or is
    !> ignored completely, which is the decorative-knob failure feature_risks.md Risk-41 exists to
    !> forbid. Asserting presence alone here would have the same hole, since the message is present
    !> either way; it is asserting it is *gone from the other stream* that has teeth.
    subroutine check_scenario_streams(error, scenario, text, expect_on, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, text, expect_on, failure_message
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: on_out, on_err, wanted, unwanted

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        ! 124 and 137 are what `timeout` reports when the per-scenario cap fires: the scenario
        ! neither finished nor aborted. Both have to be singled out, because every check below
        ! reads a nonzero exit as "aborted" -- so a hung scenario would otherwise PASS as a
        ! successful abort, which is the failure mode the cap exists to expose rather than hide.
        ! 124 is the documented code; 137 (128+9) is what comes back instead when the cap kills
        ! with SIGKILL, which is what this harness asks for because the Fortran runtimes catch
        ! SIGTERM and a wedged process cannot run that handler either. Keying on 124 alone was
        ! tried and let a forced timeout report PASS.
        call check(error, exitstat /= 124 .and. exitstat /= 137, &
            "scenario TIMED OUT and was killed (neither finished nor aborted): "//trim(scenario))
        if (allocated(error)) return

        call file_contains(out_file, text, on_out)
        call file_contains(err_file, text, on_err)

        if (trim(expect_on) == "stdout") then
            wanted = on_out
            unwanted = on_err
        else
            wanted = on_err
            unwanted = on_out
        end if

        call check(error, wanted, failure_message // " (expected on " // trim(expect_on) // ")")
        if (allocated(error)) return
        call check(error, .not. unwanted, &
            failure_message // " (must NOT also appear on the other stream)")
    end subroutine check_scenario_streams

    !> Whether `text` occurs on any line of `path`. A missing file counts as "not present", so a
    !> scenario that wrote nothing to one stream reads as absent rather than as a test error.
    subroutine file_contains(path, text, found)
        character(len=*), intent(in) :: path, text
        logical, intent(out) :: found
        character(len=1024) :: line
        integer :: unit, ios

        found = .false.
        open(newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, text) > 0) found = .true.
        end do
        close(unit)
    end subroutine file_contains

    !> Like check_scenario_exit_status_and_stderr, but asserts `forbidden_text`
    !> is ABSENT from the captured (combined stdout+stderr) output instead of
    !> present -- used for read-time qc scenarios that must NOT print a
    !> WARNING (e.g. qc: miss: Null explicitly allowing Nulls, or qc=.false.
    !> suppressing a would-be violation).
    subroutine check_scenario_exit_status_and_no_output(error, scenario, expect_abort, failure_message, forbidden_text)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message, forbidden_text
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: aborted, found

        call run_error_scenario(scenario, exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        ! 124 and 137 are what `timeout` reports when the per-scenario cap fires: the scenario
        ! neither finished nor aborted. Both have to be singled out, because every check below
        ! reads a nonzero exit as "aborted" -- so a hung scenario would otherwise PASS as a
        ! successful abort, which is the failure mode the cap exists to expose rather than hide.
        ! 124 is the documented code; 137 (128+9) is what comes back instead when the cap kills
        ! with SIGKILL, which is what this harness asks for because the Fortran runtimes catch
        ! SIGTERM and a wedged process cannot run that handler either. Keying on 124 alone was
        ! tried and let a forced timeout report PASS.
        call check(error, exitstat /= 124 .and. exitstat /= 137, &
            "scenario TIMED OUT and was killed (neither finished nor aborted): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, forbidden_text, found)

        call check(error, .not. found, &
            "expected output to NOT contain '" // trim(forbidden_text) // "' for scenario '" // trim(scenario) // "'")
    end subroutine check_scenario_exit_status_and_no_output
    !
    ! ------------------------------------------------------------------------------
    ! parquet_temporal (parquet_date/parquet_time/parquet_timestamp) abort paths
    ! ------------------------------------------------------------------------------
    !
    subroutine test_temporal_date_set_invalid_month_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_invalid_month", expect_abort=.true., &
            failure_message="parquet_date%set with month 13 was expected to abort", &
            required_stderr="parquet_temporal: invalid month in parquet_date%set")
    end subroutine test_temporal_date_set_invalid_month_aborts

    subroutine test_temporal_date_set_invalid_day_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_invalid_day", expect_abort=.true., &
            failure_message="parquet_date%set with 1900-02-29 was expected to abort", &
            required_stderr="parquet_temporal: invalid day of month in parquet_date%set")
    end subroutine test_temporal_date_set_invalid_day_aborts

    subroutine test_temporal_date_set_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_out_of_range", expect_abort=.true., &
            failure_message="parquet_date%set beyond the int32 day range was expected to abort", &
            required_stderr="parquet_temporal: date out of range in parquet_date%set")
    end subroutine test_temporal_date_set_out_of_range_aborts

    subroutine test_temporal_date_set_mjd_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_mjd_out_of_range", expect_abort=.true., &
            failure_message="parquet_date%set_mjd beyond the int32 day range was expected to abort", &
            required_stderr="parquet_temporal: MJD out of range in parquet_date%set_mjd")
    end subroutine test_temporal_date_set_mjd_out_of_range_aborts

    subroutine test_temporal_date_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_parse_invalid", expect_abort=.true., &
            failure_message="parquet_date%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 date (expected YYYY-MM-DD) in parquet_date%parse")
    end subroutine test_temporal_date_parse_invalid_aborts

    subroutine test_temporal_date_null_get_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_null_get", expect_abort=.true., &
            failure_message="parquet_date%get on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_date element accessed in get")
    end subroutine test_temporal_date_null_get_aborts

    subroutine test_temporal_date_null_comparison_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_null_comparison", expect_abort=.true., &
            failure_message="comparing against a null parquet_date was expected to abort", &
            required_stderr="parquet_temporal: comparison with a null parquet_date element")
    end subroutine test_temporal_date_null_comparison_aborts

    subroutine test_temporal_time_set_invalid_hour_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_invalid_hour", expect_abort=.true., &
            failure_message="parquet_time%set with hour 24 was expected to abort", &
            required_stderr="parquet_temporal: invalid hour in parquet_time%set")
    end subroutine test_temporal_time_set_invalid_hour_aborts

    subroutine test_temporal_time_set_invalid_nanosecond_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_invalid_nanosecond", expect_abort=.true., &
            failure_message="parquet_time%set with nanosecond 1e9 was expected to abort", &
            required_stderr="parquet_temporal: invalid nanosecond in parquet_time%set")
    end subroutine test_temporal_time_set_invalid_nanosecond_aborts

    subroutine test_temporal_time_set_raw_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_raw_out_of_range", expect_abort=.true., &
            failure_message="parquet_time%set_raw outside a day was expected to abort", &
            required_stderr="parquet_temporal: nanoseconds-of-day out of range in parquet_time%set_raw")
    end subroutine test_temporal_time_set_raw_out_of_range_aborts

    subroutine test_temporal_time_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_parse_invalid", expect_abort=.true., &
            failure_message="parquet_time%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 time")
    end subroutine test_temporal_time_parse_invalid_aborts

    subroutine test_temporal_time_null_get_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_null_get", expect_abort=.true., &
            failure_message="parquet_time%hour on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_time element accessed in hour")
    end subroutine test_temporal_time_null_get_aborts

    subroutine test_temporal_ts_set_invalid_day_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_day", expect_abort=.true., &
            failure_message="parquet_timestamp%set with Feb 30 was expected to abort", &
            required_stderr="parquet_temporal: invalid day of month in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_day_aborts

    subroutine test_temporal_ts_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_parse_invalid", expect_abort=.true., &
            failure_message="parquet_timestamp%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 date-time")
    end subroutine test_temporal_ts_parse_invalid_aborts

    subroutine test_temporal_ts_null_to_unix_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_null_to_unix", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_timestamp element accessed in to_unix")
    end subroutine test_temporal_ts_null_to_unix_aborts

    subroutine test_temporal_ts_null_comparison_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_null_comparison", expect_abort=.true., &
            failure_message="comparing against a null parquet_timestamp was expected to abort", &
            required_stderr="parquet_temporal: comparison with a null parquet_timestamp element")
    end subroutine test_temporal_ts_null_comparison_aborts

    subroutine test_temporal_ts_to_unix_precision_loss_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_precision_loss", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix with sub-unit precision was expected to abort", &
            required_stderr="parquet_temporal: precision loss in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_precision_loss_aborts

    subroutine test_temporal_ts_to_unix_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix overflowing int64 was expected to abort", &
            required_stderr="parquet_temporal: overflow in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_overflow_aborts

    subroutine test_temporal_ts_set_mjd_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_mjd_out_of_range", expect_abort=.true., &
            failure_message="parquet_timestamp%set_mjd beyond the int64 seconds range was expected to abort", &
            required_stderr="parquet_temporal: MJD out of range in parquet_timestamp%set_mjd")
    end subroutine test_temporal_ts_set_mjd_out_of_range_aborts

    subroutine test_temporal_ts_set_raw_invalid_nanoseconds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_raw_invalid_nanoseconds", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%set_raw with a negative nanosecond part was expected to abort", &
            required_stderr="parquet_temporal: nanosecond part out of range in parquet_timestamp%set_raw")
    end subroutine test_temporal_ts_set_raw_invalid_nanoseconds_aborts

    subroutine test_temporal_invalid_time_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_invalid_time_unit", expect_abort=.true., &
            failure_message="an invalid time-unit selector was expected to abort", &
            required_stderr="parquet_temporal: invalid time unit")
    end subroutine test_temporal_invalid_time_unit_aborts

    subroutine test_temporal_ts_get_year_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_year_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp%get with a year beyond int32 was expected to abort", &
            required_stderr="parquet_temporal: year out of integer(int32) range in parquet_timestamp%get")
    end subroutine test_temporal_ts_get_year_overflow_aborts

    !> C++-side precision guard (build_time_array), distinct from parquet_timestamp%to_unix's own
    !> Fortran-side guard (see test_temporal_ts_to_unix_precision_loss_aborts, above).
    subroutine test_temporal_write_time_precision_loss_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_time_precision_loss", expect_abort=.true., &
            failure_message="writing a sub-millisecond time value into a time[ms] column was expected to abort", &
            required_stderr="time value has finer precision than the column's declared unit")
    end subroutine test_temporal_write_time_precision_loss_aborts

    !> parquet_get_column_time_info's documented abort for a column that is not a
    !> TIME/TIMESTAMP. The scenario queries the file's timestamp column first and error stops
    !> if that does not answer micros, so a build whose time-info query aborted unconditionally
    !> fails here rather than passing -- see the scenario's own comment for why the abort is
    !> provoked with a `date` column rather than an int32 one.
    subroutine test_temporal_time_info_on_date_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "time_info_on_date_column", expect_abort=.true., &
            failure_message="parquet_get_column_time_info on a date column was expected to abort", &
            required_stderr="column is not a time/timestamp column")
    end subroutine test_temporal_time_info_on_date_column_aborts

    !> `qc: miss:` is supported on a temporal column even though `qc:` BOUNDS are not: a `date`
    !> field declaring qc_miss="Null" must write its null element without a WARNING. Both halves
    !> are asserted here, because either alone is vacuous -- the no-WARNING assertion passes
    !> against a build that never miss-checks temporal columns, and the WARNING assertion passes
    !> against one that ignores a declared qc_miss.
    subroutine test_temporal_qc_miss_declared_null_no_warning(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_no_output(error, "qc_miss_temporal_declared_null_no_warning", &
            expect_abort=.false., &
            failure_message="declaring qc_miss on a date column was not expected to abort", &
            forbidden_text="WARNING:")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "qc_miss_temporal_warns", expect_abort=.false., &
            failure_message="a date column with an undeclared qc_miss was not expected to abort", &
            required_stderr="qc violation for column 'd'")
    end subroutine test_temporal_qc_miss_declared_null_no_warning

    subroutine test_temporal_protected_col_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_protected_col_null", expect_abort=.true., &
            failure_message="writing a Null into a protected timestamp column was expected to abort", &
            required_stderr="column 'ev' is protected")
    end subroutine test_temporal_protected_col_null_aborts

    subroutine test_temporal_read_date_via_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_read_date_via_int32", expect_abort=.true., &
            failure_message="reading a date column via parquet_read_int32_column was expected to abort", &
            required_stderr="expected int32/int64, got date32")
    end subroutine test_temporal_read_date_via_int32_aborts

    subroutine test_temporal_read_int32_via_date_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_read_int32_via_date", expect_abort=.true., &
            failure_message="reading an int32 column via parquet_read_date_column was expected to abort", &
            required_stderr="expected date, got int32")
    end subroutine test_temporal_read_int32_via_date_aborts

    subroutine test_temporal_date_eq_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_eq_null", expect_abort=.true., &
            failure_message="parquet_date operator(==) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_date element")
    end subroutine test_temporal_date_eq_null_aborts

    subroutine test_temporal_time_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_get_null", expect_abort=.true., &
            failure_message="parquet_time%get on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in get")
    end subroutine test_temporal_time_get_null_aborts

    subroutine test_temporal_time_minute_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_minute_null", expect_abort=.true., &
            failure_message="parquet_time%minute on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in minute")
    end subroutine test_temporal_time_minute_null_aborts

    subroutine test_temporal_time_second_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_second_null", expect_abort=.true., &
            failure_message="parquet_time%second on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in second")
    end subroutine test_temporal_time_second_null_aborts

    subroutine test_temporal_time_nanosecond_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_nanosecond_null", expect_abort=.true., &
            failure_message="parquet_time%nanosecond on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in nanosecond")
    end subroutine test_temporal_time_nanosecond_null_aborts

    subroutine test_temporal_time_eq_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_eq_null", expect_abort=.true., &
            failure_message="parquet_time operator(==) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_time element")
    end subroutine test_temporal_time_eq_null_aborts

    subroutine test_temporal_time_lt_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_lt_null", expect_abort=.true., &
            failure_message="parquet_time operator(<) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_time element")
    end subroutine test_temporal_time_lt_null_aborts

    subroutine test_temporal_date_year_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_year_null", expect_abort=.true., &
            failure_message="parquet_date%year on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in year")
    end subroutine test_temporal_date_year_null_aborts

    subroutine test_temporal_date_month_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_month_null", expect_abort=.true., &
            failure_message="parquet_date%month on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in month")
    end subroutine test_temporal_date_month_null_aborts

    subroutine test_temporal_date_day_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_day_null", expect_abort=.true., &
            failure_message="parquet_date%day on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in day")
    end subroutine test_temporal_date_day_null_aborts

    subroutine test_temporal_date_to_mjd_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_to_mjd_null", expect_abort=.true., &
            failure_message="parquet_date%to_mjd on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in to_mjd")
    end subroutine test_temporal_date_to_mjd_null_aborts

    subroutine test_temporal_date_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_to_string_null", expect_abort=.true., &
            failure_message="parquet_date%to_string on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in to_string")
    end subroutine test_temporal_date_to_string_null_aborts

    subroutine test_temporal_time_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_to_string_null", expect_abort=.true., &
            failure_message="parquet_time%to_string on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in to_string")
    end subroutine test_temporal_time_to_string_null_aborts

    subroutine test_temporal_ts_set_invalid_month_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_month", expect_abort=.true., &
            failure_message="parquet_timestamp%set with month 13 was expected to abort", &
            required_stderr="invalid month in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_month_aborts

    subroutine test_temporal_ts_set_invalid_nanosecond_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_nanosecond", expect_abort=.true., &
            failure_message="parquet_timestamp%set with nanosecond 1e9 was expected to abort", &
            required_stderr="invalid nanosecond in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_nanosecond_aborts

    subroutine test_temporal_ts_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_null", expect_abort=.true., &
            failure_message="parquet_timestamp%get on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in get")
    end subroutine test_temporal_ts_get_null_aborts

    subroutine test_temporal_ts_get_date_range_exceeded_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_date_range_exceeded", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%get_date beyond parquet_date's range was expected to abort", &
            required_stderr="date part out of parquet_date range in parquet_timestamp%get_date")
    end subroutine test_temporal_ts_get_date_range_exceeded_aborts

    subroutine test_temporal_ts_to_unix_overflow_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_overflow_negative", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix negative-branch overflow was expected to abort", &
            required_stderr="overflow in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_overflow_negative_aborts

    subroutine test_temporal_ts_to_mjd_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_mjd_null", expect_abort=.true., &
            failure_message="parquet_timestamp%to_mjd on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in to_mjd")
    end subroutine test_temporal_ts_to_mjd_null_aborts

    subroutine test_temporal_ts_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_string_null", expect_abort=.true., &
            failure_message="parquet_timestamp%to_string on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in to_string")
    end subroutine test_temporal_ts_to_string_null_aborts

    subroutine test_temporal_ts_lt_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_lt_null", expect_abort=.true., &
            failure_message="parquet_timestamp operator(<) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_timestamp element")
    end subroutine test_temporal_ts_lt_null_aborts

    subroutine test_temporal_date_diff_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_diff_null", expect_abort=.true., &
            failure_message="parquet_date operator(-) diff with a null operand was expected to abort", &
            required_stderr="difference with a null parquet_date element")
    end subroutine test_temporal_date_diff_null_aborts

    subroutine test_temporal_date_offset_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_offset_null", expect_abort=.true., &
            failure_message="parquet_date day-offset operator(+) with a null operand was expected to abort", &
            required_stderr="null parquet_date element accessed in operator(+)/operator(-)")
    end subroutine test_temporal_date_offset_null_aborts

    subroutine test_temporal_date_offset_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_offset_out_of_range", expect_abort=.true., &
            failure_message="parquet_date day-offset arithmetic beyond the +-5.8M year range was expected to abort", &
            required_stderr="date out of range in parquet_date operator(+)/operator(-)")
    end subroutine test_temporal_date_offset_out_of_range_aborts

    subroutine test_temporal_date_offset_int64_overflow_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_offset_int64_overflow_positive", &
            expect_abort=.true., &
            failure_message="parquet_date day-offset int64-overflow guard (positive) was expected to abort", &
            required_stderr="date out of range in parquet_date operator(+)/operator(-)")
    end subroutine test_temporal_date_offset_int64_overflow_positive_aborts

    subroutine test_temporal_date_offset_int64_overflow_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_offset_int64_overflow_negative", &
            expect_abort=.true., &
            failure_message="parquet_date day-offset int64-overflow guard (negative) was expected to abort", &
            required_stderr="date out of range in parquet_date operator(+)/operator(-)")
    end subroutine test_temporal_date_offset_int64_overflow_negative_aborts

    subroutine test_temporal_date_sub_int64_min_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_sub_int64_min", expect_abort=.true., &
            failure_message="parquet_date operator(-) with an INT64_MIN offset was expected to abort", &
            required_stderr="date out of range in parquet_date operator(-)")
    end subroutine test_temporal_date_sub_int64_min_aborts

    subroutine test_temporal_time_diff_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_diff_null", expect_abort=.true., &
            failure_message="parquet_time operator(-) diff with a null operand was expected to abort", &
            required_stderr="difference with a null parquet_time element")
    end subroutine test_temporal_time_diff_null_aborts

    subroutine test_temporal_time_offset_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_offset_null", expect_abort=.true., &
            failure_message="parquet_time ns-offset operator(+) with a null operand was expected to abort", &
            required_stderr="null parquet_time element accessed in operator(+)/operator(-)")
    end subroutine test_temporal_time_offset_null_aborts

    subroutine test_temporal_time_offset_magnitude_add_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_offset_magnitude_add", expect_abort=.true., &
            failure_message="parquet_time operator(+) offset exceeding 24h was expected to abort", &
            required_stderr="offset magnitude exceeds 24 hours in parquet_time operator(+)")
    end subroutine test_temporal_time_offset_magnitude_add_aborts

    subroutine test_temporal_time_offset_magnitude_sub_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_offset_magnitude_sub", expect_abort=.true., &
            failure_message="parquet_time operator(-) offset exceeding 24h was expected to abort", &
            required_stderr="offset magnitude exceeds 24 hours in parquet_time operator(-)")
    end subroutine test_temporal_time_offset_magnitude_sub_aborts

    subroutine test_temporal_ts_diff_ns_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_diff_ns_null", expect_abort=.true., &
            failure_message="parquet_timestamp operator(-) diff with a null operand was expected to abort", &
            required_stderr="difference with a null parquet_timestamp element")
    end subroutine test_temporal_ts_diff_ns_null_aborts

    subroutine test_temporal_ts_diff_ns_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_diff_ns_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp operator(-) diff beyond ~292.3 years was expected to abort", &
            required_stderr="parquet_timestamp difference exceeds the representable nanosecond range")
    end subroutine test_temporal_ts_diff_ns_overflow_aborts

    subroutine test_temporal_ts_diff_seconds_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_diff_seconds_null", expect_abort=.true., &
            failure_message="parquet_timestamp diff_seconds with a null operand was expected to abort", &
            required_stderr="difference with a null parquet_timestamp element")
    end subroutine test_temporal_ts_diff_seconds_null_aborts

    subroutine test_temporal_ts_offset_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_offset_null", expect_abort=.true., &
            failure_message="parquet_timestamp ns-offset operator(+) with a null operand was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in operator(+)/operator(-)")
    end subroutine test_temporal_ts_offset_null_aborts

    subroutine test_temporal_ts_offset_ns_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_offset_ns_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp ns-offset nanosecond-addition int64 overflow was expected to abort", &
            required_stderr="parquet_timestamp offset arithmetic overflows int64")
    end subroutine test_temporal_ts_offset_ns_overflow_aborts

    subroutine test_temporal_ts_offset_seconds_overflow_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_offset_seconds_overflow_positive", &
            expect_abort=.true., &
            failure_message="parquet_timestamp ns-offset seconds-carry int64 overflow (positive) was expected to abort", &
            required_stderr="parquet_timestamp offset arithmetic overflows int64 seconds")
    end subroutine test_temporal_ts_offset_seconds_overflow_positive_aborts

    subroutine test_temporal_ts_offset_seconds_overflow_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_offset_seconds_overflow_negative", &
            expect_abort=.true., &
            failure_message="parquet_timestamp ns-offset seconds-carry int64 overflow (negative) was expected to abort", &
            required_stderr="parquet_timestamp offset arithmetic overflows int64 seconds")
    end subroutine test_temporal_ts_offset_seconds_overflow_negative_aborts

    subroutine test_temporal_ts_sub_int64_min_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_sub_int64_min", expect_abort=.true., &
            failure_message="parquet_timestamp operator(-) with an INT64_MIN offset was expected to abort", &
            required_stderr="parquet_timestamp offset arithmetic overflows int64")
    end subroutine test_temporal_ts_sub_int64_min_aborts

    subroutine test_temporal_write_column_not_defined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_column_not_defined", expect_abort=.true., &
            failure_message="writing an undeclared temporal column was expected to abort", &
            required_stderr="column not defined in parquet_open_writer: not_a_real_column")
    end subroutine test_temporal_write_column_not_defined_aborts

    subroutine test_temporal_write_array_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_array_size_mismatch", &
            expect_abort=.true., &
            failure_message="writing a temporal vector column with the wrong col_size was expected to abort", &
            required_stderr="array size mismatch for column ev")
    end subroutine test_temporal_write_array_size_mismatch_aborts

    subroutine test_temporal_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_type_mismatch", expect_abort=.true., &
            failure_message="writing a timestamp array into a date-declared column was expected to abort", &
            required_stderr="type mismatch for column day (expected timestamp, got date)")
    end subroutine test_temporal_write_type_mismatch_aborts

    subroutine test_temporal_chunk_column_not_defined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_column_not_defined", expect_abort=.true., &
            failure_message="chunk-writing an undeclared temporal column was expected to abort", &
            required_stderr="parquet_write_column_chunk: column not defined")
    end subroutine test_temporal_chunk_column_not_defined_aborts

    subroutine test_temporal_chunk_array_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_array_size_mismatch", &
            expect_abort=.true., &
            failure_message="chunk-writing a temporal vector column with the wrong col_size was expected to abort", &
            required_stderr="parquet_write_column_chunk: array size mismatch for column ev")
    end subroutine test_temporal_chunk_array_size_mismatch_aborts

    subroutine test_temporal_chunk_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_type_mismatch", expect_abort=.true., &
            failure_message="chunk-writing a timestamp array into a date-declared column was expected to abort", &
            required_stderr="parquet_write_column_chunk: type mismatch for column day")
    end subroutine test_temporal_chunk_type_mismatch_aborts

    subroutine test_get_version_invalid_mode_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "get_version_invalid_mode", expect_abort=.true., &
            failure_message="parquet_get_version with an unrecognized mode was expected to abort", &
            required_stderr="parquet_get_version: invalid mode 'bogus'")
    end subroutine test_get_version_invalid_mode_aborts

    !> mode="arrow"/"parquet" moved to parquet_get_arrow_version when parquet_get_version moved
    !> into the Arrow-free leaf module parquet_version. Asserting on the second half of the message
    !> is what pins the migration hint: without it the abort would merely say "invalid".
    subroutine test_get_version_arrow_mode_removed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "get_version_arrow_mode_removed", expect_abort=.true., &
            failure_message="parquet_get_version(mode='arrow') was expected to abort now that the mode has moved", &
            required_stderr="call parquet_get_arrow_version")
    end subroutine test_get_version_arrow_mode_removed_aborts

    subroutine test_get_arrow_version_invalid_mode_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "get_arrow_version_invalid_mode", expect_abort=.true., &
            failure_message="parquet_get_arrow_version with an unrecognized mode was expected to abort", &
            required_stderr="parquet_get_arrow_version: invalid mode 'internal'")
    end subroutine test_get_arrow_version_invalid_mode_aborts

    subroutine test_column_exists_bad_type_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_exists_bad_type_token", expect_abort=.true., &
            failure_message="parquet_column_exists with an unrecognized types= token was expected to abort", &
            required_stderr="parquet_column_exists: unrecognized data type token 'itn32'")
    end subroutine test_column_exists_bad_type_token_aborts

    !> The ORDER of parquet_column_exists's two checks, which its sibling above cannot show.
    !>
    !> That one names a column the fixture HAS, so it aborts with this same message whichever
    !> check runs first. Here the column is absent: an implementation validating existence first
    !> would return .false. quietly (a missing column is an answer for this procedure, not an
    !> abort), so the scenario would exit 0 and this test would fail on expect_abort. The two
    !> together are what pin the ordering doc/pages/io/reading.md documents; neither is enough
    !> alone, which is why the older one is left exactly as it was rather than being retargeted.
    subroutine test_column_exists_bad_type_token_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_exists_bad_type_token_missing_column", &
            expect_abort=.true., &
            failure_message="parquet_column_exists must validate types= before looking the column up", &
            required_stderr="parquet_column_exists: unrecognized data type token 'itn32'")
    end subroutine test_column_exists_bad_type_token_missing_column_aborts

    subroutine test_column_exists_empty_type_filter_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_exists_empty_type_filter", expect_abort=.true., &
            failure_message="parquet_column_exists with a blank types= filter was expected to abort", &
            required_stderr="parquet_column_exists: types= must not be empty")
    end subroutine test_column_exists_empty_type_filter_aborts


    subroutine test_mask_row_mask_after_write_started_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_after_write_started", expect_abort=.true., &
            failure_message="parquet_write_row_mask after the writer had started was expected to abort", &
            required_stderr="parquet_write_row_mask: must be called before the writer's first")
    end subroutine test_mask_row_mask_after_write_started_aborts

    subroutine test_mask_row_mask_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_shape_mismatch", expect_abort=.true., &
            failure_message="a whole-column write with the wrong row count for the mask was expected to abort", &
            required_stderr="does not match the mask set via parquet_write_row_mask")
    end subroutine test_mask_row_mask_shape_mismatch_aborts

    subroutine test_mask_row_mask_zero_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_zero_length", expect_abort=.true., &
            failure_message="parquet_write_row_mask with a zero-length mask was expected to abort", &
            required_stderr="parquet_write_row_mask: mask must not be zero-length")
    end subroutine test_mask_row_mask_zero_length_aborts

    subroutine test_mask_row_mask_called_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_called_twice", expect_abort=.true., &
            failure_message="a second parquet_write_row_mask call for the same writer was expected to abort", &
            required_stderr="parquet_write_row_mask: already called for this writer")
    end subroutine test_mask_row_mask_called_twice_aborts

    subroutine test_mask_chunk_row_mask_after_row_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_after_row_mask", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after parquet_write_row_mask was expected to abort", &
            required_stderr="cannot be used together with parquet_write_row_mask")
    end subroutine test_mask_chunk_row_mask_after_row_mask_aborts

    subroutine test_mask_row_mask_after_chunk_row_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_after_chunk_row_mask", expect_abort=.true., &
            failure_message="parquet_write_row_mask after the writer had started row groups was expected to abort", &
            required_stderr="parquet_write_row_mask: must be called before the writer's first")
    end subroutine test_mask_row_mask_after_chunk_row_mask_aborts

    subroutine test_mask_chunk_row_mask_after_whole_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_after_whole_column", &
            expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after a whole-column write was expected to abort", &
            required_stderr="unavailable once a column has been written whole")
    end subroutine test_mask_chunk_row_mask_after_whole_column_aborts

    subroutine test_mask_chunk_row_mask_not_used_every_group_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_not_used_every_group", &
            expect_abort=.true., &
            failure_message="a row group without parquet_write_chunk_row_mask after an earlier one used it " // &
                "was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask must be called for every row group")
    end subroutine test_mask_chunk_row_mask_not_used_every_group_aborts

    subroutine test_mask_chunk_row_mask_introduced_late_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_introduced_late", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after this row group's first chunk write " // &
                "was expected to abort", &
            required_stderr="must be called before this row group's first")
    end subroutine test_mask_chunk_row_mask_introduced_late_aborts

    subroutine test_mask_chunk_row_mask_called_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_called_twice", expect_abort=.true., &
            failure_message="a second parquet_write_chunk_row_mask call for the same row group was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask: already called for this row group")
    end subroutine test_mask_chunk_row_mask_called_twice_aborts

    subroutine test_mask_chunk_row_mask_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_size_mismatch", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask with the wrong mask size was expected to abort", &
            required_stderr="mask size (3) does not match the open row group's own nrows (2)")
    end subroutine test_mask_chunk_row_mask_size_mismatch_aborts

    subroutine test_mask_row_mask_window_exhausted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_window_exhausted", expect_abort=.true., &
            failure_message="a row group claiming more mask positions than parquet_write_row_mask provided " // &
                "was expected to abort", &
            required_stderr="would claim more positions of the mask")
    end subroutine test_mask_row_mask_window_exhausted_aborts

    subroutine test_mask_row_mask_not_fully_consumed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_not_fully_consumed", expect_abort=.true., &
            failure_message="closing a writer with an unconsumed parquet_write_row_mask tail was expected to abort", &
            required_stderr="was not fully consumed by this writer's row groups")
    end subroutine test_mask_row_mask_not_fully_consumed_aborts

    subroutine test_mask_chunk_row_mask_no_row_group_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_no_row_group_open", &
            expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask with no row group open was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask: no row group is open")
    end subroutine test_mask_chunk_row_mask_no_row_group_open_aborts

    subroutine test_mask_chunk_row_mask_scheme_declined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_scheme_declined", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after an earlier row group declined it " // &
                "was expected to abort", &
            required_stderr="was not used for this writer's first row group")
    end subroutine test_mask_chunk_row_mask_scheme_declined_aborts

    subroutine test_mask_row_group_no_writes_at_all_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_group_no_writes_at_all", expect_abort=.true., &
            failure_message="finishing a row group with no column ever written for it was expected to abort", &
            required_stderr="no columns have been written")
    end subroutine test_mask_row_group_no_writes_at_all_aborts
    !
    ! Note: the temporal_foreign_int96_roundtrip/temporal_foreign_tz_roundtrip scenarios (clean
    ! exits, not aborts) are checked from test_reading.f90's test_datetime_foreign_fixtures, not
    ! here -- same convention as test_writing.f90's test_large_string_column_roundtrip/
    ! test_string_view_column_roundtrip, which also drive an out-of-process debug-hook fixture
    ! via check_scenario_exit_status but live in the suite whose read/write feature they exercise.
    subroutine test_table_detached_read_unmaterialized_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_detached_read_unmaterialized", expect_abort=.true., &
            failure_message="reading a column left behind by a detach was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_table_detached_read_unmaterialized_aborts

    subroutine test_table_slice_mutate_then_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_slice_mutate_then_read", expect_abort=.true., &
            failure_message="reading a slice's stranded column after a row mutation was expected to abort", &
            required_stderr="detached")
    end subroutine test_table_slice_mutate_then_read_aborts

    subroutine test_table_detached_prefetch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_detached_prefetch", expect_abort=.true., &
            failure_message="prefetching on a detached table was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_table_detached_prefetch_aborts

    subroutine test_table_detached_materialize_all_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_detached_materialize_all", expect_abort=.true., &
            failure_message="materialize_all on a detached table was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_table_detached_materialize_all_aborts

    subroutine test_table_detached_reload_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_detached_reload", expect_abort=.true., &
            failure_message="reloading a column of a detached table was expected to abort", &
            required_stderr="row-structural change, so a re-read would no longer line up")
    end subroutine test_table_detached_reload_aborts

    subroutine test_table_detached_row_group_bounds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_detached_row_group_bounds", expect_abort=.true., &
            failure_message="row_group_bounds on a detached table was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_table_detached_row_group_bounds_aborts

    subroutine test_table_row_group_bounds_sorted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_row_group_bounds_sorted", expect_abort=.true., &
            failure_message="row_group_bounds on a sorted table was expected to abort", &
            required_stderr="row_group_bounds: this table was opened with a sort")
    end subroutine test_table_row_group_bounds_sorted_aborts

    subroutine test_table_mutate_unmaterialized_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_mutate_unmaterialized_column", expect_abort=.true., &
            failure_message="reading a column stranded by a detach was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_table_mutate_unmaterialized_column_aborts

    subroutine test_table_mutate_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_mutate_unsupported_column", expect_abort=.true., &
            failure_message="sorting by an unsupported column was expected to abort", &
            ! table_resolve raises this now, not sort_by's own check -- sorting stopped keeping a
            ! second copy of "which columns can be reached" when it started reading key columns
            ! implicitly. Same refusal, one wording, and the message still names sort_by.
            required_stderr="this column's type is not supported by parquet_table, so its values were never read")
    end subroutine test_table_mutate_unsupported_column_aborts

    !> The message must name argsort_by. All four sorting bindings share one key-lookup helper, so
    !> a dropped `proc` argument would silently make every %argsort_by failure blame %sort_by.
    subroutine test_table_argsort_by_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_by_vector_column", expect_abort=.true., &
            failure_message="argsort_by on a vector column was expected to abort", &
            required_stderr="argsort_by: a ")
    end subroutine test_table_argsort_by_vector_column_aborts

    subroutine test_table_argsort_by_no_keys_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_by_no_keys", expect_abort=.true., &
            failure_message="argsort_by with no key was expected to abort", &
            required_stderr="argsort_by: no sort key was given")
    end subroutine test_table_argsort_by_no_keys_aborts

    subroutine test_table_argsort_by_group_nkeys_too_many_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_by_group_nkeys_too_many", &
            expect_abort=.true., &
            failure_message="group_nkeys above the key count was expected to abort", &
            required_stderr="group_nkeys is 2, which is not between 1 and the 1 keys given")
    end subroutine test_table_argsort_by_group_nkeys_too_many_aborts

    subroutine test_table_argsort_by_group_nkeys_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_by_group_nkeys_zero", &
            expect_abort=.true., &
            failure_message="group_nkeys of zero was expected to abort", &
            required_stderr="group_nkeys is 0, which is not between 1 and the 1 keys given")
    end subroutine test_table_argsort_by_group_nkeys_zero_aborts

    subroutine test_table_argsort_by_group_nkeys_without_offsets_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_by_group_nkeys_without_offsets", &
            expect_abort=.true., &
            failure_message="group_nkeys without group_offsets was expected to abort", &
            required_stderr="group_nkeys was given without group_offsets")
    end subroutine test_table_argsort_by_group_nkeys_without_offsets_aborts

    subroutine test_table_argsort_partial_negative_n_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_argsort_partial_negative_n", &
            expect_abort=.true., &
            failure_message="argsort_partial with a negative n was expected to abort", &
            required_stderr="argsort_partial: n is -1; a negative number of rows cannot be ordered")
    end subroutine test_table_argsort_partial_negative_n_aborts

    subroutine test_table_top_n_negative_n_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        ! The message must name top_n, not argsort_partial: %top_n reaches the same check through
        ! table_build_top_n_permutation, and passing the wrong procedure name there would be
        ! invisible except here.
        call check_scenario_exit_status_and_stderr(error, "table_top_n_negative_n", &
            expect_abort=.true., &
            failure_message="top_n with a negative n was expected to abort", &
            required_stderr="top_n: n is -1; a negative number of rows cannot be ordered")
    end subroutine test_table_top_n_negative_n_aborts

    subroutine test_table_top_n_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_top_n_unknown_column", &
            expect_abort=.true., &
            failure_message="top_n on a column that does not exist was expected to abort", &
            required_stderr="top_n: no column of this name (column 'nope')")
    end subroutine test_table_top_n_unknown_column_aborts

    subroutine test_table_top_n_detached_read_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_top_n_detached_read", &
            expect_abort=.true., &
            failure_message="reading a column stranded by top_n was expected to abort", &
            required_stderr="detached")
    end subroutine test_table_top_n_detached_read_aborts

    subroutine test_column_gather_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_gather_out_of_range", &
            expect_abort=.true., &
            failure_message="gathering a row outside the column was expected to abort", &
            required_stderr="gather: row index 9 is outside this column's 1..3 rows")
    end subroutine test_column_gather_out_of_range_aborts

    subroutine test_string_column_gather_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_gather_out_of_range", &
            expect_abort=.true., &
            failure_message="gathering an element outside the string column was expected to abort", &
            required_stderr="gather: index out of range")
    end subroutine test_string_column_gather_out_of_range_aborts

    subroutine test_table_filter_rows_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_filter_rows_mask_length", expect_abort=.true., &
            failure_message="filtering with a mask of the wrong length was expected to abort", &
            required_stderr="the mask has 2 entries but the table has 3 rows")
    end subroutine test_table_filter_rows_mask_length_aborts

    subroutine test_table_delete_rows_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_delete_rows_out_of_range", expect_abort=.true., &
            failure_message="deleting a row that does not exist was expected to abort", &
            required_stderr="row index 9 is outside this table's 1..3 rows")
    end subroutine test_table_delete_rows_out_of_range_aborts

    subroutine test_table_truncate_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_truncate_negative", expect_abort=.true., &
            failure_message="truncating to a negative row count was expected to abort", &
            required_stderr="truncate: cannot keep -1 rows")
    end subroutine test_table_truncate_negative_aborts

    subroutine test_table_append_null_rows_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_null_rows_negative", expect_abort=.true., &
            failure_message="appending a negative number of rows was expected to abort", &
            required_stderr="append_null_rows: cannot append -3 rows")
    end subroutine test_table_append_null_rows_negative_aborts

    subroutine test_table_sort_by_no_keys_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_sort_by_no_keys", expect_abort=.true., &
            failure_message="sorting with no key was expected to abort", &
            required_stderr="sort_by: no sort key was given")
    end subroutine test_table_sort_by_no_keys_aborts

    subroutine test_table_sort_by_flag_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_sort_by_flag_count_mismatch", expect_abort=.true., &
            failure_message="a short descending= list was expected to abort", &
            required_stderr="descending= has 1 entries but 2 keys were given")
    end subroutine test_table_sort_by_flag_count_mismatch_aborts

    subroutine test_table_sort_by_nulls_first_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_sort_by_nulls_first_count_mismatch", &
            expect_abort=.true., &
            failure_message="a short nulls_first= list was expected to abort", &
            required_stderr="nulls_first= has 1 entries but 2 keys were given")
    end subroutine test_table_sort_by_nulls_first_count_mismatch_aborts

    subroutine test_table_sort_by_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_sort_by_unknown_column", expect_abort=.true., &
            failure_message="sorting by a missing column was expected to abort", &
            required_stderr="sort_by: no column of this name")
    end subroutine test_table_sort_by_unknown_column_aborts

    subroutine test_table_sort_by_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_sort_by_vector_column", expect_abort=.true., &
            failure_message="sorting by a vector column was expected to abort", &
            required_stderr="column cannot be a sort key; there is no defined order on a whole vector row")
    end subroutine test_table_sort_by_vector_column_aborts

    subroutine test_table_append_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_unknown_column", expect_abort=.true., &
            failure_message="appending a table with an extra column was expected to abort", &
            required_stderr="the appended table has a column this table does not")
    end subroutine test_table_append_unknown_column_aborts

    subroutine test_table_append_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_kind_mismatch", expect_abort=.true., &
            failure_message="appending a column of another kind was expected to abort", &
            required_stderr="in the appended table; convert it first (%cast)")
    end subroutine test_table_append_kind_mismatch_aborts

    subroutine test_table_append_width_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_width_mismatch", expect_abort=.true., &
            failure_message="appending a vector column of another width was expected to abort", &
            required_stderr="values per row here but 2 in the appended table")
    end subroutine test_table_append_width_mismatch_aborts

    subroutine test_table_append_unit_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_unit_mismatch", expect_abort=.true., &
            failure_message="appending rows in another unit was expected to abort", &
            required_stderr="and this library does not convert units")
    end subroutine test_table_append_unit_mismatch_aborts

    subroutine test_table_append_row_no_common_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_row_no_common_column", expect_abort=.true., &
            failure_message="appending a row with nothing in common was expected to abort", &
            required_stderr="the row's table has no column in common with this table")
    end subroutine test_table_append_row_no_common_column_aborts

    subroutine test_table_set_element_row_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_element_row_out_of_range", expect_abort=.true., &
            failure_message="writing past the last row was expected to abort", &
            required_stderr="row index 9 is outside this table's 1..2 rows")
    end subroutine test_table_set_element_row_out_of_range_aborts

    subroutine test_table_set_element_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_element_kind_mismatch", expect_abort=.true., &
            failure_message="writing a value of another kind was expected to abort", &
            required_stderr="column kind is PK_INT32, not PK_FLOAT64")
    end subroutine test_table_set_element_kind_mismatch_aborts

    subroutine test_table_set_null_row_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_null_row_out_of_range", expect_abort=.true., &
            failure_message="nulling row 0 was expected to abort", &
            required_stderr="row index 0 is outside this table's 1..2 rows")
    end subroutine test_table_set_null_row_out_of_range_aborts

    !> A row mask must have exactly one entry per row. The message quotes both counts, because a
    !! mask of the wrong length is almost always one that was right before rows were added or
    !! deleted, and the two numbers side by side say that immediately.
    subroutine test_table_set_null_mask_wrong_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_null_mask_wrong_length", &
            expect_abort=.true., &
            failure_message="a validity mask one entry short was expected to abort", &
            required_stderr="set_null: the mask has 2 entries but the table has 3 rows")
    end subroutine test_table_set_null_mask_wrong_length_aborts

    !> An element mask is `(width, rows)`. The scenario passes a transposed one, and the message
    !! must state both shapes -- naming only "wrong shape" would leave the caller guessing which
    !! way round the library wants it, which is the whole difficulty with this argument.
    subroutine test_table_set_null_mask_wrong_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_null_mask_wrong_shape", &
            expect_abort=.true., &
            failure_message="a transposed element validity mask was expected to abort", &
            required_stderr="set_null: the mask is shaped 3 x 2 but the column is 2 x 3 (width x rows)")
    end subroutine test_table_set_null_mask_wrong_shape_aborts

    subroutine test_table_rename_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_rename_duplicate_name", expect_abort=.true., &
            failure_message="renaming onto an existing name was expected to abort", &
            required_stderr="rename_column: a column of the new name already exists")
    end subroutine test_table_rename_duplicate_name_aborts

    subroutine test_table_rename_blank_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_rename_blank_name", expect_abort=.true., &
            failure_message="renaming to a blank name was expected to abort", &
            required_stderr="rename_column: the new name is blank")
    end subroutine test_table_rename_blank_name_aborts

    subroutine test_table_copy_unsupported_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_unsupported_kind", expect_abort=.true., &
            failure_message="copying a string column into an integer kind was expected to abort", &
            required_stderr="copy_column: only the numeric kinds convert")
    end subroutine test_table_copy_unsupported_kind_aborts

    subroutine test_table_copy_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_duplicate_name", expect_abort=.true., &
            failure_message="copying onto an existing name was expected to abort", &
            required_stderr="copy_column: a column of the new name already exists")
    end subroutine test_table_copy_duplicate_name_aborts

    subroutine test_table_copy_blank_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_blank_name", expect_abort=.true., &
            failure_message="copying a column to a blank name was expected to abort", &
            required_stderr="copy_column: the new name is blank")
    end subroutine test_table_copy_blank_name_aborts

    subroutine test_table_copy_lossy_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_lossy_value", expect_abort=.true., &
            failure_message="copying a value that cannot be represented was expected to abort", &
            required_stderr="cannot be represented as PK_INT32, so converting from PK_FLOAT64 would lose information")
    end subroutine test_table_copy_lossy_value_aborts

    subroutine test_table_cast_non_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_non_numeric", expect_abort=.true., &
            failure_message="casting a string column was expected to abort", &
            required_stderr="cast: only the numeric kinds convert, not from PK_STRING to PK_INT64")
    end subroutine test_table_cast_non_numeric_aborts

    subroutine test_table_cast_rank_change_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_rank_change", expect_abort=.true., &
            failure_message="casting a scalar column to a vector kind was expected to abort", &
            required_stderr="would change the column's width, which is a reshape rather than a conversion")
    end subroutine test_table_cast_rank_change_aborts

    subroutine test_table_cast_int_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_int_overflow", expect_abort=.true., &
            failure_message="narrowing an out-of-range integer was expected to abort", &
            required_stderr="cast: the value at row 2 (3000000000) cannot be represented as PK_INT32")
    end subroutine test_table_cast_int_overflow_aborts

    subroutine test_table_cast_fractional_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_fractional", expect_abort=.true., &
            failure_message="casting a fractional value to an integer kind was expected to abort", &
            required_stderr="cast: the value at row 1")
    end subroutine test_table_cast_fractional_aborts

    subroutine test_table_cast_float_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_float_overflow", expect_abort=.true., &
            failure_message="overflowing a float32 cast was expected to abort", &
            required_stderr="cannot be represented as PK_FLOAT32, so converting from PK_FLOAT64")
    end subroutine test_table_cast_float_overflow_aborts

    subroutine test_table_cast_exact_precision_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_exact_precision", expect_abort=.true., &
            failure_message="an exact= cast that loses precision was expected to abort", &
            required_stderr="cannot be represented exactly as PK_FLOAT32")
    end subroutine test_table_cast_exact_precision_aborts

    !> The integer sources of the same precision rule. Each scenario performs the DEFAULT-rules
    !! cast first, so a check that refused the conversion outright -- rather than only under
    !! `exact=` -- would fail these rather than pass them.
    subroutine test_table_cast_exact_i32_to_f32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_exact_i32_to_f32", expect_abort=.true., &
            failure_message="an exact= int32 -> real32 cast that loses precision was expected to abort", &
            required_stderr="cannot be represented exactly as PK_FLOAT32, so converting from PK_INT32")
    end subroutine test_table_cast_exact_i32_to_f32_aborts

    !> bind_predefined's four parallel arrays are written by a code generator, so a length
    !! disagreement means the generator is out of step with itself.
    subroutine test_table_bind_predefined_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_predefined_size_mismatch", &
            expect_abort=.true., &
            failure_message="bind_predefined with a short kinds array was expected to abort", &
            required_stderr="bind_predefined: names, kinds, widths and from_file must all have the same size")
    end subroutine test_table_bind_predefined_size_mismatch_aborts

    !> units= is optional and so is checked separately from the four required arrays.
    subroutine test_table_bind_predefined_units_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_predefined_units_size_mismatch", &
            expect_abort=.true., &
            failure_message="bind_predefined with a short units array was expected to abort", &
            required_stderr="bind_predefined: units, when given, must have the same size as names")
    end subroutine test_table_bind_predefined_units_size_mismatch_aborts

    !> A narrowing declaration warns rather than aborting: the declaration is a contract the file
    !! need not honour exactly. The scenario binds a WIDENING declaration first, which must stay
    !! silent -- without that control, a bind that warned about every column would pass.
    subroutine test_table_bind_predefined_lossy_warning(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_wide, warned_narrow

        call run_error_scenario("table_bind_predefined_lossy_warning", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a narrowing declaration must warn, never abort")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'wide' is declared", warned_wide)
        call check(error, warned_wide, &
            "declaring an int64 file column as int32 may lose range, so bind_predefined must say so")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'narrow' is declared", warned_narrow)
        call check(error, .not. warned_narrow, &
            "int32 -> int64 loses nothing, so the widening declaration must stay silent")
    end subroutine test_table_bind_predefined_lossy_warning

    !> A computed column has no file behind it, so its name must be free -- otherwise the table
    !! would end up with two columns answering to one name.
    subroutine test_table_bind_predefined_computed_name_taken_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_bind_predefined_computed_name_taken", &
            expect_abort=.true., &
            failure_message="binding a computed column over an existing name was expected to abort", &
            required_stderr="but the table already has a column of that name")
    end subroutine test_table_bind_predefined_computed_name_taken_aborts

    !> A scattered selection pairs one validity entry with each SELECTED row, not with each row of
    !! the table -- applying a longer mask anyway would null rows the caller never named.
    subroutine test_table_set_rows_valid_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_rows_valid_length_mismatch", &
            expect_abort=.true., &
            failure_message="a slice write whose is_valid is longer than the selection was expected to abort", &
            required_stderr="is_valid has 3 entries but the selection has 2 rows")
    end subroutine test_table_set_rows_valid_length_mismatch_aborts

    !> A rank-2 is_valid is per ELEMENT and is checked on both extents, so a transposed mask is
    !! rejected rather than silently applied the wrong way round.
    subroutine test_table_set_elem_valid_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_elem_valid_shape_mismatch", &
            expect_abort=.true., &
            failure_message="a whole-column write with a transposed element mask was expected to abort", &
            required_stderr="is_valid is shaped 3 x 2 but the column is 2 x 3 (width x rows)")
    end subroutine test_table_set_elem_valid_shape_mismatch_aborts

    !> Both axes at once: a scattered selection of a vector column, whose mask is (width, selected).
    subroutine test_table_set_rows_elem_valid_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_rows_elem_valid_shape_mismatch", &
            expect_abort=.true., &
            failure_message="a vector slice write with a mis-shaped element mask was expected to abort", &
            required_stderr="is_valid is shaped 2 x 3 but the selection is 2 x 2 (width x rows)")
    end subroutine test_table_set_rows_elem_valid_shape_mismatch_aborts

    !> A slice write pairs one value with each selected row; writing only what fits would put
    !! values on rows they were never meant for, and every value involved is legal.
    subroutine test_table_set_slice_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_set_slice_size_mismatch", &
            expect_abort=.true., &
            failure_message="a slice write with more values than selected rows was expected to abort", &
            required_stderr="the array has 3 values but the selection picks 2 rows")
    end subroutine test_table_set_slice_size_mismatch_aborts

    subroutine test_table_cast_exact_i64_to_f32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_exact_i64_to_f32", expect_abort=.true., &
            failure_message="an exact= int64 -> real32 cast that loses precision was expected to abort", &
            required_stderr="cannot be represented exactly as PK_FLOAT32, so converting from PK_INT64")
    end subroutine test_table_cast_exact_i64_to_f32_aborts

    subroutine test_table_cast_exact_i64_to_f64_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_exact_i64_to_f64", expect_abort=.true., &
            failure_message="an exact= int64 -> real64 cast that loses precision was expected to abort", &
            required_stderr="cannot be represented exactly as PK_FLOAT64, so converting from PK_INT64")
    end subroutine test_table_cast_exact_i64_to_f64_aborts

    !> The real32 source of the fractional rule, which reaches `chk_from_f32` rather than the
    !! real64 checker `table_cast_fractional` exercises. The message must name PK_FLOAT32 as the
    !! source, or the two checkers could be confused for one.
    subroutine test_table_cast_f32_fractional_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_f32_fractional", expect_abort=.true., &
            failure_message="casting a fractional real32 to an integer kind was expected to abort", &
            required_stderr="cannot be represented as PK_INT64, so converting from PK_FLOAT32")
    end subroutine test_table_cast_f32_fractional_aborts

    subroutine test_table_cast_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_cast_unsupported_column", expect_abort=.true., &
            failure_message="casting an unsupported column was expected to abort", &
            required_stderr="cast: this column's type is not supported by parquet_table")
    end subroutine test_table_cast_unsupported_column_aborts

    subroutine test_table_clone_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_clone_type_mismatch", expect_abort=.true., &
            failure_message="cloning into another table type was expected to abort", &
            required_stderr="source and destination must be the same table type")
    end subroutine test_table_clone_type_mismatch_aborts

end module test_errors
