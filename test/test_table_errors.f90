!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Abort-path tests for the column and table layer, driving the scenarios in
!> `error_scenarios_table.f90`: quality-control rules, column adders and sinks, schema and
!> metadata handling, string and temporal columns, write row masks, the column containers,
!> and the `parquet_table` type with its generated accessors.
!!
!! One of three modules split out of `test_errors.f90`, which keeps the subprocess-driving
!! machinery (`run_error_scenario`, the `check_scenario_*` helpers, `prime_error_scenarios`)
!! and the tests for `error_scenarios_io.f90`. The split is for COMPILE TIME: at about 23000
!! lines the one module took some 27 s under ifx, second only to `error_scenarios.f90` itself.
!! Each module's tests mirror one `error_scenarios_*` group, so a scenario and the test that
!! drives it stay in files with the same name.
module test_table_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64
    !$ use omp_lib, only : omp_get_max_threads, omp_get_num_procs
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_no_output, &
        check_scenario_exit_status_and_stderr, check_scenario_streams, run_error_scenario, scenario_capture_contains
    !
    implicit none
    private
    public :: collect_tests_parquet_table_errors
    !
contains

    subroutine collect_tests_parquet_table_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end, and every part is capped at 255
        ! continuation lines -- the two traps that shape apart are written out in full in
        ! test_errors.f90's own collect_tests_parquet_errors. WHEN A PART IS FULL, ADD A NEW
        ! ONE rather than growing an existing one, and close it by removing the COMMA, not
        ! the `&`. check_statement_continuation_lines (tools/check_source_conventions.py)
        ! fails the lint stage before nagfor ever sees it.
        type(unittest_type), allocatable :: p1(:), p2(:), p3(:), p4(:), p5(:)

        p1 = [ &
            new_unittest("writing the same column twice on a schema-less writer aborts", &
                test_write_column_twice_no_schema_aborts), &
            new_unittest("table: a code-declared qc bound the data violates aborts", &
                test_table_qc_violation_aborts), &
            new_unittest("table: a maml extra: sort: on a slice-regime open aborts", &
                test_table_slice_maml_sort_aborts), &
            new_unittest("table: a filter naming a column the file lacks aborts", &
                test_table_filter_unknown_column_aborts), &
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
            new_unittest("opening a nonexistent file for reading aborts", &
                test_open_reader_missing_file_aborts), &
            new_unittest("parquet_open_reader(nrows=) with a filter matching zero rows aborts", &
                test_open_reader_nrows_zero_rows_aborts), &
            new_unittest("opening a writer at a bad path aborts", &
                test_open_writer_bad_path_aborts), &
            new_unittest("opening a writer with a schema that was never built aborts", &
                test_open_writer_empty_schema_aborts), &
            new_unittest("writing an over-length string into a fixed-size string matrix column aborts", &
                test_write_string_matrix_exceeds_array_size_aborts), &
            new_unittest("writing an over-length string into a fixed-size string vector column (flat form) aborts", &
                test_write_string_exceeds_array_size_aborts), &
            new_unittest("protected_cols: referencing an unknown field aborts", &
                test_validate_protected_cols_unknown_name_aborts), &
            new_unittest("nullable_cols: referencing an unknown field aborts", &
                test_validate_nullable_cols_unknown_name_aborts), &
            new_unittest("one column under both protected_cols: and nullable_cols: aborts", &
                test_validate_protected_and_nullable_overlap_aborts), &
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
            new_unittest("schema%set_nullable on an unknown column aborts", &
                test_set_nullable_unknown_column_aborts), &
            new_unittest("schema%set_nullable on a protected column aborts", &
                test_set_nullable_on_protected_column_aborts), &
            new_unittest("schema%set_protected on a column declared nullable aborts", &
                test_set_protected_on_nullable_column_aborts), &
            new_unittest("unprotecting a protected column warns without naming a MAML, and " // &
                "never warns for a column that was not protected", test_set_protected_unprotect_warns), &
            new_unittest("undeclaring a nullable column is silent, where unprotecting warns", &
                test_set_nullable_undeclare_is_silent), &
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
            new_unittest("a negative random work floor aborts while 0 and a positive value are accepted", &
                test_set_random_parallel_min_elements_negative_aborts), &
            new_unittest("parquet_write_table through a schema declaring no fields aborts", &
                test_write_table_schema_init_no_fields_aborts), &
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
                test_table_require_columns_missing_aborts) &
            ]
        p2 = [ &
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
            new_unittest("parquet_list_column init with a vector payload kind aborts", &
                test_list_init_unsupported_payload_aborts), &
            new_unittest("parquet_list_column append_row before init aborts", &
                test_list_append_before_init_aborts), &
            new_unittest("parquet_list_column append_row of the wrong element type aborts", &
                test_list_append_wrong_kind_aborts), &
            new_unittest("parquet_list_column append_row with a short validity mask aborts", &
                test_list_append_mask_length_aborts), &
            new_unittest("parquet_list_column view of a row that does not exist aborts", &
                test_list_view_out_of_range_aborts), &
            new_unittest("parquet_list_row accessor on an unassigned handle aborts", &
                test_list_unassociated_handle_aborts), &
            new_unittest("parquet_list_row get into the wrong element type aborts", &
                test_list_get_wrong_kind_aborts), &
            new_unittest("parquet_list_column gather_rows naming a row that does not exist aborts", &
                test_list_gather_out_of_range_aborts), &
            new_unittest("parquet_column adopt_container of an unallocated container aborts", &
                test_list_adopt_not_allocated_aborts), &
            new_unittest("parquet_column paste into a container column aborts", &
                test_list_column_paste_refused_aborts), &
            new_unittest("appending a map onto a list aborts", &
                test_container_append_wrong_container_kind_aborts), &
            new_unittest("appending across list element kinds aborts", &
                test_container_append_wrong_element_kind_aborts), &
            new_unittest("appending a transposed struct aborts", &
                test_container_append_struct_field_mismatch_aborts), &
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
                test_table_get_element_kind_mismatch_aborts), &
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
            new_unittest("row_index_name= together with schema= aborts", &
                test_table_write_row_index_with_schema_aborts), &
            new_unittest("a blank row_index_name= aborts", &
                test_table_write_row_index_blank_aborts), &
            new_unittest("row_index_name= naming a written column aborts", &
                test_table_write_row_index_collides_aborts), &
            new_unittest("row_index_name= on an in-memory table aborts", &
                test_table_write_row_index_in_memory_aborts), &
            new_unittest("row_index_name= on a detached table aborts", &
                test_table_write_row_index_detached_aborts), &
            new_unittest("row_index_name= under the reserved name warns and writes", &
                test_table_write_row_index_reserved_warns), &
            new_unittest("metadata_keys= naming a writer-generated key aborts", &
                test_table_copy_metadata_regenerated_key_aborts), &
            new_unittest("metadata_keys= naming an ordinary key still carries it", &
                test_table_copy_metadata_regenerated_control_runs), &
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
                test_table_reload_in_memory_column_aborts) &
            ]
        p3 = [ &
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
            new_unittest("append of a table to itself aborts", &
                test_table_append_self_aborts), &
            new_unittest("append of a DIFFERENT table still works", &
                test_table_append_other_still_works), &
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
                test_table_cast_int_overflow_aborts), &
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
            new_unittest("sorting by a container column aborts", &
                test_container_sort_key_aborts), &
            new_unittest("an unrecognized list_columns= token aborts naming both values", &
                test_container_bad_list_columns_aborts), &
            new_unittest("%print_stat reports a container column's row-length extremes", &
                test_container_print_stat_lengths), &
            new_unittest("reading a container column a mutation skipped aborts", &
                test_container_skipped_by_mutation_aborts), &
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
            new_unittest("%print_stat over a NaN-bearing float column prints rather than aborting", &
                test_table_print_stat_nan_survives), &
            new_unittest("%print_stat leaves a NaN out of a float column's min and max", &
                test_table_print_stat_nan_excluded), &
            new_unittest("a NaN stays out of a write-time qc violation's reported data range", &
                test_qc_warning_float64_nan), &
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
                test_reindex_trusted_length_aborts) &
            ]
        p4 = [ &
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
            new_unittest("parquet_get_column_arrow_type aborts only on a missing column", &
                test_arrow_type_unknown_column_aborts), &
            new_unittest("%print_stat names an unsupported column's stored Arrow type", &
                test_table_print_stat_unsupported_column), &
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
            new_unittest("parquet_timestamp get on a null element aborts", test_temporal_ts_get_null_aborts), &
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
                test_mask_chunk_row_mask_after_row_mask_aborts) &
            ]
        p5 = [ &
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
            new_unittest("an over-long invalid mode is quoted back capped, never in full", &
                test_invalid_mode_message_is_capped), &
            new_unittest("parquet_column_exists with an unrecognized types= token aborts", &
                test_column_exists_bad_type_token_aborts), &
            new_unittest("parquet_column_exists validates types= before checking the column exists", &
                test_column_exists_bad_type_token_missing_column_aborts), &
            new_unittest("parquet_column_exists with a blank types= filter aborts", &
                test_column_exists_empty_type_filter_aborts), &
            new_unittest("parquet_string_column set_validity with a short mask aborts", &
                test_string_column_set_validity_length_mismatch_aborts), &
            new_unittest("parquet_string_column set_where with a short mask aborts", &
                test_string_column_set_where_length_mismatch_aborts), &
            new_unittest("column gather_from with an out-of-range source row aborts", &
                test_column_gather_from_out_of_range_aborts), &
            new_unittest("column gather_from with a mask of the wrong length aborts", &
                test_column_gather_from_mask_length_mismatch_aborts), &
            new_unittest("column gather_from from a container column aborts", &
                test_column_gather_from_container_source_aborts), &
            new_unittest("string column gather_from with an out-of-range index aborts", &
                test_string_column_gather_from_out_of_range_aborts), &
            new_unittest("string column gather_from with a short mask aborts", &
                test_string_column_gather_from_length_mismatch_aborts), &
            new_unittest("%print_stat's statistics scan prints every kind's nulls, min and max, serially", &
                test_table_print_stat_scan_serial), &
            new_unittest("%print_stat's statistics scan prints the same rows on a team", &
                test_table_print_stat_scan_parallel), &
            new_unittest("%print_stat(stats=.false.) lists kind and width only", &
                test_table_print_stat_no_stats), &
            new_unittest("column row_validity_range with a range past the end aborts", &
                test_column_row_validity_range_out_of_range_aborts), &
            new_unittest("column row_validity_range with a mask shorter than the range aborts", &
                test_column_row_validity_range_short_mask_aborts), &
            new_unittest("parquet_derive_schema on a table with no resident column aborts", &
                test_derive_schema_needs_a_resident_column_aborts), &
            new_unittest("parquet_open_writer_like on a table with no resident column aborts", &
                test_open_writer_like_needs_a_resident_column_aborts), &
            new_unittest("parquet_write_table_chunk on a table with no rows aborts", &
                test_write_table_chunk_zero_rows_aborts), &
            new_unittest("parquet_write_table_chunk with a row group already open aborts with the writer's message", &
                test_write_table_chunk_row_group_already_open_aborts), &
            new_unittest("parquet_write_table_chunk through a schema naming a column the table lacks aborts", &
                test_write_table_chunk_schema_names_a_missing_column_aborts), &
            new_unittest("a Null in a protected column aborts a chunk write, naming parquet_write_column_chunk", &
                test_write_table_chunk_protected_null_aborts), &
            new_unittest("a sink refuses an appended column its template did not declare", &
                test_sink_extra_column_refused_aborts), &
            new_unittest("a sink refuses a kind mismatch with %append's own message", &
                test_sink_kind_mismatch_refused_aborts), &
            new_unittest("closing a sink twice aborts", test_sink_double_close_aborts), &
            new_unittest("appending to a closed sink aborts", test_sink_use_after_close_aborts), &
            new_unittest("assigning a parquet_table_writer aborts", test_sink_assignment_refused_aborts), &
            new_unittest("a sink's schema naming a column the template lacks aborts at open", &
                test_sink_schema_names_a_missing_column_aborts), &
            new_unittest("appending to a sink that was never opened aborts", test_sink_never_opened_aborts), &
            new_unittest("a sink with chunk_size <= 0 aborts", test_sink_chunk_size_not_positive_aborts), &
            new_unittest("a sink over a template with no resident column and no schema aborts", &
                test_sink_template_has_no_column_aborts), &
            new_unittest("appending to a shared sink from inside a parallel region aborts", &
                test_sink_shared_in_parallel_aborts) &
            ]
        testsuite = [p1, p2, p3, p4, p5]
    end subroutine collect_tests_parquet_table_errors

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
    !! care which thread set it, which is exactly what makes the hook enough.
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
            ! The appended stored type is the point: "not supported" alone says which column is
            ! the problem and not what it is, and PK_NONE is the same answer for every one of
            ! them. Asserted as one substring so the clause cannot drift away from the sentence.
            required_stderr="this column's type is not supported by parquet_table, so its values " // &
                "were never read (stored as map<int32, int32 ('m_intkey')>)")
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

    subroutine test_table_write_row_index_with_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_with_schema", expect_abort=.true., &
            failure_message="row_index_name= alongside schema= was expected to error stop", &
            required_stderr="row_index_name= and schema= cannot both be given")
    end subroutine test_table_write_row_index_with_schema_aborts

    subroutine test_table_write_row_index_blank_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_blank", expect_abort=.true., &
            failure_message="a blank row_index_name= was expected to error stop", &
            required_stderr="row_index_name= must not be blank")
    end subroutine test_table_write_row_index_blank_aborts

    subroutine test_table_write_row_index_collides_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_collides", expect_abort=.true., &
            failure_message="row_index_name= naming a written column was expected to error stop", &
            required_stderr="already the name of a column this write is writing")
    end subroutine test_table_write_row_index_collides_aborts

    subroutine test_table_write_row_index_in_memory_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_in_memory", expect_abort=.true., &
            failure_message="row_index_name= on an in-memory table was expected to error stop", &
            required_stderr="was not read from a parquet file")
    end subroutine test_table_write_row_index_in_memory_aborts

    subroutine test_table_write_row_index_detached_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_detached", expect_abort=.true., &
            failure_message="row_index_name= on a detached table was expected to error stop", &
            required_stderr="materialize it BEFORE the mutation that detaches")
    end subroutine test_table_write_row_index_detached_aborts

    !> The negative control for the four refusals above: the reserved name is warned about, not
    !! refused, so this exits cleanly with the warning on the captured output.
    subroutine test_table_write_row_index_reserved_warns(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_write_row_index_reserved_warning", &
            expect_abort=.false., &
            failure_message="row_index_name= under the reserved name was expected to warn and write", &
            required_stderr="is the reserved name of the automatic row-index column")
    end subroutine test_table_write_row_index_reserved_warns

    subroutine test_table_copy_metadata_both_forms_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_metadata_both_forms", expect_abort=.true., &
            failure_message="copy_metadata= with metadata_keys= was expected to abort", &
            required_stderr="copy_metadata=.true. and metadata_keys= cannot both be given")
    end subroutine test_table_copy_metadata_both_forms_aborts

    subroutine test_table_copy_metadata_regenerated_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_copy_metadata_regenerated_key", expect_abort=.true., &
            failure_message="naming a writer-generated key in metadata_keys= was expected to abort", &
            required_stderr="which the writer generates itself from the output schema")
    end subroutine test_table_copy_metadata_regenerated_key_aborts

    subroutine test_table_copy_metadata_regenerated_control_runs(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "table_copy_metadata_regenerated_control", expect_abort=.false., &
            failure_message="negative control: an ordinary metadata_keys= entry should still be carried")
    end subroutine test_table_copy_metadata_regenerated_control_runs

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

    !> %print_stat over a float column holding a NaN must PRINT, not abort.
    !!
    !! The failure this pins is invisible to every compiler but one: `min`/`max` over a NaN compile
    !! to x86 `minsd`/`maxsd`, which raise IEEE_INVALID for a quiet-NaN operand, and nagfor unmasks
    !! the IEEE traps by default (`-ieee=stop`). Reverting the screen in
    !! tools/generate_parquet_tables.py takes this scenario from exit 0 to exit 134 under
    !! `--profile release`, with the report's header printed and not one column row -- which is
    !! also why the sibling test below, asserting the printed values, has teeth of its own.
    subroutine test_table_print_stat_nan_survives(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "table_print_stat_nan", expect_abort=.false., &
            failure_message="%print_stat on a float column holding a NaN must print rather than abort")
    end subroutine test_table_print_stat_nan_survives

    !> ... and the NaN must stay out of the min/max, in all four float specifics.
    !!
    !! Each value asserted here is one the ordering can only reach with the NaN excluded: the
    !! scalar float64 column is `[1, NaN, 3]`, and a NaN admitted to the comparison takes the
    !! answer to a processor-dependent value rather than to 3. The all-NaN column is the other
    !! half of the rule -- it reports NaN rather than the "-" that means "nothing to report".
    subroutine test_table_print_stat_nan_excluded(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "table_print_stat_nan", "3.00000", expect_on="stdout", &
            failure_message="the float64 column's max should be 3, with the NaN left out")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_nan", "36.0000", expect_on="stdout", &
            failure_message="the float32 VECTOR column's max should be 36, with the NaN left out")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_nan", "NaN", expect_on="stdout", &
            failure_message="a column whose every value is NaN should report NaN, not ""-""")
    end subroutine test_table_print_stat_nan_excluded

    !> The twelve rows scenario_table_print_stat_scan prints, one per column kind, asserted WHOLE --
    !! name, kind, width, null count, minimum and maximum, padded exactly as %print_stat pads them --
    !! so a count or an extreme moved by one row at a block boundary cannot pass.
    !! Shared by the serial and the on-a-team wrapper, which differ only in the "parallel scan:"
    !! line they then assert.
    subroutine check_print_stat_scan_rows(error, scenario)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario !! the scenario whose stdout carries the rows.
        character(len=88), parameter :: rows(12) = [character(len=88) :: &
            "  i32     PK_INT32            1       6           1000002                 1010006", &
            "  i64     PK_INT64            1       2           2000000000001           2000000010007", &
            "  f64     PK_FLOAT64          1       2           0.500000                5003.00", &
            "  f32     PK_FLOAT32          1       2           NaN                     NaN", &
            "  bool    PK_LOGICAL          1       2           T:3335                  F:6670", &
            "  str     PK_STRING           1       3           aaa                     zzz", &
            "  i32v    PK_INT32_VEC        16      2           101                     1000616", &
            "  f64v    PK_FLOAT64_VEC      2       1           1.50000                 2503.75", &
            "  boolv   PK_LOGICAL_VEC      2       1           T:5003                  F:15009", &
            "  date    PK_DATE             1       2           2020-01-01              2047-05-25", &
            "  datev   PK_DATE_VEC         2       1           2020-01-01              2102-02-26", &
            "  strv    PK_STRING_VEC       2       1           v00001a                 v10007b"]
        integer :: k

        do k = 1, size(rows)
            call check_scenario_streams(error, scenario, trim(rows(k)), "stdout", &
                "column " // trim(rows(k)(3:8)) // " must print exactly this statistics row: " // trim(rows(k)))
            if (allocated(error)) return
        end do
    end subroutine check_print_stat_scan_rows

    !> The block-wise scan on one thread: every kind's row, and the scan really was serial.
    subroutine test_table_print_stat_scan_serial(error)
        type(error_type), allocatable, intent(out) :: error
        call check_print_stat_scan_rows(error, "table_print_stat_scan_serial")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_scan_serial", "parallel scan: no", "stdout", &
            "parquet_set_table_threads(1) must keep the statistics pass on one thread")
    end subroutine test_table_print_stat_scan_serial

    !> The same rows from a scan the fixture is large enough to spread over a team, plus the proof
    !! that the team was opened -- without which this would be the serial test run twice.
    !!
    !! Skipped where no team can open: without OpenMP, or with fewer than two threads or two
    !! processors, the pass resolves to one thread and the rows would be asserted against the path
    !! the serial test already covers.
    subroutine test_table_print_stat_scan_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads, nprocs

        nthreads = 1
        nprocs = 1
        !$ nthreads = omp_get_max_threads()
        !$ nprocs = omp_get_num_procs()
        if (nthreads < 2 .or. nprocs < 2) then
            call skip_test(error, "needs OpenMP with at least two threads and two processors: the " // &
                "statistics pass resolves to one thread otherwise, and its team would go untested")
            return
        end if
        call check_print_stat_scan_rows(error, "table_print_stat_scan_parallel")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_scan_parallel", "parallel scan: yes", "stdout", &
            "the statistics pass was expected to open a team on a fixture above the work floor")
    end subroutine test_table_print_stat_scan_parallel

    !> stats=.false. drops the three statistics columns from the header and every row, keeps the
    !! kind and width, and moves the edited marker to after the width.
    subroutine test_table_print_stat_no_stats(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "table_print_stat_no_stats", "  column  kind                width", &
            "stdout", "the header must end at the width column")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_no_stats", "  a       PK_INT32            1 *", &
            "stdout", "a written-into resident column must show its width and then the edited marker")
        if (allocated(error)) return
        call check_scenario_streams(error, "table_print_stat_no_stats", "  b       PK_INT32            1", &
            "stdout", "an unread column must still list its kind and width")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "table_print_stat_no_stats", expect_abort=.false., &
            failure_message="printing without statistics was not expected to abort", &
            forbidden_text="nulls")
    end subroutine test_table_print_stat_no_stats

    subroutine test_column_row_validity_range_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_row_validity_range_out_of_range", &
            expect_abort=.true., &
            failure_message="a row range reaching past the column was expected to abort", &
            required_stderr="row_validity_range: row range out of range")
    end subroutine test_column_row_validity_range_out_of_range_aborts

    subroutine test_column_row_validity_range_short_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_row_validity_range_short_mask", &
            expect_abort=.true., &
            failure_message="a mask shorter than the row range was expected to abort", &
            required_stderr="row_validity_range: valid is shorter than the row range")
    end subroutine test_column_row_validity_range_short_mask_aborts

    !> A NaN is a write-time qc violation, and reporting it must not require ordering it.
    !!
    !! `qc_numeric_r64` accumulates the observed data range with `min`/`max`, which compile to x86
    !! `minsd`/`maxsd` and raise IEEE_INVALID for a quiet-NaN operand -- so under nagfor's default
    !! `-ieee=stop` the writer aborted on the way to the warning rather than naming the column.
    !! Reverting the screen in src/parquet_write_numeric.f90 takes this scenario from exit 0 to
    !! exit 134 under `--profile release`, with no warning printed at all.
    !!
    !! The two ranges are the assertion: `[2, 9]` can only be reached with the NaN left out of the
    !! ordering, and `[NaN, NaN]` is the all-NaN column reporting what it holds instead of the
    !! zero the accumulators start at -- which would read as a real observed range of [0, 0].
    subroutine test_qc_warning_float64_nan(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status(error, "qc_warning_float64_nan", expect_abort=.false., &
            failure_message="a qc violation on float64 data holding a NaN must warn rather than abort")
        if (allocated(error)) return
        call check_scenario_streams(error, "qc_warning_float64_nan", "data range [2, 9]", &
            expect_on="stdout", &
            failure_message="the reported range should be over the non-NaN values")
        if (allocated(error)) return
        call check_scenario_streams(error, "qc_warning_float64_nan", "data range [NaN, NaN]", &
            expect_on="stdout", &
            failure_message="an all-NaN column should report NaN as its range, not [0, 0]")
    end subroutine test_qc_warning_float64_nan

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
        ! assertion above.
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

    subroutine test_string_column_set_validity_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_set_validity_length_mismatch", &
            expect_abort=.true., &
            failure_message="set_validity with a short mask was expected to abort", &
            required_stderr="parquet_strings: set_validity: mask has 1 entries but the column has 2 elements")
    end subroutine test_string_column_set_validity_length_mismatch_aborts

    subroutine test_string_column_set_where_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_set_where_length_mismatch", &
            expect_abort=.true., &
            failure_message="set_where with a short mask was expected to abort", &
            required_stderr="parquet_strings: set_where: mask has 1 entries but the column has 2 elements")
    end subroutine test_string_column_set_where_length_mismatch_aborts

    subroutine test_column_gather_from_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_gather_from_out_of_range", &
            expect_abort=.true., &
            failure_message="gather_from of a row outside the source was expected to abort", &
            required_stderr="gather_from: row index 9 is outside the source column's 1..3 rows")
    end subroutine test_column_gather_from_out_of_range_aborts

    subroutine test_column_gather_from_mask_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_gather_from_mask_length_mismatch", &
            expect_abort=.true., &
            failure_message="gather_from with a mask of the source's length was expected to abort", &
            required_stderr="gather_from: mask has 3 entries but the index list names 2 rows")
    end subroutine test_column_gather_from_mask_length_mismatch_aborts

    subroutine test_column_gather_from_container_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_gather_from_container_source", &
            expect_abort=.true., &
            failure_message="gather_from from a container column was expected to abort", &
            required_stderr="gather_from: a container column is gathered in place with %gather, " // &
                "not from another column")
    end subroutine test_column_gather_from_container_source_aborts

    subroutine test_string_column_gather_from_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_gather_from_out_of_range", &
            expect_abort=.true., &
            failure_message="string gather_from of an element outside the source was expected to abort", &
            required_stderr="parquet_strings: gather_from: index out of range")
    end subroutine test_string_column_gather_from_out_of_range_aborts

    subroutine test_string_column_gather_from_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_gather_from_length_mismatch", &
            expect_abort=.true., &
            failure_message="string gather_from with a short mask was expected to abort", &
            required_stderr="parquet_strings: gather_from: mask has 1 entries but the index list names 2 elements")
    end subroutine test_string_column_gather_from_length_mismatch_aborts

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
            failure_message="initializing a container kind through init was expected to abort", &
            required_stderr="parquet_columns: init: a container column is built with adopt_container, not init")
    end subroutine test_columns_init_container_kind_aborts

    subroutine test_list_init_unsupported_payload_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_init_unsupported_payload", expect_abort=.true., &
            failure_message="a vector payload kind is nesting and was expected to be refused", &
            required_stderr="parquet_list: init: PK_INT32_VEC is not a supported list payload kind")
    end subroutine test_list_init_unsupported_payload_aborts

    subroutine test_list_append_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_append_before_init", expect_abort=.true., &
            failure_message="appending to a list column with no payload kind was expected to abort", &
            required_stderr="parquet_list: append_row: this list column has no payload kind")
    end subroutine test_list_append_before_init_aborts

    subroutine test_list_append_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_append_wrong_kind", expect_abort=.true., &
            failure_message="appending int64 values to an int32 list column was expected to abort", &
            required_stderr="parquet_list: append_row: this is a int32 list column; int64 values cannot be appended")
    end subroutine test_list_append_wrong_kind_aborts

    subroutine test_list_append_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_append_mask_length", expect_abort=.true., &
            failure_message="a validity mask of the wrong length was expected to be refused", &
            required_stderr="parquet_list: append_row: is_valid has a different length from values")
    end subroutine test_list_append_mask_length_aborts

    subroutine test_list_view_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_view_out_of_range", expect_abort=.true., &
            failure_message="viewing a row past the end of the column was expected to abort", &
            required_stderr="parquet_list: view: row index is out of range")
    end subroutine test_list_view_out_of_range_aborts

    subroutine test_list_unassociated_handle_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_unassociated_handle", expect_abort=.true., &
            failure_message="reading through a handle that refers to no column was expected to abort", &
            required_stderr="parquet_list: length: this row handle is not associated with a column")
    end subroutine test_list_unassociated_handle_aborts

    subroutine test_list_get_wrong_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_get_wrong_kind", expect_abort=.true., &
            failure_message="reading a float64 list column into int32 values was expected to abort", &
            required_stderr="parquet_list: get: this is a float64 list column; it cannot be read into int32 values")
    end subroutine test_list_get_wrong_kind_aborts

    subroutine test_list_gather_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_gather_out_of_range", expect_abort=.true., &
            failure_message="gathering a source row past the end of the column was expected to abort", &
            required_stderr="parquet_list: gather_rows: source row index out of range")
    end subroutine test_list_gather_out_of_range_aborts

    subroutine test_list_adopt_not_allocated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_adopt_not_allocated", expect_abort=.true., &
            failure_message="adopting an unallocated container was expected to abort", &
            required_stderr="parquet_columns: adopt_container: the container is not allocated")
    end subroutine test_list_adopt_not_allocated_aborts

    subroutine test_list_column_paste_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "list_column_paste_refused", expect_abort=.true., &
            failure_message="pasting into a container column was expected to be refused", &
            required_stderr="parquet_columns: paste: a container column cannot be overwritten in place")
    end subroutine test_list_column_paste_refused_aborts

    !> The three `append_from` layout refusals. Each is a SILENT WRONG ANSWER if it does not
    !! fire: a map read as a list, a payload read at the wrong kind, or two struct field columns
    !! transposed -- none of which changes a row count, so nothing downstream would notice.
    subroutine test_container_append_wrong_container_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_append_wrong_container_kind", &
            expect_abort=.true., &
            failure_message="appending a map column onto a list column was expected to abort", &
            required_stderr="append_from: cannot append a map<string,int32> onto a list<int32>")
    end subroutine test_container_append_wrong_container_kind_aborts

    subroutine test_container_append_wrong_element_kind_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_append_wrong_element_kind", &
            expect_abort=.true., &
            failure_message="appending list<int64> onto list<int32> was expected to abort", &
            required_stderr="append_from: cannot append a list<int64> onto a list<int32>")
    end subroutine test_container_append_wrong_element_kind_aborts

    subroutine test_container_append_struct_field_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_append_struct_field_mismatch", &
            expect_abort=.true., &
            failure_message="appending struct<b,a> onto struct<a,b> was expected to abort", &
            required_stderr="append_from: field 1 is 'b' in the source and 'a' here")
    end subroutine test_container_append_struct_field_mismatch_aborts

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
    !> null, and the row count -- on the whole-column path and on the row-group-scoped one, which
    !> converts the view array through its own code. This test asserted the OPPOSITE until the compact path learned to
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

    !> Same rule as test_write_column_twice_aborts, but for a schema-less
    !> writer (no cinfo), which previously had no tracking at all for this.
    subroutine test_write_column_twice_no_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_column_twice_no_schema", expect_abort=.true., &
            failure_message="writing the same column twice on a schema-less writer was expected to error stop", &
            required_stderr="parquet_write_column: column written more than once: id")
    end subroutine test_write_column_twice_no_schema_aborts

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

    !> A MAML source line over 1024 characters used to be silently truncated by a fixed-length read
    !! (iostat still 0, no diagnostic). Now aborts, naming the offending line number, instead of
    !! producing incomplete metadata silently.
    subroutine test_maml_line_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "maml_line_too_long", expect_abort=.true., &
            failure_message="a MAML source line exceeding the length limit was expected to abort", &
            required_stderr="exceeds")
    end subroutine test_maml_line_too_long_aborts

    !> get_parquet_maml's `case default` arm -- the one both MAML generators emit from the same
    !! shared template, so this pins the message a downstream project's own generated parquet_maml
    !! produces too (doc/pages/utilities/embedding-maml-schemas.md).
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

    !> A `parquet_schema` that was declared and never built must be refused, not silently written
    !> as a file with no columns at all.
    !>
    !> **Asserted by MESSAGE and with a CONTROL, because exit status alone proves neither half.**
    !> The scenario opens and closes a schema-less writer first, so a failure here separates "the
    !> empty schema was rejected" from "this path, this filename or this writer could not be
    !> opened at all". And the required text is what tells the caller which of the two ways out
    !> they have -- an abort with the wrong message would leave them looking at the filename.
    subroutine test_open_writer_empty_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("open_writer_empty_schema", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "opening a writer with a schema that was never built was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: a schema-less writer", found)
        call check(error, found, &
            "a writer with NO schema must open first, or the abort below says nothing about the schema")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_open_writer: schema is empty -- build it with schema%init/%add_field", found)
        call check(error, found, &
            "the abort must name the empty schema and both ways of filling it in")
    end subroutine test_open_writer_empty_schema_aborts

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

    !> The nullable_cols: counterpart of the check above: an unknown name is refused rather than
    !! quietly declaring nothing. The message is asserted, since "validation rejected this MAML"
    !! is something several unrelated defects also produce.
    subroutine test_validate_nullable_cols_unknown_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_nullable_cols_unknown_name", &
            expect_abort=.true., &
            failure_message="nullable_cols: referencing a column not declared in fields: " // &
                "was expected to error stop", &
            required_stderr="nullable_cols: unknown column")
    end subroutine test_validate_nullable_cols_unknown_name_aborts

    !> protected_cols: and nullable_cols: are opposite declarations, so one column under both is a
    !! contradiction and is refused rather than resolved by an invented precedence. The message
    !! must name the column: a MAML carrying both keys legitimately (different columns) is the
    !! ordinary case, and rejecting THAT would be the regression this asserts against.
    subroutine test_validate_protected_and_nullable_overlap_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_protected_and_nullable_overlap", &
            expect_abort=.true., &
            failure_message="a column listed under both protected_cols: and nullable_cols: was " // &
                "expected to error stop", &
            required_stderr="is listed under both protected_cols: and nullable_cols:")
    end subroutine test_validate_protected_and_nullable_overlap_aborts

    !> A MAML key is case-insensitive, block headers included -- so `Extra:` must be read as the
    !! `extra:` section, and the `protected_cols:` inside it must still be validated. Before this
    !! was fixed the capitalized spelling validated cleanly, because the block was never found:
    !! silent, and it took a column's Null protection with it. The in-process half is
    !! `test_maml_block_headers_case_insensitive`; `check_maml_keys_case_insensitive` is the static one.
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

    !> set_nullable resolves its column through the same get_column_index every other setter uses,
    !! so an unknown name aborts there. The scenario declares a real column first, so a build that
    !! aborted on ANY set_nullable call would fail its own control rather than pass this.
    subroutine test_set_nullable_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_nullable_unknown_column", expect_abort=.true., &
            failure_message="schema%set_nullable on a column that does not exist was expected " // &
                "to error stop")
    end subroutine test_set_nullable_unknown_column_aborts

    !> The two declarations are mutually exclusive in code as well as in a MAML. Both directions
    !! are asserted because they are separate guards in separate procedures: one would keep
    !! passing while the other was deleted.
    subroutine test_set_nullable_on_protected_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_nullable_on_protected_column", &
            expect_abort=.true., &
            failure_message="declaring a protected column nullable was expected to error stop", &
            required_stderr="cannot be both protected and declared nullable")
    end subroutine test_set_nullable_on_protected_column_aborts

    !> The other direction of the same rule -- see the test above.
    subroutine test_set_protected_on_nullable_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_protected_on_nullable_column", &
            expect_abort=.true., &
            failure_message="protecting a column already declared nullable was expected to error stop", &
            required_stderr="cannot be both protected and declared nullable")
    end subroutine test_set_protected_on_nullable_column_aborts

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

    !> The mirror of the test above on the opposite declaration, and the half that had no test:
    !! `set_nullable(name, .false.)` must print NOTHING, where `set_protected(name, .false.)` on a
    !! protected column warns. `doc/pages/schema/building-schema-in-code.md` states the contrast
    !! explicitly, so the two bullets disagree with the library the moment either side moves.
    !!
    !! **Both halves are read from ONE captured run, which is what makes the absence assertion
    !! mean anything.** "Nothing mentions column 'a'" is satisfied for free by a broken capture, a
    !! renamed binary, or a scenario that died before either call; column 'p' having warned in the
    !! same output rules all three out. A guard that warned on every relaxation, rather than only
    !! on one that gives something up, is what the assertion itself catches.
    subroutine test_set_nullable_undeclare_is_silent(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_a, warned_p

        call run_error_scenario("set_nullable_undeclare_is_silent", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "undeclaring a nullable column should be silent, not abort")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'p' is currently protected", warned_p)
        call check(error, warned_p, &
            "positive control: unprotecting 'p' must still warn, or this run says nothing about silence")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'a'", warned_a)
        call check(error, .not. warned_a, &
            "set_nullable(a, .false.) must print nothing -- undeclaring relaxes nothing a caller relied on")
    end subroutine test_set_nullable_undeclare_is_silent

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

    !> The work floor accepts `0` -- which DISABLES it, and is the documented way a test asks for a
    !> team on a small array -- and refuses only a negative value.
    !>
    !> Those two facts are one rule, which is why the control matters more than usual here: a guard
    !> written `n <= 0` would abort on the very value the API promises to accept, and an
    !> exit-status-only test of the negative case could not tell the two guards apart. The scenario
    !> sets `0` and a positive value first and prints what the getter then reports.
    subroutine test_set_random_parallel_min_elements_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("set_random_parallel_min_elements_negative", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "a negative random work floor was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: 0 accepted, floor now 0", found)
        call check(error, found, &
            "0 must be ACCEPTED and must disable the floor, or the guard rejects the one value " // &
            "the API documents as meaningful")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: 1000 accepted, floor now 1000", found)
        call check(error, found, "an ordinary positive floor must be accepted and round-trip")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_set_random_parallel_min_elements: n must be >= 0", found)
        call check(error, found, "the abort must name this setter and its rule")
    end subroutine test_set_random_parallel_min_elements_negative_aborts

    !> `parquet_write_table` must not read an unparsed schema's `%cinfo`: `%get_num_fields` on one
    !> is uninitialized state, and the write loop turns that into a runaway allocation and an OOM
    !> kill rather than any diagnosable failure.
    !>
    !> A schema `%init`ed with no field added is the only state that reaches the parse call there
    !> (`%add_field` parses as it goes; a loaded or embedded MAML arrives parsed; a directly
    !> assigned `%maml` never had `%init` and takes the neighbouring "not built" abort). Its MAML
    !> is header-only, so the parse fails validation and says so. The control writes the same table
    !> through an ordinary schema first, which is what separates this from a table that could not
    !> be written at all.
    subroutine test_write_table_schema_init_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("write_table_schema_init_no_fields", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "writing a table through a schema that declares no fields was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: a table wrote through an ordinary schema", found)
        call check(error, found, &
            "the same table must write through an ordinary schema first, or the abort says " // &
            "nothing about the schema")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "no fields defined", found)
        call check(error, found, &
            "the abort must come from validating the header-only MAML and name the real problem")
    end subroutine test_write_table_schema_init_no_fields_aborts

    !> The control (one resident column) must derive first, or the abort says nothing about
    !> residency; then the lazily opened table with nothing read aborts with the library's message.
    subroutine test_derive_schema_needs_a_resident_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("derive_schema_needs_a_resident_column", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "parquet_derive_schema on a table with no resident column was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a table with a resident column derived a schema", found)
        call check(error, found, "the control must derive a schema first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_derive_schema: this table has no resident column to build a schema from", found)
        call check(error, found, "the abort must name parquet_derive_schema and the missing resident column")
    end subroutine test_derive_schema_needs_a_resident_column_aborts

    !> Same shape for parquet_open_writer_like: the control opens, writes and closes; the lazily
    !> opened table aborts before any output file exists, naming the alternative (schema=).
    subroutine test_open_writer_like_needs_a_resident_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("open_writer_like_needs_a_resident_column", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "parquet_open_writer_like on a table with no resident column was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a table with a resident column opened a writer", found)
        call check(error, found, "the control must open a writer first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_open_writer_like: this table has no resident column to derive a schema from", found)
        call check(error, found, "the abort must name parquet_open_writer_like and the missing resident column")
    end subroutine test_open_writer_like_needs_a_resident_column_aborts

    !> The zero-row refusal is the chunk write's own; the control is a two-row chunk first.
    subroutine test_write_table_chunk_zero_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("write_table_chunk_zero_rows", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "parquet_write_table_chunk on a table with no rows was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a two-row table was written as a row group", found)
        call check(error, found, "the control must write a row group first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_write_table_chunk: this table has no rows", found)
        call check(error, found, "the abort must name parquet_write_table_chunk and the empty table")
    end subroutine test_write_table_chunk_zero_rows_aborts

    !> The already-open refusal is the writer's, kept with its own message.
    subroutine test_write_table_chunk_row_group_already_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("write_table_chunk_row_group_already_open", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "a chunk write into a writer with a row group open was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a chunk was written while no row group was open", found)
        call check(error, found, "the control must write a chunk first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_new_row_group: a row group is already open -- call parquet_finish_row_group first", found)
        call check(error, found, "the abort must be the writer's own, naming the call that has to come first")
    end subroutine test_write_table_chunk_row_group_already_open_aborts

    !> The shared locator names the calling procedure: the chunk write, not parquet_write_table.
    subroutine test_write_table_chunk_schema_names_a_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("write_table_chunk_schema_names_a_missing_column", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "a chunk write through a schema naming a missing column was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a table holding every declared column was written as a row group", found)
        call check(error, found, "the control must write through the same schema first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_write_table_chunk: the schema declares a column the table does not have", found)
        call check(error, found, "the abort must name parquet_write_table_chunk, not parquet_write_table")
    end subroutine test_write_table_chunk_schema_names_a_missing_column_aborts

    !> The protected check names the chunk specific it was reached through.
    subroutine test_write_table_chunk_protected_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("write_table_chunk_protected_null", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "a Null in a protected column was expected to abort a chunk write")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "control: a Null-free chunk of a protected column was written", found)
        call check(error, found, "the control must write a Null-free chunk first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_write_column_chunk: column 'x' is protected (extra: protected_cols:) and cannot contain Null values", &
            found)
        call check(error, found, "the abort must name parquet_write_column_chunk and the protected column")
    end subroutine test_write_table_chunk_protected_null_aborts

    !> The sink's own refusal of an undeclared resident column, naming the column and the file.
    subroutine test_sink_extra_column_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_extra_column_refused", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_extra_column_refused was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: a table with the template's columns was appended", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "append: this output file has no column 'b' -- the output's columns were fixed when it was opened", found)
        call check(error, found, "the abort must be the sink's own message, naming the column")
    end subroutine test_sink_extra_column_refused_aborts

    !> A kind mismatch keeps %append's own message: no silent widening.
    subroutine test_sink_kind_mismatch_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_kind_mismatch_refused", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_kind_mismatch_refused was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: a table of the template's kind was appended", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "append: this column is PK_INT32 here but PK_FLOAT64 in the appended table", found)
        call check(error, found, "the abort must be %append's kind message")
    end subroutine test_sink_kind_mismatch_refused_aborts

    !> A second close is refused, not idempotent.
    subroutine test_sink_double_close_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_double_close", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_double_close was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: the sink was closed once", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_close_table_writer: this output file has already been closed", found)
        call check(error, found, "the abort must name parquet_close_table_writer and the closed file")
    end subroutine test_sink_double_close_aborts

    !> An append after the close is refused, naming the call and the file.
    subroutine test_sink_use_after_close_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_use_after_close", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_use_after_close was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: the sink accepted rows and was closed", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "append: this output file has already been closed", found)
        call check(error, found, "the abort must name append and the closed file")
    end subroutine test_sink_use_after_close_aborts

    !> Assignment of a sink is blocked: the writer's handle has no reference counting.
    subroutine test_sink_assignment_refused_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_assignment_refused", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_assignment_refused was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: the sink was opened", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "assignment is not supported (it would leave two writers sharing one output file)", found)
        call check(error, found, "the abort must be the assignment guard's message")
    end subroutine test_sink_assignment_refused_aborts

    !> A schema field the template lacks is refused at open, before any file exists.
    subroutine test_sink_schema_names_a_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_schema_names_a_missing_column", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_schema_names_a_missing_column was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: the schema opened over a template that has every field", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_open_table_writer: the schema declares a column the template table does not have", found)
        call check(error, found, "the abort must name parquet_open_table_writer and the missing column")
    end subroutine test_sink_schema_names_a_missing_column_aborts

    !> An append on a never-opened sink names the call that has to come first.
    subroutine test_sink_never_opened_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_never_opened", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_never_opened was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: an opened sink accepted rows", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "append: this output file has not been opened; call parquet_open_table_writer first", found)
        call check(error, found, "the abort must say the sink was never opened")
    end subroutine test_sink_never_opened_aborts

    !> chunk_size is the flush threshold and must be positive.
    subroutine test_sink_chunk_size_not_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_chunk_size_not_positive", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_chunk_size_not_positive was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: chunk_size=1 opened, wrote and closed", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_open_table_writer: chunk_size must be positive", found)
        call check(error, found, "the abort must name chunk_size")
    end subroutine test_sink_chunk_size_not_positive_aborts

    !> A template that has read nothing, with no schema, gives the output no column: refused.
    subroutine test_sink_template_has_no_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("sink_template_has_no_column", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "scenario sink_template_has_no_column was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "control: a template with one resident column opened a sink", found)
        call check(error, found, "the control must succeed first, or the abort proves nothing")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_open_table_writer: the template table has no resident column, so there is nothing to write", found)
        call check(error, found, "the abort must say the template has no resident column and name the alternatives")
    end subroutine test_sink_template_has_no_column_aborts

    !> A sink opened outside a parallel region and appended to from inside it is refused with the
    !! sink's own message; the marker proves the guard keys on ownership, not on the region --
    !! the sink this thread opened inside the region went through first.
    subroutine test_sink_shared_in_parallel_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a parallel region the sink cannot be shared, so " // &
            "both appends succeed and the scenario exits 0")
        return
#endif
        call run_error_scenario("sink_shared_in_parallel", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "appending to a shared sink from inside a parallel region was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "private sink inside the region succeeded", found)
        call check(error, found, "the sink this thread opened inside the region must go through first")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "append: this output file is being used from more than one thread", found)
        call check(error, found, "the abort must be the sink's own shared-use message")
    end subroutine test_sink_shared_in_parallel_aborts

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

        ! doc/pages/operating/thread-safety.md explicitly promises a diagnostic
        ! on stderr for this case, not just a bare abort -- check that promise
        ! from the same run as the exit-status check, rather than re-running
        ! the (race-dependent) scenario a second time. (README carried that
        ! promise in a "Thread safety" SECTION until it became a landing page;
        ! the guide is its home now, and the README feature bullet points there.)
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
    !> The message must name parquet_get_column_arrow_type: "column not found" comes from the
    !> shared check_column_exists guard, so asserting the bare phrase would pass against any of
    !> the ninety-odd queries that call it. The scenario's own negative control (see its comment)
    !> covers the other half -- that the query does not abort for a type it cannot describe.
    subroutine test_arrow_type_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "arrow_type_unknown_column", expect_abort=.true., &
            failure_message="parquet_get_column_arrow_type on a missing column was expected to abort", &
            required_stderr="parquet_get_column_arrow_type: column not found in parquet file: no_such_column")
    end subroutine test_arrow_type_unknown_column_aborts
    !> The kind cell of an unsupported column carries its stored Arrow type. Asserted through the
    !> scenario harness rather than in-process because %print_stat prints to stdout; the harness
    !> searches both captured streams. The scenario's own negative control (see its comment)
    !> covers the other direction -- that a SUPPORTED dictionary column is unaffected.
    subroutine test_table_print_stat_unsupported_column(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_print_stat_unsupported_column", expect_abort=.false., &
            failure_message="%print_stat(all=.true.) over an unsupported column was expected to run to completion", &
            required_stderr="dictionary<values=binary, indices=int8, ordered=0>")
    end subroutine test_table_print_stat_unsupported_column

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

    !> Both invalid-mode messages quote an over-long `mode` CAPPED at 100 characters plus an
    !> ellipsis, never in full.
    !>
    !> **The cap is a correctness guard, not a tidiness one.** `mode` is caller-supplied and
    !> unbounded, and ifx 2026.1.1's ERROR STOP runtime corrupts the heap once the composed message
    !> reaches 8192 bytes -- so a guard reporting the value verbatim would turn a clean abort into
    !> a crash on precisely the input that triggers it.
    !>
    !> Asserting the cap needs BOTH halves: that the first 100 characters and the ellipsis are
    !> there, and that the marker past them is not. The first alone passes against no cap at all.
    !> The two guards are separate code in separate modules (parquet_version and
    !> parquet_settings), so both are swept here rather than one standing for the other.
    subroutine test_invalid_mode_message_is_capped(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found
        integer :: k
        character(len=40) :: names(2)
        character(len=1) :: fill(2)

        names(1) = "get_version_invalid_mode_long"
        names(2) = "get_arrow_version_invalid_mode_long"
        fill(1) = "x"
        fill(2) = "y"

        do k = 1, 2
            call run_error_scenario(trim(names(k)), exitstat, cmdstat, out_file, err_file)
            call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
            if (allocated(error)) return
            call check(error, exitstat /= 0, "an invalid mode was expected to abort: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, repeat(fill(k), 100) // "...", found)
            call check(error, found, &
                "the message must quote the first 100 characters of the mode and then an " // &
                "ellipsis: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, "TAIL_MUST_NOT_APPEAR", found)
            call check(error, .not. found, &
                "the message must NOT carry anything past the cap -- without this the assertion " // &
                "above passes against a guard that quotes the value in full: " // trim(names(k)))
            if (allocated(error)) return
        end do
    end subroutine test_invalid_mode_message_is_capped

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

    subroutine test_table_append_self_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_append_self", expect_abort=.true., &
            failure_message="appending a table to itself was expected to abort", &
            required_stderr="a table cannot be appended to itself")
    end subroutine test_table_append_self_aborts

    !> The negative control for the guard above, and it is not optional: a guard that refused EVERY
    !! %append would satisfy the abort test perfectly while breaking the operation outright. The
    !! scenario appends a different table before appending itself, so this asserts that half ran.
    subroutine test_table_append_other_still_works(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "table_append_self", "append of another table ok, nrows=3", &
            expect_on="stdout", &
            failure_message="the self-append guard also blocked appending a different table")
    end subroutine test_table_append_other_still_works

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

    !> **The one container refusal left.** There were three; the other two -- the element form of
    !! `%is_null` and the rank-2 `%get_valid_mask` -- now DELEGATE to the row form instead of
    !! aborting, because a container column's `width` is 1 and the element axis is degenerate rather
    !! than absent. Their negative controls became ordinary assertions in
    !! `test/test_table_container.f90`.
    !!
    !! This one keeps its negative control there too, for the reason that applies to every
    !! refusal: what has to be shown is that the guard did NOT fire on the neighbouring permitted
    !! case, which is an assertion rather than an abort.
    !!
    !! The message must name the KIND, which is what tells this refusal apart from the *_VEC one
    !! that shares the guard -- the two have different reasons and different remedies.
    subroutine test_container_sort_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_sort_key", expect_abort=.true., &
            failure_message="sorting by a container column was expected to abort", &
            required_stderr="a PK_LIST column cannot be a sort key; there is no defined order on a list")
    end subroutine test_container_sort_key_aborts

    !> A token argument's rejection has to name what WAS expected -- a caller who typed
    !! `"containers"` needs the list, not merely the news that this was not it.
    subroutine test_container_bad_list_columns_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_bad_list_columns", expect_abort=.true., &
            failure_message="an unrecognized list_columns= token was expected to abort", &
            required_stderr="expected 'auto' or 'container'")
    end subroutine test_container_bad_list_columns_aborts

    !> **The VALUES %print_stat's container arm computes**, which nothing in process can capture:
    !! %print_stat writes to stdout and parquet_set_message_stream takes "stdout"/"stderr" and no
    !! file. The laziness half of the same feature IS asserted in process, by
    !! test/test_table_container.f90's test_container_print_stat.
    !!
    !! Three rows, each pinning a different rule. `ragged` (1..4) shows the extremes are real
    !! per-row lengths; `with_empty` (0..3) shows an EMPTY row is a present row of length zero and
    !! is counted; `null_avg` (5..5, with four null rows) shows a NULL row is excluded rather than
    !! counted as zero -- which is the rule an ordinary implementation gets wrong, and the only one
    !! of the three whose expected minimum differs from what a null-as-zero arm would print.
    subroutine test_container_print_stat_lengths(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_streams(error, "container_print_stat_lengths", &
            "ragged       PK_LIST             1       0           1                       4", &
            expect_on="stdout", &
            failure_message="%print_stat should report a ragged container column's row-length extremes")
        if (allocated(error)) return
        call check_scenario_streams(error, "container_print_stat_lengths", &
            "with_empty   PK_LIST             1       0           0                       3", &
            expect_on="stdout", &
            failure_message="an EMPTY container row is length zero and must be counted")
        if (allocated(error)) return
        call check_scenario_streams(error, "container_print_stat_lengths", &
            "null_avg     PK_LIST             1       4           5                       5", &
            expect_on="stdout", &
            failure_message="a NULL container row has no length and must be excluded from the extremes")
    end subroutine test_container_print_stat_lengths

    !> **The resolved question.** A row-structural mutation skips a column that is not resident and
    !! the detach guard catches the later read -- there is no separate container guard, because a
    !! skipped column is RES_EMPTY and holds no storage to misalign.
    !!
    !! What this actually pins is that the container accessors route through `table_resolve`: one
    !! written any other way would hand back the skipped column instead of aborting, and nothing
    !! else in the suite would see it. The negative control is test_container_row_alignment.
    subroutine test_container_skipped_by_mutation_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "container_skipped_by_mutation", expect_abort=.true., &
            failure_message="reading a container column that a row mutation had skipped was expected to abort", &
            required_stderr="this table has been detached from its file by a row-structural change")
    end subroutine test_container_skipped_by_mutation_aborts

    subroutine test_table_clone_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "table_clone_type_mismatch", expect_abort=.true., &
            failure_message="cloning into another table type was expected to abort", &
            required_stderr="source and destination must be the same table type")
    end subroutine test_table_clone_type_mismatch_aborts
    !

end module test_table_errors
