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
!>
!> **This module holds the subprocess-driving machinery and only part of the tests.** The
!> `errors` suite here drives the scenarios in `error_scenarios_io.f90`; its three siblings --
!> `test_table_errors.f90`, `test_analysis_errors.f90`, `test_numeric_errors.f90` -- hold the
!> suites `table_errors`, `analysis_errors` and `numeric_errors`, each driving the
!> `error_scenarios_*` module of the same name. The split is for COMPILE TIME: as one module
!> this file was about 23000 lines and took some 27 s under ifx, second only to
!> `error_scenarios.f90` itself, and both sat on the critical path of every build. A test
!> belongs in the file named for the scenario group it drives; `run_error_scenario`, the
!> `check_scenario_*` helpers and `prime_error_scenarios` stay here and are used from all four.
module test_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64
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
    ! Exposed for the *_errors modules split out of this one -- see the module
    ! header. Nothing outside this directory calls them.
    public :: file_contains
    public :: file_count_containing
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

    subroutine collect_tests_parquet_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end. Neither obvious alternative works:
        ! a single array constructor exceeds the standard's 255-continuation-line limit (this
        ! list is far longer than that), and the self-referential append form
        ! `testsuite = [testsuite, ...]` compiles everywhere but makes nagfor 7.2 double-free
        ! each entry's allocatable name string at run time ("Invalid deallocation of size N:
        ! block was already deallocated"), aborting the suite. Both traps are invisible under
        ! gfortran, so keep this shape: assign each part, then concatenate once.
        !
        ! EVERY PART IS ITSELF CAPPED AT 255 CONTINUATION LINES -- the limit applies per
        ! statement, not to the subroutine, so adding entries to a part that is already near
        ! the cap is what actually breaks the build. Only nagfor enforces it (gfortran, ifx and
        ! flang all accept more), so a violation compiles clean everywhere else and is found
        ! only by a nagfor run. Two parts have reached 258 that way and had to be split.
        ! WHEN A PART IS FULL, ADD A NEW ONE rather than growing an existing one: declare pN,
        ! open it after the previous part's `]`, and add it to the concatenation below. Note the
        ! trap in doing that: every part's last element ends with a trailing `&` continuing onto
        ! the `]` line, so close the new part by removing the COMMA, not the `&`.
        ! check_statement_continuation_lines (tools/check_source_conventions.py) fails the lint
        ! stage before nagfor ever sees it -- run it after adding entries here.
        ! The column-shape verbs. Their own part rather than an extension of p15: a Fortran
        ! statement may carry at most 255 continuation lines and NAG is the only compiler that
        ! enforces it, so each family gets its own array rather than one growing until a build
        ! elsewhere breaks.
        ! Split out of p4, which reached the 255-continuation-line ceiling nagfor enforces
        ! (CLAUDE.md's source conventions: close the part and open a new one rather than growing
        ! this one). Nothing else distinguishes the two halves.
        ! The line-of-sight cylinder's scenarios (%within_los, %pairs_within_los, observer=, los=):
        ! their own part rather than more entries in p7, which sits near the 255-continuation-line
        ! limit only nagfor enforces (code-style.md).
        ! p9: a new part rather than more entries in p8. A statement may carry at most 255
        ! continuation lines and only nagfor enforces it, so a part that has filled up is grown by
        ! adding another, never by appending to it.
        ! A TWELFTH part rather than more entries in p11: one array constructor may carry at most
        ! 255 continuation lines and only nagfor enforces it, so a part that fills up is grown by
        ! adding another. p4 already stands at 248.
        ! The grouping's scenarios (%group_by and parquet_grouping); its own part, so that
        ! p20 keeps headroom below the continuation-line ceiling.
        ! The `int32` and string-keyed refusals of `parquet_index`: each repeats its `int64`
        ! sibling's guard rather than delegating to it, so each copy needs a scenario of its own.
        ! p24: the remaining parquet_logging refusals -- the configuration bounds and the
        ! unconfigured default logger's own arms. A new part rather than more entries in p9, for
        ! the continuation-line reason stated above it.
        ! ---- parquet_optimize: the local engines' abort paths ----
        ! ---- parquet_optimize: the population tier's abort paths ----
        ! ---- parquet_prima: BOBYQA's abort paths ----
        ! ---- parquet_prima: LINCOA's and COBYLA's abort paths ----
        ! ---- parquet_interpolate: both interpolants' abort paths ----
        ! ---- parquet_random: points on a sphere ----
        ! ---- parquet_sphere ----
        ! ---- parquet_root ----
        ! ---- parquet_transform ----
        ! ---- parquet_stats: pf_bin_linear ----
        ! ---- parquet_kde ----
        ! ---- parquet_kde: pf_kde_grid ----
        ! ---- parquet_kde: the adaptive kernel ----
        type(unittest_type), allocatable :: p1(:), p2(:), p3(:), p4(:)

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
            new_unittest("reading a dictionary column over binary values aborts", &
                test_read_dictionary_binary_unsupported_aborts), &
            new_unittest("%print_rows refuses a negative first=/last=", &
                test_print_rows_negative_count_aborts), &
            new_unittest("the hash engine refuses an over-sized string-keyed join", &
                test_join_max_rows_hash_string_key_aborts), &
            new_unittest("the hash engine refuses an over-sized two-column join", &
                test_join_max_rows_hash_tuple_key_aborts), &
            new_unittest("%print_rows refuses rows= together with first=/last=", &
                test_print_rows_rows_with_first_aborts), &
            new_unittest("%print_rows refuses a slice row outside the table", &
                test_print_rows_slice_out_of_range_aborts), &
            new_unittest("%print_rows refuses a columns= name the table does not have", &
                test_print_rows_missing_column_aborts), &
            new_unittest("%print_rows refuses digits= outside 1..17", test_print_rows_bad_digits_aborts), &
            new_unittest("%print_rows refuses max_width= below 8", test_print_rows_bad_width_aborts), &
            new_unittest("%print_rows refuses max_columns= below 1", test_print_rows_bad_max_columns_aborts), &
            new_unittest("%print_rows on an unopened table reports THAT", test_print_rows_unopened_aborts), &
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
                test_read_array_em_bool_oob_aborts) &
            ]
        p2 = [ &
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
                test_extended_uint32_overflow_int32_aborts), &
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
            new_unittest("sort: unknown column aborts", &
                test_sort_unknown_column_aborts), &
            new_unittest("sort: vector column key aborts", &
                test_sort_vector_column_aborts), &
            new_unittest("sort: variable-length list column key aborts", &
                test_sort_list_column_aborts), &
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
            new_unittest("filter: variable-length list column aborts", &
                test_filter_list_column_aborts), &
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
            new_unittest("filter: a rule naming an unbound set aborts", &
                test_filter_set_unbound_name_aborts), &
            new_unittest("filter: 'in' taking a literal instead of @name aborts", &
                test_filter_set_missing_at_aborts), &
            new_unittest("filter: 'in' with no set name aborts", test_filter_set_no_value_aborts), &
            new_unittest("filter: a real set against an integer column aborts", &
                test_filter_set_wrong_family_aborts), &
            new_unittest("filter: an integer set against a string column aborts", &
                test_filter_set_on_string_column_aborts), &
            new_unittest("filter: a NaN inside a bound real set aborts at bind", &
                test_filter_set_nan_member_aborts), &
            new_unittest("filter: two sets bound under one name aborts", &
                test_filter_set_duplicate_name_aborts), &
            new_unittest("filter: a set name containing a space aborts", &
                test_filter_set_name_with_space_aborts), &
            new_unittest("filter: a set name containing '@' aborts", &
                test_filter_set_name_with_at_aborts), &
            new_unittest("filter: a blank set name aborts", test_filter_set_blank_name_aborts), &
            new_unittest("filter: an is_valid mask of the wrong length aborts", &
                test_filter_set_mask_length_aborts), &
            new_unittest("filter: more bound sets than the published limit aborts", &
                test_filter_set_too_many_aborts), &
            new_unittest("filter: a set clause on a vector column aborts", &
                test_filter_set_vector_column_aborts), &
            new_unittest("filter: a well-formed set clause opens cleanly (control)", &
                test_filter_set_control_succeeds), &
            new_unittest("filter: an empty literal list aborts", test_filter_list_empty_aborts), &
            new_unittest("filter: a quoted list element on a numeric column aborts", &
                test_filter_list_quoted_on_numeric_aborts), &
            new_unittest("filter: a bare list element on a string column aborts", &
                test_filter_list_unquoted_on_string_aborts), &
            new_unittest("filter: a non-numeric list element aborts", test_filter_list_bad_number_aborts), &
            new_unittest("filter: a fractional list element on an integer column aborts", &
                test_filter_list_non_integer_aborts), &
            new_unittest("filter: a NaN inside a literal list aborts", test_filter_list_nan_member_aborts), &
            new_unittest("filter: a trailing comma in a literal list aborts", &
                test_filter_list_trailing_comma_aborts), &
            new_unittest("filter: a nested parenthesis in a literal list aborts", &
                test_filter_list_nested_paren_aborts), &
            new_unittest("filter: an unclosed literal list aborts", test_filter_list_unclosed_aborts), &
            new_unittest("filter: an unterminated quoted list element aborts", &
                test_filter_list_unclosed_quote_aborts), &
            new_unittest("filter: a set clause on a boolean column aborts", &
                test_filter_list_on_bool_column_aborts), &
            new_unittest("filter: a set clause on a date column aborts naming the type", &
                test_filter_set_temporal_column_aborts), &
            new_unittest("filter: is_finite on an integer column aborts", &
                test_is_finite_on_int_column_aborts), &
            new_unittest("filter: a value after is_finite aborts", test_is_finite_takes_no_value_aborts), &
            new_unittest("filter: print_stat shows a literal list exactly as written", &
                test_filter_list_expr_text_verbatim), &
            new_unittest("filter: a bare '.' as a list element aborts", &
                test_filter_list_bare_dot_aborts), &
            new_unittest("filter: a Fortran 'd' exponent in a list element aborts", &
                test_filter_list_fortran_exponent_aborts), &
            new_unittest("filter: two numbers in one integer list element aborts", &
                test_filter_list_two_numbers_aborts), &
            new_unittest("filter: two numbers in one real list element aborts", &
                test_filter_list_two_reals_aborts), &
            new_unittest("filter: a well-formed literal list and is_finite open cleanly", &
                test_filter_list_control_succeeds), &
            new_unittest("sortkey: remap_column_names with mismatched from/to sizes aborts", &
                test_sortkey_remap_size_mismatch_aborts), &
            new_unittest("sortkey: remap_column_names to an over-long column name aborts", &
                test_sortkey_remap_name_too_long_aborts), &
            new_unittest("settings: a thread-pool capacity below 1 aborts", &
                test_set_arrow_threads_zero_aborts), &
            new_unittest("settings: an unknown default compression codec aborts", &
                test_settings_bad_codec_aborts), &
            new_unittest("a file date that is not YYYY-MM-DDTHH:MM:SS aborts, naming the part at fault", &
                test_settings_file_date_invalid_aborts), &
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
            new_unittest("a second %init aborts", test_weighted_init_twice_aborts) &
            ]
        p3 = [ &
            new_unittest("a WEIGHTED subset larger than its population aborts", test_weighted_subset_big_aborts), &
            new_unittest("a mismatched permutation array aborts", test_weighted_perm_size_aborts), &
            new_unittest("an infinite weight aborts", test_weighted_infinite_aborts), &
            new_unittest("the RACE validates its own weights, on all four of its guards", &
                test_weighted_race_weight_guards), &
            new_unittest("a population too large for an int32 item index aborts, on all three guards", &
                test_weighted_int32_population_guards), &
            new_unittest("row_mask: a mask whose length disagrees with the table aborts", &
                test_row_mask_wrong_size_aborts), &
            new_unittest("row_mask: an unparseable rule aborts with the table's own context", &
                test_row_mask_bad_rule_aborts), &
            new_unittest("row_mask: a clause naming a column the table lacks aborts", &
                test_row_mask_unknown_column_aborts), &
            new_unittest("row_mask: a clause on a vector column aborts", &
                test_row_mask_vector_column_aborts), &
            new_unittest("row_mask: a bound beyond int32 range aborts rather than matching nothing", &
                test_row_mask_int32_range_aborts), &
            new_unittest("row_mask: a non-integer bound on an integer column aborts", &
                test_row_mask_bad_integer_aborts), &
            new_unittest("row_mask: a NaN comparison bound aborts, naming is_nan instead", &
                test_row_mask_nan_bound_aborts), &
            new_unittest("row_mask: an ordering comparison on a boolean column aborts", &
                test_row_mask_bool_ordering_aborts), &
            new_unittest("row_mask: an unquoted bound on a string column aborts", &
                test_row_mask_unquoted_string_aborts), &
            new_unittest("row_mask: is_nan on an integer column aborts", &
                test_row_mask_is_nan_on_int_aborts), &
            new_unittest("row_mask: starts_with on an integer column aborts", &
                test_row_mask_starts_with_on_int_aborts), &
            new_unittest("row_mask: an unbound @name aborts through the table too", &
                test_row_mask_unbound_set_aborts), &
            new_unittest("row_mask: a quoted list element on a numeric column aborts", &
                test_row_mask_list_quoted_aborts), &
            new_unittest("row_mask: a literal finer than the column's stored unit aborts", &
                test_row_mask_temporal_precision_aborts), &
            new_unittest("row_mask: a clause on a variable-length list column aborts", &
                test_row_mask_list_column_aborts), &
            new_unittest("row_mask: a clause on a map column aborts", &
                test_row_mask_map_column_aborts), &
            new_unittest("row_mask: a clause on a struct column aborts", &
                test_row_mask_struct_column_aborts), &
            new_unittest("row_mask: a quoted bound on an integer column aborts", &
                test_row_mask_quoted_integer_aborts), &
            new_unittest("row_mask: a quoted bound on a float column aborts", &
                test_row_mask_quoted_real_aborts), &
            new_unittest("row_mask: an unreadable, non-NaN bound on a float column aborts", &
                test_row_mask_bad_real_aborts), &
            new_unittest("row_mask: a quoted bound on a boolean column aborts", &
                test_row_mask_quoted_bool_aborts), &
            new_unittest("row_mask: a bound that is not true/false on a boolean column aborts", &
                test_row_mask_bad_bool_aborts), &
            new_unittest("row_mask: an unquoted starts_with pattern aborts", &
                test_row_mask_unquoted_match_aborts), &
            new_unittest("row_mask: an unquoted bound on a temporal column aborts", &
                test_row_mask_unquoted_temporal_aborts), &
            new_unittest("row_mask/filter_rows: a well-formed multi-clause rule applies cleanly", &
                test_row_mask_control_succeeds), &
            new_unittest("fillna: a real value for an integer column aborts, naming the column", &
                test_fillna_real_into_integer_aborts), &
            new_unittest("fillna: a logical value for a numeric column aborts", &
                test_fillna_logical_into_numeric_aborts), &
            new_unittest("fillna: a character value for a numeric column aborts", &
                test_fillna_string_into_numeric_aborts), &
            new_unittest("fillna: an integer value for a string column aborts", &
                test_fillna_integer_into_string_aborts), &
            new_unittest("fillna: an int64 value that no int32 column can hold aborts", &
                test_fillna_int32_range_aborts), &
            new_unittest("fillna: a container column aborts", test_fillna_container_column_aborts), &
            new_unittest("fillna: a column whose type was never read aborts", &
                test_fillna_unsupported_column_aborts), &
            new_unittest("fillna: an unknown name in the list aborts naming it", &
                test_fillna_unknown_column_aborts), &
            new_unittest("ffill: limit=0 aborts", test_ffill_limit_zero_aborts), &
            new_unittest("bfill: a negative limit aborts", test_ffill_limit_negative_aborts), &
            new_unittest("ffill: a container column aborts", test_ffill_container_column_aborts), &
            new_unittest("dropna: how= and min_valid= together abort", &
                test_dropna_how_and_min_valid_aborts), &
            new_unittest("dropna: an unrecognised how token aborts, naming what was expected", &
                test_dropna_bad_how_aborts), &
            new_unittest("dropna: min_valid above the number of columns named aborts", &
                test_dropna_min_valid_range_aborts), &
            new_unittest("dropna: a negative min_valid aborts", &
                test_dropna_min_valid_negative_aborts), &
            new_unittest("dropna: a bad how token aborts even with nothing resident", &
                test_dropna_bad_how_nothing_resident_aborts), &
            new_unittest("fillna/ffill/bfill/dropna: every well-formed call runs cleanly", &
                test_fill_control_succeeds), &
            new_unittest("get_matrix naming a string column aborts", &
                test_get_matrix_string_column_aborts), &
            new_unittest("get_matrix naming a vector column aborts, saying so", &
                test_get_matrix_vector_column_aborts), &
            new_unittest("get_matrix naming a column of another family aborts", &
                test_get_matrix_kind_mismatch_aborts), &
            new_unittest("get_matrix naming a column that is not there aborts", &
                test_get_matrix_unknown_column_aborts), &
            new_unittest("set_matrix refuses the widening get_matrix allows", &
                test_set_matrix_no_widening_aborts), &
            new_unittest("set_matrix with more matrix rows than names aborts", &
                test_set_matrix_wrong_ncols_aborts), &
            new_unittest("set_matrix with the wrong row count aborts", &
                test_set_matrix_wrong_nrows_aborts), &
            new_unittest("set_matrix with a mis-shaped mask aborts", &
                test_set_matrix_mask_shape_aborts), &
            new_unittest("drop_columns naming an absent column aborts, naming them all", &
                test_drop_columns_missing_aborts), &
            new_unittest("drop_columns on a predefined column without force aborts", &
                test_drop_columns_predefined_aborts), &
            new_unittest("keep_columns naming an absent column aborts", &
                test_keep_columns_missing_aborts), &
            new_unittest("keep_columns dropping a predefined column by omission aborts", &
                test_keep_columns_predefined_aborts), &
            new_unittest("drop_columns on a shared table aborts", &
                test_drop_columns_shared_aborts), &
            new_unittest("get_matrix/set_matrix/drop_columns/keep_columns: every well-formed call runs", &
                test_matrix_control_succeeds), &
            new_unittest("parse_column on a numeric column aborts, naming its kind", &
                test_parse_column_non_string_aborts), &
            new_unittest("parse_column on a string VECTOR column aborts", &
                test_parse_column_vector_source_aborts), &
            new_unittest("parse_column into a kind it has no parser for aborts", &
                test_parse_column_bad_target_aborts), &
            new_unittest("parse_column with an unknown invalid= policy aborts", &
                test_parse_column_invalid_token_aborts), &
            new_unittest("parse_column on unreadable text aborts, naming row, column and text", &
                test_parse_column_malformed_aborts), &
            new_unittest("parse_column caps the offending text it echoes", &
                test_parse_column_malformed_long_aborts), &
            new_unittest("parse_column to_name naming an existing column aborts", &
                test_parse_column_to_name_exists_aborts), &
            new_unittest("parse_column on a shared table aborts", &
                test_parse_column_shared_aborts), &
            new_unittest("reload of a parsed column aborts without force", &
                test_reload_after_parse_column_aborts), &
            new_unittest("format_column on a column that is already text aborts", &
                test_format_column_string_source_aborts), &
            new_unittest("format_column on a vector column aborts", &
                test_format_column_vector_source_aborts), &
            new_unittest("format_column with fmt= on a temporal column aborts", &
                test_format_column_fmt_on_temporal_aborts), &
            new_unittest("parse_column in place on a predefined column aborts", &
                test_parse_column_predefined_aborts), &
            new_unittest("format_column in place on a predefined column aborts", &
                test_format_column_predefined_aborts), &
            new_unittest("format_column force=.false. on a predefined column aborts too", &
                test_format_column_predefined_false_aborts), &
            new_unittest("cast in place on a predefined column aborts", &
                test_cast_predefined_aborts), &
            new_unittest("cast force=.false. on a predefined column aborts too", &
                test_cast_predefined_false_aborts), &
            new_unittest("parse_column/format_column: every well-formed call runs", &
                test_convert_control_succeeds), &
            new_unittest("explode with a count list of the wrong length aborts", &
                test_explode_wrong_length_aborts), &
            new_unittest("explode with a negative count aborts", &
                test_explode_negative_count_aborts), &
            new_unittest("explode past huge(int64) rows aborts before allocating", &
                test_explode_row_count_overflow_aborts), &
            new_unittest("explode on a shared table aborts", &
                test_explode_shared_aborts), &
            new_unittest("duplicated with an unknown keep= token aborts", &
                test_duplicated_bad_keep_aborts), &
            new_unittest("drop_duplicates with an unknown keep= token blames itself", &
                test_drop_duplicates_bad_keep_aborts), &
            new_unittest("duplicated() over an unorderable resident column aborts", &
                test_duplicated_all_unorderable_aborts), &
            new_unittest("drop_duplicates() on a table nothing has read yet aborts", &
                test_duplicated_all_nothing_resident_aborts), &
            new_unittest("duplicated naming a column that is not there aborts", &
                test_duplicated_unknown_column_aborts), &
            new_unittest("sort_by_values with the wrong value count aborts", &
                test_sort_by_values_wrong_length_aborts), &
            new_unittest("argsort_by_values with the wrong value count aborts", &
                test_argsort_by_values_wrong_length_aborts), &
            new_unittest("drop_duplicates on a shared table aborts", &
                test_drop_duplicates_shared_aborts), &
            new_unittest("sort_by_values on a shared table aborts", &
                test_sort_by_values_shared_aborts), &
            new_unittest("explode/duplicated/drop_duplicates/sort_by_values: every well-formed call runs", &
                test_rowverbs_control_succeeds), &
            new_unittest("pf_remap with more keys than values aborts", &
                test_remap_length_mismatch_aborts), &
            new_unittest("pf_remap with a repeated key aborts naming both positions", &
                test_remap_duplicate_key_aborts), &
            new_unittest("pf_remap with neither default= nor found= aborts on an unmapped value", &
                test_remap_unmapped_no_policy_aborts), &
            new_unittest("value_counts of a column already called count aborts", &
                test_value_counts_count_name_collision_aborts), &
            new_unittest("value_counts naming a column that is not there blames itself", &
                test_value_counts_unknown_column_aborts), &
            new_unittest("value_counts of a vector column aborts", &
                test_value_counts_unorderable_aborts), &
            new_unittest("pf_value_counts/pf_remap/value_counts: every well-formed call runs", &
                test_counting_control_succeeds), &
            new_unittest("an environment value longer than the buffer aborts", &
                test_settings_env_too_long_aborts), &
            new_unittest("an environment value beyond a default INTEGER's range aborts", &
                test_settings_env_int32_out_of_range_aborts), &
            new_unittest("a long environment value is echoed truncated, not whole", &
                test_settings_env_long_value_preview_aborts) &
            ]
        p4 = [ &
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
            new_unittest("settings: silent takes the advice and leaves the warning", &
                test_settings_advice_is_its_own_class), &
            new_unittest("settings: an abort still reports itself when everything is silenced", &
                test_settings_error_survives_silence), &
            new_unittest("read_qc: an entry longer than the supported maximum aborts", &
                test_read_qc_entry_too_long_aborts), &
            new_unittest("read_qc: remap_column_names with mismatched from/to sizes aborts", &
                test_read_qc_remap_size_mismatch_aborts), &
            new_unittest("read_qc: an entry that grows past the cap when remapped aborts", &
                test_read_qc_remap_entry_too_long_aborts), &
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
            new_unittest("filter: starts_with against a non-string column aborts", &
                test_filter_starts_with_non_string_column_aborts), &
            new_unittest("filter: an unquoted starts_with pattern aborts", &
                test_filter_starts_with_unquoted_value_aborts), &
            new_unittest("filter: starts_with against a date column aborts naming the operator", &
                test_filter_starts_with_on_temporal_column_aborts), &
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
            new_unittest("pre-evaluated filter leaves describing the wrong row count abort", &
                test_filter_pre_leaf_row_count_mismatch_aborts), &
            new_unittest("string length query on a non-string column aborts", &
                test_string_length_on_non_string_column_aborts), &
            new_unittest("writing columns with mismatched row counts aborts", &
                test_write_row_count_mismatch_aborts), &
            new_unittest("reading a column into a wrong-size array aborts", &
                test_read_row_count_mismatch_aborts), &
            new_unittest("reading from an unopened reader aborts", &
                test_read_before_open_aborts), &
            new_unittest("writing to an unopened writer aborts", &
                test_write_before_open_aborts), &
            new_unittest("parquet_new_row_group before open names ITS OWN procedure", &
                test_new_row_group_before_open_aborts), &
            new_unittest("parquet_get_metadata before open aborts rather than using default=", &
                test_get_metadata_before_open_aborts), &
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
            new_unittest("error context survives verbosity=errors_only and follows message_stream", &
                test_error_context_survives_suppression), &
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
            new_unittest("the size queries read no column data, and a re-read is free", &
                test_size_queries_read_no_column_data), &
            new_unittest("the affinity clamp warns, on the right stream, exactly once", &
                test_affinity_clamp_warns), &
            new_unittest("verbosity=silent suppresses the affinity-clamp warning", &
                test_affinity_clamp_silent), &
            new_unittest("no clamp, no affinity warning", test_affinity_clamp_absent), &
            new_unittest("a list column written under an undeclared name is refused", &
                test_write_undeclared_column_list_aborts), &
            new_unittest("a map column written under an undeclared name is refused", &
                test_write_undeclared_column_map_aborts), &
            new_unittest("a struct column written under an undeclared name is refused", &
                test_write_undeclared_column_struct_aborts), &
            new_unittest("writing a map whose value is a container is refused by kind", &
                test_write_map_nested_value_aborts) &
            ]
        testsuite = [p1, p2, p3, p4]
    end subroutine collect_tests_parquet_errors
    !
    !> See scenario_write_undeclared_column_list.
    subroutine test_write_undeclared_column_list_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_undeclared_column_list", expect_abort=.true., &
            failure_message="a list column under an undeclared name was expected to abort", &
            required_stderr="parquet_write_column: column not defined in parquet_open_writer: not_a_real_column")
    end subroutine test_write_undeclared_column_list_aborts
    !
    !> See scenario_write_undeclared_column_map.
    subroutine test_write_undeclared_column_map_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_undeclared_column_map", expect_abort=.true., &
            failure_message="a map column under an undeclared name was expected to abort", &
            required_stderr="parquet_write_column: column not defined in parquet_open_writer: not_a_real_column")
    end subroutine test_write_undeclared_column_map_aborts
    !
    !> See scenario_write_undeclared_column_struct.
    subroutine test_write_undeclared_column_struct_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_undeclared_column_struct", expect_abort=.true., &
            failure_message="a struct column under an undeclared name was expected to abort", &
            required_stderr="parquet_write_column: column not defined in parquet_open_writer: not_a_real_column")
    end subroutine test_write_undeclared_column_struct_aborts
    !
    !> See scenario_write_map_nested_value.
    subroutine test_write_map_nested_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "write_map_nested_value", expect_abort=.true., &
            failure_message="a map with a container value was expected to abort", &
            required_stderr="writing a container inside a container is not supported")
    end subroutine test_write_map_nested_value_aborts

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
    !> (see doc/pages/operating/error-handling.md's "A C++-level process exit"),
    !> just a clean, diagnosable one.
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

    !> A dictionary column over binary values must be refused exactly as a plain binary column is
    !> -- decoding a dictionary on read does not widen what this library can read.
    subroutine test_read_dictionary_binary_unsupported_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_dictionary_binary_unsupported", expect_abort=.true., &
            failure_message="reading a dictionary column over binary values was expected to abort")
    end subroutine test_read_dictionary_binary_unsupported_aborts

    !> The eight tests below cover every guard %print_rows has.
    !>
    !> Each asserts a FRAGMENT OF THE MESSAGE as well as the exit status, which most scenarios
    !> here deliberately do not (see check_scenario_exit_status_and_stderr's own note). The reason
    !> is specific to this group: all eight abort from the same procedure on the same fixture, so
    !> a status-only assertion passes just as happily when the wrong guard fires, or when one
    !> guard has swallowed another's case. The fragment is the shortest text that tells them
    !> apart, not the whole message, so rewording stays free.

    !> A negative row count is a caller mistake, not an empty display.
    subroutine test_print_rows_negative_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_negative_count", expect_abort=.true., &
            failure_message="%print_rows(first=-1) was expected to abort", &
            required_stderr="must not be negative")
    end subroutine test_print_rows_negative_count_aborts

    !> rows= and first=/last= are two ways of choosing rows, and giving both says nothing.
    subroutine test_print_rows_rows_with_first_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_rows_with_first", expect_abort=.true., &
            failure_message="%print_rows(rows=..., first=...) was expected to abort", &
            required_stderr="rows= selects the rows explicitly")
    end subroutine test_print_rows_rows_with_first_aborts

    !> The slice machinery's own range check, reached through the display rather than duplicated.
    subroutine test_print_rows_slice_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_slice_out_of_range", expect_abort=.true., &
            failure_message="%print_rows(rows=) with a row outside the table was expected to abort", &
            required_stderr="outside the table")
    end subroutine test_print_rows_slice_out_of_range_aborts

    !> A columns= name the table does not have, reported with every missing name.
    subroutine test_print_rows_missing_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_missing_column", expect_abort=.true., &
            failure_message="%print_rows over a missing column was expected to abort", &
            required_stderr="this table does not have")
    end subroutine test_print_rows_missing_column_aborts

    !> digits= outside 1..17 cannot render a real.
    subroutine test_print_rows_bad_digits_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_bad_digits", expect_abort=.true., &
            failure_message="%print_rows(digits=0) was expected to abort", &
            required_stderr="digits= must be between 1 and 17")
    end subroutine test_print_rows_bad_digits_aborts

    !> Below eight characters a cut cell is mostly marker.
    subroutine test_print_rows_bad_width_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_bad_width", expect_abort=.true., &
            failure_message="%print_rows(max_width=3) was expected to abort", &
            required_stderr="max_width= must be at least 8")
    end subroutine test_print_rows_bad_width_aborts

    !> Zero columns is not a display.
    subroutine test_print_rows_bad_max_columns_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_bad_max_columns", expect_abort=.true., &
            failure_message="%print_rows(max_columns=0) was expected to abort", &
            required_stderr="max_columns= must be at least 1")
    end subroutine test_print_rows_bad_max_columns_aborts

    !> The open check sits above the verbosity check, so this reports the real mistake.
    subroutine test_print_rows_unopened_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_rows_unopened", expect_abort=.true., &
            failure_message="%print_rows on an unopened table was expected to abort", &
            required_stderr="print_rows: table has not been opened")
    end subroutine test_print_rows_unopened_aborts

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

    !> Read-time sort abort path: see scenario_sort_vector_column in test/error_scenarios.f90
    !> for what it does and why that state is rejected.
    subroutine test_sort_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_vector_column", expect_abort=.true., &
            failure_message="opening a reader sorted by a vector column was expected to abort", &
            required_stderr="sort key 'vec' is a vector column")
    end subroutine test_sort_vector_column_aborts

    !> The SAME guard's variable-length-list arm, which no test reached while both scenarios wrote
    !> a 2-D array. Asserts the shape word as well as the refusal: a ragged list is not a "vector
    !> column", and saying so was the defect this pairs with.
    subroutine test_sort_list_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_list_column", expect_abort=.true., &
            failure_message="opening a reader sorted by a variable-length list column was expected to abort", &
            required_stderr="sort key 'list_col' is a variable-length list column")
    end subroutine test_sort_list_column_aborts

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

    !> The SAME guard's variable-length-list arm -- see test_sort_list_column_aborts for why the
    !> two arms need separate tests.
    subroutine test_filter_list_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_column", expect_abort=.true., &
            failure_message="opening a reader with a filter naming a variable-length list column was expected to abort", &
            required_stderr="filter column 'list_col' is a variable-length list column")
    end subroutine test_filter_list_column_aborts

    !> A filter rule missing its operator ("<column> <op> [value]" shape)
    !> aborts with a clean, syntax-specific message from the Fortran-side
    !> tokenizer, before ever reaching the C++ side.
    subroutine test_filter_malformed_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_malformed_rule", expect_abort=.true., &
            failure_message="opening a reader with a malformed filter rule was expected to abort", &
            required_stderr="invalid filter rule")
    end subroutine test_filter_malformed_rule_aborts

    !> A rule naming a set nothing was bound under. The message names the set, because a typo in a
    !> `@name` is the likely cause and nothing else would identify it.
    subroutine test_filter_set_unbound_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_unbound_name", expect_abort=.true., &
            failure_message="a rule naming an unbound set was expected to abort", &
            required_stderr="no set of that name is bound")
    end subroutine test_filter_set_unbound_name_aborts

    subroutine test_filter_set_missing_at_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_missing_at", expect_abort=.true., &
            failure_message="'in' taking a literal was expected to abort", &
            required_stderr="takes either a bound set named with a leading '@'")
    end subroutine test_filter_set_missing_at_aborts

    subroutine test_filter_set_no_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_no_value", expect_abort=.true., &
            failure_message="'in' with no set name was expected to abort", &
            required_stderr="is missing a set after")
    end subroutine test_filter_set_no_value_aborts

    subroutine test_filter_set_wrong_family_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_wrong_family", expect_abort=.true., &
            failure_message="a real set against an integer column was expected to abort", &
            required_stderr="cannot be compared against that column")
    end subroutine test_filter_set_wrong_family_aborts

    subroutine test_filter_set_on_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_on_string_column", expect_abort=.true., &
            failure_message="an integer set against a string column was expected to abort", &
            required_stderr="cannot be compared against that column")
    end subroutine test_filter_set_on_string_column_aborts

    !> The refusal is at %bind, not at apply: a NaN member could never match any row, so accepting
    !> it would leave a set element that is silently inert.
    subroutine test_filter_set_nan_member_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_nan_member", expect_abort=.true., &
            failure_message="a NaN inside a bound real set was expected to abort", &
            required_stderr="is a NaN, which can never match")
    end subroutine test_filter_set_nan_member_aborts

    subroutine test_filter_set_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_duplicate_name", expect_abort=.true., &
            failure_message="two sets bound under one name were expected to abort", &
            required_stderr="is already bound to this filter")
    end subroutine test_filter_set_duplicate_name_aborts

    subroutine test_filter_set_name_with_space_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_name_with_space", expect_abort=.true., &
            failure_message="a set name containing a space was expected to abort", &
            required_stderr="must not contain a space")
    end subroutine test_filter_set_name_with_space_aborts

    subroutine test_filter_set_name_with_at_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_name_with_at", expect_abort=.true., &
            failure_message="a set name containing '@' was expected to abort", &
            required_stderr="must not contain '@'")
    end subroutine test_filter_set_name_with_at_aborts

    subroutine test_filter_set_blank_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_blank_name", expect_abort=.true., &
            failure_message="a blank set name was expected to abort", &
            required_stderr="must not be blank")
    end subroutine test_filter_set_blank_name_aborts

    subroutine test_filter_set_mask_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_mask_length", expect_abort=.true., &
            failure_message="an is_valid mask of the wrong length was expected to abort", &
            required_stderr="is_valid has 2 entries but values has 3")
    end subroutine test_filter_set_mask_length_aborts

    subroutine test_filter_set_too_many_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_too_many", expect_abort=.true., &
            failure_message="more bound sets than the published limit was expected to abort", &
            required_stderr="maximum of 64 bound sets")
    end subroutine test_filter_set_too_many_aborts

    subroutine test_filter_set_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_vector_column", expect_abort=.true., &
            failure_message="a set clause on a vector column was expected to abort", &
            required_stderr="filtering only supports scalar columns")
    end subroutine test_filter_set_vector_column_aborts

    !> The NEGATIVE CONTROL for the thirteen refusals above. Without it, a %bind that refused
    !> every input would pass all of them.
    subroutine test_filter_set_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "filter_set_control", expect_abort=.false., &
            failure_message="a well-formed set clause was expected to open cleanly")
    end subroutine test_filter_set_control_succeeds

    subroutine test_filter_list_empty_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_empty", expect_abort=.true., &
            failure_message="an empty literal list was expected to abort", &
            required_stderr="has an empty list")
    end subroutine test_filter_list_empty_aborts

    subroutine test_filter_list_quoted_on_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_quoted_on_numeric", &
            expect_abort=.true., &
            failure_message="a quoted list element on an integer column was expected to abort", &
            required_stderr="is quoted, but that column")
    end subroutine test_filter_list_quoted_on_numeric_aborts

    subroutine test_filter_list_unquoted_on_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_unquoted_on_string", &
            expect_abort=.true., &
            failure_message="a bare list element on a string column was expected to abort", &
            required_stderr="is not quoted, but that column")
    end subroutine test_filter_list_unquoted_on_string_aborts

    subroutine test_filter_list_bad_number_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_bad_number", expect_abort=.true., &
            failure_message="a non-numeric list element was expected to abort", &
            required_stderr="which is not a whole number")
    end subroutine test_filter_list_bad_number_aborts

    subroutine test_filter_list_non_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_non_integer", expect_abort=.true., &
            failure_message="a fractional element on an integer column was expected to abort", &
            required_stderr="element 2 of the list")
    end subroutine test_filter_list_non_integer_aborts

    subroutine test_filter_list_nan_member_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_nan_member", expect_abort=.true., &
            failure_message="a NaN inside a literal list was expected to abort", &
            required_stderr="say 'is_nan' instead")
    end subroutine test_filter_list_nan_member_aborts

    subroutine test_filter_list_trailing_comma_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_trailing_comma", &
            expect_abort=.true., &
            failure_message="a trailing comma in a literal list was expected to abort", &
            required_stderr="is empty (a stray or trailing comma?)")
    end subroutine test_filter_list_trailing_comma_aborts

    subroutine test_filter_list_nested_paren_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_nested_paren", expect_abort=.true., &
            failure_message="a nested parenthesis in a literal list was expected to abort", &
            required_stderr="may not contain a nested")
    end subroutine test_filter_list_nested_paren_aborts

    subroutine test_filter_list_unclosed_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_unclosed", expect_abort=.true., &
            failure_message="an unclosed literal list was expected to abort", &
            required_stderr="is missing its closing ')'")
    end subroutine test_filter_list_unclosed_aborts

    subroutine test_filter_list_unclosed_quote_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_unclosed_quote", &
            expect_abort=.true., &
            failure_message="an unterminated quoted list element was expected to abort", &
            required_stderr="is missing its closing quote")
    end subroutine test_filter_list_unclosed_quote_aborts

    subroutine test_filter_list_on_bool_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_on_bool_column", &
            expect_abort=.true., &
            failure_message="a set clause on a boolean column was expected to abort", &
            required_stderr="cannot be compared against that column")
    end subroutine test_filter_list_on_bool_column_aborts

    !> The message must come from the SET rules, naming the column's type -- not from the temporal
    !> literal conversion, which would report a missing ISO-8601 literal and say nothing about the
    !> set. That ordering is what this asserts, as much as the refusal itself.
    subroutine test_filter_set_temporal_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_set_temporal_column", &
            expect_abort=.true., &
            failure_message="a set clause on a date column was expected to abort", &
            required_stderr="read as 'date'")
    end subroutine test_filter_set_temporal_column_aborts

    subroutine test_is_finite_on_int_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "is_finite_on_int_column", expect_abort=.true., &
            failure_message="is_finite on an integer column was expected to abort", &
            required_stderr="only supported for floating-point columns")
    end subroutine test_is_finite_on_int_column_aborts

    !> The message asserted here is the one produced only when is_finite is registered as valueless
    !> in the EXPRESSION parser as well as in the clause tokenizer. Registered in the tokenizer
    !> alone, the parser would swallow the 3 as this clause's value and the abort would read "takes
    !> no value"; registered in neither, it would read "unknown operator". So a token after
    !> is_finite aborts whatever happens, and only this wording says it aborted for the right
    !> reason -- the same shape "x is_nan 3" has.
    subroutine test_is_finite_takes_no_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "is_finite_takes_no_value", expect_abort=.true., &
            failure_message="a token after is_finite was expected to abort", &
            required_stderr="unexpected '3' (a missing and/or?)")
    end subroutine test_is_finite_takes_no_value_aborts

    !> The caller's own list text must reach print_stat's expression line unchanged, spacing and
    !> all. Reassembling the list from its tokens instead of lifting it verbatim out of the rule
    !> would select exactly the same rows and print `( 1,3 , 4)` as something else -- a difference
    !> with no other observable in the whole library, which is why this is asserted here rather
    !> than left to the row-set tests.
    subroutine test_filter_list_expr_text_verbatim(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: verbatim, read_happened

        call run_error_scenario("filter_list_expr_text", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a well-formed literal list was expected to open cleanly")
        if (allocated(error)) return

        ! The read really happened -- otherwise the absence of a wrong expression line proves nothing.
        call scenario_capture_contains(out_file, err_file, &
            "literal list expr_text scenario read rows = 3", read_happened)
        call check(error, read_happened, "the scenario must have read the three matching rows")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "id in ( 1,3 , 4)", verbatim)
        call check(error, verbatim, &
            "print_stat must show the literal list exactly as the caller wrote it, spacing included")
    end subroutine test_filter_list_expr_text_verbatim

    !> gfortran and nagfor reject "." through `read`'s own iostat; flang accepts it as 0.0 with
    !> iostat 0 (measured), so the mantissa-digit check is what makes this abort on every compiler
    !> rather than silently meaning 0.0 on one of them.
    subroutine test_filter_list_bare_dot_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_bare_dot", expect_abort=.true., &
            failure_message="a bare '.' as a list element was expected to abort", &
            required_stderr="which is not a number")
    end subroutine test_filter_list_bare_dot_aborts

    !> A narrowing, deliberately: strtod refuses `x == 1d3` for a bare literal, so accepting it in a
    !> list would make the list the more permissive parser. Fortran's own `read` accepts it, so only
    !> the shape check refuses it.
    subroutine test_filter_list_fortran_exponent_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_fortran_exponent", &
            expect_abort=.true., &
            failure_message="a Fortran 'd' exponent in a list element was expected to abort", &
            required_stderr="which is not a number")
    end subroutine test_filter_list_fortran_exponent_aborts

    !> The list-directed `read` hazard: it accepts "5 6" with iostat 0 and yields 5, so without the
    !> hand-rolled shape check `id in (1 2)` would silently mean `id in (1)`. Found by mutation --
    !> removing the shape check survived the whole filter suite, because no in-process test can see
    !> a wrong answer that is still a valid row set.
    subroutine test_filter_list_two_numbers_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_two_numbers", expect_abort=.true., &
            failure_message="two numbers in one integer list element were expected to abort", &
            required_stderr="which is not a whole number")
    end subroutine test_filter_list_two_numbers_aborts

    subroutine test_filter_list_two_reals_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_list_two_reals", expect_abort=.true., &
            failure_message="two numbers in one real list element were expected to abort", &
            required_stderr="which is not a number")
    end subroutine test_filter_list_two_reals_aborts

    !> The NEGATIVE CONTROL for the eighteen refusals above. Without it, a literal-list parser or an
    !> is_finite guard that refused every input would pass all of them.
    subroutine test_filter_list_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "filter_list_control", expect_abort=.false., &
            failure_message="a well-formed literal list and is_finite clause were expected to open cleanly")
    end subroutine test_filter_list_control_succeeds

    ! ---- The table's in-memory filter evaluator ------------------------------------------------
    !
    ! Each of these asserts that %row_mask reports a refusal the READER also makes, and reports it
    ! with the TABLE's own prefix and context suffix. The prefix is what the assertions are really
    ! about: the checks are shared between the two engines (parquet_resolve_set_payload, the
    ! literal parsers, parquet_check_set_column_shape), so a message reaching a table caller with
    ! the reader's wording would mean the table path had gone through the reader -- and one with no
    ! context at all would mean it had skipped table_context_suffix. Both are invisible to every
    ! other test.

    subroutine test_row_mask_wrong_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_wrong_size", expect_abort=.true., &
            failure_message="a mask whose length disagrees with the table was expected to abort", &
            required_stderr="the mask has 3 entries but the table has 4 rows")
    end subroutine test_row_mask_wrong_size_aborts

    subroutine test_row_mask_bad_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_bad_rule", expect_abort=.true., &
            failure_message="an unparseable rule was expected to abort through the table", &
            required_stderr="invalid filter rule: filter rule 'id >' is missing a value")
    end subroutine test_row_mask_bad_rule_aborts

    subroutine test_row_mask_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_unknown_column", expect_abort=.true., &
            failure_message="a clause naming a column the table lacks was expected to abort", &
            required_stderr="no column of this name")
    end subroutine test_row_mask_unknown_column_aborts

    subroutine test_row_mask_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_vector_column", expect_abort=.true., &
            failure_message="a clause on a vector column was expected to abort", &
            required_stderr="is a vector column; filtering only supports scalar columns")
    end subroutine test_row_mask_vector_column_aborts

    subroutine test_row_mask_int32_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_int32_range", expect_abort=.true., &
            failure_message="an out-of-int32-range bound was expected to abort", &
            required_stderr="is out of int32 range for column 'id'")
    end subroutine test_row_mask_int32_range_aborts

    subroutine test_row_mask_bad_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_bad_integer", expect_abort=.true., &
            failure_message="a non-integer bound on an integer column was expected to abort", &
            required_stderr="is not a valid integer for column 'id'")
    end subroutine test_row_mask_bad_integer_aborts

    subroutine test_row_mask_nan_bound_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_nan_bound", expect_abort=.true., &
            failure_message="a NaN comparison bound was expected to abort", &
            required_stderr="use the 'is_nan'/'is_not_nan' operators instead")
    end subroutine test_row_mask_nan_bound_aborts

    subroutine test_row_mask_bool_ordering_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_bool_ordering", expect_abort=.true., &
            failure_message="an ordering comparison on a boolean column was expected to abort", &
            required_stderr="are not supported for boolean column 'flag'")
    end subroutine test_row_mask_bool_ordering_aborts

    subroutine test_row_mask_unquoted_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_unquoted_string", expect_abort=.true., &
            failure_message="an unquoted bound on a string column was expected to abort", &
            required_stderr="value for string column 'name' must be double-quoted")
    end subroutine test_row_mask_unquoted_string_aborts

    subroutine test_row_mask_is_nan_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_is_nan_on_int", expect_abort=.true., &
            failure_message="is_nan on an integer column was expected to abort", &
            required_stderr="is only supported for floating-point columns, and column 'id' is not one")
    end subroutine test_row_mask_is_nan_on_int_aborts

    !> The in-memory engine refuses a matcher on a non-string column, in the reader's words.
    subroutine test_row_mask_starts_with_on_int_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_starts_with_on_int", expect_abort=.true., &
            failure_message="starts_with on an integer column was expected to abort", &
            required_stderr="is only supported for string columns, and column 'id' is not one")
    end subroutine test_row_mask_starts_with_on_int_aborts

    subroutine test_row_mask_unbound_set_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_unbound_set", expect_abort=.true., &
            failure_message="an unbound @name through the table was expected to abort", &
            required_stderr="but no set of that name is bound to this filter")
    end subroutine test_row_mask_unbound_set_aborts

    subroutine test_row_mask_list_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_list_quoted", expect_abort=.true., &
            failure_message="a quoted list element on a numeric column was expected to abort", &
            required_stderr="write each member as a bare number")
    end subroutine test_row_mask_list_quoted_aborts

    subroutine test_row_mask_temporal_precision_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_temporal_precision", expect_abort=.true., &
            failure_message="a literal finer than the column's stored unit was expected to abort", &
            required_stderr="is more precise than that column's stored unit can represent")
    end subroutine test_row_mask_temporal_precision_aborts

    subroutine test_row_mask_list_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_list_column", expect_abort=.true., &
            failure_message="a clause on a list column was expected to abort", &
            required_stderr="is a variable-length list column; filtering only supports scalar columns")
    end subroutine test_row_mask_list_column_aborts

    subroutine test_row_mask_map_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_map_column", expect_abort=.true., &
            failure_message="a clause on a map column was expected to abort", &
            required_stderr="is a map column; filtering only supports scalar columns")
    end subroutine test_row_mask_map_column_aborts

    subroutine test_row_mask_struct_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_struct_column", expect_abort=.true., &
            failure_message="a clause on a struct column was expected to abort", &
            required_stderr="is a struct column; filtering only supports scalar columns")
    end subroutine test_row_mask_struct_column_aborts

    subroutine test_row_mask_quoted_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_quoted_integer", expect_abort=.true., &
            failure_message="a quoted bound on an integer column was expected to abort", &
            required_stderr="is not a valid integer for column 'id'")
    end subroutine test_row_mask_quoted_integer_aborts

    subroutine test_row_mask_quoted_real_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_quoted_real", expect_abort=.true., &
            failure_message="a quoted bound on a float column was expected to abort", &
            required_stderr="is not a valid number for column 'x'")
    end subroutine test_row_mask_quoted_real_aborts

    subroutine test_row_mask_bad_real_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_bad_real", expect_abort=.true., &
            failure_message="an unreadable bound on a float column was expected to abort", &
            required_stderr="is not a valid number for column 'x'")
    end subroutine test_row_mask_bad_real_aborts

    subroutine test_row_mask_quoted_bool_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_quoted_bool", expect_abort=.true., &
            failure_message="a quoted bound on a boolean column was expected to abort", &
            required_stderr="must be true or false (unquoted)")
    end subroutine test_row_mask_quoted_bool_aborts

    subroutine test_row_mask_bad_bool_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_bad_bool", expect_abort=.true., &
            failure_message="a non-boolean bound was expected to abort", &
            required_stderr="is not true/false for boolean column 'flag'")
    end subroutine test_row_mask_bad_bool_aborts

    subroutine test_row_mask_unquoted_match_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_unquoted_match", expect_abort=.true., &
            failure_message="an unquoted matcher pattern was expected to abort", &
            required_stderr="value for string column 'name' must be double-quoted")
    end subroutine test_row_mask_unquoted_match_aborts

    subroutine test_row_mask_unquoted_temporal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_mask_unquoted_temporal", expect_abort=.true., &
            failure_message="an unquoted temporal bound was expected to abort", &
            required_stderr="must be a double-quoted ISO-8601 literal")
    end subroutine test_row_mask_unquoted_temporal_aborts

    !> The NEGATIVE CONTROL for the thirteen refusals above, and for %filter_rows as well: one rule
    !> naming a scalar column of every family the evaluator supports, plus a literal list, applied
    !> through both entry points. Without it, an evaluator that refused every input would pass all
    !> thirteen.
    subroutine test_row_mask_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "row_mask_control", expect_abort=.false., &
            failure_message="a well-formed multi-clause rule was expected to apply cleanly through " // &
                "both %row_mask and %filter_rows")
    end subroutine test_row_mask_control_succeeds

    ! ---- The missing-data family --------------------------------------------------------------
    !
    ! Each asserts a DISTINCTIVE fragment of the message rather than only the exit status: these
    ! guards are close relatives, and a copy that raised the family message for the range check
    ! (or the other way round) would still abort and would still pass an exit-status-only test.

    subroutine test_fillna_real_into_integer_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_real_into_integer", expect_abort=.true., &
            failure_message="a real fill value for an integer column was expected to abort", &
            required_stderr="a real value cannot fill a PK_INT32 column")
    end subroutine test_fillna_real_into_integer_aborts

    subroutine test_fillna_logical_into_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_logical_into_numeric", expect_abort=.true., &
            failure_message="a logical fill value for a numeric column was expected to abort", &
            required_stderr="a logical value cannot fill a PK_INT32 column")
    end subroutine test_fillna_logical_into_numeric_aborts

    subroutine test_fillna_string_into_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_string_into_numeric", expect_abort=.true., &
            failure_message="a character fill value for a real column was expected to abort", &
            required_stderr="a character value cannot fill a PK_FLOAT64 column")
    end subroutine test_fillna_string_into_numeric_aborts

    subroutine test_fillna_integer_into_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_integer_into_string", expect_abort=.true., &
            failure_message="an integer fill value for a string column was expected to abort", &
            required_stderr="an integer value cannot fill a PK_STRING column")
    end subroutine test_fillna_integer_into_string_aborts

    !> The value check rather than the family check -- an integer value IS accepted for an integer
    !> column, so only the range test can refuse this one.
    subroutine test_fillna_int32_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_int32_range", expect_abort=.true., &
            failure_message="an out-of-range int64 fill value was expected to abort", &
            required_stderr="does not fit an int32 column")
    end subroutine test_fillna_int32_range_aborts

    subroutine test_fillna_container_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_container_column", expect_abort=.true., &
            failure_message="filling a container column was expected to abort", &
            required_stderr="there is no meaning to replacing a missing list with one")
    end subroutine test_fillna_container_column_aborts

    subroutine test_fillna_unsupported_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_unsupported_column", expect_abort=.true., &
            failure_message="filling a column of an unsupported type was expected to abort", &
            required_stderr="so its values were never read")
    end subroutine test_fillna_unsupported_column_aborts

    subroutine test_fillna_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "fillna_unknown_column", expect_abort=.true., &
            failure_message="an unknown column name in a fillna list was expected to abort", &
            required_stderr="nope")
    end subroutine test_fillna_unknown_column_aborts

    subroutine test_ffill_limit_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "ffill_limit_zero", expect_abort=.true., &
            failure_message="ffill with limit=0 was expected to abort", &
            required_stderr="limit must be at least 1")
    end subroutine test_ffill_limit_zero_aborts

    subroutine test_ffill_limit_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "ffill_limit_negative", expect_abort=.true., &
            failure_message="bfill with a negative limit was expected to abort", &
            required_stderr="limit must be at least 1")
    end subroutine test_ffill_limit_negative_aborts

    subroutine test_ffill_container_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "ffill_container_column", expect_abort=.true., &
            failure_message="forward-filling a container column was expected to abort", &
            required_stderr="cannot be filled from its neighbouring rows")
    end subroutine test_ffill_container_column_aborts

    subroutine test_dropna_how_and_min_valid_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "dropna_how_and_min_valid", expect_abort=.true., &
            failure_message="how= and min_valid= together were expected to abort", &
            required_stderr="pass how= or min_valid=, not both")
    end subroutine test_dropna_how_and_min_valid_aborts

    subroutine test_dropna_bad_how_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "dropna_bad_how", expect_abort=.true., &
            failure_message="an unrecognised how token was expected to abort", &
            required_stderr="how must be ""any"" or ""all""")
    end subroutine test_dropna_bad_how_aborts

    subroutine test_dropna_min_valid_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "dropna_min_valid_range", expect_abort=.true., &
            failure_message="a min_valid above the number of columns named was expected to abort", &
            required_stderr="min_valid is out of range")
    end subroutine test_dropna_min_valid_range_aborts

    subroutine test_dropna_min_valid_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "dropna_min_valid_negative", expect_abort=.true., &
            failure_message="a negative min_valid was expected to abort", &
            required_stderr="min_valid must not be negative")
    end subroutine test_dropna_min_valid_negative_aborts

    !> The guard is only worth having if it fires on a table with nothing to drop as well.
    subroutine test_dropna_bad_how_nothing_resident_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "dropna_bad_how_nothing_resident", &
            expect_abort=.true., &
            failure_message="a bad how token was expected to abort even with no resident column", &
            required_stderr="how must be ""any"" or ""all""")
    end subroutine test_dropna_bad_how_nothing_resident_aborts

    !> The NEGATIVE CONTROL for the fourteen refusals above: one fixture carrying all three
    !> storage classes, filled through %fillna in two kinds, scanned by %ffill and %bfill with and
    !> without a limit, and dropped by both policies. Without it, a compatibility rule that
    !> refused every value would pass all fourteen.
    subroutine test_fill_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "fill_control", expect_abort=.false., &
            failure_message="every well-formed fill and drop was expected to run cleanly")
    end subroutine test_fill_control_succeeds

    subroutine test_get_matrix_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_matrix_string_column", expect_abort=.true., &
            failure_message="%get_matrix naming a string column was expected to abort", &
            required_stderr="get_matrix: column kind (PK_STRING) does not match a float64 matrix")
    end subroutine test_get_matrix_string_column_aborts

    subroutine test_get_matrix_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_matrix_vector_column", expect_abort=.true., &
            failure_message="%get_matrix naming a vector column was expected to abort", &
            required_stderr="get_matrix: this is a VECTOR column (PK_FLOAT64_VEC)")
    end subroutine test_get_matrix_vector_column_aborts

    subroutine test_get_matrix_kind_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_matrix_kind_mismatch", expect_abort=.true., &
            failure_message="%get_matrix naming an int32 column for a real64 matrix was expected to abort", &
            required_stderr="get_matrix: column kind (PK_INT32) does not match a float64 matrix")
    end subroutine test_get_matrix_kind_mismatch_aborts

    subroutine test_get_matrix_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_matrix_unknown_column", expect_abort=.true., &
            failure_message="%get_matrix naming a column the table does not have was expected to abort", &
            required_stderr="get_matrix: no column of this name (column 'nosuch')")
    end subroutine test_get_matrix_unknown_column_aborts

    subroutine test_set_matrix_no_widening_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_matrix_no_widening", expect_abort=.true., &
            failure_message="%set_matrix writing a real64 matrix into a float32 column was expected to abort", &
            required_stderr="set_matrix: column kind (PK_FLOAT32) does not match a float64 matrix")
    end subroutine test_set_matrix_no_widening_aborts

    subroutine test_set_matrix_wrong_ncols_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_matrix_wrong_ncols", expect_abort=.true., &
            failure_message="%set_matrix with more matrix rows than names was expected to abort", &
            required_stderr="set_matrix: the matrix has 3 rows but 2 columns were named")
    end subroutine test_set_matrix_wrong_ncols_aborts

    subroutine test_set_matrix_wrong_nrows_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_matrix_wrong_nrows", expect_abort=.true., &
            failure_message="%set_matrix with the wrong row count was expected to abort", &
            required_stderr="set_matrix: the matrix has 4 rows of values but the table has 3 rows")
    end subroutine test_set_matrix_wrong_nrows_aborts

    subroutine test_set_matrix_mask_shape_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_matrix_mask_shape", expect_abort=.true., &
            failure_message="%set_matrix with a mis-shaped is_valid was expected to abort", &
            required_stderr="set_matrix: is_valid is shaped 2 x 2 but the matrix is 2 x 3")
    end subroutine test_set_matrix_mask_shape_aborts

    subroutine test_drop_columns_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "drop_columns_missing", expect_abort=.true., &
            failure_message="%drop_columns naming an absent column was expected to abort", &
            required_stderr="drop_columns: the table has no column called nosuch, alsonot")
    end subroutine test_drop_columns_missing_aborts

    subroutine test_drop_columns_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "drop_columns_predefined", expect_abort=.true., &
            failure_message="%drop_columns on a predefined column without force= was expected to abort", &
            required_stderr="drop_columns: this is a predefined column")
    end subroutine test_drop_columns_predefined_aborts

    subroutine test_keep_columns_missing_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "keep_columns_missing", expect_abort=.true., &
            failure_message="%keep_columns naming an absent column was expected to abort", &
            required_stderr="keep_columns: the table has no column called nosuch")
    end subroutine test_keep_columns_missing_aborts

    subroutine test_keep_columns_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "keep_columns_predefined", expect_abort=.true., &
            failure_message="%keep_columns dropping a predefined column by omission was expected to abort", &
            required_stderr="keep_columns: this is a predefined column")
    end subroutine test_keep_columns_predefined_aborts

    subroutine test_drop_columns_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the scenario's parallel region never " // &
            "opens, so the table is never shared and the guard has nothing to refuse")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, "drop_columns_shared", expect_abort=.true., &
            failure_message="%drop_columns on a table another thread may hold was expected to abort", &
            required_stderr="drop_columns: this table was not opened by this thread")
    end subroutine test_drop_columns_shared_aborts

    subroutine test_matrix_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "matrix_control", expect_abort=.false., &
            failure_message="every well-formed matrix and projection call was expected to run cleanly")
    end subroutine test_matrix_control_succeeds

    subroutine test_parse_column_non_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_non_string", expect_abort=.true., &
            failure_message="%parse_column on a column that is not text was expected to abort", &
            required_stderr="parse_column: this column holds PK_INT32, not text")
    end subroutine test_parse_column_non_string_aborts

    subroutine test_parse_column_vector_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_vector_source", expect_abort=.true., &
            failure_message="%parse_column on a string VECTOR column was expected to abort", &
            required_stderr="parse_column: this column holds PK_STRING_VEC, not text")
    end subroutine test_parse_column_vector_source_aborts

    subroutine test_parse_column_bad_target_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_bad_target", expect_abort=.true., &
            failure_message="%parse_column into a kind it has no parser for was expected to abort", &
            required_stderr="parse_column: PK_STRING is not a target this verb can parse into")
    end subroutine test_parse_column_bad_target_aborts

    subroutine test_parse_column_invalid_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_invalid_token", expect_abort=.true., &
            failure_message="%parse_column with an unknown invalid= policy was expected to abort", &
            required_stderr="parse_column: invalid=""skip"" is not a policy")
    end subroutine test_parse_column_invalid_token_aborts

    !> The message must carry all three of the row, the column and the text. Asserted as three
    !! separate checks rather than one long string, so a failure says WHICH of the three went
    !! missing -- and because a caller fixing a data file cannot act on a message naming only the
    !! column.
    subroutine test_parse_column_malformed_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_malformed", expect_abort=.true., &
            failure_message="%parse_column meeting unreadable text was expected to abort", &
            required_stderr="parse_column: row 2 holds")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "parse_column_malformed", expect_abort=.true., &
            failure_message="%parse_column meeting unreadable text was expected to abort", &
            required_stderr="""2 3"", which is not readable as int32")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "parse_column_malformed", expect_abort=.true., &
            failure_message="%parse_column meeting unreadable text was expected to abort", &
            required_stderr="column 'bad'")
    end subroutine test_parse_column_malformed_aborts

    !> A 300-character unreadable value is echoed truncated. Not cosmetic: ifx's ERROR STOP
    !! runtime corrupts the heap once the composed message reaches 8192 bytes, and this guard is
    !! by construction reached with text the caller controls.
    subroutine test_parse_column_malformed_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_malformed_long", expect_abort=.true., &
            failure_message="%parse_column on overlong unreadable text was expected to abort", &
            required_stderr=repeat("z", 100) // "...")
        if (allocated(error)) return
        ! The negative half: 101 z's must NOT appear, or the cap did nothing.
        call check_scenario_exit_status_and_no_output(error, "parse_column_malformed_long", &
            expect_abort=.true., &
            failure_message="%parse_column on overlong unreadable text was expected to abort", &
            forbidden_text=repeat("z", 101))
    end subroutine test_parse_column_malformed_long_aborts

    subroutine test_parse_column_to_name_exists_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_to_name_exists", expect_abort=.true., &
            failure_message="%parse_column writing over an existing column was expected to abort", &
            required_stderr="parse_column: to_name=""i"" is already a column of this table")
    end subroutine test_parse_column_to_name_exists_aborts

    subroutine test_parse_column_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the scenario's parallel region never " // &
            "opens, so the table is never shared and the guard has nothing to refuse")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, "parse_column_shared", expect_abort=.true., &
            failure_message="%parse_column on a table another thread may hold was expected to abort", &
            required_stderr="parse_column: this table was not opened by this thread")
    end subroutine test_parse_column_shared_aborts

    subroutine test_reload_after_parse_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "reload_after_parse_column", expect_abort=.true., &
            failure_message="%reload of a parsed column was expected to abort without force=", &
            required_stderr="reload: this column holds values written into the table")
    end subroutine test_reload_after_parse_column_aborts

    subroutine test_format_column_string_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "format_column_string_source", expect_abort=.true., &
            failure_message="%format_column on a column that is already text was expected to abort", &
            required_stderr="format_column: this column is already text")
    end subroutine test_format_column_string_source_aborts

    subroutine test_format_column_vector_source_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "format_column_vector_source", expect_abort=.true., &
            failure_message="%format_column on a vector column was expected to abort", &
            required_stderr="format_column: a PK_STRING_VEC column cannot be rendered")
    end subroutine test_format_column_vector_source_aborts

    subroutine test_format_column_fmt_on_temporal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "format_column_fmt_on_temporal", expect_abort=.true., &
            failure_message="%format_column with fmt= on a temporal column was expected to abort", &
            required_stderr="format_column: fmt= is not accepted for a PK_DATE column")
    end subroutine test_format_column_fmt_on_temporal_aborts

    subroutine test_parse_column_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "parse_column_predefined", expect_abort=.true., &
            failure_message="%parse_column in place on a predefined column without force= was " // &
                "expected to abort", &
            required_stderr="parse_column: this is a predefined column")
    end subroutine test_parse_column_predefined_aborts

    subroutine test_format_column_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "format_column_predefined", expect_abort=.true., &
            failure_message="%format_column in place on a predefined column without force= was " // &
                "expected to abort", &
            required_stderr="format_column: this is a predefined column")
    end subroutine test_format_column_predefined_aborts

    !> `force=.false.` spelled out must behave exactly as an absent `force=`. A guard keyed on
    !> `present(force)` rather than on its value passes the scenario above and lets this one
    !> through, so the pair is the discriminator.
    subroutine test_format_column_predefined_false_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "format_column_predefined_false", &
            expect_abort=.true., &
            failure_message="%format_column with force=.false. on a predefined column was " // &
                "expected to abort, exactly as an absent force= does", &
            required_stderr="format_column: this is a predefined column")
    end subroutine test_format_column_predefined_false_aborts

    !> `%cast` carries the same guard as the two conversion verbs above, and the same pair of
    !> scenarios for the same reason.
    subroutine test_cast_predefined_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "cast_predefined", expect_abort=.true., &
            failure_message="%cast on a predefined column without force= was expected to abort", &
            required_stderr="cast: this is a predefined column")
    end subroutine test_cast_predefined_aborts

    subroutine test_cast_predefined_false_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "cast_predefined_false", &
            expect_abort=.true., &
            failure_message="%cast with force=.false. on a predefined column was expected to " // &
                "abort, exactly as an absent force= does", &
            required_stderr="cast: this is a predefined column")
    end subroutine test_cast_predefined_false_aborts

    subroutine test_convert_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "convert_control", expect_abort=.false., &
            failure_message="every well-formed conversion call was expected to run cleanly")
    end subroutine test_convert_control_succeeds

    subroutine test_explode_wrong_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "explode_wrong_length", expect_abort=.true., &
            failure_message="%explode with a short count list was expected to abort", &
            required_stderr="explode: the count list has 2 entries but the table has 4 rows")
    end subroutine test_explode_wrong_length_aborts

    subroutine test_explode_negative_count_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "explode_negative_count", expect_abort=.true., &
            failure_message="%explode with a negative count was expected to abort", &
            required_stderr="explode: count -2 at row 2 is negative")
    end subroutine test_explode_negative_count_aborts

    subroutine test_explode_row_count_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "explode_row_count_overflow", expect_abort=.true., &
            failure_message="%explode past huge(int64) rows was expected to abort", &
            required_stderr="explode: the exploded row count passes huge(0_int64) at row 2")
    end subroutine test_explode_row_count_overflow_aborts

    subroutine test_explode_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the scenario's parallel region never " // &
            "opens, so the table is never shared and the guard has nothing to refuse")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, "explode_shared", expect_abort=.true., &
            failure_message="%explode on a table another thread may hold was expected to abort", &
            required_stderr="explode: this table was not opened by this thread")
    end subroutine test_explode_shared_aborts

    subroutine test_duplicated_bad_keep_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "duplicated_bad_keep", expect_abort=.true., &
            failure_message="%duplicated with an unknown keep= token was expected to abort", &
            required_stderr="duplicated: keep=""middle"" is not a policy")
    end subroutine test_duplicated_bad_keep_aborts

    subroutine test_drop_duplicates_bad_keep_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "drop_duplicates_bad_keep", expect_abort=.true., &
            failure_message="%drop_duplicates with an unknown keep= token was expected to abort", &
            required_stderr="drop_duplicates: keep=""middle"" is not a policy")
    end subroutine test_drop_duplicates_bad_keep_aborts

    subroutine test_duplicated_all_unorderable_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "duplicated_all_unorderable", expect_abort=.true., &
            failure_message="%duplicated() over a resident vector column was expected to abort", &
            required_stderr="duplicated: a PK_INT32_VEC column cannot be a sort key")
    end subroutine test_duplicated_all_unorderable_aborts

    subroutine test_duplicated_all_nothing_resident_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "duplicated_all_nothing_resident", &
            expect_abort=.true., &
            failure_message="%drop_duplicates() with nothing resident was expected to abort", &
            required_stderr="drop_duplicates: no column of this table has been read yet")
    end subroutine test_duplicated_all_nothing_resident_aborts

    subroutine test_duplicated_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "duplicated_unknown_column", expect_abort=.true., &
            failure_message="%duplicated naming a column that is not there was expected to abort", &
            required_stderr="duplicated: no column of this name")
    end subroutine test_duplicated_unknown_column_aborts

    subroutine test_sort_by_values_wrong_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sort_by_values_wrong_length", expect_abort=.true., &
            failure_message="%sort_by_values with a short value list was expected to abort", &
            required_stderr="sort_by_values: the value list has 2 entries but the table has 4 rows")
    end subroutine test_sort_by_values_wrong_length_aborts

    subroutine test_argsort_by_values_wrong_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "argsort_by_values_wrong_length", expect_abort=.true., &
            failure_message="%argsort_by_values with a short value list was expected to abort", &
            required_stderr="argsort_by_values: the value list has 2 entries")
    end subroutine test_argsort_by_values_wrong_length_aborts

    subroutine test_drop_duplicates_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the scenario's parallel region never " // &
            "opens, so the table is never shared and the guard has nothing to refuse")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, "drop_duplicates_shared", expect_abort=.true., &
            failure_message="%drop_duplicates on a table another thread may hold was expected to abort", &
            required_stderr="drop_duplicates: this table was not opened by this thread")
    end subroutine test_drop_duplicates_shared_aborts

    subroutine test_sort_by_values_shared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the scenario's parallel region never " // &
            "opens, so the table is never shared and the guard has nothing to refuse")
        return
#endif
        call check_scenario_exit_status_and_stderr(error, "sort_by_values_shared", expect_abort=.true., &
            failure_message="%sort_by_values on a table another thread may hold was expected to abort", &
            required_stderr="sort_by_values: this table was not opened by this thread")
    end subroutine test_sort_by_values_shared_aborts

    subroutine test_remap_length_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "remap_length_mismatch", expect_abort=.true., &
            failure_message="a lookup table with more keys than values was expected to abort", &
            required_stderr="pf_remap: from_keys has 3 keys but to_values has 2 values")
    end subroutine test_remap_length_mismatch_aborts

    subroutine test_remap_duplicate_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "remap_duplicate_key", expect_abort=.true., &
            failure_message="a lookup table repeating a key was expected to abort", &
            required_stderr="pf_remap: from_keys repeats a key at positions 1 and 3")
    end subroutine test_remap_duplicate_key_aborts

    subroutine test_remap_unmapped_no_policy_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "remap_unmapped_no_policy", expect_abort=.true., &
            failure_message="an unmapped value with no default= and no found= was expected to abort", &
            required_stderr="pf_remap: the value at position 2 matches no key in from_keys")
    end subroutine test_remap_unmapped_no_policy_aborts

    subroutine test_value_counts_count_name_collision_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "value_counts_count_name_collision", &
            expect_abort=.true., &
            failure_message="counting a column already called count was expected to abort", &
            required_stderr="value_counts: the counted column is already called ""count""")
    end subroutine test_value_counts_count_name_collision_aborts

    subroutine test_value_counts_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "value_counts_unknown_column", &
            expect_abort=.true., &
            failure_message="counting a column that does not exist was expected to abort", &
            required_stderr="value_counts: no column of this name")
    end subroutine test_value_counts_unknown_column_aborts

    subroutine test_value_counts_unorderable_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "value_counts_unorderable", &
            expect_abort=.true., &
            failure_message="counting a vector column was expected to abort", &
            required_stderr="value_counts: a PK_INT32_VEC column cannot be a sort key")
    end subroutine test_value_counts_unorderable_aborts

    subroutine test_counting_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "counting_control", expect_abort=.false., &
            failure_message="every well-formed counting and mapping call was expected to run cleanly")
    end subroutine test_counting_control_succeeds

    subroutine test_rowverbs_control_succeeds(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "rowverbs_control", expect_abort=.false., &
            failure_message="every well-formed row-set verb call was expected to run cleanly")
    end subroutine test_rowverbs_control_succeeds

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

    !> `parquet_set_file_date` refuses a value on ten separate grounds and names the one at fault.
    !>
    !> **Shape and range are independent checks and both halves are swept**, because a shape-only
    !> validator accepts `2020-13-02T03:04:05` -- nineteen characters, every digit and separator
    !> where it belongs, naming a month that does not exist. Asserting only that "something aborted"
    !> would not tell the ten apart, so each arm asserts its own message: a wrong message means the
    !> wrong branch fired, which is exactly how a mis-indexed character position hides.
    !>
    !> Every scenario runs the same two controls first -- a valid date must be accepted and
    !> round-trip, and an EMPTY string must be accepted, that being the documented way back to
    !> reading the clock and precisely what a guard written for "non-empty" would reject.
    subroutine test_settings_file_date_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat, k
        logical :: found
        character(len=40), parameter :: names(10) = [character(len=40) :: &
            "settings_file_date_wrong_shape", "settings_file_date_too_short", &
            "settings_file_date_not_a_digit", "settings_file_date_bad_date_sep", &
            "settings_file_date_bad_time_sep", "settings_file_date_out_of_range", &
            "settings_file_date_bad_day", "settings_file_date_bad_hour", &
            "settings_file_date_bad_minute", "settings_file_date_bad_second"]
        character(len=40), parameter :: wanted(10) = [character(len=40) :: &
            "character 11 must be 'T'", "it is 16 characters long, not 19", &
            "character 10 is not a digit", "the date separators must be '-'", &
            "the time separators must be ':'", "the month is out of the range 01-12", &
            "the day is out of the range 01-31", "the hour is out of the range 00-23", &
            "the minute is out of the range 00-59", "the second is out of the range 00-59"]

        do k = 1, size(names)
            call run_error_scenario(trim(names(k)), exitstat, cmdstat, out_file, err_file)
            call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
            if (allocated(error)) return
            call check(error, exitstat /= 0, "an invalid file date was expected to abort: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, &
                "control: accepted and read back '2020-12-02T03:04:05'", found)
            call check(error, found, &
                "a valid file date must be accepted and round-trip first, or the abort proves " // &
                "only that the setter rejects everything: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, "control: empty accepted, length now 0", found)
            call check(error, found, &
                "an empty file date must be accepted and must clear the setting -- it is the " // &
                "documented way back to the clock: " // trim(names(k)))
            if (allocated(error)) return
            call scenario_capture_contains(out_file, err_file, trim(wanted(k)), found)
            call check(error, found, &
                "the abort must name the part of the date that is wrong: " // trim(names(k)))
            if (allocated(error)) return
        end do
    end subroutine test_settings_file_date_invalid_aborts

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
    !> channel.
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
    !> pushed verbosity entirely, so this is the only test that would catch the two drifting
    !> apart. The warning provoked here is printed from parquet_wrapper.cpp. The mirrored settings
    !> reach C++ at reader/writer OPEN, not when a setter is called -- so a knob changed while a
    !> reader is already open does not apply to that reader.
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

    !> `parquet_emit_advice` is a CLASS, not a wording: `"silent"` takes it and leaves an ordinary
    !> warning standing, in the same process.
    !>
    !> Four assertions over two runs, and none of them is redundant. The two at `"normal"` are what
    !> stop the `"silent"` half passing against a build that never coarsens an `nside=` or never
    !> reaches the qc check at all; the warning at `"silent"` is what stops it passing against a
    !> channel that is simply broken, or against a `"silent"` that silences everything.
    !>
    !> The prefix is asserted too, because it is the channel's job to supply it: a site that
    !> hand-wrote `"NOTE: "` into its own message would satisfy a text-only assertion.
    subroutine test_settings_advice_is_its_own_class(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "settings_advice_normal", expect_abort=.false., &
            failure_message="the advice scenario was not expected to abort", &
            required_stderr="NOTE: pf_spatial_index%build_sky: nside= was coarsened from")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "settings_advice_normal", expect_abort=.false., &
            failure_message="the advice scenario must also raise the qc warning it is controlled against", &
            required_stderr="WARNING: qc violation")
        if (allocated(error)) return
        call check_scenario_exit_status_and_no_output(error, "settings_advice_silent", expect_abort=.false., &
            failure_message="the silenced advice scenario was not expected to abort", &
            forbidden_text="nside= was coarsened")
        if (allocated(error)) return
        call check_scenario_exit_status_and_stderr(error, "settings_advice_silent", expect_abort=.false., &
            failure_message="silent must take the advice and leave the warning about the data", &
            required_stderr="WARNING: qc violation")
    end subroutine test_settings_advice_is_its_own_class

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

    !> The three matchers match part of a STRING; a numeric column is refused, naming the operator
    !> and the column's actual type.
    subroutine test_filter_starts_with_non_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_starts_with_non_string_column", &
            expect_abort=.true., &
            failure_message="starts_with against an integer filter column was expected to abort", &
            required_stderr="only supported for string columns")
    end subroutine test_filter_starts_with_non_string_column_aborts

    !> A pattern is a string literal and must be double-quoted, exactly as an equality value is.
    subroutine test_filter_starts_with_unquoted_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_starts_with_unquoted_value", &
            expect_abort=.true., &
            failure_message="an unquoted starts_with pattern was expected to abort", &
            required_stderr="must be double-quoted")
    end subroutine test_filter_starts_with_unquoted_value_aborts

    !> The negative control for convert_temporal_filter_values' positive operator test: the message
    !> must be about starts_with and the column's type, NOT about a malformed ISO-8601 literal. If
    !> the temporal conversion ever stops skipping the matchers, the pattern below parses as a real
    !> date and this assertion is what notices.
    subroutine test_filter_starts_with_on_temporal_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_starts_with_on_temporal_column", &
            expect_abort=.true., &
            failure_message="starts_with against a date filter column was expected to abort", &
            required_stderr="only supported for string columns")
    end subroutine test_filter_starts_with_on_temporal_column_aborts

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

    !> parquet_reader_set_filter's twin of the guard above, for the verdict array a set-valued
    !> (`in`/`not_in`) clause hands over: it is indexed by PHYSICAL row, and the counts it was built
    !> from must be the file's own.
    !>
    !> Unreachable through the public API for the same reason -- parquet_prepare_set_leaves derives
    !> both counts from a freshly opened private reader on the same file -- so the scenario arms it
    !> with parquet_debug_set_force_pre_leaf_mismatch. See
    !> `scenario_filter_pre_leaf_row_count_mismatch` (test/error_scenarios_io.f90) for what the
    !> misalignment would cost if the guard were removed.
    subroutine test_filter_pre_leaf_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: saw_control, saw_message

        call run_error_scenario("filter_pre_leaf_row_count_mismatch", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, &
            "pre-evaluated filter leaves disagreeing with the file's row count were expected to abort")
        if (allocated(error)) return

        ! The negative control comes FIRST, for the same reason it does above: a guard that fired on
        ! every set-valued clause would pass the assertion above just as happily.
        call scenario_capture_contains(out_file, err_file, &
            "control: the set-valued open succeeded with the row-count hook clear", saw_control)
        call check(error, saw_control, &
            "the identical open must succeed with the row-count hook clear, or the abort below " // &
            "proves only that the guard fires, not that it fires for the right reason")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, &
            "pre-evaluated filter leaves describe 5 row(s) in 1 row group(s)", saw_message)
        call check(error, saw_message, &
            "the abort must name what the leaves describe as well as what the file holds")
    end subroutine test_filter_pre_leaf_row_count_mismatch_aborts

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

    !> The message must name the procedure the CALLER wrote, not the one the guard happens to sit
    !> behind. `check_writer_open` hard-coded "parquet_write_column" until 2026-09-05, so every one
    !> of the nine public procedures taking a writer reported that name; only
    !> `parquet_write_column` itself was right. Its sibling above is the negative control -- it must
    !> keep naming `parquet_write_column`, or this pair would pass against a guard that had simply
    !> been re-hard-coded to `parquet_new_row_group`.
    subroutine test_new_row_group_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "new_row_group_before_open", expect_abort=.true., &
            failure_message="starting a row group on an unopened writer was expected to abort", &
            required_stderr="parquet_new_row_group: writer has not been opened (call parquet_open_writer first)")
    end subroutine test_new_row_group_before_open_aborts

    !> The guard added to parquet_get_metadata on 2026-09-05, and the reason it needed one: with
    !> `default=` present the call used to SUCCEED on a reader that was never opened, returning the
    !> default, so a use-before-open looked exactly like an absent key. The scenario passes
    !> `default=` for that reason -- it is the path that could not abort before.
    subroutine test_get_metadata_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_metadata_before_open", expect_abort=.true., &
            failure_message="reading metadata from an unopened reader was expected to abort", &
            required_stderr="parquet_get_metadata: reader has not been opened (call parquet_open_reader first)")
    end subroutine test_get_metadata_before_open_aborts

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

    !> Error CONTEXT lines survive `verbosity="errors_only"`, the strictest level, and travel with
    !> `message_stream` like everything else the library writes.
    !!
    !! `parquet_emit_error_context` (src/parquet_settings_base.f90) carries half a contract: the
    !! lines it prints are the ones parquet_close_writer's abort message deliberately leaves out,
    !! the output filename and the schema name, so **no verbosity level may suppress them** --
    !! losing them turns a diagnosable failure into one that names nothing. Where they GO is not
    !! part of that: they follow `message_stream`, so a caller who routed the library's output to
    !! stderr reads one stream to diagnose one failure instead of two.
    !!
    !! **This needs check_scenario_streams, not the usual helper.** Every other assertion here goes
    !! through scenario_capture_contains, which searches BOTH captured streams -- so a context line
    !! that stopped honouring `message_stream` and stayed on stdout would look byte-identical to it.
    !! check_scenario_streams asserts the text is on one stream *and absent from the other*, which
    !! is the half with teeth (the decorative-knob rule).
    !!
    !! **The negative control is the warning the scenario emits first.** Under the same two settings
    !! it must reach neither stream. Without it, this test would pass just as happily against a
    !! build where `parquet_set_verbosity` did nothing at all -- it is the warning vanishing that
    !! proves the level was really in force while the context lines came through it.
    subroutine test_error_context_survives_suppression(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call check_scenario_streams(error, "close_writer_missing_write_silenced", &
            "parquet_close_writer: output file:", "stderr", &
            "the error-context filename must survive verbosity=errors_only and follow message_stream=stderr")
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
            "nothing about the error-context channel surviving the same setting")
    end subroutine test_error_context_survives_suppression

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
    !> `.claude/rules/build.md`'s "Stale build cache"). Avoiding that guess is half the reason the
    !> argument-0 path is primary -- do not demote it back.
    !>
    !> Written as a subroutine with an allocatable `character` argument rather than a function
    !> returning one, per `.claude/rules/build.md`'s "Hand-run compiles": gfortran's codegen for receiving
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
    !> the concurrency guard's stderr message, promised in
    !> doc/pages/operating/thread-safety.md) -- not applied broadly to every scenario, since most
    !> diagnostic wording isn't a documented contract and shouldn't be
    !> locked down by regression tests.
    !>
    !> `forbidden_stderr`, when given, must NOT appear in the capture. It is for a scenario whose
    !> abort is only correct if the run reached it CLEANLY -- the text names the wrong ending, so
    !> the assertion fails on a run that ends the wrong way while printing the right message.
    subroutine check_scenario_exit_status_and_stderr(error, scenario, expect_abort, failure_message, required_stderr, &
                                                     forbidden_stderr)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message, required_stderr
        character(len=*), intent(in), optional :: forbidden_stderr
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: aborted, found, present_when_forbidden

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
        if (allocated(error)) return

        if (present(forbidden_stderr)) then
            call scenario_capture_contains(out_file, err_file, forbidden_stderr, present_when_forbidden)
            call check(error, .not. present_when_forbidden, &
                "expected stderr NOT to contain '" // trim(forbidden_stderr) // "' for scenario '" // &
                trim(scenario) // "'")
        end if
    end subroutine check_scenario_exit_status_and_stderr

    !> Runs `scenario` capturing stdout and stderr SEPARATELY, and asserts `text` appears on the
    !> stream named by `expect_on` and is absent from the other.
    !>
    !> The absence half is the entire point. Every other helper here redirects with `2>&1`, so a
    !> message that moved from stdout to stderr produces a byte-identical capture -- a
    !> `message_stream` test written against those helpers passes whether the setting works or is
    !> ignored completely, which is the decorative-knob failure this exists to forbid. Asserting
    !> presence alone here would have the same hole, since the message is present either way; it is
    !> asserting it is *gone from the other stream* that has teeth.
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

    !> How many lines of `path` contain `text`. A missing file counts as zero, so a scenario that
    !> wrote nothing to one stream reads as an absence rather than as a test error -- the same
    !> convention file_contains uses.
    subroutine file_count_containing(path, text, n)
        character(len=*), intent(in) :: path, text
        integer, intent(out) :: n
        character(len=1024) :: line
        integer :: unit, ios

        n = 0
        open(newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, text) > 0) n = n + 1
        end do
        close(unit)
    end subroutine file_count_containing

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

    !> The hash engine's max_rows report probes a string key through its own shape.
    subroutine test_join_max_rows_hash_string_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_hash_string_key", &
            expect_abort=.true., &
            failure_message="a string-keyed hash join over max_rows= was expected to abort", &
            required_stderr="max_rows")
    end subroutine test_join_max_rows_hash_string_key_aborts

    !> And a composite key through the tuple shape.
    subroutine test_join_max_rows_hash_tuple_key_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "join_max_rows_hash_tuple_key", &
            expect_abort=.true., &
            failure_message="a two-column hash join over max_rows= was expected to abort", &
            required_stderr="max_rows")
    end subroutine test_join_max_rows_hash_tuple_key_aborts
    !

end module test_errors
