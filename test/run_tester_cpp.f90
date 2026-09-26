!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
! https://github.com/fortran-lang/test-drive
!
! To exclude code lines from gcovr coverage report, use the following markers:
! GCOVR_EXCL_START
! GCOVR_EXCL_STOP
! GCOVR_EXCL_LINE
! GCOVR_EXCL_FUNCTION
!
!> Test driver (c): every suite that reaches the C++/Arrow layer.
!!
!! **Not undef-safe, by construction**: an executed `bind(C)` call is miscompiled under
!! `nagfor -C=undefined`, and reaching that layer is exactly what these suites are for. It warms
!! Arrow's memory pool for the reason `parquet_warmup_memory_pool` gives — test-drive runs a
!! suite's tests concurrently, and Arrow's `default_memory_pool()` singleton is not safely
!! reentrant on first call.
!!
!! **It forks nothing.** The abort-path tests that used to live in `writing`, `reading`, `maml` and
!! `metadata` are in `run_tester_errors`.
program run_tester_cpp
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_container_nested, only : collect_tests_container_nested
    use test_diagnostics, only : collect_tests_diagnostics
    use test_examples, only : collect_tests_parquet_examples
    use test_filter, only : collect_tests_filter
    use test_filter_screen, only : collect_tests_filter_screen
    use test_list_read, only : collect_tests_parquet_list_read
    use test_list_write, only : collect_tests_parquet_list_write
    use test_logging_env, only : collect_tests_logging_env
    use test_maml, only : collect_tests_parquet_maml
    use test_map_read, only : collect_tests_parquet_map_read
    use test_map_write, only : collect_tests_parquet_map_write
    use test_metadata, only : collect_tests_parquet_metadata
    use test_module_surface, only : collect_tests_module_surface
    use test_openmp, only : collect_tests_parquet_openmp, collect_tests_parquet_openmp_write
    use test_reading, only : collect_tests_parquet_reading
    use test_settings, only : collect_tests_parquet_settings
    use test_sort, only : collect_tests_sort
    use test_sorting_cpp, only : collect_tests_sorting_cpp
    use test_struct_read, only : collect_tests_parquet_struct_read
    use test_struct_write, only : collect_tests_parquet_struct_write
    use test_table, only : collect_tests_parquet_table
    use test_table_codegen, only : collect_tests_table_codegen
    use test_table_container, only : collect_tests_table_container
    use test_table_display, only : collect_tests_table_display
    use test_table_parallel, only : collect_tests_table_parallel
    use test_table_join, only : collect_tests_table_join, collect_tests_table_join_hash
    use test_table_verbs, only : collect_tests_table_verbs
    use test_table_rowverbs, only : collect_tests_table_rowverbs
    use test_table_fill, only : collect_tests_table_fill
    use test_table_matrix, only : collect_tests_table_matrix
    use test_table_convert, only : collect_tests_table_convert
    use test_table_index, only : collect_tests_table_index
    use test_table_group, only : collect_tests_table_group
    use test_table_stream, only : collect_tests_table_stream
    use test_temporal_cpp, only : collect_tests_parquet_temporal_cpp
    use test_writing, only : collect_tests_parquet_writing
    use parquet_bindings, only : parquet_warmup_memory_pool
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    call parquet_warmup_memory_pool()
    !
    testsuites = [ &
        new_testsuite("writing", collect_tests_parquet_writing), &
        new_testsuite("reading", collect_tests_parquet_reading), &
        new_testsuite("maml", collect_tests_parquet_maml), &
        new_testsuite("examples", collect_tests_parquet_examples), &
        new_testsuite("metadata", collect_tests_parquet_metadata), &
        ! openmp_write must run (and fully complete) as its own suite before openmp:
        ! testdrive runs every test *within* one suite concurrently with each other by
        ! default, but different suites still run sequentially relative to each other (see
        ! the note on collect_tests_parquet_openmp_write in test_openmp.f90) -- several
        ! tests in the openmp suite depend on files this one writes.
        new_testsuite("openmp_write", collect_tests_parquet_openmp_write), &
        new_testsuite("openmp", collect_tests_parquet_openmp), &
        new_testsuite("temporal_cpp", collect_tests_parquet_temporal_cpp), &
        new_testsuite("list_read", collect_tests_parquet_list_read), &
        new_testsuite("list_write", collect_tests_parquet_list_write), &
        new_testsuite("struct_read", collect_tests_parquet_struct_read), &
        new_testsuite("struct_write", collect_tests_parquet_struct_write), &
        new_testsuite("map_read", collect_tests_parquet_map_read), &
        new_testsuite("map_write", collect_tests_parquet_map_write), &
        new_testsuite("filter", collect_tests_filter), &
        new_testsuite("filter_screen", collect_tests_filter_screen), &
        new_testsuite("sort", collect_tests_sort), &
        new_testsuite("sorting_cpp", collect_tests_sorting_cpp), &
        new_testsuite("table", collect_tests_parquet_table), &
        new_testsuite("table_parallel", collect_tests_table_parallel), &
        new_testsuite("table_codegen", collect_tests_table_codegen), &
        new_testsuite("table_container", collect_tests_table_container), &
        new_testsuite("table_display", collect_tests_table_display), &
        ! table_join_hash runs the join suite with the hash engine forced through a process-global
        ! hook, and table_join's own collector puts the hook back to automatic -- so the two stay
        ! in THIS order, hash first, or the forced engine leaks into every suite after it.
        new_testsuite("table_join_hash", collect_tests_table_join_hash), &
        new_testsuite("table_join", collect_tests_table_join), &
        new_testsuite("table_verbs", collect_tests_table_verbs), &
        new_testsuite("table_rowverbs", collect_tests_table_rowverbs), &
        new_testsuite("table_fill", collect_tests_table_fill), &
        new_testsuite("table_matrix", collect_tests_table_matrix), &
        new_testsuite("table_convert", collect_tests_table_convert), &
        new_testsuite("table_index", collect_tests_table_index), &
        new_testsuite("table_group", collect_tests_table_group), &
        new_testsuite("table_stream", collect_tests_table_stream), &
        new_testsuite("container_nested", collect_tests_container_nested), &
        new_testsuite("settings", collect_tests_parquet_settings), &
        new_testsuite("module_surface", collect_tests_module_surface), &
        new_testsuite("diagnostics", collect_tests_diagnostics), &
        new_testsuite("logging_env", collect_tests_logging_env) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester_cpp
