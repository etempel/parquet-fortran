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
!> Driver for unit testing
program tester
    use testdrive, only : new_testsuite, testsuite_type
    use test_writing, only : collect_tests_parquet_writing
    use test_reading, only : collect_tests_parquet_reading
    use test_maml, only : collect_tests_parquet_maml
    use test_errors, only : collect_tests_parquet_errors, prime_error_scenarios
    use test_examples, only : collect_tests_parquet_examples
    use test_metadata, only : collect_tests_parquet_metadata
    use test_openmp, only : collect_tests_parquet_openmp_write, collect_tests_parquet_openmp
    use test_parquet_string, only : collect_tests_parquet_string
    use test_columns, only : collect_tests_parquet_columns
    use test_list, only : collect_tests_parquet_list
    use test_list_read, only : collect_tests_parquet_list_read
    use test_list_write, only : collect_tests_parquet_list_write
    use test_struct, only : collect_tests_parquet_struct
    use test_struct_read, only : collect_tests_parquet_struct_read
    use test_struct_write, only : collect_tests_parquet_struct_write
    use test_map, only : collect_tests_parquet_map
    use test_map_read, only : collect_tests_parquet_map_read
    use test_map_write, only : collect_tests_parquet_map_write
    use test_filter, only : collect_tests_filter
    use test_filter_screen, only : collect_tests_filter_screen
    use test_sort, only : collect_tests_sort
    use test_sorting, only : collect_tests_parquet_sorting
    use test_sorting_cpp, only : collect_tests_sorting_cpp
    use test_stats, only : collect_tests_parquet_stats
    use test_temporal, only : collect_tests_parquet_temporal
    use test_temporal_cpp, only : collect_tests_parquet_temporal_cpp
    use test_table, only : collect_tests_parquet_table
    use test_table_parallel, only : collect_tests_table_parallel
    use test_string_parallel, only : collect_tests_string_parallel
    use test_table_codegen, only : collect_tests_table_codegen
    use test_table_container, only : collect_tests_table_container
    use test_container_nested, only : collect_tests_container_nested
    use test_settings, only : collect_tests_parquet_settings
    use test_module_surface, only : collect_tests_module_surface
    use test_diagnostics, only : collect_tests_diagnostics
    use test_logging, only : collect_tests_logging
    use test_logging_env, only : collect_tests_logging_env
    use test_utils, only : collect_tests_utils
    use test_random, only : collect_tests_parquet_random, collect_tests_parquet_random_perm
    use test_random_omp, only : collect_tests_parquet_random_omp
    use test_healpix, only : collect_tests_parquet_healpix
    use test_healpix_reference, only : collect_tests_healpix_reference
    use test_healpix_tier_b, only : collect_tests_healpix_tier_b
    use test_healpix_grid, only : collect_tests_healpix_grid
    use test_random_weighted, only : collect_tests_parquet_random_weighted
    use test_random_dist, only : collect_tests_parquet_random_dist
    use test_spatial, only : collect_tests_parquet_spatial
    use parquet_bindings, only : parquet_warmup_memory_pool
    use test_runner_support, only : run_tester_args, run_tester_main
    !
    implicit none
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    !
    ! Forces Arrow's default_memory_pool() singleton to be constructed here,
    ! single-threaded, before test-drive starts running tests within a suite
    ! concurrently via OpenMP. Its first call is not safely reentrant in
    ! every Arrow build -- without this, two threads racing to be the first
    ! caller can abort with "Internal error: cannot create default memory
    ! pool" (observed nondeterministically, e.g. reliably with
    ! OMP_NUM_THREADS=2 on one Arrow 23.0.1 build).
    !
    ! It is called HERE rather than in test_runner_support because that module is shared by every
    ! runner and must stay Arrow-free -- see its own header. This is also the one bind(C) call a
    ! runner makes, and it is safe under nagfor's -C=undefined only because it takes NO ARGUMENTS:
    ! that option corrupts every argument after the first of a call into C. Giving this procedure
    ! an argument would break every undef-safe runner at once.
    call parquet_warmup_memory_pool()
    !
    ! Pre-run every error scenario once, in parallel, before any suite starts -- see
    ! prime_error_scenarios in test_errors.f90 for what this buys and why it is safe here and
    ! nowhere else (exactly one fork, with no OpenMP team active).
    !
    ! Two gates, and both exist to keep a targeted run fast rather than to protect correctness --
    ! priming is a pure optimisation, and a suite that is not primed simply spawns its scenarios
    ! on demand exactly as it always did:
    !   * a named single test never primes, since it would pay for every scenario to run one;
    !   * a named suite primes only if it consumes essentially the whole set, i.e. only "errors".
    call run_tester_args(suite_name, test_name)
    if (.not. allocated(test_name)) then
        if (.not. allocated(suite_name)) then
            call prime_error_scenarios()
        else if (suite_name == "errors") then
            call prime_error_scenarios()
        end if
    end if
    !
    ! Add all testsuites here as a comma separated list
    testsuites = [ &
        new_testsuite("writing", collect_tests_parquet_writing), &
        new_testsuite("reading", collect_tests_parquet_reading), &
        new_testsuite("maml", collect_tests_parquet_maml), &
        new_testsuite("errors", collect_tests_parquet_errors), &
        new_testsuite("examples", collect_tests_parquet_examples), &
        new_testsuite("metadata", collect_tests_parquet_metadata), &
        ! openmp_write must run (and fully complete) as its own suite before
        ! openmp: testdrive runs every test *within* one suite concurrently
        ! with each other by default, but different suites still run
        ! sequentially relative to each other (see the note on
        ! collect_tests_parquet_openmp_write in test_openmp.f90) -- several
        ! tests in the openmp suite depend on files this one writes.
        new_testsuite("openmp_write", collect_tests_parquet_openmp_write), &
        new_testsuite("openmp", collect_tests_parquet_openmp), &
        new_testsuite("parquet_string", collect_tests_parquet_string), &
        new_testsuite("temporal", collect_tests_parquet_temporal), &
        new_testsuite("temporal_cpp", collect_tests_parquet_temporal_cpp), &
        new_testsuite("columns", collect_tests_parquet_columns), &
        new_testsuite("list", collect_tests_parquet_list), &
        new_testsuite("list_read", collect_tests_parquet_list_read), &
        new_testsuite("list_write", collect_tests_parquet_list_write), &
        new_testsuite("struct", collect_tests_parquet_struct), &
        new_testsuite("struct_read", collect_tests_parquet_struct_read), &
        new_testsuite("struct_write", collect_tests_parquet_struct_write), &
        new_testsuite("map", collect_tests_parquet_map), &
        new_testsuite("map_read", collect_tests_parquet_map_read), &
        new_testsuite("map_write", collect_tests_parquet_map_write), &
        new_testsuite("filter", collect_tests_filter), &
        new_testsuite("filter_screen", collect_tests_filter_screen), &
        new_testsuite("sort", collect_tests_sort), &
        new_testsuite("sorting", collect_tests_parquet_sorting), &
        new_testsuite("sorting_cpp", collect_tests_sorting_cpp), &
        new_testsuite("stats", collect_tests_parquet_stats), &
        new_testsuite("table", collect_tests_parquet_table), &
        new_testsuite("table_parallel", collect_tests_table_parallel), &
        new_testsuite("string_parallel", collect_tests_string_parallel), &
        new_testsuite("table_codegen", collect_tests_table_codegen), &
        new_testsuite("table_container", collect_tests_table_container), &
        new_testsuite("container_nested", collect_tests_container_nested), &
        new_testsuite("settings", collect_tests_parquet_settings), &
        new_testsuite("module_surface", collect_tests_module_surface), &
        new_testsuite("diagnostics", collect_tests_diagnostics), &
        new_testsuite("logging", collect_tests_logging), &
        new_testsuite("logging_env", collect_tests_logging_env), &
        new_testsuite("utils", collect_tests_utils), &
        new_testsuite("random", collect_tests_parquet_random), &
        new_testsuite("random_perm", collect_tests_parquet_random_perm), &
        new_testsuite("random_omp", collect_tests_parquet_random_omp), &
        new_testsuite("random_weighted", collect_tests_parquet_random_weighted), &
        new_testsuite("random_dist", collect_tests_parquet_random_dist), &
        new_testsuite("spatial", collect_tests_parquet_spatial), &
        new_testsuite("healpix", collect_tests_parquet_healpix), &
        new_testsuite("healpix_reference", collect_tests_healpix_reference), &
        new_testsuite("healpix_tier_b", collect_tests_healpix_tier_b), &
        new_testsuite("healpix_grid", collect_tests_healpix_grid) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program tester
