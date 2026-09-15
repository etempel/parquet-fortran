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
!> Test driver (a): the `pf_`-prefixed utility tier — sorting, statistics, random numbers and
!> sampling, spatial indexing, HEALPix, logging and path/string helpers.
!!
!! **This program executes no `bind(C)` call at all**, which is the property the split exists to
!! create: it can therefore be built and run under `nagfor -C=undefined`, where an executed
!! `bind(C)` call is miscompiled (see `feature_tests.md` section 2). It neither warms Arrow's
!! memory pool nor primes the error scenarios, because it reaches neither.
!!
!! Membership is a rule, not a preference — `feature_tests.md` section 5 — and
!! `check_test_runner_partition` (`tools/check_source_conventions.py`) enforces it.
program run_tester_pf
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_healpix, only : collect_tests_parquet_healpix
    use test_index, only : collect_tests_index
    use test_integrate, only : collect_tests_integrate
    use test_integrate_omp, only : collect_tests_integrate_omp
    use test_index_omp, only : collect_tests_index_omp
    use test_index_multimap, only : collect_tests_index_multimap
    use test_index_strings, only : collect_tests_index_strings
    use test_healpix_grid, only : collect_tests_healpix_grid
    use test_healpix_reference, only : collect_tests_healpix_reference
    use test_healpix_tier_b, only : collect_tests_healpix_tier_b
    use test_logging, only : collect_tests_logging
    use test_random, only : collect_tests_parquet_random, collect_tests_parquet_random_perm
    use test_random_dist, only : collect_tests_parquet_random_dist
    use test_random_omp, only : collect_tests_parquet_random_omp
    use test_random_weighted, only : collect_tests_parquet_random_weighted
    use test_spatial, only : collect_tests_parquet_spatial
    use test_stats, only : collect_tests_parquet_stats
    use test_toml, only : collect_tests_toml, collect_tests_toml_serial
    use test_utils, only : collect_tests_utils
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    !
    testsuites = [ &
        new_testsuite("utils", collect_tests_utils), &
        new_testsuite("index", collect_tests_index), &
        new_testsuite("integrate", collect_tests_integrate), &
        new_testsuite("integrate_omp", collect_tests_integrate_omp), &
        new_testsuite("index_omp", collect_tests_index_omp), &
        new_testsuite("index_multimap", collect_tests_index_multimap), &
        new_testsuite("index_strings", collect_tests_index_strings), &
        new_testsuite("logging", collect_tests_logging), &
        new_testsuite("toml", collect_tests_toml), &
        new_testsuite("toml_serial", collect_tests_toml_serial), &
        new_testsuite("stats", collect_tests_parquet_stats), &
        new_testsuite("spatial", collect_tests_parquet_spatial), &
        new_testsuite("healpix", collect_tests_parquet_healpix), &
        new_testsuite("healpix_reference", collect_tests_healpix_reference), &
        new_testsuite("healpix_tier_b", collect_tests_healpix_tier_b), &
        new_testsuite("healpix_grid", collect_tests_healpix_grid), &
        new_testsuite("random", collect_tests_parquet_random), &
        new_testsuite("random_perm", collect_tests_parquet_random_perm), &
        new_testsuite("random_omp", collect_tests_parquet_random_omp), &
        new_testsuite("random_weighted", collect_tests_parquet_random_weighted), &
        new_testsuite("random_dist", collect_tests_parquet_random_dist) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester_pf
