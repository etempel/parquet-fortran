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
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type, select_suite, run_selected,get_argument,init_color_output
    use test_writing, only : collect_tests_parquet_writing
    use test_reading, only : collect_tests_parquet_reading
    use test_maml, only : collect_tests_parquet_maml
    use test_errors, only : collect_tests_parquet_errors
    use test_examples, only : collect_tests_parquet_examples
    use test_metadata, only : collect_tests_parquet_metadata
    use test_openmp, only : collect_tests_parquet_openmp_write, collect_tests_parquet_openmp
    use test_parquet_string, only : collect_tests_parquet_string
    use test_columns, only : collect_tests_parquet_columns
    use test_temporal, only : collect_tests_parquet_temporal
    use parquet_bindings, only : parquet_warmup_memory_pool
    !
    implicit none
    integer :: stat, is, cmdstat
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    !
    stat = 0
    !
    ! Tests write scratch parquet/maml files under test_run/; ensure it
    ! exists up front so a fresh checkout or `rm -rf test_run` doesn't
    ! break tests that don't create the directory themselves.
    call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
    !
    ! Forces Arrow's default_memory_pool() singleton to be constructed here,
    ! single-threaded, before test-drive starts running tests within a suite
    ! concurrently via OpenMP. Its first call is not safely reentrant in
    ! every Arrow build -- without this, two threads racing to be the first
    ! caller can abort with "Internal error: cannot create default memory
    ! pool" (observed nondeterministically, e.g. reliably with
    ! OMP_NUM_THREADS=2 on one Arrow 23.0.1 build).
    call parquet_warmup_memory_pool()
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
        new_testsuite("columns", collect_tests_parquet_columns) &
        ]
    !
    ! command line argument for a specific testsuite and test
    call get_argument(1, suite_name)
    call get_argument(2, test_name)
    !
    call init_color_output(.true.)
    !
    if (allocated(suite_name)) then
        is = select_suite(testsuites, suite_name)
        if (is > 0 .and. is <= size(testsuites)) then
            if (allocated(test_name)) then
                write(error_unit, fmt) "Suite:", testsuites(is)%name
                call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
                if (stat < 0) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat, &
                    parallel=suite_is_safe_to_parallelize(testsuites(is)%name))
            end if
        else
            write(error_unit, fmt) "Available testsuites"
            do is = 1, size(testsuites)
                write(error_unit, fmt) "-", testsuites(is)%name
            end do
            error stop 1
        end if
    else
        do is = 1, size(testsuites)
            write(error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat, &
                parallel=suite_is_safe_to_parallelize(testsuites(is)%name))
        end do
    end if
    !
    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if
    !
contains

    !> "writing", "errors", "metadata" and "maml" contain tests that call
    !> execute_command_line (fork()+exec() under the hood) to drive the
    !> error_scenarios helper as a subprocess. test-drive runs the tests
    !> within a suite concurrently via `!$omp parallel do` by default;
    !> forking while sibling OpenMP worker threads are alive mid-barrier is
    !> unsafe with libiomp5 (observed: deterministic SIGSEGV inside
    !> __kmp_invoke_microtask). Run those suites' tests sequentially instead
    !> so the fork always happens with no other team threads active.
    !>
    !> "parquet_string" no longer needs an entry here: it used to, because of a
    !> gfortran/OpenMP runtime bug (not a bug in parquet_strings.f90's own
    !> logic) that silently corrupted memory when multiple threads
    !> concurrently called a function returning `character(len=:), allocatable`
    !> on a type with two or more allocatable components -- `parquet_string_column`
    !> hit this via its (formerly function-form) `get`/`summary`. Fixed at the
    !> source by converting every such accessor in this library to a
    !> subroutine with an `intent(out)`/`intent(inout)` allocatable `character`
    !> argument instead (see "Build and compiler notes" in CLAUDE.md), which
    !> this suite's re-enabled parallel execution exercises as its own ongoing
    !> regression check.
    logical function suite_is_safe_to_parallelize(name) result(safe)
        character(len=*), intent(in) :: name
        safe = .not. (name == "writing" .or. name == "errors" .or. name == "metadata" .or. name == "maml")
    end function suite_is_safe_to_parallelize

end program tester
