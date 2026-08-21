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
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type, unittest_type, select_suite, run_selected, &
        get_argument, init_color_output
    use test_writing, only : collect_tests_parquet_writing
    use test_reading, only : collect_tests_parquet_reading
    use test_maml, only : collect_tests_parquet_maml
    use test_errors, only : collect_tests_parquet_errors, prime_error_scenarios
    use test_examples, only : collect_tests_parquet_examples
    use test_metadata, only : collect_tests_parquet_metadata
    use test_openmp, only : collect_tests_parquet_openmp_write, collect_tests_parquet_openmp
    use test_parquet_string, only : collect_tests_parquet_string
    use test_columns, only : collect_tests_parquet_columns
    use test_filter, only : collect_tests_filter
    use test_filter_screen, only : collect_tests_filter_screen
    use test_sort, only : collect_tests_sort
    use test_sorting, only : collect_tests_parquet_sorting
    use test_temporal, only : collect_tests_parquet_temporal
    use test_table, only : collect_tests_parquet_table
    use test_table_parallel, only : collect_tests_table_parallel
    use test_string_parallel, only : collect_tests_string_parallel
    use test_table_codegen, only : collect_tests_table_codegen
    use test_settings, only : collect_tests_parquet_settings
    use test_module_surface, only : collect_tests_module_surface
    use test_diagnostics, only : collect_tests_diagnostics
    use test_random, only : collect_tests_parquet_random, collect_tests_parquet_random_perm
    use test_random_omp, only : collect_tests_parquet_random_omp
    use test_random_weighted, only : collect_tests_parquet_random_weighted
    use test_random_dist, only : collect_tests_parquet_random_dist
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
        new_testsuite("columns", collect_tests_parquet_columns), &
        new_testsuite("filter", collect_tests_filter), &
        new_testsuite("filter_screen", collect_tests_filter_screen), &
        new_testsuite("sort", collect_tests_sort), &
        new_testsuite("sorting", collect_tests_parquet_sorting), &
        new_testsuite("table", collect_tests_parquet_table), &
        new_testsuite("table_parallel", collect_tests_table_parallel), &
        new_testsuite("string_parallel", collect_tests_string_parallel), &
        new_testsuite("table_codegen", collect_tests_table_codegen), &
        new_testsuite("settings", collect_tests_parquet_settings), &
        new_testsuite("module_surface", collect_tests_module_surface), &
        new_testsuite("diagnostics", collect_tests_diagnostics), &
        new_testsuite("random", collect_tests_parquet_random), &
        new_testsuite("random_perm", collect_tests_parquet_random_perm), &
        new_testsuite("random_omp", collect_tests_parquet_random_omp), &
        new_testsuite("random_weighted", collect_tests_parquet_random_weighted), &
        new_testsuite("random_dist", collect_tests_parquet_random_dist) &
        ]
    !
    ! command line argument for a specific testsuite and test
    call get_argument(1, suite_name)
    call get_argument(2, test_name)
    !
    ! Pre-run every error scenario once, in parallel, before any suite starts -- see
    ! prime_error_scenarios in test_errors.f90 for what this buys and why it is safe here and
    ! nowhere else (exactly one fork, with no OpenMP team active).
    !
    ! Two gates, and both exist to keep a targeted run fast rather than to protect correctness --
    ! priming is a pure optimisation, and a suite that is not primed simply spawns its scenarios
    ! on demand exactly as it always did:
    !   * a named single test never primes, since it would pay for ~690 scenarios to run one;
    !   * a named suite primes only if it consumes essentially the whole set, i.e. only "errors".
    ! Priming is all-or-nothing, so anything narrower than that is a bad trade -- see
    ! suite_drives_error_scenarios. Both gates are therefore allowed to go stale in the safe
    ! direction: a missing name costs that suite its speedup, nothing more.
    if (.not. allocated(test_name)) then
        if (.not. allocated(suite_name)) then
            call prime_error_scenarios()
        else if (suite_drives_error_scenarios(suite_name)) then
            call prime_error_scenarios()
        end if
    end if
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
                call run_suite(testsuites(is), error_unit, stat)
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
            call run_suite(testsuites(is), error_unit, stat)
        end do
    end if
    !
    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if
    !
contains

    !> Runs one suite, choosing between test-drive's own parallel driver and a serial loop here.
    !!
    !! **A suite excluded from test-drive's per-test parallelism must not go through
    !! `run_testsuite` at all, and `parallel=.false.` is not enough.** That argument leaves the
    !! `!$omp parallel do` in place and switches it off with an `if` clause, which still OPENS a
    !! parallel region -- an inactive one, with a team of a single thread. Inside it
    !! `omp_get_level()` is 1 while `omp_in_parallel()` is `.false.`, so every test in the suite
    !! runs one level down from the top of the program, and any OpenMP team the library then opens
    !! is a NESTED team.
    !!
    !! libgomp deadlocks on that shape. Measured on gfortran 15.2 / macOS arm64: a full `fpm test`
    !! hung about one run in three, always somewhere in "sorting", with the main thread and its
    !! workers parked on one libgomp mutex that nobody held. See `feature_risks.md` Risk-104 for
    !! the reduction and the bisect. `parquet_sorting` now refuses to open a team when it can see
    !! it would be nested, which is the library-side half of that fix -- but the refusal would then
    !! apply to EVERY test in an excluded suite, including the nine whose whole purpose is to assert
    !! that a requested team really is opened. Those nine would have had to stop asserting it.
    !!
    !! Driving the tests through `run_selected` instead runs each with no enclosing region at all.
    !! The library sees `omp_get_level() == 0` and threads exactly as it does for a caller in a
    !! serial program, so the suite keeps both properties: no nested team is ever built, and the
    !! threading assertions stay meaningful.
    !!
    !! The progress line is reproduced here because `run_testsuite` prints it from inside its own
    !! loop. Losing it would cost the one diagnostic that says which test a hang stopped in, which
    !! is how this bug was located in the first place.
    subroutine run_suite(suite, unit, stat)
        type(testsuite_type), intent(in) :: suite       !! the suite to run
        integer, intent(in) :: unit                     !! unit the results are written to
        integer, intent(inout) :: stat                  !! running count of failed tests
        type(unittest_type), allocatable :: tests(:)
        character(len=32) :: counter
        integer :: it, jt

        if (suite_is_safe_to_parallelize(suite%name)) then
            call run_testsuite(suite%collect, unit, stat, parallel=.true.)
            return
        end if

        call suite%collect(tests)
        ! `run_selected` finds its test BY NAME, so two tests sharing one name would run the first
        ! twice and the second never -- silently, with the count still looking right. test-drive's
        ! own loop indexes instead and cannot notice, so nothing else would report it.
        do it = 1, size(tests)
            do jt = it + 1, size(tests)
                if (tests(it)%name == tests(jt)%name) then
                    write(unit, '(a)') "run_suite: suite '" // trim(suite%name) // "' has two tests named '" // &
                        trim(tests(it)%name) // "'; a name-selected run would skip one of them"
                    error stop 1
                end if
            end do
        end do

        do it = 1, size(tests)
            write(counter, '("(",i0,"/",i0,")")') it, size(tests)
            write(unit, '(1x, 3(1x, a))') "Starting", trim(tests(it)%name), "... " // trim(counter)
            call run_selected(suite%collect, tests(it)%name, unit, stat)
        end do
    end subroutine run_suite

    !> "sorting" is excluded for the same reason as "filter_screen": its counting-fast-path and
    !> parallel-threshold tests drive parquet_set_sort_counting_path and
    !> parquet_set_sort_counting_bucket_limit, which are process-global. **Promoting those from debug
    !> hooks to real settings did not weaken this** -- a parquet_settings knob is a saved module
    !> variable, exactly as process-global as the C++ static it replaced, so the exclusion is as
    !> necessary as it ever was. Run concurrently, a sibling test could turn the counting path off
    !> during that test's own "fast" measurement, leaving it comparing the comparator path against
    !> itself -- which PASSES while testing nothing, the failure mode this project treats as worst.
    !> The suite is pure in-memory work and runs in well under a second, so the lost parallelism
    !> costs nothing.
    !>
    !> "sort" is excluded for the same reason as "sorting", and was ALREADY relying on it before
    !> anyone noticed: its counting-path tests set parquet_set_sort_counting_path, a process-global
    !> value, and now also read the process-global comparison counter to prove the two halves really
    !> reached different engines. The setting half was a latent hazard for as long as those tests
    !> have existed -- it never bit only because their assertions were path-agnostic, so a sibling
    !> test stealing the setting mid-run changed no result. The counter half would have been
    !> immediately and visibly wrong: measured at 11 and 14 comparisons where both should have been
    !> 0 and nonzero, purely from sibling tests sorting at the same time. The suite is ~25 fast
    !> file-based tests and the lost parallelism is not measurable.
    !>
    !> "filter_screen" is excluded for a different reason from the four below: the screen setting
    !> (parquet_set_statistics_prescreen) and the pruned-row-group count
    !> (parquet_debug_get_row_groups_pruned) are both process-global -- the first because every
    !> setting is, the second because parquet_reader's components are private and a pruned-row-group
    !> count does not belong in the public API. Run concurrently, one test would read another's count
    !> and turn off another's screen mid-run.
    !>
    !> "writing", "errors", "metadata" and "maml" contain tests that call
    !> execute_command_line (fork()+exec() under the hood) to drive the
    !> error_scenarios helper as a subprocess. test-drive runs the tests
    !> within a suite concurrently via `!$omp parallel do` by default;
    !> forking while sibling OpenMP worker threads are alive mid-barrier is
    !> unsafe with libiomp5 (observed: deterministic SIGSEGV inside
    !> __kmp_invoke_microtask). Run those suites' tests sequentially instead
    !> so the fork always happens with no other team threads active.
    !>
    !> "diagnostics" is excluded because every phase timer it asserts on is a process-global C++
    !> static that ACCUMULATES. Each test resets one, performs the operation it measures, and
    !> asserts the counter moved -- so a sibling test running a filtered read, a padded string read
    !> or a threaded sort in the same window would charge the very counter under assertion. That
    !> does not fail, it PASSES, against a timer that recorded nothing itself, which is this
    !> project's worst failure mode. Its sort test additionally forces two process-global settings
    !> (the counting fast path off, the parallel row threshold down) for the same reason "sorting"
    !> is excluded. The suite is three tests over small fixtures and costs nothing.
    !>
    !> "settings" is excluded for the same reason as those two, one level up: every setting in
    !> parquet_settings is process-global by definition, and its thread-pool tests resize Arrow's
    !> single shared CPU pool. Run concurrently, one test would resize the pool out from under
    !> another's assertion -- and, worse, restore a capacity a sibling had deliberately changed.
    !> The suite is pure in-memory work and runs in milliseconds, so the lost parallelism costs
    !> nothing.
    !>
    !> "random_omp" is excluded for exactly the same reason as "table_parallel" below, and it is
    !> the clearest case of it: the suite exists to prove that parquet_random gives identical
    !> values under schedule(static), schedule(dynamic,1) and several thread counts. Run inside
    !> test-drive's own `!$omp parallel do`, each of those regions is NESTED, and with nesting
    !> disabled by default a nested region gets a team of ONE -- so every arm would really be the
    !> serial arm and every comparison would pass without two threads ever having run. The test
    !> carries its own vacuity guard on the team size, so that failure is loud rather than silent,
    !> but the guard reports a broken setup; this exclusion is what makes the setup right. Enabling
    !> nested parallelism instead is not an option: it is process-global OpenMP state, and it would
    !> be changed underneath every concurrently running sibling suite. The suite is two in-memory
    !> tests and costs a fraction of a second.
    !>
    !> "table_parallel" is excluded for a reason that is not about shared state at all, and it
    !> cannot work any other way. Its tests compare a table mutated on several threads against an
    !> independent clone mutated with parquet_set_table_threads(1) -- and a table mutation resolves
    !> to ONE thread inside an existing parallel region, deliberately, since a nested region is the
    !> caller's business. Run inside test-drive's own `!$omp parallel do`, every test in it would
    !> compare the serial path against itself and pass while testing nothing, which is this
    !> project's worst failure mode. Its thread counter is process-global as well, which is the
    !> second and independent reason.
    !>
    !> "string_parallel" is excluded for the SAME reason as "table_parallel", and it is a separate
    !> suite from "parquet_string" precisely so that only these tests pay for it. A string column's
    !> bulk operation resolves to ONE thread inside an existing parallel region, deliberately, so
    !> every test in it would compare the serial path against itself and pass while testing nothing.
    !> That is not hypothetical here: two mutations survived exactly this way while these tests were
    !> still in the parallelized suite (feature_string_parallel.md S4). Its tests also write
    !> parquet_set_string_threads and the payload-floor override, both process-global, which is the
    !> second and independent reason.
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
        safe = .not. (name == "writing" .or. name == "errors" .or. name == "metadata" .or. name == "maml" &
            .or. name == "filter_screen" .or. name == "sorting" .or. name == "sort" .or. name == "settings" &
            .or. name == "table_parallel" .or. name == "string_parallel" .or. name == "diagnostics" &
            .or. name == "random_omp" .or. name == "random_perm" .or. name == "module_surface")
    end function suite_is_safe_to_parallelize

    !> Whether running just this suite is worth pre-running the whole scenario set for. Purely a
    !> cost question -- see the gate at the top of this program.
    !!
    !! **Only "errors" qualifies, because priming is all-or-nothing.** It runs every scenario in
    !! `tools/run_error_scenarios.sh`'s array (~690 of them), so it pays off only for the suite
    !! that goes on to consume essentially all of them. Several other suites do drive scenarios --
    !! "writing", "metadata", "maml" and "reading" each drive a few dozen at most -- and for those
    !! the trade is backwards: ~690 subprocesses to save a few dozen. They spawn theirs on demand
    !! instead, which is what a targeted `fpm test run_tester -- <suite>` should do.
    logical function suite_drives_error_scenarios(name) result(drives)
        character(len=*), intent(in) :: name
        drives = (name == "errors")
    end function suite_drives_error_scenarios

end program tester
