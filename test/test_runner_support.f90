!> The driver every `run_tester*` program shares: argument parsing, suite selection, and the
!> per-suite choice between test-drive's parallel driver and a serial loop.
!!
!! **This module is deliberately Arrow-free and scenario-free, and both properties are
!! load-bearing rather than tidy.** Every runner imports it, so anything it reaches, every runner
!! reaches: one `use parquet_bindings` here would put a C++ call into `run_tester_pf` and
!! `run_tester` and undo the whole point of the split, and one `use test_errors` would make every
!! runner able to fork a scenario. It therefore imports `testdrive` and `iso_fortran_env` and
!! nothing else, and the two things a runner may need beyond that are done BY THE PROGRAM:
!!
!!   * `parquet_warmup_memory_pool()` -- only the runners that touch Arrow call it;
!!   * `prime_error_scenarios()` -- only `run_tester_errors` calls it.
!!
!! That is why `run_tester_args` is separate from `run_tester_main`: a program parses the command
!! line first, decides whether to prime or warm up, and only then hands the suites over.
!!
!! See feature_tests.md for the runner split this exists to serve.
module test_runner_support
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, testsuite_type, unittest_type, select_suite, run_selected, &
        get_argument, init_color_output
    implicit none
    private

    public :: run_tester_args
    public :: run_tester_main

    character(len=*), parameter :: fmt = '("#", *(1x, a))' !! how each progress line is written.

contains

    !> Parses the optional suite name and test name from the command line.
    !!
    !! Separate from `run_tester_main` so a program can decide what to set up before running --
    !! `run_tester_errors` primes its scenario set only for a whole-suite run, and needs the parsed
    !! arguments to know that.
    subroutine run_tester_args(suite_name, test_name)
        character(len=:), allocatable, intent(out) :: suite_name !! argument 1, unallocated when absent.
        character(len=:), allocatable, intent(out) :: test_name  !! argument 2, unallocated when absent.

        call get_argument(1, suite_name)
        call get_argument(2, test_name)
    end subroutine run_tester_args

    !> Runs the requested suite, the requested test within it, or every suite, and stops with a
    !> nonzero status if anything failed.
    !!
    !! Creates `test_run/` first: tests write scratch parquet/maml files there, and a fresh
    !! checkout or an `rm -rf test_run` would otherwise break the ones that do not create it
    !! themselves.
    subroutine run_tester_main(testsuites, suite_name, test_name)
        ! INTENT(INOUT), not intent(in): test-drive's `select_suite` declares its suite array
        ! intent(inout), so an intent(in) dummy cannot be passed to it. Nothing here modifies it.
        type(testsuite_type), intent(inout) :: testsuites(:)         !! every suite this runner owns.
        character(len=:), allocatable, intent(in) :: suite_name      !! from `run_tester_args`.
        character(len=:), allocatable, intent(in) :: test_name       !! from `run_tester_args`.
        integer :: stat, is, cmdstat

        stat = 0
        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        call init_color_output(.true.)

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

        if (stat > 0) then
            write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
            error stop 1
        end if
    end subroutine run_tester_main

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
    !> "spatial" is excluded for the same reason again, in its sharpest form: the probe counter,
    !> the rebuild counter, the resolved thread count and the forced cell size
    !> (parquet_debug_spatial_probe_count, _rebuilds, _threads_used, parquet_debug_set_spatial_cell)
    !> are all process-global, because the state they force is private to parquet_spatial and there
    !> is no bind(C) boundary to hide a hook behind. A sibling test forcing a cell mid-run would
    !> leave the probe test asserting a count nobody produced -- and since a spatial query's ANSWERS
    !> do not depend on the cell size, every correctness test in the suite would go on passing while
    !> the tuner tests measured each other. The suite is pure in-memory work and costs under a
    !> second serially.
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
        ! **Every `*_errors` suite is excluded, BY SHAPE rather than by name.** Such a suite exists
        ! precisely because its tests drive `test/error_scenarios.f90`, and a test that is not
        ! answered from the primed cache spawns a subprocess -- so running several concurrently
        ! forks from inside an OpenMP team, which is the hazard `prime_error_scenarios` is built
        ! to avoid (see its own comment: exactly one fork, with no team active). Matching the
        ! suffix rather than listing the four means the next split inherits this for free; listing
        ! them is what a future split would forget. Both failures were observed, in order:
        ! `writing_errors` segfaulted because 41 tests that had always run serially suddenly ran
        ! concurrently, and `reading_errors` segfaulted with only FOUR tests, because all four
        ! forked at once where before they had been spread thinly through a 56-test suite.
        if (len(name) > 7) then
            if (name(len(name) - 6:) == "_errors") then
                safe = .false.
                return
            end if
        end if
        safe = .not. (name == "writing" .or. name == "errors" .or. name == "metadata" .or. name == "maml" &
            .or. name == "filter_screen" .or. name == "sorting" .or. name == "sorting_cpp" &
            .or. name == "sort" .or. name == "settings" &
            .or. name == "table_parallel" .or. name == "string_parallel" .or. name == "diagnostics" &
            .or. name == "random_omp" .or. name == "random_perm" .or. name == "module_surface" &
            .or. name == "spatial" .or. name == "logging" .or. name == "stats")
    end function suite_is_safe_to_parallelize

end module test_runner_support
