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
    !! workers parked on one libgomp mutex that nobody held. See the reduction and the bisect.
    !! `parquet_sorting` now refuses to open a team when it can see it would be nested, which is the
    !! library-side half of that fix -- but the refusal would then apply to EVERY test in an
    !! excluded suite, including the nine whose whole purpose is to assert that a requested team
    !! really is opened. Those nine would have had to stop asserting it.
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
    !> "spatial_serial" is excluded for the same reason again, in its sharpest form: the probe
    !> counter, the rebuild counter, the resolved thread count, the forced cell size and the
    !> line-of-sight counters (parquet_debug_spatial_probe_count, _rebuilds, _threads_used,
    !> _los_bounds, parquet_debug_set_spatial_cell, ...) are all process-global, because the state
    !> they force is private to parquet_spatial and there is no bind(C) boundary to hide a hook
    !> behind. A sibling test forcing a cell mid-run would leave the probe test asserting a count
    !> nobody produced -- and since a spatial query's ANSWERS do not depend on the cell size, every
    !> correctness test in the suite would go on passing while the tuner tests measured each other.
    !>
    !> **Only the 38 tests that observe such a hook are in it.** "spatial" was ONE suite of 130
    !> until the per-test timings showed it was the most expensive thing in `fpm test`, 15 s of a
    !> 62 s run, because all 130 paid for the 38. The other 92 are the "spatial" suite and run
    !> concurrently; check_spatial_suite_split_is_by_observability re-derives the membership.
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
    !> still in the parallelized suite. Its tests also write parquet_set_string_threads and the
    !> payload-floor override, both process-global, which is the second and independent reason.
    !>
    !> "index_strings" is excluded because one of its tests sets
    !> parquet_debug_set_index_string_hash_bits, the process-global hook that narrows every
    !> string hash so that the map's collision chain can be reached at all. A sibling test
    !> building a string map while it is set would probe at full width a map built narrow, and
    !> find nothing -- the hook is read on every hash, on the calling thread. The suite is pure
    !> in-memory work over small fixtures and costs a fraction of a second serially.
    !>
    !> "integrate_omp" is excluded for the nested-team reason "random_omp" states, with nothing
    !> process-global about it at all: parquet_integrate holds no variable that is not a parameter,
    !> and the suite exists to demonstrate that by integrating on several threads at once. Run
    !> inside test-drive's own `!$omp parallel do`, each of its regions would be NESTED and would
    !> get a team of ONE -- so "the concurrent answers equal the serial ones" would hold because
    !> nothing ran concurrently, which is this project's worst failure mode. Its third test asserts
    !> the team size for exactly that reason, so a collapse is loud rather than silent; this
    !> exclusion is what keeps it from happening. The suite is pure arithmetic over no fixture and
    !> costs a fraction of a second serially.
    !>
    !> "interpolate_omp" is excluded for the same nested-team reason: parquet_interpolate holds no
    !> variable that is not a parameter, and the suite demonstrates that by evaluating one object and
    !> building many on a whole team at once. Its third test asserts the team size, so a region that
    !> collapsed to a team of one would be reported rather than pass. Serially it costs well under a
    !> second.
    !>
    !> "transform_omp" is excluded for the same nested-team reason: parquet_transform holds no
    !> variable that is not a parameter, and the suite demonstrates that by running all four
    !> transforms over four hundred different sequences on a whole team at once and requiring the
    !> serial bits back exactly. Its second test asserts the team size and that every sequence was
    !> really transformed, so a region that collapsed to a team of one would be reported rather
    !> than pass -- there the concurrent arm is a second serial arm and agreement means nothing.
    !> `check_parquet_transform_holds_no_state` is the static half of the same claim. Serially it
    !> costs well under a second.
    !>
    !> "cosmology_omp" is excluded for the same nested-team reason: parquet_cosmology's only
    !> module variables are the two test-only debug hooks, which this suite deliberately never
    !> touches, and it demonstrates the absence of any other shared state by reading one built
    !> object and building four more on a whole team at once. Its third test asserts the team size,
    !> so a region that collapsed to a team of one would be reported rather than pass. Serially it
    !> costs well under a second.
    !>
    !> "table_join_hash" is excluded because its collector forces the join's pair-list engine
    !> through a process-global hook (parquet_debug_set_join_engine) and every test then asserts,
    !> through the process-global observable parquet_debug_join_engine_used, that the hash engine
    !> really ran. A join the hash engine declines -- order="key", a logical key -- runs the sort
    !> engine and writes THAT into the observable, so a sibling test running concurrently could
    !> read a decline that was not its own between its join and its assertion; and one test moves
    !> the hook itself to prove it switches. The "table_join" suite, which runs the same tests
    !> with the sort engine forced, has neither problem and stays parallel. Pure in-memory work
    !> over five-row fixtures; serial it costs well under a second.
    !>
    !> "kde_serial" is excluded because its tests silence `%print` through the process-global
    !> `verbosity` setting, so a concurrent printer elsewhere would write nothing, or a test's
    !> negative control would. The concurrent "kde" suite stays parallel: the one global it writes
    !> is `pf_kde_grid%add`'s team counter, which every `%add` inside test-drive's region sets to
    !> 1 (no nested team opens there) and which no test of that suite reads.
    !>
    !> "cosmology_config_serial" is excluded for the reason "toml_serial" is: its two tests
    !> attach a file sink to the process-global DEFAULT logger to read the unknown-key sweep back,
    !> and a sink attached or closed while a sibling test is writing to one is an abort. The
    !> concurrent "cosmology_config" suite touches no global at all -- it reads committed fixtures
    !> and writes files named for their own test -- and stays parallel.
    !>
    !> "kde_omp" is excluded for the nested-team reason "integrate_omp" states, and because it
    !> reads that process-global team counter straight after each `%add` it asserts: run inside
    !> test-drive's region, every team would collapse to one and a concurrent `%add` elsewhere
    !> could overwrite the counter between a call and its assertion. Serially it costs well under
    !> a second.
    !>
    !> "sorting_cpp" is excluded for the reason "sorting" gives, many times over: twenty-two of its
    !> thirty-five tests drive a process-global, and they are of both kinds. The FORCING kind --
    !> parquet_debug_use_fortran_sort_engine, parquet_set_sort_counting_path,
    !> parquet_debug_set_sort_depth_limit, parquet_debug_set_sort_radix_fail_alloc and the merge
    !> overrides its co-rank sweep cannot run without -- decides which engine and which path a
    !> SIBLING's sort takes, and each is restored at the end of the test that set it, so a sibling
    !> is left running against a setting that was never its own. The OBSERVING kind --
    !> parquet_debug_sort_threads_used, _tie_threads_used, _offsets_threads_used, _radix_passes,
    !> _refine_runs -- is a stale counter holding whatever the last sort anywhere in the process
    !> wrote, which is the form this project treats as worst: a test reading a sibling's count
    !> PASSES.
    !>
    !> "columns_parallel" is excluded for the reason "table_parallel" and "string_parallel" give,
    !> one tier down: its single test gathers a column on several threads and compares it with the
    !> same gather forced serial, and a bulk column operation resolves to ONE thread inside an
    !> existing parallel region -- so run inside test-drive's own region both arms would be the
    !> serial path and the comparison would pass while testing nothing. It reads
    !> parquet_debug_column_gather_threads and parquet_debug_string_bulk_threads to prove the
    !> threaded arm really threaded, and writes parquet_debug_set_string_min_bytes to reach it, all
    !> three process-global, which is the second and independent reason.
    !>
    !> "index_omp" is excluded for BOTH reasons at once, which is why it is stated separately:
    !> thirteen of its twenty-three tests open a region or pass threads=, so the nested-team
    !> collapse "random_omp" describes would silently serialise them, and fourteen read a global
    !> observable -- parquet_debug_index_threads_used, _get_many_threads_used, _spills,
    !> _concurrent_builds -- straight after the call they are about. Either failure alone passes;
    !> together they would leave the suite green against work that never happened.
    !>
    !> "optimize_omp" and "prima_omp" are excluded for the nested-team reason "random_omp" states:
    !> all six of their tests run a multistart or a differential-evolution population over a team
    !> and compare the answer with the serial one, so a team of one would compare the serial path
    !> with itself. Both also read parquet_debug_optimize_threads_used, the process-global
    !> observable their team assertions are written against.
    !>
    !> "sphere_omp" is excluded for the same nested-team reason, and its own file header says so:
    !> its one test fills a polygon and a mask over a dynamic schedule at three team sizes and
    !> compares them with the serial fill bit for bit, which a nested team of one would satisfy
    !> without two threads ever having run.
    !>
    !> "random_perm" is excluded because half of its six tests force the permutation construction
    !> itself through process-global hooks -- parquet_debug_set_perm_parity, _force_feistel and
    !> _rounds -- and read parquet_debug_perm_config back to say which one answered. The parity
    !> hook is the sharpest: it fixes the sign of every permutation the process produces, so a
    !> sibling drawing one while it is set gets a permutation it did not ask for. At 6.2 s it is the
    !> second most expensive suite on this list, after "spatial_serial" at 9.4 s, and that cost is
    !> inherent rather than a consequence of the exclusion: its sample sizes ARE its assertions,
    !> since a coverage or k-tuple claim is a statement about how many draws were taken, and its
    !> four slowest tests are four of those claims.
    !>
    !> "stats" is excluded because fifteen of its hundred and fifty tests reset a process-global
    !> counter, do the work they measure, and then assert the counter moved by exactly so much:
    !> parquet_debug_reset_stats_scans/_sorts against parquet_debug_stats_scans/_sorts. A
    !> sibling computing a statistic or taking a quantile in that window charges the very counter
    !> under assertion, which does not fail -- it PASSES, against a measurement that never
    !> happened. Two more force the threading floor (parquet_debug_set_stats_min_per_thread) and
    !> read parquet_debug_stats_team, one forces the quantile's sort threshold, and one silences
    !> %print through the process-global verbosity, for the reasons "settings" and "kde_serial"
    !> give. The other hundred and thirty-five pay for those fifteen, which is the shape that made
    !> the spatial split worth doing -- but not here: the whole suite is 0.51 s, so a split would
    !> buy nothing measurable. Its two slowest tests are 0.41 s of that, and neither is one of the
    !> fifteen.
    !>
    !> "module_surface" is excluded for the reason "settings" gives, one entry module at a time:
    !> every one of its thirty tests round-trips process-global settings -- set, get, assert,
    !> restore -- through a single module import, ninety-seven setter calls in the file, among them
    !> twenty-two of parquet_set_verbosity and seventeen of parquet_set_message_stream. Run
    !> concurrently, a sibling would be read between the set and the get; and because each helper
    !> restores the value it FOUND rather than a default, a sibling suite's deliberate setting
    !> would be put back to whatever this suite happened to see. Pure in-memory work; serially it
    !> costs nothing.
    !>
    !> "cosmology_serial" is excluded because all three of its tests drive a process-global: two
    !> force a build mode (parquet_debug_set_cosmology_exact_nu,
    !> parquet_debug_set_cosmology_max_neval) that every concurrent %init would take as well, and
    !> the third reads parquet_debug_cosmology_neval, which reports what the LAST %init spent and
    !> is zeroed by each one -- so a sibling building a cosmology between the call and the read
    !> hands it that build's count. The concurrent "cosmology" suite touches none of the three.
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
        ! `logging_env` is here for the same reason as `logging`: both drive the process-global
        ! DEFAULT logger, so two of their tests running at once can close a sink the other is
        ! writing to -- an abort reading "writing a record failed ... Unit number is negative".
        ! It was absent from this list while the suite held a single test, where the omission
        ! could not bite and `test_logging_env.f90`'s own header already claimed the exclusion;
        ! adding a second test to that suite is what found it.
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
            .or. name == "columns_parallel" &
            .or. name == "random_omp" .or. name == "random_perm" .or. name == "module_surface" &
            .or. name == "spatial_serial" .or. name == "logging" .or. name == "logging_env" &
            .or. name == "toml_serial" .or. name == "cosmology_config_serial" &
            .or. name == "cosmology_serial" &
            .or. name == "index_omp" .or. name == "index_strings" &
            .or. name == "integrate_omp" .or. name == "interpolate_omp" .or. name == "optimize_omp" &
            .or. name == "transform_omp" &
            .or. name == "cosmology_omp" &
            .or. name == "prima_omp" .or. name == "sphere_omp" .or. name == "kde_serial" .or. name == "kde_omp" &
            .or. name == "stats" .or. name == "table_join_hash")
    end function suite_is_safe_to_parallelize

end module test_runner_support
