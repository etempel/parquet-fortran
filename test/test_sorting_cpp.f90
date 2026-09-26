!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The `parquet_sorting` tests that reach the C++ layer, split out of `test_sorting.f90`.
!!
!! **This is a partition by dependency, not by subject.** Both halves test the same module. What
!! separates them is that every test here either declares a `bind(C)` interface of its own (the
!! `parquet_debug_*` thread and comparison counters), drives the C++ sort engine through
!! `parquet_sorting_oracle`, or round-trips a file through a reader or writer. A test file
!! declaring any `bind(C)` interface cannot feed an undef-safe runner, and the rule is file-level
!! because a per-test rule is not statically decidable.
!!
!! The split was computed from the call graph rather than by eye: a test moved here if it, or
!! anything it calls, reaches one of those. It came out at 55 tests here against 93 that stay,
!! with **no helper needed by both halves** -- which is why neither file has to export anything to
!! the other.
!!
!! Registered as the suite `sorting_cpp` in `run_tester_cpp`; the Arrow-free remainder stays as
!! `sorting`. Both are excluded from test-drive's per-test parallelism, for the process-global
!! reasons `run_tester.f90`'s own exclusion list gives.
module test_sorting_cpp
    use parquet
    use test_sorting, only : itoa, negative_zero, radix_tag, str_prefix_column, ties_fixture, &
        engine_only_introsort, force_fortran_parallel_threshold
    ! The C++ sort engine is TEST-ONLY and is not re-exported by the `parquet` facade: reaching it
    ! needs this import, which is what keeps it out of every other program's dependency graph.
    ! Importing it is also what BINDS it -- `parquet_debug_use_fortran_sort_engine` lives here and
    ! registers the engine's entry points as a side effect of being called.
    use parquet_sorting_oracle, only : parquet_debug_use_fortran_sort_engine
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan, ieee_positive_inf, ieee_negative_inf
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    ! Used by every test that states its own precondition: a guard, or a threaded design, that is
    ! about teams has nothing to prove on a build or a machine that can never open one.
    ! `omp_get_num_procs` rather than `omp_get_max_threads` for the engine tests below, because that
    ! is the quantity the engine clamps an explicit `threads=` against (see
    ! `sort_build_permutation_threaded`, src/parquet_argsort_engine.f90): on a one-processor machine
    ! `threads=4` resolves to 1 and no threaded design is entered.
    use omp_lib, only : omp_get_max_threads, omp_in_parallel, omp_get_num_procs
#endif
    ! For the Stage 1 conformance oracle only: it builds a C++ key set of its own so that both
    ! engines can be asked about the same rows. These are ordinary library bindings, not debug
    ! hooks -- the two debug hooks are declared locally in `sweep_pairs`, per convention.
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_free, &
        parquet_sort_builder_add_key_int64, parquet_sort_builder_add_key_double, &
        parquet_sort_builder_add_key_string
    !
    implicit none
    private
    public :: collect_tests_sorting_cpp
    !
    ! ==================================================================================
    ! The shared sweep fixture
    ! ==================================================================================
    !
    !> Every "one call per specific" sweep below builds its values from these parameters, so an
    !> expectation is written once as a RANK and never as a per-type literal. That is what keeps a
    !> sixty-call sweep readable, and it keeps the oracle independent of the engine under test: the
    !> answer is read off `SWRANK`/`SWASC` by hand, not obtained from a second call into `pf_sort`.
    !>
    !> The order is scrambled deliberately. No element is already in place, the smallest and the
    !> largest both sit in the interior, and the ascending permutation is neither the identity nor a
    !> reversal -- so a specific wired to the wrong extractor, or one that transposes a value against
    !> an index, cannot pass by coincidence.
    integer, parameter :: SWN = 6
    !> `SWRANK(k)` is element k's 1-based position in ascending order; no two elements tie.
    integer, parameter :: SWRANK(SWN) = [3, 1, 5, 2, 6, 4]
    !> `SWASC(r)` is the element index holding rank r -- the permutation a full ascending sort gives.
    integer, parameter :: SWASC(SWN) = [2, 4, 1, 6, 3, 5]
    !> The `logical` fixture, the one type that cannot hold six distinct values: `.false.` at
    !> elements 2, 5 and 6, so a stable ascending sort must begin with exactly those three, in that
    !> order. Weaker than the others, which is why it is spelled out here rather than left to look
    !> equivalent.
    logical, parameter :: SWBOOL(SWN) = [.true., .false., .true., .true., .false., .false.]
    !> The DUPLICATE fixture, for the questions that only mean something with ties: six elements
    !> over three classes, each class appearing twice, neither contiguously nor in class order. A
    !> body that reported one entry per RUN rather than per VALUE would answer four rather than
    !> three, and one that lost the tie in ranking would never produce a repeated rank.
    integer, parameter :: SWDUP(SWN) = [3, 1, 3, 2, 1, 2]
    !> The SORTED duplicate fixture, for the search questions, which require sorted input: one
    !> element of class 1, two of class 2, three of class 3. Class 2 is what the sweeps look for --
    !> it is neither at the start nor at the end, and its run is neither of length one nor the
    !> longest, so its lower bound (2), upper bound (4) and equal range (2, 3) are four different
    !> numbers and no two of the three operations can be confused for each other.
    integer, parameter :: SWSRT(SWN) = [1, 2, 2, 3, 3, 3]
    !> The `logical` search fixture: the same shape as far as a two-valued type allows.
    logical, parameter :: SWSRTB(SWN) = [.false., .false., .false., .true., .true., .true.]
    !
contains

    !> Registers every test in this suite.
    subroutine collect_tests_sorting_cpp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("pf_argsort matches a read-time sort_by=", test_oracle_matches_read_time_sort), &
            new_unittest("the counting path matches the comparator", cpp_test_counting_path_agrees), &
            new_unittest("the counting path matches the comparator on a NULL-BEARING key", &
                cpp_test_counting_path_nulls_agree), &
            new_unittest("partial_sort really is partial", cpp_test_partial_is_partial), &
            new_unittest("unique agrees on both sort paths", cpp_test_unique_both_paths), &
            new_unittest("rank agrees on both sort paths", cpp_test_rank_both_paths), &
            new_unittest("threads are really created", cpp_test_threads_really_used), &
            new_unittest("auto is serial inside a parallel region", cpp_test_threads_auto_in_parallel), &
            new_unittest("threads=1 forces serial", cpp_test_threads_one_is_serial), &
            new_unittest("the final merge round is really co-ranked", cpp_test_merge_round_threads_used), &
            new_unittest("co-ranking equals the serial permutation at every size", &
                cpp_test_corank_size_sweep), &
            new_unittest("engine: integer keys match the C++ comparators", test_engine_conf_int), &
            new_unittest("engine: real keys with NaNs match the C++ comparators", test_engine_conf_real), &
            new_unittest("engine: string keys match the C++ comparators", test_engine_conf_str), &
            new_unittest("engine: variable-length strings compare like memcmp", test_engine_conf_varstr), &
            new_unittest("engine: multi-key and the nkeys prefix match the C++ comparators", &
                test_engine_conf_multi), &
            new_unittest("engine: the Fortran/C++ engine selector really switches engines", &
                test_fortran_engine_switches), &
            new_unittest("engine: the Fortran sort matches the C++ one on every family and size", &
                test_fortran_engine_ab_families), &
            new_unittest("engine: the Fortran engine matches C++ on every operation that is not a sort", &
                test_fortran_engine_ab_operations), &
            new_unittest("engine: the two engines agree on string, real and two-key rank queries", &
                test_engine_ab_string_real_ts), &
            new_unittest("engine: the Fortran sort matches on degenerate input shapes", &
                test_fortran_engine_adversarial), &
            new_unittest("engine: the radix path matches C++ on the values a key transform can lose", &
                test_radix_path_value_shapes), &
            new_unittest("engine: the radix path matches C++ on strings that outrun its prefix", &
                test_radix_path_string_shapes), &
            new_unittest("engine: the radix path falls back rather than aborting when it cannot allocate", &
                test_radix_path_alloc_fallback), &
            new_unittest("engine: the radix path's deep string refine matches C++ past its window", &
                test_radix_path_deep_strings), &
            new_unittest("engine: the multi-key radix matches C++ and declines a string key", &
                test_radix_path_multi_key), &
            new_unittest("engine: a narrow integer key is biased, and a wide one is not", &
                test_radix_path_narrow_integer), &
            new_unittest("engine: the forced heapsort fallback matches the C++ sort", &
                test_fortran_engine_heapsort_fallback), &
            new_unittest("engine: the counting path agrees with the comparator path and with C++", &
                test_counting_path_matches_comparator), &
            new_unittest("engine: the counting path's bucket limit declines a wide range", &
                test_counting_path_bucket_limit), &
            new_unittest("engine: the counting range check survives int64 extremes", &
                test_counting_path_int64_extremes), &
            new_unittest("engine: the multi-key chain threads a STRING key's radix", &
                test_multi_string_threads), &
            new_unittest("engine: the string refine threads, at both of its two levels", &
                test_refine_threads), &
            new_unittest("engine: the two engines agree on the runs path with BOTH threaded", &
                test_runs_threaded_conformance), &
            new_unittest("engine: the tie pass and the offsets pass run on the team and give the serial answer", &
                test_runs_tail_passes_threads) &
            ]
    end subroutine collect_tests_sorting_cpp
    !
    !> The tie pass and the offsets pass of the grouped path run on the team `threads=` asked for,
    !! give the serial answer, and agree with the C++ engine's serial walk of the same flags.
    !!
    !! **Both passes are invisible in any answer**: the flags and the offsets are pure functions of
    !! the permutation, identical at every team size, so `parquet_debug_sort_tie_threads_used` and
    !! `parquet_debug_sort_offsets_threads_used` are the only observations that the resolved count
    !! reached them -- feature_risks.md Risk-189: the count was resolved and dropped on this very
    !! path once, and the two passes are handed it separately, hence two records. Four arms: the
    !! Fortran engine at `threads=4` records 4 in both; at `threads=1` it records 1 in both -- the
    !! control, without which a pass that always opened the machine would pass the first; the
    !! offsets of the two agree element for element, which is the serial-against-threaded A/B of
    !! `runs_to_offsets` and, through them, of the tie pass; and the C++ engine at `threads=4`,
    !! whose own tie walk is serial, gives the same offsets again -- the cross-engine A/B of the
    !! threaded tie pass. Ties are dense on purpose: run detection over distinct values detects
    !! nothing. The tail floor is lowered so a 4096-element fixture opens the team at all, and
    !! restored with the engine floors before the first assertion.
    subroutine test_runs_tail_passes_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64          !! rows; 97 distinct values, dense ties.
        real(real64) :: v(n)
        integer(int64), allocatable :: perm(:), go4(:), go1(:), goc(:)
        integer(int64) :: tie4, off4, tie1, off1, i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: neither pass can open a team, so every record would " // &
            "read 1 and the equalities would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is " // &
                "clamped to omp_get_num_procs(), so both passes would run serially")
            return
        end if
#endif
        do i = 1_int64, n
            v(i) = real(mod(i * 7919_int64, 97_int64), real64) * 0.5_real64
        end do
        ! Lowered around every arm and restored BEFORE the first assertion: every `check` can
        ! return early, and a leaked floor would rethread every later test in this suite.
        call force_parallel_threshold(4_int64)
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call restore_engine_default()
        call pf_argsort(v, perm, group_offsets=go4, threads=4)
        tie4 = parquet_debug_sort_tie_threads_used()
        off4 = parquet_debug_sort_offsets_threads_used()
        call pf_argsort(v, perm, group_offsets=go1, threads=1)
        tie1 = parquet_debug_sort_tie_threads_used()
        off1 = parquet_debug_sort_offsets_threads_used()
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_argsort(v, perm, group_offsets=goc, threads=4)
        call restore_engine_default()
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        call force_parallel_threshold(0_int64)
        !
        call check(error, tie4 == 4_int64, &
            "the tie pass must run on the team threads= asked for: sort_build_runs_permutation " // &
            "is handed the resolved count and has to open it past the tail floor")
        if (allocated(error)) return
        call check(error, off4 == 4_int64, &
            "runs_to_offsets must run on the team: drive_engine_grouped has to hand it the count " // &
            "engine_build_runs resolved, not leave it to run serially")
        if (allocated(error)) return
        ! The control: without it a pass that ignored threads= and always opened the machine
        ! would pass both lines above.
        call check(error, tie1 == 1_int64 .and. off1 == 1_int64, &
            "threads=1 must run the tie pass and the offsets pass serially")
        if (allocated(error)) return
        call check(error, size(go4) == size(go1), &
            "the threaded and the serial offsets must describe the same number of groups")
        if (allocated(error)) return
        call check(error, all(go4 == go1), &
            "the threaded offsets must equal the serial ones element for element: the chunked " // &
            "count, prefix and fill of runs_to_offsets, and the threaded tie flags they read")
        if (allocated(error)) return
        call check(error, size(goc) == size(go4), &
            "the C++ engine's serial tie walk must find the same number of groups as the " // &
            "threaded Fortran tie pass")
        if (allocated(error)) return
        call check(error, all(goc == go4), &
            "the C++ engine's serial tie walk must give the offsets the threaded Fortran tie " // &
            "pass gives, element for element -- the cross-engine A/B the pass rests on")
    end subroutine test_runs_tail_passes_threads
    !
    !> The grouped (runs) path must give the same answer on both engines when both really thread.
    !!
    !! **This exists because the other runs conformance arm cannot cover it.** The engine A/B in
    !! `test_engine_conformance` sorts a 60-element fixture with no `threads=` and no floor
    !! override, and both engines refuse a team far below their own floors (32768 rows in Fortran,
    !! 8192 in C++) -- so it compares serial against serial. That is a perfectly good correctness
    !! test and no evidence at all about threading, and it was being cited as evidence about
    !! threading while `engine_build_runs` discarded its resolved thread count on the Fortran
    !! branch. See `feature_risks.md` Risk-189.
    !!
    !! **What makes it non-vacuous is asserting BOTH counters**, which are two different counters:
    !! `threads_used()` reads the C++ engine's and `parquet_debug_sort_threads_used()` the
    !! Fortran one. Without both, a run in which either engine quietly declined would still pass
    !! the equality below -- the vacuous A/B shape `feature_risks.md` Risk-49 describes.
    !!
    !! The oracle is the usual one: every comparator ends in a row-index tiebreaker, so exactly one
    !! permutation is correct and a disagreement is a defect rather than a variation.
    !!
    !! **Since the Fortran tie pass threads, this A/B is the one the two engines' structural
    !! identity was replaced by.** The C++ engine still walks its flags serially after its
    !! threaded build; the Fortran engine flags its runs on the team. The tail floor is lowered
    !! here so a 2000-element fixture reaches that team, and the Fortran tie record is asserted as
    !! a third precondition -- without it a tie pass that quietly declined would leave this
    !! comparing two serial walks again, exactly the vacuous shape above.
    subroutine test_runs_threaded_conformance(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        real(real64) :: v(2000)                             !! ties are dense; runs are the point.
        integer(int64) :: cpp_team, ftn_team                !! each engine's own team counter.
        integer(int64) :: ftn_tie                           !! the Fortran tie pass's own record.
        real(real64), allocatable :: cd(:), fd(:)           !! per-engine distinct values.
        integer, allocatable :: cr(:), fr(:)                !! per-engine ranks.
        integer :: cc, fc                                   !! per-engine distinct counts.
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: neither engine can open a team, so both arms would " // &
            "run the same serial code and the equality would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is " // &
                "clamped to omp_get_num_procs(), so neither engine would thread")
            return
        end if
#endif
        call ties_fixture(v)
        ! Lowered around BOTH arms and restored before the first assertion -- every `check` can
        ! return early, and a leaked floor would rethread every later test in this suite.
        call force_parallel_threshold(4_int64)
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_unique_count(v, cc, threads=4)
        call pf_unique(v, cd, threads=4)
        call pf_rank(v, cr, threads=4)
        cpp_team = threads_used()
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_unique_count(v, fc, threads=4)
        call pf_unique(v, fd, threads=4)
        call pf_rank(v, fr, threads=4)
        ftn_team = parquet_debug_sort_threads_used()
        ftn_tie = parquet_debug_sort_tie_threads_used()
        call restore_engine_default()
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        call force_parallel_threshold(0_int64)
        !
        call check(error, cpp_team == 4_int64, &
            "precondition: the C++ engine must really thread the runs path, or this compares " // &
            "serial against serial")
        if (allocated(error)) return
        call check(error, ftn_team == 4_int64, &
            "precondition: the Fortran engine must really thread the runs path -- it resolved a " // &
            "thread count and dropped it for as long as the grouped path existed")
        if (allocated(error)) return
        call check(error, ftn_tie == 4_int64, &
            "precondition: the Fortran engine's tie pass must run on the team -- it is what this " // &
            "A/B compares against the C++ engine's serial walk of the same flags")
        if (allocated(error)) return
        call check(error, cc == fc, "unique_count: the two threaded engines disagreed")
        if (allocated(error)) return
        call check(error, size(cd) == size(fd) .and. all(cd == fd), &
            "unique: the two threaded engines returned different distinct values")
        if (allocated(error)) return
        call check(error, all(cr == fr), "rank: the two threaded engines returned different ranks")
    end subroutine test_runs_threaded_conformance

    !
    !> The multi-key chain must run a STRING key's radix on the team, not serially.
    !!
    !! **Why this cannot be an answer test.** `sort_radix_lsd_chain`'s serial arms produce exactly the
    !! permutation its threaded arm does -- that is what makes the serial fallback safe -- so every
    !! correctness test in this file passes whether or not the string key ever reaches a thread. The
    !! string pass was in fact serial for the whole of the parallel-sort campaign and nothing failed:
    !! `multi3` scaled 1.49x against `multi2`'s 3.01x, and only a benchmark could see it.
    !!
    !! **Both keys are strings on purpose.** `dbg_sort_design` is set to 1 in exactly two places --
    !! `sort_radix_design_a`, which only the SINGLE-key path reaches, and the threaded arm of
    !! `sort_radix_lsd_chain`. So for a multi-key sort whose every key is a string, `design == 1` can
    !! only mean the string pass itself threaded. Add a numeric key and the assertion goes vacuous,
    !! because that key's own pass would set it.
    subroutine test_multi_string_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: n = 40000_int64
        character(len=8) :: a(n), b(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: ref(:), got(:)
        integer(int64) :: d_serial, d_threaded, i
        ! **Preconditions, declared rather than assumed.** The assertion is that a multi-key STRING radix runs ON THE
        ! TEAM. Design 1 is also what the shared serial chain produces, so without a team the check
        ! cannot tell the two apart -- it would pass for the wrong reason.
        ! Where no team can be opened the assertions are not merely untestable but VACUOUS:
        ! they would pass just as happily against a library that had stopped threading
        ! altogether. Skipping says so out loud, which a silent pass would not. Same reasoning
        ! and same shape as `test_nested_team_guard`.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded sort designs are " // &
            "preprocessed out entirely -- the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP -- so no team is " // &
            "ever opened and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! 40000 rows clears the engine's own threading floor (max(32768, 2048*nt)) at four threads.
        ! The two keys tie often enough that the second one really is consulted.
        do i = 1_int64, n
            write (a(i), '(a,i5.5)') "k", mod(i, 400_int64)
            write (b(i), '(a,i5.5)') "z", mod(i * 7_int64, 9973_int64)
        end do
        call parquet_debug_use_fortran_sort_engine(.true.)
        call keys%clear()
        call keys%add(a)
        call keys%add(b)
        !
        call pf_argsort(keys, ref, threads=1)
        d_serial = parquet_debug_sort_design()
        call pf_argsort(keys, got, threads=4)
        d_threaded = parquet_debug_sort_design()
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        call check(error, d_threaded == 1_int64, &
            "a multi-key STRING radix must run on the team: design 1 comes only from the shared chain")
        if (allocated(error)) return
        call check(error, d_serial == 0_int64, &
            "at one thread the chain must not report a threaded design, or the check above is vacuous")
        if (allocated(error)) return
        call check(error, size(got) == size(ref) .and. all(got == ref), &
            "threading a string key must give the serial permutation exactly")
    end subroutine test_multi_string_threads

    !
    !> The string refine must run on the team, at whichever of its two levels has the work.
    !!
    !! **Nothing else can see this.** `sort_radix_refine_run`'s serial and threaded arms produce
    !! byte-identical permutations, so every correctness test in this file passes whether or not the
    !! refine ever reaches a thread -- and it did not, for the whole parallel-sort campaign. A
    !! 5e6-row column whose values shared a leading prefix scaled **1.06x from 1 to 64 threads**
    !! before this, against 2.94x for the same column with no shared prefix.
    !!
    !! **Both levels are asserted because they are reached by opposite fixtures.** A prefix SHORTER
    !! than the radix window still separates rows into many small runs, so the parallelism is over
    !! runs; a prefix that covers the whole window collapses the column into ONE run, the run-level
    !! loop has nothing to spread, and the only parallelism left is that run's own 256-way
    !! sub-bucket loop. A test using either fixture alone would leave the other level unguarded.
    subroutine test_refine_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: n = 200000_int64
        character(len=12) :: many(n), one(n)
        integer(int64), allocatable :: ref(:), got(:)
        integer(int64) :: d_many, d_one, d_serial, i, q
        ! **Preconditions, declared rather than assumed.** The assertion is that the run-level and the within-run loops
        ! both thread; neither loop exists without a team.
        ! Where no team can be opened the assertions are not merely untestable but VACUOUS:
        ! they would pass just as happily against a library that had stopped threading
        ! altogether. Skipping says so out loud, which a silent pass would not. Same reasoning
        ! and same shape as `test_nested_team_guard`.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded sort designs are " // &
            "preprocessed out entirely -- the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP -- so no team is " // &
            "ever opened and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! `many`: 4 shared leading characters, so the 8-byte radix window still splits the column
        ! into thousands of small runs. `one`: 8 shared, covering the whole window, so every row
        ! lands in a single run and only the sub-bucket loop can thread.
        do i = 1_int64, n
            q = mod(i * 2654435761_int64, 100000_int64)
            write (many(i), '(a,i7.7)') "aaaa", q
            write (one(i), '(a,i4.4)') "aaaaaaaa", mod(q, 10000_int64)
        end do
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call force_parallel_threshold(1_int64)
        call pf_argsort(many, ref, threads=1)
        d_serial = parquet_debug_sort_refine_runs()
        call pf_argsort(many, got, threads=4)
        d_many = parquet_debug_sort_refine_runs()
        !
        call check(error, all(got == ref), "threading the refine must not change the answer")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            call parquet_debug_use_fortran_sort_engine(.false.)
            return
        end if
        call pf_argsort(one, ref, threads=1)
        call pf_argsort(one, got, threads=4)
        d_one = parquet_debug_sort_refine_runs()
        !
        call force_parallel_threshold(0_int64)
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        call check(error, d_serial == 0_int64, &
            "at one thread the refine must dispatch nothing, or the checks below are vacuous")
        if (allocated(error)) return
        call check(error, d_many > 0_int64, &
            "many small runs must be refined by the team: the run-level loop did not thread")
        if (allocated(error)) return
        call check(error, d_one > 0_int64, &
            "one giant run must thread its sub-bucket loop: run-level threading cannot help there")
        if (allocated(error)) return
        call check(error, all(got == ref), &
            "threading a single giant run's sub-buckets must not change the answer")
    end subroutine test_refine_threads

    !
    !> **The test that justifies sharing an engine.** The same values are ordered two ways -- by
    !> the reader's `sort_by=` on the way out of a file, and by `pf_argsort` in memory -- and the
    !> two must agree row for row. If either path ever grew its own comparator, null placement,
    !> NaN placement or tie order could drift apart and no other test in either suite would
    !> notice, because each one only ever checks its own side.
    subroutine test_oracle_matches_read_time_sort(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: file = "test_run/pf_sorting_oracle.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(8) = [1, 2, 3, 4, 5, 6, 7, 8]
        real(real64) :: v(8) = [3.5_real64, -1.0_real64, 3.5_real64, 9.25_real64, &
            0.0_real64, -7.5_real64, 2.0_real64, 3.5_real64]
        integer(int32), allocatable :: from_reader(:), perm(:)
        integer(int64) :: nrows
        integer :: k

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call srt%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        allocate(from_reader(nrows))
        call parquet_read_column(reader, "id", from_reader)
        call parquet_close_reader(reader)

        call pf_argsort(v, perm)
        call check(error, size(from_reader) == 8, "the fixture must read back eight rows")
        if (allocated(error)) return
        ! `id(k) == k`, so the reader's id column IS the permutation it applied.
        call check(error, all(from_reader == perm), &
            "pf_argsort must produce exactly the row order a read-time sort_by= produces")
        if (allocated(error)) return
        ! The three tied 3.5 values are what make this more than a smoke test: both paths must
        ! break the tie the same way (original file order), not merely sort the distinct values.
        call check(error, all([(v(perm(k)), k = 1, 8)] == [-7.5_real64, -1.0_real64, 0.0_real64, &
            2.0_real64, 3.5_real64, 3.5_real64, 3.5_real64, 9.25_real64]), &
            "gathering by the permutation must give the values in ascending order")
    end subroutine test_oracle_matches_read_time_sort

    !
    !> The integer counting fast path is a SECOND code path producing the same answer, so it is
    !> compared against the comparator path rather than trusted. Low-cardinality integers are what
    !> select it; the debug hook forces the comparator path for the same input.
    !>
    !> The hook is process-global, which is why this suite is excluded from test/run_tester.f90's
    !> per-test parallelism.
    subroutine test_counting_path_agrees(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(40)
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)   ! only five distinct values: the counting path's case
        end do
        call arm_sort_comparisons()
        call pf_argsort(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_argsort(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            "the counting fast path must produce exactly the comparator path's permutation")
        if (allocated(error)) return
        call check(error, all([(v(fast(k)), k = 1, 40)] == [(v(slow(k)), k = 1, 40)]), &
            "both paths must gather the same values in the same order")
    end subroutine test_counting_path_agrees

    !
    !> The counting fast path with NULLS, which it used to decline outright.
    !!
    !! Declining cost the whole fast path to a single null anywhere in the column -- a step
    !! function of WHETHER a null exists, not how many, measured as a 3.1x end-to-end loss on a
    !! 4M-row column at 0.1% null density. Nulls are a TIER in this engine, never a value, so they
    !! form one contiguous block the permutation places directly.
    !!
    !! **Every case sweeps `descending` x `nulls_first`, and that is the point rather than
    !! thoroughness for its own sake.** The null block's position must depend on `nulls_first` and
    !! must NOT depend on `descending` -- Arrow's rule, and the one property a partition-then-count
    !! implementation gets wrong by default. An ascending nulls-last test cannot see either.
    !!
    !! Each case asserts BOTH halves: that the two engines really diverged (zero comparisons on the
    !! fast half, nonzero on the slow one) and that they agree. Without the first, a fast path that
    !! silently declined would pass the equality against itself -- feature_risks.md Risk-35/Risk-52.
    subroutine test_counting_path_nulls_agree(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(40)
        logical :: ok(40), none_null(40), all_null(40)
        integer :: k

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)     ! five distinct values: the counting path's case
            ok(k) = mod(k, 7) /= 0               ! a scattered handful of nulls
        end do
        none_null = .true.
        all_null = .false.
        !
        call one_null_case(error, v, ok, .false., .false., "asc/nulls-last")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .false., "desc/nulls-last")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .false., .true., "asc/nulls-first")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .true., "desc/nulls-first")
        if (allocated(error)) return
        ! The two ends. All-null has no value range at all, so the candidate has nothing to bound
        ! and must still answer file order; no-null must reach the same result as before the mask
        ! argument existed.
        call one_null_case(error, v, all_null, .false., .false., "all-null asc")
        if (allocated(error)) return
        call one_null_case(error, v, all_null, .true., .true., "all-null desc/nulls-first")
        if (allocated(error)) return
        call one_null_case(error, v, none_null, .false., .false., "no-null asc")
        if (allocated(error)) return
        call one_null_case(error, v, none_null, .true., .false., "no-null desc")
        if (allocated(error)) return
        !
        ! A NULL ROW'S KEY SLOT IS NOT A VALUE, and this is the case that proves the candidate
        ! knows it. Arrow promises nothing about the bytes behind a null, so a null row can carry
        ! anything -- here a value four orders of magnitude outside the valid rows' 0..4 range, and
        ! far past the bucket limit. Counting the null rows into the range scan would size the
        ! bucket domain from that value, blow the limit, and silently decline the fast path; the
        ! `cmp_fast == 0` assertion in one_null_case is what catches it. Every other fixture in
        ! this test holds an in-range value behind its nulls, so none of them can see this.
        do k = 1, 40
            if (mod(k, 7) == 0) v(k) = 2000000000_int32
        end do
        call one_null_case(error, v, ok, .false., .false., "null slot holds a huge value")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .true., "huge null slot, desc/nulls-first")
    end subroutine test_counting_path_nulls_agree

    !
    !> One (descending, nulls_first) case of the test above: both engines, both halves asserted.
    subroutine one_null_case(error, v, ok, desc, nfirst, label)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), intent(in) :: v(:)      !! key values.
        logical, intent(in) :: ok(:)            !! validity mask; .false. marks a null.
        logical, intent(in) :: desc             !! sort direction.
        logical, intent(in) :: nfirst           !! null placement.
        character(len=*), intent(in) :: label   !! names the case in a failure message.
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k, n

        n = size(v)
        call arm_sort_comparisons()
        call pf_argsort(v, fast, is_valid=ok, descending=desc, nulls_first=nfirst)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_argsort(v, slow, is_valid=ok, descending=desc, nulls_first=nfirst)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        !
        call check(error, cmp_fast == 0_int64, &
            label // ": the fast half must reach the COUNTING path (zero comparisons); a nonzero " // &
            "count means the candidate declined and the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, cmp_slow > 0_int64, &
            label // ": the slow half must reach the COMPARATOR path")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            label // ": the counting path must produce exactly the comparator path's permutation")
        if (allocated(error)) return
        ! Not implied by the permutation check: it pins WHERE the nulls landed, which is the
        ! property `descending` must not disturb.
        call check(error, all([(ok(fast(k)), k = 1, n)] .eqv. [(ok(slow(k)), k = 1, n)]), &
            label // ": the null block must land in the same place under both engines")
    end subroutine one_null_case

    !
    !> **A partial sort that is not actually partial is invisible to every test above.** Returning
    !> the first n of a FULL sort is correct and merely slower, so only a comparison count
    !> distinguishes them -- a wall-clock benchmark would be flaky and needs warm-up.
    !>
    !> Uses a REAL key on purpose: the integer counting fast path performs zero comparisons, so a
    !> low-cardinality integer key would report 0 on both paths and the test would pass vacuously.
    subroutine test_partial_is_partial(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(4000)
        real(real64), allocatable :: out(:)
        integer(int64) :: n_partial, n_full
        integer :: k
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(n)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: n !! comparisons since the counter was armed.
            end function got_cmp
        end interface

        do k = 1, 4000
            v(k) = real(mod(k * 7919, 4001), real64) * 0.5_real64
        end do
        call count_cmp(1)
        call pf_partial_sort(v, out, 10)
        n_partial = got_cmp()
        call count_cmp(1)
        call pf_sort(v, out)
        n_full = got_cmp()
        call count_cmp(0)
        call check(error, n_partial > 0 .and. n_full > 0, &
            "both sorts must reach the comparator path for this comparison to mean anything")
        if (allocated(error)) return
        call check(error, n_partial < n_full, &
            "a partial sort of 10 of 4000 must do fewer comparisons than a full sort")
    end subroutine test_partial_is_partial

    !
    !> **Risk-35 discipline.** A low-cardinality integer array takes the counting fast path, which
    !> performs zero comparisons -- so a test that only ever runs it proves nothing about the
    !> comparator's run detection. Both paths are forced and required to agree.
    subroutine test_unique_both_paths(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(60)
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k, c_fast, c_slow

        do k = 1, 60
            v(k) = int(mod(k * 7, 6), int32)   ! six distinct values: the counting path's case
        end do
        call arm_sort_comparisons()
        call pf_unique_count(v, c_fast)
        call pf_unique(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_unique_count(v, c_slow)
        call pf_unique(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, c_fast == 6 .and. c_slow == 6, &
            "both sort paths must find the same six distinct values")
        if (allocated(error)) return
        call check(error, size(fast) == size(slow), "both paths must return the same number of values")
        if (allocated(error)) return
        call check(error, all(fast == slow), "both paths must return the same distinct values")
    end subroutine test_unique_both_paths

    !
    !> **Risk-35 discipline**, for the same reason as `test_unique_both_paths`: ranking turns on
    !> run detection, and a counting-path fixture never invokes the comparator that finds the runs.
    subroutine test_rank_both_paths(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(50)
        integer, allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k

        do k = 1, 50
            v(k) = int(mod(k * 3, 4), int32)   ! four distinct values, heavily tied
        end do
        call arm_sort_comparisons()
        call pf_rank(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_rank(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            "both sort paths must produce the same competition ranks")
        if (allocated(error)) return
        call check(error, maxval(fast) == 50 - count(v == maxval(v)) + 1, &
            "the largest competition rank must be one past the count below the top run")
    end subroutine test_rank_both_paths

    !
    ! ==================================================================================
    ! M4: parallel sorting
    ! ==================================================================================
    !
    !> Arms and zeroes the sort's comparison counter.
    !>
    !> **This is what makes every counting-path A/B test in this file non-vacuous.** Those tests run
    !> one fixture down both sort engines and assert the two agree -- but "both engines" is a claim
    !> about which code ran, and nothing in an equality assertion can see it. Turn the setting the
    !> wrong way round, or stop it reaching C++, and both halves take the SAME path: the comparison
    !> holds trivially and the test passes while testing nothing (feature_risks.md Risk-35).
    !>
    !> The counting path performs exactly zero comparisons by construction, so `0` on one half and
    !> nonzero on the other proves the two halves really diverged.
    subroutine arm_sort_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine arm_sort_comparisons


    !> Comparisons counted since the last arm_sort_comparisons, then disarms the counter.
    integer(int64) function sort_comparisons() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since the counter was armed.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function sort_comparisons


    !> Lowers the row count below which threading is refused, so a test-sized array can reach the
    !> parallel path at all. Every fixture here is orders of magnitude below the real threshold.
    subroutine force_parallel_threshold(rows)
        integer(int64), intent(in) :: rows !! new threshold; <= 0 restores the built-in ones.

        ! **Three floors, because one setting used to be three**, and this file is the only place
        ! all three matter: its tests run BOTH engines, so the C++ floor has to move with the two
        ! Fortran ones. The Fortran half is `force_fortran_parallel_threshold` in `test_sorting`,
        ! shared rather than duplicated -- the tests that never switch engines need only that half
        ! and now live there.
        block
            use iso_c_binding, only : c_int64_t
            interface
                subroutine set_cpp_min(n) bind(C, name="parquet_debug_set_sort_parallel_min_rows")
                    use iso_c_binding, only : c_int64_t
                    integer(c_int64_t), value :: n !! rows; <= 0 restores the built-in floor.
                end subroutine set_cpp_min
            end interface
            call set_cpp_min(int(rows, c_int64_t))
        end block
        call force_fortran_parallel_threshold(rows)
    end subroutine force_parallel_threshold

    !
    !> How many threads the last sort actually put to work, the calling thread included.
    function threads_used() result(n)
        integer(int64) :: n !! 1 means the sort ran serially.
        interface
            function get_used() bind(C, name="parquet_debug_get_sort_threads_used") result(k)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: k !! threads used by the last threaded build.
            end function get_used
        end interface
        n = int(get_used(), int64)
    end function threads_used

    !
    !> **Without this the whole feature is untestable.** A `threads=` that is silently ignored
    !> returns the serial permutation, which is CORRECT -- so every assertion above passes just as
    !> happily against an implementation that never spawns anything. Zero parallelism is a passing
    !> test, exactly as zero comparisons was for the partial sort (`feature_risks.md` Risk-35).
    subroutine test_threads_really_used(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:)

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 4_int64, &
            "asking for 4 threads must actually put 4 threads to work")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! The negative control: below the threshold nothing threads, however many were asked for.
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 1_int64, &
            "an array below the minimum-work threshold must sort serially whatever was asked for")
        call force_parallel_threshold(0_int64)
    end subroutine test_threads_really_used

    !
    subroutine test_threads_auto_in_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:)
        integer(int64) :: auto_outside, auto_inside, explicit_inside
        logical :: active_inside

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm)
        auto_outside = threads_used()
        auto_inside = -1_int64
        explicit_inside = -1_int64
        active_inside = .false.
        !$omp parallel
        !$omp single
        block
            integer(int32), allocatable :: p2(:)
            call pf_argsort(v, p2)
            auto_inside = threads_used()
            call pf_argsort(v, p2, threads=3)
            explicit_inside = threads_used()
#ifdef _OPENMP
            ! Whether this region is ACTIVE decides which contract applies below. It is not a
            ! constant: `!$omp parallel` yields a team of one under `OMP_NUM_THREADS=1`, and an
            ! inactive region is where an explicit `threads=` is clamped rather than honoured.
            active_inside = omp_in_parallel()
#endif
        end block
        !$omp end single
        !$omp end parallel
        call force_parallel_threshold(0_int64)
        call check(error, auto_inside == 1_int64, &
            "auto must resolve to serial inside an OpenMP parallel region")
        if (allocated(error)) return
#ifdef _OPENMP
        if (active_inside) then
            call check(error, explicit_inside == 3_int64, &
                "an explicit threads= must still be honoured inside an ACTIVE parallel region")
        else
            ! A team of one: the region exists but runs serially, which is the shape that deadlocks
            ! libgomp when a team is opened inside it. See feature_risks.md Risk-104.
            call check(error, explicit_inside == 1_int64, &
                "inside a region running on a single thread an explicit threads= must clamp to serial")
        end if
#else
        call check(error, explicit_inside == 3_int64, &
            "without OpenMP no region exists at all, so an explicit threads= is honoured unconditionally")
#endif
        if (allocated(error)) return
        ! Guards the test itself: if auto were serial everywhere, the assertion above would pass
        ! while proving nothing about the parallel-region rule.
        call check(error, auto_outside >= 1_int64, "auto outside a parallel region must resolve")
    end subroutine test_threads_auto_in_parallel

    !
    !> `threads=1` is the documented way to turn parallelism off, now that absence means auto.
    subroutine test_threads_one_is_serial(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:), ref(:)

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=1)
        call check(error, threads_used() == 1_int64, "threads=1 must sort serially")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        call pf_argsort(v, ref, threads=8)
        call force_parallel_threshold(0_int64)
        call check(error, all(perm == ref), "threads=1 and threads=8 must agree element for element")
    end subroutine test_threads_one_is_serial

    !
    !> Shrinks the smallest output range the co-ranked merge gives its own thread, so a test-sized
    !! array reaches the co-rank at all; 0 restores the real floor.
    !!
    !! **Every test below that sorts fewer than ~32000 elements is worthless without this**, and
    !! silently so. The real floor is 16384, and a pair shorter than twice that is merged in one
    !! piece — which is exactly the old, correct, single-threaded merge. So a dense sweep over small
    !! arrays exercises the path this feature *replaced*, calls the co-rank zero times, and passes.
    !! Found by mutation, not by reasoning: two deliberate co-rank defects survived the entire suite
    !! until the sweeps started calling this. `feature_risks.md` Risk-49.
    subroutine force_merge_segments(min_segment)
        integer(int64), intent(in) :: min_segment !! new floor in elements; 0 restores the built-in one.
        interface
            subroutine set_min_seg(n) bind(C, name="parquet_debug_set_sort_merge_min_segment")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! elements; <= 0 restores the real floor.
            end subroutine set_min_seg
        end interface
        call set_min_seg(int(min_segment, int64))
    end subroutine force_merge_segments

    !
    !> How many threads worked the last sort's FINAL merge round, the calling thread included.
    function merge_threads_used() result(n)
        integer(int64) :: n !! 1 means that round ran on one thread.
        interface
            function get_merge_used() bind(C, name="parquet_debug_get_sort_merge_threads_used") result(k)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: k !! threads that worked the final merge round.
            end function get_merge_used
        end interface
        n = int(get_merge_used(), int64)
    end function merge_threads_used

    !
    !> **Without this the co-ranked merge is untestable, exactly as `threads=` itself was.** A merge
    !! that quietly stopped splitting its final round would still return the identical permutation --
    !! that identity is what makes every thread count safe -- so every assertion above passes just as
    !! happily against the old single-threaded tail. `feature_risks.md` Risk-49.
    !!
    !! The final round is asked about specifically, not the maximum over rounds: a merge that
    !! co-ranked only its first round would report a high maximum while leaving the whole O(n) tail
    !! in place, which is the thing this work exists to remove.
    !!
    !! `merge_threads_used` is a different counter from `threads_used`, which still means phase 1's
    !! chunk-sort thread count. Both are asserted here, so a future change that collapsed them into
    !! one would fail rather than silently answer the wrong question.
    subroutine test_merge_round_threads_used(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 200000
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: perm(:)

        allocate(v(n))
        call ties_fixture(v)
        ! Segments have a minimum size, so the array has to be big enough for the final round to be
        ! worth splitting at all -- a 2000-element fixture would legitimately report one thread.
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 4_int64, "phase 1 must still put 4 threads to work")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        call check(error, merge_threads_used() > 1_int64, &
            "the final merge round must be co-ranked across more than one thread")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! Two negative controls, because a hook that always answered "4" would pass the assertion
        ! above. Below the minimum-work threshold nothing threads at all...
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, merge_threads_used() == 1_int64, &
            "an array below the minimum-work threshold must report no co-ranked merge")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! ...and threads=1 is serial however large the array is.
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=1)
        call check(error, merge_threads_used() == 1_int64, &
            "threads=1 must report no co-ranked merge")
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_round_threads_used

    !
    ! ============================================================================================
    ! Stage 1 conformance oracle
    ! ============================================================================================
    !
    ! The Fortran comparator core must answer EXACTLY as the C++ one does, because after the Stage 6
    ! cutover the two decide the order of the same data by different routes. Asserting that a sort
    ! comes out sorted does not test this: both engines sort correctly and can still disagree about
    ! which of two EQUAL rows comes first, which is the maintainer's stated requirement ("the two
    ! sorting algorithms provide the same ordering, even for duplicated values").
    !
    ! So these tests compare ANSWERS, not orderings, over every ordered pair of a small tie-rich
    ! fixture. Every fixture is deliberately duplicate-heavy: a random real64 column of any size
    ! contains essentially no ties at all, which is exactly why the motivating benchmark
    ! could not have caught a tie-order defect.
    !
    ! Both engines are fed from ONE source in each test -- the same Fortran arrays go into a
    ! `pf_sort_keys` and into a C++ builder -- so a disagreement is a comparator disagreement and
    ! not a data one.
    !
    !> Asks BOTH engines about every ordered pair, and checks the total-order properties.
    !!
    !! The C++ side is reached through four test-only hooks in src/parquet_wrapper.cpp, declared
    !! locally here because that is how every `parquet_debug_*` hook is reached and it keeps them
    !! out of src/parquet_bindings.f90. The per-pair pair take rows 0-BASED (the C++ internal
    !! convention), so each index is passed as `i - 1`; the Fortran side is 1-based throughout.
    !! The other two are the BATCHED sweeps, whose checksums are asserted beside their Fortran
    !! twins' -- nothing in the library calls either, so without this they have no caller a test
    !! run reaches, and the benchmark that does call them would compare two meaningless numbers.
    !!
    !! The three property checks are what the C++ side cannot be asked about, and they are the
    !! reason this is not merely a two-way diff: irreflexivity, antisymmetry, and that `less` agrees
    !! with the sign of the tie-free answer or -- on a full tie -- falls back to row order, which is
    !! the index tiebreaker doing its job.
    subroutine sweep_pairs(error, label, keys, builder, n, nkeys, ntotal)
        type(error_type), allocatable, intent(inout) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: label                 !! names the fixture in every message.
        type(pf_sort_keys), intent(in) :: keys                !! the Fortran key set.
        type(c_ptr), intent(in) :: builder                    !! the C++ builder over the same data.
        integer(int64), intent(in) :: n                       !! rows.
        integer, intent(in) :: nkeys                          !! prefix for the tie-free comparator.
        integer, intent(in) :: ntotal                         !! engine keys the set actually holds.
        interface
            function c_dbg_row_less(handle, a, b) bind(C, name="parquet_debug_sort_row_less") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: a       !! first row, 0-based.
                integer(c_long_long), value :: b       !! second row, 0-based.
                integer(c_long_long) :: r              !! 1 = less, 0 = not, -1 = no key added.
            end function c_dbg_row_less
            function c_dbg_keys_compare(handle, a, b, nk) &
                    bind(C, name="parquet_debug_sort_keys_compare") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: a       !! first row, 0-based.
                integer(c_long_long), value :: b       !! second row, 0-based.
                integer(c_long_long), value :: nk      !! leading keys taking part.
                integer(c_long_long) :: r              !! -1/0/+1, or -2 when no key was added.
            end function c_dbg_keys_compare
            function c_sweep_less(handle, nrows, nreps) &
                    bind(C, name="parquet_debug_sort_sweep_less_cpp") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: nrows   !! rows per pass.
                integer(c_long_long), value :: nreps   !! passes.
                integer(c_long_long) :: r              !! checksum, or -1 when unusable.
            end function c_sweep_less
            function c_sweep_compare(handle, nrows, nreps, nk) &
                    bind(C, name="parquet_debug_sort_sweep_compare_cpp") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: nrows   !! rows per pass.
                integer(c_long_long), value :: nreps   !! passes.
                integer(c_long_long), value :: nk      !! leading keys taking part.
                integer(c_long_long) :: r              !! checksum, or -1 when unusable.
            end function c_sweep_compare
        end interface
        integer(int64) :: a, b
        logical :: fl, cl, fl_rev
        integer :: fc, cc
        integer(int64) :: sweep_less, sweep_cmp !! the two batched hooks' answers over this fixture.
        integer(int64) :: csweep_less, csweep_cmp !! the C++ twins' answers over the same fixture.
        type(pf_sort_keys) :: empty_keys        !! never `%add`ed to, for the unusable-input arms.
        !
        ! ---- the two BATCHED comparator hooks, against an oracle that shares none of their code ----
        !
        ! `parquet_debug_sort_sweep_less`/`_compare` exist for bench/benchmark_sort_comparator.f90,
        ! which needs the comparator's own cost rather than the cost of reaching it -- so they loop
        ! inside `parquet_sorting` and nothing in the library calls them. That leaves them with no
        ! caller a test run ever reaches, and their checksums are what the benchmark's C++/Fortran
        ! comparison rests on: a sweep that walked the wrong pairs would make both arms of that
        ! comparison meaningless while still printing a plausible number.
        !
        ! **The oracle is combinatorial, not a second sweep.** With `nreps = n - 1` the stride walk
        ! (`stride = 1 + mod(rep, n - 1)`, then `j = i + stride` wrapped) visits each stride 1..n-1
        ! once for every `i`, which is exactly every ordered pair `(i, j)` with `i /= j`, once. The
        ! row order is STRICT and TOTAL -- that is what `sweep_pairs` asserts below, pair by pair --
        ! so of each unordered pair exactly one direction is `less`, giving `n*(n-1)/2`; and
        ! `sort_keys_compare` is antisymmetric, so the same walk sums to zero whatever the prefix.
        ! Neither figure knows anything about the fixture, so a hook that dropped a stride, walked
        ! `i` twice or wrapped wrongly cannot coincide with it.
        if (n >= 2_int64) then
            sweep_less = parquet_debug_sort_sweep_less(keys, n, n - 1_int64)
            call check(error, sweep_less == n * (n - 1_int64) / 2_int64, &
                label // ": the batched less-sweep must count each unordered pair exactly once")
            if (allocated(error)) return
            sweep_cmp = parquet_debug_sort_sweep_compare(keys, n, n - 1_int64, nkeys)
            call check(error, sweep_cmp == 0_int64, &
                label // ": the batched compare-sweep must sum to zero by antisymmetry")
            if (allocated(error)) return
            ! The C++ twins of the same two hooks, against the same combinatorial oracle -- and then
            ! against the Fortran answers, which is the equality the benchmark's whole C++-versus-
            ! Fortran comparison rests on ("their returned checksums must match, which is what
            ! proves they did the same work", parquet_debug_sort_sweep_less_cpp's own comment). The
            ! oracle comes first deliberately: two sweeps that walked the same WRONG pairs would
            ! agree with each other and with nothing else.
            csweep_less = int(c_sweep_less(builder, int(n, c_long_long), int(n - 1_int64, c_long_long)), int64)
            call check(error, csweep_less == n * (n - 1_int64) / 2_int64, &
                label // ": the C++ less-sweep must count each unordered pair exactly once")
            if (allocated(error)) return
            call check(error, csweep_less == sweep_less, &
                label // ": the C++ and Fortran less-sweeps must return the same checksum")
            if (allocated(error)) return
            csweep_cmp = int(c_sweep_compare(builder, int(n, c_long_long), &
                int(n - 1_int64, c_long_long), int(nkeys, c_long_long)), int64)
            call check(error, csweep_cmp == 0_int64, &
                label // ": the C++ compare-sweep must sum to zero by antisymmetry")
            if (allocated(error)) return
            call check(error, csweep_cmp == sweep_cmp, &
                label // ": the C++ and Fortran compare-sweeps must return the same checksum")
            if (allocated(error)) return
        end if
        ! Both unusable-input arms, since a hook that answered -1 unconditionally would satisfy
        ! every assertion above by never running at all: an empty key set, and a row count with no
        ! pair in it. The second is asked of the REAL key set, so it is the guard being tested and
        ! not the emptiness.
        call check(error, parquet_debug_sort_sweep_less(empty_keys, n, 1_int64) == -1_int64, &
            label // ": the less-sweep must decline a key set nothing was added to")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_sweep_compare(empty_keys, n, 1_int64, nkeys) == -1_int64, &
            label // ": the compare-sweep must decline a key set nothing was added to")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_sweep_less(keys, 1_int64, 1_int64) == -1_int64, &
            label // ": the less-sweep must decline a single row, which has no pair to compare")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_sweep_compare(keys, 1_int64, 1_int64, nkeys) == -1_int64, &
            label // ": the compare-sweep must decline a single row too")
        if (allocated(error)) return
        ! The C++ twins decline the same way, for the same reason: a hook that answered -1
        ! unconditionally would satisfy every equality above by never running.
        call check(error, c_sweep_less(builder, 1_c_long_long, 1_c_long_long) == -1_c_long_long, &
            label // ": the C++ less-sweep must decline a single row, which has no pair to compare")
        if (allocated(error)) return
        call check(error, c_sweep_compare(builder, 1_c_long_long, 1_c_long_long, &
            int(nkeys, c_long_long)) == -1_c_long_long, &
            label // ": the C++ compare-sweep must decline a single row too")
        if (allocated(error)) return
        !
        do a = 1_int64, n
            do b = 1_int64, n
                fl = parquet_debug_sort_row_less(keys, a, b)
                cl = c_dbg_row_less(builder, int(a - 1_int64, c_long_long), &
                    int(b - 1_int64, c_long_long)) == 1_c_long_long
                call check(error, fl .eqv. cl, label // ": sort_row_less disagrees with the C++ engine")
                if (allocated(error)) return
                !
                fc = parquet_debug_sort_keys_compare(keys, a, b, nkeys)
                cc = int(c_dbg_keys_compare(builder, int(a - 1_int64, c_long_long), &
                    int(b - 1_int64, c_long_long), int(nkeys, c_long_long)))
                call check(error, fc == cc, label // ": sort_keys_compare disagrees with the C++ engine")
                if (allocated(error)) return
                !
                if (a == b) then
                    call check(error, .not. fl, label // ": sort_row_less(i, i) must be .false.")
                    if (allocated(error)) return
                else
                    fl_rev = parquet_debug_sort_row_less(keys, b, a)
                    call check(error, fl .neqv. fl_rev, &
                        label // ": exactly one of less(a,b) and less(b,a) must hold")
                    if (allocated(error)) return
                    if (fc /= 0) then
                        ! Safe for any prefix: if the leading `nkeys` keys already decide, the full
                        ! comparator decides the same way on the same key.
                        call check(error, fl .eqv. (fc < 0), &
                            label // ": less must follow the sign of the tie-free comparator")
                    else if (nkeys >= ntotal) then
                        ! Only meaningful when the prefix covers EVERY key. On a shorter prefix a
                        ! zero says "tied so far", and a later key -- not the row index -- is what
                        ! `less` used to decide. Asserting row order here would be asserting that
                        ! the prefix is the whole key list.
                        call check(error, fl .eqv. (a < b), &
                            label // ": on a full tie, less must fall back to row order")
                    end if
                    if (allocated(error)) return
                end if
            end do
        end do
    end subroutine sweep_pairs

    !
    !> Integer keys: three-way ties, two nulls, every direction and null placement.
    subroutine test_engine_conf_int(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        integer(int64), target :: vals(n)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf
        logical :: desc, nf
        !
        ! Three 5s and two 3s, so tie order is exercised on most pairs; rows 3 and 5 are null, and
        ! their value slots deliberately hold ordinary values -- a comparator that forgets the tier
        ! test would order them by those and still look plausible.
        vals = [5_int64, 3_int64, 5_int64, 1_int64, 3_int64, 9_int64, 0_int64, 5_int64]
        fvalid = [.true., .true., .false., .true., .false., .true., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_int64(builder, vals, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "int desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_int

    !
    !> Real keys: all three tiers at once -- values, NaNs and nulls -- with ties inside each.
    subroutine test_engine_conf_real(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        real(real64), target :: vals(n)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        real(real64) :: qnan
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf
        logical :: desc, nf
        !
        qnan = ieee_value(1.0_real64, ieee_quiet_nan)
        ! Two NaNs and two nulls, plus repeated values: this is the only fixture that can catch a
        ! comparator putting NaNs on the wrong side of the null block, which no ascending,
        ! null-free test can see.
        vals = [2.0_real64, qnan, -1.0_real64, 2.0_real64, qnan, 0.0_real64, 3.0_real64, -1.0_real64]
        fvalid = [.true., .true., .true., .false., .true., .false., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_double(builder, vals, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "real desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_real

    !
    !> Fixed-width string keys, including a byte above 127.
    !!
    !! `%add` on a `character(len=*)` array sorts on the FULL declared width, blanks included, so
    !! every row here is the same length and the length tail of the comparison is not reached — that
    !! is `test_engine_conf_varstr`'s job. What this fixture does cover is the **unsigned** byte
    !! rule: row 6 carries `achar(200)`, which must sort ABOVE every ASCII row. A comparator reading
    !! bytes as signed puts it below them, and nothing else in the suite would notice.
    subroutine test_engine_conf_str(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 7_int64
        integer(int64), parameter :: w = 3_int64
        character(len=3), target :: vals(n)
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(n * w)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf, j
        integer(int64) :: k
        logical :: desc, nf
        !
        vals = ["abc", "ab ", "abc", "b  ", "a  ", "z  ", "abc"]
        vals(6)(1:1) = achar(200)
        fvalid = [.true., .true., .false., .true., .true., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        ! The same bytes the Fortran side will pack, laid out for the C++ builder: fixed width,
        ! 0-based offsets, no trimming.
        do k = 1_int64, n + 1_int64
            offs(k) = int((k - 1_int64) * w, c_long_long)
        end do
        do k = 1_int64, n
            do j = 1, int(w)
                bytes((k - 1_int64) * w + j) = vals(k)(j:j)
            end do
        end do
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_string(builder, offs, bytes, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "str desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_str

    !
    !> Variable-length string keys, where a prefix must sort BEFORE the string it is a prefix of.
    !!
    !! This is the one fixture that separates `memcmp` semantics from Fortran's own `<`, which
    !! blank-pads the shorter operand and would call "ab" and "ab " equal. A `parquet_string_column`
    !! is the only route to genuinely ragged rows, since the `character(len=*)` form is fixed width.
    subroutine test_engine_conf_varstr(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 6_int64
        character(len=*), parameter :: raw(n) = ["ab ", "a  ", "abc", "ab ", "   ", "b  "]
        integer(int64), parameter :: lens(n) = [2_int64, 1_int64, 3_int64, 2_int64, 0_int64, 1_int64]
        type(parquet_string_column) :: col
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(9) !! sum(lens) -- sized exactly, not guessed.
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf, j
        integer(int64) :: k, pos
        logical :: desc, nf
        !
        ! "a" is a prefix of "ab", which is a prefix of "abc", and "" is a prefix of everything;
        ! rows 1 and 4 are an exact tie. Fortran's own comparison would order several of these
        ! differently from memcmp, which is precisely the point.
        call col%clear()
        do k = 1_int64, n
            call col%append_string(raw(k)(1:lens(k)))
        end do
        pos = 0_int64
        do k = 1_int64, n
            offs(k) = int(pos, c_long_long)
            do j = 1, int(lens(k))
                bytes(pos + j) = raw(k)(j:j)
            end do
            pos = pos + lens(k)
        end do
        offs(n + 1) = int(pos, c_long_long)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(col, descending=desc, nulls_first=nf)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_string(builder, offs, bytes, c_null_ptr, &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "varstr desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_varstr

    !
    !> Three keys of mixed family, swept over every `nkeys` prefix including an over-long one.
    !!
    !! The prefix is what run detection uses -- "sort by field then magnitude, but group by field
    !! alone" -- so an off-by-one there silently changes what `pf_unique`/`pf_rank` treat as one
    !! group while leaving every ordering correct. The primary key repeats heavily so that the
    !! second and third keys actually decide most pairs.
    subroutine test_engine_conf_multi(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        integer(int64), parameter :: w = 2_int64
        integer(int64), target :: k1(n)
        real(real64), target :: k2(n)
        character(len=2), target :: k3(n)
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(n * w)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: nk, j
        integer(int64) :: k
        !
        k1 = [1_int64, 1_int64, 1_int64, 2_int64, 2_int64, 2_int64, 1_int64, 2_int64]
        k2 = [7.0_real64, 7.0_real64, 5.0_real64, 1.0_real64, 1.0_real64, 9.0_real64, 7.0_real64, 1.0_real64]
        k3 = ["bb", "aa", "cc", "aa", "zz", "mm", "bb", "aa"]
        ! Nulls on the SECOND key only, so the tier rule has to be applied per key rather than per
        ! row -- a comparator that hoisted the null test out of the key loop would pass every
        ! single-key fixture above and fail here.
        fvalid = [.true., .true., .false., .true., .true., .true., .true., .false.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do k = 1_int64, n + 1_int64
            offs(k) = int((k - 1_int64) * w, c_long_long)
        end do
        do k = 1_int64, n
            do j = 1, int(w)
                bytes((k - 1_int64) * w + j) = k3(k)(j:j)
            end do
        end do
        !
        call keys%add(k1)
        call keys%add(k2, descending=.true., is_valid=fvalid)
        call keys%add(k3, nulls_first=.true.)
        builder = parquet_sort_builder_new(int(n, c_long_long))
        call parquet_sort_builder_add_key_int64(builder, k1, c_null_ptr, 0_c_int8_t, 0_c_int8_t)
        call parquet_sort_builder_add_key_double(builder, k2, c_loc(cvalid), 1_c_int8_t, 0_c_int8_t)
        call parquet_sort_builder_add_key_string(builder, offs, bytes, c_null_ptr, 0_c_int8_t, 1_c_int8_t)
        ! nkeys = 4 is deliberately one more than exists: both engines must clamp, not read past.
        do nk = 1, 4
            call sweep_pairs(error, "multi nkeys=" // achar(iachar("0") + nk), keys, builder, n, nk, 3)
            if (allocated(error)) then
                call parquet_sort_builder_free(builder)
                return
            end if
        end do
        call parquet_sort_builder_free(builder)
    end subroutine test_engine_conf_multi

    !
    ! ============================================================================================
    ! Stage 2 conformance
    ! ============================================================================================
    !
    ! Stage 1 proved the two comparators agree pair by pair. These prove the two SORTS agree
    ! permutation by permutation, which is a different claim: a correct comparator driven by a
    ! defective sort still returns a sorted answer whenever the defect only reorders equal elements
    ! -- and under `sort_row_less` there are no equal elements, so the two engines' permutations
    ! must be identical element for element or one of them is wrong.
    !
    ! Both engines are reached through the ordinary public entry point (`pf_argsort`), switched by
    ! `parquet_debug_use_fortran_sort_engine`. That is deliberate: it tests the wiring in
    ! `drive_engine` as well as the algorithm, which a direct call into the engine would not.
    !
    !> Arms the C++ comparison counter; see `arm_sort_comparisons` in test_sort.f90 for the full why.
    subroutine engine_arm_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine engine_arm_comparisons

    !
    !> C++ comparisons counted since the last `engine_arm_comparisons`, then disarms the counter.
    integer(int64) function engine_comparisons() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since the counter was armed.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function engine_comparisons

    !
    !> Argsorts one key set down BOTH engines and requires the two permutations to be identical.
    !!
    !! The permutation-validity check afterwards is not redundant with the equality: two engines
    !! broken in the same way would agree with each other, and only "names every row exactly once"
    !! notices. It is the cheapest independent oracle available here.
    subroutine engine_ab(error, label, keys, n)
        type(error_type), allocatable, intent(inout) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: label                 !! names the fixture in every message.
        class(pf_sort_keys), intent(in) :: keys               !! the key set to sort by.
        integer(int64), intent(in) :: n                       !! rows.
        integer(int64), allocatable :: pc(:) !! the C++ engine's permutation.
        integer(int64), allocatable :: pf(:) !! the Fortran engine's permutation.
        logical, allocatable :: seen(:)      !! which rows the Fortran permutation named.
        integer(int64) :: k                  !! walk index.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_argsort(keys, pc)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_argsort(keys, pf)
        call restore_engine_default()
        !
        call check(error, size(pf, kind=int64) == n, label // ": the Fortran permutation is the wrong length")
        if (allocated(error)) return
        call check(error, size(pc, kind=int64) == n, label // ": the C++ permutation is the wrong length")
        if (allocated(error)) return
        call check(error, all(pf == pc), label // ": the Fortran permutation differs from the C++ one")
        if (allocated(error)) return
        !
        allocate(seen(max(n, 1_int64)))
        seen = .false.
        do k = 1_int64, n
            if (pf(k) < 1_int64 .or. pf(k) > n) then
                call check(error, .false., label // ": the Fortran permutation holds an out-of-range row")
                return
            end if
            seen(pf(k)) = .true.
        end do
        call check(error, all(seen(1:n)), label // ": the Fortran permutation does not name every row once")
    end subroutine engine_ab


    !
    !> Puts the engine selector back to the value the library ships with.
    !!
    !! **Every test here that forces an engine must end by calling this, not by calling
    !! `parquet_debug_use_fortran_sort_engine(.false.)`.** Before Stage 6 the two were the same
    !! thing, so "restore" was written as "clear" throughout this file; the flip made those opposite
    !! operations, and a leaked selector is the worst kind of leak — it does not fail the test that
    !! leaked it, it silently changes which engine some LATER test measures. That has already
    !! happened once in this suite for a different global (see `test_engine_refine_floor`, which
    !! broke two settings tests that never mention sorting).
    !!
    !! The shipped default is `dbg_fortran_engine` in **`tools/generate_parquet_sorting.py`** —
    !! `src/parquet_sorting.f90` is generated, so that is the only place it can be changed, and
    !! this helper is the only place the tests encode it.
    subroutine restore_engine_default()
        call parquet_debug_use_fortran_sort_engine(.true.)
    end subroutine restore_engine_default

    !
    !> The switch really switches: the C++ engine counts comparisons, the Fortran one cannot.
    !!
    !! **Without this every other Stage 2 test is potentially vacuous.** They assert that two
    !! permutations agree, and if `parquet_debug_use_fortran_sort_engine` did nothing at all -- a
    !! flag never read, a branch placed after the return, a regenerated file that lost the wiring --
    !! both halves would be the C++ engine and every one of them would pass while testing nothing.
    !! That is `feature_risks.md` Risk-35's failure mode exactly.
    !!
    !! The counter lives inside the C++ comparator, so it can only move when the C++ comparator
    !! runs. The key is real-valued with distinct values so that the integer counting fast path --
    !! which performs zero comparisons by construction -- declines it and the C++ arm must count.
    subroutine test_fortran_engine_switches(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 64_int64
        real(real64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: cmp_cpp, cmp_fortran, k
        !
        do k = 1_int64, n
            v(k) = real(mod(k * 37_int64, n), real64) + 0.5_real64
        end do
        call keys%add(v)
        !
        ! NOT restore_engine_default(): this arm asserts what the selector reports after being
        ! CLEARED, so it must clear it. The restore is at the end of the test.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call check(error, .not. parquet_debug_using_fortran_sort_engine(), &
            "the engine selector must report the C++ engine after being cleared")
        if (allocated(error)) return
        call engine_arm_comparisons()
        call pf_argsort(keys, perm)
        cmp_cpp = engine_comparisons()
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call check(error, parquet_debug_using_fortran_sort_engine(), &
            "the engine selector must report the Fortran engine after being set")
        if (allocated(error)) then
            call restore_engine_default()
            return
        end if
        call engine_arm_comparisons()
        call pf_argsort(keys, perm)
        cmp_fortran = engine_comparisons()
        call restore_engine_default()
        !
        call check(error, cmp_cpp > 0_int64, &
            "the C++ arm counted no comparisons, so the counter is not measuring what it should")
        if (allocated(error)) return
        call check(error, cmp_fortran == 0_int64, &
            "the Fortran arm reached the C++ comparator, so the engine selector did not switch")
    end subroutine test_fortran_engine_switches

    !
    !> Both engines, every key family, over sizes spanning the insertion cutoff and the recursion.
    !!
    !! The sizes are chosen against the algorithm rather than at random: 2 and 5 never leave the
    !! final insertion pass, 16 is exactly `SORT_INSERTION_CUTOFF`, 17 is the first size that
    !! partitions at all, and 257/1000 recurse several levels deep. A sweep that used only round
    !! numbers would miss the cutoff boundary, which is where an off-by-one in the loop condition
    !! lives.
    !!
    !! The integer key is deliberately LOW-CARDINALITY, so that both engines take their integer
    !! counting fast path and the *fast* paths are compared rather than only the comparator ones.
    !! Until Stage 3 the Fortran side had no counting path and this arm crossed the two routes; it no
    !! longer does, and `test_counting_path_matches_comparator` is what crosses them now.
    subroutine test_fortran_engine_ab_families(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sizes(6) = [2_int64, 5_int64, 16_int64, 17_int64, 257_int64, 1000_int64]
        integer(int64), allocatable :: vi(:)
        real(real64), allocatable :: vr(:)
        character(len=4), allocatable :: vs(:)
        logical, allocatable :: valid(:)
        type(pf_sort_keys) :: keys
        integer(int64) :: n, k
        integer :: is, id, inf
        logical :: desc, nf
        character(len=32) :: nstr
        character(len=:), allocatable :: tag
        !
        do is = 1, size(sizes)
            n = sizes(is)
            allocate(vi(n), vr(n), vs(n), valid(n))
            do k = 1_int64, n
                vi(k) = mod(k * 5_int64, 8_int64)
                vr(k) = real(mod(k * 7_int64, 11_int64), real64)
                ! Every thirteenth row is a NaN, so the middle tier is populated at every size from
                ! 17 upward -- and absent below it, which is itself worth covering.
                if (mod(k, 13_int64) == 0_int64) vr(k) = ieee_value(1.0_real64, ieee_quiet_nan)
                write (nstr, "(i0)") mod(k * 3_int64, 17_int64)
                vs(k) = trim(nstr)
                valid(k) = (mod(k, 5_int64) /= 0_int64)
            end do
            write (nstr, "(i0)") n
            do id = 0, 1
                do inf = 0, 1
                    desc = (id == 1)
                    nf = (inf == 1)
                    tag = " n=" // trim(nstr) // " desc=" // merge("T", "F", desc) // &
                        " nf=" // merge("T", "F", nf)
                    !
                    call keys%clear()
                    call keys%add(vi, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "int" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    call keys%clear()
                    call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "real" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    call keys%clear()
                    call keys%add(vs, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "str" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    ! Multi-key, with the null-bearing key SECOND: the tier test has to be applied
                    ! per key rather than per row, and a sort that hoisted it would still agree with
                    ! the C++ engine on every single-key fixture above.
                    call keys%clear()
                    call keys%add(vi, descending=desc)
                    call keys%add(vr, nulls_first=nf, is_valid=valid)
                    call keys%add(vs, descending=.not. desc)
                    call engine_ab(error, "multi" // tag, keys, n)
                    if (allocated(error)) return
                end do
            end do
            deallocate(vi, vr, vs, valid)
        end do
    end subroutine test_fortran_engine_ab_families

    !
    !> The A/B sweep for the key families the integer one cannot reach: STRING, REAL and a
    !> TWO-key timestamp.
    !!
    !! **What this covers that `test_fortran_engine_ab_operations` does not.** That test drives the
    !! whole operation set through an `integer(int32)` fixture, so on the C++ side it only ever
    !! reaches the `case default` (SK_INT) arm of each one-shot dispatch in
    !! src/parquet_sorting_oracle.f90, and only ever the single-key branch. Four dispatch arms and
    !! one branch are therefore reachable from nowhere else in the suite:
    !! `engine_one_shot_partial`'s SK_STR, `engine_one_shot_nth`'s SK_REAL and SK_STR,
    !! `engine_one_shot_is_sorted`'s SK_STR, and the multi-key builder path of ALL THREE of
    !! `oracle_nth`, `oracle_partial` and `oracle_is_sorted` -- which a `parquet_timestamp` is the
    !! way in to, since it binds as two integer keys (`extract_ts`,
    !! src/parquet_sorting_keys.f90) while `pf_nth_element` and `pf_partial_sort` have no
    !! `pf_sort_keys` form at all.
    !!
    !! **A multi-key path is not the single-key one with a loop around it.** Each of those three
    !! procedures shortcuts to a one-shot entry point at `size(keys) == 1` and otherwise builds a
    !! C++ sort builder, feeds it key by key and checks the returned status -- a different entry
    !! point, a different lifetime and an error arm that the shortcut does not have.
    !!
    !! **These arms differ from each other by one C++ entry point name**, which is precisely the
    !! kind of difference that is invisible until something asks: a `case` wired to the int64 entry
    !! point for a real key still returns a plausible rank, and would be caught by nothing here
    !! except a comparison against the Fortran engine over the same data.
    !!
    !! Every fixture is duplicate-heavy for the reason stated at the head of this section: the tie
    !! rules are what the two engines can disagree about while both remaining correctly sorted.
    subroutine test_engine_ab_string_real_ts(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        !> Unordered, with a three-way tie on "pear" and a two-way one on "fig", so the index
        !! tiebreaker decides several ranks rather than none.
        character(len=6), parameter :: sv(9) = &
            [character(len=6) :: "pear", "fig", "apple", "pear", "plum", "fig", "kiwi", "pear", "date"]
        character(len=6), parameter :: sorted_sv(4) = &
            [character(len=6) :: "apple", "date", "fig", "fig"]
        real(real64), parameter :: rv(9) = &
            [3.5_real64, -1.0_real64, 3.5_real64, 8.25_real64, 0.0_real64, &
             -1.0_real64, 12.0_real64, 3.5_real64, 2.0_real64]
        !> Six instants sharing ONE second, so the nanosecond -- the SECOND engine key -- decides
        !! every rank. Two nanosecond values repeat, so the index tiebreaker decides two of them.
        integer(int32), parameter :: tns(6) = [500, 100, 500, 300, 100, 700]
        type(parquet_timestamp) :: tv(size(tns))
        integer(int64) :: csec, fsec                        !! per-engine nth timestamp seconds.
        integer(int32) :: cns, fns                          !! per-engine nth timestamp nanoseconds.
        character(len=:), allocatable :: cs, fs             !! per-engine nth string value.
        character(len=6), allocatable :: cps(:), fps(:)     !! per-engine partial-sorted strings.
        real(real64) :: crv, frv                            !! per-engine nth real value.
        type(parquet_timestamp) :: cts, fts                 !! per-engine nth timestamp.
        integer(int64) :: ci, fi                            !! per-engine nth row index.
        logical :: cok, fok                                 !! per-engine is_sorted answers.
        type(parquet_timestamp), allocatable :: cpt(:), fpt(:) !! per-engine partial-sorted instants.
        integer(int64), allocatable :: tperm(:)             !! puts `tv` in rank order for the sorted arm.
        integer :: k
        character(len=32) :: kstr
        !
        ! `set_raw` rather than `set`: the pair the engine actually extracts is (seconds,
        ! nanoseconds), and setting it directly is what makes the fixture's shared second exact
        ! rather than a consequence of civil-field arithmetic.
        do k = 1, size(tv)
            call tv(k)%set_raw(100_int64, tns(k))
        end do
        !
        ! ---- nth on a STRING key: engine_one_shot_nth's SK_STR arm ----
        ! Every rank, so the ties at ranks 3-4 ("fig") and 6-8 ("pear") are each asked about from
        ! both sides rather than only in the middle.
        do k = 1, size(sv)
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_nth_element(sv, k, cs, ci)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_nth_element(sv, k, fs, fi)
            call restore_engine_default()
            call check(error, cs == fs, &
                "string nth rank=" // trim(kstr) // ": the two engines returned different values")
            if (allocated(error)) return
            ! The INDEX too: under the index tiebreaker exactly one row holds each rank, so a
            ! disagreement on the tied "fig"/"pear" rows is a real defect that values alone hide.
            call check(error, ci == fi, &
                "string nth rank=" // trim(kstr) // ": the two engines returned different row indices")
            if (allocated(error)) return
        end do
        !
        ! ---- nth on a REAL key: engine_one_shot_nth's SK_REAL arm ----
        do k = 1, size(rv)
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_nth_element(rv, k, crv, ci)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_nth_element(rv, k, frv, fi)
            call restore_engine_default()
            call check(error, crv == frv, &
                "real nth rank=" // trim(kstr) // ": the two engines returned different values")
            if (allocated(error)) return
            call check(error, ci == fi, &
                "real nth rank=" // trim(kstr) // ": the two engines returned different row indices")
            if (allocated(error)) return
        end do
        !
        ! ---- nth on a TWO-key timestamp: oracle_nth's builder path ----
        ! The single-key shortcut above it (`size(keys) == 1`) is what every other nth call in the
        ! suite takes, so this is the only thing that builds a C++ sort builder for a rank query at
        ! all -- including the `status`/`idx` check that follows the build.
        do k = 1, size(tv)
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_nth_element(tv, k, cts, ci)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_nth_element(tv, k, fts, fi)
            call restore_engine_default()
            call cts%get_raw(csec, cns)
            call fts%get_raw(fsec, fns)
            call check(error, csec == fsec .and. cns == fns, &
                "timestamp nth rank=" // trim(kstr) // ": the two engines returned different values")
            if (allocated(error)) return
            call check(error, ci == fi, &
                "timestamp nth rank=" // trim(kstr) // ": the two engines returned different row indices")
            if (allocated(error)) return
        end do
        ! Absolute, not just an A/B: with every row sharing a second, a chain that consulted only
        ! the first key would return whichever row it happened to reach and both engines could
        ! agree on it. Rank 1 must be the smallest nanosecond, which is row 5 (k*7 mod 5 = 0).
        call check(error, fi > 0_int64, "the timestamp rank must resolve to a real row")
        if (allocated(error)) return
        call pf_nth_element(tv, 1, fts, fi)
        call fts%get_raw(fsec, fns)
        call check(error, fns == 100 .and. fi == 2_int64, &
            "the timestamp nth must consult the SECOND key: rank 1 is row 2, the first ns=100 row")
        if (allocated(error)) return
        !
        ! ---- partial sort on a STRING key: engine_one_shot_partial's SK_STR arm ----
        do k = 1, 4
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_partial_sort(sv, cps, k)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_partial_sort(sv, fps, k)
            call restore_engine_default()
            call check(error, size(cps) == size(fps), &
                "string partial n=" // trim(kstr) // ": the two engines returned different lengths")
            if (allocated(error)) return
            call check(error, all(cps == fps), &
                "string partial n=" // trim(kstr) // ": the Fortran engine disagreed with the C++ one")
            if (allocated(error)) return
        end do
        ! Absolute, so that two engines agreeing on the wrong four strings cannot pass.
        call check(error, all(fps == sorted_sv), &
            "the first four strings in order must be apple, date, fig, fig")
        if (allocated(error)) return
        !
        ! ---- is_sorted on a STRING key: engine_one_shot_is_sorted's SK_STR arm ----
        ! Both answers, since a procedure that always says .true. passes a one-sided test.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(sv, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(sv, fok)
        call restore_engine_default()
        call check(error, (.not. cok) .and. (cok .eqv. fok), &
            "is_sorted on unordered strings: both engines must answer .false.")
        if (allocated(error)) return
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(sorted_sv, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(sorted_sv, fok)
        call restore_engine_default()
        call check(error, cok .and. (cok .eqv. fok), &
            "is_sorted on ordered strings WITH A TIE: both engines must answer .true.")
        if (allocated(error)) return
        !
        ! ---- partial sort on a TWO-key timestamp: oracle_partial's builder path ----
        do k = 1, 3
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_partial_sort(tv, cpt, k)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_partial_sort(tv, fpt, k)
            call restore_engine_default()
            call check(error, size(cpt) == size(fpt), &
                "timestamp partial n=" // trim(kstr) // ": the two engines returned different lengths")
            if (allocated(error)) return
            call cpt(k)%get_raw(csec, cns)
            call fpt(k)%get_raw(fsec, fns)
            call check(error, csec == fsec .and. cns == fns, &
                "timestamp partial n=" // trim(kstr) // ": the two engines disagreed on entry " // trim(kstr))
            if (allocated(error)) return
        end do
        ! Absolute, so that two engines agreeing on the wrong three instants cannot pass. Sorted by
        ! nanosecond with the index tiebreaker, the first three rows are 2 (100), 5 (100), 4 (300).
        call fpt(1)%get_raw(fsec, fns)
        call check(error, fns == 100, "the earliest instant must be one of the two ns=100 rows")
        if (allocated(error)) return
        call fpt(3)%get_raw(fsec, fns)
        call check(error, fns == 300, &
            "the third instant must be ns=300: a chain ignoring the second key could not know that")
        if (allocated(error)) return
        !
        ! ---- is_sorted on a TWO-key timestamp: oracle_is_sorted's builder path ----
        ! Both answers again, and the ordered arm is the `tv` rows in rank order rather than a
        ! second fixture, so the two arms differ only in their order.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(tv, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(tv, fok)
        call restore_engine_default()
        call check(error, (.not. cok) .and. (cok .eqv. fok), &
            "is_sorted on unordered timestamps: both engines must answer .false.")
        if (allocated(error)) return
        call pf_argsort(tv, tperm)
        call pf_permute(tv, tperm)
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(tv, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(tv, fok)
        call restore_engine_default()
        call check(error, cok .and. (cok .eqv. fok), &
            "is_sorted on the SAME timestamps once sorted: both engines must answer .true.")
    end subroutine test_engine_ab_string_real_ts

    !
    subroutine test_fortran_engine_ab_operations(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        !> **60 rows, not a dozen.** `sort_nth_index`'s quickselect only loops while the surviving
        !! range exceeds `SORT_INSERTION_CUTOFF` (16), so a fixture at or below that size runs
        !! straight into the final insertion sort and never partitions at all — a deliberate
        !! off-by-one in the side selection survived a 12-row version of this test for exactly that
        !! reason. `feature_risks.md` Risk-49 is the general form: a size threshold is a fast path
        !! wearing different clothes.
        integer(int32) :: v(60)
        integer(int32), parameter :: sa(5) = [3, 10, 10, 21, 40] !! ordered, with a tie inside it.
        integer(int32), parameter :: sb(4) = [1, 10, 22, 40]     !! ordered, and ties across sa.
        integer(int32), parameter :: targets(5) = [2, 3, 10, 22, 99] !! absent, first, tied, mid, past-end.
        integer(int32), parameter :: counts(5) = [1, 2, 16, 17, 60]  !! partial counts, spanning the cutoff.
        !> Two keys whose FIRST alone has duplicates, so grouping by a prefix shorter than the key
        !! list is a different question from grouping by all of it. Every single-key fixture makes
        !! `group_keys` and `size(keys)` equal, which is why one is needed here.
        integer(int32), parameter :: g1(8) = [2, 1, 2, 1, 3, 2, 1, 3]
        integer(int32), parameter :: g2(8) = [9, 4, 7, 6, 1, 5, 8, 2]
        !> A merge tie the OUTPUT can see. `pf_merge` returns values, so two equal integers are
        !! indistinguishable however the tie was broken — which let "take from the second range"
        !! survive a first version of this test. Negative zero compares equal to positive zero and
        !! is still a different value, so it reveals which input the element came from.
        !!
        !! **Built at runtime, not as a `parameter`, and probed by BIT PATTERN rather than by
        !! `sign()`** — both changed after ifx failed this assertion where gfortran and flang passed.
        !! `SIGN(A, B)` with a zero `B` is explicitly processor-dependent (F2018 16.9.180): a
        !! processor that does not distinguish negative zero returns `|A|`, so `sign(1.0, -0.0)` is
        !! allowed to be `+1.0`. That made the old probe report a correct merge as broken. The sign
        !! of a negative zero in a *constant expression* is a second, independent risk, removed here
        !! by constructing the array at runtime; it costs nothing and rules the question out.
        real(real64) :: za(2), zb(2)
        integer(int32), allocatable :: cp(:), fp(:) !! per-engine value results.
        real(real64), allocatable :: cz(:), fz(:)   !! per-engine merged reals.
        integer, allocatable :: cr(:), fr(:)        !! per-engine ranks.
        integer(int64), allocatable :: cperm(:), fperm(:), coff(:), foff(:) !! per-engine grouping.
        type(pf_sort_keys) :: gk                    !! the two-key set.
        integer(int32) :: cv, fv                    !! per-engine nth value.
        integer(int64) :: ci, fi                    !! per-engine nth index.
        integer(int64) :: cl, fl, ch, fh            !! per-engine lower/upper bounds.
        integer :: cc, fc                           !! per-engine distinct counts.
        logical :: cok, fok                         !! per-engine is_sorted answers.
        integer :: k                                !! sweep index.
        character(len=32) :: kstr
        !
        ! Duplicates every third row or so, and no order at all: run detection and the tie rules
        ! need repeated values, and the select paths need the input not to be already sorted.
        do k = 1, size(v)
            v(k) = int(mod(k * 37, 23), int32)
        end do
        !
        ! ---- partial: drive_engine_partial -> sort_partial_permutation ----
        do k = 1, size(counts)
            write (kstr, "(i0)") counts(k)
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_partial_sort(v, cp, counts(k))
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_partial_sort(v, fp, counts(k))
            call restore_engine_default()
            call check(error, size(cp) == size(fp), &
                "partial count=" // trim(kstr) // ": the two engines returned different lengths")
            if (allocated(error)) return
            call check(error, all(cp == fp), &
                "partial count=" // trim(kstr) // ": the Fortran engine disagreed with the C++ one")
            if (allocated(error)) return
        end do
        !
        ! ---- nth: engine_nth_index -> sort_nth_index. Every rank, so the quickselect's narrowing is
        ! exercised from both ends and not only in the middle.
        do k = 1, size(v)
            write (kstr, "(i0)") k
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_nth_element(v, k, cv, ci)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_nth_element(v, k, fv, fi)
            call restore_engine_default()
            call check(error, cv == fv, &
                "nth rank=" // trim(kstr) // ": the two engines returned different values")
            if (allocated(error)) return
            ! The INDEX too, not just the value: under the index tiebreaker exactly one row holds
            ! each rank, so a disagreement here on the duplicated 10s and 40s is a real defect that
            ! comparing values alone would miss.
            call check(error, ci == fi, &
                "nth rank=" // trim(kstr) // ": the two engines returned different row indices")
            if (allocated(error)) return
        end do
        !
        ! ---- is_sorted: engine_is_sorted -> sort_is_sorted. Both answers, since a procedure that
        ! always says .true. passes a one-sided test.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(v, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(v, fok)
        call restore_engine_default()
        call check(error, (.not. cok) .and. (cok .eqv. fok), &
            "is_sorted on unordered input: both engines must answer .false.")
        if (allocated(error)) return
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_is_sorted(sa, cok)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_is_sorted(sa, fok)
        call restore_engine_default()
        call check(error, cok .and. (cok .eqv. fok), &
            "is_sorted on ordered input WITH A TIE: both engines must answer .true.")
        if (allocated(error)) return
        !
        ! ---- run detection: engine_build_runs -> sort_build_runs_permutation ----
        ! **SERIAL on both sides, and that is not a gap here -- it is a scope limit worth stating.**
        ! `v` is 60 elements with no `threads=` and no floor override, so both engines refuse a team
        ! (32768 rows in Fortran, 8192 in C++) and this arm compares serial against serial. It is a
        ! correctness test and must not be cited as evidence about threading: that is
        ! `test_runs_threaded_conformance`, which lowers both floors and asserts both counters.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_unique_count(v, cc)
        call pf_unique(v, cp)
        call pf_rank(v, cr)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_unique_count(v, fc)
        call pf_unique(v, fp)
        call pf_rank(v, fr)
        call restore_engine_default()
        call check(error, cc == fc, "unique_count: the two engines disagreed")
        if (allocated(error)) return
        call check(error, size(cp) == size(fp), "unique: the two engines returned different lengths")
        if (allocated(error)) return
        call check(error, all(cp == fp), "unique: the two engines returned different distinct values")
        if (allocated(error)) return
        call check(error, all(cr == fr), "rank: the two engines returned different ranks")
        if (allocated(error)) return
        !
        ! ---- run detection with a GROUP PREFIX SHORTER than the key list ----
        ! The only shape in which `group_keys` and `size(keys)` differ, and therefore the only one
        ! that can catch a run-detection pass which quietly used every key: with `group_nkeys=1` the
        ! rows sharing a `g1` value are one group however much `g2` differs inside it.
        call gk%clear()
        call gk%add(g1)
        call gk%add(g2)
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_argsort(gk, cperm, group_offsets=coff, group_nkeys=1)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_argsort(gk, fperm, group_offsets=foff, group_nkeys=1)
        call restore_engine_default()
        call check(error, all(cperm == fperm), &
            "grouped argsort: the two engines returned different permutations")
        if (allocated(error)) return
        call check(error, size(coff) == size(foff), &
            "grouped argsort: the two engines found different numbers of groups")
        if (allocated(error)) return
        call check(error, all(coff == foff), &
            "grouped argsort: the two engines put the group boundaries in different places")
        if (allocated(error)) return
        ! Absolute, not just an A/B: three distinct g1 values means three groups, plus the sentinel.
        ! Without this, both engines counting by all four keys would agree with each other on eight.
        call check(error, size(foff) == 4, &
            "grouping by a one-key prefix of a two-key sort must find exactly three groups")
        if (allocated(error)) return
        !
        ! ---- search: engine_search -> sort_search_position. `assume_sorted` keeps this on the
        ! binary-search path rather than letting the call sort first.
        do k = 1, size(targets)
            write (kstr, "(i0)") targets(k)
            call parquet_debug_use_fortran_sort_engine(.false.)
            call pf_lower_bound(sa, targets(k), cl, assume_sorted=.true.)
            call pf_upper_bound(sa, targets(k), ch, assume_sorted=.true.)
            call parquet_debug_use_fortran_sort_engine(.true.)
            call pf_lower_bound(sa, targets(k), fl, assume_sorted=.true.)
            call pf_upper_bound(sa, targets(k), fh, assume_sorted=.true.)
            call restore_engine_default()
            call check(error, cl == fl, &
                "lower_bound target=" // trim(kstr) // ": the two engines disagreed")
            if (allocated(error)) return
            call check(error, ch == fh, &
                "upper_bound target=" // trim(kstr) // ": the two engines disagreed")
            if (allocated(error)) return
        end do
        !
        ! ---- merge: engine_merge -> sort_merge_permutation. The two inputs share the values 10 and
        ! 40, so the tie rule (take from the FIRST input) is what the comparison actually tests.
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_merge(sa, sb, cp)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_merge(sa, sb, fp)
        call restore_engine_default()
        call check(error, size(cp) == size(fp), "merge: the two engines returned different lengths")
        if (allocated(error)) return
        call check(error, all(cp == fp), "merge: the Fortran engine disagreed with the C++ one")
        if (allocated(error)) return
        !
        ! ---- merge, with a tie the OUTPUT can distinguish ----
        ! `-0.0` and `+0.0` compare EQUAL and are different values, so this is what makes the tie
        ! rule observable at all through an API that returns values rather than indices. Taking the
        ! first range's element on a tie is `std::merge`'s stability guarantee and the reason
        ! `pf_merge` agrees with `pf_sort` of the concatenation element for element.
        za(1) = -0.0_real64
        za(2) = 3.0_real64
        zb(1) = 0.0_real64
        zb(2) = 4.0_real64
        ! **The fixture's own precondition, and it has to come first.** Everything below distinguishes
        ! the two inputs solely by the sign bit of a zero, so if this compiler has not actually given
        ! `za(1)` a negative zero then the assertions below are measuring the fixture and not the
        ! merge — and they fail with a message blaming the engine, which is exactly what happened on
        ! ifx before this check existed. Assert the instrument before trusting the reading.
        call check(error, negative_zero(za(1)) .and. .not. negative_zero(zb(1)), &
            "fixture is broken, not the merge: za(1) must be -0.0 and zb(1) must be +0.0")
        if (allocated(error)) return
        call check(error, za(1) == zb(1), "the fixture's two zeros must still COMPARE equal")
        if (allocated(error)) return
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_merge(za, zb, cz)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_merge(za, zb, fz)
        call restore_engine_default()
        call check(error, all(cz == fz), "merge of reals: the two engines disagreed on the values")
        if (allocated(error)) return
        call check(error, negative_zero(fz(1)), &
            "merge must take the FIRST range's element on a tie: -0.0 from a, not +0.0 from b")
        if (allocated(error)) return
        call check(error, negative_zero(cz(1)), &
            "the C++ engine must break the merge tie the same way, or the two have diverged")
    end subroutine test_fortran_engine_ab_operations

    !
    !> The input shapes a quicksort degenerates on, at a size where degenerating would be visible.
    !!
    !! Already-sorted, reverse-sorted, all-equal and organ-pipe are not exotic -- they are what real
    !! column data looks like -- and each is a classic O(n^2) trapdoor for a naive pivot choice. This
    !! asserts the ANSWER rather than the running time, because a correctness test cannot see a
    !! quadratic sort; what it does catch is a median-of-three or a partition that mishandles a run
    !! of equal elements, which is the same code the degenerate shapes exercise.
    subroutine test_fortran_engine_adversarial(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 2000_int64
        integer(int64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: shape_id
        character(len=16) :: label
        !
        do shape_id = 1, 4
            select case (shape_id)
            case (1)
                label = "sorted"
                do k = 1_int64, n
                    v(k) = k
                end do
            case (2)
                label = "reversed"
                do k = 1_int64, n
                    v(k) = n - k + 1_int64
                end do
            case (3)
                label = "all-equal"
                v = 7_int64
            case default
                label = "organ-pipe"
                do k = 1_int64, n
                    v(k) = min(k, n - k + 1_int64)
                end do
            end select
            ! Both fast paths would take every one of these -- they are all dense small ranges, and
            ! n clears the radix floor -- so both are declined, leaving both arms comparison sorts
            ! over the same degenerate shape. That is the comparison this test is for.
            call engine_only_introsort(.true.)
            call keys%clear()
            call keys%add(v)
            call engine_ab(error, trim(label), keys, n)
            call engine_only_introsort(.false.)
            if (allocated(error)) return
        end do
    end subroutine test_fortran_engine_adversarial

    !
    !> The radix path's key transform must not lose a value shape the comparator distinguishes.
    !!
    !! Signed zero is the sharp one: `-0.0` and `+0.0` compare EQUAL under `<`, so the answer must
    !! be file order between them, and a transform that mapped them to different images would order
    !! a pair the comparator does not. The rest are where a sign-bit transform goes wrong --
    !! infinities, both ends of int64 -- plus the tier shapes that leave the value block empty or
    !! nearly so. All are run at a size above the radix floor, which is what makes this test about
    !! the radix path at all.
    subroutine test_radix_path_value_shapes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64
        real(real64), allocatable :: vr(:)
        integer(int64), allocatable :: vi(:)
        logical, allocatable :: valid(:)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: id, inf
        logical :: desc, nf
        !
        allocate(vr(n), vi(n), valid(n))
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                !
                ! Signed zero beside ordinary values.
                valid = .true.
                do k = 1_int64, n
                    select case (int(mod(k, 4_int64)))
                    case (0)
                        vr(k) = 0.0_real64
                    case (1)
                        vr(k) = -0.0_real64
                    case (2)
                        vr(k) = real(mod(k, 7_int64), real64)
                    case default
                        vr(k) = -real(mod(k, 5_int64), real64)
                    end select
                end do
                call keys%clear()
                call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                call engine_ab(error, "radix signed zero" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
                !
                ! Infinities and NaNs beside the largest finite magnitudes.
                do k = 1_int64, n
                    select case (int(mod(k, 6_int64)))
                    case (0)
                        vr(k) = ieee_value(1.0_real64, ieee_positive_inf)
                    case (1)
                        vr(k) = ieee_value(1.0_real64, ieee_negative_inf)
                    case (2)
                        vr(k) = ieee_value(1.0_real64, ieee_quiet_nan)
                    case (3)
                        vr(k) = huge(1.0_real64)
                    case (4)
                        vr(k) = -huge(1.0_real64)
                    case default
                        vr(k) = real(k, real64) * 1.0e-9_real64
                    end select
                end do
                call keys%clear()
                call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                call engine_ab(error, "radix inf/nan" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
                !
                ! Both ends of int64, where flipping the sign bit is the whole transform. The range
                ! spans the type, so the counting path declines and this really is the radix path.
                do k = 1_int64, n
                    select case (int(mod(k, 5_int64)))
                    case (0)
                        vi(k) = huge(0_int64)
                    case (1)
                        vi(k) = -huge(0_int64) - 1_int64
                    case (2)
                        vi(k) = 0_int64
                    case (3)
                        vi(k) = -1_int64
                    case default
                        vi(k) = k - n / 2_int64
                    end select
                end do
                call keys%clear()
                call keys%add(vi, descending=desc, nulls_first=nf, is_valid=valid)
                call engine_ab(error, "radix int64 extremes" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
                !
                ! Every row null: the value block is empty and the answer is entirely tier
                ! placement. Then one valid row among nulls, which is the same code path with a
                ! single-element value block.
                valid = .false.
                vr = 3.25_real64
                call keys%clear()
                call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                call engine_ab(error, "radix all null" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
                !
                valid(n / 2_int64) = .true.
                call keys%clear()
                call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                call engine_ab(error, "radix one valid" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_radix_path_value_shapes

    !
    !> The radix path sorts strings on a fixed-length PREFIX, so the refine pass is what settles
    !! everything past it -- and these are the shapes that reach it.
    !!
    !! Rows sharing a prefix longer than the window can only be ordered by the refine pass, so a
    !! missing or broken one shows up here and nowhere else. The rest are where the zero padding has
    !! to agree with `compare_bytes`: lengths on either side of the window and exactly on it, an
    !! empty string, an embedded NUL just past the window (a real byte that pads look like), and
    !! bytes above 127, which must sort HIGH because the comparison is unsigned.
    !!
    !! Cases 8-11 are the ones the refine pass may NOT skip even though every row fits the window:
    !! `""`/`char(0)` and `"a"`/`"a"//char(0)`/`"a"//char(0)//char(0)` are distinct strings sharing
    !! one zero-padded image, so a run of them is ordered by LENGTH and the radix leaves them in file
    !! order. The NUL at case 5 does not reach this — it sits past the window, where the refine pass
    !! runs anyway. See `feature_risks.md` Risk-89.
    subroutine test_radix_path_string_shapes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64
        type(parquet_string_column) :: col
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: id, inf
        logical :: desc, nf
        character(len=:), allocatable :: s
        character(len=8) :: num
        !
        call col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k * 7_int64, 97_int64))
            select case (int(mod(k, 12_int64)))
            case (0)
                s = "commonpre" // num          ! differs only AFTER the prefix window
            case (1)
                s = "commonpre" // num // "tl"  ! ... and again at a different length
            case (2)
                s = ""                          ! empty
            case (3)
                s = "commonp"                   ! shorter than the window
            case (4)
                s = "commonpr"                  ! exactly the window
            case (5)
                s = "commonpr" // char(0) // num ! an embedded NUL just past it
            case (6)
                s = char(200) // char(255) // num ! bytes above 127 sort HIGH
            case (8)
                s = "a"                         ! these four share one zero-padded image
            case (9)
                s = "a" // char(0)              ! ... at length 2 -- ordered AFTER "a"
            case (10)
                s = "a" // char(0) // char(0)   ! ... and at length 3
            case (11)
                s = char(0)                     ! ... as do "" (case 2) and this, at 0 and 1
            case default
                s = num
            end select
            call col%append_string(s)
        end do
        !
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(col, descending=desc, nulls_first=nf)
                call engine_ab(error, "radix strings" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_radix_path_string_shapes

    !
    !> The multi-key radix runs one stable pass per key from the LAST key to the first, so what it
    !! has to get right is that every key carries its OWN tiers and its own flags.
    !!
    !! The fixture is built so the later keys are actually reached: the primary is low-cardinality
    !! (16 distinct values over 1024 rows), which is the shape `feature_risks.md` Risk-35 is about —
    !! a multi-key test whose primary key has no ties never consults the second key and silently
    !! measures a single-key sort. Both keys carry nulls, the real key carries NaNs, and
    !! `descending`/`nulls_first` are swept INDEPENDENTLY per key, because a pass that applied one
    !! key's flags to another would agree with C++ on every fixture where the two happen to match.
    !!
    !! The last two assertions are the negative control and the decline. Two numeric keys above the
    !! floor must take the radix, which leaves the insertion tracker at zero; adding a STRING key
    !! must make `sort_radix_candidate` refuse — a string's image is only its first 8 bytes and the
    !! multi-key path has no way to repair a shared-prefix run — so the introsort runs and the
    !! tracker records a shift. Without the second half, a candidate that wrongly accepted a string
    !! key would return a quietly wrong permutation on any column with a shared prefix.
    subroutine test_radix_path_multi_key(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 1024_int64
        integer(int64) :: vi(n)
        real(real64) :: vr(n)
        logical :: valid_i(n), valid_r(n)
        type(parquet_string_column) :: col, long_col, plain_col
        type(pf_sort_keys) :: keys
        integer(int64) :: k, shift_num, shift_str, shift_long
        integer(int64), allocatable :: perm(:)
        integer :: a, b
        logical :: d1, n1, d2, n2
        character(len=8) :: num
        !
        do k = 1_int64, n
            vi(k) = mod(k * 7_int64, 16_int64)          ! 16 distinct values: long tie runs
            vr(k) = real(mod(k * 31_int64, 1009_int64), real64)
            if (mod(k, 37_int64) == 0_int64) vr(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            valid_i(k) = (mod(k, 23_int64) /= 0_int64)
            valid_r(k) = (mod(k, 29_int64) /= 0_int64)
        end do
        ! A string key with everything the MSD pass has to handle: a shared prefix that forces the
        ! recursion, values of differing length under one prefix, an embedded NUL, an empty value,
        ! and heavy duplication so that the LATER keys are reached through it.
        call col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k, 11_int64))
            select case (int(mod(k, 9_int64)))
            case (0)
                call col%append_string("shared" // num)
            case (1)
                call col%append_string("shared" // num // "x")
            case (2)
                call col%append_string("shared")
            case (3)
                call col%append_string("")
            case (4)
                call col%append_string("sh" // char(0) // num)
            case (5)
                ! High-cardinality under one prefix: 26^3 possible values over 1024 rows, so the
                ! MSD narrows to runs of one or two that are still DISTINCT and therefore reach the
                ! tail with real work to do. Every other shape here collapses to byte-identical
                ! groups, which return before the tail and leave it untested.
                call col%append_string("pfx" // achar(97 + int(mod(k, 26_int64))) // &
                    achar(97 + int(mod(k / 26_int64, 26_int64))) // &
                    achar(97 + int(mod(k / 676_int64, 26_int64))))
            case (6)
                call col%append_string("q")
            case (7)
                call col%append_string("q" // char(0))  ! same bytes as "q", one longer
            case default
                ! NULLS, and they are what makes `nulls_first` observable on a string key at all.
                ! Without them the value block always starts at row 1 and a pass that located it
                ! without consulting `nulls_first` would sort the right range by accident.
                call col%append_null()
            end select
        end do
        !
        ! The same shapes MINUS the nulls, so the string pass's no-tier branch is reached with an
        ! equality check on it rather than only a timing one. Shared prefixes and duplicates kept,
        ! since those are what make the later keys reachable through this one.
        call plain_col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k, 11_int64))
            select case (int(mod(k, 4_int64)))
            case (0)
                call plain_col%append_string("shared" // num)
            case (1)
                call plain_col%append_string("shared")
            case (2)
                call plain_col%append_string("")
            case default
                call plain_col%append_string("q" // num)
            end select
        end do
        !
        ! The same shapes, but past SORT_RADIX_MAX_BYTE: the one string a multi-key sort refuses.
        call long_col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k, 11_int64))
            call long_col%append_string(repeat("p", 70) // num)
        end do
        !
        ! Each key's own flags, swept independently of the other's.
        do a = 0, 3
            do b = 0, 3
                d1 = (mod(a, 2) == 1)
                n1 = (a / 2 == 1)
                d2 = (mod(b, 2) == 1)
                n2 = (b / 2 == 1)
                call keys%clear()
                call keys%add(vi, descending=d1, nulls_first=n1, is_valid=valid_i)
                call keys%add(vr, descending=d2, nulls_first=n2, is_valid=valid_r)
                call engine_ab(error, "multi radix" // radix_tag(d1, n1) // radix_tag(d2, n2), keys, n)
                if (allocated(error)) return
                !
                ! The same sweep with the STRING key in the middle, so its own flags are exercised
                ! against a numeric key on each side of it -- a string pass that ignored
                ! `descending` or `nulls_first` would still agree with C++ on every fixture where
                ! those happened to match its neighbours'.
                call keys%clear()
                call keys%add(vi, descending=d1, nulls_first=n1, is_valid=valid_i)
                call keys%add(col, descending=d2, nulls_first=n2)
                call keys%add(vr, descending=d1, nulls_first=n2, is_valid=valid_r)
                call engine_ab(error, "multi radix str" // radix_tag(d1, n1) // radix_tag(d2, n2), keys, n)
                if (allocated(error)) return
            end do
        end do
        !
        ! A string key FIRST, so the chain's most significant pass is the MSD one.
        call keys%clear()
        call keys%add(col)
        call keys%add(vi, descending=.true., is_valid=valid_i)
        call engine_ab(error, "multi radix str first", keys, n)
        if (allocated(error)) return
        !
        ! Three keys, to prove the chain composes rather than only handling a pair.
        call keys%clear()
        call keys%add(vi, is_valid=valid_i)
        call keys%add(vr, descending=.true., is_valid=valid_r)
        call keys%add(vi, descending=.true.)
        call engine_ab(error, "multi radix three keys", keys, n)
        if (allocated(error)) return
        !
        ! **A real key whose NaNs are its ONLY tier -- no validity array.** Every other real key in
        ! this test carries `is_valid`, which hides a whole class of defect: the multi-key pass asks
        ! `key_has_tiers` once per key to decide whether the tier work can be skipped, and that
        ! predicate is a second statement of the rule `sort_tier_of` owns. Dropping its
        ! `family == SK_REAL` half leaves it answering correctly for every key that also has nulls,
        ! so a fixture that always pairs the two cannot see the drift -- confirmed by mutation, where
        ! exactly that edit survived the whole suite. Here the NaNs must be placed as a tier with
        ! nothing else to force it.
        do a = 0, 3
            d1 = (mod(a, 2) == 1)
            n1 = (a / 2 == 1)
            call keys%clear()
            call keys%add(vi, is_valid=valid_i)
            call keys%add(vr, descending=d1, nulls_first=n1)
            call engine_ab(error, "multi radix nan-only" // radix_tag(d1, n1), keys, n)
            if (allocated(error)) return
        end do
        !
        ! **A string key with no nulls at all**, for the same reason one level along: the string
        ! pass skips its tier count on `key_has_tiers` and takes the value block to be the whole
        ! range, which is a different code path from the one `col` (which has nulls) exercises, and
        ! it has to place the block correctly under `nulls_first` with no tiers to place it against.
        do a = 0, 3
            d1 = (mod(a, 2) == 1)
            n1 = (a / 2 == 1)
            call keys%clear()
            call keys%add(plain_col, descending=d1, nulls_first=n1)
            call keys%add(vi, is_valid=valid_i)
            call engine_ab(error, "multi radix str no-nulls" // radix_tag(d1, n1), keys, n)
            if (allocated(error)) return
        end do
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call keys%clear()
        call keys%add(vi)
        call keys%add(vr)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, perm)
        shift_num = parquet_debug_sort_max_insertion_shift()
        !
        call keys%add(col)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, perm)
        shift_str = parquet_debug_sort_max_insertion_shift()
        !
        call keys%clear()
        call keys%add(vi)
        call keys%add(long_col)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, perm)
        shift_long = parquet_debug_sort_max_insertion_shift()
        call parquet_debug_set_sort_track_shift(.false.)
        call restore_engine_default()
        !
        call check(error, shift_num == 0_int64, &
            "two numeric keys above the floor should have taken the multi-key radix, but the insertion pass ran")
        if (allocated(error)) return
        call check(error, shift_str == 0_int64, &
            "a short string key should now be taken by the multi-key radix, but the introsort ran")
        if (allocated(error)) return
        ! The one string a multi-key sort still refuses. This is a COST guard, not a correctness one:
        ! the MSD tail is a stable insertion sort, whose O(m^2) is only sound while the recursion
        ! ends by exhausting bytes rather than by hitting SORT_RADIX_MAX_BYTE. If that tail is ever
        ! replaced by something with a better worst case, this becomes an equality test rather than
        ! a deletion -- what it asserts today is that the refusal is real, and what it should assert
        ! afterwards is that the replacement kept the answer.
        call check(error, shift_long > 0_int64, &
            "a string key longer than SORT_RADIX_MAX_BYTE must make the multi-key radix decline")
    end subroutine test_radix_path_multi_key

    !
    !> An integer key whose value range spans under 2^32 is imaged by SUBTRACTING its minimum rather
    !! than by flipping its sign bit, which leaves the top four bytes constant and skips four passes.
    !!
    !! **Every equality assertion here would pass against a build where the bias never fires**, since
    !! both images are order-preserving and the permutation is identical either way. That is what the
    !! pass counter is for, and the counter assertions at the end are the half of this test that
    !! cannot be satisfied by accident. `feature_risks.md` Risk-75's shape: a hook that reports what
    !! changed, because nothing about the answer can.
    !!
    !! Five fixtures, spanning the shapes the range test has to get right:
    !!
    !! * both ends non-negative, span just under 2^32 -- the bias fires;
    !! * `lo < 0 <= hi`, span still under 2^32 -- the second branch of `sort_span_under_2p32`,
    !!   reachable no other way;
    !! * a span of exactly 2^32 -- the boundary, one past where the bias applies;
    !! * both int64 extremes, and a span that overflows int64 from a minimum that is not `-2^63` --
    !!   the two widest shapes, which must decline.
    !!
    !! **What this test does NOT cover, established by mutation and worth knowing before anyone
    !! writes a sixth fixture to try.** A WRONG span decision cannot be detected here, and not for
    !! want of a better fixture: `v - vmin` under wrapping arithmetic is exactly unsigned subtraction
    !! mod 2^64, and every int64 range fits in 2^64, so the biased image stays order-preserving for
    !! any minimum whatsoever. Forcing `narrow = .true.` everywhere, and replacing the range test
    !! with the naive `hi - lo < 2^32` that overflows on both wide fixtures, each leave every
    !! permutation here bit-identical and every pass count unchanged. What those edits really cost is
    !! undefined behaviour (signed overflow) and the four skipped passes -- neither observable in an
    !! answer. `sort_span_under_2p32` says so at its own head; it is reviewed, not tested.
    !!
    !! The near-miss worth recording: at `lo = -2^63` the bias and the sign flip are the SAME
    !! transform, since `v - (-2^63)` is `v + 2^63` is `ieor(v, SORT_SIGN_BIT)`. So the both-extremes
    !! fixture is degenerate for this purpose even in principle, which is why `overflow_span` exists
    !! beside it with a minimum of `-2^62`.
    !!
    !! Nulls are present in half the sweeps because the range is over VALID rows only: a null row's
    !! key slot holds whatever the buffer contained, so a scan that failed to skip nulls could widen
    !! the range past the test and silently lose the optimisation -- or, worse, narrow it wrongly.
    subroutine test_radix_path_narrow_integer(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64
        integer(int64), parameter :: TWO32 = 4294967296_int64
        integer(int64), parameter :: MIN62 = -4611686018427387904_int64 !! -2^62: not the sign flip's fixed point.
        integer(int64), parameter :: BASE40 = 1099511627776_int64 !! 2^40: base of a band far from zero.
        integer(int64), parameter :: OFF32 = 3221225472_int64
        !! 3*2^30, so the far band STRADDLES 2^32. Load-bearing: at a byte-aligned base the unbiased
        !! image leaves byte 4 constant too and runs the same four passes, so the fixture cannot tell
        !! a biased build from an unbiased one. Straddling makes byte 4 vary without the bias.
        integer(int64) :: narrow_pos(n), narrow_signed(n), wide(n), extremes(n), overflow_span(n)
        integer(int64) :: farband(n), smallband(n)
        logical :: valid(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, spread, passes_narrow, passes_wide, passes_extremes, passes_overflow
        integer(int64) :: passes_farband, passes_off, passes_counting
        integer :: a
        logical :: desc, nf
        !
        do k = 1_int64, n
            ! A spread that reaches every one of the low four bytes, so "four passes ran" really
            ! means "the top four were skipped" and not "the data was degenerate".
            spread = mod(k * 2654435761_int64, TWO32 - 1_int64)
            narrow_pos(k) = spread                                   ! [0, 2^32-2]: span < 2^32
            narrow_signed(k) = spread - (TWO32 / 2_int64 - 1_int64)  ! straddles 0, span still < 2^32
            wide(k) = spread                                         ! ends pinned below to span 2^32
            extremes(k) = spread - TWO32 / 2_int64
            ! Alternating halves of a range whose SPAN overflows int64 while its minimum is not
            ! -2^63 -- the one shape on which a wrongly-applied bias visibly wraps and reorders.
            if (mod(k, 2_int64) == 0_int64) then
                overflow_span(k) = MIN62 + spread
            else
                overflow_span(k) = huge(0_int64) - spread
            end if
            ! A narrow band nowhere near zero: an int64 column of epoch nanoseconds or identifiers,
            ! and the shape that proves the range is the range rather than a distance from zero.
            farband(k) = BASE40 + OFF32 + mod(spread, TWO32 / 2_int64)
            ! Span ~1000 and straddling zero: narrow enough that the COUNTING path claims it while
            ! that knob is on, which is what lets switching the knob off mean something.
            smallband(k) = mod(spread, 1001_int64) - 500_int64
            valid(k) = (mod(k, 17_int64) /= 0_int64)
        end do
        ! Pin the ends so each fixture's span is exactly what it claims, whatever the spread did.
        narrow_pos(1) = 0_int64
        narrow_pos(2) = TWO32 - 1_int64                     ! span 2^32-1: the largest narrow span
        narrow_signed(1) = -(TWO32 / 2_int64)
        narrow_signed(2) = TWO32 / 2_int64 - 1_int64        ! span 2^32-1, straddling zero
        wide(1) = 0_int64
        wide(2) = TWO32                                     ! span exactly 2^32: one too wide
        extremes(1) = -huge(0_int64) - 1_int64
        extremes(2) = huge(0_int64)                         ! naive `hi - lo` overflows to -1 here
        overflow_span(1) = MIN62                            ! NOT the sign flip's fixed point
        overflow_span(2) = huge(0_int64)                    ! span ~1.15e19: overflows int64
        farband(1) = BASE40 + OFF32
        farband(2) = BASE40 + OFF32 + TWO32 / 2_int64 - 1_int64 ! span 2^31: narrow, far from zero
        !
        do a = 0, 3
            desc = (mod(a, 2) == 1)
            nf = (a / 2 == 1)
            call keys%clear()
            call keys%add(narrow_pos, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix narrow int" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            call keys%clear()
            call keys%add(narrow_signed, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix narrow signed" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            ! The same, with nulls, so the range really is the range over valid rows.
            call keys%clear()
            call keys%add(narrow_signed, descending=desc, nulls_first=nf, is_valid=valid)
            call engine_ab(error, "radix narrow nulls" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            call keys%clear()
            call keys%add(wide, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix wide int" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            call keys%clear()
            call keys%add(extremes, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix int64 extremes" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            call keys%clear()
            call keys%add(overflow_span, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix overflow span" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
            !
            call keys%clear()
            call keys%add(farband, descending=desc, nulls_first=nf)
            call engine_ab(error, "radix far band" // radix_tag(desc, nf), keys, n)
            if (allocated(error)) return
        end do
        !
        ! The non-vacuity half. A biased image occupies four bytes, so exactly four passes run; the
        ! unbiased images below span more than that and must run strictly more.
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(narrow_signed, perm)
        passes_narrow = parquet_debug_sort_radix_passes()
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(wide, perm)
        passes_wide = parquet_debug_sort_radix_passes()
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(extremes, perm)
        passes_extremes = parquet_debug_sort_radix_passes()
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(overflow_span, perm)
        passes_overflow = parquet_debug_sort_radix_passes()
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(farband, perm)
        passes_farband = parquet_debug_sort_radix_passes()
        call restore_engine_default()
        !
        call check(error, passes_narrow == 4_int64, &
            "a key spanning under 2^32 should have been biased down to four radix passes")
        if (allocated(error)) return
        call check(error, passes_wide > passes_narrow, &
            "a key spanning exactly 2^32 must NOT be biased, so it cannot run as few passes")
        if (allocated(error)) return
        call check(error, passes_extremes > passes_narrow, &
            "a key at both int64 extremes must NOT be biased -- its span overflows a naive test")
        if (allocated(error)) return
        call check(error, passes_overflow > passes_narrow, &
            "a key whose span overflows int64 must NOT be biased")
        if (allocated(error)) return
        ! The range must be a SPAN, not a distance from zero. Seeding the min/max scan at 0 instead
        ! of at the first value leaves every fixture above unchanged -- all of them straddle or touch
        ! zero -- while silently declaring this one wide and losing the optimisation on exactly the
        ! shape S4 exists for: an int64 column of identifiers, epoch times or counts.
        call check(error, passes_farband == 4_int64, &
            "a narrow band far from zero should still be biased down to four radix passes")
        if (allocated(error)) return
        !
        ! **The bias must not depend on `parquet_set_sort_counting_path`.** The range it needs is one
        ! `sort_counting_candidate` has usually just computed, and reusing it would have been free --
        ! and would have made a knob named for one fast path silently govern a second, unrelated one,
        ! so that a user disabling the counting path lost a large speedup with no indication and
        ! `doc/pages/operating/settings.md` described that knob incorrectly. The radix path scans for
        ! itself instead. This is what stops that reuse being reintroduced as an optimisation.
        !
        ! **`smallband` is what makes this non-vacuous, and the obvious fixture is not.** Every other
        ! column here spans 2^32 or more, so the counting path declines it whichever way the knob is
        ! set and switching the knob proves nothing. This one has a span of ~1000, so the knob really
        ! does decide which path runs -- and it straddles zero, so a build that failed to bias would
        ! be visible: the sign-flipped image of a straddling range differs in every byte, running all
        ! eight passes, where the biased image of `[0, 1000]` runs two.
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(smallband, perm)
        passes_counting = parquet_debug_sort_radix_passes()
        call parquet_set_sort_counting_path(.false.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(smallband, perm)
        passes_off = parquet_debug_sort_radix_passes()
        call parquet_set_sort_counting_path(.true.)
        call restore_engine_default()
        !
        call check(error, passes_counting == 0_int64, &
            "a span-1000 key should have gone to the counting path, so no radix pass should have run")
        if (allocated(error)) return
        call check(error, passes_off == 2_int64, &
            "with the counting path off the radix must still bias this key down to two passes")
    end subroutine test_radix_path_narrow_integer

    !
    !> The refine pass continues the radix a BYTE at a time past its 8-byte window, so these are the
    !! shapes that reach the recursion rather than the first window.
    !!
    !! Each case targets one clause of `sort_radix_refine_run`, and several would pass against a
    !! plainly broken one — which is why they run through `engine_ab` against C++ rather than against
    !! the introsort, and why the shapes are mixed into ONE column so that runs actually interleave:
    !!
    !! * a long shared prefix with varying tails — the recursion itself, and the bucket split;
    !! * the SAME prefix at two different lengths — the W12 hazard one level down, where a row that
    !!   has ended pads to 0 and must sort before one that continues;
    !! * an embedded NUL deep in the value, which is a real byte that looks exactly like that pad;
    !! * a run of identical long values — the single-bucket shortcut and the `minlen == maxlen`
    !!   early return, which is the case the whole item exists for;
    !! * a prefix longer than `SORT_RADIX_MAX_BYTE`, so the recursion hits its depth cap and the
    !!   introsort finishes — the one path where the radix deliberately gives up mid-string.
    !!
    !! **The `AAAAAAAAAAAAx`/`...y` pair is sized deliberately and the size is the point.** Reversing
    !! the scatter — dropping the stability the row-index tiebreaker depends on — survived every
    !! other shape here, because their byte-identical groups are all smaller than
    !! `SORT_INSERTION_CUTOFF` and so end at the introsort, which re-sorts them correctly and hides
    !! the damage. That is `feature_risks.md` Risk-86 one level down: a complete sort underneath
    !! makes everything above it invisible. This pair gives ~455 byte-identical rows per bucket,
    !! comfortably over the cutoff, in a run that really splits — so they leave through the
    !! `minlen == maxlen` return in whatever order the scatter left them, and an unstable scatter
    !! fails. Do not shrink `n` or add cases without checking this group stays over the cutoff.
    subroutine test_radix_path_deep_strings(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64
        character(len=*), parameter :: p40 = "0123456789012345678901234567890123456789"
        character(len=*), parameter :: p80 = p40 // p40
        type(parquet_string_column) :: col
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: id, inf
        logical :: desc, nf
        character(len=:), allocatable :: s
        character(len=8) :: num
        !
        call col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k * 7_int64, 97_int64))
            select case (int(mod(k, 9_int64)))
            case (0)
                s = p40 // num                       ! deep recursion, varying tail
            case (1)
                s = p40 // num // "z"                ! same tail, one byte longer
            case (2)
                s = p40                              ! a strict prefix of both above
            case (3)
                s = "identical-long-value-24"        ! all-equal run: the single-bucket shortcut
            case (4)
                s = p40(1:20) // char(0) // num      ! a NUL deep inside, past the window
            case (5)
                s = p40(1:20)                        ! ends exactly where that NUL sits
            case (6)
                s = "AAAAAAAAAAAAx"                  ! see below: a LARGE byte-identical group...
            case (7)
                s = "AAAAAAAAAAAAy"                  ! ...that its sibling splits away from
            case default
                s = p80 // num                       ! past SORT_RADIX_MAX_BYTE: the depth cap
            end select
            call col%append_string(s)
        end do
        !
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(col, descending=desc, nulls_first=nf)
                call engine_ab(error, "radix deep strings" // radix_tag(desc, nf), keys, n)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_radix_path_deep_strings

    !
    !> A radix path that cannot get its scratch must DECLINE, not abort — and still be right.
    !!
    !! It needs about 32 bytes per row where the comparison sort needs none (measured: 32.03 B/row
    !! at 5 M rows, 32.01 at 10 M, 31.92 at 20 M), so an ordinary sort of ordinary data can fail
    !! purely for being large. Both `allocate`s carry `stat=` and return `perm` untouched on
    !! failure, whereupon the caller runs the comparison sort. The answer is identical either way,
    !! which is exactly why this needs the insertion tracker as well as an equality check: without
    !! it, a fallback that never engaged and a fallback that engaged correctly are the same test.
    !!
    !! Provoking a real allocation failure would need a machine-sized array and, under Linux's
    !! default overcommit policy, would not report one anyway — hence the hook.
    subroutine test_radix_path_alloc_fallback(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64
        real(real64), allocatable :: v(:)
        integer(int64), allocatable :: perm_scratch(:)
        type(parquet_string_column) :: strs
        type(pf_sort_keys) :: keys
        integer(int64) :: k, shift_ok, shift_failed
        !
        allocate(v(n))
        do k = 1_int64, n
            v(k) = real(mod(k * 7919_int64, 4001_int64), real64)
        end do
        call keys%add(v)
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v, perm_scratch)
        shift_ok = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_debug_set_sort_radix_fail_alloc(1)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v, perm_scratch)
        shift_failed = parquet_debug_sort_max_insertion_shift()
        call parquet_debug_set_sort_radix_fail_alloc(0)
        call parquet_debug_set_sort_track_shift(.false.)
        call restore_engine_default()
        !
        call check(error, shift_ok == 0_int64, &
            "the control run should have taken the radix path, but the insertion pass ran")
        if (allocated(error)) return
        call check(error, shift_failed > 0_int64, &
            "a failed radix allocation did not fall back to the comparison sort")
        if (allocated(error)) return
        !
        ! And the answer is still right. Forced on for the whole A/B, so both Fortran arms would
        ! have to be wrong in the same way to agree with C++.
        call parquet_debug_set_sort_radix_fail_alloc(1)
        call engine_ab(error, "radix alloc fallback", keys, n)
        if (allocated(error)) then
            call parquet_debug_set_sort_radix_fail_alloc(0)
            return
        end if
        !
        ! The deep string refine allocates a SECOND buffer, on its own, only when a shared-prefix run
        ! actually splits — so it has its own fallback and the hook covers that one too. A string
        ! fixture is the only way in: the run above is a real key and never reaches it.
        call parquet_debug_set_sort_radix_fail_alloc(2)
        call keys%clear()
        call str_prefix_column(strs, n)
        call keys%add(strs)
        call engine_ab(error, "radix deep alloc fallback", keys, n)
        call parquet_debug_set_sort_radix_fail_alloc(0)
        if (allocated(error)) return
        call engine_ab(error, "radix deep alloc control", keys, n)
        if (allocated(error)) return
        !
        ! The MULTI-key driver allocates its own scratch and so has its own fallback. It shares the
        ! `1` selector with the single-key path because the two are alternatives rather than a
        ! sequence — only one of them runs for a given key list — so no third value is needed.
        call keys%clear()
        call keys%add(v)
        call keys%add(v, descending=.true.)
        call parquet_debug_set_sort_radix_fail_alloc(1)
        call engine_ab(error, "multi radix alloc fallback", keys, n)
        call parquet_debug_set_sort_radix_fail_alloc(0)
    end subroutine test_radix_path_alloc_fallback

    !
    !> The heapsort fallback, forced, must produce the same permutation as the quicksort path.
    !!
    !! **It is unreachable without the hook.** Median-of-three pivoting plus a limit of
    !! `2*floor(log2(n))` means ordinary data never approaches the depth at which the fallback fires,
    !! so every mutation to `sort_heapsort`/`sort_sift_down` would survive the whole suite. Forcing
    !! the limit to zero makes the very first oversized range heapsort instead.
    !!
    !! Both halves of the A/B run under the forced limit, and the C++ engine ignores it entirely --
    !! so the C++ arm is an unchanged reference and any difference is the fallback's.
    subroutine test_fortran_engine_heapsort_fallback(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 500_int64
        real(real64) :: v(n)
        logical :: valid(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: id
        logical :: desc
        !
        do k = 1_int64, n
            v(k) = real(mod(k * 31_int64, 97_int64), real64)
            if (mod(k, 29_int64) == 0_int64) v(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            valid(k) = (mod(k, 11_int64) /= 0_int64)
        end do
        !
        ! The depth limit is an introsort concept, so the radix path has to be declined or it takes
        ! the sort and the forced fallback is never entered at all.
        call engine_only_introsort(.true.)
        !
        ! The negative control: the same fixture through the ordinary quicksort path first. If the
        ! forced-limit run below were silently taking that same path, this pair would still pass --
        ! but `test_fortran_engine_depth_limit_bites` is what rules that out.
        call keys%clear()
        call keys%add(v, is_valid=valid)
        call engine_ab(error, "heap control", keys, n)
        if (allocated(error)) then
            call engine_only_introsort(.false.)
            return
        end if
        !
        call parquet_debug_set_sort_depth_limit(0)
        do id = 0, 1
            desc = (id == 1)
            call keys%clear()
            call keys%add(v, descending=desc, is_valid=valid)
            call engine_ab(error, "heapsort desc=" // merge("T", "F", desc), keys, n)
            if (allocated(error)) then
                call parquet_debug_set_sort_depth_limit(-1)
                call engine_only_introsort(.false.)
                return
            end if
        end do
        call parquet_debug_set_sort_depth_limit(-1)
        call engine_only_introsort(.false.)
    end subroutine test_fortran_engine_heapsort_fallback

    !
    ! ============================================================================================
    ! Stage 3 conformance -- the integer counting fast path
    ! ============================================================================================
    !
    ! The counting path is a SECOND code path to the same answer, and it fails fast rather than slow:
    ! it performs zero comparisons by construction, so a fixture that reaches it exercises none of
    ! the comparator. That is the shape behind `feature_risks.md` Risk-35, where a comparator
    ! mutation survived twice because the fixture took this path and called the comparator zero
    ! times. So every fixture here is run BOTH ways and the two are required to agree.
    !
    !> Runs one key set three ways — C++, Fortran counting, Fortran comparator — and requires agreement.
    !!
    !! **The shift tracker is what stops this being vacuous.** The A/B turns `parquet_set_sort_counting_path`
    !! off for the second Fortran arm, and if that setting did nothing, both arms would be the same
    !! path and the comparison would hold trivially — passing while testing nothing. The counting path
    !! never calls the introsort, so it must leave the insertion-shift tracker at zero, while the
    !! comparator path on a scrambled fixture must move something. Asserting both is what proves the
    !! two arms really diverged.
    subroutine counting_ab(error, label, keys, n, expect_counting, scrambled)
        type(error_type), allocatable, intent(inout) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: label                 !! names the fixture in every message.
        class(pf_sort_keys), intent(in) :: keys               !! the key set to sort by.
        integer(int64), intent(in) :: n                       !! rows.
        logical, intent(in) :: expect_counting                !! the counting path should accept this key.
        logical, intent(in) :: scrambled
        !! .true. when the fixture is disordered enough that a comparison sort must move something.
        !! An all-null or single-valued key is NOT: every row ties, so `sort_row_less` falls back to
        !! the row index, the input is already in order and the insertion pass correctly shifts
        !! nothing. Asserting otherwise there would assert a property of the fixture, not the engine.
        integer(int64), allocatable :: pc(:)    !! the C++ engine's permutation.
        integer(int64), allocatable :: p_on(:)  !! Fortran, counting path available.
        integer(int64), allocatable :: p_off(:) !! Fortran, counting path forced off.
        integer(int64) :: shift_on, shift_off   !! insertion-pass movement on each Fortran arm.
        !
        ! The RADIX path is declined for the whole comparison, not because it would answer wrongly
        ! but because it would answer FIRST: it accepts every single-key sort above its floor, so
        ! with it live the "counting off" arm would be the radix rather than the comparator and the
        ! A/B this whole subroutine exists to perform would compare two fast paths.
        call parquet_debug_set_sort_radix_min_rows(huge(0_int64))
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_argsort(keys, pc)
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_set_sort_counting_path(.true.)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, p_on)
        shift_on = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_set_sort_counting_path(.false.)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, p_off)
        shift_off = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_set_sort_counting_path(.true.)
        call parquet_debug_set_sort_track_shift(.false.)
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        call restore_engine_default()
        !
        call check(error, all(p_on == pc), label // ": the counting path disagrees with the C++ engine")
        if (allocated(error)) return
        call check(error, all(p_off == pc), label // ": the comparator path disagrees with the C++ engine")
        if (allocated(error)) return
        !
        if (expect_counting) then
            call check(error, shift_on == 0_int64, &
                label // ": the counting path was expected to apply here and did not")
            if (allocated(error)) return
            if (scrambled) then
                call check(error, shift_off > 0_int64, &
                    label // ": turning the counting path off changed nothing, so the A/B is vacuous")
            end if
        else if (scrambled) then
            call check(error, shift_on > 0_int64, &
                label // ": the counting path was expected to decline this key and did not")
        end if
    end subroutine counting_ab

    !
    !> Every shape the counting path has to handle, against the comparator path and against C++.
    !!
    !! The fixtures are chosen against the *clauses* of `sort_counting_candidate`/
    !! `sort_counting_permutation` rather than at random, since each clause encodes a correctness
    !! fact that ordinary low-cardinality data cannot exercise: a null-bearing key (accepted, and the
    !! range scan must skip the nulls' garbage value slots), an all-null key (accepted, `lo == hi == 0`,
    !! identity), a single-valued key (one bucket), a range spanning zero and a wholly negative range
    !! (the bucket index is an offset from `lo`, not an absolute value), and a range past the bucket
    !! limit (declined). Every one is run under both `descending` and `nulls_first`, because
    !! `value_base` and `null_pos` must not mention `descending` and no ascending test can see it.
    subroutine test_counting_path_matches_comparator(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 500_int64
        integer(int64) :: v(n)
        logical :: valid(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: fixture, id, inf
        logical :: desc, nf, use_nulls, expect_counting, scrambled
        character(len=24) :: name
        character(len=:), allocatable :: tag
        !
        do fixture = 1, 7
            use_nulls = .false.
            expect_counting = .true.
            scrambled = .true.
            valid = .true.
            select case (fixture)
            case (1)
                name = "low-card"
                do k = 1_int64, n
                    v(k) = mod(k * 5_int64, 8_int64)
                end do
            case (2)
                name = "low-card+nulls"
                use_nulls = .true.
                do k = 1_int64, n
                    v(k) = mod(k * 5_int64, 8_int64)
                    valid(k) = (mod(k, 7_int64) /= 0_int64)
                end do
                ! The null rows' value slots deliberately hold ordinary in-range values here. A range
                ! scan that forgot to skip them would still produce a correct answer on THIS fixture,
                ! which is why the extreme-value test below exists as well.
            case (3)
                name = "all-null"
                use_nulls = .true.
                scrambled = .false.
                v = 3_int64
                valid = .false.
            case (4)
                name = "single-value"
                scrambled = .false.
                v = 42_int64
            case (5)
                ! **The range is deliberately narrow relative to `n`, and that is load-bearing.**
                ! The counting path is admitted only when the value range is under 0.3 * n (and the
                ! sort is serial) -- see `sort_build_permutation_impl`. This fixture's job is the
                ! SIGN CROSSING in the range scan, not a wide range, so it spans zero inside that
                ! bound: 61 distinct values against n = 500. Widening it back past 150 makes
                ! `expect_counting` false and this fixture silently stops testing the counting path
                ! at all, which is how it failed when the rule was introduced.
                name = "spans-zero"
                do k = 1_int64, n
                    v(k) = mod(k * 11_int64, 61_int64) - 30_int64
                end do
            case (6)
                name = "all-negative"
                do k = 1_int64, n
                    v(k) = -1_int64 - mod(k * 13_int64, 97_int64)
                end do
            case default
                ! **The only fixture that can see the range scan skip nulls.** One null row holds
                ! `huge(int64)`; every valid row holds 0..7. Skipping it leaves a range of 7 and the
                ! fast path applies, which is what `expect_counting` asserts. Counting it gives a
                ! range of `huge`, far past the bucket limit, and the path is declined — a
                ! performance defect with no wrong answer anywhere, so nothing else here would
                ! notice. Note this differs from the C++ engine's own reasoning: there a null row's
                ! slot holds whatever the buffer contained, whereas Fortran's extractor copies every
                ! value before applying the mask, so the hazard is a caller's real value rather than
                ! garbage. Same clause, same fix, different way in.
                name = "null-holds-extreme"
                use_nulls = .true.
                do k = 1_int64, n
                    v(k) = mod(k * 5_int64, 8_int64)
                end do
                v(3) = huge(0_int64)
                valid(3) = .false.
            end select
            !
            do id = 0, 1
                do inf = 0, 1
                    desc = (id == 1)
                    nf = (inf == 1)
                    tag = trim(name) // " desc=" // merge("T", "F", desc) // " nf=" // merge("T", "F", nf)
                    call keys%clear()
                    if (use_nulls) then
                        call keys%add(v, descending=desc, nulls_first=nf, is_valid=valid)
                    else
                        call keys%add(v, descending=desc, nulls_first=nf)
                    end if
                    call counting_ab(error, tag, keys, n, expect_counting, scrambled)
                    if (allocated(error)) return
                end do
            end do
        end do
    end subroutine test_counting_path_matches_comparator

    !
    !> The bucket limit must decline a wide range, and the answer must not change when it does.
    !!
    !! `parquet_set_sort_counting_bucket_limit` bounds the key's value RANGE, not its cardinality, so
    !! a handful of values spread far apart is declined while a million dense ones are accepted. Both
    !! directions are asserted here against one fixture, by moving the limit rather than the data —
    !! which is also what proves the setting is read at all rather than merely stored.
    subroutine test_counting_path_bucket_limit(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 400_int64
        integer(int64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        !
        do k = 1_int64, n
            v(k) = mod(k * 3_int64, 64_int64)
        end do
        call keys%add(v)
        !
        ! Range 63, comfortably inside the built-in limit of 2**22.
        call counting_ab(error, "wide-range default limit", keys, n, .true., .true.)
        if (allocated(error)) return
        !
        ! Range 63 against a limit of 8: the same key, now declined.
        call parquet_set_sort_counting_bucket_limit(8_int64)
        call counting_ab(error, "wide-range small limit", keys, n, .false., .true.)
        call parquet_set_sort_counting_bucket_limit(0_int64)
    end subroutine test_counting_path_bucket_limit

    !
    !> Values at both ends of int64: the range check must not overflow, in either direction.
    !!
    !! **This is the one place the Fortran port deliberately departs from the C++ engine.** C++ bounds
    !! the range with `(uint64_t)hi - (uint64_t)lo`, which cannot overflow; Fortran has no portable
    !! unsigned integer and signed overflow is undefined, so `sort_counting_candidate` rearranges the
    !! comparison to keep every intermediate in range. Both branches of that rearrangement are
    !! exercised here, and neither is reachable from any ordinary fixture:
    !!
    !! * a key packed against `huge(int64)`, whose range is tiny but whose `lo + limit` would
    !!   overflow — must be ACCEPTED;
    !! * a key holding values near both ends, whose true range exceeds `huge(int64)` — must be
    !!   DECLINED, and the naive `hi - lo < limit` would wrap to a negative value and accept it,
    !!   after which the bucket count is meaningless.
    subroutine test_counting_path_int64_extremes(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 200_int64
        integer(int64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        !
        ! Packed against the top of the domain: range 49, but `lo + limit` overflows.
        do k = 1_int64, n
            v(k) = huge(0_int64) - mod(k * 7_int64, 50_int64)
        end do
        call keys%clear()
        call keys%add(v)
        call counting_ab(error, "near-huge", keys, n, .true., .true.)
        if (allocated(error)) return
        !
        ! Both ends at once: the true range is about 2**64, which no int64 can hold.
        do k = 1_int64, n
            if (mod(k, 2_int64) == 0_int64) then
                v(k) = huge(0_int64) - mod(k * 7_int64, 50_int64)
            else
                v(k) = -huge(0_int64) + mod(k * 11_int64, 50_int64)
            end if
        end do
        call keys%clear()
        call keys%add(v)
        call counting_ab(error, "spans-int64", keys, n, .false., .true.)
    end subroutine test_counting_path_int64_extremes

    !> Pins the C++ engine for `test_counting_path_agrees` -- see the note above.
    subroutine cpp_test_counting_path_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_counting_path_agrees(error)
        call restore_engine_default()
    end subroutine cpp_test_counting_path_agrees


    !> Pins the C++ engine for `test_counting_path_nulls_agree` -- see the note above.
    subroutine cpp_test_counting_path_nulls_agree(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_counting_path_nulls_agree(error)
        call restore_engine_default()
    end subroutine cpp_test_counting_path_nulls_agree


    !> Pins the C++ engine for `test_partial_is_partial` -- see the note above.
    subroutine cpp_test_partial_is_partial(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_partial_is_partial(error)
        call restore_engine_default()
    end subroutine cpp_test_partial_is_partial


    !> Pins the C++ engine for `test_unique_both_paths` -- see the note above.
    subroutine cpp_test_unique_both_paths(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_unique_both_paths(error)
        call restore_engine_default()
    end subroutine cpp_test_unique_both_paths


    !> Pins the C++ engine for `test_rank_both_paths` -- see the note above.
    subroutine cpp_test_rank_both_paths(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_rank_both_paths(error)
        call restore_engine_default()
    end subroutine cpp_test_rank_both_paths


    !> Pins the C++ engine for `test_threads_really_used` -- see the note above.
    subroutine cpp_test_threads_really_used(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_threads_really_used(error)
        call restore_engine_default()
    end subroutine cpp_test_threads_really_used


    !> Pins the C++ engine for `test_threads_auto_in_parallel` -- see the note above.
    subroutine cpp_test_threads_auto_in_parallel(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_threads_auto_in_parallel(error)
        call restore_engine_default()
    end subroutine cpp_test_threads_auto_in_parallel


    !> Pins the C++ engine for `test_merge_round_threads_used` -- see the note above.
    subroutine cpp_test_merge_round_threads_used(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_merge_round_threads_used(error)
        call restore_engine_default()
    end subroutine cpp_test_merge_round_threads_used

    !
    !> **The C++ co-ranked merge's correctness sweep, and the one test in this file that could not
    !> be written on the Fortran side.**
    !!
    !! The two engines parallelise differently, and that is why this test exists here rather than
    !! as one more `real64` sweep in `test_sorting`. The Fortran engine count-prefix-scatters into
    !! disjoint bucket ranges and never merges; the C++ engine sorts chunks and then co-ranks a
    !! parallel merge, splitting each merge by binary search. The classic defect co-ranking invites
    !! -- a boundary that lands one element early -- shows up at exactly one array size and passes
    !! at every neighbouring one, so the density IS the test.
    !!
    !! **`force_merge_segments(1)` is what makes it a test at all.** The real floor is 16384, so a
    !! pair shorter than twice that is merged in one piece by the old, correct, single-threaded
    !! merge: without this the dense sweep calls the co-rank ZERO times and passes. Two deliberate
    !! co-rank defects survived the entire suite before the sweeps started calling it
    !! (`feature_risks.md` Risk-49) -- and that helper drives a **C++** override, which is the
    !! whole reason this sweep cannot live beside the Fortran ones.
    !!
    !! **`merge_threads_used() > 1` is the positive control**, for the same reason the helper is
    !! needed: a merge that quietly stopped splitting returns the identical permutation, so the
    !! oracle below passes just as happily against a co-rank that never ran.
    !!
    !! Both properties are asserted, because they fail differently: equality with the serial
    !! permutation catches a boundary that reorders rows, and the each-index-once walk catches one
    !! that makes two segments overlap or leave a gap -- which is not a permutation at all, and
    !! which nothing downstream would notice on the raw path.
    subroutine cpp_test_corank_size_sweep(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: ser(:), par(:)
        logical, allocatable :: seen(:)
        integer :: n, t, k
        character(len=:), allocatable :: tag, tag2

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a team the co-ranked merge cannot run at "// &
            "all, so every assertion below would hold because both arms are the same serial merge")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least 2 processors: an explicit threads= is clamped "// &
                "to the processor count, so the co-ranked merge would never be entered")
            return
        end if
#endif
        call parquet_debug_use_fortran_sort_engine(.false.)
        call force_merge_segments(1_int64)
        do n = 2, 400
            allocate(v(n), seen(n))
            call ties_fixture(v)
            call force_parallel_threshold(1000000000_int64)   ! above the size -> serial reference
            call pf_argsort(v, ser)
            call force_parallel_threshold(2_int64)            ! below the size -> co-ranked
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call itoa(n, tag)
                call itoa(t, tag2)
                call check(error, merge_threads_used() > 1_int64, &
                    "the final merge round must really be co-ranked at n="//tag// &
                    " threads="//tag2//"; a single-threaded merge satisfies the oracle below")
                if (allocated(error)) exit
                ! Two checks, not one `.and.`: `all(par == ser)` would be evaluated over mismatched
                ! extents when the size is wrong, since `.and.` does not short-circuit.
                call itoa(n, tag)
                call itoa(t, tag2)
                call check(error, size(par) == n, &
                    "co-ranking must return n indices at n="//tag//" threads="//tag2)
                if (allocated(error)) exit
                call itoa(n, tag)
                call itoa(t, tag2)
                call check(error, all(par == ser), &
                    "co-ranking must equal the serial permutation at n="//tag// &
                    " threads="//tag2)
                if (allocated(error)) exit
                seen = .false.
                do k = 1, n
                    if (par(k) < 1 .or. par(k) > n) exit
                    if (seen(par(k))) exit
                    seen(par(k)) = .true.
                end do
                call itoa(n, tag)
                call itoa(t, tag2)
                call check(error, all(seen), &
                    "co-ranking must return each index exactly once at n="//tag// &
                    " threads="//tag2)
                if (allocated(error)) exit
            end do
            deallocate(v, seen)
            if (allocated(error)) exit
        end do
        call force_parallel_threshold(0_int64)
        call force_merge_segments(0_int64)
        call restore_engine_default()
    end subroutine cpp_test_corank_size_sweep


    !> Pins the C++ engine for `test_threads_one_is_serial` -- see the note above.
    !!
    !! This one did NOT fail when the flip landed, and that is the point: `threads_used()` is a
    !! stale C++ counter, so on the Fortran engine it kept whatever a previous test left behind,
    !! and "threads=1 must sort serially" passed by reading a 1 nobody had written for it. Running
    !! the suite ALONE is what exposed it. A vacuous pass is the failure mode these pins exist to
    !! prevent, and it is strictly worse than the eleven that failed loudly.
    subroutine cpp_test_threads_one_is_serial(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_threads_one_is_serial(error)
        call restore_engine_default()
    end subroutine cpp_test_threads_one_is_serial


end module test_sorting_cpp
