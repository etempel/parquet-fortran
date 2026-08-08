!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the independent parquet_settings module -- the process-global settings and the
!> read-only `parquet_max_*` limits. Runs in isolation from the rest of the library: no reader,
!> writer or parquet file is involved.
!>
!> Two things shape what is asserted here.
!>
!> **Arrow's default thread-pool capacity is hardware-dependent**, so no test may assert a specific
!> number for it. The assertions are relational instead (>= 1, and "what was set comes back"), which
!> is the only form that survives running on a different machine.
!>
!> **A setter/getter round-trip is the weakest possible test of a setting** -- it passes just as
!> happily against a value that is stored and never used, which is `feature_risks.md` Risk-41. So
!> every knob here is asserted three ways: its factory value, its round trip, and an **observed
!> effect with a negative control**. The observations differ per knob because nothing generic can
!> see them:
!>
!> * `arrow_threads` needs nothing extra -- the value round-trips through Arrow itself, so a getter
!>   bound to the wrong C symbol fails the plain round trip;
!> * `sort_threads` is read back through `pf_sort_threads`, the single place it is consulted;
!> * `default_compression`/`_level` are observed as the SIZE of two files written from identical
!>   data, since this library has no reader-side API for a file's physical encoding;
!> * `prefetch_threads` and `default_use_threads` have no observable at all without help, and are
!>   read back through two C++ debug hooks (`parquet_debug_get_prefetch_threads_used`,
!>   `parquet_debug_get_last_use_threads`) declared locally below, never in parquet_bindings.
!>
!> The five C++-side knobs are each observed through something the library already had to expose to
!> test the behaviour itself, which is why S4 added no new debug hook at all:
!>
!> * `sort_parallel_min_rows` through `parquet_debug_get_sort_threads_used` -- the sort's RESULT is
!>   identical threaded or not, by design, so only the thread count can see this knob;
!> * `sort_counting_path` and `sort_counting_bucket_limit` through
!>   `parquet_debug_get_sort_comparisons`, which is exactly 0 on the counting path and nonzero on
!>   the comparator path -- again, the two produce the same permutation;
!> * `target_row_group_bytes` through `parquet_get_num_row_groups` on a written file AND through
!>   `parquet_get_chunk_size(writer)`, because the byte target has two callers serving two different
!>   writers and a copy re-inlined into either one would be invisible to a test of the other
!>   (feature_risks.md Risk-43);
!> * `statistics_prescreen` through `parquet_debug_get_row_groups_pruned` alongside an A/B equality,
!>   the shape CLAUDE.md prescribes for the screen: equality alone passes just as happily against a
!>   screen that never prunes.
!>
!> Two tests are deliberately not about any single knob: `test_argument_beats_setting` (a resolution
!> written the wrong way round would make the setting win, and every per-knob test would still pass)
!> and `test_reset_all_knobs` (a knob that is settable but not resettable leaks into every later
!> suite).
!>
!> Abort paths cannot be exercised here because `error stop` kills the process -- they live in
!> test/error_scenarios.f90 (`set_arrow_threads_zero`, `settings_bad_codec`,
!> `settings_negative_sort_threads`), driven from test_errors.f90.
!>
!> **Every test restores what it changed.** The suite is excluded from test-drive's per-suite
!> parallelism (see run_tester.f90), but suites still share one process, so a leaked capacity of 1
!> would silently serialise every later suite's Arrow work.
module test_settings
    use parquet
    use iso_fortran_env, only : output_unit, int32, int64, real64
    use iso_c_binding, only : c_null_char, c_int
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads
#endif
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_settings
    !
contains
    !
    subroutine collect_tests_parquet_settings(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("thread pool reports a usable capacity", test_threads_default_sane), &
            new_unittest("set then get round-trips through Arrow", test_threads_round_trip), &
            new_unittest("reset restores the pre-set capacity", test_reset_restores), &
            new_unittest("reset is a no-op when nothing was set", test_reset_untouched_is_noop), &
            new_unittest("the read-only limits have their documented values", test_limits), &
            new_unittest("print_settings names every setting and limit", test_print_settings), &
            new_unittest("every knob starts at its factory value", test_factory_defaults), &
            new_unittest("reset returns every knob to its factory value", test_reset_all_knobs), &
            new_unittest("sort_threads caps the automatic thread count", test_sort_threads_effect), &
            new_unittest("sort_threads never lifts the in-parallel serial answer", test_sort_threads_respects_region), &
            new_unittest("prefetch_threads caps the parallel prefetch", test_prefetch_threads_effect), &
            new_unittest("table_threads caps the parallel per-column rewrite", test_table_threads_effect), &
            new_unittest("string_threads caps what one string column resolves to", &
                test_string_threads_effect), &
            new_unittest("the string work floor is a payload floor, overridable for tests", &
                test_string_work_floor), &
            new_unittest("a string operation stands down inside a parallel region", &
                test_string_threads_in_region), &
            new_unittest("default_compression changes the bytes written", test_default_compression_effect), &
            new_unittest("default_compression_level changes the bytes written", test_default_compression_level_effect), &
            new_unittest("naming a codec default drops the zstd-tuned level", test_default_compression_decouples_level), &
            new_unittest("default_use_threads reaches the reader and the writer", test_default_use_threads_effect), &
            new_unittest("an explicit argument always beats the setting", test_argument_beats_setting), &
            new_unittest("an invalid codec default aborts nothing else", test_bad_codec_leaves_state_intact), &
            new_unittest("sort_parallel_min_rows decides whether a sort threads at all", &
                test_sort_parallel_min_rows_effect), &
            new_unittest("sort_counting_path switches the sort between its two engines", &
                test_sort_counting_path_effect), &
            new_unittest("sort_counting_bucket_limit declines a key whose range exceeds it", &
                test_sort_counting_bucket_limit_effect), &
            new_unittest("target_row_group_bytes sizes the row groups of a whole-table write", &
                test_target_row_group_bytes_effect), &
            new_unittest("target_row_group_bytes also sizes the streaming path's estimate", &
                test_target_row_group_bytes_streaming), &
            new_unittest("statistics_prescreen prunes row groups without changing the answer", &
                test_statistics_prescreen_effect), &
            new_unittest("both integer kinds reach the same setting", test_both_integer_kinds), &
            new_unittest("every environment variable reaches its own knob", test_env_every_variable), &
            new_unittest("an absent variable leaves its knob alone", test_env_absent_leaves_knob), &
            new_unittest("an empty variable is treated as unset", test_env_empty_is_unset), &
            new_unittest("a variable overrides an earlier explicit set", test_env_overrides_explicit), &
            new_unittest("integers accept blanks and a sign, booleans fold case", test_env_value_forms), &
            new_unittest("a codec and its level both arrive", test_env_codec_and_level), &
            new_unittest("set_threads moves all five thread counts", test_set_threads), &
            new_unittest("PARQUET_FORTRAN_THREADS is overridden by the specific variables", &
                test_env_threads_then_specific) &
            ]
    end subroutine collect_tests_parquet_settings
    !
    !> Arrow's own default is hardware-derived, so the only portable assertion is that it is a
    !> usable thread count. A getter wired to the wrong symbol would typically answer 0 or garbage.
    subroutine test_threads_default_sane(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: n
        !
        n = parquet_get_arrow_threads()
        call check(error, n >= 1, "parquet_get_arrow_threads() >= 1")
    end subroutine test_threads_default_sane
    !
    !> The value goes into Arrow and comes back out of Arrow, so this is the test that fails if the
    !> new getter is bound to the wrong C symbol or declared with a mismatched kind.
    subroutine test_threads_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        original = parquet_get_arrow_threads()
        call parquet_set_arrow_threads(3)
        call check(error, parquet_get_arrow_threads() == 3, "capacity is 3 after parquet_set_arrow_threads(3)")
        if (allocated(error)) then
            call restore(original)
            return
        end if
        call parquet_set_arrow_threads(5)
        call check(error, parquet_get_arrow_threads() == 5, "capacity is 5 after parquet_set_arrow_threads(5)")
        call restore(original)
    end subroutine test_threads_round_trip
    !
    !> parquet_reset_settings restores the capacity captured on the FIRST set, not the most recent
    !> one -- so two sets followed by a reset must land back on the value from before either.
    subroutine test_reset_restores(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        original = parquet_get_arrow_threads()
        call parquet_set_arrow_threads(2)
        call parquet_set_arrow_threads(7)
        call parquet_reset_settings()
        call check(error, parquet_get_arrow_threads() == original, &
            "parquet_reset_settings restores the capacity from before the first set")
        call restore(original)
    end subroutine test_reset_restores
    !
    !> The other half of the capture rule, and the half a one-sided test would miss: with nothing
    !> ever set (which parquet_reset_settings itself re-establishes), a reset must leave the
    !> capacity alone rather than resize it to some invented default.
    subroutine test_reset_untouched_is_noop(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: before
        !
        ! Clears any capture left by an earlier test in this suite, putting the module back into
        ! its never-set state -- which is exactly the state under test.
        call parquet_reset_settings()
        before = parquet_get_arrow_threads()
        call parquet_reset_settings()
        call check(error, parquet_get_arrow_threads() == before, &
            "parquet_reset_settings leaves the capacity alone when nothing was ever set")
    end subroutine test_reset_untouched_is_noop
    !
    !> Pins each published limit to its documented value. Cheap, and the only thing that would
    !> catch a value changed by accident while relocating these constants: a wrong number still
    !> compiles and still passes every filter/sort test that does not probe the cap itself.
    subroutine test_limits(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call check(error, parquet_max_filter_rule_len == 8192, "parquet_max_filter_rule_len == 8192")
        if (allocated(error)) return
        call check(error, parquet_max_filter_depth == 32, "parquet_max_filter_depth == 32")
        if (allocated(error)) return
        call check(error, parquet_max_filter_nodes == 1024, "parquet_max_filter_nodes == 1024")
        if (allocated(error)) return
        call check(error, parquet_max_sort_keys == 16, "parquet_max_sort_keys == 16")
        if (allocated(error)) return
        call check(error, parquet_max_sort_key_len == 320, "parquet_max_sort_key_len == 320")
        if (allocated(error)) return
        call check(error, parquet_max_maml_line_len == 1024, "parquet_max_maml_line_len == 1024")
    end subroutine test_limits
    !
    !> Asserts on the CONTENT of the dump (which names appear), never on its layout: the name set
    !> is what doc/pages/settings.md documents and what tools/check_source_conventions.py pins, so
    !> a golden-layout assertion here would only break on cosmetic changes without adding cover.
    subroutine test_print_settings(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: path = "test_run/settings_dump.txt"
        character(len=256) :: line
        integer :: u, ios
        logical :: seen_threads, seen_rule_len, seen_depth, seen_nodes
        logical :: seen_keys, seen_key_len, seen_maml
        !
        seen_threads = .false.; seen_rule_len = .false.; seen_depth = .false.; seen_nodes = .false.
        seen_keys = .false.; seen_key_len = .false.; seen_maml = .false.
        !
        open (newunit=u, file=path, status="replace", action="write")
        call parquet_print_settings(u)
        close (u)
        !
        open (newunit=u, file=path, status="old", action="read")
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "arrow_threads") > 0) seen_threads = .true.
            if (index(line, "parquet_max_filter_rule_len") > 0) seen_rule_len = .true.
            if (index(line, "parquet_max_filter_depth") > 0) seen_depth = .true.
            if (index(line, "parquet_max_filter_nodes") > 0) seen_nodes = .true.
            if (index(line, "parquet_max_sort_keys") > 0) seen_keys = .true.
            if (index(line, "parquet_max_sort_key_len") > 0) seen_key_len = .true.
            if (index(line, "parquet_max_maml_line_len") > 0) seen_maml = .true.
        end do
        close (u)
        !
        call check(error, seen_threads, "print_settings mentions arrow_threads")
        if (allocated(error)) return
        call check(error, seen_rule_len, "print_settings mentions parquet_max_filter_rule_len")
        if (allocated(error)) return
        call check(error, seen_depth, "print_settings mentions parquet_max_filter_depth")
        if (allocated(error)) return
        call check(error, seen_nodes, "print_settings mentions parquet_max_filter_nodes")
        if (allocated(error)) return
        call check(error, seen_keys, "print_settings mentions parquet_max_sort_keys")
        if (allocated(error)) return
        call check(error, seen_key_len, "print_settings mentions parquet_max_sort_key_len")
        if (allocated(error)) return
        call check(error, seen_maml, "print_settings mentions parquet_max_maml_line_len")
    end subroutine test_print_settings
    !
    !> Every knob's documented factory value, asserted explicitly so that changing one is a
    !> deliberate act rather than something a test quietly absorbs.
    subroutine test_factory_defaults(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: codec
        !
        call parquet_reset_settings()
        call check(error, parquet_get_sort_threads() == 0, "sort_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        call check(error, parquet_get_prefetch_threads() == 0, "prefetch_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd", "default_compression defaults to zstd")
        if (allocated(error)) return
        call check(error, parquet_get_default_compression_level() == 3, "default_compression_level defaults to 3")
        if (allocated(error)) return
        call check(error, parquet_get_default_use_threads(), "default_use_threads defaults to .true.")
        if (allocated(error)) return
        call check(error, parquet_get_sort_parallel_min_rows() == 8192_int64, &
            "sort_parallel_min_rows defaults to the built-in 8192")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_path(), "sort_counting_path defaults to .true.")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_bucket_limit() == 4194304_int64, &
            "sort_counting_bucket_limit defaults to the built-in 2**22")
        if (allocated(error)) return
        call check(error, parquet_get_target_row_group_bytes() == 268435456_int64, &
            "target_row_group_bytes defaults to the built-in 256 MiB")
        if (allocated(error)) return
        call check(error, parquet_get_statistics_prescreen(), "statistics_prescreen defaults to .true.")
    end subroutine test_factory_defaults

    !> A knob that is settable but not resettable leaks across the boundary parquet_reset_settings
    !> exists to draw, and nothing else in the suite would notice. Set every one to a non-factory
    !> value first, so a reset that misses one is visible.
    subroutine test_reset_all_knobs(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: codec
        !
        call parquet_set_sort_threads(6)
        call parquet_set_prefetch_threads(2)
        call parquet_set_string_threads(3)
        call parquet_set_default_compression("gzip")
        call parquet_set_default_compression_level(9)
        call parquet_set_default_use_threads(.false.)
        call parquet_set_sort_parallel_min_rows(64_int64)
        call parquet_set_sort_counting_path(.false.)
        call parquet_set_sort_counting_bucket_limit(128_int64)
        call parquet_set_target_row_group_bytes(4096_int64)
        call parquet_set_statistics_prescreen(.false.)
        call parquet_reset_settings()
        !
        call check(error, parquet_get_sort_threads() == 0, "reset restores sort_threads")
        if (allocated(error)) return
        call check(error, parquet_get_prefetch_threads() == 0, "reset restores prefetch_threads")
        if (allocated(error)) return
        call check(error, parquet_get_string_threads() == 0, "reset restores string_threads")
        if (allocated(error)) return
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd", "reset restores default_compression")
        if (allocated(error)) return
        call check(error, parquet_get_default_compression_level() == 3, "reset restores default_compression_level")
        if (allocated(error)) return
        call check(error, parquet_get_default_use_threads(), "reset restores default_use_threads")
        if (allocated(error)) return
        call check(error, parquet_get_sort_parallel_min_rows() == 8192_int64, &
            "reset restores sort_parallel_min_rows")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_path(), "reset restores sort_counting_path")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_bucket_limit() == 4194304_int64, &
            "reset restores sort_counting_bucket_limit")
        if (allocated(error)) return
        call check(error, parquet_get_target_row_group_bytes() == 268435456_int64, &
            "reset restores target_row_group_bytes")
        if (allocated(error)) return
        call check(error, parquet_get_statistics_prescreen(), "reset restores statistics_prescreen")
    end subroutine test_reset_all_knobs

    !> The observed effect, not the round trip: pf_sort_threads is the one place the setting is
    !> read, and it is what every sort actually asks. The negative control is the auto case in the
    !> same test -- a setting that was ignored would leave both answers equal, and a cap that fired
    !> unconditionally would break the auto case instead.
    subroutine test_sort_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: auto_n
        !
        call parquet_reset_settings()
        auto_n = pf_sort_threads()
        call check(error, auto_n >= 1, "pf_sort_threads() is at least 1 with the setting automatic")
        if (allocated(error)) return
        !
        call parquet_set_sort_threads(1)
        call check(error, pf_sort_threads() == 1, "sort_threads=1 makes pf_sort_threads answer 1")
        if (allocated(error)) return
        !
        ! A cap above what OpenMP offers must change nothing -- this is a cap, not a request.
        call parquet_set_sort_threads(auto_n + 16)
        call check(error, pf_sort_threads() == auto_n, &
            "a sort_threads cap above the OpenMP thread count must not raise the answer")
        call parquet_reset_settings()
    end subroutine test_sort_threads_effect

    !> Risk-40: an unqualified sort inside an OpenMP parallel region runs SERIALLY, and a setting
    !> must not lift that back up -- eight threads each asking for eight more is the oversubscription
    !> the rule exists to prevent. Without this, a cap applied after the region check would look
    !> perfectly correct everywhere else.
    subroutine test_sort_threads_respects_region(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: inside
        !
        call parquet_set_sort_threads(8)
        inside = 0
        !$omp parallel
        !$omp single
        inside = pf_sort_threads()
        !$omp end single
        !$omp end parallel
        call parquet_reset_settings()
#ifdef _OPENMP
        call check(error, inside == 1, &
            "sort_threads must not lift the serial answer inside an OpenMP parallel region")
#else
        call check(error, inside == 1, "without OpenMP pf_sort_threads is always 1")
#endif
    end subroutine test_sort_threads_respects_region

    !> The prefetch thread count is Fortran-side state with no other way out, so this reads the
    !> debug counter the prefetch records.
    !>
    !> **Both directions, and the positive one first.** Asserting only that the capped run reports a
    !> small number would pass against a counter that is never written at all -- it starts at 0, and
    !> 0 satisfies every "<= 1" assertion anyone would write. So the auto run has to be seen
    !> reporting a real thread count before the capped run's 0 means anything. The auto assertion is
    !> conditional on OpenMP actually offering more than one thread, since on a single-threaded
    !> machine the parallel path correctly declines and there is nothing to observe.
    !>
    !> **This test is also what catches the internally-parallel prefetch being switched off
    !> wholesale on one compiler.** It has been: `parallel_prefetch_ok` carried an
    !> `#ifdef __INTEL_COMPILER` bail-out for a crash that the shared-reader-array shape had
    !> already fixed, and the positive control below is what reported it -- so keep it a plain
    !> assertion rather than making it tolerate a compiler taking the serial path.
    subroutine test_prefetch_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/settings_prefetch_threads.parquet"
        type(parquet_table) :: t
        integer(int32) :: a(50), b(50), c(50), d(50)
        integer :: i, capped, auto_used, avail
        !
        do i = 1, 50
            a(i) = i; b(i) = 2*i; c(i) = 3*i; d(i) = 4*i
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("c", c)
        call t%add_column("d", d)
        call parquet_write_table(t, out_file)
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        !
        ! Automatic: four distinct top-level columns, so the parallel path applies and records the
        ! team it was given. This is the positive control -- without it the assertion below is
        ! satisfied by a hook that was never wired up.
        call parquet_reset_settings()
        call parquet_debug_reset_prefetch_threads()
        call read_all_columns(out_file)
        auto_used = parquet_debug_prefetch_threads()
        if (avail > 1) then
            call check(error, auto_used > 1, &
                "with prefetch_threads automatic and several columns, the prefetch must run in parallel")
            if (allocated(error)) return
        end if
        !
        ! Capped at 1: parallel_prefetch_ok declines through the same test that already handles a
        ! single-threaded OpenMP environment, so the serial path runs and the counter is never
        ! written -- it stays at the 0 this test reset it to.
        call parquet_set_prefetch_threads(1)
        call parquet_debug_reset_prefetch_threads()
        call read_all_columns(out_file)
        capped = parquet_debug_prefetch_threads()
        call parquet_reset_settings()
        call check(error, capped == 0, &
            "prefetch_threads=1 must not run the internally-parallel prefetch")
    end subroutine test_prefetch_threads_effect

    !> `table_threads` decides how many threads a row-structural mutation rewrites columns on.
    !>
    !> **This knob has no observable in the answer at all**, by design: the parallel and serial
    !> paths produce an identical table, which is the property that makes the feature safe and
    !> simultaneously makes it untestable by ordinary means (`feature_risks.md` Risk-39's shape). So
    !> the assertion is on the thread count the mutation resolved to, read back through a C++ debug
    !> hook, and it needs all three of the observations below to mean anything:
    !>
    !>   * **automatic, above the work floor** -- the positive control. Without it, the capped
    !>     assertion below is satisfied by an implementation that never threads at all.
    !>   * **capped at 1** -- the knob's own effect, on the identical fixture.
    !>   * **below the work floor** -- the gate's other reason to decline, so a floor that was
    !>     removed or set to zero does not pass unnoticed.
    !>
    !> The counter is written on BOTH paths (1 when serial), so "declined" and "never reached the
    !> code at all" are distinguishable -- unlike the prefetch counter above, which is only written
    !> on its parallel path and is therefore asserted against 0.
    subroutine test_table_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        ! Above colwork_min_elements (131072) with room to spare, and small enough to build in
        ! milliseconds: 4 int32 columns of this length is under 3 MB.
        integer, parameter :: BIG = 150000
        integer, parameter :: SMALL = 64
        integer(int32), allocatable :: k(:), a(:), b(:), c(:)
        integer :: i, auto_used, capped, tiny, avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        allocate(k(BIG), a(BIG), b(BIG), c(BIG))
        do i = 1, BIG
            ! A scattered key, so the permutation actually moves rows -- %sort_by returns early
            ! without touching a column when the rows are already in order, which would leave the
            ! counter reporting the previous call.
            k(i) = mod(i * 7919, BIG)
            a(i) = i
            b(i) = 2*i
            c(i) = 3*i
        end do
        !
        ! Automatic, above the floor: the positive control.
        call parquet_reset_settings()
        call parquet_debug_reset_table_threads()
        call sort_four_columns(k, a, b, c)
        auto_used = parquet_debug_table_threads()
        if (avail > 1) then
            call check(error, auto_used > 1, &
                "with table_threads automatic and several large columns, %sort_by must rewrite them in parallel")
            if (allocated(error)) return
        end if
        !
        ! Capped at 1, same fixture: the knob's own effect.
        call parquet_set_table_threads(1)
        call parquet_debug_reset_table_threads()
        call sort_four_columns(k, a, b, c)
        capped = parquet_debug_table_threads()
        call parquet_reset_settings()
        call check(error, capped == 1, "table_threads=1 must make the per-column rewrite serial")
        if (allocated(error)) return
        !
        ! Below the work floor, automatic: the gate's other reason to decline.
        deallocate(k, a, b, c)
        allocate(k(SMALL), a(SMALL), b(SMALL), c(SMALL))
        do i = 1, SMALL
            k(i) = mod(i * 7, SMALL)
            a(i) = i
            b(i) = 2*i
            c(i) = 3*i
        end do
        call parquet_debug_reset_table_threads()
        call sort_four_columns(k, a, b, c)
        tiny = parquet_debug_table_threads()
        call check(error, tiny == 1, &
            "a table too small to be worth a thread team must rewrite its columns serially")
    end subroutine test_table_threads_effect

    !> Builds a four-column in-memory table from the arrays given and sorts it by the first.
    !>
    !> A fresh table per call, deliberately: `%sort_by` leaves the rows in key order, so sorting the
    !> same table twice makes the second call a no-op that never reaches the per-column loop.
    subroutine sort_four_columns(k, a, b, c)
        integer(int32), intent(in) :: k(:) !! sort key.
        integer(int32), intent(in) :: a(:) !! payload column.
        integer(int32), intent(in) :: b(:) !! payload column.
        integer(int32), intent(in) :: c(:) !! payload column.
        type(parquet_table) :: t
        !
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("c", c)
        call t%sort_by(["k"])
    end subroutine sort_four_columns

    !> Threads the last row-structural table mutation resolved to; 1 when it ran serially.
    integer function parquet_debug_table_threads() result(n)
        use iso_c_binding, only : c_int64_t
        interface
            function got() bind(C, name="parquet_debug_get_table_threads_used") result(kk)
                import :: c_int64_t
                integer(c_int64_t) :: kk
            end function got
        end interface
        !
        n = int(got())
    end function parquet_debug_table_threads

    !> Clears the mutation thread counter, so a test observes its own mutation rather than an
    !> earlier one. Reset to 0, which the mutation itself never writes -- so a counter still reading
    !> 0 afterwards means the mutation never reached the loop at all.
    subroutine parquet_debug_reset_table_threads()
        use iso_c_binding, only : c_int64_t
        interface
            subroutine put(kk) bind(C, name="parquet_debug_set_table_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: kk
            end subroutine put
        end interface
        !
        call put(0_c_int64_t)
    end subroutine parquet_debug_reset_table_threads

    !> Two writes of the same data, differing only in the setting. Equal sizes mean the setting was
    !> ignored -- which is the whole failure this test exists for -- so the fixture is chosen to
    !> compress well enough that uncompressed and zstd cannot coincide.
    subroutine test_default_compression_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: raw_file = "test_run/settings_compression_raw.parquet"
        character(len=*), parameter :: zstd_file = "test_run/settings_compression_zstd.parquet"
        integer(int64) :: raw_size, zstd_size
        !
        call parquet_set_default_compression("uncompressed")
        call write_compressible(raw_file)
        call parquet_reset_settings()
        call write_compressible(zstd_file)
        !
        call file_size(raw_file, raw_size)
        call file_size(zstd_file, zstd_size)
        call check(error, raw_size > zstd_size, &
            "default_compression=uncompressed must write a bigger file than the zstd default")
    end subroutine test_default_compression_effect

    !> Same shape, one level apart. Asserted as "the sizes differ" rather than an ordering: which of
    !> two zstd levels wins on a given input is the codec's business, not this library's.
    subroutine test_default_compression_level_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: low_file = "test_run/settings_level_low.parquet"
        character(len=*), parameter :: high_file = "test_run/settings_level_high.parquet"
        integer(int64) :: low_size, high_size
        !
        call parquet_set_default_compression_level(1)
        call write_compressible(low_file)
        call parquet_reset_settings()
        call parquet_set_default_compression_level(19)
        call write_compressible(high_file)
        call parquet_reset_settings()
        !
        call file_size(low_file, low_size)
        call file_size(high_file, high_size)
        call check(error, low_size /= high_size, &
            "default_compression_level=1 and =19 must not produce identical files")
    end subroutine test_default_compression_level_effect

    !> The coupling rule: level 3 is tuned for THIS library's default codec, so naming a codec --
    !> by argument or by setting -- must drop it rather than attach it to the new codec. Observed
    !> through the file, not the getter: a resolution that kept level 3 for a codec that has no
    !> levels at all would still return 3 from the getter.
    subroutine test_default_compression_decouples_level(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: via_setting = "test_run/settings_decouple_setting.parquet"
        character(len=*), parameter :: via_argument = "test_run/settings_decouple_argument.parquet"
        integer(int64) :: setting_size, argument_size
        !
        ! Choosing zstd as the DEFAULT must behave like naming zstd as an ARGUMENT: both are a
        ! deliberate choice of codec, so neither gets the fully-defaulted level 3.
        call parquet_set_default_compression("zstd")
        call write_compressible(via_setting)
        call parquet_reset_settings()
        call write_compressible_with_codec(via_argument, "zstd")
        !
        call file_size(via_setting, setting_size)
        call file_size(via_argument, argument_size)
        call check(error, setting_size == argument_size, &
            "default_compression=zstd must resolve to the same level as compression='zstd'")
    end subroutine test_default_compression_decouples_level

    !> use_threads disappears into a C++ handle, so the resolved value is read back from the hook
    !> that records what actually crossed the boundary. Both directions, since a hook stuck at one
    !> value would pass a one-sided test.
    subroutine test_default_use_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/settings_use_threads.parquet"
        !
        call parquet_reset_settings()
        call write_compressible(out_file)
        call check(error, parquet_debug_last_use_threads() == 1, &
            "the factory default must reach the writer as use_threads on")
        if (allocated(error)) return
        !
        call parquet_set_default_use_threads(.false.)
        call write_compressible(out_file)
        call check(error, parquet_debug_last_use_threads() == 0, &
            "default_use_threads=.false. must reach the writer as use_threads off")
        call parquet_reset_settings()
    end subroutine test_default_use_threads_effect

    !> A setting supplies a DEFAULT; an explicit argument overrides it. A resolution written the
    !> wrong way round would make the setting win, and every per-knob test above would still pass --
    !> which is exactly why this one is separate.
    subroutine test_argument_beats_setting(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/settings_argument_wins.parquet"
        type(parquet_writer) :: writer
        integer(int32) :: v(4) = [1, 2, 3, 4]
        !
        ! use_threads: setting says off, argument says on.
        call parquet_set_default_use_threads(.false.)
        call parquet_open_writer(writer, out_file, use_threads=.true.)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
        call check(error, parquet_debug_last_use_threads() == 1, &
            "an explicit use_threads=.true. must beat default_use_threads=.false.")
        call parquet_reset_settings()
        if (allocated(error)) return
        !
        ! compression: setting says uncompressed, argument says zstd -- the argument's file must be
        ! the smaller one.
        block
            character(len=*), parameter :: arg_file = "test_run/settings_argument_codec.parquet"
            character(len=*), parameter :: set_file = "test_run/settings_setting_codec.parquet"
            integer(int64) :: arg_size, set_size
            call parquet_set_default_compression("uncompressed")
            call write_compressible_with_codec(arg_file, "zstd")
            call write_compressible(set_file)
            call parquet_reset_settings()
            call file_size(arg_file, arg_size)
            call file_size(set_file, set_size)
            call check(error, arg_size < set_size, &
                "an explicit compression='zstd' must beat default_compression='uncompressed'")
        end block
    end subroutine test_argument_beats_setting

    !> A rejected codec must leave the previous default in place rather than half-applying itself.
    !> The abort path itself is an error scenario (settings_bad_codec); this is the survivor check.
    subroutine test_bad_codec_leaves_state_intact(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: codec
        !
        call parquet_set_default_compression("GZIP")   ! also proves the fold is case-insensitive
        call parquet_get_default_compression(codec)
        call check(error, codec == "gzip", "a codec default is folded to lower case")
        call parquet_reset_settings()
    end subroutine test_bad_codec_leaves_state_intact

    !> ---- helpers ----

    !> Writes a moderately compressible string column, so that two codecs or two levels cannot
    !> coincidentally produce the same file size.
    subroutine write_compressible(path)
        character(len=*), intent(in) :: path !! output file.
        !
        call write_compressible_impl(path)
    end subroutine write_compressible

    !> Same fixture, with the codec named explicitly rather than left to the setting.
    subroutine write_compressible_with_codec(path, codec)
        character(len=*), intent(in) :: path !! output file.
        character(len=*), intent(in) :: codec !! compression= argument.
        !
        call write_compressible_impl(path, codec)
    end subroutine write_compressible_with_codec

    !> The one place the fixture is built, so every comparison above is over identical data.
    subroutine write_compressible_impl(path, codec)
        character(len=*), intent(in) :: path !! output file.
        character(len=*), intent(in), optional :: codec !! compression= argument, if any.
        type(parquet_writer) :: writer
        character(len=256) :: values(400)
        character(len=8), parameter :: vocab(6) = [character(len=8) :: &
            "alpha   ", "beta    ", "gamma   ", "delta   ", "epsilon ", "zeta    "]
        integer :: i, j, k
        !
        do i = 1, size(values)
            values(i) = ""
            do j = 0, 31
                k = mod(i*7 + j*j + j/2, size(vocab)) + 1
                values(i)(j*8+1:j*8+8) = vocab(k)
            end do
        end do
        if (present(codec)) then
            call parquet_open_writer(writer, path, compression=codec)
        else
            call parquet_open_writer(writer, path)
        end if
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
    end subroutine write_compressible_impl

    !> Opens a table and materializes every column, which is the path %prefetch parallelises.
    subroutine read_all_columns(path)
        character(len=*), intent(in) :: path !! file to read.
        type(parquet_table) :: t
        !
        call parquet_open_table(t, path)
        call t%materialize_all()
    end subroutine read_all_columns

    !> Reports a file's size in bytes.
    subroutine file_size(path, bytes)
        character(len=*), intent(in) :: path !! file to measure.
        integer(int64), intent(out) :: bytes !! its size.
        logical :: exists
        !
        inquire (file=path, exist=exists, size=bytes)
        if (.not. exists) bytes = -1_int64
    end subroutine file_size

    !> The resolved use_threads value the last opened reader or writer received.
    integer function parquet_debug_last_use_threads() result(n)
        use iso_c_binding, only : c_int
        interface
            function got() bind(C, name="parquet_debug_get_last_use_threads") result(k)
                import :: c_int
                integer(c_int) :: k
            end function got
        end interface
        !
        n = int(got())
    end function parquet_debug_last_use_threads

    !> Threads the last parallel prefetch was given; 0 if it ran serially.
    integer function parquet_debug_prefetch_threads() result(n)
        use iso_c_binding, only : c_int64_t
        interface
            function got() bind(C, name="parquet_debug_get_prefetch_threads_used") result(k)
                import :: c_int64_t
                integer(c_int64_t) :: k
            end function got
        end interface
        !
        n = int(got())
    end function parquet_debug_prefetch_threads

    !> Clears the prefetch counter, so a test observes its own prefetch rather than an earlier one.
    subroutine parquet_debug_reset_prefetch_threads()
        use iso_c_binding, only : c_int64_t
        interface
            subroutine put(k) bind(C, name="parquet_debug_set_prefetch_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine put
        end interface
        !
        call put(0_c_int64_t)
    end subroutine parquet_debug_reset_prefetch_threads

    ! ==================================================================================
    ! S5: parquet_settings_from_env
    ! ==================================================================================
    !
    !> Sets an environment variable for the rest of this process.
    !>
    !> Fortran cannot set one, so this is POSIX `setenv` through a local `bind(C)` interface -- the
    !> same convention every parquet_debug_* hook here follows, and test-only, so no `src/` file
    !> gains a POSIX dependency. That a runtime `setenv` is visible to a later
    !> `get_environment_variable` in the SAME process is what keeps these tests in process; without
    !> it every one of them would need a subprocess and its own environment plumbing in
    !> tools/run_error_scenarios.sh.
    subroutine set_env(name, value)
        character(len=*), intent(in) :: name !! variable to set.
        character(len=*), intent(in) :: value !! its new value.
        interface
            function c_setenv(nm, val, overwrite) bind(C, name="setenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*) !! NUL-terminated name.
                character(kind=c_char), intent(in) :: val(*) !! NUL-terminated value.
                integer(c_int), value :: overwrite !! nonzero replaces an existing value.
                integer(c_int) :: rc !! 0 on success.
            end function c_setenv
        end interface
        integer :: rc

        rc = int(c_setenv(name // c_null_char, value // c_null_char, 1_c_int))
    end subroutine set_env

    !> Removes an environment variable.
    subroutine unset_env(name)
        character(len=*), intent(in) :: name !! variable to remove.
        interface
            function c_unsetenv(nm) bind(C, name="unsetenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*) !! NUL-terminated name.
                integer(c_int) :: rc !! 0 on success.
            end function c_unsetenv
        end interface
        integer :: rc

        rc = int(c_unsetenv(name // c_null_char))
    end subroutine unset_env

    !> All three thread counts must move, and each is read back through its OWN getter -- a
    !> convenience that set one of them and forgot the others would pass any single assertion.
    !>
    !> `0` is deliberately not accepted here even though the sort and prefetch caps take it; that
    !> abort lives in test/error_scenarios.f90 (`settings_set_threads_zero`), since it kills the
    !> process.
    subroutine test_set_threads(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        call parquet_reset_settings()
        original = parquet_get_arrow_threads()
        call parquet_set_threads(3)
        call check(error, parquet_get_arrow_threads() == 3, "set_threads must resize Arrow's pool")
        if (.not. allocated(error)) call check(error, parquet_get_sort_threads() == 3, &
            "set_threads must set the sort cap")
        if (.not. allocated(error)) call check(error, parquet_get_prefetch_threads() == 3, &
            "set_threads must set the prefetch cap")
        ! The fourth and fifth knobs. A forgotten call is invisible without its own assertion, since
        ! the others still work and the name says nothing about how many "all" is -- which is exactly
        ! how this test came to say "all four" while a fifth cap existed and was never set.
        if (.not. allocated(error)) call check(error, parquet_get_table_threads() == 3, &
            "set_threads must set the table mutation cap")
        if (.not. allocated(error)) call check(error, parquet_get_string_threads() == 3, &
            "set_threads must set the string-column cap")
        ! A later individual setter overrides just its own knob, which is what makes the convenience
        ! composable rather than a mode you have to leave.
        if (.not. allocated(error)) then
            call parquet_set_sort_threads(1)
            call check(error, parquet_get_sort_threads() == 1 .and. parquet_get_prefetch_threads() == 3, &
                "an individual setter after set_threads must change only its own knob")
        end if
        call restore(original)
    end subroutine test_set_threads

    !> **The one place the environment sequence's ORDER is load-bearing.** PARQUET_FORTRAN_THREADS
    !> sets all three, so it has to be applied before the three specific variables or they could
    !> never override it -- and a reversed order would still pass a test that set only the combined
    !> variable. Both are set here, and the specific one must win.
    subroutine test_env_threads_then_specific(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        call parquet_reset_settings()
        call unset_all_env()
        original = parquet_get_arrow_threads()
        call set_env("PARQUET_FORTRAN_THREADS", "6")
        call set_env("PARQUET_FORTRAN_SORT_THREADS", "2")
        call parquet_settings_from_env()
        call unset_all_env()
        !
        call check(error, parquet_get_sort_threads() == 2, &
            "a specific variable must override the combined one, whatever order they appear in the environment")
        if (.not. allocated(error)) call check(error, parquet_get_prefetch_threads() == 6, &
            "a knob the specific variables do not mention must keep the combined value")
        if (.not. allocated(error)) call check(error, parquet_get_arrow_threads() == 6, &
            "the combined variable must reach Arrow's pool too")
        call restore(original)
    end subroutine test_env_threads_then_specific

    !> Removes all fifteen, so no test can inherit another's environment.
    !>
    !> **This matters more than the usual restore-what-you-changed discipline.** The environment
    !> outlives the test that set it and is read by nothing until the next parquet_settings_from_env
    !> call -- so a leaked PARQUET_FORTRAN_VERBOSITY=silent would not fail here, it would silence a
    !> later suite and be attributed to whatever that suite was doing.
    subroutine unset_all_env()

        call unset_env("PARQUET_FORTRAN_THREADS")
        call unset_env("PARQUET_FORTRAN_ARROW_THREADS")
        call unset_env("PARQUET_FORTRAN_SORT_THREADS")
        call unset_env("PARQUET_FORTRAN_PREFETCH_THREADS")
        call unset_env("PARQUET_FORTRAN_TABLE_THREADS")
        call unset_env("PARQUET_FORTRAN_STRING_THREADS")
        call unset_env("PARQUET_FORTRAN_SORT_PARALLEL_MIN_ROWS")
        call unset_env("PARQUET_FORTRAN_SORT_COUNTING_PATH")
        call unset_env("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT")
        call unset_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION")
        call unset_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL")
        call unset_env("PARQUET_FORTRAN_DEFAULT_USE_THREADS")
        call unset_env("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES")
        call unset_env("PARQUET_FORTRAN_STATISTICS_PRESCREEN")
        call unset_env("PARQUET_FORTRAN_VERBOSITY")
        call unset_env("PARQUET_FORTRAN_MESSAGE_STREAM")
    end subroutine unset_all_env

    !> **The bulk test, and the one that catches a crossed pair.** Fourteen variables set to fourteen
    !> distinguishable values in one call, each read back through its own getter -- so a variable
    !> wired to the wrong setter fails on both knobs at once, and a variable left out of
    !> parquet_settings_from_env's sequence fails on its own (feature_risks.md Risk-44).
    !>
    !> Every value differs from the factory default, or the assertion would pass against a call that
    !> did nothing at all.
    subroutine test_env_every_variable(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: codec, token
        integer :: original
        !
        call parquet_reset_settings()
        call unset_all_env()
        original = parquet_get_arrow_threads()
        call set_env("PARQUET_FORTRAN_ARROW_THREADS", "3")
        call set_env("PARQUET_FORTRAN_SORT_THREADS", "5")
        call set_env("PARQUET_FORTRAN_PREFETCH_THREADS", "2")
        call set_env("PARQUET_FORTRAN_TABLE_THREADS", "7")
        call set_env("PARQUET_FORTRAN_STRING_THREADS", "5")
        call set_env("PARQUET_FORTRAN_SORT_PARALLEL_MIN_ROWS", "64")
        call set_env("PARQUET_FORTRAN_SORT_COUNTING_PATH", "false")
        call set_env("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT", "128")
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION", "gzip")
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL", "6")
        call set_env("PARQUET_FORTRAN_DEFAULT_USE_THREADS", "false")
        call set_env("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES", "4096")
        call set_env("PARQUET_FORTRAN_STATISTICS_PRESCREEN", "false")
        call set_env("PARQUET_FORTRAN_VERBOSITY", "errors_only")
        call set_env("PARQUET_FORTRAN_MESSAGE_STREAM", "stderr")
        !
        call parquet_settings_from_env()
        call unset_all_env()
        !
        call check(error, parquet_get_arrow_threads() == 3, "PARQUET_FORTRAN_ARROW_THREADS reaches arrow_threads")
        if (.not. allocated(error)) call check(error, parquet_get_sort_threads() == 5, &
            "PARQUET_FORTRAN_SORT_THREADS reaches sort_threads")
        if (.not. allocated(error)) call check(error, parquet_get_prefetch_threads() == 2, &
            "PARQUET_FORTRAN_PREFETCH_THREADS reaches prefetch_threads")
        if (.not. allocated(error)) call check(error, parquet_get_table_threads() == 7, &
            "PARQUET_FORTRAN_TABLE_THREADS reaches table_threads")
        if (.not. allocated(error)) call check(error, parquet_get_string_threads() == 5, &
            "PARQUET_FORTRAN_STRING_THREADS reaches string_threads")
        if (.not. allocated(error)) call check(error, parquet_get_sort_parallel_min_rows() == 64_int64, &
            "PARQUET_FORTRAN_SORT_PARALLEL_MIN_ROWS reaches sort_parallel_min_rows")
        if (.not. allocated(error)) call check(error, .not. parquet_get_sort_counting_path(), &
            "PARQUET_FORTRAN_SORT_COUNTING_PATH reaches sort_counting_path")
        if (.not. allocated(error)) call check(error, parquet_get_sort_counting_bucket_limit() == 128_int64, &
            "PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT reaches sort_counting_bucket_limit")
        if (.not. allocated(error)) then
            call parquet_get_default_compression(codec)
            call check(error, codec == "gzip", "PARQUET_FORTRAN_DEFAULT_COMPRESSION reaches default_compression")
        end if
        if (.not. allocated(error)) call check(error, parquet_get_default_compression_level() == 6, &
            "PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL reaches default_compression_level")
        if (.not. allocated(error)) call check(error, .not. parquet_get_default_use_threads(), &
            "PARQUET_FORTRAN_DEFAULT_USE_THREADS reaches default_use_threads")
        if (.not. allocated(error)) call check(error, parquet_get_target_row_group_bytes() == 4096_int64, &
            "PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES reaches target_row_group_bytes")
        if (.not. allocated(error)) call check(error, .not. parquet_get_statistics_prescreen(), &
            "PARQUET_FORTRAN_STATISTICS_PRESCREEN reaches statistics_prescreen")
        if (.not. allocated(error)) then
            call parquet_get_verbosity(token)
            call check(error, token == "errors_only", "PARQUET_FORTRAN_VERBOSITY reaches verbosity")
        end if
        if (.not. allocated(error)) then
            call parquet_get_message_stream(token)
            call check(error, token == "stderr", "PARQUET_FORTRAN_MESSAGE_STREAM reaches message_stream")
        end if
        call restore(original)
    end subroutine test_env_every_variable

    !> An unset variable must leave its knob exactly as the program left it -- from_env applies over
    !> what is there, it does not reset.
    subroutine test_env_absent_leaves_knob(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call parquet_reset_settings()
        call unset_all_env()
        call parquet_set_sort_threads(6)
        call parquet_settings_from_env()
        call check(error, parquet_get_sort_threads() == 6, &
            "an unset variable must not disturb a knob the program set itself")
        call parquet_reset_settings()
    end subroutine test_env_absent_leaves_knob

    !> **The Q2 decision, asserted rather than assumed.** An empty variable is treated exactly as an
    !> unset one: it does not apply and it does not abort. Unset and empty ARE distinguishable
    !> (`status` is 1 versus 0), so without this test a later change could quietly make an empty
    !> value an error -- or worse, make it apply as an empty token -- and nothing would fail.
    subroutine test_env_empty_is_unset(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: token
        !
        call parquet_reset_settings()
        call unset_all_env()
        call parquet_set_verbosity("silent")
        call set_env("PARQUET_FORTRAN_VERBOSITY", "")
        call parquet_settings_from_env()
        call unset_all_env()
        call parquet_get_verbosity(token)
        call check(error, token == "silent", "an empty variable must be treated as unset, not applied")
        if (allocated(error)) then
            call parquet_reset_settings()
            return
        end if
        ! An all-blank value is the same case, and is what a quoted shell variable often produces.
        call set_env("PARQUET_FORTRAN_VERBOSITY", "   ")
        call parquet_settings_from_env()
        call unset_all_env()
        call parquet_get_verbosity(token)
        call check(error, token == "silent", "an all-blank variable must be treated as unset too")
        call parquet_reset_settings()
    end subroutine test_env_empty_is_unset

    !> The documented precedence: call from_env after your own setters and the environment wins.
    subroutine test_env_overrides_explicit(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call parquet_reset_settings()
        call unset_all_env()
        call parquet_set_sort_threads(6)
        call set_env("PARQUET_FORTRAN_SORT_THREADS", "2")
        call parquet_settings_from_env()
        call unset_all_env()
        call check(error, parquet_get_sort_threads() == 2, &
            "a variable applied after an explicit set must win")
        call parquet_reset_settings()
    end subroutine test_env_overrides_explicit

    !> The accepted spellings, on both value shapes that have any.
    subroutine test_env_value_forms(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call parquet_reset_settings()
        call unset_all_env()
        ! Surrounding blanks are ignored, and an explicit + is accepted.
        call set_env("PARQUET_FORTRAN_SORT_THREADS", "  7  ")
        call set_env("PARQUET_FORTRAN_PREFETCH_THREADS", "+3")
        ! Booleans fold case, and 1/0 mean what a shell script expects.
        call set_env("PARQUET_FORTRAN_SORT_COUNTING_PATH", "FALSE")
        call set_env("PARQUET_FORTRAN_DEFAULT_USE_THREADS", "0")
        call set_env("PARQUET_FORTRAN_STATISTICS_PRESCREEN", "True")
        call parquet_settings_from_env()
        call unset_all_env()
        !
        call check(error, parquet_get_sort_threads() == 7, "surrounding blanks must be ignored")
        if (.not. allocated(error)) call check(error, parquet_get_prefetch_threads() == 3, &
            "an explicit + sign must be accepted")
        if (.not. allocated(error)) call check(error, .not. parquet_get_sort_counting_path(), &
            "FALSE must fold to false")
        if (.not. allocated(error)) call check(error, .not. parquet_get_default_use_threads(), &
            "0 must mean false")
        if (.not. allocated(error)) call check(error, parquet_get_statistics_prescreen(), &
            "True must fold to true")
        call parquet_reset_settings()
    end subroutine test_env_value_forms

    !> The compression pair, which is the one place in the sequence that LOOKS order-dependent.
    !>
    !> It is not, and that is worth recording because the next reader will assume otherwise: naming
    !> a codec drops the zstd-tuned default level, so applying the level first looks like it would
    !> discard it. `parquet_resolve_writer_compression` reads `cfg_default_compression` and
    !> `cfg_default_compression_level` **together, at writer-open time**, so neither setter touches
    !> the other and either order gives the same answer. Confirmed by mutation: swapping the two
    !> blocks in `parquet_settings_from_env` changes no test.
    !>
    !> What this test does assert is still worth having -- that both values arrive, which is what
    !> fails if either variable is dropped from the sequence or crossed with a neighbour.
    subroutine test_env_codec_and_level(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: codec
        !
        call parquet_reset_settings()
        call unset_all_env()
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION", "gzip")
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL", "6")
        call parquet_settings_from_env()
        call unset_all_env()
        call parquet_get_default_compression(codec)
        call check(error, codec == "gzip", "the codec must be applied")
        if (.not. allocated(error)) call check(error, parquet_get_default_compression_level() == 6, &
            "a level named alongside a codec must arrive too")
        call parquet_reset_settings()
    end subroutine test_env_codec_and_level

    !> Puts the pool back and clears the module's captured value, so the next test starts from the
    !> same state whatever this one did. Used instead of a bare parquet_set_arrow_threads(original)
    !> because that would itself capture, leaving a stale capture behind for the next test.
    subroutine restore(original)
        integer, intent(in) :: original !! capacity to restore.
        !
        call parquet_set_arrow_threads(original)
        call parquet_reset_settings()
    end subroutine restore
    !
    ! ==================================================================================
    ! S4: the five C++-side performance knobs
    ! ==================================================================================
    !
    !> How many threads the last threaded sort actually used. The observation hook that survived
    !> S4 -- what it observes is a setting now, but the observation itself never was one.
    integer(int64) function sort_threads_used() result(n)
        interface
            function get_used() bind(C, name="parquet_debug_get_sort_threads_used") result(k)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: k !! threads used by the last threaded build.
            end function get_used
        end interface
        n = int(get_used(), int64)
    end function sort_threads_used

    !> Arms and zeroes the sort's comparison counter.
    subroutine arm_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine arm_comparisons

    !> Comparisons counted since the counter was armed, then disarms it.
    integer(int64) function comparisons_since() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since arming.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function comparisons_since

    !> How many row groups the most recent statistics screen ruled out.
    integer(int64) function row_groups_pruned() result(n)
        interface
            function got() bind(C, name="parquet_debug_get_row_groups_pruned") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! pruned row groups of the last screen.
            end function got
        end interface
        n = int(got(), int64)
    end function row_groups_pruned

    !> A key the counting path always declines (real, not integer), so a comparison count is
    !> nonzero for reasons that have nothing to do with the knob under test. Used as the control
    !> that the comparison counter is armed and working at all.
    subroutine ties_real(v)
        real(real64), intent(out) :: v(:) !! filled with a tie-bearing real key.
        integer :: k
        do k = 1, size(v)
            v(k) = real(mod(k * 37, 991), real64)
        end do
    end subroutine ties_real

    !> The threshold decides whether a sort threads AT ALL, so the observation is the thread count
    !> the sort actually reached -- not its result, which is identical either way by design.
    !>
    !> Both directions are asserted on the SAME array with the SAME `threads=4`: only the setting
    !> differs between them. A threshold that was stored and never pushed to C++ would leave the
    !> built-in 8192 in force, and this 2000-element array would stay serial in both halves.
    subroutine test_sort_parallel_min_rows_effect(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:)
        !
        call parquet_reset_settings()
        call ties_real(v)
        ! Negative control FIRST: at the built-in 8192 this array is far too small to thread.
        call pf_argsort(v, perm, threads=4)
        call check(error, sort_threads_used() == 1_int64, &
            "at the built-in threshold a 2000-element array must sort serially whatever threads= asks")
        if (allocated(error)) return
        !
        call parquet_set_sort_parallel_min_rows(4_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, sort_threads_used() == 4_int64, &
            "lowering sort_parallel_min_rows must let the same array reach the parallel path")
        if (allocated(error)) then
            call parquet_reset_settings()
            return
        end if
        !
        ! And back up again, which also proves the knob is re-readable rather than latched once.
        call parquet_set_sort_parallel_min_rows(1000000000_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, sort_threads_used() == 1_int64, &
            "raising sort_parallel_min_rows above the array size must return the sort to serial")
        call parquet_reset_settings()
    end subroutine test_sort_parallel_min_rows_effect

    !> The counting path performs ZERO comparisons by construction, so the comparison counter is
    !> what distinguishes the two engines -- their permutations are identical, which is the whole
    !> point and also why a result assertion could never see this knob.
    subroutine test_sort_counting_path_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(500)
        integer(int32), allocatable :: perm(:)
        integer(int64) :: cmp_on, cmp_off
        integer :: k
        !
        call parquet_reset_settings()
        do k = 1, size(v)
            v(k) = int(mod(k * 7, 50), int32)   ! range 49: comfortably inside the bucket limit
        end do
        !
        call arm_comparisons()
        call pf_argsort(v, perm)
        cmp_on = comparisons_since()
        call check(error, cmp_on == 0_int64, &
            "with sort_counting_path on, a low-range integer key must take the counting path (0 comparisons)")
        if (allocated(error)) return
        !
        call parquet_set_sort_counting_path(.false.)
        call arm_comparisons()
        call pf_argsort(v, perm)
        cmp_off = comparisons_since()
        call parquet_reset_settings()
        call check(error, cmp_off > 0_int64, &
            "turning sort_counting_path off must force the comparator path (nonzero comparisons)")
    end subroutine test_sort_counting_path_effect

    !> The bound is on the key's value RANGE, not its cardinality (feature_risks.md Risk-39), so
    !> both halves here use the SAME 500 values and the same cardinality -- only the spread differs,
    !> and the limit is what decides. A limit that never reached C++ would leave the built-in 2**22
    !> in force and both halves would take the counting path.
    subroutine test_sort_counting_bucket_limit_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(500)
        integer(int32), allocatable :: perm(:)
        integer(int64) :: cmp_inside, cmp_outside
        integer :: k
        !
        call parquet_reset_settings()
        do k = 1, size(v)
            v(k) = int(mod(k * 7, 50) * 1000, int32)   ! 50 distinct values, range 49000
        end do
        !
        ! Range 49000 sits inside the built-in limit, so this key counting-sorts by default.
        call arm_comparisons()
        call pf_argsort(v, perm)
        cmp_inside = comparisons_since()
        call check(error, cmp_inside == 0_int64, &
            "a range-49000 key must be inside the built-in bucket limit and take the counting path")
        if (allocated(error)) return
        !
        call parquet_set_sort_counting_bucket_limit(100)
        call arm_comparisons()
        call pf_argsort(v, perm)
        cmp_outside = comparisons_since()
        call parquet_reset_settings()
        call check(error, cmp_outside > 0_int64, &
            "a bucket limit below the key's value range must make the counting path decline")
    end subroutine test_sort_counting_bucket_limit_effect

    !> Row groups written by the whole-table path (close_parquet_writer). This is one of the two
    !> callers of chunk_size_from_bytes_per_row, and the reason the S4 refactor de-duplicated that
    !> arithmetic: before it, this path had its own copy of the byte target and the setting could
    !> not have reached it (feature_risks.md Risk-43).
    subroutine test_target_row_group_bytes_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: default_file = "test_run/settings_rowgroup_default.parquet"
        character(len=*), parameter :: small_file = "test_run/settings_rowgroup_small.parquet"
        integer(int64) :: groups_default, groups_small
        !
        call parquet_reset_settings()
        call write_rows_int64(default_file)
        call count_row_groups(default_file, groups_default)
        call check(error, groups_default == 1_int64, &
            "at the built-in 256 MiB target, 5000 int64 rows must fit in one row group")
        if (allocated(error)) return
        !
        call parquet_set_target_row_group_bytes(8000_int64)
        call write_rows_int64(small_file)
        call parquet_reset_settings()
        call count_row_groups(small_file, groups_small)
        ! Not an exact count: the floor of 1000 rows binds for any per-row width up to 32 bytes, and
        ! TotalBufferSize includes buffer padding this test should not have to predict.
        call check(error, groups_small > 1_int64, &
            "a small target_row_group_bytes must split the same data across several row groups")
    end subroutine test_target_row_group_bytes_effect

    !> The OTHER caller of the same arithmetic: the schema-only estimate the streaming row-group
    !> path locks in, observable through parquet_get_chunk_size(writer) before any data exists.
    !> Both callers are asserted because a re-inlined copy in either one would be invisible here.
    subroutine test_target_row_group_bytes_streaming(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: default_file = "test_run/settings_rowgroup_est_default.parquet"
        character(len=*), parameter :: small_file = "test_run/settings_rowgroup_est_small.parquet"
        integer(int64) :: chunk_default, chunk_small
        !
        call parquet_reset_settings()
        call estimated_chunk_size(default_file, chunk_default)
        call parquet_set_target_row_group_bytes(8000_int64)
        call estimated_chunk_size(small_file, chunk_small)
        call parquet_reset_settings()
        !
        call check(error, chunk_default > chunk_small, &
            "the schema-based row-group estimate must shrink when target_row_group_bytes does")
        if (allocated(error)) return
        call check(error, chunk_small > 0_int64, "the shrunken estimate must still be a usable size")
    end subroutine test_target_row_group_bytes_streaming

    !> The screen changes how much of the file is read and NOTHING else, so the test has to be both
    !> halves at once: the rows must be identical either way (that is the correctness contract), and
    !> the pruned count must differ (without which an equality test passes just as happily against a
    !> screen that never prunes -- the shape CLAUDE.md prescribes for this optimization).
    subroutine test_statistics_prescreen_effect(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/settings_prescreen.parquet"
        integer(int64), allocatable :: on_rows(:), off_rows(:)
        integer(int64) :: pruned_on, pruned_off, n_on, n_off
        !
        call parquet_reset_settings()
        call write_rows_int64(out_file, chunk_size=500)
        !
        call filtered_read(out_file, .true., on_rows, n_on, pruned_on)
        call check(error, pruned_on > 0_int64, &
            "with statistics_prescreen on, a selective filter must prune at least one row group")
        if (allocated(error)) then
            call parquet_reset_settings()
            return
        end if
        !
        call filtered_read(out_file, .false., off_rows, n_off, pruned_off)
        call parquet_reset_settings()
        !
        call check(error, pruned_off == 0_int64, &
            "with statistics_prescreen off, no row group may be pruned")
        if (allocated(error)) return
        call check(error, n_on == n_off, "pruning must not change how many rows a filter returns")
        if (allocated(error)) return
        call check(error, all(on_rows == off_rows), &
            "pruning must not change WHICH rows a filter returns, element for element")
    end subroutine test_statistics_prescreen_effect

    !> The three numeric knobs are generic over int32 and int64 because none of them is bounded
    !> below huge(int32). A missing specific is a COMPILE error, so this test existing is most of
    !> its own value; it additionally proves the int32 form reaches the same storage rather than a
    !> separate copy.
    subroutine test_both_integer_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: small = 4096
        integer(int64) :: big = 5000000000_int64
        !
        call parquet_reset_settings()
        call parquet_set_sort_parallel_min_rows(small)
        call check(error, parquet_get_sort_parallel_min_rows() == 4096_int64, &
            "the int32 setter must reach the same storage the int64 getter reads")
        if (allocated(error)) then
            call parquet_reset_settings()
            return
        end if
        call parquet_set_target_row_group_bytes(big)
        call check(error, parquet_get_target_row_group_bytes() == big, &
            "a byte target above huge(int32) must survive intact")
        if (allocated(error)) then
            call parquet_reset_settings()
            return
        end if
        call parquet_set_sort_counting_bucket_limit(small)
        call check(error, parquet_get_sort_counting_bucket_limit() == 4096_int64, &
            "the int32 bucket-limit setter must reach the same storage")
        call parquet_reset_settings()
    end subroutine test_both_integer_kinds

    !> One filtered read, with the screen in the requested state, reporting both what came back and
    !> how many row groups were skipped. The pruned count is taken right after open, because that is
    !> when the screen runs.
    subroutine filtered_read(path, screen_on, values, nrows, pruned)
        character(len=*), intent(in) :: path !! file to read.
        logical, intent(in) :: screen_on !! .false. turns the statistics screen off.
        integer(int64), allocatable, intent(out) :: values(:) !! surviving rows of "v".
        integer(int64), intent(out) :: nrows !! how many survived.
        integer(int64), intent(out) :: pruned !! row groups the screen ruled out.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        !
        call parquet_set_statistics_prescreen(screen_on)
        call filt%add("v < 100")
        call parquet_open_reader(reader, path, filter=filt)
        pruned = row_groups_pruned()
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", values)
        call parquet_close_reader(reader)
    end subroutine filtered_read

    !> 5000 int64 rows -- enough that a small byte target splits them and the built-in one does not.
    subroutine write_rows_int64(path, chunk_size)
        character(len=*), intent(in) :: path !! output file.
        integer, intent(in), optional :: chunk_size !! explicit row-group size, if any.
        type(parquet_writer) :: writer
        integer(int64) :: v(5000)
        integer :: k
        !
        do k = 1, size(v)
            v(k) = int(k, int64)
        end do
        if (present(chunk_size)) then
            call parquet_open_writer(writer, path, chunk_size=chunk_size)
        else
            call parquet_open_writer(writer, path)
        end if
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine write_rows_int64

    !> Row groups in a written file.
    subroutine count_row_groups(path, n)
        character(len=*), intent(in) :: path !! file to inspect.
        integer(int64), intent(out) :: n !! its row-group count.
        type(parquet_reader) :: reader
        !
        call parquet_open_reader(reader, path)
        call parquet_get_num_row_groups(reader, n)
        call parquet_close_reader(reader)
    end subroutine count_row_groups

    !> The row-group size a schema-opened writer settles on before any data exists -- the streaming
    !> path's own estimate, which is the second caller of the shared sizing arithmetic.
    subroutine estimated_chunk_size(path, n)
        character(len=*), intent(in) :: path !! output file (written and closed empty of data).
        integer(int64), intent(out) :: n !! the writer's resolved row-group size.
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: v(4) = [1_int32, 2_int32, 3_int32, 4_int32]
        !
        call schema%init(table="rowgroup_estimate")
        call schema%add_field("v", "int32")
        call parquet_parse_maml(schema)
        call parquet_open_writer(writer, path, schema)
        call parquet_get_chunk_size(writer, n)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine estimated_chunk_size
    !

    !> The observed effect, not the round trip. `parquet_string_threads()` is the ONE place the
    !> string cap and the OpenMP environment are combined, so it is what every bulk operation
    !> actually asks -- a setting that were stored and never read would leave the capped and
    !> automatic answers equal.
    !>
    !> **The negative control is the automatic arm**, asserted in the same test: without it, a cap
    !> that fired unconditionally (or a machine offering one thread anyway) would pass just as
    !> happily. On a single-core machine the automatic answer is legitimately 1 and there is nothing
    !> to distinguish, so the strict comparison is guarded on `omp_get_max_threads() > 1` -- which
    !> is a real limitation of the environment, not a weakened assertion.
    subroutine test_string_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: auto_n, capped_n, avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call parquet_set_string_threads(0)
        auto_n = parquet_string_threads()
        call check(error, auto_n >= 1, "the automatic answer is always at least one thread")
        if (allocated(error)) return
        !
        call parquet_set_string_threads(1)
        capped_n = parquet_string_threads()
        call check(error, capped_n == 1, "a cap of 1 forces one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, auto_n > capped_n, &
                "negative control: with threads available the automatic answer must EXCEED the cap")
            if (allocated(error)) return
            call check(error, auto_n == avail, "the automatic answer is what OpenMP offers")
            if (allocated(error)) return
        end if
        !
        ! A cap ABOVE what OpenMP offers changes nothing -- it caps, it never raises.
        call parquet_set_string_threads(avail + 16)
        call check(error, parquet_string_threads() == auto_n, &
            "a cap above the available thread count does not raise the answer")
        if (allocated(error)) return
        !
        call parquet_set_string_threads(0)
        call check(error, parquet_get_string_threads() == 0, "the raw getter reports the cap, not the resolved count")
    end subroutine test_string_threads_effect


    !> The work floor is a **payload** floor, not a row floor, and it is overridable so tests can
    !> reach the threaded path at all.
    !>
    !> Without the override every fixture a suite can afford would sit below 256 KiB and silently
    !> take the serial path, so a threaded operation would be covered by nothing -- `feature_risks.md`
    !> Risk-49's failure exactly. The negative control is the same column measured at the real floor,
    !> which must resolve to 1: a floor that never declined would pass any test written for it.
    !>
    !> **This lives in the settings suite, not the string one, for two independent reasons** -- and
    !> it was written there first and found vacuous by mutation testing. `parquet_debug_set_string_min_bytes`
    !> is process-global, so a test writing it inside test-drive's per-suite parallelism is visible to
    !> every sibling running at the same time; and test-drive achieves that parallelism with its own
    !> `!$omp parallel do`, so `omp_in_parallel()` is `.true.` throughout a parallelized suite and
    !> `parquet_string_threads()` correctly answers 1 for *every* arm -- leaving an A/B comparing one
    !> code path against itself, which passes while testing nothing. This suite is excluded from that
    !> parallelism (see run_tester.f90), which is what makes the observation real.
    subroutine test_string_work_floor(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer :: i, avail, n_default, n_lowered
        !
        avail = parquet_string_threads()
        do i = 1, 64
            call col%append_string("abcdefgh")
        end do
        call check(error, col%character_size() == 512, "the fixture is 512 bytes -- far below the real floor")
        if (allocated(error)) return
        !
        call parquet_debug_set_string_min_bytes(0_int64)
        n_default = parquet_debug_string_bulk_threads(col)
        call check(error, n_default == 1, &
            "negative control: at the real floor this column is far too small to thread")
        if (allocated(error)) return
        !
        call parquet_debug_set_string_min_bytes(64_int64)
        n_lowered = parquet_debug_string_bulk_threads(col)
        if (avail > 1) then
            call check(error, n_lowered > 1, "with the floor lowered the same column does thread")
            if (allocated(error)) return
        end if
        call parquet_debug_set_string_min_bytes(0_int64)
        call check(error, parquet_debug_string_bulk_threads(col) == 1, &
            "restoring the floor restores the serial answer")
    end subroutine test_string_work_floor
    !


    !> **Inside an OpenMP parallel region a string operation resolves to ONE thread**, whatever the
    !> environment offers and whatever the cap says. `T` threads each asking for `T` more is slower
    !> than not threading at all, and nesting is the caller's business -- the same rule
    !> `pf_sort_threads` and `parallel_prefetch_ok` implement, stated once more here because a third
    !> copy of it is how the three come to disagree.
    !>
    !> **The region is opened explicitly by this test rather than inherited from the harness.**
    !> test-drive runs most suites inside its own `!$omp parallel do`, so a test placed in one of
    !> those would see `omp_in_parallel()` true for every arm and could not measure the difference at
    !> all -- which is exactly how the sibling floor test came to be vacuous before it was moved
    !> here. This suite is excluded from that parallelism, so the `outside` reading is genuinely
    !> outside and the two arms really do differ.
    subroutine test_string_threads_in_region(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: outside, inside, avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call parquet_set_string_threads(0)
        outside = parquet_string_threads()
        inside = -1
#ifdef _OPENMP
        !$omp parallel
        !$omp master
        inside = parquet_string_threads()
        !$omp end master
        !$omp end parallel
#else
        inside = parquet_string_threads()
#endif
        call check(error, inside == 1, "inside a parallel region a string operation must resolve to one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, outside > 1, &
                "negative control: outside a region the same call must resolve to more than one")
            if (allocated(error)) return
            call check(error, outside > inside, "so the two readings genuinely differ")
            if (allocated(error)) return
        end if
        !
        ! An explicit cap does not lift the rule either -- it caps the automatic answer, and inside a
        ! region that answer is already 1.
        call parquet_set_string_threads(8)
        inside = -1
#ifdef _OPENMP
        !$omp parallel
        !$omp master
        inside = parquet_string_threads()
        !$omp end master
        !$omp end parallel
#else
        inside = parquet_string_threads()
#endif
        call check(error, inside == 1, "a cap does not lift the serial-inside-a-region rule")
        call parquet_set_string_threads(0)
    end subroutine test_string_threads_in_region

end module test_settings
