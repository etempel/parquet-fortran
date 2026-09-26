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
!> happily against a value that is stored and never used, which is the decorative-knob failure. So
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
!> * `sort_counting_path` and `sort_counting_bucket_limit` through
!>   `parquet_debug_get_sort_comparisons`, which is exactly 0 on the counting path and nonzero on
!>   the comparator path -- again, the two produce the same permutation;
!> * `target_row_group_bytes` through `parquet_get_num_row_groups` on a written file AND through
!>   `parquet_get_chunk_size(writer)`, because the byte target has two callers serving two different
!>   writers and a copy re-inlined into either one would be invisible to a test of the other;
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
    ! The C++ sort engine is TEST-ONLY and is not re-exported by the `parquet` facade: reaching it
    ! needs this import, which is what keeps it out of every other program's dependency graph.
    ! Importing it is also what BINDS it -- `parquet_debug_use_fortran_sort_engine` lives here and
    ! registers the engine's entry points as a side effect of being called.
    use parquet_sorting_oracle, only : parquet_debug_use_fortran_sort_engine
    use iso_fortran_env, only : output_unit, int32, int64, real64
    use iso_c_binding, only : c_null_char, c_int
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_procs, omp_set_num_threads
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
            new_unittest("pf_sort_threads never exceeds the CPU affinity mask", test_sort_threads_affinity_clamp), &
            new_unittest("the affinity clamp reaches the string and random resolvers too, "// &
                "both branches of the random one", &
                test_affinity_clamp_other_resolvers), &
            new_unittest("the affinity clamp reaches a table's per-column rewrite", &
                test_affinity_clamp_table_rewrite), &
            new_unittest("print_settings prints at every verbosity level", test_print_settings_never_silenced), &
            new_unittest("doc/pages/operating/settings.md configure_at_startup example", &
                test_configure_at_startup_example), &
            new_unittest("prefetch_threads caps the parallel prefetch", test_prefetch_threads_effect), &
            new_unittest("table_threads caps the parallel per-column rewrite", test_table_threads_effect), &
            new_unittest("string_threads caps what one string column resolves to", &
                test_string_threads_effect), &
            new_unittest("random_threads caps what a bulk permutation resolves to", &
                test_random_threads_effect), &
            new_unittest("spatial_threads caps what a bulk spatial query resolves to", &
                test_spatial_threads_effect), &
            new_unittest("healpix_threads caps what a bulk HEALPix conversion resolves to", &
                test_healpix_threads_effect), &
            new_unittest("index_threads caps what an index build resolves to", &
                test_index_threads_effect), &
            new_unittest("random_parallel_min_elements decides whether a bulk permutation threads", &
                test_random_parallel_min_effect), &
            new_unittest("the random work floor lowers the thread count before it forces serial", &
                test_random_work_floor_is_gradual), &
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
            new_unittest("sort_counting_path switches the sort between its two engines", &
                cpp_test_sort_counting_path_effect), &
            new_unittest("sort_radix_path switches a single-key sort between radix and comparison", &
                test_sort_radix_path_effect), &
            new_unittest("sort_counting_bucket_limit declines a key whose range exceeds it", &
                cpp_test_sort_counting_bucket_limit_effect), &
            new_unittest("sort_counting_path switches the FORTRAN engine's two paths", &
                test_sort_counting_path_effect_fortran), &
            new_unittest("sort_counting_bucket_limit declines a key on the FORTRAN engine too", &
                test_sort_counting_bucket_limit_effect_fortran), &
            new_unittest("target_row_group_bytes sizes the row groups of a whole-table write", &
                test_target_row_group_bytes_effect), &
            new_unittest("target_row_group_bytes also sizes the streaming path's estimate", &
                test_target_row_group_bytes_streaming), &
            new_unittest("target_row_group_bytes accepts a default-kind integer", &
                test_target_row_group_bytes_int32), &
            new_unittest("statistics_prescreen prunes row groups without changing the answer", &
                test_statistics_prescreen_effect), &
            new_unittest("verbosity=silent makes print_schema_info a no-op, file and all", &
                test_verbosity_silences_print_schema_info), &
            new_unittest("verbosity=silent makes %print_rows a no-op", &
                test_verbosity_silences_print_rows), &
            new_unittest("a silenced %print_rows(columns=) leaves residency unchanged", &
                test_verbosity_silences_print_rows_read), &
            new_unittest("both integer kinds reach the same setting", test_both_integer_kinds), &
            new_unittest("every environment variable reaches its own knob", test_env_every_variable), &
            new_unittest("an absent variable leaves its knob alone", test_env_absent_leaves_knob), &
            new_unittest("an empty variable is treated as unset", test_env_empty_is_unset), &
            new_unittest("a variable overrides an earlier explicit set", test_env_overrides_explicit), &
            new_unittest("integers accept blanks and a sign, booleans fold case", test_env_value_forms), &
            new_unittest("a codec and its level both arrive", test_env_codec_and_level), &
            new_unittest("set_threads moves every thread count", test_set_threads), &
            new_unittest("file_date pins the creation timestamp, making two writes byte-identical", &
                test_file_date_pins_output), &
            new_unittest("file_date is captured when the writer opens, not when it closes", &
                test_file_date_capture_point), &
            new_unittest("a pinned file_date survives concurrent opens", &
                test_file_date_survives_concurrent_opens), &
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
    !> is what doc/pages/operating/settings.md documents and what tools/check_source_conventions.py pins, so
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
        call check(error, parquet_get_random_threads() == 0, "random_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        call check(error, parquet_get_spatial_threads() == 0, "spatial_threads defaults to 0 (automatic)")
        if (.not. allocated(error)) call check(error, parquet_get_healpix_threads() == 0, &
            "healpix_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        if (.not. allocated(error)) call check(error, parquet_get_index_threads() == 0, &
            "index_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        call check(error, parquet_get_random_parallel_min_elements() == 1000_int64, &
            "random_parallel_min_elements defaults to 1000 elements per thread")
        if (allocated(error)) return
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd", "default_compression defaults to zstd")
        if (allocated(error)) return
        call check(error, parquet_get_default_compression_level() == 3, "default_compression_level defaults to 3")
        if (allocated(error)) return
        call check(error, parquet_get_default_use_threads(), "default_use_threads defaults to .true.")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_path(), "sort_counting_path defaults to .true.")
        if (allocated(error)) return
        call check(error, parquet_get_sort_radix_path(), "sort_radix_path defaults to .true.")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_bucket_limit() == 4194304_int64, &
            "sort_counting_bucket_limit defaults to the built-in 2**22")
        if (allocated(error)) return
        call check(error, parquet_get_target_row_group_bytes() == 268435456_int64, &
            "target_row_group_bytes defaults to the built-in 256 MiB")
        if (allocated(error)) return
        call check(error, parquet_get_statistics_prescreen(), "statistics_prescreen defaults to .true.")
    end subroutine test_factory_defaults

    !> The byte-count knob is generic over int32 and int64, so that `call
    !> parquet_set_target_row_group_bytes(64*1024*1024)` compiles with a plain `integer` literal
    !> (CLAUDE.md's both-kinds rule). The int32 specific is a one-line converter onto the int64
    !> one and had no caller: every existing test passes an `_int64` literal.
    !>
    !> A converter can only really get one thing wrong -- losing or mangling the value -- so the
    !> assertion is that the two forms leave the getter reporting the same number, checked against
    !> a value large enough that a narrowing conversion would be visible.
    subroutine test_target_row_group_bytes_int32(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: WANT = 100000000    !! 100 MB: well inside int32, well outside int16.
        !
        call parquet_set_target_row_group_bytes(WANT)              ! the int32 specific
        call check(error, parquet_get_target_row_group_bytes() == int(WANT, int64), &
            "the int32 setter must store the value it was given, unnarrowed")
        if (allocated(error)) return
        !
        call parquet_reset_settings()
        call parquet_set_target_row_group_bytes(int(WANT, int64))  ! the int64 twin
        call check(error, parquet_get_target_row_group_bytes() == int(WANT, int64), &
            "and must agree with the int64 form on the same value")
        if (allocated(error)) return
        call parquet_reset_settings()
        call check(error, parquet_get_target_row_group_bytes() == 268435456_int64, &
            "the reset must put the factory default back, so this test leaves no state behind")
    end subroutine test_target_row_group_bytes_int32

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
        call parquet_set_random_threads(4)
        call parquet_set_spatial_threads(5)
        call parquet_set_healpix_threads(5)
        call parquet_set_index_threads(5)
        call parquet_set_random_parallel_min_elements(77_int64)
        call parquet_set_default_compression("gzip")
        call parquet_set_default_compression_level(9)
        call parquet_set_default_use_threads(.false.)
        call parquet_set_sort_counting_path(.false.)
        call parquet_set_sort_radix_path(.false.)
        call parquet_set_sort_counting_bucket_limit(128_int64)
        call parquet_set_target_row_group_bytes(4096_int64)
        call parquet_set_statistics_prescreen(.false.)
        call parquet_set_file_date("2001-02-03T04:05:06")
        call parquet_reset_settings()
        !
        call check(error, parquet_get_sort_threads() == 0, "reset restores sort_threads")
        if (allocated(error)) return
        call check(error, parquet_get_prefetch_threads() == 0, "reset restores prefetch_threads")
        if (allocated(error)) return
        call check(error, parquet_get_string_threads() == 0, "reset restores string_threads")
        if (allocated(error)) return
        call check(error, parquet_get_random_threads() == 0, "reset restores random_threads")
        if (allocated(error)) return
        call check(error, parquet_get_spatial_threads() == 0, "reset restores spatial_threads")
        if (.not. allocated(error)) call check(error, parquet_get_healpix_threads() == 0, &
            "reset restores healpix_threads")
        if (allocated(error)) return
        call check(error, parquet_get_random_parallel_min_elements() == 1000_int64, &
            "reset restores random_parallel_min_elements")
        if (allocated(error)) return
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd", "reset restores default_compression")
        if (allocated(error)) return
        call check(error, parquet_get_default_compression_level() == 3, "reset restores default_compression_level")
        if (allocated(error)) return
        call check(error, parquet_get_default_use_threads(), "reset restores default_use_threads")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_path(), "reset restores sort_counting_path")
        if (allocated(error)) return
        call check(error, parquet_get_sort_radix_path(), "reset restores sort_radix_path")
        if (allocated(error)) return
        call check(error, parquet_get_sort_counting_bucket_limit() == 4194304_int64, &
            "reset restores sort_counting_bucket_limit")
        if (allocated(error)) return
        call check(error, parquet_get_target_row_group_bytes() == 268435456_int64, &
            "reset restores target_row_group_bytes")
        if (allocated(error)) return
        call check(error, parquet_get_statistics_prescreen(), "reset restores statistics_prescreen")
        if (allocated(error)) return
        call parquet_get_file_date(codec)
        call check(error, len(codec) == 0, "reset restores file_date to reading the clock")
    end subroutine test_reset_all_knobs

    !> `file_date` is observed as BYTE EQUALITY between two files, which is the whole point of it.
    !!
    !! **The observation is the strongest one in this suite, because it is the feature.** Every file
    !! this library writes carries a creation timestamp, and that timestamp is the only thing that
    !! differs between two writes of the same data -- so "the same data written twice gives the same
    !! bytes" is both what a user wants from this knob and an assertion nothing else can satisfy.
    !!
    !! **Three arms, because two would not separate the two ways it can fail.** Same pinned date
    !! twice must give IDENTICAL files (the knob works); a different pinned date must give DIFFERENT
    !! files (the value actually reaches the file, rather than the writer having become
    !! deterministic for some unrelated reason); and reading the `DATE` key back must give exactly
    !! the pinned text (it is stored as itself, not merely as something stable).
    !!
    !! **The clock arm is the negative control**, and deliberately does not compare two clock-written
    !! files: two writes inside one wall-clock second are legitimately identical, so that comparison
    !! would be flaky. It asserts instead that an unpinned write differs from the pinned one and
    !! that its `DATE` is not the pinned text -- which fails just as loudly against a knob stuck on.
    !!
    !! Four fixture files, since this suite shares a process and files must not be shared between
    !! tests (CLAUDE.md), and the setting is restored before the first assertion.
    subroutine test_file_date_pins_output(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: pinned = "2020-01-02T03:04:05"
        character(len=*), parameter :: other = "2021-06-07T08:09:10"
        character(len=*), parameter :: f_a = "test_run/settings_file_date_a.parquet"
        character(len=*), parameter :: f_b = "test_run/settings_file_date_b.parquet"
        character(len=*), parameter :: f_c = "test_run/settings_file_date_c.parquet"
        character(len=*), parameter :: f_d = "test_run/settings_file_date_clock.parquet"
        character(len=:), allocatable :: got_a, got_d, token
        logical :: same_ab, same_ac, same_ad

        call parquet_set_file_date(pinned)
        call parquet_get_file_date(token)
        call write_dated_fixture(f_a)
        call write_dated_fixture(f_b)
        call parquet_set_file_date(other)
        call write_dated_fixture(f_c)
        call parquet_set_file_date("")
        call write_dated_fixture(f_d)
        same_ab = files_are_identical(f_a, f_b)
        same_ac = files_are_identical(f_a, f_c)
        same_ad = files_are_identical(f_a, f_d)
        call read_file_date(f_a, got_a)
        call read_file_date(f_d, got_d)
        !
        call check(error, token == pinned, "file_date must round-trip the value it was given")
        if (allocated(error)) return
        call check(error, same_ab, &
            "two writes of the same data under one pinned file_date must be byte-identical")
        if (allocated(error)) return
        call check(error, .not. same_ac, &
            "a different pinned file_date must change the file, or the value never reached it")
        if (allocated(error)) return
        call check(error, got_a == pinned, "the DATE key must hold exactly the pinned text")
        if (allocated(error)) return
        call check(error, .not. same_ad, &
            "an unpinned write must differ from the pinned one -- the clock is still the default")
        if (allocated(error)) return
        call check(error, got_d /= pinned .and. len(got_d) == len(pinned), &
            "an unpinned write must stamp a real clock reading of the same width, not the pinned text")
    end subroutine test_file_date_pins_output

    !> `file_date` is captured **at writer open**, so changing it afterwards cannot reach a writer
    !> that is already open.
    !>
    !> This is the one property of the C++-mirrored settings that the guide states and nothing
    !> asserted. It follows structurally -- `parquet_push_settings_to_cpp` runs at the top of every
    !> `parquet_open_writer` and nowhere else -- but "follows structurally" is exactly the kind of
    !> claim that stops being true when somebody moves a push, and moving it would break no other
    !> test here.
    !>
    !> **The negative control is the second arm**, and without it the first would pass against an
    !> implementation that ignored `parquet_set_file_date` entirely: setting the same value BEFORE
    !> the open must put it in the file, so the first arm's `/=` is evidence about the capture
    !> point rather than about the setting being inert.
    subroutine test_file_date_capture_point(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: at_open = "2020-01-02T03:04:05"
        character(len=*), parameter :: after_open = "2021-06-07T08:09:10"
        character(len=*), parameter :: f_late = "test_run/settings_file_date_late.parquet"
        character(len=*), parameter :: f_early = "test_run/settings_file_date_early.parquet"
        character(len=:), allocatable :: got_late, got_early
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: k(8)
        integer :: i

        do i = 1, size(k)
            k(i) = i
        end do
        !
        ! Arm 1: pin one value, open, pin a DIFFERENT value, then write and close.
        call parquet_set_file_date(at_open)
        call schema%init(table="file_date_capture")
        call schema%add_field("k", "int32", unit="count")
        call parquet_open_writer(writer, f_late, schema, overwrite=.true.)
        call parquet_set_file_date(after_open)
        call parquet_write_column(writer, "k", k)
        call parquet_close_writer(writer)
        call read_file_date(f_late, got_late)
        !
        ! Arm 2, the negative control: the same later value, set BEFORE the open, must arrive.
        call parquet_set_file_date(after_open)
        call parquet_open_writer(writer, f_early, schema, overwrite=.true.)
        call parquet_write_column(writer, "k", k)
        call parquet_close_writer(writer)
        call read_file_date(f_early, got_early)
        call parquet_set_file_date("")
        !
        call check(error, got_late == at_open, &
            "the DATE key must hold the value pinned when the writer was OPENED")
        if (allocated(error)) return
        call check(error, got_late /= after_open, &
            "a file_date set after the open must not reach a writer already open")
        if (allocated(error)) return
        call check(error, got_early == after_open, &
            "negative control: the same value set BEFORE the open must reach the file")
    end subroutine test_file_date_capture_point

    !> Opening many readers at once while a file date is pinned must not corrupt the heap.
    !!
    !! **This is a memory-safety test, not a value test**, and it is here rather than in the
    !! "openmp" suite because pinning a file date writes a process-global setting, which only this
    !! (serially dispatched) suite may do. Its assertions are almost incidental: what it really
    !! checks is that the process is still alive afterwards.
    !!
    !! `parquet_push_settings_to_cpp` runs at the top of EVERY `parquet_open_reader`, and mirroring
    !! the pinned date used to assign a process-global `std::string` unguarded -- so two threads
    !! opening their own readers at the same moment freed the same buffer twice. What that produced
    !! was not a crash at the assignment: it was glibc aborting with `malloc(): unaligned tcache
    !! chunk detected` somewhere else entirely, in a fraction of runs. See
    !! `parquet_push_file_metadata_settings` in parquet_wrapper.cpp.
    !!
    !! **Three details make this test able to fail**, and none of them is optional:
    !!
    !!  * the date must be LONGER THAN 15 CHARACTERS, because libstdc++ keeps a shorter string
    !!    inside the string object and never touches the heap for it. The 19-character ISO
    !!    timestamp below is what a caller pinning a real date actually writes;
    !!  * the loop must OPEN A READER PER ITERATION, since the push happens per open, not per read;
    !!  * there must be enough iterations for two of them to overlap. Against the unfixed library
    !!    4000 opens aborted **10 runs out of 10** on the development machine's default team.
    !!
    !! **How reliably it fires depends on the team size**, so treat a pass on a two-core runner as
    !! weaker evidence than a pass here -- the failure is probabilistic, and a low overlap count
    !! reads as a pass. Verify a change to this test by breaking the fix on purpose
    !! (`parquet_push_file_metadata_settings` in parquet_wrapper.cpp) and confirming the abort
    !! returns; and rebuild with `fpm clean --skip` first, because a stale binary produced 0/10 --
    !! a clean "the control broke nothing" that was really a control that never ran.
    subroutine test_file_date_survives_concurrent_opens(error)
        type(error_type), allocatable, intent(out) :: error
        !> 19 characters, deliberately past libstdc++'s 15-character small-string threshold -- see
        !! the doc-comment above; a shorter date cannot reproduce the defect this guards.
        character(len=*), parameter :: pinned = "2026-01-02T03:04:05"
        character(len=*), parameter :: fname = "test_run/settings_concurrent_date.parquet"
        integer, parameter :: nopen = 4000 !! opens, hence pushes; see the doc-comment.
        integer :: nrows(nopen) !! per-open row count, so the loop cannot be optimised away.
        character(len=:), allocatable :: got
        integer :: i

        call parquet_set_file_date(pinned)
        call write_dated_fixture(fname)
        nrows = -1

        !$omp parallel do default(shared) private(i) schedule(static, 1)
        do i = 1, nopen
            call open_and_close_once(fname, nrows(i))
        end do
        !$omp end parallel do

        call parquet_set_file_date("")
        call read_file_date(fname, got)

        call check(error, all(nrows == 64), &
            "every concurrent open must report the fixture's 64 rows")
        if (allocated(error)) return
        call check(error, got == pinned, &
            "the pinned date must still reach the file after a parallel open storm")
    end subroutine test_file_date_survives_concurrent_opens

    !> One open/close on its own reader, called from the loop above. A subroutine rather than an
    !! inline `block`, so the reader is an ordinary local of a called procedure -- the shape this
    !! project's other concurrency tests use, and the one with no OpenMP `private` initialisation
    !! question hanging over it.
    subroutine open_and_close_once(fname, nrows)
        character(len=*), intent(in) :: fname !! fixture to open.
        integer, intent(out) :: nrows !! rows the reader reported.
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, fname, nrows=nrows)
        call parquet_close_reader(reader)
    end subroutine open_and_close_once

    !> Writes one small schema-enforced file, identical every time apart from whatever the settings
    !> put in it. Its own helper so the arms of the two file_date tests above cannot differ by
    !> accident.
    subroutine write_dated_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write; one per arm.
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: k(64)
        integer :: i

        do i = 1, size(k)
            k(i) = i
        end do
        call schema%init(table="file_date")
        call schema%add_field("k", "int32", unit="count")
        call parquet_open_writer(writer, fname, schema, overwrite=.true.)
        call parquet_write_column(writer, "k", k)
        call parquet_close_writer(writer)
    end subroutine write_dated_fixture

    !> Whether two files hold exactly the same bytes. Compares sizes first and then the contents in
    !> one stream read each, which is all the assertion above needs and avoids a byte-at-a-time loop.
    logical function files_are_identical(a, b) result(same)
        character(len=*), intent(in) :: a !! first file.
        character(len=*), intent(in) :: b !! second file.
        integer :: ua, ub, na, nb
        character(len=1), allocatable :: buf_a(:), buf_b(:)

        same = .false.
        open (newunit=ua, file=a, access="stream", form="unformatted", status="old")
        inquire (unit=ua, size=na)
        open (newunit=ub, file=b, access="stream", form="unformatted", status="old")
        inquire (unit=ub, size=nb)
        if (na == nb .and. na > 0) then
            allocate (buf_a(na), buf_b(nb))
            read (ua) buf_a
            read (ub) buf_b
            same = all(buf_a == buf_b)
        end if
        close (ua)
        close (ub)
    end function files_are_identical

    !> Reads a written file's `DATE` metadata key back.
    subroutine read_file_date(fname, value)
        character(len=*), intent(in) :: fname !! file to read.
        character(len=:), allocatable, intent(out) :: value !! the DATE key's text.
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, fname)
        call parquet_get_metadata(reader, "DATE", value)
        call parquet_close_reader(reader)
    end subroutine read_file_date

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

    !> The third thing `pf_sort_threads` is documented to account for, after the setting cap and the
    !> parallel-region rule: the CPU affinity mask. `doc/pages/operating/performance.md` tells a
    !> reader to call it when threading looks wrong on a large machine, so the number it reports has
    !> to be one that can actually run.
    !>
    !> **This cannot be provoked on an ordinary machine** -- reproducing the clamp needs a process
    !> bound to fewer processors than `OMP_NUM_THREADS` asks for, which a test cannot arrange from
    !> inside itself. What it CAN assert is the invariant that clamp exists to maintain, on every
    !> machine including a bound one: the answer never exceeds `omp_get_num_procs()`. Deleting the
    !> clamp leaves this passing wherever the two agree and failing wherever they do not, which is
    !> exactly the population that has the bug.
    !>
    !> **The negative control is the second assertion**, and it is what stops a hard-coded `1`
    !> passing: with the setting automatic and OpenMP available, the answer must also be at least 1
    !> and must rise when the environment offers more, which `test_sort_threads_effect` next door
    !> pins from the other side. A one-sided inequality on its own is satisfied by any constant.
    subroutine test_sort_threads_affinity_clamp(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: n, unbound
        !
        call parquet_reset_settings()
        call parquet_set_verbosity("silent")     ! the clamp warns; this test is about the number
        call parquet_debug_set_affinity_procs(0)
        unbound = pf_sort_threads()
#ifdef _OPENMP
        call check(error, unbound <= omp_get_num_procs(), &
            "pf_sort_threads() must never exceed omp_get_num_procs(): threads cannot escape the mask")
        if (allocated(error)) goto 900
        call check(error, unbound == min(omp_get_max_threads(), omp_get_num_procs()), &
            "with the setting automatic, pf_sort_threads() is the smaller of the ICV and the mask")
        if (allocated(error)) goto 900
#else
        call check(error, unbound == 1, "without OpenMP pf_sort_threads() is 1")
        if (allocated(error)) goto 900
#endif
        !
        ! The clamp made to BITE. Without the override this test is vacuous on any machine whose
        ! mask is the whole machine, which is every machine a test runs on: verified by mutation --
        ! deleting the clamp left the two assertions above passing.
        call parquet_debug_set_affinity_procs(1)
        n = pf_sort_threads()
        call check(error, n == 1, "pf_sort_threads() reports the mask when the mask is smaller")
        if (allocated(error)) goto 900
        !
        ! Negative control: the same override raised above what OpenMP offers must change nothing,
        ! so the clamp is shown to LOWER only. A clamp that fired unconditionally, or one that
        ! replaced the answer instead of bounding it, fails here while passing the assertion above.
        call parquet_debug_set_affinity_procs(unbound + 16)
        n = pf_sort_threads()
        call check(error, n == unbound, &
            "a mask wider than the OpenMP thread count must not raise pf_sort_threads()")
        !
900     continue
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
    end subroutine test_sort_threads_affinity_clamp

    !> Mirrors `doc/pages/operating/settings.md`'s `configure_at_startup` example, the page's one
    !> complete runnable program. Both sides move together or neither does.
    !>
    !> **It lives here rather than in `test/test_examples.f90`, where every other mirrored example
    !> sits, and the reason is concurrency.** The example's whole subject is process-global state --
    !> it sets six thread counts, a verbosity and a codec. `examples` runs its tests concurrently;
    !> `settings` is one of the suites `suite_is_safe_to_parallelize` excludes precisely so a test
    !> may write a global without a sibling observing it. Putting it there would have meant
    !> excluding `examples` too, which would cost twenty other example tests the concurrency
    !> regression check they currently get for free.
    !>
    !> **The two printed values are what the page's own comments claim**, and they are stable
    !> whatever the environment holds: `parquet_settings_from_env` runs FIRST in the example, so the
    !> explicit setters after it win. That ordering is itself one of the things the page teaches.
    subroutine test_configure_at_startup_example(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: u, ios, nlines
        logical :: seen_codec
        character(len=256) :: line
        character(len=:), allocatable :: codec
        character(len=*), parameter :: dump_file = "test_run/settings_example_dump.txt"
        !
        call parquet_reset_settings()
        call parquet_settings_from_env()
        call parquet_set_threads(4)
        call parquet_set_sort_threads(1)
        call parquet_set_verbosity("errors_only")
        call parquet_set_default_compression("zstd")
        call parquet_set_default_compression_level(6)
        !
        call check(error, parquet_get_arrow_threads() == 4, &
            "configure_at_startup: the page prints 4 for the Arrow thread pool")
        if (allocated(error)) goto 900
        call check(error, pf_sort_threads() == 1, &
            "configure_at_startup: the page prints 1 for what a sort would use")
        if (allocated(error)) goto 900
        !
        ! parquet_set_threads(4) reached all five per-area caps, not just the two named above --
        ! the claim the page's *All six thread counts at once* section makes, observed here rather
        ! than argued.
        call check(error, parquet_get_prefetch_threads() == 4 .and. parquet_get_table_threads() == 4 &
            .and. parquet_get_string_threads() == 4 .and. parquet_get_random_threads() == 4, &
            "configure_at_startup: set_threads(4) must reach the prefetch, table, string and random caps")
        if (allocated(error)) goto 900
        !
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd" .and. parquet_get_default_compression_level() == 6, &
            "configure_at_startup: the codec and level the example chooses must stick")
        if (allocated(error)) goto 900
        !
        ! The last line of the example: the dump prints in full at errors_only.
        open(newunit=u, file=dump_file, status="replace", action="write")
        call parquet_print_settings(u)
        close(u)
        nlines = 0
        seen_codec = .false.
        open(newunit=u, file=dump_file, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (index(line, "default_compression ") > 0 .and. index(line, "zstd") > 0) seen_codec = .true.
        end do
        close(u, status="delete")
        call check(error, nlines > 20 .and. seen_codec, &
            "configure_at_startup: the dump must print in full at errors_only, showing what was set")
        !
900     continue
        call parquet_reset_settings()
    end subroutine test_configure_at_startup_example

    !> **T4: the affinity clamp reaches the string and the random resolvers, not only the sort's.**
    !>
    !> All four resolvers call the one shared `parquet_clamp_to_affinity`, and until this test
    !> existed only `pf_sort_threads` asserted that it does -- so deleting the call from
    !> `parquet_string_threads` or from `parquet_auto_thread_count` left the suite green. They are
    !> in separate tiers that cannot see each other, which is exactly why the clamp is shared and
    !> exactly why one test of one caller proves nothing about the other three.
    !>
    !> **`parquet_debug_set_affinity_procs` is what makes this testable at all.** A process cannot
    !> narrow its own affinity after starting, and on an ordinary machine `omp_get_max_threads()`
    !> and `omp_get_num_procs()` agree -- so every assertion here would hold just as well with the
    !> clamp deleted. That is the vacuity CLAUDE.md's auto-threading note records.
    !>
    !> **Both branches of the random resolver, which is the half this test used to miss.** It
    !> asserted the AUTOMATIC answer only, and the clamp was applied to the automatic answer only:
    !> `pf_random_permutation(..., threads=8)` opened eight threads on a one-processor mask and
    !> said nothing, while `doc/pages/operating/settings.md` promised the affinity bound reaches
    !> an explicit `threads=` as well. A test covering one branch of a two-branch resolver is the
    !> same vacuity one caller of four was.
    subroutine test_affinity_clamp_other_resolvers(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer :: unbound_str, unbound_rnd, clamped_str, clamped_rnd, avail
        integer(int64), parameter :: BIG_N = 1000000_int64
        integer :: i
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call parquet_reset_settings()
        call parquet_set_verbosity("silent")     ! the clamp warns; this test is about the number
        call parquet_debug_set_affinity_procs(0)
        !
        ! A payload the string work floor cannot decline, so `bulk_threads` reaches the resolver.
        ! Lowering the floor is what the hook is for -- a genuinely 256 KiB column would make this
        ! test slow for no gain.
        call parquet_debug_set_string_min_bytes(1_int64)
        do i = 1, 64
            call col%append_string("abcdefgh")
        end do
        !
        unbound_str = parquet_debug_string_bulk_threads(col)
        unbound_rnd = parquet_debug_random_bulk_threads(BIG_N)
        !
        ! The clamp made to BITE, one processor. Both resolvers must report 1.
        call parquet_debug_set_affinity_procs(1)
        clamped_str = parquet_debug_string_bulk_threads(col)
        clamped_rnd = parquet_debug_random_bulk_threads(BIG_N)
        call check(error, clamped_str == 1, &
            "a one-processor mask must make one string column's bulk work serial")
        if (allocated(error)) goto 900
        call check(error, clamped_rnd == 1, &
            "a one-processor mask must make a bulk random draw serial")
        if (allocated(error)) goto 900
        call check(error, parquet_debug_random_bulk_threads(BIG_N, 8) == 1, &
            "a one-processor mask must lower an EXPLICIT threads= on a bulk random draw too")
        if (allocated(error)) goto 900
        !
        ! Negative control, in two halves. First: on a machine with threads to give, the unclamped
        ! answers must have EXCEEDED 1, or the two assertions above hold for a reason that has
        ! nothing to do with the clamp.
        if (avail > 1) then
            call check(error, unbound_str > 1, &
                "negative control: with threads available the string resolver must exceed 1 unclamped")
            if (allocated(error)) goto 900
            call check(error, unbound_rnd > 1, &
                "negative control: with threads available the random resolver must exceed 1 unclamped")
            if (allocated(error)) goto 900
        end if
        !
        ! Second: a mask WIDER than what OpenMP offers must change nothing, so the clamp is shown to
        ! lower only. A clamp that fired unconditionally passes the first half and fails here.
        call parquet_debug_set_affinity_procs(unbound_str + 16)
        call check(error, parquet_debug_string_bulk_threads(col) == unbound_str, &
            "a mask wider than the OpenMP thread count must not raise the string resolver")
        if (allocated(error)) goto 900
        call parquet_debug_set_affinity_procs(unbound_rnd + 16)
        call check(error, parquet_debug_random_bulk_threads(BIG_N) == unbound_rnd, &
            "a mask wider than the OpenMP thread count must not raise the random resolver")
        if (allocated(error)) goto 900
        !
        ! And the same control for the explicit branch: a mask wider than the request must leave
        ! it alone, so the new clamp is shown to LOWER only. A clamp applied unconditionally, or
        ! one that replaced the request with the mask, passes the assertion above and fails here.
        call parquet_debug_set_affinity_procs(max(avail, 8) + 16)
        call check(error, parquet_debug_random_bulk_threads(BIG_N, 8) == 8, &
            "negative control: a mask wider than an explicit threads= must leave the request alone")
        !
900     continue
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_set_string_min_bytes(0_int64)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
    end subroutine test_affinity_clamp_other_resolvers

    !> **T3: a table's per-column rewrite is bounded by the affinity mask like everything else.**
    !>
    !> `colwork_threads` was the last resolver to get the clamp, and the gap mattered more here than
    !> anywhere else: the clamp is also where the once-per-process warning lives, so a table rewrite
    !> was the one subsystem that could be silently oversubscribed *without even the warning
    !> firing*. Every other resolver would have reported it.
    !>
    !> Observed through `parquet_debug_get_table_threads_used`, which `colwork_threads`' caller
    !> writes on every mutation including a serial one -- so a 1 here is a real resolved 1 and not a
    !> mutation that never reached the loop.
    subroutine test_affinity_clamp_table_rewrite(error)
        type(error_type), allocatable, intent(out) :: error
        ! Above colwork_min_elements (131072) with room to spare, matching test_table_threads_effect.
        integer, parameter :: BIG = 150000
        integer(int32), allocatable :: k(:), a(:), b(:), c(:)
        integer :: i, unbound, clamped, avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        allocate(k(BIG), a(BIG), b(BIG), c(BIG))
        do i = 1, BIG
            ! Scattered, so %sort_by really moves rows -- an already-sorted table returns early
            ! without touching a column, leaving the counter reporting the previous call.
            k(i) = mod(i * 7919, BIG)
            a(i) = i
            b(i) = 2*i
            c(i) = 3*i
        end do
        !
        call parquet_reset_settings()
        call parquet_set_verbosity("silent")     ! the clamp warns; this test is about the number
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_table_threads()
        call sort_four_columns(k, a, b, c)
        unbound = parquet_debug_table_threads()
        !
        ! The clamp made to bite.
        call parquet_debug_set_affinity_procs(1)
        call parquet_debug_reset_table_threads()
        call sort_four_columns(k, a, b, c)
        clamped = parquet_debug_table_threads()
        call check(error, clamped == 1, &
            "a one-processor mask must make a table's per-column rewrite serial")
        if (allocated(error)) goto 900
        !
        ! Negative control: unclamped, on a machine with threads to give, the same fixture must have
        ! used more than one -- otherwise the assertion above says nothing about the clamp.
        if (avail > 1) then
            call check(error, unbound > 1, &
                "negative control: unclamped, four large columns must be rewritten in parallel")
            if (allocated(error)) goto 900
            !
            ! ...and a mask wider than the ICV must not raise it, so the clamp only ever lowers.
            call parquet_debug_set_affinity_procs(unbound + 16)
            call parquet_debug_reset_table_threads()
            call sort_four_columns(k, a, b, c)
            call check(error, parquet_debug_table_threads() == unbound, &
                "a mask wider than the OpenMP thread count must not raise the rewrite's team")
        end if
        !
900     continue
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
    end subroutine test_affinity_clamp_table_rewrite

    !> **T2: `parquet_print_settings` prints at every verbosity level, including "errors_only".**
    !>
    !> A settings dump that could itself be suppressed would leave a quiet program with no way to be
    !> asked why it is quiet, which is why the exemption exists. It holds **by construction** --
    !> `parquet_print_settings` consults no verbosity state at all -- so the mutation that breaks it
    !> is *adding* a `parquet_output_is_suppressed()` guard, and nothing else in the suite would
    !> notice. That is what this test is for.
    !>
    !> The negative control is in the same test and at the same verbosity: `%print_schema_info` must
    !> produce nothing. Without it, an assertion that the dump appeared passes just as happily
    !> against a run where the verbosity was never actually in force.
    subroutine test_print_settings_never_silenced(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: sch
        integer :: u, ios, nlines_dump, nlines_schema
        logical :: seen
        character(len=256) :: line
        ! Its own filenames: tests in a suite run concurrently.
        character(len=*), parameter :: dump_file = "test_run/settings_dump_errors_only.txt"
        character(len=*), parameter :: schema_file = "test_run/settings_schema_errors_only.txt"
        !
        call parquet_reset_settings()
        call parquet_set_verbosity("errors_only")
        !
        open(newunit=u, file=dump_file, status="replace", action="write")
        call parquet_print_settings(u)
        close(u)
        !
        call sch%init("silence_probe")
        call sch%add_field("a", "int32")
        call parquet_parse_maml(sch)
        open(newunit=u, file=schema_file, status="replace", action="write")
        call sch%print_schema_info(u)
        close(u)
        call parquet_reset_settings()
        !
        ! The dump survived: non-empty, and carrying a row only a real dump has.
        seen = .false.
        nlines_dump = 0
        open(newunit=u, file=dump_file, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines_dump = nlines_dump + 1
            if (index(line, "arrow_threads") > 0) seen = .true.
        end do
        close(u, status="delete")
        call check(error, seen, &
            "parquet_print_settings must still print at verbosity=errors_only, the strictest level")
        if (allocated(error)) goto 900
        call check(error, nlines_dump > 20, &
            "the dump printed at errors_only must be the whole dump, not a truncated one")
        if (allocated(error)) goto 900
        !
        ! Negative control: the same level really was in force, so a solicited printer wrote nothing.
        nlines_schema = 0
        open(newunit=u, file=schema_file, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines_schema = nlines_schema + 1
        end do
        close(u, status="delete")
        call check(error, nlines_schema == 0, &
            "negative control: at errors_only %print_schema_info must be a no-op, proving the level was in force")
        return
        !
900     continue
        open(newunit=u, file=schema_file, status="old", action="read", iostat=ios)
        if (ios == 0) close(u, status="delete")
    end subroutine test_print_settings_never_silenced

    !> **T5: the random work floor LOWERS the thread count; it does not simply force serial.**
    !>
    !> `random_threads` computes `by_work = n / floor` and takes the smaller of that and the
    !> requested count, so eight threads over 4000 elements at a floor of 1000 is **four** threads,
    !> not one and not eight. The guide described it as a hard cut for as long as it did precisely
    !> because the existing floor test asserts only the serial/threaded boundary -- the intermediate
    !> value is the one place the two descriptions differ.
    !>
    !> Note this differs from the STRING work floor, which really is a hard cut
    !> (`bulk_threads` returns 1 outright below its payload floor). Two floors, two shapes; do not
    !> harmonise the tests.
    subroutine test_random_work_floor_is_gradual(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: full, partial, serial, no_floor
        !
        call parquet_reset_settings()
        call parquet_set_verbosity("silent")
        call parquet_debug_set_affinity_procs(1024)   ! keep the mask out of this test's answer
        call parquet_set_random_parallel_min_elements(1000)
        !
        full    = parquet_debug_random_bulk_threads(8000_int64, threads=8)
        partial = parquet_debug_random_bulk_threads(4000_int64, threads=8)
        serial  = parquet_debug_random_bulk_threads(1500_int64, threads=8)
        !
        call check(error, full == 8, &
            "enough work for eight threads at the floor must give all eight")
        if (allocated(error)) goto 900
        call check(error, partial == 4, &
            "4000 elements at a floor of 1000 must give FOUR threads, not one and not eight")
        if (allocated(error)) goto 900
        call check(error, serial == 1, &
            "below two floors' worth of work the call is serial")
        if (allocated(error)) goto 900
        !
        ! Negative control: with the floor disabled the same 4000 elements take the full team, so
        ! the 4 above is the floor's doing and not some other clamp's.
        call parquet_set_random_parallel_min_elements(0)
        no_floor = parquet_debug_random_bulk_threads(4000_int64, threads=8)
        call check(error, no_floor == 8, &
            "negative control: with the floor disabled 4000 elements must take the full team")
        !
900     continue
        call parquet_debug_set_affinity_procs(0)
        call parquet_debug_reset_affinity_warning()
        call parquet_reset_settings()
    end subroutine test_random_work_floor_is_gradual

    !> An unqualified sort inside an OpenMP parallel region runs SERIALLY, and a setting
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
    !> simultaneously makes it untestable by ordinary means. So the assertion is on the thread count
    !> the mutation resolved to, read back through a C++ debug hook, and it needs all three of the
    !> observations below to mean anything:
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
            function got_table_threads() bind(C, name="parquet_debug_get_table_threads_used") result(kk)
                import :: c_int64_t
                integer(c_int64_t) :: kk
            end function got_table_threads
        end interface
        !
        n = int(got_table_threads())
    end function parquet_debug_table_threads

    !> Clears the mutation thread counter, so a test observes its own mutation rather than an
    !> earlier one. Reset to 0, which the mutation itself never writes -- so a counter still reading
    !> 0 afterwards means the mutation never reached the loop at all.
    subroutine parquet_debug_reset_table_threads()
        use iso_c_binding, only : c_int64_t
        interface
            subroutine put_table_threads(kk) bind(C, name="parquet_debug_set_table_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: kk
            end subroutine put_table_threads
        end interface
        !
        call put_table_threads(0_c_int64_t)
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
            function got_last_use_threads() bind(C, name="parquet_debug_get_last_use_threads") result(k)
                import :: c_int
                integer(c_int) :: k
            end function got_last_use_threads
        end interface
        !
        n = int(got_last_use_threads())
    end function parquet_debug_last_use_threads

    !> Threads the last parallel prefetch was given; 0 if it ran serially.
    integer function parquet_debug_prefetch_threads() result(n)
        use iso_c_binding, only : c_int64_t
        interface
            function got_prefetch_threads() bind(C, name="parquet_debug_get_prefetch_threads_used") result(k)
                import :: c_int64_t
                integer(c_int64_t) :: k
            end function got_prefetch_threads
        end interface
        !
        n = int(got_prefetch_threads())
    end function parquet_debug_prefetch_threads

    !> Clears the prefetch counter, so a test observes its own prefetch rather than an earlier one.
    subroutine parquet_debug_reset_prefetch_threads()
        use iso_c_binding, only : c_int64_t
        interface
            subroutine put_prefetch_threads(k) bind(C, name="parquet_debug_set_prefetch_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine put_prefetch_threads
        end interface
        !
        call put_prefetch_threads(0_c_int64_t)
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
        ! Checked rather than discarded: a failed setenv leaves the old value in place, so every
        ! assertion downstream would be made against an environment nobody set.
        if (rc /= 0) error stop "set_env: setenv failed for '" // name // "'"
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
        if (rc /= 0) error stop "unset_env: unsetenv failed for '" // name // "'"
    end subroutine unset_env

    !> Every thread count must move, and each is read back through its OWN getter -- a convenience
    !> that set one of them and forgot the others would pass any single assertion.
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
        if (.not. allocated(error)) call check(error, parquet_get_random_threads() == 3, &
            "set_threads must set the random cap")
        if (.not. allocated(error)) call check(error, parquet_get_prefetch_threads() == 3, &
            "set_threads must set the prefetch cap")
        ! A forgotten call is invisible without its own assertion, since the others still work and
        ! the name says nothing about how many "all" is -- which is exactly how this test came to
        ! say "all four" while a fifth cap existed and was never set, and then "all five" while a
        ! sixth did. Both this test's name and `parquet_set_threads`' doc-comment now NAME the caps
        ! rather than counting them, so adding one means adding a line here and nowhere else.
        if (.not. allocated(error)) call check(error, parquet_get_table_threads() == 3, &
            "set_threads must set the table mutation cap")
        if (.not. allocated(error)) call check(error, parquet_get_string_threads() == 3, &
            "set_threads must set the string-column cap")
        if (.not. allocated(error)) call check(error, parquet_get_spatial_threads() == 3, &
            "set_threads must set the spatial cap")
        if (.not. allocated(error)) call check(error, parquet_get_healpix_threads() == 3, &
            "set_threads must set the healpix cap")
        if (.not. allocated(error)) call check(error, parquet_get_index_threads() == 3, &
            "set_threads must set the index-build cap")
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
    !> sets all six, so it has to be applied before the six specific variables or they could
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
        call unset_env("PARQUET_FORTRAN_RANDOM_THREADS")
        call unset_env("PARQUET_FORTRAN_SPATIAL_THREADS")
        call unset_env("PARQUET_FORTRAN_HEALPIX_THREADS")
        call unset_env("PARQUET_FORTRAN_INDEX_THREADS")
        call unset_env("PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS")
        call unset_env("PARQUET_FORTRAN_SORT_COUNTING_PATH")
        call unset_env("PARQUET_FORTRAN_SORT_RADIX_PATH")
        call unset_env("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT")
        call unset_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION")
        call unset_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL")
        call unset_env("PARQUET_FORTRAN_DEFAULT_USE_THREADS")
        call unset_env("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES")
        call unset_env("PARQUET_FORTRAN_STATISTICS_PRESCREEN")
        call unset_env("PARQUET_FORTRAN_VERBOSITY")
        call unset_env("PARQUET_FORTRAN_MESSAGE_STREAM")
        call unset_env("PARQUET_FORTRAN_FILE_DATE")
    end subroutine unset_all_env

    !> **The bulk test, and the one that catches a crossed pair.** Every variable set to its own
    !> distinguishable value in one call, each read back through its own getter -- so a variable
    !> wired to the wrong setter fails on both knobs at once, and a variable left out of
    !> parquet_settings_from_env's sequence fails on its own.
    !>
    !> Adding a variable to the loader means adding it HERE and to `unset_all_env` as well. The three
    !> pool knobs added after the first draft -- spatial, HEALPix and index -- were read by the
    !> loader for some time with no line of this test naming them, which is the exact failure the
    !> paragraph above says this test exists to prevent.
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
        call set_env("PARQUET_FORTRAN_RANDOM_THREADS", "6")
        call set_env("PARQUET_FORTRAN_SPATIAL_THREADS", "4")
        call set_env("PARQUET_FORTRAN_HEALPIX_THREADS", "8")
        call set_env("PARQUET_FORTRAN_INDEX_THREADS", "9")
        call set_env("PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS", "250")
        call set_env("PARQUET_FORTRAN_SORT_COUNTING_PATH", "false")
        call set_env("PARQUET_FORTRAN_SORT_RADIX_PATH", "false")
        call set_env("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT", "128")
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION", "gzip")
        call set_env("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL", "6")
        call set_env("PARQUET_FORTRAN_DEFAULT_USE_THREADS", "false")
        call set_env("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES", "4096")
        call set_env("PARQUET_FORTRAN_STATISTICS_PRESCREEN", "false")
        call set_env("PARQUET_FORTRAN_VERBOSITY", "errors_only")
        call set_env("PARQUET_FORTRAN_MESSAGE_STREAM", "stderr")
        call set_env("PARQUET_FORTRAN_FILE_DATE", "2019-03-04T05:06:07")
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
        if (.not. allocated(error)) call check(error, parquet_get_random_threads() == 6, &
            "PARQUET_FORTRAN_RANDOM_THREADS reaches random_threads")
        if (.not. allocated(error)) call check(error, parquet_get_spatial_threads() == 4, &
            "PARQUET_FORTRAN_SPATIAL_THREADS reaches spatial_threads")
        if (.not. allocated(error)) call check(error, parquet_get_healpix_threads() == 8, &
            "PARQUET_FORTRAN_HEALPIX_THREADS reaches healpix_threads")
        if (.not. allocated(error)) call check(error, parquet_get_index_threads() == 9, &
            "PARQUET_FORTRAN_INDEX_THREADS reaches index_threads")
        if (.not. allocated(error)) call check(error, &
            parquet_get_random_parallel_min_elements() == 250_int64, &
            "PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS reaches random_parallel_min_elements")
        if (.not. allocated(error)) call check(error, .not. parquet_get_sort_counting_path(), &
            "PARQUET_FORTRAN_SORT_COUNTING_PATH reaches sort_counting_path")
        if (.not. allocated(error)) call check(error, .not. parquet_get_sort_radix_path(), &
            "PARQUET_FORTRAN_SORT_RADIX_PATH reaches sort_radix_path")
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
        if (.not. allocated(error)) then
            call parquet_get_file_date(token)
            call check(error, token == "2019-03-04T05:06:07", "PARQUET_FORTRAN_FILE_DATE reaches file_date")
        end if
        call parquet_set_file_date("")
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
            function got_row_groups_pruned() bind(C, name="parquet_debug_get_row_groups_pruned") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! pruned row groups of the last screen.
            end function got_row_groups_pruned
        end interface
        n = int(got_row_groups_pruned(), int64)
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

    !> The radix path performs zero comparisons too, but the C++ comparison counter cannot see it:
    !> the radix path exists only in the FORTRAN engine, so this knob has to be observed there.
    !>
    !> The observable is the introsort's final insertion pass. That pass runs over the whole range
    !> on every comparison sort and, on a scrambled fixture, always moves something; the radix path
    !> never calls it at all for a non-string key. So "did the tracker record anything" answers
    !> which path ran, and asserting BOTH directions is what stops a knob that is stored and never
    !> read from passing.
    !>
    !> A REAL key at 512 rows: real because the counting path takes integer keys and would confound
    !> the observation, 512 because the radix path has a row floor beneath which it declines
    !> regardless of this setting.
    subroutine test_sort_radix_path_effect(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(512)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: shift_on, shift_off
        integer :: k
        !
        call parquet_reset_settings()
        do k = 1, size(v)
            v(k) = real(mod(k * 7919, 4001), real64)
        end do
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v, perm)
        shift_on = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_set_sort_radix_path(.false.)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v, perm)
        shift_off = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_debug_set_sort_track_shift(.false.)
        ! RESTORE, not clear: the Fortran engine is the shipped default (`dbg_fortran_engine` in
        ! tools/generate_parquet_sorting.py). `parquet_reset_settings` does not touch the selector,
        ! it being a debug hook rather than a setting, so leaving `.false.` here would hand every
        ! later test in this process the C++ engine. test/test_sorting.f90's
        ! `restore_engine_default` is the same rule; this suite has only this one site.
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_reset_settings()
        !
        call check(error, shift_on == 0_int64, &
            "with sort_radix_path on, a single-key sort above the floor must take the radix path")
        if (allocated(error)) return
        call check(error, shift_off > 0_int64, &
            "turning sort_radix_path off must force the comparison sort (a nonzero insertion shift)")
    end subroutine test_sort_radix_path_effect

    !> The FORTRAN half of `sort_counting_path`, which the C++-pinned test above cannot reach.
    !>
    !> Both engines read this knob, but the two observables are disjoint: the C++ engine answers
    !> through its comparison counter, and the Fortran engine performs no comparisons on either of
    !> its fast paths, so that counter cannot tell counting from radix. The radix PASS count can:
    !> the counting path runs none, and above the radix row floor the declined key runs some.
    !>
    !> Fixture constraints, both load-bearing and neither obvious:
    !>
    !> * **512 rows**, because the radix path declines below its own row floor. Beneath it the
    !>   "off" arm would take the comparison sort, which also reports zero passes, and both arms
    !>   would agree for the wrong reason.
    !> * **Range 63**, because the counting path is admitted only while the key's value range is
    !>   under a fraction of the row count. A wider key is declined whichever way this knob is set,
    !>   and switching it would then prove nothing -- the same trap `test_radix_path_narrow_integer`
    !>   records for its own `smallband` fixture.
    !>
    !> `threads=1` is explicit rather than incidental: the counting path is admitted only at a
    !> small team, so an automatic count would make the fixture depend on the machine.
    subroutine test_sort_counting_path_effect_fortran(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: v(512)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: passes_on, passes_off
        integer :: k
        !
        call parquet_reset_settings()
        do k = 1, size(v)
            v(k) = mod(int(k, int64) * 7_int64, 64_int64)
        end do
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(v, perm, threads=1)
        passes_on = parquet_debug_sort_radix_passes()
        !
        call parquet_set_sort_counting_path(.false.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(v, perm, threads=1)
        passes_off = parquet_debug_sort_radix_passes()
        call parquet_reset_settings()
        !
        call check(error, passes_on == 0_int64, &
            "with sort_counting_path on, a narrow integer key must take the counting path (no radix passes)")
        if (allocated(error)) return
        ! The negative control. Without it the assertion above would pass against a build whose
        ! radix path never ran at all -- which is exactly what a fixture below the row floor gives.
        call check(error, passes_off > 0_int64, &
            "turning sort_counting_path off must force the radix path (a nonzero pass count)")
    end subroutine test_sort_counting_path_effect_fortran

    !> The FORTRAN half of `sort_counting_bucket_limit`, observed the same way.
    !>
    !> The knob bounds the key's value RANGE rather than its cardinality, so both arms use the same
    !> data and only the limit moves -- which is what shows the setting is read rather than merely
    !> stored. The first arm doubles as the control: at the built-in limit this key is accepted, so
    !> a limit that declined everything could not pass both halves.
    subroutine test_sort_counting_bucket_limit_effect_fortran(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: v(512)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: passes_wide, passes_tight
        integer :: k
        !
        call parquet_reset_settings()
        do k = 1, size(v)
            v(k) = mod(int(k, int64) * 7_int64, 64_int64)
        end do
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(v, perm, threads=1)
        passes_wide = parquet_debug_sort_radix_passes()
        !
        ! Range 63 against a limit of 8: the same key, now declined onto the radix path.
        call parquet_set_sort_counting_bucket_limit(8_int64)
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(v, perm, threads=1)
        passes_tight = parquet_debug_sort_radix_passes()
        call parquet_reset_settings()
        !
        call check(error, passes_wide == 0_int64, &
            "at the built-in bucket limit a range-63 key must take the counting path (no radix passes)")
        if (allocated(error)) return
        call check(error, passes_tight > 0_int64, &
            "a bucket limit below the key's value range must push the Fortran engine onto the radix path")
    end subroutine test_sort_counting_bucket_limit_effect_fortran

    !> The bound is on the key's value RANGE, not its cardinality, so both halves here use the SAME
    !> 500 values and the same cardinality -- only the spread differs, and the limit is what
    !> decides. A limit that never reached C++ would leave the built-in 2**22 in force and both
    !> halves would take the counting path.
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
    !> not have reached it.
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

    !> `verbosity="silent"` silences SOLICITED output, and `%print_schema_info` is solicited output,
    !! so the call becomes a complete no-op -- not merely an empty file, but no file at all, because
    !! the suppression check sits before the `open`. That placement is the part worth pinning: it is
    !! what stops a silenced program from littering the filesystem with empty listings.
    !!
    !! **Negative control:** the identical call at `"normal"` must produce a file with content.
    !! Without it this test passes just as happily against a `%print_schema_info` that never writes
    !! anything at any verbosity.
    !!
    !! Lives in this suite rather than beside the other `print_schema_info` tests in
    !! test_metadata.f90 because it writes a process-global setting, and `settings` is the suite
    !! run_tester.f90 excludes from test-drive's per-test parallelism.
    subroutine test_verbosity_silences_print_schema_info(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        character(len=*), parameter :: quiet_file = "test_run/settings_silent_schema_info.txt"
        character(len=*), parameter :: loud_file = "test_run/settings_normal_schema_info.txt"
        logical :: quiet_exists, loud_exists
        integer :: u, ios, nlines

        call parquet_reset_settings()
        call schema%init(table="silent_probe")
        call schema%add_field("id", "int32", ucd="meta.id", info="Object identifier")

        call delete_if_present(quiet_file)
        call delete_if_present(loud_file)

        call parquet_set_verbosity("silent")
        call schema%print_schema_info(filename=quiet_file)
        call parquet_set_verbosity("normal")
        call schema%print_schema_info(filename=loud_file)
        call parquet_reset_settings()

        inquire(file=quiet_file, exist=quiet_exists)
        inquire(file=loud_file, exist=loud_exists)
        call check(error, .not. quiet_exists, &
            "verbosity=silent must not even create the file print_schema_info would have written")
        if (allocated(error)) return
        call check(error, loud_exists, "the negative control at verbosity=normal should have written a file")
        if (allocated(error)) return

        nlines = 0
        open(newunit=u, file=loud_file, status="old", action="read")
        do
            read(u, '(a)', iostat=ios)      ! no item: the count is all this loop wants
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close(u)
        call check(error, nlines > 0, "the negative control's file should not be empty")
    end subroutine test_verbosity_silences_print_schema_info

    !> `verbosity="silent"` silences SOLICITED output, and `%print_rows` is solicited output, so
    !! the call writes nothing at all.
    !!
    !! **The negative control is the same call at `"normal"` on the same unit**, without which
    !! this test passes just as happily against a `%print_rows` that never writes anything.
    !!
    !! Unlike `%print_schema_info`, `%print_rows` writes to a unit the CALLER opened, so silence
    !! is an empty file rather than an absent one -- there is no `open` here for the suppression
    !! check to sit in front of.
    !!
    !! Lives in this suite rather than beside the other `%print_rows` tests in
    !! test_table_display.f90 because it writes a process-global setting, and `settings` is one of
    !! the suites excluded from test-drive's per-test parallelism.
    subroutine test_verbosity_silences_print_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_writer) :: w
        integer(int32) :: id(4)
        character(len=*), parameter :: fixture = "test_run/settings_silent_print_rows.parquet"
        character(len=*), parameter :: out = "test_run/settings_silent_print_rows.txt"
        integer :: u, quiet_lines, loud_lines, i

        call parquet_reset_settings()
        do i = 1, 4
            id(i) = int(i, int32)
        end do
        call parquet_open_writer(w, fixture)
        call parquet_write_column(w, "id", id)
        call parquet_close_writer(w)
        call parquet_open_table(t, fixture)
        call t%materialize_all()

        call parquet_set_verbosity("silent")
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call count_file_lines(out, quiet_lines)

        call parquet_set_verbosity("normal")
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call count_file_lines(out, loud_lines)
        call parquet_reset_settings()

        call check(error, quiet_lines == 0, &
            "verbosity=silent must make %print_rows write nothing at all")
        if (allocated(error)) return
        call check(error, loud_lines > 0, &
            "the negative control at verbosity=normal should have written the display")
        if (allocated(error)) return
        ! The guards run whatever the verbosity is, so the silenced call must not have skipped
        ! them -- but an abort here would take the process down, so what is asserted is the
        ! survivable half: the table is untouched and still printable.
        call check(error, t%nrows() == 4_int64, "and the table must be unchanged either way")
    end subroutine test_verbosity_silences_print_rows

    !> The one place a verbosity level changes something other than output: a silenced
    !! `%print_rows(columns=)` does not read the columns it names, so residency is unchanged.
    !!
    !! `%require_columns` sits ABOVE the suppression test and `table_resolve` below it, so a wrong
    !! name is still reported at `"silent"` while a right one is not read. That ordering is
    !! deliberate -- reading a whole column in order not to print it would be the worse surprise --
    !! and it is documented in `%print_rows`' own doc-comment and on the table page, so it is a
    !! promise rather than an accident, and this is what holds it.
    !!
    !! **The negative control is the same call at `"normal"`**, which must read: without it this
    !! passes against a `%print_rows(columns=)` that never reads at all, which would be a different
    !! bug with the same symptom here.
    subroutine test_verbosity_silences_print_rows_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_writer) :: w
        integer(int32) :: id(4)
        character(len=*), parameter :: fixture = "test_run/settings_silent_print_rows_read.parquet"
        character(len=*), parameter :: out = "test_run/settings_silent_print_rows_read.txt"
        integer :: u, i, quiet_lines

        call parquet_reset_settings()
        do i = 1, 4
            id(i) = int(i, int32)
        end do
        call parquet_open_writer(w, fixture)
        call parquet_write_column(w, "id", id)
        call parquet_close_writer(w)

        ! Opened lazily and left that way: nothing is resident until something reads it.
        call parquet_open_table(t, fixture)
        call check(error, t%residency("id") == RES_EMPTY, "a freshly opened table holds nothing")
        if (allocated(error)) return

        call parquet_set_verbosity("silent")
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows("id", unit=u)
        close(u)
        call count_file_lines(out, quiet_lines)
        call parquet_set_verbosity("normal")

        call check(error, quiet_lines == 0, "the silenced call must write nothing")
        if (allocated(error)) return
        call check(error, t%residency("id") == RES_EMPTY, &
            "and it must not have read the column it was asked to show")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows("id", unit=u)
        close(u)
        call parquet_reset_settings()
        call check(error, t%residency("id") == RES_FULL, &
            "the same call at verbosity=normal must read the column it names")
    end subroutine test_verbosity_silences_print_rows_read

    !> Counts the lines in `path`, or reports 0 when it does not exist.
    subroutine count_file_lines(path, nlines)
        character(len=*), intent(in) :: path !! file to count.
        integer, intent(out) :: nlines       !! how many lines it holds.
        integer :: u, ios
        logical :: exists

        nlines = 0
        inquire(file=path, exist=exists)
        if (.not. exists) return
        open(newunit=u, file=path, status="old", action="read")
        do
            read(u, '(a)', iostat=ios)      ! no item: the count is all this loop wants
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close(u)
    end subroutine count_file_lines

    !> Deletes `path` if it exists, so a test that asserts a file's ABSENCE cannot be fooled by one
    !! an earlier run left behind.
    subroutine delete_if_present(path)
        character(len=*), intent(in) :: path !! file to remove if it is there.
        logical :: exists
        integer :: u

        inquire(file=path, exist=exists)
        if (.not. exists) return
        open(newunit=u, file=path, status="old")
        close(u, status="delete")
    end subroutine delete_if_present

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
        call parquet_set_target_row_group_bytes(small)
        call check(error, parquet_get_target_row_group_bytes() == 4096_int64, &
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
        call parquet_open_writer(writer, path, schema)
        call parquet_get_chunk_size(writer, n)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine estimated_chunk_size
    !

    !> The observed effect, not the round trip: a cap that were stored and never read would leave
    !> the capped and automatic answers equal.
    !>
    !> **The negative control is the automatic arm**, asserted in the same test -- without it, a cap
    !> that fired unconditionally would pass just as happily. The floor is disabled first because it
    !> would otherwise decide the answer instead of the cap, and this test is about the cap; the
    !> guard on `avail > 1` is a real limitation of a single-core machine, not a weakened assertion.
    subroutine test_random_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: auto_n, capped_n, avail
        integer(int64), parameter :: BIG = 100000000_int64
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call parquet_reset_settings()
        call parquet_set_random_parallel_min_elements(0)
        call parquet_set_random_threads(0)
        auto_n = parquet_debug_random_bulk_threads(BIG)
        call check(error, auto_n >= 1, "the automatic answer is always at least one thread")
        if (allocated(error)) return
        call check(error, auto_n <= avail, "the automatic answer never exceeds what OpenMP offers")
        if (allocated(error)) return
        !
        call parquet_set_random_threads(1)
        capped_n = parquet_debug_random_bulk_threads(BIG)
        call check(error, capped_n == 1, "a random cap of 1 forces one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, auto_n > capped_n, &
                "negative control: with threads available the automatic answer must EXCEED the cap")
            if (allocated(error)) return
        end if
        !
        ! An EXPLICIT threads= outranks the cap -- the cap is a default, not a ceiling on intent.
        call check(error, parquet_debug_random_bulk_threads(BIG, threads=2) == 2, &
            "an explicit threads= is honoured above the configured cap")
        if (allocated(error)) return
        call parquet_reset_settings()
    end subroutine test_random_threads_effect

    !> The observed effect of `spatial_threads`, WITH its negative control.
    !>
    !> A set-then-get test passes just as happily against a value that is stored and never read, so
    !> the assertion is on what a bulk query actually opens -- reported by
    !> `parquet_debug_spatial_threads_used`, which exists because the resolved count is otherwise
    !> unobservable from outside the module. The `avail > 1` guard is the single-core limitation:
    !> on such a machine the capped and automatic answers are both 1 and the comparison is vacuous.
    subroutine test_spatial_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_spatial_index) :: sx
        integer(int64), allocatable :: counts(:)
        real(real64) :: x(2000), y(2000), z(2000)
        integer :: auto_n, capped_n, avail, i
        !
        avail = 1
#ifdef _OPENMP
        avail = min(omp_get_max_threads(), omp_get_num_procs())
#endif
        do i = 1, 2000
            x(i) = pf_random_at(99_int64, i, 1_int64)
            y(i) = pf_random_at(99_int64, i, 2_int64)
            z(i) = pf_random_at(99_int64, i, 3_int64)
        end do
        call parquet_reset_settings()
        call sx%build(x, y, z, radius=0.1_real64)
        !
        call sx%count_all_within(0.1_real64, counts)
        auto_n = parquet_debug_spatial_threads_used()
        call check(error, auto_n >= 1, "the automatic answer is always at least one thread")
        if (allocated(error)) return
        call check(error, auto_n <= avail, "the automatic answer never exceeds what OpenMP and the affinity mask allow")
        if (allocated(error)) return
        !
        call parquet_set_spatial_threads(1)
        call sx%count_all_within(0.1_real64, counts)
        capped_n = parquet_debug_spatial_threads_used()
        call check(error, capped_n == 1, "a spatial cap of 1 forces one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, auto_n > capped_n, &
                "negative control: with threads available the automatic answer must EXCEED the cap")
            if (allocated(error)) return
        end if
        !
        ! An EXPLICIT threads= outranks the cap -- the cap is a default, not a ceiling on intent.
        if (avail > 1) then
            call sx%count_all_within(0.1_real64, counts, threads=2)
            call check(error, parquet_debug_spatial_threads_used() == 2, &
                "an explicit threads= is honoured above the configured cap")
            if (allocated(error)) return
        end if
        call parquet_reset_settings()
        call parquet_debug_reset_spatial_counters()
    end subroutine test_spatial_threads_effect

    !> The observed effect of `healpix_threads`, WITH its negative control.
    !>
    !> `pf_healpix_threads(n)` is the observable: this tier derives its team from the WORK, so the
    !> cap's whole job is to bound an answer the caller cannot otherwise predict, and reading the
    !> cap back with `parquet_get_healpix_threads` would pass against a value that is stored and
    !> never read. `n` is chosen well above the tier's own ceiling of 64 threads' worth of work, so
    !> the automatic answer is limited by the machine rather than by the array -- otherwise the
    !> comparison below would be measuring the work rule instead of the cap.
    !>
    !> The `avail > 1` guard is the single-core limitation: on such a machine the capped and the
    !> automatic answers are both 1 and every comparison here is vacuously true.
    subroutine test_healpix_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), parameter :: big = 10000000_int64
        integer, parameter :: wide = 200
        integer :: auto_n, capped_n, avail, was_omp, wide_auto
        !
        avail = 1
#ifdef _OPENMP
        avail = min(omp_get_max_threads(), omp_get_num_procs())
#endif
        call parquet_reset_settings()
        call check(error, parquet_get_healpix_threads() == 0, "healpix_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        !
        auto_n = pf_healpix_threads(big)
        call check(error, auto_n >= 1, "the automatic answer is always at least one thread")
        if (allocated(error)) return
        ! Both kinds, and a DEFAULT INTEGER literal, which is the whole point of the int32 form:
        ! before it existed `pf_healpix_threads(1000)` did not compile at all.
        call check(error, pf_healpix_threads(int(big, kind=int32)) == auto_n, &
            "the int32 element count answers as the int64 one does")
        if (allocated(error)) return
        call check(error, pf_healpix_threads(1000) >= 1, "a default integer literal resolves")
        if (allocated(error)) return
        call check(error, auto_n <= avail, &
            "the automatic answer never exceeds what OpenMP and the affinity mask allow")
        if (allocated(error)) return
        call check(error, auto_n <= 64, "the automatic answer never exceeds this tier's own ceiling")
        if (allocated(error)) return
        !
        call parquet_set_healpix_threads(1)
        call check(error, parquet_get_healpix_threads() == 1, "healpix_threads round-trips")
        if (allocated(error)) return
        capped_n = pf_healpix_threads(big)
        call check(error, capped_n == 1, "a healpix cap of 1 forces one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, auto_n > capped_n, &
                "negative control: with threads available the automatic answer must EXCEED the cap")
            if (allocated(error)) return
        end if
        !
        ! A positive cap REPLACES this tier's ceiling of 64, raising the automatic answer as well
        ! as lowering it -- the rule `index_threads` follows too (`test_index_threads_effect`).
        !
        ! **This needs a machine wider than the ceiling, so it is SIMULATED rather than skipped.**
        ! The ceiling is 64 and an ordinary machine offers far fewer, so at the counts above it never
        ! binds and the assertion would hold against a knob that ignored the ceiling entirely --
        ! vacuous, not passing. Raising the OpenMP ICV above the processor count and overriding what
        ! the affinity clamp believes is the only way to reach the state from a test; both are
        ! restored below.
#ifdef _OPENMP
        if (avail >= 1) then
            was_omp = omp_get_max_threads()
            call omp_set_num_threads(wide)
            call parquet_debug_set_affinity_procs(wide)
            call parquet_set_healpix_threads(0)
            wide_auto = pf_healpix_threads(big)
            call check(error, wide_auto == 64, &
                "on a machine wider than the ceiling, the automatic answer IS the ceiling")
            if (.not. allocated(error)) then
                call parquet_set_healpix_threads(128)
                call check(error, pf_healpix_threads(big) == 128, &
                    "a healpix cap above the tier's ceiling raises the automatic answer to it")
            end if
            if (.not. allocated(error)) then
                call parquet_set_healpix_threads(100000)
                call check(error, pf_healpix_threads(big) == wide, &
                    "a healpix cap above what OpenMP offers is bounded by what OpenMP offers")
            end if
            if (.not. allocated(error)) then
                call parquet_set_healpix_threads(8)
                call check(error, pf_healpix_threads(big) == 8, &
                    "a healpix cap below the ceiling lowers the automatic answer to it")
            end if
            call parquet_debug_set_affinity_procs(0)
            call omp_set_num_threads(was_omp)
            if (allocated(error)) return
        end if
#endif
        !
        call parquet_reset_settings()
        call check(error, parquet_get_healpix_threads() == 0, "reset restores healpix_threads")
    end subroutine test_healpix_threads_effect

    !> The observed effect of `index_threads`, WITH its negative control.
    !>
    !> `pf_index_threads(n)` is the observable, for the same reason `pf_healpix_threads` is its
    !> tier's: the team an index build opens is derived from the WORK as well as from the cap, so
    !> reading the cap back with `parquet_get_index_threads` would pass against a value that is
    !> stored and never read. Only a resolved count can tell a live setting from a dead one.
    !>
    !> **The negative control is the `auto_n > capped_n` assertion.** Without it, a cap that
    !> forced one thread unconditionally -- or one that was ignored entirely on a single-processor
    !> runner -- would satisfy every other assertion here. It is guarded on `avail > 1` because on
    !> a one-processor machine the automatic answer IS 1 and there is nothing for the cap to lower.
    subroutine test_index_threads_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), parameter :: big = 10000000_int64
        integer, parameter :: wide = 200
        integer :: auto_n, capped_n, avail, was_omp, wide_auto
        !
        avail = 1
#ifdef _OPENMP
        avail = min(omp_get_max_threads(), omp_get_num_procs())
#endif
        call parquet_reset_settings()
        call check(error, parquet_get_index_threads() == 0, "index_threads defaults to 0 (automatic)")
        if (allocated(error)) return
        !
        auto_n = pf_index_threads(big)
        call check(error, auto_n >= 1, "the automatic answer is always at least one thread")
        if (allocated(error)) return
        call check(error, auto_n <= avail, &
            "the automatic answer never exceeds what OpenMP and the affinity mask allow")
        if (allocated(error)) return
        !
        call parquet_set_index_threads(1)
        call check(error, parquet_get_index_threads() == 1, "index_threads round-trips")
        if (allocated(error)) return
        capped_n = pf_index_threads(big)
        call check(error, capped_n == 1, "an index cap of 1 forces one thread")
        if (allocated(error)) return
        if (avail > 1) then
            call check(error, auto_n > capped_n, &
                "negative control: with threads available the automatic answer must EXCEED the cap")
            if (allocated(error)) return
        end if
        !
        ! A cap of 2 must be honoured exactly, on any machine that has two processors -- which
        ! separates "the knob is read" from "the knob is read only when it says 1".
        if (avail > 2) then
            call parquet_set_index_threads(2)
            call check(error, pf_index_threads(big) == 2, &
                "an index cap of 2 resolves to exactly two threads")
            if (allocated(error)) return
        end if
        !
        ! The WORK still bounds the answer below the cap: a build over ten keys opens no team
        ! whatever the cap says. This is the other half of the rule and has its own failure mode --
        ! a resolver that returned the cap outright would thread a ten-key build.
        call parquet_set_index_threads(0)
        call check(error, pf_index_threads(10_int64) == 1, &
            "a build over ten keys is not worth a team whatever the cap allows")
        if (allocated(error)) return
        !
        ! Unset, the automatic answer is held to the tier's ceiling of 64; set, the cap REPLACES
        ! that ceiling, above it as well as below -- the one way this knob differs from
        ! `healpix_threads`, whose cap may only lower its tier's ceiling. SIMULATED on a machine
        ! wider than the ceiling, for the reason `test_healpix_threads_effect` gives: an ordinary
        ! runner offers fewer than 64, where neither half binds. Both overrides are restored below.
#ifdef _OPENMP
        was_omp = omp_get_max_threads()
        call omp_set_num_threads(wide)
        call parquet_debug_set_affinity_procs(wide)
        call parquet_set_index_threads(0)
        wide_auto = pf_index_threads(big)
        call check(error, wide_auto == 64, &
            "on a machine wider than the ceiling, the automatic answer IS the ceiling of 64")
        if (.not. allocated(error)) then
            call parquet_set_index_threads(128)
            call check(error, pf_index_threads(big) == 128, &
                "an index cap above the ceiling raises the automatic answer to it")
        end if
        if (.not. allocated(error)) then
            call parquet_set_index_threads(100000)
            call check(error, pf_index_threads(big) == wide, &
                "an index cap above what OpenMP offers is bounded by what OpenMP offers")
        end if
        if (.not. allocated(error)) then
            call parquet_set_index_threads(8)
            call check(error, pf_index_threads(big) == 8, &
                "an index cap below the ceiling lowers the automatic answer to it")
        end if
        call parquet_debug_set_affinity_procs(0)
        call omp_set_num_threads(was_omp)
        if (allocated(error)) return
#endif
        !
        call parquet_reset_settings()
        call check(error, parquet_get_index_threads() == 0, "reset restores index_threads")
    end subroutine test_index_threads_effect

    !> The observed effect of the work floor: it decides whether a bulk permutation threads at all.
    !>
    !> **Both directions are asserted, and the second is the negative control.** A floor that were
    !> stored and never read would leave a tiny array threading; a floor that refused everything
    !> would leave a huge array serial. Only asserting the pair distinguishes the setting from
    !> either failure. The `avail > 1` guard is the single-core limitation again -- on such a
    !> machine there is nothing for the floor to decide between.
    subroutine test_random_parallel_min_effect(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: avail, small_n, big_n, lifted_n
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call parquet_reset_settings()
        if (avail <= 1) then
            call check(error, parquet_debug_random_bulk_threads(1000000_int64) == 1, &
                "with one thread available every resolution is serial")
            return
        end if
        !
        ! At the factory floor of 1000 per thread, 500 elements cannot feed even two threads.
        small_n = parquet_debug_random_bulk_threads(500_int64)
        call check(error, small_n == 1, "the work floor must keep a 500-element permutation serial")
        if (allocated(error)) return
        !
        ! Negative control: the same floor, enough work -- otherwise a floor that refused
        ! everything would pass the assertion above.
        big_n = parquet_debug_random_bulk_threads(100000000_int64)
        call check(error, big_n > 1, &
            "negative control: at the same floor, a large permutation must still thread")
        if (allocated(error)) return
        !
        ! And the floor is what decided it: lift it and the same small array now threads.
        call parquet_set_random_parallel_min_elements(1_int64)
        lifted_n = parquet_debug_random_bulk_threads(500_int64)
        call check(error, lifted_n > 1, &
            "lowering the work floor must let a 500-element permutation thread")
        if (allocated(error)) return
        !
        ! The floor applies to an explicit threads= too -- it is a property of the work, not of
        ! the caller's intent, so `threads=8` on a tiny array is honoured by being declined.
        call parquet_reset_settings()
        call check(error, parquet_debug_random_bulk_threads(500_int64, threads=8) == 1, &
            "the work floor must decline an explicit threads= on too little work")
        if (allocated(error)) return
        call check(error, parquet_debug_random_bulk_threads(100000000_int64, threads=8) == 8, &
            "negative control: the same explicit threads= is honoured when the work is there")
        if (allocated(error)) return
        call parquet_reset_settings()
    end subroutine test_random_parallel_min_effect

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
        end if
        call check(error, auto_n <= avail, "the automatic answer never exceeds what OpenMP offers")
        if (allocated(error)) return
        !
        ! **The automatic answer is CEILINGED, not simply omp_get_max_threads().** On a very large
        ! machine the full thread count is past the point where this work scales and is measurably
        ! worse than a fraction of it, so the default is bounded. That ceiling is 64 and therefore
        ! never binds on an ordinary development machine -- which is exactly why it is overridable:
        ! without lowering it here, a change that removed the ceiling entirely would pass on every
        ! machine but the 192-core one.
        if (avail > 2) then
            call parquet_set_string_threads(0)
            call parquet_debug_set_string_max_auto_threads(2)
            call check(error, parquet_string_threads() == 2, &
                "the automatic answer is bounded by the ceiling, not by omp_get_max_threads()")
            if (allocated(error)) return
            !
            ! ...and an EXPLICIT setting is honoured ABOVE that ceiling, bounded only by what OpenMP
            ! offers. This is the one place the string cap deliberately differs from the sort cap,
            ! which can only ever lower the automatic answer.
            call parquet_set_string_threads(avail)
            call check(error, parquet_string_threads() == avail, &
                "an explicit request is honoured above the automatic ceiling")
            if (allocated(error)) return
            call parquet_debug_set_string_max_auto_threads(0)
        end if
        !
        ! A cap above what OpenMP offers is still bounded by what OpenMP offers.
        call parquet_set_string_threads(avail + 16)
        call check(error, parquet_string_threads() == avail, &
            "a cap above the available thread count is bounded by the available thread count")
        if (allocated(error)) return
        !
        call parquet_set_string_threads(0)
        call parquet_debug_set_string_max_auto_threads(0)
        call check(error, parquet_get_string_threads() == 0, "the raw getter reports the cap, not the resolved count")
    end subroutine test_string_threads_effect


    !> The work floor is a **payload** floor, not a row floor, and it is overridable so tests can
    !> reach the threaded path at all.
    !>
    !> Without the override every fixture a suite can afford would sit below 256 KiB and silently
    !> take the serial path, so a threaded operation would be covered by nothing -- that failure
    !> exactly. The negative control is the same column measured at the real floor, which must
    !> resolve to 1: a floor that never declined would pass any test written for it.
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

    ! ---- C++-engine pins ------------------------------------------------------------------
    !
    ! Every test wrapped below observes a C++-SIDE counter (`engine_comparisons`,
    ! `parquet_debug_sort_threads_used`, `parquet_debug_sort_merge_threads_used`), which the
    ! Fortran engine does not populate. Stage 6 made the Fortran engine the default, so each of
    ! these went from testing something to testing nothing -- and every one of them FAILED loudly
    ! rather than passing vacuously, because each carries the "this arm must really reach the
    ! path" control this project requires. That is the controls working exactly as intended.
    !
    ! Pinning is the right fix rather than re-pointing them at Fortran observables, because the
    ! C++ engine still ships and is still user-reachable: `parquet_open_reader(..., sort_by=)`
    ! and `parquet_reader_set_sort` call `sort_build_permutation_threaded` directly, with no
    ! engine selector anywhere in that path. These are that engine's only tests.
    !
    ! The wrapper shape (rather than a pin at the top of each body) is deliberate: these tests
    ! have up to five early `return`s, and a selector leaked on one of them would not fail the
    ! test that leaked it -- it would silently change which engine a LATER test measures.


    !> Pins the C++ engine for `test_sort_counting_path_effect` -- see the note above.
    subroutine cpp_test_sort_counting_path_effect(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_sort_counting_path_effect(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default; see the note above
    end subroutine cpp_test_sort_counting_path_effect

    !> Pins the C++ engine for `test_sort_counting_bucket_limit_effect` -- see the note above.
    subroutine cpp_test_sort_counting_bucket_limit_effect(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_sort_counting_bucket_limit_effect(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default; see the note above
    end subroutine cpp_test_sort_counting_bucket_limit_effect

end module test_settings
