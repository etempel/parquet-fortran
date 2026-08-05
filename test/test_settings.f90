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
!> Two tests are deliberately not about any single knob: `test_argument_beats_setting` (a resolution
!> written the wrong way round would make the setting win, and every per-knob test would still pass)
!> and `test_reset_all_knobs` (a knob that is settable but not resettable leaks into every later
!> suite).
!>
!> Abort paths cannot be exercised here because `error stop` kills the process -- they live in
!> test/error_scenarios.f90 (`set_max_threads_zero`, `settings_bad_codec`,
!> `settings_negative_sort_threads`), driven from test_errors.f90.
!>
!> **Every test restores what it changed.** The suite is excluded from test-drive's per-suite
!> parallelism (see run_tester.f90), but suites still share one process, so a leaked capacity of 1
!> would silently serialise every later suite's Arrow work.
module test_settings
    use parquet
    use iso_fortran_env, only : output_unit, int32, int64
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
            new_unittest("default_compression changes the bytes written", test_default_compression_effect), &
            new_unittest("default_compression_level changes the bytes written", test_default_compression_level_effect), &
            new_unittest("naming a codec default drops the zstd-tuned level", test_default_compression_decouples_level), &
            new_unittest("default_use_threads reaches the reader and the writer", test_default_use_threads_effect), &
            new_unittest("an explicit argument always beats the setting", test_argument_beats_setting), &
            new_unittest("an invalid codec default aborts nothing else", test_bad_codec_leaves_state_intact) &
            ]
    end subroutine collect_tests_parquet_settings
    !
    !> Arrow's own default is hardware-derived, so the only portable assertion is that it is a
    !> usable thread count. A getter wired to the wrong symbol would typically answer 0 or garbage.
    subroutine test_threads_default_sane(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: n
        !
        n = parquet_get_max_threads()
        call check(error, n >= 1, "parquet_get_max_threads() >= 1")
    end subroutine test_threads_default_sane
    !
    !> The value goes into Arrow and comes back out of Arrow, so this is the test that fails if the
    !> new getter is bound to the wrong C symbol or declared with a mismatched kind.
    subroutine test_threads_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        original = parquet_get_max_threads()
        call parquet_set_max_threads(3)
        call check(error, parquet_get_max_threads() == 3, "capacity is 3 after parquet_set_max_threads(3)")
        if (allocated(error)) then
            call restore(original)
            return
        end if
        call parquet_set_max_threads(5)
        call check(error, parquet_get_max_threads() == 5, "capacity is 5 after parquet_set_max_threads(5)")
        call restore(original)
    end subroutine test_threads_round_trip
    !
    !> parquet_reset_settings restores the capacity captured on the FIRST set, not the most recent
    !> one -- so two sets followed by a reset must land back on the value from before either.
    subroutine test_reset_restores(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: original
        !
        original = parquet_get_max_threads()
        call parquet_set_max_threads(2)
        call parquet_set_max_threads(7)
        call parquet_reset_settings()
        call check(error, parquet_get_max_threads() == original, &
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
        before = parquet_get_max_threads()
        call parquet_reset_settings()
        call check(error, parquet_get_max_threads() == before, &
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
        call parquet_set_default_compression("gzip")
        call parquet_set_default_compression_level(9)
        call parquet_set_default_use_threads(.false.)
        call parquet_reset_settings()
        !
        call check(error, parquet_get_sort_threads() == 0, "reset restores sort_threads")
        if (allocated(error)) return
        call check(error, parquet_get_prefetch_threads() == 0, "reset restores prefetch_threads")
        if (allocated(error)) return
        call parquet_get_default_compression(codec)
        call check(error, codec == "zstd", "reset restores default_compression")
        if (allocated(error)) return
        call check(error, parquet_get_default_compression_level() == 3, "reset restores default_compression_level")
        if (allocated(error)) return
        call check(error, parquet_get_default_use_threads(), "reset restores default_use_threads")
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

    !> Puts the pool back and clears the module's captured value, so the next test starts from the
    !> same state whatever this one did. Used instead of a bare parquet_set_max_threads(original)
    !> because that would itself capture, leaving a stale capture behind for the next test.
    subroutine restore(original)
        integer, intent(in) :: original !! capacity to restore.
        !
        call parquet_set_max_threads(original)
        call parquet_reset_settings()
    end subroutine restore
    !
end module test_settings
