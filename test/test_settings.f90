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
!> happily against a value that is stored and never used. Here the round-trip does have teeth,
!> because the value makes a round trip through Arrow itself rather than through a module variable:
!> `parquet_get_max_threads` reads Arrow's own capacity, so a getter bound to the wrong C symbol,
!> or a kind mismatch across the bind(C) boundary, fails test 2. That property is specific to this
!> knob; the knobs S2 onward add will each need their own observed effect (see
!> feature_settings.md's testing section).
!>
!> Abort paths (`parquet_set_max_threads(0)`) cannot be exercised here because `error stop` kills
!> the process -- that one lives in test/error_scenarios.f90 as the `set_max_threads_zero`
!> scenario, driven from test_errors.f90.
!>
!> **Every test restores what it changed.** The suite is excluded from test-drive's per-suite
!> parallelism (see run_tester.f90), but suites still share one process, so a leaked capacity of 1
!> would silently serialise every later suite's Arrow work.
module test_settings
    use parquet
    use iso_fortran_env, only : output_unit
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
            new_unittest("print_settings names every setting and limit", test_print_settings) &
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
