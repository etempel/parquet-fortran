!> Tests for `parquet_logging`: levels, sinks, layout, context, deduplication and threading.
!!
!! **Every assertion reads output back from a file sink**, because that is the only way to see
!! what a logger actually wrote. Each test therefore owns its own fixture path -- test-drive runs
!! a suite's tests concurrently, so a shared path is truncated out from under whichever test got
!! there second. (This suite is additionally excluded from that parallelism in `run_tester.f90`,
!! because it reconfigures the process-global default logger and the process-wide dedup table;
!! the per-test paths are belt and braces, and they also keep a failure readable.)
!!
!! **Expectations are constructed independently, never by a second call into the same renderer.**
!! Comparing two paths that share the layout engine passes just as happily against a renderer that
!! is wrong in the same way on both sides.
module test_logging
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_logging
    use iso_fortran_env, only : int64, real64
    use iso_c_binding, only : c_null_char, c_int
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_thread_num
#endif
    implicit none
    private

    public :: collect_tests_logging

contains

    !> Opens a scratch file, returns its unit. The caller closes it.
    subroutine open_scratch(path, unit)
        character(len=*), intent(in) :: path  !! Fixture path, unique to the calling test.
        integer, intent(out) :: unit          !! Receives the connected unit.

        open (newunit = unit, file = path, action = "write", form = "formatted", status = "replace")
    end subroutine open_scratch

    !> Truncates a fixture file, so a test whose sink appends starts from a known state.
    !!
    !! `add_file` appends by default, which is the right default for a log and the wrong one for a
    !! fixture read back by assertion: without this the file accumulates across runs and the line
    !! count a test asserts is a count of every run since the last `git clean`.
    subroutine truncate_file(path)
        character(len=*), intent(in) :: path  !! Fixture path to empty.
        integer :: u

        open (newunit = u, file = path, action = "write", form = "formatted", status = "replace")
        close (u)
    end subroutine truncate_file

    !> Reads a file back into `lines`, reporting how many were read.
    subroutine read_back(path, lines, n)
        character(len=*), intent(in) :: path                 !! The file to read.
        character(len=512), allocatable, intent(out) :: lines(:) !! Receives the lines.
        integer, intent(out) :: n                            !! Number of lines read.
        integer :: u, ios
        character(len=512) :: buf

        allocate (lines(512))
        lines = ""
        n = 0
        open (newunit = u, file = path, action = "read", status = "old", iostat = ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat = ios) buf
            if (ios /= 0) exit
            n = n + 1
            if (n > size(lines)) exit
            lines(n) = buf
        end do
        close (u)
    end subroutine read_back

    !> Whether `hay` contains `needle`.
    logical function has(hay, needle) result(yes)
        character(len=*), intent(in) :: hay     !! Text searched.
        character(len=*), intent(in) :: needle  !! Text sought.

        yes = index(hay, needle) > 0
    end function has

    !> A record is emitted at or above the threshold and dropped below it, with the negative
    !> control in the same test -- a threshold that admits everything would pass a one-sided check.
    subroutine test_level_threshold(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(level = PF_LEVEL_WARNING, console = .false.)
        call lg%add_file("test_run/log_level.txt", append = .false., format = "{level}|{message}")
        call lg%error("above")
        call lg%warning("at")
        call lg%info("below")
        call lg%debug("far below")
        call lg%close()

        call read_back("test_run/log_level.txt", lines, n)
        call check(error, n == 2, "exactly the two records at or above WARNING are written")
        if (allocated(error)) return
        call check(error, has(lines(1), "ERROR") .and. has(lines(1), "above"), &
            "the ERROR record is first and carries its level")
        if (allocated(error)) return
        call check(error, has(lines(2), "WARNING") .and. has(lines(2), "at"), &
            "the WARNING record is second")
    end subroutine test_level_threshold

    !> A freshly declared logger owns no sinks and is silent; `%enabled` says so at every level.
    subroutine test_fresh_logger_is_silent(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg

        call check(error, .not. lg%enabled(PF_LEVEL_CRITICAL), &
            "a fresh logger is not enabled even at CRITICAL")
        if (allocated(error)) return
        call check(error, .not. lg%enabled(PF_LEVEL_ALL), "a fresh logger is not enabled at ALL")
        if (allocated(error)) return
        ! Must not print, and must not abort.
        call lg%critical("this must reach nothing at all")
        call check(error, .not. lg%enabled(PF_LEVEL_INFO), "still silent after a call")
    end subroutine test_fresh_logger_is_silent

    !> `%init` clears every sink; `console = .false.` installs none; `%close` returns to silence.
    subroutine test_init_and_close_sink_contract(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(console = .false.)
        call check(error, .not. lg%enabled(PF_LEVEL_CRITICAL), "init(console=.false.) leaves no sink")
        if (allocated(error)) return

        call lg%add_file("test_run/log_initclose.txt", append = .false., format = "{message}")
        call check(error, lg%enabled(PF_LEVEL_INFO), "a file sink makes the logger enabled")
        if (allocated(error)) return
        call lg%info("kept")

        ! A second init must CLEAR the file sink, so nothing after it reaches the file.
        call lg%init(console = .false.)
        call lg%info("must not be written")
        call lg%close()

        call read_back("test_run/log_initclose.txt", lines, n)
        call check(error, n == 1, "only the record written before the second init is in the file")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "kept", "and it is the expected one")
    end subroutine test_init_and_close_sink_contract

    !> Two sinks filter independently, and each renders with its own layout.
    subroutine test_two_sinks_filter_independently(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: a(:), b(:)
        integer :: na, nb

        call lg%init(level = PF_LEVEL_ALL, console = .false.)
        call lg%add_file("test_run/log_two_a.txt", level = PF_LEVEL_DEBUG, append = .false., &
            format = "A:{message}")
        call lg%add_file("test_run/log_two_b.txt", level = PF_LEVEL_ERROR, append = .false., &
            format = "B:{message}")
        call lg%debug("d")
        call lg%error("e")
        call lg%close()

        call read_back("test_run/log_two_a.txt", a, na)
        call read_back("test_run/log_two_b.txt", b, nb)
        call check(error, na == 2, "the DEBUG-level sink took both records")
        if (allocated(error)) return
        call check(error, nb == 1, "the ERROR-level sink took only the error")
        if (allocated(error)) return
        call check(error, trim(a(1)) == "A:d", "the first sink used its own layout")
        if (allocated(error)) return
        call check(error, trim(b(1)) == "B:e", "the second sink used its own layout")
    end subroutine test_two_sinks_filter_independently

    !> `{field|sep}` emits the separator only when the field is non-empty, which is what keeps an
    !> unnamed record from leaving a stray separator mid-line.
    subroutine test_layout_separator_collapses(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(console = .false.)
        call lg%add_file("test_run/log_sep.txt", append = .false., format = "[{name| }]{message}")
        call lg%info("nameless")
        call lg%set_name("worker")
        call lg%info("named")
        call lg%close()

        call read_back("test_run/log_sep.txt", lines, n)
        call check(error, n == 2, "both records written")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "[]nameless", &
            "an empty name emits neither the field nor its separator")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "[ worker]named", &
            "a non-empty name emits the separator and then the value")
    end subroutine test_layout_separator_collapses

    !> Context frames nest: a nested push renders both, the inner pop leaves the outer intact, and
    !> `clear_context` empties the stack from any depth.
    subroutine test_context_nesting(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, frame

        call pf_log_clear_context()
        call pf_log_set_context("")
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_ctx.txt", append = .false., format = "{context}|{message}")

        call lg%info("none")
        call pf_log_push_context("tile=17", frame)
        call check(error, frame == 1, "the first push reports depth 1")
        if (allocated(error)) return
        call lg%info("one")
        call pf_log_push_context("fibre=3")
        call check(error, pf_log_context_depth() == 2, "depth is 2 after the second push")
        if (allocated(error)) return
        call lg%info("two")
        call pf_log_pop_context()
        call lg%info("back")
        call pf_log_push_context("x=1")
        call pf_log_push_context("y=2")
        call pf_log_clear_context()
        call check(error, pf_log_context_depth() == 0, "clear_context empties the stack from depth 3")
        if (allocated(error)) return
        call lg%info("cleared")
        call lg%close()

        call read_back("test_run/log_ctx.txt", lines, n)
        call check(error, n == 5, "five records written")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "|none", "no context renders empty")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "tile=17|one", "one frame renders alone")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "tile=17 fibre=3|two", "a nested push renders both frames")
        if (allocated(error)) return
        call check(error, trim(lines(4)) == "tile=17|back", &
            "the inner pop removes only its own frame and leaves the outer one intact")
        if (allocated(error)) return
        call check(error, trim(lines(5)) == "|cleared", "clear_context leaves no context at all")
    end subroutine test_context_nesting

    !> The shared base context renders ahead of a thread's own frames, and `clear_context` does
    !> not touch it -- the two are separate mechanisms and only one is per thread.
    subroutine test_context_base_and_frames(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call pf_log_clear_context()
        call pf_log_set_context("phase=fit")
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_ctxbase.txt", append = .false., format = "{context}|{message}")
        call lg%info("base only")
        call pf_log_push_context("tile=4")
        call lg%info("base and frame")
        call pf_log_clear_context()
        call lg%info("base survives clear")
        call pf_log_set_context("")
        call lg%info("base cleared")
        call lg%close()

        call read_back("test_run/log_ctxbase.txt", lines, n)
        call check(error, n == 4, "four records written")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "phase=fit|base only", "the base renders on its own")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "phase=fit tile=4|base and frame", &
            "the base renders ahead of the thread's frames")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "phase=fit|base survives clear", &
            "clear_context empties the frames and leaves the shared base")
        if (allocated(error)) return
        call check(error, trim(lines(4)) == "|base cleared", "an empty base clears it")
    end subroutine test_context_base_and_frames

    !> Past `PF_LOG_MAX_CONTEXT_DEPTH` the frame TEXT is dropped while the DEPTH stays exact, so a
    !> later pop still removes the right frame and no record is tagged with another's context.
    !!
    !! Asserting the depth is the point: the rendered text alone cannot tell saturation from a
    !! shifted stack, which is the failure this rule exists to prevent.
    subroutine test_context_saturation_keeps_depth(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i

        call pf_log_clear_context()
        call pf_log_set_context("")
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_ctxsat.txt", append = .false., format = "{context}|{message}")

        do i = 1, PF_LOG_MAX_CONTEXT_DEPTH + 3
            call pf_log_push_context("f")
        end do
        call check(error, pf_log_context_depth() == PF_LOG_MAX_CONTEXT_DEPTH + 3, &
            "the depth counts every push, including the ones whose text is dropped")
        if (allocated(error)) return
        call lg%info("saturated")
        do i = 1, 3
            call pf_log_pop_context()
        end do
        call check(error, pf_log_context_depth() == PF_LOG_MAX_CONTEXT_DEPTH, &
            "popping the over-budget frames restores the depth exactly")
        if (allocated(error)) return
        call lg%info("at budget")
        call pf_log_clear_context()
        call lg%close()

        call read_back("test_run/log_ctxsat.txt", lines, n)
        call check(error, n == 2, "both records written")
        if (allocated(error)) return
        ! The kept frames are exactly PF_LOG_MAX_CONTEXT_DEPTH copies of "f", space separated.
        call check(error, trim(lines(1)) == repeat("f ", PF_LOG_MAX_CONTEXT_DEPTH - 1) // "f|saturated", &
            "only the frames within the depth budget are rendered")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == repeat("f ", PF_LOG_MAX_CONTEXT_DEPTH - 1) // "f|at budget", &
            "and popping back to the budget leaves exactly the same text, not a shifted stack")
    end subroutine test_context_saturation_keeps_depth

    !> A `pop` on an empty stack is a no-op rather than an error, so a defensive pop costs nothing.
    subroutine test_pop_on_empty_is_a_noop(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.

        call pf_log_clear_context()
        call pf_log_pop_context()
        call pf_log_pop_context()
        call check(error, pf_log_context_depth() == 0, "popping an empty stack leaves it empty")
    end subroutine test_pop_on_empty_is_a_noop

    !> `once=` emits once and never again; without it the same record is emitted every time. The
    !> negative control is what makes this a test of deduplication rather than of nothing.
    subroutine test_once_and_every(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i

        call pf_log_reset_dedup()
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_once.txt", append = .false., format = "{message}")
        do i = 1, 5
            call lg%warning("deduped", once = .true.)
        end do
        do i = 1, 5
            call lg%warning("not deduped")
        end do
        do i = 1, 6
            call lg%info("throttled", every = 3)
        end do
        call lg%close()
        call pf_log_reset_dedup()

        call read_back("test_run/log_once.txt", lines, n)
        call check(error, n == 1 + 5 + 2, "once= gives one line, the control five, every=3 two")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "deduped", "the once= record is emitted the first time")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "not deduped", "the control record is not suppressed")
    end subroutine test_once_and_every

    !> The dedup table is process-wide and is cleared only by `pf_log_reset_dedup` -- deliberately
    !> not by `%init` or `%close`, which would let one logger un-suppress another's messages.
    subroutine test_dedup_reset_is_explicit(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call pf_log_reset_dedup()
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_dedupreset.txt", append = .false., format = "{message}")
        call lg%warning("m", once = .true.)
        ! init and close must NOT reset the table.
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_dedupreset.txt", append = .true., format = "{message}")
        call lg%warning("m", once = .true.)
        ! only the explicit reset does.
        call pf_log_reset_dedup()
        call lg%warning("m", once = .true.)
        call lg%close()
        call pf_log_reset_dedup()

        call read_back("test_run/log_dedupreset.txt", lines, n)
        call check(error, n == 2, &
            "init/close do not reset the dedup table; pf_log_reset_dedup does, so two lines")
    end subroutine test_dedup_reset_is_explicit

    !> A per-name override applies to the name and to every name below it in dotted notation, and
    !> `%enabled` answers exactly when it is given the name and conservatively when it is not.
    subroutine test_name_overrides(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(level = PF_LEVEL_DEBUG, console = .false.)
        call lg%add_file("test_run/log_names.txt", append = .false., format = "{name}|{message}")
        call lg%set_level(PF_LEVEL_ERROR, name = "noisy")
        call lg%debug("kept", name = "quiet")
        call lg%debug("dropped", name = "noisy")
        call lg%debug("dropped too", name = "noisy.sub")
        call lg%error("kept as error", name = "noisy")
        call lg%debug("kept, not a dotted child", name = "noisyother")

        ! Assert %enabled BEFORE closing: %close clears every sink, after which every level is
        ! disabled and the three assertions below would hold for the wrong reason.
        call check(error, lg%enabled(PF_LEVEL_DEBUG), &
            "without a name, enabled is the conservative answer and says yes")
        if (allocated(error)) return
        call check(error, .not. lg%enabled(PF_LEVEL_DEBUG, name = "noisy"), &
            "with the name, enabled answers exactly and says no")
        if (allocated(error)) return
        call check(error, lg%enabled(PF_LEVEL_DEBUG, name = "quiet"), &
            "and yes for a name no override covers")
        if (allocated(error)) return
        call lg%close()

        call read_back("test_run/log_names.txt", lines, n)
        call check(error, n == 3, "the two records below the override's level are dropped")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "noisy|kept as error", &
            "the override lets a record at its own level through")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "noisyother|kept, not a dotted child", &
            "a name that merely shares a prefix is not a dotted child and is unaffected")
    end subroutine test_name_overrides

    !> A per-name override LOWERS a threshold as well as raising one, which is what makes
    !> "turn this one subsystem up to TRACE and leave the rest alone" expressible.
    !!
    !! Four things are asserted, and the last two are what keep the first two honest. The lowered
    !! name is admitted below the logger's own level; a name with no rule at that level is still
    !! dropped, which is the negative control — a cache simply lowered for everyone would pass the
    !! first assertion and fail this one. Raising the rule back retires it, so the effect is not a
    !! one-way door. And a SINK's own threshold is not lowered by a rule, since an override
    !! governs what the logger offers rather than what a sink accepts.
    subroutine test_name_override_lowers(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_name_lower.txt"
        character(len=*), parameter :: spath = "test_run/log_name_lower_sink.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n
        logical :: on_deep, on_other, after_retire, floor_low, floor_restored

        call lg%init(level = PF_LEVEL_DEBUG, console = .false.)
        call lg%add_file(path, append = .false., format = "{name}|{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "deep")

        on_deep  = lg%enabled(PF_LEVEL_TRACE, name = "deep")
        on_other = lg%enabled(PF_LEVEL_TRACE, name = "other")
        ! The nameless %enabled reads the cached floor directly, which is the only observable of
        ! it through the public API -- and the floor is what decides whether a sub-threshold
        ! record is rejected by one integer comparison or carried as far as the name test. It must
        ! drop when a lowering rule is added and RISE AGAIN when that rule is retired, or "set the
        ! level back to normal" would restore the answers while leaving the cost in place forever.
        floor_low = lg%enabled(PF_LEVEL_TRACE)

        call lg%trace("admitted by the lowered rule", name = "deep")
        call lg%trace("no rule at this level", name = "other")          ! control: dropped
        call lg%trace("a dotted child of the rule", name = "deep.inner")
        call lg%trace("merely shares a prefix", name = "deeper")        ! control: dropped
        call lg%debug("at the logger's own level", name = "other")

        ! Raising the rule back to the logger's own level retires it.
        call lg%set_level(PF_LEVEL_DEBUG, name = "deep")
        after_retire = lg%enabled(PF_LEVEL_TRACE, name = "deep")
        floor_restored = .not. lg%enabled(PF_LEVEL_TRACE)
        call lg%trace("after the rule was raised back", name = "deep")  ! control: dropped
        call lg%close()
        call read_back(path, lines, n)

        call check(error, on_deep, "enabled says yes for the lowered name")
        if (allocated(error)) return
        call check(error, .not. on_other, "enabled still says no for a name with no rule")
        if (allocated(error)) return
        call check(error, .not. after_retire, "raising the rule back retires it")
        if (allocated(error)) return
        call check(error, floor_low, "the cached floor drops when a lowering rule is added")
        if (allocated(error)) return
        call check(error, floor_restored, &
            "and rises again when that rule is retired, so the cost does not outlive it")
        if (allocated(error)) return
        call check(error, n == 3, "exactly the lowered name, its dotted child and the DEBUG record")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "deep|admitted by the lowered rule", &
            "the lowered rule admits a record below the logger's own level")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "deep.inner|a dotted child of the rule", &
            "the rule reaches a dotted child")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "other|at the logger's own level", &
            "a name with no rule keeps the logger's threshold in both directions")
        if (allocated(error)) return

        ! A sink's own threshold is independent of any name rule.
        call lg%init(level = PF_LEVEL_DEBUG, console = .false.)
        call lg%add_file(spath, append = .false., level = PF_LEVEL_WARNING, format = "{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "deep")
        call lg%trace("below the sink's own threshold", name = "deep")
        call lg%warning("at the sink's threshold", name = "deep")
        call lg%close()
        call read_back(spath, lines, n)

        call check(error, n == 1, "a name rule does not lower a sink's own threshold")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "at the sink's threshold", &
            "the record the sink does accept is the one that arrives")
    end subroutine test_name_override_lowers

    !> `%unset_level` removes an override, frees its slot, and restores the cached floor.
    !!
    !! The slot half is the reason the procedure exists and is asserted directly: the table holds
    !! `PF_LOG_MAX_NAME_RULES` names and `%set_level` appends, so without a way to free one a
    !! program that turns tracing on and off for more distinct names than that aborts having
    !! "undone" every one. Filling the table, unsetting one and adding a further name is what
    !! proves the slot came back — a test that only checked the threshold would pass against an
    !! implementation that merely raised the rule to the logger's level.
    subroutine test_unset_level(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_unset_level.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        character(len=8) :: nm
        integer :: n, k
        logical :: hit_present, hit_absent, floor_low, floor_restored

        call lg%init(level = PF_LEVEL_DEBUG, console = .false.)
        call lg%add_file(path, append = .false., format = "{name}|{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "deep")

        floor_low = lg%enabled(PF_LEVEL_TRACE)          ! the lowering rule dropped the floor
        call lg%trace("while the rule is set", name = "deep")

        call lg%unset_level("deep", found = hit_present)
        floor_restored = .not. lg%enabled(PF_LEVEL_TRACE)
        call lg%trace("after the rule is unset", name = "deep")   ! control: dropped
        call lg%debug("still at the logger level", name = "deep")

        ! Removing something that is not there is a no-op, and reports so.
        call lg%unset_level("never-set", found = hit_absent)
        call lg%debug("unaffected by the no-op", name = "other")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, hit_present, "unset_level reports that it removed the override")
        if (allocated(error)) return
        call check(error, .not. hit_absent, "and reports that it removed nothing when there was none")
        if (allocated(error)) return
        call check(error, floor_low .and. floor_restored, &
            "the cached floor drops with the rule and rises again when it is unset")
        if (allocated(error)) return
        call check(error, n == 3, "only the record emitted while the rule was set gets through")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "deep|while the rule is set", &
            "the override applied before it was unset")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "deep|still at the logger level", &
            "and the name follows the logger's own threshold afterwards")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "other|unaffected by the no-op", &
            "a no-op unset disturbs no other rule")
        if (allocated(error)) return

        ! The slot is genuinely freed: fill the table, unset one, and add one more distinct name.
        ! A sink is attached because %enabled answers .false. for everything on a logger with no
        ! sink at all -- without one, every assertion below would hold for that reason instead.
        call lg%init(level = PF_LEVEL_DEBUG, console = .false.)
        call lg%add_file("test_run/log_unset_slots.txt", append = .false., format = "{message}")
        do k = 1, PF_LOG_MAX_NAME_RULES
            write (nm, '("r", i0)') k
            call lg%set_level(PF_LEVEL_WARNING, name = trim(nm))
        end do
        call lg%unset_level("r7", found = hit_present)
        call lg%set_level(PF_LEVEL_ERROR, name = "one-more")   ! aborts if the slot was not freed
        call check(error, hit_present, "the table was full and one entry was removed")
        if (allocated(error)) return
        call check(error, .not. lg%enabled(PF_LEVEL_DEBUG, name = "one-more"), &
            "the name added into the freed slot has its override")
        if (allocated(error)) return
        call check(error, lg%enabled(PF_LEVEL_DEBUG, name = "r7"), &
            "and the unset name is back on the logger's own threshold")
        if (allocated(error)) return

        ! With no name, every override goes.
        call lg%unset_level(found = hit_present)
        call check(error, hit_present, "unset_level with no name removed the remaining overrides")
        if (allocated(error)) return
        call check(error, lg%enabled(PF_LEVEL_DEBUG, name = "r1") .and. &
            lg%enabled(PF_LEVEL_DEBUG, name = "one-more"), &
            "every name is back on the logger's own threshold")
        if (allocated(error)) return
        call lg%unset_level(found = hit_absent)
        call check(error, .not. hit_absent, "and a second clear reports that there was nothing left")
        call lg%close()
    end subroutine test_unset_level

    !> A sink's rank filter emits only for the matching rank.
    subroutine test_rank_filter(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(console = .false.)
        call lg%set_rank(3)
        call lg%add_file("test_run/log_rank_all.txt", append = .false., format = "{rank}|{message}")
        call lg%add_file("test_run/log_rank_0.txt", append = .false., only_rank = 0, format = "{message}")
        call lg%info("from rank 3")
        call lg%close()

        call read_back("test_run/log_rank_all.txt", lines, n)
        call check(error, n == 1, "the unfiltered sink took the record")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "3|from rank 3", "and rendered the rank")
        if (allocated(error)) return
        call read_back("test_run/log_rank_0.txt", lines, n)
        call check(error, n == 0, "the rank-0 sink took nothing from rank 3")
    end subroutine test_rank_filter

    !> A line longer than `PF_LOG_MAX_LINE` is emitted in full, never truncated.
    subroutine test_long_line_is_not_truncated(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        integer :: u, ios, want
        character(len=:), allocatable :: msg, got

        want = PF_LOG_MAX_LINE + 500
        msg = repeat("x", want)
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_long.txt", append = .false., format = "{message}")
        call lg%info(msg)
        call lg%close()

        allocate (character(len=want + 100) :: got)
        open (newunit = u, file = "test_run/log_long.txt", action = "read", status = "old")
        read (u, '(a)', iostat = ios) got
        close (u)
        call check(error, ios == 0, "the over-long record was written")
        if (allocated(error)) return
        call check(error, len_trim(got) == want, &
            "a line longer than PF_LOG_MAX_LINE arrives whole rather than truncated")
    end subroutine test_long_line_is_not_truncated

    !> `%blank` reaches every sink, unlike the terminal-only blank line of older Fortran loggers.
    subroutine test_blank_reaches_file_sinks(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(console = .false.)
        call lg%add_file("test_run/log_blank.txt", append = .false., format = "{message}")
        call lg%info("a")
        call lg%blank(2)
        call lg%info("b")
        call lg%close()

        call read_back("test_run/log_blank.txt", lines, n)
        call check(error, n == 4, "two records and two blank lines reach the file")
        if (allocated(error)) return
        call check(error, len_trim(lines(2)) == 0 .and. len_trim(lines(3)) == 0, &
            "the two middle lines are blank")
    end subroutine test_blank_reaches_file_sinks

    !> `pf_str` renders each kind for concatenation, and honours a caller's format.
    subroutine test_pf_str(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.

        call check(error, trim(pf_str(12345)) == "12345", "int32 renders")
        if (allocated(error)) return
        call check(error, trim(pf_str(5000000000_int64)) == "5000000000", "int64 renders")
        if (allocated(error)) return
        call check(error, trim(pf_str(3.5_real64, '(f6.2)')) == "3.50", "a format descriptor is honoured")
        if (allocated(error)) return
        call check(error, trim(pf_str(.true.)) == "T", "logical renders")
    end subroutine test_pf_str

    !> Level names convert both ways, case-insensitively, and an unknown name is reported through
    !> `ok` rather than aborting.
    subroutine test_level_names(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        integer :: lev
        logical :: ok
        character(len=16) :: nm

        call pf_log_level_from_name("warning", lev)
        call check(error, lev == PF_LEVEL_WARNING, "a lower-case name converts")
        if (allocated(error)) return
        call pf_log_level_from_name("CRITICAL", lev)
        call check(error, lev == PF_LEVEL_CRITICAL, "an upper-case name converts")
        if (allocated(error)) return
        call pf_log_level_from_name("25", lev)
        call check(error, lev == 25, "a bare number converts")
        if (allocated(error)) return
        call pf_log_level_from_name("nonsense", lev, ok)
        call check(error, .not. ok, "an unknown name reports failure through ok")
        if (allocated(error)) return
        call pf_log_level_name(PF_LEVEL_ERROR, nm)
        call check(error, trim(nm) == "ERROR", "a level renders as its name")
        if (allocated(error)) return
        call pf_log_level_name(17, nm)
        call check(error, trim(nm) == "Level 17", "a non-standard level renders as a number")
    end subroutine test_level_names

    !> A non-standard integer level is accepted rather than rejected, and is filtered numerically.
    subroutine test_nonstandard_level_is_accepted(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(level = 25, console = .false.)
        call lg%add_file("test_run/log_lvl25.txt", append = .false., format = "{message}")
        call lg%log(24, "below")
        call lg%log(25, "at")
        call lg%log(26, "above")
        call lg%close()

        call read_back("test_run/log_lvl25.txt", lines, n)
        call check(error, n == 2, "an arbitrary integer level filters numerically")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "at", "the record at the threshold is kept")
    end subroutine test_nonstandard_level_is_accepted

    !> Buffered mode keeps one thread's records contiguous and in order, and the post-region
    !> `%flush()` recovers every worker's -- which a thread-private store could not do.
    subroutine test_buffered_mode_recovers_every_thread(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i, seen, expect
        character(len=32) :: tag

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with no threads the collector holds one slot and " // &
            "both arms of the assertion below hold for the wrong reason")
        return
#endif
        if (.not. can_thread()) then
            call skip_test(error, "needs more than one usable thread")
            return
        end if

        call lg%init(console = .false.)
        call lg%add_file("test_run/log_buffered.txt", append = .false., format = "{message}")
        call lg%set_thread_mode(PF_LOG_THREAD_BUFFERED, slot_bytes = PF_LOG_MIN_BUFFER_BYTES)
        expect = 0
        !$omp parallel do default(shared) private(i, tag) schedule(static) reduction(+:expect)
        do i = 1, 40
            write (tag, '("w",i0)') i
            call lg%info(trim(tag) // "a")
            call lg%info(trim(tag) // "b")
            expect = expect + 2
        end do
        !$omp end parallel do
        call lg%flush()
        call lg%close()

        call read_back("test_run/log_buffered.txt", lines, n)
        call check(error, n == expect, "every record buffered by every worker thread is recovered")
        if (allocated(error)) return
        ! Each worker's two records must be adjacent: that is the guarantee buffered mode offers.
        seen = 0
        do i = 1, n - 1
            if (has(lines(i), "a") .and. .not. has(lines(i), "b")) then
                if (trim(lines(i)(1:len_trim(lines(i)) - 1)) == &
                    trim(lines(i + 1)(1:len_trim(lines(i + 1)) - 1))) seen = seen + 1
            end if
        end do
        call check(error, seen == 40, "each thread's pair of records stays adjacent and in order")
    end subroutine test_buffered_mode_recovers_every_thread

    !> A record too large for a collector slot is written directly, whole and in sequence.
    subroutine test_buffered_oversize_record(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n
        character(len=:), allocatable :: big

        call lg%init(console = .false.)
        call lg%add_file("test_run/log_bufbig.txt", append = .false., format = "{message}")
        call lg%set_thread_mode(PF_LOG_THREAD_BUFFERED, slot_bytes = PF_LOG_MIN_BUFFER_BYTES)
        big = repeat("y", PF_LOG_MIN_BUFFER_BYTES + 100)
        call lg%info("before")
        call lg%info(big)
        call lg%info("after")
        call lg%flush()
        call lg%close()

        call read_back("test_run/log_bufbig.txt", lines, n)
        call check(error, n == 3, "the oversize record neither vanished nor split")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "before", "the record before it is emitted first")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "after", "and the record after it comes last")
    end subroutine test_buffered_oversize_record

    !> Concurrent `once=` calls produce exactly one line: the lookup and the insert are one
    !> decision inside the output critical section, not two.
    subroutine test_concurrent_once(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread the once= table cannot be raced, so " // &
            "the assertion holds whether or not the decision is atomic")
        return
#endif
        if (.not. can_thread()) then
            call skip_test(error, "needs more than one usable thread")
            return
        end if

        call pf_log_reset_dedup()
        call lg%init(console = .false.)
        call lg%add_file("test_run/log_conconce.txt", append = .false., format = "{message}")
        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, 200
            call lg%warning("raced", once = .true.)
        end do
        !$omp end parallel do
        call lg%close()
        call pf_log_reset_dedup()

        call read_back("test_run/log_conconce.txt", lines, n)
        call check(error, n == 1, "200 concurrent once= calls produce exactly one line")
    end subroutine test_concurrent_once

    !> A logger shared across a parallel region emits every record without corrupting a line.
    subroutine test_shared_logger_in_parallel_region(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i, bad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread no interleaving is possible, so the " // &
            "assertion cannot see a corrupted line")
        return
#endif
        if (.not. can_thread()) then
            call skip_test(error, "needs more than one usable thread")
            return
        end if

        call lg%init(console = .false.)
        call lg%add_file("test_run/log_shared.txt", append = .false., format = "<{message}>")
        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, 300
            call lg%info("record")
        end do
        !$omp end parallel do
        call lg%close()

        call read_back("test_run/log_shared.txt", lines, n)
        call check(error, n == 300, "every record from every thread is written exactly once")
        if (allocated(error)) return
        bad = 0
        do i = 1, n
            if (trim(lines(i)) /= "<record>") bad = bad + 1
        end do
        call check(error, bad == 0, "no line was interleaved or truncated by a concurrent write")
    end subroutine test_shared_logger_in_parallel_region

    !> Whether the run has more than one usable thread, so that a threading assertion is not
    !> vacuous. Both halves matter: a build without OpenMP, and a one-processor runner.
    logical function can_thread() result(yes)

        yes = .false.
#ifdef _OPENMP
        yes = omp_get_max_threads() > 1
#endif
    end function can_thread

    ! ==================================================================================
    ! The module-level default-logger surface
    !
    ! Every procedure below is a `pf_log_*` shim over the module's own `g_default`. They are NOT
    ! thin wrappers over the bindings tested above: each routes through `emit_core`'s
    ! `implicit` argument, which the bindings always pass as `.false.` and which these pass as
    ! `g_default_implicit`. That is a separate branch, so it needs separate tests -- and the
    ! branch it selects when `g_default_implicit` is still `.true.` writes to stdout, which no
    ! in-process test can read, so THAT half is covered by the `logging_implicit_console`
    ! error scenario instead.
    !
    ! These tests share one process-global logger, so each begins by re-initialising it and ends
    ! by closing it. `run_tester.f90` excludes this suite from test-drive's parallelism for
    ! exactly this reason.
    ! ==================================================================================

    !> Sets an environment variable for the rest of this process, via POSIX `setenv`.
    !!
    !! Fortran cannot set one. Declared locally here rather than in `src/`, the same convention
    !! `test/test_settings.f90` follows, so no library file gains a POSIX dependency.
    subroutine set_env(name, value)
        character(len=*), intent(in) :: name   !! Variable to set.
        character(len=*), intent(in) :: value  !! Its new value.
        interface
            function c_setenv(nm, val, overwrite) bind(C, name="setenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*)   !! NUL-terminated name.
                character(kind=c_char), intent(in) :: val(*)  !! NUL-terminated value.
                integer(c_int), value :: overwrite            !! Nonzero replaces an existing value.
                integer(c_int) :: rc                          !! 0 on success.
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
        character(len=*), intent(in) :: name  !! Variable to remove.
        interface
            function c_unsetenv(nm) bind(C, name="unsetenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*)  !! NUL-terminated name.
                integer(c_int) :: rc                         !! 0 on success.
            end function c_unsetenv
        end interface
        integer :: rc

        rc = int(c_unsetenv(name // c_null_char))
        if (rc /= 0) error stop "unset_env: unsetenv failed for '" // name // "'"
    end subroutine unset_env

    !> Every emission shim on the default logger reaches a sink, carries the right level tag, and
    !> obeys the logger-wide threshold -- with the two levels below that threshold as the negative
    !> control, since a shim wired to the wrong level would still produce a line.
    subroutine test_default_logger_emission(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_default_emit.txt"
        character(len=512), allocatable :: lines(:)
        integer :: n

        call pf_log_reset_dedup()
        call pf_log_init(level = PF_LEVEL_DEBUG, console = .false.)
        call pf_log_add_file(path, format = "{level}|{message}", append = .false.)

        call pf_log_trace("t-record")                       ! below DEBUG: dropped
        call pf_log_debug("d-record")
        call pf_log_info("i-record")
        call pf_log_warning("w-record")
        call pf_log_error("e-record")
        call pf_log_critical("c-record")
        call pf_log(PF_LEVEL_INFO, "explicit-level-record")
        call pf_log(PF_LEVEL_TRACE, "explicit-below-record")  ! below DEBUG: dropped

        call pf_log_flush()
        call pf_log_close()
        call read_back(path, lines, n)

        call check(error, n == 6, "six of the eight records clear the DEBUG threshold")
        if (allocated(error)) return
        call check(error, has(lines(1), "DEBUG") .and. has(lines(1), "d-record"), &
            "pf_log_debug emits at DEBUG")
        if (allocated(error)) return
        call check(error, has(lines(2), "INFO") .and. has(lines(2), "i-record"), &
            "pf_log_info emits at INFO")
        if (allocated(error)) return
        call check(error, has(lines(3), "WARNING") .and. has(lines(3), "w-record"), &
            "pf_log_warning emits at WARNING")
        if (allocated(error)) return
        call check(error, has(lines(4), "ERROR") .and. has(lines(4), "e-record"), &
            "pf_log_error emits at ERROR")
        if (allocated(error)) return
        call check(error, has(lines(5), "CRITICAL") .and. has(lines(5), "c-record"), &
            "pf_log_critical emits at CRITICAL")
        if (allocated(error)) return
        call check(error, has(lines(6), "INFO") .and. has(lines(6), "explicit-level-record"), &
            "pf_log carries the level it was given")
        if (allocated(error)) return
        ! The negative control: neither sub-threshold record reached the file under any name.
        call check(error, .not. has(lines(1), "t-record"), "pf_log_trace was dropped below DEBUG")
        if (allocated(error)) return
        call check(error, .not. has(lines(6), "explicit-below-record"), &
            "pf_log at TRACE was dropped below DEBUG")
    end subroutine test_default_logger_emission

    !> The configuration shims each change what the default logger writes: name, rank, layout,
    !> per-sink level, blank lines, and `pf_log_enabled`'s answer. A shim that silently did
    !> nothing would leave the line unchanged, so every assertion is on rendered output.
    subroutine test_default_logger_configuration(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_default_config.txt"
        character(len=512), allocatable :: lines(:)
        integer :: n, sk
        logical :: before, after

        call pf_log_reset_dedup()
        call pf_log_init(console = .false.)
        call pf_log_add_file(path, append = .false., sink = sk)
        call pf_log_set_format("{name}|{rank}|{message}", sink = sk)
        call pf_log_set_name("shim.app")
        call pf_log_set_rank(3)
        call pf_log_set_thread_mode(PF_LOG_THREAD_DIRECT)
        call pf_log_set_color(PF_LOG_COLOR_NEVER)

        ! pf_log_enabled is read here rather than after %close, which clears every sink and would
        ! make it answer .false. whatever the threshold had been.
        before = pf_log_enabled(PF_LEVEL_INFO)
        call pf_log_info("configured")
        call pf_log_blank(2)
        ! A console sink that can never emit, because the logger's rank is 3 and this sink admits
        ! only rank 99. It exercises pf_log_add_console without writing to the terminal during a
        ! test run -- and it is added after the blank lines above because %blank honours the rank
        ! filter but not a level, so nothing else would hold it back.
        call pf_log_add_console(only_rank = 99)
        call pf_log_set_level(PF_LEVEL_ERROR)
        after = pf_log_enabled(PF_LEVEL_INFO)
        call pf_log_info("suppressed")
        call pf_log_blank(1)
        call pf_log_flush()
        call pf_log_close()
        call read_back(path, lines, n)

        call check(error, before .and. .not. after, &
            "pf_log_enabled tracks pf_log_set_level, in both directions")
        if (allocated(error)) return
        call check(error, n == 4, "one record, two blank lines, and one more blank after them")
        if (allocated(error)) return
        call check(error, has(lines(1), "shim.app") .and. has(lines(1), "configured"), &
            "pf_log_set_name and pf_log_set_format both took effect")
        if (allocated(error)) return
        call check(error, has(lines(1), "|3|"), "pf_log_set_rank renders through {rank}")
        if (allocated(error)) return
        call check(error, len_trim(lines(2)) == 0 .and. len_trim(lines(3)) == 0 .and. &
            len_trim(lines(4)) == 0, "pf_log_blank wrote exactly the blank lines asked for")
        if (allocated(error)) return
        call check(error, .not. has(lines(4), "suppressed"), &
            "the record below the raised threshold never reached the file")
    end subroutine test_default_logger_configuration

    !> `pf_log_configure_from_env` applies each variable it recognises and leaves the logger
    !> untouched for one that is unset -- the second half being the control, since a reader that
    !> applied a default on an unset variable would pass every positive assertion.
    subroutine test_configure_from_env(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_env_file.txt"
        character(len=512), allocatable :: lines(:)
        integer :: n

        call pf_log_reset_dedup()
        call pf_log_init(console = .false.)
        ! PFLOGTEST_FILE reaches pf_log_add_file with no append= argument, so it appends -- the
        ! documented default. Start from an empty file or this asserts a count across every run.
        call truncate_file(path)
        call unset_env("PFLOGTEST_COLOR")            ! deliberately left unset: the control
        call set_env("PFLOGTEST_LEVEL", "warning")
        call set_env("PFLOGTEST_FORMAT", "env|{level}|{message}")
        call set_env("PFLOGTEST_FILE", path)
        call pf_log_configure_from_env("PFLOGTEST_")

        call pf_log_info("env-below")     ! below WARNING: dropped
        call pf_log_error("env-above")
        call pf_log_flush()
        call pf_log_close()
        call unset_env("PFLOGTEST_LEVEL")
        call unset_env("PFLOGTEST_FORMAT")
        call unset_env("PFLOGTEST_FILE")
        call read_back(path, lines, n)

        call check(error, n == 1, "PFLOGTEST_FILE added a sink and PFLOGTEST_LEVEL filtered it")
        if (allocated(error)) return
        call check(error, has(lines(1), "env|") .and. has(lines(1), "ERROR"), &
            "PFLOGTEST_FORMAT replaced the layout")
        if (allocated(error)) return
        call check(error, has(lines(1), "env-above") .and. .not. has(lines(1), "env-below"), &
            "the record below the environment's threshold was dropped")
    end subroutine test_configure_from_env

    !> A caller-owned unit is written to and NOT closed by `%close`, which is the whole difference
    !> between `add_unit` and `add_file`. Both the binding and the module-level shim are covered,
    !> since they reach different loggers.
    subroutine test_add_unit_sink(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_add_unit.txt"
        character(len=*), parameter :: dpath = "test_run/log_add_unit_default.txt"
        character(len=512), allocatable :: lines(:)
        type(pf_logger) :: lg
        integer :: u, n, ios
        logical :: still_open

        call open_scratch(path, u)
        call lg%init(console = .false.)
        call lg%add_unit(u, format = "{message}")
        call lg%info("through-a-unit")
        call lg%close()
        ! %close must NOT have closed a unit it did not open. Both halves are the proof: the unit
        ! still reports as connected, and a further write through it actually lands in the file.
        inquire (unit = u, opened = still_open)
        ios = -1
        if (still_open) write (u, '(a)', iostat = ios) "after-close"
        close (u)
        call read_back(path, lines, n)

        call check(error, still_open, "%close left a caller-owned unit open")
        if (allocated(error)) return
        call check(error, ios == 0, "a write through the caller's unit still succeeded")
        if (allocated(error)) return
        call check(error, n == 2, "the record and the post-close write both reached the file")
        if (allocated(error)) return
        call check(error, has(lines(1), "through-a-unit"), "the record reached the caller's unit")
        if (allocated(error)) return
        call check(error, has(lines(2), "after-close"), "the caller could still write afterwards")
        if (allocated(error)) return

        ! The module-level shim, on the default logger.
        call open_scratch(dpath, u)
        call pf_log_init(console = .false.)
        call pf_log_add_unit(u, format = "{message}")
        call pf_log_info("default-through-a-unit")
        call pf_log_close()
        close (u)
        call read_back(dpath, lines, n)
        call check(error, n == 1, "pf_log_add_unit attached exactly one sink")
        if (allocated(error)) return
        call check(error, has(lines(1), "default-through-a-unit"), &
            "pf_log_add_unit reached the caller's unit")
    end subroutine test_add_unit_sink

    !> The four clock fields render in the documented shapes. Every expectation is built from the
    !> field's own definition rather than from a second render, and each is checked
    !> character-class by character-class so that a field silently emitting nothing, or emitting
    !> another field's text, fails.
    subroutine test_layout_clock_fields(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_clock_fields.txt"
        character(len=512), allocatable :: lines(:)
        type(pf_logger) :: lg
        integer :: n, p, q
        character(len=512) :: d, t, s, e

        call lg%init(console = .false.)
        call lg%add_file(path, append = .false., format = "D={date}|T={time}|S={stamp}|E={elapsed}|M={message}")
        call lg%info("clock")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 1, "one record reached the file")
        if (allocated(error)) return
        call field_between(lines(1), "D=", "|T=", d)
        call field_between(lines(1), "T=", "|S=", t)
        call field_between(lines(1), "S=", "|E=", s)
        call field_between(lines(1), "E=", "|M=", e)

        ! {date} is YYYY-MM-DD.
        call check(error, len_trim(d) == 10 .and. all_digits(d(1:4)) .and. d(5:5) == "-" .and. &
            all_digits(d(6:7)) .and. d(8:8) == "-" .and. all_digits(d(9:10)), &
            "{date} renders as YYYY-MM-DD, got '" // trim(d) // "'")
        if (allocated(error)) return
        ! {time} is HH:MM:SS.mmm.
        call check(error, len_trim(t) == 12 .and. all_digits(t(1:2)) .and. t(3:3) == ":" .and. &
            all_digits(t(4:5)) .and. t(6:6) == ":" .and. all_digits(t(7:8)) .and. &
            t(9:9) == "." .and. all_digits(t(10:12)), &
            "{time} renders as HH:MM:SS.mmm, got '" // trim(t) // "'")
        if (allocated(error)) return
        ! {stamp} is exactly {date}, one space, {time} -- asserted against the two fields rendered
        ! on this same line, so a stamp built from a second clock reading would fail.
        call check(error, trim(s) == trim(d) // " " // trim(t), &
            "{stamp} is {date} space {time}, got '" // trim(s) // "'")
        if (allocated(error)) return
        ! {elapsed} is H:MM:SS.sss, zero-padded, and non-empty because a configuration call has
        ! established the clock origin.
        p = index(e, ":")
        q = index(e, ".")
        call check(error, len_trim(e) >= 10 .and. p == 2 .and. q == len_trim(e) - 3 .and. &
            all_digits(e(1:1)) .and. all_digits(e(3:4)) .and. e(5:5) == ":" .and. &
            all_digits(e(6:7)) .and. all_digits(e(q + 1:q + 3)), &
            "{elapsed} renders as H:MM:SS.sss with no embedded blank, got '" // trim(e) // "'")
    end subroutine test_layout_clock_fields

    !> Extracts the text between two markers on a line, so a field assertion names the field it
    !> failed on rather than the whole rendered record.
    subroutine field_between(line, open_mark, close_mark, out)
        character(len=*), intent(in) :: line        !! The rendered record.
        character(len=*), intent(in) :: open_mark   !! Marker immediately before the field.
        character(len=*), intent(in) :: close_mark  !! Marker immediately after it.
        character(len=*), intent(out) :: out        !! Receives the field's text.
        integer :: a, b

        out = ""
        a = index(line, open_mark)
        if (a == 0) return
        a = a + len(open_mark)
        b = index(line(a:), close_mark)
        if (b == 0) then
            out = line(a:)
        else
            out = line(a:a + b - 2)
        end if
    end subroutine field_between

    !> Whether every character of `s` is a decimal digit.
    logical function all_digits(s) result(yes)
        character(len=*), intent(in) :: s  !! The text to test.
        integer :: i

        yes = len(s) > 0
        do i = 1, len(s)
            if (s(i:i) < "0" .or. s(i:i) > "9") yes = .false.
        end do
    end function all_digits

    !> `{thread}` renders the emitting thread's number, and `{rank}` collapses with its separator
    !> when no rank is set -- the pair that a serial-only test would leave entirely unexercised.
    subroutine test_layout_thread_and_rank(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_thread_field.txt"
        character(len=512), allocatable :: lines(:)
        type(pf_logger) :: lg
        integer :: n
        character(len=512) :: th

        call lg%init(console = .false.)
        call lg%add_file(path, append = .false., format = "T={thread}|{rank| R=}|M={message}")
        call lg%info("no-rank")       ! rank unset: {rank} and its separator both collapse
        call lg%set_rank(7)
        call lg%info("with-rank")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 2, "both records reached the file")
        if (allocated(error)) return
        call field_between(lines(1), "T=", "|", th)
        ! Serially this is thread 0; inside a region it would be that thread's number. Either way
        ! it is an integer, and asserting the shape rather than the value keeps this test correct
        ! under every thread count.
        call check(error, all_digits(trim(th)), &
            "{thread} renders an integer, got '" // trim(th) // "'")
        if (allocated(error)) return
        call check(error, .not. has(lines(1), "R="), &
            "an unset {rank} collapses its separator with it")
        if (allocated(error)) return
        call check(error, has(lines(2), "R=7"), "a set {rank} renders with its separator")
    end subroutine test_layout_thread_and_rank

    !> Colour is emitted under `_ALWAYS`, absent under `_NEVER`, and absent on a file sink under
    !> `_AUTO` -- three arms, because a policy that ignored its argument would satisfy any one of
    !> them. Every level that HAS a colour is checked against its own constant, and the one that
    !> does not (`INFO`) is the control: a renderer emitting some fixed code for everything would
    !> otherwise pass every positive assertion here.
    subroutine test_color_policies(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_color.txt"
        character(len=512), allocatable :: lines(:)
        character(len=1), parameter :: esc = achar(27)
        type(pf_logger) :: lg
        character(len=:), allocatable :: wrapped
        integer :: n, sk

        call lg%init(console = .false.)
        call lg%add_file(path, append = .false., format = "{level}|{message}", sink = sk)
        call lg%warning("auto-on-a-file")             ! 1  AUTO: a file is never coloured
        call lg%set_color(PF_LOG_COLOR_ALWAYS, sink = sk)
        call lg%warning("always-warning")             ! 2
        call lg%error("always-error")                 ! 3
        call lg%info("always-info")                   ! 4  INFO has no colour code at all
        call lg%critical("always-critical")           ! 5
        call lg%debug("always-debug")                 ! 6
        call lg%trace("always-trace")                 ! 7
        call lg%set_color(PF_LOG_COLOR_NEVER, sink = sk)
        call lg%warning("never")                      ! 8
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 8, "all eight records reached the file")
        if (allocated(error)) return
        call check(error, .not. has(lines(1), esc), &
            "PF_LOG_COLOR_AUTO leaves a file sink uncoloured")
        if (allocated(error)) return
        call check(error, has(lines(2), esc // "[" // PF_LOG_C_YELLOW // "m"), &
            "PF_LOG_COLOR_ALWAYS colours WARNING yellow")
        if (allocated(error)) return
        call check(error, has(lines(3), esc // "[" // PF_LOG_C_RED // "m"), &
            "PF_LOG_COLOR_ALWAYS colours ERROR red")
        if (allocated(error)) return
        call check(error, .not. has(lines(4), esc), &
            "INFO carries no colour code even under _ALWAYS")
        if (allocated(error)) return
        call check(error, has(lines(5), esc // "[" // PF_LOG_C_BRIGHT_RED // "m"), &
            "PF_LOG_COLOR_ALWAYS colours CRITICAL bright red")
        if (allocated(error)) return
        call check(error, has(lines(6), esc // "[" // PF_LOG_C_CYAN // "m"), &
            "PF_LOG_COLOR_ALWAYS colours DEBUG cyan")
        if (allocated(error)) return
        call check(error, has(lines(7), esc // "[" // PF_LOG_C_DIM // "m"), &
            "PF_LOG_COLOR_ALWAYS dims TRACE")
        if (allocated(error)) return
        call check(error, .not. has(lines(8), esc), &
            "PF_LOG_COLOR_NEVER turns colour back off")
        if (allocated(error)) return

        call pf_log_color("hot", PF_LOG_C_RED, wrapped)
        call check(error, wrapped == esc // "[" // PF_LOG_C_RED // "m" // "hot" // esc // "[0m", &
            "pf_log_color wraps text in the code it was given")
    end subroutine test_color_policies

    !> A blank line obeys a sink's rank filter. It has no level for a threshold to act on, so the
    !> rank filter is the only thing standing between a rank-filtered sink and every rank's blank
    !> lines -- and the sink that should receive them is the control proving the filter is not
    !> simply refusing everything.
    subroutine test_blank_obeys_rank_filter(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: mine = "test_run/log_blank_rank_mine.txt"
        character(len=*), parameter :: other = "test_run/log_blank_rank_other.txt"
        character(len=512), allocatable :: lines(:)
        type(pf_logger) :: lg
        integer :: n_mine, n_other

        call lg%init(console = .false.)
        call lg%set_rank(0)
        call lg%add_file(mine, append = .false., format = "{message}", only_rank = 0)
        call lg%add_file(other, append = .false., format = "{message}", only_rank = 1)
        call lg%blank(3)
        call lg%close()

        call read_back(mine, lines, n_mine)
        call read_back(other, lines, n_other)
        call check(error, n_mine == 3, "the matching sink received every blank line")
        if (allocated(error)) return
        call check(error, n_other == 0, "the non-matching sink received none")
    end subroutine test_blank_obeys_rank_filter

    !> The two built-in templates parse and render, `PF_LEVEL_OFF` admits nothing, and
    !> `pf_log_elapsed` returns a monotonic time.
    !!
    !! The templates are worth their own test because they are the two defaults: a typo in either
    !! is an `error stop` the first time any program logs anything, and no other test names them
    !! (every one sets a layout of its own so it can assert on a predictable line).
    subroutine test_builtin_templates_and_off(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_builtin_fmt.txt"
        character(len=512), allocatable :: lines(:)
        type(pf_logger) :: lg
        integer :: n
        real(real64) :: t0, t1

        call lg%init(console = .false., name = "tpl")
        call lg%add_file(path, append = .false., format = PF_LOG_FMT_BRIEF)
        call lg%add_file("test_run/log_builtin_fmt_full.txt", append = .false., format = PF_LOG_FMT_FULL)
        call lg%warning("templated")
        ! PF_LEVEL_OFF is a threshold, not a level: nothing may clear it, CRITICAL included.
        call lg%set_level(PF_LEVEL_OFF)
        call lg%critical("must-not-appear")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 1, "PF_LEVEL_OFF admitted nothing, not even CRITICAL")
        if (allocated(error)) return
        ! BRIEF is "{time} [{level}] {message}": time, then a bracketed level, then the text.
        call check(error, has(lines(1), "[WARNING") .and. has(lines(1), "templated") .and. &
            .not. has(lines(1), "tpl"), &
            "PF_LOG_FMT_BRIEF renders time, level and message and no name")
        if (allocated(error)) return
        call read_back("test_run/log_builtin_fmt_full.txt", lines, n)
        call check(error, n == 1, "the second sink took the same one record")
        if (allocated(error)) return
        ! FULL adds the date and the logger name, which is exactly what distinguishes it.
        call check(error, has(lines(1), "tpl") .and. has(lines(1), "-") .and. &
            has(lines(1), "templated"), &
            "PF_LOG_FMT_FULL renders the date and the logger name as well")
        if (allocated(error)) return

        call pf_log_elapsed(t0)
        call lg%init(console = .false.)          ! any work at all between the two readings
        call pf_log_elapsed(t1)
        call lg%close()
        call check(error, t0 >= 0.0_real64 .and. t1 >= t0, &
            "pf_log_elapsed is non-negative and does not go backwards")
    end subroutine test_builtin_templates_and_off

    !> Registers this suite's tests.
    subroutine collect_tests_logging(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)  !! the suite's tests.

        testsuite = [ &
            new_unittest("level threshold filters, with a negative control", test_level_threshold), &
            new_unittest("a freshly declared logger is silent", test_fresh_logger_is_silent), &
            new_unittest("init clears sinks and close returns to silence", test_init_and_close_sink_contract), &
            new_unittest("two sinks filter and format independently", test_two_sinks_filter_independently), &
            new_unittest("an empty field collapses its separator", test_layout_separator_collapses), &
            new_unittest("context frames nest and pop individually", test_context_nesting), &
            new_unittest("the shared base renders ahead of thread frames", test_context_base_and_frames), &
            new_unittest("context saturation keeps the depth exact", test_context_saturation_keeps_depth), &
            new_unittest("popping an empty context stack is a no-op", test_pop_on_empty_is_a_noop), &
            new_unittest("once= and every= deduplicate, with a control", test_once_and_every), &
            new_unittest("only pf_log_reset_dedup clears the table", test_dedup_reset_is_explicit), &
            new_unittest("per-name level overrides and exact enabled", test_name_overrides), &
            new_unittest("a per-name override lowers as well as raises", test_name_override_lowers), &
            new_unittest("unset_level removes an override and frees its slot", test_unset_level), &
            new_unittest("a sink's rank filter selects one rank", test_rank_filter), &
            new_unittest("an over-long line is emitted whole", test_long_line_is_not_truncated), &
            new_unittest("blank lines reach file sinks", test_blank_reaches_file_sinks), &
            new_unittest("pf_str renders every kind", test_pf_str), &
            new_unittest("level names convert both ways", test_level_names), &
            new_unittest("a non-standard integer level is accepted", test_nonstandard_level_is_accepted), &
            new_unittest("buffered mode recovers every thread's records", test_buffered_mode_recovers_every_thread), &
            new_unittest("an oversize buffered record is written whole", test_buffered_oversize_record), &
            new_unittest("concurrent once= emits exactly one line", test_concurrent_once), &
            new_unittest("a shared logger survives a parallel region", test_shared_logger_in_parallel_region), &
            new_unittest("every default-logger emission shim reaches a sink", test_default_logger_emission), &
            new_unittest("every default-logger configuration shim takes effect", &
                test_default_logger_configuration), &
            new_unittest("pf_log_configure_from_env applies what is set", test_configure_from_env), &
            new_unittest("a caller-owned unit is written to and not closed", test_add_unit_sink), &
            new_unittest("date, time, stamp and elapsed render in shape", test_layout_clock_fields), &
            new_unittest("thread renders, and an unset rank collapses", test_layout_thread_and_rank), &
            new_unittest("colour policy is honoured in all three arms", test_color_policies), &
            new_unittest("a blank line obeys a sink's rank filter", test_blank_obeys_rank_filter), &
            new_unittest("the built-in templates render and PF_LEVEL_OFF admits nothing", &
                test_builtin_templates_and_off) &
            ]
    end subroutine collect_tests_logging

end module test_logging
