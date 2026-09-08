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
    ! Shared with test_logging_env, which holds the one test that needs a bind(C) interface and so
    ! had to move to its own file (see that file's header). Public only for that reason.
    public :: truncate_file, read_back, has

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

    !> The `max` composition with BOTH operands non-trivial, which is the guide's own worked table.
    !>
    !> **The two arms were covered separately and never together.** `test_level_threshold` binds the
    !> logger with its sink at `PF_LEVEL_ALL`; `test_two_sinks_filter_independently` binds the sinks
    !> with the logger at `PF_LEVEL_ALL`. Neither exercises `max(logger-side, sink)` with both sides
    !> actually deciding something, which is the case `doc/pages/utilities/logging.md`'s "Which
    !> threshold decides" exists to explain and the one a reader gets wrong.
    !>
    !> The fixture is that page's table verbatim: logger `INFO`, one sink at `DEBUG`, and a rule
    !> putting `deep` at `TRACE`. Each row is a different reason:
    !>
    !> * `TRACE`/`deep`  -- the rule admits it at 5, the SINK's 10 blocks it        -> dropped
    !> * `DEBUG`/`deep`  -- the rule admits it at 5, the sink's 10 admits it        -> written
    !> * `DEBUG`/`other` -- no rule, so the LOGGER's 20 blocks it though the sink would take it
    !> * `INFO`/`other`  -- no rule, the logger's 20 admits it                      -> written
    !>
    !> **The two written rows are the negative control**: without them a test asserting only the
    !> drops passes against a logger that emits nothing at all.
    !>
    !> **What this test does and does not pin, established by mutation rather than by assertion.**
    !> Removing the per-name rule's contribution to the cached floor fails it (`n == 2` becomes
    !> `n == 1`). Removing the PER-SINK gate in `emit_to_sink` does NOT fail it, and that is a
    !> property of the fixture rather than a gap: with a single sink the cached `min_level` is
    !> already `max(floor_level, sink%level)`, so the sink's threshold reaches the decision through
    !> the pre-filter and the per-sink gate is redundant here. The two are individually redundant
    !> and jointly load-bearing, and it takes two sinks at different levels to separate them --
    !> which is what `test_two_sinks_filter_independently` does, and why both tests are needed.
    subroutine test_threshold_composition(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=*), parameter :: path = "test_run/log_compose.txt"
        character(len=512), allocatable :: lines(:)
        integer :: n

        call lg%init(level = PF_LEVEL_INFO, console = .false.)
        call lg%add_file(path, level = PF_LEVEL_DEBUG, append = .false., format = "{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "deep")

        call lg%trace("trace-deep",  name = "deep")    ! rule 5  vs sink 10 -> max 10 -> dropped
        call lg%debug("debug-deep",  name = "deep")    ! rule 5  vs sink 10 -> max 10 -> written
        call lg%debug("debug-other", name = "other")   ! logger 20 vs sink 10 -> max 20 -> dropped
        call lg%info("info-other",   name = "other")   ! logger 20 vs sink 10 -> max 20 -> written
        call lg%close()

        call read_back(path, lines, n)
        call check(error, n == 2, "exactly two of the four records compose through to the sink")
        if (allocated(error)) return
        ! The sink blocked a record the name rule admitted: the half a logger-only reading misses.
        call check(error, .not. has(lines(1), "trace-deep") .and. .not. has(lines(2), "trace-deep"), &
            "the sink did not block a TRACE record its logger-side rule admitted")
        if (allocated(error)) return
        ! The logger blocked a record the sink would have taken: the half a sink-only reading misses.
        call check(error, .not. has(lines(1), "debug-other") .and. .not. has(lines(2), "debug-other"), &
            "the logger did not block a DEBUG record the sink would have accepted")
        if (allocated(error)) return
        ! NEGATIVE CONTROL: the two that must survive, in order.
        call check(error, has(lines(1), "debug-deep"), &
            "the record the rule lowered and the sink accepted was not written first")
        if (allocated(error)) return
        call check(error, has(lines(2), "info-other"), &
            "the record at the logger's own level was not written second")
    end subroutine test_threshold_composition

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

    !> A sink that does not colour strips a caller's OWN codes out of the message, the context and
    !> the name -- and one that does colour keeps them, which is the negative control the whole test
    !> rests on: a renderer that stripped unconditionally would satisfy every positive assertion
    !> here. The level tag's own colour is asserted on both arms, so a strip reaching the layout as
    !> well as the caller's data fails rather than passing quietly.
    subroutine test_strip_color_from_uncolored_sinks(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_strip_color.txt"
        character(len=512), allocatable :: lines(:)
        character(len=1), parameter :: esc = achar(27)
        type(pf_logger) :: lg
        character(len=:), allocatable :: hot
        integer :: n, sk

        call pf_log_color("42 rows", PF_LOG_C_RED, hot)

        call lg%init(console = .false.)
        call lg%add_file(path, append = .false., format = "{level}|{name|}|{context|}|{message}", &
            sink = sk)
        call lg%warning("dropped " // hot)                                        ! 1  AUTO: stripped
        call lg%warning("named", name = "a" // hot // "b")                        ! 2  name too
        call lg%warning("ctx", context = "in" // hot)                             ! 3  context too
        call lg%warning("cursor" // esc // "[2Ktail")                             ! 4  any CSI, not just SGR
        call lg%warning("cut" // esc // "[31")                                    ! 5  unterminated: tail goes
        call lg%warning("bare" // esc)                                            ! 6  a lone trailing ESC
        call lg%warning("mid" // esc // "end")                                    ! 7  a lone ESC mid-text
        call lg%set_color(PF_LOG_COLOR_ALWAYS, sink = sk)
        call lg%warning("kept " // hot)                                           ! 8  ALWAYS: preserved
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 8, "all eight records reached the file")
        if (allocated(error)) return
        call check(error, .not. has(lines(1), esc), "a file sink under AUTO strips the caller's own codes")
        if (allocated(error)) return
        call check(error, has(lines(1), "dropped 42 rows"), "stripping leaves the message text itself intact")
        if (allocated(error)) return
        call check(error, .not. has(lines(2), esc) .and. has(lines(2), "|a42 rowsb|"), &
            "a coloured {name} is stripped and its text kept")
        if (allocated(error)) return
        call check(error, .not. has(lines(3), esc) .and. has(lines(3), "|in42 rows|"), &
            "a coloured {context} is stripped and its text kept")
        if (allocated(error)) return
        call check(error, .not. has(lines(4), esc) .and. has(lines(4), "cursortail"), &
            "a non-SGR CSI sequence is stripped too")
        if (allocated(error)) return
        call check(error, .not. has(lines(5), esc) .and. has(lines(5), "|cut") .and. &
            .not. has(lines(5), "31"), "an unterminated CSI takes the rest of the text with it")
        if (allocated(error)) return
        call check(error, .not. has(lines(6), esc) .and. has(lines(6), "|bare"), &
            "a trailing lone ESC is dropped")
        if (allocated(error)) return
        call check(error, .not. has(lines(7), esc) .and. has(lines(7), "|midend"), &
            "a lone ESC mid-text is dropped without taking the text after it")
        if (allocated(error)) return

        ! The control: the same message on the same sink, colouring, keeps every byte.
        call check(error, has(lines(8), hot), "PF_LOG_COLOR_ALWAYS keeps the caller's own codes")
        if (allocated(error)) return
        call check(error, has(lines(8), esc // "[" // PF_LOG_C_YELLOW // "m"), &
            "the level tag is still coloured on the colouring arm")
        if (allocated(error)) return
        call check(error, .not. has(lines(1), esc // "[" // PF_LOG_C_YELLOW // "m"), &
            "the level tag is uncoloured on the stripping arm, as AUTO already required")
    end subroutine test_strip_color_from_uncolored_sinks

    !> Stripping happens while the line is RENDERED, so the length `emit_to_sink` measures and the
    !> bytes it writes are produced by one walk and cannot disagree. Both sides of the
    !> `PF_LOG_MAX_LINE` fork are exercised, since the measuring pass is what chooses between the
    !> fixed buffer and the allocatable fallback: a strip applied after rendering would size the
    !> fallback from the unstripped length and leave the line trailing blanks, and a strip whose
    !> measuring and writing arms disagreed would truncate it. The assertion is on the exact byte
    !> count -- "no ESC in the file" alone passes against both faults.
    subroutine test_strip_color_line_length(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_strip_long.txt"
        character(len=1), parameter :: esc = achar(27)
        character(len=:), allocatable :: over, under
        character(len=8192) :: buf
        type(pf_logger) :: lg
        integer :: u, ios, i, n1, n2

        ! 100 five-byte SGR codes among 4000 payload bytes: 4500 raw, 4000 once stripped, so this
        ! record crosses PF_LOG_MAX_LINE before stripping and sits under it after.
        under = ""
        do i = 1, 100
            under = under // esc // "[31m" // repeat("x", 40)
        end do
        ! 5000 payload bytes: over PF_LOG_MAX_LINE even stripped, so it takes the allocatable path.
        over = ""
        do i = 1, 100
            over = over // esc // "[32m" // repeat("y", 50)
        end do

        call lg%init(console = .false.)
        call lg%add_file(path, append = .false., format = "{message}")
        call lg%warning(under)
        call lg%warning(over)
        call lg%close()

        n1 = -1
        n2 = -1
        open (newunit = u, file = path, action = "read", status = "old", iostat = ios)
        call check(error, ios == 0, "the long-line fixture file was written")
        if (allocated(error)) return
        read (u, '(a)', iostat = ios) buf
        if (ios == 0) n1 = len_trim(buf)
        read (u, '(a)', iostat = ios) buf
        if (ios == 0) n2 = len_trim(buf)
        close (u)

        call check(error, n1 == 4000, "a record over PF_LOG_MAX_LINE only before stripping " // &
            "is written at its stripped length exactly")
        if (allocated(error)) return
        call check(error, n2 == 5000, "a record over PF_LOG_MAX_LINE after stripping too keeps " // &
            "every byte through the allocatable fallback")
    end subroutine test_strip_color_line_length

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

    !> A file sink flushes at `flush_level` and above, and a flush pushes everything buffered on
    !> the unit -- so a WARNING record makes the quieter records before it visible too.
    !!
    !! Only positive assertions: "record visible on disk without %flush/close" is deterministic
    !! (the flush forces it), while "record NOT yet on disk" depends on the runtime's buffer size
    !! and write-through policy and would be a flaky test on some compiler. The never-flush arm is
    !! therefore asserted through %flush recovering the records, not through their prior absence.
    !!
    !! **The flush is observed with INQUIRE, never by reading the file back while the sink still
    !! holds it open.** F2018 12.5.1 forbids connecting one file to two units at once, so an
    !! `open(..., status="old")` on a path a sink is writing is non-conforming -- gfortran, ifx and
    !! nagfor allow it as an extension and flang refuses it outright, with `iostat = 1037`, which
    !! `read_back` reports as "no lines" and the assertion then blames the library for. Enquiring
    !! about a file BY NAME is allowed whether or not it is connected, so `size=` gives the same
    !! observation conformingly: it moves only as bytes actually reach the file.
    !!
    !! **It is exactly as sensitive as the read-back was -- no more.** Measured, because the
    !! temptation is to claim the size check is stronger and it is not: with the `flush` statement
    !! removed from `deliver`, `size` while open reads 0 under nagfor and this test fails, while
    !! under gfortran and flang it reads the full record and the test passes. Those two runtimes
    !! write each record straight through, so NO instrument can observe the flush there -- the
    !! read-back could not either. So the flush property is asserted on whichever runtime buffers,
    !! and is vacuous on the ones that do not; that is a property of the runtimes, not of the
    !! assertion, and it is the same caveat the paragraph above already makes about absence.
    subroutine test_flush_level(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: pa = "test_run/log_flush_all.txt"
        character(len=*), parameter :: pw = "test_run/log_flush_warn.txt"
        character(len=*), parameter :: pn = "test_run/log_flush_never.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n
        integer :: sz_open, sz_closed

        ! flush_level = ALL: every record visible immediately.
        call lg%init(console = .false.)
        call lg%add_file(pa, append = .false., format = "{message}", flush_level = PF_LEVEL_ALL)
        call lg%info("immediately visible")
        inquire (file = pa, size = sz_open)
        call lg%close()
        inquire (file = pa, size = sz_closed)
        call check(error, sz_open > 0 .and. sz_open == sz_closed, &
            "flush_level = PF_LEVEL_ALL flushes every record")
        if (allocated(error)) return
        call read_back(pa, lines, n)
        call check(error, n == 1 .and. has(lines(1), "immediately visible"), &
            "and the record it flushed is the one that was logged")
        if (allocated(error)) return

        ! The default (WARNING): a WARNING record pushes itself AND the buffered INFO before it.
        call lg%init(console = .false.)
        call lg%add_file(pw, append = .false., format = "{message}")
        call lg%info("quieter, buffered")
        call lg%warning("trouble, flushes")
        inquire (file = pw, size = sz_open)
        call lg%close()
        inquire (file = pw, size = sz_closed)
        call check(error, sz_open > 0 .and. sz_open == sz_closed, &
            "a WARNING record's flush pushes the buffered INFO before it")
        if (allocated(error)) return
        call read_back(pw, lines, n)
        call check(error, n == 2, "and both records are on disk")
        if (allocated(error)) return
        call check(error, has(lines(1), "quieter") .and. has(lines(2), "trouble"), &
            "and the records arrive in order")
        if (allocated(error)) return

        ! flush_level = OFF: nothing flushes per record; %flush recovers everything.
        call lg%init(console = .false.)
        call lg%add_file(pn, append = .false., format = "{message}", flush_level = PF_LEVEL_OFF)
        call lg%critical("even this does not flush")
        call lg%flush()
        inquire (file = pn, size = sz_open)
        call lg%close()
        inquire (file = pn, size = sz_closed)
        call check(error, sz_open > 0 .and. sz_open == sz_closed, &
            "flush_level = PF_LEVEL_OFF still delivers through an explicit %flush")
        if (allocated(error)) return
        call read_back(pn, lines, n)
        call check(error, n == 1 .and. has(lines(1), "even this"), &
            "and what %flush delivered is the record that was logged")
    end subroutine test_flush_level

    !> The clock fields are gathered only when some attached sink's template renders them, the
    !> needs are OR-ed across sinks, and `set_format` refreshes the cache.
    !!
    !! Correctness across the mask cannot be asserted by absence (an ungathered field is simply
    !! never rendered), so the assertions are the two directions that CAN fail: a sink that needs
    !! the clock still gets a correct timestamp while sharing the logger with one that does not;
    !! and a template changed AFTER the sink was added still gets one -- which is the assertion
    !! that fails if set_format forgets to recompute the logger's cached OR.
    subroutine test_clock_mask_across_sinks(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: pmsg = "test_run/log_mask_msg.txt"
        character(len=*), parameter :: pclk = "test_run/log_mask_clk.txt"
        character(len=*), parameter :: pfmt = "test_run/log_mask_fmt.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, vb(8), va(8)
        character(len=512) :: d

        call date_and_time(values = vb)
        call lg%init(console = .false.)
        call lg%add_file(pmsg, append = .false., format = "{message}")
        call lg%add_file(pclk, append = .false., format = "D={date}|{message}")
        call lg%info("shared record")
        call lg%close()
        call date_and_time(values = va)

        call read_back(pmsg, lines, n)
        call check(error, n == 1 .and. trim(lines(1)) == "shared record", &
            "the clock-free sink renders the bare message")
        if (allocated(error)) return
        call read_back(pclk, lines, n)
        call check(error, n == 1, "the clock-carrying sibling took the same record")
        if (allocated(error)) return
        call field_between(lines(1), "D=", "|", d)
        ! The rendered date must equal the real date -- taken before AND after, so a run crossing
        ! midnight cannot fail spuriously. This is a VALUE assertion, not a shape one: a
        ! hand-formatting defect that swapped fields or dropped a digit passes any shape check.
        call check(error, date_matches(d, vb) .or. date_matches(d, va), &
            "{date} renders the actual date, got '" // trim(d) // "'")
        if (allocated(error)) return

        ! A template installed by set_format AFTER add_file must still be served with the clock.
        call lg%init(console = .false.)
        call lg%add_file(pfmt, append = .false., format = "{message}")
        call lg%set_format("T={time}|{message}")
        call lg%info("late template")
        call lg%close()
        call read_back(pfmt, lines, n)
        call check(error, n == 1, "the record reached the reformatted sink")
        if (allocated(error)) return
        call field_between(lines(1), "T=", "|", d)
        call check(error, len_trim(d) == 12 .and. all_digits(d(1:2)) .and. d(3:3) == ":", &
            "set_format after add_file still gathers the clock, got '" // trim(d) // "'")
    end subroutine test_clock_mask_across_sinks

    !> Whether rendered text `d` equals the date in a `date_and_time` values array.
    logical function date_matches(d, v) result(yes)
        character(len=*), intent(in) :: d  !! Rendered `YYYY-MM-DD`.
        integer, intent(in) :: v(8)        !! `date_and_time` values.
        character(len=10) :: expect

        write (expect, '(i4.4,"-",i2.2,"-",i2.2)') v(1), v(2), v(3)
        yes = trim(d) == expect
    end function date_matches

    !> `%print` names every piece of the configuration, and `pf_log_print` says plainly when the
    !> default logger is still implicit.
    subroutine test_print_config(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_print.txt"
        character(len=*), parameter :: sink_file = "test_run/log_print_sink.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: u, n, i
        logical :: seen_level, seen_rule, seen_path, seen_flush, seen_fmt, seen_rank

        call lg%init(level = PF_LEVEL_DEBUG, console = .false., name = "app")
        call lg%set_rank(3)
        call lg%add_file(sink_file, append = .false., format = "{level}|{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "deep")

        call open_scratch(path, u)
        call lg%print(unit = u)
        close (u)
        call lg%close()
        call read_back(path, lines, n)

        seen_level = .false.; seen_rule = .false.; seen_path = .false.
        seen_flush = .false.; seen_fmt = .false.; seen_rank = .false.
        do i = 1, n
            if (has(lines(i), "level") .and. has(lines(i), "DEBUG")) seen_level = .true.
            if (has(lines(i), "deep") .and. has(lines(i), "TRACE")) seen_rule = .true.
            if (has(lines(i), sink_file)) seen_path = .true.
            if (has(lines(i), "flush at") .and. has(lines(i), "WARNING")) seen_flush = .true.
            if (has(lines(i), "{level}|{message}")) seen_fmt = .true.
            if (has(lines(i), "rank") .and. has(lines(i), "3")) seen_rank = .true.
        end do
        call check(error, seen_level, "%print names the logger's level")
        if (allocated(error)) return
        call check(error, seen_rule, "%print names the per-name override and its level")
        if (allocated(error)) return
        call check(error, seen_path, "%print names the file sink's path")
        if (allocated(error)) return
        call check(error, seen_flush, "%print shows the sink's flush level, WARNING by default")
        if (allocated(error)) return
        call check(error, seen_fmt, "%print shows the sink's format template")
        if (allocated(error)) return
        call check(error, seen_rank, "%print shows the logger's rank")
        if (allocated(error)) return

        ! The unconfigured default logger describes its implicit console rather than dumping the
        ! (unconfigured) structure. pf_log_close leaves the default CONFIGURED-and-silent, so this
        ! arm can only run meaningfully when nothing in this test configured the default -- it did
        ! not -- but a sibling test may have, so assert only the shape that holds either way: the
        ! dump opens with the same banner.
        call open_scratch(path, u)
        call pf_log_print(unit = u)
        close (u)
        call read_back(path, lines, n)
        call check(error, n >= 1 .and. has(lines(1), "pf_logger configuration"), &
            "pf_log_print writes the configuration banner")
    end subroutine test_print_config

    !> The name stack composes onto the logger's own name, nests, and pops one frame at a time.
    !!
    !! The negative controls are what make this more than a string test: a record emitted after
    !! the pops must be back to the bare logger name (a stack that never truly popped would still
    !! pass every positive assertion), and an explicit per-call `name=` must beat the stack
    !! outright rather than composing with it.
    subroutine test_name_stack(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_name_stack.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, f1, f2, d0, d1, d2

        call pf_log_clear_names()
        call lg%init(console = .false., name = "prog")
        call lg%add_file(path, append = .false., format = "{name}|{message}")

        d0 = pf_log_name_depth()
        call lg%info("bare")
        call pf_log_push_name("io", f1)
        d1 = pf_log_name_depth()
        call lg%info("one frame")
        call pf_log_push_name("stat", f2)
        d2 = pf_log_name_depth()
        call lg%info("two frames")
        call lg%info("explicit wins", name = "other")
        call pf_log_pop_name(f2)
        call lg%info("back to one")
        call pf_log_pop_name(f1)
        call lg%info("back to bare")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, d0 == 0 .and. d1 == 1 .and. d2 == 2, &
            "the depth counts the frames pushed")
        if (allocated(error)) return
        call check(error, f1 == 1 .and. f2 == 2, "each push reports its own depth as the token")
        if (allocated(error)) return
        call check(error, n == 6, "all six records reached the file")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "prog|bare", "no frames means the logger's own name")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "prog.io|one frame", "one frame composes onto the base")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "prog.io.stat|two frames", "frames nest, dot-joined")
        if (allocated(error)) return
        call check(error, trim(lines(4)) == "other|explicit wins", &
            "an explicit per-call name= replaces the stack rather than composing with it")
        if (allocated(error)) return
        call check(error, trim(lines(5)) == "prog.io|back to one", "a pop removes exactly one frame")
        if (allocated(error)) return
        call check(error, trim(lines(6)) == "prog|back to bare", &
            "popping every frame returns the bare logger name")
    end subroutine test_name_stack

    !> The composed name is what a per-name level override matches, which is the whole point of
    !> the feature: a subprogram names itself and the application tunes it by that name.
    subroutine test_name_stack_drives_overrides(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_name_stack_level.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n
        logical :: on_in, on_out

        call pf_log_clear_names()
        call lg%init(level = PF_LEVEL_INFO, console = .false., name = "prog")
        call lg%add_file(path, append = .false., format = "{name}|{message}")
        call lg%set_level(PF_LEVEL_TRACE, name = "prog.io")   ! turn just this subtree up

        call lg%trace("outside the frame")                    ! control: "prog", no rule -> dropped
        on_out = lg%enabled(PF_LEVEL_TRACE)
        call pf_log_push_name("io")
        call lg%trace("inside the frame")                     ! "prog.io" -> the rule admits it
        on_in = lg%enabled(PF_LEVEL_TRACE, name = "prog.io")
        call pf_log_pop_name()
        call lg%trace("outside again")                        ! control again
        call lg%close()
        call read_back(path, lines, n)

        call check(error, n == 1, "only the record emitted inside the frame got through")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "prog.io|inside the frame", &
            "the pushed frame is what the per-name override matched")
        if (allocated(error)) return
        call check(error, on_in, "enabled agrees for the composed name")
        if (allocated(error)) return
        ! The nameless %enabled answers .true. here while the record it was asked about is
        ! dropped -- which is the documented conservative contract, not a defect: it cannot see a
        ! name, so with any lowering rule in the table it must assume some name might pass. The
        ! record count above is what proves the record really was dropped. Pass a name for the
        ! exact answer, as the assertion above does.
        call check(error, on_out, &
            "%enabled without a name stays conservative rather than consulting the name stack")
    end subroutine test_name_stack_drives_overrides

    !> `clear_names` is a boundary reset from any depth, and a pop on an empty stack is a no-op.
    subroutine test_name_stack_clear_and_empty_pop(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_name_clear.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n

        call pf_log_clear_names()
        call lg%init(console = .false., name = "prog")
        call lg%add_file(path, append = .false., format = "{name}|{message}")

        call pf_log_push_name("a")
        call pf_log_push_name("b")
        call pf_log_push_name("c")
        call pf_log_clear_names()
        call check(error, pf_log_name_depth() == 0, "clear_names empties the stack from any depth")
        if (allocated(error)) return
        call pf_log_pop_name()          ! must be a no-op, not an abort or an underflow
        call check(error, pf_log_name_depth() == 0, "a pop on an empty stack leaves it empty")
        if (allocated(error)) return
        call lg%info("after clearing")
        call lg%close()
        call read_back(path, lines, n)
        call check(error, n == 1 .and. trim(lines(1)) == "prog|after clearing", &
            "and the logger is back to its own bare name")
    end subroutine test_name_stack_clear_and_empty_pop

    !> Each thread owns its own name frames: one thread's push is invisible to every other.
    !!
    !! Without OpenMP both arms run on the initial thread and the assertion would hold because
    !! there is only one stack, not because the stack is per-thread -- vacuous, so it skips.
    subroutine test_name_stack_is_per_thread(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_name_threads.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        integer :: n, i, nbare, ndeep
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread the per-thread stack cannot be " // &
            "distinguished from a single shared one, so the assertion would hold for the wrong reason")
        return
#endif
        if (.not. can_thread()) then
            call skip_test(error, "needs more than one thread available")
            return
        end if

        call pf_log_clear_names()
        call lg%init(console = .false., name = "prog")
        call lg%add_file(path, append = .false., format = "{name}", flush_level = PF_LEVEL_ALL)

        !$omp parallel do default(shared) private(i)
        do i = 1, 32
            if (mod(i, 2) == 0) then
                call pf_log_push_name("deep")
                call lg%info("x")
                call pf_log_pop_name()
            else
                call lg%info("x")
            end if
        end do
        !$omp end parallel do

        call lg%close()
        call read_back(path, lines, n)
        nbare = 0
        ndeep = 0
        do i = 1, n
            if (trim(lines(i)) == "prog") nbare = nbare + 1
            if (trim(lines(i)) == "prog.deep") ndeep = ndeep + 1
        end do
        call check(error, n == 32, "every iteration emitted exactly one record")
        if (allocated(error)) return
        ! The counts are exact, not merely non-zero: a stack shared between threads would let one
        ! thread's frame leak into another's record and the split would drift from 16/16.
        call check(error, nbare == 16 .and. ndeep == 16, &
            "each thread's frames are its own, so the split is exactly the one the loop wrote")
    end subroutine test_name_stack_is_per_thread

    !> `%get_name` reports the logger's own base, not the composed name.
    subroutine test_get_name(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        type(pf_logger) :: lg
        character(len=PF_LOG_MAX_NAME) :: got, got_default

        call pf_log_clear_names()
        call lg%init(console = .false., name = "prog")
        call lg%get_name(got)
        call check(error, trim(got) == "prog", "get_name reports what set_name set")
        if (allocated(error)) return

        call pf_log_push_name("io")
        call lg%get_name(got)
        call pf_log_pop_name()
        call check(error, trim(got) == "prog", &
            "get_name reports the BASE, unaffected by this thread's pushed frames")
        if (allocated(error)) return

        call pf_log_init(console = .false.)
        call pf_log_set_name("shim")
        call pf_log_get_name(got_default)
        call pf_log_close()
        call check(error, trim(got_default) == "shim", "pf_log_get_name reads the default logger")
    end subroutine test_get_name

    !> `%get_full_name` reports the composed name, and reports exactly what `{name}` renders.
    subroutine test_get_full_name(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_full_name.txt"
        type(pf_logger) :: lg
        character(len=512), allocatable :: lines(:)
        character(len=PF_LOG_MAX_NAME) :: bare, one, two, popped, cleared, anon, shim
        integer :: n

        call pf_log_clear_names()
        call lg%init(console = .false., name = "prog")
        call lg%add_file(path, append = .false., format = "{name}")

        call lg%get_full_name(bare)
        call lg%info("x")
        call pf_log_push_name("io")
        call lg%get_full_name(one)
        call lg%info("x")
        call pf_log_push_name("stat")
        call lg%get_full_name(two)
        call lg%info("x")
        call pf_log_pop_name()
        call lg%get_full_name(popped)
        call lg%info("x")
        call pf_log_clear_names()
        call lg%get_full_name(cleared)
        call lg%info("x")
        call lg%close()
        call read_back(path, lines, n)

        call check(error, trim(bare) == "prog", "an empty stack composes to the logger's own name")
        if (allocated(error)) return
        call check(error, trim(one) == "prog.io", "one frame composes onto the base")
        if (allocated(error)) return
        call check(error, trim(two) == "prog.io.stat", "frames nest, dot-joined")
        if (allocated(error)) return
        call check(error, trim(popped) == "prog.io", "a pop is reflected immediately")
        if (allocated(error)) return
        call check(error, trim(cleared) == "prog", "clear_names returns the bare name")
        if (allocated(error)) return

        ! The tie-down, and the reason the composition rule lives in ONE procedure: a query that
        ! disagreed with what the record actually carries would be worse than no query at all,
        ! since the composed name is what a per-name override is matched against.
        call check(error, n == 5, "all five records reached the file")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == trim(bare) .and. trim(lines(2)) == trim(one) .and. &
            trim(lines(3)) == trim(two) .and. trim(lines(4)) == trim(popped) .and. &
            trim(lines(5)) == trim(cleared), &
            "get_full_name reports exactly the name {name} renders, at every depth")
        if (allocated(error)) return

        ! A logger with no name of its own is the case where the composition has no base to join
        ! onto, so the frames stand alone with no leading dot.
        call lg%init(console = .false.)
        call pf_log_push_name("io")
        call lg%get_full_name(anon)
        call pf_log_clear_names()
        call check(error, trim(anon) == "io", &
            "a nameless logger composes to the frames alone, with no leading separator")
        if (allocated(error)) return

        call pf_log_init(console = .false.)
        call pf_log_set_name("shimmed")
        call pf_log_push_name("io")
        call pf_log_get_full_name(shim)
        call pf_log_clear_names()
        call pf_log_close()
        call check(error, trim(shim) == "shimmed.io", &
            "pf_log_get_full_name reads the default logger")
    end subroutine test_get_full_name

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
            new_unittest("a caller-owned unit is written to and not closed", test_add_unit_sink), &
            new_unittest("date, time, stamp and elapsed render in shape", test_layout_clock_fields), &
            new_unittest("thread renders, and an unset rank collapses", test_layout_thread_and_rank), &
            new_unittest("colour policy is honoured in all three arms", test_color_policies), &
            new_unittest("an uncoloured sink strips the caller's own codes", &
                test_strip_color_from_uncolored_sinks), &
            new_unittest("stripping keeps the rendered length exact", test_strip_color_line_length), &
            new_unittest("a blank line obeys a sink's rank filter", test_blank_obeys_rank_filter), &
            new_unittest("the built-in templates render and PF_LEVEL_OFF admits nothing", &
                test_builtin_templates_and_off), &
            new_unittest("flush_level flushes at and above, and a flush carries earlier records", &
                test_flush_level), &
            new_unittest("clock fields follow the sinks' templates, set_format included", &
                test_clock_mask_across_sinks), &
            new_unittest("print dumps the whole configuration", test_print_config), &
            new_unittest("the name stack composes, nests and pops", test_name_stack), &
            new_unittest("the composed name drives per-name overrides", &
                test_name_stack_drives_overrides), &
            new_unittest("clear_names resets and an empty pop is a no-op", &
                test_name_stack_clear_and_empty_pop), &
            new_unittest("name frames are per-thread", test_name_stack_is_per_thread), &
            new_unittest("get_name reports the base name", test_get_name), &
            new_unittest("get_full_name reports the composed name", test_get_full_name), &
            new_unittest("logger, sink and name rule compose with max, all three binding at once", &
                test_threshold_composition), &
            new_unittest("pf_log_now renders the wall clock in the shape the record does", &
                test_log_now) &
            ]
    end subroutine collect_tests_logging

    !> `pf_log_now` renders `YYYY-MM-DD` and `HH:MM:SS`, with the separators in the right places
    !! and every field taken from the field it names.
    !!
    !! **The fields are checked against `date_and_time`'s own numbers, not merely for shape.** The
    !! mistake this procedure exists to prevent is indexing the wrong source string -- an hour
    !! followed by two digits of the *year* -- which produces text of exactly the right shape, with
    !! digits in every position and colons where colons belong. A test asserting only the shape
    !! would pass on it.
    !!
    !! The clock can tick between the two calls, so only the fields that cannot have changed are
    !! compared: the date, and the hour. Minutes and seconds are checked for range instead, which
    !! catches a wrong source field (a year's `26` is a legal minute, but a month's `09` in the
    !! seconds slot alongside a mismatched minute is not something a real clock produces twice).
    subroutine test_log_now(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        character(len=:), allocatable :: d, t, t_ms
        integer :: v(8), yyyy, mm, dd, hh, mi, ss

        call pf_log_now(d, t)
        call date_and_time(values = v)

        call check(error, len(d), 10, "the date must be exactly YYYY-MM-DD")
        if (allocated(error)) return
        call check(error, len(t), 8, "the default time must be exactly HH:MM:SS")
        if (allocated(error)) return
        call check(error, d(5:5) == "-" .and. d(8:8) == "-", "the date separators must be hyphens")
        if (allocated(error)) return
        call check(error, t(3:3) == ":" .and. t(6:6) == ":", "the time separators must be colons")
        if (allocated(error)) return

        read (d(1:4), '(i4)') yyyy
        read (d(6:7), '(i2)') mm
        read (d(9:10), '(i2)') dd
        read (t(1:2), '(i2)') hh
        read (t(4:5), '(i2)') mi
        read (t(7:8), '(i2)') ss

        call check(error, yyyy, v(1), "the year is not date_and_time's year")
        if (allocated(error)) return
        call check(error, mm, v(2), "the month is not date_and_time's month")
        if (allocated(error)) return
        call check(error, dd, v(3), "the day is not date_and_time's day")
        if (allocated(error)) return
        call check(error, hh, v(5), "the hour is not date_and_time's hour")
        if (allocated(error)) return
        ! Minutes and seconds may have advanced between the two calls, so these are range
        ! assertions -- which is still enough to catch a field read out of the date string.
        call check(error, mi >= 0 .and. mi <= 59, "the minutes field is not a minute")
        if (allocated(error)) return
        call check(error, ss >= 0 .and. ss <= 60, "the seconds field is not a second")
        if (allocated(error)) return

        ! The milliseconds form extends the same text rather than re-rendering it differently.
        call pf_log_now(d, t_ms, millis=.true.)
        call check(error, len(t_ms), 12, "the millis form must be exactly HH:MM:SS.mmm")
        if (allocated(error)) return
        call check(error, t_ms(9:9) == ".", "the millis separator must be a full stop")
        if (allocated(error)) return
        call check(error, t_ms(3:3) == ":" .and. t_ms(6:6) == ":", &
            "the millis form must keep the same colons")
    end subroutine test_log_now

end module test_logging
