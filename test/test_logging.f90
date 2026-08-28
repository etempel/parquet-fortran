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
            new_unittest("a sink's rank filter selects one rank", test_rank_filter), &
            new_unittest("an over-long line is emitted whole", test_long_line_is_not_truncated), &
            new_unittest("blank lines reach file sinks", test_blank_reaches_file_sinks), &
            new_unittest("pf_str renders every kind", test_pf_str), &
            new_unittest("level names convert both ways", test_level_names), &
            new_unittest("a non-standard integer level is accepted", test_nonstandard_level_is_accepted), &
            new_unittest("buffered mode recovers every thread's records", test_buffered_mode_recovers_every_thread), &
            new_unittest("an oversize buffered record is written whole", test_buffered_oversize_record), &
            new_unittest("concurrent once= emits exactly one line", test_concurrent_once), &
            new_unittest("a shared logger survives a parallel region", test_shared_logger_in_parallel_region) &
            ]
    end subroutine collect_tests_logging

end module test_logging
