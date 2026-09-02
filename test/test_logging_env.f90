!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The one `parquet_logging` test that declares a `bind(C)` interface, split out of
!> `test_logging.f90`.
!!
!! **It is here for the interface, not for Arrow.** `pf_log_configure_from_env` has to be given an
!! environment to read, Fortran cannot set one, and the POSIX `setenv`/`unsetenv` shims below are
!! `bind(C)` calls like any other. A test file declaring one cannot feed an undef-safe runner --
!! nagfor's `-C=undefined` corrupts every argument after the first of a call into C, and the rule
!! is file-level because a per-test rule is not statically decidable (feature_tests.md §5). So
!! this file joins `run_tester_cpp` even though it reaches no C++ of this library's own, and the
!! other 43 logging tests stay in the undef-safe `run_tester_pf`.
!!
!! Registered as the suite `logging_env`; the Arrow-free remainder stays as `logging`. Both are
!! excluded from test-drive's per-test parallelism, since they share one process-global logger.
module test_logging_env
    use parquet_logging
    use test_logging, only : truncate_file, read_back, has
    use iso_fortran_env, only : int32, int64
    use iso_c_binding, only : c_char, c_int, c_null_char
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_logging_env
    !
contains

    !> Registers this suite's single test.
    subroutine collect_tests_logging_env(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("pf_log_configure_from_env applies what is set", test_configure_from_env), &
            new_unittest("the environment's COLOR reaches the file sink the environment added", &
                         test_env_color_reaches_the_file_sink) &
            ]
    end subroutine collect_tests_logging_env

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

    !> `<prefix>COLOR` reaches the sink `<prefix>FILE` adds in the same call.
    !>
    !> **This is an ordering test wearing a colour test's clothes.** `pf_log_set_color` with no
    !> `sink=` applies to every sink that exists WHEN IT RUNS, so a `COLOR` read before `FILE`
    !> cannot reach the file sink `FILE` is about to add. That is exactly what
    !> `pf_log_configure_from_env` did until 2026-09-02: `PF_LOG_COLOR=always` with
    !> `PF_LOG_FILE=run.log` left `run.log` with no colour at all, silently. See
    !> `feature_logging_env_order.md`.
    !>
    !> **The negative control is the second half**, and without it this test passes against a
    !> library that colours every file sink unconditionally: the same fixture with `COLOR` unset
    !> must write a file with NO escape bytes, since a file sink under the default `AUTO` never
    !> colours. Together the two halves say the policy travelled from the environment to that sink,
    !> rather than the sink having been coloured all along.
    !>
    !> It has its own scratch path, since suites run concurrently and two tests sharing a fixture
    !> path is a documented source of intermittent failure.
    subroutine test_env_color_reaches_the_file_sink(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle.
        character(len=*), parameter :: path = "test_run/log_env_color.txt"
        integer :: n_with, n_without

        call count_escapes_after_env("always", path, n_with)
        call check(error, n_with > 0, &
            "PFLOGTEST_COLOR=always did not reach the sink PFLOGTEST_FILE added")
        if (allocated(error)) return

        ! NEGATIVE CONTROL: unset, the same file sink must come back with no escape bytes at all.
        call count_escapes_after_env("", path, n_without)
        call check(error, n_without, 0, &
            "a file sink wrote escape bytes with PFLOGTEST_COLOR unset")
        if (allocated(error)) return

        call check(error, n_with > n_without, &
            "the two arms did not differ, so this test observed nothing")
    end subroutine test_env_color_reaches_the_file_sink

    !> Configures the default logger from `PFLOGTEST_*`, emits one record, and counts the ANSI
    !> escape bytes the file sink received. `color` is the value for `PFLOGTEST_COLOR`; empty
    !> leaves that variable unset, which is the control arm.
    subroutine count_escapes_after_env(color, path, nesc)
        character(len=*), intent(in) :: color  !! Value for `PFLOGTEST_COLOR`, or "" to unset it.
        character(len=*), intent(in) :: path   !! Scratch file the sink writes to.
        integer, intent(out) :: nesc           !! Escape bytes found in the file.
        character(len=512), allocatable :: lines(:)
        integer :: n, i, k

        call pf_log_reset_dedup()
        call pf_log_init(console = .false.)
        call truncate_file(path)
        if (len_trim(color) == 0) then
            call unset_env("PFLOGTEST_COLOR")
        else
            call set_env("PFLOGTEST_COLOR", color)
        end if
        call set_env("PFLOGTEST_FILE", path)
        call pf_log_configure_from_env("PFLOGTEST_")
        call pf_log_error("colour probe")
        call pf_log_flush()
        call pf_log_close()
        call unset_env("PFLOGTEST_COLOR")
        call unset_env("PFLOGTEST_FILE")

        call read_back(path, lines, n)
        nesc = 0
        do i = 1, n
            do k = 1, len_trim(lines(i))
                if (iachar(lines(i)(k:k)) == 27) nesc = nesc + 1
            end do
        end do
    end subroutine count_escapes_after_env

end module test_logging_env
