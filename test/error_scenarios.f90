!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Standalone helper program that deliberately triggers `error stop` paths in
!> the parquet module. It is invoked as a subprocess (via `fpm test
!> error_scenarios -- <scenario>`) from test_errors.f90, because a Fortran
!> `error stop` aborts the whole process and cannot be caught in-process by
!> test-drive. The exit code (0 = no error stop reached, nonzero = aborted)
!> is the observable result.
!>
!> The scenarios themselves live in four `error_scenarios_*` modules, each with its own
!> `select case`; this program only walks them in turn. See `error_scenarios_io.f90` for
!> why they are apart (the single-program form was the project's longest compile), and
!> `error_scenarios_support.f90` for the fixtures more than one group needs.
program error_scenarios
    use error_scenarios_io, only : dispatch_error_scenarios_io
    use error_scenarios_table, only : dispatch_error_scenarios_table
    use error_scenarios_analysis, only : dispatch_error_scenarios_analysis
    use error_scenarios_numeric, only : dispatch_error_scenarios_numeric
    implicit none

    character(len=64) :: scenario
    integer :: nargs
    !> Set by the first group module that recognizes the name; still .false. after all
    !! four have been asked means there is no such scenario.
    logical :: handled

    nargs = command_argument_count()
    if (nargs < 1) then
        ! No scenario requested: this happens when `fpm test` auto-runs every
        ! test target with no arguments. Exit cleanly and silently rather than
        ! failing, since this program is only meant to be driven (with an
        ! explicit scenario) as a subprocess from test_errors.f90.
        stop
    end if
    call get_command_argument(1, scenario)

    select case (trim(scenario))
    case ("ok")
        ! The control scenario: reaches no library call at all, so a nonzero exit from
        ! this one means the harness is broken rather than the library.
        handled = .true.
    case default
        handled = .false.
    end select
    if (.not. handled) call dispatch_error_scenarios_io(scenario, handled)
    if (.not. handled) call dispatch_error_scenarios_table(scenario, handled)
    if (.not. handled) call dispatch_error_scenarios_analysis(scenario, handled)
    if (.not. handled) call dispatch_error_scenarios_numeric(scenario, handled)
    if (.not. handled) then
        ! Deliberately a distinctive, otherwise-unused exit code (not 0, and
        ! not the plain 1 that `error stop "message"` produces) -- callers
        ! checking exit status can tell "the scenario name doesn't exist
        ! (typo?)" apart from "the scenario ran and genuinely aborted",
        ! which a plain `stop 1` here could not be told apart from.
        print '(a)', "unknown scenario: "//trim(scenario)
        stop 97
    end if

end program error_scenarios
