!> Threading tests for `parquet_cosmology`: that one built object nobody writes may be evaluated
!> from a whole team at once, and that cosmologies built concurrently on different threads stay
!> independent of one another.
!!
!! **The claim under test is the module's own header sentence** -- "`%init` and `%clear` are the
!! only bindings that write an object; every other one is `pure`, so a built cosmology may be
!! evaluated from any number of threads at once and two objects share nothing" -- and it is a
!! claim about ABSENCE: no module variable that is not a `parameter`, no scratch state kept in an
!! object. Nothing in the source can be pointed at to prove that, so it is proved by running it,
!! with every concurrent answer held against an oracle that does not come from the concurrent pass.
!!
!! **A thread's own object lives in a slot of an array allocated before the region**, indexed by
!! the thread number. That is the shape the guide page tells a user to write: a `pf_cosmology`
!! holds `pf_interp_1d` components, which have allocatable components of their own, so an OpenMP
!! `private()` copy is not reliably initialised under gfortran, and ifx segfaults on one declared
!! in a `block` lexically inside the region (`fortran-gotchas.md`).
!!
!! **The two test-only hooks are module state and are deliberately untouched here.**
!! `parquet_debug_cosmology_neval` reports the LAST build's evaluation count, which is meaningless
!! when several builds run at once, and no test in this file reads it.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` excludes `cosmology_omp`):
!! test-drive dispatches a suite's tests inside its own `!$omp parallel do`, where a region opened
!! by a test would be nested and get a team of one. **Every test here carries the skip guard**,
!! because without OpenMP its assertions would hold for the wrong reason.
!!
!! Its only library import is `use parquet_cosmology`: it is registered in `run_tester_pf.f90`,
!! the runner that executes no `bind(C)` call.
module test_cosmology_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_cosmology
    use iso_fortran_env, only : real64
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads
#endif

    implicit none
    private

    public :: collect_tests_cosmology_omp

    !> Redshifts each test sweeps; enough work that a team really overlaps.
    integer, parameter :: N_POINTS = 4000

contains

    !> Registers this module's tests.
    subroutine collect_tests_cosmology_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("one object is evaluated by a whole team, bit for bit", &
                         test_one_object_is_evaluated_by_a_whole_team), &
            new_unittest("objects built on different threads stay independent", &
                         test_objects_built_on_different_threads_stay_independent), &
            new_unittest("the team these tests rely on is really opened", &
                         test_the_team_is_really_opened) &
            ]

    end subroutine collect_tests_cosmology_omp

    !> One built object, read by every thread at once, against the serial answers.
    subroutine test_one_object_is_evaluated_by_a_whole_team(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_cosmology) :: cosmo
        real(real64)       :: z(N_POINTS), serial(N_POINTS), threaded(N_POINTS)
        integer            :: i, nbad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the loop below runs on one thread, so " // &
            "an agreement between the two passes would say nothing about sharing")
        return
#endif
        do i = 1, N_POINTS
            z(i) = 1.0e-4_real64 * (1.0e4_real64 / 1.0e-4_real64) ** (real(i - 1, real64) &
                                                                     / real(N_POINTS - 1, real64))
        end do
        call cosmo%init("Planck18")
        do i = 1, N_POINTS
            serial(i) = cosmo%comoving_distance(z(i)) + cosmo%age(z(i)) &
                        + cosmo%lookback_time(z(i)) + cosmo%comoving_volume(z(i))
        end do
        threaded = 0.0_real64
        !$omp parallel do default(shared) private(i) schedule(static, 7)
        do i = 1, N_POINTS
            threaded(i) = cosmo%comoving_distance(z(i)) + cosmo%age(z(i)) &
                          + cosmo%lookback_time(z(i)) + cosmo%comoving_volume(z(i))
        end do
        !$omp end parallel do
        nbad = count(threaded /= serial)
        call check(error, nbad == 0, "a team's answers must equal the serial ones BIT FOR BIT; " // &
                   "a shared scratch component would show here")

    end subroutine test_one_object_is_evaluated_by_a_whole_team

    !> Four threads each build a different named cosmology and answer its own row.
    subroutine test_objects_built_on_different_threads_stay_independent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        ! A procedure-local array PARAMETER passed as an actual argument inside a parallel
        ! region does not compile under nagfor -- the generated C names an undeclared
        ! `<module>_MP_<procedure>Param_<name>_` (`fortran-gotchas.md`). So the names live in a
        ! VARIABLE, assigned before the region.
        character(len=8)                :: names(4)
        type(pf_cosmology), allocatable :: built(:)
        real(real64)                    :: got(4), want(4)
        type(pf_cosmology)              :: one
        integer                         :: i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: built serially the four objects could not race, so " // &
            "the independence this asserts would hold for the wrong reason")
        return
#endif
        names = [character(len=8) :: "Planck18", "WMAP9", "Planck13", "WMAP1"]

        ! The oracle comes from four SERIAL builds, not from the concurrent pass.
        do i = 1, 4
            call one%init(trim(names(i)))
            want(i) = one%comoving_distance(1.0_real64) + one%age(0.0_real64)
        end do

        ! A slot per object, allocated BEFORE the region: a `pf_cosmology` has allocatable
        ! components, so neither `private()` nor a `block` inside the region is portable.
        allocate (built(4))
        got = 0.0_real64
        !$omp parallel do default(shared) private(i) schedule(static, 1)
        do i = 1, 4
            call built(i)%init(trim(names(i)))
            got(i) = built(i)%comoving_distance(1.0_real64) + built(i)%age(0.0_real64)
        end do
        !$omp end parallel do
        call check(error, all(got == want), "four cosmologies built at once must each answer " // &
                   "its own model; module state in the integrand or the build would mix them")

    end subroutine test_objects_built_on_different_threads_stay_independent

    !> The negative control for the two tests above: without a real team they prove nothing.
    !!
    !! Each iteration writes the team it sees into a slot of its own, and the widest is taken after
    !! the region rather than through `reduction(max : widest)`: under nagfor's `-C=undefined` a
    !! region whose only shared variable is a reduction variable writes past its argument block on
    !! the caller's stack, which aborts the whole runner with no message (`fortran-gotchas.md`).
    subroutine test_the_team_is_really_opened(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer :: team(64), i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: there is no team to count")
        return
#endif
#ifdef _OPENMP
        if (omp_get_max_threads() < 2) then
            call skip_test(error, "needs at least two threads: a team of one cannot show sharing")
            return
        end if
        team = 0
        !$omp parallel do default(shared) private(i)
        do i = 1, size(team)
            team(i) = omp_get_num_threads()
        end do
        !$omp end parallel do
        call check(error, maxval(team) > 1, "the tests above rely on a team wider than one")
#endif

    end subroutine test_the_team_is_really_opened

end module test_cosmology_omp
