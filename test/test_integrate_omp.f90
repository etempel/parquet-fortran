!> Threading tests for `parquet_integrate`: that two threads integrating at once get the same
!> answers as one thread, and that a call inside a team reaches the whole team.
!!
!! **The claim under test is the module's own header sentence** -- "thread safety is by
!! construction" -- and it is a claim about ABSENCE: no module variable that is not a `parameter`,
!! no state outliving a call, every work array and record buffer a local. Nothing in the source
!! can be pointed at to prove that, so it is proved by running it: 2000 integrations of 2000
!! different integrand objects, concurrently, each against its own closed form, and the same
!! integrations again through the parameterless form that shares no object at all.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `integrate_omp`), for the nested-team reason
!! `test_index_omp.f90`'s header states: test-drive dispatches a suite's tests inside its own
!! `!$omp parallel do`, so a test opening a team of its own would open a nested one, which libgomp
!! deadlocks on intermittently (`feature_risks.md` Risk-104). A serial suite runs at level 0, where
!! the team this asserts actually opens.
!!
!! **Every test here carries the skip guard**, because without OpenMP its assertions are not merely
!! untestable but VACUOUS: the loop runs on one thread, the concurrent arm becomes a second serial
!! arm, and "the answers agree" holds for the wrong reason. A build without OpenMP must report a
!! skip here, never a pass.
!!
!! Its only library import is `use parquet_integrate`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call, and `check_test_runner_partition` requires that the
!! files feeding that runner never reach `parquet_bindings`.
module test_integrate_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_integrate
    use test_integrate_support, only : exp_profile, tail_exp
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, &
        ieee_underflow
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads
#endif

    implicit none
    private

    public :: collect_tests_integrate_omp

    !> Integrations in each concurrent loop: enough that every thread runs many of them, and that
    !! an object shared where it should not be would be seen by more than one at once.
    integer, parameter :: CASES = 2000
    !> Tolerance every integration in this suite asks for.
    real(real64), parameter :: RTOL = 1.0e-10_real64

contains

    !> Registers every test in this suite.
    subroutine collect_tests_integrate_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("one integrand object per thread integrates to its own closed form", &
                         test_objects_are_independent), &
            new_unittest("the parameterless form is reentrant to the last bit", &
                         test_plain_function_is_reentrant), &
            new_unittest("a call inside a team reaches the whole team", &
                         test_the_team_is_really_opened) &
            ]

    end subroutine collect_tests_integrate_omp

    !> 2000 objects, 2000 threads' worth of interleaving, 2000 different closed forms.
    !!
    !! `exp_profile` with `scale = 1/p` is `exp(-p x)`, whose integral over `[0, 1]` is
    !! `(1 - exp(-p))/p` -- a different number for every iteration, so an object leaking between
    !! two threads produces a wrong ANSWER rather than merely a wrong call count. That is what
    !! makes this a test of the integrator and not only of the fixture: the object carries the
    !! parameters the integrand is evaluated with, and it is `intent(inout)`, so it is exactly the
    !! state a reentrancy defect would corrupt.
    !!
    !! The counter is asserted too. `eval` increments it on every call, so `calls` must equal the
    !! evaluations `info` reports for that same integration; a thread that wrote into another
    !! thread's object leaves the two disagreeing even where the integral happens to survive.
    subroutine test_objects_are_independent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(exp_profile)         :: prof
        type(pf_integration_info) :: info
        real(real64)              :: got(CASES), want(CASES), p
        integer                   :: counted(CASES), evals(CASES), i, bad
        logical :: uf_ok, uf_was

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the loop below runs on one thread, so " // &
                       "no two integrations overlap and 'concurrent integrations are " // &
                       "independent' would hold because nothing was concurrent")
        return
#endif
        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            ! `prof` is `private`, and every one of its components is written as the FIRST statement of
            ! the loop body: an OpenMP private copy of a derived type is not reliably
            ! default-initialised under gfortran (`fortran-gotchas.md`), so nothing may be inherited
            ! from the declaration.
            !$omp parallel do default(shared) private(i, p, prof, info) schedule(static)
            do i = 1, CASES
                prof%amp = 1.0_real64
                prof%scale = 1.0_real64/(0.5_real64 + real(i, real64))
                prof%calls = 0
                p = 1.0_real64/prof%scale
                got(i) = pf_integrate(prof, 0.0_real64, 1.0_real64, RTOL, info=info)
                want(i) = (1.0_real64 - exp(-p))/p
                counted(i) = prof%calls
                evals(i) = info%neval
            end do
            !$omp end parallel do

            bad = 0
            do i = 1, CASES
                if (abs(got(i) - want(i)) > RTOL*abs(want(i))) bad = bad + 1
            end do
            call check(error, bad == 0, &
                       "a concurrently integrated object did not reproduce its own closed form")
            if (allocated(error)) exit run

            bad = 0
            do i = 1, CASES
                if (counted(i) /= evals(i)) bad = bad + 1
            end do
            call check(error, bad == 0, &
                       "an object's own call counter disagreed with the evaluations info reported")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_objects_are_independent

    !> The path that shares NO object: a plain module function, integrated 2000 times at once.
    !!
    !! Here the answer is the same every iteration, so the assertion is BIT equality against the
    !! serial result rather than a tolerance -- the strongest form available, and the right one:
    !! `pf_integrate` of a parameterless function is a deterministic computation over the caller's
    !! own locals, so any difference at all between the serial and the concurrent answer is shared
    !! state. A tolerance here would pass over exactly the defect the test exists for.
    subroutine test_plain_function_is_reentrant(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: serial, got(CASES)
        integer                   :: serial_neval, evals(CASES), i, bad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every iteration below runs on the one " // &
                       "thread that computed the serial answer, so bit equality with it is " // &
                       "arithmetic rather than a reentrancy result")
        return
#endif
        serial = pf_integrate(tail_exp, 0.0_real64, 1.0_real64, RTOL, info=info)
        serial_neval = info%neval

        !$omp parallel do default(shared) private(i, info) schedule(static)
        do i = 1, CASES
            got(i) = pf_integrate(tail_exp, 0.0_real64, 1.0_real64, RTOL, info=info)
            evals(i) = info%neval
        end do
        !$omp end parallel do

        bad = 0
        do i = 1, CASES
            if (got(i) /= serial) bad = bad + 1
        end do
        call check(error, bad == 0, &
                   "a concurrent integration of a plain function differed from the serial one")
        if (allocated(error)) return

        bad = 0
        do i = 1, CASES
            if (evals(i) /= serial_neval) bad = bad + 1
        end do
        call check(error, bad == 0, &
                   "a concurrent integration took a different number of evaluations")

    end subroutine test_plain_function_is_reentrant

    !> The guard against a vacuous pass: a team of one proves nothing, so assert the team.
    !!
    !! Both tests above compare a concurrent arm with a serial one, and both would pass with every
    !! iteration on a single thread -- which is what a nested region collapsing to a team of one
    !! looks like, and is why this suite is registered serially. This test reads the team size from
    !! inside the region and requires it to be what the runtime offers.
    !!
    !! The size is captured from whichever thread runs iteration 1 rather than from thread 0: under
    !! a dynamic schedule iteration 1 need not be thread 0's, and keying on the thread number would
    !! leave the capture at its initial value with a full team open.
    subroutine test_the_team_is_really_opened(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: got(CASES)
        integer      :: i, team_seen, team_offered

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it there is no team to size, and the " // &
                       "assertion below would compare one against one")
        return
#endif
        team_offered = 1
        team_seen = 0
#ifdef _OPENMP
        team_offered = omp_get_max_threads()
#endif
        if (team_offered < 2) then
            call skip_test(error, "needs more than one thread: with OMP_NUM_THREADS=1 the team " // &
                           "is a team of one, which is the very thing this test exists to " // &
                           "distinguish from a collapsed nested region")
            return
        end if

        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, CASES
            if (i == 1) team_seen = team_size()
            got(i) = pf_integrate(tail_exp, 0.0_real64, 1.0_real64, RTOL)
        end do
        !$omp end parallel do

        call check(error, team_seen == team_offered, &
                   "the integrating loop ran on a team smaller than the runtime offers, so " // &
                   "every concurrency assertion in this suite ran on too few threads")
        if (allocated(error)) return
        call check(error, got(1) == got(CASES), &
                   "two integrations of one function on one team must still agree bit for bit")

    end subroutine test_the_team_is_really_opened

    !> The current team's size: the real thing under OpenMP, 1 without it.
    !!
    !! A helper rather than a bare `omp_get_num_threads()` because its call site sits in a loop
    !! body compiled on BOTH sides of `#ifdef _OPENMP` -- the directives around the loop vanish
    !! without OpenMP, the loop itself does not, and the runtime routine is not declared there
    !! (`check_openmp_calls_are_guarded`).
    function team_size() result(t)
        integer :: t !! threads in the current team, or 1 without OpenMP

#ifdef _OPENMP
        t = omp_get_num_threads()
#else
        t = 1
#endif

    end function team_size

end module test_integrate_omp
