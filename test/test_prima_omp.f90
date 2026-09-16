!> Threading test for `parquet_prima`: that `pf_bobyqa_solver` under `pf_minimize_multistart`
!> gives the same answer at every thread count.
!!
!! **Why this needs a suite of its own rather than a test in `prima`.** The multistart driver runs
!! each start on its own thread through its own clone of the objective, so a BOBYQA run under it
!! is the first place the vendored engine is asked to be reentrant. PRIMA's core holds no `save`
!! variable, opens no parallel region and draws no random number -- `feature_optimizer.md` 3.4
!! checked that at the pinned commit and this suite is what holds it -- but a `save` added to a
!! vendored file later, or a clone that shares what it should copy, would change the answer only
!! when the team size changes. No assertion inside a single run can see it.
!!
!! The comparison is against the SERIAL run, so the serial arm is the reference and not another
!! sample, and the assertion is BIT equality: the driver's starts depend on the seed, the box and
!! `nstart` and on nothing else, and BOBYQA is deterministic, so two teams must agree exactly.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `prima_omp`): test-drive dispatches a suite's tests
!! inside its own `!$omp parallel do`, and a test opening a team of its own would open a nested
!! one, which libgomp deadlocks on intermittently.
!!
!! **The skip guard is not decoration.** Without OpenMP the threaded arm is a second copy of the
!! serial one and the assertion holds for exactly the reason that makes it worthless, so the test
!! reports a skip naming what went untested rather than a pass.
module test_prima_omp

    use parquet_prima
    use parquet_optimize, only : pf_minimize_multistart, parquet_debug_optimize_threads_used
    use test_optimize_support, only : twin_wells, rastrigin
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64, int64
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_procs
#endif

    implicit none
    private

    public :: collect_tests_prima_omp

    !> Team the threaded arms ask for; the same 7 `optimize_omp` uses, and for the same reason:
    !! it divides neither start count below, so the work lands unevenly.
    integer, parameter :: WANT = 7

contains

    !> Registers every test in this suite.
    subroutine collect_tests_prima_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a multistart run with pf_bobyqa_solver is bit-identical at every thread count", &
                         test_bobyqa_multistart_across_thread_counts), &
            new_unittest("a threaded BOBYQA multistart opens the team threads= asked for", &
                         test_bobyqa_multistart_opens_its_team) &
            ]

    end subroutine collect_tests_prima_omp

    !> One multistart-with-BOBYQA run serially, then at two and at seven threads, compared bit for
    !! bit: the point, the value, the counts and the record of minima.
    subroutine test_bobyqa_multistart_across_thread_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion

        type(pf_bobyqa_solver) :: solver
        type(pf_optimize_info) :: serial_info, two_info, many_info
        real(real64) :: xs(4), x2(4), xm(4), fs, f2, fm, lo(4), hi(4)

        if (.not. threads_are_available(error, &
            "one BOBYQA multistart run at three thread counts, compared bit for bit")) return

        lo = -2.0_real64
        hi = 2.0_real64
        solver%rhoend = 1.0e-9_real64

        call pf_minimize_multistart(rastrigin, lo, hi, 20260916_int64, xs, fs, nstart=11, &
                                    solver=solver, info=serial_info)
        call pf_minimize_multistart(rastrigin, lo, hi, 20260916_int64, x2, f2, nstart=11, &
                                    solver=solver, threads=2, info=two_info)
        call pf_minimize_multistart(rastrigin, lo, hi, 20260916_int64, xm, fm, nstart=11, &
                                    solver=solver, threads=WANT, info=many_info)

        call check(error, all(x2 == xs), "two threads returned a different point from serial")
        if (allocated(error)) return
        call check(error, all(xm == xs), "seven threads returned a different point from serial")
        if (allocated(error)) return
        call check(error, f2 == fs .and. fm == fs, "a threaded run returned a different value")
        if (allocated(error)) return
        call check(error, two_info%neval == serial_info%neval .and. &
                          many_info%neval == serial_info%neval, &
                   "a threaded run made a different number of evaluations")
        if (allocated(error)) return
        call check(error, two_info%nminima == serial_info%nminima .and. &
                          many_info%nminima == serial_info%nminima, &
                   "a threaded run counted a different number of distinct minima")
        if (allocated(error)) return
        call check(error, serial_info%nminima > 1, &
                   "every start reached one minimum: the comparison above had nothing to disagree about")

    end subroutine test_bobyqa_multistart_across_thread_counts

    !> The team the driver opened, paired with a `threads = 1` negative control.
    subroutine test_bobyqa_multistart_opens_its_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion

        type(pf_bobyqa_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)
        integer :: used

        if (.not. threads_are_available(error, &
            "that a threaded BOBYQA multistart opens the team it was asked for")) return

        lo = -2.0_real64
        hi = 2.0_real64
        solver%rhoend = 1.0e-8_real64

        call pf_minimize_multistart(twin_wells, lo, hi, 7_int64, x, fmin, nstart=11, &
                                    solver=solver, threads=WANT)
        used = parquet_debug_optimize_threads_used()
        call check(error, used == expected_team(), &
                   "the driver did not open the team threads= asked for")
        if (allocated(error)) return

        ! The negative control: without it, "the counter said 7" would hold just as well if the
        ! counter had been left at 7 by the run above.
        call pf_minimize_multistart(twin_wells, lo, hi, 7_int64, x, fmin, nstart=11, &
                                    solver=solver, threads=1)
        call check(error, parquet_debug_optimize_threads_used() == 1, &
                   "a threads=1 run reported a team larger than one")

    end subroutine test_bobyqa_multistart_opens_its_team

    !> Threads this process may actually open, which a cpuset can hold below `WANT`.
    integer function available_threads() result(n)

        n = 1
#ifdef _OPENMP
        n = omp_get_max_threads()
#endif

    end function available_threads

    !> The team a `threads = WANT` request must open on this machine, after the affinity clamp.
    integer function expected_team() result(n)

        n = 1
#ifdef _OPENMP
        n = min(WANT, omp_get_num_procs())
#endif

    end function expected_team

    !> Skips the calling test, naming what went untested, when this build has no OpenMP.
    logical function threads_are_available(error, what) result(ok)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle
        character(len=*), intent(in)               :: what  !! the assertion being skipped

        ok = .true.
#ifndef _OPENMP
        ok = .false.
        call skip_test(error, "this build has no OpenMP, so nothing can assert " // what)
#endif
        if (ok .and. available_threads() < 2) then
            ok = .false.
            call skip_test(error, "this process may open only one thread, so nothing can assert " // what)
        end if

    end function threads_are_available

end module test_prima_omp
