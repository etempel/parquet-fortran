!> Threading tests for `parquet_optimize`'s population tier: that `pf_minimize_de` and
!> `pf_minimize_multistart` give the same answer at every thread count, and that `threads=`
!> reaches the team.
!!
!! **The property under test is invisible to every other test in the suite.** Both engines are
!! designed to answer identically at any team size -- every draw is addressed by
!! `(seed, generation, individual)` and every decision that combines individuals is element-wise or
!! lowest-index -- so no assertion about `x`, `fmin`, `info` or `population` can distinguish a
!! threaded run from a serial one. Two things follow, and they are the whole suite:
!!
!!  1. **Only a comparison ACROSS thread counts can fail.** A stream seeded once per generation and
!!     walked across individuals, a "first thread to find a better value wins" update, or an OpenMP
!!     `reduction` over reals would each give a different answer per team size -- and every one of
!!     those answers is a valid run of the algorithm, so nothing else notices. The comparison is
!!     against the SERIAL run, so the serial arm is the reference rather than another sample.
!!  2. **Only a team counter can fail when `threads=` is dropped.** A count that is validated,
!!     clamped and then never reaches a `num_threads` clause leaves every answer unchanged, so
!!     `parquet_debug_optimize_threads_used` is the one observation that can see it. It is paired
!!     with a `threads = 1` negative control, without which "the counter said 7" would hold just as
!!     well if it had been left at 7 by the previous test.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `optimize_omp`), for two reasons: test-drive dispatches a
!! suite's tests inside its own `!$omp parallel do`, so a test opening a team of its own would open
!! a nested one, which libgomp deadlocks on intermittently; and the team counter is saved
!! process-global state, which two concurrent tests would overwrite for each other.
!!
!! **Every test here carries the skip guard.** Without OpenMP the threaded arms are copies of the
!! serial one and the counter is always 1, so the assertions hold for the wrong reason -- a build
!! without OpenMP must report a skip that names what went untested, never a pass.
!!
!! Its only library import is `use parquet_optimize`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call, and `check_test_runner_partition` requires that the files
!! feeding that runner never reach `parquet_bindings`.
module test_optimize_omp

    use parquet_optimize
    use test_optimize_support, only : rosenbrock, rastrigin, table_sphere
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64, int64
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_procs
#endif

    implicit none
    private

    public :: collect_tests_optimize_omp

    !> Team the threaded arms ask for. 7 divides neither population nor start count used below, so
    !! the work lands on the threads differently from any arm that happens to divide evenly.
    integer, parameter :: WANT = 7

contains

    !> Registers every test in this suite.
    subroutine collect_tests_optimize_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a DE run is bit-identical at every thread count", &
                         test_de_across_thread_counts), &
            new_unittest("a multistart run is bit-identical at every thread count", &
                         test_multistart_across_thread_counts), &
            new_unittest("DE opens the team threads= asked for", &
                         test_de_opens_its_team), &
            new_unittest("the multistart driver opens the team threads= asked for", &
                         test_multistart_opens_its_team) &
            ]

    end subroutine collect_tests_optimize_omp

    !> One DE run serially, then at two and at seven threads, compared bit for bit.
    !!
    !! The objective is the ten-variable Rosenbrock with a population of 23: seven threads share 23
    !! individuals unevenly under `schedule(dynamic)`, and the run is long enough that a divergence
    !! in one generation would be compounded by all the rest.
    subroutine test_de_across_thread_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: serial_info, two_info, many_info
        real(real64) :: xs(10), x2(10), xm(10), fs, f2, fm, lo(10), hi(10)
        real(real64), allocatable :: pops(:,:), pop2(:,:), popm(:,:)

        if (.not. threads_are_available(error, "one DE run at three thread counts, compared bit for bit")) return

        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, xs, fs, np=23, max_gen=120, &
                            info=serial_info, population=pops)
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, x2, f2, np=23, max_gen=120, threads=2, &
                            info=two_info, population=pop2)
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, xm, fm, np=23, max_gen=120, threads=WANT, &
                            info=many_info, population=popm)

        call check(error, parquet_debug_optimize_threads_used() > 1, &
            "vacuity guard: the last run opened a team of one, so no arm here varied anything")
        if (allocated(error)) return

        call check(error, all(x2 == xs), "two threads must give the serial point, bit for bit")
        if (allocated(error)) return
        call check(error, all(xm == xs), "seven threads must give the serial point, bit for bit")
        if (allocated(error)) return
        call check(error, f2 == fs .and. fm == fs, "and the serial value with it")
        if (allocated(error)) return
        call check(error, all(pop2 == pops) .and. all(popm == pops), &
            "the whole final population must agree, not only its best individual")
        if (allocated(error)) return
        call check(error, two_info%neval == serial_info%neval .and. many_info%neval == serial_info%neval, &
            "the thread count must not change how many evaluations the run needs")
        if (allocated(error)) return
        call check(error, two_info%niter == serial_info%niter .and. many_info%niter == serial_info%niter, &
            "nor how many generations it spends")
        if (allocated(error)) return
        call check(error, two_info%status == serial_info%status .and. many_info%status == serial_info%status, &
            "nor which stopping rule fires")

    end subroutine test_de_across_thread_counts

    !> One multistart run serially, then at two and at seven threads, compared bit for bit.
    !!
    !! Rastrigin over a box gives the starts wildly different costs, which is what makes
    !! `schedule(dynamic)` hand them out in a different order on every run -- and so what makes
    !! "the answer does not depend on which thread finished first" a claim worth testing.
    subroutine test_multistart_across_thread_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: serial_info, two_info, many_info
        type(pf_optimize_history) :: rec_s, rec_m
        real(real64) :: xs(2), x2(2), xm(2), fs, f2, fm, lo(2), hi(2)

        if (.not. threads_are_available(error, &
            "one multistart run at three thread counts, compared bit for bit")) return

        lo = -5.12_real64
        hi = 5.12_real64
        call pf_minimize_multistart(rastrigin, lo, hi, 11_int64, xs, fs, nstart=23, &
                                    info=serial_info, history=rec_s)
        call pf_minimize_multistart(rastrigin, lo, hi, 11_int64, x2, f2, nstart=23, threads=2, &
                                    info=two_info)
        call pf_minimize_multistart(rastrigin, lo, hi, 11_int64, xm, fm, nstart=23, threads=WANT, &
                                    info=many_info, history=rec_m)

        call check(error, parquet_debug_optimize_threads_used() > 1, &
            "vacuity guard: the last run opened a team of one, so no arm here varied anything")
        if (allocated(error)) return

        call check(error, all(x2 == xs) .and. all(xm == xs), &
            "every thread count must give the serial point, bit for bit")
        if (allocated(error)) return
        call check(error, f2 == fs .and. fm == fs, "and the serial value with it")
        if (allocated(error)) return
        call check(error, all(rec_m%x == rec_s%x) .and. all(rec_m%f == rec_s%f), &
            "every start's own minimum must agree, in start order, not only the winner")
        if (allocated(error)) return
        call check(error, many_info%nminima == serial_info%nminima, &
            "the distinct-minimum count is taken by walking the starts in index order")
        if (allocated(error)) return
        call check(error, two_info%neval == serial_info%neval .and. many_info%neval == serial_info%neval, &
            "the thread count must not change the total evaluation count")
        if (allocated(error)) return
        call check(error, many_info%nlimit == serial_info%nlimit, "nor how many runs ran out of budget")

    end subroutine test_multistart_across_thread_counts

    !> `threads=` reaches DE's own `num_threads` clause, with a serial negative control.
    subroutine test_de_opens_its_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(table_sphere) :: obj
        real(real64) :: x(3), fmin, lo(3), hi(3)
        integer :: asked, got, serial_got

        if (.not. threads_are_available(error, "that threads= reaches DE's own team")) return

        obj%centre = [0.5_real64, -0.5_real64, 1.5_real64]
        lo = -3.0_real64
        hi = 3.0_real64

        call pf_minimize_de(obj, lo, hi, 4_int64, x, fmin, max_gen=8, threads=1)
        serial_got = parquet_debug_optimize_threads_used()

        call pf_minimize_de(obj, lo, hi, 4_int64, x, fmin, max_gen=8, threads=WANT)
        got = parquet_debug_optimize_threads_used()

        ! The clamp may lower the request on a process confined to fewer processors than it asked
        ! for, so the assertion is against what this machine can actually grant, never against 7.
        asked = expected_team()

        call check(error, got, asked, &
            "threads= must reach the num_threads clause: the team opened is what was asked for")
        if (allocated(error)) return
        call check(error, serial_got, 1, &
            "negative control: threads = 1 must leave the counter at one, or 'the counter said 7' " // &
            "would hold with threads= dropped entirely")
        if (allocated(error)) return
        call check(error, got > 1, &
            "vacuity guard: this machine granted a team of one, so the positive arm asserted nothing")

    end subroutine test_de_opens_its_team

    !> `threads=` reaches the multistart driver's own `num_threads` clause, with a serial control.
    subroutine test_multistart_opens_its_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin, lo(2), hi(2)
        integer :: asked, got, serial_got

        if (.not. threads_are_available(error, "that threads= reaches the multistart driver's team")) return

        lo = -2.0_real64
        hi = 2.0_real64

        call pf_minimize_multistart(rosenbrock, lo, hi, 4_int64, x, fmin, nstart=9, threads=1)
        serial_got = parquet_debug_optimize_threads_used()

        call pf_minimize_multistart(rosenbrock, lo, hi, 4_int64, x, fmin, nstart=9, threads=WANT)
        got = parquet_debug_optimize_threads_used()

        asked = expected_team()

        call check(error, got, asked, &
            "threads= must reach the num_threads clause: the team opened is what was asked for")
        if (allocated(error)) return
        call check(error, serial_got, 1, "negative control: threads = 1 must leave the counter at one")
        if (allocated(error)) return
        call check(error, got > 1, &
            "vacuity guard: this machine granted a team of one, so the positive arm asserted nothing")

    end subroutine test_multistart_opens_its_team

    !> Threads this build and this process can actually open; 1 without OpenMP.
    integer function available_threads() result(n)

        n = 1
#ifdef _OPENMP
        n = omp_get_max_threads()
#endif

    end function available_threads

    !> The team a `threads = WANT` request must open on this machine.
    !!
    !! `parquet_clamp_to_affinity` lowers a request to `omp_get_num_procs()`, which on a process
    !! confined to a small cpuset is below `WANT`; the assertion is against that rather than
    !! against 7, so a correct library does not fail its own suite in a container.
    !!
    !! It does NOT account for `parquet_debug_set_affinity_procs`, which would override the count
    !! process-wide. Nothing in `run_tester_pf`'s suites calls it -- the clamp's own tests live in
    !! `settings`, in the other runner and so in another process -- and this is where that would
    !! first be noticed.
    integer function expected_team() result(n)

        n = 1
#ifdef _OPENMP
        n = min(WANT, omp_get_num_procs())
#endif

    end function expected_team

    !> Skips the calling test, naming what went untested, when this build has no OpenMP.
    !!
    !! A skip rather than a pass: without OpenMP every threaded arm is a second copy of the serial
    !! one and the team counter never leaves 1, so each assertion here would hold for exactly the
    !! reason that makes it worthless.
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

end module test_optimize_omp
