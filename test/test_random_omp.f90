!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The claim `parquet_random` exists to make: the same values under every OpenMP schedule.
!>
!> Everything else in the suite checks that the module computes the right numbers. This checks the
!> only property that motivated writing it at all -- that a parallel loop gets the same numbers as
!> a serial one, whatever the schedule and whatever the thread count. A stateful generator cannot
!> do this at any speed, and no amount of locking fixes it: with shared state, which value an
!> iteration receives depends on how many draws happened first.
!>
!> **This suite is excluded from test-drive's per-test parallelism** (see
!> `suite_is_safe_to_parallelize` in `run_tester.f90`), and the reason is specific rather than
!> precautionary. test-drive dispatches a suite's tests inside its own `!$omp parallel do`, so a
!> parallel region opened by a test is NESTED -- and with nesting disabled by default it gets a
!> team of one, which would make every schedule comparison below pass without ever running two
!> threads. Excluding the suite gives these regions a real team. Enabling nested parallelism
!> instead would mutate process-global OpenMP state underneath concurrently running sibling
!> suites, which is exactly the hazard the exclusion list exists for.
module test_random_omp

    use parquet
    use iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
#ifdef _OPENMP
    use omp_lib, only: omp_get_max_threads, omp_get_num_threads
#endif

    implicit none
    private
    public :: collect_tests_parquet_random_omp

    !> How many draws each arm of the schedule comparison produces.
    integer, parameter :: n = 20000
    !> The seed every arm uses. Any value would do; a fixed one keeps the test deterministic.
    integer(int64), parameter :: seed = 20260816_int64

contains

    !> Registers every test in the `random_omp` suite.
    subroutine collect_tests_parquet_random_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("draws are identical under every schedule and thread count", test_schedule_independence), &
            new_unittest("pf_random_seed differs across concurrent threads", test_seed_across_threads) &
            ]
    end subroutine collect_tests_parquet_random_omp

    !> Fills the same array five ways and requires every value to be bit-identical.
    !!
    !! Three things here are load-bearing and each is easy to leave out:
    !!
    !!  * **Variable per-iteration work.** Without it a single thread can claim the whole loop
    !!    before the others start, and the test passes without ever having tested anything. Each
    !!    iteration therefore does an amount of extra work that depends on its index -- extra draws
    !!    that are stored, so they cannot be optimised away, and that provably do not disturb the
    !!    value being checked, since the module has no state for them to disturb.
    !!  * **A vacuity guard on the team size.** If a region gets a team of one, every arm is really
    !!    the serial arm and the comparison is empty. That must fail loudly rather than pass quietly.
    !!    The guard asserts what the region ACHIEVED, which is the only thing that answers the
    !!    question -- an ambient thread count says what was available, not what OpenMP handed out.
    !!  * **Two different thread counts, one of them not a divisor of `n`.** A schedule that
    !!    happens to partition the loop the same way twice would agree for the wrong reason.
    !!
    !! **The first two arms ASK for their team rather than accepting the ambient one**, via
    !! `num_threads(want)` where `want` is `max(2, omp_get_max_threads())`. On any ordinary machine
    !! that is the ambient count and nothing changes. On a one-core machine, or under
    !! `OMP_NUM_THREADS=1`, or in a container that pins the count, it is 2 -- and that is the
    !! difference between a correct library failing its own suite and being tested properly. The
    !! `num_threads` clause overrides the ambient count for that region, which is exactly what
    !! `test_seed_across_threads` below has always relied on. The vacuity guard is unchanged and
    !! still fires if a team of one is somehow handed out anyway; it is now a real assertion about
    !! OpenMP rather than a report that the environment was small.
    subroutine test_schedule_independence(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: serial(n), stat(n), dyn(n), few(n), many(n)
        integer(int64) :: burn(n)
        integer :: i, team_static, team_dynamic, want

        team_static = 1
        team_dynamic = 1
        want = 2
#ifdef _OPENMP
        want = max(2, omp_get_max_threads())
#endif

        do i = 1, n
            serial(i) = draw(i, burn(i))
        end do

#ifdef _OPENMP
        !$omp parallel do schedule(static) num_threads(want) default(shared) private(i)
        do i = 1, n
            if (i == 1) team_static = omp_get_num_threads()
            stat(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        !$omp parallel do schedule(dynamic, 1) num_threads(want) default(shared) private(i)
        do i = 1, n
            ! Captured from whichever thread runs iteration 1 -- under schedule(dynamic,1) that
            ! is not thread 0, and keying the capture on thread 0 would leave this reading 1
            ! even with a full team, i.e. a false alarm from the guard rather than a real one.
            if (i == 1) team_dynamic = omp_get_num_threads()
            dyn(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        !$omp parallel do schedule(static) num_threads(2) default(shared) private(i)
        do i = 1, n
            few(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        ! 7 does not divide n, so this schedule's blocks land differently from every arm above.
        !$omp parallel do schedule(dynamic, 3) num_threads(7) default(shared) private(i)
        do i = 1, n
            many(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        call check(error, team_static > 1, &
            "vacuity guard: the schedule(static) region ran with a team of one, so nothing was varied -- " // &
            "check that this suite is still excluded from test-drive's own per-test parallelism")
        if (allocated(error)) return
        call check(error, team_dynamic > 1, &
            "vacuity guard: the schedule(dynamic,1) region ran with a team of one, so nothing was varied")
        if (allocated(error)) return
#else
        ! Without OpenMP there is no schedule to vary, so there is nothing this test can assert.
        ! It is genuinely vacuous here rather than falsely green: the arms are copies of the serial
        ! loop, and saying so is better than skipping, because a build that silently lost OpenMP
        ! would otherwise look the same as one that never had it.
        stat = serial
        dyn = serial
        few = serial
        many = serial
#endif

        call check(error, all(stat == serial), "schedule(static) produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(dyn == serial), "schedule(dynamic,1) produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(few == serial), "a 2-thread run produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(many == serial), "a 7-thread run produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(serial >= 0.0_real64 .and. serial < 1.0_real64), &
            "a draw escaped [0, 1) -- the arms agree but on the wrong values")
    end subroutine test_schedule_independence

    !> One draw plus an index-dependent amount of extra, stored work.
    !!
    !! The extra draws exist only to make iterations cost different amounts, so that no schedule
    !! can degenerate into "one thread does everything". They are returned rather than discarded so
    !! no compiler can delete them, and they cannot affect `r`: every value this module produces is
    !! a function of its coordinates alone.
    function draw(i, burnt) result(r)
        integer, intent(in) :: i                    !! the loop index, used as the stream
        integer(int64), intent(out) :: burnt        !! an accumulator over the extra draws
        real(real64) :: r                           !! the value under test
        integer :: k
        r = pf_random_at(seed, int(i, int64))
        burnt = 0_int64
        do k = 1, modulo(i, 23) + 1
            burnt = ieor(burnt, pf_random_bits_at(seed, int(i, int64), int(k, int64) + 1_int64))
        end do
    end function draw

    !> Concurrent calls to `pf_random_seed` must return different values.
    !!
    !! This belongs here rather than in the `random` suite: test-drive's parallelism runs whole
    !! TESTS concurrently, so a test there would compare only its own thread's values against each
    !! other. The property that matters is that two threads calling at the same moment -- possibly
    !! within the same clock tick -- come away with different seeds, which is what the critical
    !! region around the process-wide counter is for.
    !!
    !! **The vacuity guard here is not optional decoration, and its absence would be invisible.**
    !! `team` says how many threads were ASKED for; `got` says how many the region received. With a
    !! team of one there is no concurrency, and yet every assertion below still passes -- the
    !! process-wide counter guarantees distinct values when the calls are serial, so the duplicate
    !! count is zero for the wrong reason and the race this test exists to catch goes unexercised
    !! while the test stays green. `test_schedule_independence` above has always guarded this;
    !! this one did not, and read as passing on a machine where it proved nothing.
    subroutine test_seed_across_threads(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: per_thread = 50
        integer :: nt, i, j, duplicates, team, got
        integer(int64), allocatable :: s(:)

        team = 1
#ifdef _OPENMP
        team = max(2, min(8, omp_get_max_threads()))
#endif
        nt = team * per_thread
        allocate(s(nt))
        s = 0_int64
        got = 1

#ifdef _OPENMP
        !$omp parallel do schedule(static) num_threads(team) default(shared) private(i)
#endif
        do i = 1, nt
            ! Captured from whichever thread runs iteration 1, matching the sibling test's reasoning:
            ! keying this on thread 0 would read 1 even with a full team under some schedules.
            if (i == 1) got = team_size()
            s(i) = pf_random_seed()
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif

#ifdef _OPENMP
        call check(error, got > 1, &
            "vacuity guard: the seed region ran with a team of one, so no two calls were ever concurrent and " // &
            "the duplicate check below proves nothing -- check that this suite is still excluded from " // &
            "test-drive's own per-test parallelism")
        if (allocated(error)) return
#endif
        call check(error, all(s >= 1_int64), "pf_random_seed returned a value below 1 from a thread")
        if (allocated(error)) return
        duplicates = 0
        do i = 1, nt
            do j = i + 1, nt
                if (s(i) == s(j)) duplicates = duplicates + 1
            end do
        end do
        call check(error, duplicates == 0, &
            "two pf_random_seed calls returned the same value -- the process-wide counter is not being incremented " // &
            "atomically, so two threads read it before either wrote it back")
    end subroutine test_seed_across_threads

    !> The current team's size: the real thing under OpenMP, 1 without it.
    !!
    !! A helper rather than a bare `omp_get_num_threads()` because its one call site sits in a loop
    !! body that is compiled on BOTH sides of `#ifdef _OPENMP`. The directives around that loop
    !! vanish without OpenMP but the loop itself does not, and the runtime routine is not declared
    !! there -- which is the same portability trap `materialize_marked_parallel` documents in
    !! `src/parquet_tables_read.f90`, met here in a test rather than in the library.
    function team_size() result(t)
        integer :: t                                !! threads in the current team, or 1 without OpenMP
#ifdef _OPENMP
        t = omp_get_num_threads()
#else
        t = 1
#endif
    end function team_size

end module test_random_omp
