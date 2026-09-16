!> Threading tests for `parquet_interpolate`: that one object nobody writes may be evaluated from a
!> whole team at once, that objects built concurrently on different threads stay independent, and
!> that the team these tests rely on is really opened.
!!
!! **The claim under test is the module's own header sentence** -- "one object nobody writes may be
!! evaluated from any number of threads at once, and two objects never share anything" -- and it is
!! a claim about ABSENCE: no module variable that is not a `parameter`, no search state kept in an
!! object. Nothing in the source can be pointed at to prove that, so it is proved by running it,
!! with every concurrent answer held against an oracle that does not come from the serial pass.
!!
!! **A thread's own object lives in a slot of an array allocated before the region**, indexed by
!! the thread number. That is the shape the guide page tells a user to write: a `pf_interp_1d` has
!! allocatable components, so an OpenMP `private()` copy of one is not reliably initialised under
!! gfortran, and ifx segfaults on one declared in a `block` lexically inside the region
!! (`fortran-gotchas.md`).
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `interpolate_omp`): test-drive dispatches a suite's tests
!! inside its own `!$omp parallel do`, where a region opened by a test would be nested and get a
!! team of one. **Every test here carries the skip guard**, because without OpenMP its assertions
!! would hold for the wrong reason: a build without OpenMP must report a skip here, never a pass.
!!
!! Its only library import is `use parquet_interpolate`: it is registered in `run_tester_pf.f90`,
!! the runner that executes no `bind(C)` call.
module test_interpolate_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_interpolate
    use iso_fortran_env, only : real64
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads, omp_get_thread_num
#endif

    implicit none
    private

    public :: collect_tests_interpolate_omp

    !> Knots in the shared table: `sin` over one period, where the natural end condition is exact.
    integer, parameter :: KNOTS = 4097
    !> Queries evaluated concurrently against the shared table.
    integer, parameter :: QUERIES = 200000
    !> Tables built concurrently, one per iteration, each on its own thread's object.
    integer, parameter :: BUILDS = 20000

contains

    !> Registers every test in this suite.
    subroutine collect_tests_interpolate_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("one object is evaluated by a whole team at once", &
                         test_shared_object_is_read_only), &
            new_unittest("objects built on different threads at once stay independent", &
                         test_objects_per_thread_are_independent), &
            new_unittest("a call inside a team reaches the whole team", &
                         test_the_team_is_really_opened) &
            ]

    end subroutine collect_tests_interpolate_omp

    !> One cubic object over `sin` on `[0, 2*pi]`, evaluated at 200 000 queries from a parallel loop.
    !!
    !! Two assertions. Bit equality with a serial pass over the same queries, since `%eval` of a
    !! built object is a deterministic computation over it; and, because both arms share the object,
    !! every concurrent answer against `sin` itself. The natural spline of `sin` over one period has
    !! the right second derivative at both ends, so its error is the interior bound
    !! `5/384 * h**4 * max|sin''''|` with `h = 2*pi/4096`, about `7e-14`; `1e-10` leaves room for the
    !! libm `sin` the oracle calls.
    subroutine test_shared_object_is_read_only(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: PI = 4.0_real64*atan(1.0_real64)

        type(pf_interp_1d)        :: curve
        real(real64), allocatable :: x(:), y(:), q(:), serial(:), shared(:)
        integer                   :: i, differ, far

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the loop below runs on one thread, so " // &
                       "'one object evaluated by a whole team at once' would hold because " // &
                       "nothing was concurrent")
        return
#endif
        allocate (x(KNOTS), y(KNOTS), q(QUERIES), serial(QUERIES), shared(QUERIES))
        do i = 1, KNOTS
            x(i) = 2.0_real64*PI*real(i - 1, real64)/real(KNOTS - 1, real64)
            y(i) = sin(x(i))
        end do
        do i = 1, QUERIES
            ! 7919 is prime and shares no factor with QUERIES, so this visits every residue once; and
            ! `QUERIES*7919` stays below `huge(1)`.
            q(i) = 2.0_real64*PI*real(mod(i*7919, QUERIES), real64)/real(QUERIES, real64)
        end do
        call curve%init(x, y)
        serial = curve%eval(q)

        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, QUERIES
            shared(i) = curve%eval(q(i))
        end do
        !$omp end parallel do

        differ = count(shared /= serial)
        call check(error, differ == 0, "a concurrent evaluation of one object differed from the serial one")
        if (allocated(error)) return
        far = count(abs(shared - sin(q)) > 1.0e-10_real64)
        call check(error, far == 0, "a concurrent evaluation strayed from sin beyond the spline's error bound")

    end subroutine test_shared_object_is_read_only

    !> 20 000 tables, each built and evaluated on its own thread's object while the other threads do
    !! the same, each against its own closed form.
    !!
    !! Iteration `i` tabulates `s*(2*x + 1)` with a scale `s` of its own, a line both methods
    !! reproduce, so an object whose table leaked from another thread's iteration answers a wrong
    !! NUMBER rather than merely a different one. The one-shot form is asserted beside it, because
    !! its object is a local of the function and so is the other shape a caller may use.
    subroutine test_objects_per_thread_are_independent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: TABLE = 9

        type(pf_interp_1d), allocatable :: slot(:)
        real(real64) :: xt(TABLE), yt(TABLE), s, query, want
        real(real64) :: got(BUILDS), shot(BUILDS), expect(BUILDS)
        integer      :: i, j, t, far

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every object below is built on one thread, " // &
                       "so no two builds overlap and 'objects stay independent' would hold " // &
                       "because nothing was concurrent")
        return
#endif
        do j = 1, TABLE
            xt(j) = 0.25_real64*real(j - 1, real64)
        end do
        allocate (slot(team_capacity()))

        !$omp parallel do default(shared) private(i, j, t, yt, s, query, want) schedule(dynamic, 64)
        do i = 1, BUILDS
            t = thread_slot()
            s = 1.0_real64 + real(i, real64)
            do j = 1, TABLE
                yt(j) = s*(2.0_real64*xt(j) + 1.0_real64)
            end do
            query = 0.3_real64 + real(mod(i, 97), real64)/64.0_real64
            want = s*(2.0_real64*query + 1.0_real64)
            if (mod(i, 2) == 0) then
                call slot(t)%init(xt, yt, method="linear")
            else
                call slot(t)%init(xt, yt)
            end if
            got(i) = slot(t)%eval(query)
            shot(i) = pf_interp(xt, yt, query)
            expect(i) = want
        end do
        !$omp end parallel do

        far = count(abs(got - expect) > 1.0e-12_real64*abs(expect))
        call check(error, far == 0, "an object built on its own thread answered another table's value")
        if (allocated(error)) return
        far = count(abs(shot - expect) > 1.0e-12_real64*abs(expect))
        call check(error, far == 0, "a concurrent one-shot call answered another table's value")

    end subroutine test_objects_per_thread_are_independent

    !> The guard against a vacuous pass: a team of one proves nothing, so assert the team.
    !!
    !! Both tests above would pass with every iteration on a single thread -- which is what a nested
    !! region collapsing to a team of one looks like, and why this suite is registered serially. The
    !! size is captured from whichever thread runs iteration 1 rather than from thread 0.
    subroutine test_the_team_is_really_opened(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_interp_1d) :: line
        real(real64)       :: got(1000)
        integer            :: i, team_seen, team_offered

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it there is no team to size, and the " // &
                       "assertion below would compare one against one")
        return
#endif
        team_offered = team_capacity()
        team_seen = 0
        if (team_offered < 2) then
            call skip_test(error, "needs more than one thread: with OMP_NUM_THREADS=1 the team " // &
                           "is a team of one, which is the very thing this test exists to " // &
                           "distinguish from a collapsed nested region")
            return
        end if
        call line%init([0.0_real64, 1.0_real64], [1.0_real64, 3.0_real64], method="linear")

        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, size(got)
            if (i == 1) team_seen = team_size()
            got(i) = line%eval(0.5_real64)
        end do
        !$omp end parallel do

        call check(error, team_seen == team_offered, &
                   "the evaluating loop ran on a team smaller than the runtime offers, so " // &
                   "every concurrency assertion in this suite ran on too few threads")
        if (allocated(error)) return
        call check(error, all(got == 2.0_real64), "every thread must answer the line's midpoint")

    end subroutine test_the_team_is_really_opened

    !> The largest team the runtime offers: `omp_get_max_threads()` under OpenMP, 1 without it.
    function team_capacity() result(t)
        integer :: t !! threads a region opened here may use

#ifdef _OPENMP
        t = omp_get_max_threads()
#else
        t = 1
#endif

    end function team_capacity

    !> The current team's size: the real thing under OpenMP, 1 without it.
    !!
    !! A helper rather than a bare `omp_get_num_threads()` because its call site sits in a loop body
    !! compiled on BOTH sides of `#ifdef _OPENMP` (`check_openmp_calls_are_guarded`).
    function team_size() result(t)
        integer :: t !! threads in the current team, or 1 without OpenMP

#ifdef _OPENMP
        t = omp_get_num_threads()
#else
        t = 1
#endif

    end function team_size

    !> This thread's slot in a per-thread array: `omp_get_thread_num() + 1`, or 1 without OpenMP.
    function thread_slot() result(t)
        integer :: t !! the slot, from 1 to `team_capacity()`

#ifdef _OPENMP
        t = omp_get_thread_num() + 1
#else
        t = 1
#endif

    end function thread_slot

end module test_interpolate_omp
