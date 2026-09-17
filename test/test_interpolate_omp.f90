!> Threading tests for `parquet_interpolate`: that one object nobody writes may be evaluated,
!> differentiated and integrated from a whole team at once, a grid object evaluated likewise, that
!> objects built concurrently on different threads stay independent, that a table of hundreds of
!> thousands of points is built and interpolated on a worker thread's own stack, and that the team
!> these tests rely on is really opened.
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
    use test_interpolate_support, only : nan_value
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
    !> Grid lines along `x` of the shared grid, `sin(x)*sin(y)` over `[0, 2*pi]` by `[0, pi]`: a spacing
    !! of `pi/256`.
    integer, parameter :: GRID_NX = 513
    !> Grid lines along `y` of the shared grid: the same spacing.
    integer, parameter :: GRID_NY = 257

contains

    !> Registers every test in this suite.
    subroutine collect_tests_interpolate_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("one object is evaluated by a whole team at once", &
                         test_shared_object_is_read_only), &
            new_unittest("one grid object is evaluated by a whole team at once", &
                         test_shared_grid_is_read_only), &
            new_unittest("objects built on different threads at once stay independent", &
                         test_objects_per_thread_are_independent), &
            new_unittest("a large masked table and a large one-shot call fit on a worker thread's stack", &
                         test_large_tables_on_worker_threads), &
            new_unittest("a call inside a team reaches the whole team", &
                         test_the_team_is_really_opened) &
            ]

    end subroutine collect_tests_interpolate_omp

    !> One cubic object over `sin` on `[0, 2*pi]`, evaluated at 200 000 queries from a parallel loop,
    !! differentiated at each of them and integrated up to every hundredth.
    !!
    !! Two assertions on each binding. Bit equality with a serial pass over the same queries, since a
    !! reading binding of a built object is a deterministic computation over it; and, because both
    !! arms share the object, every concurrent answer against its closed form. The natural spline of
    !! `sin` over one period has the right second derivative at both ends, so its errors are the
    !! interior bounds with `h = 2*pi/4096`: `5/384 * h**4 * max|sin''''|` for the value, about
    !! `7e-14`, and `h**3/24 * max|sin''''|` for the slope, about `2e-10`; `1e-10` and `1e-8` leave room
    !! for the libm `sin` and `cos` the oracles call. The integral from 0 is `1 - cos(q)`, within the
    !! value's bound times the width, and is asserted to `1e-9`.
    subroutine test_shared_object_is_read_only(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: PI = 4.0_real64*atan(1.0_real64)
        integer, parameter      :: EVERY = 100

        type(pf_interp_1d)        :: curve
        real(real64), allocatable :: x(:), y(:), q(:), serial(:), shared(:), slope(:), slope_shared(:)
        real(real64), allocatable :: area(:), area_shared(:)
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
        allocate (slope_shared(QUERIES), area(QUERIES/EVERY), area_shared(QUERIES/EVERY))
        call curve%init(x, y)
        serial = curve%eval(q)
        slope = curve%derivative(q)
        do i = 1, QUERIES/EVERY
            area(i) = curve%integral(0.0_real64, q(i*EVERY))
        end do

        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, QUERIES
            shared(i) = curve%eval(q(i))
            slope_shared(i) = curve%derivative(q(i))
        end do
        !$omp end parallel do
        !$omp parallel do default(shared) private(i) schedule(dynamic, 8)
        do i = 1, QUERIES/EVERY
            area_shared(i) = curve%integral(0.0_real64, q(i*EVERY))
        end do
        !$omp end parallel do

        differ = count(shared /= serial) + count(slope_shared /= slope) + count(area_shared /= area)
        call check(error, differ == 0, "a concurrent evaluation of one object differed from the serial one")
        if (allocated(error)) return
        far = count(abs(shared - sin(q)) > 1.0e-10_real64)
        call check(error, far == 0, "a concurrent evaluation strayed from sin beyond the spline's error bound")
        if (allocated(error)) return
        far = count(abs(slope_shared - cos(q)) > 1.0e-8_real64)
        call check(error, far == 0, "a concurrent derivative strayed from cos beyond the spline's error bound")
        if (allocated(error)) return
        far = count(abs(area_shared - (1.0_real64 - cos(q(EVERY::EVERY)))) > 1.0e-9_real64)
        call check(error, far == 0, "a concurrent integral strayed from 1 - cos beyond the spline's error bound")

    end subroutine test_shared_object_is_read_only

    !> One bicubic grid object over `sin(x)*sin(y)`, evaluated at 200 000 points from a parallel loop.
    !!
    !! Bit equality with a serial pass over the same points, and every concurrent answer against the
    !! closed form. `sin` has a zero second derivative at `0`, `pi` and `2*pi`, so the natural spline of
    !! `sin` along each axis has the right end condition and errs by at most the interior bound
    !! `5/384 * h**4 * max|sin''''|` with `h = pi/256`, about `3e-10`. On a grid of products the
    !! bicubic spline is the product of the two one-dimensional splines, so it errs by at most the
    !! sum of their two errors, about `6e-10`; `1e-8` leaves room for the libm `sin` the oracle calls.
    subroutine test_shared_grid_is_read_only(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: PI = 4.0_real64*atan(1.0_real64)

        type(pf_interp_2d)        :: surface
        real(real64), allocatable :: gx(:), gy(:), gz(:, :), px(:), py(:), serial(:), shared(:)
        integer                   :: i, j, differ, far

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the loop below runs on one thread, so " // &
                       "'one grid object evaluated by a whole team at once' would hold because " // &
                       "nothing was concurrent")
        return
#endif
        allocate (gx(GRID_NX), gy(GRID_NY), gz(GRID_NX, GRID_NY), px(QUERIES), py(QUERIES), shared(QUERIES))
        do i = 1, GRID_NX
            gx(i) = 2.0_real64*PI*real(i - 1, real64)/real(GRID_NX - 1, real64)
        end do
        do j = 1, GRID_NY
            gy(j) = PI*real(j - 1, real64)/real(GRID_NY - 1, real64)
        end do
        do j = 1, GRID_NY
            do i = 1, GRID_NX
                gz(i, j) = sin(gx(i))*sin(gy(j))
            end do
        end do
        do i = 1, QUERIES
            ! 7919 and 10007 are primes sharing no factor with QUERIES, so each coordinate visits every
            ! residue once, in different orders; both products stay below `huge(1)`.
            px(i) = 2.0_real64*PI*real(mod(i*7919, QUERIES), real64)/real(QUERIES, real64)
            py(i) = PI*real(mod(i*10007, QUERIES), real64)/real(QUERIES, real64)
        end do
        call surface%init(gx, gy, gz)
        serial = surface%eval(px, py)

        !$omp parallel do default(shared) private(i) schedule(static)
        do i = 1, QUERIES
            shared(i) = surface%eval(px(i), py(i))
        end do
        !$omp end parallel do

        differ = count(shared /= serial)
        call check(error, differ == 0, "a concurrent evaluation of one grid object differed from the serial one")
        if (allocated(error)) return
        far = count(abs(shared - sin(px)*sin(py)) > 1.0e-8_real64)
        call check(error, far == 0, "a concurrent grid evaluation strayed from sin(x)*sin(y) beyond the spline's error bound")

    end subroutine test_shared_grid_is_read_only

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

    !> On each of four threads at once, a masked table of 600 000 points is built and answers 600 000
    !! queries, and `pf_interp` answers the same queries over the same masked table, both under
    !! `method="linear"`.
    !!
    !! **The regression is a crash, not a wrong answer.** ifx builds the result of `pack` on the stack,
    !! and a caller's temporary for an explicit-shape function result too, and an OpenMP worker's stack
    !! is the runtime's own, a few megabytes by default whatever `ulimit -s` says: a masked table or a
    !! one-shot result of 600 000 doubles, 4.6 megabytes, ended the whole process with SIGSEGV on a
    !! worker. The mask drops every seventh point, whose ordinate is a NaN that must not be judged; every
    !! answer but the last query's, which lies beyond the table, is held to `2*x + 1`, which linear
    !! interpolation reproduces exactly on these integer knots and half-integer queries. The answers are
    !! checked in scalar loops, since an array expression of that size is a stack temporary under ifx in
    !! its own right. The team is asserted to hold more than one thread: a team of one exercises no
    !! worker's stack.
    subroutine test_large_tables_on_worker_threads(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        integer, parameter :: POINTS = 600000
        integer, parameter :: WORKERS = 4

        type(pf_interp_1d), allocatable :: slot(:)
        real(real64), allocatable       :: x(:), y(:), q(:), shot(:)
        logical, allocatable            :: keep(:)
        real(real64)                    :: want
        integer                         :: far(WORKERS), thread_of(WORKERS)
        integer                         :: i, w, threads_seen

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every table below is built on the main thread, " // &
                       "whose stack is the process's own, so 'fits on a worker thread's stack' would hold " // &
                       "because there was no worker")
        return
#endif
        allocate (x(POINTS), y(POINTS), q(POINTS), keep(POINTS), slot(WORKERS))
        do i = 1, POINTS
            x(i) = real(i - 1, real64)
            y(i) = 2.0_real64*x(i) + 1.0_real64
            q(i) = x(i) + 0.5_real64
            keep(i) = mod(i, 7) /= 0
            if (.not. keep(i)) y(i) = nan_value()
        end do
        call check(error, keep(1) .and. keep(POINTS), "the mask must keep both ends, or the answers below are not the line")
        if (allocated(error)) return

        far = 0
        thread_of = 0
        !$omp parallel do num_threads(WORKERS) default(shared) private(w, i, want, shot) schedule(static, 1)
        do w = 1, WORKERS
            thread_of(w) = thread_slot()
            call slot(w)%init(x, y, method="linear", is_valid=keep)
            do i = 1, POINTS - 1
                want = 2.0_real64*q(i) + 1.0_real64
                if (abs(slot(w)%eval(q(i)) - want) > 64.0_real64*epsilon(want)*want) far(w) = far(w) + 1
            end do
            shot = pf_interp(x, y, q, method="linear", is_valid=keep)
            do i = 1, POINTS - 1
                want = 2.0_real64*q(i) + 1.0_real64
                if (abs(shot(i) - want) > 64.0_real64*epsilon(want)*want) far(w) = far(w) + 1
            end do
        end do
        !$omp end parallel do

        threads_seen = 0
        do w = 1, WORKERS
            if (all(thread_of(:w - 1) /= thread_of(w))) threads_seen = threads_seen + 1
        end do
        call check(error, threads_seen > 1, "the four builds ran on a team of one, so no worker's stack was exercised")
        if (allocated(error)) return
        call check(error, all(far == 0), "a large masked table on a worker thread answered off the line it samples")

    end subroutine test_large_tables_on_worker_threads

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
