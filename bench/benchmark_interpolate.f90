!> Fixtures and the stopwatch for `benchmark_interpolate`, as module procedures: the tables, grids and
!! queries it times, the closed forms its error column measures against, and the watch every timed
!! figure is read from.
!!
!! Every table samples `sin(x)` over `[0, 2 pi]`, and every grid `sin(x) sin(y)` over `[0, 2 pi]` by
!! `[0, pi]`. Both have an amplitude of one, so the largest distance of an answer from them is a
!! relative error as it stands; and both have zero curvature on every edge, which is what the natural
!! spline's end condition assumes, so a cubic row shows the method's own error rather than its end
!! condition's.
!!
!! Every draw comes from `parquet_random` under one fixed seed, so two runs time the same tables and
!! the same queries. The arrays are allocatable throughout: a million-point table is eight megabytes,
!! more than a default stack holds.
module benchmark_interpolate_kernels

    use iso_fortran_env, only : real64, int32, int64
    use parquet_random, only : pf_random_fill_draws
    use parquet_sampling, only : pf_random_permutation

    implicit none
    private

    public :: fixture_1d, fixture_2d, grid_values
    public :: even_axis, uneven_axis, sorted_queries, uniform_queries, shuffled
    public :: largest_error_1d, largest_error_2d, differing

    !> `pi`.
    real(real64), parameter, public :: PI = acos(-1.0_real64)
    !> Seconds a timed lap lasts at least. A lap repeats its job until it has run this long, since a
    !! clock counting microseconds resolves a shorter one too coarsely.
    real(real64), parameter, public :: MIN_LAP = 0.02_real64
    !> The largest number of repetitions a lap is doubled to, whatever the clock says.
    integer, parameter :: MAX_REPS = 2**30
    !> The seed of every draw.
    integer(int64), parameter :: SEED = 20260917_int64

    !> Times a job in laps and keeps the fastest.
    !!
    !! ```
    !! call watch%start(rounds)
    !! do
    !!     call watch%lap(done)
    !!     if (done) exit
    !!     do r = 1, watch%reps
    !!         ! the job
    !!     end do
    !! end do
    !! seconds = watch%per_rep()
    !! ```
    !!
    !! The first laps calibrate: each doubles the repetitions until one lap lasts `MIN_LAP`, which also
    !! touches every page the job writes before a lap that counts. Then `rounds` laps are timed and the
    !! fastest is kept, never the mean, because the slow laps are the machine's other work rather than
    !! the job's.
    type, public :: stopwatch
        integer      :: reps = 1                !! repetitions of the job per lap
        integer      :: rounds = 1              !! laps to time once calibrated
        integer      :: timed = 0               !! laps timed so far
        logical      :: calibrating = .true.    !! still doubling `reps`
        logical      :: running = .false.       !! a lap has started
        real(real64) :: started = 0.0_real64    !! when the running lap started, in seconds
        real(real64) :: best = huge(1.0_real64) !! the fastest timed lap, in seconds
    contains
        procedure :: start => stopwatch_start     !! Resets the watch for a new job.
        procedure :: lap => stopwatch_lap         !! Ends the running lap and starts the next one.
        procedure :: per_rep => stopwatch_per_rep !! The fastest lap's seconds per repetition.
    end type stopwatch

contains

    !> Seconds on an arbitrary origin, from the wall clock.
    function clock() result(t)
        real(real64) :: t !! seconds, on an arbitrary origin

        integer(int64) :: count, rate

        call system_clock(count, rate)
        if (rate <= 0) error stop "benchmark_interpolate: this processor has no clock to time with"
        t = real(count, real64)/real(rate, real64)

    end function clock

    !> Resets the watch for a new job.
    subroutine stopwatch_start(this, rounds)
        class(stopwatch), intent(inout) :: this   !! the watch
        integer, intent(in)             :: rounds !! laps to time once calibrated

        this%reps = 1
        this%rounds = max(1, rounds)
        this%timed = 0
        this%calibrating = .true.
        this%running = .false.
        this%best = huge(1.0_real64)

    end subroutine stopwatch_start

    !> Ends the running lap, if one is, and starts the next, unless the job has been timed often enough.
    subroutine stopwatch_lap(this, done)
        class(stopwatch), intent(inout) :: this !! the watch
        logical, intent(out)            :: done !! every timed lap has run; start no other

        real(real64) :: elapsed

        done = .false.
        if (this%running) then
            elapsed = clock() - this%started
            if (this%calibrating) then
                if (elapsed >= MIN_LAP .or. this%reps >= MAX_REPS) then
                    this%calibrating = .false.
                else
                    this%reps = 2*this%reps
                end if
            else
                this%best = min(this%best, elapsed)
                this%timed = this%timed + 1
                if (this%timed >= this%rounds) then
                    this%running = .false.
                    done = .true.
                    return
                end if
            end if
        end if
        this%running = .true.
        this%started = clock()

    end subroutine stopwatch_lap

    !> The fastest lap's seconds per repetition of the job.
    pure function stopwatch_per_rep(this) result(seconds)
        class(stopwatch), intent(in) :: this    !! a watch whose laps have all run
        real(real64)                 :: seconds !! seconds per repetition

        seconds = this%best/real(this%reps, real64)

    end function stopwatch_per_rep

    !> `sin(x)`, the closed form every table samples.
    pure elemental function fixture_1d(x) result(f)
        real(real64), intent(in) :: x !! the abscissa
        real(real64)             :: f !! the closed form there

        f = sin(x)

    end function fixture_1d

    !> `sin(x) sin(y)`, the closed form every grid samples.
    pure elemental function fixture_2d(x, y) result(f)
        real(real64), intent(in) :: x !! the coordinate along `x`
        real(real64), intent(in) :: y !! the coordinate along `y`
        real(real64)             :: f !! the closed form there

        f = sin(x)*sin(y)

    end function fixture_2d

    !> The values of `sin(x) sin(y)` on the grid `x` by `y`, `z(i, j)` at `(x(i), y(j))`.
    pure function grid_values(x, y) result(z)
        real(real64), intent(in)  :: x(:)    !! the grid lines along `x`
        real(real64), intent(in)  :: y(:)    !! the grid lines along `y`
        real(real64), allocatable :: z(:, :) !! the values

        integer :: i, j

        allocate (z(size(x, kind=int64), size(y, kind=int64)))
        do j = 1, size(y)
            do i = 1, size(x)
                z(i, j) = fixture_2d(x(i), y(j))
            end do
        end do

    end function grid_values

    !> `n` evenly spaced points from `a` to `b`, both ends exact.
    pure function even_axis(n, a, b) result(x)
        integer, intent(in)       :: n    !! points, at least two
        real(real64), intent(in)  :: a    !! the first point
        real(real64), intent(in)  :: b    !! the last point, above `a`
        real(real64), allocatable :: x(:) !! the axis

        integer :: i

        allocate (x(n))
        do i = 1, n
            x(i) = a + (b - a)*(real(i - 1, real64)/real(n - 1, real64))
        end do
        x(n) = b

    end function even_axis

    !> `n` points from `a` to `b`, both ends exact, whose gaps are drawn uniformly between a half and one
    !! and a half of their mean: the span of `even_axis`, and far too uneven to be bracketed by arithmetic.
    pure function uneven_axis(n, a, b, stream) result(x)
        integer, intent(in)       :: n      !! points, at least two
        real(real64), intent(in)  :: a      !! the first point
        real(real64), intent(in)  :: b      !! the last point, above `a`
        integer, intent(in)       :: stream !! the stream of draws; one per fixture
        real(real64), allocatable :: x(:)   !! the axis

        real(real64), allocatable :: gap(:)
        real(real64)              :: total
        integer                   :: i

        allocate (gap(n - 1), x(n))
        call pf_random_fill_draws(SEED, stream, gap)
        ! The partial sums of the gaps, each at least half a mean gap above the one before, scaled onto
        ! the span: the axis increases strictly.
        x(1) = 0.0_real64
        do i = 2, n
            x(i) = x(i - 1) + (0.5_real64 + gap(i - 1))
        end do
        total = x(n)
        do i = 2, n - 1
            x(i) = a + (b - a)*(x(i)/total)
        end do
        x(1) = a
        x(n) = b

    end function uneven_axis

    !> `q` points from `a` to `b` in ascending order, one drawn uniformly inside each of `q` equal slices
    !! of the span: a uniform sample of the span that is sorted without a sort.
    pure function sorted_queries(q, a, b, stream) result(xq)
        integer, intent(in)       :: q      !! the number of queries
        real(real64), intent(in)  :: a      !! the start of the span
        real(real64), intent(in)  :: b      !! the end of the span
        integer, intent(in)       :: stream !! the stream of draws; one per fixture
        real(real64), allocatable :: xq(:)  !! the queries, ascending

        integer :: k

        allocate (xq(q))
        call pf_random_fill_draws(SEED, stream, xq)
        do k = 1, q
            xq(k) = a + (b - a)*((real(k - 1, real64) + xq(k))/real(q, real64))
        end do

    end function sorted_queries

    !> `q` points drawn uniformly from `a` to `b`, in the order drawn.
    pure function uniform_queries(q, a, b, stream) result(xq)
        integer, intent(in)       :: q      !! the number of queries
        real(real64), intent(in)  :: a      !! the start of the span
        real(real64), intent(in)  :: b      !! the end of the span
        integer, intent(in)       :: stream !! the stream of draws; one per fixture
        real(real64), allocatable :: xq(:)  !! the queries

        allocate (xq(q))
        call pf_random_fill_draws(SEED, stream, xq)
        xq = a + (b - a)*xq

    end function uniform_queries

    !> The points `xs` in a random order.
    function shuffled(xs, stream) result(xr)
        real(real64), intent(in)  :: xs(:)  !! the points
        integer, intent(in)       :: stream !! selects the permutation; one per fixture
        real(real64), allocatable :: xr(:)  !! the same points, permuted

        integer(int32), allocatable :: perm(:)
        integer                     :: k

        allocate (perm(size(xs, kind=int64)), xr(size(xs, kind=int64)))
        ! One thread: the permutation is the same on any number, and no team is left spinning beside
        ! the timings that follow.
        call pf_random_permutation(perm, SEED + int(stream, int64), threads=1)
        do k = 1, size(xs)
            xr(k) = xs(perm(k))
        end do

    end function shuffled

    !> The largest distance of the answers `v` from `sin(xq)`; NaN when an answer is NaN.
    pure function largest_error_1d(v, xq) result(e)
        real(real64), intent(in) :: v(:)  !! the answers
        real(real64), intent(in) :: xq(:) !! where each was asked for
        real(real64)             :: e     !! the largest distance

        real(real64) :: d
        integer      :: k

        e = 0.0_real64
        do k = 1, size(v)
            d = abs(v(k) - fixture_1d(xq(k)))
            ! Written so that a NaN answer is carried rather than skipped, as `max` may skip it.
            if (.not. (d <= e)) e = d
        end do

    end function largest_error_1d

    !> The largest distance of the answers `v` from `sin(xq) sin(yq)`; NaN when an answer is NaN.
    pure function largest_error_2d(v, xq, yq) result(e)
        real(real64), intent(in) :: v(:)  !! the answers
        real(real64), intent(in) :: xq(:) !! the coordinate along `x` each was asked for at
        real(real64), intent(in) :: yq(:) !! the coordinate along `y` each was asked for at
        real(real64)             :: e     !! the largest distance

        real(real64) :: d
        integer      :: k

        e = 0.0_real64
        do k = 1, size(v)
            d = abs(v(k) - fixture_2d(xq(k), yq(k)))
            if (.not. (d <= e)) e = d
        end do

    end function largest_error_2d

    !> How many elements of `a` and `b` are not the same bits, a NaN counting as differing.
    pure function differing(a, b) result(n)
        real(real64), intent(in) :: a(:) !! one set of answers
        real(real64), intent(in) :: b(:) !! another, of the same size
        integer                  :: n    !! the elements that differ

        integer :: k

        n = 0
        do k = 1, size(a)
            if (.not. (a(k) == b(k))) n = n + 1
        end do

    end function differing

end module benchmark_interpolate_kernels

!> What `parquet_interpolate` costs: building a table's interpolant against its size (`build`), a
!> query against the table's size, spacing and query order (`eval`), a grid's build and query
!> (`grid`), and the one-shot form against an object (`oneshot`).
!!
!! Every timed row reports `relerr` beside its timing, because the fastest interpolant is the one
!! bracketing the wrong segment. `eval` and `grid` also confirm through
!! `parquet_debug_interp_force_search` that an evenly spaced table is on the arithmetic path and an
!! uneven one is not, and count the queries the same even table answers in other bits when forced to
!! bisect; `oneshot` counts the `pf_interp` answers that differ in any bit from the object's. The
!! library promises none, and the program exits nonzero on any.
!!
!! Usage and configuration are in `bench/benchmark_interpolate.sh`, which is how this program is meant
!! to be run: it is the half that verifies the optimisation flags before believing a number.
program benchmark_interpolate

    use iso_fortran_env, only : real64, int64, error_unit
    use parquet_interpolate, only : pf_interp_1d, pf_interp_2d, pf_interp, parquet_debug_interp_force_search
    use benchmark_interpolate_kernels

    implicit none

    !> The methods of a table, in the order every mode prints them.
    character(len=6), parameter :: METHODS_1D(3) = [character(len=6) :: "linear", "cubic", "pchip"]
    !> The methods of a grid.
    character(len=6), parameter :: METHODS_2D(2) = [character(len=6) :: "linear", "cubic"]

    character(len=32) :: mode
    integer           :: rounds, queries, mismatches

    call parse_arguments(mode, rounds, queries)

    mismatches = 0
    select case (trim(mode))
    case ("build")
        call run_build(rounds)
    case ("eval")
        call run_eval(rounds, queries, mismatches)
    case ("grid")
        call run_grid(rounds, queries, mismatches)
    case ("oneshot")
        call run_oneshot(rounds, mismatches)
    case default
        write (error_unit, '(a)') "benchmark_interpolate: unknown mode '"//trim(mode)//"'"
        error stop 1
    end select

    if (mismatches /= 0) then
        write (error_unit, '(a,i0,a)') "benchmark_interpolate: ", mismatches, " answers differ between two "// &
            "routes the library promises answer the same bits; no timing above measures what it says"
        error stop 1
    end if

contains

    !> Reads the `--key=value` command line, applying each default when the flag is absent.
    subroutine parse_arguments(mode, rounds, queries)
        character(len=*), intent(out) :: mode    !! which measurement to run
        integer, intent(out)          :: rounds  !! timed laps per figure; the fastest is kept
        integer, intent(out)          :: queries !! queries per lap in `eval` and `grid`

        character(len=64) :: arg, val
        integer           :: k, eq

        mode = "build"
        rounds = 5
        queries = 1000000

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                write (error_unit, '(a)') "benchmark_interpolate: unrecognised argument '"//trim(arg)//"'"
                error stop 1
            end if
            val = arg(eq + 1:)
            select case (arg(1:eq - 1))
            case ("--mode")
                mode = trim(val)
            case ("--rounds")
                read (val, *) rounds
            case ("--queries")
                read (val, *) queries
            case default
                write (error_unit, '(a)') "benchmark_interpolate: unknown option '"//trim(arg(1:eq - 1))//"'"
                write (error_unit, '(a)') "Usage: benchmark_interpolate " &
                    //"[--mode=build|eval|grid|oneshot] [--rounds=N] [--queries=N]"
                error stop 1
            end select
        end do

        if (rounds < 1) error stop "benchmark_interpolate: --rounds must be >= 1"
        if (queries < 1) error stop "benchmark_interpolate: --queries must be >= 1"

    end subroutine parse_arguments

    ! ---- build ----------------------------------------------------------------------------------

    !> `%init` against the size of the table, per method, on an even table and on an uneven one.
    subroutine run_build(rounds)
        integer, intent(in) :: rounds !! timed laps per figure

        integer, parameter :: SMALLEST = 64, LARGEST = 1048576, ERROR_POINTS = 10000

        real(real64), allocatable :: x(:), y(:), xq(:)
        real(real64)              :: t_even, t_uneven, e_even, e_uneven
        integer                   :: m, n

        print '(a)', "=== pf_interp_1d%init: nanoseconds per point ==="
        print '(a,i0,a,i0,a)', "fastest of ", rounds, " laps, each repeating the build for at least ", &
            nint(1000*MIN_LAP), " ms"
        print '(a,i0,a)', "relerr: the largest error at ", ERROR_POINTS, " points spread across the table, against sin(x)"
        print '(a)', ""
        print '(a)', "  method          n       even     uneven  relerr even  relerr uneven"

        xq = sorted_queries(ERROR_POINTS, 0.0_real64, 2*PI, 1)
        do m = 1, size(METHODS_1D)
            n = SMALLEST
            do while (n <= LARGEST)
                x = even_axis(n, 0.0_real64, 2*PI)
                y = fixture_1d(x)
                call time_build(x, y, trim(METHODS_1D(m)), rounds, xq, t_even, e_even)
                x = uneven_axis(n, 0.0_real64, 2*PI, 2)
                y = fixture_1d(x)
                call time_build(x, y, trim(METHODS_1D(m)), rounds, xq, t_uneven, e_uneven)
                print '(2x,a6,i11,2f11.2,es13.2,es15.2)', METHODS_1D(m), n, 1.0e9_real64*t_even/n, &
                    1.0e9_real64*t_uneven/n, e_even, e_uneven
                n = 2*n
            end do
            print '(a)', ""
        end do

    end subroutine run_build

    !> Times `%init` over one table, then measures the error of the interpolant it built.
    subroutine time_build(x, y, method, rounds, xq, seconds, relerr)
        real(real64), intent(in)     :: x(:)    !! the abscissae
        real(real64), intent(in)     :: y(:)    !! the ordinates
        character(len=*), intent(in) :: method  !! the method token
        integer, intent(in)          :: rounds  !! timed laps
        real(real64), intent(in)     :: xq(:)   !! where the error is measured
        real(real64), intent(out)    :: seconds !! the fastest lap's seconds per build
        real(real64), intent(out)    :: relerr  !! the largest error at `xq`

        type(pf_interp_1d) :: c
        type(stopwatch)    :: watch
        logical            :: done
        integer            :: r

        call watch%start(rounds)
        do
            call watch%lap(done)
            if (done) exit
            do r = 1, watch%reps
                call c%init(x, y, method=method)
            end do
        end do
        seconds = watch%per_rep()
        relerr = largest_error_1d(c%eval(xq), xq)

    end subroutine time_build

    ! ---- eval -----------------------------------------------------------------------------------

    !> `%eval` against the size of the table, per method, on three tables and in two query orders.
    subroutine run_eval(rounds, queries, mismatches)
        integer, intent(in)    :: rounds     !! timed laps per figure
        integer, intent(in)    :: queries    !! queries per lap
        integer, intent(inout) :: mismatches !! answers the forced bisection gave in other bits

        integer, parameter :: SIZES(3) = [16, 1024, 65536]

        type(pf_interp_1d)        :: c, forced
        real(real64), allocatable :: x(:), y(:), xs(:), xr(:), vs(:), vr(:), fs(:), fr(:)
        real(real64)              :: t_shuffled, t_sorted
        character(len=12)         :: miss
        logical                   :: was_uniform
        integer                   :: i, m, n, differ

        print '(a)', "=== pf_interp_1d%eval: nanoseconds per query ==="
        print '(a,i0,a,i0,a)', "fastest of ", rounds, " laps over ", queries, &
            " queries drawn uniformly across the table; each lap repeats"
        print '(a,i0,a)', "  them for at least ", nint(1000*MIN_LAP), " ms"
        print '(a)', "table: arithmetic is evenly spaced, and bracketed by arithmetic; bisection is the same object"
        print '(a)', "  forced to bisect by parquet_debug_interp_force_search; uneven has random gaps, and bisects"
        print '(a)', "relerr: the largest error of the answers timed, against sin(x); mismatches: the answers the"
        print '(a)', "  forced bisection gave in other bits than the arithmetic did, in either order"
        print '(a)', ""
        print '(a)', "      n  method  table        shuffled     sorted    relerr  mismatches"

        xs = sorted_queries(queries, 0.0_real64, 2*PI, 3)
        xr = shuffled(xs, 4)
        allocate (vs(size(xs, kind=int64)), vr(size(xs, kind=int64)), fs(size(xs, kind=int64)), &
                  fr(size(xs, kind=int64)))

        do i = 1, size(SIZES)
            n = SIZES(i)
            do m = 1, size(METHODS_1D)
                x = even_axis(n, 0.0_real64, 2*PI)
                y = fixture_1d(x)
                call c%init(x, y, method=trim(METHODS_1D(m)))
                forced = c
                call parquet_debug_interp_force_search(forced, was_uniform)
                if (.not. was_uniform) then
                    write (error_unit, '(a,i0,a)') "benchmark_interpolate: the even table of ", n, &
                        " points is not bracketed by arithmetic, so its rows would time bisection twice"
                    error stop 1
                end if
                call time_eval(c, xr, rounds, t_shuffled, vr)
                call time_eval(c, xs, rounds, t_sorted, vs)
                print '(i7,2x,a6,2x,a10,2f11.2,es10.2)', n, METHODS_1D(m), "arithmetic", &
                    1.0e9_real64*t_shuffled, 1.0e9_real64*t_sorted, largest_error_1d(vr, xr)

                call time_eval(forced, xr, rounds, t_shuffled, fr)
                call time_eval(forced, xs, rounds, t_sorted, fs)
                differ = differing(fr, vr) + differing(fs, vs)
                mismatches = mismatches + differ
                write (miss, '(i12)') differ
                print '(i7,2x,a6,2x,a10,2f11.2,es10.2,a)', n, METHODS_1D(m), "bisection ", &
                    1.0e9_real64*t_shuffled, 1.0e9_real64*t_sorted, largest_error_1d(fr, xr), miss

                x = uneven_axis(n, 0.0_real64, 2*PI, 2)
                y = fixture_1d(x)
                call c%init(x, y, method=trim(METHODS_1D(m)))
                forced = c
                call parquet_debug_interp_force_search(forced, was_uniform)
                if (was_uniform) then
                    write (error_unit, '(a,i0,a)') "benchmark_interpolate: the uneven table of ", n, &
                        " points is bracketed by arithmetic, so its rows would not time bisection"
                    error stop 1
                end if
                call time_eval(c, xr, rounds, t_shuffled, vr)
                call time_eval(c, xs, rounds, t_sorted, vs)
                print '(i7,2x,a6,2x,a10,2f11.2,es10.2)', n, METHODS_1D(m), "uneven    ", &
                    1.0e9_real64*t_shuffled, 1.0e9_real64*t_sorted, largest_error_1d(vr, xr)
            end do
            print '(a)', ""
        end do

    end subroutine run_eval

    !> Times one elemental `%eval` over a set of queries.
    subroutine time_eval(c, xq, rounds, seconds, v)
        type(pf_interp_1d), intent(in) :: c       !! a built interpolant
        real(real64), intent(in)       :: xq(:)   !! the queries
        integer, intent(in)            :: rounds  !! timed laps
        real(real64), intent(out)      :: seconds !! the fastest lap's seconds per query
        real(real64), intent(out)      :: v(:)    !! the answers, one per query

        type(stopwatch) :: watch
        logical         :: done
        integer         :: r

        call watch%start(rounds)
        do
            call watch%lap(done)
            if (done) exit
            do r = 1, watch%reps
                v = c%eval(xq)
            end do
        end do
        seconds = watch%per_rep()/real(size(xq), real64)

    end subroutine time_eval

    ! ---- grid -----------------------------------------------------------------------------------

    !> `%init` and `%eval` on a grid against its size, per method, on three kinds of axes.
    subroutine run_grid(rounds, queries, mismatches)
        integer, intent(in)    :: rounds     !! timed laps per figure
        integer, intent(in)    :: queries    !! queries per lap
        integer, intent(inout) :: mismatches !! answers the forced bisection gave in other bits

        integer, parameter :: SIZES(3) = [128, 512, 1024]

        type(pf_interp_2d)        :: g, forced
        real(real64), allocatable :: x(:), y(:), z(:, :), xq(:), yq(:), v(:), f(:)
        real(real64)              :: t_init, t_eval
        character(len=12)         :: miss
        character(len=11)         :: label
        logical                   :: x_uniform, y_uniform
        integer                   :: i, m, n, differ

        print '(a)', "=== pf_interp_2d: %init in nanoseconds per node, %eval in nanoseconds per query ==="
        print '(a,i0,a,i0,a)', "fastest of ", rounds, " laps over ", queries, &
            " queries drawn uniformly across the grid; each lap repeats"
        print '(a,i0,a)', "  its job for at least ", nint(1000*MIN_LAP), " ms"
        print '(a)', "axes: arithmetic are evenly spaced; bisection is the same object forced to bisect along both"
        print '(a)', "  axes by parquet_debug_interp_force_search; uneven have random gaps"
        print '(a)', "relerr: the largest error of the answers timed, against sin(x) sin(y); mismatches: the answers"
        print '(a)', "  the forced bisection gave in other bits than the arithmetic did"
        print '(a)', ""
        print '(a)', "       grid  method  axes             init       eval    relerr  mismatches"

        xq = uniform_queries(queries, 0.0_real64, 2*PI, 5)
        yq = uniform_queries(queries, 0.0_real64, PI, 6)
        allocate (v(size(xq, kind=int64)), f(size(xq, kind=int64)))

        do i = 1, size(SIZES)
            n = SIZES(i)
            write (label, '(i0,"x",i0)') n, n
            do m = 1, size(METHODS_2D)
                x = even_axis(n, 0.0_real64, 2*PI)
                y = even_axis(n, 0.0_real64, PI)
                z = grid_values(x, y)
                call time_grid_build(x, y, z, trim(METHODS_2D(m)), rounds, t_init, g)
                forced = g
                call parquet_debug_interp_force_search(forced, x_uniform, y_uniform)
                if (.not. (x_uniform .and. y_uniform)) then
                    write (error_unit, '(a,a,a)') "benchmark_interpolate: the even ", trim(label), &
                        " grid is not bracketed by arithmetic along both axes, so its rows would time bisection twice"
                    error stop 1
                end if
                call time_grid_eval(g, xq, yq, rounds, t_eval, v)
                print '(a11,2x,a6,2x,a10,2f11.2,es10.2)', adjustr(label), METHODS_2D(m), "arithmetic", &
                    1.0e9_real64*t_init/real(n, real64)**2, 1.0e9_real64*t_eval, largest_error_2d(v, xq, yq)

                call time_grid_eval(forced, xq, yq, rounds, t_eval, f)
                differ = differing(f, v)
                mismatches = mismatches + differ
                write (miss, '(i12)') differ
                print '(a11,2x,a6,2x,a10,a11,f11.2,es10.2,a)', adjustr(label), METHODS_2D(m), "bisection ", "-", &
                    1.0e9_real64*t_eval, largest_error_2d(f, xq, yq), miss

                x = uneven_axis(n, 0.0_real64, 2*PI, 7)
                y = uneven_axis(n, 0.0_real64, PI, 8)
                z = grid_values(x, y)
                call time_grid_build(x, y, z, trim(METHODS_2D(m)), rounds, t_init, g)
                forced = g
                call parquet_debug_interp_force_search(forced, x_uniform, y_uniform)
                if (x_uniform .or. y_uniform) then
                    write (error_unit, '(a,a,a)') "benchmark_interpolate: the uneven ", trim(label), &
                        " grid is bracketed by arithmetic along an axis, so its rows would not time bisection"
                    error stop 1
                end if
                call time_grid_eval(g, xq, yq, rounds, t_eval, v)
                print '(a11,2x,a6,2x,a10,2f11.2,es10.2)', adjustr(label), METHODS_2D(m), "uneven    ", &
                    1.0e9_real64*t_init/real(n, real64)**2, 1.0e9_real64*t_eval, largest_error_2d(v, xq, yq)
            end do
            print '(a)', ""
        end do

    end subroutine run_grid

    !> Times `%init` over one grid, and hands back the interpolant it built.
    subroutine time_grid_build(x, y, z, method, rounds, seconds, g)
        real(real64), intent(in)        :: x(:)    !! the grid lines along `x`
        real(real64), intent(in)        :: y(:)    !! the grid lines along `y`
        real(real64), intent(in)        :: z(:, :) !! the values
        character(len=*), intent(in)    :: method  !! the method token
        integer, intent(in)             :: rounds  !! timed laps
        real(real64), intent(out)       :: seconds !! the fastest lap's seconds per build
        type(pf_interp_2d), intent(out) :: g       !! the interpolant, built

        type(stopwatch) :: watch
        logical         :: done
        integer         :: r

        call watch%start(rounds)
        do
            call watch%lap(done)
            if (done) exit
            do r = 1, watch%reps
                call g%init(x, y, z, method=method)
            end do
        end do
        seconds = watch%per_rep()

    end subroutine time_grid_build

    !> Times one elemental `%eval` over a set of grid queries.
    subroutine time_grid_eval(g, xq, yq, rounds, seconds, v)
        type(pf_interp_2d), intent(in) :: g       !! a built interpolant
        real(real64), intent(in)       :: xq(:)   !! the queries' coordinates along `x`
        real(real64), intent(in)       :: yq(:)   !! the queries' coordinates along `y`
        integer, intent(in)            :: rounds  !! timed laps
        real(real64), intent(out)      :: seconds !! the fastest lap's seconds per query
        real(real64), intent(out)      :: v(:)    !! the answers, one per query

        type(stopwatch) :: watch
        logical         :: done
        integer         :: r

        call watch%start(rounds)
        do
            call watch%lap(done)
            if (done) exit
            do r = 1, watch%reps
                v = g%eval(xq, yq)
            end do
        end do
        seconds = watch%per_rep()/real(size(xq), real64)

    end subroutine time_grid_eval

    ! ---- oneshot --------------------------------------------------------------------------------

    !> `pf_interp` against an object, answering a growing number of queries on one table.
    subroutine run_oneshot(rounds, mismatches)
        integer, intent(in)    :: rounds     !! timed laps per figure
        integer, intent(inout) :: mismatches !! `pf_interp` answers in other bits than the object's

        integer, parameter :: POINTS = 1024
        integer, parameter :: COUNTS(3) = [1, 100, 10000]

        type(pf_interp_1d)        :: c
        type(stopwatch)           :: watch
        real(real64), allocatable :: x(:), y(:), xq(:), v_each(:), v_all(:), v_object(:)
        real(real64)              :: t_each, t_all, t_object
        logical                   :: done
        integer                   :: i, k, nq, r, differ

        print '(a)', "=== pf_interp against an object: microseconds to answer nq queries ==="
        print '(a,i0,a,i0,a)', "a table of ", POINTS, " points with random gaps, method=""cubic""; fastest of ", rounds, &
            " laps,"
        print '(a,i0,a)', "  each repeating its job for at least ", nint(1000*MIN_LAP), " ms"
        print '(a)', "each: one pf_interp call per query; all: one pf_interp call for every query; object: one"
        print '(a)', "  %init, then one %eval for every query"
        print '(a)', "relerr: the largest error of the object's answers, against sin(x); mismatches: the answers"
        print '(a)', "  either pf_interp form gave in other bits than the object did"
        print '(a)', ""
        print '(a)', "     nq          each           all        object  each/object   all/object    relerr  mismatches"

        x = uneven_axis(POINTS, 0.0_real64, 2*PI, 9)
        y = fixture_1d(x)
        do i = 1, size(COUNTS)
            nq = COUNTS(i)
            xq = uniform_queries(nq, 0.0_real64, 2*PI, 10)
            if (allocated(v_each)) deallocate (v_each, v_all, v_object)
            allocate (v_each(nq), v_all(nq), v_object(nq))

            call watch%start(rounds)
            do
                call watch%lap(done)
                if (done) exit
                do r = 1, watch%reps
                    do k = 1, nq
                        v_each(k) = pf_interp(x, y, xq(k), method="cubic")
                    end do
                end do
            end do
            t_each = watch%per_rep()

            call watch%start(rounds)
            do
                call watch%lap(done)
                if (done) exit
                do r = 1, watch%reps
                    v_all = pf_interp(x, y, xq, method="cubic")
                end do
            end do
            t_all = watch%per_rep()

            call watch%start(rounds)
            do
                call watch%lap(done)
                if (done) exit
                do r = 1, watch%reps
                    call c%init(x, y, method="cubic")
                    v_object = c%eval(xq)
                end do
            end do
            t_object = watch%per_rep()

            differ = differing(v_each, v_object) + differing(v_all, v_object)
            mismatches = mismatches + differ
            print '(i7,3f14.3,2f13.1,es10.2,i12)', nq, 1.0e6_real64*t_each, 1.0e6_real64*t_all, &
                1.0e6_real64*t_object, t_each/t_object, t_all/t_object, largest_error_1d(v_object, xq), differ
        end do

    end subroutine run_oneshot

end program benchmark_interpolate
