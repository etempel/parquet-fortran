!> What `parquet_kde` costs: `pf_kde`'s exact queries against the bandwidth, `pf_kde_grid`'s deposit
!> against the cells one kernel reaches, a merge against the grid's size, how fast the grid
!> converges to the exact estimate as its cells shrink, what the adaptive kernel adds to a fit
!> and to a query, what each bandwidth rule adds to a fit, what a draw from either form costs, and
!> how the threaded forms scale.
!!
!! Driven by `bench/benchmark_kde.sh`, whose header carries the usage and what each column means.
!! Every timed job is repeated until one lap lasts at least `MIN_LAP` seconds and the fastest of
!! `--rounds` laps is kept; every buffer is written before the first lap, and each job's answer is
!! folded into a checksum outside the timed region and printed, so that no compiler can drop the
!! work. The grid mode refuses to report a deposit that did not conserve its weight: a faster
!! deposit that loses weight is not the same computation made cheaper. The threads mode refuses to
!! report a speed-up whose answer moved: `%pdf` and `%sample` must give the serial bits at every
!! thread count, and `%add` must agree with the serial deposit to rounding.
program benchmark_kde

    use iso_fortran_env, only : real64, int64, error_unit
    use parquet_kde, only : pf_kde, pf_kde_grid, parquet_debug_kde_fit_nanos, parquet_debug_kde_threads_used, &
        parquet_debug_set_kde_sample_tries
    use parquet_argsort, only : pf_argsort
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads
#endif

    implicit none

    !> The kernels, in the module's order.
    character(len=12), parameter :: KERNELS(4) = [character(len=12) :: "gaussian", "epanechnikov", &
        "bspline", "box"]
    !> Each kernel's reach in standard deviations, as the module's kernel table has it.
    real(real64), parameter :: RADIUS(4) = [5.0_real64, sqrt(5.0_real64), 2.0_real64*sqrt(3.0_real64), &
        sqrt(3.0_real64)]
    !> The shortest lap worth timing, in seconds.
    real(real64), parameter :: MIN_LAP = 0.05_real64
    !> How far a grid's total weight may miss the points', relative. Rounding over a million
    !> deposits of one size reaches `1e-13` (the box kernel's, all equal); a deposit normalised by the
    !> midpoint rule instead of by its own sum misses by `1e-8` for the Gaussian and far more for the
    !> others.
    real(real64), parameter :: MASS_GATE = 1.0e-10_real64

    !> How much dearer a bulk `%pdf` may be per point on a grid of sixteen times the cells before
    !! the run is called a failure: the query reads what `%finish` stored, so the true ratio is one
    !! and anything near it passes, while a return to a per-call `O(cells)` sum -- which cost
    !! hundreds of times more at the larger count -- does not. Loose on purpose: this runs on a
    !! machine with other work on it.
    real(real64), parameter :: PDF_COST_GATE = 2.0_real64

    character(len=32) :: mode
    integer :: rounds, failures
    integer(int64) :: npoints, nqueries

    call parse_arguments(mode, rounds, npoints, nqueries)
    failures = 0
    select case (trim(mode))
    case ("evaluate")
        call run_evaluate(rounds, npoints, nqueries)
    case ("grid")
        call run_grid(rounds, npoints, failures)
    case ("binned")
        call run_binned(rounds, npoints, failures)
    case ("accuracy")
        call run_accuracy(npoints, nqueries)
    case ("adaptive")
        call run_adaptive(rounds, npoints, nqueries)
    case ("rules")
        call run_rules(rounds, npoints)
    case ("sample")
        call run_sample(rounds, npoints, nqueries)
    case ("threads")
        call run_threads(rounds, npoints, nqueries, failures)
    case ("boundary")
        call run_boundary(rounds, npoints)
    case default
        write(error_unit, '(a)') "benchmark_kde: unknown mode '" // trim(mode) // "'"
        error stop 1
    end select
    if (failures /= 0) then
        write(error_unit, '(a,i0,a)') "benchmark_kde: ", failures, " answers failed their gate (a grid " // &
            "that lost weight, or a threaded answer that moved); no timing above measures what it says"
        error stop 1
    end if

contains

    !> Reads the `--key=value` command line, applying each default when the flag is absent.
    subroutine parse_arguments(mode, rounds, npoints, nqueries)
        character(len=*), intent(out) :: mode     !! which measurement to run
        integer, intent(out)          :: rounds   !! timed laps per figure; the fastest is kept
        integer(int64), intent(out)   :: npoints  !! the sample's size
        integer(int64), intent(out)   :: nqueries !! query points or draws per call

        character(len=64) :: arg, val
        integer :: k, eq

        mode = "evaluate"
        rounds = 5
        npoints = 100000_int64
        nqueries = 10000_int64
        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                write(error_unit, '(a)') "benchmark_kde: unrecognised argument '" // trim(arg) // "'"
                error stop 1
            end if
            val = arg(eq + 1:)
            select case (arg(1:eq - 1))
            case ("--mode")
                mode = trim(val)
            case ("--rounds")
                read(val, *) rounds
            case ("--points")
                read(val, *) npoints
            case ("--queries")
                read(val, *) nqueries
            case default
                write(error_unit, '(a)') "benchmark_kde: unknown option '" // trim(arg(1:eq - 1)) // "'"
                write(error_unit, '(a)') "Usage: benchmark_kde [--mode=evaluate|grid|accuracy|adaptive|" // &
                    "rules|sample|threads] [--rounds=N] [--points=N] [--queries=N]"
                error stop 1
            end select
        end do
        if (rounds < 1) error stop "benchmark_kde: --rounds must be >= 1"
        if (npoints < 2_int64) error stop "benchmark_kde: --points must be >= 2"
        if (nqueries < 1_int64) error stop "benchmark_kde: --queries must be >= 1"

    end subroutine parse_arguments

    !> Seconds on an arbitrary origin, from the wall clock.
    function clock() result(t)
        real(real64) :: t !! seconds

        integer(int64) :: count, rate

        call system_clock(count, rate)
        if (rate <= 0) error stop "benchmark_kde: this processor has no clock to time with"
        t = real(count, real64)/real(rate, real64)

    end function clock

    !> The sample: a pure function of the index over about `[-500, 500]`, dense at its two ends
    !> and thin in the middle, so that a query's window holds a very different number of points in
    !> different places.
    !> What the three boundary corrections cost, and where `"linear"` differs from the other two.
    !>
    !> On `Exp(1)`'s quantiles with `lower = 0` -- a density that is large at its bound, so that the
    !> boundary zone holds most of the mass -- at three sample sizes: `%fit`, `%pdf` and `%cdf`
    !> inside the zone and between the zones, `%quantile` inside the zone, and `%sample`. Under
    !> `"linear"` a query inside the zone integrates each nearby point's corrected kernel, so its
    !> cost follows the WINDOW, and `%fit` pays one such integral per point that reaches the zone:
    !> the columns are what feature_kde_boundary.md's cost model predicted, measured. The zone
    !> queries are taken at a few points only, since one of them can cost a tenth of a second at
    !> `1e5` points.
    subroutine run_boundary(rounds, n)
        integer, intent(in)        :: rounds !! timed laps per figure
        integer(int64), intent(in) :: n      !! the largest sample

        character(len=11), parameter :: METHODS(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), qz(:), qm(:), pz(:), out(:), draws(:)
        real(real64) :: h, reach, t0, t, fit_ms, pdf_us, cdf_z, cdf_m, quant_ms, draw_us, fb_us, dep_ns
        real(real64) :: checksum, hb_max
        real(real64), allocatable :: hb(:)
        integer(int64) :: sizes(3), nn, i, reps, r, nq, nqq, nd
        integer :: sz, mm, nc

        sizes = [max(1000_int64, n/100_int64), max(1000_int64, n/10_int64), n]
        checksum = 0.0_real64
        print '(a)', "=== the boundary corrections: what each costs ==="
        print '(a,i0,a)', "Exp(1) quantiles with lower = 0, Silverman's rule, fastest of ", rounds, " laps"
        print '(a)', "pdf/cdf in microseconds per query point, quantile in milliseconds per probability, " // &
            "sample in microseconds per draw"
        print '(a)', "zone: within one kernel reach of the bound; mid: two to six reaches from it"
        print '(a)', ""
        print '(a)', "       n  correction    fit ms     pdf us  cdf zone   cdf mid  quant ms   draw us" // &
            "    fb us   dep ns"
        do sz = 1, 3
            nn = sizes(sz)
            if (sz > 1) then
                if (nn <= sizes(sz - 1)) cycle
            end if
            call exp_sample(nn, x)
            ! The zone queries are few where one of them is dear, and the draws likewise.
            nq = 50_int64
            if (nn >= 100000_int64) nq = 20_int64
            if (nn >= 1000000_int64) nq = 5_int64
            nqq = 2_int64
            if (nn >= 100000_int64) nqq = 1_int64
            nd = 200_int64
            if (nn >= 100000_int64) nd = 50_int64
            if (allocated(qz)) deallocate(qz, qm, pz, out, draws)
            allocate(qz(nq), qm(nq), pz(nqq), out(max(nq, nqq)), draws(nd))
            out = 0.0_real64
            draws = 0.0_real64
            do mm = 1, 3
                ! The fit, timed on its own: under `"linear"` it also scans each zone and integrates
                ! one term per point that reaches it.
                reps = 1_int64
                do
                    t0 = clock()
                    do r = 1_int64, reps
                        call k%fit(x, rule="silverman", lower=0.0_real64, boundary=trim(METHODS(mm)), threads=1)
                    end do
                    t = clock() - t0
                    if (t >= MIN_LAP .or. reps >= 64_int64) exit
                    reps = 2_int64*reps
                end do
                fit_ms = 1000.0_real64*t/real(reps, real64)
                h = k%bandwidth()
                reach = 5.0_real64*h
                call spread(0.02_real64*reach, 0.98_real64*reach, qz)
                call spread(2.0_real64*reach, 6.0_real64*reach, qm)
                do i = 1_int64, nqq
                    pz(i) = 0.002_real64*real(i, real64)
                end do
                pdf_us = 1.0e6_real64*time_query(k, 1, qz, out(1:nq), rounds, checksum)/real(nq, real64)
                cdf_z = 1.0e6_real64*time_query(k, 2, qz, out(1:nq), rounds, checksum)/real(nq, real64)
                cdf_m = 1.0e6_real64*time_query(k, 2, qm, out(1:nq), rounds, checksum)/real(nq, real64)
                quant_ms = 1000.0_real64*time_query(k, 3, pz, out(1:nqq), rounds, checksum)/real(nqq, real64)
                draw_us = 1.0e6_real64*time_query(k, 4, pz, draws, rounds, checksum)/real(nd, real64)
                ! The same draws with every attempt refused, which sends each one to its fallback:
                ! the inversion of the zone's own integral, a quantile solve per draw.
                fb_us = 0.0_real64
                if (mm == 3 .and. nn <= 10000_int64) then
                    call parquet_debug_set_kde_sample_tries(0)
                    fb_us = 1.0e6_real64*time_query(k, 4, pz, draws(1:min(nd, 20_int64)), 2, checksum) &
                        /real(min(nd, 20_int64), real64)
                    call parquet_debug_set_kde_sample_tries(-1)
                end if
                ! The grid's deposit, per point, on a grid the correction's own rules accept.
                nc = 400
                reps = 1_int64
                do
                    t0 = clock()
                    do r = 1_int64, reps
                        call g%init(nc, 0.0_real64, maxval(x), h, lower=0.0_real64, boundary=trim(METHODS(mm)))
                        call g%add(x, threads=1)
                    end do
                    t = clock() - t0
                    if (t >= MIN_LAP .or. reps >= 16_int64) exit
                    reps = 2_int64*reps
                end do
                dep_ns = 1.0e9_real64*t/(real(reps, real64)*real(nn, real64))
                print '(i9,2x,a11,8f10.2)', nn, METHODS(mm), fit_ms, pdf_us, cdf_z, cdf_m, quant_ms, &
                    draw_us, fb_us, dep_ns
            end do
        end do
        print '(a)', ""

        ! The adaptive kernel: a far tail's wide kernels widen the zone the correction acts in, and
        ! `bandwidth_max` is what keeps it narrow.
        print '(a)', "=== the adaptive kernel under linear: what bandwidth_max saves ==="
        print '(a)', ""
        print '(a)', "  bandwidth_max      fit ms   h_j max/h    cdf zone us"
        nn = sizes(2)
        call exp_sample(nn, x)
        do mm = 1, 2
            reps = 1_int64
            do
                t0 = clock()
                do r = 1_int64, reps
                    if (mm == 1) then
                        call k%fit(x, rule="silverman", adaptive=.true., lower=0.0_real64, &
                            boundary="linear", threads=1)
                    else
                        call k%fit(x, rule="silverman", adaptive=.true., bandwidth_max=2.0_real64*k%bandwidth(), &
                            lower=0.0_real64, boundary="linear", threads=1)
                    end if
                end do
                t = clock() - t0
                if (t >= MIN_LAP .or. reps >= 8_int64) exit
                reps = 2_int64*reps
            end do
            fit_ms = 1000.0_real64*t/real(reps, real64)
            if (allocated(hb)) deallocate(hb)
            allocate(hb(k%n_valid()))
            call k%bandwidths(hb)
            hb_max = maxval(hb)/k%bandwidth()
            call spread(0.02_real64*5.0_real64*k%bandwidth(), 0.98_real64*5.0_real64*k%bandwidth(), qz)
            cdf_z = 1.0e6_real64*time_query(k, 2, qz, out(1:nq), 2, checksum)/real(nq, real64)
            if (mm == 1) then
                print '(a,3f12.2)', "  none         ", fit_ms, hb_max, cdf_z
            else
                print '(a,3f12.2)', "  2h           ", fit_ms, hb_max, cdf_z
            end if
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_boundary

    !> The fastest of `rounds` laps of one query over `q`, in seconds per call; the answers are
    !> folded into `checksum` outside the timed region. Job 1 is `%pdf`, 2 `%cdf`, 3 `%quantile`
    !> and 4 `%sample` (which reads `out` alone). A lap over a second is repeated twice, not
    !> `rounds` times: a zone `%quantile` on a large sample takes seconds.
    function time_query(k, job, q, out, rounds, checksum) result(best)
        type(pf_kde), intent(in)    :: k        !! the fitted estimate
        integer, intent(in)         :: job      !! which query
        real(real64), intent(in)    :: q(:)     !! the points or probabilities
        real(real64), intent(inout) :: out(:)   !! the answers, written before any lap
        integer, intent(in)         :: rounds   !! laps
        real(real64), intent(inout) :: checksum !! the answers' fold
        real(real64)                :: best     !! seconds per call

        real(real64) :: t0, t
        integer(int64) :: reps, r
        integer :: lap, laps

        reps = 1_int64
        do
            t0 = clock()
            do r = 1_int64, reps
                call one_query(k, job, q, out)
            end do
            t = clock() - t0
            if (t >= MIN_LAP .or. reps >= 4096_int64) exit
            reps = 2_int64*reps
        end do
        best = t/real(reps, real64)
        laps = rounds
        if (best > 1.0_real64) laps = 2
        do lap = 2, laps
            t0 = clock()
            do r = 1_int64, reps
                call one_query(k, job, q, out)
            end do
            t = (clock() - t0)/real(reps, real64)
            if (t < best) best = t
        end do
        checksum = checksum + out(1) + out(size(out))

    end function time_query

    !> One query of the kind `job` on `k`.
    subroutine one_query(k, job, q, out)
        type(pf_kde), intent(in)    :: k      !! the fitted estimate
        integer, intent(in)         :: job    !! which query
        real(real64), intent(in)    :: q(:)   !! the points or probabilities
        real(real64), intent(inout) :: out(:) !! the answers

        select case (job)
        case (1)
            call k%pdf(q, out, threads=1)
        case (2)
            call k%cdf(q, out, threads=1)
        case (3)
            call k%quantile(q, out, threads=1)
        case default
            call k%sample(out, 20260920_int64, threads=1)
        end select

    end subroutine one_query

    !> `n` quantiles of `Exp(1)`: a density that is large at its lower bound, where most of the
    !> mass -- and so most of the boundary correction's work -- lies.
    subroutine exp_sample(n, x)
        integer(int64), intent(in)             :: n    !! how many
        real(real64), allocatable, intent(out) :: x(:) !! the values, ascending

        integer(int64) :: i

        if (allocated(x)) deallocate(x)
        allocate(x(n))
        do i = 1_int64, n
            x(i) = -log(1.0_real64 - (real(i, real64) - 0.5_real64)/real(n, real64))
        end do

    end subroutine exp_sample

    subroutine sample(n, x)
        integer(int64), intent(in)             :: n    !! how many
        real(real64), allocatable, intent(out) :: x(:) !! the values

        integer(int64) :: i

        allocate(x(n))
        do i = 1_int64, n
            x(i) = 400.0_real64*sin(real(i, real64)*0.37_real64) + 100.0_real64*cos(real(i, real64)*1.1_real64)
        end do

    end subroutine sample

    !> `size(t)` points spread irregularly over `[a, b]`: the fractional parts of `i` times the
    !> golden ratio.
    subroutine spread(a, b, t)
        real(real64), intent(in)  :: a    !! one end
        real(real64), intent(in)  :: b    !! the other end
        real(real64), intent(out) :: t(:) !! the points

        integer(int64) :: i

        do i = 1_int64, size(t, kind=int64)
            t(i) = a + (b - a)*modulo(real(i, real64)*0.6180339887498949_real64, 1.0_real64)
        end do

    end subroutine spread

    !> How many of the ascending `v` lie in `[lo, hi]`, by two binary searches.
    function count_between(v, lo, hi) result(c)
        real(real64), intent(in) :: v(:) !! ascending values
        real(real64), intent(in) :: lo   !! the lower end
        real(real64), intent(in) :: hi   !! the upper end
        integer(int64)           :: c    !! the count

        c = upper_index(v, hi) - upper_index(v, nearest(lo, -1.0_real64))

    end function count_between

    !> How many of the ascending `v` are at or below `t`.
    function upper_index(v, t) result(i)
        real(real64), intent(in) :: v(:) !! ascending values
        real(real64), intent(in) :: t    !! the threshold
        integer(int64)           :: i    !! the count

        integer(int64) :: hi, mid

        i = 0_int64
        hi = size(v, kind=int64)
        do while (i < hi)
            mid = i + (hi - i + 1_int64)/2_int64
            if (v(mid) <= t) then
                i = mid
            else
                hi = mid - 1_int64
            end if
        end do

    end function upper_index

    ! ---- evaluate ------------------------------------------------------------------------------

    !> `pf_kde%pdf` and `%cdf` per query point, per kernel, at three bandwidths relative to the
    !> sample's range, with the share of the sample one query's window holds on average.
    subroutine run_evaluate(rounds, n, m)
        integer, intent(in)        :: rounds !! timed laps per figure
        integer(int64), intent(in) :: n      !! the sample's size
        integer(int64), intent(in) :: m      !! query points per call

        real(real64), parameter :: FRACTION(3) = [1.0_real64/50.0_real64, 1.0_real64/200.0_real64, &
            1.0_real64/1000.0_real64]
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), xs(:), xq(:), f(:), p(:)
        integer(int64), allocatable :: perm(:)
        real(real64) :: lo, hi, h, t0, t, best_pdf, best_cdf, window, checksum
        integer(int64) :: i, reps, r
        integer :: kk, b, lap

        call sample(n, x)
        call pf_argsort(x, perm)
        allocate(xs(n))
        do i = 1_int64, n
            xs(i) = x(perm(i))
        end do
        lo = xs(1)
        hi = xs(n)
        allocate(xq(m), f(m), p(m))
        call spread(lo, hi, xq)
        f = 0.0_real64
        p = 0.0_real64
        checksum = 0.0_real64

        print '(a)', "=== pf_kde: microseconds per query point ==="
        print '(a,i0,a,i0,a,i0,a)', "sample of ", n, " points, ", m, " query points per call, fastest of ", &
            rounds, " laps"
        print '(a)', "window: the share of the sample within one kernel's reach of a query, on average"
        print '(a)', ""
        print '(a)', "  kernel         h/range     window     pdf us     cdf us"
        do kk = 1, 4
            do b = 1, 3
                h = FRACTION(b)*(hi - lo)
                call k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)))
                window = 0.0_real64
                do i = 1_int64, m
                    window = window + real(count_between(xs, xq(i) - RADIUS(kk)*h, xq(i) + RADIUS(kk)*h), real64)
                end do
                window = window/(real(m, real64)*real(n, real64))

                reps = 1_int64
                do
                    t0 = clock()
                    do r = 1_int64, reps
                        call k%pdf(xq, f, threads=1)
                    end do
                    t = clock() - t0
                    if (t >= MIN_LAP) exit
                    reps = 2_int64*reps
                end do
                best_pdf = t/real(reps, real64)
                do lap = 2, rounds
                    t0 = clock()
                    do r = 1_int64, reps
                        call k%pdf(xq, f, threads=1)
                    end do
                    best_pdf = min(best_pdf, (clock() - t0)/real(reps, real64))
                end do
                checksum = checksum + sum(f)

                reps = 1_int64
                do
                    t0 = clock()
                    do r = 1_int64, reps
                        call k%cdf(xq, p, threads=1)
                    end do
                    t = clock() - t0
                    if (t >= MIN_LAP) exit
                    reps = 2_int64*reps
                end do
                best_cdf = t/real(reps, real64)
                do lap = 2, rounds
                    t0 = clock()
                    do r = 1_int64, reps
                        call k%cdf(xq, p, threads=1)
                    end do
                    best_cdf = min(best_cdf, (clock() - t0)/real(reps, real64))
                end do
                checksum = checksum + sum(p)

                print '(2x,a12,f10.4,f11.4,2f11.3)', KERNELS(kk), FRACTION(b), window, &
                    1.0e6_real64*best_pdf/real(m, real64), 1.0e6_real64*best_cdf/real(m, real64)
            end do
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_evaluate

    ! ---- grid ----------------------------------------------------------------------------------

    !> `pf_kde_grid%add` per point on one thread, per kernel, at 2, 4, 8 and 16 cells per
    !> bandwidth, with the conservation of weight checked on every grid; then `%merge` per cell.
    subroutine run_grid(rounds, n, failures)
        integer, intent(in)        :: rounds   !! timed laps per figure
        integer(int64), intent(in) :: n        !! the sample's size
        integer, intent(inout)     :: failures !! grids that did not conserve their weight

        integer, parameter :: RATIO(4) = [2, 4, 8, 16]
        integer, parameter :: PARTS = 16
        type(pf_kde_grid) :: g
        type(pf_kde_grid), allocatable :: part(:)
        integer, parameter :: GATE_CELLS(2) = [1600, 25600]
        integer, parameter :: NQ_GATE = 20000
        real(real64), allocatable :: x(:), f(:)
        real(real64) :: lo, hi, h, step, xmin, t0, t, best, mass, checksum, gate_ns(2)
        real(real64) :: qx(NQ_GATE)
        integer(int64) :: reps, r, a, b
        integer :: kk, q, nc, lap, i

        call sample(n, x)
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/200.0_real64
        checksum = 0.0_real64

        print '(a)', "=== pf_kde_grid%add: nanoseconds per point, one thread ==="
        print '(a,i0,a,i0,a)', "sample of ", n, " points, bandwidth = range/200, fastest of ", rounds, " laps"
        print '(a)', "cells: the cells one kernel reaches; mass-1: the deposit's total weight over the " // &
            "points', less one"
        print '(a)', ""
        print '(a)', "  kernel         h/step    cells   ns/point     mass-1"
        do kk = 1, 4
            do q = 1, 4
                step = h/real(RATIO(q), real64)
                xmin = lo - RADIUS(kk)*h
                nc = int(ceiling((hi - lo + 2.0_real64*RADIUS(kk)*h)/step))
                reps = 1_int64
                do
                    t0 = clock()
                    do r = 1_int64, reps
                        call g%init(nc, xmin, xmin + real(nc, real64)*step, h, kernel=trim(KERNELS(kk)))
                        call g%add(x, threads=1)
                    end do
                    t = clock() - t0
                    if (t >= MIN_LAP) exit
                    reps = 2_int64*reps
                end do
                best = t/real(reps, real64)
                do lap = 2, rounds
                    t0 = clock()
                    do r = 1_int64, reps
                        call g%init(nc, xmin, xmin + real(nc, real64)*step, h, kernel=trim(KERNELS(kk)))
                        call g%add(x, threads=1)
                    end do
                    best = min(best, (clock() - t0)/real(reps, real64))
                end do
                if (allocated(f)) deallocate(f)
                allocate(f(nc))
                call g%finish()
                call g%density(f, normalise=.false.)
                mass = sum(f)*g%step()/real(n, real64) - 1.0_real64
                if (.not. (abs(mass) <= MASS_GATE)) failures = failures + 1
                checksum = checksum + sum(f)
                print '(2x,a12,i8,f9.1,f11.1,es11.2)', KERNELS(kk), RATIO(q), &
                    2.0_real64*RADIUS(kk)*real(RATIO(q), real64), 1.0e9_real64*best/real(n, real64), mass
            end do
        end do

        ! A merge adds one grid's cells to another's: its cost is the cells, not the points.
        step = h/4.0_real64
        xmin = lo - RADIUS(1)*h
        nc = int(ceiling((hi - lo + 2.0_real64*RADIUS(1)*h)/step))
        allocate(part(PARTS))
        do i = 1, PARTS
            a = (n*int(i - 1, int64))/PARTS + 1_int64
            b = (n*int(i, int64))/PARTS
            call part(i)%init(nc, xmin, xmin + real(nc, real64)*step, h)
            call part(i)%add(x(a:b), threads=1)
        end do
        reps = 1_int64
        do
            t0 = clock()
            do r = 1_int64, reps
                call g%init(nc, xmin, xmin + real(nc, real64)*step, h)
                do i = 1, PARTS
                    call g%merge(part(i))
                end do
            end do
            t = clock() - t0
            if (t >= MIN_LAP) exit
            reps = 2_int64*reps
        end do
        best = t/real(reps, real64)
        do lap = 2, rounds
            t0 = clock()
            do r = 1_int64, reps
                call g%init(nc, xmin, xmin + real(nc, real64)*step, h)
                do i = 1, PARTS
                    call g%merge(part(i))
                end do
            end do
            best = min(best, (clock() - t0)/real(reps, real64))
        end do
        deallocate(f)
        allocate(f(nc))
        call g%finish()
        call g%density(f, normalise=.false.)
        mass = sum(f)*g%step()/real(n, real64) - 1.0_real64
        if (.not. (abs(mass) <= MASS_GATE)) failures = failures + 1
        checksum = checksum + sum(f)
        print '(a)', ""
        print '(a,i0,a,i0,a)', "=== pf_kde_grid%merge: ", PARTS, " grids of ", nc, " cells into one ==="
        print '(a,f9.3,a,es11.2)', "  ns per cell merged ", 1.0e9_real64*best/(real(PARTS, real64)*real(nc, real64)), &
            "   mass-1 ", mass

        ! ---- the cost gate: a bulk `%pdf` must not grow with the cell count ----
        ! `%finish` forms the total mass and the running integral once, so a query is a segment
        ! look-up whatever the grid's size. Before that it rebuilt an `O(cells)` sum per point, and
        ! a grid of twenty-five thousand cells answered four hundred times slower than one of
        ! sixteen hundred. The gate is deliberately loose -- a small factor, not a ratio of one --
        ! because this runs on a loaded machine; what it catches is a return to `O(cells)`.
        deallocate(f)
        allocate(f(NQ_GATE))
        do i = 1, NQ_GATE
            qx(i) = lo + (hi - lo)*(real(i, real64) - 0.5_real64)/real(NQ_GATE, real64)
        end do
        print '(a)', ""
        print '(a)', "=== pf_kde_grid%pdf: nanoseconds per point against the cell count ==="
        print '(a)', "  cells     ns/point"
        do q = 1, 2
            nc = GATE_CELLS(q)
            call g%init(nc, lo - h, hi + h, h)
            call g%add(x, finish=.true.)
            reps = 1_int64
            do
                t0 = clock()
                do r = 1_int64, reps
                    call g%pdf(qx, f)
                end do
                t = clock() - t0
                if (t >= MIN_LAP) exit
                reps = 2_int64*reps
            end do
            gate_ns(q) = 1.0e9_real64*t/(real(reps, real64)*real(NQ_GATE, real64))
            do lap = 2, rounds
                t0 = clock()
                do r = 1_int64, reps
                    call g%pdf(qx, f)
                end do
                gate_ns(q) = min(gate_ns(q), 1.0e9_real64*(clock() - t0)/(real(reps, real64)*real(NQ_GATE, real64)))
            end do
            checksum = checksum + sum(f)
            print '(i7,f13.3)', nc, gate_ns(q)
        end do
        print '(a,f6.2,a,f6.2,a)', "  ", real(GATE_CELLS(2), real64)/real(GATE_CELLS(1), real64), &
            "x the cells costs ", gate_ns(2)/gate_ns(1), "x per point (gate: at most 2x)"
        if (.not. (gate_ns(2) <= PDF_COST_GATE*gate_ns(1))) failures = failures + 1
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_grid

    ! ---- binned ---------------------------------------------------------------------------------

    !> The binned method against the exact deposit: the two at matched cell counts, `%add` split
    !> from `%finish` so the transform's share is visible, the adaptive case at two class counts,
    !> and `boundary="linear"`, whose binned form is two transforms against a deposit that
    !> evaluates a corrected kernel at every cell in reach.
    !>
    !> **The two approximate differently, so a figure at matched CELLS overstates the saving**;
    !> the accuracy column beside it is what makes the comparison honest, and the last block is the
    !> comparison at matched accuracy.
    subroutine run_binned(rounds, n, failures)
        integer, intent(in)        :: rounds   !! timed laps per figure
        integer(int64), intent(in) :: n        !! the sample's size
        integer, intent(inout)     :: failures !! answers that failed their gate

        integer, parameter :: CELLS(3) = [512, 4096, 32768]
        type(pf_kde_grid) :: g, p
        real(real64), allocatable :: x(:), fb(:), fe(:)
        real(real64) :: lo, hi, h, xmin, xmax, t0, t, add_ns, fin_ns, exact_ns, best, peak, worst
        real(real64) :: checksum, mass
        integer(int64) :: reps, r
        integer :: q, nc, lap

        call sample(n, x)
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/200.0_real64
        xmin = lo - 6.0_real64*h
        xmax = hi + 6.0_real64*h
        checksum = 0.0_real64

        print '(a)', "=== pf_kde_grid, binned against exact: nanoseconds per point, one thread ==="
        print '(a,i0,a,i0,a)', "sample of ", n, " points, Gaussian, bandwidth = range/200, fastest of ", &
            rounds, " laps"
        print '(a)', "add: the binning alone; finish: the transform alone; exact: the whole deposit"
        print '(a)', "worst: the largest gap from the exact grid's cells, over its peak"
        print '(a)', ""
        print '(a)', "   cells    add ns/pt  finish ns/pt   exact ns/pt      speed-up        worst"
        do q = 1, 3
            nc = CELLS(q)
            if (allocated(fb)) deallocate(fb, fe)
            allocate(fb(nc), fe(nc))
            ! the binning
            reps = 1_int64
            do
                t0 = clock()
                do r = 1_int64, reps
                    call g%init(nc, xmin, xmax, h, method="binned")
                    call g%add(x, threads=1)
                end do
                t = clock() - t0
                if (t >= MIN_LAP) exit
                reps = 2_int64*reps
            end do
            best = t/real(reps, real64)
            do lap = 2, rounds
                t0 = clock()
                do r = 1_int64, reps
                    call g%init(nc, xmin, xmax, h, method="binned")
                    call g%add(x, threads=1)
                end do
                best = min(best, (clock() - t0)/real(reps, real64))
            end do
            add_ns = 1.0e9_real64*best/real(n, real64)
            ! the transform, timed on a grid already filled
            best = huge(1.0_real64)
            do lap = 1, rounds
                call g%init(nc, xmin, xmax, h, method="binned")
                call g%add(x, threads=1)
                t0 = clock()
                call g%finish()
                best = min(best, clock() - t0)
            end do
            fin_ns = 1.0e9_real64*best/real(n, real64)
            call g%density(fb)
            ! the exact deposit at the same cells
            best = huge(1.0_real64)
            do lap = 1, rounds
                t0 = clock()
                call p%init(nc, xmin, xmax, h)
                call p%add(x, threads=1)
                call p%finish()
                best = min(best, clock() - t0)
            end do
            exact_ns = 1.0e9_real64*best/real(n, real64)
            call p%density(fe)
            peak = maxval(fe)
            worst = maxval(abs(fb - fe))/peak
            checksum = checksum + sum(fb) + sum(fe)
            ! The binned deposit must conserve the weight it was given, as the exact one does: the
            ! cells' share, plus the share counted below `xmin` and above `xmax`, is one.
            call g%density(fb, normalise=.false.)
            call g%cdf(xmin, t)
            call g%cdf(xmax, t0)
            mass = sum(fb)*g%step()/real(n, real64) + t + (1.0_real64 - t0) - 1.0_real64
            if (.not. (abs(mass) <= MASS_GATE)) failures = failures + 1
            print '(i8,4f14.2,es13.3)', nc, add_ns, fin_ns, exact_ns, exact_ns/(add_ns + fin_ns), worst
        end do
        print '(a)', ""
        print '(a,es12.4)', "checksum (ignore): ", checksum

    end subroutine run_binned

    ! ---- accuracy ------------------------------------------------------------------------------

    !> The grid's `%pdf` against `pf_kde%pdf` at irregular points, and its `%density` against the
    !> exact estimate at its own centres, across a ladder of cell widths, per kernel, with the order
    !> of convergence each halving shows. No timing.
    subroutine run_accuracy(n, m)
        integer(int64), intent(in) :: n !! the sample's size
        integer(int64), intent(in) :: m !! query points

        integer, parameter :: RUNGS = 7
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), xq(:), fx(:), fg(:), c(:), fc(:), fe(:)
        real(real64) :: lo, hi, h, xmin, xmax, err, prev, centre_err
        integer :: kk, rung, nc
        character(len=8) :: order

        call sample(n, x)
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/100.0_real64
        allocate(xq(m), fx(m), fg(m))
        call spread(lo, hi, xq)

        print '(a)', "=== pf_kde_grid against pf_kde: the order of convergence in the cell width ==="
        print '(a,i0,a,i0,a)', "sample of ", n, " points, bandwidth = range/100, ", m, &
            " irregular points across the sample"
        print '(a)', "pdf rms: %pdf between centres; centres: the largest gap at a centre, over the peak"
        print '(a)', "order: log2 of the rms ratio between this rung and the one above (2 is second order)"
        print '(a)', ""
        print '(a)', "  kernel          cells   h/step     pdf rms    order     centres"
        do kk = 1, 4
            call k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)))
            call k%pdf(xq, fx, threads=1)
            xmin = lo - RADIUS(kk)*h
            xmax = hi + RADIUS(kk)*h
            prev = -1.0_real64
            nc = 25
            do rung = 1, RUNGS
                call g%init(nc, xmin, xmax, h, kernel=trim(KERNELS(kk)))
                call g%add(x)
                call g%finish()
                call g%pdf(xq, fg)
                err = sqrt(sum((fg - fx)**2)/real(m, real64))
                if (allocated(c)) deallocate(c, fc, fe)
                allocate(c(nc), fc(nc), fe(nc))
                call g%density(fc, x=c)
                call k%pdf(c, fe, threads=1)
                centre_err = maxval(abs(fc - fe))/maxval(fe)
                order = ""
                if (prev > 0.0_real64 .and. err > 0.0_real64) write(order, '(f8.2)') log(prev/err)/log(2.0_real64)
                print '(2x,a12,i8,f9.2,es12.3,a9,es12.3)', KERNELS(kk), nc, h/g%step(), err, order, centre_err
                prev = err
                nc = 2*nc
            end do
            print '(a)', ""
        end do

    end subroutine run_accuracy

    ! ---- adaptive ------------------------------------------------------------------------------

    !> What the adaptive kernel adds, per kernel: `%fit` fixed and adaptive, on one thread, with the
    !> adaptive fit's three phases as the library times them (the sort, the pilot, each point's
    !> bandwidth and mass), the pilot's cells, the spread of the bandwidths, and `%pdf` per query
    !> point under each fit. The bandwidth is 1/200 of the sample's range, so the pilot's cells are
    !> 800 across it.
    subroutine run_adaptive(rounds, n, m)
        integer, intent(in)        :: rounds !! timed laps per figure
        integer(int64), intent(in) :: n      !! the sample's size
        integer(int64), intent(in) :: m      !! query points per call

        type(pf_kde) :: kf, ka
        type(pf_kde_grid) :: pilot
        real(real64), allocatable :: x(:), xq(:), f(:), hb(:)
        real(real64) :: lo, hi, h, best_fixed, best_adapt, best_pf, best_pa, checksum
        integer(int64) :: best_ns(3)
        integer :: kk

        call sample(n, x)
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/200.0_real64
        allocate(xq(m), f(m), hb(n))
        call spread(lo, hi, xq)
        f = 0.0_real64
        checksum = 0.0_real64

        print '(a)', "=== pf_kde%fit adaptive against fixed: milliseconds, one thread ==="
        print '(a,i0,a,i0,a)', "sample of ", n, " points, bandwidth = range/200, fastest of ", rounds, " laps"
        print '(a)', "sort, pilot, lookup: the adaptive fit's phases (parquet_debug_kde_fit_nanos); pdf: " // &
            "microseconds per query point"
        print '(a)', ""
        print '(a)', "  kernel        fixed  adaptive      sort     pilot    lookup   cells  h_j/h min  max" // &
            "   pdf fixed  pdf adapt"
        do kk = 1, 4
            best_fixed = time_fit(kf, x, h, trim(KERNELS(kk)), .false., rounds, best_ns)
            best_adapt = time_fit(ka, x, h, trim(KERNELS(kk)), .true., rounds, best_ns)
            call ka%pilot(pilot)
            call ka%bandwidths(hb)
            best_pf = time_pdf(kf, xq, f, rounds, checksum)
            best_pa = time_pdf(ka, xq, f, rounds, checksum)
            print '(2x,a12,5f10.2,i8,2f7.3,2f11.3)', KERNELS(kk), 1.0e3_real64*best_fixed, 1.0e3_real64*best_adapt, &
                1.0e-6_real64*real(best_ns(1), real64), 1.0e-6_real64*real(best_ns(2), real64), &
                1.0e-6_real64*real(best_ns(3), real64), pilot%ncells(), minval(hb)/h, maxval(hb)/h, &
                1.0e6_real64*best_pf/real(m, real64), 1.0e6_real64*best_pa/real(m, real64)
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_adaptive

    ! ---- rules ---------------------------------------------------------------------------------

    !> What choosing the bandwidth costs, per rule, on one thread: `%fit` under `rule="isj"`,
    !> `"silverman"` and `"scott"`, and with the bandwidth given as a number, on samples of a
    !> thousand points and every tenfold larger size up to `n`, in milliseconds. The fit given a
    !> number is the sort and the bookkeeping every fit shares, so each rule's column less it is what
    !> the rule adds: the ISJ rule's is one pass binning the sample and a fixed amount for its
    !> transform and its fixed point, the rules of thumb's their passes over the population.
    subroutine run_rules(rounds, n)
        integer, intent(in)        :: rounds !! timed laps per figure
        integer(int64), intent(in) :: n      !! the largest sample's size

        character(len=9), parameter :: RULES(3) = [character(len=9) :: "isj", "silverman", "scott"]
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: h, best(4), h_isj, h_silverman, checksum
        integer(int64) :: nsize
        integer :: r

        call sample(n, x)
        h = (maxval(x) - minval(x))/200.0_real64
        checksum = 0.0_real64

        print '(a)', "=== pf_kde%fit under each rule: milliseconds, one thread ==="
        print '(a,i0,a)', "bandwidth: the rule's, or range/200 given as a number; fastest of ", rounds, " laps"
        print '(a)', "isj/silverman: the two rules' bandwidths over each other on this sample"
        print '(a)', ""
        print '(a)', "     points        isj  silverman      scott     number  isj/silverman"
        nsize = 1000_int64
        do while (nsize <= n)
            do r = 1, 3
                best(r) = time_rule_fit(k, x(1:nsize), trim(RULES(r)), h, rounds, checksum)
                if (r == 1) h_isj = k%bandwidth()
                if (r == 2) h_silverman = k%bandwidth()
            end do
            best(4) = time_rule_fit(k, x(1:nsize), "", h, rounds, checksum)
            print '(i11,4f11.3,f15.3)', nsize, 1.0e3_real64*best, h_isj/h_silverman
            nsize = 10_int64*nsize
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_rules

    !> The fastest of `rounds` fits of `x` under `rule` -- or with the bandwidth `h` given as a
    !> number when `rule` is blank -- on one thread, each lap repeated until it is long enough to
    !> time, in seconds per fit; each fit's bandwidth is folded into `checksum`.
    function time_rule_fit(k, x, rule, h, rounds, checksum) result(best)
        type(pf_kde), intent(inout) :: k        !! the estimate; refitted every repetition
        real(real64), intent(in)    :: x(:)     !! the sample
        character(len=*), intent(in) :: rule    !! the rule's token, or blank for `h`
        real(real64), intent(in)    :: h        !! the bandwidth given as a number
        integer, intent(in)         :: rounds   !! laps
        real(real64), intent(inout) :: checksum !! the keep-it-live sum
        real(real64)                :: best     !! seconds per fit, the fastest lap

        real(real64) :: t0, t
        integer(int64) :: reps, r
        integer :: lap

        ! The first lap doubles its repetitions until it lasts `MIN_LAP`; the rest repeat as many.
        reps = 1_int64
        best = huge(1.0_real64)
        lap = 0
        do while (lap < rounds)
            t0 = clock()
            do r = 1_int64, reps
                if (len(rule) == 0) then
                    call k%fit(x, bandwidth=h, threads=1)
                else
                    call k%fit(x, rule=rule, threads=1)
                end if
                checksum = checksum + k%bandwidth()
            end do
            t = clock() - t0
            if (lap == 0 .and. t < MIN_LAP) then
                reps = 2_int64*reps
                cycle
            end if
            lap = lap + 1
            best = min(best, t/real(reps, real64))
        end do

    end function time_rule_fit

    ! ---- sample --------------------------------------------------------------------------------

    !> What a draw costs, per kernel, on one thread: from the exact form unweighted, weighted and
    !> with a lower bound at the sample's minimum (where the kernels crossing it reject their draws
    !> beyond it), and from a grid of four cells to a bandwidth. The bandwidth is 1/200 of the range.
    subroutine run_sample(rounds, n, m)
        integer, intent(in)        :: rounds !! timed laps per figure
        integer(int64), intent(in) :: n      !! the sample's size
        integer(int64), intent(in) :: m      !! draws per call

        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:), v(:)
        real(real64) :: lo, hi, h, best(4), checksum
        integer(int64) :: i
        integer :: kk, form, nc

        call sample(n, x)
        allocate(w(n), v(m))
        do i = 1_int64, n
            w(i) = real(1_int64 + modulo(i, 3_int64), real64)
        end do
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/200.0_real64
        v = 0.0_real64
        checksum = 0.0_real64

        print '(a)', "=== %sample: nanoseconds per draw, one thread ==="
        print '(a,i0,a,i0,a,i0,a)', "sample of ", n, " points, ", m, " draws per call, bandwidth = range/200, " // &
            "fastest of ", rounds, " laps"
        print '(a)', "bounded: lower = the sample's minimum, so the kernels crossing it redraw beyond it; " // &
            "grid: 4 cells to a bandwidth"
        print '(a)', ""
        print '(a)', "  kernel          exact   weighted    bounded       grid"
        do kk = 1, 4
            do form = 1, 4
                select case (form)
                case (1)
                    call k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)))
                case (2)
                    call k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), weights=w)
                case (3)
                    call k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), lower=lo)
                case (4)
                    nc = int(ceiling((hi - lo + 2.0_real64*RADIUS(kk)*h)/(h/4.0_real64)))
                    call g%init(nc, lo - RADIUS(kk)*h, lo - RADIUS(kk)*h + real(nc, real64)*h/4.0_real64, h, &
                        kernel=trim(KERNELS(kk)))
                    call g%add(x, threads=1)
                end select
                best(form) = time_draws(k, g, form == 4, v, rounds, checksum)
            end do
            print '(2x,a12,4f11.1)', KERNELS(kk), 1.0e9_real64*best/real(m, real64)
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_sample

    !> The fastest of `rounds` calls of `%sample` into `v` on one thread, from the grid when
    !> `from_grid` and from `k` otherwise, repeated until a lap is long enough to time, in seconds
    !> per call; every call draws on its own stream, and the draws are folded into `checksum`.
    function time_draws(k, g, from_grid, v, rounds, checksum) result(best)
        type(pf_kde), intent(in)      :: k         !! the fitted estimate
        type(pf_kde_grid), intent(in) :: g         !! the grid
        logical, intent(in)           :: from_grid !! draw from the grid
        real(real64), intent(inout)   :: v(:)      !! the draws, written before any lap
        integer, intent(in)           :: rounds    !! laps
        real(real64), intent(inout)   :: checksum  !! the keep-it-live sum
        real(real64)                  :: best      !! seconds per call, the fastest lap

        real(real64) :: t0, t
        integer(int64) :: reps, r, stream
        integer :: lap

        stream = 0_int64
        reps = 1_int64
        do
            t0 = clock()
            do r = 1_int64, reps
                stream = stream + 1_int64
                if (from_grid) then
                    call g%sample(v, 20260919_int64, stream, threads=1)
                else
                    call k%sample(v, 20260919_int64, stream, threads=1)
                end if
            end do
            t = clock() - t0
            if (t >= MIN_LAP) exit
            reps = 2_int64*reps
        end do
        best = t/real(reps, real64)
        do lap = 2, rounds
            t0 = clock()
            do r = 1_int64, reps
                stream = stream + 1_int64
                if (from_grid) then
                    call g%sample(v, 20260919_int64, stream, threads=1)
                else
                    call k%sample(v, 20260919_int64, stream, threads=1)
                end if
            end do
            best = min(best, (clock() - t0)/real(reps, real64))
        end do
        checksum = checksum + sum(v)

    end function time_draws

    ! ---- threads -------------------------------------------------------------------------------

    !> `%add`, `%pdf` and `%sample` across a ladder of thread counts, 1, 2, 4, ... up to the
    !> threads OpenMP offers (at most 64): the wall time of one call, the speed-up over one thread,
    !> and the team the call actually opened. `%pdf` over `m` points and `%sample` of `100*m` draws
    !> must give the serial bits at every count, and `%add`'s grid must agree with the serial one
    !> to `1e-12` of its largest cell (the weaker promise of a partition that follows the team);
    !> a rung that misses its gate counts a failure, and the run exits nonzero.
    subroutine run_threads(rounds, n, m, failures)
        integer, intent(in)        :: rounds   !! timed laps per figure
        integer(int64), intent(in) :: n        !! the sample's size
        integer(int64), intent(in) :: m        !! query points per call
        integer, intent(inout)     :: failures !! rungs whose answer moved

        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), xq(:), f(:), f1(:), v(:), v1(:), acc(:), acc1(:)
        real(real64) :: lo, hi, h, t0, t, best_add, best_pdf, best_draw, base(3), gap, checksum
        integer :: nt, top, lap, nc, team(3)
        character(len=8) :: gate

        call sample(n, x)
        lo = minval(x)
        hi = maxval(x)
        h = (hi - lo)/200.0_real64
        nc = int(ceiling((hi - lo + 10.0_real64*h)/(h/4.0_real64)))
        call k%fit(x, bandwidth=h)
        allocate(xq(m), f(m), f1(m), v(100_int64*m), v1(100_int64*m), acc(nc), acc1(nc))
        call spread(lo, hi, xq)
        f = 0.0_real64
        v = 0.0_real64
        checksum = 0.0_real64
        top = min(64, max_threads())

        print '(a)', "=== the threaded forms: milliseconds per call, and the speed-up over one thread ==="
        print '(a,i0,a,i0,a,i0,a,i0,a)', "sample of ", n, " points, bandwidth = range/200, Gaussian; %pdf at ", m, &
            " points; %sample of ", 100_int64*m, " draws; %add of the sample into ", nc, " cells"
        print '(a)', "team: the threads each call actually opened (%add/%pdf/%sample); gate: the answers against " // &
            "the serial ones"
        print '(a)', ""
        print '(a)', "  threads   team         add ms   speed-up     pdf ms   speed-up  sample ms   speed-up   gate"
        nt = 1
        do while (nt <= top)
            best_add = huge(1.0_real64)
            best_pdf = huge(1.0_real64)
            best_draw = huge(1.0_real64)
            do lap = 1, rounds
                t0 = clock()
                call g%init(nc, lo - 5.0_real64*h, lo - 5.0_real64*h + real(nc, real64)*h/4.0_real64, h)
                call g%add(x, threads=nt)
                t = clock() - t0
                best_add = min(best_add, t)
                team(1) = parquet_debug_kde_threads_used()
                t0 = clock()
                call k%pdf(xq, f, threads=nt)
                t = clock() - t0
                best_pdf = min(best_pdf, t)
                team(2) = parquet_debug_kde_threads_used()
                t0 = clock()
                call k%sample(v, 20260919_int64, threads=nt)
                t = clock() - t0
                best_draw = min(best_draw, t)
                team(3) = parquet_debug_kde_threads_used()
            end do
            call g%finish()
            call g%density(acc, normalise=.false.)
            if (nt == 1) then
                base = [best_add, best_pdf, best_draw]
                f1 = f
                v1 = v
                acc1 = acc
            end if
            gap = maxval(abs(acc - acc1))/maxval(abs(acc1))
            gate = "ok"
            if (any(f /= f1) .or. any(v /= v1) .or. .not. (gap <= 1.0e-12_real64)) then
                gate = "MOVED"
                failures = failures + 1
            end if
            checksum = checksum + sum(f) + sum(v) + sum(acc)
            print '(i9,2x,i3,"/",i3,"/",i3,3(f11.2,f11.2),3x,a)', nt, team, 1.0e3_real64*best_add, base(1)/best_add, &
                1.0e3_real64*best_pdf, base(2)/best_pdf, 1.0e3_real64*best_draw, base(3)/best_draw, trim(gate)
            nt = 2*nt
        end do
        print '(a)', ""
        print '(a,es22.14)', "checksum ", checksum

    end subroutine run_threads

    !> The threads OpenMP offers here; 1 without OpenMP.
    function max_threads() result(n)
        integer :: n !! the thread count

#ifdef _OPENMP
        n = omp_get_max_threads()
#else
        n = 1
#endif

    end function max_threads

    !> The fastest of `rounds` fits of `x` on one thread, fixed or adaptive, into `k`; for the
    !> adaptive fit, the library's phase times of the fastest lap go to `best_ns`.
    function time_fit(k, x, h, kernel, adaptive, rounds, best_ns) result(best)
        type(pf_kde), intent(inout)   :: k          !! the estimate; refitted every lap
        real(real64), intent(in)      :: x(:)       !! the sample
        real(real64), intent(in)      :: h          !! the bandwidth
        character(len=*), intent(in)  :: kernel     !! the kernel's token
        logical, intent(in)           :: adaptive   !! fit the adaptive kernel
        integer, intent(in)           :: rounds     !! laps
        integer(int64), intent(inout) :: best_ns(3) !! the fastest adaptive lap's phases
        real(real64)                  :: best       !! seconds, the fastest lap

        real(real64) :: t0, t
        integer(int64) :: ns(3)
        integer :: lap

        best = huge(1.0_real64)
        do lap = 1, rounds
            t0 = clock()
            if (adaptive) then
                call k%fit(x, bandwidth=h, kernel=kernel, adaptive=.true., threads=1)
            else
                call k%fit(x, bandwidth=h, kernel=kernel, threads=1)
            end if
            t = clock() - t0
            if (adaptive) call parquet_debug_kde_fit_nanos(ns(1), ns(2), ns(3))
            if (t < best) then
                best = t
                if (adaptive) best_ns = ns
            end if
        end do

    end function time_fit

    !> The fastest of `rounds` calls of `k%pdf` over `xq`, repeated until a lap is long enough to
    !> time, in seconds per call; the answer is folded into `checksum`.
    function time_pdf(k, xq, f, rounds, checksum) result(best)
        type(pf_kde), intent(in)    :: k        !! the fitted estimate
        real(real64), intent(in)    :: xq(:)    !! the query points
        real(real64), intent(inout) :: f(:)     !! the density at each, written before any lap
        integer, intent(in)         :: rounds   !! laps
        real(real64), intent(inout) :: checksum !! the keep-it-live sum
        real(real64)                :: best     !! seconds per call, the fastest lap

        real(real64) :: t0, t
        integer(int64) :: reps, r
        integer :: lap

        reps = 1_int64
        do
            t0 = clock()
            do r = 1_int64, reps
                call k%pdf(xq, f, threads=1)
            end do
            t = clock() - t0
            if (t >= MIN_LAP) exit
            reps = 2_int64*reps
        end do
        best = t/real(reps, real64)
        do lap = 2, rounds
            t0 = clock()
            do r = 1_int64, reps
                call k%pdf(xq, f, threads=1)
            end do
            best = min(best, (clock() - t0)/real(reps, real64))
        end do
        checksum = checksum + sum(f)

    end function time_pdf

end program benchmark_kde
