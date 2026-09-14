!> Integrands for `benchmark_integrate`, as module procedures.
!!
!! A callback in this repository is never an internal procedure -- flang SIGSEGVs before the callee
!! runs when one is passed as an actual argument -- so the benchmark's integrands live in a module
!! beside its program rather than in its `contains`.
!!
!! Each carries its closed form, so the `cost` mode can report a relative error beside every
!! timing: a quadrature benchmark that reported only a time would rank a wrong answer first.
module benchmark_integrate_kernels

    use iso_fortran_env, only : real64

    implicit none
    private

    public :: k_osc, k_osc_exact
    public :: k_sing, k_sing_exact
    public :: k_peak, k_peak_exact
    public :: k_heavy, k_heavy_exact
    public :: k_linear, k_linear_exact
    public :: k_gauss, k_gauss_exact
    public :: k_tail_exp, k_tail_pow15, k_tail_pow2, k_tail_gauss, k_tail_osc
    public :: k_spike, k_spike_exact, k_bump, k_bump_exact

    !> Terms in `k_heavy`, which exists to make one evaluation expensive enough that the
    !! framework's own cost per evaluation stops dominating the measurement.
    integer, parameter :: HEAVY_TERMS = 40

    !> Half-width of `k_spike`, the narrow feature sitting just above the lower bound `1`.
    real(real64), parameter :: SPIKE_WIDTH = 1.0e-3_real64
    !> Centre of `k_spike`.
    real(real64), parameter :: SPIKE_AT = 1.02_real64
    !> Centre of `k_bump`, far along the range from every lower bound the walk mode uses.
    real(real64), parameter :: BUMP_AT = 40.0_real64

contains

    !> `2/(2 + sin(10 pi x))`: smooth and oscillatory.
    function k_osc(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 2.0_real64/(2.0_real64 + sin(10.0_real64*acos(-1.0_real64)*x))

    end function k_osc

    !> Mean of `2/(2+sin t)` over a period is `2/sqrt(3)`, and `[0,1]` is five whole periods.
    pure function k_osc_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        v = 2.0_real64/sqrt(3.0_real64)

    end function k_osc_exact

    !> `log(x)/sqrt(x)`: an integrable singularity at the left endpoint.
    function k_sing(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > 0.0_real64) f = log(x)/sqrt(x)

    end function k_sing

    !> Antiderivative `2 sqrt(x) log(x) - 4 sqrt(x)` is 0 at 0 and -4 at 1.
    pure function k_sing_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        v = -4.0_real64

    end function k_sing_exact

    !> `1/((x - 0.3)^2 + 1e-6)`: an interior peak of width about `1e-3`.
    function k_peak(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 1.0_real64/((x - 0.3_real64)**2 + 1.0e-6_real64)

    end function k_peak

    !> `atan((x-c)/e)/e` evaluated at the two bounds, with `c = 0.3` and `e = 1e-3`.
    pure function k_peak_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        v = 1000.0_real64*(atan(700.0_real64) - atan(-300.0_real64))

    end function k_peak_exact

    !> A 40-term sum: smooth, and deliberately expensive per evaluation.
    function k_heavy(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        integer      :: i
        real(real64) :: ri

        f = 0.0_real64
        do i = 1, HEAVY_TERMS
            ri = real(i, real64)
            f = f + sin(ri*x)*exp(-0.01_real64*ri*x)/ri
        end do

    end function k_heavy

    !> Term by term, `(exp(-0.01 i)(-0.01 sin i - cos i) + 1)/(i^2 (1 + 1e-4))`.
    pure function k_heavy_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        integer      :: i
        real(real64) :: ri

        v = 0.0_real64
        do i = 1, HEAVY_TERMS
            ri = real(i, real64)
            v = v + (exp(-0.01_real64*ri)*(-0.01_real64*sin(ri) - cos(ri)) + 1.0_real64) &
                /(ri*ri*1.0001_real64)
        end do

    end function k_heavy_exact

    !> `x`: the cheapest integrand there is, and exact under one rule application.
    function k_linear(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x

    end function k_linear

    !> `x^2/2` over `[0, 1]`.
    pure function k_linear_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        v = 0.5_real64

    end function k_linear_exact

    !> A gaussian of width `0.01` at `0.9`: needs real refinement, so the work arrays grow.
    function k_gauss(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-((x - 0.9_real64)/0.01_real64)**2)

    end function k_gauss

    !> `w sqrt(pi)/2 (erf((1-c)/w) + erf(c/w))` with `c = 0.9`, `w = 0.01`.
    pure function k_gauss_exact() result(v)
        real(real64) :: v !! the exact integral over [0, 1]

        v = 0.01_real64*sqrt(acos(-1.0_real64))/2.0_real64 &
            *(erf(10.0_real64) + erf(90.0_real64))

    end function k_gauss_exact

    !> `exp(-x)`: the plainest decaying tail. Integral over `[a, inf)` is `exp(-a)`.
    function k_tail_exp(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x)

    end function k_tail_exp

    !> `x**-1.5`: an algebraic tail, which decays far more slowly. Integral over `[a, inf)` is
    !! `2/sqrt(a)`.
    function k_tail_pow15(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x**(-1.5_real64)

    end function k_tail_pow15

    !> `x**-2`. Integral over `[a, inf)` is `1/a`. QUADPACK's transform turns this one into a
    !! CONSTANT, so its reference count is 15 and no method can be scored against it.
    function k_tail_pow2(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x**(-2.0_real64)

    end function k_tail_pow2

    !> `exp(-x**2)`: a gaussian tail. Integral over `[0, inf)` is `sqrt(pi)/2`.
    function k_tail_gauss(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x*x)

    end function k_tail_gauss

    !> `sin(x)/x**2`: an oscillatory tail, which defeats every method at a tight tolerance and is
    !! reported rather than scored. Integral over `[1, inf)` is `sin(1) - Ci(1)`.
    function k_tail_osc(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = sin(x)/(x*x)

    end function k_tail_osc

    !> A gaussian spike of half-width `1e-3` centred at `1.02`: the narrow feature sitting just
    !! above a lower bound of 1, which the start-panel search's NARROW retry is what finds.
    function k_spike(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-((x - SPIKE_AT)/SPIKE_WIDTH)**2)

    end function k_spike

    !> `spike_width*sqrt(pi)/2*(1 + erf(20))`, which is `spike_width*sqrt(pi)/2` to every bit a
    !! `real64` carries: 20 half-widths of a gaussian is the whole of it.
    pure function k_spike_exact() result(v)
        real(real64) :: v !! the exact integral over [1, inf)

        v = SPIKE_WIDTH*sqrt(acos(-1.0_real64))*0.5_real64 &
            *(1.0_real64 + erf((SPIKE_AT - 1.0_real64)/SPIKE_WIDTH))

    end function k_spike_exact

    !> A unit-width gaussian bump centred at 40: the feature far along the range that QUADPACK's
    !! change of variable steps straight over (section 3.4 of the design document).
    function k_bump(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-(x - BUMP_AT)**2)

    end function k_bump

    !> `sqrt(pi)`: from any lower bound well below 40 the whole bump is inside the range.
    pure function k_bump_exact() result(v)
        real(real64) :: v !! the exact integral over [a, inf) for a well below 40

        v = sqrt(acos(-1.0_real64))

    end function k_bump_exact

end module benchmark_integrate_kernels

!> What `pf_integrate` costs: accuracy against evaluations per integrand class (`cost`), and the
!> per-call floor the work arrays and the record buffer impose (`overhead`).
!!
!! Both modes report a relative error beside every timing, because the cheapest integrator is
!! always the one that returns the wrong answer fastest.
!!
!! Usage and configuration are in `bench/benchmark_integrate.sh`, which is how this program is
!! meant to be run: it is the half that verifies the optimisation flags before believing a number.
program benchmark_integrate

    use iso_fortran_env, only : real64, error_unit
    use parquet_integrate
    use benchmark_integrate_kernels

    implicit none

    character(len=32) :: mode
    integer           :: rounds, repeats

    call parse_arguments(mode, rounds, repeats)

    select case (trim(mode))
    case ("cost")
        call run_cost(rounds, repeats)
    case ("overhead")
        call run_overhead(rounds, repeats)
    case ("walk")
        call run_walk()
    case default
        write (error_unit, '(a)') "benchmark_integrate: unknown mode '"//trim(mode)//"'"
        error stop 1
    end select

contains

    !> Reads the `--key=value` command line, applying each default when the flag is absent.
    subroutine parse_arguments(mode, rounds, repeats)
        character(len=*), intent(out) :: mode    !! which measurement to run
        integer, intent(out)          :: rounds  !! timed rounds; the best of them is kept
        integer, intent(out)          :: repeats !! integrations inside one timed round

        character(len=64) :: arg, val
        integer           :: k, eq

        mode = "cost"
        rounds = 5
        repeats = 200

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                write (error_unit, '(a)') "benchmark_integrate: unrecognised argument '" &
                    //trim(arg)//"'"
                error stop 1
            end if
            val = arg(eq + 1:)
            select case (arg(1:eq - 1))
            case ("--mode")
                mode = trim(val)
            case ("--rounds")
                read (val, *) rounds
            case ("--repeats")
                read (val, *) repeats
            case default
                write (error_unit, '(a)') "benchmark_integrate: unknown option '" &
                    //trim(arg(1:eq - 1))//"'"
                write (error_unit, '(a)') "Usage: benchmark_integrate " &
                    //"[--mode=cost|overhead|walk] [--rounds=N] [--repeats=N]"
                error stop 1
            end select
        end do

        if (rounds < 1) error stop "benchmark_integrate: --rounds must be >= 1"
        if (repeats < 1) error stop "benchmark_integrate: --repeats must be >= 1"

    end subroutine parse_arguments

    !> Accuracy against cost, per integrand class and per tolerance, with the extrapolation off
    !! and on.
    subroutine run_cost(rounds, repeats)
        integer, intent(in) :: rounds  !! timed rounds; the best is kept
        integer, intent(in) :: repeats !! integrations inside one timed round

        real(real64), parameter :: LADDER(4) = [1.0e-6_real64, 1.0e-8_real64, &
                                                1.0e-10_real64, 1.0e-12_real64]
        integer :: i
        logical :: eps

        print '(a)', "=== pf_integrate: accuracy against cost ==="
        print '(a,i0,a,i0)', "best of ", rounds, " rounds, integrations per round: ", repeats
        print '(a)', ""
        print '(a)', "  variant       case        rtol     neval    relerr      us/call"

        do i = 1, 2
            eps = i == 2
            call one_case(k_osc, k_osc_exact(), LADDER, eps, rounds, repeats, "A osc     ")
            call one_case(k_sing, k_sing_exact(), LADDER, eps, rounds, repeats, "B sing    ")
            call one_case(k_peak, k_peak_exact(), LADDER, eps, rounds, repeats, "C peak    ")
            call one_case(k_heavy, k_heavy_exact(), LADDER, eps, rounds, repeats, "F heavy   ")
            print '(a)', ""
        end do

    end subroutine run_cost

    !> Times one integrand over the whole tolerance ladder.
    subroutine one_case(fn, want, ladder, eps, rounds, repeats, tag)
        procedure(pf_integrand_func) :: fn         !! the integrand
        real(real64), intent(in)     :: want       !! its closed form over [0, 1]
        real(real64), intent(in)     :: ladder(:)  !! tolerances to sweep
        logical, intent(in)          :: eps        !! run with the extrapolation on
        integer, intent(in)          :: rounds     !! timed rounds; the best is kept
        integer, intent(in)          :: repeats    !! integrations inside one timed round
        character(len=*), intent(in) :: tag        !! names the case in the report

        type(pf_integration_info) :: info
        real(real64)              :: best, t0, t1, keep, r
        integer                   :: j, k, n

        do n = 1, size(ladder)
            ! One untimed call first: it is what fills `info`, so the timed region does nothing
            ! but integrate.
            r = pf_integrate(fn, 0.0_real64, 1.0_real64, ladder(n), extrapolate=eps, info=info)

            best = huge(1.0_real64)
            keep = 0.0_real64
            do j = 1, rounds
                call cpu_time(t0)
                do k = 1, repeats
                    keep = keep + pf_integrate(fn, 0.0_real64, 1.0_real64, ladder(n), &
                                               extrapolate=eps)
                end do
                call cpu_time(t1)
                best = min(best, t1 - t0)
            end do

            print '(a,a,a,es10.1,i8,es11.2,f12.3)', "  ", &
                merge("eps on ", "eps off", eps), tag, ladder(n), info%neval, &
                abs(r - want)/abs(want), 1.0e6_real64*best/real(repeats, real64)
            ! The checksum is read outside every timed region, so that no round's work can be
            ! optimised away and no round pays for the reading.
            if (keep /= keep) print '(a)', "  (checksum went non-finite)"
        end do

    end subroutine one_case

    !> The per-call floor: what a call costs when the integrand costs nothing.
    !!
    !! `k_linear` converges in one rule application, so the time per call is the work arrays, the
    !! validation and the record, not the quadrature. The `points` arm is what the record costs;
    !! the gaussian arm is what the geometric growth of the work arrays costs when growth actually
    !! happens.
    subroutine run_overhead(rounds, repeats)
        integer, intent(in) :: rounds  !! timed rounds; the best is kept
        integer, intent(in) :: repeats !! integrations inside one timed round

        type(pf_integration_points) :: pts
        real(real64)                :: best, t0, t1, keep, r
        integer                     :: j, k

        print '(a)', "=== pf_integrate: the per-call floor ==="
        print '(a,i0,a,i0)', "best of ", rounds, " rounds, integrations per round: ", repeats
        print '(a)', ""

        ! (a) the floor with no record at all.
        best = huge(1.0_real64)
        keep = 0.0_real64
        do j = 1, rounds
            call cpu_time(t0)
            do k = 1, repeats
                keep = keep + pf_integrate(k_linear, 0.0_real64, 1.0_real64, 1.0e-6_real64)
            end do
            call cpu_time(t1)
            best = min(best, t1 - t0)
        end do
        print '(a,f12.3)', "  trivial integrand, no record      us/call: ", &
            1.0e6_real64*best/real(repeats, real64)
        if (keep /= keep) print '(a)', "  (checksum went non-finite)"

        ! (b) the same call asking for the record.
        best = huge(1.0_real64)
        keep = 0.0_real64
        do j = 1, rounds
            call cpu_time(t0)
            do k = 1, repeats
                keep = keep + pf_integrate(k_linear, 0.0_real64, 1.0_real64, 1.0e-6_real64, &
                                           points=pts)
            end do
            call cpu_time(t1)
            best = min(best, t1 - t0)
        end do
        print '(a,f12.3)', "  trivial integrand, with record    us/call: ", &
            1.0e6_real64*best/real(repeats, real64)
        if (keep /= keep) print '(a)', "  (checksum went non-finite)"

        ! (c) a call that actually grows the work arrays, which is what the growth policy is for.
        r = pf_integrate(k_gauss, 0.0_real64, 1.0_real64, 1.0e-12_real64)
        best = huge(1.0_real64)
        keep = 0.0_real64
        do j = 1, rounds
            call cpu_time(t0)
            do k = 1, repeats
                keep = keep + pf_integrate(k_gauss, 0.0_real64, 1.0_real64, 1.0e-12_real64)
            end do
            call cpu_time(t1)
            best = min(best, t1 - t0)
        end do
        print '(a,f12.3)', "  sharp gaussian, arrays grow       us/call: ", &
            1.0e6_real64*best/real(repeats, real64)
        print '(a,es11.2)', "  ... and its relative error                : ", &
            abs(r - k_gauss_exact())/abs(k_gauss_exact())
        if (keep /= keep) print '(a)', "  (checksum went non-finite)"

    end subroutine run_overhead

    !> What the outward walk costs on a tail, beside the count QUADPACK's change of variable takes.
    !!
    !! Counts only, no timing: the evaluation count IS the walk's cost, it is deterministic to
    !! within one rule application across compilers, and the oscillatory shape -- which neither
    !! method answers at this tolerance -- would otherwise dominate the wall clock without saying
    !! anything. The reference column is `dqagi`'s count at the same tolerances, recorded in the
    !! design document; there is no `dqagi` in this build.
    !!
    !! The last four rows are the two features the reference method loses: a narrow spike just
    !! above the lower bound, and a unit-width bump at 40 approached from three lower bounds.
    !! `dqagi` answers ZERO on those and reports convergence, which is why the walk exists; they
    !! carry no ratio, only a relative error that has to stay small.
    subroutine run_walk()

        real(real64), parameter :: RTOL = 1.0e-10_real64
        real(real64), parameter :: ATOL = 1.0e-14_real64
        !> `sin(1) - Ci(1)`, by parts.
        real(real64), parameter :: OSC_EXACT = 0.5040670619069284_real64

        real(real64) :: inf

        inf = pf_infinity()

        print '(a)', "=== pf_integrate: the outward walk on an infinite range ==="
        print '(a,es8.1,a,es8.1)', "rtol ", RTOL, ", atol ", ATOL
        print '(a)', ""
        print '(a)', "  tail shape                 neval  npanels    relerr   cvg   dqagi   ratio"

        call walk_row("exp(-x)      [1, inf)  ", k_tail_exp, 1.0_real64, inf, &
                      exp(-1.0_real64), 135)
        call walk_row("x**-1.5      [1, inf)  ", k_tail_pow15, 1.0_real64, inf, &
                      2.0_real64, 165)
        call walk_row("x**-2        [1, inf)  ", k_tail_pow2, 1.0_real64, inf, &
                      1.0_real64, 15)
        call walk_row("exp(-x**2)   [0, inf)  ", k_tail_gauss, 0.0_real64, inf, &
                      0.5_real64*sqrt(acos(-1.0_real64)), 195)
        call walk_row("sin(x)/x**2  [1, inf)  ", k_tail_osc, 1.0_real64, inf, OSC_EXACT, 14985)
        print '(a)', ""
        call walk_row("spike @1.02  [1, inf)  ", k_spike, 1.0_real64, inf, k_spike_exact(), 0)
        call walk_row("bump @40     [1, inf)  ", k_bump, 1.0_real64, inf, k_bump_exact(), 0)
        call walk_row("bump @40     [0.5, inf)", k_bump, 0.5_real64, inf, k_bump_exact(), 0)
        call walk_row("bump @40     [0, inf)  ", k_bump, 0.0_real64, inf, k_bump_exact(), 0)
        print '(a)', ""
        print '(a)', "  x**-2 is excluded from the score: the reference method's change of"
        print '(a)', "  variable turns it into a constant, which one rule application is exact"
        print '(a)', "  on. The oscillatory row is reported, not scored -- neither method"
        print '(a)', "  converges on it at this tolerance."

    end subroutine run_walk

    !> One tail shape: the count, the panels, the error and the ratio against the reference.
    subroutine walk_row(tag, fn, a, b, want, reference)
        character(len=*), intent(in) :: tag       !! names the shape in the report
        procedure(pf_integrand_func) :: fn        !! the integrand
        real(real64), intent(in)     :: a         !! lower bound
        real(real64), intent(in)     :: b         !! upper bound
        real(real64), intent(in)     :: want      !! the closed form over [a, b]
        integer, intent(in)          :: reference !! `dqagi`'s count, or 0 where it has none

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(fn, a, b, pf_tolerance(1.0e-10_real64, 1.0e-14_real64), info=info)

        if (reference > 0) then
            print '(a,a,i9,i9,es11.2,a,i8,f8.2)', "  ", tag, info%neval, info%npanels, &
                abs(r - want)/abs(want), merge("  yes", "   no", info%converged), reference, &
                real(info%neval, real64)/real(reference, real64)
        else
            print '(a,a,i9,i9,es11.2,a,a)', "  ", tag, info%neval, info%npanels, &
                abs(r - want)/abs(want), merge("  yes", "   no", info%converged), &
                "       -       -"
        end if

    end subroutine walk_row

end program benchmark_integrate
