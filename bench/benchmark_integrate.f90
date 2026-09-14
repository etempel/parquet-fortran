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

    !> Terms in `k_heavy`, which exists to make one evaluation expensive enough that the
    !! framework's own cost per evaluation stops dominating the measurement.
    integer, parameter :: HEAVY_TERMS = 40

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
                write (error_unit, '(a)') "Usage: benchmark_integrate [--mode=cost|overhead] " &
                    //"[--rounds=N] [--repeats=N]"
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

end program benchmark_integrate
