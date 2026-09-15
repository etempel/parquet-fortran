!> The integrands `test_integrate.f90` and `test/error_scenarios.f90` share, as module procedures
!> and as `pf_integrand` extensions.
!!
!! **Every callback here is a module procedure or a type-bound procedure, never an internal one.**
!! flang SIGSEGVs before the callee runs when an internal procedure is passed as an actual
!! argument, and gfortran implements one with a trampoline that makes the binary demand an
!! executable stack (`fortran-gotchas.md`). That is a property of the whole library's callback
!! convention, so the tests have to be written the way the guide tells a user to write them.
!!
!! **The closed form of every integrand is stated beside it, with its derivation**, so that no
!! test asserts a value that was read off a run. Where a value is transcendental the derivation
!! names the identity it comes from and the test asserts it to a tolerance.
!!
!! Its only library import is `use parquet_integrate`: a scenario and a test must be able to name
!! the same integrand without either of them reaching the facade.
module test_integrate_support

    use parquet_integrate, only : pf_integrand
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan

    implicit none
    private

    public :: runge, runge_exact
    public :: osc_a, osc_a_exact
    public :: log_sqrt, log_sqrt_exact
    public :: peak_c, peak_c_exact
    public :: heavy_f, heavy_f_exact
    public :: inv_pow15, inv_pow15_exact
    public :: sine, sharp_gauss, sharp_gauss_exact
    public :: x_squared, zero_integrand, unit_step, unit_step_exact
    public :: compact_bump, compact_bump_exact, far_bump, far_bump_exact
    public :: nan_at_half
    public :: tail_exp, tail_neg_exp, tail_pow2, tail_gauss, tail_osc, tail_osc_exact
    public :: rising_exp, exp_over_x, exp_over_x_exact, exp_cos, exp_cos_exact
    public :: narrow_spike, narrow_spike_exact, sliver_bump, sliver_bump_exact
    public :: full_wave, saw_sqrt, saw_sqrt_exact
    public :: inv_sqrt, inv_sqrt_exact, mild_pow, mild_pow_exact
    public :: divergent_pow, divergent_pow_finite_part
    public :: scaled_runge, exp_profile

    !> Half-width of `compact_bump`'s support, which is centred on `BUMP_CENTRE`.
    real(real64), parameter, public :: BUMP_HALF = 0.006_real64
    !> Centre of `compact_bump`; its support is `[1, 1.012]`.
    real(real64), parameter, public :: BUMP_CENTRE = 1.006_real64
    !> Where `unit_step` steps, an irrational point no abscissa lands on.
    real(real64), parameter, public :: STEP_AT = 0.7071067811865476_real64
    !> Terms in `heavy_f`, the deliberately expensive smooth integrand.
    integer, parameter, public :: HEAVY_TERMS = 40
    !> Half-width of `narrow_spike`, the feature sitting just above a lower bound of one.
    real(real64), parameter, public :: SPIKE_WIDTH = 1.0e-3_real64
    !> Centre of `narrow_spike`.
    real(real64), parameter, public :: SPIKE_AT = 1.02_real64
    !> Half-width of `sliver_bump`, whose whole support lies between the first two abscissae the
    !! start-panel search's WIDE first probe places above a lower bound of one.
    real(real64), parameter, public :: SLIVER_HALF = 5.0e-4_real64
    !> Centre of `sliver_bump`; its support is `[1.0005, 1.0015]`.
    real(real64), parameter, public :: SLIVER_AT = 1.001_real64
    !> Width of one tooth of `saw_sqrt`; four of them span `[0, 1]`.
    real(real64), parameter, public :: SAW_WIDTH = 0.25_real64

    !> Exponent of `divergent_pow`: above one, so the integral over `[0, 1]` does not exist.
    real(real64), parameter, public :: DIVERGENT_EXPONENT = 1.1_real64

    !> Runge's function scaled by an amplitude, with a counter, as the guide's object example.
    type, extends(pf_integrand) :: scaled_runge
        real(real64) :: amp = 1.0_real64 !! multiplies the integrand
        integer      :: calls = 0        !! evaluations this object has seen
    contains
        procedure :: eval => scaled_runge_eval !! Evaluates `amp/(1+25x^2)` and counts the call.
    end type scaled_runge

    !> A decaying exponential profile carrying its own parameters, with a counter.
    type, extends(pf_integrand) :: exp_profile
        real(real64) :: amp = 1.0_real64   !! value at `x = 0`
        real(real64) :: scale = 1.0_real64 !! e-folding length
        integer      :: calls = 0          !! evaluations this object has seen
    contains
        procedure :: eval => exp_profile_eval !! Evaluates `amp*exp(-x/scale)` and counts the call.
    end type exp_profile

contains

    !> Runge's function, `1/(1 + 25 x^2)`.
    function runge(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 1.0_real64/(1.0_real64 + 25.0_real64*x*x)

    end function runge

    !> Exact integral of `runge` over `[0, 1]`.
    !!
    !! `d/dx atan(5x)/5 = 1/(1 + 25 x^2)`, so the integral is `atan(5)/5`.
    pure function runge_exact() result(v)
        real(real64) :: v !! the exact value

        v = atan(5.0_real64)/5.0_real64

    end function runge_exact

    !> Case A of the engine comparison, `2/(2 + sin(10 pi x))`: smooth and oscillatory.
    function osc_a(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 2.0_real64/(2.0_real64 + sin(10.0_real64*acos(-1.0_real64)*x))

    end function osc_a

    !> Exact integral of `osc_a` over `[0, 1]`.
    !!
    !! The mean of `2/(2 + sin t)` over one period is `2/sqrt(2^2 - 1^2)`, and `10 pi x` sweeps
    !! exactly five whole periods as `x` runs from 0 to 1, so the average IS the integral.
    pure function osc_a_exact() result(v)
        real(real64) :: v !! the exact value

        v = 2.0_real64/sqrt(3.0_real64)

    end function osc_a_exact

    !> Case B, `log(x)/sqrt(x)`: an integrable singularity at the left endpoint.
    !!
    !! Zero at `x = 0` so that a caller who does evaluate the endpoint gets a number; the rule
    !! never evaluates an endpoint, so no integration reaches that branch.
    function log_sqrt(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > 0.0_real64) f = log(x)/sqrt(x)

    end function log_sqrt

    !> Exact integral of `log_sqrt` over `[0, 1]`.
    !!
    !! `d/dx (2 sqrt(x) log(x) - 4 sqrt(x)) = log(x)/sqrt(x)`; the antiderivative is 0 at `x = 0`
    !! and `-4` at `x = 1`.
    pure function log_sqrt_exact() result(v)
        real(real64) :: v !! the exact value

        v = -4.0_real64

    end function log_sqrt_exact

    !> Case C, `1/((x - 0.3)^2 + 1e-6)`: an interior peak of width about `1e-3`.
    function peak_c(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 1.0_real64/((x - 0.3_real64)**2 + 1.0e-6_real64)

    end function peak_c

    !> Exact integral of `peak_c` over `[0, 1]`.
    !!
    !! `integral dx/((x-c)^2 + e^2) = atan((x-c)/e)/e`, with `c = 0.3` and `e = 1e-3`, so the
    !! value is `1000*(atan(0.7/1e-3) - atan(-0.3/1e-3))`.
    pure function peak_c_exact() result(v)
        real(real64) :: v !! the exact value

        v = 1000.0_real64*(atan(700.0_real64) - atan(-300.0_real64))

    end function peak_c_exact

    !> Case F, `sum_{i=1}^{40} sin(i x) exp(-0.01 i x)/i`: smooth, and deliberately expensive.
    function heavy_f(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        integer      :: i
        real(real64) :: ri

        f = 0.0_real64
        do i = 1, HEAVY_TERMS
            ri = real(i, real64)
            f = f + sin(ri*x)*exp(-0.01_real64*ri*x)/ri
        end do

    end function heavy_f

    !> Exact integral of `heavy_f` over `[0, 1]`.
    !!
    !! Term by term, with `a = -0.01 i` and `b = i`,
    !! `integral_0^1 exp(a x) sin(b x) dx = (exp(a)(a sin b - b cos b) + b)/(a^2 + b^2)`.
    !! Dividing by `i` and using `a^2 + b^2 = i^2 (1 + 1e-4)` leaves
    !! `(exp(-0.01 i)(-0.01 sin i - cos i) + 1)/(i^2 (1 + 1e-4))`.
    pure function heavy_f_exact() result(v)
        real(real64) :: v !! the exact value

        integer      :: i
        real(real64) :: ri

        v = 0.0_real64
        do i = 1, HEAVY_TERMS
            ri = real(i, real64)
            v = v + (exp(-0.01_real64*ri)*(-0.01_real64*sin(ri) - cos(ri)) + 1.0_real64) &
                /(ri*ri*1.0001_real64)
        end do

    end function heavy_f_exact

    !> `x**-1.5`, the integrand qfeet integrates over six decades in `log x`.
    function inv_pow15(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x**(-1.5_real64)

    end function inv_pow15

    !> Exact integral of `inv_pow15` over `[lo, hi]`.
    !!
    !! `d/dx (-2 x^-0.5) = x^-1.5`, so the value is `2 (lo^-0.5 - hi^-0.5)`.
    pure function inv_pow15_exact(lo, hi) result(v)
        real(real64), intent(in) :: lo !! lower bound
        real(real64), intent(in) :: hi !! upper bound
        real(real64)             :: v  !! the exact value

        v = 2.0_real64*(lo**(-0.5_real64) - hi**(-0.5_real64))

    end function inv_pow15_exact

    !> One full period of a sine over `[0, 1]`, `sin(2 pi x)`: the integral is exactly zero, and
    !! it is zero by CANCELLATION -- the two halves are `+1/pi` and `-1/pi`.
    !!
    !! That is what makes it the fixture for the `atol/n_pieces` split. With breakpoints the sum
    !! is near zero while its pieces are not, so `rtol` on the sum means nothing and `atol` is the
    !! whole of the tolerance; a piece integrated to the caller's full `atol` instead of its share
    !! leaves the sum out of tolerance with every piece reporting convergence.
    function full_wave(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = sin(2.0_real64*acos(-1.0_real64)*x)

    end function full_wave

    !> `1/sqrt(mod(x, SAW_WIDTH))`: an inverse-square-root singularity at the left end of every
    !! tooth, and a smooth decay in between.
    !!
    !! **The fixture the `atol/n_pieces` split is actually measured on.** Cut at the teeth, every
    !! piece is an endpoint singularity the bisection resolves SLOWLY, so the engine stops at the
    !! first partition inside the tolerance and the error estimate it returns sits just under the
    !! budget it was given rather than far below it. That is what makes the split observable: a
    !! piece given four times its share stops four times earlier and reports four times the error.
    !! An integrand one rule application answers exactly -- `full_wave` -- reports the same 1e-15
    !! whatever tolerance it is handed, and cannot see the difference at all.
    !!
    !! Zero at a tooth boundary so that a caller who evaluates one gets a number; the rule never
    !! evaluates an endpoint, and with the breakpoints at the teeth no abscissa lands on one.
    function saw_sqrt(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        real(real64) :: t

        t = mod(x, SAW_WIDTH)
        f = 0.0_real64
        if (t > 0.0_real64) f = 1.0_real64/sqrt(t)

    end function saw_sqrt

    !> Exact integral of `saw_sqrt` over `[0, 1]`.
    !!
    !! One tooth is `integral of t**-0.5 dt` from 0 to `SAW_WIDTH`, which is `2*sqrt(SAW_WIDTH)`,
    !! and `[0, 1]` holds `1/SAW_WIDTH` teeth: `2*sqrt(SAW_WIDTH)/SAW_WIDTH`, which is 4.
    pure function saw_sqrt_exact() result(v)
        real(real64) :: v !! the exact value

        v = 2.0_real64*sqrt(SAW_WIDTH)/SAW_WIDTH

    end function saw_sqrt_exact

    !> `1/sqrt(x)`: the plainest integrable endpoint singularity, exactly 2 over `[0, 1]`.
    !!
    !! Zero at `x = 0` so that a caller who does evaluate the endpoint gets a number; the rule
    !! never evaluates an endpoint, so no integration reaches that branch.
    function inv_sqrt(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > 0.0_real64) f = 1.0_real64/sqrt(x)

    end function inv_sqrt

    !> Exact integral of `inv_sqrt` over `[0, 1]`: `[2 sqrt(x)]` from 0 to 1.
    pure function inv_sqrt_exact() result(v)
        real(real64) :: v !! the exact value

        v = 2.0_real64

    end function inv_sqrt_exact

    !> `x**-0.9`: an endpoint singularity mild enough to converge and steep enough to be dear.
    !!
    !! The bisection alone pays thousands of evaluations here and more as the tolerance tightens,
    !! because halving the interval next to the singularity buys a fixed factor rather than a
    !! fixed number of digits. It is the shape the extrapolation exists for.
    function mild_pow(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > 0.0_real64) f = x**(-0.9_real64)

    end function mild_pow

    !> Exact integral of `mild_pow` over `[0, 1]`: `[x**0.1/0.1]` from 0 to 1.
    pure function mild_pow_exact() result(v)
        real(real64) :: v !! the exact value

        v = 10.0_real64

    end function mild_pow_exact

    !> `x**-1.1`: a DIVERGENT integral over `[0, 1]`, and the fixture for `PF_INT_DIVERGENT`.
    !!
    !! **The one fixture here whose integral does not exist.** `integral of x**-p` over `[0, 1]`
    !! diverges for every `p >= 1`, and this is the shape QUADPACK's divergence test was written
    !! for: the extrapolation table sees the partial sums running away rather than settling, and
    !! says so in a couple of hundred evaluations.
    !!
    !! The divergence is found WITH the extrapolation, which is the default. The plain bisection
    !! keeps halving the interval next to zero and takes the abscissae below `1e-280`, where
    !! `x**-1.1` overflows to an infinity and the engine's non-finite screen ends the integration
    !! with `PF_INT_BAD_VALUE` -- the contrast `test_status_divergent` asserts, and itself part of
    !! what the default buys.
    function divergent_pow(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > 0.0_real64) f = x**(-DIVERGENT_EXPONENT)

    end function divergent_pow

    !> What the extrapolation returns for `divergent_pow`, which is NOT its integral.
    !!
    !! The epsilon table accelerates the partial sums of a series that does not converge, and
    !! what it lands on is the analytic continuation of `integral of x**-p from e to 1`, that is
    !! `(e**(1-p) - 1)/(p - 1)` as `e -> 0` with the divergent half discarded: `-1/(p - 1)`, here
    !! `-10`. A finite number, of the wrong sign for a positive integrand, and the reason the
    !! STATUS rather than the result is what tells a caller the integral does not exist.
    pure function divergent_pow_finite_part() result(v)
        real(real64) :: v !! the value the table returns

        v = -1.0_real64/(DIVERGENT_EXPONENT - 1.0_real64)

    end function divergent_pow_finite_part

    !> Plain `sin(x)`, whose integral is elementary at any bounds.
    function sine(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = sin(x)

    end function sine

    !> A gaussian of width `0.01` centred at `0.9`: narrow, but wide enough for the first rule
    !! application on `[0, 1]` to find it.
    function sharp_gauss(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-((x - 0.9_real64)/0.01_real64)**2)

    end function sharp_gauss

    !> Exact integral of `sharp_gauss` over `[0, 1]`.
    !!
    !! `integral exp(-((x-c)/w)^2) dx = w sqrt(pi)/2 erf((x-c)/w)`, so over `[0, 1]` the value is
    !! `0.01 sqrt(pi)/2 (erf(10) + erf(90))`. Both `erf` values are 1 to well below `real64`
    !! resolution, and the expression is written out rather than simplified so that the derivation
    !! is visible.
    pure function sharp_gauss_exact() result(v)
        real(real64) :: v !! the exact value

        v = 0.01_real64*sqrt(acos(-1.0_real64))/2.0_real64 &
            *(erf(10.0_real64) + erf(90.0_real64))

    end function sharp_gauss_exact

    !> `x*x`, whose integral over `[0, 1]` is `1/3`.
    function x_squared(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x*x

    end function x_squared

    !> The identically zero integrand.
    function zero_integrand(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate, unused
        real(real64)             :: f !! always zero

        f = 0.0_real64*x

    end function zero_integrand

    !> A unit step at `STEP_AT`: zero below, one above.
    !!
    !! The step sits at an irrational point, so no abscissa of any bisection lands exactly on it
    !! and the discontinuity is what the engine's round-off counters see.
    function unit_step(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = 0.0_real64
        if (x > STEP_AT) f = 1.0_real64

    end function unit_step

    !> Exact integral of `unit_step` over `[0, 1]`: the width of the part that is one.
    pure function unit_step_exact() result(v)
        real(real64) :: v !! the exact value

        v = 1.0_real64 - STEP_AT

    end function unit_step_exact

    !> A compactly supported bump on `[1, 1.012]`, zero everywhere else.
    !!
    !! `(1 - u^2)^2` in `u = (x - centre)/half`, which is smooth enough at the edges of its
    !! support for the value and its first derivative to be continuous.
    function compact_bump(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        real(real64) :: u

        f = 0.0_real64
        u = (x - BUMP_CENTRE)/BUMP_HALF
        if (abs(u) < 1.0_real64) f = (1.0_real64 - u*u)**2

    end function compact_bump

    !> Exact integral of `compact_bump` over any range containing its support.
    !!
    !! `integral_{-1}^{1} (1-u^2)^2 du = 2 - 4/3 + 2/5 = 16/15`, and `dx = half du`, so the value
    !! is `half*16/15 = 0.0064`.
    pure function compact_bump_exact() result(v)
        real(real64) :: v !! the exact value

        v = BUMP_HALF*16.0_real64/15.0_real64

    end function compact_bump_exact

    !> A unit-width gaussian bump centred at 40, the far-feature integrand of the design's
    !! blind-spot measurement.
    function far_bump(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-(x - 40.0_real64)**2)

    end function far_bump

    !> Exact integral of `far_bump` over a range that contains the bump entirely.
    !!
    !! `integral exp(-(x-c)^2) dx` over the whole line is `sqrt(pi)`, and the bump is below
    !! `real64` resolution more than about 27 away from its centre.
    pure function far_bump_exact() result(v)
        real(real64) :: v !! the exact value

        v = sqrt(acos(-1.0_real64))

    end function far_bump_exact

    !> An integrand that returns a NaN at `x = 0.5` and a finite value everywhere else.
    !!
    !! The NaN is built with `ieee_value`, never by arithmetic: nagfor's default `-ieee=stop`
    !! makes `0/0` a process abort in the FIXTURE rather than in the code under test.
    function nan_at_half(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value, NaN near 0.5

        if (abs(x - 0.5_real64) < 0.05_real64) then
            f = ieee_value(1.0_real64, ieee_quiet_nan)
        else
            f = 1.0_real64
        end if

    end function nan_at_half


    ! ---- the infinite ranges -------------------------------------------------------------------

    !> `exp(-x)`: the plainest decaying tail. Integral over `[a, inf)` is `exp(-a)`.
    function tail_exp(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x)

    end function tail_exp

    !> `-exp(-x)`: the same tail negated, which is what catches a negligibility test that lost
    !! its `abs` and steps straight over an integrand that is everywhere below zero.
    function tail_neg_exp(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = -exp(-x)

    end function tail_neg_exp

    !> `x**-2`: an algebraic tail. Integral over `[a, inf)` is `1/a`.
    function tail_pow2(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x**(-2.0_real64)

    end function tail_pow2

    !> `exp(-x**2)`: a gaussian tail, the one integrand of this set that is also integrable over
    !! the whole line.
    function tail_gauss(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x*x)

    end function tail_gauss

    !> `sin(x)/x**2`: an oscillatory tail that no method answers to a tight tolerance inside a
    !! reasonable budget, and which is therefore asserted loosely and NOT asserted to converge.
    function tail_osc(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = sin(x)/(x*x)

    end function tail_osc

    !> `sin(1) - Ci(1)`, the integral of `sin(x)/x**2` over `[1, inf)`, by parts:
    !! `integral sin(x)/x^2 = -sin(x)/x + integral cos(x)/x`, which at the bounds is
    !! `sin(1) - Ci(1)`. qfeet asserts the same literal.
    pure function tail_osc_exact() result(v)
        real(real64) :: v !! the exact value

        v = 0.5040670619069284_real64

    end function tail_osc_exact

    !> `exp(x)`: rising, and integrable only towards `-infinity`. Integral over `(-inf, b]` is
    !! `exp(b)`, so over `(-inf, 0]` it is one.
    function rising_exp(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(x)

    end function rising_exp

    !> `exp(-x)/x`: reference integrand D of the design's engine comparison, whose integral over
    !! `[1, inf)` is the exponential integral `E1(1)`.
    function exp_over_x(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x)/x

    end function exp_over_x

    !> `E1(1) = 0.219383934395520274...`, Abramowitz and Stegun 5.1.1, to the digits `real64`
    !! carries.
    pure function exp_over_x_exact() result(v)
        real(real64) :: v !! the exact value

        v = 0.2193839343955203_real64

    end function exp_over_x_exact

    !> `exp(-x)*cos(x)`: reference integrand E, an oscillatory tail that still converges quickly.
    function exp_cos(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-x)*cos(x)

    end function exp_cos

    !> `1/2`: the antiderivative of `exp(-x) cos(x)` is `exp(-x)(sin(x) - cos(x))/2`, which is
    !! `-1/2` at zero and zero at infinity.
    pure function exp_cos_exact() result(v)
        real(real64) :: v !! the exact value

        v = 0.5_real64

    end function exp_cos_exact

    !> A gaussian spike of half-width `SPIKE_WIDTH` centred at `SPIKE_AT`, just above a lower
    !! bound of one.
    !!
    !! This is the shape the start-panel search's NARROW retry exists for: the first panel it
    !! tries spans a whole factor of e, in which 21 points see nothing at all.
    function narrow_spike(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = exp(-((x - SPIKE_AT)/SPIKE_WIDTH)**2)

    end function narrow_spike

    !> A compactly supported bump in the sliver `[1.0005, 1.0015]`, which is EXACTLY zero
    !! outside it.
    !!
    !! This is the shape the start-panel search's narrow retry exists for, and the only one in
    !! this suite that needs it. The search's first probe spans `[1, e]`, whose leftmost abscissa
    !! is at about `1.0022` -- past the whole support -- so all 21 of its values are exactly zero
    !! and the panel reads as negligible. Widening from there moves every abscissa further right
    !! and never comes back, so without the retry the walk answers ZERO and reports convergence.
    !! The retry's much narrower panel `[1, 1.105]` puts an abscissa at about `1.0013`, inside the
    !! support, and the bump is found.
    !!
    !! Compact rather than gaussian on purpose: a gaussian underflows to zero so fast that a probe
    !! missing it by a few half-widths reads zeros anyway, which makes the fixture depend on the
    !! exponent range rather than on the abscissae.
    function sliver_bump(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        real(real64) :: u

        f = 0.0_real64
        u = (x - SLIVER_AT)/SLIVER_HALF
        if (abs(u) < 1.0_real64) f = (1.0_real64 - u*u)**2

    end function sliver_bump

    !> `SLIVER_HALF*16/15`: the integral of `(1-u^2)^2` over `[-1, 1]` is `16/15`, and `du` is
    !! `dx/SLIVER_HALF`.
    pure function sliver_bump_exact() result(v)
        real(real64) :: v !! the exact value

        v = SLIVER_HALF*16.0_real64/15.0_real64

    end function sliver_bump_exact

    !> Integral of `narrow_spike` over `[1, inf)`.
    !!
    !! `SPIKE_WIDTH*sqrt(pi)/2*(1 + erf((SPIKE_AT - 1)/SPIKE_WIDTH))`; the `erf` is of twenty, so
    !! the bracket is two to every bit a `real64` carries, but it is written out rather than
    !! folded so that moving the spike moves the reference with it.
    pure function narrow_spike_exact() result(v)
        real(real64) :: v !! the exact value

        v = SPIKE_WIDTH*sqrt(acos(-1.0_real64))*0.5_real64 &
            *(1.0_real64 + erf((SPIKE_AT - 1.0_real64)/SPIKE_WIDTH))

    end function narrow_spike_exact

    !> Evaluates `amp/(1 + 25 x^2)` and counts the call.
    function scaled_runge_eval(this, x) result(f)
        class(scaled_runge), intent(inout) :: this !! the integrand object
        real(real64), intent(in)           :: x    !! point at which to evaluate
        real(real64)                       :: f    !! the integrand value

        this%calls = this%calls + 1
        f = this%amp/(1.0_real64 + 25.0_real64*x*x)

    end function scaled_runge_eval

    !> Evaluates `amp*exp(-x/scale)` and counts the call.
    function exp_profile_eval(this, x) result(f)
        class(exp_profile), intent(inout) :: this !! the integrand object
        real(real64), intent(in)          :: x    !! point at which to evaluate
        real(real64)                      :: f    !! the integrand value

        this%calls = this%calls + 1
        f = this%amp*exp(-x/this%scale)

    end function exp_profile_eval

end module test_integrate_support
