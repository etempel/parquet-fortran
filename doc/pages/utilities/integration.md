---
title: Numerical integration with pf_integrate
---

`parquet_integrate` integrates a function of one `real64` variable, adaptively, to a tolerance you
ask for. It reaches no reader, no writer and no setting: a program that has a function in hand can
`use parquet_integrate` and integrate it, compiling three Fortran files and nothing of the Arrow
stack. `use parquet` brings it in too, so nothing here needs a second import. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

Underneath is QUADPACK's 21-point Gauss-Kronrod rule with adaptive bisection, vendored and
reworked for this library; `src/parquet_integrate_engine.f90`'s header carries the attribution and
the full list of changes.

## Quick example

```fortran
use parquet_integrate
use iso_fortran_env, only : real64

type(pf_integration_info) :: info
real(real64) :: r

r = pf_integrate(gaussian, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info)
print '(a, es22.15, a, i0, a)', 'integral = ', r, ' in ', info%neval, ' evaluations'
```

with the integrand a module procedure:

```fortran
function gaussian(x) result(f)
    real(real64), intent(in) :: x
    real(real64)             :: f
    f = exp(-x*x)
end function gaussian
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
res = pf_integrate(f, a, b, rtol, [max_neval], [converged], [info], [points], [context], &
                   [log_base], [extrapolate])
res = pf_integrate(f, a, b, tol,  [max_neval], [converged], [info], [points], [context], &
                   [log_base], [extrapolate])
```

## The integrand

Two ways to supply it, and the choice is about parameters rather than taste.

**A plain function** matching `pf_integrand_func` — one `real64` in, one `real64` out — is the
short form, for an integrand that needs nothing but `x`.

**An object extending `pf_integrand`** carries its parameters as components. Extend the type, add
what the integrand needs, and implement `eval`:

```fortran
type, extends(pf_integrand) :: exp_profile
    real(real64) :: amp = 1.0_real64
    real(real64) :: scale = 1.0_real64
    integer      :: calls = 0
contains
    procedure :: eval => exp_profile_eval
end type exp_profile

function exp_profile_eval(this, x) result(f)
    class(exp_profile), intent(inout) :: this
    real(real64), intent(in)          :: x
    real(real64)                      :: f
    this%calls = this%calls + 1
    f = this%amp*exp(-x/this%scale)
end function exp_profile_eval
```

The passed-object dummy is `intent(inout)`, so `eval` may keep a call counter or a cache, and an
extension must declare the same intent. Pass the object where the function would go:

```fortran
type(exp_profile) :: p
p%amp = 2.0_real64
p%scale = 3.0_real64
r = pf_integrate(p, 0.0_real64, 10.0_real64, 1.0e-10_real64)
```

**The callback must be a module procedure or a type-bound procedure, never an internal one.** An
internal procedure passed as an actual argument is a hard crash under one of this project's
compilers and makes the binary demand an executable stack under another. An object is the
supported way to carry context; a module variable is the other.

**One object per thread.** `eval` may update the object, so two threads sharing one race. Give
each thread its own and concurrent integrations are independent — nothing else in this module
holds state between calls.

## Ranges

Both bounds are `real64` and both are required. `a == b` returns zero without evaluating the
integrand at all; `a > b` aborts rather than silently returning the negative of the integral.

**`log_base=.true.` integrates in `log x`**, for a finite range spanning many decades. It needs a
positive lower bound and two finite bounds. A power law is a straight line in `log x`, so a range
the linear form needs tens of subintervals for can collapse to a single rule application:

```fortran
r = pf_integrate(power_law, 1.0e-3_real64, 1.0e3_real64, 1.0e-10_real64, log_base=.true.)
```

The record (below) still reports the abscissae in your own `x`, not in `log x`.

**An infinite range is spelled with `pf_infinity()`** as a bound, which returns `+Infinity`.
There are four spellings and they are the same call:

```fortran
r = pf_integrate(f, 0.0_real64, 1.0_real64, 1.0e-8_real64)           ! [0, 1]
r = pf_integrate(f, 1.0_real64, pf_infinity(), 1.0e-8_real64)        ! [1, +inf)
r = pf_integrate(f, -pf_infinity(), 0.0_real64, 1.0e-8_real64)       ! (-inf, 0]
r = pf_integrate(f, -pf_infinity(), pf_infinity(), 1.0e-8_real64)    ! (-inf, +inf)
```

Both bounds the same infinity aborts, and so does `log_base` on an infinite range: the walk below
works in `log x` already, by its own transform.

**An infinite range is walked outward, not transformed.** The integrator looks for a first panel
the integrand is not negligible on, integrates it, and then steps outward one factor of e at a
time, stopping when the tail estimated from the observed decay is inside the tolerance. That is
why a feature far along the range is found rather than stepped over — see
[What the integrator cannot see](#what-the-integrator-cannot-see). `(-inf, b]` is the same walk
with every evaluation mirrored, and `(-inf, +inf)` is two walks from zero.

**Build an infinite bound with `pf_infinity()`, never by overflowing an expression.** A bound
computed as, say, `1.0e308_real64*10` does reach `+Infinity` on the way, but it raises
`IEEE_OVERFLOW` doing so, which some compilers are configured to treat as fatal. A bound that
overflowed silently inside a caller's own arithmetic is the other hazard, and it is why
`max_panels` on a finite range aborts instead of being ignored: a call that caps the walk of a
range that has no walk usually meant an infinite bound that did not survive.

## Tolerances

Pass either a bare `rtol`, or a `pf_tolerance` carrying `rtol` and `atol` together. At least one
of the two must be positive. Convergence is QUADPACK's own test:

```
estimated error <= max(atol, rtol*abs(result))
```

**`rtol` alone means nothing for an integral whose value is near zero**, because a relative
tolerance on a result of `1e-30` asks for an accuracy no arithmetic can deliver. That is what
`atol` is for:

```fortran
r = pf_integrate(f, a, b, pf_tolerance(rtol=0.0_real64, atol=1.0e-10_real64))
```

**There is a floor.** An `rtol` below `50*epsilon(1.0_real64)` with no positive `atol` is refused,
because no double-precision quadrature can report having met it. The same `rtol` with a positive
`atol` is accepted: `atol` is then what convergence rests on.

## Budget and outcome

`max_neval` bounds the integrand evaluations; it defaults to 100000. **It is a ceiling, not a
suggestion**: the count never exceeds it on a finite range, and an infinite range may overshoot it
by at most one rule application, because a panel costs its 21 points even when five of them would
have been enough. Reaching it is a status, not an error.

`max_panels` bounds the panels ONE walk of an infinite range may use, counting the first panel the
search accepted; it defaults to 50, and reaching it is `PF_INT_LIMIT` in the same way. It applies
only to an infinite range — passing it with two finite bounds aborts — and on `(-inf, +inf)`,
which is two walks, it caps each of them.

`converged=` is the short answer. `info=` is the long one, a `pf_integration_info` carrying:

| Component | What it says |
|---|---|
| `status` | one of the `PF_INT_*` codes below |
| `converged` | `status == PF_INT_OK` |
| `extrapolated` | the result came from the epsilon table rather than the partition sum |
| `abserr` | the engine's own estimate of the absolute error |
| `partition_integral` | the plain sum over the final partition |
| `neval` | integrand evaluations, counted |
| `nsub` | subintervals in the final partition, summed over the walk's panels |
| `npanels` | 1 on a finite range; the panels the walk used, both halves counted, on an infinite one |

**Nothing is ever printed.** This module reads no verbosity setting and has no message stream; it
reports through these values, and speaks only by aborting when a caller contract is broken.

| Code | Means | What to do |
|---|---|---|
| `PF_INT_OK` | the requested accuracy was achieved | nothing |
| `PF_INT_LIMIT` | `max_neval` or `max_panels` ran out first | raise the cap, or loosen the tolerance |
| `PF_INT_ROUNDOFF` | round-off prevents the tolerance being met | loosen the tolerance; the result is as good as the arithmetic allows |
| `PF_INT_BAD_INTEGRAND` | the integrand behaves extremely badly somewhere | split the range at the offending point |
| `PF_INT_NO_CONVERGENCE` | round-off in the extrapolation table | loosen the tolerance, or turn `extrapolate` off |
| `PF_INT_DIVERGENT` | the integral is probably divergent or very slowly convergent | check that the integral exists |

A non-`PF_INT_OK` status still returns the engine's best estimate, and it is often a good one: a
discontinuity inside the range reports `PF_INT_BAD_INTEGRAND` while returning an answer accurate
to many digits. Read `abserr` rather than assuming the worst.

## The evaluation record

Pass `points=` and you get back a `pf_integration_points`: every abscissa, weight and value of the
**final partition**, with `n` saying how many are in use.

```fortran
type(pf_integration_points) :: pts
real(real64) :: r, again

r = pf_integrate(f, a, b, 1.0e-10_real64, points=pts)
again = sum(pts%w(1:pts%n)*pts%f(1:pts%n))     ! reproduces r
```

**The weighted sum reproduces the integral**, because the weights carry everything the rule
applied: the Kronrod weight, the subinterval's half-length and the Jacobian of `log_base`. The
record is a quadrature rule for your integrand in your own variable, so you can re-weight it,
plot it, or integrate a second function sampled at the same points.

Two things to know. It is the final partition, not a log of every evaluation — a subinterval that
was bisected is represented by its two children, never by both itself and them, because summing
both would count that region twice. And it reproduces `info%partition_integral` exactly; it
reproduces the returned result too whenever `info%extrapolated` is false, which is the default.

`%append` joins two records, for two adjacent ranges integrated separately:

```fortran
r_left = pf_integrate(f, 0.0_real64, 1.0_real64, 1.0e-10_real64, points=left)
r_right = pf_integrate(f, 1.0_real64, 2.0_real64, 1.0e-10_real64, points=right)
call left%append(right)                        ! now a rule for [0, 2]
```

Recording costs three `real64` per evaluation and happens only when `points=` is present. A call
without it allocates no record at all.

## The extrapolation

`extrapolate=.true.` turns on QUADPACK's Wynn-epsilon extrapolation. It pays for itself on one
shape — an integrable singularity at an endpoint, such as `log(x)/sqrt(x)` on `[0, 1]` — where it
cuts the cost by an order of magnitude. On a smooth integrand and on an interior peak it
contributes nothing, and it is off by default.

With it off, the result is always the plain partition sum, which is what the record reproduces
exactly. With it on, `info%extrapolated` tells you when the returned result came from the epsilon
table instead.

## What the integrator cannot see

**A feature narrower than the spacing of the first rule application's 21 points is integrated as
zero, and reported as converged.** Every sample misses it, the error estimate is therefore zero,
and nothing is ever refined. This is a property of the input, not a defect: no adaptive scheme
that starts by sampling can find a feature it never samples.

A worked example. A bump occupying `[1, 1.012]`, integrated over a range a thousand times wider:

```fortran
r = pf_integrate(bump, 1.0_real64, 10.0_real64, 1.0e-8_real64, info=info)
! r is exactly 0, info%converged is .true., info%neval is 21
```

Fit the range to the feature and the same integrand is integrated correctly:

```fortran
r = pf_integrate(bump, 1.0_real64, 1.05_real64, 1.0e-8_real64)
! r is the right answer
```

Two things help. **`points=` is the diagnostic**: a record whose abscissae all sit far from where
you expected the integrand to do something tells you exactly what happened. And **splitting the
range yourself** at a point near the feature puts a rule application where it is needed — the
integral of the parts is the integral of the whole, and `%append` joins the records.

**An infinite range is the case where this goes better, not worse.** The obvious way to integrate
to infinity is to map the range onto `(0, 1]` and bisect there, which is what QUADPACK's own
infinite-range routine does. That transform packs everything beyond about twenty past the lower
bound into the last two abscissae of the first rule application, so a unit-width feature at 40 —
and every feature narrower than about a fifth of its distance from the bound — falls between two
samples. The answer comes back as zero, converged, in a few dozen evaluations.

The outward walk does not have that shape. It samples 21 points per factor of e, all the way out,
so the same bump is found from any lower bound:

```fortran
r = pf_integrate(bump_at_40, 0.0_real64, pf_infinity(), 1.0e-8_real64)
! r is sqrt(pi), the right answer
```

What remains is the narrower blind spot above: a feature between the samples of one panel. The
search for the first panel is built around exactly that — when its first, wide probe finds
nothing it tries a much NARROWER panel before trying wider ones, because an integrand that falls
off far faster than the first guess assumed lives in a sliver just above the bound.

## What aborts

Every abort message begins `pf_integrate: `, and carries ` (context: ...)` when `context=` was
given. `context` is capped at 100 characters. These are caller-contract violations: they are
refused rather than answered, because every one of them means the call did not say what it meant.

| Condition | Message |
|---|---|
| `rtol` NaN, infinite or negative | `rtol must be a finite, non-negative number` |
| `atol` NaN, infinite or negative | `atol must be a finite, non-negative number` |
| both tolerances zero | `at least one of rtol and atol must be positive` |
| `rtol` below `50*epsilon` with no `atol` | `rtol below 50*epsilon needs a positive atol` |
| `max_neval < 1` | `max_neval must be positive` |
| `max_neval` above `huge(1)/42` | `max_neval must not exceed huge(1)/42` |
| `max_panels < 1` | `max_panels must be positive` |
| `max_panels` with two finite bounds | `max_panels applies only to an infinite range` |
| a NaN bound | `integration bounds must not be NaN` |
| `a > b` | `lower bound must not exceed the upper bound` |
| both bounds the same infinity | `bounds must not both be the same infinity` |
| `log_base` with a non-finite bound | `log_base applies only to a finite range` |
| `log_base` with `a <= 0` | `lower bound must be positive when integrating in log x` |
| the integrand returned a NaN or an infinity | `the integrand returned a non-finite value at x = ...` |

**The last one is worth planning for.** An interior singularity that happens to land exactly on an
abscissa — `1/sqrt(abs(x - 0.5))` on `[0, 1]` puts the centre point on it — reaches it. Split the
range at the singularity instead, which is also QUADPACK's own advice; an *endpoint* singularity
needs nothing, because the rule never evaluates an endpoint.

The abort is taken under a named `critical`, so one thread aborts rather than several, and the
exit status stays meaningful when the call was inside a parallel region.

## Accuracy against cost

In general terms, and measured by `bench/benchmark_integrate.sh` rather than stated here: a smooth
integrand costs a few hundred evaluations to reach ten digits, and the cost grows slowly as the
tolerance tightens, because the underlying rule is exact for polynomials up to degree 31. An
endpoint singularity is the expensive case, and the one the extrapolation turns from thousands of
evaluations into hundreds. Integrating a power law in `log x` can be an order of magnitude cheaper
than in linear `x`.

**A decaying tail costs a few hundred evaluations** — the walk pays 21 points per factor of e, and
a few probes for the panel it starts from, so what sets the price is how many factors of e the
integrand takes to die away. An exponential tail is cheap and an algebraic one several times
dearer. Against the transform-based alternative the walk is comparable on the tails both methods
answer, and it is the only one of the two that answers the tails with a feature in them. An
oscillatory tail is the case neither method answers at a tight tolerance: it exhausts the budget,
reports `PF_INT_LIMIT`, and still returns a good estimate. The `walk` mode of the benchmark is
where these come from.

No figure on this page is compiler- or machine-specific; run the benchmark on the machine you care
about.

## Thread safety

`parquet_integrate` is reentrant. It has no variable that is not a compile-time constant, every
work array is a local of the call, and the only state that outlives a call is your own integrand
object. Integrate from as many threads as you like, giving each its own `pf_integrand` object; the
plain-function form shares nothing at all.

See [Thread safety](../operating/thread-safety.html) for how this sits beside the rest of the
library.

## Where the engine comes from

The rule, the bisection driver, the error-ordered subinterval list and the epsilon table are
QUADPACK's (Piessens, de Doncker-Kapenga, Ueberhuber and Kahaner, 1983; public domain), as
modernised by Jacob Williams under BSD-3-Clause. They were reworked for this library: the
integrand became a `class(pf_integrand)` dummy so parameters travel with it, the evaluation counter
became real rather than a formula, the work arrays grow instead of being sized up front, every
diagnostic became a status code or an abort, and every evaluation is screened for a non-finite
value. The numerical logic — the abscissae, the weights, the round-off tests, the extrapolation
table — is transcribed unchanged, and the engine file's header lists every deviation.
