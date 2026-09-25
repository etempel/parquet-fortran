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

type(pf_integrate_info) :: info
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
res = pf_integrate(f, a, b, rtol, [atol], [max_neval], [log_base], [extrapolate], &
                   [max_panels], [breakpoints], [converged], [info], [points], [context])
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

function exp_profile_eval(self, x) result(f)
    class(exp_profile), intent(inout) :: self
    real(real64), intent(in)          :: x
    real(real64)                      :: f
    self%calls = self%calls + 1
    f = self%amp*exp(-x/self%scale)
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

**The callback must be a module procedure or a type-bound procedure, never an internal one**, and
one object per thread — see [Solver conventions](solvers.html#the-function-you-supply), which
states both rules once for all four solver modules.

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

### Cutting the range: `breakpoints=`

**`breakpoints=` is how you name a feature rather than hope it is found.** It takes a rank-1
`real64` of interior points; the range is cut at each of them and every piece is integrated on its
own, by the path its own bounds select. The results, the error estimates and the records are
concatenated in order.

```fortran
r = pf_integrate(bump_at_40, 0.0_real64, 1000.0_real64, 1.0e-8_real64, atol=1.0e-14_real64, &
                 breakpoints=[30.0_real64, 50.0_real64])
```

Without the cuts that call answers zero and reports convergence, for the reason
[What the integrator cannot see](#what-the-integrator-cannot-see) gives; with them, the piece
holding the bump gets a rule application of its own and the answer is right. Nothing else about
the call changes: the tolerance, the budget and the record contract all mean what they meant.

The breakpoints must be finite, distinct and strictly inside the range; anything else aborts. They
are sorted for you, so the order you list them in does not matter. They may be given on an
infinite range too, where the pieces are `(-inf, p1]`, `[p1, p2]`, ..., `[pn, +inf)` and the piece
that reaches an infinity goes through the walk:

```fortran
r = pf_integrate(f, 0.0_real64, pf_infinity(), 1.0e-10_real64, breakpoints=[1.0_real64])
```

**Reach for `breakpoints=` when a feature would otherwise be MISSED, not merely when it is
awkward.** The adaptive bisection finds an interior kink, peak or jump on its own, and cutting the
range there makes the call dearer rather than cheaper: it replaces one integration that was
already converging with two independent ones, each paying for its own first rule application and
neither able to use what the other learned. `sqrt(abs(x - 1/3))` on `[0, 1]` costs about twice as
much cut at `1/3` as left alone. What a cut buys is the case above — a feature the first rule
application does not sample at all, which on a finite range means one narrower than the spacing of
its 21 points, and on an infinite range means the shapes
[What the integrator cannot see](#what-the-integrator-cannot-see) describes. `bench/benchmark_integrate.sh`'s
`walk` mode measures both sides of this.

**Each piece is integrated to `rtol` of itself and to `atol` divided by the number of pieces**, so
the sum meets both tolerances whenever every piece meets its own. That division is what makes
`atol` mean what it says on a piecewise sum; `converged` is true only when every piece converged,
and `info` totals `neval`, `nsub` and `npanels` over them. The whole evaluation budget is carried
from one piece to the next, so `max_neval` still bounds the call rather than each piece.

## Tolerances

`rtol` is the required fourth argument and `atol` an optional one after it. At least one of the
two must be positive. Convergence is QUADPACK's own test:

```
estimated error <= max(atol, rtol*abs(result))
```

**`rtol` alone means nothing for an integral whose value is near zero**, because a relative
tolerance on a result of `1e-30` asks for an accuracy no arithmetic can deliver. That is what
`atol` is for:

```fortran
r = pf_integrate(f, a, b, rtol=0.0_real64, atol=1.0e-10_real64)
```

**There is a floor.** An `rtol` below `50*epsilon(1.0_real64)` with no positive `atol` is refused,
because no double-precision quadrature can report having met it. The same `rtol` with a positive
`atol` is accepted: `atol` is then what convergence rests on.

## Budget and outcome

`max_neval` bounds the integrand evaluations; it defaults to 100000. **It is a ceiling, not a
suggestion**: the count never exceeds it on a finite range, and an infinite range may overshoot it
by at most one rule application, because a panel costs its 21 points even when five of them would
have been enough. Reaching it is a status, not an error. With `breakpoints=` the budget covers the
whole call and is carried from one piece to the next, each piece reserving one rule application for
the pieces after it.

`max_panels` bounds the panels ONE walk of an infinite range may use, counting the first panel the
search accepted; it defaults to 100, and reaching it is `PF_INT_LIMIT` in the same way. It applies
only to an infinite range — passing it with two finite bounds aborts — and on `(-inf, +inf)`,
which is two walks, it caps each of them.

`converged=` is the short answer. `info=` is the long one, a `pf_integrate_info` carrying:

| Component | What it says |
|---|---|
| `status` | one of the `PF_INT_*` codes below |
| `converged` | `status == PF_INT_OK` |
| `extrapolated` | the result came from the epsilon table rather than the partition sum |
| `abserr` | the engine's own estimate of the absolute error |
| `partition_integral` | the plain sum over the final partition |
| `neval` | integrand evaluations, counted |
| `nsub` | subintervals in the final partition, summed over the walk's panels and the pieces |
| `npanels` | 1 per finite range or piece; the panels the walk used, both halves counted, on an infinite one |
| `nonfinite_at` | the point the integrand returned a NaN or an infinity at; meaningful only when `status` is `PF_INT_NONFINITE` |

`nonfinite_at` is zero on every other status, and zero is a point like any other — read it only
after checking `status`, never as a test for whether anything went wrong.

**Nothing is ever printed.** This module reads no verbosity setting and has no message stream; it
reports through these values, and speaks only by aborting when a caller contract is broken.

| Code | Means | What to do |
|---|---|---|
| `PF_INT_OK` | the requested accuracy was achieved | nothing |
| `PF_INT_LIMIT` | `max_neval` or `max_panels` ran out first | raise the cap, or loosen the tolerance |
| `PF_INT_ROUNDOFF` | round-off prevents the tolerance being met | loosen the tolerance; the result is as good as the arithmetic allows |
| `PF_INT_BAD_INTEGRAND` | the integrand behaves extremely badly somewhere | split the range at the offending point |
| `PF_INT_NO_CONVERGENCE` | the extrapolation table stopped making progress | loosen the tolerance, split the range at whatever the table could not accelerate, or pass `extrapolate=.false.` |
| `PF_INT_DIVERGENT` | the integral is probably divergent or very slowly convergent | check that the integral exists; the returned number is finite but is not an integral |
| `PF_INT_NONFINITE` | your integrand returned a NaN or an infinity | read `info%nonfinite_at` for the point, and fix the integrand or keep that point out of the range |

A non-`PF_INT_OK` status still returns the engine's best estimate, and it is often a good one: a
discontinuity inside the range reports `PF_INT_BAD_INTEGRAND` while returning an answer accurate
to many digits. Read `abserr` rather than assuming the worst.

`PF_INT_NONFINITE` is the exception to that: the partition it stopped on is a partition of
whatever your integrand answered before it stopped answering numbers, so the returned value and
`abserr` are both worth nothing. It is also the one outcome here that is about your code rather
than about the integral, which is why it is a status and not an abort — a sweep over a parameter
grid survives one bad parameter, and `nonfinite_at` says which point to look at.

## The evaluation record

Pass `points=` and you get back a `pf_integrate_points`: every abscissa, weight and value of the
**final partition**, with `n` saying how many are in use.

```fortran
type(pf_integrate_points) :: pts
type(pf_integrate_info) :: info
real(real64) :: r, again

r = pf_integrate(f, a, b, 1.0e-10_real64, info=info, points=pts)
again = sum(pts%w(1:pts%n)*pts%f(1:pts%n))     ! info%partition_integral, exactly
```

**The weighted sum reproduces the integral over the partition**, because the weights carry
everything the rule applied: the Kronrod weight, the subinterval's half-length and the Jacobian of
`log_base`. The record is a quadrature rule for your integrand in your own
variable, so you can re-weight it, plot it, or integrate a second function sampled at the same
points.

Two things to know. It is the final partition, not a log of every evaluation — a subinterval that
was bisected is represented by its two children, never by both itself and them, because summing
both would count that region twice. And what it reproduces is `info%partition_integral`, always
and exactly; it reproduces the RETURNED result only when `info%extrapolated` is false. On a call
the extrapolation accelerated, the returned result is the better of the two numbers and the
record is the partition it was accelerated from, so a caller re-weighting the record is working
with the plain sum — read `info%extrapolated`, or pass `extrapolate=.false.` to be handed the
number the record reproduces.

With `breakpoints=` the record covers every piece, concatenated in ascending order, and the
weighted sum reproduces the whole integral exactly as it does for an unbroken range.

`%append` joins two records, for two adjacent ranges integrated separately:

```fortran
r_left = pf_integrate(f, 0.0_real64, 1.0_real64, 1.0e-10_real64, points=left)
r_right = pf_integrate(f, 1.0_real64, 2.0_real64, 1.0e-10_real64, points=right)
call left%append(right)                        ! now a rule for [0, 2]
```

Recording costs three `real64` per evaluation and happens only when `points=` is present. A call
without it allocates no record at all.

## The extrapolation

QUADPACK's Wynn-epsilon extrapolation is **on by default**; `extrapolate=.false.` turns it off.

It exists for one shape: an integrable singularity at an endpoint, such as `log(x)/sqrt(x)` or
`1/sqrt(x)` on `[0, 1]`. Bisecting towards a singularity buys a fixed FACTOR of the remaining
error each time rather than a fixed number of digits, so the plain bisection pays more and more
for each further digit — while the table extrapolates that geometric sequence straight to its
limit and reaches full precision at the loosest tolerance you would ask for. The practical
difference is not a constant factor but a shape: **the cost of an endpoint singularity stops
growing with the tolerance**, and at a tight tolerance that is an order of magnitude or more. On
a smooth integrand, an interior peak or a polynomial it changes nothing worth noticing: the same
answer, and the same evaluation count but for the occasional extra bisection at the very tightest
tolerances.

It also earns two of the status codes. A divergent integral is reported as `PF_INT_DIVERGENT` in
a couple of hundred evaluations, where the bisection alone would keep drilling towards the
singularity until the integrand overflows and the answer becomes `PF_INT_NONFINITE` instead. A jump inside the
range — which the table cannot accelerate, because the sequence it is handed was never
converging — is reported as `PF_INT_NO_CONVERGENCE`.

Two things to weigh before leaving it on. `info%extrapolated` says whether the returned result
came from the table; when it did, the result is no longer the partition sum the record
reproduces. And on an integrand the table cannot help, the error estimate that comes back with a
non-OK status is an estimate of an acceleration that did not work: trust the status, not
`abserr`. Passing `extrapolate=.false.` gives you the plain bisection, whose result is always
exactly `info%partition_integral`.

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
you expected the integrand to do something tells you exactly what happened. And
**[`breakpoints=`](#cutting-the-range-breakpoints) is the cure**: a cut each side of the feature
puts a rule application on the piece that contains it, and the same call answers correctly.

```fortran
r = pf_integrate(bump, 1.0_real64, 10.0_real64, 1.0e-8_real64, &
                 breakpoints=[1.0_real64 + 1.0e-6_real64, 1.02_real64])
! r is the right answer, from the same range as the call that returned zero
```

Splitting the range into separate calls yourself does the same thing — the integral of the parts
is the integral of the whole, and `%append` joins the records — but it costs you the shared budget
and the summed `info`.

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
off far faster than the first guess assumed lives in a sliver just above the bound. Where you know
roughly where the feature is, `breakpoints=` settles it on an infinite range as it does on a
finite one.

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
| a breakpoint NaN or infinite | `breakpoints must be finite` |
| a breakpoint on or outside a bound | `breakpoints must lie strictly inside the range` |
| two equal breakpoints | `breakpoints must be distinct` |

Every abort is taken under a named `critical`, so one thread aborts rather than several, and the
exit status stays meaningful when the call was inside a parallel region.

**What your INTEGRAND does is not on this list.** An integrand that returns a NaN or an infinity
ends the integration, not the process: `PF_INT_NONFINITE`, `converged` false, and
`info%nonfinite_at` carrying the point. It is worth planning for — an interior singularity that
happens to land exactly on an abscissa reaches it, and `1/sqrt(abs(x - 0.5))` on `[0, 1]` puts the
centre point of the first rule application on one. Split the range at the singularity with
`breakpoints=`, which is also QUADPACK's own advice; an *endpoint* singularity needs nothing,
because the rule never evaluates an endpoint.

## Accuracy against cost

In general terms, and measured by `bench/benchmark_integrate.sh` rather than stated here: a smooth
integrand costs a few hundred evaluations to reach ten digits, and the cost grows slowly as the
tolerance tightens, because the underlying rule is exact for polynomials up to degree 31. An
endpoint singularity is the expensive case for the bisection alone, and the one the extrapolation
turns from a cost that grows with every further digit into one that does not grow at all: the
same few hundred evaluations at the loosest tolerance and at the tightest. Integrating a power
law in `log x` can be an order of magnitude cheaper than in linear `x`.

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

That is a claim about absence, so it is backed by running it rather than by reading the source.
`test/test_integrate_omp.f90` integrates thousands of different objects concurrently, each against
its own closed form, and integrates one parameterless function thousands of times concurrently
against a serial answer it must match **bit for bit**. The `threads` mode of
`bench/benchmark_integrate.sh` does the same across a thread ladder and exits nonzero if a single
result differs.

One thing is yours to get right: **an integrand object may not be shared between threads**. `eval`
takes the object `intent(inout)` precisely so it may keep a counter or a cache, and two threads
evaluating one object race over it. One object per thread.

See [Thread safety](../operating/thread-safety.html) for how this sits beside the rest of the
library.

## Where the engine comes from

The rule, the bisection driver, the error-ordered subinterval list and the epsilon table are
QUADPACK's (Piessens, de Doncker-Kapenga, Ueberhuber and Kahaner, 1983; public domain), as
modernised by Jacob Williams under BSD-3-Clause. They were reworked for this library: the
integrand became a `class(pf_integrand)` dummy so parameters travel with it, the evaluation counter
became real rather than a formula, the work arrays grow instead of being sized up front, every
diagnostic became a status code or an abort, and every evaluation is screened for a non-finite
value, which ends the integration rather than the process. The numerical logic — the abscissae, the weights, the round-off tests, the extrapolation
table — is transcribed unchanged, and the engine file's header lists every deviation.
