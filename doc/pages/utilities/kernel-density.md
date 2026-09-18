---
title: Kernel density estimation with parquet_kde
---

`parquet_kde` estimates the density a one-dimensional sample was drawn from, by placing a kernel
on every point and adding them up. `pf_kde` fits an array it keeps a sorted copy of, and then
answers the density, the distribution function and its quantiles exactly at any point, and the
density on a grid of points for plotting. It reaches no reader and no writer: a program can
`use parquet_kde` on its own, compiling the statistics tier it is built on and nothing of the
Arrow stack. `use parquet` brings it in too. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

## Quick example

```fortran
use parquet_kde
use iso_fortran_env, only : real64
type(pf_kde) :: k
real(real64) :: xg(200), fg(200)

call k%fit(mag)                    ! Silverman's rule, Gaussian kernel: the defaults
call k%curve(xg, fg)               ! the density on 200 points across the data
print '(a, f8.4)', 'bandwidth = ', k%bandwidth()
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call k%fit(x, [bandwidth], [rule], [adjust], [kernel], [lower], [upper], [boundary], &
           [is_valid], [weights], [weight_type], [skipnan], [n_null], [n_nan], [n_outside], &
           [ok], [threads])
call k%pdf(x, f)
call k%cdf(x, p)
call k%quantile(p, x)
call k%curve(x, f, [xmin], [xmax], [cut])
call k%print([unit])
```

`x` in `%fit` is a `real64` or `real32` array; a `real32` sample is widened before anything else
happens to it. `%pdf`, `%cdf` and `%quantile` each take a scalar or a rank-1 array, and every
result is `real64`. Every token (`rule`, `kernel`, `boundary`) is matched without regard to case.

## The estimate

With the retained points `x_j`, their weights `w_j` (all one when unweighted), a kernel `K` of
unit variance and a bandwidth `h`, the estimate is

```
f(x) = sum_j w_j K((x - x_j)/h) / (h * sum_j w_j)
```

It is a probability density: it is never negative and it integrates to one. `%cdf` is the same sum
over each kernel's own distribution function, so `%cdf` is exactly the integral of `%pdf`, and
`%quantile` inverts `%cdf`. Nothing is approximated on a grid: every query sums the kernels of the
points within reach of it, which the sorted copy finds by two binary searches, so a query costs time
in proportion to the number of points within one kernel's reach rather than to the sample.

## Kernels, and what `bandwidth` means

**`bandwidth` is the standard deviation of the kernel, whichever kernel is chosen.** One number
means the same amount of smoothing under every kernel, and the bandwidth rules below serve all four
without a per-kernel constant.

| `kernel=` | `K(u)` in its own units | variance of that form | support, in standard deviations |
|---|---|---|---|
| `"gaussian"` (default) | `exp(-u**2/2)/sqrt(2*pi)`, cut at `abs(u) = 5` and renormalised | 1 | `+-5` |
| `"epanechnikov"` | `3/4 (1 - u**2)` for `abs(u) < 1` | 1/5 | `+-sqrt(5)` |
| `"bspline"` | the cubic B-spline: `2/3 - u**2 + abs(u)**3/2` for `abs(u) < 1`, `(2 - abs(u))**3/6` for `1 <= abs(u) < 2` | 1/3 | `+-2 sqrt(3)` |
| `"box"` | `1/2` for `abs(u) <= 1` | 1/3 | `+-sqrt(3)` |

Each is rescaled to unit variance before the bandwidth is applied. What each is for:

- **Gaussian**: the smooth default. It is cut at five standard deviations and renormalised, which
  removes a mass of `5.7e-7` from its tails and gives it compact support like the others; its
  variance is then `1 - 1.5e-5` rather than exactly one.
- **Epanechnikov**: the kernel that minimises the mean integrated squared error; smooth inside its
  support, with a kink at its edge.
- **Cubic B-spline**: twice continuously differentiable, compact, and close in shape to the
  Gaussian.
- **Box**: the moving count; the estimate is a step function.

**Two other conventions for the word "bandwidth" are in common use, and converting from either is
one line.** A bandwidth given as a *multiplier on the sample's own standard deviation* `s` is
`bandwidth = f*s`. A bandwidth given as *the kernel's own scale parameter* -- the half-width of a
compact kernel's support -- converts as `bandwidth = c/sqrt(5)` for an Epanechnikov half-width
`c`, `bandwidth = c/sqrt(3)` for a box half-width `c`, and `bandwidth = a*sqrt(3)` for a cubic
B-spline of scale `a` (a B-spline matched to a Gaussian by its peak height instead has
`bandwidth = a*0.5984`). For the Gaussian the scale parameter is the standard deviation, so it needs
no conversion under either.

## Choosing a bandwidth

Without `bandwidth=`, the bandwidth comes from a rule over the fitted population, with the robust
scale `A = min(s, IQR/1.349)`: `s` is `pf_stddev` and `IQR` is `pf_iqr`, each over the same
population, with the same weights and `weight_type`.

| `rule=` | `h` |
|---|---|
| `"silverman"` (default) | `0.9 * A * n_eff**(-1/5)` -- Silverman's rule of thumb |
| `"scott"` | `1.06 * A * n_eff**(-1/5)` -- Scott's normal-reference rule |

- **`adjust=`** multiplies the bandwidth however it was chosen, a rule's or an explicit number:
  `adjust=0.5` halves the smoothing.
- **`bandwidth=` and `rule=` cannot both be given**; to scale a rule, use `adjust=`.
- **`n_eff` is what the weights make it.** Unweighted, it is the population's size. Under
  `weight_type="reliability"` (the default) it is `sum(w)**2 / sum(w**2)`; under
  `weight_type="frequency"` it is `sum(w)`, so a frequency weight of three counts as three
  observations. Under frequency weights `pf_iqr` takes its quartiles by the inverted CDF, the
  family's own rule for them, so the bandwidth equals the replicated sample's exactly wherever
  `s` is the smaller scale, and may differ slightly where the quartiles decide it.
- **A sample more than half of whose values tie** has an interquartile range of zero, and the rule
  then uses `s` alone rather than a bandwidth of zero.
- **One point, or a constant sample, has no scale**, so a rule cannot give it a bandwidth: the fit
  is undefined (`ok = .false.`, every answer NaN). The same sample with an explicit `bandwidth=` is a
  valid estimate, one bump per point.
- **The rules are derived for a Gaussian kernel and a unimodal density.** Under the
  standard-deviation convention they serve the other three kernels within about one per cent of
  each kernel's own optimal scaling. A multimodal sample is oversmoothed by both.

Other software computes different numbers under the same two rule names. When a particular value
is needed, pass it as `bandwidth=`.

`%bandwidth()` answers the bandwidth in use, after `adjust`, and `%rule(name)` how it was chosen:
`"silverman"`, `"scott"`, or `"explicit"` when it was given as a number.

## Bounded support: `lower=`, `upper=` and `boundary=`

A variable that cannot be negative, or cannot exceed a physical limit, has a density that is zero
beyond that limit, and a kernel placed near it would put mass where none can be. `lower=` and
`upper=` declare the support:

- **A point outside `[lower, upper]` is excluded** from the population and counted in
  `n_outside`, as a histogram counts a value outside its edges.
- **The density is zero outside the support**, `%cdf` is exactly 0 at and below `lower` and exactly
  1 at and above `upper`.

A kernel crossing a bound is corrected, in one of two ways chosen by `boundary=`:

- **`"renormalise"`** (the default): the kernel is cut at the bounds and divided by the mass it
  keeps inside them, so every point contributes exactly one unit inside the support. It never
  invents mass, and it does not force the estimate flat at the bound.
- **`"reflect"`**: the point is mirrored about each bound within its reach, and the mirror image's
  mass is folded back inside. It is exact for a density that is flat at the bound -- the density of
  `abs(v)` for a `v` symmetric about zero, say -- and it forces the estimate flat there.

Both corrections are biased near a bound by an amount proportional to `h` unless the true density
is flat there, and each is the right choice for a different shape. Two hundred points drawn from the
rising density `f(x) = 2x` on `[0, 1]`, fitted with Silverman's rule (`h = 0.0737`) and `lower=0`:

```
   x     true    unbounded    "renormalise"   "reflect"
 0.00    0.000     0.058          0.068          0.117
 0.05    0.100     0.122          0.137          0.143
 0.10    0.200     0.207          0.221          0.212
 0.20    0.400     0.400          0.405          0.400
```

At the bound itself reflection forces the estimate flat and so roughly doubles it; the unbounded
estimate is closest there only because it has lost mass below zero, which it never gets back.

**Where the kernel is wider than the whole support** -- both bounds given and closer together than
a kernel's reach -- a mirror image reaches the far bound too. Each point's kernel is then
normalised to one unit inside the support under both corrections, so the estimate still integrates
to one.

`%bounds(lo, hi)` answers the support, with `-Infinity`/`+Infinity` where no bound was given.

## Reading the estimate

- **`%pdf(x, f)`**: the density at a point, or at each point of an array.
- **`%cdf(x, p)`**: `P(X <= x)` at a point or at each point of an array, clamped to `[0, 1]`.
- **`%quantile(p, x)`**: the smallest `x` at which `%cdf` reaches `p`, found by Newton's method on
  `%cdf` inside a bracket, with bisection whenever a step would leave it. `p = 0` and `p = 1` give
  the two ends of the estimate's support: the extreme points minus and plus one kernel's reach,
  clipped to the bounds. A `p` outside `[0, 1]` aborts.
- **`%curve(x, f, [xmin], [xmax], [cut])`**: fills `x` with `size(x)` equally spaced points, both
  ends exact, and `f` with the density at each. The default range is the sample's minimum minus
  `cut` bandwidths to its maximum plus `cut` bandwidths, `cut` defaulting to 3, clipped to the
  support; `xmin=` and `xmax=` replace either end.

The counts come back as `int64` functions: `%n()` (the elements passed to `%fit`), `%n_valid()`
(the population), `%n_null()`, `%n_nan()` and `%n_outside()`. `%sum_weights()` is `sum(w)` over the
population, `%kernel(name)` the kernel's token, and `%is_fitted()` says whether `%fit` has run since
the last `%clear()`. `%print([unit])` writes all of it as one block.

## The estimate as a function

To integrate the estimate, or pass it anywhere a function is expected, extend `pf_integrand` from
`parquet_integrate` with a component holding the fitted object:

```fortran
type, extends(pf_integrand) :: density_fn
    type(pf_kde) :: k
contains
    procedure :: eval => density_eval
end type density_fn

function density_eval(this, x) result(f)
    class(density_fn), intent(inout) :: this
    real(real64), intent(in) :: x
    real(real64) :: f
    call this%k%pdf(x, f)
end function density_eval
```

Then `pf_integrate(fn, a, b, 1.0e-10_real64)` integrates the density over `[a, b]`. `%cdf(b) -
%cdf(a)` is the same number without a quadrature.

## What "in the population" means

`%fit` applies the `pf_*` statistics family's rules unchanged (see
[Array statistics](statistics.html)): a null (`is_valid(i)` false) leaves the population, then a
NaN (unless `skipnan=.false.`), then a zero weight; a negative, NaN or infinite weight aborts, as
does an `is_valid` or `weights` whose size differs from `x`. `n_null` and `n_nan` report the first
two, and the messages are the family's own, naming `pf_kde%fit`. The one rule the estimator adds is
the support: after the family's exclusions, a point outside `[lower, upper]` leaves the population
and is counted in `n_outside`. **An infinite point is outside every support**, bounded or not: a
kernel centred at infinity has nowhere to put its mass. The bandwidth rules read exactly the
population that remains.

## What returns quietly

A data condition never aborts:

- **An empty population** (every element excluded, or an empty `x`) leaves the object fitted with
  `ok = .false.`. Every query answers a quiet NaN, `%curve` fills `f` with NaN and `x` with NaN
  unless both ends were given, and `%quantile` answers NaN.
- **A population a rule cannot scale** (one point, or a constant sample) is the same: `ok =
  .false.` and NaN everywhere, `%bandwidth()` included.
- **`skipnan=.false.` with a NaN present** keeps the NaN, which makes the whole estimate NaN, with
  `ok = .false.`
- **A query at a NaN point** answers NaN; at an infinite point, the density is zero and `%cdf` is 0
  or 1.

## What aborts

Every abort is a caller contract that was broken, and names the binding it came from.

| Condition | Message |
|---|---|
| `bandwidth` NaN, infinite or `<= 0` | `pf_kde%fit: bandwidth must be a finite, positive number` |
| `bandwidth=` and `rule=` both given | `pf_kde%fit: bandwidth= and rule= cannot both be given; use adjust= to scale a rule` |
| an unknown `rule` | `pf_kde%fit: rule must be "silverman" or "scott"` |
| `adjust` NaN, infinite or `<= 0` | `pf_kde%fit: adjust must be a finite, positive number` |
| an unknown `kernel` | `pf_kde%fit: kernel must be "gaussian", "epanechnikov", "bspline" or "box"` |
| `lower` or `upper` NaN or infinite | `pf_kde%fit: lower and upper must be finite` |
| `lower >= upper` | `pf_kde%fit: lower must be below upper` |
| `boundary=` with no bound | `pf_kde%fit: boundary= needs lower= or upper=` |
| an unknown `boundary` | `pf_kde%fit: boundary must be "renormalise" or "reflect"` |
| `is_valid` or `weights` of the wrong size | `pf_kde%fit: weights has 3 elements but values has 4` (the family's text) |
| a negative, NaN or infinite weight | `pf_kde%fit: weight 2 is negative; weights must be finite and non-negative` (the family's text) |
| an unknown `weight_type` | `pf_kde%fit: weight_type "..." is not recognised; ...` (the family's text) |
| `threads <= 0` | `pf_kde%fit: threads must be positive` |
| a query or accessor before `%fit`, or after `%clear` | `pf_kde%pdf: the estimate has not been fitted` |
| an output of the wrong size | `pf_kde%pdf: f must have one element per point of x`, `pf_kde%cdf: p must have ...`, `pf_kde%quantile: x must have one element per element of p` |
| `p` outside `[0, 1]`, or NaN | `pf_kde%quantile: p must lie in [0, 1]` |
| `%curve` with `x` and `f` of different sizes | `pf_kde%curve: x and f must have the same size` |
| `xmin` or `xmax` infinite or NaN | `pf_kde%curve: xmin and xmax must be finite` |
| `xmin >= xmax` | `pf_kde%curve: xmin must be below xmax` |
| `cut` negative or NaN | `pf_kde%curve: cut must not be negative` |

## Thread safety

**A fitted `pf_kde` is read-only under every query**, so any number of threads may query one object
at once. `%fit` and `%clear` are writes: give each thread its own object, or fit once before the
region and share the result. `threads=` on `%fit` sets the team for the sort and the rules'
statistics; the answer does not depend on it, and inside your own parallel region it stands down
to one thread unless given explicitly. An abort inside a parallel region is taken by one thread.

## What it costs to import

`use parquet_kde` compiles the statistics tier it is built on (`parquet_stats`, and beneath it the
sorting tier) plus the module's own three files; no reader, writer or C++ boundary. It re-exports
the verbosity and message-stream pair, which `%print` reads, so a program importing it alone can
silence its output with `parquet_set_verbosity("silent")`.
