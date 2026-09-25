---
title: Kernel density estimation with parquet_kde
---

`parquet_kde` estimates the density a one-dimensional sample was drawn from, by placing a kernel
on every point and adding them up. `pf_kde` fits an array it keeps a sorted copy of, and then
answers the density, the distribution function and its quantiles exactly at any point, and the
density on a grid of points for plotting. `pf_kde_grid` accumulates the same estimate on a fixed
grid of cells from points streamed through it in any number of pieces, which it does not keep.
Both draw samples from the estimate, and both have an adaptive kernel, narrow where the data are
dense and wide where they are sparse. The
module reaches no reader and no writer: a program can
`use parquet_kde` on its own, compiling the statistics tier it is built on and nothing of the
Arrow stack. `use parquet` brings it in too. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

## Quick example

```fortran
use parquet_kde
use iso_fortran_env, only : real64
type(pf_kde) :: k
real(real64) :: xg(200), fg(200)

call k%fit(mag)                    ! the ISJ rule, cubic B-spline kernel: the defaults
call k%curve(xg, fg)               ! the density on 200 points across the data
print '(a, f8.4)', 'bandwidth = ', k%bandwidth()
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call k%fit(x, [bandwidth], [rule], [adjust], [kernel], [adaptive], [pilot], [alpha], &
           [bandwidth_max], [spread_max], [lower], [upper], [boundary], [is_valid], [weights], &
           [weight_type], [skipnan], [n_null], [n_nan], [n_outside], [ok], [threads], &
           [method])
call k%pdf(x, f, [threads])
call k%cdf(x, p, [threads])
call k%quantile(p, x, [threads])
call k%curve(x, f, [xmin], [xmax], [cut], [threads], [method])
call k%sample(v, seed, [stream], [threads])
call k%bandwidths(h, [x])
call k%bandwidth_at(x, h)
call k%pilot(g)
call k%print([unit])

call pf_kde_bandwidth(x, h, [rule], [adjust], [adaptive], [alpha], [lower], [upper], &
                      [is_valid], [weights], [weight_type], [skipnan], [n_null], [n_nan], &
                      [n_outside], [rule_used], [ok], [threads])
```

`x` in `%fit` is a `real64` or `real32` array, or a `parquet_column` of kind `int32`, `int64`,
`float32` or `float64`; anything but a `real64` array is widened before anything else happens to
it. A column's own validity is its null mask, so `is_valid=` cannot be given beside one, and a
column of any other kind aborts, naming it. `%pdf`, `%cdf`, `%quantile` and `%bandwidth_at` each take a scalar or a rank-1
array, and every result is `real64`; `threads=` belongs to the array forms, `%curve` and
`%sample` (see [Thread safety](#thread-safety)). Every token (`rule`, `kernel`, `boundary`) is
matched without regard to case.

## The estimate

With the retained points `x_j`, their weights `w_j` (all one when unweighted), a kernel `K` of
unit variance and a bandwidth `h`, the estimate is

```
f(x) = sum_j w_j K((x - x_j)/h) / (h * sum_j w_j)
```

It is a probability density: it is never negative and it integrates to one. `%cdf` is the same sum
over each kernel's own distribution function, so `%cdf` is exactly the integral of `%pdf`, and
`%quantile` inverts `%cdf`. `pf_kde` approximates nothing: every query sums the kernels of the
points within reach of it, which the sorted copy finds by two binary searches, so a query costs time
in proportion to the number of points within one kernel's reach rather than to the sample.

## Two forms, and when to use each

- **`pf_kde` keeps the population**, as a sorted copy, and answers the estimate exactly anywhere.
  Use it when the sample fits in memory, which is the usual case.
- **`pf_kde_grid` keeps only its cells**, however many points it has seen: a sample read one row
  group at a time, a stream, or pieces accumulated on separate threads and merged at the end. Its
  density is the estimate at each cell centre, interpolated between them, and converges to the
  exact estimate as the square of the cell width. The bandwidth is chosen before the first point
  arrives, and the adaptive kernel takes a second pass. There are two ways to fill it -- depositing
  every kernel, or binning and one transform -- and
  [Two ways to fill a grid](#two-ways-to-fill-a-grid) says which to reach for. See
  [Streaming into a grid](#streaming-into-a-grid-pf_kde_grid).

### `method="binned"`: a fit that answers from a grid

`%fit(..., method="binned")` takes the second form's speed without its bookkeeping. The
sample, the population rules, the bandwidth rule and the adaptive kernel are all the first
form's; what changes is that `%fit` ends by laying one grid over the whole extent its kernels
reach and every query afterwards reads that grid. `"exact"` is the default and is what every
call that names no method gets.

It is an opt-in because it changes what the object MEANS, not only what it costs: the binned
estimate is the estimate of a sample whose points have been moved to the cell centres, and it
converges to the exact one as the square of the cell width. `%fit` gives its grid sixteen
cells to the narrowest kernel, which is where that difference stops being visible, and says so
when the extent is too wide for the count it is held to.

**It is worth reaching for where the exact estimator's cost is in the boundary, not in the
points.** Under `boundary="linear"` with an adaptive kernel, a fit scans each corrected zone
for the stretches the clip removes; on a density that approaches zero the widened kernels make
that zone the whole support, and the scan is the dominant cost of the fit. A binned fit has no
scan at all -- the correction is carried by the cells -- so its cost is set by the cells rather
than by the sample, and it is flat in the number of points.

**It is not the cheaper route everywhere, and the two costs cross.** The exact route's cost grows
with the points and the binned route's does not, so which is cheaper depends on how many points
there are and on how dear the correction is. Where the correction is already cheap -- unbounded,
or `boundary="reflect"`, which is one division per point -- the exact route is still the cheaper
of the two at a hundred thousand points and the crossing lies somewhere beyond it, so reaching
for `method="binned"` there makes the fit slower rather than faster. Where the correction is the
scan described above, the crossing lies far below that and the binned fit wins by orders of
magnitude. `bench/benchmark_kde.sh` measures both, under each boundary.

What is unchanged: `%bandwidths`, `%bandwidth_at`, `%pilot` and `%bandwidth` read the rule and
answer the same under either method, and the counts and the support are the same. What is
different: `%pdf`, `%cdf`, `%quantile`, `%curve` and `%sample` are answered from the grid, and
`%curve` refuses a `method=` of its own. `%method(name)` says which is in force.

## Kernels, and what `bandwidth` means

**`bandwidth` is the standard deviation of the kernel, whichever kernel is chosen.** One number
means the same amount of smoothing under every kernel, and the bandwidth rules below serve all four
without a per-kernel constant.

| `kernel=` | `K(u)` in its own units | variance of that form | support, in standard deviations |
|---|---|---|---|
| `"gaussian"` | `exp(-u**2/2)/sqrt(2*pi)`, cut at `abs(u) = 5` and renormalised | 1 | `+-5` |
| `"epanechnikov"` | `3/4 (1 - u**2)` for `abs(u) < 1` | 1/5 | `+-sqrt(5)` |
| `"bspline"` (default) | the cubic B-spline: `2/3 - u**2 + abs(u)**3/2` for `abs(u) < 1`, `(2 - abs(u))**3/6` for `1 <= abs(u) < 2` | 1/3 | `+-2 sqrt(3)` |
| `"box"` | `1/2` for `abs(u) <= 1` | 1/3 | `+-sqrt(3)` |

Each is rescaled to unit variance before the bandwidth is applied. What each is for:

- **Gaussian**: the smooth classic. It is cut at five standard deviations and renormalised, which
  removes a mass of `5.7e-7` from its tails and gives it compact support like the others; its
  variance is then `1 - 1.5e-5` rather than exactly one.
- **Epanechnikov**: the kernel that minimises the mean integrated squared error; smooth inside its
  support, with a kink at its edge.
- **Cubic B-spline**: the default. Twice continuously differentiable, compact, and close in shape
  to the Gaussian -- so it smooths much as the Gaussian does, while its support ends at
  `2 sqrt(3)` standard deviations rather than five. The shorter reach is what makes a bounded,
  adaptive fit cheaper: every corrected zone is `KDE_RADIUS*h_max` wide.
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

Without `bandwidth=`, the bandwidth comes from a rule over the fitted population. The default is the
Improved Sheather-Jones rule, `rule="isj"`; Silverman's and Scott's rules of thumb, and
least-squares cross-validation (`rule="lscv"`), are there by name.

### The Improved Sheather-Jones rule

The rule chooses the bandwidth that minimises the estimate's asymptotic mean integrated squared
error, estimating the density's own derivatives from the sample rather than assuming it is normal.
It bins the population onto 16384 cells spanning its range widened by a tenth on each side, clipped
to the support when one is given, takes the discrete cosine transform of the binned sample, and
solves a fixed-point equation for the bandwidth: each derivative's norm is estimated at the
smoothing the norm of the one above makes optimal, from the seventh derivative down to the second.
The answer is the smallest solution no narrower than one cell.

- **It follows a multimodal sample.** Two narrow clusters far apart have a large standard
  deviation, which a rule of thumb reads as the sample's scale, smoothing both clusters into broad
  bumps; the ISJ rule reads the clusters' own width, and its bandwidth can be a tenth of Silverman's.
- **On a normal sample it agrees with the rules of thumb** to a few per cent: it seeks the same
  optimum without assuming the shape.
- **Where it finds no bandwidth, Silverman's rule gives one**, and `%rule(name)` answers
  `"silverman"`. That happens for a handful of points, and for values rounded to a step several
  cells wide, whose repeats the equation reads as structure finer than a cell. Named as
  `rule="isj"`, the rule is not replaced: the fit is undefined (`ok = .false.`, every answer NaN).
- **A bandwidth finer than the sample's own resolution is refused.** Values rounded to a step --
  measurements to two decimals, counts, anything on a lattice -- put a solution of the fixed point
  far below that step, because the repeats look like structure. A kernel narrower than the closest
  pair of distinct observations cannot be resolving anything the sample expresses, so the rule
  declines it and the fallback above applies. Rounded data therefore gets a sensible bandwidth
  rather than one spike per distinct value, and nothing is needed from the caller.
- **The rule has no known blind spot of shape.** Its error falls with the sample on every density
  measured against it, combs of narrow evenly spaced components included, and it is the most
  accurate of the selectors this library has been compared with. What defeats it is the RESOLUTION
  of the data rather than the shape of the density, which is what the bullet above answers.
  `bench/benchmark_kde.sh`'s `mise` mode scores every rule against the exact mean integrated
  squared error of the Marron-Wand test densities, and is what would show a rule getting worse.
- **Its cost is one pass binning the sample and a fixed amount besides**: a transform of the 16384
  cells and a few tens of evaluations of the equation. On a large sample that is less than the rules
  of thumb spend on their own passes over it; on a small one it is many times more, so a loop
  fitting many small samples should name a rule of thumb. `bench/benchmark_kde.sh`'s `rules` mode
  measures each rule.
- **The bandwidth is the Gaussian kernel's optimum**, and under the standard-deviation convention it
  serves the other three kernels as the rules of thumb do.

Other implementations of the rule bin onto other grids and count the sample differently, and their
bandwidths differ accordingly. When a particular value is needed, pass it as `bandwidth=`.

### Silverman's and Scott's rules, and what every rule shares

The two rules of thumb scale a normal density's optimum by the robust scale `A = min(s,
IQR/1.349)`: `s` is `pf_stddev` and `IQR` is `pf_iqr`, each over the same population, with the
same weights and `weight_type`.

| `rule=` | `h` |
|---|---|
| `"silverman"` | `0.9 * A * n_eff**(-1/5)` -- Silverman's rule of thumb |
| `"scott"` | `(4/3)**(1/5) * A * n_eff**(-1/5)` -- Scott's normal-reference rule |

- **`adjust=`** multiplies the bandwidth however it was chosen, a rule's or an explicit number:
  `adjust=0.5` halves the smoothing.
- **`bandwidth=` and `rule=` cannot both be given**; to scale a rule, use `adjust=`.
- **`n_eff` is what the weights make it**, under all three rules. Unweighted, it is the
  population's size. Under `weight_type="reliability"` (the default) it is `sum(w)**2 / sum(w**2)`;
  under `weight_type="frequency"` it is `sum(w)`, so a frequency weight of three counts as three
  observations, and the ISJ rule gives the replicated sample's bandwidth. Under frequency weights
  `pf_iqr` takes its quartiles by the inverted CDF, the family's own rule for them, so a rule of
  thumb's bandwidth equals the replicated sample's exactly wherever `s` is the smaller scale, and
  may differ slightly where the quartiles decide it.
- **A sample more than half of whose values tie** has an interquartile range of zero, and a rule
  of thumb then uses `s` alone rather than a bandwidth of zero.
- **One point, or a constant sample, has no scale**, so no rule can give it a bandwidth: the fit
  is undefined (`ok = .false.`, every answer NaN). The same sample with an explicit `bandwidth=` is a
  valid estimate, one bump per point.
- **The rules of thumb are derived for a Gaussian kernel and a unimodal density.** Under the
  standard-deviation convention they serve the other three kernels within about one per cent of
  each kernel's own optimal scaling. A multimodal sample is oversmoothed by both.
- **The rules choose `h` and the boundary correction does not.** With `lower=` or `upper=`, a rule
  reads the population inside the support as if it were unbounded, and the correction is applied
  to the kernels afterwards.

Other software may compute a different number under the same name. statsmodels' `bw_silverman`
agrees with `rule="silverman"` exactly, to the last bit; its `bw_scott` uses the rounded `1.059`
where this library uses the exact `(4/3)**(1/5)`. scipy's `gaussian_kde` differs under both names,
because its `bw_method="silverman"` and `"scott"` scale the sample's standard deviation rather than
the robust scale `A`. R's `bw.nrd0` -- what `density()` uses unless told otherwise -- is Silverman's
rule with the interquartile range divided by the rounded `1.34` rather than by `1.349`, so it
differs from `rule="silverman"` by a fraction of a per cent whenever the interquartile range is the
smaller scale and not at all when the standard deviation is; R's `bw.nrd` is Scott's rule with the
same rounding. When a particular value is needed, pass it as `bandwidth=`.

`%bandwidth()` answers the bandwidth in use, after `adjust`, and `%rule(name)` the rule that chose
it: `"isj"`, `"lscv"`, `"silverman"` or `"scott"`, `"explicit"` when it was given as a number or
taken from a `pilot=`, or `"none"` on an undefined estimate, where no rule produced one.

### The bandwidth on its own: `pf_kde_bandwidth`

A caller who wants the number but not the estimate can ask for it directly:

```fortran
real(real64) :: h
character(len=:), allocatable :: used

call pf_kde_bandwidth(mag, h, rule="isj", rule_used=used)
```

It takes the same sample forms `%fit` takes, and `rule`, `adjust`, `adaptive`, `alpha`, `lower`,
`upper` and the population arguments mean exactly what they mean there. The answer is the number
`%fit` would have resolved from the same arguments -- the two share one body, so they cannot
drift -- and what it skips is everything after the bandwidth: the pilot an adaptive fit would
build, each point's own bandwidth and mass, and the boundary correction's zones.

**What that saves depends entirely on what came after.** Every rule here reads an ordered sample,
so the sort is paid either way; for a plain unbounded fit there is almost nothing else after the
bandwidth, and this form costs what `%fit` costs. Under the ADAPTIVE kernel it skips the pilot pass
and the per-point look-up, which is most of the fit -- the greater part of the work, on a large
sample. Reach for it to get a number cheaply out of an adaptive configuration, or to get one at
all without an estimate; not as a faster way to fit. `bench/benchmark_kde.sh`'s `rules` mode
measures both cases.

- **Give `adaptive=.true.` when the number is for an adaptive estimate**, since that estimator's
  global bandwidth is the wider one.
- **`kernel=` is deliberately absent.** A bandwidth is the kernel's standard deviation whatever the
  kernel, so one number serves all four; accepting a kernel would imply a dependence that is not
  there.
- **`rule_used`** names the rule that produced the number, which matters because the default falls
  back to Silverman's rule where the ISJ rule finds none. `ok` is `.false.` where no rule found
  one, and `h` is then a quiet NaN.

It is what lets `pf_kde_grid` be used without building a `pf_kde` first: a grid needs its bandwidth
before the first point arrives, and this is how to get one from a sample already in hand.

### Cross-validation: `rule="lscv"`

`rule="lscv"` chooses the bandwidth that minimises the least-squares cross-validation criterion

```
LSCV(h) = integral of f_h**2  -  (2/n) * sum_i f_{h,-i}(x_i)
```

where `f_{h,-i}` is the estimate built without point `i`. The criterion estimates the integrated
squared error up to a constant, so minimising it aims at the same target the other rules aim at,
without assuming anything about the density.

- **It is the only rule here that scores the estimator actually in force.** Asked for with
  `adaptive=.true.` it minimises the criterion for the ADAPTIVE estimate, and so returns the
  adaptive estimator's own bandwidth, which no plug-in rule can do: a plug-in rule answers the
  question the fixed estimator asks. This is what to reach for when the adaptive kernel's
  bandwidth has to be measured on the data rather than derived from the fixed one's.
- **It is the expensive rule**, and how expensive depends on which estimator it is scoring. For a
  FIXED bandwidth both of its terms are self-convolutions of the weighted sample, so the sample is
  binned once and each candidate costs one filtered transform rather than a pass over every pair.
  For the ADAPTIVE kernel the widths depend on the pair, which no single convolution gives, and the
  criterion is summed over the pairs within five bandwidths of each other. Either way it is
  evaluated, above a few thousand points, over a subsample drawn at a fixed seed, which bounds the
  cost whatever the sample's size; the answer is then reproducible but is not a function of every
  point.
- **It is a high-variance criterion.** That is its known weakness, and the price of assuming
  nothing: on one sample its bandwidth can sit some tens of per cent from the one that minimises
  the true error, in either direction. The ISJ rule is the better default and remains it. The
  minimiser is located to a per cent of its own position and no further, because a criterion that
  scatters by tens of per cent does not resolve more than that; `bench/benchmark_kde.sh MODE=mise`
  scores the rule against the exact error, and scores the binned criterion beside the summed one.
- **Where the criterion has no minimum to find**, the fit is undefined (`ok = .false.`, every
  answer NaN), as it is for any rule that finds no scale.

## Bounded support: `lower=`, `upper=` and `boundary=`

A variable that cannot be negative, or cannot exceed a physical limit, has a density that is zero
beyond that limit, and a kernel placed near it would put mass where none can be. `lower=` and
`upper=` declare the support:

- **A point outside `[lower, upper]` is excluded** from the population and counted in
  `n_outside`, as a histogram counts a value outside its edges.
- **The density is zero outside the support**, `%cdf` is exactly 0 at and below `lower` and exactly
  1 at and above `upper`.

A kernel crossing a bound is corrected, in one of three ways chosen by `boundary=`:

- **`"reflect"`** (the default): the point is mirrored about each bound within its reach, and the
  mirror image's mass is folded back inside. It is exact for a density that is flat at the bound --
  the density of `abs(v)` for a `v` symmetric about zero, say -- and it forces the estimate flat
  there. Its `%cdf` has a closed form, so it costs no more than the unbounded estimate.
- **`"renormalise"`**: at each query point the summed kernels are divided by the mass a kernel
  *centred there* keeps inside the support, and the estimate is then divided by its own integral so
  that it is still a density. This is the cut-and-normalised kernel of the literature, the
  local-CONSTANT member of the same family as `"linear"` below. At the bound it answers what
  `"reflect"` answers -- both double the one-sided sum there -- and it differs from it inside.
  Because the divisor varies with the query point, `%cdf` and `%quantile` within one kernel's reach
  of a bound are quadratures rather than closed forms, and cost what `"linear"`'s cost.
- **`"linear"`**: within one kernel's reach of a bound each point's kernel is replaced by the
  linear boundary kernel, the kernel of a local linear fit there: `(a_2 - a_1 u) K(u)` over the
  moments `a_l` the support leaves it, which is the unique kernel of that form whose mass inside
  the support is one and whose first moment is zero. Its bias at the bound is of the order of
  `h**2`, the interior's, rather than `h`. It can go negative where the true density falls to zero
  at a bound: those values are set to zero. **The division by the estimate's own integral is not a
  consequence of that clip** -- neither member of the family integrates to one by itself, and both
  are divided by their own integral whether or not anything was clipped -- so `%pdf` is a density
  and `%cdf` is exactly its integral in every case.

**`"renormalise"` and `"linear"` are the two members of one family**, the kernel of a local
polynomial fit of degree zero and of degree one at the query point. They share the arithmetic: the
same truncated moments, the same normalisation by the estimate's own integral, and the same
quadrature near a bound.

The two local-constant corrections -- `"reflect"` and `"renormalise"` -- are biased near a bound by
an amount proportional to `h` unless the true density is flat there, and each is the right choice
for a different shape. Two hundred points drawn from the rising density `f(x) = 2x` on `[0, 1]`,
fitted with Silverman's rule (`h = 0.0737`), the Gaussian kernel and `lower=0`:

```
   x     true    unbounded    "renormalise"   "reflect"    "linear"
 0.00    0.000     0.058          0.116          0.117       0.000
 0.05    0.100     0.122          0.163          0.143       0.100
 0.10    0.200     0.207          0.226          0.212       0.202
 0.20    0.400     0.400          0.401          0.400       0.402
```

At the bound itself both simple corrections force the estimate flat and so roughly double it, and
they agree there to a thousandth; the unbounded estimate is closest there only because it has lost
mass below zero, which it never gets back.

**Which to choose.** `"linear"` where the density rises or falls at the bound, which is what the
table above shows and what the other two cannot follow. `"reflect"`, the default, where it is flat
there, which reflection is exact for, and wherever the estimate's cost matters: it is the only
correction whose `%cdf`, `%quantile` and `%sample` cost no more near a bound than away from it.
`"renormalise"` where the cut-and-normalised kernel is wanted by name; it is unbiased at the bound
exactly as `"reflect"` is, and pays `"linear"`'s cost for `%cdf` and `%quantile` there.

**What a corrected boundary costs.** Inside one kernel's reach of a bound, `%cdf` and
`%quantile` integrate each nearby point's corrected kernel, and `%sample` draws there from those
kernels and keeps a draw by the estimate, so each costs tens of times what it costs elsewhere --
under `"linear"` and under `"renormalise"` alike, the two sharing that machinery. `%pdf` costs
about what it costs under `"reflect"`, which needs no integral at all and is the reason it is the
default. The integrals are a fixed Gauss-Legendre rule over pieces the kernel's own knots and the
correction's edges cut, so their cost is known before a query starts and no tolerance governs it.
`bench/benchmark_kde.sh` measures all of it (`MODE=boundary`). For many draws from a density whose mass sits near a bound, accumulate the
estimate into a `pf_kde_grid` and sample that instead, which costs one quadratic solve per draw
under every correction. Under the adaptive kernel a far tail's wide kernels widen the zone the
correction acts in; `spread_max` bounds that automatically and `bandwidth_max` is the explicit
remedy.

**What the clip can miss.** The stretches where the raw estimate is negative are found at `%fit` by
a scan about a sixty-fourth of the NARROWEST kernel apart -- of the narrowest, so that the spacing
resolves every kernel in the sample -- refined at the kernels' own edges where the estimate comes
near zero. A dip below zero that begins and ends between two neighbouring scan points is not seen:
`%pdf` still clips it, while `%cdf` counts its (small, negative) mass, so the two can disagree
there by at most that mass.

The scan reads those samples from a binned grid of the same estimate wherever that grid is a
comfortable margin away from zero, and from the exact sum everywhere else; every crossing it
reports is still bisected on the exact estimate. So the grid decides only WHERE the exact
estimator is asked, and the spacing above -- which is what the paragraph's guarantee rests on --
is unchanged. It is used for the `"gaussian"` and `"bspline"` kernels, whose binned estimate
converges as the square of the cell width, and not for the other two. Without it the scan sums
every retained point at every sample, so under `"epanechnikov"` and `"box"` an adaptive
`"linear"` fit of a density that approaches zero costs time growing as the SQUARE of the sample
size. `method="binned"` builds no scan at all and is what removes that cost under those two
kernels; so does narrowing the widest kernel with `spread_max=` or `bandwidth_max=`.

**Where the kernel is wider than the whole support** -- both bounds given and closer together than
a kernel's reach -- a mirror image reaches the far bound too. Each point's kernel is then
normalised to one unit inside the support under both corrections, so the estimate still integrates
to one.

`%bounds(lo, hi)` answers the support, with `-Infinity`/`+Infinity` where no bound was given.
`pf_kde_grid%init` takes the same three arguments, and its range must lie inside the support.
**Under `"linear"` a grid takes three more rules**, so that the weight it counts beyond its own
range is never weight the correction was acting on: its range must START at `lower` and END at
`upper` where those are given (R1); where only one bound is given, its range must be at least one
kernel's reach wide, so that the free edge lies outside the bound's zone (R2); and, with the
adaptive kernel and a free edge, a point whose own reach exceeds the range's width poisons the grid
rather than have that weight miscounted (R3), for which `bandwidth_max` is the remedy -- or a
wider range, since a grid's range need not lie inside its pilot's. With both bounds given no edge
is free and only R1 applies.

R3 leaves the grid undefined quietly, as every data condition here does, but it does not leave it
undiagnosable: `%add` counts the offending points and reports that call's share in `n_overreach=`,
`%n_overreach()` answers the total over every `%add`, and a WARNING names the count and both
remedies. The warning is a finding about the data, so `verbosity = "silent"` leaves it standing and
only `"errors_only"` takes it.

## The adaptive kernel

A fixed bandwidth is a compromise: wide enough to smooth the sparse tails, it blurs the dense
peaks, and narrow enough to resolve the peaks, it leaves the tails ragged. The adaptive kernel
gives each point its own bandwidth, narrow where the data are dense and wide where they are
sparse:

```fortran
call k%fit(mag, adaptive=.true.)                    ! each point's own bandwidth, alpha = 0.5
call k%fit(mag, adaptive=.true., alpha=0.3_real64, bandwidth_max=0.5_real64)
```

Each point `x_j` takes the bandwidth

```
h_j = h * (p(x_j)/g)**(-alpha)          at most spread_max * h_min, and at most bandwidth_max
```

where `h` is the global bandwidth -- a rule's or the number given, times `adjust`, and what
`%bandwidth()` answers -- `p` is a pilot estimate of the density, the fixed estimate at `h`, and
`g` is the pilot's geometric mean, so that a point at a typical density keeps about `h`. The
estimate is then

```
f(x) = sum_j w_j K((x - x_j)/h_j) / (h_j * sum_j w_j)
```

Every point's kernel still has unit mass, so the estimate is still a density and integrates to
one, and a boundary correction applies to each kernel at its own bandwidth.

- **`alpha`**, in `[0, 1]` (default 0.5, Abramson's choice), sets how far the bandwidths follow
  the pilot. `alpha = 0` is the fixed estimate exactly; `alpha = 1` makes each bandwidth inversely
  proportional to the pilot, which is the nearest-neighbour extreme.
- **An adaptive fit given a RULE widens that rule's bandwidth.** A bandwidth rule answers the
  question the fixed estimator asks, and the adaptive kernel's own best global bandwidth is larger:
  used unchanged, a rule's number oversharpens it. So with `adaptive=.true.` and no `bandwidth=`,
  the rule's answer is multiplied by a factor that is one at `alpha = 0` -- where the adaptive
  estimate IS the fixed one -- and grows with `alpha`. An explicit `bandwidth=` is never widened,
  and neither is a scale taken from a `pilot=`; `adjust=` multiplies whatever comes out.
  `%bandwidth()` answers the widened number, which is the one every point's own bandwidth is built
  from. `bench/benchmark_kde.sh`'s `mise` mode is what the factor is measured by, and
  `rule="lscv"` is how to measure it on one particular sample instead.
- **`bandwidth_max`** caps every point's bandwidth, in the data's own units. An outlier has a
  pilot density near zero and so a very wide kernel, and since every query sums the points within
  reach of the widest kernel, one far outlier slows every query. The cap is the remedy. A cap
  BELOW the resolved global bandwidth is accepted and then governs every point, which makes the
  estimate narrower everywhere rather than only where the pilot is thin: it is a cap, not a check
  on the rule. `%bandwidth()` still answers the resolved global bandwidth in that case, not the
  cap -- it reports what the rule chose, and `%bandwidths()` reports what each point actually got,
  which is the cap.

### `spread_max`, the cap that is always in force

`spread_max` caps the SPREAD rather than the bandwidth: no point's kernel is wider than
`spread_max` times `h_min`, the narrowest bandwidth the rule can give, which it gives at the
pilot's densest cell. It defaults to **100**, and unlike `alpha` and `bandwidth_max` it applies
whether it was named or not.

There is a default because the rule is unbounded. `h_j = h * (p/g)**(-alpha)` grows without limit
as the pilot density approaches zero, so any density that vanishes anywhere -- at a bound, or in a
gap inside the support -- gives some point a kernel of unbounded width. Every later query then sums
the points within reach of that kernel, and under `boundary="linear"` the corrected zone it opens
is scanned at the resolution of the NARROWEST kernel, which makes such a fit quadratic in the
sample's size rather than linear. The spread is what bounds that scan: the zone is one kernel reach
of `h_max` wide and the step is a fixed fraction of `h_min`, so the scan's step count is at most a
fixed multiple of `spread_max`, whatever the sample is. A cap on `h_max/h` would bound none of it,
the step being set by `h_min`.

The default is far too loose to change an ordinary estimate -- the library's own pilots stay inside
a spread of a few -- and is there for the pathological case alone.

- **Where both caps are given, the tighter one binds.** They answer different questions:
  `spread_max` is a statement about the estimator's SHAPE ("never wider than this many times the
  narrowest kernel") and `bandwidth_max` about the DATA's scale ("never wider than this"). Neither
  overrides the other, and a caller with a physical reason for an absolute bound need not convert
  it into a factor.
- **The fit says when the DEFAULT cap binds**, as advice naming how many points took it. It stays
  silent when the caller passed `spread_max` or `bandwidth_max` themselves: an explicit request
  needs no advice. Advice goes quiet at `verbosity = "silent"`.
- **The fit also says when a corrected boundary's zones cover the whole support**, which is the
  other way a wide kernel makes such a fit dear: the scan then runs over the entire domain rather
  than a boundary. That advice names the bandwidth the zones stay inside the support at, and the
  `spread_max` that says the same thing, so a caller who has capped already and is advised again
  can read how much further to go. A fit with a fixed bandwidth is told to narrow `bandwidth=`
  instead, both caps needing `adaptive=.true.`. It is said whether or not the caller capped,
  because a cap that is in force and still too wide is exactly the case worth reporting.
- `%print` names the spread cap on every adaptive fit, given or not, since it is always what
  produced the bandwidths.
- **The pilot** is a `pf_kde_grid` that `%fit` builds at the global bandwidth over the population,
  with the same kernel and support, from four bandwidths below the lowest point to four above the
  highest (clipped to the support), a quarter of a bandwidth to the cell, and never fewer than 64
  cells nor more than 65536. The estimate is insensitive to the pilot's fine detail: at that
  resolution it agrees with one computed from an exact pilot to a few parts in a thousand of its
  peak. A sample spanning more than about 16000 bandwidths -- a far outlier -- reaches the ceiling
  and gets coarser pilot cells; `lower=`/`upper=`, or clipping the sample, is the remedy.
- **`g` is the mean over the pilot's own density**, `log g = sum_i p_i log p_i / sum_i p_i` over
  its cells, rather than the mean over the sample points. The two differ by a constant factor
  near one on every bandwidth, which rescales `h`; this form needs no second pass over the points,
  which is what gives the streaming form the same estimate.
- **A point the pilot reads as zero** -- which only a pilot built from other data can do -- takes
  the cap, whichever of the two is tighter. A cap is always present, so there is no case in which
  the rule has to invent a density for such a point.

### Applying one sample's smoothing to another

A smoothing measured on one dataset can be applied to a second, so that two densities are smoothed
identically and can be compared. `%pilot` hands back the first fit's pilot, and `pilot=` on the
second fit reads it instead of building one:

```fortran
type(pf_kde) :: ka, kb
type(pf_kde_grid) :: shape

call ka%fit(a, adaptive=.true., lower=0.0_real64, boundary="linear")   ! measure on A
call ka%pilot(shape)                                                   ! the smoothing, as a grid

call kb%fit(b, adaptive=.true., pilot=shape, bandwidth_max=1.0_real64, &
            lower=0.0_real64, boundary="linear")                       ! apply it to B
```

The same pilot drives `pf_kde_grid%init(pilot=)`, so the second dataset can be streamed instead of
held. Three things to know, each of which is easy to meet and awkward to discover:

- **Give `bandwidth_max`.** A pilot measured on one sample can hand another sample's tail
  bandwidths orders of magnitude above the global one, wherever the two populations reach
  differently. For an exact `pf_kde` that is slow rather than wrong -- every query sums the points
  within reach of the widest kernel, and `%fit` says so when the spread is extreme -- but for a
  grid under `boundary="linear"` with a free edge it is R3, and the grid is left undefined.
- **The pilot and the fit must describe the same estimate**: the same kernel, the same `lower` and
  `upper`, the same `boundary`. A mismatch aborts, naming which one differs, because a bandwidth
  read from a pilot of another shape is a number from a different density.
- **Given alone, `pilot=` carries the SCALE as well as the shape.** The fit takes the pilot's own
  global bandwidth, so "the same smoothing" means the same smoothing and not merely the same shape
  at whatever size the second sample suggests; `%rule` then answers `"explicit"`. Add `rule=` or
  `bandwidth=` beside it to re-scale instead: the per-point ratios still come from the pilot, and
  the scale is measured on the new data.

The transfer is accurate when the two populations are alike, and degrades gracefully as they
diverge -- it becomes the smoothing the first dataset called for, applied to the second, which is
what was asked for.

Reading the adaptive fit:

- **`%bandwidths(h, [x])`**: every retained point's bandwidth, in the order of the points,
  ascending, with the points themselves in `x` on request. A fixed fit answers `%bandwidth()` for
  every point.
- **`%bandwidth_at(x, h)`**: the bandwidth the rule gives a point at `x`, read from the pilot as
  `%fit` read it for each retained point.
- **`%pilot(g)`**: a copy of the pilot. Given to `pf_kde_grid%init(pilot=g)` with the same
  bandwidth and `alpha`, it gives every point the bandwidth `%fit` gave it, so the grid converges
  to the same adaptive estimate; given to another `%fit(pilot=g)` it does the same for the exact
  form. See [Applying one sample's smoothing to another](#applying-one-samples-smoothing-to-another).
- **`%is_adaptive()`** says whether the fit was asked for the adaptive kernel.

The adaptive `%fit` costs one grid pass for the pilot and one look-up per point beyond the fixed
one; `bench/benchmark_kde.sh` measures both (`MODE=adaptive`). Given `pilot=`, it pays a copy
instead of the grid pass.

## Reading the estimate

- **`%pdf(x, f)`**: the density at a point, or at each point of an array.
- **`%cdf(x, p)`**: `P(X <= x)` at a point or at each point of an array, clamped to `[0, 1]`.
- **`%quantile(p, x)`**: the smallest `x` at which `%cdf` reaches `p`, found by Newton's method on
  `%cdf` inside a bracket, with bisection whenever a step would leave it. `p = 0` and `p = 1` give
  the two ends of the estimate's support, where its density starts and stops: the smallest
  `x_j - R h_j` and the largest `x_j + R h_j` over the retained points, `R h_j` being point `j`'s
  kernel's reach at its own bandwidth, clipped to the bounds. A `p` outside `[0, 1]` aborts.
- **`%curve(x, f, [xmin], [xmax], [cut], [threads], [method])`**: fills `x` with `size(x)`
  equally spaced points, both ends exact, and `f` with the density at each. The default range is
  the sample's minimum minus `cut` bandwidths to its maximum plus `cut` bandwidths, `cut`
  defaulting to 3, clipped to the support; `xmin=` and `xmax=` replace either end. `method=` takes
  the same two tokens `pf_kde_grid%init` takes and means the same thing: `"binned"` splits the
  retained sample between the curve's own points and fills them with one transform, which is the
  cheap way to draw a curve of a large sample. It honours the fit's boundary correction, because a
  curve is `%pdf` on equally spaced points and the two must not disagree about what correction is
  in force, and it needs at least two points. `threads=` governs the exact form only: a binned
  curve is one pass over the sample and one transform, with no per-point work for a team to share.

    **With no `method=`, the curve chooses.** It is filled by the binned transform where that is as
    accurate as the exact sum, and by the exact sum everywhere else, so the default is never the
    worse answer. Three things have to hold for it to bin: the kernel is `"gaussian"` or
    `"bspline"`, the two whose binned estimate converges as the square of the cell width; the fit
    is unbounded or corrected by `"reflect"`, the two under which that order survives; and the
    curve has at least sixteen points per bandwidth, below which the cells are too coarse to
    resolve the kernel whatever the rest says. A one-point curve is always exact, since it cannot
    be binned at all -- though `method="binned"` named there still refuses. `%curve` does not
    report which method it used; those are the rules.

    **On a fit made with `method="binned"` the argument is refused.** That object is its
    grid, so a curve of it is that grid read at the curve's points and there is no exact sum
    to choose instead; the choice was made at `%fit`.
- **`threads=`** on the array forms of `%pdf`, `%cdf` and `%quantile`, and on `%curve`, shares the
  points among a team. Each point is answered by one thread alone, so the answer is the same bits at
  every thread count, and a team opens only where the points within the kernels' reach make the
  work worth sharing.

The counts come back as `int64` functions: `%n()` (the elements passed to `%fit`), `%n_valid()`
(the population), `%n_null()`, `%n_nan()` and `%n_outside()`. `%sum_weights()` is `sum(w)` over the
population, `%kernel(name)` the kernel's token, `%method(name)` how the queries are answered, and
`%is_fitted()` says whether `%fit` has run since the last `%clear()`. `%print([unit])` writes all of
it as one block.

## Streaming into a grid: `pf_kde_grid`

A grid lays `ncells` cells across `[xmin, xmax]`. Each point `%add` accepts deposits its kernel on
the cells and is forgotten, so the grid's memory is its cells whatever the sample's size.

**The lifecycle is `%init` -> `%add`* -> `%finish` -> query.** A grid has no `%fit` to be the seam
between filling it and reading it, so the seam is named: `%finish` closes the accumulation, and
only then do the queries answer. A query before it aborts, `%add` and `%merge` after it abort, and
`%clear` reopens the grid, empty. `finish=.true.` on the last `%add` or `%merge` is the one-line
form. The seam is also what the binned method needs: binning streams, but the one convolution that
turns the bins into cells cannot start until the last point has arrived.

```fortran
call g%init(ncells, xmin, xmax, bandwidth, [kernel], [pilot], [alpha], [bandwidth_max], &
            [spread_max], [lower], [upper], [boundary], [method])
call g%add(x, [is_valid], [weights], [skipnan], [n_null], [n_nan], [n_outside], &
           [n_overreach], [threads], [finish])
call g%merge(other, [finish])
call g%finish()
call g%density(f, [x], [normalise])
call g%pdf(x, f, [threads])
call g%cdf(x, p, [threads])
call g%quantile(p, x, [threads])
call g%sample(v, seed, [stream], [threads])
call g%grid(x)
call g%method(name)
```

- **`%init`** makes cells of width `step = (xmax - xmin)/ncells`, cell `i` centred on
  `xmin + (i - 1/2)*step`. The bandwidth is always a number, since a grid has seen no data when it
  is set up: to use a rule, fit a `pf_kde` to a subsample and pass its `%bandwidth()`. `kernel`,
  `lower`, `upper` and `boundary` are as for `%fit`, and `[xmin, xmax]` must lie inside the support.
  A second `%init` discards everything, the finished state included. `method` chooses how the
  cells are filled, and is the one argument that changes what the grid costs rather than what it
  means -- see [Two ways to fill a grid](#two-ways-to-fill-a-grid).
- **`%add`** takes one point or an array, `real64` or `real32`, or a numeric `parquet_column` as
  `%fit` does, under the population rules of `%fit`. `n_null`, `n_nan` and `n_outside` report what
  that call excluded and `n_overreach` what R3 found in it, and the accessors
  (`%n()`, `%n_valid()`, `%n_null()`, `%n_nan()`, `%n_outside()`, `%n_overreach()`,
  `%sum_weights()`) the totals over
  every call. `finish=.true.` closes the grid after that call.
- **`%finish`** closes the accumulation; `%is_finished()` reports whether it has run, and a second
  `%finish` is a no-op. The accessors and `%print` answer at any point in the lifecycle; the five
  queries need a finished grid.
- **Each point adds exactly its weight**, to rounding, at every cell width. Its kernel is evaluated
  at every cell centre it reaches and scaled so that the values sum to the point's weight, and a
  kernel crossing a bound is corrected onto the cells inside it as `boundary=` says. Under
  `method="binned"` it holds of the whole deposit rather than point by point: the cells and the two
  beyond-range counts still sum to the weight added, because the convolution leaves the total
  untouched, but no single point's share of the cells is exactly its own weight.
- **Weight beyond the range is counted, not located.** The part of a kernel reaching past `xmin` or
  `xmax` goes into a count at that end. The cells still hold the estimate over every point, and
  `%cdf` below `xmin` is the counted share: the grid knows how much weight lies beyond each end, not
  where. Under `method="binned"` it is located first and then summed -- the transform's array
  extends past both ends, and the count is what its cells there hold -- which is a stronger
  statement of the same promise, not a weaker one.
- **A bandwidth narrower than a cell is used as given.** Such a kernel reaches one or two centres,
  or none when it falls between two, and its weight lands whole in the cells it reaches or in the
  cell holding the point: the grid, not the kernel, then sets the smoothing. Nothing is widened, and
  `%bandwidth()` answers what was asked for.

Reading it:

- **`%density(f, [x], [normalise])`**: the estimate at every cell centre, with `x` receiving the
  centres. `normalise=.false.` skips the division by the total weight and answers the weighted count
  per unit length.
- **What a query costs.** `%finish` forms the total mass and the running integral once, so
  `%density` costs one pass over the cells and `%pdf`, `%cdf` and `%quantile` cost a segment
  look-up and a few operations each -- independent of the cell count, apart from `%quantile`'s
  binary search over it. Nothing a query does grows with the number of points the grid has seen.
- **`%pdf(x, f, [threads])`**: linear between neighbouring centres, constant over the outer half of the first
  and the last cell, zero outside `[xmin, xmax]`; at a centre it is `%density` exactly.
- **`%cdf(x, p)`**: the integral of `%pdf` from `xmin`, plus the share counted below `xmin`. Below
  `xmin` it is that share and above `xmax` one less the share counted above; it is exactly 0 at and
  below a lower bound and exactly 1 at and above an upper one.
- **`%quantile(p, x)`**: the smallest point of `[xmin, xmax]` at which `%cdf` reaches `p`, solved
  exactly on the quadratic `%cdf` is between two centres. A quantile the share below `xmin` covers
  answers `xmin`, one only the share above `xmax` reaches answers `xmax`, and `p = 0` and `p = 1`
  answer where the accumulated density starts and ends inside the range.
- `%grid(x)` fills the centres, `%ncells()` and `%step()` answer the geometry, and `%bandwidth()`,
  `%kernel(name)`, `%method(name)` and `%bounds(lo, hi)` the settings; `%is_finished()` says whether
  the queries are open. `%clear()` empties the grid and keeps its
  geometry and settings; `%print([unit])` writes all of it as one block.

### Two ways to fill a grid

`method=` on `%init` chooses between them, and `"exact"` is the default: nothing a caller has
today changes.

- **`method="exact"`** evaluates each point's kernel at every cell centre it reaches. It is the
  accurate one, and what to reach for when plot-grade is not enough.
- **`method="binned"`** splits each point between the two centres around it -- the share in
  proportion to its nearness to each -- and fills the cells with one cosine transform when
  `%finish` is called. Binning still streams, so `%add`, `%merge`, `threads=` and the grid's memory
  are what they are under `"exact"`; only the convolution waits for the end.

**What the binned method approximates.** It answers the exact estimate of a sample whose points
have been moved to the cell centres. That error falls as the SQUARE of the cell width for the
Gaussian and the cubic B-spline; the Epanechnikov kernel's first derivative jumps at the edge of
its support and the box kernel steps there, so moving a point by half a cell moves that edge, and
both converge more slowly -- the box by far the slowest of the four, and not monotonically, so a
finer grid does not reliably give a better answer under it.

**The boundary correction decides the order too.** Unbounded, and under `boundary="reflect"`, the
binned estimate keeps its second order: a reflection is the same kernel placed elsewhere, which one
convolution still carries. Under `"renormalise"` and `"linear"` the correction varies from place to
place while the transform carries a single kernel, and the binned estimate drops to FIRST order --
converging half as fast, and at a few thousand cells tens of times further from the exact estimate
than the unbounded case. It is still a density and still converges; it is the rate that changes.

The weight beyond each end is the transform's own pad, and `%pdf`, `%cdf`, `%quantile` and
`%sample` read the filled cells without knowing which method filled them.

**The adaptive kernel is bucketed.** A convolution has exactly one kernel width, so the binned
method sorts the per-point bandwidths into geometrically spaced classes and convolves each class
at its own width, splitting every point's weight between the two classes around its own bandwidth.
The class count follows the bandwidth SPREAD, so the accuracy is the same whatever the data does,
and `%print` reports the count in force. **The binned adaptive estimate does not converge with the
cell width the way the fixed one does**: once the cells are fine enough, bucketing rather than
binning is the error, and refining the grid past that point buys nothing -- the difference from the
exact adaptive estimate flattens out instead of falling. It is a floor, not a slower rate.
`bandwidth_max=` bounds the spread and so bounds both the class count and that floor, which matters
more here than it does under `"exact"`; where the adaptive estimate has to converge, use
`"exact"`.

**What it costs.** Binning is one pass over the points whatever the bandwidth, where a deposit
costs a pass over the cells each kernel reaches, so the saving grows with the bandwidth and with
the cell count and is largest on the big samples a grid exists for. Against it stands one transform
per bandwidth class, and two under `boundary="linear"`, whose cost depends on the cells and not at
all on the points. `bench/benchmark_kde.sh MODE=binned` measures both halves and compares the two
methods at matched cell counts and at matched accuracy.

**How close the grid comes to the exact estimate.** At a cell centre the grid's density differs
from `pf_kde%pdf` only by how finely the cells sample each kernel, and between centres `%pdf` adds
the error of a straight line. For the Gaussian and B-spline kernels the difference falls by four
each time the cells halve, once a bandwidth spans a cell or more. The Epanechnikov estimate has
kinks and the box estimate steps, and both converge more slowly. A few cells per bandwidth is a
sensible start.

**What it costs.** A deposit costs time in proportion to the cells one kernel reaches, and a merge
in proportion to the cells. `%pdf` at a point is a constant; `%cdf` and `%quantile` form a running
sum over the cells once per call, so an array of points costs one pass over the cells plus a
constant per point. `bench/benchmark_kde.sh` measures the deposit (`MODE=grid`) and the order of
convergence (`MODE=accuracy`).

The catalogue too large to hold, read one row group at a time through the slice regime (see
[Reading part of a file: the slice regime](../tables/table-open.html#reading-part-of-a-file-the-slice-regime)):

```fortran
type(pf_kde_grid) :: dens
type(parquet_table) :: t
integer(int64), allocatable :: bounds(:,:)
real(real64), allocatable :: z(:)
real(real64) :: f(400), zc(400)
integer :: rg

call parquet_table_row_group_bounds("big.parquet", bounds)
call dens%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64)
do rg = 1, size(bounds, 2)                          ! one row group at a time
    call parquet_open_table(t, "big.parquet", bounds(1, rg), bounds(2, rg))
    call t%get("z", z)
    call dens%add(z)
end do
call dens%finish()                                  ! the seam: the queries open here
call dens%density(f, x=zc)
```

**The adaptive kernel takes two passes.** `pilot=` on `%init` gives a grid the adaptive rule of
`%fit`: a first pass builds the pilot, a second deposits each point at the bandwidth the pilot
gives it.

```fortran
type(pf_kde_grid) :: pilot, dens
type(parquet_table) :: t
integer(int64), allocatable :: bounds(:,:)
real(real64), allocatable :: z(:)
real(real64) :: f(400), zc(400)
integer :: rg

call parquet_table_row_group_bounds("big.parquet", bounds)
call pilot%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64)
do rg = 1, size(bounds, 2)                          ! pass one: the pilot
    call parquet_open_table(t, "big.parquet", bounds(1, rg), bounds(2, rg))
    call t%get("z", z)
    call pilot%add(z)
end do
call pilot%finish()                                 ! a pilot is read, so it is finished first

call dens%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64, pilot=pilot)
do rg = 1, size(bounds, 2)                          ! pass two: the estimate
    call parquet_open_table(t, "big.parquet", bounds(1, rg), bounds(2, rg))
    call t%get("z", z)
    call dens%add(z)
end do
call dens%finish()
call dens%density(f, x=zc)
```

- **The pilot is copied**, so it may be discarded or reused, and its range must cover the grid's.
  Its cells and bandwidth need not match the grid's: the pilot shapes only the bandwidths, and
  the usual choice is the grid's own bandwidth. `alpha=`, `bandwidth_max=` and `spread_max=` are
  as for `%fit`.
- **`g` and every bandwidth are read exactly as `%fit` reads them**, so a grid given
  `pf_kde%pilot`'s copy reproduces that fit's adaptive estimate to the grid's resolution.
- **A pilot with no density to read** -- one holding a kept NaN (`skipnan=.false.`), or nothing
  in its cells because every row of the first pass was excluded or lay beyond its range -- makes
  the grid built on it answer NaN, as any other data condition does.

**One grid per thread, merged at the end, is the parallel form.** `%merge(other)` adds another
grid's cells and counts to this one, and refuses a grid that differs in its cells, range,
bandwidth, kernel, support, boundary correction or pilot (the same pilot cell for cell, with the
same `alpha` and `bandwidth_max`). Hold the per-thread grids in an array made before the region,
not in a `block` or a `private` copy inside it: a grid owns an allocatable array, and a derived
type that does is not reliably copied or created per thread on every compiler.

```fortran
program grid_per_thread
    use parquet
    use omp_lib, only: omp_get_max_threads, omp_get_thread_num
    use iso_fortran_env, only: int64, real64
    implicit none

    integer(int64), allocatable :: bounds(:,:)
    type(pf_kde_grid), allocatable :: part(:)
    type(pf_kde_grid) :: dens
    real(real64) :: f(400), zc(400)
    integer :: rg, i

    call parquet_table_row_group_bounds("big.parquet", bounds)
    allocate(part(omp_get_max_threads()))
    do i = 1, size(part)
        call part(i)%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64)
    end do

    !$omp parallel do
    do rg = 1, size(bounds, 2)
        block
            type(parquet_table) :: t
            real(real64), allocatable :: z(:)
            call parquet_open_table(t, "big.parquet", bounds(1, rg), bounds(2, rg))
            call t%get("z", z)
            call part(omp_get_thread_num() + 1)%add(z)
        end block
    end do
    !$omp end parallel do

    call dens%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64)
    do i = 1, size(part) - 1
        call dens%merge(part(i))
    end do
    call dens%merge(part(size(part)), finish=.true.)   ! the one-line form of %finish
    call dens%density(f, x=zc)
end program grid_per_thread
```

## Drawing from the estimate: `%sample`

```fortran
call k%sample(v, seed, [stream], [threads])
call g%sample(v, seed, [stream], [threads])
```

`%sample` fills `v` with `size(v)` independent draws from the estimate.

- **`pf_kde` draws the smoothed bootstrap**: a retained point, chosen in proportion to its weight,
  then its kernel, at the point's own bandwidth, drawn about it. The draws follow `%pdf` exactly,
  the adaptive kernel's included. A draw beyond the Gaussian's cut is drawn again from the same
  kernel; under `"renormalise"` so is a draw beyond a bound, and under `"reflect"` it is mirrored
  back inside, so the draws follow the corrected estimate and never leave the support. Under
  `"linear"` a draw first chooses between the two boundary zones and the interior by the mass each
  holds: in the interior it is the bootstrap as above, truncated there, and in a zone it is drawn
  from one point's own corrected kernel and kept with the probability the estimate gives it -- a
  draw there costs many times an interior one, and `bench/benchmark_kde.sh` (`MODE=boundary`)
  measures how many.
- **`pf_kde_grid` draws from the density its `%pdf` describes** over `[xmin, xmax]`, each draw
  inverting the integral of that piecewise-linear density. Weight the grid counted beyond its range
  has no place in it, so no draw lands there: the draws follow the density inside the range,
  normalised to the weight it holds.
- **Each draw is addressed by `(seed, stream, k)`**: element `k` of `v` is a function of the three
  and of the estimate alone. A longer sample starts with a shorter one, another `stream` is an
  independent sample, and `threads=` changes nothing but the time. `seed` is `int64`; `stream` is
  `int32` or `int64`, and 0 when absent. The Gaussian kernel's draws repeat to the bit for a given
  system mathematics library, as the library's normal draws do (see
  [The promise table](random.html#the-promise-table)).
- **The draws are independent of a program's own** at the same `seed`: the estimator derives a seed
  of its own from it, one for each form, so neither shares a number with `pf_random_at(seed, ...)`
  or with the other form's draws.
- **An undefined estimate draws NaN**, as does a grid with nothing accumulated inside its range or
  one a kept NaN has poisoned.

A draw costs about what a dozen kernel evaluations do, and a weighted one about twice that;
`bench/benchmark_kde.sh` measures it (`MODE=sample`).

## The estimate as a function

To integrate the estimate, or pass it anywhere a function is expected, extend `pf_integrand` from
`parquet_integrate` with a component holding the fitted object:

```fortran
type, extends(pf_integrand) :: density_fn
    type(pf_kde) :: k
contains
    procedure :: eval => density_eval
end type density_fn

function density_eval(self, x) result(f)
    class(density_fn), intent(inout) :: self
    real(real64), intent(in) :: x
    real(real64) :: f
    call self%k%pdf(x, f)
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
population that remains. `pf_kde_grid%add` applies the same rules to every call, and its messages
name `pf_kde_grid%add`.

## What returns quietly

A data condition never aborts:

- **An empty population** (every element excluded, or an empty `x`) leaves the object fitted with
  `ok = .false.`. Every query answers a quiet NaN, `%curve` fills `f` with NaN and `x` with NaN
  unless both ends were given, and `%quantile` answers NaN.
- **A population a rule cannot scale** (one point, or a constant sample) is the same: `ok =
  .false.` and NaN everywhere, `%bandwidth()` included.
- **`rule="isj"` named, on a sample the rule finds no bandwidth for**, is the same too. Without
  `rule=`, Silverman's rule gives the bandwidth instead, and `%rule(name)` answers `"silverman"`.
- **`skipnan=.false.` with a NaN present** keeps the NaN, which makes the whole estimate NaN, with
  `ok = .false.`
- **A query at a NaN point** answers NaN; at an infinite point, the density is zero and `%cdf` is 0
  or 1.
- **A grid with nothing accumulated** answers zeros from `%density`, as an empty histogram does,
  and NaN from `%pdf`, `%cdf` and `%quantile`, which have nothing to normalise by.
- **`skipnan=.false.` with a NaN in any `%add`** makes every later answer of that grid NaN, until
  `%clear()`.
- **A grid whose pilot has no density to read** -- a pilot holding such a NaN, one never given a
  point, or one whose points all lay beyond its range -- answers NaN from the start, `%clear()` or
  not, and still counts what `%add` accepts.
- **A bandwidth too large to use** -- an explicit one whose kernel would reach beyond the largest
  number, one that `adjust` would carry there, or an adaptive one that overflows -- leaves the fit
  undefined, `ok = .false.`, and an adaptive grid it reaches answers NaN.
- **An adaptive fit of a population it could not fit** has no pilot: `%pilot` answers a grid that
  was never initialised.
- **`%sample` from an undefined estimate** fills `v` with NaN, and so does a grid's `%sample` when
  nothing lies inside its range to draw from, or a kept NaN has poisoned it.

## What aborts

Every abort is a caller contract that was broken, and names the binding it came from.

| Condition | Message |
|---|---|
| `bandwidth` NaN, infinite or `<= 0` | `pf_kde%fit: bandwidth must be a finite, positive number` |
| `bandwidth=` and `rule=` both given | `pf_kde%fit: bandwidth= and rule= cannot both be given; use adjust= to scale a rule` |
| an unknown `rule` | `pf_kde%fit: rule must be "isj", "lscv", "silverman" or "scott"` |
| `adjust` NaN, infinite or `<= 0` | `pf_kde%fit: adjust must be a finite, positive number` |
| an unknown `kernel` | `pf_kde%fit: kernel must be "gaussian", "epanechnikov", "bspline" or "box"` |
| `lower` or `upper` NaN or infinite | `pf_kde%fit: lower and upper must be finite` |
| `lower >= upper` | `pf_kde%fit: lower must be below upper` |
| `boundary=` with no bound | `pf_kde%fit: boundary= needs lower= or upper=` |
| an unknown `boundary` | `pf_kde%fit: boundary must be "renormalise", "reflect" or "linear"` |
| `is_valid` or `weights` of the wrong size | `pf_kde%fit: weights has 3 elements but values has 4` (the family's text) |
| a negative, NaN or infinite weight | `pf_kde%fit: weight 2 is negative; weights must be finite and non-negative` (the family's text) |
| an unknown `weight_type` | `pf_kde%fit: weight_type "..." is not recognised; ...` (the family's text) |
| a column of a kind other than `int32`, `int64`, `float32` or `float64` | `pf_kde%fit: a column of kind PK_LOGICAL holds no numbers to place kernels at; the column must be int32, int64, float32 or float64` (naming the kind) |
| `is_valid=` beside a column | `pf_kde%fit: is_valid= cannot be given alongside a parquet_column; ...` (the family's text) |
| `threads <= 0` | `pf_kde%fit: threads must be positive` (or `%pdf`, `%cdf`, `%quantile`, `%curve`, `%sample`) |
| a query or accessor before `%fit`, or after `%clear` | `pf_kde%pdf: the estimate has not been fitted` |
| an output of the wrong size | `pf_kde%pdf: f must have one element per point of x`, `pf_kde%cdf: p must have ...`, `pf_kde%quantile: x must have one element per element of p` |
| `p` outside `[0, 1]`, or NaN | `pf_kde%quantile: p must lie in [0, 1]` |
| `%curve` with `x` and `f` of different sizes | `pf_kde%curve: x and f must have the same size` |
| `xmin` or `xmax` infinite or NaN | `pf_kde%curve: xmin and xmax must be finite` |
| `xmin >= xmax` | `pf_kde%curve: xmin must be below xmax` |
| `cut` negative or NaN | `pf_kde%curve: cut must not be negative` |
| `alpha=`, `bandwidth_max=` or `spread_max=` without `adaptive=.true.` | `pf_kde%fit: alpha=, bandwidth_max= and spread_max= need adaptive=.true.` |
| `spread_max` NaN, infinite or `< 1` | `pf_kde%fit: spread_max must be a finite number of at least 1` |
| `alpha` outside `[0, 1]`, or NaN | `pf_kde%fit: alpha must lie in [0, 1]` |
| `bandwidth_max` NaN, infinite or `<= 0` | `pf_kde%fit: bandwidth_max must be a finite, positive number` |
| `%bandwidths` with an output of the wrong size | `pf_kde%bandwidths: h must have one element per retained point` (`x must have ...` for the points) |
| `%bandwidth_at` with an output of the wrong size | `pf_kde%bandwidth_at: h must have one element per point of x` |
| `%pilot` on a fit that is not adaptive | `pf_kde%pilot: the fit is not adaptive` |
| `ncells < 1` | `pf_kde_grid%init: ncells must be positive` |
| `xmin` or `xmax` NaN or infinite | `pf_kde_grid%init: xmin and xmax must be finite` |
| `xmin >= xmax` | `pf_kde_grid%init: xmin must be below xmax` |
| a range wider than the largest number, or cells too narrow to represent | `pf_kde_grid%init: the cell width (xmax - xmin)/ncells must be a finite, positive number` |
| `[xmin, xmax]` reaching outside `[lower, upper]` | `pf_kde_grid%init: the grid's range must lie inside the support` |
| under `boundary="linear"`, a range that does not start at `lower` or end at `upper` (R1) | `pf_kde_grid%init: under boundary="linear" the grid's range must start at lower and end at upper` |
| an unknown `method` | `pf_kde_grid%init: method must be "exact" or "binned"` (or `pf_kde%fit`, `pf_kde%curve`) |
| `%curve(method=)` on a fit made with `method="binned"` | `pf_kde%curve: method= cannot be given for a fit made with method="binned"` |
| `method="binned"` with a bandwidth whose padded transform would be too long | `pf_kde_grid%init: method="binned" needs a transform longer than ... cells can carry` |
| `%curve(method="binned")` on a single point | `pf_kde%curve: method="binned" needs at least two points` |
| `pilot=` without `adaptive=.true.` | `pf_kde%fit: pilot= needs adaptive=.true.` |
| `pilot=` a grid that was never finished | `pf_kde%fit: pilot must be a finished grid; call %finish on it` |
| `pilot=` a grid built with another kernel | `pf_kde%fit: the pilot must use the same kernel as this fit` |
| `pilot=` a grid built under another correction | `pf_kde%fit: the pilot must use the same boundary correction as this fit` |
| `pilot=` a grid built over another support | `pf_kde%fit: the pilot must have the same support as this fit` |
| under `boundary="linear"` with one bound, a range narrower than one kernel's reach (R2) | `pf_kde_grid%init: under boundary="linear" the grid must be at least one kernel reach wide` |
| `bandwidth`, `kernel`, `alpha`, `bandwidth_max`, `lower`, `upper` or `boundary` as `%fit` refuses them | `%fit`'s texts, naming `pf_kde_grid%init` |
| `alpha=`, `bandwidth_max=` or `spread_max=` without `pilot=` | `pf_kde_grid%init: alpha=, bandwidth_max= and spread_max= need pilot=` |
| `pilot=` a grid never initialised | `pf_kde_grid%init: pilot must be an initialised grid` |
| `is_valid` or `weights` of the wrong size, a bad weight, `threads <= 0`, a column of another kind, `is_valid=` beside a column | `%fit`'s texts, naming `pf_kde_grid%add` (`pf_kde_grid%sample` for its `threads`) |
| a query, accessor, `%add` or `%merge` before `%init` | `pf_kde_grid%pdf: the grid has not been initialised` |
| `%merge` with a grid never initialised | `pf_kde_grid%merge: the other grid has not been initialised` |
| `%merge` with a grid set up differently | `pf_kde_grid%merge: the two grids differ in cells` (or `range`, `bandwidth`, `kernel`, `support`, `boundary`, `pilot`) |
| `%density` or `%grid` with an output of the wrong size | `pf_kde_grid%density: f must have one element per cell` (`x must have ...` for the centres) |
| `%pdf`, `%cdf` or `%quantile` with an output of the wrong size, or `p` outside `[0, 1]` | `pf_kde`'s texts, naming `pf_kde_grid` |

## Thread safety

**A fitted `pf_kde`, and a `pf_kde_grid`, are read-only under every query**, so any number of
threads may query one object at once. `%fit`, `%init`, `%add`, `%merge` and `%clear` are writes:
give each thread its own object, or write once before the region and share the result; one grid
per thread merged at the end is how several threads accumulate one estimate.

- **`threads=` on `%fit`** sets the team for the sort, the rules' statistics and the adaptive
  kernel's pilot; the answer does not depend on it. The pilot's deposit is cut into pieces the
  sample decides, which the team takes in turn.
- **`threads=` on `%add`** gives each thread of a team a contiguous share of the points and its own
  partial grid, added to the grid in thread order. At one thread count the answer is the same every
  time; two thread counts group the additions differently and agree to rounding.
- **`threads=` on the bulk queries and `%sample`** shares the points, probabilities or draws among a
  team, each answered by one thread alone, so the answer is the same bits at every thread count. A
  team opens only where the work is worth sharing: a few queries over a small sample, or a few
  thousand draws, stay on the calling thread whatever `threads=` asks.
- Inside your own parallel region every one of them stands down to one thread unless given
  explicitly. An abort inside a parallel region is taken by one thread.

## What it costs to import

`use parquet_kde` compiles the statistics tier it is built on (`parquet_stats`, and beneath it the
sorting tier), the random-number generator `%sample` draws from (`parquet_random`), the root finder
and the discrete cosine transform the ISJ rule is built on (`parquet_root`, `parquet_transform`),
and the module's own four files; no reader, writer or C++ boundary. It re-exports
the verbosity and message-stream pair, which `%print` reads, so a program importing it alone can
silence its output with `parquet_set_verbosity("silent")`.
