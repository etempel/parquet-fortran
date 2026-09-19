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

call k%fit(mag)                    ! Silverman's rule, Gaussian kernel: the defaults
call k%curve(xg, fg)               ! the density on 200 points across the data
print '(a, f8.4)', 'bandwidth = ', k%bandwidth()
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call k%fit(x, [bandwidth], [rule], [adjust], [kernel], [adaptive], [alpha], [bandwidth_max], &
           [lower], [upper], [boundary], [is_valid], [weights], [weight_type], [skipnan], &
           [n_null], [n_nan], [n_outside], [ok], [threads])
call k%pdf(x, f, [threads])
call k%cdf(x, p, [threads])
call k%quantile(p, x, [threads])
call k%curve(x, f, [xmin], [xmax], [cut], [threads])
call k%sample(v, seed, [stream], [threads])
call k%bandwidths(h, [x])
call k%bandwidth_at(x, h)
call k%pilot(g)
call k%print([unit])
```

`x` in `%fit` is a `real64` or `real32` array; a `real32` sample is widened before anything else
happens to it. `%pdf`, `%cdf`, `%quantile` and `%bandwidth_at` each take a scalar or a rank-1
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
  arrives, and the adaptive kernel takes a second pass. See
  [Streaming into a grid](#streaming-into-a-grid-pf_kde_grid).

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
`pf_kde_grid%init` takes the same three arguments, and its range must lie inside the support.

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
h_j = h * (p(x_j)/g)**(-alpha)          at most bandwidth_max
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
- **`bandwidth_max`** caps every point's bandwidth. An outlier has a pilot density near zero and
  so a very wide kernel, and since every query sums the points within reach of the widest
  kernel, one far outlier slows every query. The cap is the remedy.
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
  `bandwidth_max` when it is given, and otherwise the bandwidth of the pilot's smallest positive
  density.

Reading the adaptive fit:

- **`%bandwidths(h, [x])`**: every retained point's bandwidth, in the order of the points,
  ascending, with the points themselves in `x` on request. A fixed fit answers `%bandwidth()` for
  every point.
- **`%bandwidth_at(x, h)`**: the bandwidth the rule gives a point at `x`, read from the pilot as
  `%fit` read it for each retained point.
- **`%pilot(g)`**: a copy of the pilot. Given to `pf_kde_grid%init(pilot=g)` with the same
  bandwidth and `alpha`, it gives every point the bandwidth `%fit` gave it, so the grid converges
  to the same adaptive estimate.
- **`%is_adaptive()`** says whether the fit was asked for the adaptive kernel.

The adaptive `%fit` costs one grid pass for the pilot and one look-up per point beyond the fixed
one; `bench/benchmark_kde.sh` measures both (`MODE=adaptive`).

## Reading the estimate

- **`%pdf(x, f)`**: the density at a point, or at each point of an array.
- **`%cdf(x, p)`**: `P(X <= x)` at a point or at each point of an array, clamped to `[0, 1]`.
- **`%quantile(p, x)`**: the smallest `x` at which `%cdf` reaches `p`, found by Newton's method on
  `%cdf` inside a bracket, with bisection whenever a step would leave it. `p = 0` and `p = 1` give
  the two ends of the estimate's support: the extreme points minus and plus one kernel's reach,
  clipped to the bounds. A `p` outside `[0, 1]` aborts.
- **`%curve(x, f, [xmin], [xmax], [cut], [threads])`**: fills `x` with `size(x)` equally spaced
  points, both ends exact, and `f` with the density at each. The default range is the sample's
  minimum minus `cut` bandwidths to its maximum plus `cut` bandwidths, `cut` defaulting to 3,
  clipped to the support; `xmin=` and `xmax=` replace either end.
- **`threads=`** on the array forms of `%pdf`, `%cdf` and `%quantile`, and on `%curve`, shares the
  points among a team. Each point is answered by one thread alone, so the answer is the same bits at
  every thread count, and a team opens only where the points within the kernels' reach make the
  work worth sharing.

The counts come back as `int64` functions: `%n()` (the elements passed to `%fit`), `%n_valid()`
(the population), `%n_null()`, `%n_nan()` and `%n_outside()`. `%sum_weights()` is `sum(w)` over the
population, `%kernel(name)` the kernel's token, and `%is_fitted()` says whether `%fit` has run since
the last `%clear()`. `%print([unit])` writes all of it as one block.

## Streaming into a grid: `pf_kde_grid`

A grid lays `ncells` cells across `[xmin, xmax]`. Each point `%add` accepts deposits its kernel on
the cells and is forgotten, so the grid's memory is its cells whatever the sample's size:

```fortran
call g%init(ncells, xmin, xmax, bandwidth, [kernel], [pilot], [alpha], [bandwidth_max], &
            [lower], [upper], [boundary])
call g%add(x, [is_valid], [weights], [skipnan], [n_null], [n_nan], [n_outside], [threads])
call g%merge(other)
call g%density(f, [x], [normalise])
call g%pdf(x, f)
call g%cdf(x, p)
call g%quantile(p, x)
call g%sample(v, seed, [stream], [threads])
call g%grid(x)
```

- **`%init`** makes cells of width `step = (xmax - xmin)/ncells`, cell `i` centred on
  `xmin + (i - 1/2)*step`. The bandwidth is always a number, since a grid has seen no data when it
  is set up: to use a rule, fit a `pf_kde` to a subsample and pass its `%bandwidth()`. `kernel`,
  `lower`, `upper` and `boundary` are as for `%fit`, and `[xmin, xmax]` must lie inside the support.
  A second `%init` discards everything.
- **`%add`** takes one point or an array, `real64` or `real32`, under the population rules of
  `%fit`. `n_null`, `n_nan` and `n_outside` report what that call excluded, and the accessors
  (`%n()`, `%n_valid()`, `%n_null()`, `%n_nan()`, `%n_outside()`, `%sum_weights()`) the totals over
  every call.
- **Each point adds exactly its weight**, to rounding, at every cell width. Its kernel is evaluated
  at every cell centre it reaches and scaled so that the values sum to the point's weight, and a
  kernel crossing a bound is corrected onto the cells inside it as `boundary=` says.
- **Weight beyond the range is counted, not located.** The part of a kernel reaching past `xmin` or
  `xmax` goes into a count at that end. The cells still hold the estimate over every point, and
  `%cdf` below `xmin` is the counted share: the grid knows how much weight lies beyond each end, not
  where.
- **A bandwidth narrower than a cell is used as given.** Such a kernel reaches one or two centres,
  or none when it falls between two, and its weight lands whole in the cells it reaches or in the
  cell holding the point: the grid, not the kernel, then sets the smoothing. Nothing is widened, and
  `%bandwidth()` answers what was asked for.

Reading it:

- **`%density(f, [x], [normalise])`**: the estimate at every cell centre, with `x` receiving the
  centres. `normalise=.false.` skips the division by the total weight and answers the weighted count
  per unit length.
- **`%pdf(x, f)`**: linear between neighbouring centres, constant over the outer half of the first
  and the last cell, zero outside `[xmin, xmax]`; at a centre it is `%density` exactly.
- **`%cdf(x, p)`**: the integral of `%pdf` from `xmin`, plus the share counted below `xmin`. Below
  `xmin` it is that share and above `xmax` one less the share counted above; it is exactly 0 at and
  below a lower bound and exactly 1 at and above an upper one.
- **`%quantile(p, x)`**: the smallest point of `[xmin, xmax]` at which `%cdf` reaches `p`, solved
  exactly on the quadratic `%cdf` is between two centres. A quantile the share below `xmin` covers
  answers `xmin`, one only the share above `xmax` reaches answers `xmax`, and `p = 0` and `p = 1`
  answer where the accumulated density starts and ends inside the range.
- `%grid(x)` fills the centres, `%ncells()` and `%step()` answer the geometry, and `%bandwidth()`,
  `%kernel(name)` and `%bounds(lo, hi)` the settings. `%clear()` empties the grid and keeps its
  geometry and settings; `%print([unit])` writes all of it as one block.

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

call dens%init(400, 0.0_real64, 2.0_real64, 0.03_real64, lower=0.0_real64, pilot=pilot)
do rg = 1, size(bounds, 2)                          ! pass two: the estimate
    call parquet_open_table(t, "big.parquet", bounds(1, rg), bounds(2, rg))
    call t%get("z", z)
    call dens%add(z)
end do
call dens%density(f, x=zc)
```

- **The pilot is copied**, so it may be discarded or reused, and its range must cover the grid's.
  Its cells and bandwidth need not match the grid's: the pilot shapes only the bandwidths, and
  the usual choice is the grid's own bandwidth. `alpha=` and `bandwidth_max=` are as for `%fit`.
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
    do i = 1, size(part)
        call dens%merge(part(i))
    end do
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
  back inside, so the draws follow the corrected estimate and never leave the support.
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
population that remains. `pf_kde_grid%add` applies the same rules to every call, and its messages
name `pf_kde_grid%add`.

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
| `threads <= 0` | `pf_kde%fit: threads must be positive` (or `%pdf`, `%cdf`, `%quantile`, `%curve`, `%sample`) |
| a query or accessor before `%fit`, or after `%clear` | `pf_kde%pdf: the estimate has not been fitted` |
| an output of the wrong size | `pf_kde%pdf: f must have one element per point of x`, `pf_kde%cdf: p must have ...`, `pf_kde%quantile: x must have one element per element of p` |
| `p` outside `[0, 1]`, or NaN | `pf_kde%quantile: p must lie in [0, 1]` |
| `%curve` with `x` and `f` of different sizes | `pf_kde%curve: x and f must have the same size` |
| `xmin` or `xmax` infinite or NaN | `pf_kde%curve: xmin and xmax must be finite` |
| `xmin >= xmax` | `pf_kde%curve: xmin must be below xmax` |
| `cut` negative or NaN | `pf_kde%curve: cut must not be negative` |
| `alpha=` or `bandwidth_max=` without `adaptive=.true.` | `pf_kde%fit: alpha= and bandwidth_max= need adaptive=.true.` |
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
| `bandwidth`, `kernel`, `alpha`, `bandwidth_max`, `lower`, `upper` or `boundary` as `%fit` refuses them | `%fit`'s texts, naming `pf_kde_grid%init` |
| `alpha=` or `bandwidth_max=` without `pilot=` | `pf_kde_grid%init: alpha= and bandwidth_max= need pilot=` |
| `pilot=` a grid never initialised | `pf_kde_grid%init: pilot must be an initialised grid` |
| `pilot=` a grid whose range does not cover `[xmin, xmax]` | `pf_kde_grid%init: the pilot must cover this grid's range` |
| `is_valid` or `weights` of the wrong size, a bad weight, `threads <= 0` | `%fit`'s texts, naming `pf_kde_grid%add` (`pf_kde_grid%sample` for its `threads`) |
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
sorting tier), the random-number generator `%sample` draws from (`parquet_random`), and the
module's own four files; no reader, writer or C++ boundary. It re-exports
the verbosity and message-stream pair, which `%print` reads, so a program importing it alone can
silence its output with `parquet_set_verbosity("silent")`.
