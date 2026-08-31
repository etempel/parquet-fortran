---
title: Array statistics with the pf_ reduction family
---

`parquet_stats` summarises plain Fortran arrays. It has no reader state, no writer state and no
Arrow underneath it: a program that already has an array in hand can `use parquet_stats` and reduce
it, without compiling the C++ wrapper's dependencies into its build. `use parquet` brings it in too,
so nothing here needs a second import.

The family is broad: counts and moments, order statistics and quantiles, the median absolute
deviation and the mode, two-sample covariance and correlation, the sigma clip, the running folds,
and bins. `pf_stats` is the object that answers many of them off one pass and one ordering.

Every procedure here accepts the same six inputs — see
[What `values` may be](#what-values-may-be) — so the kind of array you happen to have is not a
reason to convert anything first.

## `pf_count_valid` — how many elements are in the population

```fortran
use parquet_stats
use iso_fortran_env, only : int64, real64

real(real64) :: mag(1000), w(1000)
logical      :: ok(1000)
integer(int64) :: n

call pf_count_valid(mag, n)                      ! how many elements there are
call pf_count_valid(mag, n, is_valid=ok)         ! ... excluding the nulls
call pf_count_valid(mag, n, weights=w)           ! ... and the zero-weighted
call pf_count_valid(mag, n, skipnan=.false.)     ! ... counting NaNs as ordinary values
call pf_count_valid(mag, n, is_valid=ok, n_null=nn, n_nan=nx)   ! ... and WHY they left
```

Square brackets in a signature below mark an optional argument.

**`pf_count_valid(values, n, [is_valid], [weights], [skipnan], [n_null], [n_nan])`** answers
pandas' `Series.count()`: how many elements of `values` are in the population. `n`, `n_null` and
`n_nan` are all `integer(int64)`.

`n_null` and `n_nan` report *why* elements left, which is the reason to reach for this procedure
rather than `size(values)`. The three numbers together account for every element except the
zero-weighted ones, and that difference is what a zero weight is:
`size(values) - n_null - n_nan - n` is how many were dropped for carrying no weight. There is no
`ok` here — a count is defined for every population, including an empty one, so there would be
nothing for it to report.

`skipnan` is offered only on the two real kinds and on a `parquet_column`. An integer or logical
array has no NaN to skip, and an argument that can never do anything is worse than an absent one.

## What `values` may be

Every reduction on this page — `pf_count_valid`, the moment family, and `pf_stats`' `%compute` and
`%update` — takes any of six things, and all six reach the same engine:

| `values` | notes |
|---|---|
| `real(real64)` array | the engine's own type; nothing is copied |
| `real(real32)` array | widened exactly; a NaN stays a NaN |
| `integer(int32)` array | widened exactly; no `skipnan`, no `n_nan` |
| `integer(int64)` array | **exact only below 2**53**; no `skipnan`, no `n_nan` |
| `logical` array | `.true.` is 1 and `.false.` is 0, so the mean is the fraction true |
| `type(parquet_column)` | scalar numeric kinds only; see below |

The widening is exact for every kind but one, so a statistic over an `integer(int32)` array is bit
for bit the statistic over the same values as `real(real64)`. The exception is an `integer(int64)`
above 2\*\*53, which cannot be held exactly in a 64-bit real. That is unavoidable for a mean and
irrelevant for the workloads this serves — nobody averages an object ID — but it is *not*
irrelevant for a median, which
[`pf_nth_quantile`](sorting.html) computes exactly on the original array.

### Passing a `parquet_column`

A column is dispatched on its kind at run time and needs no unpacking:

```fortran
type(parquet_column) :: mag
real(real64) :: m

call pf_mean(mag, m)
```

Three things are refused rather than guessed at, each with a message saying what to do instead:

- **a string, temporal or container column** — it has no numeric statistics, and the message names
  the kind it found;
- **a column more than one element wide** — flattening a width-16 column into one population is a
  *different* statistic. Use `%get_elem` for one element position across all rows, or pass the
  flattened array yourself and thereby say so;
- **`is_valid=` alongside a column** — the column carries its own validity, and two sources of
  truth that can disagree is exactly what this refuses. The column's nulls are excluded either
  way, and `n_null=` reports them.

## The moment family

```fortran
real(real64) :: mag(1000), w(1000), m, sd, g
integer(int64) :: nv

call pf_mean(mag, m)                             ! the mean
call pf_stddev(mag, sd, ddof=0)                  ! numpy's default; the library's is ddof=1
call pf_skewness(mag, g, bias=.true.)            ! scipy's g1; the library's default is pandas' G1
call pf_mean(mag, m, weights=w)                  ! weighted

call pf_moments(mag, n_valid=nv, mean=m, stddev=sd, skewness=g)   ! all of it, in one pair of passes
```

**`pf_moments` is the form to reach for in a per-group loop.** Asking for eight statistics costs
exactly what asking for one costs: the population is traversed twice however many outputs are
requested — once for the mean, once for the central moments — and nothing is computed that was not
asked for. Its outputs come before the common argument block and are, in order, `n_valid`, `mean`,
`variance`, `stddev`, `sem`, `skewness`, `kurtosis`, `vsum`, `vmin`, `vmax`.

Its `ok=` reports whether every output the caller **asked for** came back defined — only the present
ones are tested, which is the only reading that works here: `vsum` over an empty population is a
correct `0` and `vmin` is a NaN, so testing all ten would report a failure to a caller who asked
only for the sum and got the right answer. The three counts are always defined and are not tested.

The one-shot forms are `pf_sum`, `pf_mean`, `pf_variance`, `pf_stddev`, `pf_sem`, `pf_skewness` and
`pf_kurtosis`. Each is that same engine with one output, so they agree with `pf_moments` bit for bit.

**An `integer(int64)` population is exact only while every value is.** Every kind widens to
`real64` on the way in — that is what makes all six return the same bits — so a value above `2⁵³`
loses its low bits *before* any arithmetic, and no summation order recovers them:
`pf_sum([2**53+1, 1, 2**53+1, 1])` answers `1.8014398509481984e16` where the exact integer total is
`18014398509481988`. numpy and pandas keep an int64 sum in int64 and stay exact until it wraps. A
population of genuinely huge integers has to be summed by the caller in `int64`, or shifted and
scaled first. `int32` is unaffected: every `int32` value is exactly representable in `real64`.

**`pf_mean` is not literally `sum/n`.** The quotient is taken and then refined by
`sum(w*(x - mu))/sum(w)`, which is algebraically zero and in floating point is the rounding error
left in it — the more accurate answer, and the centre the variance and the higher moments are taken
about, so it has to be this one. It can therefore sit one ulp from the quotient you would write by
hand: the mean of ten logicals of which three are `.true.` is one ulp below `0.3_real64`. Compare
against another reduction of the same population, or with a tolerance, rather than against a
decimal literal.

### Four defaults that differ from numpy or scipy

Each is chosen to match pandas, which is the library whose semantics a per-group workflow already
assumes — but a reader cross-checking a number against numpy needs to know all four, because every
one of them changes a digit rather than raising an error.

| argument | this library | reproduce numpy/scipy with |
|---|---|---|
| `ddof` on the variance family | `1` (the sample variance, as pandas) | `ddof=0` |
| `bias` on skewness and kurtosis | `.false.` — the corrected `G1`/`G2`, as pandas | `bias=.true.` |
| `excess` on the kurtosis | `.true.` — 0 for a normal population | `excess=.false.` |
| `skipnan` | `.true.` — a NaN leaves the population | `skipnan=.false.`, matching `np.mean` rather than `np.nanmean` |

### Weights, and the one choice they force

`weights` is accepted by every reduction from the start, and a **zero weight removes the element**.
Where the weights are unequal the variance has a genuine ambiguity, because the degrees of freedom
can be charged against two different counts, so `weight_type` names which you mean:

- **`"reliability"`** (the default) — a weight says the value is that much more precise, as an
  inverse-variance weight or a membership probability does. `ddof` is charged against Kish's
  effective size `sum(w)**2 / sum(w**2)`.
- **`"frequency"`** — a weight of 3 says the value occurred three times. `ddof` is charged against
  `sum(w)`, and the answer is exactly what you would get by writing the value out three times.

With every weight equal the two agree with each other and with the unweighted answer, which is why
this only has to be decided once, for data that is actually weighted. An unrecognised token aborts.

Worked, on a population of three values where one occurred twice:

```fortran
real(real64) :: x(3) = [10.0_real64, 12.0_real64, 14.0_real64]
real(real64) :: w(3) = [1.0_real64,  2.0_real64,  1.0_real64]
real(real64) :: v

call pf_variance(x, v, weights=w)                          ! reliability -- the default
! -> 3.2   The weighted mean is 12 and sum(w*(x-12)**2) is 8. Kish's effective size
!          is sum(w)**2/sum(w**2) = 16/6 = 2.667, so ddof=1 leaves 4 - 1*6/4 = 2.5
!          to divide by: a weight of 2 says "more precise", not "seen twice", and
!          buys less than one extra independent observation.

call pf_variance(x, v, weights=w, weight_type="frequency")
! -> 2.6667  ddof=1 charged against sum(w) = 4, so the divisor is 3.

call pf_variance([10.0_real64, 12.0_real64, 12.0_real64, 14.0_real64], v)
! -> 2.6667  the same number, which is the property that DEFINES the token: a
!            frequency weight of 2 must agree with writing the value out twice.
```

The same distinction reaches `pf_stddev`, `pf_sem`, `pf_skewness`, `pf_kurtosis`, `pf_moments`,
`pf_cov`, `pf_describe`, every weighted quantile and `pf_stats%init`. It does **not** reach
`pf_corr`, `pf_trim_mean`, `pf_mad` or `pf_percentile_of_score`: in a correlation the `ddof` in the
covariance and the two in the standard deviations cancel exactly, and the other three have no
degrees-of-freedom question to charge anything against. An argument that could never change an
answer is worse than an absent one, so those procedures do not take it.

### Which procedures take `weights`, and why the rest do not

The rule is the reference libraries' own: a procedure here takes `weights` exactly where numpy,
scipy or pandas offers a weighted form of the same statistic.

**Takes `weights`** — `pf_count_valid`, `pf_sum`, `pf_mean`, `pf_gmean`, `pf_hmean`, `pf_variance`,
`pf_stddev`, `pf_sem`, `pf_skewness`, `pf_kurtosis`, `pf_moments`, `pf_median`, `pf_quantile`,
`pf_quantiles`, `pf_iqr`, `pf_trim_mean`, `pf_percentile_of_score`, `pf_mad`, `pf_describe`,
`pf_cov`, `pf_corr` (Pearson only), `pf_mode`, `pf_bucketize`, `pf_histogram`, `pf_bin_edges`,
and `pf_stats%compute`/`%update`.

**Does not, and the argument is absent rather than ignored** — so passing one is a compile error
rather than something to discover at run time:

| procedure | why |
|---|---|
| `pf_zscore` | scipy's `zscore` has no weighted form. A weighted mean over an unweighted scale, or both weighted, are different standardisations and neither leaves the output with unit variance under every convention |
| `pf_sigma_clipped_stats` | astropy's `sigma_clipped_stats` has none either; a weighted scale estimator inside an iterative clip is a further definitional choice |
| `pf_cumsum`, `pf_cumprod`, `pf_cummax`, `pf_cummin` | pandas' `cum*` are per-element by nature. A weighted running sum is `pf_cumsum(w*x)`, written at the call site, and the other three have no weighted meaning worth choosing between |
| `pf_corr(method="spearman")` | no reference library defines a weighted midrank; passing `weights` with that token aborts rather than inventing one |

`pf_bucketize` takes `weights` for one reason only: a zero weight has to remove an element there
too, or the documented `pf_histogram` = `pf_bucketize`-tallied identity would be false whenever the
histogram is weighted. A weight never changes a bin number.

### Accuracy is a documented property, not an implementation detail

The variance and the higher moments are computed in **two passes** — the mean first, then the
central moments about it — and the sums are **pairwise**, over a fixed block tree. Both matter for
the data this library is usually pointed at: an MJD around 60000 with a millisecond scatter, or a
magnitude around 20 with a millimagnitude scatter, is exactly the input on which the textbook
`sum(x**2) - sum(x)**2/n` cancels away every significant digit.

The testable form of the claim is that `pf_variance(x + c)` equals `pf_variance(x)` for a large `c`,
and it holds to a few ulp rather than approximately. The same is true of the skewness, which needs a
second correction to get there and has it.

A second consequence of the fixed block tree is that **the answer does not depend on the thread
count**: the decomposition is a function of the population size alone and never of who walks it. See
[Threading](#threading) below.

### When a statistic is undefined

An undefined answer is a **quiet NaN**, and `ok=` reports it — never an abort. `ok` is `.true.`
exactly when the value beside it is not a NaN.

| population | what comes back |
|---|---|
| empty, or every element excluded | `vsum` is exactly `0`; everything else is NaN |
| `ddof >= n_valid` — including one element at the default `ddof=1` | variance, stddev and sem are NaN |
| every value identical | variance exactly `0`; skewness and kurtosis NaN |
| fewer than 3 (skewness) or 4 (kurtosis) elements, bias-corrected | that statistic is NaN |
| contains `+Inf` (or only `-Inf`) | `sum` and `mean` are that infinity with `ok = .true.`; every central moment is NaN |
| contains **both** `+Inf` and `-Inf` | `sum` and `mean` are NaN too |

The empty sum is `0` rather than NaN because that is the additive identity and what numpy and pandas
both return; `n_valid` sits beside it, so nothing is hidden by it.

**An infinity is a value, not a failure**, so a mean of `+Inf` comes back with `ok = .true.` — which
is numpy's and pandas' answer, and this module's own `pf_sum`'s, and they must not disagree over one
population. Only a NaN sets `ok = .false.`, and the variance genuinely *is* NaN there: every
deviation from an infinite mean is either infinite or `Inf - Inf`. Nothing on this page aborts on a
non-finite **value**; a non-finite **weight**, a non-finite `center=` and a non-finite `score=` all
abort, because those can only come from the caller's own arithmetic rather than from data.

### Threading

The central-moment pass is threaded on a large population, and `threads=` on any moment procedure
overrides the automatic count. **It is a speed control and never an accuracy one:**

```fortran
call pf_variance(x, v)              ! automatic
call pf_variance(x, v, threads=4)   ! four threads
call pf_variance(x, v, threads=1)   ! serial
```

All three return the same bits, and so does a build compiled without OpenMP at all. That is not a
tolerance — it is exact equality, and it follows from the fixed block tree above: each block is
reduced by the same serial code and the blocks are combined in index order, so who computed which
block is not observable in the result.

With no `threads=`, the count comes from the same rule the sorting family uses — the
`parquet_sort_threads` setting, capped by the processors actually available, and **1 inside a
caller's own parallel region**, so a per-group loop that is already parallel does not nest teams.
There is deliberately no separate statistics thread setting; one question has one answer.

Three things worth knowing before reaching for `threads=`:

- **A small population is reduced serially whatever you ask for.** Opening a team costs more than
  it returns below a measured work floor of a few thousand elements per thread — at a few hundred
  per thread a team is a *loss*, not a small gain — so the library declines one. `threads=8` on a
  short array is not an error and not ignored; it is capped by the work available.
- **Only the central-moment pass is threaded.** The first pass applies the exclusion rules,
  which is inherently sequential where anything is actually excluded (where a survivor lands
  depends on how many earlier ones were removed), so it stays serial. That bounds what threading
  can return on a whole call to well under the thread count.
- **Where the survivors live decides what a team is worth**, and on a large machine it decides it
  more than the thread count does. Pass one is serial, so on entry the population is hot in
  whichever cache the calling thread owns; a team drawn from cores that share that cache reads it
  there, and a team spread wider drags the whole population across the machine first. Measured on
  a 2-socket EPYC (8 cores per L3), the same four-thread team returned 1.13x inside one L3 and
  0.25x spread across eight. If you are pinning threads for other reasons, keeping a team inside
  one cache domain is worth more here than making it bigger.
- **The shape of the call matters more than the thread count**, for anything but a plain array.
  Passing `weights=` roughly doubles the cost of a reduction, and `is_valid=` adds about half
  again, because both leave the fast path. Threading does not recover that.

`bench/benchmark_stats.sh` measures all of this, and its `--mode=thread` refuses to report a
timing until it has confirmed the bit-exactness above on the machine it is running on. Its
`--mode=teamsweep` is the one that answers "where does a team start paying *here*", on the shipped
call rather than on a replica, and it is what the work floor is derived from.

## Order statistics: medians, quantiles and the rest

```fortran
call pf_median(mag, med)                                  ! interpolating
call pf_quantile(mag, 0.9_real64, q90)
call pf_quantiles(mag, [0.16_real64, 0.5_real64, 0.84_real64], q)   ! ONE ordering
call pf_iqr(mag, r)
call pf_trim_mean(mag, 0.1_real64, tm)                    ! middle 80%
call pf_percentile_of_score(mag, 18.5_real64, share)
```

Probabilities are on a **0–1 scale**, never 0–100, matching every other probability in the library.
Anything outside `[0, 1]` aborts.

### `pf_median` is not `pf_nth_quantile`

They answer different questions and both are worth having:

| | `pf_nth_quantile` ([sorting](sorting.html)) | `pf_median` / `pf_quantile` (here) |
|---|---|---|
| element types | all ten, including `character` and dates | numeric only |
| result | **an element of the input**, with `index=` saying which | an interpolated `real(real64)` |
| even-length median | one of the two middle values | their mean |
| all-null input | `ok = .false.`, or aborts when `ok` is omitted; the result must not be read | NaN with `ok = .false.` |
| weights | no | yes |

If you want to know *which row* the median was, use `pf_nth_quantile`. If you want the number a
spreadsheet, numpy or pandas would give, use `pf_median`.

### Choosing a method

`method=` takes numpy's own spellings, so a cross-check is name for name:

| token | rule |
|---|---|
| `"linear"` (default) | linear interpolation between the bracketing order statistics — Hyndman–Fan type 7, numpy's and pandas' default |
| `"lower"` / `"higher"` | the order statistic below / above |
| `"nearest"` | the nearer one; an exact tie goes to the even lower index, as numpy does |
| `"midpoint"` | the mean of the two |
| `"inverted_cdf"` | the smallest value whose cumulative share reaches `p` — a step function, and the only token numpy itself accepts weights with |

Every unweighted method is checked against `np.quantile` at seven probabilities by
`tools/generate_stats_vectors.py --self-test`, so these are not merely *named* after numpy's.

### Weighted quantiles, and the one property to know

A weighted quantile has a definitional choice in it, and this library's rule was chosen for one
property: **at equal weights it reproduces the unweighted answer.** Adding uniform weights to a
working script does not move its numbers.

That matters because no reference library interpolates a weighted quantile at all — numpy accepts
`weights=` only with `method="inverted_cdf"`, and statsmodels and Hmisc are step functions too. So
the rule is this project's own, derived and pinned at 50 digits, and the equal-weight reduction is
what stands in for a cross-check. `method="inverted_cdf"` is the numpy-comparable escape hatch.

Two consequences worth knowing:

- **`q(0)` and `q(1)` are exactly the minimum and the maximum**, weighted or not. No interpolation
  happens at either end, by construction.
- **`weight_type="frequency"` changes the default method to `"inverted_cdf"`.** That token *is*
  frequency expansion: on `x = [1, 2, 3]` with `w = [1, 1, 2]` it agrees at every probability with
  the expanded `[1, 2, 3, 3]`, where the interpolating rule does not. An explicit `method=` still
  wins.

### One ordering, many statistics

This is the reason `pf_stats` exists:

```fortran
call s%compute(mag)
med = s%median()          ! orders the retained values, once
r   = s%iqr()             ! free
q90 = s%quantile(0.9_real64)   ! free
tm  = s%trim_mean(0.1_real64)  ! free
```

The retained values are sorted **in place**, so tier B costs no extra memory — and nothing in the
API ever hands them back in their original order, because they no longer are in it.

Three rules follow, and each is a real constraint rather than a note:

- **Any mutation drops the ordering.** `%update` and `%merge` invalidate it, so the next order
  statistic re-orders. A cached median that survived an update would be stale, and a stale median
  is a perfectly plausible number — which is why the library asserts this with a counter rather
  than by inspection.
- **An order statistic needs `retain = .true.`**, which is `%compute`'s default. A streaming
  accumulator kept no values and aborts with a message saying so.
- **The lazy ordering is not thread-safe on a *shared* accumulator.** Two threads calling
  `%median()` on the same object both find it unordered and both order it. Either give each thread
  its own accumulator, or call `%prepare_order()` before the parallel region — the same escape
  hatch, for the same reason, that `parquet_column%ensure_validity` provides.

`%release_order()` drops the ordering and keeps the moments, for a long-lived summary whose
quantiles have already been taken.

### `pf_trim_mean` and `pf_percentile_of_score`

`pf_trim_mean(values, prop, m)` removes `floor(prop * n_valid)` elements from **each** tail and
averages the rest, matching scipy's `trim_mean` — so the answer changes in steps as `prop` grows,
not continuously. `prop` must satisfy `0 <= prop < 0.5`; anything else aborts.

`pf_percentile_of_score(values, score, p)` is the loose inverse of `pf_quantile`: it answers where a
value sits rather than what value sits somewhere, on a 0–1 scale. `kind=` picks how a value exactly
equal to `score` counts — `"rank"` (the default, scipy's too), `"weak"`, `"strict"` or `"mean"`. A
NaN or infinite `score` **aborts**: unlike a NaN in the population, which is an ordinary data
condition, a NaN score can only come from the caller's own arithmetic.

## `pf_mad` — the median absolute deviation

`pf_mad` is what to reach for when the population may contain a few wild points and `pf_stddev`
would be wrecked by them. It is `median(|x - center|)`, scaled by default so that it estimates the
same quantity `pf_stddev` does for clean Gaussian data:

```fortran
call pf_mad(mag, spread)                    ! scaled: comparable with pf_stddev
call pf_mad(mag, spread, scale="raw")       ! the unscaled median deviation
call pf_mad(mag, spread, center=0.0_real64) ! deviations about a centre you supply
```

`scale="normal"` (the default) divides by `Phi^-1(3/4)`, which is scipy's
`median_abs_deviation(scale='normal')` and what an astronomy script means by "the MAD". Note the
constant: it is a division by `0.6744897501960817`, and **not** a multiplication by the rounded
`1.4826` most textbooks quote — those differ by about 1.5e-06 relative, which is far above the
double-precision noise floor and is enough to fail a comparison against scipy.

Three rules to know:

- **`center=` skips one selection, not the ordering.** The median of `|x - center|` still has to be
  found, so supplying a centre is a small saving rather than a different algorithm. With `center`
  absent the centre is the population's own median.
- **A NaN or infinite `center` aborts.** This is the one place in the module where a NaN is treated
  as misuse rather than as data, and the asymmetry is deliberate: a NaN *value* is an ordinary thing
  to meet in a catalogue, while a NaN *centre* can only come from the caller's own arithmetic.
- **There is no `method=`.** Both medians use linear interpolation, because scipy reaches
  `np.median`; a token that let this disagree with scipy would be worse than no token.

On the object, `%mad([scale], [center])` caches the **raw** deviation with the centre it was taken
about, so rescaling it is free and a repeated default call costs nothing. `%has_deviation()` reports
whether that cache is built; `%update`, `%merge` and `%release_order()` all drop it.

## `pf_mode` — the most common value

```fortran
integer(int32) :: filter_id, m
integer(int64) :: n
logical :: ok

call pf_mode(filter_ids, m, count=n, ok=ok)
```

**Integer, logical and string kinds only.** `integer(int32)`, `integer(int64)`, `logical`,
`character(len=*)` and `parquet_string_column` reach the generic; the real kinds do not, so
`pf_mode` over a real array does not compile. That is not an oversight — the mode of a continuous
variable is not a statistic, and floating-point equality is a trap rather than an answer. Use
`pf_bucketize` or `pf_histogram` for the question a caller reaching for it is usually asking.

- **Ties go to the smallest value**, matching `scipy.stats.mode`, and never to the first occurrence:
  an answer that depended on input order would differ between an array and its own permutation.
- **`count` reports occurrences.** Under weights, the mode is the value carrying the greatest total
  weight, ties still broken by the smallest value — and `count` still counts elements rather than
  weight.
- **Nulls are excluded and counted** through `n_null`, as everywhere. No kind here can hold a NaN,
  so there is deliberately no `skipnan` and no `n_nan`.
- **An empty population gives `ok = .false.` and `count = 0`,** and `m` must not be read. For the
  two character forms `m` is then left *unallocated*, which is the one place in this module where
  `ok = .false.` and an unallocated result coincide.
- **`modes=` gives every tied value**, ascending, which is what pandas' `Series.mode()` returns
  where `m` alone is scipy's. It comes back allocated to exactly the number that tie, so
  `size(modes)` says how many there were and `modes(1)` is always `m`. A population with one clear
  winner gives a one-element array, and an empty one a **zero-length** array rather than an
  unallocated one — so `size(modes)` is always safe to read.

```fortran
integer(int32), allocatable :: all_modes(:)

call pf_mode([1, 1, 2, 2, 3, 3, 4, 5, 5], m, count=n, modes=all_modes)
! m = 1, n = 2, all_modes = [1, 2, 3, 5] -- exactly pandas' Series.mode()
```

For the two character forms `modes` is a `character(len=:), allocatable` array, so every element
shares one length: the **longest** tied value's, with the shorter ones blank-padded. Compare with
`trim()`.

The result has the type of `values`, so the character forms return a `character(len=:), allocatable`
through an `intent(out)` argument. A `character(len=*)` array's answer is **trimmed**, so it comes
back as long as the value rather than as wide as the array was declared.

A `type(parquet_column)` is deliberately **not** accepted. A column's kind is not known until run
time, so such a specific could only be disambiguated by the type of the *result* argument — which
would turn a kind mismatch from a compile error into a runtime abort, and would make `pf_mode` the
one procedure here that does not resolve on `values` alone.

## `pf_describe` and `%print` — the whole summary at once

```fortran
type(pf_stats) :: s

call pf_describe(mag, s)
call s%print(name="mag")
```

`pf_describe` is `%compute` followed by `%prepare_order`: **one pair of traversals and one
ordering**, after which the count, the mean, the standard deviation, the extremes and every quantile
are reads. Its `ok=` is `.false.` for exactly the two populations that leave every tier-A query on
the object a NaN — one that is empty after the exclusions, and one that kept a NaN under
`skipnan = .false.` — so it is the one thing about the result a caller cannot read off the object
itself, which carries the counts but no `%ok()`. `%print` renders the block pandas' `describe()` prints:

```
pf_stats mag
  n         200000
  n_valid   199981
  n_null    12
  n_nan     7
  mean      18.4429...
  stddev    1.2201...
  min       11.7314...
  max       24.9982...
  25%       17.5544...
  50%       18.4381...
  75%       19.3327...
```

- **`n_nan` is always shown**, and that is the mitigation that makes `skipnan = .true.` a safe
  default: a silently skipped NaN is visible in that row rather than nowhere.
- **`%print` never aborts.** On a streaming accumulator (`retain = .false.`) it prints the moment
  rows and one line saying the quartiles are not available; on an object that has not been computed
  at all it prints one line saying so. A printer is what someone reaches for to find out what state
  an object is in, so killing the process would be a poor way to answer.
- **`verbosity = "silent"` silences it entirely**, like every other solicited printer in this
  library. `unit=` overrides the destination; absent, it follows the `message_stream` setting.
  `parquet_stats` re-exports `parquet_set_verbosity` and `parquet_set_message_stream` so that a
  program importing nothing else can reach both.

Reach for `%compute` rather than `pf_describe` when the order statistics are not wanted:
`pf_describe` pays for the ordering whether or not anything asks for it, which is the point of it.

## `pf_gmean` and `pf_hmean` — the other two means

```fortran
call pf_gmean(flux, g)      ! exp(sum(w*log(x)) / sum(w))
call pf_hmean(flux, h)      ! sum(w) / sum(w/x)
```

`scipy.stats.gmean` and `scipy.stats.hmean`. The geometric mean is computed in log space rather than
as a product, because the product of a few thousand fluxes overflows a `real(real64)` and its
logarithm does not.

**Two values outside the domain are answered rather than refused**, matching scipy:

- a value **exactly 0** gives exactly `0` — the limit of both means — with `ok = .true.`, because
  that is a defined answer;
- any **negative** value gives a quiet NaN with `ok = .false.`, since neither mean is defined for a
  sign-changing population. A population holding both a zero and a negative is undefined too, not
  zero.

Both branches are taken **before** the logarithm or the reciprocal is reached, and that is a
correctness requirement rather than tidiness: `log(0)` and `1/0` raise IEEE exceptions, and a
compiler that unmasks the traps would terminate the process rather than return the answer.

## `pf_cov` and `pf_corr` — two samples

```fortran
call pf_cov(mag, redshift, c)                         ! ddof = 1, as pandas
call pf_corr(mag, redshift, r)                        ! Pearson
call pf_corr(mag, redshift, r, method="spearman")     ! Pearson over midranks
```

**Pairwise-complete, and that is the only defensible rule.** A pair enters the population only when
both of its elements are usable: neither null, neither NaN, and the pair's weight non-zero. Handling
nullness independently per array would leave a covariance between vectors of different lengths, which
is not a number. `n_null` and `n_nan` therefore count **pairs** here, not elements.

`is_valid=` is one mask over both samples. A caller holding a separate mask per column passes
`mask_x .and. mask_y`, which says what it does at the call site.

Two identities hold **exactly**, and the test suite asserts them with `==` rather than a tolerance:

- `pf_cov(x, x)` is `pf_variance(x)` bit for bit, at any `ddof` **and under either
  `weight_type`**. That is why the covariance is accumulated by the same block tree the variance
  is, rather than by a loop of its own, and why `pf_cov` takes `weight_type=` at all: without it
  the identity would fail for any caller who asked the variance for frequency weights.
- `pf_corr(x, x)` is exactly `1`, and `pf_corr(x, -x)` exactly `-1`.

**`pf_corr` has neither `ddof` nor `weight_type`** — the `ddof` in the covariance and the two in
the standard deviations cancel exactly, and the two weight conventions differ only in the count
`ddof` is charged against, so neither argument could ever change the answer. Spearman is Pearson over **midranks**: each
run of equal values receives the mean of the sorted positions it spans, so it measures any monotone
relationship rather than a linear one. `weights` with `method="spearman"` **aborts**: a weighted
midrank is a further definitional choice that no reference library makes.

**Both arrays must be the same kind.** Six specifics rather than thirty-six; a caller mixing kinds
writes `real(x, real64)`.

## `pf_zscore` — standardise a whole array

```fortran
call pf_zscore(mag, z)                         ! ddof = 1
call pf_zscore(mag, z, is_valid=mask, out_valid=ok_out, n_null=nn, ok=fine)
```

`z(i) = (values(i) - mean) / stddev` over the elements that survive the exclusion rules. `z` is the
same size as `values`, and the result has mean 0 and standard deviation 1 to a few ulp.

An **excluded** element has no standardised value, and there are two ways to learn which:
`out_valid(:)` marks them exactly, and without it they are written as quiet NaNs — which is loud in
practice, since a NaN propagates through whatever the caller does next.

`ok = .false.` has **two** causes and deliberately does not distinguish them: a null was written out
as a NaN, or the population's variance was zero and *every* output is NaN. `n_null` separates them at
no cost — it is `0` in the second case — so a second flag would be another thing to document and
reset for no new information.

## `pf_sigma_clipped_stats` — the robust summary

```fortran
call pf_sigma_clipped_stats(mag, mean, median, stddev)
call pf_sigma_clipped_stats(mag, mean, median, stddev, sigma=2.5_real64, &
                            stdfunc="mad_std", keep=survivors, n_clipped=nc)
```

astropy's `sigma_clipped_stats`. Each round computes a centre and a scale over the values still
surviving and keeps `centre - sigma_lower*scale <= x <= centre + sigma_upper*scale`, inclusive at
both ends. `sigma = 3.0` and `maxiters = 5` are astropy's defaults; `cenfunc=` is `"median"` (the
default) or `"mean"`, and `stdfunc=` is `"std"` (the default) or `"mad_std"`, which is far less
disturbed by the outliers being clipped.

**A whole run costs one ordering.** The keep condition is an interval, so the survivors of every
round are a contiguous sub-range of the sorted order and each later round only moves two indices
inward — O(n log n + k·n) rather than O(k·n log n). That is asserted with a counter rather than by
inspection, because sorting once per round returns identical numbers and differs only in cost.
(`stdfunc="mad_std"` does order the *deviations* once per round as well: those are a different
population and their order is not implied by the values'.)

Five rules worth knowing:

- **`maxiters <= 0` iterates until a round removes nothing**, which is astropy's `maxiters=None`,
  capped at 100 rounds. Reaching that cap **stops and reports** through `converged = .false.`; it
  does not abort. Each non-terminating round removes at least one point, so a large catalogue can
  honestly want more rounds than any small constant.
- **A round that would leave fewer than two values stops instead**, and the previous round's
  survivors are what is reported.
- **`stddev` is the population standard deviation (`ddof = 0`)**, which is what astropy returns —
  note this differs from `pf_stddev`'s own default of 1.
- **`keep(:)` returns the surviving mask over the original array**, which is what a caller needs in
  order to apply the same clip to a second column.
- **There is no `weights` argument.** A weighted scale estimator is a further definitional choice
  that no workload in evidence asks for, and its absence makes passing one a compile error rather
  than a runtime abort.

An empty population gives three NaNs with `ok = .false.` and `n_clipped = 0`; a constant one is
reported unchanged, converged, with nothing clipped.

## `pf_cumsum`, `pf_cumprod`, `pf_cummax`, `pf_cummin` — the running folds

```fortran
call pf_cumsum(flux, total)                    ! total(i) = sum of flux(1:i)
call pf_cummax(flux, peak, is_valid=mask, out_valid=ok_out, n_null=nn)
```

pandas' `Series.cumsum()`, `cumprod()`, `cummax()` and `cummin()`. `out` is the same size as
`values` and is always `real(real64)`, so a running sum of 64-bit integers cannot silently wrap.

**Their null rule is not the reduction family's, and it is the whole point of the family.** An
excluded element yields an excluded *output* element, and the running value **continues past it
unchanged**:

```
values   1    null   3    4
cumsum   1    ---    4    8
```

The null contributes nothing and is not carried forward — `4` is `1 + 3`, and the scan does not
restart. "The output element is undefined" and "the rest of the output is undefined" are one
careless line apart, and only the first is meant.

An undefined output element is `.false.` in `out_valid(:)` when that mask is present and a quiet
NaN when it is not, exactly as for `pf_zscore`. `ok = .false.` when any element was excluded, and
`n_null`/`n_nan` say why.

**`skipnan = .false.` is the one case where a NaN does poison what follows.** It is then an ordinary
value, so the running sum of a NaN is a NaN and every later element is one too — numpy's `np.cumsum`
and pandas' `skipna=False`. This holds for the running maximum and minimum as well, which is worth
saying because it is not what `max()` would do: comparing a NaN against anything is false, so a
maximum built on comparison alone would silently discard it.

**The last element of a `pf_cumsum` is not `pf_sum(values)`.** A running sum is sequential by
definition; `pf_sum` is pairwise over a fixed block tree. The two agree to within a naive sum's
accumulated rounding, which on a large or badly scaled population is several ulps and occasionally
more. Compare them with a tolerance, and take the total itself from `pf_sum`, which is the more
accurate of the two.

**`out` must not be `values`.** There is no in-place form: passing one array to both arguments
associates it with an `intent(in)` and an `intent(out)` dummy at once, which the standard forbids and
no compiler here diagnoses. The scan would in fact survive it, reading element *i* before writing
element *i* — which is exactly what makes the mistake worth naming rather than leaving to be
discovered.

There is no `weights` argument: a weighted running sum is `pf_cumsum(w*x)`, and the other three
folds have no weighted reading worth choosing between. There is no `threads` argument either — a
prefix scan is sequential by definition, so one could only ever be ignored.

## `pf_bucketize`, `pf_histogram` and `pf_bin_edges` — bins

```fortran
real(real64) :: edges(5) = [0.0_real64, 2.0_real64, 4.0_real64, 6.0_real64, 8.0_real64]
integer(int32) :: codes(size(mag))
real(real64) :: counts(size(edges) - 1)

call pf_bin_edges(mag, 4, edges)                           ! np.histogram_bin_edges(bins=4)
call pf_bucketize(mag, edges, codes)                       ! pd.cut
call pf_histogram(mag, edges, counts)                      ! np.histogram(bins=edges)
call pf_histogram(mag, edges, counts, weights=w, n_outside=nout)
call pf_histogram(mag, edges, counts, density=.true.)      ! np.histogram(density=True)
```

`pf_bucketize` answers, for each value, the **1-based number of the bin it falls in**, or **0** for
one that joined none. `pf_histogram` answers how many values landed in each bin — or the **sum of
their weights** when `weights` is present, which is what `np.histogram(weights=)` returns and why
`counts` is real rather than integer. Unweighted the counts are whole numbers exactly, up to 2⁵³
elements in one bin.

The two are the same operation: `counts(k)` is the **total weight** of the `codes` equal to `k` over
the same arguments, and they share one edge search, so that identity cannot drift. Unweighted, that
total weight is a count. (It holds with `density` absent, naturally — the whole point of `density`
is to stop being a count.)

**That is why `pf_bucketize` takes `weights` too**, although a weight can never change a bin
number. It decides *membership*: a zero weight removes the element from the population, here as
everywhere else in this module, so a bucketize blind to weights would hand that element an ordinary
code while the histogram beside it left the element out — and the identity above would be false for
every weighted call.

```fortran
call pf_histogram(x, edges, counts, weights=w)
call pf_bucketize(x, edges, codes, weights=w)
tally = 0.0_real64
do i = 1, size(x)
    if (codes(i) > 0) tally(codes(i)) = tally(codes(i)) + w(i)
end do
! tally == counts, element for element
```

**`density = .true.`** divides each bin by its own *width* and by the total that was binned, giving
a probability density: `np.histogram(density=True)`, so `sum(counts * widths)` is 1 and an uneven
edge spacing is accounted for. The normalisation base is **what actually landed in a bin** — values
outside the edges were never counted, so they are not in it, which is numpy's rule too. When nothing
was binned there is no density: every entry is a quiet NaN with `ok = .false.` Note that this is one
place a density and a histogram genuinely differ — an empty *histogram* is all zeros with
`ok = .true.` and perfectly well defined.

**Two edge conventions, and they are mirror images.**

| `right=` | bin k covers | which end closes | equivalent |
|---|---|---|---|
| `.false.` (default) | `[edges(k), edges(k+1))` | the **last** bin closes at the top | `np.histogram`, `np.digitize` |
| `.true.` | `(edges(k), edges(k+1)]` | the **first** bin closes at the bottom | `pd.cut(right=True, include_lowest=True)` |

Either way the closed range `[edges(1), edges(nbins+1)]` is covered **exactly once**, so nothing is
lost and nothing is double-counted. The two differ only in which bin a value sitting exactly on an
interior edge joins.

`edges` is always `real(real64)`, whatever kind `values` is: it states a rule rather than carrying
data, and one type for it keeps the generic resolving on `values` alone. It must be **strictly
increasing** with at least two entries, or the call aborts naming the offending index. An **infinite
outer edge is allowed** and is how an open-ended first or last bin is asked for.

Nothing here aborts on a data condition. A value outside the edges joins no bin, is reported through
`n_outside`, and makes `ok = .false.` — numpy drops such values silently, so this is strictly more
information. `n_null`, `n_nan` and `n_outside` separate the three reasons an element reached no bin,
and `codes(i) == 0` says it per element; that is the 1-based spelling of `pd.cut`'s `-1`.

A **NaN reaches no bin under either `skipnan`, and only the accounting differs**: skipped, it is an
exclusion and lands in `n_nan`; kept, it is a value that matches no bin and lands in `n_outside`.
Every comparison against a NaN is false, so "which bin does it join" has one answer, and the argument
only decides whether the population was asked the question at all.

A **zero-weight** element is in none of the three counts. It has left the population, exactly as it
does everywhere else in this module, so it is not an element that failed to reach a bin.

### `pf_bin_edges` — the edges themselves

```fortran
call pf_bin_edges(mag, 10, edges)                 ! 11 boundaries over mag's own range
call pf_bin_edges(mag, 10, edges, is_valid=mask, ok=spanned)
```

The other half of `np.histogram(bins=10)`: it finds the range and lays `nbins + 1` boundaries across
it, ready to hand straight to `pf_histogram` or `pf_bucketize`. The population is the same one they
would bin — nulls, NaNs and zero-weight elements leave it by the usual rules — so a zero-weighted
extreme value does not stretch the range over data that will not be counted.

**The edges are always strictly increasing.** That is a contract, not an observation: they exist to
be passed to `pf_histogram`, which *aborts* on a pair that is not, so a degenerate population must
never produce edges that abort one call later. `ok = .false.` says the edges do not describe the
data's own range, and there are four ways to get it:

| population | edges | why |
|---|---|---|
| empty | `[0, 1]` split `nbins` ways | numpy's own fallback; there is no range to describe |
| constant at `x` | `[x - 0.5, x + 0.5]` split `nbins` ways | also numpy's |
| `nbins` finer than the range resolves | the boundaries nudged apart by one ulp each | the spacing underflowed |
| holds `+Inf`, `-Inf`, or a NaN kept by `skipnan = .false.` | the range of the **finite** values | an infinity cannot be a boundary, and every interior edge would come out infinite |

In all four the edges are still usable, which is what `ok` is *not* saying.

The last row is the one worth reading twice, because it is the one an ordinary data condition
reaches: an upstream division by zero or an overflowed unit conversion puts an `+Inf` in a column,
and edges taken over it would be `[1, Inf, Inf, Inf, Inf]` — not increasing, so `pf_histogram`
would abort one call later on data that is merely unusual. Non-finite values are therefore excluded
from the **range scan** only; they remain in the population everywhere else, and `pf_histogram` puts
them in `n_outside`, since an infinity matches no finite bin. numpy raises `ValueError` on the same
input.

There is deliberately no explicit range pair. A caller who already knows the bounds can write the
`nbins + 1` values directly; what earns a procedure is finding the range under this module's
exclusion rules, which is the part that is easy to get subtly wrong. `nbins` is a plain default-kind
`integer` and has no `int64` form — it is bounded by the size of `edges`, so it can never
legitimately exceed `huge(1_int32)`.

## `pf_stats` — summarise once, query as often as you like

The one-shot procedures above each traverse the population. When a group needs more than one or two
statistics, or when the values arrive in pieces, build a `pf_stats` instead: it does the work once
and every later query is a read.

```fortran
use parquet_stats

type(pf_stats) :: s
real(real64) :: mag(1000)

call s%compute(mag)
print *, s%n_valid(), s%mean(), s%stddev(), s%skewness(), s%vmin(), s%vmax()
```

That whole line costs the same two traversals `s%mean()` alone would.

### Three lifecycles

**`%compute(values, [retain], [is_valid], [weights], [weight_type], [skipnan])`** summarises a
resident array, of any of the six things
[`values` may be](#what-values-may-be). This is the usual entry point, and it discards whatever the object held, so one
`pf_stats` can be reused across a loop of groups without `%clear` in between.

**`%init([retain], [weight_type], [skipnan])` then a loop of `%update(values, [is_valid],
[weights])`** accumulates a population that never exists in memory at once — a row-group loop over
a file larger than RAM. `%init` leaves the object holding an **empty** population, which is not the
same as an uncomputed one: every count answers 0, every moment is NaN, and `%is_computed()` is
`.true.`.

**`%merge(other, [consume])`**, or `%merge(others(:), [consume])` over an array, folds separately
built accumulators together. The array form folds in **index order**, so a threaded accumulation
gives the same answer whatever order the threads finish in. `consume=.true.` clears each source as
it is folded, which is what the threaded shape wants.

```fortran
type(pf_stats) :: part(nthreads), total

!$omp parallel do
do k = 1, nthreads
    call part(k)%compute(chunk_of(k))
end do
!$omp end parallel do
call total%init()
call total%merge(part, consume=.true.)
```

Note the array is declared **before** the parallel region and indexed per thread. That is required
rather than stylistic — see [Thread safety](../operating/thread-safety.html) — because a `pf_stats`
declared inside the region is miscompiled by at least one supported compiler.

### `retain` decides whether the object is exact or merely accurate

| | `retain = .true.` (default) | `retain = .false.` |
|---|---|---|
| memory | 8 bytes per surviving element, 16 when weighted | O(1), whatever the input size |
| `%update` | appends; one traversal, recomputation deferred | one traversal, O(1) memory |
| `%merge` | concatenates; recomputation deferred | combines in O(1) |
| accuracy after `%update`/`%merge` | **exactly** `%compute` over the concatenation | the combination formulas' own |
| order statistics | available | never |

The exactness is worth being concrete about: a retained `%update` loop and a retained `%merge` both
recompute the moments by the same two-pass algorithm over the concatenated survivors, so the result
is bit for bit what `%compute` over the concatenated input gives — not "as close as the merge formula
claims". The recomputation is **deferred until something is asked of the object**, so folding a
hundred partials costs one recomputation rather than a hundred.

Every accumulator in one `%merge` must agree on `retain`, and on `weight_type`; a mismatch aborts.
Merging an accumulator that holds no population aborts too, while merging one that holds an *empty*
population is a no-op — which is what lets a threaded fold run over slots a short loop never filled.

### The queries

`%n`, `%n_valid`, `%n_null` and `%n_nan` return `integer(int64)`; everything else returns
`real(real64)`. `%is_computed` and `%retains` return a `logical`.

| group | bindings |
|---|---|
| counts | `%n()`, `%n_valid()`, `%n_null()`, `%n_nan()`, `%sum_weights()` |
| moments | `%sum()`, `%mean()`, `%variance([ddof])`, `%stddev([ddof])`, `%sem([ddof])`, `%skewness([bias])`, `%kurtosis([bias], [excess])` |
| power means | `%gmean()`, `%hmean()` — **need `retain`**, see below |
| extremes | `%vmin()`, `%vmax()`, `%range()` |
| order | `%median([method])`, `%quantile(p, [method])`, `%quantiles(probs, out, [method])`, `%iqr([method])`, `%trim_mean(prop)`, `%percentile_of_score(score, [kind])` |
| deviation | `%mad([scale], [center])` |
| output | `%print([unit], [name])` |
| state | `%is_computed()`, `%retains()`, `%is_ordered()`, `%has_deviation()`, `%prepare_order()`, `%release_order()`, `%clear()` |

The arguments mean exactly what they mean on the one-shot procedures, including every default in
the table above, and the exclusion rules are the same ones. `weight_type` and `skipnan` are fixed at
`%compute`/`%init` rather than passed per query, because a population cannot be two populations.

Those two, and `retain`, are the **three policies `%merge` checks**: folding one accumulator into
another aborts when they disagree on any of them, naming which. A `skipnan` mismatch is as much a
mistake as the other two — it decides what the population *is*, so a NaN-skipping partial merged
into a NaN-propagating one would produce a number describing neither convention.

`%gmean` and `%hmean` are the two queries that need the *retained values* rather than the
accumulator, so they **abort on a streaming accumulator** (`retain = .false.`), exactly as the
order statistics do — and, unlike those, they order nothing. A log-sum is a fifth quantity the four
central moments do not contain, and accumulating it in the hot loop would charge a transcendental
per element to every population that never asks for one. Both answer exactly what the one-shot
`pf_gmean`/`pf_hmean` would over the same population, including the domain rules: NaN for a
negative value anywhere, and exactly `0` when any value is `0`.

A query may complete a deferred recomputation, so each takes the object as `intent(inout)`: a
`pf_stats` passed as `intent(in)` cannot be queried. **Reading a statistic off an object that was
never `%compute`d or `%init`ed aborts** — that is a forgotten call, not a data condition, and
answering NaN would hide it.

## What "in the population" means, and why it is worth reading once

Every reduction in this module removes the same three classes of element, in the same order, and
`pf_count_valid` counts exactly what is left. Knowing the rule once means never having to check it
per procedure.

- **A null is excluded.** `is_valid(i)` being `.false.` marks element `i` as null, and an absent
  `is_valid` means there are no nulls. This matches pandas, and it matches `pf_nth_quantile`, which
  has always excluded nulls from the population it works over.
- **A NaN is excluded too**, by default, on the kinds that can hold one. This is the same choice
  `pf_minmax` has always made, so two reductions in one program agree about what the population is.
  It is *not* what plain `numpy.mean` does — numpy propagates a NaN and offers `nanmean` separately
  — so `skipnan=.false.` is there for a caller who wants that behaviour.
- **A zero weight removes the element.** `weights` is a `real(real64)` array, one per element, and
  weight zero is how a caller says "leave this row out". A consequence worth stating: a weighted
  count and an unweighted one over the same array can legitimately differ, and neither is wrong.

A NaN therefore does the **opposite** here of what it does in a read filter, and deliberately so. A
filter evaluates IEEE comparisons, where a NaN is an ordinary value that compares false against
everything; a reduction asks what the sample is, where a NaN is not a measurement. See
[Filtering, sorting and sampling](../io/filter-sort-sample.html) for the filter side.

**The order matters and is fixed: nullness, then NaN, then weight.** An element that is already out
of the population never has its weight looked at. That is not a detail — a weight column computed as
`1/err**2` is routinely NaN or infinite exactly where the value column is null, and a program doing
nothing wrong would otherwise fail on the first such row.

## What aborts, and what does not

**This module aborts on misuse and never on a data condition.** A per-group loop meets an empty
group, an all-null column and a group whose weights are all zero on real data; none of those is a
programming error, so none of them stops the program. An empty population simply counts `0`.

What does abort is a call that cannot be honoured:

- `is_valid` or `weights` whose size does not match `values`;
- a weight that is negative, NaN or infinite — a weight can be zero, but none of those three can be
  meant, and failing at the weight is far more useful than a NaN appearing three steps downstream;
- an unrecognised token: a `method=`, a `kind=` or a `scale=` this module does not know, each
  aborting with the accepted set named;
- a probability outside `[0, 1]`, a `prop` outside `[0, 0.5)`, or a `probs`/`out` size mismatch;
- a NaN or infinite `score` or `center` — the two arguments a caller computes rather than measures;
- an order statistic asked of a `retain = .false.` accumulator, which kept no values to order;
- `is_valid=` passed beside a `parquet_column` or a `parquet_string_column`, both of which carry
  their own validity: two sources of truth that can disagree is not something to resolve silently;
- an `out`, `out_valid`, `codes` or `counts` array of the wrong size — note `counts` holds one
  entry per **bin**, so one fewer than the number of edges;
- an `edges` array holding fewer than two entries, a NaN, or a pair that is not strictly
  increasing. A value *outside* the edges is a data condition and is reported, not an abort;
- an `nbins` below 1, or an `edges` array that is not exactly `nbins + 1` long, on `pf_bin_edges`.

## Optional arguments are in one fixed order

Every procedure in this module declares its optional arguments as a subsequence of one canonical
sequence, so a signature you have seen once you have seen everywhere:

```
is_valid, weights, weight_type, ddof, bias, excess, skipnan,
method, kind, scale, center, out_valid, n_null, n_nan, n_outside, ok, threads
```

Three short blocks sit either side of it and are part of the same sequence: an *output* prefix,
which `pf_moments` and `pf_mode` declare before the inputs (`pf_mode`'s `count` is one of these);
the rule block `sigma, sigma_lower, sigma_upper, maxiters, cenfunc, stdfunc, n_clipped, keep,
converged, right, density`, whose entries say what the operation IS and are taken by
`pf_sigma_clipped_stats` and the binning pair; and the `unit`/`name` pair, which only `%print`
takes. A block used by one procedure is not a contradiction — every other procedure omits
it, and omission is exactly what a subsequence permits.

A procedure omits the ones it has no use for and never reorders the rest. In Fortran the order of
optional arguments is part of the public contract — a caller may pass them positionally — so this is
a compatibility promise, not a style preference, and a lint check enforces it against the source.

## Related operations that live elsewhere

Several things a reader arrives at this page looking for are reductions over an array, and are in
`parquet_sorting` rather than here — because they answer *which element* rather than *what value*,
which is the line between the two modules. `use parquet_stats` already brings that module in, so
none of these needs a second import:

| you want | reach for | notes |
|---|---|---|
| the smallest and largest value | `pf_minmax` | one pass, every element type; also `pf_argminmax` for their positions |
| the rank of every element | `pf_rank` | ties by `"min"`, `"max"`, `"average"`, `"dense"` or `"ordinal"`; this is what `pf_corr(method="spearman")` uses internally |
| the distinct values | `pf_unique` | with `counts=` for a value-count table |
| an ELEMENT at a given quantile | `pf_nth_quantile` | returns a member of the input, with `index=` saying which — see [`pf_median` is not `pf_nth_quantile`](#pf_median-is-not-pf_nth_quantile) |
| the k-th smallest element | `pf_nth_element` | O(n) selection, no full sort |
| a sorted copy, or the permutation | `pf_sort` / `pf_argsort` | the ordering every quantile here is built on |

They are documented on [Sorting, ranking and selection](sorting.html). **Their `ok` argument means
the opposite of this module's**: omitting it there makes a degenerate population *abort*, where
omitting it here is the ordinary way to call and a degenerate population answers a quiet NaN. That
difference is deliberate — an order statistic that has no element to return has nothing to answer
with — but it is the one thing to check when moving a call between the two.

## What it costs to import

`use parquet_stats` compiles this module plus `parquet_sorting`'s files, and reaches no C++ at all.
The statistics that need an order take it from `pf_argsort` and `pf_nth_element` rather than
carrying a second sorting implementation. See [Choosing a module](../operating/choosing-a-module.html)
for the measured figure and for what every other import costs.
