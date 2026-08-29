---
title: Array statistics with the pf_ reduction family
---

`parquet_stats` summarises plain Fortran arrays. It has no reader state, no writer state and no
Arrow underneath it: a program that already has an array in hand can `use parquet_stats` and reduce
it, without compiling the C++ wrapper's dependencies into its build. `use parquet` brings it in too,
so nothing here needs a second import.

**This page describes what is implemented today, which is the module's foundation rather than its
whole surface.** The reduction family it is being built for arrives in stages: the moment family is
here, the order statistics — quantiles, the median absolute deviation, the robust estimators — are
not yet.

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
```

Square brackets in a signature below mark an optional argument.

**`pf_count_valid(values, n, [is_valid], [weights], [skipnan])`** answers pandas' `Series.count()`:
how many elements of `values` are in the population. `n` is an `integer(int64)`.

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

The one-shot forms are `pf_sum`, `pf_mean`, `pf_variance`, `pf_stddev`, `pf_sem`, `pf_skewness` and
`pf_kurtosis`. Each is that same engine with one output, so they agree with `pf_moments` bit for bit.

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

The empty sum is `0` rather than NaN because that is the additive identity and what numpy and pandas
both return; `n_valid` sits beside it, so nothing is hidden by it.

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
  it returns below a measured work floor of tens of thousands of elements per thread — at a few
  thousand elements a team of eight is a *loss*, not a small gain — so the library declines one.
  `threads=8` on a short array is not an error and not ignored; it is capped by the work available.
- **Only the central-moment pass is threaded.** The first pass applies the exclusion rules and
  compacts the survivors, which is inherently sequential (where an element lands depends on how
  many earlier ones were removed), so it stays serial. That bounds what threading can return on a
  whole call to well under the thread count.
- **The shape of the call matters more than the thread count**, for anything but a plain array.
  Passing `weights=` roughly doubles the cost of a reduction, and `is_valid=` adds about half
  again, because both leave the fast path. Threading does not recover that.

`bench/benchmark_stats.sh` measures all of this, and its `--mode=thread` refuses to report a
timing until it has confirmed the bit-exactness above on the machine it is running on.

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
| all-null input | aborts | NaN with `ok = .false.` |
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
| order statistics (a later release) | available | never |

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
| extremes | `%vmin()`, `%vmax()`, `%range()` |
| state | `%is_computed()`, `%retains()`, `%clear()` |

The arguments mean exactly what they mean on the one-shot procedures, including every default in
the table above, and the exclusion rules are the same ones. `weight_type` and `skipnan` are fixed at
`%compute`/`%init` rather than passed per query, because a population cannot be two populations.

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
  meant, and failing at the weight is far more useful than a NaN appearing three steps downstream.

## Optional arguments are in one fixed order

Every procedure in this module declares its optional arguments as a subsequence of one canonical
sequence, so a signature you have seen once you have seen everywhere:

```
is_valid, weights, weight_type, ddof, bias, excess, skipnan,
method, scale, center, out_valid, converged, n_null, n_nan, ok, threads
```

A procedure omits the ones it has no use for and never reorders the rest. In Fortran the order of
optional arguments is part of the public contract — a caller may pass them positionally — so this is
a compatibility promise, not a style preference, and a lint check enforces it against the source.

## What it costs to import

`use parquet_stats` compiles this module plus `parquet_sorting`'s files, and reaches no C++ at all.
The statistics that need an order take it from `pf_argsort` and `pf_nth_element` rather than
carrying a second sorting implementation. See [Choosing a module](../operating/choosing-a-module.html)
for the measured figure and for what every other import costs.
