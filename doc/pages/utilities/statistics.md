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

Today the arrays must be `real(real64)` for everything but `pf_count_valid`; the other numeric kinds
join the same generics later, which is source-compatible for every call written now.

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
how many elements of `values` are in the population. `n` is an `integer(int64)`. `values` may be
`integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` or `logical`.

`skipnan` is offered only on the two real kinds. An integer or logical array has no NaN to skip, and
an argument that can never do anything is worse than an absent one.

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

A second consequence of the fixed block tree is worth knowing in advance: **the answer will not
depend on the thread count** when threading arrives, because the decomposition is a function of the
population size alone and never of who walks it.

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
