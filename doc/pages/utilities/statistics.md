---
title: Array statistics with the pf_ reduction family
---

`parquet_stats` summarises plain Fortran arrays. It has no reader state, no writer state and no
Arrow underneath it: a program that already has an array in hand can `use parquet_stats` and reduce
it, without compiling the C++ wrapper's dependencies into its build. `use parquet` brings it in too,
so nothing here needs a second import.

**This page describes what is implemented today, which is the module's foundation rather than its
whole surface.** The reduction family it is being built for — means, variances, quantiles, robust
estimators — arrives in stages; what is here now is the one procedure that fixes the conventions
every later one inherits.

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
