---
title: Sorting arrays and columns
---

`parquet_sorting` sorts plain Fortran arrays and this library's own column types. It is the same
C++ engine that orders a read-time `parquet_open_reader(..., sort_by=)` and a
`parquet_table%sort_by` — sharing it is the point, because a read-time sort, a table sort and a
raw-array sort can then never disagree about where nulls go, where NaNs go, or how ties are
broken.

Everything here is reachable from `use parquet`.

## Naming: `pf_`, not `parquet_`

Public names in this module carry the **`pf_`** prefix (for parquet-fortran, the library as a
whole) rather than `parquet_`, because their subject is not a parquet file at all — `pf_sort` will
happily sort an array that never came from one. The module itself is `parquet_sorting` rather than
`parquet_sort` because Fortran does not allow a module to share its name with a procedure it
declares.

## The operations

| Call | Does |
|---|---|
| `pf_argsort(values, perm)` | the permutation that *would* sort `values`; never modifies it |
| `pf_sort(values, sorted)` | an independent sorted copy; never modifies its input |
| `pf_permute(values, perm)` | applies a permutation to `values` **in place** |
| `pf_is_sorted(values, answer)` | whether `values` is already in the stated order |
| `pf_partial_sort(values, sorted, n)` | the first `n` in order, without sorting the rest |
| `pf_partial_argsort(values, perm, n)` | their indices instead |
| `pf_nth_element(values, nth, p_value, [index])` | the value a full sort puts at rank `nth` |
| `pf_nth_quantile(values, quantile, p_value, [index])` | a quantile of the non-null values |

The first four are covered immediately below; the selection operations have their own section,
[Selecting without sorting](#selecting-without-sorting).

Optional arguments are shown in square brackets below — they are optional at the call site, not
part of the syntax.

```fortran
use parquet
integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
integer(int32), allocatable :: perm(:), sorted(:)
logical :: ok

call pf_argsort(v, perm)        ! perm = [2, 4, 1, 6, 3, 5]; v unchanged
call pf_sort(v, sorted)         ! sorted = [10, 20, 30, 40, 50, 60]; v unchanged
call pf_permute(v, perm)        ! v is now [10, 20, 30, 40, 50, 60]
call pf_is_sorted(v, ok)        ! ok = .true.
```

`pf_argsort` is the one to reach for when several arrays have to stay in step: sort one, then
apply the same permutation to the rest.

```fortran
call pf_argsort(ra, perm)
call pf_permute(ra, perm)
call pf_permute(dec, perm)
call pf_permute(name, perm)
```

## Supported types

Eleven element types, in three groups.

| Type | `is_valid=` | `pf_sort`/`_partial_sort` | `pf_nth_*` | Notes |
|---|---|---|---|---|
| `integer(int32)`, `integer(int64)` | yes | yes | yes | |
| `real(real32)`, `real(real64)` | yes | yes | yes | NaNs get their own tier, see below |
| `logical` | yes | yes | yes | `.false.` before `.true.` |
| `character(len=*)` | yes | yes | yes | compared over the full declared length |
| `parquet_date`, `parquet_time` | no | yes | yes | carries its own null state |
| `parquet_timestamp` | no | yes | yes | ordered by seconds, then nanoseconds |
| `parquet_string_column` | no | no | yes | `pf_permute` delegates to `%reindex` |
| `parquet_column` | no | no | no | element kind resolved at runtime |

`pf_argsort`, `pf_partial_argsort`, `pf_permute` and `pf_is_sorted` apply to **all eleven**: their
answer is a permutation, a boolean or an index, never a value, so a runtime element type is no
obstacle.

The `is_valid=` column says where nullness comes from. The six types with no null state of their
own take an **optional** `is_valid(:)` mask; the temporal types and the two column types carry
their own and take no such argument, which keeps a single source of truth for whether an element
is null.

`pf_sort` is deliberately not defined for the two container types: copying a whole column in order
to sort it serves no purpose, and reordering one in place is `pf_argsort` followed by `pf_permute`,
which says what it does at the call site.

`character(len=*)` is compared over its **full declared length, trailing blanks included** — which
is exactly how Fortran's own `<` compares two equal-length strings, so `pf_is_sorted` agrees with a
hand-written `a(k) <= a(k+1)` loop rather than quietly trimming behind it.

## Ordering rules

The ordering reproduces `arrow::compute::SortIndices` exactly, which is what lets a `pyarrow`
cross-check of a sorted file agree row for row.

**Nulls and NaNs sit in absolute tiers.** `descending` reverses the *values* and never the tiers:

```
ascending (default):   values …  NaNs  nulls
nulls_first=.true.:    nulls  NaNs  … values
descending=.true.:     values (reversed) …  NaNs  nulls
```

A NaN is an ordinary value that happens to be unordered, so it is tiered rather than treated as
missing; a null is missing. The two are separate on purpose, and a NaN is *not* a null.

**Every sort here is stable, unconditionally.** Ties keep their original input order, so there is
no `stable=` argument to pass and no unstable mode to opt into. Stability comes from the
comparator's tiebreaker on the row index, and the integer counting fast path places in order by
construction.

```fortran
call pf_argsort(v, perm, descending=.true.)
call pf_argsort(v, perm, is_valid=mask, nulls_first=.true.)
call pf_is_sorted(v, ok, descending=.true.)   ! same options as the sort
```

`pf_is_sorted` takes the same `descending`/`nulls_first` options for a reason: an ordering question
is only meaningful against a stated order. It compares adjacent elements only, with no index
tiebreaker, so a run of equal values counts as sorted.

**A null-free array should pass no mask at all.** Passing an unallocated allocatable to an optional
dummy makes that dummy absent (F2018 15.5.2.12), so a caller whose mask is simply not allocated
gets the engine's no-nulls fast path with no `if (present(...))` fork of their own:

```fortran
logical, allocatable :: mask(:)     ! left unallocated when nothing is null
call pf_argsort(v, perm, is_valid=mask)   ! reaches the engine as "no mask"
```

## Sorting on more than one key

Fortran generics cannot express "an optional second and third array, each of any type" — with
eleven element types that would need over a thousand specific procedures. Multi-key sorting
therefore goes through `pf_sort_keys`, which accumulates keys of any mix of types:

```fortran
type(pf_sort_keys) :: k
integer(int64), allocatable :: perm(:)

call k%add(ra)                        ! primary key
call k%add(dec, descending=.true.)    ! breaks ties on ra
call k%add(name)                      ! breaks ties on both
call pf_argsort(k, perm)
```

Keys apply in the order added, the first being primary, and each carries its own `descending=` and
`nulls_first=`. `%nkeys_added()` reports how many there are; `%clear()` drops them all, leaving the
object reusable for an unrelated sort. Every key must describe the same number of rows, which is
checked as each one is added rather than at sort time.

`pf_sort_keys` holds no C handle — its keys are ordinary allocatable Fortran arrays, and the C++
side is created, used and freed entirely inside `pf_argsort`. That keeps the type free of a
finalizer, which in turn makes it usable per-thread in the obvious way (a finalizable type must
never be given to OpenMP's `private()` — see [Thread safety](thread-safety.html)).

## Validating a permutation

`pf_permute` **validates `perm` before writing anything**, because an invalid permutation does not
fail — it silently duplicates some elements and drops others, and the array afterwards looks
perfectly ordinary. The check is O(N) in front of an O(N) operation, so it is a constant factor
rather than a change of complexity.

```fortran
call pf_permute(v, perm)                      ! validated (the default)
call pf_permute(v, perm, assume_valid=.true.) ! skipped: perm came from pf_argsort
```

Use `assume_valid=.true.` only for a permutation you know is good — one that came straight from
`pf_argsort` is the intended case. Note the polarity: this library's `check_*` arguments (such as
`check_null` on `parquet_string_column`) are opt-in checks defaulting `.false.`, so a
`check_valid=` defaulting `.true.` would read backwards. `assume_valid` states the caller's claim
instead, and defaults to `.false.` so that forgetting it is safe.

`pf_permute` over `parquet_column` and `parquet_string_column` **ignores `assume_valid`**: those
delegate to `%reindex`, which validates unconditionally.

## Sorting a table

To reorder a whole `parquet_table`, use its own `%sort_by`, which reorders **every column
together**:

```fortran
call t%sort_by(["mass"], descending=[.true.])
```

That call runs on this same engine, so it orders rows exactly as `pf_argsort` would on the same
key values.

> **Do not `pf_permute` a pointer obtained from `%col`.** `%col` hands back a *writable pointer
> into a table's live storage*, so permuting through it reorders that one column and leaves every
> other column exactly where it was — silently breaking the row correspondence that makes the table
> a table. Nothing detects this: the table still reports the same row count, and every subsequent
> read returns values that are individually valid and jointly wrong. `%sort_by` is the supported
> way to reorder a table.

Sorting a **standalone** `parquet_column` — one you built yourself, or copied out of a table — is
perfectly safe, and is what the `parquet_column` row of the type table above is for.

## Selecting without sorting

Three operations answer "which element ends up here?" without ordering the whole array.

```fortran
call pf_partial_sort(v, top, n=10)        ! the 10 smallest, in order
call pf_partial_argsort(v, perm, n=10)    ! their indices instead
call pf_nth_element(v, 5, val [, index])  ! the value a full sort puts at rank 5
call pf_nth_quantile(v, 0.5d0, med)       ! the median of the non-null values
```

**`n` is clamped, not checked.** `pf_partial_sort(v, top, n=1000)` on a 100-element array returns
all 100, in order — so an `n` derived from a fraction, a config value or a post-filter row count
needs no `min(n, size(v))` of your own. A *negative* `n` is still an error.

**"The last N" is `descending=.true.`**, not a separate procedure:

```fortran
call pf_partial_sort(flux, brightest, n=10, descending=.true.)
```

**The complexity claim, with its caveat.** `pf_partial_sort` is `O(n log k)` for `k` results and
`pf_nth_element` is `O(n)`, against `O(n log n)` for a full sort — but `pf_partial_sort` stops
paying as `k` approaches the array size, and at `k = size` it is strictly *worse* than calling
`pf_sort`. Use it when you want a small slice of a large array; use `pf_sort` when you want most of
it. (One case gets both for free: a low-cardinality integer key takes a counting-sort path that is
already `O(n)` and already fully ordered, so partial and full cost the same there.)

### The index `pf_nth_element` reports

`index` is optional, and the value it reports is **the index a full *stable* sort would give** —
not merely *an* element equal to the answer. That matters whenever the array has duplicates:

```fortran
integer(int32) :: v(6) = [5, 3, 5, 1, 5, 3]
call pf_nth_element(v, 3, val, index)   ! val = 3, index = 6
```

Ranks 2 and 3 both hold the value `3`; rank 3 is the *second* of them, so it reports the later
original position. `std::nth_element` on its own leaves an arbitrary member of an equal run at that
position, which would make the answer vary between builds — the ordering here ends with a
tiebreaker on the original index, so it does not.

`nth` counts nulls as ranked elements, placed by the same tier rules as the sort, and
`descending`/`nulls_first`/`is_valid` mean exactly what they do for `pf_argsort`.

### Quantiles

`pf_nth_quantile`'s `quantile` argument is on a **0–1 scale, not 0–100**. Passing `50` aborts
rather than silently answering, because `0.5` is valid on both scales and means completely
different things.

```fortran
call pf_nth_quantile(flux, 0.5d0, median)
call pf_nth_quantile(flux, 0.9d0, p90, is_valid=mask, n_null=nmissing)
```

**Nulls are excluded from the population, not placed in it** — the one operation in this module
where that is true, and the reason it takes neither `descending` nor `nulls_first`: there is no
null tier to position, and a descending quantile is just `1 - quantile`. `n_null` reports how many
values were dropped, so you can decide whether the answer is trustworthy.

**An all-null array aborts.** There is no value to return and no sentinel that works across all ten
supported types, so returning an undefined `p_value` would hand back something that looks like data.
`n_null` is for *partial* nullness; guard with `count(mask)` (or a column's own null count) if the
all-null case can happen.

A fractional position is resolved by `rounding=`, matched case-insensitively:

```fortran
call pf_nth_quantile(v, 0.5d0, q, rounding="down")   ! "nearest" (default), "down", "up"
```

An unrecognized token aborts and names the valid ones.

### Sorted validity

`pf_sort` and `pf_partial_sort` take an optional `sorted_valid=` reporting which *output* elements
are null — the mask you passed in describes the input order, which is not the order you get back:

```fortran
call pf_sort(v, sorted, is_valid=mask, sorted_valid=out_mask)   ! out_mask(k) describes sorted(k)
```

It is **always allocated** when you ask for it, including when you passed no `is_valid` at all (in
which case it is all `.true.`). That differs from the "unallocated means no nulls" convention used
for *inputs*, deliberately: an input you leave unallocated is you declining to supply information,
while an output you explicitly asked for is a direct question.

## What is not here yet

Binary search (`pf_lower_bound`/`pf_upper_bound`/`pf_equal_range`), `pf_merge`, `pf_unique` and
`pf_rank` are planned but not implemented. The `assume_sorted` argument described for them does not
exist yet either.

`pf_partial_sort` and `pf_partial_argsort` are not defined for `parquet_string_column` or
`parquet_column`, for the same reason `pf_sort` is not. `pf_nth_element` and `pf_nth_quantile` are
not defined for `parquet_column`: its element type is a runtime discriminator, so there is no
compile-time type for the value they return.
