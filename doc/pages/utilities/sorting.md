---
title: Sorting arrays and columns with pf_sort/pf_argsort
---

`parquet_sorting` sorts plain Fortran arrays and this library's own column types. It is also the
engine behind every other ordering the library performs: a read-time
`parquet_open_reader(..., sort_by=)`, a post-open `parquet_reader_set_sort` and
`parquet_table%sort_by` all go through it. So a read-time sort, a table sort and a raw-array sort
cannot disagree about where nulls go, where NaNs go, or how ties are broken — not because three
implementations are kept in step, but because there is only one, and it is this one.

Everything here is reachable from `use parquet`, and from `use parquet_sorting` on its own. A
smaller import covering only `pf_argsort` over the intrinsic types is described in
[The lighter import: `parquet_argsort`](#the-lighter-import-parquet_argsort) below.

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
| `pf_lower_bound(values, target, pos)` | where `target` belongs in an already-sorted array |
| `pf_upper_bound(values, target, pos)` | one past the last element equal to `target` |
| `pf_equal_range(values, target, first, last)` | both bounds, as an inclusive range |
| `pf_unique_count(values, count, [n_null])` | how many distinct non-null values there are |
| `pf_unique(values, distinct, [n_null])` | those values themselves, in order |
| `pf_rank(values, ranks, [method])` | the rank of every element, in place order |
| `pf_minmax(values, vmin, vmax)` | the smallest and largest value |
| `pf_argminmax(values, imin, imax)` | where those two are |
| `pf_merge(a, b, merged)` | merges two already-sorted arrays in linear time |

The first four are covered immediately below; the rest have their own sections —
[Selecting without sorting](#selecting-without-sorting),
[Searching a sorted array](#searching-a-sorted-array),
[Distinct values and ranks](#distinct-values-and-ranks),
[Extremes](#extremes) and
[Merging two sorted arrays](#merging-two-sorted-arrays).

Optional arguments are shown in square brackets — in the table above, and in prose throughout this
page. They are optional at the call site, not part of the syntax, so they never appear that way in a
runnable example.

**Every index-valued result above is generic over both integer kinds.** `perm`, `ranks`, `index`,
`pos`, `first`/`last`, `count` and `imin`/`imax` may each be declared `integer(int32)` or
`integer(int64)`, and the answers are identical either way — declare whichever suits the code around
the call. The one case where it matters is scale: an array of more than `huge(int32)` elements
cannot be addressed by an `int32` result, and asking for one aborts rather than wrapping to a
plausible wrong index. `group_offsets` reaches that ceiling one row sooner than `perm` does, because
its last entry is the sentinel `n + 1`.

```fortran
program sort_quickstart
    use parquet
    use iso_fortran_env, only: int32
    implicit none
    integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
    integer(int32), allocatable :: perm(:), sorted(:)
    logical :: ok

    call pf_argsort(v, perm)        ! perm = [2, 4, 1, 6, 3, 5]; v unchanged
    call pf_sort(v, sorted)         ! sorted = [10, 20, 30, 40, 50, 60]; v unchanged
    call pf_permute(v, perm)        ! v is now [10, 20, 30, 40, 50, 60]
    call pf_is_sorted(v, ok)        ! ok = .true.
end program sort_quickstart
```

The kind names (`int32`, `int64`, `real64`) come from `iso_fortran_env`, not from `parquet` — the
later examples on this page are fragments and leave both `use` lines out.

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
| `parquet_string_column` | no | no | yes | `pf_permute` reorders the store in place |
| `parquet_column` | no | no | no | element kind resolved at runtime |

`pf_argsort`, `pf_partial_argsort`, `pf_permute` and `pf_is_sorted` apply to **all eleven**: their
answer is a permutation, a boolean or an index, never a value, so a runtime element type is no
obstacle.

**A `parquet_column` has two restrictions the others do not**, both settled when the sort reads the
column rather than at compile time. It must be a **scalar** column: a vector column — one whose
`%colwidth()` is more than 1 — is refused, because a whole vector row has no defined order. And its
element kind must be one the engine can order: the nine scalar kinds `int32`, `int64`, `float32`,
`float64`, `logical`, `string`, `date`, `time` and `timestamp`. Anything else aborts and names the
kind it was given.

The remaining operations follow the same two rules — a type is out wherever the answer would need
a compile-time element type it does not have, and stays in wherever the answer is a permutation, a
boolean or an integer:

| Type | search | `pf_unique_count` | `pf_unique` | `pf_rank` | `pf_minmax` | `pf_argminmax` | `pf_merge` |
|---|---|---|---|---|---|---|---|
| `integer(int32/int64)`, `real(real32/real64)` | yes | yes | yes | yes | yes | yes | yes |
| `logical` | yes | yes | yes | yes | no | no | yes |
| `character(len=*)` | yes | yes | yes | yes | yes | yes | yes |
| `parquet_date`, `parquet_time`, `parquet_timestamp` | yes | yes | yes | yes | yes | yes | yes |
| `parquet_string_column` | yes | yes | yes | yes | yes | yes | no |
| `parquet_column` | no | yes | no | yes | no | yes | no |

`logical` is left out of `pf_minmax`/`pf_argminmax` as vacuous — "where is the first `.false.`" is
not a question worth an API, and letting one of the pair accept it while the other could not would
be worse than either. `pf_merge` is defined on plain arrays only: merging two columns is
`%append_column` followed by a sort.

The `is_valid=` column says where nullness comes from. The six types with no null state of their
own take an **optional** `is_valid(:)` mask; the temporal types and the two column types carry
their own and take no such argument, which keeps a single source of truth for whether an element
is null. When you do pass a mask it must have exactly as many elements as `values`; a length
mismatch aborts rather than being padded or truncated.

`pf_sort` is deliberately not defined for the two container types: copying a whole column in order
to sort it serves no purpose, and reordering one in place is `pf_argsort` followed by `pf_permute`,
which says what it does at the call site.

`character(len=*)` is compared over its **full declared length, trailing blanks included** — which
is exactly how Fortran's own `<` compares two equal-length strings, so `pf_is_sorted` agrees with a
hand-written `a(k) <= a(k+1)` loop rather than quietly trimming behind it.

## The lighter import: `parquet_argsort`

`pf_argsort` over the six **intrinsic** element types — `integer(int32/int64)`,
`real(real32/real64)`, `logical` and `character(len=*)` — together with `pf_sort_threads` and the
sort engine itself, lives one tier down, in a module of its own:

```fortran
use parquet_argsort
```

That import compiles **4** of this library's Fortran files, against 21 for `use parquet_sorting`,
and its Fortran graph never reaches the Parquet C++ bindings. `use parquet_sorting` and
`use parquet` are unaffected: they re-export the tier and *extend* `pf_argsort` with the five
element types that need a column, a packed string store or a temporal element, so a single
`use parquet_sorting` resolves all eleven. You would never know the split existed unless you went
looking for the smaller import.

**What the smaller import does not include**: `pf_sort`, `pf_permute`, `pf_is_sorted`, the
selection, search, uniqueness, rank, extreme and merge operations, `pf_sort_keys`, and every
element type beyond the six intrinsic ones. Those all stay in `parquet_sorting`. If you need any of
them, import that instead — the answers are identical either way, because there is one engine.

**Why it exists**: fpm prunes at module granularity, so without the split a library wanting one
`pf_argsort` specific would compile the whole sorting graph and link the Parquet C++ stack with it.
Keeping the intrinsic-type argsort in its own tier is what holds `use parquet_sampling` to 8
Fortran files. See
[Choosing a module](../operating/choosing-a-module.html) for the full picture, including the caveat
that no import makes the *package* Arrow-free.

The tier re-exports the four sorting settings — `sort_threads`, `sort_radix_path`,
`sort_counting_path`, `sort_counting_bucket_limit` — plus `verbosity` and `message_stream`, getter
and setter both, so a program importing it alone can still tune and quieten its own sorting.

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
`nulls_first=`. `%nkeys_added()` reports how many there are — one per `%add` call, whatever their
types; `%clear()` drops them all, leaving the object reusable for an unrelated sort. Every key must
describe the same number of rows, which is checked as each one is added rather than at sort time.

A `pf_sort_keys` holding no key at all is an error, not an empty sort: `pf_argsort`, `pf_is_sorted`
and `pf_partial_argsort` each abort on one, and so "reusable" means reusable after the next `%add`,
not immediately after `%clear()`.

A `pf_sort_keys` also works with `pf_is_sorted` and `pf_partial_argsort`, so a multi-key order can
be tested or partially built, not only sorted in full:

```fortran
call pf_is_sorted(k, ok)              ! already in this order? O(n), no allocation
call pf_partial_argsort(k, perm, 10)  ! just the ten best rows
```

## Group boundaries

`pf_argsort` can report, in the same pass, where each run of **equal** rows begins:

```fortran
integer(int64), allocatable :: perm(:), go(:)

call pf_argsort(k, perm, group_offsets=go)
do g = 1, size(go) - 1
    ! rows of group g, in order:  perm(go(g) : go(g+1) - 1)
end do
```

`group_offsets` has length `ngroups + 1`, and its last entry is the sentinel `n + 1` — which is what
lets the loop above slice every group the same way, with no special case for the last one. An empty
array gives `[1]`, meaning no groups.

Two rows share a group when they compare **equal**, which has one consequence worth stating: **all
nulls form one group, and all NaNs form one group.** That is deliberately unlike `pf_unique`, which
drops nulls from its output entirely — a group list has to account for every row.

By default a group is a run equal under *every* key. `group_nkeys=` narrows that to the first few,
**without changing the sort**:

```fortran
call k%add(field_id)
call k%add(mag)
call pf_argsort(k, perm, group_offsets=go, group_nkeys=1)
! one group per field_id, and within each group the rows are ordered by mag
```

`group_nkeys` counts the keys *you* added, one per `%add`. That is not always the engine's own
count — a `parquet_timestamp` key becomes two keys internally — and this argument never exposes the
difference. It must be between 1 and `%nkeys_added()`, and it requires `group_offsets`: on its own
it would change nothing, so passing it alone is an error rather than a silent no-op.

Asking for `group_offsets` costs one extra copy of a single key. Without it a one-key `pf_argsort`
borrows the key it just extracted; with it the call goes through the builder, which owns its keys.
That is the price of one engine entry point serving three operations instead of three of them.

`pf_sort_keys` holds no handle of any kind — its keys are ordinary allocatable Fortran arrays, and
nothing outside the type outlives a call. That keeps it free of a finalizer, which in turn makes it
usable per-thread in the obvious way (a finalizable type must never be given to OpenMP's
`private()` — see [Thread safety](../operating/thread-safety.html)).

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

`assume_valid` means the same thing for all eleven types, the two column ones included — they
reorder their storage without the scan instead of with it.

**It skips the contents check only.** `perm`'s *length* is checked either way, because a short
permutation would make the gather read past the end of `values`, and no promise from the caller can
make that defined. What a false promise costs is the other half: elements silently duplicated and
dropped, with no abort and no symptom.

**"In place" is about the result, not about memory.** `pf_permute` modifies `values` rather than
handing back a reordered copy, but it gathers through a temporary the size of the array and copies
back, so it needs room for a second copy of the column while it runs. Passing an `int32` `perm`
to a `pf_permute` also builds an `int64` copy of the permutation. Neither matters until the array
is large enough for a second copy to be the constraint.

## Sorting a table

To reorder a whole `parquet_table`, use its own `%sort_by`, which reorders **every column
together**:

```fortran
call t%sort_by(["mass"], descending=[.true.])
```

That call runs on this same engine, so it orders rows exactly as `pf_argsort` would on the same
key values. A key column that has not been read yet is read for you.

The table has three read-only counterparts that answer the same questions without moving anything —
`%argsort_by`, `%is_sorted_by` and `%argsort_partial`. Because they do not reorder, they do not
detach the table from its file, which `%sort_by` does. See
[Ordering rows without reordering them](../tables/table-mutate.html#ordering-rows-without-reordering-them).

`%top_n` is the mutating counterpart of `%argsort_partial`, and stands in the same relation to
`%sort_by` as `pf_partial_argsort` does to `pf_argsort`: it keeps only the `n` rows the keys put
first, selecting rather than ordering everything. See
[Keeping only the best rows](../tables/table-mutate.html#keeping-only-the-best-rows).

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
call pf_nth_element(v, 5, val)            ! the value a full sort puts at rank 5
call pf_nth_quantile(v, 0.5_real64, med)   ! the median of the non-null values
```

**`n` is clamped, not checked.** `pf_partial_sort(v, top, n=1000)` on a 100-element array returns
all 100, in order — so an `n` derived from a fraction, a config value or a post-filter row count
needs no `min(n, size(v))` of your own. A *negative* `n` is still an error.

**`pf_nth_element`'s `nth` is the opposite: it is checked, not clamped.** A rank outside
`1 .. size(values)` aborts rather than being pulled to the nearest end. The two arguments sit one
line apart and behave oppositely on purpose — asking for "the best 1000 of 100" is a reasonable
thing to mean, while asking for "the 1000th of 100" is not a request that has an answer. Clamp
`nth` yourself if it comes from arithmetic that can overshoot.

The four selection operations take `threads=` too; what they do and do not thread is set out under
[What `threads=` reaches in a selection](#what-threads-reaches-in-a-selection).

**"The last N" is `descending=.true.`**, not a separate procedure:

```fortran
call pf_partial_sort(flux, brightest, n=10, descending=.true.)
```

On a whole table the same operation is `parquet_table%top_n`, which keeps the `n` best rows of every
column together — see [Keeping only the best rows](../tables/table-mutate.html#keeping-only-the-best-rows).

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
call pf_nth_quantile(flux, 0.5_real64, median)
call pf_nth_quantile(flux, 0.9_real64, p90, is_valid=mask, n_null=nmissing)
call pf_nth_quantile(flux, 0.5_real64, median, index)   ! and where that value came from
```

`index` is optional here exactly as it is on `pf_nth_element`, and reports the same thing under the
same rule: the element a full **stable** sort would have put at that position, so a value occurring
several times reports the right one of them rather than an arbitrary one. The only difference is
which population the position is taken over — the non-null values, since that is what a quantile of
this array means.

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
call pf_nth_quantile(v, 0.5_real64, q, rounding="down")   ! "nearest" (default), "down", "up"
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

## Searching a sorted array

`pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` locate a value in an array that is
**already sorted**. All three answer with 1-based positions in `1 .. size(values)+1`, so the
answer is an *insertion point*: where the target belongs, whether or not it is there.

```fortran
integer(int32) :: v(9) = [10, 20, 20, 20, 30, 40, 40, 50, 60]
integer :: lo, hi, first, last

call pf_lower_bound(v, 20_int32, lo)        ! lo = 2   -- the first 20
call pf_upper_bound(v, 20_int32, hi)        ! hi = 5   -- one past the last 20
call pf_equal_range(v, 20_int32, first, last)   ! first = 2, last = 4
call pf_equal_range(v, 25_int32, first, last)   ! first = 5, last = 4  -- absent
```

`hi - lo` is how many elements equal the target, and `pf_equal_range` returns the same information
as an **inclusive** range from one pass. When the target is absent, `last == first - 1`, so
`last - first + 1` is zero — **check that count before reading `values(first)`**.

### Check the order once, not on every call

By default each search runs an **O(n) sortedness check in front of an O(log n) search**. That
default is deliberate and should not be flipped: searching unsorted input does not fail, it returns
a plausible index with no abort and no symptom at all — the worst failure mode this module has.

The cost is meant to be paid **once**, not per call:

```fortran
call pf_is_sorted(v, ok)                        ! O(N), once
if (.not. ok) call pf_sort(v, v)

do k = 1, m
    call pf_lower_bound(v, targets(k), pos, assume_sorted=.true.)   ! O(log N) each
end do
```

Without `assume_sorted=.true.` in that loop, `m` searches cost `O(m·N)` rather than `O(m·log N)`
and the feature looks broken. Only pass it for an order you have actually established.

`descending=` and `nulls_first=` **select the comparison, they do not reorder anything** — they
must describe the order the array is genuinely in, or the check rejects it. A null is ordered after
every value by default, so a target larger than every value lands *before* the nulls rather than at
the end of the array.

A `character` target is compared at the **array's** element length: a shorter target is
blank-padded, exactly as a Fortran comparison would pad it. A target carrying non-blank characters
past that length has no exact answer and is refused rather than silently truncated. A
`parquet_string_column` stores bytes verbatim and has no declared width, so its target is used
verbatim too, trailing blanks included.

## Distinct values and ranks

```fortran
integer(int32) :: v(8) = [30, 10, 20, 10, 30, 30, 40, 20]
integer(int32), allocatable :: d(:)
integer, allocatable :: r(:)
integer :: c

call pf_unique_count(v, c)          ! c = 4
call pf_unique(v, d)                ! d = [10, 20, 30, 40]
call pf_rank(v, r)                  ! r = [5, 1, 3, 1, 5, 5, 8, 3]
```

Those ranks are the default `"competition"` method, which leaves a gap after each tie — the two
`10`s both rank 1 and the next value ranks 3. `"dense"` would give `[3, 1, 2, 1, 3, 3, 4, 2]`
instead; see [Tie handling in `pf_rank`](#tie-handling-in-pf_rank) below.

**Nulls are outside the population in all three.** They are excluded from the count, absent from
the distinct values, and given **rank 0** — which is why none of the three takes `nulls_first`:
there is no null tier to position. `pf_unique_count` and `pf_unique` report how many were dropped
through an optional `n_null=`.

`pf_unique` returns the distinct values **sorted**; `descending=` reverses that order.
`pf_unique_count` takes no `descending` at all, since a count does not depend on direction.

Distinctness is the sort comparator's own equality, which on floating point is **exact**:

```fortran
real(real64) :: v(2) = [0.1_real64 + 0.2_real64, 0.3_real64]
call pf_unique_count(v, c)          ! c = 2, not 1
```

That is correct and surprising, and it is the same equality every other operation here uses. In the
other direction, every NaN counts as **one** value collectively — NaNs compare equal to each other
under this comparator even though `==` reports every NaN pair as unequal.

### Tie handling in `pf_rank`

`method=` chooses how ties are ranked, matched case-insensitively:

| `method=` | ranks of `10, 20, 20, 30` |
|---|---|
| `"competition"` (the default) | 1, 2, 2, 4 |
| `"dense"` | 1, 2, 2, 3 |
| `"ordinal"` | 1, 2, 3, 4 |

An unrecognized token aborts, naming the valid ones. `"ordinal"` ranks are exactly the inverse of
`pf_argsort`'s permutation — `r(perm(k)) == k` — so reach for `pf_argsort` when you want the order
and `pf_rank` when you want a per-element answer that stays aligned with the input.

NaNs are ranked as ordinary values (all tying with each other), unlike nulls.

## Extremes

```fortran
call pf_minmax(v, vmin, vmax)       ! the values
call pf_argminmax(v, imin, imax)    ! where they are
```

Two procedures rather than one call with optional index arguments: optional `imin`/`imax` varying
only by integer kind would make a positional call ambiguous, and each name says what it returns. A
caller who wants both pays one extra call rather than every caller paying for indices they did not
ask for.

Both **skip nulls and NaNs** — a NaN is an ordinary value everywhere else in this module, but it is
not the minimum or maximum of anything. Both report the **first** occurrence of a tied extreme, at
either end. Neither takes `descending`/`nulls_first`: a minimum and a maximum are absolute, and
reversing the order would only exchange the two answers.

**Both abort when every value is null or NaN.** There is nothing to return and no sentinel that
works across all nine types — the same decision `pf_nth_quantile` makes for the same unanswerable
question. Guard with `count(is_valid)` where that can happen.

## Merging two sorted arrays

`pf_merge` merges two **already-sorted** arrays in `O(size(a) + size(b))`, rather than the
`O(n log n)` of sorting their concatenation:

```fortran
integer(int32) :: a(4) = [1, 4, 6, 9], b(5) = [2, 3, 6, 7, 10]
integer(int32), allocatable :: m(:)

call pf_merge(a, b, m)              ! m = [1, 2, 3, 4, 6, 6, 7, 9, 10]
```

Ties take from `a` first, so the result matches `pf_sort` of the concatenation element for element.
`descending=`/`nulls_first=` select the comparison exactly as in the searches, and both inputs are
checked for sortedness unless `assume_sorted=.true.`.

**Supply `is_valid_a`/`is_valid_b` whenever either input has nulls.** A sorted array containing
nulls is precisely what `pf_sort(..., is_valid=)` produces, and a merge that is not told which
elements are null compares them as ordinary values and interleaves them into the middle of the
result. The precondition cannot be checked either — a null's stored value is indistinguishable from
a real one without the mask.

```fortran
call pf_merge(a, b, m, is_valid_a=ma, is_valid_b=mb, merged_valid=mv)
```

`merged_valid` follows the same rule as `pf_sort`'s `sorted_valid`: **always allocated** when you
ask for it, all `.true.` when neither input mask was supplied.

Two `character` arrays of different declared lengths merge into the wider one, so `merged` is
declared `character(len=:), allocatable` rather than at a fixed width — the one output in this
module whose length comes from two inputs rather than one.

## Sorting in parallel

**Sorting is parallel by default.** `pf_argsort`, `pf_sort`, `pf_unique_count`, `pf_unique` and
`pf_rank` all use the machine automatically, as do the read-time
`parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`. There is nothing to switch on.
`pf_partial_sort`, `pf_partial_argsort`, `pf_nth_element` and `pf_nth_quantile` take `threads=`
too, but thread less of their work — see
[What `threads=` reaches in a selection](#what-threads-reaches-in-a-selection) below.

Those nine are the whole list. `pf_permute`, `pf_is_sorted`, the three searches, `pf_minmax`,
`pf_argminmax` and `pf_merge` take no `threads=` at all — each is a single linear pass, so there is
nothing to hand a team — and passing one is a compile error rather than a silently ignored argument.

`threads=` is therefore how you turn parallelism **down**, not up:

```fortran
call pf_argsort(v, perm)             ! auto -- all available cores
call pf_argsort(v, perm, threads=4)  ! at most four
call pf_argsort(v, perm, threads=1)  ! serial
```

**The answer never changes.** A parallel sort returns a permutation *bit-identical* to the serial
one, on every input, at every thread count. That is structural rather than lucky: the comparator
ends with a tiebreaker on the row index, making it a total order under which no two rows compare
equal, so every correct sorting algorithm — serial, threaded, stable or not — must produce the same
answer. `threads=` is a performance control and nothing else.

### What "auto" means

| situation | threads used |
|---|---|
| ordinary serial code | `omp_get_max_threads()` |
| inside any `!$omp parallel` region | **1** — serial |
| explicit `threads=n`, inside a region running on 2+ threads | `n` — honoured |
| explicit `threads=n`, inside a region running on 1 thread | **1** — see below |
| explicit `threads=n`, ordinary serial code | `n` |
| array below the minimum-work threshold | 1 |
| a narrow-range integer key, on a small team | 1 — the counting path (see below) |

The second row is the one worth knowing. Inside a parallel region, auto stays serial because
*nested regions are the caller's business* — eight OpenMP threads each asking for eight more would
be sixty-four threads, slower than not threading at all. If you genuinely want a threaded sort from
inside your own parallel region, say so with an explicit `threads=`.

**The one place an explicit `threads=` is not honoured is a region that exists but is running on a
single thread** — `!$omp parallel if(cond)` with `cond` false, or any region at all under
`OMP_NUM_THREADS=1`. A sort there runs serially however many threads you asked for. This is not a
policy choice: opening a thread team one level down from such a region deadlocks GNU's OpenMP
runtime, intermittently and with no diagnostic, and refusing is the only reliable way to avoid it.
A region running on two or more threads is unaffected and honours `threads=` exactly as it does in
ordinary serial code.

`pf_sort_threads()` reports what auto would do right now, if you want to log it or size something
against it.

### Two reasons a sort may decline to thread

**A small array.** Spawning threads to sort a few thousand elements costs more than the sort, so
there is a minimum below which `threads=` is ignored.

**The integer fast path.** A single integer key spanning a narrow enough range is counting-sorted,
which is already O(n) and already produces this exact permutation, so it is taken in preference to
threading. `threads=` is a *hint*, not a command, and a sort that reports one thread on such a key
is behaving correctly.

Two things decide whether that path is taken, and **neither is how many distinct values there are**
— it keys on the value **range**. The range has to be narrow *relative to the row count*, so the
same range qualifies on a large array and not on a small one; and the team has to be small, because
a counting sort is serial and past a couple of threads the parallel radix sort simply wins. Nulls
do not disqualify it — a null row's slot is skipped when the range is measured, so a null-bearing
key reaches the same path. `parquet_set_sort_counting_bucket_limit` caps the range in absolute
terms as well; see [Tuning the sort](../operating/settings.html#tuning-the-sort) for that knob and
for turning the path off entirely.

### What it actually buys

`pf_argsort` over scattered `real(real64)`, on a small machine, best of several rounds — the shape
matters here rather than the absolute times, which are whatever your own hardware gives you.
`bench/benchmark_sort_engine.sh` measures it there (see CONTRIBUTING.md):

| rows | 8 threads against serial |
|---|---|
| 2 000 | threading declines — see below |
| 32 000 | threading declines — see below |
| 1 000 000 | a few times faster |
| 20 000 000 | a few times faster, and gaining slowly with size |

**The two smallest rows do not show threading losing; they show it declining.** Below the
minimum-work threshold `threads=` is ignored, so both arms run the same serial sort and differ only
by measurement noise. That is the threshold behaving correctly rather than a cost being paid —
opening a thread team costs more than sorting a few tens of thousands of elements does in the first
place, which is the comparison the threshold makes.

**The speedup falls short of the thread count, and that is expected.** A radix sort makes one pass
over the data per byte of key, so its cost is dominated by memory traffic rather than by
comparisons; adding threads adds bandwidth demand rather than relieving it. The gap widens with
size for the same reason — at twenty million rows the working set no longer fits in cache, and the
sort becomes bound by memory bandwidth rather than by how many threads are available.

**Threading is not the only thing that decides how long a sort takes, and often not the largest.**
A single integer key with a narrow enough value range takes the counting path described above,
which is O(n) and serial — so a key that qualifies beats every row of this table without opening a
team at all. Whether it qualifies depends on the requested team as well as on the data, since past
a couple of threads the parallel radix sort is the faster of the two.

### Cost

**Peak memory does not depend on `threads=`.** A threaded sort needs a scratch buffer the size of
the permutation, but so does a serial one — the threaded arm allocates it up front and the serial
arm on first need, and they are the same size. Measured peak resident set for a 50-million-row
`real(real64)` argsort was 2816 MB at `threads=1` and 2816 MB at `threads=8`. So passing
`threads=1` to save memory gives up the speed and saves nothing.

What the scratch pays for is the **radix path**, not the threading, and that has its own knob: see
[Tuning the sort](../operating/settings.html#tuning-the-sort), which sets out what the radix path
costs per row and the trade against turning it off. On the same 50-million-row sort,
`parquet_set_sort_radix_path(.false.)` took the peak from 2816 MB to 1216 MB.

### What `threads=` reaches in a selection

All four selection operations accept `threads=` — `pf_partial_sort`, `pf_partial_argsort`,
`pf_nth_element` and `pf_nth_quantile` — with the same meaning and the same defaults as everywhere
else, and with the same guarantee that the answer never changes.

**They thread less of the work than `pf_argsort` does, and it is worth knowing which part.** A
selection has three stages: reading the values into the engine's key form, choosing the elements
that belong at the requested positions, and handing the result back. **Only the first is threaded.**
It walks every element, so it is usually most of the cost; the choosing in the middle is what makes
a selection cheaper than a full sort and has no threaded form.

So `threads=` helps here in proportion to the *array*, not in proportion to how much you asked for:

```fortran
call pf_partial_argsort(flux, brightest, n=10, threads=8)   ! the read of flux is threaded
call pf_partial_sort(flux, top, n=10, threads=8)            ! likewise
call pf_nth_element(flux, 1000, median, threads=8)
call pf_nth_quantile(flux, 0.5_real64, median, threads=8)
call pf_partial_argsort(flux, brightest, n=10, threads=1)   ! wholly serial
```

Exactly what each one leaves serial, since the answer differs a little:

| call | threaded | serial |
|---|---|---|
| `pf_partial_argsort` | the read, and the int32 narrowing | the selection |
| `pf_partial_sort` | the read | the selection, and the `n`-element gather that builds the result |
| `pf_nth_element` | the read | the selection |
| `pf_nth_quantile` | the read | the selection, and the pass that counts the non-null population |

On a large array and a small `n` the read is most of the cost, which is exactly the case this
argument exists for. On a plain integer array it is cheap already and `threads=` will buy little;
on a string or a `parquet_column` key it is the dominant term.

One specific takes the argument and can do nothing with it: `pf_partial_argsort(keys, perm, n)` on a
`pf_sort_keys` producing an **int64** permutation. Its keys were already built by `%add`, so there
is nothing to read, and an int64 permutation needs no narrowing. It is accepted so that the generic
behaves uniformly rather than rejecting an argument its int32 sibling takes.

## What is not here yet

`pf_partial_sort` and `pf_partial_argsort` are not defined for `parquet_string_column` or
`parquet_column`, for the same reason `pf_sort` is not. `pf_nth_element` and `pf_nth_quantile` are
not defined for `parquet_column`: its element type is a runtime discriminator, so there is no
compile-time type for the value they return.
