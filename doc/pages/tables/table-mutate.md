---
title: Changing a table
---

An open table can be changed in memory: values replaced, nulls edited, columns added, dropped
or renamed, rows filtered, sorted, ranked or appended. This page is the reference for those
operations and the rules they share. The basics of working with a table are in
[Whole tables in memory: the basics](table.html).

## Replacing values

`%set` replaces a column's values from an array of the **same length** — it changes values, never
the row set:

```fortran
call t%get("mass", m)
m = m * 2.0_real64
call t%set("mass", m)
```

By default every row is written and the column's null bitmap is dropped outright — after a plain
`%set` the column holds no nulls at all. Pass `modify_nulls=.false.` to leave the null rows, and
their bitmap, exactly as they were.

To write a single cell, use `%set_element`, and to read one, `%get_element`:

```fortran
call t%set_element("mass", 42, 1.75_real64)   ! row 42 of the mass column
call t%get_element("mass", 42, m)             ! ... and back out again
```

`%get_element` widens into your variable exactly as `%get` does, takes either integer kind for
the row index, and is the one-call form of the two-step row handle (`r = t%row(42)` then
`call r%get("mass", m)` — see [One row at a time](table.html#one-row-at-a-time)).

The value's kind must match the column's exactly, as it does for `%set`, and writing a value
**clears that row's null** — a cell cannot be both a value and missing.

## Null values, and changing them

`%is_null`, `%set_null` and `%clear_null` each work a **row** at a time or a single **element** at
a time, depending on whether you name an element:

```fortran
call t%set_null("spectrum", 7_int64)              ! the whole row: every element of row 7
call t%set_null("spectrum", 7_int64, 3_int64)     ! just element 3 of row 7
print *, t%is_null("spectrum", 7_int64, 3_int64)  ! .true.
print *, t%is_null("spectrum", 7_int64)           ! .true. -- "any element of row 7 is null"
```

**Each call acts at the granularity you named**, and the one place that is worth stating twice is
the difference between asking and setting:

- **Naming only a row and setting** marks the row missing — every element of it.
- **Naming only a row and asking** answers about the row as a whole: `%is_null(name, i)` is `.true.`
  when *any* element of it is null. So a row you nulled one element of reports itself null, which is
  usually what you want to know before using it.

Element indices run `1..width` and are checked; a scalar column has width 1, so `e` can only be 1
and the two forms agree. The row handle takes the same pair: `r%is_null("spectrum")` and
`r%is_null("spectrum", 3_int64)`.

`%set_null` also takes a **mask**, in either shape — one `logical` per row, or one per element,
matching whichever shape `%get_valid_mask` gave you:

```fortran
call t%get_valid_mask("flux", valid)          ! valid(:)   -- per row
valid = valid .and. (flux > 0.0_real64)
call t%set_null("flux", valid)                ! nulls every row the mask marks .false.

call t%get_valid_mask("spectrum", vok)        ! vok(:,:)   -- per element, (width, nrows)
call t%set_null("spectrum", vok)              ! nulls individual elements
```

On a vector column the rank-1 `%get_valid_mask` is the **row summary** (`.false.` where any element
of the row is null) and the rank-2 one is the true per-element state. Both are useful — the first
answers "which rows are complete?" — so both are available, and which you get is decided by how you
declared the array.

It only ever *adds* nulls: a `.true.` entry leaves the entry exactly as it was, so a mask describing
only part of what you know cannot clear a null you did not mention. `%has_nulls(name)` answers
whether there are any at all — and for a column that has not been read yet it answers from the
file's footer without reading it, so it is cheap enough to ask before deciding to.

A column that holds no null at all carries no null bitmap, which is why `%compact_validity(name)`
exists: it drops the bitmap of a column that once had nulls and no longer does. A whole-column
`%set` already does this on its own, so `%compact_validity` is only for a column edited cell by
cell.

## Changing a table

Mutation falls into three classes, and the difference between them matters more than any
individual procedure:

| class | what it does | detaches? |
|---|---|---|
| **cell** — `%set_element`, `%set_null`, `%clear_null` | changes values in place | no |
| **column** — `%add_column`, `%drop_column`, `%rename_column`, `%copy_column`, `%cast` | changes which columns exist, or a column's kind | no |
| **row** — `%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows` | changes which rows exist | **yes, when it changes one** |

```fortran
call t%materialize_all()                 ! read everything you want to keep, first
call t%filter_rows(mass > 1.0e10_real64) ! keep the rows a mask selects
call t%sort_by(["mass"], descending=[.true.])
call t%drop_column("scratch")
```

The two simplest column changes are worth a sentence each, because both are cheaper than they
look:

- **`%drop_column` never reads the column it drops.** Dropping one that was never touched is the
  memory-reclaiming case and costs nothing; the remaining columns keep their order, so
  `%column_names` still reads like the file. Keeping that order costs nothing either — the
  remaining columns' storage is handed over rather than copied.
- **`%rename_column` changes only the name you look the column up by.** A file-backed column that
  has not been read yet still reads from the same physical column afterwards, which is what lets a
  rename compose with a MAML [remap](table-open.html#renaming-a-files-columns-with-a-read-in-maml) in either
  order. The new name must not be blank and must not already be taken.

### What "detaching" means

A table opened from a file reads its columns lazily. Once a **row**-changing operation runs, the
rows in memory no longer line up with the rows in the file, so any column that had not been read
by then can never be read at all. The table records this — `%is_detached()` reports it — and every
later attempt to read from the file says so rather than returning misaligned data.

**So read what you need before changing the row set**, with `%prefetch` or `%materialize_all`.
Columns you deliberately do not want are simply left behind, which is what keeps a lazy table
from having to read a whole file before it can drop a single row.

Detaching does not freeze a table: it can still be read, edited and written out, and mutated
further. What it loses is the file behind it.

**A call that changes no row does not detach.** Detaching costs you every column you have not read
yet, permanently, so it is only paid for when the row set actually moved — a row mutation that
turns out to have nothing to do returns without touching a column, without invalidating a `%col`
pointer, and with the file still attached. Five of the six are decided by the arguments you passed:

| call | changes nothing when |
|---|---|
| `%truncate(n)` | `n >= %nrows()` |
| `%filter_rows(keep)` | every entry of `keep` is `.true.` |
| `%delete_rows(indices)` | `indices` is empty |
| `%append(other)` | `other` has no rows (it is still checked for compatibility first) |
| `%append_null_rows(n)` | `n == 0` |
| `%sort_by(keys)` | the rows were already in that order |

The last is the odd one out: whether a sort moves anything depends on the **data**, not on the
call, so `%sort_by` on an already-ordered column leaves the table attached and the same call after
an edit may not. Treat "did it detach?" as something to ask (`%is_detached()`) rather than
predict, and never rely on staying attached across a mutation you expect to be a no-op.

### Removing rows

`%filter_rows(keep)` is the general form and takes a mask of exactly `%nrows()` entries.
`%delete_rows(indices)` and `%truncate(n)` are conveniences over it, in either integer kind:

```fortran
call t%delete_rows([3, 7, 7, 11])   ! a row named twice is still removed once
call t%truncate(1000)               ! keep the first 1000 rows
```

A row index outside `1..nrows` is an error, and so is a negative `n`; both are checked before
anything is changed, so a rejected call leaves the table as it was. `%truncate(0)` empties the
table. A `%truncate` asking to keep more rows than there are, a `%filter_rows` whose mask keeps
everything and a `%delete_rows` with no indices all return without touching anything and without
detaching — see [What "detaching" means](#what-detaching-means).

To keep rows by *rank* rather than by position or by mask — the best hundred by some column — use
[`%top_n`](#keeping-only-the-best-rows), which is cheaper than sorting and cutting.

### Sorting

`%sort_by` takes one or more key columns, primary first, with optional per-key `descending=` and
`nulls_first=` arrays:

```fortran
call t%sort_by(["group", "mass "], descending=[.false., .true.])
```

A key column that has not been read yet is read for you, by the same lazy first touch `%get` and
`%col` use — so sorting a table you have just opened needs no `%prefetch` first. Only the key
columns are read; the rest stay exactly as they were. Nulls and NaNs are placed absolutely and are
never flipped by `descending`; ascending order gives values, then NaNs, then nulls. Ties keep their
existing order.

**Any scalar column can be a key** — numeric, logical, string, date, time or timestamp. A vector
column cannot: there is no defined order on a whole vector row, so naming one is an error, as are
an empty key list and a `descending=`/`nulls_first=` array whose length does not match the keys.
Every key is validated before a single column is touched, so a rejected `%sort_by` leaves the
table exactly as it was — and a sort that finds the rows already in the order asked for moves
nothing and leaves the table attached (see [What "detaching" means](#what-detaching-means)).

This is the same sort engine `parquet_open_reader(..., sort_by=...)` uses, so sorting a table in
memory and reading the same file sorted give the identical row order.

### Keeping only the best rows

When you want the brightest hundred rows and not the other hundred million, `%sort_by` is the wrong
tool: it orders every row and reallocates every column, to keep 100 of them. `%top_n` selects
instead. Optional arguments are shown in square brackets; they are not part of the call syntax:

```fortran
call t%top_n(["flux"], 100, descending=[.true.])   ! the 100 brightest, brightest first
```

`call t%top_n(keys, n, [descending], [nulls_first])` takes the same keys, the same engine and the
same refusals as `%sort_by`, including reading a key column that has not been read yet. Afterwards
the table holds exactly those `n` rows, in key order — it is `%sort_by` followed by `%truncate(n)`,
at O(rows) instead of O(rows log rows), and gathering each column straight to `n` rows instead of
reindexing it in full and then cutting it down.

**"The last `n`" is `descending=`**, not a separate call — with the default ascending order,
`t%top_n(["flux"], 100)` keeps the hundred *faintest* rows.

**`n` is clamped, not checked.** Asking for more rows than the table has keeps all of them, so an
`n` derived from a fraction or a config value needs no `min()` of its own; a negative `n` is still
an error. At or above the row count this *is* a whole sort, and behaves as one — including leaving a
table whose rows are already in that order completely alone.

Like every row-changing operation it **detaches** the table (see
[What "detaching" means](#what-detaching-means)), so a column you still want must be read before the
call, not after:

```fortran
call t%prefetch("name")                  ! or %materialize_all()
call t%top_n(["flux"], 100, descending=[.true.])
```

That includes [`parquet_row_index`](table-open.html#which-row-of-the-file-is-this), which is how to find out *which*
file rows survived — ask for it before the call and it is gathered along with everything else.

There is deliberately no `%take(indices)` taking a permutation you built yourself. Applying an
arbitrary row order to a table is the one thing this type refuses: applied to some columns and not
others, or applied from an array that has since gone stale, it breaks the row correspondence that
makes a table a table, and nothing detects it. `%top_n` is safe because it builds the indices itself,
from this table's own columns, and applies them to every column together. If you want to *read* rows
in an order without changing anything, that is `%argsort_by` and `%get_slice`, next.

### Ordering rows without reordering them

`%sort_by` **detaches** the table, which is the right thing when you meant to reorder it — but it
also means there is no way to get a sorted *view* of a table and keep the file behind it. That is
what `%argsort_by` is for: it answers with the row order and moves nothing.

```fortran
integer(int64), allocatable :: perm(:)
real(real64), allocatable :: mass(:)

call t%argsort_by(["mass"], perm)                     ! the order, unapplied
call t%get_slice("mass", parquet_slice_list(perm), mass)   ! read in that order
```

The table is untouched and still attached, so lazy columns still read, `%reload` still works, and
nothing has been reallocated. Beyond a sorted read this is also how to apply one table's order to
another table or to a plain array, which nothing else in the library offers.

Same keys, same engine and same refusals as `%sort_by`, including per-key `descending=` and
`nulls_first=`. Optional arguments are shown in square brackets below; they are not part of the
call syntax:

| call | what it gives |
|---|---|
| `call t%argsort_by(keys, perm, [descending], [nulls_first], [group_offsets], [group_nkeys])` | the row order the keys imply |
| `call t%argsort_partial(keys, perm, n, [descending], [nulls_first])` | just the `n` best rows, by selection |
| `call t%top_n(keys, n, [descending], [nulls_first])` | *(mutating)* keeps only those `n` rows — see [above](#keeping-only-the-best-rows) |
| `ok = t%is_sorted_by(keys, [descending], [nulls_first])` | whether the rows are already in that order |

`perm` may be declared `integer(int32)` or `integer(int64)`; `group_offsets` follows whichever you
chose, since its entries are positions in `perm`.

**`%argsort_partial` is the one with an asymptotic argument.** Asking a 100-million-row table for
its brightest 100 rows through `%sort_by` sorts everything and reallocates every column;
`%argsort_partial` selects instead, and reorders nothing at all. `n` is *clamped* to the row count
rather than checked, so a derived `n` needs no `min()` of its own, and "the last `n`" is
`descending=`, not a separate call.

**`%is_sorted_by` costs O(rows) with an early exit**, where asking `%sort_by` the same question
costs a full sort:

```fortran
if (.not. t%is_sorted_by(["ra"])) call t%sort_by(["ra"])
```

#### Grouping

`group_offsets=` reports where each run of rows equal under the keys begins, in the same pass as
the sort:

```fortran
integer(int64), allocatable :: perm(:), go(:)

call t%argsort_by(["field_id"], perm, group_offsets=go)
do g = 1, size(go) - 1
    call t%get_slice("mag", parquet_slice_list(perm(go(g):go(g+1) - 1)), mags)
end do
```

The array has length `ngroups + 1` and its last entry is the sentinel `nrows + 1`, so every group
slices the same way and the last one needs no special case. All nulls form one group and all NaNs
form one group.

`group_nkeys=` groups on the first few keys **without changing the sort**, which is what gives
"grouped by field, ordered by magnitude within each group":

```fortran
call t%argsort_by(["field_id", "mag     "], perm, group_offsets=go, group_nkeys=1)
```

It counts key *names*, must be between 1 and the number of keys, and requires `group_offsets`.

> **A permutation goes stale silently.** It describes the table *as it was*. Any row-structural
> change — `%sort_by`, `%top_n`, `%filter_rows`, `%delete_rows`, `%truncate`, `%append` — invalidates it, and
> nothing reports that: against a table that has since shrunk, the indices stay in range and name
> the wrong rows. This is the same hazard as a saved `%col` pointer, except that a stale pointer
> usually crashes while a stale permutation just answers wrongly. `%generation()` is bumped by every
> such change — record it beside a permutation you intend to keep, and compare before reusing it.

### Adding rows

The way to add many rows is a batch that structurally cannot have the wrong columns:

```fortran
call t%clone_structure(batch)     ! same columns, zero rows, no file
call batch%append_null_rows(n)    ! give it n rows to fill
call batch%set("id", ids)         ! ... and fill them
call batch%set("mass", masses)
call t%append(batch)
```

`%clone_structure` reads nothing: every column's kind and width are known from the file's schema,
so a batch can be cloned from a freshly opened table without touching a column. Pass
`resident_only=.true.` to clone only the columns that have been read.

`%append` requires the appended table's columns to be a subset of this table's, with matching
kinds, widths and units. A column this table has and the batch does not is **null-filled**; a
column the batch has and this table does not is an error rather than being silently dropped; a
kind mismatch is an error too, and `%cast` is the way round it. There is no unit
conversion, so appending "km/h" rows to an "m/s" column is refused.

`%append(row)` adds one row from a `parquet_table_row` handle, copying just that row out of the
handle's table. The batch form above is still cheaper per row — a row append takes the table's lock,
advances the generation counter and detaches once per call — but it is no longer quadratic, so
building a table a row at a time is a perfectly reasonable thing to do.

Growing a table row by row costs nothing extra in allocation either: a column's storage grows
**geometrically**, so a run of appends reallocates a handful of times rather than once per row. The
price is that an appended-to table can hold up to 1.5x the storage its rows need. Two calls control
that, and neither changes the row set, so neither detaches:

```fortran
call t%reserve(1000000)      ! one allocation up front, if the final size is known
do i = 1, 1000000
    call t%append(rows%row(i))
end do
call t%compact()             ! give the slack back
```

**Both invalidate every `%col` pointer and row handle into the table**, because both reallocate
storage — they are the only two operations that do so without changing the row set. `%generation()`
advances only when something actually moved, so the usual take-it-before / compare-it-after /
re-fetch pattern re-fetches only when a pointer really did die.

`%compact()` is a **no-op on a table that has not been appended to**: reading a column from a file
allocates exactly what it holds, and so does every rebuild (`%filter_rows`, `%sort_by`, `%top_n`,
`%delete_rows`, `%truncate`), which hand their memory back on their own. So it is safe to call
unconditionally. It is *not* needed before `parquet_write_table` — the writer never sees the slack.
`%reserve(n)` takes the total row count to make room for, not an increment, and reserving less than
the table already holds does nothing.

Appending a **zero-row** table is checked for compatibility exactly as any other append, and then
does nothing at all — including not detaching. `%append_null_rows(0)` is the same.

### Copying, and going back

Mutation happens in place and there is no undo, so the way to keep a version to return to is to
copy first:

```fortran
call t%clone(before)     ! independent deep copy
call t%filter_rows(keep)
```

A clone copies the columns already read and leaves the unread ones unread, opening its own reader
on the same file so it stays lazy — with the same read-time transform reattached, so a column the
source never touched still comes back with the source's rows. Cloning a **detached** table copies
the values it holds and leaves the copy detached too: there is no file left to reopen, for either
of them. `before` must be declared as the same concrete table type as `t`.

> **A pointer from `%col` does not survive a row-changing operation.** `%filter_rows`, `%sort_by`,
> `%append` and the rest reallocate each column's storage, so a pointer taken before one of them
> points at freed memory afterwards. Fortran cannot detect this. Take the pointer again after the
> mutation.

## Looking a value up in a sorted table

There is no table-level binary search, and it is not missing: `%col` hands back a plain pointer to a
column's storage, and [`parquet_sorting`](../utilities/sorting.html)'s searches work on that directly.

```fortran
real(real64), pointer :: ra(:)
integer(int64) :: lo, hi
logical :: ok

call t%sort_by(["ra"])
call t%col("ra", ra)

call pf_is_sorted(ra, ok)                 ! O(n), once
do i = 1, size(targets)
    call pf_equal_range(ra, targets(i), lo, hi, assume_sorted=ok)
    ! rows lo .. hi-1 hold targets(i)
end do
```

**Check once, outside the loop.** `assume_sorted` defaults to `.false.`, which is the safe default —
searching unsorted input returns a plausible index with no symptom — but it makes each search O(n)
in front of an O(log n) operation. Hoisting one `pf_is_sorted` out of the loop turns *m* searches
from O(m·n) into O(n + m log n).

**Re-take the pointer after any row-changing operation**, per the warning above, and re-check
sortedness after anything that could disturb the order. A table does *not* remember that it is
sorted, deliberately: a stale "still sorted" flag would return wrong rows silently.

## Ranking rows by a column

Likewise there is no `%rank` — adding a rank or percentile column is three lines through `%col`:

```fortran
real(real64), pointer :: flux(:)
integer(int64), allocatable :: ranks(:)

call t%col("flux", flux)
call pf_rank(flux, ranks, descending=.true.)   ! 1 = brightest
call t%add_column("flux_rank", ranks)
```

`method=` picks how ties are handled — `"competition"` (1,2,2,4, the default), `"dense"` (1,2,2,3)
or `"ordinal"` (1,2,3,4) — and **a null gets rank 0**, since a missing value has no rank rather than
the last one. Dense ranks over a sorted key column are also the cheapest way to label groups: every
row of a group gets the same number.
