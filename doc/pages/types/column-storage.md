---
title: Column storage with parquet_column
---

`parquet_column` is one column of data whose type is not known at compile time. It holds any of the
library's element kinds behind one Fortran type, tracks a null bit for every element, grows as you
append, and can be handed to the writer, to the sorting tier or into a `parquet_table` without the
code carrying it knowing what is inside. That is what makes a program able to read a file whose
schema it learns at run time, rewrite one column of it and write it back.

`use parquet_columns` is an entry module in its own right: ten files, no Arrow, no settings read
and nothing printed. Every type in this guide that stores values in bulk is built on it — the
[list](list-columns.html), [map](map-columns.html) and [struct](struct-columns.html) container
columns each hold one as their payload, and a `parquet_table` is a named collection of them.

Signatures on this page are written with **optional arguments in square brackets**; the brackets
are notation, not Fortran, and never appear in code you write.

```fortran
use parquet_columns
type(parquet_column) :: col
integer(int64) :: n

call col%init(PK_FLOAT64, 1000_int64)      ! kind and row count
call col%set_at(1_int64, 3.5_real64)       ! one value
call col%set_null(2_int64)                 ! one null
n = col%length()                           ! 1000
print *, col%any_null()                    ! T
```

## The kinds a column can hold

A column's kind is a `PK_*` discriminator, fixed by `%init` (or by whatever the column adopted) and
readable back with `%kindof()`. There are **nine element kinds**, each with a fixed-width **vector**
form that stores `colwidth()` values per row, and **three container kinds** that store a variable
amount per row:

| | scalar | vector (`colwidth() > 1`) |
|---|---|---|
| 32-bit integer | `PK_INT32` | `PK_INT32_VEC` |
| 64-bit integer | `PK_INT64` | `PK_INT64_VEC` |
| 32-bit real | `PK_FLOAT32` | `PK_FLOAT32_VEC` |
| 64-bit real | `PK_FLOAT64` | `PK_FLOAT64_VEC` |
| logical | `PK_LOGICAL` | `PK_LOGICAL_VEC` |
| string | `PK_STRING` | `PK_STRING_VEC` |
| date | `PK_DATE` | `PK_DATE_VEC` |
| time | `PK_TIME` | `PK_TIME_VEC` |
| timestamp | `PK_TIMESTAMP` | `PK_TIMESTAMP_VEC` |

`PK_LIST`, `PK_MAP` and `PK_STRUCT` are the container kinds; a column takes one by adopting an
already-built container rather than by `%init` — see
[Containers and string stores](#containers-and-string-stores) below. `PK_NONE` is the kind of a
column that has been `%clear`ed or never initialised, and `parquet_kind_name(kind, out)` writes any
of them out as text.

Which Parquet types those map to, and what another tool's file may contain, is
[Supported data types](supported-data-types.html).

## Creating and filling a column

**`%init(kind, nrows, [width], [unit])`** activates a kind and allocates storage for `nrows` rows.
`width` is required for a `*_VEC` kind and is the values-per-row count; `unit` stores a unit string
alongside the data. Every row starts null.

**`%adopt(values, [unit])` is the way to fill a column from an array you already have.** It
`move_alloc`s the array in and derives the kind, the width and the row count from it, so nothing is
copied:

```fortran
real(real64), allocatable :: v(:)
v = [1.0_real64, 2.0_real64, 3.0_real64]
call col%adopt(v)              ! v is left deallocated; col is PK_FLOAT64 with 3 rows
```

Prefer it to `%init` followed by `%set_all` — that pair allocates the storage and then copies into
it, where `%adopt` does neither. A rank-2 array adopts as the matching `*_VEC` kind, with `width`
taken from its first dimension. The two string kinds have no `%adopt`: a string column's payload is
not one Fortran array, so there is nothing to move.

**`%clear()`** releases everything and returns the column to `PK_NONE`; the kind must be given again
before it can hold anything.

One subtlety worth knowing early: `%init` allocates its storage at **zero rows**, so a column of a
known kind always has storage even while `%capacity()` reads 0.

## Reading and writing values

Per-cell access is by generic, resolved on the type of the value you pass — which must match the
column's kind, or the call aborts naming both:

```fortran
call col%get_at(i, value)               ! row i of a scalar column
call col%set_at(i, value, [modify_nulls])
call col%get_elem(i, e, value)          ! element e of row i of a *_VEC column
call col%set_elem(i, e, value, [modify_nulls])
call col%set_all(values, [modify_nulls])   ! a whole array at once
call col%append_values(values)          ! append an array to what is there
```

`%set_at` on a null row makes it valid. Pass `modify_nulls=.false.` to leave individual null
elements alone — the way to write over a column's values without disturbing which of them are
missing.

**A character ARRAY is trimmed on the way in; a character SCALAR is not.** `%set_all` and
`%append_values` take a `character(len=*)` array whose elements all share one declared length, so
the padding cannot be what you meant and is removed. `%set_at`'s scalar form stores exactly the
bytes it is given, trailing blanks included. That asymmetry is deliberate and is the same rule the
rest of the library applies; if you need an array's blanks preserved, write the elements one at a
time.

## Nulls are per element, and there are two forms of every question

Validity is tracked for **every element**, `colwidth() * length()` bits in all — not one bit per
row. So each query and each mutation comes in a row form and an element form:

| | row form | element form |
|---|---|---|
| ask | `%is_null(i)` | `%is_null(i, e)` |
| mark | `%set_null(i)` | `%set_null(i, e)` |
| unmark | `%clear_null(i)` | `%clear_null(i, e)` |

The two are not the same question on a vector column. **`%is_null(i)` answers "is any element of
row `i` null"**, while **`%set_null(i)` marks every element of the row** — a query summarises and a
mutation is total. On a scalar column, where `colwidth()` is 1, the distinction collapses.

Three bulk forms avoid a loop:

- **`%row_validity(valid)`** allocates and fills a rank-1 mask, one entry per row, by that same
  "any element null" rule. **`%row_validity_range(first, last, valid)`** fills a caller's own mask
  for a row range instead of allocating one.
- **`%element_validity(valid)`** allocates and fills a rank-2 `(width, nrows)` mask, one entry per
  element — the only form that is lossless on a vector column.
- **`%set_validity(mask)`** writes a whole mask back in one pass, taking either rank: rank-1 is a
  per-row mask, rank-2 a `(width, nrows)` per-element one.

**Shapes match on every paired API**: a rank-1 `values` goes with a rank-1 mask and a rank-2
`values` with a `(width, nrows)` mask. Nothing is widened for you.

**`%any_null()`** is the cheap whole-column question. `%validity_bytes()` reports what the bitmap
occupies, and `%compact_validity()` drops it when no null remains.

### Nulls and threads

The bitmap is allocated **lazily**, on the first null written — so the first `%set_null` from inside
a parallel region races with that allocation. `%ensure_validity()` materialises it up front:

```fortran
call col%ensure_validity()      ! now no allocation happens inside the region
!$omp parallel do
do i = 1, n
    if (bad(i)) call col%set_null(i)
end do
```

`%has_validity_storage()` reports whether that has happened yet. The same rule, and the same escape
hatch, apply to every container column on its own page.

## Growing, reserving and assembling

Storage **grows geometrically** (1.5x), so appending one row at a time is amortised O(1).
`%reserve(n)` presizes without adding rows, `%shrink_to_fit()` gives the slack back, and
`%capacity()` is how you see what either did. Both are no-ops on a column that was never appended
to, whose storage is already exact-fit.

Two ways to assemble a column out of pieces, and **the difference between them is what happens to
validity**:

- **`%append(src)`** appends a whole column of identical kind and width, and **merges** validity:
  the appended rows bring their own null state with them.
- **`%paste(src, at, [from], [count])`** overwrites an existing row range in place and **replaces**
  the pasted range's validity. This is the primitive for building a column of known final size out
  of chunks — `%init` it at full size, then paste each piece at its offset.

`%paste` is refused on the two string kinds and aborts saying so: a string column has no fixed-width
slots to overwrite, so it grows and appends instead. `%append_nulls(n)` appends `n` all-null rows.

## Row-set rebuilds

Four operations change which rows a column holds, each rebuilding it in one pass rather than
shifting repeatedly:

```fortran
call col%reindex(perm)                 ! reorder: row k becomes the old row perm(k)
call col%delete_by_mask(keep)          ! keep the rows whose mask entry is .true.
call col%gather(idx, [valid], [threads])            ! keep the listed rows, in the listed order
call dst%gather_from(src, idx, [valid], [threads])  ! build dst from src's listed rows
```

`%reindex` validates `perm` completely — length, range and no duplicates — before touching a buffer,
so a bad permutation aborts with the column intact. `%gather` checks only the range, deliberately:
its `idx` may name a row more than once and may be any length, which is what lets one call do
reordering, filtering and duplication at once, and lets the result be shorter or longer than the
column it replaces.

**`%gather_from(src, idx, [valid], [threads])`** is `%gather` from *another* column: `dst` is
cleared and rebuilt from `src`'s listed rows, carrying the source's kind, width, unit and nulls —
and `src` is never written. It holds one copy where a `%deep_copy` followed by a `%gather` would
hold two.

The optional `valid` mask on both has one entry per *destination* row and only ever **adds** nulls:
a `.true.` entry leaves the row exactly as the source had it, so a null already there survives. An
optional `threads=` names the team the rebuild may split the rows across — the route a
`parquet_table` takes when it spends its whole team inside one column, rather than resolving a
count per column.

All four **invalidate every pointer into the column** ([below](#zero-copy-pointers)); the row set
they describe no longer exists.

## Containers and string stores

A `parquet_column` reaches the container and string types by **taking ownership** of one, not by
`%init`:

- **`%adopt_container(cc)`** takes a `class(parquet_container_column), allocatable` — a
  [list](list-columns.html), [map](map-columns.html) or [struct](struct-columns.html) column — and
  leaves it deallocated. The column's kind becomes `PK_LIST`, `PK_MAP` or `PK_STRUCT`. `%init` on a
  container kind deliberately aborts: a row count means nothing until the payload type is known, and
  by then the caller already owns the object.
- **`%adopt_string_column(sc)`** does the same for a
  [`parquet_string_column`](string-columns.html), giving a `PK_STRING` column backed by that store.

Each container page documents which of the column's own operations reach through to the container
and which are refused — `%paste` is the notable refusal, since a container row's element count is
data rather than geometry.

## Zero-copy pointers

Three bindings hand back a pointer into the column's own storage rather than a copy:

```fortran
real(real64), pointer :: p(:)
call col%data_ptr(p)                  ! the values, in place
```

`%data_ptr` resolves on the pointer's declared type and kind, `%string_column(p)` points at an
embedded string store and `%container_ptr(p)` at an embedded container.

**A pointer is borrowed, and the column owns what it points at.** Anything that rebuilds the
storage invalidates it — every operation in [Row-set rebuilds](#row-set-rebuilds), `%clear`,
`%move_from`, and any append or `%reserve` that grows past the current capacity. Take the pointer
after the structural work, not before, and do not hold one across a call you did not write.

## Ownership and copying

- **`%deep_copy(out)`** produces a fully independent copy — values, validity and the unit string.
- **`%move_from(src)`** takes over another column's storage without copying and leaves `src` empty.

## The unit string

A column carries one optional unit string, set at `%init` or with `%set_unit(text)` and read back
with `%unit_string(out)`. It is data about the column rather than about any row, it survives
`%deep_copy` and `%gather_from`, and it is what a schema's `unit:` field becomes when a column is
read from a file. `%set_unit("")` clears it.

## Where a column goes next

A `parquet_column` is the currency the rest of the library trades in:

- **Into a file** — the writer takes one directly, so a column assembled at run time needs no
  compile-time type. See [Writing parquet files](../io/writing.html).
- **Into a table** — `parquet_table%add_column` takes one, and `%col` hands back a pointer to one
  already in a table. See [Whole tables in memory](../tables/table.html).
- **Into the sorting and statistics tiers** — a `parquet_column` is one of their element types,
  under two rules its element kind being a run-time fact imposes: it must be a **scalar** column,
  and its kind must be one of the nine the engine can order. Anything whose answer needs a
  compile-time element type is out, and anything whose answer is a permutation, a boolean or an
  index is in — so `pf_argsort` applies where `pf_sort` does not. See
  [Sorting](../utilities/sorting.html) and [Array statistics](../utilities/statistics.html), whose
  tables give the per-operation detail.
