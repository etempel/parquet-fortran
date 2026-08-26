---
title: Variable-length list columns with parquet_list_column
---

`parquet_list` stores a column whose rows hold **different numbers of values**. Row 1 may hold
three integers, row 2 one, row 3 none at all, and row 4 may be a null list — absent rather than
empty. That is what the Parquet `LIST` logical type describes, and it is the one shape this
library's other column types cannot represent: a `*_VEC` column has a fixed `col_size` and every
row is exactly that wide.

**This page describes in-memory storage only.** Nothing here reads a list column from a Parquet
file or writes one to a file yet; that arrives in a later release. What you can do today is build,
fill, query, copy and mutate a list column in memory, and hand it to a `parquet_column`.

It is an **independent module**: a program whose only import is `use parquet_list` compiles eleven
Fortran files and never reaches this library's C++ bindings (the *package* still links Arrow; see
[Choosing a module](../operating/choosing-a-module.html)). It reads no settings and prints nothing.

## A first example

```fortran
use parquet_list
type(parquet_list_column), target :: lc
type(parquet_list_row) :: row
integer(int32), allocatable :: v(:)
integer(int64) :: i

call lc%init(PK_INT32)                            ! the payload kind is fixed here
call lc%append_row([1_int32, 2_int32, 3_int32])   ! a three-element row
call lc%append_row([7_int32])                     ! a one-element row
call lc%append_null_row()                         ! an absent list

do i = 1_int64, lc%size()
    row = lc%view(i)
    call row%get(v)                               ! v is sized to this row
    print *, i, row%length(), row%is_null(), v
end do
```

Two things in that example are load-bearing and are covered below: the column is declared
`target`, and the payload kind is given to `%init` rather than inferred.

## How a list column is stored

One `offsets` array and one flattened payload column:

```
   rows:      [ 1, 2, 3 ]   [ 7 ]   (null)   [ 10, 20 ]

   offsets:   0        3       4       4         6
              |        |       |       |         |
   payload:   1  2  3  7                10  20
```

`offsets` has `nrows+1` entries with `offsets(1) == 0`, and row `i` occupies payload elements
`offsets(i)+1 .. offsets(i+1)`. That is Arrow's own convention and the one
[`parquet_string_column`](string-columns.html) already uses.

**The payload is one ordinary `parquet_column`.** Everything a column already knows how to do — every
supported element kind, per-element nulls, the unit string, geometric growth — comes from there
rather than being written again. It also means a list column costs one payload column plus an
offsets array, whatever its rows look like.

## The payload kind is fixed at `%init`

`%init(payload_kind [, nrows] [, unit])` requires the payload kind and it cannot change afterwards.
Appending values of any other type aborts, naming both kinds.

```fortran
call lc%init(PK_FLOAT64)
call lc%append_row([1_int32])     ! aborts: this is a float64 list column
```

The nine supported payload kinds are the scalar value kinds: `PK_INT32`, `PK_INT64`, `PK_FLOAT32`,
`PK_FLOAT64`, `PK_LOGICAL`, `PK_STRING`, `PK_DATE`, `PK_TIME` and `PK_TIMESTAMP`. A `*_VEC` payload
is refused, because a fixed-width vector inside a variable-length list is a *nested* type rather
than a wider payload.

Requiring the kind up front is what lets `%element_kind()` answer for a column that has no rows
yet, which a schema query needs to be able to do.

The optional `nrows` creates that many **null** rows, matching `parquet_column%init`. The optional
`unit` is passed to the payload column, which is where a list column's unit lives — there is one
unit, describing the values, not one per level.

## Null rows and null elements are different things

A list column carries **two** kinds of nullness, and they answer different questions:

- **The row is null** — the list is absent. `%is_null(i)` answers this, `%append_null_row()` and
  `%set_null(i)` produce it.
- **An element inside the row is null** — the list is present and one of its values is missing.
  The `is_valid` argument of `%append_row` writes this, and the `is_valid` argument of `%get` reads
  it back.

A row of three elements one of which is null is **not** a null row:

```fortran
call lc%append_row([1_int32, 2_int32, 3_int32], is_valid=[.true., .false., .true.])
! lc%is_null(1) is .false. -- the list is present
! lc%length(1)  is 3       -- the null element still occupies a slot
call row%get(v, is_valid=ok)   ! ok == [T, F, T]
```

**A null row and an empty row are also different**, and both are representable. `%is_empty(i)` is
`.true.` for either, matching the way a null string reads as empty in
[`parquet_string_column`](string-columns.html); `%is_null(i)` is what separates them.

| row | `%is_null` | `%is_empty` | `%length` | `%get` |
|---|---|---|---|---|
| `[1, 2]` | `.false.` | `.false.` | 2 | two values |
| `[]` (present, empty) | `.false.` | `.true.` | 0 | zero-size |
| null (absent) | `.true.` | `.true.` | 0 | zero-size |

Reading a null row is not an error — nulls are ordinary in a list column, so `%get` yields a
zero-size array rather than aborting.

**`%set_null(i)` does not move any data.** It sets one bit; the row's elements stay where they are
and no later row shifts, which is what keeps it O(1) and keeps every outstanding handle correct.
The nulled row reports length 0 and yields a zero-size `%get` from that moment on, and its former
elements are unreachable. They are dropped by the next rebuild (`%gather_rows`). `%clear_null(i)`
makes the row present again with whatever its offsets still describe — which is nothing for a row
that was appended null, and its original elements for a row nulled afterwards, provided no rebuild
has happened in between.

## The row handle

`%view(i)` returns a `parquet_list_row`: a borrowed pointer plus a row index, resolved lazily. It
stays valid across appends and reserves of the column it refers to, and is invalidated by anything
that changes the row set (`%clear`, `%move_from`, a `%gather_rows` rebuild) or by the column going
out of scope.

**The column must be declared `target`.** This is not a style preference: Fortran leaves the
handle's stored pointer *undefined* on return when the actual argument has no `TARGET` attribute
(F2018 15.5.2.4), and most compilers run the non-conforming form perfectly happily until one day
they do not.

```fortran
type(parquet_list_column), target :: lc     ! `target` is required
```

`%get` fans out over the nine payload kinds and writes into an allocatable array sized to that
row. For a string payload the result is as wide as the longest value **in that row**, not the
widest in the column:

```fortran
character(len=:), allocatable :: sv(:)
call row%get(sv)
```

`%is_valid()` reports whether the handle refers to a live row; a default-constructed handle
answers `.false.` and every other accessor on it aborts rather than returning a plausible answer.

## Growing, copying and reordering

- **`%append_row` grows geometrically** (1.5x, like every other container here), so building a
  column one row at a time is amortised O(1) per row. `%reserve(n)` presizes without adding rows;
  `%shrink_to_fit()` gives back the slack.
- **`%deep_copy(out)`** produces a fully independent copy — offsets, row nullness, and the payload
  with its own per-element nullness. **`%move_from(src)`** hands the storage over without copying
  and leaves `src` empty.
- **`%gather_rows(idx)`** rebuilds the column so row `k` becomes the row that was at `idx(k)`. The
  list may be any length, may repeat a row and may omit one, so it serves reordering, filtering and
  duplication alike. It is also what compacts away the elements of rows that were nulled after
  being appended.
- **`%validate([message])`** checks the class invariants — offsets monotonic, `offsets(1) == 0`,
  the final offset matching the payload's row count — and is cheap enough to call from a test after
  any structural change.
- **`%summary(out)`** composes a one-line description such as `list<int32>: 4 rows, 6 elements,
  1 null`, and `%kind_text(out)` gives just `list<int32>`. Both write into a string rather than
  printing, so the caller decides where it goes.

## Putting a list column into a `parquet_column`

A `parquet_column` can hold a list column, which is how the container reaches the rest of the
library. The entry point is `%adopt_container`, not `%init`:

```fortran
type(parquet_column) :: col
class(parquet_container_column), allocatable :: cc

call lc%clone_into(cc)          ! or build the container and move_alloc it
call col%adopt_container(cc)    ! cc is left deallocated -- this MOVES
! col%kindof() is now PK_LIST, col%length() is the container's row count
```

`%init(PK_LIST, ...)` deliberately aborts. `init` allocates a kind's storage from a row count, and
"a hundred rows of a list column" has no meaning until the payload type is known — and by then the
caller already owns the object, so moving it in is both cheaper and unambiguous. Like the array
`%adopt` forms, `%adopt_container` takes ownership: the container is left deallocated and must not
be used again.

Once adopted, the column's ordinary operations reach the container: `%length()`, `%is_null(i)`,
`%set_null(i)`, `%clear_null(i)`, `%append_nulls(n)`, `%deep_copy`, `%ensure_validity()` and the
row-structural rebuilds all work. Two do not, and say so rather than doing something surprising:
`%paste` is refused (a container row's element count is data, so overwriting one in place would
move every following row), and `%append` of one container column onto another is not implemented
yet.

## Threading

The module is thread-safe in the same sense the rest of the library is: separate columns in
separate threads are fine, and one column must not be mutated from two threads at once.

Both null levels allocate **lazily** — the row bitmap here, and the payload's own element bitmap —
so the *first* null written from inside a parallel region races with that allocation.
`%ensure_validity()` materializes both up front, before the region:

```fortran
call lc%ensure_validity()     ! now no allocation happens inside the region
```

## What this module does not do yet

- **No file I/O.** A list column cannot yet be read from or written to a Parquet file.
- **No nesting.** A list of lists, a list of structs, and the `MAP`/`STRUCT` containers themselves
  are not available; the payload must be one of the nine scalar kinds.
- **No `parquet_table` integration.** A table cannot yet hold a list column.
- **No `%append` between container columns**, and no `%paste` at all — see above.
