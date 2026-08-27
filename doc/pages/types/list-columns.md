---
title: Variable-length list columns with parquet_list_column
---

`parquet_list` stores a column whose rows hold **different numbers of values**. Row 1 may hold
three integers, row 2 one, row 3 none at all, and row 4 may be a null list — absent rather than
empty. That is what the Parquet `LIST` logical type describes, and it is the one shape this
library's other column types cannot represent: a `*_VEC` column has a fixed `col_size` and every
row is exactly that wide.

A list column can be **read from a Parquet file** with `parquet_read_column` and **written back**
with `parquet_write_column`, whole or one row group at a time — see
[Reading a list column from a file](#reading-a-list-column-from-a-file) and
[Writing a list column to a file](#writing-a-list-column-to-a-file) below. Everything else on this
page is about building, filling, querying, copying and mutating a list column in memory, and
handing it to a `parquet_column`.

The module itself is **independent**: a program whose only import is `use parquet_list` compiles
eleven Fortran files and never reaches this library's C++ bindings (the *package* still links
Arrow; see [Choosing a module](../operating/choosing-a-module.html)). It reads no settings and
prints nothing. Reading from a file naturally does reach them, and goes through `use parquet` (or
`use parquet_io`) like every other read.

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
- **`%adopt_rows(offsets, payload, [row_valid])`** is the bulk counterpart: it takes over a whole
  column at once, moving in an offsets array and a filled `parquet_column` of elements and deriving
  the payload kind and the row count from them. Reach for it when the final row and element counts
  are known before the values are — a file read is the worked example — and for a string payload,
  where `%append_row`'s `character(len=*)` array form would trim trailing blanks. Every
  precondition is checked and fatal.
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

## Reading a list column from a file

`parquet_read_column` reads a whole `LIST` column into a `parquet_list_column`, and
`parquet_read_column_chunk` reads one row group of it:

```fortran
use parquet
type(parquet_reader) :: r
type(parquet_list_column), target :: lc
type(parquet_list_row) :: row
integer(int32), allocatable :: v(:)

call parquet_open_reader(r, "spectra.parquet")
call parquet_read_column(r, "flux", lc)          ! one call; nothing declared about the column
call parquet_close_reader(r)

row = lc%view(3_int64)
call row%get(v)                                   ! v is sized to row 3, whatever its length
```

**You do not declare the payload kind — the file does.** `lc` is cleared and rebuilt on every
read, and its `%element_kind()` comes from the column's own element type, mapped to the narrowest
Fortran kind that holds it losslessly: a `list<int8>`, a `list<int16>` and a `list<int32>` all read
into a `PK_INT32` payload, while a `list<uint32>` reads into `PK_INT64` because uint32's upper half
does not fit a signed 32-bit integer. This is the same mapping
[`parquet_get_column_type`](../io/reading.html) reports, and reading a column into a `lc` that
already held something else simply replaces it.

Both null levels survive the read, and they stay distinct:

| in the file | after the read |
|---|---|
| a **null** list row | `lc%is_null(i)` is `.true.`, `lc%length(i)` is 0 |
| a present but **empty** row | `lc%is_null(i)` is `.false.`, `lc%length(i)` is 0 |
| a **null element** in a present row | the row is not null; `row%get(v, is_valid=ok)` has `ok(e)` `.false.` |

There is no `null_value=`/`is_valid=` argument on this specific — the destination carries its own
validity, exactly as it does for
[`parquet_string_column`](string-columns.html).

**Filtering, sampling and sorting compose with it** exactly as with any other read: open the reader
with `filter=`, `sample_fraction=` or `sort_by=` and the list column receives the surviving rows,
in the requested order, each with its own length.

**The chunked form is row-group-scoped**, so `parquet_read_column_chunk(r, name, rg, lc)` fills
`lc` with just that row group's rows. Like every other row-group-scoped operation it **refuses
while a read-time sort is installed**, because a sort permutation destroys row-group locality —
read the column whole in that case. A filter is fine, since a filter only ever removes rows.

### Which columns can be read this way

Any Parquet `LIST` or `LARGE_LIST` column whose elements are one of the nine payload kinds, plus a
`FIXED_SIZE_LIST` (this library's own vector-column layout), whose rows then all happen to have the
same length. A list under a `STRUCT` is addressable by its dotted path (`"nested.vals"`) like any
other leaf.

A column whose elements are themselves a container — a `list<list<...>>`, a `list<struct<...>>`, a
`list<map<...>>` — is refused with a message naming the element type. Nesting arrives in a later
release.

**A list column and a vector column are two readings of the same file column, and the caller's
chosen output type picks which.** A `LIST` column whose rows happen to be uniformly 3 long reads
into a `parquet_list_column` *and* into a 2-D `integer(int32) :: v(3, nrows)` array, and neither
path knows about the other. The asymmetry is deliberate: a genuinely **ragged** column can only be
read as a list column, because no 2-D array can hold rows of differing lengths — but a **uniform**
one is never refused as a list column, because uniformity is a property of the data rather than of
the request, and the same column may be ragged in next month's file.

[`parquet_get_column_shape`](../io/reading.html) answers which kind of container a column is
(`"scalar"`, `"vector"`, `"list"`, `"map"`, `"struct"`), from the schema alone.

## Writing a list column to a file

`parquet_write_column` writes a whole `parquet_list_column` as a genuine variable-length `LIST`
column, and `parquet_write_column_chunk` writes one row group of it:

```fortran
use parquet
type(parquet_writer) :: w
type(parquet_list_column) :: lc

call lc%init(PK_INT32)
call lc%append_row([1_int32, 2_int32, 3_int32])
call lc%append_null_row()
call lc%append_row([integer(int32) ::])    ! present but empty -- not the same thing
call lc%append_row([7_int32])

call parquet_open_writer(w, "ragged.parquet")
call parquet_write_column(w, "flux", lc)
call parquet_close_writer(w)
```

**There is no `is_valid=` argument**, for the same reason there is none on the read: the column
carries its own nullness at both levels. A null row is one you appended with `%append_null_row` or
marked with `%set_null(i)`; a null element is one you passed `is_valid=` to `%append_row` for.
Both survive the write, and both come back distinct — the table in
[Reading a list column from a file](#reading-a-list-column-from-a-file) reads the same in either
direction.

**A list column is always written as a `LIST`, never as a vector**, even when every row happens to
hold the same number of elements. The type you pass decides the column's physical shape: a
`parquet_list_column` writes a variable-length list and a 2-D array writes a fixed-width
`FIXED_SIZE_LIST`, and neither is substituted for the other. That is the deliberate mirror image of
the reading rule above, where a file's column may be read *either* way — a reader is interpreting a
file it did not write, so every lossless reading is allowed; a writer is choosing what the file
will be, so what was asked for is what is written. `parquet_get_column_shape` on the result answers
`"list"` in both cases.

**A whole-file row mask applies.** `parquet_write_row_mask` drops whole rows before the column is
written, and the surviving rows keep their own lengths, their nullness and their elements — the
dropped rows' elements go with them.

### Declaring one in a schema

A schema-enforced writer declares a list column with the `list[<elemtype>]` data type, where
`<elemtype>` is one of the nine payload kinds:

```
fields:
  - name: flux
    data_type: list[float32]
  - name: observed
    data_type: list[timestamp[ms,utc]]
```

The element type is **required** — a bare `list` is rejected — and it must match the column's
payload kind **exactly**: there is no widening between list element kinds the way there is between
scalar numeric kinds, so a `PK_INT32` column cannot be written into a `list[int64]` declaration.

`col_size:` does not apply and is rejected: a list row's length comes from the data. `qc: min:`/
`max:` is rejected too, on the same terms as for a `date`/`time`/`timestamp` column; `qc: miss:`
does apply, and counts **null rows**. `extra: protected_cols:` applies and means the column holds
no Null at *either* level — neither a null row nor a null element — which is also what makes a
streamed list column's fields non-nullable.

A declared list column that is never written is written with zero rows when the writer closes, like
any other declared column.

### Very large list columns

Parquet's own repetition/definition-level generation addresses a row group's flattened elements
with a 32-bit counter, so **one row group may hold at most 2,147,483,647 list elements**. This is
handled for you: `parquet_close_writer`'s automatic row-group sizing accounts for the column's
longest row and picks a smaller row group when it needs to, so a column's *total* element count can
exceed that freely. Only two cases abort rather than being silently resized — an explicitly chosen
`chunk_size` that would overflow a row group, and a `parquet_new_row_group` whose rows do — because
both are values the caller chose and neither should be silently overridden. A **single row** longer
than the limit aborts too, since no row-group size can help: a row is never split across row groups.

## In a `parquet_table`

A table column can be a `parquet_list_column`. See
[Container columns in a table](../tables/table.html) for the five accessors it gets
(`%col`, `%get`, `%set`, `%add_column` and a handle's `%ref`), the two element-granular queries it
declines, and how row-structural mutations carry it along.

Open the file with `list_columns="container"` to have every variable-length `LIST` column
classified as one, from the schema alone -- see
[Opening a table](../tables/table-open.html).

## What this module does not do yet

- **No nesting.** A list of lists, a list of structs and a list of maps are not available; the
  payload must be one of the nine scalar kinds. `STRUCT` columns have their own type — see
  [Struct columns with `parquet_struct_column`](struct-columns.html) — but a struct cannot be a
  list's payload and a list cannot be a struct's field. `MAP` is not available at all yet.
- **No `%append` between container columns**, and no `%paste` at all — see above.
