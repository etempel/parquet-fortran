---
title: Map columns with parquet_map_column
---

`parquet_map` stores a column whose every row holds **zero or more `key -> value` entries**. Keys
are strings; values are one of the nine scalar kinds, fixed for the whole column.
`map<string, int32>` is one column in which every row carries its own little dictionary. That is
what the Parquet `MAP` logical type describes.

A map column can be **read from a Parquet file** with `parquet_read_column` and **written back**
with `parquet_write_column`, whole or one row group at a time — see
[Reading a map column from a file](#reading-a-map-column-from-a-file) and
[Writing a map column to a file](#writing-a-map-column-to-a-file) below.

The module is Arrow-free: `use parquet_map` compiles eleven Fortran files and reaches no C++
binding, so a program that builds map-shaped data in memory and never touches a file pays for none
of the reader or writer. It is also re-exported by `use parquet`, so a program that does touch
files needs no second import.

Signatures below are written with **optional arguments in square brackets** — `%get(key, value,
[is_valid], [occurrence], [warn], [found])` means only `key` and `value` are required. The brackets
are notation, not Fortran: they never appear in code you write.

## A first example

```fortran
use parquet_map
type(parquet_map_column), target :: mc
type(parquet_map_row) :: row
integer(int32) :: age
logical :: found

call mc%init(PK_INT32)
call mc%append_row(["age   ", "height"], [42_int32, 178_int32])
call mc%append_null_row()
call mc%append_empty_row()

row = mc%view(1_int64)
call row%get("age", age)                    !  42
call row%get("weight", age, found=found)    !  found is .false.; age must not be used
print *, row%size(), row%contains_key("age")
print *, mc%is_null(2_int64), mc%is_null(3_int64)   ! .true.  .false.
```

**The column must be declared `target`.** `%view` hands back a handle holding a pointer into it,
and F2018 15.5.2.4 leaves that pointer undefined on return if the actual argument has no `TARGET`
attribute. gfortran, ifx and flang all appear to work without it; `nagfor`'s `-C=dangling` reports
it. The same contract applies to `parquet_string_column%view` and `parquet_list_column%view`.

## How a map column is stored

Three pieces, and knowing them explains every performance note below:

- **`offsets`** — `nrows+1` entries, `offsets(1) == 0`. Row `i` holds entries
  `offsets(i)+1 .. offsets(i+1)`. This is Arrow's own convention.
- **two flattened `parquet_column`s** — every row's keys in one, every row's values in the other,
  index-aligned. The value column carries the value kind, the per-value nulls and the unit string,
  so none of that is duplicated.
- **a row bitmap** — one bit per row, allocated only once a row is actually nulled.

Physically this is a `LIST` of `struct<key, value>`, which is exactly what Parquet's `MAP` logical
type is. In the written file the two leaves are `<column>.key_value.key` and
`<column>.key_value.value`.

## Duplicate keys are preserved, and so is order

Neither Arrow nor Parquet requires a map's keys to be unique, and this library adds no check the
format does not make. A row appended with `["a", "b", "a"]` reads back with those three entries in
that order.

That is what the four lookup forms are for:

- **`%get(key, value, ...)`** returns the **first** match.
- **`%get(key, value, occurrence=n)`** returns the *n*-th match, 1-based.
- **`%key_count(key)`** says how many entries carry that key — ask this before choosing an
  occurrence.
- **`%contains_key(key)`** is the `%key_count(key) > 0` shorthand.

**Lookup is a linear scan of the row, not an index.** `%key_count` and `occurrence=` both need the
whole row walked anyway, so an index would accelerate only the single-occurrence case while adding
state that a missed invalidation would turn into a wrong answer. Rows are small in practice. When
you want *every* entry of a row rather than one, walk it positionally instead of looking each key
up by name:

```fortran
integer :: p
character(len=:), allocatable :: k
integer(int32) :: v

do p = 1, row%size()
    call row%key_at(p, k)
    call row%get_at(p, v)
    print *, k, " -> ", v
end do
```

**Within-row positions and counts are a plain `integer`.** Arrow addresses a map's entries with an
int32 offsets buffer, so no row can hold more entries than an int32 can index — see
[The entry-count ceiling](#the-entry-count-ceiling) below. Row *indices* are `integer(int64)` and
accept either kind, as everywhere else in the library.

## Null rows, empty rows and null values

A map column has exactly **two** null levels, and the pair that gets conflated is not one of them:

- **A null ROW** — this map is absent. `%is_null(i)` is `.true.`.
- **A null VALUE** — the map is present, an entry exists, and its value is missing. `%get`'s
  `is_valid` argument reports it.

**A null row and a present-but-EMPTY row both report `%size() == 0`, and only `%is_null` separates
them.** `%append_null_row()` makes the first; `%append_empty_row()` makes the second.

**A key is never null.** Arrow's `MapType` declares its key field non-nullable and offers no way to
change that, so there is no third level and no `%key_is_null`. An *empty* key string is a
legitimate key and is not a null one.

## When a lookup fails

By default a missing key, an `occurrence` beyond `%key_count`, or a position outside the row is a
hard `error stop`. Two optional arguments change that, and they follow the same convention on
`%get`, `%get_at` and `%key_at`:

- **`warn=.true.`** emits one warning and returns.
- **`found`** receives `.false.` and the call returns silently.
- **With neither**, the call aborts — otherwise a caller would have no way to learn that the lookup
  failed at all.

**On a failed lookup `value` is left at the type's default and must not be used.** A string result
comes back as `""` rather than unallocated, because an unallocated result is indistinguishable from
one the callee never reached.

**A wrong VALUE KIND is a different class and always aborts**, with no soft option: calling the
`int32` form on a `map<string,string>` column is a type mismatch, not a lookup failure, exactly as
reading any other column as the wrong type is.

## The value kind is fixed at `%init`

`%init(PK_INT32)` makes a `map<string,int32>` column, and `%append_row` with any other value type
aborts. Inferring the kind from the first row appended instead would type the column from one
element, and would leave `%element_kind()` unable to answer for an empty column — which a metadata
query has to be able to do before any row exists.

`%init` also takes an optional row count (creating that many null rows) and an optional `unit`
string for the values.

## Growing, copying and reordering

`%append_row`, `%append_null_row` and `%append_empty_row` grow the column geometrically, so
building one row at a time is amortised O(1). `%reserve(n)` presizes it and `%shrink_to_fit`
releases the slack.

`%deep_copy` produces a fully independent copy; `%move_from` takes over another column's storage
and leaves it empty. `%gather_rows(idx)` rebuilds the column so row *k* becomes the row that was at
`idx(k)` — which serves reordering, filtering and duplication at once, and drops the entries of any
row the permutation does not name.

`%set_null(i)` is O(1): the row's entries are not removed and no later row moves, so every
outstanding handle's index stays correct. The nulled row reports `%size() == 0` and finds no key
from that moment on; its former entries are dropped by the next `%gather_rows`.

## Putting a map column into a `parquet_column`

`parquet_column%adopt_container` takes ownership of a map column, so one can travel anywhere a
`parquet_column` can:

```fortran
type(parquet_column) :: col
call col%adopt_container(mc)     ! mc is left empty
print *, col%kindof() == PK_MAP
```

## Threading

Validity storage is allocated lazily, so the *first* null in a column is what allocates the row
bitmap. Several threads nulling rows of one column concurrently would race on that allocation.
`%ensure_validity()` is the escape hatch: call it before the parallel region and both levels that
can be null are materialized up front. (The keys column is untouched — a key is never null.)

## Reading a map column from a file

```fortran
type(parquet_reader) :: reader
type(parquet_map_column), target :: mc

call parquet_open_reader(reader, "people.parquet")
call parquet_read_column(reader, "attrs", mc)
call parquet_close_reader(reader)
```

The value kind, the entry counts and both null levels all come from the file; nothing is declared
at the call site. `parquet_read_column_chunk(reader, "attrs", rg, mc)` reads exactly row group `rg`
instead, and — like every other row-group-scoped operation — refuses while a read-time sort is
installed, since a permutation destroys row-group locality. Filtering, sampling and sorting compose
with a whole-column map read exactly as with any other.

`parquet_get_column_shape` answers `"map"` for such a column, from the schema alone. Note that
`parquet_get_column_type` answers `"unknown"` for it, exactly as it does for a struct column: a
container has no single element type a scalar read could target, and the shape query is how a
caller discovers what it is dealing with.

**V1 keys must be strings.** A file whose map is keyed by anything else is refused, naming the
actual key type, rather than having its keys rendered as text — that would silently change the
data, since `1`, `01` and `1.0` are three different keys. A map whose *value* is itself a list, map
or struct is refused for the same reason a list of lists is: that is nesting, and it is not
supported yet.

## Writing a map column to a file

```fortran
type(parquet_writer) :: writer
call parquet_open_writer(writer, "out.parquet", schema)
call parquet_write_column(writer, "attrs", mc)
call parquet_close_writer(writer)
```

On a schema-enforced writer the column must be declared **`map[<valuetype>]`** — `map[int32]`,
`map[string]`, `map[timestamp[ms,utc]]`. The value type must match exactly; there is no widening
between map value kinds, exactly as there is none between list element kinds.

**The token carries no key type**, because v1 keys are always strings: `map[int32]` means
`map<string,int32>`. A bare `map` is not a valid declaration — unlike the bare `struct` token,
which is valid because a struct's field layout genuinely cannot be expressed in MAML while a map's
value type is a single token the schema can perfectly well carry.

`parquet_write_column_chunk` writes one row group at a time. A protected column (`protected_cols:`)
may hold neither a null row nor a null value, and the abort names which level failed.

## The entry-count ceiling

**A map column's total entry count is bounded by `huge(int32)`, and unlike every other
variable-length thing in this library there is no way around it.** Arrow addresses a map's entries
with an int32 offsets buffer and provides no `large_map` type. A string column that will not fit
one is written as `large_utf8` and a list column as `large_list`; a map has nowhere to widen into,
so a column that does not fit is **refused** rather than written in a wider form. Split it across
more row groups.

## What this module does not do yet

- **A map column cannot be held by a `parquet_table`.** It travels through `parquet_read_column` /
  `parquet_write_column` and inside a `parquet_column`, but the table layer does not yet classify
  one.
- **A value cannot itself be a container.** `map<string, list<int32>>`,
  `map<string, struct<...>>` and `map<string, map<...>>` are all refused, naming the value type.
- **Keys must be strings.** Reading a map keyed by anything else is a clean refusal.
- **A map reached through a dotted struct path** (`profile.attrs`) is not addressable.
- **`qc:` bounds and `parquet_filter` rules** stay scalar-leaf-only, permanently. `qc: miss:` does
  apply, to row nullness.
