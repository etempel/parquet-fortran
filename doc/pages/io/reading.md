---
title: Reading parquet files from your Fortran code
---

This page is about reading a parquet file one column at a time: opening a `parquet_reader`,
pulling whole columns into Fortran arrays, reaching a single row or a single element position
without reading the rest, asking a column its shape or type, and streaming a file too large to
hold in memory. It is the level to work at when you want particular columns and want to control
when each one is read.

If what you actually want is *the whole file* — every column available by name, read only when you
touch it — start at [Whole tables in memory](../tables/index.html) instead: one
`parquet_open_table` call replaces most of this page. Choosing which *rows* a read returns, and in
what order, is [Filtering, sorting and sampling rows](filter-sort-sample.html).

To use this library in another Fortran project, add it as an FPM dependency — see
[Minimal setup to depend on this library](../../index.html#minimal-setup-to-depend-on-this-library)
in the README for the `fpm.toml` snippet. Then `use parquet` in your code.

Minimal reader example:

```fortran
program read_parquet_example
    use parquet
    use iso_fortran_env, only: int32, int64
    implicit none

    type(parquet_reader) :: reader
    integer(int64) :: nrows
    integer(int32), allocatable :: id(:)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_get_nrows(reader, nrows)

    allocate(id(nrows))
    call parquet_read_column(reader, "id", id)

    call parquet_close_reader(reader)
end program read_parquet_example
```

Notes:

- `parquet_get_nrows` returns the number of table rows.
- Allocate output arrays before calling `parquet_read_column`. Its row count (`size(values)` for a
  plain column, `size(values, 2)` for a vector column) must match `parquet_get_nrows` exactly, or
  it fails immediately with `error stop`, naming the column and both row counts.
- `name` must be a column that actually exists in the file. Every procedure that takes a column
  name checks this and fails immediately with `error stop` naming the missing column —
  `parquet_read_column`, `parquet_read_column_chunk`, `parquet_get_col_size`,
  `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_get_column_type`,
  `parquet_get_column_arrow_type`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`
  and `parquet_prefetch_columns` among them. The one deliberate exception is `parquet_column_exists`, whose whole job is to
  answer that question — it returns `.false.` instead of aborting.
- For string columns, choose a fixed string length that is large enough for your data — or read
  into a `type(parquet_string_column)` instead, which needs no pre-sizing; see
  [Reading and writing compact string columns](../types/string-columns.html#reading-and-writing-compact-string-columns).
  Not sure how long is long enough? `parquet_get_string_length` answers that directly — see
  [Column size queries](#column-size-queries) below.
- For vector columns, allocate 2D arrays with shape `(col_size, nrows)`.
- A `DATE`/`TIME`/`TIMESTAMP` column reads into a
  `parquet_date`/`parquet_time`/`parquet_timestamp` array instead — see
  [Date, time and timestamp columns](../types/date-time.html); these three carry their own null
  state, so `null_value=`/`is_valid=` don't apply to them.
- A vector column may be stored on disk either as a `fixed_size_list` (what this library's own
  writer emits) or as a variable-length `list<element>` (the Parquet `LIST` layout many other
  producers use — including its legacy 2-level and non-standard inner-element-name variants, which
  Arrow's reader normalizes to the same `list` type); both are read back identically. The only
  requirement is that every row's vector has the same length (so it fits the `(col_size, nrows)`
  shape); a genuinely ragged `list` column (rows of differing length) is rejected with
  `error stop`. `col_size` is inferred from the data in either case.

## The `nrows=` shortcut

`parquet_open_reader(..., nrows=nrows)` is a shortcut for the `parquet_open_reader` +
`parquet_get_nrows` pair above: pass an `integer(int32)` or `integer(int64)` variable (a plain
default `INTEGER` works too, on the vast majority of platforms where that's the same kind as
`int32`) as `nrows` and it's filled in for you, equivalent to calling
`parquet_get_nrows(reader, nrows, check_positive=.true.)` immediately after opening (post-filter,
if a `filter` was also given).

Because it implies `check_positive=.true.`, a file (or filter result) with **zero rows fails
immediately with `error stop`** — it does not return `nrows=0`. This is the right choice when your
code has no sensible zero-row behavior and would rather abort loudly than proceed with an empty
read.

If zero rows is a case you need to detect and handle rather than treat as a hard error, don't pass
`nrows=` to `parquet_open_reader` — open the reader as usual and call
`parquet_get_nrows(reader, nrows)` yourself afterwards, without `check_positive`, which returns
`nrows=0` instead of aborting, exactly like the code example above.

As with a direct `parquet_get_nrows(reader, nrows)` call, an `integer(int32)` (or
default-`INTEGER`) `nrows` also fails immediately with `error stop` if the actual row count
overflows `int32`'s range, rather than silently wrapping or truncating.

## Reading only touches the columns you ask for

`parquet_open_reader` only parses the file's footer (schema, row count, row-group layout) — it
does not read or decompress any column's data. Each column is read from disk only the first time
you ask for it (`parquet_read_column`, `parquet_get_string_length`, etc.), then cached for the
lifetime of that `reader`; asking for it again doesn't re-read it, and columns you never ask for
are never read at all.

This follows from Parquet's layout — each column is its own contiguous byte range, so the reader
seeks straight to just the bytes it needs, regardless of
[compression codec](writing.html#writer-options). So opening a large file with many columns and
reading only a handful is cheap in both I/O and memory, no matter how large the unrequested
columns are.

`parquet_get_col_size`/`parquet_get_column_total_elements` are cheaper still for the common case
(a vector column stored as `fixed_size_list`): they answer straight from the schema/footer,
reading no column data at all — so they're safe to call even on a column whose total element count
(`nrows * col_size`) is enormous. See
[Column size queries](#column-size-queries) for the one column layout where
they do have to read.

`parquet_read_array_row_mode` reads only the one row group the requested row lives in, not the
whole column. `parquet_read_array_element_mode` can't limit itself to one row group the way
`parquet_read_array_row_mode` does — it inherently needs every row's value at the same element
position, so every row group contributes — but it still avoids ever materializing the whole
column's flattened element count in a single internal call, by streaming the file row group by row
group instead.

A [row filter](filter-sort-sample.html#row-filtering-with-parquet_filter) or
[`sample_fraction`](filter-sort-sample.html#random-downsampling-with-sample_fraction) doesn't
change either of these: `row_index`/`elem_index` then address the filtered result, and each row
group's *surviving* row count is what they are resolved against. (Chunked reads work per row group
on a filtered reader too — see [Streaming/chunked reads](#streamingchunked-reads).)

### Releasing a column you have finished with

A cached column stays in memory until the reader is closed. `parquet_release_column(reader, name)`
frees that one column's decoded Arrow buffers once you have copied what you wanted into your own
Fortran storage — the way to read a sequence of large columns without holding all of them at once:

```fortran
call parquet_read_column(reader, "big", values)
! ... use `values`, which is your own array and unaffected by what follows ...
call parquet_release_column(reader, "big")
```

It is purely a memory/time trade and never changes an answer: asking for the column again simply
reads and decodes it from disk again, with any active `filter=`/`sample_fraction=` re-applied. A
name that doesn't exist, or a column that was never read, is a silent no-op — so it is safe in a
loop over column names.

One trap: `name` may be a dotted struct-leaf path, but the reader caches a struct as **one**
array, so releasing any leaf frees the whole struct. When walking several leaves of the same
struct, release only after the last of them, or each leaf re-reads the struct.

A released column still appears in a
[`print_stat`](#printing-reader-statistics-with-parquet_close_reader-print_stattrue) report, with
its value-derived cells marked `released`.

### Listing a file's columns

`parquet_get_column_names(reader, names)` hands back every top-level column name in schema order,
in an allocatable `character` array sized to the longest name and blank-padded — so `trim()` is
how you use them, the same convention `parquet_get_metadata_items` follows:

```fortran
character(len=:), allocatable :: names(:)
integer :: i

call parquet_get_column_names(reader, names)
do i = 1, size(names)
    print *, trim(names(i))
end do
```

It reads no column data: the answer comes from the schema parsed when the reader was opened. Names
come back as the file spells them, so a struct column appears once, under its own top-level name,
rather than as one entry per leaf.

## Random access: row mode and element mode

`parquet_read_array_row_mode` and `parquet_read_array_element_mode` read a **vector**
(`col_size > 1`) column without materializing the whole thing — the random-access counterparts to
`parquet_read_column`, for when you only need one row, or one fixed element position across every
row, out of a column that may be far larger. (In the call forms written out below, an argument in
square brackets is optional; brackets are notation for this page, never something you type.)

- **Row mode** —
  `parquet_read_array_row_mode(reader, name, values, row_index [, null_value] [, is_valid])` reads
  one row's entire element vector. `values` is a 1D array of length `col_size`; `row_index` is
  1-based (`integer(int32)` or `integer(int64)` — the latter only needed for a file with more rows
  than `huge(1_int32)`).

```fortran
type(parquet_reader) :: reader
integer(int32) :: row3(4)   ! col_size = 4

call parquet_open_reader(reader, "data.parquet")
call parquet_read_array_row_mode(reader, "vec", row3, row_index=3_int64)
call parquet_close_reader(reader)
```

- **Element mode** —
  `parquet_read_array_element_mode(reader, name, values, elem_index [, null_value] [, is_valid])`
  reads the same element position from **every** row instead. `values` is a 1D array of length
  `nrows` (the table's whole row count, not `col_size`); `elem_index` is 1-based, in
  `[1, col_size]`.

```fortran
type(parquet_reader) :: reader
integer(int64) :: nrows
integer(int32), allocatable :: first_elem(:)

call parquet_open_reader(reader, "data.parquet", nrows=nrows)
allocate(first_elem(nrows))
call parquet_read_array_element_mode(reader, "vec", first_elem, elem_index=1_int64)
call parquet_close_reader(reader)
```

Both take the same `null_value=`/`is_valid=` pair as `parquet_read_column` (see
[Null values](../types/supported-data-types.html#null-values)), both dispatch on `values`'
declared type/kind exactly like `parquet_read_column` does (including the
[widened-numeric-kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind)
conversions), and both accept a struct-nested dotted `name` (see
[Reading a nested struct field](../types/supported-data-types.html#reading-a-nested-struct-field)).

**When to prefer these over `parquet_read_column`:** row mode is the right tool when you need a
handful of specific rows out of a huge vector column and don't want to allocate/read the whole
`(col_size, nrows)` array just to throw most of it away. Element mode is the right tool when you
need one column of a vector column's own internal structure (e.g. always the first element of each
row) across the whole file, without allocating the full 2D array.

**I/O behavior** (see
[Reading only touches the columns you ask for](#reading-only-touches-the-columns-you-ask-for)
above for the full explanation): row mode reads only the one row group the requested row falls in;
element mode streams the file row group by row group, so neither ever materializes the whole
column in a single internal call. A row filter and/or `sample_fraction` doesn't change that —
`row_index`/`elem_index` then address the filtered result, resolved against each row group's
surviving row count. A
[`sort_by=`](filter-sort-sample.html#reading-rows-in-sorted-order-with-parquet_sortkey) does:
sorted row `i` belongs to no single row group, so both modes fall back to reading the whole
column. Neither is single-row I/O in the strictest sense (both still decode a full row group's
worth of data at a time), but both are genuine random access at row-group granularity rather than
a whole-column read.

## Column size queries

Three procedures answer questions about how much a column holds. What *kind* of column it is —
scalar, vector, list, map or struct — is a separate question, answered by
[`parquet_get_column_shape`](#which-kind-of-container-a-column-is-parquet_get_column_shape) below.

- **`parquet_get_col_size(reader, name, col_size)`** — a vector column's fixed per-row width (`1`
  for a plain scalar column).
- **`parquet_get_column_total_elements(reader, name, total_elements)`** — the number of elements
  the column holds across every row: `nrows` for a scalar column, `nrows * col_size` for a vector
  one, and for a variable-length `list` column the **sum of the rows' own lengths**, so a 3-row
  column holding 2, 0 and 3 elements answers `5`. Note that is deliberately *not*
  `nrows * col_size` for a ragged column, whose `col_size` is `1` (see `parquet_measure_list_width`
  below): the two queries ask different things of such a column, and only `col_size` has a single
  width to report. It agrees with
  [`%total_elements()`](../types/list-columns.html) on a `parquet_list_column` read from the same
  data.
- **`parquet_get_string_length(reader, name, max_string_length)`** — for a `string` column (or a
  vector column whose elements are strings): the longest value actually present, unaffected by
  Nulls (a Null is always skipped when computing this maximum) — the direct answer to "how long
  should my `character(len=...)` buffer be?" from the
  [minimal example](#reading-only-touches-the-columns-you-ask-for) notes above.

The first two normally answer straight from the file's schema/footer and read no column data at
all. The exception is a column stored as a plain variable-length `list`/`large_list` — which this
library writes from a
[`parquet_list_column`](../types/list-columns.html#writing-a-list-column-to-a-file), and which
another producer may write too: what its rows hold is a property of the data rather than of the
schema, so measuring it has to read the column. Even then it is bounded to **one row group at a
time**, so peak memory is a row group rather than the whole column. The two differ in how much
they can avoid: `parquet_get_col_size` screens each row group from the footer first and reads only
to prove a surviving candidate, so a visibly ragged column costs it nothing, while
`parquet_get_column_total_elements` has to sum the rows' own lengths and so always reads.

`parquet_get_string_length` is different in kind: it **always** reads the column, because the
longest string can only be found by looking at every value, and the column then stays cached like
any other read. Sizing a `character(len=...)` buffer this way is therefore not a free footer
lookup — on a very large string column it is a full decode, and
[`parquet_release_column`](#releasing-a-column-you-have-finished-with) is how you get that memory
back. Reading into a
[`parquet_string_column`](../types/string-columns.html#reading-and-writing-compact-string-columns)
avoids the question entirely, since it needs no pre-sizing.

Two lower-level queries exist for the plain-`list` case, if you need to control the measurement
rather than just take its answer:

- **`parquet_column_width_needs_data(reader, name)`** returns `.true.` only for a plain
  `list`/`large_list` column — i.e. it tells you in advance whether asking for the width will read
  anything at all.
- **`parquet_measure_list_width(reader, name, row_group_lo, row_group_hi, proven, width)`**
  measures over an inclusive 1-based row-group range (`row_group_lo <= 0` means every row group),
  with `proven=.false.` for the footer screen alone (free, and its answer is a candidate that may
  be wrong) and `proven=.true.` to confirm it by reading. A column with no rows measures `0`, and
  a genuinely ragged one measures `1`.

```fortran
type(parquet_reader) :: reader
integer(int32) :: col_size, strlen_max
integer(int64) :: total_elements

call parquet_open_reader(reader, "data.parquet")
call parquet_get_col_size(reader, "vec", col_size)
call parquet_get_column_total_elements(reader, "vec", total_elements)
call parquet_get_string_length(reader, "name", strlen_max)
call parquet_close_reader(reader)
```

All three accept a struct-nested dotted `name` (see
[Reading a nested struct field](../types/supported-data-types.html#reading-a-nested-struct-field))
and fail immediately with `error stop` if `name` doesn't exist. They also accept the wider
[descent paths](../types/supported-data-types.html#descent-paths-addressing-a-containers-child) — `[]` for a
list's elements, `{key}` and `{value}` for a map's — as do the type and shape queries below, so
`parquet_get_col_size(reader, "events[].id", ...)` is a legal question about a list of structs.

**`parquet_column_has_nulls(reader, name, row_group_lo, row_group_hi)`** answers the one remaining
shape question — whether a column contains any Null — from the per-column-chunk null counts Parquet
records in the footer, so it reads no column data either. It is a `logical` **function**, and unlike
the queries above its row-group bounds are required arguments: they restrict the question to an
inclusive 1-based range, with `row_group_lo <= 0` meaning every row group, as in
`parquet_measure_list_width` above.

```fortran
if (parquet_column_has_nulls(reader, "flux", 0, 0)) then   ! 0, 0 -- ask about the whole file
    call parquet_read_column(reader, "flux", flux, is_valid=valid)
else
    call parquet_read_column(reader, "flux", flux)         ! no mask needed, and measurably cheaper
end if
```

That is what the query is for: requesting `is_valid=` costs an extra buffer, a `LOGICAL` array four
times its size, a conversion pass and a scan, so asking first and omitting the argument on a
null-free column is worth the footer lookup.

Two properties are worth knowing before relying on it. Its uncertain answer is **`.true.`**
("might have nulls"), since statistics are optional in the format and claiming a column is clean
when it is not would make the read abort on the first Null. And it **declines a dotted struct path**
the same way, because a struct leaf's validity is combined with every ancestor struct's and the
leaf's own footer count does not describe the result.

## Checking column existence and type

`parquet_column_exists(reader, name, [types])` returns `.true.`/`.false.` for whether `name` (a
top-level or dotted
[struct-leaf path](../types/supported-data-types.html#reading-a-nested-struct-field), same as
everywhere else) exists in an open reader's schema:
```fortran
if (parquet_column_exists(reader, "ra")) then
    ...
end if
```
Pass `types` (optional) to also require that the column can be **read as** one of the given types —
a comma-separated list of tokens: any of the nine canonical single types (`int32`/`int64`/`float32`/
`float64`/`boolean`/`string`/`date`/`time`/`timestamp`), and/or the group aliases `int` (any integer
column), `float` (any column readable into a float) and `temporal` (`date`, `time`, or `timestamp`).
Matching is case-insensitive and tokens can be combined:
```fortran
if (parquet_column_exists(reader, "ra", types="float")) then     ! any numeric column
if (parquet_column_exists(reader, "flag", types="int, boolean")) ! any integer column, or boolean
```
Omitting `types` checks existence regardless of type; passing an *empty* or all-blank `types` is
treated as a mistake rather than as that shortcut, and fails immediately with `error stop`. An
unrecognized token in `types` (e.g. a typo) fails immediately with `error stop`, naming the valid
tokens — this check happens before the existence check itself, so a malformed filter is reported
even for a column that doesn't exist. A column this library cannot read at all (a `map`, say) never
matches a `types` filter, but is still found by a plain (no `types`) existence check.

`parquet_get_column_type(reader, name, type_name)` answers the same question the other way round —
**what Fortran type an existing column is read into** — directly into an allocatable `character`:
```fortran
character(len=:), allocatable :: type_name
call parquet_get_column_type(reader, "ra", type_name)   ! e.g. "float64"
```

### What a column is read as

Both procedures are driven from one mapping: the **narrowest lossless** Fortran kind for the
column's physical type. That is not the same as the physical type's own name — all four numeric
targets accept the same fifteen physical types, so which kind a read actually uses is chosen by your
declaration, not by the file (see
[Reading a column into a different numeric kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind)).
What these two queries report is the kind to declare if you want the file's values back intact.

| stored physical type | reported as |
|---|---|
| `int8`, `int16`, `int32`, `uint8`, `uint16` | `int32` |
| `int64`, `uint32` | `int64` |
| `uint64` | `int64` — **lossy**, deliberately |
| `half_float`, `float` | `float32` |
| `double` | `float64` |
| `decimal32`, `decimal64`, `decimal128`, `decimal256` | `float64` — **lossy**, deliberately |
| `bool` | `boolean` |
| `string`, `large_string` | `string` |
| `dictionary<string>`, `dictionary<large_string>` — what pandas writes for a `category` | `string` |
| `date32`, `date64` | `date` |
| `time32`, `time64` | `time` |
| `timestamp` | `timestamp` |
| anything else | `unknown` |

The two lossy rows return the conventional target rather than `unknown` on purpose: no Fortran kind
covers `uint64`'s full range or a decimal's exact value, but reporting them as unreadable would be
less useful than naming the kind the library will actually use. A `uint64` value above
`huge(1_int64)` still aborts when the column is read. The mapping reads the type ID alone, with no
precision or scale awareness, so `decimal(9,0)` reports `float64` like every other decimal rather
than `int32`.

**An alias is not the union of its member tokens, and that is intended.** `types="float"` matches an
`int32` column — an integer *is* readable into a float array — while `types="float64"` does not,
because that column's target kind is `int32`. The two ask different questions: *can I read this as a
float at all?* against *is `float64` the right declaration?* Both are useful, so neither was made to
imply the other.

A **dictionary column** reports the type of its **values**, because that is what a read of it
hands back: the column is decoded to its values as it is read, so it behaves in every way like an
ordinary column of that type — `parquet_read_column`, the compact `parquet_string_column` read,
`filter=`, `sort_by=` and `parquet_table` all accept it. What is lost is the encoding: the category
*codes*, their *order*, and pandas' `ordered` flag are not carried, since Fortran has no
categorical type to carry them in. The rule is the value type's own row above, so a dictionary over
a type this library cannot read — `dictionary<binary>` — still reports `"unknown"` and still fails
on a read, exactly as a plain `binary` column does. See
[Reading dictionary columns from other tools](../types/supported-data-types.html#reading-dictionary-columns-from-other-tools)
for the encoding, and [Files written by pandas](#files-written-by-pandas) below for the rest of
what a pandas-written file does differently.

A **vector or list** column reports its **element** type: an `int32` vector (`FIXED_SIZE_LIST`)
column and a variable-length `list<int32>` column both report `"int32"`. That unwrapping goes
exactly one level, so it does not extend to the other containers — a `map` column reports
`"unknown"` (ask `parquet_get_map_value_type` for its value type instead), and so does a
`list<list<int32>>`, whose element is itself a list rather than one of the nine types.
`parquet_get_column_type` fails with
`error stop` only if `name` doesn't exist — that is a caller mistake, and `parquet_column_exists` is
the query for it. A type it cannot read is an answer (`"unknown"`), not an error.

### What a column is actually stored as: `parquet_get_column_arrow_type`

The two queries above ask *what should I declare for this column?*, and for a column this library
cannot read they both answer `"unknown"`. That is a true answer, and a useless one in the situation
you are most likely to be in when you get it: an unfamiliar file, and a column you expected to
work. `parquet_get_column_arrow_type(reader, name, arrow_type)` answers the other question — *what
is actually in the file?* — in Arrow's own words:

```fortran
character(len=:), allocatable :: arrow_type
call parquet_get_column_arrow_type(reader, "cls", arrow_type)
! arrow_type = "dictionary<values=string, indices=int8, ordered=0>"
```

It **peels nothing**, which is what separates it from `parquet_get_column_type`:

| the column | `parquet_get_column_type` | `parquet_get_column_arrow_type` |
|---|---|---|
| a pandas `category` | `string` | `dictionary<values=string, indices=int8, ordered=0>` |
| a `float64` vector column | `float64` | `fixed_size_list<element: double>[3]` |
| a scaled decimal | `float64` | `decimal128(10, 2)` |
| a map column | `unknown` | `map<int32, int32 ('m_intkey')>` |
| a plain `int32` column | `int32` | `int32` |

Use the left column to decide what to declare, and the right one to find out what you are looking
at. The two right-hand answers you cannot get any other way are the dictionary's *index* type and
`ordered` flag, both of which the decode discards, and a decimal's precision and scale, which the
`float64` mapping loses.

`arrow_type` is `character(len=:), allocatable` and comes back allocated to exactly the length of
the type name, however long that is — a `struct` type prints its whole field list, and nests, so no
fixed buffer length would be honest for every answer. Schema-only like the queries above: it reads
no column data, takes a dotted struct-leaf path, and `error stop`s only if `name` doesn't exist.
There is no "unknown" here — whatever Arrow can name, this reports.

`parquet_table` calls it for you in two places, so a table often answers this question without
being asked: reaching the values of a column it cannot read names the stored type in the abort
message, and `%print_stat(all=.true.)` shows it in the `kind` cell of such a column instead of
`PK_NONE`.

### Which kind of container a column is: `parquet_get_column_shape`

`parquet_get_column_shape(reader, name, shape)` answers the orthogonal question — *is this a plain
value, a fixed-width vector, or a variable-length list?* — as one of six tokens:

```fortran
character(len=:), allocatable :: shape
call parquet_get_column_shape(reader, "flux", shape)   ! e.g. "list"
```

| token | what the column is |
|---|---|
| `"scalar"` | one value per row |
| `"vector"` | a fixed-size list: every row holds the same declared number of elements |
| `"list"` | a variable-length `LIST`/`LARGE_LIST` |
| `"map"` | a `MAP` — not readable |
| `"struct"` | a `STRUCT` reached as a whole — not readable; address its leaves by dotted path |
| `"unknown"` | a shape this query does not recognize — see below |

**The question is "is this a container?", not "can I read this?"** — so a column whose element type
this library cannot read at all is still a `"scalar"` if it holds one value per row. A `decimal128`
column answers `"scalar"` here while `parquet_get_column_type` answers `"unknown"`, and the pair is
the useful reading: a plain value, of a type you cannot have.

**An encoding wrapper does not change the answer.** Arrow can present a column as an *extension*
type, a *dictionary* type or a *run-end-encoded* type — wrappers that change how values are stored,
or what they mean, without changing how many of them a row holds. The shape is read through them,
so an `arrow.fixed_shape_tensor` column, whose storage is a four-wide fixed-size list, answers
`"vector"` with a `col_size` of `4` like any other vector column. You will see these only in files
written by another tool with the Arrow schema stored alongside the data.

`"unknown"` is a safety valve rather than something you should expect: it is the answer for a
column that is neither a container this library knows nor one value per row — an Arrow union, or a
list-view. No Parquet file is currently known to produce one, since Parquet has no union type and a
list-view round-trips as an ordinary `LIST`. It exists so that a shape this query does not
understand is reported honestly instead of being called a `"scalar"`.

It is schema-only: no column data is read, whatever the answer. Together with
`parquet_get_column_type` it gives a complete description of a column — `("float64", "list")` — and
neither half is redundant, which is why the shape was not folded into the existing query.

**A uniform `LIST` still answers `"list"`.** A variable-length column whose rows all happen to hold
three elements reads perfectly well into a 2-D array, and this query still calls it a `"list"` —
because whether the rows are uniform is a property of the *data*, and answering it would mean
reading the column. Ask `parquet_get_col_size` if that is the question; it does look (one row group
at a time, never the whole column). `"vector"` means the *schema* declares a width.

### What a map column's values are: `parquet_get_map_value_type`

`parquet_get_column_type` answers `"unknown"` for a map, because a map cell is not one value and
the question that query asks is *what element type would I declare?*.
`parquet_get_map_value_type(reader, name, type_name)` answers the narrower question you have once
the shape query has already told you the column is a `"map"`: what kind will the values come back
as?

```fortran
character(len=:), allocatable :: value_type
call parquet_get_map_value_type(reader, "attrs", value_type)   ! e.g. "int32"
```

It reports one of the same nine tokens `parquet_get_column_type` uses; or `"list"`, `"map"` or
`"struct"` when the values are themselves containers; or `"unknown"` — and here, unlike the shape
query, `"unknown"` really is reachable. It covers three cases a caller does not need to tell
apart: `name` is not a map at all, its keys are not strings (only string keys are supported), or
its values are of a type outside those tokens. Schema-only, like the two queries above, and it
`error stop`s only if `name` doesn't exist.

Keys are always strings, so there is no query for their type. See
[Map columns](../types/map-columns.html) for reading one.

## Files written by pandas

A Parquet file written by `DataFrame.to_parquet` is an ordinary Arrow-written file, and this
library reads it with no special handling and no conversion step. Five of its habits are worth
knowing about anyway: one used to need a workaround before opening the file, one puts a column in
the file that you did not, and the rest are things you would otherwise go looking for.

- **A `category` column arrives as plain strings.** pandas writes a `Categorical` as an Arrow
  *dictionary* — the distinct values once, plus a narrow index per row — and this library decodes
  it as it reads, so the column behaves in every way like an ordinary string column:
  `parquet_get_column_type` answers `"string"`, and `parquet_read_column`, the compact
  `parquet_string_column` read, `filter=`, `sort_by=`, `qc:` and `parquet_table` all accept it.
  `.astype(str)` before `to_parquet` is no longer needed. What is *not* carried across is the
  encoding itself: the category codes, their order, and pandas' `ordered` flag have no Fortran
  counterpart and are dropped. If you need the codes, factorise the strings you read back —
  `pf_index_map`'s `%get_or_add_many`, or `pf_rank(method="dense")`. If you only want to *see* the
  encoding, [`parquet_get_column_arrow_type`](#what-a-column-is-actually-stored-as-parquet_get_column_arrow_type)
  reports it verbatim. An *integer* categorical never reaches this path at all: Arrow restores a
  stored dictionary only for string and binary values, so pandas' `Categorical([1, 2, 1])` arrives
  as a dense `int64` column.

- **A non-default index becomes a real column called `__index_level_0__`.** A default `RangeIndex`
  writes no column and is recorded in metadata only; any other index is written as ordinary data,
  under `__index_level_0__` or under the index's own name if it has one. It is a normal column —
  nothing here invented it, and `parquet_get_column_names` lists it like any other. Drop it with
  `t%drop_columns("__index_level_0__", ignore_missing=.true.)` if it is in your way, or write the
  file with `to_parquet(index=False)` and it will not be there.

- **Strings may be `large_string`, and it makes no difference.** pandas 3 writes its default string
  dtype as Arrow `large_string` where pandas 2 wrote `string`. Both read identically here, so a
  file written by either release needs nothing.

- **The frame's own description is in the metadata.** pyarrow stores a JSON description of the
  DataFrame — its dtypes, its index, its column order — under the file-level metadata key `pandas`.
  `parquet_get_metadata(reader, "pandas", value)` returns it verbatim and
  `parquet_get_metadata_items` lists it alongside everything else, for a program that wants the
  original dtypes rather than the ones Arrow reports. See
  [Reading table metadata](#reading-table-metadata-with-parquet_get_metadata).

- **Reading this library's output back in pandas needs nothing either.** `pd.read_parquet` sees an
  ordinary Arrow file: a `parquet_string_column` column comes back as `large_string`, and the
  `.maml` sidecar and its units (`write_maml=.true.`) are a separate file that pandas neither sees
  nor needs. Nothing else differs from any other Arrow-written file.

### A pandas file end to end

A catalogue written by pandas, with a `cls` category, an object index (so `__index_level_0__` is
there) and a million rows:

```fortran
program look_at_a_pandas_file
    use parquet
    implicit none
    type(parquet_table) :: t
    integer(int64), allocatable :: rows(:)
    type(parquet_table_index) :: ix

    call parquet_open_table(t, "catalogue.parquet")
    call t%print_rows()                                 ! the header, and the "no materialized
                                                        ! columns" hint -- nothing is read yet
    call t%drop_columns("__index_level_0__", ignore_missing=.true.)
    call t%print_rows(columns="obj_id,cls,flux")        ! reads those three, shows 5 + 5 rows
    call t%filter_rows('cls == "galaxy"')               ! cls is a string column now
    call t%build_index("obj_id", ix)
    call ix%find_all(10999997_int64, rows)
    call t%print_rows(rows=parquet_slice_list(rows))    ! exactly the rows the lookup returned
end program look_at_a_pandas_file
```

Before the category column was decoded, the second `%print_rows` and the `%filter_rows` would both
have failed on `cls`. See [Showing the rows: `%print_rows`](../tables/table.html#showing-the-rows-print_rows)
for what the display prints, and
[Reading dictionary columns from other tools](../types/supported-data-types.html#reading-dictionary-columns-from-other-tools)
for the encoding itself, including what a dictionary over a type this library cannot read does.

## Reading a container column

A column whose rows hold a *variable* number of values, key/value pairs, or a set of named fields
of different types is read into one of the three container types rather than into an array.
`parquet_read_column` takes each of them, and `parquet_read_column_chunk` has the matching
row-group form:

| stored as | read into | its page |
|---|---|---|
| `LIST`/`LARGE_LIST` | `type(parquet_list_column)` | [List columns](../types/list-columns.html) |
| `MAP` | `type(parquet_map_column)` | [Map columns](../types/map-columns.html) |
| `STRUCT` read as one object | `type(parquet_struct_column)` | [Struct columns](../types/struct-columns.html) |

```fortran
type(parquet_list_column) :: flux
call parquet_read_column(reader, "flux", flux)    ! rows may differ in length
```

Each type carries its own null state — both the row's and each element's — so `null_value=` and
`is_valid=` do not apply, exactly as for the
[date/time types](../types/date-time.html). The three type pages
above are the reference; this page's queries work on container columns too, and
[`parquet_get_column_shape`](#which-kind-of-container-a-column-is-parquet_get_column_shape) is how
you find out which of them a column is before reading it.

Two things worth knowing before you reach for them. A `STRUCT`'s individual leaves can also be read
as ordinary columns by
[dotted path](../types/supported-data-types.html#reading-a-nested-struct-field), which is usually
what you want when only one field interests you — reading the whole struct is for when you need the
object. And **reading a nested container is supported where writing one is not**: a
`list<struct<...>>` reads back, and writing that same column is refused, so a read-modify-write
round trip through a nested container will not complete.

## Prefetching multiple columns at once with `parquet_prefetch_columns`

`parquet_prefetch_columns(reader, names)` reads several named columns in one call, filling the
same per-column cache `parquet_read_column` otherwise populates lazily. Because this library keeps
Arrow's `use_threads` on (see
[Thread-pool tuning](../operating/thread-safety.html#thread-pool-tuning)), decoding them together
lets Arrow use its thread pool concurrently instead of one column at a time — purely a throughput
optimization, never a requirement: a column you don't prefetch still works via the normal lazy
path. Every name is validated against the file's schema first; an unknown column fails immediately
with `error stop`, naming it. Calling it more than once is safe and efficient — already-cached
columns are not re-read, so the cache ends up holding the union of every name across all calls.

The column names can be given in either of two forms — `parquet_prefetch_columns` is generic:

```fortran
call parquet_open_reader(reader, "data.parquet")

! (1) a single string, names separated by commas and/or semicolons:
call parquet_prefetch_columns(reader, "ra; dec, mag")

! (2) an array of names (each element the same declared length):
call parquet_prefetch_columns(reader, ["ra ", "dec", "mag"])

! call parquet_read_column for these (in any order) and any other, non-prefetched column:
call parquet_read_column(reader, "ra", ra)
call parquet_read_column(reader, "id", id)   ! fine even though "id" was never prefetched
```

- **The string form (1)** splits on commas and/or semicolons (both accepted, and mixable), trims
  surrounding spaces from each name, and ignores empty tokens (so repeated or trailing delimiters
  are harmless). This is the recommended form when your names have different lengths, since it
  side-steps the array pitfall below.
- **The array form (2)** requires all names to share the same declared string length — pad shorter
  names with trailing spaces, as with any Fortran character array literal (`["ra ", "dec"]`, not
  `["ra", "dec"]`, which won't even compile). Be careful: if the declared length is *shorter* than
  a name, that name is **silently truncated** and then fails validation as a "column not found"
  for the truncated text — e.g. `[character(len=1) :: "a", "xa"]` truncates `"xa"` to `"x"` and
  aborts with `column not found in parquet file: x`. The string form has no such trap.

To prefetch **every** column in the file, rather than naming them yourself, pass `prefetch=.true.`
to `parquet_open_reader` instead of calling `parquet_prefetch_columns`:

```fortran
call parquet_open_reader(reader, "data.parquet", prefetch=.true.)

! every column is already cached -- reading any of them (in any order) never hits disk again:
call parquet_read_column(reader, "ra", ra)
call parquet_read_column(reader, "id", id)
```

`prefetch` is optional and defaults to `.false.`. It behaves exactly as if you called
`parquet_prefetch_columns` for every column in the file immediately after opening (same batched,
thread-parallel decode, same "already-cached columns are skipped" behavior), and composes
correctly with `filter`: prefetching happens *after* the filter is applied, so every prefetched
column — not just the ones the filter itself references — reflects the filtered row set. Because
it eagerly materializes the whole file in memory at open time, before you've asked for anything,
it trades memory and up-front latency for guaranteeing no column read is ever a cache miss later —
see [Performance and memory](../operating/performance.html).

## Printing reader statistics with `parquet_close_reader(..., print_stat=.true.)`

`parquet_close_reader(reader, print_stat=.true.)` prints a debug/diagnostic summary of the
reader's activity to stdout, right before actually closing it. `print_stat` is optional and
defaults to `.false.` (no output). This is *solicited* output, so it is still governed by the
library's verbosity setting: under
[`parquet_set_verbosity("silent")`](../operating/settings.html#terminal-output) or `"errors_only"`
the report is suppressed entirely and `print_stat=.true.` prints nothing.

The summary has two parts:

- Table-level: the filename, the total number of columns in the file, how many of those are
  actually shown below (see below), and the row count — written as `rows: N (of M total)` when a
  filter or sample has narrowed it. A further line appears for each of `sample:` (fraction and the
  seed actually used), `filter:` (the whole expression as re-rendered from the parse tree, which
  stays exact where the per-column `filter` cell below cannot), `sort:` (the keys, in the order
  they were added) and `screened:` (how many row groups the
  [statistics pre-screen](filter-sort-sample.html#row-groups-a-filter-cannot-match-are-never-read)
  skipped), each printed only when it applies.
- One row per column that was either prefetched (`parquet_prefetch_columns`) or actually read
  (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`) at some
  point during the reader's lifetime — a column never touched at all is left out of the list
  entirely, rather than decoding it just to fill in a report:

  | column | meaning |
  |---|---|
  | `col` | Column name. |
  | `parquet_type` | The column's physical type in the Parquet file (e.g. `int32`, `double`, `string`, `list<double>` for a vector column). |
  | `output_type` | The Fortran-side type most recently used to read this column (e.g. `float64`, `int32`, `string`); blank if the column was only ever prefetched, never actually read. |
  | `col_size` | The vector length for a fixed-length vector column; blank for a plain scalar column. |
  | `len_str` | For a `string` column only (blank otherwise): the longest string's length in the Parquet file, and, if the column was actually read via a `character(len=...)` array, the allocated output length after a `/` (e.g. `18` if they match, `18 / 24` if the Fortran buffer was allocated longer than necessary). |
  | `nulls` | Number of genuine Parquet Nulls in the column (every element, flattened, for a vector column). `released` if the column's buffers were freed by `parquet_release_column` before the close — the row still records that the column was touched, but every value-derived cell is then unavailable. |
  | `min` / `max` | Numeric/string columns: the minimum/maximum value (lexicographic for strings). Boolean columns: `T:<count>`/`F:<count>` instead, since a boolean's min/max isn't a meaningful summary. `-`/`-` if every value is Null. |
  | `qcmin` / `qcmax` | The quality-control bound declared for this column, written as the operator followed by the raw value exactly as the schema gave it (e.g. `>=0.0`, `<360.0`); blank where the schema declares none. These are what the values in `min`/`max` were checked against. |
  | `qcmiss` | `Null` if the schema's `qc: miss:` declares that Nulls are permitted in this column; blank otherwise. |
  | `fetched` | `yes` if this column was named in a `parquet_prefetch_columns` call, `no` otherwise. |
  | `read` | `yes` if this column was actually read via a typed read call, `no` otherwise (e.g. prefetched but never read). |
  | `filter` | The active row filter's clauses on this column, with the column name stripped and joined with `, ` (e.g. `>=0.0, <360.0`); blank if the filter does not mention this column. A column named only by the filter still appears in this table, because evaluating the filter had to decode it. |

```fortran
call parquet_open_reader(reader, "data.parquet")
call parquet_prefetch_columns(reader, ["ra ", "mag"])
call parquet_read_column(reader, "ra", ra)
call parquet_close_reader(reader, print_stat=.true.)
```

Requires linking Arrow's `arrow_compute` library (for the min/max calculation) in addition to
`arrow`/`parquet` — see [Environment variables](../../index.html#environment-variables);
`parquet-fortran`'s own `fpm.toml` already declares this link, and fpm propagates it to consuming
projects automatically, so no action is normally needed.

## Filtering, sorting and sampling rows

A reader can also be restricted to a subset of the file's rows — matching a
[`parquet_filter`](filter-sort-sample.html#row-filtering-with-parquet_filter), or
[randomly downsampled](filter-sort-sample.html#random-downsampling-with-sample_fraction) — and can
return them in
[sorted order](filter-sort-sample.html#reading-rows-in-sorted-order-with-parquet_sortkey).
Everything on this page then behaves as if the file only ever contained the surviving rows. See
[Filtering, sorting and sampling rows](filter-sort-sample.html) for the full reference.

## Quality control

Reading also has its own side of quality control — checking column values against
`qc: min:`/`max:`/`miss:` bounds declared in a MAML file, independent of the write-side checks —
plus two ways to build a qc-maml directly in code without a `.maml` file on disk. See
[Quality control](../schema/quality-control.html) for the full picture (both sides, plus the
in-code builders); the read side is
`parquet_open_reader(reader, filename, schema=..., qc=..., qc_soft=...)`, which by default
hard-aborts on a violation (`qc_soft=.true.` instead warns and continues).

## Reading table metadata with `parquet_get_metadata`

`parquet_get_metadata(reader, key, value [, default] [, warn])` reads one table-level metadata
entry back out of a file — the read-side counterpart to the writer's `schema%add_metadata` (see
[Runtime table metadata](../schema/building-schema-in-code.html#runtime-table-metadata-schemaadd_metadata-and-schemaclear_metadata)).
It is generic: the declared type/kind of `value` (a scalar or 1D array of any
[supported type](../types/supported-data-types.html)) selects the variant and how the stored text
is parsed back, since all metadata is stored as strings in the file. Table metadata is read once,
at `parquet_open_reader` time, and cached, so each call only scans that in-memory copy. Any key
present in the file works, including the reserved/internal ones the writer emits (`DATE`, `name`,
per-column `column.<name>.*`, per-keyword `<KEY>.datatype`, ...), not just keys added via
`add_metadata` — see
[How table-level keys become metadata entries](../schema/maml-format.html#how-table-level-keys-become-metadata-entries)
for which MAML keys produce which entries.

A **Fortran reader never needs the `<KEY>.datatype` entries.** They record what a typed
`schema%add_metadata` call stored (see
[A typed value records its own type](../schema/building-schema-in-code.html#a-typed-value-records-its-own-type)),
and here the declared type of `value` already selects the parse, so `parquet_get_metadata` neither
consults them nor checks itself against them. They exist for readers in languages where a value's
type cannot be declared at the call site, and they are readable like any other key:
`call parquet_get_metadata(reader, "NSIDE.datatype", token)`.

- A **missing** `key` triggers `error stop`, unless the optional `default` (same type/kind as
  `value`) is given, in which case `value` is set to it.
- A **present but unconvertible** stored value (e.g. non-numeric text, or an integer too large for
  the requested kind) always prints a `WARNING`, then falls back to `default` if given, else
  `error stop`.
- `warn` (optional `logical`, default `.true.`) only governs the missing-key-with-`default` case;
  pass `warn=.false.` to suppress that warning. It has no effect when the key is present.

### Listing every metadata entry

`parquet_get_metadata` answers for a key you already know. To find out what a file carries at all,
`parquet_get_metadata_items(reader, keys, values)` hands back both, index-aligned and in the
file's own order:

```fortran
character(len=:), allocatable :: keys(:), values(:)
integer :: i

call parquet_get_metadata_items(reader, keys, values)
do i = 1, size(keys)
    print *, trim(keys(i)), " = ", trim(values(i))
end do
```

Each array is allocated to its own longest entry and blank-padded, so `trim()` is how you use them
— the same convention `parquet_get_column_names` follows. Both come back zero-size for a file with
no metadata. Like `parquet_get_metadata` it reads nothing: the answer comes from the copy made
when the reader was opened. This is what copying metadata from one file to another is built on —
see
[`parquet_write_table`'s `copy_metadata=`](../tables/table-write.html#carrying-the-source-files-metadata-to-the-output).

## Streaming/chunked reads

`parquet_read_column` reads a whole column into one complete array — fine for most data, but not
for a column too large to hold in memory that way. `parquet_read_column_chunk` reads such a column
one Parquet row group at a time instead, so peak memory is bounded by a row group's worth of data
rather than the whole column — the read-side mirror of
[streaming/chunked writes](writing.html#streamingchunked-writes):

```fortran
type(parquet_reader) :: reader
integer(int64) :: num_row_groups, rg, rg_nrows
integer(int32), allocatable :: buf(:)

call parquet_open_reader(reader, "data.parquet")
call parquet_get_num_row_groups(reader, num_row_groups)

do rg = 1, num_row_groups
    call parquet_get_chunk_size(reader, rg_nrows, row_group=rg)
    allocate(buf(rg_nrows))

    call parquet_read_column_chunk(reader, "big_vec", rg, buf)
    call process_chunk(buf, rg_nrows)  ! your own code

    deallocate(buf)
end do

call parquet_close_reader(reader)
```

`parquet_get_num_row_groups(reader, num_row_groups)` returns the file's row-group count.
`parquet_get_chunk_size(reader, chunk_size, [row_group])` returns row group `row_group`'s
(1-based) own row count — its physical row count on an ordinary reader, and its *surviving* row
count once a filter or sample is active (see below) — row groups are not guaranteed uniform, so
query each one rather than assuming they all match the first; `row_group` is optional and defaults
to the first row group. `parquet_read_column_chunk(reader, name, row_group, values)` then reads
that row group's rows into `values`, dispatched by its actual/declared type/kind and rank (scalar
`values(:)` or matrix `values(:,:)`) exactly like `parquet_read_column`, and separately by
`row_group`'s own kind (`integer(int32)`/`integer(int64)`, the latter only needed for a file with
more row groups than `huge(1_int32)`).

**Stateless, random access:** unlike the write side's
`parquet_new_row_group`/`parquet_finish_row_group` pairing, there is no "currently open" row group
to track — call `parquet_read_column_chunk` with any `row_group`, in any order, as many times as
you like, for any column, independent of any other chunked read on the same reader.

**Type matching:** a chunked read converts between numeric kinds exactly as `parquet_read_column`
does — same rules, same helpers — so a `float64` column can be chunk-read into a `real32` array, an
`int32` column into an `integer(int64)` or a `real64` one, and so on. See
[Reading a column into a different numeric kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind)
for what is permitted and where an out-of-range or non-integral value aborts. `logical` and
`character` chunk reads are the exception: those require the stored column to actually be boolean or
string, and fail with `error stop` naming both types otherwise.

**Compact string columns:** a scalar `string` column can also be chunk-read into a
`type(parquet_string_column)` (`values` is cleared, then filled with just that row group's rows) —
see
[Reading and writing compact string columns](../types/string-columns.html#reading-and-writing-compact-string-columns).

**Works on a filtered or sampled reader, but not on a sorted one** (see
[what a sort disallows](filter-sort-sample.html#what-a-sort-disallows)). A chunked read on a
reader opened with [`filter=`](filter-sort-sample.html#row-filtering-with-parquet_filter) and/or
[`sample_fraction=`](filter-sort-sample.html#random-downsampling-with-sample_fraction) hands back
that row group's *surviving* rows, and `parquet_get_chunk_size` reports that same count — so the
sizes still sum to `parquet_get_nrows`, and a chunked loop needs no separate bookkeeping:

```fortran
call parquet_open_reader(reader, "data.parquet", filter=filt)
call parquet_get_num_row_groups(reader, nrg)
do rg = 1, nrg
    call parquet_get_chunk_size(reader, n, row_group=rg)   ! surviving rows in this row group
    allocate(v(n))
    call parquet_read_column_chunk(reader, "v", rg, v)      ! just those rows
    ...
    deallocate(v)
end do
```

A row group whose rows were all filtered away yields **zero** rows — the normal case under a
selective filter, not an error: `parquet_get_chunk_size` returns 0 and the read is a no-op. It
still counts as read for `check_complete` below.

**Chunked reads do not accumulate memory.** Each `parquet_read_column_chunk` call reads that one
row group, hands the values over, and frees the Arrow array before returning — nothing is added to
the reader's column cache, so a loop over a thousand row groups holds no more than a loop over one,
and calling `parquet_release_column` after each chunk would have nothing to release. This is what
makes the pattern usable on a file far larger than memory. Measured with Arrow's own pool counter
over a 400,000-row file in 20 row groups: the pool stays flat at well under a kilobyte across the
whole loop, where one `parquet_read_column` of the same column retains 3.5 MB for the reader's
lifetime. (Resident set size cannot show this — Arrow does not return freed pages to the OS — which
is why the figure comes from the pool counter.) The one bounded exception is reading into a
[`parquet_string_column`](../types/string-columns.html#reading-and-writing-compact-string-columns):
that path pins the current chunk's buffers until the next chunk is read or the reader is closed, so
exactly one chunk stays alive. Bounded, not accumulating.

### Memory-bounded filtering with a row-group scope

Applying a filter has two engines, and
**which one runs is decided by whether you name row groups at all, not by which rows you name**:

| call | engine | filter columns afterwards |
|---|---|---|
| `parquet_open_reader(..., filter=filt)` | whole file, one batched pass | **cached** — reading one later is free |
| `parquet_reader_set_filter(reader, filt)` | same as open-time | **cached** |
| `parquet_reader_set_filter(reader, filt, 0, 0)` | row group at a time, all row groups | **not cached** — only the mask is kept |
| `parquet_reader_set_filter(reader, filt, 5, 8)` | row group at a time, row groups 5..8 | **not cached** |

So the row-group arguments are **not** required. Without them the filter behaves exactly as an
open-time `filter=` does: every filter column is read whole-file in one batched, thread-parallel
pass and left decoded, which is fastest and is the right default, at the cost of one full copy of
those columns. With them, the expression is evaluated one row group at a time over that inclusive
1-based range, each chunk being released before the next is read, so peak memory is one row group's
worth of the filter columns rather than the whole file; rows outside the range never match, and
nothing is left cached, so a filter column read afterwards is read again.

A `parquet_table` reaches the same engine through
[`parquet_open_table(..., bounded=.true.)`](../tables/table-open.html#reading-a-file-larger-than-memory-bounded),
which installs the filter this way and then assembles every column from per-row-group chunks.

`row_group_lo = 0` means **every** row group — the bounded-memory engine over the whole file,
without having to ask `parquet_get_num_row_groups` how many there are first. `row_group_hi` is
ignored in that case. (A non-positive lower bound reads the same way in `parquet_measure_list_width`
and `parquet_column_has_nulls`.) Put the other way round: to store the filtered columns, filter
while opening; to store only the mask, filter afterwards with row-group arguments.

`parquet_reader_set_filter` **refuses**, with `error stop`, if the reader already has a filter
(compose the clauses into one `parquet_filter` instead — several `%add` calls are AND-combined) or
if **any column has already been decoded** on this reader, since data already handed back could
not then be aligned with anything read afterwards. A filter passed to `parquet_open_reader` counts
as an active filter for the first of those, so opening with `filter=` and then calling
`parquet_reader_set_filter` aborts as surely as calling it twice does. The second condition is easy
to meet by accident here: a `parquet_read_column_chunk` call before the `set_filter` gets the
ordering wrong. Set the filter first, then loop. It refuses on a third condition too — a reader
that **already has a sort** — since a filter must be applied before one. See
[Applying a filter after the reader is open](filter-sort-sample.html#applying-a-filter-after-the-reader-is-open).

Pair it with a chunked loop over the same row groups to filter a file larger than memory:

```fortran
call parquet_open_reader(reader, "huge.parquet")
call parquet_reader_set_filter(reader, filt, 5, 8)   ! only row groups 5..8 are examined
```

**A scoped filter scopes the whole reader, not just the loop.** Everything the reader hands back
afterwards is restricted to the chosen row groups — a whole-column `parquet_read_column` returns the
survivors of row groups 5..8 and nothing else, and `parquet_get_nrows` reports that same count. That
is the scope working as intended, not a filter that failed to match the rest of the file; rows
outside the range have no mask bits at all, so there is nothing for a later read to return. If you
want the whole file examined with bounded memory, that is what `row_group_lo = 0` above is for.

**Narrowing to an exact row range.** A row-group range can only ever begin and end on a row-group
boundary, so a caller interested in an arbitrary row range would get back every survivor of the
*covering* row groups with no way to trim them — only the mask knows which physical rows those
are. A four-argument form adds the row range itself:
`parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi, row_lo, row_hi)`, where
`row_lo`/`row_hi` are 1-based, inclusive, physical file rows. Rows outside them never match, so
`parquet_get_nrows` afterwards is that range's own surviving count:

```fortran
call parquet_reader_set_filter(reader, filt, 2, 3, 5, 8)   ! rows 5..8, inside row groups 2..3
```

The row range must lie **inside** the rows its row groups span, or the call fails with `error stop`
naming both ranges and the span. The two ranges are otherwise individually plausible — each is
checked against the file's own row-group count and row count — and a disjoint pair would quietly
yield their intersection, which is frequently empty and therefore indistinguishable from a filter
that matched nothing.

The filter may hold no rules at all in this form, in which case the range alone decides which rows
match — that is how a row scope is installed for its own sake, with no expression to hang it on.
Both integer kinds are accepted, as for the row-group bounds.

**Read-time qc still runs, scoped to one row group at a time:** if the reader was opened with
[`qc=.true.`](../schema/quality-control.html#read-side-enforcement), each
`parquet_read_column_chunk` call runs the usual `qc: min:`/`max:`/`miss:` checks against just that
row group's own data, not the whole column. In hard mode (`qc_soft=.false.`, the default), a
violation aborts immediately, naming the offending row group
(`qc violation for column 'name [row group N]'...`). In soft mode (`qc_soft=.true.`), a violation
prints a `WARNING` — still at most once per column for the reader's whole lifetime (the same
throttling `parquet_read_column` already uses), so reading many violating row groups in soft mode
doesn't spam one warning per chunk.

**Completeness checks:** pass `check_complete=.true.` to `parquet_close_reader` to verify that
every column you read via `parquet_read_column_chunk` had *every* one of the file's row groups
read by the time you close — catches a loop that forgot a row group, or exited early by mistake:

```fortran
call parquet_close_reader(reader, check_complete=.true.)
```

`check_complete` defaults to `.false.` (no check, so existing code is unaffected). When it's
`.true.`, `check_hard` (default `.true.`) picks the failure mode: `error stop` naming the column
and its missing row group(s), or (`check_hard=.false.`) a `WARNING` instead. Only columns actually
touched via `parquet_read_column_chunk` are tracked — a column read via
`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` is never
included in this check, even if the reader also chunk-read other columns.

**Threading:** `parquet_read_column_chunk` calls on the *same* `parquet_reader` are bound by the
same "one thread at a time" rule as every other call into a shared reader (see
[Thread safety](../operating/thread-safety.html#rules-at-a-glance)) — but since chunked reads are
stateless/random-access, splitting the row-group loop itself across threads works cleanly as long
as each thread uses its *own* `parquet_reader` instance opened on the same file (independent
readers on the same file are always safe to use concurrently), rather than sharing one reader
across threads.
