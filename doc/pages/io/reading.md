---
title: Reading parquet files from your Fortran code
---

To use this library in another Fortran project, add it as an FPM dependency — see
[Minimal setup to depend on this library](../../index.html#minimal-setup-to-depend-on-this-library) in
the README for the `fpm.toml` snippet. Then `use parquet` in your code.

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
- Allocate output arrays before calling `parquet_read_column`. Its row count (`size(values)` for a plain column, `size(values, 2)` for a vector column) must match `parquet_get_nrows` exactly, or it fails immediately with `error stop`, naming the column and both row counts.
- `name` must be a column that actually exists in the file — this is checked for every procedure that takes a column name (`parquet_read_column`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`), each failing immediately with `error stop` naming the missing column.
- For string columns, choose a fixed string length that is large enough for your data — or read
  into a `type(parquet_string_column)` instead, which needs no pre-sizing; see
  [Reading and writing compact string columns](../types/string-columns.html#reading-and-writing-compact-string-columns).
  Not sure how long is long enough? `parquet_get_string_length` answers that directly — see
  [Column shape and size queries](#column-shape-and-size-queries) below.
- For vector columns, allocate 2D arrays with shape `(col_size, nrows)`.
- A `DATE`/`TIME`/`TIMESTAMP` column reads into a `parquet_date`/`parquet_time`/`parquet_timestamp`
  array instead — see [Date, time and timestamp columns](../types/date-time.html); these three carry their
  own null state, so `null_value=`/`is_valid=` don't apply to them.
- A vector column may be stored on disk either as a `fixed_size_list` (what this library's own writer emits) or as a variable-length `list<element>` (the Parquet `LIST` layout many other producers use — including its legacy 2-level and non-standard inner-element-name variants, which Arrow's reader normalizes to the same `list` type); both are read back identically. The only requirement is that every row's vector has the same length (so it fits the `(col_size, nrows)` shape); a genuinely ragged `list` column (rows of differing length) is rejected with `error stop`. `col_size` is inferred from the data in either case.

## The `nrows=` shortcut

`parquet_open_reader(..., nrows=nrows)` is a shortcut for the `parquet_open_reader` + `parquet_get_nrows` pair above: pass an `integer(int32)` or `integer(int64)` variable (a plain default `INTEGER` works too, on the vast majority of platforms where that's the same kind as `int32`) as `nrows` and it's filled in for you, equivalent to calling `parquet_get_nrows(reader, nrows, check_positive=.true.)` immediately after opening (post-filter, if a `filter` was also given). Because it implies `check_positive=.true.`, a file (or filter result) with **zero rows fails immediately with `error stop`** — it does not return `nrows=0`. This is the right choice when your code has no sensible zero-row behavior and would rather abort loudly than proceed with an empty read. If zero rows is a case you need to detect and handle (rather than treat as a hard error), don't pass `nrows=` to `parquet_open_reader` — open the reader as usual and call `parquet_get_nrows(reader, nrows)` yourself afterwards (without `check_positive`), which returns `nrows=0` instead of aborting, exactly like the code example above. As with a direct `parquet_get_nrows(reader, nrows)` call, an `integer(int32)` (or default-`INTEGER`) `nrows` also fails immediately with `error stop` if the actual row count overflows `int32`'s range, rather than silently wrapping or truncating.

## Reading only touches the columns you ask for

`parquet_open_reader` only parses the file's footer (schema, row count, row-group layout) — it does not read or decompress any column's data. Each column is read from disk only the first time you ask for it (`parquet_read_column`, `parquet_get_string_length`, etc.), then cached for the lifetime of that `reader`; asking for it again doesn't re-read it, and columns you never ask for are never read at all. This follows from Parquet's layout — each column is its own contiguous byte range, so the reader seeks straight to just the bytes it needs, regardless of [compression codec](writing.html#writer-options). So opening a large file with many columns and reading only a handful is cheap in both I/O and memory, no matter how large the unrequested columns are. `parquet_get_col_size`/`parquet_get_column_total_elements` are cheaper still for the common case (a vector column stored as `fixed_size_list`): they answer straight from the schema/footer, reading no column data at all — so they're safe to call even on a column whose total element count (`nrows * col_size`) is enormous. `parquet_read_array_row_mode` reads only the one row group the requested row lives in, not the whole column. `parquet_read_array_element_mode` can't limit itself to one row group the way `parquet_read_array_row_mode` does — it inherently needs every row's value at the same element position, so every row group contributes — but it still avoids ever materializing the whole column's flattened element count in a single internal call, by streaming the file row group by row group instead. A [row filter](filter-sort-sample.html#row-filtering-with-parquet_filter) or [`sample_fraction`](filter-sort-sample.html#random-downsampling-with-sample_fraction) doesn't change either of these: `row_index`/`elem_index` then address the filtered result, and each row group's *surviving* row count is what they are resolved against. (Chunked reads work per row group on a filtered reader too — see [Streaming/chunked reads](#streamingchunked-reads).)

## Random access: row mode and element mode

`parquet_read_array_row_mode` and `parquet_read_array_element_mode` read a **vector** (`col_size >
1`) column without materializing the whole thing — the random-access counterparts to
`parquet_read_column`, for when you only need one row, or one fixed element position across every
row, out of a column that may be far larger:

- **Row mode** — `parquet_read_array_row_mode(reader, name, values, row_index [, null_value] [, is_valid])` reads one row's entire element vector. `values` is a 1D array of length `col_size`; `row_index` is 1-based (`integer(int32)` or `integer(int64)` — the latter only needed for a file with more rows than `huge(1_int32)`).

  ```fortran
  type(parquet_reader) :: reader
  integer(int32) :: row3(4)   ! col_size = 4

  call parquet_open_reader(reader, "data.parquet")
  call parquet_read_array_row_mode(reader, "vec", row3, row_index=3_int64)
  call parquet_close_reader(reader)
  ```

- **Element mode** — `parquet_read_array_element_mode(reader, name, values, elem_index [, null_value] [, is_valid])` reads the same element position from **every** row instead. `values` is a 1D array of length `nrows` (the table's whole row count, not `col_size`); `elem_index` is 1-based, in `[1, col_size]`.

  ```fortran
  type(parquet_reader) :: reader
  integer(int64) :: nrows
  integer(int32), allocatable :: first_elem(:)

  call parquet_open_reader(reader, "data.parquet", nrows=nrows)
  allocate(first_elem(nrows))
  call parquet_read_array_element_mode(reader, "vec", first_elem, elem_index=1_int64)
  call parquet_close_reader(reader)
  ```

Both take the same `null_value=`/`is_valid=` pair as `parquet_read_column` (see [Null values](../types/supported-data-types.html#null-values)), both dispatch on `values`' declared type/kind exactly like `parquet_read_column` does (including the [widened-numeric-kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind) conversions), and both accept a struct-nested dotted `name` (see [Reading a nested struct field](../types/supported-data-types.html#reading-a-nested-struct-field)).

**When to prefer these over `parquet_read_column`:** row mode is the right tool when you need a handful of specific rows out of a huge vector column and don't want to allocate/read the whole `(col_size, nrows)` array just to throw most of it away. Element mode is the right tool when you need one column of a vector column's own internal structure (e.g. always the first element of each row) across the whole file, without allocating the full 2D array.

**I/O behavior** (see [Reading only touches the columns you ask for](#reading-only-touches-the-columns-you-ask-for) above for the full explanation): row mode reads only the one row group the requested row falls in; element mode streams the file row group by row group, so neither ever materializes the whole column in a single internal call. A row filter and/or `sample_fraction` doesn't change that — `row_index`/`elem_index` then address the filtered result, resolved against each row group's surviving row count. A [`sort_by=`](filter-sort-sample.html#reading-rows-in-sorted-order-with-parquet_sortkey) does: sorted row `i` belongs to no single row group, so both modes fall back to reading the whole column. Neither is single-row I/O in the strictest sense (both still decode a full row group's worth of data at a time), but both are genuine random access at row-group granularity rather than a whole-column read.

## Column shape and size queries

Three procedures answer questions about a column's shape without reading any of its actual data — all three read straight from the file's schema/footer:

- **`parquet_get_col_size(reader, name, col_size)`** — a vector column's fixed per-row width (`1` for a plain scalar column).
- **`parquet_get_column_total_elements(reader, name, total_elements)`** — the column's total flattened element count (`nrows * col_size` for a vector column; `nrows` for a scalar one).
- **`parquet_get_string_length(reader, name, max_string_length)`** — for a `string` column only: the longest value actually present, unaffected by Nulls (a Null is always skipped when computing this maximum) — the direct answer to "how long should my `character(len=...)` buffer be?" from the [minimal example](#reading-only-touches-the-columns-you-ask-for) notes above.

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

All three accept a struct-nested dotted `name` (see [Reading a nested struct field](../types/supported-data-types.html#reading-a-nested-struct-field)) and fail immediately with `error stop` if `name` doesn't exist.

## Checking column existence and type

`parquet_column_exists(reader, name, types)` returns `.true.`/`.false.` for whether `name` (a
top-level or dotted [struct-leaf path](../types/supported-data-types.html#reading-a-nested-struct-field), same as everywhere else)
exists in an open reader's schema:
```fortran
if (parquet_column_exists(reader, "ra")) then
    ...
end if
```
Pass `types` (optional) to also require the column's physical type to match — a comma-separated
list of tokens: any of the nine canonical single types (`int32`/`int64`/`float32`/`float64`/
`boolean`/`string`/`date`/`time`/`timestamp`), and/or the group aliases `int` (`int32` or
`int64`), `float` (`float32` or `float64`), and `temporal` (`date`, `time`, or `timestamp`).
Matching is case-insensitive and tokens can be combined:
```fortran
if (parquet_column_exists(reader, "ra", types="float")) then      ! float32 or float64
if (parquet_column_exists(reader, "flag", types="int, boolean")) ! int32, int64, or boolean
```
Omitting `types` checks existence regardless of type. An unrecognized token in `types` (e.g. a
typo) fails immediately with `error stop`, naming the valid tokens — this check happens before the
existence check itself, so a malformed filter is reported even for a column that doesn't exist. A
column whose physical type isn't one of the nine canonical tokens (e.g. an `int8`/`uint32`/
`decimal` column from another tool — see
[Reading a column into a different numeric kind](../types/supported-data-types.html#reading-a-column-into-a-different-numeric-kind))
never matches a `types` filter, but is still found by a plain (no `types`) existence check.

`parquet_get_column_type(reader, name, type_name)` resolves an *existing* column's canonical
physical type directly into an allocatable `character`:
```fortran
character(len=:), allocatable :: type_name
call parquet_get_column_type(reader, "ra", type_name)   ! e.g. "float64"
```
A vector (`FIXED_SIZE_LIST`) column reports its element type (an `int32` vector column reports
`"int32"` — see `parquet_get_col_size` for its element count). Unlike `parquet_column_exists`,
this procedure's whole contract is "give me the type", so it cannot answer silently: it fails with
`error stop` if `name` doesn't exist, or if its physical type falls outside the nine canonical
tokens — use `parquet_column_exists` with no `types` filter first if the column's existence or
type isn't already guaranteed.

## Prefetching multiple columns at once with `parquet_prefetch_columns`

`parquet_prefetch_columns(reader, names)` reads several named columns in one call, filling the same per-column cache `parquet_read_column` otherwise populates lazily. Because this library keeps Arrow's `use_threads` on (see [Thread-pool tuning](../operating/thread-safety.html#thread-pool-tuning)), decoding them together lets Arrow use its thread pool concurrently instead of one column at a time — purely a throughput optimization, never a requirement: a column you don't prefetch still works via the normal lazy path. Every name is validated against the file's schema first; an unknown column fails immediately with `error stop`, naming it. Calling it more than once is safe and efficient — already-cached columns are not re-read, so the cache ends up holding the union of every name across all calls.

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

- **The string form (1)** splits on commas and/or semicolons (both accepted, and mixable), trims surrounding spaces from each name, and ignores empty tokens (so repeated or trailing delimiters are harmless). This is the recommended form when your names have different lengths, since it side-steps the array pitfall below.
- **The array form (2)** requires all names to share the same declared string length — pad shorter names with trailing spaces, as with any Fortran character array literal (`["ra ", "dec"]`, not `["ra", "dec"]`, which won't even compile). Be careful: if the declared length is *shorter* than a name, that name is **silently truncated** and then fails validation as a "column not found" for the truncated text — e.g. `[character(len=1) :: "a", "xa"]` truncates `"xa"` to `"x"` and aborts with `column not found in parquet file: x`. The string form has no such trap.

To prefetch **every** column in the file, rather than naming them yourself, pass `prefetch=.true.` to `parquet_open_reader` instead of calling `parquet_prefetch_columns`:

```fortran
call parquet_open_reader(reader, "data.parquet", prefetch=.true.)

! every column is already cached -- reading any of them (in any order) never hits disk again:
call parquet_read_column(reader, "ra", ra)
call parquet_read_column(reader, "id", id)
```

`prefetch` is optional and defaults to `.false.`. It behaves exactly as if you called `parquet_prefetch_columns` for every column in the file immediately after opening (same batched, thread-parallel decode, same "already-cached columns are skipped" behavior), and composes correctly with `filter`: prefetching happens *after* the filter is applied, so every prefetched column — not just the ones the filter itself references — reflects the filtered row set. Because it eagerly materializes the whole file in memory at open time, before you've asked for anything, it trades memory and up-front latency for guaranteeing no column read is ever a cache miss later — see [Performance and memory](../operating/performance.html).

## Printing reader statistics with `parquet_close_reader(..., print_stat=.true.)`

`parquet_close_reader(reader, print_stat=.true.)` prints a debug/diagnostic summary of the reader's activity to stdout, right before actually closing it. `print_stat` is optional and defaults to `.false.` (no output).

The summary has two parts:

- Table-level: the filename, the total number of columns in the file, how many of those are actually shown below (see below), and the total row count.
- One row per column that was either prefetched (`parquet_prefetch_columns`) or actually read (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`) at some point during the reader's lifetime — a column never touched at all is left out of the list entirely, rather than decoding it just to fill in a report:

  | column | meaning |
  |---|---|
  | `col` | Column name. |
  | `parquet_type` | The column's physical type in the Parquet file (e.g. `int32`, `double`, `string`, `list<double>` for a vector column). |
  | `output_type` | The Fortran-side type most recently used to read this column (e.g. `float64`, `int32`, `string`); blank if the column was only ever prefetched, never actually read. |
  | `col_size` | The vector length for a fixed-length vector column; blank for a plain scalar column. |
  | `len_str` | For a `string` column only (blank otherwise): the longest string's length in the Parquet file, and, if the column was actually read via a `character(len=...)` array, the allocated output length after a `/` (e.g. `18` if they match, `18 / 24` if the Fortran buffer was allocated longer than necessary). |
  | `nulls` | Number of genuine Parquet Nulls in the column (every element, flattened, for a vector column). |
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

Requires linking Arrow's `arrow_compute` library (for the min/max calculation) in addition to `arrow`/`parquet` — see [Environment variables](../../index.html#environment-variables); `parquet-fortran`'s own `fpm.toml` already declares this link, and fpm propagates it to consuming projects automatically, so no action is normally needed.

## Filtering, sorting and sampling rows

A reader can also be restricted to a subset of the file's rows — matching a
[`parquet_filter`](filter-sort-sample.html#row-filtering-with-parquet_filter), in
[sorted order](filter-sort-sample.html#reading-rows-in-sorted-order-with-parquet_sortkey), or
[randomly downsampled](filter-sort-sample.html#random-downsampling-with-sample_fraction) — and
everything on this page then behaves as if the file only ever contained the surviving rows. See
[Filtering, sorting and sampling rows](filter-sort-sample.html) for the full reference.

## Quality control

Reading also has its own side of quality control — checking column values against `qc:
min:`/`max:`/`miss:` bounds declared in a MAML file, independent of the write-side checks — plus
two ways to build a qc-maml directly in code without a `.maml` file on disk. See
[Quality control](../schema/quality-control.html) for the full picture (both sides, plus the in-code
builders); the read side is `parquet_open_reader(reader, filename, schema=..., qc=...,
qc_soft=...)`, which by default hard-aborts on a violation (`qc_soft=.true.` instead warns and
continues).

## Reading table metadata with `parquet_get_metadata`

`parquet_get_metadata(reader, key, value [, default] [, warn])` reads one table-level metadata entry back out of a file — the read-side counterpart to the writer's `schema%add_metadata` (see [Runtime table metadata](../schema/building-schema-in-code.html#runtime-table-metadata-schemaadd_metadata-and-schemaclear_metadata)). It is generic: the declared type/kind of `value` (a scalar or 1D array of any [supported type](../types/supported-data-types.html)) selects the variant and how the stored text is parsed back, since all metadata is stored as strings in the file. Table metadata is read once, at `parquet_open_reader` time, and cached, so each call only scans that in-memory copy. Any key present in the file works, including the reserved/internal ones the writer emits (`DATE`, `name`, per-column `column.<name>.*`, per-keyword `<KEY>.datatype`, ...), not just keys added via `add_metadata` — see [How table-level keys become metadata entries](../schema/maml-format.html#how-table-level-keys-become-metadata-entries) for which MAML keys produce which entries.

A **Fortran reader never needs the `<KEY>.datatype` entries.** They record what a typed `schema%add_metadata` call stored (see [A typed value records its own type](../schema/building-schema-in-code.html#a-typed-value-records-its-own-type)), and here the declared type of `value` already selects the parse, so `parquet_get_metadata` neither consults them nor checks itself against them. They exist for readers in languages where a value's type cannot be declared at the call site, and they are readable like any other key: `call parquet_get_metadata(reader, "NSIDE.datatype", token)`.

- A **missing** `key` triggers `error stop`, unless the optional `default` (same type/kind as `value`) is given, in which case `value` is set to it.
- A **present but unconvertible** stored value (e.g. non-numeric text, or an integer too large for the requested kind) always prints a `WARNING`, then falls back to `default` if given, else `error stop`.
- `warn` (optional `logical`, default `.true.`) only governs the missing-key-with-`default` case; pass `warn=.false.` to suppress that warning. It has no effect when the key is present.

### Listing every metadata entry

`parquet_get_metadata` answers for a key you already know. To find out what a file carries at all, `parquet_get_metadata_items(reader, keys, values)` hands back both, index-aligned and in the file's own order:

```fortran
character(len=:), allocatable :: keys(:), values(:)

call parquet_get_metadata_items(reader, keys, values)
do i = 1, size(keys)
    print *, trim(keys(i)), " = ", trim(values(i))
end do
```

Each array is allocated to its own longest entry and blank-padded, so `trim()` is how you use them — the same convention `parquet_get_column_names` follows. Both come back zero-size for a file with no metadata. Like `parquet_get_metadata` it reads nothing: the answer comes from the copy made when the reader was opened. This is what copying metadata from one file to another is built on — see [`parquet_write_table`'s `copy_metadata=`](../tables/table-write.html#carrying-the-source-files-metadata-to-the-output).

## Streaming/chunked reads

`parquet_read_column` reads a whole column into one complete array — fine for most data, but not for a column too large to hold in memory that way. `parquet_read_column_chunk` reads such a column one Parquet row group at a time instead, so peak memory is bounded by a row group's worth of data rather than the whole column — the read-side mirror of [streaming/chunked writes](writing.html#streamingchunked-writes):

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

`parquet_get_num_row_groups(reader, num_row_groups)` returns the file's row-group count. `parquet_get_chunk_size(reader, chunk_size, row_group=)` returns row group `row_group`'s (1-based) own physical row count — row groups are not guaranteed uniform, so query each one rather than assuming they all match the first; `row_group` is optional and defaults to the first row group. `parquet_read_column_chunk(reader, name, row_group, values)` then reads that row group's rows into `values`, dispatched by its actual/declared type/kind and rank (scalar `values(:)` or matrix `values(:,:)`) exactly like `parquet_read_column`, and separately by `row_group`'s own kind (`integer(int32)`/`integer(int64)`, the latter only needed for a file with more row groups than `huge(1_int32)`).

**Stateless, random access:** unlike the write side's `parquet_new_row_group`/`parquet_finish_row_group` pairing, there is no "currently open" row group to track — call `parquet_read_column_chunk` with any `row_group`, in any order, as many times as you like, for any column, independent of any other chunked read on the same reader.

**Type matching:** like `parquet_write_column_chunk` (and unlike `parquet_write_column`/`parquet_read_column`), there is no cross-numeric-type conversion on this path — `values`' own kind must match the column's actual stored type exactly.

**Compact string columns:** a scalar `string` column can also be chunk-read into a `type(parquet_string_column)` (`values` is cleared, then filled with just that row group's rows) — see [Reading and writing compact string columns](../types/string-columns.html#reading-and-writing-compact-string-columns).

**Works on a filtered or sampled reader, but not on a sorted one** (see [what a sort disallows](filter-sort-sample.html#what-a-sort-disallows)). A chunked read on a reader opened with [`filter=`](filter-sort-sample.html#row-filtering-with-parquet_filter) and/or [`sample_fraction=`](filter-sort-sample.html#random-downsampling-with-sample_fraction) hands back that row group's *surviving* rows, and `parquet_get_chunk_size` reports that same count — so the sizes still sum to `parquet_get_nrows`, and a chunked loop needs no separate bookkeeping:

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

A row group whose rows were all filtered away yields **zero** rows — the normal case under a selective filter, not an error: `parquet_get_chunk_size` returns 0 and the read is a no-op. It still counts as read for `check_complete` below.

**Memory-bounded filtering with a row-group scope.** By default, applying a filter reads each filter column whole-file (in one batched, thread-parallel pass) and keeps it decoded, which is fastest but costs one full copy of those columns. `parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi)` instead evaluates the expression one row group at a time over that inclusive 1-based range, releasing each chunk before reading the next — peak memory is one row group's worth of the filter columns rather than the whole file. Rows outside the range never match, and nothing is left cached, so a filter column read afterwards is read again. Pair it with a chunked loop over the same row groups to filter a file larger than memory:

```fortran
call parquet_open_reader(reader, "huge.parquet")
call parquet_reader_set_filter(reader, filt, 5, 8)   ! only row groups 5..8 are examined
```

**Narrowing to an exact row range.** A row-group range can only ever begin and end on a row-group boundary, so a caller interested in an arbitrary row range would get back every survivor of the *covering* row groups with no way to trim them — only the mask knows which physical rows those are. A four-argument form adds the row range itself: `parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi, row_lo, row_hi)`, where `row_lo`/`row_hi` are 1-based, inclusive, physical file rows. Rows outside them never match, so `parquet_get_nrows` afterwards is that range's own surviving count:

```fortran
call parquet_reader_set_filter(reader, filt, 2, 3, 5, 8)   ! rows 5..8, which lie inside row groups 2..3
```

The filter may hold no rules at all in this form, in which case the range alone decides which rows match — that is how a row scope is installed for its own sake, with no expression to hang it on. Both integer kinds are accepted, as for the row-group bounds.

**Read-time qc still runs, scoped to one row group at a time:** if the reader was opened with [`qc=.true.`](../schema/quality-control.html#read-side-enforcement), each `parquet_read_column_chunk` call runs the usual `qc: min:`/`max:`/`miss:` checks against just that row group's own data, not the whole column. In hard mode (`qc_soft=.false.`, the default), a violation aborts immediately, naming the offending row group (`qc violation for column 'name [row group N]'...`). In soft mode (`qc_soft=.true.`), a violation prints a `WARNING` — still at most once per column for the reader's whole lifetime (the same throttling `parquet_read_column` already uses), so reading many violating row groups in soft mode doesn't spam one warning per chunk.

**Completeness checks:** pass `check_complete=.true.` to `parquet_close_reader` to verify that every column you read via `parquet_read_column_chunk` had *every* one of the file's row groups read by the time you close — catches a loop that forgot a row group, or exited early by mistake:

```fortran
call parquet_close_reader(reader, check_complete=.true.)
```

`check_complete` defaults to `.false.` (no check, so existing code is unaffected). When it's `.true.`, `check_hard` (default `.true.`) picks the failure mode: `error stop` naming the column and its missing row group(s), or (`check_hard=.false.`) a `WARNING` instead. Only columns actually touched via `parquet_read_column_chunk` are tracked — a column read via `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` is never included in this check, even if the reader also chunk-read other columns.

**Threading:** `parquet_read_column_chunk` calls on the *same* `parquet_reader` are bound by the same "one thread at a time" rule as every other call into a shared reader (see [Thread safety](../operating/thread-safety.html#rules-at-a-glance)) — but since chunked reads are stateless/random-access, splitting the row-group loop itself across threads works cleanly as long as each thread uses its *own* `parquet_reader` instance opened on the same file (independent readers on the same file are always safe to use concurrently), rather than sharing one reader across threads.
