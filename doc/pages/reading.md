---
title: Reading parquet files from your Fortran code
---

To use this library in another Fortran project, add it as an FPM dependency — see
[Minimal setup to depend on this library](../index.html#minimal-setup-to-depend-on-this-library) in
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
  [Reading and writing compact string columns](string-columns.html#reading-and-writing-compact-string-columns).
  Not sure how long is long enough? `parquet_get_string_length` answers that directly — see
  [Column shape and size queries](#column-shape-and-size-queries) below.
- For vector columns, allocate 2D arrays with shape `(col_size, nrows)`.
- A `DATE`/`TIME`/`TIMESTAMP` column reads into a `parquet_date`/`parquet_time`/`parquet_timestamp`
  array instead — see [Date, time and timestamp columns](date-time.html); these three carry their
  own null state, so `null_value=`/`is_valid=` don't apply to them.
- A vector column may be stored on disk either as a `fixed_size_list` (what this library's own writer emits) or as a variable-length `list<element>` (the Parquet `LIST` layout many other producers use — including its legacy 2-level and non-standard inner-element-name variants, which Arrow's reader normalizes to the same `list` type); both are read back identically. The only requirement is that every row's vector has the same length (so it fits the `(col_size, nrows)` shape); a genuinely ragged `list` column (rows of differing length) is rejected with `error stop`. `col_size` is inferred from the data in either case.

## The `nrows=` shortcut

`parquet_open_reader(..., nrows=nrows)` is a shortcut for the `parquet_open_reader` + `parquet_get_nrows` pair above: pass an `integer(int32)` or `integer(int64)` variable (a plain default `INTEGER` works too, on the vast majority of platforms where that's the same kind as `int32`) as `nrows` and it's filled in for you, equivalent to calling `parquet_get_nrows(reader, nrows, check_positive=.true.)` immediately after opening (post-filter, if a `filter` was also given). Because it implies `check_positive=.true.`, a file (or filter result) with **zero rows fails immediately with `error stop`** — it does not return `nrows=0`. This is the right choice when your code has no sensible zero-row behavior and would rather abort loudly than proceed with an empty read. If zero rows is a case you need to detect and handle (rather than treat as a hard error), don't pass `nrows=` to `parquet_open_reader` — open the reader as usual and call `parquet_get_nrows(reader, nrows)` yourself afterwards (without `check_positive`), which returns `nrows=0` instead of aborting, exactly like the code example above. As with a direct `parquet_get_nrows(reader, nrows)` call, an `integer(int32)` (or default-`INTEGER`) `nrows` also fails immediately with `error stop` if the actual row count overflows `int32`'s range, rather than silently wrapping or truncating.

## Reading only touches the columns you ask for

`parquet_open_reader` only parses the file's footer (schema, row count, row-group layout) — it does not read or decompress any column's data. Each column is read from disk only the first time you ask for it (`parquet_read_column`, `parquet_get_string_length`, etc.), then cached for the lifetime of that `reader`; asking for it again doesn't re-read it, and columns you never ask for are never read at all. This follows from Parquet's layout — each column is its own contiguous byte range, so the reader seeks straight to just the bytes it needs, regardless of [compression codec](writing.html#writer-options). So opening a large file with many columns and reading only a handful is cheap in both I/O and memory, no matter how large the unrequested columns are. `parquet_get_col_size`/`parquet_get_column_total_elements` are cheaper still for the common case (a vector column stored as `fixed_size_list`): they answer straight from the schema/footer, reading no column data at all — so they're safe to call even on a column whose total element count (`nrows * col_size`) is enormous. `parquet_read_array_row_mode` reads only the one row group the requested row lives in, not the whole column. `parquet_read_array_element_mode` can't limit itself to one row group the way `parquet_read_array_row_mode` does — it inherently needs every row's value at the same element position, so every row group contributes — but it still avoids ever materializing the whole column's flattened element count in a single internal call, by streaming the file row group by row group instead. A [row filter](#row-filtering-with-parquet_filter) or [`sample_fraction`](#random-downsampling-with-sample_fraction) doesn't change either of these: `row_index`/`elem_index` then address the filtered result, and each row group's *surviving* row count is what they are resolved against. (Chunked reads work per row group on a filtered reader too — see [Streaming/chunked reads](#streamingchunked-reads).)

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

Both take the same `null_value=`/`is_valid=` pair as `parquet_read_column` (see [Null values](supported-data-types.html#null-values)), both dispatch on `values`' declared type/kind exactly like `parquet_read_column` does (including the [widened-numeric-kind](supported-data-types.html#reading-a-column-into-a-different-numeric-kind) conversions), and both accept a struct-nested dotted `name` (see [Reading a nested struct field](supported-data-types.html#reading-a-nested-struct-field)).

**When to prefer these over `parquet_read_column`:** row mode is the right tool when you need a handful of specific rows out of a huge vector column and don't want to allocate/read the whole `(col_size, nrows)` array just to throw most of it away. Element mode is the right tool when you need one column of a vector column's own internal structure (e.g. always the first element of each row) across the whole file, without allocating the full 2D array.

**I/O behavior** (see [Reading only touches the columns you ask for](#reading-only-touches-the-columns-you-ask-for) above for the full explanation): row mode reads only the one row group the requested row falls in; element mode streams the file row group by row group, so neither ever materializes the whole column in a single internal call. A row filter and/or `sample_fraction` doesn't change that — `row_index`/`elem_index` then address the filtered result, resolved against each row group's surviving row count. A [`sort_by=`](#reading-rows-in-sorted-order-with-parquet_sortkey) does: sorted row `i` belongs to no single row group, so both modes fall back to reading the whole column. Neither is single-row I/O in the strictest sense (both still decode a full row group's worth of data at a time), but both are genuine random access at row-group granularity rather than a whole-column read.

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

All three accept a struct-nested dotted `name` (see [Reading a nested struct field](supported-data-types.html#reading-a-nested-struct-field)) and fail immediately with `error stop` if `name` doesn't exist.

## Checking column existence and type

`parquet_column_exists(reader, name, types)` returns `.true.`/`.false.` for whether `name` (a
top-level or dotted [struct-leaf path](supported-data-types.html#reading-a-nested-struct-field), same as everywhere else)
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
[Reading a column into a different numeric kind](supported-data-types.html#reading-a-column-into-a-different-numeric-kind))
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

`parquet_prefetch_columns(reader, names)` reads several named columns in one call, filling the same per-column cache `parquet_read_column` otherwise populates lazily. Because this library keeps Arrow's `use_threads` on (see [Thread-pool tuning](thread-safety.html#thread-pool-tuning)), decoding them together lets Arrow use its thread pool concurrently instead of one column at a time — purely a throughput optimization, never a requirement: a column you don't prefetch still works via the normal lazy path. Every name is validated against the file's schema first; an unknown column fails immediately with `error stop`, naming it. Calling it more than once is safe and efficient — already-cached columns are not re-read, so the cache ends up holding the union of every name across all calls.

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

`prefetch` is optional and defaults to `.false.`. It behaves exactly as if you called `parquet_prefetch_columns` for every column in the file immediately after opening (same batched, thread-parallel decode, same "already-cached columns are skipped" behavior), and composes correctly with `filter`: prefetching happens *after* the filter is applied, so every prefetched column — not just the ones the filter itself references — reflects the filtered row set. Because it eagerly materializes the whole file in memory at open time, before you've asked for anything, it trades memory and up-front latency for guaranteeing no column read is ever a cache miss later — see [Performance and memory](performance.html).

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
  | `prefetc` | `yes` if this column was named in a `parquet_prefetch_columns` call, `no` otherwise. |
  | `read` | `yes` if this column was actually read via a typed read call, `no` otherwise (e.g. prefetched but never read). |

```fortran
call parquet_open_reader(reader, "data.parquet")
call parquet_prefetch_columns(reader, ["ra ", "mag"])
call parquet_read_column(reader, "ra", ra)
call parquet_close_reader(reader, print_stat=.true.)
```

Requires linking Arrow's `arrow_compute` library (for the min/max calculation) in addition to `arrow`/`parquet` — see [Environment variables](../index.html#environment-variables); `parquet-fortran`'s own `fpm.toml` already declares this link, and fpm propagates it to consuming projects automatically, so no action is normally needed.

## Row filtering with `parquet_filter`

`parquet_open_reader(reader, filename, filter=filt)` restricts a reader to only the rows matching a `type(parquet_filter)`. Once a filter is set, it is completely transparent to everything else: `parquet_get_nrows`, `parquet_read_column`, `parquet_prefetch_columns`, and `parquet_close_reader(..., print_stat=.true.)` all behave exactly as if the file only ever contained the matching rows — there is no separate "filtered count" to track yourself.

```fortran
type(parquet_filter) :: filt
type(parquet_reader) :: reader
integer(int32) :: nrows
integer(int32), allocatable :: ra(:)

call filt%add("(ra > 180 and ra <= 360) or ra is_null")
call filt%add("id is_not_null")

call parquet_open_reader(reader, "data.parquet", filter=filt)
call parquet_get_nrows(reader, nrows)      ! already the filtered row count
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already just the matching rows
call parquet_close_reader(reader)
```

### The rule grammar

Each `filt%add(rule)` call contributes one boolean **expression** over the file's columns. An expression is either a single clause or several clauses combined with `and`, `or`, `not` and parentheses:

```
   expr     := or_expr
   or_expr  := and_expr { or and_expr }
   and_expr := not_expr { and not_expr }
   not_expr := [ not ] not_expr | primary
   primary  := '(' expr ')' | clause
   clause   := <column> <op> [ <value> ]
```

| | |
|---|---|
| **Precedence** | `not` binds tightest, then `and`, then `or` — so `a or b and c` means `a or (b and c)`, and `not a and b` means `(not a) and b`. Parentheses override. |
| **Keywords** | `and` / `or` / `not`, in any case (`AND`, `And`, `and`). Only whole tokens are keywords, so a column named `android` or `nothing` is unaffected. Fortran-style `.and.` and C-style `&&` are *not* accepted. |
| **Operators** | `>`, `>=`, `<`, `<=`, `==`, `/=`, `is_null`, `is_not_null`, `is_nan`, `is_not_nan`. A clause's operator must be surrounded by spaces (`"v > 3"`, not `"v>3"`); parentheses need no surrounding spaces. `is_nan`/`is_not_nan` are accepted only for a floating-point column (`float32`/`float64`, and a `half_float` column written by some other tool) — any other column type is rejected, since no value of it could ever be a NaN. |
| **Values** | A bare number for a numeric column (`ra > 180`), `true`/`false` for a boolean column (`flag == true`), a **double-quoted** string for a `string` column (`name == "abell_1"`), or a **double-quoted ISO-8601 literal** for a `date`/`time`/`timestamp` column (see below). `is_null`/`is_not_null`/`is_nan`/`is_not_nan` take no value. A quoted value may contain spaces, parentheses, and the keywords themselves — it is read as one token. An `inf`/`-inf` value is accepted as an ordinary bound (`v < inf`); a bare `nan` is **rejected**, because every comparison against a NaN is false and every `/=` against it is true, so such a clause could only ever match nothing or everything — say `is_nan`/`is_not_nan` instead. |
| **Column names** | May be a dotted struct-leaf path (`main.inner.age > 35`). A column name cannot contain spaces. |
| **Several `%add` calls** | **AND-combined**: two calls mean `(expr1) and (expr2)`. This keeps every filter written as one clause per call meaning exactly what it always did; write `or` inside a single rule when you want alternatives. |
| **Limits** | 32 levels of nesting, 1024 expression terms per filter, 8192 characters per rule — each reported as a clean error rather than a crash. |

`in`, `between` and wildcard/`like` matching are deliberately not supported: the first two are shorthand for what the grammar already expresses (`x in (1,2,3)` is `x == 1 or x == 2 or x == 3`; `x between 1 and 9` is `x >= 1 and x <= 9`), and pattern matching is genuinely different work.

### Null values follow SQL's three-valued logic

A comparison against a Null is neither true nor false but **unknown**, and only rows that come out *true* survive:

| expression | a row whose `x` is Null |
|---|---|
| `x > 5` | excluded (unknown) |
| `not x > 5` | **still excluded** — negating unknown is unknown, not true |
| `x > 5 or y > 5` | survives only if `y > 5` is true |
| `x is_null` | survives |
| `not x is_null` | excluded; identical to `x is_not_null` |

`is_null`/`is_not_null` are the only operators that answer true/false for a Null row, so they are the only way to select one. This matches SQL's `WHERE` clause and Arrow's own kernels — in particular, `not` does **not** let Null rows in through the back door.

Two consequences worth stating outright, because filters are usually written assuming them without checking:

- A row whose filter column is Null can never be brought in by a *comparison* on that column — only by an explicit `is_null` on it, or by some other column's clause being true on the other side of an `or`.
- So a filter that mentions one column and never says `is_null` returns no rows with a Null in that column, whatever operators and negations it uses.

### NaN is a value, not a Null

A NaN in a floating-point column is an ordinary **value** that happens to compare false against everything, so it follows IEEE rules rather than the three-valued rules above — and behaves as almost the *opposite* of a Null:

| expression | a row whose `x` is NaN | a row whose `x` is Null |
|---|---|---|
| `x > 5`, `x >= 5`, `x < 5`, `x <= 5`, `x == 5` | excluded (every IEEE comparison against NaN is false) | excluded (unknown) |
| `x /= 5` | **survives** — `NaN /= 5` is true | excluded (unknown) |
| `not x > 5` | **survives** — false negates to true | excluded (unknown negates to unknown) |
| `x is_not_null` | survives — a NaN is not missing | excluded |
| `x is_nan` | survives | excluded (unknown) |
| `x is_not_nan` | excluded | excluded (unknown) |

`is_nan`/`is_not_nan` say this directly, on a floating-point column:

```fortran
call filt%add("flux is_not_nan")             ! only rows whose flux is a real number
call filt%add("flux is_nan or flux is_null") ! only the rows with no usable value
```

Both are Kleene-honest about nullness — a Null row is *unknown* for either of them, exactly as it is for a comparison. So `x is_not_nan` means "`x` is a real number", not "`x` is anything other than a NaN": write `x is_not_nan or x is_null` when a missing value should count too. Nullness stays governed solely by `is_null`/`is_not_null`.

Neither operator adds any expressive power the grammar lacked — under Kleene logic `not (x >= 0 or x < 0)` already meant exactly `x is_nan`, since every non-NaN real satisfies precisely one of the two disjuncts — but writing that out is easy to get wrong and hard to read, which is what these two are for.

### Filtering `date`, `time` and `timestamp` columns

A temporal column is compared against a double-quoted ISO-8601 literal, which is converted into the column's own stored unit:

```fortran
call filt%add('obs_date >= "2024-01-31"')
call filt%add('obs_time < "12:30:00"')
call filt%add('obs_ts == "2024-01-31T12:30:00"')
```

- A **less** precise literal is fine: a date-only literal against a `timestamp` column means midnight of that date.
- A **more** precise literal is an error, not a silent truncation: `"2024-01-31T12:30:00.123456"` against a `timestamp[ms]` column is rejected, because that column cannot represent the value being asked about. So is any literal with a time part against a `date` column.
- An **unquoted** value against a temporal column is rejected too — a bare number would mean days for one column and microseconds since the epoch for another, with nothing in the rule to say which.
- **Timezones are not interpreted.** A `parquet_timestamp` holds the stored epoch offset verbatim (see [Dates and times](date-time.html)), so a literal is read as a civil date/time and compared against the same stored instants a read returns.

### Validation and cost

Only plain scalar columns can be filtered — naming a vector (`col_size > 1`) column in a rule fails immediately with `error stop` when `parquet_open_reader` is called. So does naming a column that doesn't exist in the file, or a rule with invalid syntax (an unbalanced parenthesis, a dangling `and`, an unknown operator, an unquoted string value, a non-numeric value against a numeric column, ...) — every rule is fully parsed and validated (column existence, type-compatibility, and value parsing) right there in `parquet_open_reader`, before any of your own code runs.

The benefit is mostly downstream: `parquet_get_nrows` and every column you read only ever reflect the matching rows, so your own code loops over, allocates for, and processes far fewer rows when the filter is selective — at the cost of a small transient memory bump while a column's decoded array and its filtered result briefly coexist, before the unfiltered one is discarded. (The `nrows=` shortcut mentioned [above](#the-nrows-shortcut) works here too — it reflects the post-filter row count.)

### Row groups a filter cannot match are never read

A filtered reader consults each row group's own **footer statistics** (the per-column-chunk min, max and null count Parquet records) before reading anything, and skips the row groups those statistics prove cannot contain a matching row. The row groups that survive are then read and evaluated exactly, since min/max can only ever prove impossibility, never a match.

This is automatic, has no option to turn it on, and **changes nothing about the result** — the same rows come back in the same order, with the same nulls. The only difference is how much of the file was read to produce them, and it applies to the whole read, not just the filter's own columns: a payload column you read afterwards skips the same row groups.

```fortran
type(parquet_filter) :: filt
call filt%add("id == 8123456")
call parquet_open_reader(reader, "big.parquet", filter=filt)  ! 400 row groups
call parquet_read_column(reader, "flux", flux)                ! may read 1 of them, not 400
```

How much this saves depends entirely on how the file is laid out. A column whose values are **clustered by row group** (written in sorted order, or naturally grouped like an observation date) prunes well; one whose values are scattered uniformly gives every row group the same wide min/max, and nothing can be ruled out. Measured on a 4M-row, 9-column file in 40 row groups: a selective `id == …` read went from 0.53 s to 0.10 s, while a filter matching every row was unchanged.

`parquet_close_reader(reader, print_stat=.true.)` reports a `screened:` line when it skipped anything, which is the way to tell whether it engaged on your data.

Some cases can never prune, and fall back to reading everything — correctly, just without the saving:

- a file written **without statistics** (or a column chunk missing them);
- a column whose min/max this library declines to interpret: an **unsigned** integer, `decimal`, `half_float`, or any column whose declared Parquet sort order is not one of signed or unsigned-byte;
- `is_nan`/`is_not_nan`, and a **floating-point** column under `not` or `/=` — Parquet excludes NaN from min/max and records no NaN count, so for a float column the statistics can never prove a comparison is false everywhere (see [NaN is a value, not a Null](#nan-is-a-value-not-a-null));
- a column whose values are longer than Parquet's statistics size limit (4096 bytes by default), for which no bounds are recorded at all.

Two things pruning deliberately does **not** change: `parquet_get_num_row_groups` still reports every row group in the file, and a chunked read still visits every one of them (a skipped row group reads as an empty chunk, exactly as a row group the filter emptied already did). Pruning is an I/O optimization, not a view of the file.

One caveat worth stating: this trusts the file's own footer. A file whose statistics are *wrong* — written by a tool that recorded bounds not matching its data — will give a wrong answer, in the same way `parquet_column_has_nulls` already trusts the recorded null count.

### Applying a filter after the reader is open

`parquet_reader_set_filter(reader, filt)` applies a filter to an already-open reader, with exactly the same result as having passed `filter=` to `parquet_open_reader`. It exists for callers that do not own the `parquet_open_reader` call, and for filters that can only be built once the file's schema or metadata has been inspected:

```fortran
call parquet_open_reader(reader, "data.parquet")
if (parquet_column_exists(reader, "quality")) call filt%add("quality > 0.9")
call parquet_reader_set_filter(reader, filt)
```

It refuses, with `error stop`, in two situations: when the reader **already has a filter** (combine the clauses into one `parquet_filter` instead — several `%add` calls are AND-combined), and when **any column has already been read** on that reader, since data already handed back covers the unfiltered rows and could not be lined up with anything read afterwards. A reader opened with `sample_fraction=` is fine — the filter applies on top of the sample, exactly as passing both to `parquet_open_reader` does.

## Reading rows in sorted order with `parquet_sortkey`

`parquet_open_reader(reader, filename, sort_by=srt)` returns the file's rows ordered by one or more of its columns, instead of in physical file order. Every column read afterwards comes back in that order, so there is no separate "sorted index" to carry around:

```fortran
type(parquet_reader)  :: reader
type(parquet_sortkey) :: srt
integer(int64) :: nrows
real(real64), allocatable :: ra(:)

call srt%add("ra asc")
call srt%add("dec desc")
call parquet_open_reader(reader, "cat.parquet", sort_by=srt)
call parquet_get_nrows(reader, nrows)      ! unchanged: sorting reorders rows, never adds or removes
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already in (ra asc, dec desc) order
call parquet_close_reader(reader)
```

### Sort keys

One key per `%add` call, applied in the order added — the first key is the primary one, later keys break its ties:

| key text | meaning |
|---|---|
| `"ra"` | order by `ra`, ascending (the default) |
| `"ra asc"` / `"ra ascending"` | the same, spelled out |
| `"dec desc"` / `"dec descending"` | order by `dec`, descending |
| `"-dec"` | shorthand for `"dec desc"` |
| `"main.inner.age"` | a dotted struct-leaf path is a valid key |

Direction words are case-insensitive. Combining the `-` shorthand with an explicit direction (`"-dec desc"`) is rejected rather than silently resolved, since it reads equally as agreement or as cancellation.

### Null and NaN placement

Nulls sort **last** by default. Pass `nulls_first=.true.` on `%add` to move that one key's nulls to the front instead — it is per key, not per sort:

```fortran
call srt%add("quality desc", nulls_first=.true.)
```

`NaN` sits between the real values and the nulls, so an ascending float key gives *values, then NaNs, then nulls*.

**Placement is absolute: ordering a key descending reverses its values, it does not move its nulls or NaNs.** So `"v desc"` yields the largest value first and still ends with the nulls. This reproduces Arrow's own sort ordering exactly, which means a result cross-checked against `pyarrow` matches row for row.

Rows that tie on **every** key keep their original file order (the sort is stable).

### What can be sorted, and what it costs

Any scalar column can be a sort key: `int32`/`int64`, `float32`/`float64`, `boolean`, `string`, and `date`/`time`/`timestamp`. A **vector** (`col_size > 1`) column has no single value per row to order by and is rejected with `error stop` — from the schema, before any data is read. So is a key naming a column the file doesn't have, or one whose text doesn't parse.

A sort key column is always read **whole**: a global order needs every row, so there is no row-group-scoped equivalent the way there is for filtering. That is the one place sorting costs memory that filtering does not. Sorting itself never skips I/O — the benefit is that your own code receives the rows already ordered. (Composed with `filter=`, the key column is read over the surviving row groups only, since the filter's pruning applies to every column read after it.)

### Sorting composes with filtering and sampling

Give both and the filter runs **first**, then the sort orders the surviving rows:

```fortran
call filt%add("mag < 20")
call srt%add("mag asc")
call parquet_open_reader(reader, "cat.parquet", filter=filt, sort_by=srt)
```

`sample_fraction=` behaves the same way. `parquet_close_reader(..., print_stat=.true.)` prints the keys as applied, on their own `sort:` line.

### What a sort disallows

A sort permutation destroys row-group locality — sorted row 5 may come from row group 47 and row 6 from row group 3 — so anything row-group-scoped fails with `error stop` while a sort is active:

- [`parquet_read_column_chunk`](#streamingchunked-reads) and `parquet_get_chunk_size`.

This is **inherent to sorting**, not a limitation to be lifted later: there is no coherent "row group N of the sorted output" to hand back. It is also the one respect in which sorting differs from filtering, which supports both — a filter only ever *removes* rows, so row groups stay contiguous, while a sort *reorders* them.

`parquet_read_array_row_mode` and `parquet_read_array_element_mode` still work, but they fall back to reading the whole column, since the row they are asked for no longer belongs to any one row group. `row_index`/`elem_index` then address the **sorted** result, as everything else does.

### Applying a sort after the reader is open

`parquet_reader_set_sort(reader, srt)` sorts an already-open reader, with the same result as passing `sort_by=` to `parquet_open_reader` — the counterpart of [`parquet_reader_set_filter`](#applying-a-filter-after-the-reader-is-open), and for the same reasons. Any column already decoded (by `prefetch=`, say) is reordered too.

It refuses, with `error stop`, when the reader **already has a sort** (add every key to one `parquet_sortkey` instead) and when **any column has already been read** on that reader.

## Renaming the columns a filter or sort refers to

`filt%remap_column_names(from, to)` and `srt%remap_column_names(from, to)` rewrite, in place, the columns a filter's rules or a sort's keys refer to: every reference to `from(k)` becomes `to(k)`. The two arrays are parallel and must be the same size.

```fortran
type(parquet_filter) :: filt
call filt%add("mass > 1.0e12 and (redshift < 0.5 or flag is_null)")
call filt%remap_column_names(["mass    ", "redshift"], ["m_200c  ", "z       "])
! the filter now reads: (m_200c > 1.0e12 and (z < 0.5 or flag is_null))
```

This exists for callers that build a filter in one column-name vocabulary and must apply it in another. The main user is [`parquet_table`](table.html), which lets a program write filters in the table's own internal names and translates them into the file's physical names — see [Renaming a file's columns for reading](maml-format.html#renaming-columns-for-reading-with-extra-remap) — but any caller with the same split can use it directly.

Three properties are worth knowing:

- **It renames column references only.** The substitution happens on the *parsed* expression, after the parser has already separated column names from operators, keywords, parentheses and quoted literals — so a string value that happens to spell a column name (`name == "mass"`) is never touched. Sort keys keep their direction and their `nulls_first` setting.
- **It never fails on a name it does not recognize.** A `from` entry that no rule mentions is a no-op, and a rule naming a column absent from `from` is left alone rather than rejected: only the caller knows which names are supposed to exist. A rule that does not parse is also left untouched, so the reader that applies it still reports the parse error, with the file named.
- **All the renames in one call happen at once.** Passing `from = ["a", "b"]`, `to = ["b", "a"]` swaps the two columns; it does not rename `a` to `b` and then everything named `b` back to `a`.

It does `error stop` in three cases: `from` and `to` differing in size, a replacement name longer than the 64-character column-name limit, and — filters only — a rule that fitted `filter_max_rule_len` in its original names but no longer does once renamed.

## Random downsampling with `sample_fraction`

`parquet_open_reader(reader, filename, sample_fraction=0.1_real64)` keeps each row independently with probability `sample_fraction` (Bernoulli sampling) — like `filter=`, it narrows what every subsequent call sees (`parquet_get_nrows`, `parquet_read_column`, `parquet_close_reader(..., print_stat=.true.)`, ...), with no separate "sampled count" to track:

```fortran
type(parquet_reader) :: reader
integer(int64) :: nrows
integer(int32), allocatable :: id(:)

call parquet_open_reader(reader, "data.parquet", sample_fraction=0.1_real64, sample_seed=42)
call parquet_get_nrows(reader, nrows)      ! already the post-sample row count
allocate(id(nrows))
call parquet_read_column(reader, "id", id) ! already just the sampled rows
call parquet_close_reader(reader)
```

- `sample_fraction` (`real(real64)`, optional): omitted, or `>= 1.0`, reads every row — the current/default behavior. Must not be negative or `NaN` — either aborts immediately with `error stop`. Exactly `0.0` deterministically yields zero rows (not just with overwhelming probability).
- `sample_seed` (`integer(int32)`, optional): omitted, or `<= 0`, draws a fresh seed from entropy — a different sample each time you open the file. A positive value makes the draw reproducible: the same `sample_fraction`/`sample_seed` pair always selects the exact same rows. Whichever seed actually gets used (caller-supplied or entropy-drawn) is always reported by `parquet_close_reader(..., print_stat=.true.)` (a `sample: fraction=... seed=...` line) — read it back from there to reproduce a run you didn't originally seed yourself.
- Bernoulli sampling means the matched row count fluctuates around `sample_fraction * nrows` rather than equaling it exactly (most noticeable on small files) — there is no "select exactly N rows" mode.
- Sampling and `filter=` share the same underlying mechanism: give both, and the filter is applied on top of the downsample (a row must pass both to be kept). Consequently, `sample_fraction < 1.0` behaves exactly as `filter=` does throughout, even with no `filter=` given at all — `parquet_read_column_chunk`, `parquet_read_array_row_mode` and `parquet_read_array_element_mode` all stay row-group-scoped, with row indices and chunk sizes referring to the surviving rows.
- Sampling never skips I/O: every column is fully read and decoded from disk regardless of `sample_fraction`, and unlike `filter=` there are no statistics for a random draw to consult. The benefit is purely to your own code processing fewer rows afterward. (Combining `sample_fraction=` with `filter=` does get the filter's own row-group pruning — the two compose, and a sample only ever removes rows the filter already kept.)

## Quality control

Reading also has its own side of quality control — checking column values against `qc:
min:`/`max:`/`miss:` bounds declared in a MAML file, independent of the write-side checks — plus
two ways to build a qc-maml directly in code without a `.maml` file on disk. See
[Quality control](quality-control.html) for the full picture (both sides, plus the in-code
builders); the read side is `parquet_open_reader(reader, filename, schema=..., qc=...,
qc_soft=...)`, which by default hard-aborts on a violation (`qc_soft=.true.` instead warns and
continues).

## Reading table metadata with `parquet_get_metadata`

`parquet_get_metadata(reader, key, value [, default] [, warn])` reads one table-level metadata entry back out of a file — the read-side counterpart to the writer's `schema%add_metadata` (see [Runtime table metadata](building-schema-in-code.html#runtime-table-metadata-schemaadd_metadata-and-schemaclear_metadata)). It is generic: the declared type/kind of `value` (a scalar or 1D array of any [supported type](supported-data-types.html)) selects the variant and how the stored text is parsed back, since all metadata is stored as strings in the file. Table metadata is read once, at `parquet_open_reader` time, and cached, so each call only scans that in-memory copy. Any key present in the file works, including the reserved/internal ones the writer emits (`DATE`, `name`, per-column `column.<name>.*`, ...), not just keys added via `add_metadata` — see [How table-level keys become metadata entries](maml-format.html#how-table-level-keys-become-metadata-entries) for which MAML keys produce which entries.

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

Each array is allocated to its own longest entry and blank-padded, so `trim()` is how you use them — the same convention `parquet_get_column_names` follows. Both come back zero-size for a file with no metadata. Like `parquet_get_metadata` it reads nothing: the answer comes from the copy made when the reader was opened. This is what copying metadata from one file to another is built on — see [`parquet_write_table`'s `copy_metadata=`](table.html#carrying-the-source-files-metadata-to-the-output).

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

**Compact string columns:** a scalar `string` column can also be chunk-read into a `type(parquet_string_column)` (`values` is cleared, then filled with just that row group's rows) — see [Reading and writing compact string columns](string-columns.html#reading-and-writing-compact-string-columns).

**Works on a filtered or sampled reader, but not on a sorted one** (see [what a sort disallows](#what-a-sort-disallows)). A chunked read on a reader opened with [`filter=`](#row-filtering-with-parquet_filter) and/or [`sample_fraction=`](#random-downsampling-with-sample_fraction) hands back that row group's *surviving* rows, and `parquet_get_chunk_size` reports that same count — so the sizes still sum to `parquet_get_nrows`, and a chunked loop needs no separate bookkeeping:

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

**Read-time qc still runs, scoped to one row group at a time:** if the reader was opened with [`qc=.true.`](quality-control.html#read-side-enforcement), each `parquet_read_column_chunk` call runs the usual `qc: min:`/`max:`/`miss:` checks against just that row group's own data, not the whole column. In hard mode (`qc_soft=.false.`, the default), a violation aborts immediately, naming the offending row group (`qc violation for column 'name [row group N]'...`). In soft mode (`qc_soft=.true.`), a violation prints a `WARNING` — still at most once per column for the reader's whole lifetime (the same throttling `parquet_read_column` already uses), so reading many violating row groups in soft mode doesn't spam one warning per chunk.

**Completeness checks:** pass `check_complete=.true.` to `parquet_close_reader` to verify that every column you read via `parquet_read_column_chunk` had *every* one of the file's row groups read by the time you close — catches a loop that forgot a row group, or exited early by mistake:

```fortran
call parquet_close_reader(reader, check_complete=.true.)
```

`check_complete` defaults to `.false.` (no check, so existing code is unaffected). When it's `.true.`, `check_hard` (default `.true.`) picks the failure mode: `error stop` naming the column and its missing row group(s), or (`check_hard=.false.`) a `WARNING` instead. Only columns actually touched via `parquet_read_column_chunk` are tracked — a column read via `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` is never included in this check, even if the reader also chunk-read other columns.

**Threading:** `parquet_read_column_chunk` calls on the *same* `parquet_reader` are bound by the same "one thread at a time" rule as every other call into a shared reader (see [Thread safety](thread-safety.html#rules-at-a-glance)) — but since chunked reads are stateless/random-access, splitting the row-group loop itself across threads works cleanly as long as each thread uses its *own* `parquet_reader` instance opened on the same file (independent readers on the same file are always safe to use concurrently), rather than sharing one reader across threads.
