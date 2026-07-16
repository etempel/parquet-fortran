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
- For string columns, choose a fixed string length that is large enough for your data.
- For vector columns, allocate 2D arrays with shape `(col_size, nrows)`.
- A vector column may be stored on disk either as a `fixed_size_list` (what this library's own writer emits) or as a variable-length `list<element>` (the Parquet `LIST` layout many other producers use — including its legacy 2-level and non-standard inner-element-name variants, which Arrow's reader normalizes to the same `list` type); both are read back identically. The only requirement is that every row's vector has the same length (so it fits the `(col_size, nrows)` shape); a genuinely ragged `list` column (rows of differing length) is rejected with `error stop`. `col_size` is inferred from the data in either case.

### Reading only touches the columns you ask for

`parquet_open_reader` only parses the file's footer (schema, row count, row-group layout) — it does not read or decompress any column's data. Each column is read from disk only the first time you ask for it (`parquet_read_column`, `parquet_get_string_length`, etc.), then cached for the lifetime of that `reader`; asking for it again doesn't re-read it, and columns you never ask for are never read at all. This follows from Parquet's layout — each column is its own contiguous byte range, so the reader seeks straight to just the bytes it needs, regardless of [compression codec](supported-data-types.html#compression-and-row-group-size). So opening a large file with many columns and reading only a handful is cheap in both I/O and memory, no matter how large the unrequested columns are. `parquet_get_col_size`/`parquet_get_column_total_elements` are cheaper still for the common case (a vector column stored as `fixed_size_list`): they answer straight from the schema/footer, reading no column data at all — so they're safe to call even on a column whose total element count (`nrows * col_size`) is enormous. `parquet_read_array_row_mode` reads only the one row group the requested row lives in (not the whole column), unless a row filter is active, in which case it falls back to reading the whole (filtered) column, since a filter mask has no row-group structure of its own. `parquet_read_array_element_mode` can't limit itself to one row group the way `parquet_read_array_row_mode` does — it inherently needs every row's value at the same element position, so every row group contributes — but, unless a row filter is active, it still avoids ever materializing the whole column's flattened element count in a single internal call by streaming the file row group by row group instead; a row filter falls back to reading the whole (filtered) column, same as `parquet_read_array_row_mode`.

### Prefetching multiple columns at once with `parquet_prefetch_columns`

`parquet_prefetch_columns(reader, names)` reads several named columns in one call, filling the same per-column cache `parquet_read_column` otherwise populates lazily. Because this library keeps Arrow's `use_threads` on (see [Multi-threaded decoding/encoding](supported-data-types.html#multi-threaded-decodingencoding-use_threads-and-thread-pool-size)), decoding them together lets Arrow use its thread pool concurrently instead of one column at a time — purely a throughput optimization, never a requirement: a column you don't prefetch still works via the normal lazy path. Every name is validated against the file's schema first; an unknown column fails immediately with `error stop`, naming it. Calling it more than once is safe and efficient — already-cached columns are not re-read, so the cache ends up holding the union of every name across all calls.

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

### Printing reader statistics with `parquet_close_reader(..., print_stat=.true.)`

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

### Row filtering with `parquet_filter`

`parquet_open_reader(reader, filename, filter=filt)` restricts a reader to only the rows matching a `type(parquet_filter)`. Once a filter is set, it is completely transparent to everything else: `parquet_get_nrows`, `parquet_read_column`, `parquet_prefetch_columns`, and `parquet_close_reader(..., print_stat=.true.)` all behave exactly as if the file only ever contained the matching rows — there is no separate "filtered count" to track yourself.

```fortran
type(parquet_filter) :: filt
type(parquet_reader) :: reader
integer(int32) :: nrows
integer(int32), allocatable :: ra(:)

call filt%add("ra > 180")
call filt%add("ra <= 360")
call filt%add("id is_not_null")

call parquet_open_reader(reader, "data.parquet", filter=filt)
call parquet_get_nrows(reader, nrows)      ! already the filtered row count
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already just the matching rows
call parquet_close_reader(reader)
```

Each `filt%add(rule)` call adds one clause; multiple clauses always combine with AND (there is no OR/NOT) — call `%add` more than once, as above, to express a range or several independent conditions. A `rule` has the shape `"<column> <op> <value>"` or `"<column> is_null"` / `"<column> is_not_null"`:

- Supported operators: `>`, `>=`, `<`, `<=`, `==`, `/=`, `is_null`, `is_not_null`.
- `<value>` is a bare number for a numeric column (`ra > 180`), `true`/`false` for a boolean column (`flag == true`), or a **double-quoted** string for a `string` column (`name == "abell_1"`) — quotes are required for strings and not used for anything else.
- `is_null`/`is_not_null` take no value.
- Only plain scalar columns can be filtered — naming a vector (`col_size > 1`) column in a rule fails immediately with `error stop` when `parquet_open_reader` is called. So does naming a column that doesn't exist in the file, or a rule with invalid syntax (unknown operator, unquoted string value, non-numeric value against a numeric column, etc.) — every rule is fully validated (column existence, type-compatibility, and value parsing) right there in `parquet_open_reader`, before any of your own code runs.

Filtering is **not** predicate pushdown: every filter-referenced column, and every column you subsequently read, is still fully read and decoded from disk exactly as without a filter (Parquet row-group statistics are never used to skip I/O). The benefit is entirely downstream: `parquet_get_nrows` and every column you read only ever reflect the matching rows, so your own code loops over, allocates for, and processes far fewer rows when the filter is selective — at the cost of a small transient memory bump while a column's full decoded array and its filtered result briefly coexist, before the unfiltered one is discarded.

`parquet_open_reader(..., nrows=nrows)` is a shortcut for the `parquet_open_reader` + `parquet_get_nrows` pair above: pass an `integer(int32)` or `integer(int64)` variable (a plain default `INTEGER` works too, on the vast majority of platforms where that's the same kind as `int32`) as `nrows` and it's filled in for you, equivalent to calling `parquet_get_nrows(reader, nrows, check_positive=.true.)` immediately after opening (post-filter, if a `filter` was also given). Because it implies `check_positive=.true.`, a file (or filter result) with **zero rows fails immediately with `error stop`** — it does not return `nrows=0`. This is the right choice when your code has no sensible zero-row behavior and would rather abort loudly than proceed with an empty read. If zero rows is a case you need to detect and handle (rather than treat as a hard error), don't pass `nrows=` to `parquet_open_reader` — open the reader as usual and call `parquet_get_nrows(reader, nrows)` yourself afterwards (without `check_positive`), which returns `nrows=0` instead of aborting, exactly like the code example above. As with a direct `parquet_get_nrows(reader, nrows)` call, an `integer(int32)` (or default-`INTEGER`) `nrows` also fails immediately with `error stop` if the actual row count overflows `int32`'s range, rather than silently wrapping or truncating.

### Read-time quality control with a qc-maml

`parquet_open_reader(reader, filename, schema=..., qc=..., qc_soft=...)` checks column values against `qc: min:`/`max:`/`miss:` bounds declared in a MAML file — mirroring the writer's own [`qc:` range-check feature](supported-data-types.html#quality-control-qc-range-checks-on-write), but on the read side. By default a violation is a **hard error** (`qc_soft=.false.`): the process aborts with a diagnostic on stderr, the same class of clean, deliberate abort as the read-side Null/type-mismatch checks (see [Limitations](../index.html#limitations)). Pass `qc_soft=.true.` to instead **warn and continue**: a `WARNING` is printed to stdout and reading proceeds. Either way, the existing strict-by-default Null behavior (`error stop` on a genuine Null unless `null_value=`/`is_valid=` is passed — see [Null values](supported-data-types.html#null-values)) is completely unchanged.

```fortran
type(parquet_reader) :: reader
integer(int32), allocatable :: ra_back(:)

! qc.maml:
!   fields:
!   - name: ra
!     qc:
!       min: 0
!       max: 360
!   - name: id
!     qc:
!       miss: Null   ! this column is expected to contain genuine Nulls

! Default: a range violation aborts the process.
call parquet_open_reader(reader, "data.parquet", schema=parquet_load_qc_maml_file("qc.maml"))
call parquet_read_column(reader, "ra", ra_back)
! aborts: parquet-fortran: qc hard check: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)

! qc_soft=.true.: the same violation only prints a WARNING to stdout, then continues.
call parquet_open_reader(reader, "data.parquet", schema=parquet_load_qc_maml_file("qc.maml"), qc_soft=.true.)
call parquet_read_column(reader, "ra", ra_back)
! prints: WARNING: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)
```

### Building a qc-maml in code with `add_col_qc` and `set_col_qc`

Instead of authoring a `.maml` file, you can build the qc-maml in memory a column at a time from a compact, comma-separated string, then pass it straight to `parquet_open_reader(..., schema=)`. The `type(parquet_schema)` type provides two forms of the same builder — they append the identical field entry and differ only in how the parsed column name comes back:

- **`call schema%add_col_qc(qc_input [, col_name])`** — the parsed name is returned in the optional `col_name` argument (`character(len=:), allocatable, intent(out)`) when present; omit it to just add the entry.
- **`call schema%set_col_qc(col)`** — in-place: `col` is `intent(inout)` and must already be `character(len=:), allocatable`. It holds the compact `qc_input` string on entry and the parsed column name on exit, so a single variable is reused rather than assigning a result back into it.

```fortran
type(parquet_schema) :: qc
type(parquet_reader) :: reader
character(len=:), allocatable :: col

! two-argument form: name comes back in the (optional) second argument
call qc%add_col_qc("ra, >=0, <=360, Null", col)  ! col is returned as "ra"
call qc%add_col_qc("dec, , <=90")                ! col_name omitted: just add

! in-place form: col holds qc_input on entry, the parsed name on exit
col = "mag, 5"
call qc%set_col_qc(col)                           ! col becomes "mag" (bare 5 => >= 5)

call parquet_open_reader(reader, "data.parquet", schema=qc)
call parquet_read_column(reader, col, mag)        ! reuse the returned name
```

> **Why two forms?** `add_col_qc`'s `col_name` is `intent(out)`, so you must **not** pass the same variable as both arguments (`call maml%add_col_qc(x, x)`) — aliasing an `intent(out)` argument is undefined and corrupts the input. When you want the in-place `x` reused as both input and output, use `set_col_qc`, whose single argument is `intent(inout)` for exactly that — it still **mutates** `maml` (it adds the entry, like `add_col_qc`); it's a builder that also returns the name, not a pure query, hence `set_` rather than `get_`. (Neither form is a function returning `character(len=:), allocatable` — see [Thread safety](string-columns.html#thread-safety) in the string-columns guide for why that matters on some compilers.)

Both forms share the same input format and validation:

- **`qc_input` is `"col_name, qc_min, qc_max, qc_miss"`** — at most four comma-separated fields, matched **positionally**. Only `col_name` (the first field) is required and must be non-empty; any of the last three may be empty or omitted (`"ra, >0"` sets just a min; `"ra, , <=10"` sets just a max; `"ra,,, Null"` sets just miss). The `fields:` header is created automatically on the first call.
- **`qc_min`/`qc_max`** may carry a leading operator (`>=`/`>` for `min`, `<=`/`<` for `max`) or be a bare number (inclusive, i.e. `>=` for min and `<=` for max — same convention as the [write-side `qc:`](supported-data-types.html#quality-control-qc-range-checks-on-write)). A reversed operator (e.g. a `<` on `min`), or an operator with no value after it, fails immediately with `error stop`.
- **`qc_miss`** may only be empty, `Null`/`null`, or `NA`/`na`; anything else fails with `error stop`. (Meaning is exactly as for a file-based qc-maml: `Null`/`NA` ⇒ Nulls expected, empty ⇒ not expected.)
- Adding a column already present in this maml, or supplying more than four fields, also fails immediately with `error stop` — invalid input is never partially applied.
- An **empty (or all-blank) `qc_input` is a no-op**: `col_name` is returned as an empty string and nothing is added to the maml. This is distinct from a leading comma (e.g. `", >0"`), which does have content — an empty *first field* — and is the error above (a missing column name).

- `schema` is optional, `type(parquet_schema)`; `parquet_load_qc_maml_file(filename)` loads one from disk (a separate function from `parquet_parse_maml`, since a qc-maml has different, lighter requirements and is never parsed into `cinfo`/`metadata` — see below). `qc` is optional `logical`: if omitted, it defaults to `.true.` whenever `schema` is supplied and `.false.` otherwise; an explicit `qc=` always wins (so `qc=.false.` with a `schema=` present disables checking entirely, and `qc=.true.` with no `schema=` at all is a harmless no-op, nothing to check).
- `qc_soft` is optional `logical`, default `.false.` (hard: a violation aborts the process). It only ever takes effect when qc is active; with `qc=.false.` (or no `schema=`) it is irrelevant.
- A qc-maml's only required field attribute is `name` — `data_type` and everything else (including `qc:` itself) are optional, unlike a schema-authoring MAML. `qc: min:`/`max:` bounds are parsed against the column's actual Parquet type at read time, not any `data_type` the maml might declare. A qc-maml may declare fields that don't exist in the parquet file at all (they're silently ignored) or that already have a value in the file's own physical type different from the maml — validation only requires that field names not repeat, that a `qc: miss:` value (if present) is `Null`/`NA` (case-insensitive) or empty, and that any `qc: min:`/`max:` operator points the right way (`min:` a lower bound with `>=`/`>`, `max:` an upper bound with `<=`/`<` — the same rule the [write side](supported-data-types.html#quality-control-qc-range-checks-on-write) enforces; a reversed operator aborts `parquet_open_reader`).
- `qc: miss: Null` (or `NA`) means Nulls are *expected* for that field: no violation is raised if the column contains one. Leaving `miss:` empty or omitting it (the default) means Nulls are *not* expected: reading a Null in that column is a violation — regardless of whether you also pass `null_value=`/`is_valid=` to actually read it. A field with no `qc:` block at all (just a bare `name:`) gets no checking whatsoever, the same as a field never mentioned in the maml.
- qc only ever runs for a column this reader actually touches — read (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`, scalar or vector), prefetched (`parquet_prefetch_columns`), or referenced by a [`parquet_filter`](#row-filtering-with-parquet_filter) — and, when a filter is active, only ever sees the already-filtered rows. Boolean columns skip the min/max check entirely (never meaningful there) but still get the Null-presence check.
- In soft mode, each of the two violation categories (Null-presence, range) prints **at most once per column** for the whole lifetime of the reader, even if that column is read multiple times (in hard mode the first violation aborts, so this never comes up); the message notes it's based on incomplete (whatever's been decoded so far) column information.
- Requires linking `arrow_compute` (see [Printing reader statistics](#printing-reader-statistics-with-parquet_close_reader-print_stattrue) above) — same as `print_stat`, and for the same reason (min/max calculation).

### Reading table metadata with `parquet_get_metadata`

`parquet_get_metadata(reader, key, value [, default] [, warn])` reads one table-level metadata entry back out of a file — the read-side counterpart to the writer's `schema%add_metadata` (see [MAML and metadata](../module/parquet.html)). It is generic: the declared type/kind of `value` (a scalar or 1D array of any [supported type](supported-data-types.html)) selects the variant and how the stored text is parsed back, since all metadata is stored as strings in the file. Table metadata is read once, at `parquet_open_reader` time, and cached, so each call only scans that in-memory copy. Any key present in the file works, including the reserved/internal ones the writer emits (`DATE`, `name`, per-column `column.<name>.*`, ...), not just keys added via `add_metadata`.

- A **missing** `key` triggers `error stop`, unless the optional `default` (same type/kind as `value`) is given, in which case `value` is set to it.
- A **present but unconvertible** stored value (e.g. non-numeric text, or an integer too large for the requested kind) always prints a `WARNING`, then falls back to `default` if given, else `error stop`.

### Streaming/chunked reads

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

**Not compatible with `filter`:** a reader opened with [`filter=`](#row-filtering-with-parquet_filter) fails immediately with `error stop` if you call `parquet_read_column_chunk` on it. The filter mask is a single flat mask covering the whole unfiltered file, with no row-group structure of its own — there's no coherent way to say "this filtered subset of row group N". Open a second, unfiltered reader for the chunked pass if you need both.

**Read-time qc still runs, scoped to one row group at a time:** if the reader was opened with [`qc=.true.`](#read-time-quality-control-with-a-qc-maml), each `parquet_read_column_chunk` call runs the usual `qc: min:`/`max:`/`miss:` checks against just that row group's own data, not the whole column. In hard mode (`qc_soft=.false.`, the default), a violation aborts immediately, naming the offending row group (`qc violation for column 'name [row group N]'...`). In soft mode (`qc_soft=.true.`), a violation prints a `WARNING` — still at most once per column for the reader's whole lifetime (the same throttling `parquet_read_column` already uses), so reading many violating row groups in soft mode doesn't spam one warning per chunk.

**Completeness checks:** pass `check_complete=.true.` to `parquet_close_reader` to verify that every column you read via `parquet_read_column_chunk` had *every* one of the file's row groups read by the time you close — catches a loop that forgot a row group, or exited early by mistake:

```fortran
call parquet_close_reader(reader, check_complete=.true.)
```

`check_complete` defaults to `.false.` (no check, so existing code is unaffected). When it's `.true.`, `check_hard` (default `.true.`) picks the failure mode: `error stop` naming the column and its missing row group(s), or (`check_hard=.false.`) a `WARNING` instead. Only columns actually touched via `parquet_read_column_chunk` are tracked — a column read via `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` is never included in this check, even if the reader also chunk-read other columns.

**Threading:** `parquet_read_column_chunk` calls on the *same* `parquet_reader` are bound by the same "one thread at a time" rule as every other call into a shared reader (see [Thread safety](thread-safety.html)) — but since chunked reads are stateless/random-access, splitting the row-group loop itself across threads works cleanly as long as each thread uses its *own* `parquet_reader` instance opened on the same file (independent readers on the same file are always safe to use concurrently), rather than sharing one reader across threads.
- `warn` (optional `logical`, default `.true.`) only governs the missing-key-with-`default` case; pass `warn=.false.` to suppress that warning. It has no effect when the key is present.
