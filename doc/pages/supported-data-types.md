---
title: Supported data types
---

The following intrinsic Fortran kinds (from `iso_fortran_env`) are supported throughout the write, read and metadata APIs, both as scalars/1D arrays and as 2D vector columns:

| Fortran kind         | MAML `data_type` |
|-----------------------|-------------------|
| `integer(int32)`      | `int32`           |
| `integer(int64)`      | `int64`           |
| `real(real32)`        | `float32`         |
| `real(real64)`        | `float64`         |
| `logical`             | `boolean`         |
| `character(len=*)`    | `string`          |

Vector column entries use the shape convention `(col_size, nrows)` for arrays passed to `parquet_write_column` or produced by `parquet_read_column`.

### Date, time and timestamp columns

Three additional element-level types — `parquet_date`, `parquet_time`, `parquet_timestamp` (module
`parquet_temporal`, re-exported from `use parquet`) — read and write Parquet `DATE`/`TIME`/
`TIMESTAMP` columns, one value per element (`type(parquet_timestamp) :: ts(nrows)` for a scalar
column, `ts(col_size, nrows)` for a vector column), through the same `parquet_write_column`/
`parquet_read_column`/chunked/row-mode/element-mode calls as every type above. Unlike every type in
the table above, these carry their own null state (no `is_valid=`/`null_value=` argument, and a
null-containing column reads without the error-on-Null the numeric/string readers apply by
default) and their write-time unit/timezone is declared via a MAML token
(`timestamp[ns,utc]`/`time[ms]`/`date`) rather than inferred. See
[Date, time and timestamp columns](date-time.html) for the full guide — types, null semantics,
units, Unix-time/MJD/JD interop, and reading files from other tools (legacy `INT96`, arbitrary
timezones). `qc:`/`parquet_filter` are not yet supported for these three types.

### Large string columns

A `string` (scalar or vector-of-strings) column's underlying Arrow representation is chosen automatically based on size. Normally it's Arrow's default `utf8` type, which caps a single column's total string byte payload at 2^31-1 bytes (~2 GiB) — but if writing a column would exceed that, this library transparently switches that column to `large_utf8` (64-bit offsets, no such limit) instead. This is fully automatic and requires no action from either the writer or reader side: `parquet_write_column`/`parquet_read_column` and every other read function behave identically either way, including row filtering (`parquet_filter`) and `qc:` range checks. The only place the difference is visible is `parquet_close_reader(print_stat=.true.)`'s `parquet_type` column, which shows `large_string`/`list<large_string>` instead of `string`/`list<string>` for a column that was promoted.

### Reading `string_view` columns from other tools

This library's own writer never produces Arrow's `string_view` representation — it only ever
appears when reading a Parquet file written by another Arrow-based tool whose stored Arrow schema
declared a column as `utf8_view()`. Reading such a column works transparently through
`parquet_read_column`/`parquet_get_string_length` and every other scalar/vector string read
function, exactly like `string`/`large_string` above, including `qc:` range checks and row
filtering (`parquet_filter`). `parquet_close_reader(print_stat=.true.)`'s `parquet_type` column
shows `string_view` for such a column — note that once a row filter is active on the reader, it
shows as `large_string` instead, since filtering internally casts a `string_view` column to
`large_utf8` first (Arrow's own row-filter compute kernel has no `string_view` support to call
directly); this is transparent to every read result, just a cosmetic difference in that one
diagnostic column. The one exception is the compact `parquet_string_column` (see
[String columns](string-columns.html)) read path: its buffer-handoff fast path only understands
`string`/`large_string`'s offset-based layout, so reading a `string_view` column that way aborts
with a clear error — read it through a fixed-width `character` array instead.

### Vector-column width (`col_size`) limit

Unlike a *scalar* column's row count, which this library supports beyond Fortran's default-integer
`huge(1)` (2,147,483,647) for every data type, a single row's own vector width (`col_size`) is
capped at that same number. This is a hard limit of Arrow's `FixedSizeListType` itself — its
`list_size` is a plain `int32_t`, and unlike Arrow's string type (see
[Large string columns](#large-string-columns) above), there is no "large" fixed-size-list variant
to fall back to. Writing a vector column whose `col_size` would exceed this aborts the process (a
C++-level abort with a diagnostic on stderr, the same class of failure as the physical-type-mismatch
case in [Limitations](../index.html#limitations)) rather than silently truncating `col_size` and
corrupting the written column.

### Vector-column per-row-group element count limit

Parquet's own repetition/definition-level generation for list-typed columns walks every flattened
element *of a single row group* with a plain `int32_t` counter, so a row group's own
`row_group_rows * col_size` is capped at 2,147,483,647 — but this is scoped to one row group, not
the whole file. `parquet_close_writer`'s row-group auto-sizing (see `chunk_size` in
[Writing parquet files](writing.html)) already knows each column's `col_size` and silently picks a
smaller row-group size whenever a wide vector column needs it, so a column's *total*
`nrows * col_size` can exceed 2,147,483,647 — a real, hittable case (e.g. 2.5 billion rows at
`col_size=2`) — without any special handling: it is transparently split across multiple row groups
and round-trips normally. The only case that still aborts is an *explicitly* chosen `chunk_size`
(`parquet_open_writer`/`parquet_set_writer_options`) that conflicts with a vector column's
`col_size` — silently shrinking a caller's explicit request would be a surprising, hard-to-notice
performance change, so this aborts the process instead (a C++-level abort with a diagnostic on
stderr, the same class of failure as the `col_size` case above) rather than either silently
overriding the request or letting Arrow itself throw an uncaught `IOError: List index overflow`
mid-write.

### Reading a column into a different numeric kind

`parquet_read_column` (and `parquet_read_array_row_mode`/`parquet_read_array_element_mode`) dispatch on the *declared type/kind of the `values` array you pass in*, not the column's own stored type — so `values`' kind doesn't have to match the file's `data_type` exactly, as long as the conversion is one of the following:

| Requested `values` kind | Convertible **from** stored `data_type` |
|--------------------------|-------------------------------------------|
| `integer(int32)`         | `int32`, `int64` (checked for overflow — see below); `int8`, `int16`, `uint8`, `uint16` (always exact); `uint32`, `uint64` (checked for overflow); `float32`, `float64`, `half_float`, and `decimal` (checked for a fractional part and for overflow — see below) |
| `integer(int64)`         | `int64`, `int32`, `int8`, `int16`, `uint8`, `uint16`, `uint32` (always exact); `uint64` (checked for overflow); `float32`, `float64`, `half_float`, and `decimal` (checked for a fractional part and for overflow — see below) |
| `real(real32)`           | `float32`, `float64`, `int32`, `int64`, `int8`, `int16`, `uint8`, `uint16`, `uint32`, `uint64`, `half_float`, `decimal` |
| `real(real64)`           | `float64`, `float32`, `int32`, `int64`, `int8`, `int16`, `uint8`, `uint16`, `uint32`, `uint64`, `half_float`, `decimal` |
| `logical`                | `boolean` only |
| `character(len=*)`       | `string` only |

`int8`/`int16`/`uint8`/`uint16`/`uint32`/`uint64`/`half_float`/`decimal` here are physical Parquet/Arrow storage types this library's own writer never produces (writing stays limited to the six types in the table at the top of this page) — they only ever arise from a file written by some other tool. There is nothing to declare for them in a MAML schema or anywhere else: the conversion is purely internal to the read path, triggered automatically by whatever physical type the column already has on disk.

Anything not listed for a given `values` kind (e.g. requesting `logical`/`string` from anything else) fails immediately with `error stop`, naming the column and the type mismatch.

Conversions that lose information silently, with no warning:

- `int64` → `integer(int32)`, and `uint32`/`uint64` → `integer(int32)`/`integer(int64)` where the value doesn't fit, are checked for overflow: a stored value outside the target's range fails with `error stop` (e.g. `"...int64->int32 overflow..."`, `"...uint64->int64 overflow..."`) rather than wrapping.
- `int32`/`int64`/`int8`/`int16`/`uint8`/`uint16`/`uint32`/`uint64` → `real32`/`real64`, `float64` → `real32`, and `half_float`/`decimal` → `real32`/`real64`, are **not** checked for precision loss — a large `int64` (beyond ~2^53) or `int32` (beyond ~2^24) read into `real32` silently loses exact-integer precision, the same way a plain Fortran `real(int_value, kind=real32)` conversion would.

A `float32`/`float64`/`half_float`/`decimal` value read into an `integer(int32)`/`integer(int64)` array is **never** silently truncated, unlike the narrowing above: a value with a nonzero fractional part fails immediately with `error stop` (e.g. `"...double value has a fractional part, cannot convert to int32 for column: ..."`), and an otherwise-integral value that doesn't fit the target width fails with the same kind of overflow `error stop` as the integer conversions above (e.g. `"...double->int64 overflow for column: ..."`). This exists specifically to support reading a column that is intrinsically an integer quantity but happens to have been written as a floating-point or decimal type by whatever tool produced the file — not to allow lossy float-to-int rounding.

`qc: min:`/`max:` range checks and row filtering (`parquet_filter`) cover every type in the table above the same way they already cover `int32`/`int64`/`float32`/`float64` — both compare against the column's raw physical value regardless of its stored type, so no MAML/filter-side change is needed to use them against one of these extended types.

### Null values

> `date`/`time`/`timestamp` columns are the exception to this whole section: `parquet_date`/
> `parquet_time`/`parquet_timestamp` carry their own null state per element, so there is no
> `null_value=`/`is_valid=` argument for them anywhere, and a null-containing column reads
> without the error-on-Null described below — see
> [Null values are part of the element](date-time.html#null-values-are-part-of-the-element-not-a-separate-mask).

Fortran has no per-element representation for a missing/Null value. On the **read** side, if a column contains any genuine Parquet Null (e.g. a file produced by another tool), the default behavior of `parquet_read_column`, `parquet_read_array_row_mode`, and `parquet_read_array_element_mode` is to abort the process immediately, rather than silently returning undefined data. This is a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_int32_column: column contains Null value(s), which is not supported: <column>`), not a Fortran `error stop` — the same class of failure as the physical-type-mismatch case in [Limitations](../index.html#limitations).

To read a Null-containing column instead of erroring, pass one or both of these optional keyword arguments (supported by all three of the read families above, for every data type):

- `null_value=nullval` — a scalar of the same type as `values`; every Null in the column is replaced with `nullval` in the returned `values`.
- `is_valid=mask` — a `logical` array of the same shape as `values`; `.false.` wherever the Parquet value was Null, `.true.` otherwise.

If only `is_valid` is given (no `null_value`), Null slots in `values` are still filled with a safe type-appropriate default (`0` / `.false.` / blank string) rather than left as undefined data — check `is_valid` to know which entries are real. If both are given, Nulls are replaced with `nullval` *and* `is_valid` still reports which entries were originally Null. Omitting both keeps the default strict (error-on-Null) behavior.

For vector columns, a slot is reported/treated as Null if either the whole row is missing or that specific element within the row is missing (Parquet's list columns track these independently); `is_valid(j, i)` reflects the combination of both.

`parquet_get_string_length` is unaffected by any of this: it always silently skips Nulls when computing the maximum string length, since sizing an output buffer shouldn't depend on how you plan to handle Nulls when reading.

On the **write** side, `parquet_write_column(writer, name, values, is_valid=mask)` accepts the same kind of `logical` mask (same shape as `values`, `.false.` = write a genuine Null there); there is no `null_value` on the write side, since a value used to *detect* a Null (rather than substitute one, as on read) would risk misclassifying a legitimate value that happens to equal the sentinel. Whatever is in `values` at a `.false.` slot is ignored — a real Parquet Null is written there regardless. For vector columns, `is_valid` is element-level only: an entire row's vector can never be Null, only individual elements within it. A column only becomes nullable in the file's schema if `is_valid` is actually passed and contains at least one `.false.` entry; omitting `is_valid` (or passing an all-`.true.` mask) writes exactly as before, keeping the column non-nullable.

To forbid Nulls in specific columns even when a caller passes `is_valid`, list them under a MAML schema's `extra:` section as `protected_cols:`, either as a semicolon-separated scalar or a dash-list:
```
extra:
  protected_cols: col1;col2;col3
```
or equivalently:
```
extra:
  protected_cols:
  - col1
  - col2
  - col3
```
Every name listed must be one of this same MAML file's own declared `fields:` (checked by `parquet_validate_maml`; unknown names fail validation). If a user MAML overrides a base MAML, `protected_cols:` is taken from whichever MAML is actually used to build the writer's schema (the user MAML if one is provided, otherwise the base MAML) — not merged across both. Writing an `is_valid` mask with any `.false.` entry for a protected column fails immediately with `error stop`. This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., schema, ...)`); a schema-less writer has no `protected_cols:` to enforce.

### Reading a nested struct field

A Parquet `STRUCT` column's individual fields — at any nesting depth — can be read directly by
passing a dot-separated path as the `name` argument to `parquet_read_column`,
`parquet_read_array_row_mode`/`parquet_read_array_element_mode`, `parquet_read_column_chunk`,
`parquet_get_col_size`/`parquet_get_column_total_elements`, `qc:` bounds in a MAML schema, and
`parquet_filter` rules — every one of these dispatches on `name` the same way, so a struct-nested
column is used identically to a top-level one everywhere. Given a file with

```
main : STRUCT
├── id    : int32
└── inner : STRUCT
    ├── name : string
    └── age  : int32
```

`parquet_read_column(reader, "main.id", ...)` and `parquet_read_column(reader, "main.inner.age",
...)` read those two leaves directly; nesting depth is unlimited (`"main.inner.deeper.value"` works
the same way). An exact top-level field name always wins over path-splitting first, so an existing
column literally named with a `.` in it is unaffected. A dotted path may also resolve to a vector
(`FIXED_SIZE_LIST`) leaf nested inside a struct — reading it behaves exactly like any other vector
column (`col_size`, row/element modes, `(col_size, nrows)` shape, all apply unchanged).

A path must resolve all the way down to a leaf column: naming an intermediate struct directly
(`"main.inner"`, or just `"main"` when `main` is itself a struct) is not readable by any type this
library supports and fails the same way as any other unknown column (`error stop
"...: column not found in parquet file: ..."`) — there is no struct/record output type to read it
into. `MAP` columns, and variable-length `LIST` columns, are not supported anywhere along a struct
path — neither as an intermediate hop nor as the terminal leaf — and are rejected with the same
"column not found" class of error rather than a silent wrong answer or a crash.

**Null handling** combines every level a path passes through: a leaf is reported/treated as Null
(via `null_value=`/`is_valid=`, [above](#null-values)) if the top-level struct itself is missing
for that row, *or* any intermediate struct field is missing, *or* the leaf itself is missing —
generalizing the same "missing if either the row or the specific slot is missing" principle vector
columns already use, from one level of list-nesting to arbitrary levels of struct-nesting.

**`qc:` and `parquet_filter` both work against a dotted path** exactly as they do against a
top-level column — a MAML field's `name:` can be a dotted path, and a `parquet_filter%add` rule's
column can be one too.

**`parquet_close_reader(..., print_stat=.true.)`** ([above](reading.html#printing-reader-statistics-with-parquet_close_reader-print_stattrue))
shows one row per top-level *physical* struct column touched by any of its leaves being read, not
one row per leaf — its `nulls`/`min`/`max` reflect the whole struct's own top-level figures (which
degrade to blank/`-` for a struct, since those statistics aren't well-defined for a nested type),
and `output_type` shows whichever leaf under that struct was most recently read. This is a known,
accepted limitation, not a bug: reading two different leaves under one struct is still reported as
a single touched column.

This library's own writer cannot produce `STRUCT` columns — like the extended read-only source
types in [Reading a column into a different numeric kind](#reading-a-column-into-a-different-numeric-kind)
above, struct support is read-only, for files produced by some other tool.

### Quality control (qc:) range checks on write

A MAML field can declare a `qc:` block with `min:`/`max:` bounds:
```
- name: ra
  data_type: float64
  qc:
    min: '>= 0'
    max: '< 360'
```
A plain number (`min: 1`) is treated as inclusive (`>=` for `min:`, `<=` for `max:`); a quoted value with an explicit leading operator uses that comparison instead. The operator must match the bound's direction: `min:` accepts only `>=` or `>` (a lower bound) and `max:` accepts only `<=` or `<` (an upper bound); a reversed operator (e.g. `min: '< 5'`) is a nonsensical bound and fails `parquet_validate_maml`. Either bound may be omitted (only `min:` or only `max:` is fine). `parquet_validate_maml` also checks that every declared bound actually converts to a value usable for that field's `data_type`: for `int32`/`int64` it must be an exact integer within that type's range; for `float32`/`float64` it must be finite (not `NaN`/`Infinity`); a `string` field's bound is used as a literal string (nothing to convert, so nothing can fail there); `qc:` on a `boolean` field is accepted but never enforced (silently ignored, and so exempt from the operator-direction check too).

Pass `qc=.true.` to `parquet_open_writer` to turn on the actual range check during writing:
```fortran
call parquet_open_writer(writer, "data.parquet", schema, qc=.true.)
```
With `qc=.true.`, every `parquet_write_column` call checks its column's declared bound(s) (if any) against every element for which `is_valid` is `.true.` (or every element, if `is_valid` wasn't passed at all — see [Null values](#null-values)). String columns are compared lexicographically using Fortran's native string comparison. Vector columns are checked element-wise. A violation **never stops the write** — it prints one `WARNING` line to stdout naming the column, its declared bound(s), the observed data range among the checked elements, and how many of them are out of range, e.g.:
```
WARNING: qc violation for column 'ra': declared min >= 0, max < 360, data range [-1.5, 359.9], 3 of 1000 valid element(s) out of range
```
This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., schema, ...)`) and only for columns that actually declare `qc: min:`/`max:`; omitting `qc=.true.` (the default) skips the check entirely, same as before this feature existed.

### Compression and row group size

`parquet_open_writer` also accepts:
```fortran
call parquet_open_writer(writer, "data.parquet", compression="zstd", compression_level=9, chunk_size=100000)
```
- `compression` — one of `"uncompressed"`, `"snappy"` (the default), `"gzip"`, `"zstd"`, `"brotli"`, `"lz4"` (case-insensitive); an unrecognized name fails immediately with `error stop`. The default is `"snappy"`, following the ecosystem convention (pyarrow, Spark, ...) rather than Parquet's own unset-by-default `"uncompressed"`. Rough guidance: `snappy`/`lz4` for fastest read/write at a modest size reduction; `gzip`/`brotli` for the smallest files at slower speed; `zstd` for the best balance (and the only one here with a meaningfully tunable `compression_level`, roughly 1–22).
- `compression_level` — optional integer tuning the chosen codec's compression level (mainly meaningful for `zstd`/`gzip`/`brotli`); omitted means "use that codec's own default level".
- `chunk_size` — **advanced/optional: most callers never need to set this.** It's the maximum number of rows per Parquet row group. If omitted (the default, recommended for normal use), it is auto-sized — from the table's actual in-memory byte size once `parquet_close_writer` runs, for a writer that only ever uses `parquet_write_column`, or from the schema's declared types/`col_size` right away for a writer that uses the [streaming row-group API](writing.html#streamingchunked-writes) instead — targeting ~256 MiB per row group, not a flat row count, so both narrow `int32` columns and wide vector columns (large `col_size`) end up sensibly sized without tuning, up to very large files (hundreds of millions of rows). The auto-sized value is clamped between 1,000 and 10,000,000 rows, and further clamped down if needed so no vector column's per-row-group element count can exceed Arrow's own limit (see [Vector-column per-row-group element count limit](#vector-column-per-row-group-element-count-limit)). Pass an explicit value only to override this — e.g. to force multiple row groups in a small file (as some of this library's own tests do), or to hand-tune the memory-vs-overhead trade-off for a workload you've measured: larger values reduce per-row-group overhead and can improve compression, at the cost of more per-row-group encoder state (dictionaries, statistics) held in memory while writing (see [Performance and memory](performance.html)). `parquet_get_chunk_size` returns the writer's resolved value (or, for a `parquet_reader`, an existing row group's actual size) at any point after opening.

### Multi-threaded decoding/encoding (`use_threads`) and thread pool size

Both `parquet_open_reader` and `parquet_open_writer` accept an optional `use_threads` (`logical`, default `.true.`):
```fortran
call parquet_open_reader(reader, "data.parquet", use_threads=.true.)
call parquet_open_writer(writer, "data.parquet", use_threads=.true.)
```
When `.true.` (the default), that reader/writer decodes or encodes column data across Arrow's internal CPU thread pool instead of a single thread — Arrow's own library default is actually `.false.`, so this library turns it on by default since the extra parallelism is normally a pure win. This is on a per-reader/per-writer basis: it costs nothing to leave it on, and there's no shared state to worry about between independent readers/writers.

The most common reason to pass `use_threads=.false.` is to avoid **oversubscription** when you're already parallelizing at a coarser level — e.g. many OpenMP threads (see [Thread safety](thread-safety.html)) each opening their own reader/writer: without this, every one of those threads would *also* fan out across Arrow's thread pool, so N OpenMP threads times Arrow's pool size threads end up competing for the same cores. It's also useful for deterministic single-threaded benchmarking/profiling.

`parquet_set_max_threads(n)` caps the size of Arrow's thread pool itself:
```fortran
call parquet_set_max_threads(4)
```
Unlike `use_threads`, this is **not** a per-reader/per-writer setting — Arrow's CPU thread pool is a single, process-global resource shared by every reader/writer (in every thread) that has `use_threads` enabled. Call it once, e.g. near the start of your program, before opening readers/writers on other threads; calling it repeatedly with different values from multiple concurrent threads is a race, since each call resizes a pool everyone else is using at that same moment. `n` must be `>= 1`; values below that fail immediately with `error stop`.

(If you're developing `parquet-fortran` itself and want to measure how these two knobs actually affect write/read throughput on your own hardware, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#other-tools-helpers)'s `tools/benchmark_threads.sh` entry.)
