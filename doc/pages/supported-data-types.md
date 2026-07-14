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

### Large string columns

A `string` (scalar or vector-of-strings) column's underlying Arrow representation is chosen automatically based on size. Normally it's Arrow's default `utf8` type, which caps a single column's total string byte payload at 2^31-1 bytes (~2 GiB) — but if writing a column would exceed that, this library transparently switches that column to `large_utf8` (64-bit offsets, no such limit) instead. This is fully automatic and requires no action from either the writer or reader side: `parquet_write_column`/`parquet_read_column` and every other read function behave identically either way, including row filtering (`parquet_filter`) and `qc:` range checks. The only place the difference is visible is `parquet_close_reader(print_stat=.true.)`'s `parquet_type` column, which shows `large_string`/`list<large_string>` instead of `string`/`list<string>` for a column that was promoted.

### Reading a column into a different numeric kind

`parquet_read_column` (and `parquet_read_array_row_mode`/`parquet_read_array_element_mode`) dispatch on the *declared type/kind of the `values` array you pass in*, not the column's own stored type — so `values`' kind doesn't have to match the file's `data_type` exactly, as long as the conversion is one of the following:

| Requested `values` kind | Convertible **from** stored `data_type` |
|--------------------------|-------------------------------------------|
| `integer(int32)`         | `int32`, `int64` (checked for overflow — see below) |
| `integer(int64)`         | `int64`, `int32` |
| `real(real32)`           | `float32`, `float64`, `int32`, `int64` |
| `real(real64)`           | `float64`, `float32`, `int32`, `int64` |
| `logical`                | `boolean` only |
| `character(len=*)`       | `string` only |

Anything not listed for a given `values` kind (e.g. requesting `integer` from a `float32`/`float64` column, or `logical`/`string` from anything else) fails immediately with `error stop`, naming the column and the type mismatch — there is no float-to-integer or integer/float-to-boolean/string conversion in either direction.

Two conversions above lose information silently, with no warning:

- `int64` → `integer(int32)` is the one integer narrowing checked for overflow: a stored value outside `int32`'s range fails with `error stop` (`"...int64->int32 overflow..."`) rather than wrapping.
- `int32`/`int64` → `real32`/`real64`, and `float64` → `real32`, are **not** checked for precision loss — a large `int64` (beyond ~2^53) or `int32` (beyond ~2^24) read into `real32` silently loses exact-integer precision, the same way a plain Fortran `real(int_value, kind=real32)` conversion would.

### Null values

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
- `chunk_size` — **advanced/optional: most callers never need to set this.** It's the maximum number of rows per Parquet row group. If omitted (the default, recommended for normal use), it is auto-sized once `parquet_close_writer` runs, targeting ~256 MiB per row group based on the table's actual in-memory byte size — not a flat row count — so both narrow `int32` columns and wide vector columns (large `col_size`) end up sensibly sized without tuning, up to very large files (hundreds of millions of rows). The auto-sized value is clamped between 1,000 and 10,000,000 rows. Pass an explicit value only to override this — e.g. to force multiple row groups in a small file (as some of this library's own tests do), or to hand-tune the memory-vs-overhead trade-off for a workload you've measured: larger values reduce per-row-group overhead and can improve compression, at the cost of more per-row-group encoder state (dictionaries, statistics) held in memory while writing (see [Performance and memory](performance.html)).

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
