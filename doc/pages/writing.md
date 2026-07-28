---
title: Writing parquet files from your Fortran code
---

Minimal writer example:

```fortran
program write_parquet_example
    use parquet
    use iso_fortran_env, only: int32, real64
    implicit none

    type(parquet_writer) :: writer
    integer(int32) :: id(3)
    real(real64) :: value(3)

    id = [1_int32, 2_int32, 3_int32]
    value = [10.0_real64, 20.0_real64, 30.0_real64]

    call parquet_open_writer(writer, "data.parquet")
    call parquet_write_column(writer, "id", id)
    call parquet_write_column(writer, "value", value)
    call parquet_close_writer(writer)
end program write_parquet_example
```

If you want explicit column definitions and table metadata, first parse a MAML file into a `parquet_schema` and pass that to `parquet_open_writer`.

```fortran
type(parquet_schema) :: schema

call parquet_parse_maml("maml_example.maml", schema)
call parquet_open_writer(writer, "data.parquet", schema)
```

You can also build a `parquet_schema` entirely in memory, without a `.maml` file — see [Building a schema in code](building-schema-in-code.html#building-a-schema-with-init-and-add_field).

A `parquet_schema` bundles the parsed MAML source (`schema%maml`), the column definitions (`schema%cinfo`, of type `parquet_column_info`) and the table-level metadata (`schema%metadata`, of type `parquet_table_metadata`) into one value; `parquet_parse_maml` populates all three. If `schema` is omitted, `parquet_open_writer` does not enforce a fixed schema: each column's type, string length and array size are inferred from the first `parquet_write_column` call that writes it. If `schema` is given, only columns marked `is_set = .true.` (see `set_column_available`/`set_column_unavailable` below, and `schema%is_column_set` to query a column's current state) are written, and calling `parquet_write_column` with a name that is not in the schema fails immediately with `error stop`. `parquet_close_writer` also checks, for a schema-enforced writer, that every `is_set = .true.` column actually received a write — if one didn't, it prints the output filename and the schema's name before failing with `error stop`. A schema built via `schema%init`/`parquet_schema(...)` (no `.maml` file) is named `internal:<table>` for this purpose (e.g. `internal:my_table`), rather than a `.maml` filename.

Passing `schema` writes both parts of the parquet file's VOTable-style header at once: `schema%cinfo` supplies each column's own `unit`/`info`/`ucd` attributes, while `schema%metadata` supplies table-level entries (author, description, `keyarray:`, etc. — see [How table-level keys become metadata entries](maml-format.html#how-table-level-keys-become-metadata-entries)).

## Saving the source MAML alongside the parquet file

Pass `write_maml=.true.` to `parquet_open_writer` to also save a sidecar `.maml` file next to the parquet output — same path, with a trailing `.parquet` replaced by `.maml` (or `.maml` appended if there is none):

```fortran
call parquet_parse_maml("maml_example.maml", schema)
call parquet_open_writer(writer, "data.parquet", schema, write_maml=.true.)
! writes data.parquet and data.maml
```

This requires `schema` to come from `parquet_parse_maml` — it saves the verbatim MAML source that was parsed into the schema, not the parquet file's parquet/VOTable-style header. Two things follow from that:

- Calls to `schema%add_metadata` made *after* `parquet_parse_maml` (to add extra runtime metadata, as in the [combined example](combined-example.html#maml-schema-vector-columns-and-metadata)) each append a new `keyarray:` entry (`key`/`value`/`comment`) to the saved `.maml`, so runtime metadata is reflected in the sidecar too. Entries are always appended, even if a `keyarray:` entry with the same key already exists — the sidecar will then contain both. A `keyarray:` header is added automatically if the source MAML didn't have one, placed before `extra:` if present, else before `fields:`.
- The saved `.maml`'s `fields:` section only lists columns that are enabled (`schema%cinfo%col(:)%is_set`) at the time `parquet_open_writer` is called, i.e. what actually ends up in the `.parquet` file: entries for columns disabled via `set_column_unavailable`, or excluded from a user MAML subset via `parquet_validate_user_maml`, are removed from the sidecar. Matching is by column name against the `schema` passed to `parquet_open_writer`; every other section (table-level metadata, `keyarray:`, etc.) is left untouched. If disabling columns would leave zero fields, pruning is skipped entirely and the full field list is kept instead, since a MAML file with no fields cannot be read back by `parquet_validate_maml`.

Omitting `write_maml`, or passing `write_maml=.false.`, behaves exactly as before (no sidecar file). Passing `write_maml=.true.` without a `schema`, or with a `schema` that wasn't produced by `parquet_parse_maml`, fails immediately with `error stop`.

Notes:

- Every call to `parquet_write_column` writes one full column.
- A `parquet_date`/`parquet_time`/`parquet_timestamp` array writes a `DATE`/`TIME`/`TIMESTAMP`
  column — see [Date, time and timestamp columns](date-time.html); there is no `is_valid=`
  argument for these three, since validity lives in the elements themselves.
- All columns in one file must contain the same number of rows. The first `parquet_write_column` call fixes the row count for the whole file; any later call with a different row count fails immediately with `error stop`, naming the column and both row counts.
- Close the writer with `parquet_close_writer` to flush data and finalize the file.
- By default, `parquet_open_writer` silently truncates an existing file at `filename` (Fortran's usual `OPEN`/replace behavior). Pass `overwrite=.false.` to instead fail immediately with `error stop`, naming the file, if it already exists — useful when accidentally clobbering a previous run's output would otherwise go unnoticed:

  ```fortran
  call parquet_open_writer(writer, "data.parquet", overwrite=.false.)
  ! aborts if data.parquet already exists, instead of silently truncating it
  ```

## Writer options

`parquet_open_writer` also accepts:
```fortran
call parquet_open_writer(writer, "data.parquet", compression="zstd", compression_level=9, chunk_size=100000)
```
- `compression` — one of `"uncompressed"`, `"snappy"`, `"gzip"`, `"zstd"` (the default), `"brotli"`, `"lz4"` (case-insensitive); an unrecognized name fails immediately with `error stop`. **The default is `"zstd"` at level 3** — this goes one step further than the wider ecosystem convention (pyarrow, Spark, ... default to `"snappy"`, on top of Parquet's own unset-by-default `"uncompressed"`): a moderate zstd level gives a meaningfully better compression ratio than snappy for a modest write-time cost and no read-time penalty. Rough guidance if you want something else: `snappy`/`lz4` for the fastest read/write at a modest size reduction; `gzip`/`brotli` for the smallest files at slower speed; `zstd` for the best balance (and the only one here with a meaningfully tunable `compression_level`, roughly 1–22).
- `compression_level` — optional integer tuning the chosen codec's compression level (mainly meaningful for `zstd`/`gzip`/`brotli`); omitted means "use that codec's own default level" — **except** when `compression` itself is also omitted, in which case the default codec's own default level (3) applies (see above). Passing an explicit `compression="zstd"` with no `compression_level` falls through to Arrow's own zstd default (level 1), not this library's level-3 default — pass `compression_level=3` explicitly alongside `compression="zstd"` if you want the same behavior as leaving both arguments off.
- **`float32`/`float64` columns are automatically written with BYTE_STREAM_SPLIT encoding and dictionary encoding disabled — not something you configure.** This applies regardless of the `compression` codec chosen, and improves the achievable compression ratio for floating-point data (dictionary encoding rarely helps floats, since real-valued samples are usually near-unique). Every other column type keeps the writer's normal defaults (dictionary encoding enabled). There is currently no argument to opt individual columns out of this.
- `chunk_size` — **advanced/optional: most callers never need to set this.** It's the maximum number of rows per Parquet row group. If omitted (the default, recommended for normal use), it is auto-sized — from the table's actual in-memory byte size once `parquet_close_writer` runs, for a writer that only ever uses `parquet_write_column`, or from the schema's declared types/`col_size` right away for a writer that uses the [streaming row-group API](#streamingchunked-writes) instead — targeting ~256 MiB per row group, not a flat row count, so both narrow `int32` columns and wide vector columns (large `col_size`) end up sensibly sized without tuning, up to very large files (hundreds of millions of rows). The auto-sized value is clamped between 1,000 and 10,000,000 rows, and further clamped down if needed so no vector column's per-row-group element count can exceed Arrow's own limit (see [Vector-column per-row-group element count limit](supported-data-types.html#vector-column-per-row-group-element-count-limit)). Pass an explicit value only to override this — e.g. to force multiple row groups in a small file (as some of this library's own tests do), or to hand-tune the memory-vs-overhead trade-off for a workload you've measured: larger values reduce per-row-group overhead and can improve compression, at the cost of more per-row-group encoder state (dictionaries, statistics) held in memory while writing (see [Performance and memory](performance.html)). `parquet_get_chunk_size` returns the writer's resolved value (or, for a `parquet_reader`, an existing row group's actual size) at any point after opening.

Quality control (`qc:` range/miss checks run automatically against what's being written, when a schema is given) is its own topic — see [Quality control](quality-control.html#write-side-enforcement).

## Writing a large scalar string column with parquet_string_column

A scalar `string` column can also be written from a `type(parquet_string_column)` (`call
parquet_write_column(writer, "name", names)`) instead of a padded `character(len=...)` array —
no `is_valid` mask needed, since the column already tracks its own Nulls. See [Reading and
writing compact string columns](string-columns.html#reading-and-writing-compact-string-columns)
for the details and a full example; this is purely an alternative to the `character(len=...)`
form above, not a different file format.

## Streaming/chunked writes

`parquet_write_column` needs the whole column as one complete array — fine for most data, but not for a column too large to hold in memory that way (e.g. hundreds of millions of rows of a wide vector column). `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` write such a column incrementally instead, one Parquet row group at a time, so peak memory is bounded by a row group's worth of data rather than the whole column (see [Streaming/chunked reads](reading.html#streamingchunked-reads) for the read-side mirror):

```fortran
type(parquet_writer) :: writer
integer(int32) :: id_data(nrows_total)
integer(int32) :: vec_buf(col_size, rows_per_group)
integer(c_long_long) :: row, n

call parquet_open_writer(writer, "data.parquet", schema)

! Small columns: written whole, as always, but *before* the first row group --
! see "Mixing whole and chunked columns" below.
call parquet_write_column(writer, "id", id_data)

row = 1
do while (row <= nrows_total)
    n = min(int(rows_per_group, kind=c_long_long), nrows_total - row + 1)
    call fill_vec_buf(vec_buf(:, 1:n), row, n)  ! your own code, fills this row range

    call parquet_new_row_group(writer, n)
    call parquet_write_column_chunk(writer, "big_vec", vec_buf(:, 1:n))
    call parquet_finish_row_group(writer)

    row = row + n
end do

call parquet_close_writer(writer)
```

`parquet_new_row_group(writer, nrows)` opens a row group of `nrows` rows; every column already known to the writer must then receive exactly one `parquet_write_column_chunk` call with exactly `nrows` rows (dispatched by type/kind exactly like `parquet_write_column` — scalar `values(:)` or matrix `values(:,:)`) before `parquet_finish_row_group` closes it out. Repeat for as many row groups as needed, then `parquet_close_writer` as usual.

**Picking `rows_per_group`:** `parquet_get_chunk_size(writer, chunk_size)` returns a usable row-group size at any point after `parquet_open_writer` — a schema-based estimate before any data exists, or (once streaming starts) the value actually locked in by the first `parquet_new_row_group` call. Most callers can just use this rather than picking a size by hand; see [Writer options](#writer-options) above for how it's computed and how to override it with an explicit `chunk_size`.

**Mixing whole and chunked columns:** a small column can still be written the ordinary way with `parquet_write_column` — but only *before* the first `parquet_new_row_group` call. The writer slices it internally, one row-group's worth at a time, using each row group's own `nrows`. A column written once via `parquet_write_column` can never also receive `parquet_write_column_chunk` calls, or vice versa, and every column that will ever appear in the file — whole or chunked — must appear in the *first* row group written: Parquet's file-level schema is fixed once that row group is written, so a column introduced later fails immediately with `error stop`.

**Type matching:** unlike `parquet_write_column`, a schema-declared column's `data_type` must match `values`' own kind *exactly* on the chunked path — there's no int32-into-a-float64-column conversion the way `parquet_write_column` supports.

**Schema-less writers:** work the same way, inferring each column's type/`col_size` from its first chunk — but since that means `col_size` isn't known until the writer is already streaming, pass an explicit `chunk_size` to `parquet_open_writer` yourself rather than relying on the (schema-based) auto-estimate, which has nothing to estimate from without a schema.

**Completeness checks:** `parquet_close_writer` fails with `error stop` if a row group was started (`parquet_new_row_group`) but never finished, or if a whole column's own row count doesn't match how many rows the row groups actually covered — the same "don't let a caller silently under/over-write a column" guarantee `parquet_write_column`'s own row-count check already gives you. `parquet_new_row_group` itself also fails with `error stop` if called again while a row group is already open (i.e. without an intervening `parquet_finish_row_group`), rather than silently abandoning the still-open one.

**Threading:** `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` must all be called from a single thread, in row-group order, for a given writer — same rule as `parquet_write_column` (see [Thread safety](thread-safety.html#rules-at-a-glance)). If you want to parallelize the work that *produces* each row group's data, do that in an `!$omp parallel do` (or similar) around the compute step only, then make the `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` calls afterward, serially, on one thread.

## Filtering rows with a mask

`parquet_write_row_mask`/`parquet_write_chunk_row_mask` drop rows entirely from what gets written — a masked-out row leaves no trace at all in the output file (no offset, no validity bit). This is different from writing a Null: a Null still occupies a row (see `is_valid` above), while a masked-out row simply never appears. The two procedures are mutually exclusive on a given writer — pick whichever matches how you're writing:

- **`parquet_write_row_mask(writer, mask)`** — a single, whole-file mask, for a writer using `parquet_write_column` (with or without row groups). Callable at most once per writer — a second call fails with `error stop` rather than silently replacing the first mask.
- **`parquet_write_chunk_row_mask(writer, mask)`** — a mask scoped to one row group at a time, for a writer using **only** `parquet_write_column_chunk` (no whole-column writes at all). Callable at most once per row group — a second call for the same still-open row group fails with `error stop`.

## Whole-column writes

Call `parquet_write_row_mask` right after `parquet_open_writer`, before any `parquet_write_column` call. Every subsequent `parquet_write_column` call's `values` (or the row dimension of `values(:,:)`) must then have length `size(mask)` exactly — the writer applies `mask` itself, so the column ends up with `count(mask)` rows:

```fortran
type(parquet_writer) :: writer
integer(int32) :: id(5) = [1, 2, 3, 4, 5]
logical :: mask(5) = [.true., .false., .true., .false., .true.]

call parquet_open_writer(writer, "data.parquet")
call parquet_write_row_mask(writer, mask)
call parquet_write_column(writer, "id", id)   ! writes rows 1, 3, 5 only -- 3 rows on disk
call parquet_close_writer(writer)
```

A mask that is entirely `.false.` is allowed and produces a genuine, valid zero-row file (Parquet supports a schema-only, zero-row file).

## Row groups

`parquet_write_row_mask` also works with `parquet_new_row_group`/`parquet_write_column_chunk`: call it once, up front, before the first `parquet_new_row_group`. Each `parquet_new_row_group(writer, nrows)` call then automatically claims the *next* `nrows` positions of the stored mask, in file order, as that row group's window — `nrows` keeps its ordinary meaning (the width of the buffer every column's `parquet_write_column_chunk` call for that row group must supply), and the row group's actual row count on disk is `count()` of its window's slice of the mask, which can be anywhere from `0` up to `nrows`:

```fortran
! mask(1:8) = [T, F, T, F, F, T, T, F] -- windows below claim it 3+2+3 = 8 positions total.
logical :: mask(8) = [.true., .false., .true., .false., .false., .true., .true., .false.]
integer(int32) :: g1(3) = [1, 2, 3], g2(2) = [4, 5], g3(3) = [6, 7, 8]

call parquet_open_writer(writer, "data.parquet")
call parquet_write_row_mask(writer, mask)

call parquet_new_row_group(writer, 3_int64)      ! claims mask(1:3) = [T,F,T] -> keeps rows 1 and 3
call parquet_write_column_chunk(writer, "id", g1)
call parquet_finish_row_group(writer)

call parquet_new_row_group(writer, 2_int64)      ! claims mask(4:5) = [F,F] -> a genuine zero-row group
call parquet_write_column_chunk(writer, "id", g2)
call parquet_finish_row_group(writer)

call parquet_new_row_group(writer, 3_int64)      ! claims mask(6:8) = [T,T,F] -> keeps rows 6 and 7
call parquet_write_column_chunk(writer, "id", g3)
call parquet_finish_row_group(writer)

call parquet_close_writer(writer)   ! written file has 4 rows: 1, 3, 6, 7
```

A row group's window can legitimately be entirely `.false.` (as `g2` above) — this simply contributes zero rows, with no gap or corruption in the surrounding data; there is nothing special to opt into. `parquet_close_writer` checks that the mask was **fully** consumed by the writer's row groups (every position claimed by exactly one window) — a leftover, never-claimed tail is an `error stop`, since it almost always means the row groups' total `nrows` fell short of the mask actually built.

If you'd rather mask each row group independently instead of pre-building one whole-file mask, use `parquet_write_chunk_row_mask(writer, mask)` — call it once per row group, after that row group's `parquet_new_row_group` and before its first `parquet_write_column_chunk` call, with a `mask` exactly `nrows` long:

```fortran
call parquet_open_writer(writer, "data.parquet")

call parquet_new_row_group(writer, 3_int64)
call parquet_write_chunk_row_mask(writer, [.true., .false., .true.])
call parquet_write_column_chunk(writer, "id", g1)   ! keeps rows 1 and 3
call parquet_finish_row_group(writer)

call parquet_new_row_group(writer, 2_int64)
call parquet_write_chunk_row_mask(writer, [.false., .false.])   ! a genuine zero-row group
call parquet_write_column_chunk(writer, "id", g2)
call parquet_finish_row_group(writer)

call parquet_close_writer(writer)
```

If `parquet_write_chunk_row_mask` is used for a writer's first row group, it must be used for **every** row group of that writer (all-or-nothing per writer, not per row group) — and it is unavailable entirely on a writer that has any whole-column (`parquet_write_column`) write anywhere in its lifetime, since that combination can only use the shared `parquet_write_row_mask` scheme above instead.

## Interaction with `is_valid` and `protected_cols:`

`is_valid` keeps its own, separate, pre-mask-indexed meaning: for a given call, `is_valid(i)` still refers to the same `values(i)`/`mask(i)` position, and only decides Null-vs-value among rows that survive the mask. A protected column (MAML `extra: protected_cols:`) only rejects a Null among *surviving* rows — a Null at a masked-out row is irrelevant, since it leaves no trace in the output at all.

## Interaction with other types

Every column type/shape honors whichever mask is active, including `parquet_string_column` and `parquet_date`/`parquet_time`/`parquet_timestamp` — a masked-out row's own null state (these three carry their null state internally; see [Date, time and timestamp columns](date-time.html#null-values-are-part-of-the-element-not-a-separate-mask)) is simply irrelevant, exactly as for `is_valid` above.
