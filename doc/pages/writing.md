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

You can also build a `parquet_schema` entirely in memory, without a `.maml` file — see [Building a schema in code](building-schema-in-code.html).

A `parquet_schema` bundles the parsed MAML source (`schema%maml`), the column definitions (`schema%cinfo`) and the table-level metadata (`schema%metadata`) into one value; `parquet_parse_maml` populates all three. If `schema` is omitted, `parquet_open_writer` does not enforce a fixed schema: each column's type, string length and array size are inferred from the first `parquet_write_column` call that writes it. If `schema` is given, only columns marked `is_set = .true.` (see `set_column_available`/`set_column_unavailable` below) are written, and calling `parquet_write_column` with a name that is not in the schema fails immediately with `error stop`. `parquet_close_writer` also checks, for a schema-enforced writer, that every `is_set = .true.` column actually received a write — if one didn't, it prints the output filename and the schema's name before failing with `error stop`. A schema built via `schema%init`/`parquet_schema(...)` (no `.maml` file) is named `internal:<table>` for this purpose (e.g. `internal:my_table`), rather than a `.maml` filename.

Passing `schema` writes both parts of the parquet file's VOTable-style header at once: `schema%cinfo` supplies each column's own `unit`/`info`/`ucd` attributes, while `schema%metadata` supplies table-level entries (author, description, `keyarray:`, etc. — see [The MAML metadata format](maml-format.html)).

### Saving the source MAML alongside the parquet file

Pass `write_maml=.true.` to `parquet_open_writer` to also save a sidecar `.maml` file next to the parquet output — same path, with a trailing `.parquet` replaced by `.maml` (or `.maml` appended if there is none):

```fortran
call parquet_parse_maml("maml_example.maml", schema)
call parquet_open_writer(writer, "data.parquet", schema, write_maml=.true.)
! writes data.parquet and data.maml
```

This requires `schema` to come from `parquet_parse_maml` — it saves the verbatim MAML source that was parsed into the schema, not the parquet file's parquet/VOTable-style header. Two things follow from that:

- Calls to `schema%add_metadata` made *after* `parquet_parse_maml` (to add extra runtime metadata, as in the [combined example](combined-example.html) below) each append a new `keyarray:` entry (`key`/`value`/`comment`) to the saved `.maml`, so runtime metadata is reflected in the sidecar too. Entries are always appended, even if a `keyarray:` entry with the same key already exists — the sidecar will then contain both. A `keyarray:` header is added automatically if the source MAML didn't have one, placed before `extra:` if present, else before `fields:`.
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

### Writing a large scalar string column with parquet_string_column

A scalar `string` column can also be written from a `type(parquet_string_column)` (`call
parquet_write_column(writer, "name", names)`) instead of a padded `character(len=...)` array —
no `is_valid` mask needed, since the column already tracks its own Nulls. See [Reading and
writing compact string columns](string-columns.html#reading-and-writing-compact-string-columns)
for the details and a full example; this is purely an alternative to the `character(len=...)`
form above, not a different file format.

### Streaming/chunked writes

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

**Picking `rows_per_group`:** `parquet_get_chunk_size(writer, chunk_size)` returns a usable row-group size at any point after `parquet_open_writer` — a schema-based estimate before any data exists, or (once streaming starts) the value actually locked in by the first `parquet_new_row_group` call. Most callers can just use this rather than picking a size by hand; see [Compression and row group size](supported-data-types.html#compression-and-row-group-size) for how it's computed and how to override it with an explicit `chunk_size`.

**Mixing whole and chunked columns:** a small column can still be written the ordinary way with `parquet_write_column` — but only *before* the first `parquet_new_row_group` call. The writer slices it internally, one row-group's worth at a time, using each row group's own `nrows`. A column written once via `parquet_write_column` can never also receive `parquet_write_column_chunk` calls, or vice versa, and every column that will ever appear in the file — whole or chunked — must appear in the *first* row group written: Parquet's file-level schema is fixed once that row group is written, so a column introduced later fails immediately with `error stop`.

**Type matching:** unlike `parquet_write_column`, a schema-declared column's `data_type` must match `values`' own kind *exactly* on the chunked path — there's no int32-into-a-float64-column conversion the way `parquet_write_column` supports.

**Schema-less writers:** work the same way, inferring each column's type/`col_size` from its first chunk — but since that means `col_size` isn't known until the writer is already streaming, pass an explicit `chunk_size` to `parquet_open_writer` yourself rather than relying on the (schema-based) auto-estimate, which has nothing to estimate from without a schema.

**Completeness checks:** `parquet_close_writer` fails with `error stop` if a row group was started (`parquet_new_row_group`) but never finished, or if a whole column's own row count doesn't match how many rows the row groups actually covered — the same "don't let a caller silently under/over-write a column" guarantee `parquet_write_column`'s own row-count check already gives you.

**Threading:** `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` must all be called from a single thread, in row-group order, for a given writer — same rule as `parquet_write_column` (see [Thread safety](thread-safety.html)). If you want to parallelize the work that *produces* each row group's data, do that in an `!$omp parallel do` (or similar) around the compute step only, then make the `parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group` calls afterward, serially, on one thread.
