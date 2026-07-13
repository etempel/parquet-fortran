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
- All columns in one file must contain the same number of rows. The first `parquet_write_column` call fixes the row count for the whole file; any later call with a different row count fails immediately with `error stop`, naming the column and both row counts.
- Close the writer with `parquet_close_writer` to flush data and finalize the file.
