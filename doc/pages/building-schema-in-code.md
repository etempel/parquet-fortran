---
title: Building a schema in code with schema%init and schema%add_field
---

A `parquet_schema` normally comes from a MAML file (via `parquet_parse_maml`), but it can also be built entirely in memory, with no `.maml` file, using two type-bound procedures on `parquet_schema`. This is the schema-authoring counterpart to [building a qc-maml in code](reading.html#building-a-qc-maml-in-code-with-add_col_qc-and-set_col_qc), and produces a full write schema (columns + table metadata), not just qc bounds.

- **`call schema%init(table [, survey, dataset, version, date, author, description, license, maml_version])`** — starts a fresh schema and sets its top-level metadata. Only `table` is required (and must be non-empty); every other argument is an optional scalar top-level MAML key. Must be called before `add_field`, and only once per schema — a second `init`, or an empty `table`, fails with `error stop`. Since there's no source `.maml` file, `schema%maml%name` is set to `internal:<table>` instead, so diagnostics that name the schema's source (e.g. `parquet_close_writer`'s missing-write error) still have something useful to print.
- **`schema = parquet_schema(table [, survey, dataset, version, date, author, description, license, maml_version])`** — the structure-constructor form of `schema%init`: same arguments, same validation, same `error stop` conditions, but builds and returns an initialized schema in one expression instead of declaring the variable and calling `%init` separately.
- **`call schema%add_field(name, data_type [, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss])`** — appends one `fields:` entry. `name` and `data_type` are required (`data_type` one of the [supported types](supported-data-types.html), including `date`/`time[unit]`/`timestamp[unit,utc]` — see [Date, time and timestamp columns](date-time.html#units-and-schema-declared-columns)); the rest are optional and mirror the MAML `fields:` attributes, with `qc_min`/`qc_max`/`qc_miss` forming an optional `qc:` block (same operator/`miss:` rules as [`add_col_qc`](reading.html#building-a-qc-maml-in-code-with-add_col_qc-and-set_col_qc); not supported at all for a temporal `data_type`, rejected at validation). Everything is validated immediately (empty/duplicate `name`, invalid `data_type`, reversed qc operator, bad `qc_miss` value, ...), each failing with `error stop`; calling it before `schema%init` also fails.

Both build the schema's underlying MAML text, so you still call `parquet_parse_maml(schema)` afterwards to populate `schema%cinfo`/`schema%metadata` before writing, exactly as for a schema loaded from disk. List-shaped top-level sections (`coauthors:`, `comments:`, `keyarray:`, `extra:`, ...) are out of scope for `init`: use `add_metadata` for `keyarray:`-style entries, or author a `.maml` file for the rest.

## Printing column info with schema%print_schema_info

**`call schema%print_schema_info([unit, filename, prefix, header, table_name, dash_before_header, dash_after_header, dash_after_fields, dash_char])`** — writes a fixed-width listing of this schema's *enabled* (`is_set`) columns, one per line, in the order `name unit type len ucd info` (`type`/`len` are the header labels for `data_type`/`col_size`; `info` is left unpadded so no line carries trailing whitespace). Column widths are computed from the longest value actually present (and the header label, if printed), so each call produces its own self-contained, internally-aligned block — two calls for different schemas are not aligned with each other. Works for any parsed schema (`schema%cinfo` populated), whether built in code or loaded from a `.maml` file.

Exactly one of `unit`/`filename` must identify the destination:

- **`unit`** — an already-open unit. This is the primary way to print several schemas into one combined listing: open the unit once yourself and call `%print_schema_info(unit=...)` repeatedly, once per schema; each call appends its own block.
- **`filename`** — opens the file with `position="append"`, writes, and closes again before returning — a convenience for a one-off call, or for accumulating across separate calls without managing a unit yourself.

Giving neither, or an already-given `unit` that isn't open, or open for reading only, or a `unit`+`filename` pair where `filename` doesn't match (exact, trimmed string equality against `inquire(unit=unit, name=)`) the file the unit is actually connected to — all `error stop`.

Optional formatting arguments:

- `prefix` — prepended to every emitted line (e.g. `prefix="# "` for a shell/FITS-style comment block); default none.
- `header` — print a `name unit type len ucd info` header row; default `.true.`.
- `table_name` — print a `Table name: <table>` line, using this schema's required MAML `table:` key, positioned after `dash_before_header` and before the header row; default `.true.`.
- `dash_before_header` / `dash_after_header` / `dash_after_fields` — dashed separator lines at each position (independent of each other and of `header`/`table_name`); `dash_after_header` defaults `.true.`, the other two default `.false.`.
- `dash_char` — character used to draw dashed lines; default `"-"`.

Example, for a schema with `table: input_table` and columns `id`/`ra`/`dec` enabled:

```
Table name: input_table
name unit type    len ucd       info
-------------------------------------------
id        int32   1   meta.id   ID field.
ra   deg  float64 1   pos.eq.ra Right ascension
dec  deg  float64 1   pos.eq.dec Declination
```
