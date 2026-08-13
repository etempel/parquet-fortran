---
title: Building a schema in code with schema%init and schema%add_field
---

## Building a schema with init and add_field

A `parquet_schema` normally comes from a MAML file (via `parquet_parse_maml`), but it can also be built entirely in memory, with no `.maml` file, using two type-bound procedures on `parquet_schema`. This is the schema-authoring counterpart to [building a qc-maml in code](quality-control.html#building-a-qc-maml-in-code), and produces a full write schema (columns + table metadata), not just qc bounds.

- **`call schema%init(table [, survey, dataset, version, date, author, description, license, maml_version, force])`** — starts a fresh schema and sets its top-level metadata. Only `table` is required (and must be non-empty); every other argument is an optional scalar top-level MAML key. Must be called before `add_field`, and only on a schema that isn't already initialized — `schema%is_init() == .true.` (whether from an earlier `%init` call *or* a MAML parse — e.g. calling `%init` on a schema already loaded via `parquet_parse_maml`), or an empty `table`, fails with `error stop`, unless `force=.true.` is given (default `.false.`): this bypasses the "already initialized" check and fully resets the schema (discarding any fields/qc/metadata already added via `add_field`/`add_col_qc`/`add_metadata`, and `schema%cinfo`/`schema%metadata` regardless of whether they came from `%add_field` or a MAML parse) before rebuilding it from scratch with the new arguments — the schema ends up exactly as if this were its first `%init` call. `force=.true.` on a never-initialized schema is harmless (nothing to reset). Since there's no source `.maml` file, `schema%maml%name` is set to `internal:<table>` instead, so diagnostics that name the schema's source (e.g. `parquet_close_writer`'s missing-write error) still have something useful to print.
- **`schema%is_init()`** — returns `.true.` once this schema is ready to use: either `schema%init`/`parquet_schema(...)` has completed (the in-code builder path), or `parquet_parse_maml` has populated `schema%cinfo` (a schema loaded from a `.maml` file/object, which never calls `%init` at all). `.false.` only for a freshly declared `parquet_schema` that has had neither happen yet. Note `%add_field`'s own "call schema%init(...) before adding fields" check is narrower — it specifically requires `%init` to have been called, since `%add_field` only makes sense on a from-scratch schema, not one already populated via a MAML parse.
- **`schema%is_parsed()`** — returns `.true.` once `schema%cinfo` is actually populated, i.e. `parquet_parse_maml` has run. This is a narrower, stricter check than `%is_init()`: a from-scratch schema that has only had `%init`/`%add_field` called (never parsed) reports `%is_init() == .true.` but `%is_parsed() == .false.`. Use `%is_parsed()` — not `%is_init()` — to check readiness before calling anything that requires a populated `%cinfo`, such as `%get_field`, `%add_field_from`, or `%print_schema_info` (see [Printing column info with schema%print_schema_info](#printing-column-info-with-schemaprint_schema_info) below).
- **`call schema%clear()`** — resets the entire schema back to exactly the state a freshly declared, never-initialized `parquet_schema` starts in (`%maml`/`%cinfo`/`%metadata` all back to their defaults, `%is_init()` becomes `.false.` again). Unlike `%init(..., force=.true.)` (which resets and immediately rebuilds with new header-key arguments), `%clear` leaves the schema uninitialized — call `%init` again afterward to reuse the variable. Always succeeds, even on an already-blank schema (a no-op in that case); in fact `%init(..., force=.true.)` is implemented as `%clear` followed by a normal `%init`.
- **`schema = parquet_schema(table [, survey, dataset, version, date, author, description, license, maml_version])`** — the structure-constructor form of `schema%init`: same arguments, same validation, same `error stop` conditions, but builds and returns an initialized schema in one expression instead of declaring the variable and calling `%init` separately.
- **`call schema%add_field(name, data_type [, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss])`** — appends one `fields:` entry. `name` and `data_type` are required (`data_type` one of the [supported types](../types/supported-data-types.html), including `date`/`time[unit]`/`timestamp[unit,utc]` — see [Date, time and timestamp columns](../types/date-time.html#units-and-schema-declared-columns)); the rest are optional and mirror the MAML `fields:` attributes, with `qc_min`/`qc_max`/`qc_miss` forming an optional `qc:` block (same operator/`miss:` rules as [`add_col_qc`](quality-control.html#building-a-qc-maml-in-code); for a temporal `data_type` the `qc_min`/`qc_max` bounds are rejected at validation, while `qc_miss` is accepted as on any other type). Everything is validated immediately (empty/duplicate `name`, invalid `data_type`, reversed qc operator, bad `qc_miss` value, ...), each failing with `error stop`; calling it before `schema%init` also fails. `qc_miss` declares whether Nulls are an expected part of this column's output: `Null`/`NA` (case-insensitive) means Nulls are expected (no warning if found); absent or empty (the default) means Nulls are NOT expected, and `parquet_write_column`/`parquet_open_reader` print a `WARNING` (never an error) if any turn up — see [Quality control](quality-control.html) for the full min/max/miss enforcement picture on both the read and write sides — a schema built with `%add_field`'s own `qc_min`/`qc_max`/`qc_miss` works directly as `parquet_open_reader(..., schema=)`, without needing a separate `add_col_qc` call. `qc_miss` may be set on any `data_type`, including one that cannot itself carry a Null today (harmless now; kept for forward compatibility with a future null-aware column type).
- **`call schema%set_protected(name [, protected])`** — marks a column Null-protected, the code-level equivalent of listing it under a MAML's `extra: protected_cols:`; `protected` defaults to `.true.`, and `.false.` lifts protection. A protected column may hold no Null at all: `parquet_write_column` fails with `error stop` on an `is_valid` mask with any `.false.` entry, on a null `date`/`time`/`timestamp` element, and on an `%append_null()` in a `parquet_string_column` — and the column's Arrow field is written non-nullable, which is also the only way to declare a *streamed* temporal or `parquet_string_column` column null-free (see [Null values](../types/supported-data-types.html#null-values)). Call it before `parquet_open_writer`, since the writer takes its own copy of the schema at open time. Unprotecting a column the MAML itself declared protected is allowed but prints a `WARNING` naming the column — never an abort — since it overrides a declaration someone wrote down deliberately. `error stop`s if `name` isn't a declared field.
- **`call schema%get_field(name [, data_type, unit, info, ucd, array_size, col_size, qc_min, qc_max, qc_miss])`** / **`call schema%get_field(index, name [, ...])`** — reads back an already-parsed field's full definition, the same shape of values `%add_field` accepts; every output beyond the lookup key is optional, so a caller can request only what it needs. The by-`index` form (1-based MAML source order, same order `get_num_fields`/`get_field_name` count) additionally returns the field's own `name`, since the caller doesn't already know it. `qc_min`/`qc_max` come back as a single operator-prefixed string (e.g. `">= 0"`), re-feedable straight into `%add_field`'s own `qc_min`/`qc_max` arguments — always with an explicit operator, even if the original call left it implicit, which is semantically identical but not necessarily byte-identical to the original input. `qc_miss` comes back as `"Null"` or `""` (both `Null` and `NA` collapse to `"Null"` — the alias distinction isn't preserved in storage). `error stop`s if the field isn't found (by name) or the index is out of range; requires `schema%cinfo` to already be populated (`parquet_parse_maml` — or, for a from-scratch schema, `%init`/`%add_field` followed by `parquet_parse_maml` — must already have run; a field added since the last parse isn't visible yet).
- **`call schema%add_field_from(source_schema, name)`** — copies `name`'s full field definition from `source_schema` (via `%get_field`) and appends an equivalent field here via `%add_field`, so two schemas can share a column definition (e.g. a handful of "identity" columns — `obj_id`, `ra`, `dec`, ... — common to several output tables) without re-typing its type/unit/info/qc by hand and risking drift between the copies. `source_schema` must already be parsed (same precondition as `%get_field`); `this` must already have `%init` called, exactly like a direct `%add_field` call requires. Subject to the same qc_min/qc_max/qc_miss round-trip caveats as `%get_field`: the copy is semantically equivalent to the source field, not necessarily a byte-identical re-declaration.

Both `%init` and `%add_field` build the schema's underlying MAML text, so you still call `parquet_parse_maml(schema)` afterwards to populate `schema%cinfo`/`schema%metadata` before writing, exactly as for a schema loaded from disk. List-shaped top-level sections (`coauthors:`, `comments:`, `keyarray:`, `extra:`, ...) are out of scope for `%init`: use `%add_metadata` for `keyarray:`-style entries, or author a `.maml` file for the rest.

`%add_field`'s `col_size`/`array_size` arguments always take a concrete positive integer — there's no in-code equivalent of MAML's `col_size: auto`/`array_size: auto` for a from-scratch schema. If a column's width is only known later, resolve it after `parquet_parse_maml` with `schema%set_col_size(name, col_size)`/`schema%set_array_size(name, array_size)` (both also work on a schema loaded from a `.maml` file that *does* declare `auto` — see [Deferring col_size/array_size until write time with `auto`](maml-format.html#deferring-col_sizearray_size-until-write-time-with-auto) for the full picture, including automatic resolution from a matrix write's own data shape).

The file form, `parquet_parse_maml(filename, schema)`, has the same "not already initialized" requirement as `%init` (and no `force=` option): loading a `.maml` file into a `schema` that is already initialized — via `%init` or an earlier parse — fails with `error stop`, rather than silently discarding whatever `schema` held before. Call `schema%clear()` first to reuse the same variable for a different file.

## Choosing which columns a schema writes

A schema usually declares more columns than any one program has data for — that is the point of a
shared schema. Every column a schema declares is **enabled** to begin with (whether it came from a
`.maml` file or from `%add_field`), and three type-bound procedures change or query that. Square
brackets mark an optional argument; they are not part of the code you write.

- **`call schema%set_column_unavailable([name])`** — disables `name`, or **every** column when
  called with no name at all.
- **`call schema%set_column_available([name])`** — enables `name`, or every column when called with
  no name.
- **`schema%is_column_set(name)`** — `.true.` if that column is currently enabled. `error stop`s if
  the schema has no such column, so it answers about a real column or not at all.

The usual shape is to turn everything off and then name what you actually have, which is what the
[combined example](combined-example.html#maml-schema-vector-columns-and-metadata) does:

```fortran
call schema%set_column_unavailable()      ! disable every column
call schema%set_column_available("id0")   ! ... then re-enable the two we have data for
call schema%set_column_available("idarr")
```

Being enabled or not decides three things, all of them at
[write](../io/writing.html#writer-options) time:

- **Only enabled columns reach the file.** `parquet_open_writer` takes its column list from them.
- **`parquet_close_writer` requires every enabled column to have been written**, and names the
  offending column, the output file and the schema if one was missed. Disabling a column you have no
  data for is therefore how you satisfy that check, rather than an optimisation.
- **A `write_maml=.true.` sidecar lists only the enabled columns**, so the saved `.maml` describes
  the file that was actually written.

Two behaviours worth knowing before you rely on them:

- **Writing a disabled column is silently skipped, not an error.** `parquet_write_column` returns
  without doing anything for a column the schema declares but has disabled — which is what lets a
  program call it unconditionally for every column it knows about and let the schema decide. Writing
  a name the schema does not declare **at all** is still an immediate `error stop`; the two cases are
  deliberately different.
- **A column excluded by a user MAML cannot be re-enabled.** Where a user MAML declares a subset of
  a base schema (`parquet_validate_user_maml`, see [The fields:
  section](maml-format.html#the-fields-section)), the columns it leaves out are marked
  *deactivated*, and naming one in either procedure fails with
  `error stop` rather than quietly overriding the user's own subset. The no-name (bulk) forms skip
  deactivated columns instead of failing, so `set_column_unavailable()` followed by
  `set_column_available()` is always safe.

## Runtime table metadata: schema%add_metadata and schema%clear_metadata

`schema%metadata%items` (read back via [`parquet_get_metadata`](../io/reading.html#reading-table-metadata-with-parquet_get_metadata)) is populated from two sources that end up indistinguishable in storage: the schema's own top-level header keys (`table:`, `survey:`, `dataset:`, `version:`, `date:`, `author:`, `description:`, `license:`, `MAML_version:` — whichever a real `keyarray:` entry declares, `table:` included) parsed by `parquet_parse_maml` (see [How table-level keys become metadata entries](maml-format.html#how-table-level-keys-become-metadata-entries) for which MAML key produces which entry), and any `schema%add_metadata(key, value [, description] [, warn])` call made afterward. `add_metadata` is a `generic` over every scalar/array `int32`/`int64`/`float32`/`float64`/`logical`/`string` type/kind (`float32`/`float64` also accept an optional `fmt` edit descriptor); duplicate keys are never rejected or overwritten — a later `add_metadata` call with a key already present just appends a second entry, and `parquet_get_metadata` always resolves the *first* match, so a duplicate silently has no effect on read.

**`warn` (default `.true.`) prints a `WARNING:` diagnostic to the console whenever a call is about to write a key that collides with something else**, in one of three ways, checked in this order (at most one warning per call):

1. **A key the parquet writer always injects itself** into the output file's key-value metadata (`DATE`, `name`, `IVOA.VOTable-Parquet.version`) — these are written unconditionally by `parquet_close_writer`, so an `add_metadata` call using one of them (e.g. `schema%add_metadata("DATE", ...)`) is always shadowed on read.
2. **A MAML top-level scalar key** (from `schema%init`'s own keyword list: `table`, `survey`, `dataset`, `version`, `date`, `author`, `description`, `license`, `maml_version`) that the schema's own MAML source already declared — e.g. calling `schema%add_metadata("author", ...)` on a schema whose MAML has an `author:` line. Only fires if that key is actually present; calling it on a schema whose MAML never declared `author:` does not warn.
3. **Any other key** that already has an entry — covers everything not in the two categories above, including auto-indexed keys (`comment_1`, `DOI_1`, ...) and `keyarray:`-supplied keys.

In every case, the call still appends the new entry exactly as it would with `warn=.false.` — `warn` only controls whether the collision gets printed, it never blocks or changes what gets written. Pass `warn=.false.` to suppress the diagnostic for a call you know is an intentional duplicate.

### A typed value records its own type

A parquet key-value pair can only hold text — the format has no typed value — so `add_metadata(key, 1024_int32)` stores the string `1024`, and a reader in a language without Fortran's declared types has nothing to tell it apart from a keyword whose value genuinely *is* the text `1024`. The writer therefore records the type alongside the value, as a companion entry named `<KEY>.datatype`, mirroring the `column.<name>.<attribute>` convention:

| Entry in the written file | Value |
|---|---|
| `NSIDE` | `1024` |
| `NSIDE.datatype` | `int32` |
| `PIXTYPE` | `HEALPIX` |

Five things to know about it:

- **Only the typed overloads record one.** The *string* overload emits no companion, so a genuinely textual keyword such as `PIXTYPE` above stays a plain string with no extra entry — which is what lets a reader tell the two cases apart at all.
- **The tokens are the same ones columns use** — `int32`, `int64`, `float32`, `float64`, `boolean` — so a reader needs one mapping table for both. An *array* overload adds a `[]` suffix (`int32[]`, …, and `string[]` for the string-array overload), because the stored value is a bracketed list such as `[1, 2, 3]`: a reader that coerced that with the scalar token would raise rather than get a usable value.
- **It is not an entry of `schema%metadata%items`.** A typed `add_metadata` call appends exactly one item, as it always did; the companion exists only in the written file. So it does not appear in a `write_maml=.true.` sidecar, does not become a VOTable `PARAM` of its own, and does not affect `%clear_metadata`.
- **A MAML-declared key never gets one.** Every table-level value declared in a `.maml` file is a string by design — see [How table-level keys become metadata entries](maml-format.html#how-table-level-keys-become-metadata-entries).
- **If you write a `<KEY>.datatype` entry yourself, yours wins.** `parquet_open_writer` warns and drops the one it would have synthesized, so the file never carries two entries of that name free to disagree. That key's VOTable `PARAM` then falls back to `datatype="char"` too: you have taken over declaring that key's type, so the library does not guess a second answer. Note the warning is emitted at *open* time, so a schema reused for several files warns once per file, and `add_metadata`'s own `warn=.false.` does not suppress it — that argument controls the duplicate-key check described above, which is a different one.

The VOTable sidecar declares the same types (`int`, `long`, `float`, `double`, `boolean`, and `char` for everything else) instead of calling every scalar a `char` — see [The VOTable sidecar](../io/reading.html#reading-table-metadata-with-parquet_get_metadata).

**`schema%add_metadata` must be called *after* `parquet_parse_maml`, never before** — calling it right after `%init`/`%add_field` but before the schema has been parsed fails with `error stop`, since `parquet_parse_maml` would otherwise silently discard that entry when it (re)builds `%metadata%items` from `%maml%lines`.

See the [combined example](combined-example.html#maml-schema-vector-columns-and-metadata) for `add_metadata` used in a complete, worked write.

**`call schema%clear_metadata()`** — discards every entry added by `%add_metadata` since the most recent `parquet_parse_maml`, keeping the base entries (the header keys, plus any real `keyarray:` entries) from that parse intact. A no-op if nothing has been added since the last parse.

## Printing column info with schema%print_schema_info

**`call schema%print_schema_info([unit, filename, prefix, header, table_name, dash_before_header, dash_after_header, dash_after_fields, dash_char, allow_uninitialized])`** — writes a fixed-width listing of this schema's *enabled* (`is_set`) columns, one per line, in the order `name unit type len ucd info` (`type`/`len` are the header labels for `data_type`/`col_size`; `info` is left unpadded so no line carries trailing whitespace). Column widths are computed from the longest value actually present (and the header label, if printed), so each call produces its own self-contained, internally-aligned block — two calls for different schemas are not aligned with each other. Works for any parsed schema (`schema%cinfo` populated), whether built in code (`%init`/`%add_field` followed by `parquet_parse_maml`) or loaded straight from a `.maml` file (`parquet_parse_maml`, which never calls `%init`) — this readiness check is `schema%is_parsed()`, not `schema%is_init()`: a from-scratch schema that has only had `%init`/`%add_field` called (never parsed) has `is_init() == .true.` but `is_parsed() == .false.`, and would still `error stop` here.

Calling this on a schema that hasn't been parsed yet (`schema%cinfo` not populated) fails with `error stop "... schema is not initialized (not parsed) ..."` by default; pass `allow_uninitialized=.true.` to silently print nothing instead (a complete no-op — no file is opened or touched at all, even in `filename=` mode) rather than aborting.

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
- `allow_uninitialized` — silently skip printing (no output, no error) instead of `error stop`-ing when the schema hasn't been parsed yet; default `.false.`.

Example, for a schema with `table: input_table` and columns `id`/`ra`/`dec` enabled:

```
Table name: input_table
name unit type    len ucd       info
-------------------------------------------
id        int32   1   meta.id   ID field.
ra   deg  float64 1   pos.eq.ra Right ascension
dec  deg  float64 1   pos.eq.dec Declination
```
