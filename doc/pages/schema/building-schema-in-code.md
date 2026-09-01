---
title: Building a schema in code with schema%init and schema%add_field
---

## Building a schema with init and add_field

A `parquet_schema` normally comes from a MAML file (via `parquet_parse_maml`), but it can also be
built entirely in memory, with no `.maml` file, using two type-bound procedures on `parquet_schema`.
This is the schema-authoring counterpart to [building a qc-maml in
code](quality-control.html#building-a-qc-maml-in-code), and produces a full write schema (columns +
table metadata), not just qc bounds. In the signatures below, square brackets mark an optional
argument; they are not part of the code you write.

A complete program, showing the call order everything below assumes:

```fortran
program build_schema
    use parquet
    use iso_fortran_env, only: int32, real64
    implicit none

    type(parquet_schema) :: schema
    type(parquet_writer) :: writer
    integer(int32) :: id(3) = [1_int32, 2_int32, 3_int32]
    real(real64)   :: ra(3) = [10.0d0, 20.0d0, 30.0d0]

    call schema%init(table="targets", author="me", description="A schema built in code")
    call schema%add_field("id", "int32",   ucd="meta.id",   info="Object identifier")
    call schema%add_field("ra", "float64", unit="deg", ucd="pos.eq.ra", info="Right ascension", &
        qc_min=">= 0", qc_max="<= 360")

    call schema%add_metadata("SURVEY_TILE", 42_int32)

    call parquet_open_writer(writer, "targets.parquet", schema)
    call parquet_write_column(writer, "id", id)
    call parquet_write_column(writer, "ra", ra)
    call parquet_close_writer(writer)
end program build_schema
```

**There is no separate parse step, and no required order.** `%init` and `%add_field` each parse
what they just wrote, so the schema is ready to query, write with, or add metadata to as soon as its
last field is declared. `%add_field` and `%add_metadata` may be interleaved however you like, and
`parquet_parse_maml` — which is what a schema loaded from a `.maml` file needs — is not called here
at all. Calling it anyway is harmless but does nothing.

### schema%init — starting a schema

**`call schema%init(table, [survey], [dataset], [version], [date], [author], [description], [license],
[maml_version], [force])`**

Starts a fresh schema and sets its top-level metadata. Only `table` is required, and it must be
non-empty; every other argument is an optional scalar top-level MAML key.

It must be called before `%add_field`, and only on a schema that isn't already initialized. An empty
`table`, or `schema%is_init() == .true.` — whether from an earlier `%init` call *or* from a MAML
parse, e.g. calling `%init` on a schema already loaded via `parquet_parse_maml` — fails with
`error stop`.

`force=.true.` (default `.false.`) is the exception: it bypasses the "already initialized" check and
fully resets the schema before rebuilding it from the new arguments, discarding any fields, qc and
metadata already added via `%add_field`/`%add_col_qc`/`%add_metadata`, and `schema%cinfo`/
`schema%metadata` regardless of whether those came from `%add_field` or a MAML parse. The schema ends
up exactly as if this were its first `%init` call. On a never-initialized schema `force=.true.` is
harmless — there is nothing to reset.

Since there is no source `.maml` file, `schema%maml%name` is set to `internal:<table>`, so
diagnostics that name the schema's source — `parquet_close_writer`'s missing-write error, for
instance — still have something useful to print.

### schema%add_field — declaring one column

**`call schema%add_field(name, data_type, [unit], [info], [ucd], [array_size], [col_size], [qc_min],
[qc_max], [qc_miss])`**

Appends one `fields:` entry. `name` and `data_type` are required; `data_type` is one of the
[supported types](../types/supported-data-types.html), including `date`/`time[unit]`/
`timestamp[unit,utc]` (see [Date, time and timestamp
columns](../types/date-time.html#units-and-schema-declared-columns)). The rest are optional and
mirror the MAML `fields:` attributes.

Everything is validated immediately — an empty or duplicate `name`, an invalid `data_type`, a
reversed qc operator, a bad `qc_miss` value — each failing with `error stop`. Calling it before
`schema%init` fails too.

`qc_min`/`qc_max`/`qc_miss` together form an optional `qc:` block, with the same operator and `miss:`
rules as [`add_col_qc`](quality-control.html#building-a-qc-maml-in-code). For a temporal `data_type`
the `qc_min`/`qc_max` bounds are rejected at validation, while `qc_miss` is accepted as on any other
type. A schema built with `%add_field`'s own `qc_min`/`qc_max`/`qc_miss` works directly as
`parquet_open_reader(..., schema=)`, with no separate `add_col_qc` call.

**`qc_miss` has three states, and the difference between omitting it and passing an empty string is
the whole point of it:**

| `qc_miss=` | what the schema says | Nulls checked? |
|---|---|---|
| omitted | nothing at all about Nulls | no |
| `"Null"` or `"NA"` (case-insensitive) | Nulls are an expected part of this column | no |
| `""` — an explicit empty string | Nulls are **not** expected in this column | **yes** |

When checking is on, the two sides behave differently by design: `parquet_write_column` prints a
`WARNING` and carries on, while a read through `parquet_open_reader(..., schema=)` **aborts** —
pass `qc_soft=.true.` for a `WARNING` there instead. See [Quality control](quality-control.html) for
the full min/max/miss enforcement picture on both sides. `qc_miss` may be set on any `data_type`.

### Checking, resetting and reading back a schema

- **`schema%is_init()`** — returns `.true.` once this schema is ready to use: either
  `schema%init`/`parquet_schema(...)` has completed (the in-code builder path), or
  `parquet_parse_maml` has populated `schema%cinfo` (a schema loaded from a `.maml` file/object,
  which never calls `%init` at all). `.false.` only for a freshly declared `parquet_schema` that has
  had neither happen yet. Note `%add_field`'s own "call schema%init(...) before adding fields" check
  is narrower — it specifically requires `%init` to have been called, since `%add_field` only makes
  sense on a from-scratch schema, not one already populated via a MAML parse.
- **`schema%is_parsed()`** — returns `.true.` once `schema%cinfo` is actually populated: after
  `parquet_parse_maml` for a schema loaded from a file, and after the **first `%add_field`** for one
  built in code. It is a narrower check than `%is_init()`, and the gap between the two is exactly
  one case: a schema that has had `%init` but not a single `%add_field` reports
  `%is_init() == .true.` and `%is_parsed() == .false.`, because a MAML with no fields is not a
  document that can be parsed. Use `%is_parsed()` — not `%is_init()` — to check readiness before
  calling anything that requires a populated `%cinfo`, such as `%get_field`, `%add_field_from`, or
  `%print_schema_info` (see [Printing column info with
  schema%print_schema_info](#printing-column-info-with-schemaprint_schema_info) below).
- **`call schema%clear()`** — resets the entire schema back to exactly the state a freshly declared,
  never-initialized `parquet_schema` starts in (`%maml`/`%cinfo`/`%metadata` all back to their
  defaults, `%is_init()` becomes `.false.` again). Unlike `%init(..., force=.true.)` (which resets
  and immediately rebuilds with new header-key arguments), `%clear` leaves the schema uninitialized
  — call `%init` again afterward to reuse the variable. Always succeeds, even on an already-blank
  schema (a no-op in that case); in fact `%init(..., force=.true.)` is implemented as `%clear`
  followed by a normal `%init`.
- **`schema = parquet_schema(table, [survey], [dataset], [version], [date], [author], [description],
  [license], [maml_version])`** — the structure-constructor form of `schema%init`: the same
  header-key arguments and the same validation, building and returning an initialized schema in one
  expression instead of
  declaring the variable and calling `%init` separately. There is no `force=` and no "already
  initialized" abort — the result is always a fresh schema, so there is never anything to reset.
- **`call schema%set_protected(name, [protected])`** — marks a column Null-protected, the
  code-level equivalent of listing it under a MAML's `extra: protected_cols:`; `protected` defaults
  to `.true.`, and `.false.` lifts protection. A protected column may hold no Null at all:
  `parquet_write_column` fails with `error stop` on an `is_valid` mask with any `.false.` entry, on
  a null `date`/`time`/`timestamp` element, and on an `%append_null()` in a `parquet_string_column`
  — and the column's Arrow field is written non-nullable, which is also the only way to declare a
  *streamed* temporal or `parquet_string_column` column null-free (see [Null
  values](../types/supported-data-types.html#null-values)). Call it before `parquet_open_writer`,
  since the writer takes its own copy of the schema at open time. Unprotecting a column that is
  currently protected is allowed but prints a `WARNING` naming the column — never an abort — since it
  overrides a declaration someone made deliberately. Where the protection came from makes no
  difference: a MAML's `extra: protected_cols:` and an earlier `%set_protected` call in code both
  warn on the way back out. `error stop`s if `name` isn't a declared field.
- **`call schema%get_field(name, [data_type], [unit], [info], [ucd], [array_size], [col_size], [qc_min],
  [qc_max], [qc_miss])`** / **`call schema%get_field(index, name, [...])`** — reads back an
  already-parsed field's full definition, the same shape of values `%add_field` accepts; every
  output beyond the
  lookup key is optional, so a caller can request only what it needs. The by-`index` form (1-based
  MAML source order, same order `get_num_fields`/`get_field_name` count) additionally returns the
  field's own `name`, since the caller doesn't already know it. `qc_min`/`qc_max` come back as a
  single operator-prefixed string (e.g. `">= 0"`), re-feedable straight into `%add_field`'s own
  `qc_min`/`qc_max` arguments — always with an explicit operator, even if the original call left it
  implicit, which is semantically identical but not necessarily byte-identical to the original
  input. `qc_miss` comes back as `"Null"` whenever the field allows Nulls — which covers both a
  declared `Null`/`NA` (the alias distinction isn't preserved in storage) *and* a field that declared
  no `qc: miss:` at all, since those mean the same thing — and as `""` only for the explicit empty
  `qc: miss:` that asks for Null validation. Feeding either straight back into `%add_field` therefore
  reproduces the same behaviour. `error stop`s if the field isn't found (by name) or
  the index is out of range; requires `schema%cinfo` to already be populated, which means
  `parquet_parse_maml` for a schema loaded from a file and at least one `%add_field` for one built
  in code.
- **`call schema%add_field_from(source_schema, name)`** — copies `name`'s full field definition from
  `source_schema` (via `%get_field`) and appends an equivalent field here via `%add_field`, so two
  schemas can share a column definition (e.g. a handful of "identity" columns — `obj_id`, `ra`,
  `dec`, ... — common to several output tables) without re-typing its type/unit/info/qc by hand and
  risking drift between the copies. `source_schema` must already be parsed (same precondition as
  `%get_field`); `this` must already have `%init` called, exactly like a direct `%add_field` call
  requires. Subject to the same qc_min/qc_max/qc_miss round-trip caveats as `%get_field`: the copy
  is semantically equivalent to the source field, not necessarily a byte-identical re-declaration.
  Concretely, the copy's own MAML text always spells out `array_size:` and `col_size:` even where the
  source declared neither, and a qc bound comes back operator-prefixed; an unresolved `auto` width
  copies as `auto`, and the source's Null policy is preserved exactly.
- **`schema%get_num_fields()`**, **`call schema%get_field_name(index, name)`** and
  **`schema%get_column_index(name)`** — the three small queries for walking a schema whose column
  names you don't already know: how many fields are declared, the name at a 1-based MAML source
  position, and the position of a named column. `%get_field_name` `error stop`s outside
  `1..get_num_fields()`, and `%get_column_index` `error stop`s if there is no such column, so both
  answer about a real field or not at all. All three need `schema%cinfo` populated, like `%get_field`.

Both `%init` and `%add_field` build the schema's underlying MAML text *and* parse what they add, so
`schema%cinfo`/`schema%metadata` are always in step with it and no `parquet_parse_maml` call is
needed — unlike a schema loaded from disk, which is one document arriving all at once.
List-shaped top-level sections (`coauthors:`, `comments:`,
`keyarray:`, `extra:`, ...) are out of scope for `%init`: use `%add_metadata` for `keyarray:`-style
entries, or author a `.maml` file for the rest.

`%add_field`'s `col_size`/`array_size` arguments normally take a concrete positive integer, but they
also accept the public `parquet_size_auto` constant, which writes `col_size: auto`/`array_size:
auto` into the schema exactly as a `.maml` file would — so a from-scratch schema can defer a width
just as a file-based one can. Resolve it later with `schema%set_col_size(name, col_size,
[force])`/`schema%set_array_size(name, array_size, [force])` any time before `parquet_open_writer`,
or let a matrix write resolve `col_size` from its own data shape. See [Deferring col_size/array_size
until write time with
`auto`](maml-format.html#deferring-col_sizearray_size-until-write-time-with-auto) for the full
picture, including what each setter rejects and why `force` exists; both setters work the same way
on a schema loaded from a `.maml` file that declares `auto`.

The file form, `parquet_parse_maml(filename, schema)`, has the same "not already initialized"
requirement as `%init` (and no `force=` option): loading a `.maml` file into a `schema` that is
already initialized — via `%init` or an earlier parse — fails with `error stop`, rather than
silently discarding whatever `schema` held before. Call `schema%clear()` first to reuse the same
variable for a different file.

## Choosing which columns a schema writes

A schema usually declares more columns than any one program has data for — that is the point of a
shared schema. Every column a schema declares is **enabled** to begin with (whether it came from a
`.maml` file or from `%add_field`), and three type-bound procedures change or query that.

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
  a name the schema does not declare **at all** is still an immediate `error stop`; the two cases
  are deliberately different.
- **A column excluded by a user MAML cannot be re-enabled.** Where a user MAML declares a subset of
  a base schema (`parquet_validate_user_maml`, see [The fields:
  section](maml-format.html#the-fields-section)), the columns it leaves out are marked
  *deactivated*, and naming one in either procedure fails with
  `error stop` rather than quietly overriding the user's own subset. The no-name (bulk) forms skip
  deactivated columns instead of failing, so `set_column_unavailable()` followed by
  `set_column_available()` is always safe.

## Runtime table metadata: schema%add_metadata and schema%clear_metadata

`schema%metadata%items` (read back via
[`parquet_get_metadata`](../io/reading.html#reading-table-metadata-with-parquet_get_metadata)) is
populated from two sources that end up indistinguishable in storage: the schema's own top-level
header keys (`table:`, `survey:`, `dataset:`, `version:`, `date:`, `author:`, `description:`,
`license:`, `MAML_version:` — whichever a real `keyarray:` entry declares, `table:` included) parsed
by `parquet_parse_maml` (see [How table-level keys become metadata
entries](maml-format.html#how-table-level-keys-become-metadata-entries) for which MAML key produces
which entry), and any `schema%add_metadata(key, value, [description], [warn])` call made
afterward. `add_metadata` is a `generic` over every scalar/array
`int32`/`int64`/`float32`/`float64`/`logical`/`string` type/kind (`float32`/`float64` also accept an
optional `fmt` edit descriptor); duplicate keys are never rejected or overwritten — a later
`add_metadata` call with a key already present just appends a second entry, and
`parquet_get_metadata` always resolves the *first* match, so a duplicate silently has no effect on
read.

**`warn` (default `.true.`) prints a `WARNING:` diagnostic to the console whenever a call is about
to write a key that collides with something else**, in one of three ways, checked in this order (at
most one warning per call):

1. **A key the parquet writer always injects itself** into the output file's key-value metadata
   (`DATE`, `name`, `IVOA.VOTable-Parquet.version`) — these are written unconditionally by
   `parquet_close_writer`, so an `add_metadata` call using one of them (e.g.
   `schema%add_metadata("DATE", ...)`) is always shadowed on read.
2. **A MAML top-level scalar key** (from `schema%init`'s own keyword list: `table`, `survey`,
   `dataset`, `version`, `date`, `author`, `description`, `license`, `maml_version`) that the
   schema's own MAML source already declared — e.g. calling `schema%add_metadata("author", ...)` on
   a schema whose MAML has an `author:` line. Only fires if that key is actually present; calling it
   on a schema whose MAML never declared `author:` does not warn.
3. **Any other key** that already has an entry — covers everything not in the two categories above,
   including auto-indexed keys (`comment_1`, `DOI_1`, ...) and `keyarray:`-supplied keys.

In every case, the call still appends the new entry exactly as it would with `warn=.false.` — `warn`
only controls whether the collision gets printed, it never blocks or changes what gets written. Pass
`warn=.false.` to suppress the diagnostic for a call you know is an intentional duplicate.

### A typed value records its own type

A parquet key-value pair can only hold text — the format has no typed value — so `add_metadata(key,
1024_int32)` stores the string `1024`, and a reader in a language without Fortran's declared types
has nothing to tell it apart from a keyword whose value genuinely *is* the text `1024`. The writer
therefore records the type alongside the value, as a companion entry named `<KEY>.datatype`,
mirroring the `column.<name>.<attribute>` convention:

| Entry in the written file | Value |
|---|---|
| `NSIDE` | `1024` |
| `NSIDE.datatype` | `int32` |
| `PIXTYPE` | `HEALPIX` |

Five things to know about it:

- **Only the typed overloads record one.** The *string* overload emits no companion, so a genuinely
  textual keyword such as `PIXTYPE` above stays a plain string with no extra entry — which is what
  lets a reader tell the two cases apart at all.
- **The tokens are the same ones columns use** — `int32`, `int64`, `float32`, `float64`, `boolean` —
  so a reader needs one mapping table for both. An *array* overload adds a `[]` suffix (`int32[]`,
  …, and `string[]` for the string-array overload), because the stored value is a bracketed list
  such as `[1, 2, 3]`: a reader that coerced that with the scalar token would raise rather than get
  a usable value.
- **It is not an entry of `schema%metadata%items`.** A typed `add_metadata` call appends exactly one
  item; the companion exists only in the written file. So it does not appear in a
  `write_maml=.true.` sidecar, does not become a VOTable `PARAM` of its own, and does not affect
  `%clear_metadata`.
- **A MAML-declared key never gets one.** Every table-level value declared in a `.maml` file is a
  string by design — see [How table-level keys become metadata
  entries](maml-format.html#how-table-level-keys-become-metadata-entries).
- **If you write a `<KEY>.datatype` entry yourself, yours wins.** `parquet_open_writer` warns and
  drops the one it would have synthesized, so the file never carries two entries of that name free
  to disagree. That key's VOTable `PARAM` then falls back to `datatype="char"` too: you have taken
  over declaring that key's type, so the library does not guess a second answer. Note the warning is
  emitted at *open* time, so a schema reused for several files warns once per file, and
  `add_metadata`'s own `warn=.false.` does not suppress it — that argument controls the
  duplicate-key check described above, which is a different one.

The VOTable sidecar declares the same types (`int`, `long`, `float`, `double`, `boolean`, and `char`
for everything else) instead of calling every scalar a `char` — see [The VOTable
sidecar](../io/reading.html#reading-table-metadata-with-parquet_get_metadata).

**`schema%add_metadata` may be called at any point after `%init`** (or, for a file-loaded schema,
after `parquet_parse_maml`), and freely interleaved with `%add_field`. What it needs is a metadata
table to add to, which is what those two establish; calling it on a schema that has had neither
fails with `error stop`, since the entry would be discarded by whichever of them ran next.

See the [combined example](combined-example.html#maml-schema-vector-columns-and-metadata) for
`add_metadata` used in a complete, worked write.

**`call schema%clear_metadata()`** — discards every entry added by `%add_metadata`, keeping the base
entries (the header keys, plus any real `keyarray:` entries) the schema's own MAML declared. A no-op
if `%add_metadata` has not been called.

## Printing column info with schema%print_schema_info

**`call schema%print_schema_info([unit], [filename], [prefix], [header], [table_name],
[dash_before_header], [dash_after_header], [dash_after_fields], [dash_char],
[allow_uninitialized])`** — writes a fixed-width
listing of this schema's *enabled* (`is_set`) columns, one per line, in the order `name unit type
len ucd info` (`type`/`len` are the header labels for `data_type`/`col_size`; `info` is left
unpadded so no line carries trailing whitespace). Column widths are computed from the longest value
actually present (and the header label, if printed), so each call produces its own self-contained,
internally-aligned block — two calls for different schemas are not aligned with each other. Works
for any parsed schema (`schema%cinfo` populated), whether built in code (`%init` plus at least one
`%add_field`) or loaded straight from a `.maml` file (`parquet_parse_maml`, which never calls
`%init`) — this readiness check is `schema%is_parsed()`, not `schema%is_init()`: a schema that has
had `%init` and no field yet has `is_init() == .true.` but `is_parsed() == .false.`, and would still
`error stop` here.

Calling this on a schema that hasn't been parsed yet (`schema%cinfo` not populated) fails with
`error stop "... schema is not initialized (not parsed) ..."` by default; pass
`allow_uninitialized=.true.` to silently print nothing instead (a complete no-op — no file is opened
or touched at all, even in `filename=` mode) rather than aborting.

This is *solicited* output, so `parquet_set_verbosity("silent")` (or `"errors_only"`) turns the
whole call into the same complete no-op — again without opening or creating the file in `filename=`
mode — while a bad call (neither `unit` nor `filename`, an unopened `unit`, an unparsed schema
without `allow_uninitialized=.true.`) still `error stop`s, because the argument checks run first.
See [Terminal output](../operating/settings.html#terminal-output).

Exactly one of `unit`/`filename` must identify the destination:

- **`unit`** — an already-open unit. This is the primary way to print several schemas into one
  combined listing: open the unit once yourself and call `%print_schema_info(unit=...)` repeatedly,
  once per schema; each call appends its own block.
- **`filename`** — opens the file with `position="append"`, writes, and closes again before
  returning — a convenience for a one-off call, or for accumulating across separate calls without
  managing a unit yourself.

Giving neither, or an already-given `unit` that isn't open, or open for reading only, or a
`unit`+`filename` pair where `filename` doesn't match (exact, trimmed string equality against
`inquire(unit=unit, name=)`) the file the unit is actually connected to — all `error stop`.

Optional formatting arguments:

- `prefix` — prepended to every emitted line (e.g. `prefix="# "` for a shell/FITS-style comment
  block); default none.
- `header` — print a `name unit type len ucd info` header row; default `.true.`.
- `table_name` — print a `Table name: <table>` line, using this schema's required MAML `table:` key,
  positioned after `dash_before_header` and before the header row; default `.true.`.
- `dash_before_header` / `dash_after_header` / `dash_after_fields` — dashed separator lines at each
  position (independent of each other and of `header`/`table_name`); `dash_after_header` defaults
  `.true.`, the other two default `.false.`.
- `dash_char` — character used to draw dashed lines; default `"-"`.
- `allow_uninitialized` — silently skip printing (no output, no error) instead of `error stop`-ing
  when the schema hasn't been parsed yet; default `.false.`.

Example, for a schema with `table: input_table` and columns `id`/`ra`/`dec` enabled:

```
Table name: input_table
name unit type    len ucd        info
------------------------------------------------
id        int32   1   meta.id    ID field.
ra   deg  float64 1   pos.eq.ra  Right ascension
dec  deg  float64 1   pos.eq.dec Declination
```
