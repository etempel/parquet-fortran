---
title: The MAML metadata format
---

The library turns a MAML (YAML) metadata file into the VOTable-style header embedded in the
`.parquet` file — the flow is:

```
  .maml file  (YAML metadata)
       |
       |  parquet_parse_maml
       v
  parquet_schema
    %cinfo    ->  per-column definitions
    %metadata ->  table-level metadata
       |
       |  parquet_open_writer
       v
  .parquet file
    * VOTable-style header  (per-column unit/info/ucd + table author/description/keyarray, ...)
    * column data
```

A [MAML](https://github.com/asgr/MAML-Format) file is YAML. Table-level metadata (author,
description, ...) is given as top-level keys, and column definitions are given as a list under the
`fields:` key.

**Only a fixed, known set of top-level keys is accepted** — `survey`, `dataset`, `table`, `version`,
`date`, `author`, `coauthors`, `dois`, `depends`, `description`, `comments`, `license`, `keywords`,
`maml_version`, `keyarray`, `extra`, and `fields` (see `allowed_maml_sections` in
`src/parquet_metadata_maml.f90`) — any other top-level key fails `parquet_validate_maml` as an
unknown section.

**Every MAML key is case-insensitive**, at every level: `Extra:`, `FIELDS:` and `Data_Type:` mean
exactly what their lowercase spellings mean. **A value drawn from a fixed vocabulary is
case-insensitive too** — a `data_type:` token (`INT32`, `List[Int32]` and `STRUCT` all parse), the
`auto` width placeholder, `unit: unitless`, `qc: miss:`'s `Null`/`NA`, and `sort:`'s
`asc`/`desc`/`nulls_first`/`nulls_last`. **A value that is *data* is not**: a column name in
`col_map:`, `remap:`, `protected_cols:` or `nullable_cols:` must match the column exactly.

To attach your own custom metadata not covered by that list, nest it under `extra:` instead, which
accepts arbitrary structure unvalidated (see [Renaming columns for output with
`col_map:`](#renaming-columns-for-output-with-col_map) for an example of `extra:`'s own nested
keys).

`parquet_parse_maml` always runs this same validation before parsing a MAML into a `parquet_schema`,
so an invalid MAML is caught immediately rather than silently parsed.

Four full worked examples are checked into the repository:

- [schemas/maml_example.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example.maml)
  — the base example used throughout these docs.
- [schemas/maml_example2.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example2.maml)
  — adds `string` fields and `qc: min:`/`max:` bounds (both the plain-number and the quoted-operator
  forms).
- [schemas/maml_example3.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example3.maml)
  — adds `extra: col_map:` column renaming (e.g. `id` → `uberid`, `RA` → `ra_J2000`) alongside
  `qc:`.
- [table_types/maml_example4.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/table_types/maml_example4.maml)
  — a **table-type** schema rather than a write schema: the input
  [`tools/generate_user_table_code.py`](../utilities/generated-tables.html) turns into a named
  `parquet_table` extension. It lives under `table_types/` rather than `schemas/` for that reason,
  and it is the one role in which `col_size: auto`/`array_size: auto` are rejected.

The MAML format itself is defined by the [MAML specification](https://github.com/asgr/MAML-Format);
this page describes what *this library* reads and writes.

For a complete program that loads a MAML, writes a file with it and reads the result back, see the
[Combined example](combined-example.html) — the sections below are a reference for the file format,
not a tutorial for the API.

If you're new to MAML in this library, focus first on `table:` and `fields:` (`name` + `data_type`
for each field). Everything else is optional metadata or advanced behavior.

**A `.maml` source line is limited to 1024 characters** — a longer line (e.g. an unusually long
`info:`/`description:`/`keywords:` value, or a long `protected_cols:` list) fails with `error stop`
naming the offending line number, rather than being silently truncated. A `.maml` file with
Windows-style CRLF line endings is handled transparently (the trailing `\r` is stripped on read) and
needs no special handling.

An abridged version of the base example:

```yaml
dataset: input_data
table: input_table          # required by parquet_validate_maml
author: Dave Smith <dave_smith_is_not_here@gmail.com>
description: Just an example. A few sentences is usually about right.
keyarray:
- key: test_scalar
  value: 8.1
  comment: something

fields:
- name: id0                 # required
  unit: unitless
  info: ID field.
  ucd: meta.id;meta.main
  data_type: int32           # required, see Supported data types
- name: idarr
  data_type: int64
  col_size: 2                # vector column of length 2 per row
- name: name
  data_type: string
  array_size: 18             # max string length
- name: myflag
  data_type: boolean
```

## The fields: section

Notes on the `fields:` entries:

- `name` and `data_type` are required for every field; `data_type` must be one of the [supported
  types](../types/supported-data-types.html)' MAML names — including
  `date`/`time[unit]`/`timestamp[unit,utc]` (e.g. `timestamp[ns,utc]`); see [Date, time and
  timestamp columns](../types/date-time.html#units-and-schema-declared-columns) for the full
  unit/timezone token syntax.
- `col_size` (default `1`) makes the column a fixed-length vector column, read/written as a 2D array
  of shape `(col_size, nrows)`.
- `list[<elemtype>]` declares a **variable-length** list column, whose rows may hold different
  numbers of values — read and written through a
  [`parquet_list_column`](../types/list-columns.html). `<elemtype>` is one of the nine payload
  kinds (`int32`, `int64`, `float32`, `float64`, `boolean`, `string`, `date`, `time[unit]`,
  `timestamp[unit,utc]`) and is **required**: a bare `list` is rejected, because a declared column
  that is never written has to be written with zero rows at close and that cannot invent an element
  type. `col_size:` does not apply to such a column — `auto` and any value above 1 are rejected,
  since a list row's length comes from the data — and neither does `array_size:`: `auto` is
  rejected, as it is for every non-`string` column (a `list[string]` is not one), and a positive
  value is accepted and never consulted.
  > That makes the naming trap below wider than two-way: `col_size` is how many elements a
  > **vector** row holds, `array_size` is how many characters a **string** value holds, and no
  > **container** column — `list[...]`, `map[...]` or `struct` — has either, because a container's
  > rows carry their own lengths rather than sharing one width.
- `struct` declares a **struct** column, whose every row holds one value per declared field — read
  and written through a [`parquet_struct_column`](../types/struct-columns.html). The token is bare
  and takes no bracket: **a struct's fields are never declared in MAML at all**, because the field
  names, kinds and order come from the `parquet_struct_column` object passed to
  `parquet_write_column`, which is data the caller already holds. `col_size:` does not apply —
  `auto` and any value above 1 are rejected, since a struct row is one instance rather than a row
  of values — and neither does `array_size:`, on the same terms as a list's: `auto` rejected, a
  positive value accepted and never consulted. So is `qc: min:`/`max:` rejected — quality-control
  ranges are scalar-leaf only. `qc: miss:` **is** supported and applies to row nullness (an absent
  struct instance).
- `map[<valuetype>]` declares a **map** column, whose every row holds its own set of
  `key -> value` entries — read and written through a
  [`parquet_map_column`](../types/map-columns.html). `<valuetype>` is one of the same nine kinds
  `list[...]` accepts (`map[timestamp[ms,utc]]` is a valid declaration) and is **required**: a bare
  `map` is rejected as an unknown `data_type`, for the same reason a bare `list` is — a
  declared-but-unwritten column has to be written with zero rows at close, and that cannot invent a
  value kind. **Bare `struct` is the exception, and the difference is real**: a struct's field
  layout cannot be expressed in MAML at all, so there is nothing to leave out, whereas a map's value
  type is a single token the schema can perfectly well carry. **The token carries no key type**,
  because map keys are always strings: `map[int32]` means `map<string,int32>`.
  A container value type cannot be declared either: `map[list[int32]]` is rejected as an unknown
  `data_type`, and a map whose values are containers can be read from a file but not written.
  `col_size:` does not apply — `auto` and any value above 1 are rejected, since a map row's entry
  count comes from the data — and neither does `array_size:`, on the same terms as a list's:
  `auto` rejected, a positive value accepted and never consulted. `qc: min:`/`max:` is rejected on
  the same scalar-leaf-only terms as above; `qc: miss:` **is** supported and applies to row
  nullness (an absent map).
- `array_size` sets the maximum string length for `string` columns; it is ignored for other types.
  Writing a longer value through an ordinary `character` array is an error. A
  [`parquet_string_column`](../types/string-columns.html) write is the one exception: it stores each
  element's own bytes and a reader takes each length from the data, so `array_size` is not needed to
  read the column back and is not enforced on that path. Such a write is **accepted**, emits one
  `WARNING` naming the column and the actual length, and the written metadata — both the file's own
  `column.<name>.array_size` and a `write_maml=.true.` sidecar — then reports what was really
  written rather than the declaration it outgrew.
  > Don't confuse `col_size` with `array_size` — despite the similar-sounding names, they're
  > unrelated: `col_size` is how many elements a vector column's row holds, `array_size` is how many
  > characters a `string` column's values can hold. And a **container** column (`list[...]`,
  > `map[...]`, `struct`) declares NEITHER: both fix a width that is the same in every row, which
  > is exactly the property a container does not have — its rows carry their own lengths. **What
  > tells a reader of a sidecar that a column's width is per-row is its `data_type:` token**, which
  > is always there. Neither size key ever is: a `write_maml=.true.` sidecar drops both from a
  > container's block even where the source MAML declared `col_size: 1`, and a schema-less
  > `parquet_write_table` never writes either one — so the two routes agree.
- Either can be declared `auto` instead of a number (`col_size: auto` / `array_size: auto`) when the
  value is only known to the calling Fortran code, not in advance in the MAML file itself — see
  [Deferring col_size/array_size until write time with
  `auto`](#deferring-col_sizearray_size-until-write-time-with-auto) below.
- `unit`, `info` and `ucd` are optional and are carried through into the parquet file's
  VOTable-style header for that column. Two details are easy to miss: the value `unitless` (in any
  capitalization) means *no unit at all* and is stored as an empty string, so a column declaring
  `unit: unitless` is written with no unit; and `ucd:` accepts either a single string or a YAML
  list, whose items are joined with `;` into one value (`schemas/maml_example3.maml` uses the list
  form).
- `qc:` declares this column's quality-control bounds as a `min:`/`max:`/`miss:` sub-block — see
  [Quality control](quality-control.html) for the operators, the enforcement rules and the
  code-level equivalent.
- `source` is read only by
  [`tools/generate_user_table_code.py`](../utilities/generated-tables.html), which turns a MAML into
  a generated `parquet_table` extension type: `source: file` (the default) means the column is read
  from the parquet file, `source: computed` that the program fills it in and no file column is
  looked for. It is accepted and ignored everywhere else, so a table-type schema stays usable as an
  ordinary write schema.
- Run `parquet_validate_maml` on a MAML file to catch structural mistakes before using it to open a
  writer: a missing or empty `table:`, no `fields:` entries at all, a duplicate or empty field name,
  an unrecognized `data_type`, a `col_size:`/`array_size:` that is neither a positive integer nor
  `auto`, `array_size: auto` on a column that is not `string`, a `col_size:` of `auto` or above 1 on
  a container column, a `qc: min:`/`max:` on a container column, a `qc: min:`/`max:` on a
  `date`/`time`/`timestamp` column (a `qc: miss:` on either is supported and is not refused), a
  reversed qc operator (`min:` with `<`, or `max:` with `>`), a qc bound that will not convert to
  the column's own numeric type, an `extra: protected_cols:` or `extra: nullable_cols:` name that is
  not a declared column, one column named under both of those keys at once, and any unknown
  top-level section or field sub-key. It accepts
  either a `parquet_maml_file` (e.g. from `parquet_load_maml_file`, or built in memory) or a
  filename directly (`call parquet_validate_maml("schemas/maml_example2.maml")`, loading it from
  disk internally). One `error stop` reports every problem it found, not just the first — except
  that a `fields:` entry omitting `name:` or `data_type:` **entirely** stops earlier still, in the
  parse itself, with a `parquet_read_maml: missing ... in fields block` message naming only that
  one.

A second MAML file may be validated against a "base" MAML with `parquet_validate_user_maml`, to
check it only reuses column names that already exist in the base schema — useful when different
pipeline stages should write a subset of a shared schema.

## Deferring col_size/array_size until write time with `auto`

Sometimes a vector column's width, or a string column's maximum length, is only known to the calling
Fortran code — not in advance, when the MAML file is written. Declaring `col_size: auto` (vector
columns) or `array_size: auto` (`string` columns only) defers that value instead of requiring a
concrete number up front. `parquet_validate_maml` accepts `auto` as well-formed; it is resolved to a
concrete positive integer before any data for that column is actually written, in one of two ways:

On the Fortran side, an unresolved `auto` column's `%cinfo%col(:)%col_size`/`%array_size` holds the
public `parquet_size_auto` constant (`integer, parameter :: parquet_size_auto = -1`) until it's
resolved — compare against this constant (rather than a hardcoded `-1`) if your own code needs to
check whether a column is still unresolved.

- **Explicitly**, any time between `parquet_parse_maml` and `parquet_open_writer`, by calling
  `schema%set_col_size(name, col_size, [force])` / `schema%set_array_size(name, array_size,
  [force])` — square brackets mark an optional argument here and everywhere else on this page. Each
  error stops if `name` isn't a declared column, if the given value isn't a positive integer, or —
  for `set_array_size` — if `name` isn't a `string` column. By default, either setter only accepts a
  column that is currently `auto` (error stops otherwise); pass `force=.true.` to override an
  already-resolved value too.
- **Automatically**, from the first `parquet_write_column`/`parquet_write_column_chunk` call for
  that column that can supply the value: `col_size` resolves to `size(values, 1)` on a *matrix* (2D
  array) write, and `array_size` to the caller's own declared Fortran character length
  (`len(values(1,1))` for a matrix write, `len(values)` for a flat 1-D one). Both are taken from
  that call's own data shape, not its content. Once resolved (either way), every later write to that
  column is checked against the resolved value exactly like an explicitly declared
  `col_size`/`array_size` is.

The two differ in which write shapes can resolve them. `array_size` is resolved by a 1-D string
write as readily as by a matrix one, so a scalar `string` column declared `array_size: auto` needs
nothing done to it. `col_size` cannot be: the flat (1-D) `parquet_write_column` form needs
`col_size` already known to divide its own flat array into rows, so it error stops if `col_size` is
still `auto` by the time it's called; resolve it with `schema%set_col_size` first, or write that
column via its matrix form instead.

`auto` is a write-side deferral only. It is rejected outright in a table-type (Role-A) schema handed
to [`tools/generate_user_table_code.py`](../utilities/generated-tables.html), whose generated
accessors need each column's kind and rank fixed when the code is generated.

If the schema was opened with `parquet_open_writer(..., write_maml=.true.)`, the sidecar `.maml`
file is written at `parquet_close_writer` time (not at `parquet_open_writer` time), once every
column's `col_size`/`array_size` is guaranteed resolved — so the sidecar always shows the actual
resolved value, never a leftover `auto` placeholder, matching what was actually written to the
`.parquet` file.

That last part is a rule the sidecar keeps even where the declaration and the data disagree: a
`string` column written through a [`parquet_string_column`](../types/string-columns.html), which
does not enforce `array_size`, is reported at the longest element actually written whenever that
exceeds what the MAML declared. A `.maml` this library produces always describes the file beside it.

## Renaming columns for output with `col_map:`

A user MAML's `fields:` names are normally required to match the base schema's column names exactly.
`col_map:` relaxes that: it lets a user MAML give a column an arbitrary name of its own choosing for
the `fields:` section (and the resulting `.parquet`/sidecar `.maml`), while Fortran code continues
to call `parquet_write_column`/`set_column_available`/`get_column_index`/etc. with the stable,
well-known internal name from the base schema. `col_map:` is **not** a top-level MAML section — it
is only recognized nested inside `extra:` (a bare top-level `col_map:` is rejected as an unknown
section):

```yaml
table: user_table
extra:
  col_map:
  - id0: my_id
fields:
- name: my_id       # the user's own chosen name -- can differ freely from id0
  data_type: int32  # still declared in full, exactly like a non-renamed field
```

- Each `col_map:` item is `<internal_name>: <output_name>`. `parquet_validate_user_maml` checks
  these are unambiguous — each `internal_name` must exist in the base schema and not also appear
  un-renamed elsewhere in `fields:`; each `output_name` must actually be declared in this MAML's own
  `fields:` and not collide with another column's name. A violation fails validation with a message
  naming the specific problem.
- The renamed field's `fields:` entry (`my_id` above) is validated exactly like any other field
  entry (`data_type` required, etc.) — nothing is inherited from the base column's own attributes.
- After `parquet_parse_maml`, the resulting `parquet_column_type` always uses the internal name
  (`id0`) for `schema%cinfo%col(:)%name` — the same name every other API (`parquet_write_column`,
  `set_column_available`, `get_column_index`, ...) already expects — with the rename available
  separately as `schema%cinfo%col(:)%output_name` (`my_id`), which is what actually gets written to
  the `.parquet` file's schema/VOTable header and to a `write_maml=.true.` sidecar's `fields:`
  section.
- `user_maml%col_map` (populated by `parquet_validate_user_maml`) exposes the parsed entries for
  inspection.
- Since it lives inside `extra:`, `col_map:` does not produce any table-level metadata entry of its
  own (nor do `protected_cols:` and `nullable_cols:`, `extra:`'s other write-side
  specifically-parsed keys — see [Null values](../types/supported-data-types.html#null-values));
  anything else nested inside `extra:` is accepted unvalidated and otherwise unused.

## Renaming columns for reading with `extra: remap:`

`col_map:` above relabels columns on the way OUT (writing). `remap:` is its read-side counterpart,
and it is used by `parquet_table` only: a read-in MAML handed to `parquet_open_table(table, file,
maml=...)` can give the file's columns table-facing names of its own, so program code never has to
know what a column is physically called. Like `col_map:`, it is recognized only nested inside
`extra:`.

```yaml
table: input_table
extra:
  remap:
  - mass: MASS_KG      # internal name `mass` reads the file's MASS_KG column
  - ra: RA_J2000
```

- Each item is `<internal_name>: <file_column_name>`, i.e. the **opposite** direction to
  `col_map:`'s `<internal_name>: <output_name>`. The value must be a column the file actually has,
  or the open aborts naming it; an internal name may not be declared twice.
- The internal name is what every table API uses (`%get`, `%col`, `%kind`, `%rename_column`, ...).
  The file column keeps being what is actually read, so `%reload` goes back to the same place, and
  `%rename_column` changes the lookup name only — remap and rename compose freely.
- **An internal name is allowed to equal one of the file's own column names**, and does not then
  mean "itself": in `- ra: dec`, internal `ra` reads the file's `dec`. The file's own `ra` column
  simply becomes unreachable unless some other entry points at it. That shadow is deliberate, not an
  error — a file often carries columns a program does not want, and one of them sharing a name must
  not make that name unusable.
- **Two internal names may read the same file column.** They become two ordinary, independent table
  columns: identical when first read, and free to diverge afterwards, since each holds its own copy.
- A read-in MAML is held to a lighter standard than a write schema: its top-level sections and field
  sub-keys are still checked against the same allowed set, but `table:` and `fields:` are not
  required and no `data_type:` is inspected, since it describes a file that already exists rather
  than one about to be written. The `table:` line in the examples here is therefore optional.
- `remap:` is not the only part of a read-in MAML that is read. A `fields:` entry's `unit:` gives
  that column its unit (see [Units](../tables/table-open.html#units)), and a `fields:` entry's `qc:`
  block is enforced when the column is first touched (see [Quality control](quality-control.html)).
  Both name the **file's** columns, as everything outside `remap:` does.
- See [Tables](../tables/table-open.html#renaming-a-files-columns-with-a-read-in-maml) for the
  table-side view.

## Filtering and sorting on read with `extra: filter:` and `extra: sort:`

A read-in MAML can also say which rows to keep and what order to return them in — again
`parquet_table` only, and again recognized only nested inside `extra:`. Both are plain YAML string
lists, and both name the **file's own** columns (a read-in MAML describes the physical file, so
`remap:` above is the only place its internal names appear at all).

```yaml
table: input_table
extra:
  filter:
  - "(ra > 180 and ra <= 360) or ra is_null"
  - "id is_not_null"
  sort:
  - "ra asc"
  - "quality desc nulls_first"
```

- Each `filter:` entry is one rule in the existing [`parquet_filter`
  grammar](../io/filter-sort-sample.html#the-rule-grammar), unchanged. Entries are AND-combined with
  each other, exactly as several `filt%add` calls are, and then with any `filter=` the caller
  passed.
- Each `sort:` entry is one key in the existing [`parquet_sortkey`
  grammar](../io/filter-sort-sample.html#sort-keys) — `"<column> [asc|desc]"`, or a leading `-` for
  descending — plus **one MAML-only extension: an optional trailing `nulls_first` or `nulls_last`**
  (case-insensitive; omitted means `nulls_last`, matching `%add`'s own default). It spells out in
  text what the Fortran API expresses as `%add(key, nulls_first=.true.)`, since a plain string list
  has nowhere else to carry a per-key flag.
- Keys apply in list order, and the MAML's keys come **before** any the caller passed in `sort=`, so
  the MAML's are the primary ones and the caller's break its ties.
- `sort:` is refused when the table is opened as a
  [slice](../tables/table-open.html#reading-part-of-a-file-the-slice-regime) — a sort reorders rows
  across the whole file, which would leave the slice's row range naming a different set of rows than
  the caller chose. The abort happens before the file is opened and names the offending MAML.
  `filter:` has no such restriction and applies within the slice.
- See [Tables](../tables/table-open.html#filtering-sorting-and-checking-rows-as-the-file-is-opened)
  for the table-side view and the composition rules.

## How table-level keys become metadata entries

Table-level keys become parquet metadata entries, with these special mappings:

| Top-level key | Becomes |
|---|---|
| `keyarray:` | A list of `key`/`value`/`comment` maps; each becomes one metadata entry named by its `key`. |
| `DOIs:` | A list of `DOI`/`type` maps; becomes `DOI_1`, `DOI_2`, ... entries (value = DOI, description = type). |
| `depends:` | A list of `survey`/`dataset`/`table`/`version` maps — those four sub-keys and no others — for referencing upstream datasets this table was built from; becomes `depends_1`, `depends_2`, ... entries, each value being the four fields joined with `;` in that fixed order (regardless of the order they appear in the file; any missing sub-key becomes an empty segment). |
| `comments:` / `coauthors:` | Plain string lists; become `comment_1`, `comment_2`, ... / `coauthor_1`, `coauthor_2`, ... entries. |
| `keywords:` | A plain-string list, combined into a single `keywords` entry with its items joined by `;`. |
| any other allowed section, given as a plain-string list (e.g. `survey:`, `author:`, `license:`, ...) | Several entries that all share that key's name (e.g. multiple `list_key` entries with the same name). |
| any other allowed section, given as a list of *maps* | **Not** specially handled: only its first sub-key ends up captured as a raw, unparsed string, and the rest of that entry's sub-keys are silently dropped. Use `keyarray:` for arbitrary structured metadata instead. |

Every entry in this table is read back on the read side with `parquet_get_metadata` (see [Reading
table metadata with
`parquet_get_metadata`](../io/reading.html#reading-table-metadata-with-parquet_get_metadata)), with
one caveat for the two rows above that produce **several entries sharing one key**: a lookup by key
answers with the first of them, so use
[`parquet_get_metadata_items`](../io/reading.html#listing-every-metadata-entry) — which lists every
entry in file order, repeats included — to reach the rest. A schema can also add further entries at
runtime that were never in the MAML file at all — see [Runtime table
metadata](building-schema-in-code.html#runtime-table-metadata-schemaadd_metadata-and-schemaclear_metadata).

### `extra:` produces no metadata entry, but is not ignored

`extra:` is the one section whose contents never become table-level metadata — which is what makes
it the right home for anything the fixed key list does not cover. It is still read, though: the
nested keys in the table below are parsed specifically out of it, and everything else in it is
accepted unvalidated and otherwise unused.

Which of them applies depends on what the MAML is *for*, and that is the distinction to hold on to
when reading the sections above:

| nested key | belongs to | what it does |
|---|---|---|
| `col_map:` | a **write** schema | renames a column on the way out — [Renaming columns for output](#renaming-columns-for-output-with-col_map) |
| `protected_cols:` | a **write** schema | forbids Nulls in the named columns — [Null values](../types/supported-data-types.html#null-values) |
| `nullable_cols:` | a **write** schema | declares that the named columns may hold a Null, so their fields are written nullable whatever the values are — [Null values](../types/supported-data-types.html#null-values) |
| `remap:` | a **read-in** MAML (`parquet_table`) | gives a file's columns table-facing names — [Renaming columns for reading](#renaming-columns-for-reading-with-extra-remap) |
| `filter:` | a **read-in** MAML (`parquet_table`) | keeps only the rows a rule matches — [Filtering and sorting on read](#filtering-and-sorting-on-read-with-extra-filter-and-extra-sort) |
| `sort:` | a **read-in** MAML (`parquet_table`) | returns the rows in a chosen order — same section |

A third role exists and uses no `extra:` key at all: a **table-type** schema under `table_types/`,
whose `source:` field sub-key `tools/generate_user_table_code.py` reads to generate a
`parquet_table` extension — see [Generated table types](../utilities/generated-tables.html).

**Every MAML-declared table-level value is a string.** MAML carries no type for these keys, and that
is a design decision rather than a missing feature: `keyarray:` has no `datatype:` sub-key and is
not going to grow one. The consequence worth knowing is an asymmetry with the runtime API — a
keyword declared in a `.maml` file gets **no** `<KEY>.datatype` companion entry in the written file
however numeric its text looks, while the same keyword added in code through a typed
`schema%add_metadata` call does (see [A typed value records its own
type](building-schema-in-code.html#a-typed-value-records-its-own-type)). So a value that reaches the
file by way of MAML is a string to every reader, and a `write_maml=.true.` sidecar likewise records
no type. If a keyword needs to arrive typed, add it in code.

In short: validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive
for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks. (If you're
contributing to `parquet-fortran` itself and want to extend its MAML structure, see
[CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#extending-the-maml-schema).)
