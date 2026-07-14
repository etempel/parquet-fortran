---
title: The MAML metadata format
---

The library turns a MAML (YAML) metadata file into the VOTable-style header embedded in the `.parquet` file — the flow is:

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

A [MAML](https://github.com/asgr/MAML-Format) file is YAML. Table-level metadata (author, description, ...) is given as top-level keys, and column definitions are given as a list under the `fields:` key. **Only a fixed, known set of top-level keys is accepted** — `survey`, `dataset`, `table`, `version`, `date`, `author`, `coauthors`, `dois`, `depends`, `description`, `comments`, `license`, `keywords`, `maml_version`, `keyarray`, `extra`, and `fields` (matched case-insensitively; see `allowed_maml_sections` in `src/parquet_metadata_sections.f90`) — any other top-level key fails `parquet_validate_maml` as an unknown section. To attach your own custom metadata not covered by that list, nest it under `extra:` instead, which accepts arbitrary structure unvalidated (see [Renaming columns for output with `col_map:`](#renaming-columns-for-output-with-col_map) for an example of `extra:`'s own nested keys). `parquet_parse_maml` always runs this same validation before parsing a MAML into a `parquet_schema`, so an invalid MAML is caught immediately rather than silently parsed. Three full worked examples are checked into the repository under `schemas/`:

- [schemas/maml_example.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example.maml) — the base example used throughout these docs.
- [schemas/maml_example2.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example2.maml) — adds `string` fields and `qc: min:`/`max:` bounds (both the plain-number and the quoted-operator forms).
- [schemas/maml_example3.maml](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/schemas/maml_example3.maml) — adds `extra: col_map:` column renaming (e.g. `id` → `uberid`, `RA` → `ra_J2000`) alongside `qc:`.

If you're new to MAML in this library, focus first on `table:` and `fields:` (`name` + `data_type` for each field). Everything else is optional metadata or advanced behavior.

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

Notes on the `fields:` entries:

- `name` and `data_type` are required for every field; `data_type` must be one of the [supported types](supported-data-types.html)' MAML names.
- `col_size` (default `1`) makes the column a fixed-length vector column, read/written as a 2D array of shape `(col_size, nrows)`.
- `array_size` sets the maximum string length for `string` columns; it is ignored for other types.
  > Don't confuse `col_size` with `array_size` — despite the similar-sounding names, they're unrelated: `col_size` is how many elements a vector column's row holds, `array_size` is how many characters a `string` column's values can hold.
- `unit`, `info` and `ucd` are optional and are carried through into the parquet file's VOTable-style header for that column.
- Run `parquet_validate_maml` on a MAML file to catch structural mistakes (duplicate names, missing `data_type`, missing `table`, unknown top-level sections or sub-keys, etc.) before using it to open a writer. It accepts either a `parquet_maml_file` (e.g. from `parquet_load_maml_file`, or built in memory) or a filename directly (`call parquet_validate_maml("schemas/maml_example2.maml")`, loading it from disk internally).

A second MAML file may be validated against a "base" MAML with `parquet_validate_user_maml`, to check it only reuses column names that already exist in the base schema — useful when different pipeline stages should write a subset of a shared schema.

### Renaming columns for output with `col_map:`

A user MAML's `fields:` names are normally required to match the base schema's column names exactly. `col_map:` relaxes that: it lets a user MAML give a column an arbitrary name of its own choosing for the `fields:` section (and the resulting `.parquet`/sidecar `.maml`), while Fortran code continues to call `parquet_write_column`/`set_column_available`/`get_column_index`/etc. with the stable, well-known internal name from the base schema. `col_map:` is **not** a top-level MAML section — it is only recognized nested inside `extra:` (a bare top-level `col_map:` is rejected as an unknown section):

```yaml
table: user_table
extra:
  col_map:
  - id0: my_id
fields:
- name: my_id       # the user's own chosen name -- can differ freely from id0
  data_type: int32  # still declared in full, exactly like a non-renamed field
```

- Each `col_map:` item is `<internal_name>: <output_name>`. `parquet_validate_user_maml` checks these are unambiguous — each `internal_name` must exist in the base schema and not also appear un-renamed elsewhere in `fields:`; each `output_name` must actually be declared in this MAML's own `fields:` and not collide with another column's name. A violation fails validation with a message naming the specific problem.
- The renamed field's `fields:` entry (`my_id` above) is validated exactly like any other field entry (`data_type` required, etc.) — nothing is inherited from the base column's own attributes.
- After `parquet_parse_maml`, the resulting `parquet_column_type` always uses the internal name (`id0`) for `schema%cinfo%col(:)%name` — the same name every other API (`parquet_write_column`, `set_column_available`, `get_column_index`, ...) already expects — with the rename available separately as `schema%cinfo%col(:)%output_name` (`my_id`), which is what actually gets written to the `.parquet` file's schema/VOTable header and to a `write_maml=.true.` sidecar's `fields:` section.
- `user_maml%col_map` (populated by `parquet_validate_user_maml`) exposes the parsed entries for inspection.
- Since it lives inside `extra:`, `col_map:` does not produce any table-level metadata entry of its own (nor does `protected_cols:`, `extra:`'s other specifically-parsed key — see [Null values](supported-data-types.html#null-values)); anything else nested inside `extra:` is accepted unvalidated and otherwise unused.

Table-level keys become parquet metadata entries, with these special mappings:

| Top-level key | Becomes |
|---|---|
| `keyarray:` | A list of `key`/`value`/`comment` maps; each becomes one metadata entry named by its `key`. |
| `DOIs:` | A list of `DOI`/`type` maps; becomes `DOI_1`, `DOI_2`, ... entries (value = DOI, description = type). |
| `depends:` | A list of `survey`/`dataset`/`table`/`version` maps (for referencing upstream datasets this table was built from); becomes `depends_1`, `depends_2`, ... entries, each value being those four fields joined with `;` in that fixed order (regardless of the order they appear in the file; any missing sub-key becomes an empty segment). |
| `comments:` / `coauthors:` | Plain string lists; become `comment_1`, `comment_2`, ... / `coauthor_1`, `coauthor_2`, ... entries. |
| `keywords:` | A plain-string list, combined into a single `keywords` entry with its items joined by `;`. |
| any other allowed section, given as a plain-string list (e.g. `survey:`, `author:`, `license:`, ...) | Several entries that all share that key's name (e.g. multiple `list_key` entries with the same name). |
| any other allowed section, given as a list of *maps* | **Not** specially handled: only its first sub-key ends up captured as a raw, unparsed string, and the rest of that entry's sub-keys are silently dropped. Use `keyarray:` for arbitrary structured metadata instead. |
| `extra:` | Opaque to table-level metadata (produces no metadata entry of its own), but not ignored: `col_map:` and `protected_cols:` are specifically parsed out of it — see [Renaming columns for output with `col_map:`](#renaming-columns-for-output-with-col_map) and [Null values](supported-data-types.html#null-values). Anything else nested inside `extra:` is accepted unvalidated and otherwise unused. |

In short: validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks. (If you're contributing to `parquet-fortran` itself and want to extend its MAML structure, see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md#extending-the-maml-schema).)
