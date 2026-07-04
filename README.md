# Fortran parquet library for WAVES/4HS

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

## Contents

- [Prerequisites](#prerequisites)
- [Building and installing instructions](#building-and-installing-instructions)
  - [Running the error-path tests](#running-the-error-path-tests)
- [Generating the built-in MAML module](#generating-the-built-in-maml-module)
- [Reading parquet files from your fortran code](#reading-parquet-files-from-your-fortran-code)
- [Writing parquet files from your fortran code](#writing-parquet-files-from-your-fortran-code)
  - [Saving the source MAML alongside the parquet file](#saving-the-source-maml-alongside-the-parquet-file)
- [The MAML metadata format](#the-maml-metadata-format)
- [Combined example: MAML schema, matrices and metadata](#combined-example-maml-schema-matrices-and-metadata)
- [Error handling](#error-handling)
- [Supported data types](#supported-data-types)
- [parquet module API (functions/subroutines)](#parquet-module-api-functionssubroutines)
- [Troubleshooting](#troubleshooting)

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2025.3.0
    - Gfortran v15.2.0
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
- [apache-arrow](https://arrow.apache.org) (C++ library for parquet)

Unit testing is handled using test-drive, which is automatically installed by FPM.

### Environment variables

To build the code with intel compiles, the following environment variables should be set:

- LIBRARY_PATH should point to the parquet and arrow library.
- FPM_FFLAGS should point to arrows include directory
- FPM_CXXFLAGS should add relevant C++ flags
- FPM_LDFLAGS should point to arrow and parquet library
- FPM_FC can be used to set fortran compiler for FPM (e.g. FPM_FC=ifx)

In bash you can initialise them as follows:

    export LIBRARY_PATH=path_arrow/lib:$LIBRARY_PATH
    export FPM_FFLAGS="-Ipath_arrow/include"
    export FPM_CXXFLAGS="-std=c++20 -stdlib=libc++ -Ipath_arrow/include" # macOS
    export FPM_CXXFLAGS="-std=c++20"
    export FPM_LDFLAGS="-Lpath_arrow/lib"
    export FPM_FC=ifx

NB! `-std=c++20` is required on every platform (Arrow/Parquet headers use `std::span` unconditionally).
`-stdlib=libc++` is macOS/Clang-specific and should be dropped on Linux.

The build also links a C++ standard library at the final link step, since the executables/tests are
linked by the Fortran compiler driver. This library is platform/toolchain-specific and must be added
to FPM_LDFLAGS:

- macOS (Clang/libc++): `export FPM_LDFLAGS="-Lpath_arrow/lib -lc++"`
- Linux (GCC/libstdc++): `export FPM_LDFLAGS="-Lpath_arrow/lib -lstdc++"`

NB! It might depend on the operating system and fortran compiler what environment variables are needed.

## Building and installing instructions

To test the code:

    fpm test

To clean the build directory:

    fpm clean
    
To clean the build directory and all dependencies:

    fpm clean --all

To generate the executable:

    fpm install --prefix my_path

The executable is placed in the my_path/bin directory. Executable only prints the parquet-fortran library version number.

### Running the error-path tests

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, etc.) are reported via Fortran's `error stop`, which aborts the whole process — see [Error handling](#error-handling). Since test-drive assertions can't survive an `error stop` in the same process, these paths are exercised out-of-process by a small helper program, `test/error_scenarios.f90`, which is built as its own `fpm` test target named `error_scenarios`.

`error_scenarios` takes a single scenario name as a command-line argument and deliberately triggers the corresponding failure:

```bash
fpm test error_scenarios -- write_undeclared_column
```

Expected output for a failing scenario is an `ERROR STOP` message naming the violated precondition, followed by a Fortran backtrace, and a nonzero process exit code:

```
ERROR STOP parquet_write_column: column not defined in parquet_open_writer: not_a_real_column

Error termination. Backtrace:
...
```

Running it with the `ok` scenario (or no argument at all) does not trigger any failure and exits with status 0:

```bash
fpm test error_scenarios -- ok
```

The full list of scenario names is in the `select case` at the top of `test/error_scenarios.f90`. `test/test_errors.f90` drives every scenario automatically (as part of the `errors` test suite in `fpm test`) and asserts on the exit code; `tools/run_error_scenarios.sh` does the same thing standalone, without going through test-drive:

```bash
tools/run_error_scenarios.sh
```

This prints a `[PASS]`/`[FAIL]` line per scenario and exits nonzero if any scenario's exit code didn't match what was expected — useful for a quick manual check or a CI step that doesn't need the full `fpm test` output.

## Generating the built-in MAML module

Every `.maml` file under `docs/` can be compiled directly into the library, so a default schema is available without shipping or locating a `.maml` file at run time. This is done by `tools/generate_parquet_maml.sh`, which scans `docs/*.maml` and writes a Fortran module embedding each file's contents as a string array, keyed by filename.

Run it from the repository root:

```bash
tools/generate_parquet_maml.sh base   # (re)generates src/parquet_maml_base.f90 (for parquet-fortran)
tools/generate_parquet_maml.sh        # (re)generates src/parquet_maml.f90 (for external codes)
```

- `base` mode generates `parquet_maml_base`, which additionally defines the `parquet_maml_file`/`parquet_maml_missing_column` derived types used throughout the library. Run this first, or whenever those types change.
- The default (no-argument) mode generates `parquet_maml`, which exposes `get_parquet_maml(name)` (looks up an embedded MAML by its `docs/`-relative path or bare filename stem, e.g. `"maml_example"` or `"maml_example.maml"`) and `set_maml(maml_default, [maml_file])` (returns the embedded default, or — if `maml_file` is given — loads and validates a user-supplied MAML file against that default via `parquet_validate_user_maml`).

Both generated files carry a header stating they are auto-generated — do not hand-edit `src/parquet_maml_base.f90` or `src/parquet_maml.f90`; instead edit the source `.maml` files under `docs/` and re-run the script.

## Reading parquet files from your fortran code

To use this library in another Fortran project, add it as an FPM dependency in your project's `fpm.toml`:

```toml
[dependencies]
parquet-fortran = { path = "/path/to/parquet-fortran" }
# or provide a relative path to the parquet-fortran git repository, e.g.
parquet-fortran.git = "../parquet-fortran"

[build]
link = ["arrow", "parquet", "c++"]
```

Then `use parquet` in your code.

Minimal reader example:

```fortran
program read_parquet_example
    use parquet
    use iso_fortran_env, only: int32, int64
    implicit none

    type(parquet_reader) :: reader
    integer(int64) :: nrows
    integer(int32), allocatable :: id(:)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_get_nrows(reader, nrows)

    allocate(id(nrows))
    call parquet_read_column(reader, "id", id)

    call parquet_close_reader(reader)
end program read_parquet_example
```

Notes:

- `parquet_get_nrows` returns the number of table rows.
- Allocate output arrays before calling `parquet_read_column`.
- For string columns, choose a fixed string length that is large enough for your data.
- For vector/array columns, allocate 2D arrays with shape `(col_size, nrows)`.

## Writing parquet files from your fortran code

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

If you want explicit column definitions and table metadata, first parse a MAML file and pass `cinfo` and `metadata` to `parquet_open_writer`.

```fortran
type(parquet_column_info) :: cinfo
type(parquet_table_metadata) :: metadata

call parquet_read_maml("maml_example.maml", cinfo, metadata)
call parquet_open_writer(writer, "data.parquet", cinfo, metadata)
```

If `cinfo` is omitted, `parquet_open_writer` does not enforce a fixed schema: each column's type, string length and array size are inferred from the first `parquet_write_column` call that writes it. If `cinfo` is given, only columns marked `is_set = .true.` (see `set_available`/`set_unavailable` below) are written, and calling `parquet_write_column` with a name that is not in `cinfo` stops the program with an error.

### Saving the source MAML alongside the parquet file

Pass `write_maml=.true.` to `parquet_open_writer` to also save a sidecar `.maml` file next to the parquet output — same path, with a trailing `.parquet` replaced by `.maml` (or `.maml` appended if there is none):

```fortran
call parquet_read_maml("maml_example.maml", cinfo, metadata)
call parquet_open_writer(writer, "data.parquet", cinfo, metadata, write_maml=.true.)
! writes data.parquet and data.maml
```

This requires `metadata` to come from `parquet_read_maml` — it saves the verbatim MAML source that was parsed into `metadata`, not the parquet file's parquet/VOTable-style header. Two things follow from that:

- Calls to `metadata%add_metadata` made *after* `parquet_read_maml` (to add extra runtime metadata, as in the [combined example](#combined-example-maml-schema-matrices-and-metadata) below) each append a new `keyarray:` entry (`key`/`value`/`comment`) to the saved `.maml`, so runtime metadata is reflected in the sidecar too. Entries are always appended, even if a `keyarray:` entry with the same key already exists — the sidecar will then contain both. A `keyarray:` header is added automatically if the source MAML didn't have one, placed before `extra:` if present, else before `fields:`.
- The saved `.maml`'s `fields:` section only lists columns that are enabled (`cinfo%col(:)%is_set`) at the time `parquet_open_writer` is called, i.e. what actually ends up in the `.parquet` file: entries for columns disabled via `set_unavailable`, or excluded from a user MAML subset via `parquet_validate_user_maml`, are removed from the sidecar. Matching is by column name against the `cinfo` passed to `parquet_open_writer`; every other section (table-level metadata, `keyarray:`, etc.) is left untouched. If disabling columns would leave zero fields, pruning is skipped entirely and the full field list is kept instead, since a MAML file with no fields cannot be read back by `parquet_validate_maml`.

Omitting `write_maml`, or passing `write_maml=.false.`, behaves exactly as before (no sidecar file). Passing `write_maml=.true.` without `metadata`, or with `metadata` that wasn't produced by `parquet_read_maml`, stops the program with an error.

Notes:

- Every call to `parquet_write_column` writes one full column.
- All columns in one file must contain the same number of rows.
- Close the writer with `parquet_close_writer` to flush data and finalize the file.

## The MAML metadata format

A [MAML](https://github.com/asgr/MAML-Format) file is YAML. Table-level metadata (author, description, arbitrary key/value pairs, ...) is given as top-level keys, and column definitions are given as a list under the `fields:` key. A full worked example is checked into the repository at [docs/maml_example.maml](docs/maml_example.maml); an abridged version:

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

- `name` and `data_type` are required for every field; `data_type` must be one of the [supported types](#supported-data-types)' MAML names.
- `col_size` (default `1`) makes the column a fixed-length vector column, read/written as a matrix of shape `(col_size, nrows)`.
- `array_size` sets the maximum string length for `string` columns; it is ignored for other types.
- `unit`, `info` and `ucd` are optional and are carried through into the parquet file's VOTable-style header for that column.
- Run `parquet_validate_maml` on a MAML file to catch structural mistakes (duplicate names, missing `data_type`, missing `table`, unknown top-level sections or sub-keys, etc.) before using it to open a writer. It accepts either a `parquet_maml_file` (e.g. from `parquet_load_maml_file`, or built in memory) or a filename directly (`call parquet_validate_maml("docs/maml_example2.maml")`, loading it from disk internally).

A second MAML file may be validated against a "base" MAML with `parquet_validate_user_maml`, to check it only reuses column names that already exist in the base schema — useful when different pipeline stages should write a subset of a shared schema.

### Renaming columns for output with `col_map:`

A user MAML's `fields:` names are normally required to match the base schema's column names exactly. `col_map:` relaxes that: it lets a user MAML give a column an arbitrary name of its own choosing for the `fields:` section (and the resulting `.parquet`/sidecar `.maml`), while Fortran code continues to call `parquet_write_column`/`set_available`/`get_column_index`/etc. with the stable, well-known internal name from the base schema. `col_map:` is **not** a top-level MAML section — it is only recognized nested inside `extra:` (a bare top-level `col_map:` is rejected as an unknown section):

```yaml
table: user_table
extra:
  col_map:
  - id0: my_id
fields:
- name: my_id       # the user's own chosen name -- can differ freely from id0
  data_type: int32  # still declared in full, exactly like a non-renamed field
```

- Each `col_map:` item is `<internal_name>: <output_name>`; `parquet_validate_user_maml` checks that:
  - `internal_name` actually exists in the base schema;
  - no two items share the same `internal_name`, and no two items collide on the same `output_name`;
  - `output_name` is actually declared as a `name:` in this MAML's own `fields:` — otherwise the rename has nothing to apply to;
  - `internal_name` does not *also* appear directly (un-renamed) in `fields:` — ambiguous: renamed, or used as-is?
  - `output_name` does not coincide with a *different* existing base column's own name — otherwise that other column would collide with the renamed one the moment it's activated (e.g. via `set_available`), since both would then share the same output name in the written schema.
- The renamed field's `fields:` entry (`my_id` above) is validated exactly like any other field entry (`data_type` required, etc.) — nothing is inherited from the base column's own attributes.
- After `parquet_read_maml`, the resulting `parquet_column_type` always uses the internal name (`id0`) for `cinfo%col(:)%name` — the same name every other API (`parquet_write_column`, `set_available`, `get_column_index`, ...) already expects — with the rename available separately as `cinfo%col(:)%output_name` (`my_id`), which is what actually gets written to the `.parquet` file's schema/VOTable header and to a `write_maml=.true.` sidecar's `fields:` section.
- `user_maml%col_map` (populated by `parquet_validate_user_maml`) exposes the parsed entries for inspection.
- Since it lives inside `extra:`, `col_map:` does not produce any table-level metadata entry of its own — `extra:`'s content (including `col_map:`) is otherwise entirely opaque/discarded, just like the rest of `extra:`.

Table-level top-level keys become one metadata entry each, with a few special cases:

- `keyarray:` — a list of `key`/`value`/`comment` maps, each becomes one metadata entry named by its `key`.
- `DOIs:` — a list of `DOI`/`type` maps, becomes `DOI_1`, `DOI_2`, ... entries (value = DOI, description = type).
- `depends:` — a list of `survey`/`dataset`/`table`/`version` maps (for referencing upstream datasets this table was built from), becomes `depends_1`, `depends_2`, ... entries, each value being those four fields joined with `;` in that fixed order (regardless of the order they appear in the file; any missing sub-key becomes an empty segment).
- `comments:`/`coauthors:` — plain string lists, become `comment_1`, `comment_2`, ... / `coauthor_1`, `coauthor_2`, ... entries.
- `keywords:` — a plain-string list, combined into a single `keywords` entry with its items joined by `;`.
- Any other list of plain strings becomes several entries that all share that key's name (e.g. multiple `list_key` entries with the same name).
- Any other list of *maps* (not one of the above) is **not** specially handled: only its first sub-key ends up captured as a raw, unparsed string, and the rest of that entry's sub-keys are silently dropped. Use `keyarray:` for arbitrary structured metadata instead.
- `extra:` is reserved for future use and is currently ignored entirely (not even partially captured).

`parquet_validate_maml` also checks that the MAML file only uses known sections: every top-level section name, and every sub-key one level inside a map-list section's items (e.g. `name:`/`data_type:`/... inside a `fields:` entry, or `key:`/`value:`/`comment:` inside a `keyarray:` entry), must be declared in the schema at the top of [src/parquet_metadata.f90](src/parquet_metadata.f90) (`allowed_maml_sections`). This checks presence only, not values. To allow a new top-level section, or a new sub-key within an existing map-list section, add an entry there.

One level deeper still (e.g. a `fields:` entry's own `qc:` sub-block, with its `min:`/`max:`/`miss:` keys) is validated too, but only where a matching entry exists in the separate `allowed_maml_nested_sections` schema (also in `src/parquet_metadata.f90`) — add an entry there for any other sub-key that itself has structured children needing validation. `extra:` is the sole exception to all of this: its entire internal structure (however deeply nested) is accepted unvalidated. Anything nested deeper than `allowed_maml_nested_sections` covers is likewise left unvalidated.

## Combined example: MAML schema, matrices and metadata

This example ties together MAML-driven column definitions, a vector/matrix column, dropping an optional column at runtime, and adding extra table metadata not present in the MAML file.

```fortran
program write_parquet_combined_example
    use parquet
    use iso_fortran_env, only: int32, int64, real64
    implicit none

    type(parquet_writer) :: writer
    type(parquet_column_info) :: cinfo
    type(parquet_table_metadata) :: metadata
    integer(int32) :: id0(3)
    integer(int64) :: idarr(2, 3)   ! (col_size, nrows) for the "idarr" vector column

    ! Parse column definitions + table metadata from the MAML file.
    call parquet_read_maml("docs/maml_example.maml", cinfo, metadata)

    ! This schema defines more columns than we have data for in this example;
    ! disable everything, then re-enable only the columns we are about to write.
    call cinfo%set_unavailable()
    call cinfo%set_available("id0")
    call cinfo%set_available("idarr")

    ! Add an extra, run-time-only piece of metadata not present in the MAML file.
    call metadata%add_metadata("generated_by", "write_parquet_combined_example")

    id0 = [1_int32, 2_int32, 3_int32]
    idarr = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])

    call parquet_open_writer(writer, "data.parquet", cinfo, metadata)
    call parquet_write_column(writer, "id0", id0)
    call parquet_write_column(writer, "idarr", idarr)
    call parquet_close_writer(writer)
end program write_parquet_combined_example
```

## Error handling

This library reports all failures (missing files, invalid MAML, unknown column names, type mismatches, etc.) by calling Fortran's `error stop`, which aborts the running program immediately and cannot be caught or recovered from. There are no status/`ierr` return codes — check inputs (file existence, column names, array bounds) before calling into the library if you need to avoid aborting.

## Supported data types

The following intrinsic Fortran kinds (from `iso_fortran_env`) are supported throughout the write, read and metadata APIs, both as scalars/1D arrays and as 2D matrices:

| Fortran kind         | MAML `data_type` |
|-----------------------|-------------------|
| `integer(int32)`      | `int32`           |
| `integer(int64)`      | `int64`           |
| `real(real32)`        | `float32`         |
| `real(real64)`        | `float64`         |
| `logical`             | `boolean`         |
| `character(len=*)`    | `string`          |

Matrix (vector-column) entries use the shape convention `(col_size, nrows)` for arrays passed to `parquet_write_column` or produced by `parquet_read_column`.

### Null values

Fortran has no per-element representation for a missing/Null value. On the **read** side, if a column contains any genuine Parquet Null (e.g. a file produced by another tool), the default behavior of `parquet_read_column`, `parquet_read_array_row_mode`, and `parquet_read_array_element_mode` is to error out immediately, rather than silently returning undefined data.

To read a Null-containing column instead of erroring, pass one or both of these optional keyword arguments (supported by all three of the read families above, for every data type):

- `null_value=nullval` — a scalar of the same type as `values`; every Null in the column is replaced with `nullval` in the returned `values`.
- `is_valid=mask` — a `logical` array of the same shape as `values`; `.false.` wherever the Parquet value was Null, `.true.` otherwise.

If only `is_valid` is given (no `null_value`), Null slots in `values` are still filled with a safe type-appropriate default (`0` / `.false.` / blank string) rather than left as undefined data — check `is_valid` to know which entries are real. If both are given, Nulls are replaced with `nullval` *and* `is_valid` still reports which entries were originally Null. Omitting both keeps the default strict (error-on-Null) behavior.

For array/matrix columns, a slot is reported/treated as Null if either the whole row is missing or that specific element within the row is missing (Parquet's list columns track these independently); `is_valid(j, i)` reflects the combination of both.

`parquet_get_string_length` is unaffected by any of this: it always silently skips Nulls when computing the maximum string length, since sizing an output buffer shouldn't depend on how you plan to handle Nulls when reading.

On the **write** side, `parquet_write_column(writer, name, values, is_valid=mask)` accepts the same kind of `logical` mask (same shape as `values`, `.false.` = write a genuine Null there); there is no `null_value` on the write side, since a value used to *detect* a Null (rather than substitute one, as on read) would risk misclassifying a legitimate value that happens to equal the sentinel. Whatever is in `values` at a `.false.` slot is ignored — a real Parquet Null is written there regardless. For array/matrix columns, `is_valid` is element-level only: an entire row's vector can never be Null, only individual elements within it. A column only becomes nullable in the file's schema if `is_valid` is actually passed and contains at least one `.false.` entry; omitting `is_valid` (or passing an all-`.true.` mask) writes exactly as before, keeping the column non-nullable.

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
Every name listed must be one of this same MAML file's own declared `fields:` (checked by `parquet_validate_maml`; an unknown name errors out). If a user MAML overrides a base MAML, `protected_cols:` is taken from whichever MAML is actually used to build the writer's schema (the user MAML if one is provided, otherwise the base MAML) — not merged across both. Writing an `is_valid` mask with any `.false.` entry for a protected column errors out immediately. This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., cinfo, ...)`); a schema-less writer has no `protected_cols:` to enforce.

## parquet module API (functions/subroutines)

List of public callable procedures available with `use parquet`:

### Utility

- `parquet_get_version()` — returns the library version as a `character` string, e.g. `"v0.3.1"`.

### MAML and metadata

- `parquet_read_maml(maml_filename_or_maml, cinfo, metadata)` — parses a MAML source into `cinfo` (column definitions) and `metadata` (table-level key/value metadata). Two overloads are provided: pass a filename (`character`) to read and parse a `.maml` file directly, or pass an already-loaded `parquet_maml_file` object (see `parquet_load_maml_file`) to parse MAML content you already hold in memory.
- `parquet_load_maml_file(filename)` — reads a `.maml` file from disk and returns it as a `parquet_maml_file` object, without parsing it into `cinfo`/`metadata`. Useful when you want to hold on to the raw MAML content (e.g. to pass to `parquet_read_maml` later, or inspect `maml%name`/`maml%lines` directly).
- `parquet_validate_user_maml(base_maml, user_maml)` — checks that every column declared in `user_maml`'s `fields:` block also exists in `base_maml`'s `fields:` block (by name only). `user_maml` may omit any columns from `base_maml`, but must not declare any that aren't there. Errors out, naming the offending column(s), if it does.
- `parquet_validate_maml(maml)` — validates a single MAML file on its own. Checks that: at least one field is defined; every field has a non-empty `name`; every field's `data_type` is one of the recognized types (see [Supported data types](#supported-data-types) above; see `valid_maml_data_types` in `src/parquet_metadata.f90` to add more); no two fields share the same `name`; and the file's metadata includes a non-empty `table` entry. Collects and reports all violations together in a single error stop.

`cinfo` is of type `parquet_column_info`, a scalar wrapper holding the array of parsed columns in `cinfo%col(:)` (each element of type `parquet_column_type`, with fields such as `name`, `unit`, `info`, `ucd`, `data_type`, `array_size`, `col_size`, `is_set`). It provides:

- `cinfo%get_column_index(name)` — returns the index of the named column in `cinfo%col(:)`. Errors out if no column with that name exists.
- `cinfo%set_unavailable([name])` — marks the named column as not set (`is_set = .false.`), so it is skipped when the schema is written. Errors out if no column with that name exists. If `name` is omitted, marks every column unavailable at once. Use this to drop columns from a base MAML schema without writing a separate MAML file for each combination of active columns.
- `cinfo%set_available([name])` — the inverse of `set_unavailable`: marks the named column (or, if `name` is omitted, every column) as set (`is_set = .true.`).

The public derived type `parquet_table_metadata` provides:

- `metadata%add_metadata(key, value[, description][, fmt])` — attaches a table-level key/value pair (e.g. provenance, units, free-text notes) that is written into the parquet file's VOTable-style header. `value` may be a scalar or 1D array of any [supported type](#supported-data-types) (`integer(int32/int64)`, `real(real32/real64)`, `logical`, `character`). The optional `description` (`character`) is a free-text note stored alongside the value. The optional `fmt` (`character`) sets a display/print format string for the value, otherwise a type-appropriate default is used; it has no effect on how the value itself is stored.

### Writer

- `parquet_open_writer(writer, filename[, cinfo, metadata, write_maml])` — opens `filename` for writing. `cinfo`/`metadata` are optional; see the schema-enforcement note above. `write_maml` (default `.false.`) additionally saves a sidecar `.maml` file next to `filename` with the MAML source that produced `metadata`; see [Saving the source MAML alongside the parquet file](#saving-the-source-maml-alongside-the-parquet-file).
- `parquet_write_column(writer, name, values)` — writes one full column named `name`. `values` may be any [supported type](#supported-data-types), passed as a 1D array (`values(:)`) for a plain column or a 2D array (`values(col_size, nrows)`) for a vector/array column.
- `parquet_close_writer(writer)` — flushes buffered data and finalizes the file. Always call this before the program ends, or the file may be incomplete/unreadable.

### Reader (table and column info)

- `parquet_open_reader(reader, filename)` — opens an existing parquet file for reading.
- `parquet_close_reader(reader)` — releases resources associated with `reader`.
- `parquet_get_nrows(reader, nrows)` — returns the number of table rows in `nrows` (`integer(int32)` or `integer(int64)`).
- `parquet_get_col_size(reader, name, col_size)` — returns the fixed vector length of an array/matrix column `name` in `col_size`. Call this before allocating the output array for `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` on that column.
- `parquet_get_column_total_elements(reader, name, total_elements)` — returns the total number of elements in column `name` across all rows (`total_elements = col_size * nrows` for array columns), in `total_elements` (`integer(int32)` or `integer(int64)`).
- `parquet_get_string_length(reader, name, max_string_length)` — returns the longest string found in string column `name`. Call this before allocating a `character(len=...)` array for `parquet_read_column`, since the allocated length must be at least `max_string_length`.

### Reader (reads column data)

- `parquet_read_column(reader, name, values)` — reads the full column `name` into `values`, which may be any [supported type](#supported-data-types) as a 1D array (`values(nrows)`) for a plain column, or a 2D array (`values(col_size, nrows)`) for a vector/array column. Allocate `values` first, using `parquet_get_nrows`/`parquet_get_col_size`/`parquet_get_string_length` as needed.
- `parquet_read_array_row_mode(reader, name, values, row_index)` — reads only row `row_index` of vector column `name` into the 1D array `values(col_size)`. Use this to fetch one row's vector at a time (e.g. when iterating row-by-row) without loading the whole column.
- `parquet_read_array_element_mode(reader, name, values, elem_index)` — reads only element `elem_index` of vector column `name`, across all rows, into the 1D array `values(nrows)`. Use this to fetch one vector position across every row (e.g. "the 3rd element of every row's vector") without loading the whole column.

## Troubleshooting

Most build failures come from the Arrow/Parquet C++ dependency not being visible to FPM at compile or link time. See [Environment variables](#prerequisites) above for the full variable list; the following are the most common symptoms:

- **`fatal error: arrow/api.h: No such file or directory`** (or similar for `parquet/api/reader.h`) — `FPM_FFLAGS`/`FPM_CXXFLAGS` is not pointing `-I` at Arrow's `include` directory.
- **Link errors like `undefined reference to arrow::...` or `cannot find -lparquet`** — `LIBRARY_PATH`/`FPM_LDFLAGS` is not pointing `-L` at Arrow's `lib` directory, or the `link = ["arrow", "parquet", "c++"]` entry is missing from the consuming project's `fpm.toml`.
- **Linker errors mentioning `std::span` or other C++20-only symbols** — `-std=c++20` is missing from `FPM_CXXFLAGS`; this is required on every platform since Arrow/Parquet headers use `std::span` unconditionally.
- **Undefined references to `std::__1::...` (macOS) or `std::...` (Linux) at the final link step** — the C++ standard library is missing from `FPM_LDFLAGS`. Add `-lc++` on macOS/Clang or `-lstdc++` on Linux/GCC (see [Environment variables](#prerequisites)).
- **At runtime, `dyld: Library not loaded` / `error while loading shared libraries` for `libarrow`/`libparquet`** — the Arrow/Parquet shared libraries are not on the dynamic linker's search path at run time; add their directory to `DYLD_LIBRARY_PATH` (macOS) or `LD_LIBRARY_PATH` (Linux) in addition to `LIBRARY_PATH` used at build time.
- **Program aborts with an `ERROR STOP` message instead of returning a status code** — this is expected; see [Error handling](#error-handling). The message text (e.g. naming a missing column or file) indicates the failing precondition.
