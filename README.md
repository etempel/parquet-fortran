# Fortran parquet library for WAVES/4HS

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

## Contents

- [Prerequisites](#prerequisites)
- [Building and installing instructions](#building-and-installing-instructions)
- [Generating the built-in MAML module](#generating-the-built-in-maml-module)
- [Reading parquet files from your fortran code](#reading-parquet-files-from-your-fortran-code)
- [Writing parquet files from your fortran code](#writing-parquet-files-from-your-fortran-code)
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
- Run `parquet_validate_maml` on a MAML file to catch structural mistakes (duplicate names, missing `data_type`, missing `table`, etc.) before using it to open a writer.

A second MAML file may be validated against a "base" MAML with `parquet_validate_user_maml`, to check it only reuses column names that already exist in the base schema — useful when different pipeline stages should write a subset of a shared schema.

## Combined example: MAML schema, matrices and metadata

This example ties together MAML-driven column definitions, a vector/matrix column, dropping an optional column at runtime, and adding extra table metadata not present in the MAML file.

```fortran
program write_parquet_combined_example
    use parquet
    use iso_fortran_env, only: int32, real64
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

Matrix (vector-column) entries use the shape convention `(nelem, nrows)` for `parquet_write_column`/`parquet_get_column_total_elements`-derived arrays, and `(col_size, nrows)` for arrays produced by `parquet_read_column`.

## parquet module API (functions/subroutines)

List of public callable procedures available with `use parquet`:

### Utility

- `get_parquet_fortran_version()` — returns the library version as a `character` string, e.g. `"v0.3.1"`.

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

- `metadata%add_metadata(key, value[, fmt])` — attaches a table-level key/value pair (e.g. provenance, units, free-text notes) that is written into the parquet file's VOTable-style header. `value` may be a scalar or 1D array of any [supported type](#supported-data-types) (`integer(int32/int64)`, `real(real32/real64)`, `logical`, `character`). The optional `fmt` (`character`) sets a display/print format string for the value, otherwise a type-appropriate default is used; it has no effect on how the value itself is stored.

### Writer

- `parquet_open_writer(writer, filename[, cinfo, metadata])` — opens `filename` for writing. `cinfo`/`metadata` are optional; see the schema-enforcement note above.
- `parquet_write_column(writer, name, data)` — writes one full column named `name`. `data` may be any [supported type](#supported-data-types), passed as a 1D array (`data(:)`) for a plain column or a 2D array (`data(nelem, nrows)`) for a vector/array column.
- `parquet_close_writer(writer)` — flushes buffered data and finalizes the file. Always call this before the program ends, or the file may be incomplete/unreadable.

### Reader (table and column info)

- `parquet_open_reader(reader, filename)` — opens an existing parquet file for reading.
- `parquet_close_reader(reader)` — releases resources associated with `reader`.
- `parquet_get_nrows(reader, nrows)` — returns the number of table rows in `nrows` (`integer(int32)` or `integer(int64)`).
- `parquet_get_col_size(reader, name, col_size)` — returns the fixed vector length of an array/matrix column `name` in `col_size`. Call this before allocating the output array for `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` on that column.
- `parquet_get_column_total_elements(reader, name, nelem)` — returns the total number of elements in column `name` across all rows (`nelem = col_size * nrows` for array columns), in `nelem` (`integer(int32)` or `integer(int64)`).
- `parquet_get_string_length(reader, name, strlen_max)` — returns the longest string found in string column `name`. Call this before allocating a `character(len=...)` array for `parquet_read_column`, since the allocated length must be at least `strlen_max`.

### Reader (reads column data)

- `parquet_read_column(reader, name, values)` — reads the full column `name` into `values`, which may be any [supported type](#supported-data-types) as a 1D array (`values(nrows)`) for a plain column, or a 2D array (`values(col_size, nrows)`) for a vector/array column. Allocate `values` first, using `parquet_get_nrows`/`parquet_get_col_size`/`parquet_get_string_length` as needed.
- `parquet_read_array_row_mode(reader, name, values, row_index)` — reads only row `row_index` of vector column `name` into the 1D array `values(col_size)`. Use this to fetch one row's vector at a time (e.g. when iterating row-by-row) without loading the whole column.
- `parquet_read_array_element_mode(reader, name, values, col_index)` — reads only element `col_index` of vector column `name`, across all rows, into the 1D array `values(nrows)`. Use this to fetch one vector position across every row (e.g. "the 3rd element of every row's vector") without loading the whole column.

## Troubleshooting

Most build failures come from the Arrow/Parquet C++ dependency not being visible to FPM at compile or link time. See [Environment variables](#prerequisites) above for the full variable list; the following are the most common symptoms:

- **`fatal error: arrow/api.h: No such file or directory`** (or similar for `parquet/api/reader.h`) — `FPM_FFLAGS`/`FPM_CXXFLAGS` is not pointing `-I` at Arrow's `include` directory.
- **Link errors like `undefined reference to arrow::...` or `cannot find -lparquet`** — `LIBRARY_PATH`/`FPM_LDFLAGS` is not pointing `-L` at Arrow's `lib` directory, or the `link = ["arrow", "parquet", "c++"]` entry is missing from the consuming project's `fpm.toml`.
- **Linker errors mentioning `std::span` or other C++20-only symbols** — `-std=c++20` is missing from `FPM_CXXFLAGS`; this is required on every platform since Arrow/Parquet headers use `std::span` unconditionally.
- **Undefined references to `std::__1::...` (macOS) or `std::...` (Linux) at the final link step** — the C++ standard library is missing from `FPM_LDFLAGS`. Add `-lc++` on macOS/Clang or `-lstdc++` on Linux/GCC (see [Environment variables](#prerequisites)).
- **At runtime, `dyld: Library not loaded` / `error while loading shared libraries` for `libarrow`/`libparquet`** — the Arrow/Parquet shared libraries are not on the dynamic linker's search path at run time; add their directory to `DYLD_LIBRARY_PATH` (macOS) or `LD_LIBRARY_PATH` (Linux) in addition to `LIBRARY_PATH` used at build time.
- **Program aborts with an `ERROR STOP` message instead of returning a status code** — this is expected; see [Error handling](#error-handling). The message text (e.g. naming a missing column or file) indicates the failing precondition.
