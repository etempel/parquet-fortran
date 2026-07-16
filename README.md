# parquet-fortran

[![CI results](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/pipeline.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![Test coverage](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/coverage.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![API documentation](https://gitlab.4most.eu/ole/docserver/-/raw/master/API-documentation-blue.svg)](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran-734f96.svg)](https://fortran-lang.org)
[![fpm](https://img.shields.io/badge/fpm-package-729FCF.svg)](https://fpm.fortran-lang.org/)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/LICENSE)

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

**Features:**

- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character` (MAML: `boolean`/`string`) — as plain 1D columns or fixed-length vector columns (a vector column is read back whether it was stored on disk as a `fixed_size_list` or a variable-length `list`).
- Define and validate a table's schema and metadata from a [MAML](https://github.com/asgr/MAML-Format) file, including column renaming (`col_map:`), quality-control range checks (`qc:`), and protecting specific columns from ever containing a Null (`protected_cols:`). This is relevant only when *writing* — reading a parquet file never involves MAML.
- Read and write genuine Parquet Null values, with either substitution (`null_value=`) or a validity mask (`is_valid=`).
- Control output compression codec, compression level, and row group size.
- Safe to use concurrently (e.g. from OpenMP) — see [Thread safety](doc/pages/thread-safety.md) for the exact rule.

> 📖 **Full user guide:** [doc/pages/](doc/pages/index.md) — reading, writing, the MAML metadata format, thread safety, performance, and troubleshooting. The complete per-procedure API reference is generated from source via [FORD](https://forddocs.readthedocs.io/) — see [Contributing](#contributing). This page is the quick-start overview.

## Contents

- [Quick example](#quick-example)
- [Minimal setup to depend on this library](#minimal-setup-to-depend-on-this-library)
- [Important behavior](#important-behavior)
- [Prerequisites](#prerequisites)
  - [Environment variables](#environment-variables)
- [API overview](#api-overview)
- [Limitations](#limitations)
- [Contributing](#contributing)
- [License](#license)

## Quick example

```fortran
program quick_example
    use parquet
    use iso_fortran_env, only: int32, int64
    implicit none

    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    integer(int32) :: id(3) = [1, 2, 3]
    integer(int32), allocatable :: id_read(:)
    integer(int64) :: nrows

    ! Write.
    call parquet_open_writer(writer, "data.parquet")
    call parquet_write_column(writer, "id", id)
    call parquet_close_writer(writer)

    ! Read back.
    call parquet_open_reader(reader, "data.parquet")
    call parquet_get_nrows(reader, nrows)
    allocate(id_read(nrows))
    call parquet_read_column(reader, "id", id_read)
    call parquet_close_reader(reader)
end program quick_example
```
See [Reading parquet files](doc/pages/reading.md) and [Writing parquet files](doc/pages/writing.md) in the user guide for the full picture, including MAML-driven schemas, vector columns, and the features listed above. You'll need Arrow/Parquet available and a couple of environment variables set to actually build against this library — see [Prerequisites](#prerequisites).

## Minimal setup to depend on this library

Quickstart outline — see [Prerequisites](#prerequisites) and [Environment variables](#environment-variables) for the authoritative, per-platform details:

1. Install the Arrow/Parquet C++ library (e.g. macOS Homebrew: `brew install apache-arrow`).
2. Set `LIBRARY_PATH`, `FPM_FFLAGS`, `FPM_CXXFLAGS`, `FPM_LDFLAGS` to point at Arrow's `include`/`lib` — see [Environment variables](#environment-variables).
3. Add `parquet-fortran` to your own `fpm.toml`, with `link = ["arrow", "parquet", "c++"]`:

```toml
[dependencies]
parquet-fortran = { path = "/path/to/parquet-fortran" }
# or provide a relative path to the parquet-fortran git repository, e.g.
parquet-fortran.git = "../parquet-fortran"

[build]
link = ["arrow", "parquet", "c++"]
```

4. Build/test your project with `fpm test`.

> **On the link list:** your project lists `link = ["arrow", "parquet", "c++"]`, whereas `parquet-fortran`'s own `fpm.toml` lists `["arrow", "arrow_compute", "parquet"]`. The two differ intentionally — `arrow_compute` (used for read-side statistics and qc min/max) is propagated to you automatically by fpm, and the C++ runtime (`-lc++`/`-lstdc++`) comes in through `FPM_LDFLAGS` rather than the `link` list. See [Environment variables](#environment-variables) and the guide's [Troubleshooting](doc/pages/troubleshooting.md).

## Important behavior

- Most failures are reported via Fortran `error stop` and abort the process immediately. There are no status/`ierr` return codes in the public API. Some lower-level Arrow/Parquet failures may abort via C++ rather than `error stop`.
- Reading and writing a *scalar* column (`col_size = 1`) with more than 2,147,483,647 (2^31-1, Fortran's default-integer `huge(1)`) rows is fully supported for every data type — including addressing an individual row past that count via `parquet_read_array_row_mode`. A *vector* column (`col_size > 1`) is capped at that same limit for its own per-row width (`col_size`); its total element count (`nrows * col_size`) has no such cap — row-group sizing handles Parquet's per-row-group element-count ceiling automatically; see [Limitations](#limitations).

See [Error handling](doc/pages/error-handling.md) and [Limitations](#limitations) for full details.

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2025.3.0
    - Gfortran v15.2.0 (development), v13 (CI)
    - **Minimum gfortran: 13.** Older versions (e.g. Ubuntu 22.04's default compiler)
      miscompile part of the schema-building API — see
      [Troubleshooting](doc/pages/troubleshooting.md) if you hit a spurious
      "column not found" abort.
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
- A C++20-capable C++ compiler (Arrow/Parquet headers use `std::span`
  unconditionally): e.g. GCC ≥ 11 / a recent Clang. `-std=c++20` must be set
  (see [Environment variables](#environment-variables)).
- [apache-arrow](https://arrow.apache.org) (C++ library for parquet) v24.0.0

Installing the Arrow/Parquet C++ library itself (not a Fortran package, so it isn't installed by FPM):

- macOS (Homebrew): `brew install apache-arrow`
- macOS (MacPorts): `sudo port install apache-arrow`
- Debian/Ubuntu: follow [Arrow's official apt repository instructions](https://arrow.apache.org/install/) and install `libarrow-dev`, `libarrow-compute-dev` and `libparquet-dev` (the separate `libarrow-compute-dev` provides the `arrow_compute` library that `fpm.toml` links — see [Troubleshooting](doc/pages/troubleshooting.md))
- conda-forge: `conda install -c conda-forge libarrow libparquet`

Whichever route you use, take note of the resulting `include`/`lib` directories — they're what the environment variables below need to point at.

Unit testing is handled using test-drive, which is automatically installed by FPM.

### Environment variables

To build with Intel Fortran (or any supported compiler), set these variables so FPM can find Arrow/Parquet:

- LIBRARY_PATH should point to the parquet and arrow library.
- FPM_FFLAGS should point to arrows include directory
- FPM_CXXFLAGS should add relevant C++ flags
- FPM_LDFLAGS should point to arrow and parquet library
- FPM_FC can be used to set fortran compiler for FPM (e.g. FPM_FC=ifx)

In bash, initialize them as follows (replace `path_arrow` with your install root):

macOS (Clang/libc++):

```bash
export LIBRARY_PATH=path_arrow/lib:$LIBRARY_PATH
export FPM_FFLAGS="-Ipath_arrow/include"
export FPM_CXXFLAGS="-std=c++20 -stdlib=libc++ -Ipath_arrow/include"
export FPM_LDFLAGS="-Lpath_arrow/lib -lc++"
export FPM_FC=ifx
```

Linux (GCC/libstdc++):

```bash
export LIBRARY_PATH=path_arrow/lib:$LIBRARY_PATH
export FPM_FFLAGS="-Ipath_arrow/include"
export FPM_CXXFLAGS="-std=c++20 -Ipath_arrow/include"
export FPM_LDFLAGS="-Lpath_arrow/lib -lstdc++"
export FPM_FC=ifx
```

Note: `-std=c++20` is required on every platform (Arrow/Parquet headers use `std::span` unconditionally).
`-stdlib=libc++` is macOS/Clang-specific and should be dropped on Linux.

Note: the exact variable set can vary by operating system and compiler toolchain.

For genuine multi-threaded (OpenMP) use, your compiler's OpenMP flag must also be supplied via `FPM_FFLAGS` — see [Thread safety](doc/pages/thread-safety.md) for the per-compiler flags and why.

To build/test this repository itself (as opposed to depending on it from your own project), see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md).

Hitting a build or link error? See [Troubleshooting](doc/pages/troubleshooting.md) in the user guide for the common symptoms and their fixes.

## API overview

A quick index of the public `use parquet` API. For the full per-procedure reference, generate the API docs locally with FORD (see [Contributing](#contributing)); for usage, see the linked how-to sections in the [user guide](doc/pages/index.md).

**Types:** `parquet_writer`, `parquet_reader`, `parquet_schema`, `parquet_filter`, `parquet_column_info`, `parquet_column_type`, `parquet_table_metadata`, `parquet_maml_file`, `parquet_string_column`

**Utility:** `parquet_get_version`, `parquet_set_max_threads`

**MAML and metadata:** `parquet_parse_maml`, `parquet_load_maml_file`, `parquet_load_qc_maml_file`, `parquet_validate_maml`, `parquet_validate_user_maml` — plus the `parquet_schema` type-bound builders `schema%init`, `schema%add_field`, `schema%add_metadata`, `schema%add_col_qc` / `schema%set_col_qc`, `schema%set_column_available` / `set_column_unavailable`, `schema%get_column_index` / `get_num_fields` / `get_field_name`

**Writer:** `parquet_open_writer`, `parquet_write_column`, `parquet_close_writer`, `parquet_get_chunk_size`

**Writer — streaming row groups:** `parquet_new_row_group`, `parquet_write_column_chunk`, `parquet_finish_row_group` — an alternative to `parquet_write_column` for a column too large to hold as one complete array; see [Streaming/chunked writes](doc/pages/writing.md#streamingchunked-writes)

**Reader — table & column info:** `parquet_open_reader`, `parquet_close_reader`, `parquet_get_nrows`, `parquet_get_num_row_groups`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_prefetch_columns`, `parquet_get_metadata`, `parquet_filter` (`filt%add`)

**Reader — column data:** `parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`

**Reader — streaming row groups:** `parquet_read_column_chunk`, `parquet_get_chunk_size` — an alternative to `parquet_read_column` for a column too large to hold as one complete array; see [Streaming/chunked reads](doc/pages/reading.md#streamingchunked-reads)

**Compact string columns:** `parquet_string_column` — a scalar `string` column can be written/read via `parquet_write_column`/`parquet_read_column` (and their chunked counterparts) as a `parquet_string_column` instead of a padded `character(len=...)` array, with no pre-sizing needed; see [Reading and writing compact string columns](doc/pages/string-columns.md#reading-and-writing-compact-string-columns)

## Limitations

Worth knowing up front before relying on this library:

- **Writing** is limited to the six types in [Supported data types](doc/pages/supported-data-types.md) — there is no `date`/`timestamp` support, and no arbitrary nested/struct/map columns. **Reading** additionally accepts a column physically stored as `int8`/`int16`/unsigned integers/`half_float`/`decimal` (widened into `integer(int32)`/`integer(int64)`/`real(real32)`/`real(real64)` as appropriate; see [Reading a column into a different numeric kind](doc/pages/supported-data-types.md#reading-a-column-into-a-different-numeric-kind)) — this only ever arises from a file written by some other tool, since this library's own writer never produces those physical types. A `STRUCT` column's individual fields, at any nesting depth, can also be read directly via a dot-separated path (e.g. `"main.inner.age"`) passed as `name`, as long as the path resolves down to a scalar or vector (`FIXED_SIZE_LIST`) leaf — see [Reading a nested struct field](doc/pages/supported-data-types.md#reading-a-nested-struct-field); naming an intermediate struct directly is not readable, and `MAP` columns, and variable-length `LIST` columns nested inside a struct path, remain unsupported. `date`/`timestamp`/`MAP` columns remain unsupported on both sides. Vector columns are fixed-length (`col_size`) only: every row must hold the same number of elements. Such a column can be *read back* whether it was stored on disk as a `fixed_size_list` or as a variable-length `list<element>` (see the [reading notes](doc/pages/reading.md)), but a genuinely ragged list (rows of differing length) is not supported — it is rejected on read — and this library's own writer only ever emits `fixed_size_list`.
- **Reading a column whose physical Parquet type doesn't match what you asked for aborts the process, but not via a clean `error stop`** — it's a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got date32[day])`), not a Fortran `error stop`. This covers the physical type being outside the six [supported data types](doc/pages/supported-data-types.md) (e.g. the file has a `date32` column) as well as a declared vector column's shape not matching what was requested, across every read function (`parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`, and vector-column reads).
- **A vector column's per-row width (`col_size`) is capped at 2,147,483,647 elements** — a hard limit of Arrow's `FixedSizeListType` itself (its `list_size` is a plain `int32_t`, with no "large" variant to fall back to, unlike Arrow's string type). Writing a column that would exceed it aborts the same way as the physical-type-mismatch case above (a C++-level abort via a stderr diagnostic, not a clean `error stop`), rather than silently truncating/corrupting the written column.
- **A vector column's flattened element count is capped at 2,147,483,647 elements *per row group*, not per file** — Parquet's own repetition/definition-level generation for list-typed columns walks every flattened element of a row group with a plain `int32_t` counter. This is handled automatically: `parquet_close_writer`'s row-group auto-sizing already accounts for each column's `col_size` and picks a smaller row-group size whenever a wide vector column needs it, so a column's *total* `nrows * col_size` — a real, hittable case (e.g. 2.5 billion rows at `col_size=2`) — can exceed 2,147,483,647 without any special handling, transparently split across multiple row groups. Only an *explicitly*-chosen `chunk_size` (`parquet_open_writer`/`parquet_set_writer_options`) that conflicts with a vector column's `col_size` aborts (a C++-level abort via a stderr diagnostic, not a clean `error stop`) rather than silently overriding the caller's request; see [Important behavior](#important-behavior).
- **A table is capped at 2,147,483,647 columns** — another hard limit of Arrow's own C++ API (`Schema::num_fields()`/`GetFieldIndex()` return a plain `int32_t` internally, with no int64/"large" variant for column count at all, unlike row count). Writing a column that would push the table's column count past this aborts the same way as the two cases above (a C++-level abort via a stderr diagnostic, not a clean `error stop`), rather than risking Arrow's own field-count bookkeeping silently wrapping/corrupting. Reaching this in practice would first require an enormous amount of memory and time for per-column bookkeeping (each column needs its own name/type/metadata), so it is not a limit expected to be hit by accident.
- A column can only be written once per `parquet_writer` via `parquet_write_column` — there's no way to append rows to an already-closed `.parquet` file. Writing the same column name twice this way fails immediately with `error stop`, naming the column, whether or not the writer has a MAML-derived schema. If a column is too large to hold as one complete array, see [Streaming/chunked writes](doc/pages/writing.md#streamingchunked-writes) for the incremental, row-group-at-a-time alternative (`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group`).
- By default, `parquet_open_writer` silently overwrites/truncates an existing file at that path. Pass `overwrite=.false.` to instead fail immediately with `error stop`, naming the file, if it already exists.
- **No predicate pushdown:** row filtering via `parquet_filter` (see [Row filtering](doc/pages/reading.md#row-filtering-with-parquet_filter)) narrows which rows your own code sees, but it's post-decode — every referenced column, and every column you subsequently read, is still fully read/decoded off disk exactly as without a filter; Parquet's own row-group statistics are never used to skip I/O.
- **A filtered reader cannot use streaming/chunked reads:** `parquet_read_column_chunk` fails immediately with `error stop` on a reader opened with `filter=`, since the filter mask is a single flat mask over the whole unfiltered file with no row-group structure of its own; open a second, unfiltered reader if you need both. See [Streaming/chunked reads](doc/pages/reading.md#streamingchunked-reads).
- `parquet_read_array_row_mode` reads only the one row group a given row falls in — genuine random access at row-group granularity, not a whole-column read — and `parquet_read_array_element_mode` streams the file row group by row group rather than materializing the whole column at once. Neither is single-row I/O in the strict sense (both still decode a full row group's worth of data), and both fall back to a genuine whole-column read once a row filter is active, since a filter mask has no row-group structure to resolve against.

## Contributing

See [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md) for building/testing this repository itself, its error-path testing infrastructure, and a list of features that have been considered but aren't yet implemented.

Generated API reference documentation ([FORD](https://forddocs.readthedocs.io/)) can be built locally with `ford docs.md`, producing HTML output in `ford-doc/`.

## License

BSD 3-Clause License — see [LICENSE](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/LICENSE). See [CHANGELOG.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CHANGELOG.md) for release history.
