# parquet-fortran

[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)
![Language: Fortran](https://img.shields.io/badge/Language-Fortran-734f96.svg)
[![fpm](https://img.shields.io/badge/fpm-package-729FCF.svg)](https://fpm.fortran-lang.org/)

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

**Features:**
- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character` (MAML: `boolean`/`string`) — as plain 1D columns or fixed-length vector columns (a vector column is read back whether it was stored on disk as a `fixed_size_list` or a variable-length `list`).
- Define and validate a table's schema and metadata from a [MAML](https://github.com/asgr/MAML-Format) file, including column renaming (`col_map:`), quality-control range checks (`qc:`), and protecting specific columns from ever containing a Null (`protected_cols:`). This is relevant only when *writing* — reading a parquet file never involves MAML.
- Read and write genuine Parquet Null values, with either substitution (`null_value=`) or a validity mask (`is_valid=`).
- Control output compression codec, compression level, and row group size.
- Safe to use concurrently (e.g. from OpenMP) — see [Thread safety](MANUAL.md#thread-safety) for the exact rule.

> 📖 **Full user manual:** [MANUAL.md](MANUAL.md) — reading, writing, the MAML metadata format, the complete per-procedure API reference, thread safety, performance, and troubleshooting. This page is the quick-start overview.

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

Everything else — reading, writing, the MAML format, and the full per-procedure API reference — lives in the **[user manual](MANUAL.md)**.

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
See [Reading parquet files](MANUAL.md#reading-parquet-files-from-your-fortran-code) and [Writing parquet files](MANUAL.md#writing-parquet-files-from-your-fortran-code) in the manual for the full picture, including MAML-driven schemas, vector columns, and the features listed above. You'll need Arrow/Parquet available and a couple of environment variables set to actually build against this library — see [Prerequisites](#prerequisites).

## Minimal setup to depend on this library

Quickstart outline — see [Prerequisites](#prerequisites) and [Environment variables](#environment-variables) for the authoritative, per-platform details:

1. Install the Arrow/Parquet C++ library (e.g. macOS Homebrew: `brew install apache-arrow`).
2. Set `LIBRARY_PATH`, `FPM_FFLAGS`, `FPM_CXXFLAGS`, `FPM_LDFLAGS` to point at Arrow's `include`/`lib` — see [Environment variables](#environment-variables).
3. Add `parquet-fortran` to your own `fpm.toml` with `link = ["arrow", "parquet", "c++"]` — see [Reading parquet files](MANUAL.md#reading-parquet-files-from-your-fortran-code).
4. Build/test your project with `fpm test`.

> **On the link list:** your project lists `link = ["arrow", "parquet", "c++"]`, whereas `parquet-fortran`'s own `fpm.toml` lists `["arrow", "arrow_compute", "parquet"]`. The two differ intentionally — `arrow_compute` (used for read-side statistics and qc min/max) is propagated to you automatically by fpm, and the C++ runtime (`-lc++`/`-lstdc++`) comes in through `FPM_LDFLAGS` rather than the `link` list. See [Environment variables](#environment-variables) and the manual's [Troubleshooting](MANUAL.md#troubleshooting).

## Important behavior

- Most failures are reported via Fortran `error stop` and abort the process immediately.
- There are no status/`ierr` return codes in the public API.
- Some lower-level Arrow/Parquet failures may abort via C++ rather than `error stop`.

See [Error handling](MANUAL.md#error-handling) and [Limitations](#limitations) for full details.

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2025.3.0
    - Gfortran v15.2.0
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
- [apache-arrow](https://arrow.apache.org) (C++ library for parquet) v24.0.0

Installing the Arrow/Parquet C++ library itself (not a Fortran package, so it isn't installed by FPM):

- macOS (Homebrew): `brew install apache-arrow`
- macOS (MacPorts): `sudo port install apache-arrow`
- Debian/Ubuntu: follow [Arrow's official apt repository instructions](https://arrow.apache.org/install/) and install `libarrow-dev`/`libparquet-dev`
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

For genuine multi-threaded (OpenMP) use, your compiler's OpenMP flag must also be supplied via `FPM_FFLAGS` — see [Thread safety](MANUAL.md#thread-safety) for the per-compiler flags and why.

To build/test this repository itself (as opposed to depending on it from your own project), see [CONTRIBUTING.md](CONTRIBUTING.md).

Hitting a build or link error? See [Troubleshooting](MANUAL.md#troubleshooting) in the manual for the common symptoms and their fixes.

## API overview

A quick index of the public `use parquet` API. See the manual's **[parquet module API](MANUAL.md#parquet-module-api-functionssubroutines)** for the full per-procedure reference, and the linked how-to sections for usage.

**Types:** `parquet_writer`, `parquet_reader`, `parquet_schema`, `parquet_filter`, `parquet_column_info`, `parquet_column_type`, `parquet_table_metadata`, `parquet_maml_file`

**[Utility](MANUAL.md#utility):** `parquet_get_version`, `parquet_set_max_threads`

**[MAML and metadata](MANUAL.md#maml-and-metadata):** `parquet_parse_maml`, `parquet_load_maml_file`, `parquet_load_qc_maml_file`, `parquet_validate_maml`, `parquet_validate_user_maml` — plus the `parquet_schema` type-bound builders `schema%init`, `schema%add_field`, `schema%add_metadata`, `schema%add_col_qc` / `schema%get_col_qc`, `schema%set_column_available` / `set_column_unavailable`, `schema%get_column_index` / `get_num_fields` / `get_field_name`

**[Writer](MANUAL.md#writer):** `parquet_open_writer`, `parquet_write_column`, `parquet_close_writer`

**[Reader — table & column info](MANUAL.md#reader-table-and-column-info):** `parquet_open_reader`, `parquet_close_reader`, `parquet_get_nrows`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_prefetch_columns`, `parquet_get_metadata`, `parquet_filter` (`filt%add`)

**[Reader — column data](MANUAL.md#reader-reads-column-data):** `parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`

## Limitations

Worth knowing up front before relying on this library:

- Only the six types in [Supported data types](MANUAL.md#supported-data-types) are handled — there is no `date`/`timestamp`/`decimal` support, no `int8`/`int16`/unsigned integers, and no arbitrary nested/struct/map columns. Vector columns are fixed-length (`col_size`) only: every row must hold the same number of elements. Such a column can be *read back* whether it was stored on disk as a `fixed_size_list` or as a variable-length `list<element>` (see the [reading notes](MANUAL.md#reading-parquet-files-from-your-fortran-code)), but a genuinely ragged list (rows of differing length) is not supported — it is rejected on read — and this library's own writer only ever emits `fixed_size_list`.
- **Reading a column whose physical Parquet type doesn't match what you asked for aborts the process, but not via a clean `error stop`** — it's a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got date32[day])`), not a Fortran `error stop`. This covers the physical type being outside the six [supported data types](MANUAL.md#supported-data-types) (e.g. the file has a `date32` column) as well as a declared vector column's shape not matching what was requested, across every read function (`parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`, and vector-column reads).
- A column can only be written once per `parquet_writer` — there's no incremental/streaming append to a column across multiple `parquet_write_column` calls, and no way to append rows to an already-closed `.parquet` file. Writing the same column name twice fails immediately with `error stop`, naming the column, whether or not the writer has a MAML-derived schema.
- **`parquet_open_writer` silently overwrites/truncates an existing file at that path** — there is no existence check and no warning.
- There is no random-access or predicate-pushdown read: `parquet_read_array_row_mode`/`parquet_read_array_element_mode` avoid loading a whole *column* at once, but there's no way to filter which *rows* are read across a table.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for building/testing this repository itself, its error-path testing infrastructure, and a list of features that have been considered but aren't yet implemented.

## License

BSD 3-Clause License — see [LICENSE](LICENSE). See [CHANGELOG.md](CHANGELOG.md) for release history.
