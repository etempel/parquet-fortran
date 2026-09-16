<p align="center">
  <img src="doc/media/logo.svg" alt="parquet-fortran logo" width="100">
</p>

# parquet-fortran

[![CI results](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/pipeline.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![Test coverage](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/coverage.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![API documentation](https://gitlab.4most.eu/ole/docserver/-/raw/master/API-documentation-blue.svg)](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran-734f96.svg)](https://fortran-lang.org)
[![fpm](https://img.shields.io/badge/fpm-package-729FCF.svg)](https://fpm.fortran-lang.org/)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/LICENSE)

Read and write parquet files from Fortran, with the table's schema and metadata defined in the [MAML format](https://github.com/asgr/MAML-Format) and converted to VOTable-style metadata in the parquet header. Around that sit the pieces a program reading columnar data usually needs anyway: a whole-file table container, type-erased column storage, sorting and array statistics, numerical integration, interpolation of tabulated data, function minimisation including Powell's derivative-free solvers, reproducible random numbers and sampling, spatial and HEALPix indexing, key-to-index lookup, TOML configuration files, and logging, numeric and path helpers — each importable on its own, and most of them free of any Arrow dependency.

**Features:**

- **Read and write parquet columns** for `int32`/`int64`/`float32`/`float64`/`logical`/`character` (MAML: `boolean`/`string`), as plain 1-D columns or fixed-length vector columns, plus `DATE`/`TIME`/`TIMESTAMP` element types and the three container kinds — a variable-length `LIST`, a `STRUCT` and a string-keyed `MAP`, which may nest on read. Genuine Parquet Nulls throughout, by substitution (`null_value=`) or a validity mask (`is_valid=`). See [Supported data types](doc/pages/types/supported-data-types.md).
- **A table's schema and metadata from a [MAML](https://github.com/asgr/MAML-Format) file**, converted to VOTable-style metadata in the parquet header: column renaming, quality-control range checks, and columns protected from ever holding a Null — applied on write, and checked against an existing file on read. See [The MAML metadata format](doc/pages/schema/maml-format.md).
- **Reads that skip work**: row filtering as boolean expressions over the file's columns, with row groups the filter provably cannot match skipped unread via their footer statistics; membership against a bound set (`ID in @wanted`), which is evaluated before the data columns are read and so prunes exactly — on a string column, and on a file written without statistics; sorted reads; random downsampling; and random access at row-group granularity. See [Filtering, sorting and sampling](doc/pages/io/filter-sort-sample.md).
- **Streaming reads and writes** for a column too large to hold as one array, or an output file that stays open while batch after batch is appended to it, plus control over the compression codec, the compression level and the row group size. See [Writing parquet files](doc/pages/io/writing.md).
- **Whole tables in memory** (`parquet_table`): a file's columns as one named object, read on first use and reached by name as ordinary Fortran arrays or as zero-copy typed pointers — or built in memory column by column and written out. Filter, sort, take a top-N, delete or append rows, drop duplicates, count how often each value occurs, repeat rows from a count, drop or project down to the columns you want, read a group of them as one `(column, row)` matrix, fill in or drop what is missing, read a text column back as numbers or render one as text, join another table's columns on matching keys, group rows by one or more key columns and answer per group, build a lookup index over a column and query it by key, slice a file into row ranges, or walk a file larger than memory one row group at a time. See [Whole tables in memory](doc/pages/tables/index.md).
- **Column storage in its own right**: `parquet_column`, type-erased over 18 scalar and vector kinds with per-element validity, so a column can be grown, sorted and handed to the writer without the caller knowing its type at compile time; and `parquet_string_column`, a packed string column needing no fixed-width buffer. See [Data types](doc/pages/types/index.md).
- **Utilities that never open a parquet file** — sorting, ranking and selection, array statistics, adaptive numerical integration of a function of one variable, interpolation of tabulated data, minimisation of a function of one or many variables (locally from a start point, globally over a box, or by Powell's model-based methods vendored from PRIMA), reproducible counter-based random numbers and sampling, spatial neighbour search, HEALPix pixelisation, key-to-index lookup, leveled logging, TOML configuration files, and numeric, text and path helpers. See [Utilities and code generation](doc/pages/utilities/index.md).
- **Narrow imports**: `use parquet` brings in everything, but every layer is an entry module in its own right and most of them never reach Arrow at all — `use parquet_sorting`, `use parquet_random` or `use parquet_strings` compiles a small fraction of the library. See [Choosing a module](doc/pages/operating/choosing-a-module.md).
- **Safe to use concurrently** (e.g. from OpenMP): independent readers and writers per thread, and a shared `parquet_table` that many threads can read at once and append to at once. Every single-threaded requirement is enforced with a hard `error stop` naming what to do instead, rather than left to the caller. See [Thread safety](doc/pages/operating/thread-safety.md).

> 📖 **Full user guide:** [doc/pages/](doc/pages/index.md) — reading, writing, the MAML metadata format, thread safety, performance, and troubleshooting. The complete per-procedure API reference is generated from source via [FORD](https://forddocs.readthedocs.io/) and is the only exhaustive listing of the public surface — browse the published copy your checkout links to (the [API documentation](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main) badge above), or build it locally with `ford docs.md`. This page is the quick-start overview.

## Contents

- [Quick example](#quick-example)
- [Minimal setup to depend on this library](#minimal-setup-to-depend-on-this-library)
- [Important behavior](#important-behavior)
- [Prerequisites](#prerequisites)
  - [Environment variables](#environment-variables)
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
See [Reading parquet files](doc/pages/io/reading.md) and [Writing parquet files](doc/pages/io/writing.md) in the user guide for the full picture, including MAML-driven schemas, vector columns, and the features listed above. You'll need Arrow/Parquet available and a couple of environment variables set to actually build against this library — see [Prerequisites](#prerequisites).

## Minimal setup to depend on this library

Quickstart outline — see [Prerequisites](#prerequisites) and [Environment variables](#environment-variables) for the authoritative, per-platform details:

1. Install the Arrow/Parquet C++ library (e.g. macOS Homebrew: `brew install apache-arrow`).
2. Set `LIBRARY_PATH`, `FPM_FFLAGS`, `FPM_CXXFLAGS`, `FPM_LDFLAGS` to point at Arrow's `include`/`lib` — see [Environment variables](#environment-variables).
3. Add `parquet-fortran` to your own `fpm.toml`, with `link = ["arrow", "parquet"]`:

```toml
[dependencies]
# Depend on a released version (recommended) -- pin to a tag; the repository's tag list
# has what is available.
parquet-fortran = { git = "https://github.com/etempel/parquet-fortran.git", tag = "v2.0.0" }

# Or, for local development against a working copy on disk instead of a released version:
# parquet-fortran = { path = "/path/to/parquet-fortran" }

[build]
link = ["arrow", "parquet"]
```

4. Build/test your project with `fpm test`.

> **On the link list:** your project lists `link = ["arrow", "parquet"]`, whereas `parquet-fortran`'s own `fpm.toml` lists `["arrow", "arrow_compute", "parquet"]`. fpm passes that whole list on to whatever depends on the library, so listing `arrow` and `parquet` yourself is insurance rather than a requirement, and `arrow_compute` (used for read-side statistics and for the row-filter/sort kernels) never needs listing. The C++ runtime (`-lstdc++` on Linux/GCC, `-lc++` on macOS/Clang) is added by fpm itself when it links a project containing C++ sources — it comes from neither the `link` list nor `FPM_LDFLAGS`, so there is no `"c++"` entry to add here. See [Environment variables](#environment-variables) and the guide's [Troubleshooting](doc/pages/operating/troubleshooting.md).

## Important behavior

- **Parquet in, parquet out.** This library reads parquet and writes parquet, and will never read or write another file format. Any conversion — FITS, CSV, HDF5, anything else — happens before the library is called or after it returns, in whatever tool already does that job. This is the boundary that keeps the dependency list to Arrow/Parquet alone; it is a settled scope decision, not a backlog item.
- Most failures are reported via Fortran `error stop` and abort the process immediately. There are no status/`ierr` return codes in the public API. Some lower-level Arrow/Parquet failures may abort via C++ rather than `error stop`.
- Reading and writing a *scalar* column (`col_size = 1`) with more than 2,147,483,647 (2^31-1, Fortran's default-integer `huge(1)`) rows is fully supported for every data type — including addressing an individual row past that count via `parquet_read_array_row_mode`. A *vector* column (`col_size > 1`) is capped at that same limit for its own per-row width (`col_size`); its total element count (`nrows * col_size`) has no such cap — row-group sizing handles Parquet's per-row-group element-count ceiling automatically; see [Limitations](#limitations).
- **`float32`/`float64` columns are always written with BYTE_STREAM_SPLIT encoding and dictionary encoding disabled**, automatically, regardless of the chosen `compression` codec — see [Writer options](doc/pages/io/writing.md#writer-options).
- **`qc:` (`min:`/`max:`/`miss:`) enforcement defaults to *on*, on both `parquet_open_writer` and `parquet_open_reader`, whenever a `schema=` is given** (pass `qc=.false.` to opt out; a no-op without a schema). A violation never aborts on write, and only aborts on read if you haven't passed `qc_soft=.true.`; see [Quality control](doc/pages/schema/quality-control.md) for both sides.
- **What is public API**: the whole `use parquet` surface, and each advertised entry module in its own right — so a change to `parquet_column`'s bindings is a public change even when nothing reachable through `use parquet` moves. `parquet_core`, `parquet_bindings` and every `*_engine`/`*_kernel` submodule are not: they are reachable only because Fortran has no package scope. See [Choosing a module](doc/pages/operating/choosing-a-module.md#what-the-stability-promise-covers).

See [Error handling](doc/pages/operating/error-handling.md) and [Limitations](#limitations) for full details.

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler — the test suite is run against four. Only **gfortran** is exercised by the
  GitLab CI pipeline; the other three are confirmed by hand.
    - **Gfortran** v15.2.0 (development), v13 (CI). **Minimum 13** — older versions (e.g. Ubuntu
      22.04's default compiler) miscompile part of the schema-building API; see
      [Troubleshooting](doc/pages/operating/troubleshooting.md) if you hit a spurious
      "column not found" abort.
    - **Intel Fortran (ifx)** v2026.1.0 and v2026.1.1.
    - **NAG Fortran (nagfor)** v7.2, which needs two things. Set `FPM_CC`/`FPM_CXX` yourself
      (see [Environment variables](#environment-variables)), since the NAG family implies no C++
      compiler. And put this repository's `tools/nagfor_fpm_shim` first on `PATH`: fpm 0.13 builds
      a NAG command line this library cannot be compiled or linked from, and the shim corrects it.
      Without the shim the build fails with `clang: error: no such file or directory: 'arrow'` or
      `Option error: -openmp option specified twice` — neither of which is about your Arrow
      install. See
      [Troubleshooting](doc/pages/operating/troubleshooting.md#build-and-compile-errors).
    - **LLVM flang** v22.1.8.
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/)) — **minimum 0.13.0.**
  This project's `fpm.toml` uses the `[features]` table and the feature-list form of
  `[profiles]`, both introduced in that release; fpm 0.12.0 cannot parse the manifest at
  all, and reports it as an error in *your* package file — see
  [Troubleshooting](doc/pages/operating/troubleshooting.md#build-and-compile-errors).
- A C++20-capable C++ compiler (Arrow/Parquet headers use `std::span`
  unconditionally): e.g. GCC ≥ 11 / a recent Clang. `-std=c++20` must be set
  (see [Environment variables](#environment-variables)).
- [apache-arrow](https://arrow.apache.org) (C++ library for parquet) ≥ v24.0.0 — CI installs whatever is currently latest from Arrow's own apt repository, not a pinned version, so treat this as a floor rather than an exact match.

Installing the Arrow/Parquet C++ library itself (not a Fortran package, so it isn't installed by FPM):

- macOS (Homebrew): `brew install apache-arrow`
- macOS (MacPorts): `sudo port install apache-arrow`
- Debian/Ubuntu: follow [Arrow's official apt repository instructions](https://arrow.apache.org/install/) and install `libarrow-dev`, `libarrow-compute-dev` and `libparquet-dev` (the separate `libarrow-compute-dev` provides the `arrow_compute` library that `fpm.toml` links — see [Troubleshooting](doc/pages/operating/troubleshooting.md))
- conda-forge: `conda install -c conda-forge libarrow libparquet`

Whichever route you use, take note of the resulting `include`/`lib` directories — they're what the environment variables below need to point at.

**One Fortran package dependency, installed automatically by FPM:** [toml-f](https://github.com/toml-f/toml-f), the TOML parser `parquet_toml` is built on. `fpm.toml` pins a fork of it (`etempel/toml-f`, branch `nagfor_fix`) carrying two NAG compiler fixes that upstream does not have. fpm resolves a package's dependency tree before it prunes modules, so it is fetched for every consumer whether or not you import `parquet_toml` — which means `fpm build` needs that git remote reachable the first time. Unit testing is handled using test-drive, also installed automatically.

### Environment variables

To build with Intel Fortran (or any supported compiler), set these variables so FPM can find Arrow/Parquet:

- LIBRARY_PATH should point to the parquet and arrow library.
- FPM_FFLAGS should point to arrows include directory
- FPM_CXXFLAGS should add relevant C++ flags
- FPM_LDFLAGS should point to arrow and parquet library
- FPM_FC can be used to set fortran compiler for FPM (e.g. FPM_FC=ifx)
- FPM_CC / FPM_CXX set the C and C++ compilers. Usually unnecessary — fpm derives them from
  the Fortran compiler's family — but needed when that family implies nothing, as with
  `FPM_FC=nagfor` (`FPM_CC=gcc`, `FPM_CXX=g++`).

In bash, initialize them as follows (replace `path_arrow` with your install root):

macOS (Clang/libc++):

```bash
export LIBRARY_PATH=path_arrow/lib:$LIBRARY_PATH
export FPM_FFLAGS="-Ipath_arrow/include"
export FPM_CXXFLAGS="-std=c++20 -stdlib=libc++ -Ipath_arrow/include"
export FPM_LDFLAGS="-Lpath_arrow/lib"
export FPM_FC=ifx
```

Linux (GCC/libstdc++):

```bash
export LIBRARY_PATH=path_arrow/lib:$LIBRARY_PATH
export FPM_FFLAGS="-Ipath_arrow/include"
export FPM_CXXFLAGS="-std=c++20 -Ipath_arrow/include"
export FPM_LDFLAGS="-Lpath_arrow/lib"
export FPM_FC=gfortran
```

Note: `-std=c++20` is required on every platform (Arrow/Parquet headers use `std::span` unconditionally).
`-stdlib=libc++` is macOS/Clang-specific and should be dropped on Linux.

Note: the C++ standard library needs no entry in `FPM_LDFLAGS` — fpm adds it itself when it
links a project containing C++ sources. Nor is a *runtime* library path (`DYLD_LIBRARY_PATH`,
`LD_LIBRARY_PATH`) part of the normal setup; it is only needed if you built Arrow into a
private prefix, and [Troubleshooting](doc/pages/operating/troubleshooting.md#runtime-errors)
covers that case.

Note: the exact variable set can vary by operating system and compiler toolchain.

For genuine multi-threaded (OpenMP) use: `parquet-fortran`'s own `fpm.toml` already declares fpm's built-in `openmp` metapackage dependency, which supplies the right compiler-specific OpenMP flag automatically for the whole build — no manual `FPM_FFLAGS` addition needed. See [Thread safety](doc/pages/operating/thread-safety.md) for the exact rule and how to also cover your own `!$omp parallel` regions.

To build/test this repository itself (as opposed to depending on it from your own project), see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md).

Only fpm is a supported way to consume this library (`[install] library = false` in `fpm.toml` means there's no installed `.mod`/library artifact for a non-fpm build system to link against directly).

Hitting a build or link error? See [Troubleshooting](doc/pages/operating/troubleshooting.md) in the user guide for the common symptoms and their fixes. Hitting a *runtime* "library not found" error instead (`dyld: Library not loaded` / `error while loading shared libraries`) after a successful build? See that page's [Runtime errors](doc/pages/operating/troubleshooting.md#runtime-errors) section.

## Limitations

Worth knowing up front before relying on this library:

- **Writing** covers the six plain types plus `date`/`time`/`timestamp` and the three container kinds. There is no `INTERVAL`/duration type, and a container nested inside another container is **read-only** — writing one is refused, naming the column and the offending field. **Reading** additionally accepts a column physically stored as `int8`/`int16`/an unsigned integer/`half_float`/`decimal`, widened into the nearest Fortran kind that holds it, a dictionary-encoded column (what pandas writes for a `category`), decoded to its plain values, and a `STRUCT`'s individual leaves addressed by a dotted path. See [Supported data types](doc/pages/types/supported-data-types.md).
- **Vector columns are fixed-length**: every row holds the same number of elements, however the file stored them. Genuinely ragged data goes through a `parquet_list_column` instead, and the type you pass decides the file's physical shape. See [Variable-length list columns](doc/pages/types/list-columns.md).
- **A failure on the Arrow side aborts the process via C++, not a clean `error stop`** — a diagnostic on stderr naming the column, then exit. This covers a physical type that does not match what you asked for, a Null read without `null_value=`/`is_valid=`, and the ceilings below. See [Error handling](doc/pages/operating/error-handling.md).
- **Four of Arrow's own int32 ceilings apply**, each refused rather than silently wrapped: a vector column's per-row width (`col_size`), a table's column count, one `MAP` column's total entry count, and a vector or list column's flattened element count *per row group*. The last is handled for you — `parquet_close_writer` sizes row groups down to fit, so a column's *total* element count is unbounded and only an explicit `chunk_size=` that conflicts with it aborts. Row counts themselves are `int64` throughout and are not capped. See [Supported data types](doc/pages/types/supported-data-types.md#vector-column-per-row-group-element-count-limit).
- **A column is written once per writer**, and there is no appending to a closed file; writing the same name twice fails immediately with `error stop`. `parquet_open_writer` overwrites an existing file unless you pass `overwrite=.false.`. See [Writing parquet files](doc/pages/io/writing.md).
- **A sorted reader cannot be read in row-group chunks** — a sort permutation destroys row-group locality, so there is no coherent "row group N of the sorted output". Filtering and sampling have no such restriction, since they only remove rows. Pruning is row-group-granular and only for `filter=`: within a surviving row group nothing is skipped, there is no page-level pruning or bloom-filter support, and downsampling never skips I/O at all. See [Filtering, sorting and sampling](doc/pages/io/filter-sort-sample.md).

## Contributing

See [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md) for building/testing this repository itself, its error-path testing infrastructure, and a list of features that have been considered but aren't yet implemented.

Generated API reference documentation ([FORD](https://forddocs.readthedocs.io/)) can be built locally with `ford docs.md`, producing HTML output in `ford-doc/`.

## License

BSD 3-Clause License — see [LICENSE](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/LICENSE). See [CHANGELOG.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CHANGELOG.md) for release history.

The `src/parquet_prima_*.f90` files are derived from [PRIMA](https://github.com/libprima/prima) (Reference Implementation for Powell's methods with Modernization and Amelioration, Zaikun Zhang) under the BSD-3-Clause licence, at commit `43863c69`, and reworked for this library. The algorithms are M. J. D. Powell's, whose original Fortran 77 solvers carry their own BSD-3-Clause licence. The BSD-3-Clause text and the full list of changes are reproduced in `src/parquet_prima_linalg.f90`'s header, and each engine file names what it came from.

`src/parquet_integrate_engine.f90` is derived from QUADPACK (R. Piessens, E. de Doncker-Kapenga, C. W. Ueberhuber and D. K. Kahaner, *QUADPACK: A Subroutine Package for Automatic Integration*, Springer 1983; public domain), as modernised by Jacob Williams in [jacobwilliams/quadpack](https://github.com/jacobwilliams/quadpack) under the BSD-3-Clause licence, and reworked for this library. The BSD-3-Clause text and the full list of changes are reproduced in that file's header.
