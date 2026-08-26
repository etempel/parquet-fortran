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

Read and write parquet files from Fortran, with the table's schema and metadata defined in the [MAML format](https://github.com/asgr/MAML-Format) and converted to VOTable-style metadata in the parquet header. Around that sit the pieces a program reading columnar data usually needs anyway: a whole-file table container, type-erased column storage, sorting, and reproducible random numbers and sampling — each importable on its own, and most of them free of any Arrow dependency.

**Status: 2.0 — stable.** The `use parquet` API follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html) — see [API stability](#important-behavior) for exactly what that covers, and [CHANGELOG.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CHANGELOG.md) for release history, including what changed incompatibly in 2.0. `parquet_get_version` reports the version of the library you actually linked against, and `parquet_get_arrow_version` the Arrow/Parquet C++ version behind it.

**Features:**

- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character` (MAML: `boolean`/`string`) — as plain 1D columns or fixed-length vector columns (a vector column is read back whether it was stored on disk as a `fixed_size_list` or a variable-length `list`).
- Read and write `DATE`/`TIME`/`TIMESTAMP` columns via `parquet_date`/`parquet_time`/`parquet_timestamp` (nanosecond-precision, ISO-8601 parse/format, Unix-time/MJD/JD interop) — see [Date, time and timestamp columns](doc/pages/types/date-time.md).
- Define and validate a table's schema and metadata from a [MAML](https://github.com/asgr/MAML-Format) file, including column renaming (`col_map:`), quality-control range checks (`qc:`), and protecting specific columns from ever containing a Null (`protected_cols:`). MAML also drives *read-time* quality control (a qc-maml checked against an existing file) — see [Quality control](doc/pages/schema/quality-control.md).
- Read and write genuine Parquet Null values, with either substitution (`null_value=`) or a validity mask (`is_valid=`).
- Control output compression codec, compression level, and row group size.
- Streaming/chunked reads and writes for a column too large to hold as one complete array — see [Streaming/chunked writes](doc/pages/io/writing.md#streamingchunked-writes) and [Streaming/chunked reads](doc/pages/io/reading.md#streamingchunked-reads).
- Row filtering on read (`parquet_filter`), as boolean expressions over the file's columns (`and`/`or`/`not`, parentheses, SQL three-valued null handling, `is_nan`/`is_not_nan` for floating-point columns, ISO-8601 literals for `date`/`time`/`timestamp` columns), with row groups the filter provably cannot match skipped unread via their footer statistics, plus random downsampling (`sample_fraction`/`sample_seed`) — see [Row filtering](doc/pages/io/filter-sort-sample.md#row-filtering-with-parquet_filter) and [Random downsampling](doc/pages/io/filter-sort-sample.md#random-downsampling-with-sample_fraction).
- Sorted reads (`parquet_sortkey`): return a file's rows ordered by one or more columns, ascending or descending, with per-key null placement and stable ties — see [Reading rows in sorted order](doc/pages/io/filter-sort-sample.md#reading-rows-in-sorted-order-with-parquet_sortkey).
- Compact string columns (`parquet_string_column`) — append/search/mutate a scalar string column without pre-sizing a fixed-width buffer — see [Reading and writing compact string columns](doc/pages/types/string-columns.md#reading-and-writing-compact-string-columns).
- Read a file's own stored table metadata back (`parquet_get_metadata`), and prefetch specific columns before reading (`parquet_prefetch_columns`).
- Whole tables in memory (`parquet_table`): a file's columns as one named object, read on first use, reached by name as ordinary Fortran arrays or as zero-copy typed pointers — or built in memory column by column and written out. Filter, sort, take a top-N, delete or append rows, and slice a file into row ranges. Generated table types give a MAML schema named per-column accessors. See [Whole tables in memory](doc/pages/tables/index.md).
- Sorting for plain Fortran arrays and for column types (`pf_sort`, `pf_argsort`, and the partial/selection/search/unique family): stable, null- and NaN-aware, multi-key, threaded, over the six intrinsic element types plus `parquet_column`, `parquet_string_column` and the three temporal types. Nothing about it is parquet-specific, and it imports no Arrow. See [Sorting arrays and columns](doc/pages/utilities/sorting.md).
- Reproducible random numbers and sampling (`pf_random_at` and friends): counter-based, so element *k* of a stream is a pure function of `(seed, index)` — no state, no locks, identical values in any order and at any thread count. Uniforms, integers, bits, four distributions, permutations, subsets, resampling and weighted draws without replacement. See [Random numbers and sampling](doc/pages/utilities/random.md).
- Type-erased column storage (`parquet_column`): one whole column's values for any of 18 scalar/vector kinds behind a single `PK_*` kind tag, with per-element validity, so a column can be grown, sorted and handed to the writer without the caller knowing its type at compile time.
- Narrow imports: `use parquet` brings in everything, but every layer is an entry module in its own right and most of them never reach Arrow at all — `use parquet_sorting`, `use parquet_random` or `use parquet_strings` compiles a small fraction of the library. See [Which module do I import?](#which-module-do-i-import).
- Safe to use concurrently (e.g. from OpenMP): independent readers/writers per thread, and a shared `parquet_table` that many threads can read at once and append to at once (the table serialises appends itself). Every single-threaded requirement is enforced with a hard `error stop` naming what to do instead, rather than left to the caller — and `%prefetch`/`%materialize_all` read a wide file's columns in parallel internally. See [Thread safety](doc/pages/operating/thread-safety.md) for the per-operation table.

> 📖 **Full user guide:** [doc/pages/](doc/pages/index.md) — reading, writing, the MAML metadata format, thread safety, performance, and troubleshooting. The complete per-procedure API reference is generated from source via [FORD](https://forddocs.readthedocs.io/) and is the only exhaustive listing of the public surface — browse the published copy your checkout links to (the [API documentation](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main) badge above), or build it locally with `ford docs.md`. This page is the quick-start overview.

## Contents

- [Quick example](#quick-example)
- [Minimal setup to depend on this library](#minimal-setup-to-depend-on-this-library)
- [Important behavior](#important-behavior)
- [Prerequisites](#prerequisites)
  - [Environment variables](#environment-variables)
- [Which module do I import?](#which-module-do-i-import)
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
# Depend on a released version (recommended) -- pin to a tag:
parquet-fortran = { git = "https://github.com/etempel/parquet-fortran.git", tag = "v2.0.0" }

# Or, once published, from the fpm registry:
# parquet-fortran = "2.0.0"

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
- **API stability:** this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html). The stability promise covers the whole `use parquet` surface — every public type/procedure/constant reachable that way, including a public type's own type-bound procedures and operators (e.g. `schema%add_field`, `col%append_string`, `operator(-)` on the temporal types) — a breaking change to any of those requires a major version bump. Anything not reachable via `use parquet` (private module internals, `src/parquet_wrapper.cpp`'s C++ surface, file/module layout) can change in a minor or patch release. The promise also covers each advertised entry module *in its own right* — `parquet_io`, `parquet_tables`, `parquet_columns`, `parquet_list`, `parquet_strings`, `parquet_temporal`, `parquet_sorting`, `parquet_argsort`, `parquet_sampling`, `parquet_random`, `parquet_settings`, `parquet_version` and `parquet_maml_base`, listed under [Which module do I import?](#which-module-do-i-import) — since a module offered as an entry point has to stay usable through that import alone. That is wider than it sounds: a change to `parquet_column`'s type-bound procedures is a breaking change even if nothing reachable through `use parquet` moves. In particular, `parquet_core` — the internal module `parquet_io` and the `parquet` facade re-export the reader/writer/schema API from — is *not* part of the promise: `use parquet` or `use parquet_io` is the supported spelling, and `parquet_core` may be renamed or restructured at any time. Neither are `parquet_bindings`, `parquet_settings_base`, `parquet_expkey`, `parquet_ziggurat`, `parquet_sorting_oracle`, or any `*_engine`/`*_kernel` submodule: they are accessible because Fortran has no package scope, not because they are meant to be imported.

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
    - **NAG Fortran (nagfor)** v7.2. Set `FPM_CC`/`FPM_CXX` yourself (see
      [Environment variables](#environment-variables)), since the NAG family implies no C++
      compiler. fpm's OpenMP probe does not fit NAG's flag spelling, so the OpenMP flag reaches
      only the link line — add `-openmp` to `FPM_FFLAGS` to compile the threaded paths in.
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

Unit testing is handled using test-drive, which is automatically installed by FPM.

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

## Which module do I import?

**`use parquet`, unless you have a reason not to.** It brings the whole library into scope and is what the stability promise below is written around.

Every layer underneath is importable on its own, and several cost a great deal less to compile against. Sizes are this library's Fortran source files, measured by `tools/check_module_footprints.sh`:

| Import | Files | Fortran graph reaches Arrow? | What it gives you |
|---|---|---|---|
| `parquet_version` | 2 | no | `parquet_get_version`: which parquet-fortran this is |
| `parquet_temporal` | 1 | no | date/time/timestamp element types |
| `parquet_strings` | 2 | no | packed, null-aware string columns |
| `parquet_random` | 3 | no | counter-based random numbers and distributions |
| `parquet_argsort` | 4 | no | `pf_argsort` over the six intrinsic types |
| `parquet_sampling` | 8 | no | permutations, subsets, resampling, weighted draws |
| `parquet_spatial` | 9 | no | `pf_spatial_index`: neighbour and k-nearest search on a uniform grid, including on the sky |
| `parquet_columns` | 10 | no | the `parquet_column` container |
| `parquet_list` | 11 | no | `parquet_list_column`: variable-length list storage |
| `parquet_sorting` | 21 | no | the whole sorting API, every element type |
| `parquet_settings` | 3 | **yes** | the process-global knobs, and `parquet_get_arrow_version` |
| `parquet_io` | 46 | **yes** | reading and writing files, without the table layer |
| `parquet_tables` | 64 | **yes** | the `parquet_table` container |
| `parquet` | 73 | **yes** | everything, through one `use` |

**One caveat, and it is the one that matters: no import makes the *package* Arrow-free.** `link` is a package-level key in `fpm.toml` and fpm cannot prune a C++ translation unit, so depending on parquet-fortran compiles `src/parquet_wrapper.cpp` and links `-larrow -larrow_compute -lparquet` whichever module you name — `use parquet_temporal` included. Without Arrow's headers the build fails at `arrow/api.h` regardless. What the *Arrow-free* rows guarantee is narrower and is about the Fortran graph: none of the modules fpm compiles for that import names `parquet_bindings`.

One further entry module is importable on its own and carries the same stability promise, and is left out of the table above because you would reach for it for what it holds rather than for what it costs to compile: **`parquet_maml_base`**, the MAML schemas bundled with this library together with the shared `parquet_maml_file` type — see [Embedding your own MAML schemas](doc/pages/utilities/embedding-maml-schemas.md) for generating the equivalent module from your *own* schemas.

Each of the modules above re-exports, getter and setter both, the process-global settings its own code reads, so a narrow import can still be configured without naming `parquet_settings`. `parquet_get_version` is the deliberate exception to that pattern: it is not a setting and no tier re-exports it, so a narrow import that wants to report the library version adds `use parquet_version` (two files, no C++ boundary) alongside whatever else it imports. `parquet_core` is the one module never to import: it is internal, and `parquet_io` is its supported face. See [Choosing a module](doc/pages/operating/choosing-a-module.md) for the full story.

## Limitations

Worth knowing up front before relying on this library:

- **Writing** is limited to the six plain types in [Supported data types](doc/pages/types/supported-data-types.md) plus `date`/`time`/`timestamp` (see [Date, time and timestamp columns](doc/pages/types/date-time.md)) — there is no arbitrary nested/struct/map support, and no `INTERVAL`/duration type (a deliberately dropped non-goal, not a pending gap — see [Not yet supported](doc/pages/types/date-time.md#not-yet-supported) for why). **Reading** additionally accepts a column physically stored as `int8`/`int16`/unsigned integers/`half_float`/`decimal` (widened into `integer(int32)`/`integer(int64)`/`real(real32)`/`real(real64)` as appropriate; see [Reading a column into a different numeric kind](doc/pages/types/supported-data-types.md#reading-a-column-into-a-different-numeric-kind)) — this only ever arises from a file written by some other tool, since this library's own writer never produces those physical types. A `STRUCT` column's individual fields, at any nesting depth, can also be read directly via a dot-separated path (e.g. `"main.inner.age"`) passed as `name`, as long as the path resolves down to a scalar, vector (`FIXED_SIZE_LIST`) or variable-length `LIST` leaf — see [Reading a nested struct field](doc/pages/types/supported-data-types.md#reading-a-nested-struct-field); naming an intermediate struct directly is not readable, and `MAP` columns remain unsupported anywhere along a struct path. `MAP`/`INTERVAL` columns remain unsupported on both sides; `qc:` bounds (`min:`/`max:`) are not yet supported for `date`/`time`/`timestamp` columns, though `qc: miss:` and `parquet_filter` rules on them are — see [Filtering date, time and timestamp columns](doc/pages/io/filter-sort-sample.md#filtering-date-time-and-timestamp-columns). Vector columns are fixed-length (`col_size`) only: every row must hold the same number of elements, and such a column can be *read back* whether it was stored on disk as a `fixed_size_list` or as a variable-length `list<element>` (see the [reading notes](doc/pages/io/reading.md)). A genuinely **ragged** variable-length `LIST` column — rows of differing length — can be **read** into a `parquet_list_column` (see [Variable-length list columns](doc/pages/types/list-columns.md#reading-a-list-column-from-a-file)) but cannot yet be **written**: this library's own writer only ever emits `fixed_size_list`, and a ragged column is still rejected by a read into a 2-D array, which no array shape can hold.
- **Reading a column whose physical Parquet type doesn't match what you asked for aborts the process, but not via a clean `error stop`** — it's a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got timestamp[us])`), not a Fortran `error stop`. This covers the physical type being outside the supported [data types](doc/pages/types/supported-data-types.md) as well as a declared vector column's shape not matching what was requested, across every read function (`parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`, and vector-column reads).
- **A vector column's per-row width (`col_size`) is capped at 2,147,483,647 elements** — a hard limit of Arrow's `FixedSizeListType` itself (its `list_size` is a plain `int32_t`, with no "large" variant to fall back to, unlike Arrow's string type). Writing a column that would exceed it aborts the same way as the physical-type-mismatch case above (a C++-level abort via a stderr diagnostic, not a clean `error stop`), rather than silently truncating/corrupting the written column.
- **A vector column's flattened element count is capped at 2,147,483,647 elements *per row group*, not per file** — Parquet's own repetition/definition-level generation for list-typed columns walks every flattened element of a row group with a plain `int32_t` counter. This is handled automatically: `parquet_close_writer`'s row-group auto-sizing already accounts for each column's `col_size` and picks a smaller row-group size whenever a wide vector column needs it, so a column's *total* `nrows * col_size` — a real, hittable case (e.g. 2.5 billion rows at `col_size=2`) — can exceed 2,147,483,647 without any special handling, transparently split across multiple row groups. Only an *explicitly*-chosen `chunk_size` (`parquet_open_writer(..., chunk_size=)`) that conflicts with a vector column's `col_size` aborts (a C++-level abort via a stderr diagnostic, not a clean `error stop`) rather than silently overriding the caller's request; see [Important behavior](#important-behavior).
- **A table is capped at 2,147,483,647 columns** — another hard limit of Arrow's own C++ API (`Schema::num_fields()`/`GetFieldIndex()` return a plain `int32_t` internally, with no int64/"large" variant for column count at all, unlike row count). Writing a column that would push the table's column count past this aborts the same way as the two cases above (a C++-level abort via a stderr diagnostic, not a clean `error stop`), rather than risking Arrow's own field-count bookkeeping silently wrapping/corrupting. Reaching this in practice would first require an enormous amount of memory and time for per-column bookkeeping (each column needs its own name/type/metadata), so it is not a limit expected to be hit by accident.
- A column can only be written once per `parquet_writer` via `parquet_write_column` — there's no way to append rows to an already-closed `.parquet` file. Writing the same column name twice this way fails immediately with `error stop`, naming the column, whether or not the writer has a MAML-derived schema. If a column is too large to hold as one complete array, see [Streaming/chunked writes](doc/pages/io/writing.md#streamingchunked-writes) for the incremental, row-group-at-a-time alternative (`parquet_new_row_group`/`parquet_write_column_chunk`/`parquet_finish_row_group`).
- By default, `parquet_open_writer` silently overwrites/truncates an existing file at that path. Pass `overwrite=.false.` to instead fail immediately with `error stop`, naming the file, if it already exists.
- **A sorted reader cannot be read in row-group chunks.** With `sort_by=` active, `parquet_read_column_chunk` and `parquet_get_chunk_size` abort, and `parquet_read_array_row_mode`/`parquet_read_array_element_mode` fall back to a whole-column read. This is inherent rather than a pending gap: a sort permutation destroys row-group locality, so there is no coherent "row group N of the sorted output". Filtering and sampling have no such restriction — they only remove rows, never reorder them. A sort key column is also always read whole. See [What a sort disallows](doc/pages/io/filter-sort-sample.md#what-a-sort-disallows).
- **Pruning is row-group-granular, and only for `filter=`:** a filtered reader consults each row group's footer statistics and skips the row groups that provably cannot match — for the filter's own columns and for every column read afterwards (see [Row groups a filter cannot match are never read](doc/pages/io/filter-sort-sample.md#row-groups-a-filter-cannot-match-are-never-read)). Within a surviving row group nothing is skipped: there is no page-level pruning and no bloom-filter support, so a filter matching one row per row group still reads every row group in full. Files written without statistics, unsigned/decimal/`half_float` columns, and floating-point columns under `not`//= prune nothing at all. Random downsampling via `sample_fraction` (see [Random downsampling](doc/pages/io/filter-sort-sample.md#random-downsampling-with-sample_fraction)) never skips I/O — it is purely post-decode.
- `parquet_read_array_row_mode` reads only the one row group a given row falls in — genuine random access at row-group granularity, not a whole-column read — and `parquet_read_array_element_mode` streams the file row group by row group rather than materializing the whole column at once. Neither is single-row I/O in the strict sense (both still decode a full row group's worth of data). This holds with a row filter and/or sample active too: the index then addresses the filtered result, and it is resolved against each row group's surviving row count. Chunked reads (`parquet_read_column_chunk`) likewise work per row group on a filtered or sampled reader.

## Contributing

See [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md) for building/testing this repository itself, its error-path testing infrastructure, and a list of features that have been considered but aren't yet implemented.

Generated API reference documentation ([FORD](https://forddocs.readthedocs.io/)) can be built locally with `ford docs.md`, producing HTML output in `ford-doc/`.

## License

BSD 3-Clause License — see [LICENSE](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/LICENSE). See [CHANGELOG.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CHANGELOG.md) for release history.
