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

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

**Status: 1.0 — first stable release.** The `use parquet` API follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html) — see [API stability](#important-behavior) for exactly what that covers, and [CHANGELOG.md](CHANGELOG.md) for release history. `parquet_get_version` reports the version of the library you actually linked against.

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
- Safe to use concurrently (e.g. from OpenMP): independent readers/writers per thread, and a shared `parquet_table` that many threads can read at once and append to at once (the table serialises appends itself). Every single-threaded requirement is enforced with a hard `error stop` naming what to do instead, rather than left to the caller — and `%prefetch`/`%materialize_all` read a wide file's columns in parallel internally. See [Thread safety](doc/pages/operating/thread-safety.md) for the per-operation table.

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
See [Reading parquet files](doc/pages/io/reading.md) and [Writing parquet files](doc/pages/io/writing.md) in the user guide for the full picture, including MAML-driven schemas, vector columns, and the features listed above. You'll need Arrow/Parquet available and a couple of environment variables set to actually build against this library — see [Prerequisites](#prerequisites).

## Minimal setup to depend on this library

Quickstart outline — see [Prerequisites](#prerequisites) and [Environment variables](#environment-variables) for the authoritative, per-platform details:

1. Install the Arrow/Parquet C++ library (e.g. macOS Homebrew: `brew install apache-arrow`).
2. Set `LIBRARY_PATH`, `FPM_FFLAGS`, `FPM_CXXFLAGS`, `FPM_LDFLAGS` to point at Arrow's `include`/`lib` — see [Environment variables](#environment-variables).
3. Add `parquet-fortran` to your own `fpm.toml`, with `link = ["arrow", "parquet"]`:

```toml
[dependencies]
# Depend on a released version (recommended) -- pin to a tag:
parquet-fortran = { git = "https://github.com/etempel/parquet-fortran.git", tag = "v1.0.0" }

# Or, once published, from the fpm registry:
# parquet-fortran = "1.0.0"

# Or, for local development against a working copy on disk instead of a released version:
# parquet-fortran = { path = "/path/to/parquet-fortran" }

[build]
link = ["arrow", "parquet"]
```

4. Build/test your project with `fpm test`.

> **On the link list:** your project lists `link = ["arrow", "parquet"]`, whereas `parquet-fortran`'s own `fpm.toml` lists `["arrow", "arrow_compute", "parquet"]`. The two differ intentionally — `arrow_compute` (used for read-side statistics and qc min/max) is propagated to you automatically by fpm, and the C++ runtime (`-lstdc++` on Linux/GCC, `-lc++` on macOS/Clang) comes in through `FPM_LDFLAGS` rather than the `link` list, not through a `"c++"` entry here. See [Environment variables](#environment-variables) and the guide's [Troubleshooting](doc/pages/operating/troubleshooting.md).

## Important behavior

- **Parquet in, parquet out.** This library reads parquet and writes parquet, and will never read or write another file format. Any conversion — FITS, CSV, HDF5, anything else — happens before the library is called or after it returns, in whatever tool already does that job. This is the boundary that keeps the dependency list to Arrow/Parquet alone; it is a settled scope decision, not a backlog item.
- Most failures are reported via Fortran `error stop` and abort the process immediately. There are no status/`ierr` return codes in the public API. Some lower-level Arrow/Parquet failures may abort via C++ rather than `error stop`.
- Reading and writing a *scalar* column (`col_size = 1`) with more than 2,147,483,647 (2^31-1, Fortran's default-integer `huge(1)`) rows is fully supported for every data type — including addressing an individual row past that count via `parquet_read_array_row_mode`. A *vector* column (`col_size > 1`) is capped at that same limit for its own per-row width (`col_size`); its total element count (`nrows * col_size`) has no such cap — row-group sizing handles Parquet's per-row-group element-count ceiling automatically; see [Limitations](#limitations).
- **`float32`/`float64` columns are always written with BYTE_STREAM_SPLIT encoding and dictionary encoding disabled**, automatically, regardless of the chosen `compression` codec — see [Writer options](doc/pages/io/writing.md#writer-options).
- **`qc:` (`min:`/`max:`/`miss:`) enforcement defaults to *on*, on both `parquet_open_writer` and `parquet_open_reader`, whenever a `schema=` is given** (pass `qc=.false.` to opt out; a no-op without a schema). A violation never aborts on write, and only aborts on read if you haven't passed `qc_soft=.true.`; see [Quality control](doc/pages/schema/quality-control.md) for both sides.
- **API stability:** this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html). The stability promise covers the whole `use parquet` surface documented under [API overview](#api-overview) below: every public type/procedure/constant reachable that way, including a public type's own type-bound procedures and operators (e.g. `schema%add_field`, `col%append_string`, `operator(-)` on the temporal types) — a breaking change to any of those requires a major version bump. Anything not reachable via `use parquet` (private module internals, `src/parquet_wrapper.cpp`'s C++ surface, file/module layout) can change in a minor or patch release. In particular, `parquet_core` — the internal module the `parquet` facade re-exports the reader/writer/schema API from — is *not* part of the promise: `use parquet` is the supported spelling, and `parquet_core` may be renamed or restructured at any time.

See [Error handling](doc/pages/operating/error-handling.md) and [Limitations](#limitations) for full details.

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2026.1.0 and v2026.1.1 — confirmed manually outside CI; not exercised by the GitLab CI pipeline itself (gfortran only, see below).
    - Gfortran v15.2.0 (development), v13 (CI)
    - **Minimum gfortran: 13.** Older versions (e.g. Ubuntu 22.04's default compiler)
      miscompile part of the schema-building API — see
      [Troubleshooting](doc/pages/operating/troubleshooting.md) if you hit a spurious
      "column not found" abort.
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
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
export FPM_FC=gfortran
```

Note: `-std=c++20` is required on every platform (Arrow/Parquet headers use `std::span` unconditionally).
`-stdlib=libc++` is macOS/Clang-specific and should be dropped on Linux.

Note: the exact variable set can vary by operating system and compiler toolchain.

For genuine multi-threaded (OpenMP) use: `parquet-fortran`'s own `fpm.toml` already declares fpm's built-in `openmp` metapackage dependency, which supplies the right compiler-specific OpenMP flag automatically for the whole build — no manual `FPM_FFLAGS` addition needed. See [Thread safety](doc/pages/operating/thread-safety.md) for the exact rule and how to also cover your own `!$omp parallel` regions.

To build/test this repository itself (as opposed to depending on it from your own project), see [CONTRIBUTING.md](https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/CONTRIBUTING.md).

Only fpm is a supported way to consume this library (`[install] library = false` in `fpm.toml` means there's no installed `.mod`/library artifact for a non-fpm build system to link against directly).

Hitting a build or link error? See [Troubleshooting](doc/pages/operating/troubleshooting.md) in the user guide for the common symptoms and their fixes. Hitting a *runtime* "library not found" error instead (`dyld: Library not loaded` / `error while loading shared libraries`) after a successful build? The same page's Troubleshooting guide covers `DYLD_LIBRARY_PATH`/`LD_LIBRARY_PATH` too.

## API overview

A quick index of the public `use parquet` API. For the full per-procedure reference, generate the API docs locally with FORD (see [Contributing](#contributing)); for usage, see the linked how-to sections in the [user guide](doc/pages/index.md).

**One `use parquet` is all you need** — it brings the entire library into scope: the reader/writer/schema core, the `parquet_table` container, the `parquet_column` foundation and its `PK_*` kind constants, compact string columns, the date/time/timestamp types, the `pf_*` sorting and random-number APIs, and the process-global settings. The individual modules named below (`parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal`, `parquet_sorting`, `parquet_random`, `parquet_settings`, `parquet_maml_base`) remain usable on their own if you prefer a narrower import, but nothing requires it.

**Types:** `parquet_writer`, `parquet_reader`, `parquet_schema`, `parquet_filter`, `parquet_sortkey`, `parquet_read_qc`, `parquet_column_info`, `parquet_column_type`, `parquet_table_metadata`, `parquet_maml_file`, `parquet_string_column`, `parquet_string`, `parquet_date`, `parquet_time`, `parquet_timestamp`, `parquet_column`, `pf_sort_keys`

**Utility:** `parquet_get_version` (bare call reports this library's own version; `mode="internal"`/`"arrow"`/`"parquet"` instead report the linked Arrow/Parquet C++ library's own version — useful to include when filing a bug report), `parquet_set_arrow_threads` / `parquet_get_arrow_threads`, `parquet_set_threads`, `parquet_set_sort_threads` / `parquet_get_sort_threads`, `parquet_set_prefetch_threads` / `parquet_get_prefetch_threads`, `parquet_set_table_threads` / `parquet_get_table_threads`, `parquet_set_string_threads` / `parquet_get_string_threads`, `parquet_set_default_compression` / `parquet_get_default_compression`, `parquet_set_default_compression_level` / `parquet_get_default_compression_level`, `parquet_set_default_use_threads` / `parquet_get_default_use_threads`, `parquet_set_verbosity` / `parquet_get_verbosity`, `parquet_set_message_stream` / `parquet_get_message_stream`, `parquet_set_sort_counting_path` / `parquet_get_sort_counting_path`, `parquet_set_sort_radix_path` / `parquet_get_sort_radix_path`, `parquet_set_sort_counting_bucket_limit` / `parquet_get_sort_counting_bucket_limit`, `parquet_set_target_row_group_bytes` / `parquet_get_target_row_group_bytes`, `parquet_set_statistics_prescreen` / `parquet_get_statistics_prescreen`, `parquet_settings_from_env`, `parquet_reset_settings`, `parquet_print_settings`, and the read-only limits `parquet_max_filter_rule_len` / `parquet_max_filter_depth` / `parquet_max_filter_nodes` / `parquet_max_sort_keys` / `parquet_max_sort_key_len` / `parquet_max_maml_line_len` — process-global settings, see [Settings](doc/pages/operating/settings.md)

**MAML and metadata:** `parquet_parse_maml`, `parquet_load_maml_file`, `parquet_load_qc_maml_file`, `parquet_validate_maml`, `parquet_validate_user_maml`, `parquet_size_auto`, `parquet_read_qc` (`qc%add`, `qc%remap_column_names`), `parquet_compose_read_qc` — plus the `parquet_schema` type-bound builders `schema%init` / `schema%is_init` / `schema%is_parsed` / `schema%clear`, `schema%add_field`, `schema%add_field_from`, `schema%add_metadata` / `schema%clear_metadata`, `schema%add_col_qc` / `schema%set_col_qc`, `schema%set_column_available` / `set_column_unavailable`, `schema%set_col_size` / `set_array_size`, `schema%set_protected`, `schema%get_column_index` / `is_column_set` / `get_num_fields` / `get_field_name` / `get_field`, `schema%print_schema_info`

**Writer:** `parquet_open_writer`, `parquet_write_column`, `parquet_close_writer`, `parquet_get_chunk_size`

**Writer — streaming row groups:** `parquet_new_row_group`, `parquet_write_column_chunk`, `parquet_finish_row_group` — an alternative to `parquet_write_column` for a column too large to hold as one complete array; see [Streaming/chunked writes](doc/pages/io/writing.md#streamingchunked-writes)

**Writer — row filtering:** `parquet_write_row_mask`, `parquet_write_chunk_row_mask` — drop rows entirely (no trace, not a Null) from what gets written; see [Filtering rows with a mask](doc/pages/io/writing.md#filtering-rows-with-a-mask)

**Reader — table & column info:** `parquet_open_reader`, `parquet_close_reader`, `parquet_get_nrows`, `parquet_get_num_row_groups`, `parquet_get_col_size`, `parquet_measure_list_width`, `parquet_column_width_needs_data`, `parquet_column_has_nulls`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_get_column_time_info`, `parquet_column_exists`, `parquet_get_column_type`, `parquet_get_column_nullable`, `parquet_get_column_names`, `parquet_prefetch_columns`, `parquet_release_column`, `parquet_get_metadata`, `parquet_get_metadata_items`, `parquet_get_qc_columns`, `parquet_get_physical_row_indices`, `parquet_filter` (`filt%add`, `filt%remap_column_names`), `parquet_reader_set_filter` (whole-file, scoped to a row-group range, or bounded to a physical row range), `parquet_sortkey` (`srt%add`, `srt%remap_column_names`), `parquet_reader_set_sort`, `parquet_reader_adopt_transform`

**Reader — column data:** `parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`

**Reader — streaming row groups:** `parquet_read_column_chunk`, `parquet_get_chunk_size` — an alternative to `parquet_read_column` for a column too large to hold as one complete array; see [Streaming/chunked reads](doc/pages/io/reading.md#streamingchunked-reads)

**Compact string columns:** `parquet_string_column` — a scalar `string` column can be written/read via `parquet_write_column`/`parquet_read_column` (and their chunked counterparts) as a `parquet_string_column` instead of a padded `character(len=...)` array, with no pre-sizing needed; append/search/mutate with `append_string`/`append_null`/`append_from`/`find`/`set`/`erase`, read one element without allocating via `copy_to`, and extract zero-copy `parquet_string` handles with `view`/`view_all`/`view_slice` or gather them back with `build_from`; fill or extend a whole column from a `character(len=*)` array in bulk with `build_from`/`append_values` (trailing blanks trimmed, optional null mask); `parquet_string_threads` reports how many threads one bulk operation would use here (capped by `parquet_set_string_threads`); see [Reading and writing compact string columns](doc/pages/types/string-columns.md#reading-and-writing-compact-string-columns)

**Whole tables in memory:** `parquet_table` (also available on its own as `use parquet_tables`) — `parquet_open_table` presents a file's columns as one named table, reading each column's values on first use (or a contiguous row range of it, with `parquet_open_table(t, file, row_lo, row_hi)`), optionally filtered/sorted/sampled/qc-checked as it opens (`filter=`/`sort=`/`qc=` in the table's own column names, `sample_fraction=`/`sample_seed=`/`qc_soft=`) and optionally described by a read-in MAML (`maml=`, carrying `extra: remap:`/`filter:`/`sort:` and `fields: qc:` in the file's own names), `parquet_new_table` starts an empty one, and `parquet_write_table` writes a table back out through an ordinary `parquet_schema` (or, with no schema at all, writes whichever columns are resident and can emit a sidecar `.maml` describing them); reach a column with `%get` (copies, widening int32→int64 and float32→float64) or `%col` (zero-copy typed pointer, exact kind — including to a `parquet_string_column`'s packed store), one row with `%row` (`parquet_table_row`, whose `%get`/`%set`/`%ref` read, write and alias that row), one column with `%column` (`parquet_table_col`, a handle taking a name or a 1-based position, whose `%get`/`%set` reach one cell without a name lookup, `%get(i, e, v)`/`%set(i, e, v)` reach one element of a vector row without materialising it, `%ref` gives `%col`'s pointer, and `%is_null`/`%set_null`/`%clear_null`/`%name`/`%kind`/`%width`/`%unit`/`%index`/`%residency`/`%is_valid` complete it — both handles refuse to be used after a structural change rather than reading the wrong row or column), one cell with `%get_element`, a row selection with `%get_slice`/`%set_slice` (`parquet_slice`, built by `parquet_slice_range`/`parquet_slice_list`); `%get`/`%col`/`%get_slice`/`%set` all take an optional `is_valid=` carrying validity alongside the values, shaped like them (per row for a scalar column, per element `(width, nrows)` for a vector one); build one with `%add_column` (a Fortran array, a `parquet_string_column`, or a whole `parquet_column` — the last covers every kind and width in one call and is the way to hand over a column grown incrementally or carrying per-element nulls), replace values with `%set` or one cell with `%set_element`, control reads with `%materialize(names)`/`%prefetch(names)` (either name, one call: a comma/semicolon-separated string or an array; a struct's own name reads every leaf in one pass)/`%materialize_all`/`%reload`/`%evict_column` (both refuse a column you have written into unless `force=.true.` is passed, so local edits are not silently replaced by the file's own values; `%set_user_populated`/`%is_user_populated`, also on the column handle, claim or release a column whose values you wrote through a `%col`/`%ref` pointer, which the library cannot detect by itself), plan a parallel split with `parquet_table_row_group_bounds`/`%row_group_bounds`, reach each row's physical row number in the source file with the automatic `parquet_row_index` column (`PARQUET_ROW_INDEX`); and introspect with `%nrows`/`%nrows_unfiltered`/`%row_group_extent`/`%ncols`/`%column_names`/`%column_index`/`%column_name`/`%has_column`/`%require_columns`/`%missing_columns`/`%column_capacity`/`%kind`/`%width`/`%unit`/`%residency`/`%is_supported`/`%is_detached`/`%is_null`/`%has_nulls`/`%get_valid_mask`/`%generation`/`%filename`/`%get_file_metadata`/`%print_stat`/`%validate_qc`; change a table with `%set_null`/`%clear_null`/`%ensure_validity` (allocate validity storage up front, so several threads can null elements of one column)/`%compact_validity`, `%drop_column`/`%rename_column`/`%copy_column`/`%cast` (converts a column to another numeric kind in place, so `%col` takes the kind your code wants), and the row-changing (and so file-detaching) `%filter_rows`/`%sort_by`/`%top_n` (every key-taking binding takes its keys as an array or as one string, `"ra,-dec"`, in the same direction grammar `parquet_sortkey` uses) (keep only the `n` best rows, by selection rather than a full sort)/`%delete_rows`/`%truncate`/`%append`/`%append_null_rows`; give back the storage appending left behind with `%compact`, or ask for it up front with `%reserve` (neither changes the row set, so neither detaches, but both invalidate outstanding `%col` pointers); make room for COLUMNS with `%reserve_columns`, which comes with a guarantee the others do not — while spare capacity remains, `%add_column` under a new name relocates nothing and leaves an outstanding `%col` pointer, column handle and row handle valid; or ask for an order without applying one (and so without detaching) with `%argsort_by` (optionally reporting group boundaries via `group_offsets=`/`group_nkeys=`)/`%argsort_partial`/`%is_sorted_by`; copy one with `%clone`/`%clone_structure`; extend the type with your own by overriding `clone_extra` (so `%clone` carries your components) and binding declared columns with `%bind_predefined`; see [Whole tables in memory with `parquet_table`](doc/pages/tables/table.md)

**Generated table types:** `tools/generate_user_table_code.py` turns a MAML schema into a named `parquet_table` extension with one accessor per column — `t%ra()` for the whole column, `t%ra(i)` for one row and `t%ra(lo, hi)` for a row range, all zero copy — opened with `%init`/`%init_slice`/`%init_empty`; the generated module is user-editable in six marked windows and `--check` keeps it honest. `src/parquet_table_example.f90` (module `parquet_table_example`, type `parquet_table_test`) ships as a worked example of the output — nothing else in the library uses it, it is **not** re-exported by `use parquet`, and it is an example rather than API, so it is outside the stability promise below; see [Generated table types](doc/pages/utilities/generated-tables.md)

**Type-erased column storage:** `parquet_column` (also available on its own as `use parquet_columns`) — holds one whole column's values for any of 18 scalar/vector kinds behind a single `PK_*` kind discriminator, with sparse null tracking (a null-free column allocates no validity bitmap at all), an optional unit string, and the full value/structural instruction set: `init`/`clear`/`deep_copy`, `get_at`/`set_at`/`get_elem`/`set_elem` (one element of a vector row, with no width-long array built)/`set_all`/`data_ptr`/`append_values`, `is_null`/`any_null`/`row_validity`/`element_validity`/`set_null`/`clear_null`/`set_validity`/`compact_validity` (each of `is_null`/`set_null`/`clear_null` taking a row, or a row plus an element within it; `set_validity` taking a rank-2 `(width, nrows)` element mask or a rank-1 per-row one, and writing the bitmap a word at a time either way), `adopt` (take over an array without copying), `capacity`/`reserve`/`shrink_to_fit` (storage grows geometrically, so appending row by row is amortised O(1) rather than quadratic; reserve up front and shrink at the end when the final row count is known), and
`append`/`append_nulls`/`paste`/`delete_by_mask`/`reindex`/`gather` (keep the listed rows, in the listed order — the subset-and-reorder the other two do not cover between them); the shared foundation `parquet_table` and the future container column types are built on

**Sorting arrays and columns:** `pf_argsort`, `pf_sort`, `pf_permute`, `pf_is_sorted`, `pf_partial_sort`, `pf_partial_argsort`, `pf_nth_element`, `pf_nth_quantile`, `pf_lower_bound`, `pf_upper_bound`, `pf_equal_range`, `pf_unique_count`, `pf_unique`, `pf_rank`, `pf_minmax`, `pf_argminmax`, `pf_merge`, `pf_sort_threads` (also available on their own as `use parquet_sorting`) — sort a plain Fortran array, a `parquet_string_column` or a `parquet_column` on the same C++ engine that drives a read-time `sort_by=` and `parquet_table%sort_by`, so the three can never disagree about null placement, NaN placement or tie order; every sort is stable, `descending=`/`nulls_first=` select the order, the six types with no null state of their own take an optional `is_valid=` mask, `pf_permute` validates its permutation unless told `assume_valid=.true.`, and `pf_sort_keys` (`k%add`, `%nkeys_added`, `%clear`) accumulates keys of any mix of types for a multi-key sort, usable with `pf_argsort`, `pf_is_sorted` and `pf_partial_argsort` alike; `pf_argsort` can also report where each run of equal rows begins (`group_offsets=`, with `group_nkeys=` to group on a prefix of the keys while still sorting by all of them); and the selection operations answer "which element ends up here?" without ordering the whole array — `pf_partial_sort`/`pf_partial_argsort` order only the first `n` (clamped to the array size, so no `min()` at the call site; "the last n" is `descending=.true.`), `pf_nth_element` reports the value and optionally the index a full *stable* sort would put at a given rank, and `pf_nth_quantile` takes a quantile on a **0-1 scale** over the non-null values, with `rounding=`"nearest"/"down"/"up" and an optional `n_null=`; see [Sorting arrays and columns](doc/pages/utilities/sorting.md)

**Random numbers:** `pf_random_at`, `pf_random32_at`, `pf_random_bits_at`, `pf_random_int_at`, `pf_random_fill_draws`, `pf_random_fill_streams`, `pf_random_seed`, `pf_random_key`, `pf_random_algorithm`, `pf_random_stream`, `pf_random_perm_at`, `pf_random_permutation`, `pf_random_subset`, `pf_random_resample`, `pf_random_perm_algorithm` (also available on their own as `use parquet_random`) — counter-based draws addressed by `(seed, i [, draw])` rather than by call order, so a value depends only on its coordinates and a parallel loop reproduces exactly under any OpenMP schedule or thread count, which no stateful generator can do at any speed; `pf_random_at`/`pf_random32_at` give `real64`/`real32` in `[0, 1)` (1.0 unreachable by construction, and the `real32` sequence is deliberately its own rather than a narrowing), `pf_random_bits_at` gives 64 raw bits with `pf_random_at` contractually equal to its top 53, `pf_random_int_at` is **exactly** unbiased at every width including above 2⁶³ and swaps `lo > hi` rather than failing, `pf_random_fill_draws` fills a rank-1 `real64`/`real32` array with consecutive draws of one stream (prefix-consistent, and cheaper than the equivalent scalar calls) while `pf_random_fill_streams` fills one draw of each of consecutive streams — the bulk form of the `x(i) = pf_random_at(seed, i)` loop — and both fills also take an `integer` array with the range given as two further arguments; `pf_random_stream` is a stateful walk along one stream (`%seed`/`%uniform`/`%uniform32`/`%bits`/`%int_range`/`%fill`/`%jump`/`%rewind`/`%position`) for the case where how many values you need is data-dependent, handing out exactly the values the coordinate-addressed calls give; and `pf_random_key` derives independent seed families that compose by nesting; `pf_random_perm_at(seed, m, k)` extends the same idea to **permutations** — element `k` of a permutation of `1 .. m` in constant time and constant memory, so the first `n` values are a uniform random `n`-subset drawn without replacement and sampling 1000 rows out of a trillion reads 1000 elements rather than shuffling a trillion, with `pf_random_permutation`/`pf_random_subset` as its bulk forms (optional `threads=`, bit-identical at every thread count), while `pf_random_resample` draws **with** replacement — the bootstrap's sample, with an optional `stream=` naming the replicate, and no `size(idx) <= m` bound because there is none; `pf_random_algorithm` and `pf_random_perm_algorithm` name the two frozen bit contracts, and the generator is **not cryptographic**; see [Random numbers](doc/pages/utilities/random.md)

**Date/time/timestamp columns:** `parquet_date`, `parquet_time`, `parquet_timestamp` — read/written via `parquet_write_column`/`parquet_read_column` (and their chunked/row-mode/element-mode counterparts) exactly like any other type; `parquet_get_column_time_info` reads back a column's stored unit/timezone; `parquet_unit_seconds`/`parquet_unit_millis`/`parquet_unit_micros`/`parquet_unit_nanos` select a unit for `set_unix`/`to_unix`; `operator(-)`/`operator(+)` give same-type difference and integer-offset arithmetic, `%diff_seconds` a never-aborting real64-seconds difference, and `parquet_ns_per_sec`/`parquet_ns_per_day`/`parquet_ns_to_sec`/`parquet_ns_to_day` convert the raw nanosecond values they traffic in; see [Date, time and timestamp columns](doc/pages/types/date-time.md)

## Limitations

Worth knowing up front before relying on this library:

- **Writing** is limited to the six plain types in [Supported data types](doc/pages/types/supported-data-types.md) plus `date`/`time`/`timestamp` (see [Date, time and timestamp columns](doc/pages/types/date-time.md)) — there is no arbitrary nested/struct/map support, and no `INTERVAL`/duration type (a deliberately dropped non-goal, not a pending gap — see [Not yet supported](doc/pages/types/date-time.md#not-yet-supported) for why). **Reading** additionally accepts a column physically stored as `int8`/`int16`/unsigned integers/`half_float`/`decimal` (widened into `integer(int32)`/`integer(int64)`/`real(real32)`/`real(real64)` as appropriate; see [Reading a column into a different numeric kind](doc/pages/types/supported-data-types.md#reading-a-column-into-a-different-numeric-kind)) — this only ever arises from a file written by some other tool, since this library's own writer never produces those physical types. A `STRUCT` column's individual fields, at any nesting depth, can also be read directly via a dot-separated path (e.g. `"main.inner.age"`) passed as `name`, as long as the path resolves down to a scalar or vector (`FIXED_SIZE_LIST`) leaf — see [Reading a nested struct field](doc/pages/types/supported-data-types.md#reading-a-nested-struct-field); naming an intermediate struct directly is not readable, and `MAP` columns, and variable-length `LIST` columns nested inside a struct path, remain unsupported. `MAP`/`INTERVAL` columns remain unsupported on both sides; `qc:` bounds (`min:`/`max:`) are not yet supported for `date`/`time`/`timestamp` columns, though `qc: miss:` and `parquet_filter` rules on them are — see [Filtering date, time and timestamp columns](doc/pages/io/filter-sort-sample.md#filtering-date-time-and-timestamp-columns). Vector columns are fixed-length (`col_size`) only: every row must hold the same number of elements. Such a column can be *read back* whether it was stored on disk as a `fixed_size_list` or as a variable-length `list<element>` (see the [reading notes](doc/pages/io/reading.md)), but a genuinely ragged list (rows of differing length) is not supported — it is rejected on read — and this library's own writer only ever emits `fixed_size_list`.
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
