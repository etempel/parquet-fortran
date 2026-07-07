# parquet-fortran

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format). The metadata in the MAML file is converted to the VOTable style metadata in the parquet header.

**Features:**
- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character` (MAML: `boolean`/`string`) — as plain 1D columns or fixed-length vector columns (a vector column is read back whether it was stored on disk as a `fixed_size_list` or a variable-length `list`).
- Define and validate a table's schema and metadata from a [MAML](https://github.com/asgr/MAML-Format) file, including column renaming (`col_map:`), quality-control range checks (`qc:`), and protecting specific columns from ever containing a Null (`protected_cols:`). This is relevant only when *writing* — reading a parquet file never involves MAML.
- Read and write genuine Parquet Null values, with either substitution (`null_value=`) or a validity mask (`is_valid=`).
- Control output compression codec, compression level, and row group size.
- Safe to use concurrently (e.g. from OpenMP) — see [Thread safety](#thread-safety) for the exact rule.

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
See [Reading parquet files](#reading-parquet-files-from-your-fortran-code) and [Writing parquet files](#writing-parquet-files-from-your-fortran-code) below for the full picture, including MAML-driven schemas, vector columns, and the features listed above. You'll need Arrow/Parquet available and a couple of environment variables set to actually build against this library — see [Prerequisites](#prerequisites).

## Minimal setup to depend on this library

The Homebrew example below is illustrative, not independently verified end-to-end — if it doesn't match your platform or package manager (e.g. MacPorts, Linux, a manual Arrow build), treat [Environment variables](#environment-variables) as the authoritative, verified reference instead.

```bash
# 1) Install Arrow/Parquet C++ library (example: macOS Homebrew)
brew install apache-arrow

# 2) Point FPM to Arrow headers/libs -- example for the install above on macOS
#    (see Environment variables below for Linux and other details)
export LIBRARY_PATH="$(brew --prefix apache-arrow)/lib:$LIBRARY_PATH"
export FPM_FFLAGS="-I$(brew --prefix apache-arrow)/include"
export FPM_CXXFLAGS="-std=c++20 -stdlib=libc++ -I$(brew --prefix apache-arrow)/include"
export FPM_LDFLAGS="-L$(brew --prefix apache-arrow)/lib -lc++"

# 3) In your own fpm.toml, add parquet-fortran + link = ["arrow", "parquet", "c++"]
# 4) Build/test your project
fpm test
```

## Important behavior

- Most failures are reported via Fortran `error stop` and abort the process immediately.
- There are no status/`ierr` return codes in the public API.
- Some lower-level Arrow/Parquet failures may abort via C++ rather than `error stop`.

See [Error handling](#error-handling) and [Limitations](#limitations) for full details.

## Contents

- [Quick example](#quick-example)
- [Minimal setup to depend on this library](#minimal-setup-to-depend-on-this-library)
- [Important behavior](#important-behavior)
- [Prerequisites](#prerequisites)
  - [Environment variables](#environment-variables)
- [Embedding your own MAML schemas in your own project](#embedding-your-own-maml-schemas-in-your-own-project)
- [Reading parquet files from your fortran code](#reading-parquet-files-from-your-fortran-code)
  - [Reading only touches the columns you ask for](#reading-only-touches-the-columns-you-ask-for)
  - [Prefetching multiple columns at once](#prefetching-multiple-columns-at-once-with-parquet_prefetch_columns)
  - [Printing reader statistics](#printing-reader-statistics-with-parquet_close_readerprint_stattrue)
  - [Row filtering with parquet_filter](#row-filtering-with-parquet_filter)
  - [Read-time quality control with a qc-maml](#read-time-quality-control-with-a-qc-maml)
- [Writing parquet files from your fortran code](#writing-parquet-files-from-your-fortran-code)
  - [Saving the source MAML alongside the parquet file](#saving-the-source-maml-alongside-the-parquet-file)
- [The MAML metadata format](#the-maml-metadata-format)
  - [Renaming columns for output with `col_map:`](#renaming-columns-for-output-with-col_map)
- [Combined example: MAML schema, vector columns and metadata](#combined-example-maml-schema-vector-columns-and-metadata)
- [Error handling](#error-handling)
- [Thread safety](#thread-safety)
- [Supported data types](#supported-data-types)
  - [Null values](#null-values)
  - [Quality control (qc:) range checks on write](#quality-control-qc-range-checks-on-write)
  - [Compression and row group size](#compression-and-row-group-size)
  - [Multi-threaded decoding/encoding (use_threads) and thread pool size](#multi-threaded-decodingencoding-use_threads-and-thread-pool-size)
- [Performance and memory](#performance-and-memory)
  - [Combined example: Nulls, quality control and compression together](#combined-example-nulls-quality-control-and-compression-together)
- [parquet module API (functions/subroutines)](#parquet-module-api-functionssubroutines)
  - [Utility](#utility)
  - [MAML and metadata](#maml-and-metadata)
  - [Writer](#writer)
  - [Reader (table and column info)](#reader-table-and-column-info)
  - [Reader (reads column data)](#reader-reads-column-data)
- [Limitations](#limitations)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [License](#license)

## Prerequisites

The code compiles successfully with the following compilers and libraries. It might compile with previous or later versions as well but this is not tested.

- Fortran compiler:
    - Intel Fortran (ifx) v2025.3.0
    - Gfortran v15.2.0
- FPM ([Fortran Package Manager](https://fpm.fortran-lang.org/))
- [apache-arrow](https://arrow.apache.org) (C++ library for parquet)

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

For genuine multi-threaded (OpenMP) use, the compiler's OpenMP flag (e.g. `-fopenmp` for gfortran, `-qopenmp` for ifx) is also required, supplied via `FPM_FFLAGS` — see [Thread safety](#thread-safety) for why.

To build/test this repository itself (as opposed to depending on it from your own project), see [CONTRIBUTING.md](CONTRIBUTING.md).

## Embedding your own MAML schemas in your own project

`tools/generate_parquet_maml.sh` (bundled with this library) is a generic tool any project depending on `parquet-fortran` can reuse to embed *its own* `.maml` schemas directly into compiled Fortran source, so a downstream pipeline doesn't need to locate/ship `.maml` files at run time. To do this in your own project:

1. Copy `tools/generate_parquet_maml.sh` into your own project (e.g. under your own `tools/`).
2. Put your own `.maml` schema files under a `docs/` directory at your project's root.
3. Run it with no argument from your project's root: `tools/generate_parquet_maml.sh` — this writes `src/parquet_maml.f90` in *your* project, generated from *your* `docs/*.maml` files.
4. `use parquet_maml` (the module the script just generated for you) alongside `use parquet` in your code, to call `get_parquet_maml("your_schema.maml")` or `set_maml(...)` the same way this library's own tests do internally.

The generated `parquet_maml` module depends on `parquet` (`parquet_maml_file`, `parquet_validate_maml`, etc.) but not on `parquet_maml_base` directly — `parquet_maml_base` is this library's own internal module and isn't meant to be `use`d directly by consuming projects. (If you're contributing to `parquet-fortran` itself and need to regenerate its own built-in schema module, see [CONTRIBUTING.md](CONTRIBUTING.md).)

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
- Allocate output arrays before calling `parquet_read_column`. Its row count (`size(values)` for a plain column, `size(values, 2)` for a vector column) must match `parquet_get_nrows` exactly, or it fails immediately with `error stop`, naming the column and both row counts.
- `name` must be a column that actually exists in the file — this is checked for every procedure that takes a column name (`parquet_read_column`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`), each failing immediately with `error stop` naming the missing column.
- For string columns, choose a fixed string length that is large enough for your data.
- For vector columns, allocate 2D arrays with shape `(col_size, nrows)`.
- A vector column may be stored on disk either as a `fixed_size_list` (what this library's own writer emits) or as a variable-length `list<element>` (the Parquet `LIST` layout many other producers use — including its legacy 2-level and non-standard inner-element-name variants, which Arrow's reader normalizes to the same `list` type); both are read back identically. The only requirement is that every row's vector has the same length (so it fits the `(col_size, nrows)` shape); a genuinely ragged `list` column (rows of differing length) is rejected with `error stop`. `col_size` is inferred from the data in either case.

### Reading only touches the columns you ask for

`parquet_open_reader` only parses the file's footer (schema, row count, row-group layout) — it does not read or decompress any column's actual data. Each column is only read from disk the first time you ask for it (`parquet_read_column`, `parquet_get_col_size`, `parquet_get_string_length`, etc.); after that, it's cached in memory for the lifetime of that `reader`, so asking for the same column twice doesn't re-read it. Columns you never ask for are never read at all. This is a direct consequence of Parquet's on-disk layout — each column's data is stored as its own contiguous byte range, independent of every other column, so the reader can seek straight to just the bytes it needs — and holds regardless of which [compression codec](#compression-and-row-group-size) was used to write the file. Practically: opening a large file with many columns and reading only a handful of them is cheap, both in I/O and memory, no matter how large the *other*, unrequested columns are.

### Prefetching multiple columns at once with `parquet_prefetch_columns`

`parquet_prefetch_columns(reader, names)` reads several named columns in one call, filling the same per-column cache that `parquet_read_column` would otherwise populate lazily, one column at a time, on first use. Since Arrow's internal `use_threads` is always on for this library (see [Compression and row group size](#compression-and-row-group-size)), reading several columns together like this lets Arrow decode them across its internal thread pool concurrently, instead of strictly one column at a time as each is lazily requested — it's purely a throughput optimization on top of the existing lazy-read design, never a requirement: a column you don't pass to `parquet_prefetch_columns` still works exactly as before, via `parquet_read_column`'s normal lazy, read-on-first-request path. Every name is validated against the file's actual schema before any data is read; a name that isn't a real column in the file fails immediately with `error stop`, naming the offending column. Calling `parquet_prefetch_columns` more than once (e.g. with a column set that partially overlaps an earlier call) is safe and efficient: columns already cached from an earlier prefetch, or already read lazily, are not re-read — only the columns not yet cached are fetched, so the cache after several calls holds the union of every column named across all of them.

```fortran
call parquet_open_reader(reader, "data.parquet")
call parquet_prefetch_columns(reader, ["ra ", "dec", "mag"])
! call parquet_read_column for these (in any order) and any other, non-prefetched column:
call parquet_read_column(reader, "ra", ra)
call parquet_read_column(reader, "id", id)   ! fine even though "id" was never prefetched
```

Note that all names in the `names(:)` array argument must share the same declared string length (pad shorter names with trailing spaces, as with any other Fortran character array literal).

### Printing reader statistics with `parquet_close_reader(..., print_stat=.true.)`

`parquet_close_reader(reader, print_stat=.true.)` prints a debug/diagnostic summary of the reader's activity to stdout, right before actually closing it. `print_stat` is optional and defaults to `.false.` (no output).

The summary has two parts:
- Table-level: the filename, the total number of columns in the file, how many of those are actually shown below (see below), and the total row count.
- One row per column that was either prefetched (`parquet_prefetch_columns`) or actually read (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`) at some point during the reader's lifetime — a column never touched at all is left out of the list entirely, rather than decoding it just to fill in a report:

  | column | meaning |
  |---|---|
  | `col` | Column name. |
  | `parquet_type` | The column's physical type in the Parquet file (e.g. `int32`, `double`, `string`, `list<double>` for a vector column). |
  | `output_type` | The Fortran-side type most recently used to read this column (e.g. `float64`, `int32`, `string`); blank if the column was only ever prefetched, never actually read. |
  | `col_size` | The vector length for a fixed-length vector column; blank for a plain scalar column. |
  | `len_str` | For a `string` column only (blank otherwise): the longest string's length in the Parquet file, and, if the column was actually read via a `character(len=...)` array, the allocated output length after a `/` (e.g. `18` if they match, `18 / 24` if the Fortran buffer was allocated longer than necessary). |
  | `nulls` | Number of genuine Parquet Nulls in the column (every element, flattened, for a vector column). |
  | `min` / `max` | Numeric/string columns: the minimum/maximum value (lexicographic for strings). Boolean columns: `T:<count>`/`F:<count>` instead, since a boolean's min/max isn't a meaningful summary. `-`/`-` if every value is Null. |
  | `prefetc` | `yes` if this column was named in a `parquet_prefetch_columns` call, `no` otherwise. |
  | `read` | `yes` if this column was actually read via a typed read call, `no` otherwise (e.g. prefetched but never read). |

```fortran
call parquet_open_reader(reader, "data.parquet")
call parquet_prefetch_columns(reader, ["ra ", "mag"])
call parquet_read_column(reader, "ra", ra)
call parquet_close_reader(reader, print_stat=.true.)
```

Requires linking Arrow's `arrow_compute` library (for the min/max calculation) in addition to `arrow`/`parquet` — see [Environment variables](#environment-variables); `parquet-fortran`'s own `fpm.toml` already declares this link, and fpm propagates it to consuming projects automatically, so no action is normally needed.

### Row filtering with `parquet_filter`

`parquet_open_reader(reader, filename, filter=filt)` restricts a reader to only the rows matching a `type(parquet_filter)`. Once a filter is set, it is completely transparent to everything else: `parquet_get_nrows`, `parquet_read_column`, `parquet_prefetch_columns`, and `parquet_close_reader(..., print_stat=.true.)` all behave exactly as if the file only ever contained the matching rows — there is no separate "filtered count" to track yourself.

```fortran
type(parquet_filter) :: filt
type(parquet_reader) :: reader
integer(int32) :: nrows
integer(int32), allocatable :: ra(:)

call filt%add("ra > 180")
call filt%add("ra <= 360")
call filt%add("id is_not_null")

call parquet_open_reader(reader, "data.parquet", filter=filt)
call parquet_get_nrows(reader, nrows)      ! already the filtered row count
allocate(ra(nrows))
call parquet_read_column(reader, "ra", ra) ! already just the matching rows
call parquet_close_reader(reader)
```

Each `filt%add(rule)` call adds one clause; multiple clauses always combine with AND (there is no OR/NOT) — call `%add` more than once, as above, to express a range or several independent conditions. A `rule` has the shape `"<column> <op> <value>"` or `"<column> is_null"` / `"<column> is_not_null"`:

- Supported operators: `>`, `>=`, `<`, `<=`, `==`, `/=`, `is_null`, `is_not_null`.
- `<value>` is a bare number for a numeric column (`ra > 180`), `true`/`false` for a boolean column (`flag == true`), or a **double-quoted** string for a `string` column (`name == "abell_1"`) — quotes are required for strings and not used for anything else.
- `is_null`/`is_not_null` take no value.
- Only plain scalar columns can be filtered — naming a vector (`col_size > 1`) column in a rule fails immediately with `error stop` when `parquet_open_reader` is called. So does naming a column that doesn't exist in the file, or a rule with invalid syntax (unknown operator, unquoted string value, non-numeric value against a numeric column, etc.) — every rule is fully validated (column existence, type-compatibility, and value parsing) right there in `parquet_open_reader`, before any of your own code runs.

Filtering is **not** predicate pushdown: every filter-referenced column, and every column you subsequently read, is still fully read and decoded from disk exactly as without a filter (Parquet row-group statistics are never used to skip I/O). The benefit is entirely downstream: `parquet_get_nrows` and every column you read only ever reflect the matching rows, so your own code loops over, allocates for, and processes far fewer rows when the filter is selective — at the cost of a small transient memory bump while a column's full decoded array and its filtered result briefly coexist, before the unfiltered one is discarded.

### Read-time quality control with a qc-maml

`parquet_open_reader(reader, filename, maml=..., qc=..., qc_soft=...)` checks column values against `qc: min:`/`max:`/`miss:` bounds declared in a MAML file — mirroring the writer's own [`qc:` range-check feature](#quality-control-qc-range-checks-on-write), but on the read side. By default a violation is a **hard error** (`qc_soft=.false.`): the process aborts with a diagnostic on stderr, the same class of clean, deliberate abort as the read-side Null/type-mismatch checks (see [Limitations](#limitations)). Pass `qc_soft=.true.` to instead **warn and continue**: a `WARNING` is printed to stdout and reading proceeds. Either way, the existing strict-by-default Null behavior (`error stop` on a genuine Null unless `null_value=`/`is_valid=` is passed — see [Null values](#null-values)) is completely unchanged.

```fortran
type(parquet_reader) :: reader
integer(int32), allocatable :: ra_back(:)

! qc.maml:
!   fields:
!   - name: ra
!     qc:
!       min: 0
!       max: 360
!   - name: id
!     qc:
!       miss: Null   ! this column is expected to contain genuine Nulls

! Default: a range violation aborts the process.
call parquet_open_reader(reader, "data.parquet", maml=parquet_load_qc_maml_file("qc.maml"))
call parquet_read_column(reader, "ra", ra_back)
! aborts: parquet-fortran: qc hard check: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)

! qc_soft=.true.: the same violation only prints a WARNING to stdout, then continues.
call parquet_open_reader(reader, "data.parquet", maml=parquet_load_qc_maml_file("qc.maml"), qc_soft=.true.)
call parquet_read_column(reader, "ra", ra_back)
! prints: WARNING: qc violation for column 'ra' (based on incomplete column information): declared min >= 0, max <= 360, data range [...], N of M valid element(s) out of range
call parquet_close_reader(reader)
```

- `maml` is optional, `type(parquet_maml_file)`; `parquet_load_qc_maml_file(filename)` loads one from disk (a separate function from `parquet_load_maml_file`, since a qc-maml has different, lighter requirements — see below). `qc` is optional `logical`: if omitted, it defaults to `.true.` whenever `maml` is supplied and `.false.` otherwise; an explicit `qc=` always wins (so `qc=.false.` with a `maml=` present disables checking entirely, and `qc=.true.` with no `maml=` at all is a harmless no-op, nothing to check).
- `qc_soft` is optional `logical`, default `.false.` (hard: a violation aborts the process). It only ever takes effect when qc is active; with `qc=.false.` (or no `maml=`) it is irrelevant.
- A qc-maml's only required field attribute is `name` — `data_type` and everything else (including `qc:` itself) are optional, unlike a schema-authoring MAML. `qc: min:`/`max:` bounds are parsed against the column's actual Parquet type at read time, not any `data_type` the maml might declare. A qc-maml may declare fields that don't exist in the parquet file at all (they're silently ignored) or that already have a value in the file's own physical type different from the maml — validation only requires that field names not repeat, that a `qc: miss:` value (if present) is `Null`/`NA` (case-insensitive) or empty, and that any `qc: min:`/`max:` operator points the right way (`min:` a lower bound with `>=`/`>`, `max:` an upper bound with `<=`/`<` — the same rule the [write side](#quality-control-qc-range-checks-on-write) enforces; a reversed operator aborts `parquet_open_reader`).
- `qc: miss: Null` (or `NA`) means Nulls are *expected* for that field: no violation is raised if the column contains one. Leaving `miss:` empty or omitting it (the default) means Nulls are *not* expected: reading a Null in that column is a violation — regardless of whether you also pass `null_value=`/`is_valid=` to actually read it. A field with no `qc:` block at all (just a bare `name:`) gets no checking whatsoever, the same as a field never mentioned in the maml.
- qc only ever runs for a column this reader actually touches — read (`parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode`, scalar or vector), prefetched (`parquet_prefetch_columns`), or referenced by a [`parquet_filter`](#row-filtering-with-parquet_filter) — and, when a filter is active, only ever sees the already-filtered rows. Boolean columns skip the min/max check entirely (never meaningful there) but still get the Null-presence check.
- In soft mode, each of the two violation categories (Null-presence, range) prints **at most once per column** for the whole lifetime of the reader, even if that column is read multiple times (in hard mode the first violation aborts, so this never comes up); the message notes it's based on incomplete (whatever's been decoded so far) column information.
- Requires linking `arrow_compute` (see [Printing reader statistics](#printing-reader-statistics-with-parquet_close_readerprint_stattrue) above) — same as `print_stat`, and for the same reason (min/max calculation).

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

If `cinfo` is omitted, `parquet_open_writer` does not enforce a fixed schema: each column's type, string length and array size are inferred from the first `parquet_write_column` call that writes it. If `cinfo` is given, only columns marked `is_set = .true.` (see `set_available`/`set_unavailable` below) are written, and calling `parquet_write_column` with a name that is not in `cinfo` fails immediately with `error stop`.

`cinfo` and `metadata` are independent optional arguments to `parquet_open_writer` — pass either, both, or neither. Each governs a different part of the parquet file's VOTable-style header: `cinfo` supplies each column's own `unit`/`info`/`ucd` attributes, while `metadata` supplies table-level entries (author, description, `keyarray:`, etc. — see [The MAML metadata format](#the-maml-metadata-format)). If `metadata` is omitted, no table-level metadata is written to the file, regardless of whether `cinfo` is given.

### Saving the source MAML alongside the parquet file

Pass `write_maml=.true.` to `parquet_open_writer` to also save a sidecar `.maml` file next to the parquet output — same path, with a trailing `.parquet` replaced by `.maml` (or `.maml` appended if there is none):

```fortran
call parquet_read_maml("maml_example.maml", cinfo, metadata)
call parquet_open_writer(writer, "data.parquet", cinfo, metadata, write_maml=.true.)
! writes data.parquet and data.maml
```

This requires `metadata` to come from `parquet_read_maml` — it saves the verbatim MAML source that was parsed into `metadata`, not the parquet file's parquet/VOTable-style header. Two things follow from that:

- Calls to `metadata%add_metadata` made *after* `parquet_read_maml` (to add extra runtime metadata, as in the [combined example](#combined-example-maml-schema-vector-columns-and-metadata) below) each append a new `keyarray:` entry (`key`/`value`/`comment`) to the saved `.maml`, so runtime metadata is reflected in the sidecar too. Entries are always appended, even if a `keyarray:` entry with the same key already exists — the sidecar will then contain both. A `keyarray:` header is added automatically if the source MAML didn't have one, placed before `extra:` if present, else before `fields:`.
- The saved `.maml`'s `fields:` section only lists columns that are enabled (`cinfo%col(:)%is_set`) at the time `parquet_open_writer` is called, i.e. what actually ends up in the `.parquet` file: entries for columns disabled via `set_unavailable`, or excluded from a user MAML subset via `parquet_validate_user_maml`, are removed from the sidecar. Matching is by column name against the `cinfo` passed to `parquet_open_writer`; every other section (table-level metadata, `keyarray:`, etc.) is left untouched. If disabling columns would leave zero fields, pruning is skipped entirely and the full field list is kept instead, since a MAML file with no fields cannot be read back by `parquet_validate_maml`.

Omitting `write_maml`, or passing `write_maml=.false.`, behaves exactly as before (no sidecar file). Passing `write_maml=.true.` without `metadata`, or with `metadata` that wasn't produced by `parquet_read_maml`, fails immediately with `error stop`.

Notes:

- Every call to `parquet_write_column` writes one full column.
- All columns in one file must contain the same number of rows. The first `parquet_write_column` call fixes the row count for the whole file; any later call with a different row count fails immediately with `error stop`, naming the column and both row counts.
- Close the writer with `parquet_close_writer` to flush data and finalize the file.

## The MAML metadata format

A [MAML](https://github.com/asgr/MAML-Format) file is YAML. Table-level metadata (author, description, ...) is given as top-level keys, and column definitions are given as a list under the `fields:` key. **Only a fixed, known set of top-level keys is accepted** — `survey`, `dataset`, `table`, `version`, `date`, `author`, `coauthors`, `dois`, `depends`, `description`, `comments`, `license`, `keywords`, `maml_version`, `keyarray`, `extra`, and `fields` (matched case-insensitively; see `allowed_maml_sections` in `src/parquet_metadata.f90`) — any other top-level key fails `parquet_validate_maml` as an unknown section. To attach your own custom metadata not covered by that list, nest it under `extra:` instead, which accepts arbitrary structure unvalidated (see [Renaming columns for output with `col_map:`](#renaming-columns-for-output-with-col_map) for an example of `extra:`'s own nested keys). `parquet_read_maml` always runs this same validation before parsing a MAML into `cinfo`/`metadata`, so an invalid MAML is caught immediately rather than silently parsed. Three full worked examples are checked into the repository under `docs/`:
- [docs/maml_example.maml](docs/maml_example.maml) — the base example used throughout this README.
- [docs/maml_example2.maml](docs/maml_example2.maml) — adds `string` fields and `qc: min:`/`max:` bounds (both the plain-number and the quoted-operator forms).
- [docs/maml_example3.maml](docs/maml_example3.maml) — adds `extra: col_map:` column renaming (e.g. `id` → `uberid`, `RA` → `ra_J2000`) alongside `qc:`.

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

- `name` and `data_type` are required for every field; `data_type` must be one of the [supported types](#supported-data-types)' MAML names.
- `col_size` (default `1`) makes the column a fixed-length vector column, read/written as a 2D array of shape `(col_size, nrows)`.
- `array_size` sets the maximum string length for `string` columns; it is ignored for other types.
  > Don't confuse `col_size` with `array_size` — despite the similar-sounding names, they're unrelated: `col_size` is how many elements a vector column's row holds, `array_size` is how many characters a `string` column's values can hold.
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

- Each `col_map:` item is `<internal_name>: <output_name>`. `parquet_validate_user_maml` checks these are unambiguous — each `internal_name` must exist in the base schema and not also appear un-renamed elsewhere in `fields:`; each `output_name` must actually be declared in this MAML's own `fields:` and not collide with another column's name. A violation fails validation with a message naming the specific problem.
- The renamed field's `fields:` entry (`my_id` above) is validated exactly like any other field entry (`data_type` required, etc.) — nothing is inherited from the base column's own attributes.
- After `parquet_read_maml`, the resulting `parquet_column_type` always uses the internal name (`id0`) for `cinfo%col(:)%name` — the same name every other API (`parquet_write_column`, `set_available`, `get_column_index`, ...) already expects — with the rename available separately as `cinfo%col(:)%output_name` (`my_id`), which is what actually gets written to the `.parquet` file's schema/VOTable header and to a `write_maml=.true.` sidecar's `fields:` section.
- `user_maml%col_map` (populated by `parquet_validate_user_maml`) exposes the parsed entries for inspection.
- Since it lives inside `extra:`, `col_map:` does not produce any table-level metadata entry of its own (nor does `protected_cols:`, `extra:`'s other specifically-parsed key — see [Null values](#null-values)); anything else nested inside `extra:` is accepted unvalidated and otherwise unused.

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
| `extra:` | Opaque to table-level metadata (produces no metadata entry of its own), but not ignored: `col_map:` and `protected_cols:` are specifically parsed out of it — see [Renaming columns for output with `col_map:`](#renaming-columns-for-output-with-col_map) and [Null values](#null-values). Anything else nested inside `extra:` is accepted unvalidated and otherwise unused. |

In short: validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks. (If you're contributing to `parquet-fortran` itself and want to extend its MAML structure, see [CONTRIBUTING.md](CONTRIBUTING.md#extending-the-maml-schema).)

## Combined example: MAML schema, vector columns and metadata

This example ties together MAML-driven column definitions, a vector column, dropping an optional column at runtime, and adding extra table metadata not present in the MAML file.

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

This library reports all failures (missing files, invalid MAML, unknown column names, type mismatches, etc.) by calling Fortran's `error stop`, which aborts the running program immediately and cannot be caught or recovered from. There are no status/`ierr` return codes — check inputs (file existence, column names, array bounds) before calling into the library if you need to avoid aborting. If you're contributing to this library and need to add or test one of these failure paths, see [CONTRIBUTING.md](CONTRIBUTING.md) for how that's done out-of-process, and for this project's own conventions around C++-level error reporting.

Calling any reader-taking procedure (`parquet_read_column`, `parquet_prefetch_columns`, `parquet_get_nrows`, `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_get_string_length`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`) before `parquet_open_reader`, or `parquet_write_column` before `parquet_open_writer`, fails with `error stop`, naming the missing open call. The same applies to `parquet_close_reader`/`parquet_close_writer` themselves: closing a reader/writer that was never opened, or that was already closed, also fails with `error stop` rather than silently doing nothing.

Re-opening an already-open `reader`/`writer` variable (calling `parquet_open_reader`/`parquet_open_writer` again on one that's already in use, without closing it first) is safe and does **not** error: Fortran automatically finalizes (cleanly closes) the previous handle first, since `reader`/`writer` are `intent(out)` arguments of a finalizable type. The old file's handle is not leaked; the variable simply now refers to the newly-opened file.

## Thread safety

Concurrent use (e.g. from an OpenMP parallel region) is supported.

Rules at a glance:

- Each thread must use its own independent `parquet_writer`/`parquet_reader` instance.
- Never call into the same reader/writer instance from two threads at once.
- Independent readers may open/read the same parquet file concurrently.
- Never write to the same output file path from two threads at the same time.

Practical cases:

- Safe: many threads, each opening/writing/closing its own `parquet_writer` to a different file.
- Safe: many threads, each opening/reading/closing its own `parquet_reader` — including multiple threads independently opening their own reader on the *same* file at the same time (each thread's `parquet_open_reader` call is independent).
- Safe: parsing MAML files (`parquet_read_maml`, `parquet_validate_maml`, etc.) concurrently across threads. Internally this path is lock-serialized for correctness, so it is thread-safe but not expected to speed up with more threads.
- **Not safe:** sharing a single `parquet_writer`/`parquet_reader` variable across threads (e.g. a module-level or `!$omp shared` instance that multiple threads call into at once).
- **Not safe:** two threads writing to the *same* output file at the same time, even with separate `parquet_writer` instances — the underlying file itself isn't safe to write from more than one place at once.

**Building for genuine multi-threaded use:** the OpenMP flag is compiler-dependent (`-fopenmp` for gfortran, `-qopenmp` for ifx, ...), so it can't be hardcoded in `fpm.toml`. If you call into this library concurrently from your own `!$omp parallel` regions, set your own project's `FPM_FFLAGS` (or equivalent) to include your compiler's OpenMP flag, e.g.:
```sh
export FPM_FFLAGS="-fopenmp"
```
Since this library compiles from source as a dependency, that flag also reaches `parquet-fortran`'s own compiled code, not just yours. Without it, the concurrency-related code paths described above silently run single-threaded rather than failing outright. (If you're developing `parquet-fortran` itself, see [CONTRIBUTING.md](CONTRIBUTING.md#testing-genuine-openmp-concurrency) for how this project's own tests exercise real concurrency.)

Calling into a *shared* `parquet_writer`/`parquet_reader` from more than one thread at a time (the "not safe" case above) is actively detected and rejected: the second concurrent caller triggers an immediate process abort (`std::abort()`) with a diagnostic on stderr. This is a fail-fast race guard, not a locking mechanism. Sequential, non-overlapping hand-off between threads remains allowed.

`use_threads` (see [Multi-threaded decoding/encoding](#multi-threaded-decodingencoding-use_threads-and-thread-pool-size)) is separate from OpenMP-level concurrency here. If you already parallelize with OpenMP across many readers/writers, consider `use_threads=.false.` (and/or `parquet_set_max_threads`) to avoid CPU oversubscription.

## Supported data types

The following intrinsic Fortran kinds (from `iso_fortran_env`) are supported throughout the write, read and metadata APIs, both as scalars/1D arrays and as 2D vector columns:

| Fortran kind         | MAML `data_type` |
|-----------------------|-------------------|
| `integer(int32)`      | `int32`           |
| `integer(int64)`      | `int64`           |
| `real(real32)`        | `float32`         |
| `real(real64)`        | `float64`         |
| `logical`             | `boolean`         |
| `character(len=*)`    | `string`          |

Vector column entries use the shape convention `(col_size, nrows)` for arrays passed to `parquet_write_column` or produced by `parquet_read_column`.

### Null values

Fortran has no per-element representation for a missing/Null value. On the **read** side, if a column contains any genuine Parquet Null (e.g. a file produced by another tool), the default behavior of `parquet_read_column`, `parquet_read_array_row_mode`, and `parquet_read_array_element_mode` is to abort the process immediately, rather than silently returning undefined data. This is a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_int32_column: column contains Null value(s), which is not supported: <column>`), not a Fortran `error stop` — the same class of failure as the physical-type-mismatch case in [Limitations](#limitations).

To read a Null-containing column instead of erroring, pass one or both of these optional keyword arguments (supported by all three of the read families above, for every data type):

- `null_value=nullval` — a scalar of the same type as `values`; every Null in the column is replaced with `nullval` in the returned `values`.
- `is_valid=mask` — a `logical` array of the same shape as `values`; `.false.` wherever the Parquet value was Null, `.true.` otherwise.

If only `is_valid` is given (no `null_value`), Null slots in `values` are still filled with a safe type-appropriate default (`0` / `.false.` / blank string) rather than left as undefined data — check `is_valid` to know which entries are real. If both are given, Nulls are replaced with `nullval` *and* `is_valid` still reports which entries were originally Null. Omitting both keeps the default strict (error-on-Null) behavior.

For vector columns, a slot is reported/treated as Null if either the whole row is missing or that specific element within the row is missing (Parquet's list columns track these independently); `is_valid(j, i)` reflects the combination of both.

`parquet_get_string_length` is unaffected by any of this: it always silently skips Nulls when computing the maximum string length, since sizing an output buffer shouldn't depend on how you plan to handle Nulls when reading.

On the **write** side, `parquet_write_column(writer, name, values, is_valid=mask)` accepts the same kind of `logical` mask (same shape as `values`, `.false.` = write a genuine Null there); there is no `null_value` on the write side, since a value used to *detect* a Null (rather than substitute one, as on read) would risk misclassifying a legitimate value that happens to equal the sentinel. Whatever is in `values` at a `.false.` slot is ignored — a real Parquet Null is written there regardless. For vector columns, `is_valid` is element-level only: an entire row's vector can never be Null, only individual elements within it. A column only becomes nullable in the file's schema if `is_valid` is actually passed and contains at least one `.false.` entry; omitting `is_valid` (or passing an all-`.true.` mask) writes exactly as before, keeping the column non-nullable.

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
Every name listed must be one of this same MAML file's own declared `fields:` (checked by `parquet_validate_maml`; unknown names fail validation). If a user MAML overrides a base MAML, `protected_cols:` is taken from whichever MAML is actually used to build the writer's schema (the user MAML if one is provided, otherwise the base MAML) — not merged across both. Writing an `is_valid` mask with any `.false.` entry for a protected column fails immediately with `error stop`. This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., cinfo, ...)`); a schema-less writer has no `protected_cols:` to enforce.

### Quality control (qc:) range checks on write

A MAML field can declare a `qc:` block with `min:`/`max:` bounds:
```
- name: ra
  data_type: float64
  qc:
    min: '>= 0'
    max: '< 360'
```
A plain number (`min: 1`) is treated as inclusive (`>=` for `min:`, `<=` for `max:`); a quoted value with an explicit leading operator uses that comparison instead. The operator must match the bound's direction: `min:` accepts only `>=` or `>` (a lower bound) and `max:` accepts only `<=` or `<` (an upper bound); a reversed operator (e.g. `min: '< 5'`) is a nonsensical bound and fails `parquet_validate_maml`. Either bound may be omitted (only `min:` or only `max:` is fine). `parquet_validate_maml` also checks that every declared bound actually converts to a value usable for that field's `data_type`: for `int32`/`int64` it must be an exact integer within that type's range; for `float32`/`float64` it must be finite (not `NaN`/`Infinity`); a `string` field's bound is used as a literal string (nothing to convert, so nothing can fail there); `qc:` on a `boolean` field is accepted but never enforced (silently ignored, and so exempt from the operator-direction check too).

Pass `qc=.true.` to `parquet_open_writer` to turn on the actual range check during writing:
```fortran
call parquet_open_writer(writer, "data.parquet", cinfo, metadata, qc=.true.)
```
With `qc=.true.`, every `parquet_write_column` call checks its column's declared bound(s) (if any) against every element for which `is_valid` is `.true.` (or every element, if `is_valid` wasn't passed at all — see [Null values](#null-values)). String columns are compared lexicographically using Fortran's native string comparison. Vector columns are checked element-wise. A violation **never stops the write** — it prints one `WARNING` line to stdout naming the column, its declared bound(s), the observed data range among the checked elements, and how many of them are out of range, e.g.:
```
WARNING: qc violation for column 'ra': declared min >= 0, max < 360, data range [-1.5, 359.9], 3 of 1000 valid element(s) out of range
```
This only applies when writing against a MAML-derived schema (`parquet_open_writer(..., cinfo, ...)`) and only for columns that actually declare `qc: min:`/`max:`; omitting `qc=.true.` (the default) skips the check entirely, same as before this feature existed.

### Compression and row group size

`parquet_open_writer` also accepts:
```fortran
call parquet_open_writer(writer, "data.parquet", compression="zstd", compression_level=9, chunk_size=100000)
```
- `compression` — one of `"uncompressed"`, `"snappy"` (the default), `"gzip"`, `"zstd"`, `"brotli"`, `"lz4"` (case-insensitive); an unrecognized name fails immediately with `error stop`. Note that Parquet-the-library's own built-in default is actually `"uncompressed"` — this library deliberately follows the ecosystem convention (pyarrow, Spark, ...) of defaulting to `"snappy"` instead, since writing uncompressed files was never an intentional choice, just an unset option. Rough guidance: `snappy`/`lz4` for fastest read/write at a modest size reduction; `gzip`/`brotli` for the smallest files at the cost of slower compression; `zstd` for the best all-round balance of the two (and the only one of these with a meaningfully tunable `compression_level`, roughly 1–22 for higher-ratio/slower).
- `compression_level` — optional integer tuning the chosen codec's compression level (mainly meaningful for `zstd`/`gzip`/`brotli`); omitted means "use that codec's own default level".
- `chunk_size` — **advanced/optional: most callers never need to set this.** It's the maximum number of rows per Parquet row group. If omitted (the default, and the recommended setting for normal use), it is auto-sized once `parquet_close_writer` runs, targeting a row group size of roughly 256 MiB based on the table's actual in-memory byte size — not a flat row count — so both a handful of narrow `int32` columns and a table with wide vector columns (large `col_size`) end up with sensibly-sized row groups without any tuning on your part, all the way up to very large files (hundreds of millions of rows). The auto-sized value is clamped between 1,000 and 10,000,000 rows per row group, so a pathologically wide or narrow row still produces something reasonable rather than an absurd number of tiny row groups or one single enormous one. Pass an explicit value only if you need to override this — e.g. to force multiple row groups in a small file for testing (as some of this library's own tests do), or to hand-tune the read/write memory-vs-overhead trade-off described below for a workload you've actually measured. Larger values reduce per-row-group overhead and can improve compression (more data for the compressor to find patterns in), at the cost of more per-row-group encoder state (dictionaries, statistics) held in memory while writing — see [Performance and memory](#performance-and-memory). Smaller values let *other* tools that support row-group-level pruning skip more of the file when reading it back; this library's own reads always materialize a full requested column regardless of row group count (see [Performance and memory](#performance-and-memory) again), since it doesn't do row-group-scoped or predicate-pushdown reads (see [Limitations](#limitations)).

### Multi-threaded decoding/encoding (`use_threads`) and thread pool size

Both `parquet_open_reader` and `parquet_open_writer` accept an optional `use_threads` (`logical`, default `.true.`):
```fortran
call parquet_open_reader(reader, "data.parquet", use_threads=.true.)
call parquet_open_writer(writer, "data.parquet", use_threads=.true.)
```
When `.true.` (the default), that reader/writer decodes or encodes column data across Arrow's internal CPU thread pool instead of a single thread — Arrow's own library default is actually `.false.`, so this library turns it on by default since the extra parallelism is normally a pure win. This is on a per-reader/per-writer basis: it costs nothing to leave it on, and there's no shared state to worry about between independent readers/writers.

The most common reason to pass `use_threads=.false.` is to avoid **oversubscription** when you're already parallelizing at a coarser level — e.g. many OpenMP threads (see [Thread safety](#thread-safety)) each opening their own reader/writer: without this, every one of those threads would *also* fan out across Arrow's thread pool, so N OpenMP threads times Arrow's pool size threads end up competing for the same cores. It's also useful for deterministic single-threaded benchmarking/profiling.

`parquet_set_max_threads(n)` caps the size of Arrow's thread pool itself:
```fortran
call parquet_set_max_threads(4)
```
Unlike `use_threads`, this is **not** a per-reader/per-writer setting — Arrow's CPU thread pool is a single, process-global resource shared by every reader/writer (in every thread) that has `use_threads` enabled. Call it once, e.g. near the start of your program, before opening readers/writers on other threads; calling it repeatedly with different values from multiple concurrent threads is a race, since each call resizes a pool everyone else is using at that same moment. `n` must be `>= 1`; values below that fail immediately with `error stop`.

## Performance and memory

Worth knowing before working with very large files (tens of GB and up):

- **Writing materializes the whole table in memory before anything is written to disk.** Every `parquet_write_column` call builds that column's full data as an in-memory Arrow array (in addition to whatever Fortran array you already hold), and `parquet_close_writer` only starts writing once every column has been added. There is no streaming/incremental write (see [Limitations](#limitations)). Peak memory during a write is roughly 2-3x the table's logical (uncompressed) size.
- **Reading a column always materializes that entire column in memory**, regardless of [row group](#compression-and-row-group-size) count — `chunk_size` only controls on-disk structure, not how much of a column is read at once. Once read, a column stays cached in the `reader` for its lifetime (see [Reading only touches the columns you ask for](#reading-only-touches-the-columns-you-ask-for)), so re-reading it is free, but holding many large columns open at once adds up.
- **Concurrency multiplies this.** Each thread's own `reader`/`writer` (see [Thread safety](#thread-safety)) has its own independent memory footprint — running several large files through concurrently costs roughly that many times the memory of one.
- **Build with optimizations for production use.** `fpm build`'s default profile applies no optimization to either the Fortran or the C++ side; use `fpm build --profile release` (or your own equivalent `-O` flags) — this measurably affects throughput for this library's numeric read/write paths.

Practically: make sure available RAM comfortably covers a few times the decompressed logical size of the largest file(s) you'll have open at once, especially under concurrency.

### Combined example: Nulls, quality control and compression together

This ties together `is_valid` (writing a genuine Null), `qc=.true.` (range-check warnings), and a non-default compression codec in one small program:

```fortran
program write_parquet_qc_example
    use parquet
    use iso_fortran_env, only: int32
    implicit none

    type(parquet_maml_file) :: maml
    type(parquet_column_info) :: cinfo
    type(parquet_table_metadata) :: metadata
    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    integer(int32) :: ra(4) = [10_int32, 400_int32, 90_int32, 200_int32]  ! 400 is out of range
    integer(int32) :: ra_read(4)
    logical :: is_valid(4) = [.true., .true., .false., .true.]           ! row 3 will be written as Null
    logical :: is_valid_read(4)

    maml%name = "qc_example.maml"
    maml%lines = [character(len=40) :: &
        "table: qc_example_table", &
        "fields:", &
        "- name: ra", &
        "  data_type: int32", &
        "  qc:", &
        "    min: '>= 0'", &
        "    max: '< 360'" ]

    call parquet_validate_maml(maml)
    call parquet_read_maml(maml, cinfo, metadata)

    call parquet_open_writer(writer, "data.parquet", cinfo, metadata, qc=.true., compression="zstd")
    call parquet_write_column(writer, "ra", ra, is_valid=is_valid)
    ! prints: WARNING: qc violation for column 'ra': declared min >= 0, max < 360, ...
    call parquet_close_writer(writer)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_read_column(reader, "ra", ra_read, is_valid=is_valid_read)
    call parquet_close_reader(reader)
end program write_parquet_qc_example
```

## parquet module API (functions/subroutines)

List of public callable procedures available with `use parquet`:

### Utility

- `call parquet_get_version(ver_string[, internal])` — sets `ver_string` (`character`, `intent(out)`) to the library version. By default (or `internal=.false.`) returns just the release number without a `v` prefix, e.g. `"0.7.0"` (matching `VERSION.txt`, substituted in at build time via fpm). Pass `internal=.true.` to instead get the full internal version string embedded in the source, e.g. `"v0.7.0 (2026-07-07)"` (includes a `v` prefix and the last-updated date). If the two disagree (e.g. the library was built without going through fpm's version substitution), a `WARNING` is printed to stdout.
- `parquet_set_max_threads(n)` — sets the capacity (number of worker threads) of Arrow's global CPU thread pool to `n`. This is process-global, not per-reader/per-writer; see [Multi-threaded decoding/encoding](#multi-threaded-decodingencoding-use_threads-and-thread-pool-size). `n < 1` fails immediately with `error stop`.

### MAML and metadata

- `parquet_read_maml(maml, cinfo, metadata)` — parses a MAML source into `cinfo` (column definitions) and `metadata` (table-level key/value metadata). Two overloads share the same `maml` keyword: pass a filename (`character`) to read and parse a `.maml` file directly, or pass an already-loaded `parquet_maml_file` object (see `parquet_load_maml_file`) to parse MAML content you already hold in memory. Either overload always runs the same checks as `parquet_validate_maml` first, so a structurally invalid MAML (unknown top-level section, missing `table`, etc.) fails immediately with `error stop` rather than being silently parsed.
- `parquet_load_maml_file(filename)` — reads a `.maml` file from disk and returns it as a `parquet_maml_file` object, without parsing it into `cinfo`/`metadata`. Also validates it (same checks as `parquet_validate_maml`) before returning. Useful when you want to hold on to the raw MAML content (e.g. to pass to `parquet_read_maml` later, or inspect `maml%name`/`maml%lines` directly).
- `parquet_load_qc_maml_file(filename)` — reads a *qc-maml* from disk (a lighter MAML used only for read-time quality-control checks) and returns it as a `parquet_maml_file`, without `parquet_load_maml_file`'s full schema-authoring validation: a qc-maml doesn't need `table:`, `data_type:`, or even `qc:` itself. Pass the result as `parquet_open_reader(..., maml=)`; see [Read-time quality control with a qc-maml](#read-time-quality-control-with-a-qc-maml).
- `parquet_validate_user_maml(base_maml, user_maml)` — checks that every column declared in `user_maml`'s `fields:` block also exists in `base_maml`'s `fields:` block (by name only). `user_maml` may omit any columns from `base_maml`, but must not declare any that aren't there. Unknown columns fail validation and are reported by name.
- `parquet_validate_maml(maml)` — validates a single MAML file on its own. Checks that: at least one field is defined; every field has a non-empty `name`; every field's `data_type` is one of the recognized types (see [Supported data types](#supported-data-types) above; see `valid_maml_data_types` in `src/parquet_metadata.f90` to add more); no two fields share the same `name`; and the file's metadata includes a non-empty `table` entry. It also validates the `qc:` and `protected_cols:` features described in [Null values](#null-values) and [Quality control (qc:) range checks on write](#quality-control-qc-range-checks-on-write) below. Collects and reports all violations together in a single error stop.

`cinfo` is of type `parquet_column_info`, a scalar wrapper holding the array of parsed columns in `cinfo%col(:)` (each element of type `parquet_column_type`, with fields such as `name`, `output_name`, `unit`, `info`, `ucd`, `data_type`, `array_size`, `col_size`, `is_set`). It provides:

- `cinfo%get_column_index(name)` — returns the index of the named column in `cinfo%col(:)`. Missing columns fail immediately with `error stop`.
- `cinfo%set_unavailable([name])` — marks the named column as not set (`is_set = .false.`), so it is skipped when the schema is written. Missing columns fail immediately with `error stop`. If `name` is omitted, marks every column unavailable at once. Use this to drop columns from a base MAML schema without writing a separate MAML file for each combination of active columns.
- `cinfo%set_available([name])` — the inverse of `set_unavailable`: marks the named column (or, if `name` is omitted, every column) as set (`is_set = .true.`).

The public derived type `parquet_table_metadata` provides:

- `metadata%add_metadata(key, value[, description][, fmt])` — attaches a table-level key/value pair (e.g. provenance, units, free-text notes) that is written into the parquet file's VOTable-style header. `value` may be a scalar or 1D array of any [supported type](#supported-data-types) (`integer(int32/int64)`, `real(real32/real64)`, `logical`, `character`). The optional `description` (`character`) is a free-text note stored alongside the value. The optional `fmt` (`character`) sets a display/print format string for the value, otherwise a type-appropriate default is used; it has no effect on how the value itself is stored.

### Writer

- `parquet_open_writer(writer, filename[, cinfo, metadata, write_maml, qc, compression, compression_level, chunk_size, use_threads])` — opens `filename` for writing. `cinfo`/`metadata` are optional; see the schema-enforcement note above. All other arguments are optional keywords:

  | Keyword | Default | Effect |
  |---|---|---|
  | `write_maml` | `.false.` | Also save a sidecar `.maml` file next to `filename` with the MAML source that produced `metadata`. See [Saving the source MAML alongside the parquet file](#saving-the-source-maml-alongside-the-parquet-file). |
  | `qc` | `.false.` | Turn on `qc: min:`/`max:` range-check warnings during writing. See [Quality control (qc:) range checks on write](#quality-control-qc-range-checks-on-write). |
  | `compression` | `"snappy"` | Output compression codec. See [Compression and row group size](#compression-and-row-group-size). |
  | `compression_level` | codec's own default | Tunes the chosen codec's compression level (mainly `zstd`/`gzip`/`brotli`). See [Compression and row group size](#compression-and-row-group-size). |
  | `chunk_size` | auto-sized from the table's byte size (~256 MiB/row group) | **Advanced/optional** — maximum rows per Parquet row group; leave unset for normal use. See [Compression and row group size](#compression-and-row-group-size). |
  | `use_threads` | `.true.` | Encode across Arrow's internal thread pool. See [Multi-threaded decoding/encoding](#multi-threaded-decodingencoding-use_threads-and-thread-pool-size). |

- `parquet_write_column(writer, name, values[, is_valid])` — writes one full column named `name`. `values` may be any [supported type](#supported-data-types), passed as a 1D array (`values(:)`) for a plain column or a 2D array (`values(col_size, nrows)`) for a vector column. `is_valid` (optional, `logical`, same shape as `values`) writes a genuine Parquet Null wherever `.false.` — see [Null values](#null-values).
- `parquet_close_writer(writer)` — flushes buffered data and finalizes the file. Always call this before the program ends, or the file may be incomplete/unreadable. Fails with `error stop` if `writer` was never opened, or was already closed.

### Reader (table and column info)

- `type(parquet_filter)` with `call filt%add(rule)` — builds a row filter to pass as `parquet_open_reader(..., filter=filt)`. Each `%add` appends one rule; multiple rules combine with AND. A `rule` has the shape `"<column> <op> <value>"` (or `"<column> is_null"`/`"<column> is_not_null"`). See [Row filtering with parquet_filter](#row-filtering-with-parquet_filter) for the supported operators, value syntax, and validation.
- `parquet_open_reader(reader, filename[, use_threads, filter, maml, qc, qc_soft])` — opens an existing parquet file for reading. `use_threads` (optional, `logical`, default `.true.`) — decode across Arrow's internal thread pool. See [Multi-threaded decoding/encoding](#multi-threaded-decodingencoding-use_threads-and-thread-pool-size). `filter` (optional, `type(parquet_filter)`) restricts the reader to only rows matching the filter — see [Row filtering with parquet_filter](#row-filtering-with-parquet_filter). `maml`/`qc`/`qc_soft` (optional, `type(parquet_maml_file)`/`logical`/`logical`) enable read-time qc range/Null checks; a violation aborts by default (`qc_soft=.false.`) or, with `qc_soft=.true.`, only warns — see [Read-time quality control with a qc-maml](#read-time-quality-control-with-a-qc-maml).
- `parquet_load_qc_maml_file(filename)` — loads a qc-maml to pass as `parquet_open_reader(..., maml=)`; its full description is under [MAML and metadata](#maml-and-metadata) above, alongside the other MAML loaders.
- `parquet_close_reader(reader[, print_stat])` — releases resources associated with `reader`. Fails with `error stop` if `reader` was never opened, or was already closed. `print_stat` (optional `logical`, default `.false.`) prints a diagnostic summary of the reader's activity to stdout first — see [Printing reader statistics](#printing-reader-statistics-with-parquet_close_readerprint_stattrue).
- `parquet_get_nrows(reader, nrows)` — returns the number of table rows in `nrows` (`integer(int32)` or `integer(int64)`).
- `parquet_get_col_size(reader, name, col_size)` — returns the fixed row length of vector column `name` in `col_size`. Call this before allocating the output array for `parquet_read_column`/`parquet_read_array_row_mode`/`parquet_read_array_element_mode` on that column.
- `parquet_get_column_total_elements(reader, name, total_elements)` — returns the total number of elements in column `name` across all rows (`total_elements = col_size * nrows` for vector columns), in `total_elements` (`integer(int32)` or `integer(int64)`).
- `parquet_get_string_length(reader, name, max_string_length)` — returns the longest string found in string column `name`. Call this before allocating a `character(len=...)` array for `parquet_read_column`, since the allocated length must be at least `max_string_length`.
- `parquet_prefetch_columns(reader, names)` — reads every column named in the `character(len=*)` array `names(:)` in a single call, warming the lazy-read cache for all of them at once. See [Prefetching multiple columns at once](#prefetching-multiple-columns-at-once-with-parquet_prefetch_columns).

### Reader (reads column data)

- `parquet_read_column(reader, name, values[, null_value, is_valid])` — reads the full column `name` into `values`, which may be any [supported type](#supported-data-types) as a 1D array (`values(nrows)`) for a plain column, or a 2D array (`values(col_size, nrows)`) for a vector column. Allocate `values` first, using `parquet_get_nrows`/`parquet_get_col_size`/`parquet_get_string_length` as needed; a row count that doesn't match `parquet_get_nrows` fails immediately with `error stop`. `null_value`/`is_valid` (optional) opt in to reading a column that contains genuine Parquet Nulls instead of erroring — see [Null values](#null-values).
- `parquet_read_array_row_mode(reader, name, values, row_index[, null_value, is_valid])` — reads only row `row_index` of vector column `name` into the 1D array `values(col_size)`. Use this to fetch one row's vector at a time (e.g. when iterating row-by-row) without loading the whole column.
- `parquet_read_array_element_mode(reader, name, values, elem_index[, null_value, is_valid])` — reads only element `elem_index` of vector column `name`, across all rows, into the 1D array `values(nrows)`. Use this to fetch one vector position across every row (e.g. "the 3rd element of every row's vector") without loading the whole column.

## Limitations

Worth knowing up front before relying on this library:

- Only the six types in [Supported data types](#supported-data-types) are handled — there is no `date`/`timestamp`/`decimal` support, no `int8`/`int16`/unsigned integers, and no arbitrary nested/struct/map columns. Vector columns are fixed-length (`col_size`) only: every row must hold the same number of elements. Such a column can be *read back* whether it was stored on disk as a `fixed_size_list` or as a variable-length `list<element>` (see the [reading notes](#reading-parquet-files-from-your-fortran-code)), but a genuinely ragged list (rows of differing length) is not supported — it is rejected on read — and this library's own writer only ever emits `fixed_size_list`.
- **Reading a column whose physical Parquet type doesn't match what you asked for aborts the process, but not via a clean `error stop`** — it's a C++-level abort with a diagnostic printed to stderr (e.g. `parquet-fortran: parquet_read_column: type mismatch for column: d (expected int32/int64, got date32[day])`), not a Fortran `error stop`. This covers the physical type being outside the six [supported data types](#supported-data-types) (e.g. the file has a `date32` column) as well as a declared vector column's shape not matching what was requested, across every read function (`parquet_read_column`, `parquet_read_array_row_mode`, `parquet_read_array_element_mode`, and vector-column reads).
- A column can only be written once per `parquet_writer` — there's no incremental/streaming append to a column across multiple `parquet_write_column` calls, and no way to append rows to an already-closed `.parquet` file. Writing the same column name twice fails immediately with `error stop`, naming the column, whether or not the writer has a MAML-derived schema (`cinfo`).
- **`parquet_open_writer` silently overwrites/truncates an existing file at that path** — there is no existence check and no warning.
- There is no random-access or predicate-pushdown read: `parquet_read_array_row_mode`/`parquet_read_array_element_mode` avoid loading a whole *column* at once, but there's no way to filter which *rows* are read across a table.

## Troubleshooting

Most build failures come from the Arrow/Parquet C++ dependency not being visible to FPM at compile or link time. See [Environment variables](#environment-variables) above for the full variable list; the following are the most common symptoms:

- **`fatal error: arrow/api.h: No such file or directory`** (or similar for `parquet/api/reader.h`) — `FPM_FFLAGS`/`FPM_CXXFLAGS` is not pointing `-I` at Arrow's `include` directory.
- **Link errors like `undefined reference to arrow::...` or `cannot find -lparquet`** — `LIBRARY_PATH`/`FPM_LDFLAGS` is not pointing `-L` at Arrow's `lib` directory, or the `link = ["arrow", "parquet", "c++"]` entry is missing from the consuming project's `fpm.toml`. (`parquet-fortran`'s own `fpm.toml` also links `arrow_compute`, needed for `parquet_close_reader(..., print_stat=.true.)`'s min/max calculation — Arrow ships its compute kernels in a separate library from core `arrow` — and fpm propagates that automatically to consumers, so it does not need to be listed again here.)
- **Linker errors mentioning `std::span` or other C++20-only symbols** — `-std=c++20` is missing from `FPM_CXXFLAGS`; this is required on every platform since Arrow/Parquet headers use `std::span` unconditionally.
- **Undefined references to `std::__1::...` (macOS) or `std::...` (Linux) at the final link step** — the C++ standard library is missing from `FPM_LDFLAGS`. Add `-lc++` on macOS/Clang or `-lstdc++` on Linux/GCC (see [Environment variables](#environment-variables)).
- **At runtime, `dyld: Library not loaded` / `error while loading shared libraries` for `libarrow`/`libparquet`** — the Arrow/Parquet shared libraries are not on the dynamic linker's search path at run time; add their directory to `DYLD_LIBRARY_PATH` (macOS) or `LD_LIBRARY_PATH` (Linux) in addition to `LIBRARY_PATH` used at build time.
- **Program aborts with an `ERROR STOP` message instead of returning a status code** — this is expected; see [Error handling](#error-handling). The message text (e.g. naming a missing column or file) indicates the failing precondition.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for building/testing this repository itself, its error-path testing infrastructure, and a list of features that have been considered but aren't yet implemented.

## License

BSD 3-Clause License — see [LICENSE](LICENSE). See [CHANGELOG.md](CHANGELOG.md) for release history.
