# Fortran parquet library for WAVES/4HS

Library to read/write parquet files and handle MAML files. The parquet file metadata can be defined using the [MAML-format](https://github.com/asgr/MAML-Format).

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

Notes:

- Every call to `parquet_write_column` writes one full column.
- All columns in one file must contain the same number of rows.
- Close the writer with `parquet_close_writer` to flush data and finalize the file.

## parquet module API (functions/subroutines)

List of public callable procedures available with `use parquet`:

Some functions allow matxix entries. The definition is (nelem, nrows).

### Utility

- `get_parquet_fortran_version()`

### MAML and metadata

- `parquet_read_maml(maml_filename_or_maml, cinfo, metadata)`
- `parquet_load_maml_file(filename)` — reads a `.maml` file from disk and returns it as a `parquet_maml_file` object, without parsing it into `cinfo`/`metadata`. Useful when you want to hold on to the raw MAML content (e.g. to pass to `parquet_read_maml` later, or inspect `maml%name`/`maml%lines` directly).
- `parquet_validate_user_maml(base_maml, user_maml)` — checks that every column declared in `user_maml`'s `fields:` block also exists in `base_maml`'s `fields:` block (by name only). `user_maml` may omit any columns from `base_maml`, but must not declare any that aren't there. Errors out, naming the offending column(s), if it does.
- `parquet_validate_maml(maml)` — validates a single MAML file on its own. Checks that: at least one field is defined; every field has a non-empty `name`; every field's `data_type` is one of the recognized types (`int32`, `int64`, `string`, `boolean`, `float32`, `float64` — see `valid_maml_data_types` in `src/parquet_metadata.f90` to add more); no two fields share the same `name`; and the file's metadata includes a non-empty `table` entry. Collects and reports all violations together in a single error stop.

`cinfo` is of type `parquet_column_info`, a scalar wrapper holding the array of parsed columns in `cinfo%col(:)` (each element of type `parquet_column_type`, with fields such as `name`, `unit`, `info`, `ucd`, `data_type`, `array_size`, `col_size`, `is_set`). It provides:

- `cinfo%get_column_index(name)` — returns the index of the named column in `cinfo%col(:)`. Errors out if no column with that name exists.
- `cinfo%set_unavailable([name])` — marks the named column as not set (`is_set = .false.`), so it is skipped when the schema is written. Errors out if no column with that name exists. If `name` is omitted, marks every column unavailable at once.
- `cinfo%set_available([name])` — the inverse of `set_unavailable`: marks the named column (or, if `name` is omitted, every column) as set (`is_set = .true.`).

The public derived type `parquet_table_metadata` provides:

- `metadata%add_metadata(key, value[, fmt])`

### Writer

- `parquet_open_writer(writer, filename[, cinfo, metadata])`
- `parquet_write_column(writer, name, data)`
- `parquet_close_writer(writer)`

### Reader (table and column info)

- `parquet_open_reader(reader, filename)`
- `parquet_close_reader(reader)`
- `parquet_get_nrows(reader, nrows)`
- `parquet_get_col_size(reader, name, col_size)`
- `parquet_get_column_total_elements(reader, name, nelem)`
- `parquet_get_string_length(reader, name, strlen_max)`

### Reader (reads column data)

- `parquet_read_column(reader, name, values)`
- `parquet_read_array_row_mode(reader, name, values, row_index)`
- `parquet_read_array_element_mode(reader, name, values, col_index)`
