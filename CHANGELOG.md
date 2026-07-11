# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

## [0.9.0] - 2026-07-11

### Added

- `parquet_get_metadata(reader, key, value[, default, warn])` — read table-level key/value metadata back out of a parquet file (the read-side counterpart to `schema%add_metadata`); generic over all supported scalar/array types, with an optional same-type `default` and `warn` control.
- `schema%init(...)` and `schema%add_field(...)` — build a `parquet_schema` from scratch in memory (columns + table metadata) without authoring a `.maml` file.
- `schema%get_num_fields()` and `schema%get_field_name(index)` — total number of fields declared in a schema (MAML source order, unfiltered by `set_column_available`/`set_column_unavailable`) and the name of the field at a given 1-based index. Out-of-range `index` fails with `error stop`.
- Added many minor imprevements spread across the library.

### Changed

- `parquet_column_info`'s `set_available`/`set_unavailable` methods renamed to `set_column_available`/`set_column_unavailable`, matching the identically-named methods on `parquet_schema`. The documented `schema%set_column_available`/`set_column_unavailable` API is unchanged; only direct calls on a `parquet_column_info` (e.g. `schema%cinfo%set_available(...)`) are affected.
- Many more `parquet_write_column`/`parquet_read_column`/`parquet_open_writer`/`parquet_open_reader`/`schema%add_field` `error stop` messages now name the output/input file and, where relevant.
- Minor changes in the public API that does not affect standard use cases.

### Fixed

- Fixed several minor issues and edge cases (added new unit tests for them).

## [0.8.0] - 2026-07-08

First version of the parquet-fortran library.

### Added
### Changed
### Deprecated
### Removed
### Fixed
### Security
