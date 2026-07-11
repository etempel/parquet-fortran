# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `parquet_schema(table[, ...])` — structure-constructor form of `schema%init(...)`, building and returning an initialized schema in one expression (`schema = parquet_schema(table="t")`) as an alternative to declaring the variable and calling `%init` separately. Same arguments and validation as `schema%init`.

## [0.9.0] - 2026-07-10

### Added

- `parquet_get_metadata(reader, key, value[, default, warn])` — read table-level key/value metadata back out of a parquet file (the read-side counterpart to `schema%add_metadata`); generic over all supported scalar/array types, with an optional same-type `default` and `warn` control.
- `schema%init(...)` and `schema%add_field(...)` — build a `parquet_schema` from scratch in memory (columns + table metadata) without authoring a `.maml` file.

### Changed

- `parquet_column_info`'s `set_available`/`set_unavailable` methods renamed to `set_column_available`/`set_column_unavailable`, matching the identically-named methods on `parquet_schema`. The documented `schema%set_column_available`/`set_column_unavailable` API is unchanged; only direct calls on a `parquet_column_info` (e.g. `schema%cinfo%set_available(...)`) are affected.

## [0.8.0] - 2026-07-08

First version of the parquet-fortran library.

### Added
### Changed
### Deprecated
### Removed
### Fixed
### Security
