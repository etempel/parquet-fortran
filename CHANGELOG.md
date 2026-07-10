# ChangeLog
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.9.0] - 2026-07-10
### Added
- `parquet_get_metadata(reader, key, value[, default, warn])` — read table-level key/value metadata back out of a parquet file (the read-side counterpart to `schema%add_metadata`); generic over all supported scalar/array types, with an optional same-type `default` and `warn` control.
- `schema%init(...)` and `schema%add_field(...)` — build a `parquet_schema` from scratch in memory (columns + table metadata) without authoring a `.maml` file.

### Changed
- `parquet_column_info`'s `set_available`/`set_unavailable` methods renamed to `set_column_available`/`set_column_unavailable`, matching the identically-named methods on `parquet_schema`. The documented `schema%set_column_available`/`set_column_unavailable` API is unchanged; only direct calls on a `parquet_column_info` (e.g. `schema%cinfo%set_available(...)`) are affected.

## [0.8.0] - 2026-07-08
First version of the parquet-fortran library.
