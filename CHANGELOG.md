# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character`
  (MAML: `boolean`/`string`), as plain 1D columns or fixed-length vector columns.
- Read and write `DATE`/`TIME`/`TIMESTAMP` columns (`parquet_date`/`parquet_time`/
  `parquet_timestamp`), nanosecond-precision, with ISO-8601 and Unix-time/MJD/JD interop.
- Compact `parquet_string_column` type for variable-length string columns without pre-sizing,
  with search/mutation (`find`, `contains`/`startswith`/`endswith`/`equals`, `append`/`set`/
  `erase`), zero-copy element handles (`view`/`view_all`/`view_slice`), gathering handles back
  into a column (`build_from`), an owning row-range copy (`slice`), `clone`/`move`/`swap`, and
  summary statistics.
- Schema and metadata definition/validation from a MAML file, or built directly in code
  (`schema%init`/`add_field`/`add_metadata`), including column renaming (`col_map:`),
  quality-control range checks (`qc:`), and Null-protected columns (`protected_cols:`).
- Reading a file's stored metadata back (`parquet_get_metadata`) and prefetching columns
  (`parquet_prefetch_columns`).
- Genuine Parquet Null support on read/write, via value substitution or a validity mask.
- Row filtering on read (`parquet_filter`) and row masking on write (`parquet_write_row_mask`/
  `parquet_write_chunk_row_mask`, dropping rows entirely rather than substituting Null), plus
  streaming/chunked reads and writes for large columns and row-mode/element-mode random-access
  reads.
- Reading a column into a widened numeric kind (including `int8`/`int16`/unsigned integers/
  `half_float`/`decimal32..256`), `STRING_VIEW`, foreign `LIST`/`LARGE_LIST` columns, and nested
  `STRUCT` fields via a dot-separated path.
- Control over output compression codec, compression level, and row group size.
- Thread-safe concurrent use (e.g. from OpenMP).
- Support for embedding your own MAML schemas into a downstream project.

### Changed
### Deprecated
### Removed
### Fixed
### Security
