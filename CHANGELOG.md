# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Row filters are now boolean **expressions** over the file's columns, not just AND-combined
  clauses: `filt%add("(ra > 180 and dec <= 0) or id is_null")`, with `and`/`or`/`not` (any
  casing), parentheses and `not` > `and` > `or` precedence. Several `%add` calls are still
  AND-combined, so every filter written against the previous syntax means exactly what it did.
  Null handling is now genuine SQL three-valued logic — a comparison against a Null is unknown
  and never survives, in particular under `not`, and `is_null`/`is_not_null` remain the only way
  to select Null rows. A NaN, by contrast, is a value rather than a missing one, so it compares
  false against `>`/`>=`/`<`/`<=`/`==` but true against `/=` and any negated comparison; two new
  operators, `is_nan` and `is_not_nan`, select on it directly for a floating-point column (a Null
  row is unknown for both, so nullness stays governed solely by `is_null`/`is_not_null`). A bare
  `nan` as a comparison value is rejected in favour of them, while `inf`/`-inf` stay accepted as
  ordinary bounds. `date`/`time`/`timestamp` columns can now be filtered, against double-quoted
  ISO-8601 literals (`obs_ts >= "2024-01-31T12:30:00"`), where a literal finer than the column's
  stored unit is rejected rather than silently truncated. A filter can also be applied to an
  already-open reader with the new `parquet_reader_set_filter`. Rule text is no longer capped at
  512 characters, and `parquet_close_reader(..., print_stat=.true.)` prints the whole expression as
  applied. Finally, a filtered reader now **skips the row groups its filter provably cannot match**:
  each row group's own footer statistics are consulted before any column data is read, and the
  row groups ruled out are skipped for every column read afterwards, not just the filter's own — a
  selective read of a 4M-row, 40-row-group file measured 5x faster, while a filter matching every
  row was unchanged. Skipping changes no result (same rows, same order, same nulls) and has nothing
  to switch on; a file written without statistics, an unsigned/`decimal`/`half_float` column, and a
  floating-point column under `not` or `/=` simply skip nothing, and `print_stat` reports a
  `screened:` line whenever row groups were skipped. See
  [Row filtering](doc/pages/reading.md#row-filtering-with-parquet_filter).
- Read-time sorting: `parquet_open_reader(..., sort_by=srt)` returns a file's rows ordered by one
  or more columns, and every column read afterwards comes back in that order. Keys are added one
  per `srt%add("ra asc")`/`%add("-dec")` call to a `parquet_sortkey` and applied in order, with
  per-key `nulls_first=`, stable ties, and any scalar column type as a key (including `date`/
  `time`/`timestamp` and dotted struct-leaf paths). Sorting composes with `filter=`/
  `sample_fraction=` — the filter runs first, the sort orders the survivors — and can also be
  applied to an already-open reader with `parquet_reader_set_sort`. Null and NaN placement
  reproduce Arrow's own sort ordering exactly, so a result cross-checked against `pyarrow` matches
  row for row. While a sort is active, row-group-scoped operations (`parquet_read_column_chunk`,
  `parquet_get_chunk_size`) are refused and row/element mode read the whole column, since a sorted
  row belongs to no single row group. See
  [Reading rows in sorted order](doc/pages/reading.md#reading-rows-in-sorted-order-with-parquet_sortkey).
- Streaming/chunked reads now work on a filtered or sampled reader, which previously refused them
  outright: `parquet_read_column_chunk` hands back that row group's surviving rows, and
  `parquet_get_chunk_size` reports that same count, so a chunked loop's sizes still sum to
  `parquet_get_nrows` (a row group with no survivors reads as an empty chunk, and still counts as
  read for `check_complete`). Read-time qc validates the filtered chunk rather than the raw one.
  `parquet_reader_set_filter(reader, filter, row_group_lo, row_group_hi)` additionally scopes the
  filter to a row-group range and evaluates it one row group at a time, so peak memory is one row
  group's worth of the filter columns instead of the whole file — enough to filter a file larger
  than memory. See [Streaming/chunked reads](doc/pages/reading.md#streamingchunked-reads).
- Added `parquet_tables` (`parquet_table`): presents a whole parquet file as one in-memory table
  (`parquet_open_table`), hands columns back as ordinary Fortran arrays through a widening copy
  (`%get`) or a zero-copy typed pointer (`%col`), builds a table from scratch in memory
  (`parquet_new_table` + `%add_column`), and writes one back out through an ordinary
  `parquet_schema` (`parquet_write_table`). Columns are read on first use rather than at open
  time, so opening reads only the file's schema and a program pays only for the columns it
  touches — `%prefetch`, `%materialize_all` and `%reload` control this explicitly and
  `%residency` reports it. Also included: a row-range (slice) form of `parquet_open_table`, with
  `parquet_table_row_group_bounds`/`%row_group_bounds` for finding the natural boundaries to
  split a file on; `parquet_table_row` (`t%row(i)`), a lightweight handle on a single row; and
  `parquet_slice` with `parquet_slice_range`/`parquet_slice_list` and `%get_slice`, which copies
  a strided or gathered row selection out of a column. A table can also be **changed**: one cell
  at a time (`%set_element`, `%set_null`, `%clear_null`, `%compact_validity`), a column at a time
  (`%drop_column`, `%rename_column`, `%cast_column`), or a row at a time (`%filter_rows`,
  `%sort_by`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows`) — and `%clone` takes
  an independent deep copy, which is how a version is kept to go back to, since mutation is in
  place. Changing the **row set** detaches the table from its file: the rows in memory no longer
  line up with the rows on disk, so any column not read by then can never be read, `%is_detached`
  reports it, and every later read from the file is a clear error rather than misaligned data.
  `%sort_by` runs the same C++ sort engine as the read-time `sort_by=`, so sorting a table in
  memory and reading the same file sorted give the identical row order. See
  [the guide](doc/pages/table.md) for the current limitations, the detach rule and the OpenMP
  first-touch rule.
- Added `parquet_columns` (`parquet_column`): type-erased, whole-column value storage with sparse
  null tracking, covering 18 scalar/vector kinds plus reserved slots for the future
  list/map/struct column types, with the full value and structural instruction set — including
  `%adopt`, which makes a caller's array the column's storage outright via `move_alloc` instead
  of copying into freshly allocated storage, and `%paste`, which overwrites an existing row range
  from another column of the same kind and width without reallocating anything. It is the shared
  foundation `parquet_table` and the container column types are built on; its per-kind blocks are
  generated by `tools/generate_parquet_columns.py` (committed output, no build-time step).
- Added reader queries that answer without materializing a column: `parquet_get_column_names`
  (every column in a file, expanding a nested struct into one dotted leaf path per leaf),
  `parquet_release_column` (frees a column's decoded Arrow buffers once the caller has its own
  copy), `parquet_column_has_nulls` (from the file's own statistics, no column data read —
  `.false.` is a guarantee, `.true.` means "assume the worst"), and `parquet_measure_list_width`
  with `parquet_column_width_needs_data` (a variable-length `LIST`/`LARGE_LIST` column's uniform
  elements-per-row width, from the footer alone or confirmed by a scan that never holds more than
  one row group).
- Added bulk `reindex`, `delete_by_mask` and `append_nulls` operations to `parquet_string_column`.
- Added `tools/check_bindc_boundary.py`, wired into CI, to catch `bind(C)`/`extern "C"` signature
  mismatches between Fortran and C++.

### Changed

- `parquet_read_array_row_mode` and `parquet_read_array_element_mode` stay row-group-scoped on a
  filtered or sampled reader, where they previously fell back to reading the whole (filtered)
  column. `row_index`/`elem_index` still address the filtered result, and are now resolved
  against each row group's surviving row count — so row mode reads only the row group the
  requested row lives in, and element mode streams row group by row group, exactly as they
  already did without a filter. Results are unchanged; peak memory is not.
- **`parquet_open_writer`'s default compression codec is now `"zstd"` at level 3, changed from
  `"snappy"`.** This is a behavior change for any caller that omits `compression=` — files written
  without an explicit codec will now be smaller (better ratio than snappy) at a modest extra write
  cost, with no read-time penalty. Pass `compression="snappy"` explicitly to keep the previous
  default behavior.
- `float32`/`float64` columns are now automatically written with BYTE_STREAM_SPLIT encoding and
  dictionary encoding disabled, regardless of the chosen `compression` codec — improves the
  achievable compression ratio for floating-point data. Automatic and type-based; not a new
  argument, and every other column type is unaffected.

### Fixed

- Fixed a use-after-free in the compact string-buffer read path for struct-nested string columns.
- Fixed silent truncation of MAML lines beyond 1024 characters, and CRLF (Windows line-ending)
  handling in MAML files.
- Fixed a spurious "line exceeds 1024 characters" abort when loading a MAML file, observed under
  heavy multi-threaded contention (e.g. ifx with 100+ concurrent OpenMP threads reading the same
  MAML fixture at once): the non-advancing read used to detect over-length lines is now serialized
  with an OpenMP critical section.
- Fixed a documentation error pointing readers at `is_init()` instead of `is_parsed()`, plus
  several stale or broken documentation cross-links and anchors.
- Fixed a rare heap-corruption race (confirmed via ThreadSanitizer) when two OpenMP threads each
  opened a `parquet_reader`/`parquet_writer` and touched a column of the same type for the very
  first time in the process at close to the same moment: Arrow's own type-singleton construction
  (`arrow::int32()`, `arrow::utf8()`, ...) is not safely concurrent on this project's Arrow build,
  and racing it could corrupt memory that only surfaced later, in unrelated code. `parquet_open_reader`/
  `parquet_open_writer` now force every such singleton into existence once, from a single thread,
  before any concurrent caller can reach Arrow. See
  [Thread safety](doc/pages/thread-safety.md#a-note-on-arrows-own-type-singleton-construction).

## [1.0.0] - 2026-07-27

**Toolchain floor:** gfortran ≥ 13 (13 on CI; 15.2.0 the primary development target), Intel
Fortran (ifx) 2026.1.0 (confirmed manually; not exercised by CI), a C++20-capable C++ compiler,
and Arrow/Parquet C++ ≥ 24.0.0 (validated locally against 25.0.0). See
[Prerequisites](README.md#prerequisites) for the full detail.

**SemVer scope:** the stability promise covers the whole `use parquet` surface documented under
README's [API overview](README.md#api-overview) — every public type/procedure/constant reachable
that way, including a public type's own type-bound procedures and operators; anything not
reachable via `use parquet` (private module internals, the C++ surface, file/module layout) can
change in a minor or patch release.

**Compatibility:** 1.0.0 is the first published release (no prior `0.9.x` version was ever
tagged or published to the fpm registry), so there is no prior-release file format to remain
compatible with.

### Added

- Read and write parquet columns for `int32`/`int64`/`float32`/`float64`/`logical`/`character`
  (MAML: `boolean`/`string`), as plain 1D columns or fixed-length vector columns.
- Read and write `DATE`/`TIME`/`TIMESTAMP` columns (`parquet_date`/`parquet_time`/
  `parquet_timestamp`), nanosecond-precision, with ISO-8601 and Unix-time/MJD/JD interop,
  same-type difference/offset arithmetic (`operator(-)`/`operator(+)`, `%diff_seconds`, and the
  `parquet_ns_per_sec`/`parquet_ns_per_day`/`parquet_ns_to_sec`/`parquet_ns_to_day` conversion
  constants), and reading back a column's stored unit/timezone (`parquet_get_column_time_info`).
- Compact `parquet_string_column` type for variable-length string columns without pre-sizing,
  with search/mutation (`find`, `contains`/`startswith`/`endswith`/`equals`, `append`/`set`/
  `erase`), zero-copy element handles (`view`/`view_all`/`view_slice`), gathering handles back
  into a column (`build_from`), an owning row-range copy (`slice`), `clone`/`move`/`swap`, and
  summary statistics.
- Schema and metadata definition/validation from a MAML file, or built directly in code
  (`schema%init`/`add_field`/`add_metadata`), including column renaming (`col_map:`),
  quality-control range checks (`qc:`), Null-protected columns (`protected_cols:`), automatic
  `col_size`/`array_size` resolution at write time (`parquet_size_auto`), inspecting/copying an
  existing field (`schema%get_field`/`add_field_from`), and a human-readable schema dump
  (`schema%print_schema_info`).
- Read-time quality control: checking an already-written file's data against a qc-maml
  (`parquet_load_qc_maml_file`, `qc=`/`qc_soft=` on `parquet_open_reader`), independent of the
  `qc:` enforcement performed on write.
- Reading a file's stored metadata back (`parquet_get_metadata`) and prefetching columns
  (`parquet_prefetch_columns`).
- Genuine Parquet Null support on read/write, via value substitution or a validity mask.
- Row filtering on read (`parquet_filter`), random downsampling (`sample_fraction`/`sample_seed`),
  and row masking on write (`parquet_write_row_mask`/`parquet_write_chunk_row_mask`, dropping rows
  entirely rather than substituting Null), plus streaming/chunked reads and writes for large
  columns and row-mode/element-mode random-access reads.
- Reading a column into a widened numeric kind (including `int8`/`int16`/unsigned integers/
  `half_float`/`decimal32..256`), `STRING_VIEW`, foreign `LIST`/`LARGE_LIST` columns, and nested
  `STRUCT` fields via a dot-separated path.
- Reader queries answered from the file's schema/footer without reading column data: a column's
  existence/type (`parquet_column_exists`/`parquet_get_column_type`), row-group count
  (`parquet_get_num_row_groups`), vector-column width/total element count (`parquet_get_col_size`/
  `parquet_get_column_total_elements`), and a string column's longest stored value
  (`parquet_get_string_length`).
- Control over output compression codec, compression level, and row group size; opting out of
  silently overwriting an existing file (`overwrite=.false.` on `parquet_open_writer`); printing
  per-column read statistics on close (`print_stat=` on `parquet_close_reader`).
- Thread-safe concurrent use (e.g. from OpenMP), with control over Arrow's internal thread-pool
  size (`parquet_set_max_threads`).
- Support for embedding your own MAML schemas into a downstream project.

[1.0.0]: https://github.com/etempel/parquet-fortran/releases/tag/v1.0.0
