# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`parquet_index`**: an Arrow-free entry module for fast key-to-index lookup. `pf_index_map`
  maps a single integer key, or an N-component tuple of them, to an index value, with three storage
  backends behind one API — an array indexed by the key, an open-addressing hash table, and sorted
  keys plus a binary search — chosen automatically from the keys unless `method=` names one.
  `%build` fills it in bulk, `%init` plus `%set`/`%get_or_add` fill it as you go, and `%get_many`
  is the bulk lookup for a hot loop; `%remove`, `%keys`, `%probe_stats` and `%memory_bytes`
  complete the surface. Stored values are integers >= 1 and a lookup answers 0 for an absent key.
  `pf_index_pool` hands out and recycles unique index values with reuse always preceding growth,
  and `%compact` gives back grown storage and then hands out the smallest free index first. Every
  mutation of either type is serialized internally, so one shared map or pool may be mutated from
  several threads at once, while map lookups are lock-free. A new `index_threads` setting
  (`PARQUET_FORTRAN_INDEX_THREADS`) caps what a build's internally threaded key scan and scatter
  may open.
- **`parquet_toml`**: an Arrow-free entry module for reading and writing TOML configuration files,
  built on [toml-f](https://github.com/toml-f/toml-f) — which becomes this library's first Fortran
  package dependency, so every consumer now fetches it. `pf_toml_load`/`pf_toml_loads` parse a file
  or a string, `pf_toml_section` opens `[name]` and `[[name]]` entries at any nesting depth, and one
  `pf_toml_get` generic reads scalars and caller-sized arrays of `integer(int32/int64)`,
  `real(real32/real64)`, `logical` and `character`, with `pf_toml_get_alloc` for a list the file
  sizes, `pf_toml_get_strings` for a string list that keeps each element's own length, and
  `pf_toml_get_level` for a `PF_LEVEL_*` value read by name. One rule covers every getter: the bare
  call requires its key, `pf_toml_get` opts out with `default =`, and `pf_toml_get_opt`,
  `pf_toml_get_alloc_opt` and `pf_toml_get_strings_opt` opt out by keeping whatever the variable
  already holds. A wrong-typed value aborts with the offending source line quoted instead of leaving
  the variable undefined, and a default is applied without writing it into the parsed document —
  which is what lets `pf_toml_check` and `pf_toml_check_all` report every key and every section the
  program never read, from a list accumulated automatically rather than one written down.
  `pf_toml_require`, `pf_toml_retire`, `pf_toml_mark`, `pf_toml_mark_section`, `pf_toml_report` and
  raw-object escape hatches round out the read side; `pf_toml_new`, `pf_toml_new_section`,
  `pf_toml_append_section`, `pf_toml_set`, `pf_toml_update` and `pf_toml_delete` build a document,
  `pf_toml_save` writes the effective configuration back out and `pf_toml_dump` writes the document
  as parsed. Every public procedure is safe to call from inside an OpenMP parallel region. See
  [Configuration files](doc/pages/utilities/configuration-files.md).
- **`pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` accept an ARRAY of targets.** The array
  is extracted and order-checked once and every target is answered against that one key, so *m*
  targets cost `O(n + m log n)` where *m* separate calls cost `O(m*n)`. Results are one entry per
  target; a result array of the wrong length is an error and zero targets returns nothing.
- **Matching and joining.** `parquet_table%join(other, on [, other_on] [, how] [, columns] [,
  other_suffix] [, require] [, order] [, max_rows] [, matched] [, pairs] [, other_pairs] [,
  threads])` matches another table's rows against this one's on one or more key columns and brings
  that table's columns over, mutating this table in place and detaching it unless every one of its
  rows survives exactly once and in place — the m:1 left join, which keeps its file, its unread
  columns and its `%generation()`; `how=` is `"inner"`, `"left"`, `"right"`, `"outer"`, `"semi"` or
  `"anti"`; the two one-sided forms null-fill whichever table had no counterpart, and
  `"semi"`/`"anti"` select rows of this table and carry nothing, so `columns=` is refused with them.
  The key column appears once when both sides name it the same -- taking `other`'s value on a row
  this table had no counterpart for -- and only an incoming name that clashes is suffixed
  (`other_suffix=`, default `"_2"`). `columns=` absent carries the columns of `other` that are
  already resident, and naming one reads it; a container column cannot be carried, nor held on this
  side of a `"right"`/`"outer"` join, and a table cannot be joined to itself. `require=` asserts the
  cardinality (`"m:m"`, `"1:1"`, `"1:m"`, `"m:1"`, read left-side-first) and `max_rows=` the output
  size, both before anything is allocated; `matched=` reports which rows of this table found a
  counterpart, and `pairs=`/`other_pairs=` hand back the match itself as one row index per side per
  output row (0 where there is no counterpart), both over the rows this table had on entry. Rows
  come out in this table's order, and within each, its matches in the other table's order, or in key
  order under `order="key"`. At the array level `pf_match`, `pf_match_all` and `pf_in` answer the
  same question over plain arrays, for all eleven element types including `parquet_column`: one
  matching index per element and 0 where there is none, every match as an offsets/matches CSR pair,
  and elementwise membership, with neither array needing to be sorted. Throughout, a key must be of
  exactly the same kind on both sides — nothing is promoted — a null matches nothing including
  another null, and a NaN is a value and does match. See [Joining two
  tables](doc/pages/tables/table-join.md).
- **`parquet_open_table(..., bounded=.true.)` reads a filtered file larger than memory.** The
  filter is evaluated one row group at a time and every column is assembled from per-row-group
  chunks, so the peak is one row group's worth of one column rather than one whole column. Opt-in,
  and never faster on a file that fits; `sort=` is refused with it, and the row mask itself still
  scales with the file's row count. Accepted and inert on the slice forms, which read this way already.
  See [Opening a table](doc/pages/tables/table-open.md#reading-a-file-larger-than-memory-bounded).

### Changed

### Fixed

- A table type generated by `tools/generate_user_table_code.py` now forwards `list_columns=` to
  `parquet_open_table`, which its `%init`/`%init_slice` wrappers had always promised to do.
- `parquet_table%append(other)` refuses to append a table to itself instead of performing the copy
  through two aliased references to one column.
- `pf_argsort` over a `pf_sort_keys` built from an EMPTY array now returns a zero-length
  permutation and a single sentinel `group_offsets` entry, as the array forms always did. It
  previously reported one row, giving back a one-element permutation naming a row that does not
  exist and claiming one group over no rows. `character` keys were unaffected.
- Many other minor fixes and improvements.

## [v2.2.0] - 2026-09-03

### Added

- **`parquet_utils`**: an Arrow-free leaf module of text and path helpers — `pf_to_lower`/
  `pf_to_upper`, `pf_to_str` for rendering a number or a `logical` as text, and `pf_join_path`,
  `pf_dirname`, `pf_basename`, `pf_path_ext`, `pf_path_stem`, `pf_split_path` and
  `pf_path_add_suffix` for taking a path apart and rebuilding it. See
  [Text and path helpers](doc/pages/utilities/utils.md).
- **`parquet_stats`**: an Arrow-free entry module of `pf_*` array statistics over `real64`,
  `real32`, `int32`, `int64` and `logical` arrays and scalar numeric `parquet_column`s, each
  taking an optional null mask, NaN policy and weights. Counts and moments (`pf_count_valid`,
  `pf_sum`, `pf_mean`, `pf_variance`, `pf_stddev`, `pf_sem`, `pf_skewness`, `pf_kurtosis`,
  `pf_moments`), order statistics (`pf_median`, `pf_quantile`, `pf_quantiles`, `pf_iqr`,
  `pf_trim_mean`, `pf_percentile_of_score`, `pf_mad`, `pf_mode`), `pf_gmean`/`pf_hmean`,
  `pf_cov`/`pf_corr` (Pearson or Spearman), `pf_zscore`, `pf_sigma_clipped_stats`, the running
  folds `pf_cumsum`/`pf_cumprod`/`pf_cummax`/`pf_cummin`, and binning with `pf_bucketize`,
  `pf_histogram` and `pf_bin_edges`. A `pf_stats` accumulator summarises a population once —
  resident, incremental (`%update`) or merged from parallel parts (`%merge`) — and answers any
  number of queries off it; `pf_describe` and `%print` render pandas' `describe()` block. The
  moment pass is threaded, with `threads=`, and is bit-identical at every thread count. See
  [Array statistics](doc/pages/utilities/statistics.md).
- **`parquet_logging`**: a general-purpose logger for the calling program — a `pf_logger` type and
  `pf_log_*` procedures on a process-wide default logger, with eight severity levels, several
  sinks at once (console, a file, or a unit you own) each with its own threshold, layout, colour
  and flush policy, templated line layouts, `once=`/`every=` deduplication, per-name level
  overrides, per-thread name and context stacks, a caller-supplied rank filter and
  `pf_log_configure_from_env`. It is a leaf module and the library itself does not use it.
- **`parquet_healpix`**: the HEALPix sphere pixelisation, Arrow-free. Direction to pixel and back
  from angles or unit vectors, RING/NEST conversion, resolution changes, disc queries
  (`pf_query_disc` and its allocating, counting and bounding forms), grid arithmetic over `nside`,
  `npix`, order, pixel area, resolution and rings, and angular separation. Every conversion has a
  `_bulk` form over whole arrays with an optional `threads=`, and `pf_healpix_grid` carries an
  `nside`, scheme and declination convention as one object with an RA/Dec layer on top.
- **`parquet_spatial`**: `pf_spatial_index`, an Arrow-free uniform-grid spatial index over plain
  coordinate arrays in two or three dimensions. Ball search, the self-join as CSR, pair-list and
  count-only forms, `k`-nearest search and `k`-th neighbour distances, segment/cylinder/cone
  searches around an axis, and angular search on the sky backed by either a 3D grid or HEALPix.
  Queries take an inner radius, can return rows ordered by distance, and take one radius or one
  per point; periodic boundaries and an automatic cell size are supported. `pf_connected_components`
  labels an edge list's components, making a Friends-of-Friends group finder one further call.
- **`parquet_list`, `parquet_struct` and `parquet_map`**: three Arrow-free container column types
  — `parquet_list_column` (variable-length rows), `parquet_struct_column` (a fixed field set of
  possibly differing types) and `parquet_map_column` (string-keyed `key -> value` entries) — each
  with a lightweight row handle, nine scalar payload kinds, per-row and per-element null tracking,
  deep copy, move and a `%gather_rows` rebuild. A `parquet_column` takes ownership of one through
  the new `%adopt_container`. `parquet_read_column`/`parquet_read_column_chunk` read `LIST`,
  `LARGE_LIST`, `STRUCT` and `MAP` columns from a file straight into them, whole or one row group
  at a time, and `parquet_write_column`/`parquet_write_column_chunk` write them back out, declared
  in a schema as `list[<elemtype>]`, `struct` or `map[<valuetype>]`. A container nested inside
  another is readable — reached through `%nested`, or by a descent path such as
  `"list_of_struct[].x"` — and writing one is refused with a message naming the column.
- **A `parquet_table` column can be a list, a map or a struct.** A `MAP` column is classified
  automatically, a variable-length `LIST` becomes a `parquet_list_column` when
  `parquet_open_table`'s new `list_columns="container"` asks for it, and a `parquet_struct_column`
  is added in memory with `%add_column`. Each gets `%col`, `%get`, `%set`, `%add_column` and a
  column handle's `%ref`; every row-structural mutation carries them along, `%print_stat` reports
  the shortest and longest row, and `parquet_write_table` writes them back out. Element-granular
  validity, `%get_slice`, `%get_element` and row-handle access are refused, since a container row
  has no fixed-width cell.
- **Two new schema queries answered from the file metadata alone**: `parquet_get_column_shape`
  reports whether a column is a `"scalar"`, `"vector"`, `"list"`, `"map"`, `"struct"` or
  `"unknown"`, and `parquet_get_map_value_type` reports a map column's value type.

### Changed

- **`pf_nth_element` and `pf_nth_quantile` are 2.5-3.9x faster on a large array**, and
  `pf_minmax`/`pf_argminmax` are faster too. `pf_quantiles`, `pf_median` and `pf_iqr` follow suit.
  Answers are unchanged.
- **`pf_minmax` and `pf_nth_quantile` take an optional `ok=`, so an all-null population can be
  reported instead of aborting.** Omitting the argument restores the abort; `pf_argminmax` is
  unchanged.
- **`parquet_write_table`'s `copy_metadata=`/`metadata_keys=` no longer carry a key the writer
  generates itself** — `DATE`, `name`, the two `IVOA.VOTable-Parquet.*` keys and every
  `column.<name>.<attr>` entry. `copy_metadata=.true.` skips them; `metadata_keys=` naming one is
  now an error.

### Fixed

- **Concurrent `parquet_open_reader`/`parquet_open_writer` calls no longer corrupt the heap while a
  file date is pinned.** Every setting mirrored to the C++ side is now atomic or mutex-guarded.
- **`parquet_get_col_size` and `parquet_get_column_total_elements` report a variable-length `list`
  column's element count** — the sum of its rows' own lengths, rather than its row count — **and
  see through an Arrow extension, dictionary or run-end-encoded wrapper** to the width of the
  fixed-size list underneath.
- **`schema%get_field` and `schema%add_field_from` keep a temporal column's unit and UTC flag.** A
  `timestamp[ns,utc]` column copied with `%add_field_from` became a bare `timestamp`.
- **A NaN is left out of a float column's reported minimum and maximum.** `%print_stat` and a
  write-time `qc: min:`/`max:` violation warning both report the range over the values that can be
  ordered, and a column whose every value is a NaN reports `NaN`. Either previously gave a
  processor-dependent answer, and aborted under a compiler running with the IEEE traps unmasked.
- Many other minor fixes and improvements.

## [2.0.0] - 2026-08-24

**Toolchain floor:** gfortran ≥ 13 (13 on CI; 15.2.0 the primary development target), and —
confirmed by hand rather than by CI — Intel Fortran (ifx) 2026.1.0/2026.1.1, NAG Fortran 7.2, and
LLVM flang 22.1.8 (serial builds only). Also fpm ≥ 0.13.0, a C++20-capable C++ compiler, and
Arrow/Parquet C++ ≥ 24.0.0 (validated locally against 25.0.0). See
[Prerequisites](README.md#prerequisites) for the full detail.

**SemVer scope:** the promise covers the whole `use parquet` surface — every public
type/procedure/constant reachable that way, including a public type's own type-bound procedures and
operators — and, new in this release, each advertised entry module *in its own right*
(`parquet_io`, `parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal`,
`parquet_sorting`, `parquet_argsort`, `parquet_sampling`, `parquet_random`, `parquet_settings`,
`parquet_version`, `parquet_maml_base`), since a module offered as an entry point has to stay usable
through that import alone. `parquet_core`, `parquet_bindings` and every `*_base`/`*_engine`/
`*_kernel` module are explicitly outside it.

**Compatibility:** three source-incompatible changes and two behaviour changes, each listed under
Changed — `parquet_set_max_threads` is renamed, `sample_seed=` widens to `integer(int64)`, and
`parquet_get_version(v, mode=)` is removed; a file written without an explicit `compression=` now
uses zstd rather than snappy, and a schema field declaring no `qc: miss:` no longer means "no Nulls
allowed". Parquet files written by 1.0.0 are read unchanged.

### Added

- **`parquet_tables` (`parquet_table`)**: a whole parquet file as one in-memory object.
  `parquet_open_table` reads each column on first use; `%get` hands one back as an ordinary Fortran
  array and `%col` as a zero-copy typed pointer. Tables can be built in memory (`parquet_new_table`,
  `%add_column`, `%append`), mutated (`%sort_by`, `%filter_rows`, `%top_n`, `%delete_rows`,
  `%truncate`, `%cast`), opened as a row slice, cloned, written back out (`parquet_write_table`),
  and used from several threads with the library enforcing the rules rather than documenting them.
- **`parquet_columns` (`parquet_column`)**: type-erased whole-column value storage over 18 scalar
  and vector kinds, with per-element null tracking and the full value and structural instruction
  set.
- **`parquet_sorting`**: sorting for plain Fortran arrays and column types over eleven element
  types — `pf_argsort`, `pf_sort`, `pf_permute`, `pf_is_sorted`, `pf_partial_argsort`, multi-key
  sorting through `pf_sort_keys`, binary search, unique and reduction operations, and group
  boundaries from the same pass as the sort. Read-time sorting, `parquet_table%sort_by` and
  `pf_argsort` are one engine, so they cannot disagree about null placement, NaN placement or tie
  order.
- **`parquet_random` and `parquet_sampling`**: a counter-based generator whose value at
  `(seed, index)` is a pure function of its coordinates rather than of call order, so a parallel
  loop returns the same numbers at any thread count or schedule. Uniform, integer, exponential,
  normal, gamma and Poisson draws; permutations, subsets and resampling; and weighted sampling
  without replacement (`pf_weighted_draw`, `pf_weighted_subset`, `pf_weighted_permutation`).
- **`parquet_settings`**: process-global settings — Arrow's thread pool, per-area thread caps,
  default compression codec and level, terminal verbosity and message stream, the sort's integer
  counting fast path, target row-group size and the statistics prescreen — with
  `parquet_print_settings`, `parquet_reset_settings` and one `PARQUET_FORTRAN_*` environment
  variable per knob.
- **Reproducible file output**: `parquet_set_file_date`/`parquet_get_file_date`, and
  `PARQUET_FORTRAN_FILE_DATE`, pin the `DATE` metadata key to a given `YYYY-MM-DDTHH:MM:SS` instead
  of reading the clock, so the same data written twice produces byte-identical files. An empty
  value, the default, reads the clock as before.
- **Every layer of the library is now an entry module in its own right**, each covered by the
  version promise and each far cheaper to compile against than `use parquet`: `parquet_io`,
  `parquet_tables`, `parquet_columns`, `parquet_sorting`, `parquet_argsort`, `parquet_sampling`,
  `parquet_random`, `parquet_strings`, `parquet_temporal`, `parquet_settings` and
  `parquet_version`. Eight of them no longer reach the Parquet C++ bindings at all. `use parquet`
  is unchanged and remains the recommended import.
- **Row filters are boolean expressions** over a file's columns — `and`/`or`/`not`, parentheses and
  precedence — rather than AND-combined clauses only, and a filter can be installed after open with
  `parquet_reader_set_filter`.
- **Read-time sorting**: `parquet_open_reader(..., sort_by=)` and `parquet_reader_set_sort` return a
  file's rows ordered by one or more columns, with per-key direction and null placement. Streaming
  and chunked reads now work on a filtered, sampled or sorted reader.
- **Generated table types**: `tools/generate_user_table_code.py` turns a MAML schema into a named
  `parquet_table` extension with one accessor per declared column.
- **`parquet_read_qc` and `parquet_compose_read_qc`**: read-time quality control declared in code
  and composed against whatever a file's own qc-MAML declares.
- **Reader queries answered without materializing a column**: `parquet_get_column_names`,
  `parquet_get_metadata_items`, `parquet_get_physical_row_indices`, `parquet_get_qc_columns`,
  `parquet_get_column_nullable`, `parquet_column_has_nulls`, `parquet_measure_list_width` and
  `parquet_release_column`.
- **Typed table metadata records its own type in the written file**, as a companion
  `<KEY>.datatype` entry, so a value written with `schema%add_metadata("NSIDE", 1024_int32)` reads
  back as an `int32` rather than as the indistinguishable string `1024`.
- **NAG Fortran 7.2 and LLVM flang are tested toolchains**, alongside gfortran and ifx. flang builds
  are serial only; see [Prerequisites](README.md#prerequisites) for what each one needs.

### Changed

- **BREAKING: `parquet_set_max_threads` is renamed to `parquet_set_arrow_threads`.** The old name is
  removed rather than kept as an alias, so a call to it no longer compiles.
- **BREAKING: `sample_seed=` is now `integer(int64)`**, and for a given seed
  `parquet_open_reader(..., sample_fraction=)` selects a different set of rows than it did in 1.0.0.
- **BREAKING: `parquet_get_version(v, mode="arrow")` and `mode="parquet"` are removed.**
  `parquet_get_arrow_version` reports the linked Arrow and Parquet C++ versions instead.
- **`parquet_open_writer`'s default compression is now `zstd` at level 3**, changed from `snappy`,
  and `float32`/`float64` columns are written with BYTE_STREAM_SPLIT encoding and dictionary
  encoding disabled.
- **A schema built with `schema%init`/`schema%add_field` no longer needs a `parquet_parse_maml`
  call**, and those calls no longer have a required order. `%add_field` now applies the same
  per-field validation `parquet_validate_maml` applies.
- **`qc: miss:` has three states**, and a field that declares none no longer means "no Nulls
  allowed" — it now says nothing about Nulls, and none are checked on either the write or the read
  side.
- **`parquet_write_column_chunk` converts a chunk's values to the schema's declared numeric type**,
  exactly as `parquet_write_column` always has.
- **A streamed column's nullability comes from its first row group's `is_valid` mask**, and every
  row group must agree. A vector column's element field is written non-nullable when nothing can
  put a Null in it.
- **Closing a schema-enforced writer that had nothing written to it produces a valid empty file**
  instead of aborting.
- **`parquet_get_column_type` and `parquet_column_exists(types=)` report the narrowest lossless
  Fortran kind** a column can be read into, rather than whether its physical type is one of nine
  names.
- **Reads, writes, filtering, sorting and string columns are substantially faster**, in many cases
  by several times: temporal reads, string reads and writes, quality-control checks, filter
  evaluation, sorts over columns containing nulls, validity handling, and column lookup on a wide
  table. A `parquet_table`'s per-column work and one `parquet_string_column`'s bulk work now also
  run on several threads.
- **The user guide is reorganised into six groups**, so every published page URL changed from
  `page/<name>.html` to `page/<group>/<name>.html`.

### Fixed

- **An integer `qc: min:`/`max:` bound is exact at any magnitude.** Bounds were parsed and compared
  through `float64`, so past 2^53 a bound could round and a violating value be accepted.
- **An unrecognised `qc: miss:` value in a schema MAML is rejected** instead of silently meaning the
  opposite of what it says.
- **A MAML key is case-insensitive everywhere, including block headers.** A capitalised `Extra:`
  silently lost the whole block.
- **MAML files are read correctly**: lines beyond 1024 characters are no longer silently truncated,
  a CRLF file no longer produces a misleading type error, and a spurious length abort under heavy
  thread contention is gone.
- **`parquet_date`/`parquet_timestamp` offset arithmetic and `%to_unix` no longer misjudge their own
  overflow guards under nagfor**, which could abort on an ordinary date or wrap silently past
  `int64`.
- **Sharing one `parquet_reader`/`parquet_writer` across threads aborts with the documented
  diagnostic**, instead of segfaulting, or hanging when several threads trip the guard at once.
- **Two heap-corruption races on first concurrent use of Arrow's own type singletons are fixed**
  (confirmed with ThreadSanitizer).
- **A struct-nested string column read through the compact buffer path no longer returns every row
  as Null** (a use-after-free).
- **The library no longer kills a process whose IEEE traps are unmasked**: both
  `parquet_close_reader(print_stat=.true.)` and writing a NaN to an int-declared column trapped.
- Many other minor fixes and improvements.

## [1.0.0] - 2026-07-27

**Toolchain floor:** gfortran ≥ 13 (13 on CI; 15.2.0 the primary development target), Intel
Fortran (ifx) 2026.1.0 (confirmed manually; not exercised by CI), a C++20-capable C++ compiler,
and Arrow/Parquet C++ ≥ 24.0.0 (validated locally against 25.0.0). See
[Prerequisites](README.md#prerequisites) for the full detail.

**SemVer scope:** the stability promise covers the whole `use parquet` surface documented under
README's [API stability](README.md#important-behavior) — every public type/procedure/constant reachable
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

[2.0.0]: https://github.com/etempel/parquet-fortran/releases/tag/v2.0.0
[1.0.0]: https://github.com/etempel/parquet-fortran/releases/tag/v1.0.0
