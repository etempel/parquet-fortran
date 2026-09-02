# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`parquet_utils`: text and path helpers.** A new Arrow-free leaf entry module, one Fortran
  file, importing nothing but `iso_fortran_env`. `pf_to_lower`/`pf_to_upper` fold ASCII case, as a
  copy or in place, leaving every other byte — including multi-byte UTF-8 — untouched. `pf_to_str`
  renders an `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` or `logical` as
  text, with an optional minimum field width, padding character and format; a `logical` renders as
  `true`/`false`. `pf_join_path` joins two to five components, or an array of them, by CPython's
  `posixpath.join` rules, and `pf_dirname`, `pf_basename`, `pf_path_ext`, `pf_path_stem`,
  `pf_split_path` and `pf_path_add_suffix` take a path apart and rebuild it with a suffix inserted
  before the extension. Nothing in the module validates, aborts or prints, and every result comes
  back allocated. See [Text and path helpers](doc/pages/utilities/utils.md).

- **`parquet_stats`: array statistics over plain Fortran arrays.** A new Arrow-free entry module
  under `pf_*` names, for reducing arrays a program already has rather than anything about a
  parquet file. It opens with `pf_count_valid`, which answers how many elements are in the
  population over `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` and `logical`
  arrays — with `n_null=`/`n_nan=` reporting why the rest left — and with it the conventions the
  rest of the family follows: a null (`is_valid=`), a NaN (`skipnan=`, defaulting to excluded as
  `pf_minmax` already does) and a zero weight (`weights=`) each leave the population, in that
  order, so a weight belonging to an excluded element is never examined; an empty or fully
  excluded population answers zero rather than aborting; and every procedure declares its optional
  arguments in one fixed order. A non-finite value is data rather than an error: a population
  containing `+Inf` sums and means to `+Inf` with `ok=.true.`, as numpy and pandas answer, while
  its central moments are NaN. The moment
  family follows over `real(real64)` arrays: `pf_sum`, `pf_mean`, `pf_variance`, `pf_stddev`,
  `pf_sem`, `pf_skewness`, `pf_kurtosis`, and `pf_moments`, which produces all of them plus the
  counts, the sum and the extremes in at most one pair of passes, with `ok=` over the outputs asked
  for -- a call needing nothing from the second pass, `pf_sum` among them, makes only the first. They are weighted (`weights=`,
  `weight_type=`), take `ddof=`/`bias=`/`excess=` with pandas' defaults rather than numpy's, are
  computed two-pass over a fixed pairwise block tree so that the variance is shift-invariant, and
  return a quiet NaN with `ok=.false.` wherever a statistic is undefined. A `pf_stats` accumulator
  summarises a population once and answers any number of queries off it: `%compute` for a resident
  array, `%init` plus `%update` for one arriving in pieces, and `%merge` — whose array form folds in
  index order, and which requires the two accumulators to agree on all three of `retain`,
  `weight_type` and `skipnan` — for accumulating in parallel. Everything in the module takes any of six inputs:
  `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` and `logical` arrays, all
  widened exactly, and a scalar numeric `type(parquet_column)` dispatched on its kind. The
  central-moment pass is threaded on a large population, with `threads=` to override the automatic
  count; the answer is bit-identical at every thread count and without OpenMP. Order statistics
  follow: `pf_median`, `pf_quantile`, `pf_quantiles`, `pf_iqr`, `pf_trim_mean` and
  `pf_percentile_of_score`, over the same six inputs, with numpy's six `method=` tokens and
  weighted quantiles whose rule reduces exactly to the unweighted one at equal weights. A
  `pf_stats` orders its retained values once and answers every later order statistic off that
  ordering, dropping it whenever `%update` or `%merge` changes the population. `pf_mad` gives the
  median absolute deviation, scaled by default so that it estimates the standard deviation of clean
  Gaussian data, with `scale=` and `center=`; `pf_mode` gives the most common value over the
  integer, logical and string kinds, breaking ties to the smallest value — or, through `modes=`,
  returning every tied value as pandas' `Series.mode()` does. `pf_describe` fills a `pf_stats` in
  one pair of passes and one ordering, with `ok=`, and `%print` renders the summary block pandas'
  `describe()` prints. `pf_gmean` and `pf_hmean` give the geometric and harmonic means, also as
  `pf_stats` queries; `pf_cov` and `pf_corr` the pairwise-complete covariance and correlation,
  Pearson or Spearman, with `pf_cov(x, x)` reproducing `pf_variance(x)` bit for bit under either
  `weight_type`; `pf_zscore`
  standardises a whole array; and `pf_sigma_clipped_stats` reproduces astropy's iterative clip on a
  single ordering, reporting the surviving mask so the same clip can be applied to another column.
  `pf_cumsum`, `pf_cumprod`, `pf_cummax` and `pf_cummin` give the running folds, where an excluded
  element yields an excluded output element and the running value carries past it unchanged, as
  pandas does. `pf_bucketize` says which bin of a sorted edge array each value falls in, and
  `pf_histogram` how many values — or how much weight — landed in each, under either numpy's or
  pandas' edge convention, reporting the values that reached no bin rather than dropping them
  silently, and optionally as a `density=` normalised the way `np.histogram` normalises one; both
  take `weights=`, so the histogram is the bucketize tally in every case. `pf_bin_edges` supplies
  the edges themselves, spanning the finite part of the population's own range, and always
  strictly increasing so that the `pf_histogram` call they exist for cannot abort.
  See [Array statistics](doc/pages/utilities/statistics.md).
- **`parquet_logging`: general-purpose logging for the calling program.** A `pf_logger` type and a
  matching set of `pf_log_*` procedures on a process-wide default logger: eight ascending severity
  levels using Python's numbers (`PF_LEVEL_DEBUG` = 10, and an arbitrary integer level is accepted
  too), several sinks at once — console, a file the logger opens, or a unit you own — each with its
  own threshold, layout, colour policy, flush-by-level policy and rank filter, and `%print` to
  dump the whole configuration. The line layout is a template
  with named placeholders (`{stamp}`, `{level}`, `{name}`, `{thread}`, `{rank}`, `{context}`,
  `{message}`, …) where `{field|sep}` emits its separator only when the field is non-empty; ISO-8601
  timestamps and a monotonic `{elapsed}`. `%enabled` is one integer comparison, for guarding an
  expensive message. From an OpenMP region: one shared logger, serialised writes, a per-thread
  context stack (`pf_log_push_context`/`pf_log_pop_context`) over a shared base, `{thread}` in the
  layout, and an opt-in buffered mode that keeps one thread's records contiguous. Also `once=` and
  `every=` deduplication, per-name level overrides that turn a library's noise down or one
  subsystem's up and are removed again with `pf_log_unset_level`, a per-thread name stack
  (`pf_log_push_name`/`pf_log_pop_name`) letting a subprogram name itself without knowing its
  caller's name, a caller-supplied rank filter
  that adds no MPI dependency, `pf_str` for building
  messages by concatenation, and `pf_log_configure_from_env`. It is a leaf — `use parquet_logging`
  compiles one Fortran file — and it is **not** this library's own messaging, which stays with
  `verbosity` and `message_stream`; nothing in the library uses it.
- **`parquet_healpix`: the HEALPix sphere pixelisation.** Direction to pixel and back, from angles
  or from unit vectors (`pf_ang2pix_ring`/`pf_ang2pix_nest`, `pf_vec2pix_*`, `pf_pix2ang_*`,
  `pf_pix2vec_*`, `pf_ang2vec`/`pf_vec2ang`), conversion between the RING and NEST numbering
  schemes (`pf_ring2nest`/`pf_nest2ring`) and between resolutions within NEST
  (`pf_ud_pix_nest`), and the pixels of a disc — exact or overlapping, in either scheme, into your
  buffer (`pf_query_disc`), into one the library sizes itself (`pf_query_disc_alloc`), or as a
  count alone (`pf_query_disc_count`); `pf_query_disc_max_count` bounds that count for any
  position, so one buffer can serve a whole loop of queries. Grid arithmetic over `nside`, `npix`, order, pixel area,
  resolution, ring index and ring latitude (`pf_nside2npix`, `pf_npix2nside`, `pf_nside2order`,
  `pf_order2nside`, `pf_nside2pixarea`, `pf_nside2resol`, `pf_max_pixrad`, `pf_pix2ring_*`,
  `pf_ring2z`), angular separation between two directions (`pf_angdist`) or between two RA/Dec
  positions in degrees (`pf_angdist_deg`), and the squared-chord pair that replaces it in a
  comparison (`pf_chord2_from_angle`/`pf_angle_from_chord2`). Every conversion also has a `_bulk`
  form over whole arrays with an optional `threads=`, capped process-wide by
  `parquet_set_healpix_threads` and reported by `pf_healpix_threads`. Every integer argument takes
  `integer(int32)` or `integer(int64)`; the scalar conversions are `pure elemental`, so they
  accept whole arrays. No floating-point exception is raised on valid input, so a program running
  under `-ffpe-trap` needs no guard around a call. `pf_healpix_grid` carries an `nside`, a scheme
  and a declination convention as one object, validated once by `%init`, so a conversion or a disc
  query on it restates none of them and the `_ring`/`_nest` pairs collapse to one binding each; it
  is also the only place this module offers an RA/Dec layer (`%radec2pix`, `%pix2radec`,
  `%radec2vec`, `%vec2radec` and `%query_disc_radec`, in degrees, read in `PF_HP_DEC_NORTH` or
  `PF_HP_DEC_SOUTH`). A new Arrow-free entry module (`use parquet_healpix` compiles seven Fortran
  files).
- **`parquet_spatial`: `pf_spatial_index`, a uniform-grid spatial index over plain coordinate
  arrays.** Ball search into a caller-owned buffer, the self-join as CSR, the pair list and
  count-only forms, all threaded; `k`-nearest search (`%nearest`) and every point's `k`-th
  neighbour distance at once (`%kth_distance`); segment, cylinder and truncated-cone searches
  around an axis, reporting where on the axis each point sits (`axis_point=`/`axis_t=`);
  search on the sky by angular radius (`%build_sky`/`%within_sky`/`%count_within_sky`/
  `%nearest_sky`/`%kth_distance_sky` plus the three bulk forms `%all_within_sky`/
  `%pairs_within_sky`/`%count_all_within_sky`, degrees in and degrees out everywhere,
  `%rebuild_for` included), backed by either a 3D grid or the HEALPix
  pixelisation of the sphere (`backend=PF_SKY_GRID3D`, the default, or `backend=PF_SKY_HEALPIX`,
  with `nside=`, `%backend()`, `%nside()` and `%npix()`); two or three dimensions; optional periodic
  boundaries with the minimum-image convention; and an automatically chosen cell size. Every
  radius query takes an optional inner radius (`r_inner=`), making the ball an annulus, and can
  return its rows ordered by distance (`sorted=`). Every bulk query takes one radius or one per
  point; the pair list is symmetric under a per-point radius (a pair qualifies when either ball
  reaches the other) while the CSR and count forms are directed. `pf_connected_components` labels
  an edge list's connected components, so a Friends-of-Friends group finder is `%pairs_within`
  followed by one more call. A new Arrow-free entry module (`use parquet_spatial` compiles fifteen
  Fortran files), with `spatial_threads` and `spatial_rebuild_warning` joining the process-global
  settings.
- **`parquet_get_column_shape(reader, name, shape)`** reports whether a column is a `"scalar"`,
  `"vector"`, `"list"`, `"map"`, `"struct"` or `"unknown"`, from the file schema alone. Orthogonal
  to `parquet_get_column_type`, which reports the element type and is unchanged. An Arrow encoding
  wrapper — an extension, dictionary or run-end-encoded type — is read through, so the shape
  reported is that of the wrapped storage.
- **`parquet_list`: `parquet_list_column`, a variable-length list column, plus `parquet_list_row`,
  a lightweight handle to one of its rows.** Rows may hold different numbers of values, including
  none, and a row may be a null (absent) list distinct from a present but empty one; element nulls
  inside a row are tracked separately from row nulls. Nine scalar payload kinds, an
  offsets-plus-payload layout matching Arrow's, geometric growth, deep copy, move, and a
  `%gather_rows` rebuild that serves reordering, filtering and duplication. A `parquet_column` can
  take ownership of one through the new `%adopt_container`, which is how the container reaches the
  rest of the library. A new Arrow-free entry module (`use parquet_list` compiles eleven Fortran
  files) that reads no settings.
  `parquet_read_column`/`parquet_read_column_chunk` read a `LIST`/`LARGE_LIST` column from a
  Parquet file straight into one, whole or one row group at a time, with per-row lengths, null rows
  and null elements intact and with the payload kind taken from the file rather than declared;
  filtering, sampling and sorting compose with it. A list leaf nested inside a `STRUCT` is now
  addressable by its dotted path. `parquet_write_column`/`parquet_write_column_chunk` write one back
  out as a genuine variable-length `LIST` column, declared in a schema as `list[<elemtype>]`; the
  value type decides the file's physical shape, so a 2-D array still writes a fixed-width vector
  column and a `parquet_list_column` always writes a `LIST`. A list may now hold a container — a list of
  structs, of maps or of lists — **on read**; the payload is reached with `parquet_list_row%nested`,
  and a nested leaf is addressable directly by a descent path (`"list_of_struct[].x"`). **Not yet**:
  writing a nested container, which is refused with a message naming the column and the payload
  kind. A container column declares neither `col_size:` nor `array_size:` — both fix a width that is
  the same in every row — so a `write_maml=.true.` sidecar carries neither key for one, whatever the
  source MAML declared.
- **`parquet_struct`: `parquet_struct_column`, a `STRUCT` column, plus `parquet_struct_row`, a
  lightweight handle to one of its rows.** Every row holds one value per declared field and the
  fields may have different types; the field set is fixed by `%init(names, kinds)` and covers the
  nine scalar kinds. A row may be a null (absent) struct instance, distinct from a present one
  whose fields are null, and the two are tracked separately. `%get_field(name, value)` reads a
  field in one call and `%field(name)` narrows a handle for step-by-step navigation; an unknown
  field name can warn instead of aborting. Deep copy, move, and a `%gather_rows` rebuild that
  serves reordering, filtering and duplication. A `parquet_column` can take ownership of one
  through `%adopt_container`. A new Arrow-free entry module (`use parquet_struct` compiles eleven
  Fortran files). `parquet_read_column`/`parquet_read_column_chunk` read a `STRUCT` column from a
  Parquet file straight into one, whole or one row group at a time, taking the field set from the
  file; `parquet_write_column`/`parquet_write_column_chunk` write one back out, declared in a
  schema as `struct` — the field layout comes from the column object, never from MAML. Addressing
  a struct's leaves by their dotted paths (`"person.age"`) is unchanged and still reaches any
  depth of nesting. A field may now be a list or a map **on read**, reached with
  `parquet_struct_row%nested`; a field that is itself a struct is still refused, and its leaves are
  read by their dotted paths as before. A `date`, `time` or `timestamp` field is written at
  microsecond resolution — a struct's fields carry no unit declaration — and a value with finer
  precision is refused rather than truncated. **Not yet**: writing a struct whose field is a
  container.
- **`parquet_map`: `parquet_map_column`, a `MAP` column, plus `parquet_map_row`, a lightweight
  handle to one of its rows.** Every row holds zero or more `key -> value` entries; keys are
  strings and the value kind is fixed by `%init(value_kind)` and covers the nine scalar kinds.
  Duplicate keys are preserved in the order given, so `%get(key, value)` returns the first match,
  `occurrence=` selects a later one, and `%key_count`/`%contains_key` answer about them;
  `%get_at`/`%key_at` walk a row positionally. A row may be a null (absent) map, a present but
  empty one, or hold an entry whose value is null — three states, all tracked separately, and a
  key is never null. Every lookup can warn or report `found=` instead of aborting. Deep copy,
  move, and a `%gather_rows` rebuild that serves reordering, filtering and duplication. A
  `parquet_column` can take ownership of one through `%adopt_container`. A new Arrow-free entry
  module (`use parquet_map` compiles eleven Fortran files). `parquet_read_column`/
  `parquet_read_column_chunk` read a `MAP` column from a Parquet file straight into one, whole or
  one row group at a time; `parquet_write_column`/`parquet_write_column_chunk` write one back out,
  declared in a schema as `map[<valuetype>]`. A value may now be a container **on read**, reached with
  `parquet_map_row%nested`, and `parquet_get_map_value_type` reports `list`/`map`/`struct` for one.
  A map under a `STRUCT` path (`"person.attrs"`) is now readable too, closing a name that was
  listed and would not resolve. **Not yet**: its keys must be strings, and writing a map whose
  value is a container is refused. A
  map's total entry count is capped at 2,147,483,647 — Arrow has no `large_map` to widen into, so
  a column past it is refused rather than written in a wider form.
- **A `parquet_table` column can be a list, a map or a struct.** A `MAP` column is classified as a
  `parquet_map_column` automatically; a variable-length `LIST` column becomes a
  `parquet_list_column` when `parquet_open_table`'s new `list_columns="container"` argument asks
  for it, instead of being measured for a uniform width (`"auto"`, the default, is unchanged); a
  `parquet_struct_column` is added in memory with `%add_column`. Each gets the same five accessors
  a `parquet_string_column` has — `%col`, `%get`, `%set`, `%add_column` and a column handle's
  `%ref` — and none takes an `is_valid=` mask, because a container carries its own per-row
  nullness. `%is_null(name, i)`, the rank-1 `%get_valid_mask` and `%ensure_validity` answer; the
  element-granular forms are refused. Every row-structural mutation (`%sort_by`, `%filter_rows`,
  `%delete_rows`, `%truncate`, `%top_n`, `%append`) carries a container column along, though one
  may not be a sort key. `%print_stat` reports the shortest and longest row in place of a minimum
  and maximum, and `parquet_write_table` writes all three back out with a temporal payload's
  resolution intact. **Not yet**: no `%get_slice`, `%get_element` or row-handle access, which
  address a fixed-width cell a container row does not have.
- **`parquet_get_map_value_type(reader, name, type_name)`** reports a map column's value type from
  the file schema alone, or `"unknown"` for a column that is not a readable map. It is what
  `parquet_get_column_type` cannot answer, since that query reports `"unknown"` for every map.

### Changed

- **`parquet_stats` no longer copies the population when it has nothing to exclude.** Pass one
  deferred its compaction, so an unmasked, unweighted, NaN-free call allocates, writes and frees
  one array less. `pf_variance` over 10000000 elements is **2.3x faster serially**, and threading
  the same call goes from a loss to a gain — measured 1.07x before and 1.91x after under
  gfortran, 1.08x to 1.54x under ifx, on a 64-core mask. Results are unchanged, bit for bit.
- **`STATS_MIN_PER_THREAD` lowered from 32768 to 8192 survivors per thread**, re-measured on the
  shipped `pf_variance` rather than on a replica.
- **`parquet_healpix` is faster, with no interface change.** `pf_query_disc` is 1.3-1.6x faster
  under gfortran; a disc returned in the NEST scheme is about 3x faster in either compiler;
  `pf_query_disc_alloc` is about 2x faster; and `pf_ang2pix_ring`/`pf_ang2pix_nest`/`pf_vec2pix_*`
  are 1.1-1.3x faster. Results are unchanged.
- **`pf_nth_element` and `pf_nth_quantile` are 2.5-3.9x faster on a large array**, and
  `pf_minmax`/`pf_argminmax` are faster too. A selection above a couple of hundred elements is now
  answered by ordering rather than by quickselecting, which reaches the radix path and the thread
  team. Answers are unchanged. `pf_quantiles`, `pf_median` and `pf_iqr` follow suit and now always
  order.
- **`pf_minmax` and `pf_nth_quantile` take an optional `ok=`, so an all-null population can be
  reported instead of aborting.** `ok` is `.true.` whenever a value was produced — partial nullness
  is not a failure — and `.false.` only when every value is null (and, for `pf_minmax`, NaN), in
  which case the value arguments were not written and must not be read. Omitting the argument
  restores the abort, so existing callers are unaffected. `pf_argminmax` is unchanged and still
  aborts.
- **`parquet_write_table`'s `copy_metadata=`/`metadata_keys=` no longer carry a key the writer
  generates itself** — `DATE`, `name`, the two `IVOA.VOTable-Parquet.*` keys and every
  `column.<name>.<attr>` entry. `copy_metadata=.true.` skips them; `metadata_keys=` naming one is
  now an error. A copied file previously carried a second entry for each, and for `column.*` that
  second entry was the one a reader got back.

### Fixed

- **`parquet_get_column_total_elements` reports a variable-length `list` column's element count** —
  the sum of its rows' own lengths — rather than its row count.
- **`parquet_get_col_size` and `parquet_get_column_total_elements` see through an Arrow encoding
  wrapper.** A column stored as an extension, dictionary or run-end-encoded type over a fixed-size
  list now reports that list's width rather than `1`.
- **`schema%get_field` and `schema%add_field_from` keep a temporal column's unit and UTC flag.**
  They returned the stored base token, so a `timestamp[ns,utc]` column copied with `%add_field_from`
  became a bare `timestamp` — microseconds, not UTC-adjusted — while still validating and still
  writing.
- **`schema%set_col_size` refuses a container column** instead of accepting a width that
  `parquet_write_column` then rejected as an "array size mismatch", a message naming the caller's
  data rather than the declaration.
- **Concurrent `parquet_open_reader`/`parquet_open_writer` calls no longer corrupt the heap while a
  file date is pinned.** Mirroring `parquet_set_file_date` to the C++ side reassigned a
  process-global `std::string` on every open, so two threads freed the same buffer; the process
  then aborted elsewhere with glibc's `malloc(): unaligned tcache chunk detected`. Every mirrored
  setting is now atomic or mutex-guarded.
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
