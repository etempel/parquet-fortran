---
paths:
  - "src/parquet_core.f90"
  - "src/parquet_io.f90"
  - "src/parquet_read*.f90"
  - "src/parquet_write*.f90"
  - "src/parquet_metadata*.f90"
  - "src/parquet_maml_base*.f90"
  - "src/parquet_bindings.f90"
  - "src/parquet_wrapper.cpp"
  - "schemas/**/*"
  - "test/test_reading*.f90"
  - "test/test_writing*.f90"
  - "test/test_filter*.f90"
  - "test/test_metadata*.f90"
  - "test/test_maml*.f90"
  - "test/test_sort.f90"
  - "test/test_{list,map,struct}_*.f90"
---
# Reader and writer invariants

Behavioural rules of the file layer (`parquet_core` and its submodules, `src/parquet_wrapper.cpp`).
Column, string, temporal-element and table rules are in `columns-tables.md`.

## Filter evaluation: a Null is UNKNOWN, a NaN is a VALUE

`eval_filter_clause` (`parquet_wrapper.cpp`) answers `kUnknown` for a Null row on every comparison
and an ordinary `kTrue`/`kFalse` for a NaN row (a NaN is excluded by `>`/`>=`/`<`/`<=`/`==` and
survives `/=` and any negated comparison). Never harmonise the two.

- Only `is_null`/`is_not_null` answer two-valued on a Null row. `is_nan`/`is_not_nan` are
  `kUnknown` on Null and restricted to `FLOAT`/`DOUBLE`/`HALF_FLOAT` (`error stop` otherwise).
- A bare `nan` comparison value is rejected; `inf` is accepted.
- `x is_nan` ≡ `not (x >= 0 or x < 0)`; `test_filter.f90` uses it as an independent oracle. Copy
  that pattern for any future operator that is sugar over the existing grammar.

## The row-group statistics screen: every uncertainty DECLINES

`screen_row_groups` prunes row groups a filter provably cannot match; its failure mode is a silent
wrong answer, so every rule keeps the failure direction at "prune nothing":

- Every gate returns `kScreenAnything` (`may_true`, `may_false`, `may_unknown` all true) on any
  doubt: absent statistics, unusable ordering, unsupported type, unparseable literal.
- `AND`'s `may_true` is `a.may_true && b.may_true` and stays that over-approximation (per-column
  marginals carry no joint information).
- For a `FLOAT`/`DOUBLE` leaf `may_false` is unconditionally `nn > 0`, and so is `/=`'s `may_true`
  (a NaN sits outside `[min, max]` and compares false; reachable via `not (x >= 0 or x < 0)`).
- The screen walks the SAME postfix node list as `evaluate_nodes`, with the same stack shape, taking
  each leaf's family from the same Arrow schema expression; keep the two walks adjacent and
  structurally identical.
- Two guard pairs are individually redundant and jointly load-bearing: `is_stats_set()` + a null
  `statistics()`; `sort_order() == UNKNOWN` + the SIGNED/UNSIGNED check. Delete neither half.
  (`ColumnDescriptor::can_use_min_max()` exists in no Parquet C++ release here.)
- A pruned row group's mask segment is all-false, so downstream it is simply an empty row group.
- `live_mask` is `filter_mask` restricted to the live row groups and is what `apply_row_transform`
  filters with; `filter_mask` stays the canonical full-length object. The unpruned path
  deliberately keeps `ReadColumn` in `get_single_chunk_array`; re-measure before unifying.
- A test needs both halves: A/B equality against `parquet_debug_set_disable_statistics_prescreen(1)`
  AND an assertion on `parquet_debug_get_row_groups_pruned()`. Both hooks are process-global, so the
  `filter_screen` suite is excluded from per-test parallelism.

## Row transforms: a MASK or a PERMUTATION

- `filter=`/`sample_fraction=` install a boolean mask; `sort_by=` installs an `Int64Array`
  permutation (`sort_perm`). `apply_row_transform` applies filter first, then sort, at the single
  choke point every whole-column decode goes through.
- A mask only removes rows, so row groups stay contiguous and chunked reads,
  `parquet_get_chunk_size`, row mode and element mode all work under one (row-group-scoped against
  each group's surviving count). A permutation reorders, so nothing row-group-scoped survives it.
- **Every "not while transformed" check keys on `reader_has_sort_permutation` (C++) /
  `check_reader_no_sort` (Fortran), never on the mask** (`check_row_group_reads_guard_against_sort`).
  Row mode and element mode each route their whole-column fallback through ONE decision point
  (`fetch_row_mode_array`; a branch inside `stream_element_mode_row_groups`).
- Under a filter, `row_index`/`elem_index` address the filtered result: `resolve_row_group_for_row`
  and `stream_element_mode_row_groups` walk `row_group_effective_rows`, and
  `get_row_group_chunk_array` returns a chunk with the row group's mask segment already applied.

## The sort engine (`parquet_wrapper.cpp`)

- `SortKeyData`, `sort_compare_key`, `sort_build_permutation` hold no reader state; only
  `sort_bind_arrow_key` touches Arrow. Never reach for reader state under that banner.
- The ordering must reproduce `arrow::compute::SortIndices` exactly: nulls/NaNs absolute (never
  flipped by `descending`), ascending gives values → NaNs → nulls, ties hold file order.
- The integer counting-sort fast path is a second code path producing the same answer; it keeps its
  own tests plus `parquet_debug_set_disable_sort_counting_path` for comparison.
- A read-time sort with `prefetch=.true.` must not release key columns the prefetch then rebuilds
  (`keep_cache`, decided by the caller's own argument, not a knob).

## A written column's nullability is a CONTRACT with the array beside it

`build_field` (`parquet_wrapper.cpp`) decides the field's `nullable` flag (`feature_risks.md`
Risk-82):

- A whole-column write decides from the VALUES (`has_any_null`; `false` for a null mask pointer).
- A streamed write decides from mask PRESENCE on the FIRST row group (`resolve_chunk_nullability`);
  every later row group must use the same masked/unmasked form; a mismatch is a hard error both
  ways.
- A protected column is always non-nullable on every path (the per-writer `protected_columns`
  registry).
- The always-nullable kinds are `date`, `time`, `timestamp` and `parquet_string_column` (nulls live
  inside the element). `date` is int32-backed and goes through `append_typed_column_chunk`, so it
  needs `always_nullable` set explicitly — a rule touching only `stash_temporal_column_chunk` misses
  it.
- Five chunk-write sites (`append_typed_column_chunk`, `parquet_write_string_column_chunk`,
  `parquet_write_string_array_column_chunk`, `parquet_write_string_column_chunk_buffers`,
  `stash_temporal_column_chunk`); a sixth must call `resolve_chunk_nullability`.
- Invariant: a non-nullable field never receives an array containing nulls (an absent mask reaches
  the builder as a null `valid_bytes`).
- A field and its array must agree or `Table::Validate()` fails at close. `align_array_to_field`
  restamps the array's type from the field wherever the two are stored together (`append_column`'s
  two branches, every chunk site's `pending_chunk_arrays`).
- The fixed-size-list child field stays named `item` (what `FixedSizeListType(DataType)` supplies
  and what `DataType::Equals` compares); the file always carries `element` regardless.
- When a parameter STOPS being ignored, grep every call site that omitted it and check what the
  defaults now mean.

## BYTE_STREAM_SPLIT for float columns

`apply_float_byte_stream_split` is called at both `WriterProperties::Builder` sites
(`parquet_finish_row_group`'s first-row-group path and `close_parquet_writer`) and applies
`disable_dictionary(name)` **and** `encoding(name, BYTE_STREAM_SPLIT)` to every `float32`/`float64`
field. Keep the pair together on the same columns (the encoding request is a silent no-op while
dictionary stays enabled) and keep both call sites. The `parquet_debug_write_*` fixture writers are
deliberately excluded. Only an external tool (`pyarrow`'s per-column `encodings`) can confirm the
physical encoding; a Fortran test can only confirm the data round-trips.

## Validity masks: ask the footer first

- Requesting `is_valid=` is expensive (int8 buffer, 32-bit `LOGICAL` mask, conversion pass,
  `IsValid` scan; a per-row `set_null` replay on the table path). `parquet_column_has_nulls`
  (public) over `column_has_nulls_from_footer` (C++) answers from the footer's null count; every
  `mat_*`/`matchunk_*` asks first and omits `is_valid=` when the answer is no.
- The uncertain answer is `.true.`. A dotted struct path is declined outright (a leaf's validity is
  combined with its ancestors' by `unwrap_struct_path`).
- An active filter or sample does not invalidate the answer. `matchunk_*` scopes the question to
  its own row group.
- `is_stats_set()` and `HasNullCount()` are mutually redundant and both load-bearing
  (`test/fixtures/no_stats.parquet`); delete neither.
- Keep both fallbacks: `check_or_report_nulls` short-circuits to `memset` when
  `null_count() == 0`; the per-row replay sits behind `if (.not. all(valid))`.

## Measuring a column's width never materialises it

- `parquet_get_col_size`/`parquet_get_column_total_elements` answer from the schema for every type
  except a plain `LIST`/`LARGE_LIST` (`needs_data_to_measure_col_size` is the single predicate).
  `STRUCT`/`MAP` never reach the question.
- A plain `LIST` is resolved in two tiers, both kept: a footer screen (`list_width_candidate`:
  `num_values / num_rows` per row group; a non-integral mean or disagreeing row groups prove no
  uniform width; a survivor is a candidate, never a proof), then a row-group scan
  (`list_width_verified`) bailing at the first disagreement, through
  `read_row_group_array_for_measuring` — never `get_row_group_chunk_array`, which records the row
  group as read and runs qc.
- `parquet_table` resolves a deferred column with the unproven candidate
  (`table_resolve_width(..., proven=.false.)`) because `get_uniform_list_values` aborts on a
  mismatch; only `%kind`/`%width` pass `proven=.true.`.
- An empty column measures 0, not 1 (the table descriptor clamps with `max(w, 1)` itself); a slice
  measures over its own row groups.
- A new non-touching table query is checked against this: anything reading `declared_kind` or
  `width` outside `table_resolve`/`table_resolve_width` reads `PK_NONE`/0 for a deferred column.
  `%unit`, `%residency` and `%is_supported` deliberately do not touch.

## Read side: row-group scoped, never a whole-column read

- `parquet_get_col_size`, `parquet_get_column_total_elements`, `parquet_read_array_row_mode` and
  `parquet_read_array_element_mode` never materialise a whole column (Arrow's internal int32
  list-offset limit trips once `nrows * col_size` crosses int32 even though every row group was
  written safely). Row mode resolves one row group (`resolve_row_group_for_row`,
  `get_row_group_chunk_array`); element mode streams row group by row group
  (`stream_element_mode_row_groups`, `resolve_element_mode_col_size`).
- The one remaining whole-column read here is `list_width_verified`'s masked branch for a plain
  `LIST`.
- A path asking for `col_size` on every column of an arbitrary file (the table's open-time
  classification) is not metadata-only; release afterwards (`parquet_release_column`).
- Regression scenarios: `scenario_col_size_and_row_mode_avoid_whole_column_read`,
  `scenario_filter_row_element_mode_no_whole_column_read`, and the negative control
  `scenario_whole_column_read_forced_error_control` (all on `g_debug_force_whole_column_read_error`).

## String column I/O

- `parquet_string_column` is a specific of `parquet_write_column`/`_chunk` and
  `parquet_read_column`/`_chunk` through buffer-passing C++ entry points
  (`parquet_append_string_column_buffers` and the read-side buffer fill); this path does not trim.
  Scope is scalar 1-D columns; vector/matrix string columns stay on the padded path.
- A struct-nested leaf read through the buffer path needs the validity bitmap's element offset
  threaded explicitly (`extract_string_buffers`'s `validity_offset` → `append_buffers`'
  `validity_offset_bits`); a fresh `offsets(1)==0` guard alone is not sufficient. Keep the argument
  on any new non-scalar specific.
- `column_cache` stores only the pre-`unwrap_struct_path` array. A whole-column function handing a
  raw buffer pointer across `bind(C)` for a column reachable via a struct path pins its array itself
  (`last_whole_column_buffers_array`, mirroring `last_chunk_buffers_array`).

## Temporal columns in files

- Parquet has no seconds-resolution `TIME`/`TIMESTAMP` and no `DATE64`; a MAML
  `time[s]`/`timestamp[s]` token is rejected at `add_field`/parse time (`apply_temporal_unit_token`).
  `parquet_unit_seconds` exists only for `set_unix`/`to_unix`. The `DATE64` branch in
  `convert_date_values` is defensive dead code, `GCOVR_EXCL`'d.
- `enable_deprecated_int96_timestamps()` is on `parquet::ArrowWriterProperties::Builder`, not
  `WriterProperties::Builder` (`parquet_debug_write_datetime_fixture`).

## Schemas and MAML sources

- A `parquet_schema` built with `%init`/`%add_field` holds MAML text only until
  `parquet_parse_maml(schema)` runs. Any procedure walking a caller-supplied schema guards with
  `if (.not. schema%is_parsed()) error stop "<proc>: this schema has not been parsed; call
  parquet_parse_maml(schema) after building it with %init/%add_field"` before touching `%cinfo`.
  `%is_init()` is the wrong check.
- Every read of a `.maml` file's lines goes through
  `parquet_read_maml_source_lines(filename, context, lines, nlines)` (`parquet_metadata_maml.f90`):
  it enforces `maml_max_line_len` (declared once in `parquet_metadata.f90`; never hardcode 1024) by
  detecting an over-long line via non-advancing read, and strips a trailing `char(13)`.
