# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **Sorting for plain Fortran arrays and column types**, in a new `parquet_sorting` module
  re-exported by `use parquet`. `pf_argsort` returns the permutation that would sort an array,
  `pf_sort` an independent sorted copy, `pf_permute` applies a permutation in place and
  `pf_is_sorted` tests an existing order — over eleven element types: the four numeric kinds,
  `logical`, `character(len=*)`, `parquet_date`/`parquet_time`/`parquet_timestamp`,
  `parquet_string_column` and `parquet_column`. Multi-key sorting goes through `pf_sort_keys`
  (`k%add(...)` once per key, keys of any mix of types, each with its own direction), since a
  Fortran generic cannot offer "an optional second array of any type"; the same object also works
  with `pf_is_sorted` and `pf_partial_argsort`, and `%nkeys_added()` counts the keys you added, one
  per `%add`, whatever their types. These run on the **same C++
  engine** as a read-time `parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`, so the
  three can never disagree about null placement, NaN placement or tie order; every sort is stable,
  `descending=` reverses the values without moving the null/NaN tiers, and the six types with no
  null state of their own take an optional `is_valid=` mask. `pf_permute` validates that its
  permutation really is one before writing anything, since an invalid one silently duplicates some
  elements and drops others; pass `assume_valid=.true.` to skip that for a permutation from
  `pf_argsort`. Public names here carry a `pf_` prefix rather than `parquet_`, because their
  subject is not a parquet file. **Group boundaries** come out of the same pass as the sort:
  `pf_argsort(..., group_offsets=go)` reports where each run of rows comparing equal begins, as
  offsets into the permutation with a trailing sentinel, so group *g* is `perm(go(g):go(g+1)-1)`
  with no special case for the last one — all nulls form one group and all NaNs form one group,
  deliberately unlike `pf_unique`, which drops nulls entirely. `group_nkeys=` narrows the grouping
  to the leading keys **without changing the sort**, and counts the keys you added rather than the
  engine's own (a `parquet_timestamp` key becomes two internally, and this never exposes that). **Selection** comes with it: `pf_partial_sort`/`pf_partial_argsort`
  order only the first `n` elements (`n` is clamped to the array size, so an `n` derived from a
  row count needs no `min()`; "the last n" is `descending=.true.`), `pf_nth_element` reports the
  value — and optionally the index — a full *stable* sort would place at a given rank without
  sorting, and `pf_nth_quantile` takes a quantile on a **0-1 scale** over the non-null values, with
  a `rounding=` of `"nearest"`/`"down"`/`"up"` and an optional `n_null=`; it aborts rather than
  return an undefined value when every value is null. `pf_sort`/`pf_partial_sort` also take an
  optional `sorted_valid=` reporting which of the *sorted* values are null, since the mask handed
  in describes the input order. The module also **searches, deduplicates, ranks, reduces and
  merges**: `pf_lower_bound`/`pf_upper_bound`/`pf_equal_range` locate a value in an already-sorted
  array in O(log n) — each checking that the array really is sorted first, since searching unsorted
  input returns a plausible index with no symptom, with `assume_sorted=.true.` to pay that O(n)
  check once rather than per call; `pf_unique_count`/`pf_unique` report the distinct non-null
  values (exact equality, so `0.1 + 0.2` and `0.3` are two values, while every NaN is one), with an
  optional `n_null=`; `pf_rank` gives every element its rank in place order, with
  `method=`"competition"/"dense"/"ordinal" and rank 0 for a null; `pf_minmax`/`pf_argminmax` give
  the smallest and largest value, and where they are, skipping nulls and NaNs and aborting when
  nothing is left; and `pf_merge` merges two sorted arrays in linear time, taking each input's
  validity mask and producing a merged one. **Sorting is parallel by default**: `pf_argsort`,
  `pf_sort`, `pf_unique_count`, `pf_unique` and `pf_rank`, along with the read-time
  `parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`, use `omp_get_max_threads()`
  automatically — and stay serial inside an OpenMP parallel region, since a nested region is the
  caller's business. An optional `threads=` turns that down (`threads=1` forces serial) and is
  honoured wherever it is given; `pf_sort_threads()` reports what an automatic sort would do. The
  answer is bit-identical at every thread count, because the comparator is a total order under
  which no two rows compare equal, so `threads=` is purely a performance control. The chunks each
  thread sorts are combined by a **co-ranked merge** — every round is partitioned by binary search
  so each thread merges a disjoint slice of the output, rather than merging pairwise and ending in
  a single-threaded pass over the whole array — which is worth roughly 4.3x over serial at a
  million rows and 4.4x at twenty million on an 8-core laptop; the remaining gap to the thread
  count is memory latency, since every comparison chases a scattered key. Note that a threaded
  sort's peak memory is about twice a serial one's. See
  [Sorting arrays and columns](doc/pages/sorting.md).
- **Generated table types.** `tools/generate_user_table_code.py` turns a MAML schema into a named
  `parquet_table` extension with one accessor per declared column, so a program that always reads
  the same columns can write `t%ra()` instead of naming strings everywhere. Each accessor comes in
  three zero-copy forms — `%ra()` for the whole column, `%ra(i)` for one row and `%ra(lo, hi)` for a
  row range, in both integer index kinds — where the index always means a ROW; a `string` column
  additionally gets a `%name_chr(arr)` character copy-out, and a string vector column gets only
  that. The type is opened with `%init`, `%init_slice` (a contiguous row range) or `%init_empty`
  (in memory, no file), each of which checks every declared column against the file, converts it to
  the declared kind (widening silently, warning when a conversion can lose information, and taking
  an optional `exact=`), and reads them all in one pass. A field declared `source: computed` — a new
  `fields:` sub-key, accepted and ignored elsewhere — gets its accessor and a slot of null rows but
  is never looked for in the file. The generated module is **user-editable in six marked windows**,
  preserved across regeneration, and `--check` fails the build if a generated region was hand-edited
  or the file is stale; the generator also reads the components window and writes the matching
  `%clone` and reset statements itself. Two public entry points on `parquet_table` support this and
  are usable from hand-written extensions too: `clone_extra`, the hook `%clone`/`%clone_structure`
  call so an extending type's own components are copied rather than silently default-initialized,
  and `%bind_predefined`, which performs the column binding. `parquet_write_table` now accepts any
  type extending `parquet_table`. A worked example of the generator's output ships as
  `src/parquet_table_example.f90` — nothing else in the library uses it and `use parquet` does not
  re-export it, so it is an example rather than part of the API. See
  [Generated table types](doc/pages/generated-tables.md).
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
  `screened:` line whenever row groups were skipped. A scoped filter can also be narrowed to an
  exact **row** range —
  `parquet_reader_set_filter(reader, filt, row_group_lo, row_group_hi, row_lo, row_hi)` — for a
  range that begins or ends inside a row group rather than on a boundary; the filter may hold no
  rules at all in that form, letting the range stand on its own. What a filtered reader retains
  now scales with the rows it actually covers rather than with the file's total row count, so a
  scoped filter on a file far larger than its own scope no longer holds a mask sized to the whole
  file. Finally, the columns a filter's rules — or a sort's keys — refer to can be renamed
  wholesale with `%remap_column_names(from, to)`, for callers that build a filter in one
  column-name vocabulary and apply it in another; the substitution works on the parsed expression,
  so a quoted literal that happens to spell a column name is never touched, and all the renames in
  one call happen at once. See
  [Row filtering](doc/pages/reading.md#row-filtering-with-parquet_filter) and
  [Renaming the columns a filter or sort refers to](doc/pages/reading.md#renaming-the-columns-a-filter-or-sort-refers-to).
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
  (`%drop_column`, `%rename_column`, `%copy_column`, and `%cast`, which converts a column to
  another numeric kind in place so that `%col` can be called with the kind the calling code wants
  rather than the one the file holds), or a row at a time (`%filter_rows`,
  `%sort_by`, `%delete_rows`, `%truncate`, `%append`, `%append_null_rows`) — and `%clone` takes
  an independent deep copy, which is how a version is kept to go back to, since mutation is in
  place. Changing the **row set** detaches the table from its file: the rows in memory no longer
  line up with the rows on disk, so any column not read by then can never be read, `%is_detached`
  reports it, and every later read from the file is a clear error rather than misaligned data.
  `%sort_by` runs the same C++ sort engine as the read-time `sort_by=`, so sorting a table in
  memory and reading the same file sorted give the identical row order, and it reads a key column
  that has not been read yet rather than refusing. Ordering can also be asked for **without** being
  applied, which is the only way to read a table's rows in an order and keep the file behind them:
  `%argsort_by` returns the row order the keys imply (consumed with
  `%get_slice(name, parquet_slice_list(perm), values)`, and equally applicable to another table or
  a plain array), `%argsort_partial` returns just the `n` best rows by selection rather than a full
  sort — the difference that matters when a 100-million-row table is asked for its brightest 100 —
  and `%is_sorted_by` answers whether the rows are already in that order in O(rows) with an early
  exit. None of the three detaches. `%top_n(keys, n)` is the mutating counterpart of
  `%argsort_partial`: it reduces the table itself to those `n` rows, in key order, selecting rather
  than sorting everything and gathering each column straight to `n` rows instead of reindexing it in
  full and cutting it down — `n` is clamped to the row count, "the last n" is `descending=`, and at
  or above the row count it is simply a whole sort. There is deliberately no way to apply a
  permutation you built yourself to a table: partially applied, or applied from an array that has
  since gone stale, it would break the row correspondence silently. `%argsort_by` optionally reports where each run of equal rows
  begins (`group_offsets=`, with `group_nkeys=` to group on the first few keys while still sorting
  by all of them, which is "grouped by field, ordered by magnitude within each group"). A
  permutation describes the table **as it was**: any row-structural change silently invalidates it,
  and `%generation()` is how to check before reusing one. A table can also be opened
  against a **read-in MAML** (`parquet_open_table(t, file, maml=...)`) whose `extra: remap:` block
  gives the file's columns table-facing names of the program's own, so code works in one stable
  vocabulary whatever a particular file calls things; an internal name may deliberately shadow a
  file column it does not refer to, and two internal names may read one file column as two
  independent table columns. A table can be filtered, sorted, sampled and qc-checked as it opens,
  with `parquet_open_table(t, file, filter=, sort=, qc=, qc_soft=, sample_fraction=,
  sample_seed=)`: every column then covers exactly the surviving rows in exactly that order,
  including ones read lazily long afterwards, and `%clone` reattaches the same transform to its own
  reader. `filter=`/`sort=`/`qc=` name columns in the table's own internal vocabulary — the names
  `%get`/`%col` use, translated through `extra: remap:` for you — while a read-in MAML's own
  `extra: filter:`/`extra: sort:`/`fields: qc:` name the file's columns, because a read-in MAML
  describes the physical file. Both sources compose: filters AND, the MAML's sort keys lead and
  the caller's break ties, and qc is a per-column override. `extra: sort:` entries additionally
  accept a trailing `nulls_first`/`nulls_last` token, which a plain YAML string list has nowhere
  else to carry. A **slice** takes the same transform, applied within its own row range, so a
  filtered or sampled slice holds the rows of `[row_lo, row_hi]` that survive and never one from
  outside it — meaning `%nrows()` is then no longer `row_hi - row_lo + 1`, and row 1 is the slice's
  first surviving row. Without a filter or a sample a slice behaves exactly as it always did.
  Sorting is the one exception: the slice forms have no `sort` argument at all (a sort reorders
  rows across the whole file, so a row range would no longer name the rows that were asked for),
  and a read-in MAML whose `extra: sort:` is non-empty is refused on a slice open.
  `%row_group_bounds` gained a `physical=` argument for exactly this: it answers in the table's own
  row numbering by default, relating a row index in hand to the row group it came from, and in the
  file's with `physical=.true.`, which is what the next slice has to be chosen in. Both forms have
  one entry per physical row group and are index-aligned, so they can be read side by side; a row
  group contributing no rows to the table is an empty range rather than a dropped entry, and
  `physical=.true.` answers for every file-backed table whatever transform it carries — a sorted
  table, which cannot answer in its own row numbering at all, included. A **row mutation that
  changes no row does not detach**: `%truncate(n)` with `n` at or above the row count,
  `%filter_rows` with an all-`.true.` mask, `%delete_rows` with no indices, `%append` of a zero-row
  table, `%append_null_rows(0)` and a `%sort_by` whose rows were already in that order all return
  without touching a column, without invalidating a `%col` pointer and with the file still
  attached. `parquet_write_table` parses a schema built with `%init`/`%add_field` itself, so
  calling `parquet_parse_maml` beforehand is optional (the schema is left parsed afterwards), and
  takes `copy_metadata=.true.` (or `metadata_keys=[...]`) to carry the source file's own key/value
  metadata into the output, where a key the schema declares itself wins and nothing is added to
  the caller's schema. It also takes every writer option `parquet_open_writer` takes —
  `write_maml`, `qc`, `compression`, `compression_level`, `chunk_size`, `use_threads`,
  `overwrite` — forwarded untouched and with the same defaults, so a table write and the
  equivalent hand-written open produce the same file; and `release=` (default `.true.`), which
  leaves the table in the residency state the write found it in by giving back each column the
  write itself had to read, while leaving alone any column the caller had already materialized.
  The `schema` argument is now **optional**: without one, `parquet_write_table` writes whichever
  columns are currently resident, in slot order and under their own internal names, reading
  nothing — a quick path for a small or temporary table, where a table holding nothing resident
  writes a valid empty file and the automatic `parquet_row_index` column is never written. With
  `write_maml=.true.` such a write also emits a sidecar `.maml` generated from the table's own
  columns (name, type, resolved `col_size:`/`array_size:`, and `unit:` where there is one, under a
  `table:` name taken from the output file's stem), and that file is a valid `maml=` argument for
  reopening the file it describes — so a table written this way round-trips complete with the
  units a parquet file cannot itself carry. Temporal columns keep their stored resolution through
  that round trip: a `timestamp[ns]` column is written back as nanoseconds rather than coerced to
  the writer's microsecond default. `%prefetch(PARQUET_ROW_INDEX)` now materializes the automatic
  row-index column, like `%get`/`%col` already did, instead of reporting a column of that name as
  missing; `%materialize_all` still does not create it. The source file's metadata is snapshotted when the table opens, so
  `%get_file_metadata` — and that carry-over — keep working after a row mutation has detached the
  table from its file. **Column units now come from a read-in MAML**: a `fields:` entry's `unit:`
  key gives that column its unit, matched on the file's own column name, answered by `%unit`
  before the column has been read and carried onto the values once it has.
  Value access gained `is_valid=` on `%get`, `%col`, `%get_slice` and `%set` — per-row validity
  alongside the values in one call, and on `%set` the way to write a column and its nulls together
  — `%get_element(name, i, value)`, the read counterpart of `%set_element`, widening into the
  caller's variable exactly as `%get` does and taking either integer kind for the row index, and
  `%set_slice(name, s, arr)`, which writes a `parquet_slice` selection back in the order the
  selection names. A `parquet_string_column` is now a first-class table value in both directions:
  `%col` hands back a pointer to a `PK_STRING` column's packed store (read and in-place value
  edits only — changing its element count would leave the column's row count describing something
  else), and `%set`/`%add_column` accept one, so a compact string column no longer has to be
  flattened into a fixed-width character array to be put into a table. A row handle can write as
  well as read: `r%set(name, value)` updates the table it is a view of, and `r%ref(name, p)` gives
  a zero-copy pointer to that one row's storage (every kind but the two string ones, which have no
  fixed slot to point at). **`found=` now reaches every procedure that takes a column name**,
  mutators included — and on a mutating call `found=.false.` means nothing was changed, since the
  column is looked up before anything is written.
  Introspection gained `%has_nulls(name)` (from the file's own footer for a column that has not
  been read, so it costs no I/O), `%get_valid_mask(name, mask)`, a `%set_null(name, is_valid)`
  form taking a whole-column mask, `resident_only=` on `%ncols`/`%column_names`/
  `%clone_structure`, and `%generation()` — a counter bumped by every structural change, for a
  caller holding a `%col` pointer across a call that might have invalidated it (Fortran cannot
  detect a stale pointer; this is how to find out whether to re-fetch). `%clone_structure` takes
  each column's shape from the schema rather than from values, so it works on a table that has
  read nothing — the normal case for the bulk-append idiom it exists to serve — instead of
  aborting inside the column store. `%drop_column` no longer copies the remaining columns' data
  when it closes the gap. Note qc is enforced when a column is actually read, so
  on a lazy table it lands on first touch rather than at open. See
  [the guide](doc/pages/table.md) for the current limitations, the detach rule and the OpenMP
  first-touch rule.
- Added `parquet_read_qc` and `parquet_compose_read_qc`: read-time quality control declared in
  code and held **unresolved** until it can be composed against whatever a file's own qc-MAML
  declares. `qc%add("mass, >0, <=1000, Null")` takes the same compact string
  `parquet_schema%add_col_qc` already documents, so there is one read-time-QC grammar rather than
  two, and `qc%remap_column_names(from, to)` renames the column each entry declares (only the
  entry's first field, so a bound spelling a column name is never rewritten).
  `parquet_compose_read_qc` merges the two sources into the single schema
  `parquet_open_reader(..., schema=)` takes, per column rather than per bound: a column whose MAML
  `fields:` entry carries a `qc:` key at all — including an empty one, which already means "no
  Nulls here" — keeps the MAML's declaration in full and drops the code's entirely, while a column
  the MAML never mentions (or merely names, without a `qc:` key) takes the code's. See
  [Deferring qc declarations](doc/pages/quality-control.md#deferring-qc-declarations-with-parquet_read_qc).
- Added `parquet_columns` (`parquet_column`): type-erased, whole-column value storage with sparse
  null tracking, covering 18 scalar/vector kinds plus reserved slots for the future
  list/map/struct column types, with the full value and structural instruction set — including
  `%adopt`, which makes a caller's array the column's storage outright via `move_alloc` instead
  of copying into freshly allocated storage, `%paste`, which overwrites an existing row range
  from another column of the same kind and width without reallocating anything, `%gather`, which
  keeps the rows an index list names in the order it names them (the subset-and-reorder neither
  `%reindex` nor `%delete_by_mask` covers — the list may be any length, in any order, and may repeat
  a row), and `%move_from`,
  which hands a column's whole storage over to another column rather than copying it. It is the shared
  foundation `parquet_table` and the container column types are built on; its per-kind blocks are
  generated by `tools/generate_parquet_columns.py` (committed output, no build-time step).
- Added reader queries that answer without materializing a column: `parquet_get_column_names`
  (every column in a file, expanding a nested struct into one dotted leaf path per leaf),
  `parquet_get_physical_row_indices` (which file row each row a transformed reader returns came
  from, in order),
  `parquet_get_qc_columns` (the columns a qc schema actually constrains, as opposed to merely
  names), `parquet_get_metadata_items` (every key/value metadata entry a file carries, as two
  index-aligned arrays — the counterpart to `parquet_get_metadata`, which answers for one key the
  caller already knows),
  `parquet_release_column` (frees a column's decoded Arrow buffers once the caller has its own
  copy), `parquet_column_has_nulls` (from the file's own statistics, no column data read —
  `.false.` is a guarantee, `.true.` means "assume the worst"), and `parquet_measure_list_width`
  with `parquet_column_width_needs_data` (a variable-length `LIST`/`LARGE_LIST` column's uniform
  elements-per-row width, from the footer alone or confirmed by a scan that never holds more than
  one row group).
- Added bulk `reindex`, `delete_by_mask`, `gather` and `append_nulls` operations to
  `parquet_string_column`.
- Added `tools/check_bindc_boundary.py`, wired into CI, to catch `bind(C)`/`extern "C"` signature
  mismatches between Fortran and C++.

- **A `parquet_table` can now be used from several threads, with the library enforcing the rules
  rather than documenting them.** `%append` into a *shared* table is serialised by the table itself,
  so a parallel producer region needs no `!$omp critical` of its own: each thread analyses its own
  slice and appends its results to one destination. `%prefetch`/`%materialize_all` now read a wide
  file's columns **in parallel internally**, with no OpenMP in the calling code at all — measured at
  3.9x on a 24-column, 900k-row file with 8 threads (it falls back to the ordinary serial read when
  there is a read-time `filter=`/`sort=`/`qc=`/`sample_fraction=`, since a second reader would have
  to repeat that work). New `%ensure_validity([name])` materializes a column's validity storage up
  front, which is what makes nulling elements of one column from several threads safe — validity is
  allocated lazily, so the *first* null would otherwise allocate, and two threads doing that race.
  `parquet_string_column` gains the matching `has_validity`/`reserve_validity` pair.

  **Every remaining single-threaded requirement is now a hard `error stop` naming what to do
  instead, not a documented convention**: changing a shared table's structure inside a parallel
  region (`%add_column`, `%drop_column`, `%rename_column`, `%copy_column`, `%cast`, `%evict_column`,
  `%reload`, `%filter_rows`, `%sort_by`, `%delete_rows`, `%truncate`, `%append_null_rows`,
  `parquet_write_table`), reading a shared table while another thread appends to it, nulling a
  column whose validity storage does not exist yet, and writing to a string column (whose rows share
  one packed store, so a write can move the whole payload). A table a thread opened *itself* inside
  the region is thread-private and is deliberately exempt from all of them, which is what keeps the
  per-thread slice pattern working. Reading an already-resident column stays completely free — no
  lock, no atomic, any number of threads. Three cases remain undetectable and are documented as
  such: a pointer you already hold, threading the library cannot identify (pthreads, coarrays), and
  the exact instant a violation begins. See
  [Thread safety](doc/pages/thread-safety.md) for the full per-operation table.

- **Process-global settings, in a new `parquet_settings` module** re-exported by `use parquet`, for
  the parameters that apply to the whole library rather than to one reader, writer or table.
  `parquet_get_arrow_threads` is new and answers what `parquet_set_arrow_threads` (renamed from
  `parquet_set_max_threads`, see Changed below, and now living here) has set Arrow's shared CPU
  thread pool to — useful in batch and HPC work, where
  `OMP_NUM_THREADS` is chosen for the science code and the parquet layer would otherwise inherit it.
  Five more knobs supply a program-wide **default** that an explicit argument still overrides:
  `parquet_set_sort_threads` and `parquet_set_prefetch_threads` cap the threads used by every sort
  and by `parquet_table`'s internally-parallel column read (a cap, never a request — and neither
  lifts the rule that an unqualified sort inside your own OpenMP parallel region stays serial), while
  `parquet_set_default_compression`, `parquet_set_default_compression_level` and
  `parquet_set_default_use_threads` set what `parquet_open_writer`/`parquet_open_reader` use when the
  corresponding argument is omitted — so a project standardising on a codec no longer has to pass it
  at every call site. **Terminal output is controllable too**: `parquet_set_verbosity` takes
  `"normal"`/`"silent"`/`"errors_only"`, and `parquet_set_message_stream` moves the library's own
  messages between `"stdout"` and `"stderr"` — useful when a program pipes its own standard output
  to a data consumer. Errors are never suppressed at any level, but note that `"silent"` does turn
  explicitly-called printers (`%print_stat`, `%print_schema_info`, `parquet_string_column`'s
  printers, `parquet_open_reader(..., print_stat=.true.)`) into no-ops, which is what a global
  output control means. The development-build notice is now an ordinary remark rather than a
  `WARNING`. **Five performance knobs** complete the set: `parquet_set_sort_parallel_min_rows` is the
  row count below which a sort refuses to thread at all, `parquet_set_sort_counting_path` and
  `parquet_set_sort_counting_bucket_limit` govern the integer counting fast path (the limit bounds a
  key's value *range*, not its cardinality, and is the memory control — `n` buckets cost `8n` bytes),
  `parquet_set_target_row_group_bytes` sizes the row groups of a writer opened without an explicit
  `chunk_size=`, and `parquet_set_statistics_prescreen` controls whether a filtered read skips row
  groups its footer statistics rule out. All three numeric knobs take either integer kind and accept
  `0` for "restore the built-in value". **Every knob can also be set from the environment**:
  `parquet_settings_from_env()` applies one `PARQUET_FORTRAN_*` variable per knob
  (`PARQUET_FORTRAN_VERBOSITY`, `PARQUET_FORTRAN_DEFAULT_COMPRESSION`, …), plus
  `PARQUET_FORTRAN_THREADS` for all three thread counts at once, through the same
  validation a direct call uses, so a mistyped value aborts naming the variable rather than being
  ignored. It is called by your program, never automatically, and applies over whatever is already
  set rather than resetting. An empty variable counts as unset.
  **`parquet_set_threads(n)` sets Arrow's pool, the sort cap and the prefetch cap together**, for the
  common case of "give this library `n` threads and no more"; it takes `n >= 1` (unlike the sort and
  prefetch caps individually, `0` is not accepted, because Arrow's pool has no automatic value), and
  any individual setter afterwards overrides just that one knob.
  `parquet_reset_settings` restores what a program changed, and `parquet_print_settings`
  dumps every setting and limit to a unit. The caps the library enforces on filter rules, sort keys and MAML
  lines are published as read-only constants (`parquet_max_filter_rule_len`,
  `parquet_max_filter_depth`, `parquet_max_filter_nodes`, `parquet_max_sort_keys`,
  `parquet_max_sort_key_len`, `parquet_max_maml_line_len`), so code assembling any of those from
  user or configuration input can check a length before tripping an `error stop`. A setting may
  change how fast, how large or how loud the library runs — never what it answers, which is why
  there is deliberately no global default for null ordering or quality-control enforcement. See
  [Settings](doc/pages/settings.md).

### Changed

- **`parquet_table%sort_by` is substantially faster on wide tables** — measured at 6.22 s to 3.76 s
  on a 15.6-million-row, 25-column table, and the gap grows with both dimensions. Two things
  changed: the permutation check now uses a bit-packed seen-set rather than a `logical` array
  (gfortran's default `LOGICAL` is 32 bits, so validating an *n*-row permutation used to allocate
  4*n* bytes to record one bit per row — 59 MiB at 15.6 M rows), and the permutation is validated
  **once per sort** rather than once per column. Sorting a 24-column table used to re-verify the
  same permutation 26 times, which was over a third of its total run time. The trusted per-column
  path this needs is exposed as `parquet_column%reindex_trusted` /
  `parquet_string_column%reindex_trusted`, and `pf_permute`'s `assume_valid=.true.` now routes
  there for the two column types instead of ignoring the argument — so it means the same thing for
  all eleven element types. Both are internal plumbing, public only because Fortran offers no
  narrower visibility: a permutation that is not one silently duplicates and drops rows.

- **A `parquet_table`'s row-structural mutations now rewrite their columns on several threads.**
  `%sort_by`, `%filter_rows`, `%top_n`, `%delete_rows` and `%truncate` all replay their permutation
  or mask across every resident column, and each column is independent of every other, so the loop
  now runs in parallel. You do not ask for it and there is no new argument: it engages
  automatically on a table with at least two rewritable columns and enough data to be worth a thread
  team, and stands down inside a parallel region of your own, since a nested region is the caller's
  business. The answer is identical either way — the parallel and serial paths produce the same
  table, which is what makes it safe to do silently. On a 20.8-million-row table the per-column loop
  was measured at 67% of `%sort_by` at 24 columns and 80% at 48, so the gain grows with the column
  count; a two-column table gains nothing and does not try. **Two things to know:** the new
  `parquet_set_table_threads(n)` caps it (`1` forces the old serial behaviour, `0` is automatic),
  and because each thread holds a transient copy of the column it is rewriting, a parallel
  `%sort_by` can **double the table's peak memory for the duration of the call** — the thread count
  never exceeds the column count, so the transient copies come to at most one extra copy of the
  table. A program working near its memory ceiling should cap the threads, which caps the copies
  with them. `%filter_rows` and `%top_n` are proportionally cheaper, since their new storage is
  sized by the rows they keep.

- **`parquet_set_threads(n)` now sets four thread counts, not three** — Arrow's pool, the sort cap,
  the table prefetch cap and the new table mutation cap. A program that called it and then
  deliberately left the mutation cap alone will now find that cap set too; set
  `parquet_set_table_threads` afterwards to override just that one, exactly as with the other three.

- **`parquet_table%get_valid_mask` and `%get`/`%col`/`%get_slice`'s `is_valid=` are faster on a
  large column**, in both ranks. They used to build the mask with one `%is_null(i)` call per row;
  they now go through the column's own bulk builder, which walks the validity bitmap a 64-bit word
  at a time, skips a null-free word whole, and visits only the set bits inside the rest — so the
  cost follows the *number of nulls* rather than the row count, and a column with no nulls at all
  is a single fill. The answers are unchanged, including the rules that a rank-1 mask over a
  vector column is the row summary ("any element of this row is null") and that the mask always
  comes back allocated, all `.true.` for a null-free column, so no caller has to test
  `allocated()`.

- **BREAKING: `parquet_set_max_threads` is renamed to `parquet_set_arrow_threads`.** The old name is
  removed rather than kept as an alias, so a call to it no longer compiles; the replacement takes the
  same argument, does the same thing, and aborts on the same values. The new name says *whose*
  threads it sizes — Arrow's shared CPU pool, not OpenMP's — which is the distinction that actually
  matters in a batch or HPC job where `OMP_NUM_THREADS` belongs to the science code. It also brings
  the setter into line with the name the knob carries everywhere else: `arrow_threads` in
  `parquet_print_settings`' output, in the settings guide, and in `PARQUET_FORTRAN_ARROW_THREADS`.
  The C symbol is unchanged, so nothing outside Fortran is affected.

- **One `use parquet` now covers the whole library.** It brings the `parquet_table` container, the
  `parquet_column` foundation and its `PK_*` kind constants into scope alongside the readers,
  writers, schemas, string columns and temporal types it already carried, so a program mixing a
  table with a kind constant and a schema no longer needs four `use` statements. This is purely
  additive — every existing `use parquet` keeps compiling and gains names, and the individual
  modules (`parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal`) remain
  usable on their own for a narrower import. Internally, `parquet` is now a facade module and the
  reader/writer/schema implementation it re-exports has moved to a new module, `parquet_core`
  (`src/parquet.f90` → `src/parquet_core.f90`). **`parquet_core` is internal**: it is not covered
  by the semantic-versioning promise and may be renamed or restructured in any release — `use
  parquet` is the supported spelling. Because `parquet` re-exports rather than defines, the
  generated API reference lists each name on its implementing module's page; use the site-wide
  procedures/types listings to look a name up.

- **A vector column's nulls are now tracked per ELEMENT rather than per row, throughout the
  `parquet_table`/`parquet_column` layer.** A per-element null read from a parquet file is no
  longer widened to the whole row, and writing a table back out no longer broadcasts a row's null
  across its elements — one null element now survives a file → table → file round trip intact.
  Four consequences, the first of which is a **breaking API change**:
  - `%get`, `%col`, `%get_slice` and `%set` take (or return) `is_valid` **shaped like the values**:
    still `is_valid(:)` for a scalar column, but now `is_valid(:,:)` shaped `(width, nrows)` for the
    `int32`/`int64`/`float32`/`float64`/`logical`/`string` vector kinds. Passing a rank-1 mask for a
    vector column is a *compile* error, so the change cannot be missed; reshape it. Scalar columns
    are unaffected.
  - `%is_null`, `%set_null` and `%clear_null` gain an element form — `%is_null(name, i, e)` and so
    on, with `e` running `1..width` — alongside the existing row form. Setting or clearing by row
    still acts on the whole row; **asking** by row now means *"any element of the row is null"*
    (previously: "its first element is"), which is what makes a row containing one null element
    report itself null. The `parquet_table_row` handle takes the same pair (`r%is_null(name, e)`).
  - `%get_valid_mask` and the mask form of `%set_null` accept **either** shape: rank-1 is the
    per-row summary, rank-2 the true per-element state.
  - `modify_nulls=.false.` on a vector column now protects individual null **elements** rather than
    refusing to write the whole row: a row with one null element still has its other elements
    written.

  `parquet_read_column`/`parquet_write_column` are unchanged — they have always carried a rank-2
  mask for a vector column. Building the per-row mask also got faster where nulls are dense
  (measured 2.2x on a width-16 column that is half null), and is unchanged on the null-free path.
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

- **Filling a string column from a `character` array is no longer quadratic.** `%add_column`,
  `%set`, `%set_slice` and `parquet_column%set_all` wrote one element at a time, and each write
  rewrote every later offset in the packed store — so building an *n*-row string column cost
  *n*²/2 offset writes and stopped completing at realistic sizes (a 15.6-million-row
  `character(16)` column did not finish in ten minutes). They now build the store in one linear
  pass.
- **A `character` ARRAY handed to a table or column is now trimmed of trailing blanks, as
  documented.** `%add_column`'s doc-comment had promised this since 1.0.0 and nothing did it, so a
  `character(len=32)` array of short names was stored — and written to file — padded to 32 bytes.
  Every element of such an array shares one declared length, so a shorter value is blank-padded by
  Fortran and the padding cannot have been meant; `%add_column`, `%set`, `%set_slice`,
  `%append_values` and `parquet_column%set_all` therefore trim. **`%set_element` and a row handle's
  `%set` do not**: they take a *scalar*, whose length is exactly what the caller wrote.
  `parquet_string_column`'s own API is unchanged and still stores bytes verbatim unless asked with
  `trim=`/`strip=`. One visible consequence: `%get` into a `character(len=:), allocatable` now
  comes back sized to the longest *real* value rather than to the width that was put in.
- **`pf_permute(..., assume_valid=.true.)` no longer reads past the end of its array** when handed
  a permutation shorter than the values. `assume_valid` skipped the length check along with the
  contents check; it now skips the contents check only, and a wrong length aborts cleanly.
- Fixed a use-after-free in the compact string-buffer read path for struct-nested string columns.
- Fixed silent truncation of MAML lines beyond 1024 characters, and CRLF (Windows line-ending)
  handling in MAML files.
- Fixed a spurious "line exceeds 1024 characters" abort when loading a MAML file, observed under
  heavy multi-threaded contention (e.g. ifx with 100+ concurrent OpenMP threads reading the same
  MAML fixture at once): the non-advancing read used to detect over-length lines is now serialized
  with an OpenMP critical section.
- Fixed a documentation error pointing readers at `is_init()` instead of `is_parsed()`, plus
  several stale or broken documentation cross-links and anchors.
- Fixed two rare heap-corruption races (confirmed via ThreadSanitizer) when two OpenMP threads each
  opened a `parquet_reader`/`parquet_writer` and touched a column of the same type for the very
  first time in the process at close to the same moment: Arrow's own per-type singleton objects
  (`arrow::int32()`, `arrow::utf8()`, ...) are not safely concurrent on this project's Arrow build,
  neither their own one-time construction nor a lazily-cached internal "fingerprint" Arrow computes
  the first time it compares or serializes a type — racing either could corrupt memory that only
  surfaced later, in unrelated code. `parquet_open_reader`/`parquet_open_writer` now force both
  into existence once, from a single thread, before any concurrent caller can reach Arrow. See
  [Thread safety](doc/pages/thread-safety.md#a-note-on-arrows-own-type-singleton-construction).
- Fixed `parquet_close_writer` referencing an unallocated array on every writer that never set a
  row mask with `parquet_write_row_mask`: the mask-consumed check was one combined condition, and
  Fortran does not guarantee short-circuit evaluation, so the unused mask's size was queried
  regardless. Harmless in an ordinary build, but it aborted at close under `-fcheck=all`.

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
