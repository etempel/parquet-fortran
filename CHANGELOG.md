# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`call parquet_get_column_nullable(reader, name, is_nullable)`** reports whether a column's
  stored Arrow field is declared nullable — a schema-only query that reads no column data, and a
  different question from `parquet_column_has_nulls`, which answers whether the column actually
  *contains* one. A vector column reports its element (child) field's flag, the outer list field
  being non-nullable by construction; a dotted struct path reports the leaf's own flag. A column
  this library cannot read still has a meaningful flag, so an unreadable type is an answer rather
  than an error — it aborts only on a name that does not exist.

- **`call schema%set_protected(name [, protected])`** marks a column Null-protected from code, the
  equivalent of listing it under a MAML's `extra: protected_cols:`; `protected` defaults to
  `.true.` and `.false.` lifts protection. Unprotecting a column that is currently protected is
  allowed but prints a `WARNING` naming it, never aborting — where the protection came from makes
  no difference, since a MAML's `extra: protected_cols:` and an earlier `%set_protected` call in
  code are indistinguishable once set. A protected column is now also written
  **non-nullable**, which is the only way to declare a *streamed* `date`/`time`/`timestamp` or
  `parquet_string_column` column null-free — those carry their null state inside the element, so a
  null-free first row group cannot speak for the rest of the file. Passing an all-`.true.`
  `is_valid` mask for a protected column stays legal and is simply discarded once checked:
  declaring a column protected does not make the `is_valid` keyword unusable.

- **A `string_view` column can now be read straight into a `parquet_string_column`**, where the
  compact buffer path previously refused it and the guide told you to read through a fixed-width
  `character` array instead. Such a column only ever comes from a file written by another
  Arrow-based tool. It is converted to `large_utf8` first — the same conversion the row filter has
  always applied for its own Arrow-kernel gap — and the converted column replaces the cached one,
  so a second read costs nothing. The conversion is cheaper than the workaround it removes.

- **`parquet_table` gains a memory-safety guarantee for derived columns, plus several ergonomic
  forms of calls it already had.**

  **`call t%reserve_columns(n)`** makes room for `n` columns and, in doing so, publishes a
  contract `%add_column` did not previously have: *while spare column capacity remains, adding a
  column under a new name relocates no existing column's storage, moves no existing column's slot
  position, and does not advance `%generation()`* — so a `%col` pointer, a `parquet_table_col`
  handle and a `parquet_table_row` handle taken beforehand all stay valid. That makes the
  commonest derived-column idiom (take pointers, compute, `%add_column`) safe rather than
  undefined-but-usually-working: Fortran leaves a pointer's association status undefined across
  the `MOVE_ALLOC` a growing slot array performs. Replacing a column (`force=.true.`) is not
  covered, since it frees that column's storage, and neither is any row-changing mutation.
  `%column_capacity([free])` reports the slots allocated, or the spare ones; a reservation
  survives `%clone` and `%clone_structure`. Consequently **`%generation()` now advances only when
  something actually moved** — an `%add_column` within reserved capacity leaves it alone.

  **`call t%materialize(names)`** is a second name for `%prefetch(names)` — the same procedures,
  so they cannot behave differently. It exists because `%materialize_all()` takes no column list:
  facing a mutation that detaches the table, the instinct is to reach for the definitive-sounding
  name, and if four columns were wanted that reads the whole file, silently.

  **Every key- and name-list binding now accepts one string instead of an array.**
  `t%sort_by("group,-mass")`, `t%materialize("ra,dec,mag")`, and the same for `%top_n`,
  `%argsort_by`, `%argsort_partial`, `%is_sorted_by` and `%prefetch`. Commas and/or semicolons
  separate the entries, blanks are trimmed and empty tokens ignored — the spelling
  `parquet_prefetch_columns` has accepted since 1.0.0, now sharing one tokenizer with it. A *key*
  may also carry its own direction in the grammar `parquet_sortkey%add` already publishes
  (`asc`/`desc`, or a leading `-`), parsed by that same parser; giving both a direction token and
  a `descending=` argument is an error for the whole call. This is not only less typing: the array
  form needs every entry padded to one declared length, and guessing it too short **silently
  truncates** a name rather than failing.

  **`call t%require_columns(names)`** aborts unless the table has every named column, naming
  **every** missing one and echoing what was asked for — where a hand-written `%has_column` loop
  reports one per run. `call t%missing_columns(names, absent)` is the non-aborting form, giving a
  zero-size array when nothing is missing. Both match exactly (never by struct-path prefix) and
  read no column data.

- **`parquet_table` columns can now be reached by 1-based position, and by a resolved column
  handle, not only by name.**
  `t%column_index(name [, found])` gives a column's position (0 when absent) and
  `call t%column_name(j, nm [, found])` gives the name at a position; the two are inverses.
  `%kind`, `%width`, `%unit`, `%residency`, `%is_supported`, `%has_nulls` and `%is_null` each
  accept a position wherever they accept a name, so a `do j = 1, t%ncols()` sweep can report on
  every column without copying names out to ask about them. An out-of-range position reports
  through `found=` or aborts with a message naming both the position and the table's current
  width. These are metadata queries and read no values; positions address the table's own columns
  and are renumbered by `%drop_column`/`%add_column`, so re-derive them after a column-set change.

  **`call t%column(name, c [, found])` or `call t%column(j, c [, found])` gives
  `parquet_table_col`, a handle on one column** — the mirror image of `t%row(i)`, resolving the
  column once so a per-cell loop stops looking its name up on every access (that lookup is 52–77%
  of what a `%get_element` costs, and a resolve-once loop measured 1.5x–4x the name form's
  throughput across four toolchains). It carries `%get(i, value)`/`%set(i, value)` for all 18
  column kinds, `%is_null`/`%set_null`/`%clear_null` (each taking a row, or a row plus an element
  within it), `%ref(p [, is_valid])` for the same zero-copy pointer `%col` gives, and
  `%name`/`%kind`/`%width`/`%unit`/`%index`/`%residency`/`%is_valid`. **`%get(i, e, value)` and
  `%set(i, e, value)` are new capability rather than a faster spelling**: they reach one element
  of a vector row without materialising the row, which nothing before could do.
  `parquet_table_row` gains the matching `r%get(c, value)`/`r%set(c, value)`, so a row-major loop
  can drop its name lookups the same way.

  **Both handles now refuse to be used after a structural change** rather than silently reading
  the wrong row or column: each stamps the table's `%generation()` when it is made and aborts with
  a message naming the remedy once the two differ, and `%is_valid()` asks without aborting. This
  is a behaviour change for `parquet_table_row`, which previously kept a by-value row scope that
  went stale undetectably — a handle held across a `%sort_by`, `%filter_rows`, `%append`,
  `%compact` or `%drop_column` used to read whatever now sat at its index. Relatedly,
  `t%append(r)` now validates the row handle it is given, with its own distinct message when the
  handle names the table being appended to (which the append itself invalidates, so re-fetching
  cannot help). `parquet_table_row` also **loses its finalizer**, which nullified a pointer on a
  handle that owns nothing: neither handle type is finalizable now, so both may be declared in an
  OpenMP `private()` clause as well as in a `block`.

  **Per-cell access is substantially faster under the Intel compiler.** Every per-cell path in the
  table layer — both handles, `%get_element`/`%set_element`, and the per-element null queries —
  now reaches a column's storage through a non-polymorphic internal accessor rather than a
  type-bound call. `ifx` builds a runtime type descriptor for `parquet_column` whenever a
  `type(parquet_column)` is passed to a polymorphic dummy in another compilation unit, and emits
  it unconditionally in the calling procedure's prologue ahead of any branch: 178 stores on every
  access, ~35 ns, which was 78% of what a resolved-handle `%get(i, value)` cost. Answers, guards
  and error messages are unchanged, and `gfortran`, which never emitted the block, is unaffected.

- **Filling a string `parquet_column` from a character array is 6.2x faster**, and
  `parquet_string_column%build_from` gains a **character-array form** to make it so:
  `call col%build_from(values [, is_null])` clears the column and rebuilds it from a
  `character(len=*)` array, trimming each element's trailing blanks (the same rule `%set_all`
  already documented) and marking the optional mask's elements null. `%set_all` and
  `%append_values` on a `PK_STRING`/`PK_STRING_VEC` column rebuilt the store with one
  `%append_string` call per element, each of which re-derived the trim, re-checked two capacities
  that had just been reserved, and copied the payload through a `transfer` that allocates a
  temporary per element. The bulk forms do the same two passes — exact byte count, then fill — but
  copy each element's bytes as a plain array section, which needs no temporary. Measured on 1 M rows
  of `character(len=24)`: `%set_all` **65.2 ms → 10.6 ms**, of which the remaining 7.5 ms is
  `len_trim` itself. `%build_from` is now a generic over both forms, so the existing
  handle-gathering call is unchanged; **`%append_values(values [, is_null])`** is the new appending
  counterpart, and is what `parquet_column%append_values` on a string column now uses. No stored
  bytes change, and the trimming rule is unchanged in both directions.

- **Building a table row by row is no longer quadratic**, and `parquet_column` gains the capacity
  controls that make it so: **`%capacity()`**, **`%reserve(n)`** and **`%shrink_to_fit()`**, with
  **`parquet_table%reserve(n)`** and **`parquet_table%compact()`** as the whole-table forms. A
  column's storage grew exact-fit, reallocating and copying every row on every append, so appending
  `N` rows cost `O(N^2)` — and `%append(row)` additionally deep-copied every column of the row's
  whole *source* table and then discarded all but one row of each, costing another `O(source rows)`
  per call. Storage now grows geometrically (1.5x, matching `parquet_string_column`), and a row
  append copies just that row, so appending is amortised O(1) in both. Reading a file still
  allocates exactly the rows it holds, and every rebuild (`%filter_rows`, `%sort_by`, `%top_n`,
  `%delete_rows`, `%truncate`) still hands its memory back — so `%compact()` is a no-op on anything
  but a table that has been appended to, and is safe to call unconditionally. The price of the
  slack is that an appended-to table can hold up to 1.5x its rows' worth of storage until
  `%compact()` releases it; `%reserve(n)` up front turns even the amortised growth into a single
  allocation. `%compact` and `%reserve` are the only two operations that reallocate storage without
  changing the row set: they invalidate outstanding `%col` pointers and row handles (and advance
  `%generation()` only when they actually released or reserved something) but never detach the
  table. No answer changes.

- **Sorting by a string column is 1.3-1.6x faster**, and `parquet_string_column` gains two
  primitives that made it possible: **`%copy_buffers(offsets, data)`**, a safe copy-out counterpart
  to `%raw_buffers` for a consumer that wants the packed layout, and **`%compare(i, j)`**, which
  orders two elements without materializing either (Fortran's own `<` semantics, blanks and all).
  The sort-key extraction was walking the column with `%get` per element — one heap allocation per
  row, paid twice on the `parquet_column` path — where the column already holds exactly the layout
  the sort engine wants. Measured on 4 M elements: a `parquet_string_column` sort 0.566 s to 0.433 s,
  a `parquet_column` sort 0.676 s to 0.427 s. The two entry points now share one body, so they can no
  longer disagree. `parquet_table%print_stat` on a string column benefits from `%compare` the same
  way. No answer changes.
- **`parquet_set_string_threads(n)` / `parquet_get_string_threads()`**, and
  `parquet_string_threads()` reporting what one `parquet_string_column` bulk operation would
  resolve to here. This is the **within-one-column** thread axis: `parquet_set_table_threads` splits
  a table's work by column, this splits one column's work by row range, and the two never multiply
  because a string operation reached from inside the table's own parallel region stands down. Like
  the other caps it is read per operation, `0` means automatic, `1` forces serial, and it never
  overrides the rule that work inside an OpenMP parallel region runs serially.
  `parquet_set_threads(n)` now sets five subsystems rather than four, and
  `PARQUET_FORTRAN_STRING_THREADS` reaches it from the environment. **What currently uses it:**
  `%reindex`/`%reindex_trusted` — and so `parquet_column`'s string reindex and
  `parquet_table%sort_by` on a string column — plus `%to_character`, `%build_from`, `%gather`,
  `%delete_by_mask`, `%trim_all`/`%strip_all` and `%statistics`, in each case only when not already
  inside a parallel region. On an 8-core M1 Pro, at 4 M elements: `%to_character` **3.9x**,
  `%delete_by_mask` **3.5x**, `%trim_all` **1.9x**, `%build_from` **1.4x**, `%gather` **1.3x**.
  **The gain scales with both the machine and the column**, measured for `%reindex` on three:
  **1.15x** on that M1 Pro, **1.6-2.7x** on an 8-core i7, and **3.4-4.5x** on a 192-core
  dual-socket EPYC — in each case the larger figure is the larger column, and a 40 M-element column
  gains roughly 1.7x more than a 4 M one on the same hardware. The result is byte-identical at every thread count, and the serial path is untouched, so
  nothing is slower than before whatever the setting. Left alone, the automatic count is capped
  rather than taken as `omp_get_max_threads()`: past a certain point this work stops scaling and
  starts losing ground, and on a 384-logical-thread machine the uncapped answer was 18-40 % *worse*
  than the best available. Setting it explicitly overrides that ceiling. **Honouring the cap costs
  `parquet_strings` none of its independence**: the two settings it reads (this one and `verbosity`)
  live in a leaf `parquet_settings_base` module that imports nothing of the library's, so a program
  whose only import is `use parquet_strings` still links without the Arrow/Parquet C++ stack —
  taking them from `parquet_settings` itself would not, since that module mirrors knobs to C++.
- **`parquet_reader_adopt_transform(reader, source)`** gives one reader the read-time transform
  another has already worked out — its `filter=`/`sample_fraction=` row mask and its `sort_by=`
  permutation — instead of making it derive the same thing from the same file again. This is for the
  documented "one reader per thread" pattern: without it, every thread reading a filtered or sorted
  file repeats the whole filter evaluation and the whole sort. The cost is two atomic refcount
  increments, because a mask and a permutation are immutable Arrow arrays the readers can share, and
  several threads may adopt from one idle source at once. It aborts rather than producing a reader
  whose mask describes different rows: the two readers must be open on files with the same row and
  row-group counts, the adopting reader must have no transform of its own, and no column may have
  been read on it yet.
- **Sorting for plain Fortran arrays and column types**, in a new `parquet_sorting` module
  re-exported by `use parquet`. `pf_argsort` returns the permutation that would sort an array,
  `pf_sort` an independent sorted copy, `pf_permute` applies a permutation in place and
  `pf_is_sorted` tests an existing order — over eleven element types: the four numeric kinds,
  `logical`, `character(len=*)`, `parquet_date`/`parquet_time`/`parquet_timestamp`,
  `parquet_string_column` and `parquet_column`. Multi-key sorting goes through `pf_sort_keys`
  (`k%add(...)` once per key, keys of any mix of types, each with its own direction), since a
  Fortran generic cannot offer "an optional second array of any type"; the same object also works
  with `pf_is_sorted` and `pf_partial_argsort`, and `%nkeys_added()` counts the keys you added, one
  per `%add`, whatever their types. These agree **exactly** with a read-time
  `parquet_open_reader(..., sort_by=)` and with `parquet_table%sort_by` — the three can never
  disagree about null placement, NaN placement or tie order; every sort is stable,
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
  resolved count is also clamped to the CPU affinity the process actually has, since threads cannot
  escape their mask, and a one-per-process warning says so when the clamp bites -- an
  `OMP_PROC_BIND` setting combined with `OMP_PLACES=cores` otherwise disables threading silently
  (see [Performance](doc/pages/operating/performance.md#thread-placement-omp_places-and-omp_proc_bind)). The
  answer is bit-identical at every thread count, because the comparator is a total order under
  which no two rows compare equal, so `threads=` is purely a performance control. Threading is
  worth a substantial speedup over a serial sort, and the margin grows with the array size; the
  remaining gap to the thread count is memory latency, since a sort chases a scattered key
  whatever else it does. Note that a threaded
  sort's peak memory is about twice a serial one's. See
  [Sorting arrays and columns](doc/pages/utilities/sorting.md).
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
  [Generated table types](doc/pages/utilities/generated-tables.md).
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
  [Row filtering](doc/pages/io/filter-sort-sample.md#row-filtering-with-parquet_filter) and
  [Renaming the columns a filter or sort refers to](doc/pages/io/filter-sort-sample.md#renaming-the-columns-a-filter-or-sort-refers-to).
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
  row belongs to no single row group. **A filter must be applied before a sort, never after**: a
  sort permutation is sized to the post-filter row count, so `parquet_reader_set_filter` on a
  reader that already has a sort is refused rather than composed the wrong way round. Both
  `parquet_reader_set_filter` and `parquet_reader_set_sort` also refuse a reader that has already
  been read through `parquet_read_column_chunk`, alongside the whole-column case they already
  refused — a chunked read caches nothing, so it needs its own check, and its rows were handed
  back in physical row-group order. See
  [Reading rows in sorted order](doc/pages/io/filter-sort-sample.md#reading-rows-in-sorted-order-with-parquet_sortkey).
- Streaming/chunked reads now work on a filtered or sampled reader, which previously refused them
  outright: `parquet_read_column_chunk` hands back that row group's surviving rows, and
  `parquet_get_chunk_size` reports that same count, so a chunked loop's sizes still sum to
  `parquet_get_nrows` (a row group with no survivors reads as an empty chunk, and still counts as
  read for `check_complete`). Read-time qc validates the filtered chunk rather than the raw one.
  `parquet_reader_set_filter(reader, filter, row_group_lo, row_group_hi)` additionally scopes the
  filter to a row-group range and evaluates it one row group at a time, so peak memory is one row
  group's worth of the filter columns instead of the whole file — enough to filter a file larger
  than memory. See [Streaming/chunked reads](doc/pages/io/reading.md#streamingchunked-reads).
- Added `parquet_tables` (`parquet_table`): presents a whole parquet file as one in-memory table
  (`parquet_open_table`), hands columns back as ordinary Fortran arrays through a widening copy
  (`%get`) or a zero-copy typed pointer (`%col`), builds a table from scratch in memory
  (`parquet_new_table` + `%add_column`, which takes a Fortran array, a `parquet_string_column`, or
  a whole `parquet_column` — the last reads kind, width and row count off the column, so it is the
  one form covering every kind and width in a single call, and the only way to hand over a column
  that could not have been a plain array: one grown a row at a time with `%append_values` when the
  final length is not known up front, one carrying per-element nulls on a vector kind, or one
  derived with `%gather`/`%delete_by_mask`/`%reindex`; the column is copied and the caller keeps
  its own, `unit=` overrides the column's own unit, and a column that was never given a kind is
  refused rather than added as a slot nothing can read), and writes one back out through an
  ordinary `parquet_schema` (`parquet_write_table`). Columns are read on first use rather than at open
  time, so opening reads only the file's schema and a program pays only for the columns it
  touches — `%prefetch`, `%materialize_all`, `%reload` and `%evict_column` control this
  explicitly and `%residency` reports it. `%reload` and `%evict_column` **refuse a column the
  program has written into** unless `force=.true.` is passed: the values in such a column are not
  in the file, so discarding them would put the file's own values back on the next read with
  nothing to notice. `%is_user_populated(name)` reports whether a column is claimed that way, and
  `%set_user_populated(name, flag)` sets or releases the claim by hand — needed because a write
  through a `%col`/`%ref` pointer is indistinguishable from a read, so the library cannot mark it
  itself. The column handle carries the same pair. `%print_stat` marks a claimed column with a
  trailing `*`. Also included: a row-range (slice) form of `parquet_open_table`, with
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
  `%sort_by` orders rows exactly as the read-time `sort_by=` does, so sorting a table in
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
  column is looked up before anything is written. On a reported miss a reading call leaves an
  empty result rather than an undefined one, so a program that ignores `found` gets nothing rather
  than something it must not touch: `%get`, `%get_slice` and `%get_element` all leave a
  zero-length array (a defined zero, blank or null element for a scalar receiver) and `%col` a
  null pointer.
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
  [the guide](doc/pages/tables/table.md) for the current limitations, the detach rule and the OpenMP
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
  [Deferring qc declarations](doc/pages/schema/quality-control.md#deferring-qc-declarations-with-parquet_read_qc).
- Added `parquet_columns` (`parquet_column`): type-erased, whole-column value storage with sparse
  null tracking, covering 18 scalar/vector kinds plus reserved slots for the future
  list/map/struct column types, with the full value and structural instruction set — including
  `%adopt`, which makes a caller's array the column's storage outright via `move_alloc` instead
  of copying into freshly allocated storage, `%paste`, which overwrites an existing row range
  from another column of the same kind and width without reallocating anything, `%gather`, which
  keeps the rows an index list names in the order it names them (the subset-and-reorder neither
  `%reindex` nor `%delete_by_mask` covers — the list may be any length, in any order, and may repeat
  a row), `%get_elem`/`%set_elem`, which read and write ONE element of a vector row without
  building the width-long array `%get_at`/`%set_at` need (and which maintain the column's own null
  bookkeeping, unlike writing through a `%data_ptr` pointer), and `%move_from`,
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
  3.9x on a 24-column, 900k-row file with 8 threads. Reading a **single** large column is parallel
  too, split across its row groups rather than its columns, so a `%get` or `%prefetch` of one name
  is no longer serial however large the column is — measured at **3.5–3.8x** on a 30 M-row `float64`
  column over 16 row groups. String columns keep the serial read (their packed store has no fixed
  row slots to write into), as do columns too small for the split to pay for itself. **A read-time transform keeps all of
  this**: the extra readers adopt the table's own filter mask and sort permutation rather than
  rebuilding them, so a `filter=` read is 2.2x, a `sort=` read 2.2x and a `qc=` or
  `sample_fraction=` read 3.3–3.5x, where a filtered or sorted read used to be serial. A soft `qc=`
  violation still warns at most once per column.
  New `%ensure_validity([name])` materializes a column's validity storage up
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
  [Thread safety](doc/pages/operating/thread-safety.md) for the full per-operation table.

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
  `WARNING`. **Four performance knobs** complete the set: `parquet_set_sort_counting_path` and
  `parquet_set_sort_counting_bucket_limit` govern the integer counting fast path (the limit bounds a
  key's value *range*, not its cardinality, and is the memory control — `n` buckets cost `8n` bytes),
  `parquet_set_target_row_group_bytes` sizes the row groups of a writer opened without an explicit
  `chunk_size=`, and `parquet_set_statistics_prescreen` controls whether a filtered read skips row
  groups its footer statistics rule out. Both numeric knobs take either integer kind and accept
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
  [Settings](doc/pages/operating/settings.md).

- **Typed table metadata now records its own type in the written file.** A parquet key-value pair
  can only hold text, so `schema%add_metadata("NSIDE", 1024_int32)` used to arrive at a reader as
  the indistinguishable string `1024`; it is now accompanied by an `NSIDE.datatype` entry reading
  `int32`, mirroring the `column.<name>.<attribute>` convention. The tokens are the ones columns
  already use (`int32`, `int64`, `float32`, `float64`, `boolean`), with a `[]` suffix for the
  array overloads. The *string* overload records nothing, so a genuinely textual keyword stays a
  plain string — which is what lets a reader tell the two apart — and a MAML-declared key is a
  string by design and records nothing either. The companion lives only in the file: a typed
  `add_metadata` call still appends exactly one `%metadata%items` entry, so `write_maml=.true.`
  sidecars are unchanged. The VOTable sidecar declares the same types (`int`, `long`, `float`,
  `double`, `boolean`) for those keywords instead of calling every scalar a `char`. Nothing on the
  read side changes: a Fortran reader still selects its parse from the declared type of `value`.

- **A counter-based random number generator, `parquet_random`** (`pf_random_at`, `pf_random32_at`,
  `pf_random_bits_at`, `pf_random_int_at`, `pf_random_fill_at`, `pf_random_seed`, `pf_random_key`,
  `pf_random_algorithm`), re-exported by `use parquet` and usable on its own as
  `use parquet_random`. Every value is a pure function of `(seed, i [, draw])` rather than of call
  order, so a parallel loop returns the same numbers under `schedule(static)`,
  `schedule(dynamic)`, one thread or three hundred — something no stateful generator can offer at
  any speed, and which locking does not fix. The generator is Philox4x32-10; `pf_random_algorithm`
  names the frozen bit contract (`philox4x32-10/v1`) and changes only if some value the module can
  produce changes. `pf_random_at`/`pf_random32_at` give `real64`/`real32` in `[0, 1)` with 1.0
  unreachable by construction, and the `real32` sequence is deliberately its own rather than a
  narrowing of the `real64` one; `pf_random_bits_at` gives 64 raw bits, of which `pf_random_at` is
  contractually the top 53; `pf_random_int_at` is exactly unbiased at every width, including above
  2**63 where the obvious implementations silently make part of the range unreachable, and swaps
  `lo > hi` rather than failing; `pf_random_fill_at` fills a rank-1 `real64`/`real32` array with
  consecutive draws, prefix-consistently and more cheaply than the equivalent scalar calls; and
  `pf_random_key` derives independent seed families that compose by nesting. The generator is
  **not cryptographic** and is documented as such. See
  [Random numbers](doc/pages/utilities/random.md).
  See [A typed value records its own
  type](doc/pages/schema/building-schema-in-code.md#a-typed-value-records-its-own-type).

### Changed

- **A schema built with `schema%init`/`schema%add_field` no longer needs a `parquet_parse_maml`
  call, and its calls no longer have a required order.** Both builders now parse the MAML text
  they write as they write it, so the schema's fields and metadata are always in step with it: a
  schema is ready to query, write with, or add metadata to as soon as its last field is declared.
  Three consequences a reader of 1.0.0 will notice. `schema%is_parsed()` answers `.true.` after the
  first `%add_field` where it used to answer `.false.` until an explicit parse (after `%init`
  alone it still answers `.false.` — a MAML with no fields is not a document). `%add_field` after a
  parse used to append text that nothing read, so the column silently did not exist; it now takes
  effect. And `schema%add_metadata` may be called at any point after `%init` and interleaved
  freely with `%add_field`, where it previously had to come after the parse and aborted otherwise
  — a parse now preserves the entries it used to discard, so a redundant `parquet_parse_maml` call
  is harmless in both directions. `parquet_parse_maml` itself is unchanged and still required for a
  schema loaded from a `.maml` file or one whose `%maml` was populated directly, such as an
  embedded schema; `parquet_open_writer` and `parquet_write_table` parse that kind for you.

- **`schema%add_field` now applies the per-field rules `parquet_validate_maml` applies**, using the
  same code, so an in-code schema is validated even though it is never validated as a whole
  document. A `qc_min`/`qc_max` that is not convertible to the declared numeric type, a `qc:` on a
  `date`/`time`/`timestamp` column, a non-positive `col_size`/`array_size`, and `array_size: auto`
  on a non-string column each now abort at the `%add_field` call that introduced them rather than
  at a later parse — or, since that parse is no longer required, rather than not at all.

- **`qc: miss:` now has three states, and an undeclared one no longer means "no Nulls allowed".**
  A field that declares no `qc: miss:` says nothing about Nulls, and none are checked — on the
  write side or the read side. `miss: Null`/`NA` says Nulls are expected, as before. The one form
  that asks for Null validation is an **explicit, empty** `miss:`. Previously a column with no
  `qc:` block at all warned on write when a Null was written through it, and a column declaring
  only `qc: min:`/`max:` aborted on read, in both cases because "undeclared" and "declared empty"
  were indistinguishable; a schema that never mentioned Nulls therefore enforced a rule its author
  had not written down. From code, `schema%add_field(..., qc_miss="")` is how you ask for the
  check — an empty string is now kept and emitted as a bare `miss:` line rather than discarded,
  which is what makes the state reachable from the in-code builder at all. `schema%get_field`'s
  `qc_miss` output reports `"Null"` for both allowing forms and `""` only for the checking one, so
  feeding it back into `%add_field` (as `%add_field_from` does) reproduces the source's behaviour.

- **A streamed column's nullability now comes from its first row group's `is_valid` mask, and every
  row group must agree.** `parquet_write_column_chunk` fixes a column's Arrow field when the first
  row group locks the file's schema, so the only question it can answer is whether the caller
  passed a mask — and the column is now nullable exactly when the first row group did, whatever
  that mask's entries were. Previously every streamed column was written nullable, so a file
  carried validity information for columns whose writer never asked for any. **Changing the form
  afterwards is a hard error**: a mask dropped after the first row group would silently discard a
  stated intent to allow Nulls, and one added after an unmasked first row group would put a Null
  into a field whose schema forbids it. In practice a chunked write is one loop body, so the form
  is uniform anyway; if you cannot know in advance whether a Null will appear, pass an all-`.true.`
  mask in the first row group. `date`/`time`/`timestamp` columns and a `parquet_string_column` are
  exempt and stay nullable when streamed, since their nulls live in the element rather than in a
  mask — unless the column is protected.

- **A vector column's element field is now written non-nullable when nothing can put a Null in it.**
  Arrow's convenience constructor for `fixed_size_list` hard-codes a nullable child, so until now
  *every* vector column was written with a nullable element field however it was produced. The
  element field now follows the same rule as a scalar column's: for a whole-column write, whether
  the values contain a Null; for a streamed one, whether the first row group passed a mask. Nothing
  about the values changes and no round trip is affected — a Null still round-trips through a
  column written with a mask that has one — but a file's schema is more accurate about what its
  vector columns can hold.

- **Closing a schema-enforced writer that had nothing written to it now produces a valid file
  instead of aborting.** Every declared column is written with zero rows, so the output carries the
  full schema — the columns a reader expects, their `unit`/`info`/`ucd` and the table metadata —
  and a `WARNING` names the file and says it happened. Previously this aborted with `missing write
  for enabled column`, which made an analysis stage that legitimately produced no rows a failure
  rather than an empty result, and left callers writing a zero-length array to every column by hand
  to work around it. That workaround still behaves identically and warns about nothing, since the
  library only steps in when the caller wrote nothing at all. An unresolved `col_size: auto` or
  `array_size: auto` resolves to `1`, there being no data to measure. **Partial writes are
  unchanged**: writing some declared columns and not others still aborts, naming the column that
  was missed — as does closing with nothing written after `parquet_write_row_mask`, since a mask
  says rows were expected, or after `parquet_new_row_group`.

- **A zero-length `parquet_write_column` to a `string` column now registers the column** instead of
  silently writing nothing and then failing at close with a C++-level `Missing column data before
  close`. Both the scalar and the vector (rank-2) string forms returned early on an empty array
  *after* recording the column as written on the Fortran side, so the two bookkeeping halves
  disagreed and the diagnostic named the wrong problem. Every other type already appended an empty
  array. This is what made the zero-length-array idiom above work on a numeric schema and fail on
  any schema containing a string column.

- **`parquet_write_column_chunk` now converts a chunk's values to the schema's declared numeric
  type, exactly as `parquet_write_column` always has.** Writing `int32` values into a column a
  schema declares `float64` — or any other pair `int32`/`int64`/`float32`/`float64` — previously
  aborted with "requires an exact type match" on the streaming path while succeeding on the
  whole-column path, so replacing `parquet_write_column` with the row-group calls for a column too
  large to hold at once could turn a working program into an abort for a reason that had nothing to
  do with size. The two paths now accept the same values for the same schema, and the file holds
  the declared type either way. A float-to-integer conversion still aborts on a non-integral or
  out-of-range value, per chunk, as it does for a whole column, and genuinely incompatible pairs
  (a `logical` chunk into an `int32` column, say) are still refused — with the same message the
  whole-column path gives, naming `parquet_write_column_chunk`. Reading never had this asymmetry:
  `parquet_read_column` and `parquet_read_column_chunk` have always shared one conversion.

- **`parquet_get_column_type` and `parquet_column_exists(types=)` now answer "can I read this
  column, and as what?" instead of "is its physical type one of nine names?"** Both are driven
  from one published **narrowest-lossless** mapping from a column's physical type to the Fortran
  kind this library reads it into: `int8`/`int16`/`int32`/`uint8`/`uint16` → `int32`;
  `int64`/`uint32` → `int64`; `half_float`/`float` → `float32`; `double` → `float64`;
  `bool`/`string`/`date`/`time`/`timestamp` as before. `uint64` → `int64` and every `decimal` →
  `float64` are **deliberately lossy**: a caller asking what to declare is better served by the
  kind the library will actually use than by being told a readable column is unreadable (a
  `uint64` value above `huge(int64)` still aborts on read). The mapping reads the type ID alone,
  so `decimal(9,0)` answers `float64` like every other decimal. Three consequences:

  - **`parquet_get_column_type` no longer aborts on a column it cannot read** — it returns
    `"unknown"`. It still aborts on a name that does not exist, which is a caller mistake.
    Previously it aborted for both, so no working call changes behavior.
  - **`types=` matches against that target kind**, so `types="int64"` now finds a `uint32` column.
    The group alias `int` covers every integer physical type and `float` covers everything
    readable into a float — which, since every numeric type converts to `float64`, means every
    numeric column, decimals included. An alias is therefore **not** the union of its member
    tokens: `types="float"` matches an `int32` column while `types="float64"` does not, because
    the two ask different questions ("can I read this as a float at all?" against "is `float64`
    the right declaration?"). Both are useful and the asymmetry is intended.
  - **`parquet_table` inherits this**, since it classifies each column with the same query: a
    file's unsigned, narrow-integer, half-float or decimal columns are now materialized into the
    mapped kind rather than being marked unsupported and skipped.

- **`parquet_reader_set_filter` accepts `row_group_lo = 0` to mean "every row group", giving the
  memory-bounded filter engine over a whole file.** The procedure has always had two engines, and
  which one runs is decided by whether the row-group arguments are supplied at all:
  `parquet_reader_set_filter(reader, filt)` — like an open-time `filter=` — reads every filter
  column in one batched pass and leaves it decoded, so reading that column afterwards is free,
  while any form naming row groups evaluates a row group at a time and keeps only the mask, which
  is what makes a filtered read possible on a file larger than memory. Until now the second engine
  could only be asked for by naming an explicit range, so filtering a whole large file that way
  meant calling `parquet_get_num_row_groups` first and passing `1, n`; `0` now says the same thing
  directly, and `row_group_hi` is ignored when it is given. A non-positive lower bound already
  meant "all row groups" in `parquet_measure_list_width` and `parquet_column_has_nulls`, so the
  three now agree. Calls that previously aborted (a zero lower bound was rejected as out of range)
  now succeed; no working call changes behavior.

- **The user guide is reorganised into a two-layer structure**, six groups of pages instead of one
  flat list, so every published page URL changed from `page/<name>.html` to
  `page/<group>/<name>.html` (e.g. `page/io/reading.html`). Two oversized pages were split in the
  same reorganisation: `reading.md`'s filter/sort/sample block is now its own page
  (`io/filter-sort-sample.md`), and the `parquet_table` guide is now four pages under `tables/`
  (basics, opening, writing, mutating). No content was removed; old bookmarks into the previous
  flat URLs will 404.

- **`pf_permute` no longer copies a permutation that is already the right kind.** Every specific
  widened `perm` into a fresh `integer(int64)` array before using it — including the eleven
  `integer(int64)`-index specifics, where that was an allocation and a full copy to produce a value
  identical to an argument already in hand. Those eleven now use `perm` directly; the
  `integer(int32)`-index specifics still widen, because there the conversion is real. Measured on a
  4 M-element `real64` permutation with an `int64` index: 11.9 → 11.1 ms (**1.07x**), and one
  fewer `n`-element allocation per call. No answer changes, and the `int32` path is untouched.

- **Reading a `parquet_date` column is 1.6x faster**, and every temporal read is cheaper, from a
  one-word change repeated across the setters. `parquet_date`/`parquet_time`/`parquet_timestamp`
  declared their setters' passed-object dummy `class(...), intent(out)`, which makes the compiler
  default-initialise the element through the runtime on entry — and since these setters are
  `elemental` and the read path calls one per row, that reset was paid per element and cost several
  times the work the setter itself does. **They now take `intent(inout)` and assign every component
  explicitly**, which is the same thing the reset was doing, done once. Measured over a 4 M-row
  whole-column read: the element-construction loop 12.3 → 3.4 ms for `date` (**3.6x**) and
  17.2 → 5.7 ms for `timestamp` (**2.9x**), taking the whole date-column read from 23.6 ms to
  14.8 ms and the timestamp column from 55.1 ms to 52 ms. Nothing about the API changes — the
  argument is still definable, still a setter, and every value is identical. The four procedures
  with a caught-failure path (`%parse` on all three types, and `parquet_timestamp%set(date, time)`)
  deliberately **keep** `intent(out)`, because that reset is exactly what makes a failed parse or a
  null input yield a null element rather than a stale one.

- **Sorting a `parquet_column` that contains nulls is up to 5.5x faster**, from two independent
  changes that compound. Extracting the sort key's validity walked the column **twice** with a
  per-row `%is_null` — up to 2n un-inlinable calls, each re-checking the index and re-dispatching
  the kind — because `%row_validity`, which does the same work a 64-bit word at a time, was
  unreachable: it was declared `intent(inout)` so a temporal column could refresh a null cache in
  passing, while `pf_argsort` holds its column `intent(in)`. **`%row_validity` and
  `%element_validity` are now `intent(in)`**, which is a source-compatible widening — every call
  that compiled before still does — and makes both usable from a read-only reference for the first
  time. Combined with the counting-path change below, a 4 M-row `int32` column sorts in 35.4 ms at
  0.1% nulls against 168.8 ms before (**4.8x**), 33.5 ms at 10% (**5.1x**) and 25.3 ms at 50%
  (**5.4x**) — at or below what the same column costs with no nulls at all.

- **Sorting an integer column that contains nulls is 3.1x faster.** The integer counting fast path
  declined any column carrying a validity mask, so **a single null anywhere took the whole sort onto
  the general comparator path** — a step function of whether a null existed, not of how many. Nulls
  are a *tier* in this engine rather than a value, so they never interleave with values; the fast
  path now places them as one contiguous block and counting-sorts the rest. Measured on a 4 M-row
  `int32` column: 168.8 → 54.3 ms at 0.1% nulls, 172.0 → 56.3 ms at 10%, 136.7 → 43.9 ms at 50%.
  A null-bearing integer column is the common case in real files, and this reaches `pf_sort`,
  `pf_argsort`, `parquet_table%sort_by` and the reader's `sort=` alike. Null placement, the
  direction rule (`descending` never moves the null block) and stability are unchanged — the fast
  path is asserted against the comparator path across both directions and both placements. A
  related latent bug is fixed with it: the fast path's value-range scan counted null rows, whose key
  bytes Arrow does not define, so a null carrying a large value could silently disable the fast path
  for a column well within its limits.

- **Building a validity mask for a date/time/timestamp column is 2.6x faster.** These kinds keep
  their null state in the element rather than in the column's bitmap, so `%row_validity`,
  `%element_validity` and `%set_validity` took a separate branch for them — one that asked the
  generic per-element `%is_null`/`%set_null` for every element, re-validating both indices,
  re-reading the width and re-dispatching the kind each time. The kind is now resolved once, above
  the loop; since the temporal `%is_null` are `elemental`, the scalar cases collapse to a single
  array expression and the per-element call disappears. Measured on 2 M rows: `element_validity`
  10.75 → 3.97 ms for date and 10.13 → 4.01 ms for timestamp. String columns take the same
  specialised path and keep their loop, minus the dispatch. This is on the path every table read of
  a null-bearing temporal column goes through. No answer changes.

- **Null tracking is written a 64-bit word at a time instead of a bit at a time**, which is what
  most of the bulk validity operations were doing — one un-inlinable call per element, each redoing
  a division and a modulo to find the bit it wanted, and in two cases a full type-bound dispatch per
  row on top. Appending null rows, appending a null-bearing column, pasting over a row range,
  reordering rows, and replaying a mask onto a column (which every table materializer and every
  `%set(..., is_valid=)` does) all now work on whole words, masking only the ragged ends of a run.
  `parquet_column%set_validity` additionally accepts a **rank-1 per-row mask** alongside the
  existing rank-2 `(width, nrows)` element one, marking every element of a `.false.` row null — the
  same row/element pairing `%set_null` and `%is_null` already use — so a row mask no longer has to
  be replayed one call at a time. Measured on an 8-core M1 Pro over a 4 M-row `int32` column, best
  of five warmed rounds at `--profile release`: `%append_nulls` 21.73 ms to 0.66 ms (**33x**),
  `%paste` over the whole column 12.11 ms to 0.65 ms (**19x**), `%append` of a null-bearing column
  7.71 ms to 1.31 ms (**5.9x**), a per-row mask replay 4.57 ms to 2.88 ms (**1.59x**), a per-element
  mask 8.63 ms to 5.68 ms (**1.52x**), and `%reindex` — which permutes values as well as validity —
  20.19 ms to 16.12 ms (**1.25x**). Validity answers are unchanged, including `%paste`'s rule that
  it replaces the pasted range's validity where `%append` merges into fresh rows.
- **Resolving a column by name on a wide table no longer scans every column.** Every value
  accessor — `%get`, `%col`, `%set`, `%is_null`, `%get_element`, a row handle's `%get` — reaches its
  column through one lookup, and that lookup compared the requested name against each column's in
  turn. The table now keeps its column names in a sorted index, maintained by the operations that
  change the column set, and bisects it; the search compares a packed integer prefix of each name
  out of one contiguous array, touching the names themselves only where two share their first seven
  bytes. Measured on an 8-core M1 Pro over a 40-column table, best of five warmed rounds at
  `--profile release`, ns per `%get_element` call: reading four columns scattered through the table
  103.8 to 25.4 (**4.1x**), the last-added column 181.4 to 23.4 (**7.8x**). The cost is a flat
  ~6-7 ns that bisection pays whatever the table's width, so a program that only ever reads the
  *first*-added column of a wide table is **1.4x slower** (16.7 to 24.0 ns) — the one access pattern
  a linear scan wins. A lookup remains a pure read, so concurrent readers of a shared table still
  need no synchronisation, and a stale index can only ever cost a scan, never return a wrong
  column. No answer changes.
- **Extracting a sort key from a `parquet_column` no longer switches on the column's type once per
  row.** `pf_sort`/`pf_argsort`/`%sort_by` over a `parquet_column` read the column one element at a
  time through a call that re-tested the kind, re-checked the index and could not be inlined, then
  overwrote a buffer it had just zeroed. The type is now decided once and each arm is a single
  whole-array read of the column's storage. A null-free column also stops scanning for nulls it
  cannot have: whether any exist is answered from the column's own state rather than by testing
  every row. Measured over 2 M rows, best of five warmed rounds at `--profile release`: an `int32`
  column 26.2 ms to 16.9 ms (**1.55x**). A `float64` column is unchanged end-to-end, the comparison
  sort dominating it by two orders of magnitude. Identical orderings, including nulls and NaNs.
- **Filtering and read-side quality control on a string column no longer allocate per row.** Both
  built a `std::string` out of the string view they already had, purely to make a comparison —
  a malloc, a copy and a free for every row of the column. The comparison is now made on the view
  itself; `std::string_view`'s `<` and `==` are byte-lexicographic, which is exactly what the
  previous comparison was, so no ordering changes. Measured on an 8-core M1 Pro over 4 M rows of
  `character(len=16)`, best of five warmed rounds at `--profile release`: the filter's row-matching
  phase 15.80 ns/row to 11.26 ns/row (**1.40x**), and a qc-enforced read of the whole column
  0.1765 s to 0.1553 s (**1.14x**). Installing a filter as a whole gains 1.08x, the rest being the
  column decode that this does not touch.
- **Reading a space-padded string column is 1.2x faster, and neither direction stages the bytes
  through a temporary buffer any more.** Both directions of the `character(len=*)` string path
  allocated an `item_len * nrows`-byte staging array and then copied it, one character at a time,
  through a nested loop with a loop-carried cursor (`values(i)(j:j) = achar(iachar(packed(k)))`).
  A Fortran `character(len=item_len)` array and that buffer have identical memory layout, so the
  buffer was never needed: the C++ reader now fills the caller's array directly and the writer
  hands the caller's array straight to the encoder. The read side also drops a redundant
  `values(i) = ''` that blanked each element immediately before every one of its bytes was
  overwritten. Measured on an 8-core M1 Pro, best of five warmed rounds at `--profile release`,
  1.5 M rows of `character(len=16)`: read 0.0388 s to 0.0317 s (**1.22x**), and the same for a
  width-4 vector column (**1.21x**); the write is **1.05x** end-to-end and **1.13x** counting only
  `parquet_write_column` itself, the rest being Arrow encoding and file I/O that this does not
  touch. Files are byte-identical and reads return identical values, including padding, nulls and
  `null_value` substitution. The `parquet_string_column` path was already buffer-free and is
  unaffected.
- **A qc-enabled string write no longer measures every element to find the longest.** The
  `array_size` check built a complete per-element length array purely to take its maximum and
  compare that against the declared limit, so a violating first element still cost a full pass and
  a full-size temporary. It now stops at the first element that is too long. Same abort, same
  message.
- **An ordinary write no longer pays for row masking it is not using.** `parquet_write_column` and
  `parquet_write_column_chunk` fabricated an all-`.true.` row mask whenever no
  `parquet_write_row_mask`/`parquet_write_chunk_row_mask` was in force — which is the common case —
  expanded it to one logical per *element*, and then `pack`ed the whole column through it to remove
  nothing. For a 100 M-row `int32` column that was ~800 MB of transient logical arrays plus a full
  copy of data that was already contiguous. The mask is now built only when one actually applies;
  otherwise the caller's own array is written as it stands. The matrix forms additionally no longer
  `reshape` into a flattened copy first, the `boolean` forms convert only the elements they keep
  instead of converting all of them and then packing, a `parquet_string_column` write no longer
  rebuilds the entire column row by row, and a per-row-group identity mask is no longer allocated
  for every row group of an unmasked chunked write. Measured on an 8-core M1 Pro, best of five
  warmed rounds at `--profile release`: `float64` and `boolean` scalar columns **1.6x**, `int32`
  scalar **1.2x**, a width-4 `int32` matrix **1.14x**, `character(len=16)` **1.16x**. Files are
  byte-identical to what the previous version wrote, masked and unmasked alike, across every type
  and both the whole-column and chunked forms.
- **A qc-enabled numeric write no longer builds a `float64` copy of the whole column.** The
  quality-control check took its values as `float64`, so every `int32`/`int64`/`float32` write with
  `qc=.true.` widened the entire column into a temporary purely to make the call, and passed a
  full-length all-`.true.` mask alongside it whenever the caller supplied no `is_valid`. The check
  now converts the *bound* once instead, and the mask argument is optional. See **Fixed** below for
  the one case where this also changes the answer.
- **`parquet_string_column%to_character` is 4.3x faster and `%build_from` 9.3x**, for the same
  reason in both: each element was being materialized through `%get`/`%to_string`, which allocates a
  deferred-length string, fills it, copies it into the destination and frees it — one heap round-trip
  per element, which measured as 71% of `%to_character` rather than the copying it was there to do.
  Both now copy the payload bytes straight out of the source column's own buffer, and `%build_from`
  additionally sizes its destination once from the handle-validation pass it was already making,
  instead of growing it one element at a time. Measured on 4 M elements / 70 MB: `%to_character`
  0.168 s to 0.038 s, `%build_from` 0.305 s to 0.033 s. Results are unchanged in every respect —
  same padding to the longest element, same `null_value` substitution, same trimming behaviour, same
  aborts on a null with no `null_value` and on an invalid handle.
- **`parquet_string_column` gains `%copy_to(i, dest)` and `%append_from(src, i)`**, the two
  allocation-free counterparts of `%get`/`%append_string`. `%copy_to` copies one element into a
  fixed-length slot the caller already has, following Fortran's own assignment semantics exactly
  (blank-padding a short value, truncating one too long), so it is a drop-in for
  `call c%get(i, s); dest = s`. `%append_from` appends one element of another column, **null state
  included**, so a "copy the rows I want" loop needs no `is_null` fork and materializes nothing.
  Both take either integer kind for the index. With `%compare` and `%copy_buffers` (above) these
  complete the set, and every bulk loop over a string column inside this library now uses one of
  them: the eighteen remaining per-element string allocations — in `parquet_table`'s `%get`/`%row`/
  `%get_slice` accessors, the masked write path, `pf_unique` and `parquet_column%get_at` — are gone,
  and a lint check keeps new ones out. **Nothing about any result changes**; this is the last of the
  per-element heap round trips the string work has been removing.
- **`parquet_string_column%view_all` and `%view_slice` are 3.9x faster** (0.0175 s to 0.0045 s on
  4 M elements), which also makes them the fastest way to walk a column. Both filled the caller's
  array with `data_string(i) = col%view(i)`, and `parquet_string` has a finalizer — so intrinsic
  assignment ran it on the destination before overwriting it and again on the function result, twice
  per element, to set a pointer and an index. They now write those two components directly. The
  handles are identical; nothing about their lifetime or write-through behaviour changes.
- **`parquet_string_column%delete_by_mask` is faster on a column with nulls even single-threaded**
  (0.0183 s to 0.0154 s on 4 M elements). Rebuilding the validity bitmap for the surviving rows was
  setting one bit at a time, each a read-modify-write on the bitmap; it now accumulates a whole byte
  and stores it once per eight rows. Which rows survive and how many are null are unchanged.
- **`parquet_string_column%slice` and `%append_column` no longer pay for a column's nulls.** Both
  copied the validity bitmap one row at a time, so a column with nulls cost roughly three times one
  without — where a bitmap packs eight rows to a byte and the run being copied is contiguous. They
  now move it in whole bytes wherever both sides start on a byte boundary, which covers slicing from
  row 1 and appending onto an empty or 8-aligned column, and fall back to the per-row walk
  otherwise. Measured on 4 M elements with nulls: `%slice` 0.0118 s to 0.0039 s, `%append_column`
  0.0113 s to 0.0031 s — in both cases now the same cost as the null-free column. This is serial:
  it needs no threads and helps every caller. The same code now also merges the validity of a row
  group read from a file, so reading a compact string column with nulls benefits too. Null placement
  and counts are unchanged.
- **Applying a `filter=` is 3–4x faster**, and the gain grows with the number of columns the filter
  names. The per-row clause evaluation was reading each value through a helper that took the Arrow
  array by `shared_ptr` — one atomic refcount increment and decrement per row, to read one number.
  Measured on a 16-column × 2 M-row file: evaluating one clause went from 13.6 ns to 1.8 ns per row,
  and installing an eight-clause filter from 0.25 s to 0.061 s. Nothing about which rows match
  changed. Every caller benefits, `parquet_open_reader(..., filter=)` as much as `parquet_table`; a
  filtered table read is now 2.3x faster single-threaded and 3.3x faster on several threads.

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

- **A `parquet_table`'s per-column work now runs on several threads.** `%sort_by`, `%filter_rows`,
  `%top_n`, `%delete_rows` and `%truncate` all replay their permutation
  or mask across every resident column, and `%clone` copies every resident column; each column is
  independent of every other, so the loop
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
  sized by the rows they keep, and **`%clone` adds no transient at all** — a clone allocates a
  second copy of the table by definition, so there is nothing beyond the copy you asked for.

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

- **A MAML key is now case-insensitive everywhere, including the block headers.** Section names and
  field sub-keys were already matched case-insensitively by `parquet_validate_maml`, but the code
  that *locates* a block compared against a lowercase literal — so a MAML spelling its section
  `Extra:` validated cleanly while the `col_map:`, `protected_cols:`, `remap:`, `filter:` and
  `sort:` nested inside it were never found. Nothing warned: a column declared Null-protected
  simply was not, and a `parquet_table` column renamed by `remap:` kept its physical name. The same
  applied to a capitalized `Fields:`/`KeyArray:`. Values remain case-sensitive — a column name in
  `col_map:`/`remap:`/`protected_cols:` must still match its column exactly.

- **A `parquet_string_column` write now reports a truthful `array_size`.** That path stores each
  element's own bytes and a reader takes each length from the data, so it neither needs nor enforces
  a declared `array_size` — but it was still leaving its trace in the metadata wrong, in two ways.
  A column declared `array_size: auto` kept the internal `-1` sentinel all the way to
  `parquet_close_writer`, which wrote it verbatim into a `write_maml=.true.` sidecar and into the
  file's own `column.<name>.array_size` entry; the resulting sidecar was not a valid MAML at all
  (reading it back failed with "invalid array_size"), while the `.parquet` file itself round-tripped
  fine, so the mistake surfaced far from the write. And a column whose declaration the data simply
  outgrew kept advertising the declaration.

  Both are now settled at close from the longest element actually written, across every row group.
  Writing elements longer than the declaration **remains accepted** — that is what this path is for
  — but it now emits one `WARNING` naming the column, the declaration and the actual length. A
  declaration the data does *not* exceed is left exactly as written.

- **A MAML list under an ordinary top-level key no longer prints a spurious duplicate-key
  warning.** A plain string list under, say, `survey:` is *defined* to produce one metadata entry
  per item, all sharing that key's name — but each item after the first tripped the duplicate-key
  `WARNING` meant for a caller's own accidental `%add_metadata` collision, telling users their
  well-formed MAML was suspect. Reading such entries back is unchanged: a lookup by key answers
  with the first, and `parquet_get_metadata_items` lists them all.

- **`parquet_reader_set_filter`'s six-argument form now rejects a row range that lies outside the
  row groups it was given**, instead of silently returning their intersection. Both ranges were
  validated individually — against the file's row-group count and its row count — but never
  against each other, so a call like `parquet_reader_set_filter(reader, filt, 2, 3, 5, 8)` on a
  file whose row groups 2..3 hold rows 3..6 was accepted and quietly narrowed to rows 5..6, or to
  nothing at all when the two ranges were disjoint. An empty result is indistinguishable from a
  selective filter that matched nothing, so the mistake reported as data rather than as an error.
  The call now fails with `error stop`, naming both ranges and the rows the row groups actually
  span.

- **`parquet_get_column_total_elements` no longer decodes a whole column to answer a size query on
  a plain `LIST`/`LARGE_LIST` column.** For a scalar or `fixed_size_list` column it already read
  nothing at all, and for a plain variable-length `list` — which this library never writes, but
  another producer may — it read every row group at once purely to measure the width, so peak
  memory was one full copy of the column. Its sibling `parquet_get_col_size` had already moved to
  a footer screen followed by a one-row-group-at-a-time proof; both now share that helper, so the
  two size queries cost the same and neither ever holds more than one row group. Answers are
  unchanged in every case, including the deliberate whole-column fallback when a filter or sort is
  active (a width measured per row group would otherwise describe rows the caller had removed).

- **A `qc: min:`/`max:` bound on an `int64` column is now judged in `int64`, not after widening
  every value to `float64`.** Each value was converted to `float64` before being compared, and past
  2^53 that conversion rounds — so a value could be reported as compliant when it violated its
  declared bound, silently and with nothing to indicate the check had been weakened. A bound that
  is a whole number within `int64`'s range is now converted once and compared natively; a
  fractional bound (only reachable when the schema's own type is a float one) still compares in
  `float64`, which is the correct reading of that declaration. **The bound's own precision is
  unchanged** — it is still parsed from the MAML text into `float64`, so a *bound* past 2^53 is
  still rounded. `int32`, `float32` and `float64` columns are unaffected: those all convert to
  `float64` exactly, and their warnings are byte-for-byte what they were.

- **Sharing one `parquet_writer` across threads now reliably aborts with the documented diagnostic
  instead of segfaulting.** The concurrency guard was claimed only once a call reached C++, which on
  the write path is the last statement of the entry point — so two threads first raced through the
  writer's own Fortran bookkeeping, above all the reallocation that tracks which columns have been
  written, and corrupted the heap before either reached the guard. The crash then usually landed far
  from the cause, and the message promised by [Thread safety](doc/pages/operating/thread-safety.md) survived to
  stderr only some of the time. The guard is now claimed at the first statement of every write,
  row-group and row-mask entry point, and released automatically on every exit path. Measured on a
  384-core machine, the shared-writer case went from crashing on every single run to matching the
  shared-reader case, which never had the problem. Correct single-threaded use is unaffected: the
  guard now identifies the *owning* thread, so a thread may re-enter a handle it already holds, and
  sequential hand-off of a writer between threads remains allowed.
- **A `parquet_table` opened with an unseeded `sample_fraction=` now keeps one sample for its
  whole lifetime.** The seed is drawn once, at `parquet_open_table`, instead of being left to each
  reader the table opens — so a `%clone` (which reopens the file for every column the source had
  not already read) samples the *same* rows as its source rather than drawing a fresh subset of its
  own. Two columns of one table could otherwise come from two different random samples; that
  usually aborted with a row-count mismatch, but two draws keeping the same *number* of rows would
  have passed silently with different rows. Unaffected: two separate `parquet_open_table` calls
  with no `sample_seed=` still draw independently, an explicit `sample_seed=` still reproduces
  exactly, and `parquet_open_reader` is unchanged — a bare reader has no second reader to agree
  with.
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
  [Thread safety](doc/pages/operating/thread-safety.md#a-note-on-arrows-own-type-singleton-construction).
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
