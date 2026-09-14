---
paths:
  - "src/parquet_columns*.f90"
  - "src/parquet_table*.f90"
  - "src/parquet_strings.f90"
  - "src/parquet_temporal.f90"
  - "src/parquet_list.f90"
  - "src/parquet_map.f90"
  - "src/parquet_struct.f90"
  - "tools/generate_parquet_columns.py"
  - "tools/generate_parquet_tables.py"
  - "tools/generate_user_table_code.py"
  - "table_types/**/*"
  - "test/test_table*.f90"
  - "test/test_columns.f90"
  - "test/test_parquet_string.f90"
  - "test/test_string_parallel.f90"
  - "test/test_temporal*.f90"
  - "test/test_container_nested.f90"
  - "test/test_list.f90"
  - "test/test_map.f90"
  - "test/test_struct.f90"
  - "test/test_openmp.f90"
---
# Columns, strings, temporal elements and tables

Behavioural rules of the in-memory tiers (`parquet_columns`, `parquet_strings`, `parquet_temporal`,
the containers, `parquet_tables`).

## `parquet_column`

- **A character ARRAY is trimmed on the way in; a character SCALAR is not.** Arrays trim:
  `%set_all`/`%append_values`, `parquet_table`'s `%add_column`/`%set`/`%set_slice`, `%set_element`'s
  vector form (`refill_string_store`). Scalars do not: `%set_at`'s scalar form, `%set_element` on
  `PK_STRING`, a row handle's `%set`. `parquet_string_column`'s own API never trims by default
  (explicit `trim=`/`strip=`). Do not harmonise.
- **Validity is per ELEMENT** (`width * nrows` bits): every query and mutation has a row form and an
  element form (`is_null(i)`/`is_null(i, e)`, `set_null`, `clear_null`), the element index bounded
  by `width`. A row QUERY answers "any element null" (`row_validity`); a whole-row MUTATION marks
  every element; `modify_nulls=.false.` protects individual null elements.
- **Shapes match on every paired API**: rank-1 `values` ↔ rank-1 `is_valid`, rank-2 `values` ↔
  rank-2 `is_valid` shaped `(width, nrows)`; no widening anywhere. The standalone mask APIs
  (`%get_valid_mask`, `%set_null(mask)`) accept either rank (rank-1 = row summary).
- Walk the bitmap by set bits (`trailz` + `ibclr`) with the zero-word skip, never all 64 positions.
- `set_validity` writes a whole mask in one pass; a table write hands the element mask straight to
  the writer (`test_element_null_round_trip`).
- **Storage grows geometrically (`ensure_capacity`, 1.5x) and rebuilds are exact-fit**;
  `size(storage)` is `cap`, so every read is bounded by `1:nrows`
  (`spare capacity is invisible to every reader`, `test/test_columns.f90`).
  `%shrink_to_fit`/`%compact` are no-ops on a never-appended column.
- **Assemble from pieces with `init` at full size + `%paste(src, at, [from], [count])`**
  (`materialize_slice`). `%paste` REPLACES the pasted range's validity (`%append` merges). The
  string kinds are excluded and abort; they grow geometrically and append instead.
- **Fill from a temporary array with `%adopt`, never `%init` + `%set_all`** (`move_alloc`; kind,
  width and rows from the array). The two string kinds have no `adopt`.
- `%init` allocates storage at zero rows (`allocate_empty_storage`), so a known-kind column's storage
  is always allocated while `cap`/`%capacity()` stay 0. Any new bulk path must respect that the
  allocators skip zero deliberately (`grow_storage` returns at `n == 0`; `ensure_data_cap` allocates
  nothing for zero bytes), so a zero-length case must not reference storage it assumes exists.

## Typed accessor tiers

- **`parquet_tables` never calls a type-bound procedure on a `parquet_column`.** Per-cell paths use
  the `parquet_column_*` generics `parquet_columns` exports (`_get_at`, `_set_at`, `_get_elem`,
  `_set_elem`, `_data_ptr`, `_string_column`, `_is_null`, `_set_null`, `_clear_null`);
  `src/parquet.f90` privatises them all. Enforced by `check_no_type_bound_column_access`.
- The implementation lives at the `type(parquet_column)` end and the binding is a one-line
  forwarder onto it: `class` → `type` is free, `type` → `class` makes ifx build the runtime
  descriptor in the caller's prologue on every call. Never flip the arrow.
- Adding a per-cell accessor adds BOTH halves: a typed specific under the right generic in
  `tools/generate_parquet_columns.py`, and a `private ::` line in `src/parquet.f90` for a new
  generic name. A typed body calls the TYPED guards (`parquet_column_check_kind`/`_check_index`/
  `_check_element`/`_check_width`) and no `class`-dummy helper (`ensure_bitmap` takes a `type`
  dummy for this reason).
- Scope is per-cell; `mat_*`/`matchunk_*`, `add_column_*`, `set_arr_*` and once-per-table
  procedures stay as they are. Keep the tier even if a future ifx stops emitting the block. Check:
  `objdump -dr --no-show-raw-insn <obj> | awk '/<parquet_tables_mp_col_fetch_f64_>:/,/^$/' | grep -c 'R_X86_64.*\.bss'`
  must read 0.
- **`parquet_columns` never calls a type-bound procedure on a column's `str` component**; it uses
  the `parquet_string_column_*` names `parquet_strings` exports (privatised again in
  `src/parquet.f90`). Enforced by `check_no_type_bound_string_column_access`, both halves: no
  `%str%<binding>(...)` in `parquet_columns*`, no binding call on a `type` dummy inside the tier
  (ifx emits that descriptor block into `.bss`, so every thread contends on it).
- A private helper of `parquet_strings` takes a `type` dummy. `deep_copy`, `move_from` and `clear`
  still carry the block (once per column, by choice). Check per procedure:
  `objdump -d <binary> | awk '/<parquet_columns_mp_parquet_column_is_null_row_>:/,/^$/' | grep -c 'var\$'`
  must read 0.

## `parquet_strings`

- `src/parquet_strings.f90` reaches `iso_fortran_env`/`iso_c_binding`, `parquet_settings_base`
  (output suppression, the string thread cap) and `omp_lib` under `#ifdef _OPENMP`, nothing else
  (`check_parquet_strings_stays_leaf`). Adding a dependency is a decision. It reaches no `bind(C)`
  surface, which is why its test hooks are public Fortran procedures (`testing.md`).
- The module is `parquet_strings` (plural) because the type is `parquet_string`; rename neither.
- `allow_null=.true.` on `get`/`to_string` returns `""`, never unallocated; test with `is_null()`.
- Its bulk rebuilds thread internally; three of the four keep a serial twin, and any new threaded
  rebuild is measured serially before replacing its original. Same-array compaction loops stay
  scalar (`compact_all_serial`, `delete_by_mask_serial`).
- `ensure_offsets_cap`/`ensure_data_cap`/`ensure_validity_cap` grow geometrically (1.5x).

## `parquet_temporal` and the containers

- `parquet_date`/`parquet_time`/`parquet_timestamp` carry their own null state: no
  `is_valid=`/`null_value=` anywhere on their read/write path; a default-initialised element is
  null; a null-containing column reads without the error-on-Null the numeric/string readers apply.
  Deliberate and documented; not to be brought in line.
- `parquet_list`/`parquet_map`/`parquet_struct` chose the opposite deliberately: an explicit
  `is_valid=` argument on append/get (two null levels). A future element-domain module chooses
  rather than inherits.
- The polymorphic-`intent(out)` setter rule (`fortran-gotchas.md`, general) is enforced for the
  temporal types by `check_temporal_setters_assign_all`, which derives the component and setter
  lists from the source. The whole-array elemental call is slower than
  the indexed loop; the win is in the intent, not the call shape.
- `civil_from_days`'s final `if (m <= 2) y = y + 1` is easy to drop when re-deriving; re-read both
  functions and sanity-check a hand-derived boundary value by round trip
  (`days_from_civil(civil_from_days(z)) == z`).

## `parquet_table` concurrency: guards key on OWNERSHIP

- `src/parquet_tables_parallel.f90` owns the table's lock, the append/read counters and the shared
  refusal, so `#ifdef _OPENMP`/`use omp_lib` appear in that one file plus
  `unsafe_first_touch`/`record_open_thread` in `parquet_tables_read.f90`. All implement one test:
  a table this thread opened inside the current parallel region is thread-private and exempt;
  anything else may be shared.
- **Never key a guard that REFUSES on `omp_in_parallel()` alone** — test-drive runs tests inside
  `!$omp parallel do`, so it fires suite-wide. A DEFAULT chosen from it (thread-count resolution,
  `api-conventions.md`) is not a guard.
- `table_append_table` and `table_append_row` each take the lock once and call
  `append_table_worker`; neither calls the other (the lock is not recursive). A new internal caller
  goes to the worker.
- A lock is a handle: `%clone` builds a fresh one (`clone_new_cache`); `table_finalize` destroys it
  through `table_destroy_lock`, which validates nothing.
- The read path stays free of atomics: `table_check_no_append` (one atomic read) sits in
  `table_resolve`, the choke point of every value accessor; `readers_active` is taken only around
  the long windows (a lazy first touch, `materialize_marked`).
- `table_resolve(..., writing=.true.)` is how a write declares itself; a new write specific copies
  its neighbour's call.
- Validity is allocated lazily, so the FIRST null races: guard at the table layer, never in
  `parquet_columns`; `%ensure_validity` is the escape hatch. Bitmap kinds (`ensure_bitmap`) and
  string kinds (`ensure_validity_cap`) can race; temporal kinds allocate nothing and must not be
  refused.
- `%prefetch` gives each thread its own reader, gated by `parallel_prefetch_ok`; every clause there
  is a correctness rule (an unseeded `sample_fraction=` would give each thread a different subset).
  Arrow's own per-column threading stays enabled inside the region; re-measure before changing.
- **Disjoint ROWS are not disjoint BITS**: validity blocks (`parquet_validity_block_bits`, exported
  by `parquet_columns` for this caller) are read-modify-write, so threads filling adjacent row groups
  trim to whole blocks and put the ragged ends in a `critical`
  (`no two row groups' pastes share a validity block`, `test/test_table_parallel.f90`). Publish
  a layout constant a sibling needs for correctness; never copy it.
- Every new guard needs a negative control (`test_table_private_mutation_allowed`,
  `test/test_openmp.f90`).

## `parquet_table` pointers and row-structural mutation

- `%col` returns a live pointer into storage. `%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`,
  `%truncate`, `%append`, `%append_null_rows` reallocate (or may) and bump the generation counter;
  a pointer taken before is dead afterwards. A new row-set mutation goes in
  `parquet_tables_rowmutate.f90` and its doc-comment says it detaches; `..._mutate.f90` never
  changes the row set.
- Pointer invalidation = "changes the row set" ∪ "reallocates storage"; only the first decides the
  file. `%compact`/`%reserve` live in `..._mutate.f90`, invalidate pointers, say so in their
  doc-comments, and advance `%generation()` only when they actually reallocated.
- A row-structural mutation skips a non-resident column (`table_mutable_column`); that column is
  then unreadable and only `table_check_not_detached` reports it. Every path that reads from the
  file after a mutation runs that guard (`table_touch`, `table_resolve_width`,
  `materialize_marked`, `table_reload`, `table_row_group_bounds`); a sixth must too.
- "Detached" means "had a file and can no longer read it": `table_detach` sets the flag only while
  `cache%file_backed` and never clears it; a `parquet_new_table` table never reports detached.
- A returned pointer targets cache-owned heap, never `self` (`check_pointers_go_through_cache`).

## New `parquet_table` state goes on the CACHE

- `parquet_table` itself holds five scalars and one pointer and no allocatable component
  (`check_no_allocatable_component`); every piece of real state lives in `parquet_table_cache`
  behind the pointer (plain, non-finalizable, `allocate`d per open). The absence of allocatable
  components is also what keeps a block-local per-thread table legal under ifx.
- In `open_table_impl`, anything stored on the cache is assigned after `allocate(table%cache)`.
- `table%detached = .false.` is the first executable statement of `open_table_impl` and
  `parquet_new_table`; keep it (`fortran-gotchas.md`, gfortran).
- `%clone` copies `rg_bounds` with an explicit `allocate` plus an element-wise loop, never an
  intrinsic assignment through the pointer component.
- `parquet_debug_table_set_inflight` forces the in-flight counters so both concurrency aborts can
  be provoked from one thread (`table_read_during_append`, `table_append_during_read` scenarios);
  `parquet_debug_table_drop_name_index` (required `had_index` argument) forces `cache_find`'s
  linear-scan fallback (`column lookup falls back to a linear scan when the name index is gone`).
