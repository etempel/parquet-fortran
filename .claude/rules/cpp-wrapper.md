---
paths:
  - "src/parquet_wrapper.cpp"
  - "src/parquet_bindings.f90"
  - "src/parquet_settings.f90"
  - "tools/check_bindc_boundary.py"
  - "tools/generate_fixtures.cpp"
---
# The C++ side (`src/parquet_wrapper.cpp`)

## Rules for the file

- **It is not compiled with `-fopenmp` and cannot call any `omp_*` function.** Anything needing an
  OpenMP answer is resolved in Fortran and passed across `bind(C)` as a plain value; never add an
  "auto" sentinel for C++ to interpret.
- **A template needs its own `extern "C++" { … }` block** (the file sits inside one `extern "C"`;
  the error names the template, not the linkage specification). A `std::function` is fine off the
  hot path.
- **Every fatal path goes through `claim_fatal_path_or_park()` + `fatal_exit()`**
  (`std::_Exit(134)`); never call `abort()`/`exit()` directly (glibc's `abort()` deadlocks when
  several threads reach it; the `concurrent_calls_into_shared_*` scenarios). 134 is what a shell
  reports for SIGABRT, and `_Exit` skips the gcov flush exactly as `abort()` did.
- **A per-row helper takes `const arrow::Array *`, never `const std::shared_ptr<arrow::Array> &`**
  (two atomic refcount operations per call; `check_no_per_element_shared_ptr` matches by shape).
- **It stays ONE translation unit** (`check_single_cpp_translation_unit`; CONTRIBUTING.md
  "Features considered but not implemented"). If it is ever split: every file-scope `static`
  global (the `g_debug_*` overrides, the mirrored settings `g_verbosity`/`g_message_stream`/
  `g_sort_counting_path`/`g_sort_counting_bucket_limit`/`g_target_row_group_bytes`/
  `g_statistics_prescreen`, `g_next_thread_token`, `g_fatal_claimed`) becomes an `extern` with one
  definition in a shared header, then re-run every affected error scenario, `test/test_settings.f90`
  and both `concurrent_calls_into_shared_*` scenarios (exactly one message on stderr).
- Uncaught exceptions crossing `extern "C"` terminate the process; the one working `try`/`catch` is
  `parquet_reader_set_filter`. Treat every other `throw` site as unreachable by a clean test.
- `// ====` banner comments are the navigation aid; keep them when adding sections.
- **Code switching on `resolved.leaf_field->type()` peels a dictionary wrapper
  (`unwrap_encoding_layers`) or reads the decoded array's own type**: a top-level dictionary column
  is decoded to a dense array while the reader's schema keeps the dictionary type. Never peel for a
  leaf under a struct path or a list, which the decode does not reach
  (`test/fixtures/dictionary_types.parquet`'s `plain` twin of `cat`).
- **Never range-for over a reference into a temporary** (`for (auto &c : x->Slice(...)->chunks())`):
  C++20 extends no lifetime through the member call, so the loop reads a destroyed object.
  Name the temporary first, then iterate its member.

## Arrow's type singletons

- Every no-argument `arrow::<type>()` factory returns a process-wide singleton with thread-unsafe
  lazy state (its construction, and the cached `fingerprint()`/`metadata_fingerprint()`).
  `ensure_arrow_type_singletons_initialized` (`std::call_once`, at the top of
  `create_parquet_reader` and `create_parquet_writer`) constructs each one and calls both
  fingerprints once. **Every bare `arrow::<type>()` this file uses must be in that list**; grep for
  no-argument factory calls to re-derive it. Parameterized factories (`arrow::timestamp(unit)`,
  `arrow::decimal128(p, s)`) are unaffected.
- A THIRD distinct race on those singletons goes into the same warm-up, and is the point at which to
  consider a full dummy write+close round-trip in that `call_once` block instead.
- A crash whose faulting line changes between runs or fixes, or a clean-looking `error stop`
  followed by a crash in unrelated code, is heap corruption from an early race; re-run the
  sanitizer after every fix, not only the first.

## Mirrored settings

- `parquet_push_settings_to_cpp` runs on every open from every thread, always with the same value.
  Scalar mirrors are `std::atomic`; the string pair (`g_file_date`, …) is mutex-guarded and the push
  compares before assigning. **Any new mirrored setting that owns heap gets the same treatment** —
  a same-value `std::string` store from two threads is a double free.
- **Every entry point reaching a C++ printer calls the grouped `parquet_push_settings_to_cpp`
  first**, not only the opens: otherwise a setting changed after the open is not seen
  (`settings: silent turns parquet_close_reader(print_stat=) into a no-op`, `test/test_errors.f90`).
- `-fsanitize=address` is the tool (valgrind serialises threads; `-check all` moves the layout);
  under ifx add `-mllvm -asan-globals=0`.

## The `bind(C)` boundary

- Run `tools/check_bindc_boundary.py` after touching either side: a signature in
  `src/parquet_bindings.f90`, an `extern "C"` function, or a local `bind(C)` debug-hook interface in
  `test/error_scenarios*.f90`/`test/test_temporal.f90`. It cross-checks arity, base type,
  by-value-vs-by-reference and return type (CI lint stage). It does not check length/ownership
  contracts, rank or NUL-termination.
- Values cross the boundary resolved (`api-conventions.md`, Settings).

## Debug hooks and maintainer diagnostics

- A `parquet_debug_*` hook is an `extern "C"` function reached through a local `bind(C)` interface
  declared inside the test/scenario that uses it — never declared in `src/parquet_bindings.f90`.
  Same convention for maintainer diagnostics (`parquet_get_arrow_bytes_allocated`,
  `parquet_debug_get_*_nanos`). Prefer a C++ hook over a public Fortran one (`testing.md`).
- A debug fixture writer (`parquet_debug_write_*`) bypasses the schema-driven writer path.

## Guarding a hard Arrow int32-only ceiling

Known ceilings: `FixedSizeListBuilder`'s `list_size` (a vector column's `col_size`),
`Schema::num_fields()`/`GetFieldIndex()` (column count), and Parquet's repetition/definition-level
generation per ROW GROUP (apache/arrow#33188) — the last is scoped per row group, so
`close_parquet_writer`'s auto-sizing keeps every row group under it and only an explicit
`chunk_size=` conflicting with `col_size` aborts (`check_chunk_size_fits_limit_for_col_size`,
`check_explicit_chunk_size_fits_arrow_limit`, `check_chunk_size_fits_metadata_limit`). For a new
per-column-total ceiling copy `check_col_size_fits_arrow_limit`/`check_column_count_fits_arrow_limit`:

1. `static constexpr int64_t kArrowInt32...Limit = 2147483647;` named for what it bounds.
2. `static int64_t g_debug_..._limit = -1;` plus `parquet_debug_set_..._limit(int64_t n)` (`<= 0`
   restores), reached only through a local `bind(C)` interface in the scenario.
3. `check_..._fits_arrow_limit(...)` comparing against the override or the constant, calling
   `report_fatal_error` (never a truncating cast) before the value flows into the Arrow API.
4. A scenario (shrink the limit, tiny fixture) + a `test/test_errors*.f90` wrapper asserting the exact
   stderr + `tools/run_error_scenarios.sh` entry + a README Limitations bullet.

The STRING/BINARY offset ceiling is the exception: WIDENED, never refused, on both sides. The writer
picks `large_utf8` from the projected payload; `combine_column_chunks` casts a multi-chunk column
whose payload exceeds the ceiling to 64-bit offsets before concatenating
(`widen_string_chunks_if_needed`). Decide on the payload, never on the chunk count, and resolve the
limit only through `effective_string_offset_limit()`. `scenario_string_read_over_offset_limit`
reaches the path through `parquet_debug_set_force_chunk_split` beside the shrunk limit, with the
`parquet_debug_get_chunk_concat_count`/`_string_offset_widen_count` pair as its controls.
