# API conventions

## Naming

- Public module-level API in the parquet-file-facing modules (the reader/writer/schema/table and
  element/container domains) carries the `parquet_` prefix, procedures and types alike.
- **`pf_` is the prefix for library-wide utility modules whose subject is not a parquet file**
  (`parquet_sorting`, `parquet_argsort`, `parquet_random`, `parquet_sampling`, `parquet_toml`, and
  the later utility tiers). One prefix per module, applied to every public name in it. Never
  "correct" `pf_` to `parquet_`, and never rename the library-level `parquet_get_version`/
  `parquet_kind_name`. Nothing enforces the prefix rule; this file is where it lives.
- A test-only debug hook keeps the project-wide `parquet_debug_*` spelling in every module.
- Type-bound procedures take short unprefixed names; on a collision with another type's
  implementation keep the binding short and rename the private module procedure
  (`set_column_available => set_available`).
- `maml_` is reserved for MAML parsing/building helpers.
- Private module-level helpers keep `parquet_`; private module-level types drop it (intentional).
- C++ bindings (`parquet_bindings.f90`, `parquet_wrapper.cpp`) mirror the C++ names. When a
  binding collides with a public API name, rename the Fortran-side interface with a `c_` prefix and
  leave `bind(C, name=)` untouched (`c_reader_set_filter`, `parquet_get_thread_pool_capacity`,
  `c_get_arrow_version`/`c_get_parquet_version`); `tools/check_bindc_boundary.py` keys on the bound
  name.
- A module cannot share its name with a type or procedure it declares (`parquet_strings`/
  `parquet_string`, `parquet_sorting`/`pf_sort`); check the pair when naming a module after its
  contents, a MAML `dataset:`/`table:` pair included. A binding cannot share a name with a
  component: rename the component, keep the binding.
- A new module holding several related types is named after the DOMAIN (`parquet_temporal`), never
  after one type.
- Grep for an analogous existing name before inventing a convention.

## Numeric arguments: int32 and int64

- A public argument holding a row count, size or index that can legitimately exceed int32 is
  generic over `integer(int32)` and `integer(int64)` (`parquet_get_nrows_int32`/`_int64`). A value
  bounded below `huge(1_int32)` by the format, Arrow or a guard stays a single default `integer`,
  and its `!!` tag says so (`chunk_size`).
- An optional dummy differing only by kind cannot disambiguate specifics: carry the argument-absent
  case as its own specific (`parquet_open_reader_base` + `_nrows_int32`/`_int64`). A required
  argument needs no split; the two kinds share one private `_impl`.

## Errors and diagnostics

- **Every message has one shape**: its class marker, the procedure it came from, then its
  file/schema context. Each of the three is decided in one place, not per call site.
- `error stop` messages in read/write/schema paths append file/schema context through
  `writer_context_suffix`, `reader_filename_suffix`, `maml_name_suffix` (post-open messages only)
  or `table_context_suffix`. A WARNING from the same path appends the same suffix, through the same
  helper: `check_warnings_carry_their_context`, whose allow-list carries a reason per entry.
- A message names the PROCEDURE it came from, inline and type-qualified
  (`pf_spatial_index%build_sky: ...`), not the module. A module's `EP` parameter is for `error stop`
  text and stays there.
- Cap caller-supplied text inside a message to ~100 characters plus `"..."` (`parquet_filter_add`).
- Library output goes through `parquet_emit_info`/`_advice`/`_warning`/`_error_context`
  (`parquet_settings_base`), which supply `"INFO: "`, `"NOTE: "`, `"WARNING: "` and nothing
  respectively; `check_no_direct_printing`. Pick the channel by CLASS: advice is about the caller's
  code or environment and goes quiet at `"silent"`, a warning is a finding about their data and
  survives to `"errors_only"`. `parquet_toml` is the one exception (`module-structure.md`).
- An `error stop` reachable from a parallel region is reached through one `critical` so only one
  thread aborts: `pf_log_fatal` (`parquet_logging`) or `ix_abort` (`parquet_index`;
  `check_index_aborts_go_through_reporter`). Give a new one the same treatment.
- A guard-only subroutine is written impure and its doc-comment says so (`ix_check_mask_len`); ifx
  deletes a `pure` one at `-O0`.

## Mutation guards and finalizers

- A public procedure that mutates a writer, reader, schema or table guards against being called
  twice: check the flag/allocated component first and `error stop "<procedure>: already called for
  this <object>"` before any mutation (`parquet_write_row_mask_impl`, `parquet_new_row_group_impl`).
  Omit only when the second call is provably idempotent (`parquet_validate_user_maml`).
- A `FINAL` procedure never routes through a path that can throw across `extern "C"` or
  `error stop`; give it an "abandon" entry point that frees without validating
  (`abandon_parquet_writer`/`writer_finalize`). Re-derive the finalizable types with
  `grep 'final ::' src/*.f90`.
- `parquet_open_writer`/`parquet_open_reader` keep `intent(out)` (their bodies rely on the implicit
  component reset); prevent misuse another way.
- Every guard needs a negative control test (`testing.md`).

## Settings (`parquet_settings`)

- Any process-global parameter a user could want to change lives in `src/parquet_settings.f90` —
  never as a `parameter` in the module that uses it, never as a new argument threaded through a
  chain.
- **Admission test: a setting may change how FAST, how LARGE or how LOUD the library runs, never
  what it ANSWERS.** No program-wide default for null ordering, qc enforcement or a tolerance.
  Anything expressible as an argument to one call belongs there; an explicit argument always wins
  over a setting.
- Not settings: vocabulary, mathematical facts, Parquet/Arrow format ceilings, container internals,
  input-sanity bounds (`parquet_max_*` are read-only constants by design).
- Every knob needs three assertions: its default, its round trip, and an observed effect with a
  negative control (`feature_risks.md` Risk-41; `check_settings_are_read` is the static half).
- Every knob is resettable, printable, documented and reachable from the environment:
  `check_print_settings_documented` and `check_env_covers_every_setting` take their list from
  `parquet_print_settings`' printed rows, `check_env_table_matches_the_source` compares the guide's
  `PARQUET_FORTRAN_*` table with `parquet_settings_from_env` in both directions, and
  `test_reset_all_knobs` covers reset.
- A knob the C++ side needs is MIRRORED: values cross `bind(C)` already resolved (no tokens, no
  "0 means default"), one push function per group, C++ initialisers equal to the Fortran defaults
  (`feature_risks.md` Risk-42).
- No second way to set the same thing (no `parquet_debug_set_*` for a real setting); observation
  hooks (`parquet_debug_get_*`) are fine.
- Renaming a public setting is a semantic-versioning event; check the published CHANGELOG sections
  first. The C++ side is decoupled by `bind(C, name=)`.
- Every `feature_*.md` carries a settings analysis (`workflow.md`).

## Thread counts

- Two places choose a DEFAULT thread count: `parallel_prefetch_ok` (`parquet_tables_read.f90`) and
  `pf_sort_threads` (`parquet_argsort_kernel.f90`). Both resolve to serial when
  `omp_get_max_threads() <= 1` or `omp_in_parallel()`; an explicit `threads=` is still honoured
  inside a region. Reuse `pf_sort_threads` for a third decision (`omp_get_max_threads()` reads an
  ICV, not the team size).
- **Every resolved thread count is clamped through `parquet_clamp_to_affinity`**
  (`src/parquet_settings_base.f90`), the single home of the `omp_get_num_procs()` clamp and its
  once-per-process warning. A new resolver calls it, never copies it.
  `parquet_debug_set_affinity_procs` and `parquet_debug_reset_affinity_warning` make it testable.
- `parquet_set_threads` sets the per-area caps; never write the number of areas in prose — its
  doc-comment and `test_set_threads` name each other.
- A sorting procedure taking `threads` hands it to every callee that takes one
  (`check_threads_are_forwarded`).

## Reader facts for sibling modules

- `parquet_reader`'s components are private. A sibling module (`parquet_tables`) reaches a reader
  fact only through a public `parquet_*` procedure taking the reader (`parquet_column_has_nulls`,
  `parquet_measure_list_width`, `parquet_column_width_needs_data`), with the full public treatment
  (dual kinds, docs). Never widen component access instead.
