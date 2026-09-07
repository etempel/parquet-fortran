# Module structure and placement

## The tree (a map of the SHAPE; `ls src/` is the authority for membership)

```
parquet                         (module — the FACADE; re-exports the whole user surface, holds NO code)
parquet_io                      (module — I/O FACADE: re-exports parquet_core + parquet_settings only)
parquet_core                    (module — core API spec + cross-subtree private-helper interfaces)
├─ parquet_read                 (submodule — reader lifecycle, queries, shared read helpers)
│   ├─ parquet_read_numeric / _string / _temporal / _list / _map / _struct   (type families)
│   ├─ parquet_read_filter      (filter/sort-key lexing and parsing)
│   └─ parquet_read_sort        (read-time sort key installation)
├─ parquet_write                (submodule — writer lifecycle, shared write helpers)
│   └─ parquet_write_numeric / _string / _temporal / _list / _map / _struct
└─ parquet_metadata             (submodule — parse/build orchestration + shared metadata helpers)
    ├─ parquet_metadata_base    (format-agnostic column_info/table_metadata plumbing)
    ├─ parquet_metadata_get     (parquet_get_metadata queries)
    └─ parquet_metadata_maml    (MAML section schema + all validation)

parquet_bindings                (module — bind(C) interfaces to parquet_wrapper.cpp; never re-exported)
parquet_settings_base           (module — LEAF: every knob's state, getters, setters, the three emit channels)
parquet_settings                (module — settings_base + bindings: print, reset, env, the ONE C++ push,
                                 parquet_get_arrow_version)
parquet_version                 (module — LEAF: cversion and parquet_get_version)
parquet_strings / parquet_temporal            (element domains; Arrow-free)
parquet_columns                 (module — parquet_column + the abstract container base; GENERATED spec)
parquet_list / parquet_struct / parquet_map   (container domains; Arrow-free)
parquet_random                  (module — LEAF generator + distributions; reaches parquet_expkey and
                                 parquet_ziggurat only)
parquet_argsort                 (module — ARGSORT TIER: pf_argsort over intrinsic types, pf_sort_threads,
                                 the sorting knobs; reaches settings_base only)
├─ parquet_argsort_engine       (HAND-WRITTEN comparators + radix/merge/counting)
└─ parquet_argsort_kernel       (GENERATED extractors, drivers, the thread rule)
parquet_sorting                 (module — FULL sorting tier over the argsort tier)
parquet_sorting_oracle          (module — TEST-ONLY C++ sort engine behind procedure pointers)
parquet_sampling                (module — permutations/subsets/resampling/weighted draws)
parquet_spatial / parquet_healpix / parquet_index / parquet_stats / parquet_utils / parquet_logging
                                (Arrow-free utility tiers, each with submodules)
parquet_toml                    (module — pf_toml; the only module reaching a third-party package)
parquet_tables                  (module — parquet_table; GENERATED spec + a dozen submodules)
parquet_maml_base               (module — generated) └─ parquet_maml_base_add_col_qc
parquet_wrapper.cpp             (the single C++ translation unit)
```

## Entry modules, tiers and footprints

- **The advertised entry modules are the rows of the table in
  `doc/pages/operating/choosing-a-module.md`**; the same list is `ENTRY_MODULES` in
  `tools/check_module_footprints.sh`. Each carries the semantic-versioning promise in its own right
  (a `parquet_column` binding change is breaking even when `use parquet` is unaffected).
- **Every Arrow-free tier keeps its Fortran graph clear of `parquet_bindings`**, one lint check per
  tier walking the closure including submodules; re-derive the set with
  `grep -oE "def check_parquet_[a-z_]*stays_(arrow_free|leaf)" tools/check_source_conventions.py`.
  `tools/check_argsort_standalone.sh` compiles the argsort tier with a bare compiler. The PACKAGE is
  never Arrow-free (`link` is package-level); say which of the two you mean.
- **A `use` line anywhere can multiply what a consumer compiles** (fpm prunes per module, never per
  submodule). `tools/check_module_footprints.sh` diffs each entry module's compiled set against
  `tools/module_footprints.txt`; when it fails, move the needing code up a tier — updating the
  expectation is almost never the fix.
- **Where a procedure goes is a TIER decision**: anything reaching a reader, a writer or
  `parquet_bindings` is at or above `parquet_core`; anything a `use parquet_sampling` program needs
  is in `parquet_argsort` or lower. When a leaf needs one procedure from a heavyweight module, move
  the procedure down a tier; never copy it or accept the edge.
- **A module re-exports, getter and setter, every settings knob its own code reads** (including
  `verbosity`/`message_stream` when it can emit). `test/test_module_surface.f90` holds one
  single-import module per tier; never add a second library `use` to any module in that file.
- **Placement of a read/write helper**: type-generic code goes in the parent (`parquet_read`/
  `parquet_write`) as an ordinary contained procedure; type-specific code in the matching child. A
  public generic's specifics and any type-bound binding target keep their interface in
  `parquet_core.f90`. A helper needing a host-associated name goes in a leaf, not the parent
  (`code-style.md`, interface rules).
- One module per DOMAIN of related types (`parquet_temporal`, `parquet_list`/`_map`/`_struct`).
  Writing a nested container is refused, not missing (CONTRIBUTING.md, "Features considered but not
  implemented").

## The facades

- `parquet` and `parquet_io` hold no code. `parquet` re-exports its siblings with bare `use` and
  default-public accessibility (re-derive the list from `src/parquet.f90`'s `use` lines);
  `parquet_io` re-exports `parquet_core` and `parquet_settings`. `use parquet` and `use parquet_io`
  are the two supported spellings of the I/O surface; `parquet_core` is internal and documented so.
- **The core API's spec file is `src/parquet_core.f90`**: every `public ::`, interface body and
  host-associated `parameter`. The facade gains no logic (no `contains`, no parameter).
- **Anything the facade imports for its own use gets its own `private ::` line**
  (`check_facade_inventory_matches_its_use_lines`); `parquet_bindings` is never re-exported;
  `parquet_maml_base` is imported with an `only:` list. The two facades hide the SAME names
  (`check_facades_hide_the_same_names`); `src/parquet.f90` must not repeat a `private ::` for a name
  `parquet_io` already hid (nagfor reports it as an implicitly typed local).
- **`parquet_version` is re-exported by the facade only**: `parquet_get_version` is deliberately
  unavailable from `parquet_io`, `parquet_tables`, `parquet_settings` and every Arrow-free tier
  (`use parquet_version` costs two files). `parquet_get_arrow_version` lives in `parquet_settings`
  (it calls into C++). Pinned by `test_facade_covers_every_layer`, `test_module_surface_version`,
  `check_get_version_has_one_home`.
- Sibling modules `use parquet_core`, never `use parquet` (circular).
- `test_facade_covers_every_layer` (bare `use parquet`) and `test_module_surface_io` (bare
  `use parquet_io`) break the build when a re-export is dropped.

## `parquet_toml`

- The one module that emits through `parquet_logging` instead of the settings channels:
  `parquet_verbosity`/`parquet_message_stream` govern nothing it prints, and output appears without
  `pf_log_init`. Do not reroute it, and do not give another module this exception without the same
  audience argument (its diagnostics belong to the calling program's log).
- It brings `toml-f`, the only Fortran package dependency: every consumer fetches it; `fpm publish`
  is permanently off (CONTRIBUTING.md, "Why not the fpm registry?"); `tools/module_footprints.txt`
  counts this library's own files only.
- Every public entry takes `!$omp critical (parquet_toml_guard)` and no private worker does. Read
  specifics take it inline with no `return` inside the region; branchier entries use a thin wrapper
  plus a guard-free worker. Enforced by `check_parquet_toml_takes_the_guard`.

## `parquet_random` is a LEAF; `parquet_sampling` takes anything more

- `src/parquet_random.f90` reaches `parquet_expkey`, `parquet_ziggurat` and compiler-supplied
  modules (`iso_fortran_env`, `iso_c_binding`, `ieee_arithmetic`, `omp_lib`) only.
  `check_parquet_random_stays_leaf` walks the closure and also requires every reached module to be
  in the `SRC` list of every `tools/*.sh` that compiles `parquet_random` (derived by glob).
  `tools/check_exp_key.sh` compiles `parquet_expkey.f90` ALONE and must never gain a
  `parquet_random` entry.
- If the closure clause fires, do NOT add the module to a `SRC` list; move the code into
  `parquet_sampling`. If the `SRC` clause fires, add the module to both lists.
- Placement: drawing a NUMBER → `parquet_random`; drawing from a POPULATION, or anything needing a
  thread rule, a sort or a setting → `parquet_sampling`.
- `parquet_sampling` reaches the generator through the public exact pass-throughs
  `pf_random_int_at`/`pf_random_key`/`pf_random_fill_draws`; keep them exact.
- `tools/check_random_kernels.sh` and `tools/check_exp_key.sh` verify the compile-time forks; run
  both under `FC=nagfor` as well — the only run covering the `PF_SAFE64` arm as a shipped build, and
  the only one where `-Wc,-march=native` reaches the FMA class.

## `parquet_random` word spaces

- One `(seed, stream)` names FOUR independent 32-bit word sequences through the domain tags
  `DOM_REAL64` (must stay 0), `DOM_REAL32`, `DOM_INT_NARROW`, `DOM_INT_WIDE` OR-ed into the block
  index. Every `random_block` call site names its tag explicitly, `DOM_REAL64` included.
- A new generic decides which space it reads. Two contract identities: `pf_random_at` is the top 53
  bits of `pf_random_bits_at`; `pf_random_exp_at` is `-log(1 - u)` for that same `u`.
- No call site passes a block index above the one its own draw addresses (`feature_risks.md`
  Risk-133).

## Sorting tiers

- `parquet_argsort` (`pf_argsort` over the six intrinsic types, `pf_sort_threads`, `sort_key_buf`,
  the sorting knobs, the oracle's procedure pointers; reaches `parquet_settings_base` only) sits
  below `parquet_sorting` (the five down-tier element types and every other `pf_` operation). They
  share the `pf_` vocabulary because they share the generic.
- `parquet_sorting_oracle` is test-only and never re-exported by any facade.
