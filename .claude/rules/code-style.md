# Source code conventions

Layout rules for `src/`, `test/`, `app/`, `bench/` and `tools/`. Naming and public-API rules are
in `api-conventions.md`; module placement in `module-structure.md`; language and compiler traps in
`fortran-gotchas.md`.

## Files and program units

- **One program unit per file; filename == unit name** (`parquet_read_numeric.f90` ⇒
  `submodule (parquet:parquet_read) parquet_read_numeric`). fpm does not hard-fail on a mismatch
  (`module-naming = false`); treat it as a firm rule.
- **132-column limit, comments included**, in every Fortran file. Wrap with `&` or split a comment
  across `!` lines; never add `-ffree-line-length-none` or a per-line suppression.
- **Every `submodule` file has its own `implicit none`** right after the `submodule` line;
  contained procedures inherit it.
- **cpp runs over every source file** (`fpm.toml` `[preprocess.cpp]`): never write `/` directly
  followed by `*`, even inside a comment (write `tools/ *.sh`), and never end a comment or string
  with `\`.
- An `allocate` extent taken from `size(...)` asks for `kind=int64` (`check_allocate_extent_kind`);
  `parquet_random`'s bulk routines take lengths as `size(v, kind=int64)` (`check_fill_size_kind`).

## Generated files: edit the generator, never the output

- **Read the top of any `src/*.f90` before editing it.** A `GENERATED FILE -- DO NOT EDIT BY HAND`
  / `automatically generated` banner means the change goes into the generator's input (kind table,
  template) and the generator is re-run. Trust the banner, not the list below.
- Generated today: `parquet_maml_base.f90` (`tools/generate_parquet_maml.sh` + `schemas/*.maml`);
  `parquet_columns.f90`, `_access`, `_mutate` (`tools/generate_parquet_columns.py`, whose kind table
  is the single declaration of a supported column kind); `parquet_tables.f90`, `_access`, `_addcol`,
  `_materialize` (`tools/generate_parquet_tables.py`); the ten sorting/argsort files
  (`tools/generate_parquet_sorting.py`); `parquet_ziggurat.f90` (`tools/generate_parquet_ziggurat.py`);
  `parquet_stats.f90` and `parquet_stats_kernel.f90` only (`tools/generate_parquet_stats.py` — the
  other stats files are hand-written bodies against that generated spec); `parquet_table_example.f90`
  (`tools/generate_user_table_code.py` from `table_types/maml_example4.maml`).
- **`src/parquet_tables.f90` is the table layer's module spec**: adding a `parquet_table` binding
  means editing the generator's template text.
- **`src/parquet_argsort_engine.f90` is hand-written** (its header opens "NOT a generated file" —
  read it, never case-insensitively grep for "generated file"). Its four interfaces live in the
  generated `parquet_argsort.f90` (`emit_engine_interfaces()`): a signature change is a generator
  edit, a body change is not. Every other sorting file, including module-level `save` variables and
  `parquet_debug_*` declarations, is generated.
- **`src/parquet_sorting_oracle.f90` is hand-written** and outside the generator; it is the only
  sorting file allowed to import `parquet_bindings`.
- A generator must emit the project's conventions (`!>`/`!!` docs, `! GCOVR_EXCL_LINE`, 132
  columns; `check_generated_file_conventions`) and should have a `--check` mode that re-derives the
  output and fails on drift; run it after any change in that area.
- Sibling generators share one source of truth (import the kind table). A **consumer-facing**
  generator bakes its copy in and cross-checks it with `--self-test` when the source is present;
  `tools/generate_user_table_code.py` carries `parquet_table`'s binding names this way, so a new
  binding fails lint until that list is updated.
- A generator whose output is user-editable uses marker-delimited windows: refuse a malformed marker
  set before rewriting anything, run `--check` in CI, and stamp a digest of the source input in the
  header (`feature_risks.md` Risk-32).

## Interface blocks and submodules

- **Group interface bodies into several small `interface … end interface` blocks by concern**, each
  under a single-bang `! ---- ... ----` banner (never `!>`, which FORD attaches to the first
  procedure). Never name the blocks: `interface foo` declares a generic.
- **A `module procedure` body lives in a DESCENDANT of the spec declaring its interface.** When an
  interface moves into the same submodule that holds the body, convert that procedure to a plain
  contained procedure (drop `module`, restate the signature with its own doc-comment); descendants
  still reach it by host association (`parquet_parse_col_map`, `parquet_check_read_row_count`).
- **Implement a separate module procedure ABOVE any call to it in the same submodule**; nagfor
  rejects the reverse (`X is not the interface of a separate module procedure`, naming the
  implementation, not the call). Ordinary contained procedures may appear in any order.
- **A private procedure contained directly in a module and called only from its submodules does
  not link** (gfortran; `-Wunused-function` is the early warning). Declare its interface in the
  module and implement it in a submodule (`src/parquet_columns_util.f90` exists for this), or keep
  it public with a comment saying why. Verify by linking.
- **No submodule may name a module-level procedure pointer** (gfortran `-flto` ICE); reach it
  through a public relay in the owning module's `contains` (`parquet_argsort`'s `oracle_*` relays;
  `check_no_submodule_oracle_pointer_call`).
- **An intermediate submodule must not reference a name it reaches only by host association from
  the module above** (nagfor then fails to compile its descendants, `Bad module file format for X,
  could not ref Y`). Give it its own `use ..., only:`, or move the code into a leaf submodule with
  an interface in `parquet_core.f90` (`resolve_temporal_write_unit`). Import a GENERIC of the
  parent's own imports the way the parent does, bare: an `only:` list naming one is rejected by
  gfortran (`use parquet_columns` in `parquet_tables_join.f90`).
- **Keep every submodule chain two deep: module, submodule, leaf.** A submodule three levels below
  its module cannot be compiled by nagfor at all when the middle one reaches ANY host-associated
  name, and there is no `use` for a name declared in a parent submodule. Two leaves that need the
  same private helpers get them from the submodule they share, not from one another
  (`parquet_index_map.f90` holds the multimap's `mm_*` workers for `parquet_index_multi.f90` and
  `parquet_index_str.f90`).
- **A `FINAL` target stays module-contained** (nagfor ICE for every sibling submodule otherwise).
- **A type may not carry more than 255 bindings sorting alphabetically ahead of a SPECIAL binding**
  (defined assignment/operator); flang stores the index in one byte. `parquet_table`'s
  `assign_guard` name is load-bearing; give any new special binding on a large type an early-sorting
  name.
- **A statement may carry at most 255 continuation lines** (nagfor enforces it; the others do not).
  When a `test/test_errors.f90` registration part fills up, add a new part rather than growing one;
  close a split part by removing the comma, not the trailing `&`. Enforced by
  `check_statement_continuation_lines`; run it after adding entries, not only at the end.
- gfortran 15.2 ICE: `parquet_parse_protected_cols` called from a submodule two levels below
  `parquet` crashes the compiler. `parquet_metadata.f90`'s `parquet_parse_protected_cols_relay`
  is the workaround; reuse the relay pattern for the same deferred-length character-array shape.

## Repository layout facts

- `schemas/` holds the `.maml` fixtures the library writes; the whole directory is globbed into
  `src/parquet_maml_base.f90`. **Role-A MAMLs live in `table_types/`** (input to
  `tools/generate_user_table_code.py`); a Role-A MAML in `schemas/` silently becomes an embedded
  fixture. Both generators take `--dir=` (defaults `schemas`/`table_types`).
- Standalone drivers that must be compiled by a bare compiler with forced flags stay in `tools/`
  (`check_random_kernels.f90`, `check_exp_key.f90`, `check_argsort_standalone.f90`,
  `benchmark_random_kernels.f90`), because `bench/` is an fpm source dir and everything in it is
  built by fpm.
- `.claude/rules/*.md` hold the working rules (`developer-environments.md` the machine notes);
  `.claude/skills/*.md` hold the on-demand procedures.
