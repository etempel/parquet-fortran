# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to *use* the library from your own project, see [README.md](README.md) instead.

## Contents

- [Conventions](#conventions)
- [Building and testing this repository](#building-and-testing-this-repository)
  - [Running the error-path tests](#running-the-error-path-tests)
  - [Regenerating the test fixtures](#regenerating-the-test-fixtures)
  - [Testing genuine OpenMP concurrency](#testing-genuine-openmp-concurrency)
  - [Continuous integration (GitLab CI)](#continuous-integration-gitlab-ci)
- [Regenerating the built-in MAML module](#regenerating-the-built-in-maml-module)
- [Extending the MAML schema](#extending-the-maml-schema)
- [Error-handling conventions in `parquet_wrapper.cpp`](#error-handling-conventions-in-parquet_wrappercpp)
- [Features considered but not implemented](#features-considered-but-not-implemented)

## Conventions

Two project conventions worth knowing before contributing (both are applied in day-to-day development and enforced in review):

**Naming.** Public module-level API — everything in `src/parquet.f90`'s `public ::` list — carries the `parquet_` prefix (e.g. `parquet_open_reader`, `parquet_get_metadata`). Type-bound procedures (`schema%init`, `reader%...`) are namespaced by their type and do not. The `maml_` prefix is reserved for MAML-parsing/building internal helpers. When in doubt, grep for an existing analogous name before inventing a new one.

**New features need tests and docs.** A new feature should land together with (1) unit-test coverage in the relevant `test/*.f90` suite — plus error-path coverage via `test/error_scenarios.f90` + `test/test_errors.f90` + `tools/run_error_scenarios.sh` if it has failure modes that `error stop` — and (2) documentation updates: the [README](README.md) / [user manual](MANUAL.md) for any public API or behavior, and this file if it affects contributor workflow. (`CHANGELOG.md` updates are paused pre-1.0 — see CLAUDE.md.)

## Building and testing this repository

The commands below assume Arrow/Parquet and the environment variables from [README.md's Prerequisites](README.md#prerequisites) / [Environment variables](README.md#environment-variables) sections are already set up — they're needed here too, since this repository builds itself the same way a consuming project would.

To test the code:

    fpm test

To clean the build directory:

    fpm clean

To clean the build directory and all dependencies:

    fpm clean --all

To generate the executable:

    fpm install --prefix my_path

The executable is placed in the `my_path/bin` directory. It only prints the parquet-fortran library version number — useful as a quick sanity check that a build/install actually picked up the version you expect.

### Running the error-path tests

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, etc.) are reported via Fortran's `error stop`, which aborts the whole process — see [MANUAL.md's Error handling section](MANUAL.md#error-handling). Since test-drive assertions can't survive an `error stop` in the same process, these paths are exercised out-of-process by a small helper program, `test/error_scenarios.f90`, which is built as its own `fpm` test target named `error_scenarios`.

`error_scenarios` takes a single scenario name as a command-line argument and deliberately triggers the corresponding failure:

```bash
fpm test error_scenarios -- write_undeclared_column
```

Expected output for a failing scenario is an `ERROR STOP` message naming the violated precondition, followed by a Fortran backtrace, and a nonzero process exit code:

```
ERROR STOP parquet_write_column: column not defined in parquet_open_writer: not_a_real_column

Error termination. Backtrace:
...
```

Running it with the `ok` scenario (or no argument at all) does not trigger any failure and exits with status 0:

```bash
fpm test error_scenarios -- ok
```

The full list of scenario names is in the `select case` at the top of `test/error_scenarios.f90`. `test/test_errors.f90` drives every scenario automatically (as part of the `errors` test suite in `fpm test`) and asserts on the exit code; `tools/run_error_scenarios.sh` does the same thing standalone, without going through test-drive:

```bash
tools/run_error_scenarios.sh
```

This prints a `[PASS]`/`[FAIL]` line per scenario and exits nonzero if any scenario's exit code didn't match what was expected — useful for a quick manual check or a CI step that doesn't need the full `fpm test` output. Keep this script's scenario list in sync with `test/error_scenarios.f90`'s `select case` — it's meant to be a complete mirror, not a curated subset.

### Regenerating the test fixtures

A handful of tests read pre-built Parquet files committed under `test/fixtures/` rather than files this library writes itself. These deliberately contain shapes this library's own writer *cannot* produce — genuine Arrow validity-bitmap Nulls, an unsupported physical column type, and a list-encoded (per-row array) vector column — so that the reader's handling of them can be exercised. They are built directly against the Arrow/Parquet C++ API by `tools/generate_fixtures.cpp` (one function per fixture):

- `has_null.parquet` — columns with real Nulls (not sentinel values), read by the `errors`/`reading` suites.
- `unsupported_type.parquet` — a column of a physical type this library refuses to read.
- `list_vector.parquet` — a vector column stored as Parquet `LIST` rather than the fixed-size layout this library writes.

Because these files are committed, a normal `fpm test` never needs to regenerate them. Rebuild them only when you change `generate_fixtures.cpp` or otherwise need a fixture recreated, via:

```bash
tools/run_generate_fixtures.sh
```

This compiles `generate_fixtures.cpp` with `clang++` and runs it from the repository root, rewriting every fixture under `test/fixtures/`. It needs the same `FPM_CXXFLAGS`/`FPM_LDFLAGS` (Arrow/Parquet include/link flags) used to build the project itself — see [README.md's Environment variables section](README.md#environment-variables); the script errors out early if they are unset.

(Two unrelated helpers live in the same folder: `tools/count_lines.py` reports code/comment/blank line counts for `src/` and `test/`, a convenience for repository metrics; `tools/check_doc_anchors.py` validates every `#anchor` link in this repository's `*.md` files — same-file and cross-file — against the anchors GitHub would actually generate for each file's headings (using GitHub's real slugging rules, including the `-1`/`-2` suffixing for repeated headings), and exits nonzero if any link doesn't resolve. Run it after editing headings or anchor links in README.md/CONTRIBUTING.md:

```bash
tools/check_doc_anchors.py
```

Neither is part of the build or test flow.)

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp` test suite, plus the `concurrent_calls_into_shared_reader`/`writer` error scenarios) need a real OpenMP flag to actually exercise concurrency:

```sh
export FPM_FFLAGS="-fopenmp"
fpm test
```

The OpenMP flag is compiler-dependent (see [README's Thread safety section](MANUAL.md#thread-safety) for the per-compiler flags), so it can't be hardcoded in `fpm.toml` and must come from `FPM_FFLAGS` as shown. The two concurrency error-scenario tests are **self-adapting**: they check `omp_get_max_threads()` and, when it's `1` (no OpenMP flag, or `OMP_NUM_THREADS=1`), the shared-reader/writer race cannot occur, so they skip and pass trivially. So plain `fpm test` (no `FPM_FFLAGS`) is still green — it just doesn't meaningfully exercise these specific concurrency checks; set `FPM_FFLAGS="-fopenmp"` to actually verify the guard fires.

### Continuous integration (GitLab CI)

`.gitlab-ci.yml` runs the full `fpm test` suite (with OpenMP and coverage) on a GitLab Docker-executor runner. It builds the whole toolchain from scratch in the container's `before_script`, so it also serves as an executable, always-current recipe for building this project on a clean Debian/Ubuntu system.

A few choices in that file are load-bearing — each one cost a debugging round when it was wrong, so preserve them if you touch it:

- **Base image `ubuntu:24.04`** (pinned with `image:`, since the runner's own default image is older). 24.04 is the oldest Ubuntu that satisfies *every* toolchain requirement at once: gfortran 13 (gfortran ≤ 11 miscompiles the optional allocatable-character argument in `schema%add_col_qc` — see [README's Prerequisites](README.md#prerequisites)), a g++ new enough for C++20 / `std::span`, `pipx` in the repos (used to install `fpm`), and current Arrow apt packages. Its default `gcov` also matches its default compiler, so `gcovr` needs no `--gcov-executable` override.
- **`git lfs pull`** before running tests. The `test/fixtures/*.parquet` files are Git-LFS-tracked (see `.gitattributes`); without pulling them the reader tests read LFS *pointer* files and fail. `git lfs install --skip-repo` sets up only the global filter config (CI never pushes, so the repo-local hooks are deliberately skipped — installing them fails if the checkout already has one).
- **`libarrow-compute-dev`** installed alongside `libarrow-dev` / `libparquet-dev`: Arrow ships its compute kernels in a separate package, and `fpm.toml` links `arrow_compute` (see [MANUAL's Troubleshooting](MANUAL.md#troubleshooting)). Omitting it fails the C++ compile on `arrow/compute/*.h`.
- **`FPM_FFLAGS="--coverage -fopenmp -ffree-line-length-none"`.** Some source lines exceed the 132-column free-form limit; recent gfortran ignores that limit by default but older versions (e.g. gfortran 11) truncate and error, so the flag is passed unconditionally. Setting `FPM_FFLAGS` *replaces* fpm's default profile — which would otherwise supply `-ffree-line-length-none` — so it must be passed explicitly here. `FPM_CXXFLAGS="-std=c++20"` and `FPM_LDFLAGS="-lstdc++"` follow [README's Environment variables](README.md#environment-variables).

Coverage is computed by `gcovr` over `src/` and surfaced through GitLab's `coverage:` regex. For the same line-coverage report locally, `tools/coverage.sh` does the equivalent — it builds with `--coverage`, runs the suite plus every error scenario, and prints per-file and total `src/` coverage (resolving the `gcov` that matches your `gfortran` automatically).

## Regenerating the built-in MAML module

Every `.maml` file under `docs/` is compiled directly into this library via `tools/generate_parquet_maml.sh`, which scans `docs/*.maml` and writes a Fortran module embedding each file's contents as a string array, keyed by filename — this is what makes a default schema available without shipping or locating a `.maml` file at run time.

Run it from the repository root in `base` mode to regenerate this repository's own embedded module:

```bash
tools/generate_parquet_maml.sh base   # (re)generates src/parquet_maml_base.f90
```

`base` mode generates `parquet_maml_base`, which additionally defines the `parquet_maml_file`/`parquet_maml_missing_column` derived types used throughout the library. Run this whenever `docs/*.maml` changes, or whenever those types themselves change.

The generated file carries a header stating it is auto-generated — do not hand-edit `src/parquet_maml_base.f90`; instead edit the source `.maml` files under `docs/` and re-run the script.

(The same script's other, no-argument mode is a separate, consumer-facing feature for embedding schemas in a downstream project — see [README.md's "Embedding your own MAML schemas in your own project"](MANUAL.md#embedding-your-own-maml-schemas-in-your-own-project).)

## Extending the MAML schema

`parquet_validate_maml` checks that section names and known sub-keys are registered in [src/parquet_metadata.f90](src/parquet_metadata.f90) (`allowed_maml_sections`). This checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks — if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed) `allowed_maml_nested_sections` in [src/parquet_metadata.f90](src/parquet_metadata.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly (print a diagnostic to stderr and call `std::abort()` — see `report_fatal_error`/`ConcurrencyGuard` in that file) over `throw`ing and `catch`ing within the same function. Testing found that a C++ exception thrown and caught within the very same function can still go uncaught when the final executable is linked by `gfortran` on macOS: `gfortran`'s driver passes `-no_compact_unwind` to the linker, which breaks libc++abi's stack unwinding for objects compiled by `clang++`, so `try`/`catch` is unreliable across this specific toolchain combination. A `throw` that's never meant to be caught (i.e. deliberately left to crash the process — most of this file's `throw std::runtime_error(...)` sites) is unaffected, since nothing downstream depends on unwinding actually working.

## Features considered but not implemented

These were looked at (during an audit comparing this library against Arrow C++'s broader feature set) and deliberately deferred rather than rejected outright — they're worth revisiting if a concrete use case actually needs them, rather than adding speculatively:

**Plausible future candidates, if needed:**
- `date`/`timestamp` scalar types — Arrow supports these natively; would likely slot into the existing six-type scheme as new entries.
- Streaming/incremental writes — `parquet_write_column` currently buffers a full column in memory before `parquet_close_writer` writes anything (see [README.md's Performance and memory section](MANUAL.md#performance-and-memory)); Arrow's `parquet::arrow::FileWriter` supports writing row batches incrementally, which would resolve this but requires reworking the writer's internal buffering model.
- Row-group-level partial reads — `parquet::arrow::FileReader::ReadRowGroup` exists and is unused; would be a bounded step toward "read only some rows," short of full predicate pushdown.
- Per-column writer properties (e.g. `disable_statistics()` for write-heavy/throwaway files, explicit dictionary-encoding toggles) — small, additive, doesn't touch the type system.

**Bigger lifts, worth being cautious about:**
- Predicate pushdown (statistics-based I/O skipping) — *not to be confused with row filtering, which is already implemented* (`parquet_filter` / `parquet_open_reader(..., filter=)`, see [README.md's Row filtering section](MANUAL.md#row-filtering-with-parquet_filter)). That existing filter is post-decode: it narrows the rows your code sees but still reads and decodes every referenced column in full. Genuine predicate pushdown — using per-row-group statistics (or Arrow's expression/compute-filter machinery) to skip reading matching row groups off disk entirely — is the unimplemented part, and the current "no random-access read" limitation treats it as an intentional non-goal for now.
- Nested/struct/map/variable-length-list types — Arrow supports these natively, but they'd break the library's core "flat columns + fixed `col_size` vectors" data model that the whole Fortran-side API is built around; this would be a redesign, not an addition.
- Additional scalar types (`int8`/`int16`/unsigned integers/`decimal`) — straightforward from Arrow's side, but each new type multiplies the `parquet_write_*`/`parquet_read_*` interface surface (a dedicated subroutine pair per type already exists for each of the six supported types).

**Probably out of scope for this library's design:**
- Arrow's `dataset` module (multi-file/partitioned scanning), encryption, and Flight — these serve a different usage pattern (distributed/partitioned datasets, secure transport) than this library's "one file, one reader/writer" design, and adding them would cut against the intentional minimalism this codebase aims for.
