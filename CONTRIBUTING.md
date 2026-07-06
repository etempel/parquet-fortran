# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to *use* the library from your own project, see [README.md](README.md) instead.

## Contents

- [Building and testing this repository](#building-and-testing-this-repository)
  - [Running the error-path tests](#running-the-error-path-tests)
  - [Testing genuine OpenMP concurrency](#testing-genuine-openmp-concurrency)
- [Regenerating the built-in MAML module](#regenerating-the-built-in-maml-module)
- [Extending the MAML schema](#extending-the-maml-schema)
- [Error-handling conventions in `parquet_wrapper.cpp`](#error-handling-conventions-in-parquet_wrappercpp)
- [Features considered but not implemented](#features-considered-but-not-implemented)

## Building and testing this repository

The commands below assume Arrow/Parquet and the environment variables from [README.md's Prerequisites/Environment variables sections](README.md#prerequisites) are already set up — they're needed here too, since this repository builds itself the same way a consuming project would.

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

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, etc.) are reported via Fortran's `error stop`, which aborts the whole process — see [README.md's Error handling section](README.md#error-handling). Since test-drive assertions can't survive an `error stop` in the same process, these paths are exercised out-of-process by a small helper program, `test/error_scenarios.f90`, which is built as its own `fpm` test target named `error_scenarios`.

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

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp` test suite, plus the `concurrent_calls_into_shared_reader`/`writer` error scenarios) need a real OpenMP flag to actually exercise concurrency:

```sh
export FPM_FFLAGS="-fopenmp"
fpm test --features thread_safe
```

`--features thread_safe` itself adds no flags (the OpenMP flag is compiler-dependent and must come from `FPM_FFLAGS` as shown) — it's just a marker this project's own tests use to know real concurrency is expected; some scenario tests spawn `fpm test ... --features thread_safe` subprocesses and rely on the name being present. Running plain `fpm test` (without `--features thread_safe` and `FPM_FFLAGS`) still passes, but doesn't meaningfully exercise these specific concurrency checks.

## Regenerating the built-in MAML module

Every `.maml` file under `docs/` is compiled directly into this library via `tools/generate_parquet_maml.sh`, which scans `docs/*.maml` and writes a Fortran module embedding each file's contents as a string array, keyed by filename — this is what makes a default schema available without shipping or locating a `.maml` file at run time.

Run it from the repository root in `base` mode to regenerate this repository's own embedded module:

```bash
tools/generate_parquet_maml.sh base   # (re)generates src/parquet_maml_base.f90
```

`base` mode generates `parquet_maml_base`, which additionally defines the `parquet_maml_file`/`parquet_maml_missing_column` derived types used throughout the library. Run this whenever `docs/*.maml` changes, or whenever those types themselves change.

The generated file carries a header stating it is auto-generated — do not hand-edit `src/parquet_maml_base.f90`; instead edit the source `.maml` files under `docs/` and re-run the script.

(The same script's other, no-argument mode is a separate, consumer-facing feature for embedding schemas in a downstream project — see [README.md's "Embedding your own MAML schemas in your own project"](README.md#embedding-your-own-maml-schemas-in-your-own-project).)

## Extending the MAML schema

`parquet_validate_maml` checks that section names and known sub-keys are registered in [src/parquet_metadata.f90](src/parquet_metadata.f90) (`allowed_maml_sections`). This checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks — if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed) `allowed_maml_nested_sections` in [src/parquet_metadata.f90](src/parquet_metadata.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly (print a diagnostic to stderr and call `std::abort()` — see `report_fatal_error`/`ConcurrencyGuard` in that file) over `throw`ing and `catch`ing within the same function. Testing found that a C++ exception thrown and caught within the very same function can still go uncaught when the final executable is linked by `gfortran` on macOS: `gfortran`'s driver passes `-no_compact_unwind` to the linker, which breaks libc++abi's stack unwinding for objects compiled by `clang++`, so `try`/`catch` is unreliable across this specific toolchain combination. A `throw` that's never meant to be caught (i.e. deliberately left to crash the process — most of this file's `throw std::runtime_error(...)` sites) is unaffected, since nothing downstream depends on unwinding actually working.

## Features considered but not implemented

These were looked at (during an audit comparing this library against Arrow C++'s broader feature set) and deliberately deferred rather than rejected outright — they're worth revisiting if a concrete use case actually needs them, rather than adding speculatively:

**Plausible future candidates, if needed:**
- `date`/`timestamp` scalar types — Arrow supports these natively; would likely slot into the existing six-type scheme as new entries.
- Streaming/incremental writes — `parquet_write_column` currently buffers a full column in memory before `parquet_close_writer` writes anything (see [README.md's Performance and memory section](README.md#performance-and-memory)); Arrow's `parquet::arrow::FileWriter` supports writing row batches incrementally, which would resolve this but requires reworking the writer's internal buffering model.
- Row-group-level partial reads — `parquet::arrow::FileReader::ReadRowGroup` exists and is unused; would be a bounded step toward "read only some rows," short of full predicate pushdown.
- Per-column writer properties (e.g. `disable_statistics()` for write-heavy/throwaway files, explicit dictionary-encoding toggles) — small, additive, doesn't touch the type system.

**Bigger lifts, worth being cautious about:**
- Predicate pushdown / row filtering — real support means wiring Arrow's expression/compute-filter machinery or per-row-group statistics-based skipping; a genuinely large feature, and the current "no random-access read" limitation is treated as an intentional non-goal for now.
- Nested/struct/map/variable-length-list types — Arrow supports these natively, but they'd break the library's core "flat columns + fixed `col_size` vectors" data model that the whole Fortran-side API is built around; this would be a redesign, not an addition.
- Additional scalar types (`int8`/`int16`/unsigned integers/`decimal`) — straightforward from Arrow's side, but each new type multiplies the `parquet_write_*`/`parquet_read_*` interface surface (a dedicated subroutine pair per type already exists for each of the six supported types).

**Probably out of scope for this library's design:**
- Arrow's `dataset` module (multi-file/partitioned scanning), encryption, and Flight — these serve a different usage pattern (distributed/partitioned datasets, secure transport) than this library's "one file, one reader/writer" design, and adding them would cut against the intentional minimalism this codebase aims for.
