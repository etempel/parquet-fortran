# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to *use* the library from your own project, see [README.md](README.md) instead.

## Contents

- [AI assistance](#ai-assistance)
- [Conventions](#conventions)
- [Building and testing this repository](#building-and-testing-this-repository)
  - [Running the error-path tests](#running-the-error-path-tests)
  - [Regenerating the test fixtures](#regenerating-the-test-fixtures)
  - [Other tools/ helpers](#other-tools-helpers)
  - [Testing genuine OpenMP concurrency](#testing-genuine-openmp-concurrency)
  - [Continuous integration (GitLab CI)](#continuous-integration-gitlab-ci)
  - [Mirroring to GitHub](#mirroring-to-github)
  - [Publishing to the fpm registry](#publishing-to-the-fpm-registry)
- [Regenerating the built-in MAML module](#regenerating-the-built-in-maml-module)
- [Extending the MAML schema](#extending-the-maml-schema)
- [Error-handling conventions in `parquet_wrapper.cpp`](#error-handling-conventions-in-parquet_wrappercpp)
- [Features considered but not implemented](#features-considered-but-not-implemented)

## AI assistance

Portions of this codebase, including its documentation, were developed with AI assistance (Claude Code).

## Conventions

Three project conventions worth knowing before contributing (all are applied in day-to-day development and enforced in review):

**Naming.** Public module-level API — everything in `src/parquet.f90`'s `public ::` list — carries the `parquet_` prefix (e.g. `parquet_open_reader`, `parquet_get_metadata`). Type-bound procedures (`schema%init`, `reader%...`) are namespaced by their type and do not. The `maml_` prefix is reserved for MAML-parsing/building internal helpers. When in doubt, grep for an existing analogous name before inventing a new one.

**Line length.** Every line in `src/*.f90` and `test/*.f90` — code and comments alike, including trailing end-of-line comments — must stay at or under 132 columns, the standard Fortran free-form limit. Wrap long expressions/strings with `&` continuations and long comments across multiple `!`-prefixed lines rather than letting a line run past 132 columns; don't reach for a compiler flag to paper over it (see `.gitlab-ci.yml`'s `FPM_FFLAGS`, which no longer passes `-ffree-line-length-none`).

**New features need tests and docs.** A new feature should land together with (1) unit-test coverage in the relevant `test/*.f90` suite — plus error-path coverage via `test/error_scenarios.f90` + `test/test_errors.f90` + `tools/run_error_scenarios.sh` if it has failure modes that `error stop` — and (2) documentation updates: a `!>`/`!!` doc-comment on the new public API (picked up automatically by the [FORD-generated reference](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main)), the relevant [user guide page](doc/pages/index.md) for any new behavior/how-to, the [README](README.md) if the landing-page story changes, and this file if it affects contributor workflow. (`CHANGELOG.md` updates are paused pre-1.0 — see CLAUDE.md.)

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

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, etc.) are reported via Fortran's `error stop`, which aborts the whole process — see the [Error handling guide](doc/pages/error-handling.md). Since test-drive assertions can't survive an `error stop` in the same process, these paths are exercised out-of-process by a small helper program, `test/error_scenarios.f90`, which is built as its own `fpm` test target named `error_scenarios`.

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

After one `fpm build --tests`, the script runs scenarios directly against the built `error_scenarios` binary (not through a separate `fpm test error_scenarios -- ...` per scenario) and dispatches them across several scenarios at once via `xargs -P` — each scenario is an independent process writing to its own uniquely-named file under `test_run/`, so this is safe. Defaults to one worker per logical CPU; override with `RUN_ERROR_SCENARIOS_JOBS=N tools/run_error_scenarios.sh` if that's too aggressive for a given machine. The two `concurrent_calls_into_shared_*` scenarios are the exception — they specifically probe OpenMP thread contention, so they still run sequentially, after the parallel batch.

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

### Other tools/ helpers

A few more `tools/` scripts, unrelated to fixtures and not part of the build or test flow:

`tools/count_lines.py` reports code/comment/blank line counts for `src/` and `test/`, a convenience for repository metrics.

`tools/benchmark_threads.sh` measures how write and read throughput scale with Arrow's internal
thread-pool size (`parquet_set_max_threads`), sweeping a log-spaced set of thread counts and
driving `app/benchmark_threads.f90` (a maintainer-only fpm executable, not part of the public
library) once per (mode, thread-count) data point. `MAX_STEPS`, `TARGET_FILE_SIZE_GB` and
`TEST_FILE` are its own env-overridable config (`VECTOR_COL_LEN`/`STRING_LEN` are hardcoded in
`app/benchmark_threads.f90` itself); by default the synthetic test file is written under a fresh
`mktemp -d` directory and deleted when the script exits — set `TEST_FILE` to give it a path of
your own choosing instead, which also keeps the file around afterward for inspection or reuse:

```bash
tools/benchmark_threads.sh
MAX_STEPS=6 TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
TEST_FILE=/tmp/benchmark.parquet TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
```

`tools/test_large_scale.sh` is a manual, user-runnable check (never run by `fpm test`/CI) that this
library genuinely reads/writes columns correctly beyond `huge(1_int32)` (2,147,483,647) rows — the
scale no automated test in this repository ever attempts, since doing so needs a machine with
substantial memory and disk. It drives `app/test_large_scale.f90` (a maintainer/user-only fpm
executable, not part of the public library) through 12 cases, one at a time, each checking
`parquet_get_nrows` and a full read-back against the true data. `NROWS`, `NELEM`, and `MAX_SIZE_GB`
are its env-overridable config — `NROWS` sets the row count for every case (default a small, cheap
`1000`), `NELEM` sets the vector cases' `col_size` (default `2`), and `MAX_SIZE_GB` (default `8`)
skips any case whose estimated uncompressed size would exceed it instead of letting an oversized
value exhaust memory/disk. Progress is printed per case (`Running test X of 12: ...` / `Finished
test X of 12: ... -- PASSED (12.345s)` / `Skipped test X of 12: ...`):

```bash
tools/test_large_scale.sh
# Row count itself beyond huge(1_int32) (needs a large-memory machine); the vector cases here
# (NROWS * NELEM = 6 billion) still round-trip fine via row-group auto-sizing:
NROWS=3000000000 MAX_SIZE_GB=120 tools/test_large_scale.sh
```

`tools/check_doc_anchors.py` validates every `#anchor` link in this repository's `*.md` files — same-file and cross-file — against the anchors GitHub would actually generate for each file's headings (using GitHub's real slugging rules, including the `-1`/`-2` suffixing for repeated headings), and exits nonzero if any link doesn't resolve. Run it after editing headings or anchor links in README.md/CONTRIBUTING.md:

```bash
tools/check_doc_anchors.py
```

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp` test suite, plus the `concurrent_calls_into_shared_reader`/`writer` error scenarios) need a real OpenMP flag to actually exercise concurrency:

```sh
export FPM_FFLAGS="-fopenmp"
fpm test
```

The OpenMP flag is compiler-dependent (see the [Thread safety guide](doc/pages/thread-safety.md) for the per-compiler flags), so it can't be hardcoded in `fpm.toml` and must come from `FPM_FFLAGS` as shown. The two concurrency error-scenario tests are **self-adapting**: they check `omp_get_max_threads()` and, when it's `1` (no OpenMP flag, or `OMP_NUM_THREADS=1`), the shared-reader/writer race cannot occur, so they skip and pass trivially. So plain `fpm test` (no `FPM_FFLAGS`) is still green — it just doesn't meaningfully exercise these specific concurrency checks; set `FPM_FFLAGS="-fopenmp"` to actually verify the guard fires.

### Continuous integration (GitLab CI)

`.gitlab-ci.yml` runs the full `fpm test` suite (with OpenMP and coverage) on a GitLab Docker-executor runner. It builds the whole toolchain from scratch in the container's `before_script`, so it also serves as an executable, always-current recipe for building this project on a clean Debian/Ubuntu system.

A few choices in that file are load-bearing — each one cost a debugging round when it was wrong, so preserve them if you touch it:

- **Base image `ubuntu:24.04`** (pinned with `image:`, since the runner's own default image is older). 24.04 is the oldest Ubuntu that satisfies *every* toolchain requirement at once: gfortran 13 (gfortran ≤ 11 miscompiles the optional allocatable-character argument in `schema%add_col_qc` — see [README's Prerequisites](README.md#prerequisites)), a g++ new enough for C++20 / `std::span`, `pipx` in the repos (used to install `fpm`), and current Arrow apt packages. Its default `gcov` also matches its default compiler, so `gcovr` needs no `--gcov-executable` override.
- **`git lfs pull`** before running tests. The `test/fixtures/*.parquet` files are Git-LFS-tracked (see `.gitattributes`); without pulling them the reader tests read LFS *pointer* files and fail. `git lfs install --skip-repo` sets up only the global filter config (CI never pushes, so the repo-local hooks are deliberately skipped — installing them fails if the checkout already has one).
- **`libarrow-compute-dev`** installed alongside `libarrow-dev` / `libparquet-dev`: Arrow ships its compute kernels in a separate package, and `fpm.toml` links `arrow_compute` (see the [Troubleshooting guide](doc/pages/troubleshooting.md)). Omitting it fails the C++ compile on `arrow/compute/*.h`.
- **`FPM_FFLAGS="--coverage -fopenmp"`.** Source is kept within the standard 132-column free-form limit (see [Conventions](#conventions)), so no `-ffree-line-length-none` override is needed. Setting `FPM_FFLAGS` still *replaces* fpm's default profile flags, so the coverage/OpenMP flags this job needs must be passed explicitly here regardless. `FPM_CXXFLAGS="-std=c++20"` and `FPM_LDFLAGS="-lstdc++"` follow [README's Environment variables](README.md#environment-variables).

Coverage is computed by `gcovr` over `src/` and surfaced through GitLab's `coverage:` regex. For the same line-coverage report locally, `tools/coverage.sh` does the equivalent — it builds with `--coverage`, runs the suite plus every error scenario, and prints per-file and total `src/` coverage (resolving the `gcov` that matches your `gfortran` automatically).

Docs are published by two separate jobs, not the `test` job above: `.gitlab-ci.yml`'s `readthedocs` job (GitLab CI → gitlab.4most.eu's readthedocs-style docserver) and `.github/workflows/docs.yml` (GitHub Actions → GitHub Pages). Both just run `ford docs.md` plus `tools/fix_ford_page_links.sh` (fixes up `doc/pages/*.md` links that FORD doesn't resolve when embedding README.md's raw markdown as its front page — see that script's header) before publishing.

### Mirroring to GitHub

This repository is developed on GitLab (`gitlab.4most.eu`), with a manually-synced read-only
mirror on GitHub (`github.com/etempel/parquet-fortran`) for publication purposes (e.g. the
fortran-lang.org package index, which expects a GitHub repo). There is no automated sync —
push to the mirror by hand whenever you want it updated:

```bash
git remote add github git@github.com:etempel/parquet-fortran.git   # one-time setup
tools/mirror_to_github.sh --github
```

README.md and `doc/pages/*.md` contain a handful of `gitlab.4most.eu`-specific links and badges
that need to point at `github.com` instead once mirrored. `tools/mirror_to_github.sh` handles this
automatically from a disposable local branch — so `main` never carries GitHub-targeted content,
even transiently — via `tools/prep_github_mirroring.sh` (see that script's header for the exact
mechanics, including its `--reverse` mode).

### Publishing to the fpm registry

Account/namespace/token setup and the general `fpm publish` workflow are documented upstream in
[fpm's registry publishing guide](https://fpm.fortran-lang.org/registry/publish.html) — this
section only covers what's specific to *this* repository, which needs a prep step first for two
reasons undocumented upstream:

- The registry [mandatorily enforces module
  naming](https://fpm.fortran-lang.org/registry/naming.html), but `fpm.toml` keeps
  `module-naming = false` on `main` since enabling it breaks local `fpm build`/`fpm test` — the
  `test-drive` dev-dependency's own modules don't comply, and fpm has no per-dependency exemption
  (fpm PR [#828](https://github.com/fortran-lang/fpm/pull/828) / issue #883; re-check whether
  this is still true for whatever fpm version you're publishing with).
- `fpm publish` packages git HEAD, not the working tree or even the staged index (confirmed by
  testing — uncommitted and staged edits are both silently ignored). Prep edits must be committed
  somewhere to take effect, without ever landing on `main`.

**`tools/prep_fpm_publish.sh`** handles both: commits a disposable local branch
(`fpm-publish-prep`, never pushed or merged) with `module-naming` enabled, `test-drive` commented
out, GitHub-facing doc links applied, and maintainer/CI-only files stripped — see the script's own
header comment for the exact file list and mechanics (keep it in sync per CLAUDE.md's "Keeping
tools/prep_fpm_publish.sh in sync"). It then runs fpm's token-free preview commands and
self-checks the resulting tarball's actual contents, exiting nonzero with the specific mismatch if
anything's wrong.

`categories`/`keywords` in `fpm.toml` are free text with no registry-enforced vocabulary
(confirmed by reading the registry backend's source) — the current `categories = ["io"]` needs no
change.

**To publish, once you have a registry token:**

```bash
tools/prep_fpm_publish.sh                       # prep + self-check; stops here if anything's wrong
fpm publish --token TOKEN --dry-run --verbose   # dry run
fpm publish --token TOKEN                       # the real, permanent upload -- cannot be undone
```

**To clean up afterward** (always, whether or not you actually published):

```bash
git checkout main && git branch -D fpm-publish-prep
rm -f fpm_model.json
fpm clean --all
```

Publishing from a GitLab checkout or the GitHub mirror makes no difference — the disposable
branch's committed content determines what's published, not which remote you started from.

## Regenerating the built-in MAML module

Every `.maml` file under `schemas/` is compiled directly into this library via `tools/generate_parquet_maml.sh`, which scans `schemas/*.maml` and writes a Fortran module embedding each file's contents as a string array, keyed by filename — this is what makes a default schema available without shipping or locating a `.maml` file at run time.

Run it from the repository root in `base` mode to regenerate this repository's own embedded module:

```bash
tools/generate_parquet_maml.sh base   # (re)generates src/parquet_maml_base.f90
```

`base` mode generates `parquet_maml_base`, which additionally defines the `parquet_maml_file`/`parquet_maml_missing_column` derived types used throughout the library. Run this whenever `schemas/*.maml` changes, or whenever those types themselves change.

The script also accepts `--dir=<name>` (or `--dir <name>`) to scan a different directory than the default `schemas/` — this project's own regeneration above never needs it (its fixtures live under `schemas/`), but it exists so downstream projects following ["Embedding your own MAML schemas"](doc/pages/embedding-maml-schemas.md) can match whatever convention their own project already uses.

The generated file carries a header stating it is auto-generated — do not hand-edit `src/parquet_maml_base.f90`; instead edit the source `.maml` files under `schemas/` and re-run the script.

(The same script's other, no-argument mode is a separate, consumer-facing feature for embedding schemas in a downstream project — see ["Embedding your own MAML schemas in your own project"](doc/pages/embedding-maml-schemas.md).)

## Extending the MAML schema

`parquet_validate_maml` checks that section names and known sub-keys are registered in [src/parquet_metadata_sections.f90](src/parquet_metadata_sections.f90) (`allowed_maml_sections`). This checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks — if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed) `allowed_maml_nested_sections` in [src/parquet_metadata_sections.f90](src/parquet_metadata_sections.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly (print a diagnostic to stderr and call `std::abort()` — see `report_fatal_error`/`ConcurrencyGuard` in that file) over `throw`ing and `catch`ing within the same function. Testing found that a C++ exception thrown and caught within the very same function can still go uncaught when the final executable is linked by `gfortran` on macOS: `gfortran`'s driver passes `-no_compact_unwind` to the linker, which breaks libc++abi's stack unwinding for objects compiled by `clang++`, so `try`/`catch` is unreliable across this specific toolchain combination. A `throw` that's never meant to be caught (i.e. deliberately left to crash the process — most of this file's `throw std::runtime_error(...)` sites) is unaffected, since nothing downstream depends on unwinding actually working.

## Features considered but not implemented

These were looked at (during an audit comparing this library against Arrow C++'s broader feature set) and deliberately deferred rather than rejected outright — they're worth revisiting if a concrete use case actually needs them, rather than adding speculatively:

**Plausible future candidates, if needed:**
- `date`/`timestamp` scalar types — Arrow supports these natively; would likely slot into the existing six-type scheme as new entries.
- Per-column writer properties (e.g. `disable_statistics()` for write-heavy/throwaway files, explicit dictionary-encoding toggles) — small, additive, doesn't touch the type system.

**Bigger lifts, worth being cautious about:**
- Predicate pushdown (statistics-based I/O skipping) — *not to be confused with row filtering, which is already implemented* (`parquet_filter` / `parquet_open_reader(..., filter=)`, see the [Row filtering section](doc/pages/reading.md#row-filtering-with-parquet_filter)). That existing filter is post-decode: it narrows the rows your code sees but still reads and decodes every referenced column in full. Genuine predicate pushdown — using per-row-group statistics (or Arrow's expression/compute-filter machinery) to skip reading matching row groups off disk entirely — is the unimplemented part, and the README's "No predicate pushdown" limitation treats it as an intentional non-goal for now.
- Nested/struct/map/variable-length-list types — Arrow supports these natively, but they'd break the library's core "flat columns + fixed `col_size` vectors" data model that the whole Fortran-side API is built around; this would be a redesign, not an addition.
- Additional scalar types (`int8`/`int16`/unsigned integers/`decimal`) — straightforward from Arrow's side, but each new type multiplies the `parquet_write_*`/`parquet_read_*` interface surface (a dedicated subroutine pair per type already exists for each of the six supported types).

**Probably out of scope for this library's design:**
- Arrow's `dataset` module (multi-file/partitioned scanning), encryption, and Flight — these serve a different usage pattern (distributed/partitioned datasets, secure transport) than this library's "one file, one reader/writer" design, and adding them would cut against the intentional minimalism this codebase aims for.
