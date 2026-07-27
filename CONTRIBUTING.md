# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to *use* the library from your own project, see [README.md](README.md) instead.

## Contents

- [AI assistance](#ai-assistance)
- [Conventions](#conventions)
- [Building and testing this repository](#building-and-testing-this-repository)
  - [Running a single test suite/test](#running-a-single-test-suitetest)
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

**New features need tests and docs.** A new feature should land together with (1) unit-test coverage in the relevant `test/*.f90` suite — plus error-path coverage via `test/error_scenarios.f90` + `test/test_errors.f90` + `tools/run_error_scenarios.sh` if it has failure modes that `error stop` — and (2) documentation updates: a `!>`/`!!` doc-comment on the new public API (picked up automatically by the FORD-generated reference — build it locally with `ford docs.md`, or see whichever published copy your checkout links to: `https://www.4most.eu/readthedocs/etempel/parquet-fortran/main` on canonical GitLab, `https://etempel.github.io/parquet-fortran/` on the GitHub mirror), the relevant [user guide page](doc/pages/index.md) for any new behavior/how-to, the [README](README.md) if the landing-page story changes, and this file if it affects contributor workflow. (`CHANGELOG.md` updates are paused pre-1.0 — see CLAUDE.md.)

A plain `ford docs.md` run does **not** verify that a new doc-comment was actually added — its undocumented-entity warnings are opt-in and off by default in this project. To actually check coverage before committing, run `ford --warn docs.md` instead; see CLAUDE.md's "FORD doc-comment conventions" for what to expect in its (noisy) output and which warning categories are already-accepted noise.

## Building and testing this repository

The commands below assume Arrow/Parquet and the environment variables from [README.md's Prerequisites](README.md#prerequisites) / [Environment variables](README.md#environment-variables) sections are already set up — they're needed here too, since this repository builds itself the same way a consuming project would.

**Git LFS is also required locally**, not just in CI: `test/fixtures/*.parquet` (see [Regenerating the test fixtures](#regenerating-the-test-fixtures)) are Git-LFS-tracked, and `.lfsconfig` sets `skipdownloaderrors = true` — so cloning without `git lfs` installed **succeeds silently**, leaving ~128-byte pointer files in place of the real Parquet fixtures, and the reader/error-scenario tests that depend on them then fail with confusing errors that have nothing to do with the code you're testing. Before building for the first time:

```bash
git lfs install
git lfs pull
ls -l test/fixtures/*.parquet   # each file should be well over 128 bytes
```

To test the code:

    fpm test

To clean the build directory:

    fpm clean

To clean the build directory and all dependencies:

    fpm clean --all

To generate the executables:

    fpm install --prefix my_path

`fpm.toml` sets `auto-executables = true`, so **five** executables are built from `app/*.f90` and placed in `my_path/bin`:

| Executable | Source | Purpose |
|---|---|---|
| `run_parquet_fortran` | `app/program.f90` | Prints the parquet-fortran library version number — a quick sanity check that a build/install actually picked up the version you expect. The only one of the five that ships in the fpm-published package (see `tools/prep_fpm_publish.sh`) — the other four are maintainer/CI-only dev tools kept in this repository but stripped from what a consumer installs. |
| `benchmark_threads` | `app/benchmark_threads.f90` | Driven by `tools/benchmark_threads.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `test_large_scale` | `app/test_large_scale.f90` | Driven by `tools/test_large_scale.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `playground` | `app/playground.f90` | Maintainer scratch file for trying out Fortran code; no fixed purpose. |
| `demo_print_schema_info` | `app/demo_print_schema_info.f90` | Maintainer demo for reviewing `schema%print_schema_info`'s output. |

If `fpm test` behaves unexpectedly right after a source change (e.g. a test seems to still run old
code, or `error_scenarios` reports a scenario name as unrecognized even though it's clearly in
`test/error_scenarios.f90`), try `fpm clean --skip` before spending time debugging further — fpm's
build cache can serve a stale binary, and building with several different `FPM_FFLAGS` values
creates multiple `build/gfortran_<hash>/` directories, one of which
`test/test_errors.f90`'s `error_scenarios_bin` may pick up instead of the current one. Prefer
`--skip` over the heavier `fpm clean --all`: `--all` also discards external dependencies, forcing a
`test-drive` re-download that's never the cause of this particular symptom, whereas `--skip` fixes
the same stale-binary problem without that extra cost.

### Running a single test suite/test

While iterating on one area, prefer scoping `fpm test` to just the relevant suite (or a single
named test within it) over running the full suite every time — the full suite, including the
OpenMP and error-scenario subprocess tests, takes much longer:

```bash
fpm test run_tester -- reading                      # one suite
fpm test run_tester -- reading "test name"           # one test within a suite
```

The full list of suite names is the `new_testsuite(...)` array in `test/run_tester.f90`: `writing`,
`reading`, `maml`, `errors`, `examples`, `metadata`, `openmp_write`, `openmp`, `parquet_string`,
`temporal`.

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

A handful of tests read pre-built Parquet files committed under `test/fixtures/` rather than files this library writes itself. These deliberately contain shapes this library's own writer *cannot* produce — genuine Arrow validity-bitmap Nulls, an unsupported physical column type, a list-encoded (per-row array) vector column, extended read-only source types, and a `STRUCT` column — so that the reader's handling of them can be exercised. They are built directly against the Arrow/Parquet C++ API by `tools/generate_fixtures.cpp` (one function per fixture):

- `has_null.parquet` — columns with real Nulls (not sentinel values), read by the `errors`/`reading` suites.
- `unsupported_type.parquet` — a column of a physical type this library refuses to read.
- `list_vector.parquet` — a vector column stored as Parquet `LIST` rather than the fixed-size layout this library writes.
- `extended_types.parquet` — columns of the extended read-only source types (`int8`/`int16`/unsigned integers/`half_float`/`decimal`).
- `nested_struct.parquet` — a `STRUCT` column, nested 3 levels deep, with a `FIXED_SIZE_LIST` vector-column leaf in a sibling `STRUCT` column, and rows covering every independent null-combination source (see [Reading a nested struct field](doc/pages/supported-data-types.md#reading-a-nested-struct-field)).
- `map_list_types.parquet` — exercises Arrow's `MAP` and (variable-length) `LIST` types, generated by `generate_map_list_types_fixture` in `tools/generate_fixtures.cpp`. Currently **reserved/unused**: no Fortran test reads it, since `MAP` columns and struct-nested variable-length `LIST` columns remain unsupported (see [Features considered but not implemented](#features-considered-but-not-implemented)) — kept as groundwork for if that support is ever added.

Because these files are committed, a normal `fpm test` never needs to regenerate them. Rebuild them only when you change `generate_fixtures.cpp` or otherwise need a fixture recreated, via:

```bash
tools/run_generate_fixtures.sh
```

This compiles `generate_fixtures.cpp` with `clang++` and runs it from the repository root, rewriting every fixture under `test/fixtures/`. It needs the same `FPM_CXXFLAGS`/`FPM_LDFLAGS` (Arrow/Parquet include/link flags) used to build the project itself — see [README.md's Environment variables section](README.md#environment-variables); the script errors out early if they are unset.

The `clang++` invocation is unconditional, with no `FPM_CXX`/`CXX` override — so fixture regeneration is not possible as-is on a GCC-only Linux box (the same platform this project's own CI uses) without editing the script first.

### Other tools/ helpers

A few more `tools/` scripts, unrelated to fixtures and not part of the build or test flow:

`tools/count_lines.py` reports code/comment/blank line counts for `src/` and `test/`, a convenience for repository metrics.

`tools/count_tests.sh` counts test-drive unit tests per suite directly from source (no build or run required): it reads `test/run_tester.f90`'s `new_testsuite(...)` registrations, locates each suite's `collect_tests_parquet_*` subroutine, and counts the `new_unittest(...)` entries inside it — cross-checked against an actual `fpm test run_tester` run's PASSED/FAILED line count. Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).

`tools/benchmark_threads.sh` measures how write and read throughput scale with Arrow's internal
thread-pool size (`parquet_set_max_threads`), sweeping a log-spaced set of thread counts and
driving `app/benchmark_threads.f90` (a maintainer-only fpm executable, not part of the public
library) once per (mode, thread-count) data point. The synthetic file's schema is 5 scalar
columns (int32/int64/float32/float64/boolean), replicated `NMULT` times (`i32_1`, `i32_2`, ...,
`i64_1`, ...) so the file has `5*NMULT` columns — threading benefits more from many columns than
from few. `MAX_STEPS`, `TARGET_FILE_SIZE_GB`, `NMULT` and `TEST_FILE` are its own env-overridable
config; by default the synthetic test file is written under a fresh `mktemp -d` directory and
deleted when the script exits — set `TEST_FILE` to give it a path of your own choosing instead,
which also keeps the file around afterward for inspection or reuse:

```bash
tools/benchmark_threads.sh
MAX_STEPS=6 TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
NMULT=20 TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
TEST_FILE=/tmp/benchmark.parquet TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
```

`tools/test_large_scale.sh` is a manual, user-runnable check (never run by `fpm test`/CI) that this
library genuinely reads/writes columns correctly beyond `huge(1_int32)` (2,147,483,647) rows — the
scale no automated test in this repository ever attempts, since doing so needs a machine with
substantial memory and disk. It drives `app/test_large_scale.f90` (a maintainer/user-only fpm
executable, not part of the public library) through 13 cases (7 scalar types plus 6 vector-column cases, one per type), one at a time,
each checking `parquet_get_nrows` and a full read-back against the true data. Set the
`RUN_VECTOR_CASES` compile-time parameter at the top of `app/test_large_scale.f90` to `.false.` and
rebuild to skip the 6 vector cases and run only the 7 scalar ones. `NROWS`, `NELEM`, and
`MAX_SIZE_GB` are its env-overridable config — `NROWS` sets the row count for every case (default a
small, cheap `1000`), `NELEM` sets the vector cases' `col_size` (default `2`), and `MAX_SIZE_GB`
(default `8`) skips any case whose estimated uncompressed size would exceed it instead of letting an
oversized value exhaust memory/disk. Progress is printed per case (`Running test X of 13: ...` /
`Finished test X of 13: ... -- PASSED (12.345s)` / `Skipped test X of 13: ...`, or `of 7` when
`RUN_VECTOR_CASES = .false.`):

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

`tools/generate_logo_svg.py` regenerates `doc/media/logo.svg`/`logo.png`/`logo-192.png`/
`favicon.png` from an original raster source: it traces the raster into an editable SVG via
`vtracer`, then a polish pass recolors/gradient-fills the traced badge shape and adds a shadowed
outline to the letter glyph, and finally rasterizes PNGs back out at each needed size. Only needed
if the logo itself changes — see the script's own header comment for the full pipeline and its
tuning flags.

`tools/convert_fits_to_parquet.py` converts a FITS binary table into a `.parquet` file carrying
this library's own VOTable-style key-value metadata (`column.<name>.unit`/`description`/`ucd`/
`data_type`/`array_size`/`col_size`, plus flat table-level keys), so `parquet_get_metadata` can
read it back — a standalone Python tool (requires `astropy` and `pyarrow`, neither part of this
repository's own Fortran toolchain). Not part of the public Fortran library or its API, but
consumer-facing rather than maintainer/CI-only, so it's kept in the fpm-published package (see
its `KEEP_PATHS` entry in `tools/prep_fpm_publish.sh`). Only
int32/int64/float32/float64/boolean/string are ever written; unsigned integers/complex/bit-array
FITS columns are converted on a best-effort basis (widened, split into `_re`/`_im`, or unpacked
into a boolean vector column, respectively — see the script's own header comment for the full
mapping), and variable-length array columns (FITS `P`/`Q` descriptors) or genuinely
multi-dimensional per-row arrays (`TDIM` with more than one axis) are skipped with a warning and
listed in the output file's `not_converted_columns` metadata entry, rather than converted:

```bash
tools/convert_fits_to_parquet.py data.fits                                  # -> data.parquet, snappy
tools/convert_fits_to_parquet.py data.fits out.parquet --compression zstd
tools/convert_fits_to_parquet.py multi_table.fits out.parquet --hdu SPECTRA --overwrite
```

`tools/parquet_metadata_to_md.py` reads a `.parquet` file's metadata (via `pyarrow`, without loading
any column data) and writes a Markdown report: file-level info (format version, `created_by`,
row/column counts, a row-group count/size summary), this library's own VOTable-style header if
present (parsed and rendered human-readably, not just dumped as XML), every raw Parquet key-value
metadata entry (long/opaque values like `ARROW:schema` collapsed to a byte-count note), per-column
statistics aggregated across row groups (only for columns where they're actually populated), and a
merged column-definitions table (type/nesting rendered from the file's Arrow schema, so nested
`list`/`struct`/`map` columns show their real structure, plus compression codec(s)/encoding(s)
actually used and any `unit`/`ucd`/description this library's own metadata provides). General-purpose
— works on any Parquet file, not just this library's own output (a standalone Python tool, requires
`pyarrow`, not part of this repository's own Fortran toolchain). Not part of the public Fortran
library or its API, but consumer-facing rather than maintainer/CI-only, so it's kept in the
fpm-published package (see its `KEEP_PATHS` entry in `tools/prep_fpm_publish.sh`):

```bash
tools/parquet_metadata_to_md.py data.parquet                 # -> data.md, overwritten if it exists
tools/parquet_metadata_to_md.py data.parquet report.md
```

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp`/`openmp_write` test suites, plus the `concurrent_calls_into_shared_reader`/`writer` error scenarios) need OpenMP to actually be active to exercise concurrency:

```sh
fpm test
```

`fpm.toml`'s `openmp = "*"` dependency (fpm's built-in OpenMP metapackage) already supplies the compiler-appropriate flag (`-fopenmp` for gfortran, `-qopenmp` for ifx, ...) automatically — there is no need to pass one manually via `FPM_FFLAGS`. The two concurrency error-scenario tests are **self-adapting**: they check `omp_get_max_threads()` and, when it's `1` (OpenMP genuinely inactive, or `OMP_NUM_THREADS=1`), the shared-reader/writer race cannot occur, so they skip and pass trivially — so a green `fpm test` alone doesn't *prove* these specific concurrency checks ran for real; if you want to double-check that OpenMP is active in your environment, print `omp_get_max_threads()` from a small program or check the test suite's own thread-count assertions.

### Continuous integration (GitLab CI)

`.gitlab-ci.yml` runs the full `fpm test` suite (with OpenMP and coverage) on a GitLab Docker-executor runner. It builds the whole toolchain from scratch in the container's `before_script`, so it also serves as an executable, always-current recipe for building this project on a clean Debian/Ubuntu system.

A few choices in that file are load-bearing — each one cost a debugging round when it was wrong, so preserve them if you touch it:

- **Base image `ubuntu:24.04`** (pinned with `image:`, since the runner's own default image is older). 24.04 is the oldest Ubuntu that satisfies *every* toolchain requirement at once: gfortran 13 (gfortran ≤ 11 miscompiles the optional allocatable-character argument in `schema%add_col_qc` — see [README's Prerequisites](README.md#prerequisites)), a g++ new enough for C++20 / `std::span`, `pipx` in the repos (used to install `fpm`), and current Arrow apt packages. Its default `gcov` also matches its default compiler, so `gcovr` needs no `--gcov-executable` override.
- **`git lfs pull`** before running tests. The `test/fixtures/*.parquet` files are Git-LFS-tracked (see `.gitattributes`); without pulling them the reader tests read LFS *pointer* files and fail. `git lfs install --skip-repo` sets up only the global filter config (CI never pushes, so the repo-local hooks are deliberately skipped — installing them fails if the checkout already has one).
- **`libarrow-compute-dev`** installed alongside `libarrow-dev` / `libparquet-dev`: Arrow ships its compute kernels in a separate package, and `fpm.toml` links `arrow_compute` (see the [Troubleshooting guide](doc/pages/troubleshooting.md)). Omitting it fails the C++ compile on `arrow/compute/*.h`.
- **`FPM_FFLAGS="--coverage -fopenmp"`.** Source is kept within the standard 132-column free-form limit (see [Conventions](#conventions)), so no `-ffree-line-length-none` override is needed. `--coverage` genuinely must be passed explicitly here — gcov instrumentation isn't something any fpm metapackage supplies. The `-fopenmp` alongside it is no longer strictly necessary — `fpm.toml`'s `openmp = "*"` dependency already supplies it for the whole build regardless of whatever else `FPM_FFLAGS` is set to (confirmed: setting `FPM_FFLAGS` to unrelated flags, e.g. just an include path, does not disable the metapackage's own flag injection) — but it's harmless to keep explicit here too. `FPM_CXXFLAGS="-std=c++20 --coverage"` and `FPM_LDFLAGS="-lstdc++ --coverage"` follow [README's Environment variables](README.md#environment-variables), with `--coverage` added on both so `src/parquet_wrapper.cpp` is instrumented too, not just the Fortran sources.

Coverage is computed by `gcovr` over `src/` and surfaced through GitLab's `coverage:` regex — this now includes `src/parquet_wrapper.cpp`'s line coverage alongside every `src/*.f90` file, since this job's `gfortran`/`gcc`/`g++` all come from the same apt GCC install (one matched GNU toolchain), so the `.cpp`'s gcov data reads back cleanly in the same `gcovr` pass. **This is CI-only** — `tools/coverage.sh` deliberately does *not* attempt the same for `src/parquet_wrapper.cpp`, since it can't assume a matched toolchain on an arbitrary dev machine (e.g. a Mac with `gfortran` from one distribution and a default `clang++` `FPM_CXX` would produce gcov data in Clang's own format, which plain GNU `gcov`/`gcovr` doesn't reliably parse — confirmed to be exactly this project's setup on at least one contributor's machine). `tools/coverage.sh` still builds with `--coverage`, runs the suite plus every error scenario, and prints per-file and total `src/*.f90` coverage only (resolving the `gcov` that matches your `gfortran` automatically).

For local `src/parquet_wrapper.cpp` coverage on exactly this kind of mismatched-toolchain machine, use `tools/coverage_cpp.sh` instead — a separate script, not a flag on `tools/coverage.sh`, because the two can't be instrumented/collected in the same pass locally (Fortran's GNU gcov data and Clang's own gcov-compatible format need different collection code, and a `--coverage` build of one toolchain doesn't help report on the other). It auto-detects an Arrow install prefix (override with `ARROW_PREFIX`), builds `src/parquet_wrapper.cpp` with `--coverage` via `FPM_CXX` (`clang++` by default), and reports that one file's line coverage — via GNU `gcov -j` when the C++ compiler is GCC-like, or `xcrun llvm-cov gcov` when it's Clang:

```bash
tools/coverage_cpp.sh
```

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

`parquet_validate_maml` checks that section names and known sub-keys are registered in [src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90) (`allowed_maml_sections`). This checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks — if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed) `allowed_maml_nested_sections` in [src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly (print a diagnostic to stderr and call `std::abort()` — see `report_fatal_error`/`ConcurrencyGuard` in that file) over `throw`ing and `catch`ing within the same function. Testing found that a C++ exception thrown and caught within the very same function can still go uncaught when the final executable is linked by `gfortran` on macOS: `gfortran`'s driver passes `-no_compact_unwind` to the linker, which breaks libc++abi's stack unwinding for objects compiled by `clang++`, so `try`/`catch` is unreliable across this specific toolchain combination. A `throw` that's never meant to be caught (i.e. deliberately left to crash the process — most of this file's `throw std::runtime_error(...)` sites) is unaffected, since nothing downstream depends on unwinding actually working.

## Features considered but not implemented

These were looked at (during an audit comparing this library against Arrow C++'s broader feature set) and deliberately deferred rather than rejected outright — they're worth revisiting if a concrete use case actually needs them, rather than adding speculatively:

**Plausible future candidates, if needed:**
- Per-column writer properties (e.g. `disable_statistics()` for write-heavy/throwaway files, explicit dictionary-encoding toggles) — small, additive, doesn't touch the type system.
- `qc:`/`parquet_filter` support for the `date`/`time`/`timestamp` types (`parquet_date`/
  `parquet_time`/`parquet_timestamp`) — neither quality-control range/miss checks nor row filtering
  is implemented for these three yet, unlike every other supported type. An `INTERVAL`/duration
  type is a separate, deliberately dropped non-goal rather than a pending gap — see
  [Not yet supported](doc/pages/date-time.md#not-yet-supported) for why.

**Bigger lifts, worth being cautious about:**
- Predicate pushdown (statistics-based I/O skipping) — *not to be confused with row filtering, which is already implemented* (`parquet_filter` / `parquet_open_reader(..., filter=)`, see the [Row filtering section](doc/pages/reading.md#row-filtering-with-parquet_filter)). That existing filter is post-decode: it narrows the rows your code sees but still reads and decodes every referenced column in full. Genuine predicate pushdown — using per-row-group statistics (or Arrow's expression/compute-filter machinery) to skip reading matching row groups off disk entirely — is the unimplemented part, and the README's "No predicate pushdown" limitation treats it as an intentional non-goal for now.
- `MAP` columns, and variable-length `LIST` columns nested inside a `STRUCT` path (as opposed to
  a top-level list-encoded vector column, already supported — see
  [Reading a column into a different numeric kind](doc/pages/supported-data-types.md#reading-a-column-into-a-different-numeric-kind)'s
  neighboring sections) — Arrow supports these natively, but they'd break the library's core
  "flat columns + fixed `col_size` vectors" data model that the whole Fortran-side API is built
  around; this would be a redesign, not an addition. (`STRUCT` columns themselves are already
  supported for reading, at any nesting depth, down to a scalar or `FIXED_SIZE_LIST` leaf — see
  [Reading a nested struct field](doc/pages/supported-data-types.md#reading-a-nested-struct-field).)
- Additional scalar types on the **write** side (`int8`/`int16`/unsigned integers/`decimal` as a MAML-declarable/`parquet_write_column`-writable `data_type`) — straightforward from Arrow's side, but each new type multiplies the `parquet_write_*` interface surface (a dedicated subroutine pair per type already exists for each of the six supported types). Writing stays limited to the original six types; only the **read** side gained widening support for these (see below), since that side doesn't need new public entry points — it dispatches on the existing `values` array's Fortran kind, not a new declared type. A scoping pass mapped every remaining Arrow physical type against this library's six read targets (`int32`/`int64`/`real32`/`real64`/`boolean`/`string`) that was not yet supported for reading either:
  - `STRING_VIEW`→string: **implemented** (this library's own writer never produces it — it only
    arrives from a file written by another Arrow-based tool whose stored Arrow schema declared a
    column as `utf8_view()` — see [Reading `string_view` columns from other
    tools](doc/pages/supported-data-types.md#reading-string_view-columns-from-other-tools)).
  - Weak/unsafe: `BINARY`/`LARGE_BINARY`/`BINARY_VIEW`→string only if the bytes happen to be valid UTF-8.
  - Explicitly excluded from *this widening mechanism* (dispatch on `values`' own existing
    numeric/string kind): date/time/interval-family types (`DATE32`/`DATE64`/`TIMESTAMP`/
    `TIME32`/`TIME64`/`INTERVAL_*`/`DURATION` — semantically not raw numbers despite int32/int64
    storage) and nested/complex/wrapper types (`LIST`/`STRUCT`/`MAP`/unions/`DICTIONARY`/
    `FIXED_SIZE_BINARY`/etc. — no scalar target at all). `DATE32`/`DATE64`/`TIME32`/`TIME64`/
    `TIMESTAMP` are no longer unsupported overall, though — see the "Implemented since this list
    was last reviewed" bullet above: they now read/write via their own dedicated types instead of
    widening into an existing one. `INTERVAL_*`/`DURATION` remain a deliberate non-goal rather
    than a pending gap (see that bullet's own note).

**Probably out of scope for this library's design:**
- Arrow's `dataset` module (multi-file/partitioned scanning), encryption, and Flight — these serve a different usage pattern (distributed/partitioned datasets, secure transport) than this library's "one file, one reader/writer" design, and adding them would cut against the intentional minimalism this codebase aims for.
