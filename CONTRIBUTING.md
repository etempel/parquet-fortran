# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to *use* the library from your own project, see [README.md](README.md) instead.

## Contents

- [AI assistance](#ai-assistance)
- [Conventions](#conventions)
- [Building and testing this repository](#building-and-testing-this-repository)
  - [Building with link-time optimisation](#building-with-link-time-optimisation)
  - [Running a single test suite/test](#running-a-single-test-suitetest)
  - [Running the error-path tests](#running-the-error-path-tests)
  - [Regenerating the test fixtures](#regenerating-the-test-fixtures)
  - [Other tools/ helpers](#other-tools-helpers)
  - [Testing genuine OpenMP concurrency](#testing-genuine-openmp-concurrency)
  - [Continuous integration (GitLab CI)](#continuous-integration-gitlab-ci)
  - [Mirroring to GitHub](#mirroring-to-github)
  - [Releasing](#releasing)
  - [Publishing to the fpm registry](#publishing-to-the-fpm-registry)
- [Regenerating the built-in MAML module](#regenerating-the-built-in-maml-module)
- [Extending the MAML schema](#extending-the-maml-schema)
- [Error-handling conventions in `parquet_wrapper.cpp`](#error-handling-conventions-in-parquet_wrappercpp)
- [Features considered but not implemented](#features-considered-but-not-implemented)

## AI assistance

Portions of this codebase, including its documentation, were developed with AI assistance (Claude Code).

## Conventions

Project conventions worth knowing before contributing (all are applied in day-to-day development and enforced in review):

**Naming.** Public module-level API — everything in `src/parquet_core.f90`'s `public ::` list, re-exported by the `parquet` facade in `src/parquet.f90` — carries the `parquet_` prefix (e.g. `parquet_open_reader`, `parquet_get_metadata`). Type-bound procedures (`schema%init`, `reader%...`) are namespaced by their type and do not. The `maml_` prefix is reserved for MAML-parsing/building internal helpers. When in doubt, grep for an existing analogous name before inventing a new one.

**Line length.** Every line in `src/*.f90` and `test/*.f90` — code and comments alike, including trailing end-of-line comments — must stay at or under 132 columns, the standard Fortran free-form limit. Wrap long expressions/strings with `&` continuations and long comments across multiple `!`-prefixed lines rather than letting a line run past 132 columns; don't reach for a compiler flag to paper over it (see `.gitlab-ci.yml`'s `FPM_FFLAGS`, which no longer passes `-ffree-line-length-none`).

**New features need tests and docs.** A new feature should land together with (1) unit-test coverage in the relevant `test/*.f90` suite — plus error-path coverage via `test/error_scenarios.f90` + `test/test_errors.f90` + `tools/run_error_scenarios.sh` if it has failure modes that `error stop` — and (2) documentation updates: a `!>`/`!!` doc-comment on the new public API (picked up automatically by the FORD-generated reference — build it locally with `ford docs.md`, or see whichever published copy your checkout links to: `https://www.4most.eu/readthedocs/etempel/parquet-fortran/main` on canonical GitLab, `https://etempel.github.io/parquet-fortran/` on the GitHub mirror), the relevant [user guide page](doc/pages/index.md) for any new behavior/how-to, the [README](README.md) if the landing-page story changes, and this file if it affects contributor workflow. **A `CHANGELOG.md` entry is part of that too, and has been since the 1.0.0 release** (an earlier version of this paragraph said updates were paused pre-1.0 — that is no longer true). Every new feature gets a bullet under `[Unreleased]`'s `### Added`. `### Changed` and `### Fixed` are deliberately narrower: they record only what differs from the **released** version, so a change or a fix to a feature that itself landed *after* 1.0.0 gets no entry of its own — there is no released behaviour for it to differ from — and whatever the reader needs to know is folded into that feature's own `### Added` bullet instead. See [CLAUDE.md](CLAUDE.md#new-features-require-tests-and-docs) for the rest of the rules.

**Documentation describes the current state, not a former one.** When you change behaviour and update the [user guide](doc/pages/index.md), README.md or a doc-comment to match, write what the library does — not what changed. *"This used to be refused outright; that is no longer necessary"* and *"the arithmetic this bullet used to mention as missing — that is now implemented"* are both true, both were written by careful edits, and both leave a reader holding a sentence they cannot evaluate, because they never saw the version being compared against. The change itself belongs in `CHANGELOG.md` and in the commit message, which is where a reader goes looking for it. This applies to user-facing documentation; comments in `src/` are exempt, since their reader is a maintainer for whom "this was consolidated into one place" is useful context. See [CLAUDE.md](CLAUDE.md#a-guide-page-describes-the-current-state-never-a-former-one) for the three shapes this takes, the cases that are *not* in the class, and why greping for it is easy but sweeping it mechanically is not.

**Read `feature_risks.md` before editing an area it covers.** It is this repository's standing-risks register: thirty numbered entries (`Risk-1` … `Risk-30`) recording properties of the shipped code that a future change can break with **no test failing and no abort** — a wrong answer, a stale pointer, a corrupted heap, a silently skipped row group — each with whether a test would catch it today and, where none can, how to check or avoid it instead. It is organised into four sections (new / proposed test / not testable / covered-but-still-forbidding-something), and a risk keeps its number when it moves between them, so `Risk-13` means the same thing in a code comment a year from now. If you implement a proposed test, update its entry in the same change; if you find a new silent-failure property while fixing a bug, add it as a new entry rather than only writing a code comment.

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

`fpm.toml` sets `auto-executables = true`, so **eleven** executables are built from `app/*.f90` and placed in `my_path/bin`:

| Executable | Source | Purpose |
|---|---|---|
| `run_parquet_fortran` | `app/program.f90` | Prints the parquet-fortran library version number — a quick sanity check that a build/install actually picked up the version you expect. The only one of the eleven that ships in the fpm-published package (see `tools/prep_fpm_publish.sh`) — the other ten are maintainer/CI-only dev tools kept in this repository but stripped from what a consumer installs. |
| `benchmark_threads` | `app/benchmark_threads.f90` | Driven by `tools/benchmark_threads.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_table` | `app/benchmark_table.f90` | Driven by `tools/benchmark_table.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_colindex` | `app/benchmark_colindex.f90` | Driven by `tools/benchmark_colindex.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_stage7` | `app/benchmark_stage7.f90` | Driven by `tools/benchmark_stage7.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_sort_engine` | `app/benchmark_sort_engine.f90` | Driven by `tools/benchmark_sort_engine.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_sort_comparator` | `app/benchmark_sort_comparator.f90` | Driven by `tools/benchmark_sort_comparator.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `probe_isnan` | `app/probe_isnan.f90` | Driven by `tools/probe_isnan.sh`. A **temporary diagnostic** for the Stage 1e comparator campaign — what one NaN test costs, per toolchain. Delete both once that question is answered. |
| `benchmark_strings` | `app/benchmark_strings.f90` | Driven by `tools/benchmark_strings.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `benchmark_string_threads` | `app/benchmark_string_threads.f90` | Driven by `tools/benchmark_strings.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `test_large_scale` | `app/test_large_scale.f90` | Driven by `tools/test_large_scale.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `test_random_large_fill` | `app/test_random_large_fill.f90` | Driven by `tools/test_random_large_fill.sh` — see [Other tools/ helpers](#other-tools-helpers). |
| `check_arrow_release` | `app/check_arrow_release.f90` | Driven by `tools/check_arrow_release.sh` — see [Other tools/ helpers](#other-tools-helpers). |
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

### Building with link-time optimisation

**No fpm profile enables it.** `--profile release` supplies
`-O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single` and nothing more, so
`-flto` (gfortran/flang) or `-ipo` (ifx) has to be added by hand. fpm *appends* `FPM_FFLAGS` and
friends to the profile flags rather than replacing them, so adding it is straightforward:

```bash
FPM_FFLAGS="${FPM_FFLAGS:-} -flto" FPM_CXXFLAGS="${FPM_CXXFLAGS:-} -flto" \
FPM_LDFLAGS="${FPM_LDFLAGS:-} -flto" fpm install --profile release
```

Note the `${VAR:-}` prefixes: a bare assignment would discard the Arrow include and link paths those
variables normally carry.

**Two companion tools matter as much as the flag itself, and getting either wrong is silent.**

- **A plugin-capable ARCHIVER.** fpm archives the static library with plain `ar`; under LTO an object
  holds intermediate representation, and an archiver that cannot read it indexes only the machine-code
  half — so the build **succeeds, passes its tests, and does no interprocedural optimisation at all**.
  There is no error and no warning. Set `FPM_AR=gcc-ar` (or `gcc-ar-mp-<N>` under MacPorts) for
  GCC, `FPM_AR=llvm-ar` for flang/Intel. On Linux, binutils `ar` usually loads the plugin itself; on
  macOS the default `ar` is Apple cctools `ar`, which cannot — so a macOS GCC build **needs** this.
  The tell is the archive size: 14.2 MB under `-flto` against 5.2 MB plain, still full of ordinary
  text symbols under `nm`, is a disabled LTO build.
- **On ifx, the LINKER, and this one fails loudly rather than silently.** `-ipo` emits LLVM bitcode
  that the system `ld`'s plugin generally cannot read, giving **thousands of undefined
  `<module>_mp_<proc>_` references** — which reads as a defect in this library and is not. oneAPI
  ships a matching `ld.lld`, but it sits in `<oneapi>/compiler/<ver>/bin/**compiler**/`, which the
  usual environment scripts do **not** put on `PATH`. Add that directory and pass `-fuse-ld=lld`.

**Do not go looking for `xiar`: it does not exist in oneAPI 2026.1**, having been retired in favour of
`llvm-ar`. Advice to source `setvars.sh` until it appears cannot be followed. It is also not the
load-bearing piece on Intel — `-ipo` with plain `ar` was measured doing real interprocedural
optimisation, once `ld.lld` was in play.

**Exporting those variables permanently affects EVERY build, including `--profile debug`** — fpm
applies them whatever the profile (and even with no profile at all). That is rarely wanted: LTO makes
a debug build slow to link and inlines across translation units, which is exactly what a
`-fcheck=bounds` backtrace needs left alone.

**`tools/fpm_lto.sh` handles all of the above.** It is a shell function that wraps `fpm`, adds the
flags only when it sees `--profile release`, appends to rather than replaces the `FPM_*FLAGS`, picks
the right archiver for the compiler family, puts oneAPI's `ld.lld` on `PATH` when it is needed, and
**refuses to build** rather than hand back a silently LTO-less measurement. Activate it by sourcing
it from your shell startup file (`~/.zprofile`, `~/.bashrc`, ...):

```bash
source /path/to/parquet-fortran/tools/fpm_lto.sh
export PF_LTO=1        # opt in; omit this line and the wrapper is inert
```

Run it rather than sourcing it to see the activation hint and a dry run of what it would select for
the toolchain in hand:

```bash
tools/fpm_lto.sh
```

Sourcing without `PF_LTO=1` changes nothing, so it can live in a startup file permanently and be
enabled per session with `PF_LTO=1`. `PF_LTO=0 fpm build --profile release` turns it off for one
command. The script's own header carries the full rationale; read it before changing it.

**On macOS the gain may be zero however carefully this is set up, and that is structural rather than
a misconfiguration.** The normal macOS build is mixed-family (gfortran + Apple clang), so the Fortran
objects carry GCC GIMPLE while the C++ object is LLVM bitcode — two IRs no linker can optimise
across. Measured on an arm64 machine with `gcc-ar` genuinely in effect and a 3% noise floor: **no
change on any item.** A same-family toolchain (`gfortran` + `g++`, or `ifx` + `icpx`) is where LTO has
actually been observed to help here, and even there the only clear gain was on one operation.

Verified under both zsh and bash 3.2 (macOS's default), by driving it against a stub `fpm` on `PATH`
and reading the environment it passes through — ten cases across the two shells: `--profile release`
and `--profile=release` get the flag **and** `FPM_AR`; `--profile debug`, an unset `PF_LTO` and
`PF_LTO=0` get neither; and a machine with no plugin-capable archiver gets a refusal and exit 1
rather than a silently LTO-less build. `env` is what runs the real binary, so the function cannot
recurse.

Testing it in both shells was not a formality: an earlier draft collected the archiver candidates in
a single space-separated variable and looped over it unquoted, which works in bash and **silently
finds nothing in zsh** — that shell does not word-split an unquoted parameter expansion — so it
refused to build on a machine that had the archiver installed. Hence `pf_first_on_path`, which takes
the candidates as separate arguments.

**One consequence worth knowing before installing that wrapper.** Any script that builds a
release-profile baseline to compare an LTO build against — `tools/benchmark_stage7.sh` is the one in
this repository — would have its *baseline* silently become an LTO build too, and the two columns
would agree for a reason that has nothing to do with LTO. That script therefore refuses to start if
it finds an LTO flag already in the environment; set `PF_LTO=0` for such a run.

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

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, etc.) are reported via Fortran's `error stop`, which aborts the whole process — see the [Error handling guide](doc/pages/operating/error-handling.md). Since test-drive assertions can't survive an `error stop` in the same process, these paths are exercised out-of-process by a small helper program, `test/error_scenarios.f90`, which is built as its own `fpm` test target named `error_scenarios`.

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

This prints a `[PASS]`/`[FAIL]` line per scenario and exits nonzero if any scenario's exit code didn't match what was expected — useful for a quick manual check or a CI step that doesn't need the full `fpm test` output. Keep this script's scenario list in sync with `test/error_scenarios.f90`'s `select case` — it's meant to be a complete mirror, not a curated subset, and `tools/check_source_conventions.py` now fails when it isn't.

**That list is load-bearing for `fpm test`'s speed too.** Before any suite runs, `run_tester` calls `prime_error_scenarios` (`test/test_errors.f90`), which reads this same array and runs every scenario in it once, in parallel, capturing each one's exit status and its two output streams under `test_run/.primed/`; the tests then read those results instead of spawning ~630 subprocesses one at a time. That is the difference between a ~72 s and a ~25 s `fpm test`. A scenario missing from the array still works — it simply falls back to being spawned on demand — so forgetting it costs speed, not coverage. Set `PARQUET_TEST_NO_PRIME=1` to disable priming (handy when debugging a single scenario), or `PARQUET_TEST_PRIME_JOBS=N` to change the concurrency; running a single named test (`fpm test run_tester -- errors "<test name>"`) never primes.

**Every scenario runs under a wall-clock cap**, in both the priming path and `tools/run_error_scenarios.sh`: 120 s by default, `PARQUET_SCENARIO_TIMEOUT=<seconds>` to change it. A scenario that trips the cap is killed and reported as an explicit `TIMED OUT` failure rather than being waited on — and, just as importantly, rather than being counted as a successful abort, which is what a bare "exited nonzero" check would do with it. The cap exists because a scenario that provokes the concurrency guard from many threads was once observed hanging inside `abort()` and stalling a whole `--profile debug` run indefinitely; the library-side fix for that is in `src/parquet_wrapper.cpp`'s fatal path, and this is the harness-side backstop for any future hang from any cause. Stock macOS ships no `timeout` — coreutils installs it as `gtimeout`, which both paths look for — and with neither present the scenarios simply run uncapped rather than failing.

After one `fpm build --tests`, the script runs scenarios directly against the built `error_scenarios` binary (not through a separate `fpm test error_scenarios -- ...` per scenario) and dispatches them across several scenarios at once via `xargs -P` — each scenario is an independent process writing to its own uniquely-named file under `test_run/`, so this is safe. Defaults to one worker per logical CPU; override with `RUN_ERROR_SCENARIOS_JOBS=N tools/run_error_scenarios.sh` if that's too aggressive for a given machine. The two `concurrent_calls_into_shared_*` scenarios are the exception — they specifically probe OpenMP thread contention, so they still run sequentially, after the parallel batch.

### Regenerating the test fixtures

A handful of tests read pre-built Parquet files committed under `test/fixtures/` rather than files this library writes itself. These deliberately contain shapes this library's own writer *cannot* produce — genuine Arrow validity-bitmap Nulls, per-element (rather than per-row) Nulls in a vector column, an unsupported physical column type, a list-encoded (per-row array) vector column, extended read-only source types, and a `STRUCT` column — so that the reader's handling of them can be exercised. They are built directly against the Arrow/Parquet C++ API by `tools/generate_fixtures.cpp` (one function per fixture):

- `has_null.parquet` — columns with real Nulls (not sentinel values), read by the `errors`/`reading` suites.
- `unsupported_type.parquet` — a column of a physical type this library refuses to read.
- `list_vector.parquet` — a vector column stored as Parquet `LIST` rather than the fixed-size layout this library writes.
- `no_stats.parquet` — a Null-carrying column written with column statistics **disabled**, the one case `parquet_column_has_nulls` cannot answer from the footer. Proves the conservative fallback (request the validity mask anyway) rather than trusting an absent statistic. Unreachable with any other fixture, since every other one carries statistics.
- `list_widths.parquet` — one column per shape a variable-length `LIST` column can take (uniform, ragged, ragged-but-with-a-whole-number mean, uniform-except-in-the-last-row-group, containing a null row, containing an empty row, a `LIST` leaf under a `STRUCT`, plus a scalar control), written across 4 row groups. Drives the deferred-width tests in the `table` and `reading` suites and the `list_width_never_reads_whole_column` error scenario. Written **without** `store_schema()` on purpose — with it, Arrow round-trips a `fixed_size_list` as a `fixed_size_list` and none of these columns would exercise the plain-`LIST` path.
- `extended_types.parquet` — columns of the extended read-only source types (`int8`/`int16`/unsigned integers/`half_float`/`decimal`).
- `nested_struct.parquet` — a `STRUCT` column, nested 3 levels deep, with a `FIXED_SIZE_LIST` vector-column leaf in a sibling `STRUCT` column, and rows covering every independent null-combination source (see [Reading a nested struct field](doc/pages/types/supported-data-types.md#reading-a-nested-struct-field)).
- `element_nulls.parquet` — vector (`FIXED_SIZE_LIST`) columns whose nulls sit on individual **elements** rather than on whole rows, one column per validity dispatch class (`float64` bitmap, `string`, `timestamp`), each with exactly one null element. Read by the `table` suite. It has to come from here rather than from this library's own writer: every other element-null test writes its fixture with `parquet_write_column` and reads it back, so a defect that widened a null on read *and* broadcast it on write would agree with itself and look healthy.
- `map_list_types.parquet` — exercises Arrow's `MAP` and (variable-length) `LIST` types, generated by `generate_map_list_types_fixture` in `tools/generate_fixtures.cpp`. Currently **reserved/unused**: no Fortran test reads it, since `MAP` columns and struct-nested variable-length `LIST` columns remain unsupported (see [Features considered but not implemented](#features-considered-but-not-implemented)) — kept as groundwork for if that support is ever added.

Because these files are committed, a normal `fpm test` never needs to regenerate them. Rebuild them only when you change `generate_fixtures.cpp` or otherwise need a fixture recreated, via:

```bash
tools/run_generate_fixtures.sh
```

This compiles `generate_fixtures.cpp` with `clang++` and runs it from the repository root, rewriting every fixture under `test/fixtures/`. It needs the same `FPM_CXXFLAGS`/`FPM_LDFLAGS` (Arrow/Parquet include/link flags) used to build the project itself — see [README.md's Environment variables section](README.md#environment-variables); the script errors out early if they are unset.

The `clang++` invocation is unconditional, with no `FPM_CXX`/`CXX` override — so fixture regeneration is not possible as-is on a GCC-only Linux box (the same platform this project's own CI uses) without editing the script first.

### Other tools/ helpers

A few more `tools/` scripts, unrelated to fixtures and not part of the build or test flow:

`tools/run_lint_check.sh` runs the same checks as `.gitlab-ci.yml`'s `lint` stage, locally — `tools/check_bindc_boundary.py`, `tools/check_doc_anchors.py`, `tools/check_source_conventions.py`, the four generated-file `--check` calls (`generate_parquet_columns.py`, `generate_parquet_tables.py`, `generate_parquet_sorting.py`, `generate_parquet_maml.sh base`), `generate_user_table_code.py`'s own `--self-test` plus the `--check` for this project's committed generated table type, and `generate_random_golden_vectors.py`'s `--self-test` and `--check`. It needs nothing but `python3` and `bash` — no fpm, no gfortran, no Arrow — and finishes in well under a second, so it is worth running before every push:

```bash
tools/run_lint_check.sh              # run every check, then list any that failed
tools/run_lint_check.sh --fail-fast  # stop at the first failure, like CI does
```

By default it runs every check even after one fails and lists the failures together at the end, which differs deliberately from CI (whose `script:` stops at the first nonzero command): locally it is more useful to see every problem in one pass. The verdict is the same either way — it exits nonzero if anything failed. **Its `CHECKS` list is kept in sync with the CI job by hand**, so a check added to one must be added to the other; nothing enforces it. Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).

`tools/check_random_kernels.sh` standalone-compiles `src/parquet_random.f90` across six optimisation settings **including LTO**, and checks the golden vectors and the strict reference in each. It exists because two things this project relies on are otherwise untested: nothing else builds anything with `-flto`/`-ipo` (fpm's release profile is `-O3 -funroll-loops`), and the `#else` arm of that module's route (e) fork — the wrapping-arithmetic kernel that ships wherever the compiler has no 128-bit integer kind, i.e. ifx — is compiled by no other check at all, because every other compiler in the fleet takes the protected arm. By default it builds **both** kernels, forcing the second with `-U__GFORTRAN__` (or `-U__flang__`); it asserts that the two builds really did differ, so that a `-U` flag which quietly stopped working cannot leave it checking one kernel twice and reporting green. It follows `FPM_FC` when `FC` is unset, and refuses a gfortran below the project's floor of 13 rather than reporting that compiler's own miscompilations as findings. This cannot be an `fpm test` arm: `-U__GFORTRAN__` also flips `src/parquet.f90`'s stringify branch, so the package will not compile with it, and a standalone compile of the one module is the only form that works.

```bash
tools/check_random_kernels.sh                 # both kernels, six settings each
tools/check_random_kernels.sh --quick         # only the LTO/IPO setting
tools/check_random_kernels.sh --shipped-only  # only the kernel this compiler ships (what CI runs)
FC=ifx tools/check_random_kernels.sh          # ifx ships the wrapping kernel; nothing to force
```

**The forced half currently fails on gfortran under LTO, and that is expected** — see [`feature_risks.md`](feature_risks.md) Risk-101. It is a gfortran bug affecting a kernel gfortran never ships (gfortran takes the protected arm and is clean; ifx ships the wrapping arm and is clean at every setting measured, including `-ipo`), so CI runs `--shipped-only` rather than going permanently red on it. Maintainer-only (stripped from the fpm-published package).

`tools/generate_parquet_tables.py` regenerates the per-kind blocks of the `parquet_tables` table layer: `src/parquet_tables.f90` (the module spec), `src/parquet_tables_access.f90`, `src/parquet_tables_addcol.f90` and `src/parquet_tables_materialize.f90`. It **imports its kind table from `tools/generate_parquet_columns.py`** rather than keeping a second copy, so a new column kind is declared in exactly one place and the two layers cannot drift apart. Output is committed and `--check` verifies it, exactly as for the columns generator. The other four `parquet_tables_*.f90` files (`lifecycle`, `query`, `read`, `write`) are hand-written and the script never touches them. Maintainer-only (stripped from the fpm-published package).

`tools/generate_user_table_code.py` turns a Role-A MAML (one under `table_types/`) into a named `parquet_table` extension type with one accessor per declared column — see [Generated table types](doc/pages/utilities/generated-tables.md) for the user-facing story. This project's own `table_types/maml_example4.maml` generates `src/parquet_table_example.f90` (module `parquet_table_example`, type `parquet_table_test`), which **ships with the library as a worked example** — nothing else in the library uses it — and is exercised by the `table_codegen` test suite; regenerate it with `tools/generate_user_table_code.py` and commit the result. Unlike the two generators below it is **consumer-facing** (like `tools/generate_parquet_maml.sh`) and ships in the fpm-published package, so a downstream project can copy it. `--self-test` runs its own tests, including a cross-check that its baked-in list of reserved `parquet_table` binding names still matches `src/parquet_tables.f90` — so adding a type-bound procedure to `parquet_table` fails the lint stage until that list is updated.

`tools/generate_parquet_sorting.py` regenerates the per-type blocks of the `parquet_sorting` module: `src/parquet_sorting.f90` (the module spec), `src/parquet_sorting_keys.f90`, `src/parquet_sorting_argsort.f90` and `src/parquet_sorting_permute.f90` — roughly 75 near-identical procedures, since four operations over eleven element types in two permutation-index kinds is the same handful of statements repeated. Like the tables generator it **imports its kind table from `tools/generate_parquet_columns.py`** (the nine SCALAR rows) and declares locally only the three types that are not `parquet_column` storage kinds at all: `character(len=*)`, `parquet_string_column` and `parquet_column` itself. `--self-test` cross-checks the imported set, so a change over there fails here rather than silently changing what this module sorts; `--check` verifies the committed output as usual. Maintainer-only (stripped from the fpm-published package).

`tools/generate_random_golden_vectors.py` regenerates `test/test_random_vectors.f90` — the golden vectors that freeze `parquet_random`'s bit contract (see [Random numbers](doc/pages/utilities/random.md)). It is the odd one out among the generators: its output is *test data* rather than library source, and it is derived from an arbitrary-precision Python model of the contract rather than from any Fortran run, because a table read back out of the implementation could only ever confirm that the implementation agrees with itself. `--self-test` re-derives every published anchor — the three Random123 known-answer vectors, the `mix64` checksum, the wide-width oracle value and the stable rows of the reference tables — and refuses to regenerate anything if one of them fails; that is what makes the emitted table evidence rather than mere self-consistency, so run it before trusting a regeneration. `--check` verifies the committed output as usual. **Never hand-edit a vector**: a contract change is an edit to the oracle in the script plus a regeneration, and every value it emits is permanent (changing one is a major-version event, visible only through `pf_random_algorithm`). Maintainer-only (stripped from the fpm-published package).

`tools/generate_parquet_columns.py` regenerates the per-kind blocks of the `parquet_columns` foundation module: `src/parquet_columns.f90` (the module spec), `src/parquet_columns_access.f90` and `src/parquet_columns_mutate.f90`. Its output is **committed**, exactly like `tools/generate_parquet_maml.sh`'s, so nothing is generated at build time and the fpm build stays dependency-free. Re-run it after editing the kind table at the top of the script — for example when a new column kind is added — and commit the regenerated files; `tools/generate_parquet_columns.py --check` re-derives the output and fails if the committed files have drifted, which is the cheap way to catch a forgotten regeneration. The other four `parquet_columns_*.f90` files (`util`, `validity`, `structural`, `string`) are hand-written and the script never touches them. Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).

It also takes **`--bench-guards`**, which is for measurement branches only and never for `main`: it wraps `get_at`/`set_at`'s two guard calls in cpp `#ifdef`s so one source tree can be built three ways — as shipped, with the comparisons inlined (`-DPF_BENCH_INLINE_GUARDS`, every check still performed, only the two cross-submodule calls gone), or with the guards removed (`-DPF_BENCH_NO_GUARDS`). It exists because that measurement **cannot be replicated inside a benchmark program**: the question is what a call across a program-unit boundary costs, and a local copy in one file would be inlined, measuring the opposite. Without the flag the generator emits byte-identical output to what is committed, so `--check` and CI are unaffected; restore with `git checkout src/parquet_columns_access.f90`. `tools/benchmark_colindex.sh` refuses to run its non-default `GUARDS` values against an unscaffolded tree rather than silently building the shipped binary under another name.

`tools/count_lines.py` reports code/comment/blank line counts, a convenience for repository metrics. With no arguments it prints four independent summaries — `src/` (the library), `test/` (the test suite), `app/` (the manual programs) and `tools/` (this tooling) — each totalled on its own so no group inflates another, followed by a cross-group table repeating the four totals and adding the repository-wide one (the single number the independent summaries deliberately withhold). Pass explicit files or directories for a single summary over just those, with no cross-group table. It understands Fortran, C++ and Python/shell comment syntax, with one deliberate simplification: only `#` marks a comment in a script, so a Python docstring counts as code (treating triple-quoted strings as comments would misreport the generators here, whose emitted Fortran lives in exactly such strings).

`tools/count_tests.sh` counts test-drive unit tests per suite directly from source (no build or run required): it reads `test/run_tester.f90`'s `new_testsuite(...)` registrations, locates each suite's `collect_tests_parquet_*` subroutine, and counts the `new_unittest(...)` entries inside it — cross-checked against an actual `fpm test run_tester` run's PASSED/FAILED line count. Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).

`tools/benchmark_threads.sh` measures how write and read throughput scale with Arrow's internal
thread-pool size (`parquet_set_arrow_threads`), sweeping a log-spaced set of thread counts and
driving `app/benchmark_threads.f90` (a maintainer-only fpm executable, not part of the public
library) once per (mode, thread-count) data point. The synthetic file's schema is 5 scalar
columns (int32/int64/float32/float64/boolean), replicated `NMULT` times (`i32_1`, `i32_2`, ...,
`i64_1`, ...) so the file has `5*NMULT` columns — threading benefits more from many columns than
from few. `MAX_STEPS`, `TARGET_FILE_SIZE_GB`, `NMULT` and `TEST_FILE` are its own env-overridable
config; by default the synthetic test file is written under a fresh `mktemp -d` directory and
deleted when the script exits — set `TEST_FILE` to give it a path of your own choosing instead,
which also keeps the file around afterward for inspection or reuse:

```bash
# Defaults: MAX_STEPS thread-count steps, a small TARGET_FILE_SIZE_GB, in a fresh mktemp -d file:
tools/benchmark_threads.sh
# Fewer thread-count steps, against a larger synthetic file:
MAX_STEPS=6 TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
# More replicated columns (5*NMULT total), against a larger synthetic file:
NMULT=20 TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
# Keep the synthetic file at a path of your own choosing instead of a temp dir that gets deleted:
TEST_FILE=/tmp/benchmark.parquet TARGET_FILE_SIZE_GB=4.0 tools/benchmark_threads.sh
```

`tools/benchmark_colindex.sh` answers one question: where does `parquet_table%get_element`'s
per-cell cost actually go, and is an index- or handle-based accessor worth building? It drives
`app/benchmark_colindex.f90` over seven modes (`baseline`, `decompose`, `getat`, `vector`, `loop`,
`rowfinal`, `handle`) and is the screening half of the campaign described in
`feature_benchmark_colindex.md`.

**`--mode=handle` is the one that measures the SHIPPED feature** rather than screening for it: it
times `parquet_table_col` against `%get_element` on the same column, including the regression
control (`%get_element` became a caller of the handle's own body, so it must not have got slower),
the cost of re-fetching a handle per cell, the two creation forms, the staleness guard, a realistic
4-column loop both ways, and reading one element of a vector row against materialising the whole
row. Its `parquet_column%get_at` arm is untouched by anything the campaign varies and so doubles as
a control across builds and commits.

**Its central trick is that it needs no change to the library**, which is what makes it runnable
before anything is prototyped. It builds a **standalone `parquet_column`** and times
`col%get_at(i, v)` on it — the real library procedure across the real module boundary, not a
replica — then reads the two differences that matter: `%get_element` minus `%get_at` is everything
the table layer adds, and so an **upper bound** on what any index or handle could ever remove;
`%get_at` minus a `%col` pointer read is the whole cost of the two guard calls inside the column
layer. A small first difference kills the feature before a line of it is written.

Two things it does that are worth copying into any future accessor benchmark. It **reproduces two
independently measured anchors** (a `%col` pointer read at ~0.93 ns and `%get_element` at ~24 ns
on a 4-column table) before any other row is quoted, because a benchmark that replicates library
call shapes is untested code until one of its rows matches a figure measured elsewhere. And its
vector mode isolates the per-call `allocate` by running the **same** `%get_at` twice, once into a
reused buffer and once into a freshly allocated one — the first version of that arm wrapped
`allocate`/`deallocate` around a couple of local stores, which gfortran elided outright, and it
reported exactly the pointer-read figure while appearing to measure something.

```sh
tools/benchmark_colindex.sh                       # every mode
tools/benchmark_colindex.sh --mode=decompose      # just the central one
NROWS=200000 NCOLS=128 ROUNDS=7 tools/benchmark_colindex.sh --mode=baseline
```

`LADDER=<PF_BENCH_NO_*>` selects a stage-0b **compile-out ladder** rung — one phase of
`table_resolve` removed, so `%get_element` can be timed with it gone and the difference attributed
to that phase. It needs `tools/bench_resolve_ladder.py --apply` first (measurement branches only;
`--list` prints the rungs, `--check` reports whether a tree is scaffolded, and there is no
`--revert` because `git checkout src/parquet_tables_query.f90` already does it exactly). The wrapper
refuses `LADDER=` against an unscaffolded tree, and the program prints the rung it was compiled as.
**Two rungs deliberately change answers** — `NO_LOOKUP` resolves a slot by hashing one byte of the
name and `NO_ROWINDEX_CMP` drops the automatic row-index column — so checksums are not comparable
across rungs, only timings are, and nothing from a scaffolded tree is ever committed. The ladder
exists because phase timers cannot be used on a per-element path: the whole call is ~37 ns and a
`steady_clock::now()` pair costs 20–25 ns.

`GUARDS=shipped|inline|none` selects which variant of `parquet_column%get_at`'s guards is compiled
in, which is how the **call** is measured apart from the **check**. The two non-default values
require the source to have been regenerated with `tools/generate_parquet_columns.py --bench-guards`
first — a measurement-branch-only step that wraps those guards in cpp `#ifdef`s — and the script
**fails rather than degrading** when that scaffolding is absent, since building the shipped binary
under another name would report a null result that looks like a finding. Build trees go to
`test_run/colindex-<guards>-<compiler>/`, with the compiler in the name deliberately: naming a tree
for the configuration alone lets a second toolchain's binary land in the first one's directory.
Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).

`tools/benchmark_stage7.sh` runs a set of targeted micro-measurements over paths this library's
optimisation work needs numbers for, and — this is why it exists rather than being a throwaway
script — prints the **toolchain provenance** ahead of them: compiler and version, Arrow version,
core count, git commit, and whether the working tree was dirty. A performance number from this
project is only meaningful with that attached, because the same change has measured 1.84x under one
toolchain and parity under another (see CLAUDE.md's "The three machines available for testing").

It drives `app/benchmark_stage7.f90`, which covers several independent items selectable with
`--only=`; run with no selection it measures all of them. The one worth knowing about on its own is
**`s7-2`**, which times the write-side numeric range-check-and-convert loops in three shapes — the
fused loop the library has today, and two split forms that separate the check from the conversion so
both halves can vectorise — across six sizes spanning the cache hierarchy. It answers whether
splitting that loop would pay on a given target, which is a property of the compiler and the vector
width rather than of this project, and therefore has to be measured per machine rather than reasoned
about.

```bash
# everything, on the current build
tools/benchmark_stage7.sh
# one item only
tools/benchmark_stage7.sh --only=s7-2
# with and without link-time optimisation, one after the other (-ipo for ifx, -flto otherwise)
tools/benchmark_stage7.sh --both
# ...and additionally run the whole test suite under each build configuration
tools/benchmark_stage7.sh --both --test
```

`--test` is slow but is not optional when measuring LTO: a faster build that fails the suite is not
a result, and this project has four documented ifx codegen bugs that appear only at `-O1` and above,
which an interprocedural build makes more rather than less likely to surface.

**Link-time optimisation is not part of any fpm profile.** `--profile release` supplies
`-O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single` and nothing more, so
`-flto` (or `-ipo` on ifx) has to be added explicitly — the script appends it to the three
`FPM_*FLAGS` variables, which fpm adds to the profile flags rather than replacing them. On ifx it
additionally sets `FPM_AR=xiar`: fpm archives the static library with plain `ar`, and under `-ipo`
that produces an archive the linker cannot optimise across, so the build either fails or silently
does no interprocedural optimisation at all.

`FPM_FC`/`FPM_CXX`/`FPM_*FLAGS` are read from the environment and **appended to, never replaced** —
on every machine this project is built on they already carry Arrow's include and link paths.

`tools/machine_report.sh` prints everything needed to identify a machine and its toolchain — CPU,
SIMD, memory, every compiler on `PATH`, **which compilers fpm will actually use**, Arrow's version,
the already-exported `FPM_*` variables, and the load — and with `--lto-probe` additionally
link-tests a minimal mixed Fortran/C++ program per toolchain in a few seconds. It builds nothing and
is safe to run anywhere. `tools/benchmark_template.md` is the template for a run on another machine:
copy it to a `feature_*.md` file, fill in the campaign, and the machine that runs it writes its
report back into that same file.

`tools/fpm_lto.sh` is the odd one out here: it is **sourced**, not executed, and installs an opt-in
`fpm` wrapper that builds `--profile release` with link-time optimisation (see
[Building with link-time optimisation](#building-with-link-time-optimisation)). Running it instead
prints how to activate it plus a dry run of what it would select on this machine, so it is safe to
invoke to find out what it does.

**Which machine a measurement was taken on is part of the result.** CLAUDE.md's
"The three machines available for testing" lists the three reference machines, what each one
isolates (one pair differs only in architecture, another only in compiler), how to activate either
toolchain on the multi-compiler one, and two hazards there that silently invalidate a run — a
`gfortran` below this project's minimum, and an exported `-ffree-line-length-none`. Read it before
quoting a figure from anywhere but your own machine.

`tools/benchmark_sort_engine.sh` is the baseline and regression harness for `parquet_sorting`'s
engine. It drives `app/benchmark_sort_engine.f90` through four modes — `argsort` (eight key
families over a size ladder, serial and threaded), `dist` (eight input shapes, including
quicksort's classical adversaries and the null/NaN-bearing ones), `ops` (`partial_argsort`,
`nth_element`, `unique`, `rank`, `is_sorted`, `lower_bound`, `merge`) and `threads` (a thread
ladder) — and prints one greppable row per figure plus a checksum. `PERM=32|64` selects which
permutation kind to ask the library for, which is not cosmetic: the int32 specifics build an int64
permutation internally and narrow it into a second array, so the two paths differ by a full O(n)
allocation and copy. The run sheet is `feature_benchmark_sort.md`; the design it serves is
`feature_sort.md`.

Two properties are worth knowing before reading its output. The **data is generated by an
xorshift64 PRNG, not `random_number`** — deliberately, because `random_number` yields a different
sequence under gfortran and ifx, so a cross-compiler comparison would otherwise be comparing two
different inputs; the printed checksum is what confirms two runs saw the same data. And **every
permutation is verified before its timing is accepted** (it must be a permutation, and where the
arm has no nulls or NaNs it must actually order the input), because a fast wrong answer is the one
failure mode a benchmark will happily report as a win.

**Every arm makes one untimed call before its timed loop**, and that is not a formality. The
permutation is an `intent(out)` allocatable, so each call allocates its own; the first at a given
size pays to map those pages and later calls get the same block back already mapped. `reps()` drops
to 1 at the largest sizes, where the whole cost otherwise landed on whichever arm ran first — worth
7.19 ns/elem against 3.05 for identical work on the 50-million-row low-cardinality arm, i.e. it read
as a 2.3x threading win on an engine that ignores `threads=` entirely. It bites hardest on a *fast*
arm at a *large* n, which is exactly where a figure is least likely to be double-checked; a slow arm
at the same size hides the same absolute cost inside its noise.

`ENGINE=cpp|fortran` selects which sort engine to measure — the shipped C++ one, or the pure-Fortran
engine `feature_sort.md` is building, which the library reaches only through a debug hook until that
work is cut over. An A/B is two runs of the script differing in nothing else. The two engines return
identical permutations, so the printed checksum must match across the pair; a checksum that moves
means the two runs did not see the same data and neither figure is comparable. The header line
reports the engine the *library* says it will use rather than the value asked for.

```bash
tools/benchmark_sort_engine.sh                                  # every mode, default sweep
tools/benchmark_sort_engine.sh --mode=threads                   # the thread ladder alone
SIZES=50000000 FAMILIES=f64,i64lo tools/benchmark_sort_engine.sh --mode=argsort
PERM=64 tools/benchmark_sort_engine.sh --mode=argsort           # the int64 permutation path
ENGINE=fortran tools/benchmark_sort_engine.sh --mode=argsort    # the Fortran engine's own figures
```

`tools/benchmark_sort_ab.sh` is the one to reach for when the question is *"is the Fortran sort
engine faster than the C++ one on this machine?"*. It runs `benchmark_sort_engine.sh --mode=argsort`
twice — `ENGINE=cpp` then `ENGINE=fortran`, same build, same data, same seed — and joins the two
tables into one so the per-arm ratio is read off directly instead of by eye across two logs. It
**refuses to print the table at all** if the two arms' checksums disagree, since the two engines
return identical permutations by contract and a mismatch means either they do not or the arms did
not see the same data.

**It sources nothing and chooses no compiler** — it measures whatever the shell it was invoked from
is set up to build. On a machine carrying more than one toolchain, activate one, run it, then
activate the other in a fresh shell and run it again; the build trees and log files are named after
the compiler, so the two runs do not collide. It prints a provenance block describing *that shell*
(compiler, every `FPM_*` variable, commit, load) rather than asking for one separately, because a
provenance block collected in a different shell than the one that built the binary is not
provenance.

It refuses exactly one thing: a `gfortran` below 13. That is the only way to get this environment
wrong *silently* — such a compiler builds the library cleanly and miscompiles it (see
[Prerequisites](README.md#prerequisites)) — whereas a missing Arrow announces itself as
`'arrow/api.h' file not found` and a missing compiler as a build error. With `--test` it runs the
full suite as a correctness gate *before* any timing. It reports the load and does not wait on it —
say what the load was in the report rather than leaving it to be inferred, and note that a figure
taken straight after a suite is worth re-taking if it would change a decision, since an ordered
penalty biases one arm rather than adding symmetric noise and best-of-N does not remove it.

```bash
tools/benchmark_sort_ab.sh                                    # whatever this shell builds
tools/benchmark_sort_ab.sh --test                             # with the correctness gate first
SIZES=1000000,50000000 tools/benchmark_sort_ab.sh
```

`tools/benchmark_sort_comparator.sh` answers a narrower question than the harness above: what does
**one comparison** cost, in the Fortran comparator core (`src/parquet_sorting_engine.f90`) and in
the C++ one it is replacing? It drives `app/benchmark_sort_comparator.f90` over five fixtures
(`i64`, `f64`, `f64` with NaNs, `str`, `multi3`), timing each both ways.

It is a separate harness rather than a mode of `benchmark_sort_engine.sh` on purpose: its arms are
per-**call** nanoseconds where that one's are per-**element**, and mixing the two invites a reader
to compare figures that are not the same kind of number.

Three things about reading its output. Both arms are **batched** — a per-call harness would cross
`bind(C)` once per comparison and would have measured the crossing, not the comparison. The two
loops are written identically and each returns a **checksum**; if they disagree the arms did
different work, so the program says so and exits nonzero, and no timing from that run may be
quoted. And the `control` row is a plain array sum neither comparator can influence, so its
movement between two *builds* is code layout rather than cost.

The wrapper also runs the `objdump` descriptor-block check from `feature_sort.md` Stage 1e. That
check is **x86-64-specific** — the relocation it greps for cannot exist on arm64, so on an Apple
Silicon machine it prints `SKIPPED` rather than a zero that would prove nothing.

| variable | default | meaning |
|---|---|---|
| `ROWS` | `4096` | rows per pass. Keep the fixture **in cache** — raising this until it spills L2 turns the run into a bandwidth test that will read as a comparator regression. |
| `REPS` | `4096` | passes; `ROWS*REPS` is the comparison count per timed sweep (~16.7M by default). |
| `ROUNDS` | `5` | timed rounds per arm; the **minimum** is reported, being the round least disturbed. |

```bash
tools/benchmark_sort_comparator.sh                              # the default sweep
tools/benchmark_sort_comparator.sh --rows=8192 --reps=8192      # a larger working set
ROUNDS=9 tools/benchmark_sort_comparator.sh                     # more rounds on a busy machine
```

`tools/benchmark_table.sh` measures what the `parquet_table` layer costs against reading and
writing columns directly, on one synthetic float64 file. It drives `app/benchmark_table.f90`
through eight runs plus two sweeps (and a ninth, read_one, run on its own): a raw reader baseline, a table open+`materialize_all`, a lazy
open that reads only `TOUCH` of the columns, a slice-regime open covering one of `SLICES` equal row
ranges, an access comparison, `parquet_write_table` against a hand-written per-column write loop —
once on a null-free table and once on one where `NULLFRAC` of the rows are null — a sort run, a
peak-memory pair, and an argsort thread sweep.

The **access** run needs a fixture with at least two columns (`NCOLS=2` or more) and reports two
separate things. First, on already-materialized columns, what `%get` and `%col` themselves cost —
the columns are prefetched before anything is timed, because opening is lazy and a `%get` on a cold
column would otherwise decode the whole column from the file, measuring the decode rather than the
copy. Second, and the reason the mode exists, the same `z = x + y` evaluated three ways: over plain
allocatable arrays, over `%col` pointers, and over `%col` pointers the caller declared `contiguous`.
Computing through a pointer is measurably slower than over arrays you own (roughly 1.1x on
bandwidth-bound columns, ~1.7x once they fit in cache), and the `contiguous` row is there to test the
obvious explanation and refute it — promising contiguity at the call site recovers nothing, so the
gap is not `%col`'s missing stride guarantee. The actionable figure is **"passes before `%get`
wins"**: `%col` skips a copy, so it is ahead until you have iterated over the same columns enough
times for the slower arithmetic to give that saving back.

The **write** run materializes the table before timing anything, for the same reason the access run
prefetches: a `parquet_write_table` on a freshly opened table would decode every column as it wrote
it, charging the whole read to the write path, while the hand-written loop that runs afterwards
finds every column resident. The comparison still leans slightly towards the hand-written loop,
which copies each column out with `%get` first where `parquet_write_table` writes from the store
with no copy — so parity in that output means the table path is genuinely no more expensive.

The **write_nulls** run is the counterpart to `write`. `write` measures the null-free path, where a
column with no nulls is handed to the writer with no validity mask at all; `write_nulls` measures
the case that shortcut cannot help, and splits it three ways: `parquet_write_table`, a hand-written
loop building its mask a row at a time through `%is_null`, and the same loop handed a finished mask.
The third is the floor — writing with nulls and nothing else — so the gap above it is mask
construction, and `parquet_write_table` should sit at that floor because it walks the validity
bitmap a word at a time. The per-row line is legitimately much slower: `%is_null` takes a column
name, so every call repeats a lookup the library does once, and the public API offers no way to
hoist it. It builds its null-carrying input itself (an untimed extra write plus read) rather than
asking the fixture writer for one, because `parquet_table` exposes no way to mark a row null in
memory.

The **sort** run is the only one that touches no file: it builds a table of `SORT_SIZE_GB` worth
of float64 columns plus one character column **in memory**, because what it measures is the cost of
reordering an already-resident table and reading a fixture first would only add a decode to both
sides. It splits `%sort_by` into its two halves — the permutation build (`pf_argsort`, in C++) and the
per-column reindex loop — which is the split worth watching, because on a many-core machine the
second dominates: when the loop was still serial it measured 13.1 s against the permutation's 1.9 s
on a 100+ core server. **Both halves are threaded now** (the loop runs one column per thread,
gated), so that ratio is the historical motivation rather than what a run today reports.

It then measures the reindex phase **two ways**, all columns validating the permutation against
only the first one doing so. That comparison is also the only thing that would notice if
`%reindex_trusted` silently stopped differing from `%reindex` — the two figures would simply
coincide, with every test still passing. The trailing "reference" block prices the two seen-set
representations against each other; `logical` is what `reindex` used before the bit-packed set
replaced it, so those lines say what that change was worth rather than what is still available.

The **argsort** run is also file-free, and answers a different question from `sort`: there the
permutation build is a minority of `%sort_by`, but for a caller of raw-array `pf_sort`/`pf_argsort`
it *is* the whole operation. It runs `pf_argsort` at each of `ARGSORT_THREADS` over each of
`ARGSORT_NROWS`, splitting the result into the per-chunk `std::sort`s and the merge that follows,
and reporting **the merge's last round separately** — that round is where a pairwise merge collapsed
to a single thread, so it is the one the co-ranked merge exists to fix.

Each run measures **both merges, back to back in one process**: once with the minimum segment size
forced above the whole array, which leaves every pair unsegmented and so reproduces the pairwise
merge exactly, and once with co-ranking in force. That is deliberate rather than convenient — an
earlier version compared a co-ranked build against a pairwise one measured on a different day, and
the serial baseline alone had drifted 15% in between, which is larger than some of the effects being
reported. Any before/after claim about this phase should come from one process, not two runs.

Keep `1` first in `ARGSORT_THREADS`: it is the serial baseline, and it reports no phases at all,
because the engine takes the plain `std::sort` path rather than chunking.

The **peakmem** pair answers a question none of the runs above can, and the reason it needs its own
mode is worth knowing before anyone tries to fold it back into `sort`. A parallel row-structural
mutation holds one transient column copy per thread instead of one in total; the library's answer to
that is documentation plus the `parquet_set_table_threads` cap rather than a memory-derived limit, so
the "at most doubles the table's peak" claim has to be measured rather than asserted. **`sort` cannot
measure it** — it builds a *second*, standalone set of columns in order to time the reindex phase in
isolation, and that second set, not the mutation, is what sets its process peak. Three different
machines reported an RSS figure from that mode and all three had to discard it. `--mode=peakmem`
builds one table, sorts it exactly once, and allocates nothing else.

The answer is the **difference** between the run's two points, `PEAKMEM_THREADS="1 0"` (serial, then
automatic). Both build the identical table, so whatever separates their peaks is the mutation's
transient, and the mode prints the predicted value — `(T-1)` copies of one column — beside it. The
peak is read with `/usr/bin/time` wrapped around the benchmark binary via `fpm run --runner`, not
around `fpm`, whose own compile and link peaks would dominate; the mode's two in-process RSS lines
are context, not the answer, because the transient is gone before the program regains control. On an
8-core machine the transient measures about 86% of `(T-1)` copies, the shortfall being that the
copies are not simultaneous — each lives only between its column's allocation and its `move_alloc`,
and `schedule(dynamic)` staggers when columns finish.

The lazy and slice runs are the ones to read against `read_table`: the open figure shows what an
open costs when it reads nothing, and the two partial modes show that a program pays only for the
columns and rows it asks for. A slice cannot be cheaper than one row group, so a fixture written
with a single row group will show no slice saving — the mode says so in its own output.

The raw baseline reads **one array per column and holds them all at once**, matching what a table
holds. Reusing a single buffer for every column instead would measure a different job and flatter
the raw path — the same pages get overwritten and stay warm, where the table touches the whole
file's worth of distinct memory (worth ~15% of the raw-vs-table gap on a 0.4 GB 8-column file).

The number worth watching is **"Arrow pool still holding"** in the two read sections. `parquet_table`
keeps its own Fortran copy of every column and releases the reader's decoded Arrow buffers as it
goes, so a fully materialized table should report ~0 MiB there while the raw baseline reports the
whole file (on top of its own Fortran copies — roughly two copies resident). **Resident set size cannot answer this question** — Arrow's memory pool keeps freed
pages rather than returning them to the OS, so RSS stays high in both cases; the pool's own
`bytes_allocated()` is what distinguishes "released" from "still alive". The two read modes also run
as separate processes for the same reason.

```sh
tools/benchmark_table.sh
# Bigger file, more columns:
TARGET_FILE_SIZE_GB=2.0 NCOLS=16 tools/benchmark_table.sh
# Read 2 of 32 columns, and one eighth of the rows:
NCOLS=32 TOUCH=2 SLICES=8 tools/benchmark_table.sh
# Keep the synthetic file instead of a temp dir that gets deleted:
TEST_FILE=/tmp/bench_table.parquet tools/benchmark_table.sh
# The sort run is sized on its own, since it builds its table in memory:
SORT_SIZE_GB=3 NCOLS=24 tools/benchmark_table.sh
# A wider argsort sweep (also file-free), on a machine with more cores:
ARGSORT_NROWS="1000000 20000000" ARGSORT_THREADS="1 2 4 8 16 32 64" tools/benchmark_table.sh
```

The **read_one** mode is not part of the script's sequence and is run on its own. It times reading a
single whole column with Arrow's own `use_threads` on and off, which answers a question no other mode
does: whether Arrow already parallelises a single-column decode internally. On an 8-core machine it
does not — 0.96x–1.02x across two column sizes — which is why splitting one column's read across row
groups is still worth doing.

**Its most important line is the control, not the timings.** "The two arms take the same time" and
"the flag never reached the reader" produce identical output, so the mode prints the `use_threads`
value each arm's reader actually resolved to (`1` and `0`) and says outright that the timings mean
nothing if those match. Copy that shape for any future A/B benchmark whose expected result is *no
difference*: without a control, such a benchmark passes just as happily when it is measuring one
configuration against itself. The mode also uses a fresh reader per timed read (a reader caches its
decoded column, so a second read on one reader times a cache hit), one untimed warm-up read so both
arms see the same page-cache state, and alternating arms within each round.

```sh
fpm run benchmark_table --profile release -- \
    --mode=write_fixture --file=/tmp/pf_bench.parquet --size=4.0 --ncols=24
fpm run benchmark_table --profile release -- --mode=read_one --file=/tmp/pf_bench.parquet
```

The peak-memory pair can also be run on its own, which is usually what you want — it is the only
part of the script whose answer is a difference between two processes, and the rest of the script
does not have to run for that difference to mean anything. Use a narrow table to make it bite: the
bound is tightest when the thread count approaches the column count.

```sh
# The two points, at the sort run's own reference configuration:
for t in 1 0; do
    fpm run benchmark_table --profile release --runner "/usr/bin/time -l" -- \
        --mode=peakmem --size=4.0 --ncols=24 --threads="$t"
done
# GNU time (Linux) reports the same figure under -v rather than -l.
```

**Always pass `--profile release` for any measurement here, and do not try to get optimisation out of
`FPM_FFLAGS` instead.** fpm applies profile flags only when `--profile` is given: without it a build
has no `-O` at all, whether or not `FPM_FFLAGS` is set. Reaching for `FPM_FFLAGS="-O3 -fopenmp" fpm
run` instead leaves the C++ half at whatever the environment supplies, which on a dev machine may be
nothing — measured as a 5.7x difference in `pf_argsort` alone, enough to invert a comparison. The
wrapper already passes `--profile release` on every run.

**`FPM_FFLAGS` itself is additive and does not suppress the profile's flags** — an earlier version of
this file said otherwise. If your environment exports it (many do, carrying Arrow's `-I` paths), a
`--profile release` build still gets `-O3` and a `--profile debug` build still gets `-fcheck=bounds`.
Verified on fpm 0.13.0 alpha by reading the emitted compile line on two machines; see CLAUDE.md's
"Don't run the GitLab CI pipeline yourself" for the four-way table.

`tools/check_arrow_release.sh` is the assertion form of the figure the benchmark above only reports. It drives `app/check_arrow_release.f90` over every `parquet_table` materialization path — `%materialize_all`, `%prefetch`, a single lazy `%get`, a slice, and `parquet_write_table(release=.true.)` — and **exits nonzero** if any of them still holds more than `TOLERANCE` of one copy of the data it just read in Arrow's pool. That failure is otherwise completely silent: a path that forgets to release leaves the values correct and every test passing, with the table quietly holding two copies of every column ([feature_risks.md](feature_risks.md) Risk-1).

Three things about it are load-bearing rather than incidental:

- **Each mode runs in its own process.** A baseline and the path under test measured in one process report the high-water mark of the pair, which makes whichever ran second look like it retained memory it had already released.
- **The `control` run goes first and asserts the counter MOVES**, by reading a column through a plain reader (which caches it) and checking the pool grew. Without it, a measurement that silently reported zero — a different Arrow build, a pool that is not the default one — would print PASS for every path and mean nothing.
- **`materialize_all` and `prefetch` run again under `OMP_NUM_THREADS=1`.** The internally-parallel `%prefetch` gives each thread its own reader and closes it at the end of the region, and closing a reader frees whatever it cached whether or not the release ran — so the parallel path passes even with every `parquet_release_column` call deleted. Verified by deleting them: the parallel run reported 0.0 of one copy and the serial run 1.0.

```sh
tools/check_arrow_release.sh
# Bigger file, tighter tolerance:
TARGET_FILE_SIZE_GB=0.5 NCOLS=16 TOLERANCE=0.005 tools/check_arrow_release.sh
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

`tools/test_random_large_fill.sh` is a manual, user-runnable check (never run by `fpm test`/CI) that `pf_random_fill_at` is still correct past 2³¹ array elements. Both fill routines take their length from `size(v)`, and a `size` asked without an explicit `kind=` returns a **default-kind** integer, which wraps there. The wrap fails silently in two different ways, both measured on this module before they were fixed: 2³¹ elements exactly gave a length of −2147483648, which tripped the zero-size guard so the routine returned having written *nothing* and left the caller's `intent(out)` array undefined; and 2³² + 8 elements gave a length of 8, so eight values were written and the remaining 4.29 billion were left undefined. Neither raises anything, and no test `fpm test` could run is large enough to reach the boundary — the smallest array that does is 2³¹ `real32` values, about 8.6 GB. `check_fill_size_kind` in `tools/check_source_conventions.py` is the cheap always-on guard against the declarations regressing; this is the end-to-end proof, and it verifies *correctness* (each probed element against `pf_random32_at`/`pf_random_at` at the same position) rather than merely that something was written:

```bash
tools/test_random_large_fill.sh                    # 2**31 real32 elements, about 8.6 GB
RKIND=64 tools/test_random_large_fill.sh           # the same count as real64, about 17.2 GB
ELEMENTS=4294967304 tools/test_random_large_fill.sh  # 2**32 + 8: the partial-fill variant
```

`ELEMENTS` below 2³¹ still exercises the fill but **cannot** detect the bug this tool exists for, and the script says so on stderr rather than passing quietly.

`tools/check_doc_anchors.py` validates every `#anchor` link in this repository's `*.md` files — same-file and cross-file — against the anchors GitHub would actually generate for each file's headings (using GitHub's real slugging rules, including the `-1`/`-2` suffixing for repeated headings), and exits nonzero if any link doesn't resolve. Run it after editing headings or anchor links in README.md/CONTRIBUTING.md:

```bash
tools/check_doc_anchors.py
```

`tools/check_bindc_boundary.py` cross-checks every Fortran `bind(C)` interface (`src/parquet_bindings.f90`, plus the debug-hook interfaces in `test/error_scenarios.f90`/`test/test_temporal.f90`) against its C++ `extern "C"` definition in `src/parquet_wrapper.cpp`, verifying arity, base type, by-value-vs-by-reference, and (for functions) return type all agree — a `bind(C)` interface has no compile-time link to the C++ side it describes, so a kind mismatch there compiles cleanly on both sides and corrupts memory silently at runtime instead of failing to build. Run it after touching either side of that boundary:

```bash
tools/check_bindc_boundary.py
```

`tools/check_source_conventions.py` enforces fourteen structural invariants that no compiler and no runtime test can see — each one's violation compiles cleanly, passes the whole suite, and fails somewhere else entirely. Each names the [feature_risks.md](feature_risks.md) entry it protects, in its own docstring and in the message it prints:

- **`parquet_table` must gain no allocatable component.** The type is finalizable, and this project has three confirmed compiler bugs in exactly the `intent(out)`/`FINAL` machinery on exactly this type, so new table state goes on `parquet_table_cache` instead (see [New `parquet_table` state goes on the CACHE](CLAUDE.md#new-parquet_table-state-goes-on-the-cache--never-as-an-allocatable-component-of-the-type)).
- **Every pointer a table accessor hands out must be reached through `self%cache`.** Pointing at `self` directly makes the caller's table need the `target` attribute, and `target` is a requirement on the *caller* — so the library still compiles, every test still passes, and only a user's program corrupts memory.
- **The generated files must carry the doc-comments and `! GCOVR_EXCL_LINE` markers their generators are supposed to emit.** The generators' own `--check` modes compare the committed file against the generator, so a *template* that drops a convention produces output that matches perfectly and is wrong in every kind it emits at once.
- **A schema-less `parquet_write_table` must declare `col_size:`/`array_size:` as `auto`, never a measured value.** Declaring `auto` is what makes the generated schema unable to get a size wrong, since the writer resolves both from the data. A round-trip test cannot see a change here: the sidecar MAML is emitted at *close*, after the writer has resolved the real numbers, so it still looks correct.
- **Every procedure that calls `check_row_group_valid` must also call `check_reader_no_sort`.** A sort permutation destroys row-group locality, so a row-group-scoped read that forgets the guard returns the right *number* of rows and the wrong ones. Pairing the two turns "did we guard all ~19 sites?" into something mechanical, and it extends itself — a new row-group-scoped read validates its row group as a matter of course.
- **`print_stat`'s columns must match the table documenting them in `doc/pages/io/reading.md`.** That table is the format's only contract: exactly one assertion in the whole suite touches the output text, so a renamed column breaks no test. The two had already drifted when this check was added.
- **`parquet_print_settings`'s rows must all be named in `doc/pages/operating/settings.md`.** Same reasoning as `print_stat` above. Rows are matched by the *shape* of the call rather than against a list of helper names, because the list version went blind twice as new row helpers were added — the second time reporting three of five new rows as documented when the check simply could not see them.
- **Every `cfg_*` setting must be read somewhere other than where it is written.** A setting nothing consults still round-trips through its own getter and still reports the right factory value, while doing nothing at all ([Risk-41](feature_risks.md)). This catches "nothing reads it"; the observed-effect tests in `test/test_settings.f90` catch "reads it wrongly".
- **Nothing in `src/` writes to a unit directly outside the three emit channels.** Routing every message through `parquet_emit_info`/`_warning`/`_error_context` is what makes `verbosity` and `message_stream` apply everywhere. A print written the old way still appears at default settings, so the suite stays green and only users who changed a setting are affected.
- **The row-group sizing arithmetic must exist once.** Its two callers serve different writers (a whole-table write and the streaming path's estimate), so a re-inlined copy takes the built-in constant instead of the setting: `parquet_set_target_row_group_bytes` then governs one kind of write and not the other, with every row correct, the row-group count wrong, and nothing failing ([Risk-43](feature_risks.md)).
- **`src/` must hold exactly one C++ translation unit.** Every process-global in `parquet_wrapper.cpp` is a file-scope `static`, so a second `.cpp` would silently get its own copy of each — breaking both the `parquet_debug_*` test overrides and the settings mirrored from `parquet_settings`. Not a ban on splitting the file: it is [CLAUDE.md's TU-split note](CLAUDE.md#if-srcparquet_wrappercpp-is-ever-split-into-multiple-translation-units) firing at the moment somebody does.
- **Every setting must have an environment variable.** `parquet_settings_from_env` is the only way a setting can be exercised without recompiling, and a knob it forgets is unreachable that way with nothing to say so. The knob list is taken from `parquet_print_settings`' own printed rows, shared with the documentation check above, so a new setting fails both at once rather than needing two lists remembered separately.
- **An `allocate` extent taken from `size(...)` must ask for `kind=int64`.** A default-kind `size()` wraps above 2³¹ elements, and in an allocate extent that is worse than the short fill it causes elsewhere, because the loop that follows usually gets its bound *right*: ten sites in the generated table accessors read `allocate(arr(size(rows)))` on one line and `do k = 1, size(rows, kind=int64)` on the next, so a slice past 2³¹ rows would have allocated a zero-length array and then written the full count into it. Element assignment does not reallocate, so that is an out-of-bounds write on a valid call rather than a wrong answer — unlike the sibling `arr = p` shape, where intrinsic assignment to an allocatable resizes and hides the mistake. Matched by shape with no exemption list, since `kind=int64` costs nothing even where the extent is provably small ([Risk-104](feature_risks.md)).
- **No per-element helper in `parquet_wrapper.cpp` may take an Arrow array by `const std::shared_ptr<arrow::Array> &`.** A `shared_ptr` parameter looks free and is not: every `std::static_pointer_cast` inside such a helper builds a new one, i.e. an atomic increment and decrement, and in a helper called once per row that becomes the dominant cost of the whole operation — `real_family_value_at` measured at **13.6 ns per row** to read one double that way against **1.8 ns** with a raw pointer, when the clause evaluation it serves was 75–93% of the cost of installing a row filter. Nothing fails when this is undone: every answer stays identical and the suite stays green, only the clock moves. Matched by *shape* (an array parameter next to an element index), so it cannot go blind to the next helper added.
- **Every scenario `test/error_scenarios.f90` dispatches on must be named in `tools/run_error_scenarios.sh`.** That array is what CI runs *and* what `prime_error_scenarios` pre-runs in parallel, but nothing proved the reverse direction: a scenario added to the `select case` and driven from a test, yet never listed, is invisible to the script and silently loses its priming — it just falls back to spawning on demand, so nothing fails and nothing says anything. Names are derived by shape from the `select case` itself, and an empty result is a failure, so the check cannot go blind the way an enumerated list would.

Run it after touching the table layer or a generator template (it is also part of `tools/run_lint_check.sh` and CI's `lint` stage):

```bash
tools/check_source_conventions.py
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

`tools/benchmark_strings.sh` times every `parquet_string_column` bulk operation — `reindex`,
`gather`, `delete_by_mask`, `trim_all`, `clone`, `slice`, `append_column`, `to_character`,
`view_all`, `build_from` and a per-element `length` loop — on one synthetic in-memory column, so an
optimisation to that type is measured rather than argued. It touches no file. Two runs by default,
null-free and null-containing, since several operations have a separate validity pass whose cost
appears only in the second.

Each operation is best-of-`ROUNDS`, every round starting from a fresh `%clone()` of the same source
column so no round inherits another's page state or allocation. The throughput column is
payload-equivalent — the source column's byte count over the elapsed time — and compares rows
against each other only; it is not a claim about bytes moved, since `gather` selects half the rows
and `to_character` writes a wider padded result than it reads.

```bash
tools/benchmark_strings.sh
NROWS=20000000 LEN=48 ROUNDS=5 tools/benchmark_strings.sh
NULLS=no tools/benchmark_strings.sh                          # skip the null-containing run
```

Config: `NROWS` (default 4000000), `LEN` (24, the mean element length — lengths vary
deterministically around it), `ROUNDS` (3), `NULL_EVERY` (7), `NULLS` (`both`, or `no`/`yes`).

The app also takes `--threads=N`, which caps `parquet_set_string_threads` for the run — a sweep
over `1 2 4 8` is how the internally-threaded operations' scaling is measured, and it is the
only way to see the serial path and the threaded path of the same operation side by side.

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp`/`openmp_write` test suites, plus the `concurrent_calls_into_shared_reader`/`writer` error scenarios) need OpenMP to actually be active to exercise concurrency:

```sh
fpm test
```

`fpm.toml`'s `openmp = "*"` dependency (fpm's built-in OpenMP metapackage) already supplies the compiler-appropriate flag (`-fopenmp` for gfortran, `-qopenmp` for ifx, ...) automatically — there is no need to pass one manually via `FPM_FFLAGS`. The two concurrency error-scenario tests are **self-adapting**: they check `omp_get_max_threads()` and, when it's `1` (OpenMP genuinely inactive, or `OMP_NUM_THREADS=1`), the shared-reader/writer race cannot occur, so they skip and pass trivially — so a green `fpm test` alone doesn't *prove* these specific concurrency checks ran for real; if you want to double-check that OpenMP is active in your environment, print `omp_get_max_threads()` from a small program or check the test suite's own thread-count assertions.

### Continuous integration (GitLab CI)

`.gitlab-ci.yml` runs the full `fpm test` suite (with OpenMP and coverage) on a GitLab Docker-executor runner. It builds the whole toolchain from scratch in the container's `before_script`, so it also serves as an executable, always-current recipe for building this project on a clean Debian/Ubuntu system.

A separate `lint` stage runs first and needs only `python3` — no Arrow/Parquet/fpm/gfortran/git-lfs setup — so it fails fast on the cheap checks: `tools/check_bindc_boundary.py`, `tools/check_doc_anchors.py`, `tools/check_source_conventions.py`, and `--check` for all six of this project's committed-output generators (`tools/generate_parquet_columns.py`, `tools/generate_parquet_tables.py`, `tools/generate_parquet_sorting.py`, `tools/generate_parquet_maml.sh base`, `tools/generate_user_table_code.py`, `tools/generate_random_golden_vectors.py`), plus the last three's `--self-test` — see [Some `src/*.f90` files are generated](CLAUDE.md#some-srcf90-files-are-generated--edit-the-generator-never-the-output) for what each one regenerates and why a drifted generator is worth catching in CI rather than only at the next regeneration.

A few choices in that file are load-bearing — each one cost a debugging round when it was wrong, so preserve them if you touch it:

- **Base image `ubuntu:24.04`** (pinned with `image:`, since the runner's own default image is older). 24.04 is the oldest Ubuntu that satisfies *every* toolchain requirement at once: gfortran 13 (gfortran ≤ 11 miscompiles the optional allocatable-character argument in `schema%add_col_qc` — see [README's Prerequisites](README.md#prerequisites)), a g++ new enough for C++20 / `std::span`, `pipx` in the repos (used to install `fpm`), and current Arrow apt packages. Its default `gcov` also matches its default compiler, so `gcovr` needs no `--gcov-executable` override.
- **`git lfs pull`** before running tests. The `test/fixtures/*.parquet` files are Git-LFS-tracked (see `.gitattributes`); without pulling them the reader tests read LFS *pointer* files and fail. `git lfs install --skip-repo` sets up only the global filter config (CI never pushes, so the repo-local hooks are deliberately skipped — installing them fails if the checkout already has one).
- **`libarrow-compute-dev`** installed alongside `libarrow-dev` / `libparquet-dev`: Arrow ships its compute kernels in a separate package, and `fpm.toml` links `arrow_compute` (see the [Troubleshooting guide](doc/pages/operating/troubleshooting.md)). Omitting it fails the C++ compile on `arrow/compute/*.h`.
- **`FPM_FFLAGS="--coverage"`.** Source is kept within the standard 132-column free-form limit (see [Conventions](#conventions)), so no `-ffree-line-length-none` override is needed. `--coverage` genuinely must be passed explicitly here — gcov instrumentation isn't something any fpm metapackage supplies. **There is deliberately no `-fopenmp`**: `fpm.toml`'s `openmp = "*"` dependency already injects it into both the compile and the link flags, for this package and its dependencies alike, regardless of whatever else `FPM_FFLAGS` is set to. Check it yourself with `fpm build --show-model | grep -o 'fortran_compile_flags="[^"]*"'` — `-fopenmp` is there even when `FPM_FFLAGS` carries nothing but `-I` paths. It was passed here explicitly for a long time and was redundant the whole time. Two related facts worth knowing, since neither is obvious and both bite locally rather than in CI: whatever you put in `FPM_FFLAGS` **replaces** what the environment already exported (on a dev machine that is often where Arrow-adjacent `-I` paths live), and this job passes **no `--profile`**, so it runs with no `-O`, no `-g` and no `-fcheck=bounds` — exactly as a plain `fpm test` does. That is a property of omitting `--profile`, **not** of setting `FPM_FFLAGS`: `FPM_FFLAGS` is additive and leaves a profile's flags intact (an earlier version of this file claimed it suppressed them; see CLAUDE.md's "Don't run the GitLab CI pipeline yourself" for the verified four-way table). Use `fpm test --profile debug` when you want those checks — it supplies them whether or not `FPM_FFLAGS` is set. `FPM_CXXFLAGS="-std=c++20 --coverage"` and `FPM_LDFLAGS="-lstdc++ --coverage"` follow [README's Environment variables](README.md#environment-variables), with `--coverage` added on both so `src/parquet_wrapper.cpp` is instrumented too, not just the Fortran sources.

Coverage is computed by `gcovr` over `src/` and surfaced through GitLab's `coverage:` regex — this now includes `src/parquet_wrapper.cpp`'s line coverage alongside every `src/*.f90` file, since this job's `gfortran`/`gcc`/`g++` all come from the same apt GCC install (one matched GNU toolchain), so the `.cpp`'s gcov data reads back cleanly in the same `gcovr` pass. **This is CI-only** — `tools/coverage.sh` deliberately does *not* attempt the same for `src/parquet_wrapper.cpp`, since it can't assume a matched toolchain on an arbitrary dev machine (e.g. a Mac with `gfortran` from one distribution and a default `clang++` `FPM_CXX` would produce gcov data in Clang's own format, which plain GNU `gcov`/`gcovr` doesn't reliably parse — confirmed to be exactly this project's setup on at least one contributor's machine). `tools/coverage.sh` still builds with `--coverage`, runs the suite plus every error scenario, and prints per-file and total `src/*.f90` coverage only (resolving the `gcov` that matches your `gfortran` automatically). Both coverage scripts build into their own tree (`build/gcov`, `build/gcov-cpp`) and **delete it again on exit**, because an instrumented `error_scenarios` binary left under `build/` can be picked up by the `find build -type f -name error_scenarios | head -1` lookup `tools/run_error_scenarios.sh` uses — which in the worst case reports every scenario as passing while running a binary that predates your edits. (`test/test_errors.f90` is no longer exposed to this: it derives the sibling binary from its own argument 0, so it always uses whichever build tree `fpm` launched it from, and only falls back to `find` when argument 0 cannot answer.) Set `COVERAGE_KEEP_BUILD=1` to keep the tree (for inspecting `.gcda`/`.gcno` files after a failed run), and delete it yourself before the next plain `fpm test`.

For local `src/parquet_wrapper.cpp` coverage on exactly this kind of mismatched-toolchain machine, use `tools/coverage_cpp.sh` instead — a separate script, not a flag on `tools/coverage.sh`, because the two can't be instrumented/collected in the same pass locally (Fortran's GNU gcov data and Clang's own gcov-compatible format need different collection code, and a `--coverage` build of one toolchain doesn't help report on the other). It auto-detects an Arrow install prefix (override with `ARROW_PREFIX`), builds `src/parquet_wrapper.cpp` with `--coverage` via `FPM_CXX` (`clang++` by default), and reports that one file's line coverage — via GNU `gcov -j` when the C++ compiler is GCC-like, or `xcrun llvm-cov gcov` when it's Clang:

```bash
tools/coverage_cpp.sh
```

**Reproducing the CI environment locally.** `tools/build_ci_test_image.sh` bakes everything `.gitlab-ci.yml`'s `before_script` installs — gfortran/gcc/g++, `fpm` and `gcovr` via pipx, Intel oneAPI's `ifx`, and the Arrow/Parquet C++ dev packages — into one image tagged `parquet-fortran-ci-test:latest` (override with `IMAGE_NAME`). Run it once, and again only when its install steps or that `before_script` changes; testing against the built image then starts in seconds instead of reinstalling a toolchain per run. Two things in it are deliberate and explained at length in its own header, so read that before changing either: it drives `docker run --platform linux/amd64` plus `docker commit` rather than `docker build`, because a docker CLI without the buildx plugin *silently ignores* `--platform` and builds for the host arch instead; and amd64 itself is required, not cosmetic, because the `fpm` PyPI package has no linux/arm64 wheel and the source fallback fails on an unrelated toml-f/jonquil CMake error. Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`). Note this is a convenience, not an authority: [CLAUDE.md's workflow guardrails](CLAUDE.md#dont-run-the-gitlab-ci-pipeline-yourself) still say the pipeline itself is the maintainer's to run.

`tools/run_ci_test_image.sh` is the other half of that pair — it runs against the image `tools/build_ci_test_image.sh` produced (same `IMAGE_NAME` override), so it starts in seconds rather than reinstalling a toolchain. On its own it prints the image's toolchain versions (`fpm`, Arrow/Parquet, `gfortran`, `flang`, `ifx`), which is the quickest way to confirm what a given image actually contains; a marked placeholder near the end of the script is where a one-off command goes when you want to run something in that environment by hand. It fails with a pointer to the build script if the image is missing, rather than rebuilding something that takes minutes. **It deliberately does not bind-mount this repository**: the container is amd64 while the host may not be, so sharing one `build/` tree between them would mix objects of two architectures and produce a link failure that reads like anything but a mount problem — add `-v` only together with a separate `FPM_BUILD_DIR` for the container, and read the script's own header first. Maintainer-only (stripped from the fpm-published package too).

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

### Releasing

Bumping the version is currently a manual sequence across several files — nothing checks that all
of them were updated together except `parquet_get_version`'s own runtime drift warning (see below),
so work through this list in full for every release:

1. **`VERSION.txt`** — the source of truth. `fpm.toml`'s `version = "VERSION.txt"` and its
   `RELEASE_VERSION={version}` macro derive from this automatically; nothing else to do for that
   part.
2. **`src/parquet.f90`'s `cversion`** — a hand-maintained `"vX.Y.Z (date)"` string (e.g.
   `"v1.0.0 (2026-07-27)"`), used as `parquet_get_version`'s `mode="internal"` value. Update both
   the version and the date. `parquet_get_version` compares the fpm-injected `RELEASE_VERSION`
   macro against this string and prints a runtime warning if they disagree — the one part of this
   sequence that is actually checked, but only after the fact (at a user's next run), not at
   release time.
3. **`CHANGELOG.md`** — add a new `## [X.Y.Z] - YYYY-MM-DD` section (Keep-a-Changelog format,
   `### Added`/`### Changed`/`### Fixed`/etc. grouping, matching the existing `[1.0.0]` section).
   If `[Unreleased]` entries have accumulated since the last release (see `CLAUDE.md`'s changelog
   guidance), retitle that section rather than starting a new one.
4. **A git tag** matching the version (e.g. `v1.0.1`), on the commit that bumped `VERSION.txt`.
5. **`README.md`'s status line** (`**Status: 1.0 — first stable release.**`, near the top) —
   only needs touching on a **major** version bump (it deliberately names the series, not the
   patch level, specifically so routine `1.0.x` releases don't need this step at all).
6. **`fpm publish`** — see "Publishing to the fpm registry" below; this is a separate, deliberate
   step with its own prep script, not part of the version bump itself.

None of this is automated or enforced by CI today — a future release-checklist script (comparing
`VERSION.txt` against `cversion` and the latest `CHANGELOG.md` heading, say) would close that gap,
but hasn't been written.

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

The script also accepts `--dir=<name>` (or `--dir <name>`) to scan a different directory than the default `schemas/` — this project's own regeneration above never needs it (its fixtures live under `schemas/`), but it exists so downstream projects following ["Embedding your own MAML schemas"](doc/pages/utilities/embedding-maml-schemas.md) can match whatever convention their own project already uses.

The generated file carries a header stating it is auto-generated — do not hand-edit `src/parquet_maml_base.f90`; instead edit the source `.maml` files under `schemas/` and re-run the script.

(The same script's other, no-argument mode is a separate, consumer-facing feature for embedding schemas in a downstream project — see ["Embedding your own MAML schemas in your own project"](doc/pages/utilities/embedding-maml-schemas.md).)

## Extending the MAML schema

`parquet_validate_maml` checks that section names and known sub-keys are registered in [src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90) (`allowed_maml_sections`). This checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.), permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks — if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed) `allowed_maml_nested_sections` in [src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly (print a diagnostic to stderr and call `std::abort()` — see `report_fatal_error`/`ConcurrencyGuard` in that file) over `throw`ing and `catch`ing within the same function. Testing found that a C++ exception thrown and caught within the very same function can still go uncaught when the final executable is linked by `gfortran` on macOS: `gfortran`'s driver passes `-no_compact_unwind` to the linker, which breaks libc++abi's stack unwinding for objects compiled by `clang++`, so `try`/`catch` is unreliable across this specific toolchain combination. A `throw` that's never meant to be caught (i.e. deliberately left to crash the process — most of this file's `throw std::runtime_error(...)` sites) is unaffected, since nothing downstream depends on unwinding actually working.

## Features considered but not implemented

These were looked at (during an audit comparing this library against Arrow C++'s broader feature set) and deliberately deferred rather than rejected outright — they're worth revisiting if a concrete use case actually needs them, rather than adding speculatively:

**Plausible future candidates, if needed:**
- Per-column writer properties (e.g. `disable_statistics()` for write-heavy/throwaway files) — small, additive, doesn't touch the type system. (One instance of this — BYTE_STREAM_SPLIT + disabled dictionary encoding for `float32`/`float64` columns — is now implemented, but automatically/type-based rather than as a caller-facing toggle; see [Automatic BYTE_STREAM_SPLIT for float columns](CLAUDE.md#automatic-byte_stream_split-for-float-columns-in-the-writer). A *manual* per-column override for other types remains unimplemented.)
- `qc:`/`parquet_filter` support for the `date`/`time`/`timestamp` types (`parquet_date`/
  `parquet_time`/`parquet_timestamp`) — neither quality-control range/miss checks nor row filtering
  is implemented for these three yet, unlike every other supported type. An `INTERVAL`/duration
  type is a separate, deliberately dropped non-goal rather than a pending gap — see
  [Not yet supported](doc/pages/types/date-time.md#not-yet-supported) for why.

**Bigger lifts, worth being cautious about:**
- Predicate pushdown (statistics-based I/O skipping) — *not to be confused with row filtering, which is already implemented* (`parquet_filter` / `parquet_open_reader(..., filter=)`, see the [Row filtering section](doc/pages/io/filter-sort-sample.md#row-filtering-with-parquet_filter)). That existing filter is post-decode: it narrows the rows your code sees but still reads and decodes every referenced column in full. Genuine predicate pushdown — using per-row-group statistics (or Arrow's expression/compute-filter machinery) to skip reading matching row groups off disk entirely — is the unimplemented part, and the README's "No predicate pushdown" limitation treats it as an intentional non-goal for now.
- `MAP` columns, and variable-length `LIST` columns nested inside a `STRUCT` path (as opposed to
  a top-level list-encoded vector column, already supported — see
  [Reading a column into a different numeric kind](doc/pages/types/supported-data-types.md#reading-a-column-into-a-different-numeric-kind)'s
  neighboring sections) — Arrow supports these natively, but they'd break the library's core
  "flat columns + fixed `col_size` vectors" data model that the whole Fortran-side API is built
  around; this would be a redesign, not an addition. (`STRUCT` columns themselves are already
  supported for reading, at any nesting depth, down to a scalar or `FIXED_SIZE_LIST` leaf — see
  [Reading a nested struct field](doc/pages/types/supported-data-types.md#reading-a-nested-struct-field).)
- Additional scalar types on the **write** side (`int8`/`int16`/unsigned integers/`decimal` as a MAML-declarable/`parquet_write_column`-writable `data_type`) — straightforward from Arrow's side, but each new type multiplies the `parquet_write_*` interface surface (a dedicated subroutine pair per type already exists for each of the six supported types). Writing stays limited to the original six types; only the **read** side gained widening support for these (see below), since that side doesn't need new public entry points — it dispatches on the existing `values` array's Fortran kind, not a new declared type. A scoping pass mapped every remaining Arrow physical type against this library's six read targets (`int32`/`int64`/`real32`/`real64`/`boolean`/`string`) that was not yet supported for reading either:
  - `STRING_VIEW`→string: **implemented** (this library's own writer never produces it — it only
    arrives from a file written by another Arrow-based tool whose stored Arrow schema declared a
    column as `utf8_view()` — see [Reading `string_view` columns from other
    tools](doc/pages/types/supported-data-types.md#reading-string_view-columns-from-other-tools)).
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

**Maintainability changes considered and declined:**
- Splitting the single `src/parquet_wrapper.cpp` translation unit (7,000+ lines) into several
  smaller files. Looked at during a review pass and rejected — the cost/benefit is clearly
  negative, in descending order of severity:
  1. **A silent-breakage hazard for the `g_debug_*` test-only overrides.** These are file-scope
     `static` globals (e.g. `g_debug_force_whole_column_read_error`, `g_debug_string_offset_limit`,
     `g_debug_col_size_limit`) read by production guard checks and written only by dedicated
     `parquet_debug_set_*` functions reachable from `test/error_scenarios.f90`. `static` file-scope
     linkage means one object *per translation unit* — if a split ever put a setter in a different
     `.cpp` than the guard reading it, each TU would silently get its own copy: it would compile,
     link, and the override would simply stop working, with no diagnostic anywhere. Every one of
     them would have to become a genuine `extern` global in a shared internal header to survive a
     split safely (see the `CLAUDE.md` note below).
  2. **~90+ `static` (internal-linkage) helper functions would need their linkage reworked** —
     either moved into a shared internal header as `inline` (inflating compile time for every TU
     that includes it) or given external linkage inside a named namespace. Both are mechanical but
     touch nearly every function in the file: a large diff on a sensitive file, which is exactly
     what makes a real correctness regression easy to miss.
  3. **Coverage tooling is tightly coupled to this one filename.** `CLAUDE.md`'s own
     "`src/parquet_wrapper.cpp`: GCC vs Clang gcov attribution" section, `.gitlab-ci.yml`'s gcovr
     invocation, and `tools/coverage_cpp.sh`'s exclusion-pattern/artifact-marker conventions are all
     keyed to this file. A split would touch every path in every coverage report and require
     revisiting all three, for no change in what's actually covered.
  4. **The large Arrow/Parquet/standard-library include preamble would be duplicated** across every
     new translation unit, so total compile time would likely *increase* rather than decrease
     unless a shared precompiled-header-style internal header absorbed it — which is its own
     ongoing maintenance burden.

  Instead, the file gained ~15 plain-comment section banners (`// ==== ... ====`) marking its
  existing implicit sections, which gives most of the navigational benefit (an editor outline, a
  `grep '^// ===='`) with none of the above costs. Revisit an actual split only if a concrete
  trigger appears — e.g. compile time becoming a real irritant, or a genuinely independent future
  subsystem (a new element domain such as `MAP`/`LIST` support) being added with no shared helpers
  with the existing code; a new subsystem is the one case where a *second* `.cpp` is cheap, since it
  need not disturb the existing helpers at all.
