# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to
*use* the library from your own project, see [README.md](README.md) instead.

**Scope.** This file holds *project-wide* workflow: the conventions everyone follows, and how to
build, test, lint, release and publish. It deliberately does **not** describe how any individual
tool or program works — that lives in each file's own header comment, next to the code it describes.
See [CLAUDE.md](CLAUDE.md#contributingmd-is-project-wide-workflow-only--a-tools-own-detail-goes-in-its-header) for the rule and why it exists.

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

Portions of this codebase, including its documentation, were developed with AI assistance (Claude
Code).

## Conventions

Project conventions worth knowing before contributing. All are applied in day-to-day development and
enforced in review; [CLAUDE.md](CLAUDE.md) carries the full reasoning behind each.

**Naming.** Public module-level API — everything in `src/parquet_core.f90`'s `public ::` list, re-
exported by the `parquet` facade in `src/parquet.f90` — carries the `parquet_` prefix (e.g.
`parquet_open_reader`, `parquet_get_metadata`). Library-wide *utility* modules whose subject is not
a parquet file use `pf_` instead (`pf_sort`, `pf_random_at`). Type-bound procedures (`schema%init`,
`reader%...`) are namespaced by their type and take no prefix. The `maml_` prefix is reserved for
MAML-parsing/building internal helpers. When in doubt, grep for an existing analogous name before
inventing a new one. See [CLAUDE.md](CLAUDE.md#naming-conventions).

**Line length.** Every line in `src/*.f90`, `test/*.f90` and `app/*.f90` — code and comments alike,
including trailing end-of-line comments — must stay at or under 132 columns, the standard Fortran
free-form limit. Wrap with `&` continuations and split long comments across multiple `!`-prefixed
lines; don't reach for a compiler flag to paper over it. Nothing in CI passes `-ffree-line-length-
none`, so an over-long line is a build failure on a stricter compiler.

**New features need tests and docs.** A new feature lands together with (1) unit-test coverage in
the relevant `test/*.f90` suite — plus error-path coverage via `test/error_scenarios.f90` +
`test/test_errors.f90` + `tools/run_error_scenarios.sh` if it has failure modes that `error stop` —
and (2) documentation: a `!>`/`!!` doc-comment on the new public API (picked up automatically by the
FORD-generated reference), the relevant [user guide page](doc/pages/index.md), the
[README](README.md) if the landing-page story changes, and this file if it affects contributor
workflow. **A `CHANGELOG.md` entry is part of that too** — every new feature gets a bullet under
`[Unreleased]`'s `### Added`. `### Changed` and `### Fixed` are deliberately narrower: they record
only what differs from the **released** version, so a change or fix to a feature that itself landed
after the last release gets no entry of its own. See
[CLAUDE.md](CLAUDE.md#new-features-require-tests-and-docs).

**Documentation describes the current state, not a former one.** When you change behaviour and
update the [user guide](doc/pages/index.md), README.md or a doc-comment to match, write what the
library does — not what changed. A sentence a reader cannot evaluate without having seen a version
they never saw is a defect, however true it is; the change itself belongs in `CHANGELOG.md` and the
commit message. Comments in `src/` are exempt, since their reader is a maintainer. See
[CLAUDE.md](CLAUDE.md#a-guide-page-describes-the-current-state-never-a-former-one) for the three
shapes this takes and why sweeping it mechanically is not safe.

**Don't write out a count or a list the repository owns.** Point at the source instead — "the
`new_testsuite(...)` array in `test/run_tester.f90`", not a copy of it. A written-out count drifts
silently and nothing notices; several in this file had done exactly that before it was last
reviewed.

**Read `feature_risks.md` before editing an area it covers.** It is this repository's standing-risks
register: numbered `Risk-N` entries recording properties of the shipped code that a future change
can break with **no test failing and no abort** — a wrong answer, a stale pointer, a corrupted heap,
a silently skipped row group — each with whether a test would catch it today and, where none can,
how to check or avoid it instead. It has four sections (new / proposed test / not testable /
covered-but-still-forbidding-something), and a risk keeps its number when it moves between them, so
`Risk-13` means the same thing in a code comment a year from now. If you implement a proposed test,
update its entry in the same change; if you find a new silent-failure property while fixing a bug,
add a new entry rather than only writing a code comment.

**Checking doc-comment coverage.** A plain `ford docs.md` run does **not** verify that a new doc-
comment was added — FORD's undocumented-entity warnings are opt-in and off by default here. Run
`ford --warn docs.md` instead; see CLAUDE.md's "FORD doc-comment conventions" for what to expect in
its (noisy) output and which warning categories are already-accepted noise.

## Building and testing this repository

The commands below assume Arrow/Parquet and the environment variables from [README.md's
Prerequisites](README.md#prerequisites) / [Environment variables](README.md#environment-variables)
sections are already set up — they're needed here too, since this repository builds itself the same
way a consuming project would.

**Git LFS is also required locally**, not just in CI: `test/fixtures/*.parquet` are Git-LFS-tracked,
and `.lfsconfig` sets `skipdownloaderrors = true` — so cloning without `git lfs` installed
**succeeds silently**, leaving ~128-byte pointer files in place of the real Parquet fixtures, and
the reader/error-scenario tests that depend on them then fail with confusing errors that have
nothing to do with the code you're testing. Before building for the first time:

```bash
git lfs install
git lfs pull
ls -l test/fixtures/*.parquet   # each file should be well over 128 bytes
```

Then:

```bash
fpm test                        # run the suite
fpm clean                       # clean the build directory
fpm clean --all                 # and all dependencies
fpm install --prefix my_path    # generate the executables
```

**`app/` holds the one program that ships; `bench/` holds the rest.** `app/program.f90` builds
`run_parquet_fortran` and is the only executable in the fpm-published package —
`tools/prep_fpm_publish.sh`'s `APP_KEEP` is an allow-list, so anything else dropped into `app/` is
stripped from a consumer's install automatically. Every benchmark and probe program lives in
`bench/` beside the wrapper that drives it, described in its own header comment. Anything needing
more memory, disk or time than `fpm test` should ever attempt belongs there, never under `test/`.

`fpm.toml` reaches `bench/` through a single `[[executable]]` block: `auto-executables = true`
scans `app/` only, and naming one `bench/` program registers that whole directory for
auto-discovery. **A file placed in `bench/` is therefore compiled by fpm** — which is why the four
standalone drivers that must be built by a bare compiler with forced flags
(`tools/check_random_kernels.f90`, `tools/check_exp_key.f90`, `tools/check_argsort_standalone.f90`,
`tools/benchmark_random_kernels.f90`) stay in `tools/`, outside any source-dir.

**If `fpm test` behaves unexpectedly right after a source change** — a test seems to still run old
code, or `error_scenarios` reports a scenario name as unrecognized even though it is clearly in
`test/error_scenarios.f90` — run `fpm clean --skip` before spending time debugging further. fpm's
build cache can serve a stale binary, and building with several different `FPM_FFLAGS` values
creates multiple `build/gfortran_<hash>/` directories, one of which a binary lookup may pick up
instead of the current one. Prefer `--skip` over `fpm clean --all`, which also discards external
dependencies and forces a `test-drive` re-download that is never the cause of this symptom. Note
`fpm build` does **not** build anything under `test/`; use `fpm build --tests`.

### Building with link-time optimisation

**No fpm profile enables it.** `--profile release` supplies `-O3 -Wimplicit-interface -fPIC -fmax-
errors=1 -funroll-loops -fcoarray=single` and nothing more, so `-flto` (gfortran/flang) or `-ipo`
(ifx) has to be added by hand. fpm *appends* `FPM_FFLAGS` and friends to the profile flags rather
than replacing them, so `FPM_FFLAGS="${FPM_FFLAGS:-} -flto"` works — note the `${VAR:-}` prefix,
since a bare assignment would discard the Arrow include and link paths those variables normally
carry.

**Use `tools/fpm_lto.sh` rather than doing it by hand.** Two companion tools matter as much as the
flag itself and getting either wrong is silent: LTO needs a plugin-capable **archiver** (without one
the build succeeds, passes its tests and does no interprocedural optimisation at all, with no error
and no warning), and on ifx it needs oneAPI's own **`ld.lld`**, which the usual environment scripts
do not put on `PATH`. The wrapper handles both, adds the flags only for `--profile release`, and
**refuses to build** rather than hand back a silently LTO-less measurement. Read its header before
changing it — it carries the full rationale, the measurements, and the reason a macOS mixed-family
build may gain nothing however carefully it is set up.

### Running a single test suite/test

While iterating on one area, prefer scoping `fpm test` to just the relevant suite (or a single named
test within it) over running the full suite every time — the full suite, including the OpenMP and
error-scenario subprocess tests, takes much longer:

```bash
fpm test run_tester -- reading                       # one suite
fpm test run_tester -- reading "test name"           # one test within a suite
```

The suite names are the `new_testsuite(...)` array in `test/run_tester.f90`.

### Running the error-path tests

Most of this library's failure modes (invalid MAML, unknown columns, type mismatches, ...) are
reported via Fortran's `error stop`, which aborts the whole process — see the [Error handling
guide](doc/pages/operating/error-handling.md). Since test-drive assertions cannot survive an `error
stop` in the same process, these paths are exercised **out of process** by a helper program,
`test/error_scenarios.f90`, built as its own `fpm` test target. It takes a single scenario name and
deliberately triggers the corresponding failure:

```bash
fpm test error_scenarios -- write_undeclared_column   # aborts
fpm test error_scenarios -- ok                        # the control: exits 0
tools/run_error_scenarios.sh                          # every scenario, standalone
```

The scenario names are the `select case` at the top of `test/error_scenarios.f90`.
`test/test_errors.f90` drives every scenario as part of the `errors` suite;
`tools/run_error_scenarios.sh` does the same standalone, without test-drive. **Keep that script's
scenario list a complete mirror of the `select case`** — `tools/check_source_conventions.py` fails
when it is not, and the list also drives the parallel pre-run that makes `fpm test` roughly three
times faster. See the script's own header for the priming, timeout and dispatch details.

**The `ERROR STOP` prefix and the exit status are your compiler's, not this library's**, and a test
must not assert either: gfortran prints `ERROR STOP <message>` and exits 1, NAG prints `ERROR STOP:
<message>` and exits **2**, flang prints `Fortran ERROR STOP: <message>` and exits 1. Assert the
library's own message text, and — where the point is distinguishing a Fortran abort from a C++-level
one — assert `/= 0` and `/= 134` rather than a particular value. The C++ side is exactly 134 on
every compiler, because that path ends in an explicit `_Exit(134)`. See [Telling them
apart](doc/pages/operating/error-handling.md#telling-them-apart).

### Regenerating the test fixtures

A handful of tests read pre-built Parquet files committed under `test/fixtures/` rather than files
this library writes itself, because they deliberately contain shapes this library's own writer
*cannot* produce. They are built directly against the Arrow/Parquet C++ API by
`tools/generate_fixtures.cpp`, one function per fixture — see that file's header for what each
fixture is for and why.

Because these files are committed, a normal `fpm test` never regenerates them. Rebuild them only
when you change `generate_fixtures.cpp` or otherwise need a fixture recreated:

```bash
tools/run_generate_fixtures.sh
```

It needs the same `FPM_CXXFLAGS`/`FPM_LDFLAGS` used to build the project itself and errors out early
if they are unset. Its `clang++` invocation is unconditional, with no `FPM_CXX`/`CXX` override, so
fixture regeneration is not possible as-is on a GCC-only Linux box without editing the script first.

### Other tools/ helpers

The repository's own scripts live in **two directories, split by contract**:

- **`tools/`** — everything something *automated* depends on: the checks and generators CI runs, plus
  the release, packaging, coverage and mirroring workflow. Breaking one of these fails a pipeline or
  blocks a release.
- **`bench/`** — measurement and probing, run **only by hand**, usually during a campaign and often
  on another machine. Nothing in CI touches `bench/`, so breaking something there is invisible until
  someone runs it. It also holds the benchmark and probe *programs* themselves: `app/` contains only
  `program.f90`, the one executable that ships.

**Every tool is documented in its own header comment** — what it measures or checks, how to invoke
it, what its environment variables mean, and how to read its output. Read that before running or
changing one; the tables below exist only so you know what is there. Everything in both directories
is maintainer-only and stripped from the fpm-published package unless marked **Consumer-facing**
(`tools/prep_fpm_publish.sh` strips `bench/` whole and allow-lists `tools/`).

The one to know before your first push is **`tools/run_lint_check.sh`**: it runs the same checks as
`.gitlab-ci.yml`'s `lint` stage, needs nothing but `python3` and `bash`, and takes about ten seconds.

#### `tools/` — checks and verification

| tool | what it is for |
|---|---|
| `run_lint_check.sh` | Runs the whole CI lint stage locally. Its `CHECKS` list is kept in sync with the CI job **by hand** — a check added to one must be added to the other. |
| `check_source_conventions.py` | Structural invariants no compiler and no runtime test can see. |
| `check_bindc_boundary.py` | Cross-checks every Fortran `bind(C)` interface against its C++ definition. |
| `check_doc_anchors.py` | Validates every `#anchor` link in this repository's `*.md` files. |
| `check_module_footprints.sh` | What each entry module actually costs a consumer to compile against. |
| `check_argsort_standalone.sh` | Compiles the argsort tier with a bare compiler and no Arrow at all. |
| `check_random_kernels.sh` | Standalone-compiles `parquet_random` across compilers and both kernel arms, asserting they agree bit for bit. Timed by `bench/benchmark_random_kernels.sh`. |
| `check_random_ubsan.sh` | UndefinedBehaviorSanitizer over those same standalone compiles. |
| `check_exp_key.sh` | The same instrument for `parquet_expkey`'s frozen `-log(u)` transform. |
| `check_philox_compliance.sh` | Sweeps the shipped Philox kernel against `philox_reference.py`. |
| `check_downstream_maml_module.sh` | Protects the consumer-facing mode of `generate_parquet_maml.sh`. |
| `run_error_scenarios.sh` | Every error scenario, standalone — see above. |

#### `tools/` — coverage

| tool | what it is for |
|---|---|
| `coverage.sh` | Per-file and total `src/` Fortran line coverage, plus uncovered ranges. |
| `coverage_cpp.sh` | The same for `src/parquet_wrapper.cpp`, as a separate pass — see [Continuous integration](#continuous-integration-gitlab-ci). |

#### `tools/` — code generation

Output is committed; re-run the generator and its `--check` after editing one.

| tool | what it is for |
|---|---|
| `generate_parquet_columns.py` | The `parquet_columns` per-kind blocks. **Owns the kind table** the others import. |
| `generate_parquet_tables.py` | The `parquet_tables` per-kind blocks. |
| `generate_parquet_sorting.py` | The `parquet_sorting` per-type blocks. |
| `generate_parquet_ziggurat.py` | `src/parquet_ziggurat.f90`'s 771 layer constants, re-derived rather than pasted. |
| `generate_parquet_maml.sh` | The built-in MAML module — see [below](#regenerating-the-built-in-maml-module). **Consumer-facing.** |
| `generate_user_table_code.py` | A Role-A MAML into a named `parquet_table` extension type. **Consumer-facing.** |
| `generate_random_golden_vectors.py` | The golden vectors freezing `parquet_random`'s bit contract. |
| `generate_random_perm_vectors.py` | The same for the permutation contract. |
| `generate_fixtures.cpp` | The hand-built Arrow fixtures — see [above](#regenerating-the-test-fixtures). |
| `run_generate_fixtures.sh` | Builds and runs that generator. |
| `generate_logo_svg.py` | Regenerates `doc/media/logo.*`. |

#### `tools/` — environment, build and release

| tool | what it is for |
|---|---|
| `machine_report.sh` | Identifies a machine and its toolchain. Run it at the start of every benchmarking run. |
| `fpm_lto.sh` | **Sourced, not executed** — an LTO-aware `fpm` wrapper. See [above](#building-with-link-time-optimisation). |
| `nagfor_fpm_shim/nagfor` | Makes `fpm build` work with NAG despite fpm 0.13's NAG link-line defects. |
| `build_ci_test_image.sh` | Bakes the CI environment into a local Docker image. **Maintainer-only** — see below. |
| `run_ci_test_image.sh` | Runs this working tree against that image. |
| `prep_fpm_publish.sh` | Prepares the disposable branch `fpm publish` packages. **Consumer-facing.** |
| `prep_github_mirroring.sh` | Rewrites GitLab-specific links and badges for the GitHub mirror. |
| `mirror_to_github.sh` | Drives that rewrite and pushes the mirror. |
| `fix_ford_page_links.sh` | Repoints `doc/pages/*.md` links FORD does not resolve in embedded markdown. |

#### `tools/` — utilities and conversion

| tool | what it is for |
|---|---|
| `convert_fits_to_parquet.py` | A FITS binary table into a `.parquet` file. **Consumer-facing.** |
| `parquet_metadata_to_md.py` | A `.parquet` file's metadata as markdown, via `pyarrow`. **Consumer-facing.** |
| `philox_reference.py` | An independent Philox model, for `check_philox_compliance.sh`. |
| `count_lines.py` | Code/comment/blank line counts per source group. |
| `count_tests.sh` | Unit tests per suite, read from source without building. |

#### `bench/` — benchmarks and probes

Run by hand only, never by CI. **All require `--profile release`**; the wrappers pass it. Each `.sh`
drives a `.f90` program of the same name in the same directory.

| tool | what it is for |
|---|---|
| `benchmark_table.sh` | What the `parquet_table` layer costs against reading/writing columns directly. |
| `benchmark_colindex.sh` | Where `parquet_table%get_element`'s per-cell cost goes. |
| `benchmark_threads.sh` | How throughput scales with Arrow's internal thread-pool size. |
| `benchmark_strings.sh` | What each `parquet_string_column` bulk operation costs. |
| `benchmark_sort_engine.sh` | Baseline and regression harness for the sort engine. |
| `benchmark_sort_ab.sh` | Is the Fortran engine at parity with the C++ reference? |
| `benchmark_sort_comparator.sh` | What one comparison costs. |
| `benchmark_sort_readtime.sh` | Where a read-time sort's time goes. |
| `benchmark_random.sh` | `pf_random_*` against the intrinsic `random_number`. |
| `benchmark_random_kernels.sh` | Times the two route (e) kernels against each other. Its correctness twin is `tools/check_random_kernels.sh`; this one reports, that one asserts. |
| `benchmark_arrow_release.sh` | Pins the Arrow-release figure `benchmark_table.sh` only reports, as an assertion. |
| `bench_resolve_ladder.py` | Compile-out ladder over `table_resolve` — attributing per-cell cost without timers. |
| `run_practrand.sh` | Runs the PractRand battery over one axis of `parquet_random`. |
| `large_scale.sh` | Manual large-scale check — genuinely exceeding `huge(1)` rows. Never run by `fpm test` or CI. |
| `random_large_fill.sh` | The same, for the bulk random fills. |
| `benchmark_template.md` | The machine-agnostic run-sheet template. **Start here** for any cross-machine campaign. |

### Testing genuine OpenMP concurrency

This repository's own OpenMP-dependent tests (the `openmp`/`openmp_write` suites, plus the
`concurrent_calls_into_shared_reader`/`writer` error scenarios) need OpenMP to actually be active to
exercise concurrency. A plain `fpm test` gives you that: `fpm.toml`'s `openmp = "*"` dependency
(fpm's built-in OpenMP metapackage) already supplies the compiler-appropriate flag — `-fopenmp` for
gfortran, `-qopenmp` for ifx — into both the compile and the link flags, so there is nothing to pass
via `FPM_FFLAGS`. The exception is flang, where the metapackage contributes nothing and the build is
serial.

**A green `fpm test` does not by itself prove those checks ran for real.** A test that asserts
*threading* skips when no team can be opened, which is correct — the alternative is an assertion
that holds for the wrong reason — but it means a serial build reports success while testing none of
the parallel paths. Read the skip count, not just the pass count: on a threaded build it should be
zero. See [CLAUDE.md](CLAUDE.md#a-test-that-asserts-threading-must-skip-without-openmp).

### Continuous integration (GitLab CI)

`.gitlab-ci.yml` runs the full `fpm test` suite (with OpenMP and coverage) on a GitLab Docker-
executor runner. It builds the whole toolchain from scratch in the container's `before_script`, so
it also serves as an executable, always-current recipe for building this project on a clean
Debian/Ubuntu system.

A separate `lint` stage runs first and needs only `python3` — no Arrow/Parquet/fpm/gfortran/git-lfs
setup — so it fails fast on the cheap checks. `tools/run_lint_check.sh` runs the same list locally;
the two are kept in sync by hand.

A few choices in that file are load-bearing — each cost a debugging round when it was wrong, so
preserve them if you touch it:

- **Base image `ubuntu:24.04`** (pinned with `image:`, since the runner's own default is older).
  24.04 is the oldest Ubuntu that satisfies *every* toolchain requirement at once: gfortran 13
  (gfortran ≤ 11 miscompiles the optional allocatable-`character` argument in `schema%add_col_qc` —
  see [README's Prerequisites](README.md#prerequisites)), a g++ new enough for C++20 / `std::span`,
  `pipx` in the repos, and current Arrow apt packages. Its default `gcov` also matches its default
  compiler, so `gcovr` needs no `--gcov-executable` override.
- **`git lfs pull`** before running tests. Without it the reader tests read LFS *pointer* files and
  fail. `git lfs install --skip-repo` sets up only the global filter config — CI never pushes, so
  the repo-local hooks are deliberately skipped.
- **`libarrow-compute-dev`** alongside `libarrow-dev` / `libparquet-dev`: Arrow ships its compute
  kernels separately and `fpm.toml` links `arrow_compute`. Omitting it fails the C++ compile on
  `arrow/compute/*.h`.
- **`FPM_FFLAGS="--coverage"`, and deliberately no `-fopenmp`.** Coverage instrumentation is not
  something any fpm metapackage supplies, so it must be explicit; the OpenMP flag is already
  injected by the metapackage and passing it again is redundant. See [CLAUDE.md](CLAUDE.md#dont-run-the-gitlab-ci-pipeline-yourself).
- **A pinned `gcovr` version range.** Two independent gcovr regressions bracket it — one below the
  floor cannot parse a 10,000+ line file's gcov output, one at or above the ceiling silently drops
  coverage for every Fortran module subroutine. Both symptoms look like a problem in this project
  and are not. See CLAUDE.md's two `gcovr` sections before widening either bound.

Coverage is computed by `gcovr` over `src/` and surfaced through GitLab's `coverage:` regex,
including `src/parquet_wrapper.cpp` alongside the Fortran sources — CI's `gfortran`/`gcc`/`g++` are
one matched apt GCC install, so `gcovr` reads both cleanly in a single pass. A dev machine's
`FPM_CXX` frequently is not matched that way (a default `clang++` against a GNU `gcov`), which is
why local Fortran and C++ coverage are two separate scripts, `tools/coverage.sh` and
`tools/coverage_cpp.sh`, rather than one with a flag.

**Reproducing the CI environment locally.** `tools/build_ci_test_image.sh` bakes everything
`.gitlab-ci.yml`'s `before_script` installs into a Docker image, and `tools/run_ci_test_image.sh`
runs this working tree against it. **Only the maintainer builds that image** — if you need it, or
need it changed, ask rather than building or modifying one yourself.

Docs are published by two separate jobs, not the `test` job above: `.gitlab-ci.yml`'s `readthedocs`
job (GitLab CI → gitlab.4most.eu's readthedocs-style docserver) and `.github/workflows/docs.yml`
(GitHub Actions → GitHub Pages). Both run `tools/fix_ford_page_links.sh` after `ford docs.md`.

### Mirroring to GitHub

This repository is developed on GitLab (`gitlab.4most.eu`), with a manually-synced read-only mirror
on GitHub (`github.com/etempel/parquet-fortran`) for publication purposes (e.g. the fortran-lang.org
package index, which expects a GitHub repo). There is no automated sync — push to the mirror by hand
whenever you want it updated:

```bash
git remote add github git@github.com:etempel/parquet-fortran.git   # one-time setup
tools/mirror_to_github.sh --github
```

README.md and `doc/pages/*.md` contain a handful of `gitlab.4most.eu`-specific links and badges that
need to point at `github.com` instead once mirrored. `tools/mirror_to_github.sh` handles this
automatically from a disposable local branch — so `main` never carries GitHub-targeted content, even
transiently — via `tools/prep_github_mirroring.sh` (see that script's header for the exact
mechanics, including its `--reverse` mode).

### Releasing

Bumping the version is currently a manual sequence across several files — nothing checks that all of
them were updated together except `parquet_get_version`'s own runtime drift warning (see below), so
work through this list in full for every release:

1. **`VERSION.txt`** — the source of truth. `fpm.toml`'s `version = "VERSION.txt"` and its
`RELEASE_VERSION={version}` macro derive from this automatically; nothing else to do for that part.
2. **`src/parquet_version.f90`'s `cversion`** — a hand-maintained `"vX.Y.Z (date)"` string (e.g.
`"v1.0.0 (2026-07-27)"`), used as `parquet_get_version`'s `mode="internal"` value. Update both the
version and the date. `parquet_get_version` compares the fpm-injected `RELEASE_VERSION` macro
against this string and prints a runtime warning if they disagree — the one part of this sequence
that is actually checked, but only after the fact (at a user's next run), not at release time. 3.
**`CHANGELOG.md`** — add a new `## [X.Y.Z] - YYYY-MM-DD` section (Keep-a-Changelog format, `###
Added`/`### Changed`/`### Fixed`/etc. grouping, matching the existing `[1.0.0]` section). If
`[Unreleased]` entries have accumulated since the last release (see `CLAUDE.md`'s changelog
guidance), retitle that section rather than starting a new one. 4. **A git tag** matching the
version (e.g. `v1.0.1`), on the commit that bumped `VERSION.txt`. 5. **`README.md`'s status line**
(`**Status: 1.0 — first stable release.**`, near the top) — only needs touching on a **major**
version bump (it deliberately names the series, not the patch level, specifically so routine `1.0.x`
releases don't need this step at all). 6. **`fpm publish`** — see "Publishing to the fpm registry"
below; this is a separate, deliberate step with its own prep script, not part of the version bump
itself.

None of this is automated or enforced by CI today — a future release-checklist script (comparing
`VERSION.txt` against `cversion` and the latest `CHANGELOG.md` heading, say) would close that gap,
but hasn't been written.

### Publishing to the fpm registry

Account/namespace/token setup and the general `fpm publish` workflow are documented upstream in
[fpm's registry publishing guide](https://fpm.fortran-lang.org/registry/publish.html) — this section
only covers what's specific to *this* repository, which needs a prep step first for two reasons
undocumented upstream:

- The registry [mandatorily enforces module naming](https://fpm.fortran-lang.org/registry/naming.html), but `fpm.toml` keeps `module-naming = false` on `main` since
  enabling it breaks local `fpm build`/`fpm test` — the `test-drive` dev-dependency's own modules
  don't comply, and fpm has no per-dependency exemption (fpm PR [#828](https://github.com/fortran-lang/fpm/pull/828) / issue #883; re-check whether this is still true for whatever fpm version
  you're publishing with).
- `fpm publish` packages git HEAD, not the working tree or even the staged index (confirmed by
  testing — uncommitted and staged edits are both silently ignored). Prep edits must be committed
  somewhere to take effect, without ever landing on `main`.

**`tools/prep_fpm_publish.sh`** handles both: commits a disposable local branch (`fpm-publish-prep`,
never pushed or merged) with `module-naming` enabled, `test-drive` commented out, GitHub-facing doc
links applied, and maintainer/CI-only files stripped — see the script's own header comment for the
exact file list and mechanics (keep it in sync per CLAUDE.md's "Keeping tools/prep_fpm_publish.sh in
sync"). It then runs fpm's token-free preview commands and self-checks the resulting tarball's
actual contents, exiting nonzero with the specific mismatch if anything's wrong.

`categories`/`keywords` in `fpm.toml` are free text with no registry-enforced vocabulary (confirmed
by reading the registry backend's source) — the current `categories = ["io"]` needs no change.

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

Publishing from a GitLab checkout or the GitHub mirror makes no difference — the disposable branch's
committed content determines what's published, not which remote you started from.

## Regenerating the built-in MAML module

Every `.maml` file under `schemas/` is compiled directly into this library by
`tools/generate_parquet_maml.sh`, which scans that directory recursively. Run it from the repository
root in `base` mode:

```bash
tools/generate_parquet_maml.sh base   # (re)generates src/parquet_maml_base.f90
```

`base` mode additionally defines the `parquet_maml_file`/`parquet_maml_missing_column`/
`parquet_maml_col_map_entry` derived types the library uses. The generated file carries a header
saying it is auto-generated — **do not hand-edit `src/parquet_maml_base.f90`**; edit the source
`.maml` files under `schemas/` and re-run. Note `schemas/` is for schemas describing files the
library *writes*; Role-A schemas that generate table types live in `table_types/` instead.

The script's other, no-argument mode is a separate, consumer-facing feature for embedding schemas in
a downstream project — see [Embedding your own MAML schemas](doc/pages/utilities/embedding-maml-schemas.md). See the script's own header for `--dir=` and the rest.

## Extending the MAML schema

`parquet_validate_maml` checks that section names and known sub-keys are registered in
[src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90) (`allowed_maml_sections`). This
checks key presence, not semantic value meaning. To allow a new top-level section or map-list sub-
key, add it there. Validation is strict for the known schema (`fields`, `keyarray`, `DOIs`, etc.),
permissive for `extra:`, and intentionally shallow beyond the explicitly registered nested blocks —
if you extend MAML structure in this library, update `allowed_maml_sections` and (where needed)
`allowed_maml_nested_sections` in [src/parquet_metadata_maml.f90](src/parquet_metadata_maml.f90).

## Error-handling conventions in `parquet_wrapper.cpp`

Prefer reporting a fatal condition directly — print a diagnostic to stderr and terminate — over
`throw`ing and `catch`ing within the same function. **Use `report_fatal_error(...)` for that, and
never call `std::abort()` or `exit()` yourself.** A new fatal path must go through
`claim_fatal_path_or_park()` and `fatal_exit()`, because a fatal error here can be reached by
several threads at once and `std::abort()` takes a lock inside glibc — threads arriving together
pile up on it and the process hangs forever instead of dying.

`try`/`catch` is also unreliable across one specific toolchain combination here (a `gfortran`-linked
executable on macOS breaks libc++abi's unwinding for `clang++`-compiled objects), which is the other
half of why the direct-report convention exists. Both are written up in full at the top of
`src/parquet_wrapper.cpp`, beside `claim_fatal_path_or_park` — read that before adding a fatal path.
See also `feature_risks.md` Risk-99.

## Features considered but not implemented

These were looked at during an audit comparing this library against Arrow C++'s broader feature set,
and deliberately deferred rather than rejected outright — worth revisiting if a concrete use case
needs them, rather than adding speculatively.

**Plausible future candidates, if needed:**

- **Per-column writer properties** (e.g. `disable_statistics()` for write-heavy throwaway files) —
  small, additive, doesn't touch the type system. One instance already exists but is automatic and
  type-based rather than caller-facing: see [Automatic BYTE_STREAM_SPLIT for float
  columns](CLAUDE.md#automatic-byte_stream_split-for-float-columns-in-the-writer). A manual
  per-column override for other types remains unimplemented.
- **`qc:`/`parquet_filter` support for `date`/`time`/`timestamp`** — neither quality-control
  range/miss checks nor row filtering is implemented for these three, unlike every other supported
  type.

**Bigger lifts, worth being cautious about:**

- **Predicate pushdown** (statistics-based I/O skipping) — *not to be confused with row filtering,
  which is already implemented*; see [Row
  filtering](doc/pages/io/filter-sort-sample.md#row-filtering-with-parquet_filter). The existing
  filter is post-decode: it narrows the rows your code sees but still reads and decodes every
  referenced column in full. Skipping non-matching row groups off disk entirely is the unimplemented
  part, and README's "No predicate pushdown" limitation treats it as an intentional non-goal for now.
- **`MAP` columns, and variable-length `LIST` nested inside a `STRUCT` path** — Arrow supports these
  natively, but they would break the library's core "flat columns + fixed `col_size` vectors" data
  model that the whole Fortran-side API is built around; this is a redesign, not an addition.
  (Top-level list-encoded vector columns and `STRUCT` columns are both already supported for
  reading, the latter at any nesting depth down to a scalar or `FIXED_SIZE_LIST` leaf.)
- **Additional scalar types on the *write* side** (`int8`/`int16`/unsigned integers/`decimal` as a
  MAML-declarable, `parquet_write_column`-writable `data_type`) — straightforward from Arrow's side,
  but each new type multiplies the `parquet_write_*` interface surface, since a dedicated subroutine
  pair already exists per supported type. Writing stays limited to the original six. The **read**
  side is different and did gain widening support, because it dispatches on the existing `values`
  array's Fortran kind rather than needing new public entry points — see [Supported data
  types](doc/pages/types/supported-data-types.md) for exactly which source types read into which
  target, which is the authoritative list. Deliberately excluded from that widening mechanism:
  date/time/interval-family types (semantically not raw numbers despite int32/int64 storage — the
  three temporal types have their own dedicated support instead, and `INTERVAL`/`DURATION` is a
  dropped non-goal, see [Not yet
  supported](doc/pages/types/date-time.md#not-yet-supported)) and nested/wrapper types, which have no
  scalar target at all.

**Probably out of scope for this library's design:**

- Arrow's `dataset` module (multi-file/partitioned scanning), encryption, and Flight — these serve a
  different usage pattern (distributed/columnar-service workloads) than a Fortran-side
  read/write-one-file library.

**Maintainability changes considered and declined:**
- Splitting the single `src/parquet_wrapper.cpp` translation unit into several smaller files. Looked
  at during a review pass and rejected: the file-scope `static` globals it carries (the `g_debug_*`
  test overrides, the settings mirrored from `parquet_settings`, the thread-token counter and the
  fatal-path claim flag) would each silently become one copy *per translation unit*, which compiles,
  links, and breaks with no diagnostic anywhere; ~90 `static` helpers would need their linkage
  reworked; the coverage tooling is keyed to this one filename; and the large Arrow include preamble
  would be duplicated, so compile time would likely *increase*. Instead the file gained `// ==== ...
  ====` section banners, which give most of the navigational benefit at none of the cost. **Read
  [CLAUDE.md](CLAUDE.md#if-srcparquet_wrappercpp-is-ever-split-into-multiple-translation-units)
  before revisiting this** — it enumerates every global that would have to become a genuine `extern`
  first, and what each one breaks if it is missed. Revisit only on a concrete trigger: compile time
  becoming a real irritant, or a genuinely independent new subsystem with no shared helpers, which
  is the one case where a second `.cpp` is cheap.
