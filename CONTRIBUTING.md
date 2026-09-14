# Contributing to parquet-fortran

This file covers developing, testing, and extending this repository itself. If you just want to
*use* the library from your own project, see [README.md](README.md) instead.

**Scope.** This file holds *project-wide* workflow: the conventions everyone follows, and how to
build, test, lint, release and publish. It deliberately does **not** describe how any individual
tool or program works — that lives in each file's own header comment, next to the code it describes.
The rule is in `.claude/rules/documentation.md`.

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
- [Regenerating the built-in MAML module](#regenerating-the-built-in-maml-module)
- [Extending the MAML schema](#extending-the-maml-schema)
- [Error-handling conventions in `parquet_wrapper.cpp`](#error-handling-conventions-in-parquet_wrappercpp)
- [Features considered but not implemented](#features-considered-but-not-implemented)

## AI assistance

Portions of this codebase, including its documentation, were developed with AI assistance (Claude
Code). The instructions Claude Code works from live in [CLAUDE.md](CLAUDE.md) and under `.claude/`
(rules and skills); they are not part of the contributor workflow described here.

## Conventions

Project conventions are recorded in `.claude/rules/` and indexed by [CLAUDE.md](CLAUDE.md). Read
`workflow.md`, `code-style.md`, `api-conventions.md` and `documentation.md` there before a first
change, and `feature_risks.md` (this repository's open-risks register) before editing an area it
covers. The mechanical conventions are enforced by `tools/run_lint_check.sh`, the same checks CI's
lint stage runs.

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
`run_parquet_fortran` and is the only executable meant for a consumer of this library. Every
benchmark and probe program lives in
`bench/` beside the wrapper that drives it, described in its own header comment. Anything needing
more memory, disk or time than `fpm test` should ever attempt belongs there, never under `test/`.

`fpm.toml` reaches `bench/` through a single `[[executable]]` block: `auto-executables = true`
scans `app/` only, and naming one `bench/` program registers that whole directory for
auto-discovery. **A file placed in `bench/` is therefore compiled by fpm** — which is why the four
standalone drivers that must be built by a bare compiler with forced flags
(`tools/check_random_kernels.f90`, `tools/check_exp_key.f90`, `tools/check_argsort_standalone.f90`,
`tools/benchmark_random_kernels.f90`) stay in `tools/`, outside any source-dir.

**If `fpm test` seems to run old code after a source change**, run `fpm clean --skip` before
debugging; `.claude/rules/build.md` covers the build cache and the flag rules. Note `fpm build`
does **not** build anything under `test/`; use `fpm build --tests`.

### Building with link-time optimisation

No fpm profile enables it. Use `tools/fpm_lto.sh` (sourced, not executed): it adds the LTO flag for
`--profile release`, selects the plugin-capable archiver and the linker LTO needs, and refuses to
build rather than hand back a silently LTO-less binary. Its header carries the details.

### Running a single test suite/test

**The suites are split across five runner programs**, so the first thing to know is which one owns
the suite you want. `tools/count_tests.sh` prints the whole map — every suite, its runner, its test
count and the file it lives in — which is the authority; the table below is orientation only.

| runner | what it holds | runs under `-C=undefined` | forks subprocesses |
|---|---|---|---|
| `run_tester_pf` | the `pf_`-prefixed utility tier: statistics, random/sampling, spatial, HEALPix, logging, path helpers | yes | no |
| `run_tester` | the Arrow-free parquet-domain modules: string columns, the container types, temporal | yes | no |
| `run_tester_noundef` | Arrow-free, but empirically not undef-safe — a curated list, see below | no | no |
| `run_tester_cpp` | everything reaching the C++/Arrow layer | no | no |
| `run_tester_errors` | everything driving an `error_scenarios` subprocess | no | **yes** |

While iterating on one area, prefer scoping `fpm test` to just the relevant suite (or a single named
test within it) over running the full suite every time:

```bash
fpm test run_tester_cpp -- reading                   # one suite
fpm test run_tester_cpp -- reading "test name"       # one test within a suite
fpm test run_tester_errors -- reading_errors         # that suite's abort-path tests
```

A plain `fpm test` runs every runner, so the split changes nothing about the full run.

**The abort-path tests no longer sit beside the tests they are about.** A test asserting that a bad
write aborts is a test about writing, and it now lives in `writing_errors` rather than `writing`.
That is deliberate: it is what makes every runner except `run_tester_errors` a program that forks
nothing, which is what lets the two undef-safe runners exist at all.

`tools/check_source_conventions.py`'s `check_test_runner_partition` enforces the split in CI: every
suite in exactly one runner, no undef-safe runner reaching `parquet_bindings` or declaring its own
`bind(C)`, and no runner but `run_tester_errors` driving a scenario.

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

One fixture comes from elsewhere: **`test/fixtures/pandas_written.parquet` is written by pandas
itself**, via `tools/generate_pandas_fixture.py`, because what it pins is *provenance* — that a real
pandas release, given an ordinary DataFrame and no Parquet-specific arguments, produces exactly the
shapes the reader tests expect (a `category` as an Arrow dictionary over strings, a plain string
column as `large_string`, a non-default index as a `__index_level_0__` column). The C++ generator can
imitate those types but cannot testify that pandas produces them. It needs pandas and pyarrow, so
run it by hand under the `astro` environment; `--check` compares the committed file's schema and
values with what the current environment writes, without overwriting it:

```bash
conda activate astro && tools/generate_pandas_fixture.py --check
```

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
is maintainer-only unless marked **Consumer-facing** — everything in `bench/` without exception,
and everything in `tools/` but the generators, converters and build shims a downstream project
runs itself. The tables below carry the marking; do not count them out here, where the number
goes stale the next time one is added.

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
| `generate_parquet_stats.py` | The `parquet_stats` per-kind blocks. |
| `generate_parquet_ziggurat.py` | `src/parquet_ziggurat.f90`'s 771 layer constants, re-derived rather than pasted. |
| `generate_parquet_maml.sh` | The built-in MAML module — see [below](#regenerating-the-built-in-maml-module). **Consumer-facing.** |
| `generate_user_table_code.py` | A Role-A MAML into a named `parquet_table` extension type. **Consumer-facing.** |
| `generate_random_golden_vectors.py` | The golden vectors freezing `parquet_random`'s bit contract. |
| `generate_probit_reference.py` | `pf_probit` and the normal family's golden expectations, from a 50-digit `mpmath` oracle. |
| `generate_healpix_reference.py` | The reference vectors pinning `parquet_healpix` against an independent model. |
| `generate_path_reference.py` | The CPython `posixpath` values pinning `parquet_utils`' path procedures. |
| `check_healpix_fptrap.sh` | Runs `parquet_healpix` in a build where an FP exception is fatal. |
| `generate_random_perm_vectors.py` | The same for the permutation contract. |
| `generate_stats_vectors.py` | `parquet_stats`' golden expectations, from a 50-digit `mpmath` oracle. |
| `generate_fixtures.cpp` | The hand-built Arrow fixtures — see [above](#regenerating-the-test-fixtures). |
| `generate_pandas_fixture.py` | The one fixture pandas itself writes — see [above](#regenerating-the-test-fixtures). |
| `run_generate_fixtures.sh` | Builds and runs that generator. |
| `generate_logo_svg.py` | Regenerates `doc/media/logo.*`. |

#### `tools/` — environment, build and release

| tool | what it is for |
|---|---|
| `machine_report.sh` | Identifies a machine and its toolchain. Run it at the start of every benchmarking run. |
| `fpm_lto.sh` | **Sourced, not executed** — an LTO-aware `fpm` wrapper. See [above](#building-with-link-time-optimisation). |
| `nagfor_fpm_shim/nagfor` | Makes `fpm build` and `fpm install` work with NAG despite fpm 0.13's NAG link-line defects. **Consumer-facing** — a project that merely *depends* on this library cannot build it under NAG without the shim on `PATH`. |
| `build_ci_test_image.sh` | Bakes the CI environment into a local Docker image. **Maintainer-only** — see below. |
| `run_ci_test_image.sh` | Runs this working tree against that image. |
| `prep_github_mirroring.sh` | Rewrites GitLab-specific links and badges for the GitHub mirror. |
| `mirror_to_github.sh` | Drives that rewrite and pushes the mirror. |
| `fix_ford_page_links.sh` | Repoints `doc/pages/*.md` links FORD does not resolve in embedded markdown. |

#### `tools/` — utilities and conversion

| tool | what it is for |
|---|---|
| `convert_fits_to_parquet.py` | A FITS binary table into a `.parquet` file. **Consumer-facing.** |
| `parquet_metadata_to_md.py` | A `.parquet` file's metadata as markdown, via `pyarrow`. **Consumer-facing.** |
| `philox_reference.py` | An independent Philox model, for `check_philox_compliance.sh`. |
| `count_lines.py` | Code/comment/blank counts per source group, Markdown counts, and a code-vs-documentation summary. |
| `count_tests.sh` | Unit tests per suite, read from source without building. |
| `check_nag_undefined.sh` | Runs the undef-safe test runners under nagfor's `-C=undefined`. |

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
| `benchmark_spatial.sh` | `pf_spatial_index`: how good the chosen cell size is, what the probe costs, how a bulk sweep scales, and the 3D grid against the HEALPix backend on the sky. |
| `benchmark_stats.sh` | `parquet_stats`: the moment engine against the memory floor, where its two passes go, what each pass-one shape costs, the ceiling on what threading pass two could return, where a team starts paying on the shipped call (`--mode=teamsweep`, which is what `STATS_MIN_PER_THREAD` is derived from -- `--mode=thread` reports a ceiling and `--mode=library` is censored by the constant under test), whether an order statistic does better selecting or sorting, and what a robust scale estimator costs (each with a bit-exactness gate). |
| `benchmark_healpix.sh` | `parquet_healpix`: scalar entry-point cost, bulk thread scaling, the array size at which threading first pays, `pf_query_disc` in microseconds per disc, (`--mode=grid`) every `pf_healpix_grid` binding against the free procedure it delegates to, and (`--mode=dist`) the three costs that decide whether to convert RA/Dec to vectors before repeatedly measuring separations, with the break-even printed. Links no HEALPix, so any machine can reproduce it. |
| `benchmark_index.sh` | Times `pf_index_map` lookups per backend and key pattern, build cost serial against threaded (the hash backend's partitioned insert included), the guarded mutation surface under contention, `pf_index_pool`, and (`--mode=multimap`) `pf_index_multimap`'s build, `%get_first_many` and `%probe_many` beside `pf_match_all` over the same arrays. Drives `bench/benchmark_index.f90`. |
| `benchmark_filter_set.sh` | The set-valued filter clause (`id in @set`): the leaf's pre-evaluation against the row-group decode it precedes, on a sorted and a shuffled file, through the reader and the whole and `bounded=` table reads, for a dense and a sparse set; and the map's three backends over the same sets in isolation. Drives `bench/benchmark_filter_set.f90`. |
| `benchmark_join.sh` | `parquet_table%join`: every timed join under the sort engine and the hash engine side by side (`ENGINE=`), the lookup-table shape decomposed into its own phases through the public API, a size sweep, every `how`, the per-payload-column rewrite cost with the rewrite's team level swept (the library's own choice against each level forced), and the string null-fill sweep beside its no-string control and the `%fillna`/`%ffill` verbs. Drives `bench/benchmark_join.f90`. |
| `benchmark_join_crosslib.sh` | `parquet_table%join` against **pandas, polars, astropy and STILTS** over the same parquet files: every join kind compared row for row (correctness), the places the five are entitled to disagree (null keys), the properties only this library promises (row order, detaching, `matched=`, `pairs=`, `require=`, `max_rows=`), read/join/write timed separately on five shapes, the large outputs compared as row-pair multisets, and the harness's own run-to-run floor on its own fixtures (`--stages=` selects which of the six run). Needs pandas, polars, astropy, pyarrow and a `stilts` on PATH. |
| `benchmark_join_crosslib.py` | The worker `benchmark_join_crosslib.sh` drives: owns the fixtures, the pandas/polars/astropy/STILTS arms, the row-for-row and pair-multiset comparisons and the report. Not run directly. |
| `bench_resolve_ladder.py` | Compile-out ladder over `table_resolve` — attributing per-cell cost without timers. |
| `run_practrand.sh` | Runs the PractRand battery over one axis of `parquet_random`. |
| `large_scale.sh` | Manual large-scale check — genuinely exceeding `huge(1)` rows. Never run by `fpm test` or CI. |
| `random_large_fill.sh` | The same, for the bulk random fills. |

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
zero. See `.claude/rules/testing.md`.

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
  injected by the metapackage and passing it again is redundant. See `.claude/rules/build.md`.
- **A pinned `gcovr` version range.** Two independent gcovr regressions bracket it — one below the
  floor cannot parse a 10,000+ line file's gcov output, one at or above the ceiling silently drops
  coverage for every Fortran module subroutine. Both symptoms look like a problem in this project
  and are not. See `.claude/rules/coverage.md` before widening either bound.

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
that is actually checked, but only after the fact (at a user's next run), not at release time.
3. **`CHANGELOG.md`** — add a new `## [X.Y.Z] - YYYY-MM-DD` section (Keep-a-Changelog format, `###
Added`/`### Changed`/`### Fixed` grouping, matching the existing `[1.0.0]` section, including its
**Toolchain floor** / **SemVer scope** / **Compatibility** notice). If `[Unreleased]` entries have
accumulated since the last release (see `CLAUDE.md`'s changelog guidance), retitle that section
rather than starting a new one, and add its `[X.Y.Z]:` link reference at the foot of the file.
4. **A git tag** matching the version (e.g. `v1.0.1`), on the commit that bumped `VERSION.txt`.
5. **Push the tag, and mirror to GitHub** — see [Mirroring to GitHub](#mirroring-to-github). The
GitHub repository is this project's distribution route; it is not published to the fpm registry
(see [Why not the fpm registry?](#why-not-the-fpm-registry) below).

None of this is automated or enforced by CI today — a future release-checklist script (comparing
`VERSION.txt` against `cversion` and the latest `CHANGELOG.md` heading, say) would close that gap,
but hasn't been written.

### Why not the fpm registry?

**This project is distributed by git, from GitHub and from gitlab.4most.eu, and is not published to
the fpm registry.** A consumer depends on it with a `git`/`tag` entry, as README's quickstart shows.

The registry
[mandatorily enforces module naming](https://fpm.fortran-lang.org/registry/naming.html), and fpm has
no per-dependency exemption for a dependency whose modules do not comply (fpm PR
[#828](https://github.com/fortran-lang/fpm/pull/828) / issue #883). Both of this project's Fortran
package dependencies are in that position — `test-drive` and `toml-f` — and while a dev-dependency
can be commented out for a publish, `toml-f` cannot: `src/parquet_toml.f90` imports it, so it ships.
A prep script used to work around the dev-dependency half by committing a disposable branch; it was
retired when `toml-f` made the registry route impossible rather than merely awkward, and its one
remaining index row says so.

Nothing is lost. A git dependency needs no registry account, pins exactly, and is what every
consumer of this library already uses.

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

Report a fatal condition through `report_fatal_error(...)`; never call `std::abort()` or `exit()`
yourself, and route any new fatal path through `claim_fatal_path_or_park()` and `fatal_exit()`. The
rules for the C++ side are in `.claude/rules/cpp-wrapper.md` and at the top of
`src/parquet_wrapper.cpp` beside `claim_fatal_path_or_park`.

## Features considered but not implemented

These were looked at during an audit comparing this library against Arrow C++'s broader feature set,
and deliberately deferred rather than rejected outright — worth revisiting if a concrete use case
needs them, rather than adding speculatively.

**Plausible future candidates, if needed:**

- **Per-column writer properties** (e.g. `disable_statistics()` for write-heavy throwaway files) —
  small, additive, doesn't touch the type system. One instance already exists but is automatic and
  type-based rather than caller-facing: see the BYTE_STREAM_SPLIT rule in `.claude/rules/reader-writer.md`. A manual
  per-column override for other types remains unimplemented.
- **`qc: min:`/`max:` bounds for `date`/`time`/`timestamp`** — a field declaring one fails
  `parquet_validate_maml` rather than being silently ignored; see [Quality
  control](doc/pages/schema/quality-control.md). `qc: miss:` and row filtering on these three
  column types *are* implemented — see [Filtering `date`, `time` and `timestamp`
  columns](doc/pages/io/filter-sort-sample.md#filtering-date-time-and-timestamp-columns).

**Bigger lifts, worth being cautious about:**

- **Finer-grained predicate pushdown** — page-level pruning and bloom filters. Row-group-level
  pushdown *is* implemented: a filtered reader consults each row group's footer statistics and never
  reads a row group that provably cannot match, for the filter's own columns and for every column
  read afterwards; see [Row groups a filter cannot match are never
  read](doc/pages/io/filter-sort-sample.md#row-groups-a-filter-cannot-match-are-never-read). Within
  a surviving row group nothing is skipped, which is what README's Limitations section describes.
- **Writing a nested container** — a list of structs, a map of structs, or a struct with a list or
  map field all *read* (through `parquet_list`/`parquet_map`/`parquet_struct`, reached one level at
  a time with `%nested`), and writing one is refused with a message naming the column and the
  offending field or element kind. The read path needed no new data model because a container's
  payload is an ordinary `parquet_column` carrying a `class(parquet_container_column)` slot; the
  write path is a genuine lift, because a nested field has to cross the `bind(C)` boundary as a
  staged tree rather than as one buffer per column.
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
  ====` section banners, which give most of the navigational benefit at none of the cost. **Read `.claude/rules/cpp-wrapper.md` before revisiting this** — it enumerates every global that would have to become a genuine `extern`
  first, and what each one breaks if it is missed. Revisit only on a concrete trigger: compile time
  becoming a real irritant, or a genuinely independent new subsystem with no shared helpers, which
  is the one case where a second `.cpp` is cheap.
