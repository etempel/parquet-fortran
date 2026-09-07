# Instructions for Claude

This file is the entry point for working in this repository. The rules themselves live in
`.claude/rules/*.md`, one file per area, all loaded automatically. This file holds only how those
files are written and where each area lives.

## Contents

- [How these instructions are written](#how-these-instructions-are-written)
- [Where the rules live](#where-the-rules-live)
- [Headings kept for links from other documents](#headings-kept-for-links-from-other-documents)
  - [New features require tests and docs](#new-features-require-tests-and-docs)
  - [CONTRIBUTING.md is project-wide workflow ONLY — a tool's own detail goes in its header](#contributingmd-is-project-wide-workflow-only--a-tools-own-detail-goes-in-its-header)
  - [A guide page describes the CURRENT state, never a former one](#a-guide-page-describes-the-current-state-never-a-former-one)
  - [Naming conventions](#naming-conventions)
  - [Don't run the GitLab CI pipeline yourself](#dont-run-the-gitlab-ci-pipeline-yourself)
  - [Automatic BYTE_STREAM_SPLIT for float columns in the writer](#automatic-byte_stream_split-for-float-columns-in-the-writer)
  - [A test that asserts THREADING must skip without OpenMP](#a-test-that-asserts-threading-must-skip-without-openmp)
  - [If `src/parquet_wrapper.cpp` is ever split into multiple translation units](#if-srcparquet_wrappercpp-is-ever-split-into-multiple-translation-units)
  - [A static check that enumerates names goes stale silently](#a-static-check-that-enumerates-names-goes-stale-silently)
  - [Verifying a change with mutation testing](#verifying-a-change-with-mutation-testing)
  - [Instrument phases before optimising a multi-phase operation](#instrument-phases-before-optimising-a-multi-phase-operation)

## How these instructions are written

Every file under `.claude/rules/`, and this one, follow the same principles. Apply them to every
addition:

- **Compact rules and strict guidelines only.** State what to do, what not to do, where a thing
  lives and which check or test enforces it. No background, no history, no justification, no
  measurements, no account of how a rule was discovered. A one-clause reason is allowed only where
  the rule would otherwise be misapplied or reverted.
- **One home per fact.** Before adding anything, grep `.claude/rules/` for the topic; extend the
  entry that owns it, or cross-reference it by file name. Never restate a rule in a second file.
- **File by area.** A new rule goes in the file whose table row below covers it; a compiler trap
  goes in `fortran-gotchas.md` under the compiler that exhibits it (the general group when it binds
  regardless of compiler); a C++-side rule in `cpp-wrapper.md`. Create a new file only for a new
  area, and add its row below.
- **Forward-looking only.** These files are not a changelog or session log; git history and
  `CHANGELOG.md` record what happened when, and `feature_risks.md` records the silent-failure
  properties of specific areas with their test status.
- **Cite what the repository can verify**: the lint check, the test, the procedure, the
  `feature_risks.md` entry. Never write a count or a list the repository owns; point at the source
  and re-derive before quoting.
- **Scope a file-anchored rule with `paths:` frontmatter**; it is loaded when a matching file is
  read. A rule needed before any file is opened, or triggered by a task rather than a file (a NAG
  build, a benchmark campaign), carries no frontmatter and loads always. The Loaded column below says
  which is which. **When a task concerns a scoped area and no matching file has been read yet, open
  that rule file first.**
- **Keep this file short**, keep its Contents ToC in step with its headings, and run
  `tools/check_doc_anchors.py` after editing a heading here (other documents link into it).

## Where the rules live

| File | Covers | Loaded |
|---|---|---|
| `.claude/rules/workflow.md` | Guardrails: scope of edits, no commits on `main`, report before implementing, CI and the CI image, `feature_*.md` and the `feature_risks.md` register, scripted edits | always |
| `.claude/rules/documentation.md` | Tests + docs + CHANGELOG obligation, CHANGELOG rules, the three doc layers, CONTRIBUTING.md index rule, current-state rule, anchor checking, FORD conventions and config | always |
| `.claude/rules/code-style.md` | Files and program units, generated files, interface blocks and submodule rules, repository layout | always |
| `.claude/rules/module-structure.md` | The module tree, entry modules, tiers and footprints, the facades, `parquet_toml`, `parquet_random`/`parquet_sampling`, random word spaces, sorting tiers | always |
| `.claude/rules/api-conventions.md` | Naming, int32/int64 arguments, errors and diagnostics, mutation guards and finalizers, settings admission and knob rules, thread counts, reader queries for siblings | always |
| `.claude/rules/reader-writer.md` | Filter evaluation, statistics screen, row transforms, sort engine, nullability contract, BYTE_STREAM_SPLIT, validity masks, width measurement, row-group-scoped reads, string I/O, temporal units, schemas and MAML sources | when a core, read, write or metadata source, the C++ wrapper, a MAML fixture or one of their tests is read |
| `.claude/rules/columns-tables.md` | `parquet_column` (trimming, per-element validity, growth, paste/adopt), typed accessor tiers, `parquet_strings`, temporal elements and containers, table concurrency, pointers and detach, cache state | when a columns, tables, strings, temporal or container source, one of their generators, a Role-A MAML or one of their tests is read |
| `.claude/rules/build.md` | `fpm` flags and profiles, stale build cache, hand-run compiles, flang and macOS facts, LTO, `-fPIC` | always |
| `.claude/rules/fortran-gotchas.md` | Language and compiler traps: general, gfortran, ifx, flang, nagfor | always |
| `.claude/rules/nagfor-builds.md` | Running a NAG build, warning triage, `-thread_safe`, the checked profiles, the `-C=undefined` runners | always (task-triggered; no file anchors it) |
| `.claude/rules/cpp-wrapper.md` | `src/parquet_wrapper.cpp`: no OpenMP, templates, fatal paths, single TU, Arrow singletons, mirrored settings, `bind(C)` checking, debug hooks, int32 ceilings | when the C++ wrapper, the bindings, `parquet_settings.f90` or the boundary checker is read |
| `.claude/rules/testing.md` | Running tests, error scenarios, concurrency, assertions, threading skips, mutation testing, static checks, `tools/*.sh`, debug hooks | when anything under `test/`, a `tools/*.sh` or a `tools/check_*.py` is read |
| `.claude/rules/coverage.md` | Measuring coverage, gcovr bounds, closing gaps, gcov attribution artifacts (Fortran and C++) | when a coverage script, `.gitlab-ci.yml` or the C++ wrapper is read |
| `.claude/rules/benchmarking.md` | `bench/` layout, cross-machine campaigns, measurement rules, memory, phase instrumentation | when anything under `bench/`, the machine/LTO scripts, `tools/developer_environments.md` or a `feature_benchmark*.md` is read |

Maintainer notes outside the rules: `tools/developer_environments.md` (machines and toolchain
activation), `bench/benchmark_template.md` (run-sheet template). `/review_doc`
(`.claude/commands/review_doc.md`) initialises a documentation review campaign and carries the
review procedure its per-page sessions follow.

If this repository is checked out inside a larger workspace, `../../fortran/CLAUDE.md` may hold
conventions shared across sibling Fortran projects; read it when present. This repository's rules
win on conflict.

## Headings kept for links from other documents

CONTRIBUTING.md and planning documents link to the anchors below; each points at the rule's home.
Remove a stub only after every link to it is repointed (`tools/check_doc_anchors.py` lists them).

### New features require tests and docs

`.claude/rules/documentation.md`, "New features require tests, docs and a CHANGELOG entry".

### CONTRIBUTING.md is project-wide workflow ONLY — a tool's own detail goes in its header

`.claude/rules/documentation.md`, "CONTRIBUTING.md is project-wide workflow only".

### A guide page describes the CURRENT state, never a former one

`.claude/rules/documentation.md`, under the same heading.

### Naming conventions

`.claude/rules/api-conventions.md`, "Naming".

### Don't run the GitLab CI pipeline yourself

`.claude/rules/workflow.md`, "CI and the CI image"; the local verification flags are in
`.claude/rules/build.md`.

### Automatic BYTE_STREAM_SPLIT for float columns in the writer

`.claude/rules/reader-writer.md`, "BYTE_STREAM_SPLIT for float columns".

### A test that asserts THREADING must skip without OpenMP

`.claude/rules/testing.md`, "A test that asserts THREADING skips without OpenMP".

### If `src/parquet_wrapper.cpp` is ever split into multiple translation units

`.claude/rules/cpp-wrapper.md`, "Rules for the file".

### A static check that enumerates names goes stale silently

`.claude/rules/testing.md`, "Static checks".

### Verifying a change with mutation testing

`.claude/rules/testing.md`, "Mutation testing".

### Instrument phases before optimising a multi-phase operation

`.claude/rules/benchmarking.md`, "Instrumenting phases".
