# Instructions for Claude

This file is the entry point for working in this repository. The rules live in
`.claude/rules/*.md`, one file per area, loaded automatically; the on-demand procedures live in
`.claude/skills/<name>.md`, invoked as `/<name>`. This file holds only how those files are
written and where each area lives.

## Contents

- [How these instructions are written](#how-these-instructions-are-written)
- [Where the rules live](#where-the-rules-live)
- [Skills](#skills)
- [Headings kept for links from other documents](#headings-kept-for-links-from-other-documents)
  - [A static check that enumerates names goes stale silently](#a-static-check-that-enumerates-names-goes-stale-silently)
  - [Verifying a change with mutation testing](#verifying-a-change-with-mutation-testing)
  - [Instrument phases before optimising a multi-phase operation](#instrument-phases-before-optimising-a-multi-phase-operation)

## How these instructions are written

Every file under `.claude/`, and this one, follow the same principles. Apply them to every
addition:

- **Compact rules and strict guidelines only.** State what to do, what not to do, where a thing
  lives and which check or test enforces it. No background, no history, no justification, no
  measurements, no account of how a rule was discovered. A one-clause reason is allowed only where
  the rule would otherwise be misapplied or reverted.
- **One home per fact.** Before adding anything, grep `.claude/` for the topic; extend the entry
  that owns it, or cross-reference it by file or skill name. Never restate a rule in a second file,
  and never keep a document in two places.
- **File by area.** A new rule goes in the file whose table row below covers it; a compiler trap
  goes in `fortran-gotchas.md` under the compiler that exhibits it (the general group when it binds
  regardless of compiler); a C++-side rule in `cpp-wrapper.md`. Create a new file only for a new
  area, and add its row below.
- **A procedure run on demand is a skill, not a rule**: a build, a campaign, a scenario, a triage.
  It lives in `.claude/skills/<name>.md` with a hyphenated name, frontmatter (`name`,
  `description`, `argument-hint`, `allowed-tools`, `disable-model-invocation: true`), does what its
  description says and stops. The rules it applies stay in `.claude/rules/`; the skill points at
  them rather than restating them.
- **Forward-looking only.** These files are not a changelog or session log; git history and
  `CHANGELOG.md` record what happened when, and `feature_risks.md` records the silent-failure
  properties of specific areas with their test status.
- **Cite what the repository can verify**: the lint check, the test, the procedure, the
  `feature_risks.md` entry. Never write a count or a list the repository owns; point at the source
  and re-derive before quoting.
- **Scope a file-anchored rule with `paths:` frontmatter**; it is loaded when a matching file is
  read. A rule needed before any file is opened, or triggered by a task rather than a file, carries
  no frontmatter and loads always. The Loaded column below says which is which. **When a task
  concerns a scoped area and no matching file has been read yet, open that rule file first.**
- **Keep this file short**, keep its Contents ToC in step with its headings, and run
  `tools/check_doc_anchors.py` after editing a heading here (other documents link into it).
- CONTRIBUTING.md is for human contributors and carries no instruction for Claude; anything of
  that kind goes under `.claude/`.

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
| `.claude/rules/developer-environments.md` | The three machines: hardware, toolchains, activation commands, per-machine traps, the Python environment | when a `bench/` file, the machine, LTO, UBSan or NAG scripts, or a `feature_benchmark*.md` is read; the `/nag-build` and `/plan-benchmark` skills read it first |
| `.claude/rules/fortran-gotchas.md` | Language and compiler traps: general, gfortran, ifx, flang, nagfor | always |
| `.claude/rules/cpp-wrapper.md` | `src/parquet_wrapper.cpp`: no OpenMP, templates, fatal paths, single TU, Arrow singletons, mirrored settings, `bind(C)` checking, debug hooks, int32 ceilings | when the C++ wrapper, the bindings, `parquet_settings.f90` or the boundary checker is read |
| `.claude/rules/testing.md` | Running tests, error scenarios, concurrency, assertions, threading skips, mutation-test rules, static checks, `tools/*.sh`, debug hooks | when anything under `test/`, a `tools/*.sh` or a `tools/check_*.py` is read |
| `.claude/rules/coverage.md` | Measuring coverage, gcovr bounds, closing gaps, gcov attribution artifacts (Fortran and C++) | when a coverage script, `.gitlab-ci.yml` or the C++ wrapper is read |
| `.claude/rules/benchmarking.md` | `bench/` layout, cross-machine campaigns, measurement rules, memory, phase instrumentation | when anything under `bench/`, the machine/LTO scripts or a `feature_benchmark*.md` is read |

## Skills

Each is invoked by the maintainer, does what its description says, and stops; none commits.

| Skill | Does |
|---|---|
| `/feature-request` | Writes the design document `feature_<name>.md` for a requested feature (scope, placement, API, settings analysis, risks, tests, docs, plan, open questions); implements nothing |
| `/review-doc` | Initialises a documentation review campaign over the `[Unreleased]` changes (`feature_doc.md`) and carries the per-page review procedure |
| `/plan-benchmark` | Writes a cross-machine run sheet (`feature_benchmark_<name>.md` plus per-machine copies) from the template it carries |
| `/nag-build` | Builds and tests with nagfor under a chosen profile and triages its diagnostics |
| `/add-error-scenario` | Adds an out-of-process error scenario, its wrapper and its list entry, and proves it against a broken implementation |
| `/mutation-test` | Breaks a behaviour deliberately and confirms the named test fails, with snapshot-based restore |
| `/coverage-gaps` | Measures coverage and classifies every uncovered range and stale exclusion, proposing what closes each |

## Headings kept for links from other documents

Planning documents link to the anchors below; each points at the rule's home. Remove a stub only
after every link to it is repointed (`tools/check_doc_anchors.py` lists them).

### A static check that enumerates names goes stale silently

`.claude/rules/testing.md`, "Static checks".

### Verifying a change with mutation testing

`.claude/rules/testing.md`, "Mutation testing", and the `/mutation-test` skill.

### Instrument phases before optimising a multi-phase operation

`.claude/rules/benchmarking.md`, "Instrumenting phases".
