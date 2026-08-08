# Instructions for Claude

This file is forward-looking: it captures durable conventions, gotchas, and guardrails to help
maintain this library and develop new features going forward. It is not a changelog or session
log — do not add entries describing how or when a specific feature was implemented, what a past
session investigated, or a chronological record of development. Only add generalizable guidance
that will still be correct and actionable for a future task, independent of which session
produced it (git history/commit messages are the right place for "what happened when").

**If checked out inside a larger workspace** (e.g. alongside sibling Fortran projects under a
shared `fortran/` directory), check whether `../../fortran/CLAUDE.md` exists and read it too — it
captures conventions shared across those projects (workflow guardrails, documentation/FORD
conventions, generic Fortran/fpm gotchas, testing & coverage conventions) that apply here as well,
unless this file says otherwise. This repository is also developed and used completely standalone,
so that file won't always exist — treat it as supplementary, not required.

## Contents

This file is a reference, not a start-to-finish read — jump to the note you need. Keep this ToC
in sync when adding, removing, renaming, or reordering a heading (see "Documentation structure"'s
working rules).

- [Workflow & guardrails](#workflow--guardrails)
  - [Only modify files inside this repository](#only-modify-files-inside-this-repository)
  - [Report before implementing on analysis/audit requests](#report-before-implementing-on-analysisaudit-requests)
  - [Only apply low-blast-radius renames/refactors](#only-apply-low-blast-radius-renamesrefactors)
  - [`feature_*.md` planning documents](#feature_md-planning-documents)
  - [The `feature_risks.md` standing-risks register](#the-feature_risksmd-standing-risks-register)
  - [Don't run the GitLab CI pipeline yourself](#dont-run-the-gitlab-ci-pipeline-yourself)
  - [The CI-environment Docker image: ask for it, never build it](#the-ci-environment-docker-image-ask-for-it-never-build-it)
  - [Don't commit or push on the main/default branch yourself](#dont-commit-or-push-on-the-maindefault-branch-yourself)
- [Documentation conventions](#documentation-conventions)
  - [New features require tests and docs](#new-features-require-tests-and-docs)
  - [Documentation structure](#documentation-structure)
  - [Checking documentation links](#checking-documentation-links)
  - [FORD doc-comment conventions](#ford-doc-comment-conventions)
  - [FORD config gotchas](#ford-config-gotchas)
- [Source code structure & conventions](#source-code-structure--conventions)
  - [One program unit per file; filename == unit name](#one-program-unit-per-file-filename--unit-name)
  - [Some `src/*.f90` files are generated — edit the generator, never the output](#some-srcf90-files-are-generated--edit-the-generator-never-the-output)
  - [Nested submodule tree](#nested-submodule-tree)
  - [Group interface bodies into commented `interface` blocks](#group-interface-bodies-into-commented-interface-blocks)
  - [A module procedure cannot implement its own submodule's spec-declared interface](#a-module-procedure-cannot-implement-its-own-submodules-spec-declared-interface)
  - [Naming conventions](#naming-conventions)
  - [Public numeric arguments: provide both int32 and int64 kinds](#public-numeric-arguments-provide-both-int32-and-int64-kinds)
  - [A new process-global parameter goes in `parquet_settings`](#a-new-process-global-parameter-goes-in-parquet_settings-and-a-design-doc-must-say-so)
  - [Role-A MAMLs live in `table_types/`, not `schemas/`](#role-a-mamls-live-in-table_types-not-schemas)
  - [MAML fixture directory: `schemas/`](#maml-fixture-directory-schemas)
  - [Reading MAML source files: shared helper, line-length limit, CRLF handling](#reading-maml-source-files-shared-helper-line-length-limit-crlf-handling)
  - [Error stop messages: include file/schema context](#error-stop-messages-include-fileschema-context)
  - [Filter evaluation: a Null is UNKNOWN, a NaN is a VALUE](#filter-evaluation-a-null-is-unknown-a-nan-is-a-value--the-two-behave-oppositely)
  - [The row-group statistics screen: every uncertainty must DECLINE](#the-row-group-statistics-screen-every-uncertainty-must-decline)
  - [Guard mutating public procedures against being called twice](#guard-mutating-public-procedures-against-being-called-twice)
  - [Implicit finalizers must never route through a path that can throw/abort](#implicit-finalizers-must-never-route-through-a-path-that-can-throwabort)
  - [Automatic BYTE_STREAM_SPLIT for float columns in the writer](#automatic-byte_stream_split-for-float-columns-in-the-writer)
  - [A character ARRAY is trimmed on the way into a column; a character SCALAR is not](#a-character-array-is-trimmed-on-the-way-into-a-column-a-character-scalar-is-not)
  - [Validity is per ELEMENT, and a vector row is not one bit](#validity-is-per-element-and-a-vector-row-is-not-one-bit)
  - [Auto-threading: `omp_in_parallel()` picks a DEFAULT](#auto-threading-omp_in_parallel-picks-a-default-and-that-is-not-the-guard-claudemd-warns-about)
  - [`parquet_table` concurrency: one file owns the OpenMP plumbing](#parquet_table-concurrency-one-file-owns-the-openmp-plumbing-and-guards-key-on-ownership)
  - [A `parquet_table` pointer does not survive a ROW-structural mutation](#a-parquet_table-pointer-does-not-survive-a-row-structural-mutation)
  - [New `parquet_table` state goes on the CACHE](#new-parquet_table-state-goes-on-the-cache--never-as-an-allocatable-component-of-the-type)
  - [Assembling a `parquet_column` from pieces: preallocate and `%paste`](#assembling-a-parquet_column-from-pieces-preallocate-and-paste)
  - [A `parquet_schema` built in code must be parsed before anything reads its fields](#a-parquet_schema-built-in-code-must-be-parsed-before-anything-reads-its-fields)
- [Element-domain modules (`parquet_strings`, `parquet_temporal`)](#element-domain-modules-parquet_strings-parquet_temporal)
  - [The `parquet_strings` module](#the-parquet_strings-module)
  - [The `parquet_temporal` module (date/time/timestamp)](#the-parquet_temporal-module-datetimetimestamp)
- [Build & compiler notes](#build--compiler-notes)
  - [Compiler & language gotchas](#compiler--language-gotchas)
  - [Arrow's own type singletons have thread-unsafe lazy state on first concurrent use](#arrows-own-type-singletons-have-thread-unsafe-lazy-state-on-first-concurrent-use)
  - [gcovr <7.1 cannot parse gcov output for a 10,000+ line file](#gcovr-71-cannot-parse-gcov-output-for-a-10000-line-file)
  - [gcovr 8.4+ drops coverage for module-contained Fortran subroutines](#gcovr-84-drops-coverage-for-module-contained-fortran-subroutines)
  - [Verifying the bind(C) boundary](#verifying-the-bindc-boundary)
  - [If `src/parquet_wrapper.cpp` is ever split into multiple translation units](#if-srcparquet_wrappercpp-is-ever-split-into-multiple-translation-units)
  - [Stale `fpm` build cache](#stale-fpm-build-cache)
  - [Keeping `tools/prep_fpm_publish.sh` in sync](#keeping-toolsprep_fpm_publishsh-in-sync)
  - [Manual (never-`fpm test`) large-scale/benchmark tools](#manual-never-fpm-test-large-scalebenchmark-tools)
  - [Measuring whether Arrow memory was actually freed](#measuring-whether-arrow-memory-was-actually-freed-rss-cannot-answer-the-pool-counter-can)
  - [A `shared_ptr` parameter on a per-row helper](#a-shared_ptr-parameter-on-a-per-row-helper-is-the-first-thing-to-suspect-in-parquet_wrappercpp)
  - [Instrument phases before optimising a multi-phase operation](#instrument-phases-before-optimising-a-multi-phase-operation)
- [Testing & coverage](#testing--coverage)
  - [Running a single test suite/test](#running-a-single-test-suitetest)
  - [Error scenarios are pre-run in parallel](#error-scenarios-are-pre-run-in-parallel)
  - [Tests run concurrently: never share a fixture file path between two tests](#tests-run-concurrently-never-share-a-fixture-file-path-between-two-tests)
  - [Every `check()` call needs its own message](#every-check-call-needs-its-own-message)
  - [Verifying a change with mutation testing](#verifying-a-change-with-mutation-testing)
  - [A test that asserts a REFUSAL must say what to assert when the refusal lifts](#a-test-that-asserts-a-refusal-must-say-what-to-assert-when-the-refusal-lifts)
  - [A static check that enumerates names goes stale silently](#a-static-check-that-enumerates-names-goes-stale-silently)
  - [Measuring test coverage](#measuring-test-coverage)
  - [Fortran gcov attribution artifacts](#fortran-gcov-attribution-artifacts)
  - [`src/parquet_wrapper.cpp`: GCC vs Clang gcov attribution](#srcparquet_wrappercpp-gcc-vs-clang-gcov-attribution)
  - [Regression tests for "sized/typed from the first element" bugs](#regression-tests-for-sizedtyped-from-the-first-element-bugs)
  - [A Fortran-side debug hook has to be PUBLIC, so prefer a C++ one](#a-fortran-side-debug-hook-has-to-be-public-so-prefer-a-c-one)
  - [Guarding a hard Arrow int32-only ceiling](#guarding-a-hard-arrow-int32-only-ceiling)

## Workflow & guardrails

### Only modify files inside this repository

Never edit, create, or delete files outside this library's own directory tree (e.g. files
under a different project checkout, dotfiles like `~/.zprofile`, or other paths elsewhere on
the machine) — even when doing so would streamline a task (such as setting a computer-specific
environment variable). If something outside this repository genuinely needs to change, tell
the user what's needed and let them make that change themselves.

### Report before implementing on analysis/audit requests

When asked to analyze, audit, or review something (naming conventions, documentation
duplication/coverage, test coverage, etc.), report findings and a proposed plan first and
wait for confirmation before editing any files. Only proceed straight to editing when
explicitly asked to implement/fix/add something directly.

### Only apply low-blast-radius renames/refactors

When renaming or refactoring existing (non-new) code for consistency, only apply the
renames/changes that are low blast-radius (few call sites, no public API/doc impact).
For anything with wider knock-on effects (public API, many call sites, cross-file
conventions), report it as a proposed change and wait for confirmation instead of applying
it directly.

### `feature_*.md` planning documents

`feature_*.md` files in the repo root
are design/planning documents for features not yet implemented — they are git-ignored
(`.gitignore`'s `feature_*.md` entry), so they never reach a commit and exist purely as scratch
design memory between sessions.

**`feature_risks.md` is the one exception and is TRACKED** (`.gitignore` carries an explicit
`!feature_risks.md` negation) — it is a committed document, not scratch memory, so everything below
about writing for a future session applies to it doubly, and it must also read correctly for a
contributor who has never seen a planning document at all. See
[The `feature_risks.md` standing-risks register](#the-feature_risksmd-standing-risks-register) for
its structure and the rules for editing it. A *new* `feature_*.md` file is scratch by default: adding
another negation is a deliberate decision to publish that document, not a formatting choice.

**Every `feature_*.md` design or implementation document must carry a settings analysis** — does
this feature introduce any process-global parameter, does each one pass the admission test, and if
so what are its knob name, default, validation and environment variable? Answer "none" explicitly
when the answer is none; a missing section is indistinguishable from the question never having been
asked. See
[A new process-global parameter goes in `parquet_settings`](#a-new-process-global-parameter-goes-in-parquet_settings-and-a-design-doc-must-say-so)
for the admission test and what follows from it.

**Whenever asked to write or update a `feature_*.md` file, write it to be fully self-explaining
without relying on the current session's conversation for context** — a future session opening
the file has no memory of this one. Concretely: don't reference "this conversation," "as discussed
above" (meaning the chat, not the document), tool-call artifacts (e.g. a clarifying-question
option that was offered but not visibly quoted), or any other detail that only makes sense to
someone who was present for the conversation that produced the file. Quote the user's own
decisions/wording directly in the document rather than alluding to them. Cross-references to
other files in the repo (source, other `feature_*.md` docs, `CLAUDE.md` sections) are fine, since
a future session can read those too.

### The `feature_risks.md` standing-risks register

`feature_risks.md` (repo root, **tracked** — see the previous section) records the properties of the
shipped code that a future change can break **without any test failing and without an abort**: a
wrong answer, a stale pointer, a corrupted heap, a silently skipped row group. It is the companion
to read *before editing an area*, not a to-do list, and it is where the reasoning behind a
non-obvious invariant lives when that reasoning is too long for a code comment and too specific for
this file.

Four rules govern it, and all four are easy to break by treating it as an ordinary document:

- **Every risk is `Risk-N`, and the number is permanent.** Numbering runs from `Risk-1` upward
  across the whole file, independent of which section the entry sits in. Moving an entry between
  sections **never** renumbers it, so a reference from this file, from `feature_table.md`, from
  `tools/check_source_conventions.py` or from a code comment stays valid for good. Numbers of
  deleted entries are **not** reused. Never renumber to make a section contiguous.
- **Four sections, and an entry moves between them as its status changes**: *1. New risks* (where a
  newly identified one lands, before anyone has decided whether it is testable — empty is the
  healthy state), *2. Risks with a proposed testing scenario*, *3. Risks not testable*, and
  *4. Risks already covered, kept for what they still forbid*. A new risk takes the next unused
  number and goes in section 1.
- **Section 4 is pruned, not archived.** A covered entry stays only if it still forbids something —
  a rule for the next contributor, a trap not visible in the code, or a test whose *design* has to
  be copied rather than merely kept passing. An entry that has become "this works and is tested" is
  deleted outright; the test is the record at that point, and a register that accumulates solved
  problems stops being read.
- **A verdict is checked against the suite, never inferred.** Before marking anything "proposed",
  grep the tests for what actually asserts it — several entries once marked proposed turned out to
  be covered already. Before marking one "covered", name the test.

**When implementing a proposed test from it, update the entry in the same change**: move it to
section 4 (or delete it, per the pruning rule), name the test that now covers it, and say what the
test's shape is protecting if that is the interesting part. An entry that still reads "proposed"
after the test exists is worse than no entry, because the next reader will write the test again.

**When a new silent-failure property is discovered** — typically while fixing a bug whose symptom
appeared far from its cause — add it as a new `Risk-N` in section 1 rather than only writing a code
comment. This file (CLAUDE.md) is for rules that apply project-wide; `feature_risks.md` is for a
specific property of a specific area, with its test status attached.

### Don't run the GitLab CI pipeline yourself

The user runs `.gitlab-ci.yml` on their own GitLab server — don't attempt to execute it
(e.g. via `gitlab-runner`, docker, or otherwise) as part of verifying changes. Verify
locally instead (`fpm build`/`fpm test` with the same `FPM_FFLAGS`/`FPM_CXXFLAGS`/
`FPM_LDFLAGS` the CI job sets, minus anything CI-environment-specific like the apt installs).

**In practice that means running plain `fpm test` and setting nothing at all.** Two separate
reasons, and both are easy to get wrong in the direction of "add the flag to be safe":

- **`-fopenmp` is not needed and never was.** `fpm.toml`'s `openmp = "*"` metapackage injects it
  into the compile *and* link flags, for this package and its dependencies alike. Confirm with
  `fpm build --show-model | grep -o 'fortran_compile_flags="[^"]*"'`, which shows `-fopenmp` even
  when `FPM_FFLAGS` carries nothing but `-I` paths; confirm end-to-end by running
  `error_scenarios concurrent_calls_into_shared_reader` from a build with no `-fopenmp` anywhere,
  which still aborts (exit 134) because several threads really do enter the reader at once. An
  earlier version of this section prescribed `FPM_FFLAGS="-fopenmp" fpm test`; that was redundant,
  and `.gitlab-ci.yml` has had the same redundant flag removed.
- **`FPM_FFLAGS` clobbers what the environment exported, but it does NOT suppress fpm's profile
  flags — the missing `-O`/`-fcheck=bounds` comes from omitting `--profile`.** Setting it on a
  command line replaces whatever the environment already exports (a dev machine typically puts
  Arrow-adjacent `-I` paths there — clobbering those is the Fortran-side twin of the
  `fatal error: 'arrow/api.h' file not found` failure below), and that half is worth avoiding. But
  the profile half is a **separate mechanism**: fpm applies profile flags only when `--profile` is
  given, and appends `FPM_FFLAGS` to them rather than instead of them.

  Verified on fpm **0.13.0 alpha** by reading the compile line
  `fpm build --verbose` actually emits, in a throwaway two-file project, all four ways
  (independently reproduced on two machines):

  | invocation | Fortran compile line gets |
  |---|---|
  | `--profile release`, `FPM_FFLAGS` **set** | `-I<env> -O3 -Wimplicit-interface …` — **additive** |
  | `--profile debug`, `FPM_FFLAGS` **set** | `-I<env> -Wall -Wextra -g -fcheck=bounds …` — **additive** |
  | no `--profile`, `FPM_FFLAGS` set | `-I<env>` and nothing else |
  | no `--profile`, `FPM_FFLAGS` unset | **nothing else either** |

  The last row is what identifies the real cause: with no `--profile` there is no `-O` *whether or
  not* `FPM_FFLAGS` is set. **So a plain `fpm test` has no bounds checking on ANY machine, not only
  on one that exports `FPM_FFLAGS`, and `fpm test --profile debug` is what turns it on — including
  on a machine that exports `FPM_FFLAGS`, which does not lose the debug flags.**

  An earlier version of this section said `FPM_FFLAGS` "REPLACES, it does not add" and attributed
  the missing flags to it. The advice that followed was right and is unchanged; the reason was
  wrong, and wrong in a way that matters — it implied a `--profile release` measurement taken on a
  machine that exports `FPM_FFLAGS` was built at `-O0` and should be discarded. It was not. Re-check
  the table above against a newer fpm before assuming it still holds.

Setting `FPM_CXXFLAGS`/`FPM_LDFLAGS` to CI's values has the same replacing behaviour on the C++
half, which on a dev machine is where Arrow's include and library paths come from — the build then
fails with `fatal error: 'arrow/api.h' file not found`, which looks like a missing dependency
rather than a flag problem. CI can set them because its own image puts Arrow on the default search
path.

So: **`fpm test`** for the ordinary check, **`fpm test --profile debug`** when you want the
bounds/`-Wall` checks (worth doing at least once for anything touching allocation or array
shapes — see the `clone_new_cache` note under "Compiler & language gotchas" for a bug only that
build could see), and `tools/coverage.sh` rather than a hand-rolled `--coverage` when measuring
coverage.

### The CI-environment Docker image: ask for it, never build it

A local run can only ever prove a change works on *this* machine's compiler. Some real failures
are specific to the CI toolchain and are invisible locally — the metadata-array copy in
`clone_new_cache` (see "Compiler & language gotchas") segfaulted on CI's gfortran while running
clean locally under `-fcheck=all`, `--coverage -fopenmp` and the full suite. When a CI failure
cannot be reproduced locally, or when a change needs checking against a compiler that isn't
installed here, **ask the maintainer for the CI-environment Docker image**: it reproduces the
GitLab CI environment and carries **gfortran, ifx and flang**, so all three can be exercised
against the current working tree.

Rules for using it:

- **Never build or rebuild the image yourself** — not via `tools/build_ci_test_image.sh`, not via
  `docker build`/`docker run` against a base image, not by any other route. Building it is the
  maintainer's job, and only the maintainer's.
- **Only ever run the library against an image the maintainer has already made available.** That
  means building and testing this repository inside it, nothing else.
- **Any change the image itself needs** (a different compiler version, another package, a changed
  `before_script` step) is a **request** to the maintainer, who will generate a new image. Do not
  work around a missing tool by installing it into a running container either — say what is
  needed and wait.
- Asking for the image is not a substitute for the local verification described above; it is what
  to reach for *after* a local run has come back green and the failure persists on CI.

### Don't commit or push on the main/default branch yourself

The user always commits and pushes their own changes on `main` — even after explicitly asking
for a feature/fix to be implemented, do not run `git commit`/`git push` on `main` yourself
unless they separately, explicitly ask for that specific commit. Leave finished work
uncommitted in the working tree for them to review and commit. (This is specific to the
main/default branch; it doesn't apply to work you've been asked to do inside your own
throwaway branch/worktree, if any.)

## Documentation conventions

### New features require tests and docs

Whenever asked to implement a new feature in this repository, always:

1. Add unit test coverage for it (in the relevant `test/*.f90` suite; add abort/error-path
   coverage via `test/error_scenarios.f90` + `test/test_errors.f90` +
   `tools/run_error_scenarios.sh` if the feature has failure modes that `error stop`).
2. Update documentation — see [Documentation structure](#documentation-structure) for what
   goes where. In brief: every new public procedure/type gets its own `!>`(leading)/`!!`(trailing) doc-comment
   (picked up automatically by the FORD-generated API reference — no hand-maintained table to
   update); user-facing behavior/how-to goes in the relevant `doc/pages/*.md` guide page; touch
   README.md only if the landing-page story changes (a new entry in its compact "API overview"
   index, a new limitation, a setup change); update CONTRIBUTING.md if it affects contributor
   workflow.

Do this without being asked separately each time — it applies by default to any
"implement/add feature" request in this repo, not just when explicitly reminded.

**CHANGELOG is active as of the 1.0.0 release.** `CHANGELOG.md` has a published `[1.0.0]`
section — every user-facing change from here on (new feature, behavior change, bug fix affecting
documented behavior) gets an `[Unreleased]` entry added at the top of the file, following the
existing [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) format already used there. Do
this without being asked separately, the same way tests/docs are added by default for a new
feature (see "New features require tests and docs" above).

**`[Unreleased]` is written for someone upgrading from the last release, not as a development
log.** Everything in it is read against `[1.0.0]` (or whatever the newest published section is),
so an entry only earns its place if it describes a difference a reader of that release would
actually see. Four rules follow, and they apply to every future entry — not just when someone
asks for a cleanup:

- **`### Changed` and `### Fixed` are for functionality that is in the RELEASED version.** A fix
  or behavior change to something that is itself still sitting in `[Unreleased]` is invisible to
  every user — there is no released behavior for it to differ from — so it does not get its own
  entry. Fold whatever the reader needs to know into that feature's own `### Added` bullet
  instead, and drop the rest. This is the rule that keeps the section from filling up with the
  history of how an unreleased feature was built.

  Stated the other way round, because that is the form it is usually needed in: **a feature
  implemented after 1.0.0 never earns a `### Changed`/`### Fixed` entry for its own subsequent
  changes and fixes**, however many rounds it goes through before it ships. It has exactly one
  bullet — under `### Added` — and that bullet is kept current instead. `### Changed`/`### Fixed`
  are reserved for the behaviour a reader of 1.0.0 (or of whatever the newest published section
  becomes) can actually observe changing under them.
- **`### Added` records overall features, not each part of one.** A sub-feature, a new
  type-bound procedure, or a helper that exists to serve a feature already listed belongs *in
  that feature's bullet*, not as a bullet of its own. Several closely related additions that
  share a purpose (say, a set of new metadata-only reader queries) go in one bullet together.
- **Leave out what is not user-facing at all**: test coverage, internal refactors and file
  reorganizations explicitly marked "no functional change", tooling that changes nothing a
  consumer of the library can observe. A genuinely new contributor-facing tool wired into CI can
  have one short line; its subsequent tweaks cannot.
- **One `### Added`/`### Changed`/`### Fixed` per release section, in that order.** Appending a
  second `### Added` after `### Fixed` is easy to do by accident when adding an entry to a long
  section, and it silently splits the list a reader is trying to read as one.

Re-read the whole `[Unreleased]` section when adding to it, rather than appending to the end: an
entry frequently belongs inside a bullet that is already there, and a fix being added may
supersede text further up.

### Documentation structure

User- and contributor-facing docs are split across three layers — keep new content in the right one:

- **README.md** — the lean *landing page*: what the library is, features, one quick example,
  install / prerequisites / environment variables, "important behavior", a compact **API
  overview** index (plain-text procedure/type names, no per-procedure detail), limitations, and
  license/contributing pointers. Keep it short — do **not** let it grow back into a manual;
  deep-dive/how-to content goes in `doc/pages/`, and the per-procedure reference is generated
  by FORD, not hand-written here.
- **`doc/pages/*.md`** — the full *user guide*, one file per topic (reading, writing, building a
  schema in code, the MAML metadata format, a combined example, error handling, thread safety,
  supported data types, performance, troubleshooting, embedding your own MAML schemas), rendered
  as FORD narrative pages (`doc/pages/index.md` is the landing page for this guide, with an
  `ordered_subpage:` frontmatter entry + body bullet per page). Never a hand-maintained
  per-procedure API table here either — link to the generated reference instead.
- **FORD-generated API reference** — every public procedure/type/module gets its own `!>`
  (leading) doc-comment plus a trailing `!!` tag on every dummy argument/function result (see
  the "FORD doc-comment conventions" section below); FORD turns these into the browsable
  modules/procedures/types reference automatically. This is the *only* place the per-procedure
  reference lives — there is no hand-written equivalent to keep in sync.
- **CONTRIBUTING.md** — *contributor-facing*: building/testing this repo, project conventions,
  and repo-maintenance tooling (see its own Contents ToC for the full topic list, kept current
  there rather than duplicated here).

Working rules:

- A new public procedure gets a `!>`(leading)/`!!`(trailing) doc-comment (see "FORD doc-comment conventions"
  below) **and**
  its name in **README.md**'s "API overview" index; keep the two in sync on any rename/removal.
- Every section heading must appear in that file's own **Contents** ToC (README.md/CONTRIBUTING.md/CLAUDE.md);
  `doc/pages/*.md` pages don't need one — FORD generates in-page navigation from headings itself.
- Moving content between README.md and a `doc/pages/*.md` page turns in-page `#anchor` links
  into cross-file `doc/pages/<page>.md#…` / `README.md#…` links — repoint them, and fix
  now-stale relative wording ("above", "below", "this README"). Re-run `tools/check_doc_anchors.py`
  afterward (see "Checking documentation links" below).
- **Optional arguments are shown in square brackets** when a signature is written out in prose or
  in a table — `call t%get_file_metadata(key, value, [found])`, `%ncols([resident_only])`. This
  applies to *descriptions* of a call, never to a runnable code example inside a ```fortran fence,
  where brackets would not compile. Adopted after the fact rather than in one sweep: apply it to
  any signature you write or edit, and retrofit a whole page the next time that page is touched
  for another reason (`doc/pages/table.md` is retrofitted; the others are not yet). The first
  bracketed signature on a page should carry a one-line note saying what the brackets mean.
- **Diagrams: plain text, not Mermaid.** This project's GitLab does not reliably render Mermaid
  diagrams, so draw flows as plain-text/ASCII inside a normal code fence (renders identically
  everywhere) — see the MAML→header flow in `doc/pages/maml-format.md`'s "The MAML metadata format".
- **Badges:** README.md carries three dynamic `gitlab.4most.eu` badges (CI pipeline, test
  coverage, API documentation) alongside the static license/language/fpm ones. These are
  GitLab-specific — `tools/prep_github_mirroring.sh` swaps them for a single GitHub Pages
  documentation badge when mirroring (see CONTRIBUTING.md's "Mirroring to GitHub").

### Checking documentation links

After editing headings or `#anchor` links in README.md/CONTRIBUTING.md/CHANGELOG.md/CLAUDE.md/docs.md/
`doc/pages/*.md`, run `tools/check_doc_anchors.py` to verify every in-page and cross-file anchor
link still resolves against GitHub's actual heading-slug rules. It exits nonzero and lists any
broken link. It scans `doc/pages/*.md` too, resolving both the raw `other.md#anchor` form (used by
README.md/CONTRIBUTING.md linking *into* `doc/pages/`) and the FORD-rendered `name.html#anchor` /
`../index.html#anchor` form those pages use to link to each other / back to README.md (see
`resolve_link_target` in the script).

### FORD doc-comment conventions

Every public/private procedure, type, dummy argument/function result, and type-bound procedure
binding in `src/*.f90` carries a `!>`(leading)/`!!`(trailing) doc-comment. `ford docs.md` should
run clean (besides the expected environment-only "Graphviz not installed" warning) — keep new
code to the same standard.

**A clean `ford docs.md` run does not by itself verify doc-comment coverage.** FORD's
undocumented-entity warnings are opt-in (`-w`/`--warn` on the command line, or a `warn:` key in
`fpm.toml`'s `[extra.ford]`) and are **not** enabled in this project's `fpm.toml` — a plain
`ford docs.md` only proves nothing is *broken* (bad cross-references, malformed metadata, parse
failures), not that everything is documented. A completely undocumented new procedure can be added
and `ford docs.md` will still "run clean." To actually check coverage, run `ford --warn docs.md`
instead — expect it to be very noisy (several thousand warnings), dominated by categories that are
*not* required by this project's conventions and can be ignored:

- `Undocumented variable` for local variables (`i`, `idx`, `res`, ...);
- `Undocumented moduleprocedure` for the abbreviated `module procedure NAME ... end procedure NAME`
  form (exempted by the bullet below);
- `Could not extract source code for proc ...` — the separate FORD limitation documented under
  "FORD config gotchas" below, not a documentation gap at all;
- **`Undocumented interface` and `Undocumented proc`** — these fire on interfaces that *are*
  documented, so they are noise too. Verify before believing either one:
  `parquet_prefetch_columns_array` and `table_materialize` both carry full `!>`/`!!`
  doc-comments and both appear in this list. The count
  scales with how many interface bodies exist, so **adding N documented procedures raises it by
  roughly N** — and a large chunk of the entries are FORD's own unnamed `'unknown'` placeholders,
  which name no entity at all and cannot be acted on even in principle.

**The one category that is genuinely load-bearing is `Unknown entity`,** which is the
use-association accessibility limitation documented under "FORD config gotchas" and sits at a
stable **23** (14 `public ::` re-exports in `parquet_core.f90` plus 9 `private ::` statements in
the `parquet` facade). Compare *that*
number across a change, not the total: it is the only one that moves for a real reason. When a
before/after total does move, break the delta down by category
(`grep "Warning" | sed -E 's/.*Warning: //'`) rather than treating the raw count as a regression —
a stage that adds a few dozen procedures will add several hundred warnings while documenting every
one of them.
The "before/after regression" check further down needs `ford --warn docs.md`'s output for the
same reason — comparing two plain `ford docs.md` runs compares two counts that are both
structurally 0 (Graphviz-only) and cannot detect a coverage regression — but compare it
per-category, per the paragraph above, not as a single total.

Keep new code to the same standard:

- Leading `!>` = predoc (documents what follows); trailing `!!` = postdoc (documents what
  precedes). **`!<` is not a FORD marker at all** (that's Doxygen).
- Every dummy argument/function result gets its own trailing `!!` tag wherever the argument list
  is actually written out — the spec in `parquet_core.f90`, and any submodule body that *restates*
  the full interface (`module subroutine name(args)` with the arguments redeclared, e.g.
  `parquet_maml_base_add_col_qc.f90`). The abbreviated `module procedure name ... end procedure
  name` form (no restated arguments) is exempt — nothing to tag, and the spec in `parquet_core.f90`
  is the canonical doc location for it.
- Every type-bound procedure binding (`procedure ::`, `generic ::`, `final ::` inside a type's
  `contains` block) needs its own short trailing `!!` description, separate from documenting the
  procedure it binds to (easy to forget since the bound procedure's own doc feels like it
  "covers" the binding), e.g.
  `procedure :: add => parquet_filter_add !! Appends one AND-combined rule clause.`.
  **Accepted exception:** the two `generic :: add_metadata => ...`/`generic :: add_metadata =>
  schema_add_metadata_...` bindings (`parquet_core.f90`'s `parquet_column_info`/`parquet_schema` type
  bodies) document with a leading `!>` above the binding instead, since both are long multi-line
  continuation lists where a trailing `!!` would be awkward. Every other binding in both type
  bodies still uses the trailing form — don't extend this exception to a new binding without a
  similar multi-line-continuation reason.
- Don't start a doc-comment's first line with a bare `word:` (e.g. "qc: min: ..."). FORD reads
  that as an attempted metadata key: it either warns (unrecognized key) or, worse, silently
  swallows the line if it happens to match a real key (`date:`, `author:`, `version:`, ...).
- **A rationale/gotcha explanation written directly above a procedure header is still that
  procedure's required doc-comment — it must use `!>`, not a plain `!` block.** The two look
  interchangeable at a glance (both are multi-line comment blocks sitting just above code), but
  only `!>` is picked up by FORD; a plain `!` block there silently leaves the procedure
  undocumented even though a human reader sees an explanation right above it. Plain `!` stays
  reserved for the *interface-block group banners* described below (`! ---- ... ----`) — never
  for a procedure's own doc, even a short one-off private helper.
- **After a file split/relocation refactor, verify FORD coverage didn't regress with a
  before/after diff, not just a clean `ford docs.md` run on the new state alone**: `git stash` the
  changes, run `ford --warn docs.md` (not plain `ford docs.md` — its warning count is always 0
  regardless of coverage, see above, so it cannot detect a regression this way), note the warning
  count, `git stash pop`, and re-run — matching counts confirm the refactor didn't silently drop
  doc-comments FORD would have warned about anyway. **A pure refactor is the only case where the
  raw totals should match**; a stage that *adds* procedures raises the total by design, so there
  compare `Unknown entity` and the per-category breakdown instead (see above). A relocated
  private helper legitimately
  disappearing from its old module's FORD
  page (private procedures don't get individual page entries) is expected and not a regression by
  itself — cross-check with `grep 'public ::'` in `parquet_core.f90` before treating an "not found on
  page" result as a problem.

### FORD config gotchas

- `md_extensions = ["markdown.extensions.toc"]` in `fpm.toml`'s `[extra.ford]` is
  **load-bearing** — without it, no heading gets an `id`, silently breaking every anchor link
  site-wide with no error.
- `preprocessor = "cpp -traditional-cpp -E"` is required, not optional — `src/parquet.f90`'s
  version-string logic uses real cpp macros, not just comment-style directives.
- `doc/pages/*.md` files deliberately have **no top-level heading in their body** (title comes
  from frontmatter only) — adding one back would reintroduce the duplicate-heading bug
  `doc/user.css` fixes on the front page.
- Two working CI doc-publish paths: `.github/workflows/docs.yml` (GitHub Actions → GitHub Pages)
  and `.gitlab-ci.yml`'s `readthedocs` job (GitLab CI → gitlab.4most.eu's readthedocs-style
  docserver). GitLab Pages isn't available on gitlab.4most.eu — the docserver replaces it.
- **FORD does not resolve `doc/pages/*.md` links written in README.md's body text** (`docs.md` is
  `{!README.md!}`, embedding README.md's raw markdown verbatim as the front page). FORD's own
  navbar correctly links to `page/index.html`, proving it knows the real mapping
  (`doc/pages/<name>.md` → `page/<name>.html`, `#anchor` preserved) — it just doesn't apply that
  resolution inside embedded markdown body content. Both CI docs jobs run
  `tools/fix_ford_page_links.sh ford-doc` right after `ford docs.md` to fix this in the generated
  output; README.md's source keeps the `doc/pages/*.md` form since that's what's correct for
  browsing the repo directly on GitLab/GitHub. This is host-independent (unlike
  `tools/prep_github_mirroring.sh`), so both CI jobs need it, not just one. `doc/pages/*.md` files
  linking to *each other* don't have this problem — they already use FORD's native
  `page/*.html`-relative form directly in source.
- `tools/generate_parquet_maml.sh`'s `end module` template line must keep emitting
  `! GCOVR_EXCL_LINE` (added deliberately, not FORD/gcovr's default) — a careless edit there
  will silently drop it from every regenerated file (`src/parquet_maml_base.f90` and any
  downstream project's generated `parquet_maml` module).
- **FORD 7.0.13 never renders per-argument docs for members of a named, multi-specific generic
  interface** (e.g. `parquet_get_metadata`'s 12 `module procedure` entries) — every member's card
  under "Module Procedures" on the generic's page permanently shows "Arguments: None". This is
  **not fixable from source** — rewriting the submodule's abbreviated `module procedure NAME` into
  a fully restated `module subroutine NAME(args)` has no effect on the rendered output. Solo public
  procedures that aren't generic members (e.g. `parquet_get_string_length`) are unaffected.
  Workaround in place: each of the 12 public generics' own leading `!>` doc-comment (the block
  immediately above `interface <name>`) spells out every distinct argument name/role in prose,
  since that comment does render on the generic's page. Do not re-attempt the submodule-restatement
  fix without first checking a newer FORD release against upstream issue
  (https://github.com/Fortran-FOSS-Programmers/ford/issues/738).
- **FORD 7.0.13 cannot resolve a `use`-association accessibility statement** — an
  `Unknown entity '<name>' with attribute '<public|private>' in module '<m>'` warning, currently
  **23** of them and the one FORD number worth tracking across a change. Two independent groups:
  **14 `public ::`** re-exports in `parquet_core.f90` (`parquet_date`/`parquet_time`/
  `parquet_timestamp` and the eight `parquet_unit_*`/`parquet_ns_*` constants from
  `parquet_temporal`; `parquet_string`/`parquet_string_column` from `parquet_strings`;
  `parquet_maml_file` from `parquet_maml_base`), which FORD silently drops from that module's
  generated page; and **9 `private ::`** statements in the `parquet` facade (`c_int`, the two
  `parquet_get_*_version` bindings, and six names from `parquet_settings` —
  `parquet_valid_compressions`, `parquet_resolve_writer_compression`, the three `parquet_emit_*`
  output channels and `parquet_output_is_suppressed`), which are the facade's only way to keep those
  names out of the namespace `use parquet` hands a user, so they cannot be removed.
  **This number rises by one for each name a future `private ::` in the facade hides**, which is
  the expected cost of keeping a sibling module's internal plumbing out of the public namespace —
  a rise of exactly that size is not a regression. **Confirmed not fixable from
  source**: for the `public ::` group, neither adding a `!>` doc-comment directly on the line, nor
  an explicit `use ..., only: name1, name2` import list (already how these modules are imported),
  nor a bare unrestricted `use` with no `only:` at all changes anything — all three were tried
  against FORD 7.0.13 and the count stayed at 14 every time. The underlying symbols are not lost
  from the generated site — the three temporal types get their own `type/parquet_date.html`-style
  pages and the constants render on `module/parquet_temporal.html`/`module/parquet_strings.html`/
  `module/parquet_maml_base.html` — they just don't appear as belonging to `parquet_core` on that
  module's own generated page. Re-test against a newer FORD release before attempting either
  source-side fix again.
- **The `parquet` facade's re-exports do not appear on `module/parquet.html` either, and that is
  the accepted cost of S10.** The facade re-exports its siblings with *bare* `use` statements
  (no `public ::` list), which produces no warning at all — but FORD equally does not list the
  re-exported names as belonging to `parquet`, so the page a user is told to `use` shows only
  `parquet_get_version`. Keeping `cversion`/`parquet_get_version` in the facade is what stops it
  being an empty page (it renders at ~33 KB). **The maintainer accepted this** and the guide points
  readers at the site-wide `lists/procedures.html`/`lists/types.html` instead, where every name does
  appear — `doc/pages/index.md` says so explicitly. This is a cosmetic gap in the generated
  reference, not a broken link. Re-test against a newer FORD before assuming it still holds.
- **FORD 7.0.13 cannot extract a "Source Code" section for a `module procedure NAME ... end
  procedure NAME` implementation** (the abbreviated form; `fpm.toml` sets `source = true`), which
  is why `ford --warn docs.md` reports roughly 240 "Could not extract source code for proc ..."
  warnings, including for a large share of the public surface (`parquet_get_nrows_int32`/`_int64`,
  every `parquet_write_<type>_column`, every `parquet_get_metadata_<type>`, ...). **Confirmed
  fixable in principle, but not worth doing**: rewriting one such procedure
  (`parquet_get_string_length` in `parquet_read.f90`) from the abbreviated form into a fully
  restated `module subroutine NAME(args) ... end subroutine NAME` does make FORD extract its
  source correctly — but it also makes FORD start requiring that submodule body to carry its own
  doc-comments (it otherwise reports a new `Undocumented interface` warning), which directly
  conflicts with this project's own deliberate convention (see "FORD doc-comment conventions"
  above) that the abbreviated form is *exempt* from restating docs precisely so they aren't
  duplicated between `parquet_core.f90`'s spec and every submodule body. Applying this fix across all
  ~240 affected procedures would mean restating and re-documenting most of
  `parquet_read.f90`/`parquet_write.f90`/`parquet_metadata*.f90` — a large, high-blast-radius
  rewrite for a convenience feature (an auto-generated source listing on each procedure's page),
  and it was left undone; the maintainer should decide before anyone attempts it project-wide.

## Source code structure & conventions

### One program unit per file; filename == unit name

Every `src/*.f90` file defines exactly one `module` or `submodule`, and the filename (sans
`.f90`) equals that program unit's name — e.g. `parquet_read_numeric.f90` ⇒
`submodule (parquet:parquet_read) parquet_read_numeric`. This is a near-universal fpm/Fortran
convention and is load-bearing for navigation and for the publish tooling (which flips
`module-naming` to `"parquet"`). Do not put two program units in one file, and do not name a
file differently from the unit it defines. (fpm does not hard-fail on a mismatch by default —
`module-naming = false` here — but treat it as a firm rule.)

### Some `src/*.f90` files are generated — edit the generator, never the output

Several source files are emitted by a `tools/` script from a single source of truth (a kind table, a
schema, a template) and are **committed** rather than regenerated at build time, so `fpm build` stays
dependency-free. They look like ordinary hand-written source when opened, which is exactly the trap:
editing one directly appears to work, passes tests, and is then silently reverted the next time anyone
regenerates.

**Always check the top of a `src/*.f90` file before editing it.** Every generated file opens with a
banner naming its generator (`GENERATED FILE -- DO NOT EDIT BY HAND` / `automatically generated`). If
that banner is there, make the change in the generator's own input instead — the kind/field table or
template inside the script — and re-run it. Currently generated: `src/parquet_maml_base.f90` (from
`tools/generate_parquet_maml.sh` + the `.maml` files under `schemas/`), and `src/parquet_columns.f90`,
`src/parquet_columns_access.f90`, `src/parquet_columns_mutate.f90` (from
`tools/generate_parquet_columns.py`, whose kind table is the single place a supported column kind is
declared), and `src/parquet_tables.f90`, `src/parquet_tables_access.f90`,
`src/parquet_tables_addcol.f90`, `src/parquet_tables_materialize.f90` (from
`tools/generate_parquet_tables.py`, which imports that same kind table), and `src/parquet_sorting.f90`,
`src/parquet_sorting_keys.f90`, `src/parquet_sorting_argsort.f90`, `src/parquet_sorting_permute.f90`,
`src/parquet_sorting_select.f90`, `src/parquet_sorting_search.f90`, `src/parquet_sorting_unique.f90`,
`src/parquet_sorting_reduce.f90`
(from `tools/generate_parquet_sorting.py`, which imports the nine SCALAR rows of that same kind table
and adds the three types that are not `parquet_column` storage kinds at all). `src/parquet_table_example.f90` is emitted by
`tools/generate_user_table_code.py` from `table_types/maml_example4.maml` (see "Role-A MAMLs live in
`table_types/`" below) — it ships as a worked example and nothing else in the library uses it, but it
is committed and `--check`ed exactly like the rest. **`src/parquet_tables.f90` is
the one most likely to be edited by mistake**, because it is the table layer's module spec — every
type-bound binding and every interface body lives there, so adding a `parquet_table` procedure means
editing the generator's literal template text, not the file it emits. Treat this list as a snapshot —
trust the banner, not the list, and add new generated files here when they appear.

Working rules for this class of file:

- **A generator must emit the project's own conventions**, or it multiplies a single template omission
  across every kind it emits: `!>`/`!!` doc-comments on everything public (only `ford --warn docs.md`
  would reveal their absence — see "FORD doc-comment conventions"), the `! GCOVR_EXCL_LINE` markers, and
  the 132-column line limit.
- **Prefer a generator that can verify its own output.** `tools/generate_parquet_columns.py --check`
  re-derives the files in memory and exits nonzero if the committed copies have drifted — run it after
  any change in that area, and give a new generator the same mode.
- **Sibling generators should share one source of truth rather than each carrying its own copy.** A
  second generator over the same kind table imports it from the existing script instead of duplicating
  it; two drifting copies of a kind list is a much worse failure than one slightly awkward import.
  **A CONSUMER-FACING generator cannot do that**, because it is copied into projects where this
  repository's files do not exist — so it bakes the copy in and has `--self-test` cross-check it
  against the real source *when that source is present*, which is always here and never downstream.
  `tools/generate_user_table_code.py` carries `parquet_table`'s ~264 type-bound procedure names
  that way (a field name colliding with one cannot become an accessor), so **adding a binding to
  `parquet_table` fails the lint stage until that list is updated** — which is the point: the
  staleness is closed by a test rather than by remembering.
- **A generator whose output is USER-EDITABLE needs marker-delimited windows, and three rules
  that make them safe.** Most generated files here are machine-owned; `tools/generate_user_table_code.py`
  emits a module a user is expected to extend, which is a different problem. The windows are the
  generator's **input**, not decoration — it lifts them out of the existing file, re-emits
  everything else, and puts them back; it also *reads* one of them to write the matching
  `%clone`/reset statements. So: a malformed marker set (missing, duplicated, unbalanced) must be
  refused **before** anything is rewritten, since that is the one state in which regenerating
  destroys user code; `--check` belongs in CI, because an edit outside a window otherwise works
  perfectly until the next regeneration silently deletes it; and the header carries a digest of the
  source input, so `--check` can say "you edited generated text" rather than "this file is stale"
  — without it both look identical and the message has to guess. See `feature_risks.md` Risk-32.
- **A generator that is maintainer-only belongs in `tools/prep_fpm_publish.sh`'s `REMOVE_PATHS`**
  (downstream projects consume the committed output). One that is consumer-facing — like
  `tools/generate_parquet_maml.sh`, which downstream projects run on their own schemas — does not. See
  "Keeping `tools/prep_fpm_publish.sh` in sync".

### Nested submodule tree

`src/*.f90`'s `parquet_core`/`parquet_read`/`parquet_write`/`parquet_metadata` files form a nested
submodule tree (not flat siblings under `parquet_core`), split by data-type family for read/write
and by format for metadata:

```
parquet                         (module — the FACADE; see below. Holds only parquet_get_version)
parquet_core                    (module — core API + cross-subtree private-helper interfaces)
├─ parquet_read                 (submodule — reader lifecycle, queries, shared read helpers)
│   ├─ parquet_read_numeric     (int32/int64/float32/float64/logical, all access modes)
│   ├─ parquet_read_string
│   └─ parquet_read_temporal    (date/time/timestamp)
├─ parquet_write                (submodule — writer lifecycle, shared write helpers)
│   ├─ parquet_write_numeric
│   ├─ parquet_write_string
│   └─ parquet_write_temporal
└─ parquet_metadata             (submodule — parse/build orchestration + shared metadata helpers)
    ├─ parquet_metadata_base    (format-agnostic column_info/table_metadata plumbing)
    ├─ parquet_metadata_get     (parquet_get_metadata queries)
    └─ parquet_metadata_maml    (MAML-specific: section schema + all validation)

parquet_bindings                (module — independent C++ interop)
parquet_strings                 (module — independent element domain)
parquet_temporal                (module — independent element domain)
parquet_maml_base               (module — generated)
└─ parquet_maml_base_add_col_qc (submodule)
parquet_wrapper.cpp             (C++ TU)
```

**`parquet` is a facade module and holds almost no code.** `src/parquet.f90` re-exports
`parquet_core`, `parquet_tables`, `parquet_columns`, `parquet_strings`, `parquet_temporal` and
three types from `parquet_maml_base`, so a user writes exactly one `use parquet`. Four rules
follow, and all four are easy to violate by reflex:

- **The core API's spec file is `src/parquet_core.f90`, not `src/parquet.f90`.** Every
  `public ::` line, every interface body, every shared `parameter` reached by host association
  from a submodule lives there. A note elsewhere in this file saying "declared in `parquet.f90`"
  and meaning the reader/writer spec means `parquet_core.f90`.
- **The facade must not gain library logic.** It holds `cversion` and `parquet_get_version` —
  which is where they belong, being about the library itself, and which is also what keeps the
  file needing cpp preprocessing and keeps `module/parquet.html` from rendering as an empty page.
  A new public procedure goes in `parquet_core` (or the relevant sibling) and is re-exported for
  free.
- **The facade uses bare `use <sibling>` with DEFAULT-PUBLIC accessibility**, deliberately — that
  is what re-exports a whole module without maintaining a ~120-name `public ::` list. Its three
  `private ::` statements (`c_int`, the two version bindings, `cversion`) are the only thing
  keeping implementation details out of the user's namespace, so anything new the facade imports
  for its own use needs its own `private ::` line. `parquet_bindings` is never re-exported.
  `parquet_maml_base` is imported with an `only:` list rather than in full, because its other
  public names are this library's own embedded MAML fixtures, not user API.
- **`parquet_core` is internal and documented as such** (README's API-stability bullet,
  `doc/pages/table.md`, and the module's own doc-comment). Only `use parquet` carries the
  semantic-versioning promise. Sibling modules must `use parquet_core`, never `use parquet` —
  the facade uses *them*, so the reverse is a circular dependency and will not compile.

`test/test_examples.f90`'s `test_facade_covers_every_layer` is the regression test: that whole test
module's only library import is a bare `use parquet`, so a dropped re-export breaks the *build*
rather than an assertion.

Reserved for future element-domain work (not yet implemented): `parquet_map`/`parquet_list`/
`parquet_struct` (independent modules, like `parquet_temporal`) plus their own
`parquet_read_*`/`parquet_write_*` type-family children — see CONTRIBUTING.md's "Features
considered but not implemented" for scope/status.

**Placement rule for a new read/write specific or shared helper:** type-generic code (used by
more than one of numeric/string/temporal) belongs in the parent (`parquet_read`/`parquet_write`)
as an ordinary contained procedure — descendants reach it by host association, no interface
needed. Type-specific code belongs in the matching child, also as an ordinary contained
procedure. A new *public* generic's specifics, and any type-bound binding target, must keep
their interface declared in `parquet_core.f90` itself regardless of family (see "A module procedure
cannot implement its own submodule's spec-declared interface" below for what breaks if you
relocate one incorrectly) — never assume a helper is safe to relocate purely from its call sites
without also checking those two disqualifiers, plus whether it's itself `public ::`-exported.

**A known gfortran 15.2.0 ICE to watch for when adding a new cross-subtree call into this
tree:** calling `parquet_parse_protected_cols` (declared in `parquet_core.f90`) directly from a
submodule nested **two levels** under `parquet` (e.g. `parquet:parquet_metadata:
parquet_metadata_maml`) crashes the compiler with an internal compiler error — isolated to this
one procedure's exact argument shape (an assumed-length `character` array paired with a
deferred-length allocatable `character` array result, i.e. `character(len=*), intent(in) ::
lines(:)` + `character(len=:), allocatable, intent(out) :: names(:)`) called from 2+ levels of
nesting. Worked around via a thin relay: `parquet_metadata.f90` (the level-1 parent, where
calling it already works) exposes a plain contained subroutine
`parquet_parse_protected_cols_relay` that just forwards to it, and the grandchild calls that
relay instead of reaching two levels up directly. If a *different* procedure with a similar
deferred-length-character-array-result shape hits the same ICE from deep nesting, use the same
relay pattern — this bug is narrow (only this one procedure's exact shape is known to trigger
it; other similarly-shaped procedures at the same nesting depth compile and run cleanly).

### Group interface bodies into commented `interface` blocks

Declare the `module subroutine`/`function` interface bodies in `parquet_core.f90` (and in any
submodule spec that hosts relocated interfaces) as **several small `interface … end interface`
blocks grouped by concern**, each introduced by a one-line plain-`!` banner (e.g.
`! ---- Read column specifics (by type x access mode) ----`) — not one monolithic block.
Fortran allows arbitrarily many interface blocks, so this costs nothing and keeps the
declarations navigable. The banner **must be a single-bang `!` comment, never `!>`**: a `!>`
immediately above the first `module subroutine` in a block is a predoc and FORD would attach it
to that procedure. Keep each new interface body in the group that matches the submodule
implementing it. Do not give the blocks `generic-spec` names (e.g. `interface foo`) purely for
labeling purposes — that declares an actual named generic interface (a callable overloaded
entry point requiring every member to be a distinguishable-argument-list overload of the
others, and unable to mix `subroutine`/`function` specifics under one name), not a label. The
plain-`!` banner is the only naming mechanism for these groups.

### A module procedure cannot implement its own submodule's spec-declared interface

A `module procedure`/full-restated `module subroutine` body must live in a **descendant** of
whichever spec declared its interface — never in the same submodule that declares it. This
matters when relocating a private helper's interface (per the Placement rule in "Nested
submodule tree" above): if the helper's *body* already lives in the same file the interface is moving into
(e.g. relocating an interface from `parquet_core.f90` into `parquet_metadata`'s own spec, when the
body already lives in `parquet_metadata.f90` itself, not one of its children), keeping it a
`module procedure` no longer works — gfortran reports errors like "Symbol ... has already been
host associated" or, for a still-public name, a `public ::` failure. Convert that one procedure
to a **plain contained procedure** instead (drop `module`, restate the full signature with its
own `!>`/`!!` doc-comment, since there's no longer an interface to hold it): callers in the same
file are unaffected, and descendants still reach it by host association (confirmed on gfortran 15
for both a module→submodule and a submodule→sub-submodule chain). This is independent of, and
does not need, the interface remaining declared anywhere — see `parquet_parse_col_map`
(`parquet_metadata.f90`) and `parquet_check_read_row_count` (`parquet_read.f90`) for worked
examples.

### Naming conventions

Follow these when adding new public API, types, or internal helpers:

- **Public module-level API** (anything in `src/parquet_core.f90`'s `public ::` list — functions,
  subroutines, types) always carries the `parquet_` prefix, e.g. `parquet_get_metadata`,
  `parquet_open_reader`, `parquet_schema`.
- **One prefix per module, applied to everything public in it — and `parquet_` is not the only
  one.** `parquet_` is for the parquet-file-facing modules (the reader/writer/schema/table/element
  domains: everything listed under "Nested submodule tree"). **`pf_`** — for parquet-fortran, the
  library as a whole — is for *library-wide utility* modules whose subject is not a parquet file at
  all. `parquet_sorting` (a general-purpose sorting API over plain Fortran arrays) is the first and
  currently only `pf_` module: its procedures are `pf_sort`,
  `pf_argsort`, `pf_permute`, …, and its type is `pf_sort_keys`. **Do not "correct" a `pf_` name to
  `parquet_`** — nothing in `tools/check_source_conventions.py` enforces either prefix, so the rule
  lives here and nowhere else. Two things this rule is *not*: it is not a licence to mix prefixes
  inside one module (pick one and apply it to every public name there), and it is **not** a reason
  to rename the existing library-level `parquet_`-named procedures (`parquet_get_version`,
  `parquet_kind_name`), which are deliberately left alone — renaming them would be a public API
  break for a naming preference.
  Note the constraint that shapes such a module's own name: **a module cannot share its name with a
  procedure it declares**, which is why the module is `parquet_sorting` and not `parquet_sort` (see
  the `parquet_strings`/`parquet_string` bullet further down).
- **Type-bound procedures** (`schema%init`, `schema%add_field`, `reader%...`) do *not* need a
  `parquet_` prefix — the type itself namespaces them. If the natural short name collides
  with another type's backing implementation, keep the short name as the type-bound binding
  target and give the private module procedure a distinguishing name (e.g.
  `parquet_column_info`'s binding `set_column_available => set_available`, kept short only to
  avoid colliding with `parquet_schema`'s own `set_column_available` impl).
- **`maml_` prefix** is reserved specifically for MAML-parsing/building internal helpers
  (e.g. `maml_push_line`, `maml_line_exists`) — don't reuse it for unrelated internal code.
- **Other private module-level helpers** (in `parquet_metadata.f90`, `parquet_read.f90`,
  `parquet_write.f90`) generally keep the `parquet_` prefix too, even though private —
  matches the existing majority convention in those files; only give it a bare, unprefixed
  name if it's a small, obviously-local helper (rare; check for existing precedent first).
- **Private module-level types** (not part of the public API, e.g. `maml_section_schema`)
  drop the `parquet_` prefix — this is intentional, not an inconsistency to "fix".
- **C++ bindings** (`parquet_bindings.f90` interfaces, `parquet_wrapper.cpp`) mirror the C++
  side's own naming (still generally `parquet_`-prefixed for the `extern "C"` surface) —
  don't rename these to match Fortran-side conventions. **One exception, and the rule for
  creating another:** `parquet_core.f90` does an unrestricted `use parquet_bindings`, so a public API
  procedure cannot share a name with a binding. When that collides, keep the public name and give
  the *Fortran-side interface* a `c_`-prefixed one while leaving `bind(C, name="...")` — and thus
  the linked symbol and `parquet_wrapper.cpp` — untouched; `tools/check_bindc_boundary.py` keys on
  the `bind(C, name=)` value, so it follows the rename with no change. `c_reader_set_filter`
  (bound to `parquet_reader_set_filter`, whose Fortran name belongs to the public post-open filter
  setter) is the existing instance.

- **A new module holding several related element/handle types** (as opposed to one module per
  type) should be named after the *domain* those types belong to, not any single type inside
  it — e.g. `parquet_temporal` for `parquet_date`/`parquet_time`/`parquet_timestamp`. See "The
  `parquet_temporal` module" below for the reasoning and the sibling modules (`parquet_map`,
  `parquet_list`) this leaves room for.

- **A module cannot share its name with a type (or a procedure) it declares** — gfortran rejects
  it outright. This has bitten twice: it is why the `parquet_strings` module is plural while its
  type is `parquet_string` (see "The `parquet_strings` module" below for that instance), and it
  constrains a generated table type's MAML, where `dataset:` names the module and `table:` derives
  the type. Check the pair whenever you name a module after what it holds.

When in doubt, grep for an existing analogous name before inventing a new convention.

### Public numeric arguments: provide both int32 and int64 kinds

When adding a public procedure argument that holds a row count / size / index (any integer a
caller might naturally declare as a plain `INTEGER`), make it generic over **both**
`integer(int32)` and `integer(int64)`, following the existing `parquet_get_nrows_int32`/
`parquet_get_nrows_int64` overload pattern. An `integer(int64)`-only dummy forces callers
with a default-kind `INTEGER` variable into a `Type mismatch ... passed INTEGER(4) to
INTEGER(8)` compile error.

**The rule only applies when the value can legitimately exceed int32.** It exists so a caller is
never forced to widen a variable the library could have accepted as-is — not as a blanket style
requirement on every integer argument. An argument whose value is bounded below `huge(1_int32)` by
the format, by Arrow, or by the library's own guards stays a single default-kind `integer`, and
adding a second kind for it would be noise. `parquet_open_writer`'s `chunk_size` is the worked
example: it is a row-group row count, and a row group cannot hold more than int32 rows (see
"Guarding a hard Arrow int32-only ceiling"), so there is deliberately no `_int64` form and its
absence is not a defect. When declining the rule on these grounds, say so in the argument's own
doc-comment, so the next reader does not "fix" it.

Fortran constraint that shapes this: an *optional* dummy that differs only by kind cannot be
the sole disambiguator between specific procedures in a generic interface (a call omitting it
is ambiguous). So when such an argument is optional, carry the argument-absent case as its
own separate specific rather than an optional dummy — see `parquet_open_reader`'s split into
`parquet_open_reader_base` (no `nrows`) plus `parquet_open_reader_nrows_int32`/`_int64`
(required `nrows`), all under one generic interface.

For a *required* (non-optional) argument, this ambiguity constraint doesn't apply — Fortran can
disambiguate two specifics differing only by a required argument's kind without any special
handling, so just add the second kind-specific directly (no base/kind-suffixed split needed),
sharing one private `_impl` worker between the two (mirrors `add_col_qc_impl`'s existing
shared-worker pattern) — see `parquet_read_array_row_mode`'s `row_index` (12 specifics: 6 data
types x `integer(int32)`/`integer(int64)` row_index, each pair delegating to one
`parquet_read_<type>_array_row_mode_impl`).

### A new process-global parameter goes in `parquet_settings`, and a design doc must say so

**Any parameter that is global to the library and that a user could reasonably want to change belongs
in `src/parquet_settings.f90`** — not as a `parameter` buried in the module that happens to use it,
and not as a new argument threaded through a call chain. That module is the single place a program
looks to find out what the library will do, and the single place `parquet_print_settings`,
`parquet_reset_settings` and `parquet_settings_from_env` can reach.

**The admission test is one sentence: a setting may change how FAST, how LARGE or how LOUD the
library runs; it may never change what the library ANSWERS.** A program-wide default for something
like null ordering, quality-control enforcement or a numeric tolerance is deliberately absent and
must stay absent — it would make the same call return different results in different programs, with
nothing at the call site to hint at it. Anything expressible as an argument to a specific call (a
writer's `compression=`, a sort's `threads=`) belongs there instead, and an explicit argument always
wins over a setting.

Most internal constants fail that test and should stay where they are. Vocabulary (accepted token
lists), mathematical facts, format ceilings imposed by Parquet or Arrow, container implementation
details, and input-sanity bounds are **not** settings. The last of those is worth stating outright:
the `parquet_max_*` limits are published as read-only constants precisely because making them
settable would convert a guard against runaway input into a way to overflow a parser's own stack.

**Whenever a `feature_*.md` design or implementation document is written, it must contain a settings
analysis** — a short, explicit section answering: does this feature introduce any process-global
parameter, does each one pass the admission test, and if so what is its knob name, its default, its
validation and its environment variable. Say "none" when the answer is none; an absent section reads
as "not considered". This is a standing obligation on every future feature document, in the same way
tests and docs are standing obligations on every feature.

Five rules apply to a knob once it is admitted, and each exists because breaking it fails silently:

- **A round-trip test is not a test of a setting.** Set-then-get passes just as happily against a
  value that is stored and never read. Every knob needs three assertions: its default, its round
  trip, and an **observed effect with a negative control** — something measurable that differs
  between the default and the set value, *and* the same observation at the default showing the other
  outcome. See `feature_risks.md` Risk-41, and `tools/check_source_conventions.py`'s
  `check_settings_are_read` for the static half.
- **Every knob must be resettable, printable, documented and reachable from the environment.** Three
  of those four are enforced statically: `check_print_settings_documented` and
  `check_env_covers_every_setting` both take their knob list from `parquet_print_settings`' own
  printed rows, and `check_settings_are_read` works from the `cfg_*` declarations instead. Resettable
  is the one a lint check cannot see, so it is asserted by a test (`test_reset_all_knobs`) that sets
  every knob to a non-factory value first. The point of sharing the printed rows is that a new knob
  fails several checks at once rather than needing several people to remember several lists.
- **A knob whose value the C++ side needs is MIRRORED, and the mirror has rules**: values cross the
  `bind(C)` boundary already **resolved** (no tokens, no "0 means default" sentinels — Fortran
  decides, C++ obeys), one push function per group rather than one per knob so a reset cannot
  half-restore, and the C++ globals' initialisers must equal the Fortran defaults because they are
  what applies before the first push. See `feature_risks.md` Risk-42.
- **Do not add a second way to set the same thing.** A test-only `parquet_debug_set_*` override for
  a value that is now a real setting is a second writer, and the two can disagree; three such hooks
  were retired for exactly this reason. Observation hooks (`parquet_debug_get_*`) are fine and are
  usually how a knob's effect is asserted at all.
- **Renaming a public setting is a semantic-versioning event.** The `use parquet` surface is covered
  by the promise in README.md, so check whether the name appears in a *published* CHANGELOG section
  before renaming it — and never edit a published section, which records what that release actually
  shipped. A Fortran-side rename is free on the C++ side: `bind(C, name=...)` decouples the two, and
  `tools/check_bindc_boundary.py` keys on the bound name.

### Role-A MAMLs live in `table_types/`, not `schemas/`

Two directories hold `.maml` files, and they are read for opposite purposes. `schemas/` describes
files the library **writes** (and is globbed wholesale into `src/parquet_maml_base.f90` by
`tools/generate_parquet_maml.sh base`, so anything dropped in there becomes a compiled-in fixture).
`table_types/` holds **Role-A** schemas — the input to `tools/generate_user_table_code.py`, which
turns each one into a named `parquet_table` extension type. A Role-A MAML put in `schemas/` by
mistake is not an error and does not fail anything; it just silently becomes an embedded base
fixture it was never meant to be, and shows up in `parquet_maml_base`'s generated accessor list.

Both generators take `--dir=` with those names as defaults, so a downstream project gets the same
split without being forced into it.

### MAML fixture directory: `schemas/`

`.maml` example/fixture files live in `schemas/`. `tools/generate_parquet_maml.sh` accepts
`--dir=<name>`/`--dir <name>` (default `schemas`) so downstream projects embedding their own
MAML schemas aren't forced to match this project's convention — see
`doc/pages/embedding-maml-schemas.md` for the user-facing how-to.

### Reading MAML source files: shared helper, line-length limit, CRLF handling

Any code path that reads a `.maml` file's lines from disk (`parquet_load_maml_file`,
`parquet_load_qc_maml_file`, or a future one) should go through the shared
`parquet_read_maml_source_lines(filename, context, lines, nlines)` subroutine in
`parquet_metadata_maml.f90` rather than writing its own read loop. It handles two things a
hand-rolled loop easily misses:

- **A per-line length cap.** Lines are read into a fixed `character(len=maml_max_line_len)`
  buffer (`maml_max_line_len = 1024`, declared once in `parquet_metadata.f90` and host-associated
  to descendants — don't hardcode `1024` again elsewhere). A line longer than this is detected via
  non-advancing read + `size=`/`iostat_eor` (not silently truncated with `iostat == 0`, which was
  the original bug this helper fixes) and aborts with a clear message naming the offending line
  number, rather than letting a truncated `keywords:`/`protected_cols:` line validate and write
  incomplete metadata into the output file.
- **CRLF transparency.** A trailing `char(13)` (from a Windows-edited `.maml` file) is stripped
  before the line is returned — plain Fortran `trim()` does not remove it, so without this a
  CRLF file produces a confusing "invalid data_type"-style error for a value that looks visibly
  correct to a human reading the file.

If a future MAML-adjacent feature needs to read a `.maml`-like file's lines directly, reuse this
helper (or extend it) instead of duplicating the read loop — that's exactly the class of bug it
was introduced to close off project-wide.

### Error stop messages: include file/schema context

New `error stop` messages in the read/write/schema-building paths should append the relevant
file and, where applicable, schema/maml name using the existing helpers — `writer_context_suffix`
(`parquet_write.f90`), `reader_filename_suffix` (`parquet_read.f90`), `maml_name_suffix`
(`parquet_metadata.f90`) — rather than naming only the offending column/field, so a failure is
identifiable when several readers/writers/schemas are in play at once. These are only
meaningful once the reader/writer/schema knows its file/name (i.e. post-open), so the
guard-clause "…has not been opened" messages are exempt.

### Filter evaluation: a Null is UNKNOWN, a NaN is a VALUE — the two behave oppositely

`eval_filter_clause` (`parquet_wrapper.cpp`) gives a Null row `kUnknown` for every comparison and a
NaN row an ordinary `kTrue`/`kFalse`, and that difference is deliberate in both directions. IEEE
makes every comparison against a NaN false, so a NaN row is **excluded** by `>`/`>=`/`<`/`<=`/`==`
but **survives** `/=` and any negated comparison — precisely where a Null row does the opposite
(`kUnknown` negates to `kUnknown`). Both halves are load-bearing and neither is a rounding error to
be "harmonized":

- **Nullness is governed solely by `is_null`/`is_not_null`.** They are the only clauses that answer
  `kTrue`/`kFalse` for a Null row. `is_nan`/`is_not_nan` deliberately do **not** join them — a Null
  row is `kUnknown` for both, so `x is_not_nan` means "is a real number", not "is not a NaN,
  whatever else it may be". Making either one two-valued on Null would quietly let Null rows into
  filters that never mention nullness, which is the back door the Kleene design exists to close.
- **`is_nan`/`is_not_nan` are restricted to `FLOAT`/`DOUBLE`/`HALF_FLOAT`** and `error stop` on
  anything else. `DECIMAL*`/`UINT64` also reach the comparison arms as doubles but can never *hold*
  a NaN, so accepting them would answer a constant for what is almost certainly a mistyped column.
- **A bare `nan` comparison value is rejected; `inf` is not.** `parse_double_strict` is `strtod`, so
  both parse — but `x == nan` can only ever match nothing and `x /= nan` everything non-null.
- **`is_nan` is expressible without the operator**, as `not (x >= 0 or x < 0)` (every non-NaN real
  satisfies exactly one disjunct; a NaN neither; a Null is unknown for both). `test_filter.f90` uses
  that equivalence as an independent oracle for the operator — a good pattern to copy for any future
  operator that is sugar over the existing grammar, and a ready-made expression for exercising the
  NaN path of anything that reasons about clauses without evaluating them.

**The last point has a sharp consequence for the row-group statistics screen** — see the next
section, which states the rule as implemented.

### The row-group statistics screen: every uncertainty must DECLINE

`screen_row_groups` (`parquet_wrapper.cpp`) reads each row group's footer statistics and skips the
row groups a filter provably cannot match. It is the only thing in the reader whose failure mode is
a **silent wrong answer** rather than an abort: a wrongly pruned row group's rows simply never
appear, with nothing to notice. Six rules keep the failure direction at "prune nothing", and every
one of them is the kind a later simplification would delete:

- **Every gate returns `kScreenAnything` (`{may_true, may_false, may_unknown} = all true`), never a
  guess.** Statistics absent, an unusable ordering, an unsupported type, an unparseable literal —
  all decline. A declining leaf prunes nothing and, per the combinators, cannot make anything else
  prune either.
- **`AND`'s `may_true` is an over-approximation and must stay one.** `a.may_true && b.may_true`
  says "some row satisfies `a`, and some row satisfies `b`" — not necessarily the *same* row.
  Row-group statistics are per-column marginals with no joint information, so nothing better is
  available; tightening it is a bug.
- **For a `FLOAT`/`DOUBLE` leaf, `may_false` is unconditionally `nn > 0`, and `/=`'s `may_true` is
  too.** Parquet excludes NaN from min/max and records no NaN count, so the bounds can rule a NaN
  neither in nor out — and a NaN is an ordinary value that compares *false* (see the previous
  section), so it makes every comparison false while sitting outside `[min, max]`. `NOT` consumes
  `may_false`, so the ordering-derived form prunes row groups that do match: `{1.0, 2.0, NaN}` under
  `not (x > 0.5)` matches the NaN row while `min = 1.0 > 0.5` claims nothing can be false. Reachable
  without `is_nan` at all, since `not (x >= 0 or x < 0)` *is* `x is_nan`.
- **The screen walks the SAME postfix node list as `evaluate_nodes`**, with the same stack shape,
  and takes each leaf's family from the same Arrow schema expression the evaluator dispatches on.
  Drift between the two is the second-biggest risk after the rules themselves; keep them adjacent
  and keep the walks structurally identical.
- **Two guard pairs are individually redundant and jointly load-bearing**, exactly as
  `column_has_nulls_from_footer` records for its own: `is_stats_set()` + a null `statistics()`
  (removing both segfaults on `test/fixtures/no_stats.parquet`), and the `sort_order() ==
  SortOrder::UNKNOWN` check + the `sort_order()` SIGNED/UNSIGNED check a few lines below it
  (removing both mis-reads an unsigned column's bounds as signed). Do not delete either half on
  the strength of a coverage report. (Note: `ColumnDescriptor` has no `can_use_min_max()` method
  in any Parquet C++ release found on this machine — verified against versions 19, 22, 23, and 24;
  the first guard reads `sort_order()` directly instead.)
- **A row group's mask segment is all-false when it is pruned**, which is why nothing downstream
  needs to learn a new concept — a pruned row group simply *is* an empty one, which
  `row_group_effective_rows` and every row-group-scoped operation already handle.

Two structural notes for anyone extending this. `live_mask` is `filter_mask` restricted to the live
row groups' rows and is what `apply_row_transform` filters with, because every whole-column decode
now reads only those row groups; `filter_mask` stays the canonical full-length object everything
row-group-indexed uses. And **the unpruned path deliberately still calls `ReadColumn`** in
`get_single_chunk_array` rather than routing through `read_live_row_groups` for symmetry: measured
at 3.4% of a whole filtered read (reproducible to 0.1%), because `ReadTable` reconstructs a Table
and its schema per call. Do not "unify" those two branches without re-measuring.

Testing this needs both halves: an **A/B equality** against
`parquet_debug_set_disable_statistics_prescreen(1)` over the same fixture, *and* an assertion on
`parquet_debug_get_row_groups_pruned()` — equality alone passes just as happily against a screen
that never prunes. Both hooks are process-global, which is why `test/run_tester.f90` excludes the
`filter_screen` suite from its per-test parallelism.

### Guard mutating public procedures against being called twice

When adding a new type-bound procedure or public subroutine that mutates a `parquet_writer`/
`parquet_reader`/`parquet_schema`'s state, consider whether a caller reusing the same object
across two calls (a loop, a copy-paste mistake, a refactor) could silently corrupt state, leak a
resource, or crash instead of getting a clean error. Default to a check-before-mutate guard: test
the relevant flag/allocated-component first and `error stop` with a message like
`"<procedure>: already called for this <writer/row group/...>"` before any mutation happens,
rather than silently overwriting state or leaving the object in an inconsistent, only-later-
surfacing-as-a-crash condition. See `parquet_write_row_mask_impl`/`parquet_write_chunk_row_mask_impl`/
`parquet_new_row_group_impl` (`parquet_write.f90`) for the established pattern. Not every mutating
procedure needs this — e.g. a procedure whose second call is genuinely idempotent (rebuilds the
same state from the same inputs, like `parquet_validate_user_maml`) doesn't need a guard — but
default to adding one unless a call is provably idempotent.

### Implicit finalizers must never route through a path that can throw/abort

A `FINAL` procedure (e.g. `writer_finalize`/`reader_finalize`) can run at unpredictable points
(variable reuse via an `intent(out)` re-open, scope exit, an early `RETURN`) with no way for a
caller to see or handle a failure. Never have a finalizer call a close/cleanup path that performs
completeness or validity checks capable of throwing a C++ exception across the `extern "C"`
boundary (which crashes the whole process — see "`src/parquet_wrapper.cpp`: GCC vs Clang gcov
attribution"'s notes on uncaught exceptions) or issuing an `error stop`: an implicit finalizer
should always succeed silently, freeing/abandoning resources without validating the object's
completeness. If the normal close path (e.g. `close_parquet_writer`) has such checks, give the
finalizer its own dedicated "abandon" entry point that skips them entirely — see
`abandon_parquet_writer`/`writer_finalize` (`parquet_wrapper.cpp`/`parquet_write.f90`) for the
pattern — rather than trying to have the finalizer conditionally decide when it's "safe" to call
the real close. Apply the same pattern to any future finalizable type (e.g. a `parquet_reader`-side
completeness check, if one is ever added).

### Automatic BYTE_STREAM_SPLIT for float columns in the writer

`apply_float_byte_stream_split` (`parquet_wrapper.cpp`) is called at both places `WriterProperties`
get built (the first-row-group path in `parquet_finish_row_group`, and `close_parquet_writer`'s own
`WriteTable` path) and, for every `float32`/`float64` field, calls `disable_dictionary(name)` *and*
`encoding(name, Encoding::BYTE_STREAM_SPLIT)` on that same column. This is automatic and type-based
— not exposed as a public argument — because dictionary encoding rarely helps floating-point data
(samples are usually near-unique) while byte-stream-splitting each value's bytes across separate
per-position streams compresses substantially better under most codecs; every other column type is
left at the writer's normal defaults (dictionary enabled, no BSS).

**The `disable_dictionary` + `encoding(..., BYTE_STREAM_SPLIT)` pair must always be applied
together, on the same set of columns, never one without the other.** Confirmed directly from
`parquet/properties.h`: `Builder::encoding(path, type)`'s own doc comment states it "is only
applied if dictionary encoding is disabled" for that column — requesting BYTE_STREAM_SPLIT while
dictionary stays enabled for that column is a **silent no-op**, not an error, and produces a file
that still uses ordinary dictionary encoding despite the (ineffective) BSS request. If a future
change relocates or refactors this logic, keep both calls paired and keep calling
`apply_float_byte_stream_split` at **both** `WriterProperties::Builder` construction sites listed
above — the debug/test-only fixture writers elsewhere in this file (`parquet_debug_write_*`) are
deliberately not included, since they bypass the normal schema-driven writer path entirely.
Verified empirically (not just from the header comment) via a scratch program writing a
float32/float64/int32 file and inspecting `pyarrow.parquet.ParquetFile(...).metadata`'s
per-column `encodings`: the float columns report `('RLE', 'BYTE_STREAM_SPLIT')` with no
`RLE_DICTIONARY`, while the int32 column keeps `('PLAIN', 'RLE', 'RLE_DICTIONARY')` — this
library has no reader-side API to introspect a file's physical encoding, so `pyarrow` (or another
external tool) is the only way to confirm this end-to-end; a pure test-drive/Fortran test can only
confirm the *data* round-trips correctly, not which encoding was used to store it.

### A character ARRAY is trimmed on the way into a column; a character SCALAR is not

Every element of a `character(len=*)` array shares one declared length, so a shorter value is
blank-padded by Fortran and the padding cannot be what the caller meant. A `character(len=*)`
**scalar** is exactly as long as the caller wrote it. So:

- **Array arguments trim** — `parquet_column%set_all`/`%append_values`, `parquet_table`'s
  `%add_column`/`%set`/`%set_slice`, and `%set_element`'s *vector* form (whose `value(:)` is an
  array). `refill_string_store` (`src/parquet_columns_string.f90`) is where the rule is implemented
  and explained.
- **Scalar arguments do not** — `%set_at`'s scalar form, `%set_element` on a `PK_STRING` column, a
  row handle's `%set`.
- **`parquet_string_column`'s own API never trims by default**, because it takes bytes the caller
  controls exactly and offers explicit `trim=`/`strip=`.

Do not "harmonize" these into one behaviour: the asymmetry *is* the rule, and it is what makes
`%get` into a `character(len=:), allocatable` come back sized to the longest real value rather than
to whatever width the caller happened to declare. `%add_column` documented the trimming from 1.0.0
and did not do it until this was fixed, so the doc-comments now state the rule rather than just the
behaviour.

### Validity is per ELEMENT, and a vector row is not one bit

`parquet_column`'s validity API comes in a **row** form and an **element** form, and the storage has
always been `width * nrows` bits. Every query and mutation exists in both shapes — `is_null(i)` /
`is_null(i, e)`, `set_null(i)` / `set_null(i, e)`, `clear_null(i)` / `clear_null(i, e)` — with the
element index bounded by `width` (`check_element`), never a flattened `(row-1)*width + element`
position.

**The row and element forms are deliberately asymmetric where they differ, and that asymmetry is the
rule to preserve:**

- A row **QUERY** answers about the row as a whole: `is_null(i)` is `.true.` when **any** element of
  row `i` is null, and `row_validity` builds that summary. Costs O(width) with an early exit.
- A whole-row **MUTATION** acts on every element: `set_null(i)`, `clear_null(i)` and `append_nulls`
  mark the entire row. Naming only a row says the row is missing.
- **`modify_nulls=.false.` protects individual null ELEMENTS**, not whole rows: a vector row with one
  null element still has its other elements written.

Each operation acts at the granularity the caller named — that one sentence generates all three.

**Shapes must match on every paired API.** A rank-1 `values` takes a rank-1 `is_valid`; a rank-2
`values` takes a rank-2 `is_valid`, shaped `(width, nrows)`. This holds for `parquet_read_column`,
`parquet_write_column` (both always did), and now for the table's `%get`/`%col`/`%get_slice`/`%set`.
There is no rank-1 form for a vector column and no widening anywhere. The **standalone** mask APIs
are the deliberate exception, because they have no values to match: `%get_valid_mask` and
`%set_null(mask)` accept **either** rank, where rank-1 is the row summary ("which rows are
complete?") and rank-2 the true element state. Both are unambiguous because the mask is a required
argument there — do not "fix" that asymmetry.

**When walking the bitmap in bulk, iterate the SET BITS, not all 64 positions of a nonzero word.**
`row_validity`/`element_validity` use `trailz` + `ibclr`. Testing every position instead makes a
densely-null wide column cost `width` times more — measured at 2.7x on a width-16 column that is
half null, against 2.2x *faster* than the pre-element-null implementation with the bit-scan. The
zero-word skip is what keeps the null-free path free and must stay.

**Do not reintroduce widening in a new read or write path.** A per-element null read from a file is
stored as such (`set_validity` writes the whole mask in one pass rather than replaying `width*nrows`
setter calls), and a table write hands the element mask straight to the writer. The round-trip test
in `test/test_table.f90` (`test_element_null_round_trip`) is what catches a regression, in both
directions at once.

### Auto-threading: `omp_in_parallel()` picks a DEFAULT, and that is not the guard CLAUDE.md warns about

Two places decide on their own how many threads to use — `parallel_prefetch_ok`
(`parquet_tables_read.f90`, for the table's internally-parallel read) and `pf_sort_threads`
(`parquet_sorting_keys.f90`, for every sort). **Both resolve to serial inside an OpenMP parallel
region**, and both do it with the same two lines:

```fortran
if (omp_get_max_threads() <= 1) return   ! or: n = 1
if (omp_in_parallel()) return            ! nested regions are the caller's business
```

This does **not** contradict "Never key a guard on `omp_in_parallel()` alone" below. That rule is
about a guard that *refuses* an operation, which under test-drive's own `!$omp parallel do` fires
across the entire suite. These refuse nothing — they choose a default, and an explicit request
(`threads=8`) is still honoured inside a parallel region. Keep the distinction when adding a third
such decision, and reuse `pf_sort_threads` rather than writing a fourth copy of the rule:
`omp_get_max_threads()` reads an ICV, not the current team size, so inside an 8-thread region it
answers 8 and a missing check means 8x8 threads.

### `parquet_table` concurrency: one file owns the OpenMP plumbing, and guards key on OWNERSHIP

`src/parquet_tables_parallel.f90` holds the table's lock, the append/read counters and the shared
refusal every structural mutation goes through, so that `#ifdef _OPENMP` and `use omp_lib` appear in
exactly one file. The two exceptions are `unsafe_first_touch`/`record_open_thread`, which stayed in
`parquet_tables_read.f90` next to the materialization path they guard. All three implement the SAME
ownership test and must agree: *a table this very thread opened inside the current parallel region is
thread-private and exempt; anything else may be shared.*

Rules a change here must not break:

- **Never key a guard on `omp_in_parallel()` alone.** test-drive runs its own tests inside
  `!$omp parallel do`, so that fires suite-wide (see "Tests run concurrently"). Ownership is the
  precise question, and refusing a thread-private table would make the whole slice regime unusable.
- **`table_append_table` and `table_append_row` each take the lock exactly once and then call
  `append_table_worker`; neither calls the other.** An OpenMP simple lock is not recursive, so a
  second acquisition on one thread deadlocks rather than failing to build. A new internal caller
  goes to the worker.
- **A lock is a HANDLE, not a value.** `%clone` builds a fresh one (`clone_new_cache`), never a copy
  of the source's, and `table_finalize` destroys it — via `table_destroy_lock`, which validates
  nothing and cannot abort, because a finalizer must always succeed silently.
- **The read path stays free of atomics.** `table_check_no_append` (one atomic read) sits in
  `table_resolve`, the single choke point every value accessor goes through; the `readers_active`
  counter is taken only around the *long* windows (a lazy first touch, and `materialize_marked`),
  never around a resident read or a per-element accessor. Two atomics per cell would dominate a
  `%get_element` loop. That asymmetry is deliberate and is documented on the cache fields.
- **`table_resolve(..., writing=.true.)` is how a write declares itself**, which is what gives every
  generated `%set`/`%set_element` specific the string-column rule from one place. A new write
  specific inherits it by copying its neighbour's call.
- **Validity is allocated lazily, so the FIRST null races** — guarded at the table layer (where
  thread ownership is reachable), never in `parquet_columns`, which is a standalone module with no
  thread knowledge. `%ensure_validity` is the escape hatch. Three dispatch classes, and only two
  can race: bitmap kinds (`ensure_bitmap`), string kinds (`parquet_string_column`'s own
  `ensure_validity_cap`), and temporal kinds — which allocate nothing and must NOT be refused.
- **The internally-parallel `%prefetch` gives each thread its own reader** and is gated by
  `parallel_prefetch_ok`. Every clause there is a correctness or cost rule, not a tuning knob; the
  sharpest is that an **unseeded `sample_fraction=` would make each per-thread reader draw a
  different subset**, so columns read by different threads would hold different rows — a silent
  wrong answer. Do not relax that gate without re-reading its own comment.
- **Arrow's own per-column threading is left enabled inside that region.** Measured both ways on a
  24-column x 900k-row file with 8 OpenMP threads: 0.037-0.040 s nested, 0.046-0.047 s with
  `use_threads=.false.`. Nesting the two is faster, so the "one level of parallelism only" instinct
  is wrong here. Re-measure before changing it.
- **Every new guard needs a NEGATIVE control**, not just an error scenario. A guard that fires
  unconditionally passes every abort test ever written for it while breaking the permitted case;
  `test_table_private_mutation_allowed` (`test/test_openmp.f90`) is the pattern.

### A `parquet_table` pointer does not survive a ROW-structural mutation

`%col` hands back a live pointer into a column's storage, and `%filter_rows`, `%sort_by`, `%top_n`,
`%delete_rows`, `%truncate`, `%append` and `%append_null_rows` all reallocate that storage
(`delete_by_mask`, `reindex`, `gather` and `append` each grow or shrink exact-fit). A pointer taken
before one of them therefore points at freed memory afterwards, and **Fortran offers no way to
detect this** — the code compiles, and usually appears to work.

Two consequences for future work here:

- **Any new mutation that changes the row set inherits this**, so it belongs in
  `parquet_tables_rowmutate.f90` next to the others, and its doc-comment should say it detaches.
  The file/`%col` split (`..._mutate.f90` never changes the row set, `..._rowmutate.f90` always
  does) is what keeps the rule checkable by looking at which file a procedure is in.
- **A row-structural mutation skips a column that is not resident** (`table_mutable_column`)
  rather than refusing to run, which is what lets a lazy table drop rows without first reading
  every column it has. The skipped column is then unreadable for good, and the detach guard
  (`table_check_not_detached`) is the only thing that reports it — so every path that would read
  from the file after a mutation must run that guard. There are five today (`table_touch`,
  `table_resolve_width`, `materialize_marked`, `table_reload`, `table_row_group_bounds`);
  a sixth that forgets it will read through a deallocated reader.

**"Detached" means "had a file and can no longer read it", never simply "was mutated".** A table
built by `parquet_new_table` has no file to lose, so growing or reordering it must leave
`%is_detached` answering `.false.` — otherwise every from-scratch table would report itself
detached the moment it was filled. `table_detach` therefore only sets the flag when
`cache%file_backed` is still true, and never clears it.

### New `parquet_table` state goes on the CACHE — never as an allocatable component of the type

`parquet_table` itself is deliberately **five scalars and one pointer, with no allocatable
components at all**; every piece of real state (`reader`, `cols`, `rg_bounds`, `source_file`, the
read-time transform) lives in `parquet_table_cache`, behind that pointer. The file header of
`parquet_tables_lifecycle.f90` states this for the column store and gives one reason (a `%col`
pointer must outlive the dummy argument). There is a second, sharper reason, and it applies to
*any* component, not just the column store:

**`parquet_table` is FINALIZABLE, so every allocatable component it gains makes the compiler
generate a deeper recursive walk for its `intent(out)` entry and its `FINAL` — and this project has
three confirmed compiler bugs in exactly that machinery on exactly this type.** Two are documented
above and in `parquet_tables_lifecycle.f90` (gfortran leaving an OpenMP `private()` copy
uninitialized; `%detached` surviving an `intent(out)` reset). The third: hanging a
`type(parquet_schema), allocatable` off `parquet_table` — for the composed read-time transform,
which really is per-table state — **segfaulted ifx inside its own runtime**, in a block-local table
opened inside an `!$omp parallel do`, at the `intent(out)` entry of `parquet_open_table`. The
backtrace named no library code at all: unnamed RTL frames with a self-recursive PC, bottoming out
in libc, i.e. the runtime's own nested-derived-type descriptor walker following a bad descriptor
into `free()`. `parquet_schema` is the deep one (`maml` + `cinfo` + `metadata`, each holding
allocatable arrays of derived types with their own allocatable components), but the rule is not
about that type specifically.

So: **put new table-level state on `parquet_table_cache`**, where it costs nothing structurally —
the cache is a plain, non-finalizable type reached through a pointer, freshly `allocate`d per open,
so its default initializers are reliable and nothing walks it on procedure entry. Reserve
`parquet_table`'s own body for plain scalars (`regime`, `row_lo`, `row_hi`, `row_count`,
`detached`). Note the ordering consequence in `open_table_impl`: anything stored on the cache has to
be assigned *after* `allocate(table%cache)`, not before.

### Assembling a `parquet_column` from pieces: preallocate and `%paste`

`grow_storage` (`parquet_columns_mutate.f90`, generated) reallocates **exact-fit** and copies
everything already in the column — there is no capacity headroom and no geometric growth. So
`%append` in a loop is O(k²) in both copying and allocation: building a column from k pieces copies
k(k-1)/2 pieces' worth of data and allocates k(k+1)/2, ending up holding only k. This is invisible
in a unit test and only shows up at scale — it made `parquet_table`'s slice regime cost as much as
reading the whole file.

**When the final row count is known before the pieces are, `init` the column once at full size and
`%paste` each piece into place.** `%paste(src, at [, from] [, count])` overwrites an existing row
range without reallocating or changing `nrows`; `from`/`count` copy a sub-range of the source, so
trimming a piece needs no `keep` mask and no `%delete_by_mask` either. `materialize_slice`
(`parquet_tables_read.f90`) is the worked example. Two things to know before using it:

- **`%paste` REPLACES the pasted range's validity, it does not merge it** (`%append`'s rule, where
  the destination rows are always fresh, is merge). A valid source element clears a null the
  destination already had. Preserve this in any change: merging instead would leave a stale null
  sitting on top of a value that really was read, and the table path cannot catch it, because there
  the destination is always freshly `init`'d — only `test/test_columns.f90` covers that difference.
- **The string kinds are excluded and abort.** A `parquet_string_column` is a packed
  variable-length store with no fixed row slots, so it cannot be overwritten in place — but it also
  does not need to be, because `ensure_offsets_cap`/`ensure_data_cap`/`ensure_validity_cap`
  (`parquet_strings.f90`) already grow it **geometrically** (1.5x). Keep the grow-and-append shape
  for those two kinds, and don't "fix" the exclusion.

If a future kind gains its own storage, decide which of these two shapes it has before adding it to
the paste path.

### A `parquet_schema` built in code must be parsed before anything reads its fields

`schema%init` + `schema%add_field` build the schema's MAML **text** only; `schema%cinfo` stays
unpopulated until `parquet_parse_maml(schema)` runs. Calling `%get_num_fields`/`%get_field_name`/
`%is_column_set` before that reads uninitialized state — in a loop bounded by `%get_num_fields()` this
becomes a runaway allocation and an **OOM kill**, with no error message and nothing pointing at the
schema. Any new procedure that walks a caller-supplied schema should therefore guard with
`if (.not. schema%is_parsed()) error stop "<procedure>: this schema has not been parsed; call
parquet_parse_maml(schema) after building it with %init/%add_field"` before touching `%cinfo`. Note
`%is_parsed()` is the correct check, not `%is_init()` — the latter is `.true.` for exactly the
unparsed from-scratch schema this guard exists to catch.

## Element-domain modules (`parquet_strings`, `parquet_temporal`)

### The `parquet_strings` module

`src/parquet_strings.f90` is a **near**-independent module (`use parquet_strings`) providing
`parquet_string_column` (Arrow-LargeUtf8-style
offsets+data+bit-packed-validity string storage) and `parquet_string` (a non-owning handle to
one element). User guide: `doc/pages/string-columns.md`.

- **"Independent" is a direction, not a fact, and the exceptions are enumerated.** It depends on
  `iso_fortran_env`/`iso_c_binding`, and on exactly two things beyond them: `parquet_settings` (for
  `parquet_output_is_suppressed`, because `verbosity="silent"` governs solicited output wherever it
  lives, and for `parquet_get_string_threads`, because a thread cap is a setting for the same reason
  every other thread cap is), and `omp_lib` under `#ifdef _OPENMP`, since its bulk rebuilds thread
  internally. **Adding a third dependency is a decision, not a detail** — the value of this module
  being reachable without the Arrow/Parquet C++ stack is what the rule protects, and each of the two
  exceptions above was argued before it was taken. Consequence worth knowing when working here:
  because the module reaches no `bind(C)` surface at all, the C++-side debug-hook convention is
  unavailable to it, which is why its test hooks are public Fortran procedures (see "A Fortran-side
  debug hook has to be PUBLIC, so prefer a C++ one").
- **The module is `parquet_strings` (plural) on purpose.** A module and a type cannot share a
  name in gfortran (`public :: parquet_string` binds to the module, and the type declaration
  then conflicts). The user-facing *type* is `parquet_string`, so the *module* had to differ —
  do not "fix" the plural back to `parquet_string`.
- **It is wired into the library.** `parquet_string_column` is a specific of
  `parquet_write_column`/`parquet_write_column_chunk`/`parquet_read_column`/
  `parquet_read_column_chunk`, going through new C++ entry points that pass offsets+data+validity
  directly (`parquet_append_string_column_buffers` write-side, the buffer-fill counterpart
  read-side — see `parquet_bindings.f90`) rather than the legacy fixed-width, space-padded block
  (`parquet_append_string_column`/`parquet_read_string_column`) every other string path still
  uses. This write path does **not** trim (the column stores bytes verbatim by default; the
  padded path trims because padding is indistinguishable from real trailing spaces). Scope is
  scalar 1-D string columns only — there is no vector/matrix `parquet_string_column` specific;
  vector/matrix string columns stay on the legacy padded path. The two interop hooks
  `raw_buffers` (export c_loc pointers for a writer) and `append_buffers` (bulk-append one row
  group from C buffers, int32/int64 offsets + validity merge) are what this integration is built
  on.
- **`allow_null=.true.` on `get`/`to_string` returns an empty string, not unallocated** — see the
  gfortran note in "Compiler & language gotchas" below.
- **A struct-nested leaf read through the compact buffer-handoff path needs its validity bitmap's
  element offset threaded through explicitly — the offsets/data buffers do not need this.**
  `extract_string_buffers` (`parquet_wrapper.cpp`) reports the source Arrow array's own
  `data()->offset` as an extra out-argument (`validity_offset` in `parquet_bindings.f90`'s
  `parquet_read_string_column_buffers`/`_chunk_buffers`), and `append_buffers` (this module) takes
  a matching optional `validity_offset_bits` argument to start its bit-walk at the right bit —
  because Arrow never pre-rebases a validity bitmap for a sliced array the way it does the
  offsets/data buffers (`raw_value_offsets()[0]` already accounts for slicing on those two).
  Precondition for any future non-scalar `parquet_string_column` specific (a vector/matrix column,
  or any new call site reached through `unwrap_struct_path`'s struct-path resolution): don't drop
  this argument or assume a fresh `offsets(1)==0` guard alone is sufficient — a sliced source whose
  removed leading elements are all empty strings passes that guard while still needing the
  validity offset to avoid misaligned nulls.
- **`get_single_chunk_array`'s whole-column struct-path result is not retained by `column_cache`
  the way a plain column's result is — anything that returns a raw pointer into it across the
  `bind(C)` boundary must pin it itself.** `column_cache` only ever stores the *pre*-
  `unwrap_struct_path` array; a dotted struct-field path additionally builds a fresh, uncached
  array (a new combined-validity buffer) on every call. `parquet_read_string_column_buffers` learned
  this the hard way (a confirmed, 100%-reproducible use-after-free: every row of a struct-nested
  string leaf read via the compact `parquet_string_column` path came back `Null`, because the
  freshly built validity buffer was freed the instant that function returned to Fortran, before
  Fortran's `append_buffers` read the pointer) — fixed by pinning the returned array in
  `ParquetReaderHandle::last_whole_column_buffers_array`, mirroring `last_chunk_buffers_array`'s
  existing pattern for the row-group-scoped chunk read. Any future whole-column function that hands
  a raw buffer pointer back across the `bind(C)` boundary for a column reachable via a struct path
  must pin its array the same way — don't assume `column_cache` alone covers it.

### The `parquet_temporal` module (date/time/timestamp)

`src/parquet_temporal.f90` provides `parquet_date`/`parquet_time`/`parquet_timestamp` — one
element each (unlike `parquet_string_column` above, which owns a whole column) — fully wired
into `parquet_read_column`/`parquet_write_column` and every chunked/row-mode/element-mode
counterpart. User guide: `doc/pages/date-time.md`.

- **Domain-grouped module naming, not one-module-per-type — `parquet_temporal` is the precedent
  for future sibling modules.** The name groups `parquet_date`/`parquet_time`/`parquet_timestamp`
  under their shared *domain* rather than any single type, leaving an obviously-parallel name for a
  future `parquet_map`/`parquet_list` module (Parquet `MAP`/variable-length `LIST` support — see
  CONTRIBUTING.md's "Features considered but not implemented") to grow into, so this one module
  never accumulates every future element type. Follow the same pattern: one module per *domain* of
  related types, named after the domain (`temporal`, `map`, `list`), not after any single type
  inside it.
- **These three types carry their own null state — no `is_valid=`/`null_value=` argument
  anywhere on their read/write path, unlike every other supported type.** A default-initialized
  element is null; write gathers validity from the elements themselves; a null-containing column
  reads without the error-on-Null the numeric/string readers apply by default. This is a
  deliberate, documented deviation (see `doc/pages/date-time.md`'s "Null values are part of the
  element" section and `supported-data-types.md`'s callout in its own "Null values" section) —
  not an oversight to bring in line with the rest of the library. A future `parquet_map`/
  `parquet_list` module should make its own considered choice here rather than assuming either
  convention by default.
- **Parquet's physical format has no seconds-resolution `TIME`/`TIMESTAMP` encoding at all**
  (only milliseconds/microseconds/nanoseconds) **and no `DATE64` physical representation**
  (`DATE` requires an `int32` day count) — confirmed empirically, not just from the spec: even
  with `ArrowWriterProperties::store_schema()`, Arrow's writer silently coerces a `SECOND`-unit
  `TIME`/`TIMESTAMP` array to `MILLI` on write, and a `date64()` array is always coerced to
  `date32()`. A MAML `time[s]`/`timestamp[s]` token is therefore rejected at `add_field`/parse
  time (`error stop`, not a silently-wrong stored unit) — see `apply_temporal_unit_token` in
  `parquet_metadata.f90`. `parquet_unit_seconds` still exists as a constant, but only for
  `set_unix`/`to_unix` (Unix-time interop), never as a file column's own declared/stored unit.
  The `DATE64` decode branch in `parquet_wrapper.cpp`'s `convert_date_values` is kept as
  defensive dead code (in case a future Arrow/Parquet version changes this) and `GCOVR_EXCL`'d
  rather than chased with an unbuildable fixture.
- **`civil_from_days`'s final `if (m <= 2) y = y + 1` line is easy to drop when hand-transcribing
  or re-deriving this algorithm** (e.g. to compute an expected civil date for a boundary-value test
  by hand/in a scratch script) — Howard Hinnant's algorithm computes a year-of-era relative to a
  March-based year, and this trailing correction is what shifts a January/February result back
  onto the actual calendar year; omitting it silently produces a year that is off by exactly one
  for any date whose month is January or February (confirmed by re-deriving the function from
  `parquet_temporal.f90`'s source without this line and getting a consistent one-year error only
  on Jan/Feb dates, e.g. `civil_from_days(0)` coming out as `1969-01-01` instead of `1970-01-01`).
  If you ever need to independently re-verify a `days_from_civil`/`civil_from_days` boundary value
  outside the Fortran source (by hand or in another language), re-read both functions' full bodies
  in `parquet_temporal.f90` first rather than reconstructing them from memory, and sanity-check the
  result via a *round-trip* (`days_from_civil(civil_from_days(z)) == z`) rather than trusting a
  single-direction computation.
- **Legacy `INT96` timestamp test fixtures**: `enable_deprecated_int96_timestamps()` is a method
  on `parquet::ArrowWriterProperties::Builder`, *not* `parquet::WriterProperties::Builder` (easy
  to guess wrong — the name doesn't indicate which builder). See
  `parquet_debug_write_datetime_fixture` in `parquet_wrapper.cpp` for the working pattern if a
  future debug fixture needs another legacy/foreign Arrow encoding.

## Build & compiler notes

### Compiler & language gotchas

- **132-column line limit is enforced — do not reintroduce `-ffree-line-length-none`.**
  `src/*.f90` and `test/*.f90` are held strictly within the standard 132-column free-form limit,
  including comments (both whole-line and trailing end-of-line) — a comment pushing a line past
  132 columns is a violation just like code would be. `.gitlab-ci.yml`'s `FPM_FFLAGS` does not
  pass `-ffree-line-length-none`, so a line over 132 columns fails CI on older gfortran (and is a
  style violation regardless of compiler). When a line runs long, wrap it with `&` continuations
  (code/strings) or split it across multiple `!`-prefixed comment lines — don't reach for the
  compiler flag again, and don't add per-file/per-line suppressions.
- **Minimum gfortran is 13; don't work around compiler bugs in source.** gfortran ≤ 11
  miscompiles the optional allocatable-`character` argument in `schema%add_col_qc` /
  `schema%set_col_qc` (corrupted column name → a spurious "column not found" abort at
  runtime — see README.md's Prerequisites). That's the reason for the version floor; don't
  refactor otherwise-correct source to accommodate an old compiler.
- **Never give a FINALIZABLE derived type to OpenMP's `private()` — declare it in a `block`
  inside the loop body instead.** gfortran does not reliably default-initialize a `private` copy
  of such a type, so a pointer component starts as garbage and the *first* thing that finalizes
  it — including the implicit finalization of an `intent(out)` dummy on entry to an "open"
  procedure — frees an undefined pointer and the process dies inside the allocator, with a
  backtrace pointing at malloc rather than at any of this library's own code. Confirmed with
  `parquet_table` (`private(t)` + `parquet_open_table`), and **reproducible with
  `OMP_NUM_THREADS=1`, which is what rules out a data race** and identifies it as initialization.
  The working form declares the variable where it is used, so ordinary block-scope
  initialization and finalization apply:

  ```fortran
  !$omp parallel do default(shared) private(rg) reduction(+:total)
  do rg = 1, n
      block
          type(parquet_table) :: mine     ! NOT private(mine)
          ...
      end block
  end do
  ```

  This applies to every finalizable type this library exposes (`parquet_table`, `parquet_reader`,
  `parquet_writer`, `parquet_table_row`), and to any future one — so a per-thread instance of any
  of them belongs in a `block`, and any example or guide page showing `private(<that type>)` is
  wrong and should be corrected on sight.
- **ifx forbids the `block` form the previous bullet prescribes, for any type that has
  ALLOCATABLE COMPONENTS — and is perfectly happy with `private()`. The two compilers forbid
  opposite shapes, so a type in that class can use neither, and needs a shared per-thread array
  instead.** ifx 2026.1 emits privatization scaffolding (`<TYPE>.omp.mold_ctor` →
  `for_alloc_private` → `do_alloc_copy` → `copy_src_xdesc_to_dest_xdesc`) for such a type declared
  in a `block` lexically nested in a parallel region, and it segfaults on every thread entering the
  region — 100% reproducible with 2 threads, independent of team size, so not a race, with a
  backtrace naming no library code. The working shape is the one in `materialize_marked_parallel`
  (`src/parquet_tables_read.f90`): allocate an array of the type **before** the region, one slot per
  thread, and index it by `omp_get_thread_num() + 1`, so no instance is constructed inside the
  construct at all. Passing an element on to an `optional, intent(inout)` dummy is fine.

  **Two conditions narrow this, and both will exonerate a broken shape if you reproduce
  carelessly.** It needs `-O1`+ (at `-O0` it runs clean, so `fpm build --profile debug` cannot see
  it — the same "only at `-O1`+" signature as the automatic-length-character ICE further down this
  section). And it needs the type to come from a **separately compiled module**: the identical type
  defined in the same file as its user does not crash, so a single-file reproducer will say the
  shape is fine when it is not.

  **The allocatable components are the whole trigger — FINALIZABILITY IS NOT REQUIRED, and reading
  it as though it were is what let the shape back in.** An earlier wording of this bullet said
  "any finalizable type that has allocatable components"; `materialize_column_parallel` then
  declared a block-local `type(parquet_column) :: chunk`, and `parquet_column` has no `FINAL` at
  all — it crashed exactly as described, on every full `fpm test` under ifx. So the class is: **any
  derived type with an allocatable component.** `parquet_reader`, `parquet_writer`,
  `parquet_schema`, `parquet_column` and `parquet_string_column` are all in it today.

  **A type with no allocatable components is exempt, which is why `parquet_table` is still safe
  block-local** — it is five scalars and a pointer by deliberate design (see "New
  `parquet_table` state goes on the CACHE"), and that design is now load-bearing for ifx too: the
  first allocatable component added to it would make every block-local per-thread table in user
  code start crashing. That is very likely what the `parquet_schema`-component segfault recorded
  in that section actually was. See `feature_risks.md` Risk-45.

  **Diagnosing it takes one command**, and it is worth running before concluding a compiler is at
  fault at all: `nm <object> | grep -E "for_alloc_private|mold_ctor"`. If the scaffolding is absent,
  the source under test *cannot* produce that backtrace, and the binary that crashed is stale —
  see "Stale `fpm` build cache" below. A per-compiler `#ifdef` bail-out was once added to
  `parallel_prefetch_ok` on the strength of a stale-binary result, disabling the internally-parallel
  prefetch under ifx for a crash the committed code had already fixed.
- **Every `submodule (parquet) name` file needs its own `implicit none`** (right after the
  `submodule` line, before `contains`) — a submodule's `implicit none` is *not* inherited from
  the ancestor module; confirmed with a minimal repro where gfortran silently accepted an
  undeclared variable in a submodule lacking it, even under `-Wall`. Contained procedures
  *within* a module/submodule do inherit their host's `implicit none` via ordinary host
  association, so it does not need repeating inside each individual function/subroutine — one
  `implicit none` per submodule file is sufficient.
- **Test for NaN with `ieee_is_nan`, not `x /= x`.** Use `use ieee_arithmetic, only: ieee_is_nan`
  and `ieee_is_nan(x)` rather than the classic self-comparison idiom — the latter is correct
  (NaN is the only value never equal to itself) but triggers gfortran's `-Wcompare-reals`
  warning. See `parquet_metadata_maml.f90`'s `parquet_qc_numeric_bound` for the pattern.
  This does not apply to the *other* `-Wcompare-reals` sites in this codebase (e.g.
  `value == anint(value)` in `parquet_write_numeric.f90`/`parquet_metadata_maml.f90`, testing
  whether a float is exactly integral) — those are exact-equality checks with no arithmetic
  drift and no equivalent NaN-style idiom, so their warning is left as an accepted false
  positive rather than "fixed" into something worse (e.g. an epsilon comparison).
- **A function returning an unallocated `allocatable` cannot yield an unallocated LHS via
  `x = func()`.** Verified on gfortran 15.2: intrinsic assignment from an unallocated allocatable
  function result leaves the LHS *allocated* (an empty string/array), even for a fresh target.
  So an API cannot signal "absent" purely by returning an unallocated result — provide an explicit
  flag/sentinel instead (this is why `parquet_strings`' `allow_null` returns `""`, guarded by
  `is_null()`).
- **Never blank a deferred-length allocatable character ARRAY with `arr = ""` — assign element by
  element.** Intrinsic assignment to an allocatable reallocates it whenever the RHS's length
  differs, and that rule applies to a whole-array assignment from a scalar too: `arr = ""` keeps
  the shape but reallocates every element to **length zero**. A later `arr(i) = name` then writes
  the declared width into a zero-length allocation, so the names come back blank *and* the heap is
  corrupted. **gfortran keeps the length and hides both symptoms; ifx follows the standard and
  shows them** — as blank strings in an error message ("column '' … for internal name ''"),
  followed some tests later by `free(): invalid next size (fast)` in unrelated code, which reads
  like a completely different bug. `parse_read_maml_remap` (`src/parquet_tables_maml.f90`) is the
  worked example; it blanks with an explicit `do i = 1, count` loop, because an array *element* is
  not itself an allocatable variable and so only blank-pads. The same hazard does **not** apply to
  a deferred-length allocatable *scalar* (`suffix = ""` is the intended idiom and is everywhere in
  `parquet_metadata.f90`), nor to an array assignment whose RHS carries the right length already
  (`values_c = pack(values, mask)` in `parquet_write_string.f90`).
- **A list-directed `read(text, *, iostat=ios) n` is NOT a strict parse, and silently accepts a
  wrong value.** Verified on gfortran 15.2: it rejects `"5abc"` and `"3.9"` (`iostat` 5010) and `""`
  (`iostat` -1) as you would hope — but it accepts **`"5 6"` with `iostat == 0`, yielding 5**. So
  parsing any caller-supplied text this way (an environment variable, a config line, a command-line
  argument) turns a typo or a shell variable that expanded to two words into a plausible wrong value
  applied silently, which is far worse than a clean failure. Parse strictly by hand instead: trim,
  allow one optional `+`/`-`, require at least one digit and **nothing else** to the end of the
  string, and only then let `read` do the conversion. `env_int64` (`src/parquet_settings.f90`) is
  the worked example, and `settings_env_two_numbers` (`test/error_scenarios.f90`) is the regression
  test that stops the lax form coming back.
- **`-128_int8` trips gfortran's range check** (it parses `128` then negates). Build the high bit
  with `ibset(0_int8, 7)` in constant expressions. Also: an array-constructor implied-do index
  (`[(f(b), b=0,7)]`) has no implicit type under `implicit none` — list the elements explicitly.
- **`transfer(source, mold, size)` into a longer target leaves the trailing bytes undefined**, not
  blank-padded. To place a short string into a longer fixed-length slot, assign normally (which
  blank-pads); reserve `transfer` for exact-size byte moves.
- **Passing an UNALLOCATED allocatable to an `optional` dummy makes that dummy ABSENT** (F2018
  15.5.2.12; verified on gfortran before relying on it). This is load-bearing, not a curiosity:
  `parquet_column%row_validity` and every `mat_*`/`matchunk_*` return or hold an unallocated mask for
  a null-free column and pass it straight on as `is_valid=`, so the callee sees no argument at all and
  takes its own no-mask fast path. It means a procedure can decline to supply an optional argument
  *at runtime*, without the caller writing an `if (present(...))` fork or duplicating the call — but
  it also means **a caller cannot tell "absent" from "the producer had nothing to say"**, so any
  procedure returning such an array must document that `allocated()` is part of its contract.
- **Never write a function that returns `character(len=:), allocatable` — use a subroutine with
  an `intent(out)`/`intent(inout)` allocatable `character` argument instead.** This is a fixed
  project-wide convention, not just advice: gfortran has a confirmed, still-open compiler bug
  (GCC [PR113797](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113797); related:
  [PR97977](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=97977)) where the codegen for *receiving*
  such a function's result uses a hidden length-tracking variable that isn't always properly
  thread-local, silently corrupting memory when the function is called concurrently. Plain
  non-`character` allocatable function results (numeric scalars/arrays, allocatable arrays of a
  derived type) are not affected and need no action.

  Already applied throughout `src/*.f90` — apply the same conversion to any new occurrence.

  **This is not a hypothetical risk — it has actually caused silent, hard-to-trace memory
  corruption in this project.** `top_level_of` (`parquet_tables_read.f90`, added alongside the
  table-mutation work) was written as exactly this forbidden shape and slipped past review.
  Confirmed via ThreadSanitizer: two OpenMP threads calling it concurrently (from
  `table_release_one`/`materialize_marked`, both reachable from ordinary `%prefetch`/
  `%materialize_all` use) raced on gfortran's hidden length-tracking temporary (TSan named it
  `slen.49.1`), corrupting memory that then surfaced later as an unrelated-looking `ERROR STOP`
  immediately followed by a SIGSEGV, in a completely different part of the table code, several
  investigation rounds later (a ThreadSanitizer-caught Arrow-singleton race — see the next
  section — was found and fixed FIRST, from a plausible-looking but ultimately secondary TSan
  report, before this one, the actual dominant cause, was found in a follow-up TSan run). Fixed
  by converting it to a subroutine per this rule; both call sites updated to
  `call top_level_of(name, top)`. **Lesson for any future concurrency investigation in this
  codebase: fixing one TSan-caught race does not mean the reported failure is resolved — rerun
  the sanitizer after every fix, since more than one independent race can be masking behind the
  same flaky symptom, and a small, easy-to-miss helper like this one can be the dominant cause
  even when a more "interesting"-looking external-library race is also genuinely present.**

  **Design note for the "assign result back into the input variable" pattern** (e.g.
  `col = schema%get_col_qc(col)`): a subroutine can't alias the same actual argument to separate
  `intent(in)`/`intent(out)` dummies, so make the single argument `intent(inout)` instead — it
  holds the input on entry and the result on exit, preserving `call schema%set_col_qc(col)`'s
  single-variable ergonomics (see `schema_set_col_qc`/`parquet_maml_file%set_col_qc`).
- **`intent(out)` on a finalizable type resets every component for free — don't convert one to
  `intent(inout)` without auditing every component first.** `parquet_writer`/`parquet_reader` both
  have a `FINAL` procedure, and Fortran resets *every* component of an `intent(out)` dummy to its
  default initializer (deallocating every allocatable, zeroing every scalar) on procedure entry.
  `parquet_open_writer`/`parquet_open_reader`'s own bodies only explicitly (re)initialize a
  handful of fields (e.g. `handle`, `filename`) and lean on this implicit reset for the rest
  (mask state, row-group tracking flags, `qc`, cached metadata, ...). If either is ever converted
  to `intent(inout)` (e.g. to add a "not already open" guard), that implicit reset goes away, and
  a caller reusing the same variable across two open calls would silently carry over stale state
  from the previous use unless the open body is rewritten to unconditionally reset every single
  component itself — a much larger, easier-to-get-subtly-wrong change than it first appears.
  Prefer keeping `intent(out)` and solving misuse-prevention some other way.
- **The previous bullet's "resets every component for free" is the documented standard behavior,
  but this project has one confirmed, empirically-reproduced counterexample — don't treat it as an
  absolute guarantee for a correctness-critical `logical` component.** `parquet_table` (finalizable,
  `FINAL :: table_finalize`) has an `intent(out)`-reopened `open_table_impl`/`parquet_new_table`
  where one component, `detached`, was found (via direct thread-tagged instrumentation, gfortran
  13/14, reproduced identically in both the real GitLab CI image and a from-scratch local
  Docker rebuild of it) to sometimes still read back `.true.` immediately after a fresh
  `intent(out)` reopen of a variable that had previously been detached (e.g. by a prior
  `%sort_by` call) — with no concurrency involved and every other component (`regime`/`row_lo`/
  `row_hi`/`row_count`/`cache`) behaving correctly. `detached` was the one component in both
  procedures that relied *solely* on the implicit default-initializer reset, unlike every sibling
  component, which is explicitly reassigned in the body regardless. Fixed by adding an explicit
  `table%detached = .false.` as the first executable statement of both `open_table_impl` and
  `parquet_new_table` (`parquet_tables_lifecycle.f90`) — do not remove it on the assumption that
  `intent(out)`'s implicit reset alone is sufficient, and apply the same explicit-reset treatment
  to any new scalar `logical`/default-initialized component added to a finalizable type's
  `intent(out)`-entry procedure, rather than trusting the implicit reset for it.
- **A component and its parent cannot both be actual arguments of one call.** Passing `t%cache` and
  `t%cache%reader` to the same procedure argument-associates two dummies with overlapping storage,
  which Fortran forbids as soon as either is defined (F2018 15.5.2.13) and which compilers optimise
  against. Neither gfortran nor ifx diagnoses it. This shapes API design rather than being a bug to
  fix afterwards: `table_open_reader_with_transform(cache, filename, [rdr])` takes the reader as an
  **optional** argument precisely because of it — absent means "open `cache%reader`", reached through
  the cache, and `rdr` names any OTHER reader. Where that leaves two near-identical code paths, a
  local `type(...), pointer` resolving to one or the other is the way out, with `target` on both
  dummies; a pointer to a component of a plain (non-`target`) dummy is not permitted, which is why
  `table_materialize` writes its own two arms out instead.
- **`.and.` does not short-circuit, and `-fcheck=bounds` is what tells you.** Fortran may evaluate
  both operands, so `size(a) == size(b) .and. all(a == b)` compares two differently-sized arrays
  whenever the sizes differ — an out-of-bounds read that a plain `fpm test` runs straight past and
  `fpm test --profile debug` aborts on. Nest the tests instead (`same = .false.; if (size(a) ==
  size(b)) same = all(a == b)`). This has bitten twice: once in `parquet_close_writer`'s
  mask-consumed check, once in a test comparing two sample draws. Both times the guarded form looked
  obviously safe.
- **A `pointer`-typed intermediate component defeats `-fcheck=bounds`'s trust in a freshly
  unallocated LHS on intrinsic assignment.** `table_clone` (`parquet_tables_clone.f90`) used to do
  `out%cache%rg_bounds = self%cache%rg_bounds` to copy an allocatable 2-D array, relying on F2003+
  automatic reallocation (assigning to an allocatable should reallocate it to match the RHS shape).
  Under `-fcheck=bounds` (fpm's own default debug profile — NOT enabled by `FPM_FFLAGS="--coverage"`
  in `.gitlab-ci.yml`, which is why this only ever surfaced via a Docker/local `fpm test` run using
  fpm's plain default profile, never in the real CI job itself), this raised a spurious "Array bound
  mismatch for dimension 1 of array 'out' (0/2)" even though `out%cache%rg_bounds` was genuinely,
  freshly unallocated — `cache` being reached through a `pointer` component rather than a plain
  allocatable one is what confuses the bounds check here. Fixed by replacing the assignment with an
  explicit `allocate(out%cache%rg_bounds(size(self%cache%rg_bounds,1), size(self%cache%rg_bounds,2)))`
  followed by an element-wise `out%cache%rg_bounds(:,:) = self%cache%rg_bounds(:,:)`. If a future
  `%clone`-style deep copy adds another allocatable array reached through a `pointer` intermediate,
  prefer this explicit allocate-then-copy shape over a bare intrinsic assignment from the start,
  rather than rediscovering the same spurious bounds-check failure.

  **The same shape carrying a deferred-length `character` array is worse than a bounds-check
  complaint — it has been seen to segfault outright, and only on CI's compiler.** `clone_new_cache`
  (same file) used to copy the file-metadata snapshot with
  `out%cache%meta_keys = self%cache%meta_keys`, where automatic reallocation has to establish the
  deferred LENGTH as well as the shape, through the same `pointer` intermediate. That crashed
  inside libc's allocator on GitLab CI's (older, Ubuntu-packaged) gfortran — backtrace naming
  `clone_new_cache` with two `???` libc frames under it and nothing else — while running clean on
  gfortran 15.2 locally, under `-fcheck=all`, and under `--coverage -fopenmp`. **A clean local run
  proves nothing about this class of bug**; the fix is the same explicit
  `allocate(character(len=len(src)) :: dst(size(src)))` plus an element-wise loop (element-wise
  because a whole-array `dst = src` is itself the reallocation hazard described further up this
  section). Reach for that shape from the start for any deferred-length `character` array component
  behind a pointer, and do not restore the plain assignment on the strength of a green local run.
- **cpp runs over every source file, so `/*` anywhere — including inside a Fortran comment —
  breaks the build.** `fpm.toml` declares `[preprocess.cpp]`, which applies to *all* sources, not
  just `.F90` ones. Writing a glob like `tools/*.sh` in a comment opens a C block comment and the
  file fails to compile with a confusing `unterminated comment` pointing at the *last* line of the
  comment block, not the offending one. A trailing `\` at the end of a comment or string literal is
  a cpp line-continuation for the same reason, and silently splices the next source line. Neither is
  a Fortran error, so nothing about the message suggests the real cause. Write `tools/ *.sh`, "a
  shell wrapper under `tools/`", or anything else that avoids the two-character sequence.
- **A type-bound procedure cannot share a name with a data component of the same type**
  (`Error: Procedure 'x' at (1) has the same name as a component of 'my_type'`). When a natural
  accessor name collides with the component it reports (`%nrows()` over an `nrows` component), rename
  the **component** — it is private implementation detail — and keep the short binding name, which is
  the public surface. Renaming the binding instead pushes an internal detail into the API.
- **A private procedure contained directly in a module, whose only callers are that module's
  submodules, compiles cleanly and then fails at LINK time** with `undefined symbol`. gfortran does
  not emit it (it also reports `-Wunused-function` for it, which is the early warning). This is
  invisible to per-file compilation and only appears when something actually links, so it can survive
  a long way into a change. Fix: declare the procedure's interface in the module and implement it in a
  submodule, exactly as `parquet_core.f90` already does for its shared private helpers (see
  `src/parquet_columns_util.f90` for a file created solely to hold such helpers). Prefer that shape
  from the start for any helper a submodule will call.
- **The "no `character`-returning function" rule above extends to an *automatic*-length result,
  not just a deferred-length one, when the length is a specification expression over
  host-associated variables — that shape is an ifx internal compiler error.** ifx 2026.1.1
  segfaults (ICE, and the source line it names is meaningless) on:

  ```fortran
  submodule (m) sm
  contains
      module procedure p
          integer, allocatable :: lo(:), hi(:)          ! host-associated allocatables
          ...
      contains
          subroutine sibling(...)
              ... // tok(k) // ...                      ! call from a SIBLING contained procedure
          end subroutine sibling
          pure function tok(k) result(res)
              character(len=hi(k)-lo(k)+1) :: res       ! length reads those allocatables
              res = text(lo(k):hi(k))
          end function tok
      end procedure p
  end submodule sm
  ```

  Every one of these conditions is required, confirmed by bisecting each away independently:
  `-O1` or higher (at `-O0` it compiles clean, which is why `fpm build --profile debug` succeeds
  while a plain `fpm build` fails — an easy signal to misread as a flaky build); the call coming
  from a **sibling** contained procedure rather than from the module procedure's own statements;
  and the whole construct sitting inside a `submodule`'s `module procedure` body rather than a
  plain `program`/`contains`. Recursion is **not** required. gfortran compiles and runs the same
  code correctly at `-O2`, so this will not show up in CI. Fix it the same way the bullet above
  forces for its own (unrelated, gfortran) reason — a subroutine with an `intent(out)` allocatable
  `character` argument — or, where the body is a one-liner, drop the helper and write the
  expression at the call sites. `tok_text` (`src/parquet_read_filter.f90`) is the worked example.
- **Never interpolate unbounded caller-supplied text into an `error stop` message — cap it to a
  short preview.** ifx 2026.1.1's `ERROR STOP` runtime corrupts the heap once the composed message
  reaches **8192 bytes** (confirmed with a minimal standalone repro: 8191 bytes aborts cleanly,
  8192 crashes every time). This bites hardest exactly where it is least expected: a "value too
  long" guard that reports the offending value verbatim is *guaranteed* to build a huge message on
  the one input that triggers it, turning a clean abort into a crash and an error-scenario test
  into a confusing stderr-mismatch failure. `parquet_filter_add` (`src/parquet_core.f90`) is the
  pattern to copy — at most the first 100 characters of the rule, plus `"..."` when truncated.
  Apply the same cap to any new message embedding a rule, a MAML line, a filename, or any other
  value whose length the caller controls; it is better behaviour regardless of compiler, since a
  multi-kilobyte error message is unreadable anyway.
- **ifx rejects a default structure constructor (`type_name()`) when the type has a component
  whose OWN type has private components declared in a different module — even when that
  component isn't touched by the constructor and was already cleared beforehand.** gfortran
  accepts this without complaint; ifx (confirmed on the Intel compiler active in the qmost
  environment) rejects it with `error #6053: Structure constructor may not have components with
  the PRIVATE attribute`, naming the *outer* type even though the private components belong to
  the nested one. `parquet_table_column` (`parquet_tables.f90`) has a `values` component of type
  `parquet_column`, whose own components are private to `parquet_columns.f90` — so
  `parquet_table_column()` used to reset a slot's metadata fields (after `%clear()`-ing `values`
  itself on the preceding line) fails under ifx despite `values` never being named. Fix: replace
  the default structure constructor with an explicit field-by-field reset of the type's own
  metadata components (matching their declared defaults), leaving the private-dependent component
  untouched (already handled separately, e.g. by `%clear()`). `table_drop_column`
  (`parquet_tables_mutate.f90`) is the worked example. Any future default structure constructor on
  a type that embeds a component from another module's private-component type needs the same
  treatment.

- **A TEMPLATE cannot go in `src/parquet_wrapper.cpp` without its own `extern "C++"` block.** The
  whole file sits inside one enormous `extern "C" { … }`, and a template declared there fails with
  `error: templates must have C++ linkage` — a message that points at the template rather than at
  the linkage specification a thousand lines above it. Linkage specifications nest, so the fix is to
  wrap just that declaration:

  ```cpp
  extern "C++" {
  template <typename F>
  static int64_t sort_spawn(std::vector<std::thread> &workers, int64_t lo, int64_t hi, F f) { … }
  }
  ```

  Taking a `std::function` instead works equally well when the call happens once per chunk rather
  than once per element; prefer the template plus `extern "C++"` when the callable is on a hot path.
  `sort_spawn` is the worked example.
- **`src/parquet_wrapper.cpp` is NOT compiled with `-fopenmp`, so it cannot call any `omp_*`
  function at all.** `.gitlab-ci.yml` sets `FPM_CXXFLAGS: "-std=c++20 --coverage"` and a dev machine
  sets whatever Arrow needs — neither adds it, and `fpm.toml`'s `openmp = "*"` metapackage covers the
  Fortran half. **So anything on the C++ side that needs an OpenMP answer must have it resolved in
  Fortran and passed across the `bind(C)` boundary as an ordinary value.** M4's threaded sort works
  exactly that way: `pf_sort_threads` asks `omp_get_max_threads()`/`omp_in_parallel()` in Fortran and
  hands C++ a plain integer count, so `parquet_wrapper.cpp` receives a number and never a policy.
  Do not add an "auto" sentinel to a `bind(C)` signature for the C++ side to interpret — it cannot.

### Arrow's own type singletons have thread-unsafe lazy state on first concurrent use

Every no-argument `arrow::<type>()` factory (`arrow::int32()`, `arrow::utf8()`, `arrow::boolean()`,
...) returns a reference to a **process-wide, function-local `static` singleton** shared by every
thread. That alone is fine — the problem is that this project's apt-installed Arrow build
(`.gitlab-ci.yml`'s Arrow apt repository) has **confirmed, ThreadSanitizer-caught data races on
more than one kind of lazily-populated mutable state hanging off that same shared object**, each
found independently and each requiring its own fix:

1. **The singleton's own construction** (its `shared_ptr` control block). `arrow::int32()` et al.
   use the standard C++11 "magic statics" pattern, normally safe to construct concurrently for the
   first time since the compiler inserts a one-time-init guard — but TSan caught two OpenMP
   threads racing on `arrow::int32()`'s construction the first time each independently wrote an
   `int32` column at process/test-suite startup: a genuine race on the `shared_ptr`'s refcount,
   not a false positive.
2. **`arrow::detail::Fingerprintable`'s lazily-cached `fingerprint()`/`metadata_fingerprint()`**,
   which every `DataType` inherits (and which Arrow's own type/field/schema equality checks use
   internally as a fast path, so it is reachable from far more call paths than an explicit
   `->fingerprint()` call would suggest — the confirmed instance here was triggered from inside
   `parquet_close_writer`). Found in a SECOND, separate TSan run, after fixing (1) above did not
   make the underlying flakiness go away: two threads racing to populate this cache the first time
   they both touch the same shared singleton type, same underlying pattern as (1) but a
   completely separate piece of state.

Root cause not chased further than "the apt Arrow build behaves this way for both of these"; do
not assume a from-source Arrow build is affected the same way without re-checking.

**Why this was so hard to trace back to its actual cause**: corrupting a process-wide singleton's
state doesn't crash where it happens — it surfaces later, in whatever unrelated code next touches
the heap. This is exactly what made the original investigation (chasing a SIGSEGV inside
`table_check_not_detached`, a completely unrelated and trivially-simple boolean check) so
misleading, and why fixing race (1) alone looked sufficient locally but did not actually clear the
CI failure — race (2) was still there, waiting to corrupt something else. **If a future
concurrency bug report shows a clean-looking `error stop`/check failure immediately followed by a
crash in unrelated code, or a crash whose faulting line changes between runs (or between fixes),
suspect heap corruption from an early race over shared, lazily-initialized Arrow state before
assuming the crash site itself is where the bug lives — and don't assume fixing one such race
means there isn't a second, independent one still lurking. Re-run the sanitizer after each fix,
not just after the first.**

Fixed in `parquet_wrapper.cpp` (`ensure_arrow_type_singletons_initialized`, `std::call_once`-
guarded, mirroring `ensure_compute_initialized`'s existing pattern for Arrow's compute-kernel
registry): every no-argument `arrow::<type>()` factory this file uses is forced into existence,
AND has `->fingerprint()`/`->metadata_fingerprint()` called on it, exactly once, from a single
thread, at the top of both `create_parquet_reader` and `create_parquet_writer` — the two entry
points any OpenMP thread can reach first. After that one call, every later concurrent read is just
a read of already-published state, which is safe. **A parameterized factory
(`arrow::timestamp(unit)`, `arrow::decimal128(p, s)`, ...) is NOT affected** — those construct a
fresh, non-shared object per call rather than caching a singleton, so they have nothing to warm
up. **Keep the type list in sync with `parquet_wrapper.cpp`'s actual usage**: if a future change
introduces a new bare `arrow::<type>()` call site, add it to the list in
`ensure_arrow_type_singletons_initialized` too — grep the file for `arrow::` factory calls taking
no arguments to re-derive the exhaustive list if in doubt.

**This fix is deliberately NOT an exhaustive guarantee against every possible Arrow-internal lazy
cache** — enumerating Arrow's private implementation details one race at a time is not a fight
this project can definitively win. Two are now known and covered. If a THIRD, distinct race
against one of these same singleton objects ever surfaces, add whatever call reproduces it to the
same warm-up function rather than treating it as a one-off; if a third instance does show up,
reconsider a broader warm-up strategy (e.g. a full dummy write+close round-trip exercising every
supported type, single-threaded, in the same `std::call_once` block) instead of continuing to
enumerate individual private caches by name. See
[Thread safety](doc/pages/thread-safety.md#a-note-on-arrows-own-type-singleton-construction) for
the user-facing writeup.

### gcovr <7.1 cannot parse gcov output for a 10,000+ line file

The CI `test:` job's `gcovr` step crashes with `gcovr.formats.gcov.parser.UnknownLineType` on
`src/parquet_wrapper.cpp`'s coverage data — reported as `<n>:10000-block 0` (then `10001-block N`,
`10002-block N`, ...). **These are real, valid gcov block-annotation lines for real source lines —
not corruption.** `src/parquet_wrapper.cpp` has grown to just over 10,000 lines (`wc -l`; it was
~7,400 when several older notes elsewhere in this file were written, and will keep growing — treat
any specific line count anywhere in this file as a snapshot, not a promise), and lines 10000/10001
are genuinely `if (!status.ok())` / `throw std::runtime_error(...)`. First suspected as heap/counter
corruption (from concurrent
OpenMP threads racing on GCC's `--coverage` counters, or from stale `.gcda` left over from an
earlier crashed run, or from the Docker reproduction's QEMU (amd64-on-arm64) emulation) — all
three were tested and ruled out: the crash reproduces identically on a genuinely fresh build, in
the real (non-emulated) GitLab CI pipeline itself, and adding `-fprofile-update=atomic` to every
coverage build changed nothing.

**Confirmed root cause: this is [gcovr issue #882](https://github.com/gcovr/gcovr/issues/882)**
("UnknownLineType thrown when parsing coverage data from 10K+ line file") — for a source file at
or past 10,000 lines, gcov drops the space between the block's hit-count field (or a `%%%%%`/
`$$$$$` exception-only-block marker) and the line number in its `-block N` annotation lines, and
gcovr's parser regex requires that space, so it throws instead of matching. Fixed upstream in
[PR #883](https://github.com/gcovr/gcovr/pull/883) ("Add support for more than 9999 lines"),
merged 2024-02-11, first released in **gcovr 7.1** (this project's CI environment was hitting it
on gcovr **7.0**, apt-installed from Ubuntu 24.04's package archive, which predates the fix and
will never receive it via a point release). [Issue #1103](https://github.com/gcovr/gcovr/issues/1103)
("GCovr on Ubuntu 24.04 Cannot Parse Coverage Reports") is another project hitting this exact
combination and confirms upgrading gcovr is the resolution — there is no compiler flag, source
change, or coverage-tool-invocation workaround; the parser itself cannot read this file's gcov
output below 7.1, full stop.

**Fix: install a `gcovr` version >= 7.1 rather than relying on the OS-packaged one.** Given
Ubuntu's own apt archive does not reliably track this (24.04 ships a pre-fix 7.0 as of this
writing, and a future Ubuntu LTS could just as easily ship another pre-fix snapshot), pin a
known-good version via `pipx` (already used for `fpm` in the same `before_script`) rather than
`apt-get install gcovr`. If a future `gcovr` release regresses this again, re-check
[gcovr's own issue tracker](https://github.com/gcovr/gcovr/issues) for "UnknownLineType" before
assuming it's a new bug in this project. This bound will need revisiting again as
`src/parquet_wrapper.cpp` keeps growing — the same class of off-by-one could recur at the next
power-of-ten boundary (100,000 lines) if gcovr's fix has any similar edge case, though nothing
currently suggests it does.

### gcovr 8.4+ drops coverage for module-contained Fortran subroutines

CI's `gcovr` step ran clean (exit 0, "All error scenarios behaved as expected") but the coverage
table came back almost empty: most `src/*.f90` files reported `Lines=0 Exec=0 --%` — not 0%
covered, but **no coverage data associated with the file at all** — while `src/parquet_wrapper.cpp`
and a small handful of `.f90` files (whichever happened to have no procedures directly `contains`ed
inside a `module`/`submodule`) reported correctly. Since virtually every procedure in this
project's `src/*.f90` lives inside a module or submodule's `contains` block (see "Nested submodule
tree" above), this wiped out nearly the whole Fortran coverage signal while leaving the C++ side
and the `coverage:` regex mechanism itself looking unremarkable — easy to misdiagnose as a
`gcovr`-can't-read-Fortran-at-all problem rather than a narrow, version-specific regression.

**Confirmed root cause: [gcovr issue #1253](https://github.com/gcovr/gcovr/issues/1253)**
("Missing coverage for Fortran module subroutines since gcovr 8.4") — gcovr 8.4 through at least
8.6 silently drops coverage for any Fortran subroutine/function contained inside a module,
apparently while filtering out compiler-generated symbols (debug output shows a real
module-mangled symbol like `__module_help_MOD_help_convert` being discarded as if it were a
compiler-generated one). Confirmed absent in gcovr 8.3. This is a **second, independent** gcovr
regression from the 10,000-line parser bug in the section above — one needed a version floor
(`>=7.1`), this one needs a version ceiling, and both bounds are load-bearing at once.

**Fix: pin `gcovr` to a range that clears the 7.1 floor and stays under the 8.4 ceiling** —
`.gitlab-ci.yml`'s `before_script` installs `gcovr>=7.1,<8.4` via `pipx` rather than an unbounded
`gcovr>=7.1`. If a future `gcovr` release fixes #1253, re-test before widening the ceiling; if a
new regression appears in some future version, check
[gcovr's own issue tracker](https://github.com/gcovr/gcovr/issues) for "module subroutine" /
"0 lines" before assuming it's a problem in this project — the symptom (a clean CI run with an
almost-empty coverage table) looks exactly like this one.

### Verifying the bind(C) boundary

A `bind(C)` interface (`src/parquet_bindings.f90`) has no compile-time link to the `extern "C"`
definition it describes in `src/parquet_wrapper.cpp` — gfortran and gcc each compile their own
half against the interface/definition text alone. A kind mismatch introduced by hand-editing
either side (e.g. a dummy silently changed from `integer(c_int32_t)` to `integer(c_int64_t)`
without the matching C++ parameter changing too) compiles cleanly on both sides and corrupts
memory silently at runtime, with no compiler diagnostic at all. Run `tools/check_bindc_boundary.py`
after touching either side of this boundary (a signature in `parquet_bindings.f90`, an `extern "C"`
function in `parquet_wrapper.cpp`, or one of the hand-written local `bind(C)` debug-hook interfaces
in `test/error_scenarios.f90`/`test/test_temporal.f90`) — it cross-checks arity, base type,
by-value-vs-by-reference, and (for functions) return type, and is wired into CI (`.gitlab-ci.yml`'s
`lint` stage) so a mismatch fails the pipeline rather than only surfacing at runtime. It does
**not** check length/ownership contracts, array rank, or NUL-termination conventions — see its own
module docstring for what's out of scope, and "The `parquet_strings` module" above for a confirmed
instance of that different class of bug.

### If `src/parquet_wrapper.cpp` is ever split into multiple translation units

A maintainability review considered splitting this single (now 10,000+-line, still growing) file and
decided against it
(see CONTRIBUTING.md's "Features considered but not implemented" for the full four-cost writeup,
and `src/parquet_wrapper.cpp`'s own `// ====`-banner comments, added instead, for a cheaper
navigability improvement). If that decision is ever revisited, the single most important, least
obvious hazard is this: **every process-global `static` at file scope means exactly one instance
per translation unit**, and this file has two families of them.

The `g_debug_*` test-only overrides (`g_debug_force_whole_column_read_error`,
`g_debug_string_offset_limit`, `g_debug_col_size_limit`, `g_debug_list_element_count_limit`,
`g_debug_column_count_limit`, `g_debug_force_sample_mask_error`,
`g_debug_physical_column_read_count`) are the first. Splitting the file without changing this would
silently give each new `.cpp` its own separate copy of every one. It would still compile and
link cleanly — there is no diagnostic for this — but any `parquet_debug_set_*` setter reachable from
`test/error_scenarios.f90` would then be writing to a *different* object than the guard code reads,
so the override would silently stop working and the corresponding error scenario would start
testing nothing at all while still reporting green.

**The settings mirrored from `parquet_settings` are the second family, and they are worse**, because
they affect production behaviour rather than only tests: `g_verbosity`, `g_message_stream`,
`g_sort_parallel_min_rows`, `g_sort_counting_path`, `g_sort_counting_bucket_limit`,
`g_target_row_group_bytes` and `g_statistics_prescreen`. `parquet_push_output_settings` and
`parquet_push_performance_settings` would write to their own TU's copy, and every read site in
another TU would keep the built-in initialiser — so a user's `parquet_set_verbosity("silent")` or
`parquet_set_target_row_group_bytes(...)` would apply to some of the library and not the rest, with
the Fortran getters still reporting the value correctly (`feature_risks.md` Risk-42).

Before any split, promote every global in both families to a genuine `extern` global with exactly
one definition in a shared internal header (not `static`), re-run every affected error scenario to
confirm the override still takes effect, and re-run `test/test_settings.f90`'s observed-effect tests
to confirm each mirrored setting still reaches the code that reads it.

### Stale `fpm` build cache

If `fpm test` behaves unexpectedly after source changes (e.g. a test target seems to run old
code), try `fpm clean --skip` to force a clean rebuild before spending time debugging — fpm's
build cache can serve a stale binary. `--skip` avoids rebuilding external (non-project)
dependencies, which are never the source of this problem, so it's faster than `--all` here.
Building with several different `FPM_FFLAGS` creates multiple `build/gfortran_<hash>/` dirs, which
a `find build -name error_scenarios | head -1` lookup resolves by guessing — symptom: tests pass
when run scoped but fail under a full `fpm test`. `test/test_errors.f90` no longer guesses:
`get_error_scenarios_bin` derives the sibling binary from **argument 0**, which names the exact
tree fpm launched this run_tester from, and only falls back to build-and-`find` when argument 0
cannot answer (someone running the built binary directly). `tools/run_error_scenarios.sh` is
standalone and still has to `find`, so the hazard remains there. `tools/coverage.sh` runs
`fpm clean` up front to avoid it; for a plain `fpm test`, `fpm clean --skip` fixes it.

**A change to a GATE is the most misleading form of this**, because a stale binary makes the tests
agree with you. Opening `parallel_prefetch_ok`'s refusal clauses and re-running left the two tests
that assert the refusal still reporting **PASSED** — i.e. the evidence said the change had not taken
effect, which reads as "my edit was wrong" rather than "my binary is old". `fpm clean --skip` showed
both failing, as intended. Treat a gate that appears not to have changed as a cache symptom first.

**A restored `src/parquet_wrapper.cpp` is the case fpm most reliably misses, and it bites hardest
during mutation testing.** Reverting that file (`cp backup src/parquet_wrapper.cpp`, `git checkout`,
a stash pop) and re-running `fpm build` repeatedly left the *mutated* object still linked — the
suite kept failing with the mutation's own symptom after the source was demonstrably clean, which
reads exactly like "my revert did not work" and invites a hunt for a second bug that does not
exist. `fpm clean --skip` fixes it. **So: after reverting a C++ mutation, `fpm clean --skip` before
believing any result** — and treat a failure that persists across a verified-correct source as a
stale-cache symptom first, not a new defect.

**`tools/coverage.sh`/`tools/coverage_cpp.sh` clean up after themselves** (they delete their own
`build/gcov`/`build/gcov-cpp` tree on exit, since an instrumented `error_scenarios` binary left under
`build/` is indistinguishable to the lookup below) — `COVERAGE_KEEP_BUILD=1` keeps it, and then it is
yours to delete before the next plain `fpm test`.

**`tools/run_error_scenarios.sh` resolves its binary the same way** (`find "${FPM_BUILD_DIR:-build}"
-type f -name error_scenarios | head -n 1`), and there the failure direction is the dangerous one: it
can print **"All error scenarios behaved as expected"** while running a binary that predates your
edits entirely. A green error-scenario run is therefore only meaningful when `find build -type f -name
error_scenarios | wc -l` is 1. Run `fpm clean --skip` first whenever you have built with more than one
`FPM_FFLAGS` value in a session (a coverage run, an OpenMP run, a release build), and treat a *new*
scenario that passes first time with suspicion until you have seen it fail against a deliberately
broken implementation.

### Keeping `tools/prep_fpm_publish.sh` in sync

`tools/prep_fpm_publish.sh` builds the tarball content for `fpm publish` (see CONTRIBUTING.md's
"Publishing to the fpm registry") by committing a disposable local branch that strips
maintainer/CI-only files (`REMOVE_PATHS`) and edits `fpm.toml` (comments out `test-drive`, flips
`module-naming` to `"parquet"`). This list/logic silently goes stale unless updated alongside the
change that invalidates it — watch for these triggers:

- **A new file lands under `tools/`.** Decide whether it's consumer-facing (like
  `tools/generate_parquet_maml.sh`, documented in `doc/pages/embedding-maml-schemas.md`) or
  maintainer/CI-only. If the latter, add it to `REMOVE_PATHS`. (A missing/renamed entry fails
  loudly — the script pre-validates every path exists — so this is at least self-enforcing for
  *existing* entries; it won't catch a *new* file that should have been added but wasn't.)
- **A new maintainer/CI-only file lands at the repo root** (another CI config, another
  AI-instructions-style file, etc.) — same call: add to `REMOVE_PATHS` if it's not
  consumer-relevant.
- **A new module is added to `src/`.** It must be named `parquet` or start with `parquet_`, and
  **nothing on `main` will tell you otherwise**: `fpm.toml` carries `module-naming = false` there,
  so a badly-named module builds, tests and ships in the working tree indefinitely — the
  constraint only appears when this script flips the setting to `"parquet"` for the registry,
  which may be months later and after the name is already in downstream code. Confirmed by
  flipping the setting and adding a `module example` to `src/`:
  `ERROR: Module example in ./src/example.f90 does not match its package name (parquet-fortran)
  or custom prefix (parquet)`. **`test/*.f90` is exempt** — test modules are not checked, which is
  why `test_table` and friends are fine and why the asymmetry is easy to mistake for "the rule
  does not apply to us". This matters most for a *generated* module, where the name comes from
  data rather than from a person: `tools/generate_user_table_code.py` takes it from its MAML's
  `dataset:` key, so a schema naming a module `example` produces a package that cannot be
  published.
- **Any `REMOVE_PATHS` entry is renamed or moved.** Update the path string. The script's
  pre-flight existence check turns a stale entry into an immediate, zero-side-effect failure
  rather than a silently-wrong tarball — but only once you actually run it; nothing catches this
  at edit time.
- **A new dev-dependency is added to `fpm.toml`** — check whether its own modules comply with fpm's
  [module-naming rules](https://fpm.fortran-lang.org/registry/naming.html) before adding it. If
  not, it needs the same "comment out in the disposable branch" treatment as `test-drive`, or it
  will reintroduce the build-breaking conflict documented in CONTRIBUTING.md.
- **The exact literal text of the `test-drive.git = ...` or `module-naming = false` lines in
  `fpm.toml` changes** for unrelated reasons — the script's own `SystemExit` checks already catch
  this by failing loudly, but it's worth knowing why a future `fpm.toml` edit might break the
  publish script.
- **Upstream fpm or `test-drive` fixes the module-naming compliance gap** (fpm PR
  [#828](https://github.com/fortran-lang/fpm/pull/828) / issue #883) — if fpm ever gains a
  per-dependency naming exemption, or `test-drive` renames its modules to comply, revisit whether
  the whole `test-drive`-comment-out workaround (and possibly `module-naming = false` on `main`)
  is still needed at all.

### Manual (never-`fpm test`) large-scale/benchmark tools

A user/maintainer-runnable check that needs more memory/disk/time than `fpm test`/CI should ever
attempt (e.g. genuinely exceeding `huge(1)` rows, or a multi-GB benchmark file) goes under `app/`
(an `auto-executables` fpm target — never auto-picked-up by `fpm test`, unlike anything under
`test/`) plus a thin `tools/*.sh` wrapper with env-var config (matching this repo's other
`tools/*.sh` scripts), never under `test/`. See `app/benchmark_threads.f90`/
`tools/benchmark_threads.sh` and `app/test_large_scale.f90`/`tools/test_large_scale.sh` for the
established shape: CLI flags (`--key=value`) parsed via `get_command_argument` in the Fortran
program; env vars read and forwarded as those flags by the shell wrapper
(`NAME="${NAME:-default}"` then `fpm run <app> -- --key="$NAME"`); `set -euo pipefail`; `cd` to
the repo root first. Document usage (parameters, defaults, example invocations) in
CONTRIBUTING.md's "Other tools/ helpers" section, not README.md — this is a contributor/
maintainer tool, not part of the public library API.

**Three traps when writing one of these, all of which produced a confidently wrong number here
before being noticed:**

- **Warm the data before timing anything, on any lazy API.** `parquet_open_table` reads no column
  data, so whichever measured path touches a column *first* silently absorbs the entire decode. This
  made `%get` look 8.94x `%col`, and `parquet_write_table` look 2.5x a hand-written write loop; both
  collapsed to roughly parity once the benchmark called `%prefetch`/`%materialize_all` up front. If
  two paths in one benchmark read the same column, exactly one of them is paying for it.
- **Warm the result array's pages too.** A freshly allocated large output array pays first-touch page
  faults on its first pass and never again, so whichever variant runs first absorbs them. This is
  strong enough to reverse a comparison: the `%col` pointer form measured *25% faster* than plain
  arrays purely by running second. Write the result once through every path before the timed loop.
- **Always pass `--profile release`; never try to get optimisation out of `FPM_FFLAGS` instead.**
  fpm applies profile flags only when `--profile` is given, so `FPM_FFLAGS="-fopenmp" fpm run` with
  no profile builds at **-O0** — not because `FPM_FFLAGS` suppressed anything (it does not; see
  "Don't run the GitLab CI pipeline yourself" for the four-way table), but because no profile was
  asked for. A bare `fpm run` with `FPM_FFLAGS` unset is equally unoptimised. And the `-fopenmp`
  there is redundant anyway. Reaching for `FPM_FFLAGS="-O3" fpm run` instead optimizes the Fortran
  half while leaving the C++ half at whatever
  the environment supplies — which on a dev machine may be nothing. Measured 5.7x on `pf_argsort`
  alone between the two, which is enough to invert a comparison and did: an early run of the sort
  benchmark showed bit-packing *losing* below 2M rows, an artifact that vanished under `-O3`. The
  `tools/*.sh` wrappers already pass `--profile release`; match them.
- **Take the best of several rounds, not one measurement.** Single rounds of the `access` mode swung
  0.96x–1.22x on the same build — wider than the effect being measured. The minimum is the run least
  disturbed by everything else on the machine, which is what these modes are actually asking about.
- **An accessor that copies is not a read — make two modes being compared do the SAME job.** `%get`
  allocates a fresh array the size of the column and copies into it, so a `%get` loop measures the
  read *plus* a full allocation and copy per column, and `sum(...)` on top adds another whole pass.
  Timed against `%materialize_all`, which only reads, that made reading 4 of 16 columns look 1.8x
  what scaling the full read by `touch / ncols` predicts — reported as the library failing to scale
  when the two sides were simply not measuring the same work. Compare `%prefetch` with
  `%materialize_all`; time any `%get` on its own line; keep the keep-it-live checksum outside every
  timed region and take it through `%col`. The same caution applies to any future accessor whose
  cost scales with rows.
- **A reference has to be allocation-free, or it is not a reference.** An existing library operation
  pressed into service as a "memcpy floor" measures its own allocation and first-touch page faults,
  not bandwidth: `%clone` labelled that way came out **27-30x** slower than a genuine warm-buffer
  copy of the same payload, and varied **5.6x between runs at the same size** on a large NUMA
  machine, where a cold destination is dominated by faulting. Allocate **and fully write** both
  buffers before the timer starts. **The falsifiable tell is worth remembering, because two
  independent reviewers used it to reject the number: if the thing being compared comes out FASTER
  than the "floor", the floor is wrong.** Every ratio taken against it is then meaningless in an
  unknown direction, which is worse than having no reference at all.

**To measure the committed baseline against the working tree**, `git stash push -- src tools`, rebuild,
measure, then `git stash pop` — this keeps `app/`, `test/` and the fixtures in place, so the benchmark
program and its input do not change between the two halves of the comparison. Confirm the stash popped
cleanly (`git status`) before trusting the "after" number.

### Measuring whether Arrow memory was actually freed: RSS cannot answer, the pool counter can

**Resident set size does not fall when Arrow buffers are freed**, so it cannot be used to verify that
some code path released them. Arrow allocates through `arrow::default_memory_pool()`, which keeps
freed pages instead of returning them to the OS (and glibc/macOS `malloc` behave the same way for
ordinary allocations). A correct release and a complete failure to release therefore look nearly
identical in `ps`/`/proc` output — and the difference that *does* show up is the transient peak, which
misleads in the opposite direction.

Measure `arrow::default_memory_pool()->bytes_allocated()` instead. `parquet_get_arrow_bytes_allocated`
(`src/parquet_wrapper.cpp`) exposes it; it is deliberately **not** declared in
`src/parquet_bindings.f90`, because it is a maintainer diagnostic rather than public API — the
consumer declares its own local `bind(C)` interface for it, the same convention the `parquet_debug_*`
hooks follow. Two further rules when writing such a measurement:

- **Measure each path in its own process.** Running a baseline and the path under test in one process
  reports the high-water mark of the pair, which makes whichever ran second look like it retained
  memory it had already released.
- **State in the output which number is the real answer.** A report that prints RSS next to the pool
  figure invites the reader to draw the wrong conclusion from the wrong line.

### A `shared_ptr` parameter on a per-row helper is the first thing to suspect in `parquet_wrapper.cpp`

A helper that takes `const std::shared_ptr<arrow::Array> &` and is called **once per row** pays two
atomic refcount operations per call, because every `std::static_pointer_cast` inside it builds a new
`shared_ptr`. Measured here: reading one `double` through `real_family_value_at` that way cost
**13.6 ns per row**, against **1.8 ns** once the helper took a plain `const arrow::Array *` — a 7.6x
improvement in the filter's clause evaluation and 3-4x in the whole cost of installing a filter, with
no threading involved at all. The caller already owns a reference for the duration of the loop; the
loop needs the pointer, not a share of the ownership.

**Three things make this worth a standing note rather than a one-off fix:**

- **Nothing fails when it comes back.** Every answer stays identical and the whole suite stays green;
  only a benchmark notices. `tools/check_source_conventions.py`'s `check_no_per_element_shared_ptr`
  is what actually guards it, matching by *shape* (an array parameter beside an element index) so it
  cannot go blind to the next helper added. See `feature_risks.md` Risk-59.
- **It hides from a profile that samples by function name**, because the atomics are attributed to a
  helper that was already expected to be hot.
- **The plausible-looking explanation was wrong**, which is the part worth copying. The same loop
  called `compare_op(value, bound, op)` with the operator as a `std::string`, testing it with up to
  five string comparisons **per row** for a loop-invariant value. Converting that to an enum changed
  the measurement by **nothing** — the compiler was already hoisting it. Measure the fix, not just
  the symptom: an obvious inefficiency that is genuinely there can still be worth 0%.

### Instrument phases before optimising a multi-phase operation

"X is slow" is not actionable until it is known *which part* of X is, and the cheapest way to find
out here is a few `parquet_debug_get_*_nanos` counters in the `parquet_debug_*` family (no
`src/parquet_bindings.f90` entry; the consumer declares its own local `bind(C)` interface). Three
counters around `parquet_reader_set_filter`'s decode / evaluate / mask-build phases turned a vague
"the filter is expensive" into "93% of it is one helper's parameter type", and they turned the *next*
question — whether to parallelise what remains — from a design argument into a number.

**Leave them in.** They cost nothing at runtime (one `steady_clock` read per phase, on a path that
runs once per reader open) and they are what makes the next measurement one benchmark away rather
than a fresh investigation. The rule that governs where such a hook may sit is the existing one: a
debug hook may sit on a coarse operation, never on a per-row or per-element path.

## Testing & coverage

### Running a single test suite/test

Use `fpm test run_tester -- <suite>` to run just one test-drive suite (e.g. `fpm test
run_tester -- reading`), or `fpm test run_tester -- <suite> "<test name>"` to run a single
named test within it. Prefer this over a full `fpm test` while iterating — the full suite
(including OpenMP/error-scenario subprocess tests) takes much longer than the one suite
relevant to a given change.

### Error scenarios are pre-run in parallel

`run_tester` calls `prime_error_scenarios` (`test/test_errors.f90`) before any suite starts: one
`execute_command_line` runs every scenario named in `tools/run_error_scenarios.sh`'s
`scenarios=(...)` array through `xargs -P`, capturing each one's exit status and its two streams
under `test_run/.primed/`. `run_error_scenario` then answers from those files. This is what takes a
full `fpm test` from ~72 s to ~25 s here — the ~630 scenario subprocesses were ~60 s of it, run
strictly one at a time because the suites driving them are excluded from test-drive's parallelism.

Four things to know before touching this area:

- **The parallelism is deliberately in the shell, not in the test process.** Exactly one fork
  happens, from `run_tester` with no OpenMP team active, so the libiomp5 fork hazard that forces
  `suite_is_safe_to_parallelize`'s exclusions is never approached. Do not "simplify" this into
  per-test spawning from a parallel suite; that is the thing the exclusion exists to prevent.
- **A new scenario must be added to `tools/run_error_scenarios.sh`'s array**, which
  `tools/check_source_conventions.py`'s `check_scenario_list_is_complete` now enforces (it derives
  the names by shape from `error_scenarios.f90`'s `select case`, so it does not go stale). Forget
  it and nothing fails — the scenario just falls back to spawning on demand — which is precisely
  why the check exists.
- **Every failure path degrades to the old on-demand spawn, never to a wrong answer.** That is
  load-bearing: it is what makes a missing list entry cost speed instead of coverage.
  `test_run/.primed/` is wiped by the prime itself and `g_prime_ok` is per-process, so a directory
  from an earlier run can never be consumed as this run's result — a vacuous pass against a binary
  that no longer exists is the failure mode this design is shaped around.
- **`PARQUET_TEST_NO_PRIME=1` turns priming off** (debug one scenario without 686 others running
  first); `PARQUET_TEST_PRIME_JOBS=<n>` sets the concurrency. A named single test never primes.

Both streams are now always captured **separately** — the old `2>&1` merge is gone, because the
primed and spawned paths have to produce the same shape. A helper wanting the old "appeared
somewhere" semantics calls `scenario_capture_contains`, which searches both.

### Tests run concurrently: never share a fixture file path between two tests

test-drive runs the tests in a suite **concurrently**, so two tests that write to the same fixture
path can truncate the file out from under each other — one opens it while the other is rewriting it,
and the reader gets an empty or half-written file (`Parquet file size is 0 bytes`). **Give every test
its own fixture filename**, even when the contents are identical, and even when one test is
"obviously" going to run before the other.

This failure is **timing-dependent, so a green run proves nothing**: the same tests can pass under a
plain `fpm test` and fail under `tools/coverage.sh` (instrumented builds change the timing), or pass
for months and fail on a busier machine. If a test that touches files fails intermittently or only
under coverage, check for a shared path before looking anywhere else. Where several tests genuinely
need the *same* fixture contents, factor the writing into one shared helper that takes the filename
as an argument, and have each caller pass its own.

**`tools/run_error_scenarios.sh` is concurrent too (`xargs -P`), and there the rule is easiest to
break by accident, because the collision is between two SCENARIO NAMES rather than two tests.** One
parameterized helper backing several `case` entries — `scenario_settings_cpp_warning(level=...)`
and friends — is one *process per name*, all running at once over whatever fixture path the helper
hardcodes. **So a helper invoked from more than one `case` must derive its fixture path from its
own arguments** (`"..._" // trim(level) // ".parquet"`), never carry a
`character(len=*), parameter :: out_file`. Check this whenever a scenario helper gains a second
call site; `grep -oE "call scenario_[a-z0-9_]+" test/error_scenarios.f90 | sort | uniq -c` lists
every multi-invoked helper.

**Its failure signature is much more alarming than the test-drive one, and points away from the
real cause.** The reader gets a half-written file, Arrow throws
`IOError: Couldn't deserialize thrift`, and — because nothing catches an exception crossing the
`extern "C"` boundary — the process dies via `std::terminate` with **exit 134**, i.e. an abort in a
scenario whose expected exit is 0. That reads as a genuine library crash in whatever the scenario
was exercising, not as a fixture collision. **Reproducing it needs forced interleaving, not more
parallelism**: on an idle machine the pair finishes too fast to overlap, and 420 runs came back
clean, while pinning both processes to one core (`taskset -c 0`) took it straight to 69/160. Reach
for `taskset` before concluding a one-off — and note that a CI re-run going green is exactly what
this bug does.

**A test that WRITES process-global state needs its suite excluded, and the reasoning is not about
files.** `parquet_settings`' knobs are saved module variables, the sort comparison counter and the
pruned-row-group count are C++ statics: any test that sets one is visible to every sibling running
at the same time. The failure is usually not a crash but a *vacuous pass* — a sibling flipping a
setting mid-run can leave an A/B test comparing one code path against itself, which passes while
testing nothing. `filter_screen`, `sorting`, `sort` and `settings` are all excluded for this reason
(`test/run_tester.f90`'s `suite_is_safe_to_parallelize`). Note `sort` was excluded only after the
fact: it had written a process-global setting for a long time without incident, because its
assertions happened to be path-agnostic — so **"it has always passed" is not evidence that a suite
writing global state is safe**, only that nothing has yet asserted anything sharp enough to notice.

**Second consequence, for any library code that inspects OpenMP state:** test-drive achieves that
concurrency with its own `!$omp parallel do`, so under a `-fopenmp` build **`omp_in_parallel()`
returns `.true.` inside every procedure a test calls**. A guard that refuses to do something "inside
a parallel region" therefore fires during the entire test suite, not just in the test that meant to
provoke it. **This is NOT avoided by running `fpm test` without `-fopenmp`** — `fpm.toml` declares
the `openmp = "*"` metapackage, which supplies the OpenMP flag across the whole resolved dependency
graph, so `_OPENMP` is defined and every `#ifdef _OPENMP` guard is compiled in even for a bare
`fpm build` with `FPM_FFLAGS` unset entirely (verified directly with a minimal standalone fpm
project). An earlier version of this note claimed the opposite — that a plain `fpm test` compiles
such a check out, so the suite would pass locally and fail only in CI — and that has not been true
since the metapackage was adopted. The rule that follows is simply to
prefer a guard keyed on something more precise than "am I in a parallel region" — see
`unsafe_first_touch` (`parquet_tables_read.f90`), which records at open time *which thread* created
an object and refuses only when the object could actually be shared, so a thread-private object used
in the obvious way is unaffected. `test/run_tester.f90`'s `suite_is_safe_to_parallelize` can exclude
a whole suite from that parallelism, but reach for it only when the suite genuinely cannot run
concurrently — narrowing the guard is the better fix, since the suite's parallelism is itself an
ongoing regression check.

### Every `check()` call needs its own message

Every test-drive `call check(error, condition, ...)` in `test/*.f90` should carry a message
argument, not just the bare condition. Without one, a failure reports only a file/line number —
which combines badly with this suite's common `all(...)`/compound-condition idiom (e.g.
`all((dates + offsets) == expected)`), where a bare line number gives no hint which element or
sub-condition actually failed. A convenient default when there's no more descriptive text at hand
is the condition's own source text as a string (whitespace-normalized, with any embedded `"`
doubled per Fortran's escaping rule) — better than nothing, and it at least echoes back exactly
what was expected without requiring a separate hand-written description. Apply this to every new
`check()` call from now on, not only when a review flags a gap.

### Verifying a change with mutation testing

A green test suite does not prove a new test covers what it was written for. The cheap check is to
break the code deliberately — invert a guard, delete a branch, return a constant — and confirm the
test fails. Worth doing for anything whose failure mode is *silent*: a fast path that skips work, a
short-circuit, a cache. This repository's recent examples are the validity-mask skip, the
`col_size` footer screen, and the write-side row-granular null mask; each had at least one mutation
that a first round of tests did not catch.

Three things about doing it *here* specifically:

- **Detect an abort, not just a failed check.** This library reports almost every error with
  `error stop`, so a mutation frequently makes a test **crash** rather than fail an assertion —
  `grep -c '\[FAILED\]'` then reports 0 and the mutation looks survived. Always check the exit
  status too (`error stop` → nonzero, SIGABRT → 134, SIGSEGV → 139/11 through `fpm run`). Getting
  this wrong made 3 of 5 mutations look uncaught in one session when they were all caught.
- **Check WHICH CODE PATH the test actually reaches before trusting it.** A mutation to the
  comparator survived a stability test twice: first because the test compared two procedures that
  both use that comparator (so it broke them identically and the comparison still held), then
  because the fixture — low-cardinality integers — took the counting fast path, which never calls
  the comparator at all. **Zero invocations is a passing test.** Assert against an independently
  constructed expectation rather than a second call into the same machinery, and where a fast path
  exists, force the slow one (`parquet_debug_set_disable_sort_counting_path` and friends) or pick a
  fixture the fast path declines. See `feature_risks.md` Risk-35.
  **A SIZE THRESHOLD is the same trap wearing different clothes, and is easier to miss** because
  nothing about it looks like a fast path. The co-ranked merge only splits a run past a
  16384-element floor, so an entire dense sweep over arrays of 2-8192 elements exercised the
  unsegmented path — the very code the feature replaced — and two deliberate co-rank defects
  survived it. Whenever a new constant gates behaviour on input size, give it a
  `parquet_debug_set_*` override and have the tests lower it, exactly as
  `parquet_debug_set_sort_merge_min_segment` now does. See `feature_risks.md` Risk-49.
- **A surviving mutation is not automatically a coverage gap.** It may be *masked*: by a redundant
  sibling guard (removing either alone changes nothing — see `column_has_nulls_from_footer`'s
  `is_stats_set()`/`HasNullCount()` pair, where removing both segfaults), or by a later check that
  catches the same error anyway (the `col_size` footer screen is masked by the row-group scan that
  follows it). Test the pair, or the tier below, before concluding anything.
- **A surviving mutation may be semantically a NO-OP, in which case it proves the design rather
  than exposing a gap.** Check that the mutation actually changes behaviour before concluding the
  test is weak. The worked example: flipping the parallel merge's tie rule from "take the left run
  unless the right is strictly less" to "take the left run when it is strictly less" changed
  nothing on any fixture — because `SortRowLess` is a **total order**, `less(a, b)` and
  `!less(b, a)` are the same predicate and there are no ties for the merge to break. The realistic
  defect was a different edit (substituting the tiebreaker-free comparator), and that one was
  caught. A mutation that is not a behaviour change is not evidence about the tests at all.
- **If a mutation cannot be caught by any fixture this repository can build, the branch is
  defensive** — say so in a comment and `GCOVR_EXCL` it rather than deleting it or inventing an
  unbuildable fixture. `list_uniform_width`'s `IsNull` check is the worked example: Arrow's own
  `ListBuilder::AppendNull` already leaves `value_length == 0`, so no Arrow-built array reaches it.

### A test that asserts a REFUSAL must say what to assert when the refusal lifts

Some guards refuse a case on **cost** rather than correctness — the machinery is built and tested,
and the clause exists only because a measurement said the case was not worth it yet. A test asserting
such a refusal is correct today and is the wrong test tomorrow, and whoever lifts the clause meets a
failing test with no indication whether it is protecting something or merely out of date.

**Write the deferral into the test's own doc-comment**, in the form "when X lands, this becomes an
equality test, not a deletion — what it asserts today is that the refusal is real, and what it should
assert afterwards is that Y kept the answer." Two such tests were written that way for the prefetch
gate's `filter=`/`sort=` clauses and both were converted rather than deleted when the clause lifted,
by following their own instructions. The same applies to the *code*: a refusal comment must read
"deferred until X", never "this cannot be done", or the next reader takes the clause as settled.

### A static check that enumerates names goes stale silently

A check in `tools/check_source_conventions.py` that works from a *list* of names — helper procedures,
constants, call sites — stops seeing anything the list does not mention, and says nothing about it.
It keeps reporting `[ok]`, so there is no moment at which anyone learns it has narrowed.

This has happened twice to one check. `check_print_settings_documented` extracted its rows by
matching `print_one`, then `print_one|print_text`, and each time a new row helper was added it went
blind to those rows — the second time reporting two of five new rows as undocumented while passing
the other three, which is worse than failing outright, because a partial failure looks like a
complete answer. It now matches by **shape** (`call print_<anything>(u, "name"`), which picks a new
helper up with no edit.

**Prefer matching a shape over enumerating names**, and where a list is genuinely unavoidable, have
the check fail when the list comes up empty rather than pass — an empty result almost always means
the code moved, not that the invariant holds. Where two checks need the same list, derive it from
one place: the documentation and environment-coverage checks both take their knob list from
`parquet_print_settings`' own printed rows, so a new knob fails both together instead of needing two
separate lists updated.

**The same blindness applies to a one-off AUDIT, where nothing reports `[ok]` and there is no second
chance to notice.** A hand-written search pattern used to answer "how many places do this?" is itself
untested, and its answer is quoted afterwards as though it were a count. A grep over `src/` for a
per-element allocation reported **4 sites**; converting the same question into a lint check over the
same scope found **17**. The audit's regex required the destination to be the call's last argument,
and the dominant real shape puts it second-to-last — so it saw the four instances that happened to
match the form the pattern was written from. **Before quoting a count, run the pattern against a
known instance you did NOT use to write it**, and prefer turning the audit into the check rather than
reporting a number and building the check later; the check is the thing that gets re-run.

**When the count is too large to fix at once, ratchet it rather than allow-list it.** An exemption
list says "these are fine"; a ratchet says "these are debt, and it may only shrink". Record a
per-file count of the remaining instances and fail on **both** directions — a file gaining one, and a
count left too high after someone fixes one. That keeps a new instance in a file nobody listed
visible, makes the list self-correcting instead of stale, and turns the remaining work into something
a reader can see the size of. `check_no_per_element_string_alloc`'s `KNOWN_REMAINING` is the worked
example; verify a new ratchet fires in both directions before trusting it, because the
count-left-too-high half is the one that never fires on its own.

### Measuring test coverage

Run `tools/coverage.sh` for per-file and total `src/` line coverage plus the uncovered line
ranges; pass one run_tester suite name to scope it (e.g. `tools/coverage.sh reading`), or no
argument for a full run (which also runs every error scenario). It auto-selects the `gcov`
matching the active `gfortran` — a mismatched gcov fails with "Invalid .gcno file!".

When closing coverage gaps, sort each uncovered line by type first: an `error stop`/abort
line can *only* be covered by an out-of-process scenario (`test/error_scenarios.f90` + a
`test/test_errors.f90` wrapper + a `tools/run_error_scenarios.sh` entry), never by an
in-process test-drive test (the abort kills the process); a normal branch is usually
reachable by extending an existing test with different data/arguments. Genuinely not
coverable and not worth chasing: `end module`/`end submodule` lines, implicit finalizers,
and interface-only files (`parquet_bindings.f90`). `src/parquet_wrapper.cpp` is measured
by `.gitlab-ci.yml`'s `test` job (its `gfortran`/`gcc`/`g++` are one matched apt GCC install,
so `gcovr` reads its gcov data cleanly alongside the Fortran sources) but deliberately
**not** by `tools/coverage.sh`'s local run, since an arbitrary dev machine's `FPM_CXX`
(e.g. a default `clang++`) may not produce gcov data in a format the locally-resolved GNU
`gcov` can read — see CONTRIBUTING.md's CI section. For local `src/parquet_wrapper.cpp`
coverage on exactly that kind of machine, use the separate `tools/coverage_cpp.sh` instead
(not a flag on `tools/coverage.sh` — Fortran and C++ coverage can't be instrumented/collected
in the same local pass when the toolchains don't match); see CONTRIBUTING.md's CI section for
what it does and why it's a standalone script. See "`src/parquet_wrapper.cpp`: GCC vs Clang gcov
attribution" below for why local and CI coverage can disagree and the conventions that keep them
in sync.

Both `tools/coverage.sh` and `tools/coverage_cpp.sh` report an extra section after the per-file
summary and uncovered-line list: every `GCOVR_EXCL`'d line that actually had a positive local hit
count, i.e. a candidate for a stale/no-longer-dead exclusion worth revisiting (not automatically
fixed — just surfaced). `tools/coverage_cpp.sh` additionally splits this into two: lines tagged as
a GCC-attribution artifact (see below) that unexpectedly show *zero* hits locally (investigate —
these are expected to be genuinely covered under Clang) and everything else that shows *positive*
hits (the stale-exclusion candidates). `tools/coverage.sh` only needs the latter, single section,
since Fortran coverage uses the same `gfortran`/`gcov` toolchain locally and in CI — there's no
GCC-vs-Clang split to make for `src/*.f90`.

### Fortran gcov attribution artifacts

Confirmed by inspecting raw per-line gcov hit counts (not just `tools/coverage.sh`'s summary):
gfortran/gcov sometimes marks an excluded, genuinely-dead line as "hit" even though it never
executes — a different mechanism from the GCC-vs-Clang split documented below (this one is
gfortran-only, present identically locally and in CI), so it needs its own convention rather than
reusing that section's.

Two confirmed shapes:

- **A guard-clause `if (cond) then` line inside a `GCOVR_EXCL_START`/`GCOVR_EXCL_STOP` block.** The
  condition itself is evaluated on *every* call to the function, whether or not the guarded
  (excluded) body is ever reached — so gcov counts that line as executed regardless. The body
  immediately inside (the `error stop`/diagnostic-and-`return`) reliably shows 0 hits, proving the
  branch itself is never actually taken; only the `if` line's own hit count is misleading. Seen in
  every `check_*_fits_arrow_limit`-style int32-ceiling guard (e.g. `parquet_read.f90`'s
  `parquet_get_nrows_int32`), every defensive branch in `parquet_strings.f90`'s `validate()`, and
  the col_map/duplicate-output-name/empty-name guards in `parquet_metadata_maml.f90`.
- **A bare `return` statement mis-attributed to a different call site's execution count**, seen in
  `parquet_read.f90`'s `parquet_tokenize_filter_rule`, `case default` arm: the `return` on the line
  right after an `errmsg = ...` assignment shows positive hits while that `errmsg` line — the
  *same* never-taken branch, one line above — reliably shows 0. Root cause not fully diagnosed
  (build uses `-O0`, so this isn't ordinary optimizer basic-block merging); treat as gfortran/gcov
  bookkeeping quirk, confirmed via the sibling line's zero count, not a real coverage gap.
- **A CI-only (toolchain-dependent) miss on the first executable statement of an abbreviated
  `module procedure ... end procedure` body, right after its declaration block** — seen in
  `parquet_metadata_maml.f90`'s `parquet_load_maml_file`/`parquet_load_qc_maml_file`, both of
  whose opening `call parquet_read_maml_source_lines(...)` line. Unlike the two shapes above, this
  one is *not* reproducible with a local `tools/coverage.sh` run (that run shows this file at a
  clean 100%, including these exact lines) — it only shows up in GitLab CI's own gcov/gcovr
  invocation, but does so consistently (confirmed across 4 separate CI runs on the same commit,
  same two lines every time), which rules out a one-off parallel-`.gcda`-write race and points to
  a CI-image-specific gfortran/gcov version difference from whatever's installed locally. Both
  procedures are confirmed exercised end-to-end (`test_load_maml_file`/`test_load_qc_maml_file` in
  `test/test_maml.f90`). If a *new* abbreviated `module procedure` body's own first statement
  (immediately following its declarations, no intervening blank-line-only gap) shows the same
  "0% only in CI, 100% locally, reproducible across ≥2 CI runs" pattern, treat it the same way —
  don't assume it's this shape from a single CI run alone, since a genuine one-off race is also
  possible; require the repeat-across-runs confirmation first.

**Convention for tagging a confirmed site** (mirrors `src/parquet_wrapper.cpp`'s own "gcov
attribution artifact under GCC" convention below, adapted for Fortran's stricter 132-column limit):
carry the literal phrase `gcov attribution artifact` either inline on the same
`GCOVR_EXCL_START`/`GCOVR_EXCL_LINE` marker line, or — when appending it inline would blow the
132-column limit — on a plain comment line (or a short contiguous run of them) directly above the
marker line instead. `tools/coverage.sh`'s `gcovr_artifact_lines()` recognizes both forms and drops
those lines from the "candidates for a stale/no-longer-dead exclusion" report, so a positive hit
there is expected and not something to chase. Before tagging a *new* site this way, verify it's
actually this pattern (check the raw per-line JSON hit counts the way this note's examples were
verified — a guard body genuinely showing 0 while its own `if` line shows positive, or a sibling
line in the same unreachable branch showing 0) rather than assuming; an exclusion that's truly gone
stale (the guarded code is now reachable and should count as covered) needs the marker removed
instead, not tagged as an artifact — see `parquet_strings.f90`'s `compact_all` header line, which
was found to be exactly that case (a real, now-covered subroutine header, not this artifact) and
had its `GCOVR_EXCL_LINE` removed rather than tagged.

### `src/parquet_wrapper.cpp`: GCC vs Clang gcov attribution

Confirmed via a real GitLab CI run: GCC's actual gcov and Clang's `llvm-cov gcov` (the default
backend `tools/coverage_cpp.sh` uses locally) attribute per-line hit counters differently for
several C++ source shapes, even for code that unquestionably executes under both. This is why
CI's coverage percentage can diverge from a local `tools/coverage_cpp.sh` run on the identical
commit, and why some `GCOVR_EXCL` markers sit on lines that are demonstrably, constantly
covered — they aren't dead code, gcov just can't always see it, so don't strip those markers as
if they were wrong. Divergent shapes found so far:
switch `case`/`default:` labels (especially the first label of a fall-through group), a function's
closing `}` immediately after its own `return`, a lambda's parameter-list line, and continuation
lines of one multi-line chained statement (`xml << ... << ...`, a multi-line `fprintf`/function
call). Before assuming a new CI-only-uncovered line is a real gap, check whether it's one of these
shapes and whether the surrounding code is otherwise demonstrably covered (e.g. by a call-graph
check, or the fact that a dependent test's assertions pass) — if so, it's this phenomenon, not a
missing test.

**Mechanical conventions to preserve when adding or moving a `GCOVR_EXCL` marker in this file**
(violating any of these silently reintroduces a local/CI coverage mismatch or miscategorizes a
report entry):

- **`GCOVR_EXCL_STOP` must be on its own dedicated comment-only line, never trailing real code**
  (`<code>; } // GCOVR_EXCL_STOP` is wrong). Real `gcovr` (CI) does not extend the exclusion to a
  `STOP` line that also carries code — only lines strictly between `START` and such a line are
  excluded, leaving that line's own code counted as an ordinary (uncovered) line. This is *not*
  what `tools/coverage_cpp.sh`'s own exclusion logic does locally (it always includes the `STOP`
  line itself) — the local tool is more lenient than the tool CI actually runs, so a `STOP` line
  that carries code can pass locally yet count as uncovered in CI.
- **A `case`/`catch`/`default:` label sitting just outside its block's `START`/`STOP` (immediately
  before `START`, or immediately after a preceding `STOP`) needs the marker moved to include it.**
  GCC gives a label its own line-attribution, separate from the code that follows it — a marker
  that only wraps the body leaves the label itself reported as a false gap.
- **A new exclusion added because a line is genuinely covered but GCC misattributes it (as opposed
  to genuinely dead/unreachable code) must carry the literal phrase `gcov attribution artifact
  under GCC` on the *same physical line* as its `GCOVR_EXCL_LINE`/`START` marker.**
  `tools/coverage_cpp.sh`'s reporting categorizes exclusions by searching for this exact substring
  on the marker's own line — wrapping the phrase onto a following, unmarked comment line silently
  drops it into the "other" (dead-code) bucket instead, and the new "zero hits" report won't catch
  it either.

**Confirmed root-cause mechanisms behind why so many lines in this file need `GCOVR_EXCL` at all**
(general knowledge for any future coverage work here, not just the GCC/Clang split above):

- `report_fatal_error` and `ConcurrencyGuard`'s constructor call `std::abort()` directly.
  `std::abort()` skips every `atexit`-registered handler, which is how gcov/llvm-cov flush a
  translation unit's counters — so a process that aborts contributes **zero** coverage data for
  that entire run, for every line it executed, not just the abort line itself. This applies to
  *every* `report_fatal_error` call site, present and future (`.gitlab-ci.yml`'s gcovr invocation
  auto-excludes standalone `report_fatal_error(...)` lines via `--exclude-lines-by-pattern`, mirrored
  by both coverage scripts' own `EXCLUDE_LINE_PATTERNS`) — but a helper function called just
  *before* the abort needs its own reasoning check, not an assumption that it's covered elsewhere.
- An uncaught C++ exception crossing the `extern "C"` boundary hits the same
  discard-all-coverage-data wall via `std::terminate()`, not just `report_fatal_error`. This file
  has exactly one working `try`/`catch` in its ~6800 lines (`parquet_reader_set_filter`) — even an
  artificial `throw` placed at the very top of a function it calls, invoked from directly inside
  that `try` block, was confirmed to escape uncaught under this project's mixed
  gfortran-driven static-library link. Treat any other `throw` site in this file as unreachable by
  a clean test by default.
- A Fortran-side pre-check often makes a C++-side defensive branch unreachable through the public
  API (e.g. `parquet_read.f90`'s `parquet_check_read_row_count`/`check_column_exists` make several
  "mismatch"/"not found" `report_fatal_error`s in `parquet_wrapper.cpp` dead code). Before writing
  a test for an uncovered `report_fatal_error`/`throw` line, `grep` the relevant
  `src/parquet_*.f90` call site for an equivalent pre-check first.
- Parquet C++'s Arrow reader always materializes a decimal column as `Decimal128Array`/
  `Decimal256Array` on read, never `Decimal32Array`/`Decimal64Array`, regardless of the physical
  `DECIMAL32`/`DECIMAL64` width actually written — so those case arms in `decimal_value_at`/
  `decimal_to_int64_checked` are permanently dead on the read path, not a testing gap, on any
  input.

If you ever find a line marked `GCOVR_EXCL_LINE`/inside a `GCOVR_EXCL_START`/`GCOVR_EXCL_STOP`
block that is actually reachable in normal (non-abort) operation — i.e. the exclusion looks
wrong, not just the line being hard to test — stop and notify the user about it rather than
silently leaving it excluded or removing the marker yourself.

**A specific gcov/gfortran quirk to know about, not chase:** in `src/parquet_temporal.f90`, six
`impure elemental` procedure header lines (`date_parse`, `date_new`, `time_parse`, `time_new`,
`ts_parse`, `ts_new_civil`) never register as "hit" in gcov even though every other line of each
one's body does — including their own `error stop` lines, which only execute on the actual abort
path a dedicated error scenario triggers, proving the procedure genuinely runs end to end. No
common dummy-argument shape distinguishes them from this same file's other, normally-attributed
`impure elemental` procedure headers (e.g. `ts_set_civil`, same "optional argument" shape, is
attributed fine) — root cause not identified, just confirmed to be a line-attribution artifact via
the fully-covered body. Marked `GCOVR_EXCL_LINE` with an explanatory comment at each site, same
category as `end module`/`end submodule` above. If a *new* elemental procedure header in this
file (or a future sibling module) shows the same "0% but body covered" pattern, treat it the same
way — confirm the body is fully covered first (that's the only way to tell it apart from a
genuine gap), then exclude with a comment rather than spending more time chasing it.

### Regression tests for "sized/typed from the first element" bugs

This bug class specifically affects **character vectors/arrays**: a fixed per-element length
gets derived from the *first* element's own length instead of the true maximum, silently
truncating every later, longer element. E.g. for a string vector `['a', 'bc', 'cd']`, if the
length is (incorrectly) derived from the first element `'a'` (length 1), the second and third
elements arrive truncated to `'b'` and `'c'`.

When writing a regression test for this bug class (e.g. the string-vector-column tests in
`test/test_reading.f90` — `test_read_string_vector_short_first`), construct the fixture so
the first element is deliberately the extreme/shortest case and a later element is longer —
i.e. whenever a test needs to provide a string array, deliberately make the *first* element the
shortest one, to actively try to trigger this bug rather than merely avoid it by accident.
A fixture where the first element happens to be the longest (or same-length) can pass even
if the underlying bug is still present.

### A Fortran-side debug hook has to be PUBLIC, so prefer a C++ one

Every `parquet_debug_*` hook that forces library state for a test is a C++ `extern "C"` function,
and a test reaches it by declaring its own local `bind(C)` interface (never via
`src/parquet_bindings.f90`) — which is what keeps debug entry points out of the library's own
interface entirely. **A hook over state that lives on the Fortran side has no such escape hatch.**
`parquet_table_cache`'s components are private to `parquet_tables`, so anything forcing them must be
a public procedure in that module, visible to every `use parquet`.

There are two groups of them, both accepted deliberately rather than by default.
`parquet_debug_table_set_inflight` (`parquet_tables_parallel.f90`, declared in
`tools/generate_parquet_tables.py`'s template) is the first: it forces the append/read in-flight
counters so that the two concurrency aborts can be provoked from **one thread**, deterministically. Without it those aborts need two
threads to overlap on demand, and a timing-dependent test is worse than no test — it passes on a
quiet machine, fails on a busy one, and gets disabled. See `feature_risks.md` Risk-6.

**The second group is `parquet_strings`' four** (`parquet_debug_set_string_min_bytes`,
`parquet_debug_set_string_max_auto_threads`, `parquet_debug_string_row_ranges`,
`parquet_debug_string_bulk_threads`), and they are what shows the rule below is a *preference* rather
than a possibility: that module reaches no `bind(C)` surface at all by design, so the C++ route is
not available to it at any price short of ending its independence. **Two of the four exist because a
threshold no test-sized fixture can reach is a threshold no test exercises** — a 256 KiB payload floor
and a 64-thread ceiling both sit far above anything a unit test builds, so without an override every
test would silently take the path the constant was written to avoid (`feature_risks.md` Risk-49).
Expect a new tuning constant to need one.

Rules for a future one:

- **Reach for the C++ side first.** If the state can be forced from `parquet_wrapper.cpp`, do it
  there and keep the hook invisible to Fortran users. Only when the state is Fortran-side and behind
  private components does a public procedure become the only option.
- **A public debug hook is excluded from README.md's API overview**, carries a doc-comment saying it
  is test-only and why it has to be public, and is called by no library code. Follow
  `parquet_debug_table_set_inflight`'s shape rather than inventing a second convention.
- **Do not put a hook in the hot path to avoid making it public.** Having
  `table_check_no_append` consult a C++ flag would work and would keep the hook private — and would
  add a `bind(C)` call to the choke point every value accessor passes through. That trade was
  considered and rejected; see Risk-6's "the read path must stay free of atomics".
- **Every scenario a hook enables still needs its negative control**: make the guarded call once
  with the hook clear before setting it, or the scenario passes just as happily against a guard that
  fires unconditionally.

### Guarding a hard Arrow int32-only ceiling

Some Arrow/Parquet C++ APIs are hard-capped to a plain `int32_t`, with no int64/"large" fallback
at all — found three times so far: `arrow::FixedSizeListBuilder`/`fixed_size_list()`'s `list_size`
(a vector column's per-row width, `col_size`), `arrow::Schema::num_fields()`/`GetFieldIndex()` (a
table's column count), and Parquet's own repetition/definition-level generation for list-typed
columns (`level_conversion.cc`), which walks every flattened element of a row group with a plain
`int32_t` counter (see apache/arrow#33188 / ARROW-17983, still open/unfixed upstream). Unlike the
other two, this last one is scoped to one **row group**, not a column's total element count
(`nrows * col_size`) — and `close_parquet_writer`'s row-group auto-sizing already keeps every row
group under it by shrinking the row-group size, however large `nrows` gets, so a large total is
never actually a problem (verified empirically with a multi-billion-element vector column). Only
an *explicit* `chunk_size=` (`parquet_open_writer`/`parquet_set_writer_options`) that itself
conflicts with a column's `col_size` still aborts, since that's a caller-forced value auto-sizing
can't silently override — see `check_chunk_size_fits_limit_for_col_size`/
`check_explicit_chunk_size_fits_arrow_limit`/`check_chunk_size_fits_metadata_limit` in
`parquet_wrapper.cpp`, and the README's Limitations section. This differs from row count
(`int64_t` throughout Arrow) or a string column's byte payload (which has an `arrow::large_utf8()`
fallback) — for those two (`col_size` and column count), there is no workaround, only a clean
failure instead of letting Arrow silently truncate/wrap internally. When a new int32-only ceiling
is found, guard it with the pattern already used for `col_size`/column-count above (see
`check_col_size_fits_arrow_limit`/`check_column_count_fits_arrow_limit` in `parquet_wrapper.cpp`)
— only reach for the row-group-scoped auto-sizing approach instead if the new ceiling is similarly
scoped per-row-group rather than per-column-total:

1. A `static constexpr int64_t kArrowInt32...Limit = 2147483647;` named for what it bounds.
2. A process-global `static int64_t g_debug_..._limit = -1;` test-only override plus a
   `parquet_debug_set_..._limit(int64_t n)` extern "C" function (`<=0` restores the real limit) —
   reachable only via a `bind(C)` interface declared locally inside the relevant
   `test/error_scenarios.f90` scenario, never `src/parquet_bindings.f90`. This lets the error
   scenario trigger the abort with a tiny fixture instead of actually building a multi-GB/
   multi-billion-element table.
3. A `check_..._fits_arrow_limit(...)` function comparing against `g_debug_..._limit > 0 ?
   g_debug_..._limit : kArrowInt32...Limit`, calling `report_fatal_error` (never a silent
   truncating cast) when exceeded — called at every place the value is about to flow into the
   truncating Arrow API, before the cast happens.
4. A matching `test/error_scenarios.f90` scenario (shrink the debug limit, trigger the abort with
   a tiny fixture) + `test/test_errors.f90` wrapper (`check_scenario_exit_status_and_stderr`,
   asserting the exact stderr message) + `tools/run_error_scenarios.sh` entry + a README
   Limitations bullet describing the ceiling and that it aborts cleanly rather than corrupting.

**A validity mask is expensive, and usually unnecessary — ask the footer before building one.**
Requesting `is_valid=` from `parquet_read_column` was measured at **+22% to +84%** on the read path,
and it is not one cost but four: an `int8` buffer allocated by `make_valid_buf`, a Fortran `LOGICAL`
mask (gfortran's default `LOGICAL` is **32 bits** — four times the buffer it is built from), a
conversion pass between them (`is_valid = valid_buf /= 0_c_int8_t`), and an O(n) `array->IsValid(i)`
scan in `check_or_report_nulls`. On the table path there was a fifth: a per-row `set_null` replay.

Parquet records a **null count per column chunk in the footer**, so whether any of that is needed can
be answered without reading a byte — that is `parquet_column_has_nulls` (public API) over
`column_has_nulls_from_footer` (C++). Every `mat_*`/`matchunk_*` asks first and omits `is_valid=`
entirely when the answer is no. Rules to preserve:

- **The uncertain answer must be `.true.`** ("might have nulls"). Statistics are optional in the
  format; claiming a column is clean when it is not makes `parquet_read_column` abort on the first
  Null. A dotted **struct path** is declined outright, because a struct leaf's validity is combined
  with every ancestor struct's by `unwrap_struct_path`, so the leaf chunk's own count does not
  describe the result.
- **A filter or sample being active does not invalidate the answer** — both only ever remove rows, so
  a column with no Nulls in the file has none in the result.
- **`matchunk_*` scopes the question to its own row group**, or a file with Nulls anywhere would force
  the mask onto every clean row group.
- **The `is_stats_set()` and `HasNullCount()` guards are mutually redundant and both load-bearing.**
  Removing either alone changes no test result; removing both segfaults on
  `test/fixtures/no_stats.parquet` (statistics-free by construction — the only fixture that reaches
  this path), because `statistics()` returns null there. Do not delete one as dead code.
- **Keep the two fallbacks** for when a mask genuinely is needed: `check_or_report_nulls` short-circuits
  to `memset` when `null_count() == 0`, and the per-row replay sits behind `if (.not. all(valid))`.

**A row transform is either a MASK or a PERMUTATION, and only the permutation restricts anything.**
`filter=`/`sample_fraction=` install a boolean mask; `sort_by=` installs an `Int64Array` permutation
(`sort_perm`). `apply_row_transform` applies them in that order — filter first, then sort within the
survivors — at the single choke point every whole-column decode goes through. The distinction is
load-bearing for every guard: a mask only ever *removes* rows, so row groups stay contiguous and
chunked reads, `parquet_get_chunk_size` and row/element mode all work under one (row-group-scoped,
against each group's surviving count). A permutation *reorders* rows, so sorted row 5 may come from
row group 47 — nothing row-group-scoped survives it. **Every "not while transformed" check must key
on `reader_has_sort_permutation` (C++) / `check_reader_no_sort` (Fortran), never on the mask**;
widening either to "any transform" silently re-bans everything filtering supports, and dropping them
silently returns physically ordered rows from a sorted reader. Row mode and element mode each route
their whole-column fallback through ONE decision point (`fetch_row_mode_array` and a branch inside
`stream_element_mode_row_groups`) rather than per-entry-point branches, so a new type family cannot
miss it.

**The sort engine is deliberately free of reader state** (`SortKeyData`, `sort_compare_key`,
`sort_build_permutation` in `parquet_wrapper.cpp`): its keys arrive as plain typed vectors, and only
`sort_bind_arrow_key` touches Arrow. That is what lets the same engine later serve `parquet_table`'s
in-memory sort (whose Arrow buffers are gone) and a possible public `parquet_sort` module over any
1-D array. Don't reach for reader state from anything under that banner. Two further rules:
its ordering **must keep reproducing `arrow::compute::SortIndices` exactly** (nulls/NaNs absolute,
never flipped by `descending`; ascending gives values → NaNs → nulls; ties hold file order), since
that is what makes a `pyarrow` cross-check agree row for row; and the **integer counting-sort fast
path is a second code path producing the same answer**, so it keeps its own tests plus the
`parquet_debug_set_disable_sort_counting_path` hook that forces the comparator path for comparison —
it is the one place in the engine where a wrong answer would be fast rather than slow.

**A new reader query that a sibling module needs has to be PUBLIC `parquet` API** — there is no
internal back door. `parquet_reader`'s components are `private`, so `parquet_tables` (and any future
sibling module) cannot reach `%handle` and therefore cannot call `parquet_bindings` directly on a
reader; it must go through a procedure in `parquet` that takes the `parquet_reader` object. That is why
`parquet_column_has_nulls`, `parquet_measure_list_width` and `parquet_column_width_needs_data` are all
public rather than internal plumbing, and why each needs the full public treatment (dual int32/int64
kinds for numeric arguments, `!>`/`!!` docs, a README API-overview entry). Budget for that when a new
one is needed; do not try to widen `parquet_reader`'s component access instead.

**Fill a `parquet_column` with `%adopt`, not `%init` + `%set_all`, when the source array is a
temporary.** `set_all` is a full array assignment into storage the column just allocated, so the
obvious "read into `tmp`, then store it" shape costs an extra full pass and, transiently, a second
live copy of the column. `%adopt` (generated for all 16 array kinds) hands the allocation over with
`move_alloc` and takes kind, width and row count from the array itself, so it replaces `init` rather
than following it. The two string kinds have no `adopt` — they own a `parquet_string_column`, not a
plain array. Together with the mask skip above this took `materialize_all` on an 8 GB, 16-column
null-free file from 5.67 s to 3.54 s.

**Beware benchmarking this on one toolchain only.** The report that prompted the work measured
`materialize_all` at 1.84x a raw read with **Intel `ifx` on a 100+ core, ~800 GB machine**; the same
comparison with gfortran on a 32 GB laptop showed the table path at *parity or faster*, because
releasing Arrow's buffers as it goes shrinks the working set enough to pay for the extra passes. The
per-row loops that dominate under `ifx` are nearly free under gfortran. When a performance claim about
this layer cannot be reproduced, suspect the compiler before suspecting the report — and record which
one was used (see feature_materialize.md).

**Measuring a column's width must never materialize it, and only ONE column type needs data at
all.** `parquet_get_col_size`/`parquet_get_column_total_elements` answer from the schema for every
type except a plain `LIST`/`LARGE_LIST` — a scalar column's width is 1 by construction and a
`FIXED_SIZE_LIST`'s is `list_size()`. Only a plain `LIST` genuinely needs the data, because Arrow
lets every row hold a different length, so whether one uniform width exists is a property of the
data (`needs_data_to_measure_col_size` is the single predicate encoding this). Two facts make this
narrower than it looks: any Arrow-based writer preserves `FIXED_SIZE_LIST` via `store_schema()`, so
the plain-`LIST` case only arises for a non-Arrow writer or genuinely ragged data; and
`STRUCT`/`MAP`
never reach the question at all (`collect_column_leaf_paths` expands a struct into per-leaf paths,
and a `MAP` fails `parquet_column_exists`'s `types=` probe first).

When it does need data, it is resolved in two tiers rather than by a whole-column read — keep both:

1. **A footer screen** (`list_width_candidate`): per row group, `num_values / num_rows`. A
   non-integral mean, or two row groups disagreeing, proves no uniform width exists, for free. A
   surviving answer is a **candidate, never a proof** — rows of length 3,1,3,1 average to exactly 2,
   and a null or empty list occupies exactly one leaf slot (both verified empirically, not assumed;
   see `test/fixtures/list_widths.parquet`'s `avg_ok` and `null_avg` columns).
2. **A row-group scan** (`list_width_verified`) only for a survivor, bailing at the first
   disagreement, so peak memory is one row group. It deliberately does **not** use
   `get_row_group_chunk_array` — that one records the row group as read
   (`parquet_reader_check_complete`)
   and runs qc, neither of which measuring a column should cause. Use
   `read_row_group_array_for_measuring` instead, or a similar side-effect-free read.

**`parquet_table` never pays for the scan on the read path**, and that is deliberate: `table_touch`
resolves a deferred column with the *unproven candidate* (`table_resolve_width(...,
proven=.false.)`)
because `get_uniform_list_values` already checks every row's length against the width it was given
and aborts on a mismatch — so the read that was going to happen anyway doubles as the proof, keeping
`%prefetch` to one pass. Only `%kind`/`%width` pass `proven=.true.`, since answering a metadata
query
with a guess would silently mis-type the column. Two consequences to preserve if this is ever
refactored: an empty column measures as **0**, not 1 (`parquet_get_col_size` has always reported 0
for
a zero-row list column — the Fortran wrapper must not clamp it, the table descriptor clamps with
`max(w, 1)` itself); and a *slice* measures over its own row groups, so a globally-ragged file can
present a uniform width within one slice and two tables over the same file can legitimately
disagree.

**A new non-touching query must be checked against this.** `%kind`/`%width` had to become
touch-triggering for deferred columns; `%unit`, `%residency` and `%is_supported` deliberately did
not
(`%residency` would always report `RES_FULL`; `%is_supported` comes from the element type alone,
since
`table_kind_from_type`'s `ok` never depends on `col_size`). Anything reading `declared_kind` or
`width` without going through `table_resolve` or `table_resolve_width` will read `PK_NONE`/0 for a
deferred column.

**Read side of the `nrows * col_size` ceiling: row-group-scoped, not a ceiling at all.** Unlike
the write side above, `parquet_get_col_size`/`parquet_get_column_total_elements`/
`parquet_read_array_row_mode`/`parquet_read_array_element_mode` on the *read* path must **not**
materialize the *whole* column via `get_single_chunk_array`'s `ReadColumn` (Arrow's whole-file,
all-row-groups-at-once convenience API) just to answer a size query, fetch one row, or fetch one
element position across all rows: doing so trips Arrow's own internal int32 list-index/offset
limit once `nrows * col_size` crosses int32, even though the column was written perfectly safely
(every row group under the limit, per the write-side guard above). So all four are genuinely
row-group-scoped rather than guarded — keep them that way, don't revert to a whole-column read:
`parquet_get_col_size`/`parquet_get_column_total_elements` read `col_size`
straight off the schema's `FixedSizeListType::list_size()` (no data read at all) for a
FIXED_SIZE_LIST column — **but only for that case: a plain `LIST`/`LARGE_LIST` column, which this
library never writes but another tool can, has no schema-level width, so both fall back to
`get_single_chunk_array` and read the whole column to measure it.** Any code path that asks for
`col_size` on every column of an arbitrary file (`parquet_table`'s open-time classification is the
existing example) is therefore not automatically metadata-only, and should release afterwards
(`parquet_release_column`, a no-op when nothing was decoded) rather than assume nothing was read.
`parquet_read_array_row_mode` resolves which row group a given
`row_index` falls in (`resolve_row_group_for_row`, walking each row group's `num_rows()` from the
file footer) and reads only that one row group (`get_row_group_chunk_array`, the same helper the
`_column_chunk` family already used) rather than the whole column.
`parquet_read_array_element_mode` is different in kind from the other three: it inherently needs
every row's value at the same fixed column position, i.e. data from *every* row group — it can't
skip all but one the way row_mode does. So it streams row group by row group rather than reading
only one: `stream_element_mode_row_groups` walks every row group, reads each
one's own chunk via `get_row_group_chunk_array`, extracts just that row group's rows' values at
the fixed offset, and writes them into the correct slice of the caller's already-allocated
`nrows`-length output arrays — so no single Arrow call ever has to flatten more than one row
group's worth of elements, even though the final output still spans the whole file.
`resolve_element_mode_col_size` mirrors `parquet_get_col_size`'s own schema-only col_size lookup,
so element mode doesn't need a whole-column read just to validate `col_index`/compute the stride
offset either. **An active row filter/sample does not change any of this.** `row_index`/
`elem_index` then address the filtered result rather than a physical file row, and both are
resolved against each row group's *surviving* count instead of the footer's physical one —
`row_group_effective_rows`, which `resolve_row_group_for_row` and
`stream_element_mode_row_groups` both walk, so one function covers the masked and unmasked cases
and neither mode has a separate filtered branch any more. This works because
`get_row_group_chunk_array` hands back a chunk with that row group's own mask segment already
applied, so a local index within the returned chunk *is* a rank among survivors. A row group with
no survivors contributes 0 and is stepped over, exactly as a physically empty one already was.
The one remaining whole-column read in this area is `list_width_verified`'s masked branch, for a
plain `LIST`/`LARGE_LIST` column only (a width measured per row group under a mask would answer
about rows the caller filtered away). Regression-tested via `test/error_scenarios.f90`'s
`scenario_col_size_and_row_mode_avoid_whole_column_read` (a process-global
`g_debug_force_whole_column_read_error` hook forces `get_single_chunk_array` to abort the instant
it would actually read a whole column, on a tiny fixture — the scenario finishing without
aborting proves none of the four calls took that path), its filtered counterpart
`scenario_filter_row_element_mode_no_whole_column_read` (same hook, armed after a filtered open,
across all four entry-point families: the int32 template, the hand-written logical and string
pair, and the temporal template), plus the shared negative control
`scenario_whole_column_read_forced_error_control` (proves the hook itself actually fires).
