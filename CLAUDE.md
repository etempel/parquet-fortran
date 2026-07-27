# Instructions for Claude

This file is forward-looking: it captures durable conventions, gotchas, and guardrails to help
maintain this library and develop new features going forward. It is not a changelog or session
log — do not add entries describing how or when a specific feature was implemented, what a past
session investigated, or a chronological record of development. Only add generalizable guidance
that will still be correct and actionable for a future task, independent of which session
produced it (git history/commit messages are the right place for "what happened when").

## Contents

This file is a reference, not a start-to-finish read — jump to the note you need. Keep this ToC
in sync when adding, removing, renaming, or reordering a heading (see "Documentation structure"'s
working rules).

- [Workflow & guardrails](#workflow--guardrails)
  - [Only modify files inside this repository](#only-modify-files-inside-this-repository)
  - [Report before implementing on analysis/audit requests](#report-before-implementing-on-analysisaudit-requests)
  - [Only apply low-blast-radius renames/refactors](#only-apply-low-blast-radius-renamesrefactors)
  - [`feature_*.md` planning documents](#feature_md-planning-documents)
  - [Don't run the GitLab CI pipeline yourself](#dont-run-the-gitlab-ci-pipeline-yourself)
  - [Don't commit or push on the main/default branch yourself](#dont-commit-or-push-on-the-maindefault-branch-yourself)
- [Documentation conventions](#documentation-conventions)
  - [New features require tests and docs](#new-features-require-tests-and-docs)
  - [Documentation structure](#documentation-structure)
  - [Checking documentation links](#checking-documentation-links)
  - [FORD doc-comment conventions](#ford-doc-comment-conventions)
  - [FORD config gotchas](#ford-config-gotchas)
- [Source code structure & conventions](#source-code-structure--conventions)
  - [One program unit per file; filename == unit name](#one-program-unit-per-file-filename--unit-name)
  - [Nested submodule tree](#nested-submodule-tree)
  - [Group interface bodies into commented `interface` blocks](#group-interface-bodies-into-commented-interface-blocks)
  - [A module procedure cannot implement its own submodule's spec-declared interface](#a-module-procedure-cannot-implement-its-own-submodules-spec-declared-interface)
  - [Naming conventions](#naming-conventions)
  - [Public numeric arguments: provide both int32 and int64 kinds](#public-numeric-arguments-provide-both-int32-and-int64-kinds)
  - [MAML fixture directory: `schemas/`](#maml-fixture-directory-schemas)
  - [Error stop messages: include file/schema context](#error-stop-messages-include-fileschema-context)
  - [Guard mutating public procedures against being called twice](#guard-mutating-public-procedures-against-being-called-twice)
  - [Implicit finalizers must never route through a path that can throw/abort](#implicit-finalizers-must-never-route-through-a-path-that-can-throwabort)
- [Element-domain modules (`parquet_strings`, `parquet_temporal`)](#element-domain-modules-parquet_strings-parquet_temporal)
  - [The `parquet_strings` module](#the-parquet_strings-module)
  - [The `parquet_temporal` module (date/time/timestamp)](#the-parquet_temporal-module-datetimetimestamp)
- [Build & compiler notes](#build--compiler-notes)
  - [Compiler & language gotchas](#compiler--language-gotchas)
  - [Stale `fpm` build cache](#stale-fpm-build-cache)
  - [Keeping `tools/prep_fpm_publish.sh` in sync](#keeping-toolsprep_fpm_publishsh-in-sync)
  - [Manual (never-`fpm test`) large-scale/benchmark tools](#manual-never-fpm-test-large-scalebenchmark-tools)
- [Testing & coverage](#testing--coverage)
  - [Running a single test suite/test](#running-a-single-test-suitetest)
  - [Measuring test coverage](#measuring-test-coverage)
  - [Fortran gcov attribution artifacts](#fortran-gcov-attribution-artifacts)
  - [`src/parquet_wrapper.cpp`: GCC vs Clang gcov attribution](#srcparquet_wrappercpp-gcc-vs-clang-gcov-attribution)
  - [Regression tests for "sized/typed from the first element" bugs](#regression-tests-for-sizedtyped-from-the-first-element-bugs)
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

**Whenever asked to write or update a `feature_*.md` file, write it to be fully self-explaining
without relying on the current session's conversation for context** — a future session opening
the file has no memory of this one. Concretely: don't reference "this conversation," "as discussed
above" (meaning the chat, not the document), tool-call artifacts (e.g. a clarifying-question
option that was offered but not visibly quoted), or any other detail that only makes sense to
someone who was present for the conversation that produced the file. Quote the user's own
decisions/wording directly in the document rather than alluding to them. Cross-references to
other files in the repo (source, other `feature_*.md` docs, `CLAUDE.md` sections) are fine, since
a future session can read those too.

### Don't run the GitLab CI pipeline yourself

The user runs `.gitlab-ci.yml` on their own GitLab server — don't attempt to execute it
(e.g. via `gitlab-runner`, docker, or otherwise) as part of verifying changes. Verify
locally instead (`fpm build`/`fpm test` with the same `FPM_FFLAGS`/`FPM_CXXFLAGS`/
`FPM_LDFLAGS` the CI job sets, minus anything CI-environment-specific like the apt installs).

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

**CHANGELOG is paused until release.** The project is pre-release; the changelog will only be
maintained from the first public release (1.0) onward — do **not** add `CHANGELOG.md`
`[Unreleased]` entries in the meantime.

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
instead — expect it to be noisy (on the order of 2,700 warnings as of this writing), dominated by
two categories that are *not* required by this project's conventions and can be ignored:
`Undocumented variable` for local variables (`i`, `idx`, `res`, ...), and
`Undocumented moduleprocedure` for the abbreviated `module procedure NAME ... end procedure NAME`
form (exempted by the bullet below). Everything else in that output is a real gap worth acting on.
The "before/after regression" check two bullets down needs `ford --warn docs.md`'s count for the
same reason — comparing two plain `ford docs.md` runs compares two counts that are both
structurally 0 (Graphviz-only) and cannot detect a coverage regression.

Keep new code to the same standard:

- Leading `!>` = predoc (documents what follows); trailing `!!` = postdoc (documents what
  precedes). **`!<` is not a FORD marker at all** (that's Doxygen).
- Every dummy argument/function result gets its own trailing `!!` tag wherever the argument list
  is actually written out — the spec in `parquet.f90`, and any submodule body that *restates*
  the full interface (`module subroutine name(args)` with the arguments redeclared, e.g.
  `parquet_maml_base_add_col_qc.f90`). The abbreviated `module procedure name ... end procedure
  name` form (no restated arguments) is exempt — nothing to tag, and the spec in `parquet.f90`
  is the canonical doc location for it.
- Every type-bound procedure binding (`procedure ::`, `generic ::`, `final ::` inside a type's
  `contains` block) needs its own short trailing `!!` description, separate from documenting the
  procedure it binds to (easy to forget since the bound procedure's own doc feels like it
  "covers" the binding), e.g.
  `procedure :: add => parquet_filter_add !! Appends one AND-combined rule clause.`.
  **Accepted exception:** the two `generic :: add_metadata => ...`/`generic :: add_metadata =>
  schema_add_metadata_...` bindings (`parquet.f90`'s `parquet_column_info`/`parquet_schema` type
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
  doc-comments FORD would have warned about anyway. A relocated private helper legitimately
  disappearing from its old module's FORD
  page (private procedures don't get individual page entries) is expected and not a regression by
  itself — cross-check with `grep 'public ::'` in `parquet.f90` before treating an "not found on
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
- **FORD 7.0.13 cannot resolve a `public ::` re-export chain** — `parquet.f90` re-exports 14 names
  from sibling modules (`parquet_date`/`parquet_time`/`parquet_timestamp` and the eight
  `parquet_unit_*`/`parquet_ns_*` constants from `parquet_temporal`; `parquet_string`/
  `parquet_string_column` from `parquet_strings`; `parquet_maml_file` from `parquet_maml_base`),
  and `ford --warn docs.md` reports all 14 as `Unknown entity ... with attribute 'public'`,
  silently dropping them from the generated `parquet` module page. **Confirmed not fixable from
  source**: neither adding a `!>` doc-comment directly on the `public ::` line, nor using an
  explicit `use ..., only: name1, name2` import list (already how these modules are imported), nor
  a bare unrestricted `use` with no `only:` at all, changes this — all three were tried against
  FORD 7.0.13 and the warning count stayed at 14 every time. The underlying symbols are not lost
  from the generated site — the three temporal types get their own `type/parquet_date.html`-style
  pages and the constants render on `module/parquet_temporal.html`/`module/parquet_strings.html`/
  `module/parquet_maml_base.html` — they just don't appear as belonging to `parquet` on that
  module's own generated page. No `doc/pages/*.md` or README text currently links a reader
  specifically to `module/parquet.html` expecting to find these symbols there (the guide's own
  pointers go to the site-wide `lists/procedures.html`/`lists/types.html`, where they do appear),
  so this is a cosmetic gap in the generated reference, not a broken link — left undocumented
  further and accepted as a FORD limitation. Re-test against a newer FORD release before
  attempting either source-side fix again.
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
  duplicated between `parquet.f90`'s spec and every submodule body. Applying this fix across all
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

### Nested submodule tree

`src/*.f90`'s `parquet`/`parquet_read`/`parquet_write`/`parquet_metadata` files form a nested
submodule tree (not flat siblings under `parquet`), split by data-type family for read/write and
by format for metadata:

```
parquet                         (module — public API + cross-subtree private-helper interfaces)
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

Reserved for future element-domain work (not yet implemented): `parquet_map`/`parquet_list`/
`parquet_struct` (independent modules, like `parquet_temporal`) plus their own
`parquet_read_*`/`parquet_write_*` type-family children — see CONTRIBUTING.md's "Features
considered but not implemented" for scope/status.

**Placement rule for a new read/write specific or shared helper:** type-generic code (used by
more than one of numeric/string/temporal) belongs in the parent (`parquet_read`/`parquet_write`)
as an ordinary contained procedure — descendants reach it by host association, no interface
needed. Type-specific code belongs in the matching child, also as an ordinary contained
procedure. A new *public* generic's specifics, and any type-bound binding target, must keep
their interface declared in `parquet.f90` itself regardless of family (see "A module procedure
cannot implement its own submodule's spec-declared interface" below for what breaks if you
relocate one incorrectly) — never assume a helper is safe to relocate purely from its call sites
without also checking those two disqualifiers, plus whether it's itself `public ::`-exported.

**A known gfortran 15.2.0 ICE to watch for when adding a new cross-subtree call into this
tree:** calling `parquet_parse_protected_cols` (declared in `parquet.f90`) directly from a
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

Declare the `module subroutine`/`function` interface bodies in `parquet.f90` (and in any
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
(e.g. relocating an interface from `parquet.f90` into `parquet_metadata`'s own spec, when the
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

- **Public module-level API** (anything in `src/parquet.f90`'s `public ::` list — functions,
  subroutines, types) always carries the `parquet_` prefix, e.g. `parquet_get_metadata`,
  `parquet_open_reader`, `parquet_schema`.
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
  don't rename these to match Fortran-side conventions.

- **A new module holding several related element/handle types** (as opposed to one module per
  type) should be named after the *domain* those types belong to, not any single type inside
  it — e.g. `parquet_temporal` for `parquet_date`/`parquet_time`/`parquet_timestamp`. See "The
  `parquet_temporal` module" below for the reasoning and the sibling modules (`parquet_map`,
  `parquet_list`) this leaves room for.

When in doubt, grep for an existing analogous name before inventing a new convention.

### Public numeric arguments: provide both int32 and int64 kinds

When adding a public procedure argument that holds a row count / size / index (any integer a
caller might naturally declare as a plain `INTEGER`), make it generic over **both**
`integer(int32)` and `integer(int64)`, following the existing `parquet_get_nrows_int32`/
`parquet_get_nrows_int64` overload pattern. An `integer(int64)`-only dummy forces callers
with a default-kind `INTEGER` variable into a `Type mismatch ... passed INTEGER(4) to
INTEGER(8)` compile error.

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

### MAML fixture directory: `schemas/`

`.maml` example/fixture files live in `schemas/`. `tools/generate_parquet_maml.sh` accepts
`--dir=<name>`/`--dir <name>` (default `schemas`) so downstream projects embedding their own
MAML schemas aren't forced to match this project's convention — see
`doc/pages/embedding-maml-schemas.md` for the user-facing how-to.

### Error stop messages: include file/schema context

New `error stop` messages in the read/write/schema-building paths should append the relevant
file and, where applicable, schema/maml name using the existing helpers — `writer_context_suffix`
(`parquet_write.f90`), `reader_filename_suffix` (`parquet_read.f90`), `maml_name_suffix`
(`parquet_metadata.f90`) — rather than naming only the offending column/field, so a failure is
identifiable when several readers/writers/schemas are in play at once. These are only
meaningful once the reader/writer/schema knows its file/name (i.e. post-open), so the
guard-clause "…has not been opened" messages are exempt.

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

## Element-domain modules (`parquet_strings`, `parquet_temporal`)

### The `parquet_strings` module

`src/parquet_strings.f90` is an independent module (`use parquet_strings`; depends only on
`iso_fortran_env`/`iso_c_binding`) providing `parquet_string_column` (Arrow-LargeUtf8-style
offsets+data+bit-packed-validity string storage) and `parquet_string` (a non-owning handle to
one element). User guide: `doc/pages/string-columns.md`.

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
- **`-128_int8` trips gfortran's range check** (it parses `128` then negates). Build the high bit
  with `ibset(0_int8, 7)` in constant expressions. Also: an array-constructor implied-do index
  (`[(f(b), b=0,7)]`) has no implicit type under `implicit none` — list the elements explicitly.
- **`transfer(source, mold, size)` into a longer target leaves the trailing bytes undefined**, not
  blank-padded. To place a short string into a longer fixed-length slot, assign normally (which
  blank-pads); reserve `transfer` for exact-size byte moves.
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

### Stale `fpm` build cache

If `fpm test` behaves unexpectedly after source changes (e.g. a test target seems to run old
code), try `fpm clean --skip` to force a clean rebuild before spending time debugging — fpm's
build cache can serve a stale binary. `--skip` avoids rebuilding external (non-project)
dependencies, which are never the source of this problem, so it's faster than `--all` here.
Building with several different `FPM_FFLAGS` creates multiple `build/gfortran_<hash>/` dirs, and
`test/test_errors.f90`'s `error_scenarios_bin` does `find build -name error_scenarios | head -1`,
which can pick a *stale* binary from an old hash dir — symptom: tests pass when run scoped but
fail under a full `fpm test`. `tools/coverage.sh` runs `fpm clean` up front to avoid this; for a
plain `fpm test`, `fpm clean --skip` fixes it.

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

## Testing & coverage

### Running a single test suite/test

Use `fpm test run_tester -- <suite>` to run just one test-drive suite (e.g. `fpm test
run_tester -- reading`), or `fpm test run_tester -- <suite> "<test name>"` to run a single
named test within it. Prefer this over a full `fpm test` while iterating — the full suite
(including OpenMP/error-scenario subprocess tests) takes much longer than the one suite
relevant to a given change.

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
FIXED_SIZE_LIST column, and `parquet_read_array_row_mode` resolves which row group a given
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
offset either. The one case that falls back to the whole-column path (for all four) is
an active row filter (`parquet_open_reader(..., filter=)`/`parquet_reader_set_filter`): a filter
mask has no row-group structure of its own (see `get_row_group_chunk_array`'s own comment), so
`row_index`/the per-row iteration there means "index into the filtered result", not a physical
file row. Regression-tested via `test/error_scenarios.f90`'s
`scenario_col_size_and_row_mode_avoid_whole_column_read` (a process-global
`g_debug_force_whole_column_read_error` hook forces `get_single_chunk_array` to abort the instant
it would actually read a whole column, on a tiny fixture — the scenario finishing without
aborting proves none of the four calls took that path) plus its negative control
`scenario_whole_column_read_forced_error_control` (proves the hook itself actually fires).
