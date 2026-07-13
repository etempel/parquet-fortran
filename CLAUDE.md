# Instructions for Claude

## New features require tests and docs

Whenever asked to implement a new feature in this repository, always:

1. Add unit test coverage for it (in the relevant `test/*.f90` suite; add abort/error-path
   coverage via `test/error_scenarios.f90` + `test/test_errors.f90` +
   `tools/run_error_scenarios.sh` if the feature has failure modes that `error stop`).
2. Update documentation — see [Documentation structure](#documentation-structure) for what
   goes where. In brief: user-facing API/behavior/how-to (plus a per-procedure entry in the
   "parquet module API" reference) go in MANUAL.md; touch README.md only if the landing-page
   story changes (a new entry in its compact "API overview" index, a new limitation, a setup
   change); update CONTRIBUTING.md if it affects contributor workflow.

Do this without being asked separately each time — it applies by default to any
"implement/add feature" request in this repo, not just when explicitly reminded.

**CHANGELOG is paused until release.** The project is pre-release; the changelog will only be
maintained from the first public release (1.0) onward — do **not** add `CHANGELOG.md`
`[Unreleased]` entries in the meantime.

## Checking documentation links

After editing headings or `#anchor` links in README.md/MANUAL.md/CONTRIBUTING.md/CHANGELOG.md, run
`tools/check_doc_anchors.py` to verify every in-page and cross-file anchor link still
resolves against GitHub's actual heading-slug rules. It exits nonzero and lists any broken
link.

## Documentation structure

User- and contributor-facing docs are split across three files — keep new content in the right one:

- **README.md** — the lean *landing page*: what the library is, features, one quick example,
  install / prerequisites / environment variables, "important behavior", a compact **API
  overview** index, limitations, and license/contributing pointers. Keep it short — do **not**
  let it grow back into a manual; deep-dive and reference material goes in MANUAL.md.
- **MANUAL.md** — the full *user manual*: reading, writing, the MAML metadata format, worked
  examples, error handling, thread safety, supported data types, performance, the complete
  per-procedure **parquet module API** reference, and troubleshooting.
- **CONTRIBUTING.md** — *contributor-facing*: building/testing this repo, the error-path test
  harness, fixtures, OpenMP testing, MAML regeneration, C++ error conventions, the project
  Conventions section, and "features considered but not implemented".

Working rules:

- A new public procedure gets its full entry in **MANUAL.md**'s "parquet module API" section
  **and** its name in **README.md**'s "API overview" index; keep the two in sync on any
  rename/removal.
- Every section heading must appear in that file's own **Contents** ToC.
- Moving content between README.md and MANUAL.md turns in-page `#anchor` links into cross-file
  `MANUAL.md#…` / `README.md#…` links — repoint them, and fix now-stale relative wording
  ("above", "below", "this README"). Re-run `tools/check_doc_anchors.py` afterward (it checks
  cross-file links too).
- **Diagrams: plain text, not Mermaid.** This project's GitLab does not reliably render Mermaid
  diagrams, so draw flows as plain-text/ASCII inside a normal code fence (renders identically
  everywhere) — see the MAML→header flow in MANUAL.md's "The MAML metadata format".
- **Badges: static only for now** (license / language / fpm). A GitLab CI pipeline now exists
  (`.gitlab-ci.yml`, running `fpm test` with coverage), but still defer dynamic build/coverage
  badges until the project is public with a stable URL to point them at.

## FORD-generated docs + fpm package-index publication — remaining: doc-comment coverage

Layout, migration, and CI are complete and live. The only remaining active task is the
doc-comment (`!>`/`!!`) coverage audit in `src/*.f90` — don't start it unless asked.

**Done:**
- FORD config lives in `fpm.toml`'s `[extra.ford]` (not a standalone `ford.md`); `docs.md` is
  `{!README.md!}`; `doc/pages/*.md` holds the migrated user guide (one file per old MANUAL.md
  section, `doc/pages/index.md` as the landing page, granularity/cross-links already settled);
  `doc/user.css` fixes a duplicate front-page `<h1>` (FORD's own auto-title plus README's own
  `# parquet-fortran` heading rendering twice).
- Two working CI paths: `.github/workflows/docs.yml` (GitHub Actions → GitHub Pages, ready but
  waiting on the GitHub mirror — see below) and `.gitlab-ci.yml`'s `readthedocs` job (GitLab CI
  → gitlab.4most.eu's readthedocs-style docserver at `escience.aip.de/readthedocs/webhooks/upload`
  using the instance-provided `$DOCSERVER_TOKEN`; **confirmed working end-to-end** by the user).
  GitLab Pages is not available on gitlab.4most.eu and won't be — this docserver replaces it.
- README.md has zero remaining references to MANUAL.md (rewritten to link into `doc/pages/`
  instead); the "parquet module API" category links there were de-linked to plain text, since
  that whole section was never migrated (superseded by FORD generation once coverage exists).
- Target for publication is the fortran-lang.org **package index** (PR to
  `fortran-lang/fortran-lang.org` per `PACKAGES.md`), not the separate `fpm publish` registry
  (still in playground/testing status — skipped deliberately). Publishing repo is a GitHub
  mirror at `github.com/etempel/parquet-fortran`, synced manually by the user (no automation).

**Remaining:**
1. **Doc-comment coverage (`src/*.f90`) — the only step needing source changes.** Audit which
   public procedures/types/modules still lack a `!>`/`!!` block (existing ones already use
   correct predoc `!>` placement, no marker migration needed); FORD only documents what's
   annotated, and already resolves this project's `submodule` blocks back to their
   parent-module interfaces automatically, so submodule files don't need their own comments.
2. Once (1) is done, validate with `ford docs.md` (FORD is installed locally at
   `~/.local/bin/ford`, separate from the `pyastro` venv's `python3`).
3. Once (1)+(2) look good and the FORD site is verified live on the GitHub mirror: **ask for
   explicit confirmation before deleting MANUAL.md**, then rewrite this file's "Documentation
   structure" section (still describes the old README/MANUAL/CONTRIBUTING three-way split) and
   fix any remaining cross-references to it.
4. Optional polish, independent of 1–3: a `[[entity_name]]` auto-link pass over
   `doc/pages/*.md`'s inline procedure-name mentions (plain backtick spans today); a
   favicon/logo for `doc/media/` (exists, currently empty besides a `.gitkeep`).
5. Outside this repo, user's own manual steps: create/push to the GitHub mirror, then enable
   GitHub Pages there (Settings → Pages → source "GitHub Actions").
6. Final step, only after everything above: the `fortran-lang/fortran-lang.org` PACKAGES.md PR.

**Gotchas if this config is touched again:**
- `md_extensions = ["markdown.extensions.toc"]` in `[extra.ford]` is **load-bearing** — without
  it, no heading gets an `id`, silently breaking every anchor link site-wide with no error.
- `preprocessor = "cpp -traditional-cpp -E"` is required, not optional — `src/parquet.f90`'s
  version-string logic uses real cpp macros, not just comment-style directives.
- `doc/pages/*.md` files deliberately have **no top-level heading in their body** (title comes
  from frontmatter only) — adding one back would reintroduce the duplicate-heading bug
  `doc/user.css` fixes on the front page.
- `tools/generate_parquet_maml.sh`'s `end module` template line must keep emitting
  `! GCOVR_EXCL_LINE` (added deliberately, not FORD/gcovr's default) — a careless edit there
  will silently drop it from every regenerated file (`src/parquet_maml_base.f90` and any
  downstream project's generated `parquet_maml` module).

## MAML fixture directory: `schemas/` (renamed from `docs/`)

`.maml` example/fixture files live in `schemas/`, not `docs/` (renamed to avoid confusion with
`doc/`, FORD's `page_dir`/`media_dir`). `tools/generate_parquet_maml.sh` accepts
`--dir=<name>`/`--dir <name>` (default `schemas`) so downstream projects embedding their own
MAML schemas aren't forced to match this project's convention — see MANUAL.md's "Embedding your
own MAML schemas" / `doc/pages/embedding-maml-schemas.md` for the user-facing how-to.

## Report before implementing on analysis/audit requests

When asked to analyze, audit, or review something (naming conventions, documentation
duplication/coverage, test coverage, etc.), report findings and a proposed plan first and
wait for confirmation before editing any files. Only proceed straight to editing when
explicitly asked to implement/fix/add something directly.

## Stale `fpm` build cache

If `fpm test` behaves unexpectedly after source changes (e.g. a test target seems to run
old code), try `fpm clean --all` to force a clean rebuild before spending time debugging —
fpm's build cache can serve a stale binary.

## Don't run the GitLab CI pipeline yourself

The user runs `.gitlab-ci.yml` on their own GitLab server — don't attempt to execute it
(e.g. via `gitlab-runner`, docker, or otherwise) as part of verifying changes. Verify
locally instead (`fpm build`/`fpm test` with the same `FPM_FFLAGS`/`FPM_CXXFLAGS`/
`FPM_LDFLAGS` the CI job sets, minus anything CI-environment-specific like the apt installs).

## Build and compiler notes

- **132-column line limit is enforced — do not reintroduce `-ffree-line-length-none`.** As of
  2026-07-12, `src/*.f90` and `test/*.f90` are held strictly within the standard 132-column
  free-form limit, including comments (both whole-line and trailing end-of-line) — a comment
  pushing a line past 132 columns is a violation just like code would be. `.gitlab-ci.yml`'s
  `FPM_FFLAGS` no longer passes `-ffree-line-length-none`, so a line over 132 columns now fails
  CI on older gfortran (and is a style violation regardless of compiler). When a line runs
  long, wrap it with `&` continuations (code/strings) or split it across multiple `!`-prefixed
  comment lines — don't reach for the compiler flag again, and don't add per-file/per-line
  suppressions.
- **Minimum gfortran is 13; don't work around compiler bugs in source.** gfortran ≤ 11
  miscompiles the optional allocatable-`character` argument in `schema%add_col_qc` /
  `schema%get_col_qc` (corrupted column name → a spurious "column not found" abort at
  runtime — see README.md's Prerequisites). That's the reason for the version floor; don't
  refactor otherwise-correct source to accommodate an old compiler.

## Renames/refactors: only apply low-blast-radius changes

When renaming or refactoring existing (non-new) code for consistency, only apply the
renames/changes that are low blast-radius (few call sites, no public API/doc impact).
For anything with wider knock-on effects (public API, many call sites, cross-file
conventions), report it as a proposed change and wait for confirmation instead of applying
it directly.

## Naming conventions

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

When in doubt, grep for an existing analogous name before inventing a new convention.

## Running a single test suite/test

Use `fpm test run_tester -- <suite>` to run just one test-drive suite (e.g. `fpm test
run_tester -- reading`), or `fpm test run_tester -- <suite> "<test name>"` to run a single
named test within it. Prefer this over a full `fpm test` while iterating — the full suite
(including OpenMP/error-scenario subprocess tests) takes much longer than the one suite
relevant to a given change.

## Measuring test coverage

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
and interface-only / `extern "C"` files (`parquet_bindings.f90`, and `parquet_wrapper.cpp`,
which the local gcov toolchain doesn't instrument at all).

## Regression tests for "sized/typed from the first element" bugs

When writing a regression test for a bug where a value/size/length was incorrectly derived
from the first element of an array/vector column (e.g. the string-vector-column tests in
`test/test_reading.f90` — `test_read_string_vector_short_first`), construct the fixture so
the first element is deliberately the extreme/shortest case and a later element is longer.
A fixture where the first element happens to be the longest (or same-length) can pass even
if the underlying bug is still present.

## Public numeric arguments: provide both int32 and int64 kinds

When adding a public procedure argument that holds a row count / size / index (any integer a
caller might naturally declare as a plain `INTEGER`), make it generic over **both**
`integer(int32)` and `integer(int64)`, following the existing `parquet_get_nrows_int32`/
`parquet_get_nrows_int64` overload pattern. An `integer(int64)`-only dummy forces callers
with a default-kind `INTEGER` variable into a `Type mismatch ... passed INTEGER(4) to
INTEGER(8)` compile error (this shipped once for `parquet_open_reader`'s `nrows=` before
being fixed).

Fortran constraint that shapes this: an *optional* dummy that differs only by kind cannot be
the sole disambiguator between specific procedures in a generic interface (a call omitting it
is ambiguous). So when such an argument is optional, carry the argument-absent case as its
own separate specific rather than an optional dummy — see `parquet_open_reader`'s split into
`parquet_open_reader_base` (no `nrows`) plus `parquet_open_reader_nrows_int32`/`_int64`
(required `nrows`), all under one generic interface.

## Error stop messages: include file/schema context

New `error stop` messages in the read/write/schema-building paths should append the relevant
file and, where applicable, schema/maml name using the existing helpers — `writer_context_suffix`
(`parquet_write.f90`), `reader_filename_suffix` (`parquet_read.f90`), `maml_name_suffix`
(`parquet_metadata.f90`) — rather than naming only the offending column/field, so a failure is
identifiable when several readers/writers/schemas are in play at once. These are only
meaningful once the reader/writer/schema knows its file/name (i.e. post-open), so the
guard-clause "…has not been opened" messages are exempt.
