# Instructions for Claude

## New features require tests and docs

Whenever asked to implement a new feature in this repository, always:

1. Add unit test coverage for it (in the relevant `test/*.f90` suite; add abort/error-path
   coverage via `test/error_scenarios.f90` + `test/test_errors.f90` +
   `tools/run_error_scenarios.sh` if the feature has failure modes that `error stop`).
2. Update documentation — see [Documentation structure](#documentation-structure) for what
   goes where. In brief: every new public procedure/type gets its own `!>`/`!!` doc-comment
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

## Checking documentation links

After editing headings or `#anchor` links in README.md/CONTRIBUTING.md/CHANGELOG.md/CLAUDE.md/docs.md,
run `tools/check_doc_anchors.py` to verify every in-page and cross-file anchor link still
resolves against GitHub's actual heading-slug rules. It exits nonzero and lists any broken
link. Note: it does not currently scan `doc/pages/*.md` — check links into/within those pages
by hand (or by cross-referencing an existing internal link to the same heading, e.g.
`doc/pages/reading.md`'s own `#row-filtering-with-parquet_filter` anchor) until that's added.

## Documentation structure

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
  the "FORD-generated docs" section below); FORD turns these into the browsable
  modules/procedures/types reference automatically. This is the *only* place the per-procedure
  reference lives — there is no hand-written equivalent to keep in sync.
- **CONTRIBUTING.md** — *contributor-facing*: building/testing this repo, the error-path test
  harness, fixtures, OpenMP testing, MAML regeneration, C++ error conventions, the project
  Conventions section, and "features considered but not implemented".

Working rules:

- A new public procedure gets a `!>`/`!!` doc-comment (see "FORD-generated docs" below) **and**
  its name in **README.md**'s "API overview" index; keep the two in sync on any rename/removal.
- Every section heading must appear in that file's own **Contents** ToC (README.md/CONTRIBUTING.md);
  `doc/pages/*.md` pages don't need one — FORD generates in-page navigation from headings itself.
- Moving content between README.md and a `doc/pages/*.md` page turns in-page `#anchor` links
  into cross-file `doc/pages/<page>.md#…` / `README.md#…` links — repoint them, and fix
  now-stale relative wording ("above", "below", "this README"). Re-run `tools/check_doc_anchors.py`
  afterward for the files it covers (see "Checking documentation links" above; it doesn't scan
  `doc/pages/*.md` itself yet).
- **Diagrams: plain text, not Mermaid.** This project's GitLab does not reliably render Mermaid
  diagrams, so draw flows as plain-text/ASCII inside a normal code fence (renders identically
  everywhere) — see the MAML→header flow in `doc/pages/maml-format.md`'s "The MAML metadata format".
- **Badges: static only for now** (license / language / fpm). A GitLab CI pipeline now exists
  (`.gitlab-ci.yml`, running `fpm test` with coverage), but still defer dynamic build/coverage
  badges until the project is public with a stable URL to point them at.

## FORD-generated docs + fpm package-index publication — remaining: publication steps only

Layout, migration, CI, doc-comment coverage, and MANUAL.md retirement are all complete and
live. What's left is outside-this-repo publication steps (see "Remaining" below) — nothing
here needs source changes anymore.

**Done:**
- FORD config lives in `fpm.toml`'s `[extra.ford]` (not a standalone `ford.md`); `docs.md` is
  `{!README.md!}`; `doc/pages/*.md` holds the full user guide (`doc/pages/index.md` as its
  landing page, granularity/cross-links settled); `doc/user.css` fixes a duplicate front-page
  `<h1>` (FORD's own auto-title plus README's own `# parquet-fortran` heading rendering twice).
- Two working CI paths: `.github/workflows/docs.yml` (GitHub Actions → GitHub Pages, ready but
  waiting on the GitHub mirror — see below) and `.gitlab-ci.yml`'s `readthedocs` job (GitLab CI
  → gitlab.4most.eu's readthedocs-style docserver at `escience.aip.de/readthedocs/webhooks/upload`
  using the instance-provided `$DOCSERVER_TOKEN`; **confirmed working end-to-end** by the user).
  GitLab Pages is not available on gitlab.4most.eu and won't be — this docserver replaces it.
- **Doc-comment coverage is complete across all of `src/*.f90`** (every module, type, public and
  private procedure, dummy argument/function result, and type-bound procedure binding carries a
  `!>`/`!!` doc-comment) and **validated end-to-end with `ford docs.md`** (clean run besides the
  expected "Graphviz not installed" environment warning — install `graphviz` locally to also get
  call graphs, not required). Two durable conventions this pass established, still apply to any
  new code:
  - **Every dummy argument and function result gets its own trailing `!!` tag**, not just a
    procedure-level `!>` prose block above the signature (e.g.
    `character(len=*), intent(in) :: key !! metadata key to look up.`). This applies wherever
    the argument list is actually written out: the canonical `module subroutine`/`module
    function` spec in `parquet.f90`, and also any submodule body that *restates* the full
    interface (`module subroutine name(args)` with the arguments redeclared, the pattern
    `parquet_maml_base_add_col_qc.f90` and `parquet_metadata_sections.f90`'s
    `parquet_validate_maml_sections` use) — only the abbreviated `module procedure name ...
    end procedure name` form (no restated arguments) is exempt, since there's nothing to tag.
    **`!<` is not a FORD marker at all** (that's Doxygen) — the trailing/postdoc marker is
    always `!!`. Also watch for a doc-comment's very first word being a bare `word:` (e.g. a
    line starting "qc: min: ...") — FORD parses that as an attempted metadata key, either
    warning (unrecognized key) or, worse, *silently* consuming the line if it happens to match
    a real FORD key (`date:`, `author:`, `version:`, `summary:`, ...); reword so the line
    doesn't open with a bare `word:`.
  - **Every type-bound procedure binding** (`procedure ::`, `generic ::`, `final ::` inside a
    type's `contains` block) needs its own short trailing `!!` description too — this is
    separate from documenting the procedure it binds to, and easy to forget since the bound
    procedure's own doc feels like it "covers" the binding. One line each is enough, e.g.
    `procedure :: add => parquet_filter_add !! Appends one AND-combined rule clause.`.
- `MANUAL.md` has been deleted; every cross-reference to it (README.md, CONTRIBUTING.md, this
  file) now points into the equivalent `doc/pages/*.md` page or the FORD-generated reference.
- Target for publication is the fortran-lang.org **package index** (PR to
  `fortran-lang/fortran-lang.org` per `PACKAGES.md`), not the separate `fpm publish` registry
  (still in playground/testing status — skipped deliberately). Publishing repo is a GitHub
  mirror at `github.com/etempel/parquet-fortran`, synced manually by the user (no automation).

**Remaining (all outside this repo / user's own manual steps):**
1. Optional polish, no urgency: a `[[entity_name]]` auto-link pass over `doc/pages/*.md`'s
   inline procedure-name mentions (plain backtick spans today); a favicon/logo for `doc/media/`
   (exists, currently empty besides a `.gitkeep`); extending `tools/check_doc_anchors.py` to
   also scan `doc/pages/*.md` (not covered today — see "Checking documentation links" above).
2. Create/push to the GitHub mirror, then enable GitHub Pages there (Settings → Pages → source
   "GitHub Actions").
3. Final step, only after (2): the `fortran-lang/fortran-lang.org` PACKAGES.md PR.

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
