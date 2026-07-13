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

## FORD-generated docs + fpm package-index publication (layout done, doc-comments not started)

Plan and implementation status for migrating API docs to [FORD](https://forddocs.readthedocs.io/)
and publishing this project to the fortran-lang package index. Researched and decided
2026-07-13; layout implemented same day. **Remaining work (doc-comment coverage, enabling
GitHub Pages, PACKAGES.md PR) is parked — pick it back up only when asked, don't start it
unprompted.** This section is written to be self-sufficient for a fresh session with no prior
conversation memory.

**GitHub mirror path (decided 2026-07-13): `https://github.com/etempel/parquet-fortran`** —
already used in `fpm.toml`'s `[extra.ford]`/`homepage` and `.github/workflows/docs.yml` below;
kept in sync with this GitLab repo manually by the user, no mirroring automation exists or is
wanted.

### Target: fortran-lang package index, not `fpm publish`

There are two distinct fortran-lang "registries" — don't conflate them:

- **Package index** (fortran-lang.org/packages) — human-curated, PR-reviewed directory. This is
  the target. Submission is a PR to `fortran-lang/fortran-lang.org` following
  [`PACKAGES.md`](https://github.com/fortran-lang/fortran-lang.org/blob/master/PACKAGES.md),
  needing ≥3 community approvals. Requires (checked against this repo as of 2026-07-13): a
  license file in-source (have: `LICENSE`, BSD-3-Clause) and a README stating purpose/build
  info (have). Also gates on ≥5 GitHub stars once the repo is public there — not a docs
  concern, just a fact to know.
- **`fpm publish` registry** (registry-phi.vercel.app) — a separate, *machine* registry for
  resolving `[dependencies]` without git URLs. Explicitly in playground/testing status (reports
  of hangs, unresolved email verification, undeletable uploads). **Decision: skip this for
  now**, package-index listing only. Revisit only if asked — cost of adding later is low
  (`version`/`license` are already registry-valid: semver `0.9.5`, SPDX `BSD-3-Clause`).

### Publishing topology

This repo's only remote is a private self-hosted GitLab (`gitlab.4most.eu`). **Decision: this
project will be mirrored to a public GitHub repo**, kept in sync **manually by the user** — no
CI/automation for the mirroring itself is needed or wanted. All FORD/CI work below assumes the
GitHub mirror as the public-facing repo (GitHub Actions + GitHub Pages, not GitLab Pages). The
existing `.gitlab-ci.yml` stays focused on `fpm test`/coverage and is untouched by this work.

### FORD config: `[extra.ford]` in `fpm.toml` — DONE

Config lives in `fpm.toml`'s `[extra.ford]` table (fpm never parses `[extra]`, only requires
valid TOML; subtable name = tool name, per the manifest spec), not a standalone `ford.md`.
FORD auto-detects `[extra.ford]` and uses it. Precedent: `toml-f/toml-f`'s real `fpm.toml` does
exactly this. Already added to `fpm.toml` — see that file for the live config (project,
summary, `project_github`/`homepage` pointing at `github.com/etempel/parquet-fortran`,
`src_dir`, `exclude_dir = ["./test"]`, `exclude = ["parquet_bindings.f90"]`,
`page_dir = "./doc/pages"`, `output_dir = "./ford-doc"`, `media_dir = "./doc/media"`,
`display`, `source`/`graph`/`search`, and `[extra.ford.extra_mods]` for
`iso_fortran_env`/`iso_c_binding`). Top-level `homepage`/`keywords`/`categories` were also
added to `fpm.toml`.

Root-level project-file body — **created as `docs.md`** — is still required even with
`[extra.ford]` present (it supplies the generated front page's body, `[TOC]`, narrative; with
`[extra.ford]` present it needs no metadata block, just body content). It reuses the README via
FORD's `{!filename!}` include directive (same technique `fortran-lang/http-client` and
`urbanjost/M_CLI2` use) rather than duplicating it:

```markdown
{!README.md!}
```

### Doc-comment style: no migration needed, but coverage is NOT DONE — next step

`src/parquet.f90`'s existing `!>` comments (e.g. around the MAML-parsing interfaces, ~line
291-371) are already placed *before* the entity they document — this is exactly FORD's default
**predoc** convention already, no marker/placement change required. **What's still needed, and
is the next actual step in this whole effort:** an audit of which public procedures/types/
modules still lack a `!>`/`!!` block, since FORD only documents what's annotated, then filling
that coverage in across `src/*.f90`. This is source-file work and was explicitly deferred — the
2026-07-13 implementation session only did the non-`src/` layout/migration/CI work below,
per the user's instruction not to touch `src/` yet. FORD correctly resolves this project's real
`submodule` blocks (`parquet_read.f90`, `parquet_write.f90`, `parquet_metadata.f90`,
`parquet_maml_base_add_col_qc.f90`) back to their parent-module interfaces, so submodule
implementation files don't need separate doc comments from the interface declarations.

### MANUAL.md migration: DONE (content migrated); deletion NOT YET DONE

**Decision: do not maintain MANUAL.md alongside the FORD site long-term.** Its content has been
migrated into FORD `page_dir` pages under `doc/pages/` (2026-07-13) — MANUAL.md itself is
**still present, deliberately not yet deleted**. Delete it only after: (a) doc-comment coverage
above is filled in, (b) the FORD site has actually been generated and verified live/good on the
GitHub mirror, and (c) **explicit user confirmation is obtained first**, per this project's
general destructive-action caution. Until then MANUAL.md and `doc/pages/` intentionally coexist
(the migration copied content out, it did not yet remove the source).

`doc/pages/` layout actually created (one file per MANUAL.md top-level `##` section, mirroring
its old Contents ToC order — matches the decided granularity):

- `doc/pages/index.md` — page_dir landing page, `ordered_subpage:` list preserving the
  original section order, links to every page below plus the FORD-generated module/proc index.
- `doc/pages/embedding-maml-schemas.md`, `reading.md`, `writing.md`,
  `building-schema-in-code.md`, `maml-format.md`, `combined-example.md`, `error-handling.md`,
  `thread-safety.md`, `supported-data-types.md`, `performance.md`, `troubleshooting.md`.
- MANUAL.md's old **"parquet module API" section was *not* migrated** — it's superseded
  entirely by FORD's auto-generated pages once doc-comment coverage (above) is sufficient, per
  the original plan; nothing to do there besides finishing the doc-comment audit.

Cross-link rewriting already applied while migrating (so a fresh session doesn't need to
re-derive the mapping):
- Same-page anchors (e.g. a `reading.md` subsection linking another `reading.md` subsection)
  were left as bare `#anchor`.
- Cross-page anchors (e.g. `reading.md` linking a `supported-data-types.md` subsection) were
  rewritten to `<page>.html#anchor`.
- Links to README.md sections were rewritten to `../index.html#anchor` (root project-file body
  renders as the site's `index.html`; pages sit one level down under `page/`).
- Links to CONTRIBUTING.md (kept GitHub-repo-only, see below) and to the `docs/*.maml` fixture
  files were rewritten to absolute `https://github.com/etempel/parquet-fortran/blob/main/...`
  URLs, since neither is part of the generated FORD site.
- FORD's `[[entity_name]]` auto-link syntax was **not** applied to inline procedure-name
  mentions in the migrated pages (left as plain backtick code spans) — deferred as a polish
  pass to do once doc-comment coverage (above) exists and entity names/pages can be verified
  against a real FORD build, rather than guessing syntax now.
- **README.md is not migrated away** — it remains the landing page (both in the repo and, via
  the `{!README.md!}` include, as the FORD site's front page).
- **CONTRIBUTING.md and CHANGELOG.md stay GitHub-repo-only, not part of the generated FORD
  site** (decided) — consistent with this project's existing README/MANUAL/CONTRIBUTING split
  where CONTRIBUTING is contributor-facing, not user-facing; no `page_dir` entries for them.

**Still to do once MANUAL.md is actually deleted** (not done yet, don't do it as part of this
note): revisit every place that currently links to it (README.md's cross-references, this
CLAUDE.md's "Documentation structure" section above, `tools/check_doc_anchors.py` scope) and
rewrite the "Documentation structure" section itself to describe the new README + FORD-site
split instead of the current README/MANUAL/CONTRIBUTING three-way split.

### CI: GitHub Actions on the mirror, GitHub Pages hosting — workflow file DONE, not yet enabled/verified

`.github/workflows/docs.yml` has been created (2026-07-13), following the modern
`actions/deploy-pages` pattern used by `toml-f/toml-f`: on push to `main` (and PRs, build-only)
it installs FORD via pip, runs `ford docs.md`, and deploys the `ford-doc` output directory to
GitHub Pages via `actions/upload-pages-artifact` + `actions/deploy-pages`. **Not yet done, and
deliberately deferred (confirmed 2026-07-13):** this workflow can't actually run/deploy until
(a) the GitHub mirror at `github.com/etempel/parquet-fortran` exists and this file is pushed to
it, and (b) GitHub Pages is switched to "GitHub Actions" as its source in the mirror repo's
Settings — both are one-time manual steps the user does themselves once they actually migrate
to GitHub, **not something to attempt to automate or prompt about before then.**

### CI: GitLab Pages on gitlab.4most.eu — job DONE, not yet enabled/verified

Decided 2026-07-13: **keep publishing docs on both hosts**, not GitHub-only — GitLab remains
the system of record + existing `test` CI job, GitHub is a manually-synced mirror mainly for
fpm-ecosystem visibility (see the "is it worth abandoning GitLab" discussion this session).
Added a `pages` job to the existing `.gitlab-ci.yml` (new `docs` stage, after `test`):
installs just `gcc` (for `cpp`, FORD's configured preprocessor) + `pipx install ford` — a much
lighter `before_script` than the `test` job's, deliberately overridden rather than inherited,
since generating docs from source comments needs no Arrow/Parquet/git-lfs setup at all; then
runs `ford docs.md` and moves `ford-doc/` to `public/`. Gated to the default branch only
(`rules: if $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH`) and `allow_failure: true` so a docs hiccup
never fails the pipeline. `public/` added to `.gitignore` (build artifact, like `ford-doc/`).

Job **must be named exactly `pages`** — that's the classic, broadly-version-compatible way
GitLab recognizes a Pages-deploying job (works regardless of the specific GitLab server
version, unlike the newer explicit `pages: true` keyword which needs GitLab 17.4+ and wasn't
used here since gitlab.4most.eu's version is unknown).

**Verified locally (not via the actual pipeline, per this file's own "don't run GitLab CI
yourself" rule):** the YAML parses correctly, and the job's `script:` steps
(`ford docs.md; rm -rf public; mv ford-doc public`) were dry-run directly in this environment
and produce a working `public/index.html`. The `apt-get`/`pipx install ford` setup steps were
**not** exercised locally (would require root and pollute this dev machine) — first real
pipeline run on gitlab.4most.eu is the actual test of that part.

**Not yet done, deliberately deferred, same shape as the GitHub side:** (a) push this
`.gitlab-ci.yml` change so the job actually runs once, and (b) **GitLab Pages must be enabled
at the instance/admin level on gitlab.4most.eu** — unlike GitHub Pages (a per-repo toggle) or
gitlab.com (Pages on by default), **self-managed GitLab instances often have Pages disabled
entirely until a sysadmin enables the feature and configures a wildcard Pages domain** for the
whole instance. If the first real pipeline run's `pages` job succeeds but no Pages URL appears
in the project's Settings → Pages, that's the likely cause — something only the gitlab.4most.eu
admin (may or may not be the user) can fix, not something fixable from this repo alone.

### Local FORD build: DONE, verified working (2026-07-13)

The user installed FORD (`ford version 7.0.13`, via `uv tool install ford`, executable at
`~/.local/bin/ford` — separate from the `pyastro` venv `python3` on `PATH`, so
`python3 -c "import ford"` will still fail there; that's expected and not a problem, only the
`ford` CLI matters). `ford docs.md` from the repo root was run and now completes cleanly. Three
real bugs surfaced and were fixed as part of this validation:

1. **`preprocess`'s default tool (`pcpp`) wasn't installed**, and `src/parquet.f90` genuinely
   needs real cpp preprocessing (its version-string logic uses `#ifdef __GFORTRAN__`/
   `#define`/stringify macros — not valid Fortran without expansion, so turning preprocessing
   off outright was not an option). Fixed by adding
   `preprocessor = "cpp -traditional-cpp -E"` to `[extra.ford]` in `fpm.toml`, using the system
   `cpp` (`/usr/bin/cpp`, already present since it ships with gcc) instead.
2. **No markdown heading got an `id` attribute at all** — FORD's default Python-Markdown setup
   doesn't enable the `toc` extension, so every `#anchor` link this session wrote across
   `doc/pages/*.md` (and every in-page anchor already in README.md, since it's pulled in
   verbatim via `{!README.md!}`) silently pointed at nothing. Fixed by adding
   `md_extensions = ["markdown.extensions.toc"]` to `[extra.ford]` — matches the real
   precedent in `toml-f/toml-f`'s own `fpm.toml`. **This is a load-bearing setting — removing it
   silently breaks every same-page and cross-page anchor link across the whole site again**,
   with no error/warning from FORD when it happens.
3. `doc/pages/index.md` linked to `../module/index.html`/`../proc/index.html` for the
   module/procedure listings — those paths don't exist. FORD actually generates
   `lists/modules.html` and `lists/procedures.html` for those indexes; fixed in `index.md`.

Minor cleanups also applied: `exclude = ["parquet_bindings.f90"]` → `["**/parquet_bindings.f90"]`
in `[extra.ford]` (silences a "not relative to any source directory" warning, FORD's own
recommended fix); created `doc/media/` (with a `.gitkeep`) since `media_dir` pointed at a
directory that didn't exist yet (harmless warning otherwise, no favicon/logo picked yet — still
open, see below); added `ford-doc/` to `.gitignore` (it's `ford`'s build output, like
`build/`); documented the one-line `ford docs.md` build command in README.md's Contributing
section (per direct 2026-07-13 instruction — normally build-tooling docs would go in
CONTRIBUTING.md per the "Documentation structure" rules, but this was an explicit exception).

**Verification method**: every `href="...html..."` and every `#anchor` fragment across
`ford-doc/page/*.html` and `ford-doc/index.html` was checked programmatically (target file
exists, and target actually has a matching `id`/`name`) — all resolve cleanly as of this fix,
*except* one known, pre-existing, out-of-scope issue:

- ~~`index.html` (rendered from README.md via `{!README.md!}`) contained several dead
  `MANUAL.md#anchor` links~~ **First fix (2026-07-13):** rewritten to absolute GitHub blob URLs.
  **Superseded same day** — the user then asked to *completely remove* any dependency on
  MANUAL.md (it's being deleted eventually; no references at all should remain, not even
  working ones). All 18 README.md references were rewritten again, this time to **repo-relative
  `doc/pages/*.md` links** (e.g. `doc/pages/reading.md`, `doc/pages/thread-safety.md`) — the
  actual migrated content now lives there. MANUAL.md itself was **not** touched, per
  instruction (and its own repo-relative links to itself, from other files, are untouched too —
  only README.md's references were in scope for this request).
  - The "API overview" section's five category links (Utility/MAML and metadata/Writer/Reader
    table & column info/Reader column data) had **no** `doc/pages/*.md` equivalent to point at
    (that whole "parquet module API" section of MANUAL.md was never migrated — see above, it's
    superseded by FORD generation instead) — de-linked to plain bold text instead of inventing
    a broken or misleading target.
  - `tools/check_doc_anchors.py` (per this file's own "Checking documentation links" rule)
    caught a real bug in the first attempt: several links included an anchor fragment matching
    the *page's own title* (e.g. `doc/pages/thread-safety.md#thread-safety`) — invalid, because
    every migrated page deliberately has **no** in-body heading duplicating its frontmatter
    `title:` (see the MANUAL.md migration section above), so no such heading/anchor exists in
    the raw markdown for GitHub's slugger to find. It only appeared to work in the FORD-rendered
    HTML because FORD synthesizes a heading from `title:` in its own template. Fixed by dropping
    the anchor fragment on all six such "whole-page" references (linking to the file is
    sufficient; an anchor to its own top is redundant). `tools/check_doc_anchors.py` now passes
    clean (all 6 checked files, zero broken links).
  - **Known, accepted, pre-existing-pattern trade-off, not fixed:** these repo-relative
    `doc/pages/*.md` links (like the already-pre-existing `CONTRIBUTING.md`/`LICENSE`/
    `CHANGELOG.md` links elsewhere in README.md) do **not** resolve inside the generated FORD
    site itself — FORD's output tree doesn't mirror the repo layout (`doc/pages/reading.md`
    source becomes `page/reading.html` output; `CONTRIBUTING.md`/`LICENSE`/`CHANGELOG.md` aren't
    copied into the build at all). Verified programmatically: 20 such repo-relative hrefs in the
    generated `index.html` don't resolve to a file in `ford-doc/`. Not addressed, because (a)
    the FORD site isn't deployed anywhere yet, so it has zero real readers today, (b) the only
    alternative fixes are either duplicating README content specifically for the FORD front
    page, or reintroducing off-site absolute/GitHub links — the exact pattern just explicitly
    rejected in favor of full removal. Revisit only if asked, e.g. once the FORD site is
    actually live and this becomes a real user-facing problem.

- **`[[entity_name]]` auto-linking pass** over the migrated `doc/pages/*.md` files (converting
  plain backtick procedure-name mentions into FORD's clickable entity links) is still not done
  — still deliberately deferred, unchanged from before this validation pass.
- **Favicon/media**: `doc/media/` now exists (empty, just a `.gitkeep`) but still has no actual
  logo/icon/favicon — still open, not blocking.
- Exact wording/placement of the "Documentation structure" section rewrite in this file once
  MANUAL.md is gone (noted above, but the precise new text wasn't drafted).
- **Doc-comment coverage (`!>`/`!!` audit across `src/*.f90`) is intentionally not being worked
  on right now** (confirmed 2026-07-13) — do not start this without being asked again, even
  though it's nominally "next" in the sequencing below. Note from this validation pass: FORD
  already generates pages for every public interface/type regardless (see `ford-doc/interface/`,
  `ford-doc/type/`, `ford-doc/module/` — e.g. `parquet_open_reader.html`,
  `parquet_schema.html` already exist and look correct structurally), just without prose until
  those doc comments are written.

### Sequencing (updated — steps 1/3/4 done, 2/5/6 remain)

1. ~~`fpm.toml`: add `[extra.ford]`, `homepage`/`keywords`/`categories`, root `docs.md`.~~ DONE.
2. **NEXT:** audit + fill `!>`/`!!` doc-comment coverage across the public API in `src/*.f90`.
3. ~~Stand up the GitHub Actions workflow~~ workflow file DONE (`.github/workflows/docs.yml`);
   actually running/verifying it needs the GitHub mirror to exist first (see CI section above).
4. ~~Migrate MANUAL.md sections into `doc/pages/*.md`~~ DONE — see MANUAL.md migration section
   above for exactly what was and wasn't carried over.
5. Once doc-comment coverage (step 2) is done and the FORD site is verified live and good on
   the GitHub mirror: confirm with the user, then delete MANUAL.md and update this file's
   "Documentation structure" section and any remaining cross-references to it.
6. Only after all the above: prepare the `fortran-lang/fortran-lang.org` PACKAGES.md PR for
   package-index listing.

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
