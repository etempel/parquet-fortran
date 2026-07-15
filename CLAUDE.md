# Instructions for Claude

## New features require tests and docs

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
- **Badges:** README.md carries three dynamic `gitlab.4most.eu` badges (CI pipeline, test
  coverage, API documentation) alongside the static license/language/fpm ones. These are
  GitLab-specific — `tools/prep_github_mirroring.sh` swaps them for a single GitHub Pages
  documentation badge when mirroring (see CONTRIBUTING.md's "Mirroring to GitHub").

## FORD doc-comment conventions

Every public/private procedure, type, dummy argument/function result, and type-bound procedure
binding in `src/*.f90` carries a `!>`(leading)/`!!`(trailing) doc-comment doc-comment (already true throughout `src/*.f90`,
validated end-to-end with `ford docs.md` — clean run besides the expected environment-only
"Graphviz not installed" warning). Keep new code to the same standard:

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
- Don't start a doc-comment's first line with a bare `word:` (e.g. "qc: min: ..."). FORD reads
  that as an attempted metadata key: it either warns (unrecognized key) or, worse, silently
  swallows the line if it happens to match a real key (`date:`, `author:`, `version:`, ...).

## FORD config gotchas

- `md_extensions = ["markdown.extensions.toc"]` in `fpm.toml`'s `[extra.ford]` is
  **load-bearing** — without it, no heading gets an `id`, silently breaking every anchor link
  site-wide with no error.
- `preprocessor = "cpp -traditional-cpp -E"` is required, not optional — `src/parquet.f90`'s
  version-string logic uses real cpp macros, not just comment-style directives.
- `doc/pages/*.md` files deliberately have **no top-level heading in their body** (title comes
  from frontmatter only) — adding one back would reintroduce the duplicate-heading bug
  `doc/user.css` fixes on the front page.
- Two working CI doc-publish paths, both confirmed working end-to-end: `.github/workflows/docs.yml`
  (GitHub Actions → GitHub Pages) and `.gitlab-ci.yml`'s `readthedocs` job (GitLab CI →
  gitlab.4most.eu's readthedocs-style docserver). GitLab Pages isn't available on
  gitlab.4most.eu — the docserver replaces it.
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
  under "Module Procedures" on the generic's page shows "Arguments: None", permanently. Root
  cause (confirmed by reading `ford/sourceform.py`): `FortranInterface.correlate()` resolves each
  generic member through `self.all_procs[name]`, which lands on the submodule's
  `FortranModuleProcedureImplementation` object — and that class hardcodes `self.args: List[str]
  = []` in `_initialize()`, unconditionally, regardless of what the submodule source actually
  declares. This is **not fixable from source**: rewriting the submodule's abbreviated `module
  procedure NAME` into a fully restated `module subroutine NAME(args)` (mirroring the spec in
  `parquet.f90`, the pattern used for `parquet_maml_base_add_col_qc.f90`) was tried and verified
  to have zero effect on the rendered output — confirmed by rebuilding `ford-doc/` before and
  after. Solo public procedures that aren't generic members (e.g. `parquet_get_string_length`)
  are unaffected — they render correctly via a different code path
  (`FortranModuleProcedureInterface`, wrapping the spec's own parsed `FortranSubroutine`).
  Workaround in place: each of the 12 public generics' own leading `!>` doc-comment (the block
  immediately above `interface <name>`) spells out every distinct argument name/role in prose,
  since that comment (unlike the per-specific ones) does render on the generic's page. Do not
  re-attempt the submodule-restatement approach without first re-verifying against a newer FORD
  release that the upstream bug is actually fixed. See FORD Issue (https://github.com/Fortran-FOSS-Programmers/ford/issues/738).

## Publishing: remaining outside-this-repo steps

FORD docs, doc-comment coverage, and MANUAL.md retirement (superseded by `doc/pages/*.md` + the
FORD-generated reference) are complete. The GitHub mirror (`github.com/etempel/parquet-fortran`)
is pushed and GitHub Pages is live — both doc-publish paths (GitLab's readthedocs docserver and
GitHub Pages) are now confirmed working end-to-end. What's left, in order:

1. The fortran-lang.org **package index** PR (`fortran-lang/fortran-lang.org`'s `PACKAGES.md`) —
   not the separate `fpm publish` registry, still playground/testing status, skipped
   deliberately.
2. Optional polish, no urgency: a `[[entity_name]]` auto-link pass over `doc/pages/*.md`'s
   inline procedure-name mentions (plain backtick spans today); a favicon/logo for `doc/media/`
   (exists, currently empty besides a `.gitkeep`); extending `tools/check_doc_anchors.py` to also
   scan `doc/pages/*.md` (not covered today — see "Checking documentation links" above).

## MAML fixture directory: `schemas/` (renamed from `docs/`)

`.maml` example/fixture files live in `schemas/`, not `docs/` (renamed to avoid confusion with
`doc/`, FORD's `page_dir`/`media_dir`). `tools/generate_parquet_maml.sh` accepts
`--dir=<name>`/`--dir <name>` (default `schemas`) so downstream projects embedding their own
MAML schemas aren't forced to match this project's convention — see
`doc/pages/embedding-maml-schemas.md` for the user-facing how-to.

## Report before implementing on analysis/audit requests

When asked to analyze, audit, or review something (naming conventions, documentation
duplication/coverage, test coverage, etc.), report findings and a proposed plan first and
wait for confirmation before editing any files. Only proceed straight to editing when
explicitly asked to implement/fix/add something directly.

## Stale `fpm` build cache

If `fpm test` behaves unexpectedly after source changes (e.g. a test target seems to run
old code), try `fpm clean --all` to force a clean rebuild before spending time debugging —
fpm's build cache can serve a stale binary.

This bit us concretely: building with several different `FPM_FFLAGS` creates multiple
`build/gfortran_<hash>/` dirs, and `test/test_errors.f90`'s `error_scenarios_bin` does
`find build -name error_scenarios | head -1` — which can then pick a *stale* binary and report
"scenario name not recognized" (or run old code) even though the source is current. Symptom:
tests that pass when run scoped but fail under a full `fpm test`. `tools/coverage.sh` now runs
`fpm clean` up front to avoid this; for a plain `fpm test`, `fpm clean --all` fixes it.

## Don't run the GitLab CI pipeline yourself

The user runs `.gitlab-ci.yml` on their own GitLab server — don't attempt to execute it
(e.g. via `gitlab-runner`, docker, or otherwise) as part of verifying changes. Verify
locally instead (`fpm build`/`fpm test` with the same `FPM_FFLAGS`/`FPM_CXXFLAGS`/
`FPM_LDFLAGS` the CI job sets, minus anything CI-environment-specific like the apt installs).

## Don't commit or push on the main/default branch yourself

The user always commits and pushes their own changes on `main` — even after explicitly asking
for a feature/fix to be implemented, do not run `git commit`/`git push` on `main` yourself
unless they separately, explicitly ask for that specific commit. Leave finished work
uncommitted in the working tree for them to review and commit. (This is specific to the
main/default branch; it doesn't apply to work you've been asked to do inside your own
throwaway branch/worktree, if any.)

## The `parquet_strings` module (standalone, not yet integrated)

`src/parquet_strings.f90` is an independent module (`use parquet_strings`; depends only on
`iso_fortran_env`/`iso_c_binding`) providing `parquet_string_column` (Arrow-LargeUtf8-style
offsets+data+bit-packed-validity string storage) and `parquet_string` (a non-owning handle to
one element). User guide: `doc/pages/string-columns.md`.

- **The module is `parquet_strings` (plural) on purpose.** A module and a type cannot share a
  name in gfortran (`public :: parquet_string` binds to the module, and the type declaration
  then conflicts). The user-facing *type* is `parquet_string`, so the *module* had to differ —
  do not "fix" the plural back to `parquet_string`.
- **It is deliberately NOT wired into the library yet.** The two interop hooks
  `raw_buffers` (export c_loc pointers for a writer) and `append_buffers` (bulk-append one row
  group from C buffers, int32/int64 offsets + validity merge) are the extension points for a
  future `parquet_read_column`/`parquet_write_column` integration. When integrating: the current
  Fortran↔C++ string boundary is a fixed-width, space-padded block (`parquet_read_string_column`/
  `parquet_append_string_column`), so efficient support needs NEW C++ entry points that pass
  offsets+data+validity directly — and the write path **must not trim** (the column stores bytes
  verbatim by default; the padded path trims because padding is indistinguishable from real
  trailing spaces). Scope is scalar 1-D string columns only; vector/matrix string columns stay on
  the legacy padded path.
- **`allow_null=.true.` on `get`/`to_string` returns an empty string, not unallocated** — see the
  gfortran note in "Build and compiler notes" below.

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
  warning. See `parquet_metadata_validate.f90`'s `parquet_qc_numeric_bound` for the pattern.
  This does not apply to the *other* `-Wcompare-reals` sites in this codebase (e.g.
  `value == anint(value)` in `parquet_write.f90`/`parquet_metadata_validate.f90`, testing
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

For a *required* (non-optional) argument, this ambiguity constraint doesn't apply — Fortran can
disambiguate two specifics differing only by a required argument's kind without any special
handling, so just add the second kind-specific directly (no base/kind-suffixed split needed),
sharing one private `_impl` worker between the two (mirrors `add_col_qc_impl`'s existing
shared-worker pattern) — see `parquet_read_array_row_mode`'s `row_index` (12 specifics: 6 data
types x `integer(int32)`/`integer(int64)` row_index, each pair delegating to one
`parquet_read_<type>_array_row_mode_impl`).

## Guarding a hard Arrow int32-only ceiling

Some Arrow/Parquet C++ APIs are hard-capped to a plain `int32_t`, with no int64/"large" fallback
at all — found three times so far: `arrow::FixedSizeListBuilder`/`fixed_size_list()`'s `list_size`
(a vector column's per-row width, `col_size`), `arrow::Schema::num_fields()`/`GetFieldIndex()` (a
table's column count), and Parquet's own repetition/definition-level generation for list-typed
columns (`level_conversion.cc`), which walks every flattened element of a row group with a plain
`int32_t` counter (see apache/arrow#33188 / ARROW-17983, confirmed still open/unfixed upstream).
Unlike the other two, this last one is scoped to one **row group**, not a column's total element
count (`nrows * col_size`) — and `close_parquet_writer`'s row-group auto-sizing already keeps
every row group under it by shrinking the row-group size, however large `nrows` gets, so a large
total is never actually a problem (confirmed empirically: a multi-billion-element vector column
writes successfully split across small-enough row groups). Only an *explicit* `chunk_size=`
(`parquet_open_writer`/`parquet_set_writer_options`) that itself conflicts with a column's
`col_size` still aborts, since that's a caller-forced value auto-sizing can't silently override —
see `check_chunk_size_fits_limit_for_col_size`/`check_explicit_chunk_size_fits_arrow_limit`/
`check_chunk_size_fits_metadata_limit` in `parquet_wrapper.cpp`, and the README's Limitations
section. This differs from row count (`int64_t` throughout Arrow) or a string column's byte
payload (which has an `arrow::large_utf8()` fallback) — for those two (`col_size` and column
count), there is no workaround, only a clean failure instead of letting Arrow silently
truncate/wrap internally. When a new int32-only ceiling is found, guard it with the pattern
already used for `col_size`/column-count above (see `check_col_size_fits_arrow_limit`/
`check_column_count_fits_arrow_limit` in `parquet_wrapper.cpp`) — only reach for the
row-group-scoped auto-sizing approach instead if the new ceiling is similarly scoped per-row-group
rather than per-column-total:

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
`parquet_read_array_row_mode` on the *read* path used to hit this ceiling for a different reason:
they materialized the *whole* column via `get_single_chunk_array`'s `ReadColumn` (Arrow's
whole-file, all-row-groups-at-once convenience API) just to answer a size query or fetch one row,
which trips Arrow's own internal int32 list-index/offset limit once `nrows * col_size` crosses
int32 — even though the column was written perfectly safely (every row group under the limit, per
the write-side guard above). Fixed by making all three genuinely row-group-scoped instead of
adding a new guard: `parquet_get_col_size`/`parquet_get_column_total_elements` read `col_size`
straight off the schema's `FixedSizeListType::list_size()` (no data read at all) for a
FIXED_SIZE_LIST column, and `parquet_read_array_row_mode` resolves which row group a given
`row_index` falls in (`resolve_row_group_for_row`, walking each row group's `num_rows()` from the
file footer) and reads only that one row group (`get_row_group_chunk_array`, the same helper the
`_column_chunk` family already used) rather than the whole column. The one case that still falls
back to the old whole-column path is an active row filter
(`parquet_open_reader(..., filter=)`/`parquet_reader_set_filter`): a filter mask has no row-group
structure of its own (see `get_row_group_chunk_array`'s own comment), so `row_index` there means
"index into the filtered result", not a physical file row. Regression-tested via
`test/error_scenarios.f90`'s `scenario_col_size_and_row_mode_avoid_whole_column_read` (a
process-global `g_debug_force_whole_column_read_error` hook forces `get_single_chunk_array` to
abort the instant it would actually read a whole column, on a tiny fixture — the scenario
finishing without aborting proves none of the three calls took that path) plus its negative
control `scenario_whole_column_read_forced_error_control` (proves the hook itself actually fires).

## Manual (never-`fpm test`) large-scale/benchmark tools

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

## Keeping `tools/prep_fpm_publish.sh` in sync

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

## Error stop messages: include file/schema context

New `error stop` messages in the read/write/schema-building paths should append the relevant
file and, where applicable, schema/maml name using the existing helpers — `writer_context_suffix`
(`parquet_write.f90`), `reader_filename_suffix` (`parquet_read.f90`), `maml_name_suffix`
(`parquet_metadata.f90`) — rather than naming only the offending column/field, so a failure is
identifiable when several readers/writers/schemas are in play at once. These are only
meaningful once the reader/writer/schema knows its file/name (i.e. post-open), so the
guard-clause "…has not been opened" messages are exempt.
