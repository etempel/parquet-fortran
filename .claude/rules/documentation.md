# Documentation conventions

## New features require tests, docs and a CHANGELOG entry

For every "implement/add/fix" request, without being asked separately:

1. **Tests** in the relevant `test/*.f90` suite; abort/error paths via `test/error_scenarios*.f90` +
   `test/test_errors*.f90` + `tools/run_error_scenarios.sh` (see `testing.md`). **A FIX writes its
   test first and runs it against the unfixed source to see it fail**; a fix whose test was never
   seen red is a surviving mutation (`testing.md`, Mutation testing).
2. **Docs**: every new public procedure/type gets a `!>`/`!!` doc-comment (the FORD reference is the
   only per-procedure index); user-facing behaviour goes in the relevant `doc/pages/<group>/*.md`
   page; README.md only when the landing-page story changes; CONTRIBUTING.md only for contributor
   workflow.
3. **CHANGELOG.md**: an `[Unreleased]` entry in the Keep-a-Changelog format already used there.

## CHANGELOG rules

`[Unreleased]` is read by someone upgrading from the newest published section.

- **State WHAT changed, never WHY**, in one or two lines: no rationale, measurement, design
  discussion or replaced alternative. Test: a sentence that would still be there had the change
  been obvious is justification — delete it.
- **`### Changed`/`### Fixed` are for behaviour in the RELEASED version only.** A fix or change to
  something still in `[Unreleased]` is folded into that feature's own `### Added` bullet. A feature
  implemented after the last release has exactly one bullet, under `### Added`, kept current.
- **`### Added` records overall features, not their parts.** Sub-features, new bindings and helpers
  serving a listed feature go in that feature's bullet; closely related additions share one bullet.
- **Leave out what is not user-facing**: test coverage, internal refactors, tooling a consumer
  cannot observe. A new contributor-facing tool wired into CI gets one short line; its tweaks none.
- **All minor changes share ONE bullet, placed last under `### Fixed`**: "Many other minor fixes and
  improvements." Never expand it into a sub-list.
- **One `### Added`/`### Changed`/`### Fixed` per release section, in that order.**
- Re-read the whole `[Unreleased]` section before adding; an entry often belongs inside an existing
  bullet. Never edit a published section.

## Documentation structure

Three layers; put new content in the right one.

- **README.md** — lean landing page: what the library is, short feature list, one example,
  install/prerequisites/environment variables, important behaviour, limitations, license and
  contributing pointers. It must NOT carry a version number or status banner (`VERSION.txt` and
  `CHANGELOG.md` own the version), an entry-module table
  (`doc/pages/operating/choosing-a-module.md` is the authority), or a per-procedure API index.
- **`doc/pages/<group>/*.md`** — the user guide, two layers deep: six groups (`io/`, `types/`,
  `schema/`, `tables/`, `utilities/`, `operating/`), one file per topic plus a group `index.md`;
  `doc/pages/index.md` is the guide's landing page. Every `index.md` carries one `ordered_subpage:`
  per child, mirrored by its body bullets; every group directory needs an `index.md` (FORD silently
  skips a group without one). Enforced by `check_doc_page_index_consistency`. Cross-group links use
  `../<group>/<name>.html#anchor`; `../../index.html` is README.md, `../index.html` the guide landing
  page. No hand-maintained per-procedure tables.
- **FORD-generated API reference** — built from `!>`/`!!` doc-comments; the sole per-procedure
  reference.
- **CONTRIBUTING.md** — contributor workflow only (next section).

Working rules:

- A new public procedure's whole doc obligation is its `!>`/`!!` doc-comment; never reintroduce a
  README API index.
- Every heading in README.md, CONTRIBUTING.md and CLAUDE.md appears in that file's own Contents ToC;
  `doc/pages/*.md` pages need no ToC.
- Moving content between README.md and `doc/pages/` turns in-page `#anchor` links into cross-file
  ones and stales "above/below/this README" wording; fix both and run `tools/check_doc_anchors.py`.
- **A performance figure on a guide page is approximate and machine-free**: no compiler names, no
  machine descriptions, no decimals on a ratio ("several times"); name the `bench/*.sh` tool that
  measures it. Contract numbers (thresholds, bounds, growth factors, documented shares) and parity
  claims stay exact. A compiler named for a correctness or build reason stays.
- **A page has ONE name**: the frontmatter `title:` is canonical; `doc/pages/index.md`'s flat list
  quotes the whole title, a group `index.md` bullet quotes the title or an initial prefix
  (`check_page_titles_match_their_list_entries`). A flat-list entry saying more than the `<h1>`
  means the title is too short — lengthen the title.
- **A count written beside the list it counts, or a list the code also owns, needs a check**, not
  careful review; point at the source instead of copying it.
- **Optional arguments in square brackets, comma OUTSIDE the bracket**: `%f(a, [b])`, never
  `%f(a [, b])` and never `%f(a, [b=,] [c=])`. The first bracketed signature on a page carries a
  one-line note on the brackets. Enforced across `doc/pages/` and README.md by
  `check_bracket_convention`, which bans both wrong spellings — do not audit this by grepping for
  one of them. `CHANGELOG.md` is outside the check. A signature-only block may keep its `fortran`
  fence tag; a runnable example never contains a bracket.
- **A reference bullet carrying more than about three claims becomes its own `###` subsection**,
  opening with the call form in the bullet's bold style; re-run `tools/check_doc_anchors.py`.
- **A fenced code block is NEVER indented** — python-markdown emits an indented fence literally and
  closes the list. Fence and content at column 0; dedent bullet prose that follows. Enforced by
  `check_no_indented_code_fence`.
- **A list item's continuation paragraph needs 4-space indentation AND a blank line before the next
  bullet.** Two spaces silently ends the list; a missing blank line swallows the following bullets
  as lazy continuation. Probe by counting `<li>` in the rendered HTML against `^- ` in the source.
  Usually the right move is promoting the bullet to a `###` heading.
- **Code-fence tags**: `fortran`, `bash`, `yaml`, `toml`, or bare (program output, plain-text
  diagrams). A MAML block takes a bare fence or `yaml`; never `maml`.
- **Diagrams are plain text** inside a code fence, never Mermaid.
- README.md carries three dynamic `gitlab.4most.eu` badges; `tools/prep_github_mirroring.sh` swaps
  them when mirroring.

## CONTRIBUTING.md is project-wide workflow only

- CONTRIBUTING.md answers "how do I work on this repository". A per-tool, per-script or per-file
  detail (invocation, environment variables, modes, output meaning) goes in **that file's own header
  comment**, never in CONTRIBUTING.md.
- **A tool gets ONE row in the index table** (name plus one-line purpose) and nothing more; a new
  tool needs only that row, a changed tool needs nothing. A consumer-facing generator's row says
  "Consumer-facing". Enforced by `check_contributing_is_an_index`; `tools/*.md` are exempt.
- Never write out a count or a list the repository owns; point at the source
  ("the `new_testsuite(...)` array in `test/run_tester.f90`").
- Before adding a paragraph, pick its file: `.claude/rules/` (a rule for future work), the tool's
  header (how the tool works), `feature_*.md` (a campaign's measurements, which nothing else may
  cite -- see `workflow.md`), `doc/pages/` (what a user needs). CONTRIBUTING.md is the residue.

## A guide page describes the CURRENT state, never a former one

- A sentence in `doc/pages/`, README.md or a `!>` doc-comment must be evaluable without knowing a
  state of the code the reader never saw. Remove three shapes: a justification appealing to a former
  API; "now"/"have always" attached to behaviour across releases; provenance (how or where
  something was discovered).
- Not in this class: comparisons between two current APIs, before/after within one call ("the
  column is now empty"), documented deprecations, `CHANGELOG.md`, and `src/` code comments.
- Re-check whenever a page is touched for any reason. Grep for candidates and decide each by hand;
  most hits are innocent:

```bash
grep -nE "used to|previously|formerly|no longer|in the past|historically|originally" doc/pages/<page>
grep -nE "ha(ve|s) always|as before|unchanged from|for (backward )?compatibility|root-caused" doc/pages/<page>
grep -nE "\bnow\b" doc/pages/<page>
grep -nE "\balways\b|\bstill\b" doc/pages/<page>
grep -nE "has moved|has changed|was renamed|has since|used to be|no longer does" doc/pages/<page>
```

- On an already-reviewed page fix the sentences and nothing else; rewrap only on the round that
  first reviews a page (the `/review-doc` skill, Pass 4).

## Checking documentation links

After editing headings or `#anchor` links in any root-level `*.md` or `doc/pages/**/*.md`, run
`tools/check_doc_anchors.py`. It resolves in-page, cross-file and FORD-rendered
(`name.html#anchor`, `../index.html`) forms and exits nonzero on a broken link. It does not scan
`.claude/rules/`.

## FORD doc-comment conventions

- Leading `!>` = predoc, trailing `!!` = postdoc. `!<` is not a FORD marker.
- Every procedure (public or private), type, dummy argument, function result and type-bound binding
  carries a doc-comment. Argument `!!` tags go wherever the argument list is written out (the spec in
  `parquet_core.f90`, and any submodule body that restates the full interface); the abbreviated
  `module procedure NAME` form is exempt.
- Every binding in a type's `contains` block (`procedure ::`, `generic ::`, `final ::`) gets its own
  trailing `!!`. Accepted exception: the two multi-line `generic :: add_metadata` bindings use a
  leading `!>`; do not extend it without the same multi-line-continuation reason.
- Never start a doc-comment's first line with a bare `word:` (FORD reads it as a metadata key;
  `check_no_doc_block_opens_with_a_ford_metadata_key`).
- A rationale block directly above a procedure header is that procedure's doc-comment and must use
  `!>`; plain `!` is reserved for interface-block group banners.
- **A clean `ford docs.md` does not verify coverage** (undocumented-entity warnings are off in
  `fpm.toml`); use `ford --warn docs.md`. Ignore `Undocumented variable` (locals),
  `Undocumented moduleprocedure` (abbreviated form), `Could not extract source code`, and
  `Undocumented interface`/`Undocumented proc` (false positives). **`Unknown entity` is the one
  load-bearing category**; it rises by one for each use-associated name a module re-exports or
  hides. Compare it per module, never as a total, and never quote a stored figure. **Count it with
  `--config="graph=false" --no-search`**, which skips the call graphs and the search index — the
  slow half of a run, and no warning depends on either:

```bash
ford --warn docs.md --config="graph=false" --no-search 2>&1 | tr '\n' ' ' | tr -s ' ' \
  | sed 's/Warning: Unknown entity/\n&/g' \
  | grep -o "attribute '[^']*' in module '[^']*'" | sort | uniq -c
```

- After a pure refactor (file split/relocation) diff those totals before and after
  (`git stash` / `git stash pop`); they must match. A stage that adds procedures compares the
  per-category breakdown instead. A relocated private helper vanishing from its old module's page
  is expected.

## FORD config gotchas

- `md_extensions = ["markdown.extensions.toc"]` in `fpm.toml`'s `[extra.ford]` is load-bearing
  (without it no heading gets an `id`).
- `preprocessor = "cpp -traditional-cpp -E"` is required.
- `doc/pages/*.md` bodies carry no top-level heading; the title comes from frontmatter only.
- Two doc-publish paths (`.github/workflows/docs.yml`, `.gitlab-ci.yml`'s `readthedocs` job) both
  run `tools/fix_ford_page_links.sh ford-doc` after `ford docs.md`, because FORD does not resolve
  `doc/pages/*.md` links inside the embedded README.md. README.md keeps that link form; pages link
  to each other with FORD's `page/*.html`-relative form.
- Each MAML generator's `end module` template line must keep emitting
  `! GCOVR_EXCL_LINE`.
- **FORD 7.0.13 renders no per-argument docs for members of a multi-specific generic interface**;
  not fixable from source (upstream issue 738). Each public generic's own leading `!>` names every
  argument in prose instead.
- **FORD 7.0.13 cannot resolve a use-association accessibility statement** (`Unknown entity`); not
  fixable from source. The `parquet` facade's re-exports do not appear on `module/parquet.html`
  either (accepted); the guide points readers at `lists/procedures.html`/`lists/types.html`.
- **FORD cannot extract "Source Code" for the abbreviated `module procedure` form.** Restating every
  body would conflict with the doc-comment exemption above; leave undone unless the maintainer
  decides otherwise.
