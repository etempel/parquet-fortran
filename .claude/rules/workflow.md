# Workflow & guardrails

Rules for how work is done in this repository. Read before any change.

## Scope of edits

- **Only modify files inside this repository.** Never edit, create or delete files elsewhere (other
  checkouts, dotfiles such as `~/.zprofile`). If something outside genuinely must change, say what
  is needed and let the user do it.
- **Never commit or push on `main`.** Leave finished work uncommitted for the maintainer, even after
  an explicit "implement this" request. Only a separate, explicit request for a specific commit
  lifts this. A throwaway branch/worktree you were asked to use is not covered.
- **Report before implementing on analysis/audit/review requests.** Present findings and a plan,
  then wait for confirmation. Edit directly only when explicitly asked to implement/fix/add.
- **Apply only low-blast-radius renames/refactors directly** (few call sites, no public API or doc
  impact). Anything wider (public API, many call sites, cross-file conventions) is proposed and
  waits for confirmation.
- **Report the reading you implemented and what you deliberately left out**, in the closing report
  of any implement/fix request that had more than one reading (`/feature-request`'s "Not in scope",
  applied without the document). Say it at the end; do not block on it.

## Verifying before reporting

- **Name the checks a change must pass before starting it, and run them before reporting it done**:
  `fpm test` (`build.md`: the ordinary check), `tools/run_lint_check.sh` (CI's whole lint stage),
  plus whatever the area adds — `fpm test --profile debug` for allocation or array shapes,
  `tools/check_doc_anchors.py` for edited headings or anchors, a generator's `--check`,
  `tools/check_module_footprints.sh` for a new `use` line, `ford --warn docs.md` per-module
  `Unknown entity` for doc-comment changes. `/feature-request` §4 carries the full list.
- **Report what was run and what it said.** A check not run is named as not run; a failure is
  quoted, never summarised away.

## CI and the CI image

- **Never run the GitLab CI pipeline yourself** (no `gitlab-runner`, no docker). Verify locally with
  `fpm test`; `build.md` says which flags and profiles reproduce CI.
- **Never build or rebuild the CI-environment Docker image** (`tools/build_ci_test_image.sh`,
  `docker build`, any other route). Only run this repository inside an image the maintainer has
  already provided. Any change the image needs (compiler version, package, `before_script` step) is
  a request to the maintainer; never install into a running container.
- Ask for the image after a local run is green and the CI failure persists; it carries gfortran,
  ifx and flang.

## `feature_*.md` planning documents

- `feature_*.md` files in the repo root are design/planning scratch documents, git-ignored
  (`.gitignore`: `feature_*.md`). `feature_risks.md` is the one **tracked** exception
  (`!feature_risks.md`); adding another negation is a decision to publish a document.
- The `/feature-request` skill writes a new design document in the expected shape.
- **Every feature document carries a settings analysis**: does the feature add a process-global
  parameter, does it pass the admission test in `api-conventions.md`, and if so its knob name,
  default, validation and environment variable. Write "none" explicitly when there is none.
- **Write every `feature_*.md` to be self-explaining without the current conversation**: no "as
  discussed", no reference to chat or tool-call artefacts; quote the user's decisions verbatim.
  Cross-references to repository files are fine.
- **NEVER delete, move, rename, `git rm` or consolidate-by-removal any `feature_*.md`**, even when
  asked to "clean up", "archive" or "merge" — such a request is for the content work only. They are
  git-ignored, so deletion is unrecoverable. Instead: write the new document with what is still
  open, leave every original in place, list what was carried across from each, put a "superseded
  by" banner at the top of a fully superseded document, and hand the list to the maintainer, who
  archives by hand.
- **Never cite a `feature_*.md` planning document from source, tests, tooling, benchmarks or any
  documentation page.** They are git-ignored, so git holds no copy: once the working tree loses
  one, every pointer to it dangles and the reader has no way to recover what it said. Write the
  reasoning the citation was standing in for into the comment itself, or point at something git
  keeps — a rule under `.claude/rules/`, a guide page under `doc/pages/`, a test, or the source.
  This holds for a document that still exists: it is one `git clean` from being unrecoverable, so
  a live pointer is a dangling one that has not happened yet.
- `feature_risks.md` is the exception, because it is **tracked**: `Risk-NNN` citations are expected
  and stay. Naming the category as a glob is machinery, not a citation, and also stays —
  `.gitignore`'s `feature_*.md`, `count_lines.py`'s shipped/planning split, `check_doc_anchors.py`'s
  existence exemption.

## The `feature_risks.md` open-risks register

Tracked list of OPEN risks: silent failures a user can suffer that no test or check catches today.
A covered risk is not in it. Read the entries for an area before editing it.

- **Admission requires all four**, each answered in the proposal:
  1. the failure is a silent wrong answer, lost or corrupted data, or a hang, reachable through the
     public API on a realistic path;
  2. no test or check in `fpm test` or `tools/run_lint_check.sh` fails when the property is broken
     (name the mutation tried, or why none can be);
  3. a test cannot be written in the same change — if one can, write the test instead;
  4. it is in no excluded class: performance or memory; diagnostics and log routing; a compiler or
     toolchain trap (`fortran-gotchas.md`); a test-design lesson (`testing.md`); a documented caller
     contract (the guide page); a contributor-only or unreachable-today trap (a comment at the site).
- **Claude never adds an entry.** Propose the text with the four answers; the maintainer decides.
- **At most 15 entries, each body at most 15 lines**: what breaks, why no test, what would close it,
  what it forbids. A sixteenth closes or replaces one. `check_risk_register_shape` enforces the shape
  and the next-number line.
- **Covering a risk deletes its entry in the same change**, after its "must not" moves into a
  comment at the site or a rule file; the change rewrites every citation of the number in
  `.claude/`, CLAUDE.md, CONTRIBUTING.md, `.gitlab-ci.yml` and lint messages to name the test.
- **`Risk-N` numbers are never reused**: a new entry takes the `Next number:` line's value and
  increments it. Cite a number only for an open entry.

## Scripted edits to documents

- **Prefer `Edit` over a hand-rolled splice**; it fails on a non-unique anchor.
- A script that replaces a range must assert each marker occurs exactly once, that `end > start`,
  and print what it discards (`s[:start] + new + s[end:]` with `end < start` silently duplicates
  text).
- A script that validates several replacements and writes once at the end loses the earlier edits
  when a late assertion fails: write after each replacement, or verify afterwards.
- **After any scripted edit to a structured document, re-derive its structure**:
  `grep "^#" file | sort | uniq -d` and a ToC-versus-headings cross-check. For a `doc/pages/` page,
  verify by rendering (`ford docs.md`, then grep `ford-doc/page/<group>/<name>.html` for one
  distinctive phrase per edit, whitespace collapsed on both sides).
- **Copy a `feature_*.md` before a scripted rewrite** — it has no history. Session transcripts
  (`~/.claude/projects/<project>/*.jsonl`) are the only other copy.
