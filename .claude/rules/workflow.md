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
- A source comment citing a planning document that is no longer present is attribution and stays.

## The `feature_risks.md` standing-risks register

Tracked register of properties a future change can break **silently** (no test failure, no abort).
Read it before editing an area.

- **Admission: MAJOR silent failures only** — a wrong answer, lost/corrupted data, a stale pointer,
  a hang, a broken frozen contract. Not a cost, not a loud failure, not a property of the test
  harness, build or benchmark method (those belong in `.claude/rules/`). Move a rejected entry's
  useful check or constant into the code rather than dropping it.
- **`Risk-N` numbers are permanent**: numbered upward across the whole file, never renumbered,
  never reused after deletion.
- **Four sections**: 1 New risks (healthy state: empty), 2 Proposed testing scenario, 3 Not
  testable, 4 Covered, kept for what they still forbid. A new risk takes the next number and goes in
  section 1.
- **Section 4 is pruned, not archived**: an entry stays only while it still forbids something; a
  "works and is tested" entry is deleted. Prune whenever you are in the file.
- **Check a verdict against the suite, never infer it**: grep the tests before marking "proposed";
  name the test before marking "covered".
- **Implementing a proposed test updates the entry in the same change** (move to section 4 or
  delete; name the test).
- **A newly discovered silent-failure property gets a new `Risk-N` in section 1**, not only a code
  comment.

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
