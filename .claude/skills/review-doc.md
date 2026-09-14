---
name: review-doc
description: "Initialise a documentation review campaign over everything under CHANGELOG.md's [Unreleased] section. Writes feature_doc.md (boundary, page map, tiers, order, report aliases, baseline, opening questions) and stops. Reviews no page."
argument-hint: "[boundary tag or commit]"
allowed-tools: Bash(git:*), Bash(grep:*), Bash(awk:*), Bash(sed:*), Bash(find:*), Bash(ls:*), Bash(wc:*), Bash(cat:*), Bash(python3:*), Bash(ford:*), Bash(tools/:*)
disable-model-invocation: true
---

# /review-doc — initialise a documentation review

**This command initialises a review campaign and then stops. It reviews no page, edits no page and
commits nothing.** Part A is what to do now. Part B is the procedure the per-page review sessions
follow afterwards; it is reference material for them and must not be started here.

Injected context:

- Working tree: !`git status --short | head -20`
- CHANGELOG headings: !`grep -n "^## " CHANGELOG.md | head -3`
- Tags: !`git tag -l | tail -3`
- Existing campaign documents: !`ls feature_doc*.md 2>/dev/null || echo none`
- Optional boundary override given as the argument: $ARGUMENTS

This file's own links are not covered by `tools/check_doc_anchors.py` (it scans the repository root
and `doc/pages/` only).

## Contents

- [Part A — initialise the campaign](#part-a--initialise-the-campaign)
  - [A1 Guard](#a1-guard)
  - [A2 Fix the boundary](#a2-fix-the-boundary)
  - [A3 Derive the page list from three sweeps](#a3-derive-the-page-list-from-three-sweeps)
  - [A4 Sort the pages into four tiers](#a4-sort-the-pages-into-four-tiers)
  - [A5 Order](#a5-order)
  - [A6 Fix the report aliases](#a6-fix-the-report-aliases)
  - [A7 Record the baseline](#a7-record-the-baseline)
  - [A8 Release status](#a8-release-status)
  - [A9 Opening questions](#a9-opening-questions)
  - [A10 Write `feature_doc.md` and stop](#a10-write-feature_docmd-and-stop)
- [Part B — the review procedure](#part-b--the-review-procedure)
  - [B1 What a review is for, and the unit of work](#b1-what-a-review-is-for-and-the-unit-of-work)
  - [B2 The three-step loop](#b2-the-three-step-loop)
  - [B3 Step 1: the six passes](#b3-step-1-the-six-passes)
  - [B4 Closing step 1](#b4-closing-step-1)
  - [B5 The review report](#b5-the-review-report)
  - [B6 Step 2: how feedback is given](#b6-step-2-how-feedback-is-given)
  - [B7 Step 3: acting on feedback](#b7-step-3-acting-on-feedback)
  - [B8 What a review may edit](#b8-what-a-review-may-edit)
  - [B9 Editing a page safely](#b9-editing-a-page-safely)
  - [B10 Checks to run](#b10-checks-to-run)
  - [B11 Tracking progress](#b11-tracking-progress)
  - [B12 Where a convention belongs](#b12-where-a-convention-belongs)
  - [B13 Settings analysis](#b13-settings-analysis)
  - [B14 Closing the campaign](#b14-closing-the-campaign)

## Part A — initialise the campaign

The page list is the one part of a campaign nothing downstream can correct: a wrong finding is caught
in step 2, a missed test by Pass 3 on a later page, but a page nobody scheduled is never reviewed and
no check reports it. Build it carefully, from the source, and hand it to the maintainer before any
review starts.

### A1 Guard

- If any `feature_doc*.md` exists, stop and report which. Never overwrite, rename or delete it
  (`.claude/rules/workflow.md`); the maintainer decides whether to resume it or name a new document.
- Record whether the working tree is clean. An unclean tree is reported, not cleaned.

### A2 Fix the boundary

- **Name a commit, not a feeling.** The boundary is the argument if one was given; otherwise the
  newest published CHANGELOG section — the first `## [` heading after `[Unreleased]`
  (`awk '/^## \[/{n++; if(n==2){print; exit}}' CHANGELOG.md`). Resolve it to a commit: the tag
  named `<ver>` or `v<ver>` if one exists (`git tag -l`), otherwise the commit that introduced the
  heading (`git log --format='%h %ad' --date=short -S'## [<ver>]' -- CHANGELOG.md | tail -1`).
  Record the tag or commit, its date and which method resolved it.
- The campaign is `<boundary>..HEAD`. Quantify it in one table: commits, new `src/` files, new
  pages, modified pages, inserted documentation lines (`git log --oneline`,
  `git diff --name-status`, `git diff --stat -- doc/`).

### A3 Derive the page list from three sweeps

A documentation diff finds the pages someone remembered to update, never the ones they did not, so
build the list from three independent sweeps and expect them to disagree:

1. `git diff --name-status <boundary>..HEAD -- doc/ README.md CONTRIBUTING.md`, split into added
   and modified.
2. **The `[Unreleased]` section, read as an inventory of what has to be documented.** Every bullet
   is a claim that something exists: name the page or pages that carry it. A claim with no page is a
   finding before the campaign has started.
3. **The new public surface, from the source**: new `src/*.f90` files, new `public ::` names and
   type-bound bindings (`git diff <boundary>..HEAD -- src/ | grep -E '^\+.*(public ::|procedure ::|generic ::)'`),
   new rows in `doc/pages/operating/choosing-a-module.md`. This sweep finds a feature nobody wrote a
   changelog entry for.

Then ask of every page the sweeps did not return: what does the new surface add to this page's own
topic? Count mentions — a page whose subject was extended and that mentions the extension zero times
is a gap with its evidence attached.

**Express the result as a map from body of work to page**: one row per `[Unreleased]` entry or
feature area, naming its modules and the page or pages that carry it. An empty page cell is a
finding. Then state the complement: pages not scheduled and why (subject unchanged since the
boundary, an audit deferred, a tracked document out of bounds), saying that out of scope means
**not scheduled**, not untouchable — a wrong claim met on an unscheduled page is still fixed under
B8 and reported.

### A4 Sort the pages into four tiers

| tier | what it is | how it is reviewed |
|---|---|---|
| **new pages** | did not exist at the boundary | the full six passes over the whole page; usually the only description of their subject |
| **extended pages** | existed, since given new material | scoped to what is new plus whatever it makes wrong elsewhere on the page; Pass 0 still reads the whole page |
| **coverage-gap candidates** | untouched, but their subject was extended | one question: what does the new surface add here? "Nothing" is a legitimate outcome and is recorded |
| **index and landing pages** | group indexes, the guide landing page, the front page | last (B11); every bullet re-derived from its page's current opening paragraph |

- State each page's tier beside it, with the mention count as evidence for the third tier. The
  candidate tier is a proposal the maintainer trims; order it worst-first.
- "Modified" means modified substantively: check each modified page's diff and say so; if nothing
  was excluded, say that too.
- Give each page its line count; it sizes the work and is what stops Pass 3 being skipped.

### A5 Order

- Default: the guide's reading order — the `ordered_subpage:` frontmatter of `doc/pages/index.md`
  and of each group `index.md`. Terminology settles from the front.
- A campaign whose new material introduces a vocabulary other pages borrow may review the group
  that owns it first, then keep reading order within every tier. **Propose the deviation as an
  opening question, with its cost** (the reviewer no longer meets the guide in reading order).
- Index pages come after every content page whenever most groups have a page in the campaign; a
  group's index cannot be right before its pages are settled. The front page comes last of all.
- Skipped pages keep empty cells; an empty cell only ever means "not yet reviewed".

### A6 Fix the report aliases

- One report file per unit in flight, assigned now so two sessions cannot pick different names:
  `feature_doc_<group>_<page>.md` for `doc/pages/<group>/<page>.md`, `feature_doc_<group>_index.md`
  for a group index, `feature_doc_index.md` for the guide landing page, `feature_doc_readme.md` for
  the front page. The `feature_` prefix keeps them git-ignored by construction.
- A group of pages taken as one unit (B1) gets one alias named for the subject; list the per-page
  aliases it replaces as **struck**, so a later session does not read them as unreviewed pages.
- Cross-check mechanically: one alias per unit, no duplicates, every page in the tier tables present
  in the order table and vice versa.

### A7 Record the baseline

Run the B10 subset that applies before any review, and record: the baseline commit and whether the
tree is clean; the machine and toolchain (`tools/machine_report.sh`); each check's result, with its
count where it prints one; the guide's size (content pages and index pages, from
`find doc/pages -name '*.md'`); and which checks were not run and why (a campaign adding no page
needs no docs build for its baseline). A failure appearing on page 20 must belong to page 20.

### A8 Release status

- Everything under `[Unreleased]` is unreleased: no tag, no user code, no semantic-versioning
  promise. Findings of the form "this API is awkward, inconsistent with its siblings, misnamed,
  takes its arguments in the wrong order, or should not exist" are actionable recommendations
  (renames, kind/order/optionality/default changes, adding/removing/merging/splitting procedures,
  behaviour changes, dropping a feature) and must be reported, never swallowed.
- **A released surface on the same page is not unreleased.** A shipped procedure that gained an
  optional argument is two things: the argument is open, the procedure is not. A recommendation
  says which of the two it is.
- Write the answer into `feature_doc.md`; a report written under the wrong assumption leaves no
  trace of the findings it never raised.

### A9 Opening questions

Numbered, each with a recommendation and its reasoning, answered by a `Comment:` line underneath
(B6). Say which questions block the first review — typically the order and the scope of the
candidate tier. Two standing questions belong in every campaign:

- Is any part of the surface in scope settled, and not to be relitigated?
- Does finishing the campaign gate anything (a release, a tag, a hand-off), and is the candidate
  tier inside that gate?

### A10 Write `feature_doc.md` and stop

Write `feature_doc.md` in the repository root, self-explaining without this session
(`.claude/rules/workflow.md`), with these sections in this order:

1. Boundary and size (A2)
2. Release status (A8)
3. Page map: entry → modules → pages (A3), and the out-of-scope complement
4. Tiers (A4)
5. Order and progress table (A5, B11)
6. Report aliases (A6)
7. Baseline (A7)
8. Standing decisions — empty at start (B12)
9. Opening questions (A9)
10. Campaign outcome — filled at close (B14)

Settings analysis: this command and the reviews it initialises introduce no process-global
parameter (B13); say so in the document. Then report to the maintainer: the size table, the tiers,
the blocking questions, and that no page was reviewed. Do not start step 1 of any page.

## Part B — the review procedure

Used by every per-page session after the campaign is initialised. It names no page and records no
review; the page list and progress live in `feature_doc.md`, each page's findings in its report.

### B1 What a review is for, and the unit of work

- A review asks four questions of one page, in order: is it true, complete, guaranteed, readable.
  The third makes the others durable: a documented behaviour with no test behind it is a claim the
  suite lets anyone break silently.
- **The unit is one page, reviewed the same way every time, and only one unit is in flight.** Do not
  start step 1 on the next page while step 2 on the current one is outstanding; feedback on one page
  routinely sets the convention the next is reviewed under.
- The unit may be a **group** when findings are only visible between pages (pages cross-describing
  one subject; index pages that describe other pages). Sharing a directory is not a reason. Propose a
  group; the maintainer decides. A group is one unit: one report, one closure, its struck aliases
  named (A6).

### B2 The three-step loop

Starting a session: take the first page whose step 1 is not done. A page with step 1 recorded and
step 3 not is mid-loop and nothing new starts until it closes. Read its existing report first. Read
B8 before the first edit.

1. **Reviewer reviews the page against the implemented features**: verify every claim against the
   code, fill gaps, apply the small documentation-only fixes B8 pre-authorises, report everything
   larger and anything that would change what the library does, and **propose a test** for every
   documented behaviour nothing in the suite guarantees. Findings go into the page's report (B5).
2. **The maintainer reads the page and the report** and answers in it (B6).
3. **The reviewer acts on the feedback** (B7).

Steps 2 and 3 repeat until a round adds no new feedback; expect a second round.

- "All three steps done" is not closed: a page reopens whenever new feedback appears. The report
  file is the authority on status; the progress table is an index over it. Record rounds
  distinguishably (`(r1)`, `(r2)`).
- A page closes when a round adds no new feedback. Say so in the report.
- A finished round sits uncommitted (`.claude/rules/workflow.md`). Before starting the next page,
  say plainly that the previous round is uncommitted, or the two become one indivisible diff.
- A round-2 comment can turn the page review into a feature request. That is a normal feature task
  with its own design write-up, settings analysis, tests and changelog handling; do not fold it into
  the page's step 3.

### B3 Step 1: the six passes

Run all six, in this order, for every page.

**Pass 0 — establish the source of truth, from the source.**

- Read the whole page first, frontmatter included.
- Build the list of what the page should cover from the code, never from the page: the `public ::`
  lines, each type's `contains` bindings, the generic interfaces and their specifics. For a
  generated family, read the generator's table and template and verify one representative per
  family; what a page gets wrong there is the kind list, the argument shape, or the excluded kinds.
- Note which examples are mirrored in the test suite (grep the test tree for the page name; an
  example about process-global state is mirrored in a suite excluded from concurrent execution, not
  in the examples suite). Editing a mirrored example is editing a test: both sides or neither.
- Note the page's inbound links across every file type that can carry one, not only `.md`; a
  renamed heading is likely someone's link target and fails the lint stage.

**Pass 1 — accuracy.** Every claim is checked against the source:

- signatures (names, order, optionality, intents, types, kinds; a page showing one kind of an
  argument that accepts two is inaccurate by omission);
- every stated default and every "if you omit this, X happens";
- every claimed abort: locate it, confirm the message text and the triggering condition;
- every threshold, ceiling, ordering guarantee, thread-safety claim, null/NaN behaviour and
  performance statement;
- every constant, kind name, setting name, environment variable and grammar operator;
- **every written-out count and hand-written enumeration, checked against the thing it counts** —
  the highest-yield item in the pass, and a recently edited page is more suspect, not less;
- anything unverifiable from source is flagged, never restated.

Where a lint check already validates a claim, lean on it and say so in the report; the maintainer
must be able to tell which claims were verified against source, which against a check, and which
not at all. When a re-derivation finds a drifted enumeration, propose a check that reads its
source (writing it is a code change, B8). A claim whose subject is the compiler, runtime, OS or
linker (an exit status, a runtime message, a flag's effect) cannot be verified on one compiler:
state the property the library guarantees, or check the fleet before writing the value down. When
reading cannot settle a claim, run it in a throwaway project outside the repository depending on
this one by **relative** path, in a scratch directory, and verify the fix the same way.

**Pass 2 — coverage.** What is missing and what is stale:

- The test is what a working program needs, not what is public. Introspection accessors, capacity
  queries and bindings that satisfy an abstract interface are documented by their doc-comment and
  the generated reference. List what you excluded and why, in one line each.
- Every public name that passes that test is reachable from somewhere in the guide; list anything
  with no narrative coverage and say where it belongs.
- Remove or rewrite anything the page documents that no longer exists.
- Anything the code enforces that a reader would only discover by hitting it (a guard, a
  cannot-be-called-twice, a detach, an invalidated pointer, a required call order) goes on the page
  before the reader hits it.
- The page must not duplicate the generated per-procedure reference; a hand-maintained argument
  table is a defect. A signature on a page is the call form a reader types, optional arguments in
  brackets, kinds mentioned in a clause; a signature shown because the procedure exists is
  duplication.

**Pass 3 — tests.** Pass 1 asks whether a claim is true today; this pass asks what stops it becoming
false tomorrow. Apply it where a breakage would be silent or surprising: a guarantee not obvious from
the code (ordering, null/NaN convention, tie-break, trimmed-versus-not asymmetry, an interaction
between features); a stated default; a documented abort (condition and message); a limit or
threshold; an invariant across an operation; a concurrency promise; a claimed absence ("nothing here
validates, aborts or prints"), whose test is usually a lint check.

- For each such claim find the test or establish there is none. A documented abort needs the
  out-of-process scenario harness (`.claude/rules/testing.md`). Never conclude "uncovered" from a
  search truncated with `head`; count first. A test that merely touches the code is not a test of
  the claim: ask **which named test would fail if exactly the documented behaviour broke**.
- An uncovered claim gets a **proposed** test, never a written one: the claim quoted with its source
  location; the file and whether in-process or scenario; what it asserts in one sentence **and its
  negative control**; the fixture, with its own filename.

**Pass 4 — conventions.** Check the page against `.claude/rules/documentation.md`; the conventions
live there, not here. Four items are about reviewing rather than the convention:

- **A page describes the current state, never a former one.** The review loop manufactures this
  defect (a fix updates the page with a sentence saying what changed), so a recently reviewed page
  is not settled on it. Run every grep in `documentation.md`'s current-state rule; read each hit and
  never replace mechanically. Most are innocent (a state after an operation, a guarantee); not
  innocent is a sentence a reader cannot evaluate without knowing a release they never used.
- Performance figures are approximate and machine-free; contract numbers and parity claims stay
  exact (the rule and its test are in `documentation.md`).
- A page that accreted edits without a rewrap has lines of wildly uneven length and enumerations
  broken mid-item; no check sees it. Rewrap on the round that first reviews a page, never on a
  follow-up, and prove the rewrap whitespace-only (B9).
- Spelling is left alone except inside a paragraph rewritten for another reason. Terminology
  precision is load-bearing: check the page's use of the guide's distinguishing terms against the
  rest of the guide.

**Pass 5 — the reader.** Read as someone who does not know the library: does the opening say what
the page is for and for whom in two sentences; is there a complete runnable example early; does the
order follow what a reader does rather than how the code is organised; is anything another page's
topic (propose the move; do not move it if it has inbound links); is anything unreadably long
without a heading to land on. **Before fixing a wrong phrase, grep the whole guide for it**: the
same sentence is usually repeated in index bullets, openings and "see also" pointers, and fixing
one occurrence leaves two pages disagreeing, which reads as a real distinction.

### B4 Closing step 1

Apply the fixes B8 pre-authorises and propose the rest; update index entries in the same edit if the
title or headings changed; run the applicable B10 checks; render once if the page gained a table, a
nested list or an unusual fence; write the report (B5); record the step-1 date. Three limits hold for
every review:

- a change to what the library does is report-only, however small (a doc-comment describing a
  behaviour may be corrected; the behaviour may not);
- no restructuring of the guide (no new, moved or split page) inside a page review; propose it;
- documentation-only changes get no changelog entry; a code fix a review uncovers gets its entry
  when it is made.

### B5 The review report

One file per unit, in the repository root, under its alias (A6). It is git-ignored scratch with no
recovery path: self-explaining without the conversation (page and commit at the top, decisions quoted
verbatim), edited with per-file edits rather than scripted rewrites, and structure-checked afterwards
(`grep "^#" <report>.md | sort | uniq -d` must be empty; an anchored append after a heading re-emits
that heading). Its links are checked: use the repository-relative Markdown form, and describe a link
recommended for a page in prose (text and target) rather than writing it, since a code span is no
escape.

Report every issue with a recommended fix, including the ones already applied. Structure, in order:

1. **Header** — page, line count, date, commit reviewed against, one sentence on the page's subject
   and audience as the review understood it, a status line (round; committed or in the tree), the
   machine any figure came from (figures from two machines are not comparable), and **one line per
   pass** saying what it covered or that it found nothing — the pass most often skipped is Pass 3.
2. **Changed** — one line each.
3. **Gaps filled** — with the source location each was verified from.
4. **Issues found and not fixed** — issue, evidence, recommended fix, why not applied; ordered
   worst-first and tagged *wrong* / *missing* / *unclear* / *cosmetic*, so reading can stop after
   the *wrong* entries.
5. **Looks like a code issue** — evidence and recommended fix, not acted on; its reach is set by
   A8.
6. **Open questions** — numbered, each with a recommendation and reasoning; a bare question is a
   decision handed back. Questions step 3 raises are appended here with what was done meanwhile.
7. **Deferred to the future** — always last: every action identified and not carried out, each
   typed (test to write, with its negative control; code change, one line; documentation work too
   large to apply), cross-referenced to its finding rather than repeating the evidence. An item
   done is moved out, naming the test or commit; an item declined is deleted with the reason.

A page through more than one round gains, before "Deferred to the future": **the agreed plan** (one
heading per item, each decision quoted against the item it decides, deliverables per item, build and
machine notes, an explicit "nothing here is waiting on an answer") and an **`Outcome:` paragraph per
item** written when built, saying whether it deviated from the plan.

An empty section says so in one word. Keep the report readable in one sitting. Keep it after step 3,
updated with what the maintainer decided. Its outstanding items stay in it when the page or the
campaign closes; they are not copied into `feature_risks.md`, an issue tracker or any other tracked
document, and the report is not deleted to tidy up.

### B6 Step 2: how feedback is given

Feedback is written into the report as lines beginning `Comment:`, directly below the entry it
answers, in any section. Before acting, `grep -n "Comment:" <report>.md` and read every hit. A bare
`Comment: approved` approves that entry's own recommended fix, which is why every entry carries one.

### B7 Step 3: acting on feedback

- **Documentation feedback**: apply, re-run the checks, no confirmation round. Feedback rejecting an
  edit step 1 applied: revert it and record it as declined with the reason.
- **Feedback changing a code feature**: a normal feature request under every standing rule (tests,
  scenarios for aborts, doc-comments, guide page, changelog; no commit on `main`). After a
  behaviour-widening change run the whole suite: tests far from the page relied on the old
  refusal, and each failure asks which fixture did; repoint them and add a test pinning the new
  behaviour. A narrowing change fails tests that are right to fail: repoint the fixture and correct
  the doc-comment that advertised the old behaviour.
- **An approved test**: a normal code task; write the negative control the report named; verify it
  by breaking the documented behaviour and confirming the test fails (checking the exit status, since
  a mutation often aborts rather than fails); no changelog entry; move the item out of "Deferred".
- **A comment's literal scope may be narrower than the real occurrence set.** Apply the decision's
  reasoning to everything it fits, list every extra site in "Changed" saying it is wider than the
  comment's words, and never widen or decline to widen silently. "Check and fix if necessary" is an
  instruction to audit the rule, not the listed instances; when the audit's count exceeds the
  report's, say the number in the reply.
- Update the report in the same pass: fold each answer into the question it resolves, quoting the
  maintainer; mark fixed issues; record declined ones. A later instruction overriding an answer is
  recorded beside it (date, new decision, reason), never rewritten over it.

### B8 What a review may edit

Pre-authorised: **small, obvious, documentation-only fixes anywhere in the repository.** Small and
obvious means the maintainer can verify the change from the report line alone in step 2 (a typo, a
broken link, a wrong argument name, a stale default, a renamed procedure, a sentence the source
contradicts); anything larger is proposed. Documentation-only means nothing that changes what the
library does. Neither bound moves for an unreleased surface (A8).

| target | applied directly |
|---|---|
| the page under review | any doc-only fix within the two bounds |
| its index entries | the bullet and its description, when the title or scope changed |
| other guide pages | any doc-only fix within the bounds, wherever a wrong claim is found; record the file and list in "Deferred" what was NOT checked on that page |
| README | an API-overview entry, a stale link into the guide, a claim the source contradicts |
| CONTRIBUTING.md, CHANGELOG.md, CLAUDE.md and `.claude/rules/` | a stale link or anchor. A published changelog section: a dead link only. `[Unreleased]`: a factual correction (an argument the code lacks, a default the source contradicts); restructuring it or moving entries between groups is proposed |
| `feature_risks.md` | a stale link or anchor only; never an entry, a number or a deletion |
| source doc-comments | a wrong or stale doc-comment within the bounds |
| generated sources | the same fix in the generator's template, regenerated, never in the emitted file |
| tests | only a mirrored example and its assertions, together with the page; a new test is proposed |
| scratch planning files | a broken link or anchor only; anything else needs approval |
| anything else in source, tests, tooling, build or CI | nothing; report it |

Mirrored examples are both-or-neither: fix page and test in one edit and run that suite; if the
example cannot be made right without changing the library, change neither and report. Every standing
project rule applies to an edit made here.

### B9 Editing a page safely

- Take a copy of the page before a scripted pass.
- A script validating several replacements and writing once loses the earlier ones on a late
  failure: write after each replacement, or verify afterwards (`.claude/rules/workflow.md`).
- A rewrap is provably whitespace-only: collapse all whitespace on both sides and refuse to write
  unless byte-identical (token-list comparison misreports a code span that spanned a line break).
  Test the *stripped* line for a table delimiter (an indented table under a list item is still a
  table), and never reattach punctuation inside an inline code span; diff the word stream and read
  every hunk.
- After any scripted or multi-part edit, render and grep the generated HTML for one distinctive
  phrase per edit with whitespace collapsed. Probe for the new text, never a newer timestamp; a
  MISSING means re-render first. Verify a code block structurally (identifiers inside a
  preformatted element, no literal fence marker anywhere in the output), not by its text. Count
  `<li>` in the HTML against list markers in the source after restructuring a list.
- No unanchored search-and-replace for link rewrites: per-file edits, or a script asserting an
  expected match count per file.

### B10 Checks to run

Run the applicable subset before starting as well as after; a check already failing is not evidence
about your edit. A structural change (page added, moved, split, renamed) runs the full set with a
docs build; a page-content review runs the rest. Silent failures first:

- [ ] structural only: rendered page count `find ford-doc/page -name '*.html' | wc -l` matches
      content plus index pages (a group without `index.md` is silently skipped).
- [ ] structural only: `tools/fix_ford_page_links.sh ford-doc` reports a non-zero rewrite count,
      with one nested README link spot-checked in the HTML (its failure is a silent no-op).
- [ ] `python3 tools/check_doc_anchors.py` exits 0.
- [ ] `python3 tools/check_source_conventions.py` exits 0. A new failure may be the check's fault
      (a shape so generic that adding a table trips it); read what it matches before editing the
      page to satisfy it, and if over-broad, anchor it on something distinctive.
- [ ] `ford docs.md` runs clean (the Graphviz warning is environment-only).
- [ ] if a `src/*.f90` doc-comment changed: the generator's `--check` exits 0 and `fpm build`
      passes (cpp runs over every source; `/*` or a trailing `\` in a comment fails on the wrong
      line); and `ford --warn docs.md`'s `Unknown entity` count is unchanged, re-derived never
      hardcoded (`documentation.md` has the command).
- [ ] if a mirrored example changed: `fpm test run_tester -- examples`, or the suite that mirrors
      it.
- [ ] `grep -rn 'doc/pages/' --include='*.md' --include='*.f90' --include='*.py' --include='*.sh' .`
      returns nothing stale; the anchor checker parses Markdown only.

### B11 Tracking progress

The progress table lives in `feature_doc.md` (A10, section 5), never here:

| column | purpose |
|---|---|
| order | the review order |
| tier | from A4 |
| page | the page under review, with its line count |
| report file | its alias (A6) |
| step 1 / step 2 / step 3 | the date each completed, round appended where more than one |

The table is an index; the report is the authority (it cannot express a second round or an
uncommitted finished round). Reading order is the default and the maintainer may reorder or skip;
each group's index page is reviewed last within its group, the front page last of all. When reviewing
an index page, re-derive every bullet from its page's current opening paragraph rather than reading
it for plausibility: the consistency check validates link targets and order and discards the prose.

### B12 Where a convention belongs

Feedback establishing a general convention is recorded as part of acting on it, so the remaining
pages are reviewed under the same rule. Test: would someone writing a new page next year need it?

- **Durable convention** (yes): `.claude/rules/documentation.md`.
- **Procedural rule** (how the loop runs, what a report holds, what a review may edit): this file.
- **A decision the campaign settles part-way through** (an answer on page 15 governing pages 16
  onward): the standing-decisions section of `feature_doc.md`, one heading per rule, quoting the
  instruction verbatim and naming its durable home or "nothing enforces it yet". Decisions, not work
  items; a rule with no enforcing check is worth proposing one for.

### B13 Settings analysis

Neither this command nor a review conducted under it introduces a process-global parameter: no
source, no interop surface, no `parquet_settings` knob, no environment variable. A code change that
a review spawns is a separate task carrying its own settings analysis. Whether the feature under
review got its own knobs right is a Pass 2 question: a module that threads but exposes no thread cap
while a sibling does, or one configured from its own environment reader rather than
`parquet_settings`, may be deliberate — confirm it, against the admission test in
`.claude/rules/api-conventions.md`.

### B14 Closing the campaign

- Run B10's full list once, docs build, page count and guide-path grep included, and record the
  result in `feature_doc.md`.
- Outstanding items stay in each page's report; nothing is consolidated into a tracked document,
  the risk register or an issue tracker, and no report is deleted.
- Record what the campaign produced beyond the page edits — checks written, library changes,
  standing decisions — in `feature_doc.md`'s outcome section, and name in one sentence the defect
  the campaign kept re-finding.
