# The documentation review process

**A repeatable procedure for reviewing the user guide, one page at a time.** It defines the loop, the
passes a review runs, the report it produces, what a review may change, and how progress is tracked.

**It names no page and records no review.** Nothing here goes stale when a page is renamed, split or
finished, and nothing has to be edited when a review campaign ends. The current layout of the guide,
the inventory of pages and the record of which have been reviewed live elsewhere — see
[§10](#10-tracking-progress) for what a campaign keeps and where.

The lessons below were paid for: most rules exist because a real review went wrong in that exact way,
and where the cost is instructive the numbers are kept ("fourteen hits, three real") without naming
the page. A rule with a measurement attached is a rule someone has already tried to skip.

**This document is tracked, and is specific to this repository.** It lives under `tools/` rather than
in the repository root because it is maintainer tooling in prose: a procedure someone runs, alongside
the scripts [§9](#9-checks-to-run) names. It is deliberately not a `feature_*.md` scratch file — those
are git-ignored, and a process meant to govern reviews for as long as the guide exists should not be
one `git clean -X` from gone, nor invisible to a contributor who never opens a planning document.

Two consequences of that placement are worth knowing. Its links and anchors are **not** currently
covered by `tools/check_doc_anchors.py`, which scans the repository root and `doc/pages/` only —
adding `tools/*.md` to its glob would restore that. And because it is tracked, every change to it
arrives as a reviewable diff, which is the main thing being bought.

It is written to be read cold: no knowledge of the session that produced it is assumed.

## Contents

- [1. What a documentation review is for](#1-what-a-documentation-review-is-for)
- [2. The three-step loop](#2-the-three-step-loop)
- [3. Step 1: reviewing one page](#3-step-1-reviewing-one-page)
  - [3.1 Pass 0 — establish the source of truth, from the source](#31-pass-0--establish-the-source-of-truth-from-the-source)
  - [3.2 Pass 1 — accuracy](#32-pass-1--accuracy)
  - [3.3 Pass 2 — coverage](#33-pass-2--coverage)
  - [3.4 Pass 3 — tests](#34-pass-3--tests)
  - [3.5 Pass 4 — conventions](#35-pass-4--conventions)
  - [3.6 Pass 5 — the reader](#36-pass-5--the-reader)
  - [3.7 Closing step 1: apply, verify, report](#37-closing-step-1-apply-verify-report)
- [4. The review report](#4-the-review-report)
- [5. Step 2: how feedback is given](#5-step-2-how-feedback-is-given)
- [6. Step 3: acting on feedback](#6-step-3-acting-on-feedback)
- [7. What a review may edit](#7-what-a-review-may-edit)
- [8. Editing a page safely](#8-editing-a-page-safely)
- [9. Checks to run](#9-checks-to-run)
- [10. Tracking progress](#10-tracking-progress)
- [11. Where a convention belongs](#11-where-a-convention-belongs)
- [12. Settings analysis](#12-settings-analysis)
- [13. Scoping a campaign](#13-scoping-a-campaign)
- [14. Release status decides what a review may recommend](#14-release-status-decides-what-a-review-may-recommend)

## 1. What a documentation review is for

A review asks four questions of one page, in this order: **is it true, is it complete, is it
guaranteed, and is it readable.** The third is the one that is easy to leave out and the one that
makes the other three durable — a documented behaviour with no test behind it is a claim the suite
will let anyone break silently.

**The unit of work is one page, reviewed the same way every time**, so that the last page is held to
the same standard as the first and the maintainer's reading effort is spread out rather than spent in
one sitting.

**Only one page is in flight at a time.** Do not start step 1 on the next page while step 2 on the
current one is outstanding: the maintainer's feedback on one page routinely establishes a convention
that changes how the next page should be reviewed, and reviewing two pages under two different
conventions wastes one of them.

**The unit can be a GROUP of pages, and the test is whether the pages cross-describe each other.**
One campaign took four of its 33 rows as groups on the maintainer's instruction, so treat one page
as the *default* rather than the rule. Grouping fits when a finding is only visible **between**
pages: six pages describing one subject from six angles produced three findings that were two of
those pages disagreeing with each other, and a set of index pages — whose only content is a
description of another page — produced the same stale group summary in two places at once. Neither
is visible from inside a single page. Grouping does **not** fit merely because pages share a
directory: asked whether six pages covering six unrelated modules should be grouped the same way,
the same maintainer answered *"no, [they] are reviewed one at a time"*. **Propose it and let the
maintainer decide**; do not assume either way.

**When a group is taken, nothing else in this document changes.** The group is one unit in flight,
it produces one report, and it closes as one. Say explicitly that the per-page report aliases it
replaces are **struck** ([§13.4](#134-fix-the-report-aliases-up-front)) and that no file was ever
created under any of them — otherwise a later session reads those unused aliases as pages nobody
reviewed.

## 2. The three-step loop

**Starting a campaign:** before any of this, build the page list, fix the report aliases and
record the baseline — [§13](#13-scoping-a-campaign). A campaign whose first review starts before
its scope is agreed reviews whatever page happened to look urgent.

**Starting a session:** take the first page whose step 1 is not yet done. If a page's step 1 is
recorded but step 3 is not, that page is mid-loop and nothing new starts until it closes. Read that
page's existing report first if one exists — it is the record of what was already decided. Read
[§7](#7-what-a-review-may-edit) before the first edit, not after.

1. **Reviewer reviews the page against the implemented features.** Verify that everything the page
   claims is true of the code as it stands, and fill any gaps found. Small, obvious
   documentation-only fixes are applied directly ([§7](#7-what-a-review-may-edit)); anything larger,
   and anything that would change what the library does, is reported rather than changed. Where the
   page documents a behaviour that nothing in the test suite guarantees, the review **proposes the
   test** — suggesting those tests is part of the review, not a follow-up to it. Findings go into a
   per-page report ([§4](#4-the-review-report)).
2. **The maintainer reads the page and the report** and gives feedback: how the documentation should
   change, what should change in the code itself, which proposed tests to write, and answers to the
   report's open questions ([§5](#5-step-2-how-feedback-is-given)).
3. **The reviewer acts on that feedback** ([§6](#6-step-3-acting-on-feedback)).

### The loop iterates

**Steps 2 and 3 repeat until a round produces no new feedback.** The three steps are one *round*, not
the whole life of a page. Expect a second round rather than treating it as an exception: a round-2
comment is usually a reaction to the page as it now reads, which is exactly what step 3 was for.

Four consequences, and the first will mislead a fresh session:

- **"All three steps done" does not mean closed.** A page reopens whenever new feedback appears.
  **The report file is the authority** on a page's status; any progress table is a coarse index over
  it. Record a second round distinguishably (`(r1)`, `(r2)`).
- **A page closes when a round adds no new feedback**, not when step 3 finishes. Say so explicitly in
  the report rather than leaving it inferred from silence.
- **A finished round sits uncommitted**, because committing on the default branch is forbidden
  ([§6](#6-step-3-acting-on-feedback)). A page can therefore be closed and still have unreviewed work
  in the tree. Before starting the next page, say plainly that the previous round is uncommitted —
  otherwise the next review's edits land on top of it and the two become one indivisible diff.
- **A round-2 comment can turn the page review into a feature request, and that is not a
  digression.** A page that finally states what the code does plainly is exactly what makes a reader
  notice the code is wrong. One review produced no documentation feedback at all in its second round
  and instead three changes to the library, which became eleven new tests and two CHANGELOG sections.
  Budget for it, and **do not fold it into the page's own step 3** — it is a normal feature task,
  wanting its own design write-up before any of it is built.

## 3. Step 1: reviewing one page

Follow all six passes, in this order, for every page. **The order matters**: the accuracy pass gives
the source knowledge the coverage and test passes need, and the reader pass is worthless before all
three.

### 3.1 Pass 0 — establish the source of truth, from the source

- **Read the whole page first**, top to bottom, including frontmatter.
- **Build the list of what the page should cover from the code, not from the page.** Deriving it from
  the page can only confirm what is already written and can never find an omission. Enumerate the
  public surface of the modules and types the page is about: the `public ::` lines, each type's
  `contains` bindings, the generic interfaces and their specifics.
- **For a generated family, the source of truth is the generator, not the emitted file.** Read the
  generator's own table and template text and verify **one representative per family**. Enumerating
  hundreds of emitted specifics is neither finishable nor informative — what a page gets wrong about a
  generated family is the kind list, the argument shape, or which kinds are excluded, and all three
  are visible in the generator.
- **Note which of the page's examples are mirrored in the test suite.** Those are asserted and must
  not drift; editing one is editing a test ([§7](#7-what-a-review-may-edit): both sides or neither).
  Find them by grepping the test tree for the page name rather than trusting a count — and note that
  an example whose subject is process-global state will be mirrored in whichever suite is *excluded*
  from concurrent execution, not in the general examples suite, so a single grep of the obvious file
  will miss it.
- **Note the page's inbound links, because a heading you are about to rename is likely to be someone's
  link target — and the search must be wider than the guide.** Anchor checking typically spans the
  README, contributor docs, the changelog, the standing instructions file and every root-level
  Markdown file including scratch planning documents, so a rename that misses one fails the lint
  stage. Grep for the page name across every file type that can carry a link, not just `.md`.

### 3.2 Pass 1 — accuracy

**Every claim on the page is checked against the source.**

- **Signatures** — name, argument names and order, which are optional, intents, types and kinds. A
  generic's specifics are part of this: a page showing only one kind of an argument that accepts two
  is inaccurate by omission.
- **Defaults** — every stated default, and every "if you omit this, X happens".
- **Failure modes** — every claimed abort. Locate it and confirm both the message text and the
  triggering condition. A page that says "this is an error" where the code silently ignores it, or
  the reverse, is the most damaging kind of inaccuracy.
- **Behaviour and limits** — every threshold, ceiling, ordering guarantee, thread-safety claim,
  null/NaN behaviour and performance statement. Find the code that implements it.
- **Named things** — constants, kind names, setting names, environment variables, grammar operators:
  confirm each exists and is spelled correctly.
- **Every written-out count and every hand-written enumeration**, checked against the thing it
  counts or lists rather than read for plausibility. This is the highest-yield item in Pass 1 and it
  is not close: over one campaign's last three rounds it was **eleven of one round's findings, four
  of the next and both of the last**, including an API-stability promise naming 16 of 19 entry
  modules with all three missing ones tabulated on the same page 130 lines below. The defect is
  manufactured by ordinary correct work — someone adds the nineteenth module and does not think of
  the sentence — so a *recently edited* page is more suspect, not less.
- **Anything that cannot be verified from source, do not restate.** Flag it in the report; an
  unverifiable claim carried forward becomes a claim nobody can ever check.

**Where a check already validates a claim, lean on it — and say in the report that you did.** Some
claims on a page are compared against the build by a lint check: a per-module file count against a
generated inventory, a promise against a list the tooling owns. Re-deriving those by hand is slower
and no more trustworthy. What the report must not do is leave the maintainer unable to tell which
claims were verified against the source, which against a check, and which not at all.

**When a re-derivation finds a drifted enumeration, propose a check that reads its source.**
Correcting the number is half the fix: nothing stops it drifting again the same afternoon, and the
whole suite stays green while it does. Five such checks came out of one campaign, each closing a
claim that had already gone stale. Propose it in the report — writing it is a code change and out of
scope ([§7](#7-what-a-review-may-edit)).

**A claim about processor-dependent behaviour cannot be verified on one compiler**, and verifying it
on the one to hand is how a page comes to state a local observation as a fact. One page was reviewed,
verified and closed carrying an `error stop`'s exit status and message wording — both of which are one
compiler's, with the standard fixing neither, and two tests hard-coded the same values and failed the
moment the suite ran under a second compiler, against a library behaving correctly. **The tell is a
claim whose subject is the compiler, the runtime, the operating system or the linker rather than this
library**: an exit status, a message a runtime emits, a backtrace, a flag's effect. State the
*property* the library guarantees rather than the value one toolchain produces, or check the fleet
before writing the value down.

**When reading the source cannot settle a claim, run it.** Some claims are about what several layers
do together, and tracing them by eye is both slow and the kind of thing that produces a confident
wrong answer. A throwaway project outside the repository, depending on this one by relative path,
settles them in minutes and can be discarded without touching the tree. Four things learned doing
this:

- **The dependency path must be relative** where the build tool rejects an absolute one with an error
  that reads like a missing file rather than a rejected form.
- **It is a real consumer of the library**, so it also checks what the public facade actually
  re-exports — which is a fact about the public surface worth knowing in its own right.
- **Put it in a scratch directory, never in the repository's own program or test directories.** Those
  are the repository's, and [§7](#7-what-a-review-may-edit) does not license editing either.
- **Verify the fix the same way, not only the defect.** Re-running a proposed fix through the probe is
  what makes a proposed test's assertions concrete rather than hopeful.

### 3.3 Pass 2 — coverage

**What is missing, and what is stale.**

- **The test is what a working program needs, not what is public.** Being callable does not earn a
  binding a place on a page: introspection accessors, capacity and storage queries, and bindings that
  exist to satisfy an abstract interface are public because Fortran has no finer visibility, not
  because a reader has to know about them. Ask instead whether a program someone actually writes
  would reach for it — to build the thing, to query it, to hand it on, or to avoid a trap. If yes it
  belongs somewhere in the guide; if no, its `!>` doc-comment and the generated reference are its
  documentation and that is enough. **List what you excluded and why**, in one line, so the decision
  is visible and the next reviewer does not re-find the same names and propose them again.
- Every public procedure and type that passes that test should be reachable from *somewhere* in the
  guide. List anything with no narrative coverage at all; it need not be on this page — say where it
  belongs.
- Anything the page documents that no longer exists, or has been superseded: remove or rewrite.
- **Anything the code enforces that a reader would only discover by hitting it** — a guard, a "cannot
  be called twice", a detach, an invalidated pointer, a required call order — belongs on the page
  *before* the reader hits it.
- **The page must not duplicate the generated per-procedure reference.** Its job is usage, sequence,
  rationale and gotchas. A hand-maintained per-procedure argument table on a page is a defect, not
  coverage: link to the generated reference instead.
- **What a signature on a page should look like**: the call form a reader types — name, the arguments
  they will actually pass, optional ones in square brackets — and stop. Types, kinds, intents and the
  full specific list belong to the generated reference. Where a generic accepts several kinds, say so
  in a clause rather than showing each specific. *A signature shown for a behaviour the prose then
  explains is coverage; a signature shown because the procedure exists is duplication.*

### 3.4 Pass 3 — tests

**Pass 1 asks whether a claim is true today. This pass asks whether anything stops it becoming false
tomorrow.** Verifying by hand in Pass 1 fixes it for one day.

**Not every sentence needs a test.** Apply this pass where a breakage would be *silent or surprising*:

- a guarantee **not obvious from reading the code** — an ordering rule, a null/NaN convention, a
  tie-break, a trimmed-versus-not asymmetry, a documented interaction between two features;
- a stated **default**, especially one living somewhere other than where a reader would look;
- a documented **abort** — its triggering condition and its message text;
- a **limit, threshold or ceiling** the page names;
- an **invariant across an operation** — a pointer detaching, a call that may not be made twice, a
  required call order, something that survives (or does not survive) a mutation;
- anything the page promises about **concurrency**;
- a claimed **absence** — "nothing in this module validates, aborts or prints", "this never
  reallocates", "no name of that kind appears anywhere in the guide". A negative is the easiest
  claim to leave uncovered, because no ordinary test asserts one and the thing that breaks it is an
  *addition* somewhere else entirely. Where it is a property of the source or of the guide rather
  than of a run, its test is a lint check rather than a unit test — propose it the same way.

**For each such claim, find the test or establish that there is none.**

- **A documented abort cannot be covered in process** — the abort kills the test runner. It needs the
  out-of-process scenario harness, which typically has three registration points that must agree.
- **Never conclude "uncovered" from a search that was truncated.** A grep piped through `head` cuts at
  whatever files the matches happen to fill it with. **An absent test and a truncated listing look
  identical**, and the mistake runs in the expensive direction: it proposes a test that already
  exists. On one page two claims looked untested and both were covered, one of them five times over.
  Count first, or re-run without the truncation.
- **A test that touches the code is not automatically a test of the claim.** It may reach the
  procedure incidentally, assert only a size or that nothing aborted, compare two calls into the same
  machinery, or take a fast path that never runs the code the claim is about — all of which pass
  against a broken implementation. The one-line test: **if I broke exactly the documented behaviour,
  which named test would fail?** If you cannot name it, treat the claim as uncovered.

**When a claim is uncovered: suggest a test, do not write one.** Writing it is a code change and out
of scope ([§7](#7-what-a-review-may-edit)). Make the suggestion concrete enough to act on without
re-deriving the review:

- the **claim**, quoted from the page, and the source location implementing it;
- **which file**, and whether it is an in-process test or an out-of-process scenario;
- **what it asserts**, in one sentence, and **its negative control** — the same observation under the
  other condition, showing the outcome differs. A test with no control passes just as happily against
  code that ignores the documented feature entirely, which is the failure mode this pass exists to
  prevent;
- the **fixture** it needs, and that it gets its own filename — tests in a suite run concurrently, and
  two sharing a fixture path is a documented source of intermittent failure.

**If breaking the claim would be silent — a wrong answer, a stale pointer, no abort at all — propose a
standing-risk register entry alongside the test.** That register exists for exactly the properties
whose breach nothing announces. Check for an entry that already covers it, and propose rather than
apply.

### 3.5 Pass 4 — conventions

Mechanical, and quick once the habit forms. **The conventions themselves — filenames, index
structure, link forms, title matching, fence tagging — are recorded in the project's own
documentation-conventions reference, not here**; this pass is the instruction to check the page
against them, plus the four items that are about *reviewing* rather than about the convention.

**A page describes the current state, never a former one.** This is the one Pass 4 item a page
reliably acquires *between* reviews, because the review loop creates it: feedback turns into a code
change, the change updates the page, and the sentence that updates it says what changed — *"this used
to be refused"*, *"that is now implemented"*. Every one was written by a correct, careful edit. So do
not treat a recently reviewed page as settled on this point.

Five greps find nearly all of it, and no single family is sufficient — each was added after the
previous set missed a real instance:

```bash
grep -nE "used to|previously|formerly|no longer|in the past|historically|originally" <page>
grep -nE "ha(ve|s) always|as before|unchanged from|for (backward )?compatibility" <page>
grep -nE "\bnow\b" <page>
grep -nE "\balways\b|\bstill\b" <page>
grep -nE "has moved|has changed|was renamed|has since|used to be|no longer does" <page>
```

The fourth exists because a page carried *"as it always did"* — squarely in this class and matched by
none of the first three. The fifth exists because another carried *"has moved"* — the tell the earlier
patterns share is a temporal *adverb*, and that one is a verb.

**Read every hit; never replace one mechanically.** Most are innocent and must stay: "no longer
belongs to any one row group" describes a state *after an operation*; "always resolves the first
match" is a guarantee, not a history. **What is not innocent is a sentence a reader cannot evaluate
without knowing a release they never used.** Two measured ratios: fourteen hits and three real on one
sweep, thirteen hits and one real on another. That ratio is the point — a mechanical replacement would
have damaged eleven correct sentences to fix three.

**A performance figure on a guide page is approximate and machine-free.** A reader cannot reproduce a
figure quoted to three significant figures against a compiler and a machine they do not have, and a
page carrying one is stating something it cannot support. No compiler names, no machine names, no
decimals on a ratio; name the tool that measures it instead, so a reader can get their own number.
**Two things are not covered**, and stripping them makes the page worse: a **contract number** (a
threshold, a documented bound, a share with real cross-machine provenance) stays exact, and a **parity
claim** is the point of its sentence rather than a speed measurement. The test is whether the number
is *evidence for a design choice a reader has to make* — keep it — or *a snapshot of one machine's
throughput* — generalise it.

**A page that has accreted edits without a rewrap is damaged in a way no check can see.**
Successive correct insertions leave a paragraph with lines of wildly uneven length, and an
enumeration broken mid-item across several of them. The page renders correctly, every check passes,
and only a reviewer reading the *source* will find it — one landing page carried lines of 173, 139,
109, 38, 20, 19 and 16 characters in a single paragraph. **Rewrap on the round that first reviews a
page, never on a follow-up**: a page-wide reflow on a later round buries a one-sentence fix in dozens
of hunks and turns step 2 into a re-read. Prove any rewrap whitespace-only the way
[§8](#8-editing-a-page-safely) requires.

**Spelling is left alone.** Where a guide mixes regional spellings, changing them costs the maintainer
a full re-read in step 2 and buys the reader nothing. Change spelling only inside a paragraph already
being rewritten for another reason.

**Terminology precision is load-bearing.** A project usually has a handful of terms whose distinction
matters, and a page using one loosely teaches the reader a wrong model. Check the page against how the
rest of the guide uses them.

### 3.6 Pass 5 — the reader

Read the page as someone who does not know the library.

- Does the opening say what the page is for and who needs it, in its first two sentences?
- Is there at least one complete, runnable example, early?
- Does the page's order follow what a reader does, rather than how the code is organised?
- Is anything on the page really another page's topic? Say so; do not move it unilaterally if it has
  inbound links — propose the move.
- Is anything unreadably long without a subheading a reader could land on?

**Before fixing a wrong phrase on one page, grep the whole guide for it.** A sentence that is wrong
here is usually repeated: a guide cross-describes itself constantly, so an index bullet, a page's own
opening and a "see also" pointer frequently carry the same claim in the same words. One review fixed a
wrong description and found the identical sentence in **four other places across three pages**, two of
them already reviewed and closed. **Fixing one occurrence is worse than fixing none in one specific
way**: it leaves two pages actively disagreeing, which reads to a user as a real distinction. The grep
costs one command and belongs with the fix, not after it.

### 3.7 Closing step 1: apply, verify, report

Apply the fixes [§7](#7-what-a-review-may-edit) pre-authorises and propose the rest. If the page's
title or headings changed, update its index entries in the same edit. Run the applicable checks from
[§9](#9-checks-to-run). **If the page gained a table, a nested list or an unusual fence, render it
once** — the guide is read as generated output, not as Markdown, and rendering is the only way to see
that the new construct survived. Then write the report ([§4](#4-the-review-report)) and record the
page's step-1 date.

**Three scope limits hold for every review, and these are the ones easiest to talk yourself out of:**

- **A change to what the library does is report-only**, however small and however obvious the fix
  looks. Correcting a doc-comment that *describes* a behaviour is a documentation fix; changing the
  behaviour it describes is not.
- **Do not restructure the guide** — no new page, no moved page, no split — as part of a page review.
  Propose it; the maintainer decides.
- **Documentation-only changes get no changelog entry.** If the review uncovers a genuine mismatch
  that becomes a code fix later, that fix gets its entry when it is made.

## 4. The review report

**One file per unit reviewed, in the repository root, named for it** — use a stable alias fixed in
advance so two sessions cannot pick different names for the same report. The unit is one page, or
one group of pages taken per [§1](#1-what-a-documentation-review-is-for); either way it is what is
in flight. Do not accumulate unrelated pages' findings in one file.

These files are git-ignored scratch, so they must be **self-explaining without the conversation that
produced them**: name the page and the commit at the top, quote the maintainer's decisions verbatim
rather than alluding to them, and assume the reader was not present. They have **no recovery path if
damaged** — no history, no diff — so edit them with per-file edits rather than scripted rewrites, and
**check the structure afterwards, because nothing else will**:

```bash
grep "^#" <report>.md | sort | uniq -d      # empty = no duplicated heading
```

That check exists because folding step-3 outcomes in by *anchored append* — appending after a string
that ends in a heading — re-emits that heading, leaving a duplicate with an orphaned paragraph under
the wrong one. One report acquired a duplicated section and a duplicated finding that way, unnoticed
until the page closed.

**A report's own links are checked, so use the repository-relative Markdown form.** A link written in
the rendered-HTML form used *inside* the guide does not resolve from the repository root and fails the
lint run — for everyone, on every later run, until it is fixed. **Putting such a link in a code span
does not save it**: an anchor checker matches the link syntax in raw text and never parses Markdown,
so backticks are not an escape. This bites when a report *recommends* a link for a page. Describe the
link in prose instead — naming its text and its target — which is unambiguous and inert.

**Report every issue found, each with a recommended solution** — including the issues already fixed,
so the maintainer can see what changed and why without diffing the page. An issue without a proposed
fix is half a report; where more than one fix is reasonable, name the one you recommend and say what
the alternative costs.

Structure, in this order:

1. **Header** — the page, its line count, the date, the commit reviewed against, and one sentence on
   the page's subject and intended audience *as the review understood it*. If that sentence is wrong,
   everything below is aimed at the wrong reader, and it is the cheapest thing for the maintainer to
   correct. Carry a **status line**: which round, and whether that round's work is committed or
   sitting in the tree.

   **Name the machine any figure came from.** Two rounds run on different machines produced counts
   that disagreed in a way that reads exactly like a regression and was not — different compiler,
   different dependency version, and a file *count* that varies with how many scratch files a checkout
   happens to carry. Say which machine, and say the tables are not comparable.

   **The header also carries one line per pass, saying what that pass covered — or that it found
   nothing.** Six lines, before the findings. Without them a review that skipped a pass produces a
   report that looks complete: the findings sections are populated, nothing is visibly absent, and the
   omission surfaces months later when an undocumented guarantee breaks. The pass most often skipped
   is [Pass 3](#34-pass-3--tests), because it is the only one whose output is work for someone else.
   *"Pass 3 — tests: four claims checked, three covered, one proposed (see D2)"* is enough; the point
   is that a blank line is visible in step 2 and a missing section is not.
2. **Changed** — what was edited, one line each.
3. **Gaps filled** — anything undocumented that is now documented, with the source location the
   behaviour was verified from, so the maintainer can spot-check rather than re-derive.
4. **Issues found and not fixed** — each as: the issue, the evidence, **the recommended fix**, and why
   it was not applied. This is the main body of most reports, so **order it worst-first and tag each
   entry**: *wrong* (the page states something the code contradicts), *missing* (a behaviour a reader
   would only discover by hitting it), *unclear* (correct but readily misread), *cosmetic*. The
   maintainer should be able to stop reading after the *wrong* entries and have lost nothing
   important. Two *wrong* entries beat twenty cosmetic ones.
5. **Looks like a code issue** — anything where the code itself appears wrong, missing or inconsistent
   with its siblings, each with evidence and a recommended fix. Explicitly not acted on. **How far this
   section may reach depends on whether the surface is released**, which is settled once per campaign
   rather than per finding — see [§14](#14-release-status-decides-what-a-review-may-recommend).
6. **Open questions** — after every finding, so the maintainer reads the diagnosis before being asked
   anything. Number them. **Propose a recommendation for each**, with the reasoning in a sentence or
   two; where none is defensible, say so and set out what each option costs. A bare question with no
   recommendation is a decision handed back untouched, and should be rare.

   **Step 3 generates questions of its own, and they belong here too rather than being decided
   silently.** Building an approved fix is what exposes the cases the report never considered — they
   appear only once you are holding the code. Append the question, say plainly what you did in the
   meantime, and surface it in the reply. A question found this way is a sign the fix is understood,
   not that the review was incomplete.
7. **Deferred to the future** — **the last section of the file**, and the single home for **every**
   action the review identified and did not carry out. Sections 4 and 5 are *diagnosis*; this is the
   *work list*, so a reader picking the page up later reads one section instead of re-deriving the
   actions from three. It is last because it is what a future session reads first. It absorbs three
   kinds of item, and each entry says which it is:

   - **Tests to write** — as specified in [§3.4](#34-pass-3--tests), including the negative control.
   - **Code changes** — the fixes proposed by section 5, one actionable line each.
   - **Documentation work too large to apply unilaterally** — a new section, a reordering, a rewritten
     example, a proposed split.

   Cross-reference each entry to the finding it came from instead of repeating the evidence — the
   finding is where the reasoning lives, and duplicating it is how the two drift apart.

   **When an item is approved and then done, move it out of this section**, naming the test or commit.
   An entry still sitting here after it has been carried out will be done a second time. An entry the
   maintainer declines is deleted with the reason recorded, not left looking pending.

**A page that has been through more than one round grows two further sections**, placed after the
first round's record and **before** "Deferred to the future", which stays last:

- **The agreed plan, one heading per item.** Every decision quoted **against the item it decides**,
  never collected at the end; the deliverables spelled out per item (test, negative control, changelog
  entry, risk-register entry); the build and machine notes *inside* the document; and an explicit
  "nothing here is waiting on an answer" so the next session knows it may start.
- **An `Outcome:` paragraph per item, written when the item is built** — not a tick, a paragraph. **An
  outcome that deviated from the plan must say so**, rather than quietly doing the right thing: the
  plan is otherwise read as having been correct, and the next plan is written with the same blind
  spot. On one page three of seven items deviated — one named only half the call sites it needed to
  change, one specified an observable that turned out to be unreachable from a test, and one sketched
  a signature wrongly. Those paragraphs are the only evidence the plan was imperfect.

**If a section is empty, say so in one word rather than omitting it** — "no code issues found" is
information; a missing section reads as "not checked".

**Keep it short enough to be read in one sitting.** That is what makes step 2 sustainable across a
whole guide. Detail belongs in the evidence lines, not in prose around them; the severity ordering is
what lets a long report still be read in one, because it can be stopped part-way without loss.

**Keep the report after step 3**, updated with what the maintainer decided. It is then the record of
why the page reads the way it does, and the first thing to read before reviewing that page again.

**A report keeps its own outstanding items, and they are not copied anywhere else.** When a page
closes, or when a whole campaign ends, whatever is still in "Deferred to the future" **stays in that
page's report**. Do not fold it into the standing-risk register, an issue tracker or any other tracked
document as a matter of course, and do not delete the report to tidy up: the maintainer tracks the
open items, and a review's job is to record them where they were found, not to file them. This is the
same boundary [§3.4](#34-pass-3--tests) draws for a proposed risk-register entry — the review
*proposes*, and something outside the review decides.

## 5. Step 2: how feedback is given

**Feedback is written into the report file itself, as lines beginning `Comment:`, and it appears in
every section — not only under "Open questions".** A `Comment:` line sits directly below the entry it
answers, so a comment under a *wrong* finding, a code issue, a proposed test or an open question all
mean the same thing: the maintainer has ruled on *that* entry.

**Before acting on anything, `grep -n "Comment:" <report>.md` and read every hit.** The first review
run under this convention had 17 comments spread across four sections, and reading only the open
questions would have missed 13 of them, including three code fixes.

**Treat a bare `Comment: approved` as approval of that entry's own recommended fix** — which is why
every entry must carry one.

## 6. Step 3: acting on feedback

Feedback splits into two kinds, handled differently.

**Documentation feedback** — apply directly, then re-run the checks. No confirmation round needed.
This includes feedback **rejecting an edit step 1 already applied**: revert it, and record it in the
report as declined with the maintainer's reason, so the next review of that page does not helpfully
reapply it.

**Feedback that changes a code feature** — a normal feature request, inheriting every standing rule:
unit tests (plus an out-of-process scenario if it can abort), doc-comments on any new public
procedure, the relevant guide page updated, the API overview updated if the public surface changed,
and a changelog entry. **Do not commit on the default branch.**

**A code change from a review reaches further than the page under review, and the suite is where it
lands.**

- **Widening what the library accepts silently invalidates every test that asserted the old
  narrowness**, and those tests are not near the page or the procedure — they are wherever somebody
  once needed an example of the thing that used to be refused. One widening broke **seven** tests, all
  correct to break: six scenarios had been using a particular unsupported case precisely *because* it
  was unsupported, and one asserted a refusal that had ceased to exist anywhere. So after any
  behaviour-widening change, run the **whole** suite and read each failure as a question about which
  fixture was relying on the old behaviour. Repoint those, and add a test pinning the newly permitted
  behaviour — or the widening itself is untested.
- **A narrowing change does the same in reverse, and there the failing test is usually right to
  fail.** The fix is to repoint the fixture **and correct the doc-comment that advertised the old
  behaviour** — the half most easily missed. A test whose comment describes something it no longer
  does will mislead the next person more than no comment at all.
- **An existing test failing is also the cheapest way to find a gap in the design.** One change missed
  that a particular type reached a different code path than the rule had been written for; no new test
  caught it — an existing round-trip did. That is the argument for running the whole suite before
  believing a design was complete.

**Approval of a proposed test** — a normal code task. Write the negative control the report named, not
only the positive assertion, and **verify the test by breaking the documented behaviour deliberately
and confirming it fails** — including checking the exit status, since a mutation frequently makes a
test *abort* rather than fail an assertion. A test added this way gets no changelog entry: it changes
nothing a user can observe. Then move the entry out of "Deferred to the future".

**A comment's literal scope may be narrower than the real occurrence set, and widening it is allowed —
but every extra site goes in the report so it can be reverted.** A comment answers the question as
asked, and the question was written from what step 1 happened to find. One approval to "fix in both
places" turned out to cover **five occurrences across four files**, two of them on a page already
closed. Doing only the two named would have left the guide contradicting itself, which is what the
approval was plainly trying to prevent. So apply the decision's *reasoning* to everything it fits, and
then **list each extra site individually in "Changed", saying in as many words that this is wider than
the comment's literal words**. That keeps the widening one revert away from being undone, and keeps
step 2 a reading task rather than a hunt for what else moved. Do not silently widen, and do not
silently decline to widen either.

**"Check and fix if necessary" is not a hedge — it is an instruction to audit, and it will cost more
than the finding it answers.** One such comment, answering a report about six sites in three files,
scoped a sweep that found **16 sites in four files**, because the report had looked at one construct
and not at its siblings. Two rules follow. Read such a comment as scoping the work to *the rule*, not
to the instances the report happened to list — the maintainer is telling you the report's inventory is
not trusted. And when the audit's answer is much larger than the report's, **say the number in the
reply**: the difference between six and sixteen is the difference between a tidy-up and a behaviour
change, and step 2 approved the former.

**Update the report in the same pass**, not afterwards: fold each answer into the open question it
resolves, quoting the maintainer's own wording; mark the issues now fixed; leave anything declined
recorded as declined with the reason. A report still showing an answered question as open is worse
than no report, because the next session will ask it again.

**A later instruction may override an answer already given, and the answer is kept rather than
rewritten.** Record the override beside it — the date, the new decision, the reason — and say what,
if anything, was wrong with the original. Usually nothing was: one campaign's decision to give each
index page its own report was overridden days later by an instruction to review them together, and
the argument the original answer had weighed and set aside is exactly the argument that later won.
Silently rewriting a decision loses the record of what was decided when, which is most of what a
report is for.

## 7. What a review may edit

The maintainer has pre-authorised **small, obvious, documentation-only fixes anywhere in the
repository** — not only on the page under review. **Two bounds carry the whole weight of that
permission**, and both are load-bearing:

- **Small and obvious.** A typo, a broken link, a wrong argument name, a stale default, a renamed
  procedure, a sentence the source contradicts. Anything larger — a new section, a reordering, a
  rewritten example, a page split — is **proposed**, not applied. The test to apply: *can the
  maintainer verify this change from the report line alone in step 2?* If they would have to re-read
  the section to see what happened, it was too big to apply unilaterally. **This is what keeps step 2
  a reading task rather than a re-review.**
- **Documentation-only.** Nothing that changes what the library *does*.

**Neither bound moves when the surface under review is unreleased.** That widens what the report
may *recommend* and nothing else — see
[§14](#14-release-status-decides-what-a-review-may-recommend). A review that starts editing
signatures because "it has not shipped yet" has stopped being a review.

| target | what may be applied directly |
|---|---|
| the page under review | any doc-only fix within the two bounds |
| its index entries | the bullet and its one-line description, when the page's title or scope changed |
| other guide pages | any doc-only fix within the two bounds — **not just** links pointing at the page under review. A *wrong* claim is fixed wherever it is found, whatever its review order: leaving it is what makes two pages disagree, which reads to a user as a real distinction. The bounds still decide — a typo or a contradicted sentence is fixed anywhere; a new section or a rewrite still waits for that page's own turn, because that is *large*, not because it is elsewhere. Record such a fix naming the file, **and list in "Deferred" what you did NOT check on that page**, so its own review knows how much was covered and does not read the visit as a partial review |
| README | an API-overview entry, a stale link into the guide, a claim the source contradicts |
| contributor docs, changelog, standing-instructions file | a stale link or anchor — needed anyway when a heading rename breaks the anchor checker. In a changelog, a **published** section records what that release shipped: repoint a dead link, change nothing else. An **unpublished** section (`[Unreleased]`) is not a record of anything yet, so a *factual* correction to it — a bullet naming an argument the code does not have, a stated default the source contradicts — is within the two bounds and may be applied. Restructuring it, or moving an entry between its `### Added`/`### Changed`/`### Fixed` groups, is *large* and is proposed |
| the standing-risk register (tracked) | a stale link or anchor only. **Never** a verdict, a risk number, a section move or a pruning |
| source doc-comments | a wrong or stale doc-comment, within the two bounds |
| **generated** sources | the same fix, made in the **generator's template** and regenerated — never in the emitted file. Check the file's banner first; CI runs each generator's `--check` and a hand-edit fails it |
| tests | only a mirrored example and its assertions, changed together with the page. A **new** test is proposed, never written during the review |
| scratch planning files | a **broken link or anchor** only — these fail the anchor checker for every session until fixed, so leaving one costs every later review its baseline. Anything beyond that needs approval: they may be mid-use by another session and have no recovery path |
| anything else in source, tests, tooling, build or CI configuration | nothing — report it |

**The mirrored examples are the one place a page edit reaches into the test tree, and the rule is
both-or-neither.** If a mirrored example is wrong, fix the page and the test in the same edit and run
that suite. If it cannot be made right without changing what the library does, change **neither side**
and report it. Never edit the page and leave the test asserting the old text: the suite still passes
against a page nobody has checked since, which is worse than the original error — and is why Pass 0
asks which examples are mirrored before anything is touched.

**Every standing project rule still applies to an edit made under this section** — line-length limits,
the generated-file rule, any preprocessor hazards in source comments, and **no commit or push on the
default branch**. Work is left in the working tree for the maintainer.

## 8. Editing a page safely

The mechanics below have each destroyed a page silently. They apply to any scripted or multi-part
edit, which most reviews involve.

**Take a copy of the page before a scripted pass.** The guide is tracked, so version control can
recover it — but only if the scripted change has not already been mixed with the review's intended
edits, which by that point it usually has.

**A multi-replacement script that validates everything and writes once at the end loses the earlier
replacements when a later assertion fails — and nothing downstream can see it.** The natural shape is
an assertion per edit followed by a single write; a failure on the third assertion aborts with the
first two applied *in memory only*. The page is untouched, the traceback scrolls away, and every check
stays green, because a page missing two paragraphs is still a valid page. Confirmed on a page where
two paragraphs went silently unwritten and were found only by rendering. Either write after each
replacement, or verify afterwards.

**A rewrapping or reformatting pass must be provably whitespace-only.** Collapse all whitespace on
both sides and **refuse to write unless the two are byte-identical**. That is stronger than reading
the diff, it is one line, and it catches every accidental word change at once. Comparing token *lists*
instead is fragile — an inline code span that spanned a line break re-pairs with a different delimiter
once rewrapped and reports as changed when nothing did. Two further hazards, both of which nearly
shipped:

- **An indented table is still a table.** Testing whether a line starts with the table delimiter
  misses a table nested under a list item, and joining its rows into a paragraph destroys it while the
  page still renders plausibly. One such table was flattened and **only a content check noticed**;
  nothing about the rendered page looked wrong. Test the *stripped* line.
- **Do not reattach punctuation inside an inline code span.** A rule that fixes detached punctuation
  after a link can mangle a code span in running prose, which no fenced-block exclusion covers. Diff
  the word stream against a pre-pass copy and read every hunk; if the intended change was 17 hunks, an
  18th is obvious.

**After any scripted or multi-part edit, render the page and grep the generated HTML for each new
passage.** Not for structure — for *presence*: search for one distinctive phrase per edit, **with
whitespace collapsed on both sides**, because a source line break survives into the rendered paragraph
and a search string spanning one reports a correct paragraph as missing. It is one command, and it is
the only check that sees a lost edit.

**Probe for the new text, never for a newer timestamp.** Comparing rendered and source mtimes looks
like a cheap staleness check and is not: filesystem granularity can make a render begun *before* an
edit produce an HTML file whose timestamp is later. Confirmed where the HTML was a minute newer than
the page and still carried the pre-edit wording; only a probe searching for the *new* sentence caught
it. Treat a MISSING as "re-render", not as "the edit was lost" — and note a full docs build can take
minutes, so a probe run against a render still in flight will find the newest pages stale for the same
reason.

**Verify a code block's rendering structurally, not by searching for its text.** A syntax highlighter
splits a line across many span elements, so no substring of it survives in the HTML and a grep reports
it missing from a block rendering perfectly. Check instead that the block's identifiers sit inside a
preformatted element rather than a paragraph, and that **no literal fence marker survives anywhere in
the generated output** — that is the exact signature of an unrendered fence.

**Count list items in the rendered HTML against list markers in the source** after restructuring a
list. One restructure lost six bullets to a lazy-continuation rule with every check green — a broken
list reads as ordinary prose in both the source and the rendered page, so reading either one does not
find it.

**Do not hand-roll a link rewrite with an unanchored search-and-replace.** Prefer per-file edits, or a
script that asserts an expected match count per file and prints what it changed.

## 9. Checks to run

**Run the applicable subset before starting work as well as after.** A check that was already failing
is not evidence about your edit, and half an hour can go into a failure that was there when you
arrived.

**Not every item applies to every change.** A structural change — a page added, moved, split or
renamed — needs the full set including a docs build; a page-content review runs the smaller subset.

Ordered so that the checks catching *silent* failures come first. **The first two exist for a
structural change and need a `ford docs.md` build; a page-content review runs the rest.**

- [ ] **The rendered page count matches the expected number** —
      `find ford-doc/page -name '*.html' | wc -l`, against the count of content pages plus index
      pages. A group directory with no `index.md` is **silently skipped**: FORD emits a warning
      naming nothing useful and exits 0, and every page in that group simply does not exist on the
      site. A build "succeeding" therefore proves nothing, and this is the only defence against it.
- [ ] **`tools/fix_ford_page_links.sh ford-doc` reports a non-zero rewrite count**, with one nested
      README link spot-checked in the generated HTML. Its failure mode is a silent no-op: a regex that
      stops matching rewrites nothing and exits 0, and every README link into the guide then 404s. A
      count of 0 looks exactly like success.
- [ ] **`python3 tools/check_doc_anchors.py` exits 0.**
- [ ] **`python3 tools/check_source_conventions.py` exits 0.** **A new failure here may be the
      check's fault rather than the page's.** Several of its checks parse a guide page, and one
      matched by a shape so generic that merely *adding* a table to a page reported four documented
      items as deleted. Read what the check actually matches before editing the page to satisfy it; if
      it is over-broad, anchor it on something distinctive and verify it still catches a real drift
      afterwards.
- [ ] **`ford docs.md` runs clean** — the "Graphviz not installed" warning is expected and
      environment-only.
- [ ] If any `src/*.f90` doc-comment changed: the relevant generator's `--check` exits 0 (proving a
      generated file was changed through its template), and `fpm build` still passes —
      `[preprocess.cpp]` runs cpp over every source file, so a stray `/*` or a trailing `\` inside a
      Fortran comment is a compile error that names the wrong line.
- [ ] If a mirrored example changed: `fpm test run_tester -- examples`, or the suite that mirrors it.
- [ ] If a `src/*.f90` doc-comment changed: `ford --warn docs.md`'s **`Unknown entity`** count is
      unchanged. That is the one FORD number worth tracking; the raw total is noise. **Do not
      hardcode the expected figure in any document** — one such number went stale and would have
      reported a false regression on the first page reviewed. Re-derive it, and note FORD wraps each
      warning across two lines, so a plain `grep -c` undercounts. A `doc/pages/`-only edit cannot move
      it, so this is worth running only when source changed.
- [ ] **A grep for guide paths across every file type that can carry one** —
      `grep -rn 'doc/pages/' --include='*.md' --include='*.f90' --include='*.py' --include='*.sh' .` —
      returns nothing stale. `check_doc_anchors.py` parses Markdown only, so this grep is the only
      guard for links in source, test and tooling files.

## 10. Tracking progress

A campaign needs a progress record. **It belongs in the campaign's own working document, not here** —
this file must stay free of any page list so that it does not go stale.
[§13](#13-scoping-a-campaign) says how that list is derived and what else the working document
holds; this section says what the record itself needs.

What that record needs:

| column | purpose |
|---|---|
| order | the review order, so a session knows what is next |
| page | the page under review |
| report file | its report's alias, **fixed up front**, so two sessions cannot pick different names for the same page's report |
| step 1 / step 2 / step 3 | the date each step completed, with the round appended where a page took more than one |

**The table is an index, not the authority — the report file is.** It has one cell per step and
therefore cannot express a page that went through two rounds, nor one whose latest round is finished
but uncommitted.

Three ordering rules:

- **Review in reading order** — the order the guide's own top-level index lists the pages.
  Terminology settles from the front of the guide, which is also the order a reader meets it, so a
  decision made early is available for every page after it.
- **The order is a default, not a rule.** The maintainer may reorder or skip groups. Nothing about the
  loop changes. **Leave a skipped page's cells empty** — an empty cell must only ever mean "not yet
  reviewed", and a skipped page is exactly that.
- **Review each group's index page last within its group**, once its pages' titles and scope are
  settled — it describes them, so it cannot be right before they are. By the same argument, a front
  page that carries an API overview and links into the guide is reviewed last of all.

**When reviewing an index page, re-derive every bullet from its page's current opening paragraph
rather than reading it for plausibility.** An index bullet's description is typically the one part of
a guide that no check compares against anything: a consistency checker validates the bullets' link
*targets* and their order and discards the prose, so a bullet may describe a page that has since grown
two whole sections while the lint stage stays green. This is not hypothetical and not rare — it is the
*expected* consequence of the ordering rule above, because a group's own review is exactly what moves
its pages' scope. On one index page all three bullets read plausibly and one was two whole sections
out of date.

## 11. Where a convention belongs

When feedback establishes a **general** convention rather than a one-page fix — a preferred term, a
formatting rule, a structural preference — record it **as part of acting on it**, so the remaining
pages are reviewed under the same rule. That is the mechanism by which the loop converges instead of
relitigating the same point on every page.

**Where it is recorded matters, and this file is usually the wrong home.** A scratch planning file is
one `git clean -X` from gone and is invisible to a contributor who never opens a planning document.
So:

- **A durable convention** — anything someone adding a *new* page next year, with this campaign long
  finished, would need — goes in `CLAUDE.md`'s "Documentation conventions" section, which is tracked
  and is this project's authority for how a page is written. Leave the working detail in the
  campaign's own document if it needs elaboration.
- **A procedural rule** — how to run this loop, what a report contains, what a review may edit —
  belongs here.

The test is one question: *would someone writing a new page next year need to know it?* If yes, it
belongs in a tracked file.

**There is a third home, and a campaign needs it: the decisions a campaign settles part-way
through.** A maintainer's answer on page 15 that governs pages 16 onward is not yet
durable-convention material and is not a rule about the loop, and leaving it in page 15's report
means a later session has to open every closed report to discover it exists. Keep a
**standing-decisions section in the campaign's own working document**: one heading per rule, each
quoting the instruction verbatim, each naming its *durable* home — this file, the project's own
documentation-conventions reference, or "nothing enforces it yet, so here until a check exists". Two
properties keep it useful. It holds **decisions, not work items** — every report still keeps its own
outstanding items where they were found ([§13.8](#138-close-the-campaign)) — and a rule whose
durable home is "nowhere yet" is a rule worth proposing a check for.

## 12. Settings analysis

**This document introduces no process-global parameter**, and neither does a page review conducted
under it: a review produces documentation edits and a report, and [§7](#7-what-a-review-may-edit)
forbids it from changing what the library does. No source, no interop surface, no `parquet_settings`
knob, no environment variable.

**A page review that turns into a code change is where the question can arise**, and that change is a
separate, explicitly requested task under `CLAUDE.md`'s rules — including the requirement that its own
`feature_*.md` document carry a settings analysis. It is not covered by this one, and a reviewer must
not treat this section as having answered it on that change's behalf.

**Whether the feature under review got its own knobs right is a different question, and it is a
review question rather than a settings decision taken here.** It belongs to
[Pass 2](#33-pass-2--coverage): a module that threads internally, or that has a tunable the caller
cannot reach, either has a `parquet_settings` knob or has a reason not to, and the page should let a
reader tell which. Two asymmetries worth checking for by name, because both are invisible from the
page alone — a module that threads but exposes no thread cap while a sibling exposes one, and a
module that takes its configuration from its own environment reader rather than from
`parquet_settings`. Either may be entirely deliberate. **Confirm it is, rather than assuming it**,
and use `CLAUDE.md`'s admission test as the yardstick: a setting may change how fast, how large or
how loud the library runs, never what it answers.

## 13. Scoping a campaign

**A campaign is a set of pages reviewed under one boundary.** Everything before this section assumes
that set exists. This one says how to build it, and it is worth the effort: **the page list is the
only part of a campaign nothing downstream can correct.** A wrong finding is caught in step 2 and a
missed test is caught by Pass 3 on the next page, but a page nobody scheduled is simply never
reviewed, and no check anywhere reports it.

**The output is one working document**, git-ignored scratch in the repository root, holding the
scope, the page list, the order, the progress record ([§10](#10-tracking-progress)) and the opening
questions. It is the campaign's own file; this one stays free of any of it.

### 13.1 Fix the boundary, and say what it is

**Name a commit, not a feeling.** "Since the last release" is not a boundary until it is a tag and a
hash: `git log <tag>..HEAD` is what every later derivation runs against, and two sessions
disagreeing about where a campaign starts will produce two different page lists from the same
repository. Record the tag, its commit and its date, and the baseline commit the campaign is scoped
against.

**Quantify what fell inside it**, in one table — commits, new source files, new pages, modified
pages, inserted documentation lines. This costs one command each and is what tells the maintainer,
before agreeing to anything, how large the campaign is.

### 13.2 Derive the page list from the code, not from the diff

**A documentation diff finds the pages someone remembered to update. It cannot find the ones they
did not.** That is the same trap [§3.1](#31-pass-0--establish-the-source-of-truth-from-the-source)
describes one level down — deriving a page's expected contents from the page can only confirm what
is already there — and it has the same fix: derive from the source.

So build the list from **three** independent sweeps, and expect them to disagree:

- `git diff --name-status <boundary>..HEAD -- doc/` — what changed, split into added and modified.
- The changelog's unpublished section, read as an **inventory of what has to be documented**. Each
  entry is a claim that something exists; ask which page carries it, and a claim with no page is a
  finding before the campaign has started.
- **The new public surface**, from the source: new modules, new entry modules, new public procedures
  and types. This is the sweep that finds a feature nobody wrote a changelog entry for.

**Then ask the coverage question of every page the sweeps did *not* return**: what does the new
surface add to *this page's own topic?* That question is what produces the third tier below, and it
cannot be answered by any diff. A cheap, high-yield form of it is to count mentions — a page whose
subject was extended and that mentions the extension zero times is a gap with its evidence already
attached.

**Express the result as a map from body of work to page**, not only as a list of pages: one row per
feature area, naming its new entry modules and the page or pages that carry it. That is what makes
the changelog sweep's "which page carries this claim?" answerable at a glance, and a row with an
empty page cell is a finding before the campaign has started.

**The list's complement is part of the output.** Say what the campaign does *not* cover and why —
pages whose subject predates the boundary and has not changed, an audit deliberately deferred, a
tracked document that is out of bounds — and say plainly that out of scope means **not scheduled**,
not untouchable: a wrong claim found in passing on an unscheduled page is still fixed under
[§7](#7-what-a-review-may-edit) and still reported. Without that sentence a reviewer meeting such a
page has to guess, and the two guesses cost very differently.

### 13.3 Sort the pages into four tiers

The tiers are not decoration: each one is reviewed differently, and saying which tier a page is in
tells its reviewer how much of it to read.

| tier | what it is | how it is reviewed |
|---|---|---|
| **new pages** | did not exist at the boundary; never reviewed | the full six passes over the whole page. These carry most of a campaign's value, and each is usually the only description of its subject in the guide, so nothing else will catch an omission |
| **extended pages** | reviewed under an earlier boundary, since given new material | scoped to what is new **plus whatever the new material makes wrong elsewhere on the page**. Pass 0 still reads the whole page, because a new section frequently contradicts an old one — and that contradiction is the most valuable thing this tier finds |
| **coverage-gap candidates** | untouched, but their subject was extended | scoped to one question: what does the new surface add to this page's topic? Cheapest reviews in the campaign, and a legitimate outcome is "nothing belongs here" — which is worth *recording* rather than assuming |
| **index and landing pages** | group indexes, the guide's landing page, the front page | last, per [§10](#10-tracking-progress). Re-derive every bullet from its page's current opening paragraph |

**State the tier a page is in beside the page**, with the evidence for the third tier attached — the
count that showed the gap. A candidate tier is a *proposal*: it is the one the maintainer is most
likely to trim, so make it trimmable by ordering it worst-first, like a report's findings.

**"Modified" means modified substantively.** Check each modified page against its own diff and say
so; a campaign that silently drops a page as a typo pass, and is wrong, has lost that page for good.
If nothing was excluded, say that too — "nothing qualified" is information.

**Give each page its line count.** It sizes the work. A page of several hundred lines is not a
one-sitting review, and pretending otherwise is how [Pass 3](#34-pass-3--tests) gets skipped.

### 13.4 Fix the report aliases up front

Every page gets its report filename assigned in the working document **before the first review**,
per [§10](#10-tracking-progress). Two properties matter and both are easy to lose:

- **They share a prefix that the repository's ignore rules already cover**, so a report is scratch by
  construction rather than by someone remembering. In this repository that means starting them with
  the ignored planning-document prefix.
- **They are unique and stable.** Cross-check the finished list mechanically — one alias per page,
  no duplicates, and every page in the tier tables present in the order table and vice versa. That
  is three lines of script and it catches the one error that would otherwise be found by two
  sessions writing into the same file.

### 13.5 Order, and when to deviate from reading order

[§10](#10-tracking-progress)'s default is the guide's own reading order. **A campaign whose new
material is concentrated in one group may deviate, and the argument for deviating is the same
argument that gives the default**: terminology should settle on the pages that *own* it before the
pages that *borrow* it are reviewed. When one group introduces a vocabulary — a set of new types and
the words for their states — that several other pages then use, review that group first and keep
reading order within every tier after it.

Three rules for a deviation:

- **Propose it, do not take it.** It is an opening question ([§13.7](#137-open-the-campaign-with-the-questions-that-block-it)),
  with the reasoning stated, because the maintainer may have a reason to meet the guide in order.
- **Say what it costs.** Here: a reader meets the guide in reading order, and the reviewer no longer
  does — which matters for a reader, not for a reviewer.
- **Index pages go after every content page**, not after their own group, whenever most groups have
  a page in the campaign. Interleaving them means starting one group's index before another group's
  review has settled a term it uses.

### 13.6 Record a baseline before the first review

[§9](#9-checks-to-run) says to run the checks before starting as well as after. A campaign records
that once, in its working document, so that a failure appearing on page 20 belongs to page 20:

- the **baseline commit** and whether the working tree is clean;
- the **machine and toolchain**, because [§4](#4-the-review-report) requires every figure a report
  quotes to name its machine and forbids comparing two machines' figures;
- the **result of each check**, named, with its count where it prints one;
- the **size of the guide** — content pages and index pages — since that is what a structural check
  at the end is compared against.

**Say which checks were *not* run and why.** A campaign that adds no page does not need a docs build
for its baseline, and recording that decision is what stops a later session reading the absence as
an oversight.

### 13.7 Open the campaign with the questions that block it

A campaign's opening questions are given and answered exactly as a report's are
([§4](#4-the-review-report), [§5](#5-step-2-how-feedback-is-given)): numbered, each with a
recommendation and its reasoning, answered by a `Comment:` line underneath. **Say which of them
block the first review** — typically the order and the scope of the trimmable tier, since both
change what the first page is — and start nothing until those are answered.

**One question is worth asking in every campaign and is easy to leave out:** is there any part of
the surface in scope that the maintainer considers settled and does not want relitigated? It costs
one line to answer and saves several reports' worth of proposals, and it is the natural companion to
[§14](#14-release-status-decides-what-a-review-may-recommend).

**A second standing question applies whenever the campaign has consequences beyond itself:** does
finishing it gate anything — a release, a tag, a hand-off — and is the trimmable tier
([§13.3](#133-sort-the-pages-into-four-tiers)) inside that gate or outside it? A campaign that
blocks a release is not one whose cheapest tier can be quietly dropped when it runs long. Ask it
while that tier is still a proposal: the answer changes how the tier is presented, not only how its
pages are reviewed.

### 13.8 Close the campaign

**Run [§9](#9-checks-to-run)'s full list once, at the end** — including the docs build, the rendered
page count and the guide-path grep — and record the result in the working document. Individual page
reviews run the smaller subset; the structural checks are worth once per campaign rather than once
per page, unless the campaign adds, moves or splits a page, in which case they belong to that
change.

**What is still outstanding stays where it was found.** Every page's report keeps its own "Deferred
to the future" items, per [§4](#4-the-review-report); a campaign does not consolidate them into a
tracked document, an issue tracker or the standing-risk register on its way out, and it does not
delete a report to tidy up.

**Record what the campaign produced beyond the page edits** — the checks it caused to be written,
the library changes it caused, the standing decisions it settled — in a few lines at the end of the
working document. That list is the evidence the campaign was worth running, it is the first thing a
later campaign reads, and it is the only place a check is attributed to the review that motivated
it. One campaign closed with five new lint checks, two library changes and three standing decisions,
none of which is visible from the page diffs.

**And name the one defect the campaign kept re-finding**, in a sentence, if there is one. That
sentence generalises where a count of findings does not: for the campaign that produced
[§3.2](#32-pass-1--accuracy)'s enumeration rule it was that a hand-written count or enumeration
sitting beside something the build already measures will drift silently, and the only two defences
are a check that reads the measured list or a review that re-derives rather than reads.

## 14. Release status decides what a review may recommend

**Before the first review, establish whether the surface under review has shipped.** It is one
question per campaign, not one per finding, and it decides how far a report's "looks like a code
issue" section ([§4](#4-the-review-report)) may reach. Getting it wrong is expensive in both
directions: a reviewer who assumes everything is frozen swallows findings that were free to act on,
and a reviewer who assumes nothing is frozen proposes breaking changes to code other people are
already running.

**An unreleased surface is still open.** Where a campaign covers functionality that sits in the
changelog's unpublished section — not in any tagged release, with no user code written against it
and no semantic-versioning promise attached — a finding of the form *"this API is awkward, is
inconsistent with its siblings, is named wrongly, takes its arguments in the wrong order, or should
not exist"* is **actionable and must be reported** rather than swallowed as arriving too late. All
of the following are then in bounds as recommendations:

- renaming a public procedure, type, constant, argument or type-bound binding;
- changing an argument's kind, order, optionality or default;
- adding, removing, merging or splitting a public procedure;
- changing a documented behaviour — a default, a null/NaN convention, a tie-break, what aborts and
  what returns quietly;
- dropping a feature that has turned out not to earn its place.

**This is a licence to recommend, never a licence to apply. Four bounds hold, and none is relaxed:**

- **[§7](#7-what-a-review-may-edit) is unchanged.** The reviewer proposes; the maintainer decides. A
  review applies documentation-only fixes within §7's two bounds and nothing else, whatever the
  release status of what it describes.
- **A released surface appearing on the same page is not unreleased.** A shipped procedure that
  *gained* an optional argument is two things at once: the argument is open, the procedure is not,
  and proposing to rename the procedure is a breaking change subject to the version promise. **Say
  which of the two a recommendation is**, because the cost differs by an order of magnitude and the
  maintainer is answering a different question in each case.
- **An approved change is its own task, not part of that page's step 3.** It is exactly the case
  [§2](#2-the-three-step-loop) warns about — a round-2 comment turning a page review into a feature
  request — so it gets its own design write-up, its own settings analysis
  ([§12](#12-settings-analysis)), its own tests and negative controls, and is scheduled separately.
- **Its changelog entry folds into the feature's existing entry.** An unreleased feature never earns
  a "changed" or "fixed" entry for its own subsequent changes: there is no released behaviour for it
  to differ from, so the reader upgrading cannot observe the difference. Keep the feature's one
  existing bullet current instead.

**Whichever the answer, write it down in the campaign's working document.** A reviewer three pages
in should not have to re-derive whether a signature is a finding or a fact of life, and a report
written under the wrong assumption is not repairable by reading it — the findings that were never
raised leave no trace.
