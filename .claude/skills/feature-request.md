---
name: feature-request
description: "Turn a feature request into a design document feature_<name>.md in the repository root (scope, placement, API, settings analysis, risks, tests, docs, plan, open questions) and stop for the maintainer's review. Implements nothing."
argument-hint: "<feature request, or a name followed by the request>"
allowed-tools: Bash(git:*), Bash(grep:*), Bash(find:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(awk:*), Bash(wc:*), Bash(python3:*)
disable-model-invocation: true
---

# /feature-request — write the design document for a requested feature

**This command produces a design document and stops. It implements nothing, edits no source, and
commits nothing.** Implementation is a separate task the maintainer starts after reading the
document (`.claude/rules/workflow.md`: report before implementing).

The request: $ARGUMENTS

Injected context:

- Working tree: !`git status --short | head -20`
- Existing planning documents: !`ls feature_*.md`
- CHANGELOG headings: !`grep -n "^## " CHANGELOG.md | head -3`

## 1 Intake

- **Name the document** `feature_<name>.md`, snake_case, named for the feature; a stage of an
  existing plan is `feature_<plan>_S<n>.md`. If that file exists, stop and report; never overwrite,
  rename or delete a planning document.
- **Quote the request verbatim** at the top. The document is self-explaining without this
  conversation: no "as discussed", decisions quoted, every claim traceable to a file.
- **Check before designing**: CONTRIBUTING.md "Features considered but not implemented" (a deferred
  or declined feature is reported as such, with the recorded reason, before any design); the guide
  and CHANGELOG `[Unreleased]` for functionality that already covers the request (grep the guide for
  the request's nouns); `feature_risks.md` entries for the area (list the ones that constrain the
  design).
- **Establish release status**: does the request touch a released surface (any tagged version) or
  only `[Unreleased]` functionality? A change to a released public name, argument, default or
  behaviour is a semantic-versioning event; say so and design around it.

## 2 Analyse from the source, not from memory

- **Placement** (`.claude/rules/module-structure.md`): the module or submodule, its tier, whether
  the change adds a `use` line (footprints), whether it touches a generated file (the generator is
  edited), whether a sibling needs a new public reader query, whether `parquet_random`'s leaf rule
  or an Arrow-free tier is at stake.
- **The surface it extends**: the `public ::` lines, bindings, generics and specifics it joins;
  the tests touching the area (`grep -l` across `test/`); the guide pages covering it.
- **The rules that bind the area**, named by file and section: naming and argument kinds
  (`api-conventions.md`); the reader/writer or column/table invariants (`reader-writer.md`,
  `columns-tables.md`); the C++ side (`cpp-wrapper.md`); the language shapes to avoid
  (`fortran-gotchas.md`).

## 3 Design

- **API**: each call form as a reader types it (optional arguments in brackets, kinds in a clause),
  behaviour, defaults, and every failure mode with its exact `error stop` text (file/schema context
  appended) or its quiet return. Apply: int32/int64 kinds for any count or index, the double-call
  guard, `parquet_clamp_to_affinity`/`pf_sort_threads` for anything threaded, the typed accessor
  tier for anything per-cell.
- **Alternatives considered** and what each costs; the recommendation.
- **Settings analysis** (mandatory): either "introduces no process-global parameter", with each
  candidate checked against the admission test (`api-conventions.md`, Settings), or per knob its
  name, default, validation, environment variable, printed row, reset, C++ mirror and
  observed-effect test.
- **Risks**: each silent-failure property the feature creates, with the test that will cover it.
  Propose a `feature_risks.md` entry only for one passing the admission test in `workflow.md`
  (usually none).
- **Performance**: if a hot path changes, the `bench/` tool that measures it and the machine-free
  wording the guide will carry.
- **Not in scope**: what the request could be read to include and deliberately is not.

## 4 Plan

- **Implementation steps in order**, each naming its files (generator versus emitted output) and
  the checks it will trip: the binding list in `tools/generate_user_table_code.py`,
  `check_scenario_list_is_complete`, `tools/module_footprints.txt`, the facade's `private ::`
  inventory, `tools/check_bindc_boundary.py`.
- **Tests**: suite and test names; fixtures under `test_run/` with unique names; error scenarios
  with their three registration points (and the `concurrency_scenarios` bucket if OpenMP); the
  negative control of every guard and knob; **the mutation each test must catch**; threading skips;
  the `.and.`/optional-argument/forwarding shapes `testing.md` warns about.
- **Docs**: doc-comments; the guide page or pages (a new page needs both index lists and an
  `ordered_subpage` entry); README or CONTRIBUTING only if their story changes; the CHANGELOG
  `[Unreleased]` `### Added` bullet, drafted — what changed, never why, one bullet per feature.
- **Verification**: `fpm test`, `fpm test --profile debug`, `tools/run_lint_check.sh`,
  `tools/check_doc_anchors.py`, the generator's `--check`, `ford --warn docs.md`'s per-module
  `Unknown entity` comparison, `tools/check_module_footprints.sh` if a `use` line changed.
- **Size estimate**: files, procedures, tests, pages.

## 5 Open questions, then stop

- Numbered, each with a recommendation and its reasoning; say which block implementation. The
  maintainer answers with a `Comment:` line under each (the `/review-doc` convention).
- Report to the maintainer: the document's name, the recommendation, the blocking questions, the
  size estimate. Do not implement.

## 6 After approval (the implementing session, not this command)

Implement per the plan; write an `Outcome:` paragraph per plan item, stating any deviation; add the
risk entries, the tests, the docs and the CHANGELOG bullet; keep the document; leave the work
uncommitted on `main`.
